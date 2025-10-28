!-------------------------------------------------------------------------------------------------!
! This file is part of UncertRadio.
!
!    UncertRadio is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    UncertRadio is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with UncertRadio. If not, see <http://www.gnu.org/licenses/>.
!
!-------------------------------------------------------------------------------------------------!

submodule (Num1) Num1a

    use UR_Gleich_globals,   only: ifehl

contains

    ! median
    ! funcs
    ! Xfit
    ! SearchBCI3
    ! dpi_funcs
    ! bipoi2_norm
    ! Norm_BiPoi2
    ! matwrite
    ! quick_sort_r
    ! quick_sort_i
    ! kaiser

    !----------------------------------------------------------------------------------------------!
    real(rn) function median(x, n)

        ! this function  calculates a median value of an array x with n elements.
        !
        ! the array ws is used as working area array of lenght n. The unsorted array
        ! x is sorted into the array ws, from  which the median value is derived.
        !
        implicit none

        integer, intent(in)   :: n
        real(rn), intent(in)  :: x(n)
        !------------------------------------------------------------------------------------------!
        integer  :: n2
        real(rn) :: ws(n)
        !------------------------------------------------------------------------------------------!
        ws = x

        call quick_sort_r(ws)
        n2 = n/2
        if (mod(n, 2) == 0) then
            median = 0.5_rn * (ws(n2) + ws(n2+1))
        else
            median = ws(n2 + 1)
        end if

    end function median

    !----------------------------------------------------------------------------------------------!

    module subroutine funcs(ix, afunc)

        !     copyright (c) 2014-2025  günter kanisch
        use, intrinsic :: iso_c_binding,  only: c_int

        use UR_Gleich_globals,   only: kpoint, messwert
        use ur_linft,            only: ma, k_tmess, kpmle, k_tstart, &
                                       mfrbg, nchannels, numd, dmesszeit, dtdiff, ifit, &
                                       keqnums, mac

        use fparser,             only: evalf
        use usub3,               only: findmessk

        use rout,                only: messageshow
        use gtk,                 only: gtk_buttons_ok, gtk_message_warning
        use translation_module,  only: T => get_translation

        implicit none
        !------------------------------------------------------------------------------------------!

        integer, intent(in)   :: ix         ! number of the xi= decay curve function
        real(rn), intent(out) :: afunc(ma)  ! function values associated with the ma fit parameters

        integer               :: i, ii, messk, keqnumber(3)
        integer(c_int)        :: resp

        character(1024) :: str1
        !------------------------------------------------------------------------------------------!
        !   channel #measurement
        ! i	kanal	#messung	x(i)		(messk-1)*3*numd+(messung-1)*3+iterm
        ! 1	  1	  1	      1	      1
        ! 2	  1	  1 	    2	      2
        ! 3	  1	  1	      3	    	3
        ! 4	  1	  2 	    1	    	4
        ! 5	  1 	2 	    2	    	5
        ! 6	  1   2	      3	    	6
        ! 7	  1	  3 	    1	    	7
        ! 8	  1 	3	      2	    	8
        ! 9	  1 	3	      3	    	9
        ! 10	1	  4	      1	    	10
        ! 11	1	  4 	    2	    	11
        ! 12	1	  4 	    3	    	12
        ! 13	2	  1 	    1	    	13
        ! 14	2	  1 	    2	    	14
        ! 15	2	  1 	    3	    	15
        ! 16	2	  2	      1	    	16
        ! 17	2	  2	      2	    	17
        ! 18	2	  2	      3	    	18
        ! 19	2	  3	      1	    	19
        ! 20	2	  3 	    2	    	20
        ! 21	2	  3	      3	    	21
        ! 22	2	  4	      1	    	22
        ! 23	2	  4	      2	    	23
        ! 24	2	  4	      3	    	24
        !
        !-----------------------------------------------------------------------

        ! Find the measurement channel (A,B C):
        messk = FindMessk(ix)

        !  store tmess and tstart in elements of Messwert, for the ix-th value of the decay curve:
        Messwert(kpoint(k_tmess)) = dmesszeit(ix)    ! counting time tmess
        Messwert(kpoint(k_tstart)) = dtdiff(ix)      ! time difference to the time of chemical separation

        !------------------------

        afunc = 0.0_rn         ! for all ma=3
        if(mac == 0) call find_mac(mac)

        if(mac > size(afunc)) then

            write(str1,*) T("Possible inconsistency between the number of Xi formulas in the FitDecay model"), &
                          new_line('A'), T("and the number of measurements, counting channels and results!"), &
                          new_line('A'), T("The calculations are stopped here, see chapter 7.11.3 within the CHM Help.")

            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
            ifehl = 1
            return
        end if

        if(allocated(kEQnums)) deallocate(kEQnums)   ! 21.6.2024
        allocate(kEQnums(nchannels*numd,3))


        do i = 1, nchannels*numd
            call findEq_afunc(i, kEQnumber)
            kEQnums(i, 1:3) = kEQnumber(1:3)
        end do

        do i = 1, mac
            ii = kEQnums(ix, i)
            if (kPMLE == 1 .and. i == mfrbg .and. ifit(i) == 3) then
                afunc(i) = 1.0_rn
            else
                afunc(i) = evalf(ii, Messwert)
            end if
        end do

    end subroutine funcs

!#######################################################################

    module subroutine findeq_afunc(ix, keqnumber)

        !     Copyright (C) 2023-2023  Günter Kanisch

        use UR_Gleich_globals,           only: knumegr, nab, nmodf
        use ur_linft,            only: ma, defineallxt, mfitfix, nchannels, numd, mac
        use usub3,               only: findmessk

        implicit none

        integer, intent(in)     :: ix            ! number of the xi= decay curve function
        integer, intent(out)    :: keqnumber(ma) ! function values of associated with the ma fit parameters

        integer              :: i, messk
        integer, parameter   :: i_array(ma) = [(i, i=1, ma)]          ! ,findmessk
        logical              :: ausnahme

        messk = FindMessk(ix)

        mac = mfitfix        ! sum of parameters being fitted or fixed
        ausnahme = .false.

        !  nmodf: number of equations of linear model
        if(.not.defineallxt) then
            if(nmodf/nchannels > mfitfix) mac = nmodf/nchannels
        else
            if(nmodf/knumEGr > mfitfix) mac = nmodf/knumEGr
            ! if(mac > mfitfix) then
            if(mac >= mfitfix) then                  ! 11.6.2024
                if(nmodf/knumEGr == numd) then
                    mac = knumEgr
                    ausnahme = .true.
                end if
            end if
        end if

        if (.not. defineallxt) then
            kEQnumber(1:mac) = nab + (messk-1)*mac + i_array(1:mac)
        else
            if (.not. ausnahme) then
                kEQnumber(1:mac) = nab + (messk-1)*(numd/nchannels*mac) + (ix/numd)*mac + i_array(1:mac)
            else
                kEQnumber(1:mac) = nab + (ix-1) * knumEGr + i_array(1:mac)
            end if
        end if

    end subroutine findEQ_afunc

!#######################################################################

    module subroutine find_mac(mac)

        !     Copyright (C) 2023-2023  Günter Kanisch

        use UR_Gleich_globals,           only: knumegr, nmodf
        use ur_linft,            only: defineallxt, mfitfix, nchannels, numd
        use usub3,               only: findmessk

        implicit none

        integer, intent(out) :: mac    ! sum of parameters being fitted or fixed
        logical              :: ausnahme

        mac = mfitfix
        ausnahme = .false.
        !  nmodf: number of equations of linear model
        if(.not.defineallxt) then
            if(nmodf/nchannels > mfitfix) mac = nmodf/nchannels
        else
            if(nmodf/knumEGr > mfitfix) mac = nmodf/knumEGr
            ! if(mac > mfitfix) then
            if(mac >= mfitfix) then                ! 11.6.2024
                if(nmodf/knumEGr == numd) then
                    mac = knumEgr
                    ausnahme = .true.
                end if
            end if
        end if
    end subroutine find_mac

    !#######################################################################

    !     SUBROUTINE XFIT

    !     PURPOSE
    !       CALCULATE THE MEAN AND ESTIMATED ERRORS FOR A SET OF DATA POINTS

    !     USAGE
    !       CALL XFIT (X, SIGMAX, NPTS, MODE, XMEAN, SIGMAM, SIGMA)

    !     DESCRIPTION OF PARAMETERS
    !       X      - ARRAY OF DATA POINTS
    !       SIGMAX - ARRAY OF STANDARD DEVIATIONS FOR DATA POINTS
    !       NPTS   - NUMBER OF DATA POINTS
    !       MODE   - DETERMINES METHOD OF WEIGHTING
    !                 0 (NO WEIGHTING) WEIGHT(I) = 1.
    !                +1 (INSTRUMENTAL) WEIGHT(I) = 1./SIGMAX(I)**2
    !                -1 (STATISTICAL)  WEIGHT(I) = 1.
    !                +2 WIE +1, ABER STANDARD DEVIATION NACH SEITE 73 UNTEN
    !       XMEAN  - WEIGHTED MEAN
    !       SIGMAM - STANDARD DEVIATION OF MEAN
    !       SIGMA  - STANDARD DEVIATION OF DATA

    !     SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
    !       NONE

    !     MODIFICATIONS FOR FORTRAN II
    !       OMIT DOUBLE PRECISION SPECIFICATIONS
    !       CHANGE DSQRT TO SQRTF IN STATEMENTS 54, 62, 64, AND 66

    !     Source: P. R. Bevington: Data Reduction and Error Analysis for
    !             the Physical Sciences. McGraw-Hill Book Company, 1969

    !-----------------------------------------------------------------------
    module subroutine xfit (x, sigmax, npts, mode, xmean, sigmam, sigma)

        implicit none

        integer   , intent(in)      :: npts
        real(rn), intent(in)        :: x(npts)
        real(rn), intent(in)        :: sigmax(npts)
        integer   , intent(in)      :: mode
        real(rn), intent(out)       :: xmean
        real(rn), intent(out)       :: sigmam
        real(rn), intent(out)       :: sigma

        integer        :: i
        real(rn)       :: sum, sumx, weight, free,fak
    !-----------------------------------------------------------------------
    !        ACCUMULATE WEIGHTED 41

        sum = 0.0_rn
        sumx = 0.0_rn
        sigma = 0.0_rn
        sigmam = 0.0_rn
        DO i=1, npts
            IF (mode > 0) THEN
                weight = 1.0_rn / sigmax(i)**2.0_rn
            else
                weight = 1.0_rn
            END IF
            sum = sum + weight
            sumx = sumx + weight*x(i)
        END DO

!        EVALUATE MEAN AND STANDARD DEVIATIONS

        xmean = sumx/sum
        DO  i=1, npts
            IF (mode > 0) THEN
                weight = 1.0_rn / sigmax(i)**2.0_rn
            else
                weight = 1.0_rn
            END IF
            fak = 1.0_rn
            IF(mode == 2) fak = weight
            sigma = sigma + fak*(x(i)-xmean)**2.0_rn
        END DO
        free = npts-1
        sigma = SQRT(sigma/max(1.0_rn, free))
        IF (mode < 0) THEN
            sigmam = SQRT(xmean/sum)
        ELSE IF (mode == 0) THEN
            sigmam = sigma / SQRT(sum)
        ELSE
            IF(mode == 2) THEN
                sigma = sigma*SQRT(npts/sum)
                sigmam = sigma/SQRT(free+1.0_rn)
            else
                sigmam = SQRT(1.0_rn/sum)
            END IF
        END IF
        RETURN

    END SUBROUTINE Xfit

!#######################################################################

    module subroutine SearchBCI3(mode,imcmax,kqtyp)

        ! This routine performs a search for the shortest coverage interval,
        ! encompassing a probability given by W1minusG, for a probability distribution
        ! given as an array of length imcmax (obtained from a MC simulation)

        !     Copyright (C) 2021-2023  Günter Kanisch

        USE UR_MCC,             only: estLQ_BCI2,estUQ_BCI2,arraymc
        USE UR_DLIM,            only: W1minusG
        use Brandt,             only: mean

        implicit none

        integer, intent(in)   :: mode      !  1: MC;  2:  MCMC-MH
        integer, intent(in)   :: imcmax    ! Länge des MC-Arrays
        integer, intent(in)   :: kqtyp     ! for:  1: output quantity; 2: DT;  3: DL

        real(rn)              :: yshort,ph,pl,yl,yh,ydiff,pgam
        integer               :: i,imcp,k,ibest,kbest

        !   shortest coverage interval:
        ! using a simple search
        yshort = 1.E+30_rn
        pgam = (1.0_rn-W1minusG)
        imcp = int(pgam*imcmax)
        ibest = 0
        ! write(63,'(a,i0,a,es12.5,2(a,i0))') 'imcp=',imcp,' Wert(imcp)=', &
        !               arraymc(imcp,kqtyp),' imcmax=',imcmax
        do i=0,imcp
            pl = real(i,rn) / real(imcmax,rn)
            ph = pgam - pl
            k  = int((1.0_rn - ph)*real(imcmax,rn))
            if(i == 0) then
                yl = 0.0_rn
            else
                if(mode == 1) yl = arraymc(i,kqtyp)
                !!! if(mode == 2) yl = mh_chain(i)
                ! if(mode ==2 .and. mh_chain(i) > mh_chain(i+1)) write(28,*) 'searchBCI3: Sort-Fehler'
            end if
            yh = arraymc(k,kqtyp)       ! 2025.01.23 GK
            if(mode == 1) yh = arraymc(k,kqtyp)
            !!! if(mode == 2) yh = mh_chain(k)
            ydiff = yh - yl
            if(ydiff < yshort) then
                yshort = ydiff
                if(mode == 1) then
                    estLQ_BCI2 = yl
                    estUQ_BCI2 = yh
                    ibest = i
                    kbest = k
                end if
            end if
            if(ibest > 0 .and. i > ibest + imcp/5) exit
            !if(abs(ydiff-yshort) < 5.E-10_rn) write(63,*) 'i=',i,' ydiff=',ydiff,' yshort=',yshort, &
            !                                               ' ibest=',ibest
        end do
        !write(63,*) 'ibest,kbest=',ibest,kbest,' estLQ_BCI2=', &
        !           sngl(estLQ_BCI2),' yshort=',sngl(yshort), &
        ! ' P=',sngl(real(ibest,rn)/real(imcmax,rn) + (real(imcmax-kbest-1,rn)+0.25_rn)/real(imcmax,rn))

    end subroutine SearchBCI3

    !#######################################################################

    module real(rn) function dpi_funcs(mwind, indeval, jp, ma, Fv1)

        ! calculates a partial derivative:
        !    d(afunc(jp)) / d(Messwert(mwind);  with afunc(jp) obtained by call funcs(indeval,...)

        !     Copyright (C) 2014-2023  Günter Kanisch

        use top,              only: dpafact
        use UR_Gleich_globals,        only: Messwert,missingval
        use UR_Linft,         only: use_WTLS

        implicit none

        integer, intent(in)    :: mwind      ! Messwert index of the variable, with respect to which
        ! a partial derivative is calculated
        integer, intent(in)    :: indeval    ! number of the equation, of which the derivative is calculated
        integer, intent(in)    :: jp         ! index of afunc(), so that afunc(jp) = function value
        integer, intent(in)    :: ma         ! length of array afunc
        real(rn), intent(in)      :: Fv1        ! value of the unmodified function, supplied externally

        real(rn)       :: fv1m,Fv2,dpa,afunc(3)

        Fv1m = Fv1
        if(abs(fv1m-missingval) < EPS1MIN) then
            call funcs(indeval,afunc)
            Fv1m = afunc(jp)
        end if
        dpa = Messwert(mwind) * dpafact(Messwert(mwind)) - Messwert(mwind)    ! Increment of parameter
        if(use_WTLS) then
            dpa = Messwert(mwind) * (1.0_rn + (1.0_rn - dpafact(Messwert(mwind)))*10._rn) - Messwert(mwind)  ! 14.7.2023
        end if
        Messwert(mwind) = Messwert(mwind) + dpa                 ! modify parameter with index mwind
        call funcs(indeval,afunc)
        Fv2 = afunc(jp)
        dpi_funcs = (Fv2/dpa - Fv1m/dpa)    ! partial derivative with respect to parameter Messwert(mwind)
        Messwert(mwind) = Messwert(mwind) - dpa   ! restore Messwert(mwind)

        if(abs(dpi_funcs) < 1.E-22_rn) dpi_funcs = 0.0_rn      ! important

    end function dpi_funcs

    !----------------------------------------------------------------------------------------------!

    module subroutine matwrite(xmat, mm, nn, kunit, frmt, ctext)

        ! writes a matrix xmat(m,n) to the unit number kunit, uses the format
        ! frmt for the write-statement, and writes a headline ctext

        !     Copyright (C) 2020-2023  Günter Kanisch

        implicit none

        ! integer   ,intent(in)        :: m,n        ! physical dims
        integer, intent(in)          :: mm, nn     ! dims to be printed
        real(rn), intent(in)         :: xmat(:,:)
        integer, intent(in)          :: kunit
        character(len=*), intent(in) :: frmt
        character(len=*), intent(in) :: ctext

        integer         :: i, m, n

        m = ubound(xmat, dim=1)
        n = ubound(xmat, dim=2)
        write(kunit,*)
        if(len_trim(ctext) > 0) write(kunit,*) trim(ctext)

        do i=1, mm
            write(kunit, frmt) xmat(i,1:nn)
        end do
        write(kunit,*)

    end subroutine matwrite

    !----------------------------------------------------------------------------------------------!

    module subroutine quick_sort_r(list)


        ! quick sort routine from:
        ! brainerd, w.s., goldberg, c.h. & adams, j.c. (1990) "programmer's guide to
        ! fortran 90", mcgraw-hill  isbn 0-07-000248-7, pages 149-150.
        ! modified by alan miller to include an associated integer array which gives
        ! the positions of the elements in the original order.
        !------------------------------------------------------------------------------------------!
        implicit none
        real(rn), intent(inout) :: list(:)
        !------------------------------------------------------------------------------------------!
        if(ubound(list, dim=1) < 1) return

        call quick_sort_1_r(1, size(list))

        contains

        recursive subroutine quick_sort_1_r(left_end, right_end)

            integer, intent(in) :: left_end, right_end

            ! local variables
            integer             :: i, j
            real(rn)            :: reference, temp
            integer, parameter  :: max_simple_sort_size = 6

            if (right_end < left_end + max_simple_sort_size) then
                ! Use interchange sort for small lists
                call interchange_sort_r(left_end, right_end)

            else
                ! Use partition ("quick") sort
                reference = list((left_end + right_end)/2)
                i = left_end - 1; j = right_end + 1

                do
                    ! scan list from left end until element >= reference is found
                    do
                        i = i + 1
                        if (list(i) >= reference) exit
                    end do
                    ! scan list from right end until element <= reference is found
                    do
                        j = j - 1
                        if (list(j) <= reference) exit
                    end do


                    if (i < j) then
                        ! swap two out-of-order elements
                        temp = list(i); list(i) = list(j); list(j) = temp
                    else if (i == j) then
                        i = i + 1
                        exit
                    else
                        exit
                    end if
                end do

                if (left_end < j) call quick_sort_1_r(left_end, j)
                if (i < right_end) call quick_sort_1_r(i, right_end)
            end if

        end subroutine quick_sort_1_r


        subroutine interchange_sort_r(left_end, right_end)

            integer, intent(in) :: left_end, right_end

            ! local variables
            integer             :: i, j
            real(rn)            :: temp

            do i = left_end, right_end - 1
                do j = i+1, right_end
                    if (list(i) > list(j)) then
                        temp = list(i); list(i) = list(j); list(j) = temp
                    end if
                end do
            end do

        end subroutine interchange_sort_r

    end subroutine quick_sort_r

    !----------------------------------------------------------------------------------------------!

    module recursive subroutine quick_sort_i(list,order)

        ! quick sort routine from:
        ! brainerd, w.s., goldberg, c.h. & adams, j.c. (1990) "programmer's guide to
        ! fortran 90", mcgraw-hill  isbn 0-07-000248-7, pages 149-150.
        ! modified by alan miller to include an associated integer array which gives
        ! the positions of the elements in the original order.

        implicit none
        integer, dimension (:), intent(in out)  :: list
        integer, dimension (:), intent(out)  :: order

        ! local variable
        integer       :: i

        if(size(list) < 1) return
        if(size(order) > 0) then
            do i = 1, size(order)   !  size(list)
                order(i) = i
            end do
        end if

    call quick_sort_1_i(1, size(list))

    contains

        recursive subroutine quick_sort_1_i(left_end, right_end)

            integer, intent(in) :: left_end, right_end

            !     local variables
            integer             :: i, j, itemp
            integer             :: reference, temp
            integer, parameter  :: max_simple_sort_size = 6

            if (right_end < left_end + max_simple_sort_size) then
                ! use interchange sort for small lists
                call interchange_sort_i(left_end, right_end)

            else
                ! use partition ("quick") sort
                reference = list((left_end + right_end)/2)
                i = left_end - 1; j = right_end + 1

                do
                    ! scan list from left end until element >= reference is found
                    do
                        i = i + 1
                        if (list(i) >= reference) exit
                    end do
                    ! scan list from right end until element <= reference is found
                    do
                        j = j - 1
                        if (list(j) <= reference) exit
                    end do

                    if (i < j) then
                        ! swap two out-of-order elements
                        temp = list(i); list(i) = list(j); list(j) = temp
                        if(size(order) > 1) then
                            itemp = order(i); order(i) = order(j); order(j) = itemp
                        end if
                    else if (i == j) then
                        i = i + 1
                        exit
                    else
                        exit
                    end if
                end do

                if (left_end < j) call quick_sort_1_i(left_end, j)
                if (i < right_end) call quick_sort_1_i(i, right_end)
            end if

        end subroutine quick_sort_1_i


        subroutine interchange_sort_i(left_end, right_end)

            integer, intent(in) :: left_end, right_end

            !     local variables
            integer          :: i, j, itemp
            integer          :: temp

            do i = left_end, right_end - 1
                do j = i+1, right_end
                    if (list(i) > list(j)) then
                        temp = list(i); list(i) = list(j); list(j) = temp
                        if(size(order) > 1) then
                            itemp = order(i); order(i) = order(j); order(j) = itemp
                        end if
                    end if
                end do
            end do

        end subroutine interchange_sort_i

    end subroutine quick_sort_i

    !#######################################################################

    module subroutine kaiser(a, nrows, n, eigenv, trace, sume, ier)

        !  EIGENVALUES AND VECTORS OF A SYMMETRIC +VE DEFINITE MATRIX,
        !  USING KAISER'S METHOD.
        !  REFERENCE: KAISER,H.F. 'THE JK METHOD: A PROCEDURE FOR FINDING THE
        !  EIGENVALUES OF A REAL SYMMETRIC MATRIX', COMPUT.J., VOL.15, 271-273, 1972.

        !  ARGUMENTS:-
        !  A       = INPUT, AN ARRAY CONTAINING THE MATRIX
        !            OUTPUT, THE COLUMNS OF A CONTAIN THE NORMALIZED EIGENVECTORS
        !            OF A.   N.B. A IS OVERWRITTEN !
        !  NROWS   = INPUT, THE FIRST DIMENSION OF A IN THE CALLING PROGRAM.
        !  N       = INPUT, THE ORDER OF A, I.E. NO. OF COLUMNS.
        !            N MUST BE <= NROWS.
        !  EIGENV()= OUTPUT, A VECTOR CONTAINING THE ORDERED EIGENVALUES.
        !  TRACE   = OUTPUT, THE TRACE OF THE INPUT MATRIX.
        !  SUME    = OUTPUT, THE SUM OF THE EIGENVALUES COMPUTED.
        !            N.B. ANY SYMMETRIC MATRIX MAY BE INPUT, BUT IF IT IS NOT +VE
        !            DEFINITE, THE ABSOLUTE VALUES OF THE EIGENVALUES WILL BE FOUND.
        !            IF TRACE = SUME, THEN ALL OF THE EIGENVALUES ARE POSITIVE
        !            OR ZERO.   IF SUME > TRACE, THE DIFFERENCE IS TWICE THE SUM OF
        !            THE EIGENVALUES WHICH HAVE BEEN GIVEN THE WRONG SIGNS !
        !  IER     = OUTPUT, ERROR INDICATOR
        !             = 0 NO ERROR
        !             = 1 N > NROWS OR N < 1
        !             = 2 FAILED TO CONVERGE IN 10 ITERATIONS

        !  LATEST REVISION - 6 September 1990
        !  Fortran 90 version - 20 November 1998
        !        https://wp.csiro.au/alanmiller/kaiser.f90

        !------------------------------------------------------------------------------------------!

        implicit none
        real (rn), intent(in out) :: a(:,:)
        integer, intent(in)       :: nrows
        integer, intent(in)       :: n
        real (rn), intent(out)    :: eigenv(:)
        real (rn), intent(out)    :: trace
        real (rn), intent(out)    :: sume
        integer, intent(out)      :: ier

        ! local variables
        integer              :: i, iter, j, k, ncount, nn
        real (rn)            :: absp, absq, cos, ctn, eps, &
                                halfp, p, q, sin, ss, tan, temp, xj, xk

        !   calculate convergence tolerance, eps.
        !   calculate trace.   initial settings.

        ier = 1
        if(n < 1 .or. n > nrows) return
        ier = 0
        iter = 0
        trace = 0.0_rn
        ss = 0.0_rn
        do j = 1,n
            trace = trace + a(j,j)
            do i = 1,n
                ss = ss + a(i,j)**2
            end do
        end do
        sume = 0.0_rn
        eps = EPS1MIN*ss/n
        nn = n*(n-1)/2
        ncount = nn

        !   ORTHOGONALIZE PAIRS OF COLUMNS J & K, K > J.

20      do j = 1, n-1
            do k = j+1, n

        !   CALCULATE PLANAR ROTATION REQUIRED

                halfp = 0.0_rn
                q = 0.0_rn
                do i = 1,n
                    xj = a(i,j)
                    xk = a(i,k)
                    halfp = halfp + xj*xk
                    q = q + (xj+xk) * (xj-xk)
                end do
                p = halfp + halfp
                absp = abs(p)

                !   If P is very small, the vectors are almost orthogonal.
                !   Skip the rotation if Q >= 0 (correct ordering).

                if (absp < eps .and. q >= 0.0_rn) then
                    ncount = ncount - 1
                    if (ncount <= 0) go to 160
                    cycle
                end if

                !   Rotation needed.

                absq = abs(q)
                if(absp <= absq) then
                    tan = absp/absq
                    cos = 1.0_rn/sqrt(1.0_rn + tan*tan)
                    sin = tan*cos
                else
                    ctn = absq/absp
                    sin = 1.0_rn/sqrt(1.0_rn + ctn*ctn)
                    cos = ctn*sin
                end if
                cos = sqrt((1.0_rn + cos)*0.5_rn)
                sin = sin/(cos + cos)
                if(q < 0.0_rn) then
                    temp = cos
                    cos = sin
                    sin = temp
                end if
                if(p < 0.0_rn) sin = -sin

                !   PERFORM ROTATION

                do i = 1,n
                    temp = a(i,j)
                    a(i,j) = temp*cos + a(i,k)*sin
                    a(i,k) = -temp*sin + a(i,k)*cos
                end do
            end do
        end do
        ncount = nn
        iter = iter + 1
        if(iter < 10) go to 20
        ier = 2

        !   CONVERGED, OR GAVE UP AFTER 10 ITERATIONS

160     do j = 1,n
            temp = sum( a(1:n,j)**2 )
            eigenv(j) = sqrt(temp)
            sume = sume + eigenv(j)
        end do

        !   SCALE COLUMNS TO HAVE UNIT LENGTH

        do j = 1,n
            if (eigenv(j) > 0.0_rn) then
                temp = 1.0_rn/eigenv(j)
            else
                temp = 0.0_rn
            end if
            a(1:n,j) = a(1:n,j)*temp
        end do

        return
    end subroutine kaiser

    !----------------------------------------------------------------------------------------------!

    module subroutine sym_eigensolve(n, a, lda, eigenv, ier)

        implicit none
        integer, intent(in)       :: n
        real (rn), intent(in out) :: a(n,n)
        integer, intent(in)       :: lda
        real (rn), intent(out)    :: eigenv(n)
        integer, intent(out)      :: ier
        !------------------------------------------------------------------------------------------!
        ! local variables
        integer :: lwork = 64

        real(rn), allocatable :: work(:)
        integer :: counter = 0
        !------------------------------------------------------------------------------------------!
        allocate(work(lwork))

        call dsyev('V', 'U', n, a, lda, eigenv, work, lwork, ier)

        counter = counter + 1
        lwork = int(work(1))
        deallocate(work)

    end subroutine sym_eigensolve

    !#######################################################################

    module recursive subroutine quick_sort2_i(list,order)

        ! This routine is a modified version of:    quick_sort_i(list,order)
        ! It is modifed such (GK), that only the output array order is sorted,
        ! while the array list to be sorted now remains unsorted (intent(in)).
        !
        ! Quick sort routine from:
        ! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
        ! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
        ! Modified by Alan Miller to include an associated integer array which gives
        ! the positions of the elements in the original order.

        implicit none
        !!! integer   , dimension (:), intent(in out)  :: list
        integer, dimension (:), intent(in)   :: list               ! changed (gk)
        integer, dimension (:), intent(out)  :: order

        ! Local variable
        integer       :: i

        if(size(list) < 1) return
        if(size(order) > 0) then
            DO i = 1, size(order)   !  SIZE(list)
                order(i) = i
            END DO
        end if

        call quick_sort2_1_i(1, size(list))

        contains

        recursive subroutine quick_sort2_1_i(left_end, right_end)

            integer, intent(in) :: left_end, right_end

            !     local variables
            integer             :: i, j, itemp
            integer             :: reference
            integer, parameter  :: max_simple_sort_size = 6

            if (right_end < left_end + max_simple_sort_size) then
                ! Use interchange sort for small lists
                call interchange_sort2_i(left_end, right_end)

            else
                ! Use partition ("quick") sort
                reference = list(order((left_end + right_end)/2))
                i = left_end - 1; j = right_end + 1

                do
                    ! scan list from left end until element >= reference is found
                    do
                        i = i + 1
                        if (list(order(i)) >= reference) exit
                    end do
                    ! scan list from right end until element <= reference is found
                    do
                        j = j - 1
                        if (list(order(j)) <= reference) exit
                    end do

                    if (i < j) then
                        ! swap two out-of-order elements
                        ! temp = list(i); list(i) = list(j); list(j) = temp
                        if(size(order) > 1) then
                            itemp = order(i); order(i) = order(j); order(j) = itemp
                        end if
                    else if (i == j) then
                        i = i + 1
                        exit
                    else
                        exit
                    end if
                end do

                if (left_end < j) call quick_sort2_1_i(left_end, j)
                if (i < right_end) call quick_sort2_1_i(i, right_end)
            end if

        end subroutine quick_sort2_1_i


        subroutine interchange_sort2_i(left_end, right_end)

            integer, intent(in) :: left_end, right_end

            !     local variables
            integer          :: i, j, itemp


            do i = left_end, right_end - 1
                do j = i+1, right_end
                    if (list(order(i)) > list(order(j))) then
                        ! temp = list(i); list(i) = list(j); list(j) = temp
                        if(size(order) > 1) then
                            itemp = order(i); order(i) = order(j); order(j) = itemp
                        end if
                    end if
                end do
            end do

        end subroutine interchange_sort2_i

    end subroutine quick_sort2_i

!#######################################################################
end submodule Num1a
