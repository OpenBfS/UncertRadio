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

    use UR_Gleich,   only: ifehl
    ! USE UR_params,   only: rn, one, two, three, half, eps1min


contains

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

!##########################################################################

    module subroutine funcs(ix,afunc)

        !     Copyright (C) 2014-2023  Günter Kanisch

        USE UR_Gleich,           only: knumEGr,nab,nmodf,kpoint,Messwert,RSeite ! ,ifehl
        USE UR_Linft,            only: ma,defineallxt,k_tmess,kPMLE,k_tstart,mfitfix, &
            mfrbg,nchannels,numd,singlenuk,dmesszeit,dtdiff,ifit,wp, &
            kEQnums,mac

        USE fparser,             ONLY: initf, parsef, evalf
        USE UR_Perror
        USE UR_DLIM,             ONLY: iteration_on
        use UR_Variables,        only: langg,MCsim_on
        use Usub3,               only: FindMessk

        use Rout,                only: MessageShow
        use gtk,                 only: GTK_BUTTONS_OK,GTK_MESSAGE_WARNING
        use Top,                 only: IntModA2
        use, intrinsic :: iso_c_binding,       only: c_int

        implicit none

        integer(4),INTENT(IN)     :: ix         ! number of the Xi= decay curve function

        real(rn),INTENT(OUT)      :: afunc(ma) ! function values associated with the ma fit parameters

        integer(4)           :: i,k,ii,messk,kEQnumber(3)          ! ,FindMessk
        integer(c_int)       :: resp
        logical              :: ausnahme
        character(:),allocatable :: str1
        !-----------------------------------------------------------------------
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

        allocate(character(len=800) :: str1)
        ! Find the measurement channel (A,B C):
        messk = FindMessk(ix)

        !  store tmess and tstart in elements of Messwert, for the ix-th value of the decay curve:
        Messwert(kpoint(k_tmess)) = dmesszeit(ix)    ! counting time tmess
        Messwert(kpoint(k_tstart)) = dtdiff(ix)      ! time difference to the time of chemical separation

        !  IF(ix == numd+1) THEN
        !    messk = 1
        !    tmstot = zero
        !    do i=1,numd
        !      tmstot = tmstot + dmesszeit(i)
        !    end do
        !    Messwert(kpoint(k_tmess)) = tmstot
        !  end if

        !------------------------

        afunc = zero         ! for all ma=3
        if(mac == 0) call find_mac(mac)

        if(mac > size(afunc)) then

            IF(langg == 'DE') WRITE(str1,*) 'Mögliche Inkonsistenz zwischen Anzahl der Xi-Formeln im FitDecay-Modell ' &
                // char(13) // 'und der Anzahlen von Messungem, Zählkanälen und Ergebnisgrößen!' &
                // CHAR(13) // 'Die Berechnung wird hier abgebrochen, ' &
                // 'siehe Kapitel 7.11.3 in der CHM-Hilfe.'
            IF(langg == 'EN') WRITE(str1,*) 'Possible inconsistency between the number of Xi formulas in the' &
                // char(13) // 'FitDecay model and the number of measurements, counting channels and results!' &
                // CHAR(13) // 'The calculations are stopped here, ' &
                // 'see chapter 7.11.3 within the CHM Help.'
            IF(langg == 'FR') WRITE(str1,*) 'Incohérence possible entre le nombre de formules Xi dans le modèle FitDecay' &
                // char(13) // 'et le nombre de mesures, de canaux de comptage et de résultats!' &
                // CHAR(13) // 'Les calculs sont arrêtés ici, '  &
                // CHAR(13) // 'voir chapitre 7.11.3 dans l''aide CHM. '
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
            ifehl = 1
            return
        end if

        if(.not.allocated(kEQnums)) then
            allocate(kEQnums(nchannels*numd,3))
            do i=1,nchannels*numd
                call findEq_afunc(i,kEQnumber)
                kEQnums(i,1:3) = kEQnumber(1:3)
            end do
        end if

        if(nchannels*numd > size(kEQnums,1)) then
            call IntModA2(kEQnums,nchannels*numd,3)
            do i=1,nchannels*numd
                call findEq_afunc(i,kEQnumber)
                kEQnums(i,1:3) = kEQnumber(1:3)
            end do
        end if
        !  write(66,*) 'funcs: allocate kEQnums: numd=',int(numd,2)
        do i=1,mac
            ! ii: equation number:
            ii = kEQnums(ix,i)
            ! write(66,*) 'funcs: ii=',int(ii,2),' Rseite(ii)=',Rseite(ii)%s
            afunc(i) = zero
            if(kPMLE == 1 .and. i == mfrbg .and. ifit(i) == 2) then
                afunc(i) = one
                cycle
            end if
            IF(ifit(i) <= 2) THEN
                afunc(i) = evalf(ii,Messwert)         ! corresponds to Messwert(nab+ii)=Fitp(i), nab is now contained in ii
                afunc(i) = afunc(i) * wp(ix,i)
                ! write(66,*) 'i=',int(i,2),' afunc(i)=',sngl(afunc(i)),' wp(ix,i)=',sngl(wp(ix,i)),' ix=',int(ix,2)
            END IF
        end do

        return
    end subroutine funcs

!#######################################################################

    module subroutine findEq_afunc(ix,kEQnumber)

        !     Copyright (C) 2023-2023  Günter Kanisch

        USE UR_Gleich,           only: knumEGr,nab,nmodf
        USE UR_Linft,            only: ma,defineallxt,mfitfix,nchannels,numd,mac,ifit
        use Usub3,               only: FindMessk

        implicit none

        integer(4),INTENT(IN)     :: ix            ! number of the Xi= decay curve function
        integer(4),INTENT(OUT)    :: kEQnumber(ma) ! function values of associated with the ma fit parameters

        integer(4)           :: i,k,ii,messk,ic          ! ,FindMessk
        logical              :: ausnahme

        messk = FindMessk(ix)

        mac = mfitfix        ! sum of parameters being fitted or fixed
        ausnahme = .false.
        !  nmodf: number of equations of linear model
        if(.not.defineallxt) then
            if(nmodf/nchannels > mfitfix) mac = nmodf/nchannels
        else
            if(nmodf/knumEGr > mfitfix) mac = nmodf/knumEGr
            if(mac > mfitfix) then
                if(nmodf/knumEGr == numd) then
                    mac = knumEgr
                    ausnahme = .true.
                end if
            end if
        end if

        ! write(66,*) 'findEQ_afunc: mac=',int(mac,2),' ausnahme=',ausnahme,'  ifit=',int(ifit,2)
        do i=1,mac
            ! ii: equation number:
            if(.not.defineallxt) ii = (messk-1)*mac + i
            if(defineallxt) then
                if(.not.ausnahme) then
                    ii = (messk-1)*(numd/nchannels*mac) + (ix/numd)*mac + i  ! equation number
                else
                    ! for the case of the French Tritium-bubblers method:
                    ii = (ix-1)*knumEGr + i
                end if
            end if
            kEQnumber(i) = nab + ii
            ! write(66,*) 'mfitfix=',int(mfitfix,2),'  ifit=',int(ifit,2)
            ! write(66,*) 'mac=',int(mac,2),' i=',int(i,1),' ii=',int(ii,2),' nab=',int(nab,2),' kEQnumber(i)=',int(kEQnumber(i),2)

        end do

    end subroutine findEQ_afunc

!#######################################################################

    module subroutine find_mac(mac)

        !     Copyright (C) 2023-2023  Günter Kanisch

        USE UR_Gleich,           only: knumEGr,nab,nmodf
        USE UR_Linft,            only: ma,defineallxt,mfitfix,nchannels,numd
        use Usub3,               only: FindMessk

        implicit none

        integer(4),INTENT(OUT)    :: mac    ! sum of parameters being fitted or fixed

        integer(4)           :: i,k,ii
        logical              :: ausnahme

        mac = mfitfix
        ausnahme = .false.
        !  nmodf: number of equations of linear model
        if(.not.defineallxt) then
            if(nmodf/nchannels > mfitfix) mac = nmodf/nchannels
        else
            if(nmodf/knumEGr > mfitfix) mac = nmodf/knumEGr
            if(mac > mfitfix) then
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
    module SUBROUTINE Xfit (x, sigmax, npts, mode, xmean, sigmam, sigma)

        implicit none

        INTEGER(4), INTENT(IN)      :: npts
        real(rn), INTENT(IN)        :: x(npts)
        real(rn), INTENT(IN)        :: sigmax(npts)
        INTEGER(4), INTENT(IN)      :: mode
        real(rn), INTENT(OUT)       :: xmean
        real(rn), INTENT(OUT)       :: sigmam
        real(rn), INTENT(OUT)       :: sigma

        INTEGER(4)     :: i
        real(rn)       :: sum, sumx, weight, free,fak
!-----------------------------------------------------------------------
!        ACCUMULATE WEIGHTED 41

        sum = zero
        sumx = zero
        sigma = zero
        sigmam = zero
        DO i=1, npts
            IF (mode > 0) THEN
                weight = one / sigmax(i)**two
            else
                weight = one
            END IF
            sum = sum + weight
            sumx = sumx + weight*x(i)
        END DO

!        EVALUATE MEAN AND STANDARD DEVIATIONS

        xmean = sumx/sum
        DO  i=1, npts
            IF (mode > 0) THEN
                weight = one / sigmax(i)**two
            else
                weight = one
            END IF
            fak = one
            IF(mode == 2) fak = weight
            sigma = sigma + fak*(x(i)-xmean)**two
        END DO
        free = npts-1
        sigma = SQRT(sigma/max(one,free))
        IF (mode < 0) THEN
            sigmam = SQRT(xmean/sum)
        ELSE IF (mode == 0) THEN
            sigmam = sigma / SQRT(sum)
        ELSE
            IF(mode == 2) THEN
                sigma = sigma*SQRT(npts/sum)
                sigmam = sigma/SQRT(free+one)
            else
                sigmam = SQRT(one/sum)
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

        integer(4),intent(in)  :: mode      !  1: MC;  2:  MCMC-MH
        integer(4),intent(in)  :: imcmax    ! Länge des MC-Arrays
        integer(4),intent(in)  :: kqtyp     ! for:  1: output quantity; 2: DT;  3: DL

        real(rn)              :: yshort,ph,pl,yl,yh,ydiff,pgam
        integer(4)            :: i,imcp,k,ibest,kbest

!   shortest coverage interval:
! using a simple search
        yshort = 1.E+30_rn
        pgam = (one-W1minusG)
        imcp = int(pgam*imcmax)
        ibest = 0
! write(63,'(a,i0,a,es12.5,2(a,i0))') 'imcp=',imcp,' Wert(imcp)=', &
!               arraymc(imcp,kqtyp),' imcmax=',imcmax
        do i=0,imcp
            pl = real(i,rn) / real(imcmax,rn)
            ph = pgam - pl
            k  = int((one - ph)*real(imcmax,rn))
            if(i == 0) then
                yl = zero
            else
                if(mode == 1) yl = arraymc(i,kqtyp)
                !!! if(mode == 2) yl = mh_chain(i)
                ! if(mode ==2 .and. mh_chain(i) > mh_chain(i+1)) write(28,*) 'searchBCI3: Sort-Fehler'
            end if
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

    module real(rn) function dpi_funcs(mwind,indeval,jp,ma,Fv1)

    ! calculates a partial derivative:
    !    d(afunc(jp)) / d(Messwert(mwind);  with afunc(jp) obtained by call funcs(indeval,...)

    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_params,        only: rn,eps1min,zero,one
    use top,              only: dpafact
    use UR_Gleich,        only: Messwert,missingval
    use UR_Linft,         only: use_WTLS

    implicit none

    integer(4),intent(in)    :: mwind      ! Messwert index of the variable, with respect to which
    ! a partial derivative is calculated
    integer(4),intent(in)    :: indeval    ! number of the equation, of which the derivative is calculated
    integer(4),intent(in)    :: jp         ! index of afunc(), so that afunc(jp) = function value
    integer(4),intent(in)    :: ma         ! length of array afunc
    real(rn),intent(in)      :: Fv1        ! value of the unmodified function, supplied externally

    real(rn)       :: fv1m,Fv2,dpa,afunc(3)

    Fv1m = Fv1
    if(abs(fv1m-missingval) < eps1min) then
        call funcs(indeval,afunc)
        Fv1m = afunc(jp)
    end if
    dpa = Messwert(mwind) * dpafact(Messwert(mwind)) - Messwert(mwind)    ! Increment of parameter
    if(use_WTLS) then
        dpa = Messwert(mwind) * (one + (one - dpafact(Messwert(mwind)))*10._rn) - Messwert(mwind)  ! 14.7.2023
    end if
    Messwert(mwind) = Messwert(mwind) + dpa                 ! modify parameter with index mwind
    call funcs(indeval,afunc)
    Fv2 = afunc(jp)
    dpi_funcs = (Fv2/dpa - Fv1m/dpa)    ! partial derivative with respect to parameter Messwert(mwind)
    Messwert(mwind) = Messwert(mwind) - dpa   ! restore Messwert(mwind)

    if(abs(dpi_funcs) < 1.E-22_rn) dpi_funcs = zero      ! important

end function dpi_funcs

!#################################################################################


module subroutine matwrite(xmat,mm,nn,kunit,frmt,ctext)

    ! writes a matrix xmat(m,n) to the unit number kunit, uses the format
    ! frmt for the write-statement, and writes a headline ctext

    !     Copyright (C) 2020-2023  Günter Kanisch

    use UR_params,     only: rn
    implicit none

! integer(4),intent(in)     :: m,n        ! physical dims
    integer(4),intent(in)       :: mm,nn      ! dims to be printed
    real(rn),intent(in)         :: xmat(:,:)
    integer(4),intent(in)       :: kunit
    character(len=*),intent(in) :: frmt
    character(len=*),intent(in) :: ctext

    integer(4)          :: i,j,m,n

    m = ubound(xmat,dim=1)
    n = ubound(xmat,dim=2)
    write(kunit,*)
    if(len_trim(ctext) > 0) write(kunit,*) trim(ctext)
    do i=1,mm
        WRITE(kunit,frmt) xmat(i,1:nn)
    end do
    write(kunit,*)

end subroutine matwrite

!###############################################################################################

module RECURSIVE SUBROUTINE quick_sort_r(list,order)
use UR_params,    only: rn

! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.

IMPLICIT NONE
REAL(rn), DIMENSION (:), INTENT(IN OUT)  :: list
INTEGER, DIMENSION (:), INTENT(OUT)  :: order

! Local variable
INTEGER(4)       :: i

if(ubound(list,dim=1) < 1) return
if(size(order) > 0) then
    DO i = 1, size(order)    ! SIZE(list)
        order(i) = i
    END DO
end if

CALL quick_sort_1_r(1, SIZE(list))

CONTAINS

RECURSIVE SUBROUTINE quick_sort_1_r(left_end, right_end)

    INTEGER, INTENT(IN) :: left_end, right_end

    !     Local variables
    INTEGER             :: i, j, itemp
    REAL(rn)            :: reference, temp
    INTEGER, PARAMETER  :: max_simple_sort_size = 6

    IF (right_end < left_end + max_simple_sort_size) THEN
        ! Use interchange sort for small lists
        CALL interchange_sort_r(left_end, right_end)

    ELSE
        ! Use partition ("quick") sort
        reference = list((left_end + right_end)/2)
        i = left_end - 1; j = right_end + 1

        DO
            ! Scan list from left end until element >= reference is found
            DO
                i = i + 1
                IF (list(i) >= reference) EXIT
            END DO
            ! Scan list from right end until element <= reference is found
            DO
                j = j - 1
                IF (list(j) <= reference) EXIT
            END DO


            IF (i < j) THEN
                ! Swap two out-of-order elements
                temp = list(i); list(i) = list(j); list(j) = temp
                if(size(order) > 1) then
                    itemp = order(i); order(i) = order(j); order(j) = itemp
                end if
            ELSE IF (i == j) THEN
                i = i + 1
                EXIT
            ELSE
                EXIT
            END IF
        END DO

        IF (left_end < j) CALL quick_sort_1_r(left_end, j)
        IF (i < right_end) CALL quick_sort_1_r(i, right_end)
    END IF

END SUBROUTINE quick_sort_1_r


SUBROUTINE interchange_sort_r(left_end, right_end)

    INTEGER, INTENT(IN) :: left_end, right_end

    !     Local variables
    INTEGER             :: i, j, itemp
    REAL(rn)            :: temp

    DO i = left_end, right_end - 1
        DO j = i+1, right_end
            IF (list(i) > list(j)) THEN
                temp = list(i); list(i) = list(j); list(j) = temp
                if(size(order) > 1) then
                    itemp = order(i); order(i) = order(j); order(j) = itemp
                end if
            END IF
        END DO
    END DO

END SUBROUTINE interchange_sort_r

END SUBROUTINE quick_sort_r

!#######################################################################

module RECURSIVE SUBROUTINE quick_sort_i(list,order)

    ! Quick sort routine from:
    ! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
    ! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
    ! Modified by Alan Miller to include an associated integer array which gives
    ! the positions of the elements in the original order.

    IMPLICIT NONE
    integer(4), DIMENSION (:), INTENT(IN OUT)  :: list
    INTEGER(4), DIMENSION (:), INTENT(OUT)  :: order

    ! Local variable
    INTEGER(4)       :: i

    if(size(list) < 1) return
    if(size(order) > 0) then
        DO i = 1, size(order)   !  SIZE(list)
            order(i) = i
        END DO
    end if

CALL quick_sort_1_i(1, SIZE(list))

CONTAINS

    RECURSIVE SUBROUTINE quick_sort_1_i(left_end, right_end)

        INTEGER, INTENT(IN) :: left_end, right_end

        !     Local variables
        INTEGER(4)          :: i, j, itemp
        integer(4)          :: reference, temp
        INTEGER, PARAMETER  :: max_simple_sort_size = 6

        IF (right_end < left_end + max_simple_sort_size) THEN
            ! Use interchange sort for small lists
            CALL interchange_sort_i(left_end, right_end)

        ELSE
            ! Use partition ("quick") sort
            reference = list((left_end + right_end)/2)
            i = left_end - 1; j = right_end + 1

            DO
                ! Scan list from left end until element >= reference is found
                DO
                    i = i + 1
                    IF (list(i) >= reference) EXIT
                END DO
                ! Scan list from right end until element <= reference is found
                DO
                    j = j - 1
                    IF (list(j) <= reference) EXIT
                END DO

                IF (i < j) THEN
                    ! Swap two out-of-order elements
                    temp = list(i); list(i) = list(j); list(j) = temp
                    if(size(order) > 1) then
                        itemp = order(i); order(i) = order(j); order(j) = itemp
                    end if
                ELSE IF (i == j) THEN
                    i = i + 1
                    EXIT
                ELSE
                    EXIT
                END IF
            END DO

            IF (left_end < j) CALL quick_sort_1_i(left_end, j)
            IF (i < right_end) CALL quick_sort_1_i(i, right_end)
        END IF

    END SUBROUTINE quick_sort_1_i


    SUBROUTINE interchange_sort_i(left_end, right_end)

        INTEGER, INTENT(IN) :: left_end, right_end

        !     Local variables
        INTEGER(4)          :: i, j, itemp
        integer(4)          :: temp

        DO i = left_end, right_end - 1
            DO j = i+1, right_end
                IF (list(i) > list(j)) THEN
                    temp = list(i); list(i) = list(j); list(j) = temp
                    if(size(order) > 1) then
                        itemp = order(i); order(i) = order(j); order(j) = itemp
                    end if
                END IF
            END DO
        END DO

    END SUBROUTINE interchange_sort_i

END SUBROUTINE quick_sort_i

!#######################################################################

module SUBROUTINE kaiser(a, nrows, n, eigenv, trace, sume, ier)

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

    !*************************************************************************

    IMPLICIT NONE
    REAL (rn), INTENT(IN OUT) :: a(:,:)
    INTEGER, INTENT(IN)       :: nrows
    INTEGER, INTENT(IN)       :: n
    REAL (rn), INTENT(OUT)    :: eigenv(:)
    REAL (rn), INTENT(OUT)    :: trace
    REAL (rn), INTENT(OUT)    :: sume
    INTEGER, INTENT(OUT)      :: ier

    ! Local variables

    REAL (rn), PARAMETER :: small = 1.0e-12_rn
    INTEGER              :: i, iter, j, k, ncount, nn
    REAL (rn)            :: absp, absq, COS, ctn, eps, &
                            halfp, p, q, SIN, ss, TAN, temp, xj, xk

    !   CALCULATE CONVERGENCE TOLERANCE, EPS.
    !   CALCULATE TRACE.   INITIAL SETTINGS.

    ier = 1
    IF(n < 1 .OR. n > nrows) RETURN
    ier = 0
    iter = 0
    trace = zero
    ss = zero
    DO j = 1,n
        trace = trace + a(j,j)
        DO i = 1,n
            ss = ss + a(i,j)**2
        END DO
    END DO
    sume = zero
    eps = small*ss/n
    nn = n*(n-1)/2
    ncount = nn

    !   ORTHOGONALIZE PAIRS OF COLUMNS J & K, K > J.

20  DO j = 1,n-1
        DO k = j+1,n

    !   CALCULATE PLANAR ROTATION REQUIRED

            halfp = zero
            q = zero
            DO i = 1,n
                xj = a(i,j)
                xk = a(i,k)
                halfp = halfp + xj*xk
                q = q + (xj+xk) * (xj-xk)
            END DO
            p = halfp + halfp
            absp = ABS(p)

    !   If P is very small, the vectors are almost orthogonal.
    !   Skip the rotation if Q >= 0 (correct ordering).

            IF (absp < eps .AND. q >= zero) THEN
                ncount = ncount - 1
                IF (ncount <= 0) GO TO 160
                CYCLE
            END IF

    !   Rotation needed.

            absq = ABS(q)
            IF(absp <= absq) THEN
                TAN = absp/absq
                COS = one/SQRT(one + TAN*TAN)
                SIN = TAN*COS
            ELSE
                ctn = absq/absp
                SIN = one/SQRT(one + ctn*ctn)
                COS = ctn*SIN
            END IF
            COS = SQRT((one + COS)*half)
            SIN = SIN/(COS + COS)
            IF(q < zero) THEN
                temp = COS
                COS = SIN
                SIN = temp
            END IF
            IF(p < zero) SIN = -SIN

    !   PERFORM ROTATION

            DO i = 1,n
                temp = a(i,j)
                a(i,j) = temp*COS + a(i,k)*SIN
                a(i,k) = -temp*SIN + a(i,k)*COS
            END DO
        END DO
    END DO
    ncount = nn
    iter = iter + 1
    IF(iter < 10) GO TO 20
    ier = 2

    !   CONVERGED, OR GAVE UP AFTER 10 ITERATIONS

160 DO j = 1,n
        temp = SUM( a(1:n,j)**2 )
        eigenv(j) = SQRT(temp)
        sume = sume + eigenv(j)
    END DO

    !   SCALE COLUMNS TO HAVE UNIT LENGTH

    DO j = 1,n
        IF (eigenv(j) > zero) THEN
            temp = one/eigenv(j)
        ELSE
            temp = zero
        END IF
        a(1:n,j) = a(1:n,j)*temp
    END DO

    RETURN
END SUBROUTINE kaiser

!#######################################################################

module RECURSIVE SUBROUTINE quick_sort2_i(list,order)

    ! This routine is a modified version of:    quick_sort_i(list,order)
    ! It is modifed such (GK), that only the output array order is sorted,
    ! while the array list to be sorted now remains unsorted (intent(in)).
    !
    ! Quick sort routine from:
    ! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
    ! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
    ! Modified by Alan Miller to include an associated integer array which gives
    ! the positions of the elements in the original order.

    IMPLICIT NONE
    !!! integer(4), DIMENSION (:), INTENT(IN OUT)  :: list
    integer(4), DIMENSION (:), INTENT(IN)   :: list               ! changed (GK)
    INTEGER(4), DIMENSION (:), INTENT(OUT)  :: order

    ! Local variable
    INTEGER(4)       :: i

    if(size(list) < 1) return
    if(size(order) > 0) then
        DO i = 1, size(order)   !  SIZE(list)
            order(i) = i
        END DO
    end if

    CALL quick_sort2_1_i(1, SIZE(list))

    CONTAINS

    RECURSIVE SUBROUTINE quick_sort2_1_i(left_end, right_end)

        INTEGER, INTENT(IN) :: left_end, right_end

        !     Local variables
        INTEGER(4)          :: i, j, itemp
        integer(4)          :: reference, temp
        INTEGER, PARAMETER  :: max_simple_sort_size = 6

        IF (right_end < left_end + max_simple_sort_size) THEN
            ! Use interchange sort for small lists
            CALL interchange_sort2_i(left_end, right_end)

        ELSE
            ! Use partition ("quick") sort
            reference = list(order((left_end + right_end)/2))
            i = left_end - 1; j = right_end + 1

            DO
                ! Scan list from left end until element >= reference is found
                DO
                    i = i + 1
                    IF (list(order(i)) >= reference) EXIT
                END DO
                ! Scan list from right end until element <= reference is found
                DO
                    j = j - 1
                    IF (list(order(j)) <= reference) EXIT
                END DO

                IF (i < j) THEN
                    ! Swap two out-of-order elements
                    ! temp = list(i); list(i) = list(j); list(j) = temp
                    if(size(order) > 1) then
                        itemp = order(i); order(i) = order(j); order(j) = itemp
                    end if
                ELSE IF (i == j) THEN
                    i = i + 1
                    EXIT
                ELSE
                    EXIT
                END IF
            END DO

            IF (left_end < j) CALL quick_sort2_1_i(left_end, j)
            IF (i < right_end) CALL quick_sort2_1_i(i, right_end)
        END IF

    END SUBROUTINE quick_sort2_1_i


        SUBROUTINE interchange_sort2_i(left_end, right_end)

            INTEGER, INTENT(IN) :: left_end, right_end

            !     Local variables
            INTEGER(4)          :: i, j, itemp
            integer(4)          :: temp

            DO i = left_end, right_end - 1
                DO j = i+1, right_end
                    IF (list(order(i)) > list(order(j))) THEN
                        ! temp = list(i); list(i) = list(j); list(j) = temp
                        if(size(order) > 1) then
                            itemp = order(i); order(i) = order(j); order(j) = itemp
                        end if
                    END IF
                END DO
            END DO

        END SUBROUTINE interchange_sort2_i

    END SUBROUTINE quick_sort2_i

!#######################################################################
end submodule Num1a

