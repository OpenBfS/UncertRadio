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

module WTLS
    use UR_types,  only: rn
    use ur_params, only: ZERO, TWO, EPS1MIN

contains


!#######################################################################

    subroutine GlsqUR2(aG, covarG, ifehl)

        !     Copyright (C) 2014-2024  Günter Kanisch

        ! Subroutine for preparing a call to E7LSQ1UR, which in turn calls LsqGen for doing
        ! weighted total least squares (WTLS).
        !
        ! This routine is a modified version of the E7LSQ1 routine of the Datan-Library
        ! described in the textbook by S. Brandt:
        !
        !   Datan-Library (Fortran) from:
        !   Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und Computerprogrammen;
        !   4. Auflage. Spektrum, Akademischer Verlag, Heidelberg-Berlin. In German.
        !   This text book is also available in an English version.
        !
        ! The calling sequence within the UncertRadio program is:
        !   call Linf() --> call GlsqUR2(l) --> call E7LSQ1UR()  --> call LsqGen()
        !
        !  GlsqUR2 prepares the input data in a way in which they are required by the routine
        !  E7LSQ1UR. It returns the final vector aG of parameters fitted by WTLS and the associated
        !  covariance matrix covarG to the calloing routine Linf.

        use ur_lsqg, only: maxnr, maxm, maxn
        use ur_derivats, only: ex1_sline, dervtype
        use ur_linft,      only: ma,chisq,chisqr_wtls,cofact,cofactlyt,numd, &
                                 mfit,ifit,nchannels,ncofact,parfixed,posdef,tnstep,kfitp,mxind, &     ! 5.8.2023
                                 dnetrate,sdnetrate,fixedrate,sdfixedrate,wtls_wild,covx,klincall
        use ur_gleich_globals,  only: klinf,kegr,upropa_on,rnetmodi,kableitnum
        use ur_general_globals, only: fname,mcsim_on,ableit_fitp, results_path
        use fparser,       only: evalf, evalerrmsg
        use ur_dlim,       only: iteration_on,limit_typ
        use ur_mcc,        only: kqtypx,imc
        use urdate,        only: get_formated_date_time
        use usub3,         only: findmessk
        use num1,          only: funcs,matwrite
        use top,           only: wrstatusbar
        use fcvx,          only: findcovx
        use translation_module, only: T => get_translation

        implicit none

        real(rn), intent(inout) :: aG(ma)              ! vector of fit parameters (ma <= 3))
        real(rn), intent(out)   :: covarG(ma,ma)       ! covariance matrix of the fit parameters
        integer, intent(out)    :: ifehl

        integer           :: i,nn,nm,nnr,nstep,kunit,k
        integer           :: nred,irun,nwh,m1,m2

        real(rn)          :: afunc(ma)
        real(rn)          :: t_array(numd*ma),s(numd),ds(numd)
        integer           :: mav,k0,klu,kqt,list(3)
        LOGICAL           :: printout
        character(len=1)  :: cmessk(3)
        character(:),allocatable :: outfile
        character(len=100) :: crestrict
        !-----------------------------------------------------------------------
        cmessk = (/ 'A','B','C' /)

        ex1_sline = .false.
        allocate(character(len=300) :: outfile)

        klu = klinf
        if(kfitp(1) > 0) klu = kfitp(1)-1+kEGr

        dervtype = 'A'     ! analytical partial derivatives
        ! if(.not.iteration_on) dervtype = 'N'    ! numerical partial derivatives
        kunit = 23
        ifehl = 0
        !----------------------------------------------
        kqt = 1
        if(iteration_on .and. limit_typ == 1) kqt = 2
        if(iteration_on .and. limit_typ == 2) kqt = 3

        printout = .FALSE.
        ! printout = .TRUE.

        printout = printout .and. .not.MCsim_On .and. .not.upropa_on .and. .not.Rnetmodi &
            .and. .not.ableit_fitp

        ! choose a further condition to reduce the output in case of iterout = .true. :
        if(.not.mcsim_on .and. kqt == 1 .and..not.iteration_on .and. kableitnum == 0 &
            .and. klincall == 1) printout = .true.
        if(klincall > 1)  printout = .false.

        !----------------------------------------------
        k0 = 0
        do i=1,ma
            list(i) = ifit(i)
            if(list(i) == 1) then
                k0 = k0 + 1
            end if
            ! if(ifit(i) > 1) list(i) = 0
        end do

        if(printout) then

            outfile = results_path // 'wtlsout.txt'
            kunit = 23
            write(kunit,'(a)')  &
                'datum: ' // get_formated_date_time() // ' input file: ' // trim(fname)
            ! write(kunit,*) 'iteration_on=',iteration_on
        end if

        !--------------
        !!      ifit = 1     ! für E7LSQ: IA=1 means fit parameter, =0 means fix the parameter

        ncofact = 0
        cofact = cofactlyt

        t_array = ZERO
        s = ZERO
        ds = ZERO

        mxind = mfit       ! 5.8.2023
        mav = mfit
        nwh = numd/nchannels
        ! loop over the count rates:
        ! Note: net count rate covariances are added in E7lsq1UR!

        ! Y values:
        s(1:numd) = dnetrate(1:numd)
        if(parfixed) s(1:numd) = s(1:numd) - fixedrate(1:numd)
        ! uncertainties of Y values:
        ds(1:numd) = sdnetrate(1:numd)
        if(parfixed) ds(1:numd) = sqrt( ds(1:numd)**TWO + SDfixedrate(1:numd)**TWO )

        do i=1,numd
            ! X values:
            call Funcs(i,afunc)
            k0 = 0
            do k=1,ma
                if(ifit(k) >= 2) cycle
                k0 = k0 + 1
                t_array(mav*i - k0 + 1) = afunc(k)
            end do
        end do

        call Findcovx(1,kunit,ifehl)
        if(ifehl == 1) return

        if(printout .and. .not.iteration_on) then
            crestrict = ''
            m1 = min(30, numd*ma)
            m2 = min(30, numd*ma)
            if(m1 == 30 .and. numd*ma > 30) write(crestrict,'(a,i0,a,i0,a)') ' (restricted to ',m1,' x ',m2,')'
            call matwrite(covx,m1,m2,kunit,'(50es11.3)',  &
                'GlsqUR2: Matrix covx:   (contains only non-diagonal elements)' // trim(crestrict) )
        end if
        !-----------------------------------------------------------------------------------------

        covarG = ZERO
        nstep = 10        ! 5
        maxnr = mav
        maxm = numd
        maxn = numd*(mav+1)

        nn = maxn
        nm = maxm
        nnr = mav

        if(printout) then
            write(kunit,'(3(a,i3))') 'numd=',numd,'  mfit=',mfit,'  mav=',mav
            write(kunit,'(a,3i3)') 'maxnr,maxm,maxn=',maxnr,maxm,maxn
            write(kunit,'(a,3i3)') 'nn,nm,nnr=',nn,nm,nnr
            write(kunit,'(a,i2,a,f15.12)') 'ncofact=',ncofact,'  cofactlyt=',cofactlyt
        end if

        if(printout) write(kunit,'(/a)') &
            ' ====================== Calculations with WTLS:  ========================='

        nred = mfit
        irun = 1
        Tnstep = nstep
        if(irun > 1 .and. printout) write(kunit,'(/,1x,a)')    &
            '     Now calculation with ''fitted uncertainty'':'

        call E7LSQ1UR(aG,ifit,nm,nn,nnr,t_array,s,ds,covarG,chisq,Tnstep,kunit, &
            nred,irun,printout)
        if(Tnstep < 0) then
            ifehl = 1
            WTLS_Wild = .true.
        end if
        if(.not.posdef) then
            if(ncofact > 3) then
                ifehl = 1
                call WrStatusBar(4, T("Abortion. Matrix not posdef!"))
                return
            else
                posdef = .true.

            end if
        end if
        Chisqr_WTLS = chisq/real(MAX(numd-mfit,1),rn)

        if(.false. .and. printout .and. kqtypx <= 2 .and.imc < 50) then
            write(kunit,'(1x,a,/,1x,a,3es12.4,a,3es12.4)')                   &
                'Result of LSQGEN:','Params=',(aG(i),i=1,ma),  &
                '   u(Params)=',(sqrt(covarG(i,i)),i=1,ma)
            write(kunit,'(1x,a,es12.4)') 'Chisqr_WTLS= ',Chisqr_WTLS
        end if

        if(allocated(outfile)) deallocate(outfile)


    end subroutine GlsqUR2


            !-----------------------------------------------------------

            subroutine E7LSQ1UR(x,list,m,n,nr,t,s,ds,covar,chisq,nstep,  &
                kunit,nred,irun,printout)

                ! Subroutine for preparing a call to LsqGen for doing weighted total least squares (WTLS)
                !
                ! This routine is a modified version (GK) of the E7LSQ1 routine of the Datan-Library
                ! described in the textbook by S. Brandt:
                !
                !   Datan-Library (Fortran) from:
                !   Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und Computerprogrammen;
                !   4. Auflage. Spektrum, Akademischer Verlag, Heidelberg-Berlin. In German.
                !   This text book is also available in an English version.
                !
                ! The calling sequence within the UncertRadio program is:
                !   call Linf() --> call GlsqUR2(l) --> call E7LSQ1UR()  --> call LsqGen()
                !
                !  E7LSQ1UR prepares a combined covariance matrix, which includes the variances/covariances
                !  of measured y values (in this case count rates of a decay curve) and also the
                !  the variances/covariances of other x input quantities (like decay constants, or detection
                !  efficiencies). E7LSQ1UR calls the LsqGen routine and organizes the printout of input data
                !  and output from LSQGEN.
                !
                !  11.3.2022: Before calling LSQGEN, the set of fit parameters (and associated covariance matrix)
                !  is reduced to the subset xred of parameters to be fitted (number mfit).
                !

                use UR_LSQG            ! (i.e., maxn,maxnr,maxm,mad)
                use UR_Derivats,   only: dervtype,dfda,dfde
                use UR_Gleich_globals,     only: Messwert,kpoint,StdUnc,kEGr,ngrs,ncov
                use UR_Linft,      only: ma,nchannels,nkovzr,k_rbl,mfit,nccg,numd, &
                                        konstant_r0,R0k,sdR0k,WTLS_wild,ifit,Chisqr_NLS,  &
                                        d0zrate,sd0zrate,parfixed,cov_fixed,fixedrate,covx, &
                                        Chis_test,posdef,ncofact,cofact,klincall, &
                                        cauchy_failed3,compare_WTLS,mxind
                use UR_Mcc,        only: imc,kqtypx
                use ur_general_globals,  only: MCSim_on
                use UR_DLIM,       only: limit_typ , Iteration_on
                use Usub3,         only: FindMessk
                use UR_params,     only: ZERO,TWO
                use Num1,          only: matwrite

                implicit none

                real(rn), intent(inout)  :: x(ma)        ! vector of fit parameters
                integer, intent(inout)   :: list(ma)     ! fit param: 1: yes ;  2: fixed; 3: not used
                integer, intent(in)      :: m            ! number of constraining equations, number of count rates
                integer, intent(in)      :: n            ! number measurements / measured values
                integer, intent(inout)   :: nr           ! number of parameters
                real(rn), intent(in)     :: t(maxm*ma)   ! vector of X values
                real(rn), intent(in)     :: s(maxm)      ! vector of Y values  (count rates)
                real(rn), intent(in)     :: ds(maxm)     ! vector of Y uncertainties
                real(rn), intent(out)    :: covar(ma,ma) ! covariance matrix of fit parameters
                real(rn), intent(out)    :: chisq
                integer, intent(inout)   :: nstep        ! number of iterations, or, < 0: error indication
                integer, intent(inout)   :: kunit        ! unit for printout
                integer, intent(in)      :: nred         ! number of fitted parameters ( <= nr)
                logical, intent(inout)   :: printout
                integer, intent(in)      :: irun         ! Number of the run

                integer            :: i,k,ir,kr,jx,nmk,k1,k2,kb1,kb2,k0
                integer            :: ik1,ik2,ifail,m1,m2
                integer            :: nnew,messk,messk2,nwh
                integer            :: nmk2,kjun,kk,k01,k02,kqt,i_arr(m),ijx_arr(m),list2(3)

                real(rn)           :: rho(maxm),r,chisqr, cymin,cymax, tfit,schisqr
                real(rn)           :: xred(ma)

                real(rn),allocatable  :: y(:),cy(:,:),cx(:,:)

                real(rn)           :: tt(ma)
                character(len=60)  :: stformat
                character(len=100) :: crestrict

                !-----------------------------------------------------------------------
                !  compare_WTLS is set in Uncw_init !

                kqt = 1
                if(iteration_on .and. limit_typ == 1) kqt = 2
                if(iteration_on .and. limit_typ == 2) kqt = 3

                if(allocated(dfda)) deallocate(dfda,dfde)
                allocate(dfda(nr))
                if(mxind == 1) then
                    allocate(dfde(mxind+1))
                else
                    allocate(dfde(mxind+1))
                end if

                xred(1:ma) = x(1:ma)
                k0 = 0
                do i=1,ma
                    if(list(i) == 1) then
                        k0 = k0 + 1
                        xred(k0) = x(i)
                    end if
                end do
                if(mfit < ma) then
                    do i=mfit+1,ma
                        xred(i) = ZERO
                    end do
                end if
                list2 = 1
                allocate(y(m*(mfit+1)), cy(m*(mfit+1),m*(mfit+1)), cx(nred,nred) )

                stformat = '(2F12.7,2x,3f12.7,3x,3es14.7,3x,3es14.7)'
                if(mfit == 2) stformat = '(2F12.7,2x,2f12.7,3x,2es14.7,3x,2es14.7)'
                if(mfit == 1) stformat = '(2F12.7,2x,1f12.7,3x,1es14.7,3x,1es14.7)'

                if(klincall > 1) printout = .false.
                if(irun == 1 .and. printout) then
                    ! identify program to user
                    write(kunit,'(/,a)') ' Subroutine E7LSQ1UR demonstrates use of LSQGEN.'

                    ! write table of data
                    if(mfit == 3) write(kunit,'(/,a)') '   S           DS            T1-3                                 DT1-3' &
                        // '                                        urel(T1-3)'
                    if(mfit == 2) write(kunit,'(/,a)') '   S           DS            T1-2                     DT1-2' &
                        // '                          urel(T1-2)'
                    if(mfit == 1) write(kunit,'(/,a)') '   S           DS            T1               DT1' &
                        // '            urel(T1)'
                    do  i=1,m
                        do k=1,mfit
                            ik1 = ma*(i-1) + k
                            tt(k) = ZERO
                            if(t(mfit*i-k+1) > ZERO) tt(k) = sqrt(covx(ik1,ik1))/t(mfit*i-k+1)
                        end do
                        write(kunit,stformat)  s(i),ds(i),   &
                            (t(mfit*i-kk+1),kk=1,mfit),  &
                            (sqrt(covx(ma*(i-1)+kk,ma*(i-1)+kk)),kk=1,mfit), (tt(kk),kk=1,mfit)
                        !!!    (sqrt(covx(mfit*(i-1)+kk,mfit*(i-1)+kk)),kk=1,mfit), (tt(kk),kk=1,mfit)
                    end do

                    write(kunit,*)
                    schisqr = ZERO
                    do i=1,m
                        nmk = FindMessk(i)
                        tfit = ZERO
                        do k=1,mfit
                            if(kqtypx == 2 .and. k == kEgr) cycle
                            tfit = tfit + t(mfit*i-k+1)*xred(k)
                        end do
                        tfit = tfit - R0k(nmk)
                        if(parfixed) tfit = tfit - fixedrate(i)
                        schisqr = schisqr + ( (tfit - s(i))/ds(i) )**TWO
                        write(kunit,'(a,i2,2x,a,f10.5,2x,a,f10.5,a,f10.5,2x,a,f7.2,2(2x,a,f10.5))') 'i=',i,' MW=',Messwert(ngrs+ncov+i), &
                            ' Rn=',s(i),' Lfit=',tfit,   &
                            'relab%=',(tfit-s(i))/s(i)*100.,'R0=',d0zrate(i),' uR0=',sd0zrate(i)
                    end do
                    schisqr = schisqr/real(m-mfit,rn)
                    write(kunit,*) '   tchisqr=',sngl(schisqr), '  x=',(sngl(xred(i)),i=1,mfit)
                end if

        !  Note: It is helpful to take care about zero-elements in the main diagonal
        !  of the covariance matrix very early!

        y = ZERO
        cy = ZERO
        rho = ZERO
        jx = mfit + 1

        i_arr = [(i,i=1,m)]
        ijx_arr = i_arr*jx
! "Y values":       (count rates)
        y(ijx_arr) = s(1:m)
! "X values":      (efficiencies, ...)
        do kk=1,mfit
            y(ijx_arr-kk) = t(i_arr*mfit-kk+1)
        end do
! Variances of the "Y values" :
! cy(ijx_arr, ijx_arr  ) = ds(i_arr)**two  ! does not work this way: see do loop below
!

        do i=1,m
            !! Variances of the "Y values" :
            cy(i*jx  ,i*jx  ) = ds(i)**TWO

            ! Variances oof the "X values":
            nmk = FindMessk(i)    ! which counting channel (A,B,c) does i belong to?

            ! Here: non-diagonal elements:
            ! Covariances between "Y values"):
            if(nkovzr == 1) then      ! covariances of count rates are to be considered
                if(i > 1) then
                    do k=1,i-1
                        nmk2 = FindMessk(k)      ! which counting channel (A,B,c) does k belong to?
                        if(konstant_r0 .and. nmk == nmk2 ) then
                            cy(k*jx,i*jx) = sdR0k(nmk)**TWO
                        end if
                        if(k_rbl > 0) then
                            if(StdUnc(kpoint(k_rbl)) > ZERO) cy(k*jx,i*jx) = cy(k*jx,i*jx) + StdUnc(kpoint(k_rbl))**TWO
                        end if
                        if(parfixed) cy(k*jx,i*jx) = cy(k*jx,i*jx) + cov_fixed(k,i)
                        cy(i*jx,k*jx) = cy(k*jx,i*jx)
                    end do
                end if
            else
                if(i == 1 .and. printout) write(23,*) 'non-diagonal covariances of Y values not used!'
            end if
        end do

        if(.false. .and. (printout .or. (compare_WTLS .and. printout))) then
            call matwrite(cy,m*(mfit+1),m*(mfit+1),23,'(120es11.3)', &
                'E7: Matrix cy, here containing only variances/covariances of the Y values (net count rates):')
        end if

        nwh = numd/nchannels
        if(nccg > 0) then
            ! Fill in non-diagonal covariances between X values:
            do k1=1,m            ! number of measurement (count rate)
                messk = FindMEssk(k1)
                k01 = 0
                do kb1=1,ma          ! parameter
                    if(ifit(kb1) >= 2) cycle
                    k01 = k01 + 1       ! number of parameters to be fitted
                    ik1 = ma*(k1-1) + kb1
                    !ik1 = mfit*(k1-1) + kb1
                    do k2=1,m
                        messk2 = FindMessk(k2)
                        k02 = 0
                        do kb2=1,ma
                            if(ifit(kb2) >= 2) cycle
                            k02 = k02 + 1
                            ik2 = ma*(k2-1) + kb2
                            !ik2 = mfit*(k2-1) + kb2
                            if(k1*jx-k01 == k2*jx-k02) then
                                !if(klincall == 1) then
                                !  write(23,*) '   E7, B : cy(',k1*jx-k01,',',k2*jx-k02,') : ik1,ik2=',ik1,ik2,'  k1,k2=',k1,k2,' kb1,kb2=',kb1,kb2, &
                                !              '  covx(ik1,ik2)=',sngl(covx(ik1,ik2)),' cy=',sngl( cy(k1*jx-k01,k2*jx-k02) )
                                !
                                !end if
                            end if
                            cy(k1*jx-k01,k2*jx-k02) = cy(k1*jx-k01,k2*jx-k02) + covx(ik1,ik2)
                            if(.false. .and. klincall == 1 .and. kqt == 1) then
                                write(23,'(a,i2,a1,i2,a,i2,1x,i2,2(a,i2,1x,i2),a,es11.4,a,es11.4)')  &
                                    '   E7, B : cy(',k1*jx-k01,',',k2*jx-k02,') : ik1,ik2=',ik1,ik2,'  k1,k2=',k1,k2,' kb1,kb2=',kb1,kb2, &
                                    '  covx(ik1,ik2)=',covx(ik1,ik2),' cy=', cy(k1*jx-k01,k2*jx-k02)
                            end if
                        end do
                    end do
                end do
            end do
        else
        ! if(printout) write(23,*) 'non-diagonal covariances of X values not used!'
        end if

        cymin = 1.E+30_rn
        cymax = -1.E+30_rn

!Repair cases with diagonal elements very close to zero
! according to ISO GUM suppl. 2, page 6, note 4
        call cy_repair(cy,m*(mfit+1))

        ifail = 0
        cauchy_failed3 = .false.
        nnew = m*(mfit+1)

        if((printout .and. .not.iteration_on) .or. (compare_WTLS .and. printout)) then
            write(23,'(a,120es15.7)') '   Array y: ',(y(i),i=1,nnew)
            write(23,*)
            crestrict = ''
            m1 = min(30, nnew)
            if(m1 == 30 .and. nnew > 30) write(crestrict,'(a,i0,a,i0,a)') ' (restricted to ',m1,' x ',m2,')'
            call matwrite(cy,m1,m1,23,'(50es11.3)',  &
                'E7: Matrix cy, incl. variances/covariances of the t values (decay function values):  ' &
                // trim(crestrict) )
        end if

        if(printout .or. (compare_WTLS .and. printout)) then
            ! header for output of results
            write(kunit,'(/,a)') ' Performing fit with LSQGEN'

            write(kunit,'(4(A,I3),A,3I2)') ' N = ',n,', NR = ',nr,  &
                ', NRED = ',nred,', M = ',m,', LIST = ',list
            write(kunit,'(1x,a,a1)') 'Type of derivative: ',dervtype

            write(kunit,'(A,3(ES16.9,1x))') ' first approx.: Params = ',(xred(i),i=1,mfit)
            write(kunit,*) 'Chisqr_NLS=',chisqr_nls

            ! write(kunit,'(/,a,/)') 'Covariances of Y-values:'
            !do i=1,m
            !  write(kunit,'(10es9.2)') (cy(2*k,2*i),k=1,m)
            !end do
            !write(kunit,'(/,a,/)') 'Uncertainties of Y-values:'
            !write(kunit,'(120es9.2)') (SQRT(cy(jx*k,jx*k)),k=1,m)
            !write(kunit,'(/,a,/)') 'Uncertainties of X-values:'
            !write(kunit,'(120es9.2)') ((SQRT(cy(jx*k-ma-1+j,jx*k-ma-1+j)),j=1,m),k=1,m)
            !write(kunit,'(/,a,/)') 'relative uncertainties of X-values:'
            !write(kunit,'(120es9.2)') ((SQRT(cy(jx*k-ma-1+j,jx*k-ma-1+j))/y(jx*k-ma-1+j),j=1,m),k=1,m)
        end if

        chisq = ZERO
        cx = ZERO
        CALL lsqgen(y,cy,m,n,nr,nred,list2,xred,cx,r,nstep,printout)
        if(nstep == -1) then
            write(kunit,*) ' Internal problem occurred in LSQGEN: not OK!'
            write(66,*)    ' Internal problem occurred in LSQGEN: not OK!'
            GOTO 9000
        end if
        if(nstep == -3) then
            write(kunit,*) ' Num. Diff. AUXDRG : not OK!'
            write(66,*)    ' Num. Diff. AUXDRG : not OK!'
            GOTO 9000
        end if
        if(nstep == -2) then
            write(kunit,'(/,a)') ' Fit did not converge!'
            write(66,'(/,a)')    ' Fit did not converge!'
            GOTO 9000
        end if
        if(.not.posdef) then
            if(printout) write(kunit,*) 'After LSQGEN: posdef=',posdef,'  Return'
            return
        end if
        if(printout) write(kunit,*) 'After LSQGEN: posdef=',posdef
!  convergence successful:
        if(m-nred <= 1) then
            r = 1.E-17_rn
            chisq = r
            chisqr = r
        else
            chisq = r
            chisqr = r/real(m-nred,rn)
        end if
! Chi-Test following ISO 11929 (2010), Eq. (C.31)
        if(numd > mfit) Chis_test(2) = abs( chisq - real(numd-mfit,rn) ) /sqrt(2._rn*real(numd-mfit,rn))
        !if(printout) then
        ! call matwrite(cx,nred,nred,nred,nred,23,'(50es11.3)','E7: Matrix cx: ')
        !end if

        ! write(kunit,*) 'ma=',ma,' list2=',list2
        kjun = kunit
        covar = ZERO
        ir = 0
        do i=1,ma
            if(list(i) > 1 .or. list(i) == 0) CYCLE
            ir = ir + 1
            x(i) = xred(ir)
            kr = 0
            do k=1,ma
                if(list(k) > 1 .or. list(k) == 0) CYCLE
                kr = kr + 1
                covar(i,k) = cx(ir,kr)
            end do
        end do
        if(printout .and. nred > 0) then
            call matwrite(covar,ma,ma,kjun,'(10es14.6)',' covariance matrix CX=covar : ')
        end if

        if(.not.printout .and. .not.(compare_WTLS .and. printout)) goto 9000

! output of results
        write(kjun,'(/,A,es16.9,A,es16.9,A,I3,/,a,es16.9,2x,a,i5)')         &
            ' Result of fit: R=SSD = ',r,'  ChisqRed=',chisqr,  &
            '  NSTEP =',nstep,' StDev of Fit = ',SQRT(chisqr),'  m-nred=',m-nred
        if(MCSim_on) write(kjun,*) 'imc=',imc

! write(kjun,'(a,4(es16.9,1x))') ' Params =',x


        write(kjun,'(a,/,60("-"))') '  i   Param            u(Param)         covar triangle'
        do i=1,nr
            ! if(abs(xred(i)) < eps1min) CYCLE
            if(i == 1) then
                write(kjun,'(1x,i2,2(1x,es16.9))') i,x(i),sqrt(covar(i,i))
            else
                write(kjun,'(1x,i2,10(1x,es16.9))') i,x(i),sqrt(covar(i,i)),(covar(i,k),k=1,i-1)
            end if
        end do
        write(kjun,'(1x)')
        write(kjun,'(a,L1,a,f15.12,a,i2)') 'E7 at end:   posdef=',posdef,'  cofact=',cofact,'  ncofact=',ncofact
        write(kjun,*) '-------------------------------------------------------------------------------------'
        write(kjun,*)

9000    continue

        if(nstep < 0) WTLS_wild = .true.

    end subroutine E7lSQ1UR

!#######################################################################

    subroutine LsqGen(y,cy,m,n,nr,nred,list,x,cx,r,nstep,printout)

        ! Subroutine for doing weighted total least squares (WTLS)

        ! This routine is a modified version (GK) of the LSQGEN routine of the Datan-Library
        ! described in the textbook by S. Brandt:

        !   Datan-Library (Fortran) from:
        !   Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und Computerprogrammen;
        !   4. Auflage. Spektrum, Akademischer Verlag, Heidelberg-Berlin. In German.
        !   This text book is also available in an English version.
        !
        ! The calling sequence within the UncertRadio program is:
        !   call Linf() --> call GlsqUR2(l) --> call E7LSQ1UR()  --> call LsqGen()

        use UR_Derivats,        only: dervtype
        use ur_general_globals, only: MCSim_on
        use UR_DLIM,            only: iteration_on,limit_typ
        use UR_Gleich_globals,  only: kableitnum
        use UR_Linft,       only: posdef,cofact,cofactlyt, compare_WTLS
        use Brandt,         only: mtxchi,mtxchl,mtxlsc
        use Num1,           only: matwrite

        implicit none

        integer   , intent(in)     :: m                 ! number of constraining equations ##############
        integer   , intent(in)     :: n                 ! number measurements / measured values
        integer   , intent(in)     :: nr                ! number of parameters
        integer   , intent(in)     :: nred              ! number of parameter to be fitted (<=nr)
        real(rn), INTENT(IN OUT)   :: y(n)              ! in:  vector of measurements (Y values, count rates)
        ! out: vector of improved measurements
        real(rn), INTENT(IN OUT)   :: cy(n,n)           ! in:  Lower triangle mat as vector (len=n*(n+1)/2)
        ! in: covariance matrix
        ! out: covariance matr. of impproved measurements
        integer   , INTENT(IN OUT) :: list(nr)          ! fit param: 1: yes ;  2: fixed; 3: not used
        real(rn), INTENT(IN OUT)   :: x(nr)             ! vector of fit parameters
        real(rn), INTENT(IN OUT)   :: cx(nred,nred)     ! covariance matrix of fit parameters
        real(rn), INTENT(OUT)      :: r                 ! Chisq
        integer   , INTENT(IN OUT) :: nstep             ! number of iterations, or, if < 0: error indication
        logical,intent(in)         :: printout

        integer, PARAMETER      :: maxstp=100

        real(rn), PARAMETER     :: tt = 1.E-16_rn  ! 1.E-17_rn   ! tt=5.E-17_rn

        integer                 :: i,l,istep,k,ired,kqt,nrepeat,m1,m2
        real(rn)                :: rlst,xff
        real(rn),allocatable    :: d(:), t(:), b(:), u(:)
        integer   ,allocatable  :: i_arr(:)
        LOGICAL                 :: ok,covmat
        real(rn), Allocatable   :: fy(:,:),G(:,:),e(:,:),a2(:,:)       ! working areas



        integer              :: kkk
        LOGICAL              :: printG
        character(len=100)   :: crestrict

        real(rn),allocatable :: fin(:,:),fxcy(:,:)
        real(rn),allocatable :: a2_2(:,:),a2_3(:,:),fy_2(:,:)
        !-----------------------------------------------------------------------

        kqt = 1
        if(iteration_on .and. limit_typ == 1) kqt = 2
        if(iteration_on .and. limit_typ == 2) kqt = 3

        if(.not.allocated(fy)) allocate ( fy(1:n,1:n) )
        if(.not.allocated(G))  allocate ( G(1:n+nred,1:n+nred) )
        if(.not.allocated(e))  allocate ( e(1:m,1:n+nred) )
        if(.not.allocated(a2)) allocate ( a2(1:n+nred,1:n+nred-m) )

        if(.not.allocated(fin)) allocate ( fin(1:n,1:n) )
        if(.not.allocated(fxcy)) allocate ( fxcy(1:n,1:n) )
        if(.not.allocated(a2_2)) allocate ( a2_2(1:m,1:n) )
        if(.not.allocated(a2_3)) allocate ( a2_3(1:m,1:nred) )
        if(.not.allocated(fy_2)) allocate ( fy_2(1:m,1:m) )

        ! allocate(d(m), t(n+nred,1), b(n+nred,1), u(n+nred,1), i_arr(n), bb(n+nred), uu(n+nred))
        allocate(d(m), t(n+nred), b(n+nred), u(n+nred), i_arr(n))   ! , bb(n+nred), uu(n+nred))   !xxx

        i_arr = [(i,i=1,n)]

        printG = .False.         ! Warning: .true. produces a large amount of output into fort23.txt
        ! printG = .True.

        printG = printG .and. .not.MCSim_on
        printG = printG .and. kableitnum >= 0 .and. kqt == 2

        printG = printout ! safer option with respect to low output into fort23.txt
        ! if(kqt >= 2) printG = .true.

        nrepeat = 0
        posdef = .true.
        if(printG) then
            write(23,*)
            write(23,'(6(a,i0))') 'Begin of LSQGEN: n=',n,'  nred=',nred,'  nr=',nr, &
                '  m=',m,'    kqt=',kqt,' kableitnum=',kableitnum
            write(23,*) 'Vector y : ',(sngl(y(i)),i=1,n)
            write(23,*) 'Parameter vector x : ',(sngl(x(i)),i=1,nr)
        end if

! general case of least squares fitting (= weighted total least-squares)
        ok = .TRUE.
        covmat = .TRUE.
        if(nstep < 0) then
            covmat = .FALSE.
            nstep = ABS(nstep)
        end if
        if(nstep < 1) nstep = maxstp
        l = n + nred
        t(1:n+nred) = ZERO        !xxx

        if(compare_WTLS .and. printG) then
            call matwrite(cy,n,n,23,'(50es11.3)','Matrix cy in LsqGen:')
        end if
        fin(1:n,1:n) = cy(1:n,1:n)
        G(1:n,1:n)   = cy(1:n,1:n)

        !do j=1,n
        !  do i=1,n
        !    if(abs(cy(j,i) - cy(i,j)) > 1.E-13_rn) write(23,*) ' LSQGEN: matrix cy asymmetric: j,i = ',j,i
        !  end do
        !end do

        ! write(23,*) 'LSQGEN n, nr, m=',int(n,2),int(nr,2),int(m,2)
        posdef = .true.

        CALL mtxchi(cy)        ! inverts matrix cy by Cholesky decomposition
        cofactLyt = cofact

        if(.not.posdef) goto 60

        CALL mtxchl(cy,fy, posdef)    ! Cholesky-decomposition of the inverted matrix cy is copied to fy

        if(.false. .and. printG) then
            call matwrite(fy,n,n,23,'(50es11.3)','Matrix fy: = Cholesky-decomposition of cy after inverting:')
        end if
        cy(1:n,1:n) = G(1:n,1:n)         ! restore the original covariance matrix

        ! start iteration --------------------------------------------------
        r = ZERO

        do istep=1,nstep
            if(printG .and. compare_WTLS) then
                write(23,*)
                write(23,*) '  ISTEP (Iteration) =',istep, '  r = Chi-squared: ',sngl(r)
            end if
            rlst = r

            ! Equations 9.11.7, 9.11.8
            G = 0._rn
            G(nred+1:nred+n,nred+1:nred+n) = fy(1:n,1:n)  ! inserts submatrix fy=cy^-1=Gy into Matrix G
            ! starting in G at (nred+1,nred+1), from fy at (1,1)
            if(printG) then
                crestrict = ''
                m1 = min(30, l)
                m2 = m1
                if(m1 == 30 .and. l > 30) write(crestrict,'(a,i0,a,i0,a)') ' (restricted to ',m1,' x ',m2,')'
                call matwrite(G,m1,m1,23,'(50es11.3)','Matrix G (after inserting submatrix fy=cy^-1=Gy into matrix G): ' &
                    // trim(crestrict) )
            end if

            do k=1,m
                d(k) = -lsqgfn2(y,x,n,nr,k)
            end do
            if(printG) then
                write(23,*) 'Vector d of negative "function values":   istep=',int(istep,2)
                write(23,'(150es11.3)') (d(kkk),kkk=1,m)
            end if
            ! calculate numerical Derivatives:
            CALL auxdrg(x,y,m,n,nr,nred,list,e,ok,LsqGfn2)    ! e is now the matrix of derivatives
            if(printG .or. (compare_WTLS .and. printG) .or. .not.OK) then
                write(23,'(a,4(i0,1x),L1,a,a,a,a,i0)') 'after AUXDRG: Matrix e of derivatives:  (m, n, nred, l, ok=',m,n,nred,l,ok,' )',  &
                    '  Method: ',dervtype,' istep=',istep
                crestrict = ''
                m1 = min(30, m)
                m2 = min(30, n+nred)
                if(m1 == 30 .and. (m > 30 .or. n+nred > 30)) write(crestrict,'(a,i0,a,i0,a)') ' (restricted to ',m1,' x ',m2,')'
                call matwrite(e,m1,m2,23,'(50es11.3)','Matrix e of derivatives: ' // trim(crestrict) )
            end if

            if(.NOT.ok) then
                write(23,*) 'LSQGEN: ok=',ok
                nstep = -3
                GO TO 60
            end if
            b = matmul(G, t)  ! Multipl. upper triangular matrix G with vector t, output: vector b
            ! Multiply vector b with -1.
            b(1:l) = -b(1:l)
            if(printG) then
                write(23,*) 'Vector t (Sum of improvements):'
                write(23,'(150es11.3)') (t(kkk),kkk=1,l)
                write(23,*) 'Vector -b = Matrix F times vector t:'
                write(23,'(150es11.3)') (b(kkk),kkk=1,l)
                ! write(23,*) 'Vector u : ',(sngl(u(kkk,1)),kkk=1,l)            ! unknown for istep=1
            end if

            ! Scheme for solving LSQ under constraints (mtxlsc):
            !    (Fu - b)^2 = min
            !    with constraints  E*u = d
            !
            ! Input:
            !       F : Matrix A of derivatives with respect to parameters u
            !       b : Vector b = -F*t
            !       E : Matrix E of the condition  E*u = d = -c (c=function values)
            !       d : Vektor d = -c (c=function values)
            !       a2: working area
            !       0.: Fraction
            ! Output:
            !       u : Vector u of parameters

            !       t : {0     ) nred
            !           {s     ) n

            ! CALL mtxlsc(G,b,e,d,u,r,a2,l,l,m,zero,ok)   ! Solves LSQ with constraints
            ! CALL mtxlsc(G,bb,e,d,uu,r,a2,ZERO,ok)   ! Solves LSQ with constraints
            call mtxlsc(G,b,e,d,u,r,a2,ZERO,ok)   ! Solves LSQ with constraints
            if (.not. ok) then
                write(23,*) 'after call mtxlsc: ok=',ok,'  Solves LSQ with constraints;  nstep=-1'
                write(23,*) '   Matrix f of derivatives:  (m, n, nred, ok=',m,n,nred,ok,' )','  Method: ',dervtype
                call matwrite(G,m,n+nred,23,'(50es11.3)','matrix G: ')
                write(23,*) '   Vektor b : ',(sngl(b(kkk)),kkk=1,l)
                write(23,*) '   Vektor u : ',(sngl(u(kkk)),kkk=1,l)
                nstep = -1
                go to 60
            end if
            if(nred > 0) then
                ired = 0
                do i=1,nr
                    if(list(i) == 1) then
                        ired = ired + 1
                        x(i) = x(i) + u(ired)
                    end if
                end do
            end if
            !y(i_arr) = y(i_arr) + u(i_arr+nred)
            !t(i_arr+nred,1) = t(i_arr+nred,1) + u(i_arr+nred)
            do i=1,n
                y(i) = y(i) + u(i+nred)
                t(i+nred) = t(i+nred) + u(i+nred)
            end do
            if(printG) then
                write(23,*) 'incremented vector t (Sum of improvements):'
                write(23,'(150es11.3)') (t(kkk),kkk=1,l)
                write(23,'(a,15es19.10)') ' improved parameters: x(i) : ',(sngl(x(i)),i=1,nr)
            end if
            ! test for convergence:
            if(.not.MCsim_on) xff = 1.0_rn   ! 4._rn         ! 13.7.2023
            if(MCsim_on) xff = 0.5_rn

            if( (istep > 1 .and. ABS(r-rlst)*xff < EPS1MIN*r+tt) .OR. istep == nstep ) then
                nstep = istep
                if(covmat) then
                    ! compute matrix GB
                    CALL auxdrg(x,y,m,n,nr,nred,list,e,ok,LsqGfn2)
                    if(.not.ok) then
                        write(23,*) 'after call auxdrg: ok=',ok,'   (Compute matrix GB; nstep=-3 )'
                        nstep = -3
                        GO TO 60
                    end if

                    a2_2(1:m,1:n) = e(1:m , nred+1:nred+n)  ! copy submatrix a2 from e, to a2; upper left point in e: (1,nred+1)

                    fy_2 = matmul(a2_2, matmul(cy, Transpose(a2_2)))
                    !call matwrite(fy2,m,m,m,m,23,'(50es11.3)','after mtxupg:         matrix fy:  ')
                    CALL mtxchi(fy_2)              ! inverts matrix fy_2 by Cholesky decomposition
                    if(printG) write(23,*) ' LSQGEN: after 2nd call MTXCHI (invert(fy_2)):  posdef=',posdef

                    if(nred > 0) then
                        a2_3(1:m,1:nred) = e(1:m ,1:nred)    ! submatrix a2_3 from e

                        cx = matmul(Transpose(a2_3), matmul(fy_2, a2_3))
                        !call matwrite(cx,nred,nred,nred,nred,23,'(50es11.3)','after mtxupg:         matrix cx:  ')

                        CALL mtxchi(cx)             ! inverts matrix cx by Cholesky decomposition
                        if(printG) write(23,*) ' LSQGEN: after 3rd call MTXCHI (invert(cx)):  posdef=',posdef
                        !call matwrite(cx,nred,nred,nred,nred,23,'(50es11.3)','after mtxchi:         matrix invert(cx):  ')
                        !if(.not.posdef) then
                        !  ncofact = ncofact + 1
                        !  cofact = cofact * (one - 1.E-10_rn)
                        !  cofactlyt = cofact
                        !  write(23,'(a,L1,a,f15.12,3(a,i3))') 'LSQGEN: (3rd mtxchi)   posDefinite=',posdef,'  cofact=',cofact, &
                        !               '  ncofact=',ncofact,'  kqt=',kqt,' kableitnum=',kableitnum
                        !  goto 60
                        !end if
                        ! array CX now contains covariance matrix of unknowns
                    ELSE
                        cy = cy  - matmul(Transpose(G), matmul(fy_2, matmul(a2_2, cy)))
                        ! array CY now contains covariance matrix of 'improved' measurements
                    end if
                end if
                GO TO 60
            end if
        end do         ! End of istep loop
        nstep = -2
60      continue

        deallocate ( fy, G, e, a2)

        RETURN

    end subroutine LsqGen

!#######################################################################

    real(rn) function LsqGfn2(eta,x,n,nr,k)

    ! A function used by LsqGen, which calculates the fitting function in the
    ! case of WTLS. It is describing in S. Brandt's textbook, chapter ! 9.11, page 308.

        use UR_Linft,      only: mfit, mxind          ! 5.8.2023
        use UR_Derivats,   only: dfda, dfde


        implicit none
        integer, intent(in)          :: n      ! number of measurements
        integer, intent(in)          :: nr     ! number of fit parameters
        real(rn), intent(in)         :: eta(n) ! Vector of measurements
        real(rn), intent(in)         :: x(nr)  ! Vector of fit parameters
        integer, intent(in)          :: k      ! index of measurement (1 bis 30)

        integer           :: j,i
        !-----------------------------------------------------------------------

        ! j = (mfit+1) * k
        j = (mxind+1) * k
        !  eta(j) : y values;  eta(j-i) : x values
        LsqGfn2 = ZERO        ! 2025.01.24 GK

        ! if(k == 1) write(23,*) 'nr=',int(nr,2),' mfit=',int(mfit,2),' j=',int(j,2),' k=',int(k,2)
        dfda = ZERO
        dfde = ZERO

        ! distinguish two cases:
        if(mxind == mfit) then
            ! consider a linear fit with mfit independent variables and mfit fit parameters:
            ! like   y = a1*X1 + a2*X2 + a3*X3      (mxind=3)
            LsqGfn2 = eta(j)
            do i=1,nr
                LsqGfn2 = LsqGfn2 - x(i)*eta(j-i)
                ! derivatives with respect to parameters:
                dfda(i) = -eta(j-i)
                ! derivatives with respect to measured values:
                dfde(i) = -x(i)
            end do
        end if

        if(mxind == 1) then   ! 6.8.2023
            ! consider a linear fit with a polynomial of a single independent variable
            ! and mxind fit parameters:
            ! like   y = a1*X**0 + a2*X**1 + a3*X**2      (mxind=1, mfit=3)
            LsqGfn2 = eta(j)
            do i=1,nr
                LsqGfn2 = LsqGfn2 - x(i)*eta(j-1)**real(i-1,rn)
            end do
            do i=1,nr
                ! derivatives with respect to parameters:
                dfda(i) = -eta(j-1)**real(i-1,rn)
                ! derivatives with respect to measured values:
                if(i == 1) dfde(1) = -x(2)
                if(i == 2) dfde(2) = 1.0_rn
                if(i > 2) dfde(1) = dfde(1) - x(i) * real(i-1,rn) * eta(j-1)**real(i-2,rn)
            end do
        end if

        if(mxind == mfit) dfde(nr+1) = 1.0_rn

        return
    !----------------------------------------------------------------------

    end function LsqGfn2


    !############################################################################################

    subroutine cy_repair(cy, nnew)

        use Num1, only: matwrite, sym_eigensolve

        implicit none

        integer, intent(in)     :: nnew
        real(rn), intent(inout) :: cy(nnew, nnew)

        integer               :: i, ier
        real(rn)              :: dmax
        real(rn),allocatable  :: ccy(:,:),vmat(:,:),d(:),dmat(:,:),cycop(:,:)

        ! Repair cases with diagonal elements of the matrix cy being very close to zero, i.e., when the
        ! matrix would not be positive definite, according to the ISO GUM suppl. 2, page 6, note 4.
        !

        allocate(ccy(nnew,nnew),vmat(nnew,nnew),d(nnew),dmat(nnew,nnew),cycop(nnew,nnew))
        ccy(1:nnew,1:nnew) = cy(1:nnew,1:nnew)
        ! call matwrite(cy,nnew,nnew,nnew,nnew,23,'(120es11.3)','input matrix cy: ')
        !---------------------------------------------------------

        call sym_eigensolve(nnew, cy, nnew, d, ier)

        vmat(1:nnew,1:nnew) = cy(1:nnew,1:nnew)

        dmax = abs(maxval(d,dim=1))
        dmat = ZERO
        do i=1,nnew
            if(abs(d(i)) < 1.E-19_rn*dmax ) then
                if(d(i) < ZERO) d(i) = -1.E-19_rn*dmax
                if(d(i) >= ZERO) d(i) = 1.E-19_rn*dmax
            end if
            dmat(i,i) = d(i)
        end do
        cy = matmul(vmat, matmul(dmat, transpose(vmat)))
        !---------------------------------------------------------

    end subroutine cy_repair

end module WTLS
