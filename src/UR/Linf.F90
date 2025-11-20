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
module LF1

use UR_types

contains

!#######################################################################

    ! Linf(rn0,SDrn0)      : weighted least squares calculation of net count rate rn0
    ! LinfAusf(rn0,sdrn0)  : calls Linf, writes the results to the file
    !                        linfout.txt and calls StoreLinfParms()
    ! StoreLinfParms(rn=,SDrn0) : stores the fit parameters and their
    !                             covariances in the GUI
    ! Linfout              : writes the fit-curve related results into linfout.txt

    !   Copyright (C) 2020-2024  Günter Kanisch

    subroutine LinfAusf(mode,rn0,SDrn0)

        use, intrinsic :: iso_c_binding,      only: c_ptr,c_int,c_null_char,c_long
        use UR_Gleich_globals,          only: ifehl,loadingpro
        use UR_Linft,           only: ma,fpa,fpaSV,sfpa,sfpaSV,kfitp
        use UR_DLIM,            only: iteration_on
        use Rout,               only: pending_events
        use Top,                only: WrStatusbar
        use translation_module, only: T => get_translation

        implicit none

        integer, intent(in)    :: mode    ! 1: without call Linfout; 2: with call Linfout
        real(rn), intent(out)  :: rn0     ! fitted net cout rate for the output quantity number kEGr
        real(rn), intent(out)  :: SDrn0   ! its standard deviation

        !-----------------------------------------------------------------------
        ifehl = 0

        call WrStatusbar(4, T("Calculating") // "....")

        if( .not. loadingPro) call pending_events()

        call Linf(rn0, SDrn0)
        IF(ifehl == 1) RETURN

        IF(.not.iteration_on) THEN
            fpaSV(1:ma) = fpa(1:ma)
            sfpaSV(1:ma) = sfpa(1:ma)
        END IF

        IF(mode == 2) THEN
            call WrStatusbar(4, T("Decay curve fit: view/save/print"))
            call Linfout()
        END IF

        IF(kfitp(1) /= 0) call StoreLinfParms(rn0,SDrn0)

    end subroutine LinfAusf

!#######################################################################

    subroutine StoreLinfParms(rn0, SDrn0)

        !   Copyright (C) 2020-2024  Günter Kanisch

        USE UR_Gleich_globals,     only: klinf,knumEGr,loadingpro,missingval,Messwert,MesswertSV,StdUnc, &
                                 StduncSV, covarval, corrval, covarvalSV
        USE UR_Linft,      only: mfit,ifit,fpa,sfpa,kfitp,covar,covfpa
        use Rout,          only: WTreeViewPutDoubleCell,pending_events
        use UR_params,     only: ZERO,ONE
        use Top,           only: RealModA1
        use Num1,          only: matwrite

        implicit none

        real(rn), intent(in)  :: rn0
        real(rn), intent(in)  :: SDrn0

        integer               :: i,kx

        Messwert(klinf) = rn0
        MesswertSV(klinf) = Messwert(klinf)
        StdUnc(klinf)   = SDrn0
        StdUncSV(klinf) = SDrn0
        call WTreeViewPutDoubleCell('treeview2', 5, klinf, Messwert(klinf))
        call WTreeViewPutDoubleCell('treeview2', 11, klinf, StdUnc(klinf))

        if(allocated(corrval)) deallocate(corrval)
        if(allocated(covarvalSV)) deallocate(covarvalSV)
        allocate(corrval(50),covarvalSV(50))
        corrval = ZERO
        covarvalSV = ZERO
        do i=1,3
            if(ifit(i) == 3) then
                IF(.false. .and. mfit == 2 .and. i == 3) then   ! 25.7.2024
                    fpa(3) = ONE
                    sfpa(3) = ZERO
                else
                    fpa(i) = ZERO
                    sfpa(i) = ZERO
                end if
            end if
            IF(i > 1 .AND. knumEGr == 1) CYCLE
            kx = kfitp(1) -1 + i
            Messwert(kx)   = fpa(i)
            MesswertSV(kx) = fpa(i)
            StdUnc(kx)     = sfpa(i)
            StdUncSV(kx)   = sfpa(i)
            call WTreeViewPutDoubleCell('treeview2', 5, kx, Messwert(kx))
            call WTreeViewPutDoubleCell('treeview2', 11, kx, StdUnc(kx))
        end do
        if(mfit < 2 .or. knumEGr == 1) return

        ! call matwrite(covar,3,3,3,3,66,'(1x,130es13.5)','Linfout: Matrix covar :')

        ! Into treeview3, the correlation values of the fitparameters are written;
        ! however, internally the values of covariances are required

        kx = kfitp(2)
        if(ubound(Corrval,dim=1) < kx) call RealModA1(corrval,kx)
        if(sfpa(1) > ZERO .and. sfpa(2) > ZERO) then
            Covarval(kx) = covar(1,2) / (sfpa(1)*sfpa(2))
            CorrVal(kx) = Covarval(kx)
        else
            Covarval(kx) = -ZERO
        end if
        call WTreeViewPutDoubleCell('treeview3', 6, kx, CovarVal(kx))
        Covarval(kx) = covar(1,2)
        if(kx > ubound(CovarvalSV,dim=1)) call RealModA1(CovarvalSV,kx)
        CovarValSV(kx) = Covarval(kx)

        if(mfit < 3) then
            Covarval(kfitp(2)+1) = missingval
            Covarval(kfitp(2)+2) = missingval
        end if
        kx = kfitp(2) + 1
        if(ubound(Corrval,dim=1) < kx) call RealModA1(corrval,kx)
        if(sfpa(2) > ZERO .and. sfpa(3) > ZERO) then
            Covarval(kx) = covar(2,3) / (sfpa(2)*sfpa(3))
            CorrVal(kx) = Covarval(kx)
        else
            Covarval(kx) = ZERO
        end if
        call WTreeViewPutDoubleCell('treeview3', 6, kx, CovarVal(kx))
        Covarval(kx) = covar(2,3)
        CovarValSV(kx) = Covarval(kx)

        kx = kfitp(2) + 2
        if(ubound(Corrval,dim=1) < kx) call RealModA1(corrval,kx)
        if(sfpa(1) > ZERO .and. sfpa(3) > ZERO) then
            Covarval(kx) = covar(1,3) / (sfpa(1)*sfpa(3))
            CorrVal(kx) = Covarval(kx)
        else
            Covarval(kx) = ZERO
        end if
        call WTreeViewPutDoubleCell('treeview3', 6, kx, Covarval(kx))   ! format frmt
        Covarval(kx) = covar(1,3)
        CovarValSV(kx) = Covarval(kx)

        covFPA = covar         ! copy of covar

        if(.not.loadingPro) call pending_events()

    end subroutine StoreLinfParms

    !#######################################################################

    subroutine Linf(rn0, SDrn0)

        !   Copyright (C) 2020-2024  Günter Kanisch

        use UR_Gleich_globals, only: messwert,messwertsv,symbole,ngrs,ncov,kableitnum,ifehl,klinf,missingval, &
                                 kpoint,upropa_on,stdunc,isymba,isymbb,covarval,kegr
        use ur_linft,      only: dnetrate,sdnetrate,covar,chisq,chisqr_nls,chisqr_wtls,chisqr, &
                                 mpfxfixed,mpfx,fixedrate,sdfixedrate,parfixed,a,numd,mfit,ifit,chis_test,export_case, &
                                 export_r,fpa,sfpa,sfpasv,ma,chisqrzz_wtls,cofact,cofactlyt,fitmeth, &
                                 kpearson,kpmle,kuse_fixed,nhp,nwei,posdef,wtls_wild,kfitp,dmesszeit, &
                                 numd,k_rbl,use_wtls,tmedian,fpasv,d0zrate,sd0zrate,dtdiff,fixedratemc,yfit,x,dnetfit, &
                                 covylf,xpl,ypl,uypl,yplfit,sdnetfit,mac,mfrbg,dgrossrate

        use fparser,       only: evalf, evalerrmsg
        use ur_perror
        use ur_dlim,       only: iteration_on,limit_typ
        use ur_mcc,        only: kqtypx,imc
        use ur_general_globals,  only: mcsim_on,fname,batest_on,batf,bat_serial, results_path
        use Top,           only: WrStatusbar,dpafact
        use Num1,          only: dpi_funcs,funcs,matwrite,find_mac
        use UR_params,     only: ZERO,ONE,TWO,EPS1MIN
        use LLcov2,        only: LinCov2
        use WTLS,          only: GlsqUR2
        use Rw1,           only: Find_lambda
        use file_io,       only: logger
        use UR_MCSR,       only: mw_rbl,umw_rbl

        implicit none

        real(rn), intent(out)  :: rn0     ! fitted net cout rate for the output quantity number kegr
        real(rn), intent(out)  :: sdrn0   ! its standard deviation

        integer             :: i,irun,irunmx, k,klu,kx,kqt,ia(3)
        integer             :: kfall,j,jj1,jj2,ios

        logical            :: ok
        real(rn)           :: sd(numd), afunc(ma)
        real(rn)           :: tm,zfact,chisqrzz,yval,mfact
        real(rn)           :: fv1,dpi,mwkabl,mwklu,rback,urback,parm
        real(rn)           :: netratesub(numd)
        character(len=150) :: str1,text
        real(rn)           :: aG(ma),covarG(ma,ma), a_wls(ma)
        character(len=512) :: log_str
        logical            :: setzero

!-----------------------------------------------------------------------

        setzero = .false.
        kqt = 1
        if(iteration_on .and. limit_typ == 1) kqt = 2
        if(iteration_on .and. limit_typ == 2) kqt = 3

        ! mac = 0      ! 17.9.2024
        mwklu = ZERO
        mwkabl = ZERO
        klu = klinf
        IF(kfitp(1) > 0) klu = kfitp(1)-1+kEGr

        ! array ifit:  ifit(k):  =1: parameter k is fitted;   =2: hold paramter k fiexed at its start value;
        !                        =3: parameter k is not included in fitting;
        ! arrays a, covar     :  fitting parameters and covar matrix
        ! arrays fpa, sfpa    :  fitting parameters and associated uncertainties

        ifehl = 0
        mfit = 0
        do i=1,3
            IF(ifit(i) == 1) mfit = mfit + 1
            if(ifit(i) == 3 .and. .not.(kPMLE == 1 .and. i == mfrbg)) then
                fpa(i) = ZERO
                sfpa(i) = ZERO
                fpaSV(i) = ZERO
                sfpaSV(i) = ZERO
            end if
        end do

        if(mac == 0) call find_mac(mac)          ! 7.7.2024

        zfact = ONE
        mw_rbl = ZERO                   ! value of (net) blank count rate
        umw_rbl = ZERO                  ! its standard deviation
        if(k_rbl > 0) then
            mw_rbl = Messwert(kpoint(k_rbl))
            umw_rbl = StdUnc(kpoint(k_rbl))
        end if
        if(allocated(x)) deallocate(x)
        if(allocated(fixedrate)) deallocate(fixedrate)
        if(allocated(SDfixedrate)) deallocate(SDfixedrate)
        if(allocated(yfit)) deallocate(yfit)
        if(allocated(dnetfit)) deallocate(dnetfit)
        if(allocated(SDnetfit)) deallocate(sDnetfit)
        if(allocated(dgrossrate)) deallocate(dgrossrate)

        allocate(x(numd),fixedrate(numd),SDfixedrate(numd),yfit(numd),dnetfit(numd),sdnetfit(numd))
        allocate(dgrossrate(numd))

        if(.true.) then
            ! calculate the net count rates of the decay curve and their standard uncertainties:
            dgrossrate(1:numd) = Messwert(ngrs+ncov+1:ngrs+ncov+numd)             ! 22.6.2024
            dnetrate(1:numd) = Messwert(ngrs+ncov+1:ngrs+ncov+numd) - d0zrate(1:numd) - mw_rbl
            SDnetrate(1:numd) = [ (max(ZERO, Messwert(ngrs+ncov+i)/dmesszeit(i)),i=1,numd) ]
            SDnetrate(1:numd) = SDnetrate(1:numd) + sd0zrate(1:numd)**TWO + umw_rbl**TWO
            SDnetrate(1:numd) = sqrt(SDnetrate(1:numd))
            do i=1,numd
                if(dnetrate(i) <= ZERO) SDnetrate(i) = SDnetrate(i) * (ONE + 1.E-7_rn)
            end do
            sd(1:numd) = SDnetrate(1:numd)
            x(1:numd) = dtdiff(1:numd)
        end if

        if(Messwert(ngrs+ncov+1) > 1.E+20_rn) then
            do i=1,ngrs+ncov+numd
                write(log_str, '(*(g0))') 'i=',i,' ',symbole(i)%s,' Messwert=',sngl(Messwert(i)),'  MEsswetSV=',sngl(MesswertSV(i))
                call logger(66, log_str)
            end do
        end if

        !----
        !   Only fixedrate() is used, but SDfixedrate() not!
        !   The latter is used in Lincov2 within the uncertainty propagation
        !   based on the matrix Qmat.
        !   nhp: number of parameters given as arguments of UR Linfit() function; "mpfx parameters"
        !   kableitnum: if > 0: designates the Messwert-element with respect to which a partial
        !               derivative of the output quantity is being calculated

        fixedrate(1:numd) = ZERO
        SDfixedrate(1:numd) = ZERO
        mpfxfixed(1:nhp) = 0

        if(parfixed) then
            do k=1,3
                if(ifit(k) == 2) then
                    do i=1,numd
                        call funcs(i,afunc)
                        fixedrate(i) = fixedrate(i) + afunc(k)
                        !###### Note: afunc(k) needs not to be multiplied by a(k), because a(k)
                        ! is already contained in afunc(k), based on the definition in the decay curve model dialog!

                        if(kuse_fixed == 1) cycle      ! kuse_fixed is an internal test variable (Rechw1),
                        ! which correctly should be = 2
                        fv1 = afunc(k)
                        do j=1,nhp
                            if(kableitnum > 0) mwkabl = Messwert(kableitnum)
                            if(kableitnum == klinf) mwklu = MEsswert(klu)
                            if(abs(StdUnc(mpfx(j))-missingval) < EPS1MIN .or. abs(StdUnc(mpfx(j))) < EPS1MIN) cycle
                            ! dpi: partial derivative of the output quantity with respect to Messwert(mpfx(j))
                            dpi = dpi_funcs(mpfx(j),i,k,ma,fv1)
                            if(kableitnum > 0) Messwert(kableitnum) = mwkabl     ! restore
                            if(kableitnum == klinf) Messwert(klu) = mwklu        !
                            SDfixedrate(i) = SDfixedrate(i) + ( dpi*StdUnc(mpfx(j)) )**TWO
                            !  write(66,*) ' linf: k,i,j=',k,i,j,' dpi=',real(dpi,8)


                            ! if dpi is /= 0, the uncertainty of Messwert(mpfx(j)) is already
                            ! contained in fixedrate(i), then set mpfxfixed(j) = 1

                            if(abs(dpi) > EPS1MIN .and. abs(StdUnc(mpfx(j))) > EPS1MIN .and. &
                                abs(StdUnc(mpfx(j))-missingval) > EPS1MIN ) mpfxfixed(j) = 1
                        end do
                    end do
                end if
            end do
            do i=1,numd
                if(SDfixedrate(i) > ZERO) SDfixedrate(i) = sqrt(SDfixedrate(i))
            end do
        end if
        !----

        if(.not.parfixed) then
            netratesub(1:numd) = dnetrate(1:numd)
        else
            if(.not.MCsim_on) then
                netratesub(1:numd) = dnetrate(1:numd) - fixedrate(1:numd)
            else
                netratesub(1:numd) = dnetrate(1:numd) - fixedrateMC(1:numd)
            end if
            ! As contributions of mpfxfixed=1 parameters are involved here,
            ! their contributions are not considered by Linscov2 within the QMAT loop !
            sd(1:numd) = sqrt( sd(1:numd)**TWO + SDfixedrate(1:numd)**TWO )
        end if

        !----
        if(nwei == 0) sd = ONE   ! disregard the statistical weighting!

        a(1:ma) = ZERO
        irunmx = 1
        if(kpearson == 1) irunmx = 3
        if(use_wtls) irunmx = 1
        if(kpmle == 1) irunmx = 1

        do irun=1,irunmx

            IF(irun > 1) then
                ! With having kPMLE=1 or Neyman WLS, one should not arrive here.
                ! Note that PLSQ is done here within the irun loop!!!
                do i=1,numd
                    call funcs(i,afunc)
                    if(ifehl == 1) return
                    yfit(i) = ZERO
                    do k=1,ma
                        parm = a(k)
                        if(ifit(k) == 2) parm = ONE
                        yfit(i) = yfit(i) + parm*afunc(k)
                    end do
                    yfit(i) = yfit(i) - fixedrate(i)
                    rback = d0zrate(i) + mw_rbl
                    urback = sqrt(sd0zrate(i)**TWO + umw_rbl**TWO)
                    tm = dmesszeit(i)
                    if(.not.parfixed) then
                        sd(i) = SQRT( (MAX(ZERO,yfit(i)) + rback )/tm  + urback**TWO )
                        if(yfit(i) <= ZERO) then
                            sd(i) = sd(i)*(ONE + 1.E-7_rn)
                        end if
                    else
                        sd(i) = SQRT( (MAX(ZERO,yfit(i)) + mw_rbl + d0zrate(i) + fixedrate(i) )/tm  + umw_rbl**TWO +  &
                            sd0zrate(i)**TWO + sdfixedrate(i)**TWO)
                        if(abs(sd(i) - sqrt(umw_rbl**TWO + sd0zrate(i)**TWO + sdfixedrate(i)**TWO)) < 1.E-3_rn) sd(i) = sd(i)*1.002_rn
                    end if
                end do
            end if

            ! Linear fit with including covariances between input values:
            if(allocated(covar)) deallocate(covar)
            allocate(covar(ma,ma))                                       !
            ia = 1
            call LinCov2(numd,mfit,netratesub,sd,a,covar,ma,Chisq,ok,ifehl)
            if(ifehl == 1) then
                do i=1,numd
                    !                     write(66,*) 'i=',i,'  netratesub=',sngl(netratesub(i)),  &
                    !                         '  sd=',sngl(sd(i)),' dnetrate=',sngl(dnetrate(i)), &
                    !                         '  Messwert(ngrs+ncov+i)=',sngl(Messwert(ngrs+ncov+i))
                    write(log_str, '(*(g0))') 'i=',i,'  netratesub=',sngl(netratesub(i)),  &
                        '  sd=',sngl(sd(i)),' dnetrate=',sngl(dnetrate(i)), &
                        '  Messwert(ngrs+ncov+i)=',sngl(Messwert(ngrs+ncov+i))
                    call logger(66, log_str)
                end do
                return
            end if
            if(.false. .and. kqt < 2 .and. .not.upropa_on) then
                write(log_str, '(*(g0))') ' Linf: Params: ',(a(i),i=1,ma)
                call logger(66, log_str)
            end if

            chisqr = chisq
            IF(numd > mfit) chisqr = chisq/real(MAX(numd-mfit,1),rn)
            Chisqr_NLS = Chisqr
            Chis_test(1) = ZERO
            Chis_test(2) = ZERO
            if(numd > mfit) Chis_test(1) = abs( chisqr_NLS*real(numd-mfit,rn) - real(numd-mfit,rn) ) /sqrt(TWO*real(numd-mfit,rn))

        end do


        !-----------------------
        !   Export to R: produce data frame files for using them in R
        kfall = 0
        IF(export_r .and. .not.batest_on .and. .not.batf .and. .not.bat_serial .and. kpmle /= 1) THEN
            kfall = 0
            IF(.not.export_case(1)) kfall = 1
            IF(export_case(1) .AND. .not.export_case(2)) kfall = 2
            IF(export_case(2) .AND. .not.export_case(3)) kfall = 3
            IF(kfall == 1) THEN
                open(77,file=trim(results_path)//'URExport-to-R.txt',STATUS='unknown')

                call logger(77, '############################################################')

                call logger(77, ' ')

                write(log_str, '(a,a)') 'File = ',TRIM(fname),'    Fit method=',TRIM(fitmeth)
                call logger(77, log_str)

                call logger(77, ' ')

                call logger(77, 'Case: output quantity')
                export_case(2) = .FALSE.
                export_case(3) = .FALSE.

                OPEN(78,FILE=trim(results_path)//'covmat1.txt',STATUS='unknown')
                OPEN(79,FILE=trim(results_path)//'data1.txt',STATUS='unknown')
            end if

            !! if(.not.iteration_on)  write(66,*) 'Linf: export_r=',export_r,' exprt_case=',export_case, &
            !!             ' kfall=',int(kfall,2)

            ! Note: the case of the detection limit is omitted here:
            IF(kfall > 0) then
                IF(.not.export_case(kfall) .AND. kableitnum == 0     &
                    .and. (kfall == 1 .or. (kfall == 2 .and. iteration_on .and. limit_typ == 1 .AND. ABS(a(1)) < 1.E-8_rn ) ) ) THEN
                    IF(kfall == 2 .AND. iteration_on .AND. limit_typ == 1 ) THEN
                        close (78)
                        close (79)
                        OPEN(78,FILE=trim(results_path)//'covmat2.txt',STATUS='unknown')
                        OPEN(79,FILE=trim(results_path)//'data2.txt',STATUS='unknown')
                        call logger(77, ' ')
                        call logger(77, 'Case: Decision threshold')
                    end if

                    call logger(77, ' ')
                    write(log_str, '(*(g0))') 'Blank count rate=',sngl(mw_rbl),'  background rate=',sngl(d0zrate(1))      !,'  kableitnum=',kableitnum
                    call logger(77, log_str)

                    write(log_str, '(*(g0))') 'Input data: variance-covariance matrix:   (rank=',numd,')'
                    call logger(77, log_str)

                    call logger(77, ' ')
                    do i=1,numd
                        if(kPMLE /= 1)  then
                            write(log_str, '(40es13.5)') (covyLF(i,k),k=1,numd)
                            call logger(77, log_str)
                        end if

                        IF(kPMLE /= 1)  then
                            write(log_str, '(40es13.5)') (covyLF(i,k),k=1,numd)
                            call logger(78, log_str)
                        end if

                        IF(kPMLE == 1) yval = ( dnetrate(i) + mw_rbl + d0zrate(i) ) * dmesszeit(i)
                        IF(kPMLE == 1)  then
                            write(log_str, '(40es13.5)') (ZERO,k=1,i-1), yval, (ZERO,k=i+1,numd)
                            call logger(77, log_str)
                        end if
                        IF(kPMLE == 1)  then
                            write(log_str, '(40es13.5)') (ZERO,k=1,i-1), yval, (ZERO,k=i+1,numd)
                            call logger(78, log_str)
                        end if
                    end do
                    call logger(77, ' ')

                    write(log_str, '(*(g0))') 'Arrays y, X1, x2, X3: '
                    call logger(77, log_str)
                    call logger(77, ' ')
                    text = '     y            ' // 'X1           '
                    IF(ifit(2) == 1 .AND. ifit(3) > 1) text = '     y            ' // 'X1           ' // 'X2           '
                    IF(ifit(2) == 1 .AND. ifit(3) == 1) text = '     y            ' // 'X1           ' // 'X2           ' // 'X3           '
                    IF(ifit(2) > 1 .AND. ifit(3) == 1) text = '     y            ' // 'X1           ' // 'X3           '
                    IF(kPMLE == 1 .and. ifit(2) > 1 .AND. ifit(3) > 1) text = '     y            ' // 'X1           ' // 'X2           '

                    call logger(77, TRIM(text))
                    call logger(79, TRIM(text))
                    do i=1,numd
                        call funcs(i,afunc)
                        yval = dnetrate(i)
                        yval = yval - fixedrate(i)
                        IF(kPMLE == 1) then
                            yval = ( dnetrate(i) + fixedrate(i) + mw_rbl + d0zrate(i) ) * tmedian
                        end if

                        WRITE(text,'(i3,1x,7es13.5)') i,yval,(afunc(j),j=1,ma)
                        IF(ifit(2) > 1) THEN
                            IF(kPMLE /= 1) THEN
                                text = text(1:30) // text(44:56)
                            else
                                text = text(1:43)
                            end if
                        end if
                        IF(ifit(3) > 1) text = text(1:43)
                        call logger(77, TRIM(text))
                        call logger(79, TRIM(text))
                    end do
                    close (78)
                    close (79)

                    call logger(77, ' ')
                    call logger(77, 'Parameter values and std uncertainties obtained by UR: ')
                    do i=1,ma
                        ! IF(ifit(i) == 0) cycle
                        IF(ifit(i) > 1) cycle
                        mfact = ONE
                        IF(kPMLE == 1) mfact = tmedian
                        write(log_str, '(i2,1x,7es13.5)') i, a(i)*mfact, SQRT(covar(i,i))*mfact
                        call logger(77, log_str)
                    end do
                    write(log_str, '(*(g0))') ' Chisqr=',sngl(chisq)/REAL(max(1,numd-mfit),rn)
                    call logger(77, log_str)
                    call logger(77, ' ')

                    IF(kfall == 1) export_case(1) = .TRUE.
                    IF(kfall == 2) THEN
                        export_case(3) = .TRUE.
                    end if

                end if
            end if
        end if

        !-----------------------

        IF(kpearson == 1) THEN
            sd(1:numd) = SDnetrate(1:numd)
            if(parfixed) sd(1:numd) = sqrt( SDnetRate(1:numd)**TWO + SDfixedrate(1:numd)**TWO )
        end if

        WTLS_wild = .false.
        ! xxxxxxxxxxxxxxxxxxxxxxxx   invoking weighted total least squares
        IF(use_WTLS .and. kPMLE == 0) THEN

            aG(1:ma) = a(1:ma)
            a_wls(1:ma) = a(1:ma)
            do j=1,1
                aG(1:ma) = a_wls(1:ma)
                call GlsqUR2(aG,covarG,ifehl)
                ! if(kqt >= 2 .or. kableitnum > 0) exit
                if(kqt >= 2) exit
                !write(66,*) 'j=',int(j,2),' WTLS: aG :',real(aG(1:ma),8)
                !call matwrite(covarG,ma,ma,66,'(10(es17.10,2x))','Matrix aG:')
            end do

            IF(ifehl == 1) RETURN
            if(.not. posdef) then
                WTLS_wild = .true.
                ifehl = 1
            end if

            a(1:ma) = aG(1:ma)
            covar(1:ma,1:ma) = covarG(1:ma,1:ma)

        end if
        chisqr = chisq
        IF(numd > mfit) chisqr = chisq/real(MAX(numd-mfit,1),rn)

        if(WTLS_Wild) then
            call logger(23, ' ')
            write(log_str, '(*(g0))') 'Linf after call GLSQUR2: Fitted net count rates:    Chisqr=',sngl(Chisqr),  &
                '  kqtypx=',kqtypx ,'  posdef=',posdef
            call logger(23, log_str)
            do i=1,numd
                call funcs(i,afunc)
                yfit(i) = dot_product(aG(1:ma),afunc(1:ma))
                yfit(i) = yfit(i) - fixedrate(i)
                write(log_str, '(a,i2,10(a,f10.5,1x))') 'i=',i,' netratesub=',netratesub(i),' sd=',sd(i),' yfit=',  &
                    yfit(i),' utest=',(yfit(i)-netratesub(i))/sd(i), &
                    ' fixedrate=',fixedrate(i)
                call logger(23, log_str)
            end do
            call logger(23, ' ')
        end if

        ! Alternatively calculated Chisqr:
        Chisqrzz = ZERO
        do i=1,numd
            call Funcs(i,afunc)
            dnetfit(i) = ZERO
            do k=1,ma
                parm = a(k)
                if(ifit(k) == 2) parm = ONE
                if(ifit(k) == 2 .and. kPMLE == 1) parm = ZERO
                dnetfit(i) = dnetfit(i) + parm * afunc(k)
            end do
            if(SDnetrate(i) > ZERO) Chisqrzz = Chisqrzz + (dnetrate(i)-dnetfit(i))**TWO / SDnetrate(i)**TWO
        end do
        if(kPMLE /= 1) Chisq = chisqrzz
        Chisqrzz_WTLS = chisqrzz/real(max(1,numd-mfit),rn)    ! Chisqr from WTLS, calculated similarly as with WLS

        ! Arrays for CurvePlot:
        if(kqt == 1 .and. .not.iteration_on .and. kableitnum == 0  .and.    &
            .not.MCsim_on ) then
            if(allocated(xpl)) deallocate(xpl,ypl,uypl,yplfit)
            allocate(xpl(numd),ypl(numd),uypl(numd),yplfit(numd))
            xpl(1:numd) = x(1:numd) / 3600._rn      ! unit : hour
            ypl(1:numd) = netratesub(1:numd)
            uypl(1:numd) = sd(1:numd)
            yplfit(1:numd) = dnetfit(1:numd)

        end if

        chisqr = chisq
        IF(numd > mfit) chisqr = chisq/real(MAX(numd-mfit,1),rn)
        if(use_WTLS) chisqr = chisqr_WTLS

        do i=1,ma
            fpa(i) = a(i)
            if(covar(i,i) > ZERO) sfpa(i) = SQRT(covar(i,i))    ! 27.6.2024, 15:45
            IF(.not.iteration_on) THEN
                fpaSV(i) = a(i)
                sfpaSV(i) = ZERO
                if(covar(i,i) > ZERO) sfpaSV(i) = SQRT(covar(i,i))
            end if
        end do

!  move these four lines to the end of lincov2!!!   27.2.2024
!sfpa(1:ma) = zero
!do i=1,ma
!  if(covar(i,i) > zero) sfpa(i) = SQRT(covar(i,i))
!end do

        rn0 = a(kEGr)
        SDrn0 = sfpa(kEGr)


        IF(iteration_on) THEN

            !############
            StdUnc(klu) = SDrn0
            !############

            ! Up to now, it was not considered, that the covariance values between fit parameters
            ! can vary through the iteration for DT or DL.

            IF(mfit > 1 .AND. kfitp(2) > 0 ) THEN
                kx = kfitp(2) - 1   ! row number in the covar-table treevie3
                do k=1,3
                    if(Symbole(IsymbA(k))%s(1:4) /= 'Fitp') cycle    ! <---   added 17.11.2025 GK
                    read(Symbole(IsymbA(k))%s(5:5),*,iostat=ios) jj1          ! index of the "left-hand" Fitp parameter
                    read(Symbole(IsymbB(k))%s(5:5),*,iostat=ios) jj2          ! index of the "right-hand" Fitp parameter
                    if(ios /= 0) then
                        call logger(66, 'Linf:    Covar: Error!  Reading the FITP index does not work; symbols mixed up??')
                        write(log_str, '(*(g0))') '      Symbole : ',Symbole(IsymbA(k))%s,'  ',Symbole(IsymbB(k))%s
                        call logger(66, log_str)
                    end if
                    Covarval(kx+k) = covar(jj1,jj2)
                end do
                ! call matwrite(covar,3,3,3,3,66,'(1x,130es13.5)','Linf: Matrix covar :')
            end if
        end if

        if(.not.MCSim_on .and. .not.iteration_on .and. kableitnum == 0 .and. cofact > ZERO .and. use_WTLS) then
            write(str1,'(a,es8.1)') 'cofact=1-',ONE-cofactlyt
            call WrStatusBar(2, trim(str1))
            ! write(66,*) trim(str1)
        end if

        if(.false. .and. use_WTLS .and. imc < 50 )  then
            write(log_str, '(*(g0))') 'Linf am Ende:   fpa =',(sngl(fpa(i)),i=1,3),  &
            ' sfpa=',(sngl(sfpa(i)),i=1,3)
            call logger(23, log_str)
        end if

    end subroutine Linf

!#######################################################################

    subroutine Linfout()

        !   Copyright (C) 2020-2024  Günter Kanisch

        USE UR_Gleich_globals, only: kpoint, Messwert
        USE UR_Linft,          only: ma,chisq,ndatmax,fitmeth,kPMLE,mfit,ifit,mfRBG_fit_PMLE, &
                                     nkovzr,numd,dnetfit,SDnetfit,fpa,covar,mfrbg, &
                                     dbzrate,sfpaSV,dnetrate,SDnetrate,dtdiff,sdbzrate, &
                                     sfpa,k_rbl,d0zrate

        use Brandt,          only: gincbt
        use Num1,            only: funcs
        use UR_params,       only: ZERO,EPS1MIN,ONE,TWO
        use chf,             only: flfu

        use translation_module, only: T => get_translation
        use file_io,            only: logger
        use ur_general_globals, only: fname, batf, batest_user, bat_serial

        implicit none

        integer           :: i,k,nterms,k1,k2,ios,ii1,kk
        real(rn)          :: xd(ma,ndatmax)
        real(rn)          :: afunc(ma),rpa(ma),rfi
        real(rn),allocatable  :: drelf(:),utest(:),dfit(:),SDdfit(:)

        real(rn)          :: tval(ma),pval(ma),df,parm,minval_net,scalef
        real(rn)          :: dyda,dyda1,dyda2,u,zfact, chisqrr, dummy,chisqr3
        character(len=90) :: headline
        character(len=11) :: cdnetzf,cdfitzf,znform
        character(len=12) :: ctfpa(3)
        character(len=9)  :: ccr
        character(len=8)  :: cxd(3)
        character(len=512) :: log_str
        logical           :: gross
        !-----------------------------------------------------------------------
        gross = .FALSE.
        IF(kPMLE == 1) THEN
            ! IF(ifit(mfrbg) <= 2) gross = .TRUE.
            IF(ifit(mfrbg) >= 2) gross = .TRUE.
        end if

        allocate(drelf(numd),utest(numd),dfit(numd),SDdfit(numd))

        chisqr3 = ZERO
        minval_net = 1.E+30_rn

        do i=1,numd
            call Funcs(i,afunc)

            dnetfit(i) = ZERO
            do k=1,ma
                xd(k,i) = afunc(k)
                if(ifit(k) == 1) then
                    dnetfit(i) = dnetfit(i) + fpa(k) * afunc(k)
                elseif(ifit(k) == 2) then               !
                    if(k == mfrbg .and. mfRBG_fit_PMLE .and. kPMLE == 1) then   !  <-- 25.6.2024
                        dnetfit(i) = dnetfit(i) + fpa(k) * afunc(k)
                    else
                        dnetfit(i) = dnetfit(i) + ONE * afunc(k)
                    end if
                end if
            end do
            ! write(22,*) ' xd(1:3,i)=',sngl(xd(1:3,i))
            dfit(i) = dnetfit(i)
            if(gross) then    ! 27.6.2024
                dfit(i) = dfit(i) + d0zrate(i)
                if(k_rbl > 0) dfit(i) = dfit(i) + Messwert(kpoint(k_rbl))
            endif

            if(dnetrate(i) > EPS1MIN .and. dnetrate(i) < minval_net) minval_net = dnetrate(i)

            IF(.not.gross) drelf(i) = (dnetrate(i)-dnetfit(i))/dnetfit(i)*100._rn
            IF(gross) drelf(i) = (dbzrate(i)-dfit(i))/dfit(i)*100._rn

            !  calculate the uncertainties u of the fitted values of the input values:
            u = ZERO
            do k1=1,ma
                dyda = afunc(k1)
                u = u + dyda**TWO * covar(k1,k1)
            end do
            do k1=1,ma-1
                do k2=k1+1,ma
                    dyda1 = afunc(k1)
                    dyda2 = afunc(k2)
                    u = u + TWO * dyda1*dyda2*covar(k1,k2)
                end do
            end do
            u = SQRT(ABS(u))
            SDnetfit(i) = u
            SDdfit(i) = u

            chisqr3 = chisqr3 + (dnetfit(i)-dnetrate(i))**TWO/sdnetrate(i)**TWO
        end do

        scalef = 1.0_rn
        if(minval_net < 1.e-6_rn) then
            do i=1,18
                scalef = 10._rn**real(i,rn)
                if(minval_net * scalef > 1.e-3_rn) then
                    exit
                end if
            end do
        end if

        IF(numd > mfit) chisqr3 = chisqr3/real(numd-mfit,rn)

        nterms = mfit
        do i=1,ma
            tval(i) = ZERO
            pval(i) = ZERO
            rpa(i)  = ZERO
            if(abs(fpa(i)) > ZERO) then
                ! in the following 3 statements a simple t test is performed
                tval(i) = abs(fpa(i)/sfpaSV(i))
                df = max (1,numd-nterms)
                pval(i) = gincbt(0.5_rn*df,0.5_rn,df/(df+tval(i)**TWO))
                ! rpa(i) = 100._rn*sfpaSV(i)/fpa(i)
                rpa(i) = 100._rn*sfpa(i)/fpa(i)
            end if
        end do

		! Add the individual filename at the begin for each project:
        if (batf .or. batest_user .or. bat_serial) then
            call logger(22, "Project:  " // trim(fname))
        else
            call logger(22, "Project:  " // trim(fname), new=.true.)
        end if

        if(nkovzr == 0 .or. gross) then
            headline = T("Result of decay curve analysis (without covariances):")
        else if(nkovzr == 1 .and. .not.gross) then
            headline = T("Result of decay curve analysis (with covariances):")
        end if
        headline = trim(headline) // '      ' // T("Method: ") // TRIM(fitmeth)
        ! call logger(22, trim(headline), new=.true.)
        call logger(22, trim(headline))
        write(log_str,'(10x,A)') 'LinFit(t) = a1*X1(t) + a2*X2(t) + a3*X3(t)'
        call logger(22, log_str)

        if(scalef > 10._rn) then
            write(log_str,'(a,es7.1)') 'count rate scaled with factor ',scalef
            call logger(22, log_str)
        end if

        ccr = '  NetRate'
        if(gross) ccr = 'GrossRate'
        write(log_str,'(*(A))')  &
                    '  i      t     X1(t)    X2(t)    X3(t)   ',ccr,'    rUnc.',  &
                    '      LinFit    relDev  uTest',  new_line('A'), &
                    '        (m)                                (cps)       (%)  ',  &
                    '      (cps)       (%)'

        call logger(22, log_str)
        call logger(22, repeat("-", 89))
        zfact = ONE
        do i=1,numd
            rfi = ZERO
            cxd = ' '
            ! re-formatting the numbers, if too big for (f11.6):

            do kk=1,3
                ios = 10
                ii1 = 5
                znform ='(f8.5)'
                do
                    write(cxd(kk),znform) xd(kk,i)
                    read(cxd(kk),*,iostat=ios) dummy
                    if(ios == 0) exit
                    if(ios /= 0 .and. ii1 > 0) then
                        ii1 = ii1 - 1
                        if(ii1 > 0) then
                            write(znform,'(a,i1,a)') '(f8.',ii1,')'
                            write(cxd(kk),znform) xd(kk,i)
                        end if
                    end if
                end do
            end do

            ii1 = 7
            znform ='(f11.7)'
            if(.not.gross) write(cdnetzf,znform) dnetrate(i)*zfact*scalef
            if(gross)      write(cdnetzf,znform) dbzrate(i)*zfact*scalef

            ios = 1000
            do while( ios /= 0)
                read(cdnetzf,*,iostat=ios) dummy
                if(ios /= 0 .and. ii1 > 0) then
                    ii1 = ii1 - 1
                    if(ii1 > 0) then
                        write(znform,'(a,i1,a)') '(f11.',ii1,')'
                        if(.not.gross) write(cdnetzf,znform) dnetrate(i)*zfact*scalef
                        if(gross)      write(cdnetzf,znform) dbzrate(i)*zfact*scalef
                    end if
                end if
            end do
            ii1 = 7
            znform = '(f11.7)'
            if(.not.gross) write(cdfitzf,znform) dnetfit(i)*zfact*scalef
            if(gross)      write(cdfitzf,znform) dfit(i)*zfact*scalef

            ios = 1000
            do while( ios /= 0)
                read(cdfitzf,*,iostat=ios) dummy
                if(ios /= 0) then
                    ii1 = ii1 - 1
                    if(ii1 > 0) then
                        write(znform,'(a,i1,a)') '(f11.',ii1,')'
                        if(.not.gross) write(cdfitzf,znform) dnetfit(i)*zfact*scalef
                        if(gross)      write(cdfitzf,znform) dfit(i)*zfact*scalef
                    end if
                end if
            end do
            select case (gross)
              case (.false.)
                IF(abs(dnetrate(i)) > EPS1MIN) rfi = abs(100._rn*SDnetrate(i)/(dnetrate(i)*scalef))
                utest(i) = (dnetrate(i) - dnetfit(i)) / SQRT(SDnetrate(i)**TWO + SDnetfit(i)**TWO)
                write(log_str,153) i,dtdiff(i)/(60._rn),cxd(1),cxd(2),cxd(3),  &
                    cdnetzf,rfi,cdfitzf,drelf(i),utest(i)   ! , rfi
                call logger(22, log_str)
              case (.true.)
                rfi = 100._rn*SDbzrate(i)/dbzrate(i)
                utest(i) = (dbzrate(i) - dfit(i)) / SQRT(SDbzrate(i)**TWO + SDdfit(i)**TWO)
                write(log_str,153) i,dtdiff(i)/(60._rn),cxd(1),cxd(2),cxd(3),  &
                    cdnetzf,rfi,cdfitzf,drelf(i),utest(i)
                call logger(22, log_str)
            end select
153         format(i3,f9.2,3(1x,a8),1x,a11,1x,f7.2,1x,  &
                1x,'| ',a11,2x,f5.1,3x,f4.1,     6x,es12.5)
        end do

        write(log_str,'(88("-"))')
        call logger(22, log_str)

        chisqrr = chisq
        IF(numd > mfit) chisqrr = chisqrr/real(numd-mfit,rn)

        do i=1,3
            znform = '(f12.7)'
            ii1 = 7
            parm = fpa(i)
            if(ifit(i) == 2) parm = ONE
            if(i == mfrbg .and. mfRBG_fit_PMLE .and. mfrbg > 0) parm = fpa(i)    ! 25.6.2024
            write(ctfpa(i),znform) parm*zfact*scalef

            ios = 1000
            do while( ios /= 0)
                read(ctfpa(i),*,iostat=ios) dummy
                if(ios /= 0) then
                    ii1 = ii1 - 1
                    if(ii1 > 0 ) then
                        write(znform,'(a,i1,a)') '(f11.',ii1,')'
                        write(ctfpa(i),znform) parm*zfact*scalef
                    end if
                end if
            end do
        end do

        if (numd >= nterms) then
            write(log_str, '(A, A12, 2X, A, A12, 2X, A, A12, A, A,&
                        & 8X, A, F9.3, 3X, A, F9.3, 3X, A, F9.3, 2X, A, A,&
                        & 7X, 19X, 36X, A, ES11.3, A,&
                        & 7X, A, F8.6, 4X, A, F8.6, 4X, A, F8.6, 1X, A)') &
                'LinFit:  a1=', ctfpa(1), ' a2=', ctfpa(2), ' a3=', ctfpa(3), ' (' // T('given in cps !') // ')', new_line('A'), &
                'ra1= ', rpa(1), ' ra2= ', rpa(2), ' ra3= ', rpa(3), ' (' // T('given in % !') // ')', new_line('A'), &
                'CHi2R=', chisqrr, new_line('A'), &
                'Prob= ', pval(1), ' Prob= ', pval(2), ' Prob= ', pval(3), ' (' // T('t-test-signific. !') // ')'
            call logger(22, log_str)
        end if

        !-----------------------------------------------------------------------
        if (batf .or. batest_user .or. bat_serial) then
            call logger(22, ' ')
        else
            call logger(22, ' ', close=.true.)
        end if

        deallocate(drelf, utest, dfit, SDdfit)

    end subroutine Linfout

end module LF1
