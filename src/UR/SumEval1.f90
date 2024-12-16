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
subroutine SumEvalCalc(yval, uyval)

    ! this routine returns the values yval and its standard uncertainty uyval
    ! by evaluating the UR function SumEval, e.g.,
    !    aSum = SumEval(1,4, a1,a2,a3,a4)
    ! yval and uyval refer to asum, which is to be calculated as the sum or a mean,
    ! of the 4 values a1,a2,a3 and a4. The latter are 4 independently measured
    ! activities.
    !
    ! The uncertainty uyval is calculated by numerical uncertainty propagation over the
    ! independent input quantities (number nux, symbols symb_nux), where covariances
    ! are taken into account when one of the input quantities, e.g. the background count
    ! rate, is used more than once in the calculation of yval.
    !
    ! See chapter 6.13 "Aggregating activities of several aliquots" of the UncertRadio
    ! CHM Help file for more details.
    !
    !     Copyright (C) 2014-2024  Günter Kanisch
    use UR_types
    use UR_params,       only: EPS1MIN, ZERO
    use, intrinsic :: iso_c_binding,   only: c_int
    use gtk,             only: gtk_buttons_OK, GTK_MESSAGE_WARNING
    use UR_gleich,       only: nparts,charv,Symbole,ngrs,nab, &
                               ifehl,Messwert,StdUnc,missingval,upropa_on,ksumeval,nux,symb_nux, &
                               modeSEval,iavar,faliq,upropa_on,RS_SymbolNr,kableitnum,mfactSE
    use UR_DLIM,         only: iteration_on,limit_typ
    use UR_VARIABLES,    only: MCsim_on,modvar_on
    use UWB,             only: gevalf
    use Top,             only: dpafact
    use LF1G,            only: LsqCoxGWM
    use UR_MCC,          only: imc
    use CHF,             only: FindlocT,testSymbol
    use Rout,            only: MessageShow
    use UR_Gspk1Fit,     only: Uxa,UxaMV
    use Num1,            only: matwrite            ! 9.1.2024
    use file_io,         only: logger
    use translation_module, only: T => get_translation

    implicit none

    real(rn),intent(out)   :: yval,uyval

    integer                   :: i,j,nabx,symb_nabx(25),k1,k2,j1,nr,n1,kfound,iw,inet,kqt
    real(rn)                  :: gwx(1),gwcx(1,1),chisq,dpa
    real(rn),allocatable      :: xvect(:),FV1(:),FV2(:),MesswertKP(:),ableit(:,:), &
                                 dpi(:,:),xvuncorrt(:),Unux(:,:)
    logical                   :: ok,found,wzero
    character(:), allocatable :: str1
    character(len=512)        :: log_str
    integer(C_int)            :: resp

    ! nparts: second parameter in the EvalSum function: number of activity values
    ! avar  : names of activity variables being arguments of SumEval function
    ! iavar : associated indexes in the symbol list
    ! nux   : number of independent input quantities
    ! symb_nux() : array symbols of independent input quantities
    !

    allocate(xvect(nparts))
    allocate(FV1(nparts),FV2(nparts))
    allocate(MesswertKP(ngrs))
    allocate(Ableit(nparts,1))
    allocate(xvuncorrt(nparts))
    allocate(character(len=400) :: str1)

    MesswertKP(1:ngrs) = Messwert(1:ngrs)

    kqt = 1
    if(iteration_on .and. limit_typ == 1) kqt = 2
    if(iteration_on .and. limit_typ == 2) kqt = 3

    if(.not.iteration_on .and. kqt == 1 .and. kableitnum > 0) then
        ! 10.1.2024
        do k1=1,nparts
            xvect(k1) = Messwert(iavar(k1))
        enddo
        Uxa(1:nparts,1:nparts) = UxaMV(1:nparts,1:nparts)
        goto 70
    end if

    if(nux > 0) goto 50

    nabx = 0
    symb_nabx = 0
    nux = 0
    symb_nux = 0
    ! Tests whether the dependent symbol with number iavar(i) (<= nab) depends on the symbol
    ! with number j.
    do i=1,nparts
        do j=nab+1,ngrs   ! loop over independent (input) symbols
            call FindSymb(iavar(i),j,found, kfound)
            if(found) then
                if(nux == 0) then
                    nux = nux + 1
                    symb_nux(nux) = j
                else
                    if(findloc(symb_nux,j,dim=1) == 0) then
                        nux = nux + 1
                        symb_nux(nux) = j
                    end if
                end if
            end if
        end do
    end do

50  continue

    allocate(dpi(nux,nparts))
    if(.not. allocated(Uxa)) allocate(Uxa(nparts,nparts))
    if(.not. allocated(UxaMV)) allocate(UxaMV(nparts,nparts))
    if(.not. allocated(Unux)) allocate(Unux(nux,nux))

    Unux = ZERO
    do i=1,nux
        Unux(i,i) = StdUnc(symb_nux(i))**2.0
    end do

    if(.true. .and. .not.iteration_on .and. .not.upropa_on .and. (.not.MCsim_on .or. modvar_on)) then
        write(log_str, '(*(g0))') 'SumEvalCalc: nux=',int(nux,2)
        call logger(66, log_str)
        write(log_str, '(*(g0))') 'symbols used in summands:  Symb_nux = ',(Symbole(symb_nux(i))%s,' ',i=1,nux)
        call logger(66, log_str)
    end if

    if(.true. .and. .not.iteration_on .and. .not.upropa_on .and. .not.MCsim_on .and. .not.modvar_on) then
        do i=1,nparts
            inet = RS_SymbolNR(iavar(i),2)
            iw = RS_SymbolNR(iavar(i),1)
            mfactSE(i) = Messwert(iavar(i)) / (Messwert(inet) * Messwert(iw))
        end do
    end if

    Fv1 = ZERO
    Fv2 = ZERO
    Uxa = ZERO
    wzero = .false.
    do k1=1,nparts

        Fv1(k1) = Messwert(iavar(k1))
        xvect(k1) = Fv1(k1)

        inet = RS_SymbolNR(iavar(k1),2)
        iw   = RS_SymbolNR(iavar(k1),1)
        if(abs(Messwert(iw)) < EPS1MIN) wzero = .true.
    end do
    if(wzero) then
        write(str1,*) T("A value of a single calibration factor is ZERO!") // new_line('A') // &
                      T("Replace it with e.g. 1.E-7!")

        call MessageShow(trim(str1), GTK_BUTTONS_OK, "SumEval:", resp,mtype=GTK_MESSAGE_WARNING)
        ifehl = 1
        return
    end if

    ! Prepare the partial derivatives and the standard uncertainties as matrices:
    do k1=1,nparts
        do j1=1,nux
            n1 = symb_nux(j1)
            if(abs(StdUnc(n1) - missingval) < EPS1MIN .or. abs(StdUnc(n1)) < EPS1MIN) then
                dpi(j1,k1) = ZERO
                cycle
            end if
            dpa = Messwert(n1)*dpafact(Messwert(n1)) - Messwert(n1)
            if(abs(dpa) < EPS1MIN) dpa = 1.0E-10_rn
            Messwert(n1) = Messwert(n1) + dpa
            do j=nab,ksumeval+1,-1
                Messwert(j) = gevalf(j,Messwert)
            end do
            Fv2(k1) = gevalf(iavar(k1),Messwert)
            dpi(j1,k1) = (Fv2(k1)/dpa - Fv1(k1)/dpa)     ! partial derivative with respect to messwert(symb_nux(j1))
            Messwert(1:ngrs) = MesswertKP(1:ngrs)              ! restore the whole messwert array
        end do
    end do
    !-------------------------------------------------
    !  10.1.2024:
    Uxa = matmul(transpose(dpi),matmul(Unux, dpi))
    !-------------------------------------------------

    if( (kqt == 1 .and. kableitnum == 0) .or. (kqt > 1 .and. modvar_on))  then
        UxaMV(1:nparts,1:nparts) = Uxa(1:nparts,1:nparts)      ! 10.1.2024
    endif

    if(.not.upropa_on .and. .not.iteration_on .and. .not.MCsim_on)  then    !  .or. modvar_on)) then

        write(log_str, '(*(g0))') 'SE:   summands:  xvect: ',sngl(xvect(1:nparts))
        call logger(66, log_str)
        call matwrite(Uxa,nparts,nparts,66,'(1x,20es14.6)','SE:   xvect-covariance Matrix Uxa:')
        call logger(66, ' ')
    end if

70  continue

    if(modeSEval == 1) then
        ! mean of activity values, use the least squares mean (Cox et al.):
        nr = 1
        call LsqCoxGWM(xvect,Uxa,nparts,nr,nparts,gwx,gwcx,chisq,ableit,ok)

        IF(.not.upropa_on .and. .not.iteration_on .and. .not.MCsim_on) THEN
            write(log_str, '(*(g0))') 'Least-sq. weighted mean: ',sngl(gwx(1)),  &
                                      '   uncertainty   =',sngl(SQRT(gwcx(1,1))), &
                                      '  ChisqR=',sngl(chisq/real(MAX(1,nparts-1),rn))
            call logger(66, log_str)
        end if
        yval = gwx(1)
        uyval = SQRT(gwcx(1,1))
    end if

    if(modeSEval == 2) then
        ! Sum of activity values:
        yval = sum(xvect(1:nparts))
        if(.not.iteration_on .and. .not.upropa_on .and. .not. MCsim_on) then
            faliq(1:nparts) = xvect(1:nparts) / sum(xvect(1:nparts))
        end if
        if (.not. MCsim_on .or. modvar_on) then
            uyval = ZERO
            do k1=1,nparts
                do k2=1,nparts
                    uyval = uyval + Uxa(k1,k2)
                end do
            end do
            uyval = sqrt(uyval)
        end if
    end if

end subroutine SumEvalCalc
