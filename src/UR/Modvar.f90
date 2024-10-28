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
subroutine ModVar(kqtyp,RD)

!-----------------------------------------------------------------------
!
! The calculation of a decision threshold (DT) or a detection limit (DL) is
! based on (modfied) "assumed" values y_tilde of the output quantity:
!   DT:  y_tilde = 0 ,
!   DL:  y_tilde = DT + k_beta*u(y_tilde) (solved by iteration)
! Assuming a general form y = w*RD = w*(Rgross - Rback), for each value y_tilde
! the corresponding value of RD (i.e. RD_tilde, a net count rate) serves as input
! to Modvar.
! In this context, ModVar modifies the gross count rate(s) (and its standard
! uncertainty) associated with the assumed value y_tilde (or RD=RD_tilde).
!
! Modvar is called from:
!    Rechw2 (for calculating DT and DL according to ISO 11929-1)
!    Prfunc ( which controls the necessary iterations within brentx)
! In the case of the MC simulation (ISO 11929-2), Modvar is called (via Prfunc)
! from MCsingRun.
!
! Rechw2/detlim_iter:
! For the purpose of being called from Rechw2/detlim_iter, only the array elements
! Messwert(.) and StdUnc(.) are modified; the corresponding arrays with names *SV(.)
! must not be modfied, because they are needed for restoring Messwert(.) and StdUnc(.).
!
! MC simulation:
! It is different for the MC simulation, where now values of the arrays MesswertSV(.)
! serve as (modified) means and standard deviations, from which random values are
! generated. After having finished the simulations, the arrays MesswertORG(.) and
! StdUncORG(.) serve for restoring the arrays Messwert(.), StdUnc(.), MesswertSV(.)
! and StdUncSV(.)
!
! If the routine gross_unc_intpol(kunit,uxg_tilde) is used, it always works with the
! arrays Messwert(.) and StdUnc(.) (not with *SV(.)), which must be available/
! be set before calling gross_unc_intpol!
!
! Use of the variable GamDistAdd (GDA) in Modvar:
!
!   With MC simulation:
!   GDA must not be added explicitly when generating random values of counts, because
!   this addition is done within the random generator (in this case rgamma).
!
!     Copyright (C) 2014-2024  Günter Kanisch
!
!-----------------------------------------------------------------------------------------

    use UR_Gleich,     only: Messwert,StdUnc,kEGr,kbrutto,knetto,Symbole,SymboleG,SDformel, &
                             RSeite,ip_binom,kbgv_binom,itm_binom,knumEGr,nab,ncov,ngrs, &
                             nonPoissGrossCounts,nvar,kpoint,ivtl,kbrutto_gl,bipoi_gl, &
                             RS_SymbolNr,iptr_cnt,iptr_time,MEsswertSV,StdUncSV,IAR,N_preset, &
                             ifehl,ksumeval,nparts,modeSEval,iavar,faliq,unit_conv_fact,apply_units_dir, &
                             use_bipoi,Nbin0_MV,missingval,FP_for_units,mfactSE
    use UR_Linft
    use fparser,       only: initf,evalf, EvalErrMsg, Parsef
    use UR_DLIM,       only: fakrb,Flinear,GamDistAdd,k_autoform,var_brutto_auto,RblTot
    use UR_Gspk1Fit
    use UR_Variables,  ONLY: MCsim_on,modvar_on
    use UWB,           only: gevalf,upropa
    use UR_perror
    use Top,           only: dpafact
    use Usub3,         only: FindMessk
    use UWB,           only: Resulta
    use Num1,          only: funcs
    use KLF,           only: CalibInter
    use LF1,           only: Linf
    use LF1G,          only: Linfg1
    use UR_types
    use UR_params,     only: EPS1MIN,ZERO,ONE,TWO
    use Brandt,        only: pnorm
    use CHF,           only: FindLocT,ucase,testSymbol

    use UR_MCC,        only: test_mg
    use PDFs,          only: BinPoi_2_PDF
    use file_io,       only: logger
    use RND,           only: scan_bipoi2

    implicit none

    real(rn),INTENT(IN)      :: RD          ! net count rate (procedure dependent)
    integer   ,intent(in)    :: kqtyp       ! 2: case of DT (decision threshold);
    ! 3: case of DL (detection limit).

    integer               :: i, k, messk,kix,k3,j0,kmmod,k1,k2,kqt,kunit,iimin
    integer               :: nvar_org,krgross,kngross,kgrosstime,krback,knback,kbacktime
    integer               :: inet,iw,ir,if3,i_arr(numd/5),nhh_arr(numd/5)
    real(rn)              :: rn0x, SDrn0x, aktx, SDaktx, rback, urback, afunc(ma)
    real(rn)              :: fpar,res,dpi,yval,uyval,dummy,help,xnm,xng,xp,xN0m,aweif
    real(rn)              :: dpi_gevalf
    real(rn)              :: uxg_tilde,RblTotMC,RblTot_T1
    logical               :: test,apply_SV
    character(:), allocatable  :: rsfG,sdfsave,ccc,string
    character(len=512)         :: log_str

    allocate(character(len=300) :: rsfG)              ! 12.8.2023
    allocate(character(len=300) :: sdfsave)           ! 12.8.2023
    allocate(character(len=300) :: ccc)               ! 12.8.2023
    allocate(character(len=2000) :: string)           ! 12.8.2023

    apply_SV = apply_units_dir

    if(apply_units_dir) FP_for_units = .false.   ! refers to the function parser

    nonPoissGrossCounts = .false.
    kunit = 30
    if(MCsim_on) kunit = 63

    nvar_org = nvar
    kqt = kqtyp

    IF(FitDecay) then
        ! fitting a decay curve with Linfit():
        ! The values and uncertainties of gross count rates are re-calculated here.
        ! The net count rate RD used as input to ModVar, has to be calculated by the
        ! function RnetVal:
        !   RD = RnetVal(y_tilde)
        ! (RnetVal calculates the net count rate from a value y = w*RD of the output
        ! quantity y.

        ! rback = MesswertSV(kpoint(k_rbl)) + d0zrate(1)
        ! urback = SQRT( StdUnc(kpoint(k_rbl))**two + sd0zrate(1)**two )

        if(kqt > 1 .and. abs(fpaLyt(kqt,1)) < EPS1MIN) kqt = kqt - 1       ! 12.6.2024

        ! kann im Prinzip stillgelegt werden, da z.ZT. für PMLE gilt: ifit(mfrbg) = 3
        if(.false. .and. kPMLE == 1 .and. mfrbg > 0) then          ! 6.6.2024
            if(ifit(mfrbg) == 2) then
                dummy = ZERO
                if(k_rbl > 0) dummy = MesswertSV(kpoint(k_rbl))
                if(singlenuk) then
                    if(abs(fpaLYT(kqt,mfrbg)-ONE) < EPS1MIN ) then
                        fpaLYT(kqt,mfrbg) =  d0zrate(1) + dummy
                        if(mfrbg-1 > kEGr) then
                            fpaLYT(kqt,mfrbg-1) = fpaLYT(kqt,mfrbg-1) - (d0zrate(1) + dummy)
                        end if
                    end if
                else
                    if(abs(fpaLYT(kqt,mfrbg)-ONE) < EPS1MIN ) then
                        fpaLYT(kqt,mfrbg) = d0zrate(1) + dummy
                        if(mfrbg-1 > kEGr) then
                            fpaLYT(kqt,mfrbg-1) = fpaLYT(kqt,mfrbg-1) - (dummy + d0zrate(1))
                        end if
                    end if
                end if
            end if
        end if

        do i=1,numd

            kix = ngrs+ncov+i
            Messwert(kix) = ZERO
            !----
            messk = FindMEssk(i)
            IF(konstant_r0) THEN
                d0zrate(i) = R0k(messk)
                sd0zrate(i) = sdR0k(Messk)
            else
                d0zrate(i) = d0zrateSV(i)
                sd0zrate(i) = sd0zrateSV(i)
            end if
            rback = d0zrate(i)
            if(k_rbl > 0) rback = rback + MesswertSV(kpoint(k_rbl))
            urback = sd0zrate(i)**TWO
            if(k_rbl > 0) urback = urback + StdUnc(kpoint(k_rbl))**TWO
            urback = sqrt(urback)
            !---------------------------------
            ! re-calculate the gross count rates:
            call Funcs(i,afunc)

            Messwert(kix) = d0zrate(i)
            if(k_rbl > 0) Messwert(kix) = Messwert(kix) + Messwert(kpoint(k_rbl))

            do k=1,ma   ! (3)
                if(ifit(k) == 3) cycle
                if(k == kEGr) then
                    fpar = RD
                    Messwert(kix) = Messwert(kix) + fpar*afunc(k)
                else
                    fpar = max(ZERO, fpaLYT(kqt,k))
                    ! if(kPMLE == 1 .and. kqtyp > 1 .and. abs(fpar-zero) < eps1min)  fpar = rback
                    if(ifit(k) == 1) then
                        Messwert(kix) = Messwert(kix) + fpar*afunc(k)
                    end if
                    if(ifit(k) == 2) then
                        Messwert(kix) = Messwert(kix) + afunc(k)      ! 13.6.2024
                    end if
                end if
            end do

            StdUnc(kix) = SQRT(ABS( Messwert(kix)/dmesszeit(i) ))
        end do

        IF(export_r .and. .not.export_case(2)) call Linf(rn0x,SDrn0x)
        !-----------------------------------------------------------------------
    else IF(Gamspk1_Fit) THEN
        ! in this case, RD is taken as an activity value!
        ! The array varadd_Rn(i) was determined once in Rechw1.
        i_arr = [(i,i=1,numd/5)]
        nhh_arr = (i_arr-1)*5 + 1
        Messwert(ngrs+ncov+nhh_arr) = RD * effi(i_arr) * pgamm(i_arr) / (fatt(i_arr) * fcoinsu(i_arr))
        StdUnc(ngrs+ncov+nhh_arr) = SQRT( (RD * effi(i_arr) * pgamm(i_arr) /  &
            (fatt(i_arr) * fcoinsu(i_arr)))/Messwert(kpoint(2)) + varadd_Rn(i_arr) )
        SDaktnzMV = ZERO
        call Linfg1(aktx,SDaktx)
        ! write(63,*) 'Modvar 213 Gamspk1: aktx=',sngl(aktx),' SDaktx=',sngl(SDaktx),' RD=',sngl(RD)
        fpaSV(kEGr) = aktx
        fpa(kEGr)   = aktx
        do i=1,numd/5
            if(abs(SDaktnzMV(i)) < EPS1MIN) SDaktnzMV(i) = SDaktnz(i)        ! ...MV for modvar values
        end do

        !-----------------------------------------------------------------------
    elseif(FitCalCurve .and. netto_involved_Fitcal) then
        ! for the example 8 in the "Beiblatt zu ISO 11929"
        Messwert(nvar) = (RblTot(kEGr)*FakRB**ZERO + RD)/FakRB
        if(abs(MEsswert(nvar)) < EPS1MIN) Messwert(nvar)= 1.E-13_rn
        StdUnc(nvar) = gevalf(kbrutto_gl(kEGr),Messwert)
        do k3=nvar-1,knetto(kEGr),-1
            res = gevalf(k3,Messwert)
            Messwert(k3) = res
        end do
        dpi = dpi_gevalf(kbrutto(kEGr),knetto(kEGr))
        StdUnc(knetto(kEGr)) = sqrt( (dpi*StdUnc(kbrutto(kEGr)))**TWO )
        !-----------------------------------------------------------------------
    else if(SumEval_fit) then
        ! RD in this case is treated as an activity value!

        do ir=1,nparts
            inet = RS_SymbolNR(iavar(ir),2)
            iw   = RS_SymbolNR(iavar(ir),1)
            if3  = RS_SymbolNR(iavar(ir),3)


            if(inet == 0) then
                write(log_str,*) 'Modvar_231: inet=0 for summand ir=',int(ir,2),' iavar(ir)=',int(iavar(ir),2)
                call logger(kunit, log_str)
            end if
            if(iw == 0) then
                write(log_str,*) 'Modvar_232: iw=0 for summand ir=',int(ir,2),' iavar(ir)=',int(iavar(ir),2)
                call logger(kunit, log_str)
            end if
            krgross = RS_SymbolNR(inet,1)
            krback =  RS_SymbolNR(inet,2)
            kngross = iptr_cnt(krgross)
            kgrosstime = iptr_time(krgross)
            knback = iptr_cnt(krback)            ! knback wird gar nicht benutzt
            kbacktime = iptr_time(krback)
            aweif = ONE
            if(if3 > 0) then
                aweif = Messwert(if3)

            end if
            if(modeSEval == 1) then

                if(kngross > 0) then
                    Messwert(kngross) = ( RD*ONE/Messwert(iw)/mfactSE(ir)/Flinear**ZERO + Messwert(krback) ) * Messwert(kgrosstime)
                else
                    ! if counts are not given, but only the count rate
                    Messwert(krgross) = ( RD*ONE/Messwert(iw)/mfactSE(ir)/Flinear**ZERO + Messwert(krback) ) !   * Messwert(kgrosstime)
                end if
            else if(modeSEval == 2) then
                if(kngross > 0) then
                    Messwert(kngross) = ( RD*faliq(ir)/(Messwert(iw)*aweif)/Flinear**ZERO + Messwert(krback) ) * Messwert(kgrosstime)
                else
                    ! if counts are not given, but only the count rate
                    Messwert(krgross) = ( RD*faliq(ir)/(Messwert(iw)*aweif)/Flinear**ZERO + Messwert(krback) ) !! * Messwert(kgrosstime)
                end if
            end if
            if(kngross > 0) then
                if(Messwert(kngross) <= ZERO) Messwert(kngross) = ONE
                StdUnc(kngross) = sqrt(Messwert(kngross))
            else
                StdUnc(krgross) = sqrt(Messwert(krgross)/kgrosstime)
            end if
            if(MCsim_on) MesswertSV(kngross) = Messwert(kngross)
            !write(63,'(3(a,i3),4(a,es12.5),a,i3,2(a,es12.5))') 'Modvar: kqtyp=',kqtyp,' SumEval: ir=',ir, &
            !                 ' kngross=',kngross,  &
            !                 ' Messwert(kngross)=',Messwert(kngross), &
            !                 ' Messwert(kgrosstime)=',Messwert(kgrosstime),' Flin=',Flinear, &
            !                 ' faliq(ir)=',faliq(ir),' krback=',krback, &
            !                 ' Messwert(krback)=',Messwert(krback),' RD=',RD
        end do
        do i=nab,ksumeval+1,-1
            Messwert(i) = gevalf(i,Messwert)
        end do

        UxaMV = ZERO
        if(MCsim_on) modvar_on = .true.
        call SumEvalCalc(aktx,SDaktx)     ! calls also gevalf!
        modvar_on = .false.

        Messwert(ksumeval) = aktx
        StdUnc(ksumeval) = SDaktx

        do k=1,nparts
            if(abs(UxaMV(k,k)) < EPS1MIN) UxaMV(k,k) = Uxa(k,k)        ! ...MV für modvar-Werte
        end do


        !-----------------------------------------------------------------------
    else

        apply_units_dir = .false.

        ! If sdformel (SD formula) contains a symbol with its index < nvar,
        ! its value must be re-calculated, with Resulta:
        iimin = 1000
        string = ucase(sdformel(nvar)%s)
        do i=knumEgr+1,ngrs
            test = testSymbol(trim(string),SymboleG(i)%s)
            if(test) then
                iimin = min(iimin,i)
            end if
        end do
        do i=1,1
            ! For i=1 re-calculate the array Messwert, because the equation number nvar
            ! can contain symbols with their indexes being < nvar
            Messwert(nvar) = ( RblTot(kEGr) + RD)/FakRB

            if(use_bipoi .and. .not. MCsim_on) then
                RblTot_T1 = (MesswertSV(iptr_cnt(kbgv_binom)) + GamDistAdd) / MesswertSV(iptr_time(kbgv_binom))
                if(kqtyp == 2) then
                    if(.not.test_mg) then
                        Messwert(nvar) = RblTot_T1 + RD
                    else
                        Messwert(nvar) = RblTot_T1 + RD*MesswertSV(ip_binom)/MesswertSV(iptr_time(nvar))
                    end if
                else
                    if(ubound(unit_conv_fact,dim=1) == 0) then
                        if(.not.test_mg) then
                            Messwert(nvar) = RblTot_T1 + RD
                        else
                            Messwert(nvar) = RblTot_T1 + RD*MesswertSV(ip_binom)/MesswertSV(iptr_time(nvar))
                        end if
                    else
                        if(.not.test_mg) then
                            Messwert(nvar) = RblTot_T1 + RD*unit_conv_fact(iptr_time(nvar))
                        else
                            Messwert(nvar) = RblTot_T1 + RD*MesswertSV(ip_binom)/MesswertSV(iptr_time(nvar)) &
                                * unit_conv_fact(iptr_time(nvar))
                        end if
                    end if
                end if
            end if

            if(use_bipoi .and. MCsim_on) then
                RblTotMC = MesswertSV(iptr_cnt(kbgv_binom))/MesswertSV(iptr_time(kbgv_binom))
                if(.not.test_mg) then
                    if(ubound(unit_conv_fact,dim=1) > 0) then
                        Messwert(nvar) = RblTotMC + RD*unit_conv_fact(iptr_time(nvar))
                    else
                        Messwert(nvar) = RblTotMC + RD
                    end if
                else
                    Messwert(nvar) = RblTotMC + RD*MesswertSV(ip_binom)/MesswertSV(iptr_time(nvar))
                    NBin0_MV = RD
                end if
            end if
            if(IVTL(nvar) == 7 .or. IVTL(nvar) == 4) then
                IF(Messwert(nvar) + GamdistAdd <= ZERO) then
                    Messwert(nvar) = -GamdistAdd + 1.0E-11_rn
                END if
            end if
            if(i == 1 .and. iimin < nvar) dummy = Resulta(iimin)  ! recalculate all MEsswert values with index >= iimin
        end do

        if(iptr_cnt(nvar) > 0 .and. kbgv_binom == 0) then
            !  iptr_cnt:  points from a counts-variable to the associated count rate variable
            !  iptr_time: points to the counts-variable associated with the counting time

            if(.not.N_preset) then
                if(GamDistAdd > ZERO .and. (MCsim_on)) then
                    Messwert(iptr_cnt(nvar)) = max(-GamdistAdd, Messwert(nvar)*Messwert(iptr_time(nvar))- ZERO*GamDistAdd)
                    if(MCsim_on) MesswertSV(iptr_cnt(nvar)) = Messwert(iptr_cnt(nvar))
                else
                    if(iptr_time(nvar) > 0) then
                        Messwert(iptr_cnt(nvar)) = Messwert(nvar)*Messwert(iptr_time(nvar))
                    else
                        Messwert(iptr_cnt(nvar)) = Messwert(nvar)
                    end if
                end if

                if(len_trim(SDformel(iptr_cnt(nvar))%s) > 0) then
                    k = FindlocT(Rseite,SDformel(iptr_cnt(nvar))%s)
                    if( k > 0) then
                        StdUnc(iptr_cnt(nvar)) = gevalf(k,Messwert)
                        if(MCsim_on) StdUncSV(iptr_cnt(nvar)) = StdUnc(iptr_cnt(nvar))
                    end if
                elseif(iptr_cnt(nvar) == k_autoform) then
                    Messwert(k_autoform) = Messwert(nvar) * Messwert(iptr_time(nvar))
                    if(MCsim_on) MesswertSV(k_autoform) = MesswertSV(nvar) * Messwert(iptr_time(nvar))
                    call gross_unc_intpol(kunit, uxg_tilde)
                    StdUnc(k_autoform) = uxg_tilde
                    if(MCsim_on) StdUncSV(k_autoform) = uxg_tilde
                end if

            elseif(N_Preset) then
                ! Measurement modus with preset counts:
                Messwert(iptr_time(nvar)) = Messwert(iptr_cnt(nvar)) / Messwert(nvar)
                if(MCsim_on) then
                    MesswertSV(nvar) = Messwert(nvar)
                    MesswertSV(iptr_time(nvar)) = Messwert(iptr_time(nvar))
                end if
                StdUnc(nvar) = gevalf(kbrutto_gl(kEGr),Messwert)
                if(MCsim_on) StdUncSV(nvar) = StdUnc(nvar)
            end if
        end if

        if(kbgv_binom > 0 .and. iptr_cnt(nvar) > 0) then
            if(ivtl(iptr_cnt(nvar)) == 7) then
                if(.not.test_mg) then
                    xng = Messwert(nvar)*Messwert(iptr_time(nvar))
                    if(ubound(unit_conv_fact,dim=1) == 1) xng = xng * unit_conv_fact(iptr_time(nvar))
                else
                    xng = Messwert(nvar)*Messwert(itm_binom)    ! modified gross counts
                    Messwert(iptr_cnt(nvar)) = xng
                end if
                if(.not.MCsim_on .or.(MCsim_on .and. .not.test_mg)) &
                    Messwert(iptr_cnt(nvar)) = xng - GamDistAdd
                xn0m = MesswertSV(iptr_cnt(kbgv_binom))   ! background counts
                xn0m = xn0m * Messwert(itm_binom)/Messwert(iptr_time(kbgv_binom))
                xnm = xng - xn0m
                xp = Messwert(ip_binom)

                nonPoissGrossCounts = .true.

                if( abs(StdUncSV(ip_binom) - missingval) < EPS1MIN .or. abs(StdUncSV(ip_binom)) < EPS1MIN) then
                    if(kqtyp /= 2) then
                        call scan_bipoi2(MesswertSV(ip_binom),Nbin0_MV,RblTot(kEGr),MesswertSV(itm_binom))
                    end if
                end if

                if(bipoi_gl(kEGr) > 0) then
                    StdUnc(iptr_cnt(nvar)) = gevalf(bipoi_gl(kEGr), Messwert)  ! Calculate the standard uncertainty
                    StdUnc(nvar) = StdUnc(iptr_cnt(nvar))/Messwert(iptr_time(nvar))

                end if
            end if
        end if

        if(IVTL(kbrutto(kEGr)) == 11) then
            ! Measurements with preset counts
        end if

        ! Model for the variation of the variance of p(nvar):
        !  StdUnc(nvar) = sqrt(Messwert(nvar) / tm)
        !--------------
        rsfg = ''
        if(.not.var_brutto_auto) then
            rsfG = trim(ucase(Rseite(kbrutto_gl(kEGr))%s))
            sdfsave = Rseite(kbrutto_gl(kEGr))%s
        end if

        j0 = index(rsfG,'KALFIT')
        kmmod = 0
        if(j0 == 0) then
            ! Search the equations whether they contain the symbol of the gross count rate
            if(kbrutto(kEGr) > 0 .and. .not.FitDecay .and. .not. Gamspk1_Fit) then
                do k2=kEGr+1,nab
                    if(k2 == kbrutto(kEGr)) cycle
                    if(k2 == knetto(kEGr)) cycle
                    if(testSymbol(rsfG,SymboleG(k2)%s)) then
!                         if(FitCalCurve)  write(66,*) 'found:  Symbol: ',symbole(k2)%s
                        if (FitCalCurve) call logger(66, 'found:  Symbol: ' // symbole(k2)%s)

                        ! The formula in equation number kmmod is used as a function, being called from the
                        ! formula which defines the standard uncertainty of the gross count rate.
                        ! This function is applied for the KALFIT table with a polynomial to be fitted.
                        kmmod = k2
                        ccc = trim(ucase(Rseite(kmmod)%s))
                        if(index(ccc,'KALFIT') > 0) then
                            do k1=kEGr+1,ngrs
                                if(k1 == RS_SymbolNr(kmmod,1)) then
                                    call CalibInter(KFmode, Messwert(k1), ZERO, yval,uyval)
                                    Messwert(kmmod) = yval
                                    exit
                                end if
                            end do
                        else
                            Messwert(kmmod) = gevalf(kmmod, Messwert)
                            exit
                        end if
                    end if
                end do
            end if
        else    ! j0 = 0
            ! here: j0 > 0 : i.e., Kalfit
            call parsef(kbrutto_gl(kEGr), Rseite(kbrutto_gl(kEGr))%s,SymboleG)
            if(ifehl == 1) then
                goto 9000
            end if
            if(.not.var_brutto_auto) then
                StdUnc(nvar) = gevalf(kbrutto_gl(kEGr),Messwert)
!                 write(30,*) 'MV_03: StdUnc(nvar)=',sngl(StdUnc(nvar))
                write(log_str, '(*(g0))') 'MV_03: StdUnc(nvar)=',sngl(StdUnc(nvar))
                call logger(30, log_str)
            end if
            if(iar(nvar) == 2) StdUnc(nvar) = StdUnc(nvar) * Messwert(nvar)
            Rseite(kbrutto_gl(kEGr))%s = trim(sdfSave)
        end if

        if(.not.var_brutto_auto) then
            ! The following statement is necessary
            StdUnc(nvar) = gevalf(kbrutto_gl(kEGr),Messwert)  ! calculate the standard uncertainty value
            ! according to the formula kbrutto_gl(kEGr)
        elseif(var_brutto_auto) then
            !!!!!!!!    if(MCsim_on) Messwert(k_autoform) = MesswertSV(k_autoform)   ! False
            call gross_unc_intpol(kunit, uxg_tilde)
            StdUnc(k_autoform) = uxg_tilde
            if(MCsim_on) StdUncSV(k_autoform) = uxg_tilde
            if(k_autoform > kbrutto(kEGr)) then
                if(iptr_cnt(kbrutto(kEGr)) == k_autoform) then
                    StdUnc(nvar) = StdUnc(k_autoform)/Messwert(iptr_time(kbrutto(kEGr)))
                    if(MCsim_on) StdUncSV(nvar) = StdUncSV(nvar)
                end if
            end if
        end if
        if(iar(nvar) == 2) StdUnc(nvar) = StdUnc(nvar) * Messwert(nvar)
        if(iar(nvar) == 2 .and. MCsim_on) StdUncSV(nvar) = StdUncSV(nvar) * MesswertSV(nvar)

        !------------------------------------------
        if(MCsim_on) then
            ! modify now also the *SV values:
            MesswertSv(nvar) = Messwert(nvar)
            StdUncSV(nvar) = StdUnc(nvar)

            if(kbgv_binom > 0) then
                MesswertSV(knetto(kEGr)) = MesswertSV(nvar) - RblTot(kEGr)   !  <-   only for MC-simul.!
                MesswertSV(iptr_cnt(kbgv_binom)) = Messwert(iptr_cnt(kbgv_binom))
                MesswertSV(iptr_cnt(nvar)) = Messwert(iptr_cnt(nvar))
                if(.not.test_mg) then
                    xng = MesswertSV(nvar)*MesswertSV(iptr_time(nvar))
                    if(ubound(unit_conv_fact,dim=1) == 1) xng = xng * unit_conv_fact(iptr_time(nvar))
                else
                    xng = MesswertSV(nvar)*MesswertSV(itm_binom)    ! modified gross counts
                    MesswertSV(iptr_cnt(nvar)) = xng
                end if
                xn0m = MesswertSV(iptr_cnt(kbgv_binom)) + 0.*GamDistAdd   ! background counts
                xn0m = xn0m * MesswertSV(itm_binom)/MesswertSV(iptr_time(kbgv_binom))
                xnm = xng - xn0m
                xp = MesswertSV(ip_binom)
                if(bipoi_gl(kEGr) > 0) then
                    StdUncSV(iptr_cnt(nvar)) = gevalf(bipoi_gl(kEGr), MesswertSV)  ! calculate the standard uncertainty value
                    ! according to the formula kbrutto_gl(kEGr)
                    StdUncSV(nvar) = StdUncSV(iptr_cnt(nvar))/MesswertSV(iptr_time(nvar))
                end if
                goto 155
            end if

            if(iptr_cnt(nvar) > 0 .and. (.NOT. FitDecay .and. .not. Gamspk1_Fit .and. .not.N_preset) ) then
                if(iptr_time(nvar) > 0) then
                    MesswertSV(iptr_time(nvar)) = Messwert(iptr_time(nvar))   ! counting time
                    StdUncSV(iptr_time(nvar)) = StdUnc(iptr_time(nvar))       ! uncertainty counting time
                else
                    ! ??
                end if

                ! iptr_cnt:  points from a counts-variable to the associated count rate variable
                ! iptr_time: points to the counts-variable associated with the counting time

                ! The value of the gross count rate (index nvar), which comes from the analytical
                ! evaluation, already contains GamDistAdd, so that the gross counts are derived here
                ! without GamDistAdd.

                help = MesswertSV(nvar)               ! gross count rate
                if(iptr_time(nvar) > 0) then
                    MesswertSV(iptr_cnt(nvar)) = max(-GamdistAdd, help*MesswertSV(iptr_time(nvar))- ZERO*gamdistadd)
                    StdUnc(iptr_cnt(nvar)) = StdUnc(nvar) * MesswertSV(iptr_time(nvar))
                else
                    MesswertSV(iptr_cnt(nvar)) = max(-GamdistAdd, help*ONE)     ! modif. value of gross counts
                    StdUnc(iptr_cnt(nvar)) = StdUnc(nvar) * ONE                 ! its uncertainty
                end if
                StdUncSV(iptr_cnt(nvar)) = StdUnc(iptr_cnt(nvar))

                if(len_trim(SDformel(iptr_cnt(nvar))%s) > 0) then
                    k = findlocT(Rseite,sdformel(iptr_cnt(nvar))%s)
                    if(k > 0) then
                        if(testSymbol(Rseite(k)%s,symbole(iptr_cnt(nvar))%s)) then
                            StdUnc(iptr_cnt(nvar)) = gevalf(k,Messwert)
                        else
                            nonPoissGrossCounts = .true.
                            ! required for: example project ISO-Beispiel-2a_DE.txp :
                            !          the iterated gross count rate in this case must not be
                            !          derived internally from the gross counts; see routine Resulta.
                        end if
                    end if
                elseif(iptr_cnt(nvar) == k_autoform) then
                    MEsswert(k_autoform) = MesswertSV(k_autoform)
                    call gross_unc_intpol(kunit, StdUncSV(k_autoform))
                end if
            end if
155         continue
        end if
        !--------End of case MCSim_on ----------------------------------

        if(kmmod > 0) then
            ! restore the value with index kmmod, which may have been temporarily modified
            Messwert(kmmod) = MesswertSV(kmmod)
        end if

    end if

9000 continue

    if(ifehl == 1) call logger(kunit, 'Warning:    Modvar: ifehl = 1')
    apply_units_dir = apply_SV


end subroutine ModVar

!########################################################################

!     Copyright (C) 2014-2024  Günter Kanisch

real(rn) function dpi_gevalf(mwind,indeval)
    use UR_params,      only: rn
    use top,            only: dpafact
    use UR_Gleich,      only: Messwert
    use UWB,            only: gevalf

    implicit none

    integer   ,intent(in)    :: mwind
    integer   ,intent(in)    :: indeval

    real(rn)       :: Fv1,Fv2,dpa

    Fv1 = gevalf(indeval,Messwert)
    dpa = Messwert(mwind)*dpafact(Messwert(mwind)) - Messwert(mwind)
    Messwert(mwind) = Messwert(mwind) + dpa
    Fv2 = gevalf(indeval,Messwert)
    Messwert(mwind) = Messwert(mwind) - dpa
    dpi_gevalf = (Fv2/dpa - FV1/dpa)

end function dpi_gevalf


!#######################################################################

subroutine gross_unc_intpol(kunit, uxg_tilde)

!     Copyright (C) 2014-2024  Günter Kanisch

    use UR_types
    use UR_params,      only: ONE,TWO
    use UR_Gleich,      only: DistPars,Symbole,Messwert,kbrutto,knetto,kEGr,var_rbtot,urelw, &
                              nvars_in_rbtot,vars_rbtot,StdUncSV,fBayMD,k_datvar, &
                              MDpointrev,iptr_time,nRSsy,RS_SymbolNr,STDUnc,iptr_cnt,umeanMD,   &
                              rinflu_known,refdataMD,theta_ref,SDwert,nvalsMD,k_MDtyp
    use UR_DLIM,        only: Flinear,Rbltot,k_autoform,fBay_g
    use CHF,            only: FindLocT
    use file_io,        only: logger
    implicit none

    integer, intent(in)    :: kunit
    real(rn), intent(out)  :: uxg_tilde

    integer       :: nn,n1,nrback,ka
    real(rn)      :: xg0,xnueg,varg0,xnueb,qx,varxg,fg,varb_add,fb,w
    real(rn)      :: xg_tilde,w2,xnet,xnet_tilde,sb2,fBay_b,tgr,t0,varb_n0,sg2
    character(len=128) :: log_str

    k_datvar = MDpointrev(k_autoform)

    w = Flinear
    w2 = w*w

    if(rinflu_known) goto 100

    !-------------------------------------------------------------------------------
    ! random influences: large influence

    if(k_autoform == kbrutto(kEGr)) then
        ! The kbrutto quantity (gross count rate) is now a mean value:
        xg_tilde = Messwert(k_autoform)        ! kbrutto(kEGr))
        xnet_tilde = xg_tilde - Rbltot(kEGr)
        tgr = ONE
        varb_add = var_rbtot    ! can be a sum of more than one background contributions
        if(nRSsy(knetto(kEGr)) == 2) then
            ! net count rate consists of the count rates: gross minus backgr.
            nrback = RS_SymbolNr(knetto(kEGr),2)
            varb_add = StdUnc(nrback)**TWO
        end if
        fBay_g = fBayMD(k_datvar)
    else
        ! the number of gross counts is now given as a mean value:
        !  iptr_cnt:  points to the counts of the associated count rate variable
        !  iptr_time: points to the counting duration variable connected with the count number variable
        tgr = Messwert(iptr_time(kbrutto(kEGr)))
        xg_tilde = Messwert(k_autoform)        ! kbrutto(kEGr))
        xnet_tilde = xg_tilde - Rbltot(kEGr)*tgr
        varb_add = var_rbtot * tgr**TWO
        if(nRSsy(knetto(kEGr)) == 2) then
            nrback = RS_SymbolNr(knetto(kEGr),2)
            if(iptr_cnt(nrback) > 0) then
            end if
        end if
        fBay_g = fBayMD(k_datvar)
    end if
    nn = findlocT(DistPars%symb,Symbole(k_autoform)%s)
    xg0 = DistPars%pval(nn,2)          ! gross quantity value of the measurement
    xnueg = DistPars%pval(nn,1)
    sg2 = DistPars%pval(nn,3)**TWO
    varg0 = umeanMD(k_datvar)**TWO     ! uncertainty of xg0

    if(nvars_in_rbtot > 0) then
        n1 = vars_rbtot(1)
        k_datvar = MDpointrev(n1)
        fBay_b = fBayMD(k_datvar)
        nn = findlocT(DistPars%symb,Symbole(n1)%s)
        varb_add = varb_add - StdUncSV(n1)**TWO
        ! If the background consists only of R0, varb_add shall be zero here!
        xnueb = DistPars%pval(nn,1)
        sb2 = DistPars%pval(nn,3)**TWO
        fb = fBay_b
    end if
    xnet = (xg0 - Rbltot(kEGr)*tgr)
    qx = xnet_tilde / xnet
    fg = fBay_g

    if(nRSsy(knetto(kEGr)) == 2) then
        nrback = RS_SymbolNr(knetto(kEGr),2)
        if(iptr_time(nrback) > 0) then
            t0 = Messwert(iptr_time(nrback))
            varb_n0 = StdUnc(nrback)**TWO*t0**TWO
        else
            varb_n0 = StdUnc(nrback)**TWO
        end if
    end if

    if(k_MDtyp(k_datvar) == 1) then
        ! compare with Eq. (15) , UR_Help, section 6.12.3.2
        ! non counts
        varxg = (fg*sb2 + varb_add) + qx*( fg*(sg2-sb2) - varb_add +  &
            (ONE-qx)*xnet**TWO * urelw**TWO )
    else if(k_MDtyp(k_datvar) == 2) then
        ! compare with Eq. (16) , UR_Help, section 6.12.3.3
        ! counts, with influence
        varxg = (varb_n0 + varb_add) + qx *(varg0 -(varb_n0 + varb_add) +  &
            (xnet*urelw)**TWO*(ONE - qx) )
    else if(k_MDtyp(k_datvar) == 3) then
        varxg = (fg*sb2 + varb_add) + qx*( fg*(sg2-sb2) - varb_add +  &
            (ONE-qx)*xnet**TWO * urelw**TWO )
    end if

    uxg_tilde = sqrt(varxg)

    return
    !-------------------------------------------------------------------------------
100 continue

    ! random influences: small influence
    ! theta_ref is calculated MDcalc (Modul Top)

    if(k_autoform /= kbrutto(kEGr)) then
        ka = k_autoform
        if(MDpointrev(ka) /= refdataMD) then
            SDwert(ka) = sqrt( (messwert(ka) + (theta_ref*messwert(ka))**TWO ) &
                / nvalsMD(MDpointrev(ka)) )
            uxg_tilde = SDwert(ka)
            write(log_str,*) 'MW(k_autoform)=',sngl(Messwert(ka))
            call logger(kunit, trim(log_str))
        end if
    end if

end subroutine gross_unc_intpol

!#######################################################################
