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
submodule (Rw2) RW2A

    use UR_params, only: EPS1MIN, ZERO, ONE, TWO, PI
    implicit none

contains
    ! Rechw2
    ! detlim_iter
    ! setupParser
    ! RnetVal


    module subroutine Rechw2()

        ! This routine executes the following set of calculations:
        !
        ! calls setupParser for initiating the function parser and feed it with
        !   all formulae given in the array RSeite; setupParser creates further
        !   formulae, RSeite_one() and RSeite_zero(), which will allow to
        !   determine the comstants Fconst and Flinear in the relation
        !       activity = Fconst + Flinear*netCountRate
        ! several arrays including Messwert and StdUnc are copied to other arrays
        !   (saved) with name endings SV and SVG;
        ! tests the gross count rate Rb in the equation for the net count rate,
        !   whether it appears to be multiplied with a factor /= 1 (called now
        !   FAKRB);
        ! searches for a 2nd gross count rate, a double of the first, with symbol list
        !   index kbrutto_double; such a case is encountered in the UncertRadio
        !   project Fe-55-with-LSC-and-standard-addition_EN.TXP;
        ! estimates the constants Fconst and Flinear using functions func_fconst and
        !   func_flinear; this is combined with few consistency checks and warning
        !   messages;
        ! then a part follows, in which values of most of the characteristic limits
        !   are calculated;
        ! in a further loop the two values of the decision thteshold (DT) and the
        !   detection limit(DL) are calculated using the routine detlim_iter;
        ! during these calculations some values of e.g. Messwert and Stdunc may have
        !   become modfied, therefore, the corresponding arrays are restored from
        !   the saved arrays (with endings "SV").
        !
        !     Copyright (C) 2014-2025  Günter Kanisch

        use, intrinsic :: iso_c_binding,  only: c_null_char,c_ptr,c_int
        use gtk, only: GTK_BUTTONS_OK,gtk_widget_set_visible,GTK_MESSAGE_WARNING

        USE UR_Gleich_globals, only: Symbole,CVFormel,Messwert,RSeite, StdUnc, wpars, &
                                     SDFormel,symtyp,coverf,coverin,DT_increase,DL_increase,ifehl,   &
                                     ilam_binom,ip_binom,increase_dpafact,itm_binom,ISymbA,   &
                                     kbgv_binom,kEGr,kgspk1,klinf,knumEGr,lintest,loadingpro, &
                                     ksumeval,nab,nabf,kbrutto2,kbrutto2_kbd,kbrutto_double,MesswertSVG, &
                                     ncov,ncovf,ngrs,nmodf,nonPoissGrossCounts,nvar,nvars_in_rbtot,  &
                                     nwpars,Resultat,StdUncSVG,covarval,percsum,Rnetmodi,sensi,tback, &
                                     tgross,Ucomb,Ucomb_anf,ueg_increase,urelw,use_bipoi,   &
                                     var_rbtot,covarval,MEsswertSV,StdUncSV,kbrutto,knetto,kbrutto_gl, &
                                     sensiSV,percSV,ueg_normal,iptr_cnt,iptr_time,vars_rbtot,Messwert_CP, &
                                     perc,StdUncSV1,StdUnc_CP,UcontribSV,Ucontrib,CovarvalSV,nwpars, &
                                     WparsInd,maxlen_symb,missingval,nRSsy,RS_SymbolNr, Ucomb_EGr
        use ur_linft, only: fitmeth, FitDecay, fpaLYT, covarLYT, numd, klincall, SumEval_fit, kfitp, &
                            kPMLE, ifit, mfrbg, fpa, FitCalCurve, covar, ma, use_WTLS, Chisqr, sfpa, &
                            sfpaLYT, cauchy_failed1, cauchy_failed2, cauchy_failed3
        use ur_dlim, only: kqtyp, var_brutto_auto, FakRB, Fconst, Flinear, iteration_on, GamDist_Zr, &
                           WertBayes, UcombBayes, W1minusG, KBgrenzo, KBgrenzu, KBgrenzoSH, KBgrenzuSH, &
                           RblTot, uFlinear, kbeta, k_autoform, decthresh, detlim, limit_typ, &
                           nit_decl, iterat_passed, nit_detl
        use ur_general_globals, only: fname,gum_restricted,multi_eval,gross_negative,kmodeltype, &
                                      kmodelold,savep,bat_serial,rw2_on,batf,ableit_fitp

        use fparser,        only:   evalf, EvalErrMsg
        USE UR_Gspk1Fit,    only:   Gamspk1_Fit
        use UWB,            only:   gevalf
        use Rout,           only:   MessageShow, WTreeViewPutDoubleCell,WDNotebookSetCurrPage, &
                                    WTreeViewPutDoubleArray,WDSetCheckMenuItem,pending_events, &
                                    WDSetCheckButton,WDPutSelRadioMenu
        use Rw1,            only:   covppcalc
        use top,            only:   idpt,WrStatusbar,dpafact,FieldUpdate,chupper_eq,CharModA1,IntModA1, &
                                    RealModA1
        use Brandt,         only:   pnorm,qnorm,mtxchi,mean,sd
        use UR_gtk_globals, only:   consoleout_gtk
        use UWB,            only:   Resulta,upropa,RbtCalc,func_Fconst,func_Flinear
        use Num1,           only:   funcs
        use LF1,            only:   Linf
        use LF1G,           only:   Linfg1,Linfg1Ausf
        use CHF,            only:   FindLocT,ucase,testSymbol
        use file_io,        only:   logger
        use LSTfillT,       only:   WDListstoreFill_table
        use translation_module, only: T => get_translation
        use DECH,           only: Decaysub1
        use UR_DecChain,    only: DChain,DChainEGr
        use RW2,            only: kqt_find

        implicit none
        integer               :: i, k, ksav, klu
        integer               :: resp,ndd,itest
        real(rn)              :: dummy,omega
        real(rn)              :: UcombSV, XRD1, XRD2, XRB1, XRB2
        real(rn)              :: xkp, xkq
        real(rn)              :: akt, SDakt, PercsumSV
        real(rn)              :: xsav, F1x, F2x, dpi
        real(rn)              :: RD, chisqr_EG, MesswertSV_nvar, &
                                 MesswertSV_icnvar
        character(LEN=15)     :: verfahren
        character(LEN=200)    :: str1
        character(len=70)     :: cforma
        character(:),allocatable  :: ch1

        logical               :: tvar,found

        integer                  :: kzero1,kone1,imax,kfound
        real(rn)                 :: varw, fv1, fv2, dpa,   w
        real(rn), allocatable    :: Bmat(:,:), dvec(:), bvec(:), Uxinv(:,:)
        character(len=512)       :: log_str
        integer, allocatable     :: iact(:), iter(:)
        !-----------------------------------------------------------------------

        RW2_on = .true.
        Rnetmodi = .FALSE.
        kqtyp = 1  ! 9.6.2024
        klu = 0                       ! 2025.01.24 GK
        MesswertSV_nvar = ZERO        !  2025.01.24 GK
        MesswertSV_icnvar = ZERO      !

        if(FitDecay)  then
            write(log_str, '(*(g0))') '##################### Rechw2: ',Symbole(kEGr)%s,'  ',trim(fitmeth),'  ##################'
            call logger(66, log_str)
        end if

        if(.not.FitDecay)  then
            write(log_str, '(*(g0))') '##################### Rechw2: ',Symbole(kEGr)%s,'  ##################'
            call logger(66, log_str)
        end if
        if(consoleout_gtk) write(0,*) '##### Begin of Rechw2  ##########################'

        if(.not.loadingPro) then
            call WrStatusBar(4, T('Calculating') // '....' )
        end if

        if(allocated(CVFormel) .and. ubound(CVFormel,dim=1) == 0)   &
            call CharModA1(CVFormel,ubound(IsymbA,dim=1))
        allocate(character(len=200) :: ch1)

        write(log_str, '(a,i2,a,i2)') 'ncov=',ncov,'   ncovf=',ncovf
        call logger(66, log_str)
        do i=1,ncov
            if(size(CVFormel) < i) exit
            if(len_trim(CVFormel(i)%s) > 0)  then
                write(log_str, '(a,i3,a,a)') 'i=',i,'  CVFormel(i)=', CVFormel(i)%s
                call logger(66, log_str)
            end if
        end do
        fpaLYT = ZERO
        covarLYT = ZERO
        ableit_fitp = .false.

        call setupParser(1)

        if(ifehl == 1) goto 9000

        call logger(66, ' ')
        write(cforma,'(a,i2.2,a)') '(i3,2x,a,2x,a1,T',maxlen_symb+8,',2(2x,a,es15.8))'

        do i=1,ngrs+ncov+numd

            write(log_str, cforma) i,Symbole(i)%s,symtyp(i)%s,' Messwert=',real(Messwert(i),8),  &
                                   ' StdUnc=',real(StdUnc(i),8)

            call logger(66, log_str)

        end do

        if(.not. allocated(MesswertSVG)) then
            allocate(MesswertSVG,source=Messwert)
            MesswertSVG = ZERO
        end if
        if(.not. allocated(StdUncSVG)) then
            allocate(StdUncSVG,source=StdUnc)
            StdUncSVG = ZERO
        end if

        Resultat = Resulta(kEGr)

        if(MesswertSVG(1) <= -ONE .and. StdUncSVG(1) <= -ONE) then
            do i=1,ngrs+ncov+numd
                MEsswertSVG(i) = Messwert(i)
                if(i> ngrs .and. i <= ngrs+ncov) MesswertSVG(i) = covarval(i-ngrs)
                StdUncSVG(i) = StdUnc(i)
            end do
        end if

        write(log_str, '(a,3es12.4,a,i0,a,f6.3)') 'Begin of Rechw2: Result, Ucomb, coverf=', &
            resultat,Ucomb,coverf,' kEGr=',kEGr,'  coverin=',coverin
        call logger(66, log_str)

        if(FitDecay)  then
            write(log_str, '(a,i0)') 'klincall =',klincall
            call logger(66, log_str)
        end if
        ! Write this value again into the first row of the grid 'values, uncertainties' and
        ! also into the "Uncertaintey budget" schreiben:
        call WTreeViewPutDoubleCell('treeview2', 5, kEGr, resultat)

        !  Save several array to arrays with 'SV' attached to the array name:
        imax = ngrs+ncov+numd
        if(allocated(MesswertSV)) deallocate(MesswertSV,StdUncSV)
        allocate(MesswertSV(imax),StdUncSV(imax))
        if(allocated(sensiSV)) deallocate(sensiSV,percSV)
        allocate(sensiSV(imax),percSV(imax))
        if(allocated(StdUncSV1)) deallocate(StdUncSV1)
        allocate(StdUncSV1(imax))
        if(allocated(UcontribSV)) deallocate(UcontribSV)
        allocate(UcontribSV(imax))
        ! write(66,*) 'RW2:  ubound(sensi)=',int(ubound(sensi,dim=1),2),' imax=',int(imax,2)
        MesswertSV(1:imax) = Messwert(1:imax)
        StdUncSV(1:imax)   = StdUnc(1:imax)
        ! arrays SensiSV, Sensi, percSV and perc:  have ubound = 0 here

        !  Calculate the combined standard uncertainty:
        if(LinTest) then
            increase_dpafact = .true.     ! test for non-linearity
            call upropa(kEGr)
            UEG_increase = Ucomb*coverf
            increase_dpafact = .false.
        end if

        increase_dpafact = .false.

        if(DChain) call DChain_Adjust_SD()              ! 27.4.2025
        call upropa(kEGr)
        Ucomb = Ucomb * coverf
        if(LinTest) UEG_normal = Ucomb
        call WDListstoreFill_table('liststore_budget',3, .true.)
        UcombSV = Ucomb
        imax = ngrs+ncov+numd
        SensiSV(1:imax)    = Sensi(1:imax)
        percSV(1:imax)     = perc(1:imax)
        StdUncSV1(1:imax)  = StdUnc(1:imax)
        UcontribSV(1:imax) = Ucontrib(1:imax)
        MesswertSV(1:imax) = Messwert(1:imax)      ! 14.7.2023

        Ucomb_EGr = Ucomb     ! introduced 20.1.2025 GK
        PercsumSV = percsum

        ! Save covar values:
        if(allocated(CovarvalSV)) deallocate(CovarvalSV)
        allocate(CovarValSV(ncov))
        CovarvalSV(1:ncov) = Covarval(1:ncov)

        !-----------------------------------------------------------------------
        !  Test the gross count rate Rb in the equation for the net count rate,
        !  whether it appears to be multiplied with a factor /= 1 (called now FAKRB):

        ! Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)

        if(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit) then
            tvar = .true.
            if(kbrutto(kEGr) > 0 .and. kbrutto(kEGr) <= ngrs) then
                tvar = len_trim(sdformel(kbrutto(kEGr))%s) == 0
            end if
            if(.not.var_brutto_auto .and. (gum_restricted .or. tvar)) then
                FakRB = ONE
                kbrutto2 = 0
                kbrutto2_kbd = 0
                kbrutto_double = 0
                Fconst = ZERO
                Flinear = ONE
                goto 60
            end if
        end if

        XRB1 = ZERO
        XRB2 = ZERO
        XRD1 = ZERO
        XRD2 = ZERO
        if(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not. SumEval_fit) then
            XRD1 = Messwert(knetto(kEGr))
            XRB1 = Messwert(kbrutto(kEGr))
            XRB2 = 4.0_rn * XRB1
            xsav = Messwert(kbrutto(kEGr))
            Messwert(kbrutto(kEGr)) = XRB2
            iteration_on = .TRUE.
            XRD2 = Resulta(knetto(kEGr))
            iteration_on = .FALSE.
            Messwert(kbrutto(kEGr)) = xsav

            FakRB = (XRD2 - XRD1) / (XRB2 - XRB1)
        else
            FakRB = ONE
        END if

        write(log_str, '(*(g0,1x))') 'XRB1,XRB2, XRD1,XRD2=',sngl(XRB1),sngl(XRB2), sngl(XRD1),sngl(XRD2)
        call logger(66, log_str)

        write(log_str, '(*(g0))') 'FakRB = ',sngl(FakRB)
        call logger(66, log_str)

        write(log_str, '(*(g0))') 'nonPoissGrossCounts=',nonPoissGrossCounts,' gross_negative=',gross_negative
        call logger(66, log_str)
        if(abs(FakRB) < EPS1MIN) FakRB = ONE

        imax = ngrs+ncov+numd
        Messwert(1:imax) = MesswertSV(1:imax)
        StdUnc(1:imax)   = StdUncSV(1:imax)
        Sensi(1:imax)    = SensiSV(1:imax)
        perc(1:imax)     = percSV(1:imax)
        Ucontrib(1:imax) = UcontribSV(1:imax)

        !-----------------------------
        kbrutto2 = 0
        kbrutto2_kbd = 0

        !---------------
        kbrutto_double = 0
        if(kbrutto(kEGr) > 0 .and. ncov > 0 .and. .not.FitDecay .and. .not.Gamspk1_Fit &
            .and. knumEGr == 1) then
            do i=1,ngrs
                if(i == kbrutto(kEGr)) CYCLE
                if(ABS(Messwert(kbrutto(kEGr))-Messwert(i)) / Messwert(kbrutto(kEGr)) < 1.E-6_rn) then
                    if(ABS(StdUnc(kbrutto(kEGr))-StdUnc(i)) / StdUnc(kbrutto(kEGr)) < 1.E-6_rn) then
                        kbrutto_double = i
                        write(log_str, '(a,i0,3a,i0)') 'Symbol(',i,') = identified double of the gross count rate: ',symbole(i)%s,' kbrutto_double=',i
                        call logger(66, log_str)
                        EXIT
                    end if
                end if
            end do
        end if

        !---------------
        ! Estimate the constants Fconst and Flinear:
        ! introduce the variable Rnetmodi

        if(Gum_restricted) then
            Fconst = ZERO
            Flinear = ONE
            goto 60
        end if
        klu = klinf
        if(Gamspk1_Fit) klu = kgspk1
        if(kfitp(1) > 0) klu = kfitp(1)-1+kEGr

        Fconst = func_Fconst(Messwert,ngrs+ncov+numd)
        Flinear = func_Flinear(Messwert,ngrs+ncov+numd)
        write(log_str, '(*(g0))') 'Flinear from func_Flinear: ',sngl(Flinear),'  Fconst=',sngl(Fconst)
        call logger(66, log_str)
        iteration_on = .FALSE.
        Rnetmodi = .FALSE.

        ndd = nab+nmodf+nabf+ncovf
        write(log_str, '(*(g0))') 'Rseite(ndd+(kEGr-1)*2+1))=',Rseite(ndd+(kEGr-1)*2+1)%s
        call logger(66, log_str)
        write(log_str, '(*(g0))') 'Rseite(ndd+(kEGr-1)*2+2))=',Rseite(ndd+(kEGr-1)*2+2)%s
        call logger(66, log_str)

        kzero1 = 0
        kone1 = 0
        do i=1,100
            if(Rseite(ndd+(kEGr-1)*2+1)%s(i:i) == ' ') cycle
            if(Rseite(ndd+(kEGr-1)*2+1)%s(i:i) == '(') cycle
            if(Rseite(ndd+(kEGr-1)*2+1)%s(i:i) == '0') then
                kzero1 = i
                exit
            end if
        end do
        do i=1,100
            if(Rseite(ndd+(kEGr-1)*2+2)%s(i:i) == ' ') cycle
            if(Rseite(ndd+(kEGr-1)*2+2)%s(i:i) == '(') cycle
            if(Rseite(ndd+(kEGr-1)*2+2)%s(i:i) == '1') then
                kone1 = i
                exit
            end if
        end do

        if(Fconst < ZERO .and. nWpars > 0) then
            do i=1,nWpars
                if(Wpars(i) > ZERO .and. abs(abs(Wpars(i))-abs(Fconst)) < 1.E-10_rn ) then
                    WparsInd(i:nWpars-1) = WparsInd(i+1:nWpars)
                    Wpars(i:nWpars-1) = WPars(i+1:nWpars)
                    nWpars = nWpars - 1
                    exit
                end if
            end do
        end if

        ! Restore the values of the arrays Messwert and StdUnc:
        ! Messwert(1:ngrs) = MesswertSV(1:ngrs)
        ! StdUnc(1:ngrs) = StdUncSV(1:ngrs)
        Messwert(1:imax) = MesswertSV(1:imax)
        StdUnc(1:imax) = StdUncSV(1:imax)

        if(ABS(Flinear) <= 1.E-10_rn) then
            write(log_str, '(*(g0))') 'Flinear practically zero: Flinear=',sngl(Flinear)
            call logger(66, log_str)

            write(str1,*) T('Warning') // ": ", &
                          T('The output quantity does not depend on the selected net conting rate'), &
                          " (" // symbole(knetto(kEGr))%s // ")!", new_line('A'), &
                          " Factor Flinear=0! " // T('Please, correct!')

            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp, mtype=GTK_MESSAGE_WARNING)

            call WrStatusBar(4, T('Eliminate error(s) in equations!'))

            call WDNotebookSetCurrPage('notebook1',2)
            ifehl = 1

            call logger(66, 'RW2_392; Return because of ifehl=1')
            goto 9000
        end if
        if(Flinear < ZERO .and. kbrutto(kEGR) > 0) then
            if(len_trim(SDFormel(kbrutto(kEGr))%s) > 0 ) then

                write(str1,*) T('Warning') // ": ", &
                              T('The coefficient of sensivity of the selected net count rate symbol is negative!'), &
                              symbole(knetto(kEGr))%s, new_line('A'), &
                              T('Please, select the appropriate symbol!')
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp,mtype=GTK_MESSAGE_WARNING)

                call WrStatusBar(4, T('Select the correct net counting rate symbol!'))

                call WDNotebookSetCurrPage('notebook1',2)
                ifehl = 1
                goto 9000
            end if
        end if
        if(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit   &
                    .and. .not.DChain .and. kbrutto(kEGR) > 0) then                   ! 27.4.2025
            if(len_trim(SDFormel(kbrutto(kEGr))%s) > 0 ) then
                ksav = kbrutto(kEGr)
                xsav = Messwert(ksav)
                iteration_on = .TRUE.

                xRD1 = Messwert(ksav)
                xsav = Messwert(ksav)
                F1x = Resulta(kEGr)
                Messwert(ksav) = xsav
                XRD2 = xRD1 * 1.00005_rn
                Messwert(ksav) = XRD2
                F2x = Resulta(kEGr)
                Rnetmodi = .FALSE.
                iteration_on = .FALSE.
                Messwert(ksav) = xsav
                dpi = (F2X - F1X) / (XRD2 - XRD1)
                if(dpi < ZERO .and. .not. gross_negative .and. FakRB > ZERO) then

                    write(str1,*) T('Warning') // ": ",  T('The coefficient of sensivity of the selected gross count rate symbol is negative!'), &
                                  " (" // symbole(knetto(kEGr))%s // ")", new_line('A'), new_line('A'), &
                                  T('Please, select the appropriate symbol!')

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp,mtype=GTK_MESSAGE_WARNING)
                    call WrStatusBar(4, T('Select the correct gross counting rate symbol!'))

                    call WDNotebookSetCurrPage('notebook1',2)
                    ifehl = 1
                    goto 9000
                end if
            end if

        end if
        imax = ngrs+ncov+numd
        Messwert(1:imax) = MesswertSV(1:imax)
        StdUnc(1:imax)   = StdUncSV(1:imax)
        Sensi(1:imax)    = SensiSV(1:imax)
        perc(1:imax)     = percSV(1:imax)
        Ucontrib(1:imax) = UcontribSV(1:imax)

        if( .not.FitDecay .and. .not.Gamspk1_Fit .and. kbrutto(kEGR) > 0 .and. .not.gross_negative) then
            if(len_trim(SDFormel(kbrutto(kEGr))%s) > 0 .And. FakRb*Messwert(kbrutto(kEGr)) < Messwert(knetto(kEGr))  &
                .and. Fakrb > ZERO ) then

                write(str1,*) T('Warning') // ": ", &
                              T('The gross count rate is smaller then the net counting rate!'), new_line('A'), &
                              T('Please, correct or select other symbols!')

                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp, mtype=GTK_MESSAGE_WARNING)
                call WrStatusBar(4, T('Remove problem with gross/net counting rates!'))
                ifehl = 1
                goto 9000
            end if
        end if

        If(.not.Gamspk1_Fit) then
            RD = RnetVal(Messwert(kEGr))
            write(log_str, '(*(g0))') 'RnetVal:   value=',sngl(RD),' mw(kEGr)=',sngl(Messwert(kEGr)),' ifehl=',ifehl
            call logger(66, log_str)
            if(ifehl == 1) then

                write(log_str, '(*(g0))') 'RnetVal:   value=',sngl(RD),' mw(kEGr)=',sngl(Messwert(kEGr)),' ifehl=',ifehl
                call logger(66, log_str)
                goto 9000
            end if
        end if


        if(FitDecay .and. kPMLE == 1 .and. ifit(2) == 1 .and. mfrbg == 3) then
            dummy = 1.E+10_rn     !  17.6.2024
            if(fpa(2) < ZERO .and. fpa(mfrbg) > ZERO) then
                dummy = abs( abs(fpa(2)) - abs(fpa(mfrbg)) ) / abs(fpa(mfrbg))
            end if
            if(fpa(2) > ZERO .and. fpa(mfrbg) < ZERO) then
                dummy = abs( abs(fpa(2)) - abs(fpa(mfrbg)) ) / abs(fpa(mfrbg))
            end if
            if(dummy > ZERO .and. dummy < 3.E-03_rn) then

                write(str1,*) T('Warning') // ": ", &
                              T('The 2nd Fit parameter apparently competes against the third!'), &
                              new_line('A'), &
                              T("Try to set the fitting option of the 2nd parameter to 'omit'!")
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp,mtype=GTK_MESSAGE_WARNING)
                call WrStatusBar(4, T('Remove problem with gross/net counting rates!'))

                ifehl = 1
                goto 9000
            end if

        end if

        !----------------------------------------------------------------------
60      continue
        Rnetmodi = .false.
        Resultat = Resulta(kEgr)

        if(.not.FitDecay) then
            ! this restriction: 15.7.2023
            call upropa(kEGr)
            Ucomb = Ucomb * coverf
        end if

        if(var_brutto_auto) Ucomb_anf = Ucomb/coverf

        write(log_str, '(*(g0))') 'Resultat=',sngl(Resultat),' Ucomb=',sngl(Ucomb)
        call logger(66, log_str)

        if(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not.FitCalCurve .and. &
                 .not.DChain .and. &                            ! 27.4.2025
                 .not.SumEval_fit .and..not.Gum_restricted) then

            write(log_str, '(4(a,es12.5))') 'RW2_5241: mw(knetto)=',Messwert(knetto(kEGr)),' mw(kbrutto)=',Messwert(kbrutto(kEGr)), &
                ' stdUnc(knetto)=',StdUnc(knetto(kEGr)),' StdUnc(kbrutto)=',StdUnc(kbrutto(kEGr))
            call logger(66, log_str)

            var_rbtot = StdUnc(knetto(kEGr))**TWO - FakRB**TWO * StdUnc(kbrutto(kEGr))**TWO

            write(log_str, '(*(g0))') 'var_rbtot=',sngl(var_rbtot),'  nvars_in_rbtot=',nvars_in_rbtot
            call logger(66, log_str)
            if(.true. .or. nvars_in_rbtot == 0) then
                do i=knetto(kEGr) + 1,ngrs
                    call FindSymb(knetto(kEGr),i,found, kfound)
                    if(i == iptr_cnt(kbrutto(kEGr))) cycle
                    if(found .and. i /= kbrutto(kEGr)) then
                    end if
                    if(ucase(symtyp(i)%s) == 'M') then
                        if(found .and. i /= kbrutto(kEGr)) then
                            nvars_in_rbtot = nvars_in_rbtot + 1
                            if(allocated(vars_rbtot)) deallocate(vars_rbtot)
                            allocate(vars_rbtot(1))
                            vars_rbtot(1) = i
                            if(nvars_in_rbtot > 1) call IntModA1(vars_rbtot,nvars_in_rbtot)
                            vars_rbtot(nvars_in_rbtot) = i
                        end if
                    end if
                end do
            end if
        end if

        if(.false. .and. .not.FitDecay .and. .not.Gamspk1_Fit .and. .not.FitCalCurve .and..not.Gum_restricted) then
            if(kbrutto(kEGr) > 0) then
                if(len_trim(sdformel(kbrutto(kEGr))%s) > 0) then
                    write(log_str, '(a,a,T50,a)') 'sdformel=',sdformel(kbrutto(kEGr))%s,trim(fname)
                    call logger(64, log_str)
                end if
            end if
        end if

        call WTreeViewPutDoubleArray('treeview2',5,ngrs+ncov+numd ,Messwert)
        call WTreeViewPutDoubleArray('treeview2',11,ngrs+ncov+numd ,StdUnc)
        call logger(66, '  ')

        if(ngrs+ncov+numd > ubound(Messwert_CP,dim=1)) then
            call RealModA1(Messwert_CP,ngrs+ncov+numd,1)
            call RealModA1(StdUnc_CP,ngrs+ncov+numd,1)
        end if
        Messwert_CP(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)
        StdUnc_CP(1:ngrs+ncov+numd) = StdUnc(1:ngrs+ncov+numd)

        call logger(66, ' ')

        write(log_str, '(*(g0))') '****************  at this point the results exist for ' &
            // 'output quantity and its associated uncertainty *******'
        call logger(66, log_str)
        call logger(66, ' ')

        if(.false. .and. use_WTLS) then
            call logger(23, ' ')
            write(log_str, '(*(g0))') 'fname=',trim(fname)
            call logger(23, log_str)
            write(log_str, '(*(g0))') '****************  at this point the results exist for ' &
                // 'output quantity and its associated uncertainty *******'
            call logger(23, log_str)
            call logger(23, ' ')
        end if

        if(GamDist_zr) then
            if(iptr_cnt(kbrutto(kEGr)) > 0) &

                write(log_str, '(*(g0))') 'iptr_cnt(kbrutto(kEGr))=',int(iptr_cnt(kbrutto(kEGr)),2), &
                ' iptr_time(iptr_cnt(kbrutto(kEGr)))=', &
                int(iptr_time(iptr_cnt(kbrutto(kEGr))),2)
                call logger(66, log_str)

            if(iptr_cnt(kbrutto(kEGr)) > 0) tgross = Messwert(iptr_time( iptr_cnt(kbrutto(kEGr))))
            do i=nab+1,ngrs
                if(i == iptr_cnt(kbrutto(kEGr))) cycle
                if(iptr_time(i) > 0) tback = Messwert(iptr_time(i))
            end do
            write(log_str, '(*(g0))') 'RW2:  tgross=',sngl(tgross),'  tback=',sngl(tback)
            call logger(66, log_str)
        end if

        if(FitDecay .and. ifit(2) < 3) then
            write(log_str, '(*(g0))') 'before Bayes: Resultat, Ucomb=',sngl(resultat),' ',sngl(Ucomb), &
                '   Corr(1,2)=',sngl(covar(1,2)/sqrt(covar(1,1)*covar(2,2))),' cov(1,2)=',sngl(covar(1,2))
            call logger(66, log_str)
        else
            write(log_str, '(*(g0))') 'before Bayes: Resultat, Ucomb=',sngl(resultat),' ',sngl(Ucomb)
            call logger(66, log_str)
        end if

        !  Calculate the values concerning the best Bayesian estimate:
        if(kModelType /= 2) then
            omega =  pnorm(Resultat/(ucomb/coverf))
            WertBayes = resultat + ( (ucomb/coverf) *   &
                EXP( max(-450._rn, -resultat**TWO/(TWO*(ucomb/coverf)**TWO) ) ) / (omega*SQRT(TWO*PI) ) )

            UcombBayes = SQRT( (ucomb/coverf)**TWO - (WertBayes - Resultat)*WertBayes )
            UcombBayes = UcombBayes * coverf

            xkp = qnorm(omega*(ONE - (ONE-W1minusg)/TWO))
            xkq = qnorm(ONE - omega*(ONE-W1minusg)/TWO)
            KBgrenzu = Resultat - xkp * (ucomb/coverf)
            KBgrenzo = Resultat + xkq * (ucomb/coverf)
            write(log_str, '(*(g0))') 'omega=',sngl(omega),' WertBayes=',sngl(WertBayes),' UcombBayes=',sngl(UcombBayes)
            call logger(66, log_str)


            ! Shortest coverage interval:
            xkp = qnorm(0.5_rn*(ONE + omega*W1minusg))
            xkq = qnorm(ONE - omega*(ONE-W1minusg))
            KBgrenzuSH = Resultat - xkp * (ucomb/coverf)
            KBgrenzoSH = Resultat + xkp * (ucomb/coverf)
            if(.not. Gum_restricted .and. KBgrenzuSH < ZERO) then
                KBgrenzuSH = max(ZERO, KBgrenzuSH)
                KBgrenzoSH = Resultat + xkq * (ucomb/coverf)
            end if

            call logger(66, 'coverage intervals:     shortest              symmetric')
            write(log_str, '(a,es12.5,10x,es12.5)') '         lower limit : ', &
                real(KBgrenzuSH,8),real(KBgrenzu,8)
            call logger(66, log_str)
            write(log_str, '(a,es12.5,10x,es12.5)') '         upper limit : ', &
                real(KBgrenzoSH,8),real(KBgrenzo,8)
            call logger(66, log_str)
            write(log_str, '(a,es12.5,10x,es12.5)') '         interval    : ', &
                real(KBgrenzoSH-KBgrenzuSH,8),real(KBgrenzo-KBgrenzu,8)
            call logger(66, log_str)
        else
            WertBayes = ZERO
            UcombBayes = ZERO
            KBgrenzu = Resultat + qnorm( (ONE - W1minusG)/TWO ) * (ucomb/coverf)
            KBgrenzo = Resultat + qnorm( (ONE + W1minusG)/TWO ) * (ucomb/coverf)
            write(log_str, '(*(g0))') 'KBgrenzu, KBgrenzo=',sngl(KBgrenzu), ' ',sngl(KBgrenzo)
            call logger(66, log_str)
        end if

        if(.not.loadingPro .and. .not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit .and.  &
            ! kbrutto_gl(kEGr) == 0 .and. kModelType == 1 .and. .not.var_brutto_auto ) then
            kbrutto_gl(kEGr) == 0 .and. kModelType == 1 .and. .not.var_brutto_auto .and.  &  !   27.7.2025 GK
                                                                               .not.DChain ) then          !
            kmodelOld = kModelType
            Gum_restricted = .true.
            gross_negative = .false.
            kModelType = 2
            if(kmodelold /= kModelType) then
                SaveP = .true.
                call FieldUpdate()
            end if
            call WDPutSelRadioMenu('MT_NegLin',2)
            if(.not. loadingPro) call pending_events()
        end if

        write(log_str, '(*(g0))') 'Rw2:  gum_restricted=', Gum_restricted,'   multi_eval=',multi_eval
        call logger(66, log_str)

        chisqr_EG = ZERO   ! 2025.01.24 GK

        if(.not.gum_restricted .and. .not.multi_eval) then

            call logger(30, ' ')                      ! 14.10.2025 GK
            call logger(30, "Project:  " // trim(fname))

            if(FitDecay)  then
                write(log_str, '(*(g0))') 'Begin of Detlim calculations    Fitmeth=',TRIM(fitmeth)
                call logger(66, log_str)
            end if
            if(FitDecay)  then
                write(log_str, '(*(g0))') 'Begin of Detlim calculations    Fitmeth=',TRIM(fitmeth)
                call logger(30, log_str)
            end if

            call pending_events()                                                         !

            !  Restoring arrays:
            imax = ngrs+ncov+numd
            Messwert(1:imax) = MesswertSV(1:imax)
            StdUnc(1:imax)   = StdUncSV(1:imax)
            Sensi(1:imax)    = SensiSV(1:imax)
            perc(1:imax)     = percSV(1:imax)

            if(FitDecay) then
                !   see also Detlim_iter!
                fpaLYT(1,1:ma) = fpa(1:ma)
                sfpaLYT(1,1:ma) = sfpa(1:ma)
                if(knumEGr > 1) covarLYT(1) = covar(1,2)
                chisqr_EG = chisqr
            end if

        else
            goto 90
        end if
!--------------------------------------------------------------------

        write(log_str, '(1X,A,i0)') 'iterative calculation of decision and detection limit: output quantity:',kEGr
        call logger(30, log_str)

        if(kbrutto_gl(kEGr) == 0 .and. .not.var_brutto_auto &           ! 27.4.2025
            .and. .not.DChain .and. .not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit) then
            if(.NOT.loadingPro) then

                str1 = T('Warning') // ": " // &
                       T('The StdDev formula of the gross count rate has not yet been defined.') // &
                       new_line('A') // &
                       T('Calculation of detection limits not possible!')
                call MessageShow(trim(str1), GTK_BUTTONS_OK, &
                                 "Rechw2:", resp, mtype=GTK_MESSAGE_WARNING)
            end if
            goto 9000
        END if

        call RbtCalc(RblTot)
        StdUnc(1:ngrs+ncov+numd) = StdUncSV(1:ngrs+ncov+numd)

        write(log_str, '(*(g0))') 'RblTot(kEgr)=',sngl(RblTot(kEGr)),'   FakRB=',sngl(FakRB)
        call logger(66, log_str)
        if(kbrutto(kEGr) > 0) then
            write(log_str, '(*(g0))') '       Messwert(kbrutto(kEGr))=',sngl(Messwert(kbrutto(kEGr))), &
                '  Stdunc=',sngl(StdUnc(kbrutto(kEGr)))
            call logger(66, log_str)
        end if
        if(knetto(kEGr) > 0) then
            write(log_str, '(*(g0))') '       Messwert(knetto(kEGr)) =',sngl(Messwert(knetto(kEGr))), &
                '  Stdunc=',sngl(StdUnc(knetto(kEGr)))
            call logger(66, log_str)
        end if

        !-------------------------------------------
        ! Calculate the relative standard uncertainty of the calibration factor w:
                ! if(Fconst /= zero) write(169,*) 'Fconst /= 0: File=',trim(fname)
        ! 13.9.2023:
        ! determine uncertainty of Flinear:

        if(DChainEGr) then               ! If construct reorganized   27.4.2025
            urelw = 0._rn
            uFlinear = 0._rn
        else
            Fv1 = func_Flinear(Messwert,ngrs)
                   w = Fv1    ! added on 28.7.2025 GK
            varw = ZERO
            do k=1,nRSsy(kEGr)
                i = RS_SymbolNr(kEGr,k)
                if(abs(StdUnc(i)) < 1.e-12_rn .or. abs(StdUnc(i)-missingval) < 1.e-12_rn) cycle
                dpa = Messwert(i)*dpafact(Messwert(i)) - Messwert(i)
                Messwert(i) = Messwert(i) + dpa
                Fv2 = func_Flinear(Messwert,ngrs)
                Messwert(i) = Messwert(i) - dpa
                dpi = (Fv2 - Fv1)/dpa
                if(abs(dpi) > ZERO) varw = varw + (dpi*StdUnc(i))**TWO
            end do
            urelw = sqrt(varw)/Fv1
            ! write(66,*)  'Calculated urelw: ',sngl(urelw)
            if(urelw > ZERO .and. abs(uFlinear) < EPS1MIN) uFlinear = urelw*Flinear
        END IF

        write(log_str, '(3(a,es11.4))') 'w=',Fv1,' urel(w)=',urelw, &
            ' StdUnc(kEGr)=',StdUnc(kEGr)
        call logger(66, log_str)

        if(.not.use_WTLS .and. urelw >= ONE/kbeta*0.98_rn .and. .not.bat_serial .and. .not.batf) then

            str1 = T('Warning') // ": " // new_line('A') // &
                   T('The relative calibration factor w uncertainty is >= 1/k_1-beta!') // &
                   new_line('A') // &
                   T('Therefore, the detection limit cannot be calculated!')

            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp, mtype=GTK_MESSAGE_WARNING)
        end if
        !-------------------------------------------


        if(FitDecay)  then
            write(log_str, '(*(g0))') ' Rechw2, before DecThresh calculation: klincall=',klincall
            call logger(66, log_str)
        end if

        if(FitDecay)  then
            write(log_str, '(*(g0))') ' Rechw2, before DecThresh calculation:  MW(klu)=',sngl(Messwert(klu)), &
            ' StdUnc(klu)=',sngl(StdUnc(klu))
            call logger(30, log_str)
        end if

        write(log_str, '(a,i0,2(a,L1),a,i0)') 'before Detlim_iter: kEGr=',kEGr,'  Gum_restricted=',GUM_restricted, &
            ' var_brutto_auto=',var_brutto_auto,' k_autoform=',k_autoform
        call logger(66, log_str)

        do itest=1,2
            if(.not.LinTest .and. itest == 2) cycle

            decthresh = ZERO
            detlim = ZERO

            verfahren = 'ISO 11929:2019'

            call logger(30, 'procedure : ' // verfahren)
            dummy = ZERO

            ! Decision threshold:
            call logger(66, '-- Begin of iteration DT')
            increase_dpafact = .false.
            if(LinTest) then
                if(itest == 1) increase_dpafact = .true.
                if(itest == 2) increase_dpafact = .false.
            end if

            nvar = kbrutto(kEGr)
            MesswertSV_nvar = ZERO
            MesswertSV_icnvar = ZERO

            if(.not.Gum_restricted) then
                if(nvar > 0) then
                    MesswertSV_nvar = MesswertSV(nvar)
                    if(iptr_cnt(nvar) > 0) MesswertSV_icnvar = MesswertSV(iptr_cnt(nvar))
                end if
                if(ksumEval > 0) then
                    if(nvar == 0) nvar = ksumEval
                    MesswertSV_nvar = MesswertSV(ksumEval)
                    if(iptr_cnt(nvar) > 0) MesswertSV_icnvar = MesswertSV(iptr_cnt(nvar))
                end if
            end if

            limit_typ = 1
            kqtyp = 2  ! 9.6.2024
            call detlim_iter(dummy,decthresh,nit_decl)
            call logger(30, ' ')
            if(LinTest .and. itest == 1) DT_increase = decthresh

            ! write(66,'(a,es12.5,a,i0)') 'after DT:  DT=',decthresh,' ifehl=',ifehl
            if(ifehl == 1) goto 20

            if(FitDecay)  then
                write(log_str, '(a,i0,a,es12.5,a,3es13.5)') ' Rechw2, after DT calculation: klincall=',klincall, &
                ' EKG=',decthresh,' fpa=',(sngl(fpa(i)),i=1,ma)
                call logger(66, log_str)
            end if
            if(.not.use_WTLS .and. urelw >= ONE/kbeta) then         ! 11.7.2023
                detlim = ZERO
                nit_detl = 0
            else
                ! Detection limit:
                write(log_str, '(a,es12.5)') '-- Begin of iteration DL:   decthresh=',sngl(decthresh)
                call logger(66, log_str)
                Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)
                StdUnc(1:ngrs+ncov+numd)   = StdUncSV1(1:ngrs+ncov+numd)

                increase_dpafact = .false.
                if(LinTest) then
                    if(itest == 1) increase_dpafact = .true.
                    if(itest == 2) increase_dpafact = .false.
                end if

                if(nvar > 0) MesswertSV(nvar) = MesswertSV_nvar

                limit_typ = 2
                kqtyp = 3  ! 9.6.2024
                call detlim_iter(decthresh,detlim,nit_detl)

                if(nvar > 0) MesswertSV(nvar) = MesswertSV_nvar
                if(ifehl == 1) goto 20
                if(LinTest .and. itest == 1) DL_increase = detlim

                write(log_str, '(a,es12.5,a,i0)') 'directly after DL:  detlim=',detlim,' ifehl=',ifehl
                call logger(66, log_str)

                if(FitDecay)  then
                    write(log_str, '(a,i0,a,es12.5,a,3es13.5)') ' Rechw2, after DL calculation: klincall=',klincall, &
                    ' DL=',detlim,' fpa=',(sngl(fpa(i)),i=1,ma)
                    call logger(66, log_str)
                end if

                write(log_str, '(1x,a,es11.4,2x,a,3x,i2,1x,a)') 'Decision thresh. = ', &
                    real(decthresh,8),'Bq/cm2',nit_decl,'Iterations'
                call logger(30, log_str)
                write(log_str, '(1x,a,es11.4,2x,a,3x,i2,1x,a)') 'Decision thresh. = ', &
                    real(decthresh,8),'Bq/cm2',nit_decl,'Iterations'
                call logger(66, log_str)

                write(log_str, '(  1x,a,es11.4,2x,a,3x,i2,1x,a)') 'Detection limit  = ', &
                    real(detlim,8),'Bq/cm2',nit_detl,'Iterations'
                call logger(30, log_str)
                write(log_str, '(  1x,a,es11.4,2x,a,3x,i2,1x,a)') 'Detection limit  = ', &
                    real(detlim,8),'Bq/cm2',nit_detl,'Iterations'
                call logger(66, log_str)
            end if

        end do     ! itest

20      continue
        limit_typ = 0

        iteration_on = .FALSE.
        kqtyp = 1  ! 9.6.2024
        if(ifehl == 1)  then
            write(log_str, '(*(g0))') 'RW2: after 20: ifehl = 1 !'
            call logger(66, log_str)
        end if
        if(ifehl == 1) then
            call WrStatusBar(4, T('Abortion!'))
            call pending_events()
            call pending_events()
        end if
        if(ifehl == 1) goto 9000

        if(LinTest) then
            write(str1,*) sngl(UEG_increase/UEG_normal),' ; ',sngl(DT_increase/decthresh),' ; ', &
                sngl(DL_increase/detlim),' ; ',kEGr,' ; ',trim(fname)
            do i=1,len_trim(str1)
                if(str1(i:i) == '.') str1(i:i) = ','
            end do
            call logger(159, trim(str1))
        end if

        if(FitDecay) then
            write(log_str, '(*(g0))') 'Limit_typ: ',limit_typ,'  fpa=',(sngl(fpa(i)),i=1,3),'   sfpa=',(sngl(sfpa(i)),i=1,3)
            call logger(66, log_str)
            do i=1,3
                write(log_str, '(a,i1,2(a,1x,3es11.4,2x),2x,a,es11.4)') 'kqtyp=',i,'  fpaLYT=',(sngl(fpaLYT(i,k)),k=1,3), &
                    '  sfpaLYT=',(sngl(sfpaLYT(i,k)),k=1,3),' covarLYT(i)=',sngl(covarLYT(i))
                call logger(66, log_str)
            end do
            chisqr = chisqr_EG
        end if

90      CONTINUE
!-----------------------------------------------------------------------
        if(nvar > 0 .and. .not.Gum_restricted) then
            MesswertSV(nvar) = MesswertSV_nvar     ! added 10.12.2020, because modvar modfies the *SV(nvar) values
            if(iptr_cnt(nvar) > 0) MesswertSV(iptr_cnt(nvar)) = MesswertSV_icnvar
        end if

!  Restoring:
        Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)
        StdUnc(1:ngrs+ncov+numd)   = StdUncSV1(1:ngrs+ncov+numd)
        Sensi(1:ngrs+ncov+numd)    = SensiSV(1:ngrs+ncov+numd)
        perc(1:ngrs+ncov+numd)     = percSV(1:ngrs+ncov+numd)
        Ucontrib(1:ngrs+ncov+numd) = UcontribSV(1:ngrs+ncov+numd)

        covarval(1:ncov) = covarvalSV(1:ncov)
        ucomb = UcombSV
        percsum = percsumSV

        if(Fitdecay) call covppcalc(1)
        if(FitDecay)  then
            write(log_str, '(*(g0))') ' Rechw2: shortly before its end:  klincall=',int(klincall,2),' ifehl=',int(ifehl,2)
            call logger(66, log_str)
        end if

        if(FitDecay) then
            iterat_passed = .TRUE.        ! ensures that an additional uncertainty calculations
            ! is recalculated in Lincov2!
            klincall = 0                  ! without klincall = 0 the following did not work
            if(ifehl == 1) goto 9000
            iterat_passed = .FALSE.
        end if

        SDakt = ZERO
        if(Gamspk1_Fit) call Linfg1ausf(1,akt,SDakt)    ! restore activity value

        write(log_str, '(a,i0,3(a,es11.4),a,i0)') 'End of Rechw2: kEGr=',kEGr,' Resultat=',resultat,  &
            ' ucomb=',ucomb,' SDakt=',SDakt,' ifehl=',int(ifehl,2)
        call logger(66, log_str)
        if(FitDecay)  then
            write(log_str, '(*(g0,1x))') 'End of Rechw2: fpa=',(sngl(fpa(i)),i=1,3)
            call logger(66, log_str)
        end if
        write(log_str, '(*(g0))') 'End of Rechw2: Resultat=',sngl(resultat),'  ucomb=',sngl(ucomb), &
            ' ifehl=',int(ifehl,2)
        call logger(30, log_str)

        if(FitDecay) then
            write(log_str, '(*(g0))') 'Cauchy-Schwarz-Test for covpp    : ',cauchy_failed1
            call logger(66, log_str)
            write(log_str, '(*(g0))') 'Cauchy-Schwarz-Test for Findcovx : ',cauchy_failed2
            call logger(66, log_str)
            write(log_str, '(*(g0))') 'Cauchy-Schwarz-Test at  E7       : ',cauchy_failed3
            call logger(66, log_str)
        end if

9000    continue

        write(log_str, '(a,5(i0,1x))') 'kbgv_binom,itm_binom,ip_binom,ilam_binom=',kbgv_binom,itm_binom,ip_binom,ilam_binom
        call logger(66, log_str)
        write(log_str, '(*(g0))') 'use_bipoi=',use_bipoi
        call logger(66, log_str)
        if(allocated(Bmat)) deallocate(Bmat,dvec,bvec,Uxinv)
        if(allocated(Uxinv)) deallocate(Uxinv)
        if(allocated(iact)) deallocate(iact)
        if(allocated(iter)) deallocate(iter)

        call logger(66, '############################# Rechw2 End ############################')
        if(consoleout_gtk) write(0,*) '##### Rechw2 End ############################'


        ! Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)
        ! iteration_on = .true.
        ! limit_typ = 2
        ! write(66,*)


    end subroutine Rechw2

!#######################################################################

    module subroutine detlim_iter(DTxx, newvalue, it)

        !  calculates the values of the decision thteshold (DT) and the
        !  detection limit(DL) according to ISO 11929:2019;
        !  The limit_typ variable is set before calling detlim_iter
        !
        !     Copyright (C) 2014-2025  Günter Kanisch

        USE UR_Gleich_globals, only: klinf, kEGr, kgspk1, knetto, ifehl, nvar, kbrutto, ksumeval, &
                                     Messwert, STDUnc, Ucomb, nab, ngrs, missingval, symbole, &
                                     Ucomb_DTv, Ucomb_DLv, knumEGr, ncov, symboleG, kpoint, &
                                     MesswertSV, StdUncSV
        USE UR_Linft,          only: kfitp, FitDecay, ifit, fpaSV, SumEval_fit, klincall, covar, &
                                     kPMLE, numd, fpa, sfpa, covarLYT, SDnetrate, k_rbl, fpaLYT, &
                                     ma, sfpaLYT, dnetrate, numd, d0zrate, dmesszeit, sd0zrate

        use Lf1,               only: Linfout
        USE fparser,           only: evalf, EvalErrMsg
        USE UR_DLIM,           only: limit_typ, kalpha, kbeta, Fconst, uFc, ffx, iteration_on, &
                                     Flinear, detlim, DCEGr, nit_detl_max
        USE UR_Gspk1Fit,       only: Gamspk1_Fit

        use gtk,               only: gtk_buttons_ok, GTK_MESSAGE_WARNING
        use Rout,              only: MessageShow
        use Top,               only: WrStatusbar
        use UWB,               only: Resulta,upropa

        use file_io,           only: logger
        use UR_MCC,            only: kqtypx
        use translation_module, only: T => get_translation
        use UR_DecChain,       only: DChain

        implicit none

        real(rn), intent(in)        :: DTxx      ! needed only for limit_typ = 2 (DL iteration)
        real(rn), intent(out)       :: newvalue  ! calculated value of DT or DL
        integer, intent(out)        :: it        ! number of iterations

        real(rn)            :: RD
        real(rn)            :: x1,x2,xacc
        real(rn)            :: oldvalue,RD_old
        real(rn)            :: fpaSVur(3)
        real(rn)            :: xcorr,maxrelu
        real(rn)            :: ratmin, ratmin2, varFL, brentx
        integer             :: i,ifitDL(3),klu,resp,ism,mode
        character(len=6)    :: vname
        character(len=512)  :: log_str
        character(len=256)  :: str1
        !-----------------------------------------------------------------------

        klu = klinf
        if(klinf == 0) klu = knetto(kEGr)
        if(Gamspk1_Fit) klu = kgspk1
        if(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1

        oldvalue = ZERO    ! 2025.01.24 GK

        if(FitDecay) ifitDL = ifit

        ifehl = 0
        if(FitDecay) fpaSVur = fpaSV     ! extended to 3 parameters

        nvar = kbrutto(kEGr)
        if(SumEval_fit) nvar = ksumeval
        it = 0
        !  Determine starting values for the iteration procedure:
        select case (limit_typ)
          case (1)
            ! DT / EKG:
            vname = 'DecThr'
          case (2)
            ! DL / NWG:
            vname = 'DetLim'
            oldvalue = (kalpha+kbeta)/kalpha * DTxx  ! for detection limit
            newvalue = oldvalue
            if(knetto(kEGr) > 0) then
                if(abs(Messwert(kEgr)-Fconst) <= EPS1MIN) then
                    varFL = 1.E-20_rn
                else
                    varFL = (StdUnc(kEGr)**TWO + uFC**TWO)/(Messwert(kEGr) - Fconst)**TWO  &
                        + (StdUnc(knetto(kEGr))/Messwert(knetto(kEGr)))**TWO
                    varFL = min(varFL, 0.5_rn)
                end if
                newvalue = DTxx * ((kalpha+kbeta)/kalpha)**(ONE + sqrt(varFL))
            else
                newvalue = newvalue * TWO
            end if
        end select

        if(limit_typ == 1) then
            ! values (close to zero) of the net count rate:
            !newvalue = 1.E-11_rn     ! 1.E-11_rn
            !  newvalue = 5.E-12_rn      ! 5.11.2020
            newvalue = ZERO       ! 13.2.2023
            RD = newvalue
            if(DChain) ffx = 1.E-12_rn               ! 27.4.2025
        end if

        iteration_on = .true.

        if(DCHain) call DChain_Adjust_SD()           ! 27.4.2025
        !------------------------------------------------------------------------
        !::::: iteration loop:
        !  for decision threshold :  decthresh = k-alpha * u(decthresh, RD=declim/Kalfactor)
        !  for detection limit:  detlim = decthresh + k-beta * u(detlim, RD=decthresh/Kalfactor)
        !  (RD: net counting rate of analyte)

        !  write(30,'(4(a,es12.5))') 'Begin of iterations: RD=',RD,' Fconst=',Fconst,' Flinear=',Flinear,' DTxx=',DTxx
        write(log_str, '(4(a,es12.5))') 'Begin of iterations: RD=',RD,' Fconst=',Fconst,' Flinear=',Flinear,' DTxx=',DTxx
        call logger(30, log_str)

        if(.true. .and. limit_typ == 2) then   ! replacing .true. by .false. would mean that the DL
            ! iteration is shifted from brentx to the following do loop
            !
            !DL / NWG: iterative search with brentx
            kqtypx = 3
            x1 = DTxx*0.9_rn          ! x1, x2: bracketing values, safely encompassing the detlim value
            x2 = newvalue*3.0_rn      !
            xacc = x1*1.5_rn*1.E-8_rn
            ! if(use_WTLS)  xacc = xacc * 10._rn       ! 4.7.2023
            xacc = xacc * 5._rn
            mode = 1  !  mode = 1:  this value ist interpreted in the subroutine PrFunc called by brentx
            detlim = brentx(x1, x2, xacc, DTxx, mode)
            if(ifehl == 1) then
                call logger(30, 'Detlim_iter: Error within brentx! ')
                call logger(66, 'Detlim_iter: Error within brentx! ')
                return
            end if
            goto 44    ! i.e., the following do loop is skipped
        end if

        do             ! Begin iteration loop (rather long)

            x1 = ZERO
            x2 = ZERO

            RD_old = RD
            if(.not.DChain) then             ! if construct extended    ! 27.4.2025
                RD = RnetVal(newvalue)
            else
                ffx = newvalue*DCEGr(kEGr)   ! ffx: see above!
            end if
            if(klu > 0) then
                MEsswert(klu) = RD
            end if

            ! Procedure: ISO 11929: set the "assumed value" as net count rate RD, and its uncertainty
            if(limit_typ == 1) then
                ! DT / EKG: (u(RD=0) !)
                call ModVar(2, RD, ffx)
            else
                ! NWG /DL
                ! actually, the iteration for the DL is done in brentx (see a bit above)
                call ModVar(3, RD, ffx)
            END if
            if(nvar > 0) then
                write(log_str, '(*(g0))') 'nach modvar: MW(nvar)=',sngl(Messwert(nvar)),'  StdUnc(nvar)=',sngl(StdUnc(nvar))
                call logger(30, log_str)
            end if

            !  calculate the uncertainty of that output quantity value corresponding to RD:
            if(it == 0) klincall = 0    ! required by Linf/lincov2 for FitDecay
            call upropa(kEGr)        ! berechnet UComb
            if(nvar > 0)  then
                write(log_str, '(*(g0))') 'Loop:  RD=',sngl(RD),' Ucomb=',sngl(Ucomb),'  it=',int(it,2),' klincall=',int(klincall,2), &
                ' MW(nvar)=',sngl(Messwert(nvar))
                call logger(30, log_str)
            end if
            if(nvar == 0)  then
                write(log_str, '(*(g0))') 'Loop:  RD=',sngl(RD),' Ucomb=',sngl(Ucomb),'  it=',int(it,2),' klincall=',int(klincall,2)
                call logger(30, log_str)
            end if

            if(.true. .and. it > 2 .and. limit_typ == 2 .and. abs(uFC/Flinear) >= 0.97_rn/kbeta) then
                ifehl = 1
                write(str1, *) T('Warning') // ": ", &
                               T('The uncertainty is too large, the DetLim iteration will thus fail!'), &
                               new_line('A'), &
                               T('Please, check uncertainties of input quantities!')


                call MessageShow(trim(str1), GTK_BUTTONS_OK, 'DetLim_Iter:', resp,mtype=GTK_MESSAGE_WARNING)
                maxrelu = ZERO
                ism = 0
                do i=nab+1,ngrs
                    if(FitDecay .and. kfitp(1)> 0 .and. i >= kfitp(1) .and. i <= kfitp(1)+2 ) cycle
                    if(abs(StdUnc(i)-missingval) > EPS1MIN .and. abs(Messwert(i)-ZERO) > EPS1MIN .and.  &
                        StdUnc(i)/Messwert(i) > maxrelu) then
                        maxrelu = StdUnc(i)/Messwert(i)
                        ism = i
                    end if
                end do

                write(str1,*) T('Max. value of relative input quantities:'), &
                              new_line('A'), &
                              symbole(ism)%s // ": ", &
                              T('rel. uncertainty') // "= ", sngl(maxrelu)

                call MessageShow(trim(str1), GTK_BUTTONS_OK, "DetLim_Iter:", resp,mtype=GTK_MESSAGE_WARNING)
                goto 1500
            end if

            select case (limit_typ)
              case (1)
                newvalue = kalpha*ucomb               ! decision limit
              case (2)
                newvalue = DTxx + kbeta*ucomb      ! detection limit
            end select
            it = it + 1    ! one iteration done

            if(nvar > 0) then
                write(log_str, 67) it,vname,real(newvalue,8), real(Messwert(nvar),8),real(RD,8),  &
                    real(Messwert(kEGr),8),real(ucomb,8)
                call logger(30, log_str)
67              format(5x,' Iteration=',i3,':   ',a,'= ',es16.9,'  Rb=',es16.9, &
                    '  RD=',es11.4,'  Value=',es11.4,'  ucomb=',es15.8)
            else
                xcorr = ZERO
                if(FitDecay) then
                    if(abs(covar(1,1)*covar(2,2)) > EPS1MIN) xcorr = &
                        covar(1,2)/sqrt(covar(1,1)*covar(2,2))
                end if
                if(FitDecay)  then
                    write(log_str, 68) it,vname,real(newvalue,8), real(RD,8),   &
                    real(Ucomb,8), real(xcorr,8),real(covar(1,2),8)
                    call logger(30, log_str)
                end if
                if(Gamspk1_Fit)  then
                    write(log_str, 68) it,vname,real(newvalue,8),real(RD,8),real(Ucomb,8)
                    call logger(30, log_str)
                end if
68              format(5x,' Iteration=',i3,':   ',a,'= ',es16.9,'  RD=',es16.9,'  Ucomb=',es12.5, &
                    '  corr(1,2)=',es11.4,' cov(1,2)=',es11.4 )

                if(DChain) then           !  37.4.2025
                  write(log_str,69) it,vname,real(newvalue,8), real(RD,8), real(Messwert(kEGr),8),real(ucomb,8)
69                 format(5x,' Iteration=',i3,':   ',a,'= ',es16.9,'  RD=',es11.4,'  Value=',es11.4,'  ucomb=',es15.8)
                  call logger(30, log_str)
                endif


            END if

            if(it > nit_detl_max) EXIT
            if(it > 200) exit
            if(limit_typ == 1) EXIT
            ratmin = 0.0000010_rn
            if(rn == 10) ratmin = 0.0000002_rn
            ratmin2 = ratmin * 10._rn
            if( kpmle /= 1 .and. it > 2 .and. abs(newvalue-oldvalue)/oldvalue < ratmin) EXIT
            if( kpmle == 1 .and. it > 2 .and. abs(newvalue-oldvalue)/oldvalue < ratmin2) EXIT
            oldvalue = newvalue

        end do      ! End of iteration loop

44      continue

        if(limit_typ == 1) Ucomb_DTv = Ucomb
        if(limit_typ == 2) Ucomb_DLv = Ucomb

        if(FitDecay) then
            fpaLYT(1+limit_typ,1:ma) = fpa(1:ma)
            sfpaLYT(1+limit_typ,1:ma) = sfpa(1:ma)
            covarLyt(1+limit_typ) = ZERO
            if(knumEGr > 1) covarLyt(1+limit_typ) = covar(1,2)
        end if

        if(.false. .and. FitDecay .and. limit_typ == 1) then

            call logger(66, 'Messwert array for DT:')
            do i=1,ngrs+ncov+numd
                if(i >= kfitp(1) .and. i <= kfitp(1)+2) then

                    write(log_str, '(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(fpa(i-kfitp(1)+1),8), &
                        'u(mw(i))=',real(sfpa(i-kfitp(1)+1),8)
                    call logger(66, log_str)
                    Messwert(i) = fpa(i-kfitp(1)+1)
                else

                    write(log_str, '(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(Messwert(i),8), &
                        'u(mw(i))=',real(StdUnc(i),8)
                    call logger(66, log_str)
                end if
            end do
        end if

        iteration_on = .false.
!-----------------------------------------------------------------------
        Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)
        StdUnc(1:ngrs+ncov+numd)   = StdUncSV(1:ngrs+ncov+numd)

        if(.false. .and. FitDecay .and. limit_typ == 1) then
            call logger(66, 'Messwert array after DT:')
            do i=1,ngrs+ncov+numd
                if(i >= kfitp(1) .and. i <= kfitp(1)+2) then
                    write(log_str, '(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(fpa(i-kfitp(1)+1),8), &
                        'u(mw(i))=',real(sfpa(i-kfitp(1)+1),8)
                    call logger(66, log_str)
                    Messwert(i) = fpa(i-kfitp(1)+1)
                else
                    write(log_str, '(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(Messwert(i),8), &
                        'u(mw(i))=',real(StdUnc(i),8)
                    call logger(66, log_str)
                end if
            end do
        end if

        if(FitDecay) then
            fpa = fpaSVur
            dnetrate(1:numd)  = Messwert(ngrs+ncov+1:ngrs+ncov+numd) - d0zrate(1:numd)
            if(k_rbl > 0) dnetrate(1:numd) = dnetrate(1:numd) -  Messwert(kpoint(k_rbl))
            SDnetrate(1:numd) = Messwert(ngrs+ncov+1:ngrs+ncov+numd)/dmesszeit(1:numd) + sd0zrate(1:numd)**TWO
            if(k_rbl > 0) SDnetrate(1:numd) = SDnetrate(1:numd) + StdUnc(kpoint(k_rbl))**TWO
            do i=1,numd
                SDnetrate(i) = MAX(ZERO,  sqrt(SDnetrate(i)) )
            END do
        end if
!-----------------------------------------------------------------------
        GOTO 1500
1500    CONTINUE
!-------------------------------------------------------------------------
        if(FitDecay) ifit = ifitDL

        return
    end subroutine detlim_iter

!#######################################################################

    module subroutine setupParser(iopt)

        ! the function parser (mnemonic SUP) is initiated for a sufficient
        ! number of formulae and fed with the array Rseite containing formula strings.
        !
        ! Up to 2*knumEGr (<=6) new formulae are generated, RSeite_zero(1:3)
        ! and Rseite_one(1:3), which later serve for deriving the two constants
        ! Fconst and Flinear characterizing the linear relation
        !    activity = Fconst + Flinear*netCountRate
        ! In these equations, the net count rate symbol in the equation kEGr (<=3)
        ! is replaced by the numbers 0.0 (RSeite_zero(kEGr)) and by 1.0
        ! (RSeite_one(kEGr)). These formualae are also supplied to the function
        ! parser. Fconst can then be determined by calculating the value of
        ! RSeite_zero(kEGr), and Flinear by knowing Fconst and calcualating the
        ! value of RSeite_one(kEGr).
        !
        !     Copyright (C) 2014-2024  Günter Kanisch

        use UR_Gleich_globals,      only: kEGr,knumEGr,nab,nmodf,nabf,ncovf,nfkf,klinf, &
                                          kgspk1,kfitcal,SymboleG,RSeite,Rseite_zero,Rseite_one, &
                                          knetto,Symbole,ifehl,ksumeval, &
                                          Messwert, MesswertSV
        use UR_Linft,       only: FitDecay, FitCalCurve,kfitp,netto_involved_Fitcal,SumEval_fit
        use UR_Gspk1Fit,    only: Gamspk1_Fit
        use UR_Perror,      only: ifehlp
        USE fparser,        ONLY: initf, parsef
        USE ur_general_globals,   only: Gum_restricted
        use Top,            only: WrStatusbar,CharmodA1,dpafact
        use UWB,            only: ResultA
        use KLF,            only: CalibInter
        use file_io,        only: logger
        use CHF,            only: ucase,testSymbol, StrReplace
        use translation_module, only: T => get_translation

        use UR_DecChain,    only: nDCnet,indDCnet,indDCgross,indDCbg,DChainEGr
        use UR_Dlim,        only: DCFlin,DCEGr,DCRnet


        implicit none

        integer   ,intent(in) :: iopt     ! for control output: (0 (none) or 1 (with))

        integer            :: nhg,i,i0,j,ndd,klu,k,klu2,ix1,ix2
        real(rn)           :: zfit0,zfit,uzfit, dpa, Fv1, Fv2
        character(len=350) :: rsfG
        character(len=512) :: log_str
        character(:),allocatable :: RSeite_zeroSV2
!----------------------------------------------------------------------
        if(Gum_restricted) then
            call initf(nab+nmodf+nabf+ncovf+nfkf+2*knumEGr+10)
        else
            call initf(nab+nmodf+nabf+ncovf+nfkf+2*knumEGr+10)
        end if

        if(iopt == 1)  then
            write(log_str, '(4(a,i3))') 'SUP:   fparser: initf erfolgt:  nab=',nab,    &
            ' nmodf=',nmodf,' nabf=',nabf,' ncovf=',ncovf
            call logger(66, log_str)
        end if

        nhg = nab+nmodf+nabf

        do i=1,nab+nmodf+nabf+ncovf+nfkf
            rsfG = trim(ucase(Rseite(i)%s))
            ifehlp = 0
            if(FitDecay .and. i == klinf) CYCLE
            if(Gamspk1_Fit .and. i == kgspk1) CYCLE
            if(SumEval_Fit .and. i == ksumeval) CYCLE
            if(FitCalCurve .and. i == kfitcal) CYCLE
            if(index(rsfG,'KALFIT') > 0) cycle
            if(len_trim(RSeite(i)%s) == 0) cycle
            if(index(RSeite(i)%s,'SDECAY') > 0) cycle                  ! <-- 28.4.2025
            if(i <= nhg) then
                call parsef(i,RSeite(i)%s,SymboleG)
                if(ifehl == 1) return
                ! write(66,*) 'SUP: fparser: i=',i,',  i<=nhg: parsef von ',TRIM(Rseite(i)%s), &
                !             ' erfolgt: ifehlp=',ifehlp,' kgspk1=',kgspk1
            end if
            if(i > nhg .and. len_trim(Rseite(i)%s) > 0) then
                call parsef(i,Rseite(i)%s,SymBoleG)
                if(ifehlp == 1) return
                ! write(66,*) 'SUP: fparser: i=',i,',  i>nhg: parsef von ',TRIM(Rseite(i)%s), &
                !             ' erfolgt: ifehlp=',ifehlp,' kgspk1=',kgspk1
            end if
            if(ifehlp == 1)  then
                write(log_str, '(*(g0))') 'SUP:  parsef:  kEGr=',int(kEGr,2),'  ifehlp=1 (Fehler)  für Gleichung Nr=',int(i,2)
                call logger(66, log_str)
            end if
        end do

        ndd = nab+nmodf+nabf+ncovf+nfkf
        if(Gum_restricted) return

        write(log_str, '(a,i2,a,L1,a,L1,a,i3)') 'knumEgr=',knumEGr,'  FitDecay=',FitDecay,'  Gamspk1_Fit=',Gamspk1_Fit,'  klinf=',klinf
        call logger(66, log_str)
        write(log_str, '(a,i0,a,L1,a,L1,a,i3)') 'kgspk1=',kgspk1,'  gum_restricted=',gum_restricted,'  FitCalCurve=',FitCalCurve, &
            ' ndd=',ndd
        call logger(66, log_str)
        if(allocated(Rseite_zero)) deallocate(Rseite_zero,Rseite_one)
        allocate(Rseite_zero(20),Rseite_one(20))
        if(ubound(Rseite,dim=1) < 20 ) call CharModA1(Rseite,20)

        do j=1,knumEGr
            klu = 0
            klu2 = 0
            if(FitDecay) then
                klu = klinf
                if(kfitp(1) > 0)  klu = kfitp(1)-1+j
            end if
            if(DChainEGr) klu = j                   ! 27.4.2025
            if(Gamspk1_Fit) klu = kgspk1
            if(klu == 0) klu = knetto(j)
            if(klu == 0 .and. gum_restricted) klu = kEGr
            if(klu == 0) cycle
            write(log_str, '(a,i3,2x,a)') 'klu=',klu,Symbole(klu)%s
            call logger(66, log_str)

            Rseite_zero(j)%s = Rseite(j)%s
            rsfG = trim(ucase(Rseite_zero(j)%s))
            Rseite_zero(j)%s = trim(rsfG)
            i0 = index(RSeite_zero(j)%s,SymboleG(klu)%s)
            if(.not.testsymbol(Rseite_zero(j)%s,SymboleG(klu)%s)) then
                do k=knumEGr+1,klu-1
                    if(testsymbol(Rseite(k)%s,SymboleG(klu)%s)) then
                        if(testsymbol(Rseite(j)%s,SymboleG(k)%s)) then
                            if(k /= kfitcal .and. k /= kgspk1 .and. k /= klinf) then
                                klu2 = k
                                Rseite_zero(klu2)%s = Rseite(klu2)%s
                                rsfG = trim(ucase(Rseite_zero(klu2)%s))
                                Rseite_zero(klu2)%s = trim(rsfG)
                                exit
                            end if
                        end if
                    end if
                end do
            else
                ! nothing
            end if

            if(klu2 == 0) i0 = index(RSeite_zero(j)%s,SymboleG(klu)%s)
            if(klu2 > 0) i0 = index(RSeite_zero(j)%s,SymboleG(klu2)%s)
            !write(66,'(a,i3,a,i3,2(a,i3),a,a)') 'j=',j,' i0=',i0,' klu=',klu,' klu2=',klu2,  &
            !                                         ' Rseite_zero(j)=',trim(Rseite_zero(j)%s)
            if(i0 > 0) then
                if(klu2 == 0) then
                    ! write(66,*) 'A1:  Rseite_zero(j)=',trim(Rseite_zero(j)%s)
                else
                    RSeite_zeroSV2 = trim(Rseite_zero(j)%s)
                    call StrReplace(Rseite_zeroSV2,SymboleG(klu2)%s,'(' // Rseite_zero(klu2)%s // ')', .true., .true.)
                    Rseite_zero(j)%s = Rseite_zeroSV2
                end if
                Rseite_one(j)%s = Rseite_zero(j)%s
                call StrReplace(Rseite_one(j)%s,SymboleG(klu)%s,'1', .true., .true.)
                call StrReplace(Rseite_zero(j)%s,SymboleG(klu)%s,'0', .true., .true.)
            else
                Rseite_zero(j)%s = '0                         '
                Rseite_one(j)%s = '1                         '
            end if
            if(FitCalCurve .and. netto_involved_Fitcal) then
                call CalibInter(2,ZERO,ONE, zfit0,uzfit)
                write(Rseite_zero(j)%s,'(es13.6)') zfit0
                call CalibInter(2,ONE,one, zfit,uzfit)
                write(Rseite_one(j)%s,'(es13.6)') zfit
            end if
            ! write(66,*) 'RSeite_zero(j)=',trim(RSeite_zero(j))
            ! write(66,*) 'RSeite_one(j)=',trim(RSeite_one(j))

            if(DchainEGr) then
              !  27.12.2024  GK                 ! 27.4.2025
              ! The factor Flinear does no longer hold here, it is splitted into more than
              ! one of such factors:
              ! output quantity = DCFlin(1)*DCRnet(1) + DCFlin(2)*DCRnet(2):

              DCEGr(1:knumEGr) = MesswertSV(1:knumEGr)           ! save output quantity values
              DCRnet(1:nDCnet) = MesswertSV(indDCnet(1:nDCnet))  ! save net count rate values

              k = kEGr
              Fv1 = MEsswertSV(kEGr)    ! Resulta(k)
              do i=1,nDCnet
                dpa = Messwert(indDCnet(i))*dpafact(Messwert(indDCnet(i))) - Messwert(indDCnet(i))
                Messwert(indDCnet(i)) = Messwert(indDCnet(i)) + dpa
                   ! without the following singel statement, the derivative of Resulta would not work!
                   ! The reason is, that the modified net rate is destroyed, i.e., it is replaced
                   ! by the difference (gross - BG), which is always the same, unless the gross rate
                   ! is also modified!
                   Messwert(indDCgross(i)) = Messwert(indDCnet(i)) + Messwert(indDCbg(i))
                Fv2 = Resulta(k)
                DCFlin(i) = (Fv2-Fv1)/dpa
                Messwert(indDCnet(i)) = Messwert(indDCnet(i)) - dpa
                Messwert(k) = ResultA(k)
              end do
                ! write(66,*) 'SUP:  DCHAIN:  DCFlin=',sngl(DCFlin(1:nDCnet))
            end if

            call parsef(ndd+(j-1)*2+1, Rseite_zero(j)%s, SymboleG)
            if(ifehl == 0) then
                call parsef(ndd+(j-1)*2+2, Rseite_one(j)%s, SymboleG)
            end if
            if(ifehl == 1) then

                call WrStatusBar(3, T('Error') // ": SetupParser: klu=0")
                call WrStatusBar(4, T('Abortion!'))

                call logger(66, 'Error within SetupParser in Rechw2: klu=0')
                return
            end if
            ix1 = ndd+(j-1)*2+2
            ix2 = ubound(Rseite,dim=1)
            if(ix1 > ix2) call charModA1(RSeite,ix1)
            RSeite(ndd+(j-1)*2+1)%s = RSeite_zero(j)%s
            RSeite(ndd+(j-1)*2+2)%s = RSeite_one(j)%s
            ! write(66,'(a,i3,a,i3,a,a,a,i3)') 'j=',j,' EQnum=',ndd+(j-1)*2+1,'  Rseite_zero=',trim(Rseite_zero(j)%s),  &
            !                            '   parsef-Index(ndd+(j-1)*2+1)=',ndd+(j-1)*2+1
            !write(66,'(a,i3,a,i3,a,a,a,i3)') 'j=',j,' EQnum=',ndd+(j-1)*2+2,'  Rseite_one=',trim(Rseite_one(j)%s),  &
            !                            '   parsef-Index(ndd+(j-1)*2+2)=',ndd+(j-1)*2+2
        end do

        write(log_str, '(a,i2,a,i3)') 'SUP END:   knumEGr=',knumEGr,'  ndd=',ndd
        call logger(66, log_str)

    end subroutine setupParser

!#######################################################################


    module real(rn) function RnetVal(xAct)

        ! this function converts a value xAct of activity to the associated
        ! value of the net counting rate. It applies the linear relation
        !    activity = Fconst + Flinear*netCountRate
        ! Fconst is usually - but not alwys - equal to zero.
        ! As it may be possible for a project that the cited relation could
        ! be non-linear, the iterative method by Brent is used for converting
        ! to the net count rate.
        !     Copyright (C) 2014-2024  Günter Kanisch


        use UR_Gleich_globals, only: Messwert, klinf, kgspk1, kEGr, knetto, ifehl, ngrs, ncov, &
                                     nmumx, Rnetmodi, ksumeval
        use UR_Linft,          only: FitDecay, kfitp, numd, SumEval_fit
        USE UR_Gspk1Fit,       only: Gamspk1_Fit
        use ur_general_globals,      only: Gum_restricted
        use UR_DLIM,           only: FConst, FLinear, fvalueB, modeB, iteration_on, kluB
        use Top,               only: WrStatusbar
        use translation_module, only: T => get_translation
        use UR_DecChain,       only: DChainEgr


        implicit none
        real(rn), intent(in)    :: xAct

        integer          :: klu, iter, itmax, modeSV
        real(rn)         :: x1,x2,xacc,mws(nmumx),dummy
        real(rn)         :: zerof,sa,sb,fa,fb
        logical          :: iteronSV,RnetmodiSV

        interface
            function ffuncRnet(mode, x)
                use UR_types, only: rn
                implicit none

                real(rn)              :: ffuncRnet
                integer, intent(in)   :: mode
                real(rn),intent(in)   :: x

            end function ffuncRnet
        end interface

        modeSV = modeB
        RnetmodiSV = Rnetmodi
        Rnetmodi = .true.
        iteronSV = iteration_on
        iteration_on = .false.

        ifehl = 0
        if(Gum_restricted) then
            Rnetval = xAct
            return
        end if
        RnetVal= ZERO
        mws(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)   ! save Messwert

        if(.true. .and. abs(xAct) < EPS1MIN) then
            Rnetval = (xAct - Fconst)/Flinear
            goto 100
        end if

        klu = knetto(kEGr)
        if(FitDecay) klu = klinf
        if(Gamspk1_Fit) klu = kgspk1
        if(FitDecay .and. kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
        if(Sumeval_fit) klu = ksumeval
        if(DChainEGr) klu = kEGr           ! 274.2025

        if(klu == 0) then
            ifehl = 1
            call WrStatusBar(3, T('Error') // ": RnetVal: klu=0")
            call WrStatusBar(4, T('Abortion!'))
            goto 100   !return
        end if
        ! write(66,*) 'RnetVal:  Act=',sngl(Act),' Flinear=',sngl(Flinear),' Fconst=',sngl(Fconst), &
        !                ' klu=',int(klu,2)
        if(klu == kEGr .and. .not.DChainEGr) then              ! <-- modified 27.4.2025
            RnetVal = xAct
            goto 100        !return
        end if
        ! x1, x2 : bracketing values encompassing the desired net cout rate value
        x1 = 0.7_rn * (xAct-Fconst)/FLinear
        x2 = ONE/0.7_rn * (xAct-Fconst)/Flinear ! *2._rn
        if(x1 > x2) then
            dummy = x2
            x2 = x1
            x1 = dummy
        end if
        xacc = abs(x1+x2)/TWO*0.00000001_rn   ! / 10._rn

        ! Rnetval = (xAct - Fconst)/Flinear
        ! goto 100

        !write(66,*)'RnetVal:    x1, x2=',sngl(x1),sngl(x2),'xAct=',sngl(xAct),'  Fconst=',sngl(Fconst),&
        !                             ' Flinear=',sngl(Flinear),' klu=',int(klu,2)
        !  the mode parameter value 15 is used in the function Prfunc called by brentx.

        fvalueB = xAct
        kluB = klu
        itmax = 30
        ! rzeroRn correponds to rzero, but used for calculating the net count rate value
        call rzeroRn( x1, x2, epsilon(1._rn), xacc, ffuncRnet, fvalueB, zerof, itmax, iter, &
            30,.false.,sa,sb,fa,fb )
        RnetVal = zerof
        ! write(30,*) 'Rnetval: xact=',sngl(xact),' klu=',klu,' zerof=',sngl(zerof)

        ! write(30,*) 'Rnetval:   (Fconst + Flinear*Rnetval)/xAct=',sngl((Fconst + Flinear*Rnetval)/xAct)


        if(ifehl == 1) then
            if(xact < 1.e-10_rn) then
                ifehl = 0
                call WrStatusBar(3,'  ')
                call WrStatusBar(4,'..')
                x1 = - x1
                kluB = klu
                itmax = 30
                call rzeroRn( x1, x2, epsilon(1._rn), xacc, ffuncRnet, fvalueB, zerof, itmax, iter, &
                    30,.false.,sa,sb,fa,fb )
                RnetVal = zerof

                if(ifehl == 0) goto 100
            end if

            call WrStatusBar(3,T('Error') // ": " // "RnetVal: IT>10")
            call WrStatusBar(4, T('Abortion!'))

            goto 100       ! return
        end if

100 continue
        Rnetmodi = RnetmodiSV
        iteration_on = iteronSV
        modeB = modeSV
        Messwert(1:ngrs+ncov+numd) = mws(1:ngrs+ncov+numd)   ! restore Messwert array

    end function RnetVal

    !##############################################################################

    module integer function kqt_find()
        use UR_DLIM,            only: iteration_on, limit_typ
        use UR_MCC,             only: kqtypx
        use ur_general_globals, only: MCsim_on

        implicit none

        if(.not.MCsim_on) then
            kqt_find = 1
            if(iteration_on .and. limit_typ == 1) kqt_find = 2
            if(iteration_on .and. limit_typ == 2) kqt_find = 3
        else
            kqt_find = kqtypx
        end if
    end function kqt_find

    !##################################################################################


end submodule RW2A

!#######################################################################################

    subroutine rzeroRn (a, b, machep, t, ff2,fvalue, zerof, itmax, iter, &
                        munit, prout, sa, sb, fa, fb )
    ! rzeroRn correponds to rzero, but used for calculating the net count rate value.
    ! It is called directly by Rnetval.

    use UR_types,     only: rn
    use UR_params,    only: EPS1MIN
    use UWB,          only: ResultA
    use file_io,      only: logger

!*****************************************************************************80
!
!! ZERO seeks the root of a function F(X) in an interval [A,B].
!
!  Discussion:
!
!    The interval [A,B] must be a change of sign interval for F.
!    That is, F(A) and F(B) must be of opposite signs.  Then
!    assuming that F is continuous implies the existence of at least
!    one value C between A and B for which F(C) = 0.
!
!    The location of the zero is determined to within an accuracy
!    of 6 * MACHEPS * abs ( C ) + 2 * T.
!
!    Thanks to Thomas Secretin for pointing out a transcription error in the
!    setting of the value of P, 11 February 2013.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 2013
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization Without Derivatives,
!    Dover, 2002,
!    ISBN: 0-486-41998-3,
!    LC: QA402.5.B74.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the change of
!    sign interval.
!
!    Input, real ( kind = 8 ) MACHEP, an estimate for the relative machine
!    precision.
!
!    Input, real ( kind = 8 ) T, a positive error tolerance.
!
!    Input, external real ( kind = 8 ) F, the name of a user-supplied
!    function, of the form "FUNCTION F ( X )", which evaluates the
!    function whose zero is being sought.
!
!    Output, real ( kind = 8 ) ZERO, the estimated value of a zero of
!    the function F.
!
    implicit none

    external ff2

    real ( kind = rn ) a
    real ( kind = rn ) b
    real ( kind = rn ) c
    real ( kind = rn ) d
    real ( kind = rn ) e
    real ( kind = rn ) ff2     ! f
    real ( kind = rn ) fa
    real ( kind = rn ) fb
    real ( kind = rn ) fc
    real ( kind = rn ) m
    real ( kind = rn ) machep
    real ( kind = rn ) p
    real ( kind = rn ) q
    real ( kind = rn ) r
    real ( kind = rn ) s
    real ( kind = rn ) sa
    real ( kind = rn ) sb
    real ( kind = rn ) t
    real ( kind = rn ) tol
    real ( kind = rn ), intent(out) :: zerof

!--------------------

    integer, intent(in)  :: munit
    logical, intent(in)  :: prout
    integer, intent(in)  :: itmax
    integer, intent(out) :: iter

    real(rn),intent(in)    :: fvalue
    character(128)         :: log_str
    real(rn)               :: a_anf, b_anf
    !
    !  Make local copies of A and B.
    !
    a_anf = a
    b_anf = b
    sa = a
    sb = b
    fa = ff2(sa ) - fvalue
    fb = ff2(sb ) - fvalue
    c = sa
    fc = fa
    e = sb - sa
    d = e

    iter = 0
    do
        iter = iter + 1
        if ( abs ( fc ) < abs ( fb ) ) then

            sa = sb
            sb = c
            c = sa
            fa = fb
            fb = fc
            fc = fa

        end if

        tol = 2.0_rn * machep * abs ( sb ) + t
        m = 0.5_rn * ( c - sb )

        if ( abs ( m ) <= tol .or. abs(fb) < EPS1MIN ) then
            exit
        end if

        if ( abs ( e ) < tol .or. abs ( fa ) <= abs ( fb ) ) then

            e = m
            d = e

        else

            s = fb / fa

            if ( abs(sa - c) < EPS1MIN ) then

                p = 2.0_rn * m * s
                q = 1.0_rn - s

            else

                q = fa / fc
                r = fb / fc
                p = s * ( 2.0_rn * m * q * ( q - r ) - ( sb - sa ) * ( r - 1.0_rn ) )
                q = ( q - 1.0_rn ) * ( r - 1.0_rn ) * ( s - 1.0_rn )

            end if

            if ( 0._rn < p ) then
                q = - q
            else
                p = - p
            end if

            s = e
            e = d

            if ( 2.0_rn * p < 3.0_rn * m * q - abs ( tol * q ) .and. &
                p < abs ( 0.5_rn * s * q ) ) then
                d = p / q
            else
                e = m
                d = e
            end if

        end if

        sa = sb
        fa = fb

        if ( tol < abs ( d ) ) then
            sb = sb + d
        else if ( 0._rn < m ) then
            sb = sb + tol
        else
            sb = sb - tol
        end if
        fb = ff2(sb ) - fvalue    !!

        if ( ( 0._rn < fb .and. 0._rn < fc ) .or. &
            ( fb <= 0._rn .and. fc <= 0._rn ) ) then
            c = sa
            fc = fa
            e = sb - sa
            d = e
        end if
        if(prout) then

            write(log_str, '(*(g0))') ' sa=',sngl(sa),' sb=',sngl(sb),' fa=',sngl(fa), &
                                      '  fb=',sngl(fb),' fvalue=',sngl(fvalue)
            call logger(munit, log_str)
        end if
        if(iter > itmax) exit
    end do

    zerof = sb
! write(30,*) 'rzeroRn: zerof=',sngl(zerof),' iter=',iter

    return
    end subroutine rzeroRn

!#######################################################################################

    real(rn) function ffuncRnet(x)

! This function is used by Rnetval, passed as an argument to the subroutine rzerosRn.
!   Copyright (C) 2023  Günter Kanisch

    use UR_types,  only: rn
    use UR_DLIM,    only: kluB
    use UR_Gleich_globals,  only: Messwert,kEGr
    use UWB,        only: ResultA

    implicit none

    real(rn), intent(in)   :: x

    Messwert(kluB) = x
    ffuncRnet = Resulta(kEGr)

    !write(30,*) 'ffuncRnet: x=',sngl(x),' f=',sngl(ffuncRnet),'  fvalue=',sngl(fvalueB), &
    !                      ' mode=',mode,' kluB=',int(kluB,2),' Rnetmodi=',Rnetmodi

    end function ffuncRnet

!#######################################################################################

