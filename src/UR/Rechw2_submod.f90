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

    use UR_params

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
        !     Copyright (C) 2014-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,  only: c_null_char,c_ptr,c_int
        use gtk,            only:   GTK_BUTTONS_OK,gtk_widget_set_visible,GTK_MESSAGE_WARNING

        USE UR_Gleich,      only:   Symbole,CVFormel,Messwert,RSeite,SymboleG,StdUnc,wpars, &
                                    SDFormel,symtyp,coverf,coverin,DT_increase,DL_increase,ifehl,   &
                                    ilam_binom,ip_binom,increase_dpafact,itm_binom,ISymbA,   &
                                    kbgv_binom,kEGr,kfitcal,kgspk1,klinf,knumEGr,lintest,loadingpro, &
                                    ksumeval,nab,nabf,kbrutto2,kbrutto2_kbd,kbrutto_double,MesswertSVG, &
                                    ncov,ncovf,ngrs,nmodf,nonPoissGrossCounts,nvar,nvars_in_rbtot,  &
                                    nwpars,Resultat,StdUncSVG,covarval,percsum,Rnetmodi,sensi,tback, &
                                    tgross,Ucomb,Ucomb_anf,ueg_increase,urelw,use_bipoi,   &
                                    var_rbtot,covarval,MEsswertSV,StdUncSV,kbrutto,knetto,kbrutto_gl, &
                                    sensiSV,percSV,ueg_normal,iptr_cnt,iptr_time,vars_rbtot,Messwert_CP, &
                                    perc,StdUncSV1,StdUnc_CP,UcontribSV,Ucontrib,CovarvalSV,nwpars, &
                                    WparsInd,maxlen_symb,nvarsMD,missingval,nRSsy,RS_SymbolNr
        use ur_linft
        use ur_dlim
        use ur_variables,   only:   langg,fname,gum_restricted,multi_eval,gross_negative,kmodeltype, &
                                    kmodelold,savep,bat_serial,rw2_on,batf,batest_user,ableit_fitp

        use fparser,        only:   evalf, EvalErrMsg
        USE UR_Perror
        USE UR_Gspk1Fit
        use UWB,            only:   gevalf
        use Rout,           only:   MessageShow, WTreeViewPutDoubleCell,WDNotebookSetCurrPage, &
                                    WTreeViewPutDoubleArray,WDSetCheckMenuItem,pending_events, &
                                    WDSetCheckButton,WDPutSelRadioMenu
        use Rw1,            only:   covppcalc
        use top,            only:   idpt,WrStatusbar,dpafact,FieldUpdate,chupper_eq,CharModA1,IntModA1, &
                                    RealModA1
        use Brandt,         only:   pnorm,qnorm,mtxchi,mean,sd
        use UR_gtk_variables, only: consoleout_gtk
        use UWB,            only:   Resulta,upropa,RbtCalc,func_Fconst,func_Flinear
        use Num1,           only:   funcs
        use LF1,            only:   Linf
        use LF1G,           only:   Linfg1,Linfg1Ausf
        use CHF,            only:   FindLocT,ucase,testSymbol
        use file_io,        only:   logger
        use LSTfillT,       only:   WDListstoreFill_table

        implicit none

        integer               :: i,j,k,ksav,klu,kk
        integer               :: resp,ndd,itest
        real(rn)              :: dummy,omega
        real(rn)              :: UcombSV, XRD1,XRD2,XRB1,XRB2,xkp,xkq
        real(rn)              :: akt,SDakt,PercsumSV
        real(rn)              :: xsav,F1x,F2x,dpi
        real(rn)              :: RD, chisqr_EG, dpi_gevalf, MesswertSV_nvar, &
                                 MesswertSV_icnvar
        CHARACTER(LEN=15)     :: verfahren
        CHARACTER(LEN=200)    :: str1
        character(len=70)     :: cforma
        character(:),allocatable  :: ch1
        character(LEN=1)      :: cr
        logical               :: tvar,found

        integer                  :: kzero1,kone1,imax,kfound
        real(rn)                 :: w, urelw1, urelw2, varw, fv1, fv2, dpa
        real(rn), allocatable    :: Bmat(:,:), dvec(:), bvec(:), Uxinv(:,:)
        character(len=512)       :: log_str
        integer, allocatable     :: iact(:),iter(:)
!-----------------------------------------------------------------------

        WRITE(cr,'(a)') CHAR(13)

        RW2_on = .true.
        Rnetmodi = .FALSE.
        kqtyp = 1  ! 9.6.2024

!         if(FitDecay) WRITE(66,*) '##################### Rechw2: ',Symbole(kEGr)%s,'  ',trim(fitmeth),'  ##################'
        if(FitDecay)  then
            write(log_str, '(*(g0))') '##################### Rechw2: ',Symbole(kEGr)%s,'  ',trim(fitmeth),'  ##################'
            call logger(66, log_str)
        end if
!         if(.not.FitDecay) Write(66,*) '##################### Rechw2: ',Symbole(kEGr)%s,'  ##################'
        if(.not.FitDecay)  then
            write(log_str, '(*(g0))') '##################### Rechw2: ',Symbole(kEGr)%s,'  ##################'
            call logger(66, log_str)
        end if
        if(consoleout_gtk) WRITE(0,*) '##### Begin of Rechw2  ##########################'

        if(.not.loadingPro) then
            IF(langg == 'DE') call WrStatusBar(4,'Rechnet...' )
            IF(langg == 'EN') call WrStatusBar(4,'Calculating...' )
            IF(langg == 'FR') call WrStatusBar(4,'Calcule...' )
        end if

        if(allocated(CVFormel) .and. ubound(CVFormel,dim=1) == 0)   &
            call CharModA1(CVFormel,ubound(IsymbA,dim=1))
        allocate(character(len=200) :: ch1)

!         WRITE(66,'(a,i2,a,i2)') 'ncov=',ncov,'   ncovf=',ncovf
        write(log_str, '(a,i2,a,i2)') 'ncov=',ncov,'   ncovf=',ncovf
        call logger(66, log_str)
        do i=1,ncov
            if(size(CVFormel) < i) exit
!             if(len_trim(CVFormel(i)%s) > 0) write(66,'(a,i3,a,a)') 'i=',i,'  CVFormel(i)=',CVFormel(i)%s
            if(len_trim(CVFormel(i)%s) > 0)  then
                write(log_str, '(a,i3,a,a)') 'i=',i,'  CVFormel(i)=', CVFormel(i)%s
                call logger(66, log_str)
            end if
        end do
        fpaLYT = zero
        covarLYT = zero
        ableit_fitp = .false.

        call setupParser(1)

        if(ifehl == 1) goto 9000

        call logger(66, ' ')
        ! write(66,*) 'ncov=',int(ncov,2),' numd=',int(numd,2)
        write(cforma,'(a,i2.2,a)') '(i3,2x,a,2x,a1,T',maxlen_symb+8,',2(2x,a,es15.8))'
        ! write(66,*) 'cforma=',cforma,' maxlen_symb=',maxlen_symb
        do i=1,ngrs+ncov+numd
!             WRITE(66,cforma) i,Symbole(i)%s,symtyp(i)%s,' Messwert=',real(Messwert(i),8),  &
!             ! for A. Baumann: do not change this!
            write(log_str, cforma) i,Symbole(i)%s,symtyp(i)%s,' Messwert=',real(Messwert(i),8),  &
                                   ' StdUnc=',real(StdUnc(i),8)

            call logger(66, log_str)

        end do

        if(.not. allocated(MesswertSVG)) then
            allocate(MesswertSVG,source=Messwert)
            MesswertSVG = zero
        end if
        if(.not. allocated(StdUncSVG)) then
            allocate(StdUncSVG,source=StdUnc)
            StdUncSVG = zero
        end if

        Resultat = Resulta(kEGr)

        if(MesswertSVG(1) <= -one .and. StdUncSVG(1) <= -one) then
            do i=1,ngrs+ncov+numd
                MEsswertSVG(i) = Messwert(i)
                if(i> ngrs .and. i <= ngrs+ncov) MesswertSVG(i) = covarval(i-ngrs)
                StdUncSVG(i) = StdUnc(i)
            end do
        end if

!         WRITE(66,'(a,3es12.4,a,i0,a,f6.3)') 'Begin of Rechw2: Result, Ucomb, coverf=', &
!             resultat,Ucomb,coverf,' kEGr=',kEGr,'  coverin=',coverin
        write(log_str, '(a,3es12.4,a,i0,a,f6.3)') 'Begin of Rechw2: Result, Ucomb, coverf=', &
            resultat,Ucomb,coverf,' kEGr=',kEGr,'  coverin=',coverin
        call logger(66, log_str)

!         IF(FitDecay) WRITE(66,'(a,i0)') 'klincall =',klincall
        IF(FitDecay)  then
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

! do i=1,ngrs+ncov+numd
!   write(66,*) 'i=',int(i,2),' MW(i)=',sngl(Messwert(i)),' MWSV(i)=',sngl(MesswertSV(i)), &
!                         ' MWSVG(i)=',sngl(MesswertSVG(i))
! end do

        call upropa(kEGr)
        Ucomb = Ucomb * coverf
        if(LinTest) UEG_normal = Ucomb
        call WDListstoreFill_table('liststore_budget',3,.true.)   ! 14.7.2023: again
        UcombSV = Ucomb
        imax = ngrs+ncov+numd
        SensiSV(1:imax)    = Sensi(1:imax)
        percSV(1:imax)     = perc(1:imax)
        StdUncSV1(1:imax)  = StdUnc(1:imax)
        UcontribSV(1:imax) = Ucontrib(1:imax)
        MesswertSV(1:imax) = Messwert(1:imax)      ! 14.7.2023

        PercsumSV = percsum

! Save covar values:
        if(allocated(CovarvalSV)) deallocate(CovarvalSV)
        allocate(CovarValSV(ncov))
        CovarvalSV(1:ncov) = Covarval(1:ncov)

! do i=1,ngrs+ncov+numd
!   WRITE(66,'(a,a,a,es15.7,a,es15.7)') ' Covar-Saven: ',Symbole(i),' Messwert=',Messwert(i),'  StdUnc=',sngl(StdUnc(i))
! end do
! do i=1,numd
!   WRITE(66,'(a,es11.4,2x,es11.4,,2x,es11.4)') 'vor DL: d0zrate,sd0zrate,tdiff: ',d0zrate(i),sd0zrate(i),dtdiff(i)
! end do

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
                FakRB = one
                kbrutto2 = 0
                kbrutto2_kbd = 0
                kbrutto_double = 0
                Fconst = zero
                Flinear = one
                goto 60
            end if
        end if

        XRB1 = zero
        XRB2 = zero
        XRD1 = zero
        XRD2 = zero
        IF(.not.FitDecay .AND. .not.Gamspk1_Fit .and. .not. SumEval_fit) THEN
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
            FakRB = one
        END IF
!         WRITE(66,*) 'XRB1,XRB2, XRD1,XRD2=',sngl(XRB1),sngl(XRB2), sngl(XRD1),sngl(XRD2)
        write(log_str, '(*(g0))') 'XRB1,XRB2, XRD1,XRD2=',sngl(XRB1),sngl(XRB2), sngl(XRD1),sngl(XRD2)
        call logger(66, log_str)
!         WRITE(66,*) 'FakRB = ',sngl(FakRB)
        write(log_str, '(*(g0))') 'FakRB = ',sngl(FakRB)
        call logger(66, log_str)
!         write(66,*) 'nonPoissGrossCounts=',nonPoissGrossCounts,' gross_negative=',gross_negative
        write(log_str, '(*(g0))') 'nonPoissGrossCounts=',nonPoissGrossCounts,' gross_negative=',gross_negative
        call logger(66, log_str)
        IF(abs(FakRB) < eps1min) FakRB = one

        imax = ngrs+ncov+numd
        Messwert(1:imax) = MesswertSV(1:imax)
        StdUnc(1:imax)   = StdUncSV(1:imax)
        Sensi(1:imax)    = SensiSV(1:imax)
        perc(1:imax)     = percSV(1:imax)
        Ucontrib(1:imax) = UcontribSV(1:imax)

!-----------------------------
        kbrutto2 = 0
        kbrutto2_kbd = 0
!IF(kbrutto(kEGr) > 0 .and. .not.FitDecay .and. .not.Gamspk1_Fit .and. &
!                              knumEGr == 1 .and. kbrutto_gl(kEGr) > 0) THEN
!  do i=1,nab
!    if(FitCalCurve .and. i == kfitcal) cycle
!        if(i == knetto(kEGr)) cycle
!    dpi = dpi_gevalf(kbrutto(kEGr),i)
!    IF(dpi < zero .and. .not.gross_negative) THEN
!      kbrutto2 = i       ! Number of the equation, in which a "double" of the gross count rate
!      !                  ! appears
!      ch1 = Rseite(kbrutto(kEGr))%s
!      do kk=nab+1,ngrs
!        IF(testSymbol(ch1,symboleG(kk)%s) .AND. INDEX(ch1,symBoleG(kk)%s) <= 2 .and. StdUnc(kk) > zero) THEN
!          kbrutto2_kbd = kk
!          EXIT
!        end if
!      end do
!    end if
!  end do
!end if
!IF(kbrutto2 > 0) THEN
!  WRITE(66,'(a,i0,a,a, a,i0)') 'Rechw2: 2nd gross count rate found: equation ',kbrutto2,'',SymboleG(kbrutto2)%s, &
!              '  kbrutto2_kbd=',kbrutto2_kbd
!                write(66,'(a,L1,a,I0)') 'gross_negative=',gross_negative,' kModelType=',kModelType
!end if

!---------------
        kbrutto_double = 0
        IF(kbrutto(kEGr) > 0 .and. ncov > 0 .and. .not.FitDecay .and. .not.Gamspk1_Fit &
            .and. knumEGr == 1) THEN
            do i=1,ngrs
                IF(i == kbrutto(kEGr)) CYCLE
                IF(ABS(Messwert(kbrutto(kEGr))-Messwert(i)) / Messwert(kbrutto(kEGr)) < 1.E-6_rn) THEN
                    IF(ABS(StdUnc(kbrutto(kEGr))-StdUnc(i)) / StdUnc(kbrutto(kEGr)) < 1.E-6_rn) THEN
                        kbrutto_double = i
!                         WRITE(66,'(a,i0,3a,i0)') 'Symbol(',i,') = identified double of the gross count rate: ',symbole(i)%s,' kbrutto_double=',i
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
            Fconst = zero
            Flinear = one
            goto 60
        end if
        klu = klinf
        IF(Gamspk1_Fit) klu = kgspk1
        IF(kfitp(1) > 0) klu = kfitp(1)-1+kEGr

        Fconst = func_Fconst(Messwert,ngrs+ncov+numd)
        Flinear = func_Flinear(Messwert,ngrs+ncov+numd)
!         write(66,*) 'Flinear from func_Flinear: ',sngl(Flinear),'  Fconst=',sngl(Fconst)
        write(log_str, '(*(g0))') 'Flinear from func_Flinear: ',sngl(Flinear),'  Fconst=',sngl(Fconst)
        call logger(66, log_str)
        iteration_on = .FALSE.
        Rnetmodi = .FALSE.

        ndd = nab+nmodf+nabf+ncovf
!         write(66,*) 'Rseite(ndd+(kEGr-1)*2+1))=',Rseite(ndd+(kEGr-1)*2+1)%s
        write(log_str, '(*(g0))') 'Rseite(ndd+(kEGr-1)*2+1))=',Rseite(ndd+(kEGr-1)*2+1)%s
        call logger(66, log_str)
!         write(66,*) 'Rseite(ndd+(kEGr-1)*2+2))=',Rseite(ndd+(kEGr-1)*2+2)%s
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

! 59    continue

        if(Fconst < zero .and. nWpars > 0) then
            do i=1,nWpars
                if(Wpars(i) > zero .and. abs(abs(Wpars(i))-abs(Fconst)) < 1.E-10_rn ) then
                    WparsInd(i:nWpars-1) = WparsInd(i+1:nWpars)
                    Wpars(i:nWpars-1) = WPars(i+1:nWpars)
                    nWpars = nWpars - 1
                    exit
                end if
            end do
        end if

!  Restore the values of the arrays Messwert and StdUnc:
!Messwert(1:ngrs) = MesswertSV(1:ngrs)
!StdUnc(1:ngrs) = StdUncSV(1:ngrs)
        Messwert(1:imax) = MesswertSV(1:imax)
        StdUnc(1:imax) = StdUncSV(1:imax)

        IF(ABS(Flinear) <= 1.E-10_rn) THEN
!             write(66,*) 'Flinear practically zero: Flinear=',sngl(Flinear)
            write(log_str, '(*(g0))') 'Flinear practically zero: Flinear=',sngl(Flinear)
            call logger(66, log_str)
            IF(langg == 'DE') WRITE(str1,*) 'Warnung: Die Ergebnisgröße hängt nicht von der ', cr, &
                ' selektierten Nettozählrate ', symbole(knetto(kEGr))%s,' ab!', cr, &
                ' Faktor Flinear=0! Bitte korrigieren!'
            IF(langg == 'EN') WRITE(str1,*) 'Warning: The output quantity does not depend on the',cr, &
                ' selected net conting rate, ', symbole(knetto(kEGr))%s,' !', cr, &
                ' Factor Flinear=0! Please, correct!'
            IF(langg == 'FR') WRITE(str1,*) 'Attention: La quantité de sortie ne dépend pas de',cr, &
                ' le taux contingentaire net sélectionné, ', symbole(knetto(kEGr))%s,' !', cr, &
                ' Facteur Flinear=0! S''il vous plaît, corrigez!'
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp,mtype=GTK_MESSAGE_WARNING)

            IF(langg == 'DE') call WrStatusBar(4, 'Fehler in Gleichungen beheben!')
            IF(langg == 'EN') call WrStatusBar(4, 'Eliminate error(s) in equations!')
            IF(langg == 'FR') call WrStatusBar(4, 'Élimine les erreurs dans les équations!')
            call WDNotebookSetCurrPage('notebook1',2)
            ifehl = 1
!             write(66,*) 'RW2_392; Return because of ifehl=1'
            call logger(66, 'RW2_392; Return because of ifehl=1')
            goto 9000
        end if
        IF(Flinear < zero .AND. kbrutto(kEGR) > 0) then
            IF(LEN_TRIM(SDFormel(kbrutto(kEGr))%s) > 0 ) THEN
                IF(langg == 'DE') WRITE(str1,*) 'Warnung: Der Sensitivitätskoeffizient des selektierte Symbols ',cr, &
                    ' der Nettozählrate, ',symbole(knetto(kEGr))%s,', ist negativ!', cr, &
                    ' Bitte das passende Symbol selektieren!'
                IF(langg == 'EN') WRITE(str1,*) 'Warning: The coefficient of sensivity of the selected ', cr, &
                    ' net count rate symbol, ', symbole(knetto(kEGr))%s,', is negative!', cr, &
                    ' Please, select the appropriate symbol!'
                IF(langg == 'FR') WRITE(str1,*) 'Attention: Le coefficient de sensibilité de la sélection ', cr, &
                    ' symbole de taux de comptage net, ', symbole(knetto(kEGr))%s,', est negative!', cr, &
                    ' S''il vous plaît, sélectionnez le symbole approprié!'
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp,mtype=GTK_MESSAGE_WARNING)
                IF(langg == 'DE') call WrStatusBar(4, trim('Richtiges Symbol der Nettozählrate auswählen!'))
                IF(langg == 'EN') call WrStatusBar(4, trim('Select the correct net counting rate symbol!'))
                IF(langg == 'FR') call WrStatusBar(4, trim('Sélectionnez le symbole de taux de comptage net correct!'))
                call WDNotebookSetCurrPage('notebook1',2)
                ifehl = 1
                goto 9000
            end if
        end if
        IF(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit .and. kbrutto(kEGR) > 0) then
            IF(LEN_TRIM(SDFormel(kbrutto(kEGr))%s) > 0 ) THEN
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
                IF(dpi < zero .and. .not. gross_negative .and. FakRB > zero) THEN
                    IF(langg == 'DE') WRITE(str1,*) 'Warnung: Der Sensitivitätskoeffizient des selektierten Symbols ',cr, &
                        ' der Bruttozählrate, ',symbole(kbrutto(kEGr))%s,', ist negativ!', cr,cr, &
                        ' Bitte das passende Symbol selektieren!'
                    IF(langg == 'EN') WRITE(str1,*) 'Warning: The coefficient of sensivity of the selected ', cr, &
                        ' gross count rate symbol, ', symbole(knetto(kEGr))%s,', is negative!', cr,cr, &
                        ' Please, select the appropriate symbol!'
                    IF(langg == 'FR') WRITE(str1,*) 'Attention: Le coefficient de sensibilité de la sélection ', cr, &
                        ' symbole de taux de brut compte, ', symbole(knetto(kEGr))%s,', est negative!', cr,cr, &
                        ' S''il vous plaît, sélectionnez le symbole approprié!'
                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp,mtype=GTK_MESSAGE_WARNING)
                    IF(langg == 'DE') call WrStatusBar(4, trim('Richtiges Symbol der Bruttozählrate auswählen!'))
                    IF(langg == 'EN') call WrStatusBar(4, trim('Select the correct gross counting rate symbol!'))
                    IF(langg == 'FR') call WrStatusBar(4, trim('Sélectionnez le symbole de grand taux de comptage correct!'))
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

        IF( .not.FitDecay .and. .not.Gamspk1_Fit .and. kbrutto(kEGR) > 0 .and. .not.gross_negative) then
            if(LEN_TRIM(SDFormel(kbrutto(kEGr))%s) > 0 .And. FakRb*Messwert(kbrutto(kEGr)) < Messwert(knetto(kEGr))  &
                .and. Fakrb > zero ) THEN

                IF(langg == 'DE') WRITE(str1,*) 'Warnung: Die Bruttozählrate ist kleiner als die Nettozählrate!',cr, &
                    ' Bitte korrigieren oder andere Symbole dafür selektieren!'
                IF(langg == 'EN') WRITE(str1,*) 'Warning: The gross count rate is smaller then the net counting rate!', cr, &
                    ' Please, correct or select other symbols!'
                IF(langg == 'FR') WRITE(str1,*) 'Avertissement: Le taux de gros compte est plus bas que le taux de comptage net!', cr, &
                    ' Veuillez corriger ou sélectionner d''autres symboles!'
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp, mtype=GTK_MESSAGE_WARNING)
                IF(langg == 'DE') call WrStatusBar(4, trim('Problem Brutto-/Nettozählrate beheben!'))
                IF(langg == 'EN') call WrStatusBar(4, trim('Remove problem with gross/net counting rates!'))
                IF(langg == 'FR') call WrStatusBar(4, trim('Supprimez le problème avec les taux de comptage brut/net!'))
                ifehl = 1
                goto 9000
            end if
        end if

!++++++++++++++++++++++++++++++++
        If(.not.Gamspk1_Fit) then
            RD = RnetVal(Messwert(kEGr))
!             write(66,*) 'RnetVal:   value=',sngl(RD),' mw(kEGr)=',sngl(Messwert(kEGr)),' ifehl=',ifehl
            write(log_str, '(*(g0))') 'RnetVal:   value=',sngl(RD),' mw(kEGr)=',sngl(Messwert(kEGr)),' ifehl=',ifehl
            call logger(66, log_str)
            if(ifehl == 1) then
!                 write(66,*) 'RnetVal:   value=',sngl(RD),' mw(kEGr)=',sngl(Messwert(kEGr)),' ifehl=',ifehl
                write(log_str, '(*(g0))') 'RnetVal:   value=',sngl(RD),' mw(kEGr)=',sngl(Messwert(kEGr)),' ifehl=',ifehl
                call logger(66, log_str)
                goto 9000
            end if
        end if
!++++++++++++++++++++++++++++++++

        if(FitDecay .and. kPMLE == 1 .and. ifit(2) == 1 .and. mfrbg == 3) then
            dummy = 1.E+10_rn     !  17.6.2024
            if(fpa(2) < zero .and. fpa(mfrbg) > zero) then
                dummy = abs( abs(fpa(2)) - abs(fpa(mfrbg)) ) / abs(fpa(mfrbg))
            end if
            if(fpa(2) > zero .and. fpa(mfrbg) < zero) then
                dummy = abs( abs(fpa(2)) - abs(fpa(mfrbg)) ) / abs(fpa(mfrbg))
            end if
            if(dummy > zero .and. dummy < 3.E-03_rn) then
                IF(langg == 'DE') WRITE(str1,*) 'Warnung: Der 2. Fit-Parameter konkurriert offenbar mit dem dritten!',cr, &
                    ' Ggf. die Fit-Option des 2. Fit-Parameters auf "weglassen" setzen!'
                IF(langg == 'EN') WRITE(str1,*) 'Warning: The 2nd Fit parameter apparently comptes against the third!', cr, &
                    ' Try to to set the fitting option of the 2nd parameter to "omit"!'
                IF(langg == 'FR') WRITE(str1,*) 'Attention: le 2ème paramètre d''ajustement apparaît compte contre le troisième!', cr, &
                    ' Essayez de définir l''option d''ajustement du 2ème paramètre sur "omettre"!'
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp,mtype=GTK_MESSAGE_WARNING)
                IF(langg == 'DE') call WrStatusBar(4, trim('Problem Brutto-/Nettozählrate beheben!'))
                IF(langg == 'EN') call WrStatusBar(4, trim('Remove problem with gross/net counting rates!'))
                IF(langg == 'FR') call WrStatusBar(4, trim('Supprimez le problème avec les taux de comptage brut/net!'))
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
!         write(66,*) 'Resultat=',sngl(Resultat),' Ucomb=',sngl(Ucomb)
        write(log_str, '(*(g0))') 'Resultat=',sngl(Resultat),' Ucomb=',sngl(Ucomb)
        call logger(66, log_str)

        if(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not.FitCalCurve .and. .not.SumEval_fit .and..not.Gum_restricted) then
!             write(66,'(4(a,es12.5))') 'RW2_5241: mw(knetto)=',Messwert(knetto(kEGr)),' mw(kbrutto)=',Messwert(kbrutto(kEGr)), &
!                 ' stdUnc(knetto)=',StdUnc(knetto(kEGr)),' StdUnc(kbrutto)=',StdUnc(kbrutto(kEGr))
            write(log_str, '(4(a,es12.5))') 'RW2_5241: mw(knetto)=',Messwert(knetto(kEGr)),' mw(kbrutto)=',Messwert(kbrutto(kEGr)), &
                ' stdUnc(knetto)=',StdUnc(knetto(kEGr)),' StdUnc(kbrutto)=',StdUnc(kbrutto(kEGr))
            call logger(66, log_str)

            var_rbtot = StdUnc(knetto(kEGr))**two - FakRB**two * StdUnc(kbrutto(kEGr))**two
!             write(66,*) 'var_rbtot=',sngl(var_rbtot),'  nvars_in_rbtot=',nvars_in_rbtot
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
!                     write(64,'(a,a,T50,a)') 'sdformel=',sdformel(kbrutto(kEGr))%s,trim(fname)
                    write(log_str, '(a,a,T50,a)') 'sdformel=',sdformel(kbrutto(kEGr))%s,trim(fname)
                    call logger(64, log_str)
                end if
            end if
        end if

        call WTreeViewPutDoubleArray('treeview2',5,ngrs+ncov+numd ,Messwert)
        call WTreeViewPutDoubleArray('treeview2',11,ngrs+ncov+numd ,StdUnc)
!         write(66,*) '  '
        call logger(66, '  ')

        if(ngrs+ncov+numd > ubound(Messwert_CP,dim=1)) then
            call RealModA1(Messwert_CP,ngrs+ncov+numd,1)
            call RealModA1(StdUnc_CP,ngrs+ncov+numd,1)
        end if
        Messwert_CP(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)
        StdUnc_CP(1:ngrs+ncov+numd) = StdUnc(1:ngrs+ncov+numd)
        !do i=1,ngrs+ncov+numd
        !  Messwert_CP(i) = Messwert(i)
        !  StdUnc_CP(i) = StdUnc(i)
        !end do

!         write(66,*)
        call logger(66, ' ')
!         write(66,*) '****************  at this point the results exist for ' &
!             // 'output quantity and its associated uncertainty *******'
        write(log_str, '(*(g0))') '****************  at this point the results exist for ' &
            // 'output quantity and its associated uncertainty *******'
        call logger(66, log_str)
!         write(66,*)
        call logger(66, ' ')

        if(.false. .and. use_WTLS) then
!             write(23,*)
            call logger(23, ' ')
!             write(23,*) 'fname=',trim(fname)
            write(log_str, '(*(g0))') 'fname=',trim(fname)
            call logger(23, log_str)
!             write(23,*) '****************  at this point the results exist for ' &
!                 // 'output quantity and its associated uncertainty *******'
            write(log_str, '(*(g0))') '****************  at this point the results exist for ' &
                // 'output quantity and its associated uncertainty *******'
            call logger(23, log_str)

!             write(23,*)
            call logger(23, ' ')
        end if

        if(GamDist_zr) then
            if(iptr_cnt(kbrutto(kEGr)) > 0) &
!                 write(66,*) 'iptr_cnt(kbrutto(kEGr))=',int(iptr_cnt(kbrutto(kEGr)),2), &
!                 ' iptr_time(iptr_cnt(kbrutto(kEGr)))=', &
!                 int(iptr_time(iptr_cnt(kbrutto(kEGr))),2)
                write(log_str, '(*(g0))') 'iptr_cnt(kbrutto(kEGr))=',int(iptr_cnt(kbrutto(kEGr)),2), &
                ' iptr_time(iptr_cnt(kbrutto(kEGr)))=', &
                int(iptr_time(iptr_cnt(kbrutto(kEGr))),2)
                call logger(66, log_str)

            if(iptr_cnt(kbrutto(kEGr)) > 0) tgross = Messwert(iptr_time( iptr_cnt(kbrutto(kEGr))))
            do i=nab+1,ngrs
                if(i == iptr_cnt(kbrutto(kEGr))) cycle
                if(iptr_time(i) > 0) tback = Messwert(iptr_time(i))
            end do
!             write(66,*) 'RW2:  tgross=',sngl(tgross),'  tback=',sngl(tback)
            write(log_str, '(*(g0))') 'RW2:  tgross=',sngl(tgross),'  tback=',sngl(tback)
            call logger(66, log_str)
        end if

        if(FitDecay .and. ifit(2) < 3) then
!             WRITE(66,*) 'before Bayes: Resultat, Ucomb=',sngl(resultat),sngl(Ucomb), &
!                 '   Corr(1,2)=',sngl(covar(1,2)/sqrt(covar(1,1)*covar(2,2))),' cov(1,2)=',sngl(covar(1,2))
            write(log_str, '(*(g0))') 'before Bayes: Resultat, Ucomb=',sngl(resultat),sngl(Ucomb), &
                '   Corr(1,2)=',sngl(covar(1,2)/sqrt(covar(1,1)*covar(2,2))),' cov(1,2)=',sngl(covar(1,2))
            call logger(66, log_str)
        else
!             WRITE(66,*) 'before Bayes: Resultat, Ucomb=',sngl(resultat),sngl(Ucomb)
            write(log_str, '(*(g0))') 'before Bayes: Resultat, Ucomb=',sngl(resultat),sngl(Ucomb)
            call logger(66, log_str)
        end if

!  Calculate the values concerning the best Bayesian estimate:
        if(kModelType /= 2) then
            omega =  pnorm(Resultat/(ucomb/coverf))
            WertBayes = resultat + ( (ucomb/coverf) *   &
                EXP( max(-450._rn, -resultat**two/(two*(ucomb/coverf)**two) ) ) / (omega*SQRT(two*Pi) ) )

            UcombBayes = SQRT( (ucomb/coverf)**two - (WertBayes - Resultat)*WertBayes )
            UcombBayes = UcombBayes * coverf

            xkp = qnorm(omega*(one - (one-W1minusg)/two))
            xkq = qnorm(one - omega*(one-W1minusg)/two)
            KBgrenzu = Resultat - xkp * (ucomb/coverf)
            KBgrenzo = Resultat + xkq * (ucomb/coverf)
!             WRITE(66,*) 'omega=',sngl(omega),' WertBayes=',sngl(WertBayes),' UcombBayes=',sngl(UcombBayes)
            write(log_str, '(*(g0))') 'omega=',sngl(omega),' WertBayes=',sngl(WertBayes),' UcombBayes=',sngl(UcombBayes)
            call logger(66, log_str)


            ! Shortest coverage interval:
            xkp = qnorm(0.5_rn*(one + omega*W1minusg))
            xkq = qnorm(one - omega*(one-W1minusg))
            KBgrenzuSH = Resultat - xkp * (ucomb/coverf)
            KBgrenzoSH = Resultat + xkp * (ucomb/coverf)
            if(.not. Gum_restricted .and. KBgrenzuSH < zero) then
                KBgrenzuSH = max(zero, KBgrenzuSH)
                KBgrenzoSH = Resultat + xkq * (ucomb/coverf)
            end if

!             write(66,'(a)') 'coverage intervals:     shortest              symmetric'
            call logger(66, 'coverage intervals:     shortest              symmetric')
!             WRITE(66,'(a,es12.5,10x,es12.5)') '         lower limit : ', &
!                 real(KBgrenzuSH,8),real(KBgrenzu,8)
            write(log_str, '(a,es12.5,10x,es12.5)') '         lower limit : ', &
                real(KBgrenzuSH,8),real(KBgrenzu,8)
            call logger(66, log_str)
!             WRITE(66,'(a,es12.5,10x,es12.5)') '         upper limit : ', &
!                 real(KBgrenzoSH,8),real(KBgrenzo,8)
            write(log_str, '(a,es12.5,10x,es12.5)') '         upper limit : ', &
                real(KBgrenzoSH,8),real(KBgrenzo,8)
            call logger(66, log_str)
!             WRITE(66,'(a,es12.5,10x,es12.5)') '         interval    : ', &
!                 real(KBgrenzoSH-KBgrenzuSH,8),real(KBgrenzo-KBgrenzu,8)
            write(log_str, '(a,es12.5,10x,es12.5)') '         interval    : ', &
                real(KBgrenzoSH-KBgrenzuSH,8),real(KBgrenzo-KBgrenzu,8)
            call logger(66, log_str)
        else
            WertBayes = zero
            UcombBayes = zero
            KBgrenzu = Resultat + qnorm( (one - W1minusG)/two ) * (ucomb/coverf)
            KBgrenzo = Resultat + qnorm( (one + W1minusG)/two ) * (ucomb/coverf)
!             WRITE(66,*) 'KBgrenzu, KBgrenzo=',sngl(KBgrenzu), sngl(KBgrenzo)
            write(log_str, '(*(g0))') 'KBgrenzu, KBgrenzo=',sngl(KBgrenzu), sngl(KBgrenzo)
            call logger(66, log_str)
        end if

        if(.not.loadingPro .and. .not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit .and.  &
            kbrutto_gl(kEGr) == 0 .and. kModelType == 1 .and. .not.var_brutto_auto ) then
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

!         write(66,*) 'Rw2:  gum_restricted=', Gum_restricted,'   multi_eval=',multi_eval
        write(log_str, '(*(g0))') 'Rw2:  gum_restricted=', Gum_restricted,'   multi_eval=',multi_eval
        call logger(66, log_str)

        if(.not.gum_restricted .and. .not.multi_eval) then

            call logger(30, "Project:  " // trim(fname))

!             IF(FitDecay) WRITE(66,*) 'Begin of Detlim calculations    Fitmeth=',TRIM(fitmeth)
            IF(FitDecay)  then
                write(log_str, '(*(g0))') 'Begin of Detlim calculations    Fitmeth=',TRIM(fitmeth)
                call logger(66, log_str)
            end if
!             IF(FitDecay) WRITE(30,*) 'Begin of Detlim calculations    Fitmeth=',TRIM(fitmeth)
            IF(FitDecay)  then
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

!         WRITE(30,'(/,1x,a,i0)') 'iterative calculation of decision and detection limit: output quantity:',kEGr
        write(log_str, '(1X,A,i0)') 'iterative calculation of decision and detection limit: output quantity:',kEGr
        call logger(30, log_str)

        IF(kbrutto_gl(kEGr) == 0 .and. .not.var_brutto_auto &
            .AND. .not.FitDecay .AND. .not.Gamspk1_Fit .and. .not.SumEval_fit) THEN
            IF(.NOT.loadingPro) THEN
                IF(langg == 'DE') str1 = 'Achtung: die StdAbw-Formel für die Brutto-zählrate ' // cr &
                    // 'ist noch nicht definert!' // cr //'NWG-Berechnung nicht möglich!'
                IF(langg == 'EN') str1 = 'Warning: the StdDev formula of the gross count rate ' // cr &
                    // 'has not yet been defined!' // cr //'Calculation of detection limits not possible!'
                IF(langg == 'FR') str1 = 'Attention: la formule d''écart-type du brut compte ' // cr &
                    // 'n''a pas encore été défini!' // cr //'Calcul des limites de détection impossible!'
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp,mtype=GTK_MESSAGE_WARNING)
            end if
            goto 9000
        END IF

        call RbtCalc(RblTot)
        StdUnc(1:ngrs+ncov+numd) = StdUncSV(1:ngrs+ncov+numd)

!         WRITE(66,*) 'RblTot(kEgr)=',sngl(RblTot(kEGr)),'   FakRB=',sngl(FakRB)
        write(log_str, '(*(g0))') 'RblTot(kEgr)=',sngl(RblTot(kEGr)),'   FakRB=',sngl(FakRB)
        call logger(66, log_str)
        IF(kbrutto(kEGr) > 0) then
!             WRITE(66,*) '       Messwert(kbrutto(kEGr))=',sngl(Messwert(kbrutto(kEGr))), &
!                 '  Stdunc=',sngl(StdUnc(kbrutto(kEGr)))
            write(log_str, '(*(g0))') '       Messwert(kbrutto(kEGr))=',sngl(Messwert(kbrutto(kEGr))), &
                '  Stdunc=',sngl(StdUnc(kbrutto(kEGr)))
            call logger(66, log_str)
        end if
        IF(knetto(kEGr) > 0) then
!             WRITE(66,*) '       Messwert(knetto(kEGr)) =',sngl(Messwert(knetto(kEGr))), &
!                 '  Stdunc=',sngl(StdUnc(knetto(kEGr)))
            write(log_str, '(*(g0))') '       Messwert(knetto(kEGr)) =',sngl(Messwert(knetto(kEGr))), &
                '  Stdunc=',sngl(StdUnc(knetto(kEGr)))
            call logger(66, log_str)
        end if

!-------------------------------------------
! Calculate the relative standard uncertainty of the calibration factor w:
        ! if(Fconst /= zero) write(169,*) 'Fconst /= 0: File=',trim(fname)
! 13.9.2023:
! determine uncertainty of Flinear:
        Fv1 = func_Flinear(Messwert,ngrs)
        varw = zero
        do k=1,nRSsy(kEGr)
            i = RS_SymbolNr(kEGr,k)
            if(abs(StdUnc(i)) < 1.e-12_rn .or. abs(StdUnc(i)-missingval) < 1.e-12_rn) cycle
            dpa = Messwert(i)*dpafact(Messwert(i)) - Messwert(i)
            Messwert(i) = Messwert(i) + dpa
            Fv2 = func_Flinear(Messwert,ngrs)
            Messwert(i) = Messwert(i) - dpa
            dpi = (Fv2 - Fv1)/dpa
            if(abs(dpi) > zero) varw = varw + (dpi*StdUnc(i))**two
        end do
        urelw = sqrt(varw)/Fv1
        ! write(66,*)  'Calculated urelw: ',sngl(urelw)
        if(urelw > zero .and. abs(uFlinear) < eps1min) uFlinear = urelw*Flinear


        !  write(66,'(3(a,es11.4),a,i0,4(a,es11.4))') 'w=',w,' urel(w)=',urelw,' uFc=',uFc,' klu=',klu, &
        !       ' urelw1=',sqrt(urelw1),' urelw2=',sqrt(urelw2) ,' urelw=',urelw,' StdUnc(kEGr)=',StdUnc(kEGr)
!         write(66,'(3(a,es11.4))') 'w=',Fv1,' urel(w)=',urelw, &
!             ' StdUnc(kEGr)=',StdUnc(kEGr)
        write(log_str, '(3(a,es11.4))') 'w=',Fv1,' urel(w)=',urelw, &
            ' StdUnc(kEGr)=',StdUnc(kEGr)
        call logger(66, log_str)

! if(.not.use_WTLS .and. urelw >= one/kbeta .and. .not.bat_serial .and. .not.batf .and. .not.batest_user) then
        if(.not.use_WTLS .and. urelw >= one/kbeta*0.98_rn .and. .not.bat_serial .and. .not.batf) then
            IF(langg == 'DE') WRITE(str1,*) 'Warnung: Die relative Unsicherheit des Kalibrierfaktors w ist >= 1/k_1-beta!',cr, &
                ' Daher kann die Nachweisgrenze nicht berechnet werden!'
            IF(langg == 'EN') WRITE(str1,*) 'Warning: The relative calibration factor w uncertainty is >= 1/k_1-beta!', cr, &
                ' Therefore,the detection limit cannot be calculated!'
            IF(langg == 'FR') WRITE(str1,*) 'Attention: Le facteur d''étalonnage relatif w incertitude est >= 1/k_1-beta!', cr, &
                ' Par conséquent, la limite de détection ne peut pas être calculée!'
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp, mtype=GTK_MESSAGE_WARNING)
        end if
!-------------------------------------------

!         IF(FitDecay) WRITE(66,*) ' Rechw2, before DecThresh calculation: klincall=',klincall
        IF(FitDecay)  then
            write(log_str, '(*(g0))') ' Rechw2, before DecThresh calculation: klincall=',klincall
            call logger(66, log_str)
        end if
!         IF(FitDecay) WRITE(30,*) ' Rechw2, before DecThresh calculation:  MW(klu)=',sngl(Messwert(klu)), &
!             ' StdUnc(klu)=',sngl(StdUnc(klu))
        IF(FitDecay)  then
            write(log_str, '(*(g0))') ' Rechw2, before DecThresh calculation:  MW(klu)=',sngl(Messwert(klu)), &
            ' StdUnc(klu)=',sngl(StdUnc(klu))
            call logger(30, log_str)
        end if
!         WRITE(66,'(a,i0,2(a,L1),a,i0)') 'before Detlim_iter: kEGr=',kEGr,'  Gum_restricted=',GUM_restricted, &
!             ' var_brutto_auto=',var_brutto_auto,' k_autoform=',k_autoform
        write(log_str, '(a,i0,2(a,L1),a,i0)') 'before Detlim_iter: kEGr=',kEGr,'  Gum_restricted=',GUM_restricted, &
            ' var_brutto_auto=',var_brutto_auto,' k_autoform=',k_autoform
        call logger(66, log_str)

        do itest=1,2
            if(.not.LinTest .and. itest == 2) cycle

            decthresh = zero
            detlim = zero

            verfahren = 'ISO 11929:2019'

            call logger(30, 'procedure : ' // verfahren)
            dummy = zero

            ! Decision limit:
!             WRITE(66,*) '-- Begin of iteration DT'
            call logger(66, '-- Begin of iteration DT')
            increase_dpafact = .false.
            if(LinTest) then
                if(itest == 1) increase_dpafact = .true.
                if(itest == 2) increase_dpafact = .false.
            end if

            nvar = kbrutto(kEGr)
            MesswertSV_nvar = zero
            MesswertSV_icnvar = zero

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
!             write(30,*)
            call logger(30, ' ')
            if(LinTest .and. itest == 1) DT_increase = decthresh

            ! write(66,'(a,es12.5,a,i0)') 'after DT:  DT=',decthresh,' ifehl=',ifehl
            IF(ifehl == 1) goto 20

!             IF(FitDecay) WRITE(66,'(a,i0,a,es12.5,a,3es13.5)') ' Rechw2, after DT calculation: klincall=',klincall, &
!                 ' EKG=',decthresh,' fpa=',(sngl(fpa(i)),i=1,ma)
            IF(FitDecay)  then
                write(log_str, '(a,i0,a,es12.5,a,3es13.5)') ' Rechw2, after DT calculation: klincall=',klincall, &
                ' EKG=',decthresh,' fpa=',(sngl(fpa(i)),i=1,ma)
                call logger(66, log_str)
            end if
            ! if(urelw >= one/kbeta) then
            if(.not.use_WTLS .and. urelw >= one/kbeta) then         ! 11.7.2023
                detlim = zero
                nit_detl = 0
            else
                ! Detection limit:
!                 WRITE(66,'(a,es12.5)') '-- Begin of iteration DL:   decthresh=',sngl(decthresh)
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
                IF(ifehl == 1) goto 20
                if(LinTest .and. itest == 1) DL_increase = detlim

!                 write(66,'(a,es12.5,a,i0)') 'directly after DL:  detlim=',detlim,' ifehl=',ifehl
                write(log_str, '(a,es12.5,a,i0)') 'directly after DL:  detlim=',detlim,' ifehl=',ifehl
                call logger(66, log_str)

!                 IF(FitDecay) WRITE(66,'(a,i0,a,es12.5,a,3es13.5)') ' Rechw2, after DL calculation: klincall=',klincall, &
!                     ' DL=',detlim,' fpa=',(sngl(fpa(i)),i=1,ma)
                IF(FitDecay)  then
                    write(log_str, '(a,i0,a,es12.5,a,3es13.5)') ' Rechw2, after DL calculation: klincall=',klincall, &
                    ' DL=',detlim,' fpa=',(sngl(fpa(i)),i=1,ma)
                    call logger(66, log_str)
                end if

!                 write(30,'(/,1x,a,es11.4,2x,a,3x,i2,1x,a)')  'Decision thresh. = ', &
!                     real(decthresh,8),'Bq/cm2',nit_decl,'Iterations'
                write(log_str, '(1x,a,es11.4,2x,a,3x,i2,1x,a)') 'Decision thresh. = ', &
                    real(decthresh,8),'Bq/cm2',nit_decl,'Iterations'
                call logger(30, log_str)
!                 write(66,'(/,1x,a,es11.4,2x,a,3x,i2,1x,a)')  'Decision thresh. = ', &
!                     real(decthresh,8),'Bq/cm2',nit_decl,'Iterations'
                write(log_str, '(1x,a,es11.4,2x,a,3x,i2,1x,a)') 'Decision thresh. = ', &
                    real(decthresh,8),'Bq/cm2',nit_decl,'Iterations'
                call logger(66, log_str)

!                 write(30,'(  1x,a,es11.4,2x,a,3x,i2,1x,a)')  'Detection limit  = ', &
!                     real(detlim,8),'Bq/cm2',nit_detl,'Iterations'
                write(log_str, '(  1x,a,es11.4,2x,a,3x,i2,1x,a)') 'Detection limit  = ', &
                    real(detlim,8),'Bq/cm2',nit_detl,'Iterations'
                call logger(30, log_str)
!                 write(66,'(  1x,a,es11.4,2x,a,3x,i2,1x,a)')  'Detection limit  = ', &
!                     real(detlim,8),'Bq/cm2',nit_detl,'Iterations'
                write(log_str, '(  1x,a,es11.4,2x,a,3x,i2,1x,a)') 'Detection limit  = ', &
                    real(detlim,8),'Bq/cm2',nit_detl,'Iterations'
                call logger(66, log_str)
            end if

        end do     ! itest

20      continue
        limit_typ = 0

        iteration_on = .FALSE.
        kqtyp = 1  ! 9.6.2024
!         if(ifehl == 1) write(66,*) 'RW2: after 20: ifehl = 1 !'
        if(ifehl == 1)  then
            write(log_str, '(*(g0))') 'RW2: after 20: ifehl = 1 !'
            call logger(66, log_str)
        end if
        if(ifehl == 1) then
            if(langg == 'DE') call WrStatusBar(4,'Abbruch.')
            if(langg == 'EN') call WrStatusBar(4,'Abortion.')
            if(langg == 'FR') call WrStatusBar(4,'Avortement.')
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
!             write(159,'(a)') trim(str1)
            call logger(159, trim(str1))
        end if

        if(FitDecay) then
!             write(66,*) 'Limit_typ: ',limit_typ,'  fpa=',(sngl(fpa(i)),i=1,3),'   sfpa=',(sngl(sfpa(i)),i=1,3)
            write(log_str, '(*(g0))') 'Limit_typ: ',limit_typ,'  fpa=',(sngl(fpa(i)),i=1,3),'   sfpa=',(sngl(sfpa(i)),i=1,3)
            call logger(66, log_str)
            do i=1,3
!                 write(66,'(a,i1,2(a,1x,3es11.4,2x),2x,a,es11.4)') 'kqtyp=',i,'  fpaLYT=',(sngl(fpaLYT(i,k)),k=1,3), &
!                     '  sfpaLYT=',(sngl(sfpaLYT(i,k)),k=1,3),' covarLYT(i)=',sngl(covarLYT(i))
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
!         IF(FitDecay) WRITE(66,*) ' Rechw2: shortly before its end:  klincall=',int(klincall,2),' ifehl=',int(ifehl,2)
        IF(FitDecay)  then
            write(log_str, '(*(g0))') ' Rechw2: shortly before its end:  klincall=',int(klincall,2),' ifehl=',int(ifehl,2)
            call logger(66, log_str)
        end if

        IF(FitDecay) THEN
            iterat_passed = .TRUE.        ! ensures that an additional uncertainty calculations
            ! is recalculated in Lincov2!
            klincall = 0                  ! without klincall = 0 the following did not work
            IF(ifehl == 1) goto 9000
            iterat_passed = .FALSE.
        end if

        SDakt = zero
        IF(Gamspk1_Fit) call Linfg1ausf(1,akt,SDakt)    ! restore activity value

!         WRITE(66,'(a,i0,3(a,es11.4),a,i0)') 'End of Rechw2: kEGr=',kEGr,' Resultat=',resultat,  &
!             ' ucomb=',ucomb,' SDakt=',SDakt,' ifehl=',int(ifehl,2)
        write(log_str, '(a,i0,3(a,es11.4),a,i0)') 'End of Rechw2: kEGr=',kEGr,' Resultat=',resultat,  &
            ' ucomb=',ucomb,' SDakt=',SDakt,' ifehl=',int(ifehl,2)
        call logger(66, log_str)
!         if(FitDecay) WRITE(66,*) 'End of Rechw2: fpa=',(sngl(fpa(i)),i=1,3)
        if(FitDecay)  then
            write(log_str, '(*(g0))') 'End of Rechw2: fpa=',(sngl(fpa(i)),i=1,3)
            call logger(66, log_str)
        end if
!         WRITE(30,*) 'End of Rechw2: Resultat=',sngl(resultat),'  ucomb=',sngl(ucomb), &
!             ' ifehl=',int(ifehl,2)
        write(log_str, '(*(g0))') 'End of Rechw2: Resultat=',sngl(resultat),'  ucomb=',sngl(ucomb), &
            ' ifehl=',int(ifehl,2)
        call logger(30, log_str)

        if(FitDecay) then
!             write(66,*) 'Cauchy-Schwarz-Test for covpp    : ',cauchy_failed1
            write(log_str, '(*(g0))') 'Cauchy-Schwarz-Test for covpp    : ',cauchy_failed1
            call logger(66, log_str)
!             write(66,*) 'Cauchy-Schwarz-Test for Findcovx : ',cauchy_failed2
            write(log_str, '(*(g0))') 'Cauchy-Schwarz-Test for Findcovx : ',cauchy_failed2
            call logger(66, log_str)
!             write(66,*) 'Cauchy-Schwarz-Test at  E7       : ',cauchy_failed3
            write(log_str, '(*(g0))') 'Cauchy-Schwarz-Test at  E7       : ',cauchy_failed3
            call logger(66, log_str)
        end if

9000    continue

!         write(66,'(a,5(i0,1x))') 'kbgv_binom,itm_binom,ip_binom,ilam_binom=',kbgv_binom,itm_binom,ip_binom,ilam_binom
        write(log_str, '(a,5(i0,1x))') 'kbgv_binom,itm_binom,ip_binom,ilam_binom=',kbgv_binom,itm_binom,ip_binom,ilam_binom
        call logger(66, log_str)
!         write(66,*) 'use_bipoi=',use_bipoi
        write(log_str, '(*(g0))') 'use_bipoi=',use_bipoi
        call logger(66, log_str)
        if(allocated(Bmat)) deallocate(Bmat,dvec,bvec,Uxinv)
        if(allocated(Uxinv)) deallocate(Uxinv)
        if(allocated(iact)) deallocate(iact)
        if(allocated(iter)) deallocate(iter)

!         write(66,*) '############################# Rechw2 End ############################'
        call logger(66, '############################# Rechw2 End ############################')
        if(consoleout_gtk) write(0,*) '##### Rechw2 End ############################'


        ! Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)
        ! iteration_on = .true.
        ! limit_typ = 2
        ! write(66,*)


    end subroutine Rechw2

!#######################################################################

    module subroutine detlim_iter(DTxx,newvalue,it)

        !  calculates the values of the decision thteshold (DT) and the
        !  detection limit(DL) according to ISO 11929:2019;
        !  The limit_typ variable is set before calling detlim_iter
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        USE UR_Gleich
        USE UR_Linft
        use Lf1,               only: Linfout
        USE fparser,           ONLY: evalf, EvalErrMsg
        USE UR_Perror
        USE UR_DLIM
        USE UR_Gspk1Fit
        USE UR_Variables,      ONLY: langg
        use gtk,               only: gtk_buttons_ok,GTK_MESSAGE_WARNING
        use Rout,              only: MessageShow
        use Top,               only: WrStatusbar
        use UWB,               only: Resulta,upropa

        use file_io,           only: logger
        use UR_MCC,            only: kqtypx

        implicit none

        real(rn), INTENT(IN)        :: DTxx      ! needed only for limit_typ = 2 (DL iteration)
        real(rn), INTENT(OUT)       :: newvalue  ! calculated value of DT or DL
        integer(4), INTENT(OUT)     :: it        ! number of iterations

        real(rn)            :: act,RD
        real(rn)            :: x1,x2,xacc
        real(rn)            :: oldvalue,RD_old
        real(rn)            :: fpaSVur(3)
        real(rn)            :: xcorr,maxrelu
        real(rn)            :: ratmin,ratmin2 , varFL,brentx
        integer(4)          :: i,ifitDL(3),klu,resp,ism,mode
        CHARACTER(LEN=6)    :: vname
        character(len=512)           :: log_str
        CHARACTER(LEN=300)  :: str1
!-----------------------------------------------------------------------

        klu = klinf
        if(klinf == 0) klu = knetto(kEGr)
        IF(Gamspk1_Fit) klu = kgspk1
        IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1

        IF(FitDecay) ifitDL = ifit

        ifehl = 0
        IF(FitDecay) fpaSVur = fpaSV     ! extended to 3 parameters

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
                if(abs(Messwert(kEgr)-Fconst) <= eps1min) then
                    varFL = 1.E-20_rn
                else
                    varFL = (StdUnc(kEGr)**two + uFC**two)/(Messwert(kEGr) - Fconst)**two  &
                        + (StdUnc(knetto(kEGr))/Messwert(knetto(kEGr)))**two
                    varFL = min(varFL, 0.5_rn)
                end if
                newvalue = DTxx * ((kalpha+kbeta)/kalpha)**(one + sqrt(varFL))
            else
                newvalue = newvalue * two
            end if
        end select

        IF(limit_typ == 1) then
            ! values (close to zero) of the net count rate:
            !newvalue = 1.E-11_rn     ! 1.E-11_rn
            !  newvalue = 5.E-12_rn      ! 5.11.2020
            newvalue = zero       ! 13.2.2023
            RD = newvalue
        end if

        iteration_on = .true.
!------------------------------------------------------------------------
!::::: iteration loop:
!  for decision threshold :  decthresh = k-alpha * u(decthresh, RD=declim/Kalfactor)
!  for detection limit:  detlim = decthresh + k-beta * u(detlim, RD=decthresh/Kalfactor)
!  (RD: net counting rate of analyte)

!         WRITE(30,'(4(a,es12.5))') 'Begin of iterations: RD=',RD,' Fconst=',Fconst,' Flinear=',Flinear,' DTxx=',DTxx
        write(log_str, '(4(a,es12.5))') 'Begin of iterations: RD=',RD,' Fconst=',Fconst,' Flinear=',Flinear,' DTxx=',DTxx
        call logger(30, log_str)

        if(.true. .and. limit_typ == 2) then   ! replacing .true. by .false. would mean that the DL
            ! iteration is shifted from brentx to the following do loop
            !
            !DL / NWG: iterative search with brentx
            kqtypx = 3
            x1 = DTxx*0.9_rn          ! x1, x2: bracketing values, safely encompassing the detlim value
            x2 = newvalue*3.0_rn       !
            xacc = x1*1.5_rn*1.E-8_rn
            ! if(use_WTLS)  xacc = xacc * 10._rn       ! 4.7.2023
            xacc = xacc * 5._rn
            mode = 1                  !  mode = 1:  this value ist interpreted in the subroutine PrFunc called by brentx
            detlim = brentx(x1,x2,xacc,DTxx,mode)
            if(ifehl == 1) then
!                 write(30,*) 'Detlim_iter: Error within brentx! '
                call logger(30, 'Detlim_iter: Error within brentx! ')
!                 write(66,*) 'Detlim_iter: Error within brentx! '
                call logger(66, 'Detlim_iter: Error within brentx! ')
                return
            end if
            goto 44    ! i.e., the following do loop is skipped
        end if

        do             ! Begin iteration loop (rather long)

            x1 = zero
            x2 = zero

            RD_old = RD
            RD = RnetVal(newvalue)
            if(klu > 0) then
                MEsswert(klu) = RD
            end if

            ! Procedure: ISO 11929: set the "assumed value" as net count rate RD, and its uncertainty
            IF(limit_typ == 1) then
                ! DT / EKG: (u(RD=0) !)
                call ModVar(2, RD)
            else
                ! NWG /DL
                ! actually, the iteration for the DL is done in brentx (see a bit above)
                call ModVar(3, RD)
            END if
            if(nvar > 0) then
!                 write(30,*) 'nach modvar: MW(nvar)=',sngl(Messwert(nvar)),'  StdUnc(nvar)=',sngl(StdUnc(nvar))
                write(log_str, '(*(g0))') 'nach modvar: MW(nvar)=',sngl(Messwert(nvar)),'  StdUnc(nvar)=',sngl(StdUnc(nvar))
                call logger(30, log_str)
            end if

            !  calculate the uncertainty of that output quantity value corresponding to RD:
            if(it == 0) klincall = 0    ! required by Linf/lincov2 for FitDecay
            call upropa(kEGr)        ! berechnet UComb
!             if(nvar > 0) WRITE(30,*)   'Loop:  RD=',sngl(RD),' Ucomb=',sngl(Ucomb),'  it=',int(it,2),' klincall=',int(klincall,2), &
!                 'MW(nvar)=',sngl(Messwert(nvar))
            if(nvar > 0)  then
                write(log_str, '(*(g0))') 'Loop:  RD=',sngl(RD),' Ucomb=',sngl(Ucomb),'  it=',int(it,2),' klincall=',int(klincall,2), &
                'MW(nvar)=',sngl(Messwert(nvar))
                call logger(30, log_str)
            end if
!             if(nvar == 0) WRITE(30,*)   'Loop:  RD=',sngl(RD),' Ucomb=',sngl(Ucomb),'  it=',int(it,2),' klincall=',int(klincall,2)
            if(nvar == 0)  then
                write(log_str, '(*(g0))') 'Loop:  RD=',sngl(RD),' Ucomb=',sngl(Ucomb),'  it=',int(it,2),' klincall=',int(klincall,2)
                call logger(30, log_str)
            end if

            if(.true. .and. it > 2 .and. limit_typ == 2 .and. abs(uFC/Flinear) >= 0.97_rn/kbeta) then
                ifehl = 1
                if(langg == 'DE') write(str1,'(a,a1,a)') &
                    'Problem: die Unsicherheit ist zu groß, so dass die NWG-Iteration misslingt!', &
                    char(13),'Bitte die Unsicherheiten der Eingangsgrößen überprüfen!'
                if(langg == 'EN') write(str1,'(a,a1,a)') &
                    'Problem: the uncertainty is too large, the DetLim iteration will thus fail!', &
                    char(13),'Please, check uncertainties of input quantities!'
                if(langg == 'FR') write(str1,'(a,a1,a)') &
                    'Problème: l''incertitude est trop grande, l''itération DL va donc échouer!', &
                    char(13),'S''il vous plaît, vérifiez les incertitudes des quantités d''entrée!'
                call MessageShow(trim(str1), GTK_BUTTONS_OK, 'DetLim_Iter:', resp,mtype=GTK_MESSAGE_WARNING)
                maxrelu = zero
                ism = 0
                do i=nab+1,ngrs
                    if(FitDecay .and. kfitp(1)> 0 .and. i >= kfitp(1) .and. i <= kfitp(1)+2 ) cycle
                    if(abs(StdUnc(i)-missingval) > eps1min .and. abs(Messwert(i)-zero) > eps1min .and.  &
                        StdUnc(i)/Messwert(i) > maxrelu) then
                        maxrelu = StdUnc(i)/Messwert(i)
                        ism = i
                    end if
                end do
                if(langg == 'DE') write(str1,*) 'Max. Wert der relativen Eingangs-Unsicherheiten:', &
                    char(13),symbole(ism)%s,' :  rel. Unsicherheit=',sngl(maxrelu)
                if(langg == 'EN') write(str1,*) 'Max. value of relative input quantities:', &
                    char(13),symbole(ism)%s,' :  rel. uncertainty=',sngl(maxrelu)
                if(langg == 'FR') write(str1,*) 'Valeur maxim. des quantités d''entrée relatives:', &
                    char(13),symbole(ism)%s,' :  relat. incertitude=',sngl(maxrelu)
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

            IF(nvar > 0) THEN
!                 write(30,67) it,vname,real(newvalue,8), real(Messwert(nvar),8),real(RD,8),  &
!                     real(Messwert(kEGr),8),real(ucomb,8)
                write(log_str, 67) it,vname,real(newvalue,8), real(Messwert(nvar),8),real(RD,8),  &
                    real(Messwert(kEGr),8),real(ucomb,8)
                call logger(30, log_str)
67              format(5x,' Iteration=',i3,':   ',a,'= ',es16.9,'  Rb=',es16.9, &
                    '  RD=',es11.4,'  Value=',es11.4,'  ucomb=',es15.8)
            else
                xcorr = zero
                if(FitDecay) then
                    if(abs(covar(1,1)*covar(2,2)) > eps1min) xcorr = &
                        covar(1,2)/sqrt(covar(1,1)*covar(2,2))
                end if
!                 if(FitDecay) write(30,68) it,vname,real(newvalue,8), real(RD,8),   &
!                     real(Ucomb,8), real(xcorr,8),real(covar(1,2),8)
                if(FitDecay)  then
                    write(log_str, 68) it,vname,real(newvalue,8), real(RD,8),   &
                    real(Ucomb,8), real(xcorr,8),real(covar(1,2),8)
                    call logger(30, log_str)
                end if
!                 if(Gamspk1_Fit) write(30,68) it,vname,real(newvalue,8),real(RD,8),real(Ucomb,8)
                if(Gamspk1_Fit)  then
                    write(log_str, 68) it,vname,real(newvalue,8),real(RD,8),real(Ucomb,8)
                    call logger(30, log_str)
                end if
68              format(5x,' Iteration=',i3,':   ',a,'= ',es16.9,'  RD=',es16.9,'  Ucomb=',es12.5, &
                    '  corr(1,2)=',es11.4,' cov(1,2)=',es11.4 )
            END IF

            IF(it > nit_detl_max) EXIT
            if(it > 200) exit
            IF(limit_typ == 1) EXIT
            ratmin = 0.0000010_rn
            if(rn == 10) ratmin = 0.0000002_rn
            ratmin2 = ratmin * 10._rn
            if( kpmle /= 1 .and. it > 2 .AND. abs(newvalue-oldvalue)/oldvalue < ratmin) EXIT
            if( kpmle == 1 .and. it > 2 .AND. abs(newvalue-oldvalue)/oldvalue < ratmin2) EXIT
            oldvalue = newvalue

        end do      ! End of iteration loop

44      continue

        IF(limit_typ == 1) Ucomb_DTv = Ucomb
        IF(limit_typ == 2) Ucomb_DLv = Ucomb

        if(FitDecay) then
            fpaLYT(1+limit_typ,1:ma) = fpa(1:ma)
            sfpaLYT(1+limit_typ,1:ma) = sfpa(1:ma)
            covarLyt(1+limit_typ) = zero
            if(knumEGr > 1) covarLyt(1+limit_typ) = covar(1,2)
        end if

        if(.false. .and. FitDecay .and. limit_typ == 1) then
!             write(66,*) 'Messwert array for DT:'
            call logger(66, 'Messwert array for DT:')
            do i=1,ngrs+ncov+numd
                if(i >= kfitp(1) .and. i <= kfitp(1)+2) then
!                     write(66,'(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(fpa(i-kfitp(1)+1),8), &
!                         'u(mw(i))=',real(sfpa(i-kfitp(1)+1),8)
                    write(log_str, '(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(fpa(i-kfitp(1)+1),8), &
                        'u(mw(i))=',real(sfpa(i-kfitp(1)+1),8)
                    call logger(66, log_str)
                    Messwert(i) = fpa(i-kfitp(1)+1)
                else
!                     write(66,'(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(Messwert(i),8), &
!                         'u(mw(i))=',real(StdUnc(i),8)
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
!             write(66,*) 'Messwert array after DT:'
            call logger(66, 'Messwert array after DT:')
            do i=1,ngrs+ncov+numd
                if(i >= kfitp(1) .and. i <= kfitp(1)+2) then
!                     write(66,'(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(fpa(i-kfitp(1)+1),8), &
!                         'u(mw(i))=',real(sfpa(i-kfitp(1)+1),8)
                    write(log_str, '(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(fpa(i-kfitp(1)+1),8), &
                        'u(mw(i))=',real(sfpa(i-kfitp(1)+1),8)
                    call logger(66, log_str)
                    Messwert(i) = fpa(i-kfitp(1)+1)
                else
!                     write(66,'(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(Messwert(i),8), &
!                         'u(mw(i))=',real(StdUnc(i),8)
                    write(log_str, '(a,i3,1x,a15,2(2x,a,es12.5))') 'i=',i,symboleG(i)%s,'Mw(i)=',real(Messwert(i),8), &
                        'u(mw(i))=',real(StdUnc(i),8)
                    call logger(66, log_str)
                end if
            end do
        end if

        IF(FitDecay) then
            fpa = fpaSVur
            dnetrate(1:numd)  = Messwert(ngrs+ncov+1:ngrs+ncov+numd) - d0zrate(1:numd)
            if(k_rbl > 0) dnetrate(1:numd) = dnetrate(1:numd) -  Messwert(kpoint(k_rbl))
            SDnetrate(1:numd) = Messwert(ngrs+ncov+1:ngrs+ncov+numd)/dmesszeit(1:numd) + sd0zrate(1:numd)**two
            if(k_rbl > 0) SDnetrate(1:numd) = SDnetrate(1:numd) + StdUnc(kpoint(k_rbl))**two
            do i=1,numd
                SDnetrate(i) = MAX(zero,  sqrt(SDnetrate(i)) )
            END do
        end if
!-----------------------------------------------------------------------
        GOTO 1500
1500    CONTINUE
!-------------------------------------------------------------------------
        IF(FitDecay) ifit = ifitDL

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
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_Gleich,      only: kEGr,knumEGr,nab,nmodf,nabf,ncovf,nfkf,klinf, &
            kgspk1,kfitcal,SymboleG,RSeite,Rseite_zero,Rseite_one, &
            knetto,Symbole,ifehl,ksumeval
        use UR_Linft,       only: FitDecay, FitCalCurve,kfitp,netto_involved_Fitcal,SumEval_fit
        use UR_Gspk1Fit,    only: Gamspk1_Fit
        use UR_Perror,      only: ifehlp
        USE fparser,        ONLY: initf, parsef
        USE UR_VARIABLES,   only: Gum_restricted,langg
        use Top,            only: WrStatusbar,CharmodA1
        use UWB,            only: TextReplace
        use KLF,            only: CalibInter
        use file_io,           only: logger
        use CHF,            only: ucase,testSymbol

        implicit none

        integer(4),intent(in) :: iopt     ! for control output: (0 (none) or 1 (with))

        integer(4)         :: nhg,i,i0,j,ndd,klu,k,klu2,ix1,ix2
        real(rn)           :: zfit0,zfit,uzfit
        character(len=350) :: rsfG
        character(len=512)           :: log_str
        character(:),allocatable :: RSeite_zeroSV2
!----------------------------------------------------------------------
        if(Gum_restricted) then
            call initf(nab+nmodf+nabf+ncovf+nfkf+2*knumEGr+10)
        else
            call initf(nab+nmodf+nabf+ncovf+nfkf+2*knumEGr+10)
        end if

!         if(iopt == 1) WRITE(66,'(4(a,i3))') 'SUP:   fparser: initf erfolgt:  nab=',nab,    &
!             ' nmodf=',nmodf,' nabf=',nabf,' ncovf=',ncovf
        if(iopt == 1)  then
            write(log_str, '(4(a,i3))') 'SUP:   fparser: initf erfolgt:  nab=',nab,    &
            ' nmodf=',nmodf,' nabf=',nabf,' ncovf=',ncovf
            call logger(66, log_str)
        end if

        nhg = nab+nmodf+nabf

        do i=1,nab+nmodf+nabf+ncovf+nfkf
            rsfG = trim(ucase(Rseite(i)%s))
            ifehlp = 0
            IF(FitDecay .AND. i == klinf) CYCLE
            IF(Gamspk1_Fit .AND. i == kgspk1) CYCLE
            IF(SumEval_Fit .AND. i == ksumeval) CYCLE
            IF(FitCalCurve .AND. i == kfitcal) CYCLE
            if(index(rsfG,'KALFIT') > 0) cycle
            if(len_trim(RSeite(i)%s) == 0) cycle
            IF(i <= nhg) THEN
                call parsef(i,RSeite(i)%s,SymboleG)
                if(ifehl == 1) return
                ! WRITE(66,*) 'SUP: fparser: i=',i,',  i<=nhg: parsef von ',TRIM(Rseite(i)%s), &
                !             ' erfolgt: ifehlp=',ifehlp,' kgspk1=',kgspk1
            end if
            IF(i > nhg .AND. LEN_TRIM(Rseite(i)%s) > 0) THEN
                call parsef(i,Rseite(i)%s,SymBoleG)
                if(ifehlp == 1) return
                ! WRITE(66,*) 'SUP: fparser: i=',i,',  i>nhg: parsef von ',TRIM(Rseite(i)%s), &
                !             ' erfolgt: ifehlp=',ifehlp,' kgspk1=',kgspk1
            end if
!             if(ifehlp == 1) write(66,*) 'SUP:  parsef:  kEGr=',int(kEGr,2),'  ifehlp=1 (Fehler)  für Gleichung Nr=',int(i,2)
            if(ifehlp == 1)  then
                write(log_str, '(*(g0))') 'SUP:  parsef:  kEGr=',int(kEGr,2),'  ifehlp=1 (Fehler)  für Gleichung Nr=',int(i,2)
                call logger(66, log_str)
            end if
        end do

        ndd = nab+nmodf+nabf+ncovf+nfkf
        if(Gum_restricted) return

!         write(66,'(a,i2a,L1,a,L1,a,i3)') 'knumEgr=',knumEGr,'  FitDecay=',FitDecay,'  Gamspk1_Fit=',Gamspk1_Fit,'  klinf=',klinf
        write(log_str, '(a,i2a,L1,a,L1,a,i3)') 'knumEgr=',knumEGr,'  FitDecay=',FitDecay,'  Gamspk1_Fit=',Gamspk1_Fit,'  klinf=',klinf
        call logger(66, log_str)
!         write(66,'(a,i0,a,L1,a,L1,a,i3)') 'kgspk1=',kgspk1,'  gum_restricted=',gum_restricted,'  FitCalCurve=',FitCalCurve, &
!             ' ndd=',ndd
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
            IF(Gamspk1_Fit) klu = kgspk1
            if(klu == 0) klu = knetto(j)
            if(klu == 0 .and. gum_restricted) klu = kEGr
            if(klu == 0) cycle
!             write(66,'(a,i3,2x,a)') 'klu=',klu,Symbole(klu)%s
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
                    call TextReplace(Rseite_zeroSV2,SymboleG(klu2)%s,'(' // Rseite_zero(klu2)%s // ')')
                    Rseite_zero(j)%s = Rseite_zeroSV2
                end if
                Rseite_one(j)%s = Rseite_zero(j)%s
                call TextReplace(Rseite_one(j)%s,SymboleG(klu)%s,'1')
                call TextReplace(Rseite_zero(j)%s,SymboleG(klu)%s,'0')
            else
                Rseite_zero(j)%s = '0                         '
                Rseite_one(j)%s = '1                         '
            end if
            if(FitCalCurve .and. netto_involved_Fitcal) then
                call CalibInter(2,zero,one, zfit0,uzfit)
                write(Rseite_zero(j)%s,'(es13.6)') zfit0
                call CalibInter(2,one,one, zfit,uzfit)
                write(Rseite_one(j)%s,'(es13.6)') zfit
            end if
            ! write(66,*) 'RSeite_zero(j)=',trim(RSeite_zero(j))
            ! write(66,*) 'RSeite_one(j)=',trim(RSeite_one(j))

            call parsef(ndd+(j-1)*2+1, Rseite_zero(j)%s, SymboleG)
            if(ifehl == 0) then
                call parsef(ndd+(j-1)*2+2, Rseite_one(j)%s, SymboleG)
            end if
            if(ifehl == 1) then
                if(langg == 'DE') then
                    call WrStatusBar(3,'Fehler in SetupParser: klu=0')
                    call WrStatusBar(4,'Abbruch.')
                elseif(langg == 'EN') then
                    call WrStatusBar(3,'Error in SetupParser: klu=0')
                    call WrStatusBar(4,'Abortion.')
                elseif(langg == 'FR') then
                    call WrStatusBar(3,'Erreur en SetupParser: klu=0')
                    call WrStatusBar(4,'Avortement.')
                end if
!                 write(66,*) 'Error within SetupParser in Rechw2: klu=0'
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

!         write(66,'(a,i2,a,i3)') 'SUP END:   knumEGr=',knumEGr,'  ndd=',ndd
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
    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_gleich,         only: Messwert, klinf, kgspk1, kEGr, knetto, ifehl, ngrs, ncov, &
                                 nmumx, Rnetmodi, ksumeval
    use UR_Linft,          only: FitDecay, kfitp, numd, SumEval_fit
    USE UR_Gspk1Fit,       only: Gamspk1_Fit
    use UR_VARIABLES,      only: Gum_restricted, langg
    use UR_DLIM,           only: FConst, FLinear, fvalueB, modeB, iteration_on, kluB
    use Top,               only: WrStatusbar


    implicit none
    real(rn), intent(in)    :: xAct

    integer          :: klu, iter, itmax, modeSV
    real(rn)         :: x1,x2,xacc,mws(nmumx),dummy
    real(rn)         :: zerof,sa,sb,fa,fb
    logical          :: iteronSV,RnetmodiSV

    interface
        function ffuncRnet(mode,x)
            use UR_params,   only: rn
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
    RnetVal= zero
    mws(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)   ! save Messwert

    if(.true. .and. abs(xAct) < eps1min) then
        Rnetval = (xAct - Fconst)/Flinear
        goto 100
    end if

    klu = knetto(kEGr)
    if(FitDecay) klu = klinf
    IF(Gamspk1_Fit) klu = kgspk1
    IF(FitDecay .and. kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
    if(Sumeval_fit) klu = ksumeval

    if(klu == 0) then
        ifehl = 1
        if(langg == 'DE') then
            call WrStatusBar(3,'Fehler in RnetVal: klu=0')
            call WrStatusBar(4,'Abbruch.')
        elseif(langg == 'EN') then
            call WrStatusBar(3,'Error in RnetVal: klu=0')
            call WrStatusBar(4,'Abortion.')
        elseif(langg == 'FR') then
            call WrStatusBar(3,'Erreur en RnetVal: klu=0')
            call WrStatusBar(4,'Avortement.')
        end if
        goto 100   !return
    end if
    ! write(66,*) 'RnetVal:  Act=',sngl(Act),' Flinear=',sngl(Flinear),' Fconst=',sngl(Fconst), &
    !                ' klu=',int(klu,2)
    if(klu == kEGr) then
        RnetVal = xAct
        goto 100        !return
    end if
! x1, x2 : bracketing values encompassing the desired net cout rate value
    x1 = 0.7_rn * (xAct-Fconst)/FLinear
    x2 = one/0.7_rn * (xAct-Fconst)/Flinear ! *2._rn
    if(x1 > x2) then
        dummy = x2
        x2 = x1
        x1 = dummy
    end if
    xacc = abs(x1+x2)/two*0.00000001_rn   ! / 10._rn

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

        if(langg == 'DE') then
            call WrStatusBar(3,'Fehler in RnetVal: IT>10')
            call WrStatusBar(4,'Abbruch.')
        elseif(langg == 'EN') then
            call WrStatusBar(3,'Error in RnetVal: IT>10')
            call WrStatusBar(4,'Abortion.')
        elseif(langg == 'FR') then
            call WrStatusBar(3,'Erreur en RnetVal: IT>10')
            call WrStatusBar(4,'Avortement.')
        end if
        goto 100       ! return
    end if

100 continue
    Rnetmodi = RnetmodiSV
    iteration_on = iteronSV
    modeB = modeSV
    Messwert(1:ngrs+ncov+numd) = mws(1:ngrs+ncov+numd)   ! restore Messwert array

end function RnetVal

!##############################################################################
end submodule RW2A
