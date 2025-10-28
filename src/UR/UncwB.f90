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
module UWB
    use UR_types,  only: rn
    use UR_params, only: EPS1MIN, ZERO, ONE, TWO

contains

    ! changeSname
    ! ResultA
    ! upropa
    ! RbtCalc
    ! median
    ! CorrectLists
    ! corrmatEGr
    ! Exchange2Symbols
    ! ExchgText
    ! ExchgDB
    ! gevalf
    ! func_Fconst
    ! func_Flinear
    ! dpi_uprop1
    ! RebuildEquations
    ! fSD
    !

    !#######################################################################

    subroutine ChangeSname()

        ! Replace the name (oldname) of a symbol by a new one, within all
        ! fields containing the oldname
        !
        !     Copyright (C) 2014-2025  Günter Kanisch

        use, intrinsic :: iso_c_binding

        use gtk_hl,             only: gtk_buttons_OK,GTK_BUTTONS_OK_CANCEL,GTK_RESPONSE_CANCEL
        use gtk,                only: GTK_MESSAGE_WARNING
        use UR_Gleich_globals,  only: meanID,Symbole,SymboleG,RSSy,symbole_CP,Formeltext,linfit_rename, &
                                      Rseite,CVFormel,CVFormel_CP,SDFormel,SDFormel_CP,FormeltextFit,nRSsy, &
                                      knetto,kbrutto,nRSsyanf,Formelt,nglp,nglp_read,  &
                                      kEGr,ifehl,nab,ncov,nmodf,nvarsMD,klinf,ngrs
        use ur_general_globals, only: Gum_restricted, savep
        use UR_Loadsel,      only: kopt,sname,soldname
        use UR_Linft,        only: FitDecay,SumEval_fit
        use UR_Gspk1Fit,     only: Gamspk1_Fit
        use Rout,            only: MessageShow, WDPutTextviewString, WTreeViewPutStrArray, &
                                   WDSetComboboxAct, WDPutEntryInt, WDPutEntryString, &
                                   WDListstoreFill_1
        use Top,             only: Wrstatusbar, FieldUpdate, CharModA1, CharModStr
        use LF1,             only: Linf
        use UR_perror

        use CHF,             only: ucase, StrReplace
        use RG,              only: modify_Formeltext
        use translation_module, only: T => get_translation

        implicit none

        integer             :: i1, resp, i, k
        character(len=60)   :: oldname,newname,oldnameg,newnameg
        character(:),allocatable  :: text,str1

        ifehl = 0
        if(len_trim(Sname) == 0) RETURN

        allocate(character(len=300) :: str1)

        if(.not.linfit_rename) then
            oldname = Symbole(kopt)%s
        else
            oldname = Soldname
        end if

        newname = Sname
        oldnameG = ucase(Oldname)
        NewnameG = ucase(newname)

        do i=1,ngrs
            IF(TRIM(SymboleG(i)%s) == TRIM(newnameG)) THEN
                call CharModStr(str1,300)

                str1 = T('Error') // ": " // T('The new symbol name is already existing') //": " // trim(newname) // &
                    new_line('A') // T('Change anyway?')


                call MessageShow(trim(str1), GTK_BUTTONS_OK_CANCEL, "ChangeSname:", resp,mtype=GTK_MESSAGE_WARNING)

                if(resp == GTK_RESPONSE_CANCEL) then
                    ifehl = 1
                    return
                end if
            end if
        end do

        IF(FitDecay) THEN
            ! write(66,*) ' verbotenen Namen gefunden:  trim(oldnameG)=',trim(oldnameG),'   RBL=','RBL'
            IF(TRIM(oldnameG) == 'RBL' .or. TRIM(oldnameG) == 'TMESS' .or. TRIM(oldnameG) == 'TSTART') THEN
                call CharModStr(str1,300)
                str1 = T('Error') // ": " // T('This symbol name must not be changed') // ": " // trim(oldname)
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "ChangeSname:", resp,mtype=GTK_MESSAGE_WARNING)
                ifehl = 1
                return
            end if
        end if

        do i=1,ngrs
            if(klinf > 0 .and. i /= klinf .and. linfit_rename) cycle

            IF(TRIM(symbole(i)%s) == TRIM(oldname)) Symbole(i)%s = newname
            IF(TRIM(symboleG(i)%s) == TRIM(oldnameG)) SymboleG(i)%s = newnameG
            IF(TRIM(symbole_CP(i)%s) == TRIM(oldname)) Symbole_CP(i)%s = newname
            if(i <= ubound(nRSsy,dim=1)) then
                do k=1,nRSsy(i)
                    IF(TRIM(RSSy(nRSsyanf(i)+k-1)%s) == TRIM(oldnameG) ) RSSy(nRSsyanf(i)+k-1)%s = newnameG
                end do
            end if
            if(i <= ubound(RSeite,dim=1)) call StrReplace(Rseite(i)%s,oldname,newname, .true., .true.)
        end do

        if(nmodf > 0) then
            do i=nab+1, nab+nmodf
                if(i <= ubound(nRSsy,dim=1)) then
                    do k=1,nRSsy(i)
                        IF(TRIM(RSSy(nRSsyanf(i)+k-1)%s) == TRIM(oldnameG) ) RSSy(nRSsyanf(i)+k-1)%s = newnameG
                    end do
                    call StrReplace(Rseite(i)%s,oldname,newname, .true., .true.)
                end if
            end do
        end if
        if(linfit_rename) goto 22

        do i=1,ncov
            text = CVFormel(i)%s
            IF(LEN_TRIM(text) == 0) CYCLE
            call StrReplace(text,oldname,newname, .true., .true.)
            CVFormel(i)%s = TRIM(text)
        end do

        do i=1,ncov
            text = CVFormel_CP(i)%s
            call StrReplace(text,oldname,newname, .true., .true.)
            CVFormel_CP(i)%s = TRIM(text)
        end do

        do i=1,ngrs
            text = SDFormel(i)%s
            IF(LEN_TRIM(text) == 0) CYCLE
            call StrReplace(text,oldname,newname, .true., .true.)
            SDFormel(i)%s = TRIM(text)
        end do

        do i=1,ngrs
            text = SDFormel_CP(i)%s
            IF(LEN_TRIM(text) == 0) CYCLE
            call StrReplace(text,oldname,newname, .true., .true.)
            SDFormel_CP(i)%s = TRIM(text)
        end do

        if(nvarsMD > 0) then
            do i=1,nvarsMD
                i1 = Index(meanID(i)%s,'_')
                str1 = ucase(meanID(i)%s)
                if(trim(str1(1:i1-1)) == trim(oldnameG)) then
                    meanID(i)%s = trim(newname) // '_' // trim(meanID(i)%s(i1+1:))
                end if
            end do
            call WDListstoreFill_1('liststore_MDvars', nvarsMD, meanID)
        end if


        if(size(Formeltext) > 0) then
            do i=1,size(Formeltext)
                call StrReplace(Formeltext(i)%s,oldname,newname, .true., .true.)
            end do
            do i=1,size(Formelt)
                call StrReplace(Formelt(i)%s,oldname,newname, .true., .true.)
            end do
            if(FitDecay) then
                do i=1,size(FormeltextFit)
                    call StrReplace(FormeltextFit(i)%s,oldname,newname, .true., .true.)
                end do
            end if
            call modify_formeltext(2)
            write(66,*) 'Exchg: nglp_read=',nglp_read,' nglp=',nglp,' size(Formtext)=',size(Formeltext)
            call WDPutTextviewString('textview2', Formeltext)
            if(FitDecay) call WDPutTextviewString('textviewModelEQ', FormeltextFit)
        end if
22      continue

        call WTreeViewPutStrArray('treeview1', 2, ngrs, symbole)
        call WDListstoreFill_1('liststore_symbols', ngrs, symbole)

        if(.not.Gum_restricted) then
            IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
            IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGr))
        end if

        call CharModStr(str1,300)
        str1 = T('Selected output quantity:') // ": " // TRIM(Symbole(kEGr)%s)

        call WDPutEntryString('LBOutpQuantity', trim(str1))
        call WDPutEntryString('entryActiveKegr', TRIM(Symbole(kEGr)%s))

        call WTreeViewPutStrArray('treeview2', 2, ngrs, symbole)
        call WTreeViewPutStrArray('treeview2', 7, ngrs, sdformel)
        call WTreeViewPutStrArray('treeview3', 5, ncov, CVformel)

        SaveP = .TRUE.
        call FieldUpdate()
        call WrStatusBar(3, T('Unsaved', .true.) // "!")

    end subroutine ChangeSname

    !#######################################################################


    real(rn) Function Resulta(nn,nmax)

        !     Copyright (C) 2014-2023  Günter Kanisch

        ! this is the basic function which performs the calculations of dependent
        ! symbols (variables), one equation after another, starting always with equation
        ! index nab down to index nn.
        !
        ! kableitnum: index defined by: for a derivative of the output quantity with respect
        !                               to the symbol index kableitnum;
        !                               ableit_fitp = T while calculating this derivative
        ! iteration_on:  this variable is true during the iterative derivation of the decision threshold
        !                and the detection limit
        ! Rnetmodi:      is true if the net cout rate is externally modfied

        !----------------------------------------------------------------------------------------------

        ! List of formulae (i.e., right-hand-sided of equations; or standard Deviation formualae:

        ! 1 - nab                                     : formulae for dependent quantities
        ! (nab+1) - (nab+nmodf)                       : formulae for FitDecay terms Xi in LinFit
        ! (nab+nmodf+1) - (nab+nmodf+nabf)            : formulae for standarda deviations (SDFormel())
        ! (nab+nmodf+nabf+1) - (nab+nmodf+nabf+ncovf) : formualae for covariaces
        !
        ! (nab+nmodf+nabf+ncovf+1)                    : Formelula kalfit_arg_expr,     or
        !                                               kpoint_kalarg = nab+nmodf+nabf+ncovf+1  or
        !                                               kpoint_kalarg = nab+nmodf+nabf+ncovf+nfkf  (nfkf=0 or 1)

        ! numberinge of symbols:

        ! 1 - nab                                     : dependent symbols
        ! (nab + 1) - (nab+nmu=ngrs)                  : independent symbols
        ! (nab+nmu+1) - (nab+nmu+ncov)                : pseudo-symbols for covariances
        ! (nab+nmu+ncov+1) - (nab+nmu+ncov+numd)      : gross count rates of decay curves (FitDecay); or
        !                                               gamma peak data (Gamspk1), 5 values per peak;

        ! Note:
        ! A right-hand-side formula containg an UncertRadio function (Linfit,Kalfit,Gampk1,...)
        ! must not be supplied to fparser, otherwise this leads to an error, because these functions
        ! cannot be handled by evalf.
        ! If in an fparse error message the strings LINFIT or GAMSPK1 occur, thsi indicates a programming
        ! error!
        !----------------------------------------------------------------------------------------------

        use UR_Gleich_globals, only: MEsswert,ifehl,kableitnum,kEGr,kfitcal,kgspk1,klinf,knumEGr, &
                                  nab,nonPoissGrossCounts,Rnetmodi,kpointkb,knetto,kbrutto,StdUnc, &
                                  missingval,ksumeval,use_dependent_sdwert,use_sdf_brutto
        use UR_Linft
        use UR_DLIM,        only: A_Result,iteration_on,RD_Result
        use ur_general_globals, only: MCSim_on, Gum_restricted,ableit_fitp, Resulta_on,MCsim_localOff
        use fparser,        only: initf, parsef, evalf, EvalErrMsg
        use UR_Perror
        use UR_Gspk1Fit
        use KLF,            only: calibinter
        use LF1,            only: Linf
        use LF1G,           only: Linfg1
        use DECH,           only: Decaysub1
        use UR_DecChain,    only: DCpar,DChain,AdestMC,uAdestMC
        use RND,            only: rnorm

        implicit none

        integer   ,intent(in)           :: nn   ! index of the equation number, for which the value of the output quantity is calculated
        integer   ,intent(in),optional  :: nmax ! index > nn, from which the calculations shall start, down to nn

        integer            :: j, iwh, klu, nmin, nmx, knd, kdc
        real(rn)           :: res, RD, rn0,SDrn0,akt,SDakt,Adest,uAdest    ! ,Act,
        real(rn)           :: Messwert_klu, yval, uyval, resuSV, kf_save

        !-----------------------------------------------------------------------
        Resulta = ZERO

        Resulta_on = .true.              ! 27.4.2025

        klu = klinf
        if(klinf == 0) klu = knetto(kEGr)
        IF(Gamspk1_Fit) klu = kgspk1
        IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
        Messwert_klu = ZERO
        resuSV = ZERO
        kf_save = missingval
        if(ksumeval > 0) klu = ksumeval

        iwh = 0

        iwh = iwh + 1

        ! If nn <= knumEGR, all knumEGr output quantity values are calculated
        nmin = nn
        if(nn <= knumEgr) nmin = 1

        nmx = nab
        if(present(nmax)) then
            if(nmax > 0) nmx = min(nab,nmax)
        end if
        do j=nmx,nmin,-1

            kdc = 0   !  shifted into this loop!    8.1.2025  GK
            if(DChain) then           ! 16.12.2024 GK      27.4.2025
                kdc = 0
                knd = Findloc(DCpar%indx,j,dim=1)
                if(knd > 0) kdc = DCpar(knd)%indx
                !  write(66,*) 'kdc=',kdc,' knd=',knd
            endif

            !Calculation of the formulae:
            IF(iteration_on  .AND. .not.FitDecay .AND. .not.Gamspk1_Fit .and. .not.SumEval_fit) then
                if(j == kbrutto(kEGr)) then
                    if(.not.nonPoissGrossCounts) cycle    ! i.e., cycle in the case of Poisson compatability
                end if
            end if

            IF(FitDecay .AND. j == klinf) THEN

                IF(Rnetmodi .or. ableit_fitp) CYCLE
                IF(klu > 0) then
                    Messwert_klu = Messwert(klu)
                end if
                call Linf(rn0,SDrn0)
                IF(ifehl == 1) then
                    ResultA_on = .false.          ! 27.4.2025
                    RETURN
                end if
                Messwert(klu) = rn0

                !-----
                if( (MCSim_on .or. iteration_on ) .and. knumEGr > 1) then
                    Messwert(kfitp(1):kfitp(1)+2) = fpa(1:3)
                end if
                !-----
                ! Notes: while MCSim_on == T holds, kableitnum is always = 0!
                IF(FitDecay .AND. kableitnum == klu .AND. abs(Messwert_klu) > EPS1MIN ) THEN
                    ! When calculating a derivative with respect to the klinf variable (net count rate)
                    ! the value Messwert(klu), i.e. Fitp-i-parameter, is also modified, which is restored here from
                    ! the local Messwert_klu.
                    ! Note: Messwert values can become negative!
                    ! WRITE(66,*) 'Resulta: kableitnum=',kableitnum,'  klu=',klu,' Messwert(klu)=',sngl(Messwert(klu)), &
                    !                                                    ' Messwert_klu=',sngl(Messwert_klu)
                    ! WRITE(66,*) 'Resulta: kableitnum=',kableitnum,'  klu=',klu,' StdUnc(klu)=',sngl(StdUnc(klu)), &
                    !                                                    ' StdUnc_klu=',sngl(StdUnc_klu)
                    Messwert(klu) =  Messwert_klu

                end if

                ! if(iteration_on) write(66,*) '  Resulta:    at End of 1st FitDecay case'
            else IF(Gamspk1_Fit .AND. j == kgspk1) THEN
                IF(Rnetmodi .or. ableit_fitp) CYCLE
                call Linfg1(akt,SDakt)
                Messwert(j) = akt
                ! if(iteration_on .or. kableitnum == kgspk1) WRITE(66,*) 'Resulta, Linfg1: ',' j=',j,'  akt=',akt,'  SDakt=',sngl(SDakt)

            else if(FitCalCurve .and. j == kfitcal) then
                if(KFmode == 1) then
                    if(Rnetmodi) cycle
                elseif(KFmode == 2) then
                    ! for Rnetmodi==T: no cycle!
                    ! Messwert(j) is saved on a variable and then interpolated from the calibration curve
                end if
                Messwert_klu = Messwert(j)
                !write(66,*) '        Res:  j=',j,'  Messwert_klu=',sngl(Messwert_klu),' Messwert(kpointKB(1))=', &
                !             sngl(Messwert(kpointKB(1)))
                call CalibInter(KFMode, Messwert(kpointKB(1)), StdUnc(kpointKB(1)), yval,uyval)
                Messwert(j) = yval
                if(kableitnum == j .and. abs(Messwert_klu) > EPS1MIN) Messwert(j) = Messwert_klu
                ! write(66,*) 'Resulta:  Gl. j=',int(j,2),' MW(j)=',sngl(Messwert(j)),' MWKB(1)=',sngl(Messwert(kpointKB(1)))

            else if(SumEval_fit .and. j == ksumeval) then
                IF(Rnetmodi .or. ableit_fitp) CYCLE
                call SumEvalCalc(akt,SDakt)
                Messwert(j) = akt
            else if(kdc > 0 .and. kdc == j) then
                  ! j: UR's Eq number
                if(.not.MCsim_on .or. MCsim_localOff) then
                    call Decaysub1(knd,Adest,uAdest)
                    Messwert(j) = Adest
                else
                    Messwert(j) = AdestMC(knd) + rnorm()*uAdestMC(knd)     ! 14.1.2025  GK
                end if
                     !if(kableitnum == 0) write(66,*) 'Result: Gl. j=',int(j,2),' MW(j)=',sngl(Messwert(j)),' nach Dsub1'
                     ! if(kableitnum == 0) write(66,*) 'Result: Gl. j=',int(j,2),' Adest=',sngl(Adest), &
                     !     ' knd=',int(knd,2),' nn=',int(nn,2)

            else

                IF(Rnetmodi .AND. j == knetto(kEGr)) CYCLE

                if(.true. .and. (use_dependent_sdwert .or. use_sdf_brutto)) then
                    if(j == kableitnum .and. j /= kbrutto(kEGr)) cycle
                    IF(Rnetmodi .AND. j == kableitnum ) CYCLE
                end if


                ! if(j == 2) write(66,*) 'before gevalf:  j=',j,' MW(j)=',sngl(Messwert(j)),' Gl.:',trim(Rseite(j))

                res = gevalf(j,Messwert)         ! <---- call evaluation by the function parser
                Messwert(j) = res
                ! if(.not.iteration_on)  &
                !   write(66,*) 'Resu: j=',int(j,2),'nn=',int(nn,2),' ',Symbole(j)%s,'  MW=',real(Messwert(j),8),' Rnetmodi=',Rnetmodi, &
                !              '  iter_on=',iteration_on,' MW=',real(Messwert(1:10),8)
                ! if(MCSim_on .and. .not.iteration_on) write(63,*) 'ResA: j=',j,'  after gevalf,  res=',sngl(res),' nmin=',int(nmin,2),' nab=',int(nab,2)
            END IF
            if(j == nn) ResuSV = Messwert(j)
            ! if(j == nn)  write(66,*) 'Result: ResuSV(j=nn)=',sngl(resuSV),' j=',int(j,2)
        end do


        Resulta = resuSV
        !if(.not.iteration_on)  then
        !  do i=1,ngrs
        !    write(66,*) Symbole(i)%s,'  mw=',sngl(messwert(i)),' umw=',sngl(stdunc(i))
        !  end do
        !end if

        IF(FitDecay .or. Gamspk1_Fit) THEN
            RD = Messwert(klu)
        else
            ! if(.not.Gum_restricted) then
            if(.not.Gum_restricted .and. knetto(kEGr) > 0) then         ! 1.5.2025 GK
                RD  = Messwert(knetto(kEGr))
            else
                RD = ONE
            end if
        end if
        akt = Resulta
        IF(nn == kEGr) THEN
            RD_result = RD
            A_result = Messwert(kEGr)
        end if

        ResultA_on = .false.               ! 27.4.2025

    end function Resulta

!#######################################################################

    subroutine upropa(nn)

        !     Copyright (C) 2014-2025  Günter Kanisch

        ! Full uncertainty propgation:

        ! This routine performs all necessary calculations for calculation the uncertainty
        ! Ucomb of the dependent quantity with symbol list index nn, based on the uncertainties,
        ! and covariances between, the independent input quatities.
        ! Similarly as with ResultA,  this calculation also starts with equation nab
        ! - down to nn.
        ! The partial derivatives with respect to the input quantities always refer to
        ! the equation nn.
        !
        ! function dpafact(Messert(j)) : used to calculate the small increment dpa of Messwert(j)
        !                                required for calculating the derivative:
        !                                dpa = Messwert(j)*dpafact(Messert(j) - Messwert(j)
        !


        use UR_Gleich_globals
        use UR_Linft
        use UR_DLIM,           only : iteration_on,limit_typ, GamDist_ZR,GamDistAdd
        use UR_Gspk1Fit
        use ur_general_globals, only: ableit_fitp,kModelType,chh1,chh2, &
                                      mwert1,kbd,Messwert_kbruttoSV,fv1back, mwert2
        use fparser,         only: evalf, EvalErrMsg
        use Top,             only: dpafact,CharModStr
        use CHF,             only: ucase,testSymbol
        use KLF,             only: CalibInter
        use LF1,             only: Linf
        use Num1,            only: matwrite

        use RW2,             only: kqt_find
        use DECH,            only: Decaysub1
        use UR_DecChain,     only: DCpar,DChain

        implicit none

        integer, intent(in) :: nn    ! Number of equation, for which the standard uncertainty Ucomb is to be calculated

        integer            :: i,k,kk,m1,m2,ii,klu, kk2,kout,j,i1,iim1,iim2
        integer            :: ngrmax,kdoub, nhg,kk1,nj,ibb,imin,mkm1,mkm2,jj
        integer            :: kqt,jj1,jj2,nfd,knet,knd,nndest
        real(rn)           :: fv1,fv2,dpa,dpi,var,dummy,dpi1,dpi2,dpisum
        real(rn)           :: var1,varsq,Uc2,mwklu,fv1R_SV
        real(rn),allocatable :: cju(:),ry_xi(:),corrx(:)
        character(:),allocatable  :: ch1,str1
        logical, allocatable      :: use_sdwert_nn(:)
        integer   ,allocatable    :: nbez_sdf(:)
        LOGICAL            :: testout,condecay,congamma,hh1,hh2
        real(rn)           :: MesswertKP(nabmx+nmumx),covx1(ncovmx)
        real(rn)           :: MEsswert_kbd,covlk
        real(rn)           :: mw_save,sd_save,help,upar        ! ,corrx(100)
        real(rn)           :: yval, uyval, ptmin, dpi1z, dpi2z, ssi, dpisum2
        !-----------------------------------------------------------------------

        if(.not.allocated(dpi1v)) allocate(dpi1v(50),dpi2v(50))
        allocate(character(len=800) :: ch1,str1)
        allocate(corrx(max(1,ncov)))

        upropa_on = .TRUE.

        kout = 66

        if(symtyp(nn)%s == 'p' .or. symtyp(nn)%s == 'P') then
            Ucomb = ZERO
            upropa_on = .false.
            return
        end if

        kqt = kqt_find()

        sd_save = ZERO
        mw_save = ZERO

        kableitnum = 0
        klu = klinf
        IF(Gamspk1_Fit) klu = kgspk1
        IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
        if(ksumeval > 0) klu = ksumeval

        ableit_fitp = .false.         ! ab 11.7.2023
        Rnetmodi = .false.
        knd = 0
        mwklu = ZERO     ! 2025.01.24 GK
        fv1R_SV = ZERO   !
        var = ZERO       !

        !  Omitting the following Resulta-call affects only the special case in project NLWKN_Fe-55_mit_KALFIT_DE.txp aus!
        if(kqt > 1 .and. kbrutto(kEGr) > 0) then
            if(knetto(kEGr) > 0) then
                imin = knetto(kEGr)-1
            else
                imin = klu - 1
            end if
            dummy = Resulta(kEGr,imin)
        end if

        MesswertKP(1:ngrs+ncov+numd*knd) = Messwert(1:ngrs+ncov+numd*knd)  ! save the Messwert array values!
        ! upropa_on = .TRUE.

        testout = .FALSE.
        !!  testout = .TRUE.

        !      choose the condition for output to unit 66:

        ! IF(nn == kEGr .and. .not.iteration_on) testout = .TRUE.
        ! IF(nn == 4 .and. .not.iteration_on .and. kEGr == 1 .and. use_WTLS) testout = .TRUE.
        ! if(kqt >= 2) testout = .true.
        ! testout = .TRUE.
        ! IF(iteration_on .and. limit_typ == 1) testout = .TRUE.
        ! if(kqt == 1 .and. .not. iteration_on) testout = .true.
        ! if(kqt == 1 .and. .not. iteration_on .and. use_WTLS .and. nn == 1) testout = .true.
        ! if(kqt == 2) testout = .true.
        ! if(kqt == 2 .and. kableitnum == 0) testout = .true.
        ! if(kqt == 2 .and. iteration_on .and..not.MCSim_on) testout = .true.

        if(testout) write(66,*) '####################### Start Upropa ',symbole(kEGr)%s,'  ####################'
        IF(testout) write(66,*) 'use_WTLS=',use_WTLS
        IF(testout) write(66,*) 'MesswertKP(1:10)=',real(MesswertKP(1:10),8)

        Ucomb = ZERO
        IF(.not.iteration_on) UcombLinf = ZERO
        covx1(1:ncovmx) = ZERO
        kk2 = 0

        ngrmax = ngrs+ncov
        IF(FitDecay) ngrmax = ngrs+ncov+numd
        IF(Gamspk1_Fit) ngrmax = ngrs+ncov+numd

        if(allocated(perc)) deallocate(perc)
        if(allocated(sensi)) deallocate(sensi)
        if(allocated(Ucontrib)) deallocate(Ucontrib)
        allocate(perc,source=Messwert)
        allocate(sensi,source=Messwert)
        allocate(Ucontrib,source=Messwert)
        perc(1:ngrmax) = ZERO   ! i=1,ngrs+ncov+numd
        sensi(1:ngrmax) = ZERO
        Ucontrib(1:ngrmax) = ZERO
        if(allocated(cju)) deallocate(cju)
        if(allocated(ry_xi)) deallocate(ry_xi)
        allocate(cju,source=Messwert)
        allocate(ry_xi,source=Messwert)
        cju(1:ngrmax) = ZERO
        ry_xi(1:ngrmax) = ZERO
        allocate(use_sdwert_nn(nab),nbez_sdf(ngrmax))

        IF(testout) write(66,*) ' ******************  Propagating variances in Upropa: ',trim(fitmeth)

        ! Special case: If in the row nn within the "red" region of the table ValUnc a SDformel is defined,
        ! then the value SDWert shall be used for StdUnc(nn) instead of the uncertainty calculated below!
        use_dependent_sdwert = .false.
        use_sdwert_nn = .false.
        nbez_sdf = 0
        do i=max(kEGr+1,nn),nab
            if(len_trim(Sdformel(i)%s) > 0 .and. SDWert(i) > ZERO .and. i /= kbrutto(kEGr)) then
                           nfd = 0  ! 2025.01.24 GK
                if(knumEGr > 1) then
                    ! In case of more than 1 outpout quantity, here only the value from SDformel(kEGr) must be taken,
                    ! not the one from other output quantities:
                    nfd = 0
                    do k=1,knumEGr
                        if(k == kEGr) cycle
                        if(i == kbrutto(k)) nfd = 1
                    end do
                end if
                if(nfd == 0) then
                    ibb = RS_SymbolNr(i,1)
                    use_sdwert_nn(i) = .true.
                    nbez_sdf(ibb) = i
                    use_dependent_sdwert = .true.
                    ! write(66,*) 'UP: i=',int(i,2),'  ibb=',int(ibb,2)
                end if
            end if
        end do

        IF(kModelType /= 2 .and. FitCalCurve .and. netto_involved_Fitcal) then
            ! d Messwert(knetto(kEGr)) / d Messwert(kbrutto(kEgr)):
            call dpi_uprop1(1,kbrutto(kEGr), knetto(kEGr), kbrutto(kEGr)-1, knetto(kEGr), missingval,0, dpi,fv2,dpa )
            StdUnc(knetto(kEGr)) = sqrt( (dpi*fSD(kbrutto(kEGr)))**TWO )
        end if

        ! if(FitDecay .and. nn <= klinf) then
        !   ! dummy = Resulta(klinf)
        !          dummy = Resulta(nn)
        !   mwklu = Messwert(klu)
        ! end if
        if(FitDecay) then
            ! 15.7.2023:
            if(.not.use_WTLS) then
                if(nn <= klinf) then
                    dummy = Resulta(nn)
                    mwklu = Messwert(klu)
                end if
            else
                if(nn == klinf) then
                    dummy = Resulta(nn)
                    mwklu = Messwert(klu)
                    MesswertSV_klu = mwklu
                    MesswertSV_klinf = Messwert(klinf)
                elseif(nn < klinf) then
                    Messwert(klu) = MesswertSV_klu
                    mwklu = Messwert(klu)
                    Messwert(klinf) = MesswertSV_klinf
                    do j=klinf-1,nn,-1
                        Messwert(j) = gevalf(j,Messwert)
                    end do
                end if
            end if
        end if

        if(Gamspk1_Fit .and. nn <= kgspk1) then
            dummy = Resulta(kgspk1)
            mwklu = Messwert(klu)
        end if

        if(SumEval_fit .and. nn <= ksumeval) then
            dummy = Resulta(ksumeval)
            mwklu = Messwert(klu)
        end if

        if(Fitdecay .and. nn <= klinf) MesswertKP(1:klinf) = Messwert(1:klinf)  ! save the Messwert array values!

        ! begin of loop over the input quantity uncertainty contributions:
        ! covariances are handled after this long loop

        do i=nab+1,ngrmax
            perc(i) = ZERO
            kableitnum = i
            kdoub = 0
            kbd = 0
            ! WRITE(kout,*) 'uncpropa: kableitnum=',kableitnum

            IF(ncov > 0 .AND. i > ngrs .AND. i <= ngrs+ncov) CYCLE

            IF(iteration_on .and. .not.FitDecay .AND. .not.Gamspk1_Fit .and. .not.SumEval_fit ) then
                ! if(kbrutto(kEGr) <= nab) THEN
                if(kbrutto(kEGr) > 0 .and. kbrutto(kEGr) <= nab) THEN        ! 9.1.2024
                    ! Consider the case, that the gross counting rate is also defined by an equation,
                    ! e.g., Rb=Nb/tb. Then, for the purpose of the DL iteration, the uncertainty propagation
                    ! must not use the uncertainty of Nb, because that variable is NOT modified, but only Rb,
                    ! with using the uncertainty formula (SDFormel), e.g., sqrt(Rb/tb).
                    ! This latter step will be treated later, behind this do i=nab+1,ngrmax loop.
                    ch1 = Rseite(kbrutto(kEGr))%s
                    kbd = 0
                    IF(testSymbol(ch1,symboleG(i)%s) .and. INDEX(ch1,symboleG(i)%s) <= 2) THEN
                        !WRITE(kout,*) 'xxxxxx Upropa: skipped variable: ',TRIM(SymboleG(i)), &
                        !              '  Formula=',TRIM(ch1),' MW(i)=',sngl(Messwert(i)),' MWKP(i)=',sngl(MesswertKP(i))
                        IF(kbrutto2 > 0) then
                            ch1 = Rseite(kbrutto(kEGr))%s
                            IF(testSymbol(ch1,symboleG(i)%s) .and. StdUnc(i) > ZERO) THEN
                                ! WRITE(kout,*) 'xxxxxx Upropa: skipped variable: ',TRIM(SymboleG(i)),'  Formel=',TRIM(ch1)
                                kdoub = 1
                                ch1 = Rseite(kbrutto(kEGr))%s
                                do kk=nab+1,ngrs
                                    IF(testSymbol(ch1,symboleG(kk)%s) .and. kk == i) THEN
                                        kbd = kk
                                        mw_save = Messwert(kbd)
                                        SD_save = StdUnc(kbd)
                                        !WRITE(66,*) 'xxxxxx Upropa, Covars: i2: Messwert(kbd)= ',sngl(Messwert(kbd)),'  ',TRIM(SymboleG(kk)), &
                                        !              '  Formel=',TRIM(ch1)
                                        EXIT
                                    end if
                                end do

                                !WRITE(kout,*) 'UPROPA: kdoub=1:   Messwert(',i,')=',sngl(Messwert(i)),' SD(i)=',sngl(stdUnc(kbd)),  &
                                !              '  MesswertSV(i)=',sngl(MesswertSV(i)),'  StdUnc(i)=',sngl(stdunc(i))
                            end if
                        end if
                    end if
                end if
            end if

            ! negative values may also exist!
            IF( (abs(Messwert(i)) > EPS1MIN .AND. StdUnc(i) > ZERO) .OR.   &
                (abs(Messwert(i)) < EPS1MIN .AND. StdUnc(i) > ZERO) .or. kqt == 2 ) THEN

                kk2 = 0
                IF(FitDecay .AND. klu > 0 .AND. i > klu ) THEN

                    !   Note: a corresponding if case construct is not required for Gamspk1_Fit.
                    ! The uncertainty components of the gross count rates and of the Linfit-arguments parameter
                    ! are not taken into account here
                    kk2 = 0
                    IF(i >= ngrs+ncov+1 .AND. i <= ngrs+ncov+numd) THEN
                        ! Omit contributions of count rates
                        kk2 = 1
                        GOTO 120
                    end if
                    do ii=1,nhp
                        IF(mpfx(ii) == i .and. .not. mpfx_extern(ii) ) THEN
                            ! IF(mpfx(ii) == i .and. mpfx_extern(ii) ) THEN         ! <-- Versuch 29.3.2023
                            ! if(testout) write(66,*) 'Upropa: omittted symbol (goto 120) : ',SymboleG(mpfx(ii)))
                            ! Omitted: this part of uncertainty propagation is done via QSMAT in the routine lincov2
                            ! called by Linf, i.e., by ResultA
                            kk2 = 1
                            GOTO 120
                        end if
                    end do

                end if
                if(FitDecay .and. k_rbl > 0) then
                    if(kpoint(k_rbl) == i .and. StdUnc(i) > ZERO) then
                        goto 120
                    end if
                end if

                IF(testout) WRITE(kout,'(a,i3,a,i3,a,a,a,a,es15.8)') 'Contrib. by parameter i=',i,   &
                    '  (nn=',nn,')','     ',SymboleG(i)%s,'      val=',sngl(Messwert(i))
                !  write(66,*) '  upropa:  netto_involved_Fitcal=',netto_involved_Fitcal

                !  numerical partial derivative dpi for parameter p(i):
                !----  1st function value fv1:
                ableit_fitp = .false.
                IF(iteration_on) then
                    if(.not. run_corrmat .and. FitDecay .and. i >= kfitp(1) .and.   &
                        i <= kfitp(1)+2 .and. i /= klu) ableit_fitp = .true.
                    fv1 = Resulta(nn)
                    fv1R_SV = fv1     ! save for later calls!
                else
                    Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
                    fv1 = Messwert(nn)
                    if(.not. run_corrmat .and. FitDecay .and. i >= kfitp(1) .and.   &
                        i <= kfitp(1)+2 .and. i /= klu) ableit_fitp = .true.
                    if(GamDist_ZR) fv1 = Resulta(nn)
                    fv1R_SV = fv1     ! save for later calls!
                end if
                ableit_fitp = .false.
                if(testout .and. nn == 1) write(66,*) ' fv1=fv1R_SV=',sngl(fv1),' nn=1'

                IF(ABS(fv1) > 1.E+12_rn .AND. testout) THEN
                    do ii=1,ngrs+ncov+numd
                        WRITE(kout,*) Symbole(ii)%s,' Messwert=',sngl(Messwert(ii)),'  StdUnc=',sngl(StdUnc(ii))
                    end do
                end if

                if(.true. .and. abs(GamDistAdd) < EPS1MIN) then
                    if(IVTL(i) == 4 .and. abs(Messwert(i)) < EPS1MIN) then
                        if(abs(GamDistAdd) < EPS1MIN)  then
                            ! (N+x)-rule:
                            Messwert(i) = ONE
                        else
                            Messwert(i) = GamDistAdd
                        end if
                    end if
                end if
                !----  2nd function value:
                if(testout) write(66,*) 'Fv2:'
                IF(.true.) then
                    dpa = Messwert(i) * dpafact(Messwert(i)) - Messwert(i)
                    if(use_WTLS) then      ! 14.7.2023
                        dpa = Messwert(i) * (ONE + (ONE - dpafact(Messwert(i)))*10._rn) - Messwert(i)
                    end if

                    if(abs(dpa) < EPS1MIN) dpa = 1.0E-10_rn
                    if(.false. .and. IVTL(i) == 4) then
                        if(abs(Messwert(i)) < EPS1MIN) then
                            if(abs(GamDistAdd) < EPS1MIN) then
                                dpa = ONE
                            else
                                dpa = GamDistAdd
                            end if
                        end if
                    end if
                else
                    dpa = 1.0E-10_rn  !  13.2.2023  ! old:  1.0E-6_rn
                end if
                Messwert(i) = Messwert(i) + dpa
                ableit_fitp = .false.
                !  if(ivtl(i) == 4) write(66,*) 'ivtl=4: i=',int(i,2),' MW(i) before fv2: ',sngl(Messwert(i))
                if(iteration_on) then
                    if(.not. run_corrmat .and. FitDecay .and. i >= kfitp(1) .and. i <= kfitp(1)+2 .and.  &
                        i /= klu) ableit_fitp = .true.

                    fv2 = Resulta(nn)
                else
                    fv2 = Resulta(nn)    ! have in mind: GamdistAdd is also added within gevalf!
                end if
                !----

                ! if(FitDecay .and. i >= kfitp(1) .and. i <= kfitp(1)+2 .and. i /= klu) ableit_fitp = .false.
                Messwert(i) = Messwert(i) - dpa
                !  Messwert = MesswertKP    ! -------------------------
                ! Restore the Messwert array values:
                Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)       !!!
                !----   sensitivity factor = dpi (partial derivative):
                IF(abs(fv2/dpa - fv1/dpa) > EPS1MIN) THEN
                    dpi = (fv2/dpa - fv1/dpa)
                else
                    dpi = ZERO
                end if

                sensi(i) = dpi
                var = ( dpi * fSD(i) )**TWO
                !----------------- 27.4.2025 ---------------------------------------
                dpnni(nn,i) = dpi             ! <--  9.1.2025  GK
                ssi = dpi

                if(fSD(i) > zero .and. DChain) then            ! .and. .not.iteration_on) then
                    jj = findloc(DCpar%indx,nn,dim=1)
                    if(jj > 0) then
                        dpisum = zero
                        nfd = 0
                        dpisum2 = zero
                        nndest = DCPar(jj)%Ndestin - DCpar(jj)%Nstart + 1
                        do k=1,nndest          ! DCpar(jj)%nchlen
                          kk = DCpar(jj)%symbind(k)
                          if(abs(dpnni(kk, i)) > zero) then
                          ! if(dpnni(nn,i) /= zero) then
                            ! ssi = dpi * DCpar(jj)%derv(k)           !  welches von beiden ist korrekt?
                            ssi = dpnni(kk,i) * DCpar(jj)%derv(k)     ! diese Variante ist korrekt
                            ! ssi = dpnni(nn,i) * DCpar(jj)%derv(k)     ! diese Variante ist korrekt
                            dummy = ssi**two * fSD(i)**two
                            dpisum = dpisum + dummy

                            var = dpisum
                          end if
                        end do
                       ! write(66,*) 'dpisum2=',sngl(dpisum2)
                    end if
                end if
                !---------------------------------------------------------------

                varsq = ZERO
                IF(var > ZERO) then
                    varsq = SQRT(var)
                end if
                IF(testout) WRITE(kout,'(a, 10es15.7)') '     dpa,fv1,fv2,dpi,StdUnc(i),fv2-fv1,Ukompon,Ucomb=',  &
                    dpa,fv1,fv2,dpi,fSD(i),fv2-fv1, varsq,sqrt(Ucomb+var)   ! , unit_conv_fact(i)
                !  quadratic addition of variances :
                If(FitcalCurve .and. netto_involved_fitcal .and. i == kbrutto(kEGr)) then
                    ! nothing    see close befor the end of this routine !
                else
                    Ucomb = Ucomb + var
                    perc(i) = var
                    ! additional part according to Kessel et al. (2006):
                    cju(i) = dpi * fSD(i)
                    ry_xi(i) = cju(i)
                end if

                IF(kdoub == 1) THEN
                    Messwert(kbd) = mw_save
                    StdUnc(kbd) = sd_save
                end if

                IF(testout .and. .true. ) WRITE(kout,'(a,i3,5(a,es15.7))') 'i=',i,' sqrt(Ucomb)=',SQRT(Ucomb), &
                    ' cju(i)=',cju(i),'   perc(i)=',perc(i),' StdUnc(i)=',StdUnc(i),' dpi=',dpi

                IF(testout .AND. use_WTLS) WRITE(66,*) 'WTLS:   before 120: Variance and budget contrib. by ',SymboleG(i)%s, &
                    '  cju=',sngl(cju(i))
                !if(use_WTLS .and. .not.iteration_on .and. kableitnum > 0) then
                !   WRITE(23,*) 'UPRopa: before 120 continue: kableitnum=',kableitnum ,' ',trim(symbole(kableitnum)),' var=',sngl(var),'  dpi=',sngl(dpi)
                !end if

120             CONTINUE

                !!!! IF(FitDecay .AND. .NOT.iteration_on .and. StdUnc(i) > zero .and. dep_unc_done) THEN
                IF(FitDecay .AND. StdUnc(i) > ZERO .and. dep_unc_done) THEN
                    ! In this if statement the uncertainty propagation for the net cout rate is performed
                    ! yielding UcombLinf, which then may be compared with that uncertainty obtained
                    ! from the least-squares routine.
                    Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
                    dpi = ZERO
                    fv1 = mwklu
                    ! d Messwert(klinf) / d Messwert(i)
                    call dpi_uprop1(3,i,klinf,0,0,fv1,0, dpi,fv2,dpa)
                    Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)

                    !  quadratic addition of variances :
                    var1 = ( dpi * fSD(i) )**TWO
                    upar = fSD(i)
                    ! if(testout) write(66,*) ' i=',i,'  MW=',sngl(Messwert(i)),' dpa=',sngl(dpa),   &
                    !            '  fv1,fv2=',sngl(fv1),sngl(fv2),'  StdUnc(i)=',sngl(stdUnc(i)) ! ,' xdpa=',sngl(xdpa)
                    if(k_rbl > 0) then
                        IF(i == kpoint(k_rbl)) then
                            ! in this case, fSD(i) is the uncertainty of the net blank count rate
                            var1 = dpi**TWO * (fSD(i)**TWO + sd0zrate(1)**TWO)
                            upar = sqrt(fSD(i)**TWO + sd0zrate(1)**TWO)
                            if(testout) write(66,*) ' i=',i,'   Rbl:   fv2-fv1=',fv2-fv1,'  StdUnc(i)=',sngl(stdUnc(i)),  &
                                '  sd0zrate(1)=',sngl(sd0zrate(1)),'  dpi=',sngl(dpi),'  dpa=',sngl(dpa)
                        end if
                    end if
                    if(i <= ngrs) then
                        do ii=1,nRSsy(klinf)
                            ! contibutions of the right-hand-side symbols of the Linfit function call:
                            ! (called mpfx-paramaters in this program), which do not include the gross count rates
                            IF(ii == k_tmess) CYCLE
                            IF(ii == k_tstart) CYCLE
                            IF(kpoint(ii) == i) THEN
                                do j=1,nhp
                                    if(mpfx(j) == 0) cycle
                                    if(mpfx(j) == RS_SymbolNr(klinf,ii)) then
                                        UcombLinf = UcombLinf + var1
                                        if(testout) WRITE(66,*) 'UcbLF i=',i,' UcombLinf=',  &
                                            sngl(SQRT(UcombLinf)),' ',RSSy(nRSsyanf(klinf)+ii-1)%s,   &
                                            ' contrib. var1=',sngl(var1),' dpi=',sngl(dpi),'  upar=',sngl(upar), &
                                            ' fv1=',sngl(fv1),' fv2=',sngl(fv2),' dpa=',sngl(dpa)
                                    end if
                                end do
                            end if
                        end do
                    end if

                    IF(i > ngrs+ncov .AND. i <= ngrs+ncov+numd ) THEN
                        ! Contributions of the gross count rates:
                        UcombLinf = UcombLinf + var1
                        if(testout) WRITE(66,'(a,i2,a,i2,2x,8(a,es13.6,1x))') 'UcbLF i=',i,' numd=',i-ngrs-ncov,' ZR=',MEsswert(i),  &
                        ! ' var1=',var1,' UcombLinf=', SQRT(UcombLinf),' Beitrag var1=', &
                            ' u=',sqrt(var1/dpi**TWO),' UcombLinf=', SQRT(UcombLinf),' Beitrag var1=', &    ! 25.6.2024
                            var1,' dpi=',dpi,' Fv1=',fv1,' Fv2=',Fv2,' dpa=',dpa
                    END IF
                END IF

                IF(Gamspk1_Fit .AND. .NOT.iteration_on) THEN
                    ! In this section the uncertainty contribution for the net count rate ((Messwert(kgspk1))
                    ! is calculated, which then may be compared with that uncertainty obtained from the
                    ! least-squares routine.
                    Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
                    fv1 = Messwert(klu)
                    ! d Messwert(kgspk1) / d Messwert(i):
                    call dpi_uprop1(3,i,kgspk1,0,0,fv1,0, dpi,fv2,dpa)
                    Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
                    !  quadratic addition of variances :
                    var1 = ( dpi * fSD(i) )**TWO
                    IF(i > ngrs+ncov .AND. i <= ngrs+ncov+numd ) THEN
                        ! contributions of peak net count rates :
                        UcombLinf = UcombLinf + var1
                        !WRITE(66,*) 'xxxx i=',i,' var=',sngl(var1),' UcombLinf=',  &
                        !            sngl(SQRT(UcombLinf)),'   numd=',numd,' StdUnc=',sngl(StdUnc(i)),' dpi=',sngl(dpi)
                    END IF
                end if
            end if
        end do

        IF(ncov > 0 .and. testout) write(66,*) ' ******************  Propagating covariances in Upropa:','   ncov=',ncov
        if(FitDecay .and. .true. .and. testout) write(66,*) ' fpa= ',(sngl(fpa(i)),i=1,3),'  sfpa= ',(sngl(sfpa(i)),i=1,3)

        IF(ncov > 0) THEN

            ! Consideration of covariances:
            ! iim1, iim2 : index values of the two correlated symbols
            ! m1, m2 : if iim1 and iim2 define count rates being dependent
            !          (iim1,iim2 <= nab), m1 and m2 denote the associated independent symbols
            !          (being counts then). Then, the Messwert values of m1 and m2 are
            !          modified for the partial derivatives, and the corresponding modifications
            !          of Messwert values for iim1 and iim2 are used as parameter deviation
            !          dpa=mwert2-mwert1 for calculating the derivative with respect to
            !          iim1 and iim2.
            ! Exception: If one of the two symbols represents the gross count rate (<= nab),
            !            which is dependent,darstellt, then,in case of the detection limit
            !            iterationd, the dependent symbol iim1 or iim2 is used for
            !            calculating the partial derivative.

            Messwert_kbruttoSV = ZERO
            IF(kbrutto(kEGr) > 0) then
                messwert_kbruttoSV = Messwert(kbrutto(kEGr))    ! save a Messwert value
            end if

            nhg = nab+nmodf+nabf
            ! write(66,*) 'Upropa: covarval: ',(covarval(k),k=1,ncov)
            if(testout .and. allocated(covar)) then
                call matwrite(covar,knumEGr,knumEGr,66,'(3es16.8)','matrix covar:')
                write(66,'(a,2x,3es16.8)') 'FPA: ',(fpa(i),i=1,knumEGr)
            end if

            !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

            do k=1,ncov

                messwert_kbd = ZERO
                kbd = 0
                covlk = ZERO
                nhg = nhg + 1
                IF(abs(covarval(k)-missingval)<EPS1MIN .or. abs(covarval(k)-ZERO)<EPS1MIN) CYCLE
                IF(Gamspk1_Fit .AND. ecorruse == 0) CYCLE
                corrx(k) = ZERO
                IF(iteration_on) THEN
                    ! Re-calculate the covariances:
                    select case (icovtyp(k))
                      case (1)    ! type covariance:
                        IF(LEN_TRIM(CVFormel(k)%s) > 0) THEN
                            CovarVal(k) = gevalf(nhg,Messwert)   ! 5.6.2024
                        end if
                      case (2)    ! type correlation:
                        IF(LEN_TRIM(CVFormel(k)%s) == 0 .and. abs(CovarVal(k)-ZERO) > EPS1MIN) THEN
                            CovarVal(k) = Corrval(k) * ( StdUnc(ISymbA(k)) * StdUnc(ISymbB(k)) )         !
                            if(FitDecay .and. knumEGr > 1) then
                                read(Symbole(IsymbA(k))%s(5:5),*) jj1          ! index of the "left" Fitp parameter
                                read(Symbole(IsymbB(k))%s(5:5),*) jj2          ! index of the "right" Fitp parameter
                                covarval(k) = covar(jj1,jj2)
                                if(testout) then
                                    write(66,*)
                                    write(66,*) 'Upropa: covarval from covar * :   k=',k,'   covarval=',sngl(covarval(k)), &
                                        sngl(covar(jj1,jj2))
                                end if
                                corrx(k) = covarval(k)/sqrt(covar(jj1,jj1)*covar(jj2,jj2))
                            end if
                        end if
                        IF(LEN_TRIM(CVFormel(k)%s) > 0 )  THEN
                            CovarVal(k) = gevalf(nhg, Messwert)        ! 5.6.2024
                            CovarVal(k) = CovarVal(k)*fSD(ISymbA(k))*fSD(ISymbB(k))
                        end if
                      case default
                    end select

                end if
                if(.false. .and. testout) then
                    write(66,*) 'Upropa, kqt=',kqt   ! ,':   covarval(1)=',sngl(covarval(1))
                    WRITE(66,*) 'Upropa:   k=',k,'   CVFormel=',CVFormel(k)%s,';  covarVal=',sngl(covarval(k))
                    WRITE(66,*) 'Upropa:   k=',k,'   Std(A)=',sngl(StdUnc(ISymbA(k))),' Std(B)=',sngl(StdUnc(ISymbB(k)))
                    WRITE(66,*) 'Upropa:   k=',k,'   StdSV(A)=',sngl(StdUncSV(ISymbA(k))),' StdSV(B)=',sngl(StdUncSV(ISymbB(k)))
                    WRITE(66,*) 'Upropa:   k=',k,'   IsymbA(k)=',symboleG(ISymbA(k))%s,' IsymbB(k)=',SymboleG(ISymbB(k))%s
                    if(FitDecay .and. knumEGr > 1) write(66,*) 'Upropa:   k=',k,'   corrx(k)=',sngl(corrx(k))
                end if

                iim1 = 0
                iim2 = 0
                chh1 = ucase(SymboleA(k)%s)
                chh2 = ucase(SymboleB(k)%s)
                if(ubound(SymboleG,dim=1) < ngrsP) ngrsP = ubound(SymboleG,dim=1)

                do kk=2,ngrsP
                    IF(TRIM(chh1) == SymboleG(kk)%s) iim1 = kk
                    IF(TRIM(chh2) == SymboleG(kk)%s) iim2 = kk
                end do
                IF(iim1 == 0) WRITE(kout,'(5a,i3,a,i3)') 'Upropa: iim1=0; chh1=',TRIM(chh1),'  chh2=',TRIM(chh2), &
                    '  ngrsP=',ngrsP,' ngrs+ncov+numd=',ngrs+ncov+numd
                IF(iim2 == 0) WRITE(kout,'(5a,i3,a,i3)') 'Upropa: iim2=0; chh1=',TRIM(chh1),'  chh2=',TRIM(chh2),  &
                    '  ngrsP=',ngrsP,' numd=',numd

                if(testout .and.iim1*iim2 > 0) write(66,*) ' '
                if(testout .and.iim1*iim2 > 0) Write(66,*) ' upropa: ncovs:  iim1,iim2=',iim1,iim2,'  ',Symbole(iim1)%s,'  ',Symbole(iim2)%s,'  ncov=',ncov

                IF(iim1 == 0 .OR. iim2 == 0) GOTO 147        ! for Gamspk1 the efficiency covariances are considered directly inLifg1

                if(FitDecay) then
                    ! It is not allowed to derive partial derivatives withe respect to such parameters
                    ! not explicitly occurring in the right-hand-sides of the equatiosn (1 - nab, with
                    ! klinf excluded)
                    nj = 0
                    do kk1=1,nab
                        if(kk1 == klinf) cycle
                        do kk2=1,nRSsy(kk1)
                            IF(TRIM(chh1) == RSSy(nRSSyanf(kk1)+kk2-1)%s) then
                                nj = 1
                                exit
                            end if
                            IF(TRIM(chh2) == RSSy(nRSSyanf(kk1)+kk2-1)%s) then
                                nj = 1
                                exit
                            end if
                        end do
                        if(nj == 1) exit
                    end do
                    ! if(testout) write(66,*)   'Upropa: search for nvh Symbolen: nj=',nj,' chh1,2=',trim(chh1),' ',trim(chh2),' n cov=',k
                    if(nj == 0) cycle    ! the loop do k=1,ncov
                end if

                m1 = iim1
                m2 = iim2
                IF(iim1 <= nab) THEN
                    do i=nab+1,ngrs
                        IF(RS_SymbolNr(iim1,1) == i) m1 = i
                    end do
                    IF(iteration_on .AND. iim1 == kbrutto(kEGr)) m1 = iim1
                END IF
                IF(iim2 <= nab) THEN
                    do i=nab+1,ngrs
                        IF(RS_SymbolNr(iim2,1) == i) m2 = i
                    end do
                    IF(iteration_on .AND. iim2 == kbrutto(kEGr)) m2 = iim2
                END IF

                IF(kbrutto_double > 0 .and. iteration_on .and. .not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit) THEN
                    ch1 = Rseite(kbrutto(kEGr))%s
                    do kk=nab+1,ngrs
                        IF(testSymbol(ch1,symboleG(kk)%s) .and. kk == iim1 .and. StdUnc(kk) > ZERO) THEN
                            kbd = kk
                            Messwert_kbd = Messwert(kk)
                            Messwert(kbd) = Messwert(kbd) * ( Messwert(kbrutto(kEGr)) / MesswertSV(kbrutto(kEGr)) )
                            WRITE(kout,*) 'xxxxxx Upropa, Covars: iim1:  Messwert(kbd)= ',sngl(Messwert(kbd)),'  ',SymboleG(kk)%s, &
                                '  Formel=',TRIM(ch1)
                            EXIT
                        end if
                    end do
                end if

                if(testout) Write(66,'(a,i0,1x,i0,a,i0,a,es12.5)') ' upropa: ncovs:  m1,m2=',m1,m2,'  klu=',klu,' fv1R_SV=',fv1R_SV

                mwert1 = Messwert(iim1)
                fv1 = fv1R_SV     ! 1st function value

                !    if(iteration_on .and. .not. run_corrmat .and. FitDecay .and. iim1 >= kfitp(1) .and.   &
                !                     iim1 <= kfitp(1)+2 .and. iim1 /= klu) ableit_fitp = .true.
                ! partial derivative with respect to Messwert value of iim1 :
                !----  2nd function value:
                ! d Messwert(nn) / d Messwert(m1) :
                call dpi_uprop1(4,m1,nn,klinf-1,nn,fv1,iim1, dpi1,fv2,dpa )
                ableit_fitp = .false.
                if(abs(mwert1-mwert2) < EPS1MIN) exit   ! in this case, Resulta(nn) does not depend on SymboleA(k)
                dpi1v(k) = dpi1
                IF(testout) WRITE(kout,'(a,2i3,1es16.8,a,i3,a,2es16.8,a,es20.12)') 'iim1,m1,mwert1=',i1,m1,mwert1,  &
                    '  nn=',nn,'  Fv1,Fv2=',fv1,fv2,'  dpi1=',dpi1

                ! partial derivative with respect to Messwert value of iim2 :
                IF(kbd == 0 .and. kbrutto_double > 0 .and. iteration_on .and. .not.FitDecay   &
                    .and. .not.Gamspk1_Fit .and. .not.SumEval_fit) THEN
                    ch1 = Rseite(kbrutto(kEGr))%s
                    do kk=nab+1,ngrs
                        IF(testSymbol(ch1,symboleG(kk)%s) .and. kk == iim2) THEN
                            kbd = kk
                            Messwert_kbd = Messwert(kk)
                            Messwert(kbd) = Messwert(kbd) * ( Messwert(kbrutto(kEGr)) / MesswertSV(kbrutto(kEGr)) )
                            WRITE(kout,*) 'xxxxxx Upropa, Covars: iim2: Messwert(kbd)= ',sngl(Messwert(kbd)),'  ', &
                                SymboleG(kk)%s,'  Formel=',TRIM(ch1)
                            EXIT
                        end if
                    end do
                end if

                fv1 = fv1back      ! dummy   !  fv1back refers to ResultA(mwfv) in dpi_uprop1 for mode=4
                mwert1 = Messwert(iim2)
                !    if(iteration_on .and. .not. run_corrmat .and. FitDecay .and. iim2 >= kfitp(1) .and.   &
                !                     iim2 <= kfitp(1)+2 .and. iim2 /= klu) ableit_fitp = .true.
                !----  2. function value:
                ! d Messwert(nn) / d Messwert(m2) :
                call dpi_uprop1(4,m2,nn,klinf-1,nn,fv1,iim2, dpi2,fv2,dpa )    ! partial derivative
                ableit_fitp = .false.

                if(abs(mwert1-mwert2) < EPS1MIN) exit   ! in this case, Resulta(nn) does not depend on SymboleB(k)
                dpi2v(k) = dpi2
                IF(testout) WRITE(kout,'(a,2i3,1es16.8,a,i3,2(a,es20.12))') 'iim2,m2,mwert2=',iim2,m2,mwert2, &
                    '  nn=',nn,'  dpi2=',sngl(dpi2),' covarval(k)=',sngl(covarval(k))

                ! var = dpi1 * dpi2 * CovarVal(k) * 2._rn    ! <--    replaced by the following block:
                !++++++  +++++
                ! 10.6.2024
                ! Additional test whether a covarval(k)-related uncertainty contribution can really
                ! be included. Example: normally covariances between Fitp1, Fitp2 and Fitp3 are not
                ! to be included here. Exception: in project DWD-LSC-3kanal-V2_EN.txp the parameter
                ! Fitp3 serves for calculating the chemical yield; there, the first two output quantities
                ! each depend on two of these fit paramaters.
                mkm1 = 0
                mkm2 = 0
                call dpi_uprop1(4,m1,kEGr,klinf-1,nn,fv1,iim1, dpi1z,fv2,dpa )
                call dpi_uprop1(4,m2,kEGr,klinf-1,nn,fv1,iim2, dpi2z,fv2,dpa )
                if(index(Symbole(m1)%s,'Fitp') > 0) mkm1 = 1
                if(index(Symbole(m2)%s,'Fitp') > 0) mkm2 = 1
                hh1 = abs(dpi1z) > 1.E-14_rn
                hh2 = abs(dpi2z) > 1.E-14_rn
                if(mkm1 + mkm2 <= 1 .or. (mkm1 + mkm2 == 2 .and. hh1 .and. hh2)) then
                    var = dpi1 * dpi2 * CovarVal(k) * 2._rn
                    Ucomb = Ucomb + var
                end if
                !++++++  +++++

                ! IF(iteration_on .AND. kbrutto_double > 0 .AND. Messwert_kbd > zero) THEN
                !   covlk = messwert(kbd)
                !   covlk = + covlk
                !   var = dpi1 * dpi2 * 2._rn * covlk
                ! end if

                covx1(k) = var
                IF(testout) THEN
                    WRITE(kout,*) 'ncov=',k,' Ucomb =',sngl(SQRT(Ucomb)),'ucomb^2=',sngl(Ucomb),'  nn=',nn
                    WRITE(kout,*) 'ncov=',k,' Covvar=',sngl(var),'  fv1back=',sngl(fv1back)
                    WRITE(kout,'(a,3es16.8,3(a,es16.8),/,70x,a, es16.8,a,L1)') 'dpi1,dpi2,CovarVal=',dpi1,dpi2,CovarVal(k),  &
                        ' corrx(k)=',corrx(k),'covarval=',covarval(k),' covlk=',covlk,' U-Beitrag von covlk=',var

                END IF
                ! if(.not.omit_var) perc(ngrs+k) = var
                perc(ngrs+k) = var       ! 10.6.2024
                if(testout) write(66,*) '  ngrs+k=',ngrs+k,'   perc(ngrs+k)=',sngl(perc(ngrs+k))

                IF(testout .AND. use_WTLS) WRITE(66,*) 'WTLS: before 150: Covariance and budget contrib. from ',SymboleG(ngrs+k)%s, &
                    ' covx1=',sngl(covx1(k))

                !!!!!if(.not.omit_var) Ucomb = Ucomb + var
                IF(Gamspk1_Fit .AND. ecorruse == 1) UcombLinf = UcombLinf + var

                ! if(.not.iteration_on .and. FitDecay .and. omit_var) UcombLinf = UcombLinf + var    ! 11.6.2024 weggenommen
                help = EPS1MIN
                if(UcombLinf > EPS1MIN) help = SQRT(UcombLinf)
                if(FitDecay) then
                    if(nRSsyanf(klinf)+ii-1 <= ubound(RSSy,dim=1)) then
                        if(.not.iteration_on .and. testout .and. FitDecay) WRITE(66,'(a,i3,2(a,es15.7),6a)')   &
                            'UcbLF i=',i,' covar=',var,' UcombLinf=',help,' ',RSSy(nRSsyanf(klinf)+ii-1)%s, &
                            ' ',trim(chh1),' ',trim(chh2)
                    end if
                end if
                IF(testout) then
                    help = ZERO
                    if(Ucomb > EPS1MIN) help = sqrt(Ucomb)
                    WRITE(kout,*) 'Ucomb=',sngl(help),'  Ucomb^2=',sngl(help**TWO)
                end if
                if(FitDecay .and. nn == klinf) goto 145
                IF(Ucomb < -EPS1MIN .AND. var < -EPS1MIN) THEN
                    call CharModStr(str1,800)

                    write(str1,*) 'Covar(', trim(chh1),',',trim(chh2),   &
                        ') leads to negative total variance=',Ucomb, &
                        ' !', CHAR(13),CHAR(13),                     &
                        'Are false symbols selected for that? Please, check!'

                    write(66,*)
                    write(66,*) trim(str1)             !   Warning:
                    write(66,*)
                    goto 145
                    ifehl = 1
                    upropa_on = .false.
                    RETURN
                end if
145             CONTINUE
                IF(.not.iteration_on) THEN
                    IF(iim1 == 0) WRITE(kout,*) 'Error with ry_xi(iim1): iim1=0;   k=',k,', iim2=',iim2,   &
                        '   SymbA=',SymboleA(k)%s,'  SymbB=',SymboleB(k)%s
                    if(StdUnc(iim1) > ZERO .and. StdUnc(iim2) > ZERO) then
                        ! additiona part according to Kessel et al. (2006):
                        ry_xi(iim1) = ry_xi(iim1) + cju(iim2)* (CovarVal(k)/(StdUnc(iim1)*StdUnc(iim2)))
                        ry_xi(iim2) = ry_xi(iim2) + cju(iim1)* (CovarVal(k)/(StdUnc(iim1)*StdUnc(iim2)))
                    else
                        ry_xi(iim1) = ZERO
                        ry_xi(iim2) = ZERO
                    end if
                end if
                ! IF(kbrutto_double > 0 .and. iteration_on .and. .not.FitDecay .and. .not.Gamspk1_Fit &
                !   .and. .not.SumEval_fit .and. kbd > 0) THEN
                if(kbd > 0) then   ! 5.6.2024
                    IF(kbrutto_double > 0 .and. iteration_on .and. .not.FitDecay .and. .not.Gamspk1_Fit &
                        .and. .not.SumEval_fit) THEN
                        Messwert(kbd) = MesswertSV(kbd)
                    end if
                end if
147             CONTINUE
            end do    ! k=1,ncov
            !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        end if

        Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)     ! restore the messwert array

        condecay = .false.
        congamma = .false.
        if(FitDecay .and. knumEgr == 1 .and. nn == kEGr) condecay = .true.
        if(Gamspk1_Fit .and. WMextSD == 1) congamma = .true.

        IF( condecay .or. congamma )  THEN

            ! FitDecay: Special case when only one fit parameter is really selected for being fitted,
            ! in which case no Fitpx parameters occur in the symbol list :
            ! This part earlier came from the beginning of Upropoa;
            ! this shift has the advantage, that StdUnc(klu) now receives that value correctly adapated
            ! to the decision threshold.
            ! This seems to make one call Linf() in ModVar unnecessary.

            ! Note for FitDecay:
            ! The content of this if statement could be removed,
            ! if also in this case all three patameters Fitp1..Fitp3 would be included in the symbol table!

            if(condecay) knet = klinf
            if(congamma) knet = kgspk1
            dummy = Resulta(1)
            fv1 = Messwert(1)
            if(condecay) then
                ! d Messwert(1) / d Messwert(knet) :
                call dpi_uprop1(1,knet,1,knet-1,1,fv1,0, dpi,fv2,dpa )
            elseif(congamma) then
                ! d Messwert(1) / d Messwert(knet) :
                call dpi_uprop1(2,knet,1,knet-1,1,fv1,0, dpi,fv2,dpa )
            end if
            sensi(knet) = dpi
            var = ( dpi * StdUnc(knet) )**TWO
            Ucomb = Ucomb + var
            if(testout) then
                IF(iteration_on .AND. limit_typ == 1) WRITE(66,*) 'Upropa: MW(knet)=',sngl(Messwert(knet)), &
                    ' dpi=',sngl(dpi),'  sqrt(var)=',sngl(SQRT(var)), &
                    ' Ucomb=',sngl(SQRT(Ucomb)),' ucombvorher=',sngl(SQRT(Ucomb - var))
                IF(iteration_on .AND. limit_typ == 1) WRITE(66,*) ' dpafact=',sngl(dpafact(Messwert(knet))),' dpa=',sngl(dpa), &
                    ' StdUnc(knet)=',sngl(StdUnc(knet)),  &
                    ' Fv1=',sngl(Fv1),' Fv2=',sngl(Fv2)
            end if
            IF(.false. .and. iteration_on .AND. limit_typ == 1) then
                do i=1,ngrs
                    write(66,'(a,i3,a,es15.8)') 'i=',i,'  Messwertkp=',sngl(Messwertkp(i))
                end do
            end if
            perc(knet) = var
            ! additional part according to Kessel et al. (2006):
            cju(knet) = dpi * fSD(knet)
            ry_xi(knet) = cju(knet)
            IF(testout .and. knet > 0) THEN
                WRITE(kout,*) '### Contrib. of parameter i=knet=',knet,  &
                    '  MW(knet)=',sngl(Messwert(knet)),'  sqrt(Ucomb)=',sngl(SQRT(Ucomb)), &
                    ' StdUnc(knet)=',sngl(StdUnc(knet)),' dpi=',sngl(dpi),' var=',sngl(var)
            end if
            Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)   ! restore Messwert

        end if

        if(FitCalCurve) then
            if(kfitcal > 0 .and. (nn == kfitcal .or. nn == kEGr) ) then
                call CalibInter(KFMode, Messwert(kpointKB(1)), StdUnc(kpointKB(1)), yval,uyval)
                ! Differentiate between the two cases of nn:
                if(nn == kEGr) then
                    fv1 = Messwert(kEGr)
                    ! d Messwert(kEGr) / d Messwert(kfitcal) :
                    call dpi_uprop1(2,kfitcal,kEGr,kfitcal-1,1,fv1,0, dpi,fv2,dpa)
                    Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
                    var = (dpi * uyval)**TWO
                    Ucomb = Ucomb + var
                    Perc(kfitcal) = var
                    sensi(kfitcal) = dpi
                    ! additional part according to Kessel et al. (2006):
                    cju(kfitcal) = dpi * uyval
                    ry_xi(kfitcal) = cju(kfitcal)
                elseif(nn == kfitcal) then
                    var = uyval**TWO
                    Ucomb = Ucomb + uyval**TWO
                    dpi = ZERO
                end if
            end if
            Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
        end if

        if(.false. .and. SumEval_fit) then
            if(ksumeval > 0 .and. (nn == ksumeval .or. nn == kEGr) ) then
                call SumEvalCalc(yval,uyval)
                ! Differentiate between the two cases of nn:
                if(nn == kEGr) then
                    fv1 = Messwert(kEGr)
                    ! d Messwert(kEGr) / d Messwert(ksumeval)
                    call dpi_uprop1(2,ksumeval,kEGr,ksumeval-1,1,fv1,0, dpi,fv2,dpa)
                    Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
                    var = (dpi * uyval)**TWO
                    Ucomb = Ucomb + var
                    Perc(ksumeval) = var
                    sensi(ksumeval) = dpi
                    ! additional part according to Kessel et al. (2006):
                    cju(ksumeval) = dpi * uyval
                    ry_xi(ksumeval) = cju(ksumeval)
                elseif(nn == ksumeval) then
                    var = uyval**TWO
                    Ucomb = Ucomb + uyval**TWO
                    dpi = ZERO
                end if
            end if
        end if

        Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)   ! restore Messwert())

        IF(iteration_on) then
            ! if(kbrutto(kEGr) <= nab .AND. .not.FitDecay .AND. .not.Gamspk1_Fit .and. .not.SumEval_fit) THEN
            if(kbrutto(kEGr) > 0 .and. kbrutto(kEGr) <= nab .AND. .not.FitDecay &
                         .and. .not.DChain   &                         ! 28.12.2024 GK
                         .AND. .not.Gamspk1_Fit) THEN    ! 9.1.2024
                kableitnum = kbrutto(kEGr)
                ! treat one additional step in the case of the iteration_on=T required,
                ! required: s. oben.       @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                k = kbrutto(kEGr)
                if(abs(Messwert(k)) > EPS1MIN .OR. (abs(Messwert(k)) < EPS1MIN .AND. StdUnc(k) > ZERO) ) then
                    !  numerical partial derivative dpi for parameter p(i):
                    !
                    !----  1st function value:
                    fv1 = fv1R_SV
                    !
                    !----  2nd function value:
                    ! d Messwert(nn) / d Messwert(k) :
                    call dpi_uprop1(3,k,nn,0,0,fv1,0, dpi,fv2,dpa)
                    sensi(k) = dpi
                    !  quadratic addition of variances :
                    var = ( dpi * fSD(k) )**TWO
                    perc(k) = var
                    Ucomb = Ucomb + var
                    IF(testout) THEN
                        WRITE(kout,*) '### Contrib. of parameter i=kbrutto(kEGr)=',kbrutto(kEGr),  &
                            '  MW(kbrutto(kEGr))=',sngl(Messwert(kbrutto(kEGr))),'  sqrt(var)=',sngl(SQRT(var)), &
                            ' StdUnc(kbrutto(kEGr)=',sngl(StdUnc(kbrutto(kEGr))),' dpi=',sngl(dpi), &
                            ' sqrt(Ucomb)=',sngl(sqrt(Ucomb))
                    end if
                end if
                Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
            end if
        END IF

        do k=nab,kEGr+1,-1

            if(Dchain) cycle        ! 27.4.2025  GK

            if(iteration_on .and. k == kbrutto(kEGr)) cycle       ! <--  this case has just been processed in the loop above this one
            if(k == kbrutto(kEGr) .and. .not.use_sdf_brutto) cycle
            if(.not.use_dependent_sdwert) cycle

            if(k == kbrutto(kEGr) .and. use_sdf_brutto) then
                if(StdUnc(k) <= ZERO) StdUnc(k) = gevalf(kbrutto_gl(kEGr),Messwert)
            end if
            if(use_sdwert_nn(k) .or. k == kbrutto(kEGr)) then
                if(abs(Messwert(k)) > EPS1MIN .OR. (abs(Messwert(k)) < EPS1MIN .AND. StdUnc(k) > ZERO) ) then
                    !   WRITE(66,*) 'contribution of symbol i=',i
                    !  numerical partial derivative dpi for parameter p(i):
                    !
                    if(iteration_on) then
                        Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
                    end if
                    kableitnum = k

                    !----  1st function value:
                    fv1 = fv1R_SV
                    !
                    !----  2nd function value:
                    if(k /= kbrutto(kEGr)) then
                        ! d Messwert(nn) / d Messwert(k) :
                        call dpi_uprop1(3,k,nn,0,0,fv1,0,dpi,fv2,dpa)
                    else
                        ! d Messwert(nn) / d Messwert(k) :
                        call dpi_uprop1(2,k,nn,k-1,nn,fv1,0, dpi,fv2,dpa)
                    end if
                    sensi(k) = dpi
                    if(k /= kbrutto(kEGr)) StdUnc(k) = SDwert(k)
                    !  quadratic addition of variances :
                    var = ( dpi * fSD(k) )**TWO
                    Ucomb = Ucomb + var
                    perc(k) = var
                    sensi(k) = dpi
                    ! additional part according to Kessel et al. (2006):
                    cju(k) = dpi * fSD(k)
                    ry_xi(k) = cju(k)
                    Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)  ! restore Messwert

                    IF(testout) THEN
                        WRITE(kout,*) '### nn=',int(nn,2),'  Contrib. of parameter k=',k,  &
                            '  MW(k)=',sngl(Messwert(k)),'  sqrt(var)=',sngl(SQRT(var)), &
                            ' StdUnc(k)=',sngl(StdUnc(k)),' dpi=',sngl(dpi), &
                            ' sqrt(Ucomb)=',sngl(sqrt(Ucomb))
                    end if
                end if
            end if
        end do

! if the uncertainty is calculated for an nn /= kEGr, a budget need not be established.
        IF(nn /= kEGr) goto 55

        IF(testout) write(66,*) ' ******************  Budget in Upropa:   ngrs=',ngrs
        IF(.NOT. iteration_on) THEN
            percsum = ZERO
            Uc2 = ZERO
            ptmin = 5.E-07_rn
            ! ptmin = 5.E-10_rn

            if(FitCalCurve .and. kfitcal > 0) then
                perc(kfitcal) = perc(kfitcal) / Ucomb*100._rn
                percsum = percsum + Perc(kfitcal)
                IF(ABS(perc(kfitcal)) < ptmin) perc(kfitcal) = ZERO
                Ucontrib(kfitcal) = ABS(cju(kfitcal))
                IF(ABS(Ucontrib(kfitcal)) < ptmin*SQRT(Ucomb)) Ucontrib(kfitcal) = ZERO
                Uc2 = Uc2 + Ucontrib(kfitcal)**TWO
            end if

            do i=kEGr+1,ngrs+ncov+numd
                if(i <= nab) then
                    if(i > ubound(SDformel,dim=1)) cycle
                    if(len_trim(SDformel(i)%s) == 0) cycle
                end if
                if(i == kbrutto(kEGr) .and. .not.use_sdf_brutto .and. i <= nab) cycle
                IF(ncov == 0) THEN
                    perc(i) = perc(i)/Ucomb*100._rn
                    IF(ABS(perc(i)) < ptmin) perc(i) = ZERO
                    Ucontrib(i) = ABS(cju(i))
                    IF(ABS(Ucontrib(i)) < ptmin*SQRT(Ucomb)) Ucontrib(i) = ZERO
                    Uc2 = Uc2 + Ucontrib(i)**TWO
                else
                    IF(i > ngrs .AND. i <= ngrs+ncov) THEN
                        perc(i) = ZERO
                        IF(abs(covx1(i-ngrs)) > EPS1MIN) THEN
                            Ucontrib(i) = SIGN(SQRT(ABS(covx1(i-ngrs))), covx1(i-ngrs))
                            Uc2 = Uc2 + SIGN(ABS(covx1(i-ngrs)), covx1(i-ngrs))
                        end if
                    else
                        if(testout .and. abs(cju(i)-ZERO) > EPS1MIN)  write(66,*) ' i=',i,'  cju(i)=',sngl(cju(i)),'  ry_xi(i)=',sngl(ry_xi(i)),  &
                            '  Ucomb=',sngl(Ucomb),' perc(i)=',sngl(perc(i))
                        if(abs(Ucomb) > EPS1MIN) then
                            perc(i) = cju(i)*ry_xi(i)/Ucomb*100._rn
                        end if
                        IF(ABS(perc(i)) < ptmin) perc(i) = ZERO
                        Ucontrib(i) = ABS(cju(i))
                        IF(ABS(Ucontrib(i)) < ptmin*SQRT(Ucomb)) Ucontrib(i) = ZERO
                        Uc2 = Uc2 + Ucontrib(i)**TWO
                    end if
                end if
                percsum = percsum + perc(i)
                IF(abs(perc(i)) > EPS1MIN .AND. testout) WRITE(66,*) '    i=',i,'  perc(i)=',sngl(perc(i))
            end do

            IF(FitDecay) then
                if(kfitp(1) > 0 .AND. knumEgr == 1 ) THEN
                    ! amendment for this special case
                    perc(klinf) = perc(klinf)/Ucomb*100._rn
                    IF(ABS(perc(klinf)) < ptmin) perc(klinf) = ZERO
                    percsum = percsum + perc(klinf)
                    Ucontrib(klinf) = ABS(cju(klinf))
                    IF(ABS(Ucontrib(klinf)) < ptmin*SQRT(Ucomb)) Ucontrib(klinf) = ZERO
                    Uc2 = Uc2 + Ucontrib(klinf)**TWO
                end if
            end if

            Ucontrib(kEgr) = SQRT(Uc2)
            IF(testout) WRITE(66,*) '     percsum=',sngl(percsum)

        end if

!IF(.not.iteration_on) THEN
        ! Testausgaben
        ! WRITE(kout,*) '   Ucomb=',SQRT(ucomb),'    percsum=',percsum
        ! WRITE(kout,*) 'Correlation coefficients r(y,xi):'
        ! do i=nab+1,ngrs
        !   WRITE(kout,*) '  ',TRIM(Symbole(i)),' corr=',sngl(ry_xi(i)/SQRT(Ucomb)),'  perc=',sngl(perc(i)),' ry_xi=',sngl(ry_xi(i))
        ! end do
!end if

55      continue

        IF(iteration_on) then
            if(kbrutto(kEGr) <= nab .AND. .not.FitDecay .AND. .not.Gamspk1_Fit .and. &
                .not.DChain .and.               &                     ! 27.4.2025
                .not.SumEval_fit  ) THEN
                k = kbrutto(kEGr)
                perc(k) = perc(k)/Ucomb*100._rn
                percsum = percsum + perc(k)
            end if
        end if


        IF( (FitDecay .OR. Gamspk1_Fit) .AND. .not.iteration_on ) THEN
            UcombLinf = SQRT(UcombLinf)
            UcombLinf_kqt1 = UcombLinf
        END IF
        Ucomb = SQRT(Ucomb)
        kableitnum = 0
! restoring:
        Messwert(1:ngrs+ncov+numd*knd) = MesswertKP(1:ngrs+ncov+numd*knd)
        IF(iteration_on) THEN
            CovarVal(1:ncov) = covarValSV(1:ncov)
        end if

        upropa_on = .FALSE.

        if(testout)   &
            WRITE(66,*) '   at the end of UncPropa: nn=',nn,'  Ucomb=',sngl(Ucomb),'   percsum=',sngl(percsum),  &
            '  UcombLinf=',sngl(UcombLinf)

! if(.true. .and. kqt == 1) then
!   do i=1,ngrs+ncov+numd*0
!     write(66,*) 'uncwb-end:  i=',int(i,2),' MW(i)=',sngl(Messwert(i)),'  MWSV(i)=',sngl(MesswertSV(i)), &
!                            ' MW - MWSV=',sngl(Messwert(i)-MesswertSV(i)),' SD=',sngl(StdUnc(i))
!   end do
! end if



    end subroutine upropa


!#######################################################################

    subroutine RbtCalc(RblTot)

        ! this routine returns total background values for three output quantities.
        ! They are determined simply as the difference between the values of the
        ! gross count rate and the net count rate.
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_Linft,       only: FitDecay,k_rbl
        use UR_Gleich_globals
        use UR_Gspk1Fit,    only: Gamspk1_Fit
        use UR_DLIM,        only: FakRB,GamDist_Zr
        use ur_general_globals, only: MCsim_on,Gum_restricted

        implicit none

        real(rn),INTENT(OUT)     :: RblTot(3)

        integer         :: i,kkne,kE
        real(rn)        :: help
!-----------------------------------------------------------------------
        RblTot = ZERO
        if(Gum_restricted) return

        do kE=1,knumEGr
            kkne = 0
            RblTot(kE) = ZERO
            IF(FitDecay .and. k_rbl > 0) THEN
                RblTot(kE) = Messwert(kpoint(k_rbl))
                RblTot(kE) = ZERO
            else IF(Gamspk1_Fit) THEN
                RblTot(kE) = ZERO
            else
                if(kbrutto(kE) > 0) then
                    help = Messwert(kbrutto(kE))
                    RblTot(kE) = ( help*FakRB - Messwert(knetto(kE)) ) / FakRB**ZERO
                end if
                IF(GamDist_ZR .AND. MCsim_on) THEN
                    ! kkne: Messwert index of the background countin time, belonging to the background counts
                    ! It is assumed that exactly two counting number variables are selected in the GUI, TAB Equations.
                    do i=nab+1,ngrs
                        IF(iptr_time(i) > 0 .AND. ivtl(i) == 4 .AND. iptr_time(i) /= kbrutto(kE)) kkne = iptr_time(i)
                    end do
                    ! RbltotGDA(kE) = GamDistAdd / Messwert(kkne)
                    !  ??? what happens after introduction of Fconst and Flinear?????
                end if
            END IF

            ! if(kbrutto(kEGr) > 0) write(66,*) 'RbtCalc: Rbltot=',sngl(Rbltot),'   Messwert(kbrutto(kEGr))=',sngl(Messwert(kbrutto(kEGr)))

            IF(.false. .and. GamDist_ZR) THEN
                WRITE(66,*) 'RbtCalc: kkne=',kkne,'   RblTot=',(sngl(RblTot(kE))),'  NetRate=',sngl(Messwert(knetto(kE)))
                do i=1,ngrs
                    IF(iptr_time(i) > 0) WRITE(66,*) '   ',Symbole(i)%s,' i=',i,'  iptr_time(i)=',iptr_time(i)
                end do
            end if
        end do

    END subroutine RbtCalc


    !#######################################################################

    subroutine CorrectLists(trow,brow)

        ! this routine is called by AdjustRemoveTVRows, which means that
        ! after having removed a set of rows with in a treeview, it is
        ! necessary to re-adjsut all the correpending arrays depending on which
        ! treeview was edited.
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use ur_general_globals, only: actual_grid
        use UR_Gleich_globals
        use Sym1,       only: Readj_kbrutto,Readj_knetto
        ! use UR_Linft,    only: FitDecay
        ! use UR_Gspk1fit, only: Gamspk1_Fit
        use Rout,       only: WTreeViewPutStrArray,WTreeViewPutDoubleArray,WTreeViewPutComboArray, &
                              WDListstoreFill_1
        use CHF,        only: ucase

        implicit none

        integer   ,intent(in)      :: trow      ! first row in a grid, to be erased
        integer   ,intent(in)      :: brow      ! last  row in a grid, to be erased

        integer               :: i,k,j,kk,k1,ngrs_new
        !----------------------------------------------------------------------

                ngrs_new = ngrs - (trow-brow+1)

        !  write(66,*) 'CorrectLists: Before executing eraseing: ngrs=',int(ngrs,2),' ngrs_CP=',int(ngrs_CP,2)
        !  do i=1,ngrs
        !    write(66,*) 'i=',int(i,2),'Mw(i)=',sngl(Messwert(i)),' Sdw(i)=',sngl(SDwert(i)),' ', &
        !         Symbole(i)%s, &
        !               '  ','Mw_CP(i)=',sngl(Messwert_cp(i)),' Sdw_CP(i)=',sngl(SDwert_CP(i)),' ', &
        !         Symbole_CP(i)%s
        !  end do

        if(ngrs_CP > 0) then
            k = 0
            do kk=1,ngrs
                if(kk >= trow .and. kk <= brow) cycle
                do k1=1,nsyn
                    if(TRIM(ucase(symbole(kk)%s)) == TRIM(ucase(symb_n(k1)%s)) ) THEN
                        k = k + 1
                        Symbole(k)%s = Symbole(kk)%s
                        symtyp(k)%s = symtyp(kk)%s
                        exit
                    end if
                end do

                do j=1,ngrs_CP
                    IF(TRIM(ucase(symbole(kk)%s)) == TRIM(ucase(symbole_CP(j)%s)) ) THEN
                        ! k are here now the remaining symbols (being sorted)
                        k = k + 1
                        if(k > ngrs_new) goto 25

                        Symbole(k)%s = Symbole_CP(j)%s
                        symtyp(k)%s = symtyp_CP(j)%s
                        einheit(k)%s = einheit_CP(j)%s
                        bedeutung(k)%s = bedeutung_CP(j)%s
                        Messwert(k) = Messwert_CP(j)
                        ivtl(k) = ivtl_CP(j)
                        SDFormel(k)%s = SDFormel_CP(j)%s
                        SDWert(k) = SDWert_CP(j)
                        HBreite(k) = HBreite_CP(j)
                        IAR(k) = IAR_CP(j)
                        STDUnc(k) = STDUnc_CP(j)

                        ! I skip the following two!
                        ! sensi(k) = sensi_CP(j)
                        ! perc(k) = perc_CP(j)
                        exit
                    end if
                end do
            end do
        end if
25      continue

        do i=ngrs_new+1,ngrs
            Symbole(i)%s = ' '
            symtyp(i)%s = ' '
            einheit(i)%s = ' '
            bedeutung(i)%s = ' '
            Messwert(i) = missingval
            ivtl(i) = 1
            SDFormel(i)%s = ' '
            SDWert(i) = missingval
            HBreite(i) = missingval
            IAR(i) = 1
            STDUnc(i) = missingval
        end do
        call WDListstoreFill_1('liststore_symbols', ngrs_new, symbole)

        call Readj_knetto()
        call Readj_kbrutto()
        if(knetto(kEGr) > 0) write(66,*) 'UWB:correctlists:  knetto(kEGr)=',symbole(knetto(kEGR))%s,' Name=',knetto_name(kEGR)%s
        if(kbrutto(kEGr) > 0) write(66,*) 'UWB:correctlists:  kbrutto(kEGr)=',symbole(kbrutto(kEGR))%s,' Name=',kbrutto_name(kEGR)%s
!write(66,*) 'After CorrectLists:  '
!do i=1,max(ngrs,ngrs_CP)
!  WRITE(66,'(a,i3,2x,2(a,a,2x,a,Es11.4,2x))') 'i=',i,'  symbole(i) = ',symbole(i)%s, &
!            '  MW(i)=',Messwert(i),'  symbole_CP(i) = ',symbole_CP(i)%s,'  MW_CP(i)=',Messwert_CP(i)
!end do
!write(66,*)

! ---------------
        if(trim(actual_grid) == 'treeview1') then
            Messwert_CP(1:ngrs) = Messwert(1:ngrs)
            ivtl_CP(1:ngrs) = ivtl(1:ngrs)
            SDWert_CP(1:ngrs) = SDWert(1:ngrs)
            HBreite_CP(1:ngrs) = HBreite(1:ngrs)
            IAR_CP(1:ngrs) = IAR(1:ngrs)
            STDUnc_CP(1:ngrs) = STDUnc(1:ngrs)

            do k=1,ngrs
                Symbole_CP(k)%s = Symbole(k)%s
                symtyp_CP(k)%s = symtyp(k)%s
                einheit_CP(k)%s = einheit(k)%s
                bedeutung_CP(k)%s = bedeutung(k)%s
                SDFormel_CP(k)%s = SDFormel(k)%s
                ! I skip the following two!
                ! sensi_CP(k) = sensi(k)
                ! perc_CP(k) = perc(k)
            end do
            ngrs_CP = ngrs
            call WTreeViewPutStrArray('treeview2', 2, ngrs, symbole)
            call WTreeViewPutStrArray('treeview2', 3, ngrs, symtyp)
            call WTreeViewPutStrArray('treeview2', 4, ngrs, einheit)
            call WTreeViewPutDoubleArray('treeview2', 5, ngrs, Messwert)
            call WTreeViewPutComboArray('treeview2', 6, ngrs, IVTL)
            call WTreeViewPutStrArray('treeview2', 7, ngrs, SDformel)
            call WTreeViewPutDoubleArray('treeview2', 8, ngrs, SDwert)
            call WTreeViewPutDoubleArray('treeview2', 8, ngrs, HBreite)
            call WTreeViewPutComboArray('treeview2', 10, ngrs, IAR)
            call WTreeViewPutDoubleArray('treeview2', 11, ngrs, StdUnc)
        end if
! ---------------

!  write(66,*) 'After executing erasing in Correctlists:  ngrs=',int(ngrs,2),' ngrs_CP=',int(ngrs_CP,2)
!  do i=1,ngrs
!    write(66,*) 'i=',int(i,2),'Mw(i)=',sngl(Messwert(i)),' Sdw(i)=',sngl(SDwert(i)),' ', &
!         Symbole(i)%s, &
!               '  ','Mw_CP(i)=',sngl(Messwert_cp(i)),' Sdw_CP(i)=',sngl(SDwert_CP(i)),' ', &
!         Symbole_CP(i)%s
!  end do

    end subroutine CorrectLists

!#######################################################################

    subroutine corrmatEGr

        ! this routine is only called for displaying a confidence ellipse,
        ! it prepares several small-dim matrices/covariance matrices,
        ! like Jmat,Qsmat,Qyq,covEGr,....
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_Gleich_globals, only: nab,ngrs,kEGr,knumEGr,kableitnum, Messwert,StdUnc, &
                                     nabmx,nmumx,missingval,ncov, &
                                     Ucomb,kEGrSV,MesswertSV,StdUncSV
        use UR_Linft,       only: valEGr,uncEGr,corrEGR,covEGr,nhp,mpfx,numd,kfitp,covFPA,FitDecay,run_corrmat
        use UR_DLIM,        only: iteration_on
        use ur_general_globals,   only: ableit_fitp
        use Top,            only: dpafact
        use Num1,           only: matwrite

        implicit none

        integer            :: i,k,j,ne1,ne2,n1,n2,nn,kk,nn1
        real(rn)           :: MesswertKP((nabmx+nmumx)), Fv11,Fv12,dpa1,dpa2,dpi1,dpi2, &
            Fv21,Fv22, upr(3),Jmat(3,3), &
            qsmat(3,ngrs-nab),Umq(ngrs-nab,ngrs-nab),Uyq(3,3)
        logical            :: testout    !
!-----------------------------------------------------------------------
        kEGrSV = kEGr
        testout = .false.
        testout = .true.
        if(testout) write(66,*) 'CorrmatEGR:  kEGr=',kEGr,'--------------------------------------------'

        if(testout .and. FitDecay) then
            call matwrite(covFPA,3,3,66,'(3es16.8)','Covariance matrix of parameters Fitpi:')
        end if

        corrEGr = ZERO
        covEGr = ZERO

        do i=1,ngrs+ncov+numd
            if(abs(messwert(i)-MesswertSV(i)) > 0.001_rn*abs(Messwert(i))) &
                write(66,*) 'i=',i, ' values different : MW=',sngl(Messwert(i)),' mwSV=',sngl(MesswertSV(i))
            ! Messwert(i)   = MesswertSV(i)
            ! StdUnc(i)     = StdUncSV(i)
            !MesswertKP(i) = Messwert(i)
        end do
        Messwert(1:ngrs+ncov+numd)   = MesswertSV(1:ngrs+ncov+numd)
        StdUnc(1:ngrs+ncov+numd)     = StdUncSV(1:ngrs+ncov+numd)
        MesswertKP(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)

        run_corrmat = .true.
        iteration_on = .false.
        ableit_fitp = .true.
! create the arrays valEGr and uncEGr/upr:
        do nn=knumEGr,1,-1
            kEgr = nn
            valEGr(nn) = ResultA(nn)
            call upropa(nn)
            upr(nn) = Ucomb
            uncEGr(nn) = Ucomb
        end do
        kEGr = kEGrSV
        Messwert(1:ngrs+ncov+numd) = MesswertKP(1:ngrs+ncov+numd)      ! restore Messwert array
!do j=1,ngrs+ncov+numd
!  Messwert(j) = MesswertKP(j)      ! restore Messwert array
!end do
        if(testout) write(66,*) 'Vector of uncertainties: ',(sngl(upr(k)),k=1,knumEGr)

        if(testout) then
            write(66,*)
            write(66,'(a,a)') '      ','ne1 ne2  n1 n2   mw(n1)      mw(n2)      dpa1        dpa2       ' &
                // '             Fv12        Fv11        Fv22        Fv21        dpi1        dpi2        Beitrag'
        end if

        Jmat = ZERO       ! Matrix of partial derivatives of ys with respect to y (only /=null for FitDecay);  ys = D x y
        qsmat = ZERO      ! Matrix of partial derivatives of ys with respect to q, which exclusively are contained in D.
        Umq = ZERO        ! Diagional covariance matrix of parameters q, which exclusively are contained in D.
        do ne1=knumEGr,1,-1
            FV11 = valEGr(ne1)
            ! write(66,*) 'ne1=',ne1,'---------------------------------------'
            covEGr(ne1,ne1) = upr(ne1)**TWO

            do ne2=knumEGr,1,-1
                FV21 = valEGr(ne2)

                do n1=nab+1,ngrs
                    nn1 = n1 - nab
                    if(abs(StdUnc(n1)) < EPS1MIN .or. abs(StdUnc(n1)-missingval) < EPS1MIN) cycle
                    dpi1 = ZERO
                    kableitnum = n1
                    ! select the output quantity number ne1:
                    ! partial derivative of eq. ne1 with respect to Messwert(n1):
                    kEGr = ne1
                    fv11 = ResultA(ne1)
                    if(abs(fv11/valEGr(ne1) -ONE) > 1.E-6_rn) write(66,*) ' ne1=',ne1,'  fv11 /= valEgr',sngl(fv11),sngl(valEGr(ne1))
                    dpa1 = Messwert(n1) * dpafact(Messwert(n1)) - Messwert(n1)
                    Messwert(n1) = Messwert(n1) + dpa1
                    fv12 = ResultA(ne1)
                    Messwert(n1) = Messwert(n1) - dpa1
                    dpi1 = (Fv12-Fv11)/dpa1
                    do j=1,ngrs+ncov+numd
                        Messwert(j) = MesswertKP(j)        ! restore Messwert array
                    end do
                    ! if(n1 >= kfitp(1) .and. n1 <= kfitp(1)+2) write(66,*) 'ne1=',ne1,' n1=',n1,'  dpi1=',sngl(dpi1)
                    if(FitDecay) then
                        if(n1 == kfitp(1)-1+ne2 .and. ne1 /= ne2) Jmat(ne1,ne2) = dpi1
                    end if
                    if(abs(dpi1) < EPS1MIN) cycle
                    if(.not.FitDecay .or. ( FitDecay .and. (n1 < kfitp(1) .or. n1 > kfitp(1)+2))) then
                        kk = 0
                        do j=1,nhp
                            if(mpfx(j) == n1) kk = 1
                        end do
                        if(kk == 0) then
                            qsmat(ne1,nn1) = dpi1
                            if(ne1 == 1) Umq(nn1,nn1) = StdUnc(n1)**TWO
                        end if
                    end if

                    ! write(66,*) '   ne2=',ne2,'.....................................'
                    do n2=nab+1,ngrs
                        if(abs(StdUnc(n2)) < EPS1MIN .or. abs(StdUnc(n2)-missingval) < EPS1MIN) cycle
                        dpi2 = ZERO
                        kableitnum = n2
                        ! select the output quantity to number ne2:
                        ! partial derivative of eq. ne2 with respect to Messwert(n2):
                        kEGr = ne2
                        Fv21 = ResultA(ne2)
                        if(abs(fv21/valEGr(ne2) -ONE) > 1.E-6_rn) write(66,*) ' ne2=',ne2,'  fv21 /= valEgr',sngl(fv21),sngl(valEGr(ne2))
                        dpa2 = Messwert(n2) * dpafact(Messwert(n2)) - Messwert(n2)
                        Messwert(n2) = Messwert(n2) + dpa2
                        Fv22 = ResultA(ne2)
                        Messwert(n2) = Messwert(n2) - dpa2
                        dpi2 = (Fv22-Fv21)/dpa2
                        do j=1,ngrs+ncov+numd
                            Messwert(j) = MesswertKP(j)
                        end do

                        if(abs(dpi2) < EPS1MIN) cycle
                        if(FitDecay) then
                            if(testout .and.  n1 >= kfitp(1) .and. n1 <= kfitp(1)+2 .and. n2 >= kfitp(1) .and. n2 <= kfitp(1)+2)  &
                                write(66,*) 'ne1=',ne1,' ne2=',ne2,' n1=',n1,' n2=',n2,' dpi1=',sngl(dpi1),'  dpi2=',sngl(dpi2)

                            if(ne1 == knumEGr .and. (n1 >= kfitp(1) .and. n1 <= kfitp(1)+2) .and.   &
                                (n2 >= kfitp(1) .and. n2 <= kfitp(1)+2))  then
                                if(ne2 == knumEGr-1) then
                                    if(abs(Jmat(ne1,ne1)) < EPS1MIN ) Jmat(ne1,ne1) = dpi1
                                    if(abs(Jmat(ne2,ne2)) < EPS1MIN ) Jmat(ne2,ne2) = dpi2
                                end if
                                if(ne2 == knumEGr-2) then
                                    if(abs(Jmat(ne2,ne2)) < EPS1MIN ) Jmat(ne2,ne2) = dpi2
                                end if
                            end if
                        end if

                    end do   ! n2
                end do    ! n1

            end do      ! ne2
            if(testout) Write(66,*)
        end do      ! ne1

        if(testout) then
            write(66,*) 'matrix qsmat'
            do i=nab+1,ngrs
                nn1 = i - nab
                write(66,'(i2,2x,3es16.8)') i,(qsmat(k,nn1),k=1,3)
            end do
        end if

        Uyq =Matmul(qsmat, Matmul(Umq, Transpose(qsmat)))
        covEGR = Matmul(Jmat, Matmul(covFPA, Transpose(Jmat))) + Uyq

        do i=1,knumEgr
            do k=1,knumEGr
                corrEGr(i,k) = covEGr(i,k) /sqrt(covEGR(i,i)*covEGR(k,k))
            end do
        end do
        kableitnum = 0
        iteration_on = .false.
        kEGr = kEGrSV
        ableit_fitp = .false.
        run_corrmat = .false.

        if(testout) then
            write(66,*) 'Vector of values         : ',(sngl(valEGr(k)),k=1,knumEGr)
            write(66,*) 'Vector of uncertainties  : ',(sngl(uncEGr(k)),k=1,knumEGr)
            call matwrite(Uyq,knumEGr,knumEGr,66,'(3es16.8)','Matrix Uyq:')
            call matwrite(corrEGr,knumEGr,knumEGr,66,'(3es16.8)','Correlation matrix of output quantities:')
            call matwrite(covEGr,knumEGr,knumEGr,66,'(3es16.8)','Covariance matrix covEGR of output quantities:')
            call matwrite(Jmat,knumEGr,knumEGr,66,'(3es16.8)','Matrix Jmat:')
            call matwrite(Qsmat,knumEGr,knumEGr,66,'(3es16.8)','Matrix Qsmat:')
            write(66,*) 'End corrmatEGR-------------------------------------------------'
        end if

    end subroutine corrmatEGr

    !#######################################################################

    subroutine Exchange2Symbols(k1_exchg, k2_exchg)

        ! this routine performs a change of the sequence of the up to three
        ! output quantities, where the aim could be to have one of the ouput
        ! quantity as the first one. So, it interchanges the two output quantities
        ! with index numbers k1_exchg, k2_exchg.
        ! This change requires a lot of other changes with character arrays and
        ! real arrays, which must also be made visible in the corresponding
        ! elements of the GUI.
        ! Thus, calls of ExchgText() and of ExchgDB() are necessary. The
        ! equations also have to re-ordered calling RebuildEquations.
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_Gleich_globals,        only: Symbole,symtyp,einheit,bedeutung, MEsswert,IVTL,IAR,SDformel, &
                                    SDwert,HBreite, StdUnc, MesswertSV,StdUncSV,Formelt,nglp,  &
                                    nmodf,Formeltext,FormeltextFit,ngrs,sensi,sensiSV, &
                                    perc,percSV,kegr,knetto,kbrutto,knumEGr,ncov,kbrutto_gl
        use UR_Linft,         only: FitDecay,kfitp,nchannels,numd,ifit
        use Rout,             only: WDSetComboboxAct,WDPutTextviewString,WDPutLabelString,WDListstoreFill_1
        use LSTfillT,         only: WDListstoreFill_table
        use CHF,              only: ucase
        use Top,              only: CharModA1
        use RG,               only: modify_Formeltext

        implicit none
        integer, intent(inout)                :: k1_exchg, k2_exchg


        integer             :: ii,i,i1,i2,ii1,ii2,kk,inet,ibrut
        integer             :: kEGrneu,ifk1,ifk
        character(len=60)   :: oldname1, oldname2
        character(:),allocatable  :: text, textG

        allocate(character(len=800) :: text, textG)

        ! jj=1: Symbole 1-3;   jj=2: Fitp1-Fitp3
        kEGrneu = 0
        if(kEGr == k1_exchg) then
            kEGrneu = k2_exchg
        elseif(kEGr == k2_exchg) then
            kEGrneu = k1_exchg
        end if
        if(.not.FitDecay) then
            if(kEGrneu > 0) then
                inet = knetto(kEGr)
                ibrut = kbrutto(kEGr)
                knetto(kEGr)= knetto(kEGrneu)
                kbrutto(kEGr) = kbrutto(kEGrneu)
                knetto(kEGrneu) = inet
                kbrutto(kEGrneu) = ibrut

                ibrut = kbrutto_gl(kEGr)
                kbrutto_gl(kEGr) = kbrutto_gl(kEGrneu)
                kbrutto_gl(kEGrneu) = ibrut
            end if
            IF(knetto(kEGrneu) > 0) call WDSetComboboxAct('comboboxNetRate', knetto(kEGrneu))
            IF(kbrutto(kEGrneu) > 0) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGrneu))
            ! write(66,*)' kEGrneu=',kEGrneu,'   knetto=',knetto(kEGrneu),' kbrutto=',kbrutto(kEGrneu)
        end if
        oldname1 = Symbole(k1_exchg)%s
        oldname2 = Symbole(k2_exchg)%s
        Symbole(k1_exchg)%s = oldname2
        Symbole(k2_exchg)%s = oldname1

        call ExchgText(symtyp,ngrs, k1_exchg,k2_exchg)
        call ExchgText(einheit,ngrs, k1_exchg,k2_exchg)
        call ExchgText(bedeutung,ngrs, k1_exchg,k2_exchg)

        call ExchgDB(Messwert,ngrs, k1_exchg,k2_exchg)
        call ExchgDB(MesswertSV,ngrs, k1_exchg,k2_exchg)
        call ExchgDB(SDwert,ngrs, k1_exchg,k2_exchg)
        call ExchgDB(HBreite,ngrs, k1_exchg,k2_exchg)
        call ExchgDB(StdUnc,ngrs, k1_exchg,k2_exchg)
        call ExchgDB(StdUncSV,ngrs, k1_exchg,k2_exchg)

        text = SDformel(k2_exchg)%s
        SDformel(k2_exchg)%s = SDformel(k1_exchg)%s
        SDformel(k1_exchg)%s = trim(text)

        ii = IVTL(k2_exchg)
        IVTL(k2_exchg) = IVTL(k1_exchg)
        IVTL(k1_exchg) = ii

        ii = IAR(k2_exchg)
        IAR(k2_exchg) = IAR(k1_exchg)
        IAR(k1_exchg) = ii

        call ExchgDB(sensi,ngrs+ncov+numd, k1_exchg,k2_exchg)
        call ExchgDB(sensiSV,ngrs+ncov+numd, k1_exchg,k2_exchg)
        call ExchgDB(perc,ngrs+ncov+numd, k1_exchg,k2_exchg)
        call ExchgDB(percSV,ngrs+ncov+numd, k1_exchg,k2_exchg)

        text = Formelt(k2_exchg)%s
        Formelt(k2_exchg)%s = Formelt(k1_exchg)%s
        Formelt(k1_exchg)%s = trim(text)

        if(FitDecay) then

            k1_exchg = k1_exchg + kfitp(1) - 1
            k2_exchg = k2_exchg + kfitp(1) - 1

            oldname1 = Symbole(k1_exchg)%s
            oldname2 = Symbole(k2_exchg)%s

            Symbole(k1_exchg)%s = oldname2
            Symbole(k2_exchg)%s = oldname1

            !adjust FitpX symbols:
            write(Symbole(k1_exchg)%s(5:5),'(i1)') k1_exchg -(kfitp(1) - 1)
            write(Symbole(k2_exchg)%s(5:5),'(i1)') k2_exchg -(kfitp(1) - 1)

            call ExchgText(symtyp,ngrs, k1_exchg,k2_exchg)
            call ExchgText(einheit,ngrs, k1_exchg,k2_exchg)
            call ExchgText(bedeutung,ngrs, k1_exchg,k2_exchg)

            call ExchgDB(Messwert,ngrs, k1_exchg,k2_exchg)
            call ExchgDB(MesswertSV,ngrs, k1_exchg,k2_exchg)
            call ExchgDB(SDwert,ngrs, k1_exchg,k2_exchg)
            call ExchgDB(HBreite,ngrs, k1_exchg,k2_exchg)
            call ExchgDB(StdUnc,ngrs, k1_exchg,k2_exchg)
            call ExchgDB(StdUncSV,ngrs, k1_exchg,k2_exchg)

            text = SDformel(k2_exchg)%s
            SDformel(k2_exchg)%s = SDformel(k1_exchg)%s
            SDformel(k1_exchg)%s = trim(text)

            ii = IVTL(k2_exchg)
            IVTL(k2_exchg) = IVTL(k1_exchg)
            IVTL(k1_exchg) = ii

            ii = IAR(k2_exchg)
            IAR(k2_exchg) = IAR(k1_exchg)
            IAR(k1_exchg) = ii

            call ExchgDB(sensi,ngrs+ncov+numd, k1_exchg,k2_exchg)
            call ExchgDB(sensiSV,ngrs+ncov+numd, k1_exchg,k2_exchg)
            call ExchgDB(perc,ngrs+ncov+numd, k1_exchg,k2_exchg)
            call ExchgDB(percSV,ngrs+ncov+numd, k1_exchg,k2_exchg)

            if(k1_exchg > 3) k1_exchg = k1_exchg - (kfitp(1) - 1)
            if(k2_exchg > 3) k2_exchg = k2_exchg - (kfitp(1) - 1)
        end if

        call WDListstoreFill_1('liststore_symbols', ngrs, symbole)
        call WDListstoreFill_table('liststore_symtable',1, .false.)
        call WDListstoreFill_table('liststore_valunc',2, .true.)
        call WDListstoreFill_table('liststore_budget',3, .false.)
        !--------------------------------------------------

        if(FitDecay .and. nmodf > 0) then
            !do i=1,nglp
            !  write(66,'(a,i2,a,a)') 'Testx:  Formelt(',i,')=',trim(Formelt(i))
            !end do

            kk = ifit(k2_exchg)
            ifit(k2_exchg) = ifit(k1_exchg)
            ifit(k1_exchg) = kk
            call WDSetComboboxAct('comboboxA1', ifit(1))
            call WDSetComboboxAct('comboboxA2', ifit(2))
            call WDSetComboboxAct('comboboxA3', ifit(3))

            text = Formelt(k2_exchg)%s
            textG = trim(ucase(text))
            ii1 = index(textG,'FITP')
            if(ii1 > 0) write(Formelt(k2_exchg)%s(ii1+4:ii1+4),'(i1)') k2_exchg

            text = Formelt(k1_exchg)%s
            textG = ucase(trim(text))
            ii1 = index(textG,'FITP')
            if(ii1 > 0) write(Formelt(k1_exchg)%s(ii1+4:ii1+4),'(i1)') k1_exchg
        end if

        if(Fitdecay .and. nmodf > 0) then

            do kk=1,nchannels

                text = Formelt(nglp+k1_exchg + (kk-1)*3)%s
                ii1 = max( index(text(1:5),'X'), index(text(1:5),'x') )
                read(text(ii1+1:ii1+1),*) i1
                text = Formelt(nglp+k2_exchg + (kk-1)*3)%s
                ii2 = max( index(text(1:5),'X'), index(text(1:5),'x') )
                read(text(ii2+1:ii2+1),*) i2
                Formelt(nglp+k2_exchg + (kk-1)*3)%s = Formelt(nglp+k1_exchg + (kk-1)*3)%s
                Formelt(nglp+k1_exchg + (kk-1)*3)%s = trim(text)

                write(Formelt(nglp+k1_exchg + (kk-1)*3)%s(ii2+1:ii2+1),'(i1)') i1
                write(Formelt(nglp+k2_exchg + (kk-1)*3)%s(ii1+1:ii1+1),'(i1)') i2

            end do
        end if

        call RebuildEquations(ifk1,ifk)

        call CharModA1(Formeltext,ifk)
        call CharModA1(FormeltextFit,ifk -ifk1)         ! nglf+iandk)
        call WDPutTextviewString('textview2',Formeltext)
        call WDPutTextviewString('textviewModelEQ',FormeltextFit)

        do i=1,knumEGr
            if(i == 1) call WDPutLabelString('QFirst',symbole(i)%s)
            if(i == 2) call WDPutLabelString('QSecond',symbole(i)%s)
            if(i == 3) call WDPutLabelString('QThird',symbole(i)%s)
        end do

    end subroutine Exchange2Symbols

!#######################################################################

    subroutine ExchgText(strarr,n, k1_exchg,k2_exchg)
        use UR_Gleich_globals,      only: charv

        !  this routine interchanges in a character array strarr of type(charv)
        !  the two elements with indices k1_exchg,k2_exchg
        !     Copyright (C) 2014-2023  Günter Kanisch

        implicit none
        integer   ,intent(in)            :: n
        type(charv),intent(inout)        :: strarr(n)
        integer   ,intent(in)            :: k1_exchg, k2_exchg

        integer                   :: i
        character(len=300)        :: text

        do i=1,n
            strarr(i)%s = strarr(i)%s
        end do

        text = strarr(k2_exchg)%s
        strarr(k2_exchg)%s = strarr(k1_exchg)%s
        strarr(k1_exchg)%s = trim(text)

    end subroutine ExchgText

!#######################################################################

    subroutine ExchgDB(dbarr,n, k1_exchg,k2_exchg)

        !  this routine interchanges in real(rn) array dbarr the two
        !  elements with indices k1_exchg,k2_exchg
        !     Copyright (C) 2014-2023  Günter Kanisch

        implicit none

        integer   ,intent(in)         :: n
        real(rn),intent(inout)        :: dbarr(n)
        integer   ,intent(in)         :: k1_exchg, k2_exchg

        real(rn)        :: dummy

        dummy = dbarr(k2_exchg)
        dbarr(k2_exchg) = dbarr(k1_exchg)
        dbarr(k1_exchg) = dummy

    end subroutine ExchgDB

!#######################################################################

    real(rn) Function gevalf(i, mw)

        ! this function replaces evalf in the following way:
        ! it takes the incoming array mw as if it were the Messwert array,
        ! and copies it temporarily to the array mvalues.
        ! It then looks for symbols with the distribution type 4 or 7 (counts),
        ! and if found, adds the value of GamDistAdd to it.
        ! It finally calls evalf with this modfied array mvalues:
        !   gevalf = evalf(i,mvalues)
        !
        ! if an MC or an MCMC simulation is running, evalf is called directly with
        ! the array mw; the reason is, that the addition of GamDistAdd in this case
        ! is done within the gamma distribution random generator.
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_DLIM,          only: GamDistAdd
        use UR_Gleich_globals,        only: ivtl,ngrs,ncov,kbgv_binom,itm_binom,iptr_time, &
            ifehl,ifehl_string           ! ,Symbole,use_bipoi,
        use fparser,          only: EvalErrMsg, evalf
        use ur_general_globals, only: MCSim_on
        use UR_Linft,         only: numd
        use Top,              only: WrStatusbar

        IMPLICIT NONE

        integer   ,intent(in)             :: i                 ! function (equation) number
        real(rn),intent(in),dimension(:)  :: mw                ! values of array Messwert

        integer            :: k,nval
        real(rn)           :: xng
        real(rn),allocatable  :: mvalues(:)
        character(len=4)   :: cnum
!----- -------- --------- --------- --------- --------- --------- --------- -------
        gevalf = ZERO

        if(MCSim_on) then
            gevalf = evalf(i,mw)
            return
        end if
        nval = ngrs
        nval = ngrs+ncov+numd
        nval = min(nval,size(mw,dim=1))

        allocate(mvalues(nval))
        mvalues(1:nval) = mw(1:nval)

        do k=1,nval
            ! mvalues(k) = mw(k)

            if(k > ubound(IVTL,dim=1)) cycle

            ! write(66,*) 'k=',k,' mvalues(k)=',mvalues(k)
            if(.not.MCSim_on) then
                if(ivtl(k) == 4) then
                    if(abs(mw(k)) < EPS1MIN) then
                        if(abs(GamDistAdd) < EPS1MIN) then
                            mvalues(k) = mw(k) + ONE
                        else
                            mvalues(k) = mw(k) + GamDistAdd
                        end if
                    else
                        mvalues(k) = mw(k) + GamDistAdd
                    end if
                end if
                if(ivtl(k) == 7) then
                    if(iptr_time(kbgv_binom) == 0) then
                        ifehl = 1
                        write(cnum,'(i3)') kbgv_binom
                        ifehl_string = 'Error: Binomial/Poisson: setup case again! kbgv_binom=' // cnum
                        write(66,*) trim(ifehl_string)
                        call wrstatusbar(3,ifehl_string)
                        return
                    end if
                    xng = mw(k)            ! number of gross counts
                    if(.not. MCsim_on) then
                        if(abs(mw(k)) < EPS1MIN) then
                            if(abs(GamDistAdd) < EPS1MIN) then
                                mvalues(k) = mw(k) + ONE*mw(itm_binom)/mw(iptr_time(kbgv_binom))
                            else
                                mvalues(k) = mw(k) + GamDistAdd
                            end if
                        else
                            mvalues(k) = mw(k) + GamDistAdd !
                        end if
                    end if
                end if
            end if

        end do

        gevalf = evalf(i,mvalues)

    end function gevalf

!#######################################################################

    real(rn) function func_Fconst(Messwert,nm)

        !  calculates the value of Fconst using the equation kcind = ndd+(j-1)*2+1
        !  with ndd = nab+nmodf+nabf+ncovf   and   j=kEGr
        !
        ! val of output quantity = Fconst + Flinear*(net count rate)
        !
        !     Copyright (C) 2018-2023  Günter Kanisch

        use UR_Gleich_globals,      only: nab,nmodf,nabf,ncovf,kEGr
        use fparser,        only: evalf

        implicit none

        integer   ,intent(in)  :: nm
        real(rn),intent(in)    :: Messwert(nm)

        integer         :: ndd,j,kcind

        func_Fconst = 0.0_rn
        ndd = nab+nmodf+nabf+ncovf
        j = kEgr
        kcind = ndd+(j-1)*2+1
        !write(66,*) 'FC:  ndd=',ndd,' j=',j, '  Index kcind=',kcind,'  nm=',nm, &
        !           'Reite(kcind)=',trim(Rseite(kcind))
        func_Fconst = gevalf(kcind,Messwert)
        ! write(66,*) 'FC:  Wert=',sngl(func_Fconst)

    end function func_Fconst

!########################################################################

    real(rn) function func_Flinear(Messwert,nm)

        !  calculates the value of Flinear using the equation kcind = ndd+(j-1)*2+2
        !  with ndd = nab+nmodf+nabf+ncovf   and   j=kEGr
        !
        ! val of output quantity = Fconst + Flinear*(net count rate)

        !     Copyright (C) 2018-2023  Günter Kanisch

        use UR_Gleich_globals, only: nab,nmodf,nabf,ncovf,kEGr
        use fparser,        only: evalf

        implicit none

        integer   ,intent(in)  :: nm
        real(rn),intent(in)    :: Messwert(nm)

        integer          :: ndd,j,kcind

        func_Flinear = ZERO
        ndd = nab+nmodf+nabf+ncovf
        j = kEgr
        kcind = ndd+(j-1)*2+2
        ! write(66,*) 'Formel kcind=',trim(Rseite(kcind)%s),'   Formel kcind-1=',trim(Rseite(kcind-1)%s)
        func_Flinear = gevalf(kcind,Messwert) - evalf(kcind-1,Messwert)
        ! write(66,*) 'FL:  Wert1=',sngl(evalf(kcind,Messwert)),' minus Wert2=',sngl(evalf(kcind,Messwert))

    end function func_Flinear

!########################################################################

    subroutine dpi_uprop1(mode,mwind,mwfv,k3anf,k3end,fv1,iim, dpi,fv2,dpa)

        ! calculates a partial derivative of the equation mwfv with respect to Messwert(mwind);
        !     d Messwert(mwfv) / d Messwert(mwind))
        ! this depends on four values of mode
        !
        !     Copyright (C) 2019-2023  Günter Kanisch

        use UR_Gleich_globals, only: Messwert,missingval,klinf,kgspk1,kEGr,knumEGr,kbrutto_double, &
                                     kbrutto
        use UR_Gspk1Fit,  only: Gamspk1_Fit
        use UR_Linft,     only: kfitp,FitDecay,use_WTLS
        use Top,          only: dpafact
        use UR_DLIM,      only: iteration_on
        use ur_general_globals, only: chh1,chh2,mwert1,mwert2,kbd,Messwert_kbruttoSV,fv1back

        implicit none

        integer   ,intent(in)     :: mode          ! for calculating fv2:
        ! 1: use gevalf; 2: use Messwert(mwfv);
        ! 3: use ResultA(mwfv);
        ! 4: replace ResultA(mwfv) by gevalf, applied to equations
        !    k3anf..k3end, thereafter take Messwert(mwfv))

        integer   ,intent(in)     :: mwind         ! indicates the index of the Messwert value, with respect
        ! to which the partial derivative is build
        integer   ,intent(in)     :: mwfv          ! indicates the index of the equation, to which the
        ! derivative refers
        integer   ,intent(in)     :: k3anf,k3end   ! k3anf > k3end (loop over Messwert, bottom-->up)
        real(rn),intent(in)       :: fv1           ! first function value
        integer   ,intent(in)     :: iim           ! is iim1 or iim2

        real(rn),intent(out)      :: dpi,fv2,dpa

        integer           :: k3,klu
        real(rn)          :: res,fv1x,dummy     ! fv2,dpa,
        real(rn),allocatable  :: MesswertK(:)

        ! write(66,'(5(a,i0),a,es12.5)') 'dpi_uprop1: mode=',mode,' mwind=',mwind,' mwfv=',mwfv, &
        !                 ' k3anf=',k3anf,' k3end=',k3end,' fv1=',fv1
        allocate(MesswertK(k3anf))
        MesswertK(1:k3anf) = Messwert(1:k3anf)

        klu = 0
        if(mwfv == klinf .or. mwfv == kgspk1) then
            klu = klinf
            IF(Gamspk1_Fit) klu = kgspk1
            IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
        end if
! dpi_uprop1 = zero
        dpi = ZERO

        fv1x = fv1
        if(mode == 2) then
            ! increase Messwert(mwind) by dpa:
            dpa = Messwert(mwind) * dpafact(Messwert(mwind)) - Messwert(mwind)
            if(use_WTLS) then
                dpa = Messwert(mwind) * (ONE + (ONE - dpafact(Messwert(mwind)))*10._rn) - Messwert(mwind)
            end if
            Messwert(mwind) = Messwert(mwind) + dpa
        end if

        if(mode == 3) then           ! also for Gamspk1
            !----  2nd function value:
            IF(abs(Messwert(mwind)) > EPS1MIN) THEN
                dpa = Messwert(mwind) * dpafact(Messwert(mwind))  - Messwert(mwind)
                if(use_WTLS) then
                    dpa = Messwert(mwind) * (ONE + (ONE - dpafact(Messwert(mwind)))*10._rn) - Messwert(mwind)
                end if
            else
                dpa = 1.0E-10_rn
            end if
            ! increase Messwert(mwind) by dpa:
            Messwert(mwind) = Messwert(mwind) + dpa
            fv2 = Resulta(mwfv)
            if(klu > 0) fv2 = Messwert(klu)
            Messwert(mwind) = Messwert(mwind) - dpa
            dummy = Resulta(mwfv)
        end if

        if(mode <= 2) then
            ! use gevalf:
            do k3=k3anf,k3end,-1
                res = gevalf(k3,Messwert)
                Messwert(k3) = res
            end do
            if(abs(fv1x-missingval) < EPS1MIN) fv1x = MEsswert(mwfv)
            if(mode == 1) then
                dpa = Messwert(mwind) * dpafact(Messwert(mwind)) - Messwert(mwind)
                if(abs(dpa) < EPS1MIN) dpa = 1.e-10_rn
                Messwert(mwind) = Messwert(mwind) + dpa
            end if
            if(mode == 1) fv2 = gevalf(mwfv, Messwert)
            if(mode == 2) fv2 = Messwert(mwfv)     ! use MEsswert(mwfv)
            if(mode <= 2) Messwert(mwind) = Messwert(mwind) - dpa  ! remove the increment in Messwert(mwind)
        end if
        if(abs(dpa-ZERO) > EPS1MIN .and. abs(fv2 - ZERO) > EPS1MIN) then
            IF(abs(fv2/dpa - fv1x/dpa) > EPS1MIN .and. abs(dpa) > EPS1MIN ) THEN
                dpi = (fv2/dpa - fv1x/dpa)
            else
                dpi = ZERO
            end if
        end if

        if(mode == 4) then
            ! write(66,'(5(a,i0))') 'mwind=',mwind,' mwfv=',mwfv,' k3anf=',k3anf,' k3end=',k3end,'  iim=',iim
            IF(abs(Messwert(mwind)) > EPS1MIN) THEN
                dpa = Messwert(mwind) * dpafact(Messwert(mwind)) - Messwert(mwind)
                if(use_WTLS) then
                    dpa = Messwert(mwind) * (ONE + (ONE - dpafact(Messwert(mwind)))*10._rn) - Messwert(mwind)
                end if
            else
                dpa = 1.0e-10_rn
            end if
            ! increase Messwert(mwind) by dpa:
            Messwert(mwind) = Messwert(mwind) + dpa
            IF(iteration_on .and. kbrutto_double > 0 .AND. kbd > 0 .AND. kbd == iim ) THEN
                res = gevalf(kbrutto(kEGr),Messwert)
                Messwert(kbrutto(kEGr)) = res
            end if
            ! IF(FitDecay .AND. k3end <= 2 .AND. index(chh1,'FITP') == 1 .AND. index(chh2,'FITP') == 1) THEN
            IF(FitDecay .AND. k3end <= knumEGr .AND. index(chh1,'FITP') == 1 .AND. index(chh2,'FITP') == 1) THEN
                ! for Fitp(i) DONT use Resulta(nn)
                do k3=k3anf,k3end,-1
                    IF(k3 <= knumEgr .AND. k3 /= k3end) CYCLE
                    res = gevalf(k3,Messwert)
                    Messwert(k3) = res
                end do
                res = Messwert(mwfv)
                fv2 = res
            else
                fv2 = Resulta(mwfv)
            end if
            mwert2 = Messwert(iim)
            !  IF(abs(mwert1 - mwert2)< eps1min) EXIT       ! Resulta(nn) does not depend on SymboleA(k)
            !----
            IF(abs(mwert1 - mwert2)< EPS1MIN) then
                dpi = ZERO
                goto 100          ! return
            END IF
            Messwert(mwind) = Messwert(mwind) - dpa      ! remove the increment in Messwert(mwind)
            IF(iteration_on .and. kbrutto_double > 0 .AND. kbd > 0 .and. kbd == iim) then
                Messwert(kbrutto(kEGr)) = Messwert_kbruttoSV
            end if

            fv1back = Resulta(mwfv)

            ! sensitivity factor = dpi:
            IF(abs(mwert2-mwert1) > EPS1MIN) THEN
                dpi = (fv2/(mwert2-mwert1) - fv1/(mwert2-mwert1))
            else
                dpi = ZERO
            end if
        end if

100     continue
        Messwert(1:k3anf) = MesswertK(1:k3anf)

    end subroutine dpi_uprop1

!########################################################################

    subroutine RebuildEquations(ifk1,ifk)

        !     Copyright (C) 2018-2023  Günter Kanisch

        use UR_Gleich_globals,          only: Formelt,Formeltext,FormeltextFit,nab,nglf
        use UR_Linft,           only: FitDecay
        use top,                only: CharModA1

        implicit none

        integer   ,intent(out)     :: ifk1,ifk

        integer                   :: i,k,izlen,iandk,istep,kmf,ifs
        character(:),allocatable  :: buffer

        allocate(character(len=800) :: buffer)

        izlen = 100
        ifk = 0      ! counts the number of lines covered in Report
        iandk = 0    ! counts continuation lines
        istep = 0
        do i=1,nab + nglf        ! size(Formelt)                    !<<<<<<<<<<<<<<<<<<<<<<<<<<<< oder nglp+nglpf????
            buffer = Formelt(i)%s
            do ifs=1,10
                kmf = 0
                IF(LEN_TRIM(buffer) > izlen-3) THEN
                    ! kmf = izlen-3
                    do k=izlen-3,1,-1
                        IF(buffer(k:k) == ' ' .OR. buffer(k:k) == ',' .OR. buffer(k:k) == '+' .OR. &
                            buffer(k:k) == '-' .OR. buffer(k:k) == '*' .OR. buffer(k:k) == '/') THEN
                            kmf = k
                            EXIT
                        end if
                    end do
                    if(kmf > 0) then
                        ifk = ifk + 1
                        iandk = iandk + 1
                        if(istep == 0) then
                            ! write(66,*) 'Rebuild A:  ifk=',int(ifk,2),' trim(buffer(1:kmf)=',trim(buffer(1:kmf))
                            if(ifk > size(Formeltext)) call CharModA1(Formeltext,ifk)
                            if(ifs == 1) Formeltext(ifk)%s = buffer(1:kmf) // ' &'
                            if(ifs > 1) Formeltext(ifk)%s = '       ' // buffer(1:kmf) // ' &'
                            ! WRITE(Formeltext(ifk)%s,'(a,1x,a)') buffer(1:kmf),'&'
                            ! write(66,*) 'ifk=',int(ifk,2),' a: ',Formeltext(ifk)%s
                        else
                            ! write(66,*) 'Rebuild B:  ifk=',int(ifk,2),' istep=',int(istep,2),' trim(buffer(1:kmf)=',trim(buffer(1:kmf))
                            if(ifk-istep > size(FormeltextFit)) call CharModA1(FormeltextFit,ifk-istep)
                            if(ifs == 1) FormeltextFit(ifk-istep)%s = buffer(1:kmf) // ' &'
                            if(ifs > 1) FormeltextFit(ifk-istep)%s = '       ' // buffer(1:kmf) // ' &'
                            ! WRITE(Formeltext(ifk)%s,'(a,1x,a)') buffer(1:kmf),'&'
                            ! write(66,*) 'ifk-istep=',int(ifk-istep,2),' a: ',FormeltextFit(ifk-istep)%s
                        end if
                        buffer = buffer(kmf+1:)
                    else
                        ! Continuation lines, without having gfound the & character
                        ifk = ifk + 1
                        if(istep == 0) then
                            ! write(66,*) 'Rebuild C:  ifk=',int(ifk,2),' trim(buffer)=',trim(buffer)
                            if(ifk > size(Formeltext)) call CharModA1(Formeltext,ifk)
                            Formeltext(ifk)%s = '       ' // trim(buffer)
                            ! write(66,*) 'ifk=',int(ifk,2),' b: ',Formeltext(ifk)%s
                        else
                            ! write(66,*) 'Rebuild D:  ifk=',int(ifk,2),' istep=',int(istep,2),' trim(buffer(1:kmf)=',trim(buffer(1:kmf))
                            if(ifk-istep > size(FormeltextFit)) call CharModA1(FormeltextFit,ifk-istep)
                            FormeltextFit(ifk-istep)%s = '       ' // trim(buffer)
                            ! write(66,*) 'ifk-istep=',int(ifk-istep,2),' b: ',FormeltextFit(ifk-istep)%s
                        end if
                        exit
                    end if
                Else
                    ! There is no continuation line:
                    ifk = ifk + 1
                    if(istep ==  0) then
                        ! write(66,*) 'Rebuild: E  ifk=',int(ifk,2),' trim(buffer)=',trim(buffer)
                        if(ifk > size(Formeltext)) call CharModA1(Formeltext,ifk)
                        if(ifs == 1) Formeltext(ifk)%s = trim(buffer)
                        if(ifs > 1) Formeltext(ifk)%s = '       ' // trim(buffer)
                        ! write(66,*) 'ifk=',int(ifk,2),' c: ',Formeltext(ifk)%s
                    else
                        ! write(66,*) 'Rebuild: F  ifk=',int(ifk,2),' trim(buffer)=',trim(buffer)
                        if(ifk-istep > size(FormeltextFit)) call CharModA1(FormeltextFit,ifk-istep)
                        if(ifs == 1) FormeltextFit(ifk-istep)%s = trim(buffer)
                        if(ifs > 1) FormeltextFit(ifk-istep)%s = '       ' // trim(buffer)
                        ! write(66,*) 'ifk-istep=',int(ifk-istep,2),' c: ',FormeltextFit(ifk-istep)%s
                    end if
                    exit
                end if
            end do
            if(ifk == nab+iandk) then
                if(.not.FitDecay) exit
                istep= nab+iandk
                ifk1 = ifk
            end if
            if(istep > 0 .and. ifk == nab+nglf+iandk) then
                ! write(66,*) 'nach FormeltextFit: ifk end =',ifk
                exit
            end if
        end do
        if(ifk1 == 0) ifk1 = ifk
        deallocate(buffer)

    end subroutine RebuildEquations

!########################################################################

    real(rn) function fSD(k)

        ! normally returns the value StdUnc(k));
        ! may be modified such, that a unit conversion factor is included in the return value

        !     Copyright (C) 2014-2023  Günter Kanisch
        use UR_Gleich_globals,      only: StdUnc, apply_units_dir,unit_conv_fact,nab

        implicit none

        integer   ,intent(in)     :: k   ! number of the symbol in the symbol list

        if(.true. .or. .not.apply_units_dir) then
            fSD = StdUnc(k)
            return
        else
            if(k > nab) then
                fSD = StdUnc(k)
                if(apply_units_dir) fSD = fSD * unit_conv_fact(k)
            else
                fSD = StdUnc(k)
            end if
        end if

    end function fSD


!########################################################################



end module UWB
