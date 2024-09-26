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
recursive subroutine ProcessLoadPro_new(iwahl, kEGRneu, user_settings)

    !  this routine runs in an automated way sequences of user actions, including
    !  e.g. the run through the evaluations steps, up to the TAB Results:
    !
    !  iwahl = 0 : Loading from TXP file
    !        = 1 : changing the output quantity, without re-loading the txp file
    !        = 2 : re-adjust calculations after changes ocurred in the linear model
    !        = 3 : re-adjust calculations starting with the button CalcValUnc
    !        = 4 : re-adjust calculations starting with the button AcceptAll
    !
    !  called by various routines

    !     Copyright (C) 2014-2023  Günter Kanisch

    use gtk,                 only: gtk_widget_set_sensitive,gtk_widget_show,gtk_widget_hide, &
                                   gtk_widget_set_visible, gtk_widget_get_sensitive, &
                                   GTK_MESSAGE_WARNING,GTK_BUTTONS_OK,gtk_widget_show_all
    use UR_types
    use top,                 only: FindItemS,idpt,FieldUpdate,WrStatusbar
    use UR_gtk_variables,    only: clobj,consoleout_gtk,NBsoftSwitch,item_setintern
    USE Rout,                only: Fopen,WDNotebookSetCurrPage,pending_events, &
                                   WDNotebookSetCurrPage,WDPutLabelString, &
                                   WDNotebookGetCurrPage,WDGetComboboxAct, MessageShow, &
                                   ExpandTV2Col7,WDSetComboboxAct

    use PMD,                 only: ProcMainDiag
    use UR_Variables,        only: FileTyp,SAVEP,langg,project_loadw,fname,fname_getarg, &
                                   batest_on,autoreport,bat_serial,batf,batest_user,simul_ProSetup, &
                                   done_simul_ProSetup

    USE UR_Gleich,           only: loadingPro,kEgr,ifehl,symlist_modified,kbrutto,knetto, &
                                   refresh_but,kEGr_old,nvarsMD
    USE UR_perror
    USE UR_Linft,            only: ifit,FitDecay,SumEval_fit,FitCalCurve
    use UR_Gspk1Fit,         only: Gamspk1_fit
    use UR_loadsel
    use gtk_sup
    use URinit,              only: UncW_Init
    use Pread,               only: ProRead

    implicit none
    type(user_settings_type), intent(inout) :: user_settings

    integer, intent(in)           :: iwahl
    integer, intent(in),optional  :: kEGrneu     ! another output quantity number, not the active one

    integer              :: ncitem,kpage1,kknetR,kkbrutR
    CHARACTER(LEN=150)   :: str1
    logical              :: prout,test1
    integer(c_int)       :: resp
!-----------------------------------------------------------------------
!   iwahl = 0 : Loading from TXP file
!         = 1 : changing the output quantity, without re-loading the txp file
!         = 2 : re-adjust calculations after changes ocurred in the linear model
!         = 3 : re-adjust calculations starting with the button CalcValUnc
!         = 4 : re-adjust calculations starting with the button AcceptAll

    !  if(present(kEGrneu)) write(66,*) 'PLP:   iwahl=',iwahl,' kEgrneu=',kEgrneu,'  SaveP=',SaveP
    !  if(.not.present(kEGrneu)) write(66,*) 'PLP:   iwahl=',iwahl
    if(consoleout_gtk) write(0,*) '##### PLP  begin  ###########################'

    prout = .false.
    ! prout = .true.
    !  write(66,*) 'iwahl=',int(iwahl,2),' kEGrneu=',int(kEGrneu,2),' file=',trim(fname)

    IF(langg == 'DE') call WrStatusbar(4,'Rechnet...' )
    IF(langg == 'EN') call WrStatusbar(4,'Calculating...' )
    IF(langg == 'FR') call WrStatusbar(4,'Calcule...' )

    if(prout) write(66,*) 'PLoadpronew: Start!'

    Call WDNotebookGetCurrPage('notebook1', kpage1)

    loadingPro = .TRUE.
    call ExpandTV2Col7(.false.)

    kknetR = 0
    kkbrutR = 0

    ifehl = 0
    IF(iwahl == 0) kEgr = 1
    IF(PRESENT(kEGrneu))  kEgr = kEGrneu
    if(iwahl == 0) call WDNotebookSetCurrPage('notebook1', 1)
    if(iwahl > 0 .and. iwahl /= 3 .and. refresh_but .and. kpage1 >= 3) goto 70
    if(iwahl == 3) goto 75
    if(iwahl == 4) goto 36
    IF(iwahl == 1) THEN
        IF(.not.batest_on .and. .not.autoreport) THEN
            IF(PRESENT(kEGrneu))  kEgr = kEGrneu
            call WDNotebookSetCurrPage('notebook1', 2)     ! <--  i.e., kpage1=2
            GOTO 50
        else
            ! for the BatchTest
            IF(PRESENT(kEGrneu)) THEN
                kEgr = kEGrneu
            else
                kEgr = 2
            end if
            GOTO 50
        end if
    end if

    IF(iwahl == 2) GOTO 30

    call WrStatusBar(2,'Loading Project ...')
    if(langg == 'FR') call WrStatusBar(2,'Chargement du projet ...')

! WRITE(66,*) 'Entering ProcesssLoadPro : loadingPro=',loadingPro

    FileTyp = 'P'
    if(bat_serial .or. batf .or. batest_user) then
        !   take the filename fname from Batch_MC (fnameMCB)
    else IF(LEN_TRIM(fname_getarg) > 0) THEN
        fname = trim(fname_getarg)                       !
    else
        fname = ' '
        IF(.not.batest_on .and. .not.autoreport .and. &
            .not.bat_serial .and. .not.batf) THEN
            IF(langg == 'DE') write(str1,*) 'Öffnen der Projekt-Datei (txp; csv):'
            IF(langg == 'EN') write(str1,*) 'Open project file (txp; csv):'
            IF(langg == 'FR') write(str1,*) 'Ouvrir le fichier de projet (txp; csv):'
            CALL FOpen(ifehl,create=.false., hinweis = str1)
        end if
    end if
    IF(.not.autoreport .and. .not.bat_serial .and. .not.batf) fname_getarg = ' '

    IF(ifehl == 0 .AND. LEN_TRIM(fname) > 0) THEN
        call UncW_Init(user_settings)
        if(project_loadw) loadingpro = .true.
        FileTyp = 'P'
        if(prout) write(66,*) 'PLP:135: fname=',trim(fname)
        ! call ProRead()
        if(.not. simul_ProSetup) call ProRead(user_settings)
        if(prout) WRITE(str1,*) 'behind call ProRead: ifehl=',int(ifehl)
        IF(ifehl == 1) GOTO 100
        SAVEP = .FALSE.
        call FieldUpdate()
        call gtk_widget_set_sensitive(idpt('MenuSaveProject'), 1_c_int)
        call gtk_widget_set_sensitive(idpt('MenuSaveProjectAs'), 1_c_int)
        call gtk_widget_set_sensitive(idpt('TBSaveProject'), 1_c_int)
        call gtk_widget_set_sensitive(idpt('TBSaveProjectAs'), 1_c_int)
        call gtk_widget_set_sensitive(idpt('SerialEval'), 1_c_int)
    else
        loadingPro = .FALSE.
        call WrStatusBar(2,'')
        goto 100
    END IF

    IF(project_loadw) THEN
        loadingPro = .TRUE.
    else
        loadingPro = .FALSE.
        goto 100
    end if

30  continue
    if(symlist_modified) goto 33
    call WDNotebookSetCurrPage('notebook1', 1)
    call WDNotebookSetCurrPage('notebook1', 2)
    call gtk_widget_set_sensitive(idpt('NBValUnc'), 0_c_int)
    call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
    call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
    call gtk_widget_hide(idpt('box4'))
    call gtk_widget_hide(idpt('box5'))
    call gtk_widget_hide(idpt('grid5'))

!call gtk_widget_set_sensitive(idpt('box4'), 0_c_int)
!call gtk_widget_set_sensitive(idpt('box5'), 0_c_int)
!call gtk_widget_set_sensitive(idpt('grid5'), 0_c_int)

    call FindItemS('notebook1', ncitem)
    if(ncitem == 0) then
        write(66,*) 'Error in PLPnew:  notebook1 not found!'
        ifehl = 1
    else
        call ProcMainDiag(ncitem, user_settings)
    end if

    IF(ifehl == 1) GOTO 100

    if(prout) write(66,*) 'PLoadpronew: button_LoadSymbols step started.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
    call FindItemS('button_LoadSymbols', ncitem)
    ! write(66,*) 'FindItemS: ncitem=',ncitem
    call ProcMainDiag(ncitem, user_settings)
    if(prout) write(66,*) 'PLoadpronew: button_LoadSymbols step finished.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)

    IF(ifehl == 1) GOTO 100

    if(FitDecay) then
        if(ifit(1) == 1) then
            call gtk_widget_set_sensitive(idpt('QFirst'), 1_c_int)
        else
            call gtk_widget_set_sensitive(idpt('QFirst'), 0_c_int)
        end if

        if(ifit(2) == 1) then
            call gtk_widget_set_sensitive(idpt('QSecond'), 1_c_int)
        else
            call gtk_widget_set_sensitive(idpt('QSecond'), 0_c_int)
        end if

        if(ifit(3) == 1) then
            call gtk_widget_set_sensitive(idpt('QThird'), 1_c_int)
        else
            call gtk_widget_set_sensitive(idpt('QThird'), 0_c_int)
        end if
    end if

33  continue

    if(simul_ProSetup .and. .not.done_simul_ProSetup) then
        call modifSymbols()
        if(nvarsMD > 0) call setMeanData()
        !call setDistPars()
    end if

    if(prout) write(66,*) 'PLoadpronew: LoadCompletedSyms step started.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
    call FindItemS('LoadCompletedSyms', ncitem)
    call ProcMainDiag(ncitem, user_settings)
    if(prout) write(66,*) 'PLoadpronew: LoadCompletedSyms step finished.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
    IF(ifehl == 1) GOTO 100

36  continue

    if(prout) write(66,*) 'PLoadpronew: AccepAll step started.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
    call FindItemS('AcceptAll', ncitem)
    call ProcMainDiag(ncitem, user_settings)
    if(prout) write(66,*) 'PLoadpronew: AcceptAll step finished.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)

    IF(ifehl == 1) GOTO 100


50  CONTINUE
! start label for the case of re-adjusting calculations after having changed another output quantity

    IF(kEGr == 0 .and. PRESENT(kEGrneu))  kEgr = kEGrneu
    IF(kEgr == 0) then
        WRITE(66,*) 'Error in ProcessLoadPro_new:  # of output quantity: kEGr=',int(kEGr,2)
        ifehl = 1
        goto 100
    end if

    call WDNotebookSetCurrPage('notebook1', 2)
    call gtk_widget_set_sensitive(idpt('comboboxNetRate'), 1_c_int)      ! 2.6.2023
    call WDGetComboboxAct('comboboxNetRate', kknetR)
    call gtk_widget_set_sensitive(idpt('comboboxGrossRate'), 1_c_int)
    call WDGetComboboxAct('comboboxGrossRate', kkbrutR)

    if(kpage1 > 1 .and. iwahl /= 0) then
        if(kpage1 > 2) then
            call WDNotebookSetCurrPage('notebook1', 2)
            if(prout) write(66,*) 'PLoadpronew: x -> 2:  previousPage=',int(NBpreviousPage,2),'   currentPage=',int(NBcurrentPage,2)
            call FindItemS('notebook1', ncitem)
            if(prout) write(66,*) ' PLPnew:  ncitem=',int(ncitem,2)
        end if

        if(gtk_widget_get_sensitive(idpt('LoadCompletedSyms')) >= 0_c_int) then
            call FindItemS('button_LoadSymbols', ncitem)
            call ProcMainDiag(ncitem, user_settings)
            if(prout) write(66,*) 'PLoadpronew: button_LoadSymbols done'
            IF(ifehl == 1 .OR. ifehlp == 1) GOTO 100

            call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'), 1_c_int)
            call FindItemS('LoadCompletedSyms', ncitem)
            call ProcMainDiag(ncitem, user_settings)
            if(prout) write(66,*) 'PLoadpronew: button_LoadCompletedSyms done'
            IF(ifehl == 1 .OR. ifehlp == 1) GOTO 100

            call gtk_widget_set_sensitive(idpt('AcceptAll'), 1_c_int)
            call FindItemS('AcceptAll', ncitem)
            call ProcMainDiag(ncitem, user_settings)
            if(prout) write(66,*) 'PLoadpronew: AcceptAll done'
            IF(ifehl == 1 .OR. ifehlp == 1) GOTO 100

            test1 = .false.
            if(kEGr_old > 0 .and. kEGr_old /= kEGr .and. .not.FitDecay .and. .not. Gamspk1_Fit) &
                test1 = knetto(kEGr_old) == knetto(kEGr) .or. kbrutto(kEGr_old) == kbrutto(kEGr)

            if( ( (knetto(kEGr) > 0 .and. kknetR /= knetto(kEGr)) .or. &
                (kbrutto(kEGr) > 0 .and. kkbrutR /= kbrutto(kEGr)) ) .and. test1 ) then
                if(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit) then
                    IF(langg == 'DE') WRITE(str1,*) 'Mindestens eins der beiden selektierten ' &
                        //'Zählraten-Symbole hat sich geändert!', &
                        CHAR(13),'Bitte überprüfen!'
                    IF(langg == 'EN') WRITE(str1,*) 'At least one of the two selected count rate symbols has changed!', &
                        CHAR(13),'Please, check!'
                    IF(langg == 'FR') WRITE(str1,*) 'Au moins un des deux symboles de taux de comptage sélectionnés a changé!', &
                        CHAR(13),'S''il vous plaît, vérifiez!'
                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
                    ifehl = 1
                    goto 100
                end if
            end if

            call WDNotebookSetCurrPage('notebook1', 3)
            if(prout) write(66,*) 'PLoadpronew: 2 -> 3:  previousPage=',int(NBpreviousPage,2),'   currentPage=',int(NBcurrentPage,2)
            call FindItemS('notebook1', ncitem)
            if(prout) write(66,*) ' PLPnew:  ncitem=',int(ncitem,2)
            call ProcMainDiag(ncitem, user_settings)
            if(prout) write(66,*) 'PLoadpronew: step finished.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
            IF(ifehl == 1 .OR. ifehlp == 1) GOTO 100
            ! ---------------------------------

        end if
    end if

70  continue     ! start label for repeating the evaluation

    if(simul_ProSetup .and. .not.done_simul_ProSetup) then
        if(nvarsMD > 0) call setMeanData()
        call setDistPars()
    end if

    call gtk_widget_set_sensitive(idpt('NBValUnc'), 1_c_int)
    call gtk_widget_set_visible(idpt('NBValUnc'), 1_c_int)
    call gtk_widget_set_sensitive(idpt('box4'), 1_c_int)
    if(.not.batest_on) call gtk_widget_show(idpt('box4'))

    call WDNotebookSetCurrPage('notebook1', 3)
    if(prout) write(66,*) 'PLoadpronew: 2 -> 3:  previousPage=',int(NBpreviousPage,2),'   currentPage=',int(NBcurrentPage,2)
    call FindItemS('notebook1', ncitem)
    if(prout) write(66,*) ' PLPnew:  ncitem=',int(ncitem,2)
    call ProcMainDiag(ncitem, user_settings)
    if(prout) write(66,*) 'PLoadpronew: step finished.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
    IF(ifehl == 1 .OR. ifehlp == 1) GOTO 100

75  continue

    if(simul_ProSetup .and. .not.done_simul_ProSetup) then
        call setDataTV2()
        call setCovTable
        if(FitDecay) then
            call setFdecayModel()
        end if
        if(Gamspk1_Fit) then
            call setGspk1Data()
        end if
        if(FitCalCurve) then
            call setKalfitData
        end if
        !FindItemS('BinPoiPars', ncitem)
        !call ProcMainDiag(ncitem, user_settings)
        !call setBinpoiDiag

        done_simul_ProSetup = .true.
        simul_ProSetup = .false.
    end if

    if(prout)  write(66,*) 'PLoadpronew: CalcValUnc step started.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
    call FindItemS('CalcValUnc', ncitem)
    call ProcMainDiag(ncitem, user_settings)
    if(prout) write(66,*) 'PLoadpronew: CalcValUnc step finished.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
    IF(ifehl == 1 .OR. ifehlp == 1) GOTO 100

    call WDNotebookSetCurrPage('notebook1', 4)
    if(prout) write(66,*) 'PLoadpronew: 3 -> 4:  previousPage=',int(NBpreviousPage,2),'   currentPage=',int(NBcurrentPage,2)
    call FindItemS('notebook1', ncitem)
    clobj%signal(ncitem)%s = 'switch-page'
    call ProcMainDiag(ncitem, user_settings)
    if(ifehl == 1 .or. ifehlp == 1) goto 100

    call gtk_widget_set_sensitive(idpt('NBBudget'), 1_c_int)
    call gtk_widget_set_sensitive(idpt('NBResults'), 1_c_int)
    call gtk_widget_set_visible(idpt('NBBudget'), 1_c_int)
    call gtk_widget_set_visible(idpt('NBResults'), 1_c_int)

    call gtk_widget_set_sensitive(idpt('box5'), 1_c_int)
    call gtk_widget_set_sensitive(idpt('grid5'), 1_c_int)
    if(.not.loadingPro) then
        call gtk_widget_show(idpt('box5'))
        call gtk_widget_show(idpt('grid5'))
    end if

    if(prout) write(66,*) 'PLoadpronew: step finished.    ifehl=',int(ifehl,2),'  ifehlp=',int(ifehlp,2)
    IF(ifehl == 1 .OR. ifehlp == 1) GOTO 100

    call WDNotebookSetCurrPage('notebook1', 5)
    call FindItemS('notebook1', ncitem)
    clobj%signal(ncitem)%s = 'switch-page'
    call ProcMainDiag(ncitem, user_settings)

    call gtk_widget_set_visible(idpt('TRButtonHelp'), 1_c_int)
    call gtk_widget_set_visible(idpt('TRbuttonSavecsv'), 1_c_int)

    call WrStb_Ready()


!   if(prout) write(66,*) 'PLoadpronew: 4 -> 5:  previousPage=',NBpreviousPage,'   currentPage=',NBcurrentPage

100 CONTINUE

    call WrStatusBar(2,' ')
    loadingPro = .FALSE.

    NBsoftSwitch = .false.
    item_setintern = .false.

    call gtk_widget_hide(idpt('dialog_LoadPro'))

    if(consoleout_gtk) write(0,*) '##### PLP  End  ###########################'
    if(prout) write(66,*) 'PLoadpronew: subroutine finished.','   SaveP=',SaveP,' ifehl=',int(ifehl,2)

end subroutine ProcessLoadPro_new
