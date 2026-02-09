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
recursive subroutine ProcMenu(ncitem)

    ! processing user actions in the graphical user interface
    ! called by SelOpt and PrepReport
    ! a minor part of actions is handled here, the major part is transferred to
    ! ProcMainDiag

    !     Copyright (C) 2014-2024  Günter Kanisch

    use UR_types
    use UR_params,           only: EPS1MIN
    use PMD,                 only: procmaindiag
    use, intrinsic :: iso_c_binding

    use gtk,                 only: GTK_BUTTONS_YES_NO, GTK_RESPONSE_YES, GTK_BUTTONS_OK, &
                                   GTK_MESSAGE_WARNING, GTK_MESSAGE_INFO, &
                                   GTK_RESPONSE_NO, &
                                   gtk_widget_set_sensitive, gtk_widget_hide, &
                                   gtk_check_menu_item_get_active, gtk_widget_set_visible, &
                                   gtk_window_get_position, &
                                   gtk_notebook_get_current_page, &
                                   gtk_widget_set_size_request

    use gdk,                 only: gdk_screen_get_monitor_at_point

    use gtk_draw_hl,         only: hl_gtk_drawing_area_resize, gtkallocation

    use UR_gtk_globals,    only: clobj, ioption, QuitProg, HelpButton, consoleout_gtk, &
                                   dialog_leave, item_setintern, gscreen, scrwidth_min, &
                                   scrwidth_max, scrheight_min, scrheight_max, plot_setintern, &
                                   zoomf, replot_on, nbook2, zoomf_prev
    use UR_Linft,            only: FitDecay, export_case, klincall, ifit, dmodif, SumEval_fit, export_r
    use UR_Gspk1Fit,         only: Gamspk1_Fit, gmodif
    use ur_general_globals
    use UR_Gleich_globals,   only: loadingpro, kEGr, refresh_type, Symbole, knetto, kbrutto, kEGr, &
                                   knumEGr, ifehl, syntax_check, symlist_modified, linmod1_on, &
                                   knumold, ngrs, refresh_but, kEGr_old, apply_units, ncov, &
                                   symtyp, syntax_check, retain_triggers

    use UR_DLIM,             only: KBgrenzu, KBgrenzo, KBgrenzuSH, KBgrenzoSH
    use UR_perror,           only: ifehlp
    use top,                 only: FindItemS, idpt, FieldUpdate, WrStatusbar, load_unit_conv
    use Rout,                only: WDNotebookSetCurrPage, WDPutTextviewEditor, fopen, Messageshow, &
                                   pending_events, WDPutEntryString, WDSetComboboxAct, ClearMCfields, &
                                   WDGetSelRadioMenu, EraseNWGfields, &
                                   WDGetCheckButton, WDPutEntryDouble, WDSetCheckButton,  &
                                   WDGetComboboxAct

    use urInit,              only: UncW_Init, ReadUnits
    use MCC,                 only: Run_MCstart
    use UR_MCC,              only: use_BCI, xLQ, xUQ, rxLQ, rxUQ, &
                                   est1LQ_bci, est1UQ_bci, rxLQ, rxUQ, rx1LQbci, rx1UQbci
    use PLsubs,              only: Replot
    use UR_interfaces,       only: DisplayHelp, ProcessLoadPro_new
    use PSave,               only: ProSave

    use RdSubs,              only: rmcformF
    use common_sub1,         only: cc, drawing, width_da, height_da
    use translation_module,  only: T => get_translation

    use file_io,             only: logger
    use UR_DecChain,         only: DChain


    implicit none

    integer, intent(in)    :: ncitem
    integer                :: k, ix, resp, i, j, iwahl, nci2, kpi, &
                              ncurrp, notebook_last_free
    integer(c_int)         :: cmoni, curp, szx, szy

    character(len=64)      :: idstring, signal, parent, name, label, cheader
    character(len=256)     :: str1
    character(len=64)      :: str2
    character(len=1)       :: ccc

    logical                :: SavepSV, prout

    integer(c_int), target :: rtx, rty
    type(gtkallocation), target :: alloc
    character(len=512)           :: log_str
    real(rn)               :: zoomfSV
!----------------------------------------------------------------------------
    prout = .false.
    ! prout = .true.
    ifehl = 0
    if(consoleout_gtk) write(0,*) '##### PM:  Anfang  ###########################'
    HelpButton = .false.
    signal = ''
    parent = ''

    idstring = clobj%idd(ncitem)%s
    signal = clobj%signal(ncitem)%s
    label  = clobj%label(ncitem)%s
    i = clobj%idparent(ncitem)

    if(i > 0) parent = clobj%name(i)%s

    if(i == 0 .and. trim(idstring) == 'window1') then
        parent = ''
        signal = 'delete-event'
    end if
    ! if(i == 0 .and. ncitem > 0) signal = clobj%signal(ncitem)

    name = clobj%name(ncitem)%s

    iwahl = 1    ! for Decayfit, Gamspk1
    refresh_type = 1
    refresh_but = .false.
    if(trim(label) == 'Hilfe') HelpButton = .true.
    if(trim(label) == 'Help') HelpButton = .true.
    if(HelpButton .and. trim(idstring) == 'HelpFX') HelpButton = .false.   ! 8.3.2024

    if(trim(parent) == 'GtkWindow' .or. len_trim(parent) == 0) then
        ! write(66,'(a,i1,a,i5,a,a)') 'PM: Start! kEGr=',kEGr,'  ncitem=',ncitem,' idstring=',trim(idstring)

        select case (trim(idstring))

          case ('TRButtonStartMC')
            item_setintern = .true.
            call Run_MCStart(ifehl)
            call pending_events()
            goto 9000

          case ('TRcheckbutton3')
            call WDGetCheckButton('TRcheckbutton3', ix)
            use_BCI = .FALSE.
            IF(ix == 1) use_BCI = .TRUE.
            if(use_BCI .and. (KBgrenzuSH >= 0._rn .or. Gum_restricted)) then
                call WDPutEntryDouble('TRentryLQBy', KBgrenzuSH, frmtres)
                call WDPutEntryDouble('TRentryUQBy', KBgrenzoSH, frmtres)
            end if
            if(.not.use_BCI) then
                call WDPutEntryDouble('TRentryLQBy', KBgrenzu, frmtres)
                call WDPutEntryDouble('TRentryUQBy', KBgrenzo, frmtres)
            end if
            goto 9000

          case ('TRcheckbutton2')
            call WDGetCheckButton('TRcheckbutton2', ix)
            use_BCI = .FALSE.
            IF(ix == 1) use_BCI = .TRUE.
            write(log_str, '(*(g0))') 'use_BCI=',use_BCI,'  est1UQ_BCI=',sngl(est1UQ_BCI),'  xUQ=',sngl(xUQ)
            call logger(66, log_str)
            if(use_BCI .and. (abs(est1UQ_BCI) > EPS1MIN .or. Gum_restricted)) then
                call WDPutEntryDouble('TRentryMClq', est1LQ_BCI, frmtres)
                call WDPutEntryDouble('TRentryMCuq', est1UQ_BCI, frmtres)
                call WDPutEntryDouble('TRentryMClqRSD', rx1LQbci, rmcformF(rx1LQbci))
                call WDPutEntryDouble('TRentryMCuqRSD', rx1UQbci, rmcformF(rx1UQbci))
            end if
            if(.not.use_BCI .and. (abs(xUQ) > EPS1MIN .or. Gum_restricted)) then
                call WDPutEntryDouble('TRentryMClq', xLQ, frmtres)
                call WDPutEntryDouble('TRentryMCuq', xUQ, frmtres)
                call WDPutEntryDouble('TRentryMClqRSD', rxLQ, rmcformF(rxLQ))
                call WDPutEntryDouble('TRentryMCuqRSD', rxUQ, rmcformF(rxUQ))
            end if
            goto 9000

          case ('Report')
            if(ngrs > 0 .and. knumEGr > 0) then
                call gtk_widget_set_visible(idpt('TEClose'), 1_c_int)
                call PrepReport
                ! if(.not.wpunix) EditorFileName = trim(results_path) // 'Report.txt'
                EditorFileName = results_path // 'Report.txt'      !
                write(log_str, '(*(g0))') 'EditorFileName=',trim(EditorFileName)
                call logger(66, log_str)
                call WDNotebookSetCurrPage('notebook1',6)
                call gtk_widget_set_sensitive(idpt('NBEditor'),1_c_int)
                call WDPutTextviewEditor('textviewEditor', EditorFileName, ifehl)
            end if
            goto 9000   !return

          case ('CheckUnits')
            do i=1,ngrs
                if(symtyp(i)%s == 't' .or.symtyp(i)%s == 'T') then
                    write(str1,*) T("At least one trigger included in the project!")//new_line('A') // &
                                  T("Should triggers be retained?")

                    call MessageShow(trim(str1), GTK_BUTTONS_YES_NO, "PM:", resp,mtype=GTK_MESSAGE_WARNING)
                    IF (resp == GTK_RESPONSE_YES) then   !                           ! -8
                        retain_triggers = .true.
                        exit
                    end if
                end if
            end do
            call Save_Ucheck()
            apply_units = .true.
            call gtk_widget_set_visible(idpt('TEClose'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('CheckUnits'), 0_c_int)
            call ReadUnits()
            call load_unit_conv(ngrs+ncov)
            call CalcUnits()
            !call ProcessLoadPro_new(3,kEGr)      ! Aufruf für die Ergebnisgröße kEGr

            !! call Report_Ucheck()
            ! if(ifehl == 0) then                   !  <-- 19.11.2024  GK
            if(ifehl >= 0) then                   !  <-- 2.8.2025  GK
                call Report_Ucheck()
                call WDNotebookSetCurrPage('notebook1',6)
                call gtk_widget_set_sensitive(idpt('NBEditor'),1_c_int)
                call WDPutTextviewEditor('textviewEditor', EditorFileUcheck, ifehl)
                call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
                call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
                call gtk_widget_set_sensitive(idpt('NBValUnc'), 0_c_int)
            end if
            goto 9000


          case ('MenuQuitProgram', 'MenuLoadProject', 'TBCloseProject', 'window1',  &
                'TBLoadProject', 'MenuCloseProject' )    ! Closerequest????

            if(trim(idstring) == 'window1' .and. trim(signal) /= 'delete-event') then
                write(log_str, '(*(g0))') 'ProcMenu:   Bedingung für Return: idstring=',trim(idstring),'  signal=',trim(signal)
                call logger(66, log_str)
                goto 9000    !return
            end if
            FileTyp = 'P'
            IF (Filetyp == 'P' .AND. SAVEP) then
                    str1 = T('Shall the open project be saved before closing it?')
                    str2 = T('Close Project')

                call MessageShow(trim(str1), GTK_BUTTONS_YES_NO, trim(str2), resp,GTK_MESSAGE_WARNING)
                IF (resp == GTK_RESPONSE_YES) then   !                           ! -8
                    if(len_trim(fname)== 0) then
                        cheader = 'Choose filename:'
                        call FOpen(ifehl, .true., cheader )
                        if(ifehl == 1) goto 9000   !return
                    end if
                    call ProSave()
                END IF
                if(resp == GTK_RESPONSE_NO) then
                    QuitProg = .false.
                end if
            END IF
            IF(trim(idstring) == 'MenuQuitProgram' .or. trim(idstring) == 'window1') then
                QUITprog = .TRUE.
                goto 9000 !return
            end if
            IF(trim(idstring) == 'TBCloseProject' .or. trim(idstring) == 'MenuCloseProject' .or.        &
                trim(idstring) == 'TBLoadProject'  .or. trim(idstring) == 'MenuLoadProject') then
                QUITprog = .FALSE.
                call UncW_Init()
                call WDNotebookSetCurrPage('notebook1',1)
                FileTyp = 'P'
                SAVEP = .FALSE.
                call FieldUpdate()

            END IF
            IF(trim(idstring) == 'window1' .and. (trim(signal) == 'delete-event' .or. trim(signal) == 'destroy')) then
                QuitProg = .true.
                goto 9000     !return
            END IF
            goto 9000   ! return

          case ('TBRefreshCalc')
            ! case of changing the actual outpout quantity: (1,kEGr);
            ! case of changes in the model :   (2, kEGr);
            write(log_str, '(a,i0)') 'TBRefreshCalc: refresh_type=',refresh_type
            call logger(66, log_str)
            if(refresh_type > 0) then
                call WrStatusbar(4, T('Calculating') // '....' )
                call pending_events()
                call pending_events()
                call pending_events()
                refresh_but = .true.

                call ProcessLoadPro_new(refresh_type,kEGr)
            end if
            goto 9000    ! return

          case ('TBModelDialog', 'FittingModel')
            ! If in the model dialog a change occurred, the calculations have to be repeated
            SavepSV = Savep
            Savep = .FALSE.
            IF(FitDecay) then
                linmod1_on = .true.
                call Linmod1(2)
                linmod1_on = .false.
            END IF

            iwahl = 1
            IF(Savep) then
                export_case = .FALSE.
                if(syntax_check) then
                    ! 17.9.2023:
                    refresh_type = 2        ! iwahl = 2
                else
                    refresh_type = 3        ! iwahl = 3
                endif
                call logger(66, ' ')
                call logger(66, '******************************* Change in the FitDecay model ***********')
                write(log_str, '(*(g0))') '             loadingPro=',loadingPro,'  project_laodw=',project_loadw, &
                    '  syntax_check=',syntax_check,' refresh_type=',int(refresh_type,2)
                call logger(66, log_str)
                call logger(66, ' ')

                if(.not.symlist_modified .and. dmodif) then    ! 31.1.2024
                    refresh_type = 2
                    goto 150
                end if
            else
                call WrStb_Ready(ifehl)
                Savep = SavepSV
            end if
            goto 9000  !return

          case ('TBFittingResult', 'FittingResult', 'Gspk1Mean')
            call WDNotebookSetCurrPage('notebook1',6)
            call gtk_widget_set_sensitive(idpt('NBEditor'),1_c_int)
            if(FitDecay) EditorFileName = results_path // 'linfout.txt'
            if(Gamspk1_Fit) EditorFileName = results_path // 'linfout.txt'
            call WDPutTextviewEditor('textviewEditor', trim(EditorFileName), ifehl)
            if(FitDecay) then
                call CurvePlot()
            end if
            goto 9000   !return

          case ('QFirst','QSecond','QThird')
            if (kEGr > 0) then
                if(gtk_check_menu_item_get_active(idpt(trim(idstring))) == 1_c_int ) then
                    kEGr_old = kEGr
                    if(trim(idstring) == 'QFirst') k = 1
                    if(trim(idstring) == 'QSecond') k = 2
                    if(trim(idstring) == 'QThird') k = 3
                    write(str1,'(a,i1,a,a,a,i2)') '******** PM: selected: k=',k,  &
                                                ' idstrg=',trim(idstring),' kEGr_old=',kEGr_old
                    call logger(66, trim(str1))

                    ! for the FitDecay model: select an output quantity only, if it has to be fitted
                    if(.not.FitDecay .or. (FitDecay .and. ifit(k) == 1)) then
                        kEGr = k
                        IF(kEGr /= kEGr_old) then
                            if(MCsim_on) then
                                MCSim_on = .FALSE.
                                call ClearMCfields(1)
                            end if
                        end if
                        if(FitDecay) klincall = 0
                        ! write(66,'(3a,i0,2x,i0)') 'PM:  widget=',trim(idstring),'   kEGr_old, kegr=',kEGr_old, kegr
                        refresh_type = 1
                        if(refresh_type > 0) call ProcessLoadPro_new(refresh_type, kEGr)
                    end if
                end if
            end if
            goto 9000   ! return

          case ('MT_PosLin','MT_GUMonly','MT_NegLin')
            kModelOld = kModelType
            if(gtk_check_menu_item_get_active(idpt(trim(idstring))) == 1_c_int ) then
                if(trim(idstring) == 'MT_PosLin') then
                    Gum_restricted = .false.
                    gross_negative = .false.
                    kModelType = 1
                    call WDGetSelRadioMenu('MT_NegLin',k)
                end if
                if(trim(idstring) == 'MT_GUMonly') then
                    Gum_restricted = .true.
                    gross_negative = .false.
                    kModelType = 2
                    call WDGetSelRadioMenu('MT_NegLin',k)
                end if
                if(trim(idstring) == 'MT_NegLin') then
                    Gum_restricted = .false.
                    gross_negative = .true.
                    kModelType = 3
                    call WDGetSelRadioMenu('MT_NegLin',k)
                end if
                call EraseNWGfields()
                write(log_str, '(a,i0,a,L1)') 'PM:  kModelType=',kModelType,'  gross_negative=',gross_negative
                call logger(66, log_str)
                if(kModelOld /= kModelType .and. ngrs > 0) then
                    ! write(66,*) 'PM (386): SaveP=T gesetzt'
                    SaveP = .true.
                    call FieldUpdate()
                    refresh_type = 2
                    goto 150
                end if

            end if

          case ('TBProblems', 'TBInfoDialog')
            call DisplayHelp(ncitem)
            goto 9000  !return

          case ('MonitorNum')

            call gtk_window_get_position(idpt('window1'),c_loc(rtx),c_loc(rty))


            cmoni = gdk_screen_get_monitor_at_point(gscreen,rtx+10_c_int, rty+10_c_int)
            write(str1,'(a,i0,a1,a,i0,a,i0,a1,a,i0,a,i0)') ' Monitor#= ',cmoni+1_c_int,char(13), &
                ' width : ',scrwidth_min,' - ',scrwidth_max,char(13), &
                ' height: ',scrheight_min,' - ',scrheight_max
            call MessageShow('  '//trim(str1)//'  ', GTK_BUTTONS_OK, "Monitor#:", resp, mtype=GTK_MESSAGE_INFO)
            goto 9000

          case ('ExportToR')
            call WDGetCheckButton('ExportToR', k)
            ! IF(.not.export_r) then
            if(k == 0) then
                call WDSetCheckButton('ExportToR', 1)
                export_r = .TRUE.
            else
                call WDSetCheckButton('ExportToR', 0)
                export_r = .FALSE.
            end if
            goto 9000  ! return

          case default
        end select

        select case (trim(signal))

          case ('changed')
            write(log_str, '(*(g0))') '"Signal changed" recognized:    item=',trim(idstring)
            call logger(66, log_str)
            ! Zoom of graphics area
            if(trim(idstring) == 'ComboboxTextZoomGr') then
                call WDGetComboboxAct('ComboboxTextZoomGr',i)
                write(log_str, '(*(g0))') 'Zoom field i=',int(i,2)
                call logger(66, log_str)
                zoomf_prev = zoomf
                if(i == 1) Zoomf = 1.0_rn
                if(i == 2) Zoomf = 1.1_rn
                if(i == 3) Zoomf = 1.2_rn
                zoomfSV = zoomf
                curp = gtk_notebook_get_current_page(nbook2)
                kpi = curp + 1
                write(log_str, '(*(g0))') 'kpi=',int(kpi,2)
                call logger(66, log_str)
                alloc%width = int(width_da(kpi)*zoomf)
                alloc%height= int(height_da(kpi)*zoomf)

                ! call gtk_widget_set_size_request(drawing(kpi),alloc%width,alloc%height)

                !call hl_gtk_drawing_area_get_size(drawing(kpi), width, height)
                !     write(66,*) 'before resize: kpi=',int(kpi,2),' cc(kpi)=',cc(kpi),' width=',width,' height=',height
                szx = int(width_da(kpi)*zoomf)
                szy = int(height_da(kpi)*zoomf)

                if(c_associated(cc(kpi))) call hl_gtk_drawing_area_resize(drawing(kpi),size=[szx,szy], copy=.true.)    ! ///// GK
                write(log_str, '(*(g0))') 'after resize: width_da(kpi),height_da(kpi)=', &
                    width_da(kpi),height_da(kpi),'  szx,szy=',szx,szy
                call logger(66, log_str)

                replot_on = .true.
                call RePlot(kpi)
                call pending_events
                replot_on = .false.

            end if
            goto 9000

          case ('group-changed')
            write(log_str, '(*(g0))') 'Signal group-changed recognized:    item=',trim(idstring)
            call logger(66, log_str)
            goto 9000

          case ('check-resize')
            write(log_str, '(*(g0))') 'Signal check-resize recognized:    item=',trim(idstring)
            call logger(66, log_str)
            goto 9000

          case ('state-changed')
            ! deactivated in Glade
            write(log_str, '(*(g0))') 'Signal state-changed recognized:    item=',trim(idstring)
            call logger(66, log_str)
            if(item_setintern) goto 9000
            if(plot_setintern) goto 9000
            goto 9000

          case ('size-allocate')
            ! write(66,*) 'Signal size-allocate recognized:    item=',trim(idstring)
            !call RePlot()
            !call pending_events
            goto 9000

        end select

        if(trim(name) == 'GtkButton' .and. HelpButton) then
            call DisplayHelp(ncitem)
            goto 9000  ! return
        end if
        if(trim(idstring) == 'TBmeansMD') then
            call ProcMainDiag(ncitem)
            goto 9000  ! return
        end if
        if(trim(idstring) == 'TBRemoveGridLine') then
            call ProcMainDiag(ncitem)
            goto 9000  ! return
        end if

        if(trim(name) == 'GtkButton' .or. trim(name) == 'GtkRadioMenuItem' .or.  &
            trim(name) == 'GtkNotebook' .or. trim(name) == 'GtkMenuItem' .or.      &
            trim(name) == 'GtkToolButton' .or. trim(name) == 'GtkImageMenuItem' .or.  &
            trim(name) == 'GtkDialog' .or. trim(name) == 'GtkCheckMenuItem' .or.  &
            trim(name) == '') then
            if(prout)  then
                write(log_str, '(*(g0))') 'PM:   arrived before call ProcMainDiag:   id=',trim(idstring)
                call logger(66, log_str)
            end if
            knumold = knumEGr
            call ProcMainDiag(ncitem)
            if(ifehl == 0 .and. trim(idstring) == 'SerialEval' .and. QuitProg) goto 9000
            if(ifehl == 1 .or. ifehlp == 1) goto 9000 !return
            if(trim(idstring) == 'NumberOutputQuantities') then
                if(prout)  then
                    write(log_str, '(a,i0,1x,i0,a,L1)') 'knumold,knumEGr=',knumold,knumEGr,' dialog_leave=',dialog_leave
                    call logger(66, log_str)
                end if
                if(dialog_leave == 1 .and. knumold == 1 .and. knumold < knumEGr) then
                    loadingPro = .true.
                    call FindItemS('button_LoadSymbols', nci2)
                    call ProcMainDiag(nci2)
                    loadingPro = .false.
                end if
            end if
            if(ioption == 67) then
                call ProcessLoadPro_new(2,1)
                ioption = 0
            end if
            if( (Gamspk1_Fit .and. gmodif) .or. (FitDecay .and. dmodif) ) then
                refresh_type = 1
                if(ioption == 2 .or. ioption == 3) refresh_type = 3
                if(.not.symlist_modified) goto 150
            end if

            goto 9000 !return
        end if

    end if

    write(log_str, '(*(g0))') 'ProcMenu:   call ProcMainDiag not applied:   id=',trim(idstring),'  signal=',trim(signal)
    call logger(66, log_str)
    ioption = 0

    goto 9000 !return
!-------------------------------------------------------------------

150 CONTINUE

    IF(.NOT.MCSim_on) then
        if(SaveP) call ClearMCFields(0)
        call gtk_widget_hide(idpt('window_graphs'))
    end if

    call WDPutEntryString('entryActiveKegr',Symbole(kEGr)%s)
    IF(Fitdecay .or. Gamspk1_Fit) then       ! .or. multi_eval) then
        !--+-
        call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)

        call gtk_widget_set_visible(idpt('NBBudget'), 0_c_int)
        call gtk_widget_set_visible(idpt('NBResults'), 0_c_int)

        call gtk_widget_set_sensitive(idpt('box5'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('grid5'), 0_c_int)
        call gtk_widget_hide(idpt('box5'))
        call gtk_widget_hide(idpt('grid5'))

        ! call pending_events()
        write(log_str, '(*(g0))') 'refresh_type=',refresh_type
        call logger(66, log_str)
        project_loadw = .true.
        call ProcessLoadPro_new(refresh_type,kEgr)
        !! Call WDNotebookSetCurrPage('notebook1', 5)
        ncurrp = notebook_last_free()                 ! 29.1.2024
        call WDNotebookSetCurrPage('notebook1', ncurrp)  ! 29.1.2024

    else
        IF(knetto(kEGr) > 0 .AND. kbrutto(kEGr) > 0 .and. .not.Gum_restricted) then

            ! Check whether two output quantities have got the same values of knetto or kbrutto:
            do j=1,knumEGr
                IF(j == kEGr) CYCLE
                IF(knetto(kEgr) == knetto(j)) then

                    write(str1,*) T('Warning') // ": " // T('For the quantities') // " " // symbole(j)%s // " "// T('and') // " ", &
                                  symbole(kEGr)%s // new_line('A') // T('the identical symbol for the net counting rates has been selected!') // ": " //  &
                                  symbole(knetto(kEGr))%s // T('Please, correct!')

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMenu:", resp,mtype=GTK_MESSAGE_WARNING)
                    GOTO 9000
                end if
                IF(kbrutto(kEgr) == kbrutto(j)) then

                    write(str1,*) T('Warning') // ": " // T('For the quantities') // " " // symbole(j)%s // " "// T('and') // " ", &
                        symbole(kEGr)%s // new_line('A') // T('the identical symbol for the gross counting rates has been selected!') // ": " //  &
                        symbole(knetto(kEGr))%s // T('Please, correct!')

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMenu:", resp,mtype=GTK_MESSAGE_WARNING)
                    GOTO 9000
                end if
            end do

            IF(.not.Gamspk1_Fit .and. .not.FitDecay) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
            IF(.not.Gamspk1_Fit .and. .not.FitDecay) call WDSetComboboxAct('comboboxGrossRate',kbrutto(kEGr))
            if(prout)  then
                write(log_str, '(*(g0))') 'Set kEGr:  ##################  kEGr=',kEGr,'    knetto,kbrutto=',knetto(kEGr),kbrutto(kEGr)
                call logger(66, log_str)
            end if
            project_loadw = .true.
            call ProcessLoadPro_new(refresh_type, kEGr)
            ncurrp = notebook_last_free()                 ! 29.1.2024
            call WDNotebookSetCurrPage('notebook1', ncurrp)  ! 29.1.2024

            GOTO 9000
        else
            ! project_loadw = .true.
            if(.not.simul_ProSetup) project_loadw = .true.         ! 17.11.2024        27.4.2025

            if(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not.Gum_restricted .and. &
               .not.DChain .and.  &                                     ! 27.4.2025
               .not.SumEval_fit) then
                if(knetto(kEGr) == 0 .or. kbrutto(kEGr) == 0 .and. project_loadw) then
                    project_loadw = .false.
                    loadingpro = .false.
                    write(ccc,'(i1)') kEGr
                    write(str1,*) T("Warning: for output quantity no.") // " " // ccc // &
                                  T("net or gross count rate symbols are not yet defined!")//new_line('A') &
                                  //ccc
                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMenu:", resp,mtype=GTK_MESSAGE_WARNING)
                    ! ifehl = 1
                    Call WDNotebookSetCurrPage('notebook1', 2)
                    call WrStatusBar(4,T("Select net and gross count rate symbols!"))

                    goto 9000
                end if
            end if

            call ProcessLoadPro_new(refresh_type, kEGr)      !
            ncurrp = notebook_last_free()                 ! 29.1.2024
            call WDNotebookSetCurrPage('notebook1', ncurrp)  ! 29.1.2024

            goto 9000

        end if
    end if

    write(str1,*) 'MCSim_on=',MCSim_on
    ! call WindowOutStatusBar(2,TRIM(str1))

9000 continue

    if(consoleout_gtk) &
        write(0,*) '##### PM:  End  ###########################'
    ! write(66,*) 'End of PM:  SaveP=',SaveP
    ! write(0,*) 'End of PM:  SaveP=',SaveP
end subroutine ProcMenu

!################################################################################

subroutine WrStb_Ready(ifehl)
    use Top,          only: WrStatusbar
    use translation_module, only: T => get_translation

    integer, intent(in) :: ifehl

    if(ifehl == 0) then
        call WrStatusBar(4, T('Ready') // "!")
    else
        call WrStatusBar(4, T('Ready') // ", " // T('with error') // "!")

    end if

end subroutine WrStb_Ready

!################################################################################

integer function notebook_last_free()
    ! introduced 29.1.2024
    ! find the right-most Notebook tab which is accessible

    use, intrinsic :: iso_c_binding
    use gtk,          only: gtk_widget_get_sensitive
    use top,          only: idpt

    implicit none

    integer            :: ncp
    integer(c_int)     :: resp

    ncp = 5
    resp = gtk_widget_get_sensitive(idpt('NBResults'))
    if(resp == 0_c_int) ncp = 4
    resp = gtk_widget_get_sensitive(idpt('NBBudget'))
    if(resp == 0_c_int) ncp = 3
    resp = gtk_widget_get_sensitive(idpt('NBValUnc'))
    if(resp == 0_c_int) ncp = 2

    notebook_last_free = ncp

end function notebook_last_free

!################################################################################

subroutine ReloadMissD()

! This routine allows to select an older version of a specific UR project file,
! which then scans it for values (and uncertainties) of those members of the arrays
! Messwert and StdUnc being associated with 'independent' input quantities.
!
! These are used to update those values missing in the current version of the
! specific project file.
! This helps in setting up a new version of the current project file, if the previous
! version does not load any longer, but has already (most of) the required values in it.
!
! GK 2.5.2025

    use UR_types,          only: rn
    use UR_params,         only: EPS1MIN
    use UR_Loadsel,        only: missdata_file
    use UR_Gleich_globals, only: nab, ngrs, Symbole, Messwert, IVTL, SDFormel, SDWert, &
                                 HBreite, IAR, missingval
    use CHF,               only: ucase, flfu
    use Rout,              only: WTreeViewPutDoubleCell, WTreeViewPutStrCell, WTreeViewPutComboCell


    implicit none

    integer             :: i,j,ii,i1,i2,ios,ivt,kk,nnab,nabb, nio
    real(rn)            :: mw,sdw,hbr
    character(len=120)  :: ffm,text
    character(len=20)   :: varb

    open(newunit=nio, file=flfu(missdata_file), status='old')

    do
        read(nio,'(a)',iostat=ios) text
        if(ios /= 0) exit
        if(index(text,'@Symbole-GRID:') == 1) then
            do i=1,5
              read(nio,'(a)') text
            end do
            nnab = 0
            do i=1,50
              read(nio,'(a)') text
              i1 = index(text,'#')
              if(i1 > 1) then
                varb = trim(text(1:i1-1))
                text = text(i1+1:)
                i2 = index(text,'#')
                if(i2 > 0) then
                  if(adjustL(trim(ucase(text(1:i2-1)))) == 'U') goto 20
                end if
              end if
            end do
        end if

    end do

20  continue
    nabb = i-1
    ! varb ist die erste U-Variable:
    do
        read(nio,'(a)',iostat=ios) text
        if(ios /= 0) exit
        if(index(text,'@Unc-Grid:') == 1) then
            do j=1,nabb
              read(nio,'(a)') text
            end do
            exit
        endif
    end do

    do j=1, 150
        read(nio,'(a)', iostat=ios) text
        if(text(1:1) == '@') exit
        i1 = index(text,'#')
        varb = trim(text(1:i1-1))

        do ii = nab+1, ngrs
            if(Symbole(ii)%s == trim(varb) .and. abs(Messwert(ii) - missingval) <= EPS1MIN) then
                kk = 1
                do
                  i1 = index(text,'#')
                  text = text(i1+1:)
                  i1 = index(text,'#')
                  if(ios /= 0) exit
                  if(i1 > 0) then
                    kk = kk + 1
                    if(kk == 2) then; read(text(1:i1-1),*) mw; if(mw > missingval) then; Messwert(ii) = mw; call WTreeViewPutDoubleCell('treeview2',5,ii,mw); end if; end if
                    if(kk == 3) then; read(text(1:i1-1),*) ivt; if(ivt > 0) then; IVTL(ii) = ivt; call WTreeViewPutComboCell('treeview2',6,ii,ivt); end if; end if;
                    if(kk == 4) then; ffm = adjustL(text(1:i1-1)); if(len_trim(ffm) > 3) then; SDformel(ii)%s = trim(ffm); call WTreeViewPutStrCell('treeview2',7,ii,trim(ffm)); end if; end if;
                    if(kk == 5) then; read(text(1:i1-1),*) sdw; if(sdw > missingval) then; SDWert(ii) = sdw; call WTreeViewPutDoubleCell('treeview2',8,ii,sdw);end if; end if;
                    if(kk == 6) then; read(text(1:i1-1),*) hbr; if(hbr > missingval) then; HBreite(ii) = hbr; call WTreeViewPutDoubleCell('treeview2',9,ii,hbr);end if; end if;
                    if(kk == 7) then; read(text(1:i1-1),*) ivt; if(ivt > 0) then; IAR(ii) = ivt; call WTreeViewPutComboCell('treeview2',10,ii,ivt);end if; end if;
                    if(kk == 7) exit
                  endif
                end do
            end if
        end do
    end do
    close (nio)

end subroutine ReloadMissD

!###################################################################################
