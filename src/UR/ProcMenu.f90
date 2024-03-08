
!#################################################################################

recursive subroutine ProcMenu(ncitem)

   ! processing user actions in the graphical user interface
   ! called by SelOpt and PrepReport
   ! a minor part of actions is handled here, the major part is transferred to
   ! ProcMainDiag

   !     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,           only: rn,eps1min,zero,one
use PMD,                 only: procmaindiag
use, intrinsic :: iso_c_binding

use gtk,                 only: GTK_BUTTONS_YES_NO, GTK_RESPONSE_YES, GTK_BUTTONS_OK, &
                               GTK_RESPONSE_CANCEL, GTK_MESSAGE_WARNING, GTK_MESSAGE_INFO, &
                               gtk_widget_set_sensitive, gtk_widget_hide, &
                               gtk_check_menu_item_get_active, gtk_widget_set_visible, &
                               gtk_window_get_position, &
                               gtk_notebook_get_current_page, &
                               gtk_widget_set_size_request

use gdk,                 only: gdk_screen_get_monitor_at_point

use gtk_draw_hl,         only: hl_gtk_drawing_area_resize, gtkallocation

use UR_gtk_variables,    only: clobj, ioption, QuitProg, HelpButton, consoleout_gtk, &
                               dialog_leave, item_setintern, gscreen, scrwidth_min, &
                               scrwidth_max, scrheight_min, scrheight_max, plot_setintern, &
                               zoomf, replot_on, nbook2, zoomf_prev
use UR_Linft,            only: FitDecay, export_case, klincall, ifit, dmodif, SumEval_fit, export_r
use UR_Gspk1Fit,         only: Gamspk1_Fit, gmodif
use UR_variables
use UR_Gleich,           only: loadingpro, kEGr, refresh_type, Symbole, knetto, kbrutto, kEGr, &
                               knumEGr, ifehl, syntax_check, symlist_modified, linmod1_on, &
                               knumold, ngrs, refresh_but, incall, kEGr_old, apply_units, ncov, &
                               einheit, symtyp, syntax_check, retain_triggers

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


implicit none

integer(4),intent(in)  :: ncitem
integer(4)             :: k, ix, ios, resp, i, j, iwahl, nci2, kpi, &
                          ncurrp, notebook_last_free
integer(c_int)         :: cmoni, ccx, ccy, curp, szx, szy

character(len=60)      :: idstring, signal, parent, name, label, cheader
character(len=200)     :: str1
character(len=40)      :: str2
character(len=1)       :: ccc
character(len=12)      :: cpos

logical                :: SavepSV
logical                :: prout

type(c_ptr), target    :: rtx,rty
type(gtkallocation), target :: alloc
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
  if(prout) write(66,'(a,i0,2(a,a),2x,i16)') 'PM: ncitem=',ncitem,' idstring=',clobj%idd(ncitem)%s,' signal=',  &
                                               clobj%signal(ncitem)%s,clobj%id_ptr(ncitem)

if(i == 0) then
  !write(66,'(a,i0,2(a,a),2x,i16)') 'PM: i=0: ncitem=',ncitem,' idstring=',clobj%idd(ncitem)%s,' signal=', &
  !                            clobj%signal(ncitem)%s, clobj%id_ptr(ncitem)
end if
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
          write(66,*) 'use_BCI=',use_BCI,'  est1UQ_BCI=',sngl(est1UQ_BCI),'  xUQ=',sngl(xUQ)
      if(use_BCI .and. (abs(est1UQ_BCI) > eps1min .or. Gum_restricted)) then
        call WDPutEntryDouble('TRentryMClq', est1LQ_BCI, frmtres)
        call WDPutEntryDouble('TRentryMCuq', est1UQ_BCI, frmtres)
        call WDPutEntryDouble('TRentryMClqRSD', rx1LQbci, rmcformF(rx1LQbci))
        call WDPutEntryDouble('TRentryMCuqRSD', rx1UQbci, rmcformF(rx1UQbci))
      end if
      if(.not.use_BCI .and. (abs(xUQ) > eps1min .or. Gum_restricted)) then
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
        EditorFileName = trim(results_path) // 'Report.txt'      !
        write(66,*) 'EditorFileName=',trim(EditorFileName)
        call WDNotebookSetCurrPage('notebook1',6)
        call gtk_widget_set_sensitive(idpt('NBEditor'),1_c_int)
        call WDPutTextviewEditor('textviewEditor', EditorFileName, ifehl)
      end if
      goto 9000   !return

    case ('CheckUnits')
      do i=1,ngrs
        if(symtyp(i)%s == 't' .or.symtyp(i)%s == 'T') then
          IF(langg == 'DE') WRITE(str1,*) 'Mindestes ein Trigger im Projekt enthalten!'//char(13) &
                                          //'Sollen Trigger erhalten bleiben?'
          IF(langg == 'EN') WRITE(str1,*) 'At least one trigger included in the project!'//char(13) &
                                          //'Should triggers be retained?'
          IF(langg == 'FR') WRITE(str1,*) 'Au moins un déclencheur inclus dans le projet !' // char(13) &
                                          //'Faut-il conserver les déclencheurs ?'
          call MessageShow(trim(str1), GTK_BUTTONS_YES_NO, "PM:", resp,mtype=GTK_MESSAGE_WARNING)
          IF (resp == GTK_RESPONSE_YES) THEN   !                           ! -8
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
      call Report_Ucheck()
          call WDNotebookSetCurrPage('notebook1',6)
          call gtk_widget_set_sensitive(idpt('NBEditor'),1_c_int)
          call WDPutTextviewEditor('textviewEditor', EditorFileUcheck, ifehl)
          call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
          call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
          call gtk_widget_set_sensitive(idpt('NBValUnc'), 0_c_int)
      goto 9000


    case ('MenuQuitProgram', 'MenuLoadProject', 'TBCloseProject', 'window1',  &
          'TBLoadProject', 'MenuCloseProject' )    ! Closerequest????

      if(trim(idstring) == 'window1' .and. trim(signal) /= 'delete-event') then
        write(66,*) 'ProcMenu:   Bedingung für Return: idstring=',trim(idstring),'  signal=',trim(signal)
        goto 9000    !return
      end if
      FileTyp = 'P'
      IF (Filetyp == 'P' .AND. SAVEP) THEN
        IF(langg == 'DE') then
          str1 = 'Soll das geöffnete Projekt vor dem Beenden' //CHAR(13) //'gespeichert oder gesichert werden?'
          str2 = 'Projekt schließen:'
        END IF
        IF(langg == 'EN') then
          str1 = 'Shall the open project be saved before closing it? '
          str2 = 'Closing project:'
        END IF
        IF(langg == 'FR') then
          str1 = 'Le projet ouvert doit-il être sauvegardé avant de le fermer? '
          str2 = 'Projet de clôture:'
        END IF
        call MessageShow(trim(str1), GTK_BUTTONS_YES_NO, trim(str2), resp,GTK_MESSAGE_WARNING)
        IF (resp == GTK_RESPONSE_YES) THEN   !                           ! -8
          if(len_trim(fname)== 0) then
            cheader = 'Choose filename:'
            call FOpen(ifehl, .true., cheader )
            if(ifehl == 1) goto 9000   !return
          end if
          call ProSave()
        END IF
        if(resp == GTK_RESPONSE_CANCEL) then
          QuitProg = .false.
          goto 9000
        end if
      END IF
      IF(trim(idstring) == 'MenuQuitProgram' .or. trim(idstring) == 'window1') THEN
        QUITprog = .TRUE.
        goto 9000 !return
      end if
      IF(trim(idstring) == 'TBCloseProject' .or. trim(idstring) == 'MenuCloseProject' .or.        &
         trim(idstring) == 'TBLoadProject'  .or. trim(idstring) == 'MenuLoadProject') THEN
        QUITprog = .FALSE.
        incall = 1
        call UncW_Init()
        call WDNotebookSetCurrPage('notebook1',1)
        FileTyp = 'P'
        SAVEP = .FALSE.
        call FieldUpdate('PM 281')
          !  no goto 9000   !!
      END IF
      IF(trim(idstring) == 'window1' .and. (trim(signal) == 'delete-event' .or. trim(signal) == 'destroy')) THEN
        QuitProg = .true.
        goto 9000     !return
      END IF
      goto 9000   ! return

    case ('TBRefreshCalc')
      ! case of changing the actual outpout quantity: (1,kEGr);
      ! case of changes in the model :   (2, kEGr);
           write(66,'(a,i0)') 'TBRefreshCalc: refresh_type=',refresh_type
      if(refresh_type > 0) then
        IF(langg == 'DE') call WrStatusbar(4,'Rechnet...' )
        IF(langg == 'EN') call WrStatusbar(4,'Calculating...' )
        IF(langg == 'FR') call WrStatusbar(4,'Calcule...' )
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
      IF(Savep) THEN
        export_case = .FALSE.
        if(syntax_check) then
          ! 17.9.2023:
          refresh_type = 2        ! iwahl = 2
        else
          refresh_type = 3        ! iwahl = 3
        endif
        write(66,*)
        WRITE(66,*) '******************************* Change in the FitDecay model ***********'
        write(66,*) '             loadingPro=',loadingPro,'  project_laodw=',project_loadw, &
                       '  syntax_check=',syntax_check,' refresh_type=',int(refresh_type,2)
        write(66,*)

        if(.not.symlist_modified .and. dmodif) then    ! 31.1.2024
          refresh_type = 2
          goto 150
        end if
      else
        call WrStb_Ready()
        Savep = SavepSV
      end if
      goto 9000  !return

    case ('TBFittingResult', 'FittingResult', 'Gspk1Mean')
      call WDNotebookSetCurrPage('notebook1',6)
      call gtk_widget_set_sensitive(idpt('NBEditor'),1_c_int)
      if(FitDecay) EditorFileName = trim(results_path)//'linfout.txt'
      if(Gamspk1_Fit) EditorFileName = trim(results_path)//'linfout.txt'
      call WDPutTextviewEditor('textviewEditor', trim(EditorFileName), ifehl)
      if(FitDecay) then
        call CurvePlot()
      end if
      goto 9000   !return

    case ('QFirst','QSecond','QThird')
      if(gtk_check_menu_item_get_active(idpt(trim(idstring))) == 1_c_int ) then
        kEGr_old = kEGr
        if(trim(idstring) == 'QFirst') k = 1
        if(trim(idstring) == 'QSecond') k = 2
        if(trim(idstring) == 'QThird') k = 3
        write(str1,'(a,i1,a,a,a,i2)') '******** PM: selected: k=',k,  &
                          ' idstrg=',trim(idstring),' kEGr_old=',kEGr_old
        write(66,*) trim(str1)
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
          if(refresh_type > 0) call ProcessLoadPro_new(refresh_type,kEGr)
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
          write(66,'(a,i0,a,L1)') 'PM:  kModelType=',kModelType,'  gross_negative=',gross_negative
        if(kModelOld /= kModelType .and. ngrs > 0) then
              ! write(66,*) 'PM (386): SaveP=T gesetzt'
          SaveP = .true.
          call FieldUpdate('PM 388')
          refresh_type = 2
          goto 150
        end if

      end if

    case ('TBProblems', 'TBInfoDialog')
      call DisplayHelp(ncitem)
      goto 9000  !return

    case ('MonitorNum')
      rtx = c_null_ptr
      rty = c_null_ptr
      call gtk_window_get_position(idpt('window1'),c_loc(rtx),c_loc(rty))
      write(cpos,'(i8)',iostat=ios) rtx
        if(ios /= 0) write(66,*) 'PM: Monitornum: write error with: rtx=',rtx
      if(ios == 0) read(cpos,*,iostat=ios) ccx
        if(ios /= 0) write(66,*) 'PM: Monitornum: read error with: ccx: cpo=',rtx
      if(ios == 0) write(cpos,'(i8)',iostat=ios) rty
        if(ios /= 0) write(66,*) 'PM: Monitornum: write Error with: rty=',rty
      if(ios == 0) read(cpos,*,iostat=ios) ccy
      if(ios == 0) then
        cmoni = gdk_screen_get_monitor_at_point(gscreen,ccx+10_c_int, ccy+10_c_int)
        write(str1,'(a,i0,a1,a,i0,a,i0,a1,a,i0,a,i0)') ' Monitor#= ',cmoni+1_c_int,char(13), &
                                   ' width : ',scrwidth_min,' - ',scrwidth_max,char(13), &
                                   ' height: ',scrheight_min,' - ',scrheight_max
        call MessageShow('  '//trim(str1)//'  ', GTK_BUTTONS_OK, "Monitor#:", resp, mtype=GTK_MESSAGE_INFO)
      else
        ifehl = 1
        write(str1,'(a,i0)') 'Read/write error: see fort66.txt!'
        call MessageShow(trim(str1), GTK_BUTTONS_OK, "Monitor#:", resp, mtype=GTK_MESSAGE_WARNING)
      end if
      goto 9000

    case ('ExportToR')
      call WDGetCheckButton('ExportToR', k)
      ! IF(.not.export_r) THEN
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
      write(66,*) 'Signal changed recognized:    item=',trim(idstring)
      ! Zoom of graphics area
      if(trim(idstring) == 'ComboboxTextZoomGr') then
        call WDGetComboboxAct('ComboboxTextZoomGr',i)
        write(66,*) 'Zoom field i=',int(i,2)
            zoomf_prev = zoomf
        if(i == 1) Zoomf = 1.0_rn
        if(i == 2) Zoomf = 1.1_rn
        if(i == 3) Zoomf = 1.2_rn
           zoomfSV = zoomf
        curp = gtk_notebook_get_current_page(nbook2)
        kpi = curp + 1
          write(66,*) 'kpi=',int(kpi,2)
        alloc%width = int(width_da(kpi)*zoomf)
        alloc%height= int(height_da(kpi)*zoomf)

        call gtk_widget_set_size_request(drawing(kpi),alloc%width,alloc%height)

         !call hl_gtk_drawing_area_get_size(drawing(kpi), width, height)
         !     write(66,*) 'before resize: kpi=',int(kpi,2),' cc(kpi)=',cc(kpi),' width=',width,' height=',height
        szx = int(width_da(kpi)*zoomf)
        szy = int(height_da(kpi)*zoomf)

        if(c_associated(cc(kpi))) call hl_gtk_drawing_area_resize(drawing(kpi),size=[szx,szy], copy=.true.)    ! ///// GK
                write(66,*) 'after resize: width_da(kpi),height_da(kpi)=', &
                      width_da(kpi),height_da(kpi),'  szx,szy=',szx,szy

          replot_on = .true.
        call RePlot(kpi)
        call pending_events
          replot_on = .false.

      end if
      goto 9000

    case ('group-changed')
      write(66,*) 'Signal group-changed recognized:    item=',trim(idstring)
      goto 9000

    case ('check-resize')
      write(66,*) 'Signal check-resize recognized:    item=',trim(idstring)
      goto 9000

    case ('state-changed')
      ! deactivated in Glade
      write(66,*) 'Signal state-changed recognized:    item=',trim(idstring)
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
       if(prout) write(66,*) 'PM:   arrived before call ProcMainDiag:   id=',trim(idstring)
       knumold = knumEGr
    call ProcMainDiag(ncitem)
    if(ifehl == 0 .and. trim(idstring) == 'SerialEval' .and. QuitProg) goto 9000
         if(ifehl == 1 .or. ifehlp == 1) goto 9000 !return
    if(trim(idstring) == 'NumberOutputQuantities') then
         if(prout) write(66,'(a,i0,1x,i0,a,L1)') 'knumold,knumEGr=',knumold,knumEGr,' dialog_leave=',dialog_leave
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

write(66,*) 'ProcMenu:   call ProcMainDiag not applied:   id=',trim(idstring),'  signal=',trim(signal)
    ioption = 0

goto 9000 !return
!-------------------------------------------------------------------

150   CONTINUE

IF(.NOT.MCSim_on) THEN
  if(SaveP) call ClearMCFields(0)
  call gtk_widget_hide(idpt('window_graphs'))
end if

call WDPutEntryString('entryActiveKegr',Symbole(kEGr)%s)
IF(Fitdecay .or. Gamspk1_Fit) then       ! .or. multi_eval) THEN
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
      write(66,*) 'refresh_type=',refresh_type
  project_loadw = .true.
  call ProcessLoadPro_new(refresh_type,kEgr)
  !! Call WDNotebookSetCurrPage('notebook1', 5)
  ncurrp = notebook_last_free()                 ! 29.1.2024
  call WDNotebookSetCurrPage('notebook1', ncurrp)  ! 29.1.2024

else
  IF(knetto(kEGr) > 0 .AND. kbrutto(kEGr) > 0 .and. .not.Gum_restricted) THEN

    ! Check whether two output quantities have got the same vlues of kentto or kbrutto:
    do j=1,knumEGr
      IF(j == kEGr) CYCLE
      IF(knetto(kEgr) == knetto(j)) THEN
        IF(langg == 'DE') WRITE(str1,*) 'Warnung: für die Größen ',symbole(j)%s,' und ', &
                                        symbole(kEGr)%s,char(13),'ist für die Nettozählraten das identische ', &
                                        'Symbol ',symbole(knetto(kEGr))%s,' gewählt worden! Bitte beheben!'
        IF(langg == 'EN') WRITE(str1,*) 'Warning: for the quantities ',symbole(j)%s,' and ', &
                                        symbole(kEGr)%s,CHAR(13),'the identical symbol ', &
                                        symbole(knetto(kEGr))%s,' for the net ', &
                                        'counting rates has been selected! Please, correct!'
        IF(langg == 'FR') WRITE(str1,*) 'Attention: pour les quantités ',symbole(j)%s,' et ', &
                                        symbole(kEGr)%s, CHAR(13),'le symbole identique ', &
                                        symbole(knetto(kEGr))%s,' pour les taux de comptage nets ' // char(13) // &
                                        'a été sélectionné! S''il vous plaît, corrigez!'
        call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMenu:", resp,mtype=GTK_MESSAGE_WARNING)
        GOTO 9000
      end if
      IF(kbrutto(kEgr) == kbrutto(j)) THEN
        IF(langg == 'DE') WRITE(str1,*) 'Warnung: für die Größen ',symbole(j)%s,' und ', &
                                        symbole(kEGr)%s,char(13),'ist für die Bruttozählraten das identische ', &
                                        'Symbol ',symbole(kbrutto(kEGr))%s,' gewählt worden! Bitte beheben!'
        IF(langg == 'EN') WRITE(str1,*) 'Warning: for the quantities ',symbole(j)%s,' and ', &
                                        symbole(kEGr)%s, CHAR(13),'the identical symbol ',   &
                                        symbole(kbrutto(kEGr))%s,' for the gross ', &
                                        'counting rates has been selected! Please, correct!'
        IF(langg == 'FR') WRITE(str1,*) 'Attention: pour les quantités ',symbole(j)%s,' et ', &
                                        symbole(kEGr)%s, CHAR(13),'le symbole identique ',   &
                                        symbole(kbrutto(kEGr))%s,' pour le taux de comptage brut ', &
                                        'a été sélectionné! S''il vous plaît, corrigez!'
        call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMenu:", resp,mtype=GTK_MESSAGE_WARNING)
        GOTO 9000
      end if
    end do

    IF(.not.Gamspk1_Fit .and. .not.FitDecay) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
    IF(.not.Gamspk1_Fit .and. .not.FitDecay) call WDSetComboboxAct('comboboxGrossRate',kbrutto(kEGr))
    if(prout) WRITE(66,*) 'Set kEGr:  ##################  kEGr=',kEGr,'    knetto,kbrutto=',knetto(kEGr),kbrutto(kEGr)
    project_loadw = .true.
    call ProcessLoadPro_new(refresh_type, kEGr)
    ncurrp = notebook_last_free()                 ! 29.1.2024
    call WDNotebookSetCurrPage('notebook1', ncurrp)  ! 29.1.2024

    GOTO 9000
  else
    project_loadw = .true.

    if(.not.FitDecay .and. .not.Gamspk1_Fit .and. .not.Gum_restricted .and. .not.SumEval_fit) then
      if(knetto(kEGr) == 0 .or. kbrutto(kEGr) == 0 .and. project_loadw) then
        project_loadw = .false.
        loadingpro = .false.
        write(ccc,'(i1)') kEGr
        if(langg == 'DE') write(str1,*) 'Achtung: für die Ergebnisgröße Nr. ',ccc,  &
                            ' sind Netto- oder Bruttozählraten-Symbole noch nicht definiert!'
        if(langg == 'EN') write(str1,*) 'Warning: for output quantity no. ',ccc,  &
                            ' net or gross count rate symbols are not yet defined!'
        if(langg == 'FR') write(str1,*) 'Attention: pour la quantité de sortie n ° ',ccc,  &
                            ' les symboles de débit net ou brut ne sont pas encore définis!'
        call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMenu:", resp,mtype=GTK_MESSAGE_WARNING)
        ! ifehl = 1
        Call WDNotebookSetCurrPage('notebook1', 2)
        IF(langg == 'DE') call WrStatusBar(4,'Selektiere Netto- u. Bruttozählraten-Symbole!')
        IF(langg == 'EN') call WrStatusBar(4,'Select net and gross count rate symbols!')
        IF(langg == 'FR') call WrStatusBar(4,'Sélectionnez les symboles de taux de compte net et brut!')
        goto 9000
      end if
    end if

    call ProcessLoadPro_new(refresh_type, kEGr)      !
    ncurrp = notebook_last_free()                 ! 29.1.2024
    call WDNotebookSetCurrPage('notebook1', ncurrp)  ! 29.1.2024

    GOTO 9000

  end if
end if

WRITE(str1,*) 'MCSim_on=',MCSim_on
! call WindowOutStatusBar(2,TRIM(str1))

9000  continue

if(consoleout_gtk) &
   write(0,*) '##### PM:  End  ###########################'
          ! write(66,*) 'End of PM:  SaveP=',SaveP
          ! write(0,*) 'End of PM:  SaveP=',SaveP
end subroutine ProcMenu

!################################################################################

subroutine WrStb_Ready()
use Top,          only: WrStatusbar
use UR_Gleich,    only: ifehl
use UR_VARIABLES, only: langg

if(ifehl == 0) then
  IF(langg == 'DE') call WrStatusBar(4,'Fertig!' )
  IF(langg == 'EN') call WrStatusBar(4,'Ready!' )
  IF(langg == 'FR') call WrStatusBar(4,'Terminé !' )
else
  IF(langg == 'DE') call WrStatusBar(4,'Fertig, mit Fehler!' )
  IF(langg == 'EN') call WrStatusBar(4,'Ready, with error!' )
  IF(langg == 'FR') call WrStatusBar(4,'Terminé, avec erreur !' )
end if

end subroutine WrStb_Ready

!################################################################################

integer(4) function notebook_last_free()
    ! introduced 29.1.2024
    ! find the right-most Notebook tab which is accessible

    use, intrinsic :: iso_c_binding
    use gtk,          only: gtk_widget_get_sensitive
    use top,          only: idpt

    implicit none

    integer(4)         :: ncp
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
