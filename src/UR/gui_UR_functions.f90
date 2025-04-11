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

module gui_functions

    !   Copyright (C) 2014-2025  Günter Kanisch

    use, intrinsic :: iso_c_binding
    use UR_types, only: rn

    implicit none

    private
    public :: create_window, &
              show_window, &
              lowcase, &
              UR_field_edit_cb, &
              SetColors

contains

    ! create_window, connect_signals
    ! SelOpt,
    ! UR_key_Press_cb
    ! ProjectOpen_cb
    ! ProjectSave_cb
    ! UR_tree_text_edit_cb
    ! UR_field_edit_cb
    ! UR_field_doact_cb
    ! UR_tree_toggle_edit_cb
    ! UR_NBPage_switched_cb
    ! UR_ActualTreeV_cb
    ! UR_TV_button_pressed_cb
    ! UR_TV_button_released_cb
    ! UR_TV_column_clicked_cb
    ! SetColors

    ! c_f_string_chars



    subroutine create_window(UR_widgets, ifehl)

        ! this routine uses a gtk_builder to build the window from the Glade file
        !
        ! It then connects the signals.
        !
        ! UncW_init is then called for a lot of initialisations; the label strings of the
        ! widgets are then translated into the selected language by the routine TranslateUR.
        !
        ! Finally, the geometric sizes of the graphical windows are defined/prepared.
        !
        ! Significant parts are taken from GTK-Fortran.

        use UR_gtk_globals,       only: Notebook_labeltext, nbook2,  &
                                        consoleout_gtk, &
                                        scrwidth_min, scrwidth_max, scrheight_min, scrheight_max, &
                                        gscreen, provider, builder

        use ur_general_globals,   only: SaveP, project_loadw

        use g,                    only: g_object_unref

        use gtk,                  only: gtk_builder_new,gtk_builder_get_object, &
                                        gtk_widget_set_sensitive,gtk_builder_connect_signals_full, &
                                        gtk_label_get_text, &
                                        gtk_notebook_page_num, &
                                        gtk_window_set_transient_for, &
                                        gtk_widget_grab_focus,gtk_widget_set_focus_on_click, &
                                        TRUE,FALSE,gtk_notebook_set_current_page, &
                                        gtk_style_context_add_provider_for_screen, &
                                        gtk_css_provider_get_default, &
                                        gtk_builder_add_from_resource

        use gtk_sup,              only: gvalue, Gerror, c_f_string
        use Top,                  only: WrStatusbar, idpt
        use URinit,               only: Uncw_Init
        use gdk,                  only: gdk_cursor_new, gdk_synthesize_window_state, &
                                        gdk_display_get_default_screen,  &
                                        gdk_display_get_default

        use Rout,                 only: pending_events, WDPutLabelColorB, WDPutLabelColorF
        use gtk_draw_hl,          only: gtkallocation, hl_gtk_drawing_area_new
        use gtk_hl,               only: hl_gtk_notebook_new,hl_gtk_notebook_add_page, &
                                        hl_gtk_button_new,hl_gtk_box_pack

        use file_io,              only: logger
        use common_sub1,          only: drawboxpackedMC, drawboxpackedELI, &
                                        drawboxpackedBS,drawboxpackedCP, &
                                        draw_baseELI, drawing, width_da, height_da


        use handlers_sub1,        only: quit_cb
        use UR_types,             only: widgets_named


        implicit none

        type(widgets_named), intent(out), target :: UR_widgets
        integer, intent(out)        :: ifehl

        type(c_ptr)                 :: qbut
        type(c_ptr), target         :: error
        integer(c_int)              :: guint
        type(c_ptr)                 :: cptr
        integer(c_int)              :: pno
        real(rn)                    :: start,finish

        integer                     :: i
        character(len=512)          :: log_str

        ifehl = 0

        guint = 0
        ! load GUI into builder
        builder = gtk_builder_new()

        call cpu_time(start)

        error = c_null_ptr        ! necessary
        guint = gtk_builder_add_from_resource(builder, "/org/UncertRadio/UR2_5.glade" // c_null_char, &
                                              c_loc(error))

        call cpu_time(finish)
        write(log_str, '(a,f8.3,a,i0)') 'Builder_add_from_string: cpu-time= ', sngl(finish-start),'  guint=',guint
        call logger(66, log_str)
        if(consoleout_gtk) write(0,*) 'Behind processing the Glade file'

        if (guint == 0_c_int) then    ! False
            if(c_associated(error)) call EvalGerror('Load glade from string: ',error)

            write(log_str, '(a,a)') "  c_associated(Error)=",c_associated(error)
            call logger(66, log_str)
            call logger(66, "Could not load the glade file")
        end if

        write(log_str, '(*(g0))') 'URGladesys done: cpu-time= ', sngl(finish-start)
        call logger(66, log_str)


        ! get references to some GUI elements
        ! The name passed to the gtk_builder_get_object function has to match the name
        ! of the objects in the Glade file
        !
        UR_widgets%window1 = gtk_builder_get_object(builder, "window1"//c_null_char)
        UR_widgets%notebooks(1) = gtk_builder_get_object(builder, "NBProcedure"//c_null_char)
        UR_widgets%notebooks(2) = gtk_builder_get_object(builder, "NBEquations"//c_null_char)
        UR_widgets%notebooks(3) = gtk_builder_get_object(builder, "NBValUnc"//c_null_char)
        UR_widgets%notebooks(4) = gtk_builder_get_object(builder, "NBBudget"//c_null_char)
        UR_widgets%notebooks(5) = gtk_builder_get_object(builder, "NBResults"//c_null_char)
        UR_widgets%notebooks(6) = gtk_builder_get_object(builder, "NBEditor"//c_null_char)
        UR_widgets%dialog_infofx = gtk_builder_get_object(builder, "dialog_infoFX"//c_null_char)
        UR_widgets%comboboxtextinfofx = gtk_builder_get_object(builder, "comboboxtextInfoFX"//c_null_char)

        ! connect signal handlers
        call gtk_builder_connect_signals_full(builder, c_funloc(connect_signals), c_null_ptr)

        call gtk_widget_set_sensitive(idpt('MenuDecayCurve'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('MenuGSpekt1'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('KalFit'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('ExportToR'), 0_c_int)

        call gtk_widget_set_sensitive(idpt('TBModelDialog'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('TBInputDialog'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('TBFittingResult'), 0_c_int)

        provider = gtk_css_provider_get_default()

        call gtk_style_context_add_provider_for_screen(gscreen, provider, 800)

        call SetColors()
        !----
        drawboxpackedMC = .false.
        drawboxpackedELI = .false.
        draw_baseELI = idpt('boxELI')
        !----
        drawboxpackedBS = .false.
        drawboxpackedCP = .false.

        call Uncw_Init()

        call cpu_time(start)
        call TranslateUR()
        call cpu_time(finish)

        write(log_str, '(*(g0))') 'TranslateUR: cpu-time= ', sngl(finish-start)
        call logger(66, log_str)
        SaveP = .False.
        project_loadw = .TRUE.

        do i=1,6
            cptr = gtk_label_get_text(UR_widgets%notebooks(i))
            call c_f_string(cptr, Notebook_labeltext(i))
        end do

        call WrStatusbar(3,' ')

        if(.false.) then

            write(log_str, '(*(g0))') 'Page 1: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box2'))+1
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'Page 2: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box3'))+1
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'Page 3: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box4'))+1
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'Page 4: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box5'))+1
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'Page 5: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('grid5'))+1
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'Page 6: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box7'))+1
            call logger(66, log_str)
        end if
        call gtk_window_set_transient_for(idpt('dialogDecayModel'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialogColB'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialogELI'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_LoadPro'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_decayvals'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_fontbutton'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_gspk1'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_kalfit'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_numegr'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_options'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_symbExchg'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_symbchg'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_symbchg'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialogMeanData'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialogSerEval'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_BinPoi'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_distributions'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialog_Batest'), UR_widgets%window1)
        call gtk_window_set_transient_for(idpt('dialogBatEval'), UR_widgets%window1)
        call gtk_window_set_transient_for(UR_widgets%dialog_infofx, UR_widgets%window1)

        call gtk_widget_grab_focus(idpt('textview1'))


        call gtk_widget_set_focus_on_click(UR_widgets%window1, 1_c_int)

        !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        nbook2 = c_null_ptr
        nbook2 = hl_gtk_notebook_new()
        call hl_gtk_box_pack(idpt('box_wgraphs'), nbook2,expand=TRUE)

        ! Re-scaling with the ratios between stadndard size and another monitor size

        width_da(1) = 580
        height_da(1) = 710
        width_da(1) = int( width_da(1) * real(scrwidth_max - scrwidth_min,rn)/1920._rn + 0.4999_rn)
        height_da(1) = int( height_da(1) * real(scrheight_max - scrheight_min,rn)/1050._rn + 0.4999_rn)
        drawing(1) = hl_gtk_drawing_area_new(size=(/width_da(1),height_da(1)/), has_alpha=FALSE)
        pno = hl_gtk_notebook_add_page(nbook2, drawing(1), label="MC"//c_null_char)


        !width_da(2) = 580
        !height_da(2) = 710
        !  width_da(2) = int( width_da(2) * real(scrwidth_max - scrwidth_min,rn)/1920._rn + 0.4999_rn)
        !  height_da(2) = int( height_da(2) * real(scrheight_max - scrheight_min,rn)/1050._rn + 0.4999_rn)
        !drawing(2) = hl_gtk_drawing_area_new(size=(/width_da(2),height_da(2)/),has_alpha=FALSE)
        !pno = hl_gtk_notebook_add_page(nbook2, drawing(2),label="MCMC"//c_null_char)

        width_da(3) = 580    ! 561       ! 580:
        height_da(3) = 540   ! 440
        width_da(3) = int( width_da(3) * real(scrwidth_max - scrwidth_min,rn)/1920._rn + 0.4999_rn)
        height_da(3) = int( height_da(3) * real(scrheight_max - scrheight_min,rn)/1050._rn + 0.4999_rn)
        drawing(3) = hl_gtk_drawing_area_new(size=(/width_da(3),height_da(3)/),has_alpha=FALSE)
        pno = hl_gtk_notebook_add_page(nbook2, drawing(3),label="LinF"//c_null_char)

        qbut = hl_gtk_button_new("Close"//c_null_char, clicked=c_funloc(quit_cb))
        call hl_gtk_box_pack(idpt('box_wgraphs'), qbut, expand=FALSE)

        call gtk_notebook_set_current_page(nbook2,0_c_int)

        height_da(4) = int(438._rn*0.938_rn)
        width_da(4) = 458

        width_da(4) = int( width_da(4) * real(scrwidth_max - scrwidth_min,rn)/1920._rn + 0.4999_rn)
        height_da(4) = int( height_da(4) * real(scrheight_max - scrheight_min,rn)/1050._rn + 0.4999_rn)

        drawing(4) = hl_gtk_drawing_area_new(size=(/width_da(4),height_da(4)/), &
            has_alpha=FALSE)

        call hl_gtk_box_pack(idpt('boxELI'), drawing(4), expand=True, fill=True, &
            atend=True)
        drawboxpackedELI = .true.

        write(*,*) 'end of create_window'
        !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    end subroutine create_window

    !############################################################################

    subroutine show_window(widget_ptr)

        use gtk, only: gtk_widget_show, &
                       gtk_window_set_gravity, &
                       GDK_gravity_NORTH_WEST

        implicit none

        type(c_ptr), intent(in) :: widget_ptr

        call gtk_window_set_gravity(widget_ptr, GDK_gravity_NORTH_WEST)
        call gtk_widget_show(widget_ptr)

    end subroutine show_window

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !Window and object creation above
    !==================================================================================
    !
    !
    !
    !==================================================================================
    !Connect signals to objects below
    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

    subroutine connect_signals(builder, object, signal_name, handler_name, connect_object, flags, c_Win) bind(c)

        use, intrinsic :: iso_c_binding,    only: c_ptr, c_char, c_int
        use file_io,          only: logger
        use gtk_sup,          only: c_f_string, f_c_string, convert_c_string_scalar
        use gtk,              only: g_signal_connect, gtk_builder_get_type, gtk_widget_get_name, &
                                    gtk_buildable_get_name
        use Rout,             only: get_gladeid_name, get_widget_class

        use UR_gtk_window_types, only: widget_type
        implicit none

        type(c_ptr), value               :: builder           !a GtkBuilder
        type(c_ptr), value               :: object            !object to connect a signal to
        character(kind=c_char), target   :: signal_name(*)    !name of the signal
        character(kind=c_char), target   :: handler_name(*)   !name of the handler
        type(c_ptr), value               :: connect_object    !a GObject, if non-NULL, use g_signal_connect_object()
        integer(c_int), value            :: flags             !GConnectFlags to use
        type(c_ptr), value               :: c_Win             !user data

        character(len=25), target        :: h_name, h_signal, galdeid
        character(kind=c_char), allocatable :: tmp_str(:)
        type(widget_type), pointer       :: ur_widget
        !--------------------------------------------------------------------------------------------------------------

        call c_f_string_chars(handler_name, h_name)
        call c_f_string_chars(signal_name, h_signal)

        allocate(widget_type :: ur_widget)

        ur_widget%id_ptr = object
        call f_c_string(h_signal, tmp_str)
        ur_widget%signal(1:size(tmp_str)) = tmp_str
        call f_c_string(h_name, tmp_str)
        ur_widget%handler(1:size(tmp_str)) = tmp_str

        ! don't get the glade id and class, if the objects are no buildable
        ! flo: better
        ur_widget%gladeid(:) = ''
        if (h_name /= 'edit_table' .and. h_name /= 'edit_t_toggle') then
            call f_c_string(get_gladeid_name(object), tmp_str)
            ! print *, h_signal, h_name, 'STILL'
            ur_widget%gladeid(1:size(tmp_str)) = tmp_str

            call f_c_string(get_widget_class(object), tmp_str)
            ur_widget%classname(1:size(tmp_str)) = tmp_str
        else
            call f_c_string(get_gladeid_name(object), tmp_str)
            ! print *, h_signal, h_name, 'NOT'
        end if

        c_Win = c_loc(ur_widget)
        select case (h_name)

        ! Add event handlers created in Glade below, otherwise the widgets won't connect to functions
        ! The names in the case have to match the names of the *signals* in Glade and the
        ! text in c_funloc(...) has to match the name of the actual function in the code.

        case ("clickbut")
            call g_signal_connect (object, signal_name, c_funloc(button_clicked), c_Win)
        case ("SelOpt")
            call g_signal_connect (object, signal_name, c_funloc(SelOpt), c_Win)
        case ("LoadProjectFile")
            call g_signal_connect (object, signal_name, c_funloc(ProjectOpen_cb), c_Win)
        case ("SavePro")
            call g_signal_connect (object, signal_name, c_funloc(ProjectSave_cb), c_Win)
        case ("edit_table")
            call g_signal_connect (object, signal_name, c_funloc(UR_tree_text_edit_cb), c_Win)
        case ("edit_t_toggle")
            call g_signal_connect (object, signal_name, c_funloc(UR_tree_toggle_edit_cb), c_Win)
        case ("edit_field")
            call g_signal_connect (object, signal_name, c_funloc(UR_field_edit_cb), c_Win)
        case ("doact")
            call g_signal_connect (object, signal_name, c_funloc(UR_Field_doact_cb), c_Win)
        case ("pageswitch")   !  for notebook1'
            call g_signal_connect (object, signal_name, c_funloc(UR_NBPage_switched_cb), c_Win)
        case ("ActualTreeV")
            call g_signal_connect (object, signal_name, c_funloc(UR_ActualTreeV_cb))

        case("on_help_button_clicked")
            call g_signal_connect (object, signal_name, c_funloc(on_help_button_clicked), c_Win)

        case("on_destroy_selected")
            call g_signal_connect (object, signal_name, c_funloc(on_destroy_selected), c_Win)

        case("on_show_monitor_info")
            call g_signal_connect (object, signal_name, c_funloc(on_show_monitor_info), c_Win)

        case("on_show_dialog")
            call g_signal_connect (object, signal_name, c_funloc(on_show_dialog), c_Win)

        case("on_change_infofx_topic")
            call g_signal_connect (object, signal_name, c_funloc(on_change_infofx_topic), c_Win)

        case("on_close_dialog")
            call g_signal_connect (object, signal_name, c_funloc(on_close_dialog), c_Win)

        case("on_show_about_windows")
            call g_signal_connect (object, signal_name, c_funloc(on_show_about_windows), c_Win)

        case ("keyPress")
            call g_signal_connect (object, signal_name, c_funloc(UR_keyPress_cb), c_Win)
        case ("col_clicked")
            call g_signal_connect (object, signal_name, c_funloc(UR_TV_column_clicked_cb), c_Win)
        case default
            call logger(66, "Connect signals: Unknown handler = " // h_name, stdout=.true.)
            call logger(66, "Program terminated")
            stop "Program terminated"
        end select

    end subroutine connect_signals

    !==================================================================================
    ! Create functions for the gui below.  Then attach them to the gui's code using event
    ! handlers in the above
    ! function.  Attach the event handlers to the actual buttons or whatever in the gui
    ! using Glade
    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv


    ! Note: -------------------------------------------------------------------
    !  Event Callbacks are functions (>= 3 args)
    !  Signal Callbacks are subroutines (args)
    !-----------------------------------------------------------------------------

    !#####################################################################################

    recursive subroutine SelOpt(widget, gdata) bind(c) ! result(ret)

        ! the routine SelOpt is invoked by many of the user actions on widgets of the
        ! main window or on dialogs. SelOpt identifies the id name of the corresponding
        ! widget. It then forwards the minor part of the cases, especially requates from the
        ! main menu of the main window to ProcMenu for further treatment. The majority of
        ! cases is forwarded to ProcMainDiag, which then executes the tasks required by the
        ! action. It checks after return from ProcMenu for a request to close the program;
        ! in case of a QuitProg, it stops the main GTK loop, wehereafter the program ends
        ! in the main routine gui_UR_main.
        !

        use, intrinsic :: iso_c_binding, only: c_null_char, &
                                               c_ptr, c_int, &
                                               c_int16_t, &
                                               c_associated

        use gtk, only: gtk_main_quit, &
                       gtk_widget_set_focus_on_click, &
                       gtk_widget_destroy, &
                       gtk_buildable_get_name


        use UR_gtk_globals,     only: UR_widgets, item_setintern, &
                                      ioption, dialogstr, quitprog
        use Rout,               only: get_gladeid_name
        use file_io,            only: logger
        use ur_general_globals, only: actual_grid


        implicit none


        type(c_ptr), value    :: widget, gdata
        type(c_ptr)           :: ctext

        character(len=64)     :: signal, parentstr, f_text
        character(:), allocatable :: error, idstring
        integer               :: ncitem, i
        !---------------------------------------------------------------------
        if(item_setintern) return

        idstring = get_gladeid_name(widget, error)

        if(len(error) == 0) then
            ioption = 1000
            dialogstr = ''

            if(trim(idstring) /= 'TBRemoveGridLine' .and. trim(idstring) /= 'TBRemoveGridLine') return


            ! i = clobj%idparent(ncitem)
            ! if(i > 0) then
            !     parentstr = clobj%name(i)%s
            ! else
            !     ! if the signal does not come from window1:
            !     parentstr = ''
            !     signal = 'delete-event'
            ! end if
            ! name = clobj%name(ncitem)%s
        else

            ! write(log_str, '(*(g0))') '****** SelOpt:  non-associated widget: ', widget
            ! call logger(66, log_str)
            write(0,*) '****** SelOpt:  non-associated widget: '!, widget
            return
        end if

        if(trim(parentstr) == 'GtkWindow' .or. trim(idstring) == 'window1'    &
            .or. trim(idstring) == 'window_graphs' .or. trim(actual_grid) >= 'treeview5' ) then

            ! call ProcMenu(ncitem)
            if( .not. QuitProg) call gtk_widget_set_focus_on_click(UR_widgets%window1, 1_c_int)
            if(QuitProg) then
                ! if(c_associated(gdkcursor)) call g_object_unref(gdkcursor)

                call gtk_widget_destroy(UR_widgets%window1)
                call gtk_main_quit()
            end if
        end if

    end subroutine Selopt

    !------------------------------------------------------------------------------------------

    subroutine button_clicked(widget, gdata) bind(c)    ! result(ret)

        ! this function identifies the widget sets ButtonClicke and/or HelpButton to true.
        !
        ! The latter variables are used in the GTK dialog loop, located in the middle of
        ! Loadsel_diag_new, where the loop would then terminates, and Loadsel_diag_new will
        ! then react on that.

        use, intrinsic :: iso_c_binding, only: c_null_char, c_ptr, c_int, c_int16_t

        use UR_gtk_globals,    only: HelpButton, str_item_clicked, &
                                     ButtonClicked, ncitemClicked
        use gtk, only: gtk_buildable_get_name
        use gtk_sup, only: c_f_string
        use UR_Gleich_globals, only: loadingpro
        use chf,               only: ucase
        use rout,              only: get_gladeid_name

        implicit none

        type(c_ptr), value     :: widget, gdata
        type(c_ptr)            :: item_clicked

        item_clicked = widget

        str_item_clicked = get_gladeid_name(widget)

        if(trim(str_item_clicked) == 'LoadWithCalc') goto 10
        if(trim(str_item_clicked) == 'LoadWithoutCalc') goto 10
        if(trim(str_item_clicked) == 'BinPoiOK' .or. trim(str_item_clicked) == 'BinPoiCancel') goto 10

        if(loadingpro) return
10      continue

        ! stritem = ucase(str_item_clicked)


        ! write(66,*) 'button_clicked:   HelpButton=',HelpButton
        ! ret = True
        ButtonClicked = .true.

    end subroutine button_clicked

    !---------------------------------------------------------------------------------------------!
    subroutine on_help_button_clicked(widget) bind(c)

        use gtk_hl_combobox, only: hl_gtk_combo_box_get_active
        use UR_gtk_globals,  only: UR_widgets
        use CHF,             only: ucase
        use Rout,            only: get_gladeid_name
        use file_io,         only: logger
        use UR_params,       only: fxtopics

        implicit none
        type(c_ptr), value, intent(in) :: widget
        character(:), allocatable :: button_id, error, topic
        integer(c_int) :: i_fx
        !-----------------------------------------------------------------------------------------!

        button_id = get_gladeid_name(widget, error)
        if (len(error) == 0) then
            topic = button_id
            if (button_id == 'HelpFX') then

                i_fx = hl_gtk_combo_box_get_active(UR_widgets%comboboxtextinfofx)
                if (i_fx > 0) topic = trim(fxtopics(i_fx))

            end if

            call DisplayHelp(topic)
        else
            call logger(65, 'Error: on_help_button_clicked: widget is not associated!')
        end if

    end subroutine on_help_button_clicked

    !---------------------------------------------------------------------------------------------!
    subroutine on_destroy_selected(widget, data0) bind(c)

        use gtk, only: gtk_widget_destroy, gtk_main_quit
        use UR_gtk_globals, only: UR_widgets
        use file_io, only: logger

        implicit none
        type(c_ptr), value, intent(in) :: widget, data0
        !-----------------------------------------------------------------------------------------!
        ! tbd. save project?

        call gtk_widget_destroy(UR_widgets%window1)
        call gtk_main_quit()


    end subroutine on_destroy_selected

    !---------------------------------------------------------------------------------------------!
    subroutine on_change_infofx_topic(widget, data0) bind(c)

        use gtk,                 only: gtk_widget_show_all, gtk_image_set_from_resource, gtk_image_clear
        use gtk_hl_combobox,     only: hl_gtk_combo_box_get_active

        use UR_gtk_globals,      only: UR_widgets
        use ur_general_globals,  only: help_path
        use UR_gtk_window_types, only: charv
        use top,                 only: idpt,CharModA1
        use rout,                only: WDPutTextviewString
        use translation_module,  only: get_language
        use CHF,                 only: ucase, flfu

        use file_io, only: logger

        implicit none
        type(c_ptr), value, intent(in) :: widget, data0
        integer(c_int) :: i_fx
        integer                    :: ios, i, nfd, imax
        type(charv), allocatable   :: textcode(:)
        character(len=100)         :: iomessg
        character(len=400)         :: text, textfile
        character(len=15)          :: code
        !-----------------------------------------------------------------------------------------!
        i_fx = hl_gtk_combo_box_get_active(UR_widgets%comboboxtextinfofx)
        if ( i_fx == 0 ) return

        call gtk_image_clear(idpt('InfoFX_image1'))
        call gtk_image_clear(idpt('InfoFX_image2'))
        call gtk_image_clear(idpt('InfoFX_image3'))

        select case (i_fx + 1)
            case (2)
            code = 'LINFIT'
            call gtk_image_set_from_resource(idpt('InfoFX_image1'), &
                                                '/org/UncertRadio/icons/preferences-system.png' // c_null_char)
            call gtk_image_set_from_resource (idpt('InfoFX_image2'), &
                                                '/org/UncertRadio/icons/FittingData_24.png' // c_null_char)
            call gtk_image_set_from_resource (idpt('InfoFX_image3'), &
                                                '/org/UncertRadio/icons/FittingResults_24.png' // c_null_char)


            case (3)
            code = 'GAMSPK1'
            call gtk_image_set_from_resource (idpt('InfoFX_image2'), &
                                                '/org/UncertRadio/icons/FittingData_24.png' // c_null_char)
            call gtk_image_set_from_resource (idpt('InfoFX_image3'), &
                                                '/org/UncertRadio/icons/FittingResults_24.png' // c_null_char)

            case (4)
            code = 'KALFIT'

            case (5)
            code = 'SUMEVAL'

            case (6)
            code = 'UVAL'

            case (7)
            code = 'FD'

        end select

        allocate(textcode(15))

        textfile = help_Path // 'InfoFX1.txt'

        nfd = 0
        open(35, file=flfu(textfile), status='old')
        do
            read(35,'(a)',iostat=ios,iomsg=iomessg) text
!             if(ios /= 0) write(66,*) 'headline: error=',trim(iomessg)
            if(ios /= 0) call logger(66, 'headline: error=' // trim(iomessg) )

            if(ios /= 0) exit
            if(get_language() == 'de' .and. index(ucase(text),'#DE#') > 0) exit
            if((get_language()== 'en' .or. get_language() == 'fr') &
                .and. index(ucase(text),'#EN#') > 0) exit
        end do
        do
            read(35,'(a)',iostat=ios,iomsg=iomessg) text
            if(ios /= 0) call logger(66, 'headline: error=' // trim(iomessg))

            if(ios /= 0) exit
            if(text(1:1) == '#' .and. index(ucase(text),trim(code)) == 2) then
                nfd = 1
                do i=1,15
                    read(35,'(A)',iostat=ios,iomsg=iomessg) text
                    if(i > 3 .and. (text(1:1) == '#' .or. ios /= 0)) then
                        imax = i -1
                        exit
                    end if
                    textcode(i)%s = trim(text)
                    ! write(66,*) textcode(i)%s
                    if(ios /= 0) then

                        call logger(66, 'error=' //trim(iomessg))
                        exit
                    end if
                end do
            end if
            if(nfd == 1) exit
        end do
        close (35)

        do i=imax, 3, -1
            if(len_trim(textcode(i)%s) > 0) then
                imax = i
                call CharModA1(textcode,imax)
                exit
            end if
        end do
        call WDPutTextviewString('textview_InfoFX', textcode)


    end subroutine on_change_infofx_topic

    !---------------------------------------------------------------------------------------------!
    subroutine on_show_dialog(widget, data0) bind(c)

        use gtk, only: gtk_widget_show_all
        use UR_gtk_globals, only: UR_widgets
        use file_io, only: logger

        implicit none
        type(c_ptr), value, intent(in) :: widget, data0
        !-----------------------------------------------------------------------------------------!

        call gtk_widget_show_all(UR_widgets%dialog_infofx)


    end subroutine on_show_dialog

    !---------------------------------------------------------------------------------------------!
    subroutine on_close_dialog(widget, data0) bind(c)

        use gtk, only: gtk_widget_hide
        use UR_gtk_globals, only: UR_widgets
        use file_io, only: logger

        implicit none
        type(c_ptr), value, intent(in) :: widget, data0
        !-----------------------------------------------------------------------------------------!

        call gtk_widget_hide(UR_widgets%dialog_infofx)


    end subroutine on_close_dialog

    !---------------------------------------------------------------------------------------------!

    subroutine on_show_about_windows(widget, data0) bind(c)
        use gtk, only: TRUE, GTK_LICENSE_GPL_3_0, GTK_LICENSE_GPL_2_0_ONLY, &
                       GTK_LICENSE_LGPL_2_1, GTK_LICENSE_BSD_3, &
                       gtk_get_major_version, gtk_get_minor_version, gtk_get_micro_version
        use gtk_hl, only: hl_gtk_about_dialog_show, hl_gtk_about_dialog_gtk_fortran
        use gdk_pixbuf,     only: gdk_pixbuf_new_from_resource_at_scale
        use ur_general_globals, only: UR_version_tag, UR_git_hash
        use UR_gtk_globals, only: UR_widgets
        use UR_params, only: CR
        use rout, only: get_gladeid_name
        use file_io, only: logger
        use translation_module, only: get_language


        implicit none
        type(c_ptr), value, intent(in) :: widget, data0
        type(c_ptr)               :: logo
        character(len=128)        :: authors(6)
        character(len=256)        :: url_str, versgtk
        character(len=2048)       :: comment_str
        character(:), allocatable :: idstring
        !-----------------------------------------------------------------------------------------!

        idstring = get_gladeid_name(widget)
        select case(idstring)
        case ('About_UR')

            if (get_language() == 'de') then
                url_str =  'https://www.thuenen.de/de/fachinstitute/fischereioekologie/arbeitsbereiche/' &
                    // 'meeresumwelt/leitstelle-umweltradioaktivitaet-in-fisch/uncertradio' // c_null_char
                comment_str = 'Programm zur Berechnung von Messunsicherheit, ' // CR &
                    // 'Unsicherheiten-Budget, Erkennungs- und Nachweisgrenze bei' // CR &
                    // 'Messungen der Umweltradioaktivität ' // CR // CR &
                    // 'Das Programm steht unter der GNU GPL v3 Lizenz und wurde vom Autor nach derzeitigem Stand von Wissenschaft,' // CR &
                    // 'Normung und Technik entwickelt und bezüglich der Richtigkeit der ' // CR &
                    // 'mathematischen Behandlung der eingegebenen Modellgleichungen validiert.' // CR // CR &
                    // 'E-Mail:    guenter.kanisch(at)hanse.net' // CR  &
                    // '           florian.ober(at)mri.bund.de' // CR  &
                    // '           leitstelle-fisch(at)thuenen.de' // c_null_char
                authors(1) = 'Günter Kanisch, früher Thünen-Institut für Fischereiökologie, Hamburg'
                authors(2) = '    (Hauptentwickler, Dokumentation und Bereitstellung von Windows-Versionen bis 2024)'
                authors(3) = 'Florian Ober, Max Rubner-Institut, Kiel'
                authors(4) = '    (Weiterentwicklung und Betreuung des Github Repositorys)'
                authors(5) = 'Marc-Oliver Aust, Thünen-Institut für Fischereiökologie, Bremerhaven'
                authors(6) = '    (Anwenderberatung, Betreuung der Projektseite)'
            else
                url_str =  'https://www.thuenen.de/en/institutes/fisheries-ecology/fields-of-activity/' &
                    // 'marine-environment/coordination-centre-of-radioactivity/uncertradio' // c_null_char
                comment_str = trim('Software for calculating measurement uncertainty, ' // CR &
                    // 'uncertainty budget, decision threshold and detection limit for' // CR &
                    // 'measurement of environmental radioactivity.' // CR // CR &
                    // 'The software is licensed under GNU GPL3 and was developed by the author following state-of-the-art ' // CR &
                    // 'of science, standardization and technology and validated with respect' // CR &
                    // 'to the correct mathematical treatment of the model input equations of' // CR &
                    // 'the evaluation model.' // CR // CR &
                    // 'E-Mail:    guenter.kanisch(at)hanse.net' // CR &
                    // '           florian.ober(at)mri.bund.de' // CR  &
                    // '           leitstelle-fisch(at)thuenen.de') // c_null_char
                authors(1) = 'Günter Kanisch, formerly at the Thünen Institute of Fisheries Ecology, Hamburg'
                authors(2) = '    (Main developer, documentation and provision of Windows versions until 2024)'
                authors(3) = 'Florian Ober, Max Rubner-Institute, Kiel'
                authors(4) = '    (Further development and maintenance of the Github repository)'
                authors(5) = 'Marc-Oliver Aust, Thünen Institute of Fisheries Ecology, Hamburg'
                authors(6) = '    (User consulting, support of the project site)'
            end if

            logo = gdk_pixbuf_new_from_resource_at_scale("/org/UncertRadio/icons/ur2_symbol.png" // c_null_char, &
                                                        width=30_c_int, height=30_c_int, &
                                                        preserve_aspect_ratio=TRUE, error=c_null_ptr)

            call hl_gtk_about_dialog_show(    &
                name='UncertRadio' // c_null_char, &
                license= 'This program is free software: you can redistribute it and/or modify' // CR &
                // 'it under the terms of the GNU General Public License as published by' // CR &
                // 'the Free Software Foundation, either version 3 of the License, or' // CR &
                // '(at your option) any later version.' // CR // CR &
                // 'This program is distributed in the hope that it will be useful,' // CR &
                // 'but WITHOUT ANY WARRANTY; without even the implied warranty of' // CR &
                // 'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' // CR &
                // 'GNU General Public License for more details.' // CR // CR &
                // 'You should have received a copy of the GNU General Public License' // CR &
                // 'along with this program.  If not, see <https://www.gnu.org/licenses/>' // CR // CR &
                // 'The files and libraries from the following Open Source Products: ' // CR // CR &
                // '   GTK-Fortran' // CR &
                // '   GTK+ 3' // CR &
                // '   Glade' // CR &
                // '   MSYS2' // CR &
                // '   PLplot ' // CR // CR &
                // 'being used when working with UncertRadio and which ' // CR &
                // 'were used for programming it, underly GNU GPL licenses ' &
                // '(see <https://www.gnu.org/licenses/>). ' // CR  // c_null_char, &
                license_type=GTK_LICENSE_GPL_3_0, &
                comments=comment_str, &
                authors=authors, &
                website=url_str, &
                version=trim(UR_version_tag)// CR // trim(UR_git_hash) // c_null_char, &
                logo=logo,      &
                parent=UR_widgets%window1)

        case ('About_Glade')
            logo = gdk_pixbuf_new_from_resource_at_scale("/org/UncertRadio/icons/glade.png" // c_null_char, &
                                                        width=30_c_int, height=30_c_int, &
                                                        preserve_aspect_ratio=TRUE, error=c_null_ptr)
            call hl_gtk_about_dialog_show(    &
                name='Glade Interface Designer'//c_null_char, &
                license_type=GTK_LICENSE_GPL_2_0_ONLY, &
                comments='A user interface designer for GTK+ and GNOME.'//c_null_char, &
                website='https://gitlab.gnome.org/GNOME/glade'//c_null_char, &
                website_label='Homepage'//c_null_char, &
                version='3.40.0'//c_null_char, &
                logo=logo, &
                parent=UR_widgets%window1)

        case ('About_LAPACK')

            logo = gdk_pixbuf_new_from_resource_at_scale("/org/UncertRadio/icons/lapack.png" // c_null_char, &
                                                        width=30_c_int, height=30_c_int, &
                                                        preserve_aspect_ratio=TRUE, error=c_null_ptr)
            call hl_gtk_about_dialog_show(    &
                name='LAPACK - Linear Algebra PACKage'//c_null_char, &
            ! license_type=GTK_LICENSE_BSD_3, &
                license='modified BSD license' // CR // &
                '(see https://raw.githubusercontent.com/Reference-LAPACK/lapack/refs/heads/master/LICENSE )'//c_null_char, &
                comments='LAPACK is a library of Fortran subroutines for solving the most commonly occurring problems in numerical linear algebra.'//c_null_char, &
                website='https://www.netlib.org/lapack'//c_null_char, &
                website_label='Homepage'//c_null_char, &
                version='3.12.0'//c_null_char, &
                logo=logo, &
                parent=UR_widgets%window1)

        case ('About_FParser')
            call hl_gtk_about_dialog_show(    &
                name='FParser'//c_null_char, &
                copyright='Copyright (c) 2000-2008, Roland Schmehl. All rights reserved.'//c_null_char, &
                license_type=GTK_LICENSE_BSD_3, &
                comments='Fortran 95 function parser'//c_null_char, &
                website='https://fparser.sourceforge.net/'//c_null_char, &
                website_label='Homepage'//c_null_char, &
                version='1.0'//c_null_char, &
                parent=UR_widgets%window1)

        case ('About_PLPLOT')
            call hl_gtk_about_dialog_show(    &
                name='PLplot'//c_null_char, &
                license_type=GTK_LICENSE_GPL_3_0, &
                comments='Cross-platform Plotting Library, with Fortran interface.'//c_null_char, &
                website_label='Homepage'//c_null_char, &
                website='http://plplot.sourceforge.net/'//c_null_char, &
                version='5.15.0'//c_null_char, &
                parent=UR_widgets%window1)

        case ('About_GTK_Fortran')
            call hl_gtk_about_dialog_gtk_fortran()

        case ('About_GTK')

            logo = gdk_pixbuf_new_from_resource_at_scale("/org/UncertRadio/icons/gtk-logo.png" // c_null_char, &
                                                        width=30_c_int, height=30_c_int, &
                                                        preserve_aspect_ratio=TRUE, error=c_null_ptr)
            write(versgtk,'(i0,a1,i0,a1,i0)') gtk_get_major_version(),'.', gtk_get_minor_version(),'.', &
                gtk_get_micro_version()
            call hl_gtk_about_dialog_show(    &
                name='GTK+ Project'//c_null_char, &
                license_type=GTK_LICENSE_LGPL_2_1, &
                comments='GTK+, or the GIMP Toolkit, is a multi-platform toolkit for ' // CR &
                // 'creating graphical user interfaces. Offering a complete set ' // CR &
                // 'of widgets, GTK+ is suitable for projects ranging from small ' // CR &
                // 'one-off tools to complete application suites.'//  CR // CR  &
                // 'GTK+ is a free software cross-platform graphical library ' // CR &
                // 'available for Linux, Unix, Windows and MacOs X.' // c_null_char, &
                website='https://www.gtk.org'//c_null_char, &
                website_label='Homepage'//c_null_char, &
                version=trim(versgtk)//c_null_char, &
                logo=logo, &
                parent=UR_widgets%window1)

        case ('About_MSYS2')
            logo = gdk_pixbuf_new_from_resource_at_scale("/org/UncertRadio/icons/msys2logo.png" // c_null_char, &
                                                        width=30_c_int, height=30_c_int, &
                                                        preserve_aspect_ratio=TRUE, error=c_null_ptr)
            call hl_gtk_about_dialog_show(    &
                name='MSYS2'//c_null_char, &
            ! license='The licenses of those tools apply, which are installed by MSYS2' // c_null_char, &
                license_type=GTK_LICENSE_GPL_3_0, &
                comments='MSYS2 is a software platform with the aim of better interoperability ' &
                // 'with native Windows software.' // CR // CR &
                // 'Actual Windows compatible versions of the gfortran compiler, the' // CR &
                // ' GTK3 library and the Glade Interface Designer ' &
                // 'are available as MSYS2 download packages. ' // c_null_char, &
                website='https://www.msys2.org/wiki/Home/'//c_null_char, &
                website_label='Homepage'//c_null_char, &
                logo=logo, &
                parent=UR_widgets%window1)
        end select
    end subroutine on_show_about_windows
     !---------------------------------------------------------------------------------------------!

    subroutine on_show_monitor_info(widget, data0) bind(c)

        use Rout, only: get_gladeid_name, MessageShow
        use gtk, only: gtk_widget_destroy, gtk_main_quit, &
                       gtk_window_get_position, &
                       GTK_BUTTONS_OK, GTK_MESSAGE_INFO

        use gdk, only: gdk_screen_get_monitor_at_point
        use UR_gtk_globals, only: UR_widgets, gscreen, &
                                  scrwidth_min, scrwidth_max, &
                                  scrheight_min, scrheight_max
        use file_io, only: logger

        implicit none
        type(c_ptr), value, intent(in) :: widget, data0

        character(128)                 :: msg_string
        integer(c_int), target         :: rtx, rty
        integer(c_int)                 :: cmoni, resp
        !-----------------------------------------------------------------------------------------!

        call gtk_window_get_position(UR_widgets%window1, c_loc(rtx),c_loc(rty))


        cmoni = gdk_screen_get_monitor_at_point(gscreen,rtx+10_c_int, rty+10_c_int)
        write(msg_string,'(a,i0,a1,a,i0,a,i0,a1,a,i0,a,i0)') ' Monitor#= ',cmoni+1_c_int,char(13), &
            ' width : ',scrwidth_min,' - ',scrwidth_max,char(13), &
            ' height: ',scrheight_min,' - ',scrheight_max
        call logger(65, msg_string)
        call MessageShow('  '//trim(msg_string)//'  ', GTK_BUTTONS_OK, &
                         "Monitor#:", resp, mtype=GTK_MESSAGE_INFO, parent=UR_widgets%window1)

    end subroutine on_show_monitor_info
    !---------------------------------------------------------------------------------------------!


    recursive function UR_keyPress_cb(widget, event, gdata) result(ret) bind(c)

        ! a small callback routine for keypress events (a few only)

        use, intrinsic :: iso_c_binding,      only: c_null_char,c_ptr,c_int,c_int16_t,c_int16_t
        use gdk,                only: gdk_event_get_keyval
        use gdk_events,         only: GdkEventKey
        use gtk,                only: gtk_widget_grab_focus,gtk_main_do_event, TRUE, FALSE, &
                                      gtk_tree_view_get_cursor
        use gtk_hl,             only: hl_gtk_listn_get_selections, hl_gtk_listn_get_cell
        use g,                  only: g_object_get_data

        use Rout,               only: WTreeViewSetCursorCell
        use top,                only: idpt, FindItemP
        use file_io,            only: logger


        implicit none

        type(c_ptr), value     :: widget, gdata, event
        integer(c_int)         :: ret
        type(c_ptr)            :: pcol
        integer(c_int),pointer :: jcol1
        integer                :: ncitem
        integer                :: akey, allowdkeys(6)
        character(len=512)     :: log_str
        character(len=80)      :: entry
        type(GdkEventKey),pointer   :: event_struct

        ! see file gdkkeysyms.h for keyboard key codes!

        !  65289 : TAB;   65293: Return;          escape: 65307
        !  65361 : left arrow;  65362 : up arrow;  65363 : right arrow; 65364: down arrow
!         allowdkeys = [ 65289, 65293, 65361, 65362, 65363, 65364 ]
!         ret = False
!         call c_f_pointer(event, event_struct)
!         akey = event_struct%keyval
!         call FindItemP(widget, ncitem)

!         !----------------------------------------------------------------------
!         if(akey == 65307) then    ! ESC key
! !         if(ncitem > 0) write(66,*) 'ESC given by: ncitem=',ncitem,' idd=',clobj%idd(ncitem)%s
!             if(ncitem > 0)  then
!                 write(log_str, '(*(g0))') 'ESC given by: ncitem=',ncitem,' idd=',clobj%idd(ncitem)%s
!                 call logger(66, log_str)
!             end if
!             ret = FALSE
!             if(clobj%name(ncitem)%s == 'GtkDialog') ret = TRUE
!             ! with ret=TRUE, dialog-destroy is prevented for ESC !!!
!             ! therefore, the callback should be a function !!!!!!
!             return
!         end if
!         !----------------------------------------------------------------------
!         if(Findloc(allowdkeys, akey,dim=1) == 0) return

!         if(ncitem == 0) return

!         if(akey == 65293) then
!             ret = FALSE
!             return
!         end if
!         entry = clobj%idd(ncitem)%s
!         if(trim(entry) == 'TRentryMCanzM') then
!             call gtk_widget_grab_focus(idpt('TRentryMCanzR'))
!             ret = TRUE
!             return
!         end if
!         if(trim(entry) == 'TRentryMCanzM1') then
!             call gtk_widget_grab_focus(idpt('TRentryMCanzR1'))
!             ret = TRUE
!             return
!         end if
!         if(.true. .and. index(entry,'treeview') > 0 .and. akey >= 65361 .and. akey <= 65364) then
!             ! count = hl_gtk_listn_get_selections(widget, indices)

!             pcol = g_object_get_data(widget,"column-number"//c_null_char)
!             call c_f_pointer(pcol,jcol1)

!             return
!         end if

    end function UR_keyPress_cb

    !---------------------------------------------------------------------------------------------

    subroutine ProjectOpen_cb(widget, gdata) bind(c)   ! result(ret)

        ! this routine receives the request for opening a project file and forwards finally
        ! to ProcessLoadPro_new for really doing the steps necessary for opening the file
        ! and loading it into the GUI.
        ! In between these two steps, it checks, and gives the chance to it, whether the
        ! project shall be saved before executing a closing step.

        use, intrinsic :: iso_c_binding,    only: c_null_char,c_ptr,c_int
        use gtk,                only: GTK_BUTTONS_YES_NO,gtk_response_yes,GTK_MESSAGE_WARNING

        use gtk_sup, only: c_f_string
        use UR_gtk_globals,     only: Quitprog,dialog_on
        use ur_general_globals, only: FileTyp, fname, Savep
        use UR_interfaces,      only: ProcessLoadPro_new
        use UR_Gleich_globals,  only: ifehl
        use file_io,            only: logger
        use Rout,               only: MessageShow, fopen, get_gladeid_name
        use Top,                only: FieldUpdate
        use translation_module, only: T => get_translation
        use UR_gtk_window_types, only: widget_type

        implicit none

        type(c_ptr), value    :: widget, gdata
        integer(c_int)        :: resp     ! ret

        integer               :: ncitem, status
        character(len=60)     :: title,cheader
        character(len=512)    :: log_str
        character(len=120)    :: str1
        character(:), allocatable :: idstring, error
        type(widget_type), pointer :: UR_widget
        !----------------------------------------------------------------------------
        idstring = get_gladeid_name(widget, error)
        if(dialog_on .and. idstring /= 'TBRemoveGridLine') return

        call c_f_pointer(gdata, UR_widget)

        ! print *, idstring, ' data0'
        ! call c_f_string_chars(UR_widget%signal, str1)
        ! print *, 'is this|', trim(str1), '|working??'
        ! call c_f_string_chars(UR_widget%handler, str1)
        ! print *, 'is this|', trim(str1), '|even more working??'
        ! call c_f_string_chars(UR_widget%gladeid, str1)
        ! print *, 'is this|', trim(str1), '|even this even more working??'
        ! call c_f_string_chars(UR_widget%classname, str1)
        ! print *, 'is this|', trim(str1), '|finally even this even more working??'

        FileTyp = 'P'

        IF (Filetyp == 'P' .AND. SAVEP) then
            write(str1,*) T("Shall the open project be saved before closing it?")
            call MessageShow(str1, &
                             GTK_BUTTONS_YES_NO, &
                             T("Closing Project:"), &
                             resp, &
                             mtype=GTK_MESSAGE_WARNING)

            IF (resp == GTK_RESPONSE_YES) then   !
                if(len_trim(fname)== 0) then
                    cheader = 'Choose filename:'
                    call FOpen(ifehl, .true., cheader )
                    if(ifehl == 1) return
                end if
                call ProjectSave_CB(widget, gdata)
            ELSE
                SaveP = .false.         ! 20.9.2024 GK
                call FieldUpdate()
            END IF
        END IF

        ifehl = 0               !  20.9.2024 GK

        title = 'Open project file:'

        call ProcessLoadPro_new(0, 1)

        write(log_str, '(*(g0))') 'ProjectOpen:   after call ProcessLoadPro_new, QuitProg=',QuitProg
        call logger(65, log_str)

    end subroutine ProjectOpen_cb

!#############################################################################################

    subroutine ProjectSave_cb(widget, gdata) bind(c)     ! result(ret)

        ! this routine receives the request for saving a project file and forwards it
        ! to the routie Save.

        use, intrinsic :: iso_c_binding,   only: c_null_char,c_ptr,c_int

        use UR_gtk_globals,     only: dialog_on
        use ur_general_globals, only: saveas, FileTyp
        use top,                only: FindItemP

        implicit none

        type(c_ptr), value    :: widget, gdata

        character(len=60)     :: title, idstring
        integer               :: mode, ncitem
        !----------------------------------------------------------------------------
        ! if(dialog_on) then
        !     call FindItemP(widget, ncitem)
        !     idstring = clobj%idd(ncitem)%s
        !     if(trim(idstring) /= 'TBRemoveGridLine') return
        ! end if

        ! ! mode:  0:  save;   1: save as;
        ! Mode = 0
        ! title = "Save project file:"
        ! saveas = .false.

        ! call FindItemP(widget, ncitem)
        ! if( clobj%idd(ncitem)%s == 'TBSaveProjectAs' .or.   &
        !     clobj%idd(ncitem)%s == 'MenuSaveProjectAs') then
        !     mode = 1
        !     title = "Save project file as:"
        !     saveas = .true.
        ! end if

        ! FileTyp = 'P'

        ! call Save(mode, title)

    end subroutine ProjectSave_cb

    !#############################################################################################

    function UR_tree_text_edit_cb(renderer, path, text) result(ret) bind(c)

        ! this function handles actions for editing a cell or a cell block within
        ! a GTK treeview. It interprets the received variables path (position within the
        ! table) and text (new text input received from the user, not yet stored in the
        ! treeview). For a single cell, it stores the new text, which can be numbers as
        ! strings, in the cell using tree-dependent and column-type-dependent specific
        ! formats frmt*.
        !
        ! It also handles the input (by paste) of a larger cell-block, e.g. copied in from
        ! Excel, and distributes it over the associated numbers of columns and rows. The
        ! number of columns pasted is derived from the number of char(9) or char(10) in it.
        ! If such characters are found, a cell-block is detected and the internal variable
        ! insert_block is set true.
        !


        use gtk,              only: gtk_cell_renderer_toggle_get_active,gtk_widget_set_sensitive,  &
                                    gtk_widget_hide,FALSE, &
                                    gtk_tree_view_column_set_max_width,gtk_tree_view_column_set_expand, &
                                    gtk_tree_selection_select_path, &
                                    gtk_tree_path_to_string

        use gtk_hl,           only: hl_gtk_listn_set_cell, gtk_tree_view_get_model, hl_gtk_listn_get_cell
        use gtk_sup,          only: convert_c_string
        use gdk,              only: gdk_beep
        use UR_gtk_globals,   only: nstores, storename, lsgtype,lstype,item_setintern, &
                                    tv_colwidth_digits,tvnames,ntvs,TVlastCell

        use ur_general_globals,     only: frmt,frmtg,saveP,frmt_min1,frmtc,sDecimalPoint    ! ,clipd
        use UR_Gleich_globals,        only: SDformel,SDFormel_CP,SDwert,SDWert_CP,missingval,ngrs_CP,  &
                                    SDWert_CP,Symbole_CP,Symbole,IVTL,IAR,SymboleA,  &
                                    Messwert,HBreite,StdUnc, ngrs,ngrs_CP,use_DP,charv
        use Rout,             only: WTreeViewSetCursorCell,WTreeViewGetComboArray,WTreeViewGetStrArray, &
                                    WTreeViewGetDoubleArray,ClearMCfields,WTreeViewSetCursorCell
        use Top,              only: FieldUpdate, wrstatusbar, idpt
        use g,                only: g_signal_emitv
        use file_io,          only: logger
        use CHF,              only: FormatNumStr

        implicit none

        type(c_ptr), value                  :: renderer, path, text    ! , gdata

        character(:),allocatable            :: ftextXX
        type(charv)                         :: fpath, ftextcp,gtext,gtextorg
        type(charv)                         :: ftext

        character(len=50),allocatable       :: ftable(:,:)
        integer(kind=c_int), allocatable    :: irow(:)
        integer(kind=c_int)                 :: irow1,icol1,ret
        integer                             :: i, n, k, j, ind_rend, krow,kcol,i1,ios,jp,nc
        integer                             :: krow_1,krow_2,kcol_1,kcol_2,kc,kr,nt
        type(c_ptr)                         :: tree, store
        integer(c_int)                      :: ncol
        character(len=40)                   :: treename,liststore
        integer                             :: ind_list,jcol,jrow,jplast
        real(rn)                            :: dval16
        character(len=10)                   :: coltyp
        character(len=20)                   :: frmtv
        logical                             :: insert_block
        logical                             :: prout

        character(:),allocatable            :: strX
        character(len=512)                  :: log_str
        character(len=40)                   :: cnumb

        !--------------------------------------------------------------------------------------
        ! Default callback for tree cell edited.
        !
        ! RENDERER: c_ptr: required: The renderer which sent the signal
        ! PATH: c_ptr: required: The path at which to insert
        ! TEXT: c_ptr: required: The text to insert
        ! GDATA: c_ptr: required: User data, not used.
        !
        ! The column number is passed via the "column-number" gobject data value.
        ! The treeview containing the cell is passed via the "view" gobject
        ! data value.
        ! The row number is passed as a string in the PATH argument.
        !
        ! This routine is not normally called by the application developer.
        !-

        ! gdata does not seem to have meeningful values
        !----------------------------------------
        ! ret = FALSE
        ! if(item_setintern) return
        ! prout = .false.
        ! ! prout = .true.

        ! jrow = 0
        ! jcol = 0
        ! nc = 0
        ! do i=1,nclobj
        !     if(c_associated(clobj%id_ptr(i), renderer)) then
        !         nc = i
        !         exit
        !     end if
        ! end do

        ! if(prout) write(0,'(a,i0)') 'nc= ',nc


        ! allocate(character(len=10000)::strX)
        ! allocate(ftable(200,5))
        ! allocate(character(len=10000):: ftextXX)

        ! ! if(prout) write(66,*) 'before convert path','  path=',path
        ! ! if(prout)  then
        ! !     write(log_str, '(*(g0))') 'before convert path','  path=',path
        ! !     call logger(66, log_str)
        ! ! end if
        ! call convert_c_string(path,ftextXX)
        ! fpath%s = trim(ftextXX)

        ! ! if(prout) write(66,*) 'CB on entry: fpath = ',trim(fpath%s),'   path=',path
        ! ! if(prout)  then
        ! !     write(log_str, '(*(g0))') 'CB on entry: fpath = ',trim(fpath%s),'   path=',path
        ! !     call logger(66, log_str)
        ! ! end if
        ! call convert_c_string(text, ftextXX)
        ! ftext%s = trim(ftextXX)
        ! !     if(prout) write(66,*) 'ftext%s=',trim(ftext%s)
        ! if(prout)  then
        !     write(log_str, '(*(g0))') 'ftext%s=',trim(ftext%s)
        !     call logger(66, log_str)
        ! end if

        ! ! Important note:
        ! ! If the TreeView has only 2 columns, like e.g. treeview8,
        ! ! the path-string consists of only 1 number, the row number; the path-string does noct contain ":"

        ! insert_block = .false.
        ! if(index(ftext%s,char(9)) > 0 .OR. index(ftext%s,char(10)) > 0 ) then
        !     ! try to insert a column block from clipboard input:
        !     ftable = ' '
        !     jrow = 0
        !     jp = 1
        !     jplast = 1
        !     do i=1,len_trim(ftext%s)
        !         if(ftext%s(i:i) == char(10)) then
        !             jrow = jrow + 1
        !             jcol = 0
        !             do j=jp,i-1
        !                 if(ftext%s(j:j) == char(9)) then
        !                     jcol = jcol + 1
        !                     ftable(jrow,jcol) = ftext%s(jplast:j-1)
        !                     jplast = j+1
        !                 end if
        !             end do
        !             jcol = jcol + 1
        !             ftable(jrow,jcol) = ftext%s(jplast:i-1)
        !             if(prout) then

        !                 write(log_str, '(2(a,i0),a,a)') 'jcol=',jcol,' jplast=',jplast,' ftable=',trim(ftext%s(jplast:j-1))
        !                 call logger(66, log_str)

        !                 write(log_str, '(2(a,i0),a,20a)') 'fext: line=',jrow,' # of tabs=',jcol,'  columns: ',(ftable(jrow, k)(1:15),' : ',k=1,jcol)
        !                 call logger(66, log_str)
        !             end if
        !             jp = i+1
        !             jplast = jp
        !         end if
        !     end do
        !     insert_block = .true.
        ! end if

        ! if(prout)  then
        !     write(log_str, '(*(g0))') 'insert_block =',insert_block
        !     call logger(66, log_str)
        ! end if
        ! if(prout) write(0,*) 'insert_block =',insert_block

        ! n = 0
        ! do i = 1, len_trim(fpath%s)
        !     if (fpath%s(i:i) == ":") then
        !         n = n+1
        !         fpath%s(i:i) = ' '   ! : is not a separator for a Fortran read
        !     end if
        ! end do
        ! allocate(irow(n+1))
        ! read(fpath%s, *) irow
        ! read(fpath%s, *) irow1
        ! read(fpath%s, *) krow
        ! krow = krow + 1

        ! ncol = 0
        ! nt = 0   ! 2025.01.23 GK
        ! treename = ''
        ! liststore = ''
        ! do i=1,nclobj
        !     if(c_associated(clobj%id_ptr(i), renderer)) then
        !         ind_rend = i
        !         nt = 0
        !         do k=i,1,-1
        !             if(clobj%name(k)%s == trim('GtkTreeView')) then
        !                 tree = clobj%id_ptr(k)
        !                 treename = clobj%idd(k)%s
        !                 do j=1,ntvs
        !                     if(trim(treename) == tvnames(j)%s) nt = j
        !                 end do
        !                 exit
        !             end if
        !         end do
        !         ncol = 0
        !         do j=k,ind_rend
        !             if(index(clobj%idd(j)%s, 'treeviewcol') > 0) ncol = ncol + 1
        !         end do
        !         exit
        !     end if
        ! end do
        ! kcol = ncol
        ! ncol = ncol - 1
        ! icol1 = ncol

        ! ! Get list store
        ! store = gtk_tree_view_get_model(tree)

        ! ! Find the type for the requested column
        ! ! ctype = gtk_tree_model_get_column_type(store, icol1)

        ! do i=1,nclobj
        !     if(c_associated(clobj%id_ptr(i), store)) then
        !         liststore = clobj%idd(i)%s
        !         exit
        !     end if
        ! end do
        ! ind_list = 0
        ! do i=1,nstores
        !     if(storename(i)%s == trim(liststore)) then
        !         ind_list = i
        !         exit
        !     end if
        ! end do
        ! if(prout) then

        !     ! write(log_str, '(*(g0))') 'CB-text angekommen: tree=',tree,'  treename=',trim(treename),' fpath=',trim(fpath%s),'   ftext=',trim(ftext%s)
        !     ! call logger(66, log_str)

        !     write(log_str, '(*(g0))') '       kcol=',kcol,'  krow=',krow,'  ind_list=',ind_list
        !     call logger(66, log_str)

        !     write(log_str, '(*(g0))') '       renderer=',clobj%idd(ind_rend)%s
        !     call logger(66, log_str)
        !     write(log_str, '(*(g0))') '       liststore=',trim(liststore),'   lsgtype=',lsgtype(ind_list,kcol)%s, &
        !         '   lstype=',lstype(ind_list,kcol),'  ind_list=',ind_list,' krow=',kcol
        !     call logger(66, log_str)
        ! end if

        ! !!! Important:  hl_gtk_listn_set_cell   requires a treeview pointer as 'List'-Parameter
        ! krow_1 = krow
        ! krow_2 = krow
        ! kcol_1 = kcol
        ! kcol_2 = kcol
        ! if(insert_block) then
        !     krow_1 = krow
        !     krow_2 = krow + jrow - 1
        !     kcol_1 = kcol
        !     kcol_2 = kcol + jcol - 1
        ! end if
        ! if(prout) then

        !     write(log_str, '(*(g0))') 'insert_block=',insert_block,'  kcol_1, kcol_2=',kcol_1, kcol_2,'  krow_1, krow_2=',krow_1, krow_2, &
        !         '  ftext=',trim(ftext%s)
        !     call logger(66, log_str)
        ! end if

        ! do kr=krow_1,krow_2
        !     irow1 = kr - 1
        !     krow = kr

        !     if(trim(treename) == 'treeview2' .and. kr > ubound(Symbole,dim=1)) exit
        !     if(trim(treename) == 'treeview3' .and. kr > ubound(SymboleA,dim=1)) exit

        !     do kc=kcol_1,Kcol_2
        !         icol1 = kc - 1
        !         kcol = kc
        !         if(.not.insert_block) gtext%s = trim(ftext%s)
        !         if(insert_block) gtext%s = ftable(kr-krow_1+1, kc-kcol_1+1)
        !         select case (lstype(ind_list,kcol))
        !           case (1, 2)
        !             select case (trim(lsgtype(ind_list,kcol)%s))

        !                 ! Note: from icol1 given here, -1 is already subtracted
        !               case ('gchararray')
        !                 coltyp = 'text'
        !                 if(trim(treename) == 'treeview2' .and. (icol1 == 4 .or. icol1 == 7 .or. icol1 == 8  &
        !                     .or. icol1 == 10)) then
        !                     coltyp = 'double'
        !                     frmtv = frmt
        !                 end if
        !                 if(trim(treename) == 'treeview3' .and. icol1 == 5) then
        !                     coltyp = 'double'                 ! covar-grid
        !                     frmtv = frmtc
        !                 end if
        !                 if(trim(treename) == 'treeview4' .and. icol1 > 4) then
        !                     coltyp = 'double'                  ! budget-grid
        !                     frmtv = frmt
        !                 end if
        !                 if(trim(treename) == 'treeview5' .and. icol1 > 1) then
        !                     coltyp = 'double'                  ! deacy-data grid
        !                     frmtv = frmt
        !                 end if
        !                 if(trim(treename) == 'treeview6' .and. icol1 > 1) then
        !                     coltyp = 'double'                  ! Gskp1- grid
        !                     frmtv = frmtg
        !                 end if
        !                 if(trim(treename) == 'treeview7' .and. icol1 > 1) then
        !                     coltyp = 'double'                  ! KalFit- grid
        !                     frmtv = frmtg
        !                 end if
        !                 if(trim(treename) == 'treeview8' .and. icol1 > 1) then
        !                     coltyp = 'double'                  ! data-mean grid
        !                     frmtv = frmtg
        !                 end if

        !                 select case (trim(coltyp))
        !                   case ('text')
        !                     ! call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=gtextorg%s)
        !                     call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=strX)
        !                     gtextorg%s = trim(strX)
        !                     if(trim(gtext%s) /= trim(gtextorg%s)) then
        !                         call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=trim(gtext%s))
        !                         call modify_grid_value(trim(treename), krow,kcol,gtext%s)    ! 9.12.2024 GK
        !                         SaveP = .true.
        !                         call FieldUpdate()
        !                         if(trim(treename) == 'treeview2') then
        !                             call clearMCfields(1)
        !                             call gtk_widget_hide(idpt('window_graphs'))
        !                         end if
        !                     end if

        !                     if(trim(treename) == 'treeview2' .and. icol1 == 6) then
        !                         ! If a SD formula is deleted, the associated array elements  SDFormel and SDWert
        !                         ! must also be deleted.
        !                         if(len_trim(SDformel(krow)%s) > 0 .and. len_trim(gtext%s) == 0) then
        !                             call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1+1,  svalue=' ')
        !                         end if
        !                         SDformel(krow)%s = trim(gtext%s)
        !                         SDformel_CP(krow)%s = trim(gtext%s)
        !                         SDWert(krow) = missingval
        !                         do j=1,ngrs_CP
        !                             IF(TRIM(lowcase(symbole(krow)%s)) == TRIM(lowcase(symbole_CP(j)%s)) ) THEN
        !                                 SDWert_CP(j) = missingval
        !                                 exit
        !                             end if
        !                         end do
        !                     end if

        !                   case ('double')
        !                     ftextcp%s = gtext%s
        !                     i1 = index(ftextcp%s,',')
        !                     if(i1 > 0) then
        !                         ftextcp%s(i1:i1) = '.'
        !                     end if
        !                     read(ftextcp%s,*,iostat=ios) dval16
        !                     if(ios == 0) then
        !                         if(trim(frmtv) == trim(frmt) .and. dval16 < 0.10_rn) frmtv = frmt_min1
        !                         ! write(gtext%s,frmtv) real(dval16,8)
        !                         write(cnumb,frmtv) real(dval16,8)       ! 16.8.2023
        !                         gtext%s = FormatNumStr(trim(cnumb), sDecimalPoint)
        !                         if(nt > 0) then
        !                             tv_colwidth_digits(nt,icol1+1) = &
        !                                 max(tv_colwidth_digits(nt,icol1+1), len_trim(gtext%s))
        !                         end if
        !                         call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=gtext%s)
        !                         call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=gtext%s)
        !                         call modify_grid_value(trim(treename), krow,kcol,gtext%s)    ! 9.12.2024 GK
        !                         SaveP = .true.
        !                         call FieldUpdate()
        !                         if(trim(treename) == 'treeview2') then
        !                             call clearMCfields(1)
        !                             call gtk_widget_hide(idpt('window_graphs'))
        !                         end if
        !                     else
        !                         if(len_trim(ftextcp%s) == 0) then
        !                             call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=ftextcp%s)
        !                             call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=ftextcp%s)
        !                             call modify_grid_value(trim(treename), krow,kcol,ftextcp%s)    ! 9.12.2024 GK
        !                             SaveP = .true.
        !                             call FieldUpdate()
        !                             if(trim(treename) == 'treeview2') then
        !                                 call clearMCfields(1)
        !                                 call gtk_widget_hide(idpt('window_graphs'))
        !                             end if
        !                         else
        !                             call WTreeViewSetCursorCell(trim(treename), kcol, krow)
        !                             call gdk_beep()
        !                         end if
        !                     end if
        !                 end select

        !               case ('')

        !             end select
        !         end select
        !         TVlastCell = [nt, krow, kcol]
        !     end do
        ! end do

        ! if(trim(treename) == 'treeview2' .and. SaveP) then
        !     call WTreeviewGetComboArray('treeview2',6,ngrs,IVTL)
        !     call WTreeviewGetStrArray('treeview2',7,ngrs,sdformel)
        !     call WTreeviewGetComboArray('treeview2',10,ngrs,IAR)
        !     call WTreeviewGetDoubleArray('treeview2',5,ngrs,Messwert)
        !     call WTreeviewGetDoubleArray('treeview2',8,ngrs,SDWert)
        !     call WTreeviewGetDoubleArray('treeview2',9,ngrs,HBreite)
        !     call WTreeviewGetDoubleArray('treeview2',11,ngrs,StdUnc)

        !     call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
        !     call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
        !     call gtk_widget_hide(idpt('box5'))
        !     call gtk_widget_hide(idpt('grid5'))
        ! end if

        ! deallocate(irow)

        ! if(trim(treename) == 'treeview2') then
        !     if(.not.use_DP) then
        !         call WTreeviewGetComboArray('treeview2',6,ngrs,IVTL)
        !         if(maxval(IVTL) > 8) then
        !             use_DP = .true.
        !             call gtk_widget_set_sensitive(idpt('TBDistribDialog'), 1_c_int)
        !         end if
        !     end if

        !     ! write(chcol,'(i3)') tvcolindex(nt,5)
        !     ! cwidth = 12*15
        !     ! call gtk_tree_view_column_set_max_width(idpt('treeviewcolumn' // trim(adjustL(chcol))),cwidth )
        !     ! call gtk_tree_view_column_set_expand(idpt('treeviewcolumn' // trim(adjustL(chcol))), 1_c_int)
        ! end if

        ! if(.true. .and. trim(treename) == 'treeview8') then
        !     if(len_trim(gtext%s) > 0) then
        !         call WTreeViewSetCursorCell(trim(treename),kcol,krow+1,.true.)
        !     else
        !         call WTreeViewSetCursorCell(trim(treename),kcol,krow+0,.false.)
        !     end if
        ! end if

        ! if(trim(treename) == 'treeview1' .and. SaveP) then
        !     call gtk_widget_set_sensitive(idpt('AcceptAll'), 0_c_int)
        !     call gtk_widget_set_sensitive(idpt('NBValUnc'), 0_c_int)
        !     call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
        !     call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
        ! end if

        ! if(allocated(ftext%s)) deallocate(ftext%s)
        ! if(allocated(ftable)) deallocate(ftable)

        ! ret = False

    end function UR_tree_text_edit_cb

    !#############################################################################################

    subroutine modify_grid_value(treename, krow, kcol, newvalstr)

       ! This subroutine used by UR_tree_text_edit_cb copies the modified cell value of the
       ! actual treeview (with name treename) to the corresponding array element.
       ! GK  9.12.2024

        use UR_Gleich_globals, only: missingval, ISymbA, ISymbB, &
                                     CVFormel, icovtyp, covarval, XDataMD
        use UR_Linft,          only: CStartzeit, dmesszeit, dbimpulse, &
                                     dbzrate, sdbzrate, d0messzeit, d0impulse, &
                                     d0zrate, sd0zrate, dnetrate, sdnetrate, &
                                     xkalib, uxkalib, ykalib, uykalib
        use UR_Gspk1Fit,       only: guse, erg, GNetRate, RateCB, RateBG, &
                                     SDRateBG, effi, SDeffi, pgamm, sdpgamm, &
                                     fatt, sdfatt, fcoinsu, sdfcoinsu

        implicit none

        character(len=*),intent(in)    :: treename          ! name of the actual treeview (grid))
        integer(4),intent(in)          :: krow, kcol         ! col and row numbers of the modeified grid cell
        character(len=*),intent(in)    :: newvalstr         ! new value given as string

        character(len=100)     :: nvstr

        ! ! Replace an emptzy string by the string value "missingval":
        ! nvstr = newvalstr
        ! if(len_trim(nvstr) == 0) write(nvstr,*) missingval

        ! select case (trim(treename))

        ! case ('treeview3')
        !     if(kcol == 2) read(nvstr,*) ISymbA(krow)
        !     if(kcol == 3) read(nvstr,*) ISymbB(krow)
        !     if(kcol == 4) read(nvstr,*) icovtyp(krow)
        !     if(kcol == 5) CVFormel(krow)%s = trim(nvstr)
        !     if(kcol == 6) read(nvstr,*) covarval(krow)

        ! case ('treeview5')
        !     if(kcol == 2) CStartzeit(krow)%s = trim(nvstr)
        !     if(kcol == 3) read(nvstr,*) dmesszeit(krow)
        !     if(kcol == 4) read(nvstr,*) dbimpulse(krow)
        !     if(kcol == 5) read(nvstr,*) dbzrate(krow)
        !     if(kcol == 6) read(nvstr,*) sdbzrate(krow)
        !     if(kcol == 7) read(nvstr,*) d0messzeit(krow)
        !     if(kcol == 8) read(nvstr,*) d0impulse(krow)
        !     if(kcol == 9) read(nvstr,*) d0zrate(krow)
        !     if(kcol == 10) read(nvstr,*) sd0zrate(krow)
        !     if(kcol == 11) read(nvstr,*) dnetrate(krow)
        !     if(kcol == 12) read(nvstr,*) sdnetrate(krow)

        ! case ('treeview6')
        !     if(kcol == 2) read(nvstr,*) guse(krow)
        !     if(kcol == 3) read(nvstr,*) erg(krow)
        !     if(kcol == 4) read(nvstr,*) GNetRate(krow)
        !     if(kcol == 5) read(nvstr,*) RateCB(krow)
        !     if(kcol == 6) read(nvstr,*) RateBG(krow)
        !     if(kcol == 7) read(nvstr,*) SDRateBG(krow)
        !     if(kcol == 8) read(nvstr,*) effi(krow)
        !     if(kcol == 9) read(nvstr,*) SDeffi(krow)
        !     if(kcol == 10) read(nvstr,*) pgamm(krow)
        !     if(kcol == 11) read(nvstr,*) SDpgamm(krow)
        !     if(kcol == 12) read(nvstr,*) fatt(krow)
        !     if(kcol == 13) read(nvstr,*) SDfatt(krow)
        !     if(kcol == 14) read(nvstr,*) fcoinsu(krow)
        !     if(kcol == 15) read(nvstr,*) SDfcoinsu(krow)

        ! case ('treeview7')
        !     if(kcol == 2) read(nvstr,*) xkalib(krow)
        !     if(kcol == 3) read(nvstr,*) uxkalib(krow)
        !     if(kcol == 4) read(nvstr,*) ykalib(krow)
        !     if(kcol == 5) read(nvstr,*) uykalib(krow)

        ! case ('treeview8')
        !     if(kcol == 2) read(nvstr,*) XDataMD(krow)

        ! end select

    end subroutine modify_grid_value

    !#############################################################################################

    recursive subroutine UR_field_edit_cb(renderer, path, text)  bind(c)

        ! this function identifies the widget (field renderer) by its idstring (a name)
        ! and dependent on it performs the necessary actions.

        use gtk_hl,             only: hl_gtk_listn_set_cell,gtk_tree_view_get_model, hl_gtk_listn_get_cell, &
                                      hl_gtk_combo_box_get_active
        use UR_gtk_globals,     only: FieldEditCB, list_filling_on, item_setintern
        use gtk,                only: gtk_widget_set_sensitive, gtk_widget_hide, &
                                      gtk_widget_set_state_flags, GTK_STATE_FLAG_NORMAL, gtk_notebook_set_current_page, &
                                      gtk_buildable_get_name
        use gtk_sup,            only: c_f_string, c_f_pointer

        use ur_general_globals, only: saveP,Gum_restricted,gross_negative,kModelType
        use UR_Gleich_globals,  only: syntax_check,dialogfield_chg, kEGr,knetto, kbrutto, &
                                      knumEGr, knumold
        use UR_Linft,           only: FitDecay, dmodif
        use Top,                only: FieldUpdate, idpt, FindItemP

        use file_io,            only: logger
        use Rout,               only: WDPutLabelString
        use translation_module, only: T => get_translation

        implicit none

        type(c_ptr), value, intent(in) :: renderer, path, text
        integer(kind=c_int), pointer :: fdata

        character(:),allocatable     :: str1
        integer(kind=c_int)          :: indx
        integer                      :: nind
        character(len=32)            :: idstring, signal, dparent
        character(len=512)           :: log_str
        CHARACTER(LEN=1)             :: cnu
        type(c_ptr)                  :: ctext
        !------------------------------------------------------------------------------------
        ! When using GTK+ directly, keep in mind that only functions can be connected to signals, not methods.
        ! So you will need to use global functions or "static" class functions for signal connections.

        allocate(character(len=50)  ::str1)

        if(list_filling_on) return
        if(item_setintern) then
            return
        end if

        ctext = gtk_buildable_get_name(renderer)

        if(c_associated(ctext)) then
            call c_f_string(ctext, idstring)
            signal   = 'none' !clobj%signal(ncitem)%s
            dparent  = 'none' !clobj%idd(clobj%idparent(ncitem))%s

            if(trim(idstring) == 'LoadWithCalc') goto 10
            if(trim(idstring) == 'LoadWithoutCalc') goto 10
            if(trim(idstring) == 'textview2') goto 10
            if(trim(idstring) == 'MT_PosLin') goto 10
            if(trim(idstring) == 'MT_GUMonly') goto 10
            if(trim(idstring) == 'MT_NegLin') goto 10

            if(trim(idstring) == 'entryOptAlpha') goto 10     ! 13.4.2023
            if(trim(idstring) == 'entryOptBeta') goto 10      !
            if(trim(idstring) == 'entryOptKalpha') goto 10    !
            if(trim(idstring) == 'entryOptKbeta') goto 10     !
            if(trim(idstring) == 'entryOptKbeta') goto 10     !

            ! GK: 17.11.2024:
            ! The preceding conditional return was replaced by the following if-construct
            ! and unconditial return.
            ! This refers to signals of widgets, which shall not be be further processed:
            ! the vanish silently. It is recognized only that they may change the project,
            ! which then needs to be saved.
            ! Widget signals which are to be processed further have to use the "goto 10" above.
            print *, '####### else, signal ? edit_cb', idstring
            if(trim(signal) == 'group_changed' .or. trim(signal) == 'toggled' .or. &
               trim(signal) == 'changed' ) then
              if(trim(idstring) /= 'notebook1') then
                SaveP = .true.
                call FieldUpdate()
              end if
            end if
            return

        else

            call logger(66, '****** UR_field_edit_cb :  non-associated widget:')
            return
        end if
10      continue
        ! write(66,*) '*** Field_edit:  item=',ncitem ,'   ',trim(idstring),'  ',trim(signal) ! (ncitem)   ! ,'  path=',path,'   text=',text

        if(trim(idstring) == 'MT_PosLin') then
            Gum_restricted = .false.
            gross_negative = .false.
            kModelType = 1
            goto 100
        end if
        if(trim(idstring) == 'MT_GUMonly') then
            Gum_restricted = .true.
            gross_negative = .false.
            kModelType = 2
            goto 100
        end if
        if(trim(idstring) == 'MT_NegLin') then
            Gum_restricted = .false.
            gross_negative = .true.
            kModelType = 3
            goto 100
        end if

        if( (trim(idstring) == 'textview2'.or. trim(idstring) == 'textbufferEQ' )   &
            .and. .not.syntax_check) then
            syntax_check = .true.
            dialogfield_chg = 'Equations_main'
        elseif( trim(idstring) == 'textviewModelEQ' .and. .not.syntax_check) then
            syntax_check = .true.
            dialogfield_chg = 'Equations_fit'
            SaveP = .true.
            call FieldUpdate()
            return
        end if

        if( (trim(idstring) == 'comboboxNetRate' .or. trim(idstring) == 'comboboxGrossRate') .and.    &
            kEGr > 0) then
            indx = hl_gtk_combo_box_get_active(renderer)
            nind = indx + 1

            if(nind > 0 .and. .not. FitDecay) then
                if( (trim(idstring) == 'comboboxNetRate' .and. nind == knetto(kEGr))   .or. &
                    (trim(idstring) == 'comboboxGrossRate' .and. nind == kbrutto(kEGr) ) ) then
                    return
                end if
                if( (trim(idstring) == 'comboboxNetRate' .and. nind /= knetto(kEGr))   .or. &
                    (trim(idstring) == 'comboboxGrossRate' .and. nind /= kbrutto(kEGr) ) ) then
                    if(nind > 0)  then
                        write(log_str, '(*(g0))') 'NBvalUnc deactivated:   idstring=',trim(idstring),  &
                            '   signal=',trim(signal),' nind=',nind,'  knetto(kEGr)=',  &
                            knetto(kEGr)! ,' render=',renderer
                        call logger(66, log_str)
                    end if


                    call gtk_widget_set_sensitive(idpt('NBValUnc'), 0_c_int)
                    call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
                    call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
                    call gtk_widget_set_sensitive(idpt('TBRefreshCalc'), 0_c_int)

                    call gtk_widget_set_sensitive(idpt('box4'), 0_c_int)
                    call gtk_widget_set_sensitive(idpt('box5'), 0_c_int)
                    call gtk_widget_set_sensitive(idpt('grid5'), 0_c_int)
                    call gtk_widget_hide(idpt('box4'))
                    call gtk_widget_hide(idpt('box5'))
                    call gtk_widget_hide(idpt('grid5'))
                end if
            end if
        end if

        if(trim(idstring) == 'comboboxtextKnumegr') then
            knumold = knumEGr
            indx = hl_gtk_combo_box_get_active(idpt('comboboxtextKnumegr'),ctext, cnu)
            read(cnu,*) knumEGr
            IF(knumold /= KnumEGr .and. knumold /= 0) then
                WRITE(cnu,'(i1)') knumEGr
                IF(KnumEGr > knumold) THEN
                    str1 = T("Please insert the main equation for the") //" " // cnu // ". " // &
                           T("output quantity in the formula textfield") // ","//  char(13) // &
                           T("also equations for further auxiliary quantities!")

                end if
                if(KnumEGr < knumold) then

                    str1 = T('Please delete the lines belonging to the deleted result variable in the formula text field')
                end if
                call WDPutLabelString('LBnumegrWarn',trim(str1))
            end if
        end if

100     continue

        ! if(clobj%name(ncitem)%s == 'GtkEntry') then
        !     call gtk_widget_set_state_flags(clobj%id_ptr(ncitem), GTK_STATE_FLAG_NORMAL, 1_c_int)
        ! end if

        ! ret = True
        FieldEditCB = .true.
        if(trim(dparent) == 'dialogDecayModel') then  ! 30.1.2024
            dmodif = .true.
            FieldEditCB = .false.
        end if

        !if(trim(idstring) == 'comboboxDKPgrad') FieldEditCB = .true.
        if(trim(idstring) == 'DKcheckWTLS') FieldEditCB = .true.     ! 7.8.2023
        if(trim(idstring) == 'comboboxA1') FieldEditCB = .true.     ! 7.8.2023
        ! ncitemClicked = ncitem

        if(trim(idstring) /= 'notebook1') then
            SaveP = .true.
            call FieldUpdate()
        end if

    end subroutine UR_field_edit_cb

!#############################################################################################

    !//  function UR_field_doact_cb(renderer, path, text, gdata) result(ret) bind(c)
    subroutine UR_field_doact_cb(renderer, path, text)  bind(c)   ! result(ret)

        ! this routine identifies the widget (field renderer) by its idstring (a name)
        ! and sets the following two variables:
        !
        !        FieldDoActCB = .true.
        !        ncitemClicked = ncitem
        !
        ! These variables are used in the GTK dialog loop, located in the middle of
        ! Loadsel_diag_new. If FieldDoActCB is true, that loop terminates, and
        ! Loadsel_diag_new will then react on that.
        !

        use gtk_hl,             only: hl_gtk_listn_set_cell, gtk_tree_view_get_model, &
                                      hl_gtk_listn_get_cell
        use gtk,                only: gtk_buildable_get_name
        use gtk_sup,            only: c_f_string

        use UR_gtk_globals,     only: FieldDoActCB, item_setintern
        use ur_general_globals, only: saveP
        use file_io,            only: logger
        use Top,                only: FieldUpdate

        implicit none

        type(c_ptr), value        :: renderer, path, text
        type(c_ptr)               :: c_text

        character(len=512)        :: log_str
        character(len=64)         :: idstring
        !------------------------------------------------------------------------------------
        ! When using GTK+ directly, keep in mind that only functions can be connected to signals, not methods.
        ! So you will need to use global functions or "static" class functions for signal connections.

        ! ret = 1
        if(item_setintern) return

        c_text = gtk_buildable_get_name(renderer)
        if (c_associated(c_text)) then
            call c_f_string(c_text, idstring)
        else
        !   write(66,*) '****** UR_field_doact_cb :  non-associated widget:'     ! ,renderer
            write(log_str, '(*(g0))') '****** UR_field_doact_cb :  non-associated widget:'     ! ,renderer
            call logger(66, log_str)
            return
        end if

        ! ret = True
        FieldDoActCB = .true.

        ! Exclude following items from emitting a "SaveP"-action:
        if(trim(idstring) /= 'notebook1' .and. trim(idstring) /= 'doELIplot' .and. &
            trim(idstring) /= 'checkbuttonRS' .and. trim(idstring) /= 'comboboxLangg' .and.  &
            trim(idstring) /= 'radiobutton_bg_color' .and. trim(idstring) /= 'buttonCBOK' .and. &
            trim(idstring) /= 'buttonFBApply' .and. trim(idstring) /= 'buttonCBOK' .and. &
            trim(idstring) /= 'CopyGrELI' .and. trim(idstring) /= 'FillDecColumn' .and.   &
            trim(idstring) /= 'combobox_MDselVar' .and. trim(idstring) /= 'MDCalcMean' .and. &
            trim(idstring) /= 'ChooserButton2SE' .and. trim(idstring) /= 'CheckMCSE' .and.  &
            trim(idstring) /= 'check_contrastmode' .and. trim(idstring) /= 'comboboxtextinfofx' .and. &
            trim(idstring) /= 'URfunctions' .and. trim(idstring) /= 'ExportToR' .and.    &
            trim(idstring) /= 'checkAbsTime' .and. trim(idstring) /= 'comboboxtextinfofx' .and. &
            trim(idstring) /= 'HelpFX' ) then        ! 25.2.2024
            SaveP = .true.
            call FieldUpdate()
        end if

    end subroutine UR_field_doact_cb

    !##########################################################################


    subroutine UR_tree_toggle_edit_CB(renderer, path, gdata) bind(c)     ! Result(ret)

        ! this routine identifies the toggle request from a widget (renderer)
        ! and its idstring (a name) and executes the toggleing
        !

        use gtk,             only: gtk_cell_renderer_toggle_get_active
        use gtk_hl,          only: hl_gtk_listn_set_cell, hl_gtk_listn_get_cell
        use gtk_sup,         only: c_f_logical, convert_c_string
        use UR_gtk_globals,  only: toggleTypeGTK,item_setintern
        use ur_general_globals,    only: SaveP        ! ,actual_grid
        use top,             only: FieldUpdate, FindItemP, idpt

        implicit none

        type(c_ptr), value :: renderer, path, gdata

        ! Default call back for a toggle button in a list
        !
        ! RENDERER: c_ptr: required: The renderer which sent the signal
        ! PATH: c_ptr: required: The path at which to insert
        ! GDATA: c_ptr: required: User data, Not used.
        !
        ! The column number is passed via the "column-number" gobject data value.
        ! The treeview containing the cell is passed via the "view" gobject
        ! data value.
        ! The row number is passed as a string in the PATH argument.
        ! This routine is not normally called by the application developer.
        !-
        character(len=200)            :: fpath  ! ,fgdata
        integer(kind=c_int)           :: irow,      icol1, irow1
        type(c_ptr)                   :: list, tree      ! ,listg
        logical                       :: state,stateold,statenew
        character(len=1)              :: str
        integer                       :: ncitem,ncol

    !     ! ret = 1
    !     if(item_setintern) return
    !     call FindItemP(renderer, ncitem)
    !     call convert_c_string(path, fpath)
    !     read(fpath, *) irow

    !     ncol = 1

    !     if(clobj%idd(ncitem)%s == 'cellrenderertext64') then
    !         list = idpt('liststore_gspk1')
    !         tree = idpt('treeview6')
    !     end if
    !     icol1 = 1
    !     irow1 = irow

    !     select case (toggleTypeGTK)
    !       case ('text')
    !         call hl_gtk_listn_get_cell(tree, row=irow1, col=ncol,  svalue=str)
    !         stateold = .false.
    !         if(str == 'T') stateold = .true.
    !         statenew = .not.stateold
    !         str = 'F'
    !         if(statenew) str = 'T'
    !         call hl_gtk_listn_set_cell(tree, row=irow1, col=ncol,  svalue=str)
    !         str = ' '
    !         call hl_gtk_listn_get_cell(tree, row=irow1, col=ncol,  svalue=str)

    !       case ('bool')

    !         state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    !         call hl_gtk_listn_set_cell(tree, irow, ncol, &
    !         & logvalue= .not. state)
    !         state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    !       case default
    !     end select

    !     SaveP = .true.
    !     call FieldUpdate()

    end subroutine UR_tree_toggle_edit_CB

    ! !#############################################################################################

    recursive subroutine UR_NBPage_switched_cb(renderer, path, ppage) bind(c)

        ! this routine identifies the notebook by the renderer pointer and sets
        ! the requestes page (from ppage) and highlights itby the its idstring (a name)
        ! and sets the following two variables:

        use UR_gtk_globals,   only: PageSwitchedCB, ncitemClicked,NBsoftSwitch, &
                                    item_setintern, switched_ignore
        use UR_Gleich_globals, only: loadingpro
        use UR_Loadsel,       only: NBpreviousPage, NBcurrentPage
        use gtk,              only: gtk_widget_is_sensitive,gtk_notebook_set_current_page,&
                                    gtk_notebook_set_tab_pos
        use Rout,             only: NBlabelmodify,pending_events
        use top,              only: idpt, FindItemP

        use PMD,              only: ProcMainDiag
        use file_io,          only: logger

        implicit none

        type(c_ptr), value             :: renderer, path, ppage
        integer(c_intptr_t)            :: ipage


        integer                        :: i, ncp, ncpr
        integer                        :: ncitem
        character(len=60)              :: idstring, signal, parentstr, name
        !------------------------------------------------------------------------------------
        ! When using GTK+ directly, keep in mind that only functions can be connected to signals, not methods.
        ! So you will need to use global functions or "static" class functions for signal connections.

    !     if(item_setintern) return
    !     if(NBsoftSwitch) then
    !         return
    !     end if

    !     call FindItemP(renderer, ncitem)
    !     if(ncitem > 0) then
    !         idstring = clobj%idd(ncitem)%s
    !         i = clobj%idparent(ncitem)
    !         parentstr = clobj%name(i)%s
    !         signal = clobj%signal(ncitem)%s
    !         name = clobj%name(ncitem)%s

    !         if(loadingpro) return
    !     else
    !         if(switched_ignore) then
    !             switched_ignore = .false.
    !             return
    !         end if

    !         ! write(log_str, '(a,i11)') '****** UR_PageSwitched_cb :  not associated widget:', renderer
    !         ! call logger(66, log_str)
    !         return
    !     end if

    !     ipage = transfer(ppage, ipage) + 1

    !     if(trim(idstring) == 'notebook1' .and. len_trim(signal) > 0) then
    !         ncpr = NBpreviousPage
    !         ! current page:
    !         ncp = NBcurrentPage
    !         if(ipage /= ncp) then
    !             NBpreviousPage = ncp
    !             NBcurrentPage = int(ipage)
    !             if(ipage == 3 .and. gtk_widget_is_sensitive(idpt('NBValUnc')) == 0_c_int) call NBlabelmodify()
    !             if(ipage == 4 .and. gtk_widget_is_sensitive(idpt('NBBudget')) == 0_c_int) call NBlabelmodify()
    !             if(ipage == 5 .and. gtk_widget_is_sensitive(idpt('NBResults')) == 0_c_int) call NBlabelmodify()
    !         end if
    !         call ProcMainDiag(ncitem)

    !     end if

    !     ! ret = True
    !     PageSwitchedCB = .true.
    !     ncitemClicked = ncitem

    end subroutine UR_NBPage_switched_cb

    !#############################################################################################

    ! function UR_ActualTreeV_CB(renderer, path, text, gdata) result(ret) bind(c)
    subroutine UR_ActualTreeV_CB(renderer, path, text, gdata)  bind(c)    ! result(ret)

        ! this routine identifies the actual treeview as string variable
        ! actual_grid and also finds out which rows (variable numrows-marked) in
        ! that grid (treeview) have been marked by the user.

        use UR_gtk_globals,     only: item_setintern
        use ur_general_globals, only: actual_grid
        use gtk_hl,             only: hl_gtk_listn_get_selections

        use file_io,            only: logger
        use top,                only: idpt, FindItemP

        implicit none

        type(c_ptr), value           :: renderer, text,path, gdata
        integer(kind=c_int)          :: ret

        character(len=60)            :: idstring, name, parentstr, signal
        integer(kind=c_int), allocatable   :: rownums_marked(:)
        character(len=512)           :: log_str
        integer                      :: numrows_marked,krow
        !------------------------------------------------------------------------------------
        ! When using GTK+ directly, keep in mind that only functions can be connected to signals, not methods.
        ! So you will need to use global functions or "static" class functions for signal connections.

        !   path is expected to be a colon separated list of numbers.
        !   For example, the string 10:4:0 would create a path of depth 3 pointing to
        !   the 11th child of the root node, the 5th child of that 11th child, and the
        !   1st child of that 5th child. If an invalid path string is passed in, NULL is returned.

    !     ret = 1
    !     if(item_setintern) return

    !     ret = 0
    !     call FindItemP(renderer, ncitem)
    !     if(ncitem > 0) then
    !         idstring = clobj%idd(ncitem)%s
    !         i = clobj%idparent(ncitem)
    !         parentstr = clobj%name(i)%s
    !         signal = clobj%signal(ncitem)%s
    !         name = clobj%name(ncitem)%s
    !     else

    !         write(log_str, '(*(g0))') '****** UR_ActualTreeV_cb :  nicht zugeordnetes widget:'     ! ,renderer
    !         call logger(66, log_str)
    !         return
    !     end if

    !     if(trim(Name) == 'GtkTreeSelection') then
    !         actual_grid = ' '
    !         if(trim(idstring) == 'treeview-selectionTV1') actual_grid = 'treeview1'
    !         if(trim(idstring) == 'treeview-selectionTV2') actual_grid = 'treeview2'
    !         if(trim(idstring) == 'treeview-selectionTV3') actual_grid = 'treeview3'
    !         if(trim(idstring) == 'treeview-selectionTV4') actual_grid = 'treeview4'
    !         if(trim(idstring) == 'treeview-selectionTV5') actual_grid = 'treeview5'
    !         if(trim(idstring) == 'treeview-selectionTV6') actual_grid = 'treeview6'
    !         if(trim(idstring) == 'treeview-selectionTV7') actual_grid = 'treeview7'
    !         if(trim(idstring) == 'treeview-selectionTV8') actual_grid = 'treeview8'
    !         ret = 1
    !     end if
    !     if(len_trim(actual_grid) == 0) then
    !         return
    !     else
    !         krow = 0
    !         numrows_marked = hl_gtk_listn_get_selections(idpt(actual_grid), rownums_marked)
    !         if(numrows_marked == 1) krow = minval(rownums_marked) + 1
    !         return
    !     end if

    end subroutine UR_ActualTreeV_CB

    ! !#############################################################################################

    subroutine UR_TV_column_clicked_cb(renderer, path, text)  bind(c)     !  result(ret)

        ! this routine identfies which treeview column was clicked
        !  - and does not do any more significant

        use file_io,        only: logger
        use gtk_sup,        only: convert_c_string

        implicit none

        type(c_ptr), value    :: renderer, path, text

        integer               :: i, ncitem
        character(len=60)     :: idstring, signal, parentstr, name, fpath, ftext
        character(len=512)    :: log_str
        logical               :: pout

    !     pout = .false.
    !     pout = .true.

    !     if(pout)  then
    !         write(log_str, '(*(g0))') 'UR_TV_column_clicked_cb  arrived at'
    !         call logger(66, log_str)
    !     end if
    !     call FindItemP(renderer, ncitem)
    !     if(ncitem > 0) then
    !         idstring = clobj%idd(ncitem)%s
    !         i = clobj%idparent(ncitem)
    !         parentstr = clobj%name(i)%s
    !         signal = clobj%signal(ncitem)%s
    !         name = clobj%name(ncitem)%s
    !         ! if(pout)  then
    !         !     write(log_str, '(*(g0))') '****** UR_TV_column_clicked_cb : idstring=',trim(idstring),'  path=',path,' text=',text
    !         !     call logger(66, log_str)
    !         ! end if
    !     else
    !         if(pout)  then
    !             write(log_str, '(*(g0))') '****** UR_TV_column_clicked_cb :  nicht zugeordnetes widget:'     ! ,renderer
    !             call logger(66, log_str)
    !         end if
    !         return
    !     end if

    !     call convert_c_string(path, fpath)
    !     if(pout)  then
    !         write(log_str, '(*(g0))') '****** UR_TV_column_clicked_cb : fpath=', trim(fpath)
    !         call logger(66, log_str)
    !     end if

    !     call convert_c_string(text, ftext)

    !     if(pout)  then
    !         write(log_str, '(*(g0))') '****** UR_TV_column_clicked_cb : ftext=', trim(ftext)
    !         call logger(66, log_str)
    !     end if

    end subroutine UR_TV_column_clicked_cb

    !#############################################################################################

    subroutine SetColors()

        ! sets the colors for various widgets, dependent on the contrast mode
        ! chosen in UR2cfg.dat. Several entry fields are set markable and whether
        ! the can grab focus or not.

        use UR_gtk_globals,       only: provider
        use gtk,                  only: GTK_STATE_FLAG_NORMAL,gtk_widget_set_focus_on_click, &
                                        gtk_widget_set_sensitive,gtk_entry_set_has_frame, &
                                        gtk_entry_grab_focus_without_selecting, &
                                        gtk_widget_is_sensitive, &
                                        gtk_widget_get_state_flags,gtk_label_set_attributes, &
                                        gtk_css_provider_load_from_data, &
                                        gtk_widget_override_cursor, &
                                        gtk_text_view_reset_cursor_blink, &
                                        gtk_text_view_set_cursor_visible

        use Rout,                 only: pending_events, WDPutLabelColorB, WDPutLabelColorF
        use top,                  only: idpt
        use file_io,              only: logger
        use color_theme
        implicit none

        integer             :: i
        character(len=7)    :: colorname
        character(len=:),allocatable  :: custom_css_style
        integer(c_int)      :: res
        type(c_ptr),target  :: cerror
        character(len=128)  :: log_str

        cerror = c_null_ptr

        ! Problem with css: I have not been very successful in applying :not() selectors
        ! for excluding more than one entry from coloring. The only successful way was to
        ! use chaining of several elements to be exluded: see the "long line" starting
        ! with ' entry:not(#TRMCpm):not(#TRMCpmu):not......
        ! The reason for excluding these nine entries is that otherwise they cannot be
        ! re-colored with red foreground color in MCCalc, if they were initiated by css.
        !
        ! Beforing apply this, the MC-related 9 entries must get values called "TRMC*" in the
        ! fields "Widget-name" (gemeinsam) in the Glade file!
        ! It seems that for gtk_css the field "Widget-name" is treated as id in css!
        ! 20.9.2024 GK

        if(get_theme_name() == 'contrast') then
            custom_css_style = &
                    ' .button, filechooser entry { color: white; background: #5A5A5A; } ' // &
                    ' .textview, textview text { color: white; background-color: black; } ' // &
                    ' entry:not(#TRMCpm):not(#TRMCpmu):not(#TRMCval):not(#TRMCvalu):not(#TRMCvalru):not(#TRMClq):not(#TRMCuq):not(#TRMCdt):not(#TRMCdl) { color: white; background: #00002F; } ' // &
                    ' .treeview.view header button { color: white; background-color: #4A4A4A; } ' // &
                    ' input, entry, textview { caret-color: white; border-style: solid} ' // &
                    ' box.linked > button.combo > box > button, cellview { color: white;  background-color: #1d1d1d } '
        else
            custom_css_style = &
                    '.button, filechooser entry { color: black; background: #ECECE9; } ' // &
                    '.button:disabled { color: #F1F1BE; } '  // &
                    ' entry:not(#TRMCpm):not(#TRMCpmu):not(#TRMCval):not(#TRMCvalu):not(#TRMCvalru):not(#TRMClq):not(#TRMCuq):not(#TRMCdt):not(#TRMCdl) { color: black; background: #FFFFDF; } ' // &
                    ' .textview text { color: black; background-color: white; } ' // &
                    ' .treeview.view header button { color: black; background-color: white; } ' // &
                    ' input, entry, textview { caret-color: black; border-style: solid} ' // &
                    ' box.linked > button.combo > box > button, cellview { color: black;  background-color: white } '
        end if

        res = gtk_css_provider_load_from_data(provider, &
                                              custom_css_style // c_null_char, &
                                              -1_c_size_t, &
                                              c_loc(cerror))

        ! write(log_str, '(A,I0)') "load css from data: res= ", res
        ! call logger(66, log_str)

        ! if(c_associated(cerror)) then
        !     call EvalGerror('Load css from data:  errormessage=', cerror)
        ! end if

        ! do i=1, nclobj

        !     if(clobj%name(i)%s == 'GtkTextView' .or. clobj%idd(i)%s == 'window1') then
        !         call gtk_widget_set_focus_on_click(clobj%id_ptr(i), 1_c_int)
        !         call gtk_widget_set_sensitive(idpt(clobj%idd(i)%s), 1_c_int)
        !     end if

        !     if(clobj%name(i)%s == 'GtkMenu' ) then
        !         ! Do not include GtkMenuItem here!
        !         res = gtk_widget_is_sensitive(clobj%id_ptr(i))
        !         call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('frame_bg'))
        !         call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('label_fg'))
        !         cycle
        !     else if (clobj%name(i)%s == 'GtkMenuBar') then
        !         call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('label_bg'))
        !         call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('label_fg'))
        !         cycle
        !     else if(clobj%name(i)%s == 'GtkToolbar') then
        !         if (get_theme_name() /= 'contrast') then
        !             call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#F6F5F0")
        !             call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#000000")
        !             call WDPutLabelColorB('grid42',GTK_STATE_FLAG_NORMAL, "#F6F5F0")
        !         else
        !             call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#9D9D9D")
        !             call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
        !             call WDPutLabelColorB('grid42',GTK_STATE_FLAG_NORMAL, "#9D9D9D")
        !         end if
        !         cycle
        !     else if(clobj%name(i)%s == 'GtkNotebook') then
        !         call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('GtkNotebook_fg'))
        !         call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('GtkNotebook_bg'))
        !         cycle
        !     else if(clobj%name(i)%s == 'GtkRadioButton') then
        !         call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('label_fg'))
        !         cycle
        !     else if(clobj%name(i)%s == 'GtkTreeView') then
        !         call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('label_fg'))
        !         cycle
        !     else if(clobj%name(i)%s == 'GtkScrolledWindow') then
        !         cycle
        !     else if(clobj%name(i)%s == 'GtkFileChooserButton') then
        !         cycle
        !     end if

        !     if( clobj%name(i)%s == 'GtkFrame') then
        !         call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, &
        !                               get_color_string('frame_bg'))
        !         call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, &
        !                               get_color_string('frame_fg'))
        !     end if

        !     if( clobj%name(i)%s == 'GtkLabel'  .or.    &
        !         clobj%name(i)%s == 'GtkCheckButton' .or.  &
        !         clobj%name(i)%s == 'GtkStatusbar' ) then

        !         call WDPutLabelColorF(clobj%idd(i)%s, GTK_STATE_FLAG_NORMAL, &
        !                               get_color_string('label_fg'))  !  "#e5a50a")
        !     end if

        !     if(clobj%name(i)%s == 'GtkEntry') then
        !         call gtk_entry_set_has_frame(clobj%id_ptr(i), 0_c_int)
        !         ! exception:  24.7.2023, 3.8.2023
        !         if(clobj%idd(i)%s == 'entrySeparation') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
        !         if(clobj%idd(i)%s == 'entryDecaycolVal') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
        !         if(clobj%idd(i)%s == 'entryFormula') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
        !         if(clobj%idd(i)%s == 'entry_b2LFactor') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
        !         if(clobj%idd(i)%s == 'DistribEntry1') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
        !         if(clobj%idd(i)%s == 'DistribEntry2') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
        !         if(clobj%idd(i)%s == 'DistribEntry3') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
        !         if(clobj%idd(i)%s == 'DistribEntry4') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)


        !         if(clobj%idd(i)%s(1:7) == 'TRentry' .and.  clobj%idd(i)%s(1:12) /= 'TRentryMCanz') then
        !           ! The Entrys on the TAB "Results" must be made insensitive (i.e. manual editing is forbidden),
        !           ! apart from few Entrys required for manual input of MC repetions and MC runs.
        !           ! 20.9.2024 GK
        !           call gtk_widget_set_sensitive(idpt(clobj%idd(i)%s), 0_c_int)
        !         end if

        !         if(clobj%idd(i)%s(1:9) == 'TRentryMC' .and.  clobj%idd(i)%s(1:12) /= 'TRentryMCanz') then
        !           ! 20.9.2024 GK
        !           ! the MC related output fields are colored here, which allows also for the contrast mode displaying
        !           call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('entry_fg'))
        !           call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, get_color_string('entry_bg'))
        !         end if
        !     end if

        ! end do
        colorname = get_color_string('frame_bg')
        call WDPutLabelColorB('box1',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box2',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box3',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box4',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box5',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('grid5',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('grid7',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('grid34',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('grid35',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box8',GTK_STATE_FLAG_NORMAL, colorname)

        call WDPutLabelColorB('box7',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box9',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box13',GTK_STATE_FLAG_NORMAL, colorname)

        call WDPutLabelColorB('dialog_vbox17',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox21',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox6',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox9',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox13',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox1',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox4',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox2',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox5',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox11',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox15',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox2',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box6',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box27',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box12',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box45',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box23',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box26',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box14',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('boxELI',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('boxDistrib',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('boxBatEval',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('BTBox1',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('boxSerEval',GTK_STATE_FLAG_NORMAL, colorname)

        call WDPutLabelColorB('grid26',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box14',GTK_STATE_FLAG_NORMAL, colorname)

        call WDPutLabelColorB('dialog_vbox2',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('box23',GTK_STATE_FLAG_NORMAL, colorname)
        call WDPutLabelColorB('dialog_vbox4',GTK_STATE_FLAG_NORMAL, colorname)


    end subroutine SetColors

    !#############################################################################################

    elemental function lowcase(string)

        ! turns all characters in the string to lower case.

        implicit none
        character(len=*), intent(in) :: string
        character(len=len(string))   :: lowcase

        integer   , parameter :: ucmin = iachar('A'), ucmax = iachar('Z')
        integer   , parameter :: case_diff = iachar('A') - iachar('a')
        integer    :: i, ic

        lowcase = string
        do i = 1, len(string)
            ic = iachar(string(i:i))
            if (ic >= ucmin .and. ic <= ucmax) lowcase(i:i) = achar(ic-case_diff)
        end do
    end function lowcase

!#############################################################################################

end module gui_functions

!#############################################################################################

!Don't modify the c_f_string_chars subroutine
subroutine c_f_string_chars(c_string, f_string)
    ! Helper function
    use, intrinsic :: iso_c_binding
    implicit none
    character(len=1, kind=c_char), intent(in) :: c_string(*)
    character(len=*), intent(out) :: f_string
    integer    :: i
    i=1
    do while(c_string(i)/=c_null_char .and. i<=len(f_string))
        f_string(i:i) = c_string(i)
        i=i+1
    end do
    if (i<=len(f_string)) f_string(i:) = ' '
end subroutine c_f_string_chars

!#############################################################################################
