!------------------------------------------------------------------------------!
! UncertRadio - Software for calculation of characteristic threshold values    !
! Copyright (C) 2014 - 2024 Günter Karnisch                                    !
!                                                                              !
! This program is free software: you can redistribute it and/or modify         !
! it under the terms of the GNU General Public License as published by         !
! the Free Software Foundation, either version 3 of the License, or            !
! (at your option) any later version.                                          !
!                                                                              !
! This program is distributed in the hope that it will be useful,              !
! but WITHOUT ANY WARRANTY; without even the implied warranty of               !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                 !
! GNU General Public License for more details.                                 !
!                                                                              !
! You should have received a copy of the GNU General Public License            !
! along with this program. If not, see <https://www.gnu.org/licenses/>.        !
!------------------------------------------------------------------------------!

! contains:
! gui_main
! checkStart
! monitor_coordinates
! FindMonitorRect

! DefColors
! Glade_modify
! check_cargs

program UncertRadio

    ! the main program of UncertRadio:

    ! it gets and interprets the command line arguments; sets the work_path and
    ! other path variables; gets the current Windows language and sets it later to
    ! a different language, if requated;
    ! sets envirnonment variables for PLPLOT; opens a file fort66.txt and several
    ! other files with similar names;
    ! sets the filename of the Glade file containing the GUI, which is user later
    ! in create_window;
    ! reads the congfig file UR2_cfg.dat;
    ! reads the screen/monitor configuration;
    ! checks if another UR instance is already running to prevent from running two instances;
    ! set the colors of UR (DefColors); includes also a contrast mode;
    ! create the UR window (create_window);
    ! starts processes like processing batch files, if requested by command line arguments;
    ! if no such process is to be started: it then starts the main GTK loop running the
    ! UncertRadio GUI;
    ! if the GTK main loop was stopped by the user, it closes files and terminates the
    ! program;
    ! if UR was used from within Excel (runauto=.true.), it terminates with an exit
    ! status=3 if an error had occurred before the GTK loop was stopped.

    use, intrinsic :: iso_c_binding

    use gtk,              only: gtk_init, &
                                gtk_main, &
                                gtk_main_quit, &
                                gtk_widget_set_visible, &
                                gtk_buttons_OK, &
                                gtk_widget_get_allocation, &
                                gtk_window_resize, &
                                gtk_widget_set_size_request, &
                                gtk_widget_set_sensitive, &
                                gtk_window_move, &
                                FALSE, &
                                GTK_MESSAGE_WARNING

    use gdk,              only: gdk_screen_get_monitor_at_point
    use gtk_sup
    use UR_types
    use gui_functions,    only: idpt, create_window, show_window
    use UR_gtk_variables, only: UR_win, gladeorg_file, glade_org, &
                                item_setintern,runauto,winPL_shown,prout_gldsys,  &
                                scrwidth_min,scrwidth_max,scrheight_min,monitorUR,gscreen, &
                                monitor_at_point,runbatser

    use ur_variables,     only: automode, fname_getarg, &
                                work_path, log_path, results_path, help_path, example_path, &
                                langg, wpunix, batest_on, actpath, Excel_langg,  &
                                autoreport, fname, Sample_ID, &
                                Excel_sDecimalPoint,Excel_sListSeparator,sDecimalPoint,sListSeparator, &
                                Michel_opt1, &
                                bat_serial,bat_mc,langgSV,serial_csvinput, &
                                base_project_SE, kfrom_SE, kto_SE,cgetarg, progstart_on, simul_ProSetup, &
                                done_simul_ProSetup,open_project_parts, dir_sep, UR_git_hash, UR_version_tag, &
                                fileToSimulate

    use g,                  only: g_get_current_dir, g_strip_context, g_path_is_absolute, g_chdir

    use Rout,               only: MessageShow,pending_events,WDNotebookSetCurrPage
    use Usub3,              only: AutoReportWrite
    use UR_interfaces,      only: ProcessLoadPro_new
    use CHF,                only: ucase, StrReplace, fltu, flfu
    use gtk_draw_hl,        only: gtkallocation
    use UR_Loadsel,         only: NBcurrentPage
    use Top,                only: CharModA1
    use urInit,             only: READ_CFG
    use UR_Gleich,          only: ifehl

    use UR_params,          only: UR2_CFG_FILE, LOCKFILENAME, GPL_HEADER
    use translation_module, only: T => get_translation
    use file_io
    use UR_tests

    implicit none

    integer                    :: ncomargs, i, i1, error_str_conv

    character(512)             :: tmp_str
    character(256)             :: log_str
    character(:), allocatable  :: message, title

    real(rn)                   :: start, finish
    integer(c_int)             :: resp, mposx, mposy

    type(gtkallocation), target  :: alloc

    integer                    :: finfo(13)
    logical                    :: lexist, ur_runs

    character(5)               :: flang

    !--------------------------------------------------------------------------------------

    allocate(character(512) :: fname_getarg)
    ! Check the os; i think atm the convinient way to do this is to use
    ! the is_UNIX_OS function from gtk_sup
    wpunix = is_UNIX_OS()
    if (wpunix) then
        dir_sep = '/'
    else
        dir_sep = '\'
    end if
	! set all path variables. Ensure that they all have utf-8 encoding
    ! find the UncertRadio work path
    call get_command_argument(0, tmp_str)
	! convert to utf-8 if the local encoding is different
    tmp_str = fltu(tmp_str, error_str_conv)
    if (error_str_conv > 0) write(*,*) 'Warning, could not convert programm call string to utf-8'

    work_path = ' '
    if(len_trim(tmp_str) > 0) then
        i1 = index(tmp_str, dir_sep, back=.true.)
        if(i1 > 0) work_path = tmp_str(1:i1)
    else
        write(*,*) "CRITICAL ERROR: could not find UR work path"
        stop
    end if

    ! get the current directory using the GLib function
    allocate(character(len=len(tmp_str))  :: actpath)
    call convert_c_string(g_get_current_dir(), actpath)
    actpath = trim(actpath) // dir_sep
    ! write(*,*) 'curr_dir = ', trim(actpath)

    ! if the work path is relativ, convert to an absolute path
    if (g_path_is_absolute(work_path) == 0) then
        work_path = actpath // work_path(3:)
    end if

    ! get the (relative) log path from config file
    call read_config('log_path', log_path, work_path // UR2_CFG_FILE)
    log_path = work_path // log_path
    call StrReplace(log_path, '/', dir_sep, .TRUE., .FALSE.)

    ! from here on we are able to write to logfiles!
    call logger(66, GPL_HEADER, new=.true., stdout=.true.)
    call logger(66, "This program comes with ABSOLUTELY NO WARRANTY;", stdout=.true.)
    call logger(66, "This is free software, and you are welcome to redistribute it", stdout=.true.)
    call logger(66, "under certain conditions; see COPYING"// char(10), stdout=.true.)

    if (wpunix) then
        call logger(66, "Operating System: Linux")
    else
        call logger(66, "Operating System: Windows")
    endif

    ! change the current path to the work path.
    i1 = g_chdir(work_path // c_null_char)
    if (i1 /= 0) then
        call logger(66, "CRITICAL ERROR: could not change current dir to work path")
        call quit_uncertRadio(3)
    end if

    call logger(66, "work_path = " // work_path)

    ! get the (relative) results path from config file
    call read_config('results_path', results_path, work_path // UR2_CFG_FILE)
    results_path = work_path // results_path
    call StrReplace(results_path, '/', dir_sep, .true., .false.)
    call logger(66, "results_path = " // results_path)

    ! get the (relative) help path from config file
    call read_config('Help_path', help_path, work_path // UR2_CFG_FILE)
    help_path = work_path // help_path
    call StrReplace(help_path, '/', dir_sep, .true., .false.)
    call logger(66, "help_path = " // help_path)

    ! get the (relative) example path from config file
    call read_config('example_path', example_path, work_path // UR2_CFG_FILE)
    example_path = work_path // example_path
    call StrReplace(example_path, '/', dir_sep, .true., .false.)
    call logger(66, "example_path = " // example_path)
    call logger(66, "")

    ! initate log and result files
    call logger(30, GPL_HEADER, new=.true.)
    call logger(30, '')
    call logger(63, GPL_HEADER, new=.true.)
    call logger(63, '')

    ! get the UR Version and git hash
#ifdef GITVERSIONTAG
    UR_version_tag = GITVERSIONTAG
    call logger(66, text="Version: "// trim(UR_version_tag))
#endif
#ifdef GITHASH
    UR_git_hash = GITHASH
    call logger(66, "Git Hash: "// trim(UR_git_hash))
#endif
    call logger(66, "")
	! initiate gtk to show show gui error-messages
    call gtk_init()

    NBcurrentPage = 0

    runauto = .false.
    automode = .false.
    Excel_langg = ''
    langg = ''

    progstart_on = .true.

    ! try to get the user/system language
    call get_environment_variable("LANG", flang)
    langg = 'EN' ! set a fall-back language, atm english
    if ( any(ucase(flang(1:2)) == ['DE', 'EN', 'FR'] )) then
        langg = ucase(flang(1:2))
    else
        call logger(66, "Warning: $LANG not defined, falling back to: " // langg)
    endif

    call logger(66, "Language before reading UR2_cfg: " // langg )

    ifehl = 0
    glade_org = .false.

    ! check Glade file:
    inquire(file=flfu(work_path // gladeorg_file), exist=lexist)
    call logger(66, "gladefile= " // work_path // gladeorg_file)
    if(lexist) then
        call stat(flfu(work_path // gladeorg_file), finfo)
        glade_org = .true.
    end if

    if(.not. glade_org) then
!         write(66,*) 'No Glade file found!'
        call logger(66, "No Glade file found!")
        call quit_uncertradio(4)
    end if

    ! read the config file (UR2_cfg.dat)
    call Read_CFG()

    call monitor_coordinates()

    if(ifehl == 1) then
        call gtk_main_quit()
        call quit_uncertradio(3)
    end if

    prout_gldsys = .false.

    call cpu_time(start)

    call create_window(UR_win, ifehl)

    if(ifehl == 1) then
        call logger(66, "Create window NOT successful!")
        call quit_uncertradio(3)
    end if

    ! Test for an already running instance of UR2; if so, don't start a second one.
    ! and stop UR with errorcode 2
    call check_if_running(work_path // LOCKFILENAME, ur_runs)
    if(ur_runs) then
        call logger(66, "An UR2 instance is already running! A second one is not allowed!")
        tmp_str = T('An UR2 instance is already running! A second one is not allowed! If this is an error, please delete the file: ') // work_path // lockFileName
        call MessageShow(trim(tmp_str)//'  ', GTK_BUTTONS_OK, "Warning", resp, mtype=GTK_MESSAGE_WARNING)
        call quit_uncertradio(2)
    end if

    call get_command_argument(1, tmp_str)
    if (tmp_str == 'run_tests') then
        call run_tests()
        call quit_uncertradio(0)
    end if

    call cpu_time(finish)

    write(log_str, '(A, F0.2, A)') " Create window1 successful!  cpu-time: ", finish - start, " s"
    call logger(66, log_str)

    call gtk_widget_set_visible(idpt('dialog_LoadPro'), False)

    !------------------------------------------------------------
    ! get the number of given command arguments
    ncomargs = command_argument_count()
    allocate(cgetarg(ncomargs))
    call CharModA1(cgetarg, ncomargs)

    if(ncomargs > 0) then

        ! first read all arguments
        do i = 1, ncomargs
            call get_command_argument(i, tmp_str)
            ! convert to utf-8 if the local encoding is different
            cgetarg(i)%s = fltu(tmp_str, error_str_conv)
            if (error_str_conv > 0) call logger(66, "Warning, could not convert command " // &
                                                    "line argument string to utf-8: " // &
                                                    trim(tmp_str) )
            write(log_str, '(*(g0))') 'CmdLine-Argument ',i,' : ', cgetarg(i)%s
            call logger(66, log_str)

        end do

        ! now check the first argument for keywords
        call logger(66, "fname_getarg 1= " // ucase(cgetarg(1)%s) )
        if (ncomargs > 2 .and. any(ucase(cgetarg(1)%s) == ['AUTO   ', 'AUTOSEP', 'BATSER '])) then
            autoreport = .true.
            runauto = .true.
            automode = .true.
            fname_getarg = cgetarg(2)%s
            call StrReplace(fname_getarg, '/', dir_sep, .true., .false.)

            if (ucase(cgetarg(1)%s) == 'BATSER') then
                serial_csvinput = cgetarg(3)%s
                call StrReplace(serial_csvinput, '/', dir_sep, .true., .false.)
                sample_ID = cgetarg(4)%s
                base_project_SE = fname_getarg
            else ! AUTO, AUTOSEP
                sample_ID = cgetarg(3)%s
            end if

            do i = 3, ncomargs
                if (cgetarg(i)%s(1:2) == 'LC') then
                    if (len(cgetarg(i)%s) == 7) then
                        Excel_langg = cgetarg(i)%s(4:5)
                        langg = Excel_langg
                        Excel_sDecimalPoint = cgetarg(i)%s(6:6)
                        Excel_sListSeparator = cgetarg(i)%s(7:7)
                    else
                        ifehl = 1
                        call logger(66, "The command string argument  " // cgetarg(i)%s // &
                                        " is incomplete!")
                        call quit_uncertradio(3)
                    end if
                end if
            end do

            if(automode .and. len_trim(Excel_langg) == 2) then
                sDecimalPoint = Excel_sDecimalPoint
                sListSeparator = Excel_sListSeparator
                call logger(66, 'UR2 called from Excel:  language=' // langg // &
                                '  sDecimalPoint=' // sDecimalPoint // &
                                '  sListSeparator=' // sListSeparator )
            end if

        else if (ncomargs == 1) then
            ! project to open given as first argument
            fname_getarg = cgetarg(1)%s

            ! check if the given project name indicates a correct project file
            if(index(fname_getarg,'.txp') == 0 .and. index(fname_getarg,'.csv') == 0) then
                if(langg == 'DE') then
                    message = 'Diese Datei ist keine Projektdatei!'
                    title = "Projekt öffnen"
                else if(langg == 'FR') then
                    message = "Ce fichier n''est pas un fichier de projet!"
                    title = "Projet ouvert"
                else
                    message = 'The file is not a project file!'
                    title = "Open project"
                end if

                call messageshow(message=message, &
                                 button_set=gtk_buttons_ok, &
                                 title=title, &
                                 resp=resp, &
                                 mtype=gtk_message_warning)
                fname = ''
                fname_getarg = ''
            else
                fname = trim(fname_getarg)
                call logger(66, 'iosargument: ' // trim(fname_getarg))
                ifehl= 0
                call processloadpro_new(0, 1)       ! start calculations with the first output quantity
                call wdnotebooksetcurrpage('notebook1', 5)
                nbcurrentpage = 5
            end if
        end if
    end if

    Michel_opt1 = .false.
    !Inquire(FILE=trim(work_path)//'Michelplot.txt',exist=Lexist)
    !  if(Lexist) Michel_opt1 = .true.

    ! must be called here, i.e., before the first call of show_window
    call gtk_widget_get_allocation(idpt('window1'),c_loc(alloc))

    !---------------------------------------------------------------------------------
    if(.not. runauto) call show_window(UR_win)
    call cpu_time(finish)
    !---------------------------------------------------------------------------------

    if(monitorUR > 0) then
        ! don't use this setting for monitor 0 !
        ! mposx = scrwidth_min + int(real(scrwidth_max - scrwidth_min,rn)*0.05_rn)
        mposx = scrwidth_min + int(real(scrwidth_max - scrwidth_min,rn)*0.10_rn)

        mposy = scrheight_min + 50
        write(log_str, '(a,2I5)') '***  Main window: first Show:  upper-left pos: mposx,mposy=',mposx,mposy
        call logger(66, log_str)
        call gtk_window_move(idpt('window1'),mposx,mposy)

        monitor_at_point = gdk_screen_get_monitor_at_point(gscreen,mposx+10_c_int,mposy+10_c_int)+1_c_int
        write(log_str, '(a,I5)') '***  Main window: Monitor# at mposx+10,mposy+10= ',monitor_at_point
        call logger(66, log_str)

    end if

    call gtk_widget_get_allocation(idpt('window1'),c_loc(alloc))
    write(log_str, '(a,i0,a,i0)') '***  Main window:  width= ',alloc%width,'  height= ',alloc%height
    call logger(66, log_str)
    call logger(66, '------------------------------------------------------------------------------')

    ! With simul_ProSetup = .true., it is tested to load that project file given
    ! under fileToSimulate in such a way, as if the user would proceed if he
    ! would set up the project for the first time.
    ! However, the user interaction is not necessary: the equations, e.g., are taken
    ! from the project file and transferred to the corresponding textview. Similarly,
    ! other data read from the project file are transferred to the treeviews. In this
    ! way, the proper functioning of the sequence of buttons is tested, which are
    ! to be "clicked internally" on the way to the final result table.
    !
    ! To start this test, set simul_ProSetup = .true., recompile the program; then start
    ! uncertRadio.exe and load any project file, which means that actually
    ! the file identified by fileToSimulate is loaded.
    !
    ! GK: this test with its example projects was repeated and updated on 21.-22.9.2023.

    simul_ProSetup = .false.
    !simul_ProSetup = .true.       ! set to .true. for testing

    done_simul_ProSetup = .false.
    open_project_parts = .false.

    if(simul_ProSetup) then
        open_project_parts = .true.
        ! fileToSimulate = example_path // 'de' // dir_sep // 'Ac228_binomial_V2_DE.txp'     ! R0 fehlt
        ! fileToSimulate = example_path // 'de' // dir_sep // 'DWD-LSC-3kanal-V2_DE.txp'        ! ok
        fileToSimulate = example_path // 'de' // dir_sep // 'DWD-LSC-3kanal-V2_DE.txp'        ! ok
        ! fileToSimulate = example_path // 'en' // dir_sep // 'La140_REMSPEC-4Lines-V3_EN.txp'  ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'NLWKN_Fe-55_mit_KALFIT_DE.txp'   ! OK
        ! fileToSimulate = example_path // 'en' // dir_sep // 'Mean-theta_EN.txp'               ! OK
        ! fileToSimulate = example_path // 'en' // dir_sep // 'Alpha-IAEA-1401-Kanisch_EN.txp'  ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'BSH_Gesamt-Gamma_var2_DE.txp'      ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'DWD_AB-Gesamt-Aeros-Alpha1_DE.txp'  ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'DWD_sr89_sr90_TDCR_Verfahren_V2_DE.txp' ! OK
        ! fileToSimulate = example_path // 'en' // dir_sep // 'Example_8_with_KALFIT_EN.txp'           ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'Fe-55-mit-LSC-und-Standardaddition_DE.TXP'  ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'Galpha_beta_Rusconi_2006_V2_DE.txp'   ! OK
        ! fileToSimulate = example_path // 'en' // dir_sep // 'ISO-Example-2a_V2_EN.txp'       ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'ISO-Neutronen-Dosis_DE.txp'      ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'Janszen-Sr-89-Sr-90_V2_DE.txp'   ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'Michel-2000-b_DE.txp'            ! OK
        ! fileToSimulate = example_path // 'en' // dir_sep // 'Moreno-Sr90_IAEA-135_V2_EN.txp'  ! OK
        ! fileToSimulate = example_path // 'en' // dir_sep // 'PresetCounts_EN.txp'             ! OK
        ! fileToSimulate = example_path // 'en' // dir_sep // 'Ra226_U235-at-186keV_EN.txp'     ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'Rn-222-Emanation_DE.txp'         ! OK
        ! fileToSimulate = example_path // 'en' // dir_sep // 'sumEval_mean_V2_EN.txp'          ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'Tritium_4Bubbler_used_1-3_DE.txp'  ! OK
        ! fileToSimulate = example_path // 'de' // dir_sep // 'J-ALUFT-Sr89-Sr-90_V2_DE.txp'      ! OK
    else
        done_simul_ProSetup = .true.
    end if

    write(log_str, '(*(g0))') 'Main:  after show_window:   MonitorUR=',int(MonitorUR,2)
    call logger(66, log_str)

    batest_on = .false.
    if(NBcurrentPage == 0) NBcurrentPage = 1
    winPL_shown = .false.

    bat_serial = .false.
    bat_mc = .false.
    call gtk_widget_set_sensitive(idpt('SerialEval'), 1_c_int)

    langgSV = langg

    !-----------------------------------------------------------

    if(runauto) then
        call pending_events()
        call AutoReportWrite()
    elseif(runbatser) then
        call pending_events()
        bat_serial = .true.
        kfrom_se = 1
        kto_se = 1000
        call Batch_proc()
    else
        item_setintern = .false.
        call gtk_main()
    end if

    !-----------------------------------------------------------
    ! stop UncertRadio correctly
    call quit_uncertradio(0)

end program UncertRadio

!------------------------------------------------------------------------------!
subroutine quit_uncertradio(error_code)
    use, intrinsic :: iso_c_binding, only : c_null_char

    use UR_VARIABLES,             only: work_path, actpath
    use UR_params,                only: LOCKFILENAME

    use UR_Gleich,                only: ifehl
    use UR_gtk_variables,         only: runauto
    use file_io,                  only: logger
    use g,                        only: g_chdir
    use chf,                      only: flfu

    implicit none
    integer, intent(in)           :: error_code
    logical                       :: exists
    character(len=512)            :: log_str
    integer                       :: stat, nio

    ! possible error_codes are:
    ! 0: everything is fine
    ! 1: not specified atm
    ! 2: UR is already running
    ! 3: ifehl == 1
    ! 4: there is an error with the glade file

    ! Remove the lock file if specified
    if (error_code /= 2) then
        inquire(file=flfu(work_path // LOCKFILENAME), exist=exists)
        if (exists) then
            ! The lock file exists, so remove it
            open(file=flfu(work_path // LOCKFILENAME), newunit=nio, iostat=stat)
            if (stat == 0) close(nio, status='delete', iostat=stat)
        endif
    endif
    if (error_code > 0) then
        write(*,*) 'Warning: Stoping UR with errorcode: ', error_code
!         write(66,*) 'Warning: Stoping UR with errorcode: ', error_code
        write(log_str, '(*(g0))') 'Warning: Stoping UR with errorcode: ', error_code
        call logger(66, log_str)
    end if

    ! change the current path back to the current path, when UR was started.
    stat = g_chdir(actpath // c_null_char)
    if (stat /= 0) then
        call logger(66, "Warning: Could not revert the curr_dir")
    end if

    ! Write log messages and perform necessary cleanup
!     write(66, *) 'runauto=', runauto, ' ifehl=', ifehl
    write(log_str, '(*(g0))') 'runauto=', runauto, ' ifehl=', ifehl
    call logger(66, log_str)
!     write(66,'(A, I0)') ' UR2 terminated with errorcode: ', error_code
    write(log_str, '(A, I0)') ' UR2 terminated with errorcode: ', error_code
    call logger(66, log_str)
    close(66)
    ! Terminate the program showing the error_code

    stop error_code
end subroutine quit_uncertradio


!------------------------------------------------------------------------------!
subroutine check_if_running(lock_file, ur_runs)

    ! checks if another UR instance is already running to prevent from
    ! running two instances of UR; returns ur_runs = false if not running
    ! and .true. otherwise
    !
    ! This example has been completely rewritten and greatly simplified.
    ! It now uses a lock file system. It must be ensured that the lock file
    ! is also deleted again.
    ! This is particularly problematic in the event of crashes, but can also
    ! be used to better identify code errors.
    !
    ! Copyright (C) 2018-2024  Günter Kanisch, Florian Ober

    use chf, only: flfu
    implicit none

    character(len=*), intent(in)   :: lock_file
    integer                        :: nio, iostat
    logical, intent(out)           :: ur_runs

    ! Attempt to open the lock file for exclusive access

    open(newunit=nio, &
         file=flfu(lock_file), &
         status='new', &
         action='write', &
         iostat=iostat)

    if (iostat == 0) then
        ur_runs = .false.
        close(nio)
    else
        ! maybe we need to create more condition which removes the
        ! file e.g. after a certain time?
        ur_runs = .true.
    endif

end subroutine check_if_running


!------------------------------------------------------------------------------!
subroutine monitor_coordinates()

    ! finds the number of monitors combined within a screen;
    ! it then estimates the coordinates (height, width) of the monitor(s)
    !
    ! calls FindMonitorRect
    !
    ! See chapter 1.3 "Using several monitors" of the UncertRadio CHM Help
    ! file for more details.

    !   Copyright (C) 2020-2023  Günter Kanisch

    use UR_params,        only: rn

    use, intrinsic :: iso_c_binding, only:  c_int, &
                                            c_ptr, &
                                            c_loc, &
                                            c_f_pointer, &
                                            c_associated, &
                                            c_char, &
                                            c_size_t, &
                                            c_null_ptr

    use gdk,  only: gdk_display_get_default, &
                    gdk_display_get_default_screen, &
                    gdk_screen_get_n_monitors, &
                    gdk_screen_get_primary_monitor, &
                    gdk_display_get_monitor, &
                    gdk_monitor_get_workarea, &
                    gdk_monitor_get_geometry

    use UR_gtk_variables, only: display,GdkRectangle, &
                                monitorUR, &
                                scrwidth_min, &
                                scrwidth_max, &
                                scrheight_min, &
                                scrheight_max, &
                                monitor, &
                                gscreen

    use Top,              only: idpt
    use UR_Gleich,        only: ifehl

    use file_io,          only: logger

    implicit none

    integer :: monisel, nprim, tmon, tmonx

    integer(c_int)              :: nmonit, atmonx
    type(GdkRectangle),pointer  :: URgdkRect
    type(c_ptr), target         :: cgdkrect
    logical                     :: m0out
    character(len=512)          :: log_str
    integer, allocatable        :: widthmin(:), &
                                   widthmax(:), &
                                   heightmin(:), &
                                   heightmax(:)

    ! GDK: a single GdkScreen combines several physical monitors.

    ifehl = 0
    allocate(URgdkRect)
    display = c_null_ptr
    gscreen = c_null_ptr
    display = gdk_display_get_default()
    gscreen = gdk_display_get_default_screen(display)

    nmonit = max(0_c_int, gdk_screen_get_n_monitors(gscreen))
    tmonx = nmonit
    !     write(66,'(a,i0,a,i0)') 'number of monitors:',int(tmonx,2),'   nmonit=',nmonit
    write(log_str, '(a,i0,a,i0)') 'number of monitors:',int(tmonx,2),'   nmonit=',nmonit
    call logger(66, log_str)
    allocate(widthmin(nmonit), widthmax(nmonit), heightmin(nmonit), heightmax(nmonit))
    widthmin(:) = 0
    widthmax(:) = 0
    heightmin(:) = 0
    heightmax(:) = 0

    monitor = c_null_ptr
    monitor = gdk_display_get_monitor(display, nmonit)

    ! Note: monitorx or monitor should be created only once; for further creations of the
    ! C-pointer monitorx with the GDK function, it must first be reset by monitorx = c_null_ptr;
    ! if not, GDK-critical warnings appear.

    m0out = .false.
    do tmon=1,tmonx
        !         if(tmon == 1) write(66,'(a)') '***  Monitors:'
        if(tmon == 1)  then
            write(log_str, '(a)') '***  Monitors:'
            call logger(66, log_str)
        end if
        call gdk_monitor_get_geometry(gdk_display_get_monitor(display,tmon - 1_c_int), c_loc(cGdkRect))        !
        call c_f_pointer(c_loc(cGdkRect), URGdkRect)

        if(m0out) then
            write(0,'(a,i2,a,4I6)') 'tmon=',tmon,'  URGdkRect=',URGdkRect%x,URGdkRect%y, &
                                     URGdkRect%width,URGdkRect%height
            !             write(66,'(a,i2,a,4I6)') 'tmon=',tmon,'  URGdkRect=',URGdkRect%x,URGdkRect%y, &
            !                 URGdkRect%width,URGdkRect%height
            write(log_str, '(a,i2,a,4I6)') 'tmon=',tmon,'  URGdkRect=',URGdkRect%x,URGdkRect%y, &
                                            URGdkRect%width,URGdkRect%height
            call logger(66, log_str)
        endif

        widthmin(tmon) = URGdkRect%x
        heightmin(tmon) = URGdkRect%y
        widthmax(tmon) = URGdkRect%x + URGdkRect%width
        heightmax(tmon) = URGdkRect%y + URGdkRect%height

    end do
    write(log_str, '(A,i0)') '***  Monitor number selected as given in UR2_cfg.dat: ', monitorUR
    call logger(66, log_str)
    nprim = gdk_screen_get_primary_monitor(gscreen)+0_c_int
    monisel = 1

    atmonx = max(0_c_int, monitorUR - 0_c_int)
    tmon = atmonx + 0_c_int

    scrwidth_min = widthmin(tmon) + 1
    scrwidth_max = widthmax(tmon) - 1
    scrheight_min = heightmin(tmon) + 2
    scrheight_max = heightmax(tmon) - int(0.032_rn*real(heightmax(tmon)-heightmin(tmon), rn) + 0.4999_rn)

    write(log_str, '(a,i0,2(a,i0,a,i0))') '***  Selected monitor: ',monitorUR,'; Screen min-max horiz.: ',  &
          scrwidth_min,' - ',scrwidth_max,'  min-max vertical: ',scrheight_min,' - ',scrheight_max
    call logger(66, log_str)

end subroutine monitor_coordinates
