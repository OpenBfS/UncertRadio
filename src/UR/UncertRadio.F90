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
    use gtk,                only: gtk_init, &
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
    use gui_functions,    only: idpt, create_window, show_window
    use UR_gtk_variables, only: UR_win, gladeorg_file, glade_org, &
                                item_setintern,runauto,winPL_shown,prout_gldsys,  &
                                scrwidth_min,scrwidth_max,scrheight_min,monitorUR,gscreen, &
                                monitor_at_point,runbatser,contrast_mode,contrast_mode_at_start, &
                                item_setintern_window1

    use UR_variables,     only: callBatest, automode, fname_getarg, &
                                work_path, log_path, results_path, help_path, example_path, &
                                langg, wpunix, batest_on, actpath, Excel_langg,  &
                                autoreport, fname, Sample_ID, &
                                Excel_sDecimalPoint,Excel_sListSeparator,sDecimalPoint,sListSeparator, &
                                Michel_opt1, &
                                bat_serial,bat_mc,langgSV,serial_csvinput, &
                                base_project_SE, kfrom_SE, kto_SE,cgetarg, progstart_on, simul_ProSetup, &
                                done_simul_ProSetup,open_project_parts, dir_sep, UR_git_hash, UR_version_tag, &
                                fileToSimulate, GPL_header

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

    use UR_params,          only: rn, UR2_cfg_file, lockFileName
    use file_io


    implicit none

    integer                    :: ncomargs, i, i1, error_str_conv

    character(512)             :: tmp_str
    character(:), allocatable  :: message, title

    real(rn)                   :: start, finish
    integer(c_int)             :: resp, mposx, mposy

    type(gtkallocation), target  :: alloc

    integer                    :: finfo(13), ios
    logical                    :: lexist, ur_runs

    character(5)               :: flang
    !--------------------------------------------------------------------------------------

    allocate(character(512) :: fname_getarg)

    GPL_header = "UncertRadio Copyright (C) 2014 - 2024  G. Kanisch"
    ! Print the copyright informations to stdout
    write(*,*) GPL_header
    write(*,*) "This program comes with ABSOLUTELY NO WARRANTY;"
    write(*,*) "This is free software, and you are welcome to redistribute it"
    write(*,*) "under certain conditions; see COPYING"
    write(*,*)

#ifdef GITVERSIONTAG
    UR_version_tag = GITVERSIONTAG
    write(*,*) "Version: "// trim(UR_version_tag)
#endif
#ifdef GITHASH
    UR_git_hash = GITHASH
    write(*,*) "Git Hash: "// trim(UR_git_hash)
#endif
    ! Check the os; i think atm the convinient way to do this is to use
    ! the is_UNIX_OS function from gtk_sup
    wpunix = is_UNIX_OS()
    if (wpunix) then
        dir_sep = '/'
        write(*,*) 'Operating System: Linux'
    else
        dir_sep = '\'
        write(*,*) 'Operating System: Windows'
    endif

    write(*,*)

	! set all path variables. Ensure that they all have utf-8 encoding

    ! get the current directory using the GLib function
    allocate(character(len=len(tmp_str))  :: actpath)
    call convert_c_string(g_get_current_dir(), actpath)
    actpath = trim(actpath) // dir_sep
    write(*,*) 'curr_dir = ', trim(actpath)

    ! find the UncertRadio work path
    ! get the complete programm command
    call get_command_argument(0, tmp_str)
	! convert to utf-8 if the local encoding is different
    tmp_str = fltu(tmp_str, error_str_conv)
    if (error_str_conv > 0) write(*,*) 'Warning, could not convert programm call string to utf-8'

    write(*,*) 'command_line = ', trim(tmp_str)
    work_path = ' '
    if(len_trim(tmp_str) > 0) then
        i1 = index(tmp_str, dir_sep, back=.true.)
        if(i1 > 0) work_path = tmp_str(1:i1)
    else
        write(*,*) "CRITICAL ERROR: could not find UR work path"
        stop
    end if

    ! if the work path is relativ, convert to an absolute path
    if (g_path_is_absolute(work_path) == 0) then
        work_path = flfu(actpath) // work_path(3:)
    end if
    write(*,*) 'work_path = ', work_path

    ! change the current path to the work path.
    i1 = g_chdir(work_path //c_null_char)
    if (i1 /= 0) then
        write(*,*) "CRITICAL ERROR: could not change current dir to work path"
        stop 3
    end if

    ! get the (relative) log path from config file
    call read_config('log_path', log_path, work_path // UR2_cfg_file)
    log_path = work_path // log_path
    call StrReplace(log_path, '/', dir_sep, .TRUE., .FALSE.)
    write(*,*) 'log_path = ', log_path

    ! get the (relative) results path from config file
    call read_config('results_path', results_path, work_path // UR2_cfg_file)
    results_path = work_path // results_path
    call StrReplace(results_path, '/', dir_sep, .TRUE., .FALSE.)
    write(*,*) 'results_path = ', results_path

    ! get the (relative) help path from config file
    call read_config('Help_path', help_path, work_path // UR2_cfg_file)
    help_path = work_path // help_path
    call StrReplace(help_path, '/', dir_sep, .TRUE., .FALSE.)
    write(*,*) 'help_path = ', help_path

    ! get the (relative) example path from config file
    call read_config('example_path', example_path, work_path // UR2_cfg_file)
    example_path = work_path // example_path
    call StrReplace(example_path, '/', dir_sep, .TRUE., .FALSE.)
    write(*,*) 'example_path = ', example_path
    write(*,*) ''

    ! open UR2 log files
    open(66,file=flfu(log_path) // "Fort66.txt", iostat=ios)
    open(65,file=flfu(log_path) // "Fort65.txt")
    open(67,file=flfu(log_path) // "Fort67.txt")
    open(55,file=flfu(log_path) // "Fort55.txt")
    open(30,file=flfu(log_path) // "Fort30.txt")
    open(23,file=flfu(log_path) // "Fort23.txt")
    open(166,file=flfu(log_path) // "Fort166.txt")
    open(15,file=flfu(log_path) // "Fort15.txt")

	! initiate gtk to show show gui error-messages
    call gtk_init()

    NBcurrentPage = 0
    callBatest = .false.
    runauto = .false.
    automode = .false.
    Excel_langg = ''
    langg = ''

    progstart_on = .true.

    call get_environment_variable("LANG", flang)
    if( len_trim(flang) > 0) then
        langg = flang(4:5)
    else
        ! set a dummy language, atm german
        langg = 'DE'
        write(66,*) 'Warning: $LANG not defined, falling back to: ' // langg
    endif
    write(66,*) 'Language before reading UR2_cfg: ', langg

    ifehl = 0
    glade_org = .false.

    ! check Glade file:
    inquire(file=work_path // gladeorg_file, exist=lexist)
    write(66,*) 'gladefile=',work_path // gladeorg_file
    if(lexist) then
        call stat(trim(work_path // gladeorg_file), finfo)
        glade_org = .true.
    end if

    if(.not. glade_org) then
        write(66,*) 'No Glade file found!'
        call quit_uncertradio(4)
    end if

    contrast_mode_at_start = .false.

    ! read the config file (UR2_cfg.dat)
    call Read_CFG()
    ! Test for an already running instance of UR2; if so, don't start a second one.
    ! and stop UR with errorcode 2
    call check_if_running(work_path // lockFileName, ur_runs)
    if(ur_runs) then
        write(*,*) 'An UR2 instance is already running! A second one is not allowed!'
        write(*,*) tmp_str
        IF(langg == 'DE') tmp_str = 'Es läuft bereits eine UR2-Instanz! Eine Zweite ist nicht erlaubt! Sollte dies ein Fehler sein, bitte löschen Sie die Datei: ' // work_path // lockFileName
        if(langg == 'EN') tmp_str = 'An UR2 instance is already running! A second one is not allowed! If this is an error, please delete the file: ' // work_path // lockFileName
        IF(langg == 'FR') tmp_str = 'Une instance UR2 est déjà en cours d''exécution! Une seconde n''est pas autorisée! S''il s''agit d''une erreur, veuillez supprimer le fichier: ' // work_path // lockFileName
        call MessageShow(trim(tmp_str)//'  ', GTK_BUTTONS_OK, "Warning", resp, mtype=GTK_MESSAGE_WARNING)
        call quit_uncertradio(2)
    end if

    if(contrast_mode) contrast_mode_at_start = .true.

    call monitor_coordinates()

    if(ifehl == 1) then
        call gtk_main_quit()
        call quit_uncertradio(3)
    end if

    call DefColors()

    prout_gldsys = .false.                 !  <---  nach gui_UR_main verlegt!

    call cpu_time(start)

    call create_window(UR_win, ifehl)

    if(ifehl == 1) then
        write(66,*) "Create window NOT successful!"
        call quit_uncertradio(3)
    end if
    call cpu_time(finish)

    write(66,'(A, F0.2, A)') " Create window1 successful!  cpu-time: ", finish - start, " s"

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
            if (error_str_conv > 0) write(*,*) 'Warning, could not convert command ' // &
                                                'line argument string to utf-8: ' // trim(tmp_str)
            write(66,*) 'CmdLine-Argument ',i,' : ', cgetarg(i)%s
        end do

        if (ncomargs == 3) then
            write(66,*) 'fname_getarg=', ucase(cgetarg(1)%s)

            if(ucase(cgetarg(1)%s) == 'AUTO' .or. ucase(cgetarg(1)%s) == 'AUTOSEP') then
                autoreport = .true.
                runauto = .true.
                automode = .true.
                fname_getarg = cgetarg(2)%s
                sample_ID = cgetarg(3)%s
                call StrReplace(fname_getarg, '/', dir_sep, .TRUE., .FALSE.)

                call check_cargs(ncomargs, sample_ID)
                if(ifehl == 1) call quit_UncertRadio(3)

                if(automode .and. len_trim(Excel_langg) == 2) then
                    sDecimalPoint = Excel_sDecimalPoint
                    sListSeparator = Excel_sListSeparator
                    write(66,'(a,a,a,a,a,a)') 'UR2 called from Excel:  language=',langg,'  sDecimalPoint=',sDecimalPoint, &
                        '  sListSeparator=',sListSeparator
                end if
            end if
        else if (ncomargs == 4) then
            if (ucase(cgetarg(1)%s) == 'BATSER') THEN
                autoreport = .true.
                runbatser = .true.
                automode = .true.
                fname_getarg = cgetarg(2)%s
                serial_csvinput = cgetarg(3)%s
                sample_ID = cgetarg(4)%s

                call StrReplace(fname_getarg, '/', dir_sep, .TRUE., .FALSE.)
                call StrReplace(serial_csvinput, '/', dir_sep, .TRUE., .FALSE.)
                base_project_SE = fname_getarg

                write(0,*) 'fname_getarg=',trim(fname_getarg),'  serial_csvinput=',trim(serial_csvinput)

                call check_cargs(ncomargs, sample_ID)
                if(ifehl == 1) call quit_UncertRadio(3)

                if(automode .and. len_trim(Excel_langg) == 2) then
                    sDecimalPoint = Excel_sDecimalPoint
                    sListSeparator = Excel_sListSeparator
                    write(66,'(a,a,a,a,a,a)') 'UR2 called from Excel:  language=',langg,'  sDecimalPoint=',sDecimalPoint, &
                        '  sListSeparator=',sListSeparator
                end if
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
                write(66,*) 'iosargument: ', trim(fname_getarg)
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
        write(66,'(a,2I5)') '***  Main window: first Show:  upper-left pos: mposx,mposy=',mposx,mposy
        call gtk_window_move(idpt('window1'),mposx,mposy)

        monitor_at_point = gdk_screen_get_monitor_at_point(gscreen,mposx+10_c_int,mposy+10_c_int)+1_c_int
        write(66,'(a,I5)') '***  Main window: Monitor# at mposx+10,mposy+10= ',monitor_at_point

    end if

    call gtk_widget_get_allocation(idpt('window1'),c_loc(alloc))
    write(66,'(a,i0,a,i0)') '***  Main window:  width= ',alloc%width,'  height= ',alloc%height

    write(66,'(a)') '------------------------------------------------------------------------------'

    !call testP2G()
    !return

    ! call Reconstr()
    ! return


    ! With simul_ProSetup = .true., it is tested to load that project file given
    ! under fileToSimulate in such a way, as if the user would proceed if he
    ! would set up the projet for the first time.
    ! However, the user interaction is not necessary: the equations, e.g., are taken
    ! from the project file and transferred to the corresponding textview. Similarly,
    ! other data read from the project file are transferred to the treeviews. In this
    ! way, the proper functioning of the sequence of buttons is tested, which are
    ! to be "clicked internally" on the way to the final result table.
    !
    ! To start this test, set simul_ProSetup = .true., recompile the program; then start
    ! uncertRadio.exe and load any project file, which means that actually
    ! the file identified by fileToSimulate is laoded.
    !
    ! GK: this test with its example projects was repeated and updated on 21.-22.9.2023.

    simul_ProSetup = .false.
    ! simul_ProSetup = .true.       ! set to .true. for testing

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

    write(0,*) 'Main:  after show_window:   MonitorUR=',int(MonitorUR,2)
    write(66,*) 'Main:  after show_window:   MonitorUR=',int(MonitorUR,2)

    batest_on = .false.
    if(NBcurrentPage == 0) NBcurrentPage = 1
    winPL_shown = .false.

    bat_serial = .false.
    bat_mc = .false.
    call gtk_widget_set_sensitive(idpt('SerialEval'), 1_c_int)

    langgSV = langg

    !-----------------------------------------------------------

    if(callBatest) then
        call batest()
    elseif(runauto) then
        call pending_events()
        call AutoReportWrite()
    elseif(runbatser) then
        call pending_events()
        bat_serial = .true.
        kfrom_se = 1
        kto_se = 1000
        call Batch_proc()
    else
        write(*,*) 'Main:  before call gtk_main()'
        item_setintern = .false.
        item_setintern_window1 = .false.         ! 16.8.2023
        call gtk_main()
        write(*,*) 'Main:  after call gtk_main()'
    end if

    !-----------------------------------------------------------
    ! stop UncertRadio correctly
    call quit_uncertradio(0)

end program UncertRadio


!------------------------------------------------------------------------------!
subroutine quit_uncertradio(error_code)
	use, intrinsic :: iso_c_binding, only : c_null_char

    use UR_VARIABLES,             only: work_path, actpath
    use UR_params,                only: lockFileName

    use UR_Gleich,                only: ifehl
    use UR_gtk_variables,         only: runauto
    use g,                        only: g_chdir

    implicit none
    integer, intent(in)           :: error_code
    logical                       :: exists
    integer                       :: stat, nio

    ! possible error_codes are:
    ! 0: everything is fine
    ! 1: not specified atm
    ! 2: UR is already running
    ! 3: ifehl == 1
    ! 4: there is an error with the glade file

    ! Remove the lock file if specified
    if (error_code /= 2) then
        inquire(file=work_path // lockFileName, exist=exists)
        if (exists) then
            ! The lock file exists, so remove it
            open(file=work_path // lockFileName, newunit=nio, iostat=stat)
            if (stat == 0) close(nio, status='delete', iostat=stat)
        endif
    endif
    if (error_code > 0) then
        write(*,*) 'Warning: Stoping UR with errorcode: ', error_code
        write(66,*) 'Warning: Stoping UR with errorcode: ', error_code
    end if

    ! change the current path back to the current path, when UR was started.
    stat = g_chdir(actpath // c_null_char)
	if (stat /= 0) then
		write(*,'(A)') ' Warning: Could not revert the curr_dir '
		write(66,'(A)') ' Warning: Could not revert the curr_dir '
	end if

    ! Close open log files
    close(30)
    close(55)
    close(65)
    ! Write log messages and perform necessary cleanup
    write(66, *) 'runauto=', runauto, ' ifehl=', ifehl
    write(66,'(A, I0)') ' UR2 terminated with errorcode: ', error_code

	close(66)
    ! Terminate the program showing the error_code

    stop error_code
end subroutine quit_uncertradio


!------------------------------------------------------------------------------!
subroutine check_if_running(lockFileName, ur_runs)

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

    implicit none

    character(len=*), intent(in)   :: lockFileName
    integer                        :: nio, iostat
    logical, intent(out)           :: ur_runs

    ! Attempt to open the lock file for exclusive access

    open(newunit=nio, &
        file=lockFileName, &
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

    use, intrinsic :: iso_c_binding, only: c_int, &
        c_ptr, &
        c_loc, &
        c_f_pointer, &
        c_associated, &
        c_char, &
        c_size_t, &
        c_null_ptr

    use gdk,              only: gdk_display_get_default, &
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

    use UR_VARIABLES,     only: langg

    implicit none

    integer                     :: monisel, &
        ios, &
        nprim, &
        tmon, &
        tmonx

    integer(c_int)              :: nmonit, atmonx
    type(GdkRectangle),pointer  :: URgdkRect
    type(c_ptr), target         :: cgdkrect
    logical                     :: m0out
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
    gscreen = gdk_display_get_default_screen (display)

    nmonit = max(0_c_int, gdk_screen_get_n_monitors(gscreen))
    tmonx = nmonit
    write(*,*) 'number of monitors:',int(tmonx,2)
    write(66,'(a,i0,a,i0)') 'number of monitors:',int(tmonx,2),'   nmonit=',nmonit
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
        if(tmon == 1) write(66,'(a)') '***  Monitors:'
        call gdk_monitor_get_geometry(gdk_display_get_monitor(display,tmon - 1_c_int), c_loc(cGdkRect))        !
        call c_f_pointer(c_loc(cGdkRect), URGdkRect)

        if(m0out) then
            write(0,'(a,i2,a,4I6)') 'tmon=',tmon,'  URGdkRect=',URGdkRect%x,URGdkRect%y, &
                URGdkRect%width,URGdkRect%height
            write(66,'(a,i2,a,4I6)') 'tmon=',tmon,'  URGdkRect=',URGdkRect%x,URGdkRect%y, &
                URGdkRect%width,URGdkRect%height
        endif

        widthmin(tmon) = URGdkRect%x
        heightmin(tmon) = URGdkRect%y
        widthmax(tmon) = URGdkRect%x + URGdkRect%width
        heightmax(tmon) = URGdkRect%y + URGdkRect%height

    end do
    ! call gdk_monitor_get_workarea(monitorx, c_loc(cGdkRect))        !
    ! call c_f_pointer(c_loc(cGdkRect), URGdkRect)

    write(66,'(/,a,i0)') '***  Monitor number selected as given in UR2_cfg.dat: ',monitorUR
    nprim = gdk_screen_get_primary_monitor(gscreen)+0_c_int
    monisel = 1
    if(.false. .and. nmonit+1_c_int > 0) then
        write(6,*)
        write(6,*) '########################################'
        if(langg == 'DE') write(6,'(a,i0,a)') 'Der Screen besteht aus ',nmonit+1_c_int,' Monitoren!'
        if(langg == 'EN') write(6,'(a,i0,a)') 'The screen consists of ',nmonit+1_c_int,' monitors!'
        if(langg == 'FR') write(6,'(a,i0,a)') 'L''écran est composé de ',nmonit+1_c_int,' moniteurs!'

        write(66,'(a,i0)') '***  Primary monitor # = ', nprim ! 23.3.2020

        if(langg == 'DE') write(6,'(a,i0)') 'Primärer Monitor # = ', nprim     ! 23.3.2020
        if(langg == 'EN') write(6,'(a,i0)') 'Primary monitor # = ', nprim     !
        if(langg == 'FR') write(6,'(a,i0)') 'Moniteur principal # = ', nprim  !

        if(monitorUR <= 0) then
            if(langg == 'DE') write(6,'(a)') '   Eingabe der Nummer des zu verwendenden Monitors: '
            if(langg == 'EN') write(6,'(a)') '   Enter the monitor# to work with: '
            if(langg == 'FR') write(6,'(a)') '   Entrez le numéro de moniteur avec lequel travailler: '
            read(5,*,iostat=ios) monisel
            ! if(ios /= 0 .and. monisel > 0) then
            if(ios == 0) then
                monitorUR = monisel
                write(66,'(a,i0)') '***  direct input of monitorUR (monisel)= ',monitorUR
            end if
        end if
    end if

    atmonx = max(0_c_int, monitorUR - 0_c_int)
    tmon = atmonx + 0_c_int

    scrwidth_min = widthmin(tmon) + 1
    scrwidth_max = widthmax(tmon) - 1
    scrheight_min = heightmin(tmon) + 2
    scrheight_max = heightmax(tmon) - int(0.032_rn*real(heightmax(tmon)-heightmin(tmon), rn) + 0.4999_rn)

    write(66,'(a,i0,2(a,i0,a,i0))') '***  Selected monitor: ',monitorUR,'; Screen min-max horiz.: ',  &
        scrwidth_min,' - ',scrwidth_max,'  min-max vertical: ',scrheight_min,' - ',scrheight_max

    return
    !---------------------------------------------------------------------------------

end subroutine monitor_coordinates


subroutine DefColors()

    use UR_gtk_variables, only: contrast_mode, &
                                entry_bg,entry_fg, &
                                entry_mark_bg, &
                                entry_mark_fg, &
                                label_bg,label_fg, &
                                frame_bg,frame_fg, &
                                green_bg,orange_bg, &
                                table_bg
    implicit none

    entry_bg = "#FFFFEC"
    entry_fg = "#000000"
    entry_mark_bg = "#FFFFFF"
    entry_mark_fg = "#000000"
    label_fg = "#000000"
    label_bg = "#FFFFFF"
    frame_bg = "#FFFFFF"
    frame_fg = "#000000"
    green_bg = "#00FF48"
    orange_bg = "#F57900"
    table_bg = "#FFFFFF"

    if(contrast_mode) then
        entry_bg = "#000000"
        entry_fg = "#FFFFFF"
        entry_mark_bg = "#000000"
        entry_mark_fg = "#FFFFFF"
        label_fg = "#FFFFFF"
        label_bg = "#000000"
        frame_bg = "#1D1D1D"
        frame_fg = "#A1E1FF"
        green_bg = "#0000d5"
        orange_bg = "#B54900"
        table_bg = "#252525"
    end if

end subroutine DefColors

!#############################################################################

subroutine check_cargs(ncomargs, sample_ID)

    !   Copyright (C) 2020-2023  Günter Kanisch

    use, intrinsic :: iso_c_binding,  only: c_int, c_null_char
    use ur_variables, only: Excel_langg, &
                            langg, &
                            Excel_sDecimalPoint, &
                            Excel_sListSeparator, &
                            cgetarg
    use UR_Gleich, only: ifehl

    implicit none

    integer   ,intent(in)        :: ncomargs
    character(len=*),intent(in)  :: sample_ID

    integer              :: nLC

    if(index(sample_ID,'LC=') == 1) then
        if(len_trim(sample_ID) < 7) then
            ifehl = 1
            write(66,*) 'The command string argument  ',sample_ID,' is incomplete!'
            return
        end if
        Excel_langg = sample_ID(4:5)
        langg = Excel_langg
        Excel_sDecimalPoint = sample_ID(6:6)
        Excel_sListSeparator = sample_ID(7:7)
    elseif(ncomargs >= 3) then
        ! write(66,*) 'cgetarg(3)=',trim(cgetarg(3)%s)
        if(index(cgetarg(3)%s,'LC=') == 1) nLC = 3          ! nLC introduced 2021-11-23
        if(ncomargs > 3) then
            write(66,*) 'cgetarg(4)=',trim(cgetarg(4)%s)
            if(index(cgetarg(4)%s,'LC=') == 1) nLC = 4
        end if
        if(len_trim(cgetarg(nLC)%s) < 7) then
            ifehl = 1
            write(66,*) 'The command string argument ',cgetarg(nLC)%s,' is incomplete!'
            return
        end if
        Excel_langg = cgetarg(nLC)%s(4:5)
        langg = Excel_langg
        Excel_sDecimalPoint = cgetarg(nLC)%s(6:6)
        Excel_sListSeparator = cgetarg(nLC)%s(7:7)
    end if

end subroutine check_cargs
