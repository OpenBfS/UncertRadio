

          !      contains:
          ! gui_main
          ! FNopen
          ! FNclose
          ! checkStart
          ! monitor_coordinates
          ! FindMonitorRect
          ! xy_scalef
          ! DefColors
          ! Glade_modify
          ! check_cargs

program gui_main

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
  !
  !   Copyright (C) 2014-2023  Günter Kanisch

  use, intrinsic :: iso_c_binding
  use iso_fortran_env,    only: error_unit,compiler_version,output_unit
  use gtk,                only: gtk_init, gtk_main, gtk_events_pending, gtk_main_iteration_do, &
                                gtk_main_quit, gtk_widget_set_visible,gtk_buttons_OK, &
                                gtk_main_iteration,gtk_widget_get_allocation, &
                                gtk_window_resize,FALSE,GTK_MESSAGE_WARNING, &
                                gtk_window_resize_to_geometry,gtk_window_reshow_with_initial_size, &
                                gtk_widget_size_allocate_with_baseline,gtk_about_dialog_new, &
                                gtk_window_set_default_size,gtk_widget_set_size_request, &
                                gtk_widget_get_preferred_height_and_baseline_for_width,gtk_widget_show, &
                                gtk_widget_set_sensitive,gtk_window_move, &
                                gtk_window_get_position,gtk_window_get_screen

  use gdk,                only: gdk_window_resize,gdk_screen_get_n_monitors,gdk_screen_get_default, &
                                gdk_screen_get_monitor_geometry,gdk_screen_get_primary_monitor, &
                                gdk_display_get_monitor,gdk_display_get_default_screen,gdk_monitor_get_workarea, &
                                gdk_display_get_default,gdk_screen_get_monitor_at_point, &
                                gdk_display_get_primary_monitor,gdk_display_get_monitor_at_window, &
                                gdk_screen_get_monitor_at_window,gdk_window_new,gdk_monitor_get_display, &
                                gdk_screen_width,gdk_screen_height
  use gtk_sup
  use gui_functions,      only: idpt,create_window,show_window
  use UR_gtk_variables,   only: UR_win, gladeorg_file,gladedec_file,time_gladeorg,time_gladedec,glade_org,glade_dec, &
                                item_setintern,screenh,screenw,runauto,winPL_shown,prout_gldsys,  &
                                scrwidth_min,scrwidth_max,scrheight_min,scrheight_max,monitorUR,gscreen, &
                                monitor_at_point,runbatser,contrast_mode,contrast_mode_at_start, &
                                item_setintern_window1
  use UR_variables,       only: callBatest,automode,fname_getarg,work_path,work_path_unix,work_path_getarg, &
                                langg, wpunix,batest_on,actpath, Excel_langg,ierrunit,  &
                                autoreport,fname,Sample_ID,plplot_copied,&
                                Excel_sDecimalPoint,Excel_sListSeparator,sDecimalPoint,sListSeparator, &
                                Michel_opt1,GTKpath,Batest_out,Batest_ref_file, &
                                bat_serial,bat_mc,langgSV,serial_csvinput, &
                                base_project_SE,kfrom_SE,kto_SE,cgetarg,progstart_on,simul_ProSetup, &
                                done_simul_ProSetup,open_project_parts, DirectorySeparator, UR_GIT_Version

  use g,                  only: g_settings_schema_source_new_from_directory, g_settings_new_with_path, &
                                g_settings_schema_source_get_default, g_settings_list_schemas, &
                                g_settings_schema_get_id,g_file_parse_name,g_get_current_dir, &
                                g_file_copy,g_file_delete,g_getenv,g_setenv

  use Rout,               only: MessageShow,pending_events,WDNotebookSetCurrPage
  use Usub3,              only: AutoReportWrite
  use UR_interfaces,      only: ProcessLoadPro_new
  use CHF,                only: FLTU,ucase
  use gtk_draw_hl,        only: gtkallocation
  use UR_Loadsel,         only: NBcurrentPage
  use Top,                only: LFU,LTU,CharModA1
  use urInit,             only: READ_CFG
  use UR_Gleich,          only: ifehl,charv
  ! use keyfB
  use UR_params,          only: rn
  use Brandt,             only: glngam
  use Num1,               only: quick_sort2_i
  use Rnd,                only: Rndu

  use fparser,        only: initf,parsef,evalf

  implicit none

  integer(4)                 :: ncomargs,i,iglen,istat,i1,Larg1,elength,estatus,i2
  integer(4)                 :: itask,n66
  character(len=110)         :: evalue
  character(:),allocatable   :: str1,str2
  character(len=2)           :: ceunit
  real(rn)                   :: start,finish
  integer(c_int)             :: resp,mposx,mposy
  type(c_ptr)                :: cdir_ptr
  type(gtkallocation), target  :: alloc

  integer(4)                 :: finfo(13),ios
  logical                    :: lexist
  character(:),allocatable   :: currdir,text,textG,cmdstring,pfile(:),f300
  character(len=270),pointer :: fpath
  character(len=5),pointer   :: flang
  character(:),allocatable   :: wflang,syspath
  character(:),allocatable   :: pltvar
  type(c_ptr)                :: pathptr,langptr
  character(len=360),allocatable :: f66(:)
  !--------------------------------------------------------------------------------------

  allocate(character(len=460)  :: currdir,text,textG,cmdstring,pfile(4))
  allocate(character(len=2000) :: pltvar,syspath)
  allocate(character(len=1000) :: fname_getarg)
  allocate(character(len=300)  :: f300)
  allocate(character(len=300)  :: str1,str2)
  allocate(f66(200))

  n66 = 0

  work_path = ' '
  work_path_unix= ' '

  ! Check the os; i think atm the convinient way to do this is to use
  ! the is_UNIX_OS function from gtk_sup
  wpunix = is_UNIX_OS()
  if (wpunix) then
      DirectorySeparator = '/'
      write(*,*) 'Operating System: Linux'
  else
      DirectorySeparator = '\'
      write(*,*) 'Operating System: Windows'
  endif

#ifdef GITHASH
    UR_GIT_Version = GITHASH
    write(*,*) "Git Version: "//trim(UR_GIT_Version)
#endif

  NBcurrentPage = 0
  callBatest = .false.
  runauto = .false.
  automode = .false.
  Excel_langg = ''
  langg = ''

  resp = g_setenv('GFORTRAN_UNBUFFERED_ALL'//c_null_char,'Y'//c_null_char, 1_c_int)

  progstart_on = .true.

  ! get the complete programm call
  call get_command(str1)
  n66 = n66 + 1
  write(f66(n66),*) trim(str1)

  ! get the number of given command arguments
  ncomargs = command_argument_count()
  allocate(cgetarg(ncomargs))
  call CharModA1(cgetarg,ncomargs)
  do i=1,ncomargs
    cgetarg(i)%s = f300
  end do

  n66 = n66 + 1
  write(f66(n66),*) 'ncomargs=', ncomargs
  if(ncomargs > 0) then
    do i=1, ncomargs
      n66 = n66 + 1
      call get_command_argument(i,cgetarg(i)%s)
      write(f66(n66),*) 'i=',int(i,2),cgetarg(i)%s
      if(i == 1 .and. TRIM(ucase(cgetarg(1)%s)) == 'AUTO' .or. TRIM(ucase(cgetarg(1)%s)) == 'AUTOSEP') THEN
        autoreport = .true.
        runauto = .true.
        automode = .true.
      end if
      ! when called from within the shiped Excel file (UR2_SingleAutoRun.xlsm) the third argument
      ! specifies the language, decimal separator and list separator settings Excel is using
      ! should be 'LC=DE,;' in Germany
      if(automode .and. ncomargs >= 3) then
        i1 = index(ucase(cgetarg(i)%s),'LC=')
        if(i1 > 0) then
          Excel_langg = ucase(cgetarg(i)%s(4:5))
          Excel_sDecimalPoint = cgetarg(i)%s(6:6)
          Excel_sListSeparator = cgetarg(i)%s(7:7)
          langg = Excel_langg
          sDecimalPoint = Excel_sDecimalPoint
          sListSeparator = Excel_sListSeparator
          n66 = n66 + 1
          write(f66(n66),*) 'langg=',Excel_langg,' sdeci=',Excel_sDecimalPoint,' sList=',Excel_sListSeparator
          if(langg == 'DE') resp = g_setenv('LANG'//c_null_char,'de_DE'//c_null_char, 1_c_int)
          if(langg == 'EN') resp = g_setenv('LANG'//c_null_char,'en_EN'//c_null_char, 1_c_int)
          if(langg == 'FR') resp = g_setenv('LANG'//c_null_char,'fr_FR'//c_null_char, 1_c_int)
        end if
      end if
    end do
  end if

  work_path = ' '
  if(len_trim(str1) > 0) then
    do i=len_trim(str1),1,-1
      if(str1(i:i) == '\') then
        work_path = str1(1:i)
        exit
      end if
    end do
    str2 = trim(ucase(work_path))
    i1 = index(str2,'UNCERTRADIO.EXE ')
    if(i1 > 0) then
      work_path = work_path(1:i1-1)
    else
      i1 = index(str2,'UNCERTRADIO ')
      if(i1 > 0) work_path = work_path(1:i1-1)
    end if

    if(len_trim(work_path) > 0) then
      n66 = n66 + 1
      write(f66(n66),*) 'work_path=',trim(work_path)
      work_path_getarg = work_path
      cdir_ptr = g_get_current_dir()
      call convert_c_string(cdir_ptr,currdir)
      currdir = FLTU(currdir)

    end if
    if(len_trim(work_path) == 0) then
      cdir_ptr = g_get_current_dir()
      call convert_c_string(cdir_ptr, currdir)
      currdir = FLTU(currdir)
      work_path = trim(currdir)//DirectorySeparator
      n66 = n66 + 1
      write(f66(n66),*) 'work_path=',trim(work_path)
      work_path_getarg = work_path
    end if
  end if


  i1 = 0
  i2 = 0
! Flo: deprecated
!  if(.true.) then
!    call get_environment_variable('PATH', pltvar, status=ios)
!    pltvar = ucase(pltvar)
!    i1 = index(pltvar,'\GTKUSER64')
!    if(i1 == 0) then
!      n66 = n66 + 1
!      write(f66(n66),*) '   Warning: The GTKuser64-related entry was not found in the  Windows'
!      n66 = n66 + 1
!      write(f66(n66),*) '            environment variable PATH!    program aborted'
!    end if
!    do i=i1,1,-1
!      if(pltvar(i:i) == ';') then
!        i2 = i
!        exit
!      end if
!    end do
!    GTKpath = adjustL(pltvar(i2+1:i1))
!    n66 = n66 + 1
!    Write(f66(n66),*) 'GTK-path=',trim(GTKpath)

!    pathptr = g_getenv('PATH'//c_null_char)
!        call c_f_pointer(pathptr,fpath)
!  end if

 langptr = c_null_ptr
 langptr = g_getenv('LANG'//c_null_char)
 if(c_associated(langptr)) then
   call c_f_pointer(langptr, flang)
   n66 = n66 + 1
   write(f66(n66),*) 'flang (g_getenv)=',trim(flang)
   langg = flang(4:)
 else
  ! set a dummy language, atm german
  langg = 'DE'
  write(0,*) 'unsing dummy language: ' // langg
endif
allocate(character(len=30) :: wflang)
wflang = langg
!      call convert_c_string(ctxt,wflang)
write(0,*)  'a:   g_win32_getlocale: Locale=',trim(wflang),'  langg=',langg
!      if(len_trim(langg) == 0) langg = ucase(trim(wflang(4:)))
write(0,*)  'a:   langg=',langg
!   endif
!resp = g_setenv('PLPLOT_DRV_DIR'//c_null_char,   &
!             trim(GTKpath)//'GTKuser64\plplot-5.15.0\buildmingw64\install\lib\plplot5.15.0\drivers'//c_null_char, 1_c_int)    ! 16.5.2019
!resp = g_setenv('PLPLOT_LIB'//c_null_char,   &
!            trim(GTKpath)//'GTKuser64\plplot-5.15.0\buildmingw64\install\share\plplot5.15.0'//c_null_char, 1_c_int)    ! 16.5.2019
!resp = g_setenv('PLPLOT_HOME'//c_null_char,   &
!            trim(GTKpath)//'GTKuser64\plplot-5.15.0\buildmingw64\install'//c_null_char, 1_c_int)    ! 13.8.2022

! 4.6.2023: Flo: not used
!resp = g_setenv('XDG_DATA_HOME'//c_null_char,   &
!            trim(GTKpath)//'GTKuser64\share'//c_null_char, 1_c_int)
!resp = g_setenv('XDG_DATA_DIRS'//c_null_char,   &
!            trim(GTKpath)//'GTKuser64\share'//c_null_char, 1_c_int)
!  CALL get_environment_variable("XDG_DATA_HOME", evalue)
!     ! if(len_trim(evalue) > 0)
!    n66 = n66 + 1
!     write(f66(n66),*) 'Environ: XDG_DATA_HOME: ',trim(evalue)
!  CALL get_environment_variable("XDG_DATA_DIRS", evalue)
!     ! if(len_trim(evalue) > 0)
!    n66 = n66 + 1
!     write(f66(n66),*) 'Environ: XDG_DATA_DIRS: ',trim(evalue)

  Larg1 = 0
  if(ncomargs == 1) then
    ! for calling the evaluation by double-clicking the project file name
    call get_command_argument(1, fname_getarg)
    str2 = trim(ucase(fname_getarg))
    Larg1 = len_trim(str2)
  end if

  write(0,*) 'work_path=',trim(work_path)
  write(0,*) 'work_path_unix=',trim(work_path_unix)

  Plplot_copied = .false.

  !  call get_environment_variable('PLPLOT_DRV_DIR',pltvar,status=ios)
  !  call get_environment_variable('PLPLOT_LIB',pltvar,status=ios)

  call get_environment_variable('PATH',syspath,status=ios)

  n66 = n66 + 1
  write(f66(n66),*) 'Work_path=',trim(work_path),'        wpunix=',wpunix
  n66 = n66 + 1
  write(f66(n66),*) 'Work_path_getarg=',trim(work_path_getarg)

  actpath = work_path
  if(len_trim(actpath) == 0) actpath = work_path

  open(66,file=trim(actpath) // "Fort66.txt",iostat=ios)
  open(65,file=trim(actpath) // "Fort65.txt")
  open(67,file=trim(actpath) // "Fort67.txt")
  open(55,file=trim(actpath) // "Fort55.txt")
  open(30,file=trim(actpath) // "Fort30.txt")
  open(23,file=trim(actpath) // "Fort23.txt")
  open(166,file=trim(actpath) // "Fort166.txt")
  open(15,file=trim(actpath) // "Fort15.txt")

  do i=1,n66
    write(66,'(a)') trim(f66(i))
  end do
  write(66,*) '------------------------- fort.66 until here ---------------------------'
  write(66,*)
  deallocate(f66)

  write(66,*) 'Environ: path: ',trim(syspath)

  cmdstring = 'CHDIR ' // trim(work_path)
  write(66,*) 'cmdstring=',cmdstring
  str1 = ' '
  ! CALL EXECUTE_COMMAND_LINE(cmdstring, wait=.true., EXITSTAT=j, CMDSTAT=k,CMDMSG=str1)
  if(len_trim(str1) > 0) write(66,*) 'message=',trim(str1)
  ncomargs = command_argument_count()
  write(66,'(a,i0)') ' number comline-Args= ',ncomargs
  CALL get_environment_variable("GFORTRAN_SHOW_LOCUS", evalue,elength,estatus)
! if(elength > 0 .and. estatus == 0) then
    !if(len_trim(evalue) > 0)
  write(66,*) 'Environ: GFORTRAN_SHOW_LOCUS: ',trim(evalue)
  CALL get_environment_variable("GFORTRAN_ERROR_BACKTRACE", evalue)
     ! if(len_trim(evalue) > 0)
     write(66,*) 'Environ: GFORTRAN_ERROR_BACKTRACE: ',trim(evalue)
  CALL get_environment_variable("GFORTRAN_STDERR_UNIT", ceunit, length=iglen, status=istat)
      ios = 0
      write(66,'(2a,3(a,i3))') ' Environ: GFORTRAN_STDERR_UNIT: ',trim(ceunit), &
             ' constant: error_unit=',error_unit,' iglen=',iglen,' istat=',istat
  CALL get_environment_variable("TMPDIR", evalue)
     !if(len_trim(evalue) > 0)
     write(66,*) 'Environ: TMPDIR: ',trim(evalue)
  CALL get_environment_variable("GFORTRAN_UNBUFFERED_ALL", evalue)
     ! if(len_trim(evalue) > 0)
     write(66,*) 'GFORTRAN_UNBUFFERED_ALL: ',trim(evalue)

  ifehl = 0

  glade_org = .false.
  glade_dec = .false.

  ! Original Glade file:
  ! gladeorg_file = 'UR_338_V9.glade'   ! 2.4.2021
  ! gladeorg_file = 'UR_338_V10.glade'   ! 21.10.2021
  ! gladeorg_file = 'UR_338_V10a.glade'   ! 23.6.2022
  ! gladeorg_file = 'UR_338_V11.glade'   ! 27.7.2022
  ! gladeorg_file = 'UR_340_V12.glade'   !  3.12.2022
  gladeorg_file = 'UR_340_V12_DM.glade'   !  29.6.2023

  gladeorg_file = trim(work_path) // trim(gladeorg_file)
  Inquire(file=gladeorg_file, exist=lexist)
  if(lexist) then
    call STAT(trim(gladeorg_file),finfo)
    time_gladeorg = finfo(10)
    glade_org = .true.
  end if


  ! encrypted Glade file:
  gladedec_file = "Glade.dat"
  gladedec_file = trim(work_path) // trim(gladedec_file)
  Inquire(file=gladedec_file, exist=lexist)
  if(lexist) then
    call STAT(trim(gladedec_file),finfo)
    time_gladedec = finfo(10)
    glade_dec = .true.
  end if
  if(.not.glade_org .and. .not. glade_dec) then
    write(66,*) 'No Glade file found!'
    goto 9000
  end if

  call gtk_init()
  contrast_mode_at_start = .false.
  call Read_CFG()
  if(contrast_mode) contrast_mode_at_start = .true.

  write(66,'(a)') '------------------------------------------------------------------------------'
  write(66,'(a)') 'screen coordinates: width x height'
  screenw = gdk_screen_width()          ! encompasses several monitors
  screenh = gdk_screen_height()         !
  write(66,'(a,i0,a,i0)') '***  Screen: ',screenw,' x ',screenh

  call monitor_coordinates()
  if(ifehl == 1) then
    call gtk_main_quit()
    goto 9000
  end if

!    goto 55
! 55 continue

  langptr = g_getenv('LANG'//c_null_char)
  if(c_associated(langptr)) then
    call c_f_pointer(langptr,flang)
    flang = FLTU(flang)
    write(0,*) 'Language after set LANG: ',trim(flang)
  end if

  ! Test for an already running instance of UR2; if so, don't start a second one.
  ! call checkStart(itask) ! FO: not working this way under UNIX
  itask = 0
  if(itask > 1) then
    write(66,*) 'An UR2 instance is already running! A second one is not allowed!'
    IF(langg == 'DE') str1 = 'Es läuft bereits eine UR2-Instanz! Eine zweite ist nicht erlaubt!'
    if(langg == 'EN') str1 = 'An UR2 instance is already running! A second one is not allowed!'
    IF(langg == 'FR') str1 = 'Une instance UR2 est déjà en cours d''exécution! Une seconde n''est pas autorisée!'
    call MessageShow(trim(str1)//'  ', GTK_BUTTONS_OK, "Warning", resp,mtype=GTK_MESSAGE_WARNING)
    goto 9000
  end if

  call DefColors()

  prout_gldsys = .false.                 !  <---  nach gui_UR_main verlegt!


  call cpu_time(start)

  call create_window(UR_win, trim(gladeorg_file)//c_null_char, ifehl)

  if(ifehl == 1) then
    write(66,*) "Create window NOT successful!"
    goto 9000
  end if
  call cpu_time(finish)
              ! call pending_events()
  write(66,*) "Create window1 successful!  cpu-time: ",sngl(finish-start)  ! ,' cput (s)=',sngl(finish)

  call gtk_widget_set_visible(idpt('dialog_LoadPro'), False)

  !------------------------------------------------------------
  ncomargs = command_argument_count()
  write(66,'(a,i0)') ' number cmdline-Args= ',ncomargs
  if(ncomargs > 0) then
    do i=1,ncomargs
        if(allocated(fname_getarg)) deallocate(fname_getarg); allocate(character(len=500) :: fname_getarg)
      call get_command_argument(i,fname_getarg)
      cgetarg(i)%s = trim(fname_getarg)
      write(66,*) 'CmdLine-Argument ',i,' : ',trim(fname_getarg)
    end do
  end if
  if(allocated(fname_getarg)) deallocate(fname_getarg); allocate(character(len=500) :: fname_getarg)
  call get_command_argument(1,fname_getarg)
  if(allocated(sample_id)) deallocate(sample_id)
  allocate(character(len=100) :: sample_id)

  if (LEN_TRIM(fname_getarg) > 0) then
    str1 = trim(ucase(fname_getarg))
        write(66,*) 'fname_getarg=',trim(str1)
    if(TRIM(str1) == 'AUTO' .or. TRIM(str1) == 'AUTOSEP') then
      autoreport = .TRUE.
      runauto = .true.
      automode = .true.
      call get_command_argument(2, fname_getarg)
      call get_command_argument(3, sample_ID)
      do i=LEN_TRIM(fname_getarg),1,-1
        IF(fname_getarg(i:i) == '/') fname_getarg(i:i) = DirectorySeparator
      end do
      call check_cargs(ncomargs,sample_ID)
      if(ifehl == 1) goto 9000
      if(automode .and. len_trim(Excel_langg) == 2) then
        sDecimalPoint = Excel_sDecimalPoint
        sListSeparator = Excel_sListSeparator
        write(66,'(a,a,a,a,a,a)') 'UR2 called from Excel:  language=',langg,'  sDecimalPoint=',sDecimalPoint, &
                        '  sListSeparator=',sListSeparator
      end if
    elseif(TRIM(str1) == 'BATSER') THEN
      autoreport = .TRUE.
      runbatser = .true.
      automode = .true.
      call get_command_argument(2,fname_getarg)
      call get_command_argument(3,serial_csvinput)

      call get_command_argument(4,sample_ID)
      do i=LEN_TRIM(fname_getarg),1,-1
        IF(fname_getarg(i:i) == '/') fname_getarg(i:i) = '\'
      end do
      do i=LEN_TRIM(serial_csvinput),1,-1
        IF(serial_csvinput(i:i) == '/') serial_csvinput(i:i) = '\'
      end do
      base_project_SE = fname_getarg

      write(0,*) 'fname_getarg=',trim(fname_getarg),'  serial_csvinput=',trim(serial_csvinput)

      call check_cargs(ncomargs,sample_ID)
      if(ifehl == 1) goto 9000
      if(automode .and. len_trim(Excel_langg) == 2) then
        sDecimalPoint = Excel_sDecimalPoint
        sListSeparator = Excel_sListSeparator
        write(66,'(a,a,a,a,a,a)') 'UR2 called from Excel:  language=',langg,'  sDecimalPoint=',sDecimalPoint, &
                        '  sListSeparator=',sListSeparator
      end if
    else
      IF(INDEX(str1,'.TXP') == 0 .and. INDEX(str1,'.CSV') == 0) THEN
        IF(langg == 'EN') call MessageShow('The file is not a project file!', GTK_BUTTONS_OK, "Open Project:", &
                                                                                  resp,mtype=GTK_MESSAGE_WARNING)
        IF(langg == 'DE') call MessageShow('Diese Datei ist keine Projektdatei!', GTK_BUTTONS_OK, "Open Project:", &
                                                                                  resp,mtype=GTK_MESSAGE_WARNING)
        IF(langg == 'FR') call MessageShow('Ce fichier n''est pas un fichier de projet!', GTK_BUTTONS_OK, &
                                                                    "Projet ouvert:", resp,mtype=GTK_MESSAGE_WARNING)
        GOTO 9000
      end if
      fname = trim(fname_getarg)
                Write(66,*) 'IOSArgument: ',trim(fname_getarg)
      ifehl= 0
      call ProcessLoadPro_new(0,1)       ! Start calculations with the first output quantity
          call WDNotebookSetCurrPage('notebook1',5)
      NBcurrentPage = 5
    end if
  end if


  !-----------------------------------------------------------
                  ! write(*,*) 'Main:  before show_window'
  Michel_opt1 = .false.
  !Inquire(FILE=trim(work_path)//'Michelplot.txt',exist=Lexist)
  !  if(Lexist) Michel_opt1 = .true.

  if(.true.) then
    ! must be called here, i.e., before the first call of show_window
    call gtk_widget_get_allocation(idpt('window1'),c_loc(alloc))

    ! alloc%height = int(0.6_rn*real(scrheight_max-scrheight_min,rn))
    alloc%height = int(0.80_rn*real(scrheight_max-scrheight_min,rn))         ! 15.8.2023
    call gtk_widget_set_size_request(idpt('window1'),alloc%width,alloc%height)
    call gtk_window_resize(idpt('window1'),alloc%width,alloc%height)
  end if
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
    !  fileToSimulate = 'd:\gf_pros\ur24\pros\de\Ac228_binomial_V2_DE.txp'     ! R0 fehlt
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\DWD-LSC-3kanal-V2_DE.txp'        ! ok
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\en\La140_REMSPEC-4Lines-V3_EN.txp'  ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\NLWKN_Fe-55_mit_KALFIT_DE.txp'   ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\en\Mean-theta_EN.txp'               ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\en\Alpha-IAEA-1401-Kanisch_EN.txp'  ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\BSH_Gesamt-Gamma_var2_DE.txp'      ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\DWD_AB-Gesamt-Aeros-Alpha1_DE.txp'  ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\DWD_sr89_sr90_TDCR_Verfahren_V2_DE.txp' ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\en\Example_8_with_KALFIT_EN.txp'           ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\Fe-55-mit-LSC-und-Standardaddition_DE.TXP'  ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\Galpha_beta_Rusconi_2006_V2_DE.txp'   ! OK
    !  fileToSimulate = 'd:\gf_pros\ur24\pros\en\ISO-Example-2a_V2_EN.txp'       ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\ISO-Neutronen-Dosis_DE.txp'      ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\Janszen-Sr-89-Sr-90_V2_DE.txp'   ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\Michel-2000-b_DE.txp'            ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\en\Moreno-Sr90_IAEA-135_V2_EN.txp'  ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\en\PresetCounts_EN.txp'             ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\en\Ra226_U235-at-186keV_EN.txp'     ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\Rn-222-Emanation_DE.txp'         ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\en\sumEval_mean_V2_EN.txp'          ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\Tritium_4Bubbler_used_1-3_DE.txp'  ! OK
    ! fileToSimulate = 'd:\gf_pros\ur24\pros\de\J-ALUFT-Sr89-Sr-90_V2_DE.txp'      ! OK
  else
    done_simul_ProSetup = .true.
  end if

  write(0,*) 'Main:  after show_window:   MonitorUR=',int(MonitorUR,2)
  write(66,*) 'Main:  after show_window:   MonitorUR=',int(MonitorUR,2)

  batest_on = .false.
  if(NBcurrentPage == 0) NBcurrentPage = 1
  winPL_shown = .false.

  ! Batest_ref_file = 'BatListRef_v03.txt'   ! since 28.5.2020
  !Batest_ref_file = 'BatListRef_v04.txt'   ! since midth of August, 2021
  Batest_ref_file = 'BatListRef_v05.txt'   ! since about 2023-02-18
  Batest_out = 'vgltest.txt'

  bat_serial = .false.
  bat_mc = .false.
  call gtk_widget_set_sensitive(idpt('SerialEval'), 1_c_int)

  langgSV = langg

  !-----------------------------------------------------------
  deallocate(currdir,text,textG,cmdstring,pfile,pltvar)

  if(callBatest) then
    call batest()
    goto 9000
  elseif(runauto) then
    call pending_events()
    call AutoReportWrite()
    GOTO 9000
  elseif(runbatser) then
    call pending_events()
    bat_serial = .true.
    kfrom_se = 1
    kto_se = 1000
    call Batch_proc()
    GOTO 9000
  else
              write(0,*) 'Main:  before call gtk_main()'
    item_setintern = .false.
    item_setintern_window1 = .false.         ! 16.8.2023
    call gtk_main()
              write(0,*) 'Main:  after call gtk_main()'
  end if
  !-----------------------------------------------------------
9000   continue
  write(66,*) 'runauto=',runauto,' ifehl=',ifehl
  if(runauto .and. ifehl == 1) write(66,*) 'UR2 terminated with Exit(status)'

  ! restore original language for Windows:
  resp = g_setenv('LANG'//c_null_char, trim(wflang)//c_null_char, 1_c_int)

  close (66)
  close (30)
  close (55)
  close (65)


  if(itask == 1) then
    call FNclose (23)
    call FNclose (166)
    ! call FNclose (196)
    call FNclose (15)
    ! call FNclose (69)
    call FNclose (65)
    call FNclose (67)
  end if

  close (ierrunit)

  if(runauto .and. ifehl == 1) then
      write(66,*) 'UR2 terminated with Exit(status)'
    call Exit(status = 3)
  else
    stop
  end if

end program gui_main

!###############################################################################

subroutine heights

     ! estimates heights of some container widgets of GKT
     !   Copyright (C) 2020-2023  Günter Kanisch

use, intrinsic :: iso_c_binding,        only: c_loc,c_int,c_ptr,c_associated,c_f_pointer,c_char
use gtk,                  only: gtk_widget_get_allocation,gtk_widget_get_preferred_height, &
                                gtk_container_get_children

use g,                    only: g_list_nth_data,g_list_alloc,g_list_nth,g_list_find
use gtk_sup,              only: convert_c_string
use UR_gtk_variables,     only: clobj,nclobj
use gtk_draw_hl,          only: gtkallocation
use top,                  only: idpt

implicit none

integer(4)                   :: i,ifd
integer(c_int),target        :: phmin,phmax

 do i=1,nclobj
              !  exit
   ifd = 0
   if(index(clobj%idd(i)%s,'window1') == 1) ifd = 1
   if(index(clobj%idd(i)%s,'notebook') == 1) ifd = 1
   if(index(clobj%idd(i)%s,'box') == 1) ifd = 1
   if(index(clobj%idd(i)%s,'grid') == 1) ifd = 1
   if(index(clobj%idd(i)%s,'textv') == 1) ifd = 1
   if(index(clobj%idd(i)%s,'frame') == 1) ifd = 1
   if(index(clobj%idd(i)%s,'Frame') == 1) ifd = 1
   if(index(clobj%idd(i)%s,'align') == 1) ifd = 1
   if(ifd == 1) then
     call gtk_widget_get_preferred_height(clobj%id_ptr(i),c_loc(phmin),c_loc(phmax))
     write(66,'(a,i4,2x,i4,2x,a)') 'pref.height_min,max=',phmin,phmax,clobj%idd(i)%s
     write(0,'(a,i4,2x,i4,2x,a)') 'pref.height_min,max=',phmin,phmax,clobj%idd(i)%s
   end if
 end do

end subroutine heights

!#######################################################################

subroutine FNopen(fnum)

use UR_VARIABLES,     only: actpath,work_Path

implicit none

integer(4),intent(in)    :: fnum       ! number of I/O unit

integer(4)          :: ivals(13),ios
character(len=50)   :: fortname

if(len_trim(actpath) == 0) actpath = work_path
write(fortname,'(a,I0)') 'fort.', fnum
call stat(fortname,ivals,ios)
if(ios == 2) then
  open(166,file=trim(actpath) // "Fort" // trim(fortname(6:)) // ".txt")
end if

end subroutine FNopen

!#################################################################################

subroutine FNclose(fnum)

use UR_VARIABLES,     only: actpath,work_path

implicit none

integer(4),intent(in)    :: fnum       ! number of I/O unit

integer(4)          :: ivals(13),ios,j,k
character(len=150)  :: fortname,str1
character(len=255)  :: cmdstring
logical             :: prout

prout = .false.
 ! prout = .true.

close (fnum)
if(len_trim(actpath) == 0) actpath = work_path

write(fortname,'(a,I0,a)') 'fort', fnum,'.txt'
fortname = trim(actpath) // trim(fortname)
  if(prout) write(0,*) 'Fortname =' ,trim(fortname)

call stat(fortname,ivals,ios)
  if(prout) write(0,'(a,i0,13i8)') 'ios=',ios,ivals(8)
if(ios == 0 .and. ivals(8) == 0) then
  str1 = ''
  cmdstring = 'del ' // trim(fortname)
        if(prout) write(0,*) 'cmdstring=',trim(cmdstring)
  CALL EXECUTE_COMMAND_LINE(cmdstring, wait=.false., EXITSTAT=j, CMDSTAT=k,CMDMSG=str1)
    if(prout) write(0,*) ' EXITSTAT=',j,'  CMDSTAT=',k
   if(k /= 0 .and. len_trim(str1) > 0) write(66,*) '       Message=',trim(str1)
end if

end subroutine FNclose

!#################################################################################

subroutine checkStart(itask)

   ! checks if another UR instance is already running to prevent from
   ! running two instances of UR;

   !   Copyright (C) 2018-2023  Günter Kanisch

  use g,               only: g_file_get_contents,g_remove
  use, intrinsic :: iso_c_binding,   only: c_int,c_ptr,c_null_char,c_associated
  use CHF,             only: ucase
  use gui_functions,   only: c_f_string

  implicit none

  integer(4),intent(out)      :: itask    ! number of UR instances within Windows-TaskList,
                                          ! only 1 allowd

  integer(4)          :: i1,zt1(9),j,k
  character(len=20)   :: cdtime
  character(len=300)  :: str1,cmdstring
  character(len=500)  :: str3
  integer(c_int)      :: resp
  type(c_ptr)         :: clen
  type(c_ptr),target  :: contents
  type(c_ptr),target  :: cerror


  call date_and_time(values=zt1)                       !values=[year, month, day, gmt_min, hr,min,sec,msec]
  write(cdtime,*) zt1(7)*60 + zt1(6)
  cmdstring = 'TASKLIST /FI "imagename EQ uncertradio.exe" > taskx_' // trim(adjustL(cdtime)) // '.txt'
  CALL EXECUTE_COMMAND_LINE(cmdstring, wait=.true., EXITSTAT=j, CMDSTAT=k,CMDMSG=str1)
  itask = 0
  if(k /= 0) then
    write(66,*) trim(cmdstring),' ;      Message=',trim(str1)
  else
    resp = g_file_get_contents('taskx_' // trim(adjustL(cdtime)) // '.txt' // c_null_char,  &
                               contents, clen, cerror)
    itask = 0
    if(c_associated(contents)) then
      call c_f_string(contents,str3)
      str3 = ucase(str3)
      i1 = index(str3,'UNCERTRADIO')
      if(i1 > 0) then
        itask = itask + 1
        str3 = str3(i1+10:)
        i1 = index(str3,'UNCERTRADIO')
        if(i1 > 0) itask = itask + 1
      end if
    end if
    write(66,*) 'UR tasks: ',int(itask,2)
    write(0,*) 'UR tasks: ',int(itask,2)
    resp = g_remove('taskx_' // trim(adjustL(cdtime)) // '.txt' // c_null_char)
  end if

end subroutine checkStart

!#########################################################################

subroutine monitor_coordinates()

    ! finds the number of monitors combined within a screen;
    ! it then estimates the coordinates (height, width) of the monitor(s)
    !
    ! calls xy_scalef(), FindMonitorRect
    !
    ! See chapter 1.3 "Using several monitors" of the UncertRadio CHM Help
    ! file for more details.

    !   Copyright (C) 2020-2023  Günter Kanisch

  use UR_params,        only: rn

  use, intrinsic :: iso_c_binding,    only: c_int,c_ptr,c_loc,c_f_pointer,c_associated,c_char,c_size_t,c_null_ptr
  use gtk,              only: gtk_window_get_position,gtk_window_get_screen, &
                              gtk_menu_get_monitor
  use gdk,              only: gdk_display_get_default,gdk_display_get_default_screen, &
                              gdk_screen_get_n_monitors,gdk_screen_get_primary_monitor, &
                              gdk_screen_get_monitor_at_point,gdk_screen_get_monitor_at_window, &
                              gdk_display_get_monitor,gdk_monitor_get_workarea,gdk_monitor_get_display, &
                              gdk_display_get_monitor_at_window,gdk_screen_get_root_window, &
                              gdk_monitor_get_geometry,gdk_monitor_get_scale_factor

  use UR_gtk_variables, only: display,GdkRectangle,monitorUR,  &
                              scrwidth_min,scrwidth_max,scrheight_min,scrheight_max,monitor,gscreen, &
                              PixelxZoom,PixelyZoom
  use Top,              only: idpt
  use UR_Gleich,        only: ifehl
  use Rout,             only: MessageShow
  use gtk_sup,          only: c_f_string,convert_c_string
  use UR_VARIABLES,     only: langg
  use CHF,              only: FLTU

  implicit none

  integer(4)             :: monisel,ios,nprim,tmon,tmonx
  integer(c_int)         :: nmonit, atmonx
  type(GdkRectangle),pointer  :: URgdkRect
  type(c_ptr), target    :: cgdkrect
  logical                :: m0out
  integer, allocatable   :: widthmin(:), widthmax(:), heightmin(:), heightmax(:)

  ! GDK: a single GdkScreen combines several physical monitors.

  ifehl = 0
  allocate(URgdkRect)
  display = c_null_ptr
  gscreen = c_null_ptr
  display = gdk_display_get_default()
  gscreen = gdk_display_get_default_screen (display)

  nmonit = max(0_c_int, gdk_screen_get_n_monitors(gscreen))
  tmonx = nmonit
  write(0,'(a,i0)') 'number of monitors:',int(tmonx,2)
  write(66,'(a,i0,a,i0)') 'number of monitors:',int(tmonx,2),'   nmonit=',nmonit
  allocate(widthmin(nmonit), widthmax(nmonit), heightmin(nmonit), heightmax(nmonit))
  widthmin(:) = 0
  widthmax(:) = 0
  heightmin(:) = 0
  heightmax(:) = 0

  monitor = c_null_ptr
  monitor = gdk_display_get_monitor(display, nmonit)
   ! call xy_scalef() ! Was macht diese Funktion?? Keine Rückgabewerte, es wird nur eine txt
                      ! Datei erzeugt, die die Ausgabe der Pixelzahl pro Inch mit einer Windows eigenen
                      ! Routine abfragt? Notwendig? Die Datei wird wohl nicht weiter verwendet.


   ! Note: monitorx or monitor should be created only once; for further creations of the
   ! C-pointer monitorx with the GDK function, it must first be reset by monitorx = c_null_ptr;
   ! if not, GDK-critical warnings appear.

  m0out = .false.
  do tmon=1,tmonx
    if(tmon == 1) write(66,'(a)') '***  Monitors:'
      call gdk_monitor_get_geometry(gdk_display_get_monitor(display,tmon - 1_c_int), c_loc(cGdkRect))        !
      call c_f_pointer(c_loc(cGdkRect), URGdkRect)
      call FindMonitorRect(URgdkRect,PixelxZoom,PixelyZoom)
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
       ! if(langg == 'DE') write(0,'(a,i0)') trim(FLTU('Primäre Monitor # =')), nprim     ! 23.3.2020
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

!#########################################################################

subroutine FindMonitorRect(URgdkRect,xPixelpc,yPixelpc)

use UR_params,          only: rn
use UR_gtk_variables,   only: twidth,theight,xscalef,yscalef,GdkRectangle

implicit none

integer(4),intent(in)             :: xPixelpc,yPixelpc
type(GdkRectangle),intent(inout)  :: URgdkRect

integer(4)          :: i,j,mind,mindx,mindy,ttx,tty,ttw,tth,dummy
real(rn)            :: difminx,difminy,dratiox,dratioy
logical             :: x_fit

! A pixel density of 96 dpi is assumed, which means 96 pixels per inch.

xscalef = real(xPixelpc,rn)/96._rn
yscalef = real(yPixelpc,rn)/96._rn

! Note:
! When intent(out) is used with a derived type, any component not assigned in a procedure
! could become undefined on exit. For example, even though a%y was defined on entry to this
! routine, it could become undefined on exit because it was never assigned within the routine.
! The lesson is that all components of a derived type should be assigned within a procedure,
! when intent(out) is used. Intent(out) behaves like the result variable in a function: all
! components must be assigned.

!   Copyright (C) 2020-2023  Günter Kanisch

ttw = URgdkRect%width
tth = URgdkRect%height
ttx = URgdkRect%x
tty = URgdkRect%y

x_fit = .false.
do i=1,20
  if(ttw == twidth(i)) then
    do j=1,20
      if(tth == theight(j)) then
        x_fit = .true.
        exit
      end if
    end do
    if(x_fit) exit
  end if
end do
if(x_fit) then
  ! no modification necessary: the sizes agree with one of the given monitor sizes
  xscalef = 1._rn
  yscalef = 1._rn
end if

if(.not.x_fit) then
  difminx = 10000
  ! find the best width value given in twidth():
  do i=1,20
    dummy = int(real(ttw,rn)/xscalef + 0.4999_rn) - twidth(i)
    if(abs(dummy) < difminx) then
      difminx = abs(dummy)
      dratiox = real(ttw,rn)/xscalef / real(twidth(i),rn)
      mindx = i
      difminy = abs(int(real(tth,rn)/yscalef + 0.4999_rn) - theight(i))
      dratioy = real(tth,rn)/yscalef / real(theight(i),rn)
    end if
  end do
  mind = mindx
  ! find the best height value given in theight():
  mindy = 0
  do i=1,20
    if(i == mindx .or. twidth(mindx) /= twidth(i)) cycle
    dummy = int(real(tth,rn)/yscalef + 0.4999_rn) - theight(i)
    if(abs(dummy) < difminy) then
      difminy = abs(dummy)
      dratioy = real(tth,rn)/yscalef / real(theight(i),rn)
      mindy = i
    end if
  end do
  if(mindy > 0) mind = mindy
  if(.false.) then
    write(66,*) 'width anpassen: mind=',int(mind,2),' adopted width=',twidth(mind), &
                         '  dratiox=',sngl(dratiox)
    write(66,*) '  adopt. height=',int(theight(mind)),'  dratioy=',sngl(dratioy)
  end if
  ttw = twidth(mind)
  tth = theight(mind)
  ttx = int(real(ttx,rn)/xscalef +0.4999_rn)
  tty = int(real(tty,rn)/yscalef +0.4999_rn)

end if

URgdkRect%width = ttw
URgdkRect%height = tth
URgdkRect%x = ttx
URgdkRect%y = tty


end subroutine FindMonitorRect

!#########################################################################

subroutine xy_scalef()
use, intrinsic :: iso_c_binding,    only: c_int,c_null_char

use g,                only: g_remove
use UR_gtk_variables, only: PixelxZoom,PixelyZoom

implicit none

integer(4)          :: i,i1,j,ios,k
character(:),allocatable   :: text1,str1,cmdstring   ! geht nicht so einfach
character(len=1)    :: char1
integer(c_int)      :: resp

  ! Note:
  ! The produced file xyscalef77.txt has the UCS-2 LE-BOM coding:
  ! this requires a different way of reading, because of the many NUL characters in it.

  !   Copyright (C) 2020-2023  Günter Kanisch
 allocate(character(len=300) :: text1,str1,cmdstring)

 cmdstring = 'wmic desktopmonitor get PixelsPerXLogicalInch, PixelsPerYLogicalInch > xyscalef77.txt'
 CALL EXECUTE_COMMAND_LINE(cmdstring, wait=.true., EXITSTAT=j, CMDSTAT=k,CMDMSG=str1)

 if(k /= 0) then
   write(66,*) trim(cmdstring),' ;      Message=',trim(str1)
 else
   close(133)
   open(133,file='xyscalef77.txt',status='old',access='stream',iostat=ios)
   i = 0
   j = 0
   text1 = ' '
   do
     i = i + 1
     read(133,iostat=ios) char1
     if(ios /= 0) exit
     if(i > 2 .and. ichar(char1) > 9) then
       j = j + 1
       text1 = text1(1:j-1) // char1
     end if
   end do
   close(133)
     resp = g_remove('xyscalef77.txt'// c_null_char)

   i1 = index(text1,char(13)//char(10))
   read(text1(i1+2:),'(i5,18x,i7)') PixelxZoom,PixelyZoom
 end if

end subroutine xy_scalef

!#########################################################################


subroutine DefColors()

use UR_gtk_variables,   only: contrast_mode,entry_bg,entry_fg,entry_mark_bg,entry_mark_fg, &
                              label_bg,label_fg,frame_bg,frame_fg,green_bg,orange_bg, &
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

!#########################################################################


!#############################################################################

subroutine check_cargs(ncomargs, sample_ID)

!   Copyright (C) 2020-2023  Günter Kanisch

use, intrinsic :: iso_c_binding,  only: c_int,c_null_char
use UR_VARIABLES,        only: Excel_langg,langg,Excel_sDecimalPoint,Excel_sListSeparator, &
                               cgetarg
use UR_Gleich,           only: ifehl
use g,                   only: g_setenv

implicit none

integer(4),intent(in)  :: ncomargs
character(len=*),intent(in)  :: sample_ID

integer(4)           :: nLC
integer(c_int)       :: resp

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
      if(langg == 'DE') resp = g_setenv('LANG'//c_null_char,'de_DE'//c_null_char, 1_c_int)  ! 22.11.2020
      if(langg == 'EN') resp = g_setenv('LANG'//c_null_char,'en_EN'//c_null_char, 1_c_int)  ! 22.11.2020
      if(langg == 'FR') resp = g_setenv('LANG'//c_null_char,'fr_FR'//c_null_char, 1_c_int)  ! 22.11.2020

end subroutine check_cargs
