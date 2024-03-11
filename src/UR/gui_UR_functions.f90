
module gui_functions

    ! Note:  the Compiler message like "type(c_ptr) is defined ambiguous" is prevented
    !        by USE gtk_sup

    !   Copyright (C) 2014-2023  Günter Kanisch

    use, intrinsic :: iso_c_binding
    use gtk_sup
    use UR_gtk_window
    use top,      only: idpt,FindItemS,FindItemP
    use UR_gini

   ! private
    public :: window, create_window, show_window, lowcase, &
              UR_field_edit_cb,SetColors

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
       ! lowcase
       ! enable_locale_c
       ! c_f_string_chars

    ! what means in the routines:
    !      if(item_setintern) return   ?
    ! User actions, like a click to a certain notebook page can also be
    ! simulated by programming (ProcessLoadPor_new does a lot of this)), like
    !      call WDNotebookSetCurrPage('notebook1', 1)
    ! Such a call also triggers the notebook signal which calls then the
    ! correspondent callback routine. To prevent this calllback routine from
    ! full execution, the code in WDNotebookSetCurrPage contains a statement
    !      item_setintern = .true.
    ! Therefore, the above cited "if(item_setintern) return" reacts to that
    ! by immediately leaving the notebook callback function.



! subroutine create_window(Win, glade_file_name, ifehl)
subroutine create_window(Win, glade_file_name_f, ifehl)             ! 25.2.2024

    ! this routine uses a gtk_builder to build the window from the Glade file
    ! (glade_file_name) the Window, makes available the icons (partly self-prepared).
    !
    ! It calls URGladesys which extracts a structure clobj which contains arrays
    ! of idd_names, labels, signals and so on for each widget contained in the Glade
    ! file. The idd names and the label names of the widgets are then used to obtain
    ! with gtk_builder_get_object the associated C-pointers.
    ! It then connects the signals. Then, CSSLoad loads the CSS file gtk_UR2.css
    ! belonging to UncertRadio, and SetColor sets the UncertRadio specific colors.
    !
    ! UncW_init is then called for a lot of initialisations; the label strings of the
    ! widgets are then translated into the selected language by the routine TranslateUR.
    !
    ! Finally, the geometric sizes of the graphical windows are defined/prepared.
    !
    ! Significant parts are taken from GTK-Fortran.

use UR_gtk_variables,     only: clobj,nclobj, &
                                Notebook_labelid,Notebook_labeltext, &
                                gladeorg_file,gladedec_file,time_gladeorg,time_gladedec,  &
                                glade_org,glade_dec, keya,keystrg,nbook2,        &
                                prout_gldsys,consoleout_gtk,transdomain,winRelSizeWidth, &
                                winRelSizeHeight,scrwidth_min,scrwidth_max,scrheight_min,scrheight_max, &
                                pixbuf_info,pixbuf_warning,pixbuf_error,pixbuf_question,gscreen, &
                                monitorUR

use UR_Variables,         only: SaveP,project_loadw, work_path, dir_sep

use gtk,                  only: gtk_builder_new,gtk_builder_get_object,gtk_builder_add_from_file, &
                                gtk_widget_set_sensitive,gtk_builder_connect_signals_full, &
                                gtk_label_get_text, &
                                gtk_notebook_page_num,gtk_notebook_get_nth_page,gtk_widget_get_name, &
                                gtk_builder_get_translation_domain,gtk_builder_add_from_string, &
                                gtk_window_set_transient_for,gtk_icon_theme_get_default, &
                                gtk_widget_grab_focus,gtk_widget_set_focus_on_click, &
                                TRUE,FALSE,gtk_notebook_set_current_page,gtk_builder_set_translation_domain, &
                                gtk_widget_size_request,gtk_window_maximize,       &
                                gtk_window_set_resizable,gtk_widget_set_size_request

use gtk_sup,              only: gvalue,Gerror,FALSE
use Top,                  only: WrStatusbar,CharModStr
use URinit,               only: Uncw_Init,GtkSettingsIO
use gdk,                  only: gdk_cursor_new, gdk_synthesize_window_state
use gdk_pixbuf_hl,        only: hl_gdk_pixbuf_new_file
use Rout,                 only: pending_events,WDPutLabelColorB,WDPutLabelColorF
use gtk_draw_hl,          only: gtkallocation,hl_gtk_drawing_area_new
use gtk_hl,               only: hl_gtk_notebook_new,hl_gtk_notebook_add_page, &
                                hl_gtk_button_new,hl_gtk_box_pack
use gtk_hl_tree,          only: hl_gtk_list_tree_set_gvalue
use UR_params,            only: rn
use common_sub1,          only: drawboxpackedMC, drawboxpackedELI,drawboxpackedBS,drawboxpackedCP, &
                                draw_baseELI,drawing,width_da,height_da
use handlers_sub1

implicit none

type(window),  target       :: Win
integer(4),intent(out)      :: ifehl
character(len=*),intent(in) :: glade_file_name_f           ! 25.2.2024

type(c_ptr)                 :: builder,qbut
type(c_ptr), target         :: error
integer(c_int)              :: guint
!! character(kind=c_char)      :: glade_file_name(*)
character(kind=c_char)      :: glade_file_name(256)        ! 25.2.2024
type(c_ptr)                 :: cptr,pname
integer(c_int)              :: pno
integer(4)                  :: i0,i1,i2,i3,jj
real(rn)                    :: start,finish
type(GtkRequisition),target :: winsize

character(len=256)      :: f_glade_file_name
integer(4)              :: i,ncitem,ios,j,klen,lenbuff,kk,idatei
character(len=40)       :: wname
character(len=4)        :: twd,tht
type(c_ptr)             :: transl_domain
character(c_char)       :: glade_file(256)
character(c_char),allocatable :: bufferGL(:)
integer(c_size_t)       :: bufferLen
logical                 :: testgl

type(c_ptr)                   :: icth
type(gerror),pointer          :: error_struct
integer(c_int),pointer        :: coldat
character(:),allocatable      :: text,textcd
type(gtkallocation),target    :: alloc
!-------------------------------------------------------------------------------
ifehl = 0

allocate(character(len=30)  :: text,textcd)

             ! write(66,*) 'Begin create_window --------------------------'
! 5 lines added, from 25.2.2024
i0 = len_trim(glade_file_name_f)
do i=1,i0
  glade_file_name(i) = glade_file_name_f(i:i)
end do
glade_file_name(i0+1) = c_null_char

guint = 0
! load GUI into builder
builder = gtk_builder_new()
if(.false.) then
  if(.true.) call gtk_builder_set_translation_domain (builder, trim(transdomain)//c_null_char)
  transl_domain = gtk_builder_get_translation_domain(builder)
       ! write(66,'(a,i11)') 'transl_domain=',transl_domain
     if(C_associated(transl_domain)) call c_f_string(transl_domain,transdomain)
      write(66,'(a,i11,a,a)') 'transl_domain=',transl_domain, '  Translation_domain: ',trim(transdomain)
end if

idatei = 0
if(glade_org) then
  if(.not. glade_dec .or.( glade_dec .and. time_gladeorg > time_gladedec) ) then
    ! Initial state (idatei= 1):
    ! If only the original Glade file is available, or, if the
    ! original Glade file has a newer date/time as the encrypted version (Glade.dat),
    ! the original Glade file is loaded, which also produces with URGladesys the encrypted
    ! version Glade.dat.

    do i=1,len_trim(gladeorg_file)
      glade_file(i:i) = gladeorg_file(i:i)
    end do
    kk = len_trim(gladeorg_file)+1
    glade_file(kk:kk) = c_null_char
    call c_f_string_chars(glade_file, f_glade_file_name)
    error = c_null_ptr        ! This initialisation (set to null-pointer) is necessary!
    guint = gtk_builder_add_from_file(builder, glade_file, c_loc(error))

    if(c_associated(error)) then
      call EvalGerror('gtk from glade file: ',error)
      call c_f_pointer(error, error_struct)
      ifehl = 1
       return
     end if
    call c_f_string_chars(glade_file_name, f_glade_file_name)
    idatei = 1
  end if
end if

if((glade_dec .or. idatei == 0) )  then
  if(.not. glade_org .or. (glade_org .and. time_gladedec > time_gladeorg)) then
    ! State of delivery (idatei= 2):
    ! If the original Glade file is not available its date/time is older,
    ! the encrypted version Glade.dat will be read, stored then, character by
    ! character to C string, from which then the GUI is build.

        !!!!  call ReadGladeDec(Gladedec_file)
      goto 10
    !----------------------------------------------------------------------
10  continue
    lenbuff = 0
    testgl = .false.
       ! testgl = .true.

    if(testgl) open(121,file='testgl.glade',status='unknown')

    if(allocated(bufferGL)) deallocate(bufferGL)
    allocate(bufferGL(850000))

    close (18)
    open(18, file=gladedec_file, status='old', iostat=ios)
    if(ios /= 0) then
      write(65,*) 'Error with trying to open the Glade file!'
    end if
    klen = size(keya)
    keystrg = ' '
    do i=1,klen
      keystrg = trim(keystrg) // char(keya(i))
    end do

    call cpu_time(start)

    do i=1,100000
      call CharModStr(textcd, 450)
      read(18,'(a)',iostat=ios) textcd
      if(ios /= 0) then
        exit
      end if
      call CharModStr(text,450)
      call StrgEncode(keystrg,textcd,text,2, i)

      if(.true.) then              !  .and. windowRelSize >= 0.4_rn) then
        ! maximize the window: include width- and height-request for "box1"!
        ! So, only the gtkbuilder allows for this maximazation!

        ! For a fixed window size, the parameter "size is variable" of window1 must
        ! be deactivated in the original glade file. ! 15.8.2023

        !  <object class="GtkWindow" id="window1">
        !    <property name="width-request">1800</property>
        !    <property name="height-request">950</property>


        if(.false.) then
          i1 = index(text,'<object class="GtkBox" id="box1">')
          if(i1 == 0) then
            if(index(text,'<object class=') > 0 .and. index(text,'GtkBox') > 0 .and. &
               index(text,'"box1"') > 0 ) i1 = 1
          end if
        end if
        if(.true.) then
          ! 16.8.2023:
          i1 = index(text,'<object class="GtkWindow" id="window1">')
          if(i1 == 0) then
            if(index(text,'<object class=') > 0 .and. index(text,'GtkWindo1') > 0 .and. &
               index(text,'window1') > 0 ) i1 = 1
          end if
        end if

        if(i1 > 0) then
          !write(twd,'(i4)') int(real(scrwidth_max - 210,rn) *windowRelSize,4)
          !write(tht,'(i4)') int(real(scrheight_max - 40,rn) *windowRelSize,4)
         ! write(twd,'(i4)') int(real(scrwidth_max - 0,rn) *windowRelSize * 0.8_rn, 4)       ! 15.8.2023
         ! write(tht,'(i4)') int(real(scrheight_max - 13,rn) *windowRelSize, 4)      !
         ! write(twd,'(i4)') int(real(scrwidth_max - scrwidth_min - 0,rn) *windowRelSize*0.8_rn, 4)   ! 17.8.2023
         ! write(tht,'(i4)') int(real(scrheight_max - scrheight_min - 13,rn) *windowRelSize, 4)      !
          write(twd,'(i4)') int(real(scrwidth_max - scrwidth_min - 0,rn) *winRelSizeWidth, 4)   ! 17.8.2023
          write(tht,'(i4)') int(real(scrheight_max - scrheight_min - 13,rn) *winRelSizeHeight, 4)      !

          twd = adjustL(twd)
          tht = adjustL(tht)
                  write(66,*) 'found <object class="GtkBox" id="box1">  : i1=',i1,' twd=',twd,' tht=',tht
          do jj=1,3
            if(.false.) then
              ! 'box1':
              if( jj == 1) then
              elseif(jj == 2) then
                text = '        <property name="width-request">' // trim(twd) // '</property>'
              elseif(jj == 3) then
                text = '        <property name="height-request">' // trim(tht) // '</property>'
              end if
            else
              ! 'window1':    16.8.2023
              if( jj == 1) then
              elseif(jj == 2) then
                text = '    <property name="width-request">' // trim(twd) // '</property>'
              elseif(jj == 3) then
                text = '    <property name="height-request">' // trim(tht) // '</property>'
              end if
            end if

            if(jj < 3) then
              kk = len_trim(text)
              do j=1,kk
                bufferGL(lenbuff+j) = text(j:j)
              end do
              lenbuff = lenbuff+kk
              bufferGL(lenbuff+1) = char(10)
              lenbuff = lenbuff + 1
            end if
          end do
        end if
      end if
      if(.FALSE.) then
        ! beseitigt keine Fehler!
        i2 = index(text,'<col id="')
        i3 = index(text,'> </col>')
        ! write(66,*) 'i=',i,'text=',trim(text)
        ! if(i2 > 0 .and. i3 > i2) write(66,*) 'i=',i,'text=',trim(text),' i2,i3=',int(i2,2),int(i3,2)
        if(i3 >= i2+12 .and. len_trim(text)==i3+7 ) then
          !text = text(1:i3) // '" "' // trim(text(i3+2:))
          !  write(66,'(i6,2x,a)') i,trim(text)
        end if
      end if

      ! Ensure that the self-prepared Icon images can be read also from the
      ! work_path, in the case that project file is called from a different
      ! path.

      i0 = index(text,'pixbuf')
      if(i0 > 1) then
        ! Warning: these filename strings are case-sensitive!
        ! FO: I'm not sure, why the icons are hardcoded here
        ! -> A more general approach which should work for windows and linux:
        i1 = index(text,'">icons')
        if(i1 > 1) text = text(1:i1+1) // trim(work_path) // 'icons' // dir_sep // text(i1+8:)

!        i1 = index(text,'icons\FittingData_24.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\FittingResults_24.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\Distrib_24.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!
!        i1 = index(text,'icons\Window-close.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\system-run.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\help-contents.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\edit-okay.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\document-save.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\apply.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\application-exit.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!
!        i1 = index(text,'icons\document-open.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\document-save-as.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\view-refresh.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\format-justify-fill.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\preferences-system.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\preferences-desktop-font.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\dialog-about.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
!        i1 = index(text,'icons\help-about.png')
!        if(i1 > 1) text = text(1:i1-1) // trim(work_path) // text(i1:)
      end if
      kk = len_trim(text)
      do j=1,kk
        bufferGL(lenbuff+j) = text(j:j)
      end do
      lenbuff = lenbuff+kk
      bufferGL(lenbuff+1) = char(10)
      lenbuff = lenbuff + 1
    end do

    bufferGL(lenbuff+1) = c_null_char
    bufferLen = lenbuff
    if(testgl) close (121)
     call cpu_time(finish)
        write(66,*) 'Glade file read: cpu-time= ',sngl(finish-start)

         call cpu_time(start)
    error = c_null_ptr        ! necessary
               write(0,*) 'before gtk_builder --------------------------'

    guint = gtk_builder_add_from_string(builder, bufferGL, bufferLen, c_loc(error))
               write(0,*) 'After gtk_builder --------------------------'

    deallocate(bufferGL)
        call cpu_time(finish)
          write(66,'(a,f8.3,a,i0)') 'Builder_add_from_string: cpu-time= ',sngl(finish-start),'  guint=',guint
          write(0,'(a,f8.3,a,f8.3)') 'Builder_add_from_string: cpu-time= ',sngl(finish-start),'  cput=',sngl(finish)
    idatei = 2
  end if

end if
20 continue

pixbuf_info = hl_gdk_pixbuf_new_file(trim(work_path)//'icons'//dir_sep//'dialog-information.png'//c_null_char)
pixbuf_error = hl_gdk_pixbuf_new_file(trim(work_path)//'icons'//dir_sep//'dialog-error.png'//c_null_char)
pixbuf_question = hl_gdk_pixbuf_new_file(trim(work_path)//'icons'//dir_sep//'dialog-question.png'//c_null_char)
pixbuf_warning = hl_gdk_pixbuf_new_file(trim(work_path)//'icons'//dir_sep//'dialog-warning.png'//c_null_char)

if(consoleout_gtk) write(0,*) 'Behind processing the Glade file'

if(idatei == 0) then
    write(66,*)  'Glade file  not found:  idatei=',idatei
    write(66,*) "Program terminated!"
    ifehl = 1
    return   ! "Program terminated"
endif

if (guint == 0_c_int) then    ! False
  write(66,'(a,i0,a,a)')  'idatei= ',idatei,' file=',trim(gladedec_file)
  if(c_associated(error)) call EvalGerror('Load glade from string: ',error)
  write(66,'(a,a)') "  c_associated(Error)=",c_associated(error)
  if(idatei == 1) write(66,*) "Could not load the glade file: ",trim(gladeorg_file)
  if(idatei == 2) write(66,*) "Could not load the glade file: ",trim(gladedec_file)
end if

call cpu_time(start)

call URGladesys(idatei)

call cpu_time(finish)
write(66,*) 'URGladesys done: cpu-time= ',sngl(finish-start)

icth = gtk_icon_theme_get_default()
call cpu_time(start)
do i=1,nclobj
  clobj%id_ptr(i) = gtk_builder_get_object(builder,clobj%idd(i)%s//c_null_char)
  if(len_trim(clobj%label(i)%s) > 0) then
    clobj%label_ptr(i) = gtk_builder_get_object(builder,clobj%label(i)%s//c_null_char)
  end if
  if(prout_gldsys) write(65,'(a,i4,3a,i16,9a,i4)') 'i=',i,' id=',clobj%idd(i)%s,',  id_ptr=',clobj%id_ptr(i),   &
              ' name=',clobj%name(i)%s, ' ; Label=',clobj%label(i)%s, &   ! ,', label_ptr(i)=',clobj%label_ptr(i)
              ' handler=',clobj%handler(i)%s,' signal=',clobj%signal(i)%s, &
              '  idparent=',clobj%idparent(i)
end do
         call cpu_time(finish)

! get references to GUI elements
! The name passed to the gtk_builder_get_object function has to match the name
! of the objects in Glade

Win%window_ptr  = gtk_builder_get_object(builder,"window1"//c_null_char)
     write(66,'(a,i11,i11)') "Win, the first; PTR=",Win%window_ptr, idpt('window1')

! connect signal handlers
call gtk_builder_connect_signals_full(builder,c_funloc(connect_signals), c_loc(Win))

! free memory
call g_object_unref(builder)

call gtk_widget_set_sensitive(idpt('MenuDecayCurve'), 0_c_int)
call gtk_widget_set_sensitive(idpt('MenuGSpekt1'), 0_c_int)
call gtk_widget_set_sensitive(idpt('KalFit'), 0_c_int)
call gtk_widget_set_sensitive(idpt('ExportToR'), 0_c_int)

call gtk_widget_set_sensitive(idpt('TBModelDialog'), 0_c_int)
call gtk_widget_set_sensitive(idpt('TBInputDialog'), 0_c_int)
call gtk_widget_set_sensitive(idpt('TBFittingResult'), 0_c_int)

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
write(66,*) 'TranslateUR: cpu-time= ',sngl(finish-start)
SaveP = .False.
project_loadw = .TRUE.

do i=1,6
  cptr = gtk_label_get_text(idpt(Notebook_labelid(i)))
  call c_f_string(cptr,Notebook_labeltext(i))
end do

call WrStatusbar(3,' ')

do i=0,5
  cptr = gtk_notebook_get_nth_page(idpt('notebook1'), i)
  call FindItemP(cptr, ncitem)
  if(ncitem > 0) then
    pname = gtk_widget_get_name(cptr)
    call c_f_string(pname, wname)
  end if
end do
if(.false.) then
  write(66,*) 'Page 1: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box2'))+1
  write(66,*) 'Page 2: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box3'))+1
  write(66,*) 'Page 3: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box4'))+1
  write(66,*) 'Page 4: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box5'))+1
  write(66,*) 'Page 5: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('grid5'))+1
  write(66,*) 'Page 6: has number=',gtk_notebook_page_num(idpt('notebook1'),idpt('box7'))+1
end if

call gtk_window_set_transient_for(idpt('dialogDecayModel'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialogColB'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialogELI'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_LoadPro'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_decayvals'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_fontbutton'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_gspk1'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_kalfit'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_numegr'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_options'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_symbExchg'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_symbchg'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_symbchg'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialogMeanData'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialogSerEval'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_BinPoi'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_distributions'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_Batest'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialogBatEval'), Win%window_ptr)
call gtk_window_set_transient_for(idpt('dialog_infoFX'), Win%window_ptr)

call gtk_widget_grab_focus(idpt('textview1'))
call gtk_widget_set_focus_on_click(idpt('window1'),1_c_int)

!  call gtk_window_fullscreen_on_monitor(idpt('window1'), gscreen, monitorUR)  !  does not work

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

allocate(coldat)
coldat = 4_c_int
call g_object_set_data(idpt('treeviewcolumn10'),"column-number"//c_null_char,c_loc(coldat))

alloc%width = 1200_c_int
alloc%height = 800_c_int

!call gtk_widget_set_size_request(idpt('window1'),alloc%width,alloc%height)

! call gtk_window_set_default_size(GTK_WINDOW(mainWindow), 400, 300);
! call gtk_window_set_resizable(idpt('window1'), FALSE)

write(0,*) 'end of create_window'
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


end subroutine create_window

!############################################################################

subroutine show_window(Win)

    use gtk,                only: gtk_widget_show,gtk_window_set_gravity,GDK_gravity_NORTH_WEST
    use UR_gtk_variables,   only: item_setintern_window1
    implicit none

    type(window), intent(in) :: Win

    item_setintern_window1 = .true.        ! 16.8.2023
    call gtk_window_set_gravity(Win%window_ptr, GDK_gravity_NORTH_WEST)
    call gtk_widget_show(Win%window_ptr)
    ! item_setintern_window1 = .false.

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

subroutine connect_signals (builder,object, signal_name, handler_name, connect_object, flags, c_Win) bind(c)

use, intrinsic :: iso_c_binding,    only: c_ptr, c_char, c_int
use gtk,              only: g_signal_connect

implicit none

type(c_ptr), value               :: builder           !a GtkBuilder
type(c_ptr), value               :: object            !object to connect a signal to
character(kind=c_char)           :: signal_name(*)    !name of the signal
character(kind=c_char), target   :: handler_name(*)   !name of the handler
type(c_ptr), value               :: connect_object    !a GObject, if non-NULL, use g_signal_connect_object()
integer(c_int), value            :: flags             !GConnectFlags to use
type(c_ptr), value               :: c_Win             !user data
character(len=25)                :: h_name,h_signal
!--------------------------------------------------------------------------------------------------------------

call c_f_string_chars(handler_name, h_name)
call c_f_string_chars(signal_name, h_signal)
! write(66,*) "Connect signal for h_name= ", h_name,"  object=",object,'  h_signal=',h_signal


select case (h_name)  !Add event handlers created in Glade below, otherwise the widgets won't connect to functions
                      !The names in the case have to match the names of the *signals* in Glade and the
                      !text in c_funloc(...) has to match the name of the actual function in the code.

case ("clickbut")
    call g_signal_connect (object, signal_name, c_funloc(button_clicked), c_Win)
case ("SelOpt")
    call g_signal_connect (object, signal_name, c_funloc(SelOpt), c_Win)
case ("LoadFile", "LoadProjectFile")
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

!case ("button_pressed")
!    call g_signal_connect (object, signal_name, c_funloc(UR_TV_button_pressed_cb), c_Win)
!case ("button_released")
!    call g_signal_connect (object, signal_name, c_funloc(UR_TV_button_released_cb), c_Win)
case ("keyPress")
    call g_signal_connect (object, signal_name, c_funloc(UR_keyPress_cb), c_Win)
case ("col_clicked")
    call g_signal_connect (object, signal_name, c_funloc(UR_TV_column_clicked_cb), c_Win)
case ("SizeAlloc")
    call g_signal_connect (object, signal_name, c_funloc(UR_window_size_alloc_clicked_cb), c_Win)         ! 16.8.2023


case default
    write(66,*) "Unknown handler = "//h_name
    write(66,*) "Program terminated"
    stop "Program terminated"
end select

end subroutine connect_signals

!==================================================================================
!Create functions for the gui below.  Then attach them to the gui's code using event handlers in the above
!function.  Attach the event handlers to the actual buttons or whatever in the gui using Glade
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv


! Note: -------------------------------------------------------------------
!  Event Callbacks are functions (>= 3 args)
!  Signal Callbacks are subroutines (args)
!-----------------------------------------------------------------------------

!#####################################################################################

recursive subroutine SelOpt(widget, gdata)  bind(c)     ! result(ret)

   ! the routine SelOpt is invoked by many of the user actions on widgets of the
   ! main window or on dialogs. SelOpt identifies the id name of the corresponding
   ! widget. It then forwards the minor part of the cases, especially requates from the
   ! main menu of the main window to ProcMenu for further treatment. The majority of
   ! cases is forwarded to ProcMainDiag, which then executes the tasks required by the
   ! action. It checks after return from ProcMenu for a request to close the program;
   ! in case of a QuitProg, it stops the main GTK loop, wehereafter the program ends
   ! in the main routine gui_UR_main.
   !

use, intrinsic :: iso_c_binding,   only: c_null_char,c_ptr,c_int,c_int16_t,c_associated

use gtk,             only: gtk_main_quit,gtk_widget_set_focus_on_click,FALSE, &
                           gtk_widget_destroy,gtk_main_iteration_do,gtk_events_pending

use UR_gtk_window
use UR_gtk_variables
use UR_gini,         only: gdkcursor
use UR_VARIABLES,    only: actual_grid

implicit none

type(c_ptr), value    :: widget, gdata
integer(c_int)        :: boolresult
integer(c_int)        :: run_status = TRUE

character(len=60)     :: idstring,signal,parentstr,name
integer(4)            :: ncitem, i
!---------------------------------------------------------------------
if(item_setintern) return

if(dialog_on) then
  call FindItemP(widget, ncitem)
  idstring = clobj%idd(ncitem)%s
  if(trim(idstring) /= 'TBRemoveGridLine') return
end if

call FindItemP(widget, ncitem)
        ! write(66,*) 'SelOpt:  At begin,   widget=',widget,'  ncitem=',ncitem,' id=',clobj%idd(ncitem)%s
ioption = 1000
dialogstr = ''
if(ncitem > 0) then
  idstring = clobj%idd(ncitem)%s
  signal = clobj%signal(ncitem)%s
  i = clobj%idparent(ncitem)
  if(i > 0) then
    parentstr = clobj%name(i)%s
  else
    ! if the signal does not come from window1:
    parentstr = ''
    signal = 'delete-event'
  end if
  name = clobj%name(ncitem)%s
else
   write(66,*) '****** SelOpt:  non-associated widget: ',widget
  if(consoleout_gtk) write(0,*) '****** SelOpt:  non-associated widget: ',widget
  return
end if
         ! write(66,*) 'idparent=',i,' parentstr=',trim(parentstr)
if(trim(parentstr) == 'GtkWindow' .or. trim(idstring) == 'window1'    &
   .or. trim(idstring) == 'window_graphs' .or. trim(actual_grid) >= 'treeview5' ) then
  call ProcMenu(ncitem)
  if(.not.QuitProg) call gtk_widget_set_focus_on_click(idpt('window1'),1_c_int)
  if(QuitProg) then
    if(c_associated(gdkcursor)) call g_object_unref(gdkcursor)

    call gtk_widget_destroy(idpt('window1'))
    goto 22

    call gtk_widget_set_focus_on_click(idpt('window1'),0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialogDecayModel'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialogColB'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialogELI'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_LoadPro'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_decayvals'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_fontbutton'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_gspk1'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_kalfit'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_numegr'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_options'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_symbExchg'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_symbchg'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_symbchg'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialogMeanData'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialogSerEval'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_Batest'), 0_c_int)
    call gtk_widget_set_focus_on_click(idpt('dialog_distributions'), 0_c_int)

       ! call pending_events()
    do while(IAND(gtk_events_pending(), run_status) /= FALSE)       ! wait until True
      boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
22  continue
    call gtk_main_quit()
  end if
end if

    !   ret = False
end subroutine Selopt

!------------------------------------------------------------------------------------------

subroutine button_clicked(widget, gdata)  bind(c)    ! result(ret)

   ! this function identifies the widget sets ButtonClicke and/or HelpButton to true.
   !
   ! The latter variables are used in the GTK dialog loop, located in the middle of
   ! Loadsel_diag_new, where the loop would then terminates, and Loadsel_diag_new will
   ! then react on that.

use, intrinsic :: iso_c_binding,      only: c_null_char,c_ptr,c_int,c_int16_t
use UR_gtk_window
use UR_gtk_variables
use UR_Gleich,          only: loadingpro
use CHF,                only: ucase

implicit none

type(c_ptr), value     :: widget, gdata
! integer(c_int)         :: ret
integer(4)             :: ncitem
type(c_ptr)            :: item_clicked
character(len=80)      :: stritem

   ! ret = False

item_clicked = widget
call FindItemP(widget, ncitem)
str_item_clicked = clobj%idd(ncitem)%s
     ! write(66,*) 'Button clicked:   item_clicked=',item_clicked,'   id=',trim(str_item_clicked)

HelpButton = .false.
if(trim(str_item_clicked) == 'LoadWithCalc') goto 10
if(trim(str_item_clicked) == 'LoadWithoutCalc') goto 10
if(trim(str_item_clicked) == 'BinPoiOK' .or. trim(str_item_clicked) == 'BinPoiCancel') goto 10
if(trim(str_item_clicked) == 'HelpFX') goto 10

if(loadingpro) return
10      continue

stritem = ucase(str_item_clicked)
! if(index(stritem,'HELP') > 0 .or. trim(clobj%label(ncitem)) == 'gtk-label') HelpButton = .true.
if(index(stritem,'HELP') > 0 .or. clobj%label(ncitem)%s == 'Hilfe') HelpButton = .true.
   if(trim(stritem) == 'HelpFX' .and. HelpButton) HelpButton = .false.   ! 8.3.2024
                 ! write(66,*) 'button_clicked:   HelpButton=',HelpButton
   ! ret = True
ButtonClicked = .true.
ncitemClicked = ncitem

end subroutine button_clicked

!----------------------------------------------------------------------------

recursive function UR_keyPress_cb(widget,event, gdata) result(ret) bind(c)

    ! a small callback routine for keypress events (a few only)

use, intrinsic :: iso_c_binding,      only: c_null_char,c_ptr,c_int,c_int16_t,c_int16_t
use gdk,                only: gdk_event_get_keyval
use gdk_events,         only: GdkEventKey
use gtk,                only: gtk_widget_grab_focus,gtk_main_do_event,FALSE, &
                              gtk_tree_view_get_cursor
use gtk_hl,             only: hl_gtk_listn_get_selections,hl_gtk_listn_get_cell
use g,                  only: g_object_get_data
use UR_gtk_window
use UR_gtk_variables,   only: clobj
use Rout,               only: WTreeViewSetCursorCell
use CHF,                only: ucase

implicit none

type(c_ptr), value     :: widget, gdata, event
integer(c_int)         :: ret
type(c_ptr)            :: pcol
integer(c_int),pointer :: jcol1
integer(4)             :: ncitem
integer(4)             :: akey, allowdkeys(6)
character(len=80)      :: entry
type(GdkEventKey),pointer   :: event_struct

! see file gdkkeysyms.h for keyboard key codes!

!  65289 : TAB;   65293: Return;          escape: 65307
!  65361 : left arrow;  65362 : up arrow;  65363 : right arrow; 65364: down arrow
  allowdkeys = [ 65289, 65293, 65361, 65362, 65363, 65364 ]
ret = False
call c_f_pointer(event, event_struct)
akey = event_struct%keyval
call FindItemP(widget, ncitem)

!----------------------------------------------------------------------
      if(akey == 65307) then    ! ESC key
        if(ncitem > 0) write(66,*) 'ESC given by: ncitem=',ncitem,' idd=',clobj%idd(ncitem)%s
        ret = FALSE
        if(clobj%name(ncitem)%s == 'GtkDialog') ret = TRUE
        ! with ret=TRUE, dialog-destroy is prevented for ESC !!!
        ! therefore, the callback should be a function !!!!!!
        return
      end if
!----------------------------------------------------------------------
if(Findloc(allowdkeys, akey,dim=1) == 0) return

if(ncitem == 0) return

    if(akey == 65293) then
      ret = FALSE
      return
    end if
entry = clobj%idd(ncitem)%s
if(trim(entry) == 'TRentryMCanzM') then
  call gtk_widget_grab_focus(idpt('TRentryMCanzR'))
  ret = TRUE
  return
end if
if(trim(entry) == 'TRentryMCanzM1') then
  call gtk_widget_grab_focus(idpt('TRentryMCanzR1'))
  ret = TRUE
  return
end if
if(.true. .and. index(entry,'treeview') > 0 .and. akey >= 65361 .and. akey <= 65364) then
  ! count = hl_gtk_listn_get_selections(widget, indices)

  pcol = g_object_get_data(widget,"column-number"//c_null_char)
  call c_f_pointer(pcol,jcol1)

       return
end if

end function UR_keyPress_cb

!---------------------------------------------------------------------------------------------

subroutine ProjectOpen_cb(widget, gdata)  bind(c)      ! result(ret)

   ! this routine receives the request for opening a project file and forwards finally
   ! to ProcessLoadPro_new for really doing the steps necessary for opening the file
   ! and loading it into the GUI.
   ! In between these two steps, it checks, and gives the chance to it, whether the
   ! project shall be saved before executing a closing step.

use, intrinsic :: iso_c_binding,    only: c_null_char,c_ptr,c_int
use gtk,              only: GTK_BUTTONS_YES_NO,gtk_response_yes,GTK_MESSAGE_WARNING
use UR_gtk_window
use UR_gtk_variables, only: Quitprog,dialog_on,clobj
use UR_variables,     only: FileTyp,fname,langg,Savep,simul_ProSetup
use UR_interfaces,    only: ProcessLoadPro_new
use UR_Gleich,        only: ifehl
use Rout,             only: MessageShow,fopen

implicit none

type(c_ptr), value    :: widget, gdata
integer(c_int)        :: resp     ! ret

integer               :: ncitem
character(len=60)     :: title,cheader
character(len=120)    :: str1,idstring
 !----------------------------------------------------------------------------

if(dialog_on) then
  call FindItemP(widget, ncitem)
  idstring = clobj%idd(ncitem)%s
  if(trim(idstring) /= 'TBRemoveGridLine') return
end if

FileTyp = 'P'

IF (.true. .and. Filetyp == 'P' .AND. SAVEP) THEN
  IF(langg == 'DE') then
    write(str1,*)   &
    'Soll das geöffnete Projekt vor dem Beenden'//CHAR(13) &
    //'gespeichert oder gesichert werden?'
    call MessageShow(trim(str1), GTK_BUTTONS_YES_NO, "Projekt schließen:", resp,mtype=GTK_MESSAGE_WARNING)
  END IF
  IF(langg == 'EN') then
    write(str1,*)   &
    'Shall the open project be saved before closing it? '
    call MessageShow(trim(str1), GTK_BUTTONS_YES_NO, "Closing Project:", resp,mtype=GTK_MESSAGE_WARNING)
  END IF
  IF(langg == 'FR') then
    write(str1,*)   &
    'Le projet ouvert doit-il être sauvegardé avant de le fermer? '
    call MessageShow(trim(str1), GTK_BUTTONS_YES_NO, "Projet de clôture:", resp,mtype=GTK_MESSAGE_WARNING)
  END IF

  IF (resp == GTK_RESPONSE_YES) THEN   !                           ! -8
    if(len_trim(fname)== 0) then
      cheader = 'Choose filename:'
      call FOpen(ifehl, .true., cheader )
      if(ifehl == 1) goto 9000
    end if
    call ProjectSave_CB(widget,gdata)
  END IF
END IF

title = 'Open project file:'
call ProcessLoadPro_new(0,1)
  write(66,*) 'ProjectOpen:   after call ProcessLoadPro_new, QuitProg=',QuitProg

   ! ret = False
9000  continue

end subroutine ProjectOpen_cb

!#############################################################################################

subroutine ProjectSave_cb(widget, gdata) bind(c)     ! result(ret)

   ! this routine receives the request for saving a project file and forwards it
   ! to the routie Save.

use, intrinsic :: iso_c_binding,   only: c_null_char,c_ptr,c_int
! use gtk,             only: gtk_widget_set_sensitive
use UR_gtk_window
use UR_gtk_variables,only: clobj,dialog_on
use UR_variables,    only: saveas,FileTyp

implicit none

type(c_ptr), value    :: widget, gdata

character(len=60)     :: title,idstring
integer(4)            :: mode,ncitem
!----------------------------------------------------------------------------
if(dialog_on) then
  call FindItemP(widget, ncitem)
  idstring = clobj%idd(ncitem)%s
  if(trim(idstring) /= 'TBRemoveGridLine') return
end if

! mode:  0:  save;   1: save as;
Mode = 0
title = "Save project file:"
saveas = .false.

call FindItemP(widget, ncitem)
if( clobj%idd(ncitem)%s == 'TBSaveProjectAs' .or.   &
    clobj%idd(ncitem)%s == 'MenuSaveProjectAs') then
  mode = 1
  title = "Save project file as:"
  saveas = .true.
end if

FileTyp = 'P'

call Save(mode, title)

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
                            gtk_widget_hide,FALSE,TRUE,gtk_tree_view_column_get_width, &
                            gtk_tree_view_column_set_max_width,gtk_tree_view_column_set_expand, &
                            gtk_tree_path_new_from_string,gtk_tree_selection_select_path, &
                            gtk_tree_path_to_string,gtk_tree_view_set_drag_dest_row,   &
                            gtk_tree_view_row_activated
use gtk_sup,          only: convert_f_string_s

use gtk_hl,           only: hl_gtk_listn_set_cell,gtk_tree_view_get_model, hl_gtk_listn_get_cell, &
                            hl_gtk_listn_set_selection
use gdk,              only: gdk_beep
use UR_gtk_variables, only: clobj,nclobj, nstores, storename, lsgtype,lstype,item_setintern, &
                            tv_colwidth_digits,tvnames,ntvs,tvcolindex, &   ! ,tv_colwidth_pixel, iter, &
                            TVlastCell
!!! use gtk_sup
use UR_variables,     only: frmt,frmtg,saveP,frmt_min1,frmtc    ! ,clipd
use UR_Gleich,        only: SDformel,SDFormel_CP,SDwert,SDWert_CP,missingval,ngrs_CP,  &
                            SDWert_CP,Symbole_CP,Symbole,IVTL,IAR,SymboleA,  &
                            Messwert,HBreite,StdUnc, ngrs,ngrs_CP,use_DP,charv
use Rout,             only: WTreeViewSetCursorCell,WTreeViewGetComboArray,WTreeViewGetStrArray, &
                            WTreeViewGetDoubleArray,ClearMCfields,WTreeViewSetCursorCell
use Top,              only: FieldUpdate,wrstatusbar
use UR_params,        only: rn
use g,                only: g_signal_emitv
use CHF,              only: FormatNumStr

implicit none

type(c_ptr), value                  :: renderer, path, text    ! , gdata

! character(len=100)                  :: fpath, ftextcp,gtext,gtextorg  ! ,f2path,f3path
! character(:),allocatable            :: ftext
character(:),allocatable            :: ftextXX
type(charv)                         :: fpath, ftextcp,gtext,gtextorg
type(charv)                         :: ftext

character(len=50),allocatable       :: ftable(:,:)
integer(kind=c_int), allocatable    :: irow(:)
integer(kind=c_int)                 :: irow1,icol1,ret
integer(4)                          :: i, n, k, j, ind_rend, krow,kcol,i1,ios,jp,nc
integer(4)                          :: krow_1,krow_2,kcol_1,kcol_2,kc,kr,nt
type(c_ptr)                         :: tree, store
integer(c_int)                      :: ncol,cwidth
character(len=40)                   :: treename,liststore
integer(4)                          :: ind_list,jcol,jrow,jplast
real(rn)                            :: dval16
character(len=10)                   :: coltyp
character(len=20)                   :: frmtv
logical                             :: insert_block
logical                             :: prout
character(len=20)                   :: chcol
character(:),allocatable            :: strX
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
ret = FALSE
if(item_setintern) return
prout = .false.
  ! prout = .true.

jrow = 0
jcol = 0
nc = 0
do i=1,nclobj
  if(c_associated(clobj%id_ptr(i), renderer)) then
    nc = i
    exit
  end if
end do
! if(prout) write(66,'(a,i0)') 'nc= ',nc
if(prout) write(0,'(a,i0)') 'nc= ',nc


allocate(character(len=10000)::strX)
allocate(ftable(200,5))
allocate(character(len=10000):: ftextXX)

if(prout) write(66,*) 'before convert path','  path=',path
call convert_c_string(path,ftextXX)
fpath%s = trim(ftextXX)

if(prout) write(66,*) 'CB on entry: fpath = ',trim(fpath%s),'   path=',path
call convert_c_string(text, ftextXX)
ftext%s = trim(ftextXX)
    if(prout) write(66,*) 'ftext%s=',trim(ftext%s)

  ! Important note:
  ! If the TreeView has only 2 columns, like e.g. treeview8,
  ! the path-string consists of only 1 number, the row number; the path-string does noct contain ":"

insert_block = .false.
if(index(ftext%s,char(9)) > 0 .OR. index(ftext%s,char(10)) > 0 ) then
  ! try to insert a column block from clipboard input:
  ftable = ' '
  jrow = 0
  jp = 1
  jplast = 1
  do i=1,len_trim(ftext%s)
    if(ftext%s(i:i) == char(10)) then
      jrow = jrow + 1
      jcol = 0
      do j=jp,i-1
        if(ftext%s(j:j) == char(9)) then
          jcol = jcol + 1
          ftable(jrow,jcol) = ftext%s(jplast:j-1)
          jplast = j+1
        end if
      end do
      jcol = jcol + 1
      ftable(jrow,jcol) = ftext%s(jplast:i-1)
        if(prout) then
          write(66,'(2(a,i0),a,a)') 'jcol=',jcol,' jplast=',jplast,' ftable=',trim(ftext%s(jplast:j-1))
          write(66,'(2(a,i0),a,20a)') 'fext: line=',jrow,' # of tabs=',jcol,'  columns: ',(ftable(jrow, k)(1:15),' : ',k=1,jcol)
        end if
      jp = i+1
      jplast = jp
    end if
  end do
  insert_block = .true.
end if
  if(prout) write(66,*) 'insert_block =',insert_block
  if(prout) write(0,*) 'insert_block =',insert_block

n = 0
do i = 1, len_trim(fpath%s)
  if (fpath%s(i:i) == ":") then
    n = n+1
    fpath%s(i:i) = ' '   ! : is not a separator for a Fortran read
  end if
end do
allocate(irow(n+1))
read(fpath%s, *) irow
read(fpath%s, *) irow1
read(fpath%s, *) krow
  krow = krow + 1

ncol = 0
treename = ''
liststore = ''
do i=1,nclobj
  if(c_associated(clobj%id_ptr(i), renderer)) then
    ind_rend = i
    nt = 0
    do k=i,1,-1
      if(clobj%name(k)%s == trim('GtkTreeView')) then
        tree = clobj%id_ptr(k)
        treename = clobj%idd(k)%s
        do j=1,ntvs
          if(trim(treename) == tvnames(j)%s) nt = j
        end do
        exit
      end if
    end do
    ncol = 0
    do j=k,ind_rend
      if(index(clobj%idd(j)%s, 'treeviewcol') > 0) ncol = ncol + 1
    end do
    exit
  end if
end do
kcol = ncol
ncol = ncol - 1
icol1 = ncol

! Get list store
store = gtk_tree_view_get_model(tree)

! Find the type for the requested column
! ctype = gtk_tree_model_get_column_type(store, icol1)

do i=1,nclobj
  if(c_associated(clobj%id_ptr(i), store)) then
    liststore = clobj%idd(i)%s
    exit
  end if
end do
ind_list = 0
do i=1,nstores
  if(storename(i)%s == trim(liststore)) then
    ind_list = i
    exit
  end if
end do
   if(prout) then
     write(66,*) 'CB-text angekommen: tree=',tree,'  treename=',trim(treename),' fpath=',trim(fpath%s),'   ftext=',trim(ftext%s)
     write(66,*) '       kcol=',kcol,'  krow=',krow,'  ind_list=',ind_list
     write(66,*) '       renderer=',clobj%idd(ind_rend)%s
     write(66,*) '       liststore=',trim(liststore),'   lsgtype=',lsgtype(ind_list,kcol)%s, &
                         '   lstype=',lstype(ind_list,kcol),'  ind_list=',ind_list,' krow=',kcol
   end if

!!! Important:  hl_gtk_listn_set_cell   requires a treeview pointer as 'List'-Parameter
krow_1 = krow
krow_2 = krow
kcol_1 = kcol
kcol_2 = kcol
if(insert_block) then
  krow_1 = krow
  krow_2 = krow + jrow - 1
  kcol_1 = kcol
  kcol_2 = kcol + jcol - 1
end if
  if(prout) then
    write(66,*) 'insert_block=',insert_block,'  kcol_1, kcol_2=',kcol_1, kcol_2,'  krow_1, krow_2=',krow_1, krow_2, &
                '  ftext=',trim(ftext%s)
  end if

do kr=krow_1,krow_2
  irow1 = kr - 1
  krow = kr

  if(trim(treename) == 'treeview2' .and. kr > ubound(Symbole,dim=1)) exit
  if(trim(treename) == 'treeview3' .and. kr > ubound(SymboleA,dim=1)) exit

  do kc=kcol_1,Kcol_2
    icol1 = kc - 1
    kcol = kc
    if(.not.insert_block) gtext%s = trim(ftext%s)
    if(insert_block) gtext%s = ftable(kr-krow_1+1, kc-kcol_1+1)
    select case (lstype(ind_list,kcol))
      case (1, 2)
        select case (trim(lsgtype(ind_list,kcol)%s))

            ! Note: from icol1 given here, -1 is already subtracted
          case ('gchararray')
            coltyp = 'text'
            if(trim(treename) == 'treeview2' .and. (icol1 == 4 .or. icol1 == 7 .or. icol1 == 8  &
                                                                    .or. icol1 == 10)) then
              coltyp = 'double'
              frmtv = frmt
            end if
            if(trim(treename) == 'treeview3' .and. icol1 == 5) then
              coltyp = 'double'                 ! covar-grid
              frmtv = frmtc
            end if
            if(trim(treename) == 'treeview4' .and. icol1 > 4) then
              coltyp = 'double'                  ! budget-grid
              frmtv = frmt
            end if
            if(trim(treename) == 'treeview5' .and. icol1 > 1) then
              coltyp = 'double'                  ! deacy-data grid
              frmtv = frmt
            end if
            if(trim(treename) == 'treeview6' .and. icol1 > 1) then
              coltyp = 'double'                  ! Gskp1- grid
              frmtv = frmtg
            end if
            if(trim(treename) == 'treeview7' .and. icol1 > 1) then
              coltyp = 'double'                  ! KalFit- grid
              frmtv = frmtg
            end if
            if(trim(treename) == 'treeview8' .and. icol1 > 1) then
              coltyp = 'double'                  ! data-mean grid
              frmtv = frmtg
            end if

            select case (trim(coltyp))
              case ('text')
                ! call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=gtextorg%s)
                call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=strX)
                gtextorg%s = trim(strX)
                if(trim(gtext%s) /= trim(gtextorg%s)) then
                  call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=trim(gtext%s))
                  SaveP = .true.
                  call FieldUpdate('GUI 1219')
                  if(trim(treename) == 'treeview2') then
                    call clearMCfields(1)
                    call gtk_widget_hide(idpt('window_graphs'))
                  end if
                end if

                if(trim(treename) == 'treeview2' .and. icol1 == 6) then
                  ! If a SD formula is deleted, the associated array elements  SDFormel and SDWert
                  ! must also be deleted.
                  if(len_trim(SDformel(krow)%s) > 0 .and. len_trim(gtext%s) == 0) then
                    call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1+1,  svalue=' ')
                  end if
                  SDformel(krow)%s = trim(gtext%s)
                  SDformel_CP(krow)%s = trim(gtext%s)
                  SDWert(krow) = missingval
                  do j=1,ngrs_CP
                    IF(TRIM(lowcase(symbole(krow)%s)) == TRIM(lowcase(symbole_CP(j)%s)) ) THEN
                      SDWert_CP(j) = missingval
                      exit
                    end if
                  end do
                end if

              case ('double')
                ftextcp%s = gtext%s
                i1 = index(ftextcp%s,',')
                if(i1 > 0) then
                  ftextcp%s(i1:i1) = '.'
                end if
                read(ftextcp%s,*,iostat=ios) dval16
                if(ios == 0) then
                  if(trim(frmtv) == trim(frmt) .and. dval16 < 0.10_rn) frmtv = frmt_min1
                  ! write(gtext%s,frmtv) real(dval16,8)
                  write(cnumb,frmtv) real(dval16,8)       ! 16.8.2023
                  gtext%s = FormatNumStr(trim(cnumb))
                     if(nt > 0) then
                        tv_colwidth_digits(nt,icol1+1) = &
                                      max(tv_colwidth_digits(nt,icol1+1), len_trim(gtext%s))
                     end if
                  call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=gtext%s)
                  call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=gtext%s)
                  SaveP = .true.
                  call FieldUpdate('GUI 1264')
                    if(trim(treename) == 'treeview2') then
                      call clearMCfields(1)
                      call gtk_widget_hide(idpt('window_graphs'))
                    end if
                else
                  if(len_trim(ftextcp%s) == 0) then
                    call hl_gtk_listn_set_cell(tree, row=irow1, col=icol1,  svalue=ftextcp%s)
                    call hl_gtk_listn_get_cell(tree, row=irow1, col=icol1,  svalue=ftextcp%s)
                    SaveP = .true.
                    call FieldUpdate('GUI 1275')
                      if(trim(treename) == 'treeview2') then
                        call clearMCfields(1)
                        call gtk_widget_hide(idpt('window_graphs'))
                      end if
                  else
                    call WTreeViewSetCursorCell(trim(treename), kcol, krow)
                    call gdk_beep()
                  end if
                end if
            end select

          case ('')

        end select
    end select
    TVlastCell = [nt, krow, kcol]
  end do
end do

if(trim(treename) == 'treeview2' .and. SaveP) then
  call WTreeviewGetComboArray('treeview2',6,ngrs,IVTL)
  call WTreeviewGetStrArray('treeview2',7,ngrs,sdformel)
  call WTreeviewGetComboArray('treeview2',10,ngrs,IAR)
  call WTreeviewGetDoubleArray('treeview2',5,ngrs,Messwert)
  call WTreeviewGetDoubleArray('treeview2',8,ngrs,SDWert)
  call WTreeviewGetDoubleArray('treeview2',9,ngrs,HBreite)
  call WTreeviewGetDoubleArray('treeview2',11,ngrs,StdUnc)

  call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
  call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
  call gtk_widget_hide(idpt('box5'))
  call gtk_widget_hide(idpt('grid5'))
end if

deallocate(irow)

if(trim(treename) == 'treeview2') then
  if(.not.use_DP) then
    call WTreeviewGetComboArray('treeview2',6,ngrs,IVTL)
    if(maxval(IVTL) > 8) then
      use_DP = .true.
      call gtk_widget_set_sensitive(idpt('TBDistribDialog'), 1_c_int)
    end if
  end if

  write(chcol,'(i3)') tvcolindex(nt,5)
  cwidth = 12*15
    call gtk_tree_view_column_set_max_width(idpt('treeviewcolumn' // trim(adjustL(chcol))),cwidth )
    call gtk_tree_view_column_set_expand(idpt('treeviewcolumn' // trim(adjustL(chcol))), 1_c_int)
end if

if(.true. .and. trim(treename) == 'treeview8') then
 if(len_trim(gtext%s) > 0) then
   call WTreeViewSetCursorCell(trim(treename),kcol,krow+1,.true.)
 else
   call WTreeViewSetCursorCell(trim(treename),kcol,krow+0,.false.)
 end if
end if

if(trim(treename) == 'treeview1' .and. SaveP) then
  call gtk_widget_set_sensitive(idpt('AcceptAll'), 0_c_int)
  call gtk_widget_set_sensitive(idpt('NBValUnc'), 0_c_int)
  call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
  call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
end if

if(allocated(ftext%s)) deallocate(ftext%s)
if(allocated(ftable)) deallocate(ftable)

   ret = False

end function UR_tree_text_edit_cb

!#############################################################################################

   !//   function UR_field_edit_cb(renderer, path, text, gdata) result(ret) bind(c)
recursive subroutine UR_field_edit_cb(renderer, path, text)  bind(c)      ! result(ret)

   ! this function identifies the widget (field renderer) by its idstring (a name)
   ! and dependent on it performs the necessary actions.

use gtk_hl,           only: hl_gtk_listn_set_cell,gtk_tree_view_get_model, hl_gtk_listn_get_cell, &
                            hl_gtk_combo_box_get_active
use UR_gtk_variables, only: clobj, FieldEditCB, ncitemClicked,list_filling_on,item_setintern
use gtk,              only: gtk_notebook_get_current_page,gtk_widget_set_sensitive,gtk_widget_hide, &
                            gtk_widget_set_state_flags,GTK_STATE_FLAG_NORMAL,gtk_notebook_set_current_page
use UR_variables,     only: saveP,langg,Gum_restricted,gross_negative,kModelType
use UR_gleich,        only: loadingpro,syntax_check,dialogfield_chg, kEGr,knetto,kbrutto,grid1_gleichg_time, &
                            knumEGr,knumold
use UR_Linft,         only: FitDecay,dmodif
use Top,              only: FieldUpdate
use URdate,           only: clockm
use Rout,             only: WDPutLabelString,WDGetCheckButton

implicit none

type(c_ptr), value           :: renderer, path, text
character(len=100)           :: fpath, ftext
character(:),allocatable     :: str1
integer(kind=c_int)          :: indx
integer(4)                   :: i, k, ncitem,ios,nind
character(len=60)            :: idstring,signal,dparent
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

call FindItemP(renderer, ncitem)
if(ncitem > 0) then
  idstring = clobj%idd(ncitem)%s
  signal   = clobj%signal(ncitem)%s
  dparent  = clobj%idd(clobj%idparent(ncitem))%s
  ! write(66,*) 'idstring=',trim(idstring),' signal=',trim(signal),' parent=',trim(dparent) ! deactivated 16.8.2023

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

  if(loadingpro) return
else
  write(66,*) '****** UR_field_edit_cb :  non-associated widget:'
  return
end if
10  continue
     ! write(66,*) '*** Field_edit:  item=',ncitem ,'   ',trim(idstring),'  ',trim(signal) ! (ncitem)   ! ,'  path=',path,'   text=',text

!if(trim(idstring) == 'entryOptAlpha' .or. trim(idstring) == 'entryOptBeta' .or.  &
!  trim(idstring) == 'entryOptKalpha' .or. trim(idstring) == 'entryOptKbeta') then
!  FieldEditCB = .true.
!  ncitemClicked = ncitem
!  call gtk_widget_set_state_flags(clobj%id_ptr(ncitem),GTK_STATE_FLAG_NORMAL,1_c_int)
!  SaveP = .true.
!  call FieldUpdate('GUI 1549')
!  return
!endif

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
if(c_associated(path)) then
  call convert_c_string(path, fpath)
end if

write(ftext,*) text
read(ftext,*,iostat=ios) k

if(k > 100) call c_f_string(text, ftext)
if( (trim(idstring) == 'textview2'.or. trim(idstring) == 'textbufferEQ' )   &
                                            .and. .not.syntax_check) then
  syntax_check = .true.
  dialogfield_chg = 'Equations_main'
elseif( trim(idstring) == 'textviewModelEQ' .and. .not.syntax_check) then
  syntax_check = .true.
  dialogfield_chg = 'Equations_fit'
  SaveP = .true.
  call FieldUpdate('GUI 1507')
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
      if(nind > 0) write(66,*) 'NBvalUnc deactivated:   idstring=',trim(idstring),  &
                                '   signal=',trim(signal),' nind=',nind,'  knetto(kEGr)=',  &
                                knetto(kEGr)! ,' render=',renderer
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
      IF(langg == 'DE') THEN
        str1 = 'Bitte die Hauptgleichung für die '//cnu//'. Ergebnisgröße im Formel-Textfeld einfügen,'
        str1 = TRIM(str1) // char(13) // 'weitere Gleichungen für Hilfsgrößen ebenfalls!'
      else IF( langg == 'EN') THEN
        str1 = 'Please insert the main equation for the '//cnu//'. output quantity in the formula textfield,'
        str1 = TRIM(str1) // char(13) // ' also equations for further auxiliary quantities!'
      else IF( langg == 'FR') THEN
        str1 = 'Veuillez insérer l''équation principale pour la quantité de sortie '//cnu//'. dans le champ de texte de la formule,'
        str1 = TRIM(str1) // char(13) // ' aussi des équations pour d''autres quantités auxiliaires!'
      end if
    end if
    IF(KnumEGr < knumold) THEN
      IF(langg == 'DE') THEN
        str1 = 'Bitte streichen Sie die zur gelöschten Ergebnisgröße gehörenden Zeilen'
        str1 = TRIM(str1) // char(13) // ' im Formel-Textfeld!'
      else IF( langg == 'EN') THEN
        str1 = 'Please delete all equations in the formula text field belonging to the'
        str1 = TRIM(str1) // char(13) // ' output quantity to be to deleted!'
      else IF( langg == 'FR') THEN
        str1 = 'Veuillez supprimer toutes les équations dans le champ de texte de formule appartenant à'
        str1 = TRIM(str1) // char(13) // ' quantité de sortie à supprimer!'
      end if
    end if
    call WDPutLabelString('LBnumegrWarn',TRIM(str1))
  end if
end if

100   continue
if(clobj%name(ncitem)%s == 'GtkEntry') then
  call gtk_widget_set_state_flags(clobj%id_ptr(ncitem),GTK_STATE_FLAG_NORMAL,1_c_int)
end if

   ! ret = True
FieldEditCB = .true.
  if(trim(dparent) == 'dialogDecayModel') then  ! 30.1.2024
    dmodif = .true.
    FieldEditCB = .false.
  end if

  !if(trim(idstring) == 'comboboxDKPgrad') FieldEditCB = .true.
  if(trim(idstring) == 'DKcheckWTLS') FieldEditCB = .true.     ! 7.8.2023
  if(trim(idstring) == 'comboboxA1') FieldEditCB = .true.     ! 7.8.2023
ncitemClicked = ncitem

if(trim(idstring) /= 'notebook1') then
  SaveP = .true.
  call FieldUpdate('GUI 1602')
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

use gtk_hl,           only: hl_gtk_listn_set_cell,gtk_tree_view_get_model, hl_gtk_listn_get_cell
use UR_gtk_variables, only: clobj,FieldDoActCB, ncitemClicked, item_setintern
use UR_variables,     only: saveP
use Top,              only: FieldUpdate,FindItemP

implicit none

type(c_ptr), value        :: renderer, path, text

integer(4)                :: i, ncitem
character(len=60)         :: idstring,signal,parentstr,name
!------------------------------------------------------------------------------------
! When using GTK+ directly, keep in mind that only functions can be connected to signals, not methods.
! So you will need to use global functions or "static" class functions for signal connections.

   ! ret = 1
if(item_setintern) return
ncitem = 0
call FindItemP(renderer, ncitem)

if(ncitem > 0) then
  idstring = clobj%idd(ncitem)%s
  i = clobj%idparent(ncitem)
  parentstr = clobj%name(i)%s
  signal = clobj%signal(ncitem)%s
  name = clobj%name(ncitem)%s
   ! write(66,*) '***** UR_field_doact_cb :  signal=',trim(signal),'  ncitem=',ncitem,' id=',clobj%idd(ncitem)%s

else
  write(66,*) '****** UR_field_doact_cb :  non-associated widget:'     ! ,renderer
  return
end if

    ! ret = True
 FieldDoActCB = .true.
 ncitemClicked = ncitem

 ! Exclude following items from emitting a "SaveP"-action:
 if(trim(idstring) /= 'notebook1' .and. trim(idstring) /= 'doELIplot' .and. &
    trim(idstring) /= 'checkbuttonRS' .and. trim(idstring) /= 'comboboxLangg' .and.  &
    trim(idstring) /= 'radiobutton_bg_color' .and. trim(idstring) /= 'buttonCBOK' .and. &
    trim(idstring) /= 'buttonFBApply' .and. trim(idstring) /= 'buttonCBOK' .and. &
    trim(idstring) /= 'CopyGrELI' .and. trim(idstring) /= 'FillDecColumn' .and.   &
    trim(idstring) /= 'combobox_MDselVar' .and. trim(idstring) /= 'MDCalcMean' .and. &
    trim(idstring) /= 'ChooserButton2SE' .and. trim(idstring) /= 'CheckMCSE' .and.  &
    trim(idstring) /= 'check_contrastmode' .and. trim(idstring) /= 'comboboxtextInfoFX' .and. &
    trim(idstring) /= 'URfunctions' .and. trim(idstring) /= 'ExportToR' .and.    &
    trim(idstring) /= 'checkAbsTime' .and. trim(idstring) /= 'comboboxtextInfoFX' .and. &
    trim(idstring) /= 'HelpFX' ) then        ! 25.2.2024
   SaveP = .true.
   call FieldUpdate('GUI 1759')
 end if

end subroutine UR_field_doact_cb

!##########################################################################

    !//  function UR_tree_toggle_edit_CB(renderer, path, gdata) Result(ret) bind(c)
subroutine UR_tree_toggle_edit_CB(renderer, path, gdata) bind(c)     ! Result(ret)

   ! this routine identifies the toggle request from a widget (renderer)
   ! and its idstring (a name) and executes the toggleing
   !

use gtk,             only: gtk_cell_renderer_toggle_get_active
use gtk_hl,          only: hl_gtk_listn_set_cell, hl_gtk_listn_get_cell
use UR_gtk_variables,only: toggleTypeGTK,item_setintern,clobj
use UR_variables,    only: SaveP        ! ,actual_grid
use top,             only: idpt,FieldUpdate,FindItemP

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
integer(4)                       :: i,ncitem,ncol

    ! ret = 1
if(item_setintern) return
call FindItemP(renderer, ncitem)
call convert_c_string(path, fpath)
read(fpath, *) irow

ncol = 1

if(clobj%idd(ncitem)%s == 'cellrenderertext64') then
  list = idpt('liststore_gspk1')
  tree = idpt('treeview6')
end if
icol1 = 1
irow1 = irow

select case (toggleTypeGTK)
  case ('text')
    call hl_gtk_listn_get_cell(tree, row=irow1, col=ncol,  svalue=str)
        stateold = .false.
        if(str == 'T') stateold = .true.
             statenew = .not.stateold
        str = 'F'
        if(statenew) str = 'T'
    call hl_gtk_listn_set_cell(tree, row=irow1, col=ncol,  svalue=str)
    str = ' '
    call hl_gtk_listn_get_cell(tree, row=irow1, col=ncol,  svalue=str)

  case ('bool')

    state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
    call hl_gtk_listn_set_cell(tree, irow, ncol, &
                                  & logvalue= .not. state)
         state = c_f_logical(gtk_cell_renderer_toggle_get_active(renderer))
  case default
end select

  SaveP = .true.
  call FieldUpdate('GUI 1688')

end subroutine UR_tree_toggle_edit_CB

!#############################################################################################

    !//   function UR_NBPage_switched_cb(renderer, path, ppage, gdata) result(ret) bind(c)
recursive subroutine UR_NBPage_switched_cb(renderer, path, ppage) bind(c)     ! result(ret)

   ! this routine identifies the notebook by the renderer pointer and sets
   ! the requestes page (from ppage) and highlights itby the its idstring (a name)
   ! and sets the following two variables:

use UR_gtk_variables, only: clobj, PageSwitchedCB, ncitemClicked,NBsoftSwitch, &
                            item_setintern
use UR_gleich,        only: loadingpro
use UR_Loadsel,       only: NBpreviousPage, NBcurrentPage
use gtk,              only: gtk_widget_is_sensitive,gtk_notebook_set_current_page,&
                            gtk_notebook_set_tab_pos
use Rout,             only: NBlabelmodify,pending_events
use top,              only: idpt
! use UR_interfaces,    only: ProcMainDiag
use PMD,              only: ProcMainDiag
use UR_gtk_variables, only: consoleout_gtk,switched_ignore

implicit none

type(c_ptr), value             :: renderer, path, ppage
integer(c_int), target         :: pagenum
integer(4),pointer             :: fppage

integer(4)                     :: i,ipage,ncp,ncpr,ncitem2
integer(4)                     :: ncitem
character(len=60)              :: idstring,signal,parentstr,name
character(len=30), target      :: pstr

!------------------------------------------------------------------------------------
! When using GTK+ directly, keep in mind that only functions can be connected to signals, not methods.
! So you will need to use global functions or "static" class functions for signal connections.

if(item_setintern) return
if(NBsoftSwitch) then
  return
end if

call FindItemP(renderer, ncitem)
if(ncitem > 0) then
  idstring = clobj%idd(ncitem)%s
  i = clobj%idparent(ncitem)
  parentstr = clobj%name(i)%s
  signal = clobj%signal(ncitem)%s
  name = clobj%name(ncitem)%s

  if(loadingpro) return
else
  if(switched_ignore) then
    switched_ignore = .false.
    return
  end if
  write(66,'(a,i11)') '****** UR_PageSwitched_cb :  not associated widget:', renderer
  return
end if

call FindItemP(path, ncitem2)   ! path contains: box4, grid5, ...

write(pstr,*) ppage        ! does not work under 2003-, 2008-Standard
read(pstr,*) pagenum

    call c_f_pointer(ppage, fppage)     ! <-- works, with: integer(4),pointer :: fppage
    fppage => pagenum

  if(consoleout_gtk) write(0,*) '*** NBPage-Switched:  item=',ncitem,'   ',clobj%idd(ncitem)%s, &        !  '  ppage=',ppage, &
                             '  pagenum=',pagenum,'  loadingpro=',loadingpro

if(trim(idstring) == 'notebook1' .and. len_trim(signal) > 0) then
  ncpr = NBpreviousPage
  ! current page:
  ipage = pagenum + 1
  ncp = NBcurrentPage
  if(ipage /= ncp) then
    NBpreviousPage = ncp
    NBcurrentPage = ipage
    if(ipage == 3 .and. gtk_widget_is_sensitive(idpt('NBValUnc')) == 0_c_int) call NBlabelmodify()
    if(ipage == 4 .and. gtk_widget_is_sensitive(idpt('NBBudget')) == 0_c_int) call NBlabelmodify()
    if(ipage == 5 .and. gtk_widget_is_sensitive(idpt('NBResults')) == 0_c_int) call NBlabelmodify()
  end if
  call ProcMainDiag(ncitem)

end if

   ! ret = True
PageSwitchedCB = .true.
ncitemClicked = ncitem

end subroutine UR_NBPage_switched_cb

!#############################################################################################

   ! function UR_ActualTreeV_CB(renderer, path, text, gdata) result(ret) bind(c)
subroutine UR_ActualTreeV_CB(renderer, path, text, gdata)  bind(c)    ! result(ret)

      ! this routine identifies the actual treeview as string variable
      ! actual_grid and also finds out which rows (variable numrows-marked) in
      ! that grid (treeview) have been marked by the user.

use UR_gtk_variables, only: clobj,item_setintern
use UR_variables,     only: actual_grid
use gtk_hl,           only: hl_gtk_listn_get_selections
use gtk,              only: gtk_tree_view_scroll_to_cell,gtk_tree_path_to_string, &
                            gtk_tree_path_get_type,gtk_tree_path_new,    &
                            gtk_tree_path_new_from_string, gtk_tree_view_scroll_to_point, &
                            gtk_tree_view_set_cursor,gtk_tree_view_get_visible_range, &
                            gtk_widget_grab_focus,gtk_tree_path_get_indices,   &
                            gtk_tree_path_get_depth,gtk_tree_selection_select_path,  &
                            gtk_tree_view_get_cursor,gtk_tree_model_get_string_from_iter, &
                            gtk_tree_view_get_model,gtk_tree_path_new_from_string, &
                            gtk_tree_path_free,gtk_tree_view_get_column
use g,                only: g_object_get_data,g_type_name,g_value_take_string

implicit none

type(c_ptr), value           :: renderer, text,path, gdata
integer(kind=c_int)          :: ret
integer(4)                   :: i, ncitem
character(len=60)            :: idstring,name,parentstr,signal
integer(kind=c_int), allocatable   :: rownums_marked(:)
integer(4)                   :: numrows_marked,krow
!------------------------------------------------------------------------------------
! When using GTK+ directly, keep in mind that only functions can be connected to signals, not methods.
! So you will need to use global functions or "static" class functions for signal connections.

!   path is expected to be a colon separated list of numbers.
!   For example, the string 10:4:0 would create a path of depth 3 pointing to
!   the 11th child of the root node, the 5th child of that 11th child, and the
!   1st child of that 5th child. If an invalid path string is passed in, NULL is returned.

    ret = 1
if(item_setintern) return

    ret = 0
call FindItemP(renderer, ncitem)
if(ncitem > 0) then
  idstring = clobj%idd(ncitem)%s
  i = clobj%idparent(ncitem)
  parentstr = clobj%name(i)%s
  signal = clobj%signal(ncitem)%s
  name = clobj%name(ncitem)%s
else
  write(66,*) '****** UR_ActualTreeV_cb :  nicht zugeordnetes widget:'     ! ,renderer
  return
end if

if(trim(Name) == 'GtkTreeSelection') then
  actual_grid = ' '
  if(trim(idstring) == 'treeview-selectionTV1') actual_grid = 'treeview1'
  if(trim(idstring) == 'treeview-selectionTV2') actual_grid = 'treeview2'
  if(trim(idstring) == 'treeview-selectionTV3') actual_grid = 'treeview3'
  if(trim(idstring) == 'treeview-selectionTV4') actual_grid = 'treeview4'
  if(trim(idstring) == 'treeview-selectionTV5') actual_grid = 'treeview5'
  if(trim(idstring) == 'treeview-selectionTV6') actual_grid = 'treeview6'
  if(trim(idstring) == 'treeview-selectionTV7') actual_grid = 'treeview7'
  if(trim(idstring) == 'treeview-selectionTV8') actual_grid = 'treeview8'
      ret = TRUE
end if
if(len_trim(actual_grid) == 0) then
  return
else
  krow = 0
  numrows_marked = hl_gtk_listn_get_selections(idpt(actual_grid), rownums_marked)
  if(numrows_marked == 1) krow = minval(rownums_marked) + 1
  return
end if

end subroutine UR_ActualTreeV_CB

!#############################################################################################

subroutine UR_TV_column_clicked_cb(renderer, path, text)  bind(c)     !  result(ret)

    ! this routine identfies which treeview column was clicked
    !  - and does not do any more significant

use UR_gtk_variables, only: clobj

implicit none

type(c_ptr), value         :: renderer, path, text  ! , gdata

integer(4)            :: i, ncitem
character(len=60)     :: idstring,signal,parentstr,name,fpath,ftext
logical               :: pout

pout = .false.
  pout = .true.

   ! ret = 0
  if(pout) write(66,*) 'UR_TV_column_clicked_cb  arrived at'
call FindItemP(renderer, ncitem)
if(ncitem > 0) then
  idstring = clobj%idd(ncitem)%s
  i = clobj%idparent(ncitem)
  parentstr = clobj%name(i)%s
  signal = clobj%signal(ncitem)%s
  name = clobj%name(ncitem)%s
   if(pout) write(66,*) '****** UR_TV_column_clicked_cb : idstring=',trim(idstring),'  path=',path,' text=',text
else
  if(pout) write(66,*) '****** UR_TV_column_clicked_cb :  nicht zugeordnetes widget:'     ! ,renderer
  return
end if

call convert_c_string(path, fpath)
 if(pout) write(66,*) '****** UR_TV_column_clicked_cb : fpath=',trim(fpath)

call convert_c_string(text, ftext)
 if(pout) write(66,*) '****** UR_TV_column_clicked_cb : ftext=',trim(ftext)

end subroutine UR_TV_column_clicked_cb

!#############################################################################################


!----------------------------------------------------------------------------------------------------


!#############################################################################################

subroutine SetColors()

      ! sets the colors for various widgets, dependent on the contrast mode
      ! chosen in UR2cfg.dat. Several entry fields are set markable and whether
      ! the can grab focus or not.

use UR_gtk_variables,     only: clobj,nclobj,entry_bg,entry_fg,label_fg, &
                                contrast_mode,entry_markle,entry_mark_fg, &
                                entry_mark_bg,frame_fg,frame_bg,provider      ! ,table_bg
use gtk,                  only: GTK_STATE_FLAG_NORMAL,gtk_widget_set_focus_on_click, &
                                gtk_widget_set_sensitive,gtk_entry_set_has_frame, &
                                gtk_entry_grab_focus_without_selecting, &
                                GTK_STATE_FLAG_INSENSITIVE,gtk_widget_is_sensitive, &
                                gtk_widget_get_state_flags,gtk_label_set_attributes, &
                                gtk_css_provider_load_from_data,gtk_widget_override_cursor, &              ! gtk_text_view_set_cursor_visible
                                gtk_text_view_reset_cursor_blink,gtk_text_view_set_cursor_visible

use Rout,                 only: pending_events,WDPutLabelColorB,WDPutLabelColorF

implicit none

logical             :: entry_focus
integer(4)          :: i,i1mark
character(len=7)    :: colorname
character(len=:),allocatable  :: str1
integer(c_int)      :: res,res2
type(c_ptr),target  :: cerror

allocate(character(len=100) :: str1)

  cerror = c_null_ptr
if(contrast_mode) then
  str1 = ' .button, filechooser entry { color: white; background: #5A5A5A; } ' // &
         ' .textview, textview text { color: white; background-color: black; } ' // &
         ' .treeview.view header button { color: white; background-color: #4A4A4A; } ' // &
         ' input, entry, textview { caret-color: white; border-style: solid} ' // &
         ' box.linked > button.combo > box > button, cellview { color: white;  background-color: #1d1d1d } '
else
  str1 = '.button, filechooser entry { color: black; background: #ECECE9; } ' // &
         '.button:disabled { color: #F1F1BE; }'  // &
         ' .textview text { color: black; background-color: white; } ' // &
         ' .treeview.view header button { color: black; background-color: white; } ' // &   ! klappt
         ' input, entry, textview { caret-color: black; border-style: solid} ' // &
         ' box.linked > button.combo > box > button, cellview { color: black;  background-color: white } '
end if

res = gtk_css_provider_load_from_data(provider, trim(str1)//c_null_char,  &
                                         -1_c_size_t,c_loc(cerror) )
 if(c_associated(cerror)) then
   call EvalGerror('Load css from data:  errormessage=',cerror)
   write(0,*) 'Load css from data:  error: see file fort66.txt'
 end if

! entry_markle identifies markable entries; other entries will not be markable
entry_markle = ' '
entry_markle(1) = 'entryNetBlindVal'
entry_markle(2) = 'entrySeparationXX'
entry_markle(3) = 'entry_b2LFactor'
entry_markle(4) = 'entryDecaycolVal'
entry_markle(5) = 'entryOptKalpha'
entry_markle(6) = 'entryOptKbeta'
entry_markle(7) = 'entryOptAlpha'
entry_markle(8) = 'entryOptBeta'
entry_markle(9) = 'entryOpt1minusG'
entry_markle(10) = 'entryOptCoverf'
entry_markle(11) = 'entryOptCoverIn'
entry_markle(12) = 'entryOptDLMethod'
entry_markle(13) = 'entryOptGamDistAdd'
entry_markle(14) = 'TRentryMCanzM'
entry_markle(15) = 'TRentryMCanzR'
entry_markle(16) = 'TRentryMCanzM1'
entry_markle(17) = 'TRentryMCanzR1'
entry_markle(18) = 'DistribEntry1'
entry_markle(19) = 'DistribEntry2'
entry_markle(20) = 'DistribEntry3'
entry_markle(21) = 'DistribEntry4'
entry_markle(22) = 'entrySymbchg'
entry_markle(23) = 'entryDKTitel'

do i=1,nclobj

  if(clobj%name(i)%s == 'GtkTextView' .or. clobj%idd(i)%s == 'window1') then
    call gtk_widget_set_focus_on_click(clobj%id_ptr(i), 1_c_int)
    call gtk_widget_set_sensitive(idpt(clobj%idd(i)%s), 1_c_int)

  end if

  if(clobj%name(i)%s == 'GtkMenu' ) then
    ! Do not include GtkMenuItem here!
    res = gtk_widget_is_sensitive(clobj%id_ptr(i))
    res2 = gtk_widget_get_state_flags(clobj%id_ptr(i))

    if(.not.contrast_mode) then
      if(res == 1_c_int) then    ! is sesnsitive
        call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
        call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#000000")
      else
        call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_INSENSITIVE, "#FFFFFF")
        call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_INSENSITIVE, "#B0B0B0")
      end if
    else
      if(res == 1_c_int) then
        call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#3D3D3D")
        call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
      else
        call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_INSENSITIVE, "#3D3D3D")
        call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_INSENSITIVE, "#C5C5C5")
      end if
    end if
    cycle
  elseif (clobj%name(i)%s == 'GtkMenuBar') then
    res = gtk_widget_is_sensitive(clobj%id_ptr(i))
    if(.not.contrast_mode) then
      call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#000000")
    else
      call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#1D1D1D")
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
    end if
    cycle
  else if(clobj%name(i)%s == 'GtkToolbar') then
    if(.not.contrast_mode) then
      call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#F6F5F0")
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#000000")
      call WDPutLabelColorB('grid42',GTK_STATE_FLAG_NORMAL, "#F6F5F0")
    else
      call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#9D9D9D")
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
      call WDPutLabelColorB('grid42',GTK_STATE_FLAG_NORMAL, "#9D9D9D")
    end if
    cycle
  else if(clobj%name(i)%s == 'GtkNotebook') then
    if(.not.contrast_mode) then
      call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#E2FFFA")
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#000000")
    else
      call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#2E2E2E")
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
    end if
    cycle
  else if(clobj%name(i)%s == 'GtkRadioButton') then
    if(.not.contrast_mode) then
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#000000")
    else
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
    end if
    cycle
  else if(clobj%name(i)%s == 'GtkTreeView') then
    if(.not.contrast_mode) then
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#000000")
    else
      call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FFFFFF")
    end if
    cycle
  else if(clobj%name(i)%s == 'GtkScrolledWindow') then
    cycle
  else if(clobj%name(i)%s == 'GtkFileChooserButton') then
    cycle
  end if

  if( clobj%name(i)%s == 'GtkFrame') then
    call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL,frame_bg)
    call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL,frame_fg)
  end if

  if( clobj%name(i)%s == 'GtkLabel'  .or.    &
      clobj%name(i)%s == 'GtkCheckButton' .or.  &
      clobj%name(i)%s == 'GtkStatusbar' ) then

    call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL,label_fg)  !  "#e5a50a")
  end if

  if(clobj%name(i)%s == 'GtkEntry') then
    call gtk_entry_set_has_frame(clobj%id_ptr(i), 0_c_int)
       ! exception:  24.7.2023, 3.8.2023
       if(clobj%idd(i)%s == 'entrySeparation') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
       if(clobj%idd(i)%s == 'entryDecaycolVal') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
       if(clobj%idd(i)%s == 'entryFormula') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
       if(clobj%idd(i)%s == 'entry_b2LFactor') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
       if(clobj%idd(i)%s == 'DistribEntry1') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
       if(clobj%idd(i)%s == 'DistribEntry2') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
       if(clobj%idd(i)%s == 'DistribEntry3') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
       if(clobj%idd(i)%s == 'DistribEntry4') call gtk_entry_set_has_frame(clobj%id_ptr(i), 1_c_int)
    entry_focus = &
       clobj%idd(i)%s == 'TRentryMCanzM' .or. clobj%idd(i)%s == 'TRentryMCanzM1' .or. &
       clobj%idd(i)%s == 'TRentryMCanzR' .or. clobj%idd(i)%s == 'TRentryMCanzR1' .or. &
       clobj%idd(i)%s == 'entrySymbchg'  .or. clobj%idd(i)%s == 'entryDKTitel'   .or. &
       clobj%idd(i)%s == 'entry_b2LFactor' .or. clobj%idd(i)%s == 'entryNetBlindVal' .or. &
       clobj%idd(i)%s == 'entrySeparationXX' .or. index(clobj%idd(i)%s, 'entryOpt')> 0 .or.   &
       clobj%idd(i)%s == 'entryFormula' .or. clobj%idd(i)%s == 'entry_b2LFactor' .or.  &
       clobj%idd(i)%s == 'entryDecaycolVal' .or. clobj%idd(i)%s == 'FilenameSE' .or.   &
       clobj%idd(i)%s == 'EntryRunsMCSE' .or.                                          &
       clobj%idd(i)%s == 'EntryMCnumSE' .or.                                           &
       clobj%idd(i)%s == 'entryFromSE' .or. clobj%idd(i)%s == 'entryToSE'  .or.        &
       clobj%idd(i)%s == 'EntryRunsMCBEV' .or.                                         &
       clobj%idd(i)%s == 'EntryMCnumBEV' .or.                                          &
       clobj%idd(i)%s == 'entryFromBEV' .or. clobj%idd(i)%s == 'entryToBEV'  .or.   &
       clobj%idd(i)%s == 'DistribEntry1' .or. clobj%idd(i)%s == 'DistribEntry2' .or.   &
       clobj%idd(i)%s == 'DistribEntry3' .or. clobj%idd(i)%s == 'DistribEntry4' .or. &
       clobj%idd(i)%s == 'entryDKTitel'

    if(entry_focus) then
      i1mark = Findloc(entry_markle,clobj%idd(i)%s,dim=1)
      if(i1mark > 0)then
        ! focus and markable:
        call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, entry_mark_fg)
        call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, entry_mark_bg)
      end if

      call gtk_widget_set_focus_on_click(clobj%id_ptr(i), 1_c_int)
      call gtk_widget_set_sensitive(idpt(clobj%idd(i)%s), 1_c_int)

      if(index(lowcase(clobj%idd(i)%s),'entry') > 0) then
        call gtk_entry_grab_focus_without_selecting(clobj%id_ptr(i))
      end if

      cycle
    else
      ! entry without focus, not markable:
      if(clobj%idd(i)%s /= 'entrySeparationXX') then
        call gtk_widget_set_sensitive(idpt(clobj%idd(i)%s), 0_c_int)
        call WDPutLabelColorF(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, entry_fg)
        call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, entry_bg)
      end if
    end if
  end if

end do

   colorname = "#FFFFFF"       ! white
   if(contrast_mode) colorname = "#1D1D1D"
   call WDPutLabelColorB('box1',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box2',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box3',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box4',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box5',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('grid5',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('grid7',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('grid34',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('grid35',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box8',GTK_STATE_FLAG_NORMAL, colorname)

   call WDPutLabelColorB('box7',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box9',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box13',GTK_STATE_FLAG_NORMAL,colorname)

   call WDPutLabelColorB('dialog-vbox17',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox21',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox6',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox9',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox13',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox1',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox4',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox2',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox5',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox11',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox15',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox2',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box6',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box27',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box12',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box45',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box23',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box26',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box14',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('boxELI',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('boxDistrib',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('boxBatEval',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('BTBox1',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('boxSerEval',GTK_STATE_FLAG_NORMAL,colorname)

   colorname = "#FCFCFC"
   if(contrast_mode) colorname = "#4D4D4D"
   call WDPutLabelColorB('grid26',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box14',GTK_STATE_FLAG_NORMAL,colorname)

   call WDPutLabelColorB('dialog-vbox2',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('box23',GTK_STATE_FLAG_NORMAL,colorname)
   call WDPutLabelColorB('dialog-vbox4',GTK_STATE_FLAG_NORMAL,colorname)

do i=1,nclobj
  exit
  ! make the GtkButton colors more intensive
  if(trim(clobj%name(i)%s) == 'GtkButton') then
    call WDPutLabelColorB(clobj%idd(i)%s,GTK_STATE_FLAG_NORMAL, "#FABDB6")
  end if
end do

    write(0,*) 'End SetColors'

end subroutine SetColors

!#############################################################################################

  elemental function lowcase(string)

    ! turns all characters in the string to lower case.

    implicit none
    character(len=*), intent(in) :: string
    character(len=len(string))   :: lowcase

    integer(4), parameter :: ucmin = iachar('A'), ucmax = iachar('Z')
    integer(4), parameter :: case_diff = iachar('A') - iachar('a')
    integer(4) :: i, ic

    lowcase = string
    do i = 1, len(string)
       ic = iachar(string(i:i))
       if (ic >= ucmin .and. ic <= ucmax) lowcase(i:i) = achar(ic-case_diff)
    end do
  end function lowcase

!#############################################################################################

recursive subroutine UR_Window_size_alloc_clicked_cb(renderer, path, text)  bind(c)      ! result(ret)

   ! this function identifies the widget (field renderer) by its idstring (a name)
   ! and dependent on it performs the necessary actions.

use UR_gtk_variables,   only: clobj, ncitemClicked,item_setintern_window1
USE gtk,                only: gtk_widget_get_allocated_width,gtk_widget_get_allocated_height, &
                              gtk_widget_set_size_request
use top,                only: idpt
use gtk_draw_hl,        only: gtkallocation

implicit none

type(c_ptr), value           :: renderer, path, text
character(len=100)           :: fpath, ftext
! character(len=40),pointer    :: ftextp
integer(4),pointer           :: fintp(:)
character(:),allocatable     :: str1
integer(kind=c_int)          :: indx,width,height
integer(4)                   :: i, k, ncitem,ios,nind
character(len=60)            :: idstring,signal,dparent
CHARACTER(LEN=1)             :: cnu
type(c_ptr)                  :: ctext
type(gtkallocation),target   :: alloc
!------------------------------------------------------------------------------------
! When using GTK+ directly, keep in mind that only functions can be connected to signals, not methods.
! So you will need to use global functions or "static" class functions for signal connections.

!   Signal:  GtkWidget:  size-aAllocate,  SizeAlloc

     return  !!!!!!!!!!

if(item_setintern_window1) then
  return
end if
allocate(character(len=50)  ::str1)

        write(66,*) 'renderer=',renderer

call FindItemP(renderer, ncitem)
    write(66,*) 'ncitem=',ncitem
if(ncitem > 0) then
  idstring = clobj%idd(ncitem)%s
  signal   = clobj%signal(ncitem)%s
  ! dparent  = clobj%idd(clobj%idparent(ncitem))%s
       write(66,*) 'idstring=',trim(idstring),', signal=',trim(signal) ! ' parent=',trim(dparent) ! deactivated 16.8.2023

end if

 alloc%width = gtk_widget_get_allocated_width(idpt('window1'))
 alloc%height = gtk_widget_get_allocated_height(idpt('window1'))
  write(66,*) 'width, height=',alloc%width, alloc%height
 ! item_setintern_window1 = .true.
  call gtk_widget_set_size_request(idpt('window1'),alloc%width,alloc%height)

  return


call convert_c_string(path, fpath)
 ! if(pout)
   write(66,*) '****** UR_Window_size_alloc_clicked_cb : fpath=',trim(fpath)

call convert_c_string(text, ftext)
  ! if(pout)
  write(66,*) '****** UR_Window_size_alloc_clicked_cb : ftext=',trim(ftext)

call c_f_string(text,ftext)
  ! if(pout)
  write(66,*) '****** UR_Window_size_alloc_clicked_cb : ftext=',trim(ftext)

call c_f_pointer(text, fintp, [3])
   write(66,*) 'fintp=',fintp

end subroutine UR_Window_size_alloc_clicked_cb

!#############################################################################################

end module gui_functions

!#############################################################################################

!Don't modify the c_f_string_chars subroutine
subroutine c_f_string_chars(c_string, f_string)
! Helper function
use, intrinsic :: iso_c_binding
implicit none
character(len=1,kind=c_char), intent(in) :: c_string(*)
character(len=*), intent(out) :: f_string
integer(4) :: i
i=1
do while(c_string(i)/=c_null_char .and. i<=len(f_string))
    f_string(i:i) = c_string(i)
    i=i+1
end do
if (i<=len(f_string)) f_string(i:) = ' '
end subroutine c_f_string_chars

!#############################################################################################

