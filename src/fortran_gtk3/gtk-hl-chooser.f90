! Copyright (C) 2011
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran GTK+ Fortran Interface library.

! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.

! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.

! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Last modification: 2012-12-31

! --------------------------------------------------------
! gtk-hl-chooser.f90
! Generated: Fri Jan 31 09:38:38 2020 GMT
! Generated for GTK+ version: 3.24.0.
! Generated for GLIB version: 2.62.0.
! --------------------------------------------------------


!*
! File Choosers
module gtk_hl_chooser
  ! hl_gtk_file_chooser_button_new implements the GtkFileChooserButton
  ! and its GtkFileChooser options in a convenient package.
  !
  ! hl_gtk_file_chooser_new and _show implement a more general chooser
  ! dialogue via the file_chooser_widget (file_choose_dialog only has
  ! variadic constructors). Unless you need to add extra items to the
  ! dialog it is usually easiest to use the hl_gtk_file_chooser_show function.
  !
  ! Filters may be either patterns (e.g. '*.f90' or '2011*.lis') or mime types
  ! (e.g. 'image/png' or 'text/*'). The constructors recognise the difference by
  ! the presence or absence of a '/' character. Each filter is a
  ! comma-separated list, which may contain any mixture of patterns and mime
  ! types (e.g. '*.png,image/tiff,*.jpg'). If a name is not provided, then
  ! the filter specification is used as the name.
  !/

  use gtk_sup
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: error_unit

  ! Auto generated use's
  use g, only: g_free, g_slist_free, g_slist_length, g_slist_nth_data

  use gtk, only: gtk_box_pack_start, gtk_dialog_add_button, &
       & gtk_dialog_get_content_area, gtk_dialog_new, gtk_dialog_run, &
       & gtk_entry_set_text, gtk_file_chooser_add_filter, &
       & gtk_file_chooser_button_new, gtk_file_chooser_button_set_width_chars, &
       & gtk_file_chooser_get_current_folder, gtk_file_chooser_get_filenames, &
       & gtk_file_chooser_get_local_only, gtk_file_chooser_get_uris, &
       & gtk_file_chooser_select_filename, &
       & gtk_file_chooser_set_current_folder, &
       & gtk_file_chooser_set_current_name, &
       & gtk_file_chooser_set_do_overwrite_confirmation, &
       & gtk_file_chooser_set_extra_widget, gtk_file_chooser_set_local_only, &
       & gtk_file_chooser_set_filename, &
       & gtk_file_chooser_set_select_multiple, &
       & gtk_file_chooser_set_show_hidden, gtk_file_chooser_widget_new, &
       & gtk_file_filter_add_mime_type, gtk_file_filter_add_pattern, &
       & gtk_file_filter_new, gtk_file_filter_set_name, gtk_label_new, &
       & gtk_widget_destroy, gtk_widget_set_sensitive, &
       & gtk_widget_set_tooltip_text, gtk_widget_show_all, &
       & gtk_window_set_default_size, gtk_window_set_destroy_with_parent, &
       & gtk_window_set_modal, gtk_window_set_title, &
       & gtk_window_set_transient_for, g_signal_connect, TRUE, FALSE, &
       & GTK_RESPONSE_DELETE_EVENT, GTK_RESPONSE_CANCEL, GTK_RESPONSE_APPLY, &
       & GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE, &
       & GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, &
       & GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER, &
       & gtk_window_set_transient_for                       ! GK

  ! Building the chooser uses a number of other high-level interfaces.

  use gtk_hl_container
  use gtk_hl_entry
  use gtk_hl_button
  use TOP,   only:   idpt                       ! GK

  implicit none

  !+
  type, bind(c) :: hl_gtk_chooser_info
     type(c_ptr) :: chooser=C_NULL_PTR, chooser_sel_list=C_NULL_PTR
     type(c_ptr) :: chooser_curdir=C_NULL_PTR, fentry=C_NULL_PTR
     integer(kind=c_int) :: iselect=0
  end type hl_gtk_chooser_info

  ! These items must be shared between the file chooser widget and its event
  ! handler or the filter editor. They are passed to the signal handlers
  ! via the user data argument. Even though it's never used in the C code,
  ! it still has to be bind(c) otherwise c_loc() will croak on it.
  !-

contains
  !+
  function hl_gtk_file_chooser_button_new(directory, title, &
       & width, show_hidden, initial_dir, current, &
       & initial_folder, initial_file, filter, filter_name, file_set, &
       & data, sensitive, tooltip) result(cbutton)

    type(c_ptr) :: cbutton
    integer(kind=c_int), intent(in), optional :: directory
    character(kind=c_char), dimension(*), optional, intent(in) :: title
    integer(kind=c_int), intent(in), optional :: width
    integer(kind=c_int), intent(in), optional :: show_hidden, current
    character(kind=c_char), dimension(*), optional, intent(in) :: &
         & initial_folder, initial_file, initial_dir
    character(len=*), dimension(:), intent(in), optional :: filter
    character(len=*), dimension(:), optional, intent(in) :: filter_name
    type(c_funptr), optional :: file_set
    type(c_ptr), optional :: data
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), optional, intent(in) :: tooltip

    ! Bundled file chooser button
    !
    ! DIRECTORY: boolean: optional: Set to TRUE to select directories rather
    ! 		than files.
    ! TITLE: string: optional: A title for the button.
    ! WIDTH: c_int: optional: A maximum number of characters to show.
    ! SHOW_HIDDEN: boolean: optional: Set to TRUE to display hidden files.
    ! INITIAL_DIR: string: optional: Use to start the search other than
    ! 		in the current directory. (INITIAL_FOLDER is a deprecated
    ! 		alias).
    ! CURRENT: boolean: optional: Use to force start in current directory.
    ! INITIAL_FILE: string: optional: An initial file selection.
    ! FILTER: string(): optional: An initial list of filename patterns to
    ! 		allow. Each filter is a comma-separated list.
    ! FILTER_NAME: string(): optional: Names for the filters.
    ! FILE_SET: f_funptr: optional: The callback routine for the "file-set"
    ! 		signal.
    ! DATA: c_ptr: optional: User data to pass to the file_Set callback.
    ! SENSITIVE: boolean: optional: Set to FALSE to make the widget start in an
    ! 		insensitive state.
    ! TOOLTIP: string: optional: A tooltip to display when the pointer is
    ! 		held over the widget.
    !-

    integer(kind=c_int) :: mode, lval
    type(c_ptr) :: gfilter
    integer :: i, idx0, idx1

    if (present(directory)) then
       if (directory == TRUE) then
          mode = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
       else
          mode = GTK_FILE_CHOOSER_ACTION_OPEN
       end if
    else
       mode = GTK_FILE_CHOOSER_ACTION_OPEN
    end if

    if (present(title)) then
       cbutton = gtk_file_chooser_button_new(title, mode)
    else  if (mode == GTK_FILE_CHOOSER_ACTION_OPEN) then
       cbutton = gtk_file_chooser_button_new("Choose file"//c_null_char, mode)
    else
       cbutton = gtk_file_chooser_button_new("Choose directory"//c_null_char,&
            & mode)
    end if

    call gtk_file_chooser_set_local_only(cbutton, TRUE)

    if (present(show_hidden)) then
       lval = show_hidden
    else
       lval = FALSE
    end if
    call gtk_file_chooser_set_show_hidden(cbutton, lval)

    if (present(width)) call &
         & gtk_file_chooser_button_set_width_chars(cbutton, width)

    if (present(initial_dir)) then
       lval = gtk_file_chooser_set_current_folder(cbutton, initial_dir)
    else if (present(initial_folder)) then
       lval = gtk_file_chooser_set_current_folder(cbutton, initial_folder)
       write(error_unit, *) "HL_GTK_FILE_CHOOSER_BUTTON_NEW:: "// &
            & "INITIAL_FOLDER is deprecated, INITIAL_DIR is preferred"
    else if (present(current)) then
       if (c_f_logical(current)) &
            & lval = gtk_file_chooser_set_current_folder(cbutton, "."//c_null_char)
    end if
    if (present(initial_file)) &
         & lval = gtk_file_chooser_set_filename(cbutton, initial_file)

    if (present(filter)) then
       do i = 1, size(filter)
          gfilter = gtk_file_filter_new()

          idx0 = 1
          do
             idx1 = index(filter(i),',')-2
             if (idx1 < 0) then
                if (index(filter(i)(idx0:), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//c_null_char)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//c_null_char)
                end if
                exit
             else
                if (index(filter(i)(idx0:idx1), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//c_null_char)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//c_null_char)
                end if
                idx0=idx1+2
             end if
          end do
          if (present(filter_name)) then
             call gtk_file_filter_set_name(gfilter, filter_name(i)//c_null_char)
          else
             call gtk_file_filter_set_name(gfilter, &
                  & trim(filter(i))//c_null_char)
          end if
          call gtk_file_chooser_add_filter(cbutton, gfilter)
       end do
    end if

    if (present(file_set)) then
       if (present(data)) then
          call g_signal_connect(cbutton, "file-set"//c_null_char,&
               & file_set, data)
       else
          call g_signal_connect(cbutton, "file-set"//c_null_char, file_set)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(cbutton, &
         & tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(cbutton, sensitive)
  end function hl_gtk_file_chooser_button_new

  !+
  function hl_gtk_file_chooser_new(chooser_info, cdir, directory, create, &
       & multiple, allow_uri, show_hidden, confirm_overwrite, title, &
       & initial_dir, current, initial_file, filter, filter_name, parent, &
       & all, wsize, edit_filters) result(dialog)


    use UR_VARIABLES,   only: langg                    ! GK

    type(c_ptr) :: dialog
    type(hl_gtk_chooser_info), intent(out), target :: chooser_info
    character(len=*), intent(out), optional :: cdir
    integer(kind=c_int), intent(in), optional :: directory, create, multiple
    integer(kind=c_int), intent(in), optional :: allow_uri, show_hidden
    integer(kind=c_int), intent(in), optional :: confirm_overwrite
    character(kind=c_char), dimension(*), intent(in), optional :: title, initial_dir, initial_file
    integer(kind=c_int), intent(in), optional :: current
    character(len=*), dimension(:), intent(in), optional :: filter
    character(len=*), dimension(:), intent(in), optional :: filter_name
    type(c_ptr), intent(in), optional :: parent
    integer(kind=c_int), intent(in), optional :: all
    integer(kind=c_int), intent(in), dimension(2), optional :: wsize
    integer(kind=c_int), intent(in), optional :: edit_filters

    ! Create a file chooser widget.
    !
    ! CHOOSER_INFO: hl_gtk_chooser_info: required: IDs and flags of various
    ! 		subwidgets needed to process the dialog actions.
    ! CDIR: string: optional: The directory from which they were chosen.
    ! DIRECTORY: boolean: optional: Set to TRUE to select directories
    ! 		instead of files.
    ! CREATE: boolean: optional: Set to FALSE to prohibit creating new files.
    ! MULTIPLE: boolean: optional: Set to TRUE to allow the selection of
    ! 		multiple files.
    ! ALLOW_URI: boolean: optional: Set to TRUE to allow nonlocal selections.
    ! SHOW_HIDDEN: boolean: optional: Set to TRUE to show hidden files.
    ! CONFIRM_OVERWRITE: boolean: optional: Set to TRUE to request
    ! 		confirmation of an overwrite (only used if CREATE
    ! 		is TRUE).
    ! TITLE: string: optional: Title for the window.
    ! INITIAL_DIR: string: optional: Set the initial directory here instead
    ! 		of the current directory.
    ! CURRENT: boolean: optional: Use to force start in current directory.
    ! INITIAL_FILE: string: optional: Set the initial file selection.
    ! FILTER: string(): optional:  The file selection filter. Elements
    ! 		may either be patterns or mime types. Each filter is a
    ! 		comma-separated list of patterns
    ! FILTER_NAME: string(): optional: Names for the filters
    ! PARENT: c_ptr: optional: Parent window for the dialogue.
    ! ALL: boolean: optional: Set to TRUE to add an all-files filter pattern
    ! WSIZE: c_int(2): optional: Set the size for the dialog.
    ! EDIT_FILTERS: boolean: optional: Set to TRUE to proves an entry window
    ! 		to add extra filters.
    !-

    type(c_ptr) :: content, junk, gfilter
    integer(kind=c_int) :: icreate, idir, action, lval
    integer(kind=c_int) :: i, idx0, idx1
    type(c_ptr) :: fbox, fapply

    ! Create a modal dialogue
    dialog = gtk_dialog_new()
    call gtk_window_set_modal(dialog, TRUE)
    if (present(title)) call gtk_window_set_title(dialog, title)
    if (present(wsize)) then
       call gtk_window_set_default_size(dialog, wsize(1),&
            & wsize(2))
    else
       call gtk_window_set_default_size(dialog, 700_c_int, 500_c_int)
    end if

    if (present(parent)) then
       call gtk_window_set_transient_for(dialog, parent)
       call gtk_window_set_destroy_with_parent(dialog, TRUE)
    end if

    ! Attach the action buttonsa to the dialogue
    !!! junk = gtk_dialog_add_button(dialog, "_Open"//C_NULL_CHAR, GTK_RESPONSE_APPLY)
    !!! junk = gtk_dialog_add_button(dialog, "_Cancel"//C_NULL_CHAR, &
    !!!      & GTK_RESPONSE_CANCEL)

    if(create == 0_c_int) then       ! 13.12.2017    !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      junk = gtk_dialog_add_button(dialog, GTK_STOCK_OPEN, GTK_RESPONSE_APPLY)
          ! 27.2.2020:
         if(langg == 'DE') call hl_gtk_button_set_label(junk, 'Öffnen'//c_null_char)
         if(langg == 'EN') call hl_gtk_button_set_label(junk, 'Open'//c_null_char)
         if(langg == 'FR') call hl_gtk_button_set_label(junk, 'Ouvrir'//c_null_char)
    else
      junk = gtk_dialog_add_button(dialog, GTK_STOCK_SAVE, GTK_RESPONSE_APPLY)
          ! 27.2.2020:
         if(langg == 'DE') call hl_gtk_button_set_label(junk, 'Speichern'//c_null_char)
         if(langg == 'EN') call hl_gtk_button_set_label(junk, 'Save'//c_null_char)
         if(langg == 'FR') call hl_gtk_button_set_label(junk, 'Enregistrer'//c_null_char)
    endif
    junk = gtk_dialog_add_button(dialog, GTK_STOCK_CANCEL, &
         & GTK_RESPONSE_CANCEL)
          ! 27.2.2020:
         if(langg == 'DE') call hl_gtk_button_set_label(junk, 'Abbrechen'//c_null_char)
         if(langg == 'EN') call hl_gtk_button_set_label(junk, 'Cancel'//c_null_char)
         if(langg == 'FR') call hl_gtk_button_set_label(junk, 'Annuler'//c_null_char)
    !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

    ! Decode the action
    if (present(create)) then
       icreate = create
    else
       icreate = TRUE
    end if
    if (present(directory)) then
       idir = directory
    else
       idir = FALSE
    end if

    if (idir == TRUE) then
       if (icreate == TRUE) then
          action = GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER
       else
          action = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
       end if
    else
       if (icreate == TRUE) then
          action = GTK_FILE_CHOOSER_ACTION_SAVE
       else
          action = GTK_FILE_CHOOSER_ACTION_OPEN
       end if
    end if

    ! Create the chooser & put it in the content area
    content = gtk_dialog_get_content_area(dialog)
    chooser_info%chooser = gtk_file_chooser_widget_new(action)
    call gtk_box_pack_start(content, chooser_info%chooser, TRUE, TRUE, 0_c_int)

    ! Local/URI
    if (present(allow_uri)) then
       if (allow_uri == FALSE) then
          lval = TRUE
       else
          lval = FALSE
       end if
    else
       lval = TRUE
    end if
    call gtk_file_chooser_set_local_only(chooser_info%chooser, lval)

    ! Multiple selections
    if (present(multiple)) then
       lval = multiple
    else
       lval = FALSE
    end if
    call gtk_file_chooser_set_select_multiple(chooser_info%chooser, lval)

    ! Hidden files
    if (present(show_hidden)) then
       lval = show_hidden
    else
       lval = FALSE
    end if
    call gtk_file_chooser_set_show_hidden(chooser_info%chooser, lval)

    ! Confirm overwrite
    if (icreate == TRUE) then
       if (present(confirm_overwrite)) then
          lval = confirm_overwrite
       else
          lval = FALSE
       end if
       call gtk_file_chooser_set_do_overwrite_confirmation(chooser_info%chooser,&
            & lval)
    end if

    ! Initial directory (precedes file so if file contains a dir it
    ! will overwrite)

    if (present(initial_dir)) then
       lval = gtk_file_chooser_set_current_folder(chooser_info%chooser, &
            & initial_dir)
    else if (present(current)) then
       if (c_f_logical(current)) &
            & lval = gtk_file_chooser_set_current_folder(chooser_info%chooser, &
            & "."//c_null_char)
    end if

    ! Initial file

    if (present(initial_file)) then
       if (action == GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER .or. &
            & action == GTK_FILE_CHOOSER_ACTION_SAVE) then
          call gtk_file_chooser_set_current_name(chooser_info%chooser, &
               & initial_file)
       else
          lval = gtk_file_chooser_select_filename(chooser_info%chooser, &
               & initial_file)
       end if
    end if

    ! Set up filters
    if (present(filter)) then
       do i = 1, size(filter)
          gfilter = gtk_file_filter_new()

          idx0 = 1
          do
             idx1 = index(filter(i)(idx0:),',')+idx0-2
             if (idx1 < idx0) then
                if (index(filter(i)(idx0:), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//c_null_char)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:)))//c_null_char)
                end if
                exit
             else
                if (index(filter(i)(idx0:idx1), '/') == 0) then
                   call gtk_file_filter_add_pattern(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//c_null_char)
                else
                   call gtk_file_filter_add_mime_type(gfilter, &
                        & trim(adjustl(filter(i)(idx0:idx1)))//c_null_char)
                end if
                idx0=idx1+2
             end if
          end do
          if (present(filter_name)) then
             call gtk_file_filter_set_name(gfilter, &
                  & trim(filter_name(i))//c_null_char)
          else
             call gtk_file_filter_set_name(gfilter, &
                  & trim(filter(i))//c_null_char)
          end if
          call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)
       end do
       if (present(all)) then
          if (all == TRUE) then
             gfilter = gtk_file_filter_new()
             call gtk_file_filter_add_pattern(gfilter, &
                  & "*"//c_null_char)
             call gtk_file_filter_set_name(gfilter, &
                  "All Files"//c_null_char)
             call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)
          end if
       end if
    end if

    ! Add an entry box for extra filters.
    if (present(edit_filters)) then
       if (edit_filters == TRUE) then
          fbox = hl_gtk_box_new(horizontal=TRUE)
          junk = gtk_label_new(c_null_char)
          call hl_gtk_box_pack(fbox, junk)
          junk = gtk_label_new("New filter:"//c_null_char)
          call hl_gtk_box_pack(fbox, junk, expand=FALSE)
          chooser_info%fentry = &
               & hl_gtk_entry_new(activate=c_funloc(hl_gtk_chooser_filt_cb), &
               & len=60_c_int, &
               & tooltip="Enter a new filter here."//c_null_char, &
               & data=c_loc(chooser_info))
          call hl_gtk_box_pack(fbox, chooser_info%fentry)
          fapply = hl_gtk_button_new("Apply"//c_null_char, &
               & clicked=c_funloc(hl_gtk_chooser_filt_cb), &
               & data=c_loc(chooser_info))
          call hl_gtk_box_pack(fbox, fapply, expand=FALSE)
          call gtk_file_chooser_set_extra_widget(chooser_info%chooser, fbox)
       end if
    end if

    call g_signal_connect(dialog, "response"//c_null_char, &
         & c_funloc(hl_gtk_chooser_resp_cb), c_loc(chooser_info))

  end function hl_gtk_file_chooser_new

  !+
  function hl_gtk_file_chooser_show(files, cdir, directory, create, &
       & multiple, allow_uri, show_hidden, confirm_overwrite, title, &
       & initial_dir, current, initial_file, filter, filter_name, parent, &
       & all, wsize, edit_filters) result(isel)

    integer(kind=c_int) :: isel
    character(len=*), dimension(:), intent(out), allocatable :: files
    character(len=*), intent(out), optional :: cdir
    integer(kind=c_int), intent(in), optional :: directory, create, multiple
    integer(kind=c_int), intent(in), optional :: allow_uri, show_hidden
    integer(kind=c_int), intent(in), optional :: confirm_overwrite
    character(kind=c_char), dimension(*), intent(in), optional :: title, initial_dir, initial_file
    integer(kind=c_int), intent(in), optional :: current
    character(len=*), dimension(:), intent(in), optional :: filter
    character(len=*), dimension(:), intent(in), optional :: filter_name
    type(c_ptr), intent(in), optional :: parent
    integer(kind=c_int), intent(in), optional :: all
    integer(kind=c_int), intent(in), dimension(2), optional :: wsize
    integer(kind=c_int), intent(in), optional :: edit_filters

    ! Create and show a file chooser widget.
    !
    ! FILES: string(): required: The file or files selected.
    ! CDIR: string: optional: The directory from which they were chosen.
    ! DIRECTORY: boolean: optional: Set to TRUE to select directories
    ! 		instead of files.
    ! CREATE: boolean: optional: Set to FALSE to prohibit creating new files.
    ! MULTIPLE: boolean: optional: Set to TRUE to allow the selection of
    ! 		multiple files.
    ! ALLOW_URI: boolean: optional: Set to TRUE to allow nonlocal selections.
    ! SHOW_HIDDEN: boolean: optional: Set to TRUE to show hidden files.
    ! CONFIRM_OVERWRITE: boolean: optional: Set to TRUE to request
    ! 		confirmation of an overwrite (only used if CREATE
    ! 		is TRUE).
    ! TITLE: string: optional: Title for the window.
    ! INITIAL_DIR: string: optional: Set the initial directory here instead
    ! 		of the current directory.
    ! CURRENT: boolean: optional: Use to force start in current directory.
    ! INITIAL_FILE: string: optional: Set the initial file selection.
    ! FILTER: string(): optional:  The file selection filter. Elements
    ! 		may either be patterns or mime types. Each filter is a
    ! 		comma-separated list of patterns
    ! FILTER_NAME: string(): optional: Names for the filters
    ! PARENT: c_ptr: optional: Parent window for the dialogue.
    ! ALL: boolean: optional: Set to TRUE to add an all-files filter pattern
    ! WSIZE: c_int(2): optional: Set the size for the dialog.
    ! EDIT_FILTERS: boolean: optional: Set to TRUE to proves an entry window
    ! 		to add extra filters.
    !
    ! Returns TRUE if one or more files was selected, FALSE otherwise.
    !-

    type(c_ptr) :: dialog, strptr
    type(hl_gtk_chooser_info) :: chooser_info
    integer(kind=c_int) :: i, nsel, resp

    dialog =  hl_gtk_file_chooser_new(chooser_info, cdir, directory, create, &
         & multiple, allow_uri, show_hidden, confirm_overwrite, title, &
         & initial_dir, current, initial_file, filter, filter_name, parent, &
         & all, wsize, edit_filters)

       call gtk_window_set_transient_for(dialog, idpt('window1'))              ! GK

    call gtk_widget_show_all (dialog)
    resp = gtk_dialog_run(dialog)
    call gtk_widget_destroy(dialog)

    isel = chooser_info%iselect
    if (chooser_info%iselect == TRUE) then
       nsel = g_slist_length(chooser_info%chooser_sel_list)
       allocate(files(nsel))
       do i = 1, nsel
          strptr = g_slist_nth_data(chooser_info%chooser_sel_list, i-1_c_int)
          call convert_c_string(strptr, files(i))
          call g_free(strptr)
       end do
       call g_slist_free(chooser_info%chooser_sel_list)

       if (present(cdir)) call convert_c_string(chooser_info%chooser_curdir, &
            & cdir)
    end if
  end function hl_gtk_file_chooser_show

  !+
  subroutine hl_gtk_chooser_resp_cb(dialog, response, gdata) bind(c)

   use gtk,           only: gtk_file_chooser_get_filter,gtk_file_filter_get_name   ! GK
   use UR_VARIABLES,  only: filtname                                               !
   use gtk_sup,       only: c_f_string                                             !

    type(c_ptr), value :: dialog
    integer(c_int), value :: response
    type(c_ptr), value :: gdata

    ! Callback for the "response" signal of the chooser
    !
    ! DIALOG: c_ptr: required: The dialog sending the response
    ! RESPONSE: c_int: required: The response code.
    ! GDATA: c_ptr: required: User data used to return a select/cancel value
    !
    ! The application developer should never need to use this routine directly.
    !-

    type(hl_gtk_chooser_info), pointer :: chooser_info
    type(c_ptr)  :: c_filter,c_filter_name       !######################################

    call c_f_pointer(gdata, chooser_info)

    select case (response)
    case (GTK_RESPONSE_DELETE_EVENT)
       chooser_info%iselect = FALSE
    case (GTK_RESPONSE_CANCEL)
       chooser_info%iselect = FALSE
    case (GTK_RESPONSE_APPLY)
       chooser_info%iselect = TRUE
       if (gtk_file_chooser_get_local_only(chooser_info%chooser) == TRUE) then
          chooser_info%chooser_sel_list = &
               & gtk_file_chooser_get_filenames(chooser_info%chooser)
       else
          chooser_info%chooser_sel_list = &
               & gtk_file_chooser_get_uris(chooser_info%chooser)
       end if
       chooser_info%chooser_curdir = &
            & gtk_file_chooser_get_current_folder(chooser_info%chooser)

       !#########################################################################
            ! 30.1.2021   GK
             c_filter = gtk_file_chooser_get_filter(chooser_info%chooser)
             if(c_associated(c_filter)) then
               c_filter_name = gtk_file_filter_get_name(c_filter)
               call c_f_string(c_filter_name,filtname)
             end if

       !#########################################################################

    case default
       chooser_info%iselect = FALSE
       write(error_unit,*) &
            & "hl_gtk_chooser_resp_cb:: Invalid response received", response
    end select
  end subroutine hl_gtk_chooser_resp_cb

  !+
  subroutine hl_gtk_chooser_filt_cb(widget, gdata) bind(c)

    type(c_ptr), value :: widget
    type(c_ptr), value :: gdata

    ! Callback for the new filter entry.
    !
    ! WIDGET: c_ptr: required: The widget sending the signal
    ! GDATA: c_ptr: required: User data used to return a select/cancel value
    !
    ! The application developer should never need to use this routine directly.
    !-

    type(hl_gtk_chooser_info), pointer :: chooser_info

    character(len=60) :: filter
    type(c_ptr) :: gfilter
    integer :: idx0, idx1

    call c_f_pointer(gdata, chooser_info)

    call hl_gtk_entry_get_text(chooser_info%fentry, filter)
    if (filter == "") return   ! No filter was given.

    gfilter = gtk_file_filter_new()

    idx0 = 1
    do
       idx1 = index(filter(idx0:),',')+idx0-2
       if (idx1 < idx0) then
          if (index(filter(idx0:), '/') == 0) then
             call gtk_file_filter_add_pattern(gfilter, &
                  & trim(adjustl(filter(idx0:)))//c_null_char)
          else
             call gtk_file_filter_add_mime_type(gfilter, &
                  & trim(adjustl(filter(idx0:)))//c_null_char)
          end if
          exit
       else
          if (index(filter(idx0:idx1), '/') == 0) then
             call gtk_file_filter_add_pattern(gfilter, &
                  & trim(adjustl(filter(idx0:idx1)))//c_null_char)
          else
             call gtk_file_filter_add_mime_type(gfilter, &
                  & trim(adjustl(filter(idx0:idx1)))//c_null_char)
          end if
          idx0=idx1+2
       end if
    end do
    call gtk_file_filter_set_name(gfilter, &
         & trim(filter)//c_null_char)

    call gtk_file_chooser_add_filter(chooser_info%chooser, gfilter)
    call gtk_entry_set_text(chooser_info%fentry, C_NULL_CHAR)
  end subroutine hl_gtk_chooser_filt_cb
end module gtk_hl_chooser
