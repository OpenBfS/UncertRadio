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
! Last modification: 2012-05-22, vmagnin 2020-02-11

! --------------------------------------------------------
! gtk-hl-entry.f90
! Generated: Tue Oct 29 17:12:20 2013 GMT
! Generated for GTK+ version: 3.10.0.
! Generated for GLIB version: 2.38.0.
! --------------------------------------------------------


!*
! Text Entry
module gtk_hl_entry
  ! Convenience functions for both single and multiple line text boxes.
  !
  ! The single line is just wrappers for the GtkEntry widget.
  !
  ! The multi line editor is based around the GtkTextView widget family.
  ! The HL interface hides the text buffer from the user, except in some
  ! callbacks where the signal is attached to the buffer not the view.
  !
  ! If you do need to access the text buffer directly it can be obtained with
  ! the gtk_text_view_get_buffer function, or it can be returned via the
  ! optional BUFFER argument to the constructor.
  !/

  use gtk_sup
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: error_unit

  ! auto-generated use's
  use gtk, only: gtk_container_add, gtk_editable_set_editable,&
       & gtk_entry_get_text, gtk_entry_get_text_length, gtk_entry_new&
       &, gtk_entry_set_activates_default, gtk_entry_set_max_length,&
       & gtk_entry_set_text, gtk_scrolled_window_new,&
       & gtk_scrolled_window_set_policy, gtk_text_buffer_delete,&
       & gtk_text_buffer_get_char_count, gtk_text_buffer_get_end_iter&
       &, gtk_text_buffer_get_insert,&
       & gtk_text_buffer_get_iter_at_line,&
       & gtk_text_buffer_get_iter_at_line_offset,&
       & gtk_text_buffer_get_iter_at_mark,&
       & gtk_text_buffer_get_line_count, gtk_text_buffer_get_modified&
       &, gtk_text_buffer_get_selection_bound,&
       & gtk_text_buffer_get_selection_bounds,&
       & gtk_text_buffer_get_start_iter, gtk_text_buffer_get_text,&
       & gtk_text_buffer_insert, gtk_text_buffer_insert_at_cursor,&
       & gtk_text_buffer_new, gtk_text_buffer_set_modified,&
       & gtk_text_buffer_set_text, gtk_text_iter_forward_char,&
       & gtk_text_iter_forward_chars, gtk_text_iter_forward_line,&
       & gtk_text_iter_forward_lines, gtk_text_iter_get_line,&
       & gtk_text_iter_get_line_offset, gtk_text_iter_get_offset,&
       & gtk_text_view_get_buffer, gtk_text_view_new,&
       & gtk_text_view_new_with_buffer, gtk_text_view_set_editable,&
       & gtk_widget_set_sensitive, gtk_widget_set_size_request,&
       & gtk_widget_set_tooltip_text, GTK_POLICY_AUTOMATIC, &
       & TRUE, FALSE, g_signal_connect, GDK_FOCUS_CHANGE_MASK

  implicit none

contains
  !+
  function hl_gtk_entry_new(len, editable, activate, data, tooltip, value, &
       & sensitive, changed, data_changed, delete_text, data_delete_text, &
       & insert_text, data_insert_text, focus_in_event, focus_out_event, &
       & data_focus_in, data_focus_out, size) result(entry)

    type(c_ptr) :: entry
    integer(kind=c_int), intent(in), optional :: len
    integer(c_int), intent(in), optional :: editable
    type(c_funptr), optional :: activate, focus_in_event, focus_out_event
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip, value
    integer(kind=c_int), intent(in), optional :: sensitive
    type(c_funptr), optional :: changed, delete_text, insert_text
    type(c_ptr), optional :: data_changed, data_delete_text, data_insert_text
    type(c_ptr), optional :: data_focus_in, data_focus_out
    integer(kind=c_int), intent(in), optional :: size

    ! Higher level text entry box
    !
    ! LEN: integer: optional: The maximum length of the entry field.
    ! EDITABLE: boolean: optional: whether the entry box can be edited
    ! 		by the user
    ! ACTIVATE: c_funptr: optional: Callback function for the "activate" signal
    ! DATA: c_ptr: optional: Data to be passed to the activate callback (this
    ! 		is a plain DATA because the changed and other signals were
    ! 		added later.
    ! TOOLTIP: string: optional: tooltip to be displayed when the pointer
    ! 		is held over the button.
    ! VALUE: string: optional: An initial value for the entry box.
    ! SENSITIVE: boolean: optional: Whether the widget should initially
    ! 		be sensitive or not.
    ! CHANGED: c_funptr: optional: Callback for the "changed" signal.
    ! DATA_CHANGED: c_ptr: optional: Data to be passed to the changed callback.
    ! DELETE_TEXT: c_funptr: optional: Callback for the "delete-text" signal.
    ! DATA_DELETE_TEXT: c_ptr: optional: Data to be passed to the delete_text
    !            callback
    ! INSERT_TEXT: c_funptr: optional: Callback for the "insert-text" signal.
    ! DATA_INSERT_TEXT: c_ptr: optional: Data to be passed to the insert_text
    !            callback
    ! FOCUS_OUT_EVENT: c_funptr: optional: Callback for the "focus-out-event"
    ! 		signal, this is a GDK event rather than a GTK signal, so the
    ! 		call back is a function of 3 arguments returning gboolean.
    ! DATA_FOCUS_OUT: c_ptr: optional: Data to pass to the focus_out_event
    ! 		callback
    ! FOCUS_IN_EVENT: c_funptr: optional: Callback for the "focus-in-event"
    ! 		signal, this is a GDK event rather than a GTK signal, so the
    ! 		call back is a function of 3 arguments returning gboolean.
    ! DATA_FOCUS_IN: c_ptr: optional: Data to pass to the focus_in_event
    ! 		callback
    ! SIZE: integer : optional : The X-size request for the widget. Y is set
    ! 		to default (-1). Note that Gtk may make the widget bigger than
    ! 		this if expand/fill options in the packing require it.
    !-

    entry = gtk_entry_new()
    call gtk_entry_set_activates_default(entry, TRUE)

    if (present(len)) call gtk_entry_set_max_length(entry, len)
    if (present(size)) call gtk_widget_set_size_request(entry, size, -1_c_int)

    if (present(editable)) &
         & call gtk_editable_set_editable(entry, editable)

    if (present(value)) call gtk_entry_set_text(entry, value)

    if (present(activate)) then
       if (present(data)) then
          call g_signal_connect(entry, &
               & "activate"//C_NULL_CHAR, activate, data)
       else
          call g_signal_connect(entry, &
               & "activate"//C_NULL_CHAR, activate)
       end if
    end if

    if (present(changed)) then
       if (present(data_changed)) then
          call g_signal_connect(entry, "changed"//C_NULL_CHAR, changed, &
               & data_changed)
       else
          call g_signal_connect(entry, "changed"//C_NULL_CHAR, changed)
       end if
    end if
    if (present(delete_text)) then
       if (present(data_delete_text)) then
          call g_signal_connect(entry, "delete-text"//C_NULL_CHAR, &
               & delete_text, data_delete_text)
       else
          call g_signal_connect(entry, "delete-text"//C_NULL_CHAR, delete_text)
       end if
    end if
    if (present(insert_text)) then
       if (present(data_insert_text)) then
          call g_signal_connect(entry, "insert-text"//C_NULL_CHAR, &
               & insert_text, data_insert_text)
       else
          call g_signal_connect(entry, "insert-text"//C_NULL_CHAR, insert_text)
       end if
    end if
    if (present(focus_out_event)) then
       if (present(data_focus_out)) then
          call g_signal_connect(entry, &
               & "focus-out-event"//C_NULL_CHAR, focus_out_event, &
               & data_focus_out)
       else
          call g_signal_connect(entry, &
               & "focus-out-event"//C_NULL_CHAR, focus_out_event)
       end if
    end if

    if (present(focus_in_event)) then
       if (present(data_focus_in)) then
          call g_signal_connect(entry, &
               & "focus-in-event"//C_NULL_CHAR, focus_in_event, data_focus_in)
       else
          call g_signal_connect(entry, &
               & "focus-in-event"//C_NULL_CHAR, focus_in_event)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(entry, tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(entry, sensitive)

  end function hl_gtk_entry_new

  !+
  subroutine hl_gtk_entry_get_text(entry, text, status)

    type(c_ptr), intent(in) :: entry
    character(len=*), intent(out) :: text
    integer(kind=c_int), optional, intent(out) :: status

    ! Return the text in an entry box as a fortran string.
    !
    ! ENTRY: c_ptr: required: The text entry to read
    ! TEXT: f_string: required: The text read.
    ! STATUS: c_int: optional: Returns -1 if the string is truncated.
    !
    ! To return the text as a c-pointer use gtk_entry_get_text
    !-

    type(c_ptr) :: ctext
    character(kind=c_char), dimension(:), pointer :: textptr
    integer(kind=c_int16_t) :: ntext
    integer(kind=c_int) :: istat

    ntext = gtk_entry_get_text_length(entry)
    if (ntext == 0) then
       text=''
       return
    end if
    ctext = gtk_entry_get_text(entry)

    call c_f_pointer(ctext, textptr, (/int(ntext)/))
    call convert_c_string(textptr, text, istat)

    if (present(status)) status=istat
  end subroutine hl_gtk_entry_get_text

  !+
  function hl_gtk_text_view_new(scroll, editable, changed, data_changed, &
       & insert_text, data_insert_text, delete_range, data_delete_range, &
       & initial_text, sensitive, tooltip, ssize, buffer, focus_in_event, &
       & focus_out_event, data_focus_in, data_focus_out, hscroll_policy, &
       & vscroll_policy) result(view)

    type(c_ptr) :: view
    type(c_ptr), intent(out), optional :: scroll
    integer(kind=c_int), intent(in), optional :: editable
    type(c_funptr), optional :: changed, insert_text, delete_range
    type(c_ptr), optional :: data_changed, data_insert_text, data_delete_range
    character(kind=c_char, len=*), dimension(:), intent(in), &
         & optional :: initial_text
    integer(kind=c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), optional :: tooltip
    integer(kind=c_int), dimension(:), optional :: ssize
    type(c_ptr), intent(out), optional :: buffer
    type(c_funptr), optional :: focus_in_event, focus_out_event
    type(c_ptr), optional :: data_focus_in, data_focus_out
    integer(kind=c_int), intent(in), optional :: hscroll_policy, vscroll_policy

    ! A multiline text edit widget
    !
    ! SCROLL: c_ptr: optional: A scrolled window in which the text editor
    ! 		is placed. If it is present, then it must be used used for
    ! 		packing the widget into your application. If it is not used,
    ! 		then scroll bars will not be added if the text goes beyond
    ! 		the edge of the box.
    ! EDITABLE: boolean: optional: Set to FALSE to make a non-editable text box.
    ! CHANGED: c_funptr: optional: Callback for the "activate" signal.
    ! DATA_CHANGED: c_ptr: optional: User data to pass to/from the activate
    ! 		callback
    ! INSERT_TEXT: c_funptr: optional: Callback for the "insert-text" signal.
    ! 		This handler is attached to the text buffer not the text view.
    ! DATA_INSERT_TEXT: c_ptr: optional: User data for the insert-text callback.
    ! DELETE_RANGE: c_funptr: optional: Callback for the "delete-range" signal.
    ! 		This handler is attached to the text buffer not the text view.
    ! DATA_DELETE_RANGE: c_ptr: optional: User data for the delete-range
    ! 		callback.
    ! INITIAL_TEXT: string(): optional: Initial text to put in the text window.
    ! SENSITIVE: boolean: optional: Set to FALSE to make the widget start in an
    ! 		insensitive state.
    ! TOOLTIP: string: optional: A tooltip to display when the pointer is
    ! 		held over the widget.
    ! SSIZE: c_int(2): optional: Size of the scroll widget.
    ! BUFFER: c_ptr: optional: Variable to return the buffer pointer
    ! FOCUS_OUT_EVENT: c_funptr: optional: Callback for the "focus-out-event"
    ! 		signal, this is a GDK event rather than a GTK signal, so the
    ! 		call back is a function of 3 arguments returning gboolean.
    ! DATA_FOCUS_OUT: c_ptr: optional: Data to pass to the focus_out_event
    ! 		callback
    ! FOCUS_IN_EVENT: c_funptr: optional: Callback for the "focus-in-event"
    ! 		signal, this is a GDK event rather than a GTK signal, so the
    ! 		call back is a function of 3 arguments returning gboolean.
    ! DATA_FOCUS_IN: c_ptr: optional: Data to pass to the focus_in_event
    ! 		callback
    ! HSCROLL_POLICY: int: optional: Horizontal scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC). 
    ! VSCROLL_POLICY: int: optional: Vertical scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC). 
    !
    ! NOTE -- The insert-text and delete-range callbacks take extra arguments.
    ! They are called before the buffer is actually modified. The changed
    ! callback is called after the change.
    !-

    type(c_ptr) :: tbuf
    character(kind=c_char), dimension(:), allocatable :: text0
    type(gtktextiter), target :: iter
    integer(kind=c_int) :: itextlen, hscroll, vscroll

    tbuf = gtk_text_buffer_new(C_NULL_PTR)
    view = gtk_text_view_new_with_buffer(tbuf)

    if (present(scroll)) then
       if (present(hscroll_policy)) then
          hscroll = hscroll_policy
       else
          hscroll = GTK_POLICY_AUTOMATIC
       end if
       if (present(vscroll_policy)) then
          vscroll = vscroll_policy
       else
          vscroll = GTK_POLICY_AUTOMATIC
       end if
       scroll = gtk_scrolled_window_new(C_NULL_PTR, C_NULL_PTR)
       call gtk_scrolled_window_set_policy(scroll,  hscroll, vscroll)
       if (present(ssize)) &
            & call gtk_widget_set_size_request(scroll, ssize(1), ssize(2))
       call gtk_container_add(scroll, view)
    else if (present(ssize)) then
       call gtk_widget_set_size_request(view, ssize(1), ssize(2))
    end if

    if (present(editable)) then
       call gtk_text_view_set_editable(view, editable)
    else
       call gtk_text_view_set_editable(view, TRUE)
    end if

    ! If there's an initial value, set it before binding the signals.
    if (present(initial_text)) then
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(iter))
      if (len(initial_text) > 1) then
          call convert_f_string(initial_text, text0)
           call gtk_text_buffer_insert(tbuf, c_loc(iter), text0, -1_c_int)
          deallocate(text0)
       else
          if (initial_text(size(initial_text)) == c_null_char) then
             itextlen = -1
          else
             itextlen = size(initial_text)
          end if
          call gtk_text_buffer_insert(tbuf, c_loc(iter), initial_text, &
               & itextlen)
       end if
    end if

    ! Attach the various signals
    if (present(changed)) then
       if (present(data_changed)) then
          call g_signal_connect(tbuf, "changed"//c_null_char, changed, &
               & data_changed)
       else
          call g_signal_connect(tbuf, "changed"//c_null_char, changed)
       end if
    end if
    if (present(insert_text)) then
       if (present(data_insert_text)) then
          call g_signal_connect(tbuf, "insert-text"//c_null_char, insert_text, &
               & data_insert_text)
       else
          call g_signal_connect(tbuf, "insert-text"//c_null_char, insert_text)
       end if
    end if
    if (present(delete_range)) then
       if (present(data_delete_range)) then
          call g_signal_connect(tbuf, "delete-range"//c_null_char, delete_range, &
               & data_delete_range)
       else
          call g_signal_connect(tbuf, "delete-range"//c_null_char, delete_range)
       end if
    end if

    if (present(focus_out_event)) then
       if (present(data_focus_out)) then
          call g_signal_connect(view, &
               & "focus-out-event"//C_NULL_CHAR, focus_out_event, data_focus_out)
       else
          call g_signal_connect(view, &
               & "focus-out-event"//C_NULL_CHAR, focus_out_event)
       end if
    end if

    if (present(focus_in_event)) then
       if (present(data_focus_in)) then
          call g_signal_connect(view, &
               & "focus-in-event"//C_NULL_CHAR, focus_in_event, data_focus_in)
       else
          call g_signal_connect(view, &
               & "focus-in-event"//C_NULL_CHAR, focus_in_event)
       end if
    end if

    if (present(sensitive)) call gtk_widget_set_sensitive(view, sensitive)
    if (present(tooltip)) call gtk_widget_set_tooltip_text(view, tooltip)
    if (present(buffer)) buffer = tbuf

  end function hl_gtk_text_view_new

  !+
  subroutine hl_gtk_text_view_insert(view, text, line, column, replace, &
       & at_cursor, buffer)

    type(c_ptr), intent(in) :: view
    character(len=*), dimension(:), intent(in) :: text
    integer(kind=c_int), optional, intent(in) :: line, column
    integer(kind=c_int), optional, intent(in) :: replace, at_cursor
    type(c_ptr), intent(in), optional :: buffer

    ! Insert text to an text view
    !
    ! VIEW: c_ptr: required: The text view into which to insert.
    ! TEXT: string(): required: The text to insert.
    ! LINE: c_int: optional: The line at which to insert (if omitted,
    ! 		then the text is appended).
    ! COLUMN: c_int: optional: The column as which to insert the text
    ! 		(If omitted, then insert at the start of the line).
    ! REPLACE: boolean: optional: If set to TRUE and LINE and COLUMN are omitted
    ! 		then replace the text in the buffer.
    ! AT_CURSOR: boolean: optional: Set to TRUE to insert the text at the
    ! 		cursor.
    ! BUFFER: c_ptr: optional: The text buffer in which to insert the text
    ! 		If this is given, then VIEW is ignored -- used in signal
    ! 		handlers attached to the buffer.
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: iter
    integer(kind=c_int) :: icol, irep, atc
    character(kind=c_char), dimension(:), allocatable :: text0

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf= gtk_text_view_get_buffer(view)
    end if

    call convert_f_string(text, text0)

    ! Check if we are going to insert at the cursor, and if so do so.
    if (present(at_cursor)) then
       atc = at_cursor
    else
       atc = FALSE
    end if

    if (atc == TRUE) then
       call gtk_text_buffer_insert_at_cursor(tbuf, text0, -1_c_int)
       deallocate(text0)
       return
    end if

    if (present(line)) then
       if (present(column)) then
          icol = column
       else
          icol = 0
       end if
       if (present(replace)) then
          call hl_gtk_text_view_delete(C_NULL_PTR, line=line, column=icol, &
               & n_chars=size(text0, kind=c_int), buffer=tbuf)
       end if
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(iter), &
            & line, column)
    else
       if (present(replace)) then
          irep = replace
       else
          irep = FALSE
       end if
       if (irep == TRUE) then
          call gtk_text_buffer_set_text(tbuf, text0, -1_c_int)
          deallocate(text0)
          return
       end if
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(iter))
    end if

    call gtk_text_buffer_insert(tbuf, c_loc(iter), text0, -1_c_int)
    deallocate(text0)
  end subroutine hl_gtk_text_view_insert
  
    !+   GK
  subroutine hl_gtk_text_view_insert_single(view, text, line, column, replace, &
       & at_cursor, buffer)

    type(c_ptr), intent(in) :: view
    character(len=*),intent(in)               :: text
    integer(kind=c_int), optional, intent(in) :: line, column
    integer(kind=c_int), optional, intent(in) :: replace, at_cursor
    type(c_ptr), intent(in), optional :: buffer

    ! Insert text to an text view
    !
    ! VIEW: c_ptr: required: The text view into which to insert.
    ! TEXT: string(): required: The text to insert.
    ! LINE: c_int: optional: The line at which to insert (if omitted,
    ! 		then the text is appended).
    ! COLUMN: c_int: optional: The column as which to insert the text
    ! 		(If omitted, then insert at the start of the line).
    ! REPLACE: boolean: optional: If set to TRUE and LINE and COLUMN are omitted
    ! 		then replace the text in the buffer.
    ! AT_CURSOR: boolean: optional: Set to TRUE to insert the text at the
    ! 		cursor.
    ! BUFFER: c_ptr: optional: The text buffer in which to insert the text
    ! 		If this is given, then VIEW is ignored -- used in signal
    ! 		handlers attached to the buffer.
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: iter
    integer(kind=c_int) :: icol, irep, atc
    ! character(kind=c_char), dimension(:), allocatable :: text0
    character(kind=c_char), dimension(:), allocatable :: text0

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf= gtk_text_view_get_buffer(view)
    end if

      ! write(66,*) 'TEVInsert:  len(text=',len(text)
      ! write(66,*) 'TEVInsert: text=',text

    call convert_f_string(text, text0)

    ! Check if we are going to insert at the cursor, and if so do so.
    if (present(at_cursor)) then
       atc = at_cursor
    else
       atc = FALSE
    end if

    if (atc == TRUE) then
       call gtk_text_buffer_insert_at_cursor(tbuf, text0, -1_c_int)
       deallocate(text0)
       return
    end if

    if (present(line)) then
       if (present(column)) then
          icol = column
       else
          icol = 0
       end if
       if (present(replace)) then
          call hl_gtk_text_view_delete(C_NULL_PTR, line=line, column=icol, &
               & n_chars=size(text0, kind=c_int), buffer=tbuf)
       end if
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(iter), &
            & line, column)
    else
       if (present(replace)) then
          irep = replace
       else
          irep = FALSE
       end if
       if (irep == TRUE) then
          call gtk_text_buffer_set_text(tbuf, text0, -1_c_int)
          deallocate(text0)
          return
       end if
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(iter))
    end if

    call gtk_text_buffer_insert(tbuf, c_loc(iter), text0, -1_c_int)
    deallocate(text0)
  end subroutine hl_gtk_text_view_insert_single




  !+
  subroutine hl_gtk_text_view_delete(view, line, column, n_chars, n_lines, &
       & buffer)

    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(in), optional :: line, column, n_chars, n_lines
    type(c_ptr), intent(in), optional :: buffer

    ! Delete from a text view
    !
    ! VIEW: c_ptr: required: The text view from which to delete.
    ! LINE: c_int: optional: The line at which to start the deletion
    ! COLUMN: c_int: optional: The column at which to start the deletion.
    ! 		required if N_CHARS is given. Ignored if N_LINES is given.
    ! N_CHARS: c_int: optional: How many characters to delete.
    ! N_LINES: c_int: optional: How many lines to delete.
    ! BUFFER: c_ptr: optional: The text buffer from which to delete. If this
    ! 		is given, then VIEW is ignored, used in signal handlers
    ! 		attached to the buffer.
    !
    ! If no location specifiers are given then the buffer is cleared
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: s_iter, e_iter
    integer(kind=c_int) :: isok

    ! Input checking
    if (present(n_chars) .and. present(n_lines)) then
       write(error_unit, *) &
            & "hl_gtk_text_view_delete:: May not specify both N_CHARS and N_LINES"
       return
    end if

    if (present(n_chars) .and. .not. present(column)) then
       write(error_unit, *) &
            & "hl_gtk_text_view_delete:: Character delete requires a start column"
       return
    end if

    ! Find the buffer

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    if (present(n_chars)) then
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & line, column)
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & line, column)
       isok = gtk_text_iter_forward_chars(c_loc(e_iter), n_chars)
    else if (present(n_lines)) then
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), line)
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), line)
       isok = gtk_text_iter_forward_lines(c_loc(e_iter), n_lines)
    else
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))
    end if

    call gtk_text_buffer_delete(tbuf, c_loc(s_iter), c_loc(e_iter))
  end subroutine hl_gtk_text_view_delete

  !+
  subroutine hl_gtk_text_view_get_text(view, text, start_line, start_column, &
       & end_line, end_column, hidden, buffer)

    type(c_ptr), intent(in) :: view
    character(len=*), dimension(:), allocatable, intent(out) :: text
    integer(kind=c_int), intent(in), optional :: start_column, start_line, &
         & end_line, end_column
    integer(kind=c_int), intent(in), optional :: hidden
    type(c_ptr), intent(in), optional :: buffer

    ! Get text from s text view.
    !
    ! VIEW: c_ptr: required: The text view to read.
    ! TEXT: string(): required: A variable to contain the output text.
    ! START_LINE: c_int: optional: The first line to read.
    ! START_COLUMN: c_int: optional: The column at which to start reading.
    ! END_LINE: c_int: optional: The last line to read.
    ! END_COLUMN: c_int: optional: The column at which to stop reading.
    ! HIDDEN: boolean: optional: If set to FALSE, then do not get hidden
    ! 		characters
    ! BUFFER: c_ptr: optional: The text buffer from which to read. If this
    ! 		is given, then VIEW is ignored, useful for signal handlers
    ! 		attached to the buffer.
    !
    ! Note the rules for selection.
    !
    ! * If no selection arguments are present, the whole text is returned.
    ! * If either start_column or end_column is absent, but the matching line
    ! is present, then selection is by line.
    ! * If end_line is absent, but both columns are present, then the selection
    ! is within start_line
    ! * If neither start_line nor start_column is present, then the selection is
    ! from the start of the buffer
    ! * If neither end_line nor end_column is present, then the selection is
    ! to the end of the buffer.
    !-

    type(c_ptr) :: tbuf, ctext0
    character(kind=c_char), dimension(:), pointer :: ftext0
    type(gtktextiter), target :: s_iter, e_iter
    integer(kind=c_int) :: ihid
    integer :: nchars_r

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    ! Fully specified
    if (present(start_line) .and. present(start_column) .and. &
         & present(end_line) .and. present(end_column)) then
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & end_line, end_column)
            
    ! Not Fully specified xxxxxxxxxxxxxxxxxxxxx GK   9.5.2020       GK
    else if (present(start_line) .and. present(start_column) .and. &
         & present(end_line) .and. .not. present(end_column)) then
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), &
            & start_line)
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), &
            & end_line)


       ! Both columns only start line
    else if (present(start_line) .and. present(start_column) .and. &
         &  present(end_column)) then
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & start_line, end_column)

       ! Both lines, at least one column not given
    else if (present(start_line) .and. present(start_column)) then
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), &
            & start_line)
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), &
            & end_line)

       ! Fully specified start, no end
    else if (present(start_line) .and. present(start_column)) then
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(s_iter), &
            & start_line, start_column)
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))

       ! Start line only
    else if (present(start_line)) then
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(s_iter), &
            & start_line)
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))

       ! Fully specified end, no start
    else if (present(end_line) .and. present(end_column)) then
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
       call gtk_text_buffer_get_iter_at_line_offset(tbuf, c_loc(e_iter), &
            & start_line, end_column)

       ! End line only
    else if (present(end_line)) then
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
       call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(e_iter), &
            & end_line)

       ! Should only get here with nothing specified
    else
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(s_iter))
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(e_iter))
    end if

    if (present(hidden)) then
       ihid = hidden
    else
       ihid = TRUE
    end if
    ctext0 = gtk_text_buffer_get_text(tbuf, c_loc(s_iter), c_loc(e_iter), ihid)
    nchars_r = int(gtk_text_iter_get_offset(c_loc(e_iter)) - &
         & gtk_text_iter_get_offset(c_loc(s_iter))) + 1
         
    nchars_r = nchars_r + 50       !  GK <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         

    call c_f_pointer(ctext0, ftext0, (/ nchars_r /))
    call convert_c_string(ftext0, text)

  end subroutine hl_gtk_text_view_get_text

  !+
  function hl_gtk_text_view_get_cursor(view, buffer) result(ipos)

    integer(kind=c_int), dimension(3) :: ipos
    type(c_ptr), intent(in) :: view
    type(c_ptr), intent(in), optional :: buffer

    ! Get the current cursor location
    !
    ! VIEW: c_ptr: required: The text view to query
    ! BUFFER: c_ptr: optional: The buffer to query (if given, then
    ! 		VIEW is ignored).
    !
    ! Returns a 3-element array with the line, column and offset of the cursor
    !-

    type(c_ptr) :: tbuf, mark
    type(gtktextiter), target :: iter

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    mark = gtk_text_buffer_get_insert(tbuf)
    call gtk_text_buffer_get_iter_at_mark(tbuf, c_loc(iter), mark)
    ipos(1) = gtk_text_iter_get_line(c_loc(iter))
    ipos(2) = gtk_text_iter_get_line_offset(c_loc(iter))
    ipos(3) = gtk_text_iter_get_offset(c_loc(iter))
  end function hl_gtk_text_view_get_cursor

  !+
  function hl_gtk_text_view_get_selection(view, s_start, s_end, buffer) &
       & result(issel)

    integer(kind=c_int) :: issel
    type(c_ptr), intent(in) :: view
    integer(kind=c_int), dimension(3), intent(out) :: s_start, s_end
    type(c_ptr), intent(in), optional :: buffer

    ! Get the selection range
    !
    ! VIEW: c_ptr: required: The text view to query.
    ! S_START: c_int(): required: The start of the selection. (line, column, offset)
    ! S_END: c_int(): required: The end of the selection. (line, column, offset)
    ! BUFFER: c_ptr: optional: The text buffer to query. If present, then the
    ! 		view argument is ignored.
    !
    ! Returns TRUE if there is a selection, FALSE if there isn't
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: s_iter, e_iter

    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    issel = gtk_text_buffer_get_selection_bounds(tbuf, c_loc(s_iter), &
         & c_loc(e_iter))

    if (issel == FALSE) then ! No selection
       s_start(:) = -1
       s_end(:) = -1
    else
       s_start(1) = gtk_text_iter_get_line(c_loc(s_iter))
       s_start(2) = gtk_text_iter_get_line_offset(c_loc(s_iter))
       s_start(3) = gtk_text_iter_get_offset(c_loc(s_iter))
       s_end(1) = gtk_text_iter_get_line(c_loc(e_iter))
       s_end(2) = gtk_text_iter_get_line_offset(c_loc(e_iter))
       s_end(3) = gtk_text_iter_get_offset(c_loc(e_iter))
    end if
  end function hl_gtk_text_view_get_selection

  !+
  function hl_gtk_text_view_get_modified(view) result(ismod)

    integer(kind=c_int) :: ismod
    type(c_ptr), intent(in) :: view

    ! Check if the buffer of a text view is modified
    !
    ! VIEW: c_ptr: required: The text view to check.
    !
    ! N.B. No BUFFER argument is provided as gtk_text_buffer_get_modified
    ! is just a single call
    !-

    type(c_ptr) :: tbuf

    tbuf = gtk_text_view_get_buffer(view)
    ismod = gtk_text_buffer_get_modified(tbuf)

  end function hl_gtk_text_view_get_modified

  !+
  subroutine hl_gtk_text_view_set_modified(view, state)

    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(in) :: state

    ! Set/clear the modified flag on the text buffer of a text view
    !
    ! VIEW: c_ptr: required: The text view to set
    ! STATE: boolean: required: The state to set the flag to.
    !-

    type(c_ptr) :: tbuf

    tbuf = gtk_text_view_get_buffer(view)
    call gtk_text_buffer_set_modified(tbuf, state)

  end subroutine hl_gtk_text_view_set_modified

  !+
  subroutine hl_gtk_text_view_get_info(view, nlines, nchars, ncline, buffer)

    type(c_ptr), intent(in) :: view
    integer(kind=c_int), intent(out), optional :: nlines, nchars
    integer(kind=c_int), intent(out), optional, allocatable, dimension(:) :: ncline
    type(c_ptr), intent(in), optional :: buffer

    ! Get various useful information about a text view
    !
    ! VIEW: c_ptr: required: The view to query
    ! NLINES: c_int: optional: Return the number of lines in the view
    ! NCHARS: c_int: optional: Return the number of characters in the view
    ! NCLINE: c_int(): optional: Return the nuber of characters in each
    ! 		line. Must be an allocatable array.
    ! BUFFER: c_ptr: optional: If present use this buffer and ignore the
    ! 		VIEW argument
    !-

    type(c_ptr) :: tbuf
    type(gtktextiter), target :: i1, i2
    integer(kind=c_int) :: nl
    integer(kind=c_int) :: i
    if (present(buffer)) then
       tbuf = buffer
    else
       tbuf = gtk_text_view_get_buffer(view)
    end if

    if (present(nlines) .or. present(ncline)) &
         &  nl = gtk_text_buffer_get_line_count(tbuf)
    if (present(nlines)) nlines = nl

    if (present(nchars)) &
         & nchars = gtk_text_buffer_get_char_count(tbuf)

    if (present(ncline)) then
       allocate(ncline(nl))
       call gtk_text_buffer_get_start_iter(tbuf, c_loc(i1))
       do i = 1, nl-1
          call gtk_text_buffer_get_iter_at_line(tbuf, c_loc(i2), i)
          ncline(i) = gtk_text_iter_get_offset(c_loc(i2)) - &
               & gtk_text_iter_get_offset(c_loc(i1))-1
          i1 = i2
       end do
       call gtk_text_buffer_get_end_iter(tbuf, c_loc(i2))
       ncline(nl) = gtk_text_iter_get_offset(c_loc(i2)) - &
               & gtk_text_iter_get_offset(c_loc(i1))
    end if
  end subroutine hl_gtk_text_view_get_info
end module gtk_hl_entry
