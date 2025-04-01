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

module gtk3_functions
  !

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

    use gtk_hl

  implicit none

contains
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


end module gtk3_functions