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

subroutine EvalGerror(errhint, error)

    !  this routine "tries" to interpret from Fortran the structure and details
    !  of a GUI-related error given as a C-pointer
    !
    !  This routine is not really well developped.
    !
    !   Copyright (C) 2018-2025  Günter Kanisch

    use, intrinsic :: iso_c_binding
    use GTK_sup,       only: convert_c_string,c_f_string, Gerror
    use g,             only: g_file_error_from_errno,g_error_get_type,g_quark_to_string

    use file_io,       only: logger
    use UR_gtk_window, only: GerrorF

    implicit none

    character(len=*),intent(in)    :: errhint    ! a textstring
    type(c_ptr),intent(in), target :: error      ! the error as received by a GUI error

    character(len=255)            :: ferrmsg
    type(Gerror),pointer          :: error_struct

    integer(c_int)                :: errno
    integer(c_size_t)             :: errtype
    type(c_ptr)                   :: cptr
    character(len=100),pointer    :: domainstr
    character(len=256)            :: log_str
    logical                       :: prout

    prout = .false.
    !  prout = .true.

    call c_f_pointer(error, error_struct)
    ! if(prout) then
    !     write(log_str, '(*(g0))') 'nach c_f_pointer(error, error_struct):  error_struct%code=', error_struct%code
    !     call logger(66, log_str)

    !     write(log_str, '(*(g0))') 'error_struct%message=', error_struct%message
    !     call logger(66, log_str)
    ! end if

    if(c_associated(error_struct%message)) then
        ! if(prout) then
        !     write(log_str, '(*(g0))') 'error_struct%message=', error_struct%message, &
        !                               ' error_struct%code=', error_struct%code
        !     call logger(66, log_str)
        ! end if
        call convert_c_string(error_struct%message,ferrmsg)

        if(prout) then
            write(log_str, '(*(g0))') 'nach convert_c_string'
            call logger(66, log_str)
        end if

        write(log_str, '(*(g0))') trim(errhint) // ' errmsg=', trim(ferrmsg)
        call logger(66, log_str)
    end if

    !------------------------------------------------------------------------------------
    cptr = g_quark_to_string(Error_struct%domain)

    ! if(prout)  then
    !     write(log_str, '(*(g0))') 'cptr=', cptr
    !     call logger(66, log_str)
    ! end if

    if(c_associated(cptr))  then
        call c_f_pointer(cptr, domainstr)
        if(prout)  then
            write(log_str, '(*(g0))') 'domainstr=',trim(domainstr)
            call logger(66, log_str)
        end if
    else
        return
    end if

    if(.true. .and. c_associated(Error_struct%message)) then
        call c_f_string(Error_struct%message, ferrmsg)
        write(log_str, '(*(g0))') 'EvalGerror: Errtxt=', trim(ferrmsg)
        call logger(66, log_str)
        write(log_str, '(*(g0))') 'EvalGerror: Error-Code=', Error_struct%code
        call logger(66, log_str)
    end if

    errtype = g_error_get_type()

    write(log_str, '(*(g0))') 'Errtype=', errtype
    call logger(66, log_str)

    errno = g_file_error_from_errno(Error_struct%code)

    write(log_str, '(*(g0))') 'errno=', errno
    call logger(66, log_str)

end subroutine EvalGError
