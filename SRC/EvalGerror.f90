subroutine EvalGerror(errhint,error)

  !  this routine "tries" to interpret from Fortran the structure and details
  !  of a GUI-related error given as a C-pointer
  !
  !  This routine is not really well developped.
  !
  !   Copyright (C) 2018-2023  Günter Kanisch

use, intrinsic :: iso_c_binding
use GTK_sup,           only: convert_c_string,c_f_string, Gerror
use g,                 only: g_file_error_from_errno,g_error_get_type,g_quark_to_string
use TOP,               only: LFU
use UR_gtk_window,     only: GerrorF

implicit none

character(len=*),intent(in)    :: errhint    ! a textstring
type(c_ptr),intent(in), target :: error      ! the error as received by a GUI error

character(len=255)            :: ferrmsg
type(Gerror),pointer          :: error_struct
! type(GerrorF), pointer        :: errstructF
integer(c_int)                :: errno
integer(c_size_t)             :: errtype
type(c_ptr)                   :: cptr
character(len=100),pointer    :: domainstr
logical                       :: prout

prout = .false.
  !  prout = .true.

call c_f_pointer(error, error_struct)   ! xxxxxxxxxxxxxxx worked
    if(prout) write(66,*) 'nach c_f_pointer(error, error_struct):  error_struct%code=',error_struct%code
    if(prout) write(66,*) 'error_struct%message=', error_struct%message
if(c_associated(error_struct%message)) then
    if(prout) write(66,*) 'error_struct%message=',error_struct%message,' error_struct%code=',error_struct%code
  call convert_c_string(error_struct%message,ferrmsg)
    if(prout) write(66,*) 'nach convert_c_string'
   ! call c_f_pointer(c_loc(error), errstructF)   ! xxxxxxxxxxxxxxx ging
        ! write(66,*) 'Error=',error
       ! write(0,*) 'ErrstructF%fdomain =',ErrstructF%fdomain
       !write(66,*) 'Error_struct%message=',Error_struct%message
  write(66,*) trim(errhint) // ' errmsg=',trim(ferrmsg)    ! ging
end if

!------------------------------------------------------------------------------------

cptr = g_quark_to_string(Error_struct%domain)
  if(prout) write(66,*) 'cptr=',cptr
if(c_associated(cptr))  then
  call c_f_pointer(cptr, domainstr)
    if(prout) write(66,*) 'domainstr=',trim(domainstr)
else
  return
end if

if(.true. .and. c_associated(Error_struct%message)) then
  call c_f_string(Error_struct%message, ferrmsg)      ! #########################
  ! call convert_c_string(Error_struct%message, ferrmsg)
     ! call LFU(ferrmsg)

  write(66,*) 'EvalGerror: Errtxt=',trim(ferrmsg)
  write(66,*) 'EvalGerror: Error-Code=',Error_struct%code
end if

 errtype = g_error_get_type()
   write(66,*) 'Errtype=',errtype

errno = g_file_error_from_errno(Error_struct%code)
    write(66,*) 'errno=',errno

end subroutine EvalGError
