
subroutine Save(mode, cnote)

  ! Saves or saves as file; calls ProSave for saving a txp project
  !     Copyright (C) 2014-2023  Günter Kanisch

use, intrinsic :: iso_c_binding,   only: c_null_char,c_ptr,c_int
use UR_gtk_window
use UR_gtk_variables
use UR_Gleich,       only: ifehl
use UR_variables
use PSave,           only: ProSave
use Rout,            only: UpdateProName,FOpen
use CHF,             only: ucase

implicit none

integer(4),intent(in)              :: mode         ! 0:  save;   1: save as;
character(len=*),intent(inout)     :: cnote

integer(4)                   :: mift, i1
!--------------------------------------------------------------------------
! FileTyp = 'P'

mift = 0
IF (LEN_TRIM(FNAME) == 0 .OR. Mode == 1) THEN

  call FOpen(ifehl,.true., cnote)
  if(ifehl == 0) then
    IF(FileTyp == 'P') THEN      ! if the extension ".txp" is missing, it will attached
      ! fnameUcase = ucase(FNAME)
      if(INDEX(ucase(fname), '.TXP') > 0) mift = 1
      if(INDEX(ucase(fname), '.CSV') > 0) mift = 2
      if(mift == 0) then
        mift = 1
        fname = trim(fname) // '.txp'
      end if

      call ProSave()
      SaveP = .false.
      if(mode == 1) call UpdateProName(fname)
    end if
    IF(FileTyp == 'F') then
      if(INDEX(ucase(fname), '.TXT') > 0) mift = 1
      if(mift == 0) then
        mift = 1
        fname = trim(fname) // '.txt'
      end if
    end if
  END IF
else
  call ProSave()
  SaveP = .false.

END IF

IF (Mode == 0 .AND. LEN_TRIM(FNAME) > 0) THEN
  !  Place save code here
  call ProSave()
END IF

end subroutine Save

