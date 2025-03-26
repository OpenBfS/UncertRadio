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
subroutine Save(mode, cnote)

    ! Saves or saves as file; calls ProSave for saving a txp project
    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_Gleich_globals,       only: ifehl
    use ur_general_globals
    use PSave,           only: ProSave
    use Rout,            only: UpdateProName, FOpen
    use CHF,             only: ucase

    implicit none

    integer(4),intent(in)          :: mode         ! 0:  save;   1: save as;
    character(len=*),intent(inout) :: cnote

    integer(4)                     :: mift
    !--------------------------------------------------------------------------
    ! FileTyp = 'P'

    mift = 0
    if (LEN_TRIM(FNAME) == 0 .OR. Mode == 1) then

        call FOpen(ifehl,.true., cnote)
        if(ifehl == 0) then
            IF(FileTyp == 'P') then      ! if the extension ".txp" is missing, it will attached
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
            if(FileTyp == 'F') then
                if(INDEX(ucase(fname), '.TXT') > 0) mift = 1
                if(mift == 0) then
                    mift = 1
                    fname = trim(fname) // '.txt'
                end if
            end if
        end if
    else
        call ProSave()
        SaveP = .false.

    end if

    if (Mode == 0 .and. LEN_TRIM(FNAME) > 0) then
        !  Place save code here
        call ProSave()
    end if

end subroutine Save

