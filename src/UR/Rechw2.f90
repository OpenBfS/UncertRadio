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
module Rw2
    use UR_types
    implicit none

    interface
        module subroutine Rechw2()
        end subroutine Rechw2

        module subroutine detlim_iter(DTxx, newvalue, it)

            real(rn), intent(in)        :: DTxx      ! needed only for limit_typ = 2 (DL iteration)
            real(rn), intent(out)       :: newvalue  ! calculated value of DT or DL
            integer, intent(out)        :: it        ! number of iterations
        end subroutine detlim_iter

        module subroutine setupParser(iopt)
            integer, intent(in) :: iopt     ! for control output: (0 (none) or 1 (with))
        end subroutine setupParser

        module real(rn) function RnetVal(xAct)
            real(rn), intent(in)    :: xAct
        end function RnetVal

        module integer function kqt_find()
            USE UR_DLIM,       only: iteration_on,limit_typ
            use UR_MCC,        only: kqtypx
            use ur_general_globals,  only: MCsim_on
        end function kqt_find

    end interface

contains


end module Rw2
