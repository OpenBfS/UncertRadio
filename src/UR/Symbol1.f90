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
module sym1

    use UR_types
    implicit none
    interface

        !-----------------------------------------------------------------------
        module subroutine symbol1(user_settings)
            implicit none
            type(user_settings_type), intent(in) :: user_settings
        end subroutine symbol1

        module subroutine pointnach(mfall)
            implicit none
            integer, intent(in)    :: mfall    ! 1: called from symbol1;   2: called from rechw1
        end subroutine pointnach

        module subroutine readj_knetto()
        end subroutine readj_knetto

        module subroutine readj_kbrutto()
            implicit none
        end subroutine readj_kbrutto

        module subroutine rs_numbers
        end subroutine rs_numbers

    end interface

end module sym1
