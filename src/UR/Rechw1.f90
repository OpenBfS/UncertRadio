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
module Rw1

    use UR_types

    interface

        !#######################################################################

        module Subroutine Rechw1(user_settings)
            type(user_settings_type), intent(inout) :: user_settings
        end subroutine Rechw1

        module subroutine covppcalc(mode)
            implicit none
            integer, intent(in)   :: mode        ! 1: called from Rechw1 or Rechw2;
            ! 2: called from MCcalc;
            ! 3: Test, on
        end subroutine covppcalc

        module subroutine LinCalib(user_settings)
            type(user_settings_type), intent(inout) :: user_settings
        end subroutine LinCalib

        module real(rn) function uval(symb)
            implicit none
            character(len=*),intent(in)  :: symb
        end function uval

        module recursive subroutine FindWparsR(kstart,klu)
            implicit none
            integer, intent(in)     :: kstart       ! index of start equation
            integer, intent(in)     :: klu          ! definition klu: see (begin of) upropa
        end subroutine FindWparsR

        module subroutine Find_lambda()
            implicit none
        end subroutine Find_lambda

        module subroutine PrepCovars(i)
            implicit none
            integer, intent(in)        :: i
        end subroutine PrepCovars


    end interface

end module Rw1
