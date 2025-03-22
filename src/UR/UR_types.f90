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
module UR_types
    use, intrinsic :: iso_fortran_env, only: rn => real64
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none

    type, bind(c)   :: GtkRequisition
        integer(kind=c_int)   :: width       !/* width of cell renderer */
        integer(kind=c_int)   :: height       !/* height of cell renderer */
    end type GtkRequisition

end module UR_types