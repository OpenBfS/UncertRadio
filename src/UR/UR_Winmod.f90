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

module UR_gtk_window

    !     Copyright (C) 2014-2025  Günter Kanisch

    use, intrinsic :: iso_c_binding, only: c_ptr, c_double, c_int, c_char
    use g

    implicit none

    type widgets_type
        type(c_ptr) :: window1
    end type

    integer, parameter           :: nclmax = 1250

    !   see: https://fortranwiki.org/fortran/files/character_handling_in_Fortran.html,
    !        section 10. Character arrays
    type :: charv                   ! ca. May 2020
        character(:), allocatable  :: s
    end type charv

    type Wclobj
        type(charv), allocatable  :: name(:)
        type(charv), allocatable  :: idd(:)
        type(charv), allocatable  :: label(:)
        type(c_ptr), allocatable  :: id_ptr(:)
        type(c_ptr), allocatable  :: label_ptr(:)
        type(charv), allocatable  :: signal(:)
        type(charv),  allocatable :: handler(:)
        integer, allocatable      :: idparent(:)
    end type

    type, bind(c)   :: GdkRGBA
        real(c_double) :: red   = 0.1_c_double
        real(c_double) :: green = 0.1_c_double
        real(c_double) :: blue  = 0.2_c_double
        real(c_double) :: alpha = 1.0_c_double
    end type

    type GErrorF
        integer            :: fdomain
        integer            :: fcode
        character(len=300) :: fmessage
    end type

    type KSetting
        type(c_ptr)          :: GtkSetDef
        integer              :: nprops
        character(len=60)    :: sproperty(20)
        character(len=250)   :: sproperty_val(20)
    end type KSetting

    type, bind(c)      :: TreeIterF
        integer(c_int)       :: stamp
        type(c_ptr)          :: user_data
        type(c_ptr)          :: user_data2
        type(c_ptr)          :: user_data3
    end type TreeIterF

    type, bind(c)      :: ginX
        integer(c_int)       :: type
        integer(c_int)       :: state
        integer(c_int)       :: keysym
        integer(c_int)       :: subwindow
        character(c_char)    :: string
        real(c_double)       :: pX,pY
        real(c_double)       :: dX,dY
        real(c_double)       :: wX,wY
    end type ginX

    type, bind(c)   :: GtkRequisition
        integer(kind=c_int)   :: width       !/* width of cell renderer */
        integer(kind=c_int)   :: height       !/* height of cell renderer */
    end type GtkRequisition


end module UR_gtk_window
