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
module UR_params

    use UR_types

    implicit none
    ! private :: rn

    real(rn), parameter      :: PI = acos(-1.0_rn)
    real(rn), parameter      :: ZERO = 0._rn, &
                                HALF = 0.5_rn, &
                                ONE = 1._rn, &
                                TWO = 2._rn, &
                                EPS1MIN = epsilon(1._rn)

    character(*), parameter  :: UR2_CFG_FILE = 'UR2_cfg.dat'               ! UR2_cfg.dat file
    character(*), parameter  :: LOCKFILENAME = '.UncertRadio.lock'         ! lock file
    character(*), parameter  :: BATEST_OUT = 'vgltest.txt'
    character(*), parameter  :: BATEST_REF_FILE  = 'BatListRef_v06.txt'    ! since about 2024-01 (v.2.5)
    character(*), parameter  :: GPL_HEADER = 'UncertRadio Copyright (C) ' // &
                                             '2014 - 2024  G. Kanisch'
    character(*), parameter  :: WIN_TITLE  = 'UncertRadio: Calculation ' // &
                                             'of uncertainty budget and ' // &
                                             'detection limits'            ! Main window title

    ! colors for the "normal/default" mode
    type(color_settings_type), parameter :: DEFAULTMODE_COLORS = color_settings_type( &
        "#FFFFEC", & ! entry_bg
        "#000000", & ! entry_fg
        "#FFFFFF", & ! entry_mark_bg
        "#000000", & ! entry_mark_fg
        "#FFFFFF", & ! label_bg
        "#000000", & ! label_fg
        "#FFFFFF", & ! frame_bg
        "#000000", & ! frame_fg
        "#00FF48", & ! green_bg
        "#F57900", & ! orange_bg
        "#FFFFFF" )  ! table_bg

    ! colors for the "contrast" mode
    type(color_settings_type), parameter :: CONTRASTMODE_COLORS = color_settings_type( &
        "#000000", & ! entry_bg
        "#FFFFFF", & ! entry_fg
        "#000000", & ! entry_mark_bg
        "#FFFFFF", & ! entry_mark_fg
        "#000000", & ! label_bg
        "#FFFFFF", & ! label_fg
        "#1D1D1D", & ! frame_bg
        "#A1E1FF", & ! frame_fg
        "#0000d5", & ! green_bg
        "#B54900", & ! orange_bg
        "#252525" )  ! table_bg

end module UR_params