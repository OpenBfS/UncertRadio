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

    real(rn), parameter      :: pi = acos(-1.0_rn)
    real(rn), parameter      :: zero = 0._rn, &
                                half = 0.5_rn, &
                                one = 1._rn, &
                                two = 2._rn, &
                                eps1min = epsilon(1._rn)

    character(*), parameter  :: UR2_cfg_file = 'UR2_cfg.dat'               ! UR2_cfg.dat file
    character(*), parameter  :: lockFileName = '.UncertRadio.lock'         ! lock file
    character(*), parameter  :: Batest_out = 'vgltest.txt'
    character(*), parameter  :: Batest_ref_file  = 'BatListRef_v06.txt'    ! since about 2024-01 (v.2.5)
    character(*), parameter  :: GPL_header = 'UncertRadio Copyright (C) ' // &
                                             '2014 - 2024  G. Kanisch'
    character(*), parameter  :: win_title  = 'UncertRadio: Calculation ' // &
                                             'of uncertainty budget and ' // &
                                             'detection limits'            ! Main window title

    ! color mode parameters
    type(color_settings), parameter :: default_colormode = color_settings( &
        "#FFFFEC", & ! entry_bg
        "#000000", & ! entry_fg
        "#FFFFFF", & ! entry_mark_bg
        "#000000", & ! entry_mark_fg
        "#FFFFFF", & ! label_bg
        "#000000", & ! frame_bg
        "#FFFFFF", & ! frame_fg
        "#000000", & ! green_bg
        "#00FF48", & ! orange_bg
        "#F57900", & ! table_bg
        "#FFFFFF" )

    type(color_settings), parameter :: contrast_colormode = color_settings( &
        "#000000", & ! entry_bg
        "#FFFFFF", & ! entry_fg
        "#000000", & ! entry_mark_bg
        "#FFFFFF", & ! entry_mark_fg
        "#000000", & ! label_bg
        "#FFFFFF", & ! frame_bg
        "#1D1D1D", & ! frame_fg
        "#A1E1FF", & ! green_bg
        "#0000d5", & ! orange_bg
        "#B54900", & ! table_bg
        "#252525" )

end module UR_params