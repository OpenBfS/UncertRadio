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

    use UR_types, only : rn

    implicit none
    public
    private :: rn
    !---------------------------------------------------------------------------------------------!
    integer, parameter       :: nclmax = 1550
    !---------------------------------------------------------------------------------------------!
    real(rn), parameter      :: PI = acos(-1.0_rn), &
                                ZERO = 0._rn, &
                                HALF = 0.5_rn, &
                                ONE = 1._rn, &
                                TWO = 2._rn, &
                                EPS1MIN = epsilon(1._rn)
    !---------------------------------------------------------------------------------------------!
    character(*), parameter :: UR2_CFG_FILE = 'UR2_cfg.dat', &                ! UR2_cfg.dat file
                               LOCKFILENAME = '.UncertRadio.lock', &          ! lock file
                               BATEST_OUT = 'vgltest.txt', &
                               BATEST_REF_FILE  = 'BatListRef_v06.txt', &     ! since about 2024-01                                          ! (v.2.5)
                               GPL_HEADER = 'UncertRadio Copyright (C) ' // & ! V 2.5 GPL Header
                                            '2014 - 2025  G. Kanisch', &
                               WIN_TITLE  = 'UncertRadio: Calculation ' // &  ! Main window title
                                            'of uncertainty budget and ' // &
                                            'detection limits', &
                               GLADEORG_FILE = 'UR2_5.glade'                  ! name of the Glade-file
    !---------------------------------------------------------------------------------------------!
end module UR_params
