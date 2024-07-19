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
    use, intrinsic :: iso_fortran_env, only: rn => real64

    implicit none

    logical                  :: fd_found(100)
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

end module UR_params