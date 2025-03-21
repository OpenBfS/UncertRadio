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
module KLF
    use UR_types


    implicit none

    interface

        module subroutine XKalfit()

            ! this routine takes a measured calibration curve (xkalib(),yklaib()) from the
            ! associated dialog and fits a linear polynomial function funcsKB to it.
            ! The routine CalibInter then reads for a given x0-value an y-value from the fitted
            ! curve (mode = 1), or (mode = 2, only for a straight line), if an y0-value given,
            ! it determines from the fitted curve the associated x-value.

            ! See chapter 6.4 "Utilizing a calibration curve" of the UncertRadio CHM Help file
            ! for more details.

            ! Xkafit uses Lfit8_92, Calibinter, Fkalib and SD_y0.
            ! The routine KaliumTest, if called, would run a specfic example.

        end subroutine XKalfit


        module subroutine CalibInter(mode, vv, uvv, zfit, uzfit)

            implicit none

            integer, intent(in) :: mode          ! 1: interpolate y from given x;  2: interpolate x from given y;
            real(rn), intent(in) :: vv,uvv        ! value and SD of measured counting rate of the unknown concentration
            real(rn), intent(out) :: zfit,uzfit    ! value and SD obtained from curve interpolation for xx, uxx

        end subroutine CalibInter


        module subroutine funcsKB(x, afunc, maKB)

            !  used as the funcs function by the WLS routine Lfit8_92

            implicit none

            integer   ,intent(in)     :: maKB
            real(rn),intent(in)       :: x
            real(rn),intent(out)      :: afunc(maKB)

        end subroutine funcsKB


        module real(rn) function Fkalib(mode, x0, maKB, a_kalib)

            implicit none

            integer, intent(in) :: mode     ! 1: interpolate y from given x;  2: interpolate x from given y;
            real(rn), intent(in) :: x0
            integer, intent(in) :: maKB
            real(rn), allocatable, intent(in) :: a_kalib(:)    ! a_kalib(maKB)
        end function Fkalib


        module real(rn) Function SD_y0(mode,v0,uv0,maKB,a_kalib,covar_kalib)


            implicit none

            integer, intent(in)    :: mode     ! 1: interpolate y from given x;  2: interpolate x from given y;
            real(rn), intent(in)   :: v0       ! a measured value x (mode 1) or y (mode 2)
            real(rn), intent(in)   :: uv0      ! the associated standard uncertainty
            integer, intent(in)    :: maKB     ! number of polynomial coefficients (<= 4)
            real(rn), allocatable,intent(in)    :: a_kalib(:)       ! a_kalib(maKB)   ! polynomial coefficients = fit parameters
            real(rn), allocatable,intent(in)    :: covar_kalib(:,:) ! covar_kalib(4,4)  ! covariance matrix of coefficients

        end function SD_y0


        module subroutine KaliumTest

            implicit none

        end subroutine KaliumTest

    end interface


end module KLF

