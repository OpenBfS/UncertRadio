

module KLF


interface

      ! subroutine LinCalib(): see the end of module RW1!

!#############################################################################################

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

use, intrinsic :: iso_c_binding,    only: c_ptr
use gtk,              only: gtk_label_set_text
use UR_Linft
use UR_Variables,     only: langg,MCSim_on
USE UR_Gleich,        only: missingval
use Rout,             only: WTreeViewPutDoubleArray,WDPutLabelString

use UR_params,        only: rn,eps1min,zero,one
use Brandt,           only: Lsqlin ! ,LfuncKB
end subroutine XKalfit

!#############################################################################################

module subroutine CalibInter(mode,vv,uvv, zfit,uzfit)

use, intrinsic :: iso_c_binding,   only: c_null_char
use UR_Linft
use UR_Gleich,       only: loadingpro,ifehl
use UR_Variables,    only: langg, MCSim_on
use UR_MCC,          only: idum,imc
use gtk,             only: GTK_BUTTONS_OK,GTK_MESSAGE_ERROR,GTK_MESSAGE_WARNING
use Rout,            only: MessageShow,WDNotebookSetCurrPage,WTreeViewPutDoubleArray, &
                           WDPutLabelString

use Top,             only: WrStatusbar
use UR_params,       only: rn,eps1min,zero
use UR_DLIM,         only: iteration_on

implicit none

integer(4),intent(in) :: mode          ! 1: interpolate y from given x;  2: interpolate x from given y;
real(rn),intent(in)   :: vv,uvv        ! value and SD of measured counting rate of the unknown concentration
real(rn),intent(out)  :: zfit,uzfit    ! value and SD obtained from curve interpolation for xx, uxx

end subroutine CalibInter

!#############################################################################################

module subroutine funcsKB(x,afunc,maKB)

   !  used as the funcs function by the WLS routine Lfit8_92

use UR_params,     only: rn,one

implicit none

integer(4),INTENT(IN)     :: maKB
real(rn),INTENT(IN)       :: x
real(rn),INTENT(OUT)      :: afunc(maKB)

end subroutine funcsKB

!#############################################################################################

module real(rn) function Fkalib(mode,x0,maKB,a_kalib)

use UR_params,     only: rn,zero

implicit none

integer(4),intent(in) :: mode     ! 1: interpolate y from given x;  2: interpolate x from given y;
real(rn),intent(in)   :: x0
integer(4),intent(in) :: maKB
real(rn),allocatable,intent(in)   :: a_kalib(:)    ! a_kalib(maKB)
end function Fkalib

!#############################################################################################

module real(rn) Function SD_y0(mode,v0,uv0,maKB,a_kalib,covar_kalib)

use UR_Variables,  only: MCSim_on
use UR_Linft,      only: netto_involved_Fitcal
use UR_params,     only: rn,zero,two,eps1min
use UR_Gleich,     only: missingval
use UR_DLIM,       only: iteration_on

implicit none

integer(4),intent(in)    :: mode     ! 1: interpolate y from given x;  2: interpolate x from given y;
real(rn),intent(in)      :: v0       ! a measured value x (mode 1) or y (mode 2)
real(rn),intent(in)      :: uv0      ! the associated standard uncertainty
integer(4),intent(in)    :: maKB     ! number of polynomial coefficients (<= 4)
real(rn),allocatable,intent(in)    :: a_kalib(:)       ! a_kalib(maKB)   ! polynomial coefficients = fit parameters
real(rn),allocatable,intent(in)    :: covar_kalib(:,:) ! covar_kalib(4,4)  ! covariance matrix of coefficients

end function SD_y0

!#############################################################################################

module subroutine KaliumTest

use UR_Linft
use UR_DLIM,         only: kalpha,kbeta
use UR_params,       only: rn,two

implicit none


end subroutine KaliumTest

!#############################################################################################

end interface


end module KLF

!#############################################################################################

      ! subroutine LinCalib(): see the end of module RW1!

!#############################################################################################

