

submodule (KLF)  KLFa


contains

          !    contains:
          ! XKalfit
          ! CalibInter
          ! funcsKB
          ! Fkalib
          ! SD_y0

          ! subroutine LinCalib(): see the end of module RW1!

     !  Copyright (C) 2014-2023  Günter Kanisch

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
    ! The routine KaliumTest, if called, would run a specific example.

     !  Copyright (C) 2014-2023  Günter Kanisch

use, intrinsic :: iso_c_binding,    only: c_ptr
use UR_Linft
use UR_Variables,     only: langg,MCSim_on,fname,kModelType
USE UR_Gleich,        only: missingval
use Rout,             only: WTreeViewPutDoubleArray,WDPutLabelString

use UR_params,        only: rn,EPS1MIN,ZERO,ONE
use Brandt,           only: Lsqlin ! ,LfuncKB
use CHF,              only: ucase

implicit none

integer(4)            :: i,j,m
character(len=200)    :: str1
real(rn),allocatable  :: fval_k(:),fuval_k(:), zuykalib(:)

if(allocated(ifitKB)) deallocate(ifitKB)

maKB = kal_Polgrad + 1
if(nkalpts == 0) return
allocate(ifitKB(maKB))
ifitKB = 1

if(allocated(fval_k)) deallocate(fval_k)
if(allocated(fuval_k)) deallocate(fuval_k)
if(allocated(zuykalib)) deallocate(zuykalib)
  allocate(fval_k(nkalpts),fuval_k(nkalpts),zuykalib(nkalpts))

if(allocated(a_kalib)) deallocate(a_kalib,covar_kalib)     ! 6.8.2023
if(.not.allocated(a_kalib)) then
  allocate(a_kalib(maKB))
  a_kalib(1:maKB) = ZERO
end if
if(.not.allocated(covar_kalib)) allocate(covar_kalib(maKB,maKB))

if(.not.MCSim_on) write(66,'(2(a,i0),a,L1)') 'XKalfit: maKB=',maKB,' KFMode=',KFmode,' use_WTLS_kal=',use_WTLS_kal


if(nkalpts == 0) return

do i=1,nkalpts
  zuykalib(i) = uykalib(i)
  if(abs(uykalib(1)-missingval) < EPS1MIN .and. abs(uykalib(2)-missingval) < EPS1MIN) zuykalib(i) = ONE
end do

  ! 20.1.2023 GK
call Lsqlin(funcsKB,xkalib,ykalib,zuykalib,nkalpts,maKB,ifitKB,a_kalib,covar_kalib,chisqKB)

if(MCSim_on) return

    write(66,*) 'Xkalfit:   Covar-Matrix der Fitparameter:'
    do i=1,maKB
      write(66,'(3es12.4)') (covar_kalib(i,j),j=1,maKB)
    end do
    write(66,*) 'Xkalfit:   Fitparameter:'
    write(66,'(3es12.4)') (a_kalib(i),i=1,maKB)
    write(66,*) 'Xkalfit:   u(y):'
    write(66,'(6es12.4)') (zuykalib(i),i=1,nkalpts)
    write(66,*) 'ifitKB=',int(ifitKB,2)

ChisqrKB = ChisqKB
if(nkalpts-maKB > 0) ChisqrKB = ChisqrKB / real(nkalpts-maKB,rn)
    write(66,*) 'chisqr=',sngl(chisqrKB)

!if( ( sum(abs(uxkalib)) <= eps1min .or. (abs(uxkalib(1)-missingval) < eps1min  &
!          .and. abs(uxkalib(2)-missingval) < eps1min) ) .and. use_WTLS_kal) then
if(use_WTLS_kal) then
  ! run WTLS:    ! 7.8.2023
  call GlsqCalib(maKB,nkalpts,a_kalib,covar_kalib,ykalib,zuykalib,xkalib,uxkalib, chisq)
  if(nkalpts-maKB > 0) ChisqrKB = Chisq / real(nkalpts-maKB,rn)
end if


if(maKB >= 1 .and. abs(zuykalib(1)-ONE)< EPS1MIN .and. abs(zuykalib(nkalpts)-ONE) < EPS1MIN ) then
  ! last statement for arithm. mean
  covar_kalib = covar_kalib * chisqrKB
end if

do j=1,nkalpts
  call CalibInter(1, xkalib(j),ZERO, fval_k(j),fuval_k(j))
end do
call WTreeViewPutDoubleArray('treeview7',6,nkalpts, fval_k)
call WTreeViewPutDoubleArray('treeview7',7,nkalpts, fuval_k)

! 6.8.2023:
if(kModelType == 2 .and. index(ucase(fname),'PEARSONYORK') > 0) then
    if(langg =='DE') write(str1,'(a,4es19.11)') 'Fitparameter:  ',(a_kalib(j),j=1,maKB)
    if(langg =='EN') write(str1,'(a,4es19.11)') 'Fit parameters:',(a_kalib(j),j=1,maKB)
    if(langg =='FR') write(str1,'(a,4es19.11)') 'Ajuster les paramètres:',(a_kalib(j),j=1,maKB)
    call WDPutLabelString('DKlabelFparms', trim(str1))
  else
    if(langg =='DE') write(str1,'(a,4es13.5)') 'Fitparameter:  ',(a_kalib(j),j=1,maKB)
    if(langg =='EN') write(str1,'(a,4es13.5)') 'Fit parameters:',(a_kalib(j),j=1,maKB)
    if(langg =='FR') write(str1,'(a,4es13.5)') 'Ajuster les paramètres:',(a_kalib(j),j=1,maKB)
    call WDPutLabelString('DKlabelFparms', trim(str1))
end if

if(kModelType == 2 .and. index(ucase(fname),'PEARSONYORK') > 0) then
    if(langg =='DE') write(str1,'(a,4es19.11)') 'deren StdDevs: ',(sqrt(covar_kalib(j,j)),j=1,maKB)
    if(langg =='EN') write(str1,'(a,4es19.11)') 'their StdDevs: ',(sqrt(covar_kalib(j,j)),j=1,maKB)
    if(langg =='FR') write(str1,'(a,4es19.11)') 'leurs StdDevs: ',(sqrt(covar_kalib(j,j)),j=1,maKB)
    call WDPutLabelString('DKlabelFsdev', trim(str1))
  else
    if(langg =='DE') write(str1,'(a,4es13.5)') 'deren StdDevs: ',(sqrt(covar_kalib(j,j)),j=1,maKB)
    if(langg =='EN') write(str1,'(a,4es13.5)') 'their StdDevs: ',(sqrt(covar_kalib(j,j)),j=1,maKB)
    if(langg =='FR') write(str1,'(a,4es13.5)') 'leurs StdDevs: ',(sqrt(covar_kalib(j,j)),j=1,maKB)
    call WDPutLabelString('DKlabelFsdev', trim(str1))
end if

write(str1,'(a,es13.5,a,es13.5)') 'ChisqR :',ChisqrKB,';    Root-MSE(SD) :',sqrt(ChisqrKB)
call WDPutLabelString('DKlabelChisqr', trim(str1))

end subroutine XKalfit

!#############################################################################################

module subroutine CalibInter(mode,vv,uvv, zfit,uzfit)

     ! This routine interpolates a polynomial function:
     !   mode=1: interpolate y from given x=vv;  mode=2: interpolate x from given y=vv;
     ! The result value in both cases is zfit and uzfit is its associated standard uncertaint value.
     !  Copyright (C) 2014-2023  Günter Kanisch

use, intrinsic :: iso_c_binding,   only: c_null_char
use UR_Linft
use UR_Gleich,       only: loadingpro,ifehl
use UR_Variables,    only: langg, MCSim_on
use UR_MCC,          only: idum,imc
use gtk,             only: GTK_BUTTONS_OK,GTK_MESSAGE_ERROR,GTK_MESSAGE_WARNING
use Rout,            only: MessageShow,WDNotebookSetCurrPage,WTreeViewPutDoubleArray, &
                           WDPutLabelString

use Top,             only: WrStatusbar
use RND,             only: rnorm
use UR_params,       only: rn,EPS1MIN,ZERO

implicit none

integer(4),intent(in) :: mode          ! 1: interpolate y from given x=vv;  2: interpolate x from given y=vv;
real(rn),intent(in)   :: vv,uvv        ! value and SD of measured counting rate of the unknown concentration
real(rn),intent(out)  :: zfit,uzfit    ! value and SD obtained from curve interpolation for xx, uxx

character(:),allocatable  :: str1
integer(4)                :: mode2,resp
!------------------------------------------------------------------------
allocate(character(len=200) :: str1)

mode2 = mode
if(mode == 0) mode2 = 1

if(mode2 == 2 .and. kal_polgrad > 1) then
  IF(langg == 'DE') str1 = 'Fehler: Zur Umkehrung des Kalibrier-Polynoms ist nur der'  // char(13) // &
                                  'Polynomgrad 1 erlaubt!' // char(13) // &
                                  ' Bitte den Menü-Punkt Kalibrierkurve überprüfen!'
  IF(langg == 'EN') str1 = 'Error: The inversion of the calibration polynomial is allowed' // char(13) // &
                                  ' only for a polynomial degree of 1!' // char(13) // &
                                  ' Please, check the menu item Calibration curve!'
  IF(langg == 'FR') str1 = 'Erreur: L''inversion du polynôme d''étalonnage est autorisée' // char(13) // &
                                  ' seulement pour un degré polynomial de 1!' // char(13) // &
                                  ' Veuillez vérifier l''élément de menu Courbe d''étalonnage!'
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "Kalfit1:", resp, mtype=GTK_MESSAGE_ERROR)
  IF(langg == 'DE') call WrStatusBar(4, &
                    'Menü-Punkt Kalibrierkurve überprüfen!')
  IF(langg == 'EN') call WrStatusBar(4, &
                     'Check the menu item Calibration curve!')
  IF(langg == 'FR') call WrStatusBar(4, &
                     'Veuillez vérifier l''élément de menu Courbe d''étalonnage!')
    write(66,*) 'CalibInter: mode = 2 and kal_polgrad > 1;  ifehl=1'
  ifehl = 1
  return
end if

zfit = FKalib(Mode2,vv,maKB,a_kalib)

if(use_UfitKal) uzfit = SD_y0(Mode2,vv,uvv,maKB,a_kalib,covar_kalib)

if(.not.use_UfitKal) uzfit = ZERO

if(MCSim_on .and. netto_involved_Fitcal .and. imc > 0 ) then
  zfit = zfit + uzfit*rnorm()
end if

if(abs(zfit) < EPS1MIN .and. .not.loadingpro .and. .not.MCSim_on) then
  IF(langg == 'DE') str1 = 'Warnung: Der mit KALFIT interpolierte Wert ist Null!' // char(13) // &
                                  ' Bitte den Menü-Punkt Kalibrierkurve überprüfen!'
  IF(langg == 'EN') str1 = 'Warning: The interpolated value from KALFIT is null!' // char(13) // &
                                  ' Please, check the menu item Calibration curve!'
  IF(langg == 'FR') str1 = 'Attention: La valeur interpolée de KALFIT est nulle!' // char(13) // &
                                  ' Veuillez vérifier l''élément de menu Courbe d''étalonnage!'
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "CalibInter:", resp, mtype=GTK_MESSAGE_WARNING)
  IF(langg == 'DE') call WrStatusBar(4, &
                    'Menü-Punkt Kalibrierkurve überprüfen!')
  IF(langg == 'EN') call WrStatusBar(4, &
                    'Check the menu item Calibration curve!')
  IF(langg == 'FR') call WrStatusBar(4, &
                    'Veuillez vérifier l''élément de menu Courbe d''étalonnage!')
  call WDNotebookSetCurrPage('notebook1', 3)

  ifehl = 1
  RETURN
end if

end subroutine CalibInter

!#######################################################################

module subroutine funcsKB(x,afunc,maKB)

   !  used as the funcs function by the WLS routine Lsqlin (weighted linear LS)

        !  Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,ONE

implicit none

integer(4),INTENT(IN)     :: maKB
real(rn),INTENT(IN)       :: x
real(rn),INTENT(OUT)      :: afunc(maKB)

integer(4)     :: i
real(rn)       :: xprod

xprod = ONE
do i=1,maKB
  if(i > 1) xprod = xprod * x
  afunc(i) = xprod
end do

end subroutine funcsKB

!#############################################################################################

module real(rn) function Fkalib(mode,x0,maKB,a_kalib)

     ! Fkalib is calculated as the polynmial value resulting from x0 (mode=1)
     ! or as the abscissa value of the polynomial value x0 (mode=2, only for
     ! a straight line, maKB=2)
     !
     !  Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,ZERO

implicit none

integer(4),intent(in) :: mode     ! 1: interpolate y from given x;  2: interpolate x from given y;
real(rn),intent(in)   :: x0
integer(4),intent(in) :: maKB
real(rn),allocatable,intent(in)   :: a_kalib(:)    ! a_kalib(maKB)

integer(4)       :: i
real(rn)         :: afunc(maKB)

FKalib = ZERO
call funcsKB(x0,afunc,maKB)
if(mode == 1) then
  do i=1,maKB
    FKalib = FKalib + afunc(i)*a_kalib(i)
  end do
elseif(mode == 2) then
  if(maKB == 2) then
    FKalib = (x0 - a_kalib(1)) / a_kalib(2)
  end if
end if

end function Fkalib

!#############################################################################################

module real(rn) Function SD_y0(mode,v0,uv0,maKB,a_kalib,covar_kalib)

     ! calculates the standard uncertainty of a polynomial value by uncertainty
     ! propagation; the polynomial values is calculated by Fkalib, dependent on mode.
     !  Copyright (C) 2014-2023  Günter Kanisch

use UR_Variables,  only: MCSim_on
use UR_Linft,      only: netto_involved_Fitcal
use UR_params,     only: rn,ZERO,TWO,EPS1MIN
use UR_Gleich,     only: missingval

implicit none

integer(4),intent(in)    :: mode     ! 1: interpolate y from given x;  2: interpolate x from given y;
real(rn),intent(in)      :: v0       ! a measured value x (mode 1) or y (mode 2)
real(rn),intent(in)      :: uv0      ! the associated standard uncertainty
integer(4),intent(in)    :: maKB     ! number of polynomial coefficients (<= 4)
real(rn),allocatable,intent(in)    :: a_kalib(:)       ! a_kalib(maKB)   ! polynomial coefficients = fit parameters
real(rn),allocatable,intent(in)    :: covar_kalib(:,:) ! covar_kalib(4,4)  ! covariance matrix of coefficients

integer(4)        :: i,k
real(rn)          :: z0,uz0, Tv0,uz1
real(rn)          :: dpi,dpk,dpa,Fv1,Fv2
real(rn),allocatable :: ta_kalib(:)      !ta_kalib(maKB)

Tv0 = v0
if(allocated(Ta_kalib)) deallocate(Ta_kalib)
allocate(Ta_kalib(maKB))
do i=1,maKB
  Ta_Kalib(i) = a_kalib(i)
end do

uz0 = ZERO
z0 = ZERO
dpi = ZERO
dpk = ZERO

! uncertainty contribution of uv0:
Fv1 = FKalib(mode,Tv0,maKB,Ta_kalib)
if(abs(Tv0) > EPS1MIN  .and. abs(uv0-missingval) > EPS1MIN) then
  z0 = FKalib(mode,Tv0,maKB,Ta_kalib)
  Fv1 = z0
  uz0 = ZERO
  dpa = 2.E-9_rn*Tv0
  Tv0 = Tv0 + dpa
  FV2 = FKalib(mode,Tv0,maKB,Ta_kalib)
  Tv0 = Tv0 - dpa
  dpi = (Fv2/dpa-Fv1/dpa)
  uz0 = uz0 + dpi**TWO * uv0**TWO
end if
   ! if(.not.iteration_on) write(66,*) 'SD_y0:  x0=',sngl(x0),' ux0=',sngl(ux0)

! uncertainty contributions of the partameters Ta_kalib:
uz1 = ZERO
do i=1,makB
  dpa = 2.E-9_rn*Ta_kalib(i)
  Ta_kalib(i) = Ta_kalib(i) + dpa
  FV2 = FKalib(mode,Tv0,maKB,Ta_kalib)
  Ta_kalib(i) = Ta_kalib(i) - dpa
  dpi = (Fv2-Fv1)/dpa         ! partial derivative related to index i

  do k=1,maKB
    dpa = 2.E-9_rn*Ta_kalib(k)
    Ta_kalib(k) = Ta_kalib(k) + dpa
    FV2 = FKalib(mode,Tv0,maKB,Ta_kalib)
    Ta_kalib(k) = Ta_kalib(k) - dpa
    dpk = (Fv2/dpa-Fv1/dpa)          ! partial derivative related to index k
    uz1 = uz1 + dpi*dpk * covar_kalib(i,k)  !  (i,k) contribution to the covar matrix
  end do
end do
   ! if(.not.iteration_on) write(66,*) 'SD_y0:  x0=',sngl(x0),' ux1=',sngl(ux1)
if(MCSim_on .and. netto_involved_Fitcal) then
  uz0 = sqrt(uz0 + uz1)
else
  uz0 = sqrt(uz0 + uz1)
end if

SD_y0 = uz0

end function SD_y0

!#############################################################################################

end submodule KLFa

!#############################################################################################

      ! subroutine LinCalib(): see the end of module RW1!

!#############################################################################################

