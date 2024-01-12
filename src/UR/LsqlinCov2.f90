
module LLcov2


  use UR_params,   only: rn
  implicit none

  interface

module subroutine LinCov2(n,nred,x1,sx1,yvar,covL,maL,R,ok,ifehl)

!
!  The routine prepares for the call to LsqLinCov2 for the weighted
!  least-squares analysis. It is called from Linf() with maL=ma=3.
!  It especially prepares the covariance matrix covyLF.
!  Lincov2 returns the vector yvar of fitting parameters and the associated
!  covariance matrix covL to Linf().

USE UR_LSQG
USE UR_Derivats
USE UR_Linft
USE UR_Gleich,      ONLY: Messwert,kpoint,StdUnc,kableitnum,ncov,  &
                          kEGr,ngrs,klinf,missingval
use UR_Linft,       only: numd
USE UR_DLIM,        ONLY: iteration_on,iterat_passed,limit_typ
USE UR_Variables,   ONLY: MCSim_on
use UR_MCC,         only: imc,covpmc

use UR_interfaces
use Top,            only: dpafact
use Usub3,          only: FindMessk
use Num1,           only: funcs,matwrite
use UR_params,      only: rn,eps1min,zero,one,two
use FCVX,           only: FindCovx

implicit none

! EXTERNAL  funcs

integer(4),INTENT(IN)    :: n             ! number of measured x values
integer(4),INTENT(IN)    :: nred          ! Anzahl der tatsächlich zu fittenden Parameter
real(rn),INTENT(IN)      :: x1(n)         ! vector of x values
real(rn),INTENT(IN)      :: sx1(n)        ! vector of standard deviations of the x values
integer(4),INTENT(IN)    :: maL           ! number of all fit parameters, including those which are
                                          ! not to be fitted
real(rn),INTENT(OUT)     :: yvar(maL)     ! vector of fitting parameters
real(rn),INTENT(OUT)     :: covL(maL,maL) ! covariance matrix of all fitting parameters, including
                                          ! also those not to be fitted
real(rn),INTENT(OUT)     :: R             ! value of the minimum function (chisq)
LOGICAL,INTENT(OUT)      :: ok
integer(4),INTENT(OUT)   :: ifehl         ! error variable;  0: nor error;  1: an error occurred
end subroutine LinCov2

module SUBROUTINE LsqLinCov2(x,covy1,n,nr,y,Uy,r,a,ok,maL,xparbgval,ifehl)

USE UR_Linft,     ONLY: xA,kPMLE,ifit,mfrbg,posdef,klincall        ! x1a,x2a,x3a,
USE UR_Gleich,    ONLY: kableitnum
USE UR_DLIM,      ONLY: iteration_on,limit_typ
USE UR_Variables, ONLY: langg
use Brandt,       only: mtxchi
use Num1,         only: funcs,matwrite
use Top,          only: WrStatusbar
use UR_params,    only: rn,zero,one,two
implicit none
integer(4), INTENT(IN)      :: n             ! number of measured values
real(rn), INTENT(IN)        :: x(n)          ! vector of independent input values (x values)
real(rn), INTENT(INOUT)     :: covy1(n,n)    ! covariance matrix of the x values
integer(4), INTENT(IN)      :: nr            ! number of fitted output quantities (dependent unknowns)
real(rn), INTENT(OUT)       :: y(nr)         ! vector of the values of the output quantities
real(rn), INTENT(OUT)       :: Uy(nr,nr)     ! covariance matrix dof the output quantities
real(rn), INTENT(OUT)       :: r             ! value of the minimum fanction (chisq)
real(rn), INTENT(OUT)       :: a(n,nr)       ! LS design matrix: (n x r) matrix A of partial derivatives
                                             ! of the fitting parameters, i.e., afunc()
LOGICAL, INTENT(OUT)        :: ok
integer(4), INTENT(IN)      :: maL           ! number of all fit parameters, including those which are not to be fitted
real(rn), INTENT(OUT)       :: xparbgval     ! parameter 2 for PMLE, with ifit(2)=0
integer(4),INTENT(OUT)      :: ifehl         ! error indicator
end subroutine LsqLinCov2

module subroutine RunPMLE(x,covy1,n,nr,y,yp,cyp,r,maL,xparbgval,ifehl)

USE UR_Linft,     ONLY: kPMLE,k_rbl,ifit,d0zrate,sd0zrate,    &
                        sd0zrateSV,fpaSV,singlenuk,mfrbg,mfrbg_2_fitnonlin
USE UR_Gleich,    ONLY: kpoint,StdUnc,Messwert,kableitnum
USE UR_DLIM,      ONLY: iteration_on,limit_typ
USE UR_MCC,       ONLY: imc
use Brandt,       only: mtxchi
use Num1,         only: funcs,matwrite
! use Fpmle,        only: FitDecayPMLE,PMFit
use UR_params,    only: rn,zero,one,two
use Top,          only: dpafact
USE UR_Variables, ONLY: MCSim_on

implicit none

integer(4), INTENT(IN)      :: n             ! number of measured values
real(rn), INTENT(IN)        :: x(n)          ! vector of independent input values (x values)

real(rn), INTENT(IN)        :: covy1(n,n)    ! covariance matrix of the x values
integer(4), INTENT(IN)      :: nr            ! number of fitted output quantities (dependent unknowns)
real(rn), INTENT(IN)        :: y(nr)         ! on input: vector of the values of the output quantities (y values)
real(rn),allocatable,INTENT(OUT) :: yp(:)       ! on output: vector of the values of the output quantities (y values)
real(rn),allocatable,INTENT(OUT) :: cyp(:,:)  ! covariance matrix associated with yp()
real(rn), INTENT(OUT)       :: r             ! valueof the minimum function (chisq)
integer(4), INTENT(IN)      :: maL           ! total number of fit parameters, including those being not fitted
real(rn), INTENT(INOUT)     :: xparbgval     ! parameter 2 for PMLE, with ifit(2)=0
integer(4),INTENT(OUT)      :: ifehl
end subroutine RunPMLE

module subroutine FitDecayPMLE(yv1,syv1,nv1,pa,ma,covpa,nca,chisq, ifehl)
USE UR_Linft,      ONLY: xA,k_rbl,d0zrate, &                   ! x1a,x2a,x3a
                         ifit,mfrbg,singlenuk,tmedian,condition_upg,  &
                         use_constr,kconstr,pcstr,upcstr,penalty_factor, &
                         mfrbg_2_fitnonlin,mfix,indfix,xfix
USE UR_Gleich,     ONLY: Messwert,kpoint,kEGr
USE UR_Variables,  ONLY: MCSim_on
use UR_DLIM,       only: iteration_on,limit_typ
use Num1,          only: funcs
use UR_params,     only: rn,zero,one,eps1min,two
use Brandt,        only: Lsqmar,Lsqfpmle,mean

implicit none

integer(4),INTENT(IN)   :: ma                          ! number of fitting parameters
integer(4),INTENT(IN)   :: nca                         ! physical dim of covar
integer(4),INTENT(IN)   :: nv1                         ! number of net count rates
real(rn),INTENT(IN)     :: yv1(nv1),syv1(nv1)          ! net count rates and their uncertainties
real(rn),INTENT(OUT)    :: chisq
! real(rn),INTENT(OUT)    :: covar(nca,nca)              ! covariance matrix of parameters a()
real(rn),allocatable,INTENT(OUT)   :: covpa(:,:)        ! covariance matrix of parameters a()
real(rn),allocatable,INTENT(INOUT) :: pa(:)              ! fitting parameters, count rates
integer(4),INTENT(OUT)  :: ifehl                       ! error indicator
end subroutine FitDecayPMLE



  end interface

end module LLcov2
