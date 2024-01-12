module Fpmle


contains


!###########################################################################################

subroutine FitDecayPMLE(yv1,syv1,nv1,pa,ma,covpa,nca,chisq, ifehl)

      ! this routine performs a non-linear Poisson maximum likelihood fit (PMLE)
      ! of the measured decay curve.
      ! It uses the non-linear least squares fitting routine MRQMIN (Numerical
      ! Recipes, 1992), in which the subroutine MRQCOF is replaced by a version,
      ! which originates from Timo Hauschild and M. Jentschel (ILL Grenoble,
      ! FZ Rossendorf), who had implemented an option for PMLE in it; the latter
      ! is accessible by the MRQMIN paramater STATMOD=0.
      ! For PMLE, see:
      !   Hauschild, T., Jentschel, M., 2001. Comparison of maximum likelihood estimation
      !   and chi-square statistics applied to counting experiments.
      !   Nucl. Instr. & Meth A 457 (1-2), S 384-401.
      !
      ! This routine is called by:  linf --> lincov2 --> runPMLE --> FitDecayPMLE
      !
      ! The fitting function is given the routine PMFit.
      ! An option was added to the MRQCOF routine which allows applying constraints
      ! on the fitting parameters, in this case for background fitting parameter; it
      ! has the effect of stabilizing the fit.
      !

USE UR_Linft,      ONLY: x1a,x2a,x3a,k_rbl,d0zrate, &
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

LOGICAL          :: iterout
integer(4)       :: i,k,mfit,jpr,nfree,iap(3),kqt
real(rn)         :: spa(ma),chisqr
real(rn)         :: y1A(nv1),sy1A(nv1)
integer(4)       :: nstep
real(rn)         :: alamda,yy,mw_rbl
real(rn),allocatable  :: aa(:,:),scrat(:,:),px(:),py(:),puy(:)
integer(4),allocatable  :: list(:)

!--------------------------------------------------------------------------
jpr = 66
ifehl = 0

kqt = 1
if(iteration_on .and. limit_typ == 1) kqt = 2
if(iteration_on .and. limit_typ == 2) kqt = 3

iterout = .FALSE.
   iterout = .true.

! iteration_on: is true only when iterations for DT or DL are being calculated

! choose a condition to reduce the output in case of iterout = .true. :

  !  IF(.not.iteration_on .and. kableitnum == 0 .and. kqt == 1 .and. .not.Rnetmodi .and.  &
  !     .not.condition_upg .and. .not.upropa_on) iterout = .TRUE.
   !  IF(iteration_on .and. limit_typ == 1 .and. kableitnum == 0 .and..not.condition_upg) iterout = .TRUE.
    ! IF(iteration_on .and. kableitnum == 0) iterout = .TRUE.
   ! IF(iteration_on .and. .not.upropa_on) iterout = .TRUE.
    !  if(kableitnum == 0) iterout = .TRUE.
  !                        if(kableitnum == klinf) iterout = .true.

 if(MCsim_on) iterout = .false.
            if(iterout) write(0,*) 'ifit=',int(ifit,2)    ! -->  1, 2 or 3!
 mw_rbl = zero
 if(k_rbl > 0) mw_rbl = Messwert(kpoint(k_rbl))        ! (net) blank value
 IF(MCSim_on .and. kqt == 2) then
   pa(kEGr) = 1.E-7_rn         ! decision threshold case in MC simulation
 ENDIF

 IF(iterout) WRITE(66,'(a,3(es12.5,1x),a,3(es12.5,1x),a,i3)') 'FitPMLE:   start parameters: ', &
                         (pa(i),i=1,ma),'  x1a,x2a,x3a=',x1A(1),x2a(1),x3a(1),'  mfrbg=',mfrbg
 IF(iterout) WRITE(66,'(a,3i3,a,i3)') 'ifit=',ifit,'  ma=',ma

 if(allocated(px)) deallocate(px,py,puy); allocate(px(nv1),py(nv1),puy(nv1))
 if(allocated(covpa)) deallocate(covpa); allocate(covpa(ma,ma));
 px = zero
 py = zero
 puy = zero
 covpa = zero

! convert measured count rate values to counts, for fitting :
do i=1,nv1
  y1A(i) = yv1(i) * tmedian
  sy1A(i) = syv1(i) * tmedian
  sy1A(i) = max(one,y1A(i))
  py(i) = y1A(i)
  px(i) = real(i,rn)      ! transfer the index i instead of the value x1A(i))
  puy(i) = sy1A(i)
end do

mfit = 0
do i=1,ma
  IF(ifit(i) == 1) THEN
    mfit = mfit + 1
    iap(i) = 1               ! fitting the parameter: 1=yes, 0=no
  else
    iap(i) = 0
  ENDIF
end do

if(mfrbg_2_fitnonlin) then
  iap(mfrbg) = 1   !  mfrbg designates the parameter of back ground count rate
  mfit = mfit + 1
endif
nfree = nv1 - mfit
if(allocated(list)) deallocate(list);   allocate(list(ma))
list(1:ma) = iap(1:ma)
if(allocated(indfix)) deallocate(indfix);  allocate(indfix(ma-mfit))
if(allocated(xfix)) deallocate(xfix);  allocate(xfix(ma-mfit))
indfix = 0
xfix = zero

IF(ABS(pa(mfrbg)) < 1.E-12_rn) THEN
  pa(mfrbg) = mean(d0zrate(1:nv1))   !  is average of background count rates d0zrate())
  if(k_rbl > 0) pa(mfrbg) = pa(mfrbg) + mw_rbl
ENDIF
pa(1:ma) = pa(1:ma)  * tmedian ! convert fitting parameters to counts; tmedian: median(counting times)

IF(iterout) WRITE(jpr,'(3(a,i3),a,3i3)') 'PMLE:  ma=',ma,' nca=',nca,' mfit=',mfit,'  ifit=',ifit

! apply constraints to the parameter ab(mfrbg)?
use_constr =  .false. !  .true. !  .false.   ! .true.
kconstr = 0
IF(use_constr) THEN
  penalty_factor = 0.05_rn
  ! Constraints for BG Parameter:
  pcstr(mfrbg) = pa(mfrbg)
  upcstr(mfrbg) = 0.50_rn * sqrt(pa(mfrbg))
  kconstr(mfrbg) = iap(mfrbg)
  !  write(jpr,*) 'pcstr(mfrbg)=',sngl(pcstr(mfrbg)),'   upcstr(mfrbg)=',sngl(upcstr(mfrbg))
endif

if(allocated(aa)) deallocate(aa);    allocate(aa(nv1,mfit))
if(allocated(scrat)) deallocate(scrat); allocate(scrat(mfit,mfit))
aa = zero
scrat = zero
              write(0,*)   'A1'
! store the "fixed" information:
mfix = 0
indfix = 0
xfix = zero
do i=1,ma
  if(iap(i) == 0) then
    mfix = mfix + 1
    xfix(mfix) = pa(i)       ! fixed parameter
    indfix(mfix) = i
  endif
enddo
              write(0,*)   'A2'
nstep = 30
          write(0,*) 'list=',int(list,2),'  iap=',int(iap,2),' ma=',int(ma,2),' nv1=',int(nv1,2)
          write(0,*) 'pa=',sngl(pa)
call lsqmar(Lsqfpmle,px,py,puy,nv1,ma,list,pa,covpa,chisq,aa,scrat,nstep)
chisqr = chisq / real(MAX(1,nv1 - mfit),rn)
       write(66,*) 'pa=',sngl(pa)
       write(66,*) 'chisqr=',sngl(chisqr)

 ! switch back to count rate parameters a
 pa = pa / tmedian
 covpa = covpa / tmedian**two

! IF(iterout ) THEN
! IF(kableitnum > 0) THEN
  WRITE(jpr,110) nv1,chisqr,tmedian,nstep
110     FORMAT('count rates:    Npts=',i2,4x,'Chsiqr=',es11.3,'  tmedian=',es11.4, &
                                                            ' niter=',i0,/,   &
        ' i    Par-value        Par-Stddev     correlation coefficients:',/, &
        '--------------------------------------------------------------------', &
        '-------------')
  DO i=1,ma
    spa(i) = SQRT(ABS(covpa(i,i)))
    WRITE(jpr,'(i2,2X,g14.6,2X,g14.6,3x,6(f7.4,1x))') i,pa(i), spa(i)
  END DO
  write(jpr,*)
  write(jpr,*) 'fitted decay curve (in counts):'
  do i=1,nv1
    yy = Lsqfpmle(pa,ma-mfix,px(i))
    WRITE(66,'(a,i3,a,es12.5,3(a,es12.5))') 'i=',i,'  y1a=',y1a(i),'  fitted=',yy*tmedian, &
                               ' ratefit=',yy,'  bg=', &
                                             tmedian*(mw_rbl + d0zrate(i))
  end do

! ENDIF
      write(0,*)  ' FitDecayPMLE end'
!------------------------------------------------------------------------------------

end subroutine FitDecayPMLE

!#######################################################################


SUBROUTINE PMFit (i,pa,y,dyda,ma,iap)

USE UR_Linft,     ONLY: x1a,x2a,x3a,ifit,   &
                        d0zrate_add,mfrbg,tmedian
use UR_params,    only: rn,zero

implicit none

integer(4), INTENT(IN)      :: ma
integer(4),INTENT(IN)       :: i
real(rn), INTENT(IN)        :: pa(ma)
real(rn), INTENT(OUT)       :: y
real(rn), INTENT(OUT)       :: dyda(ma)
integer(4), INTENT(IN)      :: iap(ma)

integer(4)         :: mfit,k
!-----------------------------------------------------------------------
mfit = 0
y = zero
dyda = zero
do k=1,3
  ! mfit = mfit + 1
  if(iap(k) == 0 ) cycle
  select case (k)
       ! replaced mfit by k: 8.10.2021
    case (1)
      y = y + pa(k)*x1A(i)
      dyda(k) = x1A(i)*tmedian**0
    case (2)
      y = y + pa(k)*x2A(i)
      dyda(k) = x2A(i)*tmedian**0
    case (3)
      y = y + pa(k)*x3A(i)
      dyda(k) = x3A(i)*tmedian**0
  end select
end do
IF(mfrbg > 0) then
  if(ifit(mfrbg) == 2) THEN
    y = y + d0zrate_add(i)*tmedian    ! for partial derivation with respect to the backgroun count rate
  endif
ENDIF
y =  y * tmedian**0

RETURN

END SUBROUTINE PMFit
!=======================================================================

!#######################################################################


!=======================================================================



end module Fpmle
