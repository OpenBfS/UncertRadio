
submodule (LLcov2)    LLcov2a

contains

          !     contains:
          ! LinCov2
          ! LsqLinCov2
          ! RunPMLE
          ! FitDecayPMLE

!#######################################################################


module subroutine LinCov2(n,nred,x1,sx1,yvar,covL,maL,R,ok,ifehl)

!
!  The routine prepares for the call to LsqLinCov2 for the weighted
!  least-squares analysis. It is called from Linf() with maL=ma=3.
!  It especially prepares the covariance matrix covyLF.
!  Lincov2 returns the vector yvar of fitting parameters and the associated
!  covariance matrix covL to Linf().
!
!     Copyright (C) 2014-2023  Günter Kanisch
!

USE UR_LSQG
USE UR_Derivats
USE UR_Linft
USE UR_Gleich,      ONLY: Messwert,kpoint,StdUnc,kableitnum,ncov,  &
                          kEGr,ngrs,klinf,missingval
use UR_Linft,       only: numd,use_PMLE
USE UR_DLIM,        ONLY: iteration_on,iterat_passed,limit_typ
USE UR_Variables,   ONLY: MCSim_on
use UR_MCC,         only: imc,covpmc

use UR_interfaces
use Top,            only: dpafact
use Usub3,          only: FindMessk
use Num1,           only: funcs,matwrite
use UR_params,      only: rn,eps1min,zero,one,two
!!!! use FCVX,           only: FindCovx

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

real(rn)          :: amt(n,nred)          ! LS design matrix: (n x nred) matrix A of partial derivatives
                                          ! of the fitting parameters, i.e., afunc()

real(rn),allocatable  :: covx1(:,:)      ! copy of covy
integer(4)        :: i,k,j
real(4)           :: start1
real(rn)          :: Uyf(nred,nred)
real(rn)          :: yvar2(nred)
integer(4)        :: kk,ir,kr,messk,messk2

real(rn)          :: dpa,xparbgval
real(rn)          :: Qsum(nred,nred), Qsumx(nred,nred)
! real(rn)          :: xp(maL),cxp(maL,maL),yvar2p(maL)
real(rn),allocatable :: xp(:),cxp(:,:),yvar2p(:)

real(rn)          :: MesswertKP(ngrs+ncov+numd)
real(rn)          :: xvar2a(nred),cxa(nred,nred)
integer(4)        :: ifitS(3), klu, ne,mfitp
integer(4)        :: kqt
character(len=1)  :: cmessk(3)

!-----------------------------------------------------------------------
posdef = .true.

cmessk = (/'A','B','C' /)  ! names of up to three counting channels (e.g. in an LSC)

use_PMLE = .false.
if(kPMLE ==1) use_PMLE = .true.

klu = klinf
IF(kfitp(1) > 0) klu = kfitp(1) - 1 + kEGr

kqt = 1
if(iteration_on .and. limit_typ == 1) kqt = 2
if(iteration_on .and. limit_typ == 2) kqt = 3
  if(MCSim_on) kqt = 0

  ! array ifit:  ifit(k):  =1: parameter k is fitted;   =2: hold paramter k fiexed at its start value;
  !                        =3: parameter k is not included in fitting;

ifitS(1:mAL) = ifit(1:maL)
ifehl = 0
mfit = 0
do i=1,maL
  IF(ifit(i) == 1) mfit = mfit + 1
end do

mfitp = max(mfit, mfrbg)           ! for the case of using PMLE
allocate(xp(maL))
allocate(cxp(maL,maL))
allocate(yvar2p(maL))
xp(:) = zero
cxp(:,:) = zero
yvar2p(:) = zero

! covariance matrix between x values:
! this covariance matrix is also used in MCCALC!

if(allocated(covyLF)) deallocate(covyLF);  allocate(covyLF(n,n))
if(allocated(covx1)) deallocate(covx1);    allocate(covx1(n,n))
if(allocated(covppc)) deallocate(covppc);  allocate(covppc(nhp,nhp))
covyLF(:,:) = zero
covx1(:,:) = zero
covppc(:,:) = zero

do i=1,n
  messk = FindMessk(i)
  covyLF(i,i) = sx1(i)**two
  IF(nkovzr == 1) THEN
    ! take covariances between net count rates into account,
    ! if the same background count rate is applied for the net count rates
    do k=1,i
      messk2 = FindMessk(k)
      IF(i /= k) THEN
        covyLF(k,i) = zero
        if(konstant_r0 .and. messk == messk2) then
          covyLF(k,i) = sdR0k(messk)**two
        end if
        if(k_rbl > 0) then
          IF(abs(StdUnc(kpoint(k_rbl))-missingval) > eps1min) covyLF(k,i) = covyLF(k,i) + StdUnc(kpoint(k_rbl))**two
        end if
        if(parfixed) covyLF(k,i) = covyLF(k,i) + cov_fixed(k,i)
        covyLF(i,k) = covyLF(k,i)
      END IF
    end do
  end if
end do

! Prepare a copy of von covyLF:
covx1(1:n,1:n) = covyLF(1:n,1:n)

call LsqLinCov2(x1,covx1,n,mfit,yvar2,Uyf,r,amt,ok,maL,xparbgval,ifehl)

if(ifehl == 1) then
  write(66,*) 'Lincov2: Vektor sx1: '
  write(66,'(100es10.3)') (sx1(i),i=1,n)
  write(66,*)
  return
end if
  yvar2p = zero
  yvar2p(1:mfit) = yvar2(1:mfit)
  if(mfit == mfitp-1 ) yvar2p(mfitp) = zero
if(kPMLE == 1) then
  call RunPMLE(x1,covx1,n,mfitp,yvar2p,xp,cxp,r,maL,xparbgval,ifehl)
end if
if(ifehl == 1) return

!-----------------------------------------------------------------

! CALL CPU_TIME(start)

klincall = klincall + 1

if(.not. MCSim_on) then
  covppc(1:nhp,1:nhp) = covpp(1:nhp,1:nhp)
elseif(MCSim_on) then                         ! 4.7.2023
  covpp(1:nhp,1:nhp) = covpmc(1:nhp,1:nhp)
end if

! if(.not. MCSim_on) then
if(.not. MCSim_on .and. .not.use_WTLS) then

  condition_upg = ((klincall == 1 .OR. iteration_on .OR. iterat_passed ) .AND. nhp > 0)
  condition_upg = condition_upg .or. (MCSim_on .and. imc <= 4)

  ! ("mpfx parameters": parameters given as arguments of UR Linfit() function)
  !  (e.g., half-lives or efficiencies including uncertainties, being part of the decay function terms)
  ! Here is always klincall=1, if mpfx parameters or their uncertainties are modfied!

  ! The calculation of the covariance matrix of the mpfx parameters has been integrated
  ! in the following loop; it is executed for i=1 and klincall=1. the "local" covp is
  ! copied to the "global" covpp, from which it can be copied back when needed.
  ! covpp is used especially by WTLS.

  IF(condition_upg) THEN

    MesswertKP(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)        ! copy of Messwert array
    call LsqLinCov2(x1,covx1,n,mfit,yvar2,Uyf,r,amt,ok,maL,xparbgval,ifehl)

    if(.not. use_WTLS .or. compare_WTLS) then
      if(allocated(Qxp)) deallocate(Qxp)
      allocate(Qxp(1:nred,1:nhp))
      Qxp = zero
      ! do ne=1,nred
        do j=1,nhp
               ! write(66,*) 'ne,j=',int(ne,2),int(j,2),' parfixed=',parfixed,' mpfxfixed(j)=',mpfxfixed(j)
          if(mpfx_extern(j) .and. .not. MCSim_on) cycle
          if(parfixed .and. mpfxfixed(j) == 1) cycle
          ! if(Messwert(mpfx(j)) > zero) then
          if(Messwert(mpfx(j)) > zero .and. StdUnc(mpfx(j)) > zero) then    ! 6.7.2023
            dpa = Messwert(mpfx(j)) * dpafact(Messwert(mpfx(j))) - Messwert(mpfx(j))
            Messwert(mpfx(j)) = Messwert(mpfx(j)) + dpa
            call LsqLinCov2(x1,covx1,n,mfit,xvar2a,cxa,r,amt,ok,maL,xparbgval,ifehl)
            Messwert(mpfx(j)) = Messwert(mpfx(j)) - dpa
            if(ifehl == 1) return
            yvar2p = zero
            yvar2p(1:mfit) = yvar2(1:mfit)
            if(mfit == mfitp-1 ) yvar2p(mfitp) = zero
            if(kPMLE == 1) call RunPMLE(x1,covx1,n,mfitp,yvar2p,xp,cxp,r,maL,xparbgval,ifehl)
            if(ifehl == 1) return
                  !  if(kPMLE == 1) xvar2a(ne) = yvar2p(ne)   ! 6.7.2023    noch testen!
            do ne=1,nred           ! 6.7.2023
              Qxp(ne,j) = (xvar2a(ne) - yvar2(ne))/dpa     ! partial derivatives
            end do
          end if
          ! Qxp is needed for the test calculations at the end of LSQgen
        end do
      ! end do
    end if
  end if

  if(.not.allocated(Qsumarr)) then
    allocate(Qsumarr(50*50))
    Qsumarr = zero
  end if

  ! nhp and mpfx() come from Rechw1 and Upropa, resp.
  IF(condition_upg) THEN
    ! re-calculate the vector Qsumarr:

    ! restore the initial state:
    call LsqLinCov2(x1,covx1,n,mfit,yvar2,Uyf,r,amt,ok,maL,xparbgval,ifehl)
    if(ifehl == 1) return
              yvar2p(1:mfit) = yvar2(1:mfit)
              if(mfit == mfitp-1 ) yvar2p(mfitp) = zero
    if(kPMLE == 1) call RunPMLE(x1,covx1,n,mfitp,yvar2p,xp,cxp,r,maL,xparbgval,ifehl)
    if(ifehl == 1) return

      IF(klincall == 1 .and. .not.iteration_on) THEN
         write(66,*) 'yvar2=',sngl(yvar2),'  x1=',sngl(x1)
        if(klincall == 1) call matwrite(covx1,n,n,66,'(1x,130es16.8)', &
                        'Lsqlincov2: Matrix covx1:')
        call matwrite(Uyf,mfit,mfit,66,'(1x,130es16.8)', &
                        'Lsqlincov2: Matrix Uyf   Before QMAT treatment:')
      end if

    if(.not.use_WTLS .or. compare_WTLS) then

            !call matwrite(Qxp,nhp,nhp,66,'(1x,130es16.8)', &
            !            'Lsqlincov2: Matrix Qxp   Before QMAT treatment:')
            !call matwrite(covppc,nhp,nhp,66,'(1x,130es16.8)', &
            !            'Lsqlincov2: Matrix covppc   Before QMAT treatment:')

             ! write(66,*) 'condition_upg=',condition_upg,' klincall=',klincall

       Qsumx = matmul(Qxp, matmul(covppc, Transpose(Qxp)))
       IF(.true. .and. klincall == 1) THEN
         call matwrite(Qsumx,mfit,mfit,66,'(1x,130es16.8)', &
                     'Lsqlincov2: Matrix Qsumx   Before Qsumarr:')
         !call matwrite(Qxp,nred,nhp,66,'(1x,130es16.8)', &
         !            'Lsqlincov2: Matrix Qxp   Before Qsumarr:')
       end if
    end if

    forall(i=1:nred, k=1:nred)
      Qsumarr((i-1)*nred + k) = Qsumx(i,k)
    end forall

  end if

  IF(nhp == 0 .and. (.not. use_WTLS .or. (use_WTLS .and. compare_WTLS))) then
    Qsumarr(1:size(Qsumarr)) = zero
  end if

   if(.not.use_WTLS .or. (use_WTLS .and. compare_WTLS)) then
     forall(i=1:nred, k=1:nred)
       Qsum(i,k) = Qsumarr((i-1)*nred + k)
     end forall

     IF(.false. .and. klincall == 1) THEN
       call matwrite(Qsum,mfit,mfit,66,'(1x,130es16.8)', &
                    'Lsqlincov2: Matrix Qsum after ....')
     end if
     Uyf(1:nred,1:nred) = Uyf(1:nred,1:nred) + Qsum(1:nred,1:nred)

     IF(klincall == 1 .and. .not.iteration_on) THEN
       call matwrite(Uyf,mfit,mfit,66,'(1x,130es16.8)', &
                    'Lsqlincov2: Matrix Uyf   After QMAT treatment:')
     end if
   end if
end if   ! .not.MCsim_on

ifit(1:maL) = ifitS(1:maL)

!----------------

! copy the shortened list yvar2 of fitting parameter values onto the full list(array) yvar:
yvar(1:size(yvar)) = zero
kk = 0
if(kPMLE == 0) then
  do i=1,size(yvar)
    if(ifit(i) == 3) cycle
    if(mfit < maL) then
      if(i <= mfit) then
        if(ifit(i) == 2) yvar(i) = one       ! parameter fixed
      end if
    end if
    kk = kk + 1
    if(kk <= nred) yvar(i) = yvar2(kk)
  end do
else
  yvar(1:size(yvar)) = xp(1:size(yvar))
end if
! copy also the shortened matrix Uyf onto the full matrix covL:
covL(1:maL,1:maL) = zero
ir = 0
if(kPMLE == 0) then
  do i=1,maL
    IF(mfit < maL .and. ifit(i) > 2) CYCLE
    ir = min(ir+1,mfit)
    kr = 0
    do k=1,maL
       IF(mfit < maL .and. ifit(k) >= 2) CYCLE
      kr = min(kr+1,mfit)
      if(ifit(i) == 1 .and. ifit(k) == 1) then
        covL(i,k) = Uyf(ir,kr)
      end if
    end do
  end do
else
  ! covL(1:maL,1:maL) = cxp(1:maL,1:maL)
  do i=1,maL
    covL(i,1:maL) = cxp(i,1:maL)
  end do
end if
  if(.false. .and. kableitnum == 0 .and. .not. iteration_on ) then
    call matwrite(covL,maL,maL,66,'(1x,130es16.8)', &
                'End of Lincov2: Matrix covL:')
    write(66,*) 'fit parameter yvar: ',real(yvar,8)
  end if
IF(kPMLE == 1 .AND. mfrbg > 0 ) then
  if(ifit(mfrbg) == 2) THEN
    yvar(mfrbg) = xparBGval
  end if
end if

end subroutine LinCov2

!#######################################################################

!  The weighted least squares subroutine LsqLinCov2 calculates the fitting
!  parameters y (usually activities) for a fitting function which is linear
!  in the measured values x, usually count rates. It is possible that the
!  measured values are correlated, which requires a non-diagonal covariance
!  matrix covy.
!  The partial derivatives of the fitting function with respect to the
!  fitting parameters are elements of a matrix A.
!
!  The matrix algebra methodD is taken from then textbook:
!     Klaus Weise u. Wolfgang Wöger: Meßunsicherheit und Meßdatenauswertung.
!     Verlag Wiley-VCH Weinheim,1999,
!     S. 200 oben (Abschnitt 5.4.2 Lineare Kurvenanpassung)
!
!  For the necessary matrix algebra, if not directly available from the Fortan
!  compiler, routines are used which are taken from the textbook
!
!   Datan-Library (Fortran) from:
!   Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und Computerprogrammen;
!   4. Auflage. Spektrum, Akademischer Verlag, Heidelberg-Berlin. In German.
!   This text book is also available in an English version.
!
!  The results of LsqLinCov2 are successfully compared  with the Datan subroutine
!  LSQGEN (works iteratively). In fact, LSQGEN performs the much more complex
!  weighted total least squares (WTLS) method!
!
!  25.10.2005 Günter Kanisch, BFAFi Hamburg, Institut für Fischereiökologie

!  Note about matrices in Fortran:
!  the 1st index counts the rows, the 2nd index counts the columns of a matrix.
!
!  Copyright (C) 2014-2023  Günter Kanisch
!

!--------------------------------------------------------------------------

module SUBROUTINE LsqLinCov2(x,covy1,n,nr,y,Uy,r,a,ok,maL,xparbgval,ifehl)

USE UR_Linft,     ONLY: xA,kPMLE,ifit,mfrbg,posdef,klincall         ! x1a,x2a,x3a,
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

integer(4)       :: i,kn,k,j,mm0
real(rn)         :: aFunc(maL)

real(rn),allocatable  :: cs(:),xh(:),cpy(:), Ux(:,:)
!-----------------------------------------------------------------------

allocate(cs(n), xh(nr), cpy(n))
allocate(Ux(n,n))

cpy(1:n) = x(1:n)
ifehl = 0
! Prepare the design matrix A (amt), dimension (n x nr), refers only to parameters
! to be fitted (according to the array ifit):
a(1:n,1:nr) = zero

IF(kPMLE == 1) THEN
  !if(allocated(x1a)) deallocate(x1A,x2A,x3A)
  !allocate(x1A(n),x2A(n),x3A(n))
  !x1A(1:n) = zero
  !x2A(1:n) = zero
  !x3A(1:n) = zero
  if(allocated(xa)) deallocate(xA)
  allocate(xA(3,n))

end if

do i=1,n
  call funcs(i,afunc)     ! External function, yields for y(i) values aFunc(1:maL),
                          ! which are considered as the partial derivatives with respect
                          ! to the fitting parameters
  if(ifehl == 1) return

  ! array ifit:  ifit(k):  =1: parameter k is fitted;   =2: hold paramter k fiexed at its start value;
  !                        =3: parameter k is not included in fitting;   kn = 0

  kn = 0
  do k=1,maL
    IF(ifit(k) == 1) THEN
      kn = kn + 1
      a(i,kn) = afunc(k)
      IF(kPMLE == 1) then
        if(k == 1) xa(1,i) = afunc(k)
        if(k == 2) xa(2,i) = afunc(k)
        if(k == 3) xa(3,i) = afunc(k)
      end if
    end if
  end do
  IF(kPMLE == 1 .and. mfrbg > 0) then
    if(ifit(mfrbg) == 2) THEN
      IF(mfrbg == 2) xa(2,i) = one
      IF(mfrbg == 3) xa(3,i) = one
    end if
  end if
end do

!IF(klincall == 1 .and. .not. iteration_on .AND. limit_typ < 1 .and. kableitnum == 0) THEN
! write(66,*) 'LSQLIN: vector x: ',sngl(x)
! call matwrite(A,n,nr,n,nr,66,'(20es11.4)','Matrix A in LsqLinCov2,   last column: net rate' )
!end if

! For maintaining the original input matrix covy1,
! it is copied to the matrix Ux, which then can be inverted
Ux(1:n,1:n) = covy1(1:n,1:n)

IF(.false.) then
  if(klincall == 1 .and. .not.iteration_on .AND. limit_typ < 1 .and. kableitnum == 0) THEN
    call matwrite(Ux,n,n,66,'(1x,60es11.4)','Lincov2: inoput data: Ux=covyLF:')
  end if
end if

! invert the matrix Ux (covariance marix of the x values), with Cholesky decomposition
 if(n == 1) then
   Ux(1,1) = one / Ux(1,1)
elseif(n > 1) then
  CALL mtxchi(Ux)      ! Ux now contains its inverse
end if

if(.not.posdef) then
    call matwrite(Ux,n,n,66,'(1x,60es10.3)', &
                'Lincov2: Ux not pos. def.,  Ux:')
  write(66,*) 'Lincov2: Vektor x: '
  write(66,'(100es10.3)') (real(x(i),8),i=1,n)
  write(66,*)
  call WrStatusbar(3,'Lincov2: Matrix Ux not pos.def.')
  if(langg == 'EN')call WrStatusbar(3,'Lincov2: Matrix Ux not pos.def.')
  if(langg == 'FR')call WrStatusbar(3,'Lincov2: Matrice Ux non déf. pos.')
  ifehl = 1
  return
end if

! First step of the LS analysis: calculate the inverse of the
! covariance matrix of the output quantities:
Uy = matmul(Transpose(a), matmul(Ux, a))

! Uy is still to be inverted:
if(nr == 1) then
  Uy(1,1) = one / Uy(1,1)
elseif(nr > 1) then
  CALL mtxchi(Uy)       ! now Uy is the desired covariance matrix
end if

if(.not.posdef) then
    call matwrite(Uy,nr,nr,66,'(1x,60es11.4)', &
                'Lincov2: mtxchi:Uy not pos. def.:  Uy:')

  call WrStatusbar(3,'Lincov2: Matrix Uy not pos.def.')
  if(langg == 'EN') call WrStatusbar(3,'Lincov2: Matrix Uy not pos.def.')
  if(langg == 'FR') call WrStatusbar(3,'Lincov2: Matrice Uy non déf. pos.')

  if(langg == 'DE') call WrStatusbar(4,'Lincov2: Abbruch!')
  if(langg == 'EN') call WrStatusbar(4,'Lincov2: Abortion!')
  if(langg == 'FR') call WrStatusbar(4,'Lincov2: Avortement!')
    !call matwrite(covy,n,n,n,n,66,'(1x,40es12.5)', &
    !            'Lsqlincov2: Matrix Qxp   Before QMAT treatment:')
  ifehl = 1
  return
end if

IF(.false.) then
  if(.not. iteration_on .AND. limit_typ < 1 .and. kableitnum == 0) THEN
    mm0 = min(nr,6)
    call matwrite(Uy,mm0,mm0,66,'(1x,20es16.8)','Lsqlincov2: Matrix Uy:')
  end if
end if

! Second step of the LS analysis: calculate the vector y of output quantity values
y = Matmul(Uy, Matmul(Transpose(a), Matmul(Ux, x)))

! Third step: calculate r=Chisqr-min, according to Eq. (5.65) in Weise/Wöger:
cs = Matmul(Ux,x)
xh = Matmul(Transpose(a),cs)

r = dot_product(x,cs)
r = r - dot_product(y, xh)

ok = .TRUE.

xparbgval = zero

!-----------------------------------------------------------------------------------

END SUBROUTINE LsqLinCov2

!######################################################################################


module subroutine RunPMLE(x,covy1,n,nr,y,yp,cyp,r,maL,xparbgval,ifehl)

USE UR_Linft,     ONLY: kPMLE,k_rbl,ifit,d0zrate,sd0zrateSV,fpaSV,singlenuk,      &
                        mfrbg,mfrbg_2_fitnonlin
USE UR_Gleich,    ONLY: kpoint,StdUnc,Messwert,kableitnum
USE UR_DLIM,      ONLY: iteration_on,limit_typ
USE UR_MCC,       ONLY: imc
use Brandt,       only: mtxchi
use Num1,         only: funcs,matwrite
! use Fpmle,        only: FitDecayPMLE,PMFit
use UR_params,    only: rn,zero,one,two
use Top,          only: dpafact
USE UR_Variables, ONLY: MCSim_on
use UR_Derivats,  only: dervtype

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

integer(4)            :: i,k,mfit1,ifitSV2(3)
real(rn)              :: sdx(n),x1(n),ypw(maL),cypw(maL,maL),FV1(3),chisq,dpix
real(rn)              :: mw_rbl,umw_rbl,vadd(3)
real(rn),allocatable  :: dpi1LF(:,:,:)
!-------------------------------------------------------------------------------------
! For PMLE, the next "free" fitting parameter (i.e. number mfrbg) of in total 3 fitting parameters
! is taken to represent the sum of background and blank count rates. The existing choice
! (fit/fixed/not used) must not be changed by the user.
! Internally, the parameter of number mfrbg is fully fitted in FitDecayPMLE. However, this is
! meaningful only, if the parameter numbers < mfrbg do NOT represent a quasi-constant behavior
! over time; if the latter would be true, the fitting routine would not yield reliable values of
! the two parameters which are in concurrence then.
!
! the parameter mfrbg_2_fitnonlin is set to .true. in Uncw_Init.( Parameter is non-linearly fitted)
!-------------------------------------------------------------------------------------
ifehl = 0
dervtype = 'A'

                   write(66,*) ' Start RunPMLE:'
if(allocated(dpi1LF)) deallocate(dpi1LF);    allocate(dpi1LF(n,3,3))
dpi1LF(:,:,:) = zero
ifitSV2(1:maL) = ifit(1:maL)

x1(1:n) = x(1:n) + d0zrate(1:n)
if(k_rbl > 0) x1(1:n) = x1(1:n) + MEsswert(kpoint(k_rbl))
sdx(1:n) = [ (sqrt(covy1(i,i)),i=1,n) ]

mw_rbl = zero
umw_rbl = zero
if(k_rbl > 0) mw_rbl = Messwert(kpoint(k_rbl))
if(k_rbl > 0) umw_rbl = StdUnc(kpoint(k_rbl))

IF(.not.iteration_on) THEN
  !WRITE(66,*) 'LLCOV2: before call PMLE    pars y: ',(sngl(y(i)),i=1,nr),'  nr=',nr,'  ifit=',ifit
  !do i=1,n
  !  WRITE(66,*) 'before PMLE: i=',i,' x(i)=',sngl(x(i)),'  d0zrate(i)=',sngl(d0zrate(i)),'  sd0zrate(i)=',sngl(sd0zrate(i))
  !end do
end if

mfit1 = 0
if(allocated(yp)) deallocate(yp);   allocate(yp(maL))
if(allocated(cyp)) deallocate(cyp);  allocate(cyp(maL,maL))
yp = zero
cyp = zero

IF(nr < maL) THEN
  do i=1,maL
    IF(ifit(i) == 1) THEN
      mfit1 = mfit1 + 1
      yp(i) = y(mfit1) * 1.02_rn
    end if
  end do
else
  do i=1,maL
    IF(ifit(i) == 1) then
      yp(i) = y(i) * 1.02_rn
      mfit1 = mfit1 + 1
    end if
  end do
end if
IF(ABS(yp(mfrbg)) < 1.E-12_rn) yp(mfrbg) = fpaSV(mfrbg)

IF(singlenuk .AND. iteration_on .and. kPMLE == 1 .and. mfrbg > 0 .and. ifit(mfrbg) == 0) THEN
  yp(mfrbg) = fpaSV(mfrbg)
end if

IF(yp(mfrbg) <= zero .and. ifit(mfrbg) == 2) then
  yp(mfrbg) = d0zrate(1) + mw_rbl
END IF
!----------------------------------------------------------------
   !  WRITE(66,*) 'LLCOV2: before call PMLE    pars yp: ',(sngl(yp(i)),i=1,maL),'  nr=',int(nr,2),'  ifit=',int(ifit,2)
call FitDecayPMLE(x1,sdx,n,yp,maL,cyp,maL,chisq,ifehl)
IF(ifehl == 1) then
  write(66,*) 'Error in FitDecayPMLE: '
  RETURN
END IF
    ! WRITE(66,*) 'LLCOV2: after  call PMLE    pars yp: ',(sngl(yp(i)),i=1,maL),'  nr=',int(nr,2),'  ifit=',int(ifit,2)
    ! WRITE(66,*) 'LLCOV2: after  call PMLE   sd of yp: ',(sngl(sqrt(cyp(i,i))),i=1,maL)
!-----------------------------------------------------------------
IF(mfrbg > 0 .and. ifit(mfrbg) == 2) THEN
  IF(mfrbg == 2) xparbgval = yp(2)
  IF(mfrbg == 3) xparbgval = yp(3)
end if
IF(.true. .and. mfrbg > 0 .and. ifit(mfrbg) == 2) THEN
  ! Take into account the uncertainties of the background and a blank value
  ! by uncertainty propagation:
  do i=1,maL
    ypw(i) = yp(i)
    Fv1(i) = ypw(i)
    cypw(i,1:maL) = cyp(i,1:maL)       ! ypw and cypw are defined locally
  end do
  vadd = zero
else
  do i=1,n
    do k=1,maL
      IF(ifit(k) == 1) THEN
            !   write(66,*) 'LLV: 903:  mfrbg=',mfrbg ,'  ifit(mfrbg)=',mfrbg
        IF(.not.iteration_on) then
          dpix = dpi1LF(i,k,1)
        else
          IF(limit_typ == 1) dpix = dpi1LF(i,k,2)
          IF(limit_typ == 2) dpix = dpi1LF(i,k,3)
        end if
        cyp(k,k) = cyp(k,k) + dpix**two * ( sd0zrateSV(i)**two + umw_rbl**two )
      end if
    end do
  end do
end if
!-------------------------------------------------------------------------------------
r = chisq

  if(kableitnum== 0 .and. .not.iteration_on) then
   ! write(66,*) 'RunPMLE:  vector yp=',sngl(yp(1:3))
   ! call matwrite(cyp,maL,maL,maL,maL,66,'(5es12.4)','RunPMLE:   matrix cyp:')
  end if

  ! IF(.not.MCSim_on .and. .not.iteration_on) WRITE(66,*) 'LLCOV2: before end  pars y: ',(sngl(y(i)),i=1,nr)
  ! call matwrite(cy,nr,nr,nr,nr,66,'(3es11.4)','End of RunPMLE: matrix cy')

 ! WRITE(66,*) 'End of RunPMLE : fpa(1-3)=',(sngl(xp(i)),i=1,3),'  ifit=',ifit,'  R=',sngl(R)
  ifit(1:maL) = ifitSV2(1:maL)

end subroutine RunPMLE

!#######################################################################################

module subroutine FitDecayPMLE(yv1,syv1,nv1,pa,ma,covpa,nca,chisq, ifehl)

      ! This routine performs a non-linear Poisson maximum likelihood fit (PMLE)
      ! of the measured decay curve. It uses the non-linear Levenberg-Marquardt
      ! least squares fitting routine Lsqmar (textbook "Datenanalyse" by S. Brandt).
      !
      ! FitDecay PMLE uses the option in Lsqmar for applying Poisson MLE as it was
      ! described by Timo Hauschild and M. Jentschel (ILL Grenoble,
      ! FZ Rossendorf). See:
      !   Hauschild, T., Jentschel, M., 2001. Comparison of maximum likelihood estimation
      !   and chi-square statistics applied to counting experiments.
      !   Nucl. Instr. & Meth A 457 (1-2), S 384-401.
      !
      ! Poisson MLE is recommended for the low counts region, where the application of
      ! a normal distribution would lead to biases, e.g. of background counts. So,
      ! this is a good option when, e.g., analyzing low-count alpha spectra.
      !
      ! This routine is called by:  linf --> lincov2 --> runPMLE --> FitDecayPMLE
      !
      ! The fitting function is given the user function Lsqfpmle.
      !
      !   Copyright (C) 2014-2023  Günter Kanisch

USE UR_Linft,      ONLY: xA,k_rbl,d0zrate, &                    ! x1a,x2a,x3a,
                         ifit,mfrbg,tmedian,condition_upg,  &
                         use_constr,kconstr,pcstr,upcstr,penalty_factor, &
                         mfrbg_2_fitnonlin,mfix,indfix,xfix
USE UR_Gleich,     ONLY: Messwert,kpoint,kEGr,kableitnum
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
real(rn),allocatable,INTENT(OUT)   :: covpa(:,:)        ! covariance matrix of parameters a()
real(rn),allocatable,INTENT(INOUT) :: pa(:)              ! fitting parameters, count rates

integer(4),INTENT(OUT)  :: ifehl                       ! error indicator

LOGICAL          :: iterout
integer(4)       :: i,k,mfit,jpr,nfree,iap(3),kqt
real(rn)         :: spa(ma),chisqr
real(rn)         :: y1A(nv1),sy1A(nv1)
integer(4)       :: nstep
real(rn)         :: yy,mw_rbl
real(rn),allocatable   :: aa(:,:),scrat(:,:),px(:),py(:),puy(:)
integer(4),allocatable :: list(:)

!--------------------------------------------------------------------------
jpr = 66
ifehl = 0
            write(66,*)  ' FitDecayPMLE begin:  kableitnum=',int(kableitnum,2)
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
            ! if(iterout) write(0,*) 'ifit=',int(ifit,2)    ! -->  1, 2 or 3!
 mw_rbl = zero
 if(k_rbl > 0) mw_rbl = Messwert(kpoint(k_rbl))        ! (net) blank value
 IF(MCSim_on .and. kqt == 2) then
   pa(kEGr) = 1.E-7_rn         ! decision threshold case in MC simulation
 end if

 IF(iterout) WRITE(66,'(a,3(es12.5,1x),a,3(es12.5,1x),a,i3)') 'FitPMLE:   start parameters: ', &
                         (pa(i),i=1,ma),'  x1a,x2a,x3a=',xA(1,1),xa(2,1),xa(3,1),'  mfrbg=',mfrbg
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
  end if
end do

if(mfrbg_2_fitnonlin) then
  iap(mfrbg) = 1   !  mfrbg designates the parameter of back ground count rate
  mfit = mfit + 1
end if
nfree = nv1 - mfit
if(allocated(list)) deallocate(list);   allocate(list(ma))
list(1:ma) = iap(1:ma)
if(allocated(indfix)) deallocate(indfix);  allocate(indfix(ma-mfit))
if(allocated(xfix)) deallocate(xfix);  allocate(xfix(ma-mfit))
indfix = 0
xfix = zero

!IF(ABS(pa(mfrbg)) < 1.E-12_rn) THEN
  pa(mfrbg) = mean(d0zrate(1:nv1))   !  is average of background count rates d0zrate())
        if(k_rbl > 0) pa(mfrbg) = pa(mfrbg) + mw_rbl   ! xxxxxxxxxxxxxxxxxxxxxxxxxx
!end if
pa(1:ma) = pa(1:ma)  * tmedian ! convert fitting parameters to counts; tmedian: median(counting times)

IF(iterout) WRITE(jpr,'(3(a,i3),a,3i3)') 'PMLE:  ma=',ma,' nca=',nca,' mfit=',mfit,'  ifit=',ifit
             write(66,*) 'fitPMLE:  pa: ',sngl(pa)
! apply constraints to the parameter ab(mfrbg)?
use_constr = .false.   ! .true.
kconstr = 0
IF(use_constr) THEN
  penalty_factor = 0.05_rn
  ! Constraints for BG Parameter:
  pcstr(mfrbg) = pa(mfrbg)     ! mean(d0zrate(1:nv1)) * tmedian          ! pa(mfrbg)
  upcstr(mfrbg) = 0.50_rn * sqrt(pa(mfrbg))
  kconstr(mfrbg) = iap(mfrbg)
     ! write(0,*) 'pcstr(mfrbg)=',sngl(pcstr(mfrbg)),'   upcstr(mfrbg)=',sngl(upcstr(mfrbg))
     ! write(0,'(2(a,i0),a,10es11.4)') 'kconstr(mfrbg)=',kconstr(mfrbg),' mfrbg=',mfrbg, &
     !                                    'pa=',pa
end if

if(allocated(aa)) deallocate(aa);    allocate(aa(nv1,mfit))
if(allocated(scrat)) deallocate(scrat); allocate(scrat(mfit,mfit))
aa = zero
scrat = zero
! store the "fixed" information:
mfix = 0
indfix = 0
xfix = zero
do i=1,ma
  if(iap(i) == 0) then
    mfix = mfix + 1
    xfix(mfix) = pa(i)       ! fixed parameter
    indfix(mfix) = i
  end if
end do
nstep = 30
         ! write(0,*) 'list=',int(list,2),'  iap=',int(iap,2),' ma=',int(ma,2),' nv1=',int(nv1,2)
         ! write(0,*) 'pa=',sngl(pa)
call lsqmar(Lsqfpmle,px,py,puy,nv1,ma,list,pa,covpa,chisq,aa,scrat,nstep)
chisqr = chisq / real(MAX(1,nv1 - mfit),rn)
      ! write(66,*) 'pa=',sngl(pa)
      ! write(66,*) 'chisqr=',sngl(chisqr)

 ! switch back to count rate parameters a
 pa = pa / tmedian
 covpa = covpa / tmedian**two

IF(iterout ) THEN

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

end if
!------------------------------------------------------------------------------------

end subroutine FitDecayPMLE

!#######################################################################


!############################################################################################

end submodule LLcov2a
