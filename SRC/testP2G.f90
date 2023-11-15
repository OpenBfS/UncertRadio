subroutine testP2G

  ! test fitting methods for two gaussian peaks on a constant low backgroud
  ! results: see CHM Help, chapter 7.4.3

use UR_params,    only: rn,two,zero,one
use Brandt,       only: Lsqmar,Lsqlin,mean,sd,mtxchi,backsort,fixprep
use RND,          only: Rndu,UR_random_seed
use UR_mcc,       only: idum
use Num1,         only: matwrite
use UR_Linft,     only: mfix,indfix,xfix,use_PMLE,use_PLSQ,use_constr,use_WLS
use UR_Derivats,  only: dervtype
! use RndGInt,      only: indx
use UR_plotp
use UR_interfaces, only: plot3fig
use fgsl

implicit none

integer(4), parameter  :: nrep = 400
integer(4)             :: nr,n,nstep,i,nred,irun,zt1(9),irunmx,nrx,iwh,iwhx
real(rn)               :: chisq,chisqr,LsqP2G,parea,rnnd,sumdev_p,sumdev_sd,pposi(2)
real(rn)               :: bgh,fy,chisq1(1,1),area_true,areaf,area_ratio
real(rn),allocatable   :: a(:,:),scrat(:,:),t(:),y(:),uy(:),afunc(:),diff(:,:),xt(:,:),pa3a(:)
real(rn),allocatable   :: pa(:),covpa(:,:),covpared(:,:),ptrue(:),paLin(:),covpaLin(:,:),pa3(:,:)
real(rn),allocatable   :: pared(:),pafit(:),mline(:),values(:,:),sds(:,:), Ux(:,:),mUy(:,:),Amat(:,:)
integer(4),allocatable :: list(:),list2(:)
logical                :: plot_two,fit_nonlinear
character(len=60)      :: pltfile2
character(len=30),allocatable      :: ctextL(:)
type(fgsl_rng)         :: r5
type(fgsl_rng_type)    :: t5
integer(fgsl_long)     :: fgsl_idum
external         linP2G

  t5 = fgsl_rng_env_setup()
  t5 = fgsl_rng_default
  r5 = fgsl_rng_alloc (t5)

!call test_expand()
!return


use_PMLE = .false.
   !  use_PMLE = .true.
use_PLSQ = .false.

use_constr = .false.
dervtype = 'N'
plot_two = .false.
  ! plot_two = .true.
fit_nonlinear = .true.
  ! fit_nonlinear = .false.


n = 150           ! number of channels
  ! if(plot_two) n = 900 ! für Plotte
pposi = [30._rn, 90._rn]          ! peak positions in channels
   ! if(plot_two) pposi = pposi * 6  ! für Plotten
nr = 6        ! number of fit parameters
! define the array list first, to allow for aray allocations
allocate(list(nr))
 list = 1         ! = 1 : parameters are fitted
    ! list(3) = 0   ! 0 means: don't fit  xxx
nred = 0
do i=1,nr
  if(list(i) == 1) nred = nred + 1    ! number of fitted (not fixed) parameters
end do

allocate(a(n,nred), scrat(nred,nred), t(n),y(n),uy(n),pa(6),covpa(6,6),pared(nred))
allocate(covpared(nred,nred),mline(nr),indfix(nr-nred),xfix(nr-nred),pafit(nr))
allocate(values(nrep,nr),sds(nrep,nr),ptrue(1:nr),ctextL(nr))
allocate(paLin(nr),covpaLin(nr,nr),list2(nr))
allocate(Ux(n,n),mUy(3,3),Amat(n,3),pa3(3,1),afunc(3),diff(3,1),xt(n,1),pa3a(3))

call date_and_time(values=zt1)                       !values=[year, month, day, gmt_min, hr,min,sec,msec]
idum = zt1(7)*60 + zt1(6)
! idum =  6543987
write(66,*) 'idum=',idum
call UR_random_seed(idum)
fgsl_idum = idum
rnnd = Rndu()
call fgsl_rng_set(r5, fgsl_idum)

write(66,*) (int(fgsl_ran_poisson(r5, 4.75d0),2),i=1,10)


irunmx = nrep
if(plot_two) irunmx = 1

 use_PMLE = .false.
 use_PLSQ = .false.
 use_WLS = .true.

40   continue

do irun=1,irunmx

  ! set "true" paramater values of a gamma spectrum with two peaks:
  parea = 150._rn ! * 10._rn
  bgh = 4._rn
 ! if(plot_two) bgh = 50._rn
  pa(1:6) = [bgh, parea, pposi(1), 5.1_rn, parea, pposi(2) ]
     ! if(plot_two) pa(4) = pa(4) * 6._rn   ! für Plotten
  ptrue(1:6) = pa(1:6)

  ! if(irun == 1) then
  nred = 0
  mfix = 0
  indfix = 0
  xfix = zero
  do i=1,nr
    if(list(i) == 1) then
      nred = nred + 1
      pared(nred) = pa(i)      ! reduced set of parameters
    else
      mfix = mfix + 1
      xfix(mfix) = pa(i)       ! fixed parameter
      indfix(mfix) = i
    end if
  end do
  ! end if
             write(66,*) 'irun=',int(irun,2),'  pared=',sngl(pared(1:nred))
  ! Generate Poisson random values of a "measured" spectrum:
  area_true = zero
  do i=1,n
    t(i) = real(i,rn)
    ! user function, requires reduced parameter set
    y(i) = LsqP2G(pared,nred,t(i))           ! ,mfix,indfix,xfix)
            area_true = area_true + y(i)
    if(irunmx >= 1) then
      y(i) = fgsl_ran_poisson(r5, real(y(i),8))
      ! !!! y(i) = max(0.5_rn, y(i))                     ! 30.6.2023 weggenommen
      y(i) = max(0.5_rn, y(i))
       ! y(i) = y(i) + sqrt(max(0.5_rn,y(i)))*rnorm()
    end if
    uy(i) = sqrt(max(y(i),2.0_rn*0.5_rn))
  end do
  if(.false.) then
    do i=1,n
      write(66,*) 'i=',int(i,2),' y(i)=',sngl(y(i))
    end do
  end if

  if(fit_nonlinear) then
    ! Take modified parameters as initial guess:
    pa(1:6) = [0.8_rn*bgh, 0.8*parea, 29.3_rn, 4.8_rn, parea*1.1, 90.8_rn ]
      ! if(plot_two) pa(1:6) = [5._rn, 0.8*parea, 29.3_rn*6._rn, 4.8_rn*6._rn, parea*1.1, 90.8_rn*6._rn ]

    nstep = 150
    ! call with full set pa of parameters:
    call LSQmar(LsqP2G,t,y,uy,n,nr,list,pa,covpa,chisq,a,scrat,nstep)
      write(66,*) 'nstep=',int(nstep,2)
    nrx = nr
    areaF = zero
    nred = 0
    do i=1,nr
      if(list(i) == 1) then
        nred = nred + 1
        pared(nred) = pa(i)      ! reduced set of parameters
      end if
    end do
    do i=1,n
      areaF = areaF + LsqP2G(pared,nred,t(i))
      if(nstep == -2) write(66,*) 'i=',int(i,2),' y(i)=',sngl(y(i)),' uy=',sngl(uy(i))
    end do
    area_ratio = area_ratio + areaF/area_true

  else
    nstep = 1
    list2 = 0
    list2(1) = 1
    list2(2) = 1
    list2(5) = 1
    paLin(1:6) = [0.8_rn*bgh, 0.8*parea, 30.0_rn, 5.1_rn, parea*1.1, 90.0_rn ]
    call Lsqlin(LinP2G,t,y,uy,n,nr,list2,paLin,covpaLin,chisq)
    nrx = 3
    pa(1:nr) = paLin(1:nr)
    covpa(1:nr,1:nr) = covpaLin(1:nr,1:nr)
         write(66,*) 'nach lin: nr=',int(nr,2),' pa=',sngl(pa)
  end if

  chisqr = chisq / real(n-nrx,rn)
  if(plot_two) chisqr = chisq / real(n-nrx,rn)
  if(nstep <= 0) return
   ! call matwrite(covpa,nr,nr,nr,nr,66,'(10(es13.5))',' Matrix covpa:')

  write(66,*) 'chisqr=',sngl(chisqr),'  nstep=',nstep
  do i=1,nr
    values(irun,i) = pa(i)
    sds(irun,i) = sqrt(covpa(i,i))
    if(nstep > 0) write(66,'(a,i0,2(a,es12.5))') 'i=',i,'  pa=',pa(i),'  upa(i)=',sqrt(covpa(i,i))
    if(nstep < 0) write(66,'(a,i0,2(a,es12.5))') 'i=',i,'  pa=',pa(i)
  end do
  write(66,*)
  !if(abs(pa(5)) < 1.E-15_rn) then
  !  do i=1,n
  !    write(66,*) 'i=',int(i,2),' y(i)=',sngl(y(i))
  !  end do
  !end if

end do     ! irun

if(irunmx > 1) then

  write(66,*) '---------------------------------------------'
  write(66,*)
  write(66,*)  'mean values:    use_WLS=',use_WLS,'  use_PMLE=',use_PMLE,'  use_PLSQ=',use_PLSQ
  area_ratio = area_ratio /real(nrep,rn)
  sumdev_p = zero
  sumdev_sd = zero
  do i=1,nr
    if(abs(covpa(i,i)) < 1.E-20_rn) cycle
     write(66,'(a,i0,3(a,es12.5))') 'i=',i,'  pa=',mean(values(1:nrep,i)), &
                '  sd(pa(i))=',sd(values(1:nrep,i)),' mean(sds(i))=',mean(sds(1:nrep,i))
    sumdev_p = sumdev_p + abs(mean(values(1:nrep,i))-ptrue(i))
    sumdev_sd = sumdev_sd + abs(sd(values(1:nrep,i))-mean(sds(1:nrep,i)))
       !  write(66,*) ' sumdev_p=',sngl(sumdev_p)
  end do
  write(66,*) 'sum of absolute deviations from true(pa)   : ',sngl(sumdev_p)
  write(66,*) 'sum of absolute deviations of two sd types : ',sngl(sumdev_sd)
  write(66,*) 'mean of (fitted area / true area)          : ',sngl(area_ratio)
end if
if(.not.fit_nonlinear) then
  if(.not.use_PLSQ) then
    use_PLSQ = .true.
    goto 40
  end if
  if(use_PLSQ) then
    use_PLSQ = .false.
    goto 40
  end if
end if
if(fit_nonlinear) then
  if(use_WLS) then
    use_WLS =.false.
    use_PLSQ = .true.
    goto 40
  end if
  if(use_PLSQ) then
    use_PMLE = .true.
    use_PLSQ = .false.
    goto 40
  end if

end if

if(plot_two) then
   pa(1:6) = [4._rn, parea, 30.0_rn, 5.1_rn, parea, 90.0_rn ]
  do i=1,n
    plty(1,i) = y(i)
    pltx(1,i) = t(i)
  end do
  do i=1,n
    pltx(2,i) = t(i)
    plty(2,i) = LsqP2G(pa,nr,t(i))
  end do
  ymaxv = maxval(plty(2,1:n))

  use_PMLE = .false.
  ! Take modified parameters as initial guess:
  ! pa(1:6) = [5._rn, 0.8*parea, 29.3_rn, 4.8_rn, parea*1.1, 90.8_rn ]
   pa(1:6) = [4._rn, 0.8*parea, 29.3_rn*1._rn, 4.8_rn*1._rn, parea*1.1, 90.8_rn*1._rn ]
  nstep = 150
  call LSQmar(LsqP2G,t,y,uy,n,nr,list,pa,covpa,chisq,a,scrat,nstep)
  chisqr = chisq / real(n-nr,rn)
  write(66,*) 'chisqr=',sngl(chisqr),'  nstep=',nstep
  do i=1,nr
    if(nstep > 0) write(66,'(a,i0,2(a,es12.5))') 'i=',i,'  pa=',pa(i),'  upa(i)=',sqrt(covpa(i,i))
    if(nstep < 0) write(66,'(a,i0,2(a,es12.5))') 'i=',i,'  pa=',pa(i)
  end do
  do i=1,n
    pltx(4,i) = t(i)
    plty(4,i) = LsqP2G(pa,nr,t(i))
  end do

  use_PMLE = .true.
  ! Take modified parameters as initial guess:
  pa(1:6) = [5._rn, 0.8*parea, 29.3_rn, 4.8_rn, parea*1.1, 90.8_rn ]
   pa(1:6) = [4._rn, 0.8*parea, 29.3_rn*1._rn, 4.8_rn*1._rn, parea*1.1, 90.8_rn*1._rn ]
  nstep = 150
  call LSQmar(LsqP2G,t,y,uy,n,nr,list,pa,covpa,chisq,a,scrat,nstep)
  chisqr = chisq / real(n-nr,rn)
  write(66,*) 'chisqr=',sngl(chisqr),'  nstep=',nstep
  do i=1,nr
    if(nstep > 0) write(66,'(a,i0,2(a,es12.5))') 'i=',i,'  pa=',pa(i),'  upa(i)=',sqrt(covpa(i,i))
    if(nstep < 0) write(66,'(a,i0,2(a,es12.5))') 'i=',i,'  pa=',pa(i)
  end do
  do i=1,n
    pltx(3,i) = t(i)
    plty(3,i) = LsqP2G(pa,nr,t(i))  ! /3._rn
  end do

  knum = 4
  nkpts(1:4) = n
  xminv = 0._rn
  xmaxv = n
  yminv = 0._rn
  ymaxv = 25._rn              !  max(ymaxv,maxval(plty(3,1:n)))
    write(ptitle(1),'(a,f7.1,a,f7.1)') '2 peaks'
    pltfile2 = 'bild_testP2GPdxGM_curve.png'
    ctextL(1:4) = ['Spektrum','true    ','PMLE-Fit','WLS-Fit ']

   call plot3fig(4,nkpts, [1,2,3,4],[67,0,3,2],[0.02d0,5.d0,2.5d0,3.5d0], &
             .false.,.false.,'Kanal','Impulse',trim(ptitle(1)), pltfile2, &
         mimax=[xminv,xmaxv],mimay=[yminv,ymaxv],cTextL=cTextL )
 end if

end subroutine testP2G

!#############################################################################

real(rn) function LsqP2G(xv,nred,t)
      ! From Datan library, modified by GK
      ! user function describing a sum of two gaussian peaks plus background
      ! polynomial;
      ! used for non-linear fitting with LSQmar

use UR_params,    only: rn,two,zero,pi
use UR_linft,     only: mfix,indfix,xfix
use Brandt,       only: expand
implicit none

integer(4),intent(in) :: nred            ! number of unfixed parameters
real(rn),intent(in)   :: xv(nred)        ! values of non-fixed parameters
real(rn),intent(in)   :: t               ! independent value of a measurement point

integer(4)          :: i,j
real(rn),parameter  :: big = 700._rn
real(rn)            :: back, arg1, arg2,gauss1,gauss2   ! ,x(nred+mfix)
real(rn),allocatable  :: x(:)

allocate(x(nred+mfix))

call expand(xv,nred,x)

! meaning of fit parameters:
! x(1)  : height background (counts)
! x(2)  : amplitude of the left peak (gaussian)
! x(3)  : channel position of the left peak (gaussian)
! x(4)  : gaussian width parameter (channels) of both peaks
! x(5)  : amplitude of the reight peak (gaussian)
! x(6)  : channel position of the right peak (gaussian)

back = x(1)
arg1 = (x(3)-t)**two/(two*x(4)**two)
arg2 = (x(6)-t)**two/(two*x(4)**two)
if(arg1 > big) then
  gauss1 = zero
else
  gauss1 = x(2)/sqrt(two*pi*x(4)**two)*exp(-arg1)
end if
if(arg2 > big) then
  gauss2 = zero
else
  gauss2 = x(5)/sqrt(two*pi*x(4)**two)*exp(-arg2)
end if
LsqP2G = back + gauss1 + gauss2

end function LsqP2G

subroutine LinP2G(tval,afunc,nr)
      ! From Datan library, modified by GK
      ! user function describing a sum of two gaussian peaks plus background
      ! polynomial;
      ! used for non-linear fitting with LSQmar

use UR_params,    only: rn,two,zero,pi
! use UR_linft,     only: mfix,indfix,xfix
implicit none

integer(4),intent(in) :: nr            ! number of unfixed parameters to be fitted
real(rn),intent(in)   :: tval          ! channel number
real(rn),intent(out)  :: afunc(nr)     !

real(rn)            :: arg1, arg2,x3,x4,x6     ! ,x(nr+mfix)

x3 = 30._rn
x4 = 5.1_rn
x6 = 90._rn

! meaning of fit parameters:
! x(1)  : height background (counts)
! x(2)  : amplitude of the left peak (gaussian)
! x(3)  : channel position of the left peak (gaussian)
! x(4)  : gaussian width parameter (channels) of both peaks
! x(5)  : amplitude of the reight peak (gaussian)
! x(6)  : channel position of the right peak (gaussian)

arg1 = (x3-tval)**two/(two*x4**two)
arg2 = (x6-tval)**two/(two*x4**two)

afunc(1) = 1.0_rn
afunc(2) = 1.0_rn/sqrt(two*pi*x4**two)*exp(-arg1)
afunc(3) = 1.0_rn/sqrt(two*pi*x4**two)*exp(-arg2)

end subroutine LinP2G


subroutine test_expand
use UR_params,    only: rn,two,zero,pi
use UR_linft,     only: mfix,indfix,xfix
use RND,          only: Rndu,UR_random_seed
use Brandt,       only: fixprep,expand
implicit none

integer,parameter :: nall = 10
integer(4)  :: idum
real(rn)    :: xall(nall), rnnd
integer(4)  :: list(nall)

integer(4)            :: nred,i,j,nfd,nrr
real(rn),allocatable  :: x(:),x2(:)

idum = 6543987
call UR_random_seed(idum)
rnnd = Rndu()

do i=1,nall
  xall(i) = real(i,rn)
end do

nrr = 0
do j=1,100

  do i=1,nall
    list(i) = 1
    if(Rndu() < 0.50_rn) list(i) = 0
    if(list(i) == 1) nrr = nrr + 1
  end do
  if(allocated(x)) deallocate(x)
  allocate(x(nrr))

  if(allocated(x2)) deallocate(x2)
  allocate(x2(nall))

  call fixprep(xall,nall,list,nred,x)

  call expand(x,nred,x2)  ! ,nall)

  nfd = 0
  do i=1,nall
    if(abs(xall(i)-x2(i)) > 1.E-5_rn ) nfd = 1
  end do
  write(66,'(10f6.1, a,i0)') xall, '  nfd=',nfd
  write(66,'(10i6)') list
  write(66,'(10f6.1)') x2
  write(66,*)
end do

end subroutine test_expand

