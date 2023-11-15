

!-----------------------------------------------------------
subroutine GlsqCalib(maKB,nkalpts,a_kalib,covar_kalib,ykalib,zuykalib,xkalib,uxkalib, &
                     chisq)
!  prepared on 6.8.2023
!
!     Copyright (C) 2023  Günter Kanisch

use UR_params,      only: rn,zero,one,eps1min
use UR_Linft,       only: mfit,mxind,posdef,ncofact,IfitKB
use UR_VARIABLES,   only: langg
use Top,            only: WrStatusbar
use UR_LSQG

implicit none

integer(4),intent(in)      :: maKB      ! number of fit parameters
integer(4),intent(in)      :: nkalpts   ! number of calibration points
real(rn),intent(inout)     :: a_kalib(maKB)
real(rn),intent(inout)     :: covar_kalib(maKB,maKB)
real(rn),intent(in)        :: xkalib(nkalpts),uxkalib(nkalpts)
real(rn),intent(in)        :: ykalib(nkalpts),zuykalib(nkalpts)
real(rn),intent(out)       :: chisq

integer(4)            :: ifehl,irun,nred,nstep,kunit
logical               :: printout
real(rn),allocatable  :: s(:),ds(:),t(:),dt(:),afunc(:)

printout = .true.
kunit = 23
allocate(s(nkalpts),ds(nkalpts),t(nkalpts*maKB),dt(nkalpts*maKB),afunc(maKB))

IF(printout) WRITE(kunit,'(/a)') &
  ' ====================== Calculations with WTLS:  ========================='

if(allocated(ifitKB)) deallocate(ifitKB)
allocate(ifitKB(maKB))
ifitKB = 1
mfit = maKB
nred = mfit
irun = 1
nstep = 15
mxind = 1

s(1:nkalpts) = ykalib(1:nkalpts)     ! Y / dependent values:
ds(1:nkalpts) = zuykalib(1:nkalpts)  ! uncertainties of Y / dependent values:
t(1:nkalpts) = xkalib(1:nkalpts)     ! X / independent values:
dt(1:nkalpts) = uxkalib(1:nkalpts)   ! uncertainties of X / independet values:

IF(irun > 1 .AND. printout) WRITE(kunit,'(/,1x,a)')    &
                '     Now calculation with ''fitted uncertainty'':'

call E7LSQ1Cal(a_kalib,ifitKB,nkalpts,nkalpts*(mxind+1),maKB,t,dt,s,ds,  &
           covar_kalib,chisq,nstep,kunit, mfit,irun,printout,maKB)

IF(nstep < 0) THEN
  ifehl = 1
end if
if(.not.posdef) then
  if(ncofact > 3) then
    ifehl = 1
    if(langg == 'DE') call WrStatusBar(4,'Abbruch. Matrix nicht posdef!')
    if(langg == 'EN') call WrStatusBar(4,'Abortion. Matrix not posdef!')
    if(langg == 'FR') call WrStatusBar(4,'Avortement. Matrix pas posdef!')
    return
  else
    posdef = .true.
  end if
end if

end subroutine GlsqCalib

!##########################################################################################

SUBROUTINE E7LSQ1Cal(x,list,m,n,nr,t,dt,s,ds,covar,chisq,nstep,  &
                    kunit,nred,irun,printout,maKB)   ! ,nch)

! Subroutine for preparing a call to LsqGen for doing weighted total least squares (WTLS)
! on polynomial representation (up to a degree of 3) of a calibration curve.
!
! This routine is a modified version (GK) of the E7LSQ1 routine of the Datan-Library
! described in the textbook by S. Brandt:
!
!   Datan-Library (Fortran) from:
!   Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und Computerprogrammen;
!   4. Auflage. Spektrum, Akademischer Verlag, Heidelberg-Berlin. In German.
!   This text book is also available in an English version.
!
! The calling sequence within the UncertRadio program is:
!   call Xkalfit() --> call GlsqCalib --> call E7LSQ1Calib()  --> call LsqGen()
!
!  E7LSQ1Calib calls the LsqGen routine and organizes the printout of input data
!  and output from LSQGEN.
!
!  11.3.2022: Before calling LSQGEN, the set of fit parameters (and associated covariance matrix)
!  is reduced to the subset xred of parameters to be fitted (number mfit).
!
!     Copyright (C) 2023  G�nter Kanisch

USE UR_LSQG            ! (i.e., maxn,maxnr,maxm,mad)
USE UR_Derivats,   ONLY: dervtype,dfda,dfde
USE UR_Linft,      ONLY: mfit,numd, &
                         WTLS_wild,Chisqr_NLS,  &
                         Chis_test,posdef,ncofact,cofact,klincall, &
                         cauchy_failed3,compare_WTLS,mfit,mxind
use UR_Mcc,        only: imc
USE UR_Variables,  only: MCSim_on
use UR_DLIM,       only: limit_typ , Iteration_on
use Usub3,         only: FindMessk
use UR_params,     only: rn,eps1min,zero,two
use Num1,          only: matwrite
use WTLS,          only: LsqGen,cy_repair
use Top,           only: WrStatusbar

implicit none

real(rn),INTENT(INOUT)     :: x(maKB)        ! vector of fit parameters
integer(4),INTENT(IN)      :: list(maKB)     ! fit param: 1: yes ;  2: fixed; 3: not used
integer(4),INTENT(IN)      :: m            ! number of constraining equations ##############
integer(4),INTENT(IN)      :: n            ! number measurements / measured values
integer(4),INTENT(IN)      :: nr           ! number of parameters
real(rn),INTENT(IN)        :: t(m*maKB)    ! vector of X values
real(rn),INTENT(IN)        :: dt(m*maKB)   ! vector of X values
real(rn),INTENT(IN)        :: s(m)         ! vector of Y values  (count rates)
real(rn),INTENT(IN)        :: ds(m)        ! vector of Y uncertainties
real(rn),INTENT(OUT)       :: covar(maKB,maKB) ! covariance matrix of fit parameters
real(rn),INTENT(OUT)       :: chisq
integer(4),INTENT(INOUT)   :: nstep        ! number of iterations, or, < 0: error indication
integer(4),INTENT(IN)      :: kunit        ! unit for printout
integer(4),INTENT(IN)      :: nred         ! number of fitted parameters ( <= nr)
LOGICAL,INTENT(INOUT)      :: printout
integer(4),INTENT(IN)      :: irun         ! Number of the run
integer(4),intent(in)      :: maKB         ! number of fit parameters

integer(4)         :: i,k,ir,kr,jx,mm,j,k0
integer(4)         :: ifail,m1,m2
integer(4)         :: nnew
integer(4)         :: kjun,kqt

real(rn)           :: rho(m),r,chisqr, cymin,cymax
real(rn)           :: xred(maKB)
real(rn),allocatable     :: y(:),cy(:,:),cx(:,:)
integer(4),allocatable   :: list2(:)

character(len=60)  :: stformat
character(len=100) :: crestrict

! EXTERNAL        LsqGfn2

!-----------------------------------------------------------------------
   !  compare_WTLS is set in Uncw_init !

dervtype = 'A'  !  'N'      ! analytical or numerical partial derivatives

  if(.false.) then
     write(kunit,'(10(a,i0))') 'maKB=',maKB,' m=',m,' n=',n,' nr=',nr,' maxm=',maxm,' mxind=',mxind
   end if

kqt = 1
if(iteration_on .and. limit_typ == 1) kqt = 2
if(iteration_on .and. limit_typ == 2) kqt = 3

if(allocated(list2)) deallocate(list2)
allocate(list2(maKB))
list2 = 1

mfit = 0
do i=1,maKB
  if(list2(i) == 1) mfit = mfit + 1
end do

if(allocated(dfda)) deallocate(dfda,dfde)
allocate(dfda(nr))
if(mxind == 1) then
  allocate(dfde(mxind+1))
else
  allocate(dfde(mxind+1))
end if

  xred(1:maKB) = x(1:maKB)
  k0 = 0
  do i=1,maKB
    if(list(i) == 1) then
     k0 = k0 + 1
      xred(k0) = x(i)
    end if
  end do
   if(mfit < maKB) then
     do i=mfit+1,maKB
       xred(i) = zero
     end do
   end if
 list2 = 1
allocate(y(m*(mxind+1)), cy(m*(mxind+1),m*(mxind+1)), cx(nred,nred) )

mm = m             ! number of calibration points

stformat = '(2F12.7,2x,3f13.6,3x,3es14.7,3x,3es14.7)'
if(mxind == 2) stformat = '(2F12.7,2x,2f12.7,3x,2es14.7,3x,2es14.7)'
if(mxind == 1) stformat = '(2F12.7,2x,1f12.7,3x,1es14.7,3x,1es14.7)'
   ! stformat = '(2F12.7,2x,1f12.7,3x,1es14.7,3x,1es14.7)'

        ! write(kunit,*) 'mm=',mm,' mfit=',mfit

  if(klincall > 1) printout = .false.
if(irun == 1 .and. printout) then
  ! identify program to user
  WRITE(kunit,'(/,a)') ' Subroutine E7LSQ1UR demonstrates use of LSQGEN.'

  ! write table of data
  if(mxind == 3) WRITE(kunit,'(/,a)') '   S           DS            T1-3                                 DT1-3' &
                // '                                        urel(T1-3)'
  if(mxind == 2) WRITE(kunit,'(/,a)') '   S           DS            T1-2                     DT1-2' &
                // '                          urel(T1-2)'
  if(mxind == 1) WRITE(kunit,'(/,a)') '   S           DS            T1               DT1' &
                // '                          urel(T1)'

  DO  i=1,mm
    WRITE(kunit,stformat)  s(i),ds(i),t(i),dt(i)
  END DO

  write(kunit,*)

end if
!  Note: It is helpful to take care about zero-elements in the main diagonal
!  of the covariance matrix very early!

! combine dependent values s and ds, and also independent values t and dt,
! into a common vector y and a common covariance matrix cy

y = zero
cy = zero
rho = zero
! jx = mfit + 1
jx = mxind + 1

DO i=1,mm
  y(i*jx) = s(i)
  y(i*jx-1) = t(i)
  ! Variances of the "Y values" :
  cy(i*jx ,i*jx) = ds(i)**two
  do k=1,mxind
    j = i*jx - k
    ! Variances oof the "X values":
    cy(j,j) = dt(i)**two
  end do
END DO

  IF(.false. .and. (printout .or. (compare_WTLS .and. printout))) THEN
    call matwrite(cy,m*2,m*2,23,'(120es11.3)', &
        'E7: Matrix cy, here containing only variances/covariances of the Y values (net count rates):')
  end if

cymin = 1.E+30_rn
cymax = -1.E+30_rn

!Repair cases with diagonal elements very close to zero
! according to ISO GUM suppl. 2, page 6, note 4
           !! call cy_repair(cy,mm*(mxind+1), printout)

ifail = 0
cauchy_failed3 = .false.
nnew = m*(mxind+1)

  IF((printout .and. .not.iteration_on) .or. (compare_WTLS .and. printout)) THEN
    WRITE(23,'(a,120es15.7)') '   Array y: ',(y(i),i=1,nnew)
    write(23,*)
    crestrict = ''
    m1 = min(30, nnew)
    if(m1 == 30 .and. nnew > 30) write(crestrict,'(a,i0,a,i0,a)') ' (restricted to ',m1,' x ',m2,')'
    call matwrite(cy,m1,m1,23,'(50es11.3)',  &
            'E7: Matrix cy, incl. variances/covariances of the t values (decay function values):  ' &
            // trim(crestrict) )
  end if

IF(printout .or. (compare_WTLS .and. printout)) THEN
  ! header for output of results
  WRITE(kunit,'(/,a)') ' Performing fit with LSQGEN'

  WRITE(kunit,'(4(A,I3),A,3I2)') ' N = ',n,', NR = ',nr,  &
                     ', NRED = ',nred,', M = ',m,', LIST = ',list
  WRITE(kunit,'(1x,a,a1)') 'Type of derivative: ',dervtype

  WRITE(kunit,'(A,3(ES16.9,1x))') ' first approx.: Params = ',(xred(i),i=1,mfit)
  write(kunit,*) 'Chisqr_NLS=',chisqr_nls
end if

chisq = zero
cx = zero
CALL lsqgen(y,cy,m,n,nr,nred,list2,xred,cx,r,nstep,printout)
   IF(nstep == -1) THEN
     WRITE(kunit,*) ' Internal problem occurred in LSQGEN: not OK!'
     WRITE(66,*)    ' Internal problem occurred in LSQGEN: not OK!'
     GOTO 9000
   END IF
   IF(nstep == -3) THEN
     WRITE(kunit,*) ' Num. Diff. AUXDRG : not OK!'
     WRITE(66,*)    ' Num. Diff. AUXDRG : not OK!'
     GOTO 9000
   END IF
  IF(nstep == -2) THEN
     WRITE(kunit,'(/,a)') ' Fit did not converge!'
     WRITE(66,'(/,a)')    ' Fit did not converge!'
     GOTO 9000
  END IF
  if(.not.posdef) then
     if(printout) write(kunit,*) 'After LSQGEN: posdef=',posdef,'  Return'
    return
  end if
                 if(printout) write(kunit,*) 'After LSQGEN: posdef=',posdef
!  convergence successful:
IF(m-nred <= 1) THEN
  r = 1.E-17_rn
  chisq = r
  chisqr = r
else
  chisq = r
  chisqr = r/real(m-nred,rn)
end if
! Chi-Test following ISO 11929 (2010), Eq. (C.31)
if(numd > mfit) Chis_test(2) = abs( chisq - real(numd-mfit,rn) ) /sqrt(2._rn*real(numd-mfit,rn))
   !if(printout) then
   ! call matwrite(cx,nred,nred,nred,nred,23,'(50es11.3)','E7: Matrix cx: ')
   !end if

            ! write(kunit,*) 'ma=',ma,' list2=',list2
kjun = kunit
covar = zero
ir = 0
do i=1,maKB
  IF(list(i) > 1 .or. list(i) == 0) CYCLE
  ir = ir + 1
     x(i) = xred(ir)
  kr = 0
  do k=1,maKB
    IF(list(k) > 1 .or. list(k) == 0) CYCLE
    kr = kr + 1
    covar(i,k) = cx(ir,kr)
  end do
end do
IF(printout .and. nred > 0) THEN
  call matwrite(covar,maKB,maKB,kjun,'(10es14.6)',' covariance matrix CX=covar : ')
END IF

IF(.not.printout .and. .not.(compare_WTLS .and. printout)) goto 9000

! output of results
WRITE(kjun,'(/,A,es16.9,A,es16.9,A,I3,/,a,es16.9,2x,a,i5)')         &
       ' Result of fit: R=SSD = ',r,'  ChisqRed=',chisqr,  &
       '  NSTEP =',nstep,' StDev of Fit = ',SQRT(chisqr),'  m-nred=',m-nred
IF(MCSim_on) write(kjun,*) 'imc=',imc


WRITE(kjun,'(a,/,60("-"))') '  i   Param            u(Param)         covar triangle'
do i=1,nr
  ! IF(abs(xred(i)) < eps1min) CYCLE
  IF(i == 1) THEN
    WRITE(kjun,'(1x,i2,2(1x,es18.11))') i,x(i),sqrt(covar(i,i))
  else
    WRITE(kjun,'(1x,i2,10(1x,es18.11))') i,x(i),sqrt(covar(i,i)),(covar(i,k),k=1,i-1)
  END IF
end do
WRITE(kjun,'(1x)')
write(kjun,'(a,L1,a,f15.12,a,i2)') 'E7 at end:   posdef=',posdef,'  cofact=',cofact,'  ncofact=',ncofact
write(kjun,*) '-------------------------------------------------------------------------------------'
write(kjun,*)

9000  CONTINUE

if(nstep < 0) WTLS_wild = .true.

END SUBROUTINE E7lSQ1Cal

!#######################################################################
