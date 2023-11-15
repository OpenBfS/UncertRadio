
submodule (Brandt) Brandta


 use Num1,          only: matwrite

contains

!  Most of the routines ('mtx*') contained in the module Brandt are taken
!  from the textbook
!
!  Datan-Library (Fortran) from:
!  Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und Computerprogrammen;
!  4. Auflage. Spektrum, Akademischer Verlag, Heidelberg-Berlin. In German.
!  This text book is also available in an English version.
!
!  Other routines are taken from Alan Miller's or John Burkhardt's websites.


!#######################################################################

module SUBROUTINE mtxchi(a)

  ! from Datan library, modified by GK
  ! this routine inverts a (n x n) matrix by Cholesky decomposition

use UR_params,     only: rn,zero,one
USE UR_Variables,  ONLY: MCsim_ON
use UR_Linft,      only: posdef
use UR_Gleich,     only: ifehl

implicit none

real(rn), INTENT(INOUT)   :: a(:,:)

real(rn), allocatable     :: u(:,:)
real(rn)                  :: maxdev
integer(4)                :: i,k,l,n,j
LOGICAL                   :: printout , symmetric
!---------------------------------------------------------------------------------
printout = .FALSE.
   ! printout = .TRUE.

!  printout = printout .and. use_WTLS .and. .not. iteration_on

n = ubound(a,dim=1)
Allocate(u(n,n))
!---------------------------------------------------------------------------------
! Step 1: Cholesky decomposition
symmetric = .TRUE.

  posdef = .true.
             IF(printout) THEN
               !call matwrite(A,n,min(n,100),23,'(150es11.3)','MTXCHI, am Beginn: Matrix A :')
             end if
 !-------------------------------------------------------------

  CALL mtxchl(a,u)
    if(.not.posdef) then
      write(23,*) ' in MTXCHI:  after call MTXCHL: posdef=',posdef,'  n=',int(n,2)
      call matwrite(A,n,min(n,100),23,'(150es11.3)','MTXCHI, at the begin: Matrix A :')
      return
    end if
          IF(printout) THEN
            !call matwrite(A,n,min(n,100),23,'(150es15.7)','MTXCHI, after MTXCHL: matrix A:')
            !call matwrite(u,n,min(n,100),23,'(150es11.3)','MTXCHI, after MTXCHL: upper triang U:')
          end if

  DO  i=1,n
    ! Step 2: Forward Substitution
    DO  l=i,n
      IF(l == i) THEN
        a(n,l) = one/u(l,l)
      ELSE
        a(n,l) = zero
        if(l-1 >= i) a(n,l) = -sum(u(i:l-1,l)*a(n,i:l-1))
        a(n,l) = a(n,l)/u(l,l)
      END IF
    END DO
    ! Step 3: Back Substitution
    DO  l=n,i,-1
      IF(l == n) THEN
        a(i,l) = a(n,l)/u(l,l)
      ELSE
        a(i,l) = a(n,l)
        a(i,l) = a(i,l) - sum( u(l,l+1:n)*a(i,l+1:n))
        a(i,l) = a(i,l)/u(l,l)
      END IF
    END DO
  END DO

  ! Fill lower triangle symmetrically
  IF(n > 1) THEN
    DO  i=1,n
      a(i,1:i-1) = a(1:i-1,i)
    END DO
  END IF

    ! write(66,*) 'mtxchi: posdef=',posdef
           IF(printout) THEN
             ! call matwrite(A,n,min(n,100),23,'(150es11.3)','MTXCHI, at end: matrix A:')
           end if

END SUBROUTINE mtxchi

!#######################################################################

module SUBROUTINE mtxchl(a,u)   ! ,n)

  ! from Datan library, modified by GK
  ! this routine performs a Cholesky decomposition for a positive definite
  ! symmetric matrix A and returns the upper triangular matrix U.

use UR_Linft,     only: posdef,use_WTLS
use UR_params,    only: rn,zero,one
use UR_Linft,     only: posdef,ncofact,cofact,cofactlyt

implicit none

real(rn), INTENT(IN)         :: a(:,:)
real(rn), INTENT(IN OUT)     :: u(:,:)

integer(4)    :: j,k,l,i,n
real(rn)      :: s
LOGICAL       :: printout , symmetric
real(rn)      :: etiny
!-----------------------------------------------------------------------------
printout = .FALSE.
    ! printout = .TRUE.

etiny = 1.E-20_rn
!----------------------------------------------------------------------------
n = ubound(a,dim=1)

 ! The Cholesky decomposition requires a symmetric matrix!
 symmetric = .TRUE.
           if(printout) then
             ! call matwrite(A,n,n,23,'(150es11.3)','MTXCHI, Matrix A :')
             ! call matwrite(A,n,n,23,'(150es21.13)','MTXCHI, Matrix A :')
           end if

if(.false.) then
  do i=1,n
    do k=i+1,n
      ! IF(ABS(a(i,k)-a(k,i)) > 1.E-3_rn*ABS(a(i,k)) ) then
      IF(ABS(a(i,k)-a(k,i)) > 1.E-20_rn*ABS(a(i,k)) ) then
        symmetric = .FALSE.
        ! if(printout)
              write(23,*) 'asymmetric: i,k=',int(i,2),int(k,2),a(i,k),a(k,i),' diff=',a(i,k)-a(k,i)
      end if
    end do
  end do
   ! IF(printout .and. .not.symmetric)
       WRITE(23,*) '  MTXCHL:  Warning: matrix A is not symmetric! No result from mtxchl !'
end if
!-----------------------------------------------------------------------------------------
ncofact = 0
cofact = 1.0_rn
if(use_WTLS) cofact = one - 1.E-10_rn

11    continue

u = zero
posdef = .true.
DO  k=1,n
  s = zero
  DO  j=k,n
    IF(k > 1) THEN
      s = sum(u(1:k-1,k)*u(1:k-1,j))
    END IF
    u(k,j) = a(k,j) - s
       if(k /= j) u(k,j) = a(k,j)*cofact - s
    IF(k == j) THEN
      IF(abs(u(k,k)) < etiny) THEN
        ncofact = ncofact + 1
        cofact = cofact * (one - 1.E-10_rn)
        if(printout) &
              write(23,'(a,L1,2(a,i3),a,es14.7,a,es8.1)') 'MTXCHL: posdef=',posdef, &
                            '  k=',k,'  j=',j,' u(k,k)=',u(k,k),' cofact= 1-',(one-cofact)
        if(ncofact <= 4) goto 11
        posdef = .false.
        return
      end if
      u(k,j) = SQRT(ABS(u(k,j)))
    ELSE
      u(k,j) = u(k,j)/u(k,k)
    END IF
  END DO    ! j loop
end do      ! k loop
return

END SUBROUTINE mtxchl

!#######################################################################

module SUBROUTINE mtxgva(v1,v2,c,s)

 ! from Datan library, modified by GK
 ! this routine defines a Givens-transformation and applies it to
 ! directly to the defining vector.

use UR_params,     only: rn,eps1min,zero,one
implicit none

real(rn), INTENT(INOUT)   :: v1 ! vector components, defining the Givens-transformation
real(rn), INTENT(INOUT)   :: v2 !
real(rn), INTENT(OUT)     :: c    ! constants of the transformed vector
real(rn), INTENT(OUT)     :: s    !

real(rn)   :: a1,a2,w,q

a1=ABS(v1)
a2=ABS(v2)
IF(a1 > a2) THEN
  w=v2/v1
  q=SQRT(one+w*w)
  c=one/q
  IF(v1 < zero) c=-c
  s=c*w
  v1=a1*q
  v2=zero
ELSE
  IF(abs(v2) > eps1min) THEN
    w=v1/v2
    q=SQRT(one+w*w)
    s=one/q
    IF(v2 < zero) s=-s
    c=s*w
    v1=a2*q
    v2=zero
  ELSE
    c=one
    s=zero
  END IF
END IF
END SUBROUTINE mtxgva

!#######################################################################

module SUBROUTINE mtxgvd(v1,v2,c,s)

 ! from Datan library, modified by GK
 ! this routine defines a Givens-rotation.

use UR_params,     only: rn,eps1min,zero,one
implicit none

real(rn), INTENT(IN)   :: v1 ! vector components, defining the Givens-rotation
real(rn), INTENT(IN)   :: v2 !
real(rn), INTENT(OUT)  :: c    ! constants of the transformed vector
real(rn), INTENT(OUT)  :: s    !

real(rn)   :: a1,a2,w,q

a1=ABS(v1)
a2=ABS(v2)
IF(a1 > a2) THEN
  w=v2/v1
  q=SQRT(one+w*w)
  c=one/q
  IF(v1 < zero) c=-c
  s=c*w
ELSE
  IF(abs(v2) > eps1min) THEN
    w=v1/v2
    q=SQRT(one+w*w)
    s=one/q
    IF(v2 < zero) s=-s
    c=s*w
  ELSE
    c=one
    s=zero
  END IF
END IF
END SUBROUTINE mtxgvd

!#######################################################################

module SUBROUTINE mtxgvt(z1,z2,c,s)

 ! from Datan library, modified by GK
 ! this routine applies a Givens-rotation to two components z1 and z2 of a vector.

use UR_params,     only: rn
implicit none

real(rn), INTENT(IN OUT)     :: z1   ! components of a vector to be transformed
real(rn), INTENT(IN OUT)     :: z2   !
real(rn), INTENT(IN)         :: c  !  constants
real(rn), INTENT(IN)         :: s  !

real(rn)  :: w

w = z1*c + z2*s
z2 = -z1*s + z2*c
z1 = w
END SUBROUTINE mtxgvt

!#######################################################################

module SUBROUTINE mtxhsd(v,up,b,n,lp,l)

 ! from Datan library, modified by GK
 ! defines a Householder-Transformation; n, Lp, L: vector indices

use UR_params,     only: rn,zero,one,two
implicit none

integer(4), INTENT(IN)       :: n
real(rn), INTENT(IN)         :: v(n)     ! n-vector
real(rn), INTENT(OUT)        :: up
real(rn), INTENT(OUT)        :: b
integer(4), INTENT(IN)       :: lp
integer(4), INTENT(IN)       :: l

integer(4)  :: i
real(rn)    :: c,c1,sd,vpprim

c=ABS(v(lp))
DO  i=l,n
  c=MAX(ABS(v(i)),c)
END DO
IF(c <= zero) THEN
  up = zero
  b  = zero
  return
END IF
c1=one/c
sd=(v(lp)*c1)**two
DO  i=l,n
  sd=sd+(v(i)*c1)**two
END DO
vpprim=sd
vpprim=c*SQRT(ABS(vpprim))
IF(v(lp) > zero) vpprim=-vpprim
up=v(lp)-vpprim
b=one/(vpprim*up)

END SUBROUTINE mtxhsd

!#######################################################################

module SUBROUTINE mtxhst(v,up,b,c,n,lp,l)

 ! from Datan library, modified by GK
 ! Applies a Householder-Transformation to a vector c

use UR_params,     only: rn
implicit none

integer(4), INTENT(IN)       :: n
real(rn), INTENT(IN)         :: v(n)    ! v, n, Lp, L: as in mtxhsd
real(rn), INTENT(IN)         :: up      ! up and b: were calculated by mtxhsd
real(rn), INTENT(IN)         :: b
real(rn), INTENT(IN OUT)     :: c(n)    ! vector to be transformed
integer(4), INTENT(IN)       :: lp
integer(4), INTENT(IN)       :: l

integer(4)   :: i
real(rn)  :: dup,s

dup=up
s=c(lp)*dup
s = s + sum(c(l:n)*v(l:n))
s=s*b
c(lp)=c(lp)+s*dup

c(l:n)=c(l:n)+s*v(l:n)

END SUBROUTINE mtxhst

!#######################################################################

module SUBROUTINE mtxlsc(a,b,e,d,x,r,a2,frac,ok)        ! m,n,l,

 ! from Datan library, modified by GK
 ! solves the least squares problem (A*x -b)^2 = min
 ! under the constraint E*x = d

use UR_params,     only: rn

implicit none

real(rn), INTENT(INout)      :: a(:,:)     ! matrix a(m,n))
real(rn), INTENT(INout)      :: b(:)       ! vector b(m))
real(rn), INTENT(INout)      :: e(:,:)     ! matrix e(l,n)
real(rn), INTENT(IN)         :: d(:)       ! vektor d(l))
real(rn), INTENT(OUT)        :: x(:)       ! vector x(n))
real(rn), INTENT(OUT)        :: r          ! chi-square
real(rn), INTENT(INout)      :: a2(:,:)    ! working array a2(m,n-l)
real(rn), INTENT(IN)         :: frac       ! fraction f
LOGICAL, INTENT(OUT)         :: ok

real(rn)    :: rrr(20)
integer(4)  :: m,n,l
! real(rn)    :: up(l),bb(l), p2(n),s(n),v(n),bout(n,1)
real(rn),allocatable :: up(:),bb(:), p2(:,:),s(:),v(:),bout(:,:),b3(:,:)
LOGICAL     :: printout

integer(4)   :: i,j,k,l2,nminl
!-----------------------------------------------------------------------
m = ubound(a,dim=1)
n = ubound(a,dim=2)
l = ubound(d,dim=1)
allocate(up(l),bb(l), p2(n,1),s(n),v(n),bout(n,1),b3(m,1))

printout = .FALSE.
  ! printout = .TRUE.

  IF(printout) THEN
    WRITE(23,*) 'MTXLSC: m, n, l = ',m,n,l
    call matwrite(a,m,n,23,'(130(es10.2,1x))','MTXLSC: Matrix A:')
    WRITE(23,*) 'MTXLSC, at beginning: Vektor B:'
    WRITE(23,'(130(es10.2,1x))') (b(i),i=1,m)
    WRITE(23,*)
    WRITE(23,*) 'MTXLSC, at beginning: Vektor D:'
    WRITE(23,'(130(es10.2,1x))') (d(i),i=1,l)
    call matwrite(e,l,n,23,'(130(es10.2,1x))','MTXLSC: Matrix e:')
  end if

x = 0.0_rn

! step 1:
nminl=n-l
DO  i=1,l
  v(1:n) = e(i,1:n)
  CALL mtxhsd(v,up(i),bb(i),n,i,i+1)
  DO  j=i,l
    s(1:n) = e(j,1:n)
    CALL mtxhst(v,up(i),bb(i),s,n,i,i+1)
    IF(j == i .AND. n > i) THEN
      s(i+1:n) = v(i+1:n)     !xx
    END IF
    e(j,1:n) = s(1:n)
  END DO
  DO  j=1,m
    s(1:n) = a(j,1:n)
    CALL mtxhst(v,up(i),bb(i),s,n,i,i+1)
    a(j,1:n) = s(1:n)
  END DO
END DO
! step 2:
x(1)=d(1)/e(1,1)
IF(l > 1) THEN
  DO  j=2,l
    x(j)=d(j)
    x(j) = x(j) - sum( e(j,1:j-1)*x(1:j-1) )
    x(j)=x(j)/e(j,j)
  END DO
END IF
! step 3:
DO  j=1,m
  b(j) = b(j) - sum( a(j,1:l)*x(1:l) )
END DO
! step 4:
l2=1
  IF(printout) THEN
    call matwrite(a,m,n,23,'(130(es10.2,1x))','MTXLSC, before call mtxgsm: Matrix A:')
  end if
         ! write(0,'(a,4i5)') ' m,n,nminl,l=',m,n,nminl,l,'  Ubound(a2,1)=',Ubound(a2,dim=1)
  a2(1:m, 1:nminl) = a(1:1-1+m , l+1:l+nminl)

  IF(printout) THEN
    call matwrite(a2,m,n-l,23,'(130(es10.2,1x))','MTXLSC, before call mtxsvd: Matrix A2:')
    WRITE(23,*) 'MTXLSC, before call MTXSVD: Vektor B:'
    WRITE(23,'(130(es10.2,1x))') (b(i),i=1,m)
    WRITE(23,*)
  end if

b3(1:m,1) = b(1:m)
CALL mtxsvd(a2,b3,p2,rrr,frac,ok,bout)
r = rrr(1)
b(1:m) = b3(1:m,1)

IF(ok) THEN
  x(L+1:L+nminl) = p2(1:nminl,1)
  DO  i=l,1,-1
    v(1:n) = e(i,1:n)
    CALL mtxhst(v,up(i),bb(i),x,n,i,i+1)
  END DO
END IF
END SUBROUTINE mtxlsc

!#######################################################################

module SUBROUTINE mtxsv1(a,b,d,e)

 ! from Datan library, modified by GK
 ! performs the bi-diagonalization of an (m x n) matrix A, such that
 !  A = Q C transpose(H), with C bi-diagonal

use UR_params,     only: rn,zero,one
implicit none

real(rn), INTENT(IN OUT)    :: a(:,:)      ! matrix a(m,n))
real(rn), INTENT(IN OUT)    :: b(:,:)      ! matrix b(m,nb)
real(rn), INTENT(OUT)       :: d(:)        ! vector d(n)
real(rn), INTENT(OUT)       :: e(:)        ! vector e(n)

integer(4)           :: m,n,nb
real(rn),allocatable :: v(:),s(:),ups(:),bbs(:)
integer(4)           :: i,j,k
real(rn)             :: bb,up

m = ubound(a,dim=1)
n = ubound(a,dim=2)
nb = ubound(b,dim=2)

allocate(v(m),s(m),ups(n),bbs(n))
    ! call matwrite(A,m,n,m,n,66,'(100(es11.3,1x))',' SV1-in: Matrix A')
DO  i=1,n
  ! set up Householder Transformation Q(I)
  IF(i < n .OR. m > n) THEN
    v(1:m) = a(1:m,i)
    CALL mtxhsd(v,up,bb,m,i,i+1)
    ! apply Q(I) to A
    DO  j=i,n
      s(1:m) = a(1:m,j)
      CALL mtxhst(v,up,bb,s,m,i,i+1)
      a(1:m,j) = s(1:m)
    END DO
    ! apply Q(I) to B
    DO  k=1,nb
      s(1:m) = b(1:m,k)
      CALL mtxhst(v,up,bb,s,m,i,i+1)
      b(1:m,k) = s(1:m)
    END DO
  END IF
  IF(i < n-1) THEN
    ! set up Householder Transformation H(I)
    v(1:n) = a(i,1:n)
    CALL mtxhsd(v,up,bb,n,i+1,i+2)
    ! save H(I)
    ups(i) = up
    bbs(i) = bb
    ! apply H(I) to A
    DO  j=i,m
      s(1:n) = a(j,1:n)
      CALL mtxhst(v,up,bb,s,n,i+1,i+2)
      ! save elements I+2,... in row J of matrix A
      IF (j == i) THEN
        s(i+2:n) = v(i+2:n)
      END IF
      a(j,1:n) = s(1:n)
    END DO
  END IF
END DO
! copy diagonal of transformed matrix A to D
! and upper parallel A to E
IF(n > 1) THEN
  DO  i=2,n
    d(i) = a(i,i)
    e(i) = a(i-1,i)
  END DO
END IF
d(1)=a(1,1)
e(1)=zero
! construct product matrix H=H(2)*H(3)*...*H(N), H(N)=I
DO  i=n,1,-1
  IF(i <= n-1) v(1:n) = a(i,1:n)
  a(i,1:n) = zero
  a(i,i)=one
  IF(i < n-1) THEN
    DO  k=i,n
      s(1:m) = a(1:m,k)
      CALL mtxhst(v,ups(i),bbs(i),s,n,i+1,i+2)
      a(1:m,k) = s(1:m)
    END DO
  END IF
END DO
END SUBROUTINE mtxsv1

!#######################################################################

module SUBROUTINE mtxsv2(a,b,d,e,ok)

 ! from Datan library, modified by GK
 ! it diagonalizes iteratively the bi-diagonal matrix C, such that
 ! S' = transpose(U') C V'  is diagonal
 ! modification (GK): goto eliminated

use UR_params,     only: rn,zero,one,eps1min
implicit none

real(rn), INTENT(IN OUT)    :: a(:,:)        ! matrix a(m,n)
real(rn), INTENT(IN OUT)    :: b(:,:)        ! matrix b(m,nb)
real(rn), INTENT(IN OUT)    :: d(:)          ! vector d(n)
real(rn), INTENT(IN OUT)    :: e(:)          ! vector e(n)
LOGICAL, INTENT(OUT)        :: ok

LOGICAL             :: elzero
integer(4)          :: niterm,niter,i,k,ll,j,l, m,n,nb
real(rn)            :: bmx

m = ubound(a,dim=1)
n = ubound(a,dim=2)
nb = ubound(b,dim=2)

ok=.true.
niterm=10*n
    niterm = 6*n
niter=0
bmx=d(1)
IF(n > 1) THEN
  DO i=2,n
    bmx=MAX(ABS(d(i))+ABS(e(i)),bmx)
  END DO
END IF
DO k=n,1,-1
  do
    IF(k /= 1) THEN
      IF(abs((bmx+d(k))-bmx) < eps1min) THEN
        ! WRITE(166,*) 'MTXSV2 (a): Element D(',k,') = 0._rn'
        ! Since D(K).EQ.0. perform Givens transform with result E(K)=0.
        CALL mtxs21(a,d,e,k)
      END IF
      ! Find L (2. LE. L .LE. K) so that either E(L)=0. or D(L-1)=0.
      ! In the latter case transform E(L) to zero. In both cases the
      ! matrix splits and the bottom right minor begins with row L.
      ! If no such L is found set L=1
      DO  ll=k,1,-1
        l=ll
        IF(l == 1) THEN
          elzero=.false.
          EXIT
        ELSE IF( abs( (bmx-e(l))-bmx) < eps1min) THEN
          elzero=.true.
          EXIT
        ELSE IF(abs( (bmx+d(l-1))-bmx) < eps1min) THEN
          elzero=.false.
        END IF
      END DO
      IF (l > 1 .AND. .NOT.elzero) THEN
        CALL mtxs22(b,d,e,k,l)
      END IF
      IF(l /= k) THEN
        ! one more QR pass with order K
        CALL mtxs23(a,b,d,e,k,l)
        niter=niter+1
        if(niter <= niterm) cycle

        ! set flag indicating non-convergence
        ok=.false.
        IF(.not.ok) THEN
          ! WRITE(166,*) 'MTXSV2 (d): ok=.F.  !   niter=',niter,'  niterm=',niterm
          ! WRITE(166,*) '     m,n,nb=',m,n,nb
        end if
      END IF
    END IF

    IF(d(k) < zero) THEN
      ! for negative singular values perform change of sign
      d(k)=-d(k)
      a(1:n,k) = -a(1:n,k)
    END IF
    exit
  end do
 ! order is decreased by one in next pass
END DO
END SUBROUTINE mtxsv2
!#######################################################################

module SUBROUTINE mtxs21(a,d,e,k)

 ! from Datan library, modified by GK
 ! subprogram of mtxsv2

use UR_params,     only: rn,zero
implicit none

real(rn), INTENT(IN OUT)    :: a(:,:)      ! matrix a(m,n)
real(rn), INTENT(IN OUT)    :: d(:)        ! vector d(n)
real(rn), INTENT(IN OUT)    :: e(:)        ! vector e(n)
integer(4), INTENT(IN)      :: k

integer(4)    :: i,j,m,n
real(rn)      :: sn,cs,h

m = ubound(a,dim=1)
n = ubound(a,dim=2)

DO  i = k-1, 1, -1
  IF(i == k-1) THEN
    CALL mtxgva(d(i),e(i+1),cs,sn)
  ELSE
    CALL mtxgva(d(i),h,cs,sn)
  END IF
  IF(i > 1) THEN
    h=zero
    CALL mtxgvt(e(i),h,cs,sn)
  END IF
  DO  j =  1,n
    CALL mtxgvt(a(j,i),a(j,k),cs,sn)
  END DO
END DO
END SUBROUTINE mtxs21

!#######################################################################

module SUBROUTINE mtxs22(b,d,e,k,l)

 ! from Datan library, modified by GK
 ! subprogram of mtxsv2

use UR_params,     only: rn,zero
implicit none

real(rn), INTENT(IN OUT)    :: b(:,:)      ! matrix b(m,nb)
real(rn), INTENT(IN OUT)    :: d(:)        ! vector d(n)
real(rn), INTENT(IN OUT)    :: e(:)        ! vector e(n)
integer(4), INTENT(IN)      :: k
integer(4), INTENT(IN)      :: l

integer(4)  :: i,j, m,nb,n
real(rn)    :: sn,cs,h

m = ubound(b,dim=1)
nb = ubound(b,dim=2)
n = ubound(d,dim=1)

DO  i=l,k
  IF(i == l) THEN
    CALL mtxgva(d(i),e(i),cs,sn)
  ELSE
    CALL mtxgva(d(i),h,cs,sn)
  END IF
  IF(i < k) THEN
    h=zero
    CALL mtxgvt(e(i+1),h,cs,sn)
  END IF
  DO  j=1,nb
    CALL mtxgvt(cs,sn,b(i,j),b(l-1,j))
  END DO
END DO
END SUBROUTINE mtxs22

!#######################################################################

module SUBROUTINE mtxs23(a,b,d,e,k,l)
   ! from Datan library, modified by GK
   ! subprogram of mtxsv2

use UR_params,     only: rn,one,zero,two
implicit none

real(rn), INTENT(IN OUT)    :: a(:,:)     ! matrix a(m,n)
real(rn), INTENT(IN OUT)    :: b(:,:)     ! matrix b(m,nb)
real(rn), INTENT(IN OUT)    :: d(:)       ! vector d(n)
real(rn), INTENT(IN OUT)    :: e(:)       ! vector e(n)
integer(4), INTENT(IN)      :: k
integer(4), INTENT(IN)      :: l

integer(4)           :: i,j, m,n,nb
real(rn)             :: f,g,t,sn,cs,h

m = ubound(a,dim=1)
n = ubound(a,dim=2)
nb = ubound(b,dim=2)

! Determine shift parameter
f=((d(k-1)-d(k))*(d(k-1)+d(k))+(e(k-1)-e(k))*(e(k-1)+e(k)))/  &
                                             (two*e(k)*d(k-1))
IF(ABS(f) > 1.E+10_rn) THEN
  g=ABS(f)
ELSE
  g=SQRT(one+f*f)
END IF
IF(f >= zero) THEN
  t=f+g
ELSE
  t=f-g
END IF
f=((d(l)-d(k))*(d(l)+d(k))+e(k)*(d(k-1)/t-e(k)))/d(l)
DO  i = l , k-1
  IF(i == l) THEN
    ! Define R(L)
    CALL mtxgvd(f,e(i+1),cs,sn)
  ELSE
    ! Define R(I) , I.NE.L
    CALL mtxgva(e(i),h,cs,sn)
  END IF
  CALL mtxgvt(d(i),e(i+1),cs,sn)
  h=zero
  CALL mtxgvt(h,d(i+1),cs,sn)
  DO  j =  1, n
    CALL mtxgvt(a(j,i),a(j,i+1),cs,sn)
  END DO
  ! Define T(I)
  CALL mtxgva(d(i),h,cs,sn)
  CALL mtxgvt(e(i+1),d(i+1),cs,sn)
  IF(i < k-1) THEN
    h=zero
    CALL mtxgvt(h,e(i+2),cs,sn)
  END IF
  DO  j =  1, nb
    CALL mtxgvt(b(i,j),b(i+1,j),cs,sn)
  END DO
END DO
END SUBROUTINE mtxs23

!#######################################################################

module SUBROUTINE mtxsv3(a,b,d)

 ! from Datan library, modified by GK
 ! performs a permutational transformation of the diagonal matrix S',
 ! such that S'' = transpoes(U'') S' V'' has not-ascendingly ordered
 ! diagonal elements
 ! modification (GK): goto eliminated

use UR_params,     only: rn
implicit none

real(rn), INTENT(IN OUT)    :: a(:,:)      ! matrix a(m,n)
real(rn), INTENT(IN OUT)    :: b(:,:)      ! matrix b(m,nb)
real(rn), INTENT(IN OUT)    :: d(:)        ! vector d(n)

integer(4)  :: i,j,k, nfd, m,n,nb
real(rn)    :: t

m = ubound(a,dim=1)
n = ubound(a,dim=2)
nb = ubound(b,dim=2)

! Order singular values
if(n <= 1) return
do
  nfd = 0
  DO  i=2,n
    IF(d(i) > d(i-1)) then
      nfd = 1
      exit
    end if
  end do
  if(nfd == 0) return

  DO  i =2,n
    t=d(i-1)
    k=i-1
    DO  j = i , n
      IF(t < d(j)) THEN
        t=d(j)
        k=j
      END IF
    END DO
    IF(k /= i-1) THEN
      ! perform permutation on singular values
      d(k)=d(i-1)
      d(i-1)=t
      ! perform permutation on matrix A
      DO  j =  1, n
        t=a(j,k)
        a(j,k)=a(j,i-1)
        a(j,i-1)=t
      END DO
      ! perform permutation on matrix B
      DO  j =  1, nb
        t=b(k,j)
        b(k,j)=b(i-1,j)
        b(i-1,j)=t
      END DO
    END IF
  END DO
END DO
END SUBROUTINE mtxsv3

!#######################################################################

module SUBROUTINE mtxsv4(a,b,d,x,r,frac,bout)

 ! from Datan library, modified by GK
 ! performs the singular value analysis

use UR_params,     only: rn,eps1min,zero,one,two
use UR_Linft,      only: ycopy,use_PMLE,uycopy,penalty_factor,use_constr,pcstr,upcstr,kconstr
implicit none

real(rn), INTENT(IN)        :: a(:,:)      ! matrix a(m,n)
real(rn), INTENT(INOUT)     :: b(:,:)      ! matrix b(m,nb)
real(rn), INTENT(IN)        :: d(:)        ! vector d(n)
real(rn), INTENT(OUT)       :: x(:,:)      ! matrix x(n,nb)
real(rn), INTENT(OUT)       :: r(:)        ! vector (nb) of squares of the residuals for columns of the Matrix A
real(rn), INTENT(IN)        :: frac
real(rn), intent(out)       :: bout(:,:)   ! matrix bout(m,nb)

! real(rn), PARAMETER :: epsiln=1.E-15_rn
real(rn), PARAMETER :: epsiln=5._rn*eps1min

integer(4)  :: i,j,k,kk, m,n,nb
real(rn)    :: fract,sinmax,sinmin,s1,yfi

m = ubound(a,dim=1)
n = ubound(a,dim=2)
nb = ubound(b,dim=2)

r = zero

fract=ABS(frac)
IF(fract < epsiln) fract=epsiln
sinmax=zero
sinmax = max(sinmax, maxval(d))
sinmin=sinmax*fract
kk=n
DO  i=1,n
  IF(d(i) <= sinmin) THEN
    kk = i-1
    EXIT
  END IF
END DO
DO  i = 1, m
  IF(i <= kk) THEN
    s1 = one/d(i)
    b(i,1:nb) = b(i,1:nb) *s1
  ELSE
    DO  j=1,nb
      if(.not.use_PMLE) then
        IF(i == kk+1) THEN
          r(j) = b(i,j)**two
        ELSE
          r(j) = r(j) + b(i,j)**two
        END IF
      else
        yfi = b(i,j)*uycopy(i) + ycopy(i)      ! function value
        IF(i == kk+1) THEN
          r(j) = 2._rn*( b(i,j)*uycopy(i) + ycopy(i)*log(yfi/max(ycopy(i),0.5_rn)) )
        else
          r(j) = r(j) + 2._rn*( b(i,j)*uycopy(i) + ycopy(i)*log(yfi/max(ycopy(i),0.5_rn)) )
        end if
      end if
      IF(i <= n) b(i,j) = zero
    END DO
    if(use_PMLE .and. use_constr) then
      do k=1,n
        if(kconstr(j) == 1) then
          r(1) = r(1) + penalty_factor*( (x(k,nb) - pcstr(k))/upcstr(k) )**two
        end if
      end do
    end if

  END IF
END DO
DO  i=1,n
  DO  j=1,nb
    x(i,j)= sum(a(i,1:n)*b(1:n,j))
  END DO
END DO
do i=1,m
  bout(i,1:nb) = b(i,1:nb)
end do
n=kk

    ! IF(.not.iteration_on .AND. .not.MCsim_ON) THEN
    !   do i=1,nb
    !     WRITE(123,*) 'mtxsv4:  (nb=',nb,');  i=',i,' r(i)=',r(i)
    !   end do
    ! end if

   !write(23,*) 'mtxsv4:  m=',m,' n=',n,' nb=',nb,' fract=',sngl(fract),  &
   !            '  R=',sngl(r(1)),' sinmax=',sngl(sinmax)

END SUBROUTINE mtxsv4

!#######################################################################

module SUBROUTINE mtxsvd(a,b,x,r,frac,ok,bout)

 ! from Datan library, modified by GK
 ! performs a singular value decomposition and - analysis of an (m x n) mtarix a

use UR_params,     only: rn,zero
USE UR_DLIM,       ONLY: iteration_on
USE UR_Variables,  ONLY: MCsim_ON

implicit none

real(rn), INTENT(IN OUT)    :: a(:,:)      ! matrix a(m,n)
real(rn), INTENT(IN OUT)    :: b(:,:)      ! matrix b(m,nb)
real(rn), INTENT(IN OUT)    :: x(:,:)      ! matrix x(n,nb)
real(rn), INTENT(IN OUT)    :: r(:)        ! vector r(nb)
real(rn), INTENT(IN)        :: frac
LOGICAL, INTENT(OUT)        :: ok
real(rn),intent(out)        :: bout(:,:)   ! matrix bout(m,nb)

integer(4)           :: i, m,n,nb
real(rn)             :: chi2
real(rn),allocatable :: d(:),e(:),bin4(:,:)
real(rn),allocatable :: Cmat(:,:),Hmat(:,:)
logical              :: test

m = ubound(a,dim=1)
n = ubound(a,dim=2)
nb = ubound(b,dim=2)

test = .false.
   ! test = .true.
allocate(d(n),e(n),bin4(m,nb))
if(test) then
  allocate(Cmat(n,n),Hmat(n,n))
  Cmat = zero;   Hmat = zero
end if

   IF(.not.iteration_on .AND. .not.MCsim_ON) THEN
   end if

! STEP 1: Bidiagonalisation of A
CALL mtxsv1(a,b,d,e)
   if(test) then
     do i=1,n
       Cmat(i,i) = d(i)
       if(i < n) Cmat(i,i+1) = e(i+1)
     end do
     call matwrite(Cmat,n,n,66,'(150es11.3)','mtxsvd, step 1:  bidiagonal matrix Cmat:')
     call matwrite(A,n,n,66,'(150es11.3)','mtxsvd, step 1:  matrix Hmat (=A):')
   end if

! STEP 2: Diagonalisation of bidiagonal matrix
CALL mtxsv2(a,b,d,e,ok)
   if(ok .and. test) then
     Cmat = zero
     do i=1,n
       Cmat(i,i) = d(i)
     end do
     call matwrite(Cmat,n,n,66,'(150es11.3)','mtxsvd, step 2  bidiagonal matrix Cmat:')
     call matwrite(A,n,n,66,'(150es11.3)',"mtxsvd, step 2:  matrix Hmat x V' (=A):")
   end if

! STEP 3: Order singular values and perform  permutations
CALL mtxsv3(a,b,d)
   if(ok .and. test) then
     write(66,'(a,10(es12.5,1x))') "mtxsvd, step 3: Vector d of diagonal elements of S'':",d(1:n)
   end if

! STEP 4: Singular value analysis
   if(ok .and. test) then
     bin4(1:m,1) = b(1:m,1)
   end if
CALL mtxsv4(a,b,d,x,r,frac,bout)
   if(ok .and. test) then
     write(66,'(a,10(es12.5,1x))') "mtxsvd, step 4: Vector d of diagonal elements of S'':",d(1:n)
     call matwrite(x,n,nb,66,'(150es11.3)','mtxsvd, step 4:  solution matrix x:')
     write(66,'(a,10(es12.5,1x))') "mtxsvd, step 4: Vector r of squared residuals :",r(1:nb)
     write(66,*) 'mtxsvd:  step 4: Vectors bin4 and bout:'
     write(66,*) 'bin4        bout'
       chi2 = 0._rn
     do i=1,m
       write(66,'(2(es14.4,2x))') bin4(i,1),bout(i,1)
       if(i == n+1) chi2 = chi2 + bin4(i,1)**2._rn
       if(i > n+1) chi2 = chi2 + bin4(i,1)**2._rn
     end do
     write(66,*) 'sum (bin4(n+1:m)^2 = chi2=',sngl(chi2)
   end if

END SUBROUTINE mtxsvd

!##############################################################################

module SUBROUTINE lsqmar(userfn,t,y,deltay,n,nall,list,xall,cxall,r,a,scrat,nstep)
       ! From Datan library, modified by GK
       ! performs a non-linear fit to a function of unknown parameters using
       ! a Marquardt-method based on singular value decomposition.
       ! Extended (GK) for applying also "Poisson maximum likelihood estimation",
       ! if the fitting function represents low count numbers of gamma of alpha spectrum

use UR_params,    only: rn, zero, one,two
use UR_Linft,     only: mfit,indfix,xfix,kpt, use_constr,kconstr,pcstr,upcstr, &
                  penalty_factor,use_PMLE,use_PLSQ,ycopy,uycopy,use_WLS
implicit none

real(rn), EXTERNAL :: userfn

real(rn)                               :: userfn
real(rn), allocatable,intent(in)       :: t(:)        ! Werte der kontrollierten Variablen
real(rn), allocatable,INTENT(IN)       :: y(:)        ! Messwerete
real(rn), allocatable,INTENT(IN)       :: deltay(:)   ! Unsicherheiten
integer(4), INTENT(IN)                 :: n           ! number of measured values
integer(4), INTENT(IN)                 :: nall        ! Anzahl der ncht-fixierten Unbekannten
integer(4), allocatable,INTENT(INOUT)  :: list(:)     ! welche Unbekannte werden / werden nicht gefittet?
real(rn), allocatable,INTENT(INOUT)    :: xall(:)     ! Näherungswerte (in) bzw. gefittete Werte d. Unbekanten
real(rn), allocatable,INTENT(OUT)      :: cxall(:,:)  ! Kovarinazmatrix der gefitteten Werte
real(rn), INTENT(out)                  :: r           ! Chi-quadrat
real(rn), allocatable,INTENT(IN OUT)   :: a(:,:)      ! Arbeitsspeicher
real(rn), allocatable,INTENT(IN OUT)   :: scrat(:,:)  ! Arbeitsspeicher
integer(4), INTENT(IN OUT)             :: nstep       ! Schrittzahl (in), bzw. Fehlerindikator, wenn < 0

real(rn), allocatable      :: x(:)        ! Näherungswerte der zui fittenden Parameter

integer(4)         :: i,k,istep,nout,mma,nna,j,nred,nr,mfix,irunPLSQ
real(rn)           :: r1,r2,rmin,frac,hp1,hp2,hp1N,hp2N
real(rn),allocatable   :: c(:,:),x1(:),x2(:),x1red(:,:),x2red(:,:),rr(:),cx(:,:),bout(:,:)

integer(4), PARAMETER    :: maxstp = 100
real(rn), PARAMETER      :: epsiln = 1.E-8_rn
real(rn), PARAMETER      :: tt = 1.E-10_rn      ! 1.E-15_rn
real(rn), PARAMETER      :: alams = 1.E-3_rn
LOGICAL                  :: NEW,ok,final,covmat,use_PMLE
real(rn)                 :: alam
real(rn)                 :: wmax,wmin

if(minval(list) == 0) then
  call fixprep(xall,nall,list,nred,x)
else
  nred = nall
  allocate(x(nred))
  x(1:nall) = xall(1:nall)
  if(allocated(kpt)) deallocate(kpt)
  allocate(kpt(nall))
  kpt(1:nall) = [ (i,i=1,nall) ]
end if

! if(.not.allocated(a)) allocate(a(nred,nred))
if(allocated(a)) deallocate(a)
if(allocated(cx)) deallocate(cx)
allocate(a(n,nred))
allocate(cx(nred,nred))
nr = nred

if(allocated(c)) deallocate(c)
if(allocated(x1)) deallocate(x1)
if(allocated(x2)) deallocate(x2)
if(allocated(x1red)) deallocate(x1red)
if(allocated(x2red)) deallocate(x2red)
if(allocated(rr)) deallocate(rr)
if(allocated(bout)) deallocate(bout)
allocate(c(n,1),x1(nr),x2(nr),x1red(nred,1),x2red(nred,1),rr(1),bout(n,1))

if(allocated(ycopy)) deallocate(ycopy);  allocate(ycopy(n))
if(allocated(uycopy)) deallocate(uycopy);  allocate(uycopy(n))
ycopy(1:n) = y(1:n)
uycopy(1:n) = deltay(1:n)

!use_PMLE = .false.
!   use_PMLE = .true.

! Extension of this routine by the introduction of an option
!     use_PMLE = T
! for applying Poisson MLE (PMLE) instead of Levenberg-Marquardt (use_PMLE = F).
! This is achieved by a different transformation of the matrix A of partial
! derivatives and of the vector C, which refers to measured input data.
! Günter Kanisch, January 2023

covmat = .true.
IF(nstep < 0) THEN
  covmat = .false.
  nstep = ABS(nstep)
END IF
IF(nstep < 1) nstep = maxstp
if(use_PLSQ) irunPLSQ = 1

40    continue

nstep = maxstp
alam = alams
! Initial value of minimum function
r = zero
DO  i=1,n
  hp1 = userfn(x,nr,t(i))
  if(use_WLS) then
    r = r + ( (y(i) - hp1)/deltay(i) )**two
  elseif(use_PMLE) then
    r = r + two*( (hp1- y(i)) - y(i)*log(hp1/max(y(i),0.5_rn) ) )
  elseif(use_PLSQ) then
    r = r + ( (y(i) - hp1)/sqrt(hp1) )**two
  end if
END DO
IF(nred <= 0) THEN
  nstep = 1
  return
END IF
! For NR = NRED :  set LIST
IF(nr == nred) THEN
  list(1:nr) = 1
END IF
! start iteration
final = .false.
DO  istep=1,nstep
  ! Numerical Derivatives
  CALL auxdri(userfn,x,t,n,nr,nred,list,a,ok)

                 ! call matwrite(a,25,nr,23,'(130es11.3)','Lsqmar, Matrix A :')
                  !write(0,*) 'nach auxdri: UB(A,1,2)=',ubound(a,dim=1),ubound(a,dim=2)
  IF(.NOT.ok) THEN
    nstep = -3
    return
  END IF
  DO  i=1,n
    hp1 = userfn(x,nr,t(i))
    if(use_WLS) then
      a(i,1:nred) = a(i,1:nred) / deltay(i)
      c(i,1) = (y(i) - hp1) / deltay(i)
    elseif(use_PMLE) then
      a(i,1:nred) = a(i,1:nred) * sqrt(max(y(i), 0.5_rn))/hp1
      c(i,1) = (y(i) - hp1) / sqrt(max(y(i),0.5_rn))
    elseif(use_PLSQ) then
      a(i,1:nred) = a(i,1:nred) / sqrt(hp1)
      c(i,1) = (y(i) - hp1) / sqrt(hp1)
    end if
  END DO

  IF(final) THEN
    ! Final Step
    frac = zero
    rr(1) = r
    call mtxsvd(a,c,x1red,rr,frac,ok,bout)
    r = rr(1)
    IF(.NOT.ok) THEN
      nstep = -1
      return
    END IF
    x1 = zero
    CALL mtxpsv(x1,x1red,nr,nred,list)
    x(1:nred) = x(1:nred) + x1(1:nred)

    if(use_PMLE) then
      r = zero
      DO  i=1,n
        hp1 = userfn(x,nr,t(i))
        r = r + two*( (hp1- y(i)) - y(i)*log(hp1/max(y(i),0.5_rn) ) )
      END DO
      if(use_constr) then
        do j=1,nr
          if(kconstr(j) == 1) then
            r = r + penalty_factor*( (x(j) - pcstr(j))/upcstr(j) )**two
          end if
        end do
      end if
    end if

    if(use_PLSQ .and. irunPLSQ < 3) then
      irunPLSQ = irunPLSQ + 1
      goto 40
    end if

    ! Compute covariance matrix CX of unknowns
    IF(covmat) THEN
      CALL auxdri(userfn,x,t,n,nr,nred,list,a,ok)
      IF(.NOT.ok) THEN
        nstep = -3
        return
      END IF
      DO  i=1,n
        hp1 = userfn(x,nr,t(i))
        if(use_WLS) then
          a(i,1:nred) = a(i,1:nred)/deltay(i)
        elseif(use_PLSQ) then
          a(i,1:nred) = a(i,1:nred)/sqrt(hp1)
        elseif(use_PMLE) then
          a(i,1:nred) = a(i,1:nred)*sqrt(max(y(i), 0.5_rn))/userfn(x,nr,t(i))
        end if
      END DO
      cx = matmul(transpose(a),a)
        ! call matwrite(cx,nred,nred,66,'(10(es13.5))',' Matrix cx before inversion:')
      CALL mtxchi(cx)
        ! call matwrite(cx,nred,nred,66,'(10(es13.5))',' Matrix cx after inversion:')
    END IF

    ! back-sort the result vector and its covar matrix
    ! if(minval(list) == 0) then
    if(mfix == 0) then
      call backsort(x,cx,nred, xall,cxall)
    else
      xall(1:nall) = x(1:nall)
      cxall = cx
    end if
    nstep = istep
    return
  END IF

  ! compute minimum function for two values of lambda (ALAM)
  frac = zero
  CALL mtxmar(a,c,alam,x1red,x2red,nout,frac,ok)
      ! write(0,*) 'nach mtxmar: ok=',ok,' istep=',int(istep,2)
      if(nout /= nr) write(66,'(a,2(i0,1x))') 'mtxmar: nout /= nr: nout,nr=',nout,nr

  IF(.NOT.ok) THEN
    nstep = -1
    return
  END IF
  x1(1:nr) = zero
  x2(1:nr) = zero
  CALL mtxpsv(x1,x1red,nr,nred,list)
  CALL mtxpsv(x2,x2red,nr,nred,list)

  x1(1:nr) = x(1:nr) + x1(1:nr)
  x2(1:nr) = x(1:nr) + x2(1:nr)
  r1 = zero
  r2 = zero
  DO  i=1,n
    hp1 = userfn(x1,nr,t(i))
    hp2 = userfn(x2,nr,t(i))
    if(use_WLS) then
      r1 = r1 + ( (y(i) - hp1)/deltay(i) )**two
      r2 = r2 + ( (y(i) - hp2)/deltay(i) )**two
    elseif(use_PMLE) then
      r1 = r1 + two*( (hp1 - y(i)) - y(i)*log(hp1/max(y(i),0.5_rn) ) )
      r2 = r2 + two*( (hp2 - y(i)) - y(i)*log(hp2/max(y(i),0.5_rn) ) )
    elseif(use_PLSQ) then
      r1 = r1 + ( (y(i) - hp1)/sqrt(hp1) )**two
      r2 = r2 + ( (y(i) - hp2)/sqrt(hp2) )**two
    end if
  END DO
  if(use_PMLE .and. use_constr) then
    do j=1,nr
      if(kconstr(j) == 1) then
        r1 = r1 + penalty_factor*( (x1(j) - pcstr(j))/upcstr(j) )**two
        r2 = r2 + penalty_factor*( (x2(j) - pcstr(j))/upcstr(j) )**two
      end if
    end do
  end if

  IF(.NOT.ok) THEN
    nstep = -1
    return
  END IF
  ! evaluate results
  IF(r2 <= r + tt) THEN
    ! reduce lambda and accept new point
    alam = alam*0.1_rn
    x(1:nr) = x2(1:nr)
    rmin = r2
    NEW = .true.
  ELSE IF (r2 > r+tt .AND. r1 <= r+tt) THEN
    ! keep current value of lambda and accept new point
    x(1:nr) = x1(1:nr)
    rmin = r1
    NEW = .true.
  ELSE
    ! increase lambda and reject new point
    alam = alam*10._rn
    NEW = .false.
  END IF
  IF(NEW) THEN
    ! test for break-off criterion
    IF(ABS(r-rmin) < epsiln*ABS(rmin)+tt) THEN
      final = .true.
    ELSE
      r = rmin
    END IF
  END IF
END DO
nstep = -2
100   continue

RETURN
END SUBROUTINE lsqmar

!#######################################################################

module subroutine fixprep(xall,nall,list,nred,x)         ! ,mfix,indfix,xfix)
use UR_params,    only: rn,two,zero,pi
use UR_Linft,     only: mfix,indfix,xfix,kpt
implicit none

integer(4),intent(in)              :: nall        ! number of all parameters
real(rn),intent(in)                :: xall(nall)  ! values of all parameters
integer(4), intent(in)             :: list(nall)  !
integer(4), intent(out)            :: nred        ! number of non-fixed parameters
real(rn),allocatable,intent(out)   :: x(:)        ! values of non-fixed parameters

integer(4)         :: i,n,mm

mfix = 0
nred = 0
do i=1,nall
  if(list(i) == 1) nred = nred + 1
end do
mfix = nall - nred
if(allocated(indfix)) deallocate(indfix)
if(allocated(xfix)) deallocate(xfix)
if(allocated(x)) deallocate(x)
if(allocated(kpt)) deallocate(kpt)
allocate(x(nred),kpt(nred),indfix(mfix),xfix(mfix))

   !write(66,*) 'fixprep: xall=',sngl(xall)
   !write(66,*) 'fixprep: list=',int(list,2)

indfix = 0
xfix = zero
mm = 0
n = 0
do i=1,nall
  if(list(i) == 1) then
    n = n + 1
    x(n) = xall(i)
    kpt(n) = i
  else
    mm = mm + 1
    xfix(mm) = xall(i)
    indfix(mm) = i
  end if
end do

end subroutine fixprep

!#######################################################################

module subroutine backsort(xred,cxred,nred, x,cx)
use UR_params,    only: rn,two,zero,pi
use UR_Linft,     only: mfix,indfix,xfix,kpt
implicit none

real(rn),allocatable,intent(in)    :: xred(:)       ! values of non-fixed parameters
real(rn),allocatable,intent(in)    :: cxred(:,:)    ! values of non-fixed parameters
integer(4), intent(in)             :: nred          ! number of non-fixed paramaters
real(rn),allocatable,intent(out)   :: x(:)          ! values of non-fixed parameters
real(rn),allocatable,intent(out)   :: cx(:,:)       ! values of non-fixed parameters

integer(4)         :: i0,i,j,k,nfd,nfd2,nr,i1,k1
integer(4),allocatable :: list(:)

                   ! write(66,*)  ' backsort angekommen:  mfix=',int(mfix,2),' nred=',int(nred,2)
                   ! write(66,*)  '                       xfix=',sngl(xfix)
nr = nred + mfix
if(allocated(x)) deallocate(x)
if(allocated(cx)) deallocate(cx)
allocate(x(nr),cx(nr,nr))
allocate(list(nr))
cx = zero

! a)   the array of fit parameters:
x = zero
if(nred == nr) then
  x(1:nr) = xred(1:nr)
else
do i=1,nr
  nfd = 0
  do j=1,nred
    if(kpt(j) == i) then
      x(i) = xred(j)
      nfd = 1
      exit
    end if
  end do
  if(nfd == 1) cycle
  do j=1,mfix
    if(i == indfix(j)) then
      nfd = j
      exit
    end if
  end do
  if(nfd > 0) then
    x(i) = xfix(j)
  end if
end do
end if
  ! write(66,*) 'Backsort:  kpt=',int(kpt(1:nred))

! b)   the covariance matrix:
     ! corrected: 25.6.2023 GK
cx = zero
! i1 = i1
do i=1,nred
  do k=1,nred
         ! write(0,*) 'dim cx: ',size(cx,1),size(cx,2)
    cx(kpt(i),kpt(k)) = cxred(i,k)
    if(i /= k) cx(kpt(k),kpt(i)) = cxred(k,i)
  end do
end do

end subroutine backsort

!#########################################################################

module real(rn) function Lsqfpmle(pa,nr,t)

    ! user function, used by the subroutine FitDecayPMLE, see FitDecayPMLE
    !  for more information

use UR_params,    only: rn,two,zero,pi
use UR_linft,     only: mfix,indfix,xfix
USE UR_Linft,     ONLY: xa,ifit,mfrbg,tmedian

implicit none

integer(4),intent(in) :: nr            ! number of unfixed parameters = mfit
real(rn),intent(in)   :: pa(nr)        ! values of non-fixed parameters
real(rn),intent(in)   :: t             ! independent value of a measurement point

integer(4)            :: i,j,k,nfd
real(rn)              :: y
real(rn),allocatable  :: x(:)

allocate(x(nr+mfix))

call expand(pa,nr,x)
!x(1:nr) = pa(1:nr)
!do i=1,mfix
!  j = indfix(i)
!  x(1:nr+i) = [x(i:j-1),xfix(i),x(j:nr+i-1)]
!end do

i = int(t + 0.001_rn)
y = zero
! do k=1,3

do k=1,nr+mfix
  nfd = 0
  do j=1,mfix
    if(k == indfix(j)) nfd = 1
  end do
  if(nfd == 1) cycle
  select case (k)
       ! replaced mfit by k
    case (1)
      y = y + x(k)*xA(k,i)
    case (2)
      y = y + x(k)*xA(k,i)
    case (3)
      y = y + x(k)*xA(k,i)
  end select
end do

Lsqfpmle = y

end function Lsqfpmle

!#########################################################################

module subroutine expand(pa,nred,x)
use UR_params,    only: rn,two,zero,pi
use UR_linft,     only: mfix,indfix,xfix
implicit none

integer(4),intent(in)  :: nred
real(rn),intent(in)    :: pa(nred)
real(rn),allocatable   :: x(:)
! real(rn),intent(out),allocatable   :: x(:)

integer(4)             :: i,j,nall

nall = nred + mfix
x(1:nred) = pa(1:nred)
do i=1,mfix
  j = indfix(i)
  if(j == 1) then
    x(1:nred+i) = [xfix(i),x(j:nred+i-1)]
  else
    x(1:nred+i) = [x(1:j-1),xfix(i),x(j:nred+i-1)]
  end if
end do

end subroutine expand





!#######################################################################

module SUBROUTINE mtxmar(a,b,alam,x1,x2,nout,frac,ok)
       ! From Datan library, modified by GK
       ! a subroutine of LSQmar, calculates to solution vextores x1 and x2
       ! of the equation A.x = b

use UR_params,     only: rn
implicit none

integer(4), INTENT(INOUT)            :: nout         ! n
real(rn), allocatable,INTENT(INOUT)  :: a(:,:)       ! a(m,n)
real(rn), allocatable,INTENT(INOUT)  :: b(:,:)       ! b(m,1)
real(rn), INTENT(IN)                 :: alam
real(rn), allocatable,INTENT(out)    :: x1(:,:)      ! x1(n)
real(rn), allocatable,INTENT(out)    :: x2(:,:)      ! x2(n)
real(rn), INTENT(in)                 :: frac
LOGICAL, INTENT(OUT)                 :: ok

real(rn),allocatable   :: d(:),e(:)
integer(4), PARAMETER  :: nb=1                   ! xxxxx
integer(4)             :: m,n,L,L2,i,k

n = ubound(a,dim=2)
m = ubound(a,dim=1)
L = ubound(b,dim=2)
L2 = ubound(x1,dim=2)

allocate(d(n),e(n))
d(1:n) = 0._rn
e(1:n) = 0._rn
if(.not.allocated(x1)) allocate(x1(n,L))
if(.not.allocated(x2)) allocate(x2(n,L))
x1 = 0._rn
x2 = 0._rn

! STEP 1: Bidiagonalisation of A
   ! call matwrite(A,25,min(n,100),23,'(150es15.7)','MTXMAR, before MTXSV1: matrix A:')
call mtxsv1(a,b,d,e)
   ! call matwrite(A,25,min(n,100),23,'(150es15.7)','MTXMAR, after MTXSV1: matrix A:')
! STEP 2: Diagonalisation of bidiagonal matrix
call mtxsv2(a,b,d,e,ok)
   ! call matwrite(A,25,min(n,100),23,'(150es15.7)','MTXMAR, after MTXSV2: matrix A:')
! STEP 3: Order singular values and perform  permutations
call mtxsv3(a,b,d)
   ! call matwrite(A,25,min(n,100),23,'(150es15.7)','MTXMAR, after MTXSV3: matrix A:')
CALL mtxsvm(a,b,d,alam,x1,x2,nout,frac)
   ! call matwrite(x1,n,1,23,'(150es15.7)','MTXMAR, after MTXSVM: matrix x1:')

END SUBROUTINE mtxmar

!##############################################################################

module SUBROUTINE mtxsvm(a,b,d,alam,x1,x2,nout,frac)
        ! From Datan library, modified by GK
        ! subroutine used by mtxmar

use UR_params,    only: rn,zero,one,eps1min,two
implicit none

real(rn),allocatable, INTENT(IN)     :: a(:,:)    ! a(m,n)
real(rn),allocatable, INTENT(IN)     :: b(:,:)    ! b(m)
real(rn),allocatable, INTENT(IN)     :: d(:)      ! d(n)
real(rn),INTENT(IN)                  :: alam
real(rn),allocatable, INTENT(OUT)    :: x1(:,:)    ! x1(n)
real(rn),allocatable, INTENT(OUT)    :: x2(:,:)    ! x2(n)
real(rn), INTENT(IN)                 :: frac
integer(4), INTENT(INOUT)            :: nout

!// real(rn), PARAMETER    :: epsiln=1.E-15_rn
real(rn), PARAMETER    :: epsiln = 5._rn*eps1min
real(rn),allocatable   :: p1(:),p2(:)
integer(4)             :: i,k,kk,m,n,L,nb
real(rn)               :: fract,sinmax,alam2,alamp,alamp2
real(rn)               :: sinmin,g,den1,den2

m = ubound(a,dim=1)
n = ubound(a,dim=2)
L = ubound(x1,dim=2)
nb = ubound(b,dim=2)

allocate(p1(n),p2(n))
if(allocated(x1)) deallocate(x1);   allocate(x1(n,1))
if(allocated(x2)) deallocate(x2);   allocate(x2(n,1))
x1 = zero
x2 = zero

fract = ABS(frac)
IF(fract < epsiln) fract = epsiln
sinmax = zero
alam2 = alam**two
alamp = alam*0.1_rn
alamp2 = alamp**two
DO  i=1,n
  sinmax = MAX(sinmax,d(i))
END DO
sinmin = sinmax*fract
kk = n
DO  i=1,n
  IF(d(i) <= sinmin) THEN
    kk = i-1
       write(66,*) 'warning: mtxmar: diagonal element d(',int(i,2),') is < sinmin'
       write(66,*) '         d(i)=',sngl(d(i)),' sinmin=',sngl(sinmin)
    EXIT
  END IF
END DO

DO  i=1,m
  g = b(i,1)
  IF(i <= kk) THEN
    den1 = one/(d(i)**two + alam2)
    den2 = one/(d(i)**two + alamp2)
    p1(i) = g*d(i)*den1
    p2(i) = g*d(i)*den2
  ELSE
    IF(i <= n) THEN
      p1(i) = zero
      p2(i) = zero
    END IF
  END IF
END DO
DO  i=1,n
  x1(i,1) = sum(a(i,1:n) * p1(1:n))
  x2(i,1) = sum(a(i,1:n) * p2(1:n))
END DO
nout = kk   ! refers to the number of fit parameters, which may be
            ! reduced by the SVD treatment.

END SUBROUTINE mtxsvm

!#######################################################################

module SUBROUTINE mtxpsv(u,v,n,nred,list)
    ! From Datan library, modified by GK
    ! replaces elements of the n-vector U by the elements of vector V

use UR_params,     only: rn
implicit none

integer(4), INTENT(IN)      :: n
integer(4), INTENT(IN)      :: nred
real(rn), INTENT(OUT)       :: u(n)
real(rn), INTENT(IN)        :: v(nred)
integer(4), INTENT(IN)      :: list(n)

integer(4)  :: ired,i

ired=0
! DO  i=1,n
DO  i=1,nred
  IF(list(i) == 1) THEN
    ired=ired+1
    u(i)=v(ired)
  END IF
END DO
END SUBROUTINE mtxpsv

!#######################################################################

module SUBROUTINE Lsqlin(userfn,t,y,deltay,n,nall,list,pa,covpa,r)
       ! linear weighted Least squares fit of a linear function to a curve
       ! of measured values (t,y) using singular value decomposiition.
       ! From Datan library, modified by GK

use UR_params,     only: rn,zero,two
use UR_Linft,      only: use_PLSQ

implicit none

EXTERNAL    userfn

REAL(rn),allocatable,INTENT(IN)      :: t(:)        ! t(n)           ! independent values
REAL(rn),allocatable,INTENT(in)      :: y(:)        ! y(n)           ! dependent values
REAL(rn),allocatable,INTENT(IN)      :: deltay(:)   ! deltay(n)      ! uncertainties of c
INTEGER(4), INTENT(IN)               :: n           ! number of values
INTEGER(4), INTENT(IN)               :: nall        ! number of fit parameters
INTEGER(4),allocatable,INTENT(INOUT) :: list(:)     ! list(nall)      ! indicates which parameters are to be fixed
REAL(rn),allocatable,INTENT(IN OUT)  :: pa(:)       ! values of fitted parameters
REAL(rn),allocatable                 :: covpa(:,:)  ! covariance matrix of fitted parameters
REAL(rn), INTENT(OUT)                :: r           ! chi-square value

LOGICAL               :: ok
integer(4)            :: i,k,nr,nred,j,nrep
real(rn)              :: rr(1),c(n),fx,yfit(n)
real(rn),allocatable  :: x(:),cx(:,:),a(:,:),afunc(:),bout(:,:), cc(:,:),xx(:,:)

!-----------------------------------------------------------------------

if(minval(list) == 0) then
    nred = sum(list)
    allocate(x(nred))
     ! write(66,*) 'nall=',int(nall,2),' pa=',sngl(pa)
     ! write(66,*) 'list=',int(list,2),' nall=',int(nall,2)
  call fixprep(pa,nall,list,nred,x)    ! ,mfix,indfix,xfix)
     ! write(66,*) 'nred=',int(nred,2),' x=',sngl(x)
else
  nred = nall
    write(66,*) 'LSQlin: nall=',nall
  allocate(x(nred))
  x(1:nall) = pa(1:nall)
end if
allocate(a(n,nred),cx(nred,nred),afunc(nred),bout(n,1))
allocate(cc(1:n,1),xx(1:nred,1))
nrep = 0

35  continue

nr = nred
! Build the matrix A:
do i=1,n
  call userfn(t(i),afunc,nred)
  A(i,1:nred) = afunc(1:nred)
end do

! compute matrix A' from A
DO i=1,n
  if(nrep > 0) then
    yfit(i) = zero
    do k=1,nred
      yfit(i) = yfit(i) + x(k)*a(i,k)
    end do
  end if

  DO k=1,nr
    if(.not.use_PLSQ .or. nrep == 0) then
      a(i,k) = a(i,k)/deltay(i)
    else
      a(i,k) = a(i,k)/sqrt(yfit(i))
    end if
  END DO
END DO

! Set up vector C'
DO i=1,n
  if(.not.use_PLSQ) then
    c(i) = y(i)/deltay(i)
  else
    if(nrep > 0) then
      c(i) = y(i)/sqrt(yfit(i))
    else
      c(i) = y(i)/deltay(i)
    end if
  end if
END DO

! Set up matrix GX
cx = matmul(transpose(a),a)
! Determine vector X of unknowns
   cc(1:n,1) = c(1:n)
   xx(1:nred,1) = x(1:nred)
CALL mtxsvd(a,cc,xx,rr,0._rn,ok,bout)
   c(1:n) = cc(1:n,1)
   x(1:nred) = xx(1:nred,1)
IF(ok) THEN
  ! Determine covariance matrix CX by inverting CX
  CALL mtxchi(cx)
END IF

!write(66,*) 'Lsqlin: x: ',sngl(x)
!write(66,*) 'cx:'
!do i=1,nred
!  write(66,*) (sngl(cx(i,k)),k=1,nred)
!end do
if(use_PLSQ .and. nrep < 3) then
  nrep = nrep + 1
  goto 35
end if

! back-sort the result vector and its covar matrix:
if(minval(list) == 0) then
  call backsort(x,cx,nred, pa,covpa)
     if(abs(pa(5)-150._rn) < 1.E-6_rn) write(66,*) 'pa5=150:  x=',sngl(x),' pa=',sngl(pa)
  !write(66,*) 'Lsqlin: pa: ',sngl(pa)
  !write(66,*) 'covpa:'
  !do i=1,nall
  !  write(66,*) (sngl(covpa(i,k)),k=1,nall)
  !end do

else
  pa(1:nall) = x(1:nall)
  covpa(1:nall,1:nall) = cx(1:nall,1:nall)
  ! covpa = cx
end if
         !  write(66,*) 'nall=',int(nall,2),' pa=',sngl(pa),'  minval(list)=',minval(list)
r = zero
do i=1,n
  fx = zero
  call userfn(t(i),afunc,nred)
  ! do j=1,nall
  do j=1,nred
    fx = fx + afunc(j)*x(j)
  end do
  r = r + (fx - y(i))**two/deltay(i)**two
end do

END SUBROUTINE Lsqlin

!################################################################################

module real(rn) FUNCTION qchi2(p,n)

  ! from Datan library, modified by GK
  ! calculates the p quantile of the chi-square distribution

use UR_params,     only: rn,one,zero,half
implicit none

real(rn), INTENT(IN)      :: p   ! probability
integer(4), INTENT(IN)    :: n   ! dof

real(rn), PARAMETER :: big=1.E+10_rn
real(rn)            :: epsiln,x0,x1,xzero

! EXTERNAL szchi2
if(rn == 8) epsiln = 1.E-12_rn
if(rn == 10) epsiln = 1.E-15_rn

! boundary of range
IF(p >= one) qchi2 = big
IF(p <= zero) qchi2 = zero

! normal range
IF(p < one .AND. p > zero) THEN
  x1 = real(n,rn)
  x0 = half*x1
  CALL auxzbr(x0,x1,szchi2,p,n,0)
  CALL auxzfn(x0,x1,xzero,szchi2,p,n,0,epsiln)
  qchi2 = xzero
END IF

END FUNCTION qchi2
!-----------------------------------------------------------------

!############################################################################

module real(rn) FUNCTION szchi2(x,p,n)

 ! from Datan library, modified by GK
 ! returns P minus cumulative chisquared distribution of (X,N)

use UR_params,    only: rn
IMPLICIT none

real(rn), INTENT(IN)         :: x
real(rn), INTENT(IN)         :: p
INTEGER(4), INTENT(IN)       :: n
!-----------------------------------------------------------
szchi2 = p - pchi2(x,n)

END FUNCTION szchi2

!############################################################################

module real(rn) FUNCTION pchi2(x,n)

 ! from Datan library, modified by GK
 ! distribution function of the chis-square distribution

use UR_params,    only: rn
IMPLICIT none

real(rn), INTENT(IN)         :: x
INTEGER(4), INTENT(IN)       :: n

real(rn), PARAMETER :: half=0.5_rn
real(rn)           :: a
!-----------------------------------------------------------------------------
a = half*real(n, rn)
pchi2 = gincgm(a,half*x)

END FUNCTION pchi2

!#######################################################################

module real(rn) FUNCTION SCSTNR(X)

 ! from Datan library, modified by GK
 ! distribution function of the standard normal distribution

use UR_params,     only: rn,zero,one,half,two
implicit none

real(rn), INTENT(IN)     :: x

real(rn)               :: arg,s,f
!-------------------------------------------
arg = Half*(x**two)
s = one
IF(x < zero) s = -one
f = gincgm(Half,Arg)
SCSTNR = Half*(one+s*f)

END function SCSTNR

!#######################################################################

module real(rn) FUNCTION sqstnr(p)

 ! from Datan library, modified by GK
 ! calculates the quantile of the standard normal distribution
 ! associated with probability p

use UR_params,     only: rn,zero,one
implicit none

real(rn), INTENT(IN)       :: p

real(rn), PARAMETER :: big=1.E10_rn

real(rn)           :: x0,x1,epsiln,xzero

if(rn == 8) epsiln = 1E-8_rn
if(rn == 10) epsiln = 1E-18_rn

! boundary of range
IF(p >= one) sqstnr=big
IF(p <= zero) sqstnr=-big
! normal range
IF(p < one .AND. p > zero) THEN
  x0=zero
  x1=0.1_rn
  CALL auxzbr(x0,x1,szstnr,p,0,0)
  CALL auxzfn(x0,x1,xzero,szstnr,p,0,0,epsiln)
  sqstnr=xzero
END IF
END FUNCTION sqstnr

!-----------------------------------------------------------------------

module real(rn) FUNCTION szstnr(x,p)

 ! from Datan library, modified by GK
 ! returns P minus cumulative standardized normal of X

use UR_params,     only: rn
implicit none

real(rn), INTENT(IN OUT)    :: x
real(rn), INTENT(IN)        :: p

!------------------------------------------------------------------
szstnr = p - scstnr(x)

END FUNCTION szstnr

!#######################################################################

module real(rn) function pnorm(x, x0, sigma)

! calculates the probability (normal distribution) for a quantile value x

use UR_params,     only: rn,zero,one
implicit none

real(rn),intent(in)           :: x
real(rn),intent(in),optional  :: x0, sigma

real(rn)       :: u,xx0,xsigma

if(.not.present(x0) .and. .not.present(sigma)) then
  xx0 = zero
  xsigma = one
else
  xx0 = x0
  xsigma = sigma
end if

! Distribution function (integral) of the Normal distribution N(x0,sigma):
u = (x - xx0) / xsigma
if(abs(u) < 15._rn) then
  pnorm = scstnr(u)
else
  if(x > xx0) pnorm = one
  if(x < xx0) pnorm = zero
end if

end function pnorm

!#######################################################################

module real(rn) function qnorm(p, x0, sigma)

! calculates the quantile for probability p (normal distribution)

use UR_params,     only: rn,zero,one
implicit none

real(rn),intent(in)           :: p
real(rn),intent(in),optional  :: x0, sigma
real(rn)       :: u,xx0,xsigma

if(.not.present(x0) .and. .not.present(sigma)) then
  xx0 = zero
  xsigma = one
else
  xx0 = x0
  xsigma = sigma
end if

! Quantile of the normal distribution N(x0,sigma):

u = sqstnr(p)
qnorm = u*xsigma + xx0

end function qnorm

!#######################################################################

module real(rn) FUNCTION glngam(x)

 ! from Datan library, modified by GK
 ! calculates the natural logarithm of the gamma function

use UR_params,     only: rn,pi
implicit none

real(rn), INTENT(IN)        :: x

LOGICAL :: reflec
real(rn), PARAMETER :: rtwopi=sqrt(2._rn*Pi)
real(rn), PARAMETER :: one=1._rn
real(rn), PARAMETER :: half=0.5_rn
real(rn)            :: c(6)
integer(4)          :: i
real(rn)            :: xx,xh,xgh,s,anum,g

!DATA c/76.18009173_rn,-86.50532033_rn,24.01409822_rn,  &
!    -1.231739516_rn,0.120858003E-2_rn,-0.536382E-5_rn/

  c = (/76.18009172947146_rn, -86.50532032941677_rn, &
        24.01409824083091_rn, -1.231739572450155_rn, &
        .1208650973866179E-2_rn, -.5395239384953E-5_rn /)

IF(x >= one) THEN
  reflec = .false.
  xx = x-one
ELSE
  reflec = .true.
  xx = one-x
END IF
xh = xx+half
xgh = xx+5.5_rn
s = one
anum = xx
DO i=1,6
  anum = anum+one
  s = s + c(i)/anum
END DO
s = s * rtwopi
g = xh*LOG(xgh)+LOG(s)-xgh
IF (reflec) THEN
  glngam = LOG(pi*xx) - g - LOG(SIN(pi*xx))
ELSE
  glngam = g
END IF

END FUNCTION glngam

!#######################################################################

module SUBROUTINE auxzbr(x0,x1,funct,par,npar1,npar2)

 ! from Datan library, modified by GK
 ! Bracketing the root of the function given by funct, delivers x0,x1

use UR_params,     only: rn,zero,one,two,eps1min
implicit none

! EXTERNAL funct

real(rn), INTENT(IN OUT)    :: x0   !  Arguments safely encompassing the root
real(rn), INTENT(IN OUT)    :: x1   !
real(rn), INTENT(IN)        :: par     !  three parameters, on which funct
integer(4), INTENT(IN)      :: npar1   !  depends
integer(4), INTENT(IN)      :: npar2   !

real(rn), EXTERNAL :: funct

integer(4)    :: i
real(rn)      :: f0,f1,xs,funct
!------------------------------------------------------------------
IF(abs(x0-x1) < Eps1min) x1 = x0 + one
f0 = funct(x0,par,npar1,npar2)
f1 = funct(x1,par,npar1,npar2)
DO  i=1,1000
  IF(f0*f1 > zero) THEN
    IF(ABS(f0) <= ABS(f1)) THEN
      xs = x0
      x0 = x0 + two*(x0-x1)
      x1 = xs
      f1 = f0
      f0 = funct(x0,par,npar1,npar2)
    ELSE
      xs = x1
      x1 = x1 + two*(x1-x0)
      x0 = xs
      f0 = f1
      f1 = funct(x1,par,npar1,npar2)
    END IF
  ELSE
    EXIT
  END IF
END DO

END SUBROUTINE auxzbr

!#######################################################################

module SUBROUTINE auxzfn(x0,x1,xzero,funct,par,npar1,npar2,epsiln)

 ! from Datan library, modified by GK
 ! Finding the root (by bisection) of the function given by funct

use UR_params,     only: rn,zero,half,eps1min
implicit none

EXTERNAL funct

real(rn), INTENT(IN OUT)   :: x0
real(rn), INTENT(IN OUT)   :: x1
real(rn), INTENT(OUT)      :: xzero
real(rn), INTENT(IN)       :: par
integer(4), INTENT(IN)     :: npar1
integer(4), INTENT(IN)     :: npar2
real(rn), INTENT(IN)       :: epsiln

integer(4)      :: i
real(rn)        :: f0,f1,fm,test,xm,funct
!------------------------------------------------------------------
xzero = x0
DO  i=1,2000
  f0 = funct(x0,par,npar1,npar2)
  f1 = funct(x1,par,npar1,npar2)
  IF(abs(f0) < Eps1min) THEN
    xzero = x0
    EXIT
  ELSE IF(abs(f1) < Eps1min) THEN
    xzero = x1
    EXIT
  END IF
  xm =half*(x0+x1)
  IF(ABS(x0-x1) >= epsiln) THEN
    fm = funct(xm,par,npar1,npar2)
    test = f0*fm
    IF(test < zero) THEN
      x1 = xm
    ELSE
      x0 = xm
    END IF
  ELSE
    xzero = xm
    EXIT
  END IF
END DO

END SUBROUTINE auxzfn

!#######################################################################

module REAL(rn) FUNCTION gincgm(a,x)
            ! from Datan library, modified by GK
            ! calculates the incomplete gamma function

use UR_params,     only: rn,zero,one,eps1min
implicit none

REAL(rn), INTENT(IN)             :: a
REAL(rn), INTENT(IN)             :: x

integer(4)    :: i,j
real(rn)      :: a0,a1,b0,b1,a2j,a2j1,b2j,b2j1,cf,fnorm,f,s,anum,aloggm
real(rn)      :: cfnew,help
REAL(rn), PARAMETER :: big=500._rn
REAL(rn), PARAMETER :: epsiln=1.E-12    ! 1.E-6_rn
!-----------------------------------------------------------------------
cfnew = one
aloggm = glngam(a)
IF(x <= a+one) THEN
  ! series development
  f = one/a
  s = f
  anum = a
  DO  i=1,100
    anum = anum + one
    f = x*f/anum
    s = s + f
    IF(f < epsiln) EXIT
  END DO
  IF(x < epsiln) THEN
    gincgm = zero
  ELSE
    help = a*LOG(x) - x - aloggm
    IF(ABS(help) >= big) THEN
      gincgm = zero
    ELSE
      gincgm = s*EXP(help)
    END IF
  END IF
ELSE
  ! continued fraction
  a0 = zero
  b0 = one
  a1 = one
  b1 = x
  cf = one
  fnorm = one
  DO  j=1,100
    a2j = real(j,rn) - a
    a2j1 = real(j,rn)
    b2j = one
    b2j1 = x
    a0 = (b2j*a1 + a2j*a0)*fnorm
    b0 = (b2j*b1 + a2j*b0)*fnorm
    a1 = b2j1*a0 + a2j1*a1*fnorm
    b1 = b2j1*b0 + a2j1*b1*fnorm
    IF(abs(b1-zero) > eps1min) THEN
      ! renormalize and test for convergence
      fnorm = one/b1
      cfnew = a1*fnorm
      IF(ABS(cf-cfnew)/cf < epsiln) EXIT
      cf = cfnew
    END IF
  END DO
  help = a*LOG(x) - x - aloggm
  IF(ABS(help) >= big) THEN
    gincgm = one
  ELSE
    gincgm = one - EXP(help)*cfnew
  END IF
END IF
END FUNCTION gincgm

!#######################################################################

module REAL(rn) FUNCTION gincbt(aa,bb,xx)
            ! from Datan library, modified by GK
            ! calculates the incomplete beta function

use UR_params,     only: rn,zero,one,two,eps1min
implicit none

REAL(rn), INTENT(IN)             :: aa
REAL(rn), INTENT(IN)             :: bb
REAL(rn), INTENT(IN)             :: xx

LOGICAL :: reflec
REAL(rn), PARAMETER :: epsiln=1.E-12_rn    ! 1.d-8

integer(4)      :: m
real(rn)        :: a,b,x,a1,a2,b1,b2,rm,apl2m,cf,cfnew,d2m,d2m1,fnorm,xlim
!-----------------------------------------------------------------------
xlim = (aa + one)/(aa + bb + one)
IF (xx < xlim) THEN
  reflec = .false.
  a = aa
  b = bb
  x = xx
ELSE
  reflec = .true.
  a = bb
  b = aa
  x = one - xx
END IF
IF(x < epsiln) THEN
  ! function known at end of range
  cf = zero
ELSE
  ! continued fraction
  a1 = one
  b1 = one
  a2 = one
  b2 = one -(a + b)*x/(a + one)
  fnorm = one/b2
  cf = a2*fnorm
  DO  m=1,100
    rm = real(m,rn)
    apl2m = a + two*rm
    d2m = rm*(b - rm)*x/((apl2m - one)*apl2m)
    d2m1 = -(a + rm)*(a + b + rm)*x/(apl2m*(apl2m + one))
    a1 = (a2 + d2m*a1)*fnorm
    b1 = (b2 + d2m*b1)*fnorm
    a2 = a1 + d2m1*a2*fnorm
    b2 = b1 + d2m1*b2*fnorm
    IF(b2 /= zero) THEN
      ! renormalize and test for convergence
      fnorm = one/b2
      cfnew = a2*fnorm
      IF(ABS(cf-cfnew)/cf < epsiln) EXIT
      cf = cfnew
    END IF
  END DO
  cf = cf*(x**a)*((one - x)**b)/(a*gbetaf(a,b))
END IF
IF(reflec) THEN
  gincbt = one - cf
ELSE
  gincbt = cf
END IF
END FUNCTION gincbt

!#######################################################################

module real(rn) function gbetaf(z,w)
      ! from Datan library, modified by GK
      ! calculates the beta function B(z,w)

use UR_params,   only: rn
implicit none
real(rn),intent(in)   :: z
real(rn),intent(in)   :: w
real(rn),parameter    :: big=1.E+30_rn, epsiln=1.E-12_rn
!--------------------------------------
if(w < epsiln) then
  gbetaf = big
else
  gbetaf = exp(glngam(z) + glngam(w) - glngam(z+w))
end if

end function gbetaf

!#######################################################################

module real(rn) function mean(x)

 ! function for calculating the aritmetic mean of the array x values

use UR_params,   only: rn
implicit none
real(rn),intent(in)   :: x(:)
integer     :: n
!--------------------------------------
n = size(x,1)
mean  = sum(x(1:n)) / real(n,rn)

end function mean

!#############################################################################

module real(rn) function sd(x)

 ! function for calculating the standard deviation of the array x values

use UR_params,   only: rn,one,two
implicit none
 real(rn),intent(in)   :: x(:)
 integer    :: i,n
real(rn)    :: m2,sum_x,sum_x2,m3   ! , m1,mean,q2
!--------------------------------------
n = size(x,1)

m2 = sum(x(1:n)) / real(n,rn)
sum_x2 = dot_product(x-m2,x-m2)
sum_x = sum(x-m2)
m3 = sum_x/real(n,rn)
sd = sqrt( sum_x2 - real(n,rn)*m3**two ) / sqrt(real(n,rn)-one)

end function sd

!#######################################################################


module subroutine MatRand(icn,ncr,covxy,muvect,zvect,bvect,kk)

!  Generate normal distributed values having a pre-defined covariance
!  according to the text book
!     V. Blobel & E. Lohrmann (1998), "Statistische und numerische
!     Methoden der Datenanalyse", Teubner, page Seite 167

!  For the selection of correlating quantities:
!    Input:
!    zvect  : vector of values zi from the standard normal distribution
!    muvect : vector of normal mean values  mue
!    covxy  : covariance matrix

!    Output:
!    bvect  : vector of transformed values of the correlating quantities

!     Copyright (C) 2014-2023  Günter Kanisch
!-----------------------------------------------------------------------

use UR_params,     only: rn,eps1min

implicit none

integer(4),INTENT(IN)   :: icn            ! rank of the matrix covxy = number of correlating quantities muvect
integer(4),intent(IN)   :: ncr            ! phsyical dimension of covxy
real(rn),INTENT(IN)     :: covxy(ncr,ncr)      ! covariance matrix
real(rn),INTENT(IN)     :: muvect(icn)         ! input vector of mean values
real(rn),INTENT(IN)     :: zvect(icn,1)        ! random standard normal values
real(rn),INTENT(OUT)    :: bvect(icn)          ! random fluctuations around muvect
integer(4),INTENT(IN)   :: kk                  ! MC loop number

integer(4)       :: i,j,kkmin
real(rn)         :: cov(icn,icn), z(icn,1), b(icn,1), Rmat(icn,icn)

!-----------------------------------------------------------------------
kkmin = -1
 ! kkmin = 3

cov(1:icn,1:icn) = covxy(1:icn,1:icn)

if(kk <= kkmin) then
  do i=1,icn
    write(66,'(15(es11.3))') (cov(i,j),j=1,icn)
  end do
  write(66,*)
end if

! Warning: one should guarantee that the matrix diagonal does not contain zero values,
! otherwise MTXCHL stops too early and Rmat is not complete!
   do i=1,icn
     if(abs(cov(i,i)) < eps1min) cov(i,i) = 1.E-17_rn
   end do

! Replace (Cholesky) the covariance matrix cov as a product (transpose(R) x R):
! Rmat = Triangular matrix  R

call MTXCHL(cov,Rmat)

IF(kk <= kkmin) THEN
  Write(66,*) '    Rank of matrix RMAT: ',icn,'   physikcal dim of covxy: ',ncr
  call matwrite(Rmat,icn,icn,66,'(20es11.3)','triangular matrix Rmat:')
end if
z(1:icn,1) = zvect(1:icn,1)
! Product: b-vector = R-trans x z-Vector:
b = matmul(Transpose(Rmat), z)
bvect(1:icn) = b(1:icn,1)

IF(kk <= kkmin) WRITE(66,'(a,2x,20es11.3)') 'muvect: ',(muvect(i),i=1,icn)
IF(kk <= kkmin) WRITE(66,'(a,2x,20es11.3)') 'zvect: ',(zvect(i,1),i=1,icn)
IF(kk <= kkmin) WRITE(66,'(a,2x,20es11.3)') 'bvect: ',(bvect(i),i=1,icn)
bvect(1:icn) = b(1:icn,1) + muvect(1:icn)

end subroutine MatRand

!#####################################################################################

module SUBROUTINE auxdri(f,x,tt,ny,nr,nred,list,aa,ok)
        ! from Datan library, modified by GK
        ! Computes matrix of derivatives
        ! Based on H. Rutishauser, Ausdehnung des Rombergschen Prinzips
        ! (Extension of Romberg's Principle), Numer. Math. 5 (1963) 48-54
        ! Program based on routine DERIV by K.S.Koelbig in
        ! CERN Computer Center Program Library, CERN, Geneva

use UR_params,          only: rn,one,two,three,zero
use UR_Linft,           only: mfix,indfix,xa !  x1a,x2a,x3a
use UR_Derivats,        only: dervtype



implicit none

real(rn), EXTERNAL   :: f

integer(4), INTENT(IN)       :: ny            ! number of measurement points
integer(4), INTENT(IN)       :: nr            ! number of unknowns
integer(4), INTENT(IN)       :: nred          ! number of parameters to be fitted
real(rn), INTENT(OUT)        :: x(nr)         ! vector fit parameters
real(rn), INTENT(in)         :: tt(ny)        ! vector of controlled variables
integer(4), INTENT(IN)       :: list(nr)      !  list(k): =1 fit parameter k; =0 do not fit parameter k
real(rn), INTENT(OUT)        :: aa(ny,nred)   ! matrix of working area, the derivatives
LOGICAL, INTENT(OUT)         :: ok

! real(rn), PARAMETER :: eps = 5.E-8_rn           ! until 20.1.2023
real(rn), PARAMETER :: eps = 5.E-10_rn
real(rn), PARAMETER :: epsi = 1.E-10_rn
real(rn), PARAMETER :: delta = 10.0_rn
real(rn), PARAMETER :: s = 1.0E-1_rn

integer(4)    :: i,ir,k,ix,iy,m,k3,ib,j,nfd
integer(4),allocatable  :: indb(:)
real(rn)      :: dx(0:9),w(0:9,3),t(0:9,0:9),a(0:9)
real(rn)      :: del,fminus,fplus,h,xsav,derv
LOGICAL       :: lev(0:9),lmt
! real(8)       :: wx(0:9,3)

dx = [ 0.0256_rn, 0.0192_rn, 0.0128_rn, 0.0096_rn, 0.0064_rn,  &
       0.0048_rn, 0.0032_rn, 0.0024_rn, 0.0016_rn, 0.0012_rn ]

!DATA (lev(k),k = 0,8,2) /5*.true./
!DATA (lev(k),k=1,9,2) /5*.false./

lev(0:9) = [ .true., .false., .true., .false., .true., .false., .true., .false., .true., .false. ]

! DATA wx(1,1) /1.3333333333333333D+00/
! DATA wx(3,1) /1.0666666666666667D+00/
! DATA wx(5,1) /1.0158730158730159D+00/
! DATA wx(7,1) /1.0039215686274510D+00/
! DATA wx(2,1) /3.3333333333333333D-01/
! DATA wx(4,1) /6.6666666666666667D-02/
! DATA wx(6,1) /1.5873015873015873D-02/
! DATA wx(8,1) /3.9215686274509804D-03/
! DATA wx(0,2) /2.2857142857142857D+00/
! DATA wx(2,2) /1.1636363636363636D+00/
! DATA wx(4,2) /1.0364372469635628D+00/
! DATA wx(6,2) /1.0088669950738916D+00/
! DATA wx(8,2) /1.0022021042329337D+00/
! DATA wx(1,2) /1.2857142857142857D+00/
! DATA wx(3,2) /1.6363636363636364D-01/
! DATA wx(5,2) /3.6437246963562753D-02/
! DATA wx(7,2) /8.8669950738916256D-03/
! DATA wx(9,2) /2.2021042329336922D-03/
! DATA wx(0,3) /1.8000000000000000D+00/
! DATA wx(2,3) /1.1250000000000000D+00/
! DATA wx(4,3) /1.0285714285714286D+00/
! DATA wx(6,3) /1.0069930069930070D+00/
! DATA wx(8,3) /1.0017391304347826D+00/
! DATA wx(1,3) /8.0000000000000000D-01/
! DATA wx(3,3) /1.2500000000000000D-01/
! DATA wx(5,3) /2.8571428571428571D-02/
! DATA wx(7,3) /6.9930069930069930D-03/
! DATA wx(9,3) /1.7391304347826087D-03/

w(1,1) = one + one/three
! w(3,1) = one + 3.2_rn/three  ! until 15.1.2023
w(3,1) = zero + 3.2_rn/three
w(5,1) = one + one/63._rn
w(7,1) = one + one/255._rn
w(2,1) = one/three
w(4,1) = two/30._rn
w(6,1) = one/63._rn
w(8,1) = one/255._rn
w(0,2) = two + two/7.0_rn
w(2,2) = one + 9._rn/55._rn
w(4,2) = one + one/(27._rn + one/2.25_rn)
w(6,2) = one + one/(112.5_rn + one/3.6_rn)
w(8,2) = one + one/(454._rn + one/9._rn)
w(1,2) = one + two/7._rn
w(3,2) = 9.0_rn/55.0_rn
w(5,2) = one/(27._rn + one/2.25_rn)
w(7,2) = one/(112.5_rn + one/3.6_rn)
w(9,2) = one/(454._rn + one/9.0_rn)
w(0,3) = 1.80_rn
w(2,3) = 1.125_rn
w(4,3) = one + two/7._rn/10._rn
w(6,3) = one + one/143._rn
w(8,3) = one + one/575._rn
w(1,3) = 0.8_rn
! w(3,3) = 1.25_rn  ! until 15.1.2023
w(3,3) = 0.125_rn
! w(5,3) = 20._rn/7._rn  ! until 15.1.2023
w(5,3) = 0.20_rn/7._rn
w(7,3) = one/143._rn
w(9,3) = one/575._rn

!do i=0,9
!  do k=1,3
!    write(66,'(a,i0,a,i0,2(a,es20.10))') 'w(',i,',',k,')=',real(w(i,k),8),' w-wx=',real(w(i,k)-wx(i,k),8)
!  end do
!end do

allocate(indb(nr+mfix))
indb = 0
ib = 0
do i=1,nr
  nfd = 0
  do k3=1,mfix
    if(indfix(k3) == i) nfd = 1
  end do
  if(nfd == 1) then
    cycle
  else
    ib = ib + 1
    indb(ib) = i
  end if
end do

            ! write(0,'(a,i0,1x,i0,2(a,i0),a,5i4)') ' auxdri: nr,nred=',nr,nred,' mfix=',mfix,' ny=',ny,' list=',list
            ! write(0,*) 'userfn: ',sngl(f(x,nr, tt(1),mfix,indfix,xfix))
ok = .true.
aa = 0._rn
DO  iy=1,ny

  ix = 0
  DO  ir=1,nr
    ! IF(list(ir) /= 0) THEN
    if(.true.) then
      ix = ix + 1
      if(dervtype == 'A') then
        derv = zero
        if(indb(ir) == 1) derv = xA(1,iy)   ! x1A(iy)
        if(indb(ir) == 2) derv = xA(2,iy)   ! x2A(iy)
        if(indb(ir) == 3) derv = xA(3,iy)   ! x3A(iy)
        aa(iy,ix) = derv
        cycle
      end if

      del = delta
      xsav = x(ir)
          ! write(0,*) ' iy=',int(iy,2),' ir=',int(ir,2)
      DO  i=1,10
        del = s*del
        IF(i == 10 .OR. ABS(xsav + del*dx(9) - xsav) < eps) THEN
          ok = .false.
                write(0,*) 'auxdri: ok=F:  xsav + del*dx(9) - xsav=',xsav + del*dx(9) - xsav
          RETURN
        END IF
             a = 0._rn
             t = 0._rn

        DO  k = 0,9
          h = del*dx(k)
          x(ir) = xsav + h
          fplus = f(x,nr, tt(iy))
          x(ir) = xsav - h
          fminus = f(x,nr,tt(iy))
          x(ir) = xsav
          t(k,0) = (fplus - fminus)/(h + h)
          a(k) = t(k,0)
        END DO
        IF(a(0) >=  a(9)) THEN
          a(0:9) = -a(0:9)
        END IF

        lmt = .true.
        DO  k = 1,9
          h = a(k-1) - a(k)
          lmt = lmt .AND. (h <= epsi .OR. ABS(h) <= eps*ABS(a(k)) + epsi)
        END DO
        IF(lmt) EXIT
      END DO
      DO  m = 1,9
        DO  k = 0,9-m
          IF(lev(m)) THEN
            t(k,m) = w(m-1,1)*t(k+1,m-1) - w(m,1)*t(k,m-1)
          ELSE IF(lev(k)) THEN
            t(k,m) = w(m-1,2)*t(k+1,m-1) - w(m,2)*t(k,m-1)
          ELSE
            t(k,m) = w(m-1,3)*t(k+1,m-1) - w(m,3)*t(k,m-1)
          END IF
        END DO
      END DO
      aa(iy,ix) = t(0,9)

         ! if(iy >= 1 .and. iy <= 25 .and. ix >= 1) write(23,*) 'iy=',int(iy,2),' ix=',int(ix,2), '  array t(0,1:9)=',sngl(t(0,1:9))

    END IF
  END DO
    ! if(iy >= 1 .and. iy <= 25) write(23,*) 'iy=',int(iy,2),'  aa(iy,1:nr)=',sngl(aa(iy,1:nr))
END DO
END SUBROUTINE auxdri

!##############################################################################################

module SUBROUTINE mtxequ(a,b,n,m)
         ! not used!
use UR_params,          only: rn,one,two,three,zero
implicit none

real(rn), INTENT(IN OUT)      :: a(n,n)
real(rn), INTENT(OUT)         :: b(n,m)
INTEGER(4), INTENT(IN)        :: n
INTEGER(4), INTENT(IN)        :: m

integer(4)         :: k,kk,l,i,i1,j
real(rn)           :: amax,save

DO  k=1,n-1
  amax = zero
  kk = k
  DO  l=k,n
    IF(ABS(amax) < ABS(a(l,k))) THEN
      amax = a(l,k)
      kk = l
    END IF
  END DO
  IF(kk /= k) THEN
    DO  j=k,n
      SAVE = a(k,j)
      a(k,j) = a(kk,j)
      a(kk,j) = SAVE
    END DO
    DO  i=1,m
      SAVE= b(k,i)
      b(k,i) = b(kk,i)
      b(kk,i) = SAVE
    END DO
  END IF
  DO  i=k+1,n
    DO  j=k+1,n
      a(i,j) = a(i,j) - a(k,j)*a(i,k)/a(k,k)
    END DO
    DO  j=1,m
      b(i,j) = b(i,j) - b(k,j)*a(i,k)/a(k,k)
    END DO
  END DO
END DO
DO  j=1,m
  b(n,j) = b(n,j)/a(n,n)
  IF(n > 1) THEN
    DO  i1=1,n-1
      i = n-i1
      DO  l=i+1,n
        b(i,j) = b(i,j) - a(i,l)*b(l,j)
      END DO
      b(i,j) = b(i,j)/a(i,i)
    END DO
  END IF
END DO
END SUBROUTINE mtxequ

!#############################################################################


 end submodule Brandta
