 module Brandt

  use UR_params,   only: rn
  implicit none


  interface
    module SUBROUTINE mtxchi(a)
      ! integer(4), INTENT(IN)    :: n
      real(rn), INTENT(INOUT)   :: a(:,:)
    end SUBROUTINE mtxchi

    module SUBROUTINE mtxchl(a,u)
      ! integer(4), INTENT(IN)      :: n
      real(rn), INTENT(IN)        :: a(:,:)
      real(rn), INTENT(IN OUT)    :: u(:,:)
    end SUBROUTINE mtxchl

    module SUBROUTINE mtxchm(u,a,r,m,n)
      integer(4), INTENT(IN)     :: m
      integer(4), INTENT(IN)     :: n
      real(rn), INTENT(IN)       :: u(m,m)
      real(rn), INTENT(IN)       :: a(m,n)
      real(rn), INTENT(OUT)      :: r(m,n)
    end SUBROUTINE mtxchm

    module SUBROUTINE mtxgva(v1,v2,c,s)
      real(rn), INTENT(INOUT)   :: v1 ! vector components, defining the Givens-transformation
      real(rn), INTENT(INOUT)   :: v2 !
      real(rn), INTENT(OUT)     :: c    ! constants of the transformed vector
      real(rn), INTENT(OUT)     :: s    !
    END SUBROUTINE mtxgva

    module SUBROUTINE mtxgvd(v1,v2,c,s)
      real(rn), INTENT(IN)   :: v1 ! vector components, defining the Givens-rotation
      real(rn), INTENT(IN)   :: v2 !
      real(rn), INTENT(OUT)  :: c    ! constants of the transformed vector
      real(rn), INTENT(OUT)  :: s    !
    END SUBROUTINE mtxgvd

    module SUBROUTINE mtxgvt(z1,z2,c,s)
      real(rn), INTENT(IN OUT)     :: z1   ! components of a vector to be transformed
      real(rn), INTENT(IN OUT)     :: z2   !
      real(rn), INTENT(IN)         :: c  !  constants
      real(rn), INTENT(IN)         :: s  !
    END SUBROUTINE mtxgvt

    module SUBROUTINE mtxhsd(v,up,b,n,lp,l)
      integer(4), INTENT(IN)       :: n
      real(rn), INTENT(IN)         :: v(n)
      real(rn), INTENT(OUT)        :: up
      real(rn), INTENT(OUT)        :: b
      integer(4), INTENT(IN)       :: lp
      integer(4), INTENT(IN)       :: l
    END SUBROUTINE mtxhsd

    module SUBROUTINE mtxhst(v,up,b,c,n,lp,l)
      integer(4), INTENT(IN)       :: n
      real(rn), INTENT(IN)         :: v(n)
      real(rn), INTENT(IN)         :: up
      real(rn), INTENT(IN)         :: b
      real(rn), INTENT(IN OUT)     :: c(n)
      integer(4), INTENT(IN)       :: lp
      integer(4), INTENT(IN)       :: l
    END SUBROUTINE mtxhst

    module SUBROUTINE mtxlsc(a,b,e,d,x,r,a2,frac,ok)        ! m,n,l,
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
    END SUBROUTINE mtxlsc

    module SUBROUTINE mtxsv1(a,b,d,e)
      real(rn), INTENT(IN OUT)    :: a(:,:)      ! matrix a(m,n))
      real(rn), INTENT(IN OUT)    :: b(:,:)      ! matrix b(m,nb)
      real(rn), INTENT(OUT)       :: d(:)        ! vector d(n)
      real(rn), INTENT(OUT)       :: e(:)        ! vector e(n)
    END SUBROUTINE mtxsv1

    module SUBROUTINE mtxsv2(a,b,d,e,ok)
      real(rn), INTENT(IN OUT)    :: a(:,:)        ! matrix a(m,n)
      real(rn), INTENT(IN OUT)    :: b(:,:)        ! matrix b(m,nb)
      real(rn), INTENT(IN OUT)    :: d(:)          ! vector d(n)
      real(rn), INTENT(IN OUT)    :: e(:)          ! vector e(n)
      LOGICAL, INTENT(OUT)        :: ok
    END SUBROUTINE mtxsv2

    module SUBROUTINE mtxs21(a,d,e,k)
      real(rn), INTENT(IN OUT)    :: a(:,:)      ! matrix a(m,n)
      real(rn), INTENT(IN OUT)    :: d(:)        ! vector d(n)
      real(rn), INTENT(IN OUT)    :: e(:)        ! vector e(n)
      integer(4), INTENT(IN)      :: k
    END SUBROUTINE mtxs21

    module SUBROUTINE mtxs22(b,d,e,k,l)
      real(rn), INTENT(IN OUT)    :: b(:,:)      ! matrix b(m,nb)
      real(rn), INTENT(IN OUT)    :: d(:)        ! vector d(n)
      real(rn), INTENT(IN OUT)    :: e(:)        ! vector e(n)
      integer(4), INTENT(IN)      :: k
      integer(4), INTENT(IN)      :: l
    END SUBROUTINE mtxs22

    module SUBROUTINE mtxs23(a,b,d,e,k,l)
      real(rn), INTENT(IN OUT)    :: a(:,:)     ! matrix a(m,n)
      real(rn), INTENT(IN OUT)    :: b(:,:)     ! matrix b(m,nb)
      real(rn), INTENT(IN OUT)    :: d(:)       ! vector d(n)
      real(rn), INTENT(IN OUT)    :: e(:)       ! vector e(n)
      integer(4), INTENT(IN)      :: k
      integer(4), INTENT(IN)      :: l
    END SUBROUTINE mtxs23

    module SUBROUTINE mtxsv3(a,b,d)
      real(rn), INTENT(IN OUT)    :: a(:,:)      ! matrix a(m,n)
      real(rn), INTENT(IN OUT)    :: b(:,:)      ! matrix b(m,nb)
      real(rn), INTENT(IN OUT)    :: d(:)        ! vector d(n)
    END SUBROUTINE mtxsv3

    module SUBROUTINE mtxsv4(a,b,d,x,r,frac,bout)
      real(rn), INTENT(IN)        :: a(:,:)      ! matrix a(m,n)
      real(rn), INTENT(INOUT)     :: b(:,:)      ! matrix b(m,nb)
      real(rn), INTENT(IN)        :: d(:)        ! vector d(n)
      real(rn), INTENT(OUT)       :: x(:,:)      ! matrix x(n,nb)
      real(rn), INTENT(OUT)       :: r(:)        ! vector (nb) of squares of the residuals for columns of the Matrix A
      real(rn), INTENT(IN)        :: frac
      real(rn), intent(out)       :: bout(:,:)   ! matrix bout(m,nb)
    END SUBROUTINE mtxsv4

    module SUBROUTINE mtxsvd(a,b,x,r,frac,ok,bout)
      real(rn), INTENT(IN OUT)    :: a(:,:)      ! matrix a(m,n)
      real(rn), INTENT(IN OUT)    :: b(:,:)      ! matrix b(m,nb)
      real(rn), INTENT(IN OUT)    :: x(:,:)      ! matrix x(n,nb)
      real(rn), INTENT(IN OUT)    :: r(:)        ! vector r(n)
      real(rn), INTENT(IN)        :: frac
      LOGICAL, INTENT(OUT)        :: ok
      real(rn),intent(out)        :: bout(:,:)   ! matrix bout(m,nb)
    END SUBROUTINE mtxsvd

    module real(rn) FUNCTION qchi2(p,n)
      real(rn), INTENT(IN)      :: p   ! probability
      integer(4), INTENT(IN)    :: n   ! dof
    END FUNCTION qchi2

    module real(rn) FUNCTION szchi2(x,p,n)
      real(rn), INTENT(IN)         :: x
      real(rn), INTENT(IN)         :: p
      INTEGER(4), INTENT(IN)       :: n
    END FUNCTION szchi2

    module real(rn) FUNCTION pchi2(x,n)
      real(rn), INTENT(IN)         :: x
      INTEGER(4), INTENT(IN)       :: n
    END FUNCTION pchi2

    module real(rn) FUNCTION SCSTNR(X)
      real(rn), INTENT(IN)     :: x
    END function SCSTNR

    module real(rn) FUNCTION sqstnr(p)
      real(rn), INTENT(IN)       :: p
    END FUNCTION sqstnr

    module real(rn) FUNCTION szstnr(x,p)
      real(rn), INTENT(IN OUT)    :: x
      real(rn), INTENT(IN)        :: p
    END FUNCTION szstnr

    module real(rn) function pnorm(x, x0, sigma)
      real(rn),intent(in)           :: x
      real(rn),intent(in),optional  :: x0, sigma
    end function pnorm

    module real(rn) function qnorm(p, x0, sigma)
      real(rn),intent(in)           :: p
      real(rn),intent(in),optional  :: x0, sigma
      real(rn)       :: u,xx0,xsigma
    end function qnorm


    module real(rn) FUNCTION glngam(x)
      real(rn), INTENT(IN)        :: x
    END FUNCTION glngam

    module SUBROUTINE auxzbr(x0,x1,funct,par,npar1,npar2)
      real(rn), INTENT(IN OUT)    :: x0   !  Arguments safely encompassing the root
      real(rn), INTENT(IN OUT)    :: x1   !
      real(rn), INTENT(IN)        :: par     !  three parameters, on which funct
      integer(4), INTENT(IN)      :: npar1   !  depends
      integer(4), INTENT(IN)      :: npar2   !
      real(rn), EXTERNAL       :: funct
    END SUBROUTINE auxzbr

    module SUBROUTINE auxzfn(x0,x1,xzero,funct,par,npar1,npar2,epsiln)
       real(rn), INTENT(IN OUT)   :: x0
      real(rn), INTENT(IN OUT)   :: x1
      real(rn), INTENT(OUT)      :: xzero
      real(rn), INTENT(IN)       :: par
      integer(4), INTENT(IN)     :: npar1
      integer(4), INTENT(IN)     :: npar2
      real(rn), INTENT(IN)       :: epsiln
      real(rn), EXTERNAL     :: funct
    END SUBROUTINE auxzfn

    module REAL(rn) FUNCTION gincgm(a,x)
      use UR_params,     only: rn,zero,one,half,eps1min
      implicit none

      REAL(rn), INTENT(IN)             :: a
      REAL(rn), INTENT(IN)             :: x
    end function gincgm

    module REAL(rn) FUNCTION gincbt(aa,bb,xx)
      use UR_params,     only: rn,zero,one,two,eps1min
      implicit none
      REAL(rn), INTENT(IN)             :: aa
      REAL(rn), INTENT(IN)             :: bb
      REAL(rn), INTENT(IN)             :: xx
    end function gincbt


    module real(rn) function gbetaf(z,w)
      use UR_params,   only: rn
      implicit none
      real(rn),intent(in)   :: z
      real(rn),intent(in)   :: w
    end function gbetaf

    module real(rn) function mean(x)
      real(rn),intent(in)   :: x(:)
    end function mean

    module real(rn) function sd(x)
      real(rn),intent(in)   :: x(:)
    end function sd

    module subroutine MatRand(icn,ncr,covxy,muvect,zvect,bvect,kk)
      integer(4),INTENT(IN)   :: icn    ! rank of the matrix covxy = number of correlating quantities muvect
      integer(4),intent(IN)   :: ncr    ! phsyical dimension of covxy
      real(rn),INTENT(IN)     :: covxy(ncr,ncr)        ! so wurde die Matrix angelegt
      real(rn),INTENT(IN)     :: muvect(icn)
      real(rn),INTENT(IN)     :: zvect(icn,1)
      real(rn),INTENT(OUT)    :: bvect(icn)
      integer(4),INTENT(IN)   :: kk              ! MC loop number
    end subroutine MatRand

    module subroutine fixprep(xall,nall,list,nred,x)
      use UR_params,    only: rn,two,zero,pi
      use UR_Linft,     only: mfit,indfix,xfix
      implicit none
      integer(4),intent(in)              :: nall          ! number of all parameters
      real(rn),intent(in)                :: xall(nall)    ! values of non-fixed parameters
      integer(4), intent(in)             :: list(nall)    !
      integer(4), intent(out)            :: nred          !
      real(rn),allocatable,intent(out)   :: x(:)       ! values of non-fixed parameters
    end subroutine fixprep

    module subroutine backsort(xred,cxred,nred, x,cx)
      use UR_params,    only: rn,two,zero,pi
      use UR_Linft,     only: mfit,indfix,xfix
      implicit none
      real(rn),allocatable,intent(in)    :: xred(:)       ! values of non-fixed parameters
      real(rn),allocatable,intent(in)    :: cxred(:,:)    ! values of non-fixed parameters
      integer(4), intent(in)             :: nred          !
      real(rn),allocatable,intent(out)   :: x(:)          ! values of non-fixed parameters
      real(rn),allocatable,intent(out)   :: cx(:,:)          ! values of non-fixed parameters
    end subroutine backsort

    module subroutine expand(pa,nred,x)  ! ,nall)
      use UR_params,    only: rn,two,zero,pi
      use UR_linft,     only: mfix,indfix,xfix
      implicit none
      integer, intent(in)    :: nred
      real(rn),intent(in)    :: pa(nred)
      !integer(4),intent(out) :: nall
      real(rn),allocatable   :: x(:)
    end subroutine expand

    module SUBROUTINE Lsqlin(userfn,t,y,deltay,n,nall,list,pa,covpa,r)
      use UR_params,     only: rn
      implicit none
      EXTERNAL    userfn
      REAL(rn),allocatable,INTENT(IN)     :: t(:)        ! t(n)           ! independent values
      REAL(rn),allocatable,INTENT(in)     :: y(:)        ! y(n)           ! dependent values
      REAL(rn),allocatable,INTENT(IN)     :: deltay(:)   ! deltay(n)      ! uncertainties of c
      INTEGER(4), INTENT(IN)              :: n           ! number of values
      INTEGER(4), INTENT(IN)              :: nall        ! number of fit parameters
      INTEGER(4),allocatable,INTENT(INOUT) :: list(:)     ! list(nall)      ! indicates which parameters are to be fixed
      REAL(rn),allocatable,INTENT(IN OUT) :: pa(:)       ! values of fitted parameters
      REAL(rn),allocatable                :: covpa(:,:)  ! covariance matrix of fitted parameters
      REAL(rn), INTENT(OUT)               :: r           ! chi-square value
    end subroutine Lsqlin

    module SUBROUTINE mtxequ(a,b,n,m)
      use UR_params,     only: rn
      implicit none
      INTEGER, INTENT(IN)        :: n
      INTEGER, INTENT(IN)        :: m
      real(rn), INTENT(IN OUT)   :: a(n, n)
      real(rn), INTENT(OUT)      :: b(n, m)
    end subroutine mtxequ


  end interface

end module Brandt

