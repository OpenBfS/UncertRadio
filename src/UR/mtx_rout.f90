!--------------------------------------------------------------------------------------------------!
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
!--------------------------------------------------------------------------------------------------!

module brandt

    use UR_types, only: rn
    implicit none


    interface
        module subroutine mtxchi(a)
            real(rn), intent(inout)   :: a(:,:)
        end subroutine mtxchi

        module subroutine mtxchl(a,u, posdef)
            real(rn), intent(in)        :: a(:,:)
            real(rn), intent(in out)    :: u(:,:)
            logical, intent(out)        :: posdef
        end subroutine mtxchl

        module subroutine mtxchm(u,a,r,m,n)
            integer, intent(in)        :: m
            integer, intent(in)        :: n
            real(rn), intent(in)       :: u(m,m)
            real(rn), intent(in)       :: a(m,n)
            real(rn), intent(out)      :: r(m,n)
        end subroutine mtxchm

        pure module subroutine mtxgva(v1,v2,c,s)
            real(rn), intent(inout)   :: v1 ! vector components, defining the givens-transformation
            real(rn), intent(inout)   :: v2 !
            real(rn), intent(out)     :: c  ! constants of the transformed vector
            real(rn), intent(out)     :: s  !
        end subroutine mtxgva

        pure module subroutine mtxgvd(v1,v2,c,s)
            real(rn), intent(in)   :: v1 ! vector components, defining the givens-rotation
            real(rn), intent(in)   :: v2 !
            real(rn), intent(out)  :: c    ! constants of the transformed vector
            real(rn), intent(out)  :: s    !
        end subroutine mtxgvd

        elemental module subroutine mtxgvt(z1,z2,c,s)
            real(rn), intent(in out)     :: z1   ! components of a vector to be transformed
            real(rn), intent(in out)     :: z2   !
            real(rn), intent(in)         :: c  !  constants
            real(rn), intent(in)         :: s  !
        end subroutine mtxgvt

        module subroutine mtxhsd(v,up,b,n,lp,l)
            integer, intent(in)          :: n
            real(rn), intent(in)         :: v(n)
            real(rn), intent(out)        :: up
            real(rn), intent(out)        :: b
            integer, intent(in)          :: lp
            integer, intent(in)          :: l
        end subroutine mtxhsd

        module subroutine mtxhst(v,up,b,c,n,lp,l)
            integer, intent(in)          :: n
            real(rn), intent(in)         :: v(n)
            real(rn), intent(in)         :: up
            real(rn), intent(in)         :: b
            real(rn), intent(in out)     :: c(n)
            integer, intent(in)          :: lp
            integer, intent(in)          :: l
        end subroutine mtxhst

        module subroutine mtxlsc(a,b,e,d,x,r,a2,frac,ok)        ! m,n,l,
            implicit none
            real(rn), intent(inout)      :: a(:,:)     ! matrix a(m,n))
            real(rn), intent(inout)      :: b(:)       ! vector b(m))
            real(rn), intent(inout)      :: e(:,:)     ! matrix e(l,n)
            real(rn), intent(in)         :: d(:)       ! vektor d(l))
            real(rn), intent(out)        :: x(:)       ! vector x(n))
            real(rn), intent(out)        :: r          ! chi-square
            real(rn), intent(inout)      :: a2(:,:)    ! working array a2(m,n-l)
            real(rn), intent(in)         :: frac       ! fraction f
            logical, intent(out)         :: ok
        end subroutine mtxlsc

        module subroutine mtxsv1(a,b,d,e)
            real(rn), intent(in out)    :: a(:,:)      ! matrix a(m,n))
            real(rn), intent(in out)    :: b(:,:)      ! matrix b(m,nb)
            real(rn), intent(out)       :: d(:)        ! vector d(n)
            real(rn), intent(out)       :: e(:)        ! vector e(n)
        end subroutine mtxsv1

        module subroutine mtxsv2(a,b,d,e,ok)
            real(rn), intent(in out)    :: a(:,:)        ! matrix a(m,n)
            real(rn), intent(in out)    :: b(:,:)        ! matrix b(m,nb)
            real(rn), intent(in out)    :: d(:)          ! vector d(n)
            real(rn), intent(in out)    :: e(:)          ! vector e(n)
            logical, intent(out)        :: ok
        end subroutine mtxsv2

        module subroutine mtxs21(a,d,e,k)
            real(rn), intent(in out)    :: a(:,:)      ! matrix a(m,n)
            real(rn), intent(in out)    :: d(:)        ! vector d(n)
            real(rn), intent(in out)    :: e(:)        ! vector e(n)
            integer, intent(in)         :: k
        end subroutine mtxs21

        module subroutine mtxs22(b,d,e,k,l)
            real(rn), intent(in out)    :: b(:,:)      ! matrix b(m,nb)
            real(rn), intent(in out)    :: d(:)        ! vector d(n)
            real(rn), intent(in out)    :: e(:)        ! vector e(n)
            integer, intent(in)         :: k
            integer, intent(in)         :: l
        end subroutine mtxs22

        module subroutine mtxs23(a,b,d,e,k,l)
            real(rn), intent(in out)    :: a(:,:)     ! matrix a(m,n)
            real(rn), intent(in out)    :: b(:,:)     ! matrix b(m,nb)
            real(rn), intent(in out)    :: d(:)       ! vector d(n)
            real(rn), intent(in out)    :: e(:)       ! vector e(n)
            integer, intent(in)         :: k
            integer, intent(in)         :: l
        end subroutine mtxs23

        pure module subroutine mtxsv3(a,b,d)
            real(rn), intent(in out)    :: a(:,:)      ! matrix a(m,n)
            real(rn), intent(in out)    :: b(:,:)      ! matrix b(m,nb)
            real(rn), intent(in out)    :: d(:)        ! vector d(n)
        end subroutine mtxsv3

        module subroutine mtxsv4(a,b,d,x,r,frac,bout)
            real(rn), intent(in)        :: a(:,:)      ! matrix a(m,n)
            real(rn), intent(inout)     :: b(:,:)      ! matrix b(m,nb)
            real(rn), intent(in)        :: d(:)        ! vector d(n)
            real(rn), intent(out)       :: x(:,:)      ! matrix x(n,nb)
            real(rn), intent(out)       :: r(:)        ! vector (nb) of squares of the residuals for columns of the matrix a
            real(rn), intent(in)        :: frac
            real(rn), intent(out)       :: bout(:,:)   ! matrix bout(m,nb)
        end subroutine mtxsv4

        module subroutine mtxsvd(a,b,x,r,frac,ok,bout)
            real(rn), intent(in out)    :: a(:,:)      ! matrix a(m,n)
            real(rn), intent(in out)    :: b(:,:)      ! matrix b(m,nb)
            real(rn), intent(in out)    :: x(:,:)      ! matrix x(n,nb)
            real(rn), intent(in out)    :: r(:)        ! vector r(n)
            real(rn), intent(in)        :: frac
            logical, intent(out)        :: ok
            real(rn),intent(out)        :: bout(:,:)   ! matrix bout(m,nb)
        end subroutine mtxsvd

        module real(rn) function qchi2(p,n)
            real(rn), intent(in)      :: p   ! probability
            integer, intent(in)       :: n   ! dof
        end function qchi2

        module real(rn) function szchi2(x,p,n)
            real(rn), intent(in)         :: x
            real(rn), intent(in)         :: p
            integer, intent(in)          :: n
        end function szchi2

        module real(rn) function pchi2(x,n)
            real(rn), intent(in) :: x
            integer, intent(in)  :: n
        end function pchi2

        module real(rn) function scstnr(x)
            real(rn), intent(in) :: x
        end function scstnr

        module real(rn) function sqstnr(p)
            real(rn), intent(in) :: p
        end function sqstnr

        module real(rn) function szstnr(x,p)
            real(rn), intent(in out) :: x
            real(rn), intent(in)     :: p
        end function szstnr

        module real(rn) function pnorm(x, x0, sigma)
            real(rn),intent(in)           :: x
            real(rn),intent(in),optional  :: x0, sigma
        end function pnorm

        module real(rn) function qnorm(p, x0, sigma)
            real(rn),intent(in)           :: p
            real(rn),intent(in),optional  :: x0, sigma
            real(rn)  :: u,xx0,xsigma
        end function qnorm


        module real(rn) function glngam(x)
        real(rn), intent(in)        :: x
        end function glngam

        module subroutine auxzbr(x0,x1,funct,par,npar1,npar2)
            real(rn), intent(in out)    :: x0   !  arguments safely encompassing the root
            real(rn), intent(in out)    :: x1   !
            real(rn), intent(in)        :: par     !  three parameters, on which funct
            integer, intent(in)         :: npar1   !  depends
            integer, intent(in)         :: npar2   !
            real(rn), external          :: funct
        end subroutine auxzbr

        module subroutine auxzfn(x0,x1,xzero,funct,par,npar1,npar2,epsiln)
            real(rn), intent(in out)     :: x0
            real(rn), intent(in out)   :: x1
            real(rn), intent(out)      :: xzero
            real(rn), intent(in)       :: par
            integer, intent(in)        :: npar1
            integer, intent(in)        :: npar2
            real(rn), intent(in)       :: epsiln
            real(rn), external         :: funct
        end subroutine auxzfn

        module real(rn) function gincgm(a,x)
            implicit none

            real(rn), intent(in)  :: a
            real(rn), intent(in)  :: x
        end function gincgm

        module real(rn) function gincbt(aa,bb,xx)
            implicit none
            real(rn), intent(in) :: aa
            real(rn), intent(in) :: bb
            real(rn), intent(in) :: xx
        end function gincbt


        module real(rn) function gbetaf(z,w)
            implicit none
            real(rn),intent(in)   :: z
            real(rn),intent(in)   :: w
        end function gbetaf

        module pure real(rn) function mean(x)
            real(rn), intent(in) :: x(:)
        end function mean

        module pure real(rn) function sd(x)
            real(rn),intent(in)   :: x(:)
        end function sd

        module subroutine matrand(icn,ncr,covxy,muvect,zvect,bvect,kk)
            integer,intent(in)      :: icn    ! rank of the matrix covxy = number of correlating quantities muvect
            integer,intent(in)      :: ncr    ! phsyical dimension of covxy
            real(rn),intent(in)     :: covxy(ncr,ncr)        ! so wurde die matrix angelegt
            real(rn),intent(in)     :: muvect(icn)
            real(rn),intent(in)     :: zvect(icn,1)
            real(rn),intent(out)    :: bvect(icn)
            integer,intent(in)      :: kk              ! mc loop number
        end subroutine matrand

        module subroutine fixprep(xall,nall,list,nred,x)

            implicit none
            integer, intent(in)     :: nall          ! number of all parameters
            real(rn), intent(in)    :: xall(nall)    ! values of non-fixed parameters
            integer, intent(in)     :: list(nall)    !
            integer, intent(out)    :: nred          !
            real(rn), intent(out)   :: x(:)          ! values of non-fixed parameters
        end subroutine fixprep

        module subroutine backsort(xred, cxred, nred, x,cx)

            implicit none
            real(rn), intent(in)    :: xred(:)       ! values of non-fixed parameters
            real(rn), intent(in)    :: cxred(:,:)    ! values of non-fixed parameters
            integer, intent(in)     :: nred          !
            real(rn), intent(out)   :: x(:)          ! values of non-fixed parameters
            real(rn), intent(out)   :: cx(:,:)          ! values of non-fixed parameters
        end subroutine backsort

        module subroutine expand(pa,nred,x)  ! ,nall)

            implicit none
            integer, intent(in)    :: nred
            real(rn),intent(in)    :: pa(nred)
            real(rn),allocatable   :: x(:)
        end subroutine expand

        module subroutine lsqlin(userfn,t,y,deltay,n,nall,list,pa,covpa,r)
            implicit none
            external    userfn
            real(rn),allocatable,intent(in)     :: t(:)        ! t(n)           ! independent values
            real(rn),allocatable,intent(in)     :: y(:)        ! y(n)           ! dependent values
            real(rn),allocatable,intent(in)     :: deltay(:)   ! deltay(n)      ! uncertainties of c
            integer, intent(in)                 :: n           ! number of values
            integer, intent(in)                 :: nall        ! number of fit parameters
            integer,allocatable,intent(inout) :: list(:)     ! list(nall)      ! indicates which parameters are to be fixed
            real(rn),allocatable,intent(in out) :: pa(:)       ! values of fitted parameters
            real(rn),allocatable                :: covpa(:,:)  ! covariance matrix of fitted parameters
            real(rn), intent(out)               :: r           ! chi-square value
        end subroutine lsqlin

        module subroutine mtxequ(a, b, n, m)
            implicit none
            integer, intent(in)        :: n
            integer, intent(in)        :: m
            real(rn), intent(in out)   :: a(n, n)
            real(rn), intent(out)      :: b(n, m)
        end subroutine mtxequ

        module subroutine MulNormPrep(C, DPLUS, N)
            implicit none
            integer, intent(in)        :: N
            real(rn), intent(in)       :: C(N,N)
            real(rn), intent(inout)    :: DPLUS(N,N)
        end subroutine MulNormPrep

    end interface

end module brandt
