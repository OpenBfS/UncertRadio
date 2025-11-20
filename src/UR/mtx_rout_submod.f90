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
submodule (Brandt) Brandta
    use Num1, only: matwrite
    use UR_params, only: EPS1MIN, ONE, ZERO, TWO, HALF, PI

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


    ! #######################################################################

    module subroutine mtxchi(a)

        ! from Datan library, modified by GK
        ! this routine inverts a (n x n) matrix by Cholesky decomposition
        use num1,        only: matwrite

        implicit none

        real(rn), intent(inout)        :: a(:,:)

        integer                        :: n, info
        logical                        :: posdef
        integer, dimension(size(a,1))  :: ipiv   ! pivot indices
        real(rn), dimension(size(a,1)) :: work   ! work array for LAPACK
        !---------------------------------------------------------------------------------
        n = ubound(a, dim=1)
        !---------------------------------------------------------------------------------

        call DGETRF(n, n, a, n, ipiv, info)

        if (info /= 0) then
            !    stop 'Matrix is numerically singular!'
           posdef = .true.
        end if

        ! DGETRI computes the inverse of a matrix using the LU factorization
        ! computed by DGETRF.
        call DGETRI(n, a, n, ipiv, work, n, info)

        if (info /= 0) then
            !    stop 'Matrix inversion failed!'
           posdef = .true.
        end if

    end subroutine mtxchi

    !#######################################################################

    module subroutine mtxchl(a, u, posdef)

        ! from datan library, modified by gk
        ! this routine performs a cholesky decomposition for a positive definite
        ! symmetric matrix a and returns the upper triangular matrix u.
        ! The Cholesky decomposition requires a symmetric matrix!
        !------------------------------------------------------------------------------------------!
        implicit none

        real(rn), intent(in)         :: a(:, :)
        real(rn), intent(inout)      :: u(:, :)
        logical, intent(out)         :: posdef

        integer       :: j, k, n, info
        !------------------------------------------------------------------------------------------!

        n = ubound(a, dim=1)

        posdef = .true.
        u = a

        call DPOTRF('U', n, u, n, info)

        if (info /= 0) then
            ! Handle non-positive definite matrix
            posdef = .false.
            ! if(use_WTLS) cofact = ONE - EPS1MIN
        end if

        ! set the lower triangular part of the matrix to zero as DPOTRF is not
        ! updating these parts of the matrix

        do k = 1, n
            do j = k + 1, n
                u(j, k) = ZERO
            end do
        end do

    end subroutine mtxchl

    !----------------------------------------------------------------------------------------------!

    pure module subroutine mtxgva(v1,v2,c,s)

        ! from Datan library, modified by GK
        ! this routine defines a Givens-transformation and applies it to
        ! directly to the defining vector.

        implicit none

        real(rn), intent(inout)   :: v1 ! vector components, defining the Givens-transformation
        real(rn), intent(inout)   :: v2 !
        real(rn), intent(out)     :: c    ! constants of the transformed vector
        real(rn), intent(out)     :: s    !

        real(rn)   :: a1,a2,w,q

        a1=abs(v1)
        a2=abs(v2)
        if(a1 > a2) then
            w=v2/v1
            q=SQRT(ONE+w*w)
            c=ONE/q
            if(v1 < ZERO) c=-c
            s=c*w
            v1=a1*q
            v2=ZERO
        else
            if(a2 > EPS1MIN) then
                w=v1/v2
                q=SQRT(ONE+w*w)
                s=ONE/q
                if(v2 < ZERO) s=-s
                c=s*w
                v1=a2*q
                v2=ZERO
            else
                c=ONE
                s=ZERO
            end if
        end if
    end subroutine mtxgva

    !----------------------------------------------------------------------------------------------!

    pure module subroutine mtxgvd(v1,v2,c,s)

        ! from Datan library, modified by GK
        ! this routine defines a Givens-rotation.

        implicit none

        real(rn), intent(in)   :: v1 ! vector components, defining the Givens-rotation
        real(rn), intent(in)   :: v2 !
        real(rn), intent(out)  :: c    ! constants of the transformed vector
        real(rn), intent(out)  :: s    !

        real(rn)   :: a1,a2,w,q

        a1=abs(v1)
        a2=abs(v2)
        if(a1 > a2) then
            w=v2/v1
            q=SQRT(ONE+w*w)
            c=ONE/q
            if(v1 < ZERO) c=-c
            s=c*w
        else
            if(a2 > EPS1MIN) then
                w=v1/v2
                q=SQRT(ONE+w*w)
                s=ONE/q
                if(v2 < ZERO) s=-s
                c=s*w
            else
                c=ONE
                s=ZERO
            end if
        end if
    end subroutine mtxgvd

    !----------------------------------------------------------------------------------------------!

    elemental module subroutine mtxgvt(z1,z2,c,s)

        ! from Datan library, modified by GK
        ! this routine applies a Givens-rotation to two components z1 and z2 of a vector.
        implicit none

        real(rn), intent(inout)     :: z1 ! components of a vector to be transformed
        real(rn), intent(inout)     :: z2 !
        real(rn), intent(in)        :: c  !  constants
        real(rn), intent(in)        :: s  !

        real(rn)  :: w

        w = z1*c + z2*s
        z2 = -z1*s + z2*c
        z1 = w
    end subroutine mtxgvt

    !----------------------------------------------------------------------------------------------!

    module subroutine mtxhsd(v,up,b,n,lp,l)

        ! from Datan library, modified by GK
        ! defines a Householder-Transformation; n, Lp, L: vector indices

        implicit none

        integer, intent(in)    :: n
        real(rn), intent(in)   :: v(n)     ! n-vector
        real(rn), intent(out)  :: up
        real(rn), intent(out)  :: b
        integer, intent(in)    :: lp
        integer, intent(in)    :: l

        integer     :: i
        real(rn)    :: c, c1, sum_of_squares, vpprim

        c = abs(v(lp))
        do  i = l, n
            c = max(abs(v(i)), c)
        end do
        if(c <= ZERO) then
            up = ZERO
            b  = ZERO
            return
        end if
        c1=ONE/c
        sum_of_squares = (v(lp)*c1)**TWO
        do  i=l,n
            sum_of_squares = sum_of_squares + (v(i) * c1)**TWO
        end do

        vpprim = c * (-1) * SIGN(SQRT(abs(sum_of_squares)), v(lp))

        up = v(lp) - vpprim
        b = ONE / (vpprim * up)

    end subroutine mtxhsd

    !----------------------------------------------------------------------------------------------!

    module subroutine mtxhst(v,up,b,c,n,lp,l)

        ! from Datan library, modified by GK
        ! Applies a Householder-Transformation to a vector c
        implicit none

        integer, intent(in)       :: n
        real(rn), intent(in)      :: v(n)    ! v, n, Lp, L: as in mtxhsd
        real(rn), intent(in)      :: up      ! up and b: were calculated by mtxhsd
        real(rn), intent(in)      :: b
        real(rn), intent(inout)   :: c(n)    ! vector to be transformed
        integer, intent(in)       :: lp
        integer, intent(in)       :: l
        real(rn)  :: s
        !------------------------------------------------------------------------------------------!

        s = c(lp) * up
        s = s + sum(c(l:n)*v(l:n))
        s = s * b

        c(lp) = c(lp) + s*up

        c(l:n) = c(l:n) + s * v(l:n)

    end subroutine mtxhst

    !----------------------------------------------------------------------------------------------!

    module subroutine mtxlsc(a,b,e,d,x,r,a2,frac,ok)        ! m,n,l,

        ! from Datan library, modified by GK
        ! solves the least squares problem (A*x -b)^2 = min
        ! under the constraint E*x = d

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

        real(rn)    :: rrr(20)
        integer     :: m,n,l
        real(rn), allocatable :: up(:),bb(:), p2(:,:),s(:),v(:),bout(:,:),b3(:,:)
        logical     :: printout

        integer      :: i,j,l2,nminl
        !-----------------------------------------------------------------------
        m = ubound(a,dim=1)
        n = ubound(a,dim=2)
        l = ubound(d,dim=1)
        allocate(up(l),bb(l), p2(n,1),s(n),v(n),bout(n,1),b3(m,1))

        printout = .false.
        ! printout = .TRUE.

        if(printout) then
            write(23,*) 'MTXLSC: m, n, l = ',m,n,l
            call matwrite(a,m,n,23,'(130(es10.2,1x))','MTXLSC: Matrix A:')
            write(23,*) 'MTXLSC, at beginning: Vektor B:'
            write(23,'(130(es10.2,1x))') (b(i),i=1,m)
            write(23,*)
            write(23,*) 'MTXLSC, at beginning: Vektor D:'
            write(23,'(130(es10.2,1x))') (d(i),i=1,l)
            call matwrite(e,l,n,23,'(130(es10.2,1x))','MTXLSC: Matrix e:')
        end if

        x = 0.0_rn

        ! step 1:
        nminl=n-l
        do  i=1,l
            v(1:n) = e(i,1:n)
            call mtxhsd(v,up(i),bb(i),n,i,i+1)
            do  j=i,l
                s(1:n) = e(j,1:n)
                call mtxhst(v,up(i),bb(i),s,n,i,i+1)
                if(j == i .and. n > i) then
                    s(i+1:n) = v(i+1:n)     !xx
                end if
                e(j,1:n) = s(1:n)
            end do
            do  j=1,m
                s(1:n) = a(j,1:n)
                call mtxhst(v,up(i),bb(i),s,n,i,i+1)
                a(j,1:n) = s(1:n)
            end do
        end do
        ! step 2:
        x(1)=d(1)/e(1,1)
        if(l > 1) then
            do  j=2,l
                x(j)=d(j)
                x(j) = x(j) - sum( e(j,1:j-1)*x(1:j-1) )
                x(j)=x(j)/e(j,j)
            end do
        end if
        ! step 3:
        do  j=1,m
            b(j) = b(j) - sum( a(j,1:l)*x(1:l) )
        end do
        ! step 4:
        l2=1
        if(printout) then
            call matwrite(a,m,n,23,'(130(es10.2,1x))','MTXLSC, before call mtxgsm: Matrix A:')
        end if
        ! write(0,'(a,4i5)') ' m,n,nminl,l=',m,n,nminl,l,'  Ubound(a2,1)=',Ubound(a2,dim=1)
        a2(1:m, 1:nminl) = a(1:1-1+m , l+1:l+nminl)

        if(printout) then
            call matwrite(a2,m,n-l,23,'(130(es10.2,1x))','MTXLSC, before call mtxsvd: Matrix A2:')
            write(23,*) 'MTXLSC, before call MTXSVD: Vektor B:'
            write(23,'(130(es10.2,1x))') (b(i),i=1,m)
            write(23,*)
        end if

        b3(1:m,1) = b(1:m)
        call mtxsvd(a2,b3,p2,rrr,frac,ok,bout)
        r = rrr(1)
        b(1:m) = b3(1:m,1)

        if(ok) then
            x(L+1:L+nminl) = p2(1:nminl,1)
            do  i=l,1,-1
                v(1:n) = e(i,1:n)
                call mtxhst(v,up(i),bb(i),x,n,i,i+1)
            end do
        end if
    end subroutine mtxlsc

    !----------------------------------------------------------------------------------------------!

    module subroutine mtxsv1(a,b,d,e)

        ! from Datan library, modified by GK
        ! performs the bi-diagonalization of an (m x n) matrix A, such that
        !  A = Q C transpose(H), with C bi-diagonal

        implicit none

        real(rn), intent(inout) :: a(:,:)      ! matrix a(m,n))
        real(rn), intent(inout) :: b(:,:)      ! matrix b(m,nb)
        real(rn), intent(out)   :: d(:)        ! vector d(n)
        real(rn), intent(out)   :: e(:)        ! vector e(n)

        integer              :: m,n,nb
        real(rn),allocatable :: v(:),s(:),ups(:),bbs(:)
        integer              :: i,j,k
        real(rn)             :: bb,up

        m = ubound(a,dim=1)
        n = ubound(a,dim=2)
        nb = ubound(b,dim=2)

        allocate(v(m),s(m),ups(n),bbs(n))
        ! call matwrite(A,m,n,m,n,66,'(100(es11.3,1x))',' SV1-in: Matrix A')
        do  i=1,n
            ! set up Householder Transformation Q(I)
            if(i < n .OR. m > n) then
                v(1:m) = a(1:m,i)
                call mtxhsd(v,up,bb,m,i,i+1)
                ! apply Q(I) to A
                do  j=i,n
                    s(1:m) = a(1:m,j)
                    call mtxhst(v,up,bb,s,m,i,i+1)
                    a(1:m,j) = s(1:m)
                end do
                ! apply Q(I) to B
                do  k=1,nb
                    s(1:m) = b(1:m,k)
                    call mtxhst(v,up,bb,s,m,i,i+1)
                    b(1:m,k) = s(1:m)
                end do
            end if
            if(i < n-1) then
                ! set up Householder Transformation H(I)
                v(1:n) = a(i,1:n)
                call mtxhsd(v,up,bb,n,i+1,i+2)
                ! save H(I)
                ups(i) = up
                bbs(i) = bb
                ! apply H(I) to A
                do  j=i,m
                    s(1:n) = a(j,1:n)
                    call mtxhst(v,up,bb,s,n,i+1,i+2)
                    ! save elements I+2,... in row J of matrix A
                    if (j == i) then
                        s(i+2:n) = v(i+2:n)
                    end if
                    a(j,1:n) = s(1:n)
                end do
            end if
        end do
        ! copy diagonal of transformed matrix A to D
        ! and upper parallel A to E
        if(n > 1) then
            do  i=2,n
                d(i) = a(i,i)
                e(i) = a(i-1,i)
            end do
        end if
        d(1)=a(1,1)
        e(1)=ZERO
        ! construct product matrix H=H(2)*H(3)*...*H(N), H(N)=I
        do  i=n,1,-1
            if(i <= n-1) v(1:n) = a(i,1:n)
            a(i,1:n) = ZERO
            a(i,i)=ONE
            if(i < n-1) then
                do  k=i,n
                    s(1:m) = a(1:m,k)
                    call mtxhst(v,ups(i),bbs(i),s,n,i+1,i+2)
                    a(1:m,k) = s(1:m)
                end do
            end if
        end do
    end subroutine mtxsv1

    !----------------------------------------------------------------------------------------------!

    module subroutine mtxsv2(a,b,d,e,ok)

        ! from Datan library, modified by GK
        ! it diagonalizes iteratively the bi-diagonal matrix C, such that
        ! S' = transpose(U') C V'  is diagonal
        ! modification (GK): goto eliminated

        implicit none

        real(rn), intent(in out)    :: a(:,:)        ! matrix a(m,n)
        real(rn), intent(in out)    :: b(:,:)        ! matrix b(m,nb)
        real(rn), intent(in out)    :: d(:)          ! vector d(n)
        real(rn), intent(in out)    :: e(:)          ! vector e(n)
        logical, intent(out)        :: ok

        logical  :: elzero
        integer  :: niterm,niter,i,k,ll,l, m,n,nb
        real(rn) :: bmx

        m = ubound(a,dim=1)
        n = ubound(a,dim=2)
        nb = ubound(b,dim=2)
        elzero = .false.

        ok=.true.
        niterm=10*n
        niterm = 6*n
        niter=0
        bmx=d(1)
        if(n > 1) then
            do i=2,n
                bmx=MAX(abs(d(i))+abs(e(i)),bmx)
            end do
        end if
        do k=n,1,-1
            do
                if(k /= 1) then
                    if(abs((bmx+d(k))-bmx) < EPS1MIN) then
                        ! write(166,*) 'MTXSV2 (a): Element D(',k,') = 0._rn'
                        ! Since D(K).EQ.0. perform Givens transform with result E(K)=0.
                        call mtxs21(a,d,e,k)
                    end if
                    ! Find L (2. LE. L .LE. K) so that either E(L)=0. or D(L-1)=0.
                    ! In the latter case transform E(L) to zero. In both cases the
                    ! matrix splits and the bottom right minor begins with row L.
                    ! If no such L is found set L=1
                    do  ll=k,1,-1
                        l=ll
                        if(l == 1) then
                            elzero=.false.
                            exit
                        else if( abs( (bmx-e(l))-bmx) < EPS1MIN) then
                            elzero=.true.
                            exit
                        else if(abs( (bmx+d(l-1))-bmx) < EPS1MIN) then
                            elzero=.false.
                        end if
                    end do
                    if (l > 1 .and. .NOT. elzero) then
                        call mtxs22(b,d,e,k,l)
                    end if
                    if(l /= k) then
                        ! one more QR pass with order K
                        call mtxs23(a,b,d,e,k,l)
                        niter=niter+1
                        if(niter <= niterm) cycle

                        ! set flag indicating non-convergence
                        ok=.false.
                        if(.not.ok) then
                            ! write(166,*) 'MTXSV2 (d): ok=.F.  !   niter=',niter,'  niterm=',niterm
                            ! write(166,*) '     m,n,nb=',m,n,nb
                        end if
                    end if
                end if

                if(d(k) < ZERO) then
                    ! for negative singular values perform change of sign
                    d(k)=-d(k)
                    a(1:n,k) = -a(1:n,k)
                end if
                exit
            end do
            ! order is decreased by one in next pass
        end do
    end subroutine mtxsv2
    !#######################################################################

    module subroutine mtxs21(a,d,e,k)

        ! from Datan library, modified by GK
        ! subprogram of mtxsv2

        implicit none

        real(rn), intent(in out)  :: a(:,:)      ! matrix a(m,n)
        real(rn), intent(in out)  :: d(:)        ! vector d(n)
        real(rn), intent(in out)  :: e(:)        ! vector e(n)
        integer, intent(in)       :: k

        integer  :: i,j,m,n
        real(rn) :: sn,cs,h

        m = ubound(a,dim=1)
        n = ubound(a,dim=2)

        do  i = k-1, 1, -1
            if(i == k-1) then
                call mtxgva(d(i),e(i+1),cs,sn)
            else
                call mtxgva(d(i),h,cs,sn)
            end if
            if(i > 1) then
                h=ZERO
                call mtxgvt(e(i),h,cs,sn)
            end if
            do  j =  1,n
                call mtxgvt(a(j,i),a(j,k),cs,sn)
            end do
        end do
    end subroutine mtxs21

!#######################################################################

    module subroutine mtxs22(b,d,e,k,l)

        ! from Datan library, modified by GK
        ! subprogram of mtxsv2

        implicit none

        real(rn), intent(inout) :: b(:,:)      ! matrix b(m,nb)
        real(rn), intent(inout) :: d(:)        ! vector d(n)
        real(rn), intent(inout) :: e(:)        ! vector e(n)
        integer, intent(in)     :: k
        integer, intent(in)     :: l

        integer  :: i,j, m,nb,n
        real(rn) :: sn,cs,h

        m = ubound(b,dim=1)
        nb = ubound(b,dim=2)
        n = ubound(d,dim=1)

        do  i=l,k
            if(i == l) then
                call mtxgva(d(i),e(i),cs,sn)
            else
                call mtxgva(d(i),h,cs,sn)
            end if
            if(i < k) then
                h=ZERO
                call mtxgvt(e(i+1),h,cs,sn)
            end if
            do  j=1,nb
                call mtxgvt(cs,sn,b(i,j),b(l-1,j))
            end do
        end do
    end subroutine mtxs22

!#######################################################################

    module subroutine mtxs23(a,b,d,e,k,l)
        ! from Datan library, modified by GK
        ! subprogram of mtxsv2

        implicit none

        real(rn), intent(in out) :: a(:,:)     ! matrix a(m,n)
        real(rn), intent(in out) :: b(:,:)     ! matrix b(m,nb)
        real(rn), intent(in out) :: d(:)       ! vector d(n)
        real(rn), intent(in out) :: e(:)       ! vector e(n)
        integer, intent(in)      :: k
        integer, intent(in)      :: l

        integer          :: i,j, m,n,nb
        real(rn)         :: f,g,t,sn,cs,h

        m = ubound(a,dim=1)
        n = ubound(a,dim=2)
        nb = ubound(b,dim=2)

        ! Determine shift parameter
        f=((d(k-1)-d(k))*(d(k-1)+d(k))+(e(k-1)-e(k))*(e(k-1)+e(k)))/  &
            (TWO*e(k)*d(k-1))
        if(abs(f) > 1.E+10_rn) then
            g=abs(f)
        else
            g=SQRT(ONE+f*f)
        end if
        if(f >= ZERO) then
            t=f+g
        else
            t=f-g
        end if
        f=((d(l)-d(k))*(d(l)+d(k))+e(k)*(d(k-1)/t-e(k)))/d(l)
        do  i = l , k-1
            if(i == l) then
                ! Define R(L)
                call mtxgvd(f,e(i+1),cs,sn)
            else
                ! Define R(I) , I.NE.L
                call mtxgva(e(i),h,cs,sn)
            end if
            call mtxgvt(d(i),e(i+1),cs,sn)
            h=ZERO
            call mtxgvt(h,d(i+1),cs,sn)
            do  j =  1, n
                call mtxgvt(a(j,i),a(j,i+1),cs,sn)
            end do
            ! Define T(I)
            call mtxgva(d(i),h,cs,sn)
            call mtxgvt(e(i+1),d(i+1),cs,sn)
            if(i < k-1) then
                h=ZERO
                call mtxgvt(h,e(i+2),cs,sn)
            end if
            do  j =  1, nb
                call mtxgvt(b(i,j),b(i+1,j),cs,sn)
            end do
        end do
    end subroutine mtxs23

    !----------------------------------------------------------------------------------------------!

    pure module subroutine mtxsv3(a,b,d)

        ! from Datan library, modified by GK
        ! performs a permutational transformation of the diagonal matrix S',
        ! such that S'' = transpoes(U'') S' V'' has not-ascendingly ordered
        ! diagonal elements
        ! modification (GK): goto eliminated

        implicit none

        real(rn), intent(in out)    :: a(:,:)      ! matrix a(m,n)
        real(rn), intent(in out)    :: b(:,:)      ! matrix b(m,nb)
        real(rn), intent(in out)    :: d(:)        ! vector d(n)

        integer     :: i,j,k, nfd, m,n,nb
        real(rn)    :: t

        m = ubound(a,dim=1)
        n = ubound(a,dim=2)
        nb = ubound(b,dim=2)

        ! Order singular values
        if(n <= 1) return
        do
            nfd = 0
            do  i=2,n
                if(d(i) > d(i-1)) then
                    nfd = 1
                    exit
                end if
            end do
            if(nfd == 0) return

            do  i =2,n
                t=d(i-1)
                k=i-1
                do  j = i , n
                    if(t < d(j)) then
                        t=d(j)
                        k=j
                    end if
                end do
                if(k /= i-1) then
                    ! perform permutation on singular values
                    d(k)=d(i-1)
                    d(i-1)=t
                    ! perform permutation on matrix A
                    do  j =  1, n
                        t=a(j,k)
                        a(j,k)=a(j,i-1)
                        a(j,i-1)=t
                    end do
                    ! perform permutation on matrix B
                    do  j =  1, nb
                        t=b(k,j)
                        b(k,j)=b(i-1,j)
                        b(i-1,j)=t
                    end do
                end if
            end do
        end do
    end subroutine mtxsv3

!#######################################################################

    module subroutine mtxsv4(a,b,d,x,r,frac,bout)

        ! from Datan library, modified by GK
        ! performs the singular value analysis

        use UR_Linft, only: ycopy,use_PMLE,uycopy,penalty_factor,use_constr,pcstr,upcstr,kconstr
        implicit none

        real(rn), intent(in)        :: a(:,:)      ! matrix a(m,n)
        real(rn), intent(inout)     :: b(:,:)      ! matrix b(m,nb)
        real(rn), intent(in)        :: d(:)        ! vector d(n)
        real(rn), intent(out)       :: x(:,:)      ! matrix x(n,nb)
        real(rn), intent(out)       :: r(:)        ! vector (nb) of squares of the residuals for columns of the Matrix A
        real(rn), intent(in)        :: frac
        real(rn), intent(out)       :: bout(:,:)   ! matrix bout(m,nb)

        real(rn), parameter :: epsiln=5._rn*EPS1MIN

        integer  :: i,j,k,kk, m,n,nb
        real(rn) :: fract,sinmax,sinmin,s1,yfi

        m = ubound(a,dim=1)
        n = ubound(a,dim=2)
        nb = ubound(b,dim=2)

        r = ZERO

        fract=abs(frac)
        if(fract < epsiln) fract=epsiln
        sinmax=ZERO
        sinmax = max(sinmax, maxval(d))
        sinmin=sinmax*fract
        kk=n
        do  i=1,n
            if(d(i) <= sinmin) then
                kk = i-1
                exit
            end if
        end do
        do  i = 1, m
            if(i <= kk) then
                s1 = ONE/d(i)
                b(i,1:nb) = b(i,1:nb) *s1
            else
                do  j=1,nb
                    if(.not.use_PMLE) then
                        if(i == kk+1) then
                            r(j) = b(i,j)**TWO
                        else
                            r(j) = r(j) + b(i,j)**TWO
                        end if
                    else
                        yfi = b(i,j)*uycopy(i) + ycopy(i)      ! function value
                        if(i == kk+1) then
                            r(j) = 2._rn*( b(i,j)*uycopy(i) + ycopy(i)*log(yfi/max(ycopy(i),0.5_rn)) )
                        else
                            r(j) = r(j) + 2._rn*( b(i,j)*uycopy(i) + ycopy(i)*log(yfi/max(ycopy(i),0.5_rn)) )
                        end if
                    end if
                    if(i <= n) b(i,j) = ZERO
                end do
                if(use_PMLE .and. use_constr) then
                    do k=1,n
                        if(kconstr(j) == 1) then
                            r(1) = r(1) + penalty_factor*( (x(k,nb) - pcstr(k))/upcstr(k) )**TWO
                        end if
                    end do
                end if

            end if
        end do
        do  i=1,n
            do  j=1,nb
                x(i,j)= sum(a(i,1:n)*b(1:n,j))
            end do
        end do
        do i=1,m
            bout(i,1:nb) = b(i,1:nb)
        end do
        n=kk

    end subroutine mtxsv4

    !----------------------------------------------------------------------------------------------!

    module subroutine mtxsvd(a,b,x,r,frac,ok,bout)

        ! from Datan library, modified by GK
        ! performs a singular value decomposition and - analysis of an (m x n) matrix a

        implicit none

        real(rn), intent(inout)    :: a(:,:)      ! matrix a(m,n)
        real(rn), intent(inout)    :: b(:,:)      ! matrix b(m,nb)
        real(rn), intent(inout)    :: x(:,:)      ! matrix x(n,nb)
        real(rn), intent(inout)    :: r(:)        ! vector r(nb)
        real(rn), intent(in)       :: frac
        logical, intent(out)       :: ok
        real(rn),intent(out)       :: bout(:,:)   ! matrix bout(m,nb)

        integer               :: i, m,n,nb
        real(rn)              :: chi2
        real(rn), allocatable :: d(:),e(:),bin4(:,:)
        real(rn), allocatable :: Cmat(:,:),Hmat(:,:)
        logical               :: test
        !------------------------------------------------------------------------------------------!

        m = ubound(a,dim=1)
        n = ubound(a,dim=2)
        nb = ubound(b,dim=2)

        test = .false.
        ! test = .true.

        allocate(d(n),e(n),bin4(m,nb))
        if(test) then
            allocate(Cmat(n,n),Hmat(n,n))
            Cmat = ZERO;   Hmat = ZERO
        end if

        ! STEP 1: Bidiagonalisation of A
        call mtxsv1(a,b,d,e)
        if(test) then
            do i=1,n
                Cmat(i,i) = d(i)
                if(i < n) Cmat(i,i+1) = e(i+1)
            end do
            call matwrite(Cmat,n,n,66,'(150es11.3)','mtxsvd, step 1:  bidiagonal matrix Cmat:')
            call matwrite(A,n,n,66,'(150es11.3)','mtxsvd, step 1:  matrix Hmat (=A):')
        end if

        ! STEP 2: Diagonalisation of bidiagonal matrix
        call mtxsv2(a,b,d,e,ok)
        if(ok .and. test) then
            Cmat = ZERO
            do i=1,n
                Cmat(i,i) = d(i)
            end do
            call matwrite(Cmat,n,n,66,'(150es11.3)','mtxsvd, step 2  bidiagonal matrix Cmat:')
            call matwrite(A,n,n,66,'(150es11.3)',"mtxsvd, step 2:  matrix Hmat x V' (=A):")
        end if

        ! STEP 3: Order singular values and perform  permutations
        call mtxsv3(a,b,d)
        if(ok .and. test) then
            write(66,'(a,10(es12.5,1x))') "mtxsvd, step 3: Vector d of diagonal elements of S'':",d(1:n)
        end if

        ! STEP 4: Singular value analysis
        if(ok .and. test) then
            bin4(1:m,1) = b(1:m,1)
        end if
        call mtxsv4(a,b,d,x,r,frac,bout)
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

    end subroutine mtxsvd

    !----------------------------------------------------------------------------------------------!

    module subroutine fixprep(xall,nall,list,nred,x)         ! ,mfix,indfix,xfix)
        use UR_Linft,     only: mfix,indfix,xfix,kpt
        implicit none

        integer, intent(in)   :: nall        ! number of all parameters
        real(rn), intent(in)  :: xall(nall)  ! values of all parameters
        integer, intent(in)   :: list(nall)  !
        integer, intent(out)  :: nred        ! number of non-fixed parameters
        real(rn), intent(out) :: x(:)        ! values of non-fixed parameters

        integer            :: i,n,mm

        x(:) = ZERO
        mfix = 0
        nred = 0
        do i=1,nall
            if(list(i) == 1) nred = nred + 1
        end do
        mfix = nall - nred
        if(allocated(indfix)) deallocate(indfix)
        if(allocated(xfix)) deallocate(xfix)

        if(allocated(kpt)) deallocate(kpt)
        allocate(kpt(nred), indfix(mfix), xfix(mfix))

        !write(66,*) 'fixprep: xall=',sngl(xall)
        !write(66,*) 'fixprep: list=',int(list,2)

        indfix = 0
        xfix = ZERO
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

    !----------------------------------------------------------------------------------------------!

    module subroutine backsort(xred, cxred, nred, x, cx)

        use UR_Linft,     only: mfix, indfix, xfix, kpt
        implicit none

        real(rn), intent(in)  :: xred(:)       ! values of non-fixed parameters
        real(rn), intent(in)  :: cxred(:,:)    ! values of non-fixed parameters
        integer, intent(in)   :: nred          ! number of non-fixed paramaters
        real(rn), intent(out) :: x(:)          ! values of non-fixed parameters
        real(rn), intent(out) :: cx(:,:)       ! values of non-fixed parameters

        integer         :: i,j,k,nfd,nr
        integer, allocatable :: list(:)
        !------------------------------------------------------------------------------------------!
        nr = nred + mfix
        allocate(list(nr))
        cx = ZERO

        ! a)   the array of fit parameters:
        x = ZERO
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
        cx = ZERO
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

    module subroutine expand(pa,nred,x)

        use UR_linft,     only: mfix, indfix, xfix
        implicit none

        integer, intent(in)    :: nred
        real(rn), intent(in)   :: pa(nred)
        real(rn), allocatable  :: x(:)

        integer                :: i,j,nall

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

    module subroutine Lsqlin(userfn,t,y,deltay,n,nall,list,pa,covpa,r)
        ! linear weighted Least squares fit of a linear function to a curve
        ! of measured values (t,y) using singular value decomposiition.
        ! From Datan library, modified by GK

        use UR_Linft,      only: use_PLSQ,use_PMLE

        implicit none

        external    userfn

        real(rn),allocatable,intent(in)      :: t(:)        ! t(n)           ! independent values
        real(rn),allocatable,intent(in)      :: y(:)        ! y(n)           ! dependent values
        real(rn),allocatable,intent(in)      :: deltay(:)   ! deltay(n)      ! uncertainties of c
        integer, intent(in)                  :: n           ! number of values
        integer, intent(in)                  :: nall        ! number of fit parameters
        integer, allocatable,intent(inout)   :: list(:)     ! list(nall)      ! indicates which parameters are to be fixed
        real(rn),allocatable,intent(in out)  :: pa(:)       ! values of fitted parameters
        real(rn),allocatable                 :: covpa(:,:)  ! covariance matrix of fitted parameters
        real(rn), intent(out)                :: r           ! chi-square value

        logical               :: ok, upSV
        integer               :: i,k,nr,nred,j,nrep
        real(rn)              :: rr(1),c(n),fx,yfit(n)
        real(rn),allocatable  :: x(:),cx(:,:),a(:,:),afunc(:),bout(:,:), cc(:,:),xx(:,:)

        !-----------------------------------------------------------------------
        upSV = use_PMLE         !- 18.6.2024
        use_PMLE= .false.       !-

        if(minval(list) == 0) then
            nred = sum(list)
            allocate(x(nred))
            ! write(66,*) 'nall=',int(nall,2),' pa=',sngl(pa)
            ! write(66,*) 'list=',int(list,2),' nall=',int(nall,2)
            call fixprep(pa,nall,list,nred,x)    ! ,mfix,indfix,xfix)
            ! write(66,*) 'nred=',int(nred,2),' x=',sngl(x)
        else
            nred = nall
            allocate(x(nred))
            x(1:nall) = pa(1:nall)
        end if
        allocate(a(n,nred),cx(nred,nred),afunc(nred),bout(n,1))
        allocate(cc(1:n,1),xx(1:nred,1))
        nrep = 0

35      continue

        nr = nred
        ! Build the matrix A:
        do i=1,n
            call userfn(t(i),afunc,nred)
            A(i,1:nred) = afunc(1:nred)
        end do

        ! compute matrix A' from A
        do i=1,n
            if(nrep > 0) then
                yfit(i) = ZERO
                do k=1,nred
                    yfit(i) = yfit(i) + x(k)*a(i,k)
                end do
            end if

            do k=1,nr
                if(.not.use_PLSQ .or. nrep == 0) then
                    a(i,k) = a(i,k)/deltay(i)
                else
                    a(i,k) = a(i,k)/sqrt(yfit(i))
                end if
            end do
        end do

        ! Set up vector C'
        do i=1,n
            if(.not.use_PLSQ) then
                c(i) = y(i)/deltay(i)
            else
                if(nrep > 0) then
                    c(i) = y(i)/sqrt(yfit(i))
                else
                    c(i) = y(i)/deltay(i)
                end if
            end if
        end do

        ! Set up matrix GX
        cx = matmul(transpose(a),a)
        ! Determine vector X of unknowns
        cc(1:n,1) = c(1:n)
        xx(1:nred,1) = x(1:nred)
        call mtxsvd(a,cc,xx,rr,0._rn,ok,bout)
        c(1:n) = cc(1:n,1)
        x(1:nred) = xx(1:nred,1)
        if(ok) then
            ! Determine covariance matrix CX by inverting CX
            call mtxchi(cx)
        end if

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

        else
            pa(1:nall) = x(1:nall)
            covpa(1:nall,1:nall) = cx(1:nall,1:nall)
            ! covpa = cx
        end if
        !  write(66,*) 'nall=',int(nall,2),' pa=',sngl(pa),'  minval(list)=',minval(list)
        r = ZERO
        do i=1,n
            fx = ZERO
            call userfn(t(i),afunc,nred)
            ! do j=1,nall
            do j=1,nred
                fx = fx + afunc(j)*x(j)
            end do
            r = r + (fx - y(i))**TWO/deltay(i)**TWO
        end do
        use_PMLE = upSV        !- 18.6.2024

    end subroutine Lsqlin

    !################################################################################

    module real(rn) function qchi2(p,n)

        ! from Datan library, modified by GK
        ! calculates the p quantile of the chi-square distribution

        implicit none

        real(rn), intent(in)      :: p   ! probability
        integer   , intent(in)    :: n   ! dof

        real(rn), parameter :: big=1.E+10_rn
        real(rn)            :: epsiln,x0,x1,xzero

    ! EXTERNAL szchi2
        if(rn == 8) epsiln = 1.E-12_rn
        if(rn == 10) epsiln = 1.E-15_rn

    ! boundary of range
        if(p >= ONE) qchi2 = big
        if(p <= ZERO) qchi2 = ZERO

    ! normal range
        if(p < ONE .and. p > ZERO) then
            x1 = real(n,rn)
            x0 = half*x1
            call auxzbr(x0,x1,szchi2,p,n,0)
            call auxzfn(x0,x1,xzero,szchi2,p,n,0,epsiln)
            qchi2 = xzero
        end if

    end function qchi2
!-----------------------------------------------------------------

!############################################################################

    module real(rn) function szchi2(x,p,n)

        ! from datan library, modified by gk
        ! returns p minus cumulative chisquared distribution of (x,n)

        implicit none

        real(rn), intent(in)         :: x
        real(rn), intent(in)         :: p
        integer, intent(in)          :: n
        !-----------------------------------------------------------
        szchi2 = p - pchi2(x,n)

    end function szchi2

!############################################################################

    module real(rn) function pchi2(x,n)

        ! from Datan library, modified by GK
        ! distribution function of the chis-square distribution

        IMPLICIT none

        real(rn), intent(in)         :: x
        integer, intent(in)          :: n

        real(rn)                     :: a
        !-----------------------------------------------------------------------------
        a = half * n
        pchi2 = gincgm(a, half*x)

    end function pchi2

!#######################################################################

module real(rn) function SCSTNR(X)

 ! from Datan library, modified by GK
 ! distribution function of the standard normal distribution

implicit none

real(rn), intent(in)     :: x

real(rn)               :: arg,s,f
!-------------------------------------------
arg = Half*(x**TWO)
s = ONE
if(x < ZERO) s = -ONE
f = gincgm(Half,Arg)
SCSTNR = Half*(ONE+s*f)

end function SCSTNR

!#######################################################################

    module real(rn) function sqstnr(p)

        ! from Datan library, modified by GK
        ! calculates the quantile of the standard normal distribution
        ! associated with probability p

        implicit none

        real(rn), intent(in)       :: p

        real(rn), parameter :: big=1.E10_rn

        real(rn)           :: x0,x1,epsiln,xzero

        if(rn == 8) epsiln = 1E-8_rn
        if(rn == 10) epsiln = 1E-18_rn

        ! boundary of range
        if(p >= ONE) sqstnr=big
        if(p <= ZERO) sqstnr=-big
        ! normal range
        if(p < ONE .and. p > ZERO) then
            x0=ZERO
            x1=0.1_rn
            call auxzbr(x0,x1,szstnr,p,0,0)
            call auxzfn(x0,x1,xzero,szstnr,p,0,0,epsiln)
            sqstnr=xzero
        end if
    end function sqstnr

    !-----------------------------------------------------------------------

    module real(rn) function szstnr(x,p)

    ! from Datan library, modified by GK
    ! returns P minus cumulative standardized normal of X

    implicit none

    real(rn), intent(in out)    :: x
    real(rn), intent(in)        :: p

    !------------------------------------------------------------------
    szstnr = p - scstnr(x)

    end function szstnr

    !#######################################################################

    module real(rn) function pnorm(x, x0, sigma)

        ! calculates the probability (normal distribution) for a quantile value x

        implicit none

        real(rn),intent(in)           :: x
        real(rn),intent(in),optional  :: x0, sigma

        real(rn)       :: u,xx0,xsigma

        if(.not.present(x0) .and. .not.present(sigma)) then
            xx0 = ZERO
            xsigma = ONE
        else
            xx0 = x0
            xsigma = sigma
        end if

        ! Distribution function (integral) of the Normal distribution N(x0,sigma):
        u = (x - xx0) / xsigma
        if(abs(u) < 15._rn) then
            pnorm = scstnr(u)
        else
            if(x > xx0) pnorm = ONE
            if(x < xx0) pnorm = ZERO
        end if

    end function pnorm

    !#######################################################################

    module real(rn) function qnorm(p, x0, sigma)

        ! calculates the quantile for probability p (normal distribution)

        implicit none

        real(rn), intent(in)           :: p
        real(rn), intent(in), optional :: x0, sigma
        real(rn)                       :: u,xx0,xsigma

        if(.not.present(x0) .and. .not.present(sigma)) then
            xx0 = ZERO
            xsigma = ONE
        else
            xx0 = x0
            xsigma = sigma
        end if

        ! Quantile of the normal distribution N(x0,sigma):

        u = sqstnr(p)
        qnorm = u*xsigma + xx0

    end function qnorm

!#######################################################################

    module real(rn) function glngam(x)

        ! from Datan library, modified by GK
        ! calculates the natural logarithm of the gamma function

        implicit none

        real(rn), intent(in)        :: x

        logical :: reflec
        real(rn), parameter :: rtwopi=sqrt(2._rn*PI)
        real(rn)            :: c(6)
        integer             :: i
        real(rn)            :: xx,xh,xgh,s,anum,g

        !DATA c/76.18009173_rn,-86.50532033_rn,24.01409822_rn,  &
        !    -1.231739516_rn,0.120858003E-2_rn,-0.536382E-5_rn/

        c = (/76.18009172947146_rn, -86.50532032941677_rn, &
            24.01409824083091_rn, -1.231739572450155_rn, &
            .1208650973866179E-2_rn, -.5395239384953E-5_rn /)

        if(x >= one) then
            reflec = .false.
            xx = x-one
        else
            reflec = .true.
            xx = one-x
        end if
        xh = xx+half
        xgh = xx+5.5_rn
        s = one
        anum = xx
        do i=1,6
            anum = anum+one
            s = s + c(i)/anum
        end do
        s = s * rtwopi
        g = xh*LOG(xgh)+LOG(s)-xgh
        if (reflec) then
            glngam = LOG(PI*xx) - g - LOG(SIN(PI*xx))
        else
            glngam = g
        end if

    end function glngam

!#######################################################################

    module subroutine auxzbr(x0,x1,funct,par,npar1,npar2)

        ! from Datan library, modified by GK
        ! Bracketing the root of the function given by funct, delivers x0,x1

        implicit none

        real(rn), intent(in out)    :: x0      !  Arguments safely encompassing the root
        real(rn), intent(in out)    :: x1      !
        real(rn), intent(in)        :: par     !  three parameters, on which funct
        integer   , intent(in)      :: npar1   !  depends
        integer   , intent(in)      :: npar2   !

        ! real(rn), EXTERNAL :: funct

        integer       :: i
        real(rn)      :: f0, f1, xs, funct
        !------------------------------------------------------------------
        if(abs(x0-x1) < EPS1MIN) x1 = x0 + ONE
        f0 = funct(x0,par,npar1,npar2)
        f1 = funct(x1,par,npar1,npar2)
        do  i=1,1000
            if(f0*f1 > ZERO) then
                if(abs(f0) <= abs(f1)) then
                    xs = x0
                    x0 = x0 + TWO*(x0-x1)
                    x1 = xs
                    f1 = f0
                    f0 = funct(x0,par,npar1,npar2)
                else
                    xs = x1
                    x1 = x1 + TWO*(x1-x0)
                    x0 = xs
                    f0 = f1
                    f1 = funct(x1,par,npar1,npar2)
                end if
            else
                exit
            end if
        end do

    end subroutine auxzbr

    !#######################################################################

    module subroutine auxzfn(x0,x1,xzero,funct,par,npar1,npar2,epsiln)

        ! from Datan library, modified by GK
        ! Finding the root (by bisection) of the function given by funct

        implicit none

        external funct

        real(rn), intent(in out) :: x0
        real(rn), intent(in out) :: x1
        real(rn), intent(out)    :: xzero
        real(rn), intent(in)     :: par
        integer, intent(in)      :: npar1
        integer, intent(in)      :: npar2
        real(rn), intent(in)     :: epsiln

        integer         :: i
        real(rn)        :: f0,f1,fm,test,xm,funct
        !------------------------------------------------------------------
        xzero = x0
        do  i=1,2000
            f0 = funct(x0,par,npar1,npar2)
            f1 = funct(x1,par,npar1,npar2)
            if(abs(f0) < EPS1MIN) then
                xzero = x0
                exit
            else if(abs(f1) < EPS1MIN) then
                xzero = x1
                exit
            end if
            xm =half*(x0+x1)
            if(abs(x0-x1) >= epsiln) then
                fm = funct(xm,par,npar1,npar2)
                test = f0*fm
                if(test < ZERO) then
                    x1 = xm
                else
                    x0 = xm
                end if
            else
                xzero = xm
                exit
            end if
        end do

    end subroutine auxzfn

!#######################################################################

    module real(rn) function gincgm(a,x)
        ! from Datan library, modified by GK
        ! calculates the incomplete gamma function

        implicit none

        real(rn), intent(in)             :: a
        real(rn), intent(in)             :: x

        integer       :: i,j
        real(rn)      :: a0,a1,b0,b1,a2j,a2j1,b2j,b2j1,cf,fnorm,f,s,anum,aloggm
        real(rn)      :: cfnew,help
        real(rn), parameter :: big=500._rn

        !-----------------------------------------------------------------------
        cfnew = ONE
        aloggm = glngam(a)
        if(x <= a+ONE) then
            ! series development
            f = ONE/a
            s = f
            anum = a
            do  i=1,100
                anum = anum + ONE
                f = x*f/anum
                s = s + f
                if(f < EPS1MIN) exit
            end do
            if(x < EPS1MIN) then
                gincgm = ZERO
            else
                help = a*LOG(x) - x - aloggm
                if(abs(help) >= big) then
                    gincgm = ZERO
                else
                    gincgm = s*EXP(help)
                end if
            end if
        else
            ! continued fraction
            a0 = ZERO
            b0 = ONE
            a1 = ONE
            b1 = x
            cf = ONE
            fnorm = ONE
            do  j=1,100
                a2j = real(j,rn) - a
                a2j1 = real(j,rn)
                b2j = ONE
                b2j1 = x
                a0 = (b2j*a1 + a2j*a0)*fnorm
                b0 = (b2j*b1 + a2j*b0)*fnorm
                a1 = b2j1*a0 + a2j1*a1*fnorm
                b1 = b2j1*b0 + a2j1*b1*fnorm
                if(abs(b1-ZERO) > EPS1MIN) then
                    ! renormalize and test for convergence
                    fnorm = ONE/b1
                    cfnew = a1*fnorm
                    if(abs(cf-cfnew)/cf < EPS1MIN) exit
                    cf = cfnew
                end if
            end do
            help = a*LOG(x) - x - aloggm
            if(abs(help) >= big) then
                gincgm = ONE
            else
                gincgm = ONE - EXP(help)*cfnew
            end if
        end if
    end function gincgm

!#######################################################################

    module real(rn) function gincbt(aa, bb, xx)
        ! from Datan library, modified by GK
        ! calculates the incomplete beta function

        implicit none

        real(rn), intent(in)             :: aa
        real(rn), intent(in)             :: bb
        real(rn), intent(in)             :: xx

        logical :: reflec

        integer         :: m
        real(rn)        :: a,b,x,a1,a2,b1,b2,rm,apl2m,cf,cfnew,d2m,d2m1,fnorm,xlim
        !-----------------------------------------------------------------------
        xlim = (aa + ONE)/(aa + bb + ONE)
        if (xx < xlim) then
            reflec = .false.
            a = aa
            b = bb
            x = xx
        else
            reflec = .true.
            a = bb
            b = aa
            x = ONE - xx
        end if
        if(x < EPS1MIN) then
            ! function known at end of range
            cf = ZERO
        else
            ! continued fraction
            a1 = ONE
            b1 = ONE
            a2 = ONE
            b2 = ONE -(a + b)*x/(a + ONE)
            fnorm = ONE/b2
            cf = a2*fnorm
            do  m=1,100
                rm = real(m,rn)
                apl2m = a + TWO*rm
                d2m = rm*(b - rm)*x/((apl2m - ONE)*apl2m)
                d2m1 = -(a + rm)*(a + b + rm)*x/(apl2m*(apl2m + ONE))
                a1 = (a2 + d2m*a1)*fnorm
                b1 = (b2 + d2m*b1)*fnorm
                a2 = a1 + d2m1*a2*fnorm
                b2 = b1 + d2m1*b2*fnorm
                if(abs(b2) > EPS1MIN) then
                    ! renormalize and test for convergence
                    fnorm = ONE/b2
                    cfnew = a2*fnorm
                    if(abs(cf-cfnew)/cf < EPS1MIN) exit
                    cf = cfnew
                end if
            end do
            cf = cf*(x**a)*((ONE - x)**b)/(a*gbetaf(a,b))
        end if
        if(reflec) then
            gincbt = ONE - cf
        else
            gincbt = cf
        end if
    end function gincbt

    !#######################################################################

    module real(rn) function gbetaf(z,w)
        ! from Datan library, modified by GK
        ! calculates the beta function B(z,w)

        implicit none
        real(rn), intent(in)   :: z
        real(rn), intent(in)   :: w
        real(rn), parameter    :: big=1.E+30_rn, epsiln=1.E-12_rn
        !--------------------------------------
        if(w < epsiln) then
            gbetaf = big
        else
            gbetaf = exp(glngam(z) + glngam(w) - glngam(z+w))
        end if

    end function gbetaf

    !#######################################################################

    module pure real(rn) function mean(x)

        ! function for calculating the aritmetic mean of the array x values

        implicit none
        real(rn), intent(in)   :: x(:)
        !--------------------------------------

        mean  = sum(x) / size(x)

    end function mean

    !#############################################################################

    module pure real(rn) function sd(x)

        ! function for calculating the standard deviation of the array x values

        implicit none
        real(rn), intent(in)   :: x(:)
        real(rn) :: mean_x
        !--------------------------------------
        mean_x = mean(x)

        sd = sqrt(sum((x - mean_x) ** 2) / (size(x) - 1))

    end function sd

    !----------------------------------------------------------------------------------------------!

    module subroutine MatRand(icn, ncr, covxy, muvect, zvect, bvect, kk)
        !------------------------------------------------------------------------------------------!
        !  Generate normal distributed values having a pre-defined covariance
        !  according to the text book
        !     V. Blobel & E. Lohrmann (1998), "Statistische und numerische
        !     Methoden der Datenanalyse", Teubner, page Seite 167

        !  For the selection of correlating quantities:
        !    Input:
        !    zvect  : vector of values zi from the standard normal distribution
        !    muvect : vector of normal mean values  mue
        !    covxy  : covariance matrix
        !
        !    Output:
        !    bvect  : vector of transformed values of the correlating quantities

        !     Copyright (C) 2014-2025  Günter Kanisch, Florian Ober
        !------------------------------------------------------------------------------------------!

        implicit none

        integer, intent(in)   :: icn            ! rank of the matrix covxy = number of correlating quantities muvect
        integer, intent(in)   :: ncr            ! phsyical dimension of covxy
        real(rn), intent(in)  :: covxy(ncr,ncr) ! covariance matrix
        real(rn), intent(in)  :: muvect(icn)    ! input vector of mean values
        real(rn), intent(in)  :: zvect(icn,1)   ! random standard normal values
        real(rn), intent(out) :: bvect(icn)     ! random fluctuations around muvect
        integer, intent(in)   :: kk             ! mc loop number

        integer          :: i, j, kkmin
        real(rn)         :: cov(icn, icn), Rmat(icn, icn), b(icn, 1)
        logical          :: posdef
        !------------------------------------------------------------------------------------------!

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
        ! can be ignored as mtxchl is now using the LAPACK function
        !
        ! do i=1,icn
        !     if(abs(cov(i,i)) < EPS1MIN) cov(i,i) = 1.E-17_rn
        ! end do

        ! Replace (Cholesky) the covariance matrix cov as a product (transpose(R) x R):
        ! Rmat = Triangular matrix  R

        call mtxchl(cov, Rmat, posdef)

        if(kk <= kkmin) then
            write(66,*) '    Rank of matrix RMAT: ',icn,'   physikcal dim of covxy: ',ncr
            call matwrite(Rmat,icn,icn,66,'(20es11.3)','triangular matrix Rmat:')
        end if
        ! z(1:icn,1) = zvect(1:icn,1)
        ! Product: b-vector = R-trans x z-Vector:
        b = matmul(transpose(Rmat), zvect)
        bvect(1:icn) = b(1:icn,1) + muvect(1:icn)

        if(kk <= kkmin) write(66,'(a,2x,20es11.3)') 'muvect: ',(muvect(i),i=1,icn)
        if(kk <= kkmin) write(66,'(a,2x,20es11.3)') 'zvect: ',(zvect(i,1),i=1,icn)
        if(kk <= kkmin) write(66,'(a,2x,20es11.3)') 'bvect: ',(bvect(i),i=1,icn)

    end subroutine MatRand

    !----------------------------------------------------------------------------------------------!

    module subroutine mtxequ(a, b, n, m)
        !------------------------------------------------------------------------------------------!

        implicit none
        integer, intent(in)     :: n
        integer, intent(in)     :: m
        real(rn), intent(inout) :: a(n,n)
        real(rn), intent(out)   :: b(n,m)
        !------------------------------------------------------------------------------------------!
        integer        :: k, kk, l, i, i1, j
        real(rn)       :: amax, tmp
        !------------------------------------------------------------------------------------------!

        do  k=1,n-1
            amax = zero
            kk = k
            do  l=k,n
                if(ABS(amax) < ABS(a(l,k))) then
                amax = a(l,k)
                kk = l
                end if
            end do
            if(kk /= k) then
            do  j=k,n
                tmp = a(k,j)
                a(k,j) = a(kk,j)
                a(kk,j) = tmp
            end do
            do  i=1,m
                tmp= b(k,i)
                b(k,i) = b(kk,i)
                b(kk,i) = tmp
            end do
            end if
            do  i=k+1,n
                do  j=k+1,n
                    a(i,j) = a(i,j) - a(k,j)*a(i,k)/a(k,k)
                end do
                do  j=1,m
                    b(i,j) = b(i,j) - b(k,j)*a(i,k)/a(k,k)
                end do
            end do
        end do
        do  j=1,m
            b(n,j) = b(n,j)/a(n,n)
            if(n > 1) then
                do  i1=1,n-1
                    i = n-i1
                    do  l=i+1,n
                        b(i,j) = b(i,j) - a(i,l)*b(l,j)
                    end do
                    b(i,j) = b(i,j)/a(i,i)
                end do
            end if
        end do
    end subroutine mtxequ

    !----------------------------------------------------------------------------------------------!

    module subroutine MulNormPrep(C,DPLUS,N)
        !------------------------------------------------------------------------------------------!
        ! Based on the Datan routine: SUBROUTINE RNMNPR(C,DPLUS,N)  (Brandt)
        ! prepares (only initialize!) for generation of random numbers from multivariate normal
        !------------------------------------------------------------------------------------------!
        implicit none

        integer, intent(in)     :: N
        real(rn), intent(in)    :: C(N,N)
        real(rn), intent(inout) :: DPLUS(N,N)
        !------------------------------------------------------------------------------------------!
        real(rn)    :: B(N,N),D(N,N),R(N),bout(N,N)
        integer     :: nv,i
        logical     :: OK, posdef
        !------------------------------------------------------------------------------------------!

        nv = N
        B(1:n,1:n) = C(1:n,1:n)
        D(1:Nv,1:Nv) = B(1:Nv,1:Nv)
        call MTXCHI(D)
        call MTXCHL(B,D,posdef)
        B = zero
        do i=1,nv
            B(i,i) = one
        end do
        call MTXSVD(D,B,DPLUS,R,zero,OK,bout)

    end subroutine MulNormPrep

end submodule Brandta
