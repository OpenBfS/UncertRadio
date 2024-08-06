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


    module subroutine mtxchi(a)

        ! from Datan library, modified by GK
        ! this routine inverts a (n x n) matrix by Cholesky decomposition

        implicit none

        real(rn), intent(inout)        :: a(:,:)

        integer                        :: i, l, n, info
        logical                        :: posdef
        integer, dimension(size(a,1))  :: ipiv   ! pivot indices
        real(rn), dimension(size(a,1)) :: work  ! work array for LAPACK
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
        ! CALL mtxchl(a, u, posdef)
        ! if(.not. posdef) then
        !     print *, '###################################            now'
        !     write(23,*) ' in MTXCHI:  after call MTXCHL: posdef=',posdef,'  n=',int(n,2)
        !     call matwrite(A,n,min(n,100),23,'(150es11.3)','MTXCHI, at the begin: Matrix A :')
        !     return
        ! end if

        ! DO  i=1,n
        !     ! Step 2: Forward Substitution
        !     DO  l=i,n
        !         IF(l == i) THEN
        !             a(n,l) = one/u(l,l)
        !         ELSE
        !             a(n,l) = zero
        !             if(l-1 >= i) a(n,l) = -sum(u(i:l-1,l)*a(n,i:l-1))
        !             a(n,l) = a(n,l)/u(l,l)
        !         END IF
        !     END DO
        !     ! Step 3: Back Substitution
        !     DO  l=n,i,-1
        !         IF(l == n) THEN
        !             a(i,l) = a(n,l)/u(l,l)
        !         ELSE
        !             a(i,l) = a(n,l)
        !             a(i,l) = a(i,l) - sum( u(l,l+1:n)*a(i,l+1:n))
        !             a(i,l) = a(i,l)/u(l,l)
        !         END IF
        !     END DO
        ! END DO

        ! ! Fill lower triangle symmetrically
        ! IF(n > 1) THEN
        !     DO  i=1,n
        !         a(i,1:i-1) = a(1:i-1,i)
        !     END DO
        ! END IF

        ! write(66,*) 'mtxchi: posdef=',posdef
        ! IF(printout) THEN
            ! call matwrite(A,n,min(n,100),23,'(150es11.3)','MTXCHI, at end: matrix A:')
        ! end if

    END SUBROUTINE mtxchi

!#######################################################################

    module subroutine mtxchl(a, u, posdef)

        ! from datan library, modified by gk
        ! this routine performs a cholesky decomposition for a positive definite
        ! symmetric matrix a and returns the upper triangular matrix u.
        use ur_linft,     only: ncofact, cofact, use_wtls

        implicit none

        real(rn), intent(in)         :: a(:, :)
        real(rn), intent(inout)      :: u(:, :)
        logical, intent(out)         :: posdef

        integer       :: j, k, i, n, info
        real(rn)      :: s
        integer, dimension(size(a,1))  :: ipiv   ! pivot indices
        logical       :: printout , symmetric
        !-----------------------------------------------------------------------------
        printout = .false.
        ! printout = .true.
        !----------------------------------------------------------------------------
        n = ubound(a, dim=1)

        !---------------------------------------------------------------------------------
        ! posdef = .false.
        ! u = a
        ! call DGETRF(n, n, u, n, ipiv, info)

        ! if (info /= 0) then
        !     !    stop 'Matrix is numerically singular!'
        !    posdef = .true.
        ! end if
        ! The Cholesky decomposition requires a symmetric matrix!
        symmetric = .true.
        if(printout) then
            ! call matwrite(A,n,n,23,'(150es11.3)','MTXCHI, Matrix A :')
            ! call matwrite(A,n,n,23,'(150es21.13)','MTXCHI, Matrix A :')
        end if

        if(.false.) then
            do i=1, n
                do k=i+1, n
                    if(abs(a(i,k)-a(k,i)) > eps1min * abs(a(i,k)) ) then
                        symmetric = .false.
                        !if(printout)
                        write(23,*) 'asymmetric: i,k=',int(i,2),int(k,2),a(i,k),a(k,i),' diff=',a(i,k)-a(k,i)
                    end if
                end do
            end do
            ! IF(printout .and. .not.symmetric)
            write(23,*) '  MTXCHL:  Warning: matrix A is not symmetric! No result from mtxchl !'
        end if
        !-----------------------------------------------------------------------------------------
        ncofact = 0
        cofact = 1.0_rn
        if(use_WTLS) cofact = one - eps1min

11      continue

        u = zero
        posdef = .true.
        do k=1, n
            s = zero
            do  j=k, n
                if(k > 1) then
                    s = sum(u(1:k-1,k) * u(1:k-1,j))
                end if
                u(k,j) = a(k,j) - s
                if(k /= j) u(k,j) = a(k,j) * cofact - s
                if(k == j) then
                    if(abs(u(k,k)) < eps1min) then
                        ncofact = ncofact + 1
                        cofact = cofact * (one - eps1min)
                        if(printout) then
                            write(23,'(a,L1,2(a,i3),a,es14.7,a,es8.1)') 'MTXCHL: posdef=',posdef, &
                                '  k=',k,'  j=',j,' u(k,k)=',u(k,k),' cofact= 1-',(one-cofact)
                        end if

                        if(ncofact <= 4) goto 11
                        posdef = .false.
                        return
                    end if
                    u(k,j) = sqrt(abs(u(k,j)))
                else
                    u(k,j) = u(k,j) / u(k,k)
                end if
            end do    ! j loop
        end do      ! k loop
        return
    end subroutine mtxchl

!#######################################################################

    module SUBROUTINE mtxgva(v1,v2,c,s)

        ! from Datan library, modified by GK
        ! this routine defines a Givens-transformation and applies it to
        ! directly to the defining vector.

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

    module subroutine fixprep(xall,nall,list,nred,x)         ! ,mfix,indfix,xfix)
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

    module subroutine expand(pa,nred,x)

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

    module SUBROUTINE Lsqlin(userfn,t,y,deltay,n,nall,list,pa,covpa,r)
        ! linear weighted Least squares fit of a linear function to a curve
        ! of measured values (t,y) using singular value decomposiition.
        ! From Datan library, modified by GK

        use UR_Linft,      only: use_PLSQ,use_PMLE

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

        LOGICAL               :: ok, upSV
        integer(4)            :: i,k,nr,nred,j,nrep
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
        use_PMLE = upSV        !- 18.6.2024

    END SUBROUTINE Lsqlin

!################################################################################

    module real(rn) FUNCTION qchi2(p,n)

    ! from Datan library, modified by GK
    ! calculates the p quantile of the chi-square distribution

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

    module real(rn) FUNCTION pchi2(x,n)

        ! from Datan library, modified by GK
        ! distribution function of the chis-square distribution

        IMPLICIT none

        real(rn), INTENT(IN)         :: x
        INTEGER, INTENT(IN)          :: n

        real(rn)                     :: a
        !-----------------------------------------------------------------------------
        a = half * n
        pchi2 = gincgm(a, half*x)

    END FUNCTION pchi2

!#######################################################################

module real(rn) FUNCTION SCSTNR(X)

 ! from Datan library, modified by GK
 ! distribution function of the standard normal distribution

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

implicit none

real(rn), INTENT(IN OUT)    :: x
real(rn), INTENT(IN)        :: p

!------------------------------------------------------------------
szstnr = p - scstnr(x)

END FUNCTION szstnr

!#######################################################################

module real(rn) function pnorm(x, x0, sigma)

! calculates the probability (normal distribution) for a quantile value x

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

    implicit none


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

        implicit none
        real(rn), intent(in)   :: x(:)
        integer                :: n
        !--------------------------------------
        n = size(x)
        mean  = sum(x) / n

    end function mean

    !#############################################################################

    module real(rn) function sd(x)

    ! function for calculating the standard deviation of the array x values

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
    logical          :: posdef

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

    call MTXCHL(cov,Rmat, posdef)

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


end submodule Brandta
