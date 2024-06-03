module RND

    use UR_params,     only: rn, one, pi, zero, half

contains


!###########################################################################################

    subroutine UR_random_seed(idum)
        ! initialize the random generator

        implicit none

        integer(4),intent(in)    :: idum

        integer(4)               :: i, nn
        integer(4), allocatable  :: seed(:)


        call random_seed(size=nn)
        allocate(seed(nn))
        do i=1, nn
            seed(i) = int(idum**1.25_rn)
        end do
        call random_seed(put=seed)
        deallocate(seed)

    end subroutine UR_random_seed


!###########################################################################################

    real(rn) FUNCTION rndu()
        ! get a (0.,1.) uniform random number
        ! converts the subroutine random_number into a function

        implicit none
        call random_number(rndu)
    END FUNCTION rndu

!#######################################################################

    real(rn) function rgamma(nvt,svor,first)

        ! generates a gamma distributed random number
        ! uses Ran_gamma8 (according to Marsaglia)
        ! and Random_gamma2

        ! Note: the rgamma generator can be tested within the routine MCCalc,
        ! around line 350, where one finds the two lines:

        !      ! Test gamma-distributed random values (de-activate GOTO 10):
        !      GOTO 10
        ! Enable the test by simply deactivating the GOTO 10!
        !

        USE UR_MCC,        ONLY: c_mars,d_mars
        USE UR_DLIM,       ONLY: GamDistAdd
        use UR_params,     only: rn,zero,one,half
        use UR_Gleich,     only: ifehl

        implicit none

        integer(4), INTENT(IN)   :: nvt    ! number of the quantity to generate a random gamma deviate for
        real(rn), INTENT(IN)     :: svor   ! given value, for wich random gamma distrib. values
        ! are to be generated
        LOGICAL, INTENT(IN)      :: first

        real(rn)    ::  s

        s = svor + GamDistAdd

        if(first) then
            ! The initialisation must be done for both of the random generators
            rgamma = Random_gamma2(nvt,s,one,.true.)       ! s < 1.0
            rgamma = Ran_Gamma8(nvt, s,.TRUE.)             ! s >= 1.0
        else
            if(s < one) then
                rgamma = Random_gamma2(nvt,s,one,.false.)
            else
                rgamma = Ran_Gamma8(nvt, s,.FALSE.)
            end if
        end if

    end function rgamma

!#######################################################################

    real(rn) FUNCTION Ran_Gamma8(nvt, svor, first)

        ! This routine is taken from Alan Millers website for Fortran codes
        ! modified by GK
        !
        ! IMPORTANT: This routine must only be called via the routine rgamma!
        !            If called directly, the effect of GamDistAdd would be lost!
        !
        ! Uses the algorithm in
        ! Marsaglia, G. and Tsang, W.W. (2000) `A simple method for generating
        ! gamma variables', Trans. om Math. Software (TOMS).
        !
        ! G. Marsaglia and W.W. Tsang, A simple method for generating gamma variables,
        ! ACM Transactions on Mathematical Software, Vol. 26, No. 3, Pages 363-372,
        ! September, 2000.

        ! Generates a random gamma deviate for shape parameter s >= 1.
        ! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
        ! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO GAMMA**(S-1)*EXP(-GAMMA),

        ! Modifications:
        ! If the generator is used for several quantities simultaneously, these
        ! quantities have to be differentiated by an identfication variable nvt, because
        ! at the initialisation (first=T) for each quantity individual variables
        ! c and d are created, i.e., arrays c_mars() and d_mars(). For each quantity with
        ! idenfication nvt the values c(nvt) and d(nvt) are used for further random
        ! numbers (with first=F).
        ! c_mars() and d_mars() are stored in the modul MCC.


        USE UR_MCC,        ONLY: c_mars,d_mars
        USE UR_DLIM,       ONLY: GamDistAdd
        use UR_params,     only: rn,zero,one,two,half
        use UR_Gleich,     only: ifehl

        IMPLICIT NONE

        integer(4), INTENT(IN) :: nvt    ! identification number of the quantity
        real(rn), INTENT(IN)   :: svor   ! given value, for wich random gamma distrib. values
        ! are to be generated
        LOGICAL, INTENT(IN)    :: first

        integer(4)      :: kk0
        real(rn)        :: s,svor2
        real(rn)        :: u, v, x
        logical         :: exception

        !-----------------------------------------------------------------------
        ! The following few lines have the effect that the mean value of random
        ! numbers generated with svor have will be (svor + GamDistAdd).
        !
        ! This routine shall be called only for values svor >= 1.
        ! In the case of svor < 1, use the routine random_gamma2
        !          The best is always to call it always via a call to rgamma!!!!


        ifehl = 0
        ! svor2 = svor + GamDistAdd
        exception = .false.
        if(svor < one - 1.e-8_rn) exception = .true.

        s = svor           !  + GamDistAdd
        !-----------------------------------------------------------------------
        IF (s < one) THEN
            ! WRITE(*, *) 'Shape parameter must be >= 1'
            ! STOP
        END IF

        IF (first) THEN
            if(.not.exception) d_mars(nvt) = svor - one/3.0_rn
            if(exception) d_mars(nvt) = svor - one/3.0_rn

            c_mars(nvt) = one/SQRT(9.0_rn*d_mars(nvt))
        END IF

        ! Start of main loop
        DO

            ! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.
            kk0 = 0
            DO
                x = rnorm()
                v = (one + c_mars(nvt)*x)**3._rn
                IF (v > zero) EXIT

                ! emergency exit:
                kk0 = kk0 + 1
                if(kk0 > 50000) then
                    !write(66,*) 'Error in Ran_gamma: 2nd loop: kk0 > 50000'
                    ifehl = 1
                    exit
                end if

            END DO

            ! Generate uniform variable U
            u = Rndu()
            IF (u < one - 0.03310_rn*x**4._rn) THEN
                Ran_Gamma8 = d_mars(nvt)*v
                EXIT
            ELSE IF (LOG(u) < half*x**two + d_mars(nvt)*(one - v + LOG(v))) THEN
                Ran_Gamma8 = d_mars(nvt)*v
                EXIT
            END IF
        END DO

        !if(exception) then
        !  u = Ran2d(idum)
        !  Ran_Gamma8 = Ran_Gamma8 * u**(one/(s - one))
        !end if

        RETURN
    END FUNCTION Ran_Gamma8

!#######################################################################

    real(rn) FUNCTION random_gamma2(nvt,s, b, first) ! RESULT(fn_val)

        ! generate a gamma distributed random number for a given value s
        ! see comments in the function rgamma!

        ! Adapted from Fortran 77 code from the book:
        !     Dagpunar, J. 'Principles of random variate generation'
        !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

        ! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
        ! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO
        ! GAMMA2**(S-1) * EXP(-GAMMA2),
        ! USING A SWITCHING METHOD.

        !    S = SHAPE PARAMETER OF DISTRIBUTION
        !          (REAL < 1.0)
        !  b: scale factor


        use UR_params,   only: rn, zero,one,two
        use UR_Gleich,   only: ifehl
        use UR_MCC,      only: a_rg, p_rg, c_rg, uf_rg, vr_rg, d_rg


        implicit none

        integer(4), INTENT(IN) :: nvt    ! identification number of the quantity
        REAL (rn), INTENT(IN)  :: s      ! given value, for wich random gamma distrib. values
        ! are to be generated
        REAL (rn), INTENT(IN)  :: b      ! scale factor
        LOGICAL, INTENT(IN)    :: first  ! = true, if the first rnd number is to be generated

        REAL (rn)              :: fn_val
        !     Local variables
        integer(4)            :: j
        REAL (rn)             :: r, x, w
        ! following variables, transformed to arrays, are located in the module UR_MCC:
        ! REAL (rn), SAVE       :: a, p, c, uf, vr, d
        ! REAL (rn), SAVE       :: a_rg, p_rg, c_rg, uf_rg, vr_rg, d_rg
        REAL (rn), PARAMETER  :: vsmall = EPSILON(one)

        ifehl = 0
        random_gamma2 = zero
        IF (s <= zero .OR. s >= one) THEN
            !WRITE(63, *) 'SHAPE PARAMETER VALUE OUTSIDE PERMITTED RANGE'

        END IF

        IF (first) THEN       ! Initialization
            a_rg(nvt) = one - s
            p_rg(nvt) = a_rg(nvt)/(a_rg(nvt) + s*EXP(-a_rg(nvt)))
            IF (s < vsmall) THEN
                ifehl = 2
                !WRITE(63, *) 'SHAPE PARAMETER VALUE TOO SMALL'
                return   ! STOP
            END IF
            c_rg(nvt) = one/s
            uf_rg(nvt) = p_rg(nvt)*(vsmall/a_rg(nvt))**s
            vr_rg(nvt) = one - vsmall
            d_rg(nvt) = a_rg(nvt)*LOG(a_rg(nvt))
        END IF

        j = 0
        DO
            j = j + 1
            if(j == 150) then
                ifehl = 3
                return
            end if
            r = Rndu()
            IF (r >= vr_rg(nvt)) THEN
                CYCLE
            ELSE IF (r > p_rg(nvt)) THEN
                x = a_rg(nvt) - LOG((one - r)/(one - p_rg(nvt)))
                w = a_rg(nvt)*LOG(x)-d_rg(nvt)
            ELSE IF (r > uf_rg(nvt)) THEN
                x = a_rg(nvt)*(r/p_rg(nvt))**c_rg(nvt)
                w = x
            ELSE
                fn_val = zero
                RETURN
            END IF

            r = Rndu()
            IF (one-r <= w .AND. r > zero) THEN
                IF (r*(w + one) >= one) CYCLE
                IF (-LOG(r) <= w) CYCLE
            END IF
            EXIT
        END DO

        fn_val = x

! Now scale the random variable
        fn_val = b * fn_val
        random_gamma2 = fn_val

        RETURN

    END FUNCTION random_gamma2

!#######################################################################

    FUNCTION random_beta(nvt,aa, bb, first) RESULT(fn_val)

        ! generate a (two-parameter) beta distributed random number

        ! Adapted from Fortran 77 code from the book:
        !     Dagpunar, J. 'Principles of random variate generation'
        !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

        ! FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
        ! FROM A BETA DISTRIBUTION WITH DENSITY
        ! PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
        ! USING CHENG'S LOG LOGISTIC METHOD.

        !     AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
        !     BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)

        use UR_params,        only: rn,zero,one,two
        use UR_MCC,           only: d_rb,f_rb,h_rb,t_rb,c_rb,swap_rb
        use UR_Gleich,        only: ifehl

        implicit none

        integer(4),intent(in)    :: nvt
        REAL(rn), INTENT(IN)     :: aa, bb
        LOGICAL, INTENT(IN)      :: first
        REAL(rn)                 :: fn_val

        !     Local variables
        REAL(rn), PARAMETER  :: aln4 = log(4._rn)            ! 1.3862944
        REAL(rn)             :: a, b, g, r, s, x, y, z
        ! REAL(rn), SAVE       :: d, f, h, t, c
        ! LOGICAL, SAVE        :: swap
        real(rn)             :: vsmall = TINY(1.0_rn), vlarge = HUGE(1.0_rn)

        IF (aa <= zero .OR. bb <= zero) THEN
            WRITE(0, *) 'Ran_beta: IMPERMISSIBLE SHAPE PARAMETER VALUE(S)'
            fn_val= zero
            ifehl = 1
            return
        END IF

        IF (first) THEN     ! Initialization
            a = aa
            b = bb
            swap_rb(nvt) = b > a
            IF (swap_rb(nvt)) THEN
                g = b
                b = a
                a = g
            END IF
            d_rb(nvt) = a/b
            f_rb(nvt) = a + b
            IF (b > one) THEN
                h_rb(nvt) = SQRT((two*a*b - f_rb(nvt))/(f_rb(nvt) - two))
                t_rb(nvt) = one
            ELSE
                h_rb(nvt) = b
                t_rb(nvt) = one/(one + (a/(vlarge*b))**b)
            END IF
            c_rb(nvt) = a + h_rb(nvt)
        END IF

        DO
            r = Rndu()
            x = Rndu()
            s = r*r*x
            IF (r < vsmall .OR. s <= zero) CYCLE
            IF (r < t_rb(nvt)) THEN
                x = LOG(r/(one - r))/h_rb(nvt)
                y = d_rb(nvt)*EXP(x)
                z = c_rb(nvt)*x + f_rb(nvt)*LOG((one + d_rb(nvt))/(one + y)) - aln4
                IF (s - one > z) THEN
                    IF (s - s*z > one) CYCLE
                    IF (LOG(s) > z) CYCLE
                END IF
                fn_val = y/(one + y)
            ELSE
                IF (4.0_rn*s > (one + one/d_rb(nvt))**f_rb(nvt)) CYCLE
                fn_val = one
            END IF
            EXIT
        END DO

        IF (swap_rb(nvt)) fn_val = one - fn_val
        RETURN
    END FUNCTION random_beta

!####################################################################

    FUNCTION random_t(nvt,m,first) RESULT(fn_val)

        ! Adapted from Fortran 77 code from the book:
        !     Dagpunar, J. 'Principles of random variate generation'
        !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

        ! FUNCTION GENERATES A RANDOM VARIATE FROM A
        ! T DISTRIBUTION USING KINDERMAN AND MONAHAN'S RATIO METHOD.

        !     M = DEGREES OF FREEDOM OF DISTRIBUTION
        !           (1 <= 1NTEGER)

        use UR_params,     only: rn,zero,one,two
        use UR_MCC,        only: s_rt,c_rt,a_rt,f_rt,g_rt

        implicit none

        integer(4), intent(in)    :: nvt
        INTEGER(4), INTENT(IN)    :: m    ! dof
        logical,intent(in)        :: first
        REAL(rn)                  :: fn_val

        !     Local variables
        ! REAL(rn), SAVE      :: s, c, a, f, g
        REAL(rn)            :: r, x, v

        REAL(rn), PARAMETER :: three = 3.0_rn, four = 4.0_rn, quart = 0.25_rn,   &
            five = 5.0_rn, sixteen = 16.0_rn
        INTEGER(4)          :: mm = 0

        IF (m < 1) THEN
            WRITE(*, *) 'IMPERMISSIBLE DEGREES OF FREEDOM'
            fn_val = 0._rn  ! STOP
        END IF

        ! IF (m /= mm) THEN                    ! Initialization, if necessary
        if(first) then
            s_rt(nvt) = m
            c_rt(nvt) = -quart*(s_rt(nvt) + one)
            a_rt(nvt) = four/(one + one/s_rt(nvt))**c_rt(nvt)
            f_rt(nvt) = sixteen/a_rt(nvt)
            IF (m > 1) THEN
                g_rt(nvt) = s_rt(nvt) - one
                g_rt(nvt) = ((s_rt(nvt) + one)/g_rt(nvt))**c_rt(nvt)*SQRT((s_rt(nvt)+s_rt(nvt))/g_rt(nvt))
            ELSE
                g_rt(nvt) = one
            END IF
            mm = m
        END IF

        DO
            call random_number(r)

            IF (r <= zero) CYCLE
            call random_number(v)

            x = (two*v - one)*g_rt(nvt)/r
            v = x*x
            IF (v > five - a_rt(nvt)*r) THEN
                IF (m >= 1 .AND. r*(v + three) > f_rt(nvt)) CYCLE
                IF (r > (one + v/s_rt(nvt))**c_rt(nvt)) CYCLE
            END IF
            EXIT
        END DO

        fn_val = x
        RETURN
    END FUNCTION random_t

!#######################################################################

    real(rn) FUNCTION ran_Erlang(rate, xk)

        ! generate an Erlang-distributed random number

        !     Copyright (C) 2019-2023  Günter Kanisch

        use UR_params,    only: rn,pi,zero,one,two,eps1min
        implicit none

        real(rn), INTENT(IN)         :: rate      ! lambda
        real(rn), INTENT(IN)         :: xk        ! positive integer number

        integer(4)         :: k,i
        real(rn)           :: su,prd
!-------------------------------------------------------------------------
        k = xk
        prd = one
        do i=1,k
            prd = prd * Rndu()
        end do
        ran_Erlang = -log(prd) / rate

    END FUNCTION ran_Erlang

!#######################################################################

    REAL(rn) FUNCTION random_bipo2(p,N,Rb,tg)

        ! generate a random number distributed according to the distribution
        ! BinPoi_2_PDF: apply the rejection method for generating random numbers
        !
        ! Before using it, execute first scan_bipoi2(p,N,Rb,tg) to find the
        ! parameters bipoi2_maxk,bipoi2_hgt
        !
        !     Copyright (C) 2019-2023  Günter Kanisch

        use UR_params,   only: rn,zero,one,eps1min,two
        use pdfs,        only: BinPoi_2_PDF
        use UR_MCC,      only: imc
        use UR_Gleich,   only: Nbin0_MV,bipoi2_maxk,bipoi2_hgt
        implicit none

        REAL(rn), INTENT(IN)    :: p      ! paramter of the binomial distrib.
        REAL(rn), INTENT(IN)    :: N      ! paramter of the binomial distrib.
        REAL(rn), INTENT(IN)    :: Rb     ! background count rate
        REAL(rn), INTENT(IN)    :: tg     ! counting time of gross measurement

        integer(4)      :: i,jmax
        real(rn)        :: y,pval,fakt,hg(3)
        real(rn)        :: xk,xkmin,xkmax,yk,ymin,ymax,xpos

        random_bipo2 = zero
        ! apply the rejection method for generating random numbers from the BinPoi_2_PDF:
        xkmin = zero
        xkmax = bipoi2_maxk
        ymax = bipoi2_hgt
        ymax = ymax * 1.01_rn
        ymin = zero
        do i=1,200
            xk = xkmin + Rndu()*(xkmax-xkmin)
            yk = Rndu()*ymax
            call BinPoi_2_PDF(xk, N, p, Rb,tg, pval,  fakt,hg,jmax,.false.)
            if(yk <= pval) then
                random_bipo2 = xk
                return
            end if
        end do

    end function random_bipo2

!#######################################################################

    subroutine scan_bipoi2(p,N,Rb,tg)

        ! Determine the parameters bipoi22_hgt and bipoi2_maxk required by random_bipo2
        !     Copyright (C) 2019-2023  Günter Kanisch

        use UR_params,   only: rn,zero,one,eps1min,two
        use pdfs,        only: BinPoi_2_PDF
        use UR_MCC,      only: imc
        use UR_Gleich,   only: bipoi2_hgt,bipoi2_maxk

        implicit none

        REAL(rn), INTENT(IN)    :: p      ! paramter of the binomial distrib.
        REAL(rn), INTENT(IN)    :: N      ! paramter of the binomial distrib.
        REAL(rn), INTENT(IN)    :: Rb     ! background count rate
        REAL(rn), INTENT(IN)    :: tg     ! counting time of gross measurement

        integer(4)     :: i,jmax
        real(rn)       :: sumP,pval,fakt,hg(3)

        sumP = zero
        bipoi2_hgt = zero
        do i=1,350
            call BinPoi_2_PDF(real(i-1,rn), N, p,Rb, tg, pval,  fakt,hg,jmax,.false.)
            sumP = sumP + pval
            if(pval > bipoi2_hgt) bipoi2_hgt = pval
            if(sumP > 0.5_rn .and. pval/sumP < 1.e-8_rn) then
                bipoi2_maxk = real(i-1,rn)
                exit
            end if
        end do
        ! write(63,*) 'Scan: bipoi2: maxk=',sngl(bipoi2_maxk),' hgt=',sngl(bipoi2_hgt), &
        !                ' N, Rb, tg=',sngl(N),sngl(Rb),sngl(tg)

    end subroutine scan_bipoi2

!#######################################################################

    FUNCTION rnorm() RESULT(fn_val)
        use UR_params,   only: rn

        !! https://fortran-lang.discourse.group/t/normal-random-number-generator/3724/2
        !
        !              Based on Alan Miller's rnorm.f90,
        !   Generate a random normal deviate using the polar method.
        !   Reference: Marsaglia,G. & Bray,T.A. 'A convenient method for generating
        !              normal variables', Siam Rev., vol.6, 260-264, 1964.

        IMPLICIT NONE
        REAL(kind=rn)  :: fn_val

        ! Local variables

        REAL(kind=rn)            :: u, sum
        REAL(kind=rn), SAVE      :: v, sln
        LOGICAL, SAVE            :: second = .FALSE.
        REAL(kind=rn), PARAMETER :: vsmall = TINY( one )  ! Flo: shouldn't this be epsilon(one)?

        IF (second) THEN
            ! If second, use the second random number generated on last call

            second = .false.
            fn_val = v*sln

        ELSE
            ! First call; generate a pair of random normals

            second = .true.
            DO
                !CALL RANDOM_NUMBER( u )
                !CALL RANDOM_NUMBER( v )
                u = Rndu()
                v = Rndu()
                u = SCALE( u, 1 ) - one
                v = SCALE( v, 1 ) - one
                sum = u*u + v*v + vsmall         ! vsmall added to prevent LOG(zero) / zero
                IF(sum < one) EXIT
            END DO
            sln = SQRT(-SCALE(LOG(sum),1)/sum)
            fn_val = u*sln
        END IF
    END FUNCTION rnorm

!#############################################################################
    function ignpoi ( mu )

        !*****************************************************************************80

        !  https://people.sc.fsu.edu/~jburkardt/f_src/ranlib/ranlib.f90
        !
        !  IGNPOI generates a Poisson random deviate.
        !
        !  Discussion:
        !
        !    This procedure generates a single random deviate from a Poisson
        !    distribution with given mean.
        !
        !  Licensing:
        !
        !    This code is distributed under the MIT license.
        !
        !  Modified:
        !
        !    June 03 2024
        !
        !  Author:
        !
        !    Original FORTRAN77 version by Barry Brown, James Lovato.
        !    FORTRAN90 version by John Burkardt.
        !
        !  Reference:
        !
        !    Joachim Ahrens, Ulrich Dieter,
        !    Computer Generation of Poisson Deviates
        !    From Modified Normal Distributions,
        !    ACM Transactions on Mathematical Software,
        !    Volume 8, Number 2, June 1982, pages 163-179.
        !
        !  Parameters:
        !
        !    Input, real MU, the mean of the Poisson distribution
        !    from which a random deviate is to be generated.
        !
        !    Output, integer IGNPOI, a random deviate from
        !    the distribution.
        !

        implicit none

        real(rn), parameter :: a0 = -0.5_rn
        real(rn), parameter :: a1 =  0.3333333_rn
        real(rn), parameter :: a2 = -0.2500068_rn
        real(rn), parameter :: a3 =  0.2000118_rn
        real(rn), parameter :: a4 = -0.1661269_rn
        real(rn), parameter :: a5 =  0.1421878_rn
        real(rn), parameter :: a6 = -0.1384794_rn
        real(rn), parameter :: a7 =  0.1250060_rn

        real(rn) b1, b2
        real(rn) c, c0, c1, c2, c3
        real(rn) d, del, difmuk
        real(rn) e
        real(rn) fact(10)
        real(rn) fk, fx, fy
        real(rn) g

        integer ignpoi
        integer k, kflag
        integer l
        integer m
        real(rn) mu
        real(rn) omega
        real(rn) p, p0, px, py
        real(rn) q
        real(rn) s
        real(rn) t
        real(rn) u
        real(rn) v
        real(rn) x
        real(rn) xx

        save fact

        data fact / 1.0, 1.0, 2.0, 6.0, 24.0, &
                    120.0, 720.0, 5040.0, 40320.0, 362880.0 /

        ignpoi = 0
        difmuk = 0.0
        !
        !  MU < 10
        !
        if ( mu < 10.0_rn ) then
            m = max ( 1, int ( mu ) )
            l = 0
            p = exp ( - mu )
            q = p
            p0 = p
            !
            !  Uniform sample for inversion method.
            !
            do
                call random_number(u)

                if ( u <= p0 ) then
                    return
                end if
                !
                !  Creation of new Poisson probabilities.
                !
                do k = 1, 35
                    p = p * mu / real ( k, rn )
                    q = q + p
                    if ( u <= q ) then
                        ignpoi = k
                        return
                    end if
                end do
            end do
            !
            !  10 <= MU
            !
        else
            s = sqrt ( mu )
            d = 6.0_rn * mu * mu
            l = int ( mu - 1.1484_rn )
            !
            !  Normal sample.
            !

            g = mu + s * rnorm()

            if ( 0.0_rn <= g ) then
                ignpoi = int ( g )
                !
                !  Immediate acceptance if large enough.
                !
                if ( l <= ignpoi ) then
                    return
                end if
                !
                !  Squeeze acceptance.
                !
                fk = real (ignpoi, rn)
                difmuk = mu - fk
                ! u = Rndu()        ! r4_uni_01 ( )
                call random_number(u)

                if ( difmuk * difmuk * difmuk <= d * u ) then
                    return
                end if
            end if
            !
            !  Preparation for steps P and Q.
            !
            omega = 0.3989423_rn / s
            b1 = 0.04166667_rn / mu
            b2 = 0.3_rn * b1 * b1
            c3 = 0.1428571_rn * b1 * b2
            c2 = b2 - 15.0_rn * c3
            c1 = b1 - 6.0_rn * b2 + 45.0_rn * c3
            c0 = 1.0_rn - b1 + 3.0_rn * b2 - 15.0_rn * c3
            c = 0.1069_rn / mu

            if ( 0.0_rn <= g ) then

                kflag = 0

                if ( ignpoi < 10 ) then
                    px = - mu
                    py = mu ** ignpoi / fact(ignpoi+1)

                else
                    del = 0.08333333_rn / fk
                    del = del - 4.8_rn * del * del * del
                    v = difmuk / fk

                    if ( 0.25_rn < abs ( v ) ) then
                        px = fk * log ( 1.0_rn + v ) - difmuk - del
                    else
                        px = fk * v * v * ((((((( a7 &
                            * v + a6 ) &
                            * v + a5 ) &
                            * v + a4 ) &
                            * v + a3 ) &
                            * v + a2 ) &
                            * v + a1 ) &
                            * v + a0 ) - del
                    end if

                    py = 0.3989423_rn / sqrt ( fk )

                end if
                x = ( 0.5_rn - difmuk ) / s
                xx = x * x
                fx = -0.5_rn * xx
                fy = omega * ((( c3 * xx + c2 ) * xx + c1 ) * xx + c0 )

                if ( fy - u * fy <= py * exp ( px - fx ) ) then
                    return
                end if
            end if
            !
            !  Exponential sample.
            !
            do
                e = random_exponential()

                u = 2.0_rn * rndu() - 1.0_rn
                if ( u < 0.0_rn ) then
                    t = 1.8_rn - abs ( e )
                else
                    t = 1.8_rn + abs ( e )
                end if

                if ( t <= -0.6744_rn ) then
                    cycle
                end if

                ignpoi = int ( mu + s * t )
                fk = real ( ignpoi,rn )
                difmuk = mu - fk

                kflag = 1
                !
                !  Calculation of PX, PY, FX, FY.
                !
                if ( ignpoi < 10 ) then
                    px = -mu
                    py = mu ** ignpoi / fact(ignpoi+1)
                else
                    del = 0.08333333_rn / fk
                    del = del - 4.8_rn * del * del * del
                    v = difmuk / fk

                    if ( 0.25_rn < abs ( v ) ) then
                        px = fk * log ( 1.0_rn + v ) - difmuk - del
                    else
                        px = fk * v * v * ((((((( a7 &
                            * v + a6 ) &
                            * v + a5 ) &
                            * v + a4 ) &
                            * v + a3 ) &
                            * v + a2 ) &
                            * v + a1 ) &
                            * v + a0 ) - del
                    end if

                    py = 0.3989423_rn / sqrt ( fk )

                end if

                x = ( 0.5_rn - difmuk ) / s
                xx = x * x
                fx = -0.5_rn * xx
                fy = omega * ((( c3 * xx + c2 ) * xx + c1 ) * xx + c0 )

                if ( kflag <= 0 ) then
                    if ( fy - u * fy <= py * exp ( px - fx ) ) then
                        return
                    end if
                else
                    if ( c * abs ( u ) <= py * exp ( px + e ) - fy * exp ( fx + e ) ) then
                        return
                    end if
                end if
            end do
        end if
    end

!#######################################################################

    function random_exponential() result(fn_val)

        ! Adapted from Fortran 77 code from the book:
        !     Dagpunar, J. 'Principles of random variate generation'
        !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

        ! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
        ! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
        ! TO EXP(-random_exponential), USING INVERSION.

        real(rn)  :: fn_val
        !     Local variable
        real(rn)  :: r

        do
            call random_number(r)
            if (r > 0.0_rn) exit
        end do

        fn_val = -log(r)
        return

    end function random_exponential

end module RND
