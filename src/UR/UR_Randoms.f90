module RND

    use UR_types,  only: rn

contains


!###########################################################################################

    subroutine UR_random_seed(idum)
        ! initialize the random generator

        implicit none
        integer, intent(in)    :: idum

        integer                :: i, nn
        integer, allocatable   :: seed(:)


        call random_seed(size=nn)
        allocate(seed(nn))
        do i=1, nn
            seed(i) = int(idum**1.25_rn)
        end do
        call random_seed(put=seed)
        deallocate(seed)

    end subroutine UR_random_seed


    !#######################################################################

    real(rn) function rgamma(nvt,svor,first)

        ! generates a gamma distributed random number
        ! uses Ran_gamma8 (according to Marsaglia)
        ! and Random_gamma2

        ! Note: the rgamma generator can be tested within the routine MCCalc,
        ! around line 350, where 1.0_rn finds the 2.0_rn lines:

        !      ! Test gamma-distributed random values (de-activate GOTO 10):
        !      GOTO 10
        ! Enable the test by simply deactivating the GOTO 10!
        !

        use UR_DLIM, only: GamDistAdd

        implicit none

        integer, intent(in)      :: nvt    ! number of the quantity to generate a random gamma deviate for
        real(rn), intent(in)     :: svor   ! given value, for wich random gamma distrib. values
        ! are to be generated
        logical, intent(in)      :: first

        real(rn)    ::  s

        s = svor + GamDistAdd

        if(first) then
            ! The initialisation must be done for both of the random generators
                      
            if (s < 1.0_rn) then ! 26.3.2026 GK 
                rgamma = Random_gamma2(nvt,s, 1.0_rn,  .true.)    
            else 
                rgamma = Ran_Gamma8(nvt, s,.true.)
            end if
        else
            if(s < 1.0_rn) then
                rgamma = Random_gamma2(nvt,s,1.0_rn,.false.)
            else
                rgamma = Ran_Gamma8(nvt, s,.FALSE.)
            end if
        end if

    end function rgamma

    !#######################################################################

    real(rn) function Ran_Gamma8(nvt, svor, first)

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
        ! function GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
        ! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO GAMMA**(S-1)*EXP(-GAMMA),

        ! Modifications:
        ! If the generator is used for several quantities simultaneously, these
        ! quantities have to be differentiated by an identfication variable nvt, because
        ! at the initialisation (first=T) for each quantity individual variables
        ! c and d are created, i.e., arrays c_mars() and d_mars(). For each quantity with
        ! idenfication nvt the values c(nvt) and d(nvt) are used for further random
        ! numbers (with first=F).
        ! c_mars() and d_mars() are stored in the modul MCC.


        use ur_mcc,        only: c_mars, d_mars
        use UR_Gleich_globals,     only: ifehl

        implicit none

        integer, intent(in)    :: nvt    ! identification number of the quantity
        real(rn), intent(in)   :: svor   ! given value, for wich random gamma distrib. values
        ! are to be generated
        logical, intent(in)    :: first

        integer         :: kk0
        real(rn)        :: s
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
        if(svor < 1.0_rn - 1.e-8_rn) exception = .true.

        s = svor           !  + GamDistAdd
        !-----------------------------------------------------------------------
        if (s < 1.0_rn) then
            ! write(*, *) 'Shape parameter must be >= 1'
            ! STOP
        end if

        if (first) then
            if(.not.exception) d_mars(nvt) = svor - 1.0_rn/3.0_rn
            if(exception) d_mars(nvt) = svor - 1.0_rn/3.0_rn

            c_mars(nvt) = 1.0_rn/SQRT(9.0_rn*d_mars(nvt))
        end if

        ! Start of main loop
        do

            ! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.
            kk0 = 0
            do
                x = rnorm()
                v = (1.0_rn + c_mars(nvt)*x)**3._rn
                if (v > 0.0_rn) exit

                ! emergency exit:
                kk0 = kk0 + 1
                if(kk0 > 50000) then
                    !write(66,*) 'Error in Ran_gamma: 2nd loop: kk0 > 50000'
                    ifehl = 1
                    exit
                end if

            end do

            ! Generate uniform variable U
            call random_number(u)
            if (u < 1.0_rn - 0.03310_rn*x**4.0_rn) then
                Ran_Gamma8 = d_mars(nvt)*v
                exit
            else if (LOG(u) < 0.5_rn*x**2.0_rn + d_mars(nvt)*(1.0_rn - v + LOG(v))) then
                Ran_Gamma8 = d_mars(nvt)*v
                exit
            end if
        end do

        !if(exception) then
        !  u = Ran2d(idum)
        !  Ran_Gamma8 = Ran_Gamma8 * u**(1.0_rn/(s - 1.0_rn))
        !end if

        return
    end function Ran_Gamma8

!#######################################################################

    real(rn) function random_gamma2(nvt,s, b, first) ! result(fn_val)

        ! generate a gamma distributed random number for a given value s
        ! see comments in the function rgamma!

        ! Adapted from Fortran 77 code from the book:
        !     Dagpunar, J. 'Principles of random variate generation'
        !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

        ! function GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
        ! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO
        ! GAMMA2**(S-1) * EXP(-GAMMA2),
        ! USING A SWITCHING METHOD.

        !    S = SHAPE PARAMETER OF DISTRIBUTION
        !          (real < 1.0)
        !  b: scale factor


        use ur_params,   only: EPS1MIN
        use UR_Gleich_globals,   only: ifehl
        use ur_mcc,      only: a_rg, p_rg, c_rg, uf_rg, vr_rg, d_rg


        implicit none

        integer, intent(in)    :: nvt    ! identification number of the quantity
        real (rn), intent(in)  :: s      ! given value, for wich random gamma distrib. values
        ! are to be generated
        real (rn), intent(in)  :: b      ! scale factor
        logical, intent(in)    :: first  ! = true, if the first rnd number is to be generated

        real (rn)              :: fn_val
        !     local variables
        integer                :: j
        real (rn)              :: r, x, w


        ifehl = 0
        random_gamma2 = 0.0_rn
        if (s <= 0.0_rn .or. s >= 1.0_rn) then
            !write(63, *) 'shape parameter value outside permitted range'
        end if

        if (first) then       ! initialization
            a_rg(nvt) = 1.0_rn - s
            p_rg(nvt) = a_rg(nvt)/(a_rg(nvt) + s*exp(-a_rg(nvt)))
            if (s < EPS1MIN) then
                ifehl = 2
                !write(63, *) 'shape parameter value too small'
                return   ! stop
            end if
            c_rg(nvt) = 1.0_rn/s
            uf_rg(nvt) = p_rg(nvt)*(EPS1MIN/a_rg(nvt))**s
            vr_rg(nvt) = 1.0_rn - EPS1MIN
            d_rg(nvt) = a_rg(nvt)*log(a_rg(nvt))
        end if

        j = 0
        do
            j = j + 1
            if(j == 150) then
                ifehl = 3
                return
            end if
            call random_number(r)
            if (r >= vr_rg(nvt)) then
                cycle
            else if (r > p_rg(nvt)) then
                x = a_rg(nvt) - log((1.0_rn - r)/(1.0_rn - p_rg(nvt)))
                w = a_rg(nvt)*log(x)-d_rg(nvt)
            else if (r > uf_rg(nvt)) then
                x = a_rg(nvt)*(r/p_rg(nvt))**c_rg(nvt)
                w = x
            else
                fn_val = 0.0_rn
                return
            end if

            call random_number(r)
            if (1.0_rn-r <= w .and. r > 0.0_rn) then
                if (r*(w + 1.0_rn) >= 1.0_rn) cycle
                if (-log(r) <= w) cycle
            end if
            exit
        end do

        fn_val = x

        ! Now scale the random variable
        fn_val = b * fn_val
        random_gamma2 = fn_val

        return

    end function random_gamma2

!#######################################################################

    function random_beta(nvt,aa, bb, first) result(fn_val)

        ! generate a (2.0_rn-parameter) beta distributed random number

        ! Adapted from Fortran 77 code from the book:
        !     Dagpunar, J. 'Principles of random variate generation'
        !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

        ! function GENERATES A RANDOM VARIATE IN [0,1]
        ! FROM A BETA DISTRIBUTION WITH DENSITY
        ! PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
        ! USING CHENG'S LOG LOGISTIC METHOD.

        !     AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < real)
        !     BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < real)

        use ur_mcc,           only: d_rb,f_rb,h_rb,t_rb,c_rb,swap_rb
        use UR_Gleich_globals,        only: ifehl

        implicit none

        integer,intent(in)       :: nvt
        real(rn), intent(in)     :: aa, bb
        logical, intent(in)      :: first
        real(rn)                 :: fn_val

        !     local variables
        real(rn), parameter  :: aln4 = log(4._rn)            ! 1.3862944
        real(rn)             :: a, b, g, r, s, x, y, z

        real(rn)             :: vsmall = tiny(1.0_rn), vlarge = huge(1.0_rn)  ! flo: shouldn't this be epsilon(1.0_rn)?

        if (aa <= 0.0_rn .or. bb <= 0.0_rn) then
            write(0, *) 'Ran_beta: IMPERMISSIBLE SHAPE PARAMETER VALUE(S)'
            fn_val= 0.0_rn
            ifehl = 1
            return
        end if

        if (first) then     ! Initialization
            a = aa
            b = bb
            swap_rb(nvt) = b > a
            if (swap_rb(nvt)) then
                g = b
                b = a
                a = g
            end if
            d_rb(nvt) = a/b
            f_rb(nvt) = a + b
            if (b > 1.0_rn) then
                h_rb(nvt) = SQRT((2.0_rn*a*b - f_rb(nvt))/(f_rb(nvt) - 2.0_rn))
                t_rb(nvt) = 1.0_rn
            else
                h_rb(nvt) = b
                t_rb(nvt) = 1.0_rn/(1.0_rn + (a/(vlarge*b))**b)
            end if
            c_rb(nvt) = a + h_rb(nvt)
        end if

        do
            call random_number(r)
            call random_number(x)
            s = r*r*x
            if (r < vsmall .OR. s <= 0.0_rn) cycle
            if (r < t_rb(nvt)) then
                x = LOG(r/(1.0_rn - r))/h_rb(nvt)
                y = d_rb(nvt)*EXP(x)
                z = c_rb(nvt)*x + f_rb(nvt)*LOG((1.0_rn + d_rb(nvt))/(1.0_rn + y)) - aln4
                if (s - 1.0_rn > z) then
                    if (s - s*z > 1.0_rn) cycle
                    if (LOG(s) > z) cycle
                end if
                fn_val = y/(1.0_rn + y)
            else
                if (4.0_rn*s > (1.0_rn + 1.0_rn/d_rb(nvt))**f_rb(nvt)) cycle
                fn_val = 1.0_rn
            end if
            exit
        end do

        if (swap_rb(nvt)) fn_val = 1.0_rn - fn_val
        return
    end function random_beta

!####################################################################

    function random_t(nvt,m,first) result(fn_val)

        ! Adapted from Fortran 77 code from the book:
        !     Dagpunar, J. 'Principles of random variate generation'
        !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

        ! function GENERATES A RANDOM VARIATE FROM A
        ! T DISTRIBUTION USING KINDERMAN and MONAHAN'S RATIO METHOD.

        !     M = DEGREES OF FREEDOM OF DISTRIBUTION
        !           (1 <= 1NTEGER)

        use UR_MCC,        only: s_rt,c_rt,a_rt,f_rt,g_rt

        implicit none

        integer, intent(in)       :: nvt
        integer, intent(in)       :: m    ! dof
        logical,intent(in)        :: first
        real(rn)                  :: fn_val

        !     local variables
        real(rn)            :: r, x, v

        integer             :: mm = 0

        if (m < 1) then
            write(*, *) 'IMPERMISSIBLE DEGREES OF FREEDOM'
            fn_val = 0._rn  ! STOP
        end if

        ! if (m /= mm) then                    ! Initialization, if necessary
        if(first) then
            s_rt(nvt) = m
            c_rt(nvt) = -0.25_rn*(s_rt(nvt) + 1.0_rn)
            a_rt(nvt) = 4.0_rn/(1.0_rn + 1.0_rn/s_rt(nvt))**c_rt(nvt)
            f_rt(nvt) = 16.0_rn/a_rt(nvt)
            if (m > 1) then
                g_rt(nvt) = s_rt(nvt) - 1.0_rn
                g_rt(nvt) = ((s_rt(nvt) + 1.0_rn)/g_rt(nvt))**c_rt(nvt)*SQRT((s_rt(nvt)+s_rt(nvt))/g_rt(nvt))
            else
                g_rt(nvt) = 1.0_rn
            end if
            mm = m
        end if

        do
            call random_number(r)

            if (r <= 0.0_rn) cycle
            call random_number(v)

            x = (2.0_rn*v - 1.0_rn)*g_rt(nvt)/r
            v = x*x
            if (v > 5.0_rn - a_rt(nvt)*r) then
                if (m >= 1 .and. r*(v + 3.0_rn) > f_rt(nvt)) cycle
                if (r > (1.0_rn + v/s_rt(nvt))**c_rt(nvt)) cycle
            end if
            exit
        end do

        fn_val = x
        return
    end function random_t

!#######################################################################

    real(rn) function ran_Erlang(rate, xk)

        ! generate an Erlang-distributed random number

        !     Copyright (C) 2019-2023  Günter Kanisch

        implicit none

        real(rn), intent(in)         :: rate      ! lambda
        real(rn), intent(in)         :: xk        ! positive integer number

        integer   :: k, i
        real(rn)  :: prd, r
        !-------------------------------------------------------------
        k = int(xk)
        prd = 1.0_rn
        do i=1, k
            call random_number(r)
            prd = prd * r
        end do
        ran_Erlang = -log(prd) / rate

    end function ran_Erlang

!#######################################################################

    real(rn) function random_bipo2(p,N,Rb,tg)

        ! generate a random number distributed according to the distribution
        ! BinPoi_2_PDF: apply the rejection method for generating random numbers
        !
        ! Before using it, execute first scan_bipoi2(p,N,Rb,tg) to find the
        ! parameters bipoi2_maxk,bipoi2_hgt
        !
        !     Copyright (C) 2019-2023  Günter Kanisch

        use pdfs,              only: BinPoi_2_PDF
        use UR_Gleich_globals, only: bipoi2_maxk, bipoi2_hgt
        implicit none

        real(rn), intent(in)    :: p      ! paramter of the binomial distrib.
        real(rn), intent(in)    :: n      ! paramter of the binomial distrib.
        real(rn), intent(in)    :: rb     ! background count rate
        real(rn), intent(in)    :: tg     ! counting time of gross measurement

        integer         :: i,jmax
        real(rn)        :: pval,fakt,hg(3)
        real(rn)        :: xk,xkmin,xkmax,yk,ymin,ymax, r

        random_bipo2 = 0.0_rn
        ! apply the rejection method for generating random numbers from the BinPoi_2_PDF:
        xkmin = 0.0_rn
        xkmax = bipoi2_maxk
        ymax = bipoi2_hgt
        ymax = ymax * 1.01_rn
        ymin = 0.0_rn
        do i=1,200
            call random_number(r)
            xk = xkmin + r * (xkmax-xkmin)
            call random_number(r)
            yk = r * ymax
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

        use pdfs,        only: BinPoi_2_PDF
        use UR_Gleich_globals,   only: bipoi2_hgt,bipoi2_maxk

        implicit none

        real(rn), intent(in)    :: p      ! paramter of the binomial distrib.
        real(rn), intent(in)    :: n      ! paramter of the binomial distrib.
        real(rn), intent(in)    :: rb     ! background count rate
        real(rn), intent(in)    :: tg     ! counting time of gross measurement

        integer(4)     :: i,jmax
        real(rn)       :: sumP,pval,fakt,hg(3)

        sumP = 0.0_rn
        bipoi2_hgt = 0.0_rn
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

    function rnorm() result(fn_val)

        !! https://fortran-lang.discourse.group/t/normal-random-number-generator/3724/2
        !
        !              Based on Alan Miller's rnorm.f90,
        !   Generate a random normal deviate using the polar method.
        !   Reference: Marsaglia,G. & Bray,T.A. 'A convenient method for generating
        !              normal variables', Siam Rev., vol.6, 260-264, 1964.

        implicit none
        real(kind=rn)  :: fn_val

        ! local variables

        real(kind=rn)            :: u, sum
        real(kind=rn), save      :: v, sln
        logical, save            :: second = .false.
        real(kind=rn), parameter :: vsmall = tiny( 1.0_rn )  ! flo: shouldn't this be epsilon(1.0_rn)?

        if (second) then
            ! If second, use the second random number generated on last call

            second = .false.
            fn_val = v*sln

        else
            ! First call; generate a pair of random normals

            second = .true.
            do
                call random_number(u)
                call random_number(v)
                u = SCALE( u, 1 ) - 1.0_rn
                v = SCALE( v, 1 ) - 1.0_rn
                sum = u*u + v*v + vsmall         ! vsmall added to prevent LOG(0.0_rn) / 0.0_rn
                if(sum < 1.0_rn) exit
            end do
            sln = SQRT(-SCALE(LOG(sum),1)/sum)
            fn_val = u*sln
        end if
    end function rnorm

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
        real(rn) r

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
                call random_number(r)
                u = 2.0_rn * r - 1.0_rn
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

        ! function GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
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

    subroutine MulNormRnd(DPLUS,A,X,N)

        ! generation of random numbers from multivaraiate normal
        implicit none

        integer, intent(in)    :: N            ! number of variables
        real(rn), intent(in)       :: A(N)         ! mean values of variables
        real(rn), intent(out)      :: X(N)         ! N random values, one per variable
        real(rn), intent(in)       :: DPLUS(N,N)   !

        integer(4)    :: i
        real(rn)      :: R(N)

        do i=1,N
        R(i) = rnorm()
        end do
        X = matmul(DPLUS,R)
        X(1:N) = X(1:N) + A(1:N)

        end subroutine MulNormRnd

end module RND
