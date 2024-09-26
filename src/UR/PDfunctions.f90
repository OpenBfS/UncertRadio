!-------------------------------------------------------------------------------------------------!
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
!-------------------------------------------------------------------------------------------------!

module Pdfs
    use UR_params,     only: rn, EPS1MIN, ONE, TWO, ZERO, pi
    ! this module contains functions for probability distribution densities

    !    contains:
    ! PoissonPDF
    ! NormalPDF
    ! BinomPDF
    ! BinPoi_2_PDF
    ! psi

contains

    !#######################################################################

    real(rn) function PoissonPDF(N, lambda, t)

        !      Copyright (C) 2014-2023  Günter Kanisch



        implicit none

        real(rn),intent(in)    :: lambda    ! mean value of the count rate
        real(rn),intent(in)    :: N         ! measured number of counts
        real(rn),intent(in)    :: t         ! mesaurement time, or a number channels

        integer(4)    :: i

        if(abs(lambda) < 1.E-26_rn .and. abs(N) < 1.E-26_rn) then
            PoissonPDF = ONE
            return
        end if
        if(abs(lambda) < 1.E-26_rn .and. abs(N) > 1.E-26_rn) then
            PoissonPDF = ZERO
            return
        end if

        if(abs(N) <= 140._rn) then
            PoissonPDF = N*log(lambda*t) - lambda*t - log(gamma(N+ONE))
            PoissonPDF = exp(PoissonPDF)
        elseif(N > 140._rn) then
            ! Stirling formula:
            Poissonpdf = N*(ONE + log(lambda*t/N)) - lambda*t
            Poissonpdf = exp(PoissonPDF) / sqrt(TWO*Pi*(N + ONE/6._rn))
        else
            PoissonPDF = NormalPDF(N, lambda*t, sqrt(lambda*t))
        end if
        return

    end function PoissonPDF

    !######################################################################

    real(rn) function NormalPDF(x, x0, ux0)

        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_params,     only: rn,pi,ONE,TWO
        implicit none

        real(rn),intent(in)    :: x         ! a value of the normal distribution
        real(rn),intent(in)    :: x0,ux0    ! mean and standard uncertainty of the normal distrib.

        real(rn)      :: help, help1

        help = -(x - x0)**TWO / (TWO*ux0**TWO)
        help1 = ONE / ( sqrt(TWO*Pi)*ux0 )
        NormalPDF = EXP(help)/( sqrt(TWO*Pi)*ux0)

    end function NormalPDF

!######################################################################

    real(rn) function BinomPDF(k, N, p)

        ! binomial distrib.: probability of counting k decay events from N atoms  (k <= N)
        !     Copyright (C) 2014-2023  Günter Kanisch

        use Brandt,        only: glngam

        implicit none

        real(rn),intent(in)      :: k
        real(rn),intent(in)      :: N      ! parameter
        real(rn),intent(in)      :: p      ! parameter

        real(rn)      :: help

        BinomPDF = ZERO
        if(p > ZERO) then
            if(abs(k) < EPS1MIN .and. abs(N) < EPS1MIN) then
                BinomPDF = ONE
                return
            end if
        else
            return
        end if
        if(k > N) then
            BinomPDF = ZERO
            return
        end if

        help = glngam(N+ONE) - glngam(k+ONE) - glngam(n-k+ONE)
        help = help + k*log(p) + (n-k)*log(ONE - p)

        BinomPDF = EXP(help)

    end function BinomPDF

!######################################################################

    subroutine BinPoi_2_PDF(y, N, p, Rb,tm, pval,  fakt,hg,jmax,use_derv1)

        ! calculates the probability pval of counting y gross counts under the
        ! condition that background counts are Poisson-distsibuted and the
        ! sample activity contribution is binomial distributed.
        ! This routine is based on hypergeometric functions and allows also
        ! for non-integer gross counts y.
        ! It has been written (GK) following the references:
        !
        !     MathStatica, 2019. Sum of Binomial and Poisson.
        !     http://www.mathstatica.com/SumBinomialPoisson/
        ! and
        !     Pearson, J.W., Olver, S., Porter, M.A., 2016. Numerical methods
        !     for the computation of the confluent and Gauss hypergeometric
        !     functions. Numer Algor. DOI 10.1007/s11075-016-0173-0
        !
        ! See also:  NIST/DLMF   : https://dlmf.nist.gov/13.2
        !
        !     Copyright (C) 2014-2023  Günter Kanisch


        use Brandt,        only: glngam
        use UR_Gleich,     only: ifehl,ifehl_string

        use CHF,           only: isNaN

        implicit none

        real(rn),intent(in)     :: y           ! a number of gross counts
        real(rn),intent(in)     :: N           ! parameter N of the binomial distrib. part
        real(rn),intent(in)     :: p           ! parameter p of the binomial distrib. part
        real(rn),intent(in)     :: Rb          ! background coun rate
        real(rn),intent(in)     :: tm          ! count time of the gross measurement
        real(rn),intent(out)    :: pval        ! probability, = fakt*hg(1)
        real(rn),intent(out)    :: fakt, hg(3)
        integer(4),intent(out)  :: jmax
        logical, intent(in)     :: use_derv1   ! calculate the 1st derivative with respect to N ?

        real(rn)           :: z,a,b,asv,bsv,zsv,ys,dMda,dMdb,dadN,dbDn,dMdN,dFdN
        logical            :: Kummer_trans


        if(abs(N) < EPS1MIN) then
            pval = PoissonPDF(y, Rb, tm)
            !!!! return
        end if
        if(Rb < EPS1MIN .or. p < EPS1MIN) then
            pval = ZERO
            return
        end if

        z = (p-ONE)*(Rb*tm)/p   ! = symbol Z

! At y = N, where a switch between two different functions occurs, this function can be
! dis-continuous.
        if(y <= N) then
            a = -y
            b = N-y+ONE
            Fakt = -Rb*tm + (N-y)*log(ONE-p) + y*log(p) + glngam(N+ONE) - glngam(y+ONE) - glngam(N-y+ONE)
            Fakt = exp(Fakt)
            if(use_derv1) then
                dadN = ZERO
                dbdN = ONE
                dFdN = Fakt*(log(ONE - p) + psi(N+ONE) - psi(N-y+ONE) )
            end if
        else
            a = -N
            ys = (y + 0._rn*1.E-4_rn*y)
            b = ys - N + ONE               ! b = z-N+one
            Fakt = p**N * PoissonPDF(ys-N,Rb,tm)
            if(use_derv1) then
                dadN = -ONE
                dbdN = -ONE
                dFdN = Fakt*(log(p) - log(Rb*tm) + psi(ys-N+ONE))
            end if
        end if
        asv = a
        bsv = b
        zsv = z

        ! For a < 0, the Kummertransformation is applied, executed then with the routine SelfKummer
        Kummer_trans = .false.
        ! if(a < -eps1min) Kummer_trans = .true.
        if(a < EPS1MIN) Kummer_trans = .true.
        if(b < ZERO .and. abs(abs(b)-abs(floor(b))) < EPS1MIN) bsv = bsv*(ONE+1.E-10_rn)

        call SelfKummer(asv, bsv, zsv, hg(1), jmax, dMda,dMdb,use_derv1)
        pval = fakt*hg(1)
        ! pval = pval * exp(x)

        if(use_derv1) then
            if(.not.Kummer_trans) then
                dMdN = dMda*dadN + dMdb*dbdN
            else
                dMdN = dMda*(-dadN + dbdN) + dMdb*dbdN
            end if
            hg(2) = dMdN
            hg(3) = dFdN
            !write(28,'(8(a,es12.5),a,i4)') 'Mfunc=',hg(1),' dMda=',dMda,' dMdb=',dMdb,' dadN=',dadN,' dbdN=',dbdN, &
            !                         ' a=',a,' b=',b,' z=',x,' jmax=',jmax
        end if

        if(ISNAN(fakt*hg(1))) then
            write(28,*) 'BinPoi_2_: zsv=',sngl(zsv),' N=',sngl(N),' fakt=',sngl(fakt),' hg=',sngl(hg)
            write(28,*) '      p=',sngl(p),' Rb=',sngl(Rb),' tm=',sngl(tm)
            ifehl = 1
            ifehl_string = 'BinPoi_2 = NaN occurred in BinPoi_2'
            write(28,*) 'ifehl = 1: ',trim(ifehl_string)
            write(66,*) 'ifehl = 1: ',trim(ifehl_string)
        end if

    end subroutine BinPoi_2_PDF

!######################################################################


    FUNCTION psi(xx) RESULT(fn_val)

        ! this function calculates the digamma function and is required in
        ! BinPoi_2_PDF. Its source was taken from a MODULE inc_beta, which
        ! was published by Alan Miller.

        !---------------------------------------------------------------------

        !                 EVALUATION OF THE DIGAMMA FUNCTION

        !                           -----------

        !     PSI(XX) IS ASSIGNED THE VALUE 0 WHEN THE DIGAMMA FUNCTION CANNOT
        !     BE COMPUTED.

        !     THE MAIN COMPUTATION INVOLVES EVALUATION OF RATIONAL CHEBYSHEV
        !     APPROXIMATIONS PUBLISHED IN MATH. COMP. 27, 123-127(1973) BY
        !     CODY, STRECOK AND THACHER.

        !---------------------------------------------------------------------
        !     PSI WAS WRITTEN AT ARGONNE NATIONAL LABORATORY FOR THE FUNPACK
        !     PACKAGE OF SPECIAL FUNCTION SUBROUTINES. PSI WAS MODIFIED BY
        !     A.H. MORRIS (NSWC).
        !---------------------------------------------------------------------
        IMPLICIT NONE

        REAL (rn), INTENT(IN) :: xx
        REAL (rn)             :: fn_val

        REAL (rn) :: dx0 = 1.461632144968362341262659542325721325_rn
        !---------------------------------------------------------------------

        !     PIOV4 = PI/4
        !     DX0 = ZERO OF PSI TO EXTENDED PRECISION

        !---------------------------------------------------------------------
        REAL (rn) :: aug, den, piov4 = .785398163397448_rn, sgn, upper,  &
            w, x, xmax1, xmx0, xsmall, z
        INTEGER   :: i, m, n, nq
        !---------------------------------------------------------------------

        !     COEFFICIENTS FOR RATIONAL APPROXIMATION OF
        !     PSI(X) / (X - X0),  0.5 <= X <= 3.0

        !---------------------------------------------------------------------
        REAL (rn) :: p1(7) = (/ .895385022981970E-02_rn, .477762828042627E+01_rn,  &
            .142441585084029E+03_rn, .118645200713425E+04_rn,  &
            .363351846806499E+04_rn, .413810161269013E+04_rn,  &
            .130560269827897E+04_rn /),   &
            q1(6) = (/ .448452573429826E+02_rn, .520752771467162E+03_rn,  &
            .221000799247830E+04_rn, .364127349079381E+04_rn,  &
            .190831076596300E+04_rn, .691091682714533E-05_rn /)
        !---------------------------------------------------------------------

        !     COEFFICIENTS FOR RATIONAL APPROXIMATION OF
        !     PSI(X) - LN(X) + 1 / (2*X),  X > 3.0

        !---------------------------------------------------------------------
        REAL (rn) :: p2(4) = (/ -.212940445131011E+01_rn, -.701677227766759E+01_rn,  &
            -.448616543918019E+01_rn, -.648157123766197E+00_rn /), &
            q2(4) = (/  .322703493791143E+02_rn,  .892920700481861E+02_rn,  &
            .546117738103215E+02_rn,  .777788548522962E+01_rn /)
        !---------------------------------------------------------------------

        !     MACHINE DEPENDENT CONSTANTS ...

        !        XMAX1  = THE SMALLEST POSITIVE FLOATING POINT CONSTANT
        !                 WITH ENTIRELY INTEGER REPRESENTATION.  ALSO USED
        !                 AS NEGATIVE OF LOWER BOUND ON ACCEPTABLE NEGATIVE
        !                 ARGUMENTS AND AS THE POSITIVE ARGUMENT BEYOND WHICH
        !                 PSI MAY BE REPRESENTED AS ALOG(X).

        !        XSMALL = ABSOLUTE ARGUMENT BELOW WHICH PI*COTAN(PI*X)
        !                 MAY BE REPRESENTED BY 1/X.

        !---------------------------------------------------------------------
        xmax1 = MIN(real(huge(1), rn), ONE / EPS1MIN)
        xsmall = 1.E-11_rn   !  1.E-9_rn
!---------------------------------------------------------------------
        x = xx
        aug = ZERO
        IF (x >= 0.5_rn) GO TO 200
!---------------------------------------------------------------------
!     X .LT. 0.5,  USE REFLECTION FORMULA
!     PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
!---------------------------------------------------------------------
        IF (ABS(x) > xsmall) GO TO 100
        IF (abs(x) < EPS1MIN) GO TO 400
!---------------------------------------------------------------------
!     0 .LT. ABS(X) .LE. XSMALL.  USE 1/X AS A SUBSTITUTE
!     FOR  PI*COTAN(PI*X)
!---------------------------------------------------------------------
        aug = -ONE / x
        GO TO 150
!---------------------------------------------------------------------
!     REDUCTION OF ARGUMENT FOR COTAN
!---------------------------------------------------------------------
100     w = - x
        sgn = piov4
        IF (w > ZERO) GO TO 120
        w = - w
        sgn = -sgn
!---------------------------------------------------------------------
!     MAKE AN ERROR EXIT IF X .LE. -XMAX1
!---------------------------------------------------------------------
120     IF (w >= xmax1) GO TO 400
        nq = INT(w)
        w = w - nq
        nq = INT(w*4.0_rn)
        w = 4.0_rn * (w - nq * .25_rn)
!---------------------------------------------------------------------
!     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X.
!     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST
!     QUADRANT AND DETERMINE SIGN
!---------------------------------------------------------------------
        n = nq / 2
        IF ((n+n) /= nq) w = ONE - w
        z = piov4 * w
        m = n / 2
        IF ((m+m) /= n) sgn = - sgn
!---------------------------------------------------------------------
!     DETERMINE FINAL VALUE FOR  -PI*COTAN(PI*X)
!---------------------------------------------------------------------
        n = (nq + 1) / 2
        m = n / 2
        m = m + m
        IF (m /= n) GO TO 140
!---------------------------------------------------------------------
!     CHECK FOR SINGULARITY
!---------------------------------------------------------------------
        IF (abs(z) < EPS1MIN) GO TO 400
!---------------------------------------------------------------------
!     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND
!     SIN/COS AS A SUBSTITUTE FOR TAN
!---------------------------------------------------------------------
        aug = sgn * ((COS(z) / SIN(z)) * 4.0_rn)
        GO TO 150
140     aug = sgn * ((SIN(z) / COS(z)) * 4.0_rn)
150     x = ONE - x
200     IF (x > 3.0_rn) GO TO 300
!---------------------------------------------------------------------
!     0.5 .LE. X .LE. 3.0
!---------------------------------------------------------------------
        den = x
        upper = p1(1) * x

        DO i = 1, 5
            den = (den + q1(i)) * x
            upper = (upper + p1(i+1)) * x
        END DO

        den = (upper + p1(7)) / (den + q1(6))
        xmx0 = x - dx0
        fn_val = den * xmx0 + aug
        RETURN
!---------------------------------------------------------------------
!     IF X .GE. XMAX1, PSI = LN(X)
!---------------------------------------------------------------------
300     IF (x >= xmax1) GO TO 350
!---------------------------------------------------------------------
!     3.0 .LT. X .LT. XMAX1
!---------------------------------------------------------------------
        w = ONE / (x * x)
        den = w
        upper = p2(1) * w

        DO i = 1, 3
            den = (den + q2(i)) * w
            upper = (upper + p2(i+1)) * w
        END DO

        aug = upper / (den + q2(4)) - 0.5_rn / x + aug
350     fn_val = aug + LOG(x)
        RETURN
!---------------------------------------------------------------------
!     ERROR RETURN
!---------------------------------------------------------------------
400     fn_val = ZERO
        RETURN
    END FUNCTION psi

!######################################################################

end module Pdfs
