
module Pdfs

   ! this module contains functions for probability distribution densities

          !    contains:
          ! PoissonPDF
          ! GammaPDF
          ! NormalPDF
          ! BinomPDF
          ! NBinomPDF
          ! BinPoiPDF
          ! BinPoi_2_PDF
          ! beta
          ! ErlangPDF
          ! psi

contains

!#######################################################################

real(rn) function PoissonPDF(N, lambda, t)

!     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,eps1min,one,two,zero,Pi

implicit none

real(rn),intent(in)    :: lambda    ! mean value of the count rate
real(rn),intent(in)    :: N         ! measured number of counts
real(rn),intent(in)    :: t         ! mesaurement time, or a number channels

integer(4)    :: i

if(abs(lambda) < 1.E-26_rn .and. abs(N) < 1.E-26_rn) then
  PoissonPDF = one
  return
end if
if(abs(lambda) < 1.E-26_rn .and. abs(N) > 1.E-26_rn) then
  PoissonPDF = zero
  return
end if

if(abs(N) <= 140._rn) then
  PoissonPDF = N*log(lambda*t) - lambda*t - log(gamma(N+one))
  PoissonPDF = exp(PoissonPDF)
! elseif(N > 140._rn .and. N < 2500._rn) then
elseif(N > 140._rn) then
  ! Stirling formula:
  Poissonpdf = N*(one + log(lambda*t/N)) - lambda*t
  Poissonpdf = exp(PoissonPDF) / sqrt(two*Pi*(N + one/6._rn))
else
  PoissonPDF = NormalPDF(N, lambda*t, sqrt(lambda*t))
end if
return

end function PoissonPDF

!######################################################################

real(rn) function GammaPDF(lambda, alpha,beta)

!     Copyright (C) 2014-2023  Günter Kanisch

use Brandt,        only: glngam
use UR_params,     only: rn,one

implicit none

real(rn),intent(in)   :: lambda    ! Variable, e.g., count rate in 1/s
real(rn),intent(in)   :: alpha     ! Parameter: alpha=counts +1;
real(rn),intent(in)   :: beta      ! Parameter: beta=measurement time; or number of channels

logical    :: even

if(alpha < 1700._rn*100._rn) then
  even = abs(real(int(alpha+.1_rn),rn) - alpha) < epsilon(one)
  GammaPDF = (alpha-one)*log(max(1.E-10_rn,lambda)) - beta*lambda + alpha*log(beta)
  GammaPDF = GammaPDF - glngam(alpha)
  GammaPDF = exp(GammaPDF)
else
  GammaPDF = NormalPDF(lambda, (alpha-one)/beta, sqrt(alpha-one)/beta)
end if

end function GammaPDF

!######################################################################

real(rn) function NormalPDF(x, x0, ux0)

!     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,pi,one,two
implicit none

real(rn),intent(in)    :: x         ! a value of the normal distribution
real(rn),intent(in)    :: x0,ux0    ! mean and standard uncertainty of the normal distrib.

real(rn)      :: help, help1

help = -(x - x0)**two / (two*ux0**two)
help1 = one / ( sqrt(two*Pi)*ux0 )
NormalPDF = EXP(help)/( sqrt(two*Pi)*ux0)

end function NormalPDF

!######################################################################

real(rn) function BinomPDF(k, N, p)

  ! binomial distrib.: probability of counting k decay events from N atoms  (k <= N)
  !     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,eps1min,one,zero
!use Brandt,        only: gammln,betai
use Brandt,        only: glngam,gincbt

implicit none

real(rn),intent(in)      :: k
real(rn),intent(in)      :: N      ! parameter
real(rn),intent(in)      :: p      ! parameter

real(rn)      :: help

BinomPDF = zero
if(p > zero) then
  if(abs(k) < eps1min .and. abs(N) < eps1min) then
    BinomPDF = one
    return
  end if
else
  return
end if
if(k > N) then
  BinomPDF = zero
  return
end if

help = glngam(N+one) - glngam(k+one) - glngam(n-k+one)
help = help + k*log(p) + (n-k)*log(one - p)

BinomPDF = EXP(help)

end function BinomPDF

!######################################################################


real(rn) function NBinomPDF(k, N, p)

! negative binomial distribution:
!     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,pi,one,zero
use Brandt,        only: glngam

implicit none

real(rn),intent(in)      :: k,N,p      ! parameter p
                                       ! N > 0;  k=0,1,2,3...;   p < 1
                                        ! for Mathews&Gerts:  use with BG+1 as BG
                               ! b entspr.  k
                               ! a entspr. tb/tm
                               ! BG enstpr. N

!  wiki:  f(k;r,p)
!         CDF = 1 - I_p(k+1, r)      ( I_p: regularized incomplete beta function )
!         CDF in R:  1 - pbeta(p, k+1, r)


!   M&G:    Wiki:   diese Funktion:
!-----------------------------------
!    b        k        k            k: 0,1,2,3,....
!    B        r        N            r (integer) > 0;
! a/(1+a)     p        p
!                     es ist:  a/(1+a) = 1-1/(1+a)
!
!  It is important, how to differentiate bewteen p and (1-p) .

! mean    :  N*(1 - p)/p
! var     :  mean/p

NBinomPDF = glngam(k + N) - glngam(N) - glngam(k+one)
NBinomPDF = NBinomPDF + k*log(p) + N*log(one - p)
NBinomPDF = exp(NBinomPDF)

 return

end function NBinomPDF

!#########################################################################


real(rn) function BinPoiPDF(mode, z, N, p, Rnet,Rb,tb,tm)

  ! calculates for mode=2 the probability pval of counting z gross counts
  ! under the condition that background counts are Poisson-distsibuted and the
  ! sample activity related counts are binomial-distributed.

  !     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,pi,eps1min,zero,one,two

implicit none

integer(4),intent(in)   :: mode        ! 1:  binom x poisson;  2: poisson x poisson
real(rn),intent(in)     :: z           ! a number of gross counts
real(rn),intent(in)     :: N           ! parameter N of the binomial distrib. part
real(rn),intent(in)     :: p           ! parameter p of the binomial distrib. part
real(rn),intent(in)     :: Rb          ! background coun rate
real(rn),intent(in)     :: tm          ! count time of the gross measurement
real(rn),intent(in)     :: tb          ! count time of the background measurement
real(rn),intent(in)     :: Rnet        ! net count rate

real(rn)      :: xk,xn,tdb,tdmax,term,termx,xz,q
integer(4)    :: k,kimin,kimax,kmax_tdb,ksum,kNmin,kNmax,kPmin,kPmax

BinPoiPDF = zero

tdmax = zero
xn = N
xz = z
q = one / (one + tm/tb)

if(mode == 1 .or. Mode == 3) then
  kNmin = max(0, int(xn*p - 8._rn*sqrt(xn*p*(one-p)+4._rn)))
  kNmax = int(xn*p + 8._rn*sqrt(xn*p*(one-p)+4._rn))
  if(xn <= zero) kNmax = 3
  kNmax = min(kNmax, int(xn))
  kPmin = 0
  kPmin = max(0, int(Rb*tm - 8._rn*sqrt(Rb*tm+two)))
  kPmax = Rb*tm + 8._rn*sqrt(Rb*tm+two)
  kimin = max(kNmin,kPmin)
  kimax = min(kNmax,kPmax) + 5
elseif(mode == 2) then
  kimin = max(0, int(Rnet*tm - 7._rn*sqrt(Rnet*tm)))
  kimax = int(Rnet*tm + 7._rn*sqrt(Rnet*tm))
end if

 kimin = max(0, kimin-7)
 kimax = kimax + 150
        !if(z <= zero .or. mode == 3) write(28,*) 'kimin,kimax=',kimin,kimax,  &
        !                                   ' xz=',sngl(xz),' xn=',sngl(xn)
ksum = 0
do k=kimin,kimax
  xk = real(k,rn)
    ! write(28,*) 'z, xk=',sngl(z),sngl(xk)
  if(xz >= xk) then
    if(mode == 1) then           !  .or. mode == 3 .or. mode == 4) then
      tdb = zero
      tdb = BinomPDF(xk,xN,p)          ! sample contribution taken as binomial
     ! fderiv = one
      if(tdb >= one) ksum = ksum + 1
    elseif(mode == 2) then
      tdb = PoissonPDF(xk, Rnet, tm)    ! sample activity contribution as Poisson-distrib.
    end if

    if(tdb > tdmax) then; kmax_tdb = xk; tdmax = tdb; end if
    if(k > 0 .and. kmax_tdb > 0 .and. k > kmax_tdb .and. tdb < 1.e-12_rn) exit

    if(mode <= 2) term = PoissonPDF(xz-xk, Rb, tm)
    BinPoiPdf = BinPoiPDF + tdb * term
  end if
end do

! if(ksum > 0) BinPoiPDF = BinPoiPDF / real(ksum,rn)
! if(mqt == 2) write(28,*) 'mqt=2:  kimin,kimax=',kimin,kimax,' Rb=',sngl(Rb), &
!                          ' z=',sngl(z),' tb=',sngl(tb),' BinpoiPDF=',sngl(BinPoiPDF), &
!                          ' ksum=',ksum

end function BinPoiPDF

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

use UR_params,     only: rn,pi,eps1min,zero,one,two
use Brandt,        only: glngam
use chgm_func,     only: chgm
use UR_Gleich,     only: ifehl,ifehl_string
! USE, INTRINSIC   :: IEEE_ARITHMETIC
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


if(abs(N) < eps1min) then
  pval = PoissonPDF(y, Rb, tm)
  !!!! return
end if
if(Rb < eps1min .or. p < eps1min) then
  pval = zero
  return
end if

z = (p-one)*(Rb*tm)/p   ! = symbol Z

! At y = N, where a switch between two different functions occurs, this function can be
! dis-continuous.
if(y <= N) then
  a = -y
  b = N-y+one
  Fakt = -Rb*tm + (N-y)*log(one-p) + y*log(p) + glngam(N+one) - glngam(y+one) - glngam(N-y+one)
  Fakt = exp(Fakt)
  if(use_derv1) then
    dadN = zero
    dbdN = one
    dFdN = Fakt*(log(one - p) + psi(N+one) - psi(N-y+one) )
  end if
else
  a = -N
  ys = (y + 0._rn*1.E-4_rn*y)
  b = ys - N + one               ! b = z-N+one
  Fakt = p**N * PoissonPDF(ys-N,Rb,tm)
  if(use_derv1) then
    dadN = -one
    dbdN = -one
    dFdN = Fakt*(log(p) - log(Rb*tm) + psi(ys-N+one))
  end if
end if
asv = a
bsv = b
zsv = z

! For a < 0, the Kummertransformation is applied, executed then with the routine SelfKummer
Kummer_trans = .false.
! if(a < -eps1min) Kummer_trans = .true.
if(a < eps1min) Kummer_trans = .true.
if(b < zero .and. abs(abs(b)-abs(floor(b))) < eps1min) bsv = bsv*(one+1.E-10_rn)

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

real(rn) function beta(z,w)

   ! calcualtes the value of the beta function

use UR_params,    only: rn
use Brandt,       only: glngam

implicit none

real(rn), intent(in)   :: z,w

beta = exp( glngam(z) + glngam(w) - glngam(z+w))

end function beta

!######################################################################

real(rn) function ErlangPDF(t, rho, N)

   ! calculates the probability of the Erlang distribution, i.e.,
   ! the probability of having got a time t for the measurement
   ! condition of fixed (preset) counts.
   ! One can show:
   !  ErlangPDF(t, rho, N) = GammaPDF(t,N,rho)

   !     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,        only: rn,zero,one,two
use Brandt,           only: glngam

implicit none

real(rn),intent(in)     :: t
real(rn),intent(in)     :: rho  !   count rate        ! = N/t
real(rn),intent(in)     :: N    ! preset number (integer) of counts

ErlangPDF = exp(  (N-one)*log(t) + N*log(rho) - rho*t - glngam(N) )

end function ErlangPDF

!######################################################################


FUNCTION psi(xx) RESULT(fn_val)

    ! this function calculates the digamma function and is required in
    ! BinPoi_2_PDF. Its source was taken from a MODULE inc_beta, which
    ! was published by Alan Miller.

use UR_params,       only: rn,zero,one,three,four,eps1min
use constants_NSWC,  only: ipmpar,dpmpar
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

! integer(4),parameter    :: dp = rn
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
xmax1 = ipmpar(3)
xmax1 = MIN(xmax1, one/dpmpar(1))
xsmall = 1.E-11_rn   !  1.E-9_rn
!---------------------------------------------------------------------
x = xx
aug = zero
IF (x >= 0.5_rn) GO TO 200
!---------------------------------------------------------------------
!     X .LT. 0.5,  USE REFLECTION FORMULA
!     PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
!---------------------------------------------------------------------
IF (ABS(x) > xsmall) GO TO 100
IF (abs(x) < eps1min) GO TO 400
!---------------------------------------------------------------------
!     0 .LT. ABS(X) .LE. XSMALL.  USE 1/X AS A SUBSTITUTE
!     FOR  PI*COTAN(PI*X)
!---------------------------------------------------------------------
aug = -one / x
GO TO 150
!---------------------------------------------------------------------
!     REDUCTION OF ARGUMENT FOR COTAN
!---------------------------------------------------------------------
100 w = - x
sgn = piov4
IF (w > zero) GO TO 120
w = - w
sgn = -sgn
!---------------------------------------------------------------------
!     MAKE AN ERROR EXIT IF X .LE. -XMAX1
!---------------------------------------------------------------------
120 IF (w >= xmax1) GO TO 400
nq = INT(w)
w = w - nq
nq = INT(w*four)
w = four * (w - nq * .25_rn)
!---------------------------------------------------------------------
!     W IS NOW RELATED TO THE FRACTIONAL PART OF  4.0 * X.
!     ADJUST ARGUMENT TO CORRESPOND TO VALUES IN FIRST
!     QUADRANT AND DETERMINE SIGN
!---------------------------------------------------------------------
n = nq / 2
IF ((n+n) /= nq) w = one - w
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
IF (abs(z) < eps1min) GO TO 400
!---------------------------------------------------------------------
!     USE COS/SIN AS A SUBSTITUTE FOR COTAN, AND
!     SIN/COS AS A SUBSTITUTE FOR TAN
!---------------------------------------------------------------------
aug = sgn * ((COS(z) / SIN(z)) * four)
GO TO 150
140 aug = sgn * ((SIN(z) / COS(z)) * four)
150 x = one - x
200 IF (x > three) GO TO 300
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
300 IF (x >= xmax1) GO TO 350
!---------------------------------------------------------------------
!     3.0 .LT. X .LT. XMAX1
!---------------------------------------------------------------------
w = one / (x * x)
den = w
upper = p2(1) * w

DO i = 1, 3
  den = (den + q2(i)) * w
  upper = (upper + p2(i+1)) * w
END DO

aug = upper / (den + q2(4)) - 0.5_rn / x + aug
350 fn_val = aug + LOG(x)
RETURN
!---------------------------------------------------------------------
!     ERROR RETURN
!---------------------------------------------------------------------
400 fn_val = zero
RETURN
END FUNCTION psi

!######################################################################

end module Pdfs
