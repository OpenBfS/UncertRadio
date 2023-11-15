
MODULE chgm_func

use UR_params,      only: rn

! From the book "Computation of Special Functions"
!      by Shanjie Zhang and Jianming Jin
!   Copyright 1996 by John Wiley & Sons, Inc.
! The authors state:
!   "However, we give permission to the reader who purchases this book
!    to incorporate any of these programs into his or her programs
!    provided that the copyright is acknowledged."

IMPLICIT NONE
! INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
INTEGER, PARAMETER  :: dp = rn                ! 22.6.2019

CONTAINS


SUBROUTINE chgm(a, b, x, hg)
use UR_params,       only: eps1min

!       ===================================================
!       Purpose: Compute confluent hypergeometric function
!                M(a,b,x)
!       Input  : a  --- Parameter
!                b  --- Parameter ( b <> 0,-1,-2,... )
!                x  --- Argument
!       Output:  HG --- M(a,b,x)
!       Routine called: GAMMA for computing ג(x)
!       ===================================================
!       The version by Alan Miller was used and slightly modified by GK;
!       gamma function replaced by the compilers intrinsic gamma function

REAL (dp), INTENT(IN OUT)  :: a
REAL (dp), INTENT(IN)      :: b
REAL (dp), INTENT(IN OUT)  :: x
REAL (dp), INTENT(OUT)     :: hg

REAL (dp), PARAMETER  :: pi = 3.141592653589793_dp
REAL (dp)  :: a0, a1, hg1, hg2, r, r1, r2, rg, sum1, sum2, ta, tb, tba,  &
              x0, xg, y0, y1
INTEGER    :: i, j, k, la, m, n, nl

a0 = a
a1 = a
x0 = x
hg = 0.0_dp
IF (abs(b) < eps1min .OR. abs(b + ABS(INT(b))) < eps1min)  THEN
  hg = 1.0E+300_dp
ELSE IF (abs(a) < eps1min .OR. abs(x) < eps1min) THEN
  hg = 1.0_dp
ELSE IF (abs(a + 1.0_dp) < eps1min) THEN
  hg = 1.0_dp - x / b
ELSE IF (abs(a - b) < eps1min) THEN
  hg = EXP(x)
ELSE IF (abs(a-b - 1.0_dp) < eps1min) THEN
  hg = (1.0_dp + x/b) * EXP(x)
ELSE IF (abs(a - 1.0_dp) < eps1min .AND. abs(b - 2.0_dp) < eps1min) THEN
  hg = (EXP(x)-1.0_dp) / x
ELSE IF (abs(a - real(INT(a),dp)) < eps1min .AND. a < 0.0_dp) THEN
  m = INT(-a)
  r = 1.0_dp
  hg = 1.0_dp
  DO  k = 1, m
    r = r * (a+real(k-1,dp)) / real(k,rn) / (b+real(k-1,dp)) * x
    hg = hg + r
  END DO
END IF
   ! IF (hg /= 0.0_dp) RETURN        ! deactivated 16.7.2019
IF (x < 0.0_dp) THEN
  a = b - a
  a0 = a
  x = ABS(x)
END IF
IF (a < 2.0_dp) nl = 0
IF (a >= 2.0_dp) THEN
  nl = 1
  la = INT(a)
  ! a = a - la - 1
  a = a - real(la+1,dp)
END IF
DO  n = 0, nl
  IF (a0 >= 2.0_dp) a = a + 1.0_dp
  IF (x <= 20.0_dp + ABS(b) .OR. a < 0.0_dp) THEN       ! 16.7.2019
    hg = 1.0_dp
    rg = 1.0_dp
    DO  j = 1, 700
      rg = rg * (a+real(j-1,dp)) / (real(j,dp)*(b+real(j-1,dp))) * x
      hg = hg + rg
      IF (ABS(rg/hg) < 1.0E-20_dp) GO TO 40
    END DO
  ELSE
      ta = gamma(a)
      tb = gamma(b)
    xg = b - a
      tba = gamma(xg)
    sum1 = 1.0_dp
    sum2 = 1.0_dp
    r1 = 1.0_dp
    r2 = 1.0_dp
    DO  i = 1, 8
      r1 = -r1 * (a+real(i-1,dp)) * (a-b+real(i,dp)) / (x*real(i,dp))
      r2 = -r2 * (b-a+real(i-1,dp)) * (a-real(i,dp)) / (x*real(i,dp))
      sum1 = sum1 + r1
      sum2 = sum2 + r2
    END DO
    hg1 = tb / tba * x ** (-a) * COS(pi*a) * sum1
    hg2 = tb / ta * EXP(x) * x ** (a-b) * sum2
    hg = hg1 + hg2
  END IF
  40 IF (n == 0) y0 = hg
  IF (n == 1) y1 = hg
END DO
IF (a0 >= 2.0_dp) THEN
  DO  i = 1, la - 1
    hg = ((2.0_dp*a-b+x)*y1 + (b-a)*y0) / a
    y0 = y1
    y1 = hg
    a = a + 1.0_dp
  END DO
END IF
IF (x0 < 0.0_dp) hg = hg * EXP(x0)
a = a1
x = x0
RETURN
END SUBROUTINE chgm

END MODULE chgm_func

!##########################################################################

Subroutine SelfKummer(a0,b0,z0, hg2, jmax,dMda,dMdb,use_derv1)

! Computes the confluent Hypergeometric function 1F1,
! according to methods by Pearson et al. (2016):
!
!   Numerical methods for the computation
!   of the confluent and Gauss hypergeometric functions
!   J.W. Pearson, S. Olver, M.A. Porter
!   Numer Algor
!   DOI 10.1007/s11075-016-0173-0
! © Springer Science+Business Media New York 2016

use UR_params,       only:  rn, eps1min,zero,one

implicit none

real(rn),intent(in)    :: a0,b0,z0
real(rn),intent(out)   :: hg2
integer(4),intent(out) :: jmax
real(rn),intent(out)   :: dMda,dMdb
logical,intent(in)     :: use_derv1   ! calculate the 1st derivative with respect to N ?

integer(4)       :: j,i2
real(rn)         :: Sminus1, S0, S1, Sj, Sj_minus1, Sj_minus2, rj,a,b,z
real(rn)         :: Sj_1,aklam,bklam,suma_inv,sumb_inv
real(rn)         :: Sdja,Sdjb, Sdja_minus1,Sdja_minus2,Sdjb_minus1,Sdjb_minus2
logical          :: Kummer_trans

a = a0
b = b0
z = z0
jmax = 0

! compute a Taylor series (Eq. 3.2) with their method (b):
Kummer_trans = .false.
! if(a < -eps1min) then
if(a < eps1min) then        ! 23.10.2021
  ! Kummer transformation:
  a = b-a
  z = -z
  Kummer_trans = .true.
endif

i2 = 0
Sminus1 = one
S0 = one
S1 = one + a/b*z
Sj_minus1 = S1
Sj_minus2 = S0
! Extensions for the first derivatives with respect to a or b:
dMda = one
dMdb = one
suma_inv = one/a
sumb_inv = one/b
Sdja_minus1 = one/b*z
Sdja_minus2 = zero
Sdjb_minus1 = -a/b/b*z
Sdjb_minus2 = zero

do j=2,1500
  !  exit
  aklam = (a + real(j-1,rn))
  bklam = (b + real(j-1,rn))
  rj = aklam / (real(j,rn) * bklam)
  Sj = Sj_minus1 + (Sj_minus1 - Sj_minus2)*rj*z
  if(use_derv1) then
    suma_inv = suma_inv + one/aklam
    sumb_inv = sumb_inv + one/bklam
    Sdja = Sdja_minus1 + (Sj_minus1 - Sj_minus2)*rj*z*suma_inv
    Sdjb = Sdjb_minus1 + (Sj_minus1 - Sj_minus2)*rj*z*sumb_inv*(-one)
  endif
  ! stopping criterion:
  if(j > 4 .and. abs(Sj-Sj_minus1)/abs(Sj) < 1.e-18_rn ) then
    i2 = i2 + 1
    if(i2 == 3) then
      jmax = j
      exit
    endif
  end if
    Sj_minus2 = Sj_minus1
    Sj_minus1 = Sj
    if(use_derv1) then
      Sdja_minus2 = Sdja_minus1
      Sdja_minus1 = Sdja
      Sdjb_minus2 = Sdjb_minus1
      Sdjb_minus1 = Sdjb
    endif
end do

Sj_1 = Sj
hg2 = Sj
dMda = Sdja
dMdb = Sdjb

if(Kummer_trans) then
  hg2 = exp(-z)*hg2
  if(use_derv1) then
    dMda = dMda * exp(-z)
    dMdb = dMdb * exp(-z)
  endif
end if

return

end Subroutine SelfKummer
