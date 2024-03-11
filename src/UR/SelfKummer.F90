
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
