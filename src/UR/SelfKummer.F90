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

    use UR_types,  only: rn
    use UR_params, only: EPS1MIN, ZERO, ONE

    implicit none

    real(rn),intent(in)    :: a0,b0,z0
    real(rn),intent(out)   :: hg2
    integer,intent(out)    :: jmax
    real(rn),intent(out)   :: dMda,dMdb
    logical,intent(in)     :: use_derv1   ! calculate the 1st derivative with respect to N ?

    integer          :: j,i2
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
    if(a < EPS1MIN) then        ! 23.10.2021
        ! Kummer transformation:
        a = b-a
        z = -z
        Kummer_trans = .true.
    endif

    i2 = 0
    Sminus1 = ONE
    S0 = ONE
    S1 = ONE + a/b*z
    Sj_minus1 = S1
    Sj_minus2 = S0
    ! Extensions for the first derivatives with respect to a or b:
    dMda = ONE
    dMdb = ONE
    suma_inv = ONE/a
    sumb_inv = ONE/b
    Sdja_minus1 = ONE/b*z
    Sdja_minus2 = ZERO
    Sdjb_minus1 = -a/b/b*z
    Sdjb_minus2 = ZERO

    do j=2,1500
        !  exit
        aklam = (a + real(j-1,rn))
        bklam = (b + real(j-1,rn))
        rj = aklam / (real(j,rn) * bklam)
        Sj = Sj_minus1 + (Sj_minus1 - Sj_minus2)*rj*z
        if(use_derv1) then
            suma_inv = suma_inv + ONE/aklam
            sumb_inv = sumb_inv + ONE/bklam
            Sdja = Sdja_minus1 + (Sj_minus1 - Sj_minus2)*rj*z*suma_inv
            Sdjb = Sdjb_minus1 + (Sj_minus1 - Sj_minus2)*rj*z*sumb_inv*(-ONE)
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
