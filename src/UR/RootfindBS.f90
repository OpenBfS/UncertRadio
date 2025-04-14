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

real(rn) function PrFunc(mode, xx)

    ! this function is used within the routine brentx, which performs the
    ! necessary iterations for the DL but also for DT values to find
    ! these values as a root of the defining equations.
    ! root.
    ! Prfunc calculates for each iteration step the quantiles or probabilities.
    ! It handles not only the case of ISO 11929 characteristic values
    ! (modes 1,2,3), but also of a Bayesian MCMC method (modes 6,9,10,11).
    !
    !     Copyright (C) 2014-2023  Günter Kanisch
    use UR_types
    use UR_params,    only: ZERO
    use Brandt,       only: mean, sd, pnorm

    use UR_DLIM,      only: kbeta, Fconst, Flinear
    use UR_Gleich_globals,    only: Ucomb, kEGr, Messwert, klinf, kgspk1, ifehl, Messwert, &
                            knetto, use_bipoi, ksumeval, apply_units

    use UWB,          only: upropa, gevalf
    use Rw2,          only: rnetval
    use UR_Linft,     only: kfitp,FitDecay,SumEval_fit
    use UR_Gspk1Fit,  only: Gamspk1_Fit
    use UR_MCC,       only: kqtyp, arraymc, imctrue, xmit1
    use Num1,         only: Quick_sort_r
    use UR_MCSR,      only: RD
    use MCSr,         only: MCsingRun, quantile
    use PLsubs,       only: quantileM

    implicit none

    integer(4),intent(in)  :: mode              ! see below
    real(rn),intent(in)    :: xx                ! an iterated activity value given by brentx

    !     DT: decision threshold (abbreviated often as ekg (in DE))
    !     DL: detetction limit  (abbreviated often as nwg (in DE))
    !  mode  :  1 : DL iteration, for Analyt;
    !           2 : DL iteration, for MCsim;
    !           3 : DT iteration, for MCsim;
    !
    !   RD denotes a value of an iterated net counting rate, calculated by the
    !   function RnetVal(activity.

    integer                 :: klu
    real(rn)                :: prob
    real(rn)                :: meanxx
    real(rn),allocatable    :: arrsort(:)
    logical                 :: apply_SV
    integer(4),allocatable  :: indx(:)

    Prfunc = ZERO
    Prob = ZERO

    apply_SV = apply_units

    select case (mode)

      case (1)
        ! DL iteration, for Analyt;
        ! RD = (xx - Fconst)/Flinear
        klu = 0
        klu = knetto(kEGr)
        if(FitDecay) klu = klinf
        IF(Gamspk1_Fit) klu = kgspk1
        IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
        if(SumEval_fit) klu = ksumeval

        RD = RnetVal(xx)    ! does not use gevalf/evalf!   darin versteckt sich ein root-finding


        if(klu > 0) MEsswert(klu) = RD
        call ModVar(3, RD)
        call upropa(kEGr)     ! calculate Ucomb

        Prob = xx - kbeta*Ucomb     ! test lower quantile =? DT

      case (2)
        ! DL iteration, for MCsim;
        klu = 0
        klu = klinf
        IF(Gamspk1_Fit) klu = kgspk1
        IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
        if(klu == 0 .and. knetto(kEGR) > 0) klu = knetto(kEGr)
        if(SumEval_fit) klu = ksumeval

        if(.not.use_bipoi .or. (use_bipoi)) then      !  .and. .not.test_mg)) then
            RD = RnetVal(xx)
            if(SumEval_fit) RD = (xx - Fconst)/Flinear
        end if
        ! if(use_bipoi .and. test_mg) RD = xx
        if(klu > 0) MEsswert(klu) = RD

        call MCsingRun()
        if(ifehl == 1) then
            write(63,*) 'MCcalc: Error in MCsingrun!  kqtyp=',int(kqtyp,2)
            return
        end if
        allocate(arrsort(1:imctrue))
        arrsort(1:imctrue) = arraymc(1:imctrue,kqtyp)
        ! call QSort8(imctrue,arrsort,ifehl)
        ! call QSort3(arrsort,ifehl)
        allocate(indx(1))
        call Quick_sort_r(arrsort,indx)
        Prob = quantileM(1.0_rn - pnorm(kbeta), arrsort,imctrue)
        meanxx = mean(arrsort)

        deallocate(arrsort)

      case (3)
        ! DT iteration, for MCsim;
        klu = 0
        if(FitDecay) klu = klinf
        IF(Gamspk1_Fit) klu = kgspk1
        IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
        if(klu == 0 .and. knetto(kEGR) > 0) klu = knetto(kEGr)
        RD = RnetVal(xx)
        if(klu > 0) MEsswert(klu) = RD

        call MCsingRun()          ! <-- Modvar is only called in MCsingRun
        Prob = xmit1


    end select
    PrFunc = Prob

end function PrFunc
