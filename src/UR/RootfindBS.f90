

real(rn) function PrFunc(mode,xx)

     ! this function is used within the routine brentx, which performs the
     ! necessary iterations for the DL but also for DT values to find
     ! these values as a root of the defining equations.
     ! root.
     ! Prfunc calculates for each iteration step the quantiles or probabilities.
     ! It handles not only the case of ISO 11929 characteristic values
     ! (modes 1,2,3), but also of a Bayesian MCMC method (modes 6,9,10,11).
     !
     !     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,    only: rn,eps1min,zero,one
use Brandt,       only: mean,sd

use UR_DLIM,      only: kbeta,beta,Fconst,Flinear
use UR_Gleich,    only: Ucomb,kEGr,Messwert,klinf,kgspk1,ifehl,Messwert, &
                        knetto,use_bipoi,N_preset,ksumeval,apply_units,ngrs

use UWB,          only: upropa,Resulta,gevalf,func_Fconst,func_Flinear
use Rw2,          only: rnetval
use UR_Linft,     only: kfitp,FitDecay,SumEval_fit
use UR_Gspk1Fit,  only: Gamspk1_Fit
use UR_MCC,       only: kqtyp,arraymc,imctrue,xmit1,xxDT,xsdv,estUQ
use Num1,         only: Quick_sort_r ! Qsort3      ! Norm_BiPoi2, Qsort8
use UR_MCSR,      only: RD
use MCSr,         only: MCsingRun,quantile
use PLsubs,       only: MCdistrib,PlotSteps,quantileM

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
!

integer        :: kk,i,nn,klu,nf,k,mclen_x,jj,jjt,n0,jx
real(rn)       :: Prob,uProb,dgm(5),ureldgm(5)
real(rn)       :: dummy,dn,sumP,meanxx,actSV,nDT_v,Prob2, MesswertKP(ngrs)
real(rn),allocatable  :: arrsort(:)
logical        :: apply_SV
integer(4),allocatable  :: indx(:)

Prfunc = zero

Prob = 0._rn

apply_SV = apply_units
! MesswertKP(1:ngrs) = Messwert(1:ngrs)

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
    ! Messwert(1:ngrs) = MesswertKP(1:ngrs)
    ! write(30,*) 'PrFunc: directly after RnetVal: RD=',sngl(RD)
    if(klu > 0) MEsswert(klu) = RD
    call ModVar(3, RD)
    call upropa(kEGr)     ! calculate Ucomb

    Prob = xx - kbeta*Ucomb     ! test lower quantile =? DT
    ! write(30,*) 'PrFunc: after RnetVal,Modvar: act=',sngl(xx),' RD=',sngl(RD),' Prob=',sngl(prob),' Ucomb=',sngl(Ucomb)
    ! write(30,*) 'faliq=',sngl(faliq(1:2))
    ! Messwert(1:ngrs) = MesswertKP(1:ngrs)

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
    Prob = quantileM(beta,arrsort,imctrue)
    meanxx = mean(arrsort)

       if(.false.) then
         call quantile(beta,1,imctrue,arrsort,Prob2,jx,zero,zero)
         kk = 0
         jj = 0
         jjt = 0
         n0 = 0
         do i=1,size(Arrsort)
           if(Arrsort(i) > zero .and. Arrsort(i) <= xxDT(1)) then
             jjt = jjt + 1
           end if
           if(Arrsort(i) > zero .and. Arrsort(i) <= xx) then
             kk = kk + 1
           end if
           if(Arrsort(i) <= zero) n0 = n0 + 1
           if(Arrsort(i) > zero .and. Arrsort(i) <= meanxx) then
             jj = jj + 1
           else
             if(jj > imctrue/10) exit
           end if
         end do
         write(63,*) 'RD=',sngl(RD),' P(xx=RD)=',sngl(real(kk,rn)/real(imctrue,rn)),'  meanxx=',sngl(meanxx), &
                    ' P(meanxx)=',sngl(real(jj,rn)/real(imctrue,rn)),' Prob=',sngl(Prob), &
                    ' DT=',sngl(xxDT(1)),' P(DT)=',sngl(real(jjt,rn)/real(imctrue,rn)),' beta=',sngl(beta)
         write(63,*) 'begin: fraction of <= zero: ',sngl(real(n0,rn)/real(imctrue,rn)), &
                     ' minval(Arrsort)=',sngl(minval(arrsort,dim=1)),'  Probe2=',sngl(Prob2), &
                     ' jx=',jx
       end if
    deallocate(arrsort)

      ! write(63,*) 'PRF:    xx=',sngl(xx),'  RD=',sngl(RD),' Prob=',sngl(Prob),' meanxx=',  &
      !                        sngl(meanxx),' Rnetval(xx)=',sngl(Rnetval(xx))

  case (3)
    ! DT iteration, for MCsim;
    klu = 0
    if(FitDecay) klu = klinf
    IF(Gamspk1_Fit) klu = kgspk1
    IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
     if(klu == 0 .and. knetto(kEGR) > 0) klu = knetto(kEGr)
    RD = RnetVal(xx)
    if(klu > 0) MEsswert(klu) = RD
            ! write(63,*) 'klu=',int(klu,2),' RD=',sngl(RD),' xx=',sngl(xx)

    call MCsingRun()          ! <-- Modvar is only called in MCsingRun
    Prob = xmit1
         !write(63,'(2(a,es11.4),a,i0,3(a,es11.4))') 'Prfunc: xx=',xx,' RD=Rnetval=',RD,' klu=',klu, &
         !                        ' Prob=xmit1=',prob,' xsdv=',xsdv,' DTx=',estUQ

end select
PrFunc = Prob

end function PrFunc

!########################################################################



