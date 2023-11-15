real(rn) FUNCTION BCIFunc_MCMC(ps1)

    ! diese Routine wird nicht mehr verwendet

USE Rmcmc,       only: estLQ_BCImcmc,estUQ_BCImcmc,mklen,mklenL,   &
                       pdx_Act
USE UR_NWG1,     only: W1minusG
use UR_params,   only: rn

implicit none

real(rn),INTENT(IN)    :: ps1

integer(4)      :: i,mkL,mkU,mk
real(rn)        :: v1,PD,pgamvor,sums,ps2,v1a,Vfind
!-----------------------------------------------------------------------
   ! Bayesian confidence interval or Bayesian coverage interval
pgamvor = (1._rn - W1minusG)/2._rn
ps2 = 1._rn - pgamvor -(pgamvor - ps1)
! ps2 = 1._rn - ps1

    ! write(28,*) 'ps1=',sngl(ps1),'   ps2=',sngl(ps2)
estLQ_BCImcmc = -1._rn
estUQ_BCImcmc = -1._rn
mklen = mklenL
sums = 0._rn
do mk=0,mklen
  ! v1 = vkmin(1) + real(mk,rn)*vkstep(1) + vkstep(1)/2._rn
  ! PD = pdiff(1,mk)
  ! v1a = vkminAct + real(mk,rn)*vkstepAct + vkstepAct/2._rn
  v1a = Vfind(real(mk,rn),pdx_Act%vkmin,pdx_Act%vkmax,pdx_Act%vkstep,1)
  PD = pdx_Act%pdiff(mk)
  sums = sums + PD
  if(estLQ_BCImcmc < 0._rn .and. sums > ps1) then
    ! estimate lower quantile estLQ for ps1:
    ! estLQ_BCImcmc = v1 - vkstep(1) + (ps1 - (sums-PD))/PD*vkstep(1)
    estLQ_BCImcmc = v1a - pdx_Act%vkstep + (ps1 - (sums-PD))/PD*pdx_Act%vkstep
    mkL = mk-1
  endif
  if(estUQ_BCImcmc < 0._rn .and. sums > ps2) then
    ! estimate quantile estLQ for ps2:
    ! estUQ_BCImcmc = v1 - vkstep(1) + (ps2 -(sums-PD))/PD*vkstep(1)
    estUQ_BCImcmc = v1a - pdx_Act%vkstep + (ps2 -(sums-PD))/PD*pdx_Act%vkstep
    mkU = mk-1
  endif
enddo

BCIFunc_MCMC = estUQ_BCImcmc - estLQ_BCImcmc
  ! write(28,*) ' Diff_p=',sngl(estUQ_BCImcmc - estLQ_BCImcmc)

END function BCIFunc_MCMC

!######################################################################################
