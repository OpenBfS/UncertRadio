
subroutine MCtables(kr,kqtyp)

   !  writes the condensed results of a MC simulation to the file MC_Tables.txt,
   !  separated into the parts for kqtyp = 1 (output quantity), = 2 (DT) and = 3 (DL).
   !  called by MCCalc

   !     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,eps1min,zero,one,two
use UR_Gleich,     only: ngrs,ncov,ivtl,knumEGr,Messwert,MesswertSV,StdUncSV,SymboleG,kEGr, &
                         missingval,nvar,kpoint,iptr_time,kbrutto,iptr_cnt
use UR_Linft,      only: numd,FitDecay,kfitp,konstant_r0,fpa,sd0zrateSV,k_rbl,parfixed, &
                         dmesszeit,afuncSV,covar,d0zrateSV
use UR_Gspk1Fit,   only: Gamspk1_Fit
use UR_MCC,        only: covmw12,sdmw12,mw12,covmw12,xmit2,xsdv,xsdvsgl,xmit2,xmit1PE,xsdvPE, &
                         xmitsgl,mwnetvgl,mwnetmit,umwnetvgl,xsdnet
use UR_DLIM,       only: GamDistAdd
use UR_MCSR,       only: mwt1,mwt2,ratioSD,ratioval,xemit1,xesdev1,xesdev2,std2,xesdev3,utu,utw, &
                         MEsswert_eg,xcovt,ssxEG
! use UR_kpp,        only: test_mg

implicit none

integer(4),intent(in)      :: kr       ! number of the MC Run
integer(4),intent(in)      :: kqtyp    ! 3 cases: 1: output quantity; 2: decicion threshold; 3: detection limit

integer(4)      :: i,j,k,kj,kk
real(rn)        :: dummy,dpi,dpj,help,trueval,gdaf

        if(kqtyp == 1) write(63,*) 'xmit1PE=',sngl(xmit1PE),' xsdvPE=',sngl(xsdvPE)
  gdaf = zero
  ! if(test_mg) gdaf = one

IF(kqtyp <= 2) THEN
  WRITE(63,*) 'after MC-Loop, kqtyp=',int(kqtyp,2),' :'
  write(63,*)
  write(63,'(a)') '                         true value   MC value     MC/true       true SD      MC SD        MC/trueSD'

  do kk=1,ngrs+ncov+numd

    k = kk
       ! correlation between Fitp1 and Fitp2:
       if(FitDecay .and. knumEGr > 1 .and. k == ngrs+1) MEsswert(k) = Messwert(k) / xsdvsgl(kfitp(1)) / &
                                                                                  xsdvsgl(kfitp(1)+1)
    IF(ivtl(k) /= 4 .and. ivtl(k) /= 7) THEN
      utw = ' '            ! for value
      utu = ' '            ! for uncertainty
      if(kqtyp == 1) then
        ratioVal = zero
        if(abs(MesswertSV(k)) > zero) then
          if(StdUncSV(k) > zero .and. abs(xmitsgl(k)/MesswertSV(k)-one) > 0.01) utw = '*'
          if(StdUncSV(k) > zero .and. abs(StdUncSV(k)-xsdvsgl(k))/StdUncSV(k) > 0.03) utu = '*'
          if(abs(MesswertSV(k)) > eps1min) ratioval = xmitsgl(k)/MesswertSV(k)
        end if
        ratioSD = zero
        if(abs(StdUncSV(k)) > eps1min) then
          if(abs(StdUncSV(k)-missingval)>eps1min)  ratioSD = xsdvsgl(k)/StdUncSV(k)
        end if
        trueval = MesswertSV(k)
        IF(.true. .and. ubound(iptr_cnt,dim=1) >= k) then
         if(iptr_cnt(k) > 0) THEN
          help = Messwert(iptr_cnt(k))           ! *Messwert(iptr_time(iv))
          if(iptr_time(k) > 0) then
            trueval = help / Messwert(iptr_time(k))   ! convert counts to count rate
          else
            trueval = help
          end if
            !write(63,*) ' k=',int(k,2),' trueval=',sngl(trueval),' MWSV=',sngl(MesswertSV(k)), &
            !               ' help=',sngl(help)
         end if
        END IF

        WRITE(63,'(i3,1x,a,T27,es11.4,2x,es11.4,2x,es11.4,1x,a1,1x,es11.4,2x,es11.4,2x,es11.4,1x,a1)') &
                 k,SymboleG(k)%s,real(MesswertSV(k),8), &
                 real(xmitsgl(k),8),real(ratioval,8),utw,real(StdUncSV(k),8),real(xsdvsgl(k),8),  &
                 real(ratioSD,8),utu
      end if
      if(kqtyp == 2) then
        if(k <= ngrs+ncov) then
          ratioval = zero
          ratioSD = zero
          if(abs(Messwert_eg(k)) > zero) then
            if(StdUncSV(k) > zero .and. abs(xmitsgl(k)/Messwert_eg(k)-one) > 0.01) utw = '*'
            if(abs(Messwert_eg(k)) > eps1min) ratioval = xmitsgl(k)/Messwert_eg(k)
          end if
          if(StdUncSV(k) > zero) then
            if(StdUncSV(k) > zero .and. abs(StdUncSV(k)-xsdvsgl(k))/StdUncSV(k) > 0.03) utu = '*'
            if(abs(StdUncSV(k)) > eps1min .and. abs(StdUncSV(k)-missingval)>eps1min)  &
                                                                     ratioSD = xsdvsgl(k)/StdUncSV(k)
          end if
          Std2 = StdUncSV(k)
             kj=0
             if(nvar > 0) kj = iptr_time(nvar)
             if(.true. .and. .not.FitDecay .and. .not. Gamspk1_Fit .and. nvar > 0 .and.  &
                                                k == kbrutto(kEGr) .and. kj > 0) then
               ratioSD =zero
               Std2 = zero
               if(abs(MesswertSV(iptr_time(nvar))) > zero) then
                 Std2 = sqrt(MesswertSV(kbrutto(kEGr)) / MesswertSV(iptr_time(nvar)) )
                 if(ivtl(iptr_time(nvar)) == 11) Std2 = MesswertSV(kbrutto(kEGr)) /sqrt(MesswertSV(iptr_cnt(nvar)))
                 ratioSD = xsdvsgl(k)/Std2
               end if
             end if
          WRITE(63,'(i3,1x,a,T27,es11.4,2x,es11.4,2x,es11.4,1x,a1,1x,es11.4,2x,es11.4,2x,es11.4,1x,a1)') &
                      k,SymboleG(k)%s,real(Messwert_eg(k),8),real(xmitsgl(k),8),  &
                      real(ratioval,8),utw,real(Std2,8),real(xsdvsgl(k),8),real(ratioSD,8),utu
        else
          ! gross count rates:
          dummy= zero
          ratioval = zero
          dummy = zero
          if(abs(Messwert_eg(k)) > 10000._rn*eps1min) then
            if(StdUncSV(k) > 10000._rn*eps1min .and. abs(xmitsgl(k)/Messwert_eg(k)-one) > 0.01_rn) utw = '*'
            ratioval = xmitsgl(k)/Messwert_eg(k)
            if(FitDecay)  dummy = sqrt(Messwert_eg(k)/dmesszeit(k-(ngrs+ncov)))
            if(Gamspk1_Fit) dummy = sqrt(Messwert_eg(k)/Messwert(kpoint(2)))
            ratioSD = xsdvsgl(k)/dummy
            if(StdUncSV(k) > zero .and. abs(dummy-xsdvsgl(k))/dummy > 0.03) utu = '*'
          end if
          WRITE(63,'(i3,1x,a,T27,es11.4,2x,es11.4,2x,es11.4,1x,a1,1x,es11.4,2x,es11.4,2x,es11.4,1x,a1)') &
                   k,SymboleG(k)%s,real(Messwert_eg(k),8),real(xmitsgl(k),8), &
                   real(ratioval,8),utw,real(dummy,8),real(xsdvsgl(k),8),real(ratioSD,8),utu
        end if
      end if
    else
      WRITE(63,*) int(k,2), SymboleG(k)%s,' analytical:',sngl(max(0.00001_rn, MesswertSV(k)+gdaf*GamDistAdd)), &
                  'MC-Mean:',sngl(xmitsgl(k)),'MC-Mean/analyt:',sngl(xmitsgl(k)/max(0.00001_rn, MesswertSV(k)+0.*GamdistAdd)), &
                  'MC-SD: ',sngl(xsdvsgl(k))
    end if
  end do

  if(kqtyp == 1 .and. FitDecay) then
    dummy = sd0zrateSV(1)**two
    if(k_rbl > 0) then
      if(StdUncSV(kpoint(k_rbl)) > zero) dummy = dummy + StdUncSV(kpoint(k_rbl))**two
    end if
       if(parfixed) dummy = dummy + afuncSV(1,3)*afuncSV(2,3)*( 0.025_rn**two + 0.05_rn**two )
    write(63,*) 'cov(Rn(1), Rn(2))=',sngl(covmw12),'   expected: ',sngl(dummy),'  parfixed=',parfixed
       dummy = mw12/dmesszeit(1) + d0zrateSV(1)/dmesszeit(1) + sd0zrateSV(1)**two
       if(k_rbl > 0) dummy = dummy + MesswertSV(kpoint(k_rbl))/dmesszeit(1) + StdUncSV(kpoint(k_rbl))**two
    write(63,*) ' mean(Mwnet(1)=',sngl(mw12),', its SD:',sngl(sdmw12),'  expected SD: ',sngl(sqrt(dummy))

  end if

    if(FitDecay .and. kqtyp == 2) then
      write(63,*) '  Net counting rates:'
      do k=1,numd
        utu = ' '
        if(umwnetvgl(k) > zero .and. abs(umwnetvgl(k)-xsdnet(k))/umwnetvgl(k) > 0.03) utu = '*'
        WRITE(63,'(i3,1x,a,T27,es11.4,2x,es11.4,2x,es11.4,1x,a1,1x,es11.4,2x,es11.4,2x,es11.4,1x,a1)') &
                   k,'              ',real(MWnetvgl(k),8),real(mwnetmit(k),8), &
                   real(mwnetmit(k)/Mwnetvgl(k),8),utw,real(umwnetvgl(k),8),real(xsdnet(k),8),  &
                   real(xsdnet(k)/umwnetvgl(k),8),utu
      end do
      dummy = sd0zrateSV(1)**two
      if(parfixed) dummy = dummy + afuncSV(1,3)*afuncSV(2,3)*( 0.025_rn**two + 0.05_rn**two )
      write(63,*) 'cov(Rn(1), Rn(2))=',sngl(covmw12),'   erwartet: ',sngl(dummy),'  parfixed=',parfixed
    end if
  WRITE(63,*) 'Resulta(kEGr) obtained with Messwert-means of the MC Loop:',sngl(xmit2)
  if(FitDecay) write(63,*) '   konstant_r0=',konstant_r0
  WRITE(63,*) 'Ucomb-MC(kEGr): ',sngl(xsdv)
  IF(FitDecay) THEN
    WRITE(63,*) ' xxxx Mean of the SDEV values from the single MC fits: ', sngl(xesdev1), &
                                                                      '  its StdDev:',sngl(xesdev2)
    WRITE(63,*) ' xxxx Mean of the fpa values from the single MC fits : ', sngl(xemit1),  &
                                                                      '  its StdDev:',sngl(xesdev3)
    if(kEGr == 1 .and. abs(fpa(1)*fpa(2)*fpa(3)) > eps1min) then
      xcovt = zero
      Mwt1 = MesswertSV(1)
      Mwt2 = MesswertSV(2)
      if(kqtyp == 2) Mwt1 = xmit2         ! xmitsgl(1)
      do i=1,3
        if(abs(fpa(i)) < eps1min) cycle
        if(i == 2) cycle
        dpi = Mwt1/fpa(i)
        if(i == 3) dpi = - dpi
        do j=1,3
          if(abs(fpa(j)) < eps1min) cycle
          if(j == 1) cycle
          dpj = Mwt2/fpa(j)
          if(j == 3) dpj = - dpj
          xcovt = xcovt + dpi*dpj * covar(i,j)
        end do
      end do
      IF(knumEGr > 1) Write(63,*) ' MC values for the covariance(Mw1, Mw2): ',sngl(ssxEG(1)),'  theoretical: ',sngl(xcovt)
    end if

  end if
end if

end subroutine MCtables

