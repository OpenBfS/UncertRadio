
module MCSr



  contains


subroutine MCsingRun()

  ! performs a single Monte Carlo simulation run, called from the controlling routine MCCalc.

  !     Copyright (C) 2014-2023  Günter Kanisch

use, intrinsic :: iso_c_binding,          only: c_ptr, c_null_char, c_int,c_double
use gtk,                    only: gtk_buttons_ok,GTK_MESSAGE_WARNING,gtk_progress_bar_set_fraction, &
                                  gtk_widget_hide,gtk_widget_set_sensitive

USE UR_Variables,           only: langg,Gum_restricted
USE UR_Gleich,              only: MEsswert,ifehl,kEGr,kfitcal,klinf,knumEGr,missingval, &
                                  nab,ncov,ngrs,kpoint,nvar,ivtl,MesswertSV,HBreite,IAR,kbrutto,StdUnc, &
                                  iptr_time,iptr_cnt,covarval,cvformel,StdUncSV,isymbA,isymbB,covarvalSV,Symbole, &
                                  icovtyp,symboleG,nmumx,kbgv_binom,knetto,iptr_rate,  &
                                  ip_binom,itm_binom,DistPars,ksumeval,k_datvar,MDpointrev, &
                                  fbayMD,MDpoint,k_MDtyp,use_bipoi,Nbin0_MV,bipoi2_sumv, &
                                  nvalsMD

USE UR_Linft,               only: fpa,ifit,FitDecay,klincall,konstant_r0, &
                                  kPMLE,mfrbg,nchannels,netto_involved_Fitcal,nkovzr, &
                                  numd,parfixed,singlenuk,WTLS_wild,d0zrate,r0k,d0zrateSV,afuncSV, &
                                  k_rbl,fixedrateMC,sd0zrate,fpaSV,kfitp,d0messzeit,SDfixedrate, &
                                  dmesszeit,a,sfpa,covar,fpakq,SumEval_fit
USE UR_Gspk1Fit,            only: Gamspk1_Fit,GNetRateSV,varadd_Rn,GNetRate,SDGNetRate, &
                                  effi,sdeffi,pgamm,sdpgamm,fatt,sdfatt,fcoinsu,sdfcoinsu
USE UR_DLIM,                only: alpha,beta,GamDistAdd,nit_detl_max,W1minusG,RblTot
USE UR_MCC
! USE, INTRINSIC        :: IEEE_ARITHMETIC

USE fparser,                ONLY: evalf
use Rout,                   only: WDPutEntryInt,MessageShow,pending_events,WDPutEntryInt

use Top,                    only: WrStatusbar
use top,                    only: idpt
use Rw1,                    only: covppcalc
use UWB,                    only: Resulta,RbtCalc
use Usub3,                  only: FindMessk
use Num1,                   only: funcs,SearchBCI3,quick_sort_r       ! QSort8,
use KLF,                    only: fkalib,sd_y0,CalibInter
use RND,                    only: Rndu,rnorm,rgamma,Random_bipo2,random_beta,random_t, &
                                  ran_Erlang,scan_bipoi2
use PLsubs
use LF1,                    only: Linf,linfout
use Brandt,                 only: mean,sd,mtxchl,MatRand
use UR_params,              only: rn,eps1min,one,zero,two,three
use RdSubs,                 only: rmcformF
use UR_MCSR
use CHF,                    only: FindlocT,isNaN
use UR_MCC,                 only: test_mg
use fgsl,                   only: fgsl_rng,fgsl_rng_type,fgsl_double,fgsl_ran_poisson, &
                                  fgsl_rng_env_setup,fgsl_rng_default,fgsl_rng_alloc, &
                                  fgsl_ran_tdist

implicit none

integer(c_int)       :: resp
integer(4)           :: iv,i,k,j,icd1,icdmax,ii1,ii2,nvtb,iij,icnt,nn,ks,kunit,mnj,jj
integer(2)           :: mms_arr(nmumx),vfixed(200)
character(len=60)    :: cminus
! real(rn)             :: QuantileM
real(rn)             :: xN0m,Nbin0,qxN0m,valanf(100),xrnet,xtm,xt0
real(rn)             :: dumx,dumxq,dumy,dumyq,dums,dumsq,dum3,dum3q,dumx0,dumx0q
real(rn)             :: dumt0,dumt0q,dumn0,dumn0q,dumRnet,dumRnetq,dumres,dumresq
real(rn)             :: zalpha,zbeta,gamvarmin,gamvarmax,gamvarmean,gsum,fBay
real(rn)             :: aa,bb,vvar,t_ndf,t_mue,t_sig,ttmean(4),ttvar(4),dum37,dum37q,trand,mvals
real(rn)             :: mratio,gdev,divm
real(4)              :: stt1,stp1,stt3,stp3
logical              :: bgross
real(c_double)       :: fracc
real(rn),allocatable :: Rmat(:,:),RmatF(:,:),b(:,:),z(:,:),helpz(:)
character(:),allocatable  :: str1
integer(4),allocatable   :: indx(:)
type(fgsl_rng)       :: r5
type(fgsl_rng_type)  :: t5

!----------------------------------------------------------------------------------------------
call gtk_progress_bar_set_fraction(idpt('TRprogressbar'), 0.d0)
call gtk_widget_set_sensitive(idpt('TRprogressbar'), 1_c_int)

allocate(character(len=150)  :: str1)


  t5 = fgsl_rng_env_setup()
  t5 = fgsl_rng_default
  r5 = fgsl_rng_alloc (t5)

! MCSim_on = .false.

if(.true.) then
  ! restore arrays from *ORG arrays
  Messwert(1:ngrs+ncov+numd) = MesswertORG(1:ngrs+ncov+numd)
  StdUnc(1:ngrs+ncov+numd) = StdUncORG(1:ngrs+ncov+numd)
  MesswertSV(1:ngrs+ncov+numd) = MesswertORG(1:ngrs+ncov+numd)
  StdUncSV(1:ngrs+ncov+numd) = StdUncORG(1:ngrs+ncov+numd)
end if

if(allocated(netfit)) deallocate(netfit);
allocate(netfit(numd)); netfit = zero
if(allocated(fixedRateMC)) deallocate(fixedRateMC);
allocate(fixedRateMC(numd)); fixedRateMC = zero
if(allocated(rnetvar)) deallocate(rnetvar);
allocate(rnetvar(numd)); rnetvar = zero

if(allocated(icovn)) then
  icdmax = 2
  icdmax = maxval(icovn,dim=1)
  if(allocated(Rmat)) Deallocate(Rmat)
  allocate(Rmat(1:icdmax,1:icdmax))
  if(allocated(Rmatf)) Deallocate(Rmatf)
  allocate(Rmatf(1:numd,1:numd))
end if

  if(allocated(ivref)) deallocate(ivref)
  allocate(ivref(ngrs))
  mms_arr = 0
kunit = 63

mmkk = mmkk + 1     ! Iteration counter

IF(kqtyp == 2 .and. mmkk >= 0) THEN
  ! the value of RD (modified net count rate) is set outside of MCsingRUN,
  ! e.g., in RootFindBS/PrFunc
  call WDPutEntryInt('TRentryMCit', mmkk)
  call ModVar(kqtyp, RD)

  if(kbrutto(kEGr) > 0) then
    MesswertSV(kbrutto(kEGr)) = Messwert(kbrutto(kEGr))
    StdUncSV(kbrutto(kEGr)) = StdUnc(kbrutto(kEGr))
  end if

  IF(FitDecay .or. Gamspk1_Fit) THEN
    fpa(kEgr) = RD
  end if
  IF(FitDecay ) THEN
    ! make sure that the q matrices are re-calculated in Lincov2:
    klincall = 0
    call Linf(r0dummy,sdr0dummy)
    IF(ifehl == 1) then
      goto 9000
    end if
  end if
end if

IF(kqtyp == 3) THEN
  IF( ((FitDecay .or. Gamspk1_Fit) .and. mmkk > 8*nit_detl_max/12) .or. (.not.FitDecay .and. mmkk > nit_detl_max) ) THEN
    write(63,*) 'MCsingrun:  rescue exit from DL iteration'
    GOTO 9000
  end if
  call WDPutEntryInt('TRentryMCit', mmkk)
  call ModVar(kqtyp, RD)
  IF(FitDecay .or. Gamspk1_Fit) THEN
    fpa(kEgr) = RD
  end if

  IF(FitDecay ) THEN
    ! make sure (klincall = 0) that the q matrices are re-calculated in Lincov2:
    klincall = 0
    call Linf(r0dummy,sdr0dummy)
    IF(ifehl == 1) then
      write(63,*) 'MCsingrun:  Error in Linf!'
      goto 9000
    end if
  end if

end if

mw_rbl = zero
if(k_rbl > 0) mw_rbl = MesswertSV(kpoint(k_rbl))

if(kqtyp >= 1) then
 if(FitDecay) then
  do i=1,ngrs+ncov+numd
    MEsswertkq(i) = Messwert(i)
    StdUnckq(i) = StdUnc(i)
    if(i > ngrs+ncov .and. i <= ngrs+ncov+numd .and. FitDecay) then
      ik = i - (ngrs + ncov)
      messk = FindMEssk(ik)
      IF(konstant_r0) THEN
        d0zrate(ik) = R0k(messk)
      else
        d0zrate(ik) = d0zrateSV(ik)
      end if
      netfit(ik) = zero

      do j=1,ma
        if(ifit(j) == 3) cycle
        if( j /= kEGr) then
          fparm = fpa(j)
          if(kqtyp >= 2 .and. fpa(j) < zero) fparm = zero
          if(ifit(j) == 1) netfit(ik) = netfit(ik) + fparm * afuncSV(ik,j)
          if(ifit(j) == 2) then
            if(kPMLE /= 1) then
              netfit(ik) = netfit(ik) + one * afuncSV(ik,j)
            else
              netfit(ik) = netfit(ik) + fparm * afuncSV(ik,j)
            end if
          end if
        else
          netfit(ik) = netfit(ik) + RD * afuncSV(ik,j)
        end if
        fixedrateMC(ik) = zero
        if(ifit(j) == 2) fixedrateMC(ik) = afuncSV(ik,j)
      end do
      MEsswertkq(i) = netfit(ik) + d0zrate(ik) + rblindnet
      if(kpmle /= 1) then
        mwnetvgl(ik) = netfit(ik)
      else
        mwnetvgl(ik) = netfit(ik) - d0zrate(ik) - rblindnet
      end if
      if(parfixed) mwnetvgl(ik) = mwnetvgl(ik) - fixedrateMC(ik)
      umwnetvgl(ik) = sqrt(MEsswertkq(i)/dmesszeit(ik) + sd0zrate(ik)**two)
    end if
  end do
 end if
end if

IF(Gamspk1_Fit) THEN
  if(allocated(rnetvar)) deallocate(rnetvar)
  if(.not.allocated(rnetvar)) allocate(rnetvar(numd/5))
  rnetvar = zero
  IF(kqtyp == 1) rnetvar(1:numd/5) = GNetRateSV(1:numd/5)
  IF(kqtyp > 1) rnetvar(1:numd/5) = Messwert(ngrs+ncov+([(i,i=1,numd/5)]-1)*5+1)
end if

xmit1 = zero      ! MC mean of the output quantity
xmitq = zero      ! Sum of squares of the MC values of the output quantity
xsdv  = zero      ! MC standard deviation of the output quantity
xmitsgl = zero    ! MC means of the individual quantities
xmitsglq = zero   ! sum of squares of the MC values of the individual quantities
xsdvsgl = zero
xmit1PE = zero
xmit1qPE = zero
xsdvPE = zero
mwnetmit = zero
mwnetmitq = zero
covmw12 = zero
mw12 = zero
mw12q = zero
sdmw12 = zero

estUQ = zero
estLQ = zero
IF(kqtyp == 3 .or. kqtyp == 2) THEN
  mcasum(kqtyp) = 0
  mcafull(kqtyp,1:mcmax) = 0
end if
imctrue = 0
imcPE = 0
minres = 1.E+15_rn
maxres = -1.E+15_rn
kausum = 0
ntwild = 0
nminus = 0
nplus = 0
arraymc(1:mc2max,kqtyp) = zero

imc2 = 0
qxN0m = zero

dumx = zero
dumxq = zero
dumy = zero
dumyq = zero
dums = zero
dumsq = zero
dum3 = zero
dum3q = zero
dum37 = zero
dum37q = zero
dumx0 = zero
dumx0q = zero
dumt0 = zero
dumt0q = zero
dumn0 = zero
dumn0q = zero
dumRnet = zero
dumRnetq = zero
dumres = zero
dumresq = zero
gamvarmin = 1.E+30_rn
gamvarmax = -1.E+30_rn
gamvarmean = zero
gsum = zero
ttmean = zero
ttvar = zero
bipoi2_sumv = zero
if(FitDecay .and. numd > 1) allocate(helpz(numd))       ! 16.7.2023

         ! write(0,*) 'before imc_loop'
! Start the simulation loop: ------------------------------------------------
do imc=1,imcmax

  IF(kqtyp == 1 .AND. kr == 1 .AND. imc == 1) CALL CPU_TIME(start)
  IF(kqtyp == 1 .AND. kr == 1 .AND. imc == imc10) THEN
    CALL CPU_TIME(finish)
    IF(finish-start > 7) wait = .TRUE.         ! in seconds
  end if

   !IF(kqtyp == 2 .AND. kr == 1 .AND. imc == 1000*(imc/1000)) write(0,*) 'imc=',imc

  IF(imc10*(imc/imc10) == imc) THEN
    fracc = real(imc/imc10,8)/15.0_c_double
    call gtk_progress_bar_set_fraction(idpt('TRprogressbar'), fracc)
    call pending_events
  END IF

110     continue

  IF(FitDecay) THEN
    fpa(1:3) = xfpa(1:3)
    IF(kqtyp > 1) THEN
      fpa(kEgr) = RD
    end if
    fpakq(1:3) = fpa(1:3)
  end if
  IF(Gamspk1_Fit) THEN
    fpa(1) = xfpa(1)
    fpaSV(1) = xfpa(1)
    IF(kqtyp > 1) THEN
      fpa(kEgr) = RD
      fpaSV(kEgr) = RD
    end if
  end if

  IF(Gamspk1_Fit) THEN
    do i=1,numd/5
      i11 = (i-1)*5
      kix = ngrs+ ncov + i11

      SDGNetRate(i) = SQRT( rnetvar(i)/Messwert(kpoint(2)) + varadd_Rn(i) )
      GNetRate(i) = rnetvar(i)+ SDGNetRate(i)*rnorm()

      Effi(i) = EffiSV(i) + SDEffi(i)*rnorm()
      pgamm(i) = pgammSV(i) + SDpgamm(i)*rnorm()
      fatt(i) = fattSV(i) + SDfatt(i)*rnorm()
      fcoinsu(i) = fcoinsuSV(i) + SDfcoinsu(i)*rnorm()

      Messwert(kix+1) = GNetRate(i)
      StdUnc(kix+1)   = SDGNetRate(i)
      Messwert(kix+2) = effi(i)
      Messwert(kix+3) = pgamm(i)
      Messwert(kix+4) = fatt(i)
      Messwert(kix+5) = fcoinsu(i)
    end do
  end if

120     continue

  if(imc == 1) then
    c_mars = zero    !  for Marsaglia rnadom number generator
    d_mars = zero    !

    if(Findloc(IVTL,8,dim=1) > 0 .or. FindLoc(IVTL,9,dim=1) > 0) then
      ! prepare beta random generator
      d_rb = zero
      f_rb = zero
      h_rb = zero
      t_rb = zero
      c_rb = zero
      swap_rb = .false.
      nvtb = 0
    end if
    s_rt = zero  ! for random_t
    c_rt = zero
    a_rt = zero
    f_rt = zero
    g_rt = zero

    ! Count those numbers of counts, for which the (N+x)-rule is used (ivtl array):
    mwref = zero
    ivref = 0
    nvt = 0
    do iv=nab+1,ngrs
      if(ivtl(iv) == 4 .or. ivtl(iv) == 11) then
        nvt = nvt + 1
        ivref(iv) = nvt
        if(iptr_rate(iv) > 0) then
          icnt = iptr_cnt(iptr_rate(iv))  !  icnt: index of the count number within the "Messwert" array
          if(icnt == 0 .and. ivtl(iv) == 4) icnt = iv
          if(icnt > 0) then
            nvt = nvt + 1
            ivref(iv) = nvt
            nvt = nvt + 1
            ivref(icnt) = nvt
              ! write(63,*) 'success! nvt=',int(nvt,2),' iv=',int(iv,2),' icnt=',int(icnt,2)
          end if
        end if
      end if
      if(ivtl(iv) == 6) then
        nvt = nvt + 1
        ivref(iv) = nvt
      end if
      if(ivtl(iv) == 7) then
        nvt = nvt + 1
        ivref(iv) = nvt
      end if
      if(ivtl(iv) == 8 .or. ivtl(iv) == 9 .or. ivtl(iv) == 10) then
        nvtb = nvtb + 1
        ivref(iv) = nvtb
        if(ivtl(iv) == 9) then
          t_ndf = DistPars%pval(MDpointrev(iv),1)
          t_mue = DistPars%pval(MDpointrev(iv),2)
          t_sig = DistPars%pval(MDpointrev(iv),3)
        end if
      end if
    end do
  end if

  if(.false. .and. kqtyp > 1 .and. imc <= 5) then
    do i=1,ngrs
      write(63,*) int(i,2),' ',symbole(i)%s,'  MW=',sngl(Messwert(i)),' MWSV=',sngl(Messwert(i)), &
                            'U=',sngl(StdUnc(i)),'  USV=',sngl(StdUncSV(i))
    end do
  end if

  vfixed = 0

  do iv=ngrs,kEGr+1,-1
    ! (backward) loop over the input quantities for sampling their values:
    ! Find exclsuion conditions (goto 25)

    IF(iv > kEGr .AND. iv <= knumEGr) goto 25       ! CYCLE
    if(Kfitcal == 0) then
      IF(kqtyp == 1 .and. iv <= nab) goto 25     ! CYCLE
      IF(kqtyp > 1 .AND. iv <= nab .AND. iv /= kbrutto(kEGr) .AND. kbrutto(kEGr) > 0) goto 25
    else
      if(kqtyp == 1 .and. iv <= nab) goto 25     ! CYCLE
      IF(kqtyp > 1 .AND. iv <= nab .AND. iv /= kbrutto(kEGr) .AND. kbrutto(kEGr) > 0 ) goto 25
    end if
    IF(iv > nab .AND. kfitp(1) > 0) THEN
      IF(knumEGr > 1) THEN
        IF(iv >= kfitp(1) .AND. iv <= kfitp(1)+2) CYCLE
      else
        IF(iv == kfitp(1)) CYCLE
      end if
    end if
    IF(FitDecay) THEN
      IF(k_rbl > 0) then
        if(iv == kpoint(k_rbl)) CYCLE
      end if
    end if

    IF(iv <= nab .AND. klinf > 0 .AND. iv == klinf) CYCLE
    IF(iv <= nab .AND. KFitcal > 0 .AND. iv == KFitcal) CYCLE
    if(KFitcal > 0 .and. netto_involved_Fitcal .and. iv == kbrutto(kEGr)) cycle      ! see comment in the routine CalibInter!
    if(SumEval_fit .and. iv == ksumeval .and. ksumEval <= nab) cycle

    if(vfixed(iv) == 1) cycle

       ! if(imc <= 2) write(63,*) 'iv=',int(iv,2),' ivref(iv)=',int(ivref(iv),2)
    select case (ivtl(iv))        ! select distribution type of the variable number iv
      case (1)      ! normal distribution
        IF(abs(StdUnc(iv)-missingval) < eps1min .OR. abs(StdUnc(iv)-zero) < eps1min) then
          MEsswert(iv) = MesswertSV(iv)
        else
          Messwert(iv) = MesswertSV(iv) + StdUncSV(iv)*rnorm()
        end if

      case (2)      ! rectangular distribution
        IF(abs(HBreite(iv)-missingval) < eps1min) then
          MEsswert(iv) = MesswertSV(iv)
        else
          HBrt = Hbreite(iv)              ! HBreite = half-width
          IF(IAR(iv) == 2) HBrt = HBrt*MesswertSV(iv)  ! für relative Werte
          Messwert(iv) = MesswertSV(iv) + two*HBrt*(Rndu()-0.5_rn)
              !if(imc < 11) write(63,*) 'RT: iv=',int(iv,2),' HBrt=',sngl(HBrt),' MesswertSV(iv)=',MesswertSV(iv), &
              !                             ' Mw(iv)=',Messwert(iv)
        end if
      case (3)      ! triangular distribution
        IF(abs(HBreite(iv)-missingval) < eps1min) then
          MEsswert(iv) = MesswertSV(iv)
        else
          HBrt = Hbreite(iv)               ! HBreite = half-width
          IF(IAR(iv) == 2) HBrt = HBrt*MesswertSV(iv)  ! for relative values
          r1 = Rndu()
          IF(r1 <= 0.5_rn) THEN
            Messwert(iv) = MesswertSV(iv) + HBrt*(-one + SQRT(two*r1))
          else
            Messwert(iv) = MesswertSV(iv) + HBrt*(one - SQRT(two*(one-r1)))
          END IF
        end if

      case (4)      ! gamma distribution, for (N+1)-rule  of a number of counts
            ! note: the value GamDistAdd is added within Ran_Gamma8 or rgamma
        gda_SV = GamDistAdd
        IF(iv /= kbrutto(kEGr)) THEN
          IF(imc == 1) then
            valanf(iv) = MesswertSV(iv)            !!!!! + one*GamDistAdd : don't add here
            if(abs(MesswertSV(iv)) < eps1min .and. abs(GamDistAdd) < eps1min) valanf(iv) = one
            rnnd = rgamma(ivref(iv),valanf(iv),.true.)
          end if
          help = MesswertSV(iv)
          if(abs(help) < eps1min .and. abs(GamDistAdd) < eps1min) then
            ! This is the "new" (N+x)-rule of ISO 11929-1(2019), applied since about April 2018
            GamDistAdd = one
          end if
          Messwert(iv) = rgamma(ivref(iv),valanf(iv),.false.)

            !if(imc <= 50000 .and. iv == 8) dumx = dumx + Messwert(iv)/50000._rn
            !if(imc <= 50000 .and. iv == 8) dumxq = dumxq + Messwert(iv)**two/50000._rn
            !if(imc == 1 .and. iv == 8) write(63,*) 'imc=1, iv=',int(iv,2),' : valanf=',sngl(valanf(iv))

            ! if(imc < 20) write(63,*) Symbole(iv)%s,' MWSV(iv)=',sngl(MesswertSV(iv)),' iv=',int(iv,2), &
            !             'ivref(iv)=',int(ivref(iv),2),' MW(iv)=gamma=',sngl(Messwert(iv)),&
            !             ' valanf(iv)=',sngl(valanf(iv)),' GDA=',sngl(GamDistAdd), &
            !             ' dmars(ivref(iv))=',sngl(dmars(ivref(iv)))

            ! if(imc <= 50000 .and. iv == 12) dumx = dumx + Messwert(iv)/50000._rn
            ! if(imc <= 50000 .and. iv == 12) dumxq = dumxq + Messwert(iv)**two/50000._rn
          GamDistAdd = gda_SV
               !if(imc <= 20 .and. kqtyp == 2) write(67,*) 'MCC: GammaPDF:  iv=',iv,'  MesswertSV(iv)=',sngl(MesswertSV(iv)),  &
               !                          '  Mw(iv)=',sngl(Messwert(iv)),'  ivref=',ivref(iv)
               !if(imc >= imcmax-1000) mwref(ivref(iv)) = mwref(ivref(iv)) + Messwert(iv)
               !if(imc == imcmax) write(66,*) 'iv=',iv,'  mean(mw)=',sngl(mwref(ivref(iv))/1000._rn)
               !if(imc == imcmax) write(66,*) 'imcmax=',imcmax,'   imctrue=',imctrue
        end if
        !##############################
        gda_SV = GamDistAdd
        IF(iv == kbrutto(kEGr)) THEN
          ! iptr_time(iv): points to the counting time variable associated with the variable # iv (counts or  a count rate)
               if(imc == 1) write(63,*) '== kbrutto:  iv=',iv,' MwSV(iv)=',sngl(MesswertSV(iv))
          if(iptr_time(iv) > 0) then
            help = MesswertSV(iv)*Messwert(iptr_time(iv))
          else
            help = MesswertSV(iv)*one
          end if
          IF(imc == 1) rnnd = rgamma(ivref(iv),help,.true.)
          if(abs(help) < eps1min .and. abs(GamDistAdd) < eps1min) then
            ! This is the "new" (N+x)-rule of ISO 11929-1(2019), applied since about April 2018
            GamDistAdd = one
          end if
          Messwert(iv) = rgamma(ivref(iv), help,.FALSE.)
          if(iptr_time(iv) > 0) then
            Messwert(iv) = Messwert(iv)/Messwert(iptr_time(iv))   ! convert the counts to a count rate value
          else
            !
          end if
           ! if(imc <= 50000) dumx = dumx + Messwert(iv)/50000._rn
           ! if(imc <= 50000) dumxq = dumxq + Messwert(iv)**two/50000._rn
        end if
        GamDistAdd = gda_SV

      case (5)            ! lognormal distribution
        IF(abs(StdUnc(iv)-missingval) < eps1min .OR. abs(StdUnc(iv)-zero) < eps1min) then
          MEsswert(iv) = MesswertSV(iv)
        else
          Messwert(iv) = EXP(mueLN + sigmaLN*rnorm() )
        end if

      case (6)            ! Gamma distribution
        gda_SV = GamDistAdd
        GamDistAdd = zero
        if(StdUncSV(iv) > zero) then
          zalpha = MesswertSV(iv)**two/StdUncSV(iv)**two
          zbeta  = MesswertSV(iv)/StdUncSV(iv)**two
          if(imc == 1) rnnd = rgamma(ivref(iv),zalpha,.true.) / zbeta
          Messwert(iv) = rgamma(ivref(iv),zalpha,.false.) / zbeta

             dummy = one / Messwert(iv)
          if(dummy < gamvarmin) gamvarmin = dummy
          if(dummy > gamvarmax) gamvarmax = dummy
          gamvarmean = gamvarmean + dummy
          gsum = gsum + one
        end if
        GamDistAdd = gda_SV

      case (7)           ! number of gross counts for the Binomial+Poisson distribution case

        if(imc == 1) then
          xtm = MesswertSV(itm_binom)
          xt0 = MesswertSV(iptr_time(kbgv_binom))
          mnj = min(1000000,imcmax)

          if(kqtyp <= 3) then
            xrnet = (MesswertSV(knetto(kEGr))*xtm - GamDistAdd) / xtm
            Nbin0 = ( MesswertSV(iv) - (MesswertSV(kbgv_binom)*xt0 - GamDistAdd) / xt0*xtm ) &
                                                          / MesswertSV(ip_binom)
            if(test_mg .and. kqtyp > 1) Nbin0 = Nbin0_MV
          else
            Nbin0 = zero
          end if
          if(kqtyp == 2) Nbin0 = zero
          if(kqtyp /= 2) then
            call scan_bipoi2(MesswertSV(ip_binom),Nbin0,RblTot(kEGr),MesswertSV(itm_binom))
          end if
        end if

        if(kqtyp /= 2 .and. use_bipoi) then
          Messwert(iv) = random_bipo2(Messwert(ip_binom),Nbin0,rbltotSV(kEGr),xtm)
             if(imc <= mnj) dumy = dumy + Messwert(iv)/real(mnj,rn)
             if(imc <= mnj) dumyq = dumyq + Messwert(iv)**two/real(mnj,rn)
          goto 771
        end if

        ! background counts number xN0m during the gross measurement counting time:
        IF(imc == 1) then
          valanf(iv) = MesswertSV(kbgv_binom) * xtm - GamDistAdd
          ! xN0m is treated as a gamma-deviate with GamDistAdd=0!
          ! initiate random generators:
          gda_SV = GamDistAdd
          if(.not.test_mg) GamDistAdd = zero
          rnnd = rgamma(ivref(iv),valanf(iv),.true.)
          GamDistAdd = gda_SV
        end if

        gda_SV = GamDistAdd
        if(.not.test_mg) GamDistAdd = zero
        xN0m = rgamma(ivref(iv),valanf(iv),.false.)
        GamDistAdd = gda_SV
        ! xn0m is now  R0*tm
        Messwert(iv) = xN0m
        if(kqtyp == 2) then
          if(imc <= mnj) dumy = dumy + Messwert(iv)/real(mnj,rn)
          if(imc <= mnj) dumyq = dumyq + Messwert(iv)**two/real(mnj,rn)
        end if
        goto 771

771     continue

      case (8,10)           ! 2- or 4-parameter beta distribution
        vvar = StdUncSV(iv)**two
        aa = MesswertSV(iv)**two * ((one-MesswertSV(iv))/vvar - one/MesswertSV(iv))
        bb = aa * (one/MesswertSV(iv) - one)
        if(imc == 1) rnnd = random_beta(ivref(iv),aa, bb, .true.)
        Messwert(iv) = random_beta(ivref(iv),aa, bb, .true.)
        if(ifehl == 1) goto 9000
        if(ivtl(iv) == 10) Messwert(iv) = DistPars%pval(9,3) + Messwert(iv)*(DistPars%pval(9,4)-DistPars%pval(9,3))

      case (9)              ! t distribution
        iij = ivref(iv)
        k_datvar = MDpointrev(iv)
        ks = MDPoint(k_datvar)
        nn = findlocT(Distpars%symb, Symbole(ks)%s)
        fBay = fBayMD(k_datvar)

        mvals = nvalsMD(k_datvar)  ! number of values
        mratio = (mvals-one)/(mvals - three)
          t_ndf = DistPars%pval(nn,1)       ! d.o.f.          ! added 14.8.2023

        if(imc == 1) trand = random_t(iij,int(t_ndf+0.499_rn,4),.true.)      ! init random_t
        trand = random_t(iij,int(t_ndf+0.499_rn,4),.false.)   ! standard t distributed

       ! trand = real(fgsl_ran_tdist(r5, real(t_ndf,fgsl_double)), rn)

          t_ndf = DistPars%pval(nn,1)       ! d.o.f.
          t_mue = DistPars%pval(nn,2)       ! mean
          t_sig = DistPars%pval(nn,3) /sqrt(mvals)      ! is s0    ! /sqrt(m)
          if(k_MDtyp(k_datvar) == 3) t_sig = DistPars%pval(nn,3)     ! 18.8.2023 : divide not by sqrt(mvals)

          if(.false. .and. imc <= 2) then
            write(63,'(5(a,i0),a,f8.4,a,f8.1,a,f8.4)') 'iv=',iv,' iij=',iij,' k_datvar=',k_datvar,' ks=',ks,' nn=',nn, &
                         ' fBay=',fbay,' mvals=',mvals,'t_sig=',t_sig
          end if
        divm = one
            ! k_MDtyp:   1: (n-1)/(n-3)/n; not counts (Bayes)
            !            2: 1/n;           counts, with influence (Bayes)
            !            3: 1              classical

        bgross = .false.
        if(iv == kbrutto(kEGr) .or. iptr_rate(iv) == kbrutto(kEGr)) bgross = .true.
        if(bgross) then
          ! gross quantity, or kqtyp > 1:
          if(iv == kbrutto(kEGr)) then
            t_mue = MesswertSV(iv)
            t_sig = StdUncSV(iv)                    ! * sqrt( (t_ndf-two)/t_ndf)  ! /sqrt(t_ndf+one)
          end if
          if(iptr_rate(iv) == kbrutto(kEGr)) then
            t_mue = MesswertSV(iv)
            t_sig = StdUncSV(iv)
          end if
          ! back-extract s^2/m from the full uncertainty StdUncSV:
          if(k_MDtyp(k_datvar) == 1) then
            divm = sqrt(mratio)
            t_sig = sqrt( t_sig**two)
          else if(k_MDtyp(k_datvar) == 2) then
            divm = sqrt(mratio)
            t_sig = sqrt( ( (t_sig**two*mvals - t_mue)/mratio) - t_mue ) /sqrt(mvals)
          else if(k_MDtyp(k_datvar) == 3) then
             ! t_sig need not to be modified
          end if
        end if
        ! using the t random value which includes the factor sqrt(mratio)!
        if(.true. .or. abs(fBay - one) > eps1min) then
          if(k_MDtyp(k_datvar) == 1) then
            Messwert(iv) = t_mue + trand / divm * t_sig
          else if(k_MDtyp(k_datvar) == 2) then
            gdev = rnorm()
            Messwert(iv) = t_mue + trand / divm *sqrt( t_sig**two + t_mue/mvals ) + &
                           gdev*sqrt(t_mue/mvals)
          else if(k_MDtyp(k_datvar) == 3) then
            gdev = rnorm()
            if(bgross) Messwert(iv) = t_mue + gdev*sqrt(t_sig**two)   ! /mvals)
            if(.not.bgross) then
              ! in this case t_sig is obtained from DistPars%pval(nn,3); it still has
              ! to be divided by sqrt(mvals).
              Messwert(iv) = t_mue + gdev*t_sig/sqrt(mvals)
            end if
          end if
        else
          Messwert(iv) = t_mue + rnorm()*t_sig    ! ausnahmsweise, für
        end if

            if(.false.) then
              if(imcmax >= 1 .and. imc <= 1500000 .and. iv == 4) then
                ttmean(1) = ttmean(1) + (Messwert(iv)) /1500000._rn
                ttvar(1) = ttvar(1) + (Messwert(iv))**two/1500000._rn
                ttmean(2) = ttmean(2) + (t_mue + trand*sqrt( t_sig**two + t_mue/mvals)) /1500000._rn
                ttvar(2) = ttvar(2) + (t_mue + trand*sqrt( t_sig**two + t_mue/mvals))**two/1500000._rn
                ttmean(3) = ttmean(3) + (gdev*sqrt(t_mue/mvals)) /1500000._rn
                ttvar(3) = ttvar(3) + (gdev*sqrt(t_mue/mvals))**two/1500000._rn
                ttmean(4) = ttmean(4) + messwert(iv)/1500000._rn
                ttvar(4) = ttvar(4) + (Messwert(iv))**two/1500000._rn
              end if
            end if
           !if(imc < 10) write(63,*) 't_sig=',sngl(t_sig),' bgross=',bgross,' t_mue=',sngl(t_mue), &
           !                 'MesswertSV(iv)=',sngl(MesswertSV(iv)),' StdUncSV(iv)=',sngl(StdUncSV(iv))

      case (11)      ! Erlang distribution of the counting time in the case of the "preset counts" modus of measurement

        ! the MC results of the following three variants are the same

        if(.false.) then
          ! Vari-1:
          gda_SV = GamDistAdd
          GamDistAdd = zero
          Messwert(iv) = ran_Erlang(MEsswertSV(iptr_rate(iv)),MEsswertSV(iptr_cnt(iptr_rate(iv))))
          GamDistAdd = gda_SV
             ! if(imc == 2) write(63,*) 'ivtl=11:  iv=',int(iv,2)

           !  if(iv == 9 .and. imc <= 300000) dumt0 = dumt0 + Messwert(iv)/300000._rn
           !  if(iv == 9 .and. imc <= 300000) dumt0q = dumt0q + Messwert(iv)**two/300000._rn
           !  if(iv == 9 .and. imc <= 300000) dumn0 = dumn0 + Messwert(8)/Messwert(iv)/300000._rn
           !  if(iv == 9 .and. imc <= 300000) dumn0q = dumn0q + (Messwert(8)/Messwert(iv))**two/300000._rn
        end if
        if(.false.) then
          ! Vari-2:
          ! The results with gamma-distributed t agree with those obtained with ran_Erlang
          gda_SV = GamDistAdd
          GamDistAdd = zero
          if(StdUncSV(iv) > zero) then
            ! Erlang distr. as gamma distribution: Ga(t|n,rho)
            zalpha = MesswertSV(iv)**two/StdUncSV(iv)**two    ! counts
            zbeta  = MesswertSV(iv)/StdUncSV(iv)**two         ! counts/tmess
            if(imc == 1) rnnd = rgamma(ivref(iv),zalpha,.true.) / zbeta
            Messwert(iv) = rgamma(ivref(iv),zalpha,.false.) / zbeta
            GamDistAdd = gda_SV
          end if
        end if
        if(.true.) then
          ! Vari-3:
          ! represents the Bayesian variant including prior für the count rate
          gda_SV = GamDistAdd
          GamDistAdd = zero
          if(StdUncSV(iv) > zero) then
            ! Gamma distribution of the rate: Ga(rho|n,t)
            icnt = iptr_cnt(iptr_rate(iv))     ! icnt: index of the number of counts
            zalpha = MEsswertSV(icnt)          ! counts (SV)
            zbeta  = MesswertSV(iv)            ! tm (SV)
                    if(imc <=2) write(63,*) 'iv=',int(iv,2),' iptr_rate(iv)=',int(iptr_rate(iv),2), &
                                     ' icnt=',int(icnt,2),' ivref(icnt)=',int(ivref(icnt),2)

            if(imc == 1) rnnd = rgamma(ivref(icnt),zalpha,.true.) ! / zbeta * zbeta
            Messwert(icnt) = rgamma(ivref(icnt),zalpha,.false.)   ! / zbeta * zbeta
               if(imc < 3) write(63,*) 'Messwert(icnt) = rgamma=',sngl(Messwert(icnt)),' icnt=',int(icnt,2), &
                                        ' tm=',sngl(Messwert(iv))
            GamDistAdd = gda_SV
            vfixed(icnt) = 1   ! this prevents the number of counts from being replaced later in the iv loop
          end if
        end if

      case default
    end select
25    continue

    IF(.true. .and. .not.FitDecay .and. .not. Gamspk1_Fit .and. .not.Gum_restricted) then
      if(iptr_cnt(iv) > 0 .and. vfixed(iv) == 0) THEN

        ! count rate quantities, which depend on a measured number of counts, must be
        ! gamma-distributed; this is solved by calculating the count rate from a
        ! gamma distributed number of counts.

        ! iptr_cnt: points from a count rate to the associated number of counts
        ! iptr_time: points from a count rate to the associated counting duration

        ! As rgamma is not used in this if clause, the value GamDistAdd does not accur here.

        ! the index iv belongs to a count rate

        help = Messwert(iptr_cnt(iv))    ! counts
        if(iptr_time(iv) > 0) then
          Messwert(iv) = help / Messwert(iptr_time(iv))   ! convert the counts (help) to a count rate
        else
          Messwert(iv) = help    ! counts
        end if

         !     if(iv == 3 .and. imc <= 50000) dum3 = dum3 + (Messwert(iv))/50000._rn
         !     if(iv == 3 .and. imc <= 50000) dum3q = dum3q + (Messwert(iv))**two/50000._rn
         !     if(iv == 3 .and. imc <= 50000) dum37 = dum37 + (Messwert(iptr_time(iv)))/50000._rn
         !     if(iv == 3 .and. imc <= 50000) dum37q = dum37q + (Messwert(iptr_time(iv)))**two/50000._rn
          IF(iv == kbrutto(kEGr)) THEN
           ! if(imc <= 50000) dumx = dumx + Messwert(iptr_cnt(iv))/50000._rn
           ! if(imc <= 50000) dumxq = dumxq + Messwert(iptr_cnt(iv))**two/50000._rn
          end if

      end if
    end if

  end do         ! end Loop iv
  !------------------------------------------------------------------------------------------

  if(FitDecay .and. imc == 1) then
    kmode = 2
    call covppcalc(kmode)
     ! if(imc < 3) then
     !    WRITE(66,*) '  MCCALC:  Kovarianz-Matrix covpmc: '
     !    do i=1,12
     !       WRITE(66,'(30es11.3)') (covpmc(i,j1),j1=1,12)
     !    end do
     ! end if
  end if

  chit1 = zero
  nt1 = 0
  factm = one
  IF(ncov1 > 0) THEN
    ! method for producing random numbers of correlated variables; it must assume
    ! that the correlated variables are normal-distributed.

    ! Warning:     Example: DWD_AB-Gesamt-Aeros-Beta1.txp
    ! If the gross count rate variable (index nvar, kbrutto) is contained
    ! in the array icnzg, during the iteration for DT or DL not only the vector muvect,
    ! but also the covariance matrix covxy must be varied.

    ! icn: 1 x number of pairs of covariances

    chit1 = zero
    nt1 = 0
    icnvar = 0
    if(.not.Gum_restricted .and. nvar > 0) then
      do i=1,icn
        IF(icnzg(i) == nvar) icnvar = i
      end do
      if(icnvar == 0) then
        do i=1,icn
          IF(icnzg(i) == iptr_cnt(nvar)) icnvar = i
        end do
      end if
    end if

    IF(icn > 0) then
         ! The non-diagonal matrix elements probably need not to be varied.
         ! MesswertSV(nvar) and StdUnc(nvar) given here, are the alsready varied value (and uncertainty)

      if(nvar > 0 .and. icnvar > 0) then
        if(iptr_cnt(nvar) == 0) then
          muvect(icnvar) = MesswertSV(nvar)
          covxy(icnvar,icnvar) = StdUncSV(nvar)**two
        else
          muvect(icnvar) = MesswertSV(iptr_cnt(nvar))
          covxy(icnvar,icnvar) = StdUncSV(iptr_cnt(nvar))**two
        end if
      end if
      if(kqtyp > 1 .and. nvar > 0 .and. icnvar == 0 .and. icn == 2) then
        factm = one
        if(iptr_cnt(nvar) == icnzg(1)) then
          muvect(1) = MesswertSV(iptr_cnt(nvar))
          covxy(1,1) = StdUncSV(iptr_cnt(nvar))**two
          factm = muvect(1) / muvect0(1)
        elseif(iptr_cnt(nvar) == icnzg(2)) then
          muvect(1) = MesswertSV(iptr_cnt(nvar))
          covxy(2,2) = StdUncSV(iptr_cnt(nvar))**two
          factm = muvect(2) / muvect0(2)
        end if
      end if

      do k=1,ncov1
        if(covariter(k)) then

          ! This sel case construct corresponds to that one in Upropa;
          ! (Index k replaced here by kv1(k)):
          select case (icovtyp(kv1(k)))
            case (1)
              ! Type covariance:
              IF(LEN_TRIM(CVFormel(kv1(k))%s) > 0) THEN
                CovarVal(kv1(k)) = evalf(kgl(k),Messwert)
              end if
            case (2)
              ! Type correlation:
              IF(LEN_TRIM(CVFormel(kv1(k))%s) == 0 .and. abs(CovarVal(kv1(k))-zero)>eps1min) THEN
                CovarVal(kv1(k)) = CovarVal(kv1(k)) / ( StdUncSV(ISymbA(kv1(k))) * StdUncSV(ISymbB(kv1(k))) )
                CovarVal(kv1(k)) = CovarVal(kv1(k)) * ( StdUnc(ISymbA(kv1(k))) * StdUnc(ISymbB(kv1(k))) )
              end if
              IF(LEN_TRIM(CVFormel(kv1(k))%s) > 0 )  THEN
                CovarVal(kv1(k)) = evalf(kgl(k),Messwert)
                CovarVal(kv1(k)) = CovarVal(kv1(k))*StdUnc(ISymbA(kv1(k)))*StdUnc(ISymbB(kv1(k)))
              end if
            case default
          end select
          covxy(nf1(k),nf2(k)) = covarval(kv1(k))
          covxy(nf2(k),nf1(k)) = covarval(kv1(k))
        end if
      end do
          !if(imc < 5) then
          !  write(66,*) 'covxy in line 954:'
          !  do i=1,2
          !    write(66,*) (sngl(covxy(i,j)),j=1,2)
          !  end do
          !end if
    end if    ! icn > 0

    mms = 0
    do nc1=1,ncgrp    ! loop over sub-groups of correlated variables
                      ! that are contained in the single decay function terms

      icd1 = icovn(nc1)

      if(allocated(Rmat)) Deallocate(Rmat)
      if(allocated(z)) Deallocate(z)
      if(allocated(b)) Deallocate(b)
      if(allocated(bvect)) Deallocate(bvect)
      if(allocated(zvect)) Deallocate(zvect)
      if(allocated(muvectt)) Deallocate(muvectt)

      allocate(Rmat(1:icd1,1:icd1),z(1:icd1,1),b(1:icd1,1),bvect(1:icd1))
      allocate(zvect(1:icd1,1),muvectt(icd1))
      if(allocated(covxyt)) Deallocate(covxyt)
      allocate(covxyt(1:icd1,1:icd1))
      covxyt = zero

      do k1=1,icd1
        zvect(k1,1) = rnorm()
        muvectt(k1) = muvect(icovgrp(nc1,k1))
      end do
      Forall(k1=1:icd1, k2=1:icd1)
        covxyt(k1,k2) = covxy(icovgrp(nc1,k1),icovgrp(nc1,k2)) * factm
      End Forall

      if(.false. .and.imc < 5) then
        ! write(63,*) 'covxyt:  size1, size2 =',int(size(covxyt,1)),int(size(covxyt,2))
        ! write(63,*) 'covxyt:  size1 =',int(size(covxyt(1:icd1,1:icd1),1))
        write(63,*) 'covxyt:'
        do k1=1,icd1
          write(63,*) (sngl(covxyt(k1,k2)),k2=1,icd1)
        end do
        write(63,*)
        write(63,*) 'covx:'
        do k1=1,icd1
          write(63,*) (sngl(covxyt(k1,k2)),k2=1,icd1)
        end do
        write(63,*)

      end if

      call MatRand(icd1,icd1,covxyt(1:icd1,1:icd1),muvectt,zvect,bvect,imc)
      goto 1212
1212   continue
      mms = 0
      do i=1,icd1
        Messwert(icnzg(icovgrp(nc1,i))) = bvect(i)
        if(StdUncSV(icnzg(icovgrp(nc1,i))) > zero) then
          ut1 = abs(bvect(i)-MEsswertSV(icnzg(icovgrp(nc1,i))))/StdUncSV(icnzg(icovgrp(nc1,i)))
          chit1 = chit1 + ut1**two
          nt1 = nt1 + 1
          if(abs(bvect(i)-MEsswertSV(icnzg(icovgrp(nc1,i))))/StdUncSV(icnzg(icovgrp(nc1,i))) &
                                                                              > 6.0_rn) then
            mms = 1
            mms_arr(icnzg(icovgrp(nc1,i))) = 1
          end if
        end if
      end do
      if(.true. .and. mms == 1 .and. imc < 30 .and. mmkk < 4)  then
        write(23,*) ' Deviation with using MATRAND-Würfeln, Group 2:  bvect=',(sngl(bvect(j)),j=1,icovn(nc1))
        write(23,*) '     imc=',imc,'  kqt=',kqtyp,'  icnzg(icovgrp(nc1,j))=',(icnzg(icovgrp(nc1,j)),j=1,icovn(nc1))
        write(23,*) '     imc=',imc,'   vector MesswertSV=',(sngl(MEsswertSV(icnzg(icovgrp(nc1,j)))),j=1,icovn(nc1))
        write(23,*) '     imc=',imc,'   vector StdUncSV=',(sngl(StdUncSV(icnzg(icovgrp(nc1,j)))),j=1,icovn(nc1))
        write(23,*) '     imc=',imc,'   vector zvect=',(sngl(zvect(j,1)),j=1,icovn(nc1))
      end if
    end do    ! do nc1=1,ncgrp

    mcov = 0
    k11 = 0
    nc1m = 1
    if(FitDecay .and. knumEGr > 1) then
      nc1m = 2
      mcov = 3
    end if
    ! do nc1=1,ncgrp
    do nc1=nc1m,ncgrp
      do k1=1,icd1*(icd1-1)/2
        ! mcov and k11 differ for FitDecay;  k11 starts with 1; mcov starts with 3;
        k11 = k11 + 1
        if(k11 > ubound(kv1,dim=1)) cycle
        if(kv1(k11) == 0) cycle      ! If e.g. for 3 variables in a correlation group, only two covarainces are given
        if(kv1(k11) <= 0 .or. kv1(k11) > ngrs+ncov+numd) cycle  ! if covariance ndef
        mcov = mcov + 1      ! die cycle-Fälle auslassen
        MEsswert(ngrs+mcov) = zero
        if(abs( MesswertSV(IsymbA(kv1(k11)))*MesswertSV(IsymbB(kv1(k11))) ) > eps1min) then
          dummy = covarval(kv1(k11)) /(MesswertSV(IsymbA(kv1(k11)))*MesswertSV(IsymbB(kv1(k11))))
          dummy = dummy * Messwert(IsymbA(kv1(k11)))*Messwert(IsymbB(kv1(k11)))
          MEssWert(ngrs+mcov) = dummy
        end if
      end do
    end do

    if(imc < 10 .and. mms == 1) then
      write(63,*) 'random value of Messwert deviates strongly: '
      do i=1,ngrs+ncov+numd
        if(mms_arr(i) == 0) cycle
        write(63,*) 'i=',i,' ',symboleG(i)%s,'  MW=',sngl(Messwert(i)),'  ratio=',sngl(Messwert(i)/MesswertSV(i))
      end do
    end if

    ! Reset covariances:
    do k=1,ncov1
      IF(abs(covarvalSV(kv1(k))-missingval) > eps1min) THEN
        covxy(nf1(k),nf2(k)) = covarvalSV(nf3(k))
        covxy(nf2(k),nf1(k)) = covarvalSV(nf3(k))
      end if
    end do

  end if      ! if(ncov1 > 0)

!-----  -------------------------------------------------------------------------------------
  if(FitDecay) then
    if(k_rbl > 0) then
         ! produce a random value of the blank value:  once per decay curve
      if(StdUncSV(kpoint(k_rbl)) > zero) then
        Messwert(kpoint(k_rbl)) = rblindnet + StdUncSV(kpoint(k_rbl))*rnorm()
      end if
      mw_rbl = Messwert(kpoint(k_rbl))
    end if
  end if

  IF(FitDecay) THEN
    if(.not.allocated(d0zratez))  allocate(d0zrateZ(numd))
    if(.not.allocated(rblindnetZ))  allocate(rblindnetZ(numd))

    use_afuncSV = .true.
        !  use_afuncSV = .false.

    ! Generate random gross count rate values of the decay curve
    !   they can be considered as statistically independent
    !------
    ! The procedure of generating random values of a decay curve must not depend
    ! on the method chosen for fitting!!!

    do messk=1,nchannels
      R0kz(messk) = fgsl_ran_poisson( r5, real(R0k(messk)*d0messzeit(1),fgsl_double))/d0messzeit(1)
    end do
    do i=1,numd
      d0zrateZ(i) = fgsl_ran_poisson( r5, real(d0zrateSV(i)*d0messzeit(i), fgsl_double)) / d0messzeit(i)
    end do

    ivant = 1

    ! from now on, within the imc-loop, only the value mw_rbl (see few lines above)
    ! will be used as blank value!
    if(ivant == 1) then
      do i=1,numd

        messk = FindMessk(i)
        kix = ngrs + ncov + i

        IF(i == 1) then
          bgv = d0zrateSV(1)
          if(k_rbl > 0) bgv = bgv + MesswertSV(kpoint(k_rbl))
        end if

        if(nkovzr == 1 .and. k_rbl > 0) rblindnetZ(i) = mw_rbl            ! not: Messwert(kpoint(k_rbl))        ! mit ZR-Kovarianzen
        if(nkovzr == 0 .and. k_rbl > 0) rblindnetZ(i) = rblindnet + StdUncSV(kpoint(k_rbl))*rnorm()  ! ohne ZR-Kovarianzen

        ! netfit: calculated net count rate
        ! The call of Funcs must be omitted here, otherwise the uncertainties of the parameters
        ! of the decay curve function may lead to an increased spreading of the gross count numbers,
        ! more than to be expected from SQRT(Rb/tm), which must be avoided.

        netfit(i) = zero
            if(.not. use_afuncSV .or. parfixed)  call Funcs(i,bfunc)
        do k=1,ma
          fparm = fpa(k)
          if(kqtyp >= 2 .and. fpa(k) < zero) fparm = zero    ! this line is a must for example project (LUBW-fixed-Sr85!)

          if(use_afuncSV) afu = afuncSV(i,k)
          if(.not. use_afuncSV) afu = bfunc(k)

          IF(ifit(k) == 3) CYCLE
          if(kPMLE == 1 .and. k == mfrbg .and. ifit(k) > 2) cycle

          !-------
          IF(kqtyp == 1) then
            if(ifit(k) == 1) netfit(i) = netfit(i) + fparm * afu
            if(kPMLE == 1) then
              if(ifit(k) == 2) netfit(i) = netfit(i) + fparm * afu
            else
              if(ifit(k) == 2) netfit(i) = netfit(i) + afu
            end if
          else
            if(kPMLE /= 1 ) then
              if(ifit(k) == 1) netfit(i) = netfit(i) + fparm * afu
              if(ifit(k) == 2) netfit(i) = netfit(i) + afu
            else
              IF(k == mfrbg) THEN
                if(ifit(k) == 2) then
                  if(kqtyp == 3) netfit(i) = netfit(i) + max(zero, fparm) * afu
                  if(kqtyp == 2) netfit(i) = netfit(i) + max(zero, fparm) * afu
                end if
              else
                netfit(i) = netfit(i) + max(zero, fparm) * afu
              end if
            end if
          end if
          !----------------------------------------------------------
        end do

        Messwert(kix) = max(netfit(i), zero)
        IF(kPMLE /= 1) then
          if(nkovzr == 1 .and. konstant_r0) then
            Messwert(kix) =  Messwert(kix) + R0k(messk) + rblindnet
          else
            Messwert(kix) =  Messwert(kix) + d0zrateSV(i) + rblindnet   ! mw_rbl      !  + rblindnetZ(i) )
          end if
        else
          ! ?????
        end if
        Messwert(kix) = MAX(zero,real(fgsl_ran_poisson(r5, real(Messwert(kix)*dmesszeit(i),fgsl_double)),rn))/dmesszeit(i)
        ! Calculate now the background count rates to be used later in Linf
        if(nkovzr == 1 .and. konstant_r0) then
          d0zrate(i) = R0kZ(messk)
        else
          d0zrate(i) = d0zrateZ(i)
        end if

        sd0zrate(i) = SQRT( MAX(1.E-30_rn,d0zrate(i))/d0messzeit(i) )

        if(parfixed) then
          call Funcs(i,bfunc)
          do k=1,ma
            if(ifit(k) == 2) then
              if(nkovzr == 1) fixedrateMC(i) = bfunc(k)
              if(nkovzr == 0) then
                fixedrateMC(i) = bfunc(k) + sdfixedrate(i)*rnorm()
              end if
            end if
          end do
        end if

        if(nkovzr == 1 .and. konstant_r0) then
            Mwnet(i) = Messwert(kix) - R0kZ(messk)
            if(k_rbl > 0) Mwnet(i) = Mwnet(i) - Messwert(kpoint(k_rbl))  ! mw_rbl
        else
          Mwnet(i) = Messwert(kix) - d0zrateZ(i)
          if(k_rbl > 0) Mwnet(i) = Mwnet(i) - rblindnetZ(i)
        end if
        if(parfixed) Mwnet(i) = Mwnet(i) - fixedrateMC(i)
        if(kqtyp >= 1 .and. i == 2) then
          covmw12 = covmw12 + (Mwnet(1)-mwnetvgl(1))*(Mwnet(2)-mwnetvgl(2))
          mw12 = mw12 + Mwnet(1)
          mw12q = mw12q + Mwnet(1)**two
        end if
        !------

        IF(.false. .and. kqtyp <= 2 .and. imc < 6 .and. kcrun == 1 .and. mmkk == 1)  then
          if(i == 1 .and. imc == 1) write(196,*) 'kqtyp=',kqtyp,' kEGr=',kEGr
          write(196,'(2(a,i3),6(a,es10.3),a,3es10.3,2(2x,a,3es10.3,1x,a,es10.3),a,es10.3)') 'MCC: imc=',imc,  &
                           '  i=',i,' MW-MC=',real(Messwert(kix),8), &
                           ' gross=',real(netfit(i)+R0kZ(messk)+ mw_rbl,8),  &
                           ' Mwnet=',real(Mwnet(i),8),' netfit=',real(netfit(i),8), &
                           ' R0=',real(d0zrate(i),8),' Rblw=',real(mw_rbl,8),'  fpa=',(real(fpa(j),8),j=1,3), &
                           ' afuncSV=',(real(afuncSV(i,j),8),j=1,3),' Mwnetvgl(i)=',real(Mwnetvgl(i),8)
                           !  ,' bfunc=',(bfunc(j),j=1,3)   ! , ' fixrate=',(fixedrateMC(i))
        end if

      end do   ! do i=1,numd
    end if         ! End ivant=1

  end if          ! IF(FitDecay) THEN

  if(nt1 > 0) chit1 = chit1 / real(nt1,rn)

  !.............................................................................

  ifehl = 0
         !  if(imc < 100) Write(63,*) 'before Res: imc=',int(imc,2),' MW(2)=',sngl(Messwert(1:10))
  ! Calculate a single MC value of the output quantity with index kEGr:
  MCWert = Resulta(kEGr)
         ! if(imc < 100) Write(63,*) 'nach Res: imc=',int(imc,2),' MW(2)=',sngl(Messwert(1:10))

     if(ifehl == 1) then
       write(63,*) 'MCWERT: ifehl = 1,  MCWert= ',sngl(MCwert)
     end if

  if(kr == 1 .and. imc <= imcmax/50) then
    if(ISNAN(MCWert)) then
      ifehl = 1
      call WrStatusbar(3,'MCsingRun: MC value is NaN!')
      write(63,*) 'MCSingRun failed: MC value is NaN:   values:'
      goto 9000
    end if
  end if

  IF(ifehl == 1) then
      write(63,*) 'ifehl = 1   after: MCWert = Resulta(kEGr)'
    goto 9000
  end if

  !::::::::::::::::::::
  valaccpt = .false.
  if(Gum_restricted) then
    imc2 = imc2 + 1
    arraymc(imc2,kqtyp) = MCWert
    valaccpt = .true.
  else
    if( (kqtyp == 1 .and. MCWert >= zero) .or. kqtyp == 2 .or. kqtyp == 3 ) then
      imc2 = imc2 + 1
      arraymc(imc2,kqtyp) = MCWert
      valaccpt = .true.
    end if
  end if
  !::::::::::::::::::::
           if(imc <= 50000) dumres = dumres + Messwert(1)/50000._rn
           if(imc <= 50000) dumresq = dumresq + Messwert(1)**two/50000._rn

          if(use_bipoi) then
             if(imc <= 50000) dumrnet = dumrnet + Messwert(knetto(kEgr))/50000._rn
             if(imc <= 50000) dumrnetq = dumrnetq + Messwert(knetto(kEgr))**two/50000._rn
             if(imc <= 50000) dumres = dumres + Messwert(kEgr)/50000._rn
             if(imc <= 50000) dumresq = dumresq + Messwert(kEgr)**two/50000._rn
          end if

  if(imc <= 101) then
    mcval(imc) = MCWert
    if(imc == 101) then
      mcnonvary = .true.
      do i=1,100
        if(abs(mcval(i)-mcval(i+1)) > eps1min ) mcnonvary = .false.
      end do
      if(mcnonvary) then
        IF(langg == 'DE') str1 = 'Die MC-Werte zeigen keine Variation!' // char(13) // &
                'Daher wird die MC-Simulation abgebrochen!'
        IF(langg == 'EN') str1 = 'The MC values do not show any variation!' // char(13) // &
                'Therefore, the MC simulation is  interrupted!'
        IF(langg == 'FR') str1 = 'Les valeurs MC ne montrent aucune variation!' // char(13) // &
                'Par conséquent, la simulation MC est interrompue!'
        call MessageShow(trim(str1), GTK_BUTTONS_OK, "MCcalc:", resp,mtype=GTK_MESSAGE_WARNING)
        call gtk_widget_hide(windowPL)
        ifehl = 1
          do i=1,ngrs
            write(63,*) 'i=',int(i,2),' MW(i)=',sngl(Messwert(i)),' MWSV(i)=',sngl(MesswertSV(i))
          end do
        goto 9000
      end if

    end if
  end if

        !.............................................................................
  Messwertw(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)        ! save random values (w) of Messwert

  if(WTLS_wild) ntwild = ntwild + 1

  if(FitDecay .and. knumEGr > 1) then
    do k=1,3
      read(Symbole(IsymbA(k))%s(5:5),*) jj1          ! index of the "left" Fitp parameter
      read(Symbole(IsymbB(k))%s(5:5),*) jj2          ! index of the "right" Fitp parameter
      Messwert(ngrs+k) = covar(jj1,jj2)

      !  MC determination of covariances between output quantities:
      if(.false. .and. k == 1) then
        mw1 = MesswertSV(1) * (Messwert(kfitp(1))/MesswertSV(kfitp(1))) *  &
                              (MesswertSV(kfitp(1)+2)/Messwert(kfitp(1)+2)) / Messwert(5)
        mw2 = MesswertSV(2) * (Messwert(kfitp(1)+1)/MesswertSV(kfitp(1)+1)) * &
                              (MesswertSV(kfitp(1)+2)/Messwert(kfitp(1)+2)) / Messwert(5)
        ssxEG(k) = ssxEG(k) + (mw1 - MesswertSV(1))*(Mw2 - MesswertSV(2))
      end if
       !if(k == 2 .and. knumEGr == 3) ssxEG(k) = ssxEG(k) + (Messwert(1)-MesswertSV(1))*(Messwert(3)-MesswertSV(3))
       !if(k == 3 .and. knumEGr == 3) ssxEG(k) = ssxEG(k) + (Messwert(2)-MesswertSV(2))*(Messwert(3)-MesswertSV(3))
       !if(k == 1) ssxEG(k) = ssxEG(k) + (fpa(1)-fpaLYT(kqtyp,1))*(fpa(2)-fpaLYT(kqtyp,2))

    end do

    if(kfitp(1) > 0 .and. knumEGr > 1) then
      MEsswert(kfitp(1)) = fpa(1)
      MEsswert(kfitp(1)+1) = fpa(2)
      MEsswert(kfitp(1)+2) = fpa(3)
    end if
  end if

  IF(kqtyp == 2 .and. FitDecay .and. singlenuk .and. (a(1) < r0dummy - 5.*sdr0dummy)) THEN
    knegative = knegative + 1
    if(knegative < 0) then
      WRITE(63,*) 'MC value strongly negativ: imc=',imc
      do i=1,numd
        WRITE(63,*) '    MCCALC: i=',i,'  GRcountsnew=',INT(Messwert(ngrs+ncov+i)*dmesszeit(i)), &
                    '  netcountsnew=',sngl(netfit(i)*dmesszeit(i)),' BGcounts=',   &
                           sngl(( d0zrate(i) + mw_rbl )*dmesszeit(i)), &
                           ' netcounts=',sngl(Messwert(ngrs+ncov+i)*dmesszeit(i) - ( d0zrate(i) + mw_rbl )*dmesszeit(i) )
      end do
      WRITE(63,*) '      fpa: ',(sngl(fpa(i)),i=1,3),'  ifit=',ifit,'  kqtyp=',kqtyp,  &
                                                             '  konstant_r0=',konstant_r0
      write(63,*) '     sfpa: ',(sngl(sfpa(i)),i=1,3)
      WRITE(63,*) '    d0zrate(1)*tm=',sngl(d0zrate(1)*dmesszeit(1)),'  BLW=',  &
                                            sngl(mw_rbl*dmesszeit(1))
      CYCLE
    end if
  end if

  minres = MIN(mcwert,minres)
  maxres = MAX(mcwert,maxres)

  imcPE = imcPE + 1
  xmit1PE = xmit1PE + (MCWert - MesswertSV(kEGr))          !  primary estimate
  xmit1qPE = xmit1qPE + (MCWert - MesswertSV(kEGr))**two     !

  if(.not.valaccpt) CYCLE


  ! In the following, the analytical/original values (val0) are subtracted from the MC values,
  ! which shall avoid rounding problems for the standard deviations calculated sunsequently.
  !        xmit1 = xmit1 + (MCval - val0)
  !        xmitq = xmitq + (MCval - val0)**two
  ! Such a (massive) problem, ocurred with the first example project after Kessel,
  ! Kacker and Berglund, where the variable Ms had the value 10000, while the given standard
  ! deviation was only 2.25E-02, i.e., the relative standard deviation was 2.25E-07 !
  ! This linear Transformation (see Numerical Recipes, 1992) does not change the final value
  ! of the standard deviation, but helps to avoid numerical problems.

  ! intermediate values for mean and variance for output quantity:
  imctrue = imctrue + 1
  xmit1 = xmit1 + (MCWert - MesswertSV(kEGr))
  xmitq = xmitq + (MCWert - MesswertSV(kEGr))**two
    if(FitDecay .and. kqtyp == 2) then
          helpz(1:numd) = MWnetmit(1:numd) - (MWnet(1:numd) - mwnetvgl(1:numd))      ! 16.8.2023
      MWnetmit(1:numd) = helpz(1:numd)
      MWnetmitq(1:numd) = helpz(1:numd)**two
    end if

  IF(FitDecay .or. Gamspk1_Fit) THEN
    xesdev1 = xesdev1 + (sfpa(kEGr) - xsfpa(kEGr))
    xesdevq = xesdevq + (sfpa(kEGr) - xsfpa(kEGr))**two
    xemit1  = xemit1 + (fpa(kEGr) - xfpa(kEGr))
    xemitq  = xemitq + (fpa(kEGr) - xfpa(kEGr))**two

    IF(FitDecay .and. ifit(2) == 1) THEN
      xemit2  = xemit2 + (fpa(2) - xfpa(2))
      xemitq2  = xemitq2 + (fpa(2) - xfpa(2))**two
    end if
  end if

  ! intermediate values for means and variances of input quantities:
   xmitsgl(1:ngrs+ncov+numd)  = xmitsgl(1:ngrs+ncov+numd) +  &
                     (Messwert(1:ngrs+ncov+numd) - MesswertSV(1:ngrs+ncov+numd))
   xmitsglq(1:ngrs+ncov+numd) = xmitsglq(1:ngrs+ncov+numd) + &
            (Messwert(1:ngrs+ncov+numd) - MesswertSV(1:ngrs+ncov+numd))**two


end do        ! end MC-loop (IMC) ------------------------------------------------
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       ! MCSim_on = .false.

    ! write(0,*) 'after imc_loop'

    ! write(kunit,*) 'MCSR: mean(Ng)=',sngl(dumx),'  var(Ng)=',sngl(dumxq - dumx**two)
               !                   ' MesswertSV(12)=',sngl(MesswertSV(12))
  !   write(kunit,*) 'mean(mw1)=',sngl(dumres),'  var(mw1)=',sngl(dumresq - dumres**two)

    !  write(kunit,*) 'mean(Nbin*p)=',sngl(dumy),'  var(Nbin*p)=',sngl(dumyq - dumy**two), &
    !                  ' expect mean=',sngl(Nbin0*Messwert(ip_binom)), &
    !                  ' expect var=',sngl(Nbin0*Messwert(ip_binom) * (one-Messwert(ip_binom)))

    !write(kunit,*) 'mean(Ng)=',sngl(dumy),'  var(Ng)=',sngl(dumyq - dumy**two), 'expect var=', &
    !              sngl( dumy*(one - MesswertSV(ip_binom)) + MesswertSV(ip_binom)*Rbltot(kEGr)*MesswertSV(itm_binom) )

      imc = 0
call pending_events
if(imctrue == 0) then
  write(cminus,'(a,i0,a,i0,a,i0)') 'n_left: ',nminus,',  n_right: ',nplus,' kqt=',kqtyp
  if(langg == 'DE') then
    str1 = 'imctrue ist Null (alle gewürfelten Werte ausserhalb des Bereichs!)' // &
                      char(13) // trim(cminus) // &
                      char(13) // 'Abbruch der MC-Simulation.'
  end if
  if(langg == 'EN') then
    str1 = 'imctrue is null (all sampled MC values outside the allowed range!)' // &
                      char(13) // trim(cminus) // &
                      char(13) // 'The MC simulation is stopped.'
  end if
  if(langg == 'FR') then
    str1 = 'imctrue est null (toutes les valeurs MC échantillonnées en dehors de la plage autorisée!)' // &
                      char(13) // trim(cminus) // &
                      char(13) // 'La simulation MC est arrêtée.'
  end if
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "MCCalc:", resp,mtype=GTK_MESSAGE_WARNING)
  ifehl = 1
  write(63,*) 'imctrue=',imctrue,' letzter Datensatz:'
  do k=1,ngrs+ncov+numd
    write(63,*) 'k=',k,'Messwert(k)=',sngl(Messwert(k))
  end do
  goto 9000
end if

if(.false.) then
  do i=1,4
    ttvar(i) = ttvar(i) - ttmean(i)**two
    write(63,*) 'i=',int(i,2),' ttmean(i)=',sngl(ttmean(i)),'  ttsig(i)=',sngl(sqrt(ttvar(i)))
  end do
end if

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if(.true.) then
  imcmax2 = imc2
  imctrue = imc2

  if(allocated(indx)) deallocate(indx)
  allocate(indx(1))

  if(.false.) then
    !testing cpu time:
    call cpu_time(stt3)
    do jj=1,50
      call Quick_Sort_r(arraymc(1:imctrue,kqtyp),indx)
    end do
    call cpu_time(stp3)
    write(66,*) 'Sorting with Quick_sort_r: sec =',sngl(stp3-stt3)
  end if

  call Quick_Sort_r(arraymc(1:imctrue,kqtyp),indx)

  if(kr == 1) then
    mca_min(kqtyp) = quantileM(0.00001_rn,arraymc(1:imctrue,kqtyp),imctrue)
    mca_max(kqtyp) = quantileM(0.99990_rn,arraymc(1:imctrue,kqtyp),imctrue)

    if(abs(mca_max(kqtyp)/mca_min(kqtyp)) > 1.0001_rn) then
      mca_min(kqtyp) = mca_min(kqtyp) / 1.05_rn
      mca_max(kqtyp) = mca_max(kqtyp) * 1.05_rn
    end if
    xstep(kqtyp) = (mca_max(kqtyp) - mca_min(kqtyp))/real(mcmax,rn)
  end if

  ! multi-channel spectrum (MCA) of the probability distribution of the output quantity
  mcasum(kqtyp) = 0
  ii1 = 1E+8
  ii2 = 0
  do i=1,imctrue
    IF(arraymc(i,kqtyp) >= mca_min(kqtyp) .AND. arraymc(i,kqtyp) <= mca_max(kqtyp) ) THEN
      izv = INT( (arraymc(i,kqtyp) - mca_min(kqtyp)) / xstep(kqtyp) + 0.4999 )
      izv = MAX(izv,1)
      mcafull(kqtyp,izv) = mcafull(kqtyp,izv) + 1
      mcasum(kqtyp) = mcasum(kqtyp) + 1
      ii1 = min(ii1,i)
      ii2 = max(ii2,i)
    end if
  end do
end if

    kmin = 1
meanmc(kqtyp) = mean(arraymc(ii1:ii2,kqtyp))
sdmc(kqtyp)   = sd(arraymc(ii1:ii2,kqtyp))

select case (kqtyp)
  case (1)
    prob = (one - w1minusG)/two
    call quantile(prob,1,imctrue,arraymc(1:imctrue,kqtyp),qt,qtindex,meanmc(kqtyp),sdmc(kqtyp))
    estLQ = qt
    prob = one - (one - w1minusG)/two
    call quantile(prob,1,imctrue,arraymc(1:imctrue,kqtyp),qt,qtindex,meanmc(kqtyp),sdmc(kqtyp))
    estUQ = qt
    medianqt(1) = arraymc(imctrue/2,kqtyp)
    call SearchBCI3(1,imcmax2,kqtyp)

  case (2)
    prob = one - alpha
    call quantile(prob,1,imctrue,arraymc(1:imctrue,kqtyp),qt,qtindex,meanmc(kqtyp),sdmc(kqtyp))
    xxDT(kr) = qt
    estUQ = qt
    medianqt(2) = arraymc(imctrue/2,kqtyp)

  case (3)
    prob = beta
    kmin = 1
    do i=1,imctrue
      kmin = i
      if(arraymc(i,kqtyp) >= zero) exit
    end do
      kmin = 1
    call quantile(prob+(real(kmin,rn)/real(imctrue,rn)),1,imctrue,arraymc(1:imctrue,kqtyp),qt,qtindex,meanmc(kqtyp),sdmc(kqtyp))
    estLQ = qt
    medianqt(3) = arraymc(imctrue/2,kqtyp)

  case default
end select
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::

! Evaluation of MC-results:
  gamvarmean = gamvarmean/real(imcmax,rn)
  gsum = gsum / real(imcmax,rn)
    ! write(63,*) 'Gamma-Variable: min=',sngl(gamvarmin),'  max=',sngl(gamvarmax), &
    !              '  mean=',sngl(gamvarmean),' gsum=',sngl(gsum)

! Evaluation of primary estimate:
xmit1PE = xmit1PE / real(imcPE,rn)
xsdvPE = SQRT( (xmit1qPE - real(imcPE,rn)*xmit1PE**two) / real(imcPE-1,rn) )
xmit1PE = xmit1PE + MesswertSV(kEGr)


! Std devaition according to Eq. Gl. 14.1.7 in the Numerical Recipes:
xmit1 = xmit1 / real(imctrue,rn)
xsdv = SQRT( (xmitq - real(imctrue,rn)*xmit1**two) / real(imctrue-1,rn) )
xmit1 = xmit1 + MesswertSV(kEGr)

    !write(63,'(5x,2(a,i2),4(a,es12.5))') 'kqtyp=',kqtyp,' kr=',kr,' xmit1=',xmit1,'  xsdv=',xsdv, &
    !                          ' meanmc=',meanmc(kqtyp),'  sdmc=',sdmc(kqtyp)

if(FitDecay .and. kqtyp == 2) then
  mwnetmit(1:numd) = mwnetmit(1:numd) / real(imctrue,rn)
  xsdnet(1:numd) = SQRT( (mwnetmitq(1:numd) - real(imctrue,rn)*mwnetmit(1:numd)**two) / real(imctrue-1,rn) )
  mwnetmit(1:numd) = mwnetmit(1:numd) + Mwnetvgl(1:numd)
end if
if(FitDecay) then
  covmw12 = covmw12/real(imctrue,rn)
  mw12 = mw12 / real(imctrue,rn)
  sdmw12 = SQRT( (mw12q - real(imctrue,rn)*mw12**two) / real(imctrue-1,rn) )
end if

IF(FitDecay .or. Gamspk1_Fit) THEN
  xesdev1 = xesdev1 / real(imctrue,rn)
  xesdev2 = SQRT( (xesdevq - real(imctrue,rn)*xesdev1**two) / real(imctrue-1,rn) )
   xesdev1 = xesdev1 + xsfpa(kEGr)
  xemit1  = xemit1 / real(imctrue,rn)
  xesdev3 = SQRT( (xemitq - real(imctrue,rn)*xemit1**two) / real(imctrue-1,rn) )
   xemit1 = xemit1 + xfpa(kEGr)

  IF(FitDecay .and. ifit(2) == 1) THEN
    xemit2  = xemit2 / real(imctrue,rn)
    xesdev22 = SQRT( (xemitq2 - real(imctrue,rn)*xemit2**two) / real(imctrue-1,rn) )
    xemit2 = xemit2 + xfpa(2)
  end if

end if
if(FitDecay .and. knumEGr > 1) then
  ssxEG(1:3) = ssxEG(1:3) / real(imctrue,rn)
end if

xmitsgl(1:ngrs+ncov+numd) = xmitsgl(1:ngrs+ncov+numd) / real(imctrue,rn)
do k=1,ngrs+ncov+numd
  xsdvsgl(k) = zero
    dummy = xmitsglq(k) - real(imctrue,rn)*(xmitsgl(k))**two
  if(dummy > zero) xsdvsgl(k) = SQRT( (dummy) / real(imctrue-1,rn) )
end do
xmitsgl(1:ngrs+ncov+numd) = xmitsgl(1:ngrs+ncov+numd) + MesswertSV(1:ngrs+ncov+numd)

Messwert(1:ngrs+ncov+numd) = xmitsgl(1:ngrs+ncov+numd)
xmit2 = Resulta(kEGr)
Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)

9000   continue

end subroutine MCsingRun

!#######################################################################

subroutine quantile(p,mode,n,x,qt,j,x0,sigma)

! Literature:
!             Quantile:
!             R.J. Hyndman & Y. Fan: Sample Quantiles in statistical Packages.
!             The American Statistitian, Vol. 50(4), Nov. 1996, 361-365
!             Their definition 9, according to Blom (1958)

!             Quantile variance:
!             K.Y. Cheung & S.M.S. Lee: VARIANCE ESTIMATION FOR SAMPLE QUANTILES USING THE m OUT
!             OF n BOOTSTRAP. Ann. Inst. Statist. Math. Vol. 57, No. 2, 279-290 (2005)
!             Their Eq. (1.1)

use UWB,            only: median
use UR_params,      only: rn,eps1min,one,zero,three

implicit none

real(rn),intent(in)    :: p           ! probability
integer(4),intent(in)  :: mode        !   1:  lower quantile; 2: upper quantile
integer(4),intent(in)  :: n           ! number of values in the sorted array x
real(rn),intent(in)    :: x(n)        ! Array of values
real(rn),intent(out)   :: qt          ! estimated quantile
integer(4),intent(out) :: j           ! quantile index
real(rn),intent(in)    :: x0          ! centre of the MC distribution
real(rn),intent(in)    :: sigma       ! standard deviation of associated MC distribution

real(rn)            :: ggamma,uq,xm,valp,vbet
logical             :: prout

prout = .false.
  ! prout = .true.

  if(prout) write(66,'(1(a,i0),a,f0.5,2(a,i0))') 'Quantile:   n=',n,'  p=',sngl(p),  &
                                                  ' mode=',mode,'  size(x)=',size(x)

valp = three/8._rn
vbet = valp
xm = valp + p * (one-valp-vbet)
j = INT( p*real(n,rn) + xm )
j = max(j,1)
j = min(j,n-1)

ggamma = p*real(n,rn) + xm - real(j,rn)
   if(prout) write(66,'(3(a,i0),1(a,f0.6))') 'QT: mode=',mode,' j=',j,' n=',n, &
                                         ' ggamma=',sngl(ggamma)
if(mode == 1) qt = (one - ggamma) * x(j) + ggamma * x(j+1)
if(mode == 2) qt = (one - ggamma) * x(max(1,j)) + ggamma * x(max(1,j)+1)

    if(prout) write(66,'(a,i0,3(a,f0.5))') 'Quantile index:   j=',j,'   qt=',sngl(qt), &
                          '  xL=',sngl(x(max(1,j))),'  xR=',sngl(x(max(1,j))+1)
if(abs(sigma) < eps1min) then
  uq = zero
else
  uq = SDQt(p, n, x0, sigma)
end if

!if(n == 500) write(66,*) '  Sub Quantile: qt= ',sngl(qt),'  u(qt) in % = ',sngl(uq/qt*100._rn), &
!              '  sigma=',sngl(sigma),'  p=',sngl(p),'  x0=',sngl(x0),' j=',int(j,2)

end subroutine quantile

!#######################################################################

real(rn) function SDQt(p, n, x0, sigma)

!  Quantile variance:
!  K.Y. Cheung & S.M.S. Lee: VARIANCE ESTIMATION FOR SAMPLE QUANTILES USING THE m OUT
!  OF n BOOTSTRAP. Ann. Inst. Statist. Math. Vol. 57, No. 2, 279-290 (2005)
!  Their Eq. (1.1)
!
! see also Kendall & stewart I, eq. 10.29

use Brandt,        only: sqstnr
use UR_params,     only: rn,one,two,pi

implicit none

real(rn),intent(in)    :: p          ! probability
integer(4),intent(in)  :: n
real(rn),intent(in)    :: x0         ! mean of normal distrib
real(rn),intent(in)    :: sigma      ! SD of normal distrib

real(rn)           :: x,f1,t
!-----------------------------------------------------------------------

t = SQSTNR(p)             ! standard normal deviate
x = x0 + t*sigma
f1 = exp(-0.5_rn*(x-x0)**two / sigma**two) / (sqrt(two*pi) * sigma)

 ! write(66,*) '  SDQt:  t=',sngl(t),'  p=',sngl(p),'  f1*sigma=',sngl(f1*sigma),' f1=',sngl(f1)

SDQt = sqrt( p*(one-p) / (real(n,rn) * f1**two) )

end function SDQt

!#######################################################################


end module MCSr
