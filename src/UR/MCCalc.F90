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
module MCC
    use UR_types
    use UR_params, only: EPS1MIN, ZERO, ONE, TWO
    !   contains:
    ! MCCalc
    ! Run_MCstart

contains

!################################################################################

    subroutine MCCalc()

        ! main routine for running a complete Monte Carlo simulation, called by Run_MCstart

        !     Copyright (C) 2014-2025  Günter Kanisch

        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        use plplot, only: plsstrm

        use gtk,                    only: gtk_widget_set_sensitive, gtk_progress_bar_set_fraction, &
                                          GTK_STATE_FLAG_NORMAL
        use UR_gtk_globals,         only: plot_setintern,plinit_done,item_setintern

        use ur_general_globals,     only: fname,frmtres, Gum_restricted, MCsim_on, &
                                          batf_mc,gtk_strm,MCsim_localOff, MC_root_from_line
        use UR_Gleich_globals,      only: ifehl,kbrutto_double,kEGr,kgspk1,klinf,knumEGr,nab, &
                                          ncov,ngrs,nvar,rnetmodi,MEsswert,MesswertSV,kpoint,StdUncSV, &
                                          StdUnc,covarval,ivtl,Symbole,kbrutto,SymboleG,kbrutto_gl, &
                                          CovarvalSV,RSeite,kgspk1,iptr_cnt,kbgv_binom,itm_binom, &
                                          ip_binom,ilam_binom,ksumeval,use_bipoi,Nbin0_MV,coverf, &
                                          einheit
        use UR_Linft,               only: fpa,d0zrateSV,sd0zrateSV,FitCalCurve,FitDecay,ifit,ifitSV,k_rbl, &
                                          kal_Polgrad,klincall,numd,kfitp,fpaSV,sfpa, &
                                          sd0zrateSV,a_kalibSV,a_kalib,fitmeth,kPMLE,  &
                                          mfrbg,covar_kalib,covar_kalibSV,Wtls_wild,d0zrate, &
                                          sd0zrate,fixedrateMC,ykalib,ykalibSV,nkalpts,SumEval_fit, &
                                          nchannels
        use UR_Gspk1Fit,            only: Gamspk1_Fit,GNetRate,GNetRateSV,SDGNetRate,SDGNetRateSV,RateCB, &
                                          RateBG,SDRateBG,Effi,pgamm,fatt,fcoinsu
        use UR_DLIM,                only: RblTot,alpha,beta,fconst,flinear,GamDist_Zr,kalpha,kbeta, &
                                          iteration_on,W1minusG,GamDistAdd,uflinear, &
                                          var_brutto_auto,ffx,RD
        use UR_MCC
        use fparser,                only: evalf
        use Rout,                   only: WDPutEntryInt,pending_events,WDPutEntryInt,  &
                                          WDPutLabelColorF,WDPutEntryDouble,WDGetCheckButton, &
                                          WDPutLabelString
        use Top,                    only: WrStatusbar,IntModA1,RealModA2
        use top,                    only: idpt
        use Rw1,                    only: covppcalc
        use Rw2,                    only: rnetval
        use UWB,                    only: Resulta, RbtCalc
        use Usub3,                  only: FindMessk
        use Num1,                   only: Xfit, Quick_sort_r, median
        use KLF,                    only: fkalib,sd_y0,CalibInter
        use RND,                    only: Rndu,rgamma,random_bipo2,scan_bipoi2,rnorm, &
                                          UR_random_seed,random_t
        use PLsubs
        use LF1,                    only: Linf,linfout
        use Brandt,                 only: mean,sd

        use RdSubs,                 only: rmcformF
        use UR_MCSR
        use MCSr,                   only: MCsingRun, SDQt
        use CHF,                    only: FindlocT, testSymbol

        use UR_plotp
        use UR_interfaces,          only: plot3fig
        use color_theme
        use translation_module,     only: T => get_translation
        use DECH,                   only: Decaysub1
        use UR_DecChain,            only: DChain,nsubdec,AdestMC,uAdestMC,DCpar

        use file_io,                only: logger

        implicit none

        integer              :: i,k,kk,kqtypDL,ksv,mode
        integer              :: kamin,kamax,kkk,jj,imct,knd
        real(rn)             :: x1,x2,xacc,brentx,rts
        real(rn)             :: xmit1_anf,start0,stop0
        real(rn)             :: eps,ueps,alpha_eps,beta_eps,sdDT,sumP,mean1,sd1
        real(rn)             :: dm1,dm2,amin,amax, xxn,xxnq , sumabwx,sumabwn ! ,hg(3)
        real(rn)             :: parr(7),xdiff(50)
        character(len=20)    :: cct
        character(len=10)    :: itmeth
        logical              :: first                      ! (local) use_brent: replaced by use_toms748 25.10.2025 GK
        real(rn),allocatable :: fgr(:)
        character(len=512)   :: log_str

        !-------------------------------------------------------------------

        !  iptr_cnt:  points from a counts-variable to the associated count rate variable
        !  iptr_time: points to the counts-variable associated with the counting time

        !------------------------------------------------------------------
        if(.not.allocated(mcafull)) then
            allocate(mcafull(3,mcmax),mcafull3(mcmax),mcafull2(mcmax))
        end if
        if(.not.allocated(arraymc)) allocate(arraymc(mc2max,3))
        if(.not.allocated(xplt)) allocate(xplt(npltmax,3))
        if(.not.allocated(yplt)) allocate(yplt(npltmax,3))
        mcafull = ZERO
        mcafull2 = ZERO
        mcafull3 = ZERO

        xplt = ZERO
        yplt = ZERO
        test_mg = .false.

        call plsstrm(gtk_strm)

        MC_root_from_line = .false.                ! 24.10.2025 GK
        do i=1,ngrs+ncov+numd
            if(abs(Messwert(i)-MesswertSV(i)) > EPS1MIN) then
                write(log_str,'(*(g0))') 'Test: i=',int(i,2),' ',Symbole(i)%s, sngl(Messwert(i)), sngl(MesswertSV(i))
                call logger(63, log_str)
            end if
        end do

        w_gamdis = .false.
        eps_gamdis = .false.
        do i=1,ngrs
            !WRITE(28,'(a,i3,a,a,a,es12.5,a,es12.5,a,i3)') 'MHSetup:  i=',i,'  ',Symbole(i)(1:12), &
            !    ' MesswertSV=',sngl(MesswertSV(i)),'  StdUncSV=',sngl(StdUncSV(i))   ! ,  ' kpt_mw=',kpt_mw(i)

            if(abs(Flinear - MesswertSV(i))/Flinear < 1.E-4_rn) then
                if(ivtl(i) == 6) w_gamdis = .true.
                uFlinear = StdUncSV(i)
            else
                if(ivtl(i) == 6) then
                    dm1 = ResultA(kEGr)
                    Messwert(i) = Messwert(i) * TWO
                    dm2 = ResultA(kEGr)
                    Messwert(i) = Messwert(i) / TWO
                    if(abs(dm2/dm1 - 0.5_rn) < 0.00001_rn) then
                        eps_gamdis = .true.
                        epsgam = Messwert(i)
                        uepsgam = StdUnc(i)
                        write(log_str,'(*(g0))') 'epsgam=',sngl(epsgam),'  uepsgam=',sngl(uepsgam)
                        call logger(63, log_str)
                        exit
                    end if
                end if
            end if
        end do

        call plsstrm(gtk_strm)
        Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)

        call logger(63, 'File:  '//trim(fname) //'  *********************************')
        call logger(63,' ')
        call logger(63, 'Begin of the MC simulation')
        call logger(63,' ')
        call logger(63, 'kqtyp:  three cases: 1: output quantity;  2: decision threshold; 3: detection limit ')
        call logger(63,' ')

        ifehl = 0
        ! Initialise the random generators by the time:

        call date_and_time(values=zt1)                       !values=[year, month, day, gmt_min, hr,min,sec,msec]
        idum = zt1(7)*60 + zt1(6)
        ! idum = 556842   ! activate this value for reproducible MC
        call UR_random_seed(idum)

        if(kcrun == 0 .or. imcmax == 0) then
            return
        end if

        IF(FitDecay) ifitSV = ifit
        ! IF(kPMLE == 1) ifit(mfrbg) = 2
        IF(kPMLE == 1) ifit(mfrbg) = 3      ! 14.6.2024
        if(FitDecay) then
            write(log_str,*) 'MCC: ifit=',int(ifit,2)
            call logger(63, log_str)
        end if

! *ORG arrays introduced, because *SV arrays/values will be modified in the routine ModVar
        if(allocated(MesswertOrg)) deallocate(MesswertOrg,StdUncORG)
        allocate(MesswertOrg(ngrs+ncov+numd),StdUncORG(ngrs+ncov+numd))
        MesswertORG(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)
        StdUncORG(1:ngrs+ncov+numd)   = StdUnc(1:ngrs+ncov+numd)
        if(allocated(Messwertkq)) deallocate(Messwertkq,StdUnckq,Messwert_eg)
        allocate(Messwertkq(ngrs+ncov+numd),StdUnckq(ngrs+ncov+numd),Messwert_eg(ngrs+ncov+numd))
        Messwertkq = ZERO; StdUnckq = ZERO; Messwert_eg = ZERO

        MCsim_on = .TRUE.
        Rnetmodi = .FALSE.
        MCsim_localOff = .false.    ! 18.1.2025  GK

        use_shmima = .FALSE.
        shmin = ZERO
        shmax = ZERO

        nval = 0
        imc = 0
        if(allocated(mwnet)) deallocate(mwnet,mwnetmit,mwnetmitq)
        if(allocated(xsdnet)) deallocate(xsdnet)
        allocate(mwnet(numd),xsdnet(numd),mwnetmit(numd),mwnetmitq(numd))

        mwnet = ZERO; xsdnet = ZERO; mwnet = ZERO; mwnetmit = ZERO; mwnetmitq = ZERO

        klu = klinf
        IF(Gamspk1_Fit) klu = kgspk1
        IF(kfitp(1) > 0) klu = kfitp(1) + kEGr - 1
        if(Gum_restricted) klu = kEGr

        nvar = kbrutto(kEGr)
        if(SumEval_fit) klu = ksumeval
        if(SumEval_fit) nvar = ksumeval

        IF(nvar > 0 .and. .not.Gum_restricted) THEN
            nvarSV = nvar
            xwert = Messwert(nvar)
            sdxwert = StdUnc(nvar)
            call RbtCalc(RblTot)
            rbltotSV(kEGr) = RblTot(kEGr)
            write(log_str,'(a,i3,3(a,es12.5),a,f5.2,a,L1)') 'nvar=',nvar,'  Meas.value=',Messwert(nvar),'  StdUnc=',StdUnc(nvar), &
                ' rbltot(kEGr)=',rbltot(kEGr),' GamDistAdd=',GamDistAdd, '  GamDist_ZR=',GamDist_ZR
            call logger(63, log_str)
        end if
        if(kbgv_binom > 0) then
            write(log_str,'(4(a,L1))') 'kbgv_binom=',kbgv_binom,' itm_binom=',itm_binom, &
                                ' ip_binom=',ip_binom,' ilam_binom=',ilam_binom
            call logger(63, log_str)
        end if

        IF(FitDecay) THEN
            if(.not. allocated(netfit)) then; allocate(netfit(numd)); netfit = ZERO; end if

            if(allocated(d0zrateSicher)) deallocate(d0zrateSicher,sd0zrateSicher,fixedRateMC)
            allocate(d0zrateSicher(numd),sd0zrateSicher(numd),fixedRateMC(numd))
            if(allocated(d0zrateSV))  deallocate(d0zrateSV,sd0zrateSV)
            allocate(d0zrateSV(numd),sd0zrateSV(numd))

            fpaSV(1:ma) = fpa(1:ma)
            xfpa(1:ma) = fpaSV(1:ma)
            xsfpa(1:ma) = sfpa(1:ma)
            do i=1,ma
                if(kfitp(1) > 0 .and. knumEGr > 1) then
                    MEsswertSV(kfitp(1)+i-1) = xfpa(i)
                end if
            end do
            d0zrateSV(1:numd) = d0zrate(1:numd)
            sd0zrateSV(1:numd) = sd0zrate(1:numd)
            d0zrateSicher(1:numd) = d0zrateSV(1:numd)
            sd0zrateSicher(1:numd) = sd0zrateSV(1:numd)
            fixedRateMC(1:numd) = ZERO
        end if
        IF(Gamspk1_Fit) THEN
            do i=1,1
                fpaSV(i) = fpa(i)
                xfpa(i) = fpaSV(i)
                xsfpa(i) = sfpa(i)
            end do
            if(.not.allocated(GNetRateSV)) then
                allocate(GNetRateSV(numd/5),SDGNetRateSV(numd/5),RateCBSV(numd/5),RateBGSV(numd/5),effiSV(numd/5), &
                    pgammSV(numd/5),fattSV(numd/5),fcoinsuSV(numd/5))
                if(.not.allocated(SDRateBGSV)) allocate(SDRateBGSV(numd/5))
                allocate(GNetRateSicher(numd/5),SDGNetRateSicher(numd/5))
            end if
            GNetRateSV(1:numd/5) = GNetRate(1:numd/5)
            SDGNetRateSV(1:numd/5) = SDGNetRate(1:numd/5)
            GNetRateSicher(1:numd/5) = GNetRateSV(1:numd/5)
            SDGNetRateSicher(1:numd/5) = SDGNetRateSV(1:numd/5)
            RateCBSV(1:numd/5) = RateCB(1:numd/5)
            RateBGSV(1:numd/5) = RateBG(1:numd/5)
            SDRateBGSV(1:numd/5) = SDRateBG(1:numd/5)
            effiSV(1:numd/5)    = effi(1:numd/5)
            pgammSV(1:numd/5)   = pgamm(1:numd/5)
            fattSV(1:numd/5)    = fatt(1:numd/5)
            fcoinsuSV(1:numd/5) = fcoinsu(1:numd/5)
        end if
        rblindnet = ZERO
        IF(k_rbl > 0) then
            rblindnet = MesswertSV(kpoint(k_rbl))
        end if
        write(log_str,'(a,i0,a,es12.5)') ' k_rbl=',k_rbl,'  rblindnet=',rblindnet
        call logger(63, log_str)
        if(FitCalCurve) then
            if(.not.allocated(ykalibSV)) allocate(ykalibSV(nkalpts))
            if(.not.allocated(a_kalibSV)) allocate(a_kalibSV(kal_Polgrad+1))
            if(.not.allocated(covar_kalibSV)) allocate(covar_kalibSV(kal_Polgrad+1,kal_Polgrad+1))
            ykalibSV(1:nkalpts) = ykalib(1:nkalpts)
            a_kalibSV(1:kal_Polgrad+1) = a_kalib(1:kal_Polgrad+1)
            covar_kalibSV(1:kal_Polgrad+1,1:kal_Polgrad+1) = covar_kalib(1:kal_Polgrad+1,1:kal_Polgrad+1)
        end if

        if(FitDecay) then
            write(log_str,'(a,3es13.5)') 'MCCALC: fit parameters xpa=',(xfpa(i),i=1,3)
            call logger(63, log_str)
        end if

!  Save the actual measurement values (Messwert) and their standard uncertainties into
!  the associated *SV arrays:
        !!!! call logger(63, ' ')

        if(allocated(Messwertw)) deallocate(Messwertw)
        allocate(Messwertw(ngrs+ncov+numd))
        Messwertw = ZERO

        if(allocated(relSDSV)) deallocate(relSDSV)
        allocate(relSDSV(ngrs+ncov+numd))
        MesswertSV(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)
        StdUncSV(1:ngrs+ncov+numd)   = StdUnc(1:ngrs+ncov+numd)
        relSdSv(1:ngrs+ncov+numd) = ZERO

        ! 2.6.2024:         ! 30.5.2025
        if(allocated(c_mars)) deallocate(c_mars,d_mars)
        allocate(c_mars(ngrs+nchannels+2*numd),d_mars(ngrs+nchannels+2*numd))
        if(allocated(a_rg)) deallocate(a_rg,p_rg,c_rg,uf_rg,vr_rg,d_rg)
        allocate(a_rg(ngrs+nchannels+2*numd),p_rg(ngrs+nchannels+2*numd),c_rg(ngrs+nchannels+2*numd))
        allocate(uf_rg(ngrs+nchannels+2*numd),vr_rg(ngrs+nchannels+2*numd),d_rg(ngrs+nchannels+2*numd))

        do i=1,ngrs+ncov+numd
            if(abs(Messwert(i)) > EPS1MIN) relSDSV(i)   =  stdunc(i)/Messwert(i)
            write(log_str,'(a, T30,a,i3,a,es15.8,a,es15.8)') Symbole(i)%s, 'MesswertSV(',i,')=',MesswertSV(i),'  StdUncSV=',StdUncSV(i)
            call logger(63, log_str)
        end do

! For tests with a Gamma-Distr.:
        IF(GamDist_ZR) THEN
            call RbtCalc(RblTot)
            write(log_str,'(a,es12.5)') ' RblTot for GamDist: ',rbltot(kEGr)
            call logger(63, log_str)
        end if

        write(log_str,'(a,i0,a)') 'MCCALC: ncov=',int(ncov,2),' (number of covariance pairs)'
        call logger(63, log_str)
        do k=1,ncov
            covarvalSV(k) = covarval(k)
            write(log_str,'(a,i3,a,es15.8)') 'MCCALC:  covarval(',k,')=',sngl(covarval(k))
            call logger(63, log_str)
        end do

        wlognorm = .false.
        do i=1,ngrs
            if(ivtl(i) == 5) then
                urel2 = (stdunc(i)/messwert(i))**TWO
                sigmaLN = sqrt(log(urel2) + ONE)
                mueLN = log(Messwert(i)) - sigmaLN**TWO / TWO
                DTmultLN = exp(sigmaLN*(sigmaLN + kbeta))
                wlognorm = .true.
                exit
            end if
        end do

!------------------------------------------------------
! Preparations for the application of covariances:
        call MCPrepCovars()
        ! write(0,*) 'Start MCCalc:  after MCPrepCovars: plinit_done=',plinit_done
!----------------------------------------------------------------------------------------

        call WDPutLabelString('TRLBUnit22', Einheit(1)%s)
        call WDPutLabelString('TRLBUnit21', Einheit(1)%s)

        call WDPutLabelString('TRLBUnit9', Einheit(1)%s)
        call WDPutLabelString('TRLBUnit10', Einheit(1)%s)
        call WDPutLabelString('TRLBUnit11', Einheit(1)%s)
        call WDPutLabelString('TRLBUnit12', Einheit(1)%s)
        call WDPutLabelString('TRLBUnit13', Einheit(1)%s)
        call WDPutLabelString('TRLBUnit14', Einheit(1)%s)

        call WrStatusBar(4, T("Calculating") // "....")
        call pending_events
        imc10 = imcmax/15

        iteration_on = .FALSE.
        mcasum3 = 0
        mcasum2 = 0

        CALL CPU_TIME(start0)
        ! Test gamma- and t-distributed random values (de-activate GOTO 10):
        GOTO 10

        allocate(fgr(100000))
        s_rt = ZERO  ! for random_t
        c_rt = ZERO
        a_rt = ZERO
        f_rt = ZERO
        g_rt = ZERO


        CALL CPU_TIME(start)
        gda_SV = GamDistAdd
        write(log_str,*) 'GamDistAdd=',sngl(GamDistAdd)
        call logger(63, log_str)
        do jj=1,2
            do k=1,30
                if(jj == 1)  xvor = 0.25_rn * real(k-1,rn)  ! + one
                if(jj == 2)  xvor = 70.0_rn * real(k-1,rn)  ! + one
                !if(xvor < one) rnnd = random_gamma2(k,xvor, one, .true.)
                !if(xvor >= one) rnnd = Ran_Gamma8(k, xvor,.TRUE.)
                rnnd = rgamma(k, xvor,.TRUE.)
            end do
            write(log_str,*) 'Ran_init done'
            call logger(63, log_str)
            sumabwx = ZERO
            sumabwn = ZERO
            do k=2,30
                xxx = ZERO
                xxq = ZERO
                xxn = ZERO
                xxnq = ZERO
                if(jj == 1) xvor = 0.25_rn * real(k-1,rn)  ! + one
                if(jj == 2) xvor = 70._rn * real(k-1,rn)  ! + one
                do i=1,imcmax
                    xwt = rgamma(k, xvor,.FALSE.)
                    xxx = xxx + xwt
                    xxq = xxq + xwt**TWO

                    xwt = xvor+GamDistAdd + rnorm()*sqrt(xvor+GamDistAdd)
                    xxn = xxn + xwt
                    xxnq = xxnq + xwt**TWO
                end do
                xxx = xxx / real(imcmax,rn)
                xsdv = SQRT( (xxq - real(imcmax,rn)*xxx**TWO) / real(imcmax-1,rn) )
                write(log_str,*) 'Test random Gamma Marsaglia  for x=',sngl(xvor),' : value=',sngl(xxx),'  Var=',sngl(xsdv**TWO),'  N=',imcmax
                call logger(63, log_str)
                sumabwx = sumabwx + abs(xsdv**TWO - (xvor + GamDistAdd))/29._rn

                xxn = xxn / real(imcmax,rn)
                xsdv = SQRT( (xxnq - real(imcmax,rn)*xxn**TWO) / real(imcmax-1,rn) )
                write(log_str,*) 'Test random rnorm with GDA for x  =',sngl(xvor),' : value=',sngl(xxn),'  Var=',sngl(xsdv**TWO),'  N=',imcmax
                call logger(63, log_str)
                sumabwn = sumabwn + abs(xsdv**TWO - (xvor + GamDistAdd))/29._rn

            end do
            write(log_str,*) 'Marsaglia: sumabwx=',sngl(sumabwx),'    normal: sumabwn=',sngl(sumabwn)
            call logger(63, log_str)
            imct = 100000
            parr = [0.7_rn,0.9_rn, 0.95_rn, 0.975_rn,0.990_rn,0.995_rn, 0.999_rn]

            if(jj == 2) cycle
            first = .true.
            do k=2,30,2
                do i=1,imct
                    if(i == 1) fgr(i) = random_t(1,k,.true.)
                    fgr(i) = random_t(1,k,.false.)
                end do
                write(log_str,*) 'mean(t)=',sngl(mean(fgr)),'  sd(t)=',sngl(sd(fgr))
                call logger(63, log_str)
                call Quick_Sort_r(fgr(1:imct))
                do i=1,7
                    dummy = quantileM(parr(i),fgr(1:imct),imct)
                    write(log_str,'(a,i2,a,f6.4,a,f11.6)') 'DF=',k,' P=',parr(i),'  q=',dummy
                    call logger(63, log_str)
                end do

            end do
        end do

        CALL CPU_TIME(finish)
        write(log_str,*) 'CPU-time : ',sngl(finish-start)
        call logger(63, log_str)

        CALL CPU_TIME(start)
        gda_SV = GamDistAdd
        GamDistAdd = ZERO
        write(log_str,*) 'GamDistAdd=',sngl(GamDistAdd)
        call logger(63, log_str)

        eps = ONE/34.5_rn
        ueps = eps*1.2_rn

        alpha_eps = (eps/ueps)**TWO
        beta_eps = eps/ueps**TWO
        rnnd = rgamma(1, alpha_eps,.TRUE.) / beta_eps

        xxx = ZERO
        xxq = ZERO
        do i=1,imcmax
            xwt = rgamma(1, alpha_eps,.FALSE.) / beta_eps
            xxx = xxx + xwt
            xxq = xxq + xwt**TWO
        end do
        xxx = xxx / real(imcmax,rn)
        xsdv = SQRT( (xxq - real(imcmax,rn)*xxx**TWO) / real(imcmax-1,rn) )
        write(log_str,*) 'Test random Gamma Marsaglia for eps,ueps=',sngl(eps),sngl(ueps), &
            ' : value=',sngl(xxx),'  SD=',sngl(xsdv),'  N=',imcmax
        call logger(63, log_str)
        CALL CPU_TIME(finish)
        write(log_str,*) 'CPU-time : ',sngl(finish-start)
        call logger(63, log_str)
        GamDistAdd = gda_SV
        deallocate(fgr)


10      CONTINUE

        kbd = 0
        IF(kbrutto_double > 0 .AND. .not.FitDecay .and. .not.Gamspk1_Fit .and. .not.SumEval_fit) THEN
            ch1 = Rseite(kbrutto(kEGr))%s
            do kk=nab+1,ngrs
                IF(testSymbol(ch1,symboleG(kk)%s) .and. StdUnc(kk) > ZERO) THEN
                    kbd = kk
                    EXIT
                end if
            end do
        end if

        !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! kqtyp = 1: mean and standard uncertainty, and both quantiles of the output quantity y
        !         2: decision threshold derived as upper quantile of the distribution of y_tilde=0
        !         3: detection limit derived from a lower quantile

        wait = .FALSE.
        xDT = ZERO
        rXDT = ZERO

        if(allocated(xmitsgl)) deallocate(xmitsgl);  allocate(xmitsgl(ngrs+ncov+numd)); xmitsgl = ZERO
        if(allocated(xsdvsgl)) deallocate(xsdvsgl); allocate(xsdvsgl(ngrs+ncov+numd)); xsdvsgl = ZERO
        if(allocated(xmitsglq)) deallocate(xmitsglq);  allocate(xmitsglq(ngrs+ncov+numd)); xmitsglq = ZERO
        if(allocated(mwnetvgl)) deallocate(mwnetvgl);  allocate(mwnetvgl(ngrs+ncov+numd)); mwnetvgl = ZERO
        if(allocated(umwnetvgl)) deallocate(umwnetvgl);  allocate(umwnetvgl(ngrs+ncov+numd)); umwnetvgl = ZERO

        if(allocated(xzmit)) deallocate(xzmit); allocate(xzmit(kcrun)); xzmit = ZERO
        if(allocated(rxzmit)) deallocate(rxzmit); allocate(rxzmit(kcrun)); rxzmit = ZERO
        if(allocated(xzsdv)) deallocate(xzsdv); allocate(xzsdv(kcrun)); xzsdv = ZERO
        if(allocated(rxzsdv)) deallocate(rxzsdv); allocate(rxzsdv(kcrun)); rxzsdv = ZERO
        if(allocated(xzLQ)) deallocate(xzLQ); allocate(xzLQ(kcrun)); xzLQ = ZERO
        if(allocated(rxzLQ)) deallocate(rxzLQ); allocate(rxzLQ(kcrun)); rxzLQ = ZERO
        if(allocated(xzUQ)) deallocate(xzUQ); allocate(xzUQ(kcrun)); xzUQ = ZERO
        if(allocated(rxzUQ)) deallocate(rxzUQ); allocate(rxzUQ(kcrun)); rxzUQ = ZERO

        if(allocated(xzmitPE)) deallocate(xzmitPE); allocate(xzmitPE(kcrun)); xzmitPE = ZERO
        if(allocated(rxzmitPE)) deallocate(rxzmitPE); allocate(rxzmitPE(kcrun)); rxzmitPe = ZERO
        if(allocated(xzsdvPE)) deallocate(xzsdvPE); allocate(xzsdvPE(kcrun)); xzsdvPE = ZERO
        if(allocated(rxzsdvPE)) deallocate(rxzsdvPE); allocate(rxzsdvPE(kcrun)); rxzsdvPE = ZERO

        if(allocated(sx)) deallocate(sx); allocate(sx(kcrun)); sx = ZERO
        if(allocated(xzDL)) deallocate(xzDL); allocate(xzDL(kcrun)); xzDL = ZERO
        if(allocated(xzLQbci)) deallocate(xzLQbci); allocate(xzLQbci(kcrun)); xzLQbci = ZERO
        if(allocated(xzUQbci)) deallocate(xzUQbci); allocate(xzUQbci(kcrun)); xzUQbci = ZERO

        if(allocated(xzLenBci)) deallocate(xzLenBci); allocate(xzLenBci(kcrun)); xzLenBci = ZERO
        if(allocated(uxxDT)) deallocate(uxxDT); allocate(uxxDT(kcrun)); uxxDT = ZERO
        if(allocated(uxxDL)) deallocate(uxxDL); allocate(uxxDL(kcrun)); uxxDL = ZERO

        if(allocated(xxDT)) deallocate(xxDT); allocate(xxDT(kcrun)); xxDT = ZERO

        plot_setintern = .true.
        item_setintern = .true.
        plinit_done = .true.

        ! write(0,*) 'Start MCCalc:  do kqtypDL: plinit_done=',plinit_done

        do kqtypDL=1,3
            kqtyp = kqtypDL

            WTLS_wild = .false.
            MCsim_done = .true.
            call pending_events

            if(kqtyp == 3 .and. abs(xxmit1) > EPS1MIN) then
                if(xDT/abs(xxmit1)   < 1.E-7_rn) exit
            end if

            IF(kqtyp > 1 .AND. nvar == 0 .AND. .not.FitDecay .AND. .not.Gamspk1_Fit &
               .and. .not.DChain .and. .not.SumEval_fit) EXIT       ! 27.4.2025

            if(kqtyp > 1 .and. (Gum_restricted .or.                 &
                ( (kbrutto_gl(kEGr) == 0 .and. .not.var_brutto_auto) .and. .not.FitDecay .AND. &
                                             .not.Gamspk1_Fit .and. .not. DChain       &   ! 27.4.2025
                                             .and. .not.SumEval_fit))) then
                call WDPutLabelColorF('TRentryMCvalPE',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))    ! 'black')
                call WDPutLabelColorF('TRentryMCvalUPE',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                call WDPutLabelColorF('TRentryMCValue',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                call WDPutLabelColorF('TRentryMCunc',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                call WDPutLabelColorF('TRentryMCuncrel',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                call WDPutLabelColorF('TRentryMClq',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                call WDPutLabelColorF('TRentryMCuq',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                call logger(63, ' ')
                call logger(63, 'Warning: no MC simulation of DT and DL, because the gross uncertainty formula not defined!' )
                cycle ! exit
            end if

            ! for testing purposes:
            ! IF(kqtyp > 2) EXIT
            !!!!        if(batestMC .and. kqtyp > 2) exit

            kqtypx = kqtyp

            call logger(63, ' ')
            if(FitDecay) then
                write(log_str,*) '#################################   MC-Simulation:  kqtyp=', &
                                  int(kqtyp,2),'  ',trim(fitmeth),'  ', &
                                  trim(Symbole(kEGr)%s), '  ###############################'
                call logger(63, log_str)
            end if
            if(.not.FitDecay) then
                write(log_str,*) '#################################   MC-Simulationen:  kqtyp=', &
                                 int(kqtyp,2),'  ', &
                                 trim(Symbole(kEGr)%s), '  ###############################'
                call logger(63, log_str)
            end if
            call logger(63, ' ')

            if(kqtyp == 1) then                ! 27.4.2025
              if(DChain) then            ! 18.1.2025 GK
                call DChain_Adjust_SD()
                ! this part must occur AFTER call ModVar ! ????
                do knd=nsubdec,1,-1
                  call decaysub1(knd,AdestMC(knd),uAdestMC(knd))
                  write(log_str,*) 'MCcalc: knd=',int(knd,2),'  AdestMC(knd)=',sngl(AdestMC(knd)), &
                                                  '  uAdestMC(knd)=',sngl(uAdestMC(knd))
                  call logger(63, log_str)
                end do
                do knd=nsubdec,1,-1
                  write(log_str,*) 'knd=',int(knd,2),' derv: ',sngl(DCpar(knd)%derv(1:knd))
                  call logger(63, log_str)
                end do
              end if
            end if


            xzmit  = ZERO
            rxzmit = ZERO
            xzsdv  = ZERO
            rxzsdv = ZERO
            xzmitPE  = ZERO
            rxzmitPE = ZERO
            xzsdvPE  = ZERO
            rxzsdvPE = ZERO
            xzLQ   = ZERO
            rxzLQ  = ZERO
            xzUQ   = ZERO
            rxzUQ  = ZERO
            sx  = ONE
            RD = ZERO
            xDL = ZERO
            rXDL = ZERO

            xesdev1 = ZERO
            xesdev2 = ZERO
            xesdevq = ZERO
            xemit1 = ZERO
            xemitq = ZERO
            xemit2 = ZERO
            xemitq2 = ZERO
            ssxEG = ZERO
            knegative = 0

            do kr=1,kcrun

                mmkk = 0   ! Iteration counter

                call WDPutEntryInt('TRentryMCarun', ivalue=kr)
                call WDPutEntryInt('TRentryMCit', ivalue=1)
                call pending_events

                if(FitDecay) then
                    kmode = 2
                    call covppcalc(kmode)
                end if

                if(FitCalCurve) then
                    ykalib(1:nkalpts) = ykalibSV(1:nkalpts)
                    a_kalib(1:kal_Polgrad+1) = a_kalibSV(1:kal_Polgrad+1)
                    covar_kalib(1:kal_Polgrad+1,1:kal_Polgrad+1) = covar_kalibSV(1:kal_Polgrad+1,1:kal_Polgrad+1)
                end if

                select case (kqtyp)

                  case (1)
                    ! quantiles of output quantity:
                    UQprob = ONE - (ONE-W1minusG)/TWO
                    LQprob = (ONE-W1minusG)/TWO
                    ! for extremely small uncertainty:
                    ! urelx = max(1.E-5_rn, (StdUncSV(kEGr)/coverf)/ MesswertSV(kEGr))
                    call WDPutLabelColorF('TRentryMCvalPE',GTK_STATE_FLAG_NORMAL,'red')
                    call WDPutLabelColorF('TRentryMCvalUPE',GTK_STATE_FLAG_NORMAL,'red')
                    call WDPutLabelColorF('TRentryMCValue',GTK_STATE_FLAG_NORMAL,'red')
                    call WDPutLabelColorF('TRentryMCunc',GTK_STATE_FLAG_NORMAL,'red')
                    call WDPutLabelColorF('TRentryMCuncrel',GTK_STATE_FLAG_NORMAL,'red')
                    call WDPutLabelColorF('TRentryMClq',GTK_STATE_FLAG_NORMAL,'red')
                    call WDPutLabelColorF('TRentryMCuq',GTK_STATE_FLAG_NORMAL,'red')

                    if(kr == 1) then
                        WRITE(log_str,*) 'MC: kqtyp = 1:  Output Quantity: ',Symbole(kEgr)%s
                        call logger(63, log_str)
                    end if

                    IF(FitDecay ) THEN
                        write(log_str,*) ' fpa :',(sngl(fpa(i)),i=1,3)
                        call logger(63, log_str)
                        ! initiate the new calculation of the q matrices Lincov2:
                        klincall = 0
                        call Linf(r0dummy,sdr0dummy)
                        WRITE(log_str,*) '   just fitted parameters fpa: ',(sngl(fpa(i)),i=1,3),'    fpaSV: ', &
                                         (sngl(fpaSV(i)),i=1,3)
                        call logger(63, log_str)
                        write(log_str,*) '            r0dummy=',sngl(r0dummy),'  sdr0dummy=',sngl(sdr0dummy)
                        call logger(63, log_str)
                    end if

                    if(.not.DChain) then    ! <-- 13.1.2025 GK        27.4.2025
                        RD = RnetVal(MesswertSV(kEGr))
                        if(kr == 1) then
                            do i=1,ngrs+ncov+numd
                                if(ncov > 0 .and. i > ngrs .and. i <= ngrs+ncov) cycle
                                messwertkq(i) = MesswertSV(i)
                                StdUnckq(i) = StdUncSV(i)
                            end do
                        end if
                    end if
                    if(use_bipoi .and. test_mg) then
                        Nbin0_MV = RD * Messwert(itm_binom)
                        call scan_bipoi2(MesswertSV(ip_binom),Nbin0_MV,RblTot(kEGr),MesswertSV(itm_binom))
                    end if

                  case (2)
                    ! DT: upper quantile for output quantity=0, i.e. for y_tilde = 0:
                    call WDPutLabelColorF('TRentryMCvalPE',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                    call WDPutLabelColorF('TRentryMCvalUPE',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                    call WDPutLabelColorF('TRentryMCValue',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                    call WDPutLabelColorF('TRentryMCunc',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                    call WDPutLabelColorF('TRentryMCuncrel',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                    call WDPutLabelColorF('TRentryMClq',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))
                    call WDPutLabelColorF('TRentryMCuq',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))

                    if(.not.Gum_restricted) call WDPutLabelColorF('TRentryMCdt',GTK_STATE_FLAG_NORMAL,'red')

                    UQprob = (ONE - alpha)
                    RDlast = ZERO
                    xmit1last = ZERO
                    DTbracketed = .false.
                    RDmin = +1.E+30_rn
                    RDmax = -1.E+30_rn

                    IF(kbrutto(kEGr) > 0 .AND. kbrutto(kEGr) <= nab) iteration_on = .TRUE.
                    ! set RD (procedure dependent net count rate)to nearly 0:
                    RD = (1.E-11_rn - Fconst)/Flinear
                    if(DChain) ffx = 1.E-12_rn              ! 27.4.2025

                    ! calculate the assocoiated gross count rate (symbol number nvar) and its standard uncertainty:
                    ! This call also modifies StdUnc(nvar).
                    call ModVar(kqtyp, RD, ffx)

                    if(DChain) then            ! 18.1.2025 GK           ! 27.4.2025
                      ! this part must occur AFTER call ModVar !
                      do knd=nsubdec,1,-1
                          call decaysub1(knd,AdestMC(knd),uAdestMC(knd))
                          write(log_str,*) 'knd=',int(knd,2),'  AdestMC(knd)=',sngl(AdestMC(knd)), &
                                                          '  uAdestMC(knd)=',sngl(uAdestMC(knd))
                          call logger(63, log_str)
                      end do
                      do knd=nsubdec,1,-1
                          write(log_str,*) 'knd=',int(knd,2),' derv: ',sngl(DCpar(knd)%derv(1:knd))
                          call logger(63, log_str)
                      end do
                    end if


                    if(nvar > 0) then
                        dummy = ResultA(kEgr)
                        write(log_str,*) 'Resulta after Modvar=',sngl(dummy),' MW(nvar)=',sngl(Messwert(nvar))
                        call logger(63, log_str)

                        WRITE(log_str,'(3(a,es12.5),a,i3,a,L1,2(a,i1))') 'nvar-modification: Messwert(nvar)=',Messwert(nvar), &
                            '  StdUnc(nvar)=',StdUnc(nvar),'  RD=',sngl(RD), &
                            '  nvar=',int(nvar,2),' iteration_on=',iteration_on,' kqtyp=',kqtyp,'  kEGr=',kEGr
                        call logger(63, log_str)
                        if(iptr_cnt(nvar) > 0) then
                            write(log_str,*) 'counts(nvar)=',sngl(Messwert(iptr_cnt(nvar))), &
                                             ' countsSV(nvar)=',sngl(MesswertSV(iptr_cnt(nvar)))
                            call logger(63, log_str)
                        end if
                    end if

                    IF(FitDecay ) THEN
                        ! initiate the new calculation of the q matrices Lincov2:
                        klincall = 0
                        call Linf(r0dummy,sdr0dummy)
                        IF(ifehl == 1) then
                            write(log_str,*) 'MCcalc: Error in Linf!  kqtyp=',int(kqtyp,2),' run=',int(kr,2)
                            call logger(63, log_str)
                            goto 8900    !RETURN
                        end if

                        WRITE(log_str,'(a,3es13.5,a,3es13.5)') '   just fitted parameters fpa: ',(fpa(i),i=1,3),'    fpaSV: ',(fpaSV(i),i=1,3)
                        call logger(63, log_str)
                        WRITE(log_str,'(a,3es13.5)') '                         sfpa: ',(sfpa(i),i=1,3)
                        call logger(63, log_str)
                        WRITE(log_str,*) ' iteration_on=',iteration_on,' kqtyp=',int(kqtyp,2),'  r0dummy=',sngl(r0dummy), &
                            ' sdr0dummy=',sngl(sdr0dummy) ! ,'  StdUnc(klu)=',sngl(StdUnc(klu))
                        call logger(63, log_str)
                    end if
                    if(kr == 1) then
                        messwert_eg(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)
                    end if

                    ! Decision threshold iteration:   ..............................
                    !    The iteration's aims is to get the mean of this distribution close enough to zero.
                    ! First, perform a MC simulation the MC distribution resulting in the array "arraymc":
                    call MCsingRun()
                    if(ifehl == 1) then
                        write(log_str,*) 'MCcalc: Error in MCsingrun!  kqtyp=',int(kqtyp,2),' run=',int(kr,2)
                        call logger(63, log_str)
                        goto 8900   ! return
                    end if

                    xmit1_anf = xmit1
                    sd_dt = xsdv / sqrt(real(imctrue,rn))
                    help1 = abs(xmit1)/sd_dt
                    call Quick_Sort_r(arraymc(1:imctrue,kqtyp))

                    !--- Prevent from getting extreme "extrem values", by using quantiles instead
                    amin = quantileM(0.00005_rn,arraymc(1:imctrue,kqtyp),imctrue)
                    amax = quantileM(0.99995_rn,arraymc(1:imctrue,kqtyp),imctrue)

                    do i=1,imctrue
                        if(arraymc(i,kqtyp) >= amin) then
                            kamin = i
                            exit
                        end if
                    end do
                    do i=imctrue,1,-1
                        if(arraymc(i,kqtyp) <= amax) then
                            kamax = i
                            exit
                        end if
                    end do
                    kamin = 1
                    kamax = imctrue

                    arraymc(1:kamax-kamin+1, kqtyp) = arraymc(kamin:kamax,kqtyp)
                    imctrue = kamax - kamin + 1
                    !---

                    ! First value for the decision threshold:
                    DT_anf = quantileM(ONE-alpha,arraymc(1:imctrue,kqtyp),imctrue)

                    write(log_str,*) 'xmit1 Anf=',sngl(xmit1),'  SD=',sngl(xsdv),'  DT_anf=',sngl(DT_anf),' help1=',sngl(help1)
                    call logger(63, log_str)

                    ! criteria for stopping the iteration:
                    ! if( (help1 < 0.150 .and. trim(fitmeth) == 'PMLE') .or. help1 < 0.5_rn ) then
                    if( (help1 < 0.150 .and. trim(fitmeth) == 'PMLE') .or. help1 < 0.02_rn ) then            ! 20.102025 GK
                        ! the start value is good enough (its mean close to zero), no iteration of the mean necessary
                        mcafull2(1:mcmax) = mcafull2(1:mcmax) + mcafull(kqtyp,1:mcmax)
                        xxDT(kr) = DT_anf
                        goto 146
                    end if

                    mode = 3            ! iterative Monte Carlo DT calculation
                    ! define bracketing values x1, x2
                    x2 = 0.012_rn*DT_anf

                    if(xmit1 > ZERO) then
                        x1 = -xmit1*TWO * 2.2_rn   ! 1.5_rn   ! * two * zero
                        x2 = xmit1*TWO * 2.2_rn    ! 1.5_rn    ! * 0.55_rn
                    else
                        x1 = xmit1*TWO  * 2.2_rn   ! 1.5_rn   ! * 0.75_rn
                        x2 = -xmit1*TWO * 2.2_rn    !1.5_rn   ! * 0.5_rn
                    end if

                    xacc = 1.0E-5_rn *abs(xmit1)
                    itmeth = 'brentx'
                    write(log_str,'(10(a,es11.4))') ' xacc=',xacc, &
                        '  help1=',help1,'  bias/DT_anf=',xmit1/DT_anf,' DT_anf=',DT_anf
                    call logger(63, log_str)

                    ! brentx performs the iteration using rootfindbs:
                    rts = brentx(x1,x2,xacc,ZERO,mode)

                    if(ifehl == 1) then

                        write(log_str,*) 'MCcalc: Error in brentx (DL)!  kqtyp=',int(kqtyp,2),' run=',int(kr,2)
                        call logger(63, log_str)
                        goto 8900   ! return
                    end if

                    ! derive decision threshold value as (1-alpha) quantile:
                    call Quick_Sort_r(arraymc(1:imctrue,kqtyp))
                    xxDT(kr) = quantileM(ONE-alpha,arraymc(1:imctrue,kqtyp),imctrue)

                    if(batf_mc) write(168,*) 'DT('//trim(itmeth)//'): ',sngl(xxDT(kr)),'  DTanf=',sngl(DT_anf), &
                        '  xmit1_final=',sngl(xmit1min), &
                        ' xmit1_anf/xmit1_final=',sngl(abs(xmit1_anf/xmit1min)),'  ',trim(fname)

                    write(log_str,'(2(a,es11.4))') ' ratio ' //trim(itmeth)//' / DT('//trim(itmeth)//')= ',rts/xxDT(kr), &
                                          '     ratio xmit1_final/sd_DT=',xmit1min/sd_DT

                write(178,'(a,T65,2(es12.5,2x))') trim(fname), xxDT(kr),abs(xmit1min)/sd_DT

                    call logger(63, log_str)
                    write(log_str,'(10(a,es11.4))') 'DT('//trim(itmeth)//'): ',xxDT(kr),'  DTanf=',DT_anf, &
                        '  xmit1_anf=',xmit1_anf,'  xmit1_final=',xmit1min, &
                        ' xmit1_anf/xmit1_final=',abs(xmit1_anf/xmit1min)
                    call logger(63, log_str)
                    sdDT = sd(arraymc(1:imctrue,kqtyp))
                    write(log_str,*) 'SD of DT distribution=',sngl(sdDT),'  assoc. DT:',sngl(kalpha*sdDT)
                    call logger(63, log_str)
                    ! write(63,*)
                    ! write(63,*) 'mean(distrib)=',sngl(mean(arraymc(1:imctrue,kqtyp)))

                    mcafull2(1:mcmax) = mcafull2(1:mcmax) + mcafull(kqtyp,1:mcmax)
                    goto 146

                  case (3)
                    ! DL:  lower quantile for output quantity --> defines detection limit
                    call WDPutLabelColorF('TRentryMCdt',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))    ! 'black')

                    if(.not.Gum_restricted) call WDPutLabelColorF('TRentryMCdl',GTK_STATE_FLAG_NORMAL,'red')

                    write(cct,'(es11.4)') real(xxDT(kr),8)
                    read(cct,*,iostat=ios) dummy
                    if( (ios == 0 .and. abs(dummy) < EPS1MIN) .or. (ios /= 0) ) then
                        write(log_str,*) 'MCcalc: Error with xxDT(kr)!  kqtyp=',int(kqtyp,2),' run=',int(kr,2)
                        call logger(63, log_str)
                        goto 9000
                    end if

                    LQprob = beta
                    IF(kbrutto(kEGr) > 0 .AND. kbrutto(kEGr) <= nab) iteration_on = .TRUE.
                    IF(kbrutto(kEGr) > 0 .AND. kbrutto(kEGr) > nab) iteration_on = .TRUE.

                    dummy = xxDT(kR)*(kalpha+kbeta)/kalpha * 0.90 * 1.05_rn     ! DL (anf) as activity
                    dummy = dummy * TWO

                    ! Detection limit iteration:   ..............................
                    ! define bracketing values x1, x2:
                    x1 = xxDT(kr) * 0.9_rn
                    x2 = dummy
                    ! xacc = 5.e-7_rn * x1
                    mode = 2
                    ! if(use_brent) then

                    x2 = x2 * TWO
                    xacc = 1.5e-4_rn * x1

                    ! brentx performs the iteration using rootfindbs:
                    rts = brentx(x1,x2,xacc,xxDT(kr),mode)
                    xzDL(kr) = rts

                    if(ifehl == 1) then

                        write(log_str,*) 'MCcalc: Error in brentx (DL)!  kqtyp=',int(kqtyp,2),' run=',int(kr,2)
                        call logger(63, log_str)
                        goto 8900    ! return
                    end if
                    xzDL(kr) = mean(arraymc(1:imctrue,kqtyp))

                    ! Here, the result for the detection limit DL of run kr is found:
                    WRITE(log_str,*) 'Result for DL: ',sngl(xzDL(kr)),'  '//trim(itmeth)
                    call logger(63, log_str)
                    ! add the distribution of run kr to mcafull3:
                    mcafull3(1:mcmax) = mcafull3(1:mcmax) + mcafull(kqtyp,1:mcmax)
                    goto 20
                    ! ............................................................

                  case default
                end select

20              CONTINUE       ! Label for the end of the DL iteration (of run kr, for kqtpy=3)
                call pending_events

                if(kqtyp == 1) then
                    call MCsingRun()
                    if(ifehl == 1) then
                        write(log_str,*) 'MCcalc: Error in MCsingrun!  kqtyp=',int(kqtyp,2),' run=',int(kr,2)
                        call logger(63, log_str)
                        goto 8900   ! return
                    end if
                end if

146             continue

                ! write(63,*) ' before call MCDistrib:   kqtyp=',kqtyp,'  mca_min,mca_max=',sngl(mca_min(kqtyp)),sngl(mca_max(kqtyp))
                call MCDistrib(kr,imctrue,ZERO,zero)

                IF(kqtyp == 3) mcasum3 = mcasum3 + mcasum(3)
                if(kqtyp == 2) mcasum2 = mcasum2 + mcasum(2)
                xzmitPE(kr) = xmit1PE
                xzsdvPE(kr) = xsdvPE

                xzmit(kr) = xmit1
                xzsdv(kr) = xsdv
                xzLQ(kr)  = estLQ
                xzUQ(kr)  = estUQ

                if(kqtyp == 1) then
                    xzLQbci(kr) = estLQ_BCI2
                    xzUQbci(kr) = estUQ_BCI2
                    xzLenBCI(kr) = estUQ_BCI2 - estLQ_BCI2
                end if

                IF(kqtyp > 1) THEN
                    IF(nvar > 0) then
                        write(log_str,*) '                 Mean for nvar: ',sngl(xmitsgl(nvar)),  &
                            '  Mean für var-3: ',sngl(xmitsgl(3))
                        call logger(63, log_str)
                    end if
                    if(kqtyp == 3 .and. kr < 4) then
                        do i=1,ngrs+ncov+numd
                            write(log_str,'(i3,2x,a,2(a,es15.8))') i,Symbole(i)%s,'  mean=',xmitsgl(i),'  SD=',xsdvsgl(i)
                            call logger(63, log_str)
                        end do
                    end if
                    IF(FitDecay .and. ifit(2) == 1) THEN
                        call logger(63, ' ')
                        write(log_str,*) '        fpa(2)                mean=',sngl(xemit2),'  SD=',sngl(xesdev22)
                        call logger(63, log_str)
                    end if
                end if

                select case (kqtyp)
                  case (1)
                    call WDPutEntryDouble('TRentryMCvalPE', xmit1PE, frmtres)
                    call WDPutEntryDouble('TRentryMCvalUPE', xsdvPE*coverf, frmtres)
                    call WDPutEntryDouble('TRentryMCValue', xmit1, frmtres)
                    call WDPutEntryDouble('TRentryMCunc', xsdv*coverf, frmtres)
                    call WDPutEntryDouble('TRentryMCuncrel', xsdv*coverf/xmit1*100._rn, frmtres)
                    if(.not.use_BCI) then
                        call WDPutEntryDouble('TRentryMClq', estLQ, frmtres)
                        call WDPutEntryDouble('TRentryMCuq', estUQ, frmtres)
                    else
                        !call WDPutEntryDouble('TRentryMClq', estLQ_BCI, frmtres)
                        !call WDPutEntryDouble('TRentryMCuq', estUQ_BCI, frmtres)
                        call WDPutEntryDouble('TRentryMClq', estLQ_BCI2, frmtres)
                        call WDPutEntryDouble('TRentryMCuq', estUQ_BCI2, frmtres)
                    end if
                    Title(1) = trim(Symbole(kEGr)%s) // ':  ' // T('Output quantity')

                    if(FitDecay) then

                        if(trim(fitmeth) == 'WTLS') fitmeth = 'WTLS'

                        Title(1) = trim(Title(1)) // ' (' // trim(fitmeth) // ')'

                    end if
                    VertLines(1) = estLQ
                    VertLines(2) = estUQ
                    VertLines(3) = xmit1
                  case (2)
                    call WDPutEntryDouble('TRentryMCdt', estUQ, frmtres)
                    Title(2) = trim(Symbole(kEGr)%s) // ':  ' // T('Decision threshold')
                    VertLines(4) = estUQ
                  case (3)
                    call WDPutEntryDouble('TRentryMCdl', xzDL(kr), frmtres)

                    Title(3) = trim(Symbole(kEGr)%s) // ':  ' // T('Detection limit')

                    VertLines(5) = estLQ
                    VertLines(6) = xmit1
                  case default
                end select
                call pending_events

                IF(kqtyp == 1) THEN
                    call Plotsteps(kqtyp,' ')
                    call pending_events
                    if(.true.) then
                        mean1 = ZERO
                        sd1 = ZERO
                        sumP = ZERO
                        sumP = sum(yplt(1:nval(1),1) )
                        mean1 = sum( xplt(1:nval(1),1) * yplt(1:nval(1),1) )
                        sd1 = sum( xplt(1:nval(1),1)**TWO * yplt(1:nval(1),1) )
                        mean1 = mean1/sumP
                        sd1 = sd1/sumP
                        sd1 = sqrt(sd1 - mean1**TWO)
                        write(log_str,'(3(a,es11.4),a,i4)')  'showHist2:  mqt=1:  mean=',mean1,  &
                            '  sd=',sd1,' ymax=',xplt(nval(1),1),'  nval=',nval(1)
                        call logger(63, log_str)
                    end if
                end if
                IF(kqtyp == 2) THEN
                    call Plotsteps(kqtyp,' ')
                    call pending_events
                end if
                IF(kqtyp == 3) THEN
                    call Plotsteps(kqtyp,' ')
                    call pending_events
                end if
                if(k_rbl > 0) Messwert(kpoint(k_rbl)) = MesswertSV(kpoint(k_rbl))

                !   IF(kqtyp == 3) EXIT   ! if testing the DL iteration: exit the iteration

                CALL CPU_TIME(finish)
                ! WRITE(63,*) 'CPU-time für Run ',kr,' : ',sngl(finish-start),' s'
                start = finish

            end do

            call pending_events

            select case (kqtyp)
              case (1)
                imctrue1 = imctrue
                call Xfit (xzmit, sx, kcrun, 0, xxmit1, sigmam, rxmit1)
                rxmit1 = rxmit1/xxmit1*100._rn
                IF(kcrun == 1) rxmit1 = -1.

                call Xfit (xzsdv, sx, kcrun, 0, xxsdv, sigmam, rxsdv)
                rxsdv = rxsdv/xxsdv*100._rn
                IF(kcrun == 1) rxsdv = -1.

                call Xfit (xzmitPE, sx, kcrun, 0, xxmit1PE, sigmam, rxmit1PE)

                rxmit1PE = abs(rxmit1PE/xxmit1PE)*100._rn
                IF(kcrun == 1) rxmit1PE = -1.

                call Xfit (xzsdvPE, sx, kcrun, 0, xxsdvPE, sigmam, rxsdvPE)
                rxsdvPE = abs(rxsdvPE/xxsdvPE)*100._rn
                IF(kcrun == 1) rxsdvPE = -1.

                call Xfit (xzLQ, sx, kcrun, 0, xLQ, sigmam, rxLQ)
                if(rxLQ > ZERO) rxLQ = rxLQ/xLQ*100._rn
                IF(kcrun == 1)  rxLQ = -1.

                call Xfit (xzUQ, sx, kcrun, 0, xUQ, sigmam, rxUQ)
                if(rxUQ > ZERO) rxUQ = rxUQ/xUQ*100._rn
                IF(kcrun == 1) rxUQ = -1.

                ! if(kcrun >= 10 .and. .true.) then
                !   do i=1,kcrun
                !     write(69,'(2es11.4)') xzLQbci(i),xzUQbci(i)
                !   end do
                ! end if

                call Xfit (xzLQbci, sx, kcrun, 0, est1LQ_BCI, sigmam, rx1LQbci)
                if(est1LQ_BCI > ZERO) rx1LQbci = rx1LQbci/est1LQ_BCI*100._rn
                IF(kcrun == 1) rx1LQbci = -1.
                if(kcrun > 1) then
                    write(log_str,*) 'xzLQbci=',sngl(xzLQbci(1:kcrun)),' est1LQ_BCI=',sngl(est1LQ_BCI)
                    call logger(63, log_str)
                    write(log_str,*) 'median(xzLQbci)=',sngl(median(xzLQbci,kcrun))
                    call logger(63, log_str)
                end if
                call Xfit (xzUQbci, sx, kcrun, 0, est1UQ_BCI, sigmam, rx1UQbci)
                if(est1UQ_BCI > ZERO) rx1UQbci = rx1UQbci/est1UQ_BCI*100._rn
                IF(kcrun == 1) rx1UQbci = -1.
                if(.false. .and. kcrun > 2) then
                    write(log_str,*) 'median(xzUQbci)=',sngl(median(xzUQbci,kcrun))
                    call logger(63, log_str)
                    xdiff = ZERO
                    do i=1,kcrun
                        xdiff(i) = abs(xzLQbci(i)-est1LQ_bci)
                    end do
                    write(log_str,*) 'SD of median(xzUQbci)=',sngl(1.858_rn/sqrt(real(kcrun-1,rn))*median(xdiff,kcrun))
                    call logger(63, log_str)
                end if
                call Xfit (xzLenBci, sx, kcrun, 0, est1LenBCi, sigmam, rx1LenBci)
                if(est1LenBCI > ZERO) rx1LenBci = rx1LenBci/est1LenBci*100._rn
                IF(kcrun == 1) rxUQbci = -1.
                write(log_str,'(a)') 'coverage intervals:     shortest                        symmetric'
                call logger(63, log_str)
                WRITE(log_str,'(a,2(es12.5,a,f8.4,a,5x))') '         lower limit : ', &
                    real(est1LQ_BCI,8),', +- ',real(rx1LQbci,8),' %', &
                    real(xLQ,8),', +- ',real(rxLQ,8),' %'
                call logger(63, log_str)
                WRITE(log_str,'(a,2(es12.5,a,f8.4,a,5x))') '         upper limit : ', &
                    real(est1UQ_BCI,8),', +- ',real(rx1UQbci,8),' %', &
                    real(xUQ,8),', +- ',real(rxUQ,8),' %'
                call logger(63, log_str)
                WRITE(log_str,'(a,es12.5,20x,es12.5)') '         interval    : ', &
                    real(est1LenBCI,8),real(xUQ-xLQ,8)
                call logger(63, log_str)

                IF(use_BCI) THEN
                end if
                if(kcrun == 1) then
                    ! for the primary estimate:
                    rxmit1PE = xxsdvPE/sqrt(real(imcPE,rn))/xxmit1PE*100._rn
                    rxsdvPE = xxsdvPE/sqrt(TWO*real(imcPE,rn)) / xxsdvPE*100._rn
                end if

                ruxxsdv = xxsdv/sqrt(TWO*real(imctrue,rn)) / xxsdv*100._rn
                write(log_str,*) 'estimated SD (in %) of uncertainty: ',sngl(ruxxsdv),'  sigma=',sngl(xxsdv),'  mean=',sngl(xxmit1)
                call logger(63, log_str)
                if(kcrun == 1) rxsdv = ruxxsdv
                if(kcrun == 1) then
                    rxmit1 = xxsdv/sqrt(real(imctrue,rn))/xxmit1*100._rn
                    rxLQ = ZERO
                    rxUQ = ZERO
                    if(xLQ > ZERO) rxLQ = SDQt((ONE - W1minusG)/TWO, imctrue, xxmit1, xxsdv) / xLQ * 100._rn
                    if(xUQ > ZERO) rxUQ = SDQt((W1minusG + ONE)/TWO, imctrue, xxmit1, xxsdv) / xUQ * 100._rn
                    rx1LQ = rxLQ
                    rx1UQ = rxUQ
                    rxLQbci = ZERO
                    rxUQbci = ZERO
                    if(estLQ_BCI2 > ZERO) rxLQbci = SDQt((ONE - W1minusG)/TWO, imctrue, xxmit1, xxsdv) / estLQ_BCI2 * 100._rn
                    if(estUQ_BCI2 > ZERO) rxUQbci = SDQt((W1minusG + ONE)/TWO, imctrue, xxmit1, xxsdv) / estUQ_BCI2 * 100._rn
                    rx1LQbci = rxLQBci
                    rx1UQbci = rxUQBci
                end if

                call WDPutEntryDouble('TRentryMCvalPE', xxmit1PE, frmtres)
                call WDPutEntryDouble('TRentryMCvalUPE', xxsdvPE*coverf, frmtres)

                call WDPutEntryDouble('TRentryMCValue', xxmit1, frmtres)
                call WDPutEntryDouble('TRentryMCunc', xxsdv*coverf, frmtres)
                call WDPutEntryDouble('TRentryMCuncrel', xxsdv*coverf/xxmit1*100._rn, frmtres)
                call WDgetCheckButton('TRcheckbutton2',kkk)
                if(kkk == 0) then
                    call WDPutEntryDouble('TRentryMClq', xLQ, frmtres)
                    call WDPutEntryDouble('TRentryMCuq', xUQ, frmtres)
                    call WDPutEntryDouble('TRentryMClqRSD', rxLQ, rmcformF(rxLQ))
                    call WDPutEntryDouble('TRentryMCuqRSD', rxUQ, rmcformF(rxUQ))
                else
                    call WDPutEntryDouble('TRentryMClq', estLQ_BCI2, frmtres)
                    call WDPutEntryDouble('TRentryMCuq', estUQ_BCI2, frmtres)
                    call WDPutEntryDouble('TRentryMClqRSD', rxLQbci, rmcformF(rxLQbci))
                    call WDPutEntryDouble('TRentryMCuqRSD', rxUQbci, rmcformF(rxUQbci))
                end if

                call WDPutEntryDouble('TRentryMCvalPERSD', rxmit1PE, rmcformF(rxmit1PE))
                call WDPutEntryDouble('TRentryMCvalUPERSD', rxsdvPE, rmcformF(rxsdvPE))

                call WDPutEntryDouble('TRentryMCValueRSD', rxmit1, rmcformF(rxmit1))
                call WDPutEntryDouble('TRentryMCuncRSD', rxsdv, rmcformF(rxsdv))

                write(log_str,*) 'rxLQ=',sngl(rxLQ),'  rxLQbci=',sngl(rxLQbci)
                call logger(63, log_str)

                VertLines(1) = xLQ
                VertLines(2) = xUQ
                VertLines(3) = xxmit1

              case (2)
                call Xfit (xxDT, sx, kcrun, 0, xDT, sigmam, rxDT)
                rxDT = rxDT/xDT*100._rn
                IF(kcrun == 1) rxDT = -1.
                WRITE(log_str,*) 'rxDT, % =',sngl(rxDT)
                call logger(63, log_str)
                call WDPutEntryDouble('TRentryMCdt', xDT, frmtres)
                call WDPutEntryDouble('TRentryMCdtRSD', rxDT, rmcformF(rxDT))

                call Xfit (xzmit, sx, kcrun, 0, xxmit2, sigmam, rxmit2)
                uqt = SDQt((ONE-alpha), imctrue, xxmit2, xDT/kalpha)
                uxxDT(1) = uqt
                write(log_str,*) 'estimated SD of xDT: absolut: ',sngl(uqt),' ,  in %: ',sngl(uqt/xDT*100._rn)
                call logger(63, log_str)
                if(kcrun == 1) rxDT = uqt/xDT*100._rn
                call WDPutEntryDouble('TRentryMCdtRSD', rxDT, rmcformF(rxDT))
                call WDPutLabelColorF('TRentryMCdt',GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))   ! 'black')

                mcafull(kqtyp,1:mcmax) = mcafull2(1:mcmax)
                mcasum(kqtyp) = mcasum2

              case (3)
                call Xfit (xzsdv, sx, kcrun, 0, xxsdv3, sigmam, rxsdv3)

                call Xfit (xzDL, sx, kcrun, 0, xDL, sigmam, rxDL)
                rxDL = rxDL/xDL*100._rn
                IF(kcrun == 1) rxDL = -1.
                WRITE(log_str,*) 'rxDL, % =',sngl(rxDL)
                call logger(63, log_str)
                call WDPutEntryDouble('TRentryMCdl', xDL, frmtres)
                call WDPutEntryDouble('TRentryMCdlRSD', rxDL, rmcformF(rxDL))
                call WDPutLabelColorF('TRentryMCdl', GTK_STATE_FLAG_NORMAL,get_color_string('entry_fg'))   ! 'black')

                call Xfit (xzmit, sx, kcrun, 0, xxmit3, sigmam, rxmit3)
                call Xfit (xzsdv, sx, kcrun, 0, xxsdv3, sigmam, dummy)
                uqt = SDQt(beta, imctrue, xxmit3, xxsdv3)
                uxxDL(1) = sqrt( uxxDT(1)**TWO + uqt**TWO )
                write(log_str,*) 'estimated SD of xDL:  absolut:',sngl(uxxDL(1)),' ,   in %: ',sngl(uxxDL(1)/xDL*100._rn), &
                    '  sigma=',sngl(xxsdv),'  xDL=',sngl(xDL)
                call logger(63, log_str)
                if(kcrun == 1) rxDL = uxxDL(1)/xDL*100._rn
                call WDPutEntryDouble('TRentryMCdlRSD', rxDL, rmcformF(rxDL))

                mcafull(kqtyp,1:mcmax) = mcafull3(1:mcmax)
                mcasum(kqtyp) = mcasum3

                call MCDistrib(kcrun,imctrue,ZERO,zero)
                WRITE(166,*) '------------------------------------------------------'

                use_shmima = .TRUE.
                ! shmin/shmax: These extreme values, for each kqtyp, are then to be used in ShowHist.
                shmin(2) = MIN(shmin(2),shmin(3))
                shmin(3) = shmin(2)
                shmax(2) = MAX(shmax(2),shmax(3))
                shmax(3) = shmax(2)
                xmin1 = ZERO
                xmax1 = ZERO
                ksv = kqtyp
                kqtyp = 2
                call MCDistrib(kcrun,imctrue, xmin1, xmax1)
                call Plotsteps(kqtyp,' ')
                kqtyp = ksv
                ksv = kqtyp
                kqtyp = 3
                call MCDistrib(kcrun,imctrue, xmin1, xmax1)
                kqtyp = ksv

                IF(xDL > ZERO) THEN
                    ksv = kqtyp
                    kqtyp = 3
                    call Plotsteps(kqtyp,' ')
                    kqtyp = ksv
                end if
              case default
            end select
            call pending_events
            call pending_events

            call MCtables(kr,kqtyp)

            WRITE(log_str,*) 'Lower confidence limit estLQ: ',sngl(estLQ)
            call logger(63, log_str)
            WRITE(log_str,*) 'Upper confidence limit estUQ: ',sngl(estUQ)
            call logger(63, log_str)

8900        continue
            iteration_on = .FALSE.

!  Restore arrays "Messwert" and their Std uncertainties:
            Messwert(1:ngrs+ncov+numd) = MesswertORG(1:ngrs+ncov+numd)
            StdUnc(1:ngrs+ncov+numd)   = StdUncORG(1:ngrs+ncov+numd)
            MesswertSV(1:ngrs+ncov+numd) = MesswertORG(1:ngrs+ncov+numd)
            StdUncSV(1:ngrs+ncov+numd)   = StdUncORG(1:ngrs+ncov+numd)


            IF((kqtyp == 2 .OR. kqtyp == 3) .AND. nvar > 0) THEN
                Messwertsv(nvar) = xwert
                StdUnc(nvar)     = sdxwert
                Messwert(nvar)   = xwert
            end if
            IF(FitDecay) THEN
                fpa(1:3) = xfpa(1:3)
                d0zrateSV(1:numd) = d0zrateSicher(1:numd)
                d0zrate(1:numd)   = d0zrateSV(1:numd)
                sd0zrateSV(1:numd) = sd0zrateSicher(1:numd)
                sd0zrate(1:numd) = sd0zrateSV(1:numd)
            end if
            IF(Gamspk1_Fit) THEN
                fpaSV(1) = xfpa(1)
                fpa(1)   = xfpa(1)
                GNetRateSV(1:numd/5) = GNetRateSicher(1:numd/5)
                GNetRate(1:numd/5)   = GNetRateSicher(1:numd/5)
                SDGNetRateSV(1:numd/5)  = SDGNetRateSicher(1:numd/5)
                SDGNetRate(1:numd/5)    = SDGNetRateSicher(1:numd/5)
                effi(1:numd/5) = effiSV(1:numd/5)
                pgamm(1:numd/5) = pgammSV(1:numd/5)
                fatt(1:numd/5) = fattSV(1:numd/5)
                fcoinsu(1:numd/5) = fcoinsuSV(1:numd/5)
            end if
            IF(k_rbl > 0) THEN
                MesswertSV(kpoint(k_rbl)) = rblindnet
                Messwert(kpoint(k_rbl)) = rblindnet
            end if

        end do          ! kqtyp
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
9000    CONTINUE

        call gtk_progress_bar_set_fraction(idpt('TRprogressbar'), 0.d0)
        call gtk_widget_set_sensitive(idpt('TRprogressbar'), 0_c_int)

        call WrStb_Ready(ifehl)

!------------------------------------------------------

        IF(GamDist_ZR) THEN
            Rbltot = RbltotSV
        end if

! Restore arrays "Messwert" and their Std uncertainties:
        Messwert(1:ngrs+ncov+numd) = MesswertORG(1:ngrs+ncov+numd)
        StdUnc(1:ngrs+ncov+numd)   = StdUncORG(1:ngrs+ncov+numd)
        MesswertSV(1:ngrs+ncov+numd) = MesswertORG(1:ngrs+ncov+numd)
        StdUncSV(1:ngrs+ncov+numd)   = StdUncORG(1:ngrs+ncov+numd)

        if(FitCalCurve) then
            ykalib(1:nkalpts) = ykalibSV(1:nkalpts)
            a_kalib(1:kal_Polgrad+1) = a_kalibSV(1:kal_Polgrad+1)
            do i=1,kal_Polgrad+1
                ! a_kalib(i) = a_kalibSV(i)
                covar_kalib(i,1:kal_Polgrad+1) = covar_kalibSV(i,1:kal_Polgrad+1)
                !do j=1,kal_polGrad+1
                !  covar_kalib(i,j) = covar_kalibSV(i,j)
                !end do
            end do
        end if

        if(allocated(bvect)) deallocate(bvect)
        if(allocated(covxy)) deallocate(covxy)
        if(allocated(zvect)) deallocate(zvect)
        if(allocated(muvect)) deallocate(muvect)
        if(allocated(muvectt)) deallocate(muvectt)

        CALL CPU_TIME(stop0)
        cpu_time_mc = stop0-start0
        write(log_str,*) 'MC-CPU-time (s): ',(sngl(stop0-start0))
        call logger(63, log_str)

    end subroutine MCCalc

    !#######################################################################

    subroutine Run_MCstart(ifehl)

        ! runs a complete Monte Carlo simulation:
        !
        ! it prepares the plotting window, executes with MCCalc the MC simulation,
        ! writes plot files to disk and finally transfers the obtained result values
        ! to the UncertRadio GUI.

        !     Copyright (C) 2014-2024  Günter Kanisch

        use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_associated
        use plplot, only: plclear, plend1

        use gtk,                    only: gtk_widget_set_sensitive, gtk_widget_show
        use gdk_pixbuf_hl,          only: hl_gdk_pixbuf_save
        use cairo,                  only: cairo_get_reference_count
        use gtk_draw_hl,            only: hl_gtk_drawing_area_get_gdk_pixbuf, &
                                          hl_gtk_drawing_area_cairo_destroy

        use ur_general_globals,     only: actual_plot, bat_mc, fname, frmtres, &
                                          Gum_restricted, results_path, kfi, linebat, &
                                          dir_sep, MCsim_on,pngfile,png_to_cairo_surface
                                        !   cairo_png_reloaded
        use UR_gtk_globals,         only: item_setintern, plinit_done, plot_setintern, zoomf

        use Rout,                   only: WDGetEntryInt,WDGetCheckButton,pending_events, &
                                          WDPutEntryDouble,ClearMCfields
        use Top,                    only: idpt
        use UR_MCC,                 only: kcrun,use_BCI,xxmit1,xxsdv,xLQ,xUQ,xDT,xDL,imcmax, &
                                          rxDT,rxDL,rxmit1,rxsdv,rxLQ,rxUQ,xmit1,xsdv,xmit2,iopt_copygr, &
                                          mcafull,mcafull2,mcafull3,arraymc,xxmit1PE,xxsdvPE, &
                                          rxmit1PE,rxsdvPE
        use plplot_code_sub1,       only: scalable, familying, gform, three_in_one, PrepareF
        use common_sub1,            only: cc, width_da, height_da, drawing
        use UR_Linft,               only: fitmeth
        use UR_Gleich_globals,      only: kEGr, coverf
        use PLsubs,                 only: CairoPlplotPrepare,Printplot,reload_pngfile

        use RdSubs,                 only: rmcformF
        use CHF,                    only: flfu
        use file_io,                only: logger

        implicit none
        integer, intent(out)     :: ifehl         ! error indicator

        type(c_ptr)           :: pixbuf    ! idpt
        integer(c_int)        :: sizewh(2), ccounts
        character(len=256)    :: plfile
        integer               :: i, kcmx, ix, i1, i2
        character(len=40)     :: cnum
        character(len=300)    :: log_str
        !-----------------------------------------------------------------------------------------------

        ifehl = 0
        call WDGetEntryInt('TRentryMCanzM', kcmx)
        call WDGetEntryInt('TRentryMCanzR', kcrun)
        call WDGetCheckButton('TRcheckbutton2', ix)
        use_BCI = .FALSE.
        IF(ix == 1) use_BCI = .TRUE.

        call ClearMCFields(0)

        call pending_events
        !-------------------------------------------------------------------------

        png_to_cairo_surface = .false.         ! 16.5.2025 GK
        actual_plot = 'MCplot'
        call CairoPlplotPrepare(actual_plot)
        scalable = .false.
        familying = .false.
        gform = 'png'
        gform = 'jpeg'
        ! gform = 'svg'
        ! gform = 'pdf'
        three_in_one = .true.
        if(Gum_restricted) three_in_one = .false.
        call PrepareF(actual_plot)

        sizewh = (/ width_da(1)+0, height_da(1)+0 /)
        sizewh(1) = int(sizewh(1) * zoomf)
        sizewh(2) = int(sizewh(2) * zoomf)

        call gtk_widget_set_sensitive(idpt('TRButtonStartMC'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('TRButtonStartMC1'), 0_c_int)

        !-------------------------------------------------------------------------


        !!!! open(63,file=flfu(results_path)//'MC_Tables.txt', status='unknown')
        call logger(63, ' ')
        call logger(63, "Project:  " // trim(fname))        ! 17.10.2025 GK

        !write(63,*) ' MCC: kcmx=',kcmx,'  kcrun=',int(kcrun,2)
        !write(63,*) 'plinit_done=',plinit_done
        write(log_str,'(2(A,i0),a,L1)') ' MCC: kcmx=',kcmx,'  kcrun=',kcrun,'  plinit_done=',plinit_done
        call logger(63, log_str)

        call plclear()

        ! starts MCcalc:
        xmit1 = ZERO
        xmit2 = ZERO
        xsdv  = ZERO
        imcmax = kcmx
        call MCCalc()
        if(ifehl == 1) then
            call plend1()
            plinit_done = .false.
            goto 900
        end if

        !  pixbuf = hl_gtk_drawing_area_get_gdk_pixbuf(drawing(1))
        !  write(63,*) 'before Plot end:  pixbuf=',pixbuf
        call pending_events()
        call plend1()
        ! plotting output closed here.

        plinit_done = .false.

        call gtk_widget_show(drawing(2))

        do while(cairo_get_reference_count(cc(1)) > 1_c_int)
            if(c_associated(cc(1))) call hl_gtk_drawing_area_cairo_destroy(cc(1))
        end do
        ccounts = cairo_get_reference_count(cc(1))

        ! call gtk_window_set_keep_above(windowPL,0_c_int)
        call pending_events
        if(ifehl == 1) goto 900   ! return

        pixbuf = hl_gtk_drawing_area_get_gdk_pixbuf(drawing(1))

        if(trim(actual_plot) == 'MCplot') then
           if(c_associated(cc(1))) call hl_gtk_drawing_area_cairo_destroy(cc(1))  ! desactivated 16.5.2025
        end if
        call pending_events

        if( .not. bat_mc) then
            !
            plfile = 'MCplotfile.png'

            plfile = trim(results_path) // trim(plfile)
            pngfile = plfile         ! 16.5.2025

        end if           ! <-- 18.6.2024

        if(bat_mc) then
            plfile = trim(fname)
            i2 = 0
            do i=len_trim(fname),1,-1
                if(plfile(i:i) == dir_sep) then
                    i2 = i
                    exit
                end if
            end do

            if(i2 > 0) plfile = trim(results_path) // trim(plfile(i2+1:))

            i1 = index(plfile,'.')
            if(fitmeth(1:1) == char(0)) fitmeth = ' '
            if(len_trim(fitmeth) > 0) then
                if(kEGr == 1) plfile = '"' // plfile(1:i1-1) // '_MC_EG1_' // trim(fitmeth) // '.png' // '"'
                if(kEGr == 2) plfile = '"' // plfile(1:i1-1) // '_MC_EG2_' // trim(fitmeth) // '.png' // '"'
                if(kEGr == 3) plfile = '"' // plfile(1:i1-1) // '_MC_EG3_' // trim(fitmeth) // '.png' // '"'
            else
                if(kEGr == 1) plfile = plfile(1:i1-1) // '_MC_EG1' // '.png'
                if(kEGr == 2) plfile = plfile(1:i1-1) // '_MC_EG2' // '.png'
                if(kEGr == 3) plfile = plfile(1:i1-1) // '_MC_EG3' // '.png'
            end if
        end if
        if(bat_mc) then
            ! write(cnum,'(i2.2)') lineBat
            ! write(cnum,'(i2.2)') kfi
            write(cnum,'(a2,i2.2,a1,i1)') 'BT',kfi,'_',kEGr
            plfile = 'MCplotfile_' // trim(cnum) // '.png'
            plfile = trim(results_path) // trim(plfile)
        end if
        pngfile = plfile       ! 16.5.2025

        ! if( .false. ) then
        call hl_gdk_pixbuf_save(pixbuf, plfile, 'png')
        ! else
        !     ! instead of saving the extcairo pixbuf (which can have small optical problems):
        !     ! repeat the steps for the whole graphics output, but now directly into a png file
        !     ! 16.5.2025 GK
        !     png_to_cairo_surface = .true.
        !     actual_plot = 'MCplot'
        !     call CairoPlplotPrepare(actual_plot)
        !     scalable = .false.
        !     familying = .false.
        !     gform = 'png'
        !     iopt_copygr = 1
        !     call Printplot()
        !     call reload_pngfile(pngfile)
        !     cairo_png_reloaded = .true.            ! <--  added 20.5.2025
        ! endif



        if( .false. .and.  bat_mc) then          ! 18.6.2024
            write(cnum,'(i2.2)') lineBat
            plfile = 'MCplotfile_' // trim(cnum) // '.png'
            plfile = trim(results_path) // trim(plfile)
            call hl_gdk_pixbuf_save(pixbuf, plfile, 'png')
        end if

        if( .false. .and. (bat_mc)) then
            iopt_copygr = 4
            call PrintPlot()
            call pending_events
        end if

        call WDPutEntryDouble('TRentryMCvalPE', xxmit1PE, frmtres)
        call WDPutEntryDouble('TRentryMCvalUPE', xxsdvPE*coverf, frmtres)

        call WDPutEntryDouble('TRentryMCValue', xxmit1, frmtres)
        call WDPutEntryDouble('TRentryMCunc', xxsdv*coverf, frmtres)
        call WDPutEntryDouble('TRentryMCuncrel', xxsdv*coverf/xxmit1*100._rn, frmtres)
        call WDPutEntryDouble('TRentryMClq', xLQ, frmtres)
        call WDPutEntryDouble('TRentryMCuq', xUQ, frmtres)
        call WDPutEntryDouble('TRentryMCdt', xDT, frmtres)
        call WDPutEntryDouble('TRentryMCdl', xDL, frmtres)

        call WDPutEntryDouble('TRentryMCvalPERSD', rxmit1PE, rmcformF(rxmit1PE))
        call WDPutEntryDouble('TRentryMCvalUPERSD', rxsdvPE, rmcformF(rxsdvPE))

        call WDPutEntryDouble('TRentryMCValueRSD', rxmit1, rmcformF(rxmit1))
        call WDPutEntryDouble('TRentryMCuncRSD', rxsdv, rmcformF(rxsdv))
        call WDPutEntryDouble('TRentryMClqRSD', rxLQ, rmcformF(rxLQ))
        call WDPutEntryDouble('TRentryMCuqRSD', rxUQ, rmcformF(rxUQ))
        call WDPutEntryDouble('TRentryMCdtRSD', rxDT, rmcformF(rxDT))
        call WDPutEntryDouble('TRentryMCdlRSD', rxDL, rmcformF(rxDL))

900     continue

        call gtk_widget_set_sensitive(idpt('TRButtonStartMC'), 1_c_int)
        call gtk_widget_set_sensitive(idpt('TRButtonStartMC1'), 1_c_int)
        item_setintern = .false.
        plot_setintern = .false.
        MCsim_on = .false.              ! <--   19.11.2024 GK

        if(allocated(mcafull)) deallocate(mcafull)
        if(allocated(mcafull2)) deallocate(mcafull2)
        if(allocated(mcafull3)) deallocate(mcafull3)
        if(allocated(arraymc)) deallocate(arraymc)
        !!!!   close(unit=63)

    end subroutine Run_MCstart

!#######################################################################


end module MCC
