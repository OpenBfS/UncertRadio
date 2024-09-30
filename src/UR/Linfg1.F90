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
module LF1G

contains

    ! Linfg1Ausf
    ! Linfg1
    ! Linfg1out
    ! LsqCoxGWM

!#######################################################################

    subroutine Linfg1Ausf(mode,akt,SDakt)

        use, intrinsic :: iso_c_binding
        USE UR_Gleich,        only: kgspk1,loadingpro,Messwert,Stdunc,stduncsv,MesswertSV, &
                                    ngrs,ncov
        USE UR_Linft,         only: fpa,fpaSV,numd
        USE UR_Variables,     ONLY: langg
        use Rout,             only: WTreeViewPutDoubleCell,WDPutEntryString, &
                                    WDPutEntryDouble,WTreeViewPutDoubleCell,pending_events
        use Top,              only: WrStatusbar
        use UR_params,        only: rn
        use UR_DLIM,          only: iteration_on

        implicit none

        integer   ,INTENT(IN)  :: mode        !  1: do the evaluation:
        !  2: do the evaluation, with output to the file linfout.txt
        real(rn), INTENT(OUT)  :: akt         ! activity
        real(rn), INTENT(OUT)  :: SDakt       ! its standard uncertainty

        integer   , parameter  :: mag = 1

        !-----------------------------------------------------------------------
        IF(langg == 'DE') call WrStatusBar(4,'Rechnet...' )
        IF(langg == 'EN') call WrStatusBar(4,'Calculating...' )
        IF(langg == 'FR') call WrStatusBar(4,'Calcule...' )
        if(.not.loadingPro) call pending_events()              !xx
        call Linfg1(akt,SDakt)
        fpaSv(1:mag) = fpa(1:mag)

        IF(mode == 2) THEN
            IF(langg == 'DE') call WrStatusBar(4, 'Gammauswertung: ansehen/speichern/drucken' )
            IF(langg == 'EN') call WrStatusBar(4, 'Gamma evaluations:  view/save/print' )
            IF(langg == 'FR') call WrStatusBar(4, 'Gamma evaluations:  vue/enregistrer/imprimer' )
            call Linfg1out()
        END IF

        Messwert(kgspk1) = akt
        MesswertSV(kgspk1) = Messwert(kgspk1)
        StdUnc(kgspk1)   = SDakt
        StdUncSV(kgspk1) = SDakt
        call WTreeViewPutDoubleCell('treeview2', 5, kgspk1, Messwert(kgspk1))
        call WTreeViewPutDoubleCell('treeview2', 11, kgspk1, StdUnc(kgspk1))

        if(.not.iteration_on) then
            MEsswertSV(ngrs+ncov+1:ngrs+ncov+numd) = MEsswert(ngrs+ncov+1:ngrs+ncov+numd)
            StdUncSV(ngrs+ncov+1:ngrs+ncov+numd) = StdUnc(ngrs+ncov+1:ngrs+ncov+numd)
        end if

    end subroutine Linfg1Ausf

!#######################################################################

    subroutine Linfg1(akt,SDakt)

        USE UR_Gleich,     only: missingval,ncov,ngrs,upropa_on,kpoint,covarval,Messwert,StdUnc, &
            kableitnum,kgspk1
        USE UR_Linft,      only: fpa,sfpa,chisq,Chisqr,numd,mfit
        USE fparser,       ONLY: evalf, EvalErrMsg
        USE UR_Perror
        USE UR_Gspk1Fit,   only: ecorruse,gspk_chisqr,gspk_qval,gspk_free,gspk_sigext, &
            gspk_sigint,gspk_xmit,mwtyp,wmextsd,SDGNetRate,gnetrate,varadd_rn,sdeffi, &
            pgamm,sdpgamm,fatt,sdfatt,fcoinsu,SDfcoinsu,aktnz,sdaktnz,effi, &
            guse,SDaktnzSV,SDaktnzMV
        USE UR_DLIM,       ONLY: iteration_on,limit_typ
        USE UR_Variables,  ONLY: MCsim_on
        use Brandt,        only: gincgm
        use UR_params,     only: rn,ZERO,ONE,TWO,EPS1MIN
        use Rout,          only: WTreeViewGetDoubleArray
        ! USE, INTRINSIC        :: IEEE_ARITHMETIC
        use CHF,           only: isNaN
        implicit none

        real(rn), INTENT(OUT)    :: akt,SDakt        ! activity and its standard uncertainty

        integer   ,    parameter    :: mag = 1

        integer           :: i,npts,nhh,nck,k1,k2
        EXTERNAL          :: funcs
        LOGICAL           :: ok
        real(rn)          :: wi
        real(rn)          :: zfact,cov
        real(rn)          :: phi,urelphi2(numd/5),t2
        real(rn)          :: gwcovy(numd/5,numd/5),gwx(1),gwcx(1,1),gwr,gwa(numd/5,1)
        real(rn)          :: rho(numd/5,numd/5),gwpa(numd/5),u2max
        real(rn)          :: covt,psi(numd/5),wmx,uwmx,phix(numd/5)
        integer           :: gwlist(1),n1,n2,nr,j,kqt,i_arr(numd/5),nhh_arr(numd/5)
        LOGICAL           :: prout
!-----------------------------------------------------------------------

        prout = .FALSE.
        ! prout = .TRUE.

        kqt = 1
        if(iteration_on .and. limit_typ == 1) kqt = 2
        if(iteration_on .and. limit_typ == 2) kqt = 3

! wird nur benötigt, wenn weiter unten Lincov2 aufgerufen würde
        mfit = 0
        zfact = ONE

        if(allocated(aktnz)) deallocate(aktnz,SDaktnz)
        allocate(aktnz(numd/5),SDaktnz(numd/5))
        if(.not.allocated(SDaktnzSV)) allocate(SDaktnzSV(numd/5))
        if(.not.allocated(SDaktnzMV)) allocate(SDaktnzMV(numd/5))

        ! values and standard deviations of the peak net count rates:
        i_arr = [(i,i=1,numd/5)]
        nhh_arr = (i_arr-1)*5 + 1

        GNetRate(i_arr)   = Messwert(ngrs+ncov+nhh_arr)
        SDGNetRate(i_arr) = SQRT( GNetRate(i_arr)/Messwert(kpoint(2)) + varadd_Rn(i_arr) )

        ! values and standard deviations of:
        ! the peak efficiencies:
        Effi(i_arr)       = Messwert(ngrs+ncov+nhh_arr+1)
        SDEffi(i_arr)     = StdUnc(ngrs+ncov+nhh_arr+1)
        ! the gamma line emission intensities:
        pgamm(i_arr)      = Messwert(ngrs+ncov+nhh_arr+2)
        SDpgamm(i_arr)    = StdUnc(ngrs+ncov+nhh_arr+2)
        ! the attenuation corrections:
        fatt(i_arr)       = Messwert(ngrs+ncov+nhh_arr+3)
        SDfatt(i_arr)     = StdUnc(ngrs+ncov+nhh_arr+3)
        ! the true Coinsum corrections:
        fcoinsu(i_arr)    = Messwert(ngrs+ncov+nhh_arr+4)
        SDfcoinsu(i_arr)  = StdUnc(ngrs+ncov+nhh_arr+4)

        ! calculate the activities aktnz for each gamma line:
        phix(i_arr) = ( fatt(i_arr) * fcoinsu(i_arr) ) / ( effi(i_arr) * pgamm(i_arr) )
        aktnz(i_arr) = GNetRate(i_arr) * phix(i_arr)
        urelphi2(i_arr) = (SDeffi(i_arr)/effi(i_arr))**TWO + (SDpgamm(i_arr)/pgamm(i_arr))**TWO + (SDfatt(i_arr)/fatt(i_arr))**TWO  &
            + (SDfcoinsu(i_arr)/fcoinsu(i_arr))**TWO
        SDaktnz(i_arr) = SQRT( (phix(i_arr) * SDGnetRate(i_arr))**TWO + GnetRate(i_arr)**TWO*urelphi2(i_arr)*phix(i_arr)**TWO )

        do i=1,numd/5
            ! values and standard deviations of the peak net count rates:
            nhh = (i-1)*5 + 1
            if(.true.) then
                if(kqt == 1) then
                    if(kableitnum == 0 .and. .not.iteration_on) then
                        SDaktnzMV(i) = SDaktnz(i)
                    end if
                    if(kableitnum > 0 .and. kableitnum /= kgspk1) then
                        SDaktnz(i) = SDaktnzMV(i)
                    end if
                else
                    if(SDaktnzMV(i) > ZERO) SDaktnz(i) = SDaktnzMV(i)
                end if
            end if

            t2 = phix(i)**TWO * varadd_Rn(i)

            IF(prout) THEN
                WRITE(66,*) 'Linfg1: Gnetrate,Effi,pgamm,fatt,fcoinsu:', sngl(GnetRate(i)), &
                    sngl(effi(i)),sngl(pgamm(i)),sngl(fatt(i)),sngl(fcoinsu(i))
                WRITE(66,*) 'Linfg1: associated SD''s dazu:',sngl(SDGnetRate(i)),sngl(SDeffi(i)),  &
                    sngl(SDpgamm(i)),sngl(SDfatt(i)),sngl(SDfcoinsu(i))
                WRITE(66,*) 't2=',sngl(t2),'   varadd_Rn=',sngl(varadd_Rn(i)),'   phi=',sngl(phi),  &
                    '  urelphi^2=',sngl(urelphi2)
            end if


        end do

        ! write(66,*) 'Linfg1: aktnz=',sngl(aktnz(1:4))
        ! write(66,*) 'Linfg1: phi=',sngl(phix(1:4))
        ! write(66,*) 'Linfg1: effi=',sngl(effi(1:4))
        ! write(66,*) 'Linfg1: GNetRate=',sngl(GNetRate(1:4))
        ! write(66,*) 'Linfg1: urel%(GNetRate)=',(sngl(sdGNetRate(i)/GNetRate(i)*100._rn),i=1,4)

        select case (TRIM(mwtyp))
          case ('WeiMean')
            ! calculate now the weighted mean of the peaks' activities, with internal SD:
            npts = numd/5
            gspk_xmit = ZERO
            gspk_sigint = ZERO
            npts = 0
            npts = sum(guse)
            gspk_xmit = sum (real(guse(i_arr),rn)*aktnz(i_arr)/SDaktnz(i_arr)**TWO)
            gspk_sigint = sum (real(guse(i_arr),rn)/SDaktnz(i_arr)**TWO)
            !do i=1,numd/5
            !  IF(guse(i) == 1) THEN
            !    npts = npts + 1
            !    wi = one/SDaktnz(i)**two
            !    gspk_xmit = gspk_xmit + aktnz(i)*wi
            !    gspk_sigint = gspk_sigint + wi
            !  END IF
            !end do
            gspk_sigint = SQRT(ONE / gspk_sigint)
            gspk_xmit = gspk_xmit * gspk_sigint**TWO
            wmx = ZERO
            uwmx = ZERO
            psi(i_arr) = ONE/SDAktnz(i_arr)**TWO * gspk_sigint**TWO
            wmx = sum(psi(i_arr)*aktnz(i_arr))
            uwmx = sum(psi(i_arr)*TWO*SDaktnz(i_arr)**TWO)
            !do i=1,numd/5
            !  psi(i) = one/SDAktnz(i)**two * gspk_sigint**two
            !  wmx = wmx + psi(i)*aktnz(i)
            !  uwmx = uwmx + psi(i)**two*SDAktnz(i)**two
            !end do
            uwmx = SQRT(uwmx)

            cov = ZERO
            nck = 0
            IF(ncov > 0 .and. ecorruse == 1) THEN
                do k1=1,numd/5-1
                    do k2=k1+1,numd/5
                        nck = nck + 1
                        IF(guse(k1) == 0 .OR. guse(k2) == 0 .OR. abs(covarval(nck)-missingval)< EPS1MIN ) CYCLE
                        covt = gspk_sigint**TWO * aktnz(k1)/Effi(k1) * aktnz(k2)/Effi(k2) * &
                            covarval(nck)  / (SDaktnz(k1)*SDaktnz(k2))**TWO
                        cov = cov + TWO * covt
                    end do
                end do
            end if
            IF(.not.upropa_on .and. .not.iteration_on .AND. .not.MCSim_on)  &
                WRITE(66,*) 'LinFG1: Weighted mean:  cov=',sngl(cov),  &
                '  covarval(1)=',sngl(covarval(1)), &
                ' gspk_sigint=',sngl(gspk_sigint)
            gspk_sigint = gspk_sigint * SQRT(ONE + cov)

            IF(npts > 1) THEN
                gspk_free = real(npts - 1,rn)

                chisq = ZERO
                gspk_sigext = ZERO
                chisq = sum(real(guse(i_arr),rn)*(aktnz(i_arr)-gspk_xmit)**TWO /SDaktnz(i_arr)**TWO)
                !do i=1,numd/5
                !  IF(guse(i) == 1) THEN
                !    wi = one/SDaktnz(i)**two
                !    chisq = chisq + (aktnz(i)-gspk_xmit)**two * wi
                !  END IF
                !end do
                gspk_sigext = SQRT(chisq * gspk_sigint**TWO / gspk_free)
                gspk_chisqr = chisq / gspk_free
                IF(ISNAN(chisq)) chisq = ZERO
                gspk_qval = ONE - gincgm(gspk_free/TWO, chisq/TWO)
            else
                gspk_free = ZERO
                gspk_chisqr = ZERO
                gspk_sigext = gspk_sigint
            end if

            akt = gspk_xmit
            SDakt = gspk_sigint
            IF(WMextSD == 1) SDakt = gspk_sigext

          case ('LSQMean')
            ! calculate now the least-squares mean, according to Cox et al.:
            u2max = 0._rn

            do k1=1,numd/5
                IF(guse(k1) == 0) CYCLE
                IF(SDAktnz(k1)**TWO > u2max) u2max = SDAktnz(k1)**TWO
            end do
            gwpa = ONE
            n1 = 0
            do k1=1,numd/5
                IF(guse(k1) == 0) CYCLE
                n1 = n1 + 1
                gwpa(n1) = SDAktnz(k1)**TWO / u2max
            end do

            gwcovy = ZERO
            rho = ZERO
            nck = 0
            n1 = 0
            n2 = 0
            do k1=1,numd/5
                IF(guse(k1) == 1) THEN
                    n1 = n1 + 1
                    n2 = n1
                    gwcovy(n1,n1) = SDAktnz(k1)**TWO
                end if
                IF(k1 == numd/5) EXIT
                IF(ecorruse == 1) THEN
                    do k2=k1+1,numd/5
                        nck = nck + 1
                        IF(guse(k1) == 0 .OR. guse(k2) == 0 .OR. abs(covarval(nck)-missingval) < EPS1MIN ) CYCLE
                        n2 = n2 + 1
                        gwcovy(n1,n2) = aktnz(k1)/Effi(k1) * aktnz(k2)/Effi(k2) * covarval(nck)

                        rho(n1,n2) = gwcovy(n1,n2) / u2max / gwpa(n1) /gwpa(n2)
                        rho(n2,n1) = rho(n1,n2)
                        gwcovy(n2,n1) = gwcovy(n1,n2)
                    end do
                end if
            end do

            nr = 1
            gwlist(1) = 1

            call LsqCoxGWM(aktnz,gwcovy,numd/5,nr,n1,gwx,gwcx,gwr,gwa,ok)

            IF(.not.upropa_on .and. .not.iteration_on .and. .not.MCsim_on) THEN
                WRITE(66,*) 'Least-sq. weighted mean: ',sngl(gwx(1)),  &
                    '   uncertainty   =',sngl(SQRT(gwcx(1,1))),'  Chisqr=',sngl(gwr/real(MAX(1,Numd/5-1),rn))
                WRITE(66,*) 'WMean/LSQ w. mean      : ',sngl(akt/gwx(1)),'   uncert./uncLSQ=',sngl(SDakt/SQRT(gwcx(1,1))), &
                    '  Chisqr=',sngl(gspk_chisqr),'  covarval(1)=',sngl(covarval(1))
            end if

            akt = gwx(1)
            gspk_xmit = akt
            SDakt = SQRT(gwcx(1,1))
            gspk_sigint = SDAkt
            gspk_free = real(numd/5 - 1,rn)
            gspk_chisqr = gwr/MAX(ONE,gspk_free)

        end select

        IF(prout) THEN
            WRITE(66,*) 'Linfg1: akt=',sngl(akt),'  rel.U.% =',sngl(SDakt/akt*100._rn),'  SDakt=',sngl(SDakt)
            WRITE(66,*) 'aktnz: ',(sngl(aktnz(i)),i=1,npts)
            WRITE(66,*) 'SDaktnz  : ',(sngl(SDaktnz(i)),i=1,npts)
            WRITE(66,*) 'SDaktnz% : ',(sngl(SDaktnz(i)/aktnz(i)*100._rn),i=1,npts)
        end if

        do i=1,mag
            fpa(i) = akt
        end do

        sfpa = ZERO
        do i=1,mag
            IF(abs(akt) > EPS1MIN) THEN
                sfpa(i) = SDakt
            END IF
        end do

        Chisqr = gspk_chisqr

    end subroutine Linfg1

!#######################################################################

    subroutine Linfg1out()

        USE UR_Gleich,     only: loadingpro
        USE UR_Linft,      ONLY: numd
        USE UR_Gspk1Fit,   only: fbt,gspk_chisqr,gspk_free,gspk_qval,gspk_sigint,gspk_sigext,gspk_xmit, &
            mwtyp,guse,erg,gnetrate,effi,pgamm,fatt,fcoinsu,sdgnetrate,sdeffi,sdfatt, &
            sdfcoinsu,aktnz,sdpgamm,sdaktnz
        USE UR_Variables,  ONLY: langg, results_path
        use chf,           only: flfu


        implicit none

        integer            :: i,jdr
        CHARACTER(LEN=90)  :: headline
        CHARACTER(LEN=100) :: str1
        CHARACTER(LEN=30)  :: cc1,cc2
        CHARACTER(LEN=15)  :: cspec
!-----------------------------------------------------------------------
        close (22)
        OPEN(22,FILE=flfu(results_path // 'linfout.txt'),status='unknown')
        jdr = 22

        WRITE(jdr,'(a)') TRIM(mwtyp)// ':'
        WRITE(jdr,'(100a1)') ('-',i=1,70)

        IF(langg == 'DE') WRITE(jdr,'(a,3x,f6.3,/)') '(1 + b/2L)-äquivalenter Faktor für Compton-UG-Rate:', FBT
        IF(langg == 'EN') WRITE(jdr,'(a,3x,f6.3,/)') '(1 + b/2L) equivalent factor for Compton BG rate:', FBT
        IF(langg == 'FR') WRITE(jdr,'(a,3x,f6.3,/)') '(1 + b/2L) facteur équivalent pour le taux de fond Compton:', FBT

        IF(langg == 'DE') WRITE(jdr,'(a,/,3x,a,/)') 'Einzelne Peakdaten:',  &
            '(pgamm*fcoin ist ein Maß für Wichtigkeit der Linie!)'
        IF(langg == 'EN') WRITE(jdr,'(a,/,3x,a,/)') 'Individual peak data:',  &
            '(pgamm*fcoin is a measure for the importance of the line!)'
        IF(langg == 'FR') WRITE(jdr,'(a,/,3x,a,/)') 'Données de pointe individuelles:',  &
            '(pgamm*fcoin est une mesure de l''importance de la ligne!)'

        WRITE(jdr,10)
10      FORMAT(                         &
            ' i  E          PNRate    epsPeak     pgamm     fatt      fcoin  (pgamm*fcoin)',/, &
        ! '    keV        cps         %                                              ',/, &
            '    keV        cps                                                        ',/, &
            '----------------------------------------------------------------------------------')

        do i=1,numd/5
            IF(guse(i) == 0) CYCLE

            IF(langg == 'EN') cspec = 'values'
            IF(langg == 'DE') cspec = 'Werte'
            IF(langg == 'FR') cspec = 'Valeurs'
            WRITE(jdr,'(i2,2x,f7.2,2x,es10.3,2x,f9.6,3x,f8.6,2x,f6.4,2x,f8.4,1x,f8.4,1x,a)') &
                i,erg(i),GNetRate(i), effi(i),pgamm(i),  &
                fatt(i),fcoinsu(i),pgamm(i)*fcoinsu(i),TRIM(cspec)
            cspec = 'u_rels in %'
            WRITE(jdr,'(11x,2x,f6.2,4x,2x,f9.6,3x,f8.6,2x,f6.4,2x,f8.4,1x,8x,1x,a)') &
                SDGnetRate(i)*100./GNetRate(i), sdeffi(i)*100./effi(i), &
                sdpgamm(i)*100./pgamm(i),sdfatt(i)*100./fatt(i), &
                sdfcoinsu(i)*100./fcoinsu(i),TRIM(cspec)
        end do
        WRITE(jdr,*)

        IF(langg == 'DE') headline = 'Ergebnisse einzelner Peak-Aktivitäten:'
        IF(langg == 'EN') headline = 'Results from individual peak activities:'
        IF(langg == 'FR') headline = 'Résultats des activités de pointe individuelles:'
        WRITE(jdr,'(a,/)') TRIM(headline)
        WRITE(jdr,'(3x,a,/)') 'A(i) = PeakNetRate(i) * (fatt(i) * fcoin(i)) / (epsPeak(i) * pgamm(i))'

        IF(langg == 'DE') WRITE(jdr,'(a)') ' i    E(keV)    Aktivität (Bq)   rel.StdAbw (%)'
        IF(langg == 'EN') WRITE(jdr,'(a)') ' i    E(keV)    Activity (Bq)    rel.StdDev (%)'
        IF(langg == 'FR') WRITE(jdr,'(a)') ' i    E(keV)    Activité (Bq)    rel.StdDev (%)'
        WRITE(jdr,15)
15      FORMAT(50('-'))
        do i=1,numd/5
            IF(guse(i) == 0) CYCLE
            WRITE(jdr,'(i2,3x,f7.2, 3x, es11.4,6x, f6.2)') i, erg(i),aktnz(i), SDaktnz(i)/aktnz(i)*100.
        END do

        select case (mwtyp)
          case ('WeiMean')

            IF(langg == 'DE') THEN
                str1 = 'Auswertung des gewichteten Mittelwerts:'
                cc1 = '(Bayes-konform)'
                cc2 = '(nicht Bayes-komform)'
            end if
            IF(langg == 'EN') THEN
                str1 = 'Evaluation of the weighted mean:'
                cc1 = '(Bayes compliant)'
                cc2 = '(not Bayes compliant)'
            end if
            IF(langg == 'FR') THEN
                str1 = 'Évaluation de la moyenne pondérée'
                cc1 = '(Bayes conforme)'
                cc2 = '(pas conforme Bayes)'
            end if
            IF(langg == 'DE') WRITE(jdr,23) TRIM(str1) ,gspk_xmit,  &
                gspk_sigint, (gspk_sigint/gspk_xmit*100.),TRIM(cc1)
23          FORMAT(/,a,/,                                         &
                1x,'  gewichteter Mittelwert         = ',1pg13.5,/,                   &
                1x,'  Std.Abw. d. Mittelwerts        = ',1pg13.5,' (',0pf6.2,' %)  ',a)

            IF(langg == 'DE' .and. gspk_free > 0.) WRITE(jdr,24)            &
                gspk_sigext, (gspk_sigext/gspk_xmit*100.),TRIM(cc2),   &
                gspk_chisqr*gspk_free, gspk_chisqr, gspk_qval
24          FORMAT(                                                               &
                1x,'  ext. Std.Abw. d. Mittelwerts   = ',1pg13.5,' (',0pf6.2,' %)  ',a,/, &
                1x,'  Chi-Quadrat  = Testwert  T     = ',1pg13.5,/,                   &
                1x,'  reduziertes Chi-Quadrat        = ',1pg13.5,/,                   &
                1x,'  Signifikanz (Chi-Quadrat > T)  = ',f8.5,' %',/ )

            IF(langg == 'EN') WRITE(jdr,25) TRIM(str1) ,gspk_xmit,  &
                gspk_sigint, (gspk_sigint/gspk_xmit*100.),TRIM(cc1)
25          FORMAT(/,a,/,                                         &
                1x,'  weighted mean                  = ',1pg13.5,/,                   &
                1x,'  int. std. dev. of the mean     = ',1pg13.5,' (',0pf6.2,' %)  ',a)

            IF(langg == 'EN' .and. gspk_free > 0.) WRITE(jdr,26)   &
                gspk_sigext, (gspk_sigext/gspk_xmit*100.),TRIM(cc2),   &
                gspk_chisqr*gspk_free, gspk_chisqr, gspk_qval
26          FORMAT(                                                              &
                1x,'  ext. std. dev. of the mean     = ',1pg13.5,' (',0pf6.2,' %)  ',a,/, &
                1x,'  Chi-square  = test value T     = ',1pg13.5,/,                   &
                1x,'  reduced Chi-square             = ',1pg13.5,/,                   &
                1x,'  significance (Chi-square > T)  = ',f8.5,' %',/ )

            IF(langg == 'FR') WRITE(jdr,27) TRIM(str1) ,gspk_xmit,  &
                gspk_sigint, (gspk_sigint/gspk_xmit*100.),TRIM(cc1)
27          FORMAT(/,a,/,                                         &
                1x,'  moyenne pondérée               = ',1pg13.5,/,                   &
                1x,'  int. std. dev. de moyenn pond. = ',1pg13.5,' (',0pf6.2,' %)  ',a)

            IF(langg == 'FR' .and. gspk_free > 0.) WRITE(jdr,28)   &
                gspk_sigext, (gspk_sigext/gspk_xmit*100.),TRIM(cc2),   &
                gspk_chisqr*gspk_free, gspk_chisqr, gspk_qval
28          FORMAT(                                                              &
                1x,'  ext. std. dev. de moyenn pond. = ',1pg13.5,' (',0pf6.2,' %)  ',a,/, &
                1x,'  Chi-square  = test valeur T    = ',1pg13.5,/,                   &
                1x,'  Chi-carré réduit               = ',1pg13.5,/,                   &
                1x,'  importance (Chi-square > T)    = ',f8.5,' %',/ )

            IF(gspk_free > 1.) THEN
                IF(langg == 'DE') WRITE(jdr,'(a,/)') 'Hinweis: nur die interne Standardabweichung wird verwendet!'
                IF(langg == 'EN') WRITE(jdr,'(a,/)') 'Note: only the internal standard deviation will be used hereafter!'
                IF(langg == 'FR') WRITE(jdr,'(a,/)') 'Remarque: seul l''écart type interne sera utilisé ci-après!'
            end if
            WRITE(jdr,'(100a1)') ('-',i=1,70)

          case ('LSQMean')
            IF(langg == 'DE') THEN
                str1 = 'Auswertung des gewichteten Mittelwerts mit least-squares:'
                cc1 = '(Bayes-konform)'
                cc2 = '(nicht Bayes-konform)'
            end if
            IF(langg == 'EN') THEN
                str1 = 'Evaluation of the weighted mean by least-squares:'
                cc1 = '(Bayes compliant)'
                cc2 = '(not Bayes compliant)'
            end if
            IF(langg == 'FR') THEN
                str1 = 'Évaluation de la moyenne pondérée par les moindres carrés:'
                cc1 = '(Bayes conforme)'
                cc2 = '(pas Bayes conforme)'
            end if

            IF(langg == 'DE') WRITE(jdr,123) TRIM(str1) ,gspk_xmit,  &
                gspk_sigint, (gspk_sigint/gspk_xmit*100.)
123         FORMAT(/,a,/,                                         &
                1x,'  gewichteter Mittelwert         = ',1pg13.5,/,                   &
                1x,'  Std.Abw. d. Mittelwerts        = ',1pg13.5,' (',0pf6.2,' %)  ')

            IF(langg == 'DE' .and. gspk_free > 0.) WRITE(jdr,124) gspk_chisqr
124         FORMAT(                                                               &
                1x,'  reduziertes Chi-Quadrat        = ',1pg13.5,/)

            IF(langg == 'EN') WRITE(jdr,125) TRIM(str1) ,gspk_xmit,  &
                gspk_sigint, (gspk_sigint/gspk_xmit*100.)
125         FORMAT(/,a,/,                                         &
                1x,'  weighted mean                  = ',1pg13.5,/,                   &
                1x,'  std. dev. of the mean          = ',1pg13.5,' (',0pf6.2,' %)  ')

            IF(langg == 'EN' .and. gspk_free > 0.) WRITE(jdr,126) gspk_chisqr
126         FORMAT(                                                              &
                1x,'  reduced Chi-square             = ',1pg13.5,/)

            IF(langg == 'FR') WRITE(jdr,127) TRIM(str1) ,gspk_xmit,  &
                gspk_sigint, (gspk_sigint/gspk_xmit*100.)
127         FORMAT(/,a,/,                                         &
                1x,'  moyenne pondérée               = ',1pg13.5,/,                   &
                1x,'  std. dev. de moyenn pond.      = ',1pg13.5,' (',0pf6.2,' %)  ')

            IF(langg == 'FR' .and. gspk_free > 0.) WRITE(jdr,128) gspk_chisqr
128         FORMAT(                                                              &
                1x,'  Chi-carré réduit               = ',1pg13.5,/)

            WRITE(jdr,'(100a1)') ('-',i=1,70)

          case default
        end select

!-----------------------------------------------------------------------
        close (22)
        IF(loadingPro) RETURN

!!   call Report(24000)

    END subroutine Linfg1out

!#######################################################################


    SUBROUTINE LsqCoxGWM(x,covy1,nc,nr,n,y,Uy,r,A,ok)

!  This subroutine calculates fitting parameters y for a fitting function
!  which is linear in the measured values x, usually count rates. It is
!  possible that the measured values are correlated, which requires a
!  non-diagonal covariance matrix covy1.
!  The partial derivatives of the fitting function with respect to the
!  fitting parameters are elements of a matrix A.
!
!  The matrix algebra method is taken from then textbook:
!     Klaus Weise u. Wolfgang Wöger: Meßunsicherheit und Meßdatenauswertung.
!     Verlag Wiley-VCH Weinheim,1999,
!     S. 200 oben (Abschnitt 5.4.2 Lineare Kurvenanpassung)
!
!  For the necessary matrix algebra, if not directly available from the Fortan
!  compiler, routines are used which are taken from the textbook
!
!   Datan-Library (Fortran) from:
!   Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und Computerprogrammen;
!   4. Auflage. Spektrum, Akademischer Verlag, Heidelberg-Berlin. In German.
!   This text book is also available in an English version.
!
!  25.10.2005 Günter Kanisch, BFAFi Hamburg, Institut für Fischereiökologie

!  Note about matrices in Fortran:
!  the 1st index counts the rows, the 2nd index counts the columns of a matrix.

!--------------------------------------------------------------------------

        use Brandt,      only: mtxchi
        use UR_params,   only: rn,ZERO,ONE

        implicit none

        integer   , INTENT(IN)      :: n             ! number of measured values (x values, <= nc)
        integer   , INTENT(IN)      :: nc            ! physical dim of covy1
        real(rn), INTENT(IN)        :: x(n)          ! vector of x values
        real(rn), INTENT(IN)        :: covy1(nc,nc)  ! covariance matrix of the x values
        integer   , INTENT(IN)      :: nr            ! number of parameters to be fitted
        real(rn), INTENT(OUT)       :: y(nr)         ! vector of parameters to be fitted
        real(rn), INTENT(OUT)       :: Uy(nr,nr)     ! covariance matrix of the fitted parameters
        real(rn), INTENT(OUT)       :: r             ! value of the minimum function (chisq)
        real(rn), INTENT(OUT)       :: A(nc,nr)      ! (n x r) design matrix A (of partial derivatives)

        LOGICAL, INTENT(OUT)        :: ok

        real(rn),allocatable  :: cs(:),xh(:)
        real(rn),allocatable  :: Ux(:,:)
        !-----------------------------------------------------------------------

        allocate(cs(n), xh(nr), Ux(n,n))

        ! Prepare the design matrix A (amt), dimension (n x mfit):
        ! It will be shortened by contributions of those parameters which shall not be fitted
        A = ZERO
        A(1:n,1) = ONE
        Ux(1:n,1:n) = covy1(1:n,1:n)  ! use the copy Ux of covy1

        ! invert the matrix Ux (covariance matrix between x values), with Cholesky decomposition,
        CALL mtxchi(Ux)          ! Ux now contains its inverse

        Uy = matmul(transpose(A),matmul(Ux,A))      ! Uy still needs to be inverted
        if(nr == 1) then
            Uy(1,1) = ONE/Uy(1,1)
        else
            CALL mtxchi(Uy)  ! this is now the covariance matrix of the output qunatities (fit parameters)
        end if

        y = matmul(Uy, matmul(transpose(A), matmul(Ux,x)))    ! this is now the vector of fit parameters

        ! Calulate  r=Chisqr-min:
        cs = Matmul(Ux,x)
        xh = Matmul(Transpose(A),cs)

        r = dot_product(x,cs)
        r = r - dot_product(y, xh)

        ok = .TRUE.

    END SUBROUTINE LsqCoxGWM

!#######################################################################

end module LF1G
