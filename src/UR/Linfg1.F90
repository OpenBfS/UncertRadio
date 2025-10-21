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
module lf1g
    use UR_types

contains

    ! Linfg1Ausf
    ! Linfg1
    ! Linfg1out
    ! LsqCoxGWM

!#######################################################################

    subroutine Linfg1Ausf(mode, akt, SDakt)

        use, intrinsic :: iso_c_binding
        USE UR_Gleich_globals,  only: kgspk1,loadingpro,Messwert,Stdunc,stduncsv,MesswertSV, &
                                      ngrs,ncov
        USE UR_Linft,           only: fpa,fpaSV,numd
        use Rout,               only: WTreeViewPutDoubleCell,WDPutEntryString, &
                                      WDPutEntryDouble,WTreeViewPutDoubleCell,pending_events
        use Top,                only: WrStatusbar
        use UR_DLIM,            only: iteration_on
        use translation_module, only: T => get_translation

        implicit none

        integer   ,intent(in)  :: mode        !  1: do the evaluation:
                                              !  2: do the evaluation, with output to the file linfout.txt
        real(rn), intent(out)  :: akt         ! activity
        real(rn), intent(out)  :: SDakt       ! its standard uncertainty

        integer   , parameter  :: mag = 1

        !-----------------------------------------------------------------------
        call WrStatusBar(4, T('Calculating') // '....' )
        if(.not.loadingPro) call pending_events()              !xx
        call Linfg1(akt,SDakt)
        fpaSv(1:mag) = fpa(1:mag)

        IF(mode == 2) then
            call WrStatusBar(4, T('Gamma evaluations:  view/save/print') )
            call Linfg1out()
        end if

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

        USE UR_Gleich_globals,     only: missingval,ncov,ngrs,upropa_on,kpoint,covarval,Messwert,StdUnc, &
                                 kableitnum,kgspk1
        USE UR_Linft,      only: fpa,sfpa,chisq,Chisqr,numd,mfit
        USE fparser,       ONLY: evalf, EvalErrMsg
        USE UR_Perror
        USE UR_Gspk1Fit,   only: ecorruse,gspk_chisqr,gspk_qval,gspk_free,gspk_sigext, &
                                 gspk_sigint,gspk_xmit,mwtyp,wmextsd,SDGNetRate,gnetrate,varadd_rn,sdeffi, &
                                 pgamm,sdpgamm,fatt,sdfatt,fcoinsu,SDfcoinsu,aktnz,sdaktnz,effi, &
                                 guse,SDaktnzSV,SDaktnzMV
        USE UR_DLIM,       ONLY: iteration_on,limit_typ
        USE ur_general_globals,  ONLY: MCsim_on
        use Brandt,        only: gincgm
        use UR_params,     only: ZERO,ONE,TWO,EPS1MIN
        use Rout,          only: WTreeViewGetDoubleArray

        use file_io,       only: logger
        implicit none

        real(rn), intent(out) :: akt,SDakt        ! activity and its standard uncertainty

        integer, parameter    :: mag = 1

        integer           :: i,npts,nhh,nck,k1,k2

        logical           :: ok
        real(rn)          :: zfact, cov
        real(rn)          :: phi,urelphi2(numd/5),t2
        real(rn)          :: gwcovy(numd/5,numd/5),gwx(1),gwcx(1,1),gwr,gwa(numd/5,1)
        real(rn)          :: rho(numd/5,numd/5),gwpa(numd/5),u2max
        real(rn)          :: covt,psi(numd/5),wmx,uwmx,phix(numd/5)
        integer           :: gwlist(1),n1,n2,nr,kqt,i_arr(numd/5),nhh_arr(numd/5)
        character(len=256) :: log_str
        logical           :: prout
        !-----------------------------------------------------------------------
        prout = .false.
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

                write(log_str, '(*(g0))') 'Linfg1: Gnetrate,Effi,pgamm,fatt,fcoinsu:', sngl(GnetRate(i)), &
                    sngl(effi(i)),sngl(pgamm(i)),sngl(fatt(i)),sngl(fcoinsu(i))
                call logger(66, log_str)
                write(log_str, '(*(g0))') 'Linfg1: associated SD''s dazu:',sngl(SDGnetRate(i)),sngl(SDeffi(i)),  &
                    sngl(SDpgamm(i)),sngl(SDfatt(i)),sngl(SDfcoinsu(i))
                call logger(66, log_str)
                write(log_str, '(*(g0))') 't2=',sngl(t2),'   varadd_Rn=',sngl(varadd_Rn(i)),'   phi=',sngl(phi),  &
                    '  urelphi^2=',sngl(urelphi2)
                call logger(66, log_str)
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

            gspk_sigint = SQRT(ONE / gspk_sigint)
            gspk_xmit = gspk_xmit * gspk_sigint**TWO
            wmx = ZERO
            uwmx = ZERO
            psi(i_arr) = ONE/SDAktnz(i_arr)**TWO * gspk_sigint**TWO
            wmx = sum(psi(i_arr)*aktnz(i_arr))
            uwmx = sum(psi(i_arr)*TWO*SDaktnz(i_arr)**TWO)

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

            IF (.not.upropa_on .and. .not.iteration_on .and. .not. MCSim_on)  then

                write(log_str, '(3(A, F12.4))') 'LinFG1: Weighted mean:  cov=', cov,  &
                               '  covarval(1)=', covarval(1), &
                               ' gspk_sigint=', gspk_sigint
                call logger(66, log_str)
            end if
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

            IF(.not. upropa_on .and. .not.iteration_on .and. .not.MCsim_on) then
                write(log_str, '(*(g0))') 'Least-sq. weighted mean: ',sngl(gwx(1)),  &
                    '   uncertainty   =',sngl(SQRT(gwcx(1,1))),'  Chisqr=',sngl(gwr/real(MAX(1,Numd/5-1),rn))
                call logger(66, log_str)
                write(log_str, '(*(g0))') 'WMean/LSQ w. mean      : ',sngl(akt/gwx(1)),'   uncert./uncLSQ=',sngl(SDakt/SQRT(gwcx(1,1))), &
                    '  Chisqr=',sngl(gspk_chisqr),'  covarval(1)=',sngl(covarval(1))
                call logger(66, log_str)
            end if

            akt = gwx(1)
            gspk_xmit = akt
            SDakt = SQRT(gwcx(1,1))
            gspk_sigint = SDAkt
            gspk_free = real(numd/5 - 1,rn)
            gspk_chisqr = gwr/MAX(ONE,gspk_free)

        end select

        IF(prout) THEN
            write(log_str, '(*(g0))') 'Linfg1: akt=',sngl(akt),'  rel.U.% =',sngl(SDakt/akt*100._rn),'  SDakt=',sngl(SDakt)
            call logger(66, log_str)
            write(log_str, '(*(g0))') 'aktnz: ',(sngl(aktnz(i)),i=1,npts)
            call logger(66, log_str)
            write(log_str, '(*(g0))') 'SDaktnz  : ',(sngl(SDaktnz(i)),i=1,npts)
            call logger(66, log_str)
            write(log_str, '(*(g0))') 'SDaktnz% : ',(sngl(SDaktnz(i)/aktnz(i)*100._rn),i=1,npts)
            call logger(66, log_str)
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

        USE UR_Gleich_globals, only: loadingpro
        USE UR_Linft,          only: numd
        USE UR_Gspk1Fit,       only: fbt,gspk_chisqr,gspk_free,gspk_qval,gspk_sigint,gspk_sigext,gspk_xmit, &
                                     mwtyp,guse,erg,gnetrate,effi,pgamm,fatt,fcoinsu,sdgnetrate,sdeffi,sdfatt, &
                                     sdfcoinsu,aktnz,sdpgamm,sdaktnz
        use chf,                only: flfu
        use translation_module, only: T => get_translation
        use file_io,            only: logger
        use ur_general_globals, only: batf,batest_user,bat_serial,fname

        implicit none

        integer            :: i
        character(len=256) :: text
        character(len=124) :: str1, headline
        character(len=32)  :: cc1,cc2
        character(len=16)  :: cspec
        !-----------------------------------------------------------------------

        if(batf .or. batest_user .or. bat_serial) then
            call logger(22, "Project:  " // trim(fname))
        else
            call logger(22, "Project:  " // trim(fname), new=.true.)
        end if

        call logger(22, trim(mwtyp)// ':')

        write(text,'(100A1)') ('-', i=1,70)
        call logger(22, text)

        write(text,'(A,3x,F6.3)') T('(1 + b/2L) equivalent factor for Compton BG rate:'), FBT
        call logger(22, text)

        write(text,'(A,3x,A)') T('Individual peak data:') // new_line('A'),  &
                               T('(pgamm*fcoin is a measure for the importance of the line!)')
        call logger(22, text)
        call logger(22, ' ')

        write(text, '(*(A))') &
              ' i  E          PNRate    epsPeak     pgamm     fatt      fcoin  (pgamm*fcoin)' // new_line('A'),&
              '    keV        cps' // new_line('A'),&
              '----------------------------------------------------------------------------------'
        call logger(22, text)

        do i=1,numd/5
            IF(guse(i) == 0) CYCLE

            cspec = T('Values')

            write(text,'(i2,2x,f7.2,2x,es10.3,2x,f9.6,3x,f8.6,2x,f6.4,2x,f8.4,1x,f8.4,1x,a)') &
                i,erg(i),GNetRate(i), effi(i),pgamm(i),  &
                fatt(i),fcoinsu(i),pgamm(i)*fcoinsu(i),TRIM(cspec)
            call logger(22, text)

            cspec = 'u_rels in %'
            write(text,'(11x,2x,f6.2,4x,2x,f9.6,3x,f8.6,2x,f6.4,2x,f8.4,1x,8x,1x,a)') &
                SDGnetRate(i)*100./GNetRate(i), sdeffi(i)*100./effi(i), &
                sdpgamm(i)*100./pgamm(i),sdfatt(i)*100./fatt(i), &
                sdfcoinsu(i)*100./fcoinsu(i),TRIM(cspec)
            call logger(22, text)
        end do
        call logger(22, ' ')

        headline = T('Results from individual peak activities:')
        write(text,'(A)') trim(headline)
        call logger(22, text)
        call logger(22, ' ')
        write(text,'(3X,A)') 'A(i) = PeakNetRate(i) * (fatt(i) * fcoin(i)) / (epsPeak(i) * pgamm(i))'
        call logger(22, text)
        call logger(22, ' ')

        write(text,'(A)') ' i    E(keV)    ' // T('Activity (Bq)') // '    ' // T('rel.StdDev (%)')
        call logger(22, text)
        write(text, '(A)') repeat('-', 50)
        call logger(22, text)

        do i=1,numd/5
            if(guse(i) == 0) cycle
            write(text,'(i2,3x,f7.2, 3x, es11.4,6x, f6.2)') i, erg(i), aktnz(i), SDaktnz(i)/aktnz(i)*100.
            call logger(22, text)
        end do
        call logger(22, ' ')

        select case (mwtyp)
          case ('WeiMean')
            str1 = T('Evaluation of the weighted mean:')
            cc1 = T('(Bayes compliant)')
            cc2 = T('(not Bayes compliant)')

           ! The introduction of the T30 format element requires each line to be output by a separate statement
            write(text, '(a,T30," = ",1pg13.5)') T("weighted mean"), gspk_xmit
            call logger(22, text)

            write(text, '(a,T30,a,1pg13.5," (",f6.2," %) ",a)') &
                        T("int. std. dev. of the mean")," = ", gspk_sigint, (gspk_sigint/gspk_xmit*100.), cc1
            call logger(22, text)

            if(gspk_free > 0.) then
                write(text, '(a,T30," = ",1pg13.5," (",f6.2," %) ",a)') &
                            T("ext. std. dev. of the mean"), gspk_sigext, (gspk_sigext/gspk_xmit*100.), cc2
                call logger(22, text)

                write(text, '(a,T30," = ",1pg13.5)') &
                            T("Chi-square = test value T"), gspk_chisqr*gspk_free
                call logger(22, text)

                write(text, '(a,T30," = ",1pg13.5)') T("reduced Chi-square"), gspk_chisqr
                call logger(22, text)

                write(text, '(a,T30," = ",f9.5," %")') T("significance (Chi-square > T)"), gspk_qval
                call logger(22, text)
            end if

            IF(gspk_free > 1.) then!
                write(text,'(A)') T('Note: only the internal standard deviation will be used hereafter!')!
                call logger(22, text)!
            end if!

            write(text,'(100a1)') ('-',i=1,70)!
            call logger(22, text)!
          case ('LSQMean')!
            str1 = T('Evaluation of the weighted mean by least-squares:')!
            cc1 = T('(Bayes compliant)')

            write(text, '(a,T30," = ",1pg13.5)') T("weighted mean"), gspk_xmit
            call logger(22, text)

            write(text, '(a,T30,a,1pg13.5," (",f6.2," %) ",a)') &
                       T("int. std. dev. of the mean"), " = ",gspk_sigint, (gspk_sigint/gspk_xmit*100.),cc1
            call logger(22, text)

            if(gspk_free > 0.) then
                write(text, '(a,T30," = ",1pg13.5)') T("reduced Chi-square"), gspk_chisqr
                call logger(22, text)
            end if

            write(text,'(100a1)') ('-',i=1,70)
            call logger(22, text)

          case default
        end select

        if (batf .or. batest_user .or. bat_serial) then
            call logger(22, ' ')
        else
            call logger(22, ' ', close=.true.)
        end if

        !------------------------------------------------------------------------------------------!
        if(loadingPro) return

    end subroutine Linfg1out

!#######################################################################


    subroutine LsqCoxGWM(x,covy1,nc,nr,n,y,Uy,r,A,ok)

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

        implicit none

        integer   , INTENT(IN) :: n             ! number of measured values (x values, <= nc)
        integer   , INTENT(IN) :: nc            ! physical dim of covy1
        real(rn), INTENT(IN)   :: x(n)          ! vector of x values
        real(rn), INTENT(IN)   :: covy1(nc,nc)  ! covariance matrix of the x values
        integer   , INTENT(IN) :: nr            ! number of parameters to be fitted
        real(rn), INTENT(OUT)  :: y(nr)         ! vector of parameters to be fitted
        real(rn), INTENT(OUT)  :: Uy(nr,nr)     ! covariance matrix of the fitted parameters
        real(rn), INTENT(OUT)  :: r             ! value of the minimum function (chisq)
        real(rn), INTENT(OUT)  :: A(nc,nr)      ! (n x r) design matrix A (of partial derivatives)

        LOGICAL, INTENT(OUT)   :: ok

        real(rn),allocatable  :: cs(:),xh(:)
        real(rn),allocatable  :: Ux(:,:)
        !-----------------------------------------------------------------------

        allocate(cs(n), xh(nr), Ux(n,n))

        ! Prepare the design matrix A (amt), dimension (n x mfit):
        ! It will be shortened by contributions of those parameters which shall not be fitted
        A = 0.0_rn
        A(1:n,1) = 1.0_rn
        Ux(1:n,1:n) = covy1(1:n,1:n)  ! use the copy Ux of covy1

        ! invert the matrix Ux (covariance matrix between x values), with Cholesky decomposition,
        CALL mtxchi(Ux)          ! Ux now contains its inverse

        Uy = matmul(transpose(A),matmul(Ux,A))      ! Uy still needs to be inverted
        if(nr == 1) then
            Uy(1,1) = 1.0_rn/Uy(1,1)
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
