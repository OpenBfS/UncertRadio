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
module PSaveCSV

contains


!#######################################################################

    SUBROUTINE ProSave_CSV

        ! Saves the project to a project file of format .CSV.
        ! See chapter 3.6 "Structure of the project file" of the UncertRadio
        !    CHM Help file for more information.
        !
        ! For saving, all data values are to be read from the various dialog fields!
        ! Called by ProSave

        !     Copyright (C) 2014-2024  Günter Kanisch

        use, intrinsic :: iso_c_binding, only: c_ptr,c_null_ptr,c_null_char
        use UR_params, only: EPS1MIN, ZERO
        USE UR_Variables
        USE UR_Gleich
        USE UR_DLIM
        USE UR_Linft
        USE UR_Gspk1Fit
        use Rout,               only: UpdateProName,WDGetTextviewString,WDGetEntryDouble,    &
                                      WTreeViewGetStrArray,WTreeViewGetDoubleArray,        &
                                      WTreeViewGetComboArray,WDGetEntryString,WDGetComboboxAct, &
                                      WDGetSelRadio,WDGetCheckButton,WTreeViewGetCheckArray,  &
                                      WDGetSelRadioMenu
        use Top,                only: WrStatusBar,CharModA1
        use RdSubs,             only: WandelDPkt,writeMDvec
        use RG,                 only: modify_Formeltext
        use CHF,                only: flfu
        use translation_module, only: T => get_translation

        implicit none

        integer              :: k,i,i2,imenu1,kxy,kmwtyp,m1,kk,maxi
        integer              :: error_str_conv
        CHARACTER(LEN=1000)  :: text
        CHARACTER(LEN=2)     :: cdm
        CHARACTER(30)        :: zahl

        character(len=1)     :: ctr
        character(len=len(fname)+ 32) :: fname_tmp

        !------------------------------------------------------------------------------------------
        ctr = sListSeparator

        if(len_trim(fname) == 0) return

        call UpdateProName(fname)

        fname_tmp = flfu(fname, error_str_conv)
        if (error_str_conv > 0) write(*,*) 'Warning, could not convert file_name ' // &
                                           'to local encoding: ' // trim(fname_tmp)

        open (25, file=trim(fname_tmp), status='unknown')

        call WDGetTextviewString('textview1', titeltext)
        do i=size(Titeltext),1,-1
            if(len_trim(Titeltext(i)%s) > 0) then
                call Charmoda1(Titeltext,i)
                exit
            end if
        end do

        call WDGetTextviewString('textview2', formeltext)
        ! remove empty equation lines at the end:
        do i=size(Formeltext),1,-1
            if(len_trim(Formeltext(i)%s) > 0) then
                call Charmoda1(Formeltext,i)
                exit
            end if
        end do

        if(FitDecay) then
            if(FitDecay) THEN
                call WDGetTextviewString('textviewModelEQ', FormeltextFit)
                do i=size(FormeltextFit),1,-1
                    if(len_trim(FormeltextFit(i)%s) > 0) then
                        call Charmoda1(FormeltextFit,i)
                        exit
                    end if
                end do

                call WDGetComboboxAct('comboboxA1', ifit(1))
                call WDGetComboboxAct('comboboxA2', ifit(2))
                call WDGetComboboxAct('comboboxA3', ifit(3))

                call WDGetCheckButton('checkbuttonWFit', nwei)
                call WDGetCheckButton('checkbuttonCovZR', nkovzr)
                call WDGetCheckButton('checkbuttonAllm', ndefall)

                call WDGetSelRadio('radiobuttonNLSQ', kfitmeth)
                kfitmeth = kfitmeth - 1
                call WDGetComboboxAct('comboboxtextNCH', nchannels)

            end if
        end if

        i2 = 1
        do i=LEN_TRIM(fname),1,-1
            if(fname(i:i) == dir_sep) then
                i2 = i + 1
                exit
            end if
        end do

        write(25,'(a,a1,a,15a1)') 'Project:',ctr,trim(fname(i2:)),(ctr,i=1,15)

        maxi = 1
        do i=size(Titeltext),1,-1
            ! remove the last "empty" records of Titeltext:
            if(index(Titeltext(i)%s,char(32)//char(10)) == 1) cycle
            if(len_trim(Titeltext(i)%s) == 0) cycle
            maxi = i
            exit
        end do

        write(25,'(a,15a1)') 'Titeltext:',(ctr,i=1,15)
        m1 = 0
        k = 0

        do k=1,maxi
            if(INDEX(titeltext(k)%s,ctr) > 0) THEN
                write(25,'(i3,a1,a1,a,a1,14a1)') k,ctr,'"',titeltext(k)%s,'"',(ctr,i=1,14)
            else
                write(25,'(i3,a1,a,14a1)') k,ctr,titeltext(k)%s,(ctr,i=1,14)
            end if
        end do

        write(25,'(a,14a1)') 'Formeltext:',(ctr,i=1,14)
        m1 = 0
        k = 0

        do k=1,size(Formeltext)
            if(k == size(Formeltext) .and. len_trim(Formeltext(k)%s) == 0) exit
            write(25,'(i3,a1,a,14a1)') k,ctr,Formeltext(k)%s,(ctr,i=1,14)
        end do

        if(FitDecay) then
            write(25,'(a,15a1)') 'FormeltextFit:',(ctr,i=1,15)

            do k=1,size(FormeltextFit)
                if(k == size(FormeltextFit) .and. len_trim(FormeltextFit(k)%s) == 0) exit
                write(25,'(i3,a1,a,14a1)') k,ctr,FormeltextFit(k)%s,(ctr,i=1,14)
            end do
        end if

        write(25,'(a,15a1)') 'Optionen:',(ctr,i=1,15)
        write(zahl,'(f8.6)') real(kalpha,8)
        call WandelDPkt(zahl,2)
        write(25,'(a1,a,a1,a,13a1)') ctr,'kalpha',ctr,trim(zahl),(ctr,i=1,13)
        write(zahl,'(f8.6)') real(kbeta,8)
        call WandelDPkt(zahl,2)
        write(25,'(a1,a,a1,a,13a1)') ctr,'kbeta',ctr,trim(zahl),(ctr,i=1,13)

        write(zahl,'(f8.2)') real(coverf,8)
        call WandelDPkt(zahl,2)
        write(25,'(a1,a,a1,a,a1,a,12a1)') ctr,'coverf',ctr,trim(zahl),ctr, &
                                          T("coverage factor for uncertainty (output)"), &
                                          (ctr,i=1,12)

        write(zahl,'(f8.2)') real(max(ZERO,coverin),8)
        call WandelDPkt(zahl,2)
        write(25,'(a1,a,a1,a,a1,a,12a1)') ctr,'coverin',ctr,trim(zahl),ctr, &
                                          T("coverage factor for uncertainty (input)"), &
                                          (ctr,i=1,12)

        write(zahl,'(f8.6)') real(W1minusG,8)
        call WandelDPkt(zahl,2)
        write(25,'(a1,a,a1,a,a1,a,12a1)') ctr,'1-gamma',ctr,trim(zahl),ctr, &
                                          T('Probability for confidence'),(ctr,i=1,12)

        call WDGetEntryDouble('entryOptGamDistAdd', GamDistAdd)
        write(zahl,'(f8.6)') real(GamDistAdd,8)
        call WandelDPkt(zahl,2)
        write(25,'(a1,a,a1,a,a1,a,12a1)') ctr,'GamDistAdd',ctr,trim(zahl),ctr, &
                                          T('Gamma-distribution parameter'),(ctr,i=1,12)


        call WDGetSelRadiomenu('MT_NegLin',k)
        if(k == 1) then
            write(zahl,'(a)') 'PosLin'
            write(25,'(a1,a,a1,a,a1,a,12a1)') ctr,'ModelType',ctr,trim(zahl),ctr, &
                                              T('posit. linear, with DL', .true.),(ctr,i=1,12)
        end if
        if(k == 2) then
            write(zahl,'(a)') 'GUM_restricted'

            write(25,'(a1,a,a1,a,a1,a,12a1)') ctr,'ModelType',ctr,trim(zahl),ctr, &
                                              T('only GUM, without DL', .true.),(ctr,i=1,12)
        end if
        if(k == 3) then
            write(zahl,'(a)') 'NegLin'
            write(25,'(a1,a,a1,a,a1,a,12a1)') ctr,'ModelType',ctr,trim(zahl),ctr, &
                                              T('negat. linear, with DL', .true.),(ctr,i=1,12)
        end if

        write(25,'(a,15a1)') 'Projektparameter:',(ctr,i=1,15)

        write(25,'(a1,a,a1,i1,a1,a,12a1)') ctr,'nchs',ctr,nchannels,ctr, T('number of counting channels'),(ctr,i=1,12)
        write(25,'(a1,a,a1,i1,a1,a,12a1)') ctr,'nEGr',ctr,knumEGr,ctr, T('number of output quantities'),(ctr,i=1,12)
        write(25,'(a1,a,a1,i3,a1,a,12a1)') ctr,'ngrs',ctr,ngrs,ctr, T('number of quantities'),(ctr,i=1,12)
        write(25,'(a1,a,a1,i3,a1,a,12a1)') ctr,'nab',ctr,nab,ctr, T('number of dependent quantities'),(ctr,i=1,12)
        write(25,'(a1,a,a1,i3,a1,a,12a1)') ctr,'nmu',ctr,nmu,ctr, T('number of independent quantities'),(ctr,i=1,12)
        write(25,'(a1,a,a1,3(i3,a1),a,9a1)') ctr,'knetto',ctr,(knetto(i),ctr,i=1,3), T('No of net countrate symbol'),(ctr,i=1,9)
        write(25,'(a1,a,a1,3(i3,a1),a,9a1)') ctr,'kbrutto',ctr,(kbrutto(i),ctr,i=1,3), T('No of gross countrate symbol'),(ctr,i=1,9)


        write(25,'(a1,a,a1,4(i3,a1),a,9a1)') ctr,'BinPoi:',ctr,ip_binom,ctr,kbgv_binom,ctr,itm_binom,ctr,ilam_binom,ctr, &
            'index of p, R0, tm, lam (ip_binom, kbgv_binom,itm_binom,ilam_binom)',(ctr,i=1,9)


        write(25,'(15a1)') (ctr,i=1,15)

        call WTreeViewGetStrArray('treeview1', 2, ngrs, symbole)
        call WTreeViewGetStrArray('treeview1', 3, ngrs, symtyp)
        call WTreeViewGetStrArray('treeview1', 4, ngrs, einheit)
        call WTreeViewGetStrArray('treeview1', 5, ngrs, bedeutung)

        do i=1,ngrs
            symbole(i)%s   = ADJUSTL(symbole(i)%s)
            symtyp(i)%s    = ADJUSTL(symtyp(i)%s)
            einheit(i)%s   = ADJUSTL(einheit(i)%s)
            bedeutung(i)%s = ADJUSTL(bedeutung(i)%s)
        end do

        write(25,'(a,15a1)') 'Symbole:',(ctr,i=1,15)
        do k=1,ngrs
            write(25,'(i3,a1,4(a,a1),10a1)') k,ctr,Symbole(k)%s,ctr,symtyp(k)%s,ctr,einheit(k)%s, &
                ctr,bedeutung(k)%s,(ctr,i=1,10)
        end do

        write(25,'(a,15a1)') 'Werte, Unsicherheiten:',(ctr,i=1,15)

        call WTreeViewGetDoubleArray('treeview2', 5, ngrs, Messwert)
        call WTreeViewGetComboArray('treeview2', 6, ngrs, IVTL)
        call WTreeViewGetStrArray('treeview2', 7, ngrs, SDFormel)
        call WTreeViewGetDoubleArray('treeview2', 8, ngrs, SDWert)
        call WTreeViewGetDoubleArray('treeview2', 9, ngrs, HBreite)
        call WTreeViewGetComboArray('treeview2', 10, ngrs, IAR)
        call WTreeViewGetDoubleArray('treeview2', 11, ngrs, StdUnc)

        do i=1,ngrs
            sdformel(i)%s   = ADJUSTL(sdformel(i)%s)
        end do

        do k=1,ngrs

            if(k > nab .and. coverin > ZERO) then
                if(abs(Stdunc(k) - missingval) > EPS1MIN) Stdunc(k) = StdUnc(k)*coverin
                if(abs(SDwert(k) - missingval) > EPS1MIN) SDwert(k) = SDwert(k)*coverin
                if(abs(HBreite(k) - missingval) > EPS1MIN) HBreite(k) = HBreite(k)*coverin
            end if

            write(text,'(i3,a1,a,a1,es23.15e2,a1,i1,a1,  a,a1,es23.15e2,a1,es23.15e2,a1,  i1,a1,es23.15e2,a1, 6a1)')  &
                k,ctr,Symbole(k)%s,ctr,real(Messwert(k),8),ctr,IVTL(k),ctr,  &
                sdformel(k)%s,ctr,real(sdwert(k),8),ctr,real(HBreite(k),8),ctr, IAR(k),ctr,real(StdUnc(k),8),ctr,(ctr,i=1,6)
            call WandelDPkt(text,2)
            write(25,'(a)') trim(text)
        end do

        write(25,'(a,15a1)') 'CovarGrid:',(ctr,i=1,15)

        if(ncov > 0) THEN
            call WTreeViewGetComboArray('treeview3', 2, ncov, ISymbA)
            call WTreeViewGetComboArray('treeview3', 3, ncov, ISymbB)
            call WTreeViewGetComboArray('treeview3', 4, ncov, icovtyp)
            call WTreeViewGetStrArray('treeview3', 5, ncov, CVFormel)
            call WTreeViewGetDoubleArray('treeview3', 6, ncov, Covarval)

            do i=1,ncov
                call WandelDPkt(CVFormel(i)%s,2)
                CVformel(i)%s   = ADJUSTL(CVformel(i)%s)
            end do
            do k=1,ncov
                write(zahl,*) real(CovarVal(k),8)
                call WandelDPkt(zahl,2)
                write(text,'(i2,a1,i3,a1,i3,a1,i1,a1,a,a1,a,a1,9a1)') k,ctr,ISymbA(k)+1,ctr,ISymbB(k)+1,ctr,icovtyp(k),ctr,  &
                    CVformel(k)%s,ctr,zahl,ctr,(ctr,i=1,9)
                write(25,'(a)') trim(text)
            end do
        END if

        if(FitDecay) THEN

            call WDGetEntryString('entrySeparation', CFaelldatum)
            call WDGetComboboxAct('comboboxtextbase', imenu1)

            call WTreeViewGetStrArray('treeview5', 2, numd, CStartzeit)
            call WTreeViewGetDoubleArray('treeview5', 3, numd, dmesszeit)
            call WTreeViewGetDoubleArray('treeview5', 4, numd, dbimpulse)
            call WTreeViewGetDoubleArray('treeview5', 5, numd, dbzrate)
            call WTreeViewGetDoubleArray('treeview5', 6, numd, sdbzrate)
            call WTreeViewGetDoubleArray('treeview5', 7, numd, d0messzeit)
            call WTreeViewGetDoubleArray('treeview5', 8, numd, d0impulse)
            call WTreeViewGetDoubleArray('treeview5', 9, numd, d0zrate)
            call WTreeViewGetDoubleArray('treeview5', 10, numd, sd0zrate)
            call WTreeViewGetDoubleArray('treeview5', 11, numd, dnetrate)
            call WTreeViewGetDoubleArray('treeview5', 12, numd, sdnetrate)

            write(25,'(8(a,a1),6a1)') 'AbklingGrid:',ctr,'fitp1?',ctr,'fitp2?',ctr,'fitp3?',ctr,'weighted f.',ctr, &
                'use covs?',ctr,'fitmeth?',ctr,'time base(s/m):',ctr,(ctr,i=1,6)

            write(25,'(a,a1,8(i1,a1),6a1)') 'ModPar',ctr,(ifit(i),ctr,i=1,3),nwei,ctr,nkovzr,ctr,kfitmeth,ctr,  &
                imenu1,ctr,ndefall,ctr,(ctr,i=1,6)
            write(25,'(a,a1,a,a1,13a1)') 'Trenn-Datum',ctr,trim(CFaelldatum),ctr,(ctr,i=1,13)

            do k=1,numd
                write(text,'(i3,a1,a20,a1,10(es23.15e2,a1),3a1)') k,ctr,trim(CStartzeit(k)%s),ctr,real(dmesszeit(k),8),ctr,  &
                    real(dbimpulse(k),8),ctr,  &
                    real(dbzrate(k),8),ctr,real(sdbzrate(k),8),ctr,real(d0messzeit(k),8),ctr,     &
                    real(d0impulse(k),8),ctr,real(d0zrate(k),8),ctr,real(sd0zrate(k),8),ctr,      &
                    real(dnetrate(k),8),ctr,real(sdnetrate(k),8),ctr,(ctr,i=1,3)
                call WandelDPkt(text,2)
                text(5:24) = CStartzeit(k)%s
                write(25,'(a)') trim(text)
            end do
        END if

        if(Gamspk1_Fit) THEN
            write(25,'(a,15a1)') 'Gamspk1-Grid:',(ctr,i=1,15)
            call WDGetSelRadio('radiobuttonG1', unitRadio(1))
            call WDGetSelRadio('radiobuttonG5', unitRadio(2))
            call WDGetSelRadio('radiobuttonG9', unitRadio(3))
            call WDGetSelRadio('radiobuttonG11', unitRadio(4))
            call WDGetSelRadio('radiobuttonG13', unitRadio(5))
            write(25,'(a,a1,5(i1,a1),5a1)') 'UnitRadio',ctr,(UnitRadio(i),ctr,i=1,5),(ctr,i=1,5)

            call WDGetComboboxAct('comboboxGMWtyp', kmwtyp)
            write(25,'(a,a1,i2,14a1)') 'MeanTyp',ctr,kmwtyp,(ctr,i=1,14)

            call WDGetEntryDouble('entry_b2LFactor', FBT)
            write(text,'(a,a1,f9.6,14a1)') 'FBT',ctr,real(FBT,8),(ctr,i=1,14)
            call WandelDPkt(text,2)
            write(25,'(a)') trim(text)
            call WDGetCheckButton('checkbuttonGspk1EffiCov', ecorruse)
            write(25,'(a,a1,i1,14a1)') 'EcorrUse',ctr,ecorrUse,(ctr,i=1,14)
            call WDGetCheckButton('checkbuttonMeanOpt', WMextSD)

            kxy = numd/5
            call WTreeViewGetCheckArray('treeview6', 2, kxy, guse)
            call WTreeViewGetDoubleArray('treeview6', 3, kxy, erg)
            call WTreeViewGetDoubleArray('treeview6', 4, kxy, GNetRate)
            call WTreeViewGetDoubleArray('treeview6', 5, kxy, RateCB)
            call WTreeViewGetDoubleArray('treeview6', 6, kxy, RateBG)
            call WTreeViewGetDoubleArray('treeview6', 7, kxy, SDRateBG)
            call WTreeViewGetDoubleArray('treeview6', 8, kxy, effi)
            call WTreeViewGetDoubleArray('treeview6', 9, kxy, SDeffi)
            call WTreeViewGetDoubleArray('treeview6', 10, kxy, pgamm)
            call WTreeViewGetDoubleArray('treeview6', 11, kxy, SDpgamm)
            call WTreeViewGetDoubleArray('treeview6', 12, kxy, fatt)
            call WTreeViewGetDoubleArray('treeview6', 13, kxy, SDfatt)
            call WTreeViewGetDoubleArray('treeview6', 14, kxy, fcoinsu)
            call WTreeViewGetDoubleArray('treeview6', 15, kxy, SDfcoinsu)

            do k=1,kxy
                write(text,'(i3,a1,i1,a1,f7.2,a1,12(es23.15e2,a1) )') k,ctr,guse(k),ctr,real(Erg(k),8),ctr,  &
                    real(GNetRate(k),8),ctr,real(RateCB(k),8),ctr,  &
                    real(RateBG(k),8),ctr,real(SDRateBG(k),8),ctr,  &
                    real(effi(k),8),ctr,real(SDeffi(k),8),ctr,real(pgamm(k),8),ctr,real(SDpgamm(k),8),ctr,     &
                    real(fatt(k),8),ctr,real(SDfatt(k),8),ctr,real(fcoinsu(k),8),ctr,real(SDfcoinsu(k),8),ctr
                call WandelDPkt(text,2)
                write(25,'(a)') trim(text)
            end do
        END if

        if(FitCalCurve) THEN
            call WDGetEntryString('entryDKTitel', cctitle)
            call WDGetComboboxAct('comboboxDKPgrad', kal_Polgrad)
            kal_polgrad = kal_polgrad - 1
            call WTreeViewGetDoubleArray('treeview7', 2, nkalpts, xkalib)
            call WTreeViewGetDoubleArray('treeview7', 3, nkalpts, uxkalib)
            call WTreeViewGetDoubleArray('treeview7', 4, nkalpts, ykalib)
            call WTreeViewGetDoubleArray('treeview7', 5, nkalpts, uykalib)

            write(66,*) ' kal_Polgrad=',kal_Polgrad,'    cctitle=',trim(cctitle)
            kk = 0
            if(use_UfitKal) kk = 1
            write(25,'(5(a,a1),10a1)') 'Kalfit-Grid:',ctr,'nkalpts',ctr,'GradPolynom',ctr,'CCTitle',ctr,'kUseUfit',(ctr,i=1,9)
            write(25,'(a1,2(i3,a1),2(a,a1),12a1)') ctr,nkalpts,ctr,kal_polgrad,ctr,trim(CCtitle),ctr,kk, (ctr,i=1,12)
            do k=1,nkalpts
                write(text,'(i3,a1,4(es23.15e2,a1),10a1)') k,ctr,real(xkalib(k),8),ctr,real(uxkalib(k),8),ctr,real(ykalib(k),8),ctr,  &
                    real(uykalib(k),8),ctr,(ctr,i=1,10)
                call WandelDPkt(text,2)
                write(25,'(a)') trim(text)
            end do

        end if

        if(nvarsMD > 0) then
            write(25,'(a,15a1)') 'Means:',(ctr,i=1,15)
            write(25,'(a,a1,100(i1,a1))')  'Meantyp',ctr,(k_MDtyp(i),ctr,i=1,nvarsMD)
            write(25,'(a,a1,i0,15a1)') 'refmean',ctr,refdataMD,(ctr,i=1,15)
            do k=1,nvarsMD
                call writeMDvec(k,.true.,25,cdm)
            end do
        end if

        close (25)

        call WrStatusBar(3,' ')

    end subroutine ProSave_CSV

!#######################################################################

end module PSaveCSV
