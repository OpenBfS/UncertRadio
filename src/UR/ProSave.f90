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
module PSave

    use PSaveCSV

contains


!#######################################################################

    subroutine prosave

        ! Saves the project to a project file of format .TXP; it calls ProSave_CSV for the
        ! case of a CSV formatted project file.
        ! See chapter 3.6 "Structure of the project file" of the UncertRadio
        !    CHM Help file for more information.
        !
        ! For saving, all data values are to be read from the various dialog fields!

        !     Copyright (C) 2014-2023  Günter Kanisch
        use, intrinsic :: iso_c_binding,       only: c_ptr,c_null_ptr,c_null_char
        use ur_general_globals
        use UR_Gleich_globals
        use ur_dlim
        use ur_linft
        use ur_gspk1fit

        use Top,                  only: CharModA1
        use RdSubs,               only: writeMDvec
        use CHF,                  only: ucase
        use UR_params,            only: EPS1MIN,ZERO
        use RG,                   only: modify_Formeltext
        use CHF,                  only: flfu

        use UR_DecChain,          only: DCList, DCBuildupAtSepar, &
                                        chaincode, DCcommMeasmt, &
                                        DCnuclide, DCsymbT12, &
                                        DCsymbLambda, &
                                        DCsymbEffiA, DCsymbEffiB, &
                                        DCsymbEffiC, DCsymbYield, &
                                        N_nuclides, DChain

        implicit none

        integer             :: k,i,imenu1,kxy,i1,m1,j,kk,nk,maxi                   ! ,kmwtyp
        integer             :: error_str_conv,iv,kc,kim,ksep,nnch
        character(len=2000) :: text                                   ! 12.8.2023
        character(len=len(fname)+ 32) :: fname_tmp

        character(len=2)    :: cdm
        character(len=20)   :: cheader
        logical             :: prot
!-----------------------------------------------------------------------
        prot = .false.
        ! prot = .true.

        text = ucase(fname)
        i1 = len_trim(text)
        if(text(i1-3:i1) == '.CSV') then
            call ProSave_CSV()
            return
        end if

        cheader = 'Choose filename:'
        ! if(len_trim(fname)== 0) then
        !     call FOpen(ifehl, .true., cheader )
        !     if(ifehl == 1) return
        ! end if

        if(len_trim(fname) == 0) return

        ! call UpdateProName(fname)

        fname_tmp = flfu(fname, error_str_conv)
        if (error_str_conv > 0) write(*,*) 'Warning, could not convert file_name ' // &
            'to local encoding: ' // trim(fname_tmp)

        open (25, file=TRIM(fname_tmp), status='unknown')

        ! Call WDGetTextviewString('textview1',Titeltext)
        if(.true. .and. prot) then
            write(66,*) 'PS: GetTiteltext after reworking ====================================='
            do i=1,size(Titeltext)
                write(66,*) trim(Titeltext(i)%s)
            end do
            write(66,*) 'PS: GetTiteltext after reworking ====================================='
        end if

        ! Call WDGetTextviewString('textview2',Formeltext)
        ! remove empty equation lines at the end:
        do i=size(Formeltext),1,-1
            if(len_trim(Formeltext(i)%s) > 0) then
                call Charmoda1(Formeltext,i)
                exit
            end if
        end do

        if(FitDecay) then
            if(FitDecay) then
                ! Call WDGetTextviewString('textviewModelEQ',FormeltextFit)
                do i=size(FormeltextFit),1,-1
                    if(len_trim(FormeltextFit(i)%s) > 0) then
                        call Charmoda1(FormeltextFit,i)
                        exit
                    end if
                end do

                ! call WDGetComboboxAct('comboboxA1', ifit(1))
                ! call WDGetComboboxAct('comboboxA2', ifit(2))
                ! call WDGetComboboxAct('comboboxA3', ifit(3))

                ! call WDGetCheckButton('checkbuttonWFit', nwei)
                ! call WDGetCheckButton('checkbuttonCovZR', nkovzr)
                ! call WDGetCheckButton('checkbuttonAllm', ndefall)

                ! call WDGetSelRadio('radiobuttonNLSQ', kfitmeth)
                ! kfitmeth = kfitmeth - 1
                ! call WDGetComboboxAct('comboboxtextNCH', nchannels)

            end if
            !if(Gamspk1_Fit) then
            !end if
        end if

        maxi = 1
        do i=size(Titeltext),1,-1
            ! remove the last "empty" records of Titeltext:
            if(index(Titeltext(i)%s,char(32)//char(10)) == 1) cycle
            if(len_trim(Titeltext(i)%s) == 0) cycle
            maxi = i
            exit
        end do

        write(25,'(a)') '@Titeltext:'
        do i=1,maxi
            write(25,'(a)') Titeltext(i)%s
        end do
        write(25,'(a)') '@Formeltext:'

        do i=1,size(Formeltext)
            write(25,'(a)') Formeltext(i)%s
        end do

        if(allocated(FormeltextFit)) then
            if(FitDecay .OR. (Gamspk1_Fit .and. len_trim(Formeltextfit(1)%s) > 0) .or.   &
                SumEval_fit ) then
                write(25,'(a)') '@FormeltextFit:'
                do i=1,size(FormeltextFit)
                    write(25,'(a)') FormeltextFit(i)%s
                end do
            end if
        end if

        write(25,'(a)') '@Symbole-GRID:'
        write(25,'(a,i0)') 'nchs=',nchannels
        write(25,'(a,i0)') 'nEGr=',knumEGr
        write(25,'(a,i0)') 'ngrs=',ngrs
        write(25,'(a,i0)') 'nab=',nab
        write(25,'(a,i0)') 'nmu=',nmu

        ! call WTreeViewGetStrArray('treeview1', 2, ngrs, symbole)
        ! call WTreeViewGetStrArray('treeview1', 3, ngrs, symtyp)
        ! call WTreeViewGetStrArray('treeview1', 4, ngrs, einheit)
        ! call WTreeViewGetStrArray('treeview1', 5, ngrs, bedeutung)

        do i=1,ngrs
            symbole(i)%s   = ADJUSTL(symbole(i)%s)
            symtyp(i)%s    = ADJUSTL(symtyp(i)%s)
            einheit(i)%s   = ADJUSTL(einheit(i)%s)
            bedeutung(i)%s = ADJUSTL(bedeutung(i)%s)
        end do

        do k=1,ngrs
            write(25,'(10a)') Symbole(k)%s,' #',symtyp(k)%s,' #',einheit(k)%s, &
                ' #',bedeutung(k)%s,' #'
        end do

        write(25,'(a)') '@Menu1 und Menu2:'
        write(25,'(a,3i3)') 'knetto=',knetto(1),knetto(2),knetto(3)
        write(25,'(a,3i3)') 'kbrutto=',kbrutto(1),kbrutto(2),kbrutto(3)

        write(25,'(a)') '@Unc-Grid:'
        ! call WTreeViewGetDoubleArray('treeview2', 5, ngrs, Messwert)
        ! call WTreeViewGetComboArray('treeview2', 6, ngrs, IVTL)
        ! call WTreeViewGetStrArray('treeview2', 7, ngrs, SDFormel)
        ! call WTreeViewGetDoubleArray('treeview2', 8, ngrs, SDWert)
        ! call WTreeViewGetDoubleArray('treeview2', 9, ngrs, HBreite)
        ! call WTreeViewGetComboArray('treeview2', 10, ngrs, IAR)
        ! call WTreeViewGetDoubleArray('treeview2', 11, ngrs, StdUnc)
        do i=1,ngrs
            sdformel(i)%s   = ADJUSTL(sdformel(i)%s)
        end do

        do k=1,ngrs
            if(k > nab .and. coverin > ZERO) then   !
                if(abs(Stdunc(k) - missingval) > EPS1MIN) Stdunc(k) = StdUnc(k)*coverin
                if(abs(SDwert(k) - missingval) > EPS1MIN) SDwert(k) = SDwert(k)*coverin
                if(abs(HBreite(k) - missingval) > EPS1MIN) HBreite(k) = HBreite(k)*coverin
            end if
            if(k == 3) then
                write(66,*) 'len(sdformel(k)%s)=',len(sdformel(k)%s)
                write(66,*) 'len(text)=',len(text)
            end if

            write(text,'(a,1x,a2,es23.15e2,a2,i3,a2,a,a2,2(es23.15e2,a2),i2,a2,es23.15e2,a2)')    &
                Symbole(k)%s,' #', real(Messwert(k),8),' #',IVTL(k),' #',  &
                sdformel(k)%s,' #',real(sdwert(k),8),' #',real(HBreite(k),8),' #', &
                IAR(k),' #',real(StdUnc(k),8),' #'
            do j=1,20
                m1 = index(text, '  ')
                if(m1 == 0) exit
                text = text(1: m1-1) // trim(text(m1+2:))
            end do
            write(25,'(a)') ADJUSTL(TRIM(text))
        end do

        write(25,'(a)') '@Covar-Grid:'
        if(ncov > 0) then
            ! call WTreeViewGetComboArray('treeview3', 2, ncov, ISymbA)
            ! call WTreeViewGetComboArray('treeview3', 3, ncov, ISymbB)
            ! call WTreeViewGetComboArray('treeview3', 4, ncov, icovtyp)
            ! call WTreeViewGetStrArray('treeview3', 5, ncov, CVFormel)
            ! call WTreeViewGetDoubleArray('treeview3', 6, ncov, CovarVal)

            do i=1,ncov
                CVformel(i)%s   = ADJUSTL(CVformel(i)%s)
            end do
            do k=1,ncov
                ! For compatability with an old UR version, ISymbA andIsymbB
                ! are written here as number increased by +1 :
                write(text,'(3(i3,a2),a,a2,es23.15e2,a2)') ISymbA(k)+1,' #',ISymbB(k)+1,' #',icovtyp(k),' #',  &
                    CVformel(k)%s,' #',real(CovarVal(k),8),' #'
                write(25,'(a)') ADJUSTL(TRIM(text))
            end do
        end if

        IF(DChain) THEN
            ! 19.12.2024  GK         ! added 27.4.2025
            WRITE(25,'(a)') '@DChain:'
            ! call WDGetComboboxAct('ComboboxDCchains',kc)
            write(66,*) 'ComboboxDCchains:  kc=',kc
            i1 = index(DCList(kc)%s,':')
            chaincode%s = trim(adjustL(DCList(kc)%s(1:i1-1)))
            write(25,'(a,a)')'CHName=',chaincode%s

            ! call WDGetComboboxAct('comboboxtextDCNCH', nnch)
            ! call WDGetComboboxAct('DCcheckSepar', ksep)
            ! call WDGetCheckButton('DCcheckVorLam',iv)
            kim = 0
            if(DCcommMeasmt) kim = 1
            write(25,'(a,7i2)') 'Pars=',kc,ksep,iv,nnch,kim,DCBuildupAtSepar,N_nuclides

            i = N_Nuclides
            !if(allocated(nucname)) deallocate(nucname,T12Lam,effiA,effiB,effiC,eta)
            !allocate(nucname(i),T12Lam(i),effiA(i),effiB(i),effiC(i),eta(i))
            ! call WTreeViewGetStrArray('treeview9', 2, N_Nuclides, DCnuclide)
            ! if(iv == 0) call WTreeViewGetStrArray('treeview9', 3, N_nuclides, DCsymbT12)
            ! if(iv == 1) call WTreeViewGetStrArray('treeview9', 3, N_nuclides, DCsymbLambda)
            ! call WTreeViewGetStrArray('treeview9', 4, N_Nuclides, DCsymbEffiA)
            ! call WTreeViewGetStrArray('treeview9', 5, N_Nuclides, DCsymbEffiB)
            ! call WTreeViewGetStrArray('treeview9', 6, N_Nuclides, DCsymbEffiC)
            ! call WTreeViewGetStrArray('treeview9', 7, N_Nuclides, DCsymbYield)

            do i=1,N_nuclides
              if(iv == 0) then
                write(text,'(i0,a,6(a,a))') i,' #',DCnuclide(i)%s,' #',DCsymbT12(i)%s,' #', &
                                           DCsymbEffiA(i)%s,' #', DCsymbEffiB(i)%s,' #', &
                                           DCsymbEffiC(i)%s,' #',DCsymbYield(i)%s,' #'
              else
                write(text,'(i0,a,6(a,a))') i,' #',DCnuclide(i)%s,' #',DCsymbLambda(i)%s,' #', &
                                           DCsymbEffiA(i)%s,' #', DCsymbEffiB(i)%s,' #', &
                                           DCsymbEffiC(i)%s,' #',DCsymbYield(i)%s,' #'
              end if
              write(25,'(a)') trim(text)
            end do
        END IF

        if(FitDecay) then
            write(25,'(a)') '@Abkling-Grid:'
            ! call WDGetEntryString('entrySeparation', CFaelldatum)
            ! call WDGetComboboxAct('comboboxtextbase', imenu1)

            ! call WTreeViewGetStrArray('treeview5', 2, numd, CStartzeit)
            ! call WTreeViewGetDoubleArray('treeview5', 3, numd, dmesszeit)
            ! call WTreeViewGetDoubleArray('treeview5', 4, numd, dbimpulse)
            ! call WTreeViewGetDoubleArray('treeview5', 5, numd, dbzrate)
            ! call WTreeViewGetDoubleArray('treeview5', 6, numd, sdbzrate)
            ! call WTreeViewGetDoubleArray('treeview5', 7, numd, d0messzeit)
            ! call WTreeViewGetDoubleArray('treeview5', 8, numd, d0impulse)
            ! call WTreeViewGetDoubleArray('treeview5', 9, numd, d0zrate)
            ! call WTreeViewGetDoubleArray('treeview5', 10, numd, sd0zrate)
            ! call WTreeViewGetDoubleArray('treeview5', 11, numd, dnetrate)
            ! call WTreeViewGetDoubleArray('treeview5', 12, numd, sdnetrate)

            write(25,'(a,7i2)') 'ModPar=',(ifit(i),i=1,3),nwei,nkovzr,kfitmeth, ndefall
            write(25,'(a)') CFaelldatum
            write(25,*) imenu1

            do k=1,numd
                write(text,'(a,1x,a2,10(es23.15e2,a2))') trim(CStartzeit(k)%s),' #',real(dmesszeit(k),8),' #', &
                    real(dbimpulse(k),8),' #',  &
                    real(dbzrate(k),8),' #',real(sdbzrate(k),8),' #',real(d0messzeit(k),8),' #',     &
                    real(d0impulse(k),8),' #',real(d0zrate(k),8),' #',real(sd0zrate(k),8),' #',      &
                    real(dnetrate(k),8),' #',real(sdnetrate(k),8),' #'
                do j=1,20
                    m1 = index(text, '  ')
                    if(m1 == 0) exit
                    text = text(1: m1-1) //  trim(text(m1+2:))
                end do
                write(25,'(a)') ADJUSTL(TRIM(text))
            end do
        end if
        if(Gamspk1_Fit) then
            write(25,'(a)') '@Gamspk1-Grid:'
            ! call WDGetSelRadio('radiobuttonG1', unitRadio(1))
            ! call WDGetSelRadio('radiobuttonG5', unitRadio(2))
            ! call WDGetSelRadio('radiobuttonG9', unitRadio(3))
            ! call WDGetSelRadio('radiobuttonG11', unitRadio(4))
            ! call WDGetSelRadio('radiobuttonG13', unitRadio(5))

            write(25,'(a,5i2)') 'UnitRadio=',(UnitRadio(i),i=1,5)

            ! call WDGetComboboxAct('comboboxGMWtyp', kmwtyp)
            write(25,'(a,i2)') 'MeanTyp=',kmwtyp

            ! call WDGetEntryDouble('entry_b2LFactor', FBT)
            write(25,'(a,f7.4)') 'FBT=',real(FBT,8)

            ! call WDGetCheckButton('checkbuttonGspk1EffiCov', ecorruse)
            write(25,'(a,i1)') 'EcorrUse=',ecorrUse
            ! write(25,'(a,i1)') 'WMextSD=',WMextSD

            if(numd > 0) then
                kxy = numd/5
                ! call WTreeViewGetCheckArray('treeview6', 2, kxy, guse)
                ! call WTreeViewGetDoubleArray('treeview6', 3, kxy, erg)
                ! call WTreeViewGetDoubleArray('treeview6', 4, kxy, GNetRate)
                ! call WTreeViewGetDoubleArray('treeview6', 5, kxy, RateCB)
                ! call WTreeViewGetDoubleArray('treeview6', 6, kxy, RateBG)
                ! call WTreeViewGetDoubleArray('treeview6', 7, kxy, SDRateBG)
                ! call WTreeViewGetDoubleArray('treeview6', 8, kxy, effi)
                ! call WTreeViewGetDoubleArray('treeview6', 9, kxy, SDeffi)
                ! call WTreeViewGetDoubleArray('treeview6', 10, kxy, pgamm)
                ! call WTreeViewGetDoubleArray('treeview6', 11, kxy, SDpgamm)
                ! call WTreeViewGetDoubleArray('treeview6', 12, kxy, fatt)
                ! call WTreeViewGetDoubleArray('treeview6', 13, kxy, SDfatt)
                ! call WTreeViewGetDoubleArray('treeview6', 14, kxy, fcoinsu)
                ! call WTreeViewGetDoubleArray('treeview6', 15, kxy, SDfcoinsu)
                do k=1,kxy
                    write(text,'(i2,a2,13(es23.15e2,a2))') guse(k),' #',real(Erg(k),8),' #',real(GNetRate(k),8),' #', &
                        real(RateCB(k),8),' #', real(RateBG(k),8),' #',real(SDRateBG(k),8),' #',  &
                        real(effi(k),8),' #',real(SDeffi(k),8),' #',real(pgamm(k),8),' #',real(SDpgamm(k),8),' #',     &
                        real(fatt(k),8),' #',real(SDfatt(k),8),' #',real(fcoinsu(k),8),' #',real(SDfcoinsu(k),8),' #'
                    do j=1,20
                        m1 = index(text, '  ')
                        if(m1 == 0) exit
                        text = text(1: m1-1) //  trim(text(m1+2:))
                    end do
                    write(25,'(a)') ADJUSTL(TRIM(text))
                end do
            end if
        end if

        if(FitCalCurve) then
            write(25,'(a)') '@Kalfit-Grid:'
            kk = 0
            if(use_UfitKal) kk = 1
            write(25,'(a,i2,1x,i1,1x,i1)')  'KalPars=',nkalpts,kal_polgrad,kk
            write(25,'(a,a)') 'CCTitle=',trim(CCTitle)
            do i=1,nkalpts
                write(text,*) real(xkalib(i),8),' #',real(uxkalib(i),8),' #',real(ykalib(i),8),' #',real(uykalib(i),8)
                do j=1,20
                    m1 = index(text, '  ')
                    if(m1 == 0) exit
                    text = text(1: m1-1) //  trim(text(m1+2:))
                end do
                write(25,'(a)') ADJUSTL(TRIM(text))
            end do
        end if

        write(25,'(a)') '@Sonstige:'
        write(25,'(a,f8.6)') 'kalpha=',real(kalpha,8)
        write(25,'(a,f8.6)') 'kbeta=',real(kbeta,8)
        write(25,'(a,f5.3)') 'coverf=',real(coverf,8)
        if(coverin > ZERO) write(25,'(a,f5.3)') 'coverin=',real(coverin,8)
        write(25,'(a,f6.4)') '1-gamma=',real(W1minusG,8)
        write(25,'(a,f6.4)') 'GamDistAdd=',real(GamDistAdd,8)
        ! call WDGetSelRadioMenu('MT_NegLin',k)
        if(k == 1) write(25,'(a,a)') 'ModelType=','PosLin'
        if(k == 2) write(25,'(a,a)') 'ModelType=','GUM_restricted'
        if(k == 3) write(25,'(a,a)') 'ModelType=','NegLin'

        if(nvarsMD > 0) then
            write(25,'(a)') '@means:'
            if(ubound(k_MDtyp,dim=1) >= nvarsMD) write(25,'(a,20i2)') 'meantyp=',(k_MDtyp(i),i=1,nvarsMD)
            if(refdataMD > 0) then
                write(25,'(a,i3)') 'refmean=',refdataMD
            else
                write(25,'(a)') 'refmean=0'
            end if
            do nk=1,nvarsMD
                if(ubound(nvalsMD,dim=1) == 0 ) exit
                call writeMDvec(nk,.false.,25,cdm)
            end do
        end if
        if(kbgv_binom > 0) then
            write(25,'(a,i0,1x,i0,1x,i0,1x,i0)') 'BinPoi=',ip_binom,kbgv_binom,itm_binom,ilam_binom
        end if
        close (25)

    end subroutine ProSave

end module PSave
