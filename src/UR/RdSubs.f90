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
module RdSubs
    use UR_types
    !     contains:
    ! TransferToGTK
    ! WandelDPkt
    ! rmcformF
    ! writeMDvec

contains

!#######################################################################

    subroutine TransferToGTK(ugr, cvgr, fit, abgr, gsp1gr, imenu1, kmwtyp)

        ! this routine transfers values of all variables into the various fields
        ! and treeviews of the UncertRadio GUI.
        ! It is called by ProRead and by ProRedad_csv.

        !     Copyright (C) 2014-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,          only: c_ptr,c_int,c_null_char,c_null_ptr,c_associated
        use gtk,                    only: gtk_widget_set_sensitive
        use UR_gtk_variables,       only: consoleout_gtk,lstfd_syms,lstfd_symtable,TV1_lentext
        use top,                    only: FinditemS, idpt

        USE UR_Variables,           only: kModelType,batest_user
        USE UR_Gleich,              only: Formeltext, FormeltextFit, &
                                          kEGr,knumEGr,ngrs,meanID,nvarsMD,TAB_VALUNC_Grid,Titeltext,  &
                                          bedeutung,symbole,symboleG,knetto,kbrutto,MDpoint,SDFormel, &
                                          refdataMD,rinflu_known,ncov,coverf

        USE UR_DLIM,                only: alpha,beta,GamDistAdd,kalpha,kbeta,W1minusG,nwgmeth, &
                                          NWGMethode
        USE UR_Linft,               only: ifit,cctitle,CFaelldatum,FitCalCurve,kal_Polgrad,kfitmeth, &
                                          nchannels,ndefall,nkovzr,numd,nwei,use_UfitKal, &
                                          FitDecay,use_WTLS_kal

        USE UR_Gspk1Fit,            only: ecorruse,effi,erg,fatt,fcoinsu,Gamspk1_Fit,GNetRate,guse,pgamm,rateBG, &
                                          RateCB,sdeffi,sdfatt,SDfcoinsu,sdpgamm,SDRateBG,WMextSD,unitradio,fbt
        use Rout,                   only: WDPutSelRadio,WDPutEntryDouble, &
                                          WDSetComboboxAct,WDPutSelRadioMenu, &
                                          WDPutEntryString,WDPutTextviewString, &
                                          WDSetCheckButton,WTreeViewPutDoubleArray, &
                                          WTreeViewPutCheckArray,WTreeViewPutDoubleArray, &
                                          WDListstoreFill_1,SetMenuEGr,WDGetSelRadioMenu, &
                                          WDGetTextviewString, &
                                          WTreeViewPutStrCell, &
                                          WTreeViewGetComboArray

        use LSTfillT,               only: WDListstoreFill_table
        use KLF,                    only: xkalfit
        use LDN,                    only: ConvertGamD
        use CHF,                    only: ucase
        use PMD,                    only: Gamsymlist,gamPeakVals
        use Rg,                     only: modify_Formeltext


        implicit none

        external    funcsKB

        logical,intent(in)         :: ugr, cvgr, fit, abgr, gsp1gr
        integer,intent(in)         :: imenu1, kmwtyp

        logical                :: prout
        type(c_ptr)            :: tree

        integer                :: i, kk, k
        real(rn),allocatable   :: rdummy(:)
        character(len=50),allocatable :: SymboleGGG(:), Scopy(:)
        !-----------------------------------------------------------------------
        prout = .false.
        ! prout = .true.

        ! WRITE(66,*) '########## Anfang TrToGrid  ##############################'
        if(consoleout_gtk) WRITE(0,*) '##### Anfang TrToGrid  ##############################'
        NWGMethode = NWGMeth

        call WDPutEntryDouble('entryOptKalpha', kalpha, '(f8.6)')
        call WDPutEntryDouble('entryOptKbeta', kbeta, '(f8.6)')
        call WDPutEntryDouble('entryOptAlpha', alpha, '(f8.6)')
        call WDPutEntryDouble('entryOptBeta', beta, '(f8.6)')
        call WDPutEntryDouble('entryOptCoverf',coverf,'(f5.2)')
        call WDPutEntryDouble('entryOpt1minusG',W1minusG,'(f5.3)')
        call WDPutEntryString('entryOptDLMethod',trim(NWGMethode))
        call WDPutEntryDouble('entryOptGamDistAdd',GamDistAdd,'(f3.1)')

        write(66,'(a,i0)') 'kModelType=',kModelType
        call WDPutSelRadioMenu('MT_NegLin',kModelType)
        call WDGetselRadioMenu('MT_NegLin',kk)
        if(.not.batest_user) write(55,*) 'ModelType aus Menu gelesen: =',kk

        if(prout) write(66,'(a,i0)') 'TrToGTK, nach: ModelType=',kModelType
        if(consoleout_gtk) write(0,'(a,i0)') 'TrToGTK, nach: ModelType=',kModelType

        call WDSetComboboxAct('comboboxtextKnumegr', knumEGr)
        call gtk_widget_set_sensitive(idpt('QFirst'), 1_c_int)
        call SetMenuEGr(knumEGr)
        if(prout) write(66,'(a,i0,a,i0)') '    TrToGrid:  knumEGr=',knumEGr,'  kEGr=',kEGr
        if(consoleout_gtk) write(0,'(a,i0,a,i0)') '    TrToGrid:  knumEGr=',knumEGr,'  kEGr=',kEGr

        call WDPutSelRadioMenu('QThird', kEGr)
        call WDPutEntryString('entryActiveKegr',trim(Symbole(kEGr)%s))

        call WDPutTextviewString('textview1',Titeltext)
        if(consoleout_gtk) Write(0,*) 'nach WDPutTextviewString("textview1",Titeltext)'

        ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
        if(.not.batest_user) then
            do i=1,ubound(titeltext,dim=1)
                write(66,*) Titeltext(i)%s
            end do
        end if

        if(prout) WRITE(55,*) 'Before loading the symbol table: ngrs=',ngrs,'  Bedeutung(1)=',trim(Bedeutung(1)%s)

        TV1_lentext = 0
        do i=1,size(Titeltext)
            TV1_lentext = TV1_lentext + len_trim(Titeltext(i)%s)
        end do

        call modify_Formeltext(2)
        call WDPutTextviewString('textview2',Formeltext)
        if(prout)  Write(66,*) 'behind WDPutTextviewString("textview2",Formeltext)'
        if(consoleout_gtk) Write(0,*) 'behind WDPutTextviewString("textview2",Formeltext)'

        ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
        call WDGetTextviewString('textview2',Formeltext)
        call modify_Formeltext(1)

        call WDListstoreFill_table('liststore_symtable',1, .false.)

        do i=ngrs+1,ngrs+10
            do k=2,5
                call WTreeViewPutStrCell('treeview1', k, i, '  ')
            end do
        end do

        lstfd_symtable = .true.
        if(prout)  Write(66,*) 'nach WDListstoreFill_table(1)'
        if(consoleout_gtk) Write(0,*) 'nach WDListstoreFill_table(1)'

        TAB_VALUNC_Grid = .true.

        if(Gamspk1_Fit) then
            call GamSymList()
            allocate(SymboleGGG(ubound(Symbole,dim=1)))
            k = ubound(Symbole,dim=1)
            do i=1,ubound(Symbole,dim=1)
                SymboleGGG(i) = Symbole(i)%s
            end do

        end if

        if(.not.Gamspk1_Fit) then
            call WDListstoreFill_1('liststore_symbols', ngrs, symbole)
        else
            call WDListstoreFill_1('liststore_symbols', ngrs+ncov+numd, symbole)
        end if
        lstfd_syms = .true.

        if(consoleout_gtk) Write(0,*) 'behind WDListstoreFill_1(liststore_symbols)'
        if(.not.FitDecay .and. .not.Gamspk1_Fit) then
            IF(knetto(kEGr) > 0) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
            IF(kbrutto(kEGr) > 0) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGr))
        end if
        write(66,'(2(a,i3))') 'Knetto=',knetto(kEGr),'  kbrutto=',kbrutto(kEGr)

        if(nvarsMD > 0) then
            call WDListstoreFill_1('liststore_MDvars', nvarsMD, meanID)
            do i=1,nvarsMD
                if(MDpoint(i) /= kbrutto(kEGr)) SDformel( MDpoint(i))%s = ' '
            end do
            if(refdataMD > 0) then
                call WDSetComboboxAct('combobox_RefMD',refdataMD)
                rinflu_known = .true.
                write(55,*) 'refdataMD=',refdataMD
            end if
        end if

        if(FitCalCurve) then
            call gtk_widget_set_sensitive(idpt('KalFit'), 1_c_int)
            call WDListstoreFill_table('liststore_kalfit',7, .false.)
            call WDPutEntryString('entryDKTitel', trim(CCTitle))
            call WDSetComboboxAct('comboboxDKPgrad', kal_Polgrad+1)
            kk = 1
            if(.not.use_UfitKal) kk = 0
            call WDSetCheckButton('DKcheckUfit', kk)
            kk = 1
            if(.not.use_WTLS_kal) kk = 0
            call WDSetCheckButton('DKcheckWTLS', kk)     ! 7.8.2023
            !  write(0,*) 'TtoGTK: before Xkalfit: KFmode=',Kfmode,' maKB=',maKB
            ! call Xkalfit()          ! xxxxxxxxxxxxxxxxxxxxxxxxxxxx
            ! write(66,*) 'Laden Kalfit: chisqKB=',sngl(chisqKB),' a_aklib=',(sngl(a_kalib(j)),j=1,maKB)
        end if

        IF(fit) THEN
            call gtk_widget_set_sensitive(idpt('MenuDecayCurve'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('FittingModel'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('FittingData'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('FittingResult'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('ExportToR'), 1_c_int)

            if(.not. Gamspk1_Fit) call gtk_widget_set_sensitive(idpt('TBModelDialog'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('TBInputDialog'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('TBFittingResult'), 1_c_int)

            call WDSetComboboxAct('comboboxA1', ifit(1))
            call WDSetComboboxAct('comboboxA2', ifit(2))
            call WDSetComboboxAct('comboboxA3', ifit(3))
            if(prout) write(66,*) 'ProRead:   ifit=',ifit

            call WDSetCheckButton('checkbuttonWFit', nwei)
            call WDSetCheckButton('checkbuttonCovZR', nkovzr)
            call WDSetCheckButton('checkbuttonAllm', ndefall)
            if(prout) write(66,*) 'ProRead:   nwei=',nwei,'  nkovzr=',nkovzr,'  ndefall=',ndefall,' kfitmeth=',kfitmeth

            IF(kfitmeth == 0) call WDPutSelRadio('radiobuttonNLSQ', 1)
            IF(kfitmeth == 1) call WDPutSelRadio('radiobuttonNLSQ', 2)
            IF(kfitmeth == 2) call WDPutSelRadio('radiobuttonNLSQ', 3)
            IF(kfitmeth == 3) call WDPutSelRadio('radiobuttonNLSQ', 4)

            call WDSetComboboxAct('comboboxtextNCH', nchannels)
            if(prout)  write(66,*) 'nchannels=',nchannels
            if(ubound(FormelTextFit,dim=1) > 0) call WDPutTextviewString('textviewModelEQ',FormeltextFit)

        END IF

        if(ugr) then

            if(Gamspk1_Fit) then
                allocate(Scopy(ngrs+ncov+numd))
                do i=1,ngrs+ncov+numd
                    Scopy(i) = ' '
                    if(i <= ngrs) Scopy(i) = trim(Symbole(i)%s)
                end do
            end if
            call WDListstoreFill_table('liststore_valunc',2, .true.)

            tab_valunc_grid = .true.
        end if

        if(prout) write(66,*) 'TransferToGrid 284:   cvgr=',cvgr

        IF(cvgr) THEN
        !------------------------------
            if(.not.allocated(SymboleG)) allocate(symboleG, source=Symbole)
            do i=1,ngrs
                symboleG(i)%s = ucase(symbole(i)%s)
            end do

            if(.not. ugr) then
                call WDListstoreFill_table('liststore_valunc',2, ugr)
                if(consoleout_gtk) Write(0,*) 'nach 60: WDListstoreFill_table(liststore_valunc,2, .true.)'
            end if
            call WDListstoreFill_table('liststore_covtable',4, .false.)
        END IF
        !------------------------------
        IF(abgr) THEN

            call WDPutEntryString('entrySeparation', trim(CFaelldatum))
            call WDSetComboboxAct('comboboxtextbase', imenu1)

            tree = idpt('treeview5')
            call WDListstoreFill_table('liststore_Decay',5, ugr)      ! ugr hat hier keine Bedeutung
            if(consoleout_gtk) Write(0,*) 'nach WDListstoreFill_table(liststore_Decay,5, ugr)'
        END IF

        if(gsp1gr) then
            call WDSetComboboxAct('comboboxGMWtyp', max(1,kmwtyp))
            call WDPutEntryDouble('entry_b2LFactor', FBT,'(f6.3)')
            call WDSetCheckButton('checkbuttonGspk1EffiCov', ecorruse)
            call WDSetCheckButton('checkbuttonMeanOpt', WMextSD)

            call WDPutSelRadio('radiobuttonG1', unitRadio(1))
            call WDPutSelRadio('radiobuttonG5', unitRadio(2))
            call WDPutSelRadio('radiobuttonG9', unitRadio(3))
            call WDPutSelRadio('radiobuttonG11', unitRadio(4))
            call WDPutSelRadio('radiobuttonG13', unitRadio(5))

            call gtk_widget_set_sensitive(idpt('MenuGSpekt1'), 1_c_int)

            call WDListstoreFill_table('liststore_gspk1',6, .false.)
            if(consoleout_gtk) Write(0,*) 'nach WDListstoreFill_table(liststore_gspk1,6, .false.)'

            call WTreeViewPutCheckArray('treeview6', 2, numd/5, guse)
            call WTreeViewPutDoubleArray('treeview6', 3, numd/5, erg)
            call WTreeViewPutDoubleArray('treeview6', 4, numd/5, GnetRate)
            call WTreeViewPutDoubleArray('treeview6', 5, numd/5, RateCB)
            call WTreeViewPutDoubleArray('treeview6', 6, numd/5, RateBg)
            call WTreeViewPutDoubleArray('treeview6', 7, numd/5, SDRateBg)
            call WTreeViewPutDoubleArray('treeview6', 8, numd/5, effi)
            call WTreeViewPutDoubleArray('treeview6', 9, numd/5, SDeffi)
            call WTreeViewPutDoubleArray('treeview6', 10, numd/5, pgamm)
            call WTreeViewPutDoubleArray('treeview6', 11, numd/5, SDpgamm)
            call WTreeViewPutDoubleArray('treeview6', 12, numd/5, fatt)
            call WTreeViewPutDoubleArray('treeview6', 13, numd/5, SDfatt)
            call WTreeViewPutDoubleArray('treeview6', 14, numd/5, fcoinsu)
            call WTreeViewPutDoubleArray('treeview6', 15, numd/5, SDfcoinsu)

            do i=1,numd/5
                call ConvertGamD(i)
            end do
            call GamPeakvals()

        end if

        if(ncov > 0) then
            if(Gamspk1_Fit) then
                do i=1,ubound(Symbole,dim=1)
                    if(i > ngrs) Symbole(i)%s = SymboleGGG(i)
                end do
            end if

            call WDListstoreFill_table('liststore_covtable',4, .false.)

            if(Gamspk1_Fit) then
                if(ncov+numd > 0) then
                    do i=ngrs+1,ngrs+ncov+numd
                        Symbole(i)%s = ' '
                    end do
                end if
            end if

        end if

        if(allocated(rdummy)) deallocate(rdummy)
        if(allocated(SymboleGGG)) deallocate(SymboleGGG)
        if(allocated(Scopy)) deallocate(Scopy)

        ! WRITE(66,*) '########## End TrToGrid  ##############################'
        if(consoleout_gtk) WRITE(0,*) '##### End TrToGrid  ##############################'

    end subroutine TransferToGTK

!##############################################################################

    subroutine wandeldpkt(text, k)

        ! adapts the decimal point character to the local language

        !     copyright (c) 2014-2023  günter kanisch

        use translation_module, only: T => get_translation

        implicit none


        character(len=*),intent(inout)  :: text
        integer, intent(in)           :: k      ! k=1:  input (immer '.'); k=2: output (sprachabhängig)

        integer      :: i

        do i=1,len_trim(text)
            if(k == 1 .and. text(i:i) == ',') text(i:i) = '.'

            if(k == 2 .and. any(text(i:i) == ['.', ','])) text(i:i) = T('.')

        end do

    end subroutine wandeldpkt

    !#######################################################################

    character(len=15) function rmcformF(value)

        ! prepares fortran formats for numbers representing relative
        ! uncertainties given in per cent

        !     Copyright (C) 2014-2023  Günter Kanisch
        implicit none

        real(rn), intent(in)     :: value

        if(value < 0._rn) rmcformF = '(f4.1)'
        rmcformF = '(f6.3)'
        if(value >= 1._rn) rmcformF = '(f5.2)'
        if(value >= 10._rn) rmcformF = '(f5.1)'
        if(value >= 100._rn) rmcformF = '(f5.2)'
        if(value >= 100._rn) rmcformF = '(f9.0)'

    end function rmcformF

    !#######################################################################

    subroutine writeMDvec(k_datvar,is_csv,kunit,textout)

        ! writes the values of an array xdataMD (vector) of data
        ! as a vector into a single row, represented here by the character
        ! variable textout.

        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_Gleich,     only: nvalsMD,meanID,xdataMD,ixdanf
        use UR_VARIABLES,  only: sDecimalPoint,sListSeparator
        use CHF,           only: FormatNumStr

        implicit none


        integer, intent(in)   :: k_datvar          ! number of dataset for calculating a mean
        logical, intent(in)   :: is_csv            ! is it a CSV file?
        integer, intent(in) :: kunit               ! file unit number
        character(len=*),intent(inout) :: textout  !  string with the printed array of values

        character(len=20)       :: str(nvalsMD(k_datvar))
        integer                 :: i,nx,j
        character(len=20)       :: frmt
        character(len=1)        :: sdp, sLS
        character(len=:),allocatable     :: text

        sdp = sDecimalPoint
        sLs = sListSeparator

        frmt = '(1pg17.6E2)'

        allocate(character(len=2048) :: text)

        !-------------------------------------------------------------------

        nx = nvalsMD(k_datvar)
        do i=1,nx
            write(str(i),frmt) xdataMD(ixdanf(k_datvar) + i-1)
            str(i) = adjustL(FormatNumStr(trim(str(i)), sDecimalPoint))
        end do
        if(.not.is_csv) then
            do i=1,nx
                do j=1,len_trim(str(i))
                    if(str(i)(j:j) == ',') str(i)(j:j) = '.'
                end do
            end do
            if(kunit > 0) then
                write(kunit,'(a,a,400a)') trim(meanID(k_datvar)%s),': ',(trim(str(i)),' ',i=1,nx)
            else
                write(textout,'(a,a,400a)') trim(meanID(k_datvar)%s),': ',(trim(str(i)),' ',i=1,nx)
            end if
        else
            write(text,'(a,a,400a)') trim(meanID(k_datvar)%s),sLs,(trim(str(i)),sLs,i=1,nx),sLs
            do i=1,len_trim(text)
                if(text(i:i) == '.') text(i:i) = sdp
            end do
            write(kunit,'(a)') trim(text)
        end if

        sDecimalPoint = sdp

    end subroutine writeMDvec

!#########################################################################

end module RdSubs
