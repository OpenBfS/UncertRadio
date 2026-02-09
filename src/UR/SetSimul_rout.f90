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
!     contains
! copyEquats
! modifSymbols
! setDataTV2
! setBinpoiDiag
! setFdecayModel
! setGspk1Data
! setCovTable
! setKalfitData
! setMeanData

!
!    Select a txp-project for which setting it up as a new project
!    will be simulated:
!
! set kEGr = 1
!     kpage = 1
!
!
! before the button_LoadSymbols step:
!    copy equations to textview1
!          copyEquats
!
! before the LoadCompletedSyms step , behind 33:
!    make modifications to the symbol list, e.g., set special symbol type values, add time variables
!          modifSymbols
!
! befoe AcceptAll step:
!    select net and gross count rate symbols
!          set2countRates:  is done in PMD
!
! behind 75:
!   read input data treeview2 from the existing project and transfer the data into treeview2
!
!   if use_bipoi=T:
!      transfer the binomial data to the dialog       'dialog_BinPoi'
!          setBinpoiDiag
!   if FitDecay:
!      transfer the model information to the dialog   'dialogDecayModel'
!          setFdecayModel
!      transfer the decay curve data to the dialog    'dialog_decayvals'
!          setFdecayData
!   if Gamskp1 :
!      transfer the input data to the dialog          'dialog_gspk1'
!          setGspk1Data
!   if FitKal :
!      transfer the input data to the dialog          'dialog_kalfit'
!          setKalfitData
!
!   if Mean values:
!      transfer for the one or two variabales the vectors of input data into the dialog   'dialogMeanData'
!          setMeanData
!
!   if SumEvalFit:
!          setSumEval
!
!--------------------------------------------------------------------------------------------
subroutine copyEquats()

    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_types
    use UR_Gleich_globals,         only: Formeltext, FormeltextFit, ifehl
    use UR_gtk_globals,  only: item_setintern
    use Rout,              only: WDPutTextviewString, pending_events
    use ur_general_globals,      only: copyEQ, open_project_parts
    use Pread,             only: ProRead
    use RG,                only: modify_Formeltext
    implicit none

    write(0,*) 'Start copyEquats:'

    ifehl = 0
    open_project_parts = .true.
    copyEQ = .true.

    call ProRead()

    item_setintern = .true.
    call modify_Formeltext(2)
    call WDPutTextviewString('textview2',Formeltext)
    if(allocated(FormeltextFit)) then
        call WDPutTextviewString('textviewModelEQ',FormeltextFit)
    end if
    item_setintern = .false.
    copyEQ = .false.
    call pending_events()

end subroutine copyEquats

!#######################################################################

subroutine modifSymbols()

!     Copyright (C) 2014-2023  Günter Kanisch

    use UR_types
    USE UR_Gleich_globals, only: ifehl,ngrs,nab, &
                                 knetto, kbrutto, &
                                 SymboleX,symtypX, &
                                 Symbole,symtyp,einheit,bedeutung,MEsswert,SDwert,HBreite, &
                                 SDformel,IVTL,IAR,StdUnc
    use gtk,               only: gtk_widget_set_sensitive
    use UR_Linft,          only: FitDecay
    use UR_Gspk1Fit,       only: Gamspk1_Fit
    use ur_general_globals,      only: modSymb, open_project_parts
    use CHF,               only: ucase
    use Top,               only: CharModA1,DRead,IntModA1,LogModA1,RealModA1,idpt,InitVarsTV8
    use Rout,              only: WDPutTextviewString,pending_events,WTreeViewPutStrArray, &
                                 WTreeViewPutStrCell,WTreeViewGetStrCell,WDSetComboboxAct, &
                                 WDListstoreFill_1,WTreeViewGetStrArray

    use Pread,             only: ProRead
    use UR_gtk_window,     only: charv

    implicit none
    integer                   :: i, k, kfd
    type(charv)               :: xsymbol(1)

    write(0,*) 'Start modifSymbols:'
    write(66,*) 'Start modifSymbols: ngrs=',int(ngrs,2)

    ifehl = 0
    open_project_parts = .true.
    modSymb = .true.

    call ProRead()
    goto 100

100 continue

    write(66,*) 'nach ProRead: ngrs=',int(ngrs,2)


    kfd = 0
    do i=nab+1,ngrs
        call WTreeViewGetStrCell('treeview1',2,i,xsymbol(1)%s)
        do k=1,ngrs
            if(trim(ucase(xsymbol(1)%s)) == trim(ucase(SymboleX(k)%s))) then
                kfd = kfd + 1
            end if
        end do
    end do
    write(0,'(3(a,i0))') 'kfd=',kfd,' ngrs=',ngrs,' nab=',nab
    write(0,'(a,3i3,a,3i3)') 'knetto=',knetto,' kbrutto=',kbrutto

    if(nab+kfd < ngrs) then
        call CharModA1(Symbole,ngrs)
        call CharModA1(symtyp,ngrs)
        call CharModA1(Einheit,ngrs)
        call CharModA1(Bedeutung,ngrs)

        call RealModA1(Messwert,ngrs)
        call RealModA1(SDwert,ngrs)
        call RealModA1(HBreite,ngrs)
        call RealModA1(StdUnc,ngrs)
        call IntModA1(IVTL,ngrs)
        call IntModA1(IAR,ngrs)
        call CharModA1(SDformel,ngrs)

        do k=nab+kfd+1,ngrs
            Symbole(k)%s = SymboleX(k)%s
            SymTyp(k)%s = SymTypX(k)%s

            call WTreeViewPutStrCell('treeview1',2,k,SymboleX(k)%s)
            call WTreeViewPutStrCell('treeview1',3,k,SymTypX(k)%s)
        end do
        call WDListstoreFill_1('liststore_Symbols', ngrs, Symbole)
    end if

    IF(Gamspk1_Fit .or. FitDecay) call WDSetComboboxAct('comboboxNetRate', 0)
    IF(Gamspk1_Fit .or. FitDecay) call WDSetComboboxAct('comboboxGrossRate', 0)

    close (25)
    write(0,*) 'FitDecay=',FitDecay
    write(0,*) 'End modifSymbols:'
    write(66,*) 'End modifSymbols:'

    modSymb = .false.


end subroutine modifSymbols

!###############################################################################

subroutine setDataTV2

    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_params,         only: EPS1MIN
    USE UR_Gleich_globals, only: ifehl,ngrs,nab,kEGr,kbrutto,Symbole,IAR,nab,missingval, &
                                 SymboleX,symtypX,SDformelX,MesswertX,StdUncX, &
                                 SDwertX,HBreiteX,IARX,IVTLX, IVTL,Messwert,SDformel, &
                                 SDwert,StdUnc,HBreite,symtyp
    use Top,               only: CharModA1,DRead,IntModA1,LogModA1,RealModA1
    use UR_gtk_globals,    only: item_setintern
    use Rout,              only: pending_events,WTreeViewPutStrCell,WTreeViewPutDoubleCell, &
                                 WTreeViewPutComboCell,WDListstoreFill_1
    use ur_general_globals,      only: fileToSimulate
    use CHF,               only: ucase

    implicit none

    integer                   :: i1,ios,i,jv,k,mm1,kfd,ngrs_new
    character(:),allocatable  :: text
    character(len=60)         :: csymbol

    write(0,*) 'Start setDataTV2:'
    write(66,*) 'Start setDataTV2:'

    ifehl = 0
    allocate(character(len=800) :: text)

    allocate(SDformelX(ngrs))
    allocate(MesswertX(ngrs),StdUncX(ngrs),SDwertX(ngrs),HBreiteX(ngrs))
    allocate(IARX(ngrs),IVTLX(ngrs))

    close (25)
    open(25, file=fileToSimulate,status='old')

    do
        call DRead(25,text,ios)
        i1 = INDEX(text,'@Unc-Grid')
        if(i1 > 0) EXIT
    end do

    ngrs_new = ngrs
    ! ugr = .TRUE.
    do k=1,ngrs + 10
        call DRead(25,text,ios)
        IF(text(1:1) == '@') THEN
            BACKSPACE 25
            BACKSPACE 25
            ! ugr = .FALSE.
            GOTO 50
        END IF

        i1 = INDEX(text,'#')
        !! write(66,*) 'Textzeile=',trim(text)
        if(k > ngrs) then
            call CharModA1(SDformelX,k)
            call IntModA1(IARX,k)
            call IntModA1(IVTLX,k)
            call RealModA1(MesswertX,k)
            call RealModA1(StdUncX,k)
            call RealModA1(SDWertX,k)
            call RealModA1(HBreiteX,k)

            call CharModA1(SDformel,k)
            call IntModA1(IAR,k)
            call IntModA1(IVTL,k)
            call RealModA1(Messwert,k)
            call RealModA1(StdUnc,k)
            call RealModA1(SDWert,k)
            call RealModA1(HBreite,k)

            ngrs_new = k
        end if

        read(text(1:i1-1),'(a)') Csymbol
        do jv = 2,8
            text = TRIM(text(i1+1:))
            i1 = INDEX(text,'#')
            !  if(jv == 2) write(55,*,decimal='point') 'Messwert-Feld: ',text(1:i1-1)
            if(jv == 2) READ(text(1:i1-1),*,decimal='point') MesswertX(k)
            !  write(55,*) real(Messwert(k),rn)

            if(jv == 3) READ(text(1:i1-1),*) IVTLX(k)
            if(IVTLX(k) == 6) then
                mm1 = index(text(1:i1), '6  #')
                if(mm1 > 0) then
                    ! es liegt das alte txp-Format von UR1 vor:
                    IVTLX(k) = 4     !(N+1)-Regel
                end if
            end if

            ! if(jv == 4) READ(text(1:i1-1),'(a)') SDformel(k)
            if(jv == 4) SDformelX(k)%s = text(1:i1-1)
            if(jv == 5) READ(text(1:i1-1),*) SDWertX(k)
            if(jv == 6) READ(text(1:i1-1),*) HBreiteX(k)
            if(jv == 7) then
                READ(text(1:i1-1),*) IARX(k)
                if(IARX(k) == 0) IARX(k) = 1        ! NLWKN-Kalfit enthält beim 18. Wert eine Null: darf nicht sein
            end if
            if(jv == 8) READ(text(1:i1-1),*,decimal='point') StdUncX(k)

        end do
        ! WRITE(55,*) 'k=',k,' ',Messwert(k),' ',IVTL(k),' ',TRIM(sdformel(k)), &
        WRITE(55,'(a,i3,a,es12.5,a,i2,2a,2(a,es12.5),a,i0,a,es12.5)',decimal='point')  &
            'k=',k,' ',real(MesswertX(k),8),' ',IVTLX(k),' ',sdformelX(k)%s, &
            ' ',real(sdwertX(k),8),' ',real(HBreiteX(k),8),' ',IARX(k),' ',  &
            real(StdUncX(k),8)
    end do

50  continue
    ngrs = ngrs_new


    item_setintern = .true.
    kfd = 0
    do i=nab+1,ngrs
        do k=1,ngrs
            if(trim(ucase(Symbole(i)%s)) == trim(ucase(SymboleX(k)%s))) then
                kfd = kfd + 1
                if(abs(MesswertX(k)-missingval) > EPS1MIN ) call WTreeViewPutDoubleCell('treeview2',5,i,MesswertX(k))
                call WTreeViewPutComboCell('treeview2',6,i,IVTLX(k))
                if(len_trim(SDformelX(k)%s) > 0)  call WTreeViewPutStrCell('treeview2',7,i,SDformelX(k)%s)
                if(abs(SDwertX(k)-missingval) > EPS1MIN ) call WTreeViewPutDoubleCell('treeview2',8,i,SDWertX(k))
                if(abs(HbreiteX(k)-missingval) > EPS1MIN ) call WTreeViewPutDoubleCell('treeview2',9,i,HBreiteX(k))
                call WTreeViewPutComboCell('treeview2',10,i,IARX(k))
            end if
        end do
    end do
    write(0,'(3(a,i0))') 'kfd=',kfd,' ngrs=',ngrs,' nab=',nab
    if(nab+kfd < ngrs) then
        !call RealModA1(Messwert,ngrs)
        !call RealModA1(SDwert,ngrs)
        !call RealModA1(HBreite,ngrs)
        !call RealModA1(StdUnc,ngrs)
        !call IntModA1(IVTL,ngrs)
        !call IntModA1(IAR,ngrs)
        !call CharModA1(SDformel,ngrs)

        do k=nab+kfd+1,ngrs
            if(abs(MesswertX(k)-missingval) > EPS1MIN ) call WTreeViewPutDoubleCell('treeview2',5,k,MesswertX(k))
            call WTreeViewPutComboCell('treeview2',6,k,IVTLX(k))
            if(len_trim(SDformelX(k)%s) > 0)  call WTreeViewPutStrCell('treeview2',7,k,SDformelX(k)%s)
            if(abs(SDwertX(k)-missingval) > EPS1MIN ) call WTreeViewPutDoubleCell('treeview2',8,k,SDWertX(k))
            if(abs(HbreiteX(k)-missingval) > EPS1MIN ) call WTreeViewPutDoubleCell('treeview2',9,k,HBreiteX(k))
            call WTreeViewPutComboCell('treeview2',10,k,IARX(k))

            Symbole(k)%s = SymboleX(k)%s
            SymTyp(k)%s = SymTypX(k)%s
            call WTreeViewPutStrCell('treeview2',2,k,SymboleX(k)%s)
            call WTreeViewPutStrCell('treeview2',3,k,SymTypX(k)%s)
        end do
        call WDListstoreFill_1('liststore_Symbols', ngrs, Symbole)
    end if

!if(.not.Gamspk1_Fit) then
!  call WDListstoreFill_1('liststore_symbols', ngrs, symbole)
!else
!  call WDListstoreFill_1('liststore_symbols', ngrs+ncov+numd, symbole)
!end if


    if(kbrutto(kEGr) > 0) then
        do k=1,ngrs
            if(trim(ucase(Symbole(kbrutto(kEGr))%s)) == trim(ucase(SymboleX(k)%s))) then
                call WTreeViewPutStrCell('treeview2',7,kbrutto(kEGr),SDformelX(k)%s)
            end if
        end do
    end if

    item_setintern = .false.

    deallocate(SymboleX,symtypX,SDformelX)
    deallocate(MesswertX,StdUncX,SDwertX,HBreiteX)
    deallocate(IARX,IVTLX)
    write(0,*) 'End setDataTV2:'
    write(66,*) 'End setDataTV2:'

end subroutine setDataTV2

!################################################################################

subroutine setBinpoiDiag

    use UR_Gleich_globals,         only: ip_binom,kbgv_binom,itm_binom,ilam_binom
    use UR_gtk_globals,  only: item_setintern
    use Rout,              only: WDSetComboboxAct

    implicit none

    write(0,*) 'Start SetBinPoiDiag:'

    item_setintern = .true.
    call WDSetComboboxAct('comboboxBinPoi1',ip_binom)
    call WDSetComboboxAct('comboboxBinPoi2',kbgv_binom)
    call WDSetComboboxAct('comboboxBinPoi3',itm_binom)
    call WDSetComboboxAct('comboboxBinPoi4',ilam_binom)
    item_setintern = .true.


end subroutine setBinpoiDiag

!##############################################################################

subroutine setFdecayModel()

    !     Copyright (C) 2014-2023  Günter Kanisch

    use, intrinsic :: iso_c_binding,    only: c_int
    use UR_types
    USE UR_Gleich_globals,         only: ifehl
    use UR_Linft,          only: ifit, nwei, nkovzr, kfitmeth, ndefall, &
                                 CFaelldatum, &
                                 linfzbase, nchannels

    use UR_Gspk1Fit,       only: Gamspk1_Fit
    use ur_general_globals,      only: open_project_parts, FDecM
    use gtk,               only: gtk_widget_set_sensitive
    use CHF,               only: ucase
    use Top,               only: CharModA1,DRead,IntModA1,LogModA1,idpt
    use UR_gtk_globals,  only: item_setintern
    use Rout,              only: WDPutTextviewString,pending_events,WDSetComboboxAct,WDSetCheckButton, &
                                 WDPutSelRadio,WDPutEntryString
    use LSTfillT,          only: WDListstoreFill_table
    use Pread,             only: ProRead

    implicit none

    write(0,*) 'Start setFdecayModel:'

    ifehl = 0
    open_project_parts = .true.
    FDecM = .true.
    call ProRead()
    goto 100

100 continue

    close (25)


    item_setintern = .true.
!--------------------
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
    write(55,*) 'Read:   ifit=',ifit

    call WDSetCheckButton('checkbuttonWFit', nwei)
    call WDSetCheckButton('checkbuttonCovZR', nkovzr)
    call WDSetCheckButton('checkbuttonAllm', ndefall)
    write(55,*) 'Read:   nwei=',nwei,'  nkovzr=',nkovzr,'  ndefall=',ndefall,' kfitmeth=',kfitmeth

    IF(kfitmeth == 0) call WDPutSelRadio('radiobuttonNLSQ', 1)
    IF(kfitmeth == 1) call WDPutSelRadio('radiobuttonNLSQ', 2)
    IF(kfitmeth == 2) call WDPutSelRadio('radiobuttonNLSQ', 3)
    IF(kfitmeth == 3) call WDPutSelRadio('radiobuttonNLSQ', 4)

    call WDSetComboboxAct('comboboxtextNCH', nchannels)
    write(55,*) 'nchannels=',nchannels
    ! if(ubound(FormelTextFit,dim=1) > 0) call WDPutTextviewString('textviewModelEQ',FormeltextFit)

    !-----------------------------------------------------------------

    call WDPutEntryString('entrySeparation', trim(CFaelldatum))
    call WDSetComboboxAct('comboboxtextbase', linfzbase)

    ! tree = idpt('treeview5')
    call WDListstoreFill_table('liststore_Decay',5, .true.)      ! ugr=T hat hier keine Bedeutung

    !-----------------------------------------------------------------

    item_setintern = .false.
    FDecM = .false.

    close (25)
    write(0,*) 'End setFdecayModel:'

end subroutine setFdecayModel

!###############################################################################

subroutine setGspk1Data()

    !     Copyright (C) 2014-2023  Günter Kanisch

    use, intrinsic :: iso_c_binding,    only: c_int
    use UR_types
    use UR_Gleich_globals,         only: ifehl,ngrs, Symbole,ncov
    use UR_Linft,          only: numd
    use UR_Gspk1Fit,       only: Gamspk1_Fit,guse, erg,GNetRate,RateCB,RateBG, &
                                 SDRAteBG,effi,SDeffi,pgamm,SDpgamm,fatt,SDfatt,fcoinsu,SDfcoinsu, &
                                 unitradio,ecorruse,FBT, WMextSD, kmwtyp      ! 2025.01.23 GK

    use ur_general_globals,      only: open_project_parts,GspkDT
    use gtk,               only: gtk_widget_set_sensitive
    use CHF,               only: ucase
    use Top,               only: CharModA1,DRead,IntModA1,LogModA1,idpt,InitVarsTV6
    use UR_gtk_globals,  only: item_setintern
    use Rout,              only: WDSetComboboxAct,WDSetCheckButton,WTreeViewPutCheckArray, &
                                 WDPutSelRadio,WDPutEntryString,WDPutEntryDouble, &
                                 WTreeViewPutDoubleArray,pending_events,WDListstoreFill_1
    use LSTfillT,          only: WDListstoreFill_table
    use LDN,               only: ConvertGamD
    use Pread,             only: ProRead
    use PMD,               only: GamPeakVals

    implicit none
    integer                   :: i                 !! , kmwtyp


    write(0,*) 'Start setGspk1Data:'
    ifehl = 0
    open_project_parts = .true.
    GspkDT = .true.

    call ProRead()
    write(0,*) 'nach ProRead'
    goto 62

62  CONTINUE

    close (25)
    numd = numd * 5
!--------------------------------------------------
    item_setintern = .true.

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
    ! if(consoleout_gtk) Write(0,*) 'nach WDListstoreFill_table(liststore_gspk1,6, .false.)'
    !  goto 14


    write(55,*) 'ubound(erg,dim=1)=',int(ubound(erg,dim=1),2), &
        ' ubound(GnetRate,dim=1)=',int(ubound(GnetRate,dim=1),2),' numd=',numd
    write(55,*) ' erg=',sngl(erg)
    write(55,*) ' GnetRate=',sngl(GnetRate)

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

    call pending_events()

    item_setintern = .false.

    !!!!! goto 100

    do i=1,numd/5
        call ConvertGamD(i)
    end do
    call GamPeakvals()

    goto 100       ! 20.9.2023

    if(ncov > 0) then
        if(Gamspk1_Fit) then
            do i=1,ubound(Symbole,dim=1)
                ! if(i > ngrs) Symbole(i)%s = SymboleGGG(i)
            end do
        end if

        ! call WDListstoreFill_table('liststore_covtable',4, .false.)    ! 20.9.2023 deactivated, done in PLP wíth setCovTable
        write(0,*) 'ncov=',int(ncov,2),' numd=',int(numd,2)
        if(Gamspk1_Fit) then
            if(ncov+numd > 0) then
                if(ubound(Symbole,dim=1) < ngrs+ncov+numd) call CharModA1(Symbole,ngrs+ncov+numd)
                do i=ngrs+1,ngrs+ncov+numd
                    Symbole(i)%s = ' '
                end do
            end if
        end if
    end if

100 continue

! call WDListstoreFill_1('liststore_symbols', ngrs+ncov+numd, symbole)
    call WDListstoreFill_1('liststore_symbols', ubound(Symbole,dim=1), symbole)
    GspkDT = .false.
    item_setintern = .false.
    write(0,*) 'End setGspk1Data:'

end subroutine setGspk1Data

!##########################################################################

subroutine setCovTable()

!     Copyright (C) 2014-2023  Günter Kanisch

    use UR_Gleich_globals,  only: ifehl, IsymbA, IsymbB, &
                                  ncov, Symbole, ngrs
    use UR_Linft,           only: numd
    use UR_Gspk1Fit,        only: Gamspk1_Fit
    use ur_general_globals, only: open_project_parts,covTB
    use CHF,                only: ucase
    use LSTfillT,           only: WDListstoreFill_table
    use Top,                only: Dread,InitVarsTV3
    use Rout,               only: pending_events,WTreeViewPutComboArray,WTreeViewPutDoubleArray, &
                                  WTreeViewPutStrArray
    use Pread,              only: ProRead
    use PMD,                only: GamSymList,GamPeakvals

    implicit none
    integer                   :: i

    write(0,*) 'Start setCovTable:'

    ifehl = 0
    open_project_parts = .true.
    covTB = .true.

    call ProRead()
    !  call WDListstoreFill_table('liststore_valunc',2, .true.)
    write(66,*) 'setCovTable: nach ProRead: numd=',numd,' ngrs=',int(ngrs,2)
    goto 60

60  continue

    if(Gamspk1_Fit) then
        call GamSymList()
        !allocate(SymboleGGG(ubound(Symbole,dim=1)))
        !do i=1,ubound(Symbole,dim=1)
        !  SymboleGGG(i) = Symbole(i)%s
        !end do

    end if

    call gamPeakVals()        ! 20.9.2023
!----------------------------------------------
    write(66,*) 'setCovTable: ncov=',ncov,' numd=',numd,' ubound(Symbole,dim=1)=',ubound(Symbole,dim=1)
    do i=1,ncov
        write(66,*) 'i=',int(i,2), ' IsymbA, IsymbB: ',IsymbA(i),ISymbB(i)
    end do


    call WDListstoreFill_table('liststore_covtable',4, .false.)


    ! call WTreeViewPutComboArray('treeview3', 2, ncov, ISymbA)
    ! call WTreeViewPutComboArray('treeview3', 3, ncov, ISymbB)
    ! call WTreeViewPutComboArray('treeview3', 4, ncov, icovtyp)
    ! call WTreeViewPutDoubleArray('treeview3',6, ncov, covarval)
    ! if(IsymbA(1) > 0 .and. IsymbB(1) > 1) then
    !    call WTreeViewPutStrArray('treeview3',5, ncov, CVFormel)
    ! end if
    ! call WDListstoreFill_table('liststore_valunc',2, .true.)

    call pending_events()

    covTB = .false.
    write(0,*) 'End setCovTable:'


end subroutine setCovTable

!#########################################################################

subroutine setKalfitData()

!     Copyright (C) 2014-2023  Günter Kanisch

    use, intrinsic :: iso_c_binding,    only: c_int
    use UR_types
    use gtk,               only: gtk_widget_set_sensitive
    use UR_Gleich_globals,         only: ifehl
    use UR_Linft,          only: xkalib,ykalib,maKB,nkalpts, &
                                 kal_Polgrad,uxkalib,uykalib,ChisqKB,CCtitle,a_kalib, &
                                 use_UfitKal

    use ur_general_globals,      only: open_project_parts,FcalDT
    use CHF,               only: ucase
    use LSTfillT,          only: WDListstoreFill_table
    use Top,               only: Dread,idpt
    use Rout,              only: pending_events,WDPutEntryString,WDSetComboboxAct, &
                                 WDSetCheckButton,WTreeViewPutDoubleArray
    use KLF,               only: xkalfit
    use Pread,             only: ProRead


    implicit none
    integer                   :: j, kk

    write(0,*) 'Start setKalFitData:'
    ifehl = 0
    open_project_parts = .true.
    FcalDT = .true.

    call ProRead()
    write(0,*) 'nach ProRead'
    goto 60

60  continue
    close (25)

    !write(0,'(a,3i3,a,3i3)') 'knetto=',knetto,' kbrutto=',kbrutto
    !write(0,*) 'nkalpts=',int(nkalpts,2)
    call pending_events()

    call gtk_widget_set_sensitive(idpt('KalFit'), 1_c_int)
    call WDListstoreFill_table('liststore_kalfit',7, .false.)
    call WDPutEntryString('entryDKTitel', trim(CCTitle))
    call WDSetComboboxAct('comboboxDKPgrad', kal_Polgrad+1)
    write(0,*) 'CCTitle=',trim(CCTitle)
    kk = 1
    if(.not.use_UfitKal) kk = 0
    call WDSetCheckButton('DKcheckUfit', kk)
    call WDSetCheckButton('DKcheckWTLS', 0)      ! 7.8.2023
    call WTreeViewPutDoubleArray('treeview7', 2, nkalpts, xkalib)
    call WTreeViewPutDoubleArray('treeview7', 3, nkalpts, uxkalib)
    call WTreeViewPutDoubleArray('treeview7', 4, nkalpts, ykalib)
    call WTreeViewPutDoubleArray('treeview7', 5, nkalpts, uykalib)
    call pending_events()
    call Xkalfit()
    ! call LinCalib()

    write(66,*) 'Laden Kalfit: chisqKB=',sngl(chisqKB),' a_aklib=',(sngl(a_kalib(j)),j=1,maKB)
    write(0,*) 'End setKalFitData:'

end subroutine setKalfitData

!####################################################################################

subroutine setMeanData()

!     Copyright (C) 2014-2023  Günter Kanisch

    use, intrinsic :: iso_c_binding,    only: c_int
    use UR_types
    USE UR_Gleich_globals, only:    ifehl,kEGr, kbrutto, &
                                    nvarsMD,MDpoint,symtyp, &
                                    SDformel,meanID,refdataMD,rinflu_known,meanID
    use gtk,               only: gtk_widget_set_sensitive
    use ur_general_globals,      only: open_project_parts, MDDT

    use Top,               only: CharModA1,DRead,IntModA1,LogModA1,idpt,InitVarsTV8
    use Rout,              only: WDPutTextviewString,pending_events,WTreeViewPutStrArray, &
                                 WTreeViewPutStrCell,WTreeViewGetStrCell,WDSetComboboxAct, &
                                 WDListstoreFill_1
    use Pread,             only: ProRead


    implicit none
    integer                   :: i, k


    write(0,*) 'Start setMeanData:'
    write(66,*) 'Start setMeanData:'

    ifehl = 0
    open_project_parts = .true.
    MDDT = .true.

    call ProRead()
    goto 60

60  continue
    close (25)

    if(nvarsMD > 0) then
        call WDListstoreFill_1('liststore_MDvars', nvarsMD, meanID)
        do i=1,nvarsMD
            if(MDpoint(i) /= kbrutto(kEGr)) SDformel( MDpoint(i))%s = ' '
        end do
        if(refdataMD > 0) then
            call WDSetComboboxAct('combobox_RefMD',refdataMD)
            rinflu_known = .true.
            ! write(55,*) 'refdataMD=',refdataMD
        end if
    end if
    if(nvarsMD > 0) then
        do k=1,nvarsMD
            symtyp(MDpoint(k))%s = 'm'
        end do
        call gtk_widget_set_sensitive(idpt('TBmeansMD'), 1_c_int)
    end if


    MDDT = .false.
    write(0,*) 'End setMeanData:'
    write(66,*) 'End setMeanData:'

end subroutine setMeanData

!#####################################################################################

subroutine setDistPars()

    use UR_Gleich_globals,  only: ngrs, &
                                  IVTL,HBreite,IAR,Messwert,SDWert, &
                                  StdUnc,nvarsMD,MDpoint,k_datvar,MDpointrev, &
                                  SDformel
    use Rout,               only: WTreeViewGetComboArray,WTreeViewGetDoubleArray,WTreeViewGetStrArray
    use ur_general_globals, only: SetDP, top_selrow
    use UR_gtk_globals,     only: ioption
    use LDN,                only: Loadsel_diag_new
    use Top,                only: FindItemS,MDcalc

    implicit none
    integer             :: ncitem

    write(0,*) 'Start setDistPars:'
    write(66,*) 'Start setDistPars:'


    write(0,*) 'nvarsMD=',nvarsMD
    if(nvarsMD == 0) goto 100

    setDP = .true.

    call WTreeViewGetDoubleArray('treeview2', 5, ngrs, Messwert)
    call WTreeViewGetComboArray('treeview2', 6, ngrs, IVTL)
    call WTreeViewGetStrArray('treeview2', 7, ngrs, SDFormel)
    call WTreeViewGetDoubleArray('treeview2', 8, ngrs, SDWert)
    call WTreeViewGetDoubleArray('treeview2', 9, ngrs, HBreite)
    call WTreeViewGetComboArray('treeview2', 10, ngrs, IAR)
    call WTreeViewGetDoubleArray('treeview2', 11, ngrs, StdUnc)

    write(0,*) 'ubound(ivtl,dim=1)=',ubound(ivtl,dim=1)

    write(66,*) 'MDPointrev=',int(MDPointrev(1:2),2),'  MDpoint=',int(MDpoint(1:2),2)

    do k_datvar=1,nvarsMD
        top_selrow = MDpoint(k_datvar)
        call MDcalc(k_datvar)

        ioption = 74
        call FindItemS('TBDistribDialog', ncitem)
        call Loadsel_diag_new(1, ncitem)
    end do

    setDP = .false.
100 continue

    write(0,*) 'End setDistPars:'
    write(66,*) 'End setDistPars:'


end subroutine setDistPars

!################################################################################

! subroutine setSumEval
! use UR_params,         only: rn,zero
! use, intrinsic :: iso_c_binding,    only: c_int
! USE UR_Gleich,         only: ifehl,kEGr,charv,kbrutto
! use gtk,               only: gtk_widget_set_sensitive
! use UR_VARIABLES,      only: fileToSimulate
!
! use CHF,               only: ucase
! use Top,               only: CharModA1,DRead,IntModA1,LogModA1,RealModA1,idpt,InitVarsTV8
! use Rout,              only: WDPutTextviewString,pending_events,WTreeViewPutStrArray, &
!                              WTreeViewPutStrCell,WTreeViewGetStrCell,WDSetComboboxAct, &
!                              WDListstoreFill_1
!
! implicit none
!
! integer                   :: i,j,jj,kk, i1,i2,ios,i3,nddanf,nv
! character(len=150)        :: tmeanid
! CHARACTER(:),allocatable  :: ttext,text,text2
!
!   write(0,*) 'Start setMeanData:'
!
! ifehl = 0
! allocate(character(len=800) :: text,text2)
! allocate(character(len=1500) :: ttext)
!
! close (25)
! open(25, file=fileToSimulate,status='old')
!
!
!
! end subroutine setSumEval
