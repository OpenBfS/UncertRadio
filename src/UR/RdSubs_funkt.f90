
module RdSubs

contains

!#######################################################################

subroutine TransferToGTK(ugr,cvgr,fit,abgr,gsp1gr,imenu1,kmwtyp)

use UR_params,              only: rn
use iso_c_binding,          only: c_ptr,c_int,c_null_char,c_null_ptr,c_associated
use gtk,                    only: gtk_widget_set_sensitive, gtk_widget_set_visible
use gtk_hl
use UR_gtk_variables,       only: consoleout_gtk,lstfd_syms,lstfd_symtable,TV1_lentext
use top,                    only: FinditemS,idpt

USE UR_Variables,           only: kModelType
USE UR_Gleich,              only: Formeltext,FormeltextFit,Grid1_gleichg_time,Grid1_valunc_time, &
                                  kEGr,knumEGr,ngrs,meanID,nvarsMD,TAB_VALUNC_Grid,Titeltext,  &
                                  bedeutung,symbole,symboleG,knetto,kbrutto,MDpoint,SDFormel, &
                                  refdataMD,rinflu_known,ncov,Symtyp,einheit
USE UR_NWG1,                only: alpha,beta,coverf,GamDistAdd,kalpha,kbeta,W1minusG,nwgmeth, &
                                  NWGMethode
USE UR_Linft,               only: ifit,cctitle,CFaelldatum,ChisqKB,FitCalCurve,kal_Polgrad,kfitmeth, &
                                  maKB,nchannels,ndefall,nkovzr,numd,nwei,use_UfitKal,a_kalib
USE UR_Gspk1Fit,            only: ecorruse,effi,erg,fatt,fcoinsu,Gamspk1_Fit,GNetRate,guse,pgamm,rateBG, &
                                  RateCB,sdeffi,sdfatt,SDfcoinsu,sdpgamm,SDRateBG,WMextSD,unitradio,fbt
use Rout,                   only: WDPutSelRadio,WDPutEntryDouble,WDGetCheckMenuItem, &
                                  WDSetCheckMenuItem,WDSetComboboxAct,WDPutSelRadioMenu, &
                                  WDPutEntryString,WDPutTextviewString,WTreeViewGetStrArray, &
                                  WDSetCheckButton,WTreeViewPutDoubleArray,                  &
                                  WTreeViewPutCheckArray,WTreeViewPutDoubleArray,      &
                                  WDListstoreFill_1,SetMenuEGr,WDGetSelRadioMenu,WTreeViewGetStrCell, &
                                  WTreeViewGetDoubleArray,WDGetTextviewString,WTreeViewPutStrArray, &
                                  pending_events,WTreeViewPutStrCell
use URdate,                 only: clockm
use LSTfillT,               only: WDListstoreFill_table
use KLF,                    only: xkalfit
use LDN,                    only: ConvertGamD
use CHF,                    only: ucase

implicit none

external    funcsKB

LOGICAL,intent(in)         :: ugr,cvgr,fit,abgr,gsp1gr
integer(4),intent(in)      :: imenu1,kmwtyp

logical                :: prout
type(c_ptr)            :: tree
character(len=1500),allocatable   :: text(:)
character(:),allocatable    :: str1

integer(4)             :: i,j,kk,k,kkk(200)
real(rn)               :: zeffi(20)
real(rn),allocatable   :: rdummy(:)
!-----------------------------------------------------------------------
prout = .false.
    ! prout = .true.

 ! WRITE(66,*) '########## Anfang TrToGrid  ##############################'
 if(consoleout_gtk) WRITE(0,*) '##### Anfang TrToGrid  ##############################'
NWGMethode = NWGMeth

call WDPutSelRadio('OptRadio1', 2)
call WDPutEntryDouble('entryOptKalpha', kalpha, '(f8.6)')
call WDPutEntryDouble('entryOptKbeta', kbeta, '(f8.6)')
call WDPutEntryDouble('entryOptAlpha', alpha, '(f8.6)')
call WDPutEntryDouble('entryOptBeta', beta, '(f8.6)')
call WDPutEntryDouble('entryOptCoverf',coverf,'(f5.2)')
call WDPutEntryDouble('entryOpt1minusG',W1minusG,'(f5.3)')
call WDPutEntryString('entryOptDLMethod',trim(NWGMethode))
call WDPutEntryDouble('entryOptGamDistAdd',GamDistAdd,'(f3.1)')

     !write(66,*) 'TrToGTK, vor: Gum_restricted=',gum_restricted
  !call WDGetCheckMenuItem('GUMonly',i)
  !    if(prout) write(66,*) 'kopt=',i
  !if(i == 1 .and. .not. Gum_restricted) then
  !  call WDSetCheckMenuItem('GUMonly', 0)
  !end if
  !if(i == 0 .and. Gum_restricted) then
  !  call WDSetCheckMenuItem('GUMonly', 1)
  !end if
call WDPutSelRadioMenu('MT_NegLin',kModelType)
 !! call WDPutSelRadioMenu('MT_NegLin',kModelType+2)
  call WDGetselRadioMenu('MT_NegLin',kk)
    write(55,*) 'ModelType aus Menu gelesen: =',kk

     ! if(Gum_restricted) then
     !   ! write(66,*) 'Gum_restricted=',Gum_restricted
     !   call WDSetCheckMenuItem('GUMonly', 1)
     !   ! call WDGetCheckMenuItem('GUMonly',i)
     !   ! write(66,*) '  Auslesen GUMonly: i=',i
     ! else
     !   call WDSetCheckMenuItem('GUMonly', 0)
     ! endif
  if(prout) write(66,*) 'TrToGTK, nach: ModelType=',kModelType
      if(consoleout_gtk) write(0,*) 'TrToGTK, nach: ModelType=',kModelType

call WDSetComboboxAct('comboboxtextKnumegr', knumEGr)
      call gtk_widget_set_sensitive(idpt('QFirst'), 1_c_int)
      call SetMenuEGr(knumEGr)
        if(prout) write(66,*) '    TrToGrid:  knumEGr=',knumEGr,'  kEGr=',kEGr
           if(consoleout_gtk) write(0,*) '    TrToGrid:  knumEGr=',knumEGr,'  kEGr=',kEGr

  !  write(66,*) 'vor WDPutSelRadioMenu("QFirst", kEGr):'
call WDPutSelRadioMenu('QThird', kEGr)                    ! 5.11.2016
call WDPutEntryString('entryActiveKegr',trim(Symbole(kEGr)%s))          ! neues Feld


    ! write(66,*) 'vor Put Titeltext: Titeltext=',trim(Titeltext)

call WDPutTextviewString('textview1',Titeltext)
        if(consoleout_gtk) Write(0,*) 'nach WDPutTextviewString("textview1",Titeltext)'

    ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    call WDGetTextviewString('textview1',Titeltext)
       ! write(66,*) 'aus tv1 gelesen: Titeltext='
    do i=1,size(titeltext)
      write(66,*) Titeltext(i)%s
    enddo


     if(prout) WRITE(55,*) 'Vor Laden der Symbol-Tabelle: ngrs=',ngrs,'  Bedeutung(1)=',trim(Bedeutung(1)%s)

!  if(allocated(text)) deallocate(text)
!  call hl_gtk_text_view_get_text(idpt('textview1'), text, start_line=0_c_int, hidden = TRUE)
!  TV1_lentext = sum(len_trim(text(1:size(text))))

  TV1_lentext = 0
  do i=1,size(Titeltext)
    TV1_lentext = TV1_lentext + len_trim(Titeltext(i)%s)
  enddo
        !  write(66,*) 'TV1_lentext: size(titeltext)=',size(titeltext)
  ! if(allocated(text)) deallocate(text)
   ! write(66,*) 'RDS: vor put: Formeltext=',trim(Formeltext)

call WDPutTextviewString('textview2',Formeltext)
   if(prout)  Write(66,*) 'nach WDPutTextviewString("textview2",Formeltext)'
      if(consoleout_gtk) Write(0,*) 'nach WDPutTextviewString("textview2",Formeltext)'

    ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    call WDGetTextviewString('textview2',Formeltext)
    !write(66,*) 'aus tv2 gelesen: Formeltext='
    !do k=1,size(Formeltext)
    !  write(66,*) Formeltext(k)%s
    !enddo

  do i=1,ngrs
     write(66,*) int(i,2),' ',Symbole(i)%s,' ',symtyp(i)%s,' ',Einheit(i)%s,' ',Bedeutung(i)%s
  end do

 call WDListstoreFill_table('liststore_symtable',1, .false.)
    do i=ngrs+1,ngrs+10
      do k=2,5
        call WTreeViewPutStrCell('treeview1', k, i, '  ')
      enddo
    enddo
  !call pending_events()
  !call pending_events()
  call WTreeViewGetStrCell('treeview1',2,33,str1)
     write(66,*)  'Zeile 33 in TV1:  symbol=',str1

   !do i=ngrs,1,-1
   !  call WTreeViewGetStrCell('treeview1', 5, i, bedeutung(i)%s)
   !  call WTreeViewGetStrCell('treeview1', 2, i, Symbole(i)%s)
   !  write(66,*) 'GetCell  i=',int(i,2),' bedeutung=',trim(bedeutung(i)%s),' Symbole=',trim(Symbole(i)%s)
   !enddo


   !call WTreeViewPutStrArray('treeview1', 2, ngrs+30, Symbole)     ! 18.9.2020
   !call WTreeViewPutStrArray('treeview1', 3, ngrs+30, symtyp)
   !call WTreeViewPutStrArray('treeview1', 4, ngrs+30, Einheit)
   !call WTreeViewPutStrArray('treeview1', 5, ngrs+30, bedeutung)

   call WTreeViewGetStrArray('treeview1', 2, ngrs+30, Symbole)     ! 18.9.2020
   call WTreeViewGetStrArray('treeview1', 3, ngrs+30, symtyp)
   call WTreeViewGetStrArray('treeview1', 4, ngrs+30, Einheit)
   call WTreeViewGetStrArray('treeview1', 5, ngrs+30, bedeutung)

  do i=1,ngrs
     write(66,*) int(i,2),' ',Symbole(i)%s,' ',symtyp(i)%s,' ',Einheit(i)%s,' ',Bedeutung(i)%s
  end do


   lstfd_symtable = .true.
   if(prout)  Write(66,*) 'nach WDListstoreFill_table(1)'
       if(consoleout_gtk) Write(0,*) 'nach WDListstoreFill_table(1)'

    TAB_VALUNC_Grid = .true.               ! 4.9.2014
    call clockm(grid1_valunc_time)
    grid1_gleichg_time = grid1_valunc_time        ! diese Zeile am 17.4.2016 (wurde bisher nirgendwo > 0 gesetzt)

! if(Gamspk1_Fit .and. ncov > 0) then
if(Gamspk1_Fit) then               ! ab 25.5.2020
  ! 13.4.2020:
  call GamSymList()
endif

call WDListstoreFill_1('liststore_symbols', ngrs, symbole)
 lstfd_syms = .true.
     if(consoleout_gtk) Write(0,*) 'nach WDListstoreFill_1(liststore_symbols)'
IF(knetto(kEGr) > 0) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
IF(kbrutto(kEGr) > 0) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGr))

      write(66,'(2(a,i3))') 'Knetto=',knetto(kEGr),'  kbrutto=',kbrutto(kEGr)

if(nvarsMD > 0) then
  call WDListstoreFill_1('liststore_MDvars', nvarsMD, meanID)  ! 17.11.2017
  do i=1,nvarsMD
    if(MDpoint(i) /= kbrutto(kEGr)) SDformel( MDpoint(i))%s = ' '
  end do
  if(refdataMD > 0) then
    call WDSetComboboxAct('combobox_RefMD',refdataMD)   ! 7.1.2020
    rinflu_known = .true.
     write(55,*) 'refdataMD=',refdataMD
  endif
end if

if(FitCalCurve) then
  call gtk_widget_set_sensitive(idpt('KalFit'), 1_c_int)
  call WDListstoreFill_table('liststore_kalfit',7, .false.)
  call WDPutEntryString('entryDKTitel', trim(CCTitle))
  call WDSetComboboxAct('comboboxDKPgrad', kal_Polgrad+1)
  kk = 1
  if(.not.use_UfitKal) kk = 0
  call WDSetCheckButton('DKcheckUfit', kk)
  call Xkalfit()
    write(66,*) 'Laden Kalfit: chisqKB=',sngl(chisqKB),' a_aklib=',(sngl(a_kalib(j)),j=1,maKB)
endif

IF(fit) THEN
  call gtk_widget_set_sensitive(idpt('MenuDecayCurve'), 1_c_int)
  call gtk_widget_set_sensitive(idpt('FittingModel'), 1_c_int)
  call gtk_widget_set_sensitive(idpt('FittingData'), 1_c_int)
  call gtk_widget_set_sensitive(idpt('FittingResult'), 1_c_int)
  call gtk_widget_set_sensitive(idpt('ExportToR'), 1_c_int)

  ! call gtk_widget_set_sensitive(idpt('TBModelDialog'), 1_c_int)
  if(.not. Gamspk1_Fit) call gtk_widget_set_sensitive(idpt('TBModelDialog'), 1_c_int)    ! ab 30.11.2015
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
  call WDPutTextviewString('textviewModelEQ',FormeltextFit)

END IF

if(ugr) then
  !      write(66,*)  'Vor liststore_valunc :   ngrs=',ngrs
  !  do i=1,ngrs
  !    write(66,*) 'i=',i,trim(symbole(i)),' ',trim(bedeutung(i)),sngl(messwert(i)),'   IAR=',IAR(i)
  !  enddo
      if(consoleout_gtk) Write(0,*) 'vor WDListstoreFill_table(liststore_valunc,2, .true.)'
  call WDListstoreFill_table('liststore_valunc',2, .true.)
   ! lstfd_valunc = .true.
  !      write(66,*)  'Hinter liststore_valunc'
      if(consoleout_gtk) Write(0,*) 'nach WDListstoreFill_table(liststore_valunc,2, .true.)'

    TAB_VALUNC_Grid = .true.               ! 4.9.2014
    call clockm(grid1_valunc_time)          !
endif
60    CONTINUE

   if(prout) write(66,*) 'TransferToGrid, hinter Label 60:   cvgr=',cvgr

IF(cvgr) THEN
!------------------------------
 ! Ich versuche, komplett auf symboleP zu verzichten! Okt 2014
 !  symboleP(1) = '      '
 if(.not.allocated(SymboleG)) allocate(symboleG, source=Symbole)
 do i=1,ngrs
   symboleG(i)%s = ucase(symbole(i)%s)
 end do

 if(.not. ugr) then
   call WDListstoreFill_table('liststore_valunc',2, ugr)
   ! lstfd_valunc = .true.
     if(consoleout_gtk) Write(0,*) 'nach 60: WDListstoreFill_table(liststore_valunc,2, .true.)'
 end if

  ! WRITE(66,*) 'ProRead: IsymbA(1),IsymbB(1)=',IsymbA(1),IsymbB(1)
  ! do i=1,ngrs
  !   write(66,'(i2,2x,a,2x,a)') i,trim(symbole(i))
  ! enddo

 call WDListstoreFill_table('liststore_covtable',4, .false.)

 !do i=1,ngrs
 !  SymboleP_CP(i) = Symbole(i)       ! am 22.9.2013
 ! end do

  ! WRITE(66,*) 'Laden gspk1:  IsymbA(.)=',(isymba(i),i=1,20)
  ! WRITE(66,*) 'Laden gspk1:  IsymbB(.)=',(isymbb(i),i=1,20)
  ! WRITE(66,*) 'Laden gspk1:  SymbolA,b= ',symboleA(1),symboleB(1)
  ! do i=1,ngrs
  !   WRITE(66,*) 'Proread: ',symbole(i),symboleG(i),'  symboleA:',SymboleA(i)
  ! end do
END IF
!------------------------------
IF(abgr) THEN

  ! call WDPutEntryString('entryNetBlindVal', rbl)     ! <--- das erfolgt woanders?
  call WDPutEntryString('entrySeparation', trim(CFaelldatum))
  call WDSetComboboxAct('comboboxtextbase', imenu1)

   ! write(66,*) 'RDsubs: Decay: vor speichern treeview5:  numd=',int(numd,2)

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
  call WDPutSelRadio('radiobuttonG3', unitRadio(2))
  call WDPutSelRadio('radiobuttonG5', unitRadio(3))
  call WDPutSelRadio('radiobuttonG7', unitRadio(4))
  call WDPutSelRadio('radiobuttonG9', unitRadio(5))
  call WDPutSelRadio('radiobuttonG11', unitRadio(6))
  call WDPutSelRadio('radiobuttonG13', unitRadio(7))

  call gtk_widget_set_sensitive(idpt('MenuGSpekt1'), 1_c_int)

        write(66,*) 'RDsubs: GSPK1: vor speichern treeview6:  numd=',int(numd,2)

  call WDListstoreFill_table('liststore_gspk1',6, .false.)      ! ugr hat hier keine Bedeutung
    if(consoleout_gtk) Write(0,*) 'nach WDListstoreFill_table(liststore_gspk1,6, .false.)'
  !! goto 14

  call WTreeViewPutCheckArray('treeview6', 2, numd/5, guse)      ! in diesemn Block stand vorher Anzahl 20 fest drin!!! 14.4.2020
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

14  continue
  !zeffi(1:numd/5) = effi(1:numd/5)
  !call WTreeViewPutDoubleArray('treeview6', 8, numd/5, zeffi)

  do i=1,numd/5
    call ConvertGamD(i)
  enddo
  call GamPeakvals()

endif

if(allocated(text)) deallocate(text)
if(allocated(rdummy)) deallocate(rdummy)

  ! WRITE(66,*) '########## Ende TrToGrid  ##############################'
      if(consoleout_gtk) WRITE(0,*) '##### Ende TrToGrid  ##############################'

end subroutine TransferToGTK

!##############################################################################
!#######################################################################

subroutine WandelDPkt(text,k)
use UR_VARIABLES,      only: langg

implicit none

CHARACTER(LEN=*),INTENT(INOUT)  :: text
integer(4),INTENT(IN)           :: k      ! k=1:  input (immer '.'); k=2: output (sprachabhängig)

integer(4)      :: i

do i=1,LEN_TRIM(text)
  IF(k == 1 .and. text(i:i) == ',') text(i:i) = '.'
  IF(k == 2 .and. text(i:i) == '.' .and. (langg == 'DE' .or. langg == 'FR')) text(i:i) = ','   ! ab 11.12.2015
end do

END subroutine WandelDPkt

!#######################################################################

character(len=15) function rmcformF(value)

use UR_params,    only: rn

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

use UR_params,     only: rn,eps1min
use UR_Gleich,     only: nvalsMD,meanID,xdataMD,ixdanf
use UR_VARIABLES,  only: sDecimalPoint,sListSeparator
use CHF,           only: FormatNumStr

implicit none


integer, intent(in)   :: k_datvar
logical, intent(in)   :: is_csv
integer(4),intent(in) :: kunit
character(len=*),intent(inout) :: textout

character(len=20)       :: str(nvalsMD(k_datvar))
integer                 :: i,nx,j
character(len=20)       :: frmt          ! ,FormatNumStr
character(len=1)        :: sdp, sLS
character(len=1000)     :: text

sdp = sDecimalPoint
 ! sDecimalPoint ='.'
sLs = sListSeparator

frmt = '(1pg17.6E2)'

!-------------------------------------------------------------------
!!! xdatMD1 liegt als Vektor vor:
nx = nvalsMD(k_datvar)
do i=1,nx
  write(str(i),frmt) xdataMD(ixdanf(k_datvar) + i-1)
  str(i) = adjustL(FormatNumStr(trim(str(i))))
end do
if(.not.is_csv) then
  do i=1,nx
    do j=1,len_trim(str(i))
      if(str(i)(j:j) == ',') str(i)(j:j) = '.'
    end do
  enddo
  if(kunit > 0) then
    write(kunit,'(a,a,400a)') trim(meanID(k_datvar)%s),': ',(trim(str(i)),' ',i=1,nx)
  else
    write(textout,'(a,a,400a)') trim(meanID(k_datvar)%s),': ',(trim(str(i)),' ',i=1,nx)
  endif
else
  write(text,'(a,a,400a)') trim(meanID(k_datvar)%s),sLs,(trim(str(i)),sLs,i=1,nx),sLs
  do i=1,len_trim(text)
    if(text(i:i) == '.') text(i:i) = sdp
  end do
  write(kunit,'(a)') trim(text)
endif

sDecimalPoint = sdp

  RETURN   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!--------------------------------------------------------------------
!!! xdataMD liegt als Matrix vor:


nx = nvalsMD(k_datvar)
do i=1,nx
  ! write(str(i),frmt) xdataMD(i,k_datvar)
  str(i) = adjustL(FormatNumStr(trim(str(i))))
end do
if(.not.is_csv) then
  do i=1,nx
    do j=1,len_trim(str(i))
      if(str(i)(j:j) == ',') str(i)(j:j) = '.'
    end do
  enddo
  if(kunit > 0) then
    write(kunit,'(a,a,400a)') trim(meanID(k_datvar)%s),': ',(trim(str(i)),' ',i=1,nx)
  else
    write(textout,'(a,a,400a)') trim(meanID(k_datvar)%s),': ',(trim(str(i)),' ',i=1,nx)
  endif
else
  write(text,'(a,a,400a)') trim(meanID(k_datvar)%s),sLs,(trim(str(i)),sLs,i=1,nx),sLs
  do i=1,len_trim(text)
    if(text(i:i) == '.') text(i:i) = sdp
  end do
  write(kunit,'(a)') trim(text)
endif

sDecimalPoint = sdp

end subroutine writeMDvec
!#########################################################################

end module RdSubs
