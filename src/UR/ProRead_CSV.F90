
module PreadCSV

contains

!#######################################################################

SUBROUTINE ProRead_CSV

   ! reads in a project file of CSV format.
   ! See chapter 3.6 "Structure of the project file" of the UncertRadio
   !    CHM Help file for more information.
   ! It finally calls TransferToGTK to transfer the data into the GTK GUI.
   !
   ! called by ProRead

   !     Copyright (C) 2014-2023  Günter Kanisch


use UR_params,               only: rn,zero,one
use, intrinsic :: iso_c_binding
use gtk,                     only: gtk_window_set_title,gtk_buttons_ok, &
                                   gtk_widget_set_sensitive,GTK_MESSAGE_ERROR
USE UR_Variables,            only: fname,Gum_restricted,sListSeparator,Win_title, &
                                   gross_negative,kModelType,langg,work_path
use UR_gtk_variables,        only: item_setintern,runauto,winRelSizeWidth,winRelSizeHeight
USE UR_Gleich
USE UR_DLIM
USE UR_Linft
USE UR_Gspk1Fit

use CHF,                     only: FLTU,FindlocT,ucase
use Rout,                    only: updateproname,MessageShow,WDPutTextviewString, &
                                   WDPutSelRadio,WDSetComboboxAct,WDPutEntryDouble,    &
                                   WDSetCheckButton,WTreeViewPutDoubleCell
use top,                     only: idpt,ltu
use Brandt,                  only: pnorm
use RdSubs,                  only: TransferToGTK,WandelDPkt
use UR_params,               only: eps1min
use top,                     only: InitVarsTV2,InitVarsTV3,InitVarsTV5,InitVarsTV6, &
                                   InitVarsTV7,IntModA1,CharModA1,RealModA1,LogModA1, &
                                   DRead,GetCells

implicit none

integer(c_int)         :: resp

! CHARACTER(LEN=1500)    :: text,textG       ! Textzeile
CHARACTER(:),allocatable  :: ttext,text,str1 ! ,textG
! CHARACTER(:),allocatable  :: text,textG       ! Textzeile   ! macht noch Probleme: kein allocate bei READ!
type(charv),allocatable  :: cell(:)
type(charv),allocatable  :: cellk(:)
CHARACTER(LEN=50)      :: suchwort,word
integer(4)             :: k,ios,ios2,i,i1,i2,i3,imenu1,kmwtyp,kk,j,nwgtyp
integer(4)             :: kWTLS,inum,m1,ift,nn,kk1,kk2,kkk,idummy,kkL
LOGICAL                :: ugr,cvgr,fit,abgr,gsp1gr,gkalf,enloc
! CHARACTER(LEN=380)     :: str1
CHARACTER(LEN=2)       :: crlf
character(len=6)       :: cios
character(len=256)     :: prstr
character(len=128)     :: iomessg
character(len=1)       :: ctr
character(len=15)      :: ModelType
character(len=40)      :: textz
real(rn)               :: zahl

!-----------------------------------------------------------------------
item_setintern = .true.

enloc = .false.

crlf = CHAR(13) // CHAR(10)
ctr = sListSeparator
fit = .FALSE.
   prstr = trim(fname)
   do i=len_trim(fname),1,-1
     if(fname(i:i) == '\') then
       prstr = trim(fname(i+1:))
       exit
     end if
   end do
   prstr = trim(Win_Title) // '   -   ' // trim(prstr)
   prstr = FLTU(prstr)
call gtk_window_set_title(idpt('window1'),  trim(prstr) // c_null_char)

call UpdateProName(fname)

allocate(character(len=800) :: text)
allocate(character(len=1500) :: ttext)
! allocate(character(len=150) :: str1)
allocate(character(len=300) :: str1)     ! 20.8.2023

close (25)
if(index(fname,':') == 0) then
  open (25,FILE=trim(work_path) // TRIM(fname),STATUS='old',IOSTAT=ios,iomsg=iomessg)
else
open (25,FILE=TRIM(fname),STATUS='old',IOSTAT=ios,iomsg=iomessg)
end if
if(ios == 2 .and. runauto) then
  do i=1,1
    close(25)
    open (25,FILE=trim(work_path) // TRIM(fname),STATUS='old',IOSTAT=ios,iomsg=iomessg)
    if(ios == 0) exit
  end do
end if

IF(ios /= 0) THEN
          write(66,*) 'iomessg=',trim(iomessg)
  write(cios,'(i0)') ios
  if(langg == 'DE') str1 = 'Datei ' // TRIM(fname) // ' kann nicht geöffnet werden! ios='// trim(cios) // '  ' // trim(iomessg)
  if(langg == 'EN') str1 = 'File ' // TRIM(fname) // ' cannot be opened!  ' // trim(iomessg)
  if(langg == 'FR') str1 = 'Fichier ' // TRIM(fname) // ' ne peut pas être ouvert!  ' // trim(iomessg)
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProRead_CSV:", resp,mtype=GTK_MESSAGE_ERROR)
  ifehl = 1
  RETURN
end if

! Test for readability (correct listSeparator character?):
call DRead(25,ttext,ios)
if(ios == 0 .and. len_trim(ttext) > 0) then
  i1 = index(ttext,sListSeparator)
  if(i1 == 0) then
    if(langg == 'DE') str1 = 'Das sListSeparator-Zeichen ' // sListSeparator //   &
                                     ' in UR2 passt nicht zur CSV-Datei! Falsche Sprache?'
    if(langg == 'EN') str1 = 'The sListSeparator character ' // sListSeparator //  &
                                     ' in UR2 does not fit to the CSV file! Wrong language?'
    if(langg == 'FR') str1 = 'Le caractère sListSeparator ' // sListSeparator // &
                                     ' dans UR2 ne correspond pas au fichier CSV! Mauvaise langue?'
    call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProRead_CSV:", resp,mtype=GTK_MESSAGE_ERROR)
    ifehl = 1
    RETURN
  end if
end if
backspace (25)

item_setintern = .true.

if(allocated(cell)) deallocate(cell)
allocate(cell(60))
if(allocated(cellk)) deallocate(cellk)
allocate(cellk(200))
if(allocated(FormeltextFit)) deallocate(FormeltextFit)
allocate(FormeltextFit(10))

do i=1,20
  cell(i)%s = '        '
end do
do i=1,200
  cellk(i)%s = '         '
end do

! Suchwort = search word string
do ift=1,3
  IF(ift == 1) Suchwort = 'TITELTEXT:'
  IF(ift == 2) Suchwort = 'FORMELTEXT:'
  IF(ift == 3) Suchwort = 'FORMELTEXTFIT:'
  m1 = 0
  do
    m1 = m1 + 1
    IF(m1 > 200) EXIT
    call DRead(25,ttext,ios)
    IF(ios /= 0) EXIT
    IF(m1 > 10) EXIT  ! Suchwort not found

    IF(INDEX(ucase(ttext),TRIM(suchwort)) > 0) THEN
      IF(ift == 3) fit = .TRUE.
         kkk = 0
      do
        call DRead(25,ttext,ios)
        IF(ios /= 0) EXIT
        call GetCells(ttext,cell,' ',enloc)
        READ(cell(1)%s,*,IOSTAT=ios2) inum
        IF(ios2 == 0) THEN
          kkk = kkk + 1
          IF(ift == 1) call CharModA1(Titeltext,kkk)
          IF(ift == 2) call CharModA1(Formeltext,kkk)
          IF(ift == 3) call CharModA1(FormeltextFit,kkk)
          IF(LEN_TRIM(cell(2)%s) > 0) THEN
            do i=1,LEN_TRIM(cell(2)%s)
              IF(cell(2)%s(i:i) == '"') cell(2)%s(i:i) = ' '
            end do
            IF(ift == 1) titeltext(kkk)%s = TRIM(cell(2)%s)
            IF(ift == 2) Formeltext(kkk)%s = TRIM(cell(2)%s)
            IF(ift == 3) FormeltextFit(kkk)%s = TRIM(cell(2)%s)
            IF(ift == 3) nmodf = kkk     ! 29.1.2024
          else
            IF(ift == 1) titeltext(kkk)%s = ' '
            IF(ift == 2) Formeltext(kkk)%s = ' '
            IF(ift == 3) FormeltextFit(kkk)%s = ' '
          end if
        else
          EXIT
        end if
      end do
      IF(ios /= 0 .or. ios2 /= 0) EXIT
    end if
  end do
  if(.not.fit .and. allocated(Formeltextfit)) deallocate(FormeltextFit)

  BACKSPACE 25
  BACKSPACE 25
  BACKSPACE 25

end do

IF(.not.allocated(FormeltextFit)) THEN
  do i=1,m1
    BACKSPACE 25
  end do
end if

if(.false.) then
  do i=1,size(Titeltext)
    write(66,*) Titeltext(i)%s
  end do
  write(66,*)
  write(66,*)
  do i=1,size(Formeltext)
    write(66,*) Formeltext(i)%s
  end do
  if(allocated(FormeltextFit)) then
    write(66,*)
    write(66,*)
    do i=1,size(FormeltextFit)
      write(66,*) FormeltextFit(i)%s
    end do
  end if
end if

if(ubound(titeltext,dim=1) > 0) call WDPutTextviewString('textview1', titeltext)
call WDPutTextviewString('textview2', Formeltext)
if(allocated(FormeltextFit)) call WDPutTextviewString('textviewModelEQ', FormeltextFit)

do i=1,15
  backspace (25)
end do

WRITE(55,*) 'Search for Options:'
  m1 = 0
  suchwort = 'OPTIONEN:'
  do
    m1 = m1 + 1
    IF(m1 > 300) EXIT
    call DRead(25,text,ios)
    ! IF(ios /= 0) EXIT
    IF(m1 > 50) THEN
      WRITE(55,*) 'Options not found'
      ifehl = 1
      EXIT  ! Suchwort not found
    end if

    IF(INDEX(ucase(text),TRIM(suchwort)) > 0) THEN

      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      IF(INDEX(ucase(cell(2)%s),'KALPHA') > 0) READ(cell(3)%s,*,IOSTAT=ios2) kalpha
                    write(55,*) 'kalpha=',sngl(kalpha)

      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      IF(INDEX(ucase(cell(2)%s),'KBETA') > 0) READ(cell(3)%s,*,IOSTAT=ios2) kbeta

      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      IF(INDEX(ucase(cell(2)%s),'COVERF') > 0) then
        READ(cell(3)%s,*,IOSTAT=ios2) coverf
        write(55,*) 'coverf=',sngl(coverf)
      end if

      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      IF(index(ucase(cell(2)%s),'NWGTyp') > 0) then
        ! nothing
      else
         backspace (25)
      end if

      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      IF(index(ucase(cell(2)%s),'COVERIN') > 0) then
        READ(cell(3)%s,*,IOSTAT=ios2) coverin
              write(55,*) 'coverin=',sngl(coverin)
      else
        backspace(25)
        coverin = 0._rn
      end if

      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      IF(INDEX(ucase(cell(2)%s),'1-GAMMA') > 0) READ(cell(3)%s,*,IOSTAT=ios2) W1minusG
                    write(55,*) 'W1minusG=',sngl(W1minusG)

      GamDistAdd = zero      ! since 30.11.2019,  according to ISO 11929:2019
      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      IF(INDEX(ucase(cell(2)%s),'GAMDISTADD') > 0) then
        READ(cell(3)%s,*,IOSTAT=ios2) GamDistAdd
           write(55,*) 'GamDistAdd=',sngl(GamDistAdd)
      else
        Backspace 25
      end if

      ! Variable GUM_restricted:
      GUM_restricted = .false.
      gross_negative = .false.
      kModelType = 1
      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,'u',enloc)
          Gum_restricted = .false.
          gross_negative = .false.
      IF(INDEX(ucase(cell(2)%s),'GUM_RESTRICTED') > 0) then
        READ(cell(3)%s,'(L1)',IOSTAT=ios2) Gum_restricted
        kModelType = 1                             !
        if(Gum_restricted) kModelType = 2
      elseif(index(ucase(cell(2)%s),'MODELTYPE') > 0) then
        READ(cell(3)%s,'(a)',IOSTAT=ios2) ModelType
        if(trim(ucase(Modeltype)) == 'POSLIN') then
          Gum_restricted = .false.
          gross_negative = .false.
          kModelType = 1
        end if
        if(trim(ucase(ModelType)) == 'GUM_RESTRICTED') then
          Gum_restricted = .true.
          gross_negative = .false.
          kModelType = 2
        end if
        if(trim(ucase(ModelType)) == 'NEGLIN') then
          Gum_restricted = .false.
          gross_negative = .true.
          kModelType = 3
        end if
        write(55,*) 'kModelType=',kModelType,'  gross_negative=',gross_negative

      else
        Backspace 25
      end if

      EXIT

    end if
  end do
  BACKSPACE 25
  BACKSPACE 25
  BACKSPACE 25

alpha =  one - pnorm(kalpha)                       ! , zero, one)
beta =  one - pnorm(kbeta)                         ! , zero, one)

!WRITE(55,*) 'Options: kalpha,kbeta,w1minusg: ',kalpha,kbeta,w1minusg
!WRITE(55,*) 'Search for project parameters:'

  m1 = 0
  suchwort = 'PROJEKTPARAMETER:'
  do
    m1 = m1 + 1
    IF(m1 > 300) EXIT
    call DRead(25,text,ios)
    ! IF(ios /= 0) EXIT
    IF(m1 > 25) THEN
      WRITE(55,*) 'Projektparameter nicht gefunden'
      EXIT  ! Suchwort not found
    end if

    IF(INDEX(ucase(text),TRIM(suchwort)) > 0) THEN

      call DRead(25,text,ios)
      call GetCells(text,cell,'u',enloc)

      call WandelDPkt(cell(3)%s,1)
      READ(cell(3)%s,*) nchannels

      call DRead(25,text,ios)
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      READ(cell(3)%s,*) knumEGr
      IF(knumEGr > 0) kEGr = 1
        call gtk_widget_set_sensitive(idpt('QFirst'), 1_c_int)
        call gtk_widget_set_sensitive(idpt('QThird'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('QSecond'), 0_c_int)
        if(knumEGr > 1) call gtk_widget_set_sensitive(idpt('QSecond'), 1_c_int)
        if(knumEGr > 2) call gtk_widget_set_sensitive(idpt('QThird'), 1_c_int)

      call DRead(25,text,ios)
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      READ(cell(3)%s,*) ngrs

      call DRead(25,text,ios)
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      READ(cell(3)%s,*) nab

      call DRead(25,text,ios)
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      READ(cell(3)%s,*) nmu

      call DRead(25,text,ios)
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      call WandelDPkt(cell(4)%s,1)
      call WandelDPkt(cell(5)%s,1)
      knetto = 0
      READ(cell(3)%s,*,IOSTAT=ios) knetto(1)
      READ(cell(4)%s,*,IOSTAT=ios) knetto(2)
      READ(cell(5)%s,*,IOSTAT=ios) knetto(3)

      call DRead(25,text,ios)
      call GetCells(text,cell,'u',enloc)
      call WandelDPkt(cell(3)%s,1)
      kbrutto = 0
      READ(cell(3)%s,*,IOSTAT=ios) kbrutto(1)
      READ(cell(4)%s,*,IOSTAT=ios) kbrutto(2)
      READ(cell(5)%s,*,IOSTAT=ios) kbrutto(3)

      ip_binom = 0
      kbgv_binom = 0
      itm_binom = 0
      ilam_binom = 0
      call DRead(25,text,ios)
      call GetCells(text,cell,'u',enloc)
      IF(INDEX(ucase(cell(2)%s),'BINPOI') > 0) then
        ip_binom = 0
        kbgv_binom = 0
        itm_binom = 0
        ilam_binom = 0
        READ(cell(3)%s,*,IOSTAT=ios) ip_binom
        READ(cell(4)%s,*,IOSTAT=ios) kbgv_binom
        READ(cell(5)%s,*,IOSTAT=ios) itm_binom
        READ(cell(6)%s,*,IOSTAT=ios) ilam_binom
           write(55,*) 'ipoi-params:',ip_binom,kbgv_binom,itm_binom,ilam_binom
      end if
      EXIT

    end if
  end do
  BACKSPACE 25
  BACKSPACE 25
  BACKSPACE 25

  if(allocated(kbrutto_name)) deallocate(kbrutto_name)
  allocate(kbrutto_name(knumEGr))
  if(allocated(knetto_name)) deallocate(knetto_name)
  allocate(knetto_name(knumEGr))

!WRITE(55,'(1x)')
!WRITE(55,*) ' nchannels, knumEGr, ngrs, nab, nmu, knetto, kbrutto=', nchannels, knumEGr, ngrs, nab, nmu, knetto, kbrutto

call InitVarsTV2(ngrs)
allocate(SymboleA(ngrs),SymboleB(ngrs))
do i=1,ngrs
  SymboleA(i)%s = ' '
  SymboleB(i)%s = ' '
end do

m1 = 0
suchwort = 'SYMBOLE:'
do
  m1 = m1 + 1
  IF(m1 > 200) EXIT
  call DRead(25,text,ios)
  IF(ios /= 0) EXIT
  IF(m1 > 15) EXIT  ! Suchwort not found
  IF(INDEX(ucase(text),TRIM(suchwort)) > 0) THEN

    do k=1,ngrs
      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,' ',enloc)
      READ(cell(1)%s,*) inum
      IF(inum == k) THEN
        Symbole(k)%s = TRIM(cell(2)%s)
        Symtyp(k)%s  = TRIM(cell(3)%s)
        Einheit(k)%s = TRIM(cell(4)%s)
        Bedeutung(k)%s = TRIM(cell(5)%s)

        WRITE(55,*) 'k=',k,' ',TRIM(Symbole(k)%s),' ',TRIM(symtyp(k)%s),' ',TRIM(einheit(k)%s), &
                    ' ',TRIM(bedeutung(k)%s)
          if(ucase(symtyp(k)%s) == 'M') then
            nvarsMD = nvarsMD + 1
            if(nvarsMD == 1) then
              if(.not.allocated(MDpoint)) then
                allocate(MDpoint(1))
                allocate(MDpointrev(ngrs))
                allocate(MDused(1))
              end if
            else
              call IntModA1(MDpoint,nvarsMD)
              call LogModA1(MDused,nvarsMD)
            end if
            MDpoint(nvarsMD) = k
            MDpointrev(k) = nvarsMD
            MDused(nvarsMD) = .true.
          end if
      else
        EXIT
      end if
    end do
    EXIT
  end if
end do
  BACKSPACE 25
  BACKSPACE 25
  BACKSPACE 25

allocate(SymboleG, source=Symbole)
do i=1,ngrs
  SymboleG(i)%s = ucase(symbole(i)%s)
end do

ugr = .TRUE.
m1 = 0
suchwort = 'WERTE, UNSICHERHEITEN:'
do
  m1 = m1 + 1
  IF(m1 > 200) EXIT
  call DRead(25,text,ios)
  IF(m1 > 15) THEN
    ugr = .FALSE.
    WRITE(55,*) 'Werte, Unsicherheiten NOT found'
    EXIT  ! Suchwort not found
  end if
  IF(INDEX(ucase(text),TRIM(suchwort)) > 0) THEN
    WRITE(55,*) 'Werte, Unsicherheiten found'

    do k=1,ngrs
      call DRead(25,text,ios)
            text = trim(text) // ctr//ctr
          write(55,*) 'ProRead_CSV: k=',k,' text=',TRIM(text)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,' ',enloc)
      READ(cell(1)%s,*) inum
      IF(inum == k) THEN
          do i=3,9
            call WandelDPkt(cell(i)%s,1)
          end do
            sdformel(k)%s = '  '
          READ(cell(3)%s,*) Messwert(k)
          READ(cell(4)%s,*) IVTL(k)
             if(len_trim(cell(5)%s) > 0)  sdformel(k)%s = cell(5)%s
             if(.false. .and. len_trim(cell(5)%s) > 0) then
               sdformel(k)%s = cell(5)%s
               do i=1,len_trim(sdformel(k)%s)
                 if(sdformel(k)%s(i:i) == ',') sdformel(k)%s(i:i) = '.'
               end do
             end if
          READ(cell(6)%s,*) SDwert(k)
          READ(cell(7)%s,*) HBreite(k)
          READ(cell(8)%s,*) IAR(k)
          READ(cell(9)%s,*) StdUnc(k)
          IAR(k) = max(1,IAR(k))   ! Precautionary
      else
        EXIT
      end if
    end do
    EXIT
  end if
end do

  BACKSPACE 25
  BACKSPACE 25
  BACKSPACE 25

cvgr = .TRUE.
call InitVarsTV3(100)
m1 = 0

ISymbA = 0        ! 27.6.2023
ISymbB = 0        !

icovtyp = 1
suchwort = 'COVARGRID:'
do
  m1 = m1 + 1
  IF(m1 > 200) EXIT
  call DRead(25,text,ios)
  IF(ios /= 0) EXIT
  IF(m1 > 10) THEN
    cvgr = .FALSE.
    EXIT  ! Suchwort not found
  end if
  IF(INDEX(ucase(text),TRIM(suchwort)) > 0) THEN
    ncov = 0
    do
      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
          text = trim(text)//ctr
      call GetCells(text,cell,' ',enloc)
      READ(cell(1)%s,*,IOSTAT=ios) inum
      IF(ios /= 0) EXIT
      ncov = ncov + 1
      IF(inum == ncov) THEN
        do i=2,6
          call WandelDPkt(cell(i)%s,1)
        end do
        READ(cell(2)%s,*) ISymbA(ncov)
        READ(cell(3)%s,*) ISymbB(ncov)
        READ(cell(4)%s,*) icovtyp(ncov)
        CVformel(ncov)%s = cell(5)%s
          if(ios /= 0) cvformel(ncov)%s = ' '
        READ(cell(6)%s,*) CovarVal(ncov)
           ISymbA(ncov) = ISymbA(ncov) - 1
           ISymbB(ncov) = ISymbB(ncov) - 1

        WRITE(55,*) ncov,' ',ISymbA(ncov),' ',ISymbB(ncov),' ',icovtyp(ncov), &
                    ' ',TRIM(CVformel(ncov)%s),' ',CovarVal(ncov)
      else
        EXIT
      end if
    end do
    EXIT
  end if
end do
  write(55,*) 'ncov=',ncov
  call IntmodA1(IsymbA,ncov)
  call IntmodA1(IsymbB,ncov)
  call IntmodA1(Icovtyp,ncov)
  call CharmodA1(CVFormel,ncov)
  call RealmodA1(Covarval,ncov)

  BACKSPACE 25
  BACKSPACE 25
  BACKSPACE 25

suchwort = 'ABKLINGGRID:'

abgr = .FALSE.
nwei = 0
nkovzr = 0
kpearson = 0
kPMLE = 0
kWTLS = 0
use_WTLS = .FALSE.
linfzbase = 1
numd = 0
m1 = 0
do
  m1 = m1 + 1
  IF(m1 > 200) EXIT
  call DRead(25,text,ios)
  IF(ios /= 0) EXIT
  IF(m1 > 15) THEN
    cvgr = .FALSE.
    EXIT  ! Suchwort not found
  end if
  IF(INDEX(ucase(text),TRIM(suchwort)) > 0) THEN
    abgr = .TRUE.
        FitDecay = .true.
        call InitVarsTV5(ndatmax)
    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'u',enloc)
    IF(INDEX(ucase(cell(1)%s),'MODPAR') > 0) THEN
      do i=1,3
        READ(cell(1+i)%s,*) ifit(i)
      end do
      READ(cell(5)%s,*) nwei
      READ(cell(6)%s,*) nkovzr

       if(max(ifit(1),ifit(2),ifit(3)) == 1) then
         do i=1,3
           if(ifit(i) == 0) ifit(i) = 3
         end do
       end if
      mfitfix = 0
      do i=1,3
        if(ifit(i) <= 2) mfitfix = mfitfix + 1
        !---cc  29.1.2024
        if(mfitfix == i .and. knumEGr < i .and. nmodf/nchannels == knumEGr) then
          ifit(i) = 3
          mfitfix = mfitfix - 1
        end if
        !---cc
      end do

      READ(cell(7)%s,*) kfitmeth
         IF(kfitmeth == 0) kpearson = 0
         IF(kfitmeth == 1) kpearson = 1
         kPMLE = 0
         IF(kfitmeth == 2) kPMLE = 1
         IF(kfitmeth == 3) kWTLS = 1
         fitmeth = 'WLS'
         IF(kpearson == 1) fitmeth = 'PLSQ'
         IF(kPMLE == 1) fitmeth = 'PMLE'
         IF(kWTLS == 1) fitmeth = 'WTLS'
         IF(kWTLS == 1) use_WTLS = .TRUE.

      READ(cell(8)%s,*) imenu1
      linfzbase = imenu1

      read(cell(9)%s,*,iostat=ios) ndefall
      defineallxt = .false.
      if(ios == 0 .and. ndefall == 1) defineallxt = .true.
    end if

    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'u',enloc)
    IF(INDEX(ucase(cell(1)%s),'TRENN-DATUM') > 0) THEN
      READ(cell(2)%s,'(a)') CFaelldatum
    end if
         use_absTimeStart = .false.                                 ! 24.7.2023
         if(len_trim(CFaelldatum) > 10) use_absTimeStart = .true.   !

    WRITE(55,*) 'Faelldatum=',CFaelldatum

    do k=1,ndatmax
      text = ' '
      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
         text = trim(text)//ctr
      call GetCells(text,cell,' ',enloc)
      IF(LEN_TRIM(cell(1)%s) == 0) EXIT

      READ(cell(1)%s,*) inum
      numd = numd + 1
        do i=3,15
          call WandelDPkt(cell(i)%s,1)
        end do
        CStartzeit(numd)%s = TRIM(cell(2)%s)
        i3 = 0       ! number of decimal points
        i2 = 1
25            i1 = INDEX(Cstartzeit(numd)%s(i2:),'.')
          ! WRITE(55,*) 'k=',k,'   i1,i2,i3=',i1,i2,i3,'   CStartzeit=',TRIM(CStartzeit(numd)),' -- ',cstartzeit(numd)(i2:)
        IF(i1 > 0) THEN
          i3 = i3 + 1
          i2 = i2 + i1
          GOTO 25
        end if
        IF(i3 == 1) call WandelDPkt(cell(2)%s,1)
        READ(cell(3)%s,*) dmesszeit(numd)
        READ(cell(4)%s,*) dbimpulse(numd)
        READ(cell(5)%s,*) dbzrate(numd)
        READ(cell(6)%s,*) sdbzrate(numd)
        READ(cell(7)%s,*) d0messzeit(numd)
        READ(cell(8)%s,*) d0impulse(numd)
        READ(cell(9)%s,*) d0zrate(numd)
        READ(cell(10)%s,*) sd0zrate(numd)
        READ(cell(11)%s,*) dnetrate(numd)
        READ(cell(12)%s,*) sdnetrate(numd)

        !WRITE(55,*) 'numd=',numd,' ',CStartzeit(numd)%s,' ',dmesszeit(numd),' ',dbimpulse(numd),   &
        !        ' ',dbzrate(numd),' ',sdbzrate(numd),' ',d0messzeit(numd),' ',d0impulse(numd), &
        !        ' ',d0zrate(numd),' ',sd0zrate(numd),' ',dnetrate(numd),' ',sdnetrate(numd)
    end do
       if(numd > 50) export_r = .false.
    EXIT
  end if
end do

  call CharmodA1(CStartzeit,numd)
  call RealmodA1(dmesszeit,numd)
  call RealmodA1(dbimpulse,numd)
  call RealmodA1(dbzrate,numd)
  call RealmodA1(sdbzrate,numd)
  call RealmodA1(d0messzeit,numd)
  call RealmodA1(d0impulse,numd)
  call RealmodA1(d0zrate,numd)
  call RealmodA1(sd0zrate,numd)
  call RealmodA1(dnetrate,numd)
  call RealmodA1(sdnetrate,numd)

  BACKSPACE 25
  BACKSPACE 25
  BACKSPACE 25

IF(.not.abgr) THEN
  do i=1,m1
    BACKSPACE 25
  end do
end if

suchwort = 'GAMSPK1-GRID:'
gsp1gr = .FALSE.
BACKSPACE 25

m1 = 0
do
  m1 = m1 + 1
  IF(m1 > 200) EXIT
   if(gsp1gr) exit

  call DRead(25,text,ios)
  IF(ios /= 0) EXIT
  IF(m1 > 25) THEN
    gsp1gr = .FALSE.
    EXIT  ! Suchwort not found
  end if
  IF(INDEX(ucase(text),TRIM(suchwort)) > 0) THEN
    gsp1gr = .TRUE.

    call InitVarsTV6(kdatmax)
    Gamspk1_Fit = .true.

    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'u',enloc)
    IF(INDEX(ucase(cell(1)%s),'UNITRADIO') > 0) THEN
      UnitR_effi_old = 0
      UnitR_pgamm_old = 0
      kkL = len_trim(cell(1+i)%s)
      if(kkL == 13 .or. kkL == 14) then
        read(cell(1+i)%s,*) UnitRadio(1),UnitR_effi_old,UnitRadio(2),UnitR_pgamm_old,UnitRadio(3:5)
      elseif(kkL == 9 .or. kkL == 10) then
        read(cell(1+i)%s,*) UnitRadio(1:5)
      end if
    end if

    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'u',enloc)
    IF(INDEX(ucase(cell(1)%s),'MEANTYP') > 0) THEN
      READ(cell(2)%s,*) kmwtyp
    end if

    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'u',enloc)
    IF(INDEX(ucase(cell(1)%s),'FBT') > 0) THEN
      call WandelDPkt(cell(2)%s,1)
      READ(cell(2)%s,*) FBT
    end if

    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'u',enloc)
    IF(INDEX(ucase(cell(1)%s),'ECORRUSE') > 0) THEN
      READ(cell(2)%s,*) ecorrUse
    end if

    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'u',enloc)
    IF(INDEX(ucase(cell(1)%s),'WMEXTSD') > 0) THEN
      READ(cell(2)%s,*) WMextSD
         WMextSD = 0         ! since 27.7.2022   shall no longer be used
    end if
  else
    CYCLE
  end if

  call WDPutSelRadio('radiobuttonG1', unitRadio(1))
  call WDPutSelRadio('radiobuttonG5', unitRadio(2))
  call WDPutSelRadio('radiobuttonG9', unitRadio(3))
  call WDPutSelRadio('radiobuttonG11', unitRadio(4))
  call WDPutSelRadio('radiobuttonG13', unitRadio(5))

  IF(kmwtyp > 0) THEN
    call WDSetComboboxAct('comboboxGMWtyp', kmwtyp)
  else
    call WDSetComboboxAct('comboboxGMWtyp', 1)
  end if
  call WDPutEntryDouble('entry_b2LFactor', FBT,'(f6.3)')
  call WDSetCheckButton('checkbuttonGspk1EffiCov', ecorruse)
  call WDSetCheckButton('checkbuttonMeanOpt', WMextSD)

  numd = 0
  do k=1,kdatmax

    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
         text = trim(text)//ctr
    call WandelDPkt(text,1)
    call GetCells(text,cell,' ',enloc)
    IF(LEN_TRIM(cell(1)%s) > 0) THEN
      read(cell(1)%s,*,iostat=ios) idummy
      if(ios /= 0 .or. idummy /= k) exit
      numd = numd + 1

      call IntmodA1(guse,numd)
      call RealmodA1(erg,numd)
      call RealmodA1(GNetRate,numd)
      call RealmodA1(RateCB,numd)
      call RealmodA1(RateBG,numd)
      call RealmodA1(SDRateBG,numd)
      call RealmodA1(effi,numd)
      call RealmodA1(SDeffi,numd)
      call RealmodA1(pgamm,numd)
      call RealmodA1(SDpgamm,numd)
      call RealmodA1(fatt,numd)
      call RealmodA1(SDfatt,numd)
      call RealmodA1(fcoinsu,numd)
      call RealmodA1(SDfcoinsu,numd)

      call RealmodA1(SDGNetRate,numd)

      READ(cell(2)%s,*) guse(numd)
      READ(cell(3)%s,*) erg(numd)
      READ(cell(4)%s,*) GNetRate(numd)
      READ(cell(5)%s,*) RateCB(numd)
      READ(cell(6)%s,*) RateBG(numd)
      READ(cell(7)%s,*) SDRateBG(numd)
      READ(cell(8)%s,*) effi(numd)
      READ(cell(9)%s,*) SDeffi(numd)
      READ(cell(10)%s,*) pgamm(numd)
      READ(cell(11)%s,*) SDpgamm(numd)
      READ(cell(12)%s,*) fatt(numd)
      READ(cell(13)%s,*) SDfatt(numd)
      READ(cell(14)%s,*) fcoinsu(numd)
      READ(cell(15)%s,*) SDfcoinsu(numd)

      if(abs(SDfcoinsu(numd) - missingval) < eps1min) SDfcoinsu(numd)= zero
      if(abs(RateBG(numd) - missingval) < eps1min) RateBG(numd)= zero
      if(abs(SDRateBG(numd) - missingval) < eps1min) SDRateBG(numd)= zero
        if(UnitR_effi_old == 1) effi(numd) = effi(numd)/100._rn
        if(UnitR_pgamm_old == 1) pgamm(numd) = pgamm(numd)/100._rn
    else
      EXIT
    end if
    !WRITE(55,*) 'numd=',numd,' ',erg(numd),' ',GNetRate(numd),                      &
    !          ' ',RateCB(numd),' ',RateBG(numd),' ',SDRAteBG(numd),                 &
    !          ' ',effi(numd),' ',SDeffi(numd),' ',pgamm(numd),' ',SDpgamm(numd),    &
    !          ' ',fatt(numd),' ',SDfatt(numd),' ',fcoinsu(numd),' ',SDfcoinsu(numd)
  end do

end do

  BACKSPACE 25
  BACKSPACE 25
  BACKSPACE 25

IF(.not.abgr) THEN
  do i=1,m1
    BACKSPACE 25
  end do
end if

suchwort = 'KALFIT-GRID:'
gkalf = .FALSE.
BACKSPACE 25

m1 = 0
do
  m1 = m1 + 1
  IF(m1 > 200) EXIT
  call DRead(25,text,ios)
  IF(ios /= 0) EXIT
  IF(m1 > 30) THEN
    gkalf = .FALSE.
    EXIT  ! Suchwort not found
  end if
  IF(INDEX(ucase(text),TRIM(suchwort)) > 0) THEN
    gkalf = .TRUE.
       WRITE(55,*) 'Kalfit-Grid: gefunden!'
    FitCalCurve = .true.

    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'n',enloc)
    READ(cell(1+1)%s,*) nkalpts
    READ(cell(1+2)%s,*) kal_polgrad
    READ(cell(1+3)%s,'(a)') CCtitle
    call InitVarsTV7(nkalpts)

    kk = 1
    if(len_trim(cell(1+4)%s) > 0) then
      READ(cell(1+4)%s,*) kk
    end if
    use_UfitKal = .true.
    if(kk == 0) use_UfitKal = .false.

    do k=1,nkalpts
      text = ' '
      call DRead(25,text,ios)
      IF(ios /= 0) EXIT
      call GetCells(text,cell,' ',enloc)
      IF(LEN_TRIM(cell(1)%s) == 0) EXIT
      IF(INDEX(cell(1)%s,'Titeltext') > 0) EXIT
      READ(cell(1)%s,*) inum
      do i=2,5
        call WandelDPkt(cell(i)%s,1)
      end do
      READ(cell(2)%s,*) xkalib(k)
      READ(cell(3)%s,*) uxkalib(k)
      READ(cell(4)%s,*) ykalib(k)
      READ(cell(5)%s,*) uykalib(k)

    end do

    FitCalCurve = .true.
    exit
  end if
end do

suchwort = 'MEANS:'
BACKSPACE 25
backspace 25
backspace 25
backspace 25
backspace 25
backspace 25
backspace 25

m1 = 0
do
  m1 = m1 + 1
  IF(m1 > 200) EXIT
  call DRead(25,text,ios)
  IF(ios /= 0) EXIT
  if(len_trim(text) == 0) exit
  call GetCells(text,cell,'n',enloc)

  IF(m1 > 30) THEN
    EXIT  ! Suchwort not found
  end if
  IF(INDEX(ucase(cell(1)%s),TRIM(suchwort)) > 0) THEN
       WRITE(55,*) 'Means: gefunden!'
    call DRead(25,text,ios)
    IF(ios /= 0) EXIT
    call GetCells(text,cell,'n',enloc)
    READ(cell(1)%s,'(a)') word
    if(trim(ucase(word)) /= 'MEANTYP') then
      ifehl = 1
      exit
    end if
    if(allocated(k_MDtyp)) deallocate(k_MDtyp)
    allocate(k_MDtyp(nvarsMD))
    allocate(fbayMD(nvarsMD))
    allocate(umeanMD(nvarsmd),nvMD(nvarsMD),smeanMD(nvarsMD),meanMD(nvarsMD))

    do i=1,nvarsMD
      read(cell(1+i)%s,*) k_MDtyp(i)
    end do

    call DRead(25,text,ios)
      if(ios /= 0) exit
    call WandelDPkt(text,1)
    call GetCells(text,cell,'n',enloc)
    word = cell(1)%s
    if(trim(ucase(word)) /= 'REFMEAN') then
      ifehl = 1
      ! exit
    end if
    READ(cell(1+1)%s,*,iostat=ios) refdataMD
       write(55,*) 'readcsv: refdataMD=',refdataMD

    if(allocated(nvalsMD)) deallocate(nvalsMD)
    allocate(nvalsMD(nvarsMD))
    if(allocated(meanID)) deallocate(meanID)
    allocate(meanID(nvarsMD))
    nn = 0
    allocate(ixdanf(nvarsMD))
    ixdanf(1) = 0
    allocate(xdataMD(1))
    xdataMD(1)= 0._rn

    do k=1,nvarsMD

      nvalsMD(k) = 0
      call DRead(25,text,ios)
      if(ios /= 0) exit

      call GetCells(text,cellk,'n',enloc)
      word = trim(cellk(1)%s)
      i1 = index(ucase(word),'_DATA')
      if(i1 > 0) then
        kk1 = FindlocT(SymboleG,ucase(word(1:i1-1)))
                 write(55,*) 'kk1=',kk1
        kk2 = 0
        if(kk1 > 0) kk2 = Findloc(MDpoint,kk1,dim=1)
        if(kk2 /= k) write( 55,*) '-------Problem: kk2 /= k!'
        if(kk2 > 0) then
          meanID(k)%s = trim(word)
        end if
      end if

      nvalsMD(k) = 0
      do i=1,100
        READ(cellk(1+i)%s,*,iostat=ios) zahl
        if(ios /= 0) then
             write(55,*) 'Lesefehler "zahl" in cellk: ',trim(cellk(1+i)%s)
          exit
        end if
        nvalsMD(k) = nvalsMD(k) + 1
        nn = nn + 1
        if(nvalsMD(k) == 1) ixdanf(k) = nn
        if(nn == 1) then
          xdataMD(nn) = zahl
          ixdanf(1)= nn
        else
          call RealModA1(xdataMD,nn)
          xdataMD(nn) = zahl
          if(nvalsMD(k) == 1) ixdanf(k) = nn
        end if
      end do

    end do
    exit

  end if
end do
!------------------------------------------------------------------------

alpha =  one - pnorm(kalpha)
beta =  one - pnorm(kbeta)
deallocate(cell,cellk)
deallocate(ttext,text)

close (25)

   if(Gamspk1_Fit) numd = numd*5

!-----------------------------------------------------------------------------
if(coverin > zero) then
  do i=nab+1,ngrs
    if(abs(Stdunc(i) - missingval) > eps1min) Stdunc(i) = StdUnc(i)/coverin
    if(abs(SDwert(i) - missingval) > eps1min) SDwert(i) = SDwert(i)/coverin
    if(abs(HBreite(i) - missingval) > eps1min) HBreite(i) = HBreite(i)/coverin

    if(abs(Stdunc(i) - missingval) > eps1min) &
                call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
    if(abs(SDwert(i) - missingval) > eps1min) &
                call WTreeViewPutDoubleCell('treeview2', 8, i, SDwert(i))
    if(abs(HBreite(i) - missingval) > eps1min) &
                 call WTreeViewPutDoubleCell('treeview2', 9, i, HBreite(i))
   write(55,*) 'Unsicherheiten / coverin dividert Symbol=',Symbole(i)%s, &
        ' StdUnc=',sngl(StdUnc(i)),' coverin=',int(coverin,2)
  end do
end if

call TransferToGTK(ugr,cvgr,fit,abgr,gsp1gr,imenu1,kmwtyp)

item_setintern = .false.

end subroutine ProRead_CSV
!-----------------------------------------------------------------------

!#######################################################################


end module PreadCSV
