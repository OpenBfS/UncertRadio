
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
use ur_variables
use ur_gleich
use ur_dlim
use ur_linft
use ur_gspk1fit


use Rout,                 only: WDGetTextviewString,WDGetComboboxAct,WDGetCheckButton, &
                                WDGetSelRadio,WTreeViewGetStrArray,WTreeViewGetDoubleArray, &
                                WTreeViewGetComboArray,WDGetEntryString,WDGetEntryDouble, &
                                WTreeViewGetCheckArray,UpdateProName,WDGetSelRadioMenu, &
                                FOpen
use Top,                  only: WrStatusbar,CharModA1
use RdSubs,               only: writeMDvec
use CHF,                  only: ucase
use UR_params,            only: eps1min,zero
use RG,                   only: modify_Formeltext
use CHF,                  only: flfu

implicit none

integer             :: k,i,imenu1,kxy,kmwtyp,i1,m1,j,kk,nk,maxi
integer             :: error_str_conv
character(len=2000) :: text                                   ! 12.8.2023
character(len=len(fname)+ 32) :: fname_tmp

character(len=2)    :: cdm
character(len=20)   :: cheader
logical             :: prot
!-----------------------------------------------------------------------
prot = .false.
  ! prot = .true.

text = ucase(fname)
i1 = LEN_TRIM(text)
IF(text(i1-3:i1) == '.CSV') THEN
  call ProSave_CSV()
  RETURN
end if

  cheader = 'Choose filename:'
  if(len_trim(fname)== 0) then
    call FOpen(ifehl, .true., cheader )
    if(ifehl == 1) return
  end if

IF(LEN_TRIM(fname) == 0) RETURN

call UpdateProName(fname)

fname_tmp = flfu(fname, error_str_conv)
if (error_str_conv > 0) write(*,*) 'Warning, could not convert file_name ' // &
                                   'to local encoding: ' // trim(fname_tmp)

open (25, FILE=TRIM(fname_tmp),STATUS='unknown')

Call WDGetTextviewString('textview1',Titeltext)
    if(.true. .and. prot) then
      write(66,*) 'PS: GetTiteltext after reworking ====================================='
        do i=1,size(Titeltext)
          write(66,*) trim(Titeltext(i)%s)
        end do
      write(66,*) 'PS: GetTiteltext after reworking ====================================='
    end if

Call WDGetTextviewString('textview2',Formeltext)
  ! remove empty equation lines at the end:
  do i=size(Formeltext),1,-1
    if(len_trim(Formeltext(i)%s) > 0) then
      call Charmoda1(Formeltext,i)
      exit
    end if
  end do

IF(FitDecay) then
  IF(FitDecay) THEN
    Call WDGetTextviewString('textviewModelEQ',FormeltextFit)
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

  END IF
  !IF(Gamspk1_Fit) THEN
  !END IF
End if

maxi = 1
do i=size(Titeltext),1,-1
  ! remove the last "empty" records of Titeltext:
  if(index(Titeltext(i)%s,char(32)//char(10)) == 1) cycle
  if(len_trim(Titeltext(i)%s) == 0) cycle
  maxi = i
  exit
end do

WRITE(25,'(a)') '@Titeltext:'
  do i=1,maxi
    WRITE(25,'(a)') Titeltext(i)%s
  end do
WRITE(25,'(a)') '@Formeltext:'

  do i=1,size(Formeltext)
    WRITE(25,'(a)') Formeltext(i)%s
  end do

if(allocated(FormeltextFit)) then
  IF(FitDecay .OR. (Gamspk1_Fit .and. len_trim(Formeltextfit(1)%s) > 0) .or.   &
                                                              SumEval_fit ) THEN
    WRITE(25,'(a)') '@FormeltextFit:'
    do i=1,size(FormeltextFit)
      WRITE(25,'(a)') FormeltextFit(i)%s
    end do
  END if
end if

WRITE(25,'(a)') '@Symbole-GRID:'
WRITE(25,'(a,i0)') 'nchs=',nchannels
WRITE(25,'(a,i0)') 'nEGr=',knumEGr
WRITE(25,'(a,i0)') 'ngrs=',ngrs
WRITE(25,'(a,i0)') 'nab=',nab
WRITE(25,'(a,i0)') 'nmu=',nmu

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

do k=1,ngrs
  WRITE(25,'(10a)') Symbole(k)%s,' #',symtyp(k)%s,' #',einheit(k)%s, &
              ' #',bedeutung(k)%s,' #'
end do

WRITE(25,'(a)') '@Menu1 und Menu2:'
WRITE(25,'(a,3i3)') 'knetto=',knetto(1),knetto(2),knetto(3)
WRITE(25,'(a,3i3)') 'kbrutto=',kbrutto(1),kbrutto(2),kbrutto(3)

WRITE(25,'(a)') '@Unc-Grid:'
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
  if(k > nab .and. coverin > zero) then   !
    if(abs(Stdunc(k) - missingval) > eps1min) Stdunc(k) = StdUnc(k)*coverin
    if(abs(SDwert(k) - missingval) > eps1min) SDwert(k) = SDwert(k)*coverin
    if(abs(HBreite(k) - missingval) > eps1min) HBreite(k) = HBreite(k)*coverin
  end if
  if(k == 3) then
    write(66,*) 'len(sdformel(k)%s)=',len(sdformel(k)%s)
    write(66,*) 'len(text)=',len(text)
  end if

  WRITE(text,'(a,1x,a2,es23.15e2,a2,i3,a2,a,a2,2(es23.15e2,a2),i2,a2,es23.15e2,a2)')    &
                Symbole(k)%s,' #', real(Messwert(k),8),' #',IVTL(k),' #',  &
                sdformel(k)%s,' #',real(sdwert(k),8),' #',real(HBreite(k),8),' #', &
                IAR(k),' #',real(StdUnc(k),8),' #'
    do j=1,20
      m1 = index(text, '  ')
      if(m1 == 0) exit
      text = text(1: m1-1) // trim(text(m1+2:))
    end do
  WRITE(25,'(a)') ADJUSTL(TRIM(text))
end do

WRITE(25,'(a)') '@Covar-Grid:'
IF(ncov > 0) THEN
  call WTreeViewGetComboArray('treeview3', 2, ncov, ISymbA)
  call WTreeViewGetComboArray('treeview3', 3, ncov, ISymbB)
  call WTreeViewGetComboArray('treeview3', 4, ncov, icovtyp)
  call WTreeViewGetStrArray('treeview3', 5, ncov, CVFormel)
  call WTreeViewGetDoubleArray('treeview3', 6, ncov, CovarVal)

  do i=1,ncov
    CVformel(i)%s   = ADJUSTL(CVformel(i)%s)
  end do
  do k=1,ncov
    ! For compatability with an old UR version, ISymbA andIsymbB
    ! are written here as number increased by +1 :
    WRITE(text,'(3(i3,a2),a,a2,es23.15e2,a2)') ISymbA(k)+1,' #',ISymbB(k)+1,' #',icovtyp(k),' #',  &
                  CVformel(k)%s,' #',real(CovarVal(k),8),' #'
    WRITE(25,'(a)') ADJUSTL(TRIM(text))
  end do
END IF

IF(FitDecay) THEN
  WRITE(25,'(a)') '@Abkling-Grid:'
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

  WRITE(25,'(a,7i2)') 'ModPar=',(ifit(i),i=1,3),nwei,nkovzr,kfitmeth, ndefall
  WRITE(25,'(a)') CFaelldatum
  WRITE(25,*) imenu1

  do k=1,numd
    WRITE(text,'(a,1x,a2,10(es23.15e2,a2))') trim(CStartzeit(k)%s),' #',real(dmesszeit(k),8),' #', &
                  real(dbimpulse(k),8),' #',  &
                  real(dbzrate(k),8),' #',real(sdbzrate(k),8),' #',real(d0messzeit(k),8),' #',     &
                  real(d0impulse(k),8),' #',real(d0zrate(k),8),' #',real(sd0zrate(k),8),' #',      &
                  real(dnetrate(k),8),' #',real(sdnetrate(k),8),' #'
    do j=1,20
      m1 = index(text, '  ')
      if(m1 == 0) exit
      text = text(1: m1-1) //  trim(text(m1+2:))
    end do
    WRITE(25,'(a)') ADJUSTL(TRIM(text))
  end do
END IF
IF(Gamspk1_Fit) THEN
  WRITE(25,'(a)') '@Gamspk1-Grid:'
  call WDGetSelRadio('radiobuttonG1', unitRadio(1))
  call WDGetSelRadio('radiobuttonG5', unitRadio(2))
  call WDGetSelRadio('radiobuttonG9', unitRadio(3))
  call WDGetSelRadio('radiobuttonG11', unitRadio(4))
  call WDGetSelRadio('radiobuttonG13', unitRadio(5))

  WRITE(25,'(a,5i2)') 'UnitRadio=',(UnitRadio(i),i=1,5)

  call WDGetComboboxAct('comboboxGMWtyp', kmwtyp)
  WRITE(25,'(a,i2)') 'MeanTyp=',kmwtyp

  call WDGetEntryDouble('entry_b2LFactor', FBT)
  WRITE(25,'(a,f7.4)') 'FBT=',real(FBT,8)

  call WDGetCheckButton('checkbuttonGspk1EffiCov', ecorruse)
  WRITE(25,'(a,i1)') 'EcorrUse=',ecorrUse
  ! WRITE(25,'(a,i1)') 'WMextSD=',WMextSD

  if(numd > 0) then
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
      WRITE(text,'(i2,a2,13(es23.15e2,a2))') guse(k),' #',real(Erg(k),8),' #',real(GNetRate(k),8),' #', &
                  real(RateCB(k),8),' #', real(RateBG(k),8),' #',real(SDRateBG(k),8),' #',  &
                  real(effi(k),8),' #',real(SDeffi(k),8),' #',real(pgamm(k),8),' #',real(SDpgamm(k),8),' #',     &
                  real(fatt(k),8),' #',real(SDfatt(k),8),' #',real(fcoinsu(k),8),' #',real(SDfcoinsu(k),8),' #'
      do j=1,20
        m1 = index(text, '  ')
        if(m1 == 0) exit
        text = text(1: m1-1) //  trim(text(m1+2:))
      end do
      WRITE(25,'(a)') ADJUSTL(TRIM(text))
    end do
  end if
END IF

if(FitCalCurve) then
  WRITE(25,'(a)') '@Kalfit-Grid:'
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
    WRITE(25,'(a)') ADJUSTL(TRIM(text))
  end do
end if

WRITE(25,'(a)') '@Sonstige:'
WRITE(25,'(a,f8.6)') 'kalpha=',real(kalpha,8)
WRITE(25,'(a,f8.6)') 'kbeta=',real(kbeta,8)
WRITE(25,'(a,f5.3)') 'coverf=',real(coverf,8)
if(coverin > zero) WRITE(25,'(a,f5.3)') 'coverin=',real(coverin,8)
WRITE(25,'(a,f6.4)') '1-gamma=',real(W1minusG,8)
WRITE(25,'(a,f6.4)') 'GamDistAdd=',real(GamDistAdd,8)
call WDGetSelRadioMenu('MT_NegLin',k)
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

call WrStatusBar(3,' ')

end subroutine ProSave

!#######################################################################


end module PSave
