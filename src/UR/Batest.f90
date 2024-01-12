
subroutine Batest()

!   Copyright (C) 2014-2023  Günter Kanisch

use, intrinsic :: iso_c_binding
use gtk,                only: GTK_BUTTONS_OK,GTK_MESSAGE_ERROR,gtk_main_iteration, &
                              GTK_MESSAGE_INFO,gtk_widget_hide
USE UR_Variables,       only: project_loadw,fname,fname_getarg, batest_on, &
                              Michel_opt1,batest_user,langg, Batest_ref_file_ch, &
                              Batest_out_ch, dir_sep, &
                              work_path, example_path, results_path
USE UR_Gleich,          ONLY: knumEGr,kEgr,Ucomb,Symbole,Messwert,nab,kbrutto,knetto,klinf,kgspk1, &
                              ifehl,coverf
USE UR_DLIM
USE UR_Linft,           ONLY: FitDecay
use UR_gtk_variables,   only: consoleout_gtk,item_setintern
use gtk,                only: gtk_widget_show,gtk_widget_set_visible
use Rout,               only: pending_events,WDPutEntryString,MessageShow,WDPutEntryInt
use top,                only: idpt
use URdate,             only: datim
use UR_interfaces,      only: ProcessLoadPro_new
use UR_params,          only: rn,eps1min
use Usub3,              only: SaveResults

implicit none

integer(4)         :: ios,isk,ifg,kwh,kE,ndevs,ndevs_new,nfd2,k2
INTEGER(4)         :: zt1(9),i1,ivalues(13)
CHARACTER(LEN=355) :: Zeile
CHARACTER(LEN=355) :: text19,text18,str1,iomessg
character(len=255) :: fname_rel
character(len=255) :: fname_old(6)
character(len=20)  :: xsymbol
character(len=3)   :: cnum
real(rn)           :: start,finish
integer(c_int)     :: resp
logical            :: isoneuMi,equalqty
logical            :: batestmc

!-----------------------------------------------------------------------

fname_old(1) = 'La140_REMSPEC-4Linien-V3_DE.txp'
fname_old(2) = 'La140_REMSPEC-4Lines-V3_EN.txp'
fname_old(3) = 'Mehr-Linien-Nuklid-Aktivitaet-V3_DE.txp'
fname_old(4) = 'Several-peaks-nuclide-activity-V3_EN.txp'
fname_old(5) = 'sumEval_mitteln_V2_DE.txp'
fname_old(6) = 'sumEval_mean_V2_EN.txp'

project_loadw = .true.
! loadingpro = .true.

batestmc = .false.
ndevs = 0
ndevs_new = 0

call gtk_widget_set_visible(idpt('textview1'), 0_c_int)

call gtk_widget_set_visible(idpt('box3'), 0_c_int)
call gtk_widget_set_visible(idpt('box4'), 0_c_int)
call gtk_widget_set_visible(idpt('box5'), 0_c_int)
call gtk_widget_set_visible(idpt('grid5'), 0_c_int)
call gtk_widget_set_visible(idpt('box7'), 0_c_int)

if(batest_user) then
  call gtk_widget_hide(idpt('box2'))
  call gtk_widget_hide(idpt('box3'))
endif

CALL datim (zt1)
CALL CPU_TIME(start)

! File (17) contains the actual list of txp project filenames:
close (17)
if(.not. batest_user) then
  open (17,FILE=trim(work_path)//'BatList_20171212.txt', STATUS='old',IOSTAT=ios)
  IF(ios /= 0) THEN
    WRITE(66,*) 'File BatList_20171212.txt not found!'
    Return
  END IF
endif
fname_getarg = ' '
batest_on = .TRUE.
if(batest_user) call gtk_widget_hide(idpt('box4'))

! File (19) contains contains the reference values of results, used for comparison
CLOSE (19)
if(.not.batest_user) then
  OPEN(19,FILE=trim(work_path)//'BatList-Resu_20120902.txt',STATUS='old',IOSTAT=ios)
  IF(ios /= 0) THEN
    WRITE(66,*) 'File BatList-Resu_20120902.txt not found!','   IOS=',ios
    Return
  END IF
else
  OPEN(19,FILE=Batest_ref_file_ch,STATUS='old',IOSTAT=ios)
  IF(ios /= 0) THEN
    if(langg == 'DE') then
       WRITE(str1,*) 'Datei ' // trim(Batest_ref_file_ch) // ' kann nicht geöffnet werden!'
    else
       WRITE(str1,*) 'File ' // trim(Batest_ref_file_ch) // ' cannot be opened!'
    endif
    call MessageShow(trim(str1), GTK_BUTTONS_OK, "Batest:", resp,mtype=GTK_MESSAGE_ERROR)
       WRITE(66,*) 'File '  // trim(Batest_ref_file_ch) // ' not found!','   IOS=',ios
    batest_on = .false.
    ifehl = 1
    Return
  END IF
endif

! Files (20) and (18) are files, to which is written here
if(.not.batest_user) then
  open (20,FILE=results_path // 'vgltest.txt')
else
  open (20,FILE=batest_out_ch)
endif
REWIND 20
WRITE(20,'(a,2x,i2.2,''.'',i2.2,''.'',i4,1x,i2.2,'':'',i2.2)') 'Test started on:',zt1(6),zt1(7),zt1(8),zt1(5),zt1(4)

close (18)
OPEN (18,FILE=results_path // 'BatList-Resu.txt', STATUS='unknown',IOSTAT=ios)
rewind 18

if(.not.batest_user) then
  do
    READ(17,'(a)',IOSTAT=ios) Zeile
    IF(ios /= 0)  EXIT
      if(len_trim(zeile) == 0) exit
    WRITE(18,'(a)') TRIM(Zeile)
  end do
  WRITE(18,'(1x)')
end if
WRITE(18,'(a,41x,a,a)') 'File ', &
           'quantity    value y      u(y)         BestVal      u(BV)        ylow         yhigh        DT           DL ',  &
           '          NT k       ka      kb      1-g'
isk = 0
if(.not.batest_user) REWIND 17
if(batest_user) read(19,'(1x)')

! flo: not used / not working atm
!
! if(batestMC) then
!   call FNopen(76)
!   write(76,*) 'Projekt',';', 'Symbol',';','kmmt',';','xmit1_anf',';','xmit1',';',  &
!                   'xmit1_anf/xmit1',';','DT_anf',';','DT',';','DT/DT_anf'
! endif

do
  isk = isk + 1
  ios = 0
  if(.not.batest_user) then
    READ(17,'(a)',iostat=ios) fname
    if(ios /= 0) exit
  else
    READ(19,'(a)',IOSTAT=ios,iomsg=iomessg) text19
        IF(ios /= 0) EXIT
    READ(text19,'(a)',IOSTAT=ios,iomsg=iomessg) fname
         fname = fname(1:46)
  endif

  IF(LEN_TRIM(fname) == 0) CYCLE
  fname = adjustL(fname)
  IF(fname(1:1) == '#') CYCLE     !  filename entries starting with # are omitted (not evaluated)

  fname_rel = trim(fname)
  isoneuMi = .false.
  if(index(fname_rel,'isoneu_') > 0 .and. index(fname_rel,'.') > 10) isoneuMi = .true.

  !
  fname = trim(example_path) // 'de' // dir_sep // trim(fname)
      if(STAT(fname, ivalues) /= 0) then
        i1 = index(fname, dir_sep // 'de' // dir_sep)
        if(i1 > 0) then
          fname = fname(1:i1) // 'en' // dir_sep // trim(fname(i1+4:))
          if(STAT(fname, ivalues) /= 0) then
            i1 = index(fname, example_path)
            if(i1 > 0) then
              fname = fname(1:i1) // trim(fname(i1+9:))
              if(STAT(fname, ivalues) /= 0) then
                if(langg == 'DE') then
                  WRITE(str1,*) 'Datei ',TRIM(fname),' kann nicht geöffnet werden!'
                else
                  WRITE(str1,*) 'File ',TRIM(fname),' cannot be opened!'
                endif
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Batest:", resp, mtype=GTK_MESSAGE_ERROR)
                ifehl = 1
                batest_on = .false.
                return
              end if
            endif
          endif
        endif
      endif
  if(Michel_opt1 .and. batestMC .and. .not. isoneuMi) cycle

  if(batestMC .and. (index(fname,'_EN.') > 0 .or. index(fname,'_en.') > 0)   &
       .and. Index(fname,'IAEA') == 0 .and. Index(fname,'Sterlinski') == 0  &
       .and. Index(fname,'weight_Cox') == 0      ) cycle

  if(.not.batest_user) WRITE(67,*) 'Datei=',TRIM(fname)

  WRITE(0,*) 'Datei=',TRIM(fname)

          if(consoleout_gtk) write(0,*)
          if(consoleout_gtk) write(0,*) 'Project=',trim(fname)
              call gtk_widget_show(idpt('window1'))
  ios = 0
  project_loadw = .TRUE.

  do kE=1,2
    if(ke == 2 .and. knumEGr == 1) exit
    if(kE == 2 .and. ios /= 0) exit
    if(kE == 2 .and. .not.(.not.batestMC .and. knumEGr > 1)) cycle

    if(kE == 1) call ProcessLoadPro_new(0,1)       ! call for the 1. output quantity
    if(kE == 2) then
      if(batest_user) read(19,*) xsymbol
      call ProcessLoadPro_new(1,2)      ! call for the 2. output quantity
    endif
    if(ifehl == 1) return
    kwh = 0
27        continue
    if(.false. .and. .not.batestMC .and. klinf == 0 .and. kgspk1 == 0) then
      if(kbrutto(1) > nab) write(62,*) 'nab=',nab,'  kbrutto(1)=',kbrutto(1),' File=',trim(fname)
      if(knetto(1) > nab) write(62,*)  'nab=',nab,'  knetto(1) =',knetto(1),' File=',trim(fname)
    end if

    if(batestMC) then
      item_setintern = .true.
      call WDPutEntryInt('TRentryMCanzM', 75000)
      if(.not.FitDecay) call WDPutEntryInt('TRentryMCanzM', 200000)
         if(Michel_opt1) call WDPutEntryInt('TRentryMCanzM', 1000000)
    end if

    xsymbol = adjustl(symbole(kEGr)%s)
    WRITE(text18,'(a,1x,a10,1x,8(es12.5,1x),1x,i2,1x,4(f7.5,1x))') &
                 fname_rel(1:45),xsymbol(1:10), real(Messwert(kEGr),8),real(Ucomb,8), &
                 real(WertBayes,8),real(UcombBayes,8),real(KBgrenzu,8),real(KBgrenzo,8),        &
                 real(decthresh,8),real(detlim,8),1,real(Coverf,8),real(kalpha,8),real(kbeta,8), &
                 real(W1minusG,8)
    WRITE(18,'(a)') TRIM(text18)
    if(batest_user) then
      ifg = 1
      IF(text19(1:3) == 'AKS') text19(1:3) = 'ISO'

      IF(TRIM(text19(1:57)) == TRIM(text18(1:57)) ) THEN    ! same project name, same numer of output quantity
        call Bcompare(text18,text19,fname_rel,equalqty)
        if(.not. equalqty) then
          ndevs = ndevs + 1
          nfd2 = 0
          do k2=1,6
            if(trim(fname_rel) == trim(fname_old(k2))) nfd2=1
          end do
          if(nfd2 == 0) ndevs_new = ndevs_new + 1
          cycle
        endif
      endif
    else
      REWIND 19
      ifg = 0
      do
        read(19,'(a)',IOSTAT=ios) text19
        IF(ios /= 0) EXIT
        IF(text19(1:5) == 'Datei') THEN
          ifg = 1
          CYCLE
        ENDIF
        IF(ifg == 0) CYCLE
        if(ifg == 1) then
          IF(text19(1:3) == 'AKS') text19(1:3) = 'ISO'
          IF(TRIM(text19(1:57)) == TRIM(text18(1:57)) ) THEN
            call Bcompare(text18,text19,fname_rel,equalqty)
            if(.not. equalqty) then
              ndevs = ndevs + 1
              nfd2 = 0
              do k2=1,6
                if(trim(fname_rel) == trim(fname_old(k2))) nfd2=1
              end do
              if(nfd2 == 0) ndevs_new = ndevs_new + 1

              exit
            endif
          endif
        endif
      enddo
    endif
  end do  ! kE=1,2

  IF(.false. .and. .not.batestMC .and. knumEGr > 1 .AND. FitDecay) THEN
    call ProcessLoadPro_new(1,2)      ! Aufruf für die 2. Ergebnisgröße
    WRITE(text18,'(a,1x,a10,1x,8(es12.5,1x),1x,i2,1x,4(f7.5,1x))')  &
               fname_rel(1:45),adjustL(Symbole(kEGr)%s), real(Messwert(kEGr),8),real(Ucomb,8), &
               real(WertBayes,8),real(UcombBayes,8),real(KBgrenzu,8),real(KBgrenzo,8),        &
               real(decthresh,8),real(detlim,8),1,real(Coverf,8),real(kalpha,8),real(kbeta,8), &
               real(W1minusG,8)
  ENDIF
enddo

CALL CPU_TIME(finish)
batest_on = .false.
if(langg == 'DE') WRITE(20,*) 'Ende des Tests !    Run-time (s) : ',sngl(finish-start)
if(langg == 'EN' .or. langg == 'FR') WRITE(20,*) 'End of test !    Run-time (s) : ',sngl(finish-start)

write(20,*)
! if(langg == 'DE') then
!   write(20,'(A)') 'Abweichungen für "Mehr-Linien-Nuklid-Aktivitaet-V3_DE.txp",'
!   write(20,'(A)') '"La140_REMSPEC-4Linien-V3_DE.txp" und "sumEval_mitteln_V2_DE.txp"'
!   write(20,'(A)') 'gehen auf eine Programmänderung in Dez-2020 zurück.'
! endif
! if(langg == 'EN' .or. langg == 'FR') then
!   write(20,'(A)') 'Deviations reported for "Several-peaks-nuclide-activity-V3_EN.txp",'
!   write(20,'(A)') '"La140_REMSPEC-4Lines-V3_EN.txp" and "sumEval_mean_V2_EN.txp"'
!   write(20,'(A)') 'are due to a program modification in Dec-2020.'
! endif
close (18)
close (17)
close (19)
close (20)

if(batest_user) then    ! 28.6.2019
  call gtk_widget_show(idpt('box2'))
  call gtk_widget_show(idpt('box3'))
endif

if(.not. batest_user) return
    call gtk_widget_show(idpt('box4'))

if(ndevs == 0) then
  if(langg == 'DE') write(str1,*) 'Test beendet: keine Abweichungen!'
  if(langg == 'EN') write(str1,*) 'Test finished: no deviations!'
  if(langg == 'FR') write(str1,*) 'Test finished: no deviations!'
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "Batest:", resp,mtype=GTK_MESSAGE_INFO)
else
  write(cnum,'(i3)') ndevs_new
  if(langg == 'DE') write(str1,'(a,i0,a)') 'Test beendet: Abweichungen bei ',ndevs,' Projekten gefunden!' &
                    // char(13) // ' Anzahl unbekannter Abweichungen:' // cnum  &
                    // char(13) // ' Details: siehe Ausgabedatei ' // trim(Batest_out_ch) //'!'
  if(langg == 'EN') write(str1,'(a,i0,a)') 'Test finished: deviations found for ',ndevs,' projects!' &
                    // char(13) // ' Number of unknown deviations:' // cnum  &
                    // char(13) // ' Details: see output file  ' // trim(Batest_out_ch) //'!'
  if(langg == 'FR') write(str1,'(a,i0,a)') 'Test finished: deviations found for ',ndevs,' projects!' &
                    // char(13) // ' Nombre d''écarts inconnus:' // cnum  &
                    // char(13) // ' Détails: voir le fichier de sortie  ' // trim(Batest_out_ch) //'!'
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "Batest:", resp,mtype=GTK_MESSAGE_INFO)
endif

end subroutine Batest

!#############################################################################

subroutine Bcompare(text18,text19,fname_rel,equalqty)
use UR_params,    only: rn,eps1min,zero,one

implicit none

character(len=*),intent(in)  :: text18,text19
character(len=*),intent(in)  :: fname_rel
logical,intent(out)          :: equalqty

integer(4)       :: kcol,ng,i,ios
real(rn)         :: rat(8),deltaa,v18,v19

equalqty = .true.

IF(TRIM(text19(58:)) == TRIM(text18(58:))) then
    goto 55
  return        !  all values agree
endif

if(trim(text19(47:57)) /= trim(text18(47:57))) then
  equalqty = .false.
  return     ! not the same output quantities
endif

rat = zero
ng = 0
deltaa = zero
ios = 0
do kcol=1,8
  read(text18(58+(kcol-1)*13:58+(kcol)*13),*) v18
  read(text19(58+(kcol-1)*13:58+(kcol)*13),*,iostat=ios) v19
   if(ios /= 0) exit

  if(abs(v18) > eps1min) then
    rat(kcol) = v19/v18
    deltaa = max(deltaa, abs(one-rat(kcol)))
  else
    rat(kcol) = zero
    deltaa = zero
  endif
enddo
if(deltaa > 3.E-5_rn .or. index(text18,'NaN') > 0 .or. ios /= 0) then
  WRITE(20,*) fname_rel(1:45),'  Difference found!'
  WRITE(20,*) '        new: ',trim(text18)
  if(ios == 0) then
    WRITE(20,*) '        old: ',trim(text19)
    write(20,'(58x,a, 8(es12.5,1x))') 'ratio        ',(sngl(rat(i)),i=1,8)
  else
    WRITE(20,*) '        old: does not exist!'
  endif
  equalqty = .false.
endif

55  continue

end subroutine Bcompare
