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
subroutine batest()

!   copyright (c) 2014-2023  günter kanisch

    use, intrinsic :: iso_c_binding
    use gtk,                only:   gtk_buttons_ok,gtk_message_error,gtk_main_iteration, &
                                    gtk_message_info,gtk_widget_hide
    use ur_variables,       only:   project_loadw,fname,fname_getarg, batest_on, &
                                    michel_opt1, batest_user,langg, batest_ref_file_ch, &
                                    batest_out_ch, dir_sep, &
                                    work_path, example_path, results_path
    use ur_gleich,          only:   knumegr,kegr,ucomb,symbole,messwert,nab,kbrutto, &
                                    knetto,klinf,kgspk1, &
                                    ifehl,coverf
    use ur_dlim
    use ur_linft,           only: fitdecay
    use ur_gtk_variables,   only: item_setintern
    use gtk,                only: gtk_widget_show,gtk_widget_set_visible
    use rout,               only: pending_events,wdputentrystring,messageshow,wdputentryint
    use top,                only: idpt
    use urdate,             only: get_formated_date_time
    use ur_interfaces,      only: processloadpro_new
    use ur_params,          only: rn, batest_out, Batest_ref_file
    use file_io,            only: logger, write_text_file
    use usub3,              only: saveresults
    use chf,                only: flfu

    implicit none

    integer            :: ios,isk,ifg,kwh,ke,ndevs,ndevs_new,nfd2,k2
    integer            :: i1, ivalues(13)

    character(len=355) :: text19, text18, str1, iomessg
    character(len=255) :: fname_rel
    character(len=64)  :: fname_old(6)
    character(len=20)  :: xsymbol
    character(len=3)   :: cnum
    character(:), allocatable :: full_filename_batest_out, full_filename_batest_resu
    real(rn)           :: start,finish
    integer(c_int)     :: resp
    logical            :: isoneumi,equalqty
    character(len=512) :: log_str
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

    call cpu_time(start)

    ! File (17) contains the actual list of txp project filenames:
!     close (17)
!     if(.not. batest_user) then
!         open (17, file=flfu(work_path //'BatList_20171212.txt'), status='old',iostat=ios)
!         IF(ios /= 0) THEN
! !             WRITE(66,*) 'File BatList_20171212.txt not found!'
!             call logger(66, 'File BatList_20171212.txt not found!')
!             return
!         end if
!     endif
    fname_getarg = ' '
    batest_on = .TRUE.
    if(batest_user) call gtk_widget_hide(idpt('box4'))

    ! File (19) contains contains the reference values of results, used for comparison
    close (19)
    if(.not. batest_user) then
        open(19, file=flfu(work_path // Batest_ref_file), status='old',IOSTAT=ios)
        IF(ios /= 0) then
            write(log_str, '(*(g0))') 'File BatList-Ref file not found!','   IOS=',ios
            call logger(66, log_str)
            return
        end if
    else
        open(19, file=flfu(batest_ref_file_ch), status='old',iostat=ios)
        if(ios /= 0) then
            if(langg == 'DE') then
                write(str1,*) 'Datei ' // trim(Batest_ref_file_ch) // ' kann nicht geöffnet werden!'
            else
                write(str1,*) 'File ' // trim(Batest_ref_file_ch) // ' cannot be opened!'
            endif
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Batest:", resp,mtype=GTK_MESSAGE_ERROR)

            write(log_str, '(*(g0))') 'File '  // trim(Batest_ref_file_ch) // ' not found!','   IOS=',ios
            call logger(66, log_str)
            batest_on = .false.
            ifehl = 1
            return
        end if
    endif

    ! Files (20) and (18) are files, to which is written here
    if(.not. batest_user) then
        full_filename_batest_out = results_path // Batest_out
    else
        full_filename_batest_out = batest_out_ch
    endif

    ! this is the old unit=20 file
    call write_text_file(text='Test started on: ' // get_formated_date_time(), &
                         full_filename=full_filename_batest_out, &
                         status='new')

    ! if(.not.batest_user) then
    !     do
    !         READ(17,'(a)',IOSTAT=ios) Zeile
    !         IF(ios /= 0)  EXIT
    !         if(len_trim(zeile) == 0) exit
    !         call logger(18, TRIM(Zeile))
    !     end do
    !     call logger(18, ' ')
    ! end if

    ! this is the old unit=18 file
    full_filename_batest_resu = results_path // 'BatList-Resu.txt'
    write(log_str, '(a,41x,a,a)') 'File ', &
                                  'quantity    value y      u(y)         BestVal      u(BV)' // &
                                  '        ylow         yhigh        DT           DL ',  &
                                  '          NT k       ka      kb      1-g'
    call write_text_file(text=log_str, &
                         full_filename=full_filename_batest_resu, &
                         status='new')
    isk = 0
    if( .not. batest_user) rewind 17
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
        if( .not. batest_user) then
            read(17,'(a)',iostat=ios) fname
            if(ios /= 0) exit
        else
            read(19,'(a)',iostat=ios,iomsg=iomessg) text19
            IF(ios /= 0) exit
            read(text19,'(a)',iostat=ios,iomsg=iomessg) fname
            fname = fname(1:46)
        endif

        if(len_trim(fname) == 0) cycle
        fname = adjustL(fname)
        if(fname(1:1) == '#') cycle     !  filename entries starting with # are omitted (not evaluated)

        fname_rel = trim(fname)
        isoneuMi = .false.
        if(index(fname_rel,'isoneu_') > 0 .and. index(fname_rel,'.') > 10) isoneuMi = .true.

        !
        fname = trim(example_path) // 'de' // dir_sep // trim(fname)
        if(stat(flfu(fname), ivalues) /= 0) then
            i1 = index(fname, dir_sep // 'de' // dir_sep)
            if(i1 > 0) then
                fname = fname(1:i1) // 'en' // dir_sep // trim(fname(i1+4:))
                if(stat(flfu(fname), ivalues) /= 0) then
                    i1 = index(fname, example_path)
                    if(i1 > 0) then
                        fname = fname(1:i1) // trim(fname(i1+9:))
                        if(stat(flfu(fname), ivalues) /= 0) then
                            if(langg == 'DE') then
                                write(str1,*) 'Datei ',TRIM(fname),' kann nicht geöffnet werden!'
                            else
                                write(str1,*) 'File ',TRIM(fname),' cannot be opened!'
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


        if( .not. batest_user)  then
            call logger(67, "Datei=" // trim(fname))
        end if

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

            if(.false. .and. .not.batestMC .and. klinf == 0 .and. kgspk1 == 0) then
!                 if(kbrutto(1) > nab) write(62,*) 'nab=',nab,'  kbrutto(1)=',kbrutto(1),' File=',trim(fname)
                if(kbrutto(1) > nab)  then
                    write(log_str, '(*(g0))') 'nab=',nab,'  kbrutto(1)=',kbrutto(1),' File=',trim(fname)
                    call logger(62, log_str)
                end if
!                 if(knetto(1) > nab) write(62,*)  'nab=',nab,'  knetto(1) =',knetto(1),' File=',trim(fname)
                if(knetto(1) > nab)  then
                    write(log_str, '(*(g0))') 'nab=',nab,'  knetto(1) =',knetto(1),' File=',trim(fname)
                    call logger(62, log_str)
                end if
            end if

            if(batestMC) then
                item_setintern = .true.
                call WDPutEntryInt('TRentryMCanzM', 75000)
                if(.not.FitDecay) call WDPutEntryInt('TRentryMCanzM', 200000)
                if(Michel_opt1) call WDPutEntryInt('TRentryMCanzM', 1000000)
            end if

            xsymbol = adjustl(symbole(kEGr)%s)
            write(text18,'(a,1x,a10,1x,8(es12.5,1x),1x,i2,1x,4(f7.5,1x))') &
                fname_rel(1:45),xsymbol(1:10), real(Messwert(kEGr),8),real(Ucomb,8), &
                real(WertBayes,8),real(UcombBayes,8),real(KBgrenzu,8),real(KBgrenzo,8),        &
                real(decthresh,8),real(detlim,8),1,real(Coverf,8),real(kalpha,8),real(kbeta,8), &
                real(W1minusG,8)

            call write_text_file(text=trim(text18), full_filename=full_filename_batest_resu)
            if(batest_user) then
                ifg = 1
                IF(text19(1:3) == 'AKS') text19(1:3) = 'ISO'

                if(trim(text19(1:57)) == trim(text18(1:57)) ) then    ! same project name, same numer of output quantity
                    call Bcompare(text18, text19, fname_rel, equalqty, full_filename_batest_out)
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
                rewind 19
                ifg = 0
                do
                    read(19,'(a)',IOSTAT=ios) text19
                    if(ios /= 0) exit
                    if(text19(1:5) == 'Datei') then
                        ifg = 1
                        cycle
                    endif
                    if(ifg == 0) cycle
                    if(ifg == 1) then
                        if(text19(1:3) == 'AKS') text19(1:3) = 'ISO'
                        if(trim(text19(1:57)) == trim(text18(1:57)) ) then
                            call Bcompare(text18, text19, fname_rel, equalqty, &
                                          full_filename_batest_out)
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

        IF(.false. .and. .not. batestMC .and. knumEGr > 1 .AND. FitDecay) THEN
            call ProcessLoadPro_new(1,2)      ! Aufruf für die 2. Ergebnisgröße
            write(text18,'(a,1x,a10,1x,8(es12.5,1x),1x,i2,1x,4(f7.5,1x))')  &
                fname_rel(1:45),adjustL(Symbole(kEGr)%s), real(Messwert(kEGr),8),real(Ucomb,8), &
                real(WertBayes,8),real(UcombBayes,8),real(KBgrenzu,8),real(KBgrenzo,8),        &
                real(decthresh,8),real(detlim,8),1,real(Coverf,8),real(kalpha,8),real(kbeta,8), &
                real(W1minusG,8)
        endif
    enddo

    call cpu_time(finish)
    batest_on = .false.

    if(langg == 'DE')  then
        write(log_str, '(A, F0.2)') 'Ende des Tests !    Run-time (s) : ', finish-start
    else if(langg == 'EN' .or. langg == 'FR')  then
        write(log_str, '(A, F0.2)') 'End of test !    Run-time (s) : ', finish-start
    end if

    call write_text_file(text=log_str, full_filename=full_filename_batest_out)
    call write_text_file(text=' ', full_filename=full_filename_batest_out)


    close (17)
    close (19)


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

subroutine Bcompare(text18,text19,fname_rel,equalqty, full_filename)
    use file_io,           only: write_text_file
    use UR_params,         only: rn,EPS1MIN

    implicit none

    character(len=*),intent(in)  :: text18,text19
    character(len=*),intent(in)  :: fname_rel
    character(len=*),intent(inout)  :: full_filename
    logical,intent(out)          :: equalqty

    integer                      :: kcol,ng,i,ios
    character(len=512)           :: log_str
    real(rn)                     :: rat(8), deltaa, v18, v19

    equalqty = .true.

    IF(trim(text19(58:)) == trim(text18(58:))) then
        return        !  all values agree
    endif

    if(trim(text19(47:57)) /= trim(text18(47:57))) then
        equalqty = .false.
        return     ! not the same output quantities
    endif

    rat = 0.0_rn
    ng = 0
    deltaa = 0.0_rn
    ios = 0
    do kcol=1,8
        read(text18(58+(kcol-1)*13:58+(kcol)*13),*) v18
        read(text19(58+(kcol-1)*13:58+(kcol)*13),*,iostat=ios) v19
        if(ios /= 0) exit

        if(abs(v18) > EPS1MIN) then
            rat(kcol) = v19/v18
            deltaa = max(deltaa, abs(1.0_rn-rat(kcol)))
        else
            rat(kcol) = 0.0_rn
            deltaa = 0.0_rn
        endif
    enddo
    if(deltaa > 3.E-5_rn .or. index(text18,'NaN') > 0 .or. ios /= 0) then
        call write_text_file(text=fname_rel(1:45) // '  Difference found!', &
                             full_filename=full_filename)

        call write_text_file(text='        new: ' // trim(text18), &
                             full_filename=full_filename)

        if(ios == 0) then
            call write_text_file(text='        old: ' // trim(text19), &
                                 full_filename=full_filename)

            write(log_str, '(57x,a, 8(es12.5,1x))') 'ratio        ', (rat(i),i=1,8)
            call write_text_file(text=log_str, &
                                 full_filename=full_filename)
        else
            call write_text_file(text='        old: does not exist!', &
                                 full_filename=full_filename)
        endif
        equalqty = .false.
    endif

end subroutine Bcompare
