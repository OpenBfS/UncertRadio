
module Usub3

    !     contains
    ! FindMessk
    ! AutoReportWrite
    ! errwrite
    ! SaveResults
    ! TransToTV2

contains

!#######################################################################

    pure integer function FindMessk(k) result(messk)
        use UR_Linft, only: nchannels, numd
        implicit none
        integer, intent(in) :: k

        messk = (k - 1) / (numd / nchannels) + 1

    end function FindMessk

!#######################################################################

    subroutine autoreportwrite()

        ! uncertradio can be started from an excel application for evaluating
        ! a project the filename of which was transferred to uncertradio
        ! as part of the uncertradios command line arguments (which also
        ! contains an argument like 'auto' or 'autosep').
        ! in this mode of operation, ur executes the evaluation and finally
        ! saves the set of result values as a record into a csv file with
        ! the name 'autoreport-result.csv' (using unit # 21);
        !
        ! see chapter 5. of the uncertradio chm help file for more details.

        !     copyright (c) 2014-2023  günter kanisch

        use, intrinsic :: iso_c_binding
        use ur_variables
        use ur_gleich
        use ur_linft
        use ur_dlim
        use ur_gspk1fit
        use ur_mcc
        use ur_perror
        use, intrinsic :: iso_c_binding,   only: c_int
        use gtk,             only: gtk_widget_set_visible, &
                                   gtk_buttons_ok_cancel, gtk_response_ok, gtk_response_cancel, &
                                   gtk_buttons_ok, gtk_message_warning
        use top,             only: idpt, charmodstr
        use rout,            only: messageshow, pending_events, wdgetentrydouble
        use urdate,          only: get_formated_date_time
        use ur_interfaces,   only: processloadpro_new
        use chf,             only: ucase

        implicit none

        integer                 :: i,i1,ios
        integer                 :: finfo(13),ik, neg, ibc

        character(len=50)       :: smpid
        character(len=20)       :: tdatum
        character(len=1)        :: ctr
        character(len=22)       :: cnum
        logical                 :: lexist,outsepar
        character(:),allocatable  :: fng,fncsv, str1,tpart,str2,text18,text19,cfnam,fname_auto_sep
        character(:),allocatable  :: btext,textb,text18b
        real(rn)                :: pe,upe,be,ube,lq,uq,slq,suq,dt,dl

        integer(c_int)          :: resp
!---------------------------------------------------------------------------------------
        ctr = sListSeparator         ! ';'

        autoreport = .true.
        ifehl = 0
        ibc = 0

        outsepar = .false.
        if(index(ucase(fname),'ZZURPR.') == 0 .and. ucase(cgetarg(1)%s) == 'AUTOSEP') outsepar = .true.

        allocate(character(len=800) :: fnG,fncsv, str1,tpart,str2,cfnam,fname_auto_sep)
        allocate(character(len=1500) :: text18,text19,textb,text18b,btext)

        call gtk_widget_set_visible(idpt('box3'), 0_c_int)
        call gtk_widget_set_visible(idpt('box4'), 0_c_int)
        call gtk_widget_set_visible(idpt('box5'), 0_c_int)
        call gtk_widget_set_visible(idpt('grid5'), 0_c_int)
        call gtk_widget_set_visible(idpt('box7'), 0_c_int)

        tdatum = get_formated_date_time()

        if(.not.outsepar) then
            fnG = trim(ucase(fname_getarg))
            fncsv = results_path // 'AutoReport-Result.csv'
        else
            fname = fname_getarg
            i1 = index(ucase(fname),'.CSV')
            if(i1 > 0) then
                if(index(fname,':') == 0) then
                    fname_auto_sep = results_path // trim(fname(1:i1-1))//'_res.csv'
                else
                    fname_auto_sep = trim(fname(1:i1-1))//'_res.csv'
                end if
                write(66,*) 'fname_auto_sep=',trim(fname_auto_sep)
            end if

        end if
        smpid = trim(sample_ID)

        !  finfo(9): file size in kB
        !  finfo(1): file size in Byte, or -1, if not existing

        if(outsepar) then
            !-------- 16.5.2024
26          continue
            call stat(fname_auto_sep,finfo)
            inquire(file=fname_auto_sep, exist=lexist)
            ifehl = 0
            if(.not.lexist) then
                open (18,file=trim(fname_auto_sep), status='unknown',position='append',iostat=ios)
                if(ios == 2) open (18,file=trim(fname_auto_sep), status='new',position='append',iostat=ios)
                if(ios /= 0 .and. ios /= 2) then
                    call CharModStr(str1,1200)
                    if(langg == 'DE') write(str1,*) 'Fehler beim Öffnen von ' // trim(fname_auto_sep) // ', ios=',ios,char(13), &
                        'Wenn die Datei geöffnet ist: diese bitte erst schliessen, dann OK geben,',char(13), &
                        'ansonsten Cancel klicken!'
                    if(langg == 'EN') write(str1,*) 'Error when opening ' // trim(fname_auto_sep) // ', ios=',ios,char(13), &
                        'If file is open: close it and then click OK, otherwise Cancel!'
                    if(langg == 'FR') write(str1,*) 'Erreur lors de l''ouverture ' // trim(fname_auto_sep) // ', ios=',ios,char(13), &
                        'Si le fichier est ouvert: fermez-le et cliquez sur Valider, sinon Annuler!'
                    call MessageShow(trim(str1), GTK_BUTTONS_OK_CANCEL, "AutoReportWrite:", resp,mtype=GTK_MESSAGE_WARNING)
                    if(resp == GTK_RESPONSE_OK) goto 26
                    if(resp == GTK_RESPONSE_CANCEL) then
                        ifehl = 1
                        return
                    end if
                end if
            end if
            !----------

            goto 32
        end if

        call STAT(fncsv, finfo)
        Inquire(file=fncsv, exist=lexist)

28      continue
        if (.not. lexist) then
            open(21, file=TRIM(fncsv), status='new', action='write', iostat=ios)
            write(21,'(100a)') '"#"',ctr,'"File"',ctr,'"Sample_id"',ctr, '"Date"',ctr,'"quantity"',ctr, &
                '"PE"',ctr, '"uPE"',ctr,'"BE"',ctr,'"uBE"',ctr,'"LQ"',ctr,'"UQ"',ctr,'"sLQ"',ctr,'"sUQ"',ctr,&
                '"DT*"',ctr,'"DL#"',ctr,'"NT"',ctr,'"k"',ctr,'"kalpha"',ctr,'"kbeta"',ctr,  &
                '"1-gamma"',ctr,'"Chisqr"'
        else
            open(21, FILE=TRIM(fncsv), STATUS='old', action='read', IOSTAT=ios)
        end if
        close(21)
        write(66,*) 'fncsv=',trim(fncsv)
        write(66,'(a,I0,a,i0,a,L1,a,a)') 'AutoReportWrite: File 21: finfo(8) in KB=',finfo(8)/1024,'  ios=',ios,' lexist=',lexist,' file=',trim(fncsv)
        if(ios /= 0 .and. ios /= 2) then
            call CharModStr(str1,1200)
            IF(langg == 'DE') write(str1,*) 'Fehler beim Öffnen von ' // trim(fncsv) // ', ios=',ios,char(13), &
                'Wenn die Datei geöffnet ist: diese bitte erst schliessen, dann OK geben,',char(13), &
                'ansonsten Cancel klicken!'
            IF(langg == 'EN') write(str1,*) 'Error when opening ' // trim(fncsv) // ', ios=',ios,char(13), &
                'If file is open: close it and then click OK, otherwise Cancel!'
            IF(langg == 'FR') write(str1,*) 'Erreur lors de l''ouverture ' // trim(fncsv) // ', ios=',ios,char(13), &
                'Si le fichier est ouvert: fermez-le et cliquez sur Valider, sinon Annuler!'
            call MessageShow(trim(str1), GTK_BUTTONS_OK_CANCEL, "AutoReportWrite:", resp,mtype=GTK_MESSAGE_WARNING)
            if(resp == GTK_RESPONSE_OK) goto 28
            if(resp == GTK_RESPONSE_CANCEL) then
                ifehl = 1
                return
            end if
        end if

        if (lexist) then
            ifehl = 0
            open(21, FILE=TRIM(fncsv), STATUS='old', action='read', IOSTAT=ios)
            if(ios /= 0) call errwrite(.true.,21,'  ', trim(fncsv),ios, resp)

            if(ios == 0) then
                read(21,'(a)') text18   ! headline
                read(21,'(a)', iostat=ifehl) text18
                write(66,*) 'Warning: There is no second line in the AutoReport-Result.csv file, ios',int(ios,2),' ifehl=',int(ifehl,2)
                if(ifehl == 0) then
                    i1 = index(text18,sListSeparator)
                    if(i1 == 0 .or. i1 > 50) then
                        ifehl = 1
                        write(66,*) 'A:   i1=',int(i1,2),' sListSeparator=',sListSeparator,' text18=',trim(text18)
                    end if
                    if(ifehl == 0) then
                        i1 = index(text18(120:),sDecimalPoint)
                        if(i1 == 0) then
                            ifehl = 1
                            write(66,*) 'B:'
                        end if
                    end if
                end if
            end if
            close(21)
            if(ifehl == 1) then
                write(66,*) 'C:'
                call CharModStr(str1,800)
                write(66,*) 'Fehler bei "Save to CSV": das Listentrennzeichen von UR2: "',sListSeparator,'"', &
                    char(13),' oder der Dezimalpunkt von UR2: "',sDecimalPoint,'"', &
                    char(13),' wurde in der Datei ',trim(fncsv),' nicht gefunden!', char(13), &
                    char(13),' Das kann z. B. an der in UR2 gewählten Sprache liegen!).', &
                    char(13),' Bitte überprüfen. Siehe auch Optionen - Voreinstellungen.'
                if(langg == 'DE') then
                    write(str2,*)  'Fehler bei "Save to CSV": das Listentrennzeichen von UR2: "',sListSeparator,'"', &
                        char(13),' oder der Dezimalpunkt von UR2: "',sDecimalPoint,'"', &
                        char(13),' wurde in der Datei ',trim(fncsv),' nicht gefunden!', char(13), &
                        char(13),' Das kann z. B. an der in UR2 gewählten Sprache liegen!).', &
                        char(13),' Bitte überprüfen. Siehe auch Optionen - Voreinstellungen.'

                end if
                if(langg == 'EN') write(str2,*) 'Error with "Save to CSV": the list separator of UR2: "',sListSeparator,'"', &
                    char(13),' or the decimal point of UR2: "',sDecimalPoint,'"', &
                    char(13),' was not found in the file ',trim(fncsv), '!' , char(13), &
                    char(13),' One reason, e.g., might be an incompatible language selected in UR2.', &
                    char(13),' Please, check. See also Options - Pre-settings.'
                if(langg == 'FR') write(str2,*) 'Erreur avec "enregistrer au format CSV": le séparateur de liste de UR2: "',sListSeparator,'"', &
                    char(13),' ou le point décimal de UR2: "',sDecimalPoint,'"', &
                    char(13),' n''a pas été trouvé dans le fichier ',trim(fncsv), '!' , char(13), &
                    char(13),' Une raison, par exemple, pourrait être un langage incompatible sélectionné dans UR2.', &
                    char(13),' Vérifiez s''il vous plaît. Voir aussi Options - Préférences.'
                call MessageShow(trim(str2), GTK_BUTTONS_OK, "SaveResults:", resp,mtype=GTK_MESSAGE_WARNING)
                return
            end if
        end if

        open(21, file=TRIM(fncsv), status='old ', position='append', action='write', iostat=ios)
        fname = TRIM(fname_getarg)
        WRITE(66,*) 'Datei=',TRIM(fname)
        ibc = 1
        do i=LEN_TRIM(fname_getarg),1,-1
            IF(fname(i:i) == dir_sep) THEN
                ibc = i+1
                EXIT
            end if
        end do

32      continue

        autoreport = .TRUE.

        neg = 1        ! Number of the output quantity

34      continue
        if(neg == 1) call ProcessLoadPro_new(0,1)      ! call for the first output quantity
        if(neg > 1)  call ProcessLoadPro_new(1,neg)    ! call for the output quanity number neg
        call CharModStr(str1,500)
        IF(ifehl == 1) GOTO 9000
        call pending_events()

        write(66,*) 'behind call ProcessLoadPro B :   autoreport=',autoreport
        IF(.not.FitDecay .AND. .not.Gamspk1_Fit .and. .not.SumEval_fit) Chisqr = -1._rn

        call WDGetEntryDouble('TRentryValue', PE)
        call WDGetEntryDouble('TRentryUnc', uPe)
        call WDGetEntryDouble('TRentryValueBy', BE)
        call WDGetEntryDouble('TRentryUncBy', uBe)

        call WDGetEntryDouble('TRentryLQBy', LQ)
        call WDGetEntryDouble('TRentryUQBy', UQ)
        sLQ = KBgrenzuSH
        sUQ = KBgrenzoSH

        write(cnum,'(es12.5)') sLQ
        read(cnum,*) sLQ
        write(cnum,'(es12.5)') sUQ
        read(cnum,*) sUQ

        if(.not.gum_restricted) then
            call WDGetEntryDouble('TRentryDT', DT)
            call WDGetEntryDouble('TRentryDL', DL)
        end if

        if(outsepar) then
            ! Write to CSV file:
            !----------------------
            ! fcv = coverin/coverf
            !----------------------

            if(neg == 1) then

                write(btext,'(a)') 'Filename; date_time;#EG; PE; uPE; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL; ;'
                write(textb,*) (Symbole(i)%s,ctr,'u(' // Symbole(i)%s // ')',ctr, i=knumEGr+1,nab)
                btext = trim(btext) // trim(textb)

                if(ctr == ',') then
                    do i=1,len_trim(btext)
                        if(btext(i:i) == ';') btext(i:i) = ctr
                    end do
                end if
                call errwrite(.false.,18,trim(btext),fname_auto_sep,ios,resp)
                if(ifehl == 1) return
            end if
            write(text18,'(4a,i2,a1,10(es17.10,a1))') fname_auto_sep,ctr,tdatum,ctr,int(kEGr,2),ctr,  &
                PE,ctr,uPE,ctr,BE,ctr,uBE,ctr,LQ,ctr, &
            !    PE,ctr,uPE*fcv,ctr,BE,ctr,uBE*fcv,ctr,LQ,ctr, &        ! <-- 20.5.2021
                UQ,ctr,sLQ,ctr,sUQ,ctr,DT,ctr,DL,ctr

            text18 = adjustL(text18)
            write(text18b,'(a1,100(es17.10,a1))') ctr,(Messwert(i),ctr,StdUnc(i)*coverf,ctr, i=knumEGr+1,nab)
            text18 = trim(text18) // trim(text18b)

            if(sDecimalPoint /= '.') then
                ik = index(ucase(text18),'.CSV') + 4
                do i=ik,LEN_TRIM(text18)
                    IF(text18(i:i) == '.') text18(i:i) = sDecimalPoint
                end do
            end if
            call errwrite(.false.,18,text18,fname_auto_sep,ios,resp)
            if(ifehl == 1) return
            goto 40
        end if

        if(.not.outsepar) then

            ! Write to CSV file:
            write(text18,'(i2,a1,4(a,a1),10(es17.10,a1),i1,a,4(f7.5,a1),es12.5,a1)') &
                neg,ctr,'"'//TRIM(fname(ibc:))//'"',ctr,'"'//TRIM(smpid)//'"',ctr,TRIM(tdatum),ctr,  &
                '"'//TRIM(Symbole(kEGr)%s)//'"',ctr, &
                PE,ctr,uPE,ctr,BE,ctr,uBE,ctr, &
                LQ,ctr,UQ,ctr,sLQ,ctr,sUQ,ctr,DT,ctr,DL,ctr, &
                1,ctr,Coverf,ctr,kalpha,ctr,kbeta,ctr,W1minusG,ctr,chisqr,ctr
            write(tpart,'(i2,a1,4(a,a1))') neg,ctr,'"'//TRIM(fname(ibc:))//'"',ctr,'"'//TRIM(smpid)//'"',ctr,TRIM(tdatum)
            ik = index(text18,':') + 2
            IF(sDecimalPoint /= '.') then
                do i=ik,LEN_TRIM(text18)
                    if(text18(i:i) == '.') text18(i:i) = sDecimalPoint
                end do
            end if
            call errwrite(.false.,21,trim(text18),TRIM(fncsv),ios,resp)
            if(ifehl == 1) return

        end if

        project_loadw = .TRUE.

40      neg = neg + 1
        IF(neg > knumEGr) GOTO 200
        if(outsepar) goto 34
        if(FitDecay .and. ifit(neg) > 1) goto 40
        goto 34

200     CONTINUE

        call pending_events()

!---------------------------------------------------------------------------
9000    CONTINUE

        close (21)
        autoreport = .FALSE.

        deallocate(fnG,fncsv, str1,tpart,str2,text18,text19,cfnam)

    end subroutine AutoReportWrite

!#######################################################################

    subroutine errwrite(openf,kunit,text18,filename,ios,resp)

        ! this routine helps to solve the following problem:
        !   It may happen that the desired csv file to be used for saving
        !   data is still opened, which leads to an error when UR tries
        !   to open it. The user gets a message and gets the opportunity
        !   of closing the csv file before accepting the message. UR then
        !   can open the file successfully and continue with saving the record.
        !
        !     Copyright (C) 2010-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,   only: c_int
        use UR_VARIABLES,    only: langg
        use Top,             only: CharModStr
        use UR_Gleich,       only: ifehl
        use gtk,             only: GTK_BUTTONS_OK_CANCEL, &
                                   GTK_MESSAGE_WARNING
        use Rout,            only: MessageShow

        implicit none

        logical,intent(in)            :: openf
        integer(4),intent(in)         :: kunit
        character(len=*),intent(in)   :: text18
        character(len=*),intent(in)   :: filename
        integer(4),intent(inout)      :: ios
        integer(c_int),intent(out)    :: resp

        character(len=10)             :: cios
        character(len=:),allocatable  :: str1

        allocate(character(len=600)   :: str1)

        call CharModStr(str1,800)
        ! build str1 as concatenated strings:
        IF(langg == 'DE') str1 = 'Fehler beim Schreiben auf ' // trim(filename) // ', ios=' // trim(cios) //char(13) &
            // 'Wenn die Datei geöffnet ist: diese bitte erst schliessen, dann OK geben,' // char(13) &
            // 'ansonsten Cancel klicken!'
        IF(langg == 'EN') str1 = 'Error when writing to ' // trim(filename) // ', ios=' // trim(cios) // char(13) &
            // 'If file is open: close it and then click OK, otherwise Cancel!'
        IF(langg == 'FR') str1 = 'Erreur lors de l''écriture sur ' // trim(filename) // ', ios=' // trim(cios) // char(13) &
            // 'Si le fichier est ouvert: fermez-le et cliquez sur Valider, sinon Annuler!'

33      continue

        ifehl = 0

        if(.not.openf) then
            ios = 0
            WRITE(kunit,'(a)',IOSTAT=ios) TRIM(text18)
            IF(ios /= 0) THEN
                if(ios == 1 .or. ios == 6) then
                    call MessageShow(trim(str1), GTK_BUTTONS_OK_CANCEL, "AutoReportWrite:", resp,mtype=GTK_MESSAGE_WARNING)
                    if(resp == 0) then
                        ifehl = 1
                        return
                    end if
                    if(resp == 1) then
                        !   If answer 'Yes'
                        goto 33
                    END IF
                end if
            end if
        else
            ! after open file did fail at the first time:
            ifehl = 0
            IF(ios /= 0) THEN
                if(ios == 1 .or. ios == 6) then
                    call MessageShow(trim(str1), GTK_BUTTONS_OK_CANCEL, "AutoReportWrite:", resp,mtype=GTK_MESSAGE_WARNING)
                    ifehl = 1
                end if
            end if

        end if

    end subroutine errwrite

!###############################################################################


    subroutine SaveResults()

        ! this routine reacts to the SaveCSV button and saves the set of characteristic
        ! values to a csv file named UR-Saved-Results.csv, in the form of one record per
        ! UR evaluation. If this file already exists, it opens the file appends a record
        ! and closes the file again, otherwise, it creates this file, writes a header
        ! record and then the record from the UR evaluation and closes the file.
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding
        use gtk,                    only: GTK_BUTTONS_OK, GTK_MESSAGE_WARNING
        USE UR_Variables
        USE UR_Interfaces

        USE UR_Gleich
        USE UR_Linft
        USE UR_DLIM
        USE UR_Mcc
        USE UR_perror
        use Rout,             only: WDGetEntryInt, WDGetEntryDouble, MessageShow
        use top,              only: DRead, CharModStr
        use URdate,           only: get_formated_date_time
        use UR_params,        only: eps1min

        implicit none

        integer                   :: i, i1, ios
        integer                   :: k, finfo(13), ik, ibc

        CHARACTER(LEN=10)         :: fityp
        CHARACTER(LEN=20)         :: tdatum
        CHARACTER(LEN=1)          :: ctr
        integer(4)                :: nmctot
        integer(c_int)            :: resp
        logical                   :: lexist
        character(:),allocatable  :: str1, tpart, str2, text16, cfnam
!---------------------------------------------------------------------------------------
        ctr = sListSeparator         ! ';'
        ifehl = 0

        allocate(character(len=900)  ::  text16)
        allocate(character(len=1100) ::  str2, tpart)
        allocate(character(len=300)  ::  str1, cfnam)

        tdatum = get_formated_date_time()

        cfnam = results_path // 'UR-Saved-Results.csv'    ! unit 16 is used for writing
        !  finfo(9): file size in kB
        !  finfo(1): file size in Byte, or -1, if not existing
        call STAT(cfnam,finfo)
        Inquire(file=cfnam, exist=lexist)
        write(66,*) 'SaveResults:   cfnam=',trim(cfnam),'   lexist=',lexist,'  filesize(byte)=',finfo(8), &
            'P-file = ',trim(fname)
        ibc = 1
        do i=LEN_TRIM(fname),1,-1
            IF(fname(i:i) == dir_sep) THEN
                ibc = i+1
                EXIT
            end if
        end do

        if(lexist .and. finfo(8) > 0) then
            OPEN (16,FILE=cfnam, STATUS='unknown',IOSTAT=ios)
            if(ios == 0) then
                read(16,'(1x)')    ! 1 headline
                call DRead(16,text16,ios)
                i1 = index(text16,sListSeparator)
                if(i1 == 0 .or. i1 > 50) then
                    ifehl = 1
                end if
                if(ifehl == 0) then
                    i1 = index(text16(120:),sDecimalPoint)
                    if(i1 == 0) then
                        ifehl = 1
                    end if
                end if
            end if
            close (16)
            if(ifehl == 1) then
                call CharModStr(str1,400)
                if(langg == 'DE') write(str2,*)  'Fehler bei "Save to CSV": das Listentrennzeichen von UR2: "',sListSeparator,'"', &
                    char(13),' oder der Dezimalpunkt von UR2: "',sDecimalPoint,'"', &
                    char(13),' wurde in der Datei ',trim(cfnam), ' nicht gefunden!', char(13), &
                    char(13),' Das kann z. B. an der in UR2 gewählten Sprache liegen.', &
                    char(13),' Bitte überprüfen. Siehe auch Optionen - Voreinstellungen.'
                if(langg == 'EN') write(str2,*) 'Error with "Save to CSV": the list separator of UR2: "',sListSeparator,'"', &
                    char(13),' or the decimal point of UR2: "',sDecimalPoint,'"', &
                    char(13),' was not found in the file ',trim(cfnam), '!' , char(13), &
                    char(13),' One reason, e.g., might be an incompatible language selected in UR2.', &
                    char(13),' Please, check. See also Options - Pre-settings.'
                if(langg == 'FR') write(str2,*) 'Erreur avec "enregistrer au format CSV": le séparateur de liste de UR2: "',sListSeparator,'"', &
                    char(13),' ou le point décimal de UR2: "',sDecimalPoint,'"', &
                    char(13),' n''a pas été trouvé dans le fichier ',trim(cfnam), '!' , char(13), &
                    char(13),' Une raison, par exemple, pourrait être un langage incompatible sélectionné dans UR2.', &
                    char(13),' Vérifiez s''il vous plaît. Voir aussi Options - Préférences.'
                call MessageShow(trim(str2), GTK_BUTTONS_OK, "SaveResults:", resp,mtype=GTK_MESSAGE_WARNING)
                return
            end if
        end if

22      continue

        OPEN (16,FILE=cfnam, STATUS='unknown',POSITION='Append',IOSTAT=ios)
        if(ios /= 0) then
            write(66,*) 'Save to csv:  open: ios=',ios
            call errwrite(.true.,16,'  ',cfnam,ios,resp)
            if(resp == 1_c_int) then
                goto 22
            else
                ifehl = 1
                return
            end if
        end if

        Call WDGetEntryInt('TRentryMCanzM', kcmx)
        Call WDGetEntryInt('TRentryMCanzR', kcrun)

        Call WDGetEntryDouble('TRentryMCValue', xmit1)
        Call WDGetEntryDouble('TRentryMCunc', xsdv)
        Call WDGetEntryDouble('TRentryMClq', xLQ)
        Call WDGetEntryDouble('TRentryMCuq', xUQ)

        Call WDGetEntryDouble('TRentryMCdt', xDT)
        Call WDGetEntryDouble('TRentryMCdl', xDL)

        nmctot = kcmx*kcrun

        fityp = ' '
        if(FitDecay) fityp = fitmeth

        if(.not.lexist .or. finfo(8) == 0) then
            WRITE(16,'(100a)') 'Comment',ctr,'CalcType',ctr,' ',ctr,'"#"',ctr,'"File"',ctr, &
                '"Date"',ctr,'"FitMeth"',ctr,'"quantity"',ctr,'"PE"',ctr, &
                '"uPE"',ctr,'"BE"',ctr,'"uBE"',ctr,'"LQ"',ctr,'"UQ"',ctr, &
            !  '"uPE"',ctr,'"uPE/PE"',ctr,'"BE"',ctr,'"uBE"',ctr,'"uPE/PE"',ctr, '"LQ"',ctr,'"UQ"',ctr, &
                '"sLQ"',ctr,'"sUQ"',ctr, &
                '"DT*"',ctr,'"DL#"',ctr,'"NT"',ctr,'"k"',ctr,'"kalpha"',ctr,'"kbeta"',ctr,  &
                '"1-gamma"',ctr,'"Chisqr"',ctr,'u(Fitp)',ctr,'Up(Fitp)',ctr,  &
                '"a1(1)"',ctr,'"a2(1)"',ctr,'"a3(1)"',ctr,  &
                '"ua1(1)"',ctr,'"ua2(1)"',ctr,'"ua3(1)"',ctr,  &
                '"a1(2)"',ctr,'"a2(2)"',ctr,'"a3(2)"',ctr,  &
                '"ua1(2)"',ctr,'"ua2(2)"',ctr,'"ua3(2)"',ctr,  &
                '"a1(3)"',ctr,'"a2(3)"',ctr,'"a3(3)"',ctr,  &
                '"ua1(3)"',ctr,'"ua2(3)"',ctr,'"ua3(3)"',ctr
        end if

        ! Write data to CSV file:

        write(tpart,'(3(a,a1),i2,a1,4(a,a1))') '',ctr,'Analyt',ctr,'',ctr,kEGr,ctr,  &
            '"'//TRIM(fname(ibc:))//'"',ctr,trim(tdatum),ctr,trim(fityp),ctr,  &
            '"'//Symbole(kEGr)%s//'"',ctr
        if(FitDecay) then
            WRITE(text16,'(a,10(es15.8,a1),i1,a1,7(es15.8,a1),18(es15.8,a1))')   &
                trim(tpart),Messwert(kEGr),ctr,Ucomb,ctr,WertBayes,ctr, &
                UcombBayes,ctr,KBgrenzu,ctr,KBgrenzo,ctr,KBgrenzuSH,ctr,KBgrenzoSH,ctr,decthresh,ctr,  &
                detlim,ctr,1,ctr,Coverf,ctr,kalpha,ctr,kbeta,ctr,W1minusG,ctr,chisqrLfit,ctr,  &
                UcombLfit,ctr,Ucomblinf,ctr,  &
                ( (fpaLYT(k,i),ctr,i=1,3),(sfpaLYT(k,i),ctr,i=1,3), k=1,3)
        else
            WRITE(text16,'(a,10(es15.8,a1),i1,a1,7(es15.8,a1))')  &
                trim(tpart), Messwert(kEGr),ctr,Ucomb,ctr,WertBayes,ctr, &
                UcombBayes,ctr,KBgrenzu,ctr,KBgrenzo,ctr,KBgrenzuSH,ctr,KBgrenzoSH,ctr,decthresh,ctr,  &
                detlim,ctr,1,ctr,Coverf,ctr,kalpha,ctr,kbeta,ctr,W1minusG,ctr,chisqrLfit,ctr,  &
                UcombLfit,ctr,Ucomblinf,ctr
        end if
        ik = len_trim(tpart)

        IF(sDecimalPoint /= '.') THEN
            do i=ik,LEN_TRIM(text16)
                IF(text16(i:i) == '.') text16(i:i) = sDecimalPoint
            end do
        end if

        call errwrite(.false.,16,trim(text16),cfnam,ios,resp)
        if(ifehl == 1) return

        if(abs(xmit1) > eps1min .and. abs(xsdv) > eps1min) then
            ! Results of MC simulation:
            if(allocated(tpart)) deallocate(tpart)
            allocate(character(len=800) :: tpart)
            write(tpart,'(2(a,a1),i8,a1,i3,a1,4(a,a1))') &
                '',ctr,'MC',ctr,nmctot,ctr,kEGr,ctr,'"'//TRIM(fname(ibc:))//'"',ctr, &
                TRIM(tdatum),ctr,trim(fityp),ctr,  &
                '"'//Symbole(kEGr)%s//'"',ctr
            WRITE(text16,'(a,10(es15.8,a1))')  &
                trim(tpart),xxmit1PE,ctr,xxsdvPE,ctr,xxmit1,ctr,xxsdv,ctr, &
                xLQ,ctr,xUQ,ctr,est1LQ_BCI,ctr,est1UQ_BCI,ctr, xDT,ctr,xDL,ctr

            ik = len_trim(tpart) - 3
            IF(sDecimalPoint /= '.') THEN
                do i=ik,LEN_TRIM(text16)
                    IF(text16(i:i) == '.') text16(i:i) = sDecimalPoint
                end do
            end if
            call errwrite(.false.,16,trim(text16),cfnam,ios,resp)
            if(ifehl == 1) return
        end if

!---------------------------------------------------------------------------

        deallocate(text16, str2,tpart, str1, cfnam)

        close (16)

    end subroutine SaveResults

!#######################################################################

    subroutine TransToTV2()

        ! this routine transfers the values of the arrays to the grid Treeview2.
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_Gleich
        use UR_Variables
        use Rout,               only: WTreeViewPutStrArray,WTreeviewPutComboArray,    &
            WTreeviewPutDoubleCell,WTreeviewPutComboCell,   &
            WTreeviewPutStrCell,WTreeviewGetComboArray
        use UR_gtk_variables,   only: consoleout_gtk
        use UR_params,          only: eps1min

        implicit none

        integer(4)          :: i
!----------------------------------------------------------------------------------

        if(consoleout_gtk) write(0,*) '##### PMD: TransToTV2 beginning  ###########################'
        ! write(0,*) 'Usub3_680:   ngrs=',int(ngrs,2),' ubound(Symbole)=',ubound(Symbole,dim=1)
        call WTreeviewPutStrArray('treeview2',2,ngrs,Symbole)
        call WTreeviewPutStrArray('treeview2',3,ngrs,symtyp)
        call WTreeviewPutStrArray('treeview2',4,ngrs,Einheit)
        call WTreeviewPutComboArray('treeview2',6,ngrs,IVTL)
        call WTreeviewPutStrArray('treeview2',7,ngrs,sdformel)
        call WTreeviewPutComboArray('treeview2',10,ngrs,IAR)
        if(consoleout_gtk) write(0,*) 'nach Block 1'

        do i=1,ngrs
            IF(abs(Messwert(i)-missingval) > eps1min) call WTreeviewPutDoubleCell('treeview2',5,i,Messwert(i))
            IF(abs(SDWert(i)-missingval) > eps1min) call WTreeviewPutDoubleCell('treeview2',8,i,SDWert(i))
            IF(abs(HBreite(i)-missingval) > eps1min) call WTreeviewPutDoubleCell('treeview2',9,i,HBreite(i))
            IF(abs(StdUnc(i)-missingval) > eps1min) call WTreeviewPutDoubleCell('treeview2',11,i,StdUnc(i))
        end do
        if(consoleout_gtk) write(0,*) 'nach Loop 1'

        if(consoleout_gtk) write(0,*) '##### PMD: TransToTV2  End  ###########################'

    end subroutine TransToTV2
!#######################################################################


end module Usub3
