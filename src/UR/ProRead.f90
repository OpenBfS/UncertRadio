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
module Pread
    use PreadCSV

    !    contains:
    ! ProRead


contains

!#######################################################################

    subroutine ProRead()

        ! reads in a project file of format .TXP; it calls ProRead_CSV for the
        ! case of a CSV formatted project file.
        ! See chapter 3.6 "Structure of the project file" of the UncertRadio
        !    CHM Help file for more information.
        ! It finally calls TransferToGTK to transfer the data into the GTK GUI.

        !     Copyright (C) 2014-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,       only: c_null_ptr,c_null_char, c_associated, c_ptr

        use gtk,                    only: GTK_BUTTONS_OK,GTK_MESSAGE_ERROR
        use top,                    only: FinditemS,idpt,WrStatusbar,RealModA1,CharModA1,IntModA1,LogModA1, &
                                          InitVarsTV6,DRead,ModVarsTV2,CharModStr
        USE ur_general_globals,           only: open_project_parts,modSymb,copyEQ,batest_user,fname,gross_negative, &
                                          gum_restricted,kmodeltype,project_loadw,proStartNew, &
                                          fileToSimulate,FDecM,GspkDT,covTB,FcalDT,MDDT
        USE UR_Gleich_globals,              only: Messwert,Stdunc,Symbole,symtyp,einheit,bedeutung,IVTl,IAR,SDformel, &
                                          SDwert,HBreite,Titeltext,Formeltext,FormeltextFit,cvformel, &
                                          SymboleG,ixdanf,coverf,icovtyp,ifehl,ilam_binom,ip_binom,itm_binom, &
                                          kbgv_binom,ISymbA,IsymbB,knumEGr,kEGr,ngrs,nab,nmu,SymboleA, &
                                          SymboleB,coverin,loadingPro,meanID,nvarsMD,refdataMD,missingval, &
                                          xdataMD,MDpoint,MDpointrev,MDused,k_MDtyp,nvalsMD,CovarVal,CorrVal, &
                                          ncov,SymboleX,symtypX,knetto,kbrutto,kbrutto_name,knetto_name, &
                                          CVFormel,covarvalSV,fBayMD, umeanMD,nvMD,smeanMD,meanMD,MDtyp,nmodf
        USE UR_DLIM,                only: kalpha,kbeta,alpha,beta,GamDistAdd,W1minusG
        USE UR_Linft,               only: ifit,nwei,nkovzr,kfitmeth,ndefall,CCtitle,CFaelldatum,CStartzeit, &
                                          d0impulse,d0messzeit,dbimpulse,dbzrate,defineallxt,dmesszeit, &
                                          dnetrate,export_r,FitCalCurve,FitDecay,fitmeth,kal_Polgrad,kfitmeth, &
                                          kpearson,kPMLE,linfzbase,mfitfix,nchannels,ndatmax,nkalpts,nkovzr, &
                                          numd,SumEval_fit,use_UfitKal,use_WTLS,sdbzrate,d0zrate,sd0zrate, &
                                          sdnetrate,xkalib,uxkalib,ykalib,uykalib,use_absTimeStart
        USE UR_Gspk1Fit,            only: ecorruse,FBT,Gamspk1_Fit,varadd_rn,WMextSD,unitradio,erg,gnetrate, &
                                          RateCB,RateBG,SDRateBG,effi,sdeffi,pgamm,sdpgamm,fatt,sdfatt, &
                                          fcoinsu,SDfcoinsu,kdatmax,guse,UnitR_effi_old,UnitR_pgamm_old, kmwtyp  !!!! 2025.01.23 GK
        use Rout,                   only: MessageShow,UpdateProName,WDPutSelRadio,WDPutEntryDouble, &
                                          WDPutEntryDouble,WDGetCheckMenuItem,WDSetCheckMenuItem,   &
                                          WDSetComboboxAct,WDPutSelRadioMenu,WDPutEntryString, &
                                          WTreeViewGetStrArray,WTreeViewPutDoubleCell, &
                                          WDSetCheckButton,WDputEntryInt,WDGetComboboxAct

        use Brandt,                 only: pnorm
        use UR_gtk_globals,       only: consoleout_gtk,item_setintern
        use RdSubs,                 only: TransferToGTK
        use UR_params,              only: EPS1MIN
        use CHF,                    only: ucase, flfu
        use LSTfillT,               only: WDListstoreFill_table
        use translation_module,     only: T => get_translation

        use UR_DecChain,            only: apply_separation,DCBuildupAtSepar,DCcommMeasmt, &
                                          chaincode,ChainSelected, DCnuclide,DCsymbT12,DCsymbLambda, &
                                          DCsymbEffiA,DCsymbEffiB,DCsymbEffiC,DCsymbYield,N_nuclides, &
                                          DChain_read_data,DCindx,DChain,DChainEGr

        implicit none

        external funcsKB


        character(:), allocatable :: ttext,text,text2,str1
        character(len=60)         :: csymbol
        integer                   :: k,ios,i,i1,i2,i3,imenu1,i22,kuseUfit,nv,j,jj              ! kmwtyp
        integer                   :: kWTLS,kk, iz0,k22,rmode,nbb,nddanf,kkL, nrec, resp

        LOGICAL                   :: ugr,cvgr,fit,abgr,gsp1gr,conda
        character(len=150)        :: subs,tmeanid
        integer                   :: jv,mm1, error_str_conv,iv,kc,kim,ksep,nnch
        character(len=15)         :: ModelType
        character(len=60)         :: cdummy
        character(len=max(len(fileToSimulate),len(fname)) + 32) :: fname_tmp

        !-----------------------------------------------------------------------


        ifehl = 0
        allocate(character(len=800) :: text,text2)
        allocate(character(len=1500) :: ttext)
        allocate(character(len=150) :: str1)

        proStartNew = .false.
        Gum_restricted = .false.

        GamDistAdd = 0.0_rn    !  since 30.11.2019; new condition following ISO 11929:2019

        text2 = ucase(fname)
        i1 = LEN_TRIM(text2)
        IF(text2(i1-3:i1) == '.CSV') THEN
            deallocate(text,text2)
            deallocate(ttext)

            call ProRead_CSV()
            RETURN
        end if

        item_setintern = .true.

        if(.not.open_project_parts .or. (open_project_parts .and. copyEQ)) then
            if(allocated(Formeltext)) deallocate(Formeltext)
            allocate(Formeltext(100))
            Formeltext(1)%s = ' '

            if(allocated(FormeltextFit)) deallocate(FormeltextFit)
            allocate(FormeltextFit(50))
            FormeltextFit(1)%s = ' '

            if(allocated(Titeltext)) deallocate(Titeltext)
            allocate(Titeltext(50))
            Titeltext(1)%s = ' '
        end if

        fit = .FALSE.
        if(.not.open_project_parts) then
            FitDecay = .false.
            SumEval_fit = .false.
            DChain = .false.           ! 16.12.2024
            DChainEGr = .false.        ! 27.12.2024
            Gamspk1_Fit = .false.      ! 18.11.2025
        end if

        close (25)
        if( .not. open_project_parts) then
            fname_tmp = fname
        else
            fname_tmp = fileToSimulate
        end if

        fname_tmp = flfu(fname_tmp, error_str_conv)
        if (error_str_conv > 0) write(*,*) 'Warning, could not convert file_name ' // &
                                           'to local encoding: ' // trim(fname_tmp)

        open (25, file=trim(fname_tmp), STATUS='old', IOSTAT=ios)
        if(.not. open_project_parts) call UpdateProName(fname)

        IF(ios /= 0) THEN
            str1 = T('File cannot be opened') // ": " // trim(fname)
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProRead:", resp,mtype=GTK_MESSAGE_ERROR)
            ifehl = 1
            close (25)
            goto 9000             ! RETURN
        end if


        if(consoleout_gtk) WRITE(0,*) 'PR: A',' ios=',int(ios,2)
        if(open_project_parts .and. FDecM) goto 1030
        if(open_project_parts .and. GspkDT) goto 1050
        if(open_project_parts .and. covTB) goto 50
        if(open_project_parts .and. FcalDT) goto 65

        if(open_project_parts .and. MDDT) goto 1115      ! 21.9.2023
        if(consoleout_gtk) WRITE(0,*) 'PR: B',' ios=',int(ios,2)
        if(.not.open_project_parts .or. (open_project_parts .and. copyEQ)) then
            do
                call DRead(25,ttext,ios)
                if(ios /= 0) then
                    ifehl = 1
                    return
                end if
                if(index(ttext,'@Titeltext:') > 0) exit
            end do
            nrec = 0
            do
                nrec = nrec + 1
                call DRead(25,ttext,ios)
                if(ios /= 0) then
                    ifehl = 1
                    return
                end if
                if(index(ttext,'@Formeltext:') > 0) then
                    nrec = nrec - 1
                    exit
                else
                    Titeltext(nrec)%s = trim(ttext)
                    ! write(55,'(a)') Titeltext(nrec)%s     ! xxxxxxxx
                end if
            end do
            call CharModA1(Titeltext,nrec)
            if(consoleout_gtk) WRITE(0,*) 'PR: B1',' ios=',int(ios,2)
            nrec = 0
            do
                nrec = nrec + 1
                call DRead(25,ttext,ios)
                if(ios /= 0) then
                    ifehl = 1
                    return
                end if
                if(index(ttext,'@FormeltextFit:') > 0 .or. index(ttext,'@Symbole-GRID:') > 0) then
                    nrec = nrec - 1
                    exit
                else
                    Formeltext(nrec)%s = trim(ttext)
                    ! write(55,'(a)') Formeltext(nrec)%s
                    if(index(ucase(ttext),'SUMEVAL') > 0) then
                        fit = .false.
                        SumEval_fit = .true.
                    end if
                    if(index(ucase(ttext),'LINFIT') > 0) then
                        fit = .true.
                        FitDecay = .true.
                    end if
                    ! folgende 6 Zeilen neu:
                    if(index(ucase(ttext),'SDECAY') > 0) then        ! 27.12.2024
                      DChain = .true.
                    end if
                    if(index(ucase(ttext),'GAMSPK1') > 0) then        ! 18.11.2025
                      Gamspk1_Fit = .true.
                    end if

                end if
            end do
            if(consoleout_gtk) WRITE(0,*) 'PR: B2',' ios=',int(ios,2)
            call CharModA1(Formeltext,nrec)
            ! write(55,*) 'Formeltext: nrec=',nrec,' ubound=',ubound(Formeltext,dim=1)
            if(consoleout_gtk) WRITE(0,*) 'PR: B3',' ios=',int(ios,2)
            if(index(ttext,'@FormeltextFit:') > 0) then
                nrec = 0
                do
                    nrec = nrec + 1
                    call DRead(25,ttext,ios)
                    if(index(ttext,'@Symbole-GRID:') > 0) then
                        nrec = nrec - 1
                        exit
                    else
                        FormeltextFit(nrec)%s = trim(ttext)
                        ! write(55,'(a)') FormeltextFit(nrec)%s
                    end if
                end do
                call CharModA1(FormeltextFit,nrec)
                fit = .true.
                if(nrec == 0) then
                    deallocate(FormelTextFit)
                    fit = .false.
                end if
            else
                deallocate(FormeltextFit)
                fit = .false.
            end if
            if(consoleout_gtk) WRITE(0,*) 'PR: B4',' ios=',int(ios,2)
            ! write(55,*) 'fit=',fit,' SumEval_fit=',SumEval_fit,'FitDecay=',FitDecay

            !write(66,*) 'ProRead: Titeltext ======================================'
            !write(66,*) trim(titeltext)
            !write(66,*) 'ProRead: Titeltext ======================================'

            goto 30
30          continue

            !write(66,*) 'ProRead 30 : Titeltext ======================================'
            !write(66,*) trim(titeltext)
            !write(66,*) 'ProRead 30 : Titeltext ======================================'

            !write(66,*) 'ProRead 30 : Formeltext ======================================'
            !write(66,*) trim(Formeltext)
            !write(66,*) 'ProRead 30 : Formeltext ======================================'

            if(.not.batest_user .and..not.open_project_parts) then
                WRITE(55,*) 'Titeltext='
                do i=1,size(titeltext)
                    WRITE(55,*) titeltext(i)%s
                end do
                WRITE(55,*) 'Formeltext='
                do i=1,size(Formeltext)
                    WRITE(55,*) Formeltext(i)%s
                end do
                IF(fit .or. SumEval_fit) THEN
                    if(allocated(FormeltextFit)) then
                        if(fit) FitDecay = .true.
                        nmodf = size(FormeltextFit)        ! 29.1.2024
                        write(55,*) 'FormeltextFit='
                        do i=1,nmodf                      ! 29.1.2024
                            write(55,*) FormeltextFit(i)%s
                        end do
                    end if
                END IF
            end if
            if(open_project_parts .and. copyEQ) goto 9000

        end if

        deallocate(ttext)
        if(consoleout_gtk) write(0,*) 'PR: C',' ios=',int(ios,2)

        if(.not.open_project_parts .or. (open_project_parts .and. modSymb)) then
            rewind 25
            do
                call DRead(25,text,ios)
                ! write(0,*) 'PR_226: text=',trim(text)
                i1 = INDEX(text,'@Symbole-')
                IF(i1 > 0) EXIT
            end do

            nchannels = 1
            call DRead(25,text,ios)
            i1 = INDEX(text,'=')
            IF(TRIM(text(1:i1-1)) == 'nchs') THEN
                READ(text(i1+1:),*) nchannels
            else
                nchannels = 1
                BACKSPACE 25
            end if

            knumEGr = 0
            call DRead(25,text,ios)
            i1 = INDEX(text,'=')
            IF(TRIM(text(1:i1-1)) == 'nEGr') THEN
                READ(text(i1+1:),*) knumEGr
                kEGr = 1
            else
                knumEGr = 1
                kEGr = 1
                BACKSPACE 25
            end if

            call DRead(25,text,ios)
            i1 = INDEX(text,'=')
            IF(i1 > 0) THEN
                READ(text(i1+1:),*) ngrs
            else
                READ(text,*) ngrs
            END IF

            call DRead(25,text,ios)
            i1 = INDEX(text,'=')
            IF(i1 > 0) THEN
                READ(text(i1+1:),*) nab
            else
                READ(text,*) nab
            END IF

            call DRead(25,text,ios)
            i1 = INDEX(text,'=')
            IF(i1 > 0) THEN
                READ(text(i1+1:),*) nmu
            else
                READ(text,*) nmu
            END IF

            if(nmu < ngrs-nab) nmu = ngrs - nab
        end if
        if(consoleout_gtk) WRITE(0,*) 'PR: D',' ios=',int(ios,2)

        if(.not.batest_user) then
            WRITE(55,'(1x)')
            WRITE(55,'(3(a,i0))') 'ngrs=',ngrs,'  nab=',nab,'  nmu=',nmu
        end if

        if(.not.open_project_parts) then
            nbb = ngrs
            allocate(Symbole(nbb),SymboleG(nbb),symtyp(nbb),einheit(nbb),Bedeutung(nbb))
            allocate(IVTL(nbb),Messwert(nbb),StdUnc(nbb),SDformel(nbb),SDwert(nbb),HBreite(nbb),IAR(nbb))
            allocate(SymboleA(nbb),SymboleB(nbb))
        else
            if(modSymb) then
                allocate(SymboleX(ngrs),SymtypX(ngrs))
                call CharModA1(SymboleX,ngrs)
                call CharModA1(SymtypX,ngrs)
            end if
        end if

        conda = .false.    ! 2025.02.23 GK

        if(.not.open_project_parts .or. (open_project_parts .and. modSymb)) then
            nvarsMD = 0
            do k=1,ngrs
                call DRead(25,text,ios)
                ! write(55,*) 'satz ',int(k,2),' text=',trim(text)
                i1 = INDEX(text,'#')
                READ(text(1:i1-1),'(a)') subs
                if(.not.open_project_parts) then
                    Symbole(k)%s = trim(subs)
                    SymboleG(k)%s = ucase(Symbole(k)%s)
                else
                    SymboleX(k)%s = trim(subs)
                end if

                text = TRIM(text(i1+1:))
                i1 = INDEX(text,'#')
                READ(text(1:i1-1),'(a)') subs
                if(.not.open_project_parts) then
                    symtyp(k)%s = trim(subs)
                    ! write(55,*) 'Symbol=',trim(symbole(k)),'  Symtyp(k)=',symtyp(k)
                else
                    symtypx(k)%s = trim(subs)
                end if
                if(.not.open_project_parts) conda = ucase(symtyp(k)%s) == 'M'
                if(open_project_parts .and. modSymb) conda = ucase(symtypX(k)%s) == 'M'
                ! if(ucase(symtyp(k)%s) == 'M') then
                if(conda) then
                    if(nvarsMD == 0) then
                        allocate(MDpoint(1)); MDpoint(1) = 0
                        allocate(MDpointrev(1)); MDpointrev(1) = 0; call IntModA1(MDpointrev,ngrs)
                        allocate(MDused(1)); MDused(1) = .false.
                    end if
                    nvarsMD = nvarsMD + 1
                    if(nvarsMD > 1) call IntModA1(MDpoint,nvarsMD)
                    ! if(nvarsMD > 1) IntModeA1(MDpointrev,nvarsMD,1)  ! s.oben
                    if(nvarsMD > 1) call LogModA1(MDused,nvarsMD)
                    MDpoint(nvarsMD) = k
                    MDpointrev(k) = nvarsMD
                    MDused(nvarsMD) = .true.
                    write(55,'(3(a,i0))') 'k=',k,' MDpoint(nvarsMD)=',MDpoint(nvarsMD),' MDpointrev(k)=', &
                        MDpointrev(k)
                end if
                if(.not.open_project_parts) then
                    text = TRIM(text(i1+1:))
                    i1 = INDEX(text,'#')
                    READ(text(1:i1-1),'(a)') subs
                    Einheit(k)%s = trim(subs)

                    text = TRIM(text(i1+1:))
                    i1 = INDEX(text,'#')
                    READ(text(1:i1-1),'(a)') subs
                    Bedeutung(k)%s = subs

                    if(.not.batest_user) WRITE(55,'(a,i2,10a)') 'k=',k,' ',Symbole(k)%s,' ',symtyp(k)%s, &
                        ' ',einheit(k)%s, ' ',bedeutung(k)%s
                end if

            end do
            if(consoleout_gtk) WRITE(0,*) 'PR: E',' ios=',int(ios,2),' next=@menu1'

            backspace (25)
            backspace (25)
            backspace (25)

            do
                call DRead(25,text,ios)
                i1 = INDEX(text,'@Menu1')
                IF(i1 > 0) EXIT
            end do

            call DRead(25,text,ios)
            i1 = INDEX(text,'=')
            IF(i1 > 0) THEN
                do j=3,1,-1
                    READ(text(i1+1:),*,IOSTAT=ios) knetto(1:j)
                    if(ios == 0) exit
                end do
            else
                do j=3,1,-1
                    READ(text,*,IOSTAT=ios) knetto(1:j)
                    if(ios == 0) exit
                end do
            END IF

            call DRead(25,text,ios)
            i1 = INDEX(text,'=')
            IF(i1 > 0) THEN
                do j=3,1,-1
                    READ(text(i1+1:),*,IOSTAT=ios) kbrutto(1:j)
                    if(ios == 0) exit
                end do
            else
                do j=3,1,-1
                    READ(text,*,IOSTAT=ios) kbrutto(1:j)
                    if(ios == 0) exit
                end do
            END IF
            ! write(66,'(a,3i3)') 'ProRead: knetto: ',knetto
            ! write(66,'(a,3i3)') 'ProRead: kbrutto: ',kbrutto

            if(allocated(kbrutto_name)) deallocate(kbrutto_name,knetto_name)
            allocate(kbrutto_name(knumEGr),knetto_name(knumEGr))
            do i=1,knumEGr
                if(kbrutto(i) > 0 .and. kbrutto(i) <= ngrs) kbrutto_name(i)%s = Symbole(kbrutto(i))%s
                if(knetto(i) > 0) knetto_name(i)%s = Symbole(knetto(i))%s
            end do

        end if

! if(open_project_parts .and. modSymb) goto 120

        do
            call DRead(25,text,ios)
            i1 = INDEX(text,'@Unc-Grid')
            IF(i1 > 0) EXIT
        end do

! ngrs_new = ngrs
        ugr = .TRUE.
        do k=1,ngrs
            deallocate(text)
            allocate(character(len=2000) :: text)  ! 12.8.2023
            call DRead(25,text,ios)
            IF(text(1:1) == '@') THEN
                BACKSPACE 25
                BACKSPACE 25
                ugr = .FALSE.
                GOTO 50
            END IF

            i1 = INDEX(text,'#')

            if(k > ubound(messwert,dim=1)) then   ! 21.9.2023 first time
                call CharModA1(SDformel,k)
                call IntModA1(IAR,k)
                call IntModA1(IVTL,k)
                call RealModA1(Messwert,k)
                call RealModA1(StdUnc,k)
                call RealModA1(SDWert,k)
                call RealModA1(HBreite,k)
                write(0,*) 'k=',int(k,2),' ngrs=',int(ngrs,2)
            end if


            READ(text(1:i1-1),'(a)') Csymbol
            do jv = 2,8
                text = TRIM(text(i1+1:))
                i1 = INDEX(text,'#')
                !  if(jv == 2) write(55,*,decimal='point') 'Messwert-Feld: ',text(1:i1-1)
                if(jv == 2) READ(text(1:i1-1),*,decimal='point') Messwert(k)
                !  write(55,*) real(Messwert(k),rn)

                if(jv == 3) READ(text(1:i1-1),*) IVTL(k)
                if(IVTL(k) == 6) then
                    mm1 = index(text(1:i1), '6  #')
                    if(mm1 > 0) then
                        ! the case of an old txp-format of UR1:
                        IVTL(k) = 4     !(N+1)-rule
                    end if
                end if

                ! if(jv == 4) READ(text(1:i1-1),'(a)') SDformel(k)
                if(jv == 4) SDformel(k)%s = text(1:i1-1)
                if(jv == 5) READ(text(1:i1-1),*) SDWert(k)
                if(jv == 6) READ(text(1:i1-1),*) HBreite(k)
                if(jv == 7) then
                    READ(text(1:i1-1),*) IAR(k)
                    if(IAR(k) == 0) IAR(k) = 1        ! NLWKN-Kalfit enthält beim 18. Wert eine Null: darf nicht sein
                end if
                if(jv == 8) READ(text(1:i1-1),*,decimal='point') StdUnc(k)

            end do
            if(.not.batest_user) WRITE(55,'(a,i3,a,es12.5,a,i2,2a,2(a,es12.5),a,i0,a,es12.5)',decimal='point')  &
                'k=',k,' ',real(Messwert(k),8),' ',IVTL(k),' ',sdformel(k)%s, &
                ' ',real(sdwert(k),8),' ',real(HBreite(k),8),' ',IAR(k),' ',  &
                real(StdUnc(k),8)
        end do

50      CONTINUE

        if(open_project_parts .and. modSymb) then
            call WDListstoreFill_table('liststore_valunc',2, .true.)
            goto 120
        endif

        do
            call DRead(25,text,ios)
            i1 = INDEX(text,'@')
            IF(i1 == 1) EXIT
        end do

        IF(INDEX(text,'@Covar-Grid') == 0) THEN
            ! IF(INDEX(text,'@Covar-Grid') > 0) THEN
            if(open_project_parts .and. covTB) goto 50
        else

            call DRead(25,text,ios)
            i1 = index(text,'ncov=')
            if(i1 == 0) backspace 25

            if(allocated(IsymbA)) deallocate(IsymbA,IsymbB,icovtyp,cvformel,covarval,corrval,covarvalSV)

            k22 = 0
            do
                call DRead(25,text,ios)
                if(index(text,'@') > 0) then
                    do kk=1,k22+1
                        backspace(25)
                    end do
                    exit
                end if
                k22 = k22 + 1
            end do

            ! k22 = ncovmx
            rmode = 2
            ncov = k22

52          continue
            iz0 = 0
            cvgr = .TRUE.

            if(rmode == 2) then
                allocate(IsymbA(ncov),IsymbB(ncov),icovtyp(ncov),cvformel(ncov),covarval(ncov), &
                    corrval(ncov),covarvalSV(ncov))
                ISymbA = 0       ! 27.6.2023
                ISymbB = 0       !
                icovtyp = 1
            end if
            do k=1,ncov
                call DRead(25,text,ios)
                IF(text(1:1) == '@') THEN
                    BACKSPACE 25
                    IF(k == 1) cvgr = .FALSE.
                    GOTO 55
                END IF
                iz0 = iz0 + 1
                if(rmode == 1) cycle

                ! ncov = ncov + 1
                i1 = INDEX(text,'#')
                READ(text(1:i1-1),*) ISymbA(k)
                ISymbA(k) = ISymbA(k) - 1

                text = TRIM(text(i1+1:))
                i1 = INDEX(text,'#')
                READ(text(1:i1-1),*) ISymbB(k)
                ISymbB(k) = ISymbB(k) - 1

                text = TRIM(text(i1+1:))
                i1 = INDEX(text,'#')
                READ(text(1:i1-1),*) icovtyp(k)

                text = TRIM(text(i1+1:))
                i1 = INDEX(text,'#')
                ! READ(text(1:i1-1),'(a)') CVformel(k)%s
                CVformel(k)%s = trim(ucase(text(1:i1-1)))
                !  write(55,*) 'text=',trim(text),'  CVFormel(k)=',CVFormel(k)%s
                if(len_trim(CVFormel(k)%s) == 0) CVFormel(k)%s = '  '

                text = TRIM(text(i1+1:))
                i1 = INDEX(text,'#')
                READ(text(1:i1-1),*,iostat=ios) CovarVal(k)
                if(ios /= 0) CovarVal(k) = missingval

                if(.not.batest_user) WRITE(55,'(a,i2,3(a,i3),a,a,a,es12.5)') 'k=',k,' ',ISymbA(k),' ',ISymbB(k),' ',icovtyp(k), &
                    ' ',CVformel(k)%s,' ',real(CovarVal(k),8)

                if(icovtyp(k) == 2 .and. abs(covarval(k)-missingval) > EPS1MIN) then
                    CorrVal(k) = covarval(k)
                else
                    CorrVal(k) = 0.0_rn
                end if
                if(.false. .and. IsymbA(k) == IsymbB(k) .and. abs(Covarval(k) - missingval) < EPS1MIN) then
                    do i=k,k-1
                        SymboleA(i) = SymboleA(i+1)
                        SymboleB(i) = SymboleB(i+1)
                    end do
                    ncov = ncov - 1
                end if
            end do

            if(.not.batest_user) write(55,*) 'ncov=',int(ncov,2)
            if(.false. .and. rmode == 1) then
                rmode = 2
                do i=1,iz0
                    backspace(25)
                end do
                goto 52
            end if
55          CONTINUE
            if(open_project_parts .and. covTB) then
                ! close (25)
                ! goto 9000
            end if

        END IF

1030    continue
        ! write(0,*) '1030:   FDecM=',FDecm,' modSymb',modSymb

! if(.not.batest_user) WRITE(55,*) 'Zeile @Abkling-Grid:= ',TRIM(text)

        if(.not.open_project_parts .or. (open_project_parts .and. (modSymb .or. FDecM))) then
            IF(ncov > 0) BACKSPACE 25
            kc = 0          ! added 12.7.2025 GK
1035        continue
            do
                call DRead(25,text,ios)
                i1 = INDEX(text,'@')
                IF(i1 == 1) EXIT
            end do

            !--- 19.12.2024 GK
            IF(INDEX(text,'@DChain:') > 0) THEN
            ! .
            ! .
            ! @Covar-Grid:
            ! @DChain:
            ! CHName=Chaincode=Pb-210-3N
            ! Pars= 3 1 1 1 0 0 3
            ! 1 #Pb-210 #lamPb210 #epsPb210 #  #  #etaPb #
            ! 2 #Bi-210 #lamBi210 #epsBi210 #  #  #etaBi #
            ! 3 #Po-210 #lamPo210 #epsPo210 #  #  #etaPo #
            ! @Sonstige:
            ! .
            ! .
            !        "DC": Decay chain
            !     CHName    : name of decay chain
            !     Pars=3 1 1 1 0 0 3
            !          3 : kc        index of selected chain
            !          1 : ksep      =1: apply separation                             logical apply_separation
            !          1 : iv        =1: use lambda instead of half-live              logical use_Lambda
            !          1 : nnch      number of counting channels (energy windows)     integer nnch
            !          0 : kim       =1: common measurement of radionuclides          logical DCcommMeasmt
            !          0 : DCBuildupAtSepar    =1: buildup after chemical separation  logical DCBuildupAtSepar
            !          3 : N_nuclides  number of elements of the decay chain          integer N_nuclides
            !
            !         1 #Pb-210 #lamPb210 #epsPb210 #  #  #etaPb #
            !         DC member i; DCnuclide(i); DCsymbLambda(i) or DCsymbT12(i); DCsymbEffiA(i);
            !                            DCsymbEffiB(i); DCsymbEffiC(i); DCsymbYield(i)

              DChain = .true.
              call DRead(25,text2,ios)
              Chaincode%s = adjustl(trim(text2(8:)))
                     write(55,*) 'Chaincode=',chaincode%s
              call DRead(25,text2,ios)      !     vertauscht
                read(text2(6:),*,IOSTAT=ios) kc,ksep,iv,nnch,kim,DCBuildupAtSepar,N_nuclides
                    write(55,'(6(a,i3),a,L1)') 'DChain: kc=',kc,' ksep=',ksep,' iv=',iv,' nnch=',nnch,' kim=',kim, &
                              ' N_Nuclides=',N_nuclides,' DCbuildupAtSepar=',DCBuildupAtSepar
              call WDSetComboboxAct('ComboboxDCchains',kc)
                ChainSelected = kc
                          call WDGetComboboxAct('ComboboxDCchains',kc)
                write(55,*) 'chainSelected=',kc
                write(66,*) 'chainSelected=',kc
              apply_separation = .false.
              if(ksep == 1) apply_separation = .true.
              call WDSetComboboxAct('DCcheckSepar', ksep)
              call WDSetCheckButton('DCcheckVorLam',iv)
              call WDSetComboboxAct('comboboxtextDCNCH', nnch)
              DCcommMeasmt = .false.
              if(kim == 1) DCcommMeasmt = .true.
              call WDSetCheckButton('DCcheckCommMeasmt',kim)
              call WDPutEntryInt('entryDCZBuildupNuk',DCBuildupAtSepar)

              i = N_Nuclides
              if(allocated(DCnuclide)) deallocate(DCnuclide,DCsymbT12,DCsymbLambda,DCsymbEffiA, &
                                         DCsymbEffiB,DCsymbEffiC,DCsymbYield,DCindx)
              if(allocated(DCsymbT12)) deallocate(DCsymbT12)
              if(allocated(DCsymbLambda)) deallocate(DCsymbLambda)
              allocate(DCnuclide(i+5),DCsymbT12(i+5),DCsymbLambda(i+5),DCsymbEffiA(i+5), &
                                         DCsymbEffiB(i+5),DCsymbEffiC(i+5),DCsymbYield(i+5), &
                                         DCindx(i+5))

              do i=1,N_nuclides
                call DRead(25,text,ios); i1 = INDEX(text,'#')

                text = trim(text(i1+1:));  i1 = INDEX(text,'#')
                DCNuclide(i)%s = trim(adjustL(text(1:i1-1)))
                text = trim(text(i1+1:)); i1 = INDEX(text,'#')
                if(iv == 0) DCsymbT12(i)%s = trim(adjustL(text(1:i1-1)))
                if(iv == 1) DCsymbLambda(i)%s = trim(adjustL(text(1:i1-1)))
                text = trim(text(i1+1:)); i1 = INDEX(text,'#')
                DCsymbeffiA(i)%s = trim(adjustL(text(1:i1-1)))
                text = trim(text(i1+1:)); i1 = INDEX(text,'#')
                DCsymbeffiB(i)%s = trim(adjustL(text(1:i1-1)))
                text = trim(text(i1+1:)); i1 = INDEX(text,'#')
                DCsymbeffiC(i)%s = trim(adjustL(text(1:i1-1)))
                text = trim(text(i1+1:)); i1 = INDEX(text,'#')
                DCsymbYield(i)%s = trim(adjustL(text(1:i1-1)))
                  write(55,*) 'i=',int(i,2),' ',DCnuclide(i)%s,' ',DCsymbLambda(i)%s,DCsymbYield(i)%s
              end do
              DChain_read_data = .true.
              DCHain = .true.
              BACKSPACE 25

            else
              !
            END IF
            !---
            BACKSPACE 25
            do
              call DRead(25,text,ios)
              i1 = INDEX(text,'@')
              IF(i1 == 1) EXIT
            end do
            !..............................................................

            ! write(0,*) 'Before reading decay grid,  Text=',trim(text)
            abgr = .FALSE.
            IF(INDEX(text,'@Abkling-Grid:') == 0) THEN
                if(open_project_parts .and. (modSymb .or. FDecM)) goto 1035
            END IF
            IF(INDEX(text,'@Abkling-Grid:') > 0) THEN
                if(.not.batest_user) WRITE(55,*) 'Zeile @Abkling-Grid:= ',TRIM(text)
                abgr = .TRUE.
                numd = 0
                FitDecay = .true.
                call DRead(25,text2,ios)      !     vertauscht
                read(text2(8:),*,IOSTAT=ios) (ifit(i),i=1,3), nwei,nkovzr,kfitmeth,ndefall
                kWTLS = 0
                defineallxt = .false.
                if(ndefall == 1) defineallxt = .true.
                IF(kfitmeth == 0) kpearson = 0
                IF(kfitmeth == 1) kpearson = 1
                kPMLE = 0
                IF(kfitmeth == 2) kPMLE = 1
                IF(kfitmeth == 3) kWTLS = 1
                fitmeth = 'WLS'
                IF(kpearson == 1) fitmeth = 'PLSQ'
                IF(kPMLE == 1) fitmeth = 'PMLE'
                IF(kWTLS == 1) fitmeth = 'WTLS'
                !---cc 29.1.2024
                mfitfix = 0
                do i=1,3
                    if(ifit(i) <= 2) mfitfix = mfitfix + 1
                    if(i == 3 .and. mfitfix > knumEGr .and. mfitfix == nmodf/nchannels) then
                        if(ifit(i) < 3) ifit(i) = 3
                    end if
                end do
                !---cc

                ! if(.not.open_project_parts) then
                IF(ios /= 0) THEN
                    ! reading old txp files, when nwei, nkovzr, kpearson were not saved
                    READ(text2(8:),*,IOSTAT=ios) (ifit(i),i=1,3)
                    nwei = 0
                    nkovzr = 0
                    !! kpearson = 0
                    kPMLE = 0

                    mfitfix = 0
                    do i=1,3
                        if(ifit(i) <= 2) mfitfix = mfitfix + 1
                    end do
                end if

                if(max(ifit(1),ifit(2),ifit(3)) == 1) then
                    do i=1,3
                        if(ifit(i) == 0) ifit(i) = 3
                    end do
                end if
                use_WTLS = .FALSE.
                IF(kWTLS == 1) use_WTLS = .TRUE.
                READ(25,'(a)',iostat=ios) CFaelldatum
                use_absTimeStart = .true.          ! 24.7.2023
                if(len_trim(CFaelldatum) == 0) use_absTimeStart = .false.
                if(ios /= 0) use_absTimeStart = .false.
                if(cfaelldatum(1:1) == '@') then
                    project_loadw = .false.
                    loadingpro = .false.
                    use_absTimeStart = .false.
                    cfaelldatum = ''
                    goto 57
                end if
                call DRead(25,text2,ios)
                if(text2(1:1) == '@') then
                    project_loadw = .false.
                    loadingpro = .false.
                    goto 57
                end if
                READ(text2,*,IOSTAT=ios) imenu1
                linfzbase = imenu1

                !if(.not.open_project_parts) then
                if(.not.batest_user) then
                    if(FitDecay) WRITE(55,*) 'Faelldatum=',CFaelldatum
                    WRITE(55,*) 'imenu1=',imenu1
                end if

                if(allocated(dmesszeit)) &
                    deallocate(dmesszeit,dbimpulse,dbzrate,sdbzrate,d0messzeit,d0impulse,d0zrate,sd0zrate, &
                    dnetrate,sdnetrate,CStartzeit)
                if(allocated(varadd_rn)) deallocate(varadd_rn)

                do k=1,ndatmax
                    call DRead(25,text,ios)
                    ! write(66,*) 'text=',trim(text)
                    IF(text(1:1) == '@') THEN
                        BACKSPACE 25
                        IF(k == 1) abgr = .FALSE.
                        if(numd > 50) export_r = .false.

                        GOTO 57
                    END IF

                    if(len_trim(text) == 0) then
                        project_loadw = .false.
                        loadingpro = .false.
                        cycle
                    end if

                    numd = numd + 1
                    if(numd == 1) then
                        allocate(dmesszeit(1),dbimpulse(1),dbzrate(1),sdbzrate(1),d0messzeit(1),d0impulse(1),   &
                            d0zrate(1),sd0zrate(1),dnetrate(1),sdnetrate(1),CStartzeit(1),varadd_rn(1) )
                    end if

                    call realModA1(dmesszeit,numd)
                    call realModA1(dbimpulse,numd)
                    call realModA1(dbzrate,numd)
                    call realModA1(sdbzrate,numd)
                    call realModA1(d0messzeit,numd)
                    call realModA1(d0impulse,numd)
                    call realModA1(d0zrate,numd)
                    call realModA1(sd0zrate,numd)
                    call realModA1(dnetrate,numd)
                    call realModA1(sdnetrate,numd)
                    call CharModA1(CStartzeit,numd)
                    call realModA1(varadd_rn,numd)

                    i1 = INDEX(text,'#')
                    READ(text(1:i1-1),'(a)') cdummy
                    CStartzeit(numd)%s = trim(cdummy)
                    do jv = 2,11
                        text = TRIM(text(i1+1:))
                        i1 = INDEX(text,'#')
                        if(jv == 2) READ(text(1:i1-1),*) dmesszeit(numd)
                        if(jv == 3) READ(text(1:i1-1),*) dbimpulse(numd)
                        if(jv == 4) READ(text(1:i1-1),*) dbzrate(numd)
                        if(jv == 5) READ(text(1:i1-1),*) sdbzrate(numd)
                        if(jv == 6) READ(text(1:i1-1),*) d0messzeit(numd)
                        if(jv == 7) READ(text(1:i1-1),*) d0impulse(numd)
                        if(jv == 8) READ(text(1:i1-1),*) d0zrate(numd)
                        if(jv == 9) READ(text(1:i1-1),*) sd0zrate(numd)
                        if(jv == 10) READ(text(1:i1-1),*) dnetrate(numd)
                        if(jv == 11) READ(text(1:i1-1),*) sdnetrate(numd)
                    end do
                    if(.not.batest_user) WRITE(55,*) 'numd=',numd,' ',CStartzeit(numd)%s,' ',real(dmesszeit(numd),8),' ',real(dbimpulse(numd),8),   &
                        ' ',real(dbzrate(numd),8),' ',real(sdbzrate(numd),8),' ',real(d0messzeit(numd),8),' ',   &
                        real(d0impulse(numd),8), ' ',real(d0zrate(numd),8),' ',real(sd0zrate(numd),8),' ',      &
                        real(dnetrate(numd),8),' ',real(sdnetrate(numd))
                end do
            END IF

57          CONTINUE
            if(FitDecay) then
                nbb = ngrs+ncov+numd
                call ModVarsTV2(nbb)
                call CharModA1(SymboleG,nbb)
                if(.not.batest_user) write(55,*) 'size(SymboleG)=',size(SymboleG),' FitDecay=',FitDecay
            end if
            !---------------------------------------
            if(open_project_parts .and. (modSymb .or. FDecM)) then
                ! write(55,*) 'numd=',int(numd,2)
                close(25)
                goto 9000
            end if

        end if

        BACKSPACE 25

1050    continue
        do
            call DRead(25,text,ios)
            i1 = INDEX(text,'@')
            IF(i1 == 1) EXIT
        end do

        gsp1gr = .FALSE.
        Gamspk1_Fit = .false.
        IF(INDEX(text,'@Gamspk1-Grid:') == 0) THEN
            if(open_project_parts .and. GspkDT) goto 1050

! IF(INDEX(text,'@Gamspk1-Grid:') > 0) THEN
        else
            write(55,*) 'Gamspk1-Grid:'
            gsp1gr = .TRUE.
            numd = 0
            Gamspk1_Fit = .true.

            if(.true.) then
                ! must stiil be read, because of older txp files, otherwise, the old txp cannot be read correctly.
                call DRead(25,text,ios)
                i22 = INDEX(text,'CurveUse=')
                if(i22 > 0) then
                    ! old version curveuse:
                    ! READ(text(i22+10:),*) (curveuse(i),i=1,4)
                else
                    backspace 25     ! for the case, that the txp file does not containe a line "curveuse" (deprecated)
                end if
            end if

            call DRead(25,text,ios)
            i22 = INDEX(text,'UnitRadio=')
            IF(i22 > 0) then
                ! READ(text(i22+10:),*) (UnitRadio(i),i=1,7)
                kkL = len_trim(text(i22+10:))
                UnitR_effi_old = 0
                UnitR_pgamm_old = 0
                if(kkL == 13 .or. kkL == 14) then
                    READ(text(i22+10:),*) UnitRadio(1),UnitR_effi_old,UnitRadio(2),UnitR_pgamm_old,UnitRadio(3:5)
                else
                    READ(text(i22+10:),*) (UnitRadio(i),i=1,5)
                end if
                ! if(.not.batest_user) write(55,*) 'Gamspk1-Grid:   UnitRadio read: ',(Unitradio(i),i=1,7)
                if(.not.batest_user) write(55,*) 'Gamspk1-Grid:   UnitRadio read: ',(Unitradio(i),i=1,5)
            END IF

            call DRead(25,text,ios)
            i1 = INDEX(text,'MeanTyp=')
            IF(i1 > 0) THEN
                READ(text(i1+8:),*) kmwtyp
            else
                BACKSPACE 25
            end if

            call DRead(25,text,ios)

            i1 = INDEX(text,'FBT=')
            IF(i1 > 0) THEN
                READ(text(i1+4:),*) FBT
            end if
            EcorrUse = 0

            call DRead(25,text,ios)
            i1 = INDEX(text,'EcorrUse=')
            IF(i1 > 0) THEN
                READ(text(i1+9:),*) ecorrUse
            end if

            WMextSD = 0
            call DRead(25,text,ios)
            i1 = INDEX(text,'WMextSD=')
            IF(i1 > 0) THEN
                READ(text(i1+8:),*) WMextSD
                WMextSD = 0    ! since 27.7.2022: shall no longer be used
            else
                BACKSPACE 25
            end if

            numd = 0
            call InitVarsTV6(kdatmax)

            do k=1,kdatmax
                call DRead(25,text,ios)
                IF(text(1:1) == '@') THEN
                    BACKSPACE 25
                    !!!!        IF(k == 1) gsp1gr = .FALSE.
                    write(55,*) 'ProRead: goto 62: no numd value!'
                    GOTO 62
                END IF


                numd = numd + 1
                write(55,*) 'PRD: numd=',numd, ' k=',int(k,2)
                !write(0,*) 'numd=',numd,' ubound(erg,dim=1)=',ubound(erg,dim=1)
                READ(text,*) guse(numd)

                i1 = INDEX(text,'#')
                do jv = 2,14
                    text = TRIM(text(i1+1:))
                    i1 = INDEX(text,'#')
                    if(jv == 2) READ(text(1:i1-1),*) erg(numd)
                    if(jv == 3) READ(text(1:i1-1),*) GNetRate(numd)
                    if(jv == 4) READ(text(1:i1-1),*) RateCB(numd)
                    if(jv == 5) READ(text(1:i1-1),*) RateBG(numd)
                    if(jv == 6) READ(text(1:i1-1),*) SDRateBG(numd)
                    if(jv == 7) READ(text(1:i1-1),*) effi(numd)
                    if(jv == 8) READ(text(1:i1-1),*) SDeffi(numd)
                    if(jv == 9) READ(text(1:i1-1),*) pgamm(numd)
                    if(jv == 10) READ(text(1:i1-1),*) SDpgamm(numd)
                    if(jv == 11) READ(text(1:i1-1),*) fatt(numd)
                    if(jv == 12) READ(text(1:i1-1),*) SDfatt(numd)
                    if(jv == 13) READ(text(1:i1-1),*) fcoinsu(numd)
                    if(jv == 14) READ(text(1:i1-1),*) SDfcoinsu(numd)
                end do

                ! write(0,*) '1 Satz gamma-Werte eingelesen'
                if(abs(SDfcoinsu(numd) - missingval) < EPS1MIN) SDfcoinsu(numd)= 0.0_rn
                if(abs(RateBG(numd) - missingval) < EPS1MIN) RateBG(numd)= 0.0_rn
                if(abs(SDRateBG(numd) - missingval) < EPS1MIN) SDRateBG(numd)= 0.0_rn
                if(UnitR_effi_old == 1) effi(numd) = effi(numd)/100._rn
                if(UnitR_pgamm_old == 1) pgamm(numd) = pgamm(numd)/100._rn

                if(.not.batest_user) WRITE(55,*) 'numd=',numd,' ',real(erg(numd),8),' ',real(GNetRate(numd),8),     &
                    ' ',real(RateCB(numd),8),' ',real(RateBG(numd),8),' ',real(SDRAteBG(numd),8), &
                    ' ',real(effi(numd),8),' ',real(SDeffi(numd),8),' ',real(pgamm(numd),8),' ',  &
                    real(SDpgamm(numd),8),' ',real(fatt(numd),8),' ',real(SDfatt(numd),8),' ',    &
                    real(fcoinsu(numd),8),' ',real(SDfcoinsu(numd),8)
            end do

62          CONTINUE
            if(open_project_parts .and. GspkDT) then
                close (25)
                goto 9000
            end if

        END IF

        BACKSPACE 25
        BACKSPACE 25
        backspace 25

65      continue
        text = ' '
        do k=1,5
            if(open_project_parts .and. FcalDT) then
                do while (INDEX(text,'@Kalfit-Grid:') == 0)
                    call DRead(25,text,ios)
                end do
            else
                call DRead(25,text,ios)
            end if
            i1 = INDEX(text,'@Kalfit-Grid:')
            IF(i1 > 0) then
                IF(INDEX(text,'@Kalfit-Grid:') > 0) THEN
                    FitCalCurve = .true.
                    nkalpts = 0
                    call DRead(25,text,ios)
                    i22 = INDEX(text,'=')
                    kuseUfit = 1
                    read(text(i22+1:),*,iostat=ios) nkalpts,kal_polgrad,  kuseUfit
                    if(ios /= 0) then
                        kuseUfit = 1
                        read(text(i22+1:),*,iostat=ios) nkalpts,kal_polgrad
                    end if
                    use_UfitKal = .true.
                    if(kuseUfit == 0) use_UfitKal = .false.

                    if(.not.batest_user) write(55,*) '  nkalpts=',nkalpts, ' kal_polgrad=',kal_polgrad,' kuseUfit=',kuseUfit

                    call DRead(25,text,ios)
                    i22 = INDEX(text,'=')
                    read(text(i22+1:),'(a)') CCTitle
                    ! write(55,*) 'Titel=',CCtitle
                    ! read(25,'(a)') text
                    ! i22 = INDEX(text,'=')
                    ! read(text(i22+1:),'(a)') unit_ykalib

                    if(allocated(xkalib)) deallocate(xkalib,uxkalib,ykalib,uykalib)
                    allocate(xkalib(nkalpts),uxkalib(nkalpts),ykalib(nkalpts),uykalib(nkalpts))

                    do i=1,nkalpts
                        call DRead(25,text,ios)

                        i1 = INDEX(text,'#')
                        read(text(1:i1-1),*) xkalib(i)
                        text = TRIM(text(i1+1:))

                        i1 = INDEX(text,'#')
                        READ(text(1:i1-1),*) uxkalib(i)
                        text = TRIM(text(i1+1:))

                        i1 = INDEX(text,'#')
                        READ(text(1:i1-1),*) ykalib(i)

                        read(text(i1+1:),*) uykalib(i)

                    end do
                end if
                EXIT
            end if
        end do
        if(open_project_parts .and. FcalDT) then
            close (25)
            goto 9000
        end if

        BACKSPACE 25
        BACKSPACE 25
        BACKSPACE 25
        BACKSPACE 25
        do
            call DRead(25,text,ios)
            i1 = INDEX(text,'@Sonstige')
            IF(i1 > 0) EXIT
        end do

        call DRead(25,text,ios)

        i1 = INDEX(text,'=')
        i2 = index(text,'kalpha')
        IF(i1 > 0) THEN
            READ(text(i1+1:),*) kalpha
        else
            READ(text,*) kalpha
        END IF

        call DRead(25,text,ios)

        i1 = INDEX(text,'=')
        i2 = index(text,'kbeta')
        IF(i1 > 0) THEN
            READ(text(i1+1:),*) kbeta
        else
            READ(text,*) kbeta
        END IF

        call DRead(25,text,ios)

        i1 = INDEX(text,'=')
        IF(i1 > 0) THEN
            READ(text(i1+1:),*) coverf
        else
            READ(text,*) coverf
        END IF

        alpha =  1.0_rn - pnorm(kalpha)
        beta =  1.0_rn - pnorm(kbeta)
        write(66,'(a,2f9.5,2f8.5)') 'ProRead:  kalpha,kbeta,alpha,beta=',  &
            kalpha,kbeta,alpha,beta

        call DRead(25,text,ios)

        i1 = INDEX(text,'=')
        if(i1 > 0 .and. index(text,'NWGTyp') > 0) then
            ! do nothing
            ! call DRead(25,text,ios)

        elseif(i1 > 0 .and. index(text,'coverin') > 0) then
            READ(text(i1+1:),*) coverin
        else
            backspace (25)
        END IF

        call DRead(25,text,ios)
        IF(ios /= 0) goto 120

        i1 = INDEX(text,'=')
        IF(i1 > 0) THEN
            READ(text(i1+1:),*) W1minusG
        else
            READ(text,*) W1minusG
        END IF

        call DRead(25,text,ios)
        IF(ios /= 0) goto 120

        i1 = INDEX(text,'=')
        IF(i1 > 0) THEN
            READ(text(i1+1:),*,iostat=ios) GamDistAdd
        else
            READ(text,*,iostat=ios) GamDistAdd
        END IF
        if(ios /= 0) then
            GamDistAdd = 0.0_rn
            backspace (25)
        end if

        Gum_restricted = .false.

        call DRead(25,text,ios)
        IF(ios /= 0) goto 120
        if(.not.batest_user) write(55,*) 'ProRead: Gum_res textzeile : ',trim(text)
        i1 = INDEX(text,'=')
        Gum_restricted = .false.
        IF(i1 > 0) THEN
            if(index(text,'GUM_restricted=') > 0) then
                READ(text(i1+1:i1+1),'(L1)') Gum_restricted
                if(.not.batest_user) write(66,*) 'Gum_restricted=',Gum_restricted
                kModelType = 1
                gross_negative = .false.
                if(Gum_restricted) kModelType = 2
            elseif(index(text,'ModelType=') > 0) then
                i1 = index(text,'ModelType=')
                Gum_restricted = .false.
                gross_negative = .false.
                ModelType = text(i1+10:)
                if(.not.batest_user)  write(55,*) 'String ModelType: ',ModelType
                if(trim(Modeltype) == 'PosLin') then
                    Gum_restricted = .false.
                    gross_negative = .false.
                    kModelType = 1
                end if
                if(trim(ModelType) == 'GUM_restricted') then
                    Gum_restricted = .true.
                    gross_negative = .false.
                    kModelType = 2
                end if
                if(trim(ModelType) == 'NegLin') then
                    Gum_restricted = .false.
                    gross_negative = .true.
                    kModelType = 3
                end if
            end if
        else
            Backspace 25
        end if

1115    continue

        if(nvarsMD == 0) goto 120
        if(.not. allocated(fbayMD)) then
            allocate(fbayMD(nvarsMD))
            allocate(umeanMD(nvarsmd),nvMD(nvarsMD),smeanMD(nvarsMD),meanMD(nvarsMD))
        endif

        nv = 0
        do
            call DRead(25,text,ios)
            if(ios /= 0) goto 120
            i1 = INDEX(text,'@means:')
            IF(i1 > 0) EXIT
        end do

        call DRead(25,text,ios)
        if(.not.allocated(k_MDtyp)) allocate(k_MDtyp(15))          ! 21.9.2023
        k_mdtyp(1) = 0; call IntModA1(k_MDtyp,15)
        if(index(ucase(text),'MEANTYP=') > 0) then
            do jj=15,1,-1
                read(text(9:),*,iostat=ios) (k_MDtyp(j),j=1,jj)
                if(ios == 0) then
                    if(.not.batest_user) write(55,*) 'Anzahl MDtyp: jj=',jj
                    exit
                else
                    call IntModA1(k_MDtyp,jj-1)
                end if
            end do

            call DRead(25,text,ios)
            read(text(9:),*,iostat=ios) kk
            write(55,*) '   test:  kk=',int(kk,2),' ios=',int(ios,2),' text=',trim(text)
            refdataMD = 0
            if(ios == 0 .and. index(text,'refmean=') > 0) then
                refdataMD = kk
            else
                backspace (25)
                refdataMD = 0
            end if
        else
            ! backspace (25)
            if(.false. .and. i1 > 0) then
                write(55,*) 'means: entry "meantyp=" not found! Or, it is misspelled: '    ,trim(text)
                write(66,*) 'means: entry "meantyp=" not found! Or, it is misspelled: '    ,trim(text)
                call WrStatusbar(3,'Stopped: means: entry "meantyp=" not found!')
                ifehl = 1
                return
            end if

        end if
        !  if(nvarsMD > 0) write(55,*) 'Anzahl MDtyp: jj=',jj
! allocate(nvalsMD(1),meanID(1),xdataMD(1), ixdanf(1))
        if(.not.allocated(nvalsMD)) allocate(nvalsMD(1))
        if(.not.allocated(meanID)) allocate(meanID(1))
        if(.not.allocated(xdataMD)) allocate(xdataMD(1))
        if(.not.allocated(ixdanf)) allocate(ixdanf(1))
        nvalsMD(1) = 0; meanID(1)%s = ' '; xdataMD = 0.0_rn; ixdanf(1)= 0
        do j=1,jj
            write(55,*) 'j=',int(j,2),'  MDTyp(j)%s=',MDTyp(j)%s
        end do


        WRITE(66,*) 'PR: H',' ios=',int(ios,2),' next= _data'

        i1 = 1
        nddanf = 0
        do while(i1 > 0)
            call DRead(25,text,ios)
            if(ios /= 0) goto 120

            i2 = index(text,': ')
            if(i2 == 0) i2 = index(text,':')
            i3 = index(ucase(text),'_DATA')
            if(i2 > 1 .and. i3 > 0) then
                nv = nv  + 1
                if(nv > 1) then
                    call IntModA1(nvalsMD,nv)
                    call CharModA1(meanID,nv)
                    call IntModA1(ixdanf,nv)
                end if
                nvalsMD(nv) = 0
                tmeanid = text(1:i2-1)
                meanID(nv)%s = tmeanID
                write(55,*) '  meanID(',int(nv,2),')=',trim(meanID(nv)%s),' i2=',int(i2,2)
                text = text(i2+2:)
                do while (len_trim(text) > 1 .and. ios == 0)    ! 12.8.2023
                    nvalsMD(nv) = nvalsMD(nv) + 1
                    nddanf = nddanf + 1
                    call RealModA1(xdataMD,nddanf)
                    ! write(55,*) 'nv=',nv, ' nvals=',nvalsMD(nv)
                    ! read(text,*) xdataMD(nvalsMD(nv),nv)
                    if(nv == 1) then
                        read(text,*) xdataMD(nddanf)
                        if(nvalsMD(nv)== 1) ixdanf(nv) = nddanf
                    elseif(nv > 1) then
                        read(text,*) xdataMD(nddanf)
                        if(nvalsMD(nv)== 1) ixdanf(nv) = nddanf
                    end if
                    text = adjustL(text)
                    i2 = index(text,' ')
                    !     if(i2 > 5) then
                    if(i2 > 1) then
                        text = text(i2+1:)
                    else
                        exit
                    end if
                end do
                ! write(55,*) (sngl(xdataMD(i,nv)),i=1,nvalsMD(nv))
                if(.not.batest_user)   write(55,*) 'nv=',int(nv,2),' nvalsMD(nv)=',nvalsMD(nv)
                if(.not.batest_user) write(55,*) sngl(xdataMD(ixdanf(nv): ixdanf(nv)+nvalsMD(nv)-1))
            end if
        end do


120     CONTINUE
        if(open_project_parts .and. MDDT) then
            close (25)
            goto 9000
        end if

        if(ios == -1) goto 127      ! 12.8.2023

        if(.not.open_project_parts .or. (open_project_parts .and. modSymb)) then
            if(nvarsMD > 0) write(66,*) 'ixdanf=',ixdanf

            backspace (25)
            backspace (25)
            backspace (25)
            backspace (25)

            ip_binom = 0
            kbgv_binom = 0
            itm_binom = 0
            ilam_binom = 0

            do
                call DRead(25,text,ios)
                if(ios /= 0) exit
                i1 = index(text,'BinPoi=')
                if(i1 > 0) then
                    read(text(i1+7:),*,iostat=ios) ip_binom,kbgv_binom,itm_binom,ilam_binom
                    if(ios /= 0) then
                        write(66,*) 'ProRead: Error when reading the BinPoi parameter values'
                        ifehl = 1
                        call WrStatusBar(3,'Errors with BinPoi parms...' )
                        ifehl = 1
                        close (25)
                        goto 9000     !return
                    end if
                    if(.not.batest_user) write(55,'(a,4i4)') 'ip_binom,kbgv_binom,itm_binom,ilam_binom=',ip_binom,kbgv_binom, &
                        itm_binom,ilam_binom
                    write(66,'(a,4i4)') 'ip_binom,kbgv_binom,itm_binom,ilam_binom=',ip_binom,kbgv_binom, &
                        itm_binom,ilam_binom
                    exit
                end if
            end do

            if(modSymb) then
                goto 9000
            end if
        end if
127     continue

        if(.not.batest_user) write(55,*) 'ProRead: kModelType=',kModelType,' gross_negative=',gross_negative, &
            '  GamDistAdd=',sngl(GamDistAdd)
        close (25)

        if(Gamspk1_Fit) numd = numd*5

        !-----------------------------------------------------------------------------
        if(coverin > 0.0_rn) then
            do i=nab+1,ngrs
                if(abs(Stdunc(i) - missingval) > EPS1MIN) Stdunc(i) = StdUnc(i)/coverin
                if(abs(SDwert(i) - missingval) > EPS1MIN) SDwert(i) = SDwert(i)/coverin
                if(abs(HBreite(i) - missingval) > EPS1MIN) HBreite(i) = HBreite(i)/coverin

                if(abs(Stdunc(i) - missingval) > EPS1MIN) &
                    call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
                if(abs(SDwert(i) - missingval) > EPS1MIN) &
                    call WTreeViewPutDoubleCell('treeview2', 8, i, SDwert(i))
                if(abs(HBreite(i) - missingval) > EPS1MIN) &
                    call WTreeViewPutDoubleCell('treeview2', 9, i, HBreite(i))

            end do
        end if

        if(.not.batest_user)  write(55,'(a,4f7.3)') 'coverf, coverin, gamdistadd, W1minusG=',coverf, coverin, gamdistadd, W1minusG

    !call WDGetComboboxAct('ComboboxDCchains',kc)
    !   write(66,*) 'End Proread, vor TrToGTK: Chainselected=',kc
        if(.not.open_project_parts) &
            call TransferToGTK(ugr,cvgr,fit,abgr,gsp1gr,imenu1,kmwtyp)

    !call WDGetComboboxAct('ComboboxDCchains',kc)
    !   write(66,*) 'End Proread, vor 9000: Chainselected=',kc


9000    continue

    call WDSetComboboxAct('ComboboxDCchains',kc)
    call WDGetComboboxAct('ComboboxDCchains',kc)
       ! write(66,*) 'End Proread, nach 9000: Chainselected=',kc

        item_setintern = .false.
        close (25)

        WRITE(66,*) '########## End of ProRead  ##############################'
        if(consoleout_gtk) WRITE(0,*) '##### End of ProRead  ##############################'

        if(.not.batest_user) WRITE(55,*) 'End of ProRead: ngrs,ncov,numd,nvarsMD=',ngrs,ncov,numd,nvarsMD

    end subroutine ProRead

end module Pread



