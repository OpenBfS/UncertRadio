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
submodule (RG) RGA

    use UR_Gleich_globals,   only: ifehl

    !   contains:
    ! Read_Gleich
    ! EditFormelt
contains


!#######################################################################

    module recursive subroutine Read_Gleich()

        ! this routine reads and prepares the character arrays Formeltext
        ! (containing the equations) and FormetextFit, if a decay curve is fitted.
        ! These are combined into one array Formelt. The latter is modified by
        ! the routine EditFormelT by combining single formulas into one long
        ! line, if these formulas are continued over a few lines (character & at
        ! the line end). This allows to find the true numbers nglp and nglf of
        ! equations in Formeltext and FormetextFit,respectively.
        ! It initiates a majority of variables. It also finds the keywords
        ! for the involved UncertRadio functions (like Linfit, Gamspk1 and others).
        !
        ! this routine is called by ProcMainDiag and Linmod1.
        ! The array Formeltext is read in ProcmainDiag or Linmod1(2), before
        ! Read_Gleich is called, while FormeltextFit is read within Read_Gleich.
        !
        !     Copyright (C) 2014-2024  Günter Kanisch


        use UR_Gleich_globals,          only: Formeltext,FormeltextFit,formelt,LSeite,RSeite,avar,  &
                                    iavar,dialogfield_chg,kbrutto2,kEGr,kgspk1,klinf,knumEgr,    &
                                    ksumeval,Symbole,knetto,kfitcal,knetto_name,  &
                                    linfit_rename,linmod1_on,modeseval,nab,nglf,nglp,nglp_read, &
                                    ngrsp,nmodf,nparts,nux,Rnetmodi,symb_nux,syntax_check, &
                                    tmess_old,tstart_old,ufc_calc,use_sdf_brutto, &
                                    linfit_eqold,ngrs,Formeltext_out

        use UR_Linft,           only: kfitp,kfmode,k_tmess,k_tstart,kpoint_kalarg,netto_involved_Fitcal, &
                                      ifit,cauchy_failed1,cauchy_failed2,cauchy_failed3,cofact, &
                                      cofactlyt,covarlyt,export_r,FitCalCurve,FitDecay,k_rbl,k_tlive, &
                                      kalfit_arg_expr,makb,SumEval_fit
        use ur_general_globals, only: Gum_restricted
        use UR_Gspk1Fit,        only: Gamspk1_Fit,WMextSD
        use UR_DLIM,            only: nit_decl,nit_detl,kqtyp

        use, intrinsic :: iso_c_binding,      only: c_null_char,c_ptr,c_int,c_null_ptr
        use UR_gtk_globals,     only: ioption,dialogstr,consoleout_gtk
        use gtk,                only: gtk_widget_set_sensitive, GTK_BUTTONS_OK,GTK_MESSAGE_WARNING, &
                                      gtk_text_view_set_cursor_visible,gtk_text_view_place_cursor_onscreen
        use gui_functions,      only: lowcase
        use top,                only: FindItemS, idpt, WrStatusbar, CharModA1, CharModStr
        use Rout,               only: MessageShow, WDPutTextviewString, WDSetCheckMenuItem,  &
                                      WDGetTextviewString, WDPutSelRadio

        use LDN,                only: Loadsel_diag_new
        use UR_Loadsel,         only: klfrename, Sname, Soldname

        use UWB,                only: TextReplace, ChangeSname
        use CHF,                only: ucase, FindlocT, IndexArr
        use translation_module, only: T => get_translation


        implicit none


        integer              :: i1,i,i2,i3,i4,j,i21,i22,jj,k,i0,iff
        integer              :: ncitem,idif,resp,ncm,ipos(15),ngl

        logical              :: prout,frepeat
        integer(c_int)       :: res
        character(:),allocatable  :: RseiteG,text,text1,text2,str1

        !-----------------------------------------------------------------------
        allocate(character(len=800) :: RseiteG,text1,text2)
        allocate(character(len=400) :: str1)

        prout = .false.
        write(66,'(a,L1,a,L1)') 'Gum_restricted=',Gum_restricted,'   syntax_check=',syntax_check

        ! Initialize a majority of variables:
        ifehl = 0
        FitDecay = .FALSE.
        Gamspk1_fit = .FALSE.
        SumEval_fit = .false.
        FitCalCurve = .false.
        klinf = 0
        kgspk1 = 0
        ksumeval = 0
        nparts = 0
        modeSEval = 0
        nux = 0
        symb_nux = 0

        kfitp  = 0
        ngrsP = 0
        WMextSD = 0
        kbrutto2 = 0
        RnetModi = .FALSE.

        k_rbl = 0
        k_tmess = 0
        k_tstart = 0
        k_tlive = 0

        nit_decl = 0
        nit_detl = 0
        kfitcal = 0
        maKB = 0
        cofactlyt = 1._rn
        cofact = 1._rn
        covarlyt = 0._rn
        cauchy_failed1 = .false.
        cauchy_failed2 = .false.
        cauchy_failed3 = .false.
        KFmode = 0
        netto_involved_Fitcal = .false.
        kpoint_kalarg = 0
        kalfit_arg_expr = ' '
        klfrename = 0
        use_sdf_brutto = .false.
        !nglf = 0          ! added 18.8.2023, was missing
        !nmodf = 0        ! added 18.8.2023, was missing

        uFc_calc = .false.
        ! uFc_calc = .true.

        kqtyp = 1         ! 9.6.2024

        if(KnumEGr == 0) THEN
            ! ask for the number of output quantities:
            ioption = 6
            dialogstr = 'dialog_numegr'
            call FindItemS(dialogstr, ncitem)
            call Loadsel_diag_new(1, ncitem)
            if(ifehl == 1) goto 9000    ! RETURN
            if(knumEGr > 0) kEGr = 1
            write(66,'(a,i0)') '  knumEGr=',knumEGr

            if(ngrs == 0 .and. nab == 0) then
                call gtk_widget_set_sensitive(idpt('TBSaveProject'),0_c_int)
                call gtk_widget_set_sensitive(idpt('TBSaveProjectAs'),0_c_int)
                call gtk_widget_set_sensitive(idpt('MenuSaveProject'),0_c_int)
                call gtk_widget_set_sensitive(idpt('MenuSaveProjectAs'),0_c_int)
            end if
        end if

        if(knumEGr == 1) then
            call gtk_widget_set_sensitive(idpt('ConfidEllipse'),0_c_int)
        else
            call gtk_widget_set_sensitive(idpt('ConfidEllipse'),1_c_int)
        end if

        if(knumEGr < 2) then
            call gtk_widget_set_sensitive(idpt('Exchange2Symbols'), 0_c_int)
        else
            call gtk_widget_set_sensitive(idpt('Exchange2Symbols'), 1_c_int)
        end if
        frepeat = .false.

22  continue

        if(.true. .or. prout) then
            if(.not.Formeltext_out) write(66,*) 'Formeltext='
            nglp = size(Formeltext)
            nglp_read = nglp
            call modify_Formeltext(1)


            do i=1,nglp
                if(.not.Formeltext_out) write(66,'(i3,a,a)') i,' : ',Formeltext(i)%s
                if(INDEX(ucase(Formeltext(i)%s),'LINFIT') > 0) THEN
                    FitDecay = .TRUE.
                    klinf = i
                end if
                if(INDEX(ucase(Formeltext(i)%s),'GAMSPK1') > 0) THEN
                    Gamspk1_Fit = .TRUE.
                    kgspk1 = i
                end if
                if(INDEX(ucase(Formeltext(i)%s),'KALFIT') > 0) THEN
                    FitCalCurve = .TRUE.
                    kfitcal = i
                end if
                if(INDEX(ucase(Formeltext(i)%s),'SUMEVAL') > 0) THEN
                    SumEval_fit = .TRUE.
                    ksumeval = i
                end if
                ! Test for more than 1 "=" character per equation:
                i0 = index(Formeltext(i)%s,'=')
                i1 = index(Formeltext(i)%s(i0+1:),'=')
                if(i1 > 0) then
                    call CharModStr(str1,500)
                    write(str1,'(a,i0,a,2a1,a)') T('Equation') // " ",i, " " // &
                                                 T('cannot contain more than 1 "=" character!') // &
                                                 T('Error') // "!", new_line('A'), new_line('A'), &
                                                 T('See chapter 7.8 of the CHM help')

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "Read_Gleich:", resp,mtype=GTK_MESSAGE_WARNING)
                    ifehl = 1
                    call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                    call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                    call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                    goto 9000
                end if
            end do
        end if


        ngl = nglp
        if(FitDecay) then
            call WDGetTextviewString('textviewModelEQ', FormeltextFit)
            nglf = size(FormeltextFit)
            ngl = nglp + nglf
            do i=1,nglf
                write(66,'(i3,a,a)') i,' : ',FormeltextFit(i)%s
            end do
            ! end if
        end if
        Formeltext_out = .true.           ! 19.6.2024

        if(allocated(Formelt)) deallocate(Formelt)
        allocate(Formelt(ngl))
        call CharModA1(Formelt,ngl)
        do i=1,ngl
            if(i <= nglp) then
                Formelt(i)%s = Formeltext(i)%s
            else
                if(FitDecay) Formelt(i)%s = FormeltextFit(i-nglp)%s
            end if
        end do

        call EditFormelt(nglp,nglf,prout)
        ngl = nglp + nglf

        nmodf = nglf

        call CharModStr(str1,500)
        if(prout) write(66,'(a,i3,a,i3)') 'nglp=',nglp,' nglf=',nglf

        if(prout) write(66,*) 'right-hand side formulae of equations:'
        if(allocated(RSeite)) deallocate(Rseite,Lseite)
        allocate(Rseite(ngl),LSeite(ngl))
        call CharModA1(RSeite,ngl)
        call CharModA1(LSeite,ngl)

        do i=1,ngl
            i1 = INDEX(formelt(i)%s,'=')
            RSeite(i)%s = adjustL(trim(formelt(i)%s(i1+1:)))
            LSeite(i)%s = TRIM(formelt(i)%s(1:i1-1))
            if(prout) write(66,'(i0,a,a)') i,' : LS=',LSeite(i)%s
            if(prout) write(66,'(i0,a,a)') i,' : RS=',RSeite(i)%s

            if(Rseite(i)%s(1:1) == '*' .or. Rseite(i)%s(1:1) == '/') then
                call CharModStr(str1,500)
                write(str1,'(a,i0,a1,a,2a1,a)') T('The first chcracter in equation') // " ",i, &
                                                new_line('A') , " " // T('is incorrect!') , &
                                                new_line('A'), new_line('A'), trim(Rseite(i)%s)

                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
                ifehl = 1
                call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                goto 9000
            end if
        end do
        nab = nglp
        if(FitDecay) THEN
            knetto(kEGr) = klinf
            call CharModA1(Knetto_name,knumEGr)
            if(ubound(Symbole,dim=1) >= knetto(kEGr))knetto_name(kEGr) = symbole(knetto(kEGr))
            kfitp(1) = klinf

            call gtk_widget_set_sensitive(idpt('MenuDecayCurve'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('FittingModel'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('FittingData'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('FittingResult'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('ExportToR'), 1_c_int)

            call gtk_widget_set_sensitive(idpt('TBModelDialog'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('TBInputDialog'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('TBFittingResult'), 1_c_int)

            if(export_r) call WDSetCheckMenuItem('ExportToR', 1)
            if(.not.export_r) call WDSetCheckMenuItem('ExportToR', 0)

            call WrStatusbar(4, T('Define the decay curve model') // ":")

            if(frepeat) then
                frepeat = .false.
                goto 28
            end if
            if(.not. linmod1_on) then
                if(.not. syntax_check .or. (syntax_check .and. trim(dialogfield_chg) == 'Equations_main')) then
                    if(.not.linfit_rename) then
                        call Linmod1(1)
                        call CharModStr(str1,500)
                        write(str1,'(a,L1,a,a)') 'Call Linmod1:    syntax_check=',syntax_check,' dialogfield_chg=',trim(dialogfield_chg)
                        ! call MessageShow(trim(str1),GTK_BUTTONS_OK, "Symbol1:", resp,mtype=0_c_int)
                    end if
                end if
            end if

            if(syntax_check) then
                frepeat = .true.
                if(trim(dialogfield_chg) == 'Equations_main') then
                    call WDGetTextviewString('textview2', Formeltext)
                end if
                goto 22
            end if
        end if

28  continue

        if(Gamspk1_Fit) THEN
            knetto(kEGr) = kgspk1
            if(ubound(Symbole,dim=1) > 1) knetto_name(kEGr)%s = symbole(knetto(kEGr))%s
            call gtk_widget_set_sensitive(idpt('MenuGSpekt1'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('Gspk1Edit'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('Gspk1Mean'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('TBInputDialog'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('TBFittingResult'), 1_c_int)
        end if
        if(FitCalCurve) THEN
            RseiteG = ucase(Rseite(kfitcal)%s)
            call gtk_widget_set_sensitive(idpt('KalFit'), 1_c_int)
            i1 = INDEX(RSeiteG,'KALFIT')
            i2 = index(RseiteG(i1:),'(')
            i3 = index(RseiteG(i1:),',')
            i4 = index(RSeiteG(i1:),')')
            read(RseiteG(i1+i2:i1+i3-1),*) KFMode
            write(66,'(a,i0)') 'KFmode=',KFMode
            kalfit_arg_expr = RSeiteG(i1+i3:i1+i4-2)
            write(66,*) 'KALFIT:  Argument-Ausdruck=',trim(kalfit_arg_expr)
        end if
        if(SumEval_fit) THEN
            knetto(kEGr) = ksumeval
            ! call CharModA1(knetto_name,kEGr)
            ! if(ubound(Symbole,dim=1) > 1) knetto_name(kEGr)%s = symbole(knetto(kEGr))%s
            RseiteG = ucase(Rseite(ksumeval)%s)
            avar = ' '
            iavar = 0
            i1 = index(RSeiteG,'(')
            i2 = index(RSeiteG,')')
            ! write(66,'(a,/,a)') 'sumEval-string:',RseiteG
            ncm = 15   ! number of commas in the string
            call IndexArr(RseiteG,',',ncm,ipos)
            ! if(ncm > 0) write(66,'(a,i0,a,100(i0,1x)') 'ncm=',ncm,' ipos=',ipos(1:ncm)

            if(i2 > i1 .and. ncm >= 2) then
                read(RseiteG(i1+1:ipos(1)-1),*) modeSEval
                read(RseiteG(ipos(1)+1:ipos(2)-1),*) nparts
                if(prout) write(66,'(2(a,i0))') 'SumEval: nparts=',nparts,' modeSEval=',modeSEval
            end if
            if(ncm + 0 - 2 /= nparts) then
                call CharModStr(str1,500)
                write(str1,'(A)') T('The number of variables in the call of sumEval is inconsistent!') // &
                                  new_line('A') // &
                                  T('Please, check')
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Read_Gl:", resp,mtype=GTK_MESSAGE_WARNING)
                ifehl = 1
                return
            end if
            str1 = RseiteG
            iff = 0
            do j=1,nparts
                jj = j + 1          ! jj is the position of the commas in the string
                if(jj <= ncm) then
                    avar(j) = adjustL(str1(ipos(jj)+1:ipos(jj+1)-1))
                else
                    avar(j) = adjustL(str1(ipos(jj)+1:i2-1))
                end if
                if(len_trim(avar(j)) == 0) iff = iff + 1
                !  write(66,'(a,i0,a,a,a,i0)') 'j=',j,' avar=',avar(j),' iff=',iff
            end do

            if(iff > 0) then
                call CharModStr(str1,500)
                write(str1,'(A)')             &
                    T('Not all variables are listed in the SumEval call!') // new_line('A') // &
                    T('Please, correct!')
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Read_Gl:", resp,mtype=GTK_MESSAGE_WARNING)
                ifehl = 1
                return
            end if
        end if

        if(FitDecay) THEN
            nmodf = nglf
            nab = nglp
        end if
        if(Gamspk1_Fit) THEN
            nglf = 0
            nmodf = 0
            nab = nglp
        end if

        if(klinf > 0 .and. linfit_rename) then
            ! if Symbol1 has found wrong keywords for tmess and tstart in the
            ! Linfit call, these keywords are corrected below

            ! write(66,*) 'Formelt(klinf)=',trim(Formelt(klinf))
            ! write(66,*) 'linfit_eqold=',trim(linfit_eqold)
            text2 = trim(ucase(Formelt(klinf)%s))
            text1 = ucase(linfit_eqold)
            tmess_old = ''
            tstart_old = ''
            do jj=1,2
                if(jj == 1) i1 = index(text2,'TMESS')
                if(jj == 2) i1 = index(text2,'TSTART')
                idif = 0
                if(jj == 2) idif = len_trim('TMESS')-len_trim(tmess_old)
                if(i1 > 0) then
                    i21 = i1-idif
                    do k=i1-idif,1,-1
                        if(text1(k:k) == ',' .or. text1(k:k) == '(') then
                            i21 = k+1
                            exit
                        end if
                    end do
                    i22 = i1-idif
                    do k=i1-idif,len_trim(text1)+10
                        if(text1(k:k) == ',' .or. text1(k:k) == ')') then
                            i22 = k-1
                            exit
                        end if
                    end do
                    if(jj == 1) tmess_old = trim(adjustl(linfit_eqold(i21:i22)))
                    if(jj == 2) tstart_old = trim(adjustl(linfit_eqold(i21:i22)))
                end if
            end do
            write(66,*) 'tmess_old=',tmess_old,'  tstart_old=',tstart_old

            do j=nab+1,nab+nmodf
                text = trim(Formelt(j)%s)
                if(len_trim(tmess_old) > 0) then
                    ! write(66,*) 'replace tmess_old:'
                    Soldname = tmess_old
                    call TextReplace(text,tmess_old,'tmess')
                    Formelt(j)%s = TRIM(text)
                    Sname = 'tmess'
                    call ChangeSname()
                end if
                if(len_trim(tstart_old) > 0) then
                    ! write(66,*) 'replace tstart_old:'
                    Soldname = tstart_old
                    call TextReplace(text,tstart_old,'tstart')
                    Formelt(j)%s = TRIM(text)
                    do k=1,size(FormelTextFit)
                        call TextReplace(FormeltextFit(k)%s,tstart_old,'tstart')
                    end do
                    Sname = 'tstart'
                    call ChangeSname()
                end if
            end do
            if(ubound(FormeltextFit,dim=1) > 0) call WDPutTextviewString('textviewModelEQ', FormeltextFit)
            linfit_rename = .false.
            tmess_old = ''
            tstart_old = ''
        end if

        if(FitDecay .and. kEGr == 1 .and. knumEGR > 1 .and. ifit(1) > 1) then
            kEGr = 2
            call WDPutSelRadio('QFirst', kEGr)
        end if

        if(prout) write(66,'(a,i0)') 'End RGL: nglp=',nglp

9000 continue

        res = gtk_text_view_place_cursor_onscreen (idpt('textview2'))
        call gtk_text_view_set_cursor_visible(idpt('textview2'), 1_c_int)
        call gtk_text_view_set_cursor_visible(idpt('textview1'), 1_c_int)

        if(allocated(RSeiteG)) deallocate(RseiteG)
        if(allocated(text1)) deallocate(text1)
        if(allocated(text2)) deallocate(text2)
        if(allocated(text)) deallocate(text)
        if(allocated(str1)) deallocate(str1)

        write(66,'(4(a,i3))') 'At the end of Read_Gleich: nmodf = ',nmodf,'  ifehl=',ifehl,' nglp=',nglp,' nglf=',nglf
        write(66,*) '########## End of Read_Gleich  ##############################'
        if(consoleout_gtk) write(0,*) '##### End of Read_Gleich  ##############################'

    end subroutine Read_Gleich

    !#######################################################################

    module subroutine EditFormelt(nglp,nglf,prout)

        !     Copyright (C) 2014-2024  Günter Kanisch

        use UR_Gleich_globals,        only: Formelt
        use, intrinsic :: iso_c_binding,    only: c_ptr


        use Top,              only: CharModA1

        implicit none

        integer   ,intent(inout)   :: nglp,nglf
        logical, intent(in)        :: prout   ! with test output or not

        logical             :: combine    ! combine formula lines,
        ! continued with '&' at the end, into one line
        integer             :: jj,n1,nglout,i, n,ngl,k,j,klen,nglsv,nglp2,nglf2
        character(len=2)    :: crlf = char(13) // char(10)
        !-----------------------------------------------------------------------
        nglp2 = nglp
        nglf2 = nglf
        combine = .true.
        ngl = size(Formelt)
        if(prout) write(66,'(a,i0)') 'ngl=size(Formelt)=',ngl

        do i=1,ngl
            klen = len_trim(Formelt(i)%s)
            do k=1,klen
                if(k < klen) then
                    if(Formelt(i)%s(k:k) == char(13) .and. Formelt(i)%s(k:k+1) /= crlf) Formelt(i)%s(k:k) = ' '
                else
                    if(Formelt(i)%s(k:k) == char(13)) Formelt(i)%s(k:k) = ' '
                end if
            end do
        end do

15  continue
        do i=1,ngl
            if(len_trim(Formelt(i)%s) == 0) then
                do j=i,ngl-1
                    formelt(j)%s = formelt(j+1)%s
                end do
                ngl = ngl - 1
                goto 15
            end if
        end do
        if(prout) write(66,'(a,i0)') 'after removing empty formulae lines, ngl=',ngl
        nglsv = ngl

        if(prout) then
            write(66,'(a,i0)') 'before loop 1 :    nglsv=',nglsv
            do n=1,nglsv
                write(66,*) trim(Formelt(n)%s)
            end do
            write(66,*) '------'
        end if
        ! call MessageShow(trim(str1), GTK_BUTTONS_OK, "ReadGleich:", resp,mtype=0_c_int)

        nglout = 0
        do jj=1,nglsv
            if(len_trim(Formelt(jj)%s) == 0) cycle
            nglout = nglout + 1
            formelt(nglout)%s = formelt(jj)%s
            if(len_trim(formelt(nglout)%s) == 0) then
                nglout = nglout -1
                cycle
            end if

            if(nglout > 1) then
                n1 = INDEX(Formelt(nglout-1)%s,'&')
                ! write(66,'(3(a,i0),a,L1)') '   jj=',jj,'   nglout=',nglout,'  n1=',n1,' combine=',combine
                if(combine .and. n1 > 0) THEN

                    Formelt(nglout-1)%s(n1:n1) = ' '
                    Formelt(nglout-1)%s = TRIM(Formelt(nglout-1)%s) // ' ' // TRIM(ADJUSTL(Formelt(nglout)%s))
                    Formelt(nglout)%s = ' '
                    nglout = nglout - 1
                    if(prout) then
                        write(66,'(a,i3,a,i3)') 'Combination continued:     jj=',jj,'  ngl=',nglout
                        write(66,'(a)') TRIM(Formelt(nglout)%s)
                    end if
                end if
                if(jj == nglp2) nglp = nglout
                ! if(jj == nglp2) nglp = max(0,nglout)        ! 18.8.2023
            end if
        end do
        nglf = nglout - nglp
        ! nglf = max(0, nglout - nglp)    ! 18.8.2023

        if(prout) then
            write(66,'(a,i0)') 'after loop 1 :    nglout=',nglout
            do n=1,nglout
                write(66,'(a,i3,a,a)') 'n=',n,' ',trim(Formelt(n)%s)
            end do
            write(66,*) '------'
        end if
        nglsv = nglout
        call CharModA1(Formelt,nglsv)

    end subroutine EditFormelt

    !###########################################################################################

    module subroutine modify_Formeltext(mode)

        use UR_Gleich_globals,     only: nglp,nglp_read,eqnum_val,Formeltext,formelstatus,eqnumber
        use Top,           only: CharModA1

        implicit none

        integer   ,intent(in)   :: mode          ! 1: remove blank lines;  2: insert original blank lines

        integer             :: i,j,k,kplus,ke

        formelstatus = ' '

        if(mode == 1) then
            nglp = size(Formeltext)
            nglp_read = nglp

            k = nglp_read
            ke = 0
            do i=1,nglp_read
                eqnum_val(i) = .true.
                if(len_trim(Formeltext(i)%s) == 0) eqnum_val(i) = .false.
                if(eqnum_val(i)) then
                    ke = ke + 1
                    eqnumber(ke) = i
                end if
            end do
            ! write(66,*) 'mode=1:  eqnumber: ',int(eqnumber(1:ke),2)              ! outcommented 19.6.2024

            ! write(66,*) 'mode 1: eqnum_val=',eqnum_val(1:nglp_read)
            do i=1,nglp_read
                if(len_trim(Formeltext(i)%s) == 0) then
                    k = k - 1
                    do j=i,k
                        Formeltext(j)%s = Formeltext(j+1)%s
                    end do
                end if
            end do
            nglp = k


            if(nglp > 1) call CharModA1(Formeltext,nglp)

            !formelstatus = 'shortened'
            !   write(66,*) 'modify_Formeltext: ',formelstatus
            ! do i=1,k
            !   write(66,*) 'RGL:  i=',int(i,2),' Formeltext=',Formeltext(i)%s
            ! end do
        end if
    !----------------------------------------------------------------------

        if(mode == 2) then
            ! for output: Consider now blank lines as if they were equations:   21.8.2023

            if(.false. .and. size(Formeltext) == nglp_read) then
                formelstatus = 'already original'
                ! write(66,*) 'modify_Formeltext: ',formelstatus

                ! do i=1,nglp_read
                !   write(66,*) 'm2: i=',int(i,2),' Formeltext=',formeltext(i)%s
                ! end do
                return
            end if
            !--------------------------------------------

            if(nglp > 1) call CharModA1(Formeltext,nglp_read)

            ! do i=1,nglp
            !   write(66,*) 'RGL extend, Input:  i=',int(i,2),' Formeltext=',Formeltext(i)%s
            ! end do

            !write(66,*) 'mode 2: eqnum_val=',eqnum_val(1:nglp_read)
            !write(66,*) 'mode 2:  nglp=',nglp,' nglp_read=',nglp_read

            k = 0
            kplus = nglp
            do i=1,nglp_read
                if(.not.eqnum_val(i)) then
                    if(kplus < nglp_read) then
                        do j=kplus+1,i+1,-1
                            Formeltext(j)%s = Formeltext(j-1)%s
                        end do
                        kplus = kplus + 1
                        Formeltext(i)%s = ' '
                        ! write(66,*) 'i=',i,' kplus=',kplus,' Formeltext=',Formeltext(i)%s
                    end if
                end if
            end do

            ! do i=1,nglp
            !   write(66,*) 'RGL extend:  i=',int(i,2),' Formeltext=',Formeltext(i)%s
            ! end do
        end if

    end subroutine modify_Formeltext

    !###########################################################################################


end submodule RGA
