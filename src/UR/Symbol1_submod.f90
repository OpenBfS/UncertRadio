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

submodule (Sym1) Sym1A

    use UR_Gleich_globals,   only: ifehl

    !     contains
    ! Symbol1
    ! PointNach
    ! Readj_knetto
    ! Readj_kbrutto
    ! RS_numbers

contains


!-----------------------------------------------------------------------
    module subroutine Symbol1()

        ! this is a (rather long) routine, which extracts the names of symbols
        ! from the number of ngl user-defined equations as already prepared
        ! by Read_Gleich in form of the array RSeite() of the right-hand sides
        !  (RSeite()) of equations.
        ! This process is combined with error checking.
        !
        ! Simply said: the symbol names are obtained by replacing all numerical
        ! operators, including brackets, in RSeite(i) by blanks. The words are
        ! tried to be taken as symbol names, if the words do not represent a
        ! number or a function name.
        ! the symbol type (independent ('u'); dependent ('a')) is derived,
        ! which depends on whether a symbol is only located in the
        ! right-hand-side strings ('u'), or also in the left-hand-side strings ('a').
        !
        ! this longer step is followed by a syntax test with using the function
        ! parser; this means, the list of symbols found and the original RSeite(i)
        ! strings (with formulae) are supplied to the faunction parser. This
        ! safely detects syntax errors, which, if necessary, are communicated
        ! per dialog to the user, who has to correct the equation(s).
        !
        ! Major routines calles by Symbol1are:
        !   PointNach, Readj_knetto, Readj_kbrutto and RS_numbers
        !
        !     Copyright (C) 2014-2024  Günter Kanisch


        use, intrinsic :: iso_c_binding,only: c_null_char,c_ptr,c_int,c_char,c_long
        use UR_Gleich_globals,          only: Rseite,Symbole,Symbole_CP,formelt,varab,varmu,symtyp,ops,nopsfd, &
                                        RSsy,symtyp_cp,kbrutto_name,nRSsy,knetto_name,einheit,einheit_CP, &
                                        Messwert,nRssyanf,bedeutung,bedeutung_cp,SDFormel,ivtl,SDWert,  &
                                        SymboleG,ngrs,ngrs_CP,IAR,IAR_CP,HBreite,STDunc,SDFormel_CP,    &
                                        Stdunc_CP,Messwert_CP,symb_n,bipoi_gl,ifehl,ilam_binom, &
                                        ip_binom,kbgv_binom,kbrutto_gl,kEGr,kfitcal,kgspk1,klinf,knullef,  &
                                        knumEgr,ksumeval,nab,nabf,ncov,ncovf,nfkf,nglp,ngrsp,nmodf,nmu, &
                                        nsyd,nsydanf,nsyn,nvarsmd,symb_n,symlist_modified,uval_used,mdpoint, &
                                        itm_binom,loadingpro,missingval,nglf,nparts,syntax_check,knetto,  &
                                        kbrutto,ivtl_cp,SDwert_cp,HBreite_cp,avar,kpointKB,ISymbA, &
                                        symlist_shorter,uncval_exists,knetto_CP,kbrutto_CP,nonPoissGrossCounts, &
                                        apply_units,maxlen_symb,ngrs_init           !,unit_conv_fact

        use UR_Linft,           only: FitCalCurve,FitDecay,nabmx,kalfit_arg_expr,kpoint_kalarg,nmumx,  &
                                      SumEval_fit
        use UR_Perror

        use fparser,            only: initf, parsef, EvalErrMsg
        use ur_general_globals,       only: proStartNew, Gum_restricted, fd_found
        use UR_Gspk1Fit,        only: Gamspk1_Fit

        use gtk_hl_tree,        only: hl_gtk_listn_set_cell
        use UR_gtk_globals,     only: consoleout_gtk
        use gtk,                only: GTK_MESSAGE_WARNING,GTK_MESSAGE_INFO,GTK_BUTTONS_OK, &
                                      gtk_widget_set_sensitive
        use Rout,               only: MessageShow,WTreeViewGetStrArray,WTreeViewPutStrCell,WTreeViewPutDoubleCell, &
                                      WTreeViewPutComboCell,WTreeViewSetColorRow,WDPutSelRadioMenu,   &
                                      SetMenuEGr,WDSetComboboxAct
        use UR_interfaces,      only: ProcessLoadPro_new
        use Usub3,              only: TransToTV2
        use Top,                only: chupper_eq,idpt,CharModA1,IntModA1,RealModA1,InitVarsTV2,ModVarsTV2_CP,  &
                                      ModVarsTV2,CharModA1,CharModStr,load_unit_conv, idpt
        use CHF,                only: FindLocT,ucase,IsNumberE,IndexArr,testSymbol
        use Num1,               only: Quick_sort2_i
        use color_theme
        use translation_module, only: T => get_translation
        use DECH,               only: analyze_SDecay_records
        use file_io,            only: logger
        use UR_DecChain,        only: DChain

        implicit none

        integer   , parameter :: nfus = 18      ! number of primary mathematical functions           ! 28.4.2025

        integer                  :: i,i1,n,k,i2,nfx,j,ios,mfd,k2,jh
        integer                  :: resp,ngmax,nrsum,fkzahl,ix,j0,imax
        integer                  :: ifd,nzk,nzlast
        type(charv),allocatable  :: oformel(:)            ! original right-hand side of an equation
        type(charv),allocatable  :: bformel(:)            ! modified right-hand side of an equation
        type(charv),allocatable  :: oformel_rein(:)       ! cleared original right-hand side of an equation
        type(charv),allocatable  :: bformel_rein(:)       ! cleared modfied right-hand side of an equation

        character(:),allocatable :: str1
        character(:),allocatable :: str2,str3,strx
        character(len=7)         :: fus(nfus)
        character(:),allocatable :: ch2
        character(:),allocatable :: ch3
        character(:),allocatable :: RseiteG               ! upper case copy of an right-hand side formula
        integer   ,allocatable   :: ivpos(:,:),ivlen(:,:),ivanz(:)
        integer                  :: ifehlps,idel,kx,nfd,kLL,kRR
        integer                  :: icp_used(nmumx+nabmx), i_1,i_2 ,i10,js,k1len
        integer                  :: kk1,ivsort(50)
        integer                  :: nfd1,nfd2,nip,ipos(50),kstt
        real(rn)                 :: zahl
        type(charv),allocatable  :: neusym(:)   ! for new found symbols
        type(charv),allocatable  :: symb_d(:)   ! for symbols no longer be needed
        character(:),allocatable :: ckbrutto
        integer                  :: ncstr,nsyform,ihg,neli
        logical                  :: Sprot,test
        character(:),allocatable :: xstr
        character(len=3)         :: ccc
        integer   ,allocatable   :: ivpos1(:)
        type(charv),allocatable  :: cstr(:),vvv(:)
        character(len=60)        :: varabx,varmux
        character(:),allocatable :: ostr

        ! GTK:
        integer(c_int)           :: crow
        character(len=512)       :: log_str
        integer(c_long)          :: intval
        type(c_ptr)              :: tree
        !-----------------------------------------------------------------------
        !  April 2020: the former matrix RS_Symbole was replaced by a 1-dim array:
        !    RS_Symbole(i,j)  -->  RSSy(nrsum) with nrsum = nRssyanf(i) + nRssy(j) - 1
        !    nsymbRS(i,j)     -->  nRssyanf(i) + nRssy(j) - 1

        !-----------------------------------------------------------------------
        call logger(66, '##################### Symbol1: ##################')
        if(consoleout_gtk) write(0,*) '##### Symbol1: ##################'

        ops = [ '+', '-', '/', '*', '(', ')', ',', '^' ]
        fus = [ 'DEXP   ','EXP    ','LOG10  ','DLOG10 ','LOG    ','DLOG   ', &
            'DABS   ','ABS    ','DSQRT  ','SQRT   ','LINFIT ','GAMSPK1', &
            'FD     ','KALFIT ','UVAL   ','SUMEVAL','LN     ','SDECAY ' ]         ! uval introduced 11.2.2018, ln: 24.6.2022

        ! number of operators in an equation n:
        nopsfd = 0
        if(ubound(Rseite,dim=1) < nglp+nmodf) call CharModA1(RSeite,nglp+nmodf)
        do n=1,nglp+nmodf
            ! write(66,*) 'n=',int(n,2),' Rseite=',trim(Rseite(n))
            ! avoid to find from an exponential number E-01 an operator + or -.
            do i=1,len_trim(Rseite(n)%s)
                do k=1,8
                    if(Rseite(n)%s(i:i) == ops(k)) THEN
                        if(ops(k) == '-' .or. ops(k) == '+') then
                            if(IsNumberE(Rseite(n)%s,i)) cycle
                        end if
                        nopsfd(n) = nopsfd(n) + 1
                    end if
                end do
            end do
            ! write(66,'(2(a,i0))') 'Gl. ',n,' nopsfd(n)=',nopsfd(n)
        end do

        ! The symbole numbers klinf, kgspk1, knetto and others, with numbers <= nab,
        ! have already been identfied in Read_Gleich

        ! allocate(character(len=600) :: str1,str2)
        allocate(character(len=2000) :: str1,str2,strx)      ! 12.8.2023
        allocate(character(len=60) :: xstr,ch2,ostr)
        nab = 0
        nmu = 0
        nabf = 0
        nfkf = 0
        kbrutto_gl = 0
        knullef = 0
        symlist_modified = .false.
        symlist_shorter = .false.
        fd_found = .false.
        nsymbnew = 0
        nonPoissGrossCounts = .false.
        bipoi_gl = 0

        uval_used = .false.
        maxlen_symb = 0
        imax = 0     ! 2025.01.24 GK
        ix = 0       !

        Sprot = .false.
        ! Sprot = .true.

        ngrs_CP = min(ngrs_CP, ubound(Symbole_CP,dim=1))
        do i=ngrs_cp,1,-1
            if(len_trim(Symbole_CP(i)%s) > 0) then
                ngrs_CP = i
                exit
            end if
        end do

        if(len_trim(kalfit_arg_expr) > 0) then
            kpoint_kalarg = nab + nmodf + nabf + ncovf + 1      ! ??????  are they already known here?
        end if

        ngmax = nglp + nmodf
        if(allocated(nRssy)) deallocate(nRssy,nRssyanf)
        allocate(nRssy(ngmax),nRssyanf(ngmax))
        nRssy = 0
        nRssyanf = 0

        if(allocated(varab)) deallocate(varab,varmu)
        allocate(varab(1)); varab(1)%s = '  '
        allocate(varmu(1)); varmu(1)%s = '  '

        if(allocated(RSSy)) deallocate(RSSy)
        allocate(RSSY(1)); RSSy(1)%s = '  '

        allocate(oformel(nglp+nmodf),bformel(nglp+nmodf))
        allocate(oformel_rein(nglp+nmodf),bformel_rein(nglp+nmodf))


        allocate(ivpos(100,50),ivlen(100,50), ivanz(100))
        ivpos = 0; ivlen = 0;  ivanz = 0

        call CharModA1(varab,200)
        call CharModA1(varmu,200)

        nsyd = 0
        nsyn = 0

        do n=1,nglp+nmodf        ! go through the eqautions and perform checks

            oformel(n)%s = Formelt(n)%s
            bformel(n)%s = trim(ucase(oformel(n)%s))

!             if(sprot) write(66,'(a,i0,a,a,a,i0)') 'n=',n,' bformel=',bformel(n)%s,' nglp=',nglp
            if(sprot)  then
                write(log_str, '(a,i0,a,a,a,i0)') 'n=',n,' bformel=',bformel(n)%s,' nglp=',nglp
                call logger(66, log_str)
            end if
            i1 = index(bformel(n)%s,'=')
            if(i1 == 0) then
                call CharModStr(str1,600)

                write(str1,'(a,i0,a,a1,a)') T('Equation') // " ",n, ": " // &
                                            T("Equal sign '=' is missing!"), &
                                            new_line('A'), bformel(n)%s

                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
                ifehl = 1
                call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                exit
            end if
            if(n <= nglp) then
                ifd = 0
                varabx = ADJUSTL(TRIM(oformel(n)%s(1:i1-1)))
                if(nab > 1) ifd = FindlocT(varab,trim(varabx))
                if(ifd == 0) then
                    nab = nab + 1
                    varab(nab)%s = trim(varabx)
                    ! check, whether the left symbol contains a '-' character:
                    if(index(varab(nab)%s,'-') > 0) then
                        call CharModStr(str1, 256)
                        write(str1,'(a,a,a1,a)') T('The symbole') // " ", varab(nab)%s, &
                            new_line('A') , T('must not contain a minus sign!')

                        call MessageShow(trim(str1), GTK_BUTTONS_OK, "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
                        ifehl = 1
                        call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                        call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                        call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                        nab = nab - 1
                        exit
                    end if
!                     if(sprot) write(66,'(a,i0,2a)') 'nab=',nab,', Variable=',varab(nab)%s
                    if(sprot)  then
                        write(log_str, '(a,i0,2a)') 'nab=',nab,', Variable=',varab(nab)%s
                        call logger(66, log_str)
                    end if
                else
!                     if(sprot) write(66,'(a,i0,3a)') 'nab=',nab,', Variable=',trim(varabx),' already exists'
                    if(sprot)  then
                        write(log_str, '(a,i0,3a)') 'nab=',nab,', Variable=',trim(varabx),' already exists'
                        call logger(66, log_str)
                    end if
                    call CharModStr(str1, 256)

                    write(str1, *) T('An equation for the following symbol already exists') // ": ", &
                                   trim(varabx), new_line('A'), new_line('A'), &
                                   T('Please combine these equations into one!')

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, &
                                     "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
                    ifehl = 1
                    call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                    call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                    call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                    exit
                end if
            end if

            ! treating the right side of an equation:
            ! a) replace operators and intrinsic functions by blank strings
            bformel(n)%s = TRIM(ADJUSTL(bformel(n)%s(i1+1:)))
            oformel(n)%s = TRIM(ADJUSTL(oformel(n)%s(i1+1:)))

            i1 = index(bformel(n)%s,'FD')
            kLL = 0
            kRR = 0
            if(i1 > 0) then
                ! Test, whether a symbol 'FD' is followed by '(', then it is the UncertRadio function FD():
!                 write(66,*) 'Testsymbol FD : ',testSymbol(bformel(n)%s(i1:),'FD')
                write(log_str, '(*(g0))') 'Testsymbol FD : ',testSymbol(bformel(n)%s(i1:),'FD')
                call logger(66, log_str)
                do i=i1+2,len_trim(bformel(n)%s)
                    if(bformel(n)%s(i:i) == ' ') cycle
                    ! if(bformel(n)%s(i:i) /= '(') exit
                    if(bformel(n)%s(i:i) /= '(' .and. kLL == 0) exit
                    if(bformel(n)%s(i:i) == '(') KLL = kLL + 1
                    if(bformel(n)%s(i:i) == ')' .and. KLL == 1) KRR = kRR + 1
                    if(kLL == 1 .and. kRR == 1) then
                        FD_found(n) = .true.
!                         write(66,*) 'FD_found: ',bformel(n)%s(i1:i)
                        write(log_str, '(*(g0))') 'FD_found: ',bformel(n)%s(i1:i)
                        call logger(66, log_str)
                        exit
                    end if
                end do
            end if

            do i=1,len_trim(bformel(n)%s)
                do k=1,8
                    if(bformel(n)%s(i:i) == ops(k)) THEN
                        if(ops(k) == '-' .or. ops(k) == '+') then
                            if(IsNumberE(bformel(n)%s,i)) cycle
                        end if
                        bformel(n)%s(i:i) = ' '
                        oformel(n)%s(i:i) = ' '
                    end if
                    if(ichar(bformel(n)%s(i:i)) <= 31) then
                        ! this treatment introduced on 29.3.2022:
!                         write(66,'(a,i0,a,i0,a,a)') 'Warning: equation number ',n,' contains a control character: ichar=',ichar(bformel(n)%s(i:i)), &
!                             ' bformel(n)=',bformel(n)%s
                        write(log_str, '(a,i0,a,i0,a,a)') 'Warning: equation number ',n,' contains a control character: ichar=',ichar(bformel(n)%s(i:i)), &
                            ' bformel(n)=',bformel(n)%s
                        call logger(66, log_str)
                        bformel(n)%s(i:i) = ' '
                        oformel(n)%s(i:i) = ' '
                    end if
                end do
            end do
            if(index(bformel(n)%s,TRIM(fus(15))) > 0) uval_used = .true.

            do
                nfx = 0
                do k=1,nfus
                    if(.not.FD_found(n) .and. TRIM(fus(k)) == 'FD') cycle
                    i1 = index(bformel(n)%s,TRIM(fus(k)))
                    if(trim(fus(k)) == 'LN') then
                        ! 15.8.2023:  substring 'ln' in a variable: is not necessarily the function ln()
                        if(i1 > 1) then
                            if( (bformel(n)%s(i1:i1) >= 'A' .and. bformel(n)%s(i1:i1) <= 'Z') ) cycle
                            if( (bformel(n)%s(i1+2:i1+2)) /= '(' .or. adjustL(bformel(n)%s(i1+2:len_trim(bformel(n)%s))) /= '(') cycle
                        end if
                    end if

                    if(i1 > 0) THEN
                        nfx = nfx + 1
                        i2 = len_trim(fus(k))
                        bformel(n)%s(i1:i1+i2-1) = ' '
                        oformel(n)%s(i1:i1+i2-1) = ' '
                    end if
                end do
                if(nfx == 0) EXIT
            end do
!             if(sprot) write(66,*) 'bformel=',bformel(n)%s
            if(sprot)  then
                write(log_str, '(*(g0))') 'bformel=',bformel(n)%s
                call logger(66, log_str)
            end if
!             if(sprot) write(66,*) 'oformel=',oformel(n)%s
            if(sprot)  then
                write(log_str, '(*(g0))') 'oformel=',oformel(n)%s
                call logger(66, log_str)
            end if

            if(FitDecay .AND. n == klinf .AND. knumEGr > 1) THEN
                nmu = nmu + 1
                varmu(nmu)%s = 'Fitp1'
                nmu = nmu + 1
                varmu(nmu)%s = 'Fitp2'
                nmu = nmu + 1
                varmu(nmu)%s = 'Fitp3'
            end if

            ! Check, whether the new (dependent) symbol already exists:
            do i=1,nab
                if(i > nglp-nmodf ) cycle
                do k2=i+1,nab
                    ! if(k2 == i) CYCLE
                    !   write(66,'(a,i0,1x,i0,2x,4a)') 'i,k2=',i,k2,'varab(i)%s,varab(k2)%s=',varab(i)%s,'  ',varab(k2)%s
                    if(chupper_eq(varab(k2)%s,varab(i)%s)) then
                        call CharModStr(str1, 512)

                        write(str1,'(a,i0,a)') T('Equation') // " ", n, &
                                               ": " // T('The symbole') // " " // varab(i)%s // " " //&
                                               T('is multiply defined!') // new_line('A') // &
                                               T('Please, correct the equation(s)!')

                        call MessageShow(trim(str1), GTK_BUTTONS_OK, &
                                         "Symbol1:", resp, mtype=GTK_MESSAGE_WARNING)
                        ifehl = 1
                        call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                        call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                        call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                        return
                    end if
                end do
            end do

            !-------------------------------------------------------------------------------------
            ! b) extract now formula symbols from the cleared string
!             if(sprot) write(66,*) 'cleared bformel: ',bformel(n)%s
            if(sprot)  then
                write(log_str, '(*(g0))') 'cleared bformel: ',bformel(n)%s
                call logger(66, log_str)
            end if
            oformel_rein(n)%s = oformel(n)%s
            bformel_rein(n)%s = bformel(n)%s
!             if(sprot) write(66,*) 'bformel_rein: ',bformel_rein(n)%s
            if(sprot)  then
                write(log_str, '(*(g0))') 'bformel_rein: ',bformel_rein(n)%s
                call logger(66, log_str)
            end if

        end do

        if(ifehl == 1) return
!         if(sprot)  write(66,*)
        if(sprot)   then
            write(log_str, '(*(g0))')
            call logger(66, log_str)
        end if
!         if(sprot)  write(66,'(a,i0)') 'Start 2nd loop:  nab=',nab
        if(sprot)   then
            write(log_str, '(a,i0)') 'Start 2nd loop:  nab=',nab
            call logger(66, log_str)
        end if
        call CharModA1(varab,nab)   ! shorten the array

        do n=1,nglp+nmodf
            fkzahl = 0
            i10 = 1
            varmux = ''
            nip = 50
            call IndexArr(oformel(n)%s,' ', nip, ipos)

            do kk1 = 1, nip
                if(kk1 > 50) exit
                if(kk1 == 1 .and. nip == 1)  ostr = adjustL(oformel(n)%s(1:))
                if(kk1 == 1 .and. nip > 1)   ostr = adjustL(oformel(n)%s(1:ipos(1)))
                if(kk1 > 1 .and. kk1 < nip)  ostr = adjustL(oformel(n)%s(ipos(kk1-1):ipos(kk1)))
                if(kk1 > 1 .and. kk1 == nip) ostr = adjustL(oformel(n)%s(ipos(kk1-1):))

                if(kk1 == 1) kstt = 1
                if(kk1 > 1)  kstt = ipos(kk1-1) + 1

                if(len_trim(ostr) == 0 ) cycle
                varmux = trim(ostr)

                ivanz(n) = ivanz(n) + 1
                ivpos(n,ivanz(n)) = kstt
                ivlen(n,ivanz(n)) = len_trim(varmux)

                nfd1 = FindlocT(varab,trim(varmux))
                nfd2 = 0
                if(nfd1 == 0) nfd2 = FindlocT(varmu,trim(varmux))
                if(nfd1 == 0 .and. nfd2 == 0) then
                    nmu = nmu + 1
                    varmu(nmu)%s = trim(varmux)
                    !write(66,'(a,a,a,i0)') 'varmu=',varmu(nmu)%s,' nmu=',nmu
                    !write(66,*) '     i10=',int(i10,2),'  i1=',int(i1,2),'  varmux=',trim(varmux)
                    k1len = len_trim(varmu(nmu)%s)
                    if(k1len >= 1) then
                        idel = 0
                        if(varmu(nmu)%s(1:1) >= '0' .and. varmu(nmu)%s(1:1) <= '9') then
                            ! Test: dont take an exponential number with exponent (E-3)
                            neli = 0
                            READ(varmu(nmu)%s,*,IOSTAT=ios) Zahl
                            if(ios == 0) THEN
                                idel = 1
                                if(FitCalCurve) fkzahl = int(zahl+0.49)
                                ! write(66,*) 'number as variable: deleted: ',varmu(nmu)%s
                                varmu(nmu)%s = ' '
                                nmu = nmu - 1
                            end if
                        end if
                    end if
                end if
                if(nmu == 0) goto 15

                if(FitCalCurve .and. trim(ucase(varmu(nmu)%s)) == 'KALFIT') then
!                     write(66,'(a,i0)') 'FitCalCurve:  Parameter before ESKV: ',fkzahl
                    write(log_str, '(a,i0)') 'FitCalCurve:  Parameter before ESKV: ',fkzahl
                    call logger(66, log_str)
                end if

                ! Test again for a number:
                READ(varmu(nmu)%s,*,IOSTAT=ios,iomsg=str1) Zahl
                ! write(66,'(a,i0,a,a, a,a)') 'at read zahl: ios=',ios,' iomsg=',trim(str1),' varmu=',varmu(nmu)%s
                if(ios == 0) THEN
                    idel = 0
                    if(varmu(nmu)%s /= 'E') THEN
                        ! Exclusion: the symbols q1,q2 are interpreted as 0.q1
                        ! (quad-precision), i.e., as a number !
                        if(varmu(nmu)%s(1:1) /= 'q' .AND. varmu(nmu)%s(1:1) /= 'Q') THEN
                            if(Sprot) then
!                                 write(66,*) 'The last Symbol is a number, with value:',sngl(zahl)
                                write(log_str, '(*(g0))') 'The last Symbol is a number, with value:',sngl(zahl)
                                call logger(66, log_str)
!                                 write(66,'(a,a,a,i0)') '    Symbol=',varmu(nmu)%s,'    ios=',ios
                                write(log_str, '(a,a,a,i0)') '    Symbol=',varmu(nmu)%s,'    ios=',ios
                                call logger(66, log_str)
                            end if
                            nmu = nmu - 1
                            idel = 1
                            cycle
                        end if
                    end if
                end if
                if(nmu > 0) then
                    if(len_trim(varmu(nmu)%s) == 0) THEN
                        nmu = nmu - 1
                        cycle
                    end if
                end if
                ! write(66,'(a,i0)') ' nmu=',nmu
                ! write(66,*) '    Symbols: further down:  varmu  ',(trim(varmu(jjj)),' ',jjj=1,nmu)

                k1len = len_trim(varmu(nmu)%s)
                if(k1len == 0) cycle

15              continue
            end do

        end do    ! do n=1,....

        call CharModA1(varmu,nmu)   ! shortens the array varmu
!         write(66,'(a,2i4,a,i0)') 'nab,nmu=',nab,nmu,' ncov=',ncov
        write(log_str, '(a,2i4,a,i0)') 'nab,nmu=',nab,nmu,' ncov=',ncov
        call logger(66, log_str)
        ngrs = nab + nmu

        if(.not.allocated(Symbole)) call InitVarsTV2(ngrs)

        if(Sprot) then
            do n=1,nglp+nmodf
!                 write(66,'(a,i0,a,a)') 'n=',n,' ',oformel(n)%s
                write(log_str, '(a,i0,a,a)') 'n=',n,' ',oformel(n)%s
                call logger(66, log_str)
            end do
!             write(66,*) '----'
            call logger(66, '----')
            do i=1,nab
!                 write(66,'(a,i0,a,a)') 'n=',i,' ',varab(i)%s
                write(log_str, '(a,i0,a,a)') 'n=',i,' ',varab(i)%s
                call logger(66, log_str)
            end do
!             write(66,*) '----'
            call logger(66, '----')
            do i=1,nmu
!                 write(66,'(a,i0,a,a)') 'n=',i,' ',varmu(i)%s
                write(log_str, '(a,i0,a,a)') 'n=',i,' ',varmu(i)%s
                call logger(66, log_str)
            end do
        end if

        if(ngrs > ubound(Symbole,dim=1)) then
            call CharModA1(Symbole,ngrs)
            call CharModA1(Symtyp,ngrs)
            call CharModA1(einheit,ngrs)
            call CharModA1(Bedeutung,ngrs)
        end if

        do i=1,ngrs
            if(i <= nab) then
                Symbole(i)%s = varab(i)%s
                if(len_trim(symtyp(i)%s) == 0 .or. (loadingPro .and. ucase(symtyp(i)%s) /= 'P')) then
                    symtyp(i)%s = 'a'
                end if
            else
                Symbole(i)%s = varmu(i-nab)%s
                if(len_trim(symtyp(i)%s) == 0 .or. (loadingPro .and. ucase(symtyp(i)%s) /= 'M')) then
                    symtyp(i)%s = 'u'
                end if
            end if
            ! if(Sprot) write(66,*) Symbole(i)%s,' typ=',symtyp(i)%s
        end do

        ! If(Sprot) write(66,*) 'Gl. ',int(n,2),': RS_Symbole:',  &
        !                  (varmu(i)%s,' ',i=nmu_0,nmu)

        !-------------------------------------------------------------------------------------
        do n=1,nab+nmodf+nabf

            if(n > ubound(Symbole,dim=1)) then            !!  2.9.2024
                call CharModA1(Symbole,n)                   !!
                call CharModA1(Symtyp,n)                    !!
                call CharModA1(einheit,n)                   !!
                call CharModA1(Bedeutung,n)                 !!
            end if                                        !!

            if(allocated(ivpos1)) deallocate(ivpos1)
            allocate(ivpos1(ivanz(n)))
            ivpos1(1:ivanz(n)) = ivpos(n,1:ivanz(n))
            if(ivanz(n) > 0) then
                call Quick_sort2_i(ivpos1(1:ivanz(n)),ivsort)        ! sort index
                !do i=1,min(10,ivanz(n))
                !   write(66,'(4(a,i0))') 'n=',n,' i=',i,' ivpos1=',ivpos1(i),' ivsort=',ivsort(i)
                ! end do
            end if

            ! n: number of equation
            if(FitDecay) then
                if(knumEGr > 1 .and. n <= knumEGr) then
!                     if(sprot) write(66,'(3(a,i0))') 'n=',n, &
!                         ' nRSSy(n)=',nRSSy(n),' nrsum=',nrsum
                    if(sprot)  then
                        write(log_str, '(3(a,i0))') 'n=',n, &
                        ' nRSSy(n)=',nRSSy(n),' nrsum=',nrsum
                        call logger(66, log_str)
                    end if
                    if(nRSSy(n) == 0) then
                        if( n == 1) nRssyanf(n) = 1
                        if( n > 1) nRssyanf(n) = sum(nRssy(1:n-1)) + 1
                    end if
                    if(nRSsy(n) > 1) THEN
                        idel = 0
                        do j=1,nrsum - 1
                            if(RSSy(j)%s == RSSy(nrsum)%s ) then
                                idel = 1
                                !  write(66,*) 'symbol to delete:',TRIM(RS_Symbole(n,nsymbRS(n)))
                            end if
                        end do
                        if(idel == 1) then
                            nRSsy(n) = nRSsy(n) - 1
                            nrsum = nrsum - 1
                            call CharModA1(RSSy,nrsum)
                        end if
                    end if
                end if
            end if

            do j=1,size(ivpos,2)
                if(j > ivanz(n)) exit
                js = ivsort(j)
                !if(js == 0) write(66,*) 'js=0: j=',int(j,2),' size(ivpos,2)=',int(size(ivpos,2),2), &
                !                            ' size(ivsort)=',int(size(ivsort),2)
                varmux = bformel_rein(n)%s(ivpos(n,js):ivpos(n,js)+ivlen(n,js)-1)
                i2 = 0
                i1 = findlocT(varmu,trim(varmux))
                if(i1 == 0) i2 = findlocT(varab,trim(varmux))
                if(i1 > 0 .or. i2 > 0) then
                    if(i2 == 1 .AND. n == 1) CYCLE  ! Prevent that the end variable is included in
                    ! the list of the right-side symbols

                    ! eliminate double occurrences
                    ! look here for double occurrences only within the equation n, not "globally"!
                    if(nRSsy(n) > 1) then
                        j0 = 1
                        if(n > 1) j0 = sum(nRssy(1:n-1)) + 1
                        if(nrsum >= j0) then
                            idel = 0
                            do k=j0,nrsum
                                if(RSSy(k)%s == TRIM(ucase(varmux))) then
                                    idel = k
                                end if
                            end do
                            if(idel > 0) cycle
                        end if
                    end if
                    nRssy(n) = nRssy(n) + 1
                    if(nRSSy(n) == 1) then
                        if( n == 1) nRssyanf(n) = 1
                        if( n > 1) nRssyanf(n) = sum(nRssy(1:n-1)) + 1
                    end if
                    nrsum = nRssyanf(n)+ nRssy(n) - 1
                    call CharModA1(RSSy,nrsum)
                    RSSy(nrsum)%s = trim(ADJUSTL(ucase(varmux)))
                end if
            end do

            goto 148
148         continue

            If(Sprot) &
!                 write(66,'(a,i0,50(a,a))') 'Gl. ',n,'  : RS_Symbole:',(RSSy(nRssyanf(n)+i-1)%s,' ',i=1,nRSsy(n))
                write(log_str, '(a,i0,50(a,a))') 'Gl. ',n,'  : RS_Symbole:',(RSSy(nRssyanf(n)+i-1)%s,' ',i=1,nRSsy(n))
                call logger(66, log_str)

            if(len_trim(Symbole(n)%s) > maxlen_symb) maxlen_symb = len_trim(Symbole(n)%s)

        end do     ! loop over n
        !============================================================
        deallocate(varab,varmu)
        if(ifehl == 1) return
        !-------------------------------------------------------------------------------------

        if(ngrs_CP > 0) then
!             write(66,'(a,i0,a,i0)') 'SY1_634: ngrs=',ngrs,'  ngrs_CP=',ngrs_CP
            write(log_str, '(a,i0,a,i0)') 'SY1_634: ngrs=',ngrs,'  ngrs_CP=',ngrs_CP
            call logger(66, log_str)
            do i=1,ngrs
                nfd = 0
                do k=1,ngrs_CP
                    if(k > ubound(Symbole_CP,dim=1)) exit
                    if(trim(ucase(symbole(i)%s)) == trim(ucase(symbole_CP(k)%s)) .and. symtyp_CP(k)%s == 'm' ) then
                        symtyp(i)%s = symtyp_CP(k)%s
                        nfd = 1
                        exit
                    end if
                    if(trim(ucase(symbole(i)%s)) == trim(ucase(symbole_CP(k)%s)) .and. symtyp_CP(k)%s == 'p' ) then
                        symtyp(i)%s = symtyp_CP(k)%s
                        nfd = 1
                        exit
                    end if
                end do
                if(nfd == 0) then
                    ! write(66,*) '  nfd=0 (not found) for Symbol ',symbole(i)
                    if(i <= nab) symtyp(i)%s = 'a'
                    if(i > nab) symtyp(i)%s = 'u'
                end if
            end do
        end if

        if(sprot) then
!             write(66,'(a,i0,1x,i0)') 'after symbols have been identified: ngrs,ngrs_CP=',ngrs,ngrs_CP
            write(log_str, '(a,i0,1x,i0)') 'after symbols have been identified: ngrs,ngrs_CP=',ngrs,ngrs_CP
            call logger(66, log_str)
            do i=1,nab
!                 write(66,'(a,a,a,a,50(a,a))') symtyp(i)%s,' ',symbole(i)%s,'  RS: ',(RSSy(nRssyanf(i)+j-1)%s,' ',j=1,nRSsy(i))
                write(log_str, '(a,a,a,a,50(a,a))') symtyp(i)%s,' ',symbole(i)%s,'  RS: ',(RSSy(nRssyanf(i)+j-1)%s,' ',j=1,nRSsy(i))
                call logger(66, log_str)
            end do
            do i=nab+1,ngrs
!                 write(66,'(a,a,a)') symtyp(i)%s,' ',symbole(i)%s
                write(log_str, '(a,a,a)') symtyp(i)%s,' ',symbole(i)%s
                call logger(66, log_str)
            end do
        end if
        !-------------------------------------------------------------------------------------
        do i=nab,1,-1
            if(i == nab) CYCLE
            ch3 = ucase(Symbole(i)%s)
            do k=i+1,nab
                do j=1,nRSsy(k)
                    ch2 = ucase(RSSy(nRssyanf(k)+j-1)%s)
                    if(TRIM(ch2) == TRIM(ch3)) THEN
                        if(Sprot)then
!                             write(66,'(a,i0,1x,i0)') '** nab,nmu=',nab,nmu
                            write(log_str, '(a,i0,1x,i0)') '** nab,nmu=',nab,nmu
                            call logger(66, log_str)
!                             write(66,'(a,i0,2(a,a))') '** i=',i,'  ch3=',trim(ch3),'  ch2=',trim(ch2)
                            write(log_str, '(a,i0,2(a,a))') '** i=',i,'  ch3=',trim(ch3),'  ch2=',trim(ch2)
                            call logger(66, log_str)
                        end if
                        call CharModStr(str1, 512)

                        write(str1,'(8a,i0,9a)')             &
                            T('Wrong order of equations found!'), new_line('A'), new_line('A'), &
                            T('In the equation for') // " ", Symbole(k)%s,  ":" // new_line('A') // &
                            T('The symbole') // " ", Symbole(i)%s, " " // &
                            T('is defined further up, in equation') // " ", i, "!", &
                            new_line('A'), new_line('A'), &
                            T('Please, correct the order of the equation(s)!') , new_line('A'), new_line('A'), &
                            T('The symbole defined by an equation must not occur in the right-hand side of equations following it!')
                        call MessageShow(trim(str1), GTK_BUTTONS_OK, &
                                         "Symbol1:", resp, mtype=GTK_MESSAGE_WARNING)
                        ifehl = 1
                        call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                        call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                        call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                        return
                    end if
                end do
            end do
        end do
        !-------------------------------------------------------------------------------------

        do k=1,knumEGr
            if(knumEGr == 1) exit
            do j=1,nRSsy(k)
                ch2 = ucase(RSSy(nRssyanf(k)+j-1)%s)
                do i=k+1,nab
                    ch3 = ucase(Symbole(i)%s)
                    if(TRIM(ch2) == TRIM(ch3) .and. i <= knumEGr) THEN
                        call CharModStr(str1, 512)

                        write(str1, *) T('Wrong order of equations found!'), new_line('A'), new_line('A'), &
                                       T('As part of the equation for') // " ", Symbole(k)%s,' ,', new_line('A'), &
                                       T('the equation for') // " ", Symbole(i)%s // " ", &
                                       T('cannot have the position of an output quantity!'), &
                                       T('Please, correct the order of the equation(s)!')

                            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
                        ifehl = 1
                        call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                        call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                        call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                        return
                    end if
                end do
            end do
        end do

        !-------------------------------------------------------------------------------------
        if(syntax_check) then
            if(allocated(SymboleG)) deallocate(SymboleG)
            allocate(SymboleG(ngrs))
            SymboleG(1)%s = ' '
            SymboleG(2)%s = ' '

            ! write(66,*) 'Syntax-Check called:'
            ! Test the equations with the function parser, for syntax-check:

            !  Set symboleG always here! If not, RS_SymbolNR is based
            !  on the "previous symbol list"!
            do i=1,ngrs
                SymboleG(i)%s = ucase(Symbole(i)%s)
            end do

            call initf(nglp+nglf)
!             write(66,'(a,i0,a,i0)') 'fparser: initf done:   nglp=',nglp,'  nglf=',nglf
            write(log_str, '(a,i0,a,i0)') 'fparser: initf done:   nglp=',nglp,'  nglf=',nglf
            call logger(66, log_str)
            ifehlps = 0
            do i=1,nglp+nglf
                ifehlp = 0
                RseiteG = TRIM(ucase(Rseite(i)%s))
                if(index(RSeiteG,'LINFIT') > 0) CYCLE
                if(index(RSeiteG,'GAMSPK1') > 0) CYCLE
                if(index(RSeiteG,'KALFIT') > 0) CYCLE
                if(index(RSeiteG,'SUMEVAL') > 0) CYCLE
                IF(INDEX(RSeiteG,'SDECAY') > 0) CYCLE          ! 28.12.2024     28.4.2025

                call parsef(i,RSeite(i)%s,SymboleG)
!                 if(ifehlp == 1) write(66,*) 'SY1_781: ifehlp=1'
                if(ifehlp == 1)  then
                    write(log_str, '(*(g0))') 'SY1_781: ifehlp=1'
                    call logger(66, log_str)
                end if
                if(ifehl == 1) goto 9000
!                 if(Sprot) write(66,'(a,a, a,i0)') 'fparser: parsef of ',Rseite(i)%s,' done: ifehlp=',ifehlp
                if(Sprot)  then
                    write(log_str, '(a,a, a,i0)') 'fparser: parsef of ',Rseite(i)%s,' done: ifehlp=',ifehlp
                    call logger(66, log_str)
                end if
!                 if(ifehlp == 1) write(66,*) '      Rseite(i)=',RSeite(i)%s
                if(ifehlp == 1)  then
                    write(log_str, '(*(g0))') '      Rseite(i)=',RSeite(i)%s
                    call logger(66, log_str)
                end if
!                 if(ifehlp == 1) write(66,*) '      RseiteG=',RSeiteG
                if(ifehlp == 1)  then
                    write(log_str, '(*(g0))') '      RseiteG=',RSeiteG
                    call logger(66, log_str)
                end if
                if(ifehlp == 1) ifehlps = 1
            end do
            ifehlp = ifehlps
            if(ifehlp /= 0) then
                ifehl = 1
                return
            end if
            syntax_check = .false.
        end if

        !-------------------------------------------------------------------------------------
        ! Look up also the value/uncertainty grid, whether it contains already the symbols
        ncstr = ngrs + 30
        if(allocated(cstr)) deallocate(cstr)
        allocate(cstr(ncstr))
        call WTreeViewGetStrArray('treeview2', 7, ncstr, cstr)

        ! Find out, which new symbols (neusym()) have been found:
        nsyn = 0
        icp_used = 0
        nsyform = 0

        mfd = 0
        do i=1,ngrs
            ihg = 0
            do k=1,ngrs_cp
                ! if(k > ubound(Symbole_CP,dim=1)) exit
                if(k > ubound(Symbole_CP,dim=1) .or. len_trim(Symbole_CP(k)%s) == 0) then  ! 12.8.2023
                    ihg = 1
                    cycle
                end if
                if(chupper_eq(symbole_CP(k)%s, symbole(i)%s)) then
                    ihg = 1
                    icp_used(i) = 1
                    if(k > ngrs) icp_used(k) = 0
                    cycle
                end if
            end do
            if(ihg == 0 .and. len_trim(symbole(i)%s) > 0) then
                mfd = mfd + 1
                if(mfd == 1 .and. .not. allocated(neusym)) allocate(neusym(1))
                if(mfd > 1) call CharModA1(neusym,mfd)
                neusym(mfd)%s = symbole(i)%s
            end if
        end do
!         Write(66,'(3(a,i3))') 'Number of new found Symbols:  mfd=',mfd,'  ngrs=',ngrs,'  ngrs_CP=',ngrs_CP
        write(log_str, '(3(a,i3))') 'Number of new found Symbols:  mfd=',mfd,'  ngrs=',ngrs,'  ngrs_CP=',ngrs_CP
        call logger(66, log_str)

        if(ngrs_CP > 0 .and. mfd > 0) then
            call CharModA1(Symbole,ngrs+mfd)
            call CharModA1(Symbole_CP,ngrs_CP+mfd)

            do k=1,mfd
                i_1 = 0
                i_2 = 0
                do i=1,ngrs_CP
                    if(symbole_CP(i)%s == neusym(k)%s) i_2 = i
                    if(ubound(Symbole,dim=1) < i) call CharModA1(Symbole,i)
                    if(symbole(i)%s == neusym(k)%s) i_1 = i
                end do
                i = 0
                if(i_1 > 0 .and. i_2 == 0) i = i_1

                if(i > 0) then
                    ! if the Symbole(i) is a new symbol and not yet conatined in Symbole_CP:
                    ! insert the new symbol also in the _CP-array.

                    ! make the place free on array index i by shifting the upper up by one place:
                    if(ngrs_CP+1 > ubound(symtyp_CP,dim=1)) call ModVarsTV2_CP(ngrs_CP+1)
                    do j=ngrs_cp,i,-1
                        Symbole_CP(j+1)%s   = Symbole_CP(j)%s
                        symtyp_CP(j+1)%s    = symtyp_CP(j)%s
                        Einheit_CP(j+1)%s   = Einheit_CP(j)%s
                        bedeutung_CP(j+1)%s = bedeutung_CP(j)%s
                        SDFormel_CP(j+1)%s  = SDFormel_CP(j)%s
                    end do
                    MEsswert_CP(i+1:ngrs_CP+1)  = Messwert_CP(i:ngrs_CP)
                    ivtl_CP(i+1:ngrs_CP+1)      = ivtl_CP(i:ngrs_CP)
                    SDWert_CP(i+1:ngrs_CP+1)    = SDWert_CP(i:ngrs_CP)
                    HBreite_CP(i+1:ngrs_CP+1)   = HBreite_CP(i:ngrs_CP)
                    IAR_CP(i+1:ngrs_CP+1)       = IAR_CP(i:ngrs_CP)
                    STDUnc_CP(i+1:ngrs_CP+1)    = STDunc_CP(i:ngrs_CP)

                    if(knetto_CP(kEGr) >= i) knetto_CP(kEGr) = knetto_CP(kEGr) + 1
                    if(kbrutto_CP(kEGr) >= i) kbrutto_CP(kEGr) = kbrutto_CP(kEGr) + 1
                    do j=1,nvarsMD
                        if(MDpoint(j) >= i) MDpoint(j) = MDpoint(j) + 1
                    end do
                    if(ngrs_CP+1 > ubound(symtyp,dim=1)) then
                        call CharModA1(symtyp,ngrs_CP+1)
                        call CharModA1(Einheit,ngrs_CP+1)
                        call CharModA1(Bedeutung,ngrs_CP+1)
                    end if
                    do j=ngrs_CP,i,-1
                        symtyp(j+1)%s    = symtyp(j)%s
                        Einheit(j+1)%s   = Einheit(j)%s
                        bedeutung(j+1)%s = bedeutung(j)%s
                    end do

                    ! initiate the arrays at index i for the new symbol:
                    if(i <= nab) symtyp(i)%s = 'a'
                    if(i > nab) symtyp(i)%s = 'u'
                    Einheit(i)%s = ' '
                    Bedeutung(i)%s = ' '

                    if(kbgv_binom > 0) then
                        if(i <= kbgv_binom) kbgv_binom = kbgv_binom + 1
                        if(i <= ip_binom) ip_binom = ip_binom + 1
                        if(i <= itm_binom) itm_binom = itm_binom + 1
                        if(i <= ilam_binom) ilam_binom = ilam_binom + 1
                    end if

                    ngrs_CP = ngrs_CP + 1
                    ! adjust special symbole related variabels:
                    ! the index variables knetto and kbrutto can be affect from the above shifting
                    ! of array parts: see the following block, with lookig for the associated
                    ! Symbol names.

                end if
            end do

            if(apply_units) call load_unit_conv(ngrs+ncov)
        end if

        !do i=1,ngrs
        !  write(66,'(a,i0,a,a,a,a,a)') 'i=',i,' ',symtyp(i)%s,' ',symbole(i)%s, '   A: '
        !end do
        !call WTreeViewGetStrCell('treeview1',2,33,str1)

        if(.not.FitDecay .and. .not. Gamspk1_Fit .and. .not.SumEval_fit .and. ubound(Symbole,dim=1) > 1 .and.   &
            knetto(kEGr) > 0) then
            if(ubound(kbrutto_name,dim=1) == 0 .and. ubound(knetto_name,dim=1) == 0) then
                allocate(kbrutto_name(knumEGr),knetto_name(knumEGr))
                kbrutto_name(1)%s = '  '
                knetto_name(1)%s = '  '
                call CharModA1(kbrutto_name,knumEGr)
                call CharModA1(knetto_name,knumEGr)
                kbrutto_name(1)%s = Symbole(kbrutto(kEGr))%s
                knetto_name(1)%s = Symbole(knetto(kEGr))%s
            end if
            do i=1,ngrs
                if(kbrutto_name(kEGr)%s > '') then
                    if(symbole(i)%s == kbrutto_name(kEGr)%s) then
                        kbrutto(kEGr) = i
                    end if
                end if
                if(knetto_name(kEGr)%s > '') then
                    if(symbole(i)%s == knetto_name(kEGr)%s) then
                        knetto(kEGr) = i
                    end if
                end if
            end do
        end if

!         write(66,'(3(a,i3))') 'Sy1_939: knetto=',knetto(kEGr),'  kbrutto=',kbrutto(kEGr),'  ngrs_CP=',ngrs_CP
        write(log_str, '(3(a,i3))') 'Sy1_939: knetto=',knetto(kEGr),'  kbrutto=',kbrutto(kEGr),'  ngrs_CP=',ngrs_CP
        call logger(66, log_str)
        if(ubound(knetto_name,dim=1) > 0) then
            call CharModA1(knetto_name,kEGr)
            call CharModA1(kbrutto_name,kEGr)
!             write(66,*) 'knetto_name=',knetto_name(kEGr)%s, ' kbrutto_name=',kbrutto_name(kEGr)%s
            write(log_str, '(*(g0))') 'knetto_name=',knetto_name(kEGr)%s, ' kbrutto_name=',kbrutto_name(kEGr)%s
            call logger(66, log_str)
        end if

        !---------------------------------------------------------------------------------
        ! The following do loop looks for symbols in the SDformulae of standard uncertainties
        if(ngrs_CP > 0) then
            do k=1,ngrs_cp
                !write(66,*) 'Do-Schleife Uns-Formeln: Symbole_CP(',k,')=',symbole_cp(k)%s, &
                !                                         '  icp_used(',k,')=',icp_used(k)
                if(icp_used(k) == 0 .or. k == ngrs_CP) THEN
                    do j=1,ncstr
                        ! cstr() : the uncertainty formulae
                        ! Check, whether the symbol is found therein
                        ! it is also checked, that in this formula the symbol name is bracketed by
                        ! blanks or sepcial characters.
                        if(len_trim(cstr(j)%s) == 0) CYCLE
                        test = testSymbol(cstr(j)%s,symbole_CP(k)%s)
                        ! write(66,'(2(a,a),a,L1)') ' TestSymbol:  Formel=',trim(cstr(j)),' Symb=',trim(symbole_CP(k)),'   Test=',test
                        if(test) THEN
                            ihg = findlocT(symbole,symbole_CP(k)%s)
                            ! the test here: to prevent the a further down observed "symbol found in SD-Formula"
                            ! is not accepted for a second time
                            if(ihg > 0) CYCLE
                            if(k == j) cycle
                            ngrs = ngrs + 1
                            call CharModA1(Symbole,ngrs)
                            call CharModA1(symtyp,ngrs)
                            call CharModA1(einheit,ngrs)
                            call CharModA1(Bedeutung,ngrs)
                            call CharModA1(SDFormel,ngrs)
                            mfd = 1
                            icp_used(ngrs) = 1
                            symbole(ngrs)%s = symbole_CP(k)%s
!                             write(66,'(a,a,a,i0)') '   in StdDev formula found Symbol: ',symbole_CP(k)%s,'  ngrs=',ngrs
                            write(log_str, '(a,a,a,i0)') '   in StdDev formula found Symbol: ',symbole_CP(k)%s,'  ngrs=',ngrs
                            call logger(66, log_str)
                            CYCLE
                        end if
                    end do      ! j=1,ncstr
                end if
            end do
        end if
        if(nvarsMD > 0) then
            do k=1,nvarsMD
                symtyp(MDpoint(k))%s = 'm'
            end do
            call gtk_widget_set_sensitive(idpt('TBmeansMD'), 1_c_int)
        end if

        tree = idpt('treeview1')
        if(consoleout_gtk) write(0,'(a,i0)') 'Sy1_991:  before double loop:  ngrs=',int(ngrs,2),'   ngrs_CP=',ngrs_CP
        if(.false.) then
            do i=1,ngrs
!                 write(66,'(i3,4(2x,a))') i,Symbole(i)%s, symtyp(i)%s,einheit(i)%s,bedeutung(i)%s
                write(log_str, '(i3,4(2x,a))') i,Symbole(i)%s, symtyp(i)%s,einheit(i)%s,bedeutung(i)%s
                call logger(66, log_str)
            end do
            do i=1,ngrs_CP
!                 write(66,'(a,i3,4(2x,a))') 'CP: ',i,Symbole_CP(i)%s, symtyp_CP(i)%s,einheit_CP(i)%s,bedeutung_CP(i)%s
                write(log_str, '(a,i3,4(2x,a))') 'CP: ',i,Symbole_CP(i)%s, symtyp_CP(i)%s,einheit_CP(i)%s,bedeutung_CP(i)%s
                call logger(66, log_str)
            end do
        end if

        !!!! call RS_numbers()

        do i=1,ngrs
            intval = i
            crow = i - 1
            if(consoleout_gtk) Write(0,'(a,i0,a)') 'double loop: i=',i,' , before column 1'
            write(ccc,'(i3.0)') i
            call hl_gtk_listn_set_cell(tree, col=0_c_int, row=crow, svalue=trim(ccc))
            if(consoleout_gtk) then
                Write(0,'(a,i0,a)') 'Double loop: i=',i,' , before cols 2-3'
                write(0,*) 'Symbole(i)=',symbole(i)%s,' symtyp=',symtyp(i)%s
            end if
            if(consoleout_gtk) Write(0,*) 'before col Symbole(i)'
            xstr = max(' ',Symbole(i)%s)
            call WTreeViewPutStrCell('treeview1', 2, i, xstr)
            if(consoleout_gtk) Write(0,*) 'before col symtyp(i)'
            xstr = max(' ',symtyp(i)%s)
            call WTreeViewPutStrCell('treeview1', 3, i, xstr)
            call WTreeViewSetColorRow('treeview1',i, get_color_string('frame_bg'))

            mfd = 0
            do k=1,ngrs_cp
                if(chupper_eq(symbole_CP(k)%s, symbole(i)%s)) then
                    if(consoleout_gtk) Write(0,*) 'double loop: i=',int(i,2),'k=',int(k,2),', before TV1 cols 4+5'
                    ! The did already exist:
                    xstr = max(' ',einheit_CP(k)%s)
                    call WTreeViewPutStrCell('treeview1', 4, i, xstr)
                    if(consoleout_gtk) Write(0,*) 'double loop: i=',int(i,2),'k=',int(k,2),', after TV1 col. 4'
                    xstr = max(' ',Bedeutung_CP(k)%s)
                    call WTreeViewPutStrCell('treeview1', 5, i, xstr)
                    if(consoleout_gtk) Write(0,*) 'double loop: i=',int(i,2),'k=',int(k,2),', after TV1 col. 5'

                    symtyp(i) = symtyp_CP(k)
                    einheit(i)%s = einheit_CP(k)%s
                    bedeutung(i)%s = bedeutung_CP(k)%s    !
                    MEsswert(i) = Messwert_CP(k)
                    ivtl(i) = ivtl_CP(k)
                    SDFormel(i)%s = SDFormel_CP(k)%s
                    SDWert(i) = SDWert_CP(k)
                    HBreite(i) = HBreite_CP(k)
                    IAR(i) = IAR_CP(k)
                    STDUnc(i) = STDunc_CP(k)
                    icp_used(i) = icp_used(k)
                    if(consoleout_gtk) Write(0,*) 'double loop: after copying'
                    if(consoleout_gtk) Write(0,*) 'double loop: i=',int(i,2),'k=',int(k,2),', before TV2 cols. 5-11'
                    call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert(i))
                    call WTreeViewPutComboCell('treeview2', 6, i, ivtl(i))
                    xstr = max(' ',SDFormel(i)%s)
                    call WTreeViewPutStrCell('treeview2', 7, i, xstr)
                    call WTreeViewPutDoubleCell('treeview2', 8, i, SDWert(i))
                    call WTreeViewPutDoubleCell('treeview2', 9, i, HBreite(i))
                    call WTreeViewPutComboCell('treeview2', 10, i, IAR(i))
                    call WTreeViewPutDoubleCell('treeview2', 11, i, missingval)
                    ! write(66,*) 'TV2 written (a) for Symbol i=',int(i,2),' k_cp=',int(k,2),' _cp: ',symbole_CP(k)%s, ' ohne _cp:',symbole(i)%s
                    !-------------------
                    if(k <= ngrs) icp_used(k) = 1
                    mfd = 1
                    Exit
                end if
            end do     ! k=1,ngrs_CP

            if(ngrs_CP > 0 .and. mfd == 0 .and. len_trim(symbole(i)%s) > 0) THEN
                ! the symbol is new:
                !write(66,'(a,i0,a,i0,a,a,3(a,i0))') 'SY1_1066:   New added Symbol: i=',i,' ichar=',ichar(symbole(i)%s), &
                !     '  Symbol=',symbole(i)%s,'  ngrs=',ngrs,' ngrs_CP=',ngrs_CP,' mfd=',mfd
                call ModvarsTV2(ngrs)
                if(ngrs > ix) call CharModA1(cstr,ngrs)

                do j=ngrs-1,i,-1
                    Einheit(j+1)%s     = Einheit(j)%s
                    Bedeutung(j+1)%s   = Bedeutung(j)%s
                    symtyp(j+1)%s      = symtyp(j)%s
                    SDformel(j+1)%s    = SDformel(j)%s
                    cstr(j+1)%s        = cstr(j)%s
                end do
                if(allocated(vvv)) deallocate(VVV)
                allocate(VVV(1))
                vvv(1)%s = ' '
                Einheit = [ einheit(1:i-1), vvv(1) , einheit(i+1:ngrs) ]
                Bedeutung = [ Bedeutung(1:i-1), vvv(1) , Bedeutung(i+1:ngrs) ]
                symtyp = [ symtyp(1:i-1), vvv(1) , symtyp(i+1:ngrs) ]
                SDFormel = [ SDFormel(1:i-1), vvv(1) , SDFormel(i+1:ngrs) ]
                cstr = [ cstr(1:i-1), vvv(1) , cstr(i+1:ngrs) ]

                Messwert = [ Messwert(1:i-1), missingval, Messwert(i+1:ngrs) ]
                IVTL = [ IVTL(1:i-1), 1, IVTL(i+1:ngrs) ]
                SDWert = [ SDWert(1:i-1), missingval, SDWert(i+1:ngrs) ]
                IAR = [ IAR(1:i-1), 1, IAR(i+1:ngrs) ]
                HBreite = [ Hbreite(1:i-1), missingval, HBreite(i+1:ngrs) ]
                StdUnc = [ StdUnc(1:i-1), missingval, StdUnc(i+1:ngrs) ]

                einheit(i)%s = ' '
                bedeutung(i)%s = ' '
                cstr(i)%s = ' '
                if(i <= nab) symtyp(i)%s = 'a'
                if(i > nab) symtyp(i)%s = 'u'

                ! free the grid cells in row i:
                tree = idpt('treeview1')
                call WTreeViewPutStrCell('treeview1', 4, i, ' ')
                call WTreeViewPutStrCell('treeview1', 5, i, ' ')

                if(ngrs_cp > 0) THEN
                    nsyn = nsyn + 1
                    ix = ubound(symb_n,dim=1)
                    if(nsyn > ix) call CharModA1(symb_n,nsyn)
                    vvv(1)%s = symbole(i)%s
                    symb_n = [ symb_n(1:nsyn-1), vvv(1) ]
                    if (get_theme_name() /= 'contrast') then
                        call WTreeViewSetColorRow('treeview1',i, '#00FF48')         ! green
                    else
                        call WTreeViewSetColorRow('treeview1',i, '#1C891D')         ! green
                    end if
                end if
                Messwert(i) = missingval
                ivtl(i) = 1
                SDFormel(i)%s = ' '
                SDWert(i) = missingval
                HBReite(i) = missingval
                IAR(i) = 1
                StdUnc(i) = missingval
                tree = idpt('treeview2')

                write(ccc,'(i3.0)') i
                call hl_gtk_listn_set_cell(tree, col=0_c_int, row=crow, svalue=trim(ccc))
                xstr = max(' ',Symbole(i)%s)
                call WTreeViewPutStrCell('treeview2', 2, i, xstr)
                xstr = max(' ',symtyp(i)%s)
                call WTreeViewPutStrCell('treeview2', 3, i, xstr)
                xstr = max(' ',einheit(i)%s)
                call WTreeViewPutStrCell('treeview2', 4, i, xstr)
                xstr = max(' ',SDFormel(i)%s)
                call WTreeViewPutStrCell('treeview2', 7, i, xstr)
                call WTreeViewPutComboCell('treeview2', 6, i, ivtl(i))
                call WTreeViewPutComboCell('treeview2', 10, i, IAR(i))
                call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert(i))
                call WTreeViewPutDoubleCell('treeview2', 8, i, SDWert(i))
                call WTreeViewPutDoubleCell('treeview2', 9, i, HBreite(i))
                call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
                ! write(66,*) 'TV2 written (b) for Symbol i=',int(i,2),' ',symbole(i)%s)
                !------
                symlist_modified = .true.
            end if
        end do       ! i=1,ngrs
        if(apply_units) call load_unit_conv(ngrs+ncov)

        if(consoleout_gtk)  write(0,*) 'Sy1_1146:  after double loop'
        !-------------------
        if(ubound(knetto_name,dim=1) > 0) then
            call Readj_knetto()
            call Readj_kbrutto()
        end if
        !-------------------

        do i=1,ngrs
            if(icp_used(i) == 0) icp_used(i) = 1
            !  write(66,'(a,i3,2x,a,2x,a,i1,2x,a,2x,a)') 'before nsyneu-Start: ',i,symbole(i)%s,'  icpused=',icp_used(i), &
            !                                            symbole_CP(i)%s
        end do
        if(.false. .and. ngrs_CP > ngrs) then
            do i=ngrs+1,ngrs_CP
                if(i > ubound(Symbole_CP,dim=1)) exit
!                 write(66,'(a,3x,2x,a17,2x,a,1x,2x,a,2x,a)') 'before nsyneu-Start: ',symbole_CP(i)%s
                write(log_str, '(a,3x,2x,a17,2x,a,1x,2x,a,2x,a)') 'before nsyneu-Start: ',symbole_CP(i)%s
                call logger(66, log_str)
            end do
        end if
!         write(66,*) 'Testing SD-formulae:'
        call logger(66, 'Testing SD-formulae:')
        if(consoleout_gtk) write(0,'(2(a,i0))') 'Sy1_1166: Testing SD formulae:   ngrs=',ngrs,' nabf=',nabf
        do i=1,ngrs
!             if(len_trim(SDFormel(i)%s) > 0) write(66,'(a,i3,a,a)') '    i=',i,'  SDFormula=',SDFormel(i)%s
            if(len_trim(SDFormel(i)%s) > 0)  then
                write(log_str, '(a,i3,a,a)') '    i=',i,'  SDFormula=',SDFormel(i)%s
                call logger(66, log_str)
            end if
            if(allocated(SDFormel_CP)) then
!                 if(len_trim(SDFormel_CP(i)%s) > 0) write(66,'(a,i3,a,a)') '    i=',i,'  SDFormula_CP=',SDFormel_CP(i)%s
                if(len_trim(SDFormel_CP(i)%s) > 0)  then
                    write(log_str, '(a,i3,a,a)') '    i=',i,'  SDFormula_CP=',SDFormel_CP(i)%s
                    call logger(66, log_str)
                end if
            end if
        end do


        str1 = ' '
        if(nsyn > 0) THEN
            ! new symbols have been added: index numbers knetto, kbrutto, klinf and
            ! kgspk1 are to be readjusted!
            ! Also the symbol indexes in the covar-grid must be readjusted.

            if(.not.allocated(knetto_name)) allocate(knetto_name(knumEGr))   ! 20.9.2023
            if(.not.allocated(kbrutto_name)) allocate(kbrutto_name(knumEGr)) !

            if(FitDecay .OR. Gamspk1_fit) THEN
                if(size(knetto_name,1) == 0) call CharModA1(knetto_name, knumEGr)   ! 20.9.2023
                if(size(kbrutto_name,1) == 0) call CharModA1(kbrutto_name, knumEGr) !
                if(FitDecay) THEN
                    knetto(kEGr) = klinf
                    knetto_CP(kEGr) = knetto(kEGr)
                    knetto_name(kEGr)%s = symbole(knetto(kEGr))%s
                    kbrutto(kEGr) = 0
                    kbrutto_CP(kEGr) = 0
                    ckbrutto = ' '
                end if
                if(Gamspk1_fit) THEN
                    knetto(kEGr) = kgspk1
                    knetto_CP(kEGr) = knetto(kEGr)
                    knetto_name(kEGr)%s = symbole(knetto(kEGr))%s
                    kbrutto(kEGr) = 0
                    kbrutto_CP(kEGr) = 0
                end if
            end if
            if(allocated(str1)) deallocate(str1)
            allocate(character(len=1000) :: str1)

            if(ngrs_init > 0) then
                !output of symbols, 8 symbols per line:
                nzk = nsyn/8
                nzlast = nsyn - nzk*8
                str1 = ' '
                do k=1,nzk
                    strx = ' '
                    do j=1,8
                        if(j == 1) strx = trim(strx) // symb_n((k-1)*8 + j)%s
                        if(j > 1) strx = trim(strx) // ',  ' // symb_n((k-1)*8 + j)%s
                    end do
                    str1 = trim(str1) // trim(strx) // ', ' // new_line('A')
                end do
                if(nzlast > 0) then
                    strx = ' '
                    do j=1,nzlast
                        strx = trim(strx) // ',  ' // symb_n(nzk*8 + j)%s
                    end do
                    str1 = trim(str1) // trim(strx) // new_line('A')
                end if

                str1 = T("The following symbols in the equations are new!") // new_line('A') // new_line('A')
                str1 = str1 // T("The corresponding rows in the table are highlighted green!") // new_line('A') // new_line('A')
                str1 = str1 // T("Please, check for correctness!")

                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
            end if
        end if

        if(sprot) then
            kx = max(ngrs, ngrs_CP)
!             write(66,'(2(a,i0))') 'ngrs=',ngrs,' ngrs_CP=',ngrs_CP
            write(log_str, '(2(a,i0))') 'ngrs=',ngrs,' ngrs_CP=',ngrs_CP
            call logger(66, log_str)
            do i=1,kx
                if(i <= ngrs) then
!                     write(66,'(a,i3,a,a1,a,a,a,i1,a,es15.8)') 'i=',i,' ',symtyp(i)%s,'  ', symbole(i)%s,' icp_used=',icp_used(i),'  mw=',sngl(messwert(i))
                    write(log_str, '(a,i3,a,a1,a,a,a,i1,a,es15.8)') 'i=',i,' ',symtyp(i)%s,'  ', symbole(i)%s,' icp_used=',icp_used(i),'  mw=',sngl(messwert(i))
                    call logger(66, log_str)
                else
!                     write(66,'(a,i3,a,a1,a,a,a,i1,a,es15.8)') 'i=',i,' ','     ','  ', '    ',' icp_used=',icp_used(i),'  mw='
                    write(log_str, '(a,i3,a,a1,a,a,a,i1,a,es15.8)') 'i=',i,' ','     ','  ', '    ',' icp_used=',icp_used(i),'  mw='
                    call logger(66, log_str)
                end if
            end do
        end if

! Append at the end such symbols, which are no longer found in the formulae:
        nsyd = 0
        nsydanf = 190
        if(allocated(symb_d)) deallocate(symb_d)
        do k=1,ngrs_cp
            if(k > ubound(Symbole_CP,dim=1)) exit

            if(nsyd > 0) then
                mfd = 0
                do i=1,nsyd
                    if(chupper_eq(symbole_CP(k)%s, symb_d(i)%s)) then
                        mfd = 1
                        exit
                    end if
                end do
                if(mfd == 1) cycle
            end if
            if(icp_used(k) >= 0) THEN
                if(len_trim(symbole_CP(k)%s) == 0) cycle
                mfd = 0
                do i=1,ngrs
                    if(k <= ngrs_CP .and. chupper_eq(symbole_CP(k)%s, symbole(i)%s)) then
                        mfd = 1
                        exit
                    end if
                end do
                if(mfd == 1) cycle
                do jh=1,ngrs-1
                    if(chupper_eq(symbole_CP(k)%s, symbole(jh)%s)) then
                        mfd = 1
                        exit
                    end if
                end do
                if(mfd == 1) cycle

                if(.not.(Symbole_CP(k)%s(1:1) >= '0' .and. Symbole_CP(k)%s(1:1) <= '9') .and. .true.) then
                    ! append at the end:
                    ngrs = ngrs + 1

                    ix = ubound(SymboleG,dim=1)
                    if(ngrs > ix) call CharModA1(SymboleG,ngrs)
                    ix = ubound(Symbole,dim=1)
                    ! call ModVarsTV2(ngrs)  ! do not use it here!

                    if(ngrs > ix) call CharModA1(Symbole,ngrs)
                    if(ngrs > ubound(symtyp,dim=1)) call CharModA1(symtyp,ngrs)
                    if(ngrs > ubound(einheit,dim=1)) call CharModA1(einheit,ngrs)
                    if(ngrs > ubound(bedeutung,dim=1)) call CharModA1(bedeutung,ngrs)
                    if(ngrs > ubound(Messwert,dim=1)) call RealModA1(Messwert,ngrs)
                    if(ngrs > ubound(IVTL,dim=1)) call IntModA1(IVTL,ngrs)
                    if(ngrs > ubound(SDformel,dim=1)) call CharModA1(SDformel,ngrs)
                    if(ngrs > ubound(SDwert,dim=1)) call RealModA1(SDwert,ngrs)
                    if(ngrs > ubound(HBreite,dim=1)) call RealModA1(HBreite,ngrs)
                    if(ngrs > ubound(IAR,dim=1)) call IntModA1(IAR,ngrs)
                    if(ngrs > ubound(StdUnc,dim=1)) call RealModA1(StdUnc,ngrs)

                    tree = idpt('treeview1')

                    write(ccc,'(i3.0)') ngrs-2
                    call hl_gtk_listn_set_cell(tree, col=0_c_int, row=ngrs-2, svalue=trim(ccc))
                    xstr = max(' ',Symbole_CP(k)%s)
                    call WTreeViewPutStrCell('treeview1', 2, ngrs, xstr)
                    xstr = max(' ',symtyp_CP(k)%s)
                    call WTreeViewPutStrCell('treeview1', 3, ngrs, xstr)
                    xstr = max(' ',einheit_CP(k)%s)
                    call WTreeViewPutStrCell('treeview1', 4, ngrs, xstr)
                    xstr = max(' ',bedeutung_CP(k)%s)
                    call WTreeViewPutStrCell('treeview1', 5, ngrs, xstr)
!                     write(66,'(a,a,a,i0)') ' xxxxxx nsyd-Symbol: ',symbole_CP(k)%s,'   in row ngrs=',ngrs
                    write(log_str, '(a,a,a,i0)') ' xxxxxx nsyd-Symbol: ',symbole_CP(k)%s,'   in row ngrs=',ngrs
                    call logger(66, log_str)
                    nsyd = nsyd + 1
                    nmu = nmu + 1
                    if(nsyd == 1) nsydanf = ngrs

                    if(nsyd == 1) allocate(symb_d(1))
                    if(nsyd > 1) call CharModA1(symb_d,nsyd)
                    symb_d(nsyd)%s = symbole_CP(k)%s
                end if
            end if
        end do

        str2 = ' '
!         if(sprot) write(66,'(a,i0)') 'Searching for symbols not used in equations: nsyd=',nsyd
        if(sprot)  then
            write(log_str, '(a,i0)') 'Searching for symbols not used in equations: nsyd=',nsyd
            call logger(66, log_str)
        end if
        if(nsyd > 0) THEN
            if(loadingPRO) GOTO 27

            symlist_modified = .true.
            symlist_shorter = .true.

            str2 = T("The following symbols are not found in the equations!") // new_line('A')//new_line('A')
            do i=1,nsyd
                str2 = str2 // symb_d(i)%s // new_line('A')
            end do
            str2 = str2 // new_line('A') // &
                T("The corresponding rows in the table are highlighted yellow!") // new_line('A')//new_line('A')  //   &
                T("Ignore this hint, if they belong to standard deviation formulae!")  &
                // new_line('A') // new_line('A') // &
                T("If no longer needed: remove these symbols with toolbar icon 'Delete grid line(s)'.") // new_line('A')
27          continue

!             write(66,'(a,L1,3(a,i0))') 'proStartNew=',proStartNew,'  nsydanf=',nsydanf,' ngrs=',ngrs,' nab=',nab   !!!!!!! 29.4.2025
            write(log_str, '(a,L1,3(a,i0))') 'proStartNew=',proStartNew,'  nsydanf=',nsydanf,' ngrs=',ngrs,' nab=',nab   !!!!!!! 29.4.2025
            call logger(66, log_str)
            if(nsyd > 0 .and. .not.proStartNew) then
                do i=nsydanf,nsydanf+nsyd-1
                    if(i <= nsydanf+nsyd-1) then
                        call WTreeViewSetColorRow('treeview1',i, '#FFDF00')         ! yellow
                    else
                        if(len_trim(symbole(i)%s) > 0) call WTreeViewSetColorRow('treeview1',i, '#FFDF00')         ! yellow
                    end if
                end do
            end if
        end if

        do i=1,nab
            if(symtyp(i)%s == 'u') symtyp(i)%s = 'a'
        end do
        do i=nab+1,ngrs
            ix = ubound(symtyp,dim=1)
            if(i > ix) call CharModA1(symtyp,i)
            if(len_trim(symtyp(i)%s) == 0) symtyp(i)%s = 'u'
            call WTreeViewPutStrCell('treeview1',3,i,symtyp(i)%s)
        end do

        if(len_trim(str2) > 0) THEN
            if(allocated(str3)) deallocate(str3)
            allocate(character(len=len_trim(str1)+len_trim(str2)+100) :: str3)
            str3 = TRIM(str1) // new_line('A') // &
                '__________________________________________________________' // new_line('A') // new_line('A') ! &
            ! // TRIM(str2)

            if(len_trim(str1) > 10) call MessageShow(trim(str3), GTK_BUTTONS_OK, "Symbol1:", resp,  &
                mtype=GTK_MESSAGE_INFO)
            deallocate(str3)

        end if
        if(.not.Gum_restricted .and. .not.FitDecay .and. .not.Gamspk1_Fit .and. .not.DChain) then       ! 27.4.2025
            call WDSetComboboxAct('comboboxGrossRate',kbrutto(kEGr))
            call WDSetComboboxAct('comboboxNetRate',knetto(kEGr))
        end if

        do i=ngrs+1,ngrs+10          ! + 30
            call WTreeviewPutStrCell('treeview1',2,i,' ')
            call WTreeviewPutStrCell('treeview1',3,i,' ')
            call WTreeviewPutStrCell('treeview1',4,i,' ')
            call WTreeviewPutStrCell('treeview1',5,i,' ')
        end do

        if(SumEval_fit .and. nparts > 0) then
            do i=1,nparts
                j = FindlocT(SymboleG,trim(avar(i)))
                if(j > nab) then
                    call CharModStr(str1, 512)
                    write(str1, *)       &
                        T('The variable listed in SumEval') // " ", trim(avar(i)) // " ", &
                        T('is listed as independent!'), new_line('A'), &
                        T('It must either be defined with an equation or deleted in SumEval!'), new_line('A'), &
                        T('Please correct!')

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
                    ifehl = 1
                    return
                end if
            end do
        end if
        if(DChain) then
          ! 27.4.2025 GK
          call analyze_Sdecay_Records()
        end if
        !-----------------------------------------------------------------------
        if(allocated(kpointKB)) deallocate(kpointKB)
        imax = ubound(IsymbA,dim=1)
        do i=imax,1,-1
            if(IsymbA(i) > 0) then
                imax = i
                exit
            end if
        end do
!         if(sum(IsymbA) > 0) write(66,'(a,30(i0,1x))') 'before PointNach(1):   IsymbA=',IsymbA(1:imax)
        if(sum(IsymbA) > 0)  then
            write(log_str, '(a,30(i0,1x))') 'before PointNach(1):   IsymbA=',IsymbA(1:imax)
            call logger(66, log_str)
        end if
!         write(66,'(a,i0,a,i0)') 'before PointNach(1):   ncov=',ncov,' ngrsP=',ngrsP
        write(log_str, '(a,i0,a,i0)') 'before PointNach(1):   ncov=',ncov,' ngrsP=',ngrsP
        call logger(66, log_str)

        call PointNach(1)
        if(ifehl == 1) return


        call RS_Numbers()

        if(symlist_modified .and. uncval_exists) then
            call TransToTV2()
        end if

        !-----------------------------------------------------------------------
        !  Test the (right-hand sides of) equations by the function parser:
        do i=1,ngrs
            if(len_trim(Symbole(i)%s) > maxlen_symb) maxlen_symb = len_trim(Symbole(i)%s)
        end do

        call initf(nab)
        do i=1,nab
!             if(sprot) write(66,'(a,i3,a,a)') 'i=',i,' Rseite = ',Rseite(i)%s
            if(sprot)  then
                write(log_str, '(a,i3,a,a)') 'i=',i,' Rseite = ',Rseite(i)%s
                call logger(66, log_str)
            end if
            if(len_trim(Symbole(i)%s) > maxlen_symb) maxlen_symb = len_trim(Symbole(i)%s)
        end do
        ifehlps = 0
        do i=1,nab
            ifehlp = 0
            RseiteG = TRIM(ucase(Rseite(i)%s))
            if(index(RSeiteG,'LINFIT') > 0) THEN
                FitDecay = .TRUE.
                klinf = i
                CYCLE
            end if
            if(FitDecay .AND. i == klinf) CYCLE

            if(index(RSeiteG,'GAMSPK1') > 0) THEN
                Gamspk1_Fit = .TRUE.
                kgspk1 = i
                CYCLE
            end if
            if(Gamspk1_Fit .AND. i == kgspk1) CYCLE

            if(index(RSeiteG,'KALFIT') > 0) THEN
                FitCalCurve = .TRUE.
                kfitcal = i
                !  write(66,*) 'RseiteG=',trim(RseiteG)
                CYCLE
            end if
            if(FitCalCurve .AND. i == kfitcal) CYCLE

            if(index(RSeiteG,'SUMEVAL') > 0) THEN
                SumEval_fit = .TRUE.
                ksumeval = i
                CYCLE
            end if
            if(SumEval_Fit .AND. i == ksumeval) CYCLE

            IF(INDEX(RSeiteG,'SDECAY') > 0) THEN            ! 15.12.2024  GK  28.4.2025
                !  write(66,*) 'RseiteG=',trim(RseiteG)
              CYCLE
            END IF


!             if(Sprot) write(66,*) 'fparser: parsef of ',Rseite(i)%s,' : '
            if(Sprot)  then
                write(log_str, '(*(g0))') 'fparser: parsef of ',Rseite(i)%s,' : '
                call logger(66, log_str)
            end if
            call parsef(i,RSeite(i)%s,SymboleG)
!             if(Sprot) write(66,*) 'fparser: parsef of ',Rseite(i)%s,' done: ifehlp=',ifehlp
            if(Sprot)  then
                write(log_str, '(*(g0))') 'fparser: parsef of ',Rseite(i)%s,' done: ifehlp=',ifehlp
                call logger(66, log_str)
            end if
!             if(ifehlp == 1) write(66,*) '      Rseite(i)=',RSeite(i)%s
            if(ifehlp == 1)  then
                write(log_str, '(*(g0))') '      Rseite(i)=',RSeite(i)%s
                call logger(66, log_str)
            end if
!             if(ifehlp == 1) write(66,*) '      RseiteG=',TRIM(RSeiteg)
            if(ifehlp == 1)  then
                write(log_str, '(*(g0))') '      RseiteG=',TRIM(RSeiteg)
                call logger(66, log_str)
            end if
            if(ifehlp == 1) ifehlps = 1
            if(ifehlp == 1) return

        end do
        ifehlp = ifehlps

        call WDPutSelRadioMenu('QThird', kEGr)
        call SetMenuEGr(knumEGr)

!         write(66,'(a,i3,a,L1,a,i0)') 'Kfitcal=',kfitcal,'  FitCalCurve=',FitCalCurve,' ngrs=',ngrs
        write(log_str, '(a,i3,a,L1,a,i0)') 'Kfitcal=',kfitcal,'  FitCalCurve=',FitCalCurve,' ngrs=',ngrs
        call logger(66, log_str)

9000    continue

        if(apply_units) call load_unit_conv(ngrs+ncov)

        deallocate(oformel,bformel,oformel_rein,bformel_rein,ivpos,ivlen)
        if(allocated(str1)) deallocate(str1)
        if(allocated(ch2)) deallocate(ch2)
        if(allocated(RSeiteG)) deallocate(RSeiteG)

        if(ngrs > 0 .and. nab > 0) then
            call gtk_widget_set_sensitive(idpt('TBSaveProject'),1_c_int)
            call gtk_widget_set_sensitive(idpt('TBSaveProjectAs'),1_c_int)
            call gtk_widget_set_sensitive(idpt('MenuSaveProject'),1_c_int)
            call gtk_widget_set_sensitive(idpt('MenuSaveProjectAs'),1_c_int)
        end if

!         write(66,*) '########## End of Symbol1  ##############################'
        call logger(66, '########## End of Symbol1  ##############################')
        if(consoleout_gtk)  write(0,*) '##### End of Symbol1  ##############################'

    end subroutine Symbol1

!######################################################################

    module subroutine PointNach(mfall)

        ! if, by whatever reason, the symbol list has been modified, these
        ! changes have to be considered also in the table of covariances, what
        ! is done by this routine.
        ! It is called in Symbol1 before the syntax test with the function parser
        ! is executed. It may also be called from Rechw1.
        !
        !     Copyright (C) 2014-2024  Günter Kanisch

        use UR_Gleich_globals
        use UR_Linft
        use UR_Gspk1Fit
        use CHF,                only: FindlocT,ucase
        use, intrinsic :: iso_c_binding ,     only: c_null_char
        use gtk_hl_dialog
        use Rout,               only: MessageShow
        use gtk,                only: GTK_MESSAGE_WARNING,gtk_widget_set_sensitive
        use Rout,               only: MessageShow
        use UR_perror
        use Top,                only: idpt,IntModA1,CharModA1,CharModStr
        use CHF,                only: testSymbol
        use file_io,           only: logger
        use translation_module, only: T => get_translation

        implicit none

        integer, intent(in)    :: mfall    ! 1: called from Symbol1;   2: called from Rechw1

        integer :: i,k,j,resp,ncov0,ix,imax, nfd, ii
        character(len=512)           :: log_str
        character(len=:),allocatable   :: str1
        !-----------------------------------------------------------------------
        ! If necessary, readjust the symbol indexes in the covariance grid,
        imax = 0  ! 2025.01.24 GK
        !---
        if(ncov > 0) then
            if(allocated(isymbA)) then
                imax = ubound(isymbA,dim=1)
                do i=imax,1,-1
                    if(isymbA(i) > 0) then
                        imax = i
                        exit
                    end if
                end do
            end if
!             if(allocated(ISymbA)) write(66,'(a,50i3)') 'before:  IsymbA = ',(isymbA(i),i=1,imax)
            if(allocated(ISymbA))  then
                write(log_str, '(a,50i3)') 'before:  IsymbA = ',(isymbA(i),i=1,imax)
                call logger(66, log_str)
            end if
!             if(allocated(ISymbB)) write(66,'(a,50i3)') 'before:  IsymbB = ',(isymbB(i),i=1,imax)
            if(allocated(ISymbB))  then
                write(log_str, '(a,50i3)') 'before:  IsymbB = ',(isymbB(i),i=1,imax)
                call logger(66, log_str)
            end if
        end if
        ncov0 = 0

        allocate(character(len=800) :: str1)

        if(mfall <= 2) then
            if(.not.Gamspk1_Fit) then
                do k=1,ncov
                    if(IsymbA(k) > 0) then
                        i = FindlocT(symbole,Symbole(IsymbA(k))%s)
                        if(i > 0) IsymbA(k) = i
                    end if
                    if(IsymbB(k) > 0) then
                        i = FindlocT(symbole,Symbole(IsymbB(k))%s)
                        if(i > 0) IsymbB(k) = i
                    end if
                    if(IsymbA(k) > 0 .and. IsymbB(k) > 0) ncov0 = ncov0 + 1
                end do
            end if

            if(Gamspk1_Fit) then
                do k=1,ncov
                    if(IsymbA(k) > 0) then
                        i = FindlocT(symbole,Symbole(IsymbA(k))%s)
                        if(i > 0) IsymbA(k) = i
                    end if
                    if(IsymbB(k) > 0) then
                        i = FindlocT(symbole,Symbole(IsymbB(k))%s)
                        if(i > 0) IsymbB(k) = i
                    end if
                    if(IsymbA(k) > 0 .and. IsymbB(k) > 0) ncov0 = ncov0 + 1
                end do
            end if
        end if
        ! write(66,'(a,100(a,1x))') 'PN: nachher: IsymbA = ',(isymbA(i),i=1,size(isymbA))
        ! write(66,'(a,100(a,1x))') 'PN: nachher: IsymbB = ',(isymbB(i),i=1,size(isymbB))

        if(ncov > 0) THEN
            if(.true.) then
!                 write(66,'(4(a,i0))') 'PointNach: Finding the covar symbol numbers:  ngrsP=',ngrsP,'  ncov=',ncov,'  ngrs=',ngrs,' numd=',numd
                write(log_str, '(4(a,i0))') 'PointNach: Finding the covar symbol numbers:  ngrsP=',ngrsP,'  ncov=',ncov,'  ngrs=',ngrs,' numd=',numd
                call logger(66, log_str)
!                 if(allocated(ISymbA)) write(66,'(a,120i4)') '    IsymbA=',(IsymbA(i),i=1,ncov)
                if(allocated(ISymbA))  then
                    write(log_str, '(a,120i4)') '    IsymbA=',(IsymbA(i),i=1,ncov)
                    call logger(66, log_str)
                end if
!                 if(allocated(ISymbB)) write(66,'(a,120i4)') '    IsymbB=',(IsymbB(i),i=1,ncov)
                if(allocated(ISymbB))  then
                    write(log_str, '(a,120i4)') '    IsymbB=',(IsymbB(i),i=1,ncov)
                    call logger(66, log_str)
                end if
            end if

            if(mfall == 2) THEN
                do k=1,ncov
                    if(IsymbA(k) > 1) then
                        if(Symbole(IsymbA(k))%s /= Symbole_CP(IsymbA(k))%s ) THEN
                            i = findlocT(Symbole,Symbole_CP(IsymbA(k))%s)
                            if(i > 0) IsymbA(k) = i
                        end if
                    end if
                    if(IsymbB(k) > 1) then
                        if(Symbole(IsymbB(k))%s /= Symbole_CP(IsymbB(k))%s ) THEN
                            i = findlocT(Symbole,Symbole_CP(IsymbB(k))%s)
                            if(i > 0) IsymbB(k) = i
                        end if
                    end if
                end do
            end if
!             write(66,'(a,i0,4(a,i0))') 'PointNach: After finding the covar symbol numbers:  ngrsP=',ngrsP,'  ncov=',ncov,'  ngrs=',ngrs,' numd=',numd
            write(log_str, '(a,i0,4(a,i0))') 'PointNach: After finding the covar symbol numbers:  ngrsP=',ngrsP,'  ncov=',ncov,'  ngrs=',ngrs,' numd=',numd
            call logger(66, log_str)
!             if(allocated(ISymbA)) write(66,'(a,120i4)') '    IsymbA=',(IsymbA(i),i=1,ncov)
            if(allocated(ISymbA))  then
                write(log_str, '(a,120i4)') '    IsymbA=',(IsymbA(i),i=1,ncov)
                call logger(66, log_str)
            end if
!             if(allocated(ISymbB)) write(66,'(a,120i4)') '    IsymbB=',(IsymbB(i),i=1,ncov)
            if(allocated(ISymbB))  then
                write(log_str, '(a,120i4)') '    IsymbB=',(IsymbB(i),i=1,ncov)
                call logger(66, log_str)
            end if
        end if
!------------------------------------

        ix = ubound(SymboleG,dim=1)
        if(ngrs > ix) call CharModA1(SymboleG,ngrs)
        ix = ubound(Symbole,dim=1)
        if(ngrs > ix) call CharModA1(Symbole,ngrs)
        if(ngrs > ubound(symtyp,dim=1)) call CharModA1(symtyp,ngrs)
        if(ngrs > ubound(einheit,dim=1)) call CharModA1(einheit,ngrs)
        if(ngrs > ubound(bedeutung,dim=1)) call CharModA1(bedeutung,ngrs)

        do i=1,ngrs
            ! write(66,*) messwert(i),ivtl(i),TRIM(SDFormel(i)%s),SDwert(i),HBreite(i),IAR(i)
            SymboleG(i)%s = ucase(Symbole(i)%s)
            if(SymboleG(i)%s == 'FITP1') kfitp(1) = i
            ! write(66,*) 'i=',i,' ',symbole(i)
        end do

!kpointKB = 0
! k_rbl = 0
! if(FitDecay .and. klinf > 0) THEN
        if(FitDecay .and. klinf > 0 .and. .not.nhp_defined) THEN   ! 13.7.2023
            if(allocated(kpoint)) deallocate(kpoint)
            allocate(kpoint(nRSsy(klinf)))
            kpoint = 0
            nkpmax = 0
            k_rbl = 0  ! 13.7.2023
            do k=1,nRSsy(klinf)    ! number of argument symbols of the UR function LINFIT
                do i=1,ngrs
                    ! The array kpoint shall point from the argument symbols of Linfit to the
                    ! index numbers in the symbol list:
                    if(RSSy(nRssyanf(klinf)+k-1)%s == SymboleG(i)%s) THEN
                        kpoint(k) = i
                        nkpmax = nkpmax + 1
                        if(SymboleG(i)%s == 'RBL') k_rbl = k
                        if(SymboleG(i)%s == 'TMESS') k_tmess = k
                        if(SymboleG(i)%s == 'TSTART') k_tstart = k
                        exit           ! 1.2.2024
                    end if
                end do
            end do

            do j=nab+1,nab+nmodf         ! this new loop: 4.7.2023
                ! write(66,*) 'Eq. j=',int(j,2),' RS-Symbole: ',(RSSy(nRssyanf(j)+k-1)%s,' ',k=1,NRSsy(j))
                do k=1,nRSsy(j)
                    do i=kfitp(1)+1,ngrs
                        if(RSSy(nRssyanf(j)+k-1)%s == SymboleG(i)%s) THEN
                            nfd = 0
                            do ii=1,nkpmax
                                if(kpoint(ii) == i) then
                                    nfd = 1
                                    exit
                                end if
                            end do
                            if(nfd == 0) then
                                ! Check in RW1: if StdUnc(kpoint()) is <=  0, delete this kpoint()
                                nkpmax = nkpmax + 1
                                call IntModA1(kpoint,nkpmax)
                                kpoint(nkpmax) = i
                            end if
                        end if
                    end do
                end do
            end do
            linfit_rename = .false.
            if(k_tmess == 0 .or. k_tstart == 0) then
                call CharModStr(str1, 1024)
                write(str1, *)             &
                    T('The symbol names'), new_line('A'), new_line('A'), &
                    "  " // T('tmess: placeholder for single count times, and/or'), &
                    new_line('A'), &
                    "  " // T('tstart: placeholder for start times of the countings,'), &
                    new_line('A'), new_line('A'), &
                    T('are missing in the LINFIT-call! They must not be replaced!'), &
                    new_line('A'), new_line('A'), &
                    T('Please, correct the corresponding equation!')
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "PointNach:", resp,mtype=GTK_MESSAGE_WARNING)
                linfit_rename = .true.
                linfit_eqold = formelt(klinf)%s
                call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                ifehl = 1
                return
            end if

!             write(66,'(a,50i3)') 'PointN: LinFit:  kpoint(1-nkpmax)=',(kpoint(i),i=1,nkpmax)
            write(log_str, '(a,50i3)') 'PointN: LinFit:  kpoint(1-nkpmax)=',(kpoint(i),i=1,nkpmax)
            call logger(66, log_str)
        end if

        if(kgspk1 > 0) then
            if(allocated(kpoint)) deallocate(kpoint)
            allocate(kpoint(2))
            do k=1,2
                do i=1,ngrs
                    ! The array kpoint shall point from the argument symbols of Linfit to the
                    ! index numbers in the symbol list:
                    if(.not.defined_RSY) then
                        if(RSSy(nRSSyanf(kgspk1)+k-1)%s == SymboleG(i)%s) kpoint(k) = i
                    else
                        if(RS_SymbolNr(kgspk1,k) == i) kpoint(k) = i
                    end if
                end do
            end do

            if(kpoint(2) > 0) k_tlive = 2
            gamspk_rename = .false.
            if(kpoint(2) > 0) &
!                 write(66,'(a,i3,a,es15.8)') 'PointN: Gamspk1: kpoint(2)=',kpoint(2), &
!                                             '  Wert=',Messwert(kpoint(2))
                write(log_str, '(a,i3,a,es15.8)') 'PointN: Gamspk1: kpoint(2)=',kpoint(2), &
                                            '  Wert=',Messwert(kpoint(2))
                call logger(66, log_str)
            if(k_tlive == 0) then
                call CharModStr(str1, 1024)
                write(str1, *) &
                    T('The symbol name'), &
                    new_line('A'), new_line('A'), &
                    "  " // T('tlive: placeholder for single count time'), &
                    new_line('A'), new_line('A'), &
                    T('is missing in the GAMSPK1-call! It must not be replaced!'), &
                    new_line('A'), new_line('A'), &
                    T('Please, correct the corresponding equation!')

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "PointNach:", &
                                     resp, mtype=GTK_MESSAGE_WARNING)
                gamspk_rename = .true.
                ifehl = 1
                call gtk_widget_set_sensitive(idpt('treeview1'),0_c_int)
                call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'),0_c_int)
                call gtk_widget_set_sensitive(idpt('AcceptAll'),0_c_int)
                return
            end if

        end if

        if(kfitcal > 0) THEN
            if(allocated(kpointKB)) deallocate(kpointKB)
            allocate(kpointKB(2)); kpointKB = 0
            ! Number of arguments of the function KALFIT
            do k=1,nRSsy(kfitcal)
                do i=1,ngrs
                    ! The array kpointKB shall point from the argument symbols of KALFIT to the
                    ! index numbers in the symbol list:
                    if(.not.defined_RSY) then
                        if(RSSy(nRssyanf(kfitcal)+k-1)%s == SymboleG(i)%s) kpointKB(k) = i
                    else
                        if(RS_SymbolNr(kfitcal,k) == i) kpointKB(k) = i
                    end if
!                     write(66,'(a,i0,a,a,2(a,i0))') 'PointN:  i=',i,'  SymboleG(i)=',symboleG(i)%s,' k=',k,  &
!                         ' kpointKB(k)=',kpointKB(k)
                    write(log_str, '(a,i0,a,a,2(a,i0))') 'PointN:  i=',i,'  SymboleG(i)=',symboleG(i)%s,' k=',k,  &
                        ' kpointKB(k)=',kpointKB(k)
                    call logger(66, log_str)
                end do
            end do
        end if

!         write(66,*) '########### End PointNach   ######################'
        call logger(66, '########### End PointNach   ######################')

    end subroutine PointNach

!#######################################################################

    module subroutine Readj_knetto()

        ! in case of modifications in the symbol list, the integer variable
        ! knetto(kEGr) has also to be modified. This is done by using the
        ! variable name (knetto_name(kEGr)) in the modified symbol table.
        ! If this name is found withe index i in that table, knetto(kEGr)
        ! is re-adjusted to the value i.

        !     Copyright (C) 2014-2024  Günter Kanisch

        use UR_Gleich_globals,      only: knetto,knetto_name,kEGr,Symbole
        use CHF,            only: FindLocT
        use file_io,           only: logger
        use ur_general_globals,   only: kModelType

        implicit none

        character(len=512)           :: log_str
        integer        :: i

        if(kModelType == 2) return

        if(knetto(kEGr) > 0) then
            if(len_trim(knetto_name(kEGr)%s) > 0) then
                if(Symbole(knetto(kEGr))%s /= knetto_name(kEGr)%s) then
                    !write(66,*) 're-adjust knetto:',int(knetto(kEGr),2),knetto_name(kEGr)%s,' ', &
                    !                                                 Symbole(knetto(kEGr))%s
                    i = findlocT(Symbole,knetto_name(kEGr)%s)
                    if(i > 0) knetto(kEGr) = i
!                     write(66,*) 're-adjust knetto:',int(knetto(kEGr),2),knetto_name(kEGr)%s,' ', &
!                         Symbole(knetto(kEGr))%s
                    write(log_str, '(*(g0))') 're-adjust knetto:',int(knetto(kEGr),2),knetto_name(kEGr)%s,' ', &
                        Symbole(knetto(kEGr))%s
                    call logger(66, log_str)
                end if
            end if
        end if

    end subroutine Readj_knetto

!#######################################################################

    module subroutine Readj_kbrutto()

        ! in case of modifications in the symbol list, the integer variable
        ! kbrutto(kEGr) has also to be modified. This is done by using the
        ! variable name (kbrutto_name(kEGr)) in the modified symbol table.
        ! If this name is found withe index i in that table, kbrutto(kEGr)
        ! is re-adjusted to the value i.

        use UR_Gleich_globals,      only: kbrutto,kbrutto_name,kEGr,Symbole
        use CHF,            only: FindLocT
        use file_io,           only: logger
        use ur_general_globals,   only: kModelType

        implicit none

        character(len=512)           :: log_str
        integer        :: i

        if(kModelType == 2) return

        if(kbrutto(kEGr) > 0) then
            if(len_trim(kbrutto_name(kEGr)%s) > 0) then
                if(Symbole(kbrutto(kEGr))%s /= kbrutto_name(kEGr)%s) then
                    !write(66,*) 're-adjust kbrutto:',int(kbrutto(kEGr),2),kbrutto_name(kEGr)%s,' ', &
                    !                                                     Symbole(kbrutto(kEGr))%s
                    i = findlocT(Symbole,kbrutto_name(kEGr)%s)
                    if(i > 0) kbrutto(kEGr) = i
!                     write(66,*) 're-adjust kbrutto:',int(kbrutto(kEGr),2),kbrutto_name(kEGr)%s,' ', &
!                         Symbole(kbrutto(kEGr))%s,' i=',int(i,2)
                    write(log_str, '(*(g0))') 're-adjust kbrutto:',int(kbrutto(kEGr),2),kbrutto_name(kEGr)%s,' ', &
                        Symbole(kbrutto(kEGr))%s,' i=',int(i,2)
                    call logger(66, log_str)
                end if
            end if
        end if

    end subroutine Readj_kbrutto

!#######################################################################


    module subroutine RS_numbers

        ! for each equation, or better, each right-hand-side of an equation,
        ! a matrix RS_SymbolNr(i,k) = j is constructed. The number i denotes
        ! the equation number and k the symbol list related index of a symbol
        ! occurring in the right-hand-side formuala. the index j then refers to
        ! the index of occurrence within the right-ghand-side formula, when
        ! counting the right-hand-side symbols from left to right beginning
        ! with 1 for the leftmost symbol.
        !
        ! During this process, also the position index numbers of numerical
        ! operators (+ - * / ^) are stored in the matrices
        !   RS_ops(i,j) = '/'  : the operator character
        !   RS_opsPos(i,j) = k : the position of the operator character within
        !                        the string RSeite()
        !
        !     Copyright (C) 2014-2024  Günter Kanisch

        use UR_Gleich_globals,   only: RS_SymbolNr,RS_ops,RS_opsPos,nab,SymboleG,RSsy,nRSsy, &
                               nRssyanf,RSeite,kEGr,knetto,defined_RSY,RS_SymbUse, &
                               nmodf
        use UR_Linft,    only: FitDecay
        use UR_Gspk1Fit, only: Gamspk1_Fit
        use file_io,           only: logger
        use CHF,         only: FindlocT

        implicit none

        character(len=512)           :: log_str
        integer            :: i, j, k, itwo, jj

        if(allocated(RS_SymbolNr)) deallocate(RS_SymbolNr)
        allocate(RS_SymbolNr(nab+nmodf,maxval(nRSSy)))

        if(allocated(RS_SymbUse)) deallocate(RS_SymbUse)
        allocate(RS_SymbUse(nab+nmodf,maxval(nRSSy)))

        RS_SymbolNr = 0
        RS_SymbUse = .true.
        if(allocated(RS_ops)) deallocate(RS_ops)
        allocate(RS_ops(nab+nmodf,maxval(nRSSy)-1))
        RS_ops = ' '
        if(allocated(RS_opsPos)) deallocate(RS_opsPos)
        allocate(RS_opsPos(nab+nmodf,maxval(nRSSy)-1))
        RS_opsPos = 0

        do i=1,nab+nmodf
            do k=1,nRSsy(i)
                j = FindlocT(SymboleG,RSSy(nRssyanf(i)+k-1)%s)
                if(j > 0) RS_SymbolNr(i,k) = j
                ! write(66,*) 'RSN: i=',int(i,2),' k=',int(k,2),' :   nRssyanf(i)=',nRssyanf(i), &
                !                       ' RSSy(nRssyanf(i)+k-1)%s=',RSSy(nRssyanf(i)+k-1)%s
            end do
            if(.not.FitDecay .and. .not.Gamspk1_Fit .and. kEGr > 0) then
                if(i == knetto(kEGr)) then
                    do k=1,nRSsy(i)
                        j = FindlocT(SymboleG,RSSy(nRssyanf(i)+k-1)%s)
                        if(j > 0) RS_SymbolNr(i,k) = j
                    end do
                end if
                if(i >= knetto(kEGr)) then
                    do
                        itwo = index(Rseite(i)%s,'**')
                        if(itwo == 0) exit
                        RSeite(i)%s(itwo:itwo) = '^'
                        RSeite(i)%s(itwo+1:) = RSeite(i)%s(itwo+2:)
                    end do
                    j = 0
                    do k=1,len_trim(Rseite(i)%s)
                        if(Rseite(i)%s(k:k) == '+' .or. Rseite(i)%s(k:k) == '-' .or.   &
                            Rseite(i)%s(k:k) == '*' .or. Rseite(i)%s(k:k) == '/' .or. Rseite(i)%s(k:k) == '^') then
                            j = j + 1
                            if(j < nRSsy(i)) then
                                RS_ops(i,j) = Rseite(i)%s(k:k)
                                RS_opsPos(i,j) = k
                            end if
                        end if
                    end do
                end if
            end if
!             if(.false. .and. .not.defined_RSY .and. kEGr == 1) write(66,*) 'RS_numbers: Eq. i=',int(i,2),' RS-Symbole: ', &
!                 (SymboleG(RS_SymbolNr(i,jj))%s,' ',jj=1,nRSsy(i))           ! 19.6.2024  prevent from output
            if(.false. .and. .not.defined_RSY .and. kEGr == 1)  then
                write(log_str, '(*(g0))') 'RS_numbers: Eq. i=',int(i,2),' RS-Symbole: ', &
                (SymboleG(RS_SymbolNr(i,jj))%s,' ',jj=1,nRSsy(i))           ! 19.6.2024  prevent from output
                call logger(66, log_str)
            end if
        end do
        defined_RSY = .true.

    end subroutine RS_numbers

    !####################################################################################
end submodule Sym1A
