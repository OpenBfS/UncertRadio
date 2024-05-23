
submodule (PMD) PMDA

    !    contains:
    ! ProcMainDiag
    ! GamSymList
    ! GamPeakvals
    ! AdjustRemoveTVRows

contains

    module recursive subroutine ProcMainDiag(ncitem)

    !   processing user actions in the graphical user interface
    !   called by Procmenu, ProcessLoadPro_new, UR_NBPage_switched_cb

    !     Copyright (C) 2014-2023  Günter Kanisch

    use, intrinsic :: iso_c_binding
    use gtk_hl,            only: gtk_widget_set_sensitive, &
        hl_gtk_listn_get_selections, &
        hl_gtk_about_dialog_show, hl_gtk_about_dialog_gtk_fortran
    use gtk,               only: GTK_BUTTONS_OK,gtk_widget_hide,GTK_LICENSE_GPL_3_0,gtk_widget_set_visible, &
        GTK_MESSAGE_WARNING,gtk_widget_show,GTK_STATE_FLAG_NORMAL, &
        gtk_widget_show_all,gtk_widget_set_size_request,gtk_widget_set_vexpand, &
        gtk_widget_set_vexpand_set,gtk_notebook_get_current_page, &
        gtk_get_major_version,gtk_get_minor_version,gtk_get_micro_version, &
        gtk_window_get_position,gtk_tree_view_columns_autosize, &
        gtk_tree_view_column_get_width,gtk_container_check_resize, &
        GTK_LICENSE_CUSTOM, GTK_MESSAGE_INFO, &
        gtk_widget_destroy, gtk_main_quit

    use gdk_pixbuf_hl,     only: hl_gdk_pixbuf_new_file
    use pango,             only: pango_renderer_set_color, pango_renderer_get_color
    use gui_functions,     only: lowcase

    use UR_variables,      only: charv,actual_grid,actual_plot,automode,autoreport,bat_serial,batest_on, &
        batf,Confidoid_activated,bottom_selrow,frmtres, &
        Gum_restricted,kModelType,langg,MCsim_on,multi_eval,plot_confidoid, &
        plot_ellipse,project_loadw,proStartNew,SaveP,top_selrow, &
        work_path,irowtab,batest_user,frmtres_min1,simul_ProSetup, &
        FileTyp,sDecimalPoint, dir_sep, UR_version_tag, UR_git_hash
    use UR_gtk_variables,  only: clobj,dialogstr,ioption,consoleout_gtk,posx,posy, &
        rootx,rooty,QuitProg,ntvs,tvnames,tv_colwidth_digits,winPL_shown, &
        tvcolindex,tvcols,nbook2,green_bg,orange_bg, &
        table_bg

    use plplot_code_sub1,  only: windowPL,width_da,height_da,drawing,hl_gtk_drawing_area_resize
    use UR_Gleich,         only: Symbole,symbole_CP,symtyp,symtyp_CP,einheit,einheit_CP,bedeutung, &
        bedeutung_CP,Messwert,Messwert_CP,IVTL,ivtl_CP,SDFormel,SDFormel_CP, &
        SDWert,SDWert_CP,HBreite,HBreite_CP,STDUnc,STDUnc_CP,ngrs,nab, &
        Formeltext,knetto_name,knetto,kbrutto,symboleG,CVFormel,CVFormel_CP, &
        IAR,IAR_CP,kbrutto_name,MDused,MDpoint,MDpointrev,Sensi,perc,sensi_CP, &
        perc_CP,meanID,nvarsMD,SymboleA,SymboleB,Titeltext,apply_units,corrval, &
        CovarVal_CP,coverf,icovtyp,icovtyp_CP,ifehl,ilam_binom,ip_binom,k_datvar, &
        kEGr,IsymbA_CP,IsymbB_CP,itm_binom,kbgv_binom,kgspk1,klinf,knumEGr, &
        loadingPro,missingval,n_preset,nabf,ncov,ngrsP,nmu,nsyd,nsydanf,nvalsMD, &
        percsum,refresh_type,Resultat,symlist_shorter,tab_gleichg_2rates, &
        tab_gleichg_grid,ucomb,ucontrib,nrssy,iptr_cnt,IsymbA,IsymbB,covarval, &
        meanMD,umeanMD,ngrs_CP,tab_valunc_grid,ucontyp,uncval_exists,use_dp, &
        RSsy,nRSsyanf,knetto_CP,kbrutto_CP,xdataMD,RS_SymbolNr, &
        ixdanf,use_bipoi,ncovf,nparts,ksumEval,iavar,avar,maxlen_symb, &
        einheit,einheit_conv,Messwert,HBreite,SDWert,nab,StdUnc, &
        IAR,unit_conv_fact,fp_for_units,apply_units_dir,uconv,grossfail, &
        ngrs_init,retain_triggers
    use UR_Linft,          only: FitDecay,corrEGR,chisqr,ChisqrLfit,fitmeth,klincall,numd,ifit, &
        posdef,SumEval_fit,UcombLfit,UcombLinf_kqt1,uncEGr,use_WTLS,valEGr,kfitp, &
        nhp_defined
    use UR_Gspk1Fit,       only: Gamspk1_Fit,gspk_chisqr,mwtyp,ecorruse,detlim_approximate
    use UR_perror,         only: ifehlp,nsymbnew
    use UR_Loadsel,        only: nbcurrentpage,NBpreviousPage
    use UR_DLIM,           only: detlim,decthresh,iteration_on,kalpha,kbeta,kbgrenzo,kbgrenzu,kbgrenzosh, &
        kbgrenzuSH,nit_decl,nit_detl,nwgmeth,UcombBayes,var_brutto_auto,W1minusG, &
        WertBayes,nit_detl_max
    use UR_MCC,            only: nmumx,iopt_copygr,ncovmx
    use top,               only: FindItemS,idpt,WrStatusbar,FieldUpdate,CharModA1,IntModA1, &
        RealModA1,LogModA1,InitVarsTV2_CP,InitVarsTV8,load_unit_conv
    use Rout,              only: MessageShow,WTreeViewGetStrArray,WTreeViewGetDoubleArray,  &
        WTreeViewGetComboArray,WDGetTextviewString,Fopen, &
        WTreeViewScrollRow,WTreeViewSetCursorCell,           &
        WTreeViewPutStrArray,WTreeViewPutComboArray,WTreeViewPutDoubleCell, &
        WDSetComboboxAct,WDPutLabelString,WTreeViewRemoveRow,        &
        WDPutEntryString,NBlabelmodify,WDNotebookSetCurrPage,pending_events, &
        WDGetComboboxAct,WTreeViewPutDoubleArray,WDPutTreeViewColumnLabel,  &
        WDSetCheckButton,NumRowsTV,WDListstoreClearCell,WDListstoreFill_1, &
        WDPutLabelStringBold,WDPutEntryDouble,WDPutLabelColorF, &
        WTreeViewPutIntArray,WTreeViewPutStrCell,WTreeViewSetColorCell, &
        WTreeViewPutComboCell,WDPutSelRadio,WTreeViewSetColorRow, &
        ClearMCfields,EraseNWGfields,WTreeViewGetStrCell,WDGetCheckButton, &
        ExpandTV2Col7,WDPutEntryInt,WTreeViewGetDoubleCell, &
        WDPutTextviewString
! use gtk_draw_hl,         only: gtkallocation

    use Sym1,                only: Symbol1,Readj_kbrutto,Readj_knetto
    use Rw1,                 only: Rechw1,LinCalib
    use Rw2,                 only: Rechw2
    use Usub3,               only: SaveResults,TransToTV2
    use UWB,                 only: CorrectLists,corrmatEGr
    use LSTfillT,            only: WDListstoreFill_table
    use PLsubs,              only: Printplot,Replot
    use UR_interfaces,       only: DisplayHelp
    use CHF,                 only: FindLocT,FLTU,lowercase,ucase
    use LDN,                 only: Loadsel_diag_new
    use Celli,               only: PrepEli, Confidoid
    use RG,                  only: Read_Gleich, modify_Formeltext
    use UR_params,           only: eps1min,zero,one,rn
    use fparser,             only: initf,parsef
    use urInit,              only: TVtrimCol_width,ReadUnits
    use PSave,               only: ProSave

    implicit none

    integer(4),intent(in)            :: ncitem   ! index of widget in the list of clobj

    integer(4)              :: IDENT1
    integer(4)              :: IDENT2
    CHARACTER(LEN=500)      :: str1
    CHARACTER(LEN=3)        :: chint

    character(len=6)        :: chcol

    integer(4)              :: i,kxx,kxy,k2,k2m,j,k,k1,kgr,resp,kanz,ncitem2,k4
    integer(4)              :: klu, kmin,icp_used(nmumx),irow,kx,ncol,nrow,jj,ii
    integer(4)              :: iarray(nmumx),ngmax,kEGrSVE,nfd,nci,ns1,nvv,mmvv(6)
    integer(4)              :: ix,nt,ios,kk
    CHARACTER(LEN=60)       :: ckt,versgtk,cpos,cheader
    LOGICAL                 :: unit_ident , sfound,loadProV
    real(rn)                :: ucrel,pSV
    real(rn),allocatable    :: rdummy(:)
    real(4)                 :: stt1,stp1,t1,tend
    character(len=60)       :: idstring,signal,parent,name
    integer(kind=c_int), allocatable :: rownums_marked(:)
    integer(c_int)          :: numrows_marked,ic
    logical                 :: prout
    character(len=1)        :: CR = char(10)
    integer(kind=c_int)     :: sizewh(2)
    character(:),allocatable  :: xstr                    ! 12.8.2023
    type(charv),allocatable :: SDformel_test(:),FT1(:)
    type(c_ptr)             :: UR2Logo
    character(len=100)      :: cerror
    character(len=2000)     :: comment_str
    character(len=200)      :: url_str
    character(len=100)      :: authors(3)
! type(gtkallocation), target  :: alloc
!----------------------------------------------------------------------------

    UR2Logo = hl_gdk_pixbuf_new_file(work_path // 'icons' //dir_sep//'ur2_symbol.png'//c_null_char, &
        height=30_c_int,error=cerror)


    prout = .false.
! prout = .true.

    if(prout) write(66,*) '***** ProcMainDiag:   ncitem=',ncitem

    idstring = clobj%idd(ncitem)%s          ! id string of a widget in Glade, e.g., "AcceptAll"
    i = clobj%idparent(ncitem)              ! e.g.,  840  (index in the list over 1100 widgets))
    if(i > 0) parent = clobj%name(i)%s      ! "GtkWindow"
    signal = clobj%signal(ncitem)%s         ! "clicked"
    name = clobj%name(ncitem)%s             ! "GtkButton"

    if(prout) write(66,*) '***** ProcMainDiag:   name=',trim(name),'  idstring=', trim(idstring), &
        '  signal=',trim(signal),'  kEgr=',int(kEgr,2)
    if(consoleout_gtk) write(0,*) '##### PMD  Begin:  id=',trim(idstring),',  signal=',trim(signal)

    select case (trim(name))

      case ('GtkButton')

        select case (trim(idstring))

          case ('button_LoadSymbols')

            call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'), 1_c_int)

            ngrs_init = ngrs   ! 21.8.2023

            if(kEGr > 0) then
                knetto_CP(kEGr) = knetto(kEGr)
                kbrutto_CP(kEGr) = kbrutto(kEGr)
            end if
            ngmax = ngrs + 40
            ngmax = min(200,ngmax)
            nhp_defined = .false.

            !---------------
            ! Here, before working with an eventually modified symbol list, the *_CP arrays (in memory))
            ! will be synchronized with those arrays which are actually visible in the Val/unc-Grid.
            ! the synchronisation is done later, when reading the equations (Read_gleich) and
            ! extracting the symbols (Symbol1), both done further down!
            ! write(66,*) 'TAB_VALUNC_GRID=',TAB_VALUNC_Grid,'   loadingPro',loadingPro

            !kxx = 200
            kxx = NumRowsTV('treeview1')
            ! write(66,*) 'kxx aus NumRowsTV: ',int(kxx,2)
            if(allocated(Symbole_CP)) then
                deallocate(Symbole_CP,symtyp_CP,einheit_CP,Bedeutung_CP,Messwert_CP,IVTL_CP)
                deallocate(sdformel_CP,sdwert_CP,HBreite_CP,IAR_CP,StdUnc_CP)
                deallocate(Sensi_CP,perc_CP)
            end if
            call InitVarsTV2_CP(kxx)
            ! write(66,*) 'TAB_VALUNC_Grid=',TAB_VALUNC_Grid,'  loadingPro=',loadingPro
            if(TAB_VALUNC_Grid .or. loadingPro) then
                ! read the *_CP arrays:

                call WTreeViewGetStrArray('treeview1', 2, kxx, Symbole_CP)
                call WTreeViewGetStrArray('treeview1', 3, kxx, symtyp_CP)
                call WTreeViewGetStrArray('treeview1', 4, kxx, einheit_CP)

                call WTreeViewGetDoubleArray('treeview2', 5, kxx, Messwert_CP)
                call WTreeViewGetComboArray('treeview2', 6, kxx, IVTL_CP)
                call WTreeViewGetStrArray('treeview2', 7, kxx, sdformel_CP)
                call WTreeViewGetDoubleArray('treeview2', 8, kxx, SDWert_CP)
                call WTreeViewGetDoubleArray('treeview2', 9, kxx, HBreite_CP)
                call WTreeViewGetComboArray('treeview2', 10, kxx, IAR_CP)
                call WTreeViewGetDoubleArray('treeview2', 11, kxx, StdUnc_CP)

                do i=1,kxx
                    call WTreeViewGetStrCell('treeview1', 5, i, bedeutung_CP(i)%s)
                end do
            else
                call WTreeViewGetStrArray('treeview1', 2, kxx, Symbole_CP)
                call WTreeViewGetStrArray('treeview1', 3, kxx, symtyp_CP)
                call WTreeViewGetStrArray('treeview1', 4, kxx, einheit_CP)
                call WTreeViewGetStrArray('treeview1', 5, kxx, bedeutung_CP)
            end if
            do i=1,ubound(Messwert_CP,dim=1)
                if(i == ubound(Messwert_CP,dim=1)) then
                    ngrs_CP = i
                    exit
                end if
                if(abs(Messwert_CP(i) - missingval) < eps1min) then
                    ngrs_CP = i-1
                    exit
                end if
            end do
            ngrs_CP = ubound(Symbole,dim=1)
            if(.not.simul_ProSetup) then
                if(Gamspk1_Fit) then
                    call GamSymList
                    call GamPeakvals
                end if
            end if
            !Write(66,*) 'Symbol list _CP from treeview1: nafter LoadSymbols:  ngrs=',int(ngrs,2),'  ngrs_CP=',int(ngrs_CP,2)
            !do i=1,ngrs+numd+ncov       ! ngrs_CP
            !  write(66,'(i2,1x,a,1x,a,1x,a,1x,es10.3,1x,i2,1x,a,1x,2(es10.3,1x),1x,i1)') &
            !           i,Symbole_CP(i)%s,symtyp_CP(i)%s,einheit_CP(i)%s,MEsswert_CP(i),  &
            !            ivtl_CP(i),sdformel_CP(i)%s,sdwert_CP(i),hbreite_CP(i),IAR_CP(i)
            !end do

            if(Gamspk1_Fit) then
                call IntModA1(ivtl,ngrs+numd+ncov)
                call IntModA1(iar,ngrs+numd+ncov)
                call CharModA1(sdformel,ngrs+numd+ncov)
                call RealModA1(sdwert,ngrs+numd+ncov)
                call RealModA1(hbreite,ngrs+numd+ncov)
                do i=1,ngrs+numd+ncov       ! ngrs_CP
                    exit
                    write(66,'(i2,1x,a,1x,a,1x,a,1x,es10.3,1x,i2,1x,a,1x,2(es10.3,1x),1x,i1,1x,es10.3)') &
                        i,Symbole(i)%s,symtyp(i)%s,einheit(i)%s,MEsswert(i),  &
                        ivtl(i),sdformel(i)%s,sdwert(i),hbreite(i),IAR(i),StdUnc(i)
                end do
            end if

            !--    klinf_CP = klinf
            !--    kgspk1_CP = kgspk1
            ifehl = 0
            ifehlp = 0
            call WDGetTextviewString('textview2',Formeltext)

            IF(size(Formeltext) > 0) THEN
                !---------------------------------
                write(66,*) 'before Read_gleich:   size(Formeltext)=',size(Formeltext)
                call Read_Gleich()
                !---------------------------------
                if(prout) WRITE(66,'(a,i1,a,i1)') 'PMD: After call Read_Gleich: ifehl=',ifehl,'  ifehlp=',ifehlp
                IF(ifehl == 1) THEN
                    IF(langg == 'DE') str1 = 'Fehler in Gleichungen beheben!'
                    IF(langg == 'EN') str1 = 'Eliminate error(s) in equations!'
                    IF(langg == 'FR') str1 = 'Élimine les erreurs dans les équations!'
                    call WrStatusBar(4,trim(str1))
                    GOTO 9000
                END IF
                !---------------------------------
                call cpu_time(stt1)
                t1 = secnds(0.0)
                call Symbol1()
                call cpu_time(stp1)
                tend = secnds(t1)
                write(66,*) 'Symbol1: cpu-time (s) :',(stp1-stt1),'  secnds=',tend
                !---------------------------------
                if(prout) write(66,*) 'PMD:    after Symbol1:  ifehlp=',int(ifehlp,2)

                IF(ifehlP == 1 .OR. ifehl == 1) THEN
                    IF(langg == 'DE') str1 = 'Fehler in Gleichungen oder Symbolliste beheben!'
                    IF(langg == 'EN') str1 = 'Eliminate error(s) in equations or in the symbol list!'
                    IF(langg == 'FR') str1 = 'Éliminer les erreurs dans les équations ou dans la liste des symboles!'
                    call WrStatusBar(4, trim(str1))
                    GOTO 9000
                END IF
                if(.not.batest_on .and. .not.automode) then
                    if(consoleout_gtk) write(0,*) 'PMD 298, LoadCompletedSyms: before WDListstoreFill_1(liststore_symbols)'
                    !!      call WDListstoreFill_1('liststore_symbols', ngrs, Symbole)
                    call WDListstoreFill_1('liststore_symbols', min(ngrs+ncov+numd, ubound(Symbole,dim=1)), Symbole)
                    if(consoleout_gtk) write(0,*) 'PMD 301, LoadCompletedSyms: behind WDListstoreFill_1(liststore_symbols)'
                end if

                IF(SumEval_fit) THEN
                    knetto(kEGr) = ksumeval
                    call CharModA1(knetto_name,kEGr)
                    knetto_name(kEGr)%s = symbole(knetto(kEGr))%s
                    do j=1,nparts
                        iavar(j) = FindlocT(Symbole,avar(j))
                    end do
                    write(66,*) 'PMD: avar: ',(trim(avar(j)),' ',j=1,nparts),'  iavar: ',int(iavar(1:nparts),2)
                end if

                iarray(1:kxx) = [ (i,i=1,kxx) ]
                call WTreeViewPutIntArray('treeview1',1,kxx,iarray)

                if(symlist_shorter) call WTreeViewScrollRow('treeview1',max(1,nsydanf-1))
                if(nsyd > 0) then
                    call WTreeViewSetCursorCell('treeview1', 4, ngrs+1-nsyd)
                end if
                if(symlist_shorter) then
                    IF(langg == 'DE') str1 = 'Ggf. Zeilen löschen; '  &
                        // 'Symboltabelle ergänzen, dann Button "Laden Symbole(1) .."'
                    IF(langg == 'EN') str1 = 'Delete lines if necessary; '  &
                        // 'Complete symbol table, then Button "Load symbols(1) .."'
                    IF(langg == 'FR') str1 = 'Supprimer les lignes si nécessaire; '  &
                        // 'Terminer la table des symboles, puis Button "Charger les symboles (1) .."'
                    call WrStatusBar(4,trim(str1))
                else
                    IF(langg == 'DE') str1 =   &
                        'Symboltabelle ergänzen, dann Button "Laden Symbole(2) .."'
                    IF(langg == 'EN') str1 =   &
                        'Complete symbol table, then Button "Load symbols(2) .."'
                    IF(langg == 'FR') str1 =   &
                        'Terminer la table des symboles, puis Button "Charger les symboles (2) .."'
                    call WrStatusBar(4,trim(str1))
                end if

                if(.not.Gum_restricted) then
                    IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) then
                        if(knetto(kEGr) > 0) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
                        if(kbrutto(kEGr) > 0) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGr))
                    end if

                    if(ubound(knetto_name,dim=1) > 0) then
                        if(len_trim(knetto_name(kEGr)%s) == 0) then
                            if(knetto(kEGr) > 0) knetto_name(kEGr)%s = Symbole(knetto(kEGr))%s
                        else
                            if(knetto_name(kEGr)%s /= Symbole(knetto(kEGr))%s ) then
                                write(66,*) 'PMD_335:  knetto(kEGr)=',symbole(knetto(kEGR))%s,' Name=',knetto_name(kEGR)%s
                            end if
                        end if
                    end if
                    if(kbrutto(kEGr) > 0) then
                        if(len_trim(kbrutto_name(kEGr)%s) == 0) then
                            if(kbrutto(kEGr) > 0) kbrutto_name(kEGr)%s = Symbole(kbrutto(kEGr))%s
                        else
                            if(kbrutto_name(kEGr)%s /= Symbole(kbrutto(kEGr))%s ) then
                                write(66,*) 'PMD_344:  kbrutto(kEGr)=',symbole(kbrutto(kEGR))%s,' Name=',kbrutto_name(kEGR)%s
                            end if
                        end if
                    end if
                    !----------------
                else
                    knetto(kEgr) = 0
                    kbrutto(kEgr) = 0
                end if

                IF(.not.TAB_GLEICHG_Grid) TAB_GLEICHG_Grid = .TRUE.
                call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'), 1_c_int)
                call gtk_widget_set_sensitive(idpt('treeview1'), 1_c_int)

                if(nsyd > 0) then
                    call WTreeViewSetCursorCell('treeview1', 4, ngrs+1-nsyd)
                end if

            END IF

            IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) then
                call gtk_widget_set_sensitive(idpt('comboboxNetRate'), 1_c_int)
                call gtk_widget_set_sensitive(idpt('comboboxGrossRate'), 1_c_int)
            end if
            if(simul_ProSetup) then
                ! ifehl = 1
            end if

            if(.not.loadingPro) call pending_events
            ! write(66,*) 'PMD:  after Sy1:  loadingpro=',loadingPro,'  project_loadw=',project_loadw

            goto 9000

!++++++++++++++
          case ('LoadCompletedSyms')

            ! write(66,*) 'PMD_391: knetto=',int(knetto,2),'  kbrutto=',int(kbrutto,2)

            ! if(knumEGr > 1 .and. .not.FitDecay .and. .not. Gamspk1_Fit) then

            if(simul_ProSetup) then
                if(.not.Gum_restricted) then
                    ! write(0,*) 'knetto combobox:  knetto=',int(knetto,2),' kbrutto=',int(kbrutto,2)
                    IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) then
                        call gtk_widget_set_sensitive(idpt('comboboxNetRate'), 1_c_int)
                        call gtk_widget_set_sensitive(idpt('comboboxGrossRate'), 1_c_int)
                        if(knetto(kEGr) > 0) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
                        if(kbrutto(kEGr) > 0) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGr))
                    end if
                end if
            end if

            if(loadingPro .and. knumEGr > 1 .and. .not.FitDecay .and. .not. Gamspk1_Fit .and. .not.SumEval_fit) then
                ! for the case of changing the active output quantity, and knetto or kbrutto are not yet defined
                write(66,*) 'knetto=',int(knetto,2),'  kbrutto=',int(kbrutto,2),' refresh_type=',refresh_type
                do i=1,knumEGr
                    if(i /= kEGr) cycle
                    if(knetto(i) == 0) then
                        if(langg == 'DE') &
                            write(str1,'(a,i1,a)') 'Fehler:  Nettozählrate für EG(',i,') ist nicht selektiert!'
                        if(langg == 'EN') &
                            write(str1,'(a,i1,a)') 'Error:  net count rate for OQ(',i,') is not selected!'
                        if(langg == 'FR') &
                            write(str1,'(a,i1,a)') 'Erreur : le taux de calcul net pour OQ(',i,') n''est pas sélectionné !'
                        call MessageShow(trim(str1), GTK_BUTTONS_OK, "PMD:", j,mtype=GTK_MESSAGE_WARNING)
                        ifehl = 1
                        goto 9000         ! return
                    elseif(kbrutto(i) == 0) then
                        if(langg == 'DE') &
                            write(str1,'(a,i1,a)') 'Fehler:  Bruttozählrate für EG(',i,') ist nicht selektiert!'
                        if(langg == 'EN') &
                            write(str1,'(a,i1,a)') 'Error:  gross count rate for OQ(',i,') is not selected!'
                        if(langg == 'FR') &
                            write(str1,'(a,i1,a)') 'Erreur : le taux de comptage brut pour OQ(',i,') n''est pas sélectionné !'
                        call MessageShow(trim(str1), GTK_BUTTONS_OK, "Read_Gleich:", j,mtype=GTK_MESSAGE_WARNING)
                        ifehl = 1
                        goto 9000              ! return
                    end if
                end do
            end if
            !write(66,*) 'PMD_432: knetto=',int(knetto,2),'  kbrutto=',int(kbrutto,2)

            nhp_defined = .false.

            call gtk_widget_set_sensitive(idpt('AcceptAll'), 1_c_int)
            proStartNew = .false.
            !!    write(0,*) 'before TTG: ubound(IVTL,Messwert)=',ubound(IVTL,dim=1),ubound(Messwert,dim=1)
            !---------------
            call TransToTV2()              ! part of the file Uncw_sub3
            !----------------------------------

            IF(.not.TAB_GLEICHG_2Rates) THEN
                TAB_GLEICHG_2Rates = .TRUE.
                if(.not.Gum_restricted) then
                    IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) call gtk_widget_set_sensitive(idpt('comboboxNetRate'), 1_c_int)
                    IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) call gtk_widget_set_sensitive(idpt('comboboxGrossRate'), 1_c_int)
                    call gtk_widget_set_sensitive(idpt('AcceptAll'), 1_c_int)
                end if
            END IF
15          CONTINUE
            kxx = NumRowsTV('treeview1')

            kxx = ubound(Symbole,dim=1) +50
            do i=1,kxx
                if(i > ubound(Symbole,dim=1)) then
                    call CharModA1(Symbole,i)
                    call WTreeViewGetStrCell('treeview1', 2, i, Symbole(i)%s)
                    if(len_trim(Symbole(i)%s) == 0) then
                        ngrs = i -1
                        call CharModA1(Symbole,i-1)
                        exit
                    else
                        call CharModA1(symtyp,i)
                        call CharModA1(einheit,i)
                        call CharModA1(bedeutung,i)
                        call WTreeViewGetStrCell('treeview1', 3, i, symtyp(i)%s)
                        call WTreeViewGetStrCell('treeview1', 4, i, einheit(i)%s)
                        call WTreeViewGetStrCell('treeview1', 5, i, bedeutung(i)%s)
                    end if
                else
                    if(i > ubound(Einheit,dim=1)) call CharModA1(Einheit,i)
                    if(i > ubound(symtyp,dim=1)) call CharModA1(symtyp,i)
                    if(i > ubound(bedeutung,dim=1)) call CharModA1(bedeutung,i)

                    call WTreeViewGetStrCell('treeview1', 2, i, Symbole(i)%s)
                    call WTreeViewGetStrCell('treeview1', 3, i, symtyp(i)%s)
                    call WTreeViewGetStrCell('treeview1', 4, i, einheit(i)%s)
                    call WTreeViewGetStrCell('treeview1', 5, i, bedeutung(i)%s)
                end if
            end do

            ngrs = 0
            do i=1,kxx+50
                if(i > ubound(Symbole,dim=1)) then
                    ngrs = i - 1
                    exit
                end if
                IF(len_TRIM(symbole(i)%s) == 0) THEN
                    IF(ngrs == 0) THEN
                        ngrs = i - 1
                        exit
                    else
                        CONTINUE
                    END IF
                END IF
            end do

            do i=1,ngrs
                if(symbole(i)%s(1:1) == ' ') symbole(i)%s   = ADJUSTL(symbole(i)%s)
                symtyp(i)%s    = ADJUSTL(symtyp(i)%s)
                einheit(i)%s   = ADJUSTL(einheit(i)%s)
                if(bedeutung(i)%s(1:1) == ' ') bedeutung(i)%s = ADJUSTL(bedeutung(i)%s)
            end do
            ! Check for double foud symbols:
            k2m = 0
            do i=1,ngrs
                do k2=i+1,ngrs
                    IF(TRIM(lowercase(symbole(i)%s)) == TRIM(lowercase(symbole(k2)%s)) ) THEN
                        IF(langg == 'DE') WRITE(str1,*) 'Das ',k2,'.te Symbol  ',symbole(k2)%s, &
                            '  ist schon vorhandent!', CHAR(13),             &
                            'Bitte die ',k2,'.te Zeile entfernen!',CHAR(13), &
                            '(Cursor in diese Zeile; benutze dann den Tool-Button zum Löschen von Zeilen)'
                        IF(langg == 'EN') WRITE(str1,*) 'The ',k2,'.th symbol  ',symbole(k2)%s, &
                            '  is already defined!', CHAR(13),             &
                            'Please, delete(remove) the ',k2,'.th row!',CHAR(13), &
                            '(put cursor into this row; use then the associated tool button for removing rows)'
                        IF(langg == 'FR') WRITE(str1,*) 'Le symbole ',k2,symbole(k2)%s,' est déjà défini!', char(13), &
                            'S''il vous plaît, supprimer (supprimer) le row', k2,'!',CHAR(13), &
                            '(mettre le curseur dans cette ligne; utilisez le bouton d''outil associé pour supprimer des lignes)'
                        call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMainDiag:", resp,mtype=GTK_MESSAGE_WARNING)
                        call WTreeViewSetCursorCell('treeview1', 2, k2)

                        k2m = k2
                        irowtab = k2m
                        ifehl = 1
                        goto 9000
                    end if
                end do
            end do

            if(prout) then
                WRITE(66,*) 'nach Laden der Symbol-Tabelle: ngrs,nab,nmu=',int(ngrs,2),int(nab,2),int(nmu,2)
                do i=1,ngrs
                    WRITE(66,*) symbole(i)%s,' ',symtyp(i)%s,' ', &
                        einheit(i)%s,' ',bedeutung(i)%s
                end do
            end if
            !  do i=1,ngrs
            !    WRITE(66,'(a,i3,1xa,2x,a,2x,a)') 'PMD: i=',i,TRIM(symbole(i)),TRIM(symbole_CP(i))
            !  end do

            if(.not.batest_on .and. .not.automode) then
                if(consoleout_gtk) write(0,*) 'PMD 551, LoadCompletedSyms: before WDListstoreFill_1(liststore_symbols)'
                call WDListstoreFill_1('liststore_symbols', ngrs, Symbole)
                if(consoleout_gtk) write(0,*) 'PMD 553, LoadCompletedSyms: after WDListstoreFill_1(liststore_symbols)'
            end if

            if(apply_units) call load_unit_conv(ngrs+ncov+numd)

            ! MDused = .false.
            nvv = 0
            do i=1,ngrs
                if(trim(lowcase(symtyp(i)%s)) == 'm') then
                    nvv = nvv + 1
                    mmvv(nvv) = i
                end if
            end do
            ! write(0,*) 'allocated(meanMD)=',allocated(meanMD),'  ubound=',ubound(meanMD,dim=1)
            if(nvv > 0 .and. .not.allocated(meanID)) call InitVarsTV8(nvv)
            ! write(0,*) 'allocated(meanMD)=',allocated(meanMD)
            if(consoleout_gtk) write(0,*) 'PMD 569, before 43:  nvarsMD=',nvarsMD

            do ii=1,nvv
                i = mmvv(ii)
                name = symbole(i)%s // '_data'
                if(nvarsMD == 0) then
                    nvarsMD = 1

                    meanID(nvarsMD)%s = trim(name)
                    k_datvar = 1
                    MDpoint(k_datvar) = i
                    MDpointrev(i) = k_datvar
                    MDused(k_datvar) = .true.
                    call gtk_widget_set_sensitive(idpt('TBmeansMD'), 1_c_int)
                    ! write(66,*) 'PMD: A: nvarsMD=1: name=',trim(name),' MDpointrev(i)=',MDpointrev(i)

                else
                    call gtk_widget_set_sensitive(idpt('TBmeansMD'), 1_c_int)
                    nfd = 0
                    k = findlocT(meanID,name)
                    if(k > 0) then
                        k_datvar = k
                        nfd = 1
                        MDpoint(k_datvar) = i
                        MDpointrev(i) = k_datvar
                        MDused(k_datvar) = .true.
                        !write(66,*) 'PMD: B: k_datvar=',int(k_datvar,2),' nvarsMD=',int(nvarsMD,2), &
                        !                   ' : name=',trim(name),' MDpointrev(i)=',int(MDpointrev(i))
                    else
                        nvarsMD = nvarsMD + 1
                        ix = ubound(meanID,dim=1)
                        meanID(nvarsMD)%s = trim(name)
                        k_datvar = nvarsMD
                        MDpoint(k_datvar) = i
                        MDpointrev(i) = k_datvar
                        nvalsMD(k_datvar) = 0
                        MDused(k_datvar) = .true.
                        !write(66,*) 'PMD: C: nvarsMD=',int(nvarsMD,2),' k_datvar=',k_datvar
                        !write(66,*) 'PMD: C: nvarsMD=',int(nvarsMD,2),' name=',trim(name), &
                        !                ' MDpointrev(i)=',int(MDpointrev(i),2)
                    end if
                end if
            end do
            if(consoleout_gtk) write(0,*) 'PMD 598, before 45'

45          continue
            do j=1,nvarsMD
                ! eliminate mean values which are no longer used because of user
                ! modfications via the GUI:
                if(.not.MDused(j)) then
                    MDused(1:nvarsMD-1) = [  MDused(1:j-1), MDused(j+1:nvarsMD) ]
                    meanID(1:nvarsMD-1) = [  meanID(1:j-1), meanID(j+1:nvarsMD) ]
                    MDpoint(1:nvarsMD-1) = [  MDpoint(1:j-1), MDpoint(j+1:nvarsMD) ]
                    meanMD(1:nvarsMD-1) = [  meanMD(1:j-1), meanMD(j+1:nvarsMD) ]
                    umeanMD(1:nvarsMD-1) = [  umeanMD(1:j-1), umeanMD(j+1:nvarsMD) ]
                    nvalsMD(1:nvarsMD-1) = [  nvalsMD(1:j-1), nvalsMD(j+1:nvarsMD) ]
                    MDpointrev(MDpoint(j:nvarsMD-1)) = [ (i,i=j,nvarsMD-1) ]
                    xdataMD(1:ixdanf(nvarsMD)+nvalsMD(nvarsMD)-1 - nvalsMD(j)) =  &
                        [ xdataMD(1:ixdanf(j)-1),     &
                        xdataMD(ixdanf(j+1):ixdanf(nvarsMD)+nvalsMD(nvarsMD)-1)  ]
                    nvarsMD = nvarsMD - 1
                    goto 45
                end if
            end do
            if(nvarsMD > 0) then
                call WDListstoreFill_1('liststore_MDvars',nvarsMD,meanID)
            end if
            if(.not.Gum_restricted) then
                IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
                IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGr))
            else
                knetto(kEgr) = 0
                kbrutto(kEgr) = 0
            end if
            if(consoleout_gtk) write(0,*) 'PMD 629'
            ! write(66,*) 'PMD_617: knetto=',int(knetto,2),'  kbrutto=',int(kbrutto,2)

            IF(langg == 'DE') WRITE(str1,'(a,a)') 'Aktive Ergebnisgröße:'
            IF(langg == 'EN') WRITE(str1,'(a,a)') 'Active output quantity:'
            IF(langg == 'FR') WRITE(str1,'(a,a)') 'Quantité de sortie active:'

            call WDPutLabelString('LBOutpQuantity', trim(str1))
            call WDPutEntryString('entryActiveKegr', TRIM(Symbole(kEGr)%s))

            IF(k2m == 0) THEN
                IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) THEN
                    IF(langg == 'DE')   &
                        str1 = 'Auswahl treffen, dann Button "Alles übernehmen"'
                    IF(langg == 'EN')   &
                        str1 = 'Make selection, then Button "Accept all"'
                    IF(langg == 'FR')   &
                        str1 = 'Effectuez une sélection, puis cliquez sur le bouton "Accepter tout"'
                    call WrStatusBar(4,trim(str1))
                else
                    IF(langg == 'DE') call WrStatusBar(4, &
                        'Button "Alles übernehmen!"')
                    IF(langg == 'EN') call WrStatusBar(4, &
                        'Button "Accept all!"')
                    IF(langg == 'FR') call WrStatusBar(4, &
                        'Bouton "Accepter tout!"')
                end if
            else
                ! after having found double entries, one of it(number k2m) must be removed:
                IF(langg == 'DE') WRITE(str1,*) k2m,'.te Zeile entfernen!'
                IF(langg == 'EN') WRITE(str1,*) 'remove ',k2m,'th row!'
                IF(langg == 'FR') WRITE(str1,*) 'enlever la rangée ',k2m,'!'
                call WrStatusBar(4, TRIM(str1))
                call WTreeViewRemoveRow('treeview1', k2m)
                call WTreeViewRemoveRow('treeview2', k2m)
                GOTO 15
            end if

            goto 9000

!+++++++++++++++
          case ('AcceptAll')

            if(consoleout_gtk) write(0,*) '##### PMD  bei AcceptAll:  '
            call gtk_widget_set_sensitive(idpt('NBValUnc'), 1_c_int)
            call gtk_widget_set_visible(idpt('NBValUnc'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('box4'), 1_c_int)
            if(.not.batest_on) call gtk_widget_show(idpt('box4'))
            call gtk_widget_set_sensitive(idpt('TBRefreshCalc'), 1_c_int)

            write(66,'(2(a,3i3))') 'PMD_665: knetto=',knetto,'  kbrutto=',kbrutto

            if(consoleout_gtk) write(0,*) '##### PMD  3 times set_sensitive:  GUM_Restricted= ',GUM_restricted,  &
                'knetto=',int(knetto(kEGr),2)
            if(prout) write(66,*) '**** ProcMainDiag:    bei AcceptAll angekommen'

            if(prout) write(66,*) '**** ProcMainDiag:    after WDGetComboboxAct(comboboxNetRate,knetto(kEGr)) '
            IF(FitDecay .OR. Gamspk1_fit) THEN
                if(ubound(knetto_name,dim=1) == 0) then
                    allocate(knetto_name(knumEGr),kbrutto_name(knumEGr))
                    do i=1,knumEGr
                        knetto_name(i)%s = ' '
                        kbrutto_name(i)%s = ' '
                    end do
                end if
                IF(FitDecay) THEN
                    knetto(kEGr) = klinf
                    knetto_CP(kEGr) = knetto(kEGr)
                    knetto_name(kEGr) = Symbole(knetto(kEGr))
                end if
                IF(Gamspk1_fit) THEN
                    knetto(kEGr) = kgspk1
                    knetto_CP(kEGr) = knetto(kEGr)
                    knetto_name(kEGr) = Symbole(knetto(kEGr))
                end if
            end if
            IF(.not.FitDecay .AND. .not.Gamspk1_Fit .and. .not.SumEval_fit .and. .not.Gum_restricted) then
                call WDGetComboboxAct('comboboxGrossRate',kbrutto(kEGr))
                call WDGetComboboxAct('comboboxNetRate',knetto(kEGr))
                if(consoleout_gtk) write(0,*) '##### PMD  after 2nd WDGetComboboxAct(comboboxGrossRate,kbrutto(kEGr))'

                ! write(66,*) 'PMD:  2 count rate symbols identified: ',knetto(kEGr),kbrutto(kEGr)

                if(knetto(kEGr) == 0 .or. kbrutto(kEGr) == 0) then
                    IF(langg == 'DE') WRITE(str1,*) 'Hinweis:' // CHAR(13) //   &
                        'Das Symbol für Netto- und/oder Bruttozählrate wurde nicht selektiert!' // CHAR(13) // &
                        'Bitte die Selektion überprüfen!'
                    IF(langg == 'EN') WRITE(str1,*) 'Warning:' // CHAR(13) //   &
                        'A symbol for net and/or gross count rate was not selected!' // CHAR(13) // &
                        'Please, check the selection!'
                    IF(langg == 'FR') WRITE(str1,*) 'Attention:' // CHAR(13) //   &
                        'Un symbole pour net et / ou grand nombre de comptes n''a pas été sélectionné!' // CHAR(13) // &
                        'S''il vous plaît, vérifiez la sélection!'

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMainDiag:", resp,mtype=GTK_MESSAGE_WARNING)
                    ifehl = 1
                    goto 9000     ! return
                end if

                if(kEGr > 1) then
                    do k=1,knumEGr
                        if(k == kEGr) cycle
                        if(kbrutto(kEGr) == kbrutto(k) .or. knetto(kEGr) == knetto(k)) then
                            IF(langg == 'DE') WRITE(str1,*) 'Hinweis:' // CHAR(13) //   &
                                'Das Symbol für Netto- und/oder Bruttozählrate wurde nicht passend selektiert!' &
                                // CHAR(13) // 'Bitte die Selektion überprüfen!'
                            IF(langg == 'EN') WRITE(str1,*) 'Warning:' // CHAR(13) //   &
                                'The symbol selected for net and/or gross count rate does not fit!' &
                                // CHAR(13) // 'Please, check the selection!'
                            IF(langg == 'FR') WRITE(str1,*) 'Attention:' // CHAR(13) //   &
                                'Un symbole pour net et / ou grand nombre de comptes n''a pas été sélectionné!' // CHAR(13) // &
                                'S''il vous plaît, vérifiez la sélection!'
                            call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMainDiag:", resp,mtype=GTK_MESSAGE_WARNING)
                            ifehl = 1
                            goto 9000     ! return
                        end if
                    end do

                end if
                if(allocated(kbrutto_name)) deallocate(kbrutto_name)
                if(allocated(knetto_name)) deallocate(knetto_name)
                allocate(kbrutto_name(knumEGr))
                allocate(knetto_name(knumEGr))
                kbrutto_name(kEGr) = symbole(kbrutto(kEGr))
                knetto_name(kEGr) = symbole(knetto(kEGr))
            end if
            if(gum_restricted) goto 165

            IF(.not.FitDecay .AND. .not.Gamspk1_Fit .and. .not.Gum_restricted .and.  &
                knetto(kEGr) /= kEGr .and. allocated(nRSsy)) then
                kgr = 0
                do i=1,nRSsy(kEGr)
                    IF( RS_SYMBOLNr(kEGr,i) == knetto(kEGr) ) kgr = 1
                end do
                if(kgr == 0) then
                    do i=1,nRSsy(kEGr)
                        do k=1,nab
                            IF( RS_SYMBOLNR(kEGr,i) == k ) then
                                do k1=1,nRSsy(k)
                                    IF( RS_SYMBOLNR(k,k1) == knetto(kEGr) ) then
                                        kgr = 1
                                        exit
                                    end if
                                end do
                            end if
                            if(kgr == 1) exit
                        end do
                        if(kgr == 1) exit
                    end do
                end if
                IF(kgr == 0) THEN
                    write(chint,'(I0)') kEGr
                    IF(langg == 'DE') WRITE(str1,*) 'Hinweis:' // CHAR(13) //   &
                        'Das Symbol für die Nettozählrate taucht nicht in Gleichung ',trim(chint),' auf!' // CHAR(13) // &
                        'Bitte diese Gleichung überprüfen!'
                    IF(langg == 'EN') WRITE(str1,*) 'Warning:' // CHAR(13) //   &
                        'The symbol for the net count rate doeas not appear in equation ',trim(chint),'!' // CHAR(13) // &
                        'Please, check this equation!'
                    IF(langg == 'FR') WRITE(str1,*) 'Attention:' // CHAR(13) //   &
                        'Le symbole du taux de comptage net n''apparaît pas dans l''équation ',trim(chint),'!' // CHAR(13) // &
                        'S''il vous plaît, vérifiez cette équation!'
                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMainDiag:", resp,mtype=GTK_MESSAGE_WARNING)
                    ifehl = 1
                    goto 9000     ! return
                end if
            end if


            IF(.not.FitDecay .AND. .not.Gamspk1_Fit .and. .not.SumEval_fit .and. kModelType == 1 .and.allocated(nRSsy)) THEN
                unit_ident = TRIM(ucase(einheit(knetto(kEGr))%s)) == TRIM(ucase(einheit(kbrutto(kEGr))%s))

                IF(.NOT. unit_ident) then
                    IF(ucase(einheit(knetto(kEGr))%s) == '1/S'   .AND. ucase(einheit(kbrutto(kEGr))%s) == 'CPS') unit_ident = .TRUE.
                    IF(ucase(einheit(knetto(kEGr))%s) == 'CPS'   .AND. ucase(einheit(kbrutto(kEGr))%s) == '1/S') unit_ident = .TRUE.
                    IF(ucase(einheit(knetto(kEGr))%s) == '1/MIN' .AND. ucase(einheit(kbrutto(kEGr))%s) == 'CPM') unit_ident = .TRUE.
                    IF(ucase(einheit(knetto(kEGr))%s) == 'CPM'   .AND. ucase(einheit(kbrutto(kEGr))%s) == '1/MIN') unit_ident = .TRUE.
                end if
                IF( .not. unit_ident .AND. .not.loadingpro ) THEN
                    IF(langg == 'DE') WRITE(str1,*) 'Hinweis:' // CHAR(13) //   &
                        'Die für Netto- und Bruttozählrate selektierten Symbole ' // CHAR(13) // &
                        'haben nicht die gleiche Einheit!' // CHAR(13) // &
                        'Bitte die korrekte Selektion dieser Variablen überprüfen!'
                    IF(langg == 'EN') WRITE(str1,*) 'Warning:' // CHAR(13) //   &
                        'The symbols selected for net and gross count rate ' // CHAR(13) // &
                        'have different units! Please, check the correct selection of these variables!'
                    IF(langg == 'FR') WRITE(str1,*) 'Avertissement:' // CHAR(13) //   &
                        'Les symboles sélectionnés pour le taux de comptage net et brut ' //char(13) // &
                        'ont des unités différentes! Veuillez vérifier la sélection correcte de ces variables!'
                    if(consoleout_gtk) write(0,*) '##### PMD  before MessageShow '
                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMainDiag:", resp,mtype=GTK_MESSAGE_WARNING)
                    WRITE(66,*) 'knetto(kEGr)=',knetto(kEGr),'   kbrutto(kEGr)=',kbrutto(kEGr)
                end if

                ! Search for the symbol for kbrutto also in auxiliary equations for knetto:
                kgr = 0
                call FindSymb(knetto(kEGr),kbrutto(kEGr),sfound, kgr)

                ! IF(kgr == 0 .and. .not.batest_on .and. .not.autoreport) THEN
                IF(.not.batest_on .and. .not.autoreport) THEN
                    if(kgr == 0) then
                        write(66,*) '** ProcMainDiag: Symbol(knetto)=',trim(symboleG(knetto(kEGr))%s), &
                            ' RS: ',(RSsy(nRSsyanf(knetto(kEGr))+j-1)%s,j=1,nRSsy(knetto(kEGr)))
                        write(66,*) '** ProcMainDiag: Symbol(kbrutto=',symboleG(kbrutto(kEGr))%s
                        write(66,*) '** ProcMainDiag: knetto=',knetto(kEGr),'  kbrutto=',kbrutto(kEGr)
                        IF(langg == 'DE') WRITE(str1,*) 'Hinweis:' // CHAR(13) //   &
                            'Das selektierte Symbol der Bruttozählrate taucht' // CHAR(13) // &
                            'in der Gleichung der Nettozählrate nicht auf!' // CHAR(13) // &
                            'Bitte die Selektion überprüfen!'
                        IF(langg == 'EN') WRITE(str1,*) 'Warning:' // CHAR(13) //   &
                            'The selected gross count rate symbol does not occur' // CHAR(13) // &
                            'in the equation defining the net count rate! ' // CHAR(13) // &
                            'Please, check the selection!'
                        IF(langg == 'FR') WRITE(str1,*) 'Avertissement:' // CHAR(13) //   &
                            'Le symbole de grand compte sélectionné ne se produit pas ' // CHAR(13) // &
                            'dans l''équation définissant le taux de comptage net! ' // CHAR(13) // &
                            'S''il vous plaît, vérifiez la sélection!'
                        call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMainDiag:", resp,mtype=GTK_MESSAGE_WARNING)
                    end if
                end if
            end if

            IF(FitDecay .or. Gamspk1_fit) THEN
                kbrutto(kEGr) = 0
                ckt = '       '
                IF(FitDecay .AND. knetto(kEGr) /= klinf) ckt = 'Linfit'
                IF(Gamspk1_fit .AND. knetto(kEGr) /= kgspk1) ckt = 'Gamspk1'
                IF(LEN_TRIM(ckt) > 0) THEN
                    IF(langg == 'DE') WRITE(str1,*) 'Die mit ' // TRIM(ckt) //  &
                        ' definierte "Netto-Zählrate" passt nicht zum dafür selektiertem Symbol!' // &
                        CHAR(13) // 'Bitte die Auswahl jetzt korrigieren!'
                    IF(langg == 'EN') WRITE(str1,*) 'The symbol selected as "net count rate" ' // &
                        'does not correespond with that defined by ' // TRIM(ckt) // ' !' // &
                        CHAR(13) // 'Please, correct the selection now!'
                    IF(langg == 'FR') WRITE(str1,*) 'Le symbole sélectionné comme "taux de comptage net" ' // &
                        'ne correspond pas à celle définie par ' // TRIM(ckt) // ' !' // &
                        CHAR(13) // 'S''il vous plaît, corrigez la sélection maintenant!'
                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "ProcMainDiag:", resp, mtype=GTK_MESSAGE_WARNING)
                    WRITE(66,'(2(a,i3))') 'knetto(kEGr)=',knetto(kEGr),'   kbrutto(kEGr)=',kbrutto(kEGr)
                    GOTO 17
                end if
            end if
            ! WRITE(66,'(2(a,i3))') 'knetto(kEGr)=',knetto(kEGr),'   kbrutto(kEGr)=',kbrutto(kEGr)

165         continue
            if(consoleout_gtk) write(0,*) '##### PMD  Label 165:  '
            IF(knetto(kEGr) > 0 .AND. knetto(kEGr) /= kbrutto(kEGr)   &
                .AND. (Fitdecay .OR. (.not.Fitdecay .AND. kbrutto(kEGr) > 0)) .OR. Gamspk1_fit) THEN
                call gtk_widget_set_sensitive(idpt('NBValUnc'), 1_c_int)
                call gtk_widget_set_visible(idpt('NBValUnc'), 1_c_int)
                if(.not.batest_on) call gtk_widget_show(idpt('box4'))
                call gtk_widget_set_sensitive(idpt('TBRefreshCalc'), 1_c_int)
                IF(langg == 'DE') call WrStatusBar(4,'zu TAB "Werte, Unsicherheiten" wechseln')
                IF(langg == 'EN') call WrStatusBar(4,'select TAB "Values, uncertainties"')
                IF(langg == 'FR') call WrStatusBar(4,'sélectionnez TAB "Valeurs, incertitudes"')
            END IF
            if(consoleout_gtk) write(0,*) '##### PMD  before 17 continue :  '
17          CONTINUE
            if(.not.loadingPro)  call pending_events

            goto 9000
            ! for testing, set the cursor position (cursorPos):
            ! call WTreeViewSetCursorCell('treeview1', 5, 4)

          case ('CalcValUnc')       ! button: calculated the uncertainties in the Table (Values, uncertainties)

            if(prout) write(66,*) '**** ProcMainDiag:    arrived at CalcValUnc'
            write(66,*) 'PMD-911: ngrs=',int(ngrs,2)
            klincall = 0
            MCSim_on = .FALSE.
            kxx = ngrs
            nhp_defined = .false.

            if(allocated(rdummy)) deallocate(rdummy)
            allocate(rdummy(ngrs))
            call WTreeViewGetDoubleArray('treeview2',5, kxx, Messwert)
            call WTreeViewGetComboArray('treeview2', 6, kxx, IVTL)
            call WTreeViewGetStrArray('treeview2',7, kxx, SDFormel)
            call WTreeViewGetDoubleArray('treeview2',8, kxx, SDWert)
            call WTreeViewGetDoubleArray('treeview2',9, kxx, HBreite)
            call WTreeViewGetComboArray('treeview2', 10, kxx, IAR)
            do i=nab+1,ngrs
                nfd = 0
                if(FitDecay) then
                    if(i >= kfitp(1) .and. i <= kfitp(1)+2) nfd = 1
                    if(ucase(symbole(i)%s) == 'TMESS') nfd = 1
                    if(ucase(symbole(i)%s) == 'TSTART') nfd = 1
                end if
                if(nfd == 0 .and. abs(Messwert(i) - missingval) < eps1min) then
                    IF(langg == 'DE') call WrStatusBar(4, 'Es fehlen noch Messwerte!')
                    IF(langg == 'EN') call WrStatusBar(4, 'Measured values are still missing!')
                    IF(langg == 'FR') call WrStatusBar(4, 'Les valeurs mesurées manquent toujours!')
                    ifehl = 1
                    GOTO 9000
                end if
            end do
            do i=knumEGr+1,kxx
                call WDListstoreClearCell('treeview2', 11, i )
            end do
            ! try to set limits to the TV colums widths
            if(.not.loadingPro) then
                call TVtrimCol_width('treeview2')
                call gtk_widget_show_all(idpt('treeview2'))
                !      call TVtrimCol_width('treeview3')
                !      call gtk_widget_show_all(idpt('treeview3'))
            end if
            if(findloc(IVTL,11,dim=1) > 0) then
                N_preset = .true.
            else
                N_preset = .false.
            end if

            i = findloc(IVTL,7,dim=1)
            if(i > 0) then
                ! iptr_ arrays will only later be defined, in Rechw1!
                if(kbgv_binom <= 0) then
                    dialogstr = 'dialog_BinPoi'
                    ioption = 71
                    call FindItemS(trim(dialogstr), ncitem2)
                    call Loadsel_diag_new(1, ncitem2)
                    if(ubound(iptr_cnt,dim=1) < i) call IntModA1(iptr_cnt,i)
                    iptr_cnt(i) = i
                    write(66,'(a,4(i0,1x))') 'itm_binom,ip_binom,ilam_binom=',itm_binom,ip_binom,ilam_binom
                    IF(ifehl == 1) goto 9000
                end if
            else
                kbgv_binom = 0
                use_bipoi = .false.
            end if

            ! Here, for the first time, the value nabf is determined (number of SD fomuelae in treview2),
            ! i.e., before Rechw1

            nabf = 0
            ns1 = nsymbnew
            do i=1,kxx
                if(len_trim(SDformel(i)%s) == 0) cycle
                nabf = nabf + 1
                call initf(1)
                call parsef(1,sdformel(i)%s,SymboleG)
            end do

            if(.false. .and. nabf > 0) then
                ! Check, whether an SD fomula has been added, or deleted, by the user
                ! write(66,*) 'Sdformel-test: at begin: nabf=',nabf
                do i=1,ngrs
                    SDformel(i)%s = ' '
                end do
            end if

            do jj=1,2
                if(jj == 1) k = FindlocT(Symbole,'kilo_Trigger',1)
                if(jj == 2) k = FindlocT(Symbole,'min_Trigger',1)
                if(k > 0) then
                    if(apply_units) Messwert(k) = zero
                    if(.not. apply_units) Messwert(k) = one
                end if
            end do

            ! covariance grid:
            kxy = ncovmx
            write(66,*) ' PMD: ncovmx=',ncovmx
            goto 156
156         continue
            ! if(.not.simul_ProSetup) then
            call WTreeViewGetComboArray('treeview3', 2, kxy, ISymbA)
            call WTreeViewGetComboArray('treeview3', 2, kxy, ISymbA_CP)
            call WTreeViewGetComboArray('treeview3', 3, kxy, ISymbB)
            call WTreeViewGetComboArray('treeview3', 3, kxy, ISymbB_CP)
            call WTreeViewGetComboArray('treeview3', 4, kxy, icovtyp)
            call WTreeViewGetComboArray('treeview3', 4, kxy, icovtyp_CP)
            call WTreeViewGetDoubleArray('treeview3',6, kxy, covarval)
            call WTreeViewGetDoubleArray('treeview3',6, kxy, covarval_CP)
            if(IsymbA(1) > 0 .and. IsymbB(1) > 1) then
                call WTreeViewGetStrArray('treeview3',5, kxy, CVFormel)
                call WTreeViewGetStrArray('treeview3',5, kxy, CVFormel_CP)
            end if
            ! end if
            if(prout .and. ubound(IsymbA,dim=1) > 0) WRITE(66,*)  &
                'PMD_981: ISymbA(1)=',int(ISymbA(1),2),'  ISymbB(1)=',int(ISymbB(1),2)
            if(prout) write(66,'(a,i0)') 'PMD: Ubound(IsymbA,dim=1)=',Ubound(IsymbA,dim=1)

            do i=1,kxy
                if(i > ubound(isymbA,dim=1)) cycle
                if(IsymbA(i) == 0 .and. IsymbB(i) == 0) then
                    ncov = i-1
                    exit
                else
                    ncov = i
                end if
                if(abs(covarval(i)-missingval) < eps1min .and. len_trim(CVFormel(i)%s) == 0) then
                    if(.not.(Gamspk1_Fit .and. ecorruse == 0)) then
                        IF(langg == 'DE') call WrStatusBar(4, 'Es fehlen noch Kovarianzwerte!')
                        IF(langg == 'EN') call WrStatusBar(4, 'Covariance values are missing!')
                        IF(langg == 'FR') call WrStatusBar(4, 'Les valeurs de covariance sont manquantes!')
                        ifehl = 1
                        GOTO 9000
                    end if
                end if
            end do
            if(ncov > 0 .and. ncovf == 0) then
                ncovf = 0
                do i=1,ncov
                    if(len_trim(CVFormel(i)%s) > 0) ncovf = ncovf + 1
                end do
            end if
            if(ncov > ubound(corrval,dim=1)) call RealModA1(corrval,ncov+3)
            if(ncov > 0 .and. apply_units .and. ubound(einheit,dim=1) == ngrs) call CharModA1(einheit,ngrs+ncov+numd)

            if(prout) then
                do i=1,ngrs
                    write(66,*) 'i=',int(i,2),'  ',Symbole(i)%s,'  Messwert=',sngl(Messwert(i)),'  StdUnc(i)=',sngl(StdUnc(i))
                end do
            end if
            write(66,*) 'PMD before RW1: ncov=',int(ncov,2),' numd=',int(numd,2),' ngrs=',int(ngrs,2)

            do i=1,ncov
                CVFormel(i)%s = ucase(CVFormel(i)%s)
                if(i > ubound(CVFormel_CP,dim=1)) then
                    call CharModA1(CVFormel_CP,i)
                    CVFormel_CP(i)%s = ' '
                    call RealModA1(covarval_CP,i)
                    covarval_cp(i) = zero
                end if
                if(i > ubound(Corrval,dim=1)) call RealModA1(CorrVal,i+3)
                if(len_trim(CVFormel_CP(i)%s) > 0) CVFormel_CP(i)%s = ucase(CVFormel_CP(i)%s)
                if(icovtyp(i) == 2) CorrVal(i) = covarval(i)
                if(abs(covarval(i)-missingval) < eps1min) covarval(i) = 0._rn
                if(abs(covarval_CP(i)-missingval) < eps1min) covarval_CP(i) = 0._rn

            end do
            if(ncov == 1 .and. ubound(corrval,dim=1) == 1) call RealModA1(CorrVal,i+3)

            ! do i=1,ubound(IsymbA,dim=1)
            !   WRITE(66,*) 'PMD: i=',int(i,2),' ISymbA(i)=',int(ISymbA(i),2),'  ISymbB(i)=',int(ISymbB(i),2), &
            !               ' covarval=',sngl(covarval(i)),' corrval=',sngl(corrval(i)), &
            !               ' CVFormel(i)=',CVFormel(i)%s
            ! end do

            ! Save the covar-symbols:
            Symbole_CP(1:ngrsP) = Symbole(1:ngrsP)
            call Readj_knetto()
            call Readj_kbrutto()

            ! Calculation of absolute standard uncertainties
            IF(langg == 'DE') call WrStatusBar(4,'Rechnet.... ' )
            IF(langg == 'EN') call WrStatusBar(4,'Calculating.... ' )
            IF(langg == 'FR') call WrStatusBar(4,'Calcule.... ' )

            ifehl = 0
            ifehlP = 0
            call cpu_time(stt1)

            call Rechw1()

            call cpu_time(stp1)
            write(66,'(a,f8.3,2(a,i0))') 'Rechw1: cpu-time (s) :',(stp1-stt1),'  ifehl=',ifehl,'  ifehlp=',ifehlp

            !---------------------------------
            if(ifehl == 1 .and. grossfail) then
                ! added: 02.6.2023 GK
                call WDNotebookSetCurrPage('notebook1', 2)
                if(kEGr > 0 .and. .not.Gum_restricted .and. .not.FitDecay .and. .not.Gamspk1_Fit) then
                    if(knetto(kEGr) > 0) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
                    if(kbrutto(kEGr) > 0) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGr))
                end if
                goto 9000
            end if
            IF(ifehlP == 1) THEN
                IF(langg == 'DE') call WrStatusBar(4, 'Fehler in Gleichungen beheben!')
                IF(langg == 'EN') call WrStatusBar(4, 'Eliminate errors in equations!')
                IF(langg == 'FR') call WrStatusBar(4, 'Éliminer les erreurs dans les équations!')
                GOTO 9000
            END IF
            IF(ifehl == 1) THEN
                if(FitDecay .and. .not.posdef) then
                    if(use_WTLS) then
                        IF(langg == 'DE') call WrStatusBar(4, &
                            'Fehler: Kovar-Matrix bei WTLS nicht positive definit, d.h. nicht invertierbar!')
                        IF(langg == 'EN') call WrStatusBar(4, &
                            'Err: covar matrix for WTLS not positive definite, i.e. not invertable!')
                        IF(langg == 'FR') call WrStatusBar(4, &
                            'Err: matrice de covariance pour WTLS non définie positive, c''est-à-dire pas inversible!')
                        GOTO 9000
                    else
                        IF(langg == 'DE') call WrStatusBar(4, &
                            'Fehler: Kovar-Matrix bei WLS nicht positive definit, d.h. nicht invertierbar!')
                        IF(langg == 'EN') call WrStatusBar(4, &
                            'Err: covar matrix for WLS not positive definite, i.e. not invertable!')
                        IF(langg == 'FR') call WrStatusBar(4, &
                            'Err: matrice de covariance pour WLS non définie positive, c''est-à-dire pas inversible!')
                        GOTO 9000
                    end if
                else
                    IF(langg == 'DE') call WrStatusBar(4, 'Fehler bei Werten in der Tabelle beheben!')
                    IF(langg == 'EN') call WrStatusBar(4, 'Eliminate value error(s) in the table!')
                    IF(langg == 'FR') call WrStatusBar(4, 'Éliminer les erreurs de valeur dans la table!')
                    GOTO 9000
                end if
            END IF

            if(.not.loadingPro) call WrStb_Ready()

            IF(langg == 'DE') call WrStatusBar(4, 'zu TAB "Unsicherheiten-Budget" wechseln' )
            IF(langg == 'EN') call WrStatusBar(4, 'select TAB "Uncertainty budget" ' )
            IF(langg == 'FR') call WrStatusBar(4, 'sélectionnez TAB "Budget d''incertitude" ' )

            call gtk_widget_set_sensitive(idpt('NBBudget'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('NBResults'), 1_c_int)
            call gtk_widget_set_visible(idpt('NBBudget'), 1_c_int)
            call gtk_widget_set_visible(idpt('NBResults'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('box5'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('grid5'), 1_c_int)
            call gtk_widget_set_visible(idpt('box5'), 1_c_int)
            call gtk_widget_set_visible(idpt('grid5'), 1_c_int)
            if(.not.loadingPro) then
                call gtk_widget_show(idpt('box5'))
                call gtk_widget_show(idpt('grid5'))
            end if

            ! try to limit the TV column width values
            if(.not.loadingPro) call TVtrimCol_width('treeview4')
            if(.not.loadingpro) call pending_events
            goto 9000
!+---------
          case ('ChangeBudgetType')
            kmin = 1
            kanz = ngrs+ncov+numd
            IF(Ucontyp == 1) THEN
                Ucontyp = 2
                call WTreeViewPutDoubleArray('treeview4', 8, kanz, Ucontrib)
                if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview4', 8, 'abs. U-Beitrag')
                if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview4', 8, 'abs. U contribut')
                if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview4', 8, 'abs. U contribut')
            else
                Ucontyp = 1
                call WTreeViewPutDoubleArray('treeview4', 8, kanz, Perc)
                if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview4', 8, 'rel. Beitrag(%)')
                if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview4', 8, 'rel. contribut(%)')
                if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview4', 8, 'rel. contribut(%)')
            end if
            goto 9000

          case ('TRbuttonSavecsv')
            call SaveResults()
            goto 9000

          case ('TEClose')     ! Text-Editor
            loadingpro = .true.
            call WDNotebookSetCurrPage('notebook1',3)
            ! write(66,*) 'TEClose: apply_units=',apply_units
            call gtk_widget_hide(idpt('window_graphs'))            ! 17.9.2023
            if(apply_units) then
                call Restore_Ucheck
                unit_conv_fact = one
                apply_units = .false.
                FP_for_units = .false.
                call ProcessLoadPro_new(3,kEGr)
            end if
            loadingpro = .false.
            call gtk_widget_set_sensitive(idpt('NBEditor'),0_c_int)

            call gtk_widget_set_sensitive(idpt('NBResults'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('NBBudget'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('NBValUnc'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('CheckUnits'), 1_c_int)

            call pending_events
            goto 9000

          case ('TESavePrjAs')
            ! Texteditor (TAB 6): save the project modified by units as a new file
            FileTyp = 'P'
            cheader = 'Choose new project filename:'
            call FOpen(ifehl, .true., cheader)
            if(ifehl == 1) goto 9000  !return
            call gtk_widget_set_sensitive(idpt('NBEditor'),0_c_int)
            ! call WTreeViewPutStrArray('treeview1', 4, ngrs, einheit)
            uconv = one
            apply_units_dir = .false.
            unit_conv_fact = one

            if(.not.retain_triggers) then
                kk = ngrs       ! ngrs may become < kk during the next loop
161             continue
                !!!! do i=nab+1,ngrs
                do i=1,ngrs !   3.2.2024
                    if(symboleG(i)%s == 'KILO_TRIGGER' .or. symboleG(i)%s == 'MIN_TRIGGER') then
                        ! The symbol with index i shall be eliminated.
                        ! If the indexes of special symbols (kbrutto, knetto, IsymbA, IsymbB) are
                        ! above i, their indexes must also be lowered by 1.
                        do jj=1,knumEGr
                            if(kbrutto(jj) > i) kbrutto(jj) = kbrutto(jj) - 1
                            if(knetto(jj) > i) knetto(jj) = knetto(jj) - 1
                        enddo
                        do jj=1,ncov
                            if(IsymbA(jj) > i) IsymbA(jj) = IsymbA(jj) - 1
                            if(IsymbB(jj) > i) IsymbB(jj) = IsymbB(jj) - 1
                        end do
                        ! Elimination of index i in symbol-dependent arrays:
                        do j=i,ngrs-1
                            Symbole(j)%s = Symbole(j+1)%s
                            SymboleG(j)%s = SymboleG(j+1)%s
                            Symtyp(j)%s = Symtyp(j+1)%s
                            Einheit(j)%s = Einheit(j+1)%s
                            Bedeutung(j)%s = Bedeutung(j+1)%s
                            SDFormel(j)%s = SDFormel(j+1)%s
                            einheit_conv(j)%s = einheit_conv(j+1)%s
                        end do
                        Messwert(1:ngrs-1) = [Messwert(1:i-1), Messwert(i+1:ngrs) ]
                        IVTL(1:ngrs-1) = [IVTL(1:i-1), IVTL(i+1:ngrs) ]
                        SDWert(1:ngrs-1) = [SDWert(1:i-1), SDWert(i+1:ngrs) ]
                        HBreite(1:ngrs-1) = [HBreite(1:i-1), HBreite(i+1:ngrs) ]
                        IAR(1:ngrs-1) = [IAR(1:i-1), IAR(i+1:ngrs) ]
                        StdUnc(1:ngrs-1) = [StdUnc(1:i-1), StdUnc(i+1:ngrs) ]

                        ngrs = ngrs - 1
                        ngrsP = ngrsP - 1
                        goto 161
                    endif
                end do
                do i=ngrs+1,kk
                    Symbole(i)%s = ' '
                    SymboleG(i)%s = ' '
                    symtyp(i)%s = ' '
                    einheit(i)%s = ' '
                    bedeutung(i)%s = ' '
                    Messwert(i) = missingval
                    IVTL(i) = 1
                    SDformel(i)%s = ' '
                    SDWert(i) = missingval
                    HBreite(i) = missingval
                    IAR(i) = 1
                    StdUnc(i) = missingval
                enddo
            end if

            do i=nab+1,ngrs
                einheit(i)%s = einheit_conv(i)%s
            enddo
            call WDListstoreFill_table('liststore_symtable',1, .false.)
            call WDListstoreFill_table('liststore_valunc',2, .true.)
            call WDPutTextviewString('textview2',Formeltext)

            loadingpro = .true.
            call WDNotebookSetCurrPage('notebook1',3)
            apply_units = .false.
            FP_for_units = .false.
            apply_units_dir = .false.
            call ProSave()
            unit_conv_fact = one
            call load_unit_conv(ngrs+ncov)
            call gtk_widget_set_visible(idpt('TESavePrjAs'), 0_c_int)
            call gtk_widget_set_sensitive(idpt('CheckUnits'), 1_c_int)
            call gtk_widget_set_visible(idpt('TEClose'), 0_c_int)

            call gtk_widget_set_sensitive(idpt('NBEditor'),0_c_int)

            call gtk_widget_set_sensitive(idpt('NBResults'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('NBBudget'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('NBValUnc'), 1_c_int)

            call pending_events
            ! goto 9000
            ! The program must be terminated, because many previous allocations
            ! would otherwise to be re-allocated: too much effort.
            IF(langg == 'DE') WRITE(str1,'(a)') 'Das Programm wird nun beendet.'
            IF(langg == 'EN') WRITE(str1,'(a)') 'The program will be terminated now.'
            IF(langg == 'FR') WRITE(str1,'(a)') 'Le programme sera terminé maintenant.'
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Batch:", resp,mtype=GTK_MESSAGE_INFO)
            call gtk_widget_destroy(idpt('window1'))
            call gtk_main_quit()
            stop


          case ('copyBS1')
            call WDGetComboboxAct('comboboxBS1',iopt_copygr)
            ic = gtk_notebook_get_current_page(nbook2)
            i = ic + 1
            if(i == 1) actual_plot = 'MCplot'
            ! if(i == 2) actual_plot = 'BSplot'
            ! if(i == 3) actual_plot = 'CurvePlot'
            ! if(i == 4) actual_plot = 'ELIplot'
            if(i == 2) actual_plot = 'CurvePlot'
            if(i == 3) actual_plot = 'ELIplot'

            call PrintPlot()
            call pending_events
            goto 9000

          case ('Copy1GrELI')
            dialogstr = 'dialogeli'
            ioption = 68
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case ('EQRenameSymb')
            dialogstr = 'dialog_symbchg'
            ioption = 7
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case default

        end select

!----------------------------------------------------

      case ('GtkRadioMenuItem')
        select case (trim(idstring))
          case ('LoadWithCalc')
            project_loadw = .true.

          case ('LoadWithoutCalc')
            project_loadw = .false.
            loadingpro = .false.

          case default
        end select
        goto 9000
!---------------------------------------------------------------------

      case ('GtkMenuItem', 'GtkToolButton', 'GtkImageMenuItem')

        select case (trim(idstring))

          case ('PreSettings')
            ioption = 1
            dialogstr = 'dialog_options'
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case ('TBInputDialog', 'Gspk1Edit', 'FittingData')
            if(FitDecay) then
                dialogstr = 'dialog_decayvals'
                ioption = 3
            end if
            if(Gamspk1_Fit) then
                dialogstr = 'dialog_gspk1'
                ioption = 5
            end if
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case ('RenameQuantity','EQRenameSymb')
            dialogstr = 'dialog_symbchg'
            ioption = 7
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case ('NumberOutputQuantities')
            ioption = 6
            dialogstr = 'dialog_numegr'
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

            call gtk_widget_set_sensitive(idpt('QFirst'), 1_c_int)
            call gtk_widget_set_sensitive(idpt('QSecond'), 0_c_int)
            call gtk_widget_set_sensitive(idpt('QThird'), 0_c_int)
            if(knumEGr > 1) call gtk_widget_set_sensitive(idpt('QSecond'), 1_c_int)
            if(knumEGr > 2) call gtk_widget_set_sensitive(idpt('QThird'), 1_c_int)

          case ('KalFit')
            call LinCalib()

          case ('Gspk1Mean')

          case ('TBFontSel', 'FontSel')
            dialogstr = 'dialog_fontbutton'
            ioption = 62
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case ('TBColorSel')
            dialogstr = 'dialogColB'
            ioption = 63
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case ('ConfidEllipse')
            multi_eval = .true.
            valEGr = 0._rn
            uncEGr = 0._rn
            corrEGr = 0._rn
            kEGrSVE = kEGr
            kEGr = knumEGr
            if(FitDecay) klincall = 0
            IF(langg == 'DE') call WrStatusbar(4,'Rechnet...' )
            IF(langg == 'EN') call WrStatusbar(4,'Calculating...' )
            IF(langg == 'FR') call WrStatusbar(4,'Calcule...' )
            call pending_events()
            call corrmatEGr
            write(66,*) 'nach call corrmatEGr'
            call PrepEli()
            if(.not.Confidoid_activated) then
                sizewh = [  width_da(4), height_da(4) ]
                call gtk_widget_set_vexpand_set(idpt('boxELI'), 1_c_int)
                call gtk_widget_set_vexpand(idpt('boxELI'), 1_c_int)
                call gtk_widget_set_size_request(idpt('boxELI'), width=sizewh(1), height=sizewh(2)+0)
                call hl_gtk_drawing_area_resize(drawing(4), size=sizewh, copy=.true.)
                IF(langg == 'DE') call WDPutLabelString('doELIplot', 'Plot Konfidenz-Ellipse')
                IF(langg == 'EN') call WDPutLabelString('doELIplot', 'plot confidence ellipse')
                IF(langg == 'FR') call WDPutLabelString('doELIplot', 'tracer l''ellipse de confiance')
            end if
            call Confidoid()
            pSV = W1minusG
            ioption = 65
            dialogstr = 'dialogELI'
            call FindItemS('dialogELI', ncitem2)
            write(66,*) 'PrepConfidoid: ncitem2=',ncitem2
            call Loadsel_diag_new(1, ncitem2)
            W1minusG = pSV
            multi_eval = .false.
            kEGr = kEGrSVE
            call WDPutSelRadio('QFirst', kEGr)
            call pending_events()
            call ProcessLoadPro_new(1,kEGr)
            plot_confidoid = .false.
            plot_ellipse = .false.
            call WrStb_Ready()

            ! refresh_type = 1
            ! goto 150
            goto 9000   !return

          case ('TBRemoveGridLine')

            if(trim(actual_GRID) == 'treeview1') then
                ngmax = NumRowsTV('treeview1')
                do i=1,ngmax
                    if(i > ubound(Symbole,dim=1)) exit
                    call WTreeViewGetStrCell('treeview1', 2, i, Symbole(i)%s)
                end do

            end if
            if(trim(actual_GRID) == 'treeview3') then
                call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
                call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
                call gtk_widget_set_visible(idpt('NBBudget'), 0_c_int)
                call gtk_widget_set_visible(idpt('NBResults'), 0_c_int)
                call gtk_widget_set_sensitive(idpt('box5'), 0_c_int)
                call gtk_widget_set_sensitive(idpt('grid5'), 0_c_int)
                call gtk_widget_set_visible(idpt('box5'), 0_c_int)
                call gtk_widget_set_visible(idpt('grid5'), 0_c_int)
            end if
            ! Requires in the Glade-file: de-activate "floating selectionl"!! And: tree-selection: multiple
            numrows_marked = hl_gtk_listn_get_selections(idpt(actual_grid), rownums_marked)
            if(numrows_marked > 0) then
                top_selrow = minval(rownums_marked) + 1
                bottom_selrow = maxval(rownums_marked) + 1

                WRITE(66,*) 'Removerow:   top_selrow=',int(top_selrow,2), &
                    ' bottom_selrow=',int(bottom_selrow,2),'  actual_GRID = ',trim(actual_GRID)
                ! write(66,*) 'numrows_marked=',numrows_marked,'  ngrs=',ngrs

                IF(trim(actual_GRID) == 'treeview1' .OR. trim(actual_GRID) == 'treeview3' .or. &
                    trim(actual_GRID) == 'treeview5' .OR. trim(actual_GRID) == 'treeview6' .or. &
                    trim(actual_GRID) == 'treeview7' .or. trim(actual_GRID) == 'treeview8'  ) then

                    IF(top_selrow > 0 .AND. bottom_selrow >= top_selrow) THEN
                        call AdjustRemoveTVRows(numrows_marked)
                    end if
                end if

                SaveP = .TRUE.
                call FieldUpdate('PMD 1334')
                IF(langg == 'DE') call WrStatusBar(3,'Ungesichert!')
                IF(langg == 'EN') call WrStatusBar(3,'Unsaved!')
                IF(langg == 'FR') call WrStatusBar(3,'Non enregistré!')
                if(trim(actual_GRID) == 'treeview1') symlist_shorter = .false.
            end if

          case ('About_UR')

            if(langg == 'DE') then
                url_str =  'https://www.thuenen.de/de/fachinstitute/fischereioekologie/arbeitsbereiche/' &
                    // 'meeresumwelt/leitstelle-umweltradioaktivitaet-in-fisch/uncertradio' // c_null_char
                comment_str = 'Programm zur Berechnung von Messunsicherheit, ' // CR &
                    // 'Unsicherheiten-Budget, Erkennungs- und Nachweisgrenze bei' // CR &
                    // 'Messungen der Umweltradioaktivität ' // CR // CR &
                    // 'Das Programm steht unter der GNU GPL v3 Lizenz und wurde vom Autor nach derzeitigem Stand von Wissenschaft,' // CR &
                    // 'Normung und Technik entwickelt und bezüglich der Richtigkeit der ' // CR &
                    // 'mathematischen Behandlung der eingegebenen Modellgleichungen validiert.' // CR // CR &
                    // 'E-Mail:    guenter.kanisch(at)hanse.net' // CR  &
                    // '           florian.ober(at)mri.bund.de' // CR  &
                    // '           leitstelle(at)thuenen.de' // c_null_char
                authors(1) = 'Günter Kanisch, früher Thünen-Institut für Fischereiökologie, Hamburg'
                authors(2) = 'Florian Ober, Max Rubner-Institut, Kiel'
                authors(3) = 'Marc-Oliver Aust, Thünen-Institut für Fischereiökologie, Hamburg'
            else
                url_str =  'https://www.thuenen.de/en/institutes/fisheries-ecology/fields-of-activity/' &
                    // 'marine-environment/coordination-centre-of-radioactivity/uncertradio' // c_null_char
                comment_str = trim('Software for calculating measurement uncertainty, ' // CR &
                    // 'uncertainty budget, decision threshold and detection limit for' // CR &
                    // 'measurement of environmental radioactivity.' // CR // CR &
                    // 'The software is licensed under GNU GPL3 and was developed by the author following state-of-the-art ' // CR &
                    // 'of science, standardization and technology and validated with respect' // CR &
                    // 'to the correct mathematical treatment of the model input equations of' // CR &
                    // 'the evaluation model.' // CR // CR &
                    // 'E-Mail:    guenter.kanisch(at)hanse.net' // CR &
                    // '           florian.ober(at)mri.bund.de' // CR  &
                    // '           leitstelle(at)thuenen.de') // c_null_char
                authors(1) = 'Günter Kanisch, formerly at the Thünen Institute of Fisheries Ecology, Hamburg'
                authors(2) = 'Florian Ober, Max Rubner-Institute'
                authors(3) = 'Marc-Oliver Aust, Thünen Institute of Fisheries Ecology, Hamburg'
            end if

            call hl_gtk_about_dialog_show(    &
                name='UncertRadio 2'//c_null_char, &
                license= 'This program is free software: you can redistribute it and/or modify' // CR &
                // 'it under the terms of the GNU General Public License as published by' // CR &
                // 'the Free Software Foundation, either version 3 of the License, or' // CR &
                // '(at your option) any later version.' // CR // CR &
                // 'This program is distributed in the hope that it will be useful,' // CR &
                // 'but WITHOUT ANY WARRANTY; without even the implied warranty of' // CR &
                // 'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' // CR &
                // 'GNU General Public License for more details.' // CR // CR &
                // 'You should have received a copy of the GNU General Public License' // CR &
                // 'along with this program.  If not, see <https://www.gnu.org/licenses/>' // CR // CR &
                // 'The files and libraries from the Open Source Products ' // CR // CR &
                // '   Gsl with fortran bindings (fgsl)' // CR &
                // '   GTK-Fortran' // CR &
                // '   GTK+ 3' // CR &
                // '   Glade' // CR &
                // '   PLplot ' // CR // CR &
                // 'being used when freely working with the program UncertRadio and which ' // CR &
                // 'were used for programming it, underly GNU GPL licenses ' &
                // '(see <https://www.gnu.org/licenses/>). ' // CR &
                // 'See also the infos regarding these products. ' // CR &
                // 'Freely usable tools with copyright are' // CR // CR &
                // '    MSYS2'  // CR  // c_null_char, &
                license_type=GTK_LICENSE_GPL_3_0, &
                comments=comment_str, &
                authors=authors, &
                website=url_str, &
                version=trim(UR_version_tag)// CR // trim(UR_git_hash)//c_null_char, &     ! defined in Uncw_Init
            ! documenters=["G. Kanisch", "F. Ober"], &
            ! artists=,   &
                logo=UR2Logo      &
            !parent  &
                )

          case ('About_Glade')
            call hl_gtk_about_dialog_show(    &
                name='Glade Interface Designer'//c_null_char, &
            ! license='License' // c_null_char, &
                license='This program is licensed under the terms of the ' // CR &
                // 'GNU General Public License version 2 or later' // CR // CR &
                // 'Available online under:' // CR &
                // ' http://www.gnu.org/licenses/old-licenses/gpl-2.0' // c_null_char, &          ! GK
                license_type=GTK_LICENSE_CUSTOM, &
            !  license_type=GTK_LICENSE_GPL_3_0, &
                comments='A user interface designer for GTK+ and GNOME.'//c_null_char, &
            !authors=[trim(FLTU('G. Kanisch, formerly at the Thünen Institute of Fisherries Ecology, Hamburg')) &
            !                                                                                //c_null_char], &
            ! website='http://glade.gnome.org'//c_null_char, &
                website_label='http://glade.gnome.org'//c_null_char, &
            ! copyright='copyright'//c_null_char, &
                version='3.40.0'//c_null_char &
            ! documenters=['G. Kanisch'//c_null_char] &
            ! translators='translators'//c_null_char &
            ! artists,   &
            ! logo,      &
            !parent  &
                )

            !   case ('About_CB')
            !     call hl_gtk_about_dialog_show(    &
            !          name='Code::Blocks'//c_null_char, &
            !          license='This program is licensed under the terms of the ' // CR &
            !            // 'GNU General Public License version 3' // CR // CR &
            !            // 'Available online under:' // CR &
            !            // ' http://www.gnu.org/licenses/gpl-3.0.html' // c_null_char, &          ! GK
            !          ! license_type=GTK_LICENSE_CUSTOM, &
            !          license_type=GTK_LICENSE_GPL_3_0, &
            !          comments='The open source, cross-platform IDE.' // CR &
            !          !  // 'The GNU Fortran 95 compiler (mingw GFortran; 7.3.0) is applied (32 bit)' //c_null_char, &
            !            // 'The GNU Fortran compiler (msys2/mingw64 GFortran; 13.1.0) is applied (64 bit)' //c_null_char, &
            !          !authors=[trim(FLTU('G. Kanisch, formerly at the Thünen Institute of Fisheries Ecology, Hamburg')) &
            !          !                                                                                //c_null_char], &
            !          ! website='http://www.codeblocks.org'//c_null_char, &
            !          website_label='http://www.codeblocks.org'//c_null_char, &
            !          ! copyright='copyright'//c_null_char, &
            !          ! version='13.12'//c_null_char &
            !          ! version='16.01'//c_null_char &
            !          ! version='17.12'//c_null_char &
            !          version='20.03'//c_null_char &
            !          ! documenters=['G. Kanisch'//c_null_char] &
            !          ! translators='translators'//c_null_char &
            !          ! artists,   &
            !          ! logo,      &
            !          !parent  &
            !          )

          case ('About_PLPLOT')
            call hl_gtk_about_dialog_show(    &
                name='PLplot'//c_null_char, &
            !license='License' ////c_null_char, &
                license='This program is licensed under the terms of the ' // CR &
                // 'GNU General Public License version 2' // CR // CR &
                // 'Available online under:' // CR &
                // ' http://www.gnu.org/licenses/old-licenses/gpl-2.0' // c_null_char, &          ! GK
            ! license_type=GTK_LICENSE_CUSTOM, &
                license_type=GTK_LICENSE_GPL_3_0, &
                comments='Cross-platform Plotting Library, with Fortran 95 interface.'//c_null_char, &
            !authors=[trim(FLTU('G. Kanisch, formerly at the Thünen Institute of Fisheries Ecology, Hamburg')) &
            !                                                                                //c_null_char], &
            ! website='http://plplot.sourceforge.net/'//c_null_char, &
                website_label='http://plplot.sourceforge.net'//c_null_char, &
            ! copyright='copyright'//c_null_char, &
            ! version='5.11.2'//c_null_char &
                version='5.15.0'//c_null_char &
            ! documenters=['G. Kanisch'//c_null_char] &
            ! translators='translators'//c_null_char &
            ! artists,   &
            ! logo,      &
            !parent  &
                )

          case ('About_GTK_Fortran')
            call hl_gtk_about_dialog_gtk_fortran()             ! (parent)

          case ('About_GTK')
            write(versgtk,'(i0,a1,i0,a1,i0)') gtk_get_major_version(),'.', gtk_get_minor_version(),'.', &
                gtk_get_micro_version()
            call hl_gtk_about_dialog_show(    &
                name='GTK+ Project'//c_null_char, &
            !license='License' ////c_null_char, &
                license='This program is licensed under the terms of the ' // CR &
                // 'GNU Lesser General Public License version 2.1' // CR // CR &
                // 'Available online under:' // CR &
                // ' http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html' // c_null_char, &          ! GK
            ! license_type=GTK_LICENSE_CUSTOM, &
            ! license_type=GTK_LICENSE_GPL_3_0, &
                comments='GTK+, or the GIMP Toolkit, is a multi-platform toolkit for ' // CR &
                // 'creating graphical user interfaces. Offering a complete set ' // CR &
                // 'of widgets, GTK+ is suitable for projects ranging from small ' // CR &
                // 'one-off tools to complete application suites.'//  CR // CR  &
                // 'GTK+ is a free software cross-platform graphical library ' // CR &
                // 'available for Linux, Unix, Windows and MacOs X.' // c_null_char, &
            !authors=[trim(FLTU('G. Kanisch, formerly at the Thünen Institute of Fisheries Ecology, Hamburg')) &
            !                                                                                //c_null_char], &
            ! website='https://www.gtk.org/'//c_null_char, &
                website_label='https://www.gtk.org'//c_null_char, &
            ! copyright='copyright'//c_null_char, &
            ! version='3.22.8-1'//c_null_char &
            !!      version='3.22.30-1'//c_null_char &
            !  version='3.22.26'//c_null_char &
                version=trim(versgtk)//c_null_char &
            ! documenters=['G. Kanisch'//c_null_char] &
            ! translators='translators'//c_null_char &
            ! artists,   &
            ! logo,      &
            !parent  &
                )

          case ('About_MSYS2')
            call hl_gtk_about_dialog_show(    &
                name='MSYS2'//c_null_char, &
                license='The licenses of those tools apply, which are installed by MSYS2' // c_null_char, &
            ! license_type=GTK_LICENSE_CUSTOM, &
                license_type=GTK_LICENSE_GPL_3_0, &
                comments='MSYS2 is a software platform with the aim of better interoperability ' &
                // 'with native Windows software.' // CR // CR &
                // 'Actual Windows compatible versions of the gfortran compiler, the' // CR &
                // ' GTK3 library and the Glade Interface Designer ' &
                // 'are available as MSYS2 download packages. ' // c_null_char, &
            !authors=[trim(FLTU('G. Kanisch, formerly at the Thünen Institute of Fisheries Ecology, Hamburg')) &
            !                                                                                //c_null_char], &
            ! website='http://msys2.github.io/'//c_null_char &
            ! website_label='http://msys2.github.io'//c_null_char &
                website_label='https://www.msys2.org/wiki/Home/'//c_null_char &    ! 4.8.2018
            ! copyright='copyright'//c_null_char, &
            ! version='3.22.1'//c_null_char &
            ! documenters=['G. Kanisch'//c_null_char] &
            ! translators='translators'//c_null_char &
            ! artists,   &
            ! logo,      &
            !parent  &
                )

          case ('About_InnoSetup')
            call hl_gtk_about_dialog_show(    &
                name='Inno Setup'//c_null_char, &
            ! license='License' // c_null_char, &
                license='This program with copyright is freely usable, also for commercial purpose. ' // CR &
                // 'See: Jordan Russel''s Software at:'  // CR &
                // ' http:/ /www.jrsoftware.org/isinfo.php' // c_null_char, &          ! GK
                license_type=GTK_LICENSE_CUSTOM, &
            !  license_type=GTK_LICENSE_GPL_3_0, &
                comments='A Html/Word converter to Compiled HTML Help v1.7.3.'//c_null_char, &
            !authors=[trim(FLTU('G. Kanisch, formerly at the Thünen Institute of Fisherries Ecology, Hamburg')) &
            !                                                                                //c_null_char], &
            ! website='http://glade.gnome.org'//c_null_char, &
                website_label='http://www.jrsoftware.org/isinfo.php/'//c_null_char, &
            ! copyright='copyright'//c_null_char, &
                version='6.2.1'//c_null_char &
            ! documenters=['G. Kanisch'//c_null_char] &
            ! translators='translators'//c_null_char &
            ! artists,   &
            ! logo,      &
            !parent  &
                )

          case ('Help_UR')
            call DisplayHelp(ncitem)

          case ('Exchange2Symbols')
            dialogstr = 'dialog_symbExchg'
            ioption = 67
            call FindItemS(trim(dialogstr), ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case ('TBmeansMD')
            ioption = 69
            dialogstr = 'dialogMeanData'
            call FindItemS(dialogstr, nci)
            !write(66,*) 'nvarsMD=',nvarsMD
            !write(66,*) 'meanID(1)=',meanID(1)
            ! call gtk_widget_set_sensitive(idpt('TBRemoveGridLine'), 1_c_int)
            call Loadsel_diag_new(1, nci)
            goto 9000

          case ('SerialEval')
            ioption = 70
            dialogstr = 'dialogSerEval'
            call FindItemS('dialogSerEval', ncitem2)
            write(66,*) 'dialogSerEval: ncitem2=',ncitem2
            bat_serial = .true.
            call Loadsel_diag_new(1, ncitem2)

            if(ifehl == 1) goto 9000
            call Batch_proc()
            QUITprog = .TRUE.
            call ClearMCFields(0)
            goto 9000

          case ('BinPoiPars')
            ioption = 71
            dialogstr = 'dialog_BinPoi'
            call FindItemS('dialog_BinPoi', ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            goto 9000

          case ('BatestUser')
            ioption = 72
            dialogstr = 'dialog_Batest'
            call FindItemS('dialog_Batest', ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            if(ifehl == 1) goto 9000

            batest_user = .true.
            call Batest()
            ! write(0,*) 'PMD: after call Batest'
            batest_user = .false.
            goto 9000

          case ('BatFiles')
            ioption = 73
            dialogstr = 'dialogBatEval'
            call FindItemS('dialogBatEval', ncitem2)
            write(66,*) 'dialogBatEval: ncitem2=',ncitem2,' BatFiles'
            bat_serial = .false.
            batf = .true.
            call Loadsel_diag_new(1, ncitem2)

            write(66,*) 'BatFiles: ifehl=',ifehl
            if(ifehl == 1) goto 9000
            call Batch_proc()
            QUITprog = .TRUE.
            call ClearMCFields(0)
            goto 9000

          case ('TBDistribDialog')
            if(.not.use_DP) goto 9000
            IF(trim(actual_GRID) /= 'treeview2') goto 9000
            numrows_marked = hl_gtk_listn_get_selections(idpt(actual_grid), rownums_marked)
            if(numrows_marked /= 1) goto 9000
            if(numrows_marked == 1) top_selrow = minval(rownums_marked) + 1
            if(IVTL(top_selrow) < 9) goto 9000

            ioption = 74
            dialogstr = 'dialog_distributions'
            ! call FindItemS('dialogBatEval', ncitem2)
            call FindItemS('dialog_distributions', ncitem2)
            call Loadsel_diag_new(1, ncitem2)
            goto 9000

          case ('URfunctions')
            dialogstr = 'dialog_infoFX'
            ioption = 75

            call FindItemS(trim(dialogstr), ncitem2)
            ! write(66,*) 'PMD: URfunctions  arrived     ncitem2=',ncitem2
            call Loadsel_diag_new(1, ncitem2)
            IF(ifehl == 1) goto 9000

          case default
        end select
        goto 9000
!---------------------------------------------------------------------

      case ('GtkNotebook')
        if(prout) write(66,*) 'PMD: GtkNotebook:   arrived.    Signal=',trim(signal), &
            '   NBprevious=',nbpreviousPage, &
            '  NBcurrent=',nbcurrentpage

        select case (trim(signal))

          case ('switch-page', 'change-current-page')

            Ident1 = NBpreviousPage
            Ident2 = NBcurrentpage
            if(prout) write(66,*) 'Notebook1:  switch-page:  arrived;     NBpreviousPage=',int(NBpreviousPage,2), &
                ' NBcurrentPage=',int(NBcurrentPage,2)
            if(consoleout_gtk) write(0,'(a,2(a,i0),a,L1)') 'Notebook1:  switch-page:  arrived; ',  &
                ' NBpreviousPage=',NBpreviousPage, &
                ' NBcurrentPage=',NBcurrentPage
            ! write(66,*) 'Ident1=',int(ident1,2),'  Ident2=',int(ident2,2)

            if(Ident2 > Ident1 .and. Ident2 < 5) then
                IF(langg == 'DE') call WrStatusBar(4,'Rechnet.... ' )
                IF(langg == 'EN') call WrStatusBar(4,'Calculating.... ' )
                IF(langg == 'FR') call WrStatusBar(4,'Calcule.... ' )
            end if

            call NBlabelmodify()
            if(NBcurrentPage == 3) call ExpandTV2Col7(.true.)
            if(NBcurrentPage /= 3) call ExpandTV2Col7(.false.)

            if(NBcurrentPage == 5) then
                call gtk_widget_set_visible(idpt('TRButtonHelp'), 1_c_int)
                call gtk_widget_set_visible(idpt('TRbuttonSavecsv'), 1_c_int)
            else
                call gtk_widget_set_visible(idpt('TRButtonHelp'), 0_c_int)
                call gtk_widget_set_visible(idpt('TRbuttonSavecsv'), 0_c_int)
            end if

            call gtk_widget_set_sensitive(idpt('TBDistribDialog'), 0_c_int)
            if(NBcurrentPage == 3) then
                if(use_DP) then               !  nmxDist > 0) then
                    call gtk_widget_set_sensitive(idpt('TBDistribDialog'), 1_c_int)
                end if
            end if


!+++++++++++++++++++++++++++
            ! IDENT1: previous TAB; IDENT2: actual TAB
            ! write(66,*) 'PMD:  Ident1, ident2=',ident1,ident2

            IF(IDENT2 /= 5 .and. IDENT1 == 5) THEN
                ! Save the position of the MC window, before it will be hided
                if(c_associated(windowPL) .and. winPL_shown) then
                    rootx = c_null_ptr
                    rooty = c_null_ptr
                    call gtk_window_get_position(windowPL,c_loc(rootx),c_loc(rooty))
                    write(cpos,'(i4)') rootx
                    read(cpos,*,iostat=ios) k
                    if(ios /= 0) then
                        ifehl = 1
                        goto 9000
                    end if
                    if(k > 0) posx = k
                    write(cpos,'(i4)') rooty
                    read(cpos,*,iostat=ios) k
                    if(ios /= 0) then
                        ifehl = 1
                        goto 9000
                    end if
                    if(k > 0) posy = k        !
                    write(66,*) 'windowPL: Hide:  posx,posy=',posx,posy
                    call gtk_widget_hide(windowPL)
                end if
            end if

!----  --------------
            ! actual TAB:
            select case (IDENT2)

              case (1)
                IF(langg == 'DE') call WrStatusbar(4,   &
                    'Beschreibung als Text eingeben, dann: TAB Gleichung')
                IF(langg == 'EN') call WrStatusbar(4,   &
                    'Describe procedure by text, then: TAB "Equations" ')
                IF(langg == 'FR') call WrStatusbar(4,   &
                    'Décrivez la procédure par le texte, puis: TAB "Equations"')
                goto 9000

              case (2)
                ! write(66,*) 'ProcMainDiag:  case(2): Anfang  '

                loadProV = loadingPro

                if(simul_ProSetup) then
                    loadingPro = .false.
                end if

                IF(KnumEGr == 0) THEN
                    kEGr = 1
                    ioption = 6
                    dialogstr = 'dialog_numegr'
                    call FindItemS(trim(dialogstr), ncitem2)
                    call Loadsel_diag_new(1, ncitem2)
                    IF(ifehl == 1) goto 9000
                end if
                ! if(project_loadw) loadingpro = .true.
                if(simul_ProSetup) loadingpro = .true.
                if(simul_ProSetup) write(66,*) 'PMD_ 1877:  loadingPro=',loadingPro,'project_loadw=',project_loadw

                if(kEGr > 0 .and. .not.Gum_restricted .and. .not.FitDecay .and. .not.Gamspk1_Fit) then
                    if(knetto(kEGr) > 0) call WDSetComboboxAct('comboboxNetRate', knetto(kEGr))
                    if(kbrutto(kEGr) > 0) call WDSetComboboxAct('comboboxGrossRate', kbrutto(kEGr))
                end if

                if(.not.loadingPro) then
                    call gtk_tree_view_columns_autosize(idpt('treeview1'))
                    call pending_events
                end if
                if(simul_ProSetup) then
                    call copyEquats()
                    kEGr = 1
                    ! ifehl = 1
                end if

              case (3)
                if(.not.loadingPro) then
                    do i=1,50
                        call WTreeViewSetColorRow('treeview3', i, table_bg)
                    end do
                    nt = 0
                    do j=1,ntvs
                        if('treeview3' == tvnames(j)%s) then; nt = j; exit; end if
                    end do
                    call TVtrimCol_width(tvnames(nt)%s)
                    ! call gtk_tree_view_columns_autosize(idpt('treeview3'))
                    do j=1,tvcols(nt)
                        exit
                        write(chcol,'(i0)') tvcolindex(nt,j)
                        write(66,*) tvnames(nt)%s,' : width col',int(j,2),': ',  &
                            gtk_tree_view_column_get_width(idpt('treeviewcolumn'// trim(adjustL(chcol))))
                    end do
                    call gtk_tree_view_columns_autosize(idpt('treeview2'))
                    call gtk_widget_show_all(idpt('treeview3'))
                end if
                TAB_VALUNC_Grid = .true.
                uncval_exists = .true.          ! 9.4.2023

              case (4)

                if(.not.loadingPro) then
                    do i=1,ngrs+5
                        call WTreeViewSetColorRow('treeview4', i, table_bg)
                    end do

                    do i=1,ntvs
                        if(tvnames(i)%s == 'treeview2') k2 = i
                        if(tvnames(i)%s == 'treeview4') k4 = i
                    end do
                    do i=1,4
                        tv_colwidth_digits(k4,i) = tv_colwidth_digits(k2,i)
                    end do
                    do i=5,8
                        tv_colwidth_digits(k4,i) = 14
                    end do
                    call TVtrimCol_width('treeview4')
                    call gtk_widget_show_all(idpt('treeview4'))
                end if
                if(.not.loadingPro) call gtk_tree_view_columns_autosize(idpt('treeview4'))

              case (5)
                if(.not.loadingPro) call pending_events
                call gtk_widget_set_sensitive(idpt('CheckUnits'), 1_c_int)
                !call gtk_window_unmaximize(idpt('window1'))
                ! call gtk_widget_get_allocation(idpt('window1'),c_loc(alloc))

                ! alloc%height = int(0.6_rn*real(scrheight_max-scrheight_min,rn))
                !  call gtk_widget_set_size_request(idpt('window1'),alloc%width,alloc%height-30_c_int)
                ! call gtk_window_resize(idpt('window1'),alloc%width,alloc%height)

                ! call gtk_window_set_default_size(idpt('window1'),alloc%width,alloc%height-30_c_int)

                call WrStb_Ready()

              case default
            end select
!---------------------------
            if(ident1 == 0) goto 110
            if(kEGr == 0) goto 9000
            ! previous TAB:
            select case (IDENT1)

              case (1)        ! TAB Titel
                if(Ident2 == 2) then
                    call WDGetTextviewString('textview1', Titeltext)
                    if(prout .and. size(Titeltext) > 0) then
                        WRITE(66,*) 'ProcMainDiag: after Get: Titeltext:'
                        do i=1,ubound(Titeltext,dim=1)
                            WRITE(66,*) Titeltext(i)%s
                        end do
                    end if
                    IF(langg == 'DE') call WrStatusBar(4, &
                        trim('Gleichungen eingeben, dann Button "Symbole aus Gleichungen laden:"' ))
                    IF(langg == 'EN') call WrStatusBar(4, &
                        trim('Enter equations, then Button "Load symbols from equations:"' ))
                    IF(langg == 'FR') call WrStatusBar(4, &
                        trim('Entrez les équations, puis le bouton "Charger les symboles des équations:"' ))
                end if

              case (2)     ! TAB Equation (Gleichung)

                IF(IDENT2 == 3) THEN            ! TAB UNCVAL

                    write(str1,'(a,i1,a,i1)') 'NB=',ident2,' kEgr=',kEgr
                    if(.not.loadingpro) call pending_events()
                    call pending_events()
                    ! call Wrstatusbar(3,trim(str1))
                    ngrs_CP = ngrs+ncov+numd

                    call WDPutEntryString('entryActiveKegr', Symbole(kEGr)%s)
                    IF(Gamspk1_Fit) THEN
                        if(.not.simul_ProSetup) call GamSymList()
                    end if
                    ngrsP = ngrs+ncov+numd
                    do i=1,ngrsP
                        if(i > size(Symbole)) then
                            call CharModA1(Symbole,i)
                        end if
                        if(i > size(SymboleG)) then
                            call CharModA1(SymboleG,i)
                        end if
                        symboleG(i)%s = ucase(symbole(i)%s)
                        if(i > size(IVTL)) then
                            call IntModA1(IVTL,i)
                        end if
                    end do
                    if(consoleout_gtk) write(0,*) ' before ListstoreFill_1: symbols:  ngrs=',int(ngrs,2), &
                        ' ngrsP=',int(ngrsP,2)
                    if(.not.batest_on .and. .not.automode .and. ngrsP > 0) then
                        call WDListstoreFill_1('liststore_symbols', ngrsP, Symbole)
                        if(Gamspk1_Fit) then
                        end if
                    end if

                    kxx = NumRowsTV('treeview2')
                    kxx = ngrs + ncov + numd

                    if(consoleout_gtk) write(0,*) 'PMD: before reading Treeview2:    kxx=',int(kxx,2)
                    call WTreeViewGetStrArray('treeview2', 2, kxx, Symbole_CP)
                    call WTreeViewGetStrArray('treeview2', 3, kxx, symtyp_CP)
                    call WTreeViewGetStrArray('treeview2', 4, kxx, einheit_CP)
                    call WTreeViewGetDoubleArray('treeview2', 5, kxx, Messwert_CP)
                    call WTreeViewGetComboArray('treeview2', 6, kxx, IVTL_CP)
                    call WTreeViewGetStrArray('treeview2', 7, kxx, sdformel_CP)
                    call WTreeViewGetDoubleArray('treeview2', 8, kxx, SDWert_CP)
                    call WTreeViewGetDoubleArray('treeview2', 9, kxx, HBreite_CP)
                    call WTreeViewGetComboArray('treeview2', 10, kxx, IAR_CP)
                    call WTreeViewGetDoubleArray('treeview2', 11, kxx, StdUnc_CP)
                    if(consoleout_gtk) write(0,*) 'PMD: After reading Treeview2:'

                    ngrs_CP = 0
                    do i=kxx,1,-1
                        IF(LEN_TRIM(symbole_CP(i)%s) > 0 .OR. abs(Messwert_CP(i)-missingval) > eps1min ) THEN
                            ngrs_CP = i
                            EXIT
                        end if
                    end do

                    do i=1,ngrs_CP
                        if(consoleout_gtk) write(0,*) 'PMD: clearing TV2: i,ngrs_CP=',int(i,2),int(ngrs_CP,2)

                        ! clear the cells in the columns 4-9 and 11, before they a bit further down
                        ! will be filled in a double do loop
                        IF(i <= knumEGr) CYCLE
                        !  write(0,*) 'do i=1,ngrs_cp:  i=',i
                        call WTreeViewPutDoubleCell('treeview2', 5, i, missingval)
                        call WTreeViewPutStrCell('treeview2', 7, i, '  ')
                        call WTreeViewPutDoubleCell('treeview2', 8, i, missingval)
                        call WTreeViewPutDoubleCell('treeview2', 9, i, missingval)
                        call WTreeViewPutComboCell('treeview2', 10, i, 1)
                        call WTreeViewPutDoubleCell('treeview2', 11, i, missingval)
                    end do
                    if(consoleout_gtk) write(0,*) 'PMD: before storing to Treeview2:   ngrs=',int(ngrs,2)

                    call WTreeViewPutStrArray('treeview2', 2, ngrs, symbole)
                    call WTreeViewPutStrArray('treeview2', 3, ngrs, symtyp)
                    call WTreeViewPutStrArray('treeview2', 4, ngrs, einheit)

                    icp_used = 0
                    if(prout) write(66,*) 'PMD: NB 2->3: determine icp_used:'
                    if(consoleout_gtk) write(0,*) 'PMD: NB 2->3: determine icp_used:'

                    IF(ngrs /= ngrs_CP) THEN
                        WRITE(66,*) '******* ProgMainDiag:  switched to UNC-TAB: ngrs ungleich ngrs_CP!'
                        WRITE(66,*) '       ngrs,ncov,numd=',int(ngrs,2),int(ncov,2),int(numd,2),'  ngrs_CP=',  &
                            int(ngrs_CP,2),'   nab,nmu=',int(nab,2),int(nmu,2)
                    end if
                    do i=1,ngrs+ncov+numd
                        ! if(consoleout_gtk) write(0,*) 'PMD: NB 2->3: i=',int(i,2)
                        kx = 0
                        do k=1,ngrs+ncov+numd
                            IF(i <= ngrs .AND. k <= ngrs_CP .and. ucase(symbole(i)%s) == ucase(symbole_CP(k)%s) ) THEN

                                if(consoleout_gtk) write(0,*) 'i=',int(i,2),' k=',int(k,2),'  ngrs=',int(ngrs,2),   &
                                    ' ngrs_CP=',int(ngrs_CP,2)
                                icp_used(i) = 1
                                kx = k
                                IF( abs(Messwert_CP(k)-missingval) > eps1min) then
                                    call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert_CP(k))
                                else if(abs(Messwert_CP(k)-missingval) < eps1min .and.  &
                                    abs(Messwert(i)-missingval) > eps1min) then
                                    call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert(i))
                                else
                                    call WTreeViewPutDoubleCell('treeview2', 5, i, missingval)
                                END IF

                                call WTreeViewPutComboCell('treeview2', 6, i, IVTL_CP(k))
                                IF(LEN_TRIM(sdformel_CP(k)%s) > 0 ) then
                                    if(allocated(xstr)) deallocate(xstr)
                                    allocate(character(len=1000) :: xstr)          ! 12.8.2023
                                    xstr = max(' ',sdformel_CP(k)%s)
                                    call WTreeViewPutStrCell('treeview2', 7, i, xstr)
                                elseif(LEN_TRIM(sdformel_CP(k)%s) == 0 .and. LEN_TRIM(sdformel(i)%s) > 0) then  ! 12.4.2019
                                    xstr = max(' ',sdformel(i)%s)
                                    call WTreeViewPutStrCell('treeview2', 7, i, xstr)
                                else
                                    xstr = max(' ',' ')
                                    call WTreeViewPutStrCell('treeview2', 7, i, xstr)
                                end if
                                IF(abs(sdwert_CP(k)-missingval) > eps1min) THEN
                                    call WTreeViewPutDoubleCell('treeview2', 8, i, SDWert_CP(k))
                                elseif(abs(sdwert_CP(k)-missingval) < eps1min .and. abs(sdwert(i)-missingval) > eps1min) THEN
                                    call WTreeViewPutDoubleCell('treeview2', 8, i, SDWert(i))
                                else
                                    call WTreeViewPutDoubleCell('treeview2', 8, i, missingval)
                                END IF
                                IF(abs(HBreite_CP(k)-missingval) > eps1min) THEN
                                    call WTreeViewPutDoubleCell('treeview2', 9, i, HBreite_CP(k))
                                elseif(abs(HBreite_CP(k)-missingval) < eps1min .and. abs(HBreite(i)-missingval) > eps1min) THEN
                                    call WTreeViewPutDoubleCell('treeview2', 9, i, HBreite(i))
                                else
                                    call WTreeViewPutDoubleCell('treeview2', 9, i, missingval)
                                end if
                                IF(IAR_CP(k) > 0 ) THEN   !
                                    call WTreeViewPutComboCell('treeview2', 10, i, IAR_CP(k))
                                elseif(IAR_CP(k) == 0 .and. IAR(i) > 0 ) THEN
                                    call WTreeViewPutComboCell('treeview2', 10, i, IAR(i))
                                end if
                                IF(abs(StdUnc_CP(k)-missingval) > eps1min ) THEN
                                    call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc_CP(k))
                                elseif(abs(StdUnc_CP(k)-missingval) < eps1min .and. abs(StdUnc(i)-missingval) > eps1min) THEN
                                    call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
                                else
                                    call WTreeViewPutDoubleCell('treeview2', 11, i, missingval)
                                end if
                            END IF
                        end do

                        IF(icp_used(i) == 0) THEN
                            call WTreeViewPutDoubleCell('treeview2', 5, i, missingval)
                            call WTreeViewPutComboCell('treeview2', 6, i, 1)
                            call WTreeViewPutStrCell('treeview2', 7, i, '  ')
                            call WTreeViewPutDoubleCell('treeview2', 8, i, missingval)
                            call WTreeViewPutDoubleCell('treeview2', 9, i, missingval)
                            call WTreeViewPutComboCell('treeview2', 10, i, 1)
                            call WTreeViewPutDoubleCell('treeview2', 11, i, missingval)
                        end if

                        IF(i <= nab) THEN
                            call WTreeViewPutComboCell('treeview2', 6, i, 1)
                            IF(KnumEGr == 1 .AND. i /= kbrutto(kEGr) ) then
                                if(len_trim(SDformel(i)%s) == 0) call WTreeViewPutStrCell('treeview2', 7, i, '  ')
                            end if
                            call WTreeViewPutDoubleCell('treeview2', 8, i, missingval)
                            call WTreeViewPutDoubleCell('treeview2', 9, i, missingval)
                            call WTreeViewPutComboCell('treeview2', 10, i, 1)
                        END IF
                    end do
                    if(prout) write(66,*) 'PMD: NB 2->3: after icp_used determined'
                    IF(ngrs_CP > ngrs) THEN
                        do i=ngrs+1,ngrs_CP
                            do k=2,11
                                if(k <= 4 .or. k == 7) call WTreeViewPutStrCell('treeview2', k, i, '  ')
                                if(k == 5 .or. k == 8 .or. k == 9 .or. k == 11 ) call WTreeViewPutDoubleCell('treeview2', k, i, missingval)
                                if(k == 6) call WTreeViewPutComboCell('treeview2', k, i, 1)
                                if(k == 10) call WTreeViewPutComboCell('treeview2', k, i, 1)
                            end do
                        end do
                    end if

                    do irow=1,ngrs + 7
                        if(.not.loadingPro) call pending_events()
                        do k=1,11
                            !if(.not.contrast_mode) then
                            !  call WTreeViewSetColorCell('treeview2',k, i, "#FFFFFF")
                            !else
                            call WTreeViewSetColorCell('treeview2',k, irow, table_bg)
                            !end if
                        end do
                    end do
                    kk = nab
                    if(FitDecay) then
                        if(kfitp(1) > 0 .and. knumEGr > 1) kk = nab + 3
                    end if
                    do i=1,kk               ! nab         ! 25.7.2023
                        if(.not.loadingPro) call pending_events()
                        call WTreeViewSetColorCell('treeview2',5, i, orange_bg)           ! orange
                        call WTreeViewSetColorCell('treeview2',6, i, orange_bg)
                        call WTreeViewSetColorCell('treeview2',7, i, orange_bg)
                        call WTreeViewSetColorCell('treeview2',8, i, orange_bg)
                        call WTreeViewSetColorCell('treeview2',9, i, orange_bg)
                        call WTreeViewSetColorCell('treeview2',10, i, orange_bg)
                        call WTreeViewSetColorCell('treeview2',11, i, orange_bg)
                        IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit .and. i == kbrutto(kEGr)  &
                            .and. .not.Gum_restricted .and. .not.var_brutto_auto) then
                            call WTreeViewSetColorCell('treeview2',7, kbrutto(kEGr), green_bg) ! green
                        end if

                    end do
                    if(loadingPro) call pending_events()
                    if(FitDecay) then
                        ! 25.7.2023:
                        do j=1,3
                            if(kfitp(1) > 0 .and. knumEGr > 1) then
                                !! if(ifit(j) == 2) then
                                if(.false. .and. ifit(j) == 2) then   ! 29.1.2024  should be bg color orange
                                    do i=5,11
                                        call WTreeViewSetColorCell('treeview2',i, kfitp(1)+j-1, table_bg)
                                    end do
                                else
                                    do i=5,11
                                        call WTreeViewSetColorCell('treeview2',i, kfitp(1)+j-1, orange_bg)
                                    end do
                                end if
                            end if
                        end do
                    end if

                    ! The following two statements guarantee that the IVTL cells and the
                    ! IAR cells, lying in the "red region" of the table, will be initialized;
                    ! especially, when beginning a new project.
                    call WTreeViewPutComboArray('treeview2', 6, nab, IVTL)
                    call WTreeViewPutComboArray('treeview2', 10, nab, IAR)

                    do i=nab+1,ngrs
                        call WTreeViewSetColorCell('treeview2',11, i, orange_bg)        ! orange
                        if(FitDecay) then      ! 25.7.2023:
                            if(kfitp(1) > 0 .and. knumEGr > 1) then
                                cycle
                            end if
                        end if
                        call WTreeViewSetColorCell('treeview2',7, i, table_bg)

                        IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit .and. i == kbrutto(kEGr)   &
                            .and. .not.Gum_restricted .and. .not.var_brutto_auto) then
                            call WTreeViewSetColorCell('treeview2',7, kbrutto(kEGr), green_bg)  ! green
                        end if
                        if(ucase(symtyp(i)%s) == 'M') then
                            call WTreeViewSetColorCell('treeview2',5, i, orange_bg)
                            if(i /= kbrutto(kEGr)) call WTreeViewSetColorCell('treeview2',7, i, orange_bg)
                            call WTreeViewSetColorCell('treeview2',8, i, orange_bg)
                            call WTreeViewSetColorCell('treeview2',9, i, orange_bg)
                            call WTreeViewSetColorCell('treeview2',10, i, orange_bg)
                        end if
                    end do

                    if(.not.loadingPro .and. .not.iteration_on)   &
                        call WDListstoreFill_1('liststore_symbols', ngrsP, Symbole)
                    do nrow=ngrs+1,ngrs+30
                        do ncol=2,11
                            call WDListstoreClearCell('treeview2',ncol,nrow)
                        end do
                    end do
                    if(.not.loadingPro) then
                        call gtk_tree_view_columns_autosize(idpt('treeview2'))
                        IF(langg == 'DE') call WrStatusBar(4, &
                            trim('beide Tabellen ausfüllen, dann Button "Berechnung der Unsicherheiten"'))
                        IF(langg == 'EN') call WrStatusBar(4, &
                            trim('fill out both tables, then Button "Calculation of uncertainties" '))
                        IF(langg == 'FR') call WrStatusBar(4, &
                            trim('remplir les deux tableaux, puis le bouton "Calcul des incertitudes" '))
                    end if
                    goto 9000
                END IF
!----------------------
              case (3)         ! TAB UNCVAL

                IF(IDENT2 == 4) THEN      ! TAB Budget

                    MCsim_on = .FALSE.
                    write(str1,'(a,i1,a,i1)') 'NB=',ident2,' kEgr=',kEgr
                    if(prout) write(66,*) trim(str1)
                    if(kEGr == 0) goto 9000

                    IF(langg == 'DE') call WDPutLabelStringBold('LBFrameBudget',      &
                        'Tabelle des Unsicherheiten-Budgets for '//symbole(kEGr)%s // ' :')
                    IF(langg == 'EN') call WDPutLabelStringBold('LBFrameBudget',   &
                        'Table of uncertainty budget for '//symbole(kEGr)%s // ' :')
                    IF(langg == 'FR') call WDPutLabelStringBold('LBFrameBudget',   &
                        'Tableau du budget d''incertitude pour '//symbole(kEGr)%s // ' :')

                    kmin = nab + 1
                    kanz = ngrs - nab
                    if(.not.FitDecay .and. .not. Gamspk1_Fit) then
                        IF(kbrutto(kEGr) <= nab .AND. kbrutto(kEGr) > 0) THEN
                            kmin = kbrutto(kEGr)
                            kanz = ngrs - kbrutto(kEGr) + 1
                        END IF
                    end if
                    IF(Gamspk1_Fit .and. numd/5 > 1 .and. ecorruse == 1) THEN
                        ncov = (numd/5)*(numd/5-1)/2
                        WRITE(66,'(a,i0,a)') 'PMD: ncov=',ncov,' set, derived from numd/5'
                    end if
                    ix = ubound(symbole,dim=1)
                    if(ix < ngrs+ncov) then
                        call CharModA1(symbole,ngrs+ncov)
                        call CharModA1(einheit,ngrs+ncov)
                        call CharModA1(symtyp,ngrs+ncov)
                        call CharModA1(bedeutung,ngrs+ncov)
                        call RealModA1(Messwert,ngrs+ncov)
                        call RealModA1(StdUnc,ngrs+ncov)
                    end if
                    write(66,'(a,i0)') 'PMD 2207:   ncov=',ncov
                    IF(FitDecay) THEN
                        call CharModA1(symbole,ngrs+ncov+numd)
                        call CharModA1(einheit,ngrs+ncov+numd)
                        call CharModA1(symtyp,ngrs+ncov+numd)
                        call CharModA1(bedeutung,ngrs+ncov+numd)
                        do i=1,numd
                            WRITE(chint,'(i3.3)') i
                            symbole(ngrs+ncov+i)%s = 'Rb_'//chint
                            einheit(ngrs+ncov+i)%s = '1/s '
                            symtyp(ngrs+ncov+i)%s = 'u'
                        end do
                    END IF
                    if(Gamspk1_Fit) then
                        call GamPeakvals()
                    end if
                    do i=1,ncov
                        symbole(ngrs+i)%s = 'cov(' // SymboleA(i)%s // ',' // SymboleB(i)%s // ')'
                        if(len_trim(symbole(ngrs+i)%s) > maxlen_symb) maxlen_symb = len_trim(symbole(ngrs+i)%s)
                        if(ngrs+i > ubound(einheit,dim=1)) then
                            call CharModA1(einheit,ngrs+i)
                            call CharModA1(symtyp,ngrs+i)
                        end if
                        einheit(ngrs+i)%s = '   '
                        symtyp(ngrs+i)%s = ' '
                        Messwert(ngrs+i) = CovarVal(i)
                        IF(abs(Messwert(ngrs+i)-missingval) < eps1min) Messwert(ngrs+i) = 0._rn
                    end do

                    if(prout) write(66,*) 'PMD: behind Label 133'
                    kmin = 1
                    kanz = ngrs+ncov+numd
                    if(numd > 0 .and. ubound(symbole,dim=1) < kanz) then
                        call CharModA1(symbole,ngrs+ncov+numd)
                        call CharModA1(einheit,ngrs+ncov+numd)
                        call CharModA1(symtyp,ngrs+ncov+numd)
                        call CharModA1(bedeutung,ngrs+ncov+numd)
                    end if
                    ngrsP = kanz
                    WRITE(str1,*) 'WGridRows: ngrs,ncov,num=',int(ngrs,2),int(ncov,2),int(numd,2)
                    if(consoleout_gtk) write(0,*) 'before PutStrArray(TV4), Sp.2-4,  kmin=',int(kmin,2)
                    if(numd > 0 .and. ubound(Messwert,dim=1) < kanz) then
                        call RealModA1(Messwert,ngrs+ncov+numd)
                        call RealModA1(StdUnc,ngrs+ncov+numd)
                    end if
                    if(consoleout_gtk) write(0,*) 'nach PutStrArray(TV4), Sp.2-4'
                    call WDListstoreFill_table('liststore_budget',3,.true.)

                    IF(langg == 'DE') call WrStatusBar(4,'Rechnet.... ' )
                    IF(langg == 'EN') call WrStatusBar(4,'Calculating.... ' )
                    IF(langg == 'FR') call WrStatusBar(4,'Calcule.... ' )

                    call EraseNWGfields()
                    if(prout) write(66,*) 'PMD: before call Rechw2'
                    if(consoleout_gtk) write(0,*) 'before Rechw2'
                    !---------------------------------
                    call Rechw2()

                    !---------------------------------
                    if(prout) write(66,*) 'PMD: after call Rechw2'

                    IF(ifehl == 1) then
                        if(.not.loadingPro) call pending_events
                        goto 9000
                    end if
                    call gtk_widget_set_sensitive(idpt('NBResults'), 1_c_int)
                    call gtk_widget_set_visible(idpt('NBResults'), 1_c_int)
                    call gtk_widget_set_sensitive(idpt('grid5'), 1_c_int)
                    if(.not.loadingPro) call gtk_widget_show(idpt('grid5'))

                    do i=1,knumEGr
                        IF(i == kEGr) THEN
                            sensi(i) = 0._rn
                            perc(i) = percsum
                        else
                            sensi(i) = 0.0_rn
                            perc(i)  = 0.0_rn
                        end if
                    end do
                    if(consoleout_gtk) write(0,*) 'PMD: behind call Rechw2:  before WDliststoreFill_table, 3'
                    call WDListstoreFill_table('liststore_budget',3, .true.)
                    if(.not.loadingPro) call pending_events
                    if(prout) write(66,*) 'PMD: behind call Rechw2:  behind WDliststoreFill_table'
                    if(consoleout_gtk) write(0,*) 'PMD: behind call Rechw2:  behind WDliststoreFill_table, 3'

                    if(.not.loadingPro) then
                        do i=1,ngrs+5
                            call WTreeViewSetColorRow('treeview4', i, table_bg)
                        end do
                    end if

                    IF(langg == 'DE') call WDPutLabelStringBold('TRLBFrameMessErg',   &
                        'Gesamtes Messergebnis für ' //symbole(kEGr)%s//' :' )
                    IF(langg == 'EN') call WDPutLabelStringBold('TRLBFrameMessErg',   &
                        'Final measurement result for ' //symbole(kEGr)%s//' :' )
                    IF(langg == 'FR') call WDPutLabelStringBold('TRLBFrameMessErg',   &
                        'Résultat de mesure final pour ' //symbole(kEGr)%s//' :' )

                    call WDPutEntryDouble('TRentryValue', Resultat, frmtres)
                    call WDPutEntryDouble('TRentryUnc', Ucomb, frmtres)
                    Ucrel = 0._rn
                    IF(Resultat > 0._rn) Ucrel = Ucomb/Resultat*100._rn
                    call WDPutEntryDouble('TRentryUncPC', Ucrel, frmtres)
                    if(consoleout_gtk) write(0,*) 'PMD: behind call Rechw2:  behind TRentryUncPC'

                    call WDPutLabelString('TRLBUnit1', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit2', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit3', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit4', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit5', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit6', Einheit(1)%s)

                    call WDPutLabelString('TRLBUnit7', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit8', Einheit(1)%s)

                    call WDPutLabelString('TRLBUnit22', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit21', Einheit(1)%s)

                    call WDPutLabelString('TRLBUnit9', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit10', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit11', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit12', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit13', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit14', Einheit(1)%s)

                    call WDPutLabelString('TRLBUnit15', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit16', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit17', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit18', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit19', Einheit(1)%s)
                    call WDPutLabelString('TRLBUnit20', Einheit(1)%s)

                    if(consoleout_gtk) write(0,*) 'PMD: after call Rechw2:  after WDPutLabelStr'
                    IF(langg == 'DE') call WDPutLabelStringBold('TRlabFrDL',   &
                        'Erkennungs- und Nachweisgrenze für ' //symbole(kEGr)%s//' :' )
                    IF(langg == 'EN') call WDPutLabelStringBold('TRlabFrDL',   &
                        'Decision threshold and detection limit for ' //symbole(kEGr)%s//' :' )
                    IF(langg == 'FR') call WDPutLabelStringBold('TRlabFrDL',   &
                        'Seuil de décision et limite de détection pour ' //symbole(kEGr)%s//' :' )

                    call WDPutEntryDouble('TRentryCoverf', coverf, '(f5.3)')
                    call WDPutEntryDouble('TRentryDT', decthresh, frmtres_min1)
                    call WDPutEntryDouble('TRentryDL', detlim, frmtres_min1)

                    IF(langg == 'DE') WRITE(str1,'(a,i3)') 'Iterationen: ',nit_decl
                    IF(langg == 'EN') WRITE(str1,'(a,i3)') 'Iterations: ',nit_decl
                    IF(langg == 'FR') WRITE(str1,'(a,i3)') 'Itérations: ',nit_decl
                    xstr = max(' ',trim(str1))
                    call WDPutLabelString('TRlabDTIteras', xstr)
                    IF(langg == 'DE') WRITE(str1,'(a,i3)') 'Iterationen: ',nit_detl
                    IF(langg == 'EN') WRITE(str1,'(a,i3)') 'Iterations: ',nit_detl
                    IF(langg == 'FR') WRITE(str1,'(a,i3)') 'Itérations: ',nit_detl
                    xstr = max(' ',trim(str1))
                    call WDPutLabelString('TRlabDLIteras', xstr)
                    call WDPutLabelString('TRlabMessage', ' ')

                    if(prout) write(66,*) 'PMD: behind call Rechw2:  before GamSpk1_Fit'
                    if(consoleout_gtk) write(0,*) 'PMD: after call Rechw2:  before GamSpk1_Fit'
                    IF(.false. .and. Gamspk1_Fit) THEN
                        IF(langg == 'DE') WRITE(str1,'(a,es11.4)') trim('Näherungsformel für NWG: '),detlim_approximate
                        IF(langg == 'EN') WRITE(str1,'(a,es11.4)') trim('Approx. formula for det. limit: '),detlim_approximate
                        IF(langg == 'FR') WRITE(str1,'(a,es11.4)') trim('Formule approximative pour limite de détection: '),detlim_approximate
                        call WDPutLabelString('TRlabMessage', trim(str1))
                    end if

                    IF(nit_detl >= nit_detl_max) THEN
                        IF(langg == 'DE') call WDPutLabelString(   &
                            'TRlabMessage', 'Nachweisgr.:    Iteration nicht konvergent')
                        IF(langg == 'EN') call WDPutLabelString(  &
                            'TRlabMessage', 'Det.Limit:    Iteration non-convergent')
                        IF(langg == 'FR') call WDPutLabelString(  &
                            'TRlabMessage', 'Limite Det.:    Itération non convergente')
                        call WDPutLabelColorF('TRlabMessage',GTK_STATE_FLAG_NORMAL, 'red')
                    elseif(nit_detl == 0 .and. .not. Gum_restricted) then
                        IF(langg == 'DE') call WDPutLabelString(   &
                            'TRlabMessage', 'Nachweisgr.:    Nicht berechenbar')
                        IF(langg == 'EN') call WDPutLabelString(  &
                            'TRlabMessage', 'Det.Limit:    Cannot be calculated')
                        IF(langg == 'FR') call WDPutLabelString(  &
                            'TRlabMessage', 'Limite Det.:    Ne peut pas être calculé')
                        call WDPutLabelColorF('TRlabMessage',GTK_STATE_FLAG_NORMAL, 'red')
                    else
                        call WDPutLabelColorF('TRlabMessage',GTK_STATE_FLAG_NORMAL, 'black')
                    end if
                    WRITE(str1,'(a,f5.3,a,f5.3)') 'k_alpha=',kalpha,',  k_beta=',kbeta
                    do i=1,len_trim(str1)
                        if(str1(i:i) == '.') str1(i:i) = sDecimalPoint
                    end do
                    call WDPutLabelString('TRlabQuantiles', trim(str1))

                    IF(langg == 'DE') call WDPutLabelString('TRlabMethod','Methode: '//TRIM(NWGMeth))
                    IF(langg == 'EN') call WDPutLabelString('TRlabMethod','Method: '//TRIM(NWGMeth))
                    IF(langg == 'FR') call WDPutLabelString('TRlabMethod','Méthode: '//TRIM(NWGMeth))

                    call WDPutEntryDouble('TRentryValueBy', WertBayes, frmtres)
                    call WDPutEntryDouble('TRentryUncBy', UcombBayes, frmtres)

                    call WDGetCheckButton('TRcheckbutton3', i)
                    if(i == 0) then
                        call WDPutEntryDouble('TRentryLQBy', KBgrenzu, frmtres)
                        call WDPutEntryDouble('TRentryUQBy', KBgrenzo, frmtres)
                    else
                        call WDPutEntryDouble('TRentryLQBy', KBgrenzuSH, frmtres)
                        call WDPutEntryDouble('TRentryUQBy', KBgrenzoSH, frmtres)
                    end if
                    call WDPutEntryDouble('TRentryGamma', W1minusG, '(f6.4)')

                    IF(Fitdecay .OR. Gamspk1_Fit) THEN
                        klu = klinf
                        IF(Gamspk1_Fit) klu = kgspk1
                        IF(kfitp(1) > 0) klu = kfitp(1)-1+kEGr
                        UcombLfit = StdUnc(klu)
                        ChisqrLfit = Chisqr
                        If(FitDecay) then
                            IF(langg == 'DE') call WDPutLabelStringBold('TRlabFRModel', trim(fitmeth) // &
                                ': Standardunsicherheiten des Fitparameters:')
                            IF(langg == 'EN') call WDPutLabelStringBold('TRlabFRModel', trim(fitmeth) // &
                                ': standard uncertainty of the fit parameter:')
                            IF(langg == 'FR') call WDPutLabelStringBold('TRlabFRModel', trim(fitmeth) // &
                                ': incertitude standard du paramètre d''ajustement:')
                        elseif(Gamspk1_Fit) then
                            IF(langg == 'DE') call WDPutLabelStringBold('TRlabFRModel', trim(mwtyp) //  &
                                ': Standardunsicherheiten des Fitparameters:')
                            IF(langg == 'EN') call WDPutLabelStringBold('TRlabFRModel', trim(mwtyp) //  &
                                ': standard uncertainty of the fit parameter:')
                            IF(langg == 'FR') call WDPutLabelStringBold('TRlabFRModel', trim(mwtyp) //  &
                                ': incertitude standard du paramètre d''ajustement:')
                        end if
                        IF(FitDecay .or. Gamspk1_Fit) call WDPutEntryDouble('TRentryUfit', StdUnc(klu), frmtres)
                        IF(FitDecay .OR. Gamspk1_Fit) call WDPutEntryDouble('TRentryUprop', UcombLinf_kqt1, frmtres)
                        IF(FitDecay) call WDPutEntryDouble('TRentryChisqr', Chisqr, '(f7.4)')
                        IF(Gamspk1_Fit) call WDPutEntryDouble('TRentryChisqr', gspk_Chisqr, '(f7.4)')

                        call WDPutLabelString('TrlabUfitUnit', Einheit(klu)%s)
                        call WDPutLabelString('TRlabUpropUnit', Einheit(klu)%s)
                    END IF
                    if(FitDecay) then
                        call WDPutEntryInt('TRentryMCanzM',10000)
                    else
                        call WDPutEntryInt('TRentryMCanzM',100000)
                    end if

                    if(.not.loadingPro) then
                        IF(langg == 'DE') call WrStatusBar(4,'zu TAB "Resultate" wechseln' )
                        IF(langg == 'EN') call WrStatusBar(4,'select TAB "Results"' )
                        IF(langg == 'FR') call WrStatusBar(4,'sélectionnez TAB "Résultats"' )
                    end if

                    if(prout) write(66,*) 'PMD: after call Rechw2:  before goto 9000'
                    if(consoleout_gtk) write(0,*) 'PMD: after call Rechw2:  before goto 9000'

                    if(.not.loadingPro) call gtk_tree_view_columns_autosize(idpt('treeview4'))
                    if(.not.loadingPro) call pending_events()
                    ! call pending_events     ! erzeugt viel output auf unit 0 !
                    if(consoleout_gtk) write(0,*) 'PMD: after call Rechw2:  after call pending_events'
                    if(.not.FitDecay .and. .not. Gamspk1_Fit) then
                        call gtk_widget_set_visible(idpt('frame9'),0_c_int)
                    else
                        call gtk_widget_set_visible(idpt('frame9'),1_c_int)
                    end if
                    goto 9000
                END IF

              case (4,5)
                if(IDENT2 == 2) then
                    ! nothing to do
                end if
                if(ident2 == 5) then
                    call gtk_container_check_resize(idpt('grid5'))
                    ! call gtk_window_unmaximize(idpt('window1'))

                    call gtk_widget_set_sensitive(idpt('CheckUnits'), 1_c_int)
                    call WrStb_Ready()
                end if

                if(batest_user .and. .not.apply_units) then
                    !!!!!!!!!!!!!!!!!!!! apply_units = .false.    ! omit this line
                    ! apply_units = .true. ! xxxxxxxxx must be set directly xxxxxxxxxxxxx
                    if(apply_units) then

                        !loadingpro = .true.
                        !call WDNotebookSetCurrPage('notebook1',3)
                        !!!!! call Restore_Ucheck
                        !call ProcessLoadPro_new(3,kEGr)
                        !loadingpro = .false.
                        do i=1,knumEGr
                            call WTreeViewGetDoubleCell('treeview2',i, 5, Messwert(i))
                            call WTreeViewGetDoubleCell('treeview2',i, 11, StdUnc(i))
                        end do
                        call Save_Ucheck()
                        ! call gtk_widget_set_sensitive(idpt('CheckUnits'), 0_c_int)
                        call ReadUnits()
                        call load_unit_conv(ngrs+ncov)
                        call CalcUnits()
                        call Report_Ucheck()
                        apply_units = .false.
                    end if
                    goto 9000
                end if


              case default
            end select



110         continue

!+++++++++++++++++++++++++++++++
          case default
        end select

!+++++++++++++++++++++++++++++++
      case ('GtkTreeView')


!+++++++++++++++++++++++++++++++
    end select

9000 continue

    if(allocated(rownums_marked)) deallocate(rownums_marked)
    if(allocated(SDformel_test)) deallocate(SDformel_test)
    if(allocated(FT1)) deallocate(FT1)

    if(prout) write(66,*) 'End of ProcMainDiag reached','  SaveP=',saveP,' idstring=',idstring
    if(consoleout_gtk) &
        write(0,*) '##### PMD  End  ###########################'

end subroutine ProcMainDiag

!#################################################################################

module subroutine GamSymList

    ! for an activitiy evaluation from few gamma lines, the lists (arraays) of symbols,
    ! symbol types and units are defined for the peaks (number numd), for the tables
    ! in Treeview2 (TAB values/uncertainties) and for the covariances between peaks
    ! for the table   ! in Treeview3 (covariance table). Only the covar-related
    ! definitions are transferred to Treeview3.
    !
    ! called from ProcMainDiag and TransferToGTK (RDsubs)

    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_params,          only: eps1min
    use UR_params,          only: rn
    use UR_Gleich,          only: ncov,ngrs,Symbole,IsymbA,IsymbB,CVFormel,Icovtyp,covarval, &
        Messwert,einheit,symtyp,StdUnc,SymboleA,SymboleB,missingval, &
        MesswertSV,STDUncSV,bedeutung

    use UR_Gspk1Fit,        only: ecorruse
    use UR_linft,           only: numd
    use top,                only: CharModA1,RealModA1,IntModA1
    use CHF,                only: ucase
    use Rout,               only: WTreeViewPutComboArray

    implicit none

    integer(4)          :: nhh,i,ncc,ix,j,k,nnn
    character(len=10)   :: chint
    character(len=100)  :: syA,syB


    if(numd == 0) return
!------------------------------------------------------------------

    !  write(66,*) 'GSL: ubound(Messwert)=',ubound(Messwert,dim=1)

    if(ecorruse == 1) then
        ncov = (numd/5)*(numd/5-1)/2
        if(.not.allocated(isymbA) .or. ubound(IsymbA,dim=1) == 0) then
            call IntModA1(IsymbA,ncov)
            call IntModA1(IsymbB,ncov)
            call IntModA1(Icovtyp,ncov)
            call CharModA1(CVFormel,ncov)
            call RealModA1(Covarval,ncov)
        end if
    end if

    nnn = ngrs+ncov+numd
    ix = ubound(symbole,dim=1)
    !write(66,*) 'GamSymList: ix=',ix,' nnn=',nnn,' numd=',numd
    call CharModA1(Symbole,nnn)
    call CharModA1(symtyp,nnn)
    call CharModA1(einheit,nnn)
    call CharModA1(bedeutung,nnn)

    ix = ubound(Messwert,dim=1)
    call RealModA1(Messwert,nnn)
    call RealModA1(StdUnc,nnn)

    if(allocated(MesswertSV)) deallocate(MesswertSV)
    if(allocated(StdUncSV)) deallocate(StdUncSV)
    allocate(MesswertSV, source=Messwert)
    allocate(StdUncSV, source=StdUnc)

    do i=1,ncov
        if(i > ubound(SymboleA,dim=1)) call CharModA1(SymboleA,i)
        if(i > ubound(SymboleB,dim=1)) call CharModA1(SymboleB,i)
        symbole(ngrs+i)%s = 'cov(' // SymboleA(i)%s // ',' // SymboleB(i)%s // ')'
        einheit(ngrs+i)%s = '   '
        symtyp(ngrs+i)%s = ' '
        if(ubound(covarval,dim=1) == 0) then
            Messwert(ngrs+i) = missingval
        else
            Messwert(ngrs+i) = CovarVal(i)
            IF(abs(Messwert(ngrs+i)-missingval) < eps1min) THEN
                Messwert(ngrs+i) = 0._rn
                StdUnc(ngrs+i) = 0._rn
            end if
        end if
    end do

    do i=1,numd/5
        nhh = (i-1)*5 + 1
        WRITE(chint,'(i2.2)') i
        symbole(ngrs+ncov+nhh)%s = 'Rnet_'//chint
        einheit(ngrs+ncov+nhh)%s = '1/s '
        symtyp(ngrs+ncov+nhh)%s = 'u'
        symbole(ngrs+ncov+nhh+1)%s = 'eps_'//chint
        einheit(ngrs+ncov+nhh+1)%s = ' '
        symtyp(ngrs+ncov+nhh+1)%s = 'u'
        symbole(ngrs+ncov+nhh+2)%s = 'pgamm_'//chint
        einheit(ngrs+ncov+nhh+2)%s = ' '
        symtyp(ngrs+ncov+nhh+2)%s = 'u'
        symbole(ngrs+ncov+nhh+3)%s = 'fatt_'//chint
        einheit(ngrs+ncov+nhh+3)%s = ' '
        symtyp(ngrs+ncov+nhh+3)%s = 'u'
        symbole(ngrs+ncov+nhh+4)%s = 'fcoinsu_'//chint
        einheit(ngrs+ncov+nhh+4)%s = ' '
        symtyp(ngrs+ncov+nhh+4)%s = 'u'
    end do

    if(ncov > 0) then
        call WTreeViewPutComboArray('treeview3', 2, ncov, ISymbA)
        call WTreeViewPutComboArray('treeview3', 3, ncov, ISymbB)
        call WTreeViewPutComboArray('treeview3', 4, ncov, [(2,i=1,ncov)])
    end if

    if(ecorruse == 1) then
        ncc = 0
        do i=1,numd/5-1
            WRITE(chint,'(i2.2)') i
            syA = 'eps_'//chint
            do k=i+1,numd/5
                WRITE(chint,'(i2.2)') k
                syB = 'eps_'//chint
                ncc = ncc + 1
                do j=ngrs+ncov+1,ngrs+ncov+numd
                    IF(TRIM(ucase(syA)) == ucase(Symbole(j)%s)) THEN
                        ISymbA(ncc) = j
                        SymboleA(ncc)%s = symbole(j)%s
                    end if
                    IF(TRIM(ucase(syB)) == ucase(Symbole(j)%s)) THEN
                        ISymbB(ncc) = j
                        SymboleB(ncc)%s = symbole(j)%s
                    end if
                end do
            end do
        end do
        if(ncc > ncov) call IntModA1(IsymbA,ncc)
        if(ncc > ncov) call IntModA1(IsymbB,ncc)
    end if

end subroutine GamSymList

!#################################################################################

module subroutine GamPeakvals

    ! for an activitiy evaluation from few gamma lines, their contributions to the
    ! arrays Messwert, StdUnc, MesswertSV and StdUncSV are set here.
    !
    ! called from ProcMainDiag, TransferToGTK (RDsubs) and Rechw1

    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_Gleich,       only: Messwert,StdUnc,MesswertSV,StduncSV,ngrs,ncov,ivtl,einheit,symtyp
    use UR_Linft,        only: numd
    use UR_Gspk1Fit,     only: GnetRate,SDGNetRate,effi,SDeffi,pgamm,SDpgamm,fatt,SDfatt, &
        fcoinsu,SDfcoinsu
    use Top,             only: RealModA1,IntModA1,CharModA1
    use UR_VARIABLES,    only: simul_ProSetup

    implicit none

    integer(4)         :: i,nng,ix,i_arr(numd/5),nhh_arr(numd/5)

    if(numd == 0) return
!------------------------------------------------------------------

    nng = ngrs+ncov+numd
    ix = ubound(Messwert,dim=1)
    if(ix < nng) then
        call CharModA1(symtyp,nng)
        call CharModA1(einheit,nng)
        call RealModA1(Messwert,nng)
        call RealModA1(StdUnc,nng)
        call RealModA1(MesswertSV,nng)
        call RealModA1(StdUncSV,nng)
        call IntModA1(ivtl,nng)
    end if
    ! write(66,'(a,i0,a,6i3)') 'GamPeakVals: ncov=',ncov,'  IsymbA(1:6)=',IsymbA(1:6)

    ! loop over the different gamma lines of a radionuclide:
    i_arr = [(i,i=1,numd/5)]
    nhh_arr = (i_arr-1)*5 + 1

    Messwert(ngrs+ncov+nhh_arr)     = GNetRate(i_arr)
    StdUnc(ngrs+ncov+nhh_arr)       = SDGNetRate(i_arr)
    MesswertSV(ngrs+ncov+nhh_arr)   = Messwert(ngrs+ncov+nhh_arr)
    StdUncSV(ngrs+ncov+nhh_arr)     = StdUnc(ngrs+ncov+nhh_arr)
    ! Values and standard uncertainties of efficiencies:
    Messwert(ngrs+ncov+nhh_arr+1)   = Effi(i_arr)
    StdUnc(ngrs+ncov+nhh_arr+1)     = SDEffi(i_arr)
    MesswertSV(ngrs+ncov+nhh_arr+1) = Messwert(ngrs+ncov+nhh_arr+1)
    StdUncSV(ngrs+ncov+nhh_arr+1)   = StdUnc(ngrs+ncov+nhh_arr+1)
    ! Values and standard uncertainties of emission intensities:
    Messwert(ngrs+ncov+nhh_Arr+2)   = pgamm(i_arr)
    StdUnc(ngrs+ncov+nhh_arr+2)     = SDpgamm(i_arr)
    MesswertSV(ngrs+ncov+nhh_arr+2) = Messwert(ngrs+ncov+nhh_arr+2)
    StdUncSV(ngrs+ncov+nhh_arr+2)   = StdUnc(ngrs+ncov+nhh_arr+2)
    ! Values and standard uncertainties of attenuation corrrections:
    Messwert(ngrs+ncov+nhh_arr+3)   = fatt(i_arr)
    StdUnc(ngrs+ncov+nhh_arr+3)     = SDfatt(i_arr)
    MesswertSV(ngrs+ncov+nhh_arr+3) = Messwert(ngrs+ncov+nhh_arr+3)
    StdUncSV(ngrs+ncov+nhh_arr+3)   = StdUnc(ngrs+ncov+nhh_arr+3)
    ! Values and standard uncertainties of Coinsum(=TCS) corrections:
    Messwert(ngrs+ncov+nhh_arr+4)   = fcoinsu(i_arr)
    StdUnc(ngrs+ncov+nhh_arr+4)     = SDfcoinsu(i_arr)
    MesswertSV(ngrs+ncov+nhh_arr+4) = Messwert(ngrs+ncov+nhh_arr+4)
    StdUncSV(ngrs+ncov+nhh_arr+4)   = StdUnc(ngrs+ncov+nhh_arr+4)

    if(simul_ProSetup) then
        call IntModA1(IVTL,ngrs+ncov+numd)
        IVTL(ngrs+ncov+1:ngrs+ncov+numd) = 1
    end if
    return

end subroutine GamPeakvals

!#################################################################################

module subroutine AdjustRemoveTVRows(numrows_marked)

    ! If in a treeview rows have been marked by the user for removing them,
    ! the rows in the table are removed, but the main effort is to remove the
    ! correponding array sections in the various arrays. Which of the arrays
    ! are concerned depends on the table in which data have to be removed.
    !
    ! called by ProcMainDiag

    !     Copyright (C) 2014-2023  Günter Kanisch

    use UR_params,            only: rn
    use, intrinsic :: iso_c_binding,        only: c_int
    use UR_variables,         only: actual_grid,bottom_selrow,top_selrow
    use UR_Gleich
    use UR_Linft
    use UR_Gspk1Fit
    use UWB,                  only: correctlists
    use LSTfillT,             only: WDListstoreFill_table
    use Rout,                 only: WTreeViewPutDoubleArray,  &
        WTreeViewRemoveRow,WTreeViewAppend,WTreeViewPutIntArray
    use Top,                  only: idpt,RealModA1
    use gtk_hl_tree,          only: hl_gtk_listn_get_n_rows
    use gtk,                  only: gtk_widget_set_visible,gtk_widget_set_sensitive, &
        gtk_widget_hide
    implicit none

    integer(c_int),intent(in) ::numrows_marked

    integer(4)              :: i,ix,kanz,km,ngmax,nj,nrows,iarray(nmumx),k,jj
    integer(4)              :: nv,nplus,nvor,nvsum,nupper,nall
    real(rn),allocatable    :: rdummy(:)
    integer(c_int)          :: nrowsc

    if(trim(actual_grid) <= 'treeview2') then     ! covariances
        call CorrectLists(top_selrow,bottom_selrow)
    end if

    do i=bottom_selrow, top_selrow, -1
        if(i < top_selrow) exit
        call WTreeViewRemoveRow(trim(actual_grid), i) ! remove row by row

        select case (trim(actual_grid))
          case ('treeview3')       ! ! covariances
            km = max(1,ncov-1)
            write(66,*) 'TV3: Delete row i=',int(i,2)
            write(66,*) 'before:'
            do k=1,ncov
                write(66,'(I3,2x,i3,2x,i1,3x,es10.3)') IsymbA(k),IsymbB(k),icovtyp(k),covarval(k)
            end do
            if(ncov > 0) then
                write(66,*) 'IsymbA: ',int(IsymbA(1:ncov),2)
                write(66,*) 'IsymbB: ',int(IsymbB(1:ncov),2)
            end if
            write(66,*) 'ubound(covarval)=',int(ubound(covarval,dim=1),2),'ubound(corrval)=',int(ubound(corrval,dim=1),2)

            if(i == 1) then
                IsymbA(1:km) = IsymbA(i+1:km+1)
                IsymbB(1:km) = IsymbB(i+1:km+1)
                icovtyp(1:km) = icovtyp(1+1:km+1)
                CVFormel(1:km) = CVFormel(i+1:km+1)
                covarval(1:km) = covarval(i+1:km+1)
                corrval(1:km) = corrval(i+1:km+1)
                SymboleA(1:km) = SymboleA(i+1:km+1)
                SymboleB(1:km) = SymboleB(i+1:km+1)
            elseif(i < ncov) then
                IsymbA(1:km) = [  IsymbA(1:i-1), IsymbA(i+1:km+1) ]
                IsymbB(1:km) = [  IsymbB(1:i-1), IsymbB(i+1:km+1) ]
                icovtyp(1:km) = [  icovtyp(1:i-1), icovtyp(i+1:km+1) ]
                CVFormel(1:km) = [  CVFormel(1:i-1), CVFormel(i+1:km+1) ]
                covarval(1:km) = [  covarval(1:i-1), covarval(i+1:km+1) ]
                corrval(1:km) = [  corrval(1:i-1), corrval(i+1:km+1) ]
                SymboleA(1:km) = [  SymboleA(1:i-1), SymboleA(i+1:km+1) ]
                SymboleB(1:km) = [  SymboleB(1:i-1), SymboleB(i+1:km+1) ]
            end if
            IsymbA(km+1) = 0
            IsymbB(km+1) = 0
            icovtyp(km+1) = 0
            CVFormel(km+1)%s = ' '
            covarval(km+1) = missingval
            corrval(km+1) = missingval
            SymboleA(km+1)%s = ' '
            SymboleB(km+1)%s = ' '
            if(Gamspk1_Fit) then
                Symbole(ngrs+ncov:ngrs+ncov+numd-1) = Symbole(ngrs+ncov+1:ngrs+ncov+numd)
                symtyp(ngrs+ncov:ngrs+ncov+numd-1) = symtyp(ngrs+ncov+1:ngrs+ncov+numd)
                einheit(ngrs+ncov:ngrs+ncov+numd-1) = einheit(ngrs+ncov+1:ngrs+ncov+numd)
                Messwert(ngrs+ncov:ngrs+ncov+numd-1) = Messwert(ngrs+ncov+1:ngrs+ncov+numd)
                StdUnc(ngrs+ncov:ngrs+ncov+numd-1) = StdUnc(ngrs+ncov+1:ngrs+ncov+numd)
                Sensi(ngrs+ncov:ngrs+ncov+numd-1) = Sensi(ngrs+ncov+1:ngrs+ncov+numd)
                Perc(ngrs+ncov:ngrs+ncov+numd-1) = Perc(ngrs+ncov+1:ngrs+ncov+numd)

                do k=1,km
                    IsymbA(k) = IsymbA(k) - 1
                    IsymbB(k) = IsymbB(k) - 1
                end do
                ! also for FitDecay???
            end if

            ncov = max(0, ncov - 1)
            write(66,*) 'afterwards:'
            do k=1,ncov
                write(66,'(I3,2x,i3,2x,i1,3x,es10.3)') IsymbA(k),IsymbB(k),icovtyp(k),covarval(k)
            end do
            write(66,*) 'IsymbA: ',int(IsymbA,2)
            write(66,*) 'IsymbB: ',int(IsymbB,2)


          case ('treeview5')          ! decay curve data
            km = max(1,numd)
            dmesszeit(i:km-1) = dmesszeit(i+1:km-1+1)
            dbimpulse(i:km-1) = dbimpulse(i+1:km-1+1)
            dbzrate(i:km-1) = dbzrate(i+1:km-1+1)
            sdbzrate(i:km-1) = sdbzrate(i+1:km-1+1)
            d0messzeit(i:km-1) = d0messzeit(i+1:km-1+1)
            d0impulse(i:km-1) = d0impulse(i+1:km-1+1)
            sd0zrate(i:km-1) = sd0zrate(i+1:km-1+1)
            dnetrate(i:km-1) = dnetrate(i+1:km-1+1)
            sdnetrate(i:km-1) = sdnetrate(i+1:km-1+1)
            do jj=i,km-1
                CStartzeit(jj)%s = CStartzeit(jj+1)%s
            end do
            CStartzeit(km)%s = '  '
            dmesszeit(km) = missingval
            dbimpulse(km) = missingval
            dbzrate(km) = missingval
            sdbzrate(km) = missingval
            d0messzeit(km) = missingval
            d0impulse(km) = missingval
            d0zrate(km) = missingval
            sd0zrate(km) = missingval
            dnetrate(km) = missingval
            sdnetrate(km) = missingval
            numd = max(0, numd - 1)

          case ('treeview6')      ! gamma peak related data
            km = max(1,numd/5)
            guse(1:km-1) = [  guse(1:i-1), guse(i+1:km) ]
            erg(1:km-1) = [  erg(1:i-1), erg(i+1:km) ]
            GnetRate(1:km-1) = [  GnetRate(1:i-1), GnetRate(i+1:km) ]
            RateCB(1:km-1) = [  RateCB(1:i-1), RateCB(i+1:km) ]
            RateBG(1:km-1) = [  RateBG(1:i-1), RateBG(i+1:km) ]
            SDRateBG(1:km-1) = [  SDRateBG(1:i-1), SDRateBG(i+1:km) ]
            effi(1:km-1) = [  effi(1:i-1), effi(i+1:km) ]
            SDeffi(1:km-1) = [  SDeffi(1:i-1), SDeffi(i+1:km) ]
            pgamm(1:km-1) = [  pgamm(1:i-1), pgamm(i+1:km) ]
            SDpgamm(1:km-1) = [  SDpgamm(1:i-1), SDpgamm(i+1:km) ]
            fatt(1:km-1) = [  fatt(1:i-1), fatt(i+1:km) ]
            SDfatt(1:km-1) = [  SDfatt(1:i-1), SDfatt(i+1:km) ]
            fcoinsu(1:km-1) = [  fcoinsu(1:i-1), fcoinsu(i+1:km) ]
            SDfcoinsu(1:km-1) = [  SDfcoinsu(1:i-1), SDfcoinsu(i+1:km) ]

            guse(km) = 0
            erg(km) = missingval
            GnetRate(km) = missingval
            RateCB(km) = missingval
            RateBG(km) = missingval
            SDRateBG(km) = missingval
            effi(km) = missingval
            SDeffi(km) = missingval
            pgamm(km) = missingval
            SDpgamm(km) = missingval
            fatt(km) = missingval
            sdfatt(km) = missingval
            fcoinsu(km) = missingval
            sdfcoinsu(km) = missingval
            numd = max(0, numd - 5)

          case ('treeview7')      ! FitcalCurve data

            km = max(1,nkalpts-1)
            xkalib(1:km-1) = [  xkalib(1:i-1), xkalib(i+1:km) ]
            uxkalib(1:km-1) = [  uxkalib(1:i-1), uxkalib(i+1:km) ]
            ykalib(1:km-1) = [  ykalib(1:i-1), ykalib(i+1:km) ]
            uykalib(1:km-1) = [  uykalib(1:i-1), uykalib(i+1:km) ]

            xkalib(km) = missingval
            uxkalib(km) = missingval
            ykalib(km) = missingval
            uykalib(km) = missingval
            nkalpts = max(0, nkalpts - 1)

          case ('treeview8')     ! input vectors for means

            ! write(66,*) 'ixdanf: ',ixdanf
            ! write(66,*) 'xdataMD: ',sngl(xdataMD)
            ! write(66,*) 'k_datvar=',k_datvar,'  meanID=',meanID(k_datvar)%s

            if(k_datvar == 1) nvsum = 0
            if(k_datvar > 1) nvsum = sum(nvalsMD(1:k_datvar)) - 1
            nall = sum(nvalsMD(1:nvarsMD))
            nupper = sum(nvalsMD(k_datvar:nvarsMD))
            nvor = nvalsMD(k_datvar)
            nplus = nvalsMD(k_datvar) - 1
            ix = ubound(xdataMD,dim=1)

            nj = ixdanf(k_datvar)
            nv = max(0, nvalsMD(k_datvar))
            if(allocated(rdummy)) deallocate(rdummy)
            allocate(rdummy(nupper-1))
            rdummy(1:nupper-1) = [ xdataMD(nj:nj+(i-1)-1),            &
                xdataMD(nj+(i+1)-1:ixdanf(nvarsMD)+nvalsMD(nvarsMD)-1) ]

            if(nplus /= 0) call RealModA1(xdataMD,nvsum+nplus)

            if(k_datvar == 1) then
                xdataMD = rdummy(1:nupper-1)
                ixdanf(k_datvar+1:nvarsMD) = ixdanf(k_datvar+1:nvarsMD) + (nplus-nvor)
            elseif(k_datvar > 1 .and. k_datvar < nvarsMD) then
                xdataMD = [ xdataMD(1:ixdanf(k_datvar)-1), rdummy(1:nupper-1) ]
                ixdanf(k_datvar+1:nvarsMD) = ixdanf(k_datvar+1:nvarsMD) + (nplus-nvor)
            elseif(k_datvar == nvarsMD) then
                xdataMD = [ xdataMD(1:ixdanf(k_datvar)-1), rdummy(1:nupper-1) ]
            end if
            deallocate(rdummy)

            nvalsMD(k_datvar) = max(0, nvalsMD(k_datvar) - 1)
            write(66,*) 'after modification: nvalsMD(k_datvar)=',nvalsMD(k_datvar),' nplus-nvor=',nplus-nvor
            write(66,*) 'ixdanf: ',ixdanf

        end select

    end do   ! deleted rows
    ! write(66,*) 'End deleting : xdataMD :',(sngl(xdataMD))

    if(trim(actual_GRID) == 'treeview3') then          ! covar table
        ngrsP = ngrs + ncov + numd
        call WDListstoreFill_table('liststore_covtable',4, .false.)
    end if
    if(trim(actual_GRID) == 'treeview5') then
        call WDListstoreFill_table('liststore_Decay',5, .false.)
    end if
    if(trim(actual_GRID) == 'treeview6') then
        call WDListstoreFill_table('liststore_gspk1',6, .false.)
    end if
    if(trim(actual_GRID) == 'treeview7') then
        call WDListstoreFill_table('liststore_kalfit',7, .false.)
    end if
    if(trim(actual_GRID) == 'treeview8') then
        if(allocated(rdummy)) deallocate(rdummy)
        allocate(rdummy(nvalsMD(k_datvar)))
        rdummy = xdataMD(nj: nj+nvalsMD(k_datvar)-1)
        call WTreeViewPutDoubleArray('treeview8', 2, nvalsMD(k_datvar), &
            rdummy)
    end if

    if(trim(actual_GRID) == 'treeview1') then

        ngrs = ngrs - numrows_marked
        symlist_shorter = .false.
        nmu = ngrs - nab
    end if

    if(trim(actual_GRID) == 'treeview3') then
        call WDListstoreFill_table('liststore_covtable',4, .false.)
    end if

    ngmax = ngrs + 30

    IF(trim(actual_GRID) == 'treeview5') ngmax = 120
    IF(trim(actual_GRID) == 'treeview6') ngmax = kdatmax
    IF(trim(actual_GRID) == 'treeview7') ngmax = 40
    do i=1,ngmax
        iarray(i) = i
    end do
    if(trim(actual_GRID) == 'treeview1') then
        call WTreeViewAppend(actual_GRID)
    end if
    call WTreeViewPutIntArray(actual_grid,1,ngmax,iarray)
    if(trim(actual_grid) == 'treeview1') then
        TAB_GLEICHG_2Rates = .false.
        call gtk_widget_set_sensitive(idpt('comboboxNetRate'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('comboboxGrossRate'), 0_c_int)
        ! call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('AcceptAll'), 0_c_int)

        call gtk_widget_set_sensitive(idpt('NBValUnc'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
        call gtk_widget_set_visible(idpt('NBValUnc'), 0_c_int)
        call gtk_widget_set_visible(idpt('NBBudget'), 0_c_int)
        call gtk_widget_set_visible(idpt('NBResults'), 0_c_int)
        call gtk_widget_hide(idpt('box4'))
        call gtk_widget_hide(idpt('box5'))
        call gtk_widget_hide(idpt('grid5'))

        call gtk_widget_set_sensitive(idpt('TBRefreshCalc'), 0_c_int)
    end if

    if(.true. .and. ( trim(actual_grid) == 'treeview1'  .or.   &
        trim(actual_grid) == 'treeview3' )  ) then
        ! Adjust the uncertainty budgets (TV4) to the modifications in Treeview2,
        ! remove the superfluous variables:
        kanz = ngrs+ncov+numd - numrows_marked         !   Unc-Budget
        nrowsc = hl_gtk_listn_get_n_rows(idpt('treeview4'))
        nrows = nrowsc + 1
        nrows = min(kanz, nrows)
        do i=nrows,kanz+1,-1
            Messwert(i) = missingval
            SDwert(i) = missingval
            Sensi(i) = missingval
            Perc(i) = missingval
            Ucontrib(i) = missingval
            Symbole(i)%s = ' '
            symtyp(i)%s = ' '
            einheit(i)%s = ' '

            Messwert_CP(i) = missingval
            SDwert_CP(i) = missingval
            !Sens_CP(i) = missingval
            Perc_CP(i) = missingval
            !Ucontrib_CP(i) = missingval
            Symbole_CP(i)%s = ' '
            symtyp_CP(i)%s = ' '
            einheit_CP(i)%s = ' '
        end do
    end if

end subroutine AdjustRemoveTVRows

!#################################################################################



end submodule PMDA
