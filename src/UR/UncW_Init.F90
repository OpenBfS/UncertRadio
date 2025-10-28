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
module urInit
    use UR_types

contains


!#########################################################################
!
!   Fortran units:
!     15  : report file
!     16  : for output to file UR-Saved-Results.csv
!     17  : intermediate file used for printing
!     18  : used locally in URGladeSys; PrepTranslate.txt
!     19  : Gladefile; Batlist_resu*;
!     20  : Gladefile;
!     21  : in Uncw_sub3
!     22  : linfout.txt; Prepreport
!     23  : LSQGEN-Output
!     24  : mcmc output file
!     25  : Projekt-Datei *.txp  oder *.csv (ProRead, ProSave,...)
!     26  : fnameMCB; Batch_proc
!     28  : mcmc output file
!     30  : Output von NWG-Iteration (decision threshold, detection limit)
!     32  : loadsel_diag_new
!     34  : lesen settings.ini
!     44  : WDListstoreFill_table
!     55  : Output von Gleichungen-Interpretieren
!     62  : in Batest
!     63  : output von MCtables
!     65  : Output von URGladeSys
!     66  : Standard-Kontroll-Output von UR
!     67  : Batest, Cofellipse, DisplayHelp,Loadseldiag_new,MCCalc,...diverse
!     69  : Lsqlincov2
!     71,72,73  : run_mcmc_metrop
!     76  : MCcalc, Batest
!     77  : Ausgabe URExport
!     78  : URExport: Ausgabe covmat1.txt
!     79  : URExport: Ausgabe data1.txt
!     96  : Read URunits
!     97  : URGladesys
!
!

!   This module includes the following routines:
!     UncW_init
!     read_cfg
!     GtkSettingsIO
!     TVtrimCol_width
!     StartAlloc
!     ReadUnits
!


!#########################################################################

    subroutine UncW_Init()

        ! UncW_init is called the first time from within create_window;
        !
        ! incall is the number of calls of UncW_init;
        ! some important g-values are initialized;
        ! reading and processing the GtkSettings (file Settings.ini);
        ! define some Fortran-formats for storing real(rn) values in the GUI;
        ! set the actual UncertRadion program version string;
        ! translates the Gtk liststores and ininaites the Gtk treeviews;
        ! initialize a lot of variables and arrays as well as correponding GUI elements;
        ! reading a file with user-dependent units;
        ! executes StartAlloc, by which a lot of allocatable arrays are initialized;
        !
        ! choose to have control output in Windows console (logical consoleout_gtk)
        !
        !     Copyright (C) 2014-2023  Günter Kanisch


        use, intrinsic :: iso_c_binding
        use ur_general_globals, only: frmt,frmtc,frmt_min1,frmtg,frmtres,frmtres_min1, &
                                    gum_restricted,MCSim_on,multi_eval, &
                                    plot_confidoid,plot_ellipse,png_to_cairo_surface, prostartnew, &
                                    savef,savep,sdecimalpoint,slistseparator, &
                                    ableit_fitp,filetyp, runauto, &
                                    Confidoid_activated,clipd,gross_negative,kModelType,modvar_on, &
                                    cModeltype, FNAME,progstart_on, UR_version_tag
        use UR_Gleich_globals, only: DistPars,apply_units,apply_units_dir,coverf,coverin, &
                                    gamspk_rename,ifehl, &
                                    ilam_binom,ip_binom,itm_binom, increase_dpafact,k_datvar, &
                                    kableitnum,kbgv_binom,kbrutto_gl,kEGr,kEGr_old,knetto,knullef, &
                                    knumEGr,linfit_rename,linmod1_on,LinTest,loadingPro,N_preset, &
                                    nab,ncov,ncovf,nglf,nglp,ngrs_CP,nmu,nmxdist,nparts,nvarsMD, &
                                    nvars_in_rbtot,refresh_type,rinflu_known,symlist_modified, &
                                    TAB_GLEICHG_2Rates,TAB_GLEICHG_Grid,TAB_VALUNC_Grid,ngrs, &
                                    kbrutto,refdataMD,tback,tgross,ucontyp,uncval_exists,use_bipoi, &
                                    use_dependent_sdwert,use_dp,knetp3,nRnetp, &
                                    kbrutto_double,ndep,use_sdf_brutto, &
                                    var_rbtot,FP_for_units,Formeltext_out,defined_RSY

        use UR_DLIM,          only: alpha,beta,decthresh,detlim,gamdistadd,kalpha,kbeta,limit_typ, &
                                    nit_detl_max,w1minusG,var_brutto_auto,k_autoform
                                    use UR_Linft
        use UR_Gspk1fit
        use UR_MCC,           only: use_BCI,imc,MCsim_done,estLQ,estUQ,kcmx,kcrun
        use UR_Loadsel,       only: NBpreviousPage,NBcurrentPage

        use gtk,              only: gtk_window_set_title,gtk_widget_set_sensitive, &
                                    gtk_widget_hide,gtk_widget_set_visible,gtk_recent_manager_get_default, &
                                    gtk_settings_get_default,gtk_text_view_set_cursor_visible, &
                                    gtk_text_view_place_cursor_onscreen, &
                                    gtk_widget_set_focus_on_click, &
                                    gtk_text_view_place_cursor_onscreen, &
                                    gtk_widget_show,gtk_widget_grab_focus, &
                                    gtk_clipboard_get, &
                                    gtk_get_major_version,gtk_get_minor_version,gtk_get_micro_version, &
                                    gtk_entry_get_width_chars, gtk_window_set_icon

        use g,                only: g_object_set_property,g_value_init,g_value_set_string, &
                                    g_value_set_int,g_get_home_dir

        use gdk,              only: gdk_atom_intern

        use gdk_pixbuf,       only: gdk_pixbuf_new_from_resource

        use gtk_sup,          only: c_f_string, G_TYPE_LONG, G_TYPE_STRING, G_TYPE_BOOLEAN

        use Rout,             only: WDSetComboboxAct,WDPutSelRadio,WDPutEntryDouble, &
                                    WDPutEntryString,WDPutSelRadioMenu,WDPutTextviewString, &
                                    WDPutEntryInt,WDSetCheckButton, &
                                    ClearMCfields,ClearPEfields

        use UR_gtk_globals, only:   toggleTypeGTK, dialogloop_on, NBsoftSwitch, list_filling_on, &
                                    item_setintern, consoleout_gtk,    &
                                    lstfd_syms,lstfd_symtable,lstfd_valunc,lstfd_budget, &
                                    TV1_lentext,dialog_on, &
                                    tv_colwidth_pixel,tv_colwidth_digits,tvnames,ntvs,tvcols,zoomf, &
                                    Settings, replot_on, zoomf_prev, &
                                    dint4, dintval, pstring, stringv, plogval, logval, UR_widgets

        use Brandt,           only: qnorm
        use Top,              only: FieldUpdate, WrStatusbar, idpt
        use CHF,              only: lowercase

        use UR_params,        only: ZERO, ONE, win_title
        use common_sub1,      only: draw_baseBS, draw_baseCP, draw_baseMC, draw_baseELI, &
                                    drawboxpackedBS, drawboxpackedCP, drawboxpackedMC, &
                                    drawboxpackedELI, cc

        use file_io,          only: logger

        use translation_module, only: T => get_translation, get_language
        use ISO_FORTRAN_ENV,  only: compiler_version
        use UR_DecChain,      only: nDCnet,nsubdec,DChain
        use DECH,             only: LoadDecayChains


        implicit none
        integer, save              :: incall = 0

        integer                    :: rowmax, i, k, itv
        integer                    :: ii, ios, np
        integer(c_int), target     :: recent_lim, res
        integer(c_int)             :: maxchars

        type(c_ptr)                :: recman, atomCLB, icon

        character(len=40)          :: cnum
        real(rn)                   :: testval

        character(len=100), allocatable  :: names(:)
        ! logical, allocatable             :: writable(:), scalable(:)
        logical, allocatable             :: scalable(:)
        character(len=200), allocatable  :: description(:)

        character(len=512)               :: log_str
        real(rn)                         :: start, finish
        type(charv), allocatable         :: leertext(:)

        !-----------------------------------------------------------------------
        call cpu_time(start)
        call logger(66, 'INIT_Start............................................ ')

        incall = incall + 1
        !-----------------------------------------------------------------------
        if(incall == 1) then           ! inca
            ! initialize some often needed g values:
            dintval = g_value_init(c_loc(dint4), G_TYPE_LONG)
            pstring = g_value_init(c_loc(stringv), G_TYPE_STRING)
            plogval = g_value_init(c_loc(logval), G_TYPE_BOOLEAN)
            zoomf = 1.0_rn
            zoomf_prev = 1.0_rn

            ! goto 11
            call GtkSettingsIO(.true., ifehl)

            Settings%GtkSetDef = gtk_settings_get_default()

            do np=1,Settings%nprops
                if(trim(lowercase(Settings%sproperty(np))) == 'false') then
                    ! call hl_gtk_list_tree_set_gvalue(plogval,G_TYPE_BOOLEAN,svalue='F')
                    call g_object_set_property(Settings%GtkSetDef, &
                        trim(Settings%sproperty(np))//c_null_char, plogval)
                else if(trim(lowercase(Settings%sproperty(np))) == 'true') then
                    ! call hl_gtk_list_tree_set_gvalue(plogval,G_TYPE_BOOLEAN,svalue='T')
                    call g_object_set_property(Settings%GtkSetDef, &
                        trim(Settings%sproperty(np))//c_null_char, plogval)
                else
                    read(Settings%sproperty_val(np),*,iostat=ios) ii
                    if(ios /= 0) then
                        call g_value_set_string(pstring,   &
                            trim(Settings%sproperty_val(np))//c_null_char)
                        call g_object_set_property(Settings%GtkSetDef, &
                            trim(Settings%sproperty(np))//c_null_char, pstring)
                    else
                        call g_value_set_int(dintval, ii)
                        call g_object_set_property(Settings%GtkSetDef, &
                            trim(Settings%sproperty(np))//c_null_char, dintval)
                    end if
                end if
            end do
        end if

        !-----------------------------------------------
        maxchars = gtk_entry_get_width_chars(idpt('TRentryValue'))
        if(incall == 1)  then
            write(log_str, '(*(g0))') 'TRentryValue: maxchars=',int(maxchars,2)
            call logger(66, log_str)
        end if

        ! ! Formats for Double dialog fields
        frmt = '(1pG19.9E2)'
        frmt_min1 = '(1pG19.8E2)'

        ! adapt formats to the width of entry field:
        testval = 1.12345678E+12_rn
        frmtres = '(1pG18.8E2)'                    ! for Tab Results, without DT/DL
        frmtres_min1 = '(1pG17.7E2)'               ! for Tab Results, onyl DT/DL

        write(cnum,frmtres) testval
        cnum = adjustL(cnum)
        if(len_trim(cnum) > int(maxchars)-1) then
            frmtres = '(1pG17.7E2)'                    ! for Tab Results
            frmtres_min1 = '(1pG16.6E2)'
            write(cnum,frmtres) testval
            cnum = adjustL(cnum)
            if(len_trim(cnum) > int(maxchars)-1) then
                frmtres = '(1pG16.6E2)'
                frmtres_min1 = '(1pG15.5E2)'
                write(cnum,frmtres) testval
                cnum = adjustL(cnum)
                if(len_trim(cnum) > int(maxchars)-1) then
                    frmtres = '(1pG15.5E2)'
                    frmtres_min1 = '(1pG14.4E2)'
                end if
            end if
        end if

        frmtg = '(es12.6E1)'
        frmtc = '(es13.6)'

        rowmax = nmumx

        toggleTypeGTK = 'bool'
        dialogloop_on = .false.
        item_setintern = .false.

        if(incall == 1)  then
            write(log_str, '(a,a)') 'Compiler version = ', compiler_version()
            call logger(66, log_str)
        end if

        if(incall == 1)  then
            write(log_str, '(a,a)') 'URversion=',trim(UR_version_tag)
            call logger(66, log_str)
        end if

        if(incall == 1)  then
            write(log_str, '(a,i0,a1,i0,a1,i0)') 'GTK3: Version=',gtk_get_major_version(),'.', &
                gtk_get_minor_version(),'.',gtk_get_micro_version()
            call logger(66, log_str)
        end if

        ! the deallocated leertext does the job of erasing the lines in the textviews 20.11.2024
        if(allocated(leertext)) deallocate(leertext)
        !allocate(leertext(1))
        !leertext(1)%s = ''
        call WDPutTextviewString('textview1',leertext)           !   deactivated: 19.11.2024 GK
        call WDPutTextviewString('textview2',leertext)           !   don't load empty strings into the textfield
        call WDPutTextviewString('textviewModelEQ',leertext)     !

        res = gtk_text_view_place_cursor_onscreen (idpt('textview1'))
        call gtk_text_view_set_cursor_visible(idpt('textview2'), 1_c_int)
        call gtk_text_view_set_cursor_visible(idpt('textview1'), 1_c_int)

        if(allocated(names)) deallocate(names)

        if(allocated(scalable)) deallocate(scalable)
        if(allocated(description)) deallocate(description)

        icon = gdk_pixbuf_new_from_resource("/org/UncertRadio/icons/ur2_symbol.png" // c_null_char, &
                                            c_null_ptr)

        call gtk_window_set_icon(UR_widgets%window1, icon)

        if(.not. runauto) call gtk_window_set_title(UR_widgets%window1, win_title // c_null_char)

        call gtk_window_set_title(idpt('window_graphs'), 'Plots'//c_null_char)

        call gtk_widget_hide(idpt('window_graphs'))
        call gtk_widget_set_focus_on_click(idpt('notebook1'), 1_c_int)

        NBpreviousPage = 1
        NBcurrentPage = 1
        !------------------------------------------------------------------------
        ! if true: produces a lot of control output in the Windows console
        consoleout_gtk = .false.
        ! consoleout_gtk = .true.
        !------------------------------------------------------------------------
        refresh_type = 0

        if(incall >= 1) then
            call ListstoreTranslate()
            if(incall >= 1) then
                call InitTreeViews()
            end if
        end if

        progstart_on = .false.

        lstfd_syms     = .false.
        lstfd_symtable = .false.
        lstfd_valunc   = .false.
        lstfd_budget   = .false.

        savep = .FALSE.
        savef = .FALSE.
        FileTyp = 'P'

        nglp = 0
        nglf = 0
        nab = 0
        nmu = 0

        tback = ZERO
        tgross = ZERO
        linfit_rename = .false.
        gamspk_rename = .false.


        ngrs = 0
        knetto = 0
        kbrutto = 0
        kbrutto_gl = 0
        ncov = 0
        ncovf = 0
        knullef = 0
        numd = 0
        ngrs_CP = 0
        k_autoform = 0
        kbrutto_double = 0
        use_sdf_brutto = .false.

        UcombLinf = 0._rn
        UcombLinf_kqt1 = 0._rn

        alpha = 0.05_rn
        beta = 0.05_rn
        kalpha =  qnorm(ONE - alpha)
        kbeta =  qnorm(ONE - beta)

        coverf = ONE
        coverin = ONE

        if(incall == 1)  then
            write(log_str, '(a,4(f0.7,2x))') 'kalpha, kbeta, alpha, beta= ',real(kalpha,8), &
                real(kbeta,8), real(alpha,8), real(beta,8)
            call logger(66, log_str)
        end if

        if(incall == 1) then
            ! test variables for numerical precision and limits:

            write(log_str, '(*(g0))') 'eps1min=epsilon(1.0_rn)=',epsilon(1.0_rn)
            call logger(66, log_str)


            write(log_str, '(*(g0))') 'epsilon(1.0d0)=',epsilon(1.0d0)
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'Tiny(1.0_rn)=',Tiny(1.0_rn)
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'Tiny(1.0d0)=', Tiny(1.0d0)
            call logger(66, log_str)
        end if

        W1minusG = 0.95_rn
        call WDPutEntryDouble('entryOptKalpha', kalpha, '(f8.6)')
        call WDPutEntryDouble('entryOptKbeta', kbeta, '(f8.6)')
        call WDPutEntryDouble('entryOptAlpha', alpha, '(f8.6)')
        call WDPutEntryDouble('entryOptBeta', beta, '(f8.6)')

        ifit = 1
        nit_detl_max = 120

        ! kfitmeth:  0: WLS/Neyman; 1: PLSQ/Pearson; 2: PMLE; 3: WTLS
        kableitnum = 0
        kfitmeth = 1
        kpearson = 1   ! Pearson-Chi-square verwenden
        kPMLE = 0

        fitmeth = ' '
        estLQ = 0._rn
        estUQ = 0._rn
        call WDPutEntryInt('TRentryMCit', 0)
        call WDPutEntryInt('TRentryMCit1', 0)
        call WDPutEntryString('TRentryMCdtMT',' ')
        call WDPutEntryString('TRentryMCbfMT',' ')

        savep = .FALSE.
        call FieldUpdate()

        TAB_GLEICHG_Grid   = .FALSE.
        TAB_GLEICHG_2Rates = .FALSE.
        TAB_VALUNC_Grid = .FALSE.
        FitDecay    = .FALSE.
        Gamspk1_Fit = .FALSE.
        FitCalCurve = .FALSE.
        SumEval_fit = .false.

        call WDPutSelRadioMenu('QThird', 1)
        call gtk_widget_set_sensitive(idpt('QFirst'), 1_c_int)
        call gtk_widget_set_visible(idpt('QSecond'), 0_c_int)
        call gtk_widget_set_visible(idpt('QThird'), 0_c_int)

        kModelType = 1
        call WDPutSelRadioMenu('MT_NegLin', kModelType)
        Gum_restricted = .false.
        gross_negative = .false.

        if(allocated(cModelType)) deallocate(cModelType)
        allocate(character(len=15) :: cModelType(3))
        cModelType(1) = 'PosLin'
        cModelType(2) = 'GUM_restricted'
        cModelType(3) = 'NegLin'

        call gtk_widget_set_sensitive(idpt('box2'), 1_c_int)
        call gtk_widget_set_sensitive(idpt('box3'), 1_c_int)

        call gtk_widget_set_visible(idpt('box2'), 1_c_int)
        call gtk_widget_set_visible(idpt('box3'), 1_c_int)
        if(.not.runauto) then
            call gtk_widget_show(idpt('box2'))
            call gtk_widget_show(idpt('box3'))
        end if
        call WDSetComboboxAct('comboboxNetRate',0)
        call WDSetComboboxAct('comboboxGrossRate',0)

        call ClearPEfields()
        call WDPutEntryString('entryActiveKegr',' ')
        call ClearMCfields(0)

        call gtk_widget_set_sensitive(idpt('treeview1'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('comboboxNetRate'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('comboboxGrossRate'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('LoadCompletedSyms'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('AcceptAll'), 0_c_int)

        call gtk_widget_set_sensitive(idpt('NBValUnc'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('TBRefreshCalc'), 0_c_int)
        call gtk_widget_hide(idpt('box4'))
        call gtk_widget_hide(idpt('box5'))
        call gtk_widget_hide(idpt('grid5'))

        call gtk_widget_set_sensitive(idpt('box4'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('box5'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('grid5'), 0_c_int)

        call gtk_widget_set_visible(idpt('TestConfEllipse'), 0_c_int)

        call gtk_widget_set_visible(idpt('FontSel'), 0_c_int)
        call gtk_widget_set_visible(idpt('ColorSel'), 0_c_int)

        call gtk_widget_set_sensitive(idpt('ExportToR'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('FittingModel'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('FittingData'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('FittingResult'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('TBModelDialog'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('TBInputDialog'), 0_c_int)
        call gtk_widget_set_sensitive(idpt('TBFittingResult'), 0_c_int)

        export_r = .TRUE.
        export_r = .FALSE.
        export_case = .FALSE.
        call gtk_widget_set_sensitive(idpt('KalFit'), 0_c_int)

        call WDSetComboboxAct('comboboxA1',0)
        call WDSetComboboxAct('comboboxA2',0)
        call WDSetComboboxAct('comboboxA3',0)

        call WDSetCheckButton('checkbuttonWFit',0)
        call WDSetCheckButton('checkbuttonCovZR',0)
        call WDSetCheckButton('checkbuttonAllm',0)
        call WDSetComboboxAct('comboboxtextNCH',1)
        call WDPutSelRadio('radiobuttonNLSQ',1)

        call WDPutEntryString('entryNetBlindVal',' ')
        call WDPutEntryString('entrySeparation',' ')

        call WDSetComboboxAct('comboboxGMWtyp',1)
        call WDSetCheckButton('checkbuttonMeanOpt',0)
        call WDSetCheckButton('checkbuttonGspk1EffiCov',0)
        call WDPutEntryString('entry_b2LFactor',' ')

        call WDPutEntryString('entryDKTitel',' ')

        call gtk_widget_set_sensitive(idpt('Exchange2Symbols'), 0_c_int)

        call WrStatusbar(1, T("no project loaded!"))
        call WrStatusbar(4, T("Next: Describe procedure by text, then: TAB 'Equations'"))

        call gtk_widget_set_visible(idpt('MenuOpenTextFile'), 0_c_int)
        call gtk_widget_set_visible(idpt('TESavePrjAs'), 0_c_int)

        MCsim_on = .FALSE.
        call gtk_widget_hide(idpt('window_graphs'))

        loadingPro = .FALSE.
        ! loadingpro = .true.
        png_to_cairo_surface = .FALSE.          ! 16.5.2025

        KnumEGr = 0
        kEGr = 0

        nchannels = 1
        use_WTLS = .FALSE.
        use_WTLS_kal = .false.     ! 7.8.2023

        GamDistAdd = ZERO       ! since 30.11.2019; according to ISO 11929:2019
        klincall = 0
        Ucontyp = 1

        Decthresh = ZERO
        Detlim = ZERO
        use_BCI = .FALSE.
        ecorruse = 0
        if(incall == 1) then
            write(log_str, '(*(g0))') 'Windows List_separator=',sListSeparator,' Windows DecimalPoint=',sDecimalPoint
            call logger(66, log_str)
        end if

        limit_typ = 0

        singlenuk = .FALSE.

        recent_lim = 29_c_int
        if(incall == 1) then
            recman = gtk_recent_manager_get_default()
        end if

        FitCalCurve = .false.
        imc = 0
        konstant_R0 = .false.
        use_otherMinv = .false.
        test_cauchy = .true.
        proStartNew = .true.

        plot_ellipse = .false.

        plot_ellipse = .false.
        plot_confidoid = .false.
        multi_eval = .false.
        ableit_fitp = .false.

        call gtk_widget_set_visible(idpt('frame18'), 0_c_int)
        call gtk_widget_set_visible(idpt('ActivateBS'),0_c_int)
        ! Note: nb2LabelMCMC can be set invisible only in/after CairoPlotPrepare,
        ! after the _show_all of the window.

        call gtk_widget_set_visible(idpt('TRButtonHelp'), 0_c_int)
        call gtk_widget_set_visible(idpt('TRbuttonSavecsv'), 0_c_int)

        uncval_exists = .false.
        symlist_modified = .false.
        run_corrmat = .false.

        kalfit_arg_expr = ' '
        NBsoftSwitch = .false.
        list_filling_on = .false.

        call gtk_widget_set_sensitive(idpt('TBSaveProjectAs'), 0_c_int)

        mfRBG_fit_PMLE = .true.
        linmod1_on = .false.
        call WDSetCheckButton('TRcheckbutton2', 0)
        use_BCI = .FALSE.
        TV1_lentext = 0
        nparts = 0

        call gtk_text_view_set_cursor_visible(idpt('textview1'), 1_c_int)
        res = gtk_text_view_place_cursor_onscreen(idpt('textview2'))
        call gtk_text_view_set_cursor_visible(idpt('textview2'), 1_c_int)

        call gtk_widget_set_focus_on_click(UR_widgets%window1, 1_c_int)

        call gtk_widget_grab_focus(idpt('textview1'))
        dialog_on = .false.
        MCsim_done = .false.

        draw_baseMC = c_null_ptr
        draw_baseELI = c_null_ptr
        draw_baseBS = c_null_ptr
        draw_baseCP = c_null_ptr
        drawboxpackedMC = .false.
        drawboxpackedELI = .false.
        drawboxpackedBS = .false.
        drawboxpackedCP = .false.
        confidoid_activated = .false.

        do i=1,4
            cc(i) = c_null_ptr
        end do

        atomCLB = gdk_atom_intern("CLIPBOARD"//c_null_char,1_c_int)
        clipd = gtk_clipboard_get(atomCLB)

        nvarsMD = 0
        call gtk_widget_set_sensitive(idpt('TBmeansMD'), 0_c_int)
        call gtk_widget_hide(idpt('dialog_LoadPro'))

        tv_colwidth_digits = 0
        tv_colwidth_pixel = 0
        itv = 0
        do i=1,ntvs
            if(loadingPro) exit

            if('treeview1' == tvnames(i)%s) then
                tv_colwidth_digits(i,1) = 3
                tv_colwidth_digits(i,2) = 10 ! 20
                tv_colwidth_digits(i,3) = 3
                tv_colwidth_digits(i,4) = 10
                tv_colwidth_digits(i,5) = 50 ! 70
                call TVtrimCol_width('treeview1')
            elseif('treeview2' == tvnames(i)%s) then
                tv_colwidth_digits(i,1) = 3
                tv_colwidth_digits(i,2) = 10   ! 20
                tv_colwidth_digits(i,3) = 3
                tv_colwidth_digits(i,4) = 10
                tv_colwidth_digits(i,5) = 12   ! 14
                tv_colwidth_digits(i,7) = 20   ! 18
                tv_colwidth_digits(i,8) = 12  ! 14
                tv_colwidth_digits(i,9) = 10   !14
                tv_colwidth_digits(i,10) = 5 ! 7
                tv_colwidth_digits(i,11) = 12  ! 14
                call TVtrimCol_width('treeview2')
            elseif('treeview3' == tvnames(i)%s) then
                tv_colwidth_digits(i,1) = 3
                tv_colwidth_digits(i,2) = 10
                tv_colwidth_digits(i,3) = 10
                tv_colwidth_digits(i,4) = 10
                tv_colwidth_digits(i,5) = 20
                tv_colwidth_digits(i,6) = 12
                call TVtrimCol_width('treeview3')
            elseif('treeview4' == tvnames(i)%s) then
                tv_colwidth_digits(i,1) = 3
                tv_colwidth_digits(i,2) = 10  ! 20
                tv_colwidth_digits(i,3) = 3
                tv_colwidth_digits(i,4) = 12 ! 14
                tv_colwidth_digits(i,5) = 12 !  14
                tv_colwidth_digits(i,7) = 12 ! 14
                tv_colwidth_digits(i,8) = 12 ! 14
                call TVtrimCol_width('treeview4')
            elseif('treeview5' == tvnames(i)%s) then
                tv_colwidth_digits(i,1) = 3
                tv_colwidth_digits(i,2) = 16
                do k=3,tvcols(i)
                    tv_colwidth_digits(i,3:12) = 8
                end do
            elseif('treeview6' == tvnames(i)%s) then
                tv_colwidth_digits(i,1) = 3
                do k=3,tvcols(i)
                    tv_colwidth_digits(i,k) = 12
                end do
            elseif('treeview7' == tvnames(i)%s) then
                tv_colwidth_digits(i,1) = 3
                tv_colwidth_digits(i,2) = 9
                tv_colwidth_digits(i,3) = 4
                do k=4,tvcols(i)
                    tv_colwidth_digits(i,k) = 10
                end do
            end if
        end do
        increase_dpafact = .false.
        LinTest = .false.

        call WDSetCheckButton('TRcheckbuttonBMT', 0)
        kcmx = 0
        kcrun = 0
        kbgv_binom = 0
        itm_binom = 0
        ip_binom = 0
        ilam_binom = 0
        use_bipoi = .false.

        nvarsMD = 0
        nmxDist = 0
        DistPars%ivtl(1:10) = 0
        DistPars%pval(1:10,:) = ZERO
        call gtk_widget_set_sensitive(idpt('TBDistribDialog'), 0_c_int)
        use_DP = .true.   ! .false.
        var_brutto_auto = .false.
        nvars_in_rbtot = 0
        rinflu_known = .false.
        refdataMD = 0
        k_datvar = 0
        kEGr_old = 0
        N_Preset = .false.
        use_dependent_sdwert = .false.
        replot_on = .false.
        knetp3 = 0
        nRnetp = 0
        var_rbtot = 0
        ndep = 0
        mac = 0
        nhp_defined = .false.
        defined_RSY = .false.
        Formeltext_out = .false.
        use_absTimeStart = .false.
        !!! PMLE_Routine = 2         ! 1:LSQmar;  2: Lm
        CCTitle = ' '                ! <--   addad 16.11.2024 GK
        nDCnet = 0                   ! 16.12.2024 GK


        apply_units = .false.
        ! apply_units = .true.
        FP_for_units = .false.
        if(apply_units) call ReadUnits()
        apply_units_dir = .false.
        if(apply_units) apply_units_dir = .true.
        modvar_on = .false.
        call gtk_widget_set_sensitive(idpt('CheckUnits'), 0_c_int)

        if(len_trim(fname) > 300 .and. len_trim(fname) == len(fname)) fname = ' '

        !----------------------------------------
        call StartAlloc()       ! of various allocatable arrays
        !----------------------------------------

        call LoadDecayChains()    ! 27.4.2025
        DChain = .false.          !
        nsubdec = 0               !

        call cpu_time(finish)

        if(incall == 1)  then
            write(log_str, '(A, F0.2)') 'INIT: cpu-time= ', finish-start
            call logger(66, log_str)
        end if

        compare_WTLS = .false.
        ! compare_WTLS = .true.     ! <--- this can produce much output in the file fort23.txt

        call logger(66, 'INIT_End.............................................................')

    end subroutine UncW_Init

    !########################################################################################

    subroutine read_cfg()

        ! this routine reads in the UncertRadio configuration parameters from the file UR2_cfg.dat
        !
        ! See chapter 1.3 "Program start" of the UncertRadio CHM Help file for more details.
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_params,        only: UR2_CFG_FILE
        use, intrinsic :: iso_c_binding,    only: c_int, c_ptr
        use ur_general_globals,     only: Help_Path, log_path, results_path, sDecimalPoint, &
                                    sListSeparator, sWindowsVersion, fname_getarg, sFontName, &
                                    sfontsize, work_path, automode

        use gtk_sup,          only: c_f_string
        use CHF,              only: ucase, flfu, lowercase
        use UR_gtk_globals, only: monitorUR
        use file_io,          only: logger
        use UR_Gleich_globals,        only: apply_units, FP_for_units

        use translation_module, only: set_language, T => get_translation
        use color_theme

        implicit none

        integer            :: i1, ios, i

        logical            :: prfound, contrast_mode
        character(len=2)   :: langg
        character(len=128) :: errmsg
        character(len=512) :: log_str, text, textG

        prfound = .false.
        sWindowsVersion = '7'
        fname_getarg = ''
        sFontName = 'Verdana 10'
        sFontSize = 10

        monitorUR = 0

        open(unit=32, file=flfu(work_path // UR2_CFG_FILE), status='old', action='read', iostat=ios)

        if(ios == 0) then
            read(32,'(a)') text
            text = ucase(text)

            if(index(text,'[UNCERTRADIO CONFIGURATION]') > 0) then
                do
                    read(32,'(a)', iostat=ios) text

                    if(ios /= 0) exit
                    text = ucase(text)
                    if(index(text,'[PATH]') > 0) then
                        read(32,'(a)',iostat=ios) text
                        if(ios /= 0) exit
                        textG = ucase(text)
                        if(index(textG,'HELP_PATH') > 0) then
                            i1 = index(text,'=')
                            if(i1 > 0) then
                                !Help_Path = trim(adjustL(text(i1+1:))) ! help_path is allready read
                                !             write(66,*) 'Found HELP_Path= ',trim(Help_Path)
                                write(log_str, '(*(g0))') 'Found HELP_Path= ',trim(Help_Path)
                                call logger(66, log_str)
                            end if
                        end if
                        read(32,'(a)',iostat=ios) text
                        if(ios /= 0) exit
                        textG = ucase(text)
                        if(index(textG,'LOG_PATH') > 0) then
                            i1 = index(text,'=')
                            if(i1 > 0) then
                                ! log_path = trim(adjustL(text(i1+1:))) ! log_path is allready read
                                !             write(66,*) 'Found log path= ',trim(log_path)
                                write(log_str, '(*(g0))') 'Found log path= ',trim(log_path)
                                call logger(66, log_str)
                            end if
                        end if

                        read(32,'(a)',iostat=ios) text
                        if(ios /= 0) exit
                        textG = ucase(text)
                        if(index(textG,'RESULTS_PATH') > 0) then
                            i1 = index(text,'=')
                            if(i1 > 0) then
                                ! results_path = trim(adjustL(text(i1+1:))) ! results_path is allready read
                                !             write(66,*) 'Found Results_Path=',trim(results_path)
                                write(log_str, '(*(g0))') 'Found Results_Path=',trim(results_path)
                                call logger(66, log_str)
                            end if
                        end if

                        backspace(32)
                        do
                            read(32,'(a)',iostat=ios) text
                            if(ios /= 0) exit
                            textG = ucase(text)
                            if(index(textG,'[LOCAL]') > 0) exit
                        end do

                        do
                            read(32,'(a)',iostat=ios) text
                            if(ios /= 0) exit
                            textG = ucase(text)
                            if(index(textG,'DECIMAL_POINT') > 0) then
                                i1 = index(text,'=')
                                if(i1 > 0 .and. trim(adjustL(text(i1+1:i1+1))) /= ' ') then
                                    if(.not.automode) sDecimalPoint = trim(adjustL(text(i1+1:i1+1)))
                                    !               write(66,*) 'sDecimalPoint found in cfg: ',sDecimalPoint
                                    write(log_str, '(*(g0))') 'sDecimalPoint found in cfg: ',sDecimalPoint
                                    call logger(66, log_str)
                                end if
                            else
                                backspace (32)
                            end if

                            read(32,'(a)',iostat=ios) text
                            if(ios /= 0) exit
                            textG = ucase(text)

                            if(index(textG,'LIST_SEPARATOR') > 0) then
                                i1 = index(text,'=')
                                if(i1 > 0 .and. trim(adjustL(text(i1+1:i1+1))) /= ' ') then
                                    if(.not.automode) sListSeparator = trim(adjustL(text(i1+1:i1+1)))
                                    write(log_str, '(*(g0))') 'sListSeparator found in cfg: ',sListSeparator
                                    call logger(66, log_str)
                                end if
                            else
                                backspace (32)
                            end if

                            read(32,'(a)',iostat=ios) text
                            if(ios /= 0) exit
                            textG = ucase(text)
                            if(index(textG,'LANGUAGE') > 0) then
                                i1 = index(textG,'=')
                                if(i1 > 0 .and. trim(adjustL(text(i1+1:i1+2))) /= '  ') then
                                    if(.not.automode) langg = trim(adjustL(lowercase(text(i1+1:i1+2))))
                                !                  write(66,*) 'language found in cfg: ',langg
                                    write(log_str, '(*(g0))') 'language found in cfg: ',langg
                                    call logger(66, log_str)
                                else

                                end if
                            else
                                backspace (32)
                            end if

                            if(.true.) then
                                monitorUR = 0
                                read(32,'(a)',iostat=ios) text

                                if(ios /= 0) exit
                                textG = ucase(text)
                                if(index(textG,'MONITOR#') > 0) then
                                    i1 = index(textG,'=')
                                    if(i1 > 0) then
                                        read(textG(i1+1:),*, iostat=ios) monitorUR
                                        if(ios /= 0) then
                                            write(*,*) 'Monitor# not defined'
                                            exit
                                        end if
                                        call logger(66, trim(textG))
                                        write(log_str, '(*(g0))') ' Monitor# found in cfg: ',int(MonitorUR,2)
                                        call logger(66, log_str)
                                    else
                                        ! exit         ! <-- deactivated because the inclusion of the Monitor#
                                    end if
                                else
                                    backspace (32)
                                end if
                            end if

                            if(.true.) then
                                contrast_mode = .false.
                                read(32,'(a)',iostat=ios,iomsg=errmsg) text

                                if(ios /= 0)  then
                                    write(log_str, '(*(g0))') 'ios=',int(ios,2),' text=',trim(text),'  err=',trim(errmsg)
                                    call logger(66, log_str)
                                end if
                                if(ios == 0) then
                                    textG = ucase(text)
                                    if(index(ucase(text),'CONTRASTMODE') > 0) then
                                        i1 = index(text,'=')
                                        if(i1 > 0) then
                                            read(textG(i1+1:i1+1),'(L1)',iostat=ios) contrast_mode
                                            if(ios /= 0) then
                                                call logger(66, 'contrastmode not defined')
                                                exit
                                            end if
                                            call logger(66, trim(textG))
                                            ! exit      ! ......
                                        end if
                                    else
                                        ! exit        ! ......
                                        backspace (32)
                                    end if
                                end if
                            end if

                            apply_units = .false.
                            if(.true.) then
                                apply_units = .false.
                                read(32,'(a)',iostat=ios,iomsg=errmsg) text
                                if(ios /= 0)  then
                                    write(log_str, '(*(g0))') 'ios=',int(ios,2),'  err=',trim(errmsg)
                                    call logger(66, log_str)
                                end if
                                if(ios == 0) then
                                    textG = ucase(text)
                                    if(index(ucase(text),'APPLY_UNITS') > 0) then
                                        i1 = index(text,'=')
                                        if(i1 > 0) then
                                            read(textG(i1+1:i1+1),'(L1)',iostat=ios) apply_units
                                            if(ios /= 0) then

                                                call logger(66, 'apply_units not defined')
                                                exit
                                            end if

                                            call logger(66, trim(textG))
                                            exit
                                        end if
                                    else
                                        exit
                                        ! backspace (32)
                                    end if
                                end if
                            end if

                            if(.false.) then
                                read(32,'(a)',iostat=ios) text
                                if(ios /= 0) exit
                                textG = ucase(text)
                                if(index(textG,'WINDOWS') > 0) then
                                    i1 = index(text,'=')
                                    if(i1 > 0) then
                                        sWindowsVersion = trim(adjustL(text(i1+1:i1+5)))
                                        call logger(66, " sWindowsVersion found in cfg: " // sWindowsVersion)
                                    end if
                                else
                                    backspace (32)
                                end if

                                read(32,'(a)',iostat=ios) text
                                if(ios /= 0) exit
                                textG = ucase(text)
                                if(index(textG,'FONTNAME') > 0) then
                                    i1 = index(text,'=')
                                    if(i1 > 0) then
                                        sFontName = trim(adjustL(text(i1+1:i1+25)))
                                        call logger(66, " sFontName found in cfg: " // sFontName)
                                        do i=len_trim(sFontName),3,-1
                                            if(sFontName(i:i) == ' ') then
                                                read(sFontName(i+1:),*) sFontSize
                                                exit
                                            end if
                                        end do
                                    end if
                                end if
                            end if
                        end do     ! endless loop
                    end if
                end do
            end if
            close (32)
            call logger(66, 'Configuration file UR2_cfg.dat read!')

        else
            call logger(66, 'Configuration file UR2_cfg.dat not found!')
            prfound = .false.
        end if

        ! if the language is not defined in the UR_config file or is not known by UR,
        ! try to use the user/system language
        if (.not. any(langg == ['de', 'en', 'fr'] )) then
            call get_environment_variable("LANG", langg)
            if ( .not. any((langg) == ['de', 'en', 'fr'] )) then
                langg = 'en' ! set a fall-back language, atm english
                call logger(66, "Warning: $LANG not defined, falling back to: " // langg)
            endif
        end if

        call set_language(lowercase(langg))
        if ( .not. automode) then
            sDecimalPoint = T('.')
        end if


        ! set the theme (contrast mode or default at the moment)
        if (contrast_mode) then
            call set_color_theme('contrast')
        else
            call set_color_theme('default')
        end if

        if(apply_units) FP_for_units = .true.
    end subroutine READ_CFG

!#################################################################################

    subroutine GtkSettingsIO(read, ifehl)

        ! this routine reads in the small file Settings.ini with GTK settings
        !
        ! See chapter 1.3 "Program start" and 3.7 "Font and colors" of the UncertRadio
        ! CHM Help file for more details.
        !
        !     Copyright (C) 2014-2025  Günter Kanisch

        use ur_general_globals, only: work_path

        use UR_gtk_globals, only: Settings, fontnameSV
        use CHF,            only: ucase, flfu
        use file_io,        only: logger

        implicit none

        logical, intent(in)  :: read       ! .true. :  read;   .false. : save
        integer, intent(out) :: ifehl

        integer              :: i0,ios,i
        character(len=512)   :: log_str
        character(len=256)   :: file,text

        ifehl = 0

        file = flfu(work_path // 'Settings.ini')
        if(read) then

            write(log_str, '(*(g0))') 'file=',trim(file)
            call logger(66, log_str)
            open (34,FILE=file, STATUS='old',IOSTAT=ios)
            if(ios == 0) THEN
                read(34,'(a)') text
                text = ucase(text)

                if(index(text,'[SETTINGS]') > 0) then
                    Settings%nprops = 0
                    do
                        read(34,'(a)',iostat=ios) text
                        if(ios /= 0) exit
                        if(len_trim(text) == 0) cycle
                        i0 = index(text,'=')
                        if(i0 > 0) then
                            Settings%nprops = Settings%nprops + 1
                            read(text(1:i0-1),'(a)',iostat=ios) Settings%sproperty(Settings%nprops)
                            read(text(i0+1:),'(a)',iostat=ios) Settings%sproperty_val(Settings%nprops)
                            if(trim(Settings%sproperty(Settings%nprops)) == 'gtk-font-name') &
                                fontnameSV = trim(Settings%sproperty_val(Settings%nprops))
                        end if
                    end do
                end if
            else
                ifehl = 1

                write(log_str, '(*(g0))') 'Gsettings file not found: ',trim(file)
                call logger(66, log_str)
            end if
            close (34)
        else
            open (34,FILE=trim(file), STATUS='unknown',IOSTAT=ios)
            if(ios == 0) then
                call logger(34, '[Settings]')
                do i=1,Settings%nprops
                    write(log_str, '(a,a,a)') trim(Settings%sproperty(i)),' = ',trim(Settings%sproperty_val(i))
                    call logger(34, log_str)
                end do
            else
                ifehl = 1
                write(log_str, '(*(g0))') 'Gsettings: file could not be opened for saving: ',trim(file)
                call logger(66, log_str)
            end if
        end if
        close (34)

    end subroutine GtkSettingsIO

!#################################################################################

    subroutine TVtrimCol_width(tvname)

        ! this routine "tries" to set limits for the width values of treeview columns;
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_Gleich_globals,        only: loadingPro
        use gtk,              only: gtk_tree_view_column_set_min_width, gtk_tree_view_column_set_max_width, &
                                    gtk_tree_view_columns_autosize
        use UR_gtk_globals, only: tvnames,tvcolindex,tv_colwidth_digits,tvcols
        use Top,              only: idpt, PixelPerString
        use CHF,              only: FindlocT
        implicit none

        character(len=*),intent(in)  :: tvname

        integer           :: kwd,kht,pixel_per_char,n,cwidth,nt
        character(len=3)  :: chcol

        if(loadingPro) return

        if(trim(tvname) /= 'treeview3') then
            call gtk_tree_view_columns_autosize(idpt(tvname))
            return
        end if

        ! limiting the widths of the columns
        nt = FindlocT(tvnames, trim(tvname))

        call PixelPerString(idpt(tvnames(nt)%s), '123456789E-02123456789E-02',kwd,kht)
        pixel_per_char = int(real(kwd)/real(len_trim('123456789E-02123456789E-02'))) + 1

        do n=2,tvcols(nt)
            write(chcol,'(i0)') tvcolindex(nt,n)
            cwidth = (tv_colwidth_digits(nt,n) + 2) * pixel_per_char
            !write(66,*) trim(tvnames(nt)),': col=',int(n,2),' cwidth=',cwidth,   &
            !           ' tv_colwidth_digits=',tv_colwidth_digits(nt,n), &
            !           ' tv_colwidth_pixel=',tv_colwidth_pixel(nt,n)
            call gtk_tree_view_column_set_max_width(idpt('treeviewcolumn' // trim(adjustL(chcol))),cwidth )
            cwidth = 8 * pixel_per_char
            call gtk_tree_view_column_set_min_width(idpt('treeviewcolumn' // trim(adjustL(chcol))),cwidth )
        end do
        ! call gtk_tree_view_columns_autosize(idpt(tvnames(nt)))

    end subroutine TVtrimCol_width

!########################################################################################

    subroutine StartAlloc()

        ! this routine performs a lot initial allocations of a lot of arrays
        !
        !     Copyright (C) 2020-2023  Günter Kanisch

        USE UR_Gleich_globals
        USE UR_Linft
        USE UR_Gspk1fit
        USE UR_MCC,           ONLY: mcafull,mcafull2,mcafull3,mwnet, &
                                    mwnetmit,mwnetmitq,mwnetvgl,xsdnet,xsdvsgl, &
                                    xmitsgl,umwnetvgl,xplt,yplt

        use UR_MCSR,          only: d0zrateSicher,sd0zrateSicher,MesswertORG,StduncORG,MEsswertkq,StdUnckq, &
                                    MEsswert_eg,Messwertw,GNetRateSicher,SDGNetRateSicher,RateCBSV,RateBGSV, &
                                    effiSV,pgammSV,fattSV,fcoinsuSV,relSdSv,xzmit,covariter,kv1,muvect0,  &
                                    ivref,rnetvar,d0zrateZ,rblindnetZ,rxzmit,xzsdv,rxzsdv,xzLQ,rxzLQ,     &
                                    xzUQ,rxzUQ,xzDL,kgl,muvectR,sx

        use Top,              only: idpt
        use UR_perror,        only: symb_new
        use UR_DecChain,      only: DCList,DCnuclide,DCsymbEffiA,DCsymbEffiB,DCsymbEffiC,DCsymbYield, &
                                    DCsymbT12,DCsymbLambda,DCindx

        implicit none

        character(len=80)       :: cdummy

        if(allocated(Titeltext)) deallocate(Titeltext);
        if(allocated(Formeltext)) deallocate(Formeltext);
        if(allocated(formelt)) deallocate(formelt);  allocate(formelt(0)); deallocate(formelt);
        if(allocated(Rseite)) deallocate(Rseite);  allocate(Rseite(0)); deallocate(Rseite);
        if(allocated(formeltextfit)) deallocate(formeltextfit);

        if(allocated(RSeite)) deallocate(RSeite);  allocate(RSeite(0)); deallocate(RSeite);
        if(allocated(LSeite)) deallocate(LSeite);  allocate(LSeite(0)); deallocate(LSeite);
        if(allocated(Varab)) deallocate(varab);  allocate(varab(0)); deallocate(varab);
        if(allocated(varmu)) deallocate(varmu);  allocate(varmu(0)); deallocate(varmu);
        if(allocated(RSSy)) deallocate(RSsy);  allocate(RSsy(0)); deallocate(RSsy);
        if(allocated(nRSsyanf)) deallocate(nRSsyanf);  allocate(nRSsyanf(0)); deallocate(nRSsyanf);
        if(allocated(nRSsy)) deallocate(nRSsy);  allocate(nRSsy(0)); deallocate(nRSsy);
        if(allocated(dpi1v)) deallocate(dpi1v);  allocate(dpi1v(0)); deallocate(dpi1v);
        if(allocated(dpi2v)) deallocate(dpi2v);  allocate(dpi2v(0)); deallocate(dpi2v);
        if(allocated(WPars)) deallocate(WPars);  allocate(WPars(0)); deallocate(WPars);
        if(allocated(WParsInd)) deallocate(WParsInd);  allocate(WParsInd(0)); deallocate(WParsInd);
        if(allocated(CovyLF)) deallocate(covyLF);  allocate(covyLF(0,0)); deallocate(covyLF);
        if(allocated(cov_fixed)) deallocate(cov_fixed);  allocate(cov_fixed(0,0)); deallocate(cov_fixed);
        if(allocated(covpp)) deallocate(covpp);  allocate(covpp(0,0)); deallocate(covpp);
        if(allocated(covx)) deallocate(covx);  allocate(covx(0,0)); deallocate(covx);
        if(allocated(Qsumarr)) deallocate(Qsumarr);  allocate(Qsumarr(0)); deallocate(Qsumarr);

        if(allocated(covar)) deallocate(covar);  allocate(covar(0,0)); deallocate(covar);

        if(allocated(RS_SymbolNr)) deallocate(RS_SymbolNr);
        if(allocated(RS_Ops)) deallocate(RS_Ops);
        if(allocated(RS_OpsPos)) deallocate(RS_OpsPos);

        if(allocated(Symbole)) deallocate(Symbole);  allocate(Symbole(0)); deallocate(Symbole);
        if(allocated(SymboleG)) deallocate(SymboleG);  allocate(SymboleG(0)); deallocate(SymboleG);
        if(allocated(symtyp)) Deallocate(symtyp);  allocate(symtyp(0)); deallocate(symtyp);
        if(allocated(einheit)) Deallocate(einheit);  allocate(einheit(0)); deallocate(einheit);
        if(allocated(einheitSV)) Deallocate(einheitSV);  allocate(einheitSV(0)); deallocate(einheitSV);
        if(allocated(Bedeutung)) Deallocate(Bedeutung);  allocate(Bedeutung(0)); deallocate(Bedeutung);
        if(allocated(IVTL)) Deallocate(IVTL);  allocate(IVTL(0)); deallocate(IVTL);
        if(allocated(Messwert)) Deallocate(Messwert);  allocate(Messwert(0)); deallocate(Messwert);
        if(allocated(SDFormel)) Deallocate(SDFormel);  allocate(SDFormel(0)); deallocate(SDFormel);
        if(allocated(SDwert)) Deallocate(SDwert);  allocate(SDwert(0)); deallocate(SDwert);
        if(allocated(HBreite)) Deallocate(HBreite);  allocate(HBreite(0)); deallocate(HBreite);
        if(allocated(IAR)) Deallocate(IAR);  allocate(IAR(0)); deallocate(IAR);
        if(allocated(StdUnc)) Deallocate(StdUnc);  allocate(StdUnc(0)); deallocate(StdUnc);
        if(allocated(Sensi)) Deallocate(Sensi);  allocate(Sensi(0)); deallocate(Sensi);
        if(allocated(perc)) Deallocate(perc);  allocate(perc(0)); deallocate(perc);

        if(allocated(Symbole_CP)) Deallocate(Symbole_CP);  allocate(Symbole_CP(0)); deallocate(Symbole_CP);
        if(allocated(symtyp_CP)) Deallocate(symtyp_CP);  allocate(symtyp_CP(0)); deallocate(symtyp_CP);
        if(allocated(einheit_CP)) Deallocate(einheit_CP);  allocate(einheit_CP(0)); deallocate(einheit_CP);
        if(allocated(Bedeutung_CP)) Deallocate(Bedeutung_CP);  allocate(Bedeutung_CP(0)); deallocate(Bedeutung_CP);
        if(allocated(IVTL_CP)) Deallocate(IVTL_CP);  allocate(IVTL_CP(0)); deallocate(IVTL_CP);
        if(allocated(Messwert_CP)) Deallocate(Messwert_CP);  allocate(Messwert_CP(0)); deallocate(Messwert_CP);
        if(allocated(SDFormel_CP)) Deallocate(SDFormel_CP);  allocate(SDFormel_CP(0)); deallocate(SDFormel_CP);
        if(allocated(SDwert_CP)) Deallocate(SDwert_CP);  allocate(SDwert_CP(0)); deallocate(SDwert_CP);
        if(allocated(HBreite_CP)) Deallocate(HBreite_CP);  allocate(HBreite_CP(0)); deallocate(HBreite_CP);
        if(allocated(IAR_CP)) Deallocate(IAR_CP);  allocate(IAR_CP(0)); deallocate(IAR_CP);
        if(allocated(StdUnc_CP)) Deallocate(StdUnc_CP);  allocate(StdUnc_CP(0)); deallocate(StdUnc_CP);
        if(allocated(Sensi_CP)) Deallocate(Sensi_CP);  allocate(Sensi_CP(0)); deallocate(Sensi_CP);
        if(allocated(perc_CP)) Deallocate(perc_CP);  allocate(perc_CP(0)); deallocate(perc_CP);

        if(allocated(MesswertSV)) deallocate(MesswertSV);  allocate(MesswertSV(0)); deallocate(MesswertSV);
        if(allocated(StdUncSV)) deallocate(StdUncSV);  allocate(StdUncSV(0)); deallocate(StdUncSV);

        if(allocated(Rseite)) deallocate(Rseite);  allocate(Rseite(0)); deallocate(Rseite);
        if(allocated(Lseite)) deallocate(Lseite);  allocate(Lseite(0)); deallocate(Lseite);

        if(allocated(IsymbA)) Deallocate(IsymbA);  allocate(IsymbA(0)); deallocate(IsymbA);
        if(allocated(IsymbB)) Deallocate(IsymbB);  allocate(IsymbB(0)); deallocate(IsymbB);
        if(allocated(icovtyp)) Deallocate(icovtyp);  allocate(icovtyp(0)); deallocate(icovtyp);
        if(allocated(CVformel)) Deallocate(CVformel);  allocate(CVformel(0)); deallocate(CVformel);
        if(allocated(Covarval)) Deallocate(CovarVal);  allocate(CovarVal(0)); deallocate(CovarVal);
        if(allocated(corrval)) Deallocate(corrval);  allocate(corrval(0)); deallocate(corrval);
        if(allocated(SymboleA)) deallocate(SymboleA);  allocate(SymboleA(0)); deallocate(SymboleA);
        if(allocated(SymboleB)) deallocate(SymboleB);  allocate(SymboleB(0)); deallocate(SymboleB);
        if(allocated(CovarvalSV)) Deallocate(CovarValSV);  allocate(CovarValSV(0)); deallocate(CovarValSV);

        if(allocated(iptr_time)) deallocate(iptr_time); allocate(iptr_time(1));  iptr_time(1) = 0;
        if(allocated(iptr_cnt)) deallocate(iptr_cnt); allocate(iptr_cnt(1));   iptr_cnt(1) = 0;
        if(allocated(is_count)) deallocate(is_count); allocate(is_count(1));   is_count(1) = .false.;
        if(allocated(iptr_rate)) deallocate(iptr_rate); allocate(iptr_rate(1));   iptr_rate(1) = 0;

        if(allocated(mpfx)) deallocate(mpfx);  allocate(mpfx(0)); deallocate(mpfx);
        if(allocated(mpfx_ind)) deallocate(mpfx_ind);  allocate(mpfx_ind(0)); deallocate(mpfx_ind);
        if(allocated(mpfxfixed)) deallocate(mpfxfixed);  allocate(mpfxfixed(0)); deallocate(mpfxfixed);
        if(allocated(mpfx_extern)) deallocate(mpfx_extern);  allocate(mpfx_extern(0)); deallocate(mpfx_extern);
        if(allocated(eqnum)) deallocate(eqnum);  allocate(eqnum(0)); deallocate(eqnum);
        if(allocated(synum)) deallocate(synum);  allocate(synum(0)); deallocate(synum);
        if(allocated(opnum)) deallocate(opnum);  allocate(opnum(0)); deallocate(opnum);
        ndep = 0

        if(allocated(fixedrate)) deallocate(fixedrate);  allocate(fixedrate(0)); deallocate(fixedrate);
        if(allocated(SDfixedrate)) deallocate(SDfixedrate);  allocate(SDfixedrate(0)); deallocate(SDfixedrate);
        if(allocated(dnetfit)) deallocate(dnetfit);  allocate(dnetfit(0)); deallocate(dnetfit);
        if(allocated(sdnetfit)) deallocate(sdnetfit);  allocate(sdnetfit(0)); deallocate(sdnetfit);

        if(allocated(aktnz)) deallocate(aktnz);  allocate(aktnz(0)); deallocate(aktnz);
        if(allocated(SDaktnz)) deallocate(SDaktnz);  allocate(SDaktnz(0)); deallocate(SDaktnz);
        if(allocated(varadd_rn)) deallocate(varadd_rn);  allocate(varadd_rn(0)); deallocate(varadd_rn);
        if(allocated(SDaktnzSV)) deallocate(SDaktnzSV);  allocate(SDaktnzSV(0)); deallocate(SDaktnzSV);
        if(allocated(SDaktnzMV)) deallocate(SDaktnzMV);  allocate(SDaktnzMV(0)); deallocate(SDaktnzMV);
        if(allocated(UxaMV)) deallocate(UxaMV);
        if(allocated(Uxa)) deallocate(Uxa);

        if(allocated(dmesszeit)) deallocate(dmesszeit);  allocate(dmesszeit(0)); deallocate(dmesszeit);
        if(allocated(dbimpulse)) deallocate(dbimpulse);  allocate(dbimpulse(0)); deallocate(dbimpulse);
        if(allocated(dbzrate)) deallocate(dbzrate);  allocate(dbzrate(0)); deallocate(dbzrate);
        if(allocated(sdbzrate)) deallocate(sdbzrate);  allocate(sdbzrate(0)); deallocate(sdbzrate);
        if(allocated(d0messzeit)) deallocate(d0messzeit);  allocate(d0messzeit(0)); deallocate(d0messzeit);
        if(allocated(d0impulse)) deallocate(d0impulse);  allocate(d0impulse(0)); deallocate(d0impulse);
        if(allocated(d0zrate)) deallocate(d0zrate);  allocate(d0zrate(0)); deallocate(d0zrate);
        if(allocated(sd0zrate)) deallocate(sd0zrate);  allocate(sd0zrate(0)); deallocate(sd0zrate);
        if(allocated(dnetrate)) deallocate(dnetrate);  allocate(dnetrate(0)); deallocate(dnetrate);
        if(allocated(sdnetrate)) deallocate(sdnetrate);  allocate(sdnetrate(0)); deallocate(sdnetrate);
        if(allocated(CStartzeit)) deallocate(CStartzeit);  allocate(CStartzeit(0)); deallocate(CStartzeit);

        if(allocated(dmesszeit_CP)) deallocate(dmesszeit_CP);  allocate(dmesszeit_CP(0)); deallocate(dmesszeit_CP);
        if(allocated(dbimpulse_CP)) deallocate(dbimpulse_CP);  allocate(dbimpulse_CP(0)); deallocate(dbimpulse_CP);
        if(allocated(dbzrate_CP)) deallocate(dbzrate_CP);  allocate(dbzrate_CP(0)); deallocate(dbzrate_CP);
        if(allocated(sdbzrate_CP)) deallocate(sdbzrate_CP);  allocate(sdbzrate_CP(0)); deallocate(sdbzrate_CP);
        if(allocated(d0messzeit_CP)) deallocate(d0messzeit_CP);  allocate(d0messzeit_CP(0)); deallocate(d0messzeit_CP);
        if(allocated(d0impulse_CP)) deallocate(d0impulse_CP);  allocate(d0impulse_CP(0)); deallocate(d0impulse_CP);
        if(allocated(d0zrate_CP)) deallocate(d0zrate_CP);  allocate(d0zrate_CP(0)); deallocate(d0zrate_CP);
        if(allocated(sd0zrate_CP)) deallocate(sd0zrate_CP);  allocate(sd0zrate_CP(0)); deallocate(sd0zrate_CP);
        if(allocated(dnetrate_CP)) deallocate(dnetrate_CP);  allocate(dnetrate_CP(0)); deallocate(dnetrate_CP);
        if(allocated(sdnetrate_CP)) deallocate(sdnetrate_CP);  allocate(sdnetrate_CP(0)); deallocate(sdnetrate_CP);
        if(allocated(CStartzeit_CP)) deallocate(CStartzeit_CP);  allocate(CStartzeit_CP(0)); deallocate(CStartzeit_CP);

        if(allocated(d0zrateSicher)) deallocate(d0zrateSicher);  allocate(d0zrateSicher(0)); deallocate(d0zrateSicher);
        if(allocated(sd0zrateSicher)) deallocate(sd0zrateSicher);  allocate(sd0zrateSicher(0)); deallocate(sd0zrateSicher);
        if(allocated(d0zrateSV)) deallocate(d0zrateSV);  allocate(d0zrateSV(0)); deallocate(d0zrateSV);
        if(allocated(sd0zrateSV)) deallocate(sd0zrateSV);  allocate(sd0zrateSV(0)); deallocate(sd0zrateSV);

        if(allocated(fixedRateMC)) deallocate(fixedRateMC);  allocate(fixedRateMC(0)); deallocate(fixedRateMC);

        if(allocated(nvalsMD)) deallocate(nvalsMD);  allocate(nvalsMD(0)); deallocate(nvalsMD);
        if(allocated(meanMD)) deallocate(meanMD);  allocate(meanMD(0)); deallocate(meanMD);
        if(allocated(meanID)) deallocate(meanID);  allocate(meanID(0)); deallocate(meanID);
        if(allocated(MDpoint)) deallocate(MDpoint);  allocate(MDpoint(0)); deallocate(MDpoint);
        if(allocated(MDpointrev)) deallocate(MDpointrev);  allocate(MDpointrev(0)); deallocate(MDpointrev);
        if(allocated(MDused)) deallocate(MDused);  allocate(MDused(0)); deallocate(MDused);
        if(allocated(fBayMD)) deallocate(fBayMD);  allocate(fBayMD(0)); deallocate(fBayMD);
        if(allocated(umeanMD)) deallocate(umeanMD);  allocate(umeanMD(0)); deallocate(umeanMD);
        if(allocated(k_MDtyp)) deallocate(k_MDtyp);  allocate(k_MDtyp(0)); deallocate(k_MDtyp);
        if(allocated(Distpars%symb)) deallocate(Distpars%symb);  allocate(Distpars%symb(0)); deallocate(Distpars%symb);
        if(allocated(nvMD)) deallocate(nvMD);  allocate(nvMD(0)); deallocate(nvMD);
        if(allocated(smeanMD)) deallocate(smeanMD);  allocate(smeanMD(0)); deallocate(smeanMD);
        if(allocated(umeanMD)) deallocate(umeanMD);  allocate(umeanMD(0)); deallocate(umeanMD);
        if(allocated(xdataMD)) deallocate(xdataMD);  allocate(xdataMD(0)); deallocate(xdataMD);
        if(allocated(ixdanf)) deallocate(ixdanf);  allocate(ixdanf(0)); deallocate(ixdanf);

        if(allocated(dtdiff)) deallocate(dtdiff);  allocate(dtdiff(0)); deallocate(dtdiff);
        if(allocated(guse_CP)) deallocate(guse_CP);  allocate(guse_CP(0)); deallocate(guse_CP);
        if(allocated(DistPars%symb)) deallocate(DistPars%symb);  allocate(DistPars%symb(0)); deallocate(DistPars%symb);

        if(allocated(covyLF)) deallocate(covyLF);  allocate(covyLF(0,0)); deallocate(covyLF);
        if(allocated(Qsumarr)) deallocate(Qsumarr);  allocate(Qsumarr(0)); deallocate(Qsumarr);

        if(allocated(mcafull)) deallocate(mcafull);  allocate(mcafull(0,0)); deallocate(mcafull);
        if(allocated(mcafull2)) deallocate(mcafull2);  allocate(mcafull2(0)); deallocate(mcafull2);
        if(allocated(mcafull3)) deallocate(mcafull3);  allocate(mcafull3(0)); deallocate(mcafull3);

        if(allocated(MesswertOrg)) deallocate(MesswertOrg);  allocate(MesswertOrg(0)); deallocate(MesswertOrg);
        if(allocated(StdUncOrg)) deallocate(StdUncOrg);  allocate(StdUncOrg(0)); deallocate(StdUncOrg);
        if(allocated(Messwertkq)) deallocate(Messwertkq);  allocate(Messwertkq(0)); deallocate(Messwertkq);
        if(allocated(StdUnckq)) deallocate(StdUnckq);  allocate(StdUnckq(0)); deallocate(StdUnckq);
        if(allocated(Messwert_eg)) deallocate(Messwert_eg);  allocate(Messwert_eg(0)); deallocate(Messwert_eg);
        if(allocated(Messwertw)) deallocate(Messwertw);  allocate(Messwertw(0)); deallocate(Messwertw);

        if(allocated(mwnet)) deallocate(mwnet);  allocate(mwnet(0)); deallocate(mwnet);
        if(allocated(xsdnet)) deallocate(xsdnet);  allocate(xsdnet(0)); deallocate(xsdnet);
        if(allocated(mwnetmit)) deallocate(mwnetmit);  allocate(mwnetmit(0)); deallocate(mwnetmit);
        if(allocated(mwnetmitq)) deallocate(mwnetmitq);  allocate(mwnetmitq(0)); deallocate(mwnetmitq);
        if(allocated(dnetfit)) deallocate(dnetfit);  allocate(dnetfit(0)); deallocate(dnetfit);
        if(allocated(sdnetfit)) deallocate(sdnetfit);  allocate(sdnetfit(0)); deallocate(sdnetfit);
        if(allocated(mwnetvgl)) deallocate(mwnetvgl);  allocate(mwnetvgl(0)); deallocate(mwnetvgl);
        if(allocated(umwnetvgl)) deallocate(umwnetvgl);  allocate(umwnetvgl(0)); deallocate(umwnetvgl);

        if(allocated(GnetRateSV)) deallocate(GnetRateSV);  allocate(GnetRateSV(0)); deallocate(GnetRateSV);
        if(allocated(SDGnetRateSV)) deallocate(SDGnetRateSV);  allocate(SDGnetRateSV(0)); deallocate(SDGnetRateSV);
        if(allocated(GnetRateSicher)) deallocate(GnetRateSicher);  allocate(GnetRateSicher(0)); deallocate(GnetRateSicher);
        if(allocated(SDGnetRateSicher)) deallocate(SDGnetRateSicher);  allocate(SDGnetRateSicher(0)); deallocate(SDGnetRateSicher);

        if(allocated(erg)) deallocate(erg);  allocate(erg(0)); deallocate(erg);
        if(allocated(GnetRate)) deallocate(GnetRate);  allocate(GnetRate(0)); deallocate(GnetRate);
        if(allocated(SDGnetRate)) deallocate(SDGnetRate);  allocate(SDGnetRate(0)); deallocate(SDGnetRate);
        if(allocated(RateCB)) deallocate(RateCB);  allocate(RateCB(0)); deallocate(RateCB);
        if(allocated(RateBG)) deallocate(RateBG);  allocate(RateBG(0)); deallocate(RateBG);
        if(allocated(SDRateBG)) deallocate(SDRateBG);  allocate(SDRateBG(0)); deallocate(SDRateBG);
        if(allocated(effi)) deallocate(effi);  allocate(effi(0)); deallocate(effi);
        if(allocated(sdeffi)) deallocate(sdeffi);  allocate(sdeffi(0)); deallocate(sdeffi);
        if(allocated(pgamm)) deallocate(pgamm);  allocate(pgamm(0)); deallocate(pgamm);
        if(allocated(sdpgamm)) deallocate(sdpgamm);  allocate(sdpgamm(0)); deallocate(sdpgamm);
        if(allocated(fatt)) deallocate(fatt);  allocate(fatt(0)); deallocate(fatt);
        if(allocated(sdfatt)) deallocate(sdfatt);  allocate(sdfatt(0)); deallocate(sdfatt);
        if(allocated(fcoinsu)) deallocate(fcoinsu);  allocate(fcoinsu(0)); deallocate(fcoinsu);
        if(allocated(sdfcoinsu)) deallocate(sdfcoinsu);  allocate(sdfcoinsu(0)); deallocate(sdfcoinsu);
        if(allocated(RateCBSV)) deallocate(RateCBSV);  allocate(RateCBSV(0)); deallocate(RateCBSV);
        if(allocated(RateBGSV)) deallocate(RateBGSV);  allocate(RateBGSV(0)); deallocate(RateBGSV);
        if(allocated(effiSV)) deallocate(effiSV);  allocate(effiSV(0)); deallocate(effiSV);
        if(allocated(pgammSV)) deallocate(pgammSV);  allocate(pgammSV(0)); deallocate(pgammSV);
        if(allocated(fattSV)) deallocate(fattSV);  allocate(fattSV(0)); deallocate(fattSV);
        if(allocated(fcoinsuSV)) deallocate(fcoinsuSV);  allocate(fcoinsuSV(0)); deallocate(fcoinsuSV);
        if(allocated(GnetRateSV)) deallocate(GnetRateSV);  allocate(GnetRateSV(0)); deallocate(GnetRateSV);
        if(allocated(SDGnetRateSV)) deallocate(SDGnetRateSV);  allocate(SDGnetRateSV(0)); deallocate(SDGnetRateSV);
        if(allocated(mwopt)) deallocate(mwopt);  allocate(mwopt(0)); deallocate(mwopt);

        write(cdummy,*) 'ubound(guse,dim=1)=',ubound(guse,dim=1),'  Lbound(guse,dim=1)=',Lbound(guse,dim=1)
        if(allocated(guse)) deallocate(guse);
        allocate(guse(0));
        deallocate(guse);

        if(allocated(erg_CP)) deallocate(erg_CP);  allocate(erg_CP(0)); deallocate(erg_CP);
        if(allocated(GnetRate_CP)) deallocate(GnetRate_CP);  allocate(GnetRate_CP(0)); deallocate(GnetRate_CP);
        if(allocated(SDGnetRate_CP)) deallocate(SDGnetRate_CP);  allocate(SDGnetRate_CP(0)); deallocate(SDGnetRate_CP);
        if(allocated(RateCB_CP)) deallocate(RateCB_CP);  allocate(RateCB_CP(0)); deallocate(RateCB_CP);
        if(allocated(RateBG_CP)) deallocate(RateBG_CP);  allocate(RateBG_CP(0)); deallocate(RateBG_CP);
        if(allocated(SDRateBG_CP)) deallocate(SDRateBG_CP);  allocate(SDRateBG_CP(0)); deallocate(SDRateBG_CP);
        if(allocated(effi_CP)) deallocate(effi_CP);  allocate(effi_CP(0)); deallocate(effi_CP);
        if(allocated(sdeffi_CP)) deallocate(sdeffi_CP);  allocate(sdeffi_CP(0)); deallocate(sdeffi_CP);
        if(allocated(pgamm_CP)) deallocate(pgamm_CP);  allocate(pgamm_CP(0)); deallocate(pgamm_CP);
        if(allocated(sdpgamm_CP)) deallocate(sdpgamm_CP);  allocate(sdpgamm_CP(0)); deallocate(sdpgamm_CP);
        if(allocated(fatt_CP)) deallocate(fatt_CP);  allocate(fatt_CP(0)); deallocate(fatt_CP);
        if(allocated(sdfatt_CP)) deallocate(sdfatt_CP);  allocate(sdfatt_CP(0)); deallocate(sdfatt_CP);
        if(allocated(fcoinsu_CP)) deallocate(fcoinsu_CP);  allocate(fcoinsu_CP(0)); deallocate(fcoinsu_CP);
        if(allocated(sdfcoinsu_CP)) deallocate(sdfcoinsu_CP);  allocate(sdfcoinsu_CP(0)); deallocate(sdfcoinsu_CP);
        if(allocated(RateCBSV)) deallocate(RateCBSV);  allocate(RateCBSV(0)); deallocate(RateCBSV);
        if(allocated(RateBGSV)) deallocate(RateBGSV);  allocate(RateBGSV(0)); deallocate(RateBGSV);
        if(allocated(effiSV)) deallocate(effiSV);  allocate(effiSV(0)); deallocate(effiSV);
        if(allocated(pgammSV)) deallocate(pgammSV);  allocate(pgammSV(0)); deallocate(pgammSV);
        if(allocated(fattSV)) deallocate(fattSV);  allocate(fattSV(0)); deallocate(fattSV);
        if(allocated(fcoinsuSV)) deallocate(fcoinsuSV);  allocate(fcoinsuSV(0)); deallocate(fcoinsuSV);
        if(allocated(guse_CP)) deallocate(guse_CP);  allocate(guse_CP(0)); deallocate(guse_CP);

        if(allocated(xkalib)) deallocate(xkalib);  allocate(xkalib(0)); deallocate(xkalib);
        if(allocated(ykalib)) deallocate(ykalib);  allocate(ykalib(0)); deallocate(ykalib);
        if(allocated(uxkalib)) deallocate(uxkalib);  allocate(uxkalib(0)); deallocate(uxkalib);
        if(allocated(uykalib)) deallocate(uykalib);  allocate(uykalib(0)); deallocate(uykalib);
        if(allocated(ykalibSV)) deallocate(ykalibSV);  allocate(ykalibSV(0)); deallocate(ykalibSV);

        if(allocated(a_kalib)) deallocate(A_kalib);  allocate(a_kalib(0)); deallocate(A_kalib);
        if(allocated(a_kalibsv)) deallocate(A_kalibsv);  allocate(a_kalibsv(0)); deallocate(A_kalibsv);
        if(allocated(covar_kalib)) deallocate(covar_kalib);  allocate(covar_kalib(0,0)); deallocate(covar_kalib);
        if(allocated(covar_kalibSV)) deallocate(covar_kalibSV);  allocate(covar_kalibSV(0,0)); deallocate(covar_kalibSV);
        if(allocated(ifitKB)) deallocate(ifitKB);  allocate(ifitKB(0)); deallocate(ifitKB);

        if(allocated(relSDSV)) deallocate(relSDSV);  allocate(relSDSV(0)); deallocate(relSDSV);

        if(allocated(xmitsgl)) deallocate(xmitsgl);  allocate(xmitsgl(0)); deallocate(xmitsgl);
        if(allocated(xsdvsgl)) deallocate(xsdvsgl);  allocate(xsdvsgl(0)); deallocate(xsdvsgl);
        if(allocated(xzmit)) deallocate(xzmit);  allocate(xzmit(0)); deallocate(xzmit);
        if(allocated(rxzmit)) deallocate(rxzmit);  allocate(rxzmit(0)); deallocate(rxzmit);
        if(allocated(xzsdv)) deallocate(xzsdv);  allocate(xzsdv(0)); deallocate(xzsdv);
        if(allocated(rxzsdv)) deallocate(rxzsdv);  allocate(rxzsdv(0)); deallocate(rxzsdv);
        if(allocated(xzLQ)) deallocate(xzLQ);  allocate(xzLQ(0)); deallocate(xzLQ);
        if(allocated(rxzLQ)) deallocate(rxzLQ);  allocate(rxzLQ(0)); deallocate(rxzLQ);
        if(allocated(xzUQ)) deallocate(xzUQ);  allocate(xzUQ(0)); deallocate(xzUQ);
        if(allocated(rxzUQ)) deallocate(rxzUQ);  allocate(rxzUQ(0)); deallocate(rxzUQ);
        if(allocated(sx)) deallocate(sx);  allocate(sx(0)); deallocate(sx);
        if(allocated(xzDL)) deallocate(xzDL);  allocate(xzDL(0)); deallocate(xzDL);

        if(allocated(covariter)) deallocate(covariter);  allocate(covariter(0)); deallocate(covariter);
        if(allocated(kv1)) deallocate(kv1);  allocate(kv1(0)); deallocate(kv1);
        if(allocated(kgl)) deallocate(kgl);  allocate(kgl(0)); deallocate(kgl);
        if(allocated(muvect0)) deallocate(muvect0);  allocate(muvect0(0)); deallocate(muvect0);

        if(allocated(ivref)) deallocate(ivref);  allocate(ivref(0)); deallocate(ivref);
        if(allocated(rnetvar)) deallocate(rnetvar);  allocate(rnetvar(0)); deallocate(rnetvar);
        if(allocated(d0zrateZ)) deallocate(d0zrateZ);  allocate(d0zrateZ(0)); deallocate(d0zrateZ);
        if(allocated(rblindnetZ)) deallocate(rblindnetZ);  allocate(rblindnetZ(0)); deallocate(rblindnetZ);

        if(allocated(kpoint)) deallocate(kpoint);  allocate(kpoint(0)); deallocate(kpoint);
        if(allocated(kpointKB)) deallocate(kpointKB);  allocate(kpointKB(0)); deallocate(kpointKB);
        if(allocated(Rnetpars)) deallocate(Rnetpars);  allocate(Rnetpars(0)); deallocate(Rnetpars);
        if(allocated(RnetparsInd)) deallocate(RnetparsInd);  allocate(RnetparsInd(0)); deallocate(RnetparsInd);
        if(allocated(Wpars)) deallocate(Wpars);  allocate(Wpars(0)); deallocate(Wpars);
        if(allocated(WparsInd)) deallocate(WparsInd);  allocate(WparsInd(0)); deallocate(WparsInd);
        if(allocated(RnetparsCRate)) deallocate(RnetparsCRate);  allocate(RnetparsCRate(0)); deallocate(RnetparsCRate);
        if(allocated(RnetparsCrule)) deallocate(RnetparsCRule);  allocate(RnetparsCRule(0)); deallocate(RnetparsCRule);

        if(allocated(symb_new)) deallocate(symb_new);  allocate(symb_new(0)); deallocate(symb_new);
        if(allocated(sig)) deallocate(sig);  allocate(sig(0)); deallocate(sig);
        if(allocated(x)) deallocate(x);  allocate(x(0)); deallocate(x);
        if(allocated(ux)) deallocate(ux);  allocate(ux(0)); deallocate(ux);
        if(allocated(y)) deallocate(y);  allocate(y(0)); deallocate(y);
        if(allocated(yfit)) deallocate(yfit);  allocate(yfit(0)); deallocate(yfit);

        if(allocated(afuncSV)) deallocate(afuncSV);  allocate(afuncSV(0,0)); deallocate(afuncSV);
        if(allocated(xA)) deallocate(xA)           ! ;  allocate(x1A(0)); deallocate(x1A);
        !if(allocated(x1A)) deallocate(x1A);  allocate(x1A(0)); deallocate(x1A);
        !if(allocated(x2A)) deallocate(x2A);  allocate(x2A(0)); deallocate(x2A);
        !if(allocated(x3A)) deallocate(x3A);  allocate(x3A(0)); deallocate(x3A);
        if(allocated(xpl)) deallocate(xpl);  allocate(xpl(0)); deallocate(xpl);
        if(allocated(ypl)) deallocate(ypl);  allocate(ypl(0)); deallocate(ypl);
        if(allocated(uypl)) deallocate(uypl);  allocate(uypl(0)); deallocate(uypl);
        if(allocated(yplfit)) deallocate(yplfit);  allocate(yplfit(0)); deallocate(yplfit);
        if(allocated(muvectR)) deallocate(muvectR);  allocate(muvectR(0)); deallocate(muvectR);

        !if(allocated(xpltBS)) deallocate(xpltBS);  allocate(xpltBS(0,0)); deallocate(xpltBS);
        !if(allocated(ypltBS)) deallocate(ypltBS);  allocate(ypltBS(0,0)); deallocate(ypltBS);

        if(allocated(xplt)) deallocate(xplt);  allocate(xplt(0,0)); deallocate(xplt);
        if(allocated(yplt)) deallocate(yplt);  allocate(yplt(0,0)); deallocate(yplt);

        if(allocated(kbrutto_name)) deallocate(kbrutto_name)
        if(allocated(knetto_name)) deallocate(knetto_name)
        if(allocated(symb_kEGr)) deallocate(symb_kEGr)
        if(allocated(DPmat)) deallocate(DPmat)               ! 6.7.2023
        if(allocated(kEQnums)) deallocate(kEQnums)           ! 6.7.2023

        if(allocated(DCList)) deallocate(DCList)             ! 30.12.2024  27.4.2025
        if(allocated(DCnuclide)) deallocate(DCnuclide,DCsymbEffiA, &
                                 DCsymbEffiB,DCsymbEffiC,DCsymbYield,DCindx)
        if(allocated(DCsymbT12)) deallocate(DCsymbT12)
        if(allocated(DCsymbLambda)) deallocate(DCsymbLambda)


    end subroutine StartAlloc

!########################################################################################

    subroutine ReadUnits

        ! this routine reads two files with definitions of basic units ( file unitsTable.csv)
        ! and other laboratory-dependent notations for units (file units_Other.csv)
        !
        ! See chapter 7.21.1 "Collection of basic units and derived units" of the UncertRadio
        ! CHM Help file for more details.
        !
        !     Copyright (C) 2021-2023  Günter Kanisch

        use UR_Gleich_globals,    only: URunits,charv,nbasis,UU,nu_other,unit_other,Unit_basis

        use Top,          only: DRead,GetCells,CharModA1,CharModA2
        use CHF,          only: ucase, flfu
        use file_io,      only: logger
        use ur_general_globals, only: work_path

        implicit none

        CHARACTER(:),allocatable  :: ttext ! text,textG
        type(charv),allocatable   :: Zelle(:)

        character(len=512)        :: log_str
        integer                   :: ios, nb, jd, js

        allocate(character(len=300) :: ttext)
        if(allocated(Zelle)) deallocate(Zelle)
        allocate(Zelle(10))

        ! type(charv),allocatable    :: EinhSymb(:)
        ! real(rn),allocatable       :: EinhVal(:)
        ! type(charv),allocatable    :: EinhSymbScd(:,:)
        ! real(rn),allocatable       :: EinhScdFact(:,:)
        ! type(charv),allocatable    :: EinhSymbSynon(:,:)


        if(allocated(UU%EinhSymb)) deallocate(UU%EinhSymb,UU%Einhval,UU%EinhSymbScd,UU%EinhScdFact,UU%EinhSymbSynon, &
            UU%nSymbCsd,UU%nSymbSyn)
        allocate(UU%EinhSymb(nbasis),UU%EinhVal(nbasis))
        allocate(UU%EinhSymbScd(nbasis,10),UU%EinhScdFact(nbasis,10),UU%EinhSymbSynon(nbasis,10))
        allocate(UU%nSymbCsd(nbasis),UU%nSymbSyn(nbasis))

        call CharmodA1(UU%EinhSymb,nbasis)
        call CharmodA2(UU%EinhSymbScd,nbasis,10)
        call CharmodA2(UU%EinhSymbSynon,nbasis,10)


        open(96, file=flfu(work_path // 'unitsTable.txt'),status='OLD')

        nb = 0
        jd = 0   ! 2025.01.23 GK
        js = 0   ! 2025.01.23 GK
        read(96,*)   ! skip headline
        do
            read(96,'(a)',iostat=ios) ttext
            if(ios /= 0) EXIT
            !  write(66,*) 'nb=',int(nb,2),' ttext=',trim(ttext)
            if(index(ucase(ttext),'BASE=') > 0) then
                nb = nb + 1
                js = 0
                jd = 0
                UU%nSymbSyn(nb) = 0
                UU%nSymbCsd(nb) = 0
                if(nb > ubound(UU%EinhSymb,dim=1)) then
                    call CharmodA1(UU%EinhSymb,nb)
                    call CharmodA2(UU%EinhSymbScd,nb,5)
                    call CharmodA2(UU%EinhSymbSynon,nb,5)
                end if
                UU%EinhSymb(nb)%s = trim(ttext(6:))
                cycle
            elseif(index(ucase(ttext),'BASE#=') > 0) then
                read(ttext(7:),*) UU%EinhVal(nb)
                write(log_str, '(*(g0))') 'RU: nb=',int(nb,2),' UU%EinhSymb(nb)%s=',UU%EinhSymb(nb)%s,' UU%EinhVal(nb)=',UU%EinhVal(nb)
                call logger(66, log_str)
                cycle
            elseif(index(ucase(ttext),'SYN=') > 0) then
                if(len_trim(ttext(5:)) > 0) then
                    js = js + 1
                    UU%EinhSymbSynon(nb,js)%s = trim(ttext(5:))
                    UU%nSymbSyn(nb) = UU%nSymbSyn(nb) + 1
                    ! write(66,*) UU%EinhSymbSynon(nb,js)%s
                end if
                cycle
            elseif(index(ucase(ttext),'DERV=') > 0) then
                if(len_trim(ttext(6:)) > 0) then
                    jd = jd + 1
                    UU%nSymbCsd(nb) = UU%nSymbCsd(nb) + 1
                    UU%EinhSymbScd(nb,jd)%s = trim(ttext(6:))
                    ! write(66,*) UU%EinhSymbScd(nb,jd)%s
                end if
                cycle
            elseif(index(ucase(ttext),'CONV=') > 0) then
                if(len_trim(ttext(6:)) > 0) then
                    ! UU%nSymbCsd(nb) = UU%nSymbCsd(nb) + 1
                    read(ttext(6:),*) UU%EinhScdFact(nb,jd)
                    ! write(66,*) UU%EinhScdFact(nb,jd)
                end if
                cycle
            end if
            write(log_str, '(*(g0))') 'nothing found: ttext=',trim(ttext)
            call logger(66, log_str)
        end do
        UU%nSymb = nb
        close (96)
        open(96,file=flfu(work_path // 'Units_Other.txt'),status='OLD')

        nu_other = 0
        do
            read(96,'(a)',iostat=ios) ttext
            if(ios /= 0) EXIT
            if(index(ucase(ttext),'UNIT=') > 0) then
                nu_other = nu_other + 1
                unit_other(nu_other) = trim(ttext(6:))
                cycle
            elseif(index(ucase(ttext),'UBASE=') > 0) then
                unit_basis(nu_other) = trim(ttext(7:))
                cycle
            end if
        end do
        ! write(66,*) 'unit_other(1)=',trim(unit_other(1)),'  unit_basis(1)=',trim(unit_basis(1))
        close (96)

    end subroutine ReadUnits

!########################################################################################


end module URinit
