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
module LDN

    use UR_types
    use UR_params, only: ZERO, ONE, TWO, EPS1MIN
    use Brandt,    only: pnorm, qnorm

    implicit none
    !    contains:
    ! Loadsel_diag_new
    ! SetLabel3Terms
    ! NetRatesCalc
    ! GetGamData
    ! ConvertGamD
    ! SaveToConfig
    ! GetxdataMD_fromTree

    !  Copyright (C) 2014-2023  Günter Kanisch

contains

!############################################################################

!GTK Response constants:
!
! gtk_RESPONSE_NONE            -1
!	gtk_RESPONSE_REJECT          -2
!	gtk_RESPONSE_ACCEPT          -3
!	gtk_RESPONSE_DELETE_EVENT    -4
!	gtk_RESPONSE_OK              -5
!	gtk_RESPONSE_CANCEL          -6
!	gtk_RESPONSE_CLOSE           -7
!	gtk_RESPONSE_YES             -8
!	gtk_RESPONSE_NO              -9
!	gtk_RESPONSE_APPLY           -10
!	gtk_RESPONSE_HELP            -11
!
!GTK Notebook Tab Constants:
!The Notebook Tab constants specify the tab position to receive focus.
!
! gtk_NOTEBOOK_TAB_FIRST   	The first gtk.Notebook tab
! gtk_NOTEBOOK_TAB_LAST     The last gtk.Notebook tab

! Values of ioption:
!         1 : options dilaog
!         2 : DecayFit model (Linf)
!         3 : decay curve data input
!         4 : not used
!         5 : Gamspk1 dialog
!         6 : dialog for number of outpt quantities
!         7 : dilaog for changing a symbol name
!         8 : calibration curve dialog
!      9-61 : not used
!        62 : FontSelection button
!        63 : Colour button
!        64 : filechooser dialog
!        65 : dialog for ellipsoid
!        66 : not used
!        67 : dialog for the exchange of two output quantity symbols
!        68 : not used
!        69 : dialog for a mean value of data
!        70 : dialog for serial evaluation
!        71 : dialog for binomial-Poisson distribution
!        72 : dialog for QC batch test
!        73 : dialog for batch evaluation
!        74 : dialog for distributions
!        75 : dialog for InfoFX
!


    subroutine Loadsel_diag_new(mode, ncitem)

        ! This is the basic routine which shows a dialog, reacts to possible user actions
        ! by processing them in this routine and reads the necessary information from the
        ! dioalog when this was requested (e.g. butto 'OK') to end the communication; finally
        ! the dialog is hided.  The necessary sequence of steps is shown by comments in the
        ! code.
        !  Copyright (C) 2014-2023  Günter Kanisch
        !

        use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_null_char, &
                                               c_null_ptr, c_associated, c_char, &
                                               c_double, c_f_pointer, c_loc, c_funptr

        use gtk,                only:   gtk_radio_button_get_group, &
                                        gtk_widget_hide,gtk_radio_button_get_group, &
                                        GTK_RESPONSE_YES,GTK_BUTTONS_OK,GTK_BUTTONS_YES_NO, &
                                        gtk_main_iteration,gtk_font_button_get_use_font, &
                                        gtk_font_button_get_font_name,gtk_widget_set_sensitive, &
                                        gtk_color_chooser_get_rgba, &
                                        gtk_widget_set_visible, gtk_widget_get_visible, &
                                        gtk_font_button_set_font_name, &
                                        gtk_color_chooser_set_rgba,GTK_MESSAGE_WARNING, &
                                        gtk_widget_show_all, &
                                        gtk_tree_view_column_get_width, &
                                        gtk_tree_view_columns_autosize, &
                                        gtk_file_chooser_get_filename,gtk_file_chooser_set_filename, &
                                        gtk_widget_grab_focus,gtk_window_set_title, &
                                        gtk_widget_set_child_visible,gtk_notebook_set_current_page, &
                                        gtk_tree_view_column_set_visible

        use gtk_sup,            only:   c_f_string

        use pango,              only:   pango_font_description_from_string, &
                                        pango_font_description_to_string, &
                                        pango_font_description_set_size, &
                                        pango_font_description_get_size

        use gtk_hl,             only:   hl_gtk_combo_box_get_active, &
                                        hl_gtk_listn_get_selections

        use g,                  only:   g_value_set_string,g_object_set_property

        use UR_gtk_globals,   only:   clobj, dialogstr,FieldDoActCB,FieldEditCB, &
                                        ncitemClicked,PageSwitchedCB,ButtonClicked,ioption,HelpButton,CloseDialogCB, &
                                        dialogloop_on,Settings,fontname,colorname,kcolortype,dialog_leave, &
                                        WinMC_resized,dialog_on,ntvs,tvcolindex, &
                                        tvnames,pixel_per_char,tvcols, pstring

        use UR_gtk_window,      only:   GdkRGBA

        use top,                only:   idpt,WrStatusbar,FieldUpdate,MDcalc,FindItemS, &
                                        PixelPerString,RealModA1,CharModa1,IntModA1,InitVarsTV5,InitVarsTV5_CP, &
                                        InitVarsTV6_CP,LogModA1,CharModStr
        use ur_general_globals, only:   actual_plot,Confidoid_activated, plot_confidoid,plot_ellipse, &
                                        sDecimalPoint,sListSeparator,mcsim_on,Savep,FileTyp,serial_csvinput, &
                                        work_path, results_path, &
                                        batest_ref_file_ch,batest_out_ch,base_project_SE,kfrom_SE,kto_SE, &
                                        kcmxMC,kcrunMC,bat_serial,batf, &
                                        bat_mc,batf_file,batf_reports,top_selrow,setDP, &
                                        open_project_parts, dir_sep
        USE UR_Gleich_globals,          only:   FormeltextFit,ifehl,k_datvar,knumEGr,knumold,loadingpro,missingval,symbole, &
                                        kpoint, Messwert,k_mdtyp,meanmd,umeanmd, &
                                        nvalsmd,xdataMD,kbgv_binom,itm_binom,ip_binom,use_bipoi,ilam_binom,  &
                                        vdoptfull,vdopt_pname,ivtl,DistPars,nmxDist,StdUnc,MDpointrev,MDpoint, &
                                        nvMD,smeanMD,rinflu_known,refdataMD,ixdanf,nvarsMD,charv,ncov,ngrsP, &
                                        sensi,perc,SymboleG,Symtyp,ngrs,MesswertSV,icovtyp,einheit,cvformel, &
                                        corrval,covarval,nvalsMD,coverf,coverin,nparts,MDused,meanID,fBayMD, &
                                        StdUncSV

        USE UR_Linft,           only:   dmesszeit_CP,dbimpulse_CP,dbzrate_CP,sdbzrate_CP,d0messzeit_CP, &
                                        d0impulse_CP,d0zrate_CP,sd0zrate_CP,dnetrate_CP,sdnetrate_CP, &
                                        CStartzeit_CP,d0zrateSV,sd0zrateSV,dtdiff,ifit,CCtitle,CFaelldatum, &
                                        CStartzeit,d0impulse,d0messzeit,d0zrate,dbimpulse,dbzrate,defineallxt, &
                                        dmodif,dnetrate,elirs,fitmeth,k_rbl,kal_Polgrad,kfitmeth,klincall, &
                                        kPMLE,linfzbase,ma,makb,mfitfix,nchannels,ndatmax,ndefall,nkalpts, &
                                        nkovzr,numd,sdbzrate,sdnetrate,use_UfitKal,use_WTLS,uxkalib,uykalib, &
                                        nwei,kpearson,fpa,sfpa,dmesszeit,sd0zrate,igsel,xkalib,ykalib,mfrbg, &
                                        singlenuk,use_absTimeStart,use_WTLS_kal,mac,FitDecay

        USE UR_DLIM,            only:   alpha,beta,GamDistAdd,kalpha,kbeta,NWGMethode,W1minusG
        USE UR_Loadsel
        USE UR_Gspk1Fit
        use Rout,               only:   MessageShow, &
                                        WDPutEntryDouble, &
                                        WDPutEntryString, &
                                        WDSetComboboxAct, &
                                        WDGetComboboxAct, &
                                        WDPutSelRadio, &
                                        WDGetTextviewString, &
                                        WDGetCheckButton, &
                                        WDGetEntryString, &
                                        WTreeViewGetStrArray, &
                                        WDGetEntryInt, &
                                        WTreeViewGetDoubleArray, &
                                        WTreeViewPutDoubleCell, &
                                        WDGetSelRadio, &
                                        WDGetEntryDouble, &
                                        WDPutEntryString, &
                                        WTreeViewGetCheckArray, &
                                        WDPutLabelString, &
                                        pending_events, &
                                        WDListstoreFill_1, &
                                        SetMenuEGr, &
                                        WDNotebookSetCurrPage, &
                                        WTreeViewPutDoubleArray, &
                                        WDPutEntryInt, &
                                        WDSetCheckButton, &
                                        fopen, &
                                        ClearMCfields, &
                                        WDPutLabelStringBold, &
                                        WTreeViewSetColorRow, &
                                        WDPutTreeViewColumnLabel, &
                                        WDSetComboboxAct

        use Brandt,             only: mean, sd
        use urInit,             only: GtkSettingsIO,TVtrimCol_width
        use UWB,                only: Exchange2Symbols,ChangeSname
        use KLF,                only: XKalfit
        use UR_interfaces,      only: DisplayHelp
        use CHF,                only: FindLocT, ucase, flfu
        use Celli,              only: Confidoid
        use PLsubs,             only: PrintPlot,PlotEli
        use UR_Mcc,             only: iopt_copygr,use_BCI,kcrun

        USE fparser,            only: evalf
        use plplot,             only: pladv, plclear, plinit

        use plplot_code_sub1,   only: scalable

        use file_io,            only: logger, read_config
        use gui_functions,      only: SetColors
        use UR_params,          only: BATEST_OUT, BATEST_REF_FILE, UR2_CFG_FILE
        use color_theme
        use translation_module, only: T => get_translation, set_language, get_language
        use UR_DecChain
        use DECH,               only: DCPrepareTable,DCReadGrid,DecCh


        implicit none

        integer, intent(in)        :: mode             ! 1: show dialog;  2: readout dialog and hide it
        integer, intent(in)        :: ncitem

        character(len=60)          :: idstring
        character(len=60)          :: widgetlabel
        character(len=60)          :: objstr
        character(len=60)          :: signal
        character(len=100)         :: hinweis
        type(charv),allocatable    :: FTF(:)
        character(len=10)          :: chcol
        character(len=15)          :: buthelp
        character(len=:), allocatable :: langg, langgSV

        type(c_ptr)                :: dialog, pfontname, pfd2_ptr, pfd_ptr
        integer(c_int)             :: indx, answer,res
        integer(c_int)             :: resp_id
        integer                    :: resp,ncitem2,k1lang,ifitXX(3),nn,kcmx,nvals,ifx
        integer                    :: iv,nc,ksep,kim,nnch
        type(c_ptr)                :: ctext, pcolor, cp1, cp2
        integer(c_int),allocatable :: indices(:)

        character(len=30)          :: zstr, treename
        ! type(GdkRGBA), pointer     :: pURcolor
        type(GdkRGBA), target      :: URcolor

        integer(c_int)             :: k1_exchg,k2_exchg,i1,i2,i3,nv,kk
        integer(c_int)             :: ios,i,kxy,k,kWTLS,j,klss,nt,n,kwd,kht,ivt,ks,nvisib,nj
        integer(c_int)             :: ikenn,kcolls(4),i11,i11max,rowtv2,ifitSV(3),knt,mfit2
        integer(c_int)             :: numd_old,ijh,nijh,fijh(4),kfm1,nwei1,nkovzr1,ndefall1,nch1,kbaseX

        real(rn)                   :: zfact, xxs1,xx,dummy,vvar,exx,aa,bb
        real(rn), save             :: kalpha1,kbeta1,coverf1,W1minusG1,alpha1,beta1,GDA1,entival(4),coverin1
        LOGICAL                    :: lpass,prout,SaveP_sv,dnew,test1,test2,selvar
        character(len=150)         :: text
        character(len=25)          :: psym(4), enti(4), cfdatx
        real(rn),allocatable       :: rdummy(:),xdat(:)
        character(len=512)         :: log_str
        character(:),allocatable   :: cdummy, str1
        !--------------------------------------------------------------------------
        nt = 0
        ivt = 0
        kcmx = 0
        mfit2 = 0
        fijh(:) = 0
        nijh = 0
        nwei1 = 0
        dnew = .false.

        if(mode == 2 .and. ncitem == 0) return
        prout = .false.

        ! if(ioption == 71) prout = .true.
        !         prout = .true.

        !write(66,*) 'gtk_RESPONSE_NONE=',gtk_RESPONSE_NONE
        !write(66,*) 'gtk_RESPONSE_REJECT=',gtk_RESPONSE_REJECT
        !write(66,*) 'gtk_RESPONSE_ACCEPT=',gtk_RESPONSE_ACCEPT
        !write(66,*) 'gtk_RESPONSE_DELETE_EVENT=',gtk_RESPONSE_DELETE_EVENT
        !write(66,*) 'gtk_RESPONSE_CANCEL=',gtk_RESPONSE_CANCEL
        !write(66,*) 'gtk_RESPONSE_CLOSE=',gtk_RESPONSE_CLOSE
        !write(66,*) 'gtk_RESPONSE_YES=',gtk_RESPONSE_YES
        !write(66,*) 'gtk_RESPONSE_NO=',gtk_RESPONSE_NO
        !write(66,*) 'gtk_RESPONSE_APPLY=',gtk_RESPONSE_APPLY

        !!! allocate(character(len=500) :: str1)

        dialog = c_null_ptr      ! 2025.01.23 GK

        if(ncitem > 0) then
            idstring    = clobj%idd(ncitem)%s
            widgetlabel = clobj%label(ncitem)%s
            objstr      = clobj%name(ncitem)%s
            signal      = clobj%signal(ncitem)%s
            ! write(66,*) 'Loadsel:   iop=',ioption,'  dialogstr=',trim(dialogstr),'  idstring=',trim(idstring), &
            !              '  widgetlabel=',trim(widgetlabel),'  objstr=',trim(objstr)
        end if
        if(prout .and. len_trim(dialogstr) > 0)  then
            write(log_str, '(*(g0))') '         dialog=',trim(dialogstr)
            call logger(66, log_str)
        end if

        if(len_trim(dialogstr) > 0) then
            dialog = idpt(trim(dialogstr))!
            if(.not.c_associated(dialog)) then
                ifehl = 1
                goto 9000
            end if
        end if
        SaveP_sv = SaveP
        dialog_on = .true.
        call gtk_widget_set_sensitive(idpt('menubar1'), 0_c_int)

        if(mode == 2) then
            goto 1000
        end if

        gmodif = .false.
        dmodif = .false.

        if(c_associated(dialog)) then
            call PixelPerString(dialog, '123456789E-02123456789E-02',kwd,kht)
            pixel_per_char = int(real(kwd)/real(len_trim('123456789E-02123456789E-02'))) + 1
        end if

        !------------------------------------------------------------------------

        ! Before showing the dialog and starting the dialog loop: set data to be shown
        ! in the dialog;
        ! ioption defines which dialog will be shown

        select case (ioption)
          case (1)        ! options dialog

            if(prout)  then
                write(log_str, '(a,4f8.5)') 'Option-Dialog, on entry: kalpha, kbeta, alpha, beta= ',kalpha, kbeta, alpha, beta
                call logger(66, log_str)
            end if
            call WDPutEntryDouble('entryOptKalpha',kalpha,'(f10.8)')
            call WDPutEntryDouble('entryOptKbeta',kbeta,'(f10.8)')
            IF(kalpha > ZERO) THEN
                alpha =  ONE - pnorm(kalpha)            ! , zero, one)
                call WDPutEntryDouble('entryOptAlpha',alpha,'(f10.8)')
            end if
            IF(kbeta > ZERO) THEN
                beta =  ONE - pnorm(kbeta)          ! , zero, one)
                call WDPutEntryDouble('entryOptBeta',beta,'(f10.8)')
            end if
            if(prout) then

                write(log_str, '(a,4f8.5)') 'Option dialog, after Put: kalpha, kbeta, alpha, beta= ',kalpha, kbeta, alpha, beta
                call logger(66, log_str)
            end if
            call WDPutEntryDouble('entryOptCoverf',coverf,'(f5.2)')
            call WDPutEntryDouble('entryOptCoverIn',coverin,'(f5.2)')
            call WDPutEntryDouble('entryOpt1minusG',W1minusG,'(f5.3)')
            call WDPutEntryString('entryOptDLMethod',trim(NWGMethode))
            kalpha1 = kalpha

            kbeta1 = kbeta
            alpha1 = alpha
            beta1 = beta
            coverf1   = coverf
            coverin1 = coverin
            W1minusG1 = W1minusG
            GDA1 = GamDistAdd
            call WDPutEntryDouble('entryOptGamDistAdd',GamDistAdd,'(f3.1)')
            if(sListSeparator == ';') call WDSetComboboxAct('comboboxtextListSep',1)
            if(sListSeparator == ',') call WDSetComboboxAct('comboboxtextListSep',2)


            if(get_language() == 'de') then
                i = 1
            else if(get_language() == 'fr') then
                i = 3
            else
                i = 2
            end if
            call WDSetComboboxAct('comboboxLangg', i)

            if (get_theme_name() == "contrast") then
                call WDSetCheckButton('check_contrastmode', 1)
            else
                call WDSetCheckButton('check_contrastmode', 0)
            end if

            call gtk_widget_set_sensitive(idpt('DOptionsLoadVals'), 0_c_int)   ! 13.4.2023
            call gtk_widget_set_sensitive(idpt('DOptionsOK'), 1_c_int)   ! 13.4.2023

          case (2)
            ifitXX = ifit
            if(loadingPro) SaveP = .false.
            ! model FitDecay:
            IF(kfitmeth == 0) call WDPutSelRadio('radiobuttonNLSQ', 1)
            IF(kfitmeth == 1) call WDPutSelRadio('radiobuttonNLSQ', 2)
            IF(kfitmeth == 2) call WDPutSelRadio('radiobuttonNLSQ', 3)
            IF(kfitmeth == 3) call WDPutSelRadio('radiobuttonNLSQ', 4)
            dnew = .false.
            call WDGetTextviewString('textviewModelEQ',FTF)
            if(size(FTF) == 0) dnew = .true.
            call SetLabel3Terms

            kfm1 = kfitmeth
            nwei1 = nwei
            nkovzr1 = nkovzr
            ndefall1 = ndefall
            nch1 = nchannels
            SaveP_sv = saveP

          case (3)
            ! decay curve data input:
            if(.not.loadingPro) then
                dmodif = .false.
                SaveP = .false.

                do i=1, 100
                    call WTreeViewSetColorRow('treeview5', i, get_color_string('table_bg'))
                end do

            end if
            if(numd == 0) then                                                             !!  2.9.2024
                IF(loadingPro) THEN                                                          !!
                loadingPro = .false.                                                       !!
                endif                                                                        !!
                dmodif = .false.                                                             !!
                SaveP = .false.                                                              !!
                do i=1,100                                                                   !!
                    call WTreeViewSetColorRow('treeview5', i, get_color_string('table_bg'))                        !!
                end do                                                                       !!
                if(.not.loadingPro) call gtk_tree_view_columns_autosize(idpt('treeview5'))   !!
                goto 1000                                                                    !!
            endif                                                                          !!

            !if(defineallxt) then        ! 4.5.2024
            !  call gtk_tree_view_column_set_visible(idpt('treeviewcolumn29'),0_c_int)
            !else
            !  call gtk_tree_view_column_set_visible(idpt('treeviewcolumn29'),1_c_int)
            !endif
            if(k_rbl > 0) then
                if(kpoint(k_rbl) > 0) then
                    call WDPutEntryDouble('entryNetBlindVal', Messwert(kpoint(k_rbl)), '(es12.6)')  ! 24.7.2023
                else
                    call WDPutEntryDouble('entryNetBlindVal', 0._rn, '(f3.1)')
                end if
            else
                call WDPutEntryDouble('entryNetBlindVal', 0._rn, '(f3.1)')
            end if
            call gtk_widget_set_sensitive(idpt('entryNetBlindVal'), 0_c_int)
            call WDGetCheckButton('checkAbsTime',i) ! 16.9.2023
            use_absTimeStart = .false.              !
            if(i == 1) use_absTimeStart = .true.    !
            write(log_str, '(*(g0))') 'use_absTimeStart=',use_absTimeStart
            call logger(66, log_str)
            !if(len_trim(CFaelldatum) > 10) cfdatX = CFaelldatum
            !CFaelldatum = ' '
            if(use_absTimeStart) then
                ! 24.7.2023:
                call WDSetCheckButton('checkAbsTime',1)
                call gtk_widget_set_sensitive(idpt('entrySeparation'),1_c_int)
                call gtk_widget_set_visible(idpt('entrySeparation'),1_c_int)
                call WDGetEntryString('entrySeparation', CFaelldatum)
                call gtk_widget_set_sensitive(idpt('comboboxtextbase'),1_c_int)
                call WDPutTreeViewColumnLabel('treeview5', 2, T('Start date')//char(13)//T('(gross)'))
            else
                call WDSetCheckButton('checkAbsTime',0)

                call WDSetComboboxAct('comboboxtextbase', linfzbase)       ! 16.9.2023

                call gtk_widget_set_sensitive(idpt('entrySeparation'),0_c_int)
                call gtk_widget_set_visible(idpt('entrySeparation'),0_c_int)

                call WDPutTreeViewColumnLabel('treeview5', 2, T('StartDiff (s)')//char(13)//T('(gross)'))

            end if
            cfdatX = CFaelldatum
            call WDGetComboboxAct('comboboxtextbase',kbaseX)
            ! write(66,*) 'gtk_widget_get_visible(idpt(''entrySeparation''))=',gtk_widget_get_visible(idpt('entrySeparation'))
            if(.not.loadingPro) call pending_events()
            kxy = ndatmax
            if(allocated(dmesszeit_CP)) then
                deallocate(dmesszeit_CP,dbimpulse_CP,dbzrate_CP,sdbzrate_CP,d0messzeit_CP, &
                    d0impulse_CP,d0zrate_CP,sd0zrate_CP,dnetrate_CP,sdnetrate_CP, &
                    CStartzeit_CP)
            end if
            call InitVarsTV5_CP(kxy)

            if(allocated(sd0zrateSV)) deallocate(sd0zrateSV)                     !
            if(allocated(d0zrateSV)) deallocate(d0zrateSV)                       !
            allocate(sd0zrateSV(kxy),d0zrateSV(kxy))
            sd0zrateSV= ZERO; d0zrateSV = ZERO
            if(allocated(dtdiff)) deallocate(dtdiff)
            allocate(dtdiff(kxy))
            dtdiff = ZERO

            call WTreeViewGetStrArray('treeview5', 2, kxy, CStartzeit_CP)
            call WTreeViewGetDoubleArray('treeview5', 3, kxy, dmesszeit_CP)
            call WTreeViewGetDoubleArray('treeview5', 4, kxy, dbimpulse_CP)
            call WTreeViewGetDoubleArray('treeview5', 5, kxy, dbzrate_CP)
            call WTreeViewGetDoubleArray('treeview5', 6, kxy, sdbzrate_CP)
            call WTreeViewGetDoubleArray('treeview5', 7, kxy, d0messzeit_CP)
            call WTreeViewGetDoubleArray('treeview5', 8, kxy, d0impulse_CP)
            call WTreeViewGetDoubleArray('treeview5', 9, kxy, d0zrate_CP)
            call WTreeViewGetDoubleArray('treeview5', 10, kxy, sd0zrate_CP)
            call WTreeViewGetDoubleArray('treeview5', 11, kxy, dnetrate_CP)
            call WTreeViewGetDoubleArray('treeview5', 12, kxy, sdnetrate_CP)

            read(Cstartzeit_CP(1)%s,*,iostat=ios) dummy
            if(ios == 0) then
                ! 16.9.2023:
                if(use_absTimeStart) then
                    use_absTimeStart = .false.
                    call WDSetCheckButton('checkAbsTime',0)
                    call WDSetComboboxAct('comboboxtextbase',1)
                    linfzbase = 1
                endif
            else
                if(.not.use_absTimeStart) then
                    use_absTimeStart = .true.
                    call WDSetCheckButton('checkAbsTime',1)
                    call WDSetComboboxAct('comboboxtextbase',linfzbase)
                endif
            end if
            numrowsold = 0
            do i=1,kxy
                ! if(defineallxt) write(CSTartzeit_CP(i)%s,'(i0)') i      ! 4.5.2024
                IF(i > 1 .AND. (abs(dmesszeit_CP(i)-missingval) < EPS1MIN .or. &
                    abs(dmesszeit_CP(i))< EPS1MIN) )  THEN
                    numrowsold = i - 1
                    EXIT
                end if
            end do

            if(.not.loadingPro) call gtk_tree_view_columns_autosize(idpt('treeview5'))
            IF(loadingPro) THEN
                call WDGetComboboxAct('comboboxtextbase',linfzbase)
                if(linfzbase == 0) then
                    call gtk_widget_hide(dialog)
                    call CharModStr(str1,500)
                    WRITE(str1,*) trim(T("Decay curve table: The time-base unit is yet undefined!"))

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "LDN_373:", resp,mtype=GTK_MESSAGE_WARNING)
                    ifehl = 1
                    goto 9000
                end if
                zfact = ONE
                IF(linfzbase == 2) zfact = 60.0_rn
                call NetRatesCalc(ikenn)
                call gtk_widget_hide(dialog)
                GOTO 9000
            end if

          case (5)
            if(.not.loadingPro) then
                gmodif = .false.
                SaveP = .false.
                    do i=1,40
                        call WTreeViewSetColorRow('treeview6', i, get_color_string('table_bg'))
                    end do
            end if

            call WDGetSelRadio('radiobuttonG1', unitRadio(1))
            call WDGetSelRadio('radiobuttonG5', unitRadio(2))
            call WDGetSelRadio('radiobuttonG9', unitRadio(3))
            call WDGetSelRadio('radiobuttonG11', unitRadio(4))
            call WDGetSelRadio('radiobuttonG13', unitRadio(5))

            call WDGetComboboxAct('comboboxGMWtyp', kxy)
            IF(kxy == 1) mwtyp = 'WeiMean'
            IF(kxy == 2) mwtyp = 'LSQMean'
            call WDGetEntryDouble('entry_b2LFactor', FBT)
            call WDGetCheckButton('checkbuttonGspk1EffiCov', ecorruse)
            call WDGetCheckButton('checkbuttonMeanOpt', WMextSD)

            kxy = kdatmax
            if(allocated(erg_CP)) then
                deallocate(guse_CP,erg_CP,GnetRate_CP,RateCB_CP,RateBG_CP)
                deallocate(SDRateBG_CP,effi_CP,SDeffi_CP,pgamm_CP,SDpgamm_CP)
                deallocate(fatt_CP,SDfatt_CP,fcoinsu_CP,SDfcoinsu_CP)
            end if
            call InitVarsTV6_CP(kxy)

            call WTreeViewGetCheckArray('treeview6',2,kxy,guse_CP)
            call WTreeViewGetDoubleArray('treeview6',3,kxy,erg_CP)
            call WTreeViewGetDoubleArray('treeview6',4,kxy,GNetRate_CP)
            call WTreeViewGetDoubleArray('treeview6',5,kxy,RateCB_CP)
            call WTreeViewGetDoubleArray('treeview6',6,kxy,RateBG_CP)
            call WTreeViewGetDoubleArray('treeview6',7,kxy,SDRateBG_CP)
            call WTreeViewGetDoubleArray('treeview6',8,kxy,effi_CP)
            call WTreeViewGetDoubleArray('treeview6',9,kxy,SDeffi_CP)
            call WTreeViewGetDoubleArray('treeview6',10,kxy,pgamm_CP)
            call WTreeViewGetDoubleArray('treeview6',11,kxy,SDpgamm_CP)
            call WTreeViewGetDoubleArray('treeview6',12,kxy,fatt_CP)
            call WTreeViewGetDoubleArray('treeview6',13,kxy,SDfatt_CP)
            call WTreeViewGetDoubleArray('treeview6',14,kxy,fcoinsu_CP)
            call WTreeViewGetDoubleArray('treeview6',15,kxy,SDfcoinsu_CP)
            ! write(66,'(2(a,i5))') 'ubound(effi_CP)=',ubound(effi_CP,dim=1),'  size(effi_CP)=',size(effi_CP)

            test1 = .false.   ! 2025.01.23 GK
            test2 = .false.   !
            i11max = min(kxy+1,ubound(effi,dim=1))
            do i11=1,i11max
                test1 = abs(effi_CP(i11)-missingval) < EPS1MIN .or. abs(effi_CP(i11)-0.0_rn) < EPS1MIN
                test2 = abs(fatt_CP(i11)-missingval) < EPS1MIN .or. abs(fatt_CP(i11)-0.0_rn) < EPS1MIN
                if(test1 .and. test2) then
                    numd = i11-1
                    EXIT
                end if
            end do
            if(i11 == i11max+1 .and. .not.(test1 .and. test2)) i11 = i11max
            numd = i11*5    ! number of gamma lines times number of quantities per line
            numd_old = numd
            IF(loadingPro) Then
                call GetGamData()
                call gtk_widget_hide(dialog)
                GOTO 9000
            end if
            if(.not.loadingPro) call gtk_tree_view_columns_autosize(idpt('treeview6'))

          case (6)
            if(knumEGr == 0) call WDSetComboboxAct('comboboxtextKnumegr',1)
            if(allocated(Symbole)) then
                if(knumEGr > 0 .and. ubound(Symbole,dim=1) >= knumEGr) call SetMenuEGr(knumEGr)
            end if

          case (7)
            call WDPutEntryString('entrySymbchg',' ')

          case (8)
            call WDPutEntryString('entryDKTitel', trim(CCTitle))
            call WDSetComboboxAct('comboboxDKPgrad',kal_polgrad+1)
            call CharModStr(str1, 500)

            call WDPutLabelString('DKlabelPGrad', T("Degree of polynomial (0-3; 0: Mean of y): "))
            if(.not.loadingPro) then
                call gtk_tree_view_columns_autosize(idpt('treeview7'))
                do i=1,100
                    call WTreeViewSetColorRow('treeview7', i, get_color_string('table_bg'))
                end do
            end if
            !  write(0,*) 'sum(abs(uxkalib))=',sngl(sum(abs(uxkalib)))
            if(sum(abs(uxkalib)) <= EPS1MIN .or. (abs(uxkalib(1)-missingval) < EPS1MIN  &
                .and. abs(uxkalib(2)-missingval) < EPS1MIN)) then
                call gtk_widget_set_sensitive(idpt('DKcheckWTLS'),0_c_int)
                use_WTLS_kal = .false.
            else
                call gtk_widget_set_sensitive(idpt('DKcheckWTLS'),1_c_int)
            end if

          case (62)
            ! FontSelection button:
            call gtk_widget_set_sensitive(idpt('buttonFBSave'), 0_c_int)
            do i=1,Settings%nprops
                if(trim(Settings%sproperty(i)) == 'gtk-font-name') then
                    resp_id = gtk_font_button_set_font_name(idpt('fontbutton1'), &
                        trim(Settings%sproperty_val(i))//c_null_char)
                    exit
                end if
            end do

          case (63)
            kcolortype = 1
            call gtk_widget_set_sensitive(idpt('buttonCBSave'), 0_c_int)
            do i=1,Settings%nprops
                if(trim(Settings%sproperty(i)) == 'gtk-color-scheme') then
                    i1 = index(Settings%sproperty_val(i),'bg_color:')
                    if(i1 > 0) then
                        colorname = Settings%sproperty_val(i)(i1+9:i1+9+7)
                        read(colorname,'(1x,z2.2,z2.2,z2.2)') i1,i2,i3
                        URcolor%red = dble(i1)/dble(256)
                        URcolor%green = dble(i2)/dble(256)
                        URcolor%blue = dble(i3)/dble(256)
                        URcolor%alpha = 1.d0

                        call gtk_color_chooser_set_rgba(idpt('colorbutton1'), c_loc(URcolor))
                    end if
                    exit
                end if
            end do

          case (64)       ! filechooser dialog

          case (65)      ! windowELI

          case (67)       ! Exchange 2 output quantity symbols
            call WDPutLabelString('LBSymbExchg', T("Select two output quantity symbols to be exchanged:"))

            call WDListstoreFill_1('liststore_3EGs', knumEGr, Symbole)
            if(knumEGr == 2) then
                call WDSetComboboxAct('comboboxSymbExchgA',1)
                call WDSetComboboxAct('comboboxSymbExchgB',2)
            end if

          case (69)       ! dialogMeanData
            res = hl_gtk_listn_get_selections(idpt('treeview2'),indices)
            selvar = .false.
            kk = 0
            if(allocated(indices)) kk = indices(1) + 1_c_int
            if(kk > 0) then
                if(kk > ngrs) return    ! 10.8.2023
                if(ucase(symtyp(kk)%s) == 'M') selvar = .true.
            end if
            ! if(res == 0 .or. .not.selvar) then
            if(.not.selvar) then
                str1 = T("First select the corresponding row with type=m in the ValUnc table!")
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "Symbol1:", resp,mtype=GTK_MESSAGE_WARNING)
                ifehl = 1
                goto 9000
            end if
            rowtv2 = indices(1) + 1_c_int
            kk = MDpointrev(rowtv2)
            k_datvar = kk
            if(k_datvar == 0) then
                k_datvar = 1
                k_MDtyp(k_datvar) = 1
            end if
            if(k_datvar > ubound(k_MDtyp,dim=1)) then
                nvarsMD = k_datvar
                call CharModA1(meanID,nvarsMD)
                call IntModA1(MDpoint,nvarsMD)
                call IntModA1(nvalsMD,nvarsMD)
                call IntModA1(k_MDtyp,nvarsMD)
                call LogModA1(MDused,nvarsMD)
                call RealModA1(fBayMD,nvarsMD)
                call RealModA1(meanMD,nvarsMD)
                call RealModA1(smeanMD,nvarsMD)
                call RealModA1(umeanMD,nvarsMD)
                call RealModA1(nvMD,nvarsMD)
                call IntModA1(ixdanf,nvarsMD)
                if(nvarsMD == 1) then
                    ixdanf(1) = 1
                else
                    ixdanf(nvarsMD) = sum(nvalsMD(1:nvarsMD-1)) + 1
                end if
                nvalsMD(k_datvar) = 0
            end if
            if(nvalsMD(k_datvar) == 0) k_MDtyp(k_datvar) = 1
            call WDSetComboboxAct('combobox_MDselVar', k_datvar)
            call WDSetComboboxAct('combobox_MDtyp', k_MDtyp(k_datvar))
            nv = nvalsMD(k_datvar)
            if(.not.loadingPro) then
                do i=1,200
                    call WTreeViewSetColorRow('treeview8', i, get_color_string('table_bg'))
                end do
            end if

            if(nv > 0) then
                nj = ixdanf(k_datvar)
                if(allocated(xdat)) then; deallocate(xdat);allocate(xdat(nv)); end if
                xdat = xdataMD(nj:nj+nv-1)
                call WTreeViewPutDoubleArray('treeview8',2,nv,xdat)
                call MDcalc(k_datvar)
                if(ifehl == 1) goto 9000

                call WDPutEntryDouble('TEmeanMD',meanMD(k_datvar),'(es12.6)')
                call WDPutEntryDouble('TEsdMD',umeanMD(k_datvar),'(es12.6)')
                if(k_MDtyp(k_datvar) == 2) then
                    call WDPutLabelString('LBsdMD','sn')
                    call WDPutLabelString('LBnvar0MD','s0n')
                else
                    call WDPutLabelString('LBsdMD','sx')
                    call WDPutLabelString('LBnvar0MD','s0x')
                end if
                call WDPutEntryDouble('TEnvar0MD',smeanMD(k_datvar)**ONE,'(es12.6)')
                call WDPutEntryInt('TEmMD',nvalsMD(k_datvar),'(i0)')
                deallocate(xdat)
            end if


          case (70)       ! dialogSerEval
            write(log_str, '(2(a,L1))') '70 : bat_serial=',bat_serial,' batf=',batf
            call logger(66, log_str)
            if(bat_serial) then
                call gtk_window_set_title(idpt('dialogSerEval'), T("Serial evaluation:") // c_null_char)

                call gtk_widget_set_sensitive(idpt('ChooserButton1SE'),1_c_int)
                call gtk_widget_show_all(idpt('ChooserButton1SE'))
                call gtk_widget_set_child_visible(idpt('ChooserButton1SE'),1_c_int)
                call gtk_widget_set_visible(idpt('LBFile1SE'),1_c_int)
                call gtk_widget_set_child_visible(idpt('LBFile1SE'),1_c_int)
                call pending_events()
            end if
            call gtk_widget_hide(idpt('LBSEinput'))
            call gtk_widget_set_visible(idpt('LBSEinput'),0_c_int)
            call gtk_widget_set_visible(idpt('ChooserButton1SE'),1_c_int)
            call gtk_widget_set_visible(idpt('LBFile1SE'),1_c_int)
            call WDPutLabelString('LBFile2SE', T("File with variable data (csv)"))
            call WDPutLabelString('LBnrecsSE', T("Which records to calculate:"))

            call WDSetCheckButton('CheckMCSE',0)
            call gtk_widget_set_sensitive(idpt('EntryRunsMCSE'),0_c_int)
            call gtk_widget_set_sensitive(idpt('EntryMCnumSE'),0_c_int)

          case (71)       ! dialog_symchg  for input of kbgv_binom
            call WDSetComboboxAct('comboboxBinPoi1', ip_binom)
            call WDSetComboboxAct('comboboxBinPoi2', kbgv_binom)
            call WDSetComboboxAct('comboboxBinPoi3', itm_binom)
            call WDSetComboboxAct('comboboxBinPoi4', ilam_binom)
            use_bipoi = .true.

          case (72)          ! BatestUser
            resp = gtk_file_chooser_set_filename(idpt('BTchooserButton_1'), &
                trim(work_path) // trim(Batest_ref_file)//c_null_char)
            resp = gtk_file_chooser_set_filename(idpt('BTchooserButton_2'), &
                trim(results_path) // trim(Batest_out)//c_null_char)

          case (73)       ! dialogBatEval
            write(log_str, '(2(a,L1))') '73 : bat_serial=',bat_serial,' batf=',batf
            call logger(66, log_str)
            call gtk_window_set_title(idpt('dialogBatEval'), T("Batch projects:") // c_null_char)
            call pending_events()
            call WDPutLabelString('LBFile2BEV', T("File with project file names (txt)"))
            call WDPutLabelString('LBnrecsBEV', T("Which projects to evaluate:"))


            call WDSetCheckButton('CheckMCBEV',0)
            call gtk_widget_set_sensitive(idpt('EntryRunsMCBEV'),0_c_int)
            call gtk_widget_set_sensitive(idpt('EntryMCnumBEV'),0_c_int)

          case (74)         ! dialog_distributions
            nvisib = 0
            ks = top_selrow
            ivt = IVTL(top_selrow)
            nn = max(0, nmxDist)
            if(nn > 0) then
                nn = findlocT(DistPars%symb,Symbole(top_selrow)%s)
            end if
            write(log_str, '(*(g0))') 'LDN_764: DistParsRead: nn=',int(nn,2),'  ivt=',int(ivt,2)
            call logger(66, log_str)
            if(nn == 0) then
                if(allocated(Distpars%symb)) deallocate(Distpars%symb)
                allocate(DistPars%symb(1)); DistPars%symb(1)%s = ' '
                nmxDist = nmxDist + 1
                nn = nmxDist
            end if
            if(nn > ubound(Distpars%symb,dim=1)) call CharModA1(Distpars%symb,nn)
            DistPars%ivtl(nn) = ivt
            DistPars%symb(nn)%s = Symbole(ks)%s

            call WDPutLabelStringBold('DistribLB1', Symbole(ks)%s, get_color_string('label_fg'))
            call WDPutLabelStringBold('DistribLB2', vdoptfull(ivt)%s, get_color_string('label_fg'))
            if(ivt == 9) then
                call WDPutLabelString('DistribLBKt', T("(standard: mu=0, sigma=1)"))
            else
                call WDPutLabelString('DistribLBKt', ' ')
            end if
            psym(1) = 'DistribSym1'
            psym(2) = 'DistribSym2'
            psym(3) = 'DistribSym3'
            psym(4) = 'DistribSym4'
            enti(1) = 'DistribEntry1'
            enti(2) = 'DistribEntry2'
            enti(3) = 'DistribEntry3'
            enti(4) = 'DistribEntry4'
            do i=1,4
                if(len_trim(vdopt_pname(ivt,i)%s) > 0) then
                    call WDPutLabelString(trim(psym(i)), vdopt_pname(ivt,i)%s)
                    call gtk_widget_set_visible(idpt(psym(i)),1_c_int)
                    call gtk_widget_set_visible(idpt(enti(i)),1_c_int)
                    call gtk_widget_set_sensitive(idpt(enti(i)),1_c_int)
                else
                    do k=i,4
                        if(k == i) nvisib = i-1
                        call gtk_widget_set_visible(idpt(psym(k)),0_c_int)
                        call gtk_widget_set_visible(idpt(enti(k)),0_c_int)
                    end do
                end if
            end do
            if(ivt == 10) then
                vvar = StdUnc(ks)**TWO
                exx = Messwert(ks)
                aa = exx**TWO * ((ONE-exx)/vvar - ONE/exx)
                bb = aa * (ONE/exx - ONE)
                DistPars%pval(nn,1:2) = [ aa,bb ]

                call WDPutEntryDouble(enti(1),aa,'(es14.7)')
                call WDPutEntryDouble(enti(2),bb,'(es14.7)')
                call gtk_widget_set_sensitive(idpt(enti(1)),0_c_int)
                call gtk_widget_set_sensitive(idpt(enti(2)),0_c_int)
            elseif(ivt == 9) then
                k_datvar = MDpointrev(ks)
                if(k_datvar > 0) then
                    DistPars%pval(nn,1) = nvMD(k_datvar)-ONE
                    DistPars%pval(nn,2) = meanMD(k_datvar)
                    DistPars%pval(nn,3) = smeanMD(k_datvar)
                    do i=1,3
                        call WDPutEntryDouble(enti(i),DistPars%pval(nn,i),'(es14.7)')
                    end do
                else
                    call gtk_widget_set_sensitive(idpt(enti(1)),0_c_int)
                    call gtk_widget_set_sensitive(idpt(enti(2)),0_c_int)
                    call gtk_widget_set_sensitive(idpt(enti(3)),0_c_int)
                    call gtk_widget_set_visible(idpt(enti(4)),0_c_int)
                    write(log_str, '(*(g0))') 'LDN_745: nmxDist=',int(nmxDist,2)
                    call logger(66, log_str)
                    if(nn > 0) then
                        do i=1,3
                            call WDPutEntryDouble(enti(i),DistPars%pval(nn,i),'(es14.7)')
                        end do
                    end if
                end if
            else
                call gtk_widget_set_sensitive(idpt(enti(1)),1_c_int)
                call gtk_widget_set_sensitive(idpt(enti(2)),1_c_int)
            end if
            do i=1,4  ! nvisib
                entival(i) = DistPars%pval(nn,i)
            end do
            if(open_project_parts .and. setDP) goto 9000


          case (75)        ! dialog_InfoFX

          case (76)        ! dialog_Decaychain

            if(.not.allocated(DCnuclide)) then
              nc = 4
              allocate(DCindx(nc),DCnuclide(nc),DCsymbT12(nc),DCsymbEffiA(nc),  &
                       DCsymbEffiB(nc),DCsymbEffiC(nc),DCsymbYield(nc),DCsymbLambda(nc), &
                       DCzji(nc),DCzjiVal(nc) )
            end if

            if(.not.FitDecay) then
              call gtk_widget_set_sensitive(idpt('DCgenerateXi'),0_c_int)
            else
              call gtk_widget_set_sensitive(idpt('DCgenerateXi'),1_c_int)
            endif
            call WDSetCheckButton('DCcheckVorLam',1)

            call WDGetCheckButton('DCcheckVorLam',iv)
            if(iv == 1) then
              call WDPutTreeViewColumnLabel('treeview9', 3, 'Lambda'//char(13)//'Symbol')
            else
              call WDPutTreeViewColumnLabel('treeview9', 3, 'T12'//char(13)//'Symbol')
            endif
            if(ChainSelected > 0) call WDSetComboboxAct('ComboboxDCchains',ChainSelected)

          case (77)           ! Reload missing values
            call WDPutLabelString('LBSelFil', T('File with project file names (txt)'))
          case default

        end select

        ! Prepare now for showing the dialog:
        if(prout)  then
            write(log_str, '(*(g0))') 'before widget_show:    dialogstr=',trim(dialogstr)
            call logger(66, log_str)
        end if

        !  goto 9000
!-------------------------------------------------------------------------------------------
1000    continue
        if(prout)  then
            write(log_str, '(*(g0))') 'Label 1000 reached;   dialogstr=',trim(dialogstr), &
            '  ioption=',ioption
            call logger(66, log_str)
        end if

1010    continue

        if(.true. .and. (ioption == 3 .or. ioption == 5 .or. ioption == 8)) then
            nt = 0
            do j=1,ntvs
                if(ioption == 3 .and. 'treeview5' == tvnames(j)%s) then; nt = j; exit; end if
                if(ioption == 5 .and. 'treeview6' == tvnames(j)%s) then; nt = j; exit; end if
                if(ioption == 8 .and. 'treeview7' == tvnames(j)%s) then; nt = j; exit; end if
            end do
            call TVtrimCol_width(tvnames(nt)%s)

        end if

        FieldEditCB = .False.
        ButtonClicked = .False.
        FieldDoActCB = .False.
        PageSwitchedCB = .false.
        CloseDialogCB = .false.
        WinMC_resized = .false.
        ncitem2 = 0
        resp_id = 0_c_Int

! show the dialog
        call gtk_widget_show_all(dialog)

        if(nt > 0) then
            do n=1,tvcols(nt)
                write(chcol,'(i0)') tvcolindex(nt,n)
                !write(66,*) trim(tvnames(nt)),' : width col',int(n,2),': ',  &
                !              gtk_tree_view_column_get_width(idpt('treeviewcolumn'// trim(adjustL(chcol))))
            end do
        end if

! start the dialog loop now:
        dialogloop_on = .true.

        if(prout)  then
            write(log_str, '(*(g0))') '----- Loop start:'
            call logger(66, log_str)
        end if

        do
            resp_id = gtk_main_iteration()

            if(FieldDoActCB) exit     !  criteria for stopping the loop
            if(buttonClicked) exit    !
            if(CloseDialogCB) exit    !
            if(FieldEditCB) exit            ! 13.4.2023
        end do
        dialogloop_on = .false.

        if(prout) then
            call logger(66, '----- Loop end:')
            write(log_str, '(a,i2)') '       resp_id=',resp_id
            call logger(66, log_str)
            if(FieldDoActCB)  then
                write(log_str, '(*(g0))') '    FieldDoActCB=',FieldDoActCB
                call logger(66, log_str)
            end if
            if(buttonClicked)  then
                write(log_str, '(*(g0))') '    ButtonClicked=',ButtonClicked
                call logger(66, log_str)
            end if
            if(CloseDialogCB)  then
                write(log_str, '(*(g0))') '    CloseDialogCB=',CloseDialogCB
                call logger(66, log_str)
            end if
            if(FieldEditCB)  then
                write(log_str, '(*(g0))') '    FieldEditCB=', FieldEditCB
                call logger(66, log_str)
            end if
        end if

        if(clobj%name(ncitemclicked)%s == 'GtkButton' .and. HelpButton) then
            ! The case of HelpFX considers several help topics and muist therefore
            ! handled below under the idstring 'HelpFX'
            ! 9.3.2024
            if(clobj%idd(ncitemclicked)%s /= 'HelpFX') then
                call DisplayHelp(ncitemclicked)
                goto 1010
            endif
        end if

        dialog_leave = 0
        if(resp_id == -3 .or. resp_id == -5 .or. resp_id == -8 .or. resp_id == -10) then
            Objstr = 'GtkButton'
            widgetlabel = 'OK'
            ncitem2 = ncitemClicked
            dialog_leave = 1
            if(prout)  then
                write(log_str, '(*(g0))') '     Exit A'
                call logger(66, log_str)
            end if
            goto 1100
        end if
        if(resp_id == -6 .or. resp_id == -9) then
            Objstr = 'GtkButton'
            widgetlabel = 'Cancel'
            ncitem2 = ncitemClicked
            if(prout)  then
                write(log_str, '(*(g0))') '     Exit B'
                call logger(66, log_str)
            end if
            goto 1100
        end if
        if(ButtonClicked .or. FieldDoActCB .or. CloseDialogCB .or. FieldEditCB) then
            ncitem2 = ncitemClicked
            objstr = clobj%name(ncitem2)%s
            idstring = clobj%idd(ncitem2)%s
            widgetlabel = clobj%label(ncitem2)%s
            signal = clobj%signal(ncitem2)%s
            !!!!!!!!!!!! call gtk_widget_hide(dialog)      ! deacivated 17.9.2023
            if(prout)  then
                write(log_str, '(*(g0))') '     Exit C'
                call logger(66, log_str)
            end if
            goto 1100
        end if
1100    continue

        if(prout) then
            if(FieldEditCB)  then
                write(log_str, '(*(g0))') 'LOADSEL:  FieldEditCB found:  ',trim(idstring)
                call logger(66, log_str)
            end if
            if(ButtonClicked)  then
                write(log_str, '(*(g0))') 'LOADSEL:  ButtonClicked found  ',trim(idstring),  &
                '  label=',trim(widgetlabel)
                call logger(66, log_str)
            end if
        end if

        if(prout) then
            write(log_str, '(*(g0))') 'resp_id per call =',resp_id
            call logger(66, log_str)
            write(log_str, '(*(g0))') ' item_clicked- string:', trim(idstring),'   wlabel=',trim(widgetlabel), &
                '   objstr=',trim(objstr)
            call logger(66, log_str)
        end if

        if(prout)  then
            write(log_str, '(*(g0))') "Label: ",trim(widgetlabel),' idstring=',trim(idstring)
            call logger(66, log_str)
        end if

        ! After the dialog loop is stopped: start now to interpret the contents of the dialog,
        ! dependent on the dialog opened (ioption)
        ! Some dialog items may require an action by the program, i.e., other dialog items
        ! dependent on them need then to be changed also (require an action); these actions
        ! are handled
        ! A goto 1010 means to restart the dialog loop, i.e., for instance an option has been
        ! modified on which other dialog items may depend, which have to be modified also

        select case (trim(objstr))
          case ('GtkButton')
            if(prout)   then
                write(log_str, '(*(g0))') ' GtkButton akzeptiert','  widgetlabel=',trim(widgetlabel)
                call logger(66, log_str)
            end if
            select case (trim(widgetlabel))

              case ('Auswählen', 'OK', 'Apply' )    ! select, OK, apply

                dialog_leave = 1

                select case (ioption)
                  case (1)
                    call WDGetEntryDouble('entryOptKalpha',kalpha)
                    call WDGetEntryDouble('entryOptKbeta',kbeta)
                    call WDGetEntryDouble('entryOptAlpha',alpha)
                    call WDGetEntryDouble('entryOptBeta',beta)
                    call WDGetEntryDouble('entryOptCoverf',coverf)
                    call WDGetEntryDouble('entryOptCoverIn',coverin)
                    call WDGetEntryDouble('entryOpt1minusG',W1minusG)
                    call WDGetEntryString('entryOptDLMethod',NWGMethode)
                    call WDGetEntryDouble('entryOptGamDistAdd',GamDistAdd)

                    ! Check consistency of quantiles and probabilities:
                    lpass = .TRUE.
                    xxs1 = ONE - pnorm(kalpha)
                    str1 = ' '
                    IF(ABS(xxs1-alpha) > 1.E-6_rn) THEN
                        lpass = .FALSE.
                        str1 = trim(str1) // &
                               T('alpha and k_alpha do not fit together!')

                    end if
                    xxs1 = ONE - pnorm(kbeta)
                    IF(ABS(xxs1-beta) > 1.E-6_rn) then
                        lpass = .FALSE.
                        str1 = trim(str1) // CHAR(13) // &
                               T('beta and k_beta do not fit together!')
                    end if
                    if( .not. lpass) then

                        str1 = trim(str1) // CHAR(13) // &
                               T("Press button 'load values'!")

                        call MessageShow(trim(str1), GTK_BUTTONS_OK, "LoadSel:", resp,mtype=0_c_int)
                        dialog_on = .false.
                        call gtk_widget_set_sensitive(idpt('menubar1'), 1_c_int)
                        goto 9000
                    end if

                    IF(abs(kalpha1-kalpha)/kalpha1 > 1.E-5_rn .OR. abs(kbeta1-kbeta)/kbeta1 > 1.E-5_rn .OR.  &
                        abs(coverf1-coverf)/coverf1 > 1.E-5_rn .OR. abs(coverin1-coverin)/coverin1 > 1.E-5_rn .or. &
                        abs(W1minusG1-W1minusG)/W1minusG1 > 1.E-5_rn .OR. &
                        abs(alpha1-alpha)/alpha1 > 1.E-5_rn .OR. abs(beta1-beta)/beta1 > 1.E-5_rn .or. &
                        abs(GDA1-GamDistAdd) > 1.E-5_rn   )  THEN
                        SaveP = .TRUE.
                        MCsim_on = .FALSE.
                        if(NBcurrentPage >= 3) then
                            call gtk_widget_set_sensitive(idpt('NBBudget'), 0_c_int)
                            call gtk_widget_set_sensitive(idpt('NBResults'), 0_c_int)
                            call gtk_widget_set_visible(idpt('NBBudget'), 0_c_int)
                            call gtk_widget_set_visible(idpt('NBResults'), 0_c_int)
                            call gtk_widget_hide(idpt('box5'))
                            call gtk_widget_hide(idpt('grid5'))
                            call WDNotebookSetCurrPage('notebook1', 3)
                            call ClearMCFields(1)
                            call WrStatusbar(4,T('Update calculations!'))

                        end if
                    end if
                    call FieldUpdate()
                    IF(SaveP) then
                        call WrStatusbar(3, T('unsaved'))
                        if(prout)  then
                            write(log_str, '(*(g0))') 'SaveP = .T. after agetting values from the Options dialog'
                            call logger(66, log_str)
                        end if
                    end if
                    call WDGetComboboxAct('comboboxLangg',k1lang)
                    call WDGetComboboxAct('comboboxtextListSep',klss)
                    if(klss == 1) sListSeparator = ';'
                    if(klss == 2) sListSeparator = ','

                    ! Save the new language to the config file?
                    ! check the value in the config file:
                    call read_config('Language', langgSV, work_path // UR2_CFG_FILE)

                    if (get_language() /= langgSV) then
                        call MessageShow(T("Shall the new language shortcut be saved in UR2_cfg.dat?"), &
                                         GTK_BUTTONS_YES_NO, "", resp, mtype=0_c_int)
                        if (resp == GTK_RESPONSE_YES) then
                            call SaveToConfig(1, langg)
                        end if
                    end if
                    ! end if

                  case (2)
                    ! ifitXX = ifit
                    mac = 0       ! 17.9.2023
                    call WDGetComboboxAct('comboboxA1', ifit(1))
                    call WDGetComboboxAct('comboboxA2', ifit(2))
                    call WDGetComboboxAct('comboboxA3', ifit(3))
                    !---++ 29.1.2024:
                    nn = 0
                    do i=1,3
                        if(ifit(i) < 3) nn = nn + 1
                    end do
                    if(nn > knumEGr) then
                        call CharModStr(str1,500)
                        str1 = T("Note: Fitting more parameters than the specified number of result variables!")

                        call MessageShow(trim(str1), GTK_BUTTONS_OK, "LDN_1128:", resp, mtype=GTK_MESSAGE_WARNING)
                    end if
                    !---++

                    if(ifitXX(1) /= ifit(1) .or. ifitXX(2) /= ifit(2) .or. ifitXX(3) /= ifit(3)) dmodif = .true.    ! 30.6.2023
                    do i=1,knumEGr      !   1,3
                        if(ifit(i) == 1 .and. ifitXX(i) > 1 ) then
                            if(i == 1) call gtk_widget_set_sensitive(idpt('QFirst'),1_c_int)
                            if(i == 2) call gtk_widget_set_sensitive(idpt('QSecond'),1_c_int)
                            if(i == 3) call gtk_widget_set_sensitive(idpt('QThird'),1_c_int)
                        elseif(ifit(i) > 1 .and. ifitXX(i) == 1 ) then
                            if(i == 1) call gtk_widget_set_sensitive(idpt('QFirst'),0_c_int)
                            if(i == 2) call gtk_widget_set_sensitive(idpt('QSecond'),0_c_int)
                            if(i == 3) call gtk_widget_set_sensitive(idpt('QThird'),0_c_int)
                        end if
                    end do
                    if(prout)  then
                        write(log_str, '(*(g0))') ' LoadSel:    ifit=', ifit
                        call logger(66, log_str)
                    end if
                    call WDGetCheckButton('checkbuttonWFit', nwei)
                    call WDGetCheckButton('checkbuttonCovZR', nkovzr)
                    call WDGetCheckButton('checkbuttonAllm', ndefall)
                    if(prout)  then
                        write(log_str, '(*(g0))') 'LoadseL:    nwei=',nwei,'  nkovzr=',nkovzr,'  ndefall=',ndefall
                        call logger(66, log_str)
                    end if

                    defineallxt = .false.
                    if(ndefall == 1) defineallxt = .true.

                    call WDGetSelRadio('radiobuttonNLSQ', kfitmeth)
                    if(prout)  then
                        write(log_str, '(*(g0))') ' LOADSEL: Readout RadiobuttonNLSQ:  original kfitmeth=',kfitmeth
                        call logger(66, log_str)
                    end if
                    ! The first of the radiobuttons in the group must in UR be associated kfitmeth = 0
                    kfitmeth = kfitmeth - 1
                    IF(kfitmeth == 0) kpearson = 0                    !
                    IF(kfitmeth == 1) kpearson = 1                    !
                    kPMLE = 0                                         !
                    kWTLS = 0                                         !
                    IF(kfitmeth == 2) kPMLE = 1                       !
                    IF(kfitmeth == 3) kWTLS = 1                       !
                    kWTLS = 0
                    use_WTLS = .FALSE.
                    IF(kfitmeth == 3) kWTLS = 1
                    IF(kWTLS == 1) use_WTLS = .TRUE.

                    fitmeth = 'WLS'
                    IF(kpearson == 1) fitmeth = 'PLSQ'
                    IF(kPMLE == 1) fitmeth = 'PMLE'
                    IF(kWTLS == 1) then
                        fitmeth = 'WTLS'
                        if(bat_mc) fitmeth = 'WTLS'
                        if(ifit(1) == 2 .or. ifit(2) == 2 .or. ifit(3) == 2) then
                            call CharModStr(str1,500)
                            str1 = T("With WTLS, the fit option 'fixed' is not allowed!")

                            call MessageShow(trim(str1), GTK_BUTTONS_OK, "LDN_1074:", resp,mtype=GTK_MESSAGE_WARNING)
                            goto 1010
                        end if
                    end if
                    klincall = 0
                    if(prout)  then
                        write(log_str, '(*(g0))') 'LoadSel:    fitmeth=',fitmeth,' kfitmeth=',int(kfitmeth,2)
                        call logger(66, log_str)
                    end if

                    call WDGetComboboxAct('comboboxtextNCH', nchannels)
                    call WDGetTextviewString('textviewModelEQ',FormeltextFit)

                    call WrStatusBar(4, T("Next: TAB 'Values, uncertainties': Button 'Calculation of (remaining) uncertainties'!"))

                    if(.not. dnew ) then
                        !!!! dmodif = .false.    ! deactivated 17.9.2023
                        if(ubound(FormeltextFit,dim=1) /= size(FTF)) then
                            dmodif = .true.
                        else
                            do i=1,size(FTF)
                                if(FTF(i)%s /= FormelTextFit(i)%s) then
                                    dmodif = .true.
                                    exit
                                end if
                            end do
                        end if
                    end if
                    if(.not.dmodif) then
                        ! 30.6.2023:
                        if(kfm1 /= kfitmeth .or. nwei1 /= nwei .or. nkovzr1 /= nkovzr .or. nch1 /= nchannels &
                            .or. ndefall1 /= ndefall) dmodif = .true.
                    end if
                    ! syntax_check is set in the UR_Field_edit_CB function
                    if(dmodif) then
                        SaveP = .true.
                        call FieldUpdate()
                        ! syntax_check = .true.
                    else
                        SaveP = .false.
                        call FieldUpdate()
                        ! syntax_check = .false.
                        ! call WrStatusbar(3,' ')
                    end if

                    mfitfix = 0
                    do i=1,ma
                        if(ifit(i) <= 2) mfitfix = mfitfix + 1
                        if(ifit(i) == 2) then
                            fpa(i) = ZERO
                            sfpa(i) = ZERO
                        end if
                    end do

                    ! addition: taken from Rechw1
                    ifitSV = ifit
                    knt = 0
                    IF(kPMLE == 1) THEN
                        ! preparation for PMLE:
                        IF(singlenuk) THEN
                            mfit2 = 0                             !
                            do i=1,3                              ! 21.12.2024 GK  , fehlte
                                if(ifit(i) == 1) mfit2 = mfit2 + 1  !
                            end do                                !
                            knt = max(knumEGr, mfit2)
                            if(knt < 3) then
                                if(ifit(knt+1) >= 2 )  then
                                    ! omit:
                                    ifit(knt+1) = 2
                                    mfrbg = knt+1
                                end if
                            end if
                        elseIF(knumEGr < 3) THEN
                            ifit(knumEGr+1) = 2
                            mfrbg = knumEGr + 1
                        end if
                        write(log_str, '(a,i0)') 'LDN: mfrbg=',mfrbg
                        call logger(66, log_str)
                        if(mfrbg == 0) then
                            call CharModStr(str1,500)
                            str1 = T("In this case, PMLE is not allowed!")

                            call MessageShow(trim(str1), GTK_BUTTONS_OK, "LDN_1164:", &
                                             resp, mtype=GTK_MESSAGE_WARNING)
                            goto 1010
                        end if
                    else
                        mfrbg = 0
                    end if

                  case (3)
                    ! if(trim(idstring) == 'CalcCountRates' .or. trim(idstring) == 'DecayValsOK') then
                    ! if(trim(idstring) == 'DecayValsOK') then
                    call WDGetEntryString('entrySeparation', CFaelldatum)
                    call WDGetComboboxAct('comboboxtextbase', linfzbase)
                    zfact = ONE
                    IF(linfzbase == 2) zfact = 60.0_rn
                    if(linfzbase == 0) then
                        call CharModStr(str1,500)
                        str1 = T("Decay curve data: The time-base unit is yet undefined!")

                        call MessageShow(trim(str1), GTK_BUTTONS_OK, "LDN_1183:", resp,mtype=GTK_MESSAGE_WARNING)
                        goto 1010
                    end if
                    kxy = ndatmax
                    if(allocated(dmesszeit)) then
                        deallocate(dmesszeit,dbimpulse,dbzrate,sdbzrate,d0messzeit, &
                            d0impulse,d0zrate,sd0zrate,dnetrate,sdnetrate, &
                            CStartzeit)
                    end if
                    call InitVarsTV5(kxy)
                    if(linfzbase /= kbaseX .or. (use_absTimeStart .and. trim(CFaelldatum) /= trim(cfdatx))) then
                        SaveP = .TRUE.
                        call FieldUpdate()
                        call WrStatusBar(3, T('unsaved') // "!")

                    end if
                    if(allocated(sd0zrateSV)) deallocate(d0zrateSV,sd0zrateSV)
                    allocate(d0zrateSV(kxy),sd0zrateSV(kxy))
                    if(allocated(dtdiff)) deallocate(dtdiff)
                    allocate(dtdiff(kxy))

                    call WTreeViewGetStrArray('treeview5', 2, kxy, CStartzeit)
                    call WTreeViewGetDoubleArray('treeview5', 3, kxy, dmesszeit)
                    call WTreeViewGetDoubleArray('treeview5', 4, kxy, dbimpulse)
                    call WTreeViewGetDoubleArray('treeview5', 5, kxy, dbzrate)
                    call WTreeViewGetDoubleArray('treeview5', 6, kxy, sdbzrate)
                    call WTreeViewGetDoubleArray('treeview5', 7, kxy, d0messzeit)
                    call WTreeViewGetDoubleArray('treeview5', 8, kxy, d0impulse)
                    call WTreeViewGetDoubleArray('treeview5', 9, kxy, d0zrate)
                    call WTreeViewGetDoubleArray('treeview5', 10, kxy, sd0zrate)
                    call WTreeViewGetDoubleArray('treeview5', 11, kxy, dnetrate)
                    call WTreeViewGetDoubleArray('treeview5', 12, kxy, sdnetrate)

                    do i=1,kxy
                        if(abs(dmesszeit(i)-missingval) < EPS1MIN .or. abs(dmesszeit(i)) < EPS1MIN) then
                            numd = MAX(0,i-1)
                            ! write(66,*) 'LDN-1289: set numd=',int(numd,2)
                            EXIT
                        END IF
                    end do
                    if(numd == 0) then
                        ifehl = 1           !   29.1.2024: to prevent from further calculations
                        goto 9000           !              with missing data
                    end if

                    IF(numd /= numrowsold) THEN
                        SaveP = .TRUE.
                        call FieldUpdate()
                        call WrStatusBar(3, T('unsaved') // "!")

                    end if
                    ! convert to the basic time unit second:
                    dmesszeit(1:numd) = dmesszeit(1:numd) * zfact
                    dbzrate(1:numd)   = dbzrate(1:numd) / zfact
                    sdbzrate(1:numd)  = sdbzrate(1:numd) / zfact
                    d0messzeit(1:numd)= d0messzeit(1:numd) * zfact
                    d0zrate(1:numd)   = d0zrate(1:numd) / zfact
                    sd0zrate(1:numd)  = sd0zrate(1:numd) / zfact
                    dnetrate(1:numd)  = dnetrate(1:numd) / zfact
                    sdnetrate(1:numd) = sdnetrate(1:numd) / zfact
                    d0zrateSV(1:numd) = d0zrate(1:numd)
                    sd0zrateSV(1:numd) = sd0zrate(1:numd)
                    if(.not.loadingPro) then
                        if(SaveP) then
                            dmodif = .true.
                        end if
                        if(.not.SaveP) SaveP = SaveP_sv
                    end if
                    write(log_str, '(*(g0))') 'TV5: width col 4: ',gtk_tree_view_column_get_width(idpt('treeviewcolumn34'))
                    call logger(66, log_str)
                    write(log_str, '(*(g0))') 'TV5: width col 5: ',gtk_tree_view_column_get_width(idpt('treeviewcolumn35'))
                    call logger(66, log_str)
                    write(log_str, '(*(g0))') 'TV5: width col 6: ',gtk_tree_view_column_get_width(idpt('treeviewcolumn36'))
                    call logger(66, log_str)
                    write(log_str, '(*(g0))') 'TV2: width col 7: ',gtk_tree_view_column_get_width(idpt('treeviewcolumn37'))
                    call logger(66, log_str)
                    write(log_str, '(*(g0))') 'TV2: width col 8: ',gtk_tree_view_column_get_width(idpt('treeviewcolumn38'))
                    call logger(66, log_str)
                    ! write(0,*) 'Decay curve reading completed'
                    if(ngrs+ncov+numd > ubound(Messwert,dim=1)) then
                        ! 20.6.2024:
                        call CharModA1(Symbole,numd)
                        call CharModA1(SymboleG,numd)
                        call RealModA1(Messwert,ngrs+ncov+numd)
                        call RealModA1(MesswertSV,ngrs+ncov+numd)
                        call RealModA1(StdUnc,ngrs+ncov+numd)
                        call RealModA1(StdUncSV,ngrs+ncov+numd)
                        call RealModA1(sensi,ngrs+ncov+numd)
                        call RealModA1(perc,ngrs+ncov+numd)
                    endif


                  case (5)
                    call GetGamData()
                    if(.not.loadingPro) then
                        if(SaveP) then
                            gmodif = .true.
                        end if
                        if(.not.SaveP) SaveP = SaveP_sv
                    end if
                    if(ecorruse == 1) ncov = (numd/5)*(numd/5-1)/2
                    ngrsP = ngrs + numd + ncov
                    call CharModA1(Symbole,ngrsP)
                    call CharModA1(SymboleG,ngrsP)
                    call RealModA1(Messwert,ngrsP)
                    call RealModA1(MesswertSV,ngrsP)
                    call RealModA1(StdUnc,ngrsP)
                    call RealModA1(corrval,ncov)
                    call RealModA1(covarval,ncov)
                    call IntModA1(icovtyp,ncov)
                    call CharModA1(CVFormel,ncov)

                    ! for TV4:
                    call CharModA1(Symtyp,ngrsP)
                    call CharModA1(einheit,ngrsP)
                    call RealModA1(Sensi,ngrsP)
                    call RealModA1(Perc,ngrsP)

                  case (6)
                    indx = hl_gtk_combo_box_get_active(idpt('comboboxtextKnumegr'),ctext, zstr)
                    read(zstr,*) knumEGr
                    if(prout)  then
                        write(log_str, '(*(g0))') '-------------  Aus dem Dialog gelesener Wert knumEGr: ',knumEGr
                        call logger(66, log_str)
                    end if
                    if(allocated(Symbole)) then
                        if(ubound(Symbole,dim=1) >= knumEGr) call SetMenuEGr(knumEGr)
                    else
                        ! 10.8.2023:
                        allocate(Symbole(knumEGr))
                        if(ubound(Symbole,dim=1) >= knumEGr) call SetMenuEGr(knumEGr)
                    end if
                    write(0,*) 'Nach call SetMenuEgr:'
                    if(knumEGr == 1) then
                        call gtk_widget_set_sensitive(idpt('ConfidEllipse'),0_c_int)
                    else
                        call gtk_widget_set_sensitive(idpt('ConfidEllipse'),1_c_int)
                    end if
                    ! exit

                  case (7)
                    call WDGetComboboxAct('comboboxSymbchg', kopt)

                    if(allocated(cdummy)) deallocate(cdummy)
                    allocate(character(len=100) ::cdummy)
                    call WDGetEntryString('entrySymbchg', cdummy)
                    Sname = trim(cdummy)
                    call ChangeSname()
                    if(ifehl == 1) goto 1010

                  case (8)
                    call WDGetEntryString('entryDKTitel', CCTitle)
                    call WDGetComboboxAct('comboboxDKPgrad',kal_polgrad)
                    kal_Polgrad = kal_Polgrad - 1
                    call WDGetCheckButton('DKcheckUfit', k)
                    use_UfitKal = .false.
                    if(k == 1) use_UfitKal = .true.
                    call WDGetCheckButton('DKcheckWTLS', k)
                    use_WTLS_kal = .false.
                    if(k == 1) use_WTLS_kal = .true.    ! 7.8.2023

                  case (62)
                    ! FontSelection button:
                    if(.not.allocated(fontname)) allocate(character(len=100) :: fontname)
                    call gtk_widget_set_sensitive(idpt('buttonFBSave'), 1_c_int)
                    answer = gtk_font_button_get_use_font(idpt('fontbutton1'))
                    if(prout)  then
                        write(log_str, '(*(g0))') '   answer von font_button: ',answer
                        call logger(66, log_str)
                    end if
                    pfontname = gtk_font_button_get_font_name(idpt('fontbutton1'))
                    if(c_associated(pfontname)) call c_f_string(pfontname,fontname)
                    if(prout)  then
                        write(log_str, '(*(g0))') 'fontname=',fontname
                        call logger(66, log_str)
                    end if
                    if(.true.) then
                        pfd_ptr = pango_font_description_from_string(trim(fontname)//c_null_char)
                        pfd2_ptr = pango_font_description_to_string(pfd_ptr)
                        if(c_associated(pfd2_ptr)) call c_f_string(pfd2_ptr,fontname)
                        write(log_str, '(*(g0))') ' fontname via pango_font_description_to_string=',fontname
                        call logger(66, log_str)
                        call pango_font_description_set_size(pfd_ptr, 15_c_int)
                        i = pango_font_description_get_size(pfd_ptr)
                        write(log_str, '(*(g0))') '   fontsize read:  ',i
                        call logger(66, log_str)
                    end if
                    call g_value_set_string(pstring, trim(fontname)//c_null_char)
                    call g_object_set_property(Settings%GtkSetDef, trim('gtk-font-name')//c_null_char, pstring)
                    goto 1010

                  case (63)
                    ! color button
                    ! colors can no longer applied to the GTK GUI
                    if(.not.allocated(colorname)) allocate(character(len=80) :: colorname)
                    ! pURcolor => URcolor
                    ! pcolor = c_loc(pURcolor)
                    call gtk_color_chooser_get_rgba(idpt('colorbutton1'), c_loc(URcolor))
                    if(prout)  then
                        write(log_str, '(*(g0))') '  from ColorSelection:   c_associated(pointer pcolor)=',c_associated(pcolor)
                        call logger(66, log_str)
                    end if
!                     if(c_associated(pcolor)) then
!                         call c_f_pointer(pcolor, pURcolor)
!                         if(prout) then
! !                             write(66,*) 'URcolor=',URcolor
!                             write(log_str, '(*(g0))') 'URcolor=',URcolor
!                             call logger(66, log_str)
! !                             write(66,*) '    red    =',URcolor%red
!                             write(log_str, '(*(g0))') '    red    =',URcolor%red
!                             call logger(66, log_str)
! !                             write(66,*) '    green  =',URcolor%green
!                             write(log_str, '(*(g0))') '    green  =',URcolor%green
!                             call logger(66, log_str)
! !                             write(66,*) '    blue   =',URcolor%blue
!                             write(log_str, '(*(g0))') '    blue   =',URcolor%blue
!                             call logger(66, log_str)
! !                             write(66,*) '    alpha  =',URcolor%alpha
!                             write(log_str, '(*(g0))') '    alpha  =',URcolor%alpha
!                             call logger(66, log_str)
!                         end if
!                     end if
                    write(colorname,'(a1,z2.2,z2.2,z2.2)') '#',int(URcolor%red*256_c_double), &
                        int(URcolor%green*256_c_double), &
                        int(URcolor%blue*256_c_double)

                    write(log_str, '(*(g0))') 'Selected color name=',Colorname
                    call logger(66, log_str)
                    !!   goto 1010

                    do i=1,Settings%nprops
                        if(trim(Settings%sproperty(i)) == 'gtk-color-scheme') then
                            if(kcolortype == 1) then
                                i1 = index(Settings%sproperty_val(i),'bg_color:')
                                if(i1 > 0) then
                                    Settings%sproperty_val(i) = trim(Settings%sproperty_val(i)(1:i1+8)) &
                                        // trim(colorname)       &
                                        // trim(Settings%sproperty_val(i)(i1+9+7:))
                                    if(prout)  then
                                        write(log_str, '(*(g0))') 'new gtk-color-scheme value-string: ',trim(Settings%sproperty_val(i))
                                        call logger(66, log_str)
                                    end if
                                end if
                            elseif(kcolortype == 2) then
                                i1 = index(Settings%sproperty_val(i),'selected_bg_color:')
                                if(i1 > 0) then
                                    Settings%sproperty_val(i) = trim(Settings%sproperty_val(i)(1:i1+17)) &
                                        // trim(colorname)       &
                                        // trim(Settings%sproperty_val(i)(i1+18+7:))
                                    if(prout)  then
                                        write(log_str, '(*(g0))') 'new gtk-color-scheme value-string: ',trim(Settings%sproperty_val(i))
                                        call logger(66, log_str)
                                    end if
                                end if
                            end if
                            call g_value_set_string(pstring, trim(Settings%sproperty_val(i))//c_null_char)
                            call g_object_set_property(Settings%GtkSetDef, trim(Settings%sproperty(i))//c_null_char, pstring)
                            exit
                        end if
                    end do
                    goto 1010



                  case (64)     ! filechooser dialog

                  case (65)     ! dialogELI
                    call WDGetCheckButton('checkbuttonELI_EG1',igsel(1))
                    call WDGetCheckButton('checkbuttonELI_EG2',igsel(2))
                    call WDGetCheckButton('checkbuttonELI_EG3',igsel(3))
                    call WDGetCheckButton('checkbuttonRS',eliRS)
                    call WDGetComboboxAct('comboboxGrELI',iopt_copygr)
                    k = 0
                    do i=1,3
                        if(igsel(i) == 1) k = k + 1
                    end do
                    if(k > 1) call PlotEli()
                    goto 1010

                  case (67)       ! Exchange 2 output quantity symbols
                    call WDGetComboboxAct('comboboxSymbExchgA',k1_exchg)
                    call WDGetComboboxAct('comboboxSymbExchgB',k2_exchg)
                    if(k1_exchg /= k2_exchg .and. knumEGr > 1) then
                        call Exchange2Symbols(k1_exchg, k2_exchg)
                    end if

                  case (69)
                    call WDGetComboboxAct('combobox_MDselVar',k_datvar)
                    call WDGetComboboxAct('combobox_MDtyp',k_MDtyp(k_datvar))
                    call GetxdataMD_fromTree()
                    if(ifehl == 0) then

                        nv = nvalsMD(k_datvar)
                        if(nv > 1) then
                            call MDcalc(k_datvar)
                            if(k_MDtyp(k_datvar) == 2) then
                                call WDPutLabelString('LBsdMD','sn')
                                call WDPutLabelString('LBnvar0MD','s0n')
                            else
                                call WDPutLabelString('LBsdMD','sx')
                                call WDPutLabelString('LBnvar0MD','s0x')
                            end if
                            call WDPutEntryDouble('TEmeanMD',meanMD(k_datvar),'(es12.6)')
                            call WDPutEntryDouble('TEsdMD',umeanMD(k_datvar),'(es12.6)')
                            call WDPutEntryDouble('TEnvar0MD',smeanMD(k_datvar)**ONE,'(es12.6)')
                            call WDPutEntryInt('TEmMD',nvalsMD(k_datvar),'(i0)')
                        end if
                        call WDGetEntryDouble('TEmeanMD', meanMD(k_datvar))
                        call WDGetEntryDouble('TEsdMD', umeanMD(k_datvar))

                        ! call WDGetComboboxAct('combobox_RefMD',refdataMD)
                        if(refdataMD > 0) call WDGetComboboxAct('combobox_RefMD',refdataMD)     ! 12.8.2023
                        if(refdataMD > 0) rinflu_known = .true.
                        write(log_str, '(*(g0))') 'LDN_1436: rinflu_known=',rinflu_known,' refdataMD=',refdataMD
                        call logger(66, log_str)
                    end if

                  case (70)
                    cp1 = gtk_file_chooser_get_filename(idpt('ChooserButton1SE'))
                    if(c_associated(cp1)) call c_f_string(cp1, base_project_SE)
                    write(log_str, '(*(g0))') 'base_project_SE =',trim(base_project_SE)
                    call logger(66, log_str)
                    cp2 = gtk_file_chooser_get_filename(idpt('ChooserButton2SE'))
                    if(c_associated(cp2)) then
                        call c_f_string(cp2,serial_csvinput)
                        write(log_str, '(*(g0))') 'serial_csvinput =',trim(serial_csvinput)
                        call logger(66, log_str)
                    end if
                    call WDGetEntryInt('entryFromSE', kfrom_SE)
                    call WDGetEntryInt('entryToSE', kto_SE)
                    call WDGetCheckButton('CheckMCSE', i1)
                    bat_mc = .false.
                    if(i1 == 1) then
                        bat_mc = .true.
                        call WDGetEntryInt('EntryMCnumSE', kcmxMC)
                        call WDPutEntryInt('TRentryMCanzM', kcmxMC)
                        call WDGetEntryInt('EntryRunsMCSE', kcrunMC)
                        call WDPutEntryInt('TRentryMCanzR', kcrunMC)
                        use_BCI = .FALSE.
                        call WDSetCheckButton('TRcheckbutton2', 0)
                    end if
                    call WDGetCheckButton('CheckMCMCSE', i1)

                  case (71)       ! dialog_symchg  for input of kbgv_binom
                    call WDGetComboboxAct('comboboxBinPoi1', ip_binom)
                    call WDGetComboboxAct('comboboxBinPoi2', kbgv_binom)
                    call WDGetComboboxAct('comboboxBinPoi3', itm_binom)
                    call WDGetComboboxAct('comboboxBinPoi4', ilam_binom)
                    use_bipoi = .true.
                    write(log_str, '(*(g0))') 'Load (71): ip_binom, kbgv_binom, itm_binom,ilam_binom=', &
                        ip_binom, kbgv_binom, itm_binom,ilam_binom
                    call logger(66, log_str)

                  case (72)          ! BatestUser
                    cp1 = gtk_file_chooser_get_filename(idpt('BTchooserButton_1'))
                    cp2 = gtk_file_chooser_get_filename(idpt('BTchooserButton_2'))
                    if(c_associated(cp1)) call c_f_string(cp1,Batest_ref_file_ch)
                    write(log_str, '(*(g0))') 'Batest_ref_file_ch =',trim(Batest_ref_file_ch)
                    call logger(66, log_str)
                    if(c_associated(cp2)) call c_f_string(cp2,Batest_out_ch)
                    write(log_str, '(*(g0))') 'Batest_out_ch =',trim(Batest_out_ch)
                    call logger(66, log_str)

                  case (73)
                    cp2 = gtk_file_chooser_get_filename(idpt('ChooserButton2BEV'))
                    if(c_associated(cp2)) then
                        call c_f_string(cp2,batf_file);  write(log_str, '(*(g0))') 'prodat =',trim(batf_file)
                        call c_f_string(cp2,batf_file);  call logger(66, log_str)
                    end if
                    call WDGetEntryInt('entryFromBEV', kfrom_SE)
                    call WDGetEntryInt('entryToBEV', kto_SE)
                    call WDGetCheckButton('CheckMCBEV', i1)
                    call WDGetCheckButton('CheckReportBEV', i2)
                    batf_reports = .false.
                    if(i2 == 1) batf_reports = .true.

                    bat_mc = .false.
                    if(i1 == 1) then
                        bat_mc = .true.
                        call WDGetEntryInt('EntryMCnumBEV', kcmxMC)
                        call WDPutEntryInt('TRentryMCanzM', kcmxMC)
                        call WDGetEntryInt('EntryRunsMCBEV', kcrunMC)
                        call WDPutEntryInt('TRentryMCanzR', kcrunMC)
                        use_BCI = .FALSE.
                        call WDSetCheckButton('TRcheckbutton2', 0)
                    end if

                  case (74)         ! dialog_distributions
                    nn = max(0, nmxDist)
                    if(nn > 0) then
                        nn = findlocT(DistPars%symb,Symbole(top_selrow)%s)
                    end if
                    write(log_str, '(*(g0))') 'DistParsRead: nn=',int(nn,2)
                    call logger(66, log_str)
                    if(nn == 0) then
                        if(allocated(Distpars%symb)) deallocate(Distpars%symb)
                        allocate(DistPars%symb(1)); DistPars%symb(1)%s = ' '
                        nmxDist = nmxDist + 1
                        nn = nmxDist
                    end if
                    if(nn > ubound(Distpars%symb,dim=1)) call CharModA1(Distpars%symb,nn)
                    DistPars%ivtl(nn) = ivt
                    DistPars%symb(nn)%s = Symbole(top_selrow)%s
                    do i=1,4    ! nvisib
                        call WDGetEntryDouble(enti(i),dummy)
                        DistPars%pval(nn,i) = dummy
                    end do

                  case (75)    ! dialog_infoFX

                  case (76)    ! dialog_DecayChain

                    call WDGetCheckButton('DCcheckVorLam',iv)
                    if(iv == 1) then
                      call WDPutTreeViewColumnLabel('treeview9', 3, 'Lambda'//char(13)//'Symbol')
                    else
                      call WDPutTreeViewColumnLabel('treeview9', 3, 'T12'//char(13)//'Symbol')
                    endif
                    call WDGetComboboxAct('ComboboxDCchains',ChainSelected)
                    call WDGetComboboxAct('DCcheckSepar', ksep)
                      apply_separation = .false.
                      if(ksep == 1) apply_separation = .true.
                    call WDGetCheckButton('DCcheckVorLam',iv)
                      use_lambda = .false.
                      if(iv == 1) use_lambda = .true.
                    call WDGetComboboxAct('comboboxtextDCNCH', nnch)
                    call WDGetCheckButton('DCcheckCommMeasmt',kim)
                      DCcommMeasmt = .false.
                      if(kim == 1) DCcommMeasmt = .true.
                    call WDGetEntryInt('entryDCZBuildupNuk',DCBuildupAtSepar)

                  case (77)
                    cp2 = gtk_file_chooser_get_filename(idpt('ChooserButtonSelFil'))

                    if(c_associated(cp2)) then
                      call c_f_string(cp2,missdata_file)
                    !   write(66,*) 'prodat =',trim(missdata_file)
                    end if


                  case default
                    call logger(66, 'LoadSel:  wlabel not accepted')
                end select         ! ioption
                !----------------------
              case ('Cancel','Quit')  !    ! break, stop

                select case (ioption)

                  case (1)

                  case (2)
                    call WDSetComboboxAct('comboboxA1', ifit(1))
                    call WDSetComboboxAct('comboboxA2', ifit(2))
                    call WDSetComboboxAct('comboboxA3', ifit(3))
                    IF(kfitmeth == 0) call WDPutSelRadio('radiobuttonNLSQ', 1)
                    IF(kfitmeth == 1) call WDPutSelRadio('radiobuttonNLSQ', 2)
                    IF(kfitmeth == 2) call WDPutSelRadio('radiobuttonNLSQ', 3)
                    IF(kfitmeth == 3) call WDPutSelRadio('radiobuttonNLSQ', 4)

                  case (3)
                    ifehl = 1           !  29.1.2024  prevents from crashing, if dialog was still empty

                  case (5)
                    ifehl = 1           !  29.1.2024  prevents from crashing, if dialog was still empty

                  case (6)
                    knumEGr = knumold
                    call WDSetComboboxAct('comboboxtextKnumegr', knumEGr)

                  case (65)
                    actual_plot = ' '
                    plot_ellipse = .false.
                    plot_confidoid = .false.

                  case (67)
                    ifehl = 1

                  case (70)
                    ifehl = 1
                    bat_serial = .false.        ! 20.9.2024 GK

                  case (71)

                  case (72)
                    ifehl = 1

                  case (73)
                    ifehl = 1
                    batf = .false.        ! 20.9.2024 GK

                  case (74)
                    do i=1,nvisib
                        call WDPutEntryDouble(enti(i),entival(i),'(es14.7)')
                    end do
                    saveP = SaveP_sv
                    call FieldUpdate()

                  case (76)
                        ! decay chain 11.12.2024
                    if(trim(widgetlabel) == 'Abbrechen') ifehl = 1

                  case (77)
                    ifehl = 1


                end select    ! ioption

            end select      ! widgetlabel
            !----------------------

            ! Actions required by LoadselDiag are handled in this part for dialog widgets
            ! identified by their name (idstring).
            ! A goto 1010 at the end of each block means to restart the dialog loop again

            select case (trim(idstring))

              case ('entryOptKalpha')

              case ('DOptionsLoadVals')

                !                 if(prout) write(66,*) ' Loadsel:  at DOptionsLoadVals arrived!'
                if(prout)  then
                    write(log_str, '(*(g0))') ' Loadsel:  at DOptionsLoadVals arrived!'
                    call logger(66, log_str)
                end if
                if(nijh > 0) then
                    do i=1, 4
                        if(fijh(i) == 1) then
                            call WDGetEntryDouble('entryOptKalpha',kalpha)
                            alpha =  ONE - pnorm(kalpha)
                            call WDPutEntryDouble('entryOptAlpha',alpha,'(f10.8)')
                            call gtk_widget_set_sensitive(idpt('entryOptAlpha'), 1_c_int)
                        elseif(fijh(i) == 2) then
                            call WDGetEntryDouble('entryOptKbeta',kbeta)
                            beta =  ONE - pnorm(kbeta)
                            call WDPutEntryDouble('entryOptBeta',beta,'(f10.8)')
                            call gtk_widget_set_sensitive(idpt('entryOptBeta'), 1_c_int)
                        elseif(fijh(i) == 3) then
                            call WDGetEntryDouble('entryOptAlpha',alpha)
                            kalpha =  qnorm(ONE - alpha)
                            call WDPutEntryDouble('entryOptKalpha',kalpha,'(f10.8)')
                            call gtk_widget_set_sensitive(idpt('entryOptKalpha'), 1_c_int)
                        elseif(fijh(i) == 4) then
                            call WDGetEntryDouble('entryOptBeta',beta)
                            kbeta =  qnorm(ONE - beta)
                            call WDPutEntryDouble('entryOptKbeta',kbeta,'(f10.8)')
                            call gtk_widget_set_sensitive(idpt('entryOptKbeta'), 1_c_int)
                        end if
                    end do
                end if
                nijh = 0
                fijh(:) = 0
                call gtk_widget_set_sensitive(idpt('DOptionsLoadVals'), 0_c_int)   ! 13.4.2023
                call gtk_widget_set_sensitive(idpt('DOptionsOK'), 1_c_int)   ! 13.4.2023
                goto 1010

              case ('CalcCountRates')
                call NetRatesCalc(ikenn)
                goto 1010

              case ('DoKalibFit')
                Call WDGetComboboxAct('comboboxDKPgrad', kal_Polgrad)
                kal_polgrad = kal_polgrad - 1
                maKB = kal_polgrad + 1
                call WDGetCheckButton('DKcheckUfit',k)
                use_UfitKal = .false.
                if(k == 1) use_UfitKal = .true.
                call WDGetCheckButton('DKcheckWTLS',k)
                use_WTLS_Kal = .false.
                if(k == 1) use_WTLS_Kal = .true.  ! 7.8.202
                write(0,*) 'Loadsel: do kalibFit Ende; k of use_WTLS_kal=',int(k,2)
                kxy = 40
                call WTreeViewGetDoubleArray('treeview7',2,kxy,xkalib)
                call WTreeViewGetDoubleArray('treeview7',3,kxy,uxkalib)
                call WTreeViewGetDoubleArray('treeview7',4,kxy,ykalib)
                call WTreeViewGetDoubleArray('treeview7',5,kxy,uykalib)

                do i=1,kxy
                    IF(abs(xkalib(i)-missingval) < EPS1MIN .and. abs(ykalib(i)-missingval)< EPS1MIN) THEN
                        nkalpts = MAX(0,i-1)
                        EXIT
                    END IF
                end do
                !do i=1,nkalpts
                !  write(66,*) 'Loadsel Kalfit: i=',i,sngl(xkalib(i)),sngl(uxkalib(i)),sngl(ykalib(i)),sngl(uykalib(i))
                !end do
                call XKalfit()
                ! write(0,*) 'Loadsel: do kalibFit Ende;  use_WTLS_kal=',use_WTLS_kal
                goto 1010

              case ('buttonFBSave')
                do i=1,Settings%nprops
                    if(trim(Settings%sproperty(i)) == 'gtk-font-name') then
                        Settings%sProperty_val(i) = trim(fontname)
                        exit
                    end if
                end do
                call GtkSettingsIO(.false., ifehl)
                ! no goto 1010 !

              case ('buttonCBSave')
                do i=1,Settings%nprops
                    if(trim(Settings%sproperty(i)) == 'gtk-color-scheme') then
                        if(kcolortype == 1) then
                            i1 = index(Settings%sproperty_val(i),'bg_color:')
                            if(i1 > 0) then
                                Settings%sproperty_val(i) = trim(Settings%sproperty_val(i)(1:i1+8)) &
                                    // trim(colorname)       &
                                    // trim(Settings%sproperty_val(i)(i1+9+7:))
                                if(prout)  then
                                    write(log_str, '(*(g0))') 'new gtk-color-scheme value-string: ',  &
                                    trim(Settings%sproperty_val(i))
                                    call logger(66, log_str)
                                end if
                            end if
                        elseif(kcolortype == 2) then
                            i1 = index(Settings%sproperty_val(i),'selected_bg_color:')
                            if(i1 > 0) then
                                Settings%sproperty_val(i) = trim(Settings%sproperty_val(i)(1:i1+17)) &
                                    // trim(colorname)       &
                                    // trim(Settings%sproperty_val(i)(i1+18+7:))
                                if(prout)  then
                                    write(log_str, '(*(g0))') 'new gtk-color-scheme value-string: ',  &
                                    trim(Settings%sproperty_val(i))
                                    call logger(66, log_str)
                                end if
                            end if
                        end if
                        exit
                    end if
                end do
                call GtkSettingsIO(.false., ifehl)
                ! no goto 1010 !

              case ('colorselectionOK')       ! no longer usable

              case ('doELIplot')
                if(.not.Confidoid_activated) then
                    call WDPutLabelString('doELIplot', T("plot confidence ellipse:"))

                    Confidoid_activated = .true.
                end if
                call WDGetCheckButton('checkbuttonELI_EG1',igsel(1))
                call WDGetCheckButton('checkbuttonELI_EG2',igsel(2))
                call WDGetCheckButton('checkbuttonELI_EG3',igsel(3))
                call WDGetCheckButton('checkbuttonRS',eliRS)
                call WDGetComboboxAct('comboboxGrELI',iopt_copygr)
                k = 0
                do i=1,3
                    if(igsel(i) == 1) k = k + 1
                end do
                if(k > 1) then
                    call Confidoid()
                end if
                goto 1010

              case ('CopyGrELI')     ! Copy in ELI
                call WDGetComboboxAct('comboboxGrELI',iopt_copygr)
                call PrintPlot()
                if(scalable) then
                    call WDSetComboboxAct('comboboxGrELI',1)
                    iopt_copygr = 1
                    scalable = .false.
                    call WDGetCheckButton('checkbuttonELI_EG1',igsel(1))
                    call WDGetCheckButton('checkbuttonELI_EG2',igsel(2))
                    call WDGetCheckButton('checkbuttonELI_EG3',igsel(3))
                    call WDGetCheckButton('checkbuttonRS',eliRS)
                    call WDGetComboboxAct('comboboxGrELI',iopt_copygr)
                    k = 0
                    do i=1,3
                        if(igsel(i) == 1) k = k + 1
                    end do
                    if(k > 1) call Confidoid()
                end if
                call pending_events
                goto 1010

              case ('comboboxLangg__X')

              case ('FillDecColumn')
                call WDGetComboboxAct('comboboxtextdcol',k)
                call WDGetEntryDouble('entryDecaycolVal',xx)
                if(k == 0.or. abs(xx) < EPS1MIN) goto 1010
                kcolls = (/ 3,4,7,8 /)      ! column numbers in 'treeview5'
                do i=1,numd
                    call WTreeViewPutDoubleCell('treeview5', kcolls(k), i, xx)
                end do
                SaveP = .true.
                call FieldUpdate()
                goto 1010

              case ('MDCalcMean')
                call WDGetComboboxAct('combobox_MDselVar',k_datvar)
                if(k_datvar == 0) goto 1000
                call WDGetComboboxAct('combobox_MDtyp',k_MDtyp(k_datvar))

                call GetxdataMD_fromTree()
                goto 1000

              case ('OpenFileSE')
                FileTyp = 'D'
                hinweis = T("Filename for input data for serial evaluations:")

                call FOpen(ifehl, .false., Hinweis)
                do i=len_trim(serial_csvinput),1,-1
                    if(serial_csvinput(i:i) == dir_sep) then
                        i1 = i+1
                        exit
                    end if
                end do
                call WDPutEntryString('FilenameSE',serial_csvinput(i1:))
                goto 1010

              case ('HelpFX')
                ! write(66,*) 'HelpFX:    buthelp=',trim(buthelp)
                call FindItemS(trim(buthelp), kk)
                call DisplayHelp(0, idstr=trim(buthelp))
                goto 1010

              case ('DCloadData')
                call DCReadGrid
                goto 1010

              case ('DCgenerateXi')
                call DecCh()
                       ! write(66,*) 'nachDecCH()'
                goto 1010

              case ('comboboxtextDCNCH')
                call WDGetComboboxAct('comboboxtextDCNCH', DCchannels)
                ! make some tree columns insensitive, but how?
                !do i=1,DCchannels
                !  call
                !end do
              case('ButtonDCTransCh')
                 call WDGetComboboxAct('ComboboxDCchains',k)
                    write(66,*) 'ComboboxDCchains: selected k= ',k
                 ChainSelected = k
                 call DCPrepareTable(k)
                 goto 1010

              case default

            end select    ! idstring

          case ('GtkRadioButton')

          case ('GtkComboBox')

            select case (trim(IDstring))
              case ('combobox_MDselVar')
                call WDGetComboboxAct('combobox_MDselVar',k_datvar)
                call WDSetComboboxAct('combobox_MDtyp',k_MDtyp(k_datvar))
                nv =nvalsMD(k_datvar)
                if(nv > 0) then
                    nj = ixdanf(k_datvar)
                    if(allocated(xdat)) deallocate(xdat)
                    allocate(xdat(nv))
                    xdat = xdataMD(nj:nj+nv-1)
                    call WTreeViewPutDoubleArray('treeview8', 2, nv, xdat)
                    if(nv > 1) then
                        call MDcalc(k_datvar)
                        if(ifehl == 1) then
                            if(allocated(xdat)) deallocate(xdat)
                            goto 1010
                        end if
                        call WDPutEntryDouble('TEmeanMD',meanMD(k_datvar),'(es12.6)')
                        call WDPutEntryDouble('TEsdMD',umeanMD(k_datvar),'(es12.6)')
                    end if
                else
                    do i=1,200
                        call WTreeViewPutDoublecell('treeview8', 2, i, missingval )
                    end do
                end if
                if(allocated(xdat)) deallocate(xdat)
                goto 1010

              case ('comboboxLangg')
                call WDGetComboboxAct('comboboxLangg',k1lang)
                if(k1lang == 1) then
                    langg = 'de'

                else if (k1lang == 2) then
                    langg = 'en'

                else if (k1lang == 3 ) then
                    langg = 'fr'
                end if

                call set_language(langg)
                call TranslateUR()
                sDecimalPoint = T('.')
                call pending_events()
                goto 1010

              case ('comboboxSymbchg')
                call WDGetComboboxAct('comboboxSymbchg', kopt)
                call WDPutEntryString('entrySymbchg',Symbole(kopt)%s)
                call gtk_widget_grab_focus(idpt('entrySymbchg'))
                goto 1010

              case ('comboboxA1')
                call WDGetComboboxAct('comboboxA1',k)
                if(k > 1 .and. knumEGr == 1) then
                    call CharModStr(str1,500)
                    str1 = T("An option other than 'fit' in this case not allowed!")

                    call MessageShow(trim(str1), GTK_BUTTONS_OK, "LDN_1948:", resp,mtype=GTK_MESSAGE_WARNING)
                    call WDSetComboboxAct('comboboxA1',1)
                    if(.not.savep_sv) then
                        saveP = .false.
                        call WrStatusbar(3, ' ')
                    endif
                end if
                goto 1010

            end select
        end select   ! widget-label
!-----------------------------------------------------------------------------
        select case (trim(signal))

            ! Actions required by LoadselDiag are handled in this part for dialog widgets
            ! identified by their signal ('group-changed', 'toggled', 'changed', 'file-set',
            ! 'close' ).
            ! A goto 1010 at the end of each block means to restart the dialog loop again
            ! With the signal 'close', LoadselDiag will terminate the interatcion and returns
            ! after having hided the dialog.

          case ('group-changed')

          case ('toggled')
            select case (ioption)
              case (1)
                !case ('check_contrastmode')
                if(ioption == 1) then
                    call WDGetCheckButton('check_contrastmode', i)
                    if(i == 0) then
                        call set_color_theme('default')
                    else
                        call set_color_theme('contrast')
                    end if

                    call SetColors()
                    do i=1,10
                        if(i == 9) cycle
                        write(treename,'(A,i1)') 'treeview',i
                        if(i == 10) treename = 'treeviewELI'
                        if(i == 3) nvals = ncov + 15
                        if(i <= 4 .and. i /= 3) nvals = ngrs + 10
                        if(i > 4 .and. i <= 6) nvals = numd + 10
                        if(i == 7) nvals = nkalpts + 10
                        if(i == 8) nvals = nparts + 5
                        if(i == 10) nvals = 3
                        do k=1,nvals
                            call WTreeViewSetColorRow(trim(treename),k, get_color_string('table_bg'))
                        end do
                    end do
                    goto 1000
                end if

              case (3)       ! 24.7.2023
                call WDGetCheckButton('checkAbsTime',i)
                write(log_str, '(*(g0))') 'LDN:    at checkAbsTime arrived:  i=',int(i,2)
                call logger(66, log_str)
                if(i == 0) then
                    use_absTimeStart = .false.
                    ! call gtk_widget_set_sensitive(idpt('entrySeparation'),0_c_int)
                    call WDSetCheckButton('comboboxtextbase',1)
                    ! call gtk_widget_set_sensitive(idpt('comboboxtextbase'),0_c_int)
                    call gtk_widget_set_sensitive(idpt('entrySeparation'),0_c_int)
                    call gtk_widget_set_visible(idpt('entrySeparation'),0_c_int)
                else
                    use_absTimeStart = .true.
                    call gtk_widget_set_sensitive(idpt('entrySeparation'),1_c_int)
                    call gtk_widget_set_sensitive(idpt('comboboxtextbase'),1_c_int)
                    call gtk_widget_set_visible(idpt('entrySeparation'),1_c_int)
                end if
                write(log_str, '(*(g0))') 'gtk_widget_get_visible(idpt(''entrySeparation''))=',gtk_widget_get_visible(idpt('entrySeparation'))
                call logger(66, log_str)

                if(use_absTimeStart) then
                    call WDPutTreeViewColumnLabel('treeview5', 2, T('Start date')//char(13)//T('(gross)'))
                else
                    call WDPutTreeViewColumnLabel('treeview5', 2, T('StartDiff (s)')//char(13)//T('(gross)'))
                end if
                call pending_events()
                goto 1010

              case (8)
                Call WDGetComboboxAct('comboboxDKPgrad', kal_Polgrad)
                kal_polgrad = kal_polgrad - 1
                maKB = kal_polgrad + 1
                call WDGetCheckButton('DKcheckUfit',k)
                use_UfitKal = .false.
                if(k == 1) use_UfitKal = .true.
                call WDGetCheckButton('DKcheckWTLS',k)
                use_WTLS_Kal = .false.
                if(k == 1) use_WTLS_Kal = .true.  ! 7.8.202
                write(0,*) 'Loadsel: do kalibFit Ende; k of use_WTLS_kal=',int(k,2)
                goto 1010

              case (63)
                ! Colorchoser: Farbentyp im Chooser wechseln:
                call WDGetSelRadio('radiobutton_bg_color', kcolortype)
                if(prout)  then
                    write(log_str, '(*(g0))') 'kcolortype=',kcolortype
                    call logger(66, log_str)
                end if
                do i=1, Settings%nprops
                    if(trim(Settings%sproperty(i)) == 'gtk-color-scheme') then
                        if(kcolortype == 1) then
                            i1 = index(Settings%sproperty_val(i),'bg_color:')
                            colorname = Settings%sproperty_val(i)(i1+9:i1+9+7)
                        elseif(kcolortype == 2) then
                            i1 = index(Settings%sproperty_val(i),'selected_bg_color:')
                            colorname = Settings%sproperty_val(i)(i1+18:i1+18+7)
                        end if

                        if(prout)  then
                            write(log_str, '(*(g0))') 'colorname=',colorname
                            call logger(66, log_str)
                        end if
                        if(i1 > 0) then
                            read(colorname,'(1x,z2.2,z2.2,z2.2)') i1,i2,i3
                            URcolor%red = dble(i1)/dble(256)
                            URcolor%green = dble(i2)/dble(256)
                            URcolor%blue = dble(i3)/dble(256)
                            URcolor%alpha = 1.d0
                            ! pURcolor => URcolor
                            ! pcolor = c_loc(pURcolor)
                            call gtk_color_chooser_set_rgba(idpt('colorbutton1'), c_loc(URcolor))
                        end if
                        exit
                    end if
                end do
                goto 1010

              case (65,68)
                ! 65:ConfidEllipse;  68: Copy1GrELI;
                call WDGetCheckButton('checkbuttonELI_EG1',igsel(1))
                call WDGetCheckButton('checkbuttonELI_EG2',igsel(2))
                call WDGetCheckButton('checkbuttonELI_EG3',igsel(3))
                call WDGetCheckButton('checkbuttonRS',eliRS)
                call WDGetComboboxAct('comboboxGrELI',iopt_copygr)
                k = 0
                do i=1,3
                    if(igsel(i) == 1) k = k + 1
                end do
                write(0,*) 'LSD: ioption=',ioption,' k=',k,'  eliRS=',eliRS
                if(k > 1) then
                    call Confidoid()
                end if
                goto 1010

              case (70)
                if(trim(idstring) == 'CheckMCSE') then
                    write(log_str, '(*(g0))') 'toggled: idstring=',trim(idstring)
                    call logger(66, log_str)
                    call WDGetCheckButton('CheckMCSE', i1)
                    bat_mc = .false.
                    if(i1 == 1) then
                        call gtk_widget_set_sensitive(idpt('EntryMCnumSE'), 1_c_int)
                        call gtk_widget_set_sensitive(idpt('EntryRunsMCSE'), 1_c_int)
                        bat_mc = .true.
                        if(kcmx > 0) then
                            call WDPutEntryInt('EntryMCnumSE', kcmxMC)
                        else
                            call WDPutEntryInt('EntryMCnumSE', 100000)
                        end if
                        if(kcrun > 0) then
                            call WDPutEntryInt('EntryRunsMCSE', kcrunMC)
                        else
                            call WDPutEntryInt('EntryRunsMCSE', 1)
                        end if
                        call gtk_widget_grab_focus(idpt('EntryRunsMCSE'))
                        call gtk_widget_grab_focus(idpt('EntryMCnumSE'))
                    else
                        call WDPutEntryString('EntryMCnumSE','')
                        call WDPutEntryString('EntryRunsMCSE','')
                    end if
                end if
                goto 1010

              case (73)
                if(trim(idstring) == 'CheckMCBEV') then
                    write(log_str, '(*(g0))') 'toggled: idstring=',trim(idstring)
                    call logger(66, log_str)
                    call WDGetCheckButton('CheckMCBEV', i1)
                    bat_mc = .false.
                    if(i1 == 1) then
                        call gtk_widget_set_sensitive(idpt('EntryMCnumBEV'), 1_c_int)
                        call gtk_widget_set_sensitive(idpt('EntryRunsMCBEV'), 1_c_int)
                        bat_mc = .true.
                        if(kcmx > 0) then
                            call WDPutEntryInt('EntryMCnumBEV', kcmxMC)
                        else
                            call WDPutEntryInt('EntryMCnumBEV', 100000)
                        end if
                        if(kcrun > 0) then
                            call WDPutEntryInt('EntryRunsMCBEV', kcrunMC)
                        else
                            call WDPutEntryInt('EntryRunsMCBEV', 1)
                        end if
                        call gtk_widget_grab_focus(idpt('EntryRunsMCBEV'))
                        call gtk_widget_grab_focus(idpt('EntryMCnumBEV'))
                    else
                        call WDPutEntryString('EntryMCnumBEV','')
                        call WDPutEntryString('EntryRunsMCBEV','')
                    end if
                end if
                goto 1010

                case (76)
                         write(66,*) 'erreicht: idstring=',idstring
                  call WDGetCheckButton('DCcheckVorLam',iv)
                  if(iv == 1) then
                    call WDPutTreeViewColumnLabel('treeview9', 3, 'Lambda'//char(13)//'Symbol')
                  else
                    call WDPutTreeViewColumnLabel('treeview9', 3, 'T12'//char(13)//'Symbol')
                  endif
                  goto 1010


            end select

          case ('changed')

            select case (ioption)

              case (1)       ! 13.4.2023  am 24.9.2024 hierher verschoben

                ijh = 0
                if(trim(idstring) == 'entryOptKalpha') ijh = 1
                if(trim(idstring) == 'entryOptKbeta')  ijh = 2
                if(trim(idstring) == 'entryOptAlpha')  ijh = 3
                if(trim(idstring) == 'entryOptBeta')   ijh = 4

                if(ijh > 0) then
                    if(ijh == 1) call gtk_widget_set_sensitive(idpt('entryOptAlpha'), 0_c_int)
                    if(ijh == 2) call gtk_widget_set_sensitive(idpt('entryOptBeta'), 0_c_int)
                    if(ijh == 3) call gtk_widget_set_sensitive(idpt('entryOptKalpha'), 0_c_int)
                    if(ijh == 4) call gtk_widget_set_sensitive(idpt('entryOptKbeta'), 0_c_int)
                    call gtk_widget_set_sensitive(idpt('DOptionsLoadVals'), 1_c_int)
                    call gtk_widget_set_sensitive(idpt('DOptionsOK'), 0_c_int)   ! 13.4.2023
                    nijh = ijh
                    fijh(nijh) = ijh
                end if
                goto 1010

              case (8)

              case (69)
                goto 1010

              case (75)
                call WDGetComboboxAct('comboboxtextInfoFX',ifx)
                if(ifx > 1) then           ! condition introduced 25.2.2024
                    ! write(66,*) 'InfoFX field ix=',ifx
                    call InfoFX_Select(ifx,buthelp)
                endif
                goto 1010

            end select

          case ('insert-text','editing-done')

            select case (ioption)


            end select

          case ('file-set')

            select case (ioption)

              case (70,73)
                ! case ('ChooserButton2SE')
                if(bat_serial) cp2 = gtk_file_chooser_get_filename(idpt('ChooserButton2SE'))
                if(batf) cp2 = gtk_file_chooser_get_filename(idpt('ChooserButton2BEV'))
                if(c_associated(cp2))then
                    if(batf) then
                        call c_f_string(cp2, batf_file)
                        ! this part restricted to ioption=73; does not work for ioption=70
                        close (113)
                        open(113, file=trim(batf_file),status='old',iostat=ios,iomsg=text)
                        i = 0
                        do
                            text = ''
                            read(113,'(A)',iostat=ios) text
                            if(ios /= 0) then
                                exit
                            end if
                            i = i + 1
                        end do
                        if(ios /= 0 .and. ios /= -1) then
                            ifehl = 1
                            call ErrOpenFile(trim(batf_file),trim(text))
                            write(log_str, '(*(g0))') 'LDN_2051: open(113):  ios=',ios
                            call logger(66, log_str)
                            goto 1010
                        end if
                    end if
                    if(bat_serial) then
                        call c_f_string(cp2, serial_csvinput)
                        call logger(66, 'serial_csvinput =' // trim(serial_csvinput))

                        open(112, file=flfu(trim(serial_csvinput)), status='old', iostat=ios, iomsg=text)

                        if(ios /= 0) then
                            ifehl = 1
                            call ErrOpenFile(trim(serial_csvinput), trim(text))
                            goto 1010
                        end if
                        i = 0
                        do
                            text = ''
                            read(112,'(A)',iostat=ios) text
                            if(ios /= 0) exit
                            if(i == 0) then
                                read(112,'(A)',iostat=ios) text
                                if(ios /= 0) exit
                            end if
                            if(len_trim(text) == 0) cycle
                            i = i + 1
                        end do
                        close (112)
                    end if
                end if
                if(bat_serial) then
                    call WDPutEntryInt('entryFromSE',1)
                    call WDPutEntryInt('entryToSE',i)
                    call gtk_widget_set_sensitive(IDPT('entryFromSE'),1_c_int)
                    call gtk_widget_set_sensitive(idpt('entryToSE'),1_c_int)
                elseif(batf) then
                    call WDPutEntryInt('entryFromBEV',1)
                    call WDPutEntryInt('entryToBEV',i)
                    call gtk_widget_set_sensitive(IDPT('entryFromBEV'),1_c_int)
                    call gtk_widget_set_sensitive(idpt('entryToBEV'),1_c_int)
                end if
                goto 1010

            end select


          case ('close')
            dialog_on = .false.
            call gtk_widget_set_sensitive(idpt('menubar1'), 1_c_int)
            goto 9000

          case default

        end select

        call gtk_widget_hide(dialog)

!---------------------
9000    continue
        dialog_on = .false.
        call gtk_widget_set_sensitive(idpt('menubar1'), 1_c_int)

        if(allocated(str1)) deallocate(str1)
        if(allocated(FTF)) deallocate(FTF)
        if(allocated(rdummy)) deallocate(rdummy)
        if(allocated(xdat)) deallocate(xdat)
        if(allocated(cdummy)) deallocate(cdummy)

        call gtk_widget_hide(dialog)
        call pending_events()

    end subroutine Loadsel_diag_new

!#########################################################################################

    subroutine SetLabel3Terms

        ! sets the label text for the label 'label3terms' in the dialog for the model
        ! of fitting a decay curve

        !   Copyright (C) 2014-2023  Günter Kanisch

        use UR_Linft,       only: defineallxt

        use Rout,           only: WDPutEntryString,WDGetCheckButton
        use translation_module, only: T => get_translation

        implicit none
        integer             :: i
        character(len=120)  :: str1

        call WDGetCheckButton('checkbuttonAllm',i)
        defineallxt = .false.
        if(i == 1) defineallxt = .true.
        if(.not. defineallxt) then
            str1 = T("Definition of functions X1 to Xn (n=nchs*3):")
            call WDPutEntryString('label3terms', trim(str1) // "   " // &
                                  T("Sequence: like SQL: 'ORDER BY countChannel, Xfunc-#')"))
        else
            str1 = T("Definition of functions X1 to Xn (n=nchs*measuremts*3):")
            call WDPutEntryString('label3terms', trim(str1) // "   " // &
                                  T("Sequence: like SQL: 'ORDER BY countChannel, measurem-#, Xfunc-#')"))
        end if


    end subroutine SetLabel3Terms

!#######################################################################

    subroutine NetRatesCalc(ikenn)

        ! After the decay curve data have been read from treeview5 of the
        ! correpondent dialog (from the non-coloured columns), the net count
        ! rates and associated standard uncertainties are calculated here,
        ! which are then transferred to treeview5:
        !     columns 5, 6: gross count rates and standard uncertainties
        !     columns 9, 10: background count rates and standard uncertainties
        !     columns 11, 12: net count rates and statndard uncertainties
        !
        ! 11.3.2022: re-arranged with regard to the time-related symbol fact
        !            (60.0 in case of minutes, 1.0 in case of seconds)

        !   Copyright (C) 2020-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,         only: c_ptr
        USE ur_general_globals,    only: SaveP
        USE UR_Gleich_globals,     only: kpoint,missingval,Messwert,Stdunc,kpoint,SDWert
        USE UR_Linft,              only: k_rbl,ndatmax,numd,linfzbase,tmedian,dmesszeit,dbimpulse, &
                                         sdbzrate,d0messzeit,d0impulse,d0zrate,sd0zrate,cstartzeit, &
                                         dbzrate,sd0zrate_CP,d0zrate_CP,dbzrate_CP,sdbzrate_CP, &
                                         sd0zrateSV,dnetrate,SDnetrate,dnetrate_CP,SDnetrate_CP

        use Rout,                  only: WDPutEntryString,WDGetComboboxAct,WTreeViewGetStrArray,    &
                                         WTreeViewGetDoubleArray,WTreeViewGetDoubleCell,           &
                                         WTreeViewPutDoubleCell
        use Top,                   only: FieldUpdate, wrstatusbar
        use file_io,               only: logger
        use num1,                  only: median
        use translation_module,    only: T => get_translation

        implicit none

        integer   ,intent(out)    :: ikenn

        integer            :: kxy,i,kksv
        character(len=512)           :: log_str
        real(rn)           :: zfact,mw_rbl,umw_rbl,dummy
!------------------------------------------------------------------

        ikenn = 0

        kxy = ndatmax
        call WDGetComboboxAct('comboboxtextbase',linfzbase)
        ! write(66,*) 'NetRatesCalc:  linfzbase=',linfzbase

        call WTreeViewGetStrArray('treeview5', 2, kxy, CStartzeit)
        call WTreeViewGetDoubleArray('treeview5', 3, kxy, dmesszeit)
        call WTreeViewGetDoubleArray('treeview5', 4, kxy, dbimpulse)
        call WTreeViewGetDoubleArray('treeview5', 5, kxy, dbzrate)
        call WTreeViewGetDoubleArray('treeview5', 6, kxy, sdbzrate)
        call WTreeViewGetDoubleArray('treeview5', 7, kxy, d0messzeit)
        call WTreeViewGetDoubleArray('treeview5', 8, kxy, d0impulse)
        call WTreeViewGetDoubleArray('treeview5', 9, kxy, d0zrate)
        call WTreeViewGetDoubleArray('treeview5', 10, kxy, sd0zrate)

        do i=1,kxy
            if(abs(dmesszeit(i)-missingval) < EPS1MIN .or. abs(dmesszeit(i)) < EPS1MIN) then
                numd = MAX(0,i-1)
                ! write(66,*) 'LDN_2396: numd determined:',numd
                EXIT
            END IF
        end do
        zfact = ONE
        IF(linfzbase == 2) zfact = 60.0_rn

        kksv = 0
        do i=1,numd       ! gross count rates:
            IF(abs(dbimpulse(i)-missingval) < EPS1MIN) THEN
                ikenn = 1
                cycle
            end if
            IF(abs(dmesszeit(i)-missingval) < EPS1MIN) THEN
                ikenn = 1
                cycle
            end if

            dmesszeit(i) = dmesszeit(i) * zfact
            dbzrate(i)  = dbimpulse(i) / dmesszeit(i)
            sdbzrate(i) = SQRT(dbzrate(i)/dmesszeit(i))
            call WTreeViewPutDoubleCell('treeview5', 5, i, dbzrate(i)*zfact)
            call WTreeViewPutDoubleCell('treeview5', 6, i, sdbzrate(i)*zfact)

            !call WTreeViewGetDoubleCell('treeview5', 5, i, dbzrate(i))
            !call WTreeViewGetDoubleCell('treeview5', 6, i, sdbzrate(i))

            if(abs(dbzrate_CP(1) - missingval) > 100.*EPS1MIN) then

                dummy = ABS(dbzrate(i)*zfact - dbzrate_CP(i)) / (2.E-8_rn*ABS(dbzrate(i)*zfact))
                IF(dummy > ONE) then
                    kksv = kksv + 1
                    write(log_str, '(a,i0,a,es9.1)') 'NetRatesCalc: deviation for dbzrate(i),i=',i,': dummy=',dummy
                    call logger(66, log_str)
                END IF
                dummy = ABS(sdbzrate(i)*zfact - sdbzrate_CP(i)) / (2.E-8_rn*ABS(sdbzrate(i)*zfact))
                IF(dummy > ONE ) then
                    kksv = kksv + 1
                    write(log_str, '(a,i0,a,es9.1)') 'NetRatesCalc: deviation for sdbzrate(i),i=',i,': dummy=',dummy
                    call logger(66, log_str)
                end if
            end if
            !  write(66,'(a,i0,2(a,es12.5))') 'i=',i,' dbzrate=',dbzrate(i),' sdbzrate=',sdbzrate(i)
            !dbzrate(i) = dbzrate(i)/zfact
            !sdbzrate(i) = sdbzrate(i)/zfact
        end do

        kksv = 0
        do i=1,numd       ! background count rates:
            IF(abs(d0impulse(i)-missingval) < EPS1MIN) THEN
                ikenn = 1
                CYCLE
            end if
            IF(abs(d0messzeit(i)-missingval) < EPS1MIN) then
                ikenn = 1
                CYCLE
            end if
            d0messzeit(i) = d0messzeit(i) * zfact
            d0zrate(i)  = d0impulse(i) / d0messzeit(i)
            sd0zrate(i) = SQRT(d0zrate(i)/d0messzeit(i))

            call WTreeViewPutDoubleCell('treeview5', 9, i, d0zrate(i)*zfact)
            call WTreeViewPutDoubleCell('treeview5', 10, i, sd0zrate(i)*zfact)
            !  read it again:
            !call WTreeViewGetDoubleCell('treeview5', 9, i, d0zrate(i))
            !call WTreeViewGetDoubleCell('treeview5', 10, i, sd0zrate(i))

            if(abs(d0zrate_CP(1) - missingval) > 100.*EPS1MIN) then
                dummy = ABS(d0zrate(i)*zfact - d0zrate_CP(i)) / (2.E-8_rn*ABS(d0zrate(i)*zfact))
                IF(dummy > ONE) then
                    kksv = kksv + 1
                    write(log_str, '(a,i0,a,es9.1,2(a,es11.4))') 'NetRatesCalc-A: deviation for d0zrate(i),i=',i,': dummy=',dummy, &
                        ' d0zrate(i)=',d0zrate(i),' d0zrate_CP(i)/zfact=',d0zrate_CP(i)/zfact
                    call logger(66, log_str)
                end if
                dummy = ABS(sd0zrate(i)*zfact - sd0zrate_CP(i)) / (2.E-8_rn*ABS(sd0zrate(i)*zfact))
                IF(dummy > ONE ) then
                    kksv = kksv + 1
                    write(log_str, '(a,i0,a,es9.1,2(a,es11.4))') 'NetRatesCalc-A: deviation for sd0zrate(i),i=',i,': dummy=',dummy, &
                        ' sd0zrate(i)=',sd0zrate(i),' sd0zrate_CP(i)/zfact=',sd0zrate_CP(i)/zfact
                    call logger(66, log_str)
                end if
            end if
            !d0zrate(i) = d0zrate(i)/zfact
            !sd0zrate(i) = sd0zrate(i)/zfact
            sd0zrateSV(i) = sd0zrate(i)
            !    write(66,'(a,i0,2(a,es12.5))') 'i=',i,' d0zrate=',d0zrate(i),' sd0zrate=',sd0zrate(i)

        end do
        tmedian = median(dmesszeit,numd)
        write(log_str, '(*(g0))') 'tmedian=',sngl(tmedian)
        call logger(66, log_str)

        mw_rbl = ZERO
        umw_rbl = ZERO
        if(k_rbl > 0) then
            mw_rbl = Messwert(kpoint(k_rbl))
            umw_rbl = StdUnc(kpoint(k_rbl))
            if(abs(umw_rbl - missingval) <= EPS1MIN .and. SDwert(kpoint(k_rbl)) > ZERO) then
                umw_rbl = SDwert(kpoint(k_rbl))
                write(log_str, '(*(g0))') 'k_rbl=',int(k_rbl,2),'  mw_rbl=',sngl(mw_rbl),'   umw_rbl=',sngl(umw_rbl)
                call logger(66, log_str)
            end if
        end if

        if(allocated(dnetrate)) deallocate(dnetrate,sdnetrate)
        allocate(dnetrate(numd),sdnetrate(numd))

        do i=1,numd       ! net count rates:
            dnetrate(i) = dbzrate(i) - d0zrate(i) - mw_rbl
            sdnetrate(i) = sdbzrate(i)**TWO + sd0zrate(i)**TWO
            if(k_rbl > 0) then
                IF(abs(umw_rbl-missingval) > EPS1MIN) sdnetrate(i) = sdnetrate(i) + umw_rbl**TWO
            end if
            sdnetrate(i) = SQRT(sdnetrate(i))

            call WTreeViewPutDoubleCell('treeview5', 11, i, dnetrate(i)*zfact)
            call WTreeViewPutDoubleCell('treeview5', 12, i, sdnetrate(i)*zfact)
            !  read it again:
            !call WTreeViewGetDoubleCell('treeview5', 11, i, dnetrate(i))
            !call WTreeViewGetDoubleCell('treeview5', 12, i, sdnetrate(i))

            if(abs(dnetrate_CP(1)-missingval) > 100.*EPS1MIN) then
                dummy = ABS(dnetrate(i)*zfact - dnetrate_CP(i)) / (2.E-8_rn*ABS(dnetrate(i)*zfact))
                IF(dummy > ONE) then
                    kksv = kksv + 1
                    write(log_str, '(a,i0,a,es9.1,2(a,es20.10))') 'NetRatesCalc-B: deviation for dnetrate(i),i=',i, &
                        ': dummy=',dummy,' ',dnetrate(i),' ',dnetrate_CP(i)
                    call logger(66, log_str)
                end if
                ! dummy =  ABS(sdnetrate(i) - sdnetrate_CP(i)) / (2.E-8_rn*ABS(sdnetrate(i)))
                dummy =  ABS(sdnetrate(i)*zfact - sdnetrate_CP(i)) / (2.E-8_rn*ABS(sdnetrate(i)*zfact))
                IF(dummy > ONE ) then
                    kksv = kksv + 1
                    write(log_str, '(a,i0,a,es9.1,2(a,es11.4))') 'NetRatesCalc-B: deviation for sdnetrate(i),i=',i,': dummy=',dummy, &
                        ' sdnetrate(i)=',sdnetrate(i),' sdnetrate_CP(i)/zfact=',sdnetrate_CP(i)/zfact
                    call logger(66, log_str)
                end if
            end if
            !dnetrate(i) = dnetrate(i)/zfact
            !sdnetrate(i) = sdnetrate(i)/zfact
            !  write(66,'(a,i0,2(a,es12.5))') 'i=',i,' dnetrate=',dnetrate(i),' sdnetrate=',sdnetrate(i)
        end do

        IF(kksv > 0) THEN
            Savep = .TRUE.
            call FieldUpdate()
            call WrStatusBar(3, T('unsaved') // "!")
        end if

    end subroutine NetRatesCalc

!#######################################################################

    subroutine GetGamData

        ! this routine refers to the Gamspk1 method of activity calculation from
        ! few gamma peaks:
        ! it reads in all the data from the dialog 'dialog_gspk1'.
        !

        !   Copyright (C) 2020-2023  Günter Kanisch

        USE UR_Gleich_globals,      only: ifehl,missingval,kpoint,Messwert
        USE UR_Linft,       only: numd
        USE UR_Gspk1Fit,    only: unitradio,ecorruse,erg,fbt,guse,kdatmax,mwtyp,wmextsd,rateBG, &
                                  RateCB,SDRateBG,effi,sdeffi,pgamm,sdpgamm,fatt,sdfatt,fcoinsu,sdfcoinsu, &
                                  GNetRate,SDGNetRate,varadd_Rn
        use Rout,           only: WDGetSelRadio,WDGetComboboxAct,WDGetEntryDouble,WDGetCheckButton,    &
                                  WTreeViewGetCheckArray,WTreeViewGetDoubleArray,MEssageShow, &
                                  WTreeViewPutDoubleArray
        use GTK,            only: GTK_BUTTONS_OK, GTK_MESSAGE_WARNING
        use file_io,            only: logger
        use Top,                only: InitVarsTV6
        use translation_module, only: T => get_translation

        implicit none

        integer             :: kxy,kmax,i11,resp,i11max
        character(len=150)  :: str1
        character(len=512)           :: log_str
        logical             :: test1,test2
!-----------------------------------------------------------------------
        call WDGetSelRadio('radiobuttonG1', unitRadio(1))
        call WDGetSelRadio('radiobuttonG5', unitRadio(2))
        call WDGetSelRadio('radiobuttonG9', unitRadio(3))
        call WDGetSelRadio('radiobuttonG11', unitRadio(4))
        call WDGetSelRadio('radiobuttonG13', unitRadio(5))

        call WDGetComboboxAct('comboboxGMWtyp', kxy)
        IF(kxy == 1) mwtyp = 'WeiMean'
        IF(kxy == 2) mwtyp = 'LSQMean'

        kxy = kdatmax
        ! kxy = 20         ! maximal number of gamma lines
        call WDGetEntryDouble('entry_b2LFactor', FBT)
        call WDGetCheckButton('checkbuttonGspk1EffiCov', ecorruse)
        call WDGetCheckButton('checkbuttonMeanOpt', WMextSD)

        if(FBT <= ZERO) then
            write(log_str, '(*(g0))') 'LDN_2422: GetGamData:  Error:  FBT=',real(FBT,4)
            call logger(66, log_str)
            ifehl = 1
            goto 100
        end if

        if(.not.allocated(guse) .and. .not.allocated(erg)) call InitVarsTV6(kxy)

        call WTreeViewGetCheckArray('treeview6',2,kxy,guse)
        call WTreeViewGetDoubleArray('treeview6',3,kxy,erg)
        call WTreeViewGetDoubleArray('treeview6',4,kxy,GNetRate)
        call WTreeViewGetDoubleArray('treeview6',5,kxy,RateCB)
        call WTreeViewGetDoubleArray('treeview6',6,kxy,RateBG)
        call WTreeViewGetDoubleArray('treeview6',7,kxy,SDRateBG)
        call WTreeViewGetDoubleArray('treeview6',8,kxy,effi)
        call WTreeViewGetDoubleArray('treeview6',9,kxy,SDeffi)
        call WTreeViewGetDoubleArray('treeview6',10,kxy,pgamm)
        call WTreeViewGetDoubleArray('treeview6',11,kxy,SDpgamm)
        call WTreeViewGetDoubleArray('treeview6',12,kxy,fatt)
        call WTreeViewGetDoubleArray('treeview6',13,kxy,SDfatt)
        call WTreeViewGetDoubleArray('treeview6',14,kxy,fcoinsu)
        call WTreeViewGetDoubleArray('treeview6',15,kxy,SDfcoinsu)


        kmax = kxy
        write(log_str, '(a,50(i0,1x))') 'UnitRadio=',UnitRadio
        call logger(66, log_str)
        if(allocated(varadd_rn)) deallocate(varadd_rn)
        allocate(varadd_rn(kmax))
        write(log_str, '(a,i0)') 'GetGam:  ubound(effi,dim=1)=',ubound(effi,dim=1)
        call logger(66, log_str)
        i11max = min(kxy, ubound(effi,dim=1))
        do i11=1,i11max                ! kxy+1
            write(log_str, '(2(a,i0))') 'GetgamData:  i11=',i11,' kxy=',kxy
            call logger(66, log_str)
            test1 = abs(effi(i11)-missingval) < EPS1MIN .or. abs(effi(i11)-0.0_rn) < EPS1MIN
            test2 = abs(fatt(i11)-missingval) < EPS1MIN .or. abs(fatt(i11)-0.0_rn) < EPS1MIN
            if(test1 .and. test2) then
                kmax = i11-1
                numd = kmax*5    ! (number of gamma lines) times ( number of quantities per gamma line)
                EXIT
            end if

            if(GnetRate(i11) <= ZERO) ifehl = 1
            if(RateCB(i11) <= ZERO) ifehl = 1
            if(effi(i11) <= ZERO) ifehl = 1
            if(SDeffi(i11) <= ZERO) ifehl = 1
            if(Pgamm(i11) <= ZERO .and. SDPgamm(i11) <= ZERO) then
                Pgamm(i11) = ONE
                SDPgamm(i11) = ZERO
            end if
            if(fatt(i11) <= ZERO .and. SDfatt(i11) <= ZERO) then
                fatt(i11) = ONE
                SDfatt(i11) = ZERO
            end if
            if(fcoinsu(i11) <= ZERO .and. SDfcoinsu(i11) <= ZERO) then
                fcoinsu(i11) = ONE
                SDfcoinsu(i11) = ZERO
            end if

            if(abs(RateBG(i11)- missingval) < EPS1MIN) RateBG(i11) = ZERO
            if(abs(SDRateBG(i11) - missingval) < EPS1MIN) SDRateBG(i11) = ZERO
            if(abs(SDfcoinsu(i11) - missingval) < EPS1MIN) SDfcoinsu(i11) = ZERO
            if(ifehl == 1) then
                call logger(66, 'Error within GetGamdata: one(some) gamma-line input values are <= 0 ')
                goto 100
            end if
            call ConvertGamD(i11)

            if(.not.allocated(Messwert)) return
!-------------------------------------------------------------

            varadd_Rn(i11) = RateCB(i11)*FBT/Messwert(kpoint(2))
            IF(abs(SDRateBG(i11)-missingval) > EPS1MIN ) varadd_Rn(i11) = varadd_Rn(i11) + &
                RateBG(i11)/Messwert(kpoint(2)) + SDRateBG(i11)**TWO

            SDGNetRate(i11) = SQRT( GNetRate(i11)/Messwert(kpoint(2)) + varadd_Rn(i11) )
            write(log_str, '(*(g0))') 'GetgamData: Gnetrate,RateCB,RateBG,Effi,pgamm,fatt,fcoinsu:', sngl(GnetRate(i11)), &
                sngl(RateCB(i11)),sngl(RateBG(i11)),sngl(effi(i11)),sngl(pgamm(i11)),sngl(fatt(i11)),sngl(fcoinsu(i11))
            call logger(66, log_str)
            write(log_str, '(*(g0))') 'GetgamData: SD''s dazu:',sngl(SDRateBG(i11)), &
                sngl(SDeffi(i11)),sngl(SDpgamm(i11)),sngl(SDfatt(i11)),sngl(SDfcoinsu(i11))
            call logger(66, log_str)

        end do
        if(i11 == i11max+1) then; kmax= i11max; numd = kmax*5; end if

        write(log_str, '(*(g0))') 'GetGamData:   kmax=',kmax,'  varadd_Rn()=',(sngl(varadd_Rn(i11)),i11=1,kmax),'   FBT=',sngl(fbt),  &
            ' t: ',sngl(Messwert(kpoint(2)))
        call logger(66, log_str)

        return

100     continue
        if(ifehl == 1) then
            if(FBT <= ZERO) then
                str1 = T("Factor(1+b/2L) is yet undefined!")
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "LoadselD:", resp, mtype=GTK_MESSAGE_WARNING)
            else
                str1 = T("At least 1 cell of the lines is yet undefined!")
                call MessageShow(trim(str1), GTK_BUTTONS_OK, "LoadselD:", resp, mtype=GTK_MESSAGE_WARNING)
            end if

        end if

    end subroutine GetGamData

!###########################################################################################################

    subroutine ConvertGamD(i)

        ! this routine converts the gamma data (from GetGamData),
        ! if:
        ! the time unit given on the dialog is "min" instead of "s";
        ! if any of the array values Effi, Pgamm, SDfatt and SDfcoinsu
        ! are given in per cent, they are converted to absolute vales.
        !
        !   Copyright (C) 2020-2023  Günter Kanisch


        USE UR_Gspk1Fit

        implicit none

        integer   ,intent(in)     :: i

        IF(UnitRadio(1) == 2) THEN
            GNetRate(i) = GNetRate(i)/60._rn
            RateCB(i) = RateCB(i)/60._rn
            RateBG(i) = RateBG(i)/60._rn
            SDRateBG(i) = SDRateBG(i)/60._rn
        end if

        IF(UnitRadio(2) == 1) THEN
            SDEffi(i) = SDEffi(i)*Effi(i) /100._rn
        end if
        IF(UnitRadio(3) == 1) THEN
            SDPgamm(i) = SDPgamm(i)*Pgamm(i) /100._rn
        end if
        IF(UnitRadio(4) == 1) SDfatt(i) = SDfatt(i)*fatt(i)/100._rn
        IF(UnitRadio(5) == 1) SDfcoinsu(i) = SDfcoinsu(i)*fcoinsu(i)/100._rn

    end subroutine ConvertGamD

!###########################################################################################################

    subroutine SaveToConfig(mode, strg)

        ! this routine saves the modified language information into the config file UR2_cfg.dat,
        ! but only, if this was requested by the user.
        !
        !   Copyright (C) 2018-2023  Günter Kanisch

        use ur_general_globals,    only: work_path
        use TOP,             only: chupper_eq
        use file_io,         only: logger
        use chf,             only: flfu
        use UR_params,       only: UR2_CFG_FILE
        implicit none

        integer   ,intent(in)        :: mode     ! 1: langg
        character(len=*),intent(in)  :: strg

        character(len=120)              :: texta
        character(len=120),allocatable  :: textcfg(:)

        integer                   :: k0,ios,i

        allocate(textcfg(60))

        open(32, file=flfu(work_path // UR2_CFG_FILE), status='unknown', iostat=ios)
        ! write(66,*) 'open 32:  ios=',ios
        if(ios == 0) then
            k0 = 0
            do
                read(32,'(a)', iostat=ios) texta
                if(ios /= 0) exit
                k0 = k0 + 1
                textcfg(k0) = trim(texta)
            end do
            rewind (32)
            do i=1, k0
                if(mode == 1) then
                    if(chupper_eq(textcfg(i)(1:9), 'LANGUAGE=')) textcfg(i)(10:11) = trim(strg)
                end if
                write(32,'(a)') trim(textcfg(i))
            end do
            call logger(66, 'Setting language in config file was successful')
        else
            call logger(66, 'Warning: Could not save language to config file')
        end if

        close (32)
        deallocate(textcfg)

    end subroutine SaveToConfig

!###########################################################################################################

    subroutine GetxdataMD_fromTree()

        ! this routine reads in the arrays of single values from which mean values
        ! are attributed to other input quantities. Each data set is evaluated by
        ! the routine MDcalc(k_datvar), which includes also the calculation of
        ! standard uncertainties for the means.
        !
        !   Copyright (C) 2020-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,      only: c_int

        use UR_Gleich_globals,          only: nvalsMD,xdataMD,missingval,k_datvar,nvarsMD,ixdanf, &
                                      meanMD,umeanMD,smeanMD,k_MDtyp,ifehl
        use gtk,                only: GTK_BUTTONS_OK,GTK_MESSAGE_WARNING
        use Top,                only: RealModA1,IntModA1,CharModStr,MDcalc
        use Rout,               only: WTreeViewGetDoubleArray,WDPutEntryDouble,WDPutEntryInt, &
                                      WDPutLabelString, MessageShow
        use translation_module, only: T => get_translation

        implicit none

        integer               :: nvsum,kxy,i,nplus,nvor,ix
        real(rn),allocatable  :: rdummy(:)
        character(len=120)    :: str1
        integer(c_int)        :: resp

        nvsum = sum(nvalsMD(1:nvarsMD))

        kxy = 200
        if(allocated(rdummy)) deallocate(rdummy)
        allocate(rdummy(1)); rdummy(1) = ZERO
        call RealModA1(rdummy,200)
        call WTreeViewGetDoubleArray('treeview8',2,kxy,rdummy)
        nvor = nvalsMD(k_datvar)
        nvalsMD(k_datvar) = 0
        do i=1,kxy
            if(abs(rdummy(i)-missingval) < EPS1MIN) then
                nvalsMD(k_datvar) = max(0,i-1)
                exit
            end if
        end do
        nplus = nvalsMD(k_datvar) - nvor
        ix = ubound(xdataMD,dim=1)
        if(ix == 0) then
            if(allocated(xdataMD)) deallocate(xdataMD)
            allocate(xdataMD(nplus))
            xdataMD = ZERO
        end if

        if(nvsum+nplus > ix) then
            call RealModA1(xdataMD,nvsum+nplus)
            xdataMD(nvsum+1:nvsum+nplus) = ZERO
            if(ix == 0) ixdanf(1) = 1
        end if
        if(k_datvar == 1) then
            if(nvarsMD == 1) then
                xdataMD(1:nvalsMD(k_datvar)) = rdummy(1:nvalsMD(k_datvar))
            elseif(nvarsMD > 1) then
                xdataMD = [ rdummy(1:nvalsMD(k_datvar)),  &
                    xdataMD(ixdanf(k_datvar+1):ixdanf(nvarsMD)+nvalsMD(nvarsMD)-1)]
                ixdanf(k_datvar+1:nvarsMD) = ixdanf(k_datvar+1:nvarsMD) + nplus
            end if
        elseif(k_datvar > 1 .and. k_datvar < nvarsMD) then
            xdataMD = [ xdataMD(1:ixdanf(k_datvar)-1), rdummy(1:nvalsMD(k_datvar)),  &
                xdataMD(ixdanf(k_datvar+1):ixdanf(nvarsMD)+nvalsMD(nvarsMD)-1)]
            ixdanf(k_datvar+1:nvarsMD) = ixdanf(k_datvar+1:nvarsMD) + nplus
        elseif(k_datvar == nvarsMD) then
            ix = ubound(ixdanf,dim=1)
            if(ix < k_datvar) then
                call IntmodA1(ixdanf,k_datvar)  ! nothing
            end if
            xdataMD = [ xdataMD(1:ixdanf(k_datvar)-1), rdummy(1:nvalsMD(k_datvar)) ]
        end if
        deallocate(rdummy)
        ! write(66,*) 'LDN_2685:  ixdanf=',int(ixdanf,2)

        if(nvalsMD(k_datvar) < 4 .and. k_MDtyp(k_datvar) < 3) then
            str1 = T("Error: the number of single values must be > 3")

            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
            ifehl = 1
            return
        end if

! Calculations:
        if(nvalsMD(k_datvar) > 1) then
            call MDcalc(k_datvar)
            call WDPutEntryDouble('TEmeanMD',meanMD(k_datvar),'(es12.6)')
            call WDPutEntryDouble('TEsdMD',umeanMD(k_datvar),'(es12.6)')
            if(k_MDtyp(k_datvar) == 2) then
                call WDPutLabelString('LBsdMD','sn')
                call WDPutLabelString('LBnvar0MD','s0n')
            else
                call WDPutLabelString('LBsdMD','sx')
                call WDPutLabelString('LBnvar0MD','s0x')
            end if
            call WDPutEntryDouble('TEnvar0MD',smeanMD(k_datvar)**ONE,'(es12.6)')
            call WDPutEntryInt('TEmMD',nvalsMD(k_datvar),'(i0)')
        end if

    end subroutine GetxdataMD_fromTree

!###########################################################################################################

    subroutine InfoFX_Select(ifx, buthelp)

        ! show infos about spocial UncertRadio functions, stored in a file InfoFX1.txt

        !   Copyright (C) 2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,      only: c_null_char
        use gtk,            only: gtk_image_set_from_resource, gtk_image_clear
        use UR_Gleich_globals,      only: charv
        use ur_general_globals,     only: help_path
        use CHF,            only: ucase, flfu
        use top,            only: idpt,CharModA1
        use file_io,        only: logger
        use Rout,           only: WDPutTextviewString
        use translation_module, only: get_language

        implicit none

        integer, intent(in)            :: ifx
        character(len=15), intent(out) :: buthelp

        integer                    :: ios, i, nfd, imax
        type(charv),allocatable    :: textcode(:)
        character(len=100)         :: iomessg
        character(len=400)         :: text, textfile
        character(len=15)          :: code

        allocate(textcode(15))

        call gtk_image_clear(idpt('InfoFX_image1'))
        call gtk_image_clear(idpt('InfoFX_image2'))
        call gtk_image_clear(idpt('InfoFX_image3'))
        textfile = help_Path // 'InfoFX1.txt'

        select case (ifx)
          case (2)
            code = 'LINFIT'
            call gtk_image_set_from_resource(idpt('InfoFX_image1'), &
                                             '/org/UncertRadio/icons/preferences-system.png' // c_null_char)
            call gtk_image_set_from_resource (idpt('InfoFX_image2'), &
                                              '/org/UncertRadio/icons/FittingData_24.png' // c_null_char)
            call gtk_image_set_from_resource (idpt('InfoFX_image3'), &
                                              '/org/UncertRadio/icons/FittingResults_24.png' // c_null_char)
            buthelp = 'HelpLinfit'

          case (3)
            code = 'GAMSPK1'
            call gtk_image_set_from_resource (idpt('InfoFX_image2'), &
                                              '/org/UncertRadio/icons/FittingData_24.png' // c_null_char)
            call gtk_image_set_from_resource (idpt('InfoFX_image3'), &
                                              '/org/UncertRadio/icons/FittingResults_24.png' // c_null_char)
            buthelp = 'HelpGspk1'

          case (4)
            code = 'KALFIT'
            buthelp = 'HelpKalib'

          case (5)
            code = 'SUMEVAL'
            buthelp = 'HelpSumEval'

          case (6)
            code = 'UVAL'
            buthelp = 'HelpTextEQ'

          case (7)          ! 2.8.2023
            code = 'FD'
            buthelp = 'HelpFD'

        end select

        close (35)
        nfd = 0
        open(35, file=flfu(textfile), status='old')
        do
            read(35,'(a)',iostat=ios,iomsg=iomessg) text
!             if(ios /= 0) write(66,*) 'headline: error=',trim(iomessg)
            if(ios /= 0) call logger(66, 'headline: error=' // trim(iomessg) )

            if(ios /= 0) exit
            if(get_language() == 'de' .and. index(ucase(text),'#DE#') > 0) exit
            if((get_language()== 'en' .or. get_language() == 'fr') &
                .and. index(ucase(text),'#EN#') > 0) exit
        end do
        do
            read(35,'(a)',iostat=ios,iomsg=iomessg) text
            if(ios /= 0) call logger(66, 'headline: error=' // trim(iomessg))

            if(ios /= 0) exit
            if(text(1:1) == '#' .and. index(ucase(text),trim(code)) == 2) then
                nfd = 1
                do i=1,15
                    read(35,'(A)',iostat=ios,iomsg=iomessg) text
                    if(i > 3 .and. (text(1:1) == '#' .or. ios /= 0)) then
                        imax = i -1
                        exit
                    end if
                    textcode(i)%s = trim(text)
                    ! write(66,*) textcode(i)%s
                    if(ios /= 0) then

                        call logger(66, 'error=' //trim(iomessg))
                        exit
                    end if
                end do
            end if
            if(nfd == 1) exit
        end do
        close (35)

        do i=imax, 3, -1
            if(len_trim(textcode(i)%s) > 0) then
                imax = i
                call CharModA1(textcode,imax)
                exit
            end if
        end do
        call WDPutTextviewString('textview_InfoFX', textcode)


    end subroutine InfoFX_Select

    !########################################################################################


end module LDN
