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
! contains
! ListstoreTranslate
! TranslateUR
!-------------------------------------------------------------------------------------------------!

subroutine ListstoreTranslate()

    ! this routine prepares arrays of mnemonics of the available distributions (vdopt)
    ! and stores them in the liststore lsisstore_dist
    ! The arrays of full names (vdopt_full) and parameter names (vdopt_pname) for distributions
    ! with more than 1 parameter are not yet stored in a liststore.
    ! Other options stored in separate liststores, refer to the arrays
    ! absrel, vcovvor, fitopt, mwopt, GrFormat, MDtyp
    !
    ! All strings are written in that language chosen from three languages, German,
    ! English or French.
    !
    ! Copyright (C) 2014-2023  Günter Kanisch

    use UR_Gleich_globals,        only: vdopt, &
                                ndopt, &
                                absrel, &
                                vcovcor, &
                                GrFormat, &
                                MDtyp,  &
                                incall, &
                                vdoptfull, &
                                vdopt_pname
    use UR_Linft,         only: fitopt, ifit
    use UR_DLIM,          only: NWGmeth, NWGMethode
    use UR_Gspk1Fit,      only: mwopt
    use Rout,             only: WDListstoreFill_1, WDSetComboboxAct
    use translation_module,   only: T => get_translation

    implicit none

    if(.not.allocated(vdopt_pname)) allocate(vdopt_pname(11,4))
    if(.not.allocated(vdopt)) allocate(vdopt(11))
    if(.not.allocated(vdoptfull)) allocate(vdoptfull(11))

    vdopt_pname(10,1)%s = 'alpha'
    vdopt_pname(10,2)%s = 'beta'
    vdopt_pname(10,3)%s = 'min'
    vdopt_pname(10,4)%s = 'max'
    !vdopt_pname(9,1:4) = ['df   ','mu   ','sigma','     ' ]
    vdopt_pname(9,1)%s = 'df'
    vdopt_pname(9,2)%s = 'mu'
    vdopt_pname(9,3)%s = 'sigma'
    vdopt_pname(9,4)%s = ' '


    vdopt(1)%s = T('Normal')
    vdopt(2)%s = T('Rectangl')
    vdopt(3)%s = T('Triangl')
    vdopt(4)%s = T('(N+x) rule')
    vdopt(5)%s = T('LogNormal')
    vdopt(6)%s = T('GammaDist')
    vdopt(7)%s = T('Binom+Poiss')
    vdopt(8)%s = T('Beta2Dist')
    vdopt(9)%s = T('T-Distrib')
    vdopt(10)%s = T('Beta4Dist')
    vdopt(11)%s = T('Npreset')

    vdoptfull(8)%s = T('2-Parameter Beta-distribution')
    vdoptfull(9)%s = T('t-distribution')
    vdoptfull(10)%s = T('4-Parameter Beta-distribution')

    ! 13.5.2024:
    call WDListstoreFill_1('liststore_dist', ndopt, vdopt)

    if(.not.allocated(absrel)) allocate(absrel(2))
    absrel(1)%s = 'abs'
    absrel(2)%s = 'rel'
    if(incall == 1) call WDListstoreFill_1('liststore_absrel', 2, absrel)

    if(.not.allocated(vcovcor)) allocate(vcovcor(2))
    vcovcor(1)%s = 'covariance'
    vcovcor(2)%s = 'correlation'
    if(incall == 1) call WDListstoreFill_1('liststore_covcor', 2, vcovcor)

    if(.not.allocated(fitopt)) allocate(fitopt(3))


    fitopt(1)%s = T('fit')
    fitopt(2)%s = T('fix')
    fitopt(3)%s = T('omit')

    ! 13.5.2024:
    call WDListstoreFill_1('liststoreFitOption', 3, fitopt)
    if(minval(ifit) > 0) then
      call WDSetComboboxAct('comboboxA1', ifit(1))
      call WDSetComboboxAct('comboboxA2', ifit(2))
      call WDSetComboboxAct('comboboxA3', ifit(3))
    end if
    ! write(0,*) 'URinit: after liststoreFitOption'

    if(.not.allocated(mwopt)) allocate(mwopt(2))

    mwopt(1)%s = T('Weighted mean')
    mwopt(2)%s = T('LS mean')

    ! 13.5.2024:
    call WDListstoreFill_1('liststore_GMWtyp', 2, mwopt)

    if(.not.allocated(GrFormat)) allocate(GrFormat(5))

    GrFormat(1)%s = 'PNG Format'
    GrFormat(2)%s = 'JPEG Format'
    GrFormat(3)%s = 'BMP Format'
    GrFormat(4)%s = 'PDF Format'

    GrFormat(5)%s = T('WIN Clipboard')

    !13.5.2024:
    call WDListstoreFill_1('liststore_copymc', 5, GrFormat)
    call WDSetComboboxAct('comboboxBS1',1)

    if(.not.allocated(MDtyp)) allocate(MDtyp(3))
    MDtyp(1)%s = T('(1) not being counts (bayes.)')
    MDtyp(2)%s = T('(2) counts, with influence (bayes.)')
    MDtyp(3)%s = T('(3) classical (non-bayes.)')

    NWGMeth = T('ISO 11929:2019, by iteration')

    ! 13.5.2024:
    call WDListstoreFill_1('liststore_MDtyp', 3, MDtyp)

    NWGMethode = NWGMeth
    ! write(0,*) 'End ListstoreTranslate'

end subroutine ListstoreTranslate

!#########################################################################################

subroutine TranslateUR()

    ! this routine re-writes the text labels of all visible widgets of the GUI,
    ! in that language chosen from three languages, German, English or French.
    ! Tooltip texts are also defined here.
    !
    use, intrinsic :: iso_c_binding,        only: c_ptr, c_null_char, &
                                                  c_null_ptr, c_char, &
                                                  c_int
    use gtk,                  only: gtk_window_set_title, &
                                    gtk_label_set_text, &
                                    gtk_combo_box_text_remove, &
                                    gtk_combo_box_text_insert_text

    use UR_gtk_globals,     only: Notebook_labeltext, clobj, nclobj
    use Top,                  only: idpt
    use Rout,                 only: WDPutLabelString, &
                                    WDPutLabelStringBold, &
                                    SetTooltipText, &
                                    WDPutTreeViewColumnLabel, &
                                    WDGetTreeViewColumnLabel, &
                                    WDSetComboboxAct, &
                                    WDGetComboboxAct, &
                                    WDGetLabelString
    use UR_Linft,             only: use_absTimeStart
    use CHF,                  only: ucase
    use UR_types

    use translation_module,   only: T => get_translation, get_language
    use color_theme

    implicit none

    integer                    :: i, k
    character(len=25)          :: dbox(4)
    character(len=300)         :: str1
    character(len=100)         :: str
    character(len=60)          :: idstr
    type(c_ptr)                :: widget
    integer(c_int)             :: ic

    call ListstoreTranslate()

    select case (ucase(get_language()))
        case ('DE')
            i = 1
        case ('FR')
            i = 3
        case default
            i = 2
    end select

    call WDSetComboboxAct('comboboxLangg', i)

    widget = c_null_ptr
    do i=1,6
        select case (i)
        case (1)
            widget = idpt('NBProcedure')
            Notebook_labeltext(i) = T('Procedure')
        case (2)
            widget = idpt('NBEquations')
            Notebook_labeltext(i) = T('Equations')
        case (3)
            widget = idpt('NBValUnc')
            Notebook_labeltext(i) = T('Values, Uncertainties')
        case (4)
            widget = idpt('NBBudget')
            Notebook_labeltext(i) = T('Uncertainty budget')
        case (5)
            widget = idpt('NBResults')
            Notebook_labeltext(i) = T('Results')
        case (6)
            widget = idpt('NBEditor')
            Notebook_labeltext(i) = T('Text Editor')
        end select
        call gtk_label_set_text(widget,trim(Notebook_labeltext(i))//c_null_char)
    end do

    call WDPutLabelString('CalcCountRates', T('Calculate count rates'))

    call WDPutLabelString('BTOk', T('Apply'))
    call WDPutLabelString('MenuLoadProject', T('Load project'))

    call WDPutLabelString('MenuSaveProject', T('Save project'))
    call WDPutLabelString('MenuSaveProjectAs', T('Save project as'))
    call WDPutLabelString('MenuCloseProject', T('Close Project'))

    call WDPutLabelString('DLLinModel1', T('Fitting function: Y(t) = Fitp1*X1(t) + Fitp2*X2(t) + Fitp3*X3(t)'))
    call WDPutLabelString('label1', T('Which terms to fit?'))
    call WDPutLabelString('label2', T("Fix:   quantify Fitp(i) in 'Values, Uncertainties'!"))
    call WDPutLabelString('checkbuttonWFit', T('apply weighted fit'))
    call WDPutLabelString('checkbuttonCovZR', T('use covariances betw. net counting rates'))
    call WDPutLabelString('checkbuttonAllm', T('define Xi(t) separately for each measurement?'))

    call WDPutLabelString('DMlabelNCH', T('number nchs of counting channels (A, B, C)'))
    call WDPutLabelString('radiobuttonNLSQ', T('Neyman Chi Square WLS'))
    call WDPutLabelString('radiobuttonPLSQ', T('Pearson Chi Square PLSQ'))
    call WDPutLabelString('radiobuttonPMLE', T('Poisson Maximum Likelihood Estimation PMLE'))
    call WDPutLabelString('radiobuttonTLSQ', T('weighted total least-squares WTLS'))

    call WDPutLabelString('label3terms', T('Definition of functions X1 to Xn (n=3*nchs):   '  &
                     // 'Ordering:  like SQL: ''ORDER BY Channel, measurement, term-number'''))

    call WDPutLabelStringBold('labelModeEq', T('Equations of the form  '  &
                        // 'Xi = i-th function Xi(t):'), get_color_string('label_fg'))

    call WDPutLabelString('LBNetBlindVal', T('Net blank count rate:'))
    call WDPutLabelString('checkAbsTime', T('Absolute time indication? (Date format : 01.01.2015 08:30:15)'))
    call WDPutLabelString('LBTimeBase', T('time basis for count-time and -rates:'))
    call WDPutLabelString('LBDateFormat', T('Date + Time of separation'))
    call WDPutLabelString('CalcCountRates', T('Calculate count rates'))
    call WDPutLabelString('Exchange2Symbols', T('Exchange 2 symbols'))
    call WDPutLabelString('LBSymbExchg', T('Select two output quantity symbols to be exchanged:'))
    call WDPutTreeViewColumnLabel('treeview1', 2, T('Symbol'))
    call WDPutTreeViewColumnLabel('treeview1', 3, T('Type'))
    call WDPutTreeViewColumnLabel('treeview1', 4, T('Unit'))
    call WDPutTreeViewColumnLabel('treeview1', 5, T('Meaning'))
    call WDPutTreeViewColumnLabel('treeview2', 2, T('Symbol'))
    call WDPutTreeViewColumnLabel('treeview2', 3, T('Type'))
    call WDPutTreeViewColumnLabel('treeview2', 4, T('Unit'))
    call WDPutTreeViewColumnLabel('treeview2', 5, T('Value'))
    call WDPutTreeViewColumnLabel('treeview2', 6, T('distribut'))
    call WDPutTreeViewColumnLabel('treeview2', 7, T('StdUncFormula'))
    call WDPutTreeViewColumnLabel('treeview2', 8, T('StdUncValue'))
    call WDPutTreeViewColumnLabel('treeview2', 9, T('Half width'))
    call WDPutTreeViewColumnLabel('treeview2', 11, T('abs.Std.Uns.'))
    call WDPutTreeViewColumnLabel('treeview3', 4, T('Type'))
    call WDPutTreeViewColumnLabel('treeview3', 5, T('Formula'))
    call WDPutTreeViewColumnLabel('treeview3', 6, T('Value'))
    call WDPutTreeViewColumnLabel('treeview4', 3, T('Type'))
    call WDPutTreeViewColumnLabel('treeview4', 4, T('Unit'))
    call WDPutTreeViewColumnLabel('treeview4', 5, T('Value'))
    call WDPutTreeViewColumnLabel('treeview4', 6, T('StdUncValue'))
    call WDPutTreeViewColumnLabel('treeview4', 7, T('Sensit. coefficient'))
    call WDPutTreeViewColumnLabel('treeview4', 8, T('rel. contribution(%)'))


    if(use_absTimeStart) then
        call WDPutTreeViewColumnLabel('treeview5', 2, T('Start date')//char(13)//T('(gross)'))
    else
        call WDPutTreeViewColumnLabel('treeview5', 2, T('StartDiff (s)')//char(13)//T('(gross)'))
    end if

    call WDPutTreeViewColumnLabel('treeview5', 3, T('Count time')//char(13)//T('(gross)'))
    call WDPutTreeViewColumnLabel('treeview5', 4, T('Counts')//char(13)//T('(gross)'))
    call WDPutTreeViewColumnLabel('treeview5', 5, T('CountRate')//char(13)//T('(gross)'))
    call WDPutTreeViewColumnLabel('treeview5', 6, T('u(CountRate)')//char(13)//T('(gross)'))

    call WDPutTreeViewColumnLabel('treeview5', 7, T('Count time')//char(13)//T('(BG)'))
    call WDPutTreeViewColumnLabel('treeview5', 8, T('Counts')//char(13)//T('(BG)'))
    call WDPutTreeViewColumnLabel('treeview5', 9, T('CountRate')//char(13)//T('(BG)'))
    call WDPutTreeViewColumnLabel('treeview5', 10, T('u(CountRate)')//char(13)//T('(BG)'))
    call WDPutTreeViewColumnLabel('treeview5', 11, T('CountRate')//char(13)//T('(net)'))
    call WDPutTreeViewColumnLabel('treeview5', 12, T('u(CountRate)')//char(13)//T('(net)'))

    call WDPutLabelString('LBGspk1b2LFactor', T('Factor (1+b/2L) for Compton-BG-countrate:'))
    call WDPutLabelString('LBGspk1EffiCov', T('include efficiency covariances?'))
    call WDPutLabelString('LBGspk1MWB', T('Method for mean calculation:'))
    call WDPutLabelString('checkbuttonMeanOpt', T('Mean: use external SD?'))
    call WDPutLabelStringBold('LBGspk1FrameZR', T('count rates:'), get_color_string('label_fg'))
    call WDPutLabelString('DKlabelTitel', T('Title of this calibration curve:'))
    call WDPutLabelString('DKcheckUfit', T('use u(Fit)?'))
    call WDPutLabelString('DKcheckWTLS', T('use WTLS?'))
    call WDPutLabelStringBold('FrLabelKalib', T('Calibration curve:'), get_color_string('label_fg'))
    call WDPutLabelString('DoKalibFit', T('run the fit:'))
    call WDPutLabelString('DKlabelFparms', T('Fitting paramaters:'))
    call WDPutLabelString('DKlabelFsdev', T('their StdDevs:'))
    call WDPutLabelString('LB1Numegr', T('How many output quantities shall be used simultaneously?'))
    call WDPutLabelString('LB2Numegr', T('The corresponding formula symbols must be defined as the first ones in the equations (top down)'))
    call WDPutLabelString('LBOptGamDistAdd', T('Gamma distribution: value of GamDistAdd(0; 0.5, 1)'))
    call WDPutLabelString('LBOpt1minusG', T('Probability for confidence interval (1-gamma):'))
    call WDPutLabelString('labelLangg', T('language:'))
    call WDPutLabelString('LBListenSeparator', T('List separator (CSV files):'))
    call WDPutLabelString('check_contrastmode', T('Contrast mode'))
    call WDPutLabelString('LBSymbchg1', T('Select symbol to be changed:'))
    call WDPutLabelString('LBSymbchg2', T('Enter new name:'))
    call WDPutLabelString('MenuFile', T('File'))
    call WDPutLabelString('MenuOpenTextFile', T('Open textfile with editor'))
    call WDPutLabelString('MenuQuitProgram', T('Quit program'))
    call WDPutLabelString('MenuEdit', T('Edit'))
    call WDPutLabelString('SelQ', T('Select output quantity:'))
    call WDPutLabelString('QFirst', T('the first'))
    call WDPutLabelString('QSecond', T('the second'))
    call WDPutLabelString('QThird', T('the third'))
    call WDPutLabelString('NumberOutputQuantities', T('number of output quantities'))
    call WDPutLabelString('MenuDecayCurve', T('Decay curve:'))
    call WDPutLabelString('FittingModel', T('Model of decay curve'))
    call WDPutLabelString('FittingData', T('Data input'))
    call WDPutLabelString('FittingResult', T('Curve-fit table'))
    call WDPutLabelString('DLabelColFill', T('Which column to fill:'))

    ! IF(langg == 'DE') call WDPutLabelString('LB2Numegr',  &
    !                     'Die dazugehörigen Formel-Symbole müssen als erste der' // char(13) &
    !                     //'Gleichungen definiert sein (von oben nach unten)')
    ! IF(langg == 'EN') call WDPutLabelString('LB2Numegr', &
    !                     'The corresponding formula symbols must be defined as' // char(13) &
    !                     // 'the first ones in the equations (top down)')
    ! IF(langg == 'FR') call WDPutLabelString('LB2Numegr', &
    !                     'La formule correspondante doit être définie comme' // char(13) &
    !                     // 'les premiers dans les équations (de haut en bas)')


    ! IF(langg == 'DE') str1 = 'Gamma-Verteilung: Wert von GamDistAdd(0; 0.5, 1)' // char(13) // &
    !                         '      Mittelwert(N) = (N + GamDistAdd)'
    ! IF(langg == 'EN') str1 = 'Gamma distribution: value of GamDistAdd(0; 0.5, 1)' // char(13) // &
    !                         '         Mean(N) = (N + GamDistAdd)'
    ! IF(langg == 'FR') str1 = 'Gamma distribution: valeur GamDistAdd(0; 0.5, 1)' // char(13) // &
    !                         '     Valeur moyenne(N) = (N + GamDistAdd)'
    !call WDPutLabelString('LBOptGamDistAdd', trim(str1))


    call WDPutLabelString('DLabelEnterVal', T('Column value:'))
    call WDPutLabelString('MenuGSpekt1', T('Gamma spectrometry'))
    call WDPutLabelString('Gspk1Edit', T('Edit gamma lines'))
    call WDPutLabelString('Gspk1Mean', T('Average line activity'))
    call WDPutLabelString('KalFit', T('Calibration curve'))
    call WDPutLabelString('Report', T('Report'))
    call WDPutLabelString('RenameQuantity', T('Change symbol name'))
    call WDPutLabelString('EQRenameSymb', T('Change symbol'))
    call WDPutLabelString('MenuOptions', T('Options'))
    call WDPutLabelString('PreSettings', T('Pre-settings'))
    call WDPutLabelString('menuitem21', T('Project load:'))

    call WDPutLabelString('LoadWithCalc', T('with automat. calculations'))
    call WDPutLabelString('LoadWithoutCalc', T('without automat. calculations'))
    call WDPutLabelString('ExportToR', T('LSQ Export to R?'))
    call WDPutLabelString('ModelType', T('Model type'))
    call WDPutLabelString('ConfidEllipse', T('calculate confidence ellipse'))
    call WDPutLabelString('MT_GUMonly', T('only GUM, without DL'))
    call WDPutLabelString('MT_PosLin', T('posit. linear, with DL'))
    call WDPutLabelString('MT_NegLin', T('negat. linear, with DL'))
    call WDPutLabelString('ActivateBS', T('Activate BS module'))
    call WDPutLabelString('FontSel', T('Select font'))
    call WDPutLabelString('ColorSel', T('Select color'))
    call WDPutLabelString('MenuHelp', T('_Help'))
    call WDPutLabelString('Help_UR', T('Help UncertRadio'))
    call WDPutLabelString('HelpExamples', T('Help Examples'))
    call WDPutLabelString('LBNoteProcedure', T('Here, a short description of the procedure can be given:'))

    call WDPutLabelString('LBNoteEquations', T('Input of equations, line by line; the fist equation defines the output quantity by other quantities, which then are defined by auxiliary equations following the first one.'))

    call WDPutLabelStringBold('LBFrameEquations', T('Equations'), get_color_string('label_fg'))
    call WDPutLabelString('button_LoadSymbols', T('Load symbols(1) from equations'))
    call WDPutLabelString('LBComplementUnits', T('Complement units and meanings below'))
    call WDPutLabelString('EnlargeSymbList', T('Enlarge the table'))
    call WDPutLabelStringBold('LBFrameSymbolTable', T('Table of symbols:'), get_color_string('label_fg'))
    call WDPutLabelString('LoadCompletedSyms', T('Load symbols(2) from finalized symbol table'))
    call WDPutLabelString('LBOutpQuantity', T('Selected output quantity:'))
    call WDPutLabelString('LBNetRate', T('net count rate:'))
    call WDPutLabelString('LBGrossRate', T('gross count rate:'))
    call WDPutLabelString('AcceptAll', T('Accept all'))
    call WDPutLabelString('LBSel2Rates', T('Select net and gross count rate symbols:'))
    call WDPutLabelString('LBEditingCell', T('Text cell for editing a longer formula'))
    call WDPutLabelString('LabNotesValUnc', T('Attention: User input is only permitted in the green and white cells!'))
    call WDPutLabelStringBold('LBFrameValTable', T('Table of values, uncertainties:'), get_color_string('label_fg'))
    call WDPutLabelStringBold('LBFrameCovars', T('Input of covariances/correlations:'), get_color_string('label_fg'))
    call WDPutLabelString('EnlargeValUnc', T('Enlarge table'))
    call WDPutLabelString('CalcValUnc', T('Calculation of uncertainties'))
    call WDPutLabelString('EnlargeBudget', T('Enlarge table'))
    call WDPutLabelString('ChangeBudgetType', T('Change budget type'))
    call WDPutLabelStringBold('LBFrameBudget', T('Table of uncertainty budget:'), get_color_string('label_fg'))

    call WDPutLabelString('TRlabCoverf', T('Coverage factor k:'))
    call WDPutLabelString('TRlabValue', T('Value output quantity:'))
    call WDPutLabelString('TRlabUnc', T('extendend (Std.-)uncertainty:'))
    call WDPutLabelString('TRlabUncPC', T('relative ext.(Std.-)uncertainty:'))
    call WDPutLabelString('TRBayes', T('Best Bayesian Estimates:'))
    call WDPutLabelString('TRlabValueBy', T('Value output quantity:'))
    call WDPutLabelString('TRlabUncBy', T('extendend (Std.-)uncertainty:'))
    call WDPutLabelString('TRlabLQBy', T('lower range limit:'))
    call WDPutLabelString('TRlabUQBy', T('upper range limit:'))
    call WDPutLabelString('TRlabGamma', T('Probability (1-gamma):'))
    call WDPutLabelStringBold('TRLBFrameMessErg', T('Final measurement result:'), get_color_string('label_fg'))
    call WDPutLabelString('TRLBMCvalPE', T('primary estimate:'))
    call WDPutLabelString('TRLBMCvalUPE', T('uncertainty primary estimate:'))
    call WDPutLabelString('TRLBMCval', T('Value output quantity:'))
    call WDPutLabelString('TRLBMCunc', T('extendend uncertainty:'))
    call WDPutLabelString('TRLBMClq', T('lower range limit:'))
    call WDPutLabelString('TRLBMCUuq', T('upper range limit:'))
    call WDPutLabelString('TRLBMCDT', T('Decision threshold') // ' (' // T('DT') // '):')
    call WDPutLabelString('TRLBMCdl', T('Detection limit') // ' (' // T('DL') // '):')
    call WDPutLabelString('TRLBMCarun', T('active run:'))
    call WDPutLabelString('TRLBMCAnzM', T('Number of simul. measurments'))
    call WDPutLabelString('TRLBMCAnzR', T('Number of runs:'))
    call WDPutLabelString('TRcheckbutton2', T('min. Coverage interval'))
    call WDPutLabelString('TRcheckbutton3', T('min. Coverage interval'))
    call WDPutLabelString('TRButtonStartMC', T('Start MC'))
    call WDPutLabelStringBold('TRLBFRMCsim', T('Monte Carlo Simulation:'), get_color_string('label_fg'))
    call WDPutLabelString('TRbuttonSavecsv', T('Save to csv'))
    call WDPutLabelString('TRlabDT', T('Decision threshold') // ' (' // T('DT') // '):')
    call WDPutLabelString('TRlabDL', T('Detection limit') // ' (' // T('DL') // '):')
    call WDPutLabelString('TRLBMCuncrel', T('relative extd.(Std.-)uncertainty:'))
    call WDPutLabelString('TRlabMethod', T('Method: ISO 11929, iteratively'))
    call WDPutLabelStringBold('TRlabFrDL', T('Decision thresh. and Detection limit:'), get_color_string('label_fg'))
    call WDPutLabelString('TRlabUfit', T('from LS analysis:'))
    call WDPutLabelString('TRlabUprop', T('from uncertainty propagation:'))
    call WDPutLabelString('TRlabChisqr', T('reduced Chi-square:'))
    call WDPutLabelStringBold('TRlabFRModel', T('LinFit: Standard uncertainty of fit parameter ai:'), get_color_string('label_fg'))

    call WDPutLabelString('LBsdMD', T('sd.dev'))
    call WDPutLabelString('LBnvar0MD', T('s0x'))
    call WDPutLabelString('LBMeanMD', T('Mean'))
    call WDPutLabelString('labelMDdata', T('sel. variable:'))
    call WDPutLabelString('labelMDtyp', T('type of mean:'))
    call WDPutLabelString('MDCalcMean', T('calculate mean'))
    call WDPutLabelString('FRlabelMD', T('Data input') // ":")
    call WDPutTreeViewColumnLabel('treeview8', 2, T('Value'))
    call WDPutLabelString('LBrefMD', T('sel. data record used as reference'))
    call WDPutLabelString('TRLBdtMT', T('Decision threshold') // ' (' // T('DT') // '):')
    call WDPutLabelString('TRLBbfMT', T('Bayes factor:'))
    call gtk_window_set_title(idpt('dialog_LoadPro'), T('Project load:') // c_null_char)

    str1 = T("The program is loading the project file and does all calculations subsequently.") // new_line('A') // &
           T("This may take few seconds!") // new_line('A')  // &
           T("Please, wait until this dialog has vanished!")
    call WDPutLabelString('DlabelLoadPro', trim(str1))

    call WDPutLabelString('doELIplot', T('plot confidence ellipse:'))
    call WDPutLabelString('TEClose', T('Close'))
    call WDPutLabelString('TESavePrjAs', T('Save modified project as'))
    call WDPutLabelStringBold('LBframeELI', T('Summary of output quantities:'), get_color_string('label_fg'))
    call gtk_window_set_title(idpt('dialogDecayModel'), T('Specifications for running the adjustment:') // c_null_char)
    call gtk_window_set_title(idpt('dialog_decayvals'), T('Values of the decay curve:') // c_null_char)
    call gtk_window_set_title(idpt('dialog_gspk1'), T('Values of the spectrum evaluation:') // c_null_char)
    call gtk_window_set_title(idpt('dialog_kalfit'), T('Calibration curve editing:') // c_null_char)
    call gtk_window_set_title(idpt('dialogELI'), T('Confidence ellipse:') // c_null_char)
    call gtk_window_set_title(idpt('dialogMeanData'), T('Means of input quantities:') // c_null_char)
    call WDPutLabelString('SerialEval', T('Serial evaluation single project'))
    call gtk_window_set_title(idpt('dialogSerEval'), T('Serial evaluation:') // c_null_char)
    call gtk_window_set_title(idpt('dialog_distributions'), T('distribution parameters:') // c_null_char)
    call WDPutLabelString('labelCB', T('Color for: '))
    call WDPutLabelString('radiobutton_bg_color', T('Background'))
    call WDPutLabelString('radiobutton_sel_bg_color', T('Background-selected'))
    call WDPutLabelString('LBnrecsSE', T('Which records to calculate:'))
    call WDPutLabelString('LBFile1SE', T('Project file (txp)'))
    call WDPutLabelString('LBFile2SE', T('File with variable data (csv)'))
    call WDPutLabelString('LBFromSE', T('from:'))
    call WDPutLabelString('LBToSE', T('until:'))
    call WDPutLabelString('LBncrunsMCSE', T('Number of runs'))
    call WDPutLabelString('CheckMCSE', T('MC-Simulation?'))
    call WDPutLabelString('LBncmaxMCSE', T('Number of simul. measurements'))
    call WDPutLabelString('LBnrecsBEV', T('Which records to calculate:'))
    call WDPutLabelString('LBFromBEV', T('from:'))
    call WDPutLabelString('LBToBEV', T('until:'))
    call WDPutLabelString('LBncrunsMCBEV', T('Number of runs'))
    call WDPutLabelString('CheckMCBEV', T('MC-Simulation?'))
    call WDPutLabelString('LBncmaxMCBEV', T('Number of simul. measurements'))
    call WDPutLabelString('BatestUser', T('QC-Batch-Test'))
    call gtk_window_set_title(idpt('dialog_Batest'), T('Self-testing:') // c_null_char)

    call WDPutLabelString('BT_label1', T('File with reference values'))
    call WDPutLabelString('BT_label2', T('Output file'))
    call gtk_window_set_title(idpt('dialog_BinPoi'), T('Binom+Poisson - parameters:') // c_null_char)
    call WDPutLabelString('BinPoiPars', T('Set binomial/poisson case'))
    call WDPutLabelString('LBBinPoi1', T('Select symbols for (binomial + poisson)'))
    call WDPutLabelString('LBBinPoi2', T('Parameter p of BinomPDF'))
    call WDPutLabelString('LBBinPoi3', T('background count rate'))
    call WDPutLabelString('LBBinPoi4', T('Sample count time'))
    call WDPutLabelString('LBBinPoi5', T('decay constant'))
    call WDPutLabelString('BatFiles', T('Batch evaluation of projects'))
    call WDPutLabelString('DistribLB3', T('Parameter values:'))
    call WDPutLabelString('CheckUnits', T('test physical units'))
    call WDPutLabelString('HelpFunctions', T('functions'))
    call WDPutLabelString('LBInfoFX', T('Infos about special UR functions'))

    ! Flo removed as it is not used?
    ! if(.false.) then
    !     !  mcmc-bezogen:
    !     IF(langg == 'DE') call WDPutLabelString('TRButtonStartMC1', 'Start MCMC')
    !     IF(langg == 'EN') call WDPutLabelString('TRButtonStartMC1', 'Start MCMC')
    !     IF(langg == 'FR') call WDPutLabelString('TRButtonStartMC1', 'Démarrer MCMC')

    !     IF(langg == 'DE') call WDPutLabelStringBold('TRLBFRMCsim1', 'Bayes MCMC:', get_color_string('label_fg'))
    !     IF(langg == 'EN') call WDPutLabelStringBold('TRLBFRMCsim1', 'Baysian MCMC:', get_color_string('label_fg'))
    !     IF(langg == 'FR') call WDPutLabelStringBold('TRLBFRMCsim1', 'MCMC bayésien:', get_color_string('label_fg'))

    !     IF(langg == 'DE') call WDPutLabelString('TRLBaccRat', 'Akzept. Verhältnis:')
    !     IF(langg == 'EN') call WDPutLabelString('TRLBaccRat', 'Accept. ratio:')
    !     IF(langg == 'FR') call WDPutLabelString('TRLBaccRat', 'taux d''accept:')

    !     IF(langg == 'DE') call WDPutLabelString('TRLBMCAnzM1', 'simul. Messungen')
    !     IF(langg == 'EN') call WDPutLabelString('TRLBMCAnzM1', 'simul. measurments')
    !     IF(langg == 'FR') call WDPutLabelString('TRLBMCAnzM1', 'simul. mesurages')

    !     IF(langg == 'DE') call WDPutLabelString('TRLBMCAnzR1', 'Anzahl der Runs:')
    !     IF(langg == 'EN') call WDPutLabelString('TRLBMCAnzR1', 'Number of runs:')
    !     IF(langg == 'FR') call WDPutLabelString('TRLBMCAnzR1', 'Nombre de courses:')

    !     IF(langg == 'DE') call WDPutLabelString('TRcheckbutton4', 'min. Überdekckungs-Intervall')
    !     IF(langg == 'EN') call WDPutLabelString('TRcheckbutton4', 'min. Coverage interval')
    !     IF(langg == 'FR') call WDPutLabelString('TRcheckbutton4', 'min. Intervalle de couverture')

    !     IF(langg == 'DE') call WDPutLabelString('TRLBMCarun1', 'aktiver Run:')
    !     IF(langg == 'EN') call WDPutLabelString('TRLBMCarun1', 'active run:')
    !     IF(langg == 'FR') call WDPutLabelString('TRLBMCarun1', 'course active')

    !     if(langg == 'DE') call WDPutLabelString('LBncrunsMCMCSE', 'Anzahl der Runs')
    !     if(langg == 'EN') call WDPutLabelString('LBncrunsMCMCSE', 'Number of runs')
    !     if(langg == 'FR') call WDPutLabelString('LBncrunsMCMCSE', 'Nombre de courses')

    !     if(langg == 'DE') call WDPutLabelString('CheckMCMCSE', 'MCMC-Simulation?')
    !     if(langg == 'EN') call WDPutLabelString('CheckMCMCSE', 'MCMC-Simulation?')
    !     if(langg == 'FR') call WDPutLabelString('CheckMCMCSE', 'MCMC-Simulation?')

    !     if(langg == 'DE') call WDPutLabelString('LBFRMCMCSE', 'MCMC:')
    !     if(langg == 'EN') call WDPutLabelString('LBFRMCMCSE', 'MCMC:')
    !     if(langg == 'FR') call WDPutLabelString('LBFRMCMCSE', 'MCMC:')

    !     if(langg == 'DE') call WDPutLabelString('LBncmaxMCMCSE', 'Anzahl der simul. Messungen')
    !     if(langg == 'EN') call WDPutLabelString('LBncmaxMCMCSE', 'Number of simul. measurements')
    !     if(langg == 'FR') call WDPutLabelString('LBncmaxMCMCSE', 'Nombre de mesures simulées')

    !     if(langg == 'DE') call WDPutLabelString('CheckMCMCBEV', 'MCMC-Simulation?')
    !     if(langg == 'EN') call WDPutLabelString('CheckMCMCBEV', 'MCMC-Simulation?')
    !     if(langg == 'FR') call WDPutLabelString('CheckMCMCBEV', 'MCMC-Simulation?')

    !     if(langg == 'DE') call WDPutLabelString('LBFRMCMCBEV', 'MCMC:')
    !     if(langg == 'EN') call WDPutLabelString('LBFRMCMCBEV', 'MCMC:')
    !     if(langg == 'FR') call WDPutLabelString('LBFRMCMCBEV', 'MCMC:')

    !     if(langg == 'DE') call WDPutLabelString('LBncrunsMCMCBEV', 'Anzahl der Runs')
    !     if(langg == 'EN') call WDPutLabelString('LBncrunsMCMCBEV', 'Number of runs')
    !     if(langg == 'FR') call WDPutLabelString('LBncrunsMCMCBEV', 'Nombre de courses')

    !     if(langg == 'DE') call WDPutLabelString('LBncmaxMCMCBEV', 'Anzahl der simul. Messungen')
    !     if(langg == 'EN') call WDPutLabelString('LBncmaxMCMCBEV', 'Number of simul. measurements')
    !     if(langg == 'FR') call WDPutLabelString('LBncmaxMCMCBEV', 'Nombre de mesures simulées')

    !     call SetTooltipText('TRentryMCanzM1', '<= 1.0 E06')
    !     call SetTooltipText('TRentryMCanzR1', '<= 50')

    !     if(langg == 'DE') then
    !         call SetTooltipText('TRcheckbuttonBMT','schätze Erkennungsgrenze mit Bayes-Modell-Test')
    !     elseif(langg == 'EN') then
    !         call SetTooltipText('TRcheckbuttonBMT','estimate decision threshold by Bayesian Model-testing')
    !     elseif(langg == 'FR') then
    !         call SetTooltipText('TRcheckbuttonBMT','estimation du seuil de décision par essai bayésien')
    !     end if
    ! end if

    call WDGetComboboxAct('comboboxtextdcol',k)
    do ic=4_c_int, 0_c_int,-1
        call gtk_combo_box_text_remove(idpt('comboboxtextdcol'), ic)
    end do

    dbox(1) = T('Count time (gr)')
    dbox(2) = T('Counts (gr)')
    dbox(3) = T('Count time (BG)')
    dbox(4) = T('Counts (BG)')

    do i = 1, size(dbox)
        ic = i - 1_c_int
        call gtk_combo_box_text_insert_text(idpt('comboboxtextdcol'), ic,trim(dbox(i))//c_null_char)
    end do
    call WDSetComboboxAct('comboboxtextdcol',k)

    !------------------------------------------------------------
    ! tooltips for toolbar icons:

    call SetTooltipText('LBFrameEquations', T('Example: ') // char(13) //&
                    T('ActConc = Rnet * CalFact') // char(13) // &
                    T('Rnet = Rgross - Rbackgrd') // char(13) // &
                    T('Rgross = Ngross / tmeas') // char(13) // &
                    T('Rbackgrd = Nbackgrd / tbg'))

    call SetTooltipText('Report', T('Summary of results'))
    call SetTooltipText('RenameQuantity', T('Replace a symbol name within the whole project'))
    call SetTooltipText('EQRenameSymb', T('Replace a symbol name within the whole project'))

    call SetTooltipText('MT_GUMonly', T('Simplified procedure:') // char(13) // &
                        T('Without calculating decision threshold and detection limit'))

    call SetTooltipText('MT_PosLin', T('Normal procedure, Activity pos. linear to CRrate:') // char(13) // &
                        T('With calculating decision threshold and detection limit'))

    call SetTooltipText('MT_NegLin', T('Other procedure, OUT neg. linear to IN:') // char(13) // &
                        T('With calculating decision threshold and detection limit'))

    call SetTooltipText('ConfidEllipse', T('Summary and plot of confidence ellipse'))
    call SetTooltipText('LoadWithCalc', T('Loading the project file includes all calculations'))
    call SetTooltipText('LoadWithoutCalc', T('Loading the project file is performed in single user steps through the TABs'))
    call SetTooltipText('statusbar4', T('Hints for the next step to be taken by the user'))
    call SetTooltipText('TBLoadProject', T('Load project file'))

    call SetTooltipText('TBSaveProject', T('Save project file'))
    call SetTooltipText('TBSaveProjectAs', T('Save project file as'))
    call SetTooltipText('TBCloseProject', T('Close project file'))
    call SetTooltipText('TBRefreshCalc', T('Update calculations for current output quantity'))
    call SetTooltipText('TBRemoveGridLine', T('Delete table/grid line(s)'))
    call SetTooltipText('TBModelDialog', T('Edit curve-fitting model'))
    call SetTooltipText('TBInputDialog', T('Edit Curve-fit input values'))
    call SetTooltipText('TBFittingResult', T('View of fitting results'))
    call SetTooltipText('TBInfoDialog', T('CHM help'))
    call SetTooltipText('TBProblems', T('First aid hints'))
    call SetTooltipText('TBFontSel', T('Font selection dialog'))
    call SetTooltipText('TBColorSel', T('Background colors selection dialog'))
    call SetTooltipText('copyBS1', T('Copy graph into file; format selectable to the left'))
    call SetTooltipText('comboboxBS1', T('Selection of graphical output format'))
    call SetTooltipText('CopyGrELI', T('Copy graph into file; format selectable to the left'))
    call SetTooltipText('comboboxGrELI', T('Selection of graphical output format'))
    call SetTooltipText('Exchange2Symbols', T('Exchange of two output quantities, including equations'))
    call SetTooltipText('comboboxSymbchg', T('The symbol list is complete only after having loaded the finalized symbol table'))
    call SetTooltipText('comboboxSymbExchgA', T('The symbol list is complete only after having loaded the finalized symbol table'))

    call SetTooltipText('TRentryMCanzM', '<= 2 E06')
    call SetTooltipText('TRentryMCanzR', '<= 50')

    call SetTooltipText('dialog-vbox20', T('at present, colors cannot be adapted'))
    call SetTooltipText('TBmeansMD', T('Dialog for variable-averages'))

    call SetTooltipText('combobox_MDtyp', T('(Type1 1): sx^2 = (m-1)/(m-3)*s0x^2/m') // char(13) //   &
                                        T('(Type 2:) sn^2 = (n_av +(m-1)/(m-3)*(n_av+s0n^2))/m') //   &
                                        char(13) // T('(Type 3:) sx^2 = s0x^2/m  (classical)') //   &
                                        char(13) // T('m: number of vals'))

    call SetTooltipText('TRcheckbutton3', T('also changes the associated values') // char(13) // T("See also: 'coverage intervals' in file MC_Tables.txt"))
    call SetTooltipText('TRcheckbutton2', T('also changes the associated values') // char(13) // T("See also: 'coverage intervals' in file MC_Tables.txt"))

    call SetTooltipText('LBEditingCell', T('copy (long) formula into it and after editing copy it back'))

    str1 = T('The symbol list is not actual before loading the finalized symbol table')
    call SetTooltipText('comboboxBinPoi1', trim(str1))
    call SetTooltipText('comboboxBinPoi2', trim(str1))
    call SetTooltipText('comboboxBinPoi3', trim(str1))
    call SetTooltipText('comboboxBinPoi4', trim(str1))

    call SetTooltipText('TBDistribDialog', T('distribution parameter: first select row of the variable in TAB 3'))
    call SetTooltipText('DistribGrid', T('Data: see diolog for means'))
    call SetTooltipText('TRbuttonSavecsv', T('Save results to a CSV file'))
    call SetTooltipText('comboboxA1', T('Click mouse, hold and select'))

    call SetTooltipText('CheckUnits', T('After the test, the project can be changed (without notice)!') // char(13) // &
                                      T('Save the project explicitly, if necessary under a new name,') // char(13) //  &
                                      T('if you want to continue using it.'))

    call SetTooltipText('HelpFunctions', T('special functions:') // char(13) // &
                                      'sqrt(x)         '// T('square root function') // char(13) // &
                                      'exp(x)          '// T('exponential function') // char(13) // &
                                      'log(x), ln(x) '// T('natural logarithm')  // char(13) // &
                                      'log10(x)      '// T('decimal logarithm') // char(13)  // &
                                      'a^b, a**b    '// T('power function') // char(13) // char(13)  // &
                                      'fd(tA,tm,lam) = exp(-lam*tA)*(1-exp(-lam*tm))/(lam*tm) ' // char(13) // &
                                      'uval(x)       '// T('get the internal uncertainty value of the symbol x') // char(13) // &
                                      T("further functions: see Toolbar Icon 'f(x)'"))

    call SetTooltipText('checkAbsTime', T("if disabled: The unit under 'StartDiff' must always be 'second'!"))

    call SetTooltipText('radiobuttonNLSQ', T('standard fit method, for large counts, CHM 7.3.4'))
    call SetTooltipText('radiobuttonPLSQ', T('fit method for small counts, CHM 7.3.4'))
    call SetTooltipText('radiobuttonPMLE', T('fit method for very small counts, CHM 7.3.4'))
    call SetTooltipText('URfunctions', T('Infos about special UR functions'))

    do i=1,nclobj
        if(clobj%name(i)%s /= 'GtkButton') cycle
        idstr = clobj%idd(i)%s
        str = clobj%label(i)%s
        call WDPutLabelString(idstr, T(trim(str)))
    end do

end subroutine TranslateUR
