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

subroutine ListstoreTranslate(langg)

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

    use UR_Gleich,        only: vdopt, &
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

    implicit none

    character(len=*), intent(in)  :: langg

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

    IF(langg == 'DE') THEN
        vdopt(1)%s = 'Normal'
        vdopt(2)%s = 'Rechteck'
        vdopt(3)%s = 'Dreieck'
        vdopt(4)%s = '(N+x)-Regel'
        vdopt(5)%s = 'LogNormal'
        vdopt(6)%s = 'GammaDist'
        vdopt(7)%s = 'Binom+Poiss'
        vdopt(8)%s = 'Beta2Dist'
        vdopt(9)%s = 'T-Vertlg'
        vdopt(10)%s = 'Beta4Dist'
        vdopt(11)%s = 'Npreset'

        vdoptfull(8)%s = '2-Parameter Beta-Verteilung'
        vdoptfull(9)%s = 't-Verteilung'
        vdoptfull(10)%s = '4-Parameter Beta-Verteilung'

    end if
    IF(langg == 'EN') THEN
        vdopt(1)%s = 'Normal'
        vdopt(2)%s = 'Rectangl'
        vdopt(3)%s = 'Triangl'
        vdopt(4)%s = '(N+x) rule'
        vdopt(5)%s = 'LogNormal'
        vdopt(6)%s = 'GammaDist'
        vdopt(7)%s = 'Binom+Poiss'
        vdopt(8)%s = 'Beta2Dist'
        vdopt(9)%s = 'T-Distrib'
        vdopt(10)%s = 'Beta4Dist'
        vdopt(11)%s = 'Npreset'

        vdoptfull(8)%s = '2-Parameter Beta-distribution'
        vdoptfull(9)%s = 't-distribution'
        vdoptfull(10)%s = '4-Parameter Beta-distribution'
    end if

    IF(langg == 'FR') THEN
        vdopt(1)%s = 'Normale'
        vdopt(2)%s = 'Rectangul'
        vdopt(3)%s = 'Triangul'
        vdopt(4)%s = '(N+x) règle'
        vdopt(5)%s = 'LogNormale'
        vdopt(6)%s = 'GammaDist'
        vdopt(7)%s = 'Binom+Poiss'
        vdopt(8)%s = 'Beta2Dist'
        vdopt(9)%s = 'T-Distrib'
        vdopt(10)%s = 'Beta4Dist'
        vdopt(11)%s = 'Npreset'

        vdoptfull(8)%s = '2-Parameter Beta-distribution'
        vdoptfull(9)%s = 't-distribution'
        vdoptfull(10)%s = '4-Parameter Beta-distribution'

    end if
    ! write(66,*) 'vor call WDListstoreFill_1: ndopt=',ndopt,'  vdopt:',(vdopt(i)%s,i=1,ndopt)

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
    IF(langg == 'DE') THEN
        fitopt(1)%s = 'fitten'
        fitopt(2)%s = 'fixieren'
        fitopt(3)%s = 'weglassen'
    end if
    IF(langg == 'EN') THEN
        fitopt(1)%s = 'fit'
        fitopt(2)%s = 'fix'
        fitopt(3)%s = 'omit'
    end if
    IF(langg == 'FR') THEN
        fitopt(1)%s = 'aligner'
        fitopt(2)%s = 'fixer'
        fitopt(3)%s = 'omettre'
    end if
    ! 13.5.2024:
    call WDListstoreFill_1('liststoreFitOption', 3, fitopt)
    if(minval(ifit) > 0) then
      call WDSetComboboxAct('comboboxA1', ifit(1))
      call WDSetComboboxAct('comboboxA2', ifit(2))
      call WDSetComboboxAct('comboboxA3', ifit(3))
    end if
    ! write(0,*) 'URinit: after liststoreFitOption'

    if(.not.allocated(mwopt)) allocate(mwopt(2))
    IF(langg == 'DE') THEN
        mwopt(1)%s = 'gewichteter Mittelwert'
        ! mwopt(2) = 'Arithm. Mittelwert, korr. nach NIST-2004'
        mwopt(2)%s = 'LSQ-Mittelwert'
    end if
    IF(langg == 'EN') THEN
        mwopt(1)%s = 'Weighted mean'
        ! mwopt(2) = 'Arithm. mean, corr. accd. NIST-2004'
        mwopt(2)%s = 'LS mean'
    end if
    IF(langg == 'FR') THEN
        mwopt(1)%s = 'Moyenne pondérée'
        ! mwopt(2) = 'Arithm. mean, corr. accd. NIST-2004'
        mwopt(2)%s = 'moyenne par LS'
    end if
    ! 13.5.2024:
    call WDListstoreFill_1('liststore_GMWtyp', 2, mwopt)
    ! call WDSetComboboxAct('comboboxGMWtyp', max(1,kmwtyp))

    if(.not.allocated(GrFormat)) allocate(GrFormat(5))
    IF(langg == 'DE' .or. langg == 'EN' .or. langg == 'FR') THEN
        GrFormat(1)%s = 'PNG Format'
        GrFormat(2)%s = 'JPEG Format'
        GrFormat(3)%s = 'BMP Format'
        GrFormat(4)%s = 'PDF Format'
        if(langg == 'EN') GrFormat(5)%s = 'WIN Clipboard'
        if(langg == 'DE') GrFormat(5)%s = 'WIN Zw.Ablage'
        if(langg == 'FR') GrFormat(5)%s = 'WIN press-papiers'
    end if
    !13.5.2024:
    call WDListstoreFill_1('liststore_copymc', 5, GrFormat)
    call WDSetComboboxAct('comboboxBS1',1)

    if(.not.allocated(MDtyp)) allocate(MDtyp(3))
    IF(langg == 'DE') THEN
        MDtyp(1)%s = '(1) nicht Impulsanzahlen (bayes.)'
        MDtyp(2)%s = '(2) Impulsanzahlen, mit Einfluss (bayes.)'
        MDtyp(3)%s = '(3) klassisch (nicht-bayes.)'         !  'nur <= 3 Werte vorhanden'
    end if
    IF(langg == 'EN') THEN
        MDtyp(1)%s = '(1) not being counts (bayes.)'
        MDtyp(2)%s = '(2) counts, with influence (bayes.)'
        MDtyp(3)%s = '(3) classical (non-bayes.)'         !  'only <= 3 values exist'
    end if
    IF(langg == 'FR') THEN
        MDtyp(1)%s = '(1) ne pas être compte (bayés.)'
        MDtyp(2)%s = '(2) compte, avec influence (bayés.)'
        MDtyp(3)%s = '(3) classique (non-bayés)'         ! 'seulement <= 3 valeurs existent'
    end if
    ! 13.5.2024:
    call WDListstoreFill_1('liststore_MDtyp', 3, MDtyp)

    ! if(.not.allocated(NWGmeth)) allocate(NWGmeth)
    ! IF(langg == 'DE') NWGMeth(1) = 'DIN25482,Teil 10, iterativ'
    IF(langg == 'DE') NWGMeth = 'ISO 11929:2019, iterativ'
    IF(langg == 'EN') NWGMeth = 'ISO 11929:2019, by iteration'
    IF(langg == 'FR') NWGMeth = 'ISO 11929:2019, itérative'
    ! IF(langg == 'DE') NWGMeth(2) = 'MAL-Kanisch, iterativ'
    NWGMethode = NWGMeth
    ! write(0,*) 'End ListstoreTranslate'

end subroutine ListstoreTranslate

!#########################################################################################

subroutine TranslateUR

    ! this routine re-writes the text labels of all visible widgets of the GUI,
    ! in that language chosen from three languages, German, English or French.
    ! Tooltip texts are also defined here, also language-dependent.
    !
    use, intrinsic :: iso_c_binding,        only: c_ptr, c_null_char, &
                                                  c_null_ptr, c_char, &
                                                  c_int
    use UR_variables,         only: langg
    use gtk,                  only: gtk_window_set_title, &
                                    gtk_label_set_text, &
                                    gtk_combo_box_text_remove, &
                                    gtk_combo_box_text_insert_text

    use UR_gtk_variables,     only: Notebook_labeltext, clobj, nclobj
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

    implicit none

    integer(4)                 :: i, kkk, k
    character(len=25)          :: dbox(4)
    character(len=300)         :: str1
    character(len=100)         :: str
    character(len=60)          :: idstr
    character(len=80)          :: str2
    type(c_ptr)                :: widget
    integer(c_int)             :: ic

    call ListstoreTranslate(langg)

    if(langg == 'DE') call WDSetComboboxAct('comboboxLangg',1)
    if(langg == 'EN') call WDSetComboboxAct('comboboxLangg',2)
    if(langg == 'FR') call WDSetComboboxAct('comboboxLangg',3)

    widget = c_null_ptr
    do i=1,6
        select case (i)
        case (1)
            widget = idpt('NBProcedure')
            IF(langg == 'DE') Notebook_labeltext(i) = 'Verfahren'
            IF(langg == 'EN') Notebook_labeltext(i) = 'Procedure'
            IF(langg == 'FR') Notebook_labeltext(i) = 'Procédure'
        case (2)
            widget = idpt('NBEquations')
            IF(langg == 'DE') Notebook_labeltext(i) = 'Gleichungen'
            IF(langg == 'EN') Notebook_labeltext(i) = 'Equations'
            IF(langg == 'FR') Notebook_labeltext(i) = 'Équations'
        case (3)
            widget = idpt('NBValUnc')
            IF(langg == 'DE') Notebook_labeltext(i) = 'Werte, Unsicherheiten'
            IF(langg == 'EN') Notebook_labeltext(i) = 'Values, Uncertainties'
            IF(langg == 'FR') Notebook_labeltext(i) = 'Valeurs, Incertitudes'
        case (4)
            widget = idpt('NBBudget')
            IF(langg == 'DE') Notebook_labeltext(i) = 'Unsicherheitsbudget'
            IF(langg == 'EN') Notebook_labeltext(i) = 'Uncertainty budget'
            IF(langg == 'FR') Notebook_labeltext(i) = 'Budget d''incertitude'
        case (5)
            widget = idpt('NBResults')
            IF(langg == 'DE') Notebook_labeltext(i) = 'Resultate'
            IF(langg == 'EN') Notebook_labeltext(i) = 'Results'
            IF(langg == 'FR') Notebook_labeltext(i) = 'Résultats'
        case (6)
            widget = idpt('NBEditor')
            IF(langg == 'DE') Notebook_labeltext(i) = 'Text Editor'
            IF(langg == 'EN') Notebook_labeltext(i) = 'Text Editor'
            IF(langg == 'FR') Notebook_labeltext(i) = 'Éditeur de texte'
        end select
        call gtk_label_set_text(widget,trim(Notebook_labeltext(i))//c_null_char)
    end do

    IF(langg == 'DE') call WDPutLabelString('CalcCountRates', 'Zaehlraten berechnen')
    IF(langg == 'EN') call WDPutLabelString('CalcCountRates', 'Plot confidence ellipse')
    IF(langg == 'FR') call WDPutLabelString('CalcCountRates', 'Plot confidence ellipse')

    IF(langg == 'DE') call WDPutLabelString('BTOk', 'Anwenden')
    IF(langg == 'EN') call WDPutLabelString('BTOk', 'Apply')
    IF(langg == 'FR') call WDPutLabelString('BTOk', 'Appliquer')

    IF(langg == 'DE') call WDPutLabelString('MenuLoadProject', 'Projekt laden')
    IF(langg == 'EN') call WDPutLabelString('MenuLoadProject', 'Load project')
    IF(langg == 'FR') call WDPutLabelString('MenuLoadProject', 'Charger le projet')

    IF(langg == 'DE') call WDPutLabelString('MenuSaveProject', 'Projekt speichern')
    IF(langg == 'EN') call WDPutLabelString('MenuSaveProject', 'Save project')
    IF(langg == 'FR') call WDPutLabelString('MenuSaveProject', 'Enregistrer projet')

    IF(langg == 'DE') call WDPutLabelString('MenuSaveProjectAs', 'Projekt speichern unter')
    IF(langg == 'EN') call WDPutLabelString('MenuSaveProjectAs', 'Save project as')
    IF(langg == 'FR') call WDPutLabelString('MenuSaveProjectAs', 'Enregistrer projet sous')

    IF(langg == 'DE') call WDPutLabelString('MenuCloseProject', 'Projekt schließen')
    IF(langg == 'EN') call WDPutLabelString('MenuCloseProject', 'Close Project')
    IF(langg == 'FR') call WDPutLabelString('MenuCloseProject', 'Fermer projet')

    ! IF(langg == 'DE') call WDPutLabelString('DLLinModel1', 'Lineares Modell: Y(t) = a1*X1(t) + a2*X2(t) + a3*X3(t)')
    ! IF(langg == 'EN') call WDPutLabelString('DLLinModel1', 'Linear Model: Y(t) = a1*X1(t) + a2*X2(t) + a3*X3(t)')
    ! IF(langg == 'FR') call WDPutLabelString('DLLinModel1', 'Formule linéaire: Y(t) = a1*X1(t) + a2*X2(t) + a3*X3(t)')

    IF(langg == 'DE') call WDPutLabelString('DLLinModel1', 'Fitfunktion: Y(t) = Fitp1*X1(t) + Fitp2*X2(t) + Fitp3*X3(t)')
    IF(langg == 'EN') call WDPutLabelString('DLLinModel1', 'Fitting function: Y(t) = Fitp1*X1(t) + Fitp2*X2(t) + Fitp3*X3(t)')
    IF(langg == 'FR') call WDPutLabelString('DLLinModel1', 'fonction d''ajustement: Y(t) = Fitp1*X1(t) + Fitp2*X2(t) + Fitp3*X3(t)')

    IF(langg == 'DE') call WDPutLabelString('label1', 'Welche Terme fitten?')
    IF(langg == 'EN') call WDPutLabelString('label1', 'Which terms to fit?')
    IF(langg == 'FR') call WDPutLabelString('label1', 'Quels termes pour s''adapter ?')

    IF(langg == 'DE') call WDPutLabelString('label2', 'Fixieren:   Fitp(i) in "Werte, Unsicherheiten" festlegen!')
    IF(langg == 'EN') call WDPutLabelString('label2', 'Fix:   quantify Fitp(i) in "Values, Uncertainties"!')
    IF(langg == 'FR') call WDPutLabelString('label2', 'Fixer:  quantifier Fitp(i) en "Valeurs, Incertitudes"!')

    ! IF(langg == 'DE') call WDPutLabelString('label3', 'a1*X1(t): erste Komponente')
    ! IF(langg == 'EN') call WDPutLabelString('label3', 'a1*X1(t): first component')
    ! IF(langg == 'FR') call WDPutLabelString('label3', 'a1*X1(t): composante premiere')

    ! IF(langg == 'DE') call WDPutLabelString('label4', 'a2*X2(t): zweite Komponente')
    ! IF(langg == 'EN') call WDPutLabelString('label4', 'a2*X2(t): second component')
    ! IF(langg == 'FR') call WDPutLabelString('label4', 'a2*X2(t): deuxième composant')

    ! IF(langg == 'DE') call WDPutLabelString('label5', 'a3*X3(t): dritte Komponente')
    ! IF(langg == 'EN') call WDPutLabelString('label5', 'a3*X3(t): third component')
    ! IF(langg == 'FR') call WDPutLabelString('label5', 'a3*X3(t): troisième composant')

    IF(langg == 'DE') call WDPutLabelString('checkbuttonWFit', 'gewichteten Fit anwenden')
    IF(langg == 'EN') call WDPutLabelString('checkbuttonWFit', 'apply weighted fit')
    IF(langg == 'FR') call WDPutLabelString('checkbuttonWFit', 'utiliser ajustement pondéré')

    IF(langg == 'DE') call WDPutLabelString('checkbuttonCovZR', 'Kovarianzen zw. Nettozählraten verwenden')
    IF(langg == 'EN') call WDPutLabelString('checkbuttonCovZR', 'use covariances betw. net counting rates')
    IF(langg == 'FR') call WDPutLabelString('checkbuttonCovZR', 'utiliser covariances de net TauxCompt.')

    IF(langg == 'DE') call WDPutLabelString('checkbuttonAllm', 'Xi(t) für jede Messung explizit vorgeben?')
    IF(langg == 'EN') call WDPutLabelString('checkbuttonAllm', 'define Xi(t) separately for each measurement?')
    IF(langg == 'FR') call WDPutLabelString('checkbuttonAllm', 'définir Xi(t) separately for each measurement?')

    IF(langg == 'DE') call WDPutLabelString('DMlabelNCH', ' Anzahl nchs der Zählkanäle (A, B, C)')
    IF(langg == 'EN') call WDPutLabelString('DMlabelNCH', ' number nchs of counting channels (A, B, C)')
    IF(langg == 'FR') call WDPutLabelString('DMlabelNCH', ' nombre nchs de canaux de comptage (A, B, C)')

    IF(langg == 'DE') call WDPutLabelString('radiobuttonNLSQ', 'Neyman Chi Quadrat WLS')
    IF(langg == 'EN') call WDPutLabelString('radiobuttonNLSQ', 'Neyman Chi Square WLS')

    IF(langg == 'DE') call WDPutLabelString('radiobuttonPLSQ', 'Pearson Chi Quadrat PLSQ')
    IF(langg == 'EN') call WDPutLabelString('radiobuttonPLSQ', 'Neyman Chi Square PLSQ')

    IF(langg == 'DE') call WDPutLabelString('radiobuttonPMLE', 'Poisson Maximum Likelihood Estimation PMLE')
    IF(langg == 'EN') call WDPutLabelString('radiobuttonPMLE', 'Poisson Maximum Likelihood Estimation PMLE')

    IF(langg == 'DE') call WDPutLabelString('radiobuttonTLSQ', 'Gewicht. total least-squares WTLS')
    IF(langg == 'EN') call WDPutLabelString('radiobuttonTLSQ', 'weighted total least-squares WTLS')

    IF(langg == 'DE') call WDPutLabelString('label3terms', 'Definition der Funktionen X1 bis Xn (n=3*nchs):   '  &
                        // 'Reihenfolge:  wie SQL: ''ORDER BY Zählkanal, Messung, Term-Nr.''')
    IF(langg == 'EN') call WDPutLabelString('label3terms', 'Definition of functions X1 to Xn (n=3*nchs):   '  &
                        // 'Ordering:  like SQL: ''ORDER BY Channel, measurement, term-number''')
    IF(langg == 'FR') call WDPutLabelString('label3terms', 'Définition des fonctions X1 à Xn (n=3*nchs):   '  &
                        // 'Commande: comme SQL: ''ORDER BY Channel, measurement, term-number''')

    IF(langg == 'DE') call WDPutLabelStringBold('labelModeEq', 'Gleichungen der Form  '  &
                        // 'Xi = i-te Funktion Xi(t):')
    IF(langg == 'EN') call WDPutLabelStringBold('labelModeEq', 'Equations of the form  '  &
                        // 'Xi = i-th function Xi(t):')
    IF(langg == 'FR') call WDPutLabelStringBold('labelModeEq', 'Equations de la forme  '  &
                        // 'Xi = i-th fonction Xi(t):')

    IF(langg == 'DE') call WDPutLabelString('LBNetBlindVal', 'Netto-Blindwert-Zählrate:')
    IF(langg == 'EN') call WDPutLabelString('LBNetBlindVal', 'Net blank count rate:')
    IF(langg == 'FR') call WDPutLabelString('LBNetBlindVal', 'Net t.de.comptage (réactif):')

    ! IF(langg == 'DE') call WDPutLabelString('LBSeparation', 'Datum + Uhrzeit der Separation')
    ! IF(langg == 'EN') call WDPutLabelString('LBSeparation', 'Date + Time of separation')
    ! IF(langg == 'FR') call WDPutLabelString('LBSeparation', 'Date + l''heure de séparation')

    IF(langg == 'DE') call WDPutLabelString('checkAbsTime', 'Absolute Zeitangaben? (Datumsformat : 01.01.2015 08:30:15)')
    IF(langg == 'EN') call WDPutLabelString('checkAbsTime', 'Absolute time indication? (Date format : 01.01.2015 08:30:15)')
    IF(langg == 'FR') call WDPutLabelString('checkAbsTime', 'temps absolus ? (Format de la date : 01.01.2015 08:30:15)')

    IF(langg == 'DE') call WDPutLabelString('LBTimeBase', 'Zeitbasis für Messdauer und Zählraten:')
    IF(langg == 'EN') call WDPutLabelString('LBTimeBase', 'time basis for count-time and -rates:')
    IF(langg == 'FR') call WDPutLabelString('LBTimeBase', 'base de temps pour le compte-temps et les taux:')

    ! IF(langg == 'DE') call WDPutLabelString('LBDateFormat', 'Datumsformat : 01.01.2015 08:30:15')
    ! IF(langg == 'EN') call WDPutLabelString('LBDateFormat', 'Date format : 01.01.2015 08:30:15')
    ! IF(langg == 'FR') call WDPutLabelString('LBDateFormat', 'Format de la date : 01.01.2015 08:30:15')

    IF(langg == 'DE') call WDPutLabelString('LBDateFormat', 'Datum + Uhrzeit der Separation')
    IF(langg == 'EN') call WDPutLabelString('LBDateFormat', 'Date + Time of separation')
    IF(langg == 'FR') call WDPutLabelString('LBDateFormat', 'Date + l''heure de séparation')

    IF(langg == 'DE') call WDPutLabelString('CalcCountRates', 'Zählraten berechnen')
    IF(langg == 'EN') call WDPutLabelString('CalcCountRates', 'calculate count rates')
    IF(langg == 'FR') call WDPutLabelString('CalcCountRates', 'calcule taux de comptage')

    IF(langg == 'DE') call WDPutLabelString('Exchange2Symbols', 'Tausche 2 Ergebnisgrößen')
    IF(langg == 'EN') call WDPutLabelString('Exchange2Symbols', 'Exchange 2 symbols')
    IF(langg == 'FR') call WDPutLabelString('Exchange2Symbols', 'Échanger 2 symboles')

    IF(langg == 'DE') call WDPutLabelString('LBSymbExchg', 'Auswahl zweier zu tauschender Ergebnisgößen-Symbole:')
    IF(langg == 'EN') call WDPutLabelString('LBSymbExchg', 'Select two output quantity symbols to be exchanged:')
    IF(langg == 'FR') call WDPutLabelString('LBSymbExchg', 'Sélectionnez deux symboles de grandeur de sortie à échanger:')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview1', 2, 'Symbol')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview1', 2, 'Symbol')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview1', 2, 'Symbole')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview1', 3, 'Typ')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview1', 3, 'Type')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview1', 3, 'Type')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview1', 4, 'Einheit')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview1', 4, 'Unit')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview1', 4, 'Unité')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview1', 5, 'Bedeutung')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview1', 5, 'Meaning')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview1', 5, 'Sense')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 2, 'Symbol')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 2, 'Symbol')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview2', 2, 'Symbole')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 3, 'Typ')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 3, 'Type')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview2', 3, 'Type')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 4, 'Einheit')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 4, 'Unit')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview2', 4, 'Unité')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 5, 'Wert')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 5, 'Value')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview2', 5, 'Valeur')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 6, 'Verteilg')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 6, 'distribut')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview2', 6, 'distribut')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 7, 'StdUnsFormel')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 7, 'StdUncFormula')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview2', 7, 'Formule écart type')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 8, 'StdUnsWert')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 8, 'StdUncValue')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview2', 8, 'ValÉcartType')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 9, 'Halbbreite')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 9, 'Half width')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview2', 9, 'Half width')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview2', 11, 'abs.Std.Uns.')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview2', 11, 'abs.Std.Incert.')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview3', 4, 'Typ')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview3', 4, 'Type')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview3', 4, 'Type')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview3', 5, 'Formel')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview3', 5, 'Formula')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview3', 5, 'Formule')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview3', 6, 'Wert')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview3', 6, 'Value')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview3', 6, 'Valeur')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview4', 3, 'Typ')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview4', 3, 'Type')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview4', 3, 'Type')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview4', 4, 'Einheit')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview4', 4, 'Unit')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview4', 4, 'Unité')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview4', 5, 'Wert')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview4', 5, 'Value')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview4', 5, 'Valeur')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview4', 6, 'StdUnsWert')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview4', 6, 'StdUncValue')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview4', 6, 'ValÉcartType')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview4', 7, 'Sensit.-Koeffizient')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview4', 7, 'Sensit. coefficient')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview4', 7, 'coefficient de sensibilit.')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview4', 8, 'rel. Beitrag(%)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview4', 8, 'rel. contribution(%)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview4', 8, 'rel. contribution(%)')

    if(use_absTimeStart) then
        if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 2, 'Startdatum'//char(13)//'(brutto)')
        if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 2, 'Start date'//char(13)//'(gross)')
        if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 2, 'Date départ '//char(13)//'(brut)')
    else
        if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 2, 'StartDiff (s)'//char(13)//'(brutto)')
        if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 2, 'StartDiff (s)'//char(13)//'(gross)')
        if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 2, 'diff. départ (s)'//char(13)//'(brut)')
    end if

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 3, 'Messdauer'//char(13)//'(brutto)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 3, 'Count time'//char(13)//'(gross)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 3, 'durée comptage'//char(13)//'(brut)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 4, 'Impulse'//char(13)//'(brutto)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 4, 'Counts'//char(13)//'(gross)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 4, 'Coups'//char(13)//'(brut)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 5, 'Zählrate'//char(13)//'(brutto)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 5, 'CountRate'//char(13)//'(gross)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 5, 'TauxCompt.'//char(13)//'(brut)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 6, 'u(Zählrate)'//char(13)//'(brutto)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 6, 'u(CountRate)'//char(13)//'(gross)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 6, 'u(TauxCompt.)'//char(13)//'(brut)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 7, 'Messdauer'//char(13)//'(UG)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 7, 'Count time'//char(13)//'(BG)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 7, 'durée mesure'//char(13)//'(BF)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 8, 'Impulse'//char(13)//'(UG)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 8, 'Counts'//char(13)//'(BG)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 8, 'Coups'//char(13)//'(BF)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 9, 'Zählrate'//char(13)//'(UG)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 9, 'CountRate'//char(13)//'(BG)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 9, 'TauxCompt.'//char(13)//'(BF)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 10, 'u(Zählrate)'//char(13)//'(UG)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 10, 'u(CountRate)'//char(13)//'(BG)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 10, 'u(TauxCompt.)'//char(13)//'(BF)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 11, 'Zählrate'//char(13)//'(netto)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 11, 'CountRate'//char(13)//'(net)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 11, 'TauxCompt.'//char(13)//'(net)')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview5', 12, 'u(Zählrate)'//char(13)//'(netto)')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview5', 12, 'u(CountRate)'//char(13)//'(net)')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview5', 12, 'u(TauxCompt.)'//char(13)//'(net)')


    IF(langg == 'DE') call WDPutLabelString('LBGspk1b2LFactor', 'Faktor (1+b/2L) für Compton-UG-ZRate:')
    IF(langg == 'EN') call WDPutLabelString('LBGspk1b2LFactor', 'Factor (1+b/2L) for Compton-BG-countrate:')
    IF(langg == 'FR') call WDPutLabelString('LBGspk1b2LFactor', 'Facteur (1+b/2L) pour le taux de comptage de fond de Compton:')

    IF(langg == 'DE') call WDPutLabelString('LBGspk1EffiCov', 'Effizienzen-Kovarianzen einbeziehen?')
    IF(langg == 'EN') call WDPutLabelString('LBGspk1EffiCov', 'include efficiency covariances?')
    IF(langg == 'FR') call WDPutLabelString('LBGspk1EffiCov', 'inclure des covariances d''efficacité?')

    IF(langg == 'DE') call WDPutLabelString('LBGspk1MWB', 'Verfahren der Mittelwertberechnung:')
    IF(langg == 'EN') call WDPutLabelString('LBGspk1MWB', 'Method for mean calculation:')
    IF(langg == 'FR') call WDPutLabelString('LBGspk1MWB', 'Méthode de calcul moyen:')

    IF(langg == 'DE') call WDPutLabelString('checkbuttonMeanOpt', 'Mean: externe SD verwenden?')
    IF(langg == 'EN') call WDPutLabelString('checkbuttonMeanOpt', 'Mean: use external SD?')
    IF(langg == 'FR') call WDPutLabelString('checkbuttonMeanOpt', 'Moyenne: utiliser SD externe?')

    IF(langg == 'DE') call WDPutLabelStringBold('LBGspk1FrameZR', 'Zähraten:')
    IF(langg == 'EN') call WDPutLabelStringBold('LBGspk1FrameZR', 'count rates:')
    IF(langg == 'FR') call WDPutLabelStringBold('LBGspk1FrameZR', 'taux des comptages:')

    IF(langg == 'DE') call WDPutLabelString('DKlabelTitel', 'Titel dieser Kalibrierkurve:')
    IF(langg == 'EN') call WDPutLabelString('DKlabelTitel', 'Title of this calibration curve:')
    IF(langg == 'FR') call WDPutLabelString('DKlabelTitel', 'Titre de cette courbe d''étalonnage:')

    IF(langg == 'DE') call WDPutLabelString('DKcheckUfit', 'u(Fit) verwenden?')
    IF(langg == 'EN') call WDPutLabelString('DKcheckUfit', 'use u(Fit)?')
    IF(langg == 'FR') call WDPutLabelString('DKcheckUfit', 'utilser u(Fit)?')

    IF(langg == 'DE') call WDPutLabelString('DKcheckWTLS', 'WTLS verwenden?')
    IF(langg == 'EN') call WDPutLabelString('DKcheckWTLS', 'use WTLS?')
    IF(langg == 'FR') call WDPutLabelString('DKcheckWTLS', 'utilser WTLS?')

    IF(langg == 'DE') call WDPutLabelStringBold('FrLabelKalib', 'Kalibrierkurve:')
    IF(langg == 'EN') call WDPutLabelStringBold('FrLabelKalib', 'Calibration curve:')
    IF(langg == 'FR') call WDPutLabelStringBold('FrLabelKalib', 'courbe d''étalonnage:')

    IF(langg == 'DE') call WDPutLabelString('DoKalibFit', 'Fit ausführen:')
    IF(langg == 'EN') call WDPutLabelString('DoKalibFit', 'run the fit:')
    IF(langg == 'FR') call WDPutLabelString('DoKalibFit', 'exécuter ajustement:')

    IF(langg == 'DE') call WDPutLabelString('DKlabelFparms', 'Fitparamater:')
    IF(langg == 'EN') call WDPutLabelString('DKlabelFparms', 'Fitting paramaters:')
    IF(langg == 'FR') call WDPutLabelString('DKlabelFparms', 'Paramètre d''ajustement:')

    IF(langg == 'DE') call WDPutLabelString('DKlabelFsdev', 'deren StdDevs:')
    IF(langg == 'EN') call WDPutLabelString('DKlabelFsdev', 'their StdDevs:')
    IF(langg == 'FR') call WDPutLabelString('DKlabelFsdev', 'dont écart type:')

    IF(langg == 'DE') call WDPutLabelString('LB1Numegr',  &
                        'Wie viele Ergebnisgrößen sollen gleichzeitig behandelt werden?')
    IF(langg == 'EN') call WDPutLabelString('LB1Numegr', &
                        'How many output quantities shall be used simultaneously?')
    IF(langg == 'FR') call WDPutLabelString('LB1Numegr', &
                        'Combien de quantités de sortie doivent être utilisées simultanément?')

    IF(langg == 'DE') call WDPutLabelString('LB2Numegr',  &
                        'Die dazugehörigen Formel-Symbole müssen als erste der' // char(13) &
                        //'Gleichungen definiert sein (von oben nach unten)')
    IF(langg == 'EN') call WDPutLabelString('LB2Numegr', &
                        'The corresponding formula symbols must be defined as' // char(13) &
                        // 'the first ones in the equations (top down)')
    IF(langg == 'FR') call WDPutLabelString('LB2Numegr', &
                        'La formule correspondante doit être définie comme' // char(13) &
                        // 'les premiers dans les équations (de haut en bas)')

    ! IF(langg == 'DE') call WDPutLabelStringBold('FrameOptSel', 'Selektion:')
    ! IF(langg == 'EN') call WDPutLabelStringBold('FrameOptSel', 'Selection:')
    ! IF(langg == 'FR') call WDPutLabelStringBold('FrameOptSel', 'Sélection:')

    ! IF(langg == 'DE') call WDPutLabelString('DOptionsLoadVals', 'Laden der Werte')
    ! IF(langg == 'EN') call WDPutLabelString('DOptionsLoadVals', 'Load values')
    ! IF(langg == 'FR') call WDPutLabelString('DOptionsLoadVals', 'Charger valeurs')

    IF(langg == 'DE') call WDPutLabelString('DOptionsLoadVals', 'anpassen')
    IF(langg == 'EN') call WDPutLabelString('DOptionsLoadVals', 'adjust')
    IF(langg == 'FR') call WDPutLabelString('DOptionsLoadVals', 'ajuster')

    IF(langg == 'DE') call WDPutLabelStringBold('label74',  &
                        'Quantile       	   	               Fehlerwahrscheinlichkeiten')
    IF(langg == 'EN') call WDPutLabelStringBold('label74',  &
                        'Quantiles      	   	                   Error probabilities')
    IF(langg == 'FR') call WDPutLabelStringBold('label74',  &
                        'Quantiles      	   	                   Probabilités d''erreur')

    IF(langg == 'DE') call WDPutLabelString('LBOptCoverf', 'Erweiterungsfaktor   Ausgabe')
    IF(langg == 'EN') call WDPutLabelString('LBOptCoverf', 'Coverage factor   output')
    IF(langg == 'FR') call WDPutLabelString('LBOptCoverf', 'Facteur de couvertur   sortie')

    IF(langg == 'DE') call WDPutLabelString('LBOptCoverIn', 'Erweiterungsfaktor   Eingabe')
    IF(langg == 'EN') call WDPutLabelString('LBOptCoverIn', 'Coverage factor   input')
    IF(langg == 'FR') call WDPutLabelString('LBOptCoverIn', 'Facteur de couvertur  entrée')

    IF(langg == 'DE') call WDPutLabelString('LBOptDLMethod', 'Nachweisgrenzenmethode')
    IF(langg == 'EN') call WDPutLabelString('LBOptDLMethod', 'Detection limit method')
    IF(langg == 'FR') call WDPutLabelString('LBOptDLMethod', 'Méthode Limite de détection')

    IF(langg == 'DE') str1 = 'Gamma-Verteilung: Wert von GamDistAdd(0; 0.5, 1)' // char(13) // &
                            '      Mittelwert(N) = (N + GamDistAdd)'
    IF(langg == 'EN') str1 = 'Gamma distribution: value of GamDistAdd(0; 0.5, 1)' // char(13) // &
                            '         Mean(N) = (N + GamDistAdd)'
    IF(langg == 'FR') str1 = 'Gamma distribution: valeur GamDistAdd(0; 0.5, 1)' // char(13) // &
                            '     Valeur moyenne(N) = (N + GamDistAdd)'
    call WDPutLabelString('LBOptGamDistAdd', trim(str1))

    IF(langg == 'DE') call WDPutLabelString('LBOpt1minusG', 'Wahrscheinlichkeit Konfidenz-Intervall (1-gamma):')
    IF(langg == 'EN') call WDPutLabelString('LBOpt1minusG', 'Probability for confidence interval (1-gamma):')
    IF(langg == 'FR') call WDPutLabelString('LBOpt1minusG', 'Probabilité pour l''intervalle confiance (1-gamma):')

    IF(langg == 'DE') call WDPutLabelString('labelLangg', 'Sprache:')
    IF(langg == 'EN') call WDPutLabelString('labelLangg', 'language:')
    IF(langg == 'FR') call WDPutLabelString('labelLangg', 'langue:')

    IF(langg == 'DE') call WDPutLabelString('LBListenSeparator', 'Listen-Separator(CSV-Dateien):')
    IF(langg == 'EN') call WDPutLabelString('LBListenSeparator', 'List separator (CSV files):')
    IF(langg == 'FR') call WDPutLabelString('LBListenSeparator', 'Séparateur de liste (CSV fichier):')

    IF(langg == 'DE') call WDPutLabelString('check_contrastmode', 'Kontrastmodus')
    IF(langg == 'EN') call WDPutLabelString('check_contrastmode', 'Contrast mode')
    IF(langg == 'FR') call WDPutLabelString('check_contrastmode', 'Mode contraste')

    IF(langg == 'DE') call WDPutLabelString('LBSymbchg1', 'Selektiere das zu ändernde Symbol:')
    IF(langg == 'EN') call WDPutLabelString('LBSymbchg1', 'Select symbol to be changed:')
    IF(langg == 'FR') call WDPutLabelString('LBSymbchg1', 'Sélecter symbole modifier:')

    IF(langg == 'DE') call WDPutLabelString('LBSymbchg2', 'Eingabe des neuen Namens:')
    IF(langg == 'EN') call WDPutLabelString('LBSymbchg2', 'Enter new name:')
    IF(langg == 'FR') call WDPutLabelString('LBSymbchg2', 'Entrée neuf nom:')

    IF(langg == 'DE') call WDPutLabelString('MenuFile', 'Datei')
    IF(langg == 'EN') call WDPutLabelString('MenuFile', 'File')
    IF(langg == 'FR') call WDPutLabelString('MenuFile', 'Fichier')

    IF(langg == 'DE') call WDPutLabelString('MenuOpenTextFile', 'Öffnen einer Textdatei mit Editor')
    IF(langg == 'EN') call WDPutLabelString('MenuOpenTextFile', 'Open textfile with editor')

    IF(langg == 'DE') call WDPutLabelString('MenuQuitProgram', 'Programm beenden')
    IF(langg == 'EN') call WDPutLabelString('MenuQuitProgram', 'Quit program')
    IF(langg == 'FR') call WDPutLabelString('MenuQuitProgram', 'Quitter programme')

    IF(langg == 'DE') call WDPutLabelString('MenuEdit', 'Bearbeiten')
    IF(langg == 'EN') call WDPutLabelString('MenuEdit', 'Edit')
    IF(langg == 'FR') call WDPutLabelString('MenuEdit', 'Modifier')

    IF(langg == 'DE') call WDPutLabelString('SelQ', 'Selektieren Ergebnisgröße:')
    IF(langg == 'EN') call WDPutLabelString('SelQ', 'Select output quantity:')
    IF(langg == 'FR') call WDPutLabelString('SelQ', 'Sélectionnez la quantité de sortie:')

    IF(langg == 'DE') call WDPutLabelString('QFirst', 'die erste')
    IF(langg == 'EN') call WDPutLabelString('QFirst', 'the first')
    IF(langg == 'FR') call WDPutLabelString('QFirst', 'la premiere')

    IF(langg == 'DE') call WDPutLabelString('QSecond', 'die zweite')
    IF(langg == 'EN') call WDPutLabelString('QSecond', 'the second')
    IF(langg == 'FR') call WDPutLabelString('QSecond', 'la deuxième')

    IF(langg == 'DE') call WDPutLabelString('QThird', 'die dritte')
    IF(langg == 'EN') call WDPutLabelString('QThird', 'the third')
    IF(langg == 'FR') call WDPutLabelString('QThird', 'la troisième')

    IF(langg == 'DE') call WDPutLabelString('NumberOutputQuantities', 'Anzahl der Ergebnisgrößen')
    IF(langg == 'EN') call WDPutLabelString('NumberOutputQuantities', 'number of output quantities')
    IF(langg == 'FR') call WDPutLabelString('NumberOutputQuantities', 'nombre de variables de sortie')

    IF(langg == 'DE') call WDPutLabelString('MenuDecayCurve', 'Zerfallskurve:')
    IF(langg == 'EN') call WDPutLabelString('MenuDecayCurve', 'Decay curve:')
    IF(langg == 'FR') call WDPutLabelString('MenuDecayCurve', 'Courbe de décroissance:')

    IF(langg == 'DE') call WDPutLabelString('FittingModel', 'Modell der Zerfallskurve')
    IF(langg == 'EN') call WDPutLabelString('FittingModel', 'Model of decay curve')
    IF(langg == 'FR') call WDPutLabelString('FittingModel', 'Formule de courbe de décroissance')

    IF(langg == 'DE') call WDPutLabelString('FittingData', 'Dateneingabe')
    IF(langg == 'EN') call WDPutLabelString('FittingData', 'Data input')
    IF(langg == 'FR') call WDPutLabelString('FittingData', 'Entrée de données')

    IF(langg == 'DE') call WDPutLabelString('FittingResult', 'Kurvenfit-Tabelle')
    IF(langg == 'EN') call WDPutLabelString('FittingResult', 'Curve-fit table')
    IF(langg == 'FR') call WDPutLabelString('FittingResult', 'Table de ajustement des courbes')

    IF(langg == 'DE') call WDPutLabelString('DLabelColFill', 'Welche Spalte füllen:')
    IF(langg == 'EN') call WDPutLabelString('DLabelColFill', 'Which column to fill:')
    IF(langg == 'FR') call WDPutLabelString('DLabelColFill', 'Rembourrer quelle colonne:')

    IF(langg == 'DE') call WDPutLabelString('DLabelEnterVal', 'Spaltenwert:')
    IF(langg == 'EN') call WDPutLabelString('DLabelEnterVal', 'Column value:')
    IF(langg == 'FR') call WDPutLabelString('DLabelEnterVal', 'Valeur de colonne:')

    IF(langg == 'DE') call WDPutLabelString('MenuGSpekt1', 'Gammaspektrometrie')
    IF(langg == 'EN') call WDPutLabelString('MenuGSpekt1', 'Gamma spectrometry')
    IF(langg == 'FR') call WDPutLabelString('MenuGSpekt1', 'Spectrométrie gamma')

    IF(langg == 'DE') call WDPutLabelString('Gspk1Edit', 'Gammalinien editieren')
    IF(langg == 'EN') call WDPutLabelString('Gspk1Edit', 'Edit gamma lines')
    IF(langg == 'FR') call WDPutLabelString('Gspk1Edit', 'Modifier gamma lignes')

    IF(langg == 'DE') call WDPutLabelString('Gspk1Mean', 'Mittlere Linien-Aktivität')
    IF(langg == 'EN') call WDPutLabelString('Gspk1Mean', 'Average line activity')
    IF(langg == 'FR') call WDPutLabelString('Gspk1Mean', 'Activité de ligne moyenne')

    IF(langg == 'DE') call WDPutLabelString('KalFit', 'Kalibrierkurve')
    IF(langg == 'EN') call WDPutLabelString('KalFit', 'Calibration curve')
    IF(langg == 'FE') call WDPutLabelString('KalFit', 'Courbe calibre')

    IF(langg == 'DE') call WDPutLabelString('Report', 'Report')
    IF(langg == 'EN') call WDPutLabelString('Report', 'Report')
    IF(langg == 'FR') call WDPutLabelString('Report', 'Rapport')

    IF(langg == 'DE') call WDPutLabelString('RenameQuantity', 'Ändern des Symbol-Namens')
    IF(langg == 'EN') call WDPutLabelString('RenameQuantity', 'Change symbol name')
    IF(langg == 'FR') call WDPutLabelString('RenameQuantity', 'Modifier nam de symbole')

    IF(langg == 'DE') call WDPutLabelString('EQRenameSymb', 'Symbol ändern')
    IF(langg == 'EN') call WDPutLabelString('EQRenameSymb', 'Change symbol')
    IF(langg == 'FR') call WDPutLabelString('EQRenameSymb', 'Changer de symbole')

    IF(langg == 'DE') call WDPutLabelString('MenuOptions', 'Optionen')
    IF(langg == 'EN') call WDPutLabelString('MenuOptions', 'Options')
    IF(langg == 'FR') call WDPutLabelString('MenuOptions', 'Options')

    IF(langg == 'DE') call WDPutLabelString('PreSettings', 'Voreinstellungen')
    IF(langg == 'EN') call WDPutLabelString('PreSettings', 'Pre-settings')
    IF(langg == 'FR') call WDPutLabelString('PreSettings', 'Préférences')

    IF(langg == 'DE') call WDPutLabelString('menuitem21', 'Projekt laden:')
    IF(langg == 'EN') call WDPutLabelString('menuitem21', 'Project load:')
    IF(langg == 'FR') call WDPutLabelString('menuitem21', 'Charger le projet:')

    IF(langg == 'DE') call WDPutLabelString('LoadWithCalc', 'mit  automat. Berechnungen')
    IF(langg == 'EN') call WDPutLabelString('LoadWithCalc', 'with automat. calculations')
    IF(langg == 'FR') call WDPutLabelString('LoadWithCalc', 'avec automat. calcul')

    IF(langg == 'DE') call WDPutLabelString('LoadWithoutCalc', 'ohne automat. Berechnungen')
    IF(langg == 'EN') call WDPutLabelString('LoadWithoutCalc', 'without automat. calculations')
    IF(langg == 'FR') call WDPutLabelString('LoadWithoutCalc', 'sans automat. calcul')

    IF(langg == 'DE') call WDPutLabelString('ExportToR', 'LSQ Export nach R?')
    IF(langg == 'EN') call WDPutLabelString('ExportToR', 'LSQ Export to R?')
    IF(langg == 'FR') call WDPutLabelString('ExportToR', 'LSQ Export vers R?')

    IF(langg == 'DE') call WDPutLabelString('ModelType', 'Modelltype')
    IF(langg == 'EN') call WDPutLabelString('ModelType', 'Model type')
    IF(langg == 'FR') call WDPutLabelString('ModelType', 'Type du Formule')

    IF(langg == 'DE') call WDPutLabelString('ConfidEllipse', 'berechne Konfidenz-Ellipse')
    IF(langg == 'EN') call WDPutLabelString('ConfidEllipse', 'calculate confidence ellipse')
    IF(langg == 'FR') call WDPutLabelString('ConfidEllipse', 'calculer l''ellipse de confiance')

    IF(langg == 'DE') call WDPutLabelString('MT_GUMonly', 'nur GUM, ohne NWG')
    IF(langg == 'EN') call WDPutLabelString('MT_GUMonly', 'only GUM, without DL')
    IF(langg == 'FR') call WDPutLabelString('MT_GUMonly', 'juste GUM, sans DL')

    IF(langg == 'DE') call WDPutLabelString('MT_PosLin', 'posit. linear, mit NWG')
    IF(langg == 'EN') call WDPutLabelString('MT_PosLin', 'posit. linear, with DL')
    IF(langg == 'FR') call WDPutLabelString('MT_PosLin', 'posit. linéaire, avec DL')

    IF(langg == 'DE') call WDPutLabelString('MT_NegLin', 'negat. linear, mit NWG')
    IF(langg == 'EN') call WDPutLabelString('MT_NegLin', 'negat. linear, with DL')
    IF(langg == 'FR') call WDPutLabelString('MT_NegLin', 'negat. linéaire, avec DL')

    IF(langg == 'DE') call WDPutLabelString('ActivateBS', 'Aktiviere BS Modul')
    IF(langg == 'EN') call WDPutLabelString('ActivateBS', 'Activate BS module')
    IF(langg == 'FR') call WDPutLabelString('ActivateBS', 'Activer BS module')

    IF(langg == 'DE') call WDPutLabelString('FontSel', 'Auswahl Schrifttyp')
    IF(langg == 'EN') call WDPutLabelString('FontSel', 'Select font')
    IF(langg == 'FR') call WDPutLabelString('FontSel', 'Sélecter fonte')

    IF(langg == 'DE') call WDPutLabelString('ColorSel', 'Auswahl Farbe')
    IF(langg == 'EN') call WDPutLabelString('ColorSel', 'Select color')
    IF(langg == 'FR') call WDPutLabelString('ColorSel', 'Selecter couleur')

    IF(langg == 'DE') call WDPutLabelString('MenuHelp', '_Hilfe')
    IF(langg == 'EN') call WDPutLabelString('MenuHelp', '_Help')
    IF(langg == 'FR') call WDPutLabelString('MenuHelp', '_Aide')

    IF(langg == 'DE') call WDPutLabelString('Help_UR', 'Hilfe UncertRadio')
    IF(langg == 'EN') call WDPutLabelString('Help_UR', 'Help UncertRadio')
    IF(langg == 'FR') call WDPutLabelString('Help_UR', 'Aide UncertRadio')

    IF(langg == 'DE') call WDPutLabelString('LBNoteProcedure', 'Hier kann eine kurze Beschreibung des Verfahrens erfolgen:')
    IF(langg == 'EN') call WDPutLabelString('LBNoteProcedure', 'Here, a short description of the procedure can be given:')
    IF(langg == 'FR') call WDPutLabelString('LBNoteProcedure', 'Ici, une brève description de la procédure peut être donnée:')

    IF(langg == 'DE') call WDPutLabelString('LBNoteEquations', 'Zeilenweise Eingabe der Gleichungen; ' &
                            // 'die erste Gleichung definiert die Ergebnisgröße; die darin enthaltenen ' &
                            // ' Größen werden mit nachfolgenden Hilfsgleichungen erklärt.')
    IF(langg == 'EN') call WDPutLabelString('LBNoteEquations', 'Input of equations, line by line; '  &
                            // 'the fist equation defines the output quantity by other quantities, ' &
                            // 'which then are defined by auxiliary equations following the first one.')
    IF(langg == 'FR') call WDPutLabelString('LBNoteEquations', 'Entrée d''équations, ligne par ligne; '  &
                            // 'la première équation définit la quantité de sortie par d''autres quantités, ' &
                            // 'qui sont ensuite définis par des équations auxiliaires suivant la première.')

    IF(langg == 'DE') call WDPutLabelStringBold('LBFrameEquations', 'Gleichungen')
    IF(langg == 'EN') call WDPutLabelStringBold('LBFrameEquations', 'Equations')
    IF(langg == 'FR') call WDPutLabelStringBold('LBFrameEquations', 'Équations')

    IF(langg == 'DE') call WDPutLabelString('button_LoadSymbols', 'Laden Symbole(1) aus Gleichungen')
    IF(langg == 'EN') call WDPutLabelString('button_LoadSymbols', 'Load symbols(1) from equations')
    IF(langg == 'FR') call WDPutLabelString('button_LoadSymbols', 'Charger symboles(1) de équations')

    IF(langg == 'DE') call WDPutLabelString('LBComplementUnits', 'Ergänze nachfolgend Einheiten und Bedeutungen')
    IF(langg == 'EN') call WDPutLabelString('LBComplementUnits', 'Complement units and meanings below')
    IF(langg == 'FR') call WDPutLabelString('LBComplementUnits', 'Compléter les unités et les significations ci-dessous')

    IF(langg == 'DE') call WDPutLabelString('EnlargeSymbList', 'Vergrößern der Tabelle')
    IF(langg == 'EN') call WDPutLabelString('EnlargeSymbList', 'Enlarge the table')
    IF(langg == 'FR') call WDPutLabelString('EnlargeSymbList', 'Agrandir la table')

    IF(langg == 'DE') call WDPutLabelStringBold('LBFrameSymbolTable', 'Tabelle der Symbole:')
    IF(langg == 'EN') call WDPutLabelStringBold('LBFrameSymbolTable', 'Table of symbols:')
    IF(langg == 'FR') call WDPutLabelStringBold('LBFrameSymbolTable', 'Tableau de symboles:')

    IF(langg == 'DE') call WDPutLabelString('LoadCompletedSyms', 'Laden Symbole(2) aus der ergänzten Symboltabelle')
    IF(langg == 'EN') call WDPutLabelString('LoadCompletedSyms', 'Load symbols(2) from finalized symbol table')
    IF(langg == 'FR') call WDPutLabelString('LoadCompletedSyms', 'Charger symboles(2) de tableau de symboles finale')

    IF(langg == 'DE') call WDPutLabelString('LBOutpQuantity', 'Selektierte Ergebnisgröße:')
    IF(langg == 'EN') call WDPutLabelString('LBOutpQuantity', 'Selected output quantity:')
    IF(langg == 'FR') call WDPutLabelString('LBOutpQuantity', 'Sélectionner variable de sortie:')

    IF(langg == 'DE') call WDPutLabelString('LBNetRate', 'Netto-Zählrate:')
    IF(langg == 'EN') call WDPutLabelString('LBNetRate', 'net count rate:')
    IF(langg == 'FR') call WDPutLabelString('LBNetRate', 't.d.comptage net:')

    IF(langg == 'DE') call WDPutLabelString('LBGrossRate', 'Brutto-Zählrate:')
    IF(langg == 'EN') call WDPutLabelString('LBGrossRate', 'gross count rate:')
    IF(langg == 'FR') call WDPutLabelString('LBGrossRate', 't.d.comptage brut:')

    IF(langg == 'DE') call WDPutLabelString('AcceptAll', 'Alles übernehmen')
    IF(langg == 'EN') call WDPutLabelString('AcceptAll', 'Accept all')
    IF(langg == 'FR') call WDPutLabelString('AcceptAll', 'Accepter tout')

    IF(langg == 'DE') call WDPutLabelString('LBSel2Rates', 'Selektion Netto- und Brutto-Zählraten-Symbole:')
    IF(langg == 'EN') call WDPutLabelString('LBSel2Rates', 'Select net and gross count rate symbols:')
    IF(langg == 'FR') call WDPutLabelString('LBSel2Rates', 'Sélecter sysmboles de net et brut comptage:')

    IF(langg == 'DE') call WDPutLabelString('LBEditingCell', 'Textzelle für das Editieren einer längeren Formel')
    IF(langg == 'EN') call WDPutLabelString('LBEditingCell', 'Text cell for editing a longer formula')
    IF(langg == 'FR') call WDPutLabelString('LBEditingCell', 'Cellule de texte pour l''édition d''une formule plus longue')

    IF(langg == 'DE') call WDPutLabelString('LabNotesValUnc', 'Achtung: Benutzereingaben sind nur in den grünen ' // &
                                        &'und weißen Zellen erlaubt!')
    IF(langg == 'EN') call WDPutLabelString('LabNotesValUnc', 'Attention: User input is only permitted in the green ' // &
                                        &'and white cells!')
    IF(langg == 'FR') call WDPutLabelString('LabNotesValUnc', 'Attention: Les entrées utilisateur ne sont autorisées ' // &
                                        &'que dans les cellules vertes et blanches!')

    IF(langg == 'DE') call WDPutLabelStringBold('LBFrameValTable', 'Tabelle der Werte, Unsicherheiten:')
    IF(langg == 'EN') call WDPutLabelStringBold('LBFrameValTable', 'Table of values, uncertainties:')
    IF(langg == 'FR') call WDPutLabelStringBold('LBFrameValTable', 'Tableau de valeurs, incertitudes:')

    IF(langg == 'DE') call WDPutLabelStringBold('LBFrameCovars', 'Eingabe von Kovarianzen/Korrelationen:')
    IF(langg == 'EN') call WDPutLabelStringBold('LBFrameCovars', 'Input of covariances/correlations:')
    IF(langg == 'FR') call WDPutLabelStringBold('LBFrameCovars', 'Entrée de covariances/corrélationes:')

    IF(langg == 'DE') call WDPutLabelString('EnlargeValUnc', 'Vergrößern der Tabelle')
    IF(langg == 'EN') call WDPutLabelString('EnlargeValUnc', 'Enlarge table')
    IF(langg == 'FR') call WDPutLabelString('EnlargeValUnc', 'Agrandir la table')

    IF(langg == 'DE') call WDPutLabelString('CalcValUnc', 'Berechnung der Unsicherheiten')
    IF(langg == 'EN') call WDPutLabelString('CalcValUnc', 'Calculation of uncertainties')
    IF(langg == 'FR') call WDPutLabelString('CalcValUnc', 'Calcul des incertitudes')

    IF(langg == 'DE') call WDPutLabelString('EnlargeBudget', 'Vergrößern der Tabelle')
    IF(langg == 'EN') call WDPutLabelString('EnlargeBudget', 'Enlarge table')
    IF(langg == 'FR') call WDPutLabelString('EnlargeBudget', 'Agrandir la table')

    IF(langg == 'DE') call WDPutLabelString('ChangeBudgetType', 'Wechseln des Budget-Typs')
    IF(langg == 'EN') call WDPutLabelString('ChangeBudgetType', 'Change budget type')
    IF(langg == 'FR') call WDPutLabelString('ChangeBudgetType', 'Changer le type de budget')

    IF(langg == 'DE') call WDPutLabelStringBold('LBFrameBudget', 'Tabelle des Unsicherheiten-Budgets:')
    IF(langg == 'EN') call WDPutLabelStringBold('LBFrameBudget', 'Table of uncertainty budget:')
    IF(langg == 'FR') call WDPutLabelStringBold('LBFrameBudget', 'Tableau de budget de incertitudes:')

    IF(langg == 'DE') call WDPutLabelString('TRlabCoverf', 'Erweiterungsfaktor k:')
    IF(langg == 'EN') call WDPutLabelString('TRlabCoverf', 'Coverage factor k:')
    IF(langg == 'FR') call WDPutLabelString('TRlabCoverf', 'facteur de couverture k:')

    IF(langg == 'DE') call WDPutLabelString('TRlabValue', 'Wert der Ergebnisgröße:')
    IF(langg == 'EN') call WDPutLabelString('TRlabValue', 'Value output quantity:')
    IF(langg == 'FR') call WDPutLabelString('TRlabValue', 'Valeur variable de sortie:')

    IF(langg == 'DE') call WDPutLabelString('TRlabUnc', 'erweiterte (Std.-)Unsicherheit:')
    IF(langg == 'EN') call WDPutLabelString('TRlabUnc', 'extendend (Std.-)uncertainty:')
    IF(langg == 'FR') call WDPutLabelString('TRlabUnc', '(Std.-)incertitude élargir:')

    IF(langg == 'DE') call WDPutLabelString('TRlabUncPC', 'relative erw.(Std.-)Unsicherheit:')
    IF(langg == 'EN') call WDPutLabelString('TRlabUncPC', 'relative ext.(Std.-)uncertainty:')
    IF(langg == 'FR') call WDPutLabelString('TRlabUncPC', 'rel. (Std.-)incertitude élargir:')

    IF(langg == 'DE') call WDPutLabelString('TRBayes', 'Beste Schätzwerte nach Bayes:')
    IF(langg == 'EN') call WDPutLabelString('TRBayes', 'Best Bayesian Estimates:')
    IF(langg == 'FR') call WDPutLabelString('TRBayes', 'Meilleur Bayesienne valeur estimée:')

    IF(langg == 'DE') call WDPutLabelString('TRlabValueBy', 'Wert der Ergebnisgröße:')
    IF(langg == 'EN') call WDPutLabelString('TRlabValueBy', 'Value output quantity:')
    IF(langg == 'FR') call WDPutLabelString('TRlabValueBy', 'Valeur de variable de sortie:')

    IF(langg == 'DE') call WDPutLabelString('TRlabUncBy', 'erweiterte (Std.-)Unsicherheit:')
    IF(langg == 'EN') call WDPutLabelString('TRlabUncBy', 'extendend (Std.-)uncertainty:')
    IF(langg == 'FR') call WDPutLabelString('TRlabUncBy', '(Std.-)incertitude élargir:')

    IF(langg == 'DE') call WDPutLabelString('TRlabLQBy', 'untere Bereichsgrenze:')
    IF(langg == 'EN') call WDPutLabelString('TRlabLQBy', 'lower range limit:')
    IF(langg == 'FR') call WDPutLabelString('TRlabLQBy', 'limite inférieure de la plage:')

    IF(langg == 'DE') call WDPutLabelString('TRlabUQBy', 'obere Bereichsgrenze:')
    IF(langg == 'EN') call WDPutLabelString('TRlabUQBy', 'upper range limit:')
    IF(langg == 'FR') call WDPutLabelString('TRlabUQBy', 'limite supérieure de la plage:')

    IF(langg == 'DE') call WDPutLabelString('TRlabGamma', 'Wahrscheinlichkeit (1-gamma):')
    IF(langg == 'EN') call WDPutLabelString('TRlabGamma', 'Probability (1-gamma):')
    IF(langg == 'FR') call WDPutLabelString('TRlabGamma', 'Probabilité (1-gamma):')

    IF(langg == 'DE') call WDPutLabelStringBold('TRLBFrameMessErg', 'Gesamtes Messergebnis:')
    IF(langg == 'EN') call WDPutLabelStringBold('TRLBFrameMessErg', 'Final measurement result:')
    IF(langg == 'FR') call WDPutLabelStringBold('TRLBFrameMessErg', 'Résultat de mesure complet:')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCvalPE', 'primärer Messwert:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCvalPE', 'primary estimate:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCvalPE', 'estimation primaire:')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCvalUPE', 'Unsichh. primärer Messwert:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCvalUPE', 'uncertainty primary estimate:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCvalUPE', 'estimation primaire d''incertitude:')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCval', 'Wert der Ergebnisgröße:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCval', 'Value output quantity:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCval', 'Valeur de variable de sortie:')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCunc', 'erweiterte Unsicherheit:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCunc', 'extendend uncertainty:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCunc', 'incertitude élargir:')

    IF(langg == 'DE') call WDPutLabelString('TRLBMClq', 'untere Bereichsgrenze:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMClq', 'lower range limit:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMClq', 'limite inférieure de la plage:')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCUuq', 'obere Bereichsgrenze:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCUuq', 'upper range limit:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCUuq', 'limite supérieure de la plage:')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCDT', 'Erkennungsgrenze (EKG):')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCDT', 'Decision threshold (DT):')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCDT', 'Seuil de décision (DT):')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCdl', 'Nachweisgrenze (NWG):')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCdl', 'Detection limit (DL):')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCdl', 'Limite de détection (DL):')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCarun', 'aktiver Run:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCarun', 'active run:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCarun', 'course active:')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCAnzM', 'Anzahl der simul. Messungen')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCAnzM', 'Number of simul. measurments')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCAnzM', 'Nombre de mesures simulées')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCAnzR', 'Anzahl der Runs:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCAnzR', 'Number of runs:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCAnzR', 'Nombre de courses:')

    IF(langg == 'DE') call WDPutLabelString('TRcheckbutton2', 'min. Überdeck.-Intervall')
    IF(langg == 'EN') call WDPutLabelString('TRcheckbutton2', 'min. Coverage interval')
    IF(langg == 'FR') call WDPutLabelString('TRcheckbutton2', 'min. Intervalle de couverture')

    IF(langg == 'DE') call WDPutLabelString('TRcheckbutton3', 'min. Überdeck.-Intervall')
    IF(langg == 'EN') call WDPutLabelString('TRcheckbutton3', 'min. Coverage interval')
    IF(langg == 'FR') call WDPutLabelString('TRcheckbutton3', 'min. Intervalle de couverture')

    IF(langg == 'DE') call WDPutLabelString('TRButtonStartMC', 'Start MC')
    IF(langg == 'EN') call WDPutLabelString('TRButtonStartMC', 'Start MC')
    IF(langg == 'FR') call WDPutLabelString('TRButtonStartMC', 'Démarrer MC')

    IF(langg == 'DE') call WDPutLabelStringBold('TRLBFRMCsim', 'Monte Carlo Simulation:')
    IF(langg == 'EN') call WDPutLabelStringBold('TRLBFRMCsim', 'Monte Carlo Simulation:')
    IF(langg == 'FR') call WDPutLabelStringBold('TRLBFRMCsim', 'Monte Carlo Simulation:')

    IF(langg == 'DE') call WDPutLabelString('TRbuttonSavecsv', 'Save to csv')
    IF(langg == 'EN') call WDPutLabelString('TRbuttonSavecsv', 'Save to csv')
    IF(langg == 'FR') call WDPutLabelString('TRbuttonSavecsv', 'Enregistrer en csv')

    IF(langg == 'DE') call WDPutLabelString('TRlabDT', 'Erkennungsgrenze (EKG):')
    IF(langg == 'EN') call WDPutLabelString('TRlabDT', 'Decision threshold (DT):')
    IF(langg == 'FR') call WDPutLabelString('TRlabDT', 'Seuil de décision (DT):')

    IF(langg == 'DE') call WDPutLabelString('TRlabDL', 'Nachweisgrenze (NWG):')
    IF(langg == 'EN') call WDPutLabelString('TRlabDL', 'Detection limit (DL):')
    IF(langg == 'FR') call WDPutLabelString('TRlabDL', 'Limite de détection (DL):')

    IF(langg == 'DE') call WDPutLabelString('TRLBMCuncrel', 'relative erw.(Std.-)Unsicherheit:')
    IF(langg == 'EN') call WDPutLabelString('TRLBMCuncrel', 'relative extd.(Std.-)uncertainty:')
    IF(langg == 'FR') call WDPutLabelString('TRLBMCuncrel', 'rel. (Std.-)incertitude élargir:')

    IF(langg == 'DE') call WDPutLabelString('TRlabMethod', 'Methode: ISO 11929, iterativ')
    IF(langg == 'EN') call WDPutLabelString('TRlabMethod', 'Method: ISO 11929, iteratively')
    IF(langg == 'FR') call WDPutLabelString('TRlabMethod', 'Method: ISO 11929, itérative')

    IF(langg == 'DE') call WDPutLabelStringBold('TRlabFrDL', 'Erkennungs- und Nachweisgrenze:')
    IF(langg == 'EN') call WDPutLabelStringBold('TRlabFrDL', 'Decision thresh. and Detection limit:')
    IF(langg == 'FR') call WDPutLabelStringBold('TRlabFrDL', 'Seuil de décision et imite de détection:')

    IF(langg == 'DE') call WDPutLabelString('TRlabUfit', 'aus LS-Analyse:')
    IF(langg == 'EN') call WDPutLabelString('TRlabUfit', 'from LS analysis:')
    IF(langg == 'FR') call WDPutLabelString('TRlabUfit', 'de l''analyse LS:')

    IF(langg == 'DE') call WDPutLabelString('TRlabUprop', 'aus Unsicherheitsfortpflanzung:')
    IF(langg == 'EN') call WDPutLabelString('TRlabUprop', 'from uncertainty propagation:')
    IF(langg == 'FR') call WDPutLabelString('TRlabUprop', 'de incertitude propagation:')

    IF(langg == 'DE') call WDPutLabelString('TRlabChisqr', 'reduziertes Chi-Quadrat:')
    IF(langg == 'EN') call WDPutLabelString('TRlabChisqr', 'reduced Chi-square:')
    IF(langg == 'FR') call WDPutLabelString('TRlabChisqr', 'Chi-carré réduit:')

    IF(langg == 'DE') call WDPutLabelStringBold('TRlabFRModel', 'LinFit: '  &
                                // 'Standardunsicherheit des Fitparameters ai:')
    IF(langg == 'EN') call WDPutLabelStringBold('TRlabFRModel', 'LinFit: '  &
                                // 'Standard uncertainty of fit parameter ai:')
    IF(langg == 'FR') call WDPutLabelStringBold('TRlabFRModel', 'LinFit: '  &
                                // 'Standard incertitude de fit parameter ai:')

    IF(langg == 'DE') call WDPutLabelString('LBsdMD', 'Stdabw.')
    IF(langg == 'EN') call WDPutLabelString('LBsdMD', 'sd.dev')
    IF(langg == 'FR') call WDPutLabelString('LBsdMD', 'écartType')

    IF(langg == 'DE') call WDPutLabelString('LBnvar0MD', 's0x')
    IF(langg == 'EN') call WDPutLabelString('LBnvar0MD', 's0x')
    IF(langg == 'FR') call WDPutLabelString('LBnvar0MD', 's0x')

    IF(langg == 'DE') call WDPutLabelString('LBMeanMD', 'Mittelw.')
    IF(langg == 'EN') call WDPutLabelString('LBMeanMD', 'Mean')
    IF(langg == 'FR') call WDPutLabelString('LBMeanMD', 'Moyenne')

    IF(langg == 'DE') call WDPutLabelString('labelMDdata', 'sel. Variable:')
    IF(langg == 'EN') call WDPutLabelString('labelMDdata', 'sel. variable:')
    IF(langg == 'FR') call WDPutLabelString('labelMDdata', 'sel. variable:')

    IF(langg == 'DE') call WDPutLabelString('labelMDtyp', 'Mittelwert-Typ:')
    IF(langg == 'EN') call WDPutLabelString('labelMDtyp', 'type of mean:')
    IF(langg == 'FR') call WDPutLabelString('labelMDtyp', 'type de moyenne:')

    IF(langg == 'DE') call WDPutLabelString('MDCalcMean', 'berechne Mittelw.')
    IF(langg == 'EN') call WDPutLabelString('MDCalcMean', 'calculate mean')
    IF(langg == 'FR') call WDPutLabelString('MDCalcMean', 'calcul moyenne')

    IF(langg == 'DE') call WDPutLabelString('FRlabelMD', 'Dateneingabe:')
    IF(langg == 'EN') call WDPutLabelString('FRlabelMD', 'data input:')
    IF(langg == 'FR') call WDPutLabelString('FRlabelMD', 'Entrée de données:')

    if(langg == 'DE') call WDPutTreeViewColumnLabel('treeview8', 2, 'Wert')
    if(langg == 'EN') call WDPutTreeViewColumnLabel('treeview8', 2, 'value')
    if(langg == 'FR') call WDPutTreeViewColumnLabel('treeview8', 2, 'valeur')

    IF(langg == 'DE') call WDPutLabelString('LBrefMD', 'sel. Datensatz für Referenz')
    IF(langg == 'EN') call WDPutLabelString('LBrefMD', 'sel. data record used as reference')
    IF(langg == 'FR') call WDPutLabelString('LBrefMD', 'sél. l''enregistrement de données utilisé comme référence')

    IF(langg == 'DE') call WDPutLabelString('TRLBdtMT', 'Erkennungsgrenze:')
    IF(langg == 'EN') call WDPutLabelString('TRLBdtMT', 'Decision threshold:')
    IF(langg == 'FR') call WDPutLabelString('TRLBdtMT', 'Seuil de décision:')

    IF(langg == 'DE') call WDPutLabelString('TRLBbfMT', 'Bayesfaktor:')
    IF(langg == 'EN') call WDPutLabelString('TRLBbfMT', 'Bsyes factor:')
    IF(langg == 'FR') call WDPutLabelString('TRLBbfMT', 'Facteur de Bayes:')

    if(langg == 'DE') call gtk_window_set_title(idpt('dialog_LoadPro'),  &
                                        'Laden des Projekts:' // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialog_LoadPro'),  &
                                        'Loading the project:' // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialog_LoadPro'),  &
                                        'Charger de projet:' // c_null_char)

    if(langg == 'DE') write(str1,*) 'Das Programm lädt die Projektdatei und führt anschliessend die ' &
                        // 'Berechnungen komplett durch.' // char(13)      &
                        // 'Dieser Vorgang kann einige Sekunden in Anspruch nehmen!' // char(13)   &
                        // 'Bitte warten Sie daher, bis dieser Dialog verschwunden ist!'
    if(langg == 'EN') write(str1,*) 'The program is loading the project file and does all calculations ' &
                        // 'subsequently.' // char(13)      &
                        // 'This may take few seconds!' // char(13)   &
                        // 'Please, wait until this dialog has vanished!'
    if(langg == 'FR') write(str1,*) 'Le programme charge le fichier de projet et fait tous les calculs par la suite. ' &
                        //  char(13)      &
                        // 'Cela peut prendre quelques secondes!' // char(13)   &
                        // 'S''il vous plaît, attendez que ce dialogue disparaisse!'
    call WDPutLabelString('DlabelLoadPro', trim(str1))

    IF(langg == 'DE') call WDPutLabelString('doELIplot', 'Plot Konfidenz-Ellipse:')
    IF(langg == 'EN') call WDPutLabelString('doELIplot', 'plot confidence ellipse:')
    IF(langg == 'FR') call WDPutLabelString('doELIplot', 'tracer l''ellipse de confiance:')

    IF(langg == 'DE') call WDPutLabelString('TEClose', 'Schließen')
    IF(langg == 'EN') call WDPutLabelString('TEClose', 'Close')
    IF(langg == 'FR') call WDPutLabelString('TEClose', 'Fermer')

    ! IF(langg == 'DE') call WDPutLabelString('TESaveTxtAs', 'Report sichern unter')
    ! IF(langg == 'EN') call WDPutLabelString('TESaveTxtAs', 'Save report as')
    ! IF(langg == 'FR') call WDPutLabelString('TESaveTxtAs', 'Enregist. le rapport sous')

    IF(langg == 'DE') call WDPutLabelString('TESavePrjAs', 'Modif. Projekt sichern unter')
    IF(langg == 'EN') call WDPutLabelString('TESavePrjAs', 'Save modified project as')
    IF(langg == 'FR') call WDPutLabelString('TESavePrjAs', 'Enreg. le projet modifié sous')


    IF(langg == 'DE') call WDPutLabelStringBold('LBframeELI', 'Zusammenfassung Egebnisgrößen:')
    IF(langg == 'EN') call WDPutLabelStringBold('LBframeELI', 'Summary of output quantities:')
    IF(langg == 'FR') call WDPutLabelStringBold('LBframeELI', 'Sommaire de variable de sortie:')

    ! if(langg == 'DE') call gtk_window_set_title(idpt('dialogDecayModel'),  &
    !                                      'Modell der Abklingkurve:' // c_null_char)
    ! if(langg == 'EN') call gtk_window_set_title(idpt('dialogDecayModel'),  &
    !                                      'Model of the decay curve:' // c_null_char)
    ! if(langg == 'FR') call gtk_window_set_title(idpt('dialogDecayModel'),  &
    !                                      trim('Formule de courbe de décroissance:') // c_null_char)

    if(langg == 'DE') call gtk_window_set_title(idpt('dialogDecayModel'),  &
                                        trim('Linfit: Vorgaben zur Ausführung der Anpassung:') // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialogDecayModel'),  &
                                        'Linfit: Specifications for running the adjustment:' // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialogDecayModel'),  &
                                        trim('Linfit : Spécifications pour l''exécution de l''ajustement :') // c_null_char)

    if(langg == 'DE') call gtk_window_set_title(idpt('dialog_decayvals'),  &
                                        'Werte der Abklingkurve:' // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialog_decayvals'),  &
                                        'Values of the decay curve:' // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialog_decayvals'),  &
                                        trim('Valeures de courbe de décroissance:') // c_null_char)

    if(langg == 'DE') call gtk_window_set_title(idpt('dialog_gspk1'),  &
                                        trim('Werte der Spektrumsauswertung:') // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialog_gspk1'),  &
                                        trim('Values of the spectrum evaluation:') // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialog_gspk1'),  &
                                        trim('Valeures de spectrum évaluation:') // c_null_char)

    if(langg == 'DE') call gtk_window_set_title(idpt('dialog_kalfit'),  &
                                        trim('Bearbeitung Kalibrierkurve:') // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialog_kalfit'),  &
                                        trim('Calibration curve editing:') // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialog_kalfit'),  &
                                        trim('Modifier de la courbe calibre:') // c_null_char)

    if(langg == 'EN') call gtk_window_set_title(idpt('dialogELI'),   &
                                        trim('Confidence ellipse:')// c_null_char)
    if(langg == 'DE') call gtk_window_set_title(idpt('dialogELI'),  &
                                        trim('Konfidenzellipse:')// c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialogELI'),  &
                                        trim('ellipse de confiance:')// c_null_char)

    if(langg == 'DE') call gtk_window_set_title(idpt('dialogMeanData'),  &
                                        trim('Mittelwerte von Eingangsgrößen:') // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialogMeanData'),  &
                                        trim('Means of input quantities:') // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialogMeanData'),  &
                                        trim('Moyennes de variables d''entrée:') // c_null_char)

    if(langg == 'DE') call WDPutLabelString('SerialEval', 'Serielle Auswertung eines Projekts')
    if(langg == 'EN') call WDPutLabelString('SerialEval', 'Serial evaluation single project')
    if(langg == 'FR') call WDPutLabelString('SerialEval', 'Projet unique d''évaluation en série')

    if(langg == 'DE') call gtk_window_set_title(idpt('dialogSerEval'),  &
                                        trim('Serielle Auswertung:') // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialogSerEval'),  &
                                        trim('Serial evaluation:') // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialogSerEval'),  &
                                        trim('Évaluation en série:') // c_null_char)

    if(langg == 'DE') call gtk_window_set_title(idpt('dialog_distributions'),  &
                                        trim('Verteilungs-Parameter:') // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialog_distributions'),  &
                                        trim('distribution parameters:') // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialog_distributions'),  &
                                        trim('paramètres de distribution:') // c_null_char)

    if(langg == 'DE') call WDPutLabelString('labelCB', 'Farbe für: ')
    if(langg == 'EN') call WDPutLabelString('labelCB', 'Color for: ')
    if(langg == 'FR') call WDPutLabelString('labelCB', 'Coleur pour: ')

    if(langg == 'DE') call WDPutLabelString('radiobutton_bg_color', 'Hintergrund')
    if(langg == 'EN') call WDPutLabelString('radiobutton_bg_color', 'Background')
    if(langg == 'FR') call WDPutLabelString('radiobutton_bg_color', 'arrière-plan')

    if(langg == 'DE') call WDPutLabelString('radiobutton_sel_bg_color', 'Hintergrund-selektiert')
    if(langg == 'EN') call WDPutLabelString('radiobutton_sel_bg_color', 'Background-selected')
    if(langg == 'FR') call WDPutLabelString('radiobutton_sel_bg_color', 'arrière-plan-sélecter')

    if(langg == 'DE') call WDPutLabelString('LBnrecsSE', 'Welche Sätze rechnen:')
    if(langg == 'EN') call WDPutLabelString('LBnrecsSE', 'Which records to calculate:')
    if(langg == 'FR') call WDPutLabelString('LBnrecsSE', 'Quels enregistrements à calculer:')

    if(langg == 'DE') call WDPutLabelString('LBFile1SE', 'Projektdatei (txp)')
    if(langg == 'EN') call WDPutLabelString('LBFile1SE', 'Project file (txp)')
    if(langg == 'FR') call WDPutLabelString('LBFile1SE', 'Dossier de projet (txp)')

    if(langg == 'DE') call WDPutLabelString('LBFile2SE', 'Datei mit variablen Daten (csv)')
    if(langg == 'EN') call WDPutLabelString('LBFile2SE', 'File with variable data (csv)')
    if(langg == 'FR') call WDPutLabelString('LBFile2SE', 'Fichier avec données variables (csv)')

    if(langg == 'DE') call WDPutLabelString('LBFromSE', 'von:')
    if(langg == 'EN') call WDPutLabelString('LBFromSE', 'from:')
    if(langg == 'FR') call WDPutLabelString('LBFromSE', 'de:')

    if(langg == 'DE') call WDPutLabelString('LBToSE', 'bis:')
    if(langg == 'EN') call WDPutLabelString('LBToSE', 'until:')
    if(langg == 'FR') call WDPutLabelString('LBToSE', 'à:')

    if(langg == 'DE') call WDPutLabelString('LBncrunsMCSE', 'Anzahl der Runs')
    if(langg == 'EN') call WDPutLabelString('LBncrunsMCSE', 'Number of runs')
    if(langg == 'FR') call WDPutLabelString('LBncrunsMCSE', 'Nombre de courses')

    if(langg == 'DE') call WDPutLabelString('CheckMCSE', 'MC-Simulation?')
    if(langg == 'EN') call WDPutLabelString('CheckMCSE', 'MC-Simulation?')
    if(langg == 'FR') call WDPutLabelString('CheckMCSE', 'MC-Simulation?')

    if(langg == 'DE') call WDPutLabelString('LBncmaxMCSE', 'Anzahl der simul. Messungen')
    if(langg == 'EN') call WDPutLabelString('LBncmaxMCSE', 'Number of simul. measurements')
    if(langg == 'FR') call WDPutLabelString('LBncmaxMCSE', 'Nombre de mesures simulées')

    if(langg == 'DE') call WDPutLabelString('LBnrecsBEV', 'Welche Sätze rechnen:')
    if(langg == 'EN') call WDPutLabelString('LBnrecsBEV', 'Which records to calculate:')
    if(langg == 'FR') call WDPutLabelString('LBnrecsBEV', 'Quels enregistrements à calculer:')

    if(langg == 'DE') call WDPutLabelString('LBFromBEV', 'von:')
    if(langg == 'EN') call WDPutLabelString('LBFromBEV', 'from:')
    if(langg == 'FR') call WDPutLabelString('LBFromBEV', 'de:')

    if(langg == 'DE') call WDPutLabelString('LBToBEV', 'bis:')
    if(langg == 'EN') call WDPutLabelString('LBToBEV', 'until:')
    if(langg == 'FR') call WDPutLabelString('LBToBEV', 'à:')

    if(langg == 'DE') call WDPutLabelString('LBncrunsMCBEV', 'Anzahl der Runs')
    if(langg == 'EN') call WDPutLabelString('LBncrunsMCBEV', 'Number of runs')
    if(langg == 'FR') call WDPutLabelString('LBncrunsMCBEV', 'Nombre de courses')

    if(langg == 'DE') call WDPutLabelString('CheckMCBEV', 'MC-Simulation?')
    if(langg == 'EN') call WDPutLabelString('CheckMCBEV', 'MC-Simulation?')
    if(langg == 'FR') call WDPutLabelString('CheckMCBEV', 'MC-Simulation?')

    if(langg == 'DE') call WDPutLabelString('LBncmaxMCBEV', 'Anzahl der simul. Messungen')
    if(langg == 'EN') call WDPutLabelString('LBncmaxMCBEV', 'Number of simul. measurements')
    if(langg == 'FR') call WDPutLabelString('LBncmaxMCBEV', 'Nombre de mesures simulées')

    IF(langg == 'DE') call WDPutLabelString('BatestUser', 'QC-Batch-Test')
    IF(langg == 'EN') call WDPutLabelString('BatestUser', 'QC batch test')
    IF(langg == 'FR') call WDPutLabelString('BatestUser', 'QC test de lot')

    if(langg == 'DE') call gtk_window_set_title(idpt('dialog_Batest'),  &
                                        trim('Selbsttest:') // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialog_Batest'),  &
                                        trim('Self-testing:') // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialog_Batest'),  &
                                        trim('Auto-test:') // c_null_char)

    if(langg == 'DE') call WDPutLabelString('BT_label1', 'Datei mit Referenzwerten')
    if(langg == 'EN') call WDPutLabelString('BT_label1', 'File with reference values')
    if(langg == 'FR') call WDPutLabelString('BT_label1', 'Fichier avec valeurs de référence')

    if(langg == 'DE') call WDPutLabelString('BT_label2', 'Ausgabedatei')
    if(langg == 'EN') call WDPutLabelString('BT_label2', 'Output file')
    if(langg == 'FR') call WDPutLabelString('BT_label2', 'Fichier de sortie')

    if(langg == 'DE') call gtk_window_set_title(idpt('dialog_BinPoi'),  &
                                        trim('Binom+Poisson - Parameter:') // c_null_char)
    if(langg == 'EN') call gtk_window_set_title(idpt('dialog_BinPoi'),  &
                                        trim('Binom+Poisson - parameters:') // c_null_char)
    if(langg == 'FR') call gtk_window_set_title(idpt('dialog_BinPoi'),  &
                                        trim('Binom+Poisson - paramètre:') // c_null_char)

    if(langg == 'DE') call WDPutLabelString('BinPoiPars', 'Binomial/Poisson-Fall setzen')
    if(langg == 'EN') call WDPutLabelString('BinPoiPars', 'Set binomial/poisson case')
    if(langg == 'FR') call WDPutLabelString('BinPoiPars', 'Définir un cas binomial/poisson')

    if(langg == 'DE') call WDPutLabelString('LBBinPoi1', 'Selektiere Symbole für (binomial + poisson)')
    if(langg == 'EN') call WDPutLabelString('LBBinPoi1', 'Select symbols for (binomial + poisson)')
    if(langg == 'FR') call WDPutLabelString('LBBinPoi1', 'Sélectionnez les symboles pour (binôme + poisson)')

    if(langg == 'DE') call WDPutLabelString('LBBinPoi2', 'Parameter p der BinomPDF')
    if(langg == 'EN') call WDPutLabelString('LBBinPoi2', 'Parameter p of BinomPDF')
    if(langg == 'FR') call WDPutLabelString('LBBinPoi2', 'Paramètre p de BinomPDF')

    if(langg == 'DE') call WDPutLabelString('LBBinPoi3', 'Nulleffektzählrate')
    if(langg == 'EN') call WDPutLabelString('LBBinPoi3', 'background count rate')
    if(langg == 'FR') call WDPutLabelString('LBBinPoi3', 'Taux de comptage de fond')

    if(langg == 'DE') call WDPutLabelString('LBBinPoi4', 'Messdauer der Probe')
    if(langg == 'EN') call WDPutLabelString('LBBinPoi4', 'Sample count time')
    if(langg == 'FR') call WDPutLabelString('LBBinPoi4', 'Temps de comptage d''échantillon')

    if(langg == 'DE') call WDPutLabelString('LBBinPoi5', 'Zerfallskonstante')
    if(langg == 'EN') call WDPutLabelString('LBBinPoi5', 'decay constant')
    if(langg == 'FR') call WDPutLabelString('LBBinPoi5', 'constante de décomposition')

    if(langg == 'DE') call WDPutLabelString('BatFiles', 'Batch-Auswertung Projekte')
    if(langg == 'EN') call WDPutLabelString('BatFiles', 'Batch evaluation of projects')
    if(langg == 'FR') call WDPutLabelString('BatFiles', 'Évaluation par lots de projets')

    if(langg == 'DE') call WDPutLabelString('DistribLB3', 'Parameterwerte:')
    if(langg == 'EN') call WDPutLabelString('DistribLB3', 'Parameter values:')
    if(langg == 'FR') call WDPutLabelString('DistribLB3', 'Valeurs des paramètres:')

    if(langg == 'DE') call WDPutLabelString('CheckUnits', 'physikal. Einheiten testen')
    if(langg == 'EN') call WDPutLabelString('CheckUnits', 'test physical units')
    if(langg == 'FR') call WDPutLabelString('CheckUnits', 'tester les unités physiques')

    if(langg == 'DE') call WDPutLabelString('HelpFunctions', 'Funktionen')
    if(langg == 'EN') call WDPutLabelString('HelpFunctions', 'functions')
    if(langg == 'FR') call WDPutLabelString('HelpFunctions', 'les fonctions')

    if(langg == 'DE') call WDPutLabelString('LBInfoFX', 'Infos zu speziellen UR-Funktionen')
    if(langg == 'EN') call WDPutLabelString('LBInfoFX', 'Infos about special UR functions')
    if(langg == 'FR') call WDPutLabelString('LBInfoFX', 'Infos sur les fonctions spéciales UR')

    if(.false.) then
        !  mcmc-bezogen:
        IF(langg == 'DE') call WDPutLabelString('TRButtonStartMC1', 'Start MCMC')
        IF(langg == 'EN') call WDPutLabelString('TRButtonStartMC1', 'Start MCMC')
        IF(langg == 'FR') call WDPutLabelString('TRButtonStartMC1', 'Démarrer MCMC')

        IF(langg == 'DE') call WDPutLabelStringBold('TRLBFRMCsim1', 'Bayes MCMC:')
        IF(langg == 'EN') call WDPutLabelStringBold('TRLBFRMCsim1', 'Baysian MCMC:')
        IF(langg == 'FR') call WDPutLabelStringBold('TRLBFRMCsim1', 'MCMC bayésien:')

        IF(langg == 'DE') call WDPutLabelString('TRLBaccRat', 'Akzept. Verhältnis:')
        IF(langg == 'EN') call WDPutLabelString('TRLBaccRat', 'Accept. ratio:')
        IF(langg == 'FR') call WDPutLabelString('TRLBaccRat', 'taux d''accept:')

        IF(langg == 'DE') call WDPutLabelString('TRLBMCAnzM1', 'simul. Messungen')
        IF(langg == 'EN') call WDPutLabelString('TRLBMCAnzM1', 'simul. measurments')
        IF(langg == 'FR') call WDPutLabelString('TRLBMCAnzM1', 'simul. mesurages')

        IF(langg == 'DE') call WDPutLabelString('TRLBMCAnzR1', 'Anzahl der Runs:')
        IF(langg == 'EN') call WDPutLabelString('TRLBMCAnzR1', 'Number of runs:')
        IF(langg == 'FR') call WDPutLabelString('TRLBMCAnzR1', 'Nombre de courses:')

        IF(langg == 'DE') call WDPutLabelString('TRcheckbutton4', 'min. Überdekckungs-Intervall')
        IF(langg == 'EN') call WDPutLabelString('TRcheckbutton4', 'min. Coverage interval')
        IF(langg == 'FR') call WDPutLabelString('TRcheckbutton4', 'min. Intervalle de couverture')

        IF(langg == 'DE') call WDPutLabelString('TRLBMCarun1', 'aktiver Run:')
        IF(langg == 'EN') call WDPutLabelString('TRLBMCarun1', 'active run:')
        IF(langg == 'FR') call WDPutLabelString('TRLBMCarun1', 'course active')

        if(langg == 'DE') call WDPutLabelString('LBncrunsMCMCSE', 'Anzahl der Runs')
        if(langg == 'EN') call WDPutLabelString('LBncrunsMCMCSE', 'Number of runs')
        if(langg == 'FR') call WDPutLabelString('LBncrunsMCMCSE', 'Nombre de courses')

        if(langg == 'DE') call WDPutLabelString('CheckMCMCSE', 'MCMC-Simulation?')
        if(langg == 'EN') call WDPutLabelString('CheckMCMCSE', 'MCMC-Simulation?')
        if(langg == 'FR') call WDPutLabelString('CheckMCMCSE', 'MCMC-Simulation?')

        if(langg == 'DE') call WDPutLabelString('LBFRMCMCSE', 'MCMC:')
        if(langg == 'EN') call WDPutLabelString('LBFRMCMCSE', 'MCMC:')
        if(langg == 'FR') call WDPutLabelString('LBFRMCMCSE', 'MCMC:')

        if(langg == 'DE') call WDPutLabelString('LBncmaxMCMCSE', 'Anzahl der simul. Messungen')
        if(langg == 'EN') call WDPutLabelString('LBncmaxMCMCSE', 'Number of simul. measurements')
        if(langg == 'FR') call WDPutLabelString('LBncmaxMCMCSE', 'Nombre de mesures simulées')

        if(langg == 'DE') call WDPutLabelString('CheckMCMCBEV', 'MCMC-Simulation?')
        if(langg == 'EN') call WDPutLabelString('CheckMCMCBEV', 'MCMC-Simulation?')
        if(langg == 'FR') call WDPutLabelString('CheckMCMCBEV', 'MCMC-Simulation?')

        if(langg == 'DE') call WDPutLabelString('LBFRMCMCBEV', 'MCMC:')
        if(langg == 'EN') call WDPutLabelString('LBFRMCMCBEV', 'MCMC:')
        if(langg == 'FR') call WDPutLabelString('LBFRMCMCBEV', 'MCMC:')

        if(langg == 'DE') call WDPutLabelString('LBncrunsMCMCBEV', 'Anzahl der Runs')
        if(langg == 'EN') call WDPutLabelString('LBncrunsMCMCBEV', 'Number of runs')
        if(langg == 'FR') call WDPutLabelString('LBncrunsMCMCBEV', 'Nombre de courses')

        if(langg == 'DE') call WDPutLabelString('LBncmaxMCMCBEV', 'Anzahl der simul. Messungen')
        if(langg == 'EN') call WDPutLabelString('LBncmaxMCMCBEV', 'Number of simul. measurements')
        if(langg == 'FR') call WDPutLabelString('LBncmaxMCMCBEV', 'Nombre de mesures simulées')

        call SetTooltipText('TRentryMCanzM1', '<= 1.0 E06')
        call SetTooltipText('TRentryMCanzR1', '<= 50')

        if(langg == 'DE') then
            call SetTooltipText('TRcheckbuttonBMT','schätze Erkennungsgrenze mit Bayes-Modell-Test')
        elseif(langg == 'EN') then
            call SetTooltipText('TRcheckbuttonBMT','estimate decision threshold by Bayesian Model-testing')
        elseif(langg == 'FR') then
            call SetTooltipText('TRcheckbuttonBMT','estimation du seuil de décision par essai bayésien')
        end if
    end if

    call WDGetComboboxAct('comboboxtextdcol',k)
    do ic=4_c_int,0_c_int,-1
        call gtk_combo_box_text_remove(idpt('comboboxtextdcol'), ic)
    enddo
    if(langg == 'DE') then
        dbox(1) = 'Messdauer (br)'
        dbox(2) = 'Impulse (br)'
        dbox(3) = 'Messdauer (UG)'
        dbox(4) = 'Impulse (UG)'
    elseif(langg == 'EN') then
        dbox(1) = 'Count time (gr)'
        dbox(2) = 'Counts (gr)'
        dbox(3) = 'Count time (BG)'
        dbox(4) = 'Counts (BG)'
    elseif(langg == 'FR') then
        dbox(1) = 'Durée compt. (br)'
        dbox(2) = 'Coups (br)'
        dbox(3) = 'Durée mesure (BF)'
        dbox(4) = 'Coups (BF)'
    end if
    do i=1,4
        ic = i - 1_c_int
        call gtk_combo_box_text_insert_text(idpt('comboboxtextdcol'), ic,trim(dbox(i))//c_null_char)

    end do
    call WDSetComboboxAct('comboboxtextdcol',k)

    !------------------------------------------------------------
    ! tooltips for toolbar icons:

    IF(langg == 'DE') call SetTooltipText('LBFrameEquations', 'Beispiel: '// char(13) &
                            // 'AktKonz = Rnet * KalFakt' // char(13) &
                            // 'Rnet = Rbrutto - Rnull' // char(13) &
                            // 'Rbrutto = Nbrutto / tmess' // char(13) &
                            // 'Rnull = Nnull / tnull' )
    IF(langg == 'EN') call SetTooltipText('LBFrameEquations', 'Example: '// char(13) &
                            // 'ActConc = Rnet * CalFact' // char(13) &
                            // 'Rnet = Rgross - Rbackgrd' // char(13) &
                            // 'Rgross = Ngross / tmeas' // char(13) &
                            // 'Rbackgrd = Nbackgrd / tbg' )
    IF(langg == 'FR') call SetTooltipText('LBFrameEquations', 'Example: '// char(13) &
                            // 'ActConc = Rnet * CalFact' // char(13) &
                            // 'Rnet = Rbrut - Rbrdefont' // char(13) &
                            // 'Rbrut = Nbrut / tmeas' // char(13) &
                            // 'Rbrdefont = Nbrdefont / tbg' )

    if(langg == 'DE') call SetTooltipText('Report', 'Zusammenfassung der Ergebnisse')
    if(langg == 'EN') call SetTooltipText('Report', 'Summary of results')
    if(langg == 'FR') call SetTooltipText('Report', 'Sommaire de résultats')

    if(langg == 'DE') call SetTooltipText('RenameQuantity', 'Ersetzung eines Symbolnamens im gesamten Projekt')
    if(langg == 'EN') call SetTooltipText('RenameQuantity', 'Replace a symbol name within the whole project')
    if(langg == 'FR') call SetTooltipText('RenameQuantity', 'Remplacer un nom de symbole dans l''ensemble du projet')

    if(langg == 'DE') call SetTooltipText('EQRenameSymb', 'Ersetzung eines Symbolnamens im gesamten Projekt')
    if(langg == 'EN') call SetTooltipText('EQRenameSymb', 'Replace a symbol name within the whole project')
    if(langg == 'FR') call SetTooltipText('EQRenameSymb', 'Remplacer un nom de symbole dans l''ensemble du projet')

    if(langg == 'DE') call SetTooltipText('MT_GUMonly', &
                        'Vereinfachtes Verfahren:' // char(13) // &
                        'Ohne Berechnung von Erkennungs- und Nachweisgrenze')
    if(langg == 'EN') call SetTooltipText('MT_GUMonly', &
                        'Simplified procedure:' // char(13) // &
                        'Without calculating decision threshold and detection limit')
    if(langg == 'FR') call SetTooltipText('MT_GUMonly', &
                        'Procédure simplifiée:' // char(13) // &
                        'Sans calcul du seuil de décision et de la limite de détection')

    if(langg == 'DE') call SetTooltipText('MT_PosLin', &
                        'Normales Verfahren, Aktivität pos. linear zur ZRate :' // char(13) // &
                        'Mit Berechnung von Erkennungs- und Nachweisgrenze')
    if(langg == 'EN') call SetTooltipText('MT_PosLin', &
                        'Normal procedure, Activity pos. linear to CRrate :' // char(13) // &
                        'With calculating decision threshold and detection limit')
    if(langg == 'FR') call SetTooltipText('MT_PosLin', &
                        'Procédure normale, activité pos. linéaire à CReate :' // char(13) // &
                        'Avec calcul du seuil de décision et de la limite de détection')

    if(langg == 'DE') call SetTooltipText('MT_NegLin', &
                        'Anderes Verfahren, OUT neg. linear zu IN :' // char(13) // &
                        'Mit Berechnung von Erkennungs- und Nachweisgrenze')
    if(langg == 'EN') call SetTooltipText('MT_NegLin', &
                        'Other procedure, OUT neg. linear to IN :' // char(13) // &
                        'With calculating decision threshold and detection limit')
    if(langg == 'FR') call SetTooltipText('MT_NegLin', &
                        'Autre procédure, "out" négatif linéaire à "in":' // char(13) // &
                        'Avec calcul du seuil de décision et de la limite de détection')

    if(langg == 'DE') call SetTooltipText('ConfidEllipse', 'Zusammenfassung und Plot der Konfidenze-Ellipse')
    if(langg == 'EN') call SetTooltipText('ConfidEllipse', 'Summary and plot of confidence ellipse')
    if(langg == 'FR') call SetTooltipText('ConfidEllipse', 'Résumé et tracé de l''ellipse de confiance')

    if(langg == 'DE') call SetTooltipText('LoadWithCalc', &
                        'Beim Laden der Projektdatei werden schon alle Berechnungen durchgeführt')
    if(langg == 'EN') call SetTooltipText('LoadWithCalc', &
                        'Loading the project file includes all calculations')
    if(langg == 'FR') call SetTooltipText('LoadWithCalc', &
                        'Le chargement du fichier de projet inclut tous les calculs')

    if(langg == 'DE') call SetTooltipText('LoadWithoutCalc', &
                        'Laden der Projektdatei erfolgt in einzelnen Schritten des Anwenders durch die TABs')
    if(langg == 'EN') call SetTooltipText('LoadWithoutCalc', &
                        'Loading the project file is performed in single user steps through the TABs')
    if(langg == 'FR') call SetTooltipText('LoadWithoutCalc', &
                        'Le chargement du fichier de projet est effectué en une seule étape par l''intermédiaire des TAB')

    if(langg == 'DE') call SetTooltipText('statusbar4', 'Hinweise zum nächsten Schritt des Anwenders')
    if(langg == 'EN') call SetTooltipText('statusbar4', 'Hints for the next step to be taken by the user')
    if(langg == 'FR') call SetTooltipText('statusbar4', 'Conseils pour la prochaine étape à prendre par l''utilisateur')

    if(langg == 'DE') call SetTooltipText('TBLoadProject', 'Laden Projektdatei')
    if(langg == 'EN') call SetTooltipText('TBLoadProject', 'Load project file')
    if(langg == 'FR') call SetTooltipText('TBLoadProject', 'Charger projet fichier')

    if(langg == 'DE') call SetTooltipText('TBSaveProject', 'Sichern Projektdatei')
    if(langg == 'EN') call SetTooltipText('TBSaveProject', 'Save project file')
    if(langg == 'FR') call SetTooltipText('TBSaveProject', 'Enregistrer fichier de projet')

    if(langg == 'DE') call SetTooltipText('TBSaveProjectAs', 'Sichern Projektdatei als')
    if(langg == 'EN') call SetTooltipText('TBSaveProjectAs', 'Save project file as')
    if(langg == 'FR') call SetTooltipText('TBSaveProjectAs', 'Enregistrer fichier projet sous')

    if(langg == 'DE') call SetTooltipText('TBCloseProject', 'Schließen der Projektdatei')
    if(langg == 'EN') call SetTooltipText('TBCloseProject', 'Close project file')
    if(langg == 'FR') call SetTooltipText('TBCloseProject', 'Fermer fichier de projet')

    if(langg == 'DE') call SetTooltipText('TBRefreshCalc', 'Berechnungen für aktuelle Ergebnisgröße aktualisieren')
    if(langg == 'EN') call SetTooltipText('TBRefreshCalc', 'Update calculations for current output quantity')
    if(langg == 'FR') call SetTooltipText('TBRefreshCalc', 'Mettre à jour les calculs pour la quantité de sortie actuelle')

    if(langg == 'DE') call SetTooltipText('TBRemoveGridLine', 'Tabelle/Grid-Zeile(n) löschen')
    if(langg == 'EN') call SetTooltipText('TBRemoveGridLine', 'Delete table/grid line(s)')
    if(langg == 'FR') call SetTooltipText('TBRemoveGridLine', 'Supprimer la ou les lignes de table')

    if(langg == 'DE') call SetTooltipText('TBModelDialog', 'Kurvenfit-Modell editieren')
    if(langg == 'EN') call SetTooltipText('TBModelDialog', 'Edit curve-fitting model')
    if(langg == 'FR') call SetTooltipText('TBModelDialog', 'Modifier le modèle d''ajustement de courbe')

    if(langg == 'DE') call SetTooltipText('TBInputDialog', 'Kurvenfit-Eingangswerte editieren')
    if(langg == 'EN') call SetTooltipText('TBInputDialog', 'Edit Curve-fit input values')
    if(langg == 'FR') call SetTooltipText('TBInputDialog', 'Modifier les valeurs d''entrée Curve-fit')

    if(langg == 'DE') call SetTooltipText('TBFittingResult', 'Anzeige der Fit-Resultate')
    if(langg == 'EN') call SetTooltipText('TBFittingResult', 'View of fitting results')
    if(langg == 'FR') call SetTooltipText('TBFittingResult', 'Vue des résultats d''ajustement')

    if(langg == 'DE') call SetTooltipText('TBInfoDialog', 'CHM-Hilfe')
    if(langg == 'EN') call SetTooltipText('TBInfoDialog', 'CHM help')
    if(langg == 'FR') call SetTooltipText('TBInfoDialog', 'CHM aide')

    if(langg == 'DE') call SetTooltipText('TBProblems', 'Erste Hilfe Tipps')
    if(langg == 'EN') call SetTooltipText('TBProblems', 'First aid hints')
    if(langg == 'FR') call SetTooltipText('TBProblems', 'Conseils de premiers secours')

    if(langg == 'DE') call SetTooltipText('TBFontSel', 'Dialog - Auswahl des Schrifttyps')
    if(langg == 'EN') call SetTooltipText('TBFontSel', 'Font selection dialog')
    if(langg == 'FR') call SetTooltipText('TBFontSel', 'Boîte de dialogue: sélection de police')

    if(langg == 'DE') call SetTooltipText('TBColorSel', 'Dialog - Auswahl Hintergrund-Farben')
    if(langg == 'EN') call SetTooltipText('TBColorSel', 'Background colors selection dialog')
    if(langg == 'FR') call SetTooltipText('TBColorSel', &
                                'Boîte de dialogue: sélection de couleur d''arrière-plan')

    if(langg == 'DE') call SetTooltipText('copyBS1', 'Grafik in Datei kopieren; Format links selektierbar')
    if(langg == 'EN') call SetTooltipText('copyBS1', 'Copy graph into file; format selectable to the left')
    if(langg == 'FR') call SetTooltipText('copyBS1', 'Copiez le graphique dans le fichier; format sélectionnable à gauch')

    if(langg == 'DE') call SetTooltipText('comboboxBS1', 'Selektieren des Grafik-Ausgabe-Formats')
    if(langg == 'EN') call SetTooltipText('comboboxBS1', 'Selection of graphical output format')
    if(langg == 'FR') call SetTooltipText('comboboxBS1', 'Sélection du format de sortie graphique')

    if(langg == 'DE') call SetTooltipText('CopyGrELI', 'Grafik in Datei kopieren; Format links selektierbar')
    if(langg == 'EN') call SetTooltipText('CopyGrELI', 'Copy graph into file; format selectable to the left')
    if(langg == 'FR') call SetTooltipText('CopyGrELI', 'Copiez le graphique dans le fichier; format sélectionnable à gauch')

    if(langg == 'DE') call SetTooltipText('comboboxGrELI', 'Selektieren des Grafik-Ausgabe-Formats')
    if(langg == 'EN') call SetTooltipText('comboboxGrELI', 'Selection of graphical output format')
    if(langg == 'FR') call SetTooltipText('comboboxGrELI', 'Sélection du format de sortie graphique')

    if(langg == 'DE') call SetTooltipText('Exchange2Symbols', 'Vertauschen zweier Ergebnisgrößen, Gleichungen einbezogen')
    if(langg == 'EN') call SetTooltipText('Exchange2Symbols', 'Exchange of two output quantities, including equations')
    if(langg == 'FR') call SetTooltipText('Exchange2Symbols', 'Echange de deux quantités de sortie, y compris les équations')

    if(langg == 'DE') call SetTooltipText('comboboxSymbchg',  &
                        'Die Symbolliste ist erst nach dem Laden der ergänzten Symbol-Tabelle komplett')
    if(langg == 'EN') call SetTooltipText('comboboxSymbchg',  &
                        'The symbol list is complete only after having loaded the finalized symbol table')
    if(langg == 'FR') call SetTooltipText('comboboxSymbchg',  &
                        'La liste des symboles n''est complète qu''après avoir chargé la table des symboles finalisée')

    if(langg == 'DE') call SetTooltipText('comboboxSymbExchgA', &
                        'Die Symbolliste ist erst aktuell nach dem Laden der ergänzten Symbol-Tabelle')
    if(langg == 'EN') call SetTooltipText('comboboxSymbExchgA', &
                        'The symbol list is actual only after having loaded the finalized symbol table')
    if(langg == 'FR') call SetTooltipText('comboboxSymbExchgA', &
                        'La liste des symboles n''est complète qu''après avoir chargé la table des symboles finalisée')

    call SetTooltipText('TRentryMCanzM', '<= 2 E06')
    call SetTooltipText('TRentryMCanzR', '<= 50')

    if(langg == 'DE') call SetTooltipText('dialog-vbox20', 'derzeit können Farben nicht übernommen werden')
    if(langg == 'EN') call SetTooltipText('dialog-vbox20', 'at present, colors cannot be be adapted')
    if(langg == 'FR') call SetTooltipText('dialog-vbox20', 'actuellement, les couleurs ne peuvent pas être adaptées')

    if(langg == 'DE') call SetTooltipText('TBmeansMD', 'Dialog für Variablen-Mittelung')
    if(langg == 'EN') call SetTooltipText('TBmeansMD', 'Dialog for variable-averages')
    if(langg == 'FR') call SetTooltipText('TBmeansMD', 'Dialog pour moyennes de variables')

    if(langg == 'DE') call SetTooltipText('combobox_MDtyp', '(Typ 1): sx^2 = (m-1)/(m-3)*s0x^2/m' // char(13) //   &
                                                    '(Typ 2:) sn^2 = (n_av +(m-1)/(m-3)*(n_av+s0n^2))/m' //   &
                                            char(13) // '(Typ 3:) sx^2 = s0x^2/m  (klassisch)' //   &
                                                    char(13) // 'm: Anzahl der Werte' )
    if(langg == 'EN') call SetTooltipText('combobox_MDtyp', '(Type1 1): sx^2 = (m-1)/(m-3)*s0x^2/m' // char(13) //   &
                                                    '(Type 2:) sn^2 = (n_av +(m-1)/(m-3)*(n_av+s0n^2))/m' //   &
                                            char(13) // '(Type 3:) sx^2 = s0x^2/m  (classical)' //   &
                                                    char(13) // 'm: number of vals' )
    if(langg == 'FR') call SetTooltipText('combobox_MDtyp', '(Type1 1): sx^2 = (m-1)/(m-3)*s0x^2/m' // char(13) //   &
                                                    '(Type 2:) sn^2 = (n_av +(m-1)/(m-3)*(n_av+s0n^2))/m' //   &
                                            char(13) // '(Type 3:) sx^2 = s0x^2/m  (classique)' //   &
                                                    char(13) // 'm: nombre de valeurs' )
    if(langg == 'DE') then
        str1 = 'wechselt auch die dazugehörigen Werte'
        str2 = char(13) // 'Siehe auch: "coverage intervals" in Datei MC_Tables.txt'
        call SetTooltipText('TRcheckbutton3',trim(str1)//char(13) // 'Siehe auch: "coverage intervals" in Datei fort66.txt')
        call SetTooltipText('TRcheckbutton2',trim(str1)//trim(str2))
        ! call SetTooltipText('TRcheckbutton4',trim(str1))
    elseif(langg == 'EN') then
        str1 = 'also changes the associated values'
        str2 = char(13) // 'See also: "coverage intervals" in file MC_Tables.txt'
        call SetTooltipText('TRcheckbutton3',trim(str1)//char(13) // 'See also: "coverage intervals" in file MC_Tables.txt')
        call SetTooltipText('TRcheckbutton2',trim(str1)//trim(str2))
        ! call SetTooltipText('TRcheckbutton4',trim(str1))
    elseif(langg == 'FR') then
        str1 = 'modifie également les valeurs associées'
        str2 = char(13) // 'Voir aussi: "coverage intervals" dans le fichier MC_Tables.txt'
        call SetTooltipText('TRcheckbutton3',trim(str1)//char(13) // 'Voir aussi: "coverage intervals" dans le fichier MC_Tables.txt')
        call SetTooltipText('TRcheckbutton2',trim(str1)//trim(str2))
        ! call SetTooltipText('TRcheckbutton4',trim(str1))
    end if

    if(langg == 'DE') then
        call SetTooltipText('LBEditingCell','kopiere (lange) Formel hinein und kopiere sie nach Editieren zurück')
    elseif(langg == 'EN') then
        call SetTooltipText('LBEditingCell','copy (long) formula into it and after editing copy it back')
    elseif(langg == 'FR') then
        call SetTooltipText('LBEditingCell','copier la formule (longue) dans celle-ci et, après édition, la copier')
    end if

    if(langg == 'DE') then
        str1 = 'Die Symbolliste ist erst aktuell nach dem Laden der ergänzten Symbol-Tabelle'
        call SetTooltipText('comboboxBinPoi1',trim(str1))
        call SetTooltipText('comboboxBinPoi2',trim(str1))
        call SetTooltipText('comboboxBinPoi3',trim(str1))
        call SetTooltipText('comboboxBinPoi4',trim(str1))
    elseif(langg == 'EN') then
        str1 = 'The symbol list is not actual before loading the finalized symbol table'
        call SetTooltipText('comboboxBinPoi1',trim(str1))
        call SetTooltipText('comboboxBinPoi2',trim(str1))
        call SetTooltipText('comboboxBinPoi3',trim(str1))
        call SetTooltipText('comboboxBinPoi4',trim(str1))
    elseif(langg == 'FR') then
        str1 = 'La liste des symboles n''est pas réelle avant le chargement de la table des symboles finalisée'
        call SetTooltipText('comboboxBinPoi1',trim(str1))
        call SetTooltipText('comboboxBinPoi2',trim(str1))
        call SetTooltipText('comboboxBinPoi3',trim(str1))
        call SetTooltipText('comboboxBinPoi4',trim(str1))
    end if

    if(langg == 'DE') then
        call SetTooltipText('TBDistribDialog','Verteilungsparameter: zuvor Zeile der Variable in TAB 3 selektierten')
    elseif(langg == 'EN') then
        call SetTooltipText('TBDistribDialog','distribution parameter: first select row of the variable in TAB 3')
    elseif(langg == 'FR') then
        call SetTooltipText('TBDistribDialog','paramètre de distribution: sélectionner la première ligne de la variable dans TAB 3')
    end if

    if(langg == 'DE') then
        call SetTooltipText('DistribGrid','Daten: siehe Dialog für Mittelwerte')
    elseif(langg == 'EN') then
        call SetTooltipText('DistribGrid','Data: see diolog for means')
    elseif(langg == 'FR') then
        call SetTooltipText('DistribGrid','Données: voir la boîte de dialogue pour les valeurs moyennes')
    end if

    if(langg == 'DE') then
        call SetTooltipText('TRbuttonSavecsv','Ergebnisse in CSV-Datei sichern')
    elseif(langg == 'EN') then
        call SetTooltipText('TRbuttonSavecsv','Save results to a CSV file')
    elseif(langg == 'FR') then
        call SetTooltipText('TRbuttonSavecsv','Enregistrer les résultats dans un fichier CSV')
    end if

    if(langg == 'DE') then
        call SetTooltipText('comboboxA1','Maus klicken, gedrückt halten und auswählen')
    elseif(langg == 'EN') then
        call SetTooltipText('comboboxA1','Click mouse, hold and select')
    elseif(langg == 'FR') then
        call SetTooltipText('comboboxA1','Cliquez sur la souris, maintenez et sélectionnez')
    end if

    IF(langg == 'DE') call SetTooltipText('CheckUnits',  &
                                'Nach dem Test kann das Projekt geändert sein (ohne Hinweis)!' // char(13) &
                            // 'Speichere das Projekt explizit, ggf. unter neuem Namen,' // char(13) &
                            // 'falls es so weiter benutzt werden soll.' )
    IF(langg == 'EN') call SetTooltipText('CheckUnits',  &
                                'After the test, the project can be changed (without notice)!' // char(13) &
                            // 'Save the project explicitly, if necessary under a new name,' // char(13) &
                            // 'if you want to continue using it.' )
    IF(langg == 'FR') call SetTooltipText('CheckUnits',  &
                                'Après le test, le projet peut être modifié (sans préavis) !' // char(13) &
                            // 'Enregistrez explicitement le projet, si nécessaire sous un nouveau nom,' // char(13) &
                            // 'si vous souhaitez continuer à l''utiliser.' )

    IF(langg == 'DE') call SetTooltipText('HelpFunctions',  &
                                'spezielle Funktionen:' // char(13)  &
                            // 'sqrt(x)       Wurzelfunktion' // char(13) &
                            // 'exp(x)        Exponentialfunktion' // char(13) &
                            // 'log(x), ln(x) natürlicher Logarithmus'  // char(13) &
                            // 'log10(x)      dekadischer Logarithmus' // char(13)  &
                            // 'a^b, a**b     Potenzfunktion' // char(13) // char(13)  &
                            // 'fd(tA,tm,lam) = exp(-lam*tA)*(1-exp(-lam*tm))/(lam*tm) ' // char(13) &
                            // 'uval(x)       internen Wert der Unsicherheit des Symbols x aufrufen' // char(13) &
                            // '  weitere Funktionen: siehe Toolbar Icon "f(x)"' )
    IF(langg == 'EN') call SetTooltipText('HelpFunctions',  &
                                'special functions:' // char(13)  &
                            // 'sqrt(x)       square root function' // char(13) &
                            // 'exp(x)        exponential function' // char(13) &
                            // 'log(x), ln(x) natural logarithm'  // char(13) &
                            // 'log10(x)      decimal logarithm' // char(13)  &
                            // 'a^b, a**b     power function' // char(13) // char(13)  &
                            // 'fd(tA,tm,lam) = exp(-lam*tA)*(1-exp(-lam*tm))/(lam*tm) ' // char(13) &
                            // 'uval(x)       get the internal uncertainty value of the symbol x'  // char(13) &
                            // '  further functions: see Toolbar Icon "f(x)"' )
    IF(langg == 'FR') call SetTooltipText('HelpFunctions',  &
                                'fonctions spéciales :' // char(13)  &
                            // 'sqrt(x)       fonction racine carrée' // char(13) &
                            // 'exp(x)        fonction exponentielle' // char(13) &
                            // 'log(x), ln(x) algorithme naturel'  // char(13) &
                            // 'log10(x)      logarithme décimal' // char(13)  &
                            // 'a^b, a**b     fonction puissance' // char(13) // char(13)  &
                            // 'fd(tA,tm,lam) = exp(-lam*tA)*(1-exp(-lam*tm))/(lam*tm) ' // char(13) &
                            // 'uval(x)       obtenir la valeur d''incertitude interne du symbole x'  // char(13) &
                            // '  autres fonctions : voir l''icône de la barre d''outils "f(x)"' )

    if(langg == 'DE') then
    ! 3.8.2023:
    call SetTooltipText('checkAbsTime', &
                        'wenn deaktiviert: Die Einheit unter "StartDiff" muss immer "Sekunde" sein!')
    elseif(langg == 'EN') then
    call SetTooltipText('checkAbsTime', &
                        'if disabled: The unit under "StartDiff" must always be "second"!')
    elseif(langg == 'FR') then
    call SetTooltipText('checkAbsTime', &
                        'si désactivé : L''unité sous la "StartDiff" doit toujours être "seconde" !')
    end if

    if(langg == 'DE') call SetTooltipText('radiobuttonNLSQ','Standard-Fitmethode, für große Impulsanzahlen, CHM 7.3.4')
    if(langg == 'EN') call SetTooltipText('radiobuttonNLSQ','standard fit method, for large counts, CHM 7.3.4')
    if(langg == 'FR') call SetTooltipText('radiobuttonNLSQ','méthode d''ajustement standard, pour les grands nombres, CHM 7.3.4')

    if(langg == 'DE') call SetTooltipText('radiobuttonPLSQ','Fitmethode für kleine Impulsanzahlen, CHM 7.3.4')
    if(langg == 'EN') call SetTooltipText('radiobuttonPLSQ','fit method for small counts, CHM 7.3.4')
    if(langg == 'FR') call SetTooltipText('radiobuttonPLSQ','méthode d''ajustement pour les petits comptes, CHM 7.3.4')

    if(langg == 'DE') call SetTooltipText('radiobuttonPMLE','Fitmethode, für sehr kleine Impulsanzahlen, CHM 7.3.4')
    if(langg == 'EN') call SetTooltipText('radiobuttonPMLE','fit method for very small counts, CHM 7.3.4')
    if(langg == 'FR') call SetTooltipText('radiobuttonPMLE','méthode d''ajustement pour les très petits nombres, CHM 7.3.4')

    if(langg == 'DE') call SetTooltipText('URfunctions','Infos zu speziellen UR-Funktionen')
    if(langg == 'EN') call SetTooltipText('URfunctions','Infos about special UR functions')
    if(langg == 'FR') call SetTooltipText('URfunctions','Infos sur les fonctions spéciales UR')

    if(.true.) then
    do i=1,nclobj
        if(clobj%name(i)%s /= 'GtkButton') cycle
        idstr = clobj%idd(i)%s
        str = clobj%label(i)%s
        kkk = 0
        if(trim(idstr) == 'FillDecColumn') then
            kkk = 1
            ! write(66,*) 'label(FillDecColumn)=',trim(str),'  kkk=',int(kkk,2),' langg=',langg
        end if
        select case (langg)
        case ('DE')
            if(trim(str) == 'Hilfe') call WDPutLabelString(idstr, 'Hilfe')
            if(trim(str) == 'Abbrechen') call WDPutLabelString(idstr, 'Abbrechen')
            if(trim(str) == 'Anwenden') call WDPutLabelString(idstr, 'Anwenden')
            if(trim(str) == 'Beenden') call WDPutLabelString(idstr, 'Beenden')
            if(trim(str) == 'OK') call WDPutLabelString(idstr, 'OK')
            if(trim(str) == 'Speichern') call WDPutLabelString(idstr, 'Speichern')
            if(trim(str) == 'Ausführen') call WDPutLabelString(idstr, 'Ausführen')
            if(trim(str) == 'Speichern als') call WDPutLabelString(idstr, 'Speichern als')
            ! if(kkk == 1) write(66,*) 'nach DE:  trim(clobj%label(i))=',trim(clobj%label(i)),' str=',trim(str)
        case ('EN')
            if(trim(str) == 'Hilfe') call WDPutLabelString(idstr, 'Help')
            if(trim(str) == 'Abbrechen') call WDPutLabelString(idstr, 'Cancel')
            if(trim(str) == 'Anwenden') call WDPutLabelString(idstr, 'Apply')
            if(trim(str) == 'Beenden') call WDPutLabelString(idstr, 'Quit')
            if(trim(str) == 'OK') call WDPutLabelString(idstr, 'OK')
            if(trim(str) == 'Speichern') call WDPutLabelString(idstr, 'Save')
            if(trim(str) == 'Ausführen') call WDPutLabelString(idstr, 'Run')
            if(trim(str) == 'Speichern als') call WDPutLabelString(idstr, 'Save as')
            !if(kkk == 1) then
            !  write(66,*) 'trim(clobj%label(i)) == ''Ausführen''', ' = ',trim(clobj%label(i)) == 'Ausführen'
            !  write(66,*) 'nach EN:  trim(clobj%label(i))=',trim(clobj%label(i)),'  str=',trim(str)
            !end if
        case ('FR')
            if(trim(str) == 'Hilfe') call WDPutLabelString(idstr, 'Aide')
            if(trim(str) == 'Abbrechen') call WDPutLabelString(idstr, 'Annuler')
            if(trim(str) == 'Anwenden') call WDPutLabelString(idstr, 'Appliquer')
            if(trim(str) == 'Beenden') call WDPutLabelString(idstr, 'Quitter')
            if(trim(str) == 'OK') call WDPutLabelString(idstr, 'OK')
            if(trim(str) == 'Speichern') call WDPutLabelString(idstr, 'Enregistrer')
            if(trim(str) == 'Ausführen') call WDPutLabelString(idstr, 'Exécuter')
            if(trim(str) == 'Speichern als') call WDPutLabelString(idstr, 'Enregistrer sous')
        end select

        call WDGetLabelString(idstr, str2)
        !if(trim(str) /= 'OK' .and. trim(str) == trim(str2)) &
        !     write(66,*) 'idstr=',trim(idstr),'  str=',trim(str),'  str2=',trim(str2)

    end do
    end if

end subroutine TranslateUR

!#################################################################################
