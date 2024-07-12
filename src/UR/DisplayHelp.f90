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

subroutine DisplayHelp(ncitem, idstr)

    ! this routine is called by clicking on Help buttons; it calls then
    ! via the HTML Help Workshop the UncertRadio CHM Help file and displays it.
    ! There are about 25 topics defined in the Help file (arrays topics_de or
    ! topics_en, defined as below) which are shown directly (i.e., context-related)
    ! when clicking an associated Help button.
    ! The UncertRadio CHM Helpfile is generated from a Word document and compiled
    ! to CHM (compiled HTML) with the software NüHelp; see the CHM help for
    ! information about NüHelp; https://sourceforge.net/projects/nuhelp/
    !
    !   Copyright (C) 2014-2023  Günter Kanisch
    ! Flo: I think we should convert the help files to a simple html page.
    !      It's easier to maintain and does not depend on the windows help system

    use, intrinsic :: iso_c_binding,       only: c_int, c_null_ptr, c_int32_t, c_char, c_null_char
    use UR_gtk_variables,                  only: clobj, HelpButton
    use UR_variables,                      only: langg, help_path, chm_opened, wpunix
    use file_io,                           only: logger
    use gtk,                               only: GTK_BUTTONS_OK, GTK_MESSAGE_WARNING, &
                                                 gtk_show_uri_on_window
    use Rout,                              only: MessageShow
    use top,                               only: idpt
    use chf,                               only: flfu

    implicit none

    integer, intent(in)                    :: ncitem
    character(len=*), optional, intent(in) :: idstr

    integer                                :: i, k, j, finfo(13), status
    character(len=128)                     :: topics(26), topics_de(26), topics_en(26)
    character(len=256)                     :: str1, log_str

    character(:), allocatable              :: cmdstring, hfile, wine_flag, idstring
    integer(c_int)                         :: resp
    !----------------------------------------------------------------------------------------------

    !  for Nühelp:
    !                topic                                                            ButtonID:
    topics_de(1)  = '2-Inhalt-des-Programms.html                                      TBInfoDialog'
    topics_de(2)  = '2-Inhalt-des-Programms.html                                      Help_UR'
    topics_de(3)  = '3.4-Dialog-Optionen-Voreinstellungen.html                        DOptionsHelp'
    topics_de(4)  = '3.8-Grafik-Fenster.html                                          HelpBS1'
    topics_de(5)  = '4.1-TAB-Verfahren.html                                           HelpProcedure'
    topics_de(6)  = '4.2-TAB-Gleichungen.html                                         HelpEquations'
    topics_de(7)  = '4.3-TAB-Werte,-Unsicherheiten.html                               HelpValUnc'
    topics_de(8)  = '4.4-TAB-Unsicherheiten-Budget.html                               HelpBudget'
    topics_de(9)  = '4.5-TAB-Resultate.html                                           TRButtonHelp'
    topics_de(10) = '5.6-Serielle-Auswertungen-eines-vorhandenen-Projekts.html        HelpSE'
    topics_de(11) = '5.7-Projekte-im-Batch-Mode-auswerten.html                        HelpBEV'
    topics_de(12) = '5.8-Testauswertung-der-Beispielprojekte-im-Batch-Mode.html       BTHelp'
    topics_de(13) = '6.3-Lineares-Least-squares-Verfahren.html                        HelpLinfit'
    topics_de(14) = '6.4-Verwendung-einer-Kalibrierkurve.html                         HelpKalib'
    topics_de(15) = '6.5-Aktivitatsbestimmung-mit-mehreren-Gammalinien.html           HelpGspk1'
    topics_de(16) = '6.6-Monte-Carlo-Simulation.html                                  HelpMC1'
    topics_de(17) = '7.8-Textfenster-fur-die-Gleichungen.html                         HelpTextEQ'
    topics_de(18) = '7.10-Dialog-Eingabe-der-Abklingkurve.html                        HelpDecayInput'
    topics_de(19) = '7.11-Dialog-Festlegung-des-Modells-der-Abklingkurve.html         HelpDecayModel'
    topics_de(20) = '3.5-Ratschlage-bei-Problemen.html                                TBProblems'
    topics_de(21) = '6.8-Konfidenz-Ellipsen.html                                      HelpELI'
    topics_de(22) = '6.9-Verwendung-von-Datensatzen-fur-Mittelwert-und-Varianz.html   MDHelp'
    topics_de(23) = '6.10.4-Implementierung-in-UncertRadio.html                       BinPoiHelp'
    topics_de(24) = '6.11-Spezielle-Verteilungen-und-ihre-Eigenschaften.html          HelpDistrib'
    topics_de(25) = '6.13-Zusammenfassung-der-Aktivitaten-mehrerer-Aliquots.html      HelpSumEval'
    topics_de(26) = '3.3.1-Liste-der-Beispiel-Projekte.html                           HelpExamples'

    topics_en(1)  = '2-Contents-of-the-Program.html                                   TBInfoDialog'
    topics_en(2)  = '2-Contents-of-the-Program.html                                   Help_UR'
    topics_en(3)  = '3.4-Options-dialog-Presetting.html                               DOptionsHelp'
    topics_en(4)  = '3.8-Graphics-window.html                                         HelpBS1'                  !
    topics_en(5)  = '4.1-TAB-Procedure.html                                           HelpProcedure'
    topics_en(6)  = '4.2-TAB-Equations.html                                           HelpEquations'
    topics_en(7)  = '4.3-TAB-Values,-Uncertainties.html                               HelpValUnc'
    topics_en(8)  = '4.4-TAB-Uncertainty-Budget.html                                  HelpBudget'
    topics_en(9)  = '4.5-TAB-Results.html                                             TRButtonHelp'
    topics_en(10) = '5.6-Serial-evaluations-of-an-existing-project.html               HelpSE'
    topics_en(11) = '5.7-Processing-projects-in-batch-mode.html                       HelpBEV'
    topics_en(12) = '5.8-Batch-mode-testing-the-evaluation-of-example-projects.html   BTHelp'
    topics_en(13) = '6.3-Linear-Least-squares-method.html                             HelpLinfit'
    topics_en(14) = '6.4-Utilizing-a-calibration-curve.html                           HelpKalib'
    topics_en(15) = '6.5-Activity-determination-from-several-gamma-lines.html         HelpGspk1'
    topics_en(16) = '6.6-Monte-Carlo-Simulation.html                                  HelpMC1'
    topics_en(17) = '7.8-Text-field-for-equations.html                                HelpTextEQ'
    topics_en(18) = '7.10-Dialog-Values-of-decay-curve.html                           HelpDecayInput'
    topics_en(19) = '7.11-Dialog-Definition-of-the-decay-curve-model.html             HelpDecayModel'
    topics_en(20) = '3.5-Advice-in-case-of-problems.html                              TBProblems'
    topics_en(21) = '6.8-Confidence-ellipses.html                                     HelpELI'
    topics_en(22) = '6.9-Using-data-sets-for-mean-and-variance.html                   MDHelp'
    topics_en(23) = '6.10.4-Implementation-in-UncertRadio.html                        BinPoiHelp'
    topics_en(24) = '6.11-Special-distributions-and-their-properties.html             HelpDistrib'
    topics_en(25) = '6.13-Aggregating-activities-of-several-aliquots.html             HelpSumEval'
    topics_en(26) = '3.3.1-List-of-example-projects.html                              HelpExamples'

    if(ncitem > 0) then

        if(clobj%idparent(ncitem) <= 0) return

        idstring = clobj%idd(ncitem)%s

        if(idstring == 'HelpFX' .and. present(idstr)) idstring = idstr
    else if(ncitem == 0 .and. present(idstr)) then
        idstring = idstr
    end if

    if (wpunix) then
        ! here we need a check if wine is installed and available,
        ! otherwise there is no UR2 help atm
        wine_flag = 'wine '
    else
        wine_flag = ''
    end if

    ! select hfile based on the selected language (in var langg)
    if(langg == 'DE') then
        hfile = trim(help_path) // 'UR2_5_Help_DE.chm'
        topics = topics_de

    else
        ! there is no translation for french atm
        hfile = trim(help_path) // 'UR2_5_Help_EN.chm'
        topics = topics_en

    end if

    if(chm_opened) then
        ! Terminate the process for the previous Help topic before invoking the next help topic
        if (.not. wine_flag == '') then
            ! wine taskkill has no filter, thus kill all hh processced
            cmdstring = wine_flag // 'taskkill /F /IM hh.exe /T'
        else
            if(langg == 'DE')  then
                cmdstring = 'taskkill /F /IM hh.exe /FI "windowtitle eq Windows-Hilfe für UncertRadio (64-bit)" /T '
            else
                cmdstring = 'taskkill /F /IM hh.exe /FI "windowtitle eq Windows Help for UncertRadio (64-bit)" /T '
            end if
        end if
        call execute_command_line(cmdstring, wait=.true., exitstat=j, cmdstat=k, cmdmsg=str1)

        write(log_str, '(*(g0))') ' EXITSTAT=', j ,'  CMDSTAT=', k
        call logger(67, log_str)
        if(k /= 0)  call logger(67, '       Message=' // trim(str1))

        chm_opened = .false.
    end if

    ! search for the correct topic that is linked to the ButtonID.
    do i=1, size(topics)
        if(idstring == trim(topics(i)(66:))) then
            call stat(flfu(hfile), finfo, status)
            if(status /= 0) then
                if(langg == 'DE') str1 = 'Die Datei ' // hfile // ' kann nicht geöffnet werden oder fehlt!'
                if(langg == 'EN') str1 = 'The file ' // hfile // ' cannot be opened or is missing!'
                if(langg == 'FR') str1 = 'Le fichier ' // hfile // ' ne peut pas être ouvert ou est manquant!'
                call MessageShow(trim(str1), &
                                 GTK_BUTTONS_OK, &
                                 "DisplayHelp:", &
                                 resp, &
                                 mtype=GTK_MESSAGE_WARNING)
            else
                resp = gtk_show_uri_on_window(idpt('window1'), &
                                              'file://' // help_path // 'html/first_steps.html#viewing-an-existing-project', &
                                              0, c_null_ptr)
                cmdstring = 'start /B hh.exe ' // flfu(hfile) // '::' // trim(topics(i)(1:64))     ! 4.9.2024:  trim()
                call logger(67, 'cmdstring=' // wine_flag // cmdstring)

                call execute_command_line(wine_flag // cmdstring, wait=.true., &
                                          exitstat=j, cmdstat=k, cmdmsg=str1)
                chm_opened = .true.
                write(log_str, '(*(g0))') ' EXITSTAT=',j,'  CMDSTAT=',k
                call logger(67, log_str)

                if(k /= 0) call logger(67, '       Message=' // trim(str1))

                HelpButton = .false.
                return
            end if
        end if
    end do

end subroutine DisplayHelp
