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
    ! topics, defined as below) which are shown directly (i.e., context-related)
    ! when clicking an associated Help button.
    ! The UncertRadio CHM Helpfile is generated from a Word document and compiled
    ! to CHM (compiled HTML) with the software NüHelp; see the CHM help for
    ! information about NüHelp; https://sourceforge.net/projects/nuhelp/
    !
    !   Copyright (C) 2014-2023  Günter Kanisch
    ! Flo: I think we should convert the help files to a simple html page.
    !      It's easier to maintain and does not depend on the windows help system

    use, intrinsic :: iso_c_binding,       only: c_int, c_null_ptr, c_null_char
    use UR_gtk_variables,                  only: clobj
    use UR_variables,                      only: langg, help_path, dir_sep
    use file_io,                           only: logger
    use gtk,                               only: GTK_BUTTONS_OK, GTK_MESSAGE_WARNING, &
                                                 gtk_show_uri_on_window
    use Rout,                              only: MessageShow
    use top,                               only: idpt
    use chf,                               only: flfu, lowercase

    implicit none

    integer, intent(in)                    :: ncitem
    character(len=*), optional, intent(in) :: idstr

    logical                                :: ex
    integer                                :: i, pos
    integer(c_int)                         :: resp
    character(len=128)                     :: topics(26)
    character(:), allocatable              :: idstring, home_url, url, lang

    !----------------------------------------------------------------------------------------------

    topics(1) = 'index.html | TBInfoDialog'
    topics(2) = 'index.html | Help_UR'
    topics(3) = 'first_steps/options_dialog.html | DOptionsHelp'
    topics(4) = 'first_steps/output_plots.html | HelpBS1'
    topics(5) = 'first_steps/TABS/Procedures.html | HelpProcedure'
    topics(6) = 'first_steps/TABS/Equations.html | HelpEquations'
    topics(7) = 'first_steps/TABS/Values_Uncertainties.html | HelpValUnc'
    topics(8) = 'first_steps/TABS/Uncertainty_Budget.html | HelpBudget'
    topics(9) = 'first_steps/TABS/Results.html | TRButtonHelp'
    topics(10) = 'batch_mode/serial_evaluation.html | HelpSE'
    topics(11) = 'batch_mode/processing_projects_batch_mode.html | HelpBEV'
    topics(12) = 'batch_mode/run_all_tests.html | BTHelp'
    topics(13) = 'special_methods/linear_least_square.html | HelpLinfit'
    topics(14) = 'special_methods/calibration_curve.html | HelpKalib'
    topics(15) = '6.5-Activity-determination-from-several-gamma-lines.html | HelpGspk1'
    topics(16) = '6.6-Monte-Carlo-Simulation.html | HelpMC1'
    topics(17) = '7.8-Text-field-for-equations.html | HelpTextEQ'
    topics(18) = '7.10-Dialog-Values-of-decay-curve.html | HelpDecayInput'
    topics(19) = '7.11-Dialog-Definition-of-the-decay-curve-model.html | HelpDecayModel'
    topics(20) = 'first_steps/problem_advice.html | TBProblems'
    topics(21) = '6.8-Confidence-ellipses.html | HelpELI'
    topics(22) = '6.9-Using-data-sets-for-mean-and-variance.html | MDHelp'
    topics(23) = '6.10.4-Implementation-in-UncertRadio.html | BinPoiHelp'
    topics(24) = '6.11-Special-distributions-and-their-properties.html | HelpDistrib'
    topics(25) = '6.13-Aggregating-activities-of-several-aliquots.html | HelpSumEval'
    topics(26) = 'first_steps/example_projects.html | HelpExamples'

    idstring = ""

    if(ncitem > 0) then

        if(clobj%idparent(ncitem) <= 0) return

        idstring = clobj%idd(ncitem)%s

        if(idstring == 'HelpFX' .and. present(idstr)) idstring = idstr
    else if(ncitem == 0 .and. present(idstr)) then
        idstring = idstr
    end if

    url = ""
    home_url = help_path // 'final' // dir_sep // 'html' // dir_sep // 'index.html'

    inquire(file=flfu(home_url), exist=ex)
    if (.not. ex) then
        call MessageShow("Could find the help files: " // new_line('A') // home_url, &
                         GTK_BUTTONS_OK, &
                         "DisplayHelp:", &
                         resp, &
                         mtype=GTK_MESSAGE_WARNING)
        call logger(66, "Help: Could find the help files: '" // home_url // "'")
        return
    end if

    if (langg == 'EN') then
        lang = ''
    else
        lang = lowercase(langg) // dir_sep
    end if

    ! search for the correct topic that is linked to the ButtonID.
    do i=1, size(topics)
        pos = index(topics(i), '|')

        if(idstring == trim(adjustl(topics(i)(pos+1:)))) then
            url = help_path // 'final' // dir_sep // 'html' // dir_sep // lang // trim(topics(i)(1:pos-1))
            inquire(file=flfu(url), exist=ex)
            if (.not. ex) then
                call logger(66, "Help: Could not find '" // url // "'")
                url = home_url
            end if
        end if
    end do

    ! check if an url is found
    if (url == "") then
        url = home_url
        call logger(66, "Help: Could not find url for button id '" // idstring // "'")
    end if

    ! finally open the help file using the systems browser
    resp = gtk_show_uri_on_window(idpt('window1'), &
                                  'file:///' // url // c_null_char, &
                                  0, c_null_ptr)

end subroutine DisplayHelp
