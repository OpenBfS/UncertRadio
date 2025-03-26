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
subroutine Linmod1(mode)

    ! If fitting a decay curve, this routine initiates the X-term function (X1, X2,...),
    ! if the array FormeltextFit has not yet been defined.
    ! It then ivokes the dialog for defining the fitting model.
    ! Following that, and being called with mode=2 and syntax_check=T, it
    ! reads in Formeltext and calls Read_Gleich (in which FormeltextFit is read)
    ! and Symbol1.
    ! If an error occurred in Read_Gleich or Symbol1 (ifehl=1) would then initiate
    ! error messages, so that the user can correct the two sets of equations.
    !
    !  Copyright (C) 2014-2024  Günter Kanisch
    !

    use UR_types
    use UR_Gleich_globals,          only: Formeltext, FormeltextFit, ifehl, loadingpro, syntax_check, &
                                  charv
    use UR_gtk_globals,   only: dialogstr, ioption
    use UR_perror

    use Sym1,               only: Symbol1
    use Rout,               only: WDGetTextviewString
    use Top,                only: WrStatusbar, FindItemS
    use LDN,                only: Loadsel_diag_new
    use RG,                 only: Read_Gleich, modify_Formeltext

    use file_io,            only: logger
    use translation_module, only: T => get_translation

    implicit none

    integer, intent(in)  :: mode         ! 1:  call von Read_Gleich;   2: call von ProcessMenu

    integer              :: ncitem

    character(len=100)   :: cgl1,cgl2,cgl3
    character(len=512)   :: log_str

    !----------------------------------------------------------------------------------------------
    cgl1 = 'X1 = (1. - exp(-log(2.)*tmess/HwzY90)) / (log(2.)*tmess/HwzY90) * exp(-log(2.)*tstart/HwzY90)'
    cgl2 = 'X2 = (1. - exp(-log(2.)*tmess/Hwzlong)) / (log(2.)*tmess/Hwzlong) * exp(-log(2.)*tstart/Hwzlong)'
    cgl3 = 'X3 = (1. - exp(-log(2.)*tmess/HwzAc228)) / (log(2.)*tmess/HwzAc228) * exp(-log(2.)*tstart/HwzAc228)'

    if(ubound(FormelTextFit,dim=1) < 1) then
        allocate(FormeltextFit(3))
        FormeltextFit(1)%s = cgl1
        FormeltextFit(2)%s = cgl2
        FormeltextFit(3)%s = cgl3
    end if
    if(loadingPro) return

    ioption = 2
    dialogstr = 'dialogDecayModel'
    call FindItemS(dialogstr, ncitem)
    call Loadsel_diag_new(1, ncitem)

    if(mode == 2 .and. syntax_check) then

        call WDGetTextviewString('textview2',Formeltext)
        call modify_Formeltext(1)

        call logger(66, 'Read_Gleich:  called by Linmod1!')
        call Read_Gleich()

        write(log_str, '(*(g0))') 'After call Read_Gleich: ifehl=', ifehl,'  ifehlp=', ifehlp
        call logger(66, log_str)
        if(ifehl == 1) then
            call WrStatusbar(4, T("Eliminate error(s) in equations!"))
            return
        end if

        call logger(66, 'Symbol1:  called by Linmod1!')
        call Symbol1()
        syntax_check = .false.

        if(ifehlP == 1 .OR. ifehl == 1) then
            call WrStatusbar(4, T("Eliminate error(s) in equations or in the symbol list!"))
            return
        end if
    end if

end subroutine Linmod1
