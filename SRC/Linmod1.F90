
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
    !  Copyright (C) 2014-2023  Günter Kanisch
    !

use, intrinsic :: iso_c_binding,      only: c_null_char,c_ptr,c_int,c_int16_t
USE UR_Gleich,          only: Formeltext,FormeltextFit,ifehl,loadingpro,syntax_check, &
                              charv
use UR_gtk_variables,   only: dialogstr,ioption
use UR_perror
use UR_variables,       only: langg,saveP
use Sym1,               only: Symbol1
use Rout,               only: WDGetTextviewString
use Top,                only: WrStatusbar,FindItemS
use LDN,                only: Loadsel_diag_new
use RG,                 only: Read_Gleich,modify_Formeltext
use UR_Linft,           only: dmodif,kfitmeth

implicit none

integer(4),intent(in)  :: mode         ! 1:  call von Read_Gleich;   2: call von ProcessMenu

integer(4)           :: i, ncitem

CHARACTER(LEN=100)   :: cgl1,cgl2,cgl3
CHARACTER(LEN=2)     :: crlf

!----------------------------------------------------------------------------
WRITE(crlf,'(a1,a1)') CHAR(13),CHAR(10)
  cgl1 = 'X1 = (1. - exp(-log(2.)*tmess/HwzY90)) / (log(2.)*tmess/HwzY90) * exp(-log(2.)*tstart/HwzY90)'
  cgl2 = 'X2 = (1. - exp(-log(2.)*tmess/Hwzlong)) / (log(2.)*tmess/Hwzlong) * exp(-log(2.)*tstart/Hwzlong)'
  cgl3 = 'X3 = (1. - exp(-log(2.)*tmess/HwzAc228)) / (log(2.)*tmess/HwzAc228) * exp(-log(2.)*tstart/HwzAc228)'

IF(ubound(FormelTextFit,dim=1) < 1) THEN
  allocate(FormeltextFit(3))
  FormeltextFit(1)%s = cgl1
  FormeltextFit(2)%s = cgl2
  FormeltextFit(3)%s = cgl3
END IF
IF(loadingPro) RETURN

ioption = 2
dialogstr = 'dialogDecayModel'
call FindItemS(dialogstr, ncitem)
call Loadsel_diag_new(1, ncitem)

if(mode == 2 .and. syntax_check) then

  call WDGetTextviewString('textview2',Formeltext)
  call modify_Formeltext(1)

  Write(66,*) 'Read_Gleich:  called by Linmod1!'
  call Read_Gleich()

   WRITE(66,*) 'After call Read_Gleich: ifehl=',ifehl,'  ifehlp=',ifehlp
   IF(ifehl == 1) THEN
     IF(langg == 'DE') call WrStatusbar(4, 'Fehler in Gleichungen beheben!')
     IF(langg == 'EN') call WrStatusbar(4, 'Eliminate error(s) in equations!')
     IF(langg == 'FR') call WrStatusbar(4, 'Élimine le(s) erreur(s) dans les équations!')
     return
   END IF

  Write(66,*) 'Symbol1:  called by Linmod1!'
  call Symbol1()
  syntax_check = .false.

   IF(ifehlP == 1 .OR. ifehl == 1) THEN
     IF(langg == 'DE') call WrStatusbar(4, 'Fehler in Gleichungen oder Symbolliste beheben!')
     IF(langg == 'EN') call WrStatusbar(4, 'Eliminate error(s) in equations or in the symbol list!')
     IF(langg == 'FR') call WrStatusbar(4, 'Éliminer le(s) erreur(s) dans les équations ou dans la liste des symboles!')
     return
   END IF

end if

end subroutine Linmod1
