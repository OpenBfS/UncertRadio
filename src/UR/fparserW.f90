         !       contains:
         ! initf
         ! parsef
         ! evalf
         ! CheckSyntax
         ! EvalErrMsg
         ! ParseErrMsg
         ! OperatorIndex
         ! MathFunctionIndex
         ! VariableIndex
         ! RemoveSpaces
         ! Replace
         ! Compile
         ! AddCompiledByte
         ! MathItemIndex
         ! CompletelyEnclosed
         ! CompileSubstr
         ! IsBinaryOp
         ! RealNum
         ! LowCase
         ! FdExpand

MODULE fparser
!------- -------- --------- --------- --------- --------- --------- --------- -------
! Fortran 90 function parser v1.0
!------- -------- --------- --------- --------- --------- --------- --------- -------
!
! This public domain function parser module is intended for applications
! where a set of mathematical expressions is specified at runtime and is
! then evaluated for a large number of variable values. This is done by
! compiling the set of function strings into byte code, which is interpreted
! very efficiently for the various variable values.
!
! The source code is available from:
! http://www.its.uni-karlsruhe.de/~schmehl/opensource/fparser-v1.0.tar.gz
!
! Please send comments, corrections or questions to the author:
! Roland Schmehl <Roland.Schmehl@mach.uni-karlsruhe.de>
!
!------- -------- --------- --------- --------- --------- --------- --------- -------
! The function parser concept is based on a C++ class library written by Warp
! <warp@iki.fi> available from:
! http://www.students.tut.fi/~warp/FunctionParser/fparser.zip
!------- -------- --------- --------- --------- --------- --------- --------- -------
use, intrinsic :: iso_c_binding,         only: c_int,c_null_char
USE UR_params,             ONLY: rn, eps1min               ! Import KIND parameters
use gtk,                   only: GTK_BUTTONS_OK
use UR_Gleich,             only: charv

IMPLICIT NONE
!------- -------- --------- --------- --------- --------- --------- --------- -------
PUBLIC                     :: initf,    & ! Initialize function parser for n functions
                              parsef,   & ! Parse single function string
                              evalf,    & ! Evaluate single function
                              EvalErrMsg  ! Error message (Use only when EvalErrType>0)
integer, PUBLIC            :: EvalErrType ! =0: no error occured, >0: evaluation error
!------- -------- --------- --------- --------- --------- --------- --------- -------
PRIVATE
SAVE
INTEGER,                                  PARAMETER :: cImmed   = 1,          &
                                                       cNeg     = 2,          &
                                                       cAdd     = 3,          &
                                                       cSub     = 4,          &
                                                       cMul     = 5,          &
                                                       cDiv     = 6,          &
                                                       cPow     = 7,          &
                                                       cAbs     = 8,          &
                                                       cExp     = 9,          &
                                                       cLog10   = 10,         &
                                                       cLog     = 11,         &
                                                       cSqrt    = 12,         &
                                                       cSinh    = 13,         &
                                                       cCosh    = 14,         &
                                                       cTanh    = 15,         &
                                                       cSin     = 16,         &
                                                       cCos     = 17,         &
                                                       cTan     = 18,         &
                                                       cAsin    = 19,         &
                                                       cAcos    = 20,         &
                                                       cAtan    = 21,         &
                                                       cUval    = 22,         &     !###
                                                       cLn      = 23,         &
                                                       VarBegin = 24
CHARACTER (LEN=1), DIMENSION(cAdd:cPow),  PARAMETER :: Ops      = (/ '+',     &
                                                                     '-',     &
                                                                     '*',     &
                                                                     '/',     &
                                                                     '^' /)
CHARACTER (LEN=5), DIMENSION(cAbs:cLN), PARAMETER :: Funcs    = (/ 'abs  ', &     !###
                                                                     'exp  ' , &
                                                                     'log10', &
                                                                     'log  ', &
                                                                     'sqrt ', &
                                                                     'sinh ', &
                                                                     'cosh ', &
                                                                     'tanh ', &
                                                                     'sin  ', &
                                                                     'cos  ', &
                                                                     'tan  ', &
                                                                     'asin ', &
                                                                     'acos ', &
                                                                     'atan ', &
                                                                     'uval ', &
                                                                     'ln   ' /)
TYPE tComp
   integer, DIMENSION(:), POINTER     :: ByteCode
   integer                            :: ByteCodeSize
   real(rn),    DIMENSION(:), POINTER :: Immed
   integer                            :: ImmedSize
   real(rn),    DIMENSION(:), POINTER :: Stack
   integer                            :: StackSize, &
                                         StackPtr
END TYPE tComp
TYPE (tComp),  DIMENSION(:),  POINTER :: Comp              ! Bytecode
integer(4), DIMENSION(:),  ALLOCATABLE :: ipos             ! Associates function strings

!
CONTAINS
!
SUBROUTINE initf (n)

  use UR_Gleich,   only: FP_equat,FP_numeq
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Initialize function parser for n functions
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  integer(4), INTENT(in) :: n                                 ! Number of functions
  integer(4)             :: i
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ALLOCATE (Comp(n))
     if(allocated(FP_equat)) deallocate(FP_equat)
     allocate(FP_equat(n))
  DO i=1,n
     NULLIFY (Comp(i)%ByteCode,Comp(i)%Immed,Comp(i)%Stack)
         FP_equat(i)%s = ' '
  END DO
  FP_numeq = 0

END SUBROUTINE initf
!
SUBROUTINE parsef (i, FuncStr, Vart)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Parse ith function string FuncStr and compile it into bytecode
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  use, intrinsic :: iso_c_binding,      only: c_int
  USE UR_Perror
  USE UR_Variables,       ONLY: langg
  USE UR_params,          ONLY: fd_found      ! function fd() found
  use Rout,               only: MessageShow
  use UR_Gleich,          only: FP_numeq,FP_equat,ifehl,eqnumber
  use gtk,                only: GTK_MESSAGE_WARNING
  use CHF,                only: ucase

  IMPLICIT NONE
  integer(4),               INTENT(in) :: i         ! Function identifier
  CHARACTER (LEN=*),        INTENT(in) :: FuncStr   ! Function string
    type(charv),intent(in)               :: vart(:)   ! Array with variable names
  CHARACTER (LEN=LEN(FuncStr))         :: Func      ! Function string, local use
  CHARACTER(LEN=150)                   :: str1
  CHARACTER(LEN=LEN(FuncStr)+2*120)    :: Fupper
  character(len=len(Funcstr)+2*120)    :: Funcmodif
  integer(c_int)                       :: resp
  integer(4)                           :: iret,j,nnv,ixx,maxlen
    type(charv),allocatable              :: Var(:)       ! Array with variable names  ! 2020-04-30 Kn
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ifehlp = 0

    ! 2020-04-08: Kn
    if(allocated(Var)) deallocate(Var)
    nnv = size(vart)
    maxlen = 0
    do j=1,nnv
      ixx = len_trim(vart(j)%s)
      if(ixx > maxlen) maxlen = ixx
    end do
    allocate(var(nnv))
    do j=1,nnv
      var(j)%s = trim(vart(j)%s)
    end do
               !      write(66,*) 'Funcstr=',trim(funcstr)

  IF (i < 1 .OR. i > SIZE(Comp)) THEN
    IF(langg == 'EN') WRITE(str1,*) '*** Parser error: Equation number ',i,' out of range',Size(Comp)
    IF(langg == 'DE') WRITE(str1,*) '*** Parser error: Nr. der Gleichung ',i,' nicht gültig',Size(Comp)
    IF(langg == 'FR') WRITE(str1,*) '*** Erreur d''analyseur: numéro d''équation ',i,' hors de portée',Size(Comp)
    call MessageShow(trim(str1), GTK_BUTTONS_OK, "fparser:", resp,mtype=GTK_MESSAGE_WARNING)
    ifehlp = 1
    RETURN
  END IF
  kequation = i
     Fupper = ucase(FuncStr)
     if(allocated(ipos)) deallocate(ipos)
  ALLOCATE (ipos(LEN_TRIM(Fupper)))           !Kn            ! Char. positions in orig. string
  Func = Fupper                               !Kn            ! Local copy of function string
  CALL Replace ('**','^ ',Func)                            ! Exponent into 1-Char. format
  CALL RemoveSpaces (Func)                                 ! Condense function string
  !+++++++++++++++++++
    ! Kn
    Funcmodif = Func
    if(fd_found(i)) then
      call FdExpand(Func, Funcmodif, iret)
      Fupper = trim(Funcmodif)
    end if
  !+++++++++++++++++++
       ! write(66,*) ' before checksyntax(Funcmodif,..)'
  call CheckSyntax (Funcmodif, Fupper, Var)
       ! write(66,*) ' behind checksyntax(Funcmodif,..)'
  DEALLOCATE (ipos)
     if(ifehlp == 1) then
       ifehl = 1
       return
     end if
  CALL Compile (i,Funcmodif,Var)                            ! Compile into bytecode

     if(i > FP_numeq) FP_numeq = i
     FP_equat(i)%s = trim(Fupper)

   deallocate(var)

END SUBROUTINE parsef


FUNCTION evalf (i, GVal) RESULT (res)
  use UR_Gleich,      only: StdUnc,apply_units_dir,FP_for_units,unit_conv_fact

  use UR_Perror

  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Evaluate bytecode of ith function for the values passed in array Val(:)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE

  REAL(rn), DIMENSION(:), INTENT(in) :: GVal                ! Variable values

  integer(4),             INTENT(in) :: i                  ! Function identifier
  REAL(rn),allocatable, DIMENSION(:)    :: Val                ! Variable values
  REAL(rn)                           :: res                ! Result
  integer(4)                         :: IP,              & ! Instruction pointer
                                        DP,              & ! Data pointer
                                        SP                 ! Stack pointer
  REAL(rn),                PARAMETER :: zero = 0._rn
  integer(4)                :: j,k,ii,nng,iimx
  integer(4)                :: isymb(200)    ! introduced on 2019-11-27
                                             ! function uval(x) introduced: 2018-02-11
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  nng = ubound(GVal,dim=1)

  iimx = ubound(unit_conv_fact,dim=1)
  allocate(Val(nng))
  if(apply_units_dir .or. FP_for_units) then
    do ii=1,nng
      if(ii <= iimx) VAL(ii) = GVAL(ii) * unit_conv_fact(ii)
      if(ii > iimx) VAL(ii) = GVAL(ii)
    end do
  else
    Val(1:nng) = GVal(1:nng)
  end if
 !------------------------------------------------------------------------------------
  DP = 1
  SP = 0
  isymb = 0
  DO IP=1,Comp(i)%ByteCodeSize
    SELECT CASE (Comp(i)%ByteCode(IP))

      CASE (cImmed)
        SP=SP+1; Comp(i)%Stack(SP)=Comp(i)%Immed(DP); DP=DP+1
      CASE   (cNeg); Comp(i)%Stack(SP)=-Comp(i)%Stack(SP)
      CASE   (cAdd); Comp(i)%Stack(SP-1)=Comp(i)%Stack(SP-1)+Comp(i)%Stack(SP); SP=SP-1
      CASE   (cSub); Comp(i)%Stack(SP-1)=Comp(i)%Stack(SP-1)-Comp(i)%Stack(SP); SP=SP-1
      CASE   (cMul); Comp(i)%Stack(SP-1)=Comp(i)%Stack(SP-1)*Comp(i)%Stack(SP); SP=SP-1;
      CASE   (cDiv); IF (abs(Comp(i)%Stack(SP)) < eps1min) THEN; EvalErrType=1; res=zero; RETURN; end if
                     Comp(i)%Stack(SP-1)=Comp(i)%Stack(SP-1)/Comp(i)%Stack(SP); SP=SP-1;
      CASE   (cPow); Comp(i)%Stack(SP-1)=Comp(i)%Stack(SP-1)**Comp(i)%Stack(SP); SP=SP-1
      CASE   (cAbs); Comp(i)%Stack(SP)=ABS(Comp(i)%Stack(SP))
      CASE   (cExp); Comp(i)%Stack(SP)=EXP(Comp(i)%Stack(SP))
      CASE (cLog10); IF (Comp(i)%Stack(SP)<=0._rn) THEN; EvalErrType=3; res=zero; RETURN; end if
                     Comp(i)%Stack(SP)=LOG10(Comp(i)%Stack(SP))
      CASE   (cLog,cLn); IF (Comp(i)%Stack(SP)<=0._rn) THEN; EvalErrType=3; res=zero; RETURN; end if
                     Comp(i)%Stack(SP)=LOG(Comp(i)%Stack(SP))
      CASE  (cSqrt); IF (Comp(i)%Stack(SP)<0._rn) THEN; EvalErrType=3; res=zero; RETURN; end if
                     Comp(i)%Stack(SP)=SQRT(Comp(i)%Stack(SP))
      CASE  (cSinh); Comp(i)%Stack(SP)=SINH(Comp(i)%Stack(SP))
      CASE  (cCosh); Comp(i)%Stack(SP)=COSH(Comp(i)%Stack(SP))
      CASE  (cTanh); Comp(i)%Stack(SP)=TANH(Comp(i)%Stack(SP))
      CASE   (cSin); Comp(i)%Stack(SP)=SIN(Comp(i)%Stack(SP))
      CASE   (cCos); Comp(i)%Stack(SP)=COS(Comp(i)%Stack(SP))
      CASE   (cTan); Comp(i)%Stack(SP)=TAN(Comp(i)%Stack(SP))
      CASE  (cAsin); IF ((Comp(i)%Stack(SP)<-1._rn).OR.(Comp(i)%Stack(SP)>1._rn)) THEN
                     EvalErrType=4; res=zero; RETURN; end if
                     Comp(i)%Stack(SP)=ASIN(Comp(i)%Stack(SP))
      CASE  (cAcos); IF ((Comp(i)%Stack(SP)<-1._rn).OR.(Comp(i)%Stack(SP)>1._rn)) THEN
                     EvalErrType=4; res=zero; RETURN; end if
                     Comp(i)%Stack(SP)=ACOS(Comp(i)%Stack(SP))
      CASE  (cAtan); Comp(i)%Stack(SP)=ATAN(Comp(i)%Stack(SP))
      CASE  (cUval)
        if(isymb(SP) > 0) then
          Comp(i)%Stack(SP)= StdUnc(isymb(SP))     ! since 2019-22-27
        end if
      CASE  DEFAULT
        SP=SP+1;
        IF (Comp(i)%ByteCode(IP)-VarBegin+1 <= 0) THEN
                     EvalErrType=4; res=zero; RETURN; end if
        Comp(i)%Stack(SP)=Val(Comp(i)%ByteCode(IP)-VarBegin+1)
        isymb(SP) = Comp(i)%ByteCode(IP)-VarBegin+1

    END SELECT
  END DO
  EvalErrType = 0
  if(ubound(Comp(i)%stack,dim=1) > 0) then
    res = Comp(i)%Stack(1)
  else
    ifehlp = 1
    res = 0._rn
  end if
      ! if(i == 3) write(30,*) 'evalf: i=3:   res=',sngl(res),' EvalErrType=',EvalErrType

END FUNCTION evalf
!
SUBROUTINE CheckSyntax (Func,FuncStr,Var)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Check syntax of function string,  returns 0 if syntax is ok
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  USE UR_Perror
  USE UR_Variables, ONLY: langg
  USE UR_params,    ONLY: fd_found      ! function fd() found

  IMPLICIT NONE
  CHARACTER (LEN=*),               INTENT(in) :: Func      ! Function string without spaces
  CHARACTER (LEN=*),               INTENT(in) :: FuncStr   ! Original function string
  type(charv), DIMENSION(:), INTENT(in)       :: Var       ! Array with variable names

  integer                                     :: n
  CHARACTER (LEN=1)                           :: c
  REAL(rn)                                    :: r
  LOGICAL                                     :: err
  integer                                     :: ParCnt   ! Parenthesis counter
  integer                                     :: j, ib, in
  integer                                     :: lFunc    ! length of trimmed Func

  integer                                     :: jjj
  real(rn)                                    :: help
  integer                                     :: ios,i
  character(len=len_trim(funcStr))            :: fstr

  !----- -------- --------- --------- --------- --------- --------- --------- -------
  j = 1
  ParCnt = 0
  lFunc = LEN_TRIM(Func)
  step: DO
     IF (j > lFunc) CALL ParseErrMsg (j, FuncStr)
     c = Func(j:j)
     !-- -------- --------- --------- --------- --------- --------- --------- -------
     ! Check for valid operand (must appear)
     !-- -------- --------- --------- --------- --------- --------- --------- -------
     IF (c == '-' .OR. c == '+') THEN                      ! Check for leading - or +
        j = j+1
        IF (j > lFunc) THEN
          IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Missing operand')
          IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Fehlender Operand')
          IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Opérande manquant')
          ifehlp = 1
        END IF
        c = Func(j:j)
        IF (ANY(c == Ops)) THEN
          IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Multiple operators')
          IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Mehrfache Operatoren')
          IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Plusieurs opérateurs')
          ifehlp = 1
        END IF
     END IF
     n = MathFunctionIndex (Func(j:))
     IF (n > 0) THEN                                       ! Check for math function
        j = j+LEN_TRIM(Funcs(n))
        IF (j > lFunc) THEN
          IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Missing function argument')
          IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Fehlendes Gleichungs-Argument')
          IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Argument de fonction manquant')
          ifehlp = 1
        END IF
        c = Func(j:j)
        IF (c /= '(') THEN
          IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Missing opening parenthesis')
          IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Fehlende öffnende Klammer')
          IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Parenthèse ouvrante manquante')
          ifehlp = 1
        END IF
     END IF
     IF (c == '(') THEN                                    ! Check for opening parenthesis
        ParCnt = ParCnt+1
        j = j+1
        CYCLE step
     END IF
     IF (SCAN(c,'0123456789.') > 0) THEN                   ! Check for number
        r = RealNum (Func(j:),ib,in,err)
        IF (err) THEN
          IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Invalid number format:  '//Func(j+ib-1:j+in-2))
          IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Ungültiges Zahlenformat:  '//Func(j+ib-1:j+in-2))
          IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Format de nombre invalide:  '//Func(j+ib-1:j+in-2))
          ifehlp = 1
        END IF
        j = j+in-1
        IF (j > lFunc) EXIT
        c = Func(j:j)
     ELSE                                                  ! Check for variable
        n = VariableIndex (Func(j:),Var,ib,in)

        IF (n == 0) THEN   ! original
          ifehlp = 1
          !IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Invalid symbol: '//CHAR(13)//Func(j+ib-1:j+in-2))
          !IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Ungültiges Symbol: '//CHAR(13)//Func(j+ib-1:j+in-2))
          !IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Symbole invalide: '//CHAR(13)//Func(j+ib-1:j+in-2))
          IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Invalid symbol: '//CHAR(13)//Func(j+ib-1:j+in-1))
          IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Ungültiges Symbol: '//CHAR(13)//Func(j+ib-1:j+in-1))
          IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Symbole invalide: '//CHAR(13)//Func(j+ib-1:j+in-1))

            ! WRITE(66,*) 'invalid element: Func(j+ib-1:j+in-2)="',Func(j+ib-1:j+in-2),'"  j,ib,in=',j,ib,in
            WRITE(66,*) 'invalid element: Func(j+ib-1:j+in-1)="',Func(j+ib-1:j+in-1),'"  j,ib,in=',j,ib,in
            write(66,*) '   Funcstr=',trim(Funcstr)
            write(66,*) '   Func(j:)=',trim(Func(j:))
            ! write(66,*) 'FD_found=',fd_found
            WRITE(66,*) 'List of variables: ',(trim(var(jjj)%s),' ',jjj=1,SIZE(var))
        END IF
        j = j+in-1
        IF (j > lFunc) EXIT
        c = Func(j:j)   ! original
     END IF
     DO WHILE (c == ')')                                   ! Check for closing parenthesis
        ParCnt = ParCnt-1
        IF (ParCnt < 0) THEN
          IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Mismatched parenthesis')
          IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Nicht zusammenpassende Klammern')
          IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Parenthèse incohérente')
          ifehlp = 1
        END IF
      if(j > 1) then          ! <-  zusätzlich am 29.7.2021
        IF (Func(j-1:j-1) == '(') THEN
          IF(langg == 'EN') CALL ParseErrMsg (j-1, FuncStr, 'Empty parentheses')
          IF(langg == 'DE') CALL ParseErrMsg (j-1, FuncStr, 'Leere Klammern')
          IF(langg == 'FR') CALL ParseErrMsg (j-1, FuncStr, 'Parenthèses vides')
          ifehlp = 1
        END IF
      end if
        j = j+1
        IF (j > lFunc) EXIT
        c = Func(j:j)
     END DO
     !-- -------- --------- --------- --------- --------- --------- --------- -------
     ! Now, we have a legal operand: A legal operator or end of string must follow
     !-- -------- --------- --------- --------- --------- --------- --------- -------
     IF (j > lFunc) EXIT
     IF (ANY(c == Ops)) THEN                               ! Check for multiple operators
        IF (j+1 > lFunc) CALL ParseErrMsg (j, FuncStr)
        IF (ANY(Func(j+1:j+1) == Ops)) THEN
          IF(langg == 'EN') CALL ParseErrMsg (j+1, FuncStr, 'Multiple operators')
          IF(langg == 'DE') CALL ParseErrMsg (j+1, FuncStr, 'Mehrfache Operatoren')
          IF(langg == 'FR') CALL ParseErrMsg (j+1, FuncStr, 'Plusieurs opérateurs')
          ifehlp = 1
        end if
     ELSE                            ! Check for next operand
        ! test Funcstr for being only a number:
        fstr = Funcstr
        do i=1,len_trim(fstr)
          if(fstr(i:i) == ',') fstr(i:i) = '.'
        end do

        read(fstr,*,iostat=ios) help

        if(ios /= 0) then
          ifehlp = 1
          IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Missing operator')
          IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Fehlender Operator')
          IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Opérateur manquant')
        end if
     END IF
     !-- -------- --------- --------- --------- --------- --------- --------- -------
     ! Now, we have an operand and an operator: the next loop will check for another
     ! operand (must appear)
     !-- -------- --------- --------- --------- --------- --------- --------- -------
     j = j+1
         if(ifehlp == 1) return
  END DO step
  IF (ParCnt > 0) THEN
    IF(langg == 'EN') CALL ParseErrMsg (j, FuncStr, 'Missing bracket )')
    IF(langg == 'DE') CALL ParseErrMsg (j, FuncStr, 'Fehlende Klammer )')
    IF(langg == 'FR') CALL ParseErrMsg (j, FuncStr, 'Support manquant )')
    ifehlp = 1
  END IF
END SUBROUTINE CheckSyntax
!
FUNCTION EvalErrMsg () RESULT (msg)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Return error message
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  CHARACTER (LEN=*), DIMENSION(4), PARAMETER :: m = (/ 'Division by zero                ', &
                                                       'Argument of SQRT negative       ', &
                                                       'Argument of LOG negative        ', &
                                                       'Argument of ASIN or ACOS illegal' /)
  CHARACTER (LEN=LEN(m))                     :: msg
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IF (EvalErrType < 1 .OR. EvalErrType > SIZE(m)) THEN
     msg = ''
  ELSE
     msg = m(EvalErrType)
  end if
END FUNCTION EvalErrMsg
!
SUBROUTINE ParseErrMsg (j, FuncStr, Msg)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Print error message and terminate program
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  USE UR_Perror
  USE UR_Variables,         ONLY: langg
  use gtk,                  only: GTK_BUTTONS_YES_NO,gtk_response_yes,GTK_MESSAGE_WARNING
  use Rout,                 only: MessageShow
  use Top,                  only: CharModA1
  use CHF,                  only: ucase
  use UR_Gleich,            only: eqnumber

  IMPLICIT NONE
  integer,                     INTENT(in) :: j             ! Number of character within function string
  CHARACTER(LEN=*),            INTENT(in) :: FuncStr       ! Original function string
  CHARACTER(LEN=*), OPTIONAL,  INTENT(in) :: Msg

  integer                     :: k,i,jjh,i1,nfd
  CHARACTER(LEN=200)          :: str1
  CHARACTER(LEN=400)          :: str2
  CHARACTER(LEN=600)          :: str3
  character(len=60)           :: str0,symbb,str4
  integer(c_int)              :: resp
  logical                     :: invalidsymb
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  if(langg == 'DE') write(str0,'(a,i3,a)') 'Gleichung ',kequation,' : '
  if(langg == 'EN') write(str0,'(a,i3,a)') 'Equation ',kequation,' : '
  if(langg == 'FR') write(str0,'(a,i3,a)') 'Équation ',kequation,' : '
  IF (PRESENT(Msg)) THEN
          !   WRITE(*,*) '*** Error in syntax of function string: '//Msg
     IF(langg == 'EN') WRITE(str1,*) '*** Error in syntax of function string: '//TRIM(Msg)
     IF(langg == 'DE') WRITE(str1,*) '*** Syntaxfehler in Gleichungs-String: '//TRIM(Msg)
     IF(langg == 'FR') WRITE(str1,*) '*** Erreur dans la syntaxe de la chaîne de fonctions: '//TRIM(Msg)
     ifehlp = 1
  ELSE
          !   WRITE(*,*) '*** Error in syntax of function string:'
     IF(langg == 'EN') WRITE(str1,*) '*** Error in syntax of function string: '
     IF(langg == 'DE') WRITE(str1,*) '*** Syntaxfehler in Gleichungs-String: '
     IF(langg == 'FR') WRITE(str1,*) '*** Erreur dans la syntaxe de la chaîne de fonctions: '
     ifehlp = 1
  end if
  ifehlp = 1

  ! catch a new Symbol in equations, the user checks: correct of wrong characters?
  invalidSymb = .false.
  if(index(msg,'Invalid symbol') > 0 .or. index(msg,'Ungültiges Symbol') > 0  .or.    &
     index(msg,'Symbole invalide') > 0)           invalidSymb = .true.

  write(str4,'(a,i0,1x,a)') 'gefunden ab Zeichen Nr. ',j,' im String:'
  if(langg == 'EN') write(str4,'(a,i0,1x,a)')  'found from character no. ',j,' in the string:'
  if(langg == 'FR') write(str4,'(a,i0,1x,a)')  'trouvé à partir du caractère n ° ',j,' de la chaîne:'
  str2 = ' '

  WRITE(str2,'(A)') TRIM(FuncStr)
  str3 = trim(str0)// char(13) // char(13) // trim(str1) //char(13) //char(13) //  &
                                trim(str4) // char(13) // trim(str2)
  if(.not.invalidSymb) then
    call MessageShow(trim(str3), GTK_BUTTONS_OK, "ParseErrMsg:", resp,mtype=GTK_MESSAGE_WARNING)
    ifehlp = 1
  else
    ifehlp = 1
      !---------------
      ! Catch the case of repeatedly the same "new" symbol
      jjh = 0
      i1 = index(Msg,char(13))
      do i=1,nsymbnew
        if(trim(Msg(i1+1:)) == trim(symb_new(i)%s)) jjh = i
      end do
      if(jjh > 0)  ifehlp = 0
      if(jjh == 0) then
         symbb = trim(Msg(i1+1:))
      end if
      !---------------
    if(jjh == 0 .and. len_trim(symbb) > 0) then
      str3 = trim(str3) // char(13)//char(13)
      if(langg == 'DE') str3 = trim(str3) // 'Ungültiges (oder neues) Symbol akzeptieren?' !  &
                                        !  // '  ' // trim(str)
      if(langg == 'EN') str3 = trim(str3) // 'Accept invalid (or new) symbol?'
      if(langg == 'FR') str3 = trim(str3) // 'Accepter un symbole invalide (ou nouveau)?'
      call MessageShow(trim(str3), GTK_BUTTONS_YES_NO, "ParseErrMsg:", resp,mtype=GTK_MESSAGE_WARNING)
      IF (resp == GTK_RESPONSE_YES) THEN
        ifehlp = 0
        nfd = 0
        do k=1,nsymbnew
          if(trim(symb_new(k)%s) == trim(symbb)) nfd = 1
        end do
        if(nfd == 0) then
          nsymbnew = nsymbnew + 1
          call CharModA1(symb_new,nsymbnew)
          symb_new(nsymbnew)%s = symbb
        end if
      end if
    end if
  end if

END SUBROUTINE ParseErrMsg
!
FUNCTION OperatorIndex (c) RESULT (n)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Return operator index
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  CHARACTER (LEN=1), INTENT(in) :: c
  integer                       :: n, j
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  n = 0
  DO j=cAdd,cPow
     IF (c == Ops(j)) THEN
        n = j
        EXIT
     END IF
  END DO
END FUNCTION OperatorIndex
!
FUNCTION MathFunctionIndex (str) RESULT (n)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Return index of math function beginnig at 1st position of string str
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(in) :: str
  integer                       :: n, j
  integer                       :: k
  CHARACTER (LEN=LEN(Funcs))    :: fun
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  n = 0
  !// DO j=cAbs,cAtan                                          ! Check all math functions
  DO j=cAbs,cLn                                                ! Check all math functions     !###
     k = MIN(LEN_TRIM(Funcs(j)), LEN(str))
     CALL LowCase (str(1:k), fun)
     IF (fun == Funcs(j)) THEN                             ! Compare lower case letters
        n = j                                              ! Found a matching function
        EXIT
     END IF
  END DO
END FUNCTION MathFunctionIndex
!
FUNCTION VariableIndex (str, Var, ibegin, inext) RESULT (n)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Return index of variable at begin of string str (returns 0 if no variable found)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
 use UR_Gleich,      only: FP_for_units

  IMPLICIT NONE
  CHARACTER (LEN=*),          INTENT(in) :: str       ! String
  type(charv), DIMENSION(:), INTENT(in)  :: Var       ! Array with variable names
  integer                                :: n         ! Index of variable
  integer, OPTIONAL,      INTENT(out)    :: ibegin, & ! Start position of variable name
                                            inext     ! Position of character after name
  integer                                :: j,ib,in,lstr
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  n = 0
  lstr = LEN_TRIM(str)
  IF (lstr > 0) THEN
     DO ib=1,lstr                                          ! Search for first character in str
        IF (str(ib:ib) /= ' ') EXIT                        ! When lstr>0 at least 1 char in str
     END DO
     DO in=ib,lstr                                         ! Search for name terminators
       if(.not.FP_for_units) then
         IF (SCAN(str(in:in),'+-*/^) ') > 0) EXIT
       else
         IF (SCAN(str(in:in),'+-*/^) ') > 0) EXIT        ! ^ not used as terminator
       end if
     END DO
     DO j=1,SIZE(Var)
        IF (str(ib:in-1) == Var(j)%s) THEN
           n = j                                           ! Variable name found
           EXIT
        END IF
     END DO
  END IF
  IF (PRESENT(ibegin)) ibegin = ib
  IF (PRESENT(inext))  inext  = in
END FUNCTION VariableIndex
!
SUBROUTINE RemoveSpaces (str)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Remove Spaces from string, remember positions of characters in old string
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(inout) :: str
  integer                          :: k, lstr
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  lstr = LEN_TRIM(str)
  ipos = (/ (k,k=1,lstr) /)
  k = 1
  DO WHILE (str(k:lstr) /= ' ')
     IF (str(k:k) == ' ') THEN
        str(k:lstr)  = str(k+1:lstr)//' '                  ! Move 1 character to left
        ipos(k:lstr) = (/ ipos(k+1:lstr), 0 /)             ! Move 1 element to left
        k = k-1
     END IF
     k = k+1
  END DO
END SUBROUTINE RemoveSpaces
!
SUBROUTINE Replace (ca,cb,str)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Replace ALL appearances of character set ca in string str by character set cb
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  CHARACTER (LEN=*),       INTENT(in) :: ca
  CHARACTER (LEN=LEN(ca)), INTENT(in) :: cb                ! LEN(ca) must be LEN(cb)
  CHARACTER (LEN=*),    INTENT(inout) :: str
  integer                             :: j, lca
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  lca = LEN(ca)
  DO j=1,LEN_TRIM(str)-lca+1
     IF (str(j:j+lca-1) == ca) str(j:j+lca-1) = cb
  END DO
END SUBROUTINE Replace
!
SUBROUTINE Compile (i, F, Var)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Compile i-th function string F into bytecode
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  USE UR_Perror
  USE UR_Variables,        ONLY: langg
  use Rout,                only: MessageShow
  use gtk,                 only: GTK_MESSAGE_WARNING

  IMPLICIT NONE
  integer,                         INTENT(in) :: i         ! Function identifier
  CHARACTER (LEN=*),               INTENT(in) :: F         ! Function string
  type(charv), DIMENSION(:), INTENT(in)       :: Var       ! Array with variable names
  integer                                     :: istat,j
  CHARACTER(LEN=150)         :: str1
  integer(c_int)             :: resp
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IF (ASSOCIATED(Comp(i)%ByteCode)) DEALLOCATE ( Comp(i)%ByteCode, &
                                                 Comp(i)%Immed,    &
                                                 Comp(i)%Stack     )
  Comp(i)%ByteCodeSize = 0
  Comp(i)%ImmedSize    = 0
  Comp(i)%StackSize    = 0
  Comp(i)%StackPtr     = 0
  CALL CompileSubstr (i,F,1,LEN_TRIM(F),Var)               ! Compile string to determine size
  ALLOCATE ( Comp(i)%ByteCode(Comp(i)%ByteCodeSize), &
             Comp(i)%Immed(Comp(i)%ImmedSize),       &
             Comp(i)%Stack(Comp(i)%StackSize),       &
             STAT = istat                            )
  IF (istat /= 0) THEN
     IF(langg == 'EN') WRITE(str1,*) '*** Parser error: Memmory allocation for byte code failed'
     IF(langg == 'DE') WRITE(str1,*) '*** Parser Fehler: Memmory allocation for byte code failed'
     IF(langg == 'FR') WRITE(str1,*) '*** Parser error: L''allocation de mémoire pour le bytecode a échoué'
     call MessageShow(trim(str1), GTK_BUTTONS_OK, "fparser-compile:", resp,mtype=GTK_MESSAGE_WARNING)
     ifehlP = 1
  ELSE
     Comp(i)%ByteCodeSize = 0
     Comp(i)%ImmedSize    = 0
     Comp(i)%StackSize    = 0
     Comp(i)%StackPtr     = 0
     CALL CompileSubstr (i,F,1,LEN_TRIM(F),Var)            ! Compile string into bytecode
  END IF
  !
END SUBROUTINE Compile
!
SUBROUTINE AddCompiledByte (i, b)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Add compiled byte to bytecode
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  integer,     INTENT(in) :: i                             ! Function identifier
  integer,     INTENT(in) :: b                             ! Value of byte to be added
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  Comp(i)%ByteCodeSize = Comp(i)%ByteCodeSize + 1
  IF (ASSOCIATED(Comp(i)%ByteCode)) Comp(i)%ByteCode(Comp(i)%ByteCodeSize) = b
END SUBROUTINE AddCompiledByte
!
FUNCTION MathItemIndex (i, F, Var) RESULT (n)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Return math item index, if item is real number, enter it into Comp-structure
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  integer,                         INTENT(in) :: i         ! Function identifier
  CHARACTER (LEN=*),               INTENT(in) :: F         ! Function substring
  type(charv),DIMENSION(:), INTENT(in)        :: Var       ! Array with variable names
  integer                                     :: n         ! Byte value of math item
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  n = 0
  IF (SCAN(F(1:1),'0123456789.') > 0) THEN                 ! Check for begin of a number
     Comp(i)%ImmedSize = Comp(i)%ImmedSize + 1
     IF (ASSOCIATED(Comp(i)%Immed)) Comp(i)%Immed(Comp(i)%ImmedSize) = RealNum (F)
     n = cImmed
  ELSE                                                     ! Check for a variable
     n = VariableIndex (F, Var)
     IF (n > 0) n = VarBegin+n-1
  END IF
END FUNCTION MathItemIndex
!
FUNCTION CompletelyEnclosed (F, b, e) RESULT (res)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Check if function substring F(b:e) is completely enclosed by a pair of parenthesis
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(in) :: F                       ! Function substring
  integer,           INTENT(in) :: b, e                    ! First and last pos. of substring
  LOGICAL                       :: res
  integer                       :: j, k
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  res=.false.
  IF (F(b:b) == '(' .AND. F(e:e) == ')') THEN
     k = 0
     DO j=b+1,e-1
        IF     (F(j:j) == '(') THEN
           k = k+1
        ELSEIF (F(j:j) == ')') THEN
           k = k-1
        END IF
        IF (k < 0) EXIT
     END DO
     IF (k == 0) res=.true.                                ! All opened parenthesis closed
  END IF
END FUNCTION CompletelyEnclosed
!
RECURSIVE SUBROUTINE CompileSubstr (i, F, b, e, Var)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Compile i-th function string F into bytecode
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  integer,                         INTENT(in) :: i         ! Function identifier
  CHARACTER (LEN=*),               INTENT(in) :: F         ! Function substring
  integer,                         INTENT(in) :: b,e       ! Begin and end position substring
  type(charv), DIMENSION(:), INTENT(in)       :: Var       ! Array with variable names
  integer                                     :: n
  integer                                     :: b2,j,k,io
  CHARACTER (LEN=*),                PARAMETER :: calpha = 'abcdefghijklmnopqrstuvwxyz'// &
                                                          'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Check for special cases of substring
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IF     (F(b:b) == '+') THEN                              ! Case 1: F(b:e) = '+...'
       !  WRITE(*,*)'1. F(b:e) = "+..."'
     CALL CompileSubstr (i, F, b+1, e, Var)
     RETURN
  ELSEIF (CompletelyEnclosed (F, b, e)) THEN               ! Case 2: F(b:e) = '(...)'
       !  WRITE(*,*)'2. F(b:e) = "(...)"'
     CALL CompileSubstr (i, F, b+1, e-1, Var)
     RETURN
  ELSEIF (SCAN(F(b:b),calpha) > 0) THEN
     n = MathFunctionIndex (F(b:e))
     IF (n > 0) THEN
        b2 = b+INDEX(F(b:e),'(')-1
        IF (CompletelyEnclosed(F, b2, e)) THEN             ! Case 3: F(b:e) = 'fcn(...)'
              !   WRITE(*,*)'3. F(b:e) = "fcn(...)"'
           CALL CompileSubstr(i, F, b2+1, e-1, Var)
           CALL AddCompiledByte (i, n)
           RETURN
        END IF
     END IF
  ELSEIF (F(b:b) == '-') THEN
     IF (CompletelyEnclosed (F, b+1, e)) THEN              ! Case 4: F(b:e) = '-(...)'
           !  WRITE(*,*)'4. F(b:e) = "-(...)"'
        CALL CompileSubstr (i, F, b+2, e-1, Var)
        CALL AddCompiledByte (i, cNeg)
        RETURN
     ELSEIF (SCAN(F(b+1:b+1),calpha) > 0) THEN
        n = MathFunctionIndex (F(b+1:e))
        IF (n > 0) THEN
           b2 = b+INDEX(F(b+1:e),'(')
           IF (CompletelyEnclosed(F, b2, e)) THEN          ! Case 5: F(b:e) = '-fcn(...)'
                 !  WRITE(*,*)'5. F(b:e) = "-fcn(...)"'
              CALL CompileSubstr(i, F, b2+1, e-1, Var)
              CALL AddCompiledByte (i, n)
              CALL AddCompiledByte (i, cNeg)
              RETURN
           END IF
        END IF
     end if
  END IF
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Check for operator in substring: check only base level (k=0), exclude expr. in ()
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  DO io=cAdd,cPow                                          ! Increasing priority +-*/^
     k = 0
     DO j=e,b,-1
        IF     (F(j:j) == ')') THEN
           k = k+1
        ELSEIF (F(j:j) == '(') THEN
           k = k-1
        END IF
        IF (k == 0 .AND. F(j:j) == Ops(io) .AND. IsBinaryOp (j, F)) THEN
           IF (ANY(F(j:j) == Ops(cMul:cPow)) .AND. F(b:b) == '-') THEN ! Case 6: F(b:e) = '-...Op...' with Op > -
                  !   WRITE(*,*)'6. F(b:e) = "-...Op..." with Op > -'
              CALL CompileSubstr (i, F, b+1, e, Var)
              CALL AddCompiledByte (i, cNeg)
              RETURN
           ELSE                                                        ! Case 7: F(b:e) = '...BinOp...'
                  !  WRITE(*,*)'7. Binary operator',F(j:j)
              CALL CompileSubstr (i, F, b, j-1, Var)
              CALL CompileSubstr (i, F, j+1, e, Var)
              CALL AddCompiledByte (i, OperatorIndex(Ops(io)))
              Comp(i)%StackPtr = Comp(i)%StackPtr - 1
              RETURN
           END IF
        END IF
     END DO
  END DO
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Check for remaining items, i.e. variables or explicit numbers
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  b2 = b
  IF (F(b:b) == '-') b2 = b2+1
  n = MathItemIndex(i, F(b2:e), Var)
  CALL AddCompiledByte (i, n)
  Comp(i)%StackPtr = Comp(i)%StackPtr + 1
  IF (Comp(i)%StackPtr > Comp(i)%StackSize) Comp(i)%StackSize = Comp(i)%StackSize + 1
  IF (b2 > b) CALL AddCompiledByte (i, cNeg)
END SUBROUTINE CompileSubstr
!
FUNCTION IsBinaryOp (j, F) RESULT (res)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Check if operator F(j:j) in string F is binary operator
  ! Special cases already covered elsewhere:              (that is corrected in v1.1)
  ! - operator character F(j:j) is first character of string (j=1)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
 use UR_Gleich,           only: FP_for_units

  IMPLICIT NONE
  integer(4),        INTENT(in) :: j                       ! Position of Operator
  CHARACTER (LEN=*), INTENT(in) :: F                       ! String
  LOGICAL                       :: res                     ! Result
  integer(4)                    :: k
  LOGICAL                       :: Dflag,Pflag
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  res=.true.
  IF (F(j:j) == '+' .OR. F(j:j) == '-') THEN               ! Plus or minus sign:
     IF (j == 1) THEN                                      ! - leading unary operator ?
        res = .false.
     !! ELSEIF (SCAN(F(j-1:j-1),'+-*/^(') > 0) THEN           ! - other unary operator ?
     elseif( (.not.FP_for_units .and. SCAN(F(j-1:j-1),'+-*/^(') > 0)  .or.  &
             (FP_for_units .and. SCAN(F(j-1:j-1),'+-*/^(') > 0) ) then
        res = .false.
     ELSEIF (SCAN(F(j+1:j+1),'0123456789') > 0 .AND. &     ! - in exponent of real number ?
             SCAN(F(j-1:j-1),'eEdD')       > 0) THEN
        Dflag=.false.; Pflag=.false.
        k = j-1
        DO WHILE (k > 1)                                   !   step to the left in mantissa
           k = k-1
           IF     (SCAN(F(k:k),'0123456789') > 0) THEN
              Dflag=.true.
           ELSEIF (F(k:k) == '.') THEN
              IF (Pflag) THEN
                 EXIT                                      !   * EXIT: 2nd appearance of '.'
              ELSE
                 Pflag=.true.                              !   * mark 1st appearance of '.'
              end if
           ELSE
              EXIT                                         !   * all other characters
           END IF
        END DO
        ! IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
        if(.not.FP_for_units) then
          IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
        else
          IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
        end if
     END IF
  END IF
END FUNCTION IsBinaryOp
!
FUNCTION RealNum (str, ibegin, inext, error) RESULT (res)

  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Get real number from string - Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(in)    :: str                    ! String
  REAL(rn)                          :: res                    ! Real number
  integer(4), OPTIONAL, INTENT(out) :: ibegin,              & ! Start position of real number
                                       inext                  ! 1st character after real number
  LOGICAL, OPTIONAL, INTENT(out)    :: error                  ! Error flag
  integer(4)                        :: ib,in,istat
  LOGICAL                           :: Bflag,               & ! .T. at begin of number in str
                                       InMan,               & ! .T. in mantissa of number
                                       Pflag,               & ! .T. after 1st '.' encountered
                                       Eflag,               & ! .T. at exponent identifier 'eEdD'
                                       InExp,               & ! .T. in exponent of number
                                       DInMan,              & ! .T. if at least 1 digit in mant.
                                       DInExp,              & ! .T. if at least 1 digit in exp.
                                       err                    ! Local error flag
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  Bflag=.true.; InMan=.false.; Pflag=.false.; Eflag=.false.; InExp=.false.
  DInMan=.false.; DInExp=.false.
  ib   = 1
  in   = 1
  DO WHILE (in <= LEN_TRIM(str))
     SELECT CASE (str(in:in))
     CASE (' ')                                            ! Only leading blanks permitted
        ib = ib+1
        IF (InMan .OR. Eflag .OR. InExp) EXIT
     CASE ('+','-')                                        ! Permitted only
        IF     (Bflag) THEN
           InMan=.true.; Bflag=.false.                     ! - at beginning of mantissa
        ELSEIF (Eflag) THEN
           InExp=.true.; Eflag=.false.                     ! - at beginning of exponent
        ELSE
           EXIT                                            ! - otherwise STOP
        end if
     CASE ('0':'9')                                        ! Mark
        IF     (Bflag) THEN
           InMan=.true.; Bflag=.false.                     ! - beginning of mantissa
        ELSEIF (Eflag) THEN
           InExp=.true.; Eflag=.false.                     ! - beginning of exponent
        end if
        IF (InMan) DInMan=.true.                           ! Mantissa contains digit
        IF (InExp) DInExp=.true.                           ! Exponent contains digit
     CASE ('.')
        IF     (Bflag) THEN
           Pflag=.true.                                    ! - mark 1st appearance of '.'
           InMan=.true.; Bflag=.false.                     !   mark beginning of mantissa
        ELSEIF (InMan .AND..NOT.Pflag) THEN
           Pflag=.true.                                    ! - mark 1st appearance of '.'
        ELSE
           EXIT                                            ! - otherwise STOP
        END IF
     CASE ('e','E','d','D')                                ! Permitted only
        IF (InMan) THEN
           Eflag=.true.; InMan=.false.                     ! - following mantissa
        ELSE
           EXIT                                            ! - otherwise STOP
        end if
     CASE DEFAULT
        EXIT                                               ! STOP at all other characters
     END SELECT
     in = in+1
  END DO
  err = (ib > in-1) .OR. (.NOT.DInMan) .OR. ((Eflag.OR.InExp).AND..NOT.DInExp)
  IF (err) THEN
     res = 0.0_rn
  ELSE

     READ(str(ib:in-1),*,IOSTAT=istat) res
     err = istat /= 0
  END IF
  IF (PRESENT(ibegin)) ibegin = ib
  IF (PRESENT(inext))  inext  = in
  IF (PRESENT(error))  error  = err
END FUNCTION RealNum
!
SUBROUTINE LowCase (str1, str2)
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  ! Transform upper case letters in str1 into lower case letters, result is str2
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  IMPLICIT NONE
  CHARACTER (LEN=*),  INTENT(in) :: str1
  CHARACTER (LEN=*), INTENT(out) :: str2
  integer(4)                     :: j,k
  CHARACTER (LEN=*),   PARAMETER :: lc = 'abcdefghijklmnopqrstuvwxyz'
  CHARACTER (LEN=*),   PARAMETER :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  !----- -------- --------- --------- --------- --------- --------- --------- -------
  str2 = str1
  DO j=1,LEN_TRIM(str1)
     k = INDEX(uc,str1(j:j))
     IF (k > 0) str2(j:j) = lc(k:k)
  END DO
END SUBROUTINE LowCase
!

subroutine FdExpand(func, funcnew,iret)
use UR_Perror
USE UR_Variables,        ONLY: langg
use Rout,                only: MessageShow
use gtk,                 only: GTK_MESSAGE_WARNING
use UR_Gleich,           only: kableitnum
use UR_DLIM,             only: iteration_on

implicit none
character(len=*),  intent(in)                 :: func
character(len=len(func)+2*120),intent(out)    :: funcnew
integer(4), intent(out)                       :: iret

integer(4)          :: i1,i2,i3,iend,k,j,jj,nc,ntrial,i,ie2,iend2,resp,jlen
integer(4)          :: nbopen,nbclose
character(len=25)   :: var1(3)
character(len=120)  :: fdformula
character(len=150)  :: str1
logical             :: LG1,LG2

iret = 0
jj = 0
var1 = ' '
funcnew = trim(func)

10    continue
jj = jj + 1
i1 = index(funcnew,'FD(')
           ! write(66,*) 'fdexpand: i1=',i1,' funcnew=',trim(funcnew)
LG1 = .false.
LG2 = .false.
LG1 = i1 == 1
if(i1 > 1) LG2 = SCAN(funcnew(i1-1:i1-1),'(+-*/^) ') > 0
if(LG1 .or. LG2) then
    iend = index(funcnew(i1:),')')
    ntrial = 0
15        nc = 0
    nbopen = 0
    nbclose = 0
    ! test for number of commas between fd brackets:
    do i=i1,i1+iend-1
      if(funcnew(i:i) == ',') nc = nc + 1
      if(funcnew(i:i) == '(') nbopen = nbopen + 1
      if(funcnew(i:i) == ')') nbclose = nbclose + 1
    end do
    if(nbopen /= nbclose) then
      iend = iend + index(funcnew(i1+iend:), ')')
      goto 15
    end if
    if(nc < 2) then
      ! less than 1 comma found, i.e. < 3 arguments of fd(): search for next')':
      iend2 = i1+iend
      ie2 = index(funcnew(iend2:),',')
      if(ie2 == 0) then
        ifehlp = 1
        IF(langg == 'EN') WRITE(str1,*) '*** Parser error k1: less than 3 arguments of function fd( , , ) found'
        IF(langg == 'DE') WRITE(str1,*) '*** Parser Fehler k1: weniger als 3 Argumente der Funktion fd( , , ) gefunden'
        IF(langg == 'FR') WRITE(str1,*) '*** Parser erreur k1: moins de 3 arguments de la fonction fd (,,) trouvés'
        call MessageShow(trim(str1), GTK_BUTTONS_OK, "FdExpand:", resp,mtype=GTK_MESSAGE_WARNING)
        return
      end if
    end if
    ntrial = ntrial + 1
    if(ntrial == 4 .or. nc > 2) then
      ifehlp = 1
      IF(langg == 'EN') WRITE(str1,*) '*** Parser error k2: less than 3 arguments of function fd( , , ) found'
      IF(langg == 'DE') WRITE(str1,*) '*** Parser Fehler k2: weniger als 3 Argumente der Funktion fd( , , ) gefunden'
      IF(langg == 'FR') WRITE(str1,*) '*** Parser erreur k2: moins de 3 arguments de la fonction fd (,,) trouvés'
      call MessageShow(trim(str1), GTK_BUTTONS_OK, "FdExpand:", resp,mtype=GTK_MESSAGE_WARNING)
      return
    elseif( nc < 2) then
      iend = iend + index(funcnew(i1+iend:),')')
      goto 15
    end if
    if(iend > 0) then
      i2 = index(funcnew(i1:i1+iend-1),',')    ! searching for the first comma
      if(i2 > 0) then
        i3 = index(funcnew(i1+i2+1:i1+iend-1),',')    ! searching for the second comma
        if(i3 > 0) then
          iret = 1
          var1(1) = adjustL(trim(funcnew(i1+3: i1+i2-2)))
          var1(2) = adjustL(trim(funcnew(i1+i2: i1+i2+1+i3-2)))
          var1(3) = adjustL(trim(funcnew(i1+i2+i3+1: i1+iend-2)))
          do k=1,3
            jlen = len_trim(var1(k))
            do j=1,jlen
              if(SCAN(var1(k)(j:j),'+-*/^') > 0) then
                if(.not. var1(k)(1:1) == '(' .and. .not. var1(k)(jlen:jlen) == ')' ) then
                  var1(k) = '('//trim(var1(k))//')'
                  exit
                end if
              end if
            end do
          end do
          fdformula = '(EXP(-'//trim(var1(3))//'*'//trim(var1(1))//  &
              ')*(1-EXP(-'//trim(var1(3))//'*'//trim(var1(2))//'))/('//trim(var1(3))//'*'//trim(var1(2))//'))'
          funcnew = trim(funcnew(1:i1-1)) // trim(fdformula) // trim(funcnew(i1+iend:))
         ! if(.not.iteration_on .and. kableitnum == 0) write(66,*) 'funcnew=',trim(funcnew)
          goto 10
        end if
      end if
    end if
  end if
! end if

end subroutine FdExpand


END MODULE fparser
