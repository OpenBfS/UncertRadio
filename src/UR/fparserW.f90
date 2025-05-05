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

module fparser
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
    use, intrinsic :: iso_c_binding,         only: c_int, c_null_char
    use UR_types
    use ur_params,             only: EPS1MIN
    use gtk,                   only: gtk_buttons_ok
    use UR_Gleich_globals,             only: charv

    implicit none
!------- -------- --------- --------- --------- --------- --------- --------- -------
    public                     :: initf,    & ! initialize function parser for n functions
                                  parsef,   & ! parse single function string
                                  evalf,    & ! evaluate single function
                                  evalerrmsg  ! error message (use only when evalerrtype>0)
    integer, public            :: evalerrtype ! =0: no error occured, >0: evaluation error
!------- -------- --------- --------- --------- --------- --------- --------- -------
    private
    save
    integer,                                   parameter :: cImmed   = 1,          &
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
    character (len=1), dimension(cadd:cpow),  parameter :: Ops       = (/ '+',     &
                                                                        '-',     &
                                                                        '*',     &
                                                                        '/',     &
                                                                        '^' /)
    character (len=5), dimension(cabs:cln), parameter :: Funcs       = (/ 'abs  ', &     !###
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
    type tcomp
        integer, dimension(:), pointer     :: bytecode
        integer                            :: bytecodesize
        real(rn),    dimension(:), pointer :: immed
        integer                            :: immedsize
        integer                            :: stacksize, &
                                              stackptr
    end type tcomp

    type (tcomp),  dimension(:),  pointer :: comp              ! bytecode
    integer, dimension(:),  allocatable :: ipos                ! Associates function strings

!
contains
!
    subroutine initf (n)

        use UR_Gleich_globals,   only: fp_equat, fp_numeq
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! initialize function parser for n functions
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        integer, intent(in) :: n                                 ! number of functions
        integer             :: i
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        allocate (comp(n))
        if(allocated(fp_equat)) deallocate(fp_equat)
        allocate(fp_equat(n))
        do i=1,n
            nullify(comp(i)%bytecode, comp(i)%immed)
            fp_equat(i)%s = ' '
        end do
        fp_numeq = 0

    end subroutine initf
    !
    subroutine parsef (i, funcstr, vart)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! parse ith function string funcstr and compile it into bytecode
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        use, intrinsic :: iso_c_binding,      only: c_int
        use ur_perror
        use ur_general_globals,       only: fd_found

        use rout,               only: messageshow
        use UR_Gleich_globals,          only: fp_numeq,fp_equat,ifehl
        use gtk,                only: gtk_message_warning
        use chf,                only: ucase
        use translation_module, only: T => get_translation

        implicit none
        integer   ,               intent(in) :: i         ! function identifier
        character (len=*),        intent(in) :: funcstr   ! function string
        type(charv),intent(in)               :: vart(:)   ! array with variable names
        character (len=len(funcstr))         :: func      ! function string, local use
        character(len=150)                   :: str1
        character(len=len(funcstr)+2*120)    :: fupper
        character(len=len(funcstr)+2*120)    :: funcmodif
        integer(c_int)                       :: resp
        integer                              :: iret,j,nnv,ixx,maxlen
        type(charv),allocatable              :: var(:)       ! array with variable names  ! 2020-04-30 kn
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ifehlp = 0

        ! 2020-04-08: kn
        if(allocated(var)) deallocate(var)
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

        if (i < 1 .or. i > size(comp)) then
            write(str1,*) T("Parser error: Equation number") // " ", i, &
                          T("out of range") // " ", Size(Comp)

            call MessageShow(trim(str1), GTK_BUTTONS_OK, "fparser:", resp,mtype=GTK_MESSAGE_WARNING)
            ifehlp = 1
            return
        end if
        kequation = i
        fupper = ucase(funcstr)
        if(allocated(ipos)) deallocate(ipos)
        allocate (ipos(len_trim(fupper)))           !kn          ! char. positions in orig. string
        func = fupper                               !kn          ! local copy of function string
        call replace ('**','^ ',func)                            ! exponent into 1-char. format
        call removespaces (func)                                 ! condense function string
        ! kn
        funcmodif = func
        if(fd_found(i)) then
            call fdexpand(func, funcmodif, iret)
            fupper = trim(funcmodif)
        end if
        call checksyntax (funcmodif, fupper, var)

        deallocate (ipos)
        if(ifehlp == 1) then
            ifehl = 1
            return
        end if
        call compile(i, funcmodif, var)                            ! compile into bytecode

        if(i > fp_numeq) fp_numeq = i
        fp_equat(i)%s = trim(fupper)

        deallocate(var)

    end subroutine parsef


    function evalf(i, gval) result (res)
        use UR_Gleich_globals,      only: stdunc, apply_units_dir, fp_for_units, unit_conv_fact

        use ur_perror

        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! Evaluate bytecode of ith function for the values passed in array Val(:)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none

        real(rn), dimension(:), intent(in)       :: gval               ! Variable values

        integer, intent(in)                      :: i                  ! Function identifier
        real(rn), dimension(ubound(gval, dim=1)) :: val                ! Variable values
        real(rn)                                 :: res                ! Result
        integer                                  :: ip,              & ! Instruction pointer
                                                    dp,              & ! Data pointer
                                                    sp                 ! Stack pointer
        real(rn), parameter                      :: zero = 0._rn
        integer                  :: iimx
        integer                  :: isymb(ubound(gval, dim=1))         ! introduced on 2019-11-27
        ! function uval(x) introduced: 2018-02-11
        type(tcomp)              :: tmp_comp              ! tmp_bytecode
        real(rn), dimension(comp(i)%stacksize)   :: tmp_stack
        !----- -------- --------- --------- --------- --------- --------- --------- -------

        if(apply_units_dir .or. fp_for_units) then
            iimx = ubound(unit_conv_fact, dim=1)

            val(1:iimx) = gval(1:iimx) * unit_conv_fact(1:iimx)
            val(iimx+1:ubound(gval, dim=1)) = gval(iimx+1:ubound(gval, dim=1))
        else
            val = gval
        end if
        !------------------------------------------------------------------------------------
        dp = 1
        sp = 0
        isymb = 0
        res = zero
        tmp_comp = comp(i)

        do ip = 1, tmp_comp%bytecodesize
            select case (tmp_comp%bytecode(ip))
                case (cimmed)
                    sp = sp + 1
                    tmp_stack(sp) = tmp_comp%immed(dp)
                    dp = dp + 1
                case (cneg)
                    tmp_stack(sp) = -tmp_stack(sp)
                case (cadd)
                    tmp_stack(sp-1) = tmp_stack(sp-1) + tmp_stack(sp)
                    sp = sp - 1
                case (csub)
                    tmp_stack(sp-1) = tmp_stack(sp-1) - tmp_stack(sp)
                    sp = sp - 1
                case (cmul)
                    tmp_stack(sp-1) = tmp_stack(sp-1) * tmp_stack(sp)
                    sp = sp - 1
                case (cdiv)
                    if (abs(tmp_stack(sp)) < EPS1MIN) then
                        evalerrtype = 1
                        return
                    end if
                    tmp_stack(sp-1) = tmp_stack(sp-1) / tmp_stack(sp)
                    sp = sp - 1
                case (cpow)
                    tmp_stack(sp-1) = tmp_stack(sp-1)**tmp_stack(sp)
                    sp = sp - 1
                case (cabs)
                    tmp_stack(sp) = abs(tmp_stack(sp))
                case (cexp)
                    tmp_stack(sp) = exp(tmp_stack(sp))
                case (clog10)
                    if (tmp_stack(sp)<=zero) then
                        evalerrtype = 3
                        return
                    end if
                    tmp_stack(sp) = log10(tmp_stack(sp))
                case (clog,cln)
                    if (tmp_stack(sp)<=zero) then
                        evalerrtype = 3
                        return
                    end if
                    tmp_stack(sp) = log(tmp_stack(sp))
                case (csqrt)
                    if (tmp_stack(sp)< zero) then
                        evalerrtype = 3
                        return
                    end if
                    tmp_stack(sp) = sqrt(tmp_stack(sp))
                case (csinh)
                    tmp_stack(sp) = sinh(tmp_stack(sp))
                case (ccosh)
                    tmp_stack(sp) = cosh(tmp_stack(sp))
                case (ctanh)
                    tmp_stack(sp) = tanh(tmp_stack(sp))
                case (csin)
                    tmp_stack(sp) = sin(tmp_stack(sp))
                case (ccos)
                    tmp_stack(sp) = cos(tmp_stack(sp))
                case (ctan)
                    tmp_stack(sp) = tan(tmp_stack(sp))
                case (casin)
                    if ((tmp_stack(sp)< -1._rn) .or. (tmp_stack(sp)> 1._rn)) then
                        evalerrtype = 4
                        return
                    end if
                    tmp_stack(sp) = asin(tmp_stack(sp))
                case (cacos)
                    if ((tmp_stack(sp)< -1._rn) .or. (tmp_stack(sp) > 1._rn)) then
                        evalerrtype = 4
                        return
                    end if
                    tmp_stack(sp) = acos(tmp_stack(sp))
                case (catan)
                    tmp_stack(sp) = atan(tmp_stack(sp))
                case (cuval)
                    if(isymb(sp) > 0) then
                        tmp_stack(sp)= stdunc(isymb(sp))     ! since 2019-22-27
                    end if
                case default
                    sp = sp + 1
                    isymb(sp) = tmp_comp%bytecode(ip) - varbegin + 1
                    if (isymb(sp) <= 0) then
                        evalerrtype = 4
                        return
                    end if
                    tmp_stack(sp) = val(isymb(sp))
            end select
        end do
        EvalErrType = 0
        if(ubound(tmp_stack, dim=1) > 0) then
            res = tmp_stack(1)
        else
            ifehlp = 1
            res = zero
        end if

    end function evalf
    !
    subroutine checksyntax (func, funcstr, var)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! check syntax of function string,  returns 0 if syntax is ok
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        use ur_perror
        use translation_module, only: T => get_translation

        implicit none
        character (len=*),               intent(in) :: func      ! function string without spaces
        character (len=*),               intent(in) :: funcstr   ! original function string
        type(charv), dimension(:), intent(in)       :: var       ! array with variable names

        integer                                     :: n
        character (len=1)                           :: c
        real(rn)                                    :: r
        logical                                     :: err
        integer                                     :: parcnt   ! parenthesis counter
        integer                                     :: j, ib, in
        integer                                     :: lfunc    ! length of trimmed func

        integer                                     :: jjj
        real(rn)                                    :: help
        integer                                     :: ios,i
        character(len=len_trim(funcstr))            :: fstr

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
                    CALL ParseErrMsg (j, FuncStr, T("Missing operand"))
                    ifehlp = 1
                END IF
                c = Func(j:j)
                IF (ANY(c == Ops)) THEN
                    CALL ParseErrMsg (j, FuncStr, T("Multiple operators"))
                    ifehlp = 1
                END IF
            END IF
            n = MathFunctionIndex (Func(j:))
            IF (n > 0) THEN                                       ! Check for math function
                j = j+LEN_TRIM(Funcs(n))
                IF (j > lFunc) THEN
                    CALL ParseErrMsg (j, FuncStr, T("Missing function argument"))
                    ifehlp = 1
                END IF
                c = Func(j:j)
                IF (c /= '(') THEN
                    CALL ParseErrMsg (j, FuncStr, T("Missing opening parenthesis"))
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
                    CALL ParseErrMsg(j, FuncStr, T("Invalid number format") // ":  " // &
                                     Func(j+ib-1:j+in-2))

                    ifehlp = 1
                END IF
                j = j+in-1
                IF (j > lFunc) EXIT
                c = Func(j:j)
            ELSE                                                  ! Check for variable
                n = VariableIndex (Func(j:),Var,ib,in)

                IF (n == 0) THEN   ! original
                    ifehlp = 1

                    CALL ParseErrMsg (j, FuncStr, T("Invalid symbol")  // ":  " // &
                                      new_line('A') // Func(j+ib-1:j+in-1))


                    ! WRITE(66,*) 'invalid element: Func(j+ib-1:j+in-2)="',Func(j+ib-1:j+in-2),'"  j,ib,in=',j,ib,in
                    write(66,*) 'invalid element: Func(j+ib-1:j+in-1)="',Func(j+ib-1:j+in-1),'"  j,ib,in=',j,ib,in
                    write(66,*) '   Funcstr=',trim(Funcstr)
                    write(66,*) '   Func(j:)=',trim(Func(j:))

                    write(66,*) 'List of variables: ',(trim(var(jjj)%s),' ',jjj=1,SIZE(var))
                end if
                j = j+in-1
                IF (j > lFunc) EXIT
                c = Func(j:j)   ! original
            end if
            do while (c == ')')                                   ! Check for closing parenthesis
                ParCnt = ParCnt-1
                IF (ParCnt < 0) THEN
                    call ParseErrMsg (j, FuncStr, T("Mismatched parenthesis"))
                    ifehlp = 1
                END IF
                if(j > 1) then
                    IF (Func(j-1:j-1) == '(') THEN
                        call ParseErrMsg (j-1, FuncStr, T("Empty parentheses"))
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
            IF (j > lFunc) exit
            IF (ANY(c == Ops)) then                               ! Check for multiple operators
                IF (j+1 > lFunc) CALL ParseErrMsg (j, FuncStr)
                IF (ANY(Func(j+1:j+1) == Ops)) THEN
                    CALL ParseErrMsg (j+1, FuncStr, T('Multiple operators'))
                    ifehlp = 1
                end if
            else                            ! Check for next operand
                ! test Funcstr for being only a number:
                fstr = Funcstr
                do i=1,len_trim(fstr)
                    if(fstr(i:i) == ',') fstr(i:i) = '.'
                end do

                read(fstr,*,iostat=ios) help

                if(ios /= 0) then
                    ifehlp = 1
                    call ParseErrMsg (j, FuncStr, T('Missing operand'))
                end if
            end if
            !-- -------- --------- --------- --------- --------- --------- --------- -------
            ! Now, we have an operand and an operator: the next loop will check for another
            ! operand (must appear)
            !-- -------- --------- --------- --------- --------- --------- --------- -------
            j = j+1
            if(ifehlp == 1) return
        END DO step
        IF (ParCnt > 0) THEN
            call ParseErrMsg(j, FuncStr, T('Missing closing parenthesis'))
            ifehlp = 1
        end if
    end subroutine checksyntax
    !
    function evalerrmsg () result (msg)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! return error message
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character (len=*), dimension(4), parameter :: m = (/ 'Division by zero                ', &
            'Argument of SQRT negative       ', &
            'Argument of LOG negative        ', &
            'Argument of ASIN or ACOS illegal' /)
        character (len=len(m))                     :: msg
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        if (evalerrtype < 1 .or. evalerrtype > size(m)) then
            msg = ''
        else
            msg = m(evalerrtype)
        end if
    end function evalerrmsg
    !
    subroutine parseerrmsg (j, funcstr, msg)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! print error message and terminate program
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        use ur_perror

        use gtk,                  only: gtk_buttons_yes_no,gtk_response_yes,gtk_message_warning
        use rout,                 only: messageshow
        use top,                  only: charmoda1
        use chf,                  only: ucase
        use translation_module,   only: T => get_translation

        implicit none
        integer,                     intent(in) :: j             ! number of character within function string
        character(len=*),            intent(in) :: funcstr       ! original function string
        character(len=*), optional,  intent(in) :: msg

        integer                     :: k,i,jjh,i1,nfd
        character(len=200)          :: str1
        character(len=400)          :: str2
        character(len=600)          :: str3
        character(len=60)           :: str0,symbb,str4
        integer(c_int)              :: resp
        logical                     :: invalidsymb
        !----- -------- --------- --------- --------- --------- --------- --------- -------

        write(str0,'(a,i3,a)') T('Equation') // ' ', kequation, ' : '
        write(str1,*) T("Error in syntax of function string")

        IF (PRESENT(Msg)) str1 = trim(str1) // ': '// trim(Msg)
        ifehlp = 1

        ! catch a new Symbol in equations, the user checks: correct of wrong characters?
        invalidSymb = .false.
        if(index(msg,'Invalid symbol') > 0 .or. index(msg,'Ungültiges Symbol') > 0  .or.    &
            index(msg,'Symbole invalide') > 0)           invalidSymb = .true.

        write(str4,'(a,i0,1x,a)') T("found from character no.") // " ", j, &
                                  T("in the string:") // " "

        str2 = ' '

        write(str2,'(a)') trim(funcstr)
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
                str3 = trim(str3) // char(13) // char(13) // T("Accept invalid (or new) symbol?")

                call MessageShow(trim(str3), GTK_BUTTONS_YES_NO, "ParseErrMsg:", &
                                 resp, mtype=GTK_MESSAGE_WARNING)
                if (resp == gtk_response_yes) then
                    ifehlp = 0
                    nfd = 0
                    do k=1,nsymbnew
                        if(trim(symb_new(k)%s) == trim(symbb)) nfd = 1
                    end do
                    if(nfd == 0) then
                        nsymbnew = nsymbnew + 1
                        call charmoda1(symb_new,nsymbnew)
                        symb_new(nsymbnew)%s = symbb
                    end if
                end if
            end if
        end if

    end subroutine parseerrmsg
!
    function operatorindex (c) result (n)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! return operator index
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character (len=1), intent(in) :: c
        integer                       :: n, j
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        n = 0
        do j=cadd,cpow
            if (c == ops(j)) then
                n = j
                exit
            end if
        end do
    end function operatorindex
!
    function mathfunctionindex (str) result (n)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! return index of math function beginnig at 1st position of string str
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character (len=*), intent(in) :: str
        integer                       :: n, j
        integer                       :: k
        character (len=len(funcs))    :: fun
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        n = 0
        !// do j=cabs,catan                                          ! check all math functions
        do j=cabs,cln                                                ! check all math functions     !###
            k = min(len_trim(funcs(j)), len(str))
            call lowcase (str(1:k), fun)
            if (fun == funcs(j)) then                             ! compare lower case letters
                n = j                                              ! found a matching function
                exit
            end if
        end do
    end function mathfunctionindex
!
    function variableindex (str, var, ibegin, inext) result (n)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! return index of variable at begin of string str (returns 0 if no variable found)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        use UR_Gleich_globals,      only: fp_for_units

        implicit none
        character (len=*),          intent(in) :: str       ! string
        type(charv), dimension(:), intent(in)  :: var       ! array with variable names
        integer                                :: n         ! index of variable
        integer, optional,      intent(out)    :: ibegin, & ! start position of variable name
            inext     ! position of character after name
        integer                                :: j,ib,in,lstr
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        n = 0
        ib = 0    ! 2025.01.24 GK
        in = 0    !
        lstr = len_trim(str)
        if (lstr > 0) then
            do ib=1,lstr                                          ! search for first character in str
                if (str(ib:ib) /= ' ') exit                        ! when lstr>0 at least 1 char in str
            end do
            if(ib > lstr) then    ! 2025.01.23 GK
                n = 0
                if (present(ibegin)) ibegin = 0
                if (present(inext))  inext = 0
                return
            end if

            do in=ib,lstr                                         ! search for name terminators
                if(.not.fp_for_units) then
                    if (scan(str(in:in),'+-*/^) ') > 0) exit
                else
                    if (scan(str(in:in),'+-*/^) ') > 0) exit        ! ^ not used as terminator
                end if
            end do
            do j=1,size(var)
                if (str(ib:in-1) == var(j)%s) then
                    n = j                                           ! variable name found
                    exit
                end if
            end do
        end if
        if (present(ibegin)) ibegin = ib
        if (present(inext))  inext  = in
    end function variableindex
!
    subroutine removespaces (str)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! remove spaces from string, remember positions of characters in old string
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character (len=*), intent(inout) :: str
        integer                          :: k, lstr
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        lstr = len_trim(str)
        ipos = (/ (k,k=1,lstr) /)
        k = 1
        do while (str(k:lstr) /= ' ')
            if (str(k:k) == ' ') then
                str(k:lstr)  = str(k+1:lstr)//' '                  ! move 1 character to left
                ipos(k:lstr) = (/ ipos(k+1:lstr), 0 /)             ! move 1 element to left
                k = k-1
            end if
            k = k+1
        end do
    end subroutine removespaces
!
    subroutine replace (ca,cb,str)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! replace all appearances of character set ca in string str by character set cb
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character (len=*),       intent(in) :: ca
        character (len=len(ca)), intent(in) :: cb                ! len(ca) must be len(cb)
        character (len=*),    intent(inout) :: str
        integer                             :: j, lca
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        lca = len(ca)
        do j=1,len_trim(str)-lca+1
            if (str(j:j+lca-1) == ca) str(j:j+lca-1) = cb
        end do
    end subroutine replace
!
    subroutine compile (i, f, var)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! compile i-th function string f into bytecode
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        use ur_perror
        use rout,                only: messageshow
        use gtk,                 only: gtk_message_warning
        use translation_module,  only: T => get_translation

        implicit none
        integer,                         intent(in) :: i         ! function identifier
        character (len=*),               intent(in) :: f         ! function string
        type(charv), dimension(:), intent(in)       :: var       ! array with variable names
        integer                                     :: istat
        integer(c_int)             :: resp
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        if (associated(comp(i)%bytecode)) deallocate ( comp(i)%bytecode, &
            comp(i)%immed)
        comp(i)%bytecodesize = 0
        comp(i)%immedsize    = 0
        comp(i)%stacksize    = 0
        comp(i)%stackptr     = 0
        call compilesubstr (i,f,1,len_trim(f),var)               ! compile string to determine size
        allocate ( comp(i)%bytecode(comp(i)%bytecodesize), &
            comp(i)%immed(comp(i)%immedsize),       &
            stat = istat                            )
        if (istat /= 0) then
            call MessageShow(T("Parser error: Memory allocation for byte code failed"), &
                             GTK_BUTTONS_OK, "fparser-compile:", &
                             resp,mtype=GTK_MESSAGE_WARNING)
            ifehlp = 1
        else
            comp(i)%bytecodesize = 0
            comp(i)%immedsize    = 0
            comp(i)%stacksize    = 0
            comp(i)%stackptr     = 0
            call compilesubstr (i,f,1,len_trim(f),var)            ! compile string into bytecode
        end if
        !
    end subroutine compile
!
    subroutine addcompiledbyte (i, b)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! add compiled byte to bytecode
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        integer,     intent(in) :: i                             ! function identifier
        integer,     intent(in) :: b                             ! value of byte to be added
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        comp(i)%bytecodesize = comp(i)%bytecodesize + 1
        if (associated(comp(i)%bytecode)) comp(i)%bytecode(comp(i)%bytecodesize) = b
    end subroutine addcompiledbyte
!
    function mathitemindex (i, f, var) result (n)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! return math item index, if item is real number, enter it into comp-structure
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        integer,                         intent(in) :: i         ! function identifier
        character (len=*),               intent(in) :: f         ! function substring
        type(charv),dimension(:), intent(in)        :: var       ! array with variable names
        integer                                     :: n         ! byte value of math item
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        n = 0
        if (scan(f(1:1),'0123456789.') > 0) then                 ! check for begin of a number
            comp(i)%immedsize = comp(i)%immedsize + 1
            if (associated(comp(i)%immed)) comp(i)%immed(comp(i)%immedsize) = realnum(f)
            n = cimmed
        else                                                     ! check for a variable
            n = variableindex (f, var)
            if (n > 0) n = varbegin+n-1
        end if
    end function mathitemindex
!
    function completelyenclosed (f, b, e) result (res)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! check if function substring f(b:e) is completely enclosed by a pair of parenthesis
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character (len=*), intent(in) :: f                       ! function substring
        integer,           intent(in) :: b, e                    ! first and last pos. of substring
        logical                       :: res
        integer                       :: j, k
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        res=.false.
        if (f(b:b) == '(' .and. f(e:e) == ')') then
            k = 0
            do j=b+1,e-1
                if     (f(j:j) == '(') then
                    k = k+1
                elseif (f(j:j) == ')') then
                    k = k-1
                end if
                if (k < 0) exit
            end do
            if (k == 0) res=.true.                                ! all opened parenthesis closed
        end if
    end function completelyenclosed
!
    recursive subroutine compilesubstr (i, f, b, e, var)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! compile i-th function string f into bytecode
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        integer,                         intent(in) :: i         ! function identifier
        character (len=*),               intent(in) :: f         ! function substring
        integer,                         intent(in) :: b,e       ! begin and end position substring
        type(charv), dimension(:), intent(in)       :: var       ! array with variable names
        integer                                     :: n
        integer                                     :: b2,j,k,io
        character (len=*),                parameter :: calpha = 'abcdefghijklmnopqrstuvwxyz'// &
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
                        return
                    end if
                end if
            end if
        end if
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
        call AddCompiledByte (i, n)
        Comp(i)%StackPtr = Comp(i)%StackPtr + 1
        IF (Comp(i)%StackPtr > Comp(i)%StackSize) Comp(i)%StackSize = Comp(i)%StackSize + 1
        IF (b2 > b) call AddCompiledByte (i, cNeg)
    end subroutine CompileSubstr
    !
    function IsBinaryOp (j, F) RESULT (res)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! Check if operator F(j:j) in string F is binary operator
        ! Special cases already covered elsewhere:              (that is corrected in v1.1)
        ! - operator character F(j:j) is first character of string (j=1)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        use UR_Gleich_globals,           only: fp_for_units

        implicit none
        integer   ,        intent(in) :: j                       ! position of operator
        character (len=*), intent(in) :: f                       ! string
        logical                       :: res                     ! result
        integer                       :: k
        logical                       :: dflag,pflag
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
                do while (k > 1)                                   !   step to the left in mantissa
                    k = k-1
                    IF     (SCAN(F(k:k),'0123456789') > 0) THEN
                        Dflag=.true.
                    elseif (f(k:k) == '.') then
                        if (pflag) then
                            exit                                      !   * exit: 2nd appearance of '.'
                        else
                            pflag=.true.                              !   * mark 1st appearance of '.'
                        end if
                    else
                        exit                                         !   * all other characters
                    end if
                end do
                ! IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
                if(.not.FP_for_units) then
                    IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
                else
                    IF (Dflag .AND. (k == 1 .OR. SCAN(F(k:k),'+-*/^(') > 0)) res = .false.
                end if
            end if
        end if
    end function isbinaryop
    !
    function realnum(str, ibegin, inext, error) result (res)

        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! Get real number from string - Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character (len=*), intent(in)     :: str                    ! string
        real(rn)                          :: res                    ! real number
        integer   , optional, intent(out) :: ibegin,              & ! start position of real number
                                             inext                  ! 1st character after real number
        logical, optional, intent(out)    :: error                  ! error flag
        integer                           :: ib,in,istat
        logical                           :: Bflag,               & ! .T. at begin of number in str
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
        do while (in <= len_trim(str))
            select case (str(in:in))
              case (' ')                                            ! only leading blanks permitted
                ib = ib+1
                if (inman .or. eflag .or. inexp) exit
              case ('+','-')                                        ! permitted only
                if     (bflag) then
                    inman=.true.; bflag=.false.                     ! - at beginning of mantissa
                elseif (eflag) then
                    inexp=.true.; eflag=.false.                     ! - at beginning of exponent
                else
                    exit                                            ! - otherwise stop
                end if
              case ('0':'9')                                        ! mark
                if     (bflag) then
                    inman=.true.; bflag=.false.                     ! - beginning of mantissa
                elseif (eflag) then
                    inexp=.true.; eflag=.false.                     ! - beginning of exponent
                end if
                if (inman) dinman=.true.                           ! mantissa contains digit
                if (inexp) dinexp=.true.                           ! exponent contains digit
              case ('.')
                if     (bflag) then
                    pflag=.true.                                    ! - mark 1st appearance of '.'
                    inman=.true.; bflag=.false.                     !   mark beginning of mantissa
                elseif (inman .and..not.pflag) then
                    pflag=.true.                                    ! - mark 1st appearance of '.'
                else
                    exit                                            ! - otherwise stop
                end if
              case ('e','E','d','D')                                ! permitted only
                if (inman) then
                    eflag=.true.; inman=.false.                     ! - following mantissa
                else
                    exit                                            ! - otherwise stop
                end if
              case default
                exit                                               ! stop at all other characters
            end select
            in = in+1
        end do
        err = (ib > in-1) .or. (.not.dinman) .or. ((eflag.or.inexp).and..not.dinexp)
        if (err) then
            res = 0.0_rn
        else

            read(str(ib:in-1),*,iostat=istat) res
            err = istat /= 0
        end if
        if (present(ibegin)) ibegin = ib
        if (present(inext))  inext  = in
        if (present(error))  error  = err
    end function realnum
    !
    subroutine lowcase (str1, str2)
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        ! Transform upper case letters in str1 into lower case letters, result is str2
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        implicit none
        character (len=*),  intent(in) :: str1
        character (len=*), intent(out) :: str2
        integer                        :: j,k
        character (LEN=*),   parameter :: lc = 'abcdefghijklmnopqrstuvwxyz'
        character (LEN=*),   parameter :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        !----- -------- --------- --------- --------- --------- --------- --------- -------
        str2 = str1
        do j=1,len_trim(str1)
            k = index(uc,str1(j:j))
            if (k > 0) str2(j:j) = lc(k:k)
        end do
    end subroutine lowcase
!

    subroutine FdExpand(func, funcnew, iret)
        use UR_Perror

        use Rout,                only: MessageShow
        use gtk,                 only: GTK_MESSAGE_WARNING
        use translation_module,  only: T => get_translation

        implicit none
        character(len=*),  intent(in)                 :: func
        character(len=len(func)+2*120),intent(out)    :: funcnew
        integer   , intent(out)                       :: iret

        integer             :: i1,i2,i3,iend,k,j,jj,nc,ntrial,i,ie2,iend2,resp,jlen
        integer             :: m0
        integer             :: nbopen,nbclose
        type(charv)         :: var1(3)                     ! variable type changed
        character(len=120)  :: fdformula
        logical             :: LG1, LG2, tm_is_null

        iret = 0
        jj = 0
        var1(1)%s = ' '; var1(2)%s = ' '; var1(3)%s = ' '       ! 29.4.2025
        funcnew = trim(func)

10      continue
        jj = jj + 1
        i1 = index(funcnew,'FD(')
        ! write(66,*) 'fdexpand: i1=',i1,' funcnew=',trim(funcnew)
        LG1 = .false.
        LG2 = .false.
        LG1 = i1 == 1
        ! if(i1 > 1) LG2 = SCAN(funcnew(i1-1:i1-1),'(+-*/^) ') > 0
        if(i1 > 1) LG2 = SCAN(funcnew(i1-1:i1-1),'(+-*/^) ') > 0         ! 2.5.2025

        if(LG1 .or. LG2) then
            iend = index(funcnew(i1:),')')
            ntrial = 0
15          nc = 0
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

                    call MessageShow("k1 " // T("Parser error: less than 3 arguments of function fd( , , ) found"), &
                                     GTK_BUTTONS_OK, "FdExpand:", resp,mtype=GTK_MESSAGE_WARNING)
                    return
                end if
            end if
            ntrial = ntrial + 1
            if(ntrial == 4 .or. nc > 2) then
                ifehlp = 1
                call MessageShow("k2 " // T("Parser error: less than 3 arguments of function fd( , , ) found"), &
                                     GTK_BUTTONS_OK, "FdExpand:", resp,mtype=GTK_MESSAGE_WARNING)
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
                        var1(1)%s = adjustL(trim(funcnew(i1+3: i1+i2-2)))
                        var1(2)%s = adjustL(trim(funcnew(i1+i2: i1+i2+1+i3-2)))
                        var1(3)%s = adjustL(trim(funcnew(i1+i2+i3+1: i1+iend-2)))
                        !.....  added 29.4.2025:   handle now the case of value '0' given for the count time
                        do k=1,3
                          jlen = len_trim(var1(k)%s)
                          do j=1,jlen
                            if(SCAN(var1(k)%s(j:j),'+-*/^') > 0) then
                              if(.not. var1(k)%s(1:1) == '(' .and. .not. var1(k)%s(jlen:jlen) == ')' ) then
                                var1(k)%s = '('//var1(k)%s//')'
                                exit
                              end if
                            end if
                          end do
                          if(k == 2) then
                            ! 16.1.2025  GK
                            ! check for a possible value tm = 0:
                            ! at first, remove blanks:
                            m0 = 1
                            do while (m0 > 0)
                              m0 = index(var1(2)%s,' ')
                              if(m0 > 0) then
                                if(m0 == 1) var1(2)%s = var1(2)%s(2:)
                                if(m0 > 1) var1(2)%s = var1(2)%s(1:m0-1) // var1(2)%s(m0+1:)
                              end if
                            end do
                            ! analyze possible ways to interpret a value 0 for tm:
                            tm_is_null = .false.
                            ! if(var1(2)%s == '0' .or. var1(2)%s(1:2) == '0.') tm_is_null = .true.
                            if(var1(2)%s == '0' .or. var1(2)%s == '0.') tm_is_null = .true.    ! 2.5.2025
                            if(index(var1(2)%s,'0*') > 0) tm_is_null = .true.
                            if(index(var1(2)%s,'0.*') > 0) tm_is_null = .true.
                            if(index(var1(2)%s,'*0') > 0) tm_is_null = .true.
                            if(index(var1(2)%s,'*0.') > 0) tm_is_null = .true.
                          end if
                        end do
                        !...................................................................
                        do k=1,3
                            jlen = len_trim(var1(k)%s)
                            do j=1,jlen
                                if(SCAN(var1(k)%s(j:j),'+-*/^') > 0) then
                                    if(.not. var1(k)%s(1:1) == '(' .and. .not. var1(k)%s(jlen:jlen) == ')' ) then
                                        var1(k)%s = '('//var1(k)%s//')'
                                        exit
                                    end if
                                end if
                            end do
                        end do
                        ! fdformula = '(EXP(-'//trim(var1(3))//'*'//trim(var1(1))//  &
                        !     ')*(1-EXP(-'//trim(var1(3))//'*'//trim(var1(2))//'))/('//trim(var1(3))//'*'//trim(var1(2))//'))'

                        ! take the required decision regarding tm > 0 or tm= 0:
                        !................................
                        if(.not.tm_is_null) then       ! 16.1.2025  GK
                          fdformula = '(EXP(-'//var1(3)%s //'*'// var1(1)%s //  &
                              ')*(1-EXP(-'//var1(3)%s//'*'//var1(2)%s//'))/('//var1(3)%s//'*'//var1(2)%s//'))'
                        else
                          fdformula = '(EXP(-'//var1(3)%s//'*'//var1(1)%s//'))'
                        end if
                        !................................

                        funcnew = trim(funcnew(1:i1-1)) // trim(fdformula) // trim(funcnew(i1+iend:))
                             !!! if(.not.iteration_on .and. kableitnum == 0) write(66,*) 'funcnew=',trim(funcnew)
                        goto 10
                    end if
                end if
            end if
        end if
    ! end if

    end subroutine FdExpand


end module fparser
