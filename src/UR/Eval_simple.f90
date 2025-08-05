 !    contains:
 ! seval
 ! parse
 ! parseExpression
 ! parseTerm
 ! parseFactor
 ! NextChar
 ! eat

 ! seval applies the function parser parse. The latter is based on:
 !  taken from: (formulated in Java, from 2010; translated here (GK) to Fortran 90)
 !  https://stackoverflow.com/questions/3422673/how-to-evaluate-a-math-expression-given-in-string-form


module xx

    use UR_types, only: rn

    character(len=512) :: str
    integer            :: numch,thisposxx      ! number of characters
    character(len=1)   :: ch                   ! a character
    logical            :: endstr=.false.
    integer, parameter :: kunit = 66
    integer            :: ke   ! hierachic level of a pair of brackets
    integer            :: ifehlxx        ! error indicator variable
    logical            :: expnum

end module xx

!##############################################################################

real(rn) function seval(String)

    use CHF, only: ucase
    use xx
    implicit none

    character(len=*),intent(in)   :: string
    real(rn)        :: parse

    str = '(' // trim(ucase(string)) // ')    '        ! <--  necessary operation

    ! write(66,*)' seval: str=',trim(str)       ! deactivated 28.7.2025

    ch = str(1:1)
    numch = 1
    thisPosxx = 1
    endstr = .false.
    ifehlxx = 0
    expnum = .false.

    seval = parse()

end function seval

!##############################################################################

!! taken from: (formulated in Java, from 2010; translated here to Fortran 90)
!!
!!https://stackoverflow.com/questions/3422673/how-to-evaluate-a-math-expression-given-in-string-form
!!

!        // Grammar:
!        // expression = term | expression `+` term | expression `-` term
!        // term = factor | term `*` factor | term `/` factor
!        // factor = `+` factor | `-` factor | `(` expression `)`
!        //        | number | functionName factor | factor `^` factor

real(rn) function parse()
    use xx, only: rn, endstr

    implicit none
    real(rn) :: parseExpression

    if(endstr) then
        parse = 0.0_rn
    else
        parse = parseExpression()
    end if

end function parse

!##############################################################################

recursive function parseExpression() result(x)
    use xx
    implicit none

    real(rn)         :: x,parseTerm
    integer          :: sttpos,stppos
    logical          :: eat

    sttPos = thisposxx
    x = parseTerm()
    if(endstr) return
    do
        if(eat('+')) then; x = x + parseTerm(); cycle;
        elseif(eat('-')) then; x = x - parseTerm(); cycle; end if;

        exit
    end do
    stpPos = thisposxx
    ! write(0,*) 'parseExpression: substring=',str(Sttpos:stppos)
end function parseExpression

!##############################################################################

recursive function parseTerm() result(x)
    use xx
    implicit none

    real(rn)         :: x, parseFactor
    logical          :: eat

    x = 1._rn           ! 2025.01.23 GK
    if(endstr) return

    x = parseFactor();
    do
        if(eat('*')) then; x = x * parseFactor(); cycle;
        elseif(eat('/')) then; x = x / parseFactor(); cycle; end if
        exit
    end do
end function parseTerm

!##############################################################################

recursive function parseFactor()  Result(x)
    use xx
    implicit none

    real(rn)            :: x,parseExpression
    logical            :: eat,xeat
    integer            :: startPos,strtPos,ios
    character(len=20)  :: funcstr
    integer            :: thisPos

    x = 1._rn           ! 2025.01.23 GK
    if(endstr) return

    if (eat('+')) then; x = parseFactor(); return; end if;   ! // unary plus;
    if (eat('-')) then; x = -parseFactor(); return; end if;   ! // unary minus

    thisPos = thisPosxx
    startPos = thisPos

    if(eat('(')) then
        ! brackets:
        thisPos = thisPosxx
        strtpos = thisPos
        x = parseExpression();
        xeat = eat(')');

    else if ((ch >= '0' .and. ch <= '9') .or. ch == '.' ) then
        !numbers:
        do while ((ch >= '0' .and. ch <= '9') .or. ch == '.' .or. (ch == 'E' .or. ch == 'e') .or. expnum)
            if(ch == 'E' .or. ch == 'e') Expnum = .true.
            call NextChar()
            if(expnum) then
                if(ch /= '+' .and. ch /= '-' .and. .not. (ch >= '0' .and. ch <= '9') ) then
                    expnum = .false.
                    exit
                end if
            end if
        end do
        thisPos = max(thisPosxx-1,startPos)
        ! read(str(startPos:thisPos),*) x
        read(str(startPos:thisPos),*,iostat=ios) x
        if(ios /= 0) then
            ifehlxx = 1
            write(kunit,*) 'EvalSimple: Error in: string=str(startPos:thisPos)=',str(startPos:thisPos)
        end if
    else if (ch >= 'A' .and. ch <= 'Z') then
        ! functions:
        thisPos = thisPosxx
        startPos = ThisPos
        do while (ch >= 'A' .and. ch <= 'Z')
            call NextChar()
        end do
        thisPos = max(thisPosxx-1,startPos)
        funcstr = str(startPos:thisPos)
        x = parseFactor();
        if (trim(funcstr) == "SQRT") then; x = sqrt(x);
            write(kunit,*) 'sqrt(x)=',sngl(x)
        else if (trim(funcstr) == "SIN") then; x = sin(x);
        else if (trim(funcstr) == "COS") then; x = cos(x);
        else if (trim(funcstr) == "TAN") then; x = tan(x);
        else if (trim(funcstr) == "EXP") then; x = exp(x);
        else if (trim(funcstr) == "LOG") then; x = log(x);
        else
            write(kunit,*) "parseFactor: Unknown function: ",trim(funcstr)
            ifehlxx = 1
        end if
    else
        write(kunit,*)   "parseFactor:  Unexpected: ", ch
        ifehlxx = 1
    end if

    if (eat('^')) then
        x = x ** parseFactor()    ! // exponentiation
    end if

end function parseFactor

!##############################################################################

subroutine NextChar()
    use xx

    implicit none

    if(endstr) return
    if(numch < len(str)) then
        numch = numch + 1
        ch = str(numch:numch)
        thisPosxx = numch
    else
        endstr = .true.
    end if

end subroutine NextChar

!##############################################################################

logical function eat(charToEat)
    use xx

    implicit none

    character(len=1),intent(in)   :: charToEat

    Eat = .false.
    if(endstr) return

    do while(ch == ' ')
        call NextChar()
        if(endstr) return
    end do
    if(ch == charToEat) then
        call NextChar()        ! if true, get the next character
        eat = .true.
    else
        eat = .false.
    end if

end function eat

!##############################################################################



