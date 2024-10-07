module CHF
    use UR_params,      only: rn

    ! FormatNumStr
    ! FLTU
    ! ucase
    ! lowercase
    ! FindlocT
    ! FLFU
    ! IndexArr
    ! IsNumberE
    ! StrReplace
    ! intStr
    ! realStr
    ! testSymbol
    ! isNaN

contains


!###############################################################################

    module function FormatNumStr(instr)

        ! reformats a number string, it removes superfluous '0' characters at the end
        ! or before the E+xx exponent

        !   Copyright (C) 2014-2023  Günter Kanisch

        use UR_variables,           only: sDecimalPoint

        implicit none

        character(len=*),intent(in)    :: instr

        integer                    :: i,i1,izbegin,izend,i2
        character(len=50)          :: str                       ! 10.1.2023
        character(:),allocatable   :: FormatNumStr
        !--------------------------------------------------------------------------

        str = adjustl(instr)
        FormatNumstr = str
        if(len_trim(str) == 0) return

        !  Adapt the number string to the actual decimal point:
        if(sDecimalPoint == ',') then
            i2 = index(str,'.')
            if(i2 > 0) then
                str(i2:i2) = sDecimalPoint
            end if
        elseif(sDecimalPoint == '.') then
            i2 = index(str,',')
            if(i2 > 0) THEN
                str(i2:i2) = sDecimalPoint
            end if
        end if
        FormatNumstr = str

        ! Reduce a series of '0' digits at the end of the number string:
        i1 = index(str,'E') - 1
        if(i1 == -1) i1 = len_trim(str)
        if(str(i1:i1) /= '0') return

        izend = i1
        izbegin = 0
        do i=i1-1,1,-1
            if(str(i:i) /= '0') exit
            if(izend  > 0 .and. str(i:i) == '0') izbegin = i
        end do
        if(izbegin > 0 .and. izend > izbegin ) then
            str = str(1:izbegin) // str(izend+1:)
        end if

        FormatNumStr = trim(str)

    end function FormatNumStr

!###############################################################################

    function fltu(local_encoded_str, error_code) result(utf8_str)

        ! transforms a string from local encoding to UTF8
        !   Copyright (C) 2014-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,       only: c_ptr, c_null_ptr, &
                                                     c_null_char, c_associated
        use gtk_sup,             only: c_f_string
        use g,                   only: g_locale_to_utf8

        implicit none

        character(len=*), intent(in)                  :: local_encoded_str
        integer, intent(out), optional                :: error_code

        character(len=len_trim(local_encoded_str)+32) :: str
        type(c_ptr)                                   :: resp
        character(:), allocatable                     :: utf8_str

        if (present(error_code)) error_code = 0

        if(Len_trim(local_encoded_str) == 0) return
        resp = g_locale_to_utf8(trim(local_encoded_str)//c_null_char, &
                                int(len_trim(local_encoded_str), 8),  &
                                c_null_ptr,                           &
                                c_null_ptr,                           &
                                c_null_ptr)

        if(c_associated(resp)) then
            call c_f_string(resp, str)
            utf8_str = trim(str)
        else
            utf8_str = trim(local_encoded_str)
            if (present(error_code)) error_code = 1 ! could not convert string
        endif

    end function fltu

!#############################################################################################

    function ucase(var)

        ! transforms a string to upper case characters
        !   Copyright (C) 2014-2023  Günter Kanisch

        implicit none

        character(len=*), intent(in)  :: var
        character(:), allocatable     :: ucase

        integer         :: k, i

        ucase = var
        do i=1,LEN_TRIM(ucase)
            k = ichar(ucase(i:i))
            if( (k >= 97 .AND. k <= 122) .OR. k == 252 .OR. k == 228 .OR.   &
                k == 246 ) ucase(i:i) = char (k-32)
        end do

    end function ucase

!#############################################################################################

    function lowercase(string)

        ! transforms a string to lower case characters
        !   Copyright (C) 2014-2024  Günter Kanisch

        implicit none
        character(len=*), intent(in) :: string
        character(len=len(string))   :: lowercase

        integer   , parameter :: ucmin = iachar('A'), ucmax = iachar('Z')
        integer   , parameter :: case_diff = iachar('a') - iachar('A')
        integer    :: i, ic

        lowercase = string
        do i = 1, len(string)
            ic = iachar(string(i:i))
            if (ic >= ucmin .and. ic <= ucmax) lowercase(i:i) = achar(ic+case_diff)
        end do
    end function lowercase

!#############################################################################################

    integer function FindlocT(carray,suchstr,imin)

        ! find that element of an array (carray) which is equal to the search-string,
        ! by comparing the upper-case versions.
        ! carray is a variable size string array based on type(charv).

        !   Copyright (C) 2014-2023  Günter Kanisch

        use UR_Gleich,     only: charv

        implicit none

        type(charv), allocatable        :: carray(:)
        character(len=*),intent(in)     :: suchstr    ! search string
        integer   ,intent(in),optional  :: imin       ! search from the imin-th array element onwards

        integer          :: i,k1

        FindLocT = 0
        if(present(imin)) then
            k1 = imin
        else
            k1 = 1
        end if
        do i=k1,ubound(carray,dim=1)
            if(trim(ucase(carray(i)%s)) == trim(ucase(suchstr))) then
                FindLocT = i
                return
            end if
        end do

    end function FindlocT

!#############################################################################################

    function flfu(utf8_str, error_code) result(local_encoded_str)

        ! transforms a string from UTF8 to local encoding
        !   Copyright (C) 2014-2024  Günter Kanisch

        use, intrinsic :: iso_c_binding,       only: c_ptr, c_null_ptr, &
                                                     c_null_char, c_associated
        use gtk_sup,             only: c_f_string
        use g,                   only: g_locale_from_utf8

        implicit none

        character(len=*),intent(in)           :: utf8_str
        integer, intent(out), optional        :: error_code

        character(len=len_trim(utf8_str)+32)  :: str
        type(c_ptr)                           :: resp
        character(:), allocatable             :: local_encoded_str

        if (present(error_code)) error_code = 0
        if(len_trim(utf8_str) == 0) return
        resp = g_locale_from_utf8(trim(utf8_str) // c_null_char,   &
                                  int(len_trim(utf8_str), 8),      &
                                  c_null_ptr,                      &
                                  c_null_ptr,                      &
                                  c_null_ptr)

        if(c_associated(resp)) then
            call c_f_string(resp, str)
            local_encoded_str = trim(str)
        else
            local_encoded_str = trim(utf8_str)
            if (present(error_code)) error_code = 1 ! could not convert string
        endif
    end function flfu

!#############################################################################################

    subroutine IndexArr(string,substring,n,ipos)

        ! determines the position values ipos() of the substring within string
        !   Copyright (C) 2014-2023  Günter Kanisch

        implicit none

        character(len=*),intent(in)   :: string, substring
        integer   ,intent(inout)      :: n , ipos(*)

        integer             :: i0,i1,stlen
        logical             :: isBlanc
        n = 0
        i0 = 1
        i1 = 1
        isBlanc = .false.
        if(len_trim(substring) == 0) isBlanc = .true.
        stlen = len_trim(string)

        do
            if(.not.isBlanc) i1 = index(trim(string(i0:)),trim(substring))
            if(isBlanc) i1 = index(trim(string(i0:)),substring)
            if(i1 == 0) exit
            n = n + 1
            if(n == 1) ipos(n) = i0 + (i1-1)
            if(n > 1) then
                ipos(n) = ipos(n-1) + (i1-0)
                if(isBlanc .and. ipos(n-1)+1 == ipos(n)) then
                    ipos(n-1) = ipos(n)
                    n = n - 1
                end if
            end if
            i0 = ipos(n) + 1
            if(len_trim(string(i0:)) == 0) exit
        end do

        if(n > 0) then
            if(stlen > ipos(n)) then
                n = n + 1
                ipos(n) = stlen + 1
            end if
        else
            n = n + 1
            ipos(n) = stlen + 1
        end if
        if(n > 0 .and. stlen > 1) then
            if(string(stlen:stlen) == ')') ipos(n) = stlen
        end if

    end subroutine IndexArr

!#############################################################################################

    logical function IsNumberE(string,ip)

        ! tests whether the given string represents a pure number
        !   Copyright (C) 2014-2023  Günter Kanisch

        implicit none

        character(len=*),intent(in)   :: string
        integer   ,intent(in)         :: ip      ! position of '-' or '+' of an exponent in the string

        IsNumberE = .false.
        if(ip <= 2) return        ! In this case string cannot represent a number in exponential representation

        if( scan(string(ip-1:ip-1),'eEqQ') > 0 .and.    &
            scan(string(ip-2:ip-2),'0123456789.') > 0 .and.   &
            scan(string(max(1,ip-2):max(1,ip-2)),'0123456789.') > 0 .and.   &
            scan(string(ip+1:ip+1),'0123456789') > 0 ) then
            IsNumberE = .true.    ! the string represents a number with Exponent
        end if

    end function IsNumberE

!#############################################################################################

    subroutine StrReplace(str, strold, strnew, all_occur, is_variable)

        ! replaces in a string (str) the substring strold by the string strnew, only once, or,
        ! if all_occur=T, at all occurrences
        ! If the substring is the name of a variable, the substring must be bracketed by special
        ! characters as given in " +-/*^()"; if the latter is not the case, it searches for the
        ! next occurrence in str

        !   Copyright (C) 2021-2023  Günter Kanisch

        implicit none

        character(:),intent(inout),allocatable   :: str
        character(len=*),intent(in)              :: strold      ! substring to be replaced
        character(len=*),intent(in)              :: strnew      ! substring replacing strold
        logical,intent(in)                       :: all_occur   ! replace at all occurrences or only the first
        logical,intent(in)                       :: is_variable

        integer       :: i1,ileng,k,i0,i3,kleng,imax
        logical       :: cond

        if(index(str,trim(strold)) == 0) return

        ileng = len_trim(strold)
        kleng = len_trim(strnew)
        i0 = 1
        do k=1,20
            i1 = index(str(i0:),trim(strold))
            if(i1 == 0) exit
            if(i1 == 1) then
                i3 = i0+i1-2

                if(i3 == 0) then
                    str = trim(strnew) // trim(str(ileng+1:))
                elseif(i3 > 0) then
                    if(scan(str(i3:i3),' +-/*^(),') > 0) then
                        str = str(1:i3) // trim(strnew) // trim(str(i3+1+ileng+0:))
                    end if
                end if
            elseif(i1 > 1) then
                i3 = i0+i1-2
                imax = i3+ileng+1
                cond = .false.
                if(scan(str(i3:i3),' +-/*^(),') > 0) then
                    if(imax > len_trim(str)) then
                        cond = .true.
                    end if
                    if(.not.cond) then
                        if(scan(str(imax:imax),' +-/*^(),') > 0) then
                            cond = .true.
                        end if
                    end if
                end if
                if(cond .or. .not.is_variable) then
                    if( i3+ileng+1 <= len_trim(str)) then
                        str = str(1:i3) // trim(strnew) // trim(str(i3+ileng+1:))
                    else
                        str = str(1:i3) // trim(strnew)
                    end if
                    if(.not.all_occur) exit
                end if
            end if
            i0 = i3 + 1 + max(1,ileng-1)
        end do

    end subroutine StrReplace

!############################################################################################

    function intStr(i)

        implicit none

        integer   , intent(in)    :: i
        character(:),allocatable  :: intStr
        character(len=8)    :: cc
        write(cc,'(i7)') i
        intstr = trim(adjustL(cc))

    end function intStr

!############################################################################################

    function realStr(x)

        implicit none

        real(rn), intent(in)    :: x
        character(:),allocatable  :: realStr
        character(len=25)    :: cc
        write(cc,*) sngl(x)
        realStr = trim(adjustL(cc))

    end function realStr

!#######################################################################

    logical function testSymbol(cfstring,symbol)

        !     This routine tests, whether the variable symbol is contained in the
        !     string cfstring, which represents a formula.
        !     This requires that the string symbol must have a blank or another of
        !     9 special characters as a left and/or right dircte neighbour within
        !     cfstring.

        !   Copyright (C) 2014-2023  Günter Kanisch

        implicit none

        CHARACTER(LEN=*),INTENT(IN)  :: cfstring    ! String with a formula containing symbols and operators
        CHARACTER(LEN=*),INTENT(IN)  :: symbol      ! Name of a symbol (a variable), which shall be tested for existing in cfstring

        integer   , parameter    :: nops = 11

        CHARACTER(LEN=1)      :: ops(nops)
        ! CHARACTER(LEN=80)     :: cstr
        character(len=:),allocatable :: cstr
        CHARACTER(LEN=60)     :: ch1
        integer               :: i1, i2, ioff
        LOGICAL               :: blinks, brechts
        !------------------------------------------------------------------------

        ops(1:9) = [ '+','-','/','*','(',')',',','^','=' ]               ! ,'q','q' /)
        ops(10) = CHAR(10)
        ops(11) = CHAR(13)

        allocate(character(len=800) :: cstr)

        cstr = trim(adjustl(ucase(cfstring)))
        ch1  = trim(adjustl(ucase(symbol)))
        testSymbol = .FALSE.

        ioff = 1
        i2 = LEN_TRIM(ch1)
50      continue

        blinks = .FALSE.
        brechts = .FALSE.

        i1 = INDEX(cstr(ioff:),TRIM(ch1))          ! corrected 2.6.2023
        IF(i1 > 0) THEN
            i1 = ioff -1 + i1
            IF(i1 == 1) THEN
                blinks = .TRUE.         ! variable is located at the begin of the string
            else
                if(Scan(cstr(i1-1:i1-1),' +-/*(),^=') > 0) blinks = .TRUE.
            end if
            IF(i1+i2-1 == LEN_TRIM(cstr)) THEN
                brechts = .TRUE.    ! variable is located at the end of the string
            else
                if(Scan(cstr(i1+i2:i1+i2),' +-/*(),^=') > 0) brechts = .TRUE.
            end if
        end if
        ! if(trim(ch1) == 'A') write(66,*) 'i1,ioff,i2=',i1,ioff,i2,' blinks=',blinks,' brechts=',brechts

        IF(blinks .AND. brechts) then
            testSymbol = .TRUE.
            ! if(trim(ch1) == 'A') write(66,*) 'i1,ioff,i2=',i1,ioff,i2,' blinks=',blinks,' brechts=',brechts,' :',trim(cfstring)
            return
        else
            if(i1 > 0) then
                ! ioff = i1 + 1
                ioff = i1 + i2
                ! if(ioff + i2 <= len_trim(cstr)) goto 50
                if(ioff  <= len_trim(cstr)) goto 50
            end if
        end if

    end function testSymbol

!#######################################################################

    logical function isNaN(xval)

        implicit none

        real(rn), intent(in)   :: xval

        character(len=25)    :: cc
        isNaN = .false.
        write(cc,'(es12.5)') xval
        if(index(cc,'NaN') > 0) isNaN = .true.
        if(index(cc,'Inf') > 0) isNaN = .true.

    end function isNaN

!#######################################################################

end module CHF
