!--------------------------------------------------------------------------------------------------!
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
!--------------------------------------------------------------------------------------------------!
module UR_tests

    ! A collection of tests for UncertRadio

    implicit none
    !----------------------------------------------------------------------------------------------!
    private
    !
    public :: run_tests
    !----------------------------------------------------------------------------------------------!

contains
    subroutine run_tests()
        implicit none
        !------------------------------------------------------------------------------------------!
        write(*,'(2X,A)') "Running UncertRadio tests"

        ! call test_sym_eigensolve()

        call test_mtxhst()

        call test_write_text_file()

        call test_str_replace()

        call test_color_themes()

        call test_translations()

        call test_format_nu_str()

        call test_ucase()

        call test_lowercase()

        call Batest_no_gui()

        write(*,'(2X,A)') "All tests done"

    end subroutine

    subroutine test_sym_eigensolve()
        !------------------------------------------------------------------------------------------!
        use UR_types, only: rn
        use UR_params, only: EPS1MIN
        use Num1, only: sym_eigensolve, kaiser
        implicit none
        !------------------------------------------------------------------------------------------!
        integer, parameter :: n = 3
        real(rn) :: a(n,n), eigenv(n), a2(n,n), eigenv2(n), sume, trace
        integer  :: ier, i, errors, lda
        !------------------------------------------------------------------------------------------!

        errors = 0
        ! Initialize matrix a
        a = 0.0_rn
        do i = 1, n
            a(i,i) = 1.0_rn
        end do
        a(1,2) = 0.5_rn
        a(2,1) = 0.5_rn
        a(1,3) = 0.2_rn
        a(3,1) = 0.2_rn
        a(2,3) = 0.1_rn
        a(3,2) = 0.1_rn

        a2 = a

        call kaiser(a2, n, n, eigenv2, trace, sume, ier)

        print *, a
        print *, ''
        lda = n
        ! Call test_sym_eigensolve subroutine
        call sym_eigensolve(n, a, lda, eigenv, ier)

        ! Check if ier is 0 (no error)
        if (ier /= 0) then
            write(*,'(4X,A)') "Error: ier /= 0"
            errors = errors + 1
        end if
        print '(3(ES23.15))', eigenv(:)
        print '(3(ES23.15))', a(1,:)
        print '(3(ES23.15))', a(2,:)
        print '(3(ES23.15))', a(3,:)
        print *, ''
        print '(3(ES23.15))', eigenv2(:)
        print '(3(ES23.15))', a2(1,:)
        print '(3(ES23.15))', a2(2,:)
        print '(3(ES23.15))', a2(3,:)

        ! Check if eigenv is correct
        if (abs(eigenv(1) - 1.5784259628995521_rn) > EPS1MIN .or. &
            abs(eigenv(2) - 0.93229899132550031_rn) > EPS1MIN .or. &
            abs(eigenv(3) - 0.48927504577494785_rn) > EPS1MIN) then
            write(*,'(4X,A)') "Error: eigenvalues are not correct"
            errors = errors + 1
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "Kaiser: no errors"
        else
            write(*, '(4X, A,I0,A)') "Kaiser: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_sym_eigensolve


    subroutine test_mtxhst()
        !------------------------------------------------------------------------------------------!
        use UR_types, only: rn
        use UR_params, only: PI, EPS1MIN
        use brandt, only: mtxhst
        implicit none
        !------------------------------------------------------------------------------------------!

        integer, parameter :: n = 10
        integer  :: lp, l, i
        real(rn) :: v(n), up, b, c(n)

        real(rn), parameter :: results(n) = &
            [0.31830988618379069_rn, &
             0.63661977236758138_rn, &
             0.95492965855137202_rn, &
             1.2732395447351628_rn, &
             374.96354436109192_rn, &
             1920.5322358084820_rn, &
             2613.6864038721087_rn, &
             3413.4307039630339_rn, &
             4319.7651360812579_rn, &
             5332.6897002267806_rn]
        integer                   :: errors
        !------------------------------------------------------------------------------------------!

        errors = 0
        ! Initialize variables
        lp = 5
        l = 6
        up = 2.23_rn
        b = 0.58288_rn

        ! Initialize vectors v and c
        do i = 1, n
            v(i) = real(i, rn)**2 / PI
            c(i) = real(i, rn) / PI
        end do

        call mtxhst(v, up, b, c, n, lp, l)

        do i=1, 10
            if (abs(c(i) + EPS1MIN - results(i)) > EPS1MIN) then
                errors = errors + 1
            end if
        end do

        if (errors == 0) then
            write(*,'(4X,A)') "mtxhst: no errors"
        else
            write(*, '(4X, A,I0,A)') "mtxhst: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_mtxhst

    subroutine test_str_replace()
        use chf, only: StrReplace
        implicit none

        character(:), allocatable :: str
        character(len=16)         :: strold, strnew
        logical                   :: all_occur, is_variable
        integer                   :: errors

        errors = 0

        ! Test case 1: Replace a simple string but only the first one
        str = 'Hello, World! World!'
        strold = 'World'
        strnew = 'Universe'
        all_occur = .false.
        is_variable = .false.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, Universe! World!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 1: should be: 'Hello, Universe!', got: '" // str // "'"
        end if

        ! Test case 2: Replace a simple string but now all
        str = 'Hello, World! World!'
        strold = 'World'
        strnew = 'Universe'
        all_occur = .true.
        is_variable = .false.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, Universe! Universe!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 2: should be: 'Hello, Universe! Universe!', got: '" // str // "'"
        end if

        ! Test case 3: Replace a string with is_variable true but no symbol
        str = 'Hello, World!'
        strold = 'World'
        strnew = 'Universe'
        all_occur = .false.
        is_variable = .true.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, World!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 3: should be: 'Hello, World!', got: '" // str // "'"
        end if

        ! Test case 4: Replace a string with is_variable true and realy could be a symbol
        str = 'Hello, + World'
        strold = 'World'
        strnew = 'Universe'
        all_occur = .false.
        is_variable = .true.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, + Universe') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 4: should be: 'Hello, + Universe', got: '" // str // "'"
        end if

        ! Test case 5: Replace a mathematical operator
        str = 'Hello, + World'
        strold = '+'
        strnew = 'new'
        all_occur = .false.
        is_variable = .false.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, new World') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 5: should be: 'Hello, new World', got: '" // str // "'"
        end if

        ! Test case 6: Replace a special character everywhere
        str = '/Hello/Path/to/a/new/World/'
        strold = '/'
        strnew = '\'
        all_occur = .true.
        is_variable = .false.
        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= '\Hello\Path\to\a\new\World\') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 6: should be: '\Hello\Path\to\a\new\World\', got: '" // str // "'"
        end if

        ! Test case 7: Replace a special character with the same
        str = '/Hello/Path/to/a/new/World/'
        strold = '/'
        strnew = '/'
        all_occur = .true.
        is_variable = .false.
        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= '/Hello/Path/to/a/new/World/') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 7: should be: '/Hello/Path/to/a/new/World/', got: '" // str // "'"
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "StrReplace: no errors"
        else
            write(*, '(4X, A,I0,A)') "StrReplace: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_str_replace

    !---------------------------------------------------------------------------------------------!


    subroutine test_write_text_file()

        use file_io, only: write_text_file
        use chf,     only: flfu
        implicit none

        character(:), allocatable :: filename, out_text1, out_text2
        character(32)             :: tmp_string
        integer                   :: errors, nio, iostat
        !-----------------------------------------------------------------------------------------!
        errors = 0
        filename = 'Testfileä.txt'
        out_text1 = 'first Täxt'
        out_text2 = 'second T~xt'

        ! write the first text to a 'new' file,
        call write_text_file(out_text1, filename, status='new', utf8_filename=.true.)
        ! now write the second text to the same file and close it afterwards
        call write_text_file(out_text2, filename, utf8_filename=.true., status='close')

        ! read the file and check the content
        open(file=flfu(filename), newunit=nio, iostat=iostat, status='old', action='read')

        if (iostat /= 0) then
            errors = errors + 1
        else
            read(nio,'(A)') tmp_string
            ! test if input == output
            if (tmp_string /= out_text1) then
                errors = errors + 1
                write(*,'(4X,A)') "Error: input /= output" // trim(tmp_string) // ", " // out_text1
            end if

            ! read the next line and check the content
            read(nio,'(A)') tmp_string
            ! test if input == output
            if (tmp_string /= out_text2) then
                errors = errors + 1
                write(*,'(4X,A)') "Error: input /= output" // trim(tmp_string) // ", " // out_text2
            end if
            close(nio)

            ! again write to the file and force to create a new one
            call write_text_file(out_text1 // out_text2, filename, status='new', utf8_filename=.true.)
            call write_text_file(out_text1 // out_text2, filename, status='close')

            ! read the file and check the content
            open(file=flfu(filename), newunit=nio, iostat=iostat, status='old', action='read')

            if (iostat /= 0 ) then
                errors = errors + 1
            else
                read(nio,'(A)') tmp_string
                close(nio)

                if (tmp_string /= out_text1 // out_text2) then
                    errors = errors + 1
                    write(*,'(4X,A)') "Error: input /= output: " // trim(tmp_string) // ", " // &
                        out_text1 // out_text2
                end if

            end if

            ! now remove the testfile:
            open(file=flfu(filename), newunit=nio, status='old')
            close(nio, status="delete")
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "write_text_file: no errors"
        else
            write(*, '(4X, A,I0,A)') "write_text_file: Warning, found ", errors, " error(s)"
        end if
    end subroutine test_write_text_file

    !---------------------------------------------------------------------------------------------!

    subroutine test_color_themes()
        use color_theme
        implicit none

        ! Test variables
        character(len=7)  :: color_result
        character(len=16) :: theme_name_result
        integer           :: errors

        errors = 0

        ! Test 1: Default color theme
        call set_color_theme("default")
        color_result = get_color_string("entry_bg")
        if (color_result /= "#FFFFEC") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 1 Failed: Expected #FFFFEC, got ", color_result
        end if

        ! Test 2: Contrast color theme
        call set_color_theme("contrast")
        color_result = get_color_string("entry_bg")
        if (color_result /= "#000000") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 2 Failed: Expected #000000, got ", color_result
        end if

        ! Test 3: Get theme name
        theme_name_result = get_theme_name()
        if (theme_name_result /= "contrast") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 3 Failed: Expected contrast, got ", theme_name_result
        end if

        ! Test 4: Invalid theme name
        call set_color_theme("invalid_theme")  ! Should print an error message
        theme_name_result = get_theme_name()
        if (theme_name_result /= "contrast") then
            errors = errors + 1
            write(*,'(4X,A)')"Test 4 Failed: Expected contrast, got ", theme_name_result
        end if

        ! Test 5: Get unknown color key
        color_result = get_color_string("unknown_key")
        if (color_result /= "       ") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 5 Failed: Expected blank string, got ", color_result
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "color_modes: no errors"
        else
            write(*, '(4X, A,I0,A)') "color_modes: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_color_themes
    !---------------------------------------------------------------------------------------------!

    subroutine test_translations()
        use translation_module, only : set_language, get_language, T => get_translation

        implicit none
        ! Test variables
        character(:), allocatable :: key, translation
        character(len=2)          :: language
        integer :: errors = 0

        key = 'Equations'

        ! Test 1: Set language with a valid file
        language = 'de'
        call set_language(language, 'translations/de/de.po')  ! Adjust the path as necessary

        if (get_language() /= language) then
            errors= errors + 1
            write(*,'(4X,A)') "Test 1 - Set Language expected 'de' but got ", trim(get_language())
        end if

        ! Test 2: Get translation for a known key
        key = "Equations"
        translation = T(key)

        if (trim(translation) /= "Gleichungen") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 2 - expected 'Gleichungen' but got '" // trim(translation) // "'"
        end if

        ! Test 3: Get translation for an unknown key
        key = "someUnknownKey"
        translation = T(key)
        if (trim(translation) /= key)  then ! Should return the key itself
            errors = errors + 1
            write(*,'(4X,A)') "Test 3 - expected 'Equation' but got '" // trim(translation) // "'"
        end if

        ! Test 4: Set language with a non-existent file
        language = 'fr'
        call set_language(language, 'translations/fr/non_existent.po')  ! Adjust the path as necessary
        ! Expect an error message to be printed
        ! Check if selected_language remains unchanged

        if (get_language() == language) then
            errors = errors + 1
            write(*,'(4X,A)') "Test 4 - Set Language should not be " // language
        end if

        ! Test 5: Check if the optional value produces upper case first letter
        !
        key = "the first"
        translation = T(key, .true.)
        if (trim(translation) /= "Die erste")  then ! The tranlation has lower first character
            errors = errors + 1
            write(*,'(4X,A)') "Test 5 - expected 'Die erste' but got '" // trim(translation) // "'"
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "translations: no errors"
        else
            write(*, '(4X, A,I0,A)') "translations: Warning, found ", errors, " error(s)"
        end if


    end subroutine test_translations
    !---------------------------------------------------------------------------------------------!

    subroutine test_format_nu_str()

        use ur_general_globals, only: sDecimalPoint
        use CHF, only: FormatNumStr

        character(len=64) :: test_input
        character(len=64) :: expected_output
        integer :: errors = 0


        sDecimalPoint = '.'
        ! Test case 1: Basic number with trailing zeros
        test_input = "123.45000"
        expected_output = "123.450"

        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            print *, "Test 1 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        ! Test case 2: Number with exponent and trailing zeros
        test_input = "1.23000E+10"
        expected_output = "1.230E+10"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)') "Test 2 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        ! Test case 3: Number with no trailing zeros
        test_input = "123.45"
        expected_output = "123.45"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)')  "Test 3 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        ! Test case 4: Empty string
        test_input = ""
        expected_output = ""
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)') "Test 4 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if
        ! Test case 5: Number with comma as decimal point
        sDecimalPoint = ','
        test_input = "123,45000"
        expected_output = "123,450"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            print *, "Test 5 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        sDecimalPoint = '.'
        ! Test case 6: Number with exponent and no trailing zeros
        test_input = "1.23E+10"
        expected_output = "1.23E+10"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)') "Test 6 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        ! Test case 7: Number with negative exponent and trailing zeros
        test_input = "1.23000E-10"
        expected_output = "1.230E-10"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)')  "Test 6 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        if (errors == 0) then
            write(*, '(4X,A)') "FormatNumStr: no errors"
        else
            write(*, '(4X, A,I0,A)') "FormatNumStr: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_format_nu_str


    subroutine test_ucase()

        use chf, only: ucase
        implicit none

        character(:), allocatable :: input, output
        integer                   :: errors
        !-----------------------------------------------------------------------------------------!
        errors = 0

        ! Test case 1: single character
        input = 'a'
        output = ucase(input)
        if (output /= 'A') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 2: multiple characters
        input = 'Hello, World!'
        output = ucase(input)
        if (output /= 'HELLO, WORLD!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 3: include special characters
        input = 'äöüÄÖÜ and some more [´'
        output = ucase(input)
        if (output /= 'äöüÄÖÜ AND SOME MORE [´') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 4: empty string
        input = ''
        output = ucase(input)
        if (output /= '') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "ucase: no errors"
        else
            write(*, '(4X, A,I0,A)') "ucase: Warning, found ", errors, " error(s)"
        end if
    end subroutine test_ucase


    subroutine test_lowercase()

        use chf, only: lowercase
        implicit none

        character(:), allocatable :: input, output
        integer                   :: errors
        !-----------------------------------------------------------------------------------------!
        errors = 0

        ! Test case 1: single uppercase letter
        input = 'A'
        output = lowercase(input)
        if (output /= 'a') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 2: single lowercase letter
        input = 'a'
        output = lowercase(input)
        if (output /= 'a') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 3: mixed case string
        input = 'Hello, World!'
        output = lowercase(input)
        if (output /= 'hello, world!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 4: all uppercase string
        input = 'HELLO, WORLD!'
        output = lowercase(input)
        if (output /= 'hello, world!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 5: all lowercase string
        input = 'hello, world!'
        output = lowercase(input)
        if (output /= 'hello, world!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 6: string with special characters
        input = 'Hello, World! 123Ä[]'
        output = lowercase(input)
        if (output /= 'hello, world! 123Ä[]') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "lowercase: no errors"
        else
            write(*, '(4X, A,I0,A)') "lowercase: Warning, found ", errors, " error(s)"
        end if
    end subroutine test_lowercase

end module UR_tests
