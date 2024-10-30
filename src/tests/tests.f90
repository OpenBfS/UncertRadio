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
module UR_tests

    ! A collection of tests for UncertRadio

    use UR_types
    implicit none
    !---------------------------------------------------------------------------------------------!
    private
    !
    public :: run_tests
    !---------------------------------------------------------------------------------------------!

contains
    subroutine run_tests()
        implicit none
        !-----------------------------------------------------------------------------------------!
        write(*,'(2X,A)') "Running UncertRadio tests"

        call test_write_text_file()

        call test_color_themes()

        call test_translations()

        call test_FormatNumStr()

        call test_ucase()

        call test_lowercase()

        write(*,'(2X,A)') "All tests done"
        stop
    end subroutine

    !---------------------------------------------------------------------------------------------!

    subroutine test_write_text_file()

        use file_io, only: write_text_file
        use chf,     only: flfu
        implicit none

        character(:), allocatable :: filename, out_text1, out_text2
        character(32)             :: tmp_string
        integer                   :: errors
        !-----------------------------------------------------------------------------------------!
        errors = 0
        filename = 'Testfileä.txt'
        out_text1 = 'first Täxt'
        out_text2 = 'second T~xt'

        ! write the first text to a 'new' file,
        call write_text_file(out_text1, filename, status='new', utf8_filename=.true.)
        ! now write the second text to the same file
        call write_text_file(out_text2, filename, utf8_filename=.true.)

        ! read the file and check the content
        open(101, file=flfu(filename), status='old', action='read')
        read(101,'(A)') tmp_string
        ! test if input == output
        if (tmp_string /= out_text1) then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output" // trim(tmp_string) // ", " // out_text1
        end if

        ! read the next line and check the content
        read(101,'(A)') tmp_string
        ! test if input == output
        if (tmp_string /= out_text2) then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output" // trim(tmp_string) // ", " // out_text2
        end if
        close(101)

        ! again write to the file and force to create a new one
        call write_text_file(out_text1 // out_text2, filename, status='new', utf8_filename=.true.)
        ! read the file and check the content
        open(101, file=flfu(filename), status='old', action='read')
        read(101,'(A)') tmp_string
        close(101)

        if (tmp_string /= out_text1 // out_text2) then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(tmp_string) // ", " // &
                              out_text1 // out_text2
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

    subroutine test_FormatNumStr()

        use ur_variables, only: sDecimalPoint
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

    end subroutine test_FormatNumStr


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
