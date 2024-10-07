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
        key = "Equation"
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

        if (errors == 0) then
            write(*,'(4X,A)') "translations: no errors"
        else
            write(*, '(4X, A,I0,A)') "translations: Warning, found ", errors, " error(s)"
        end if


    end subroutine test_translations
    !---------------------------------------------------------------------------------------------!
end module UR_tests
