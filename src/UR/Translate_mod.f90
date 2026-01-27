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

module translation_module
    implicit none
    private

    ! Variable if output is desired
    logical :: debug_output = .true.

    ! Variable to store the root of the translation directory
    character(:), allocatable :: lang_root

    ! Define a type for translations
    type :: translation_entry
        character(:), allocatable :: key
        character(:), allocatable :: translation
    end type translation_entry

    ! Array of translation entries
    type(translation_entry), dimension(:), allocatable :: translations

    ! Variable to store the selected language
    character(len=2) :: selected_language = 'XX'
    public :: set_language, get_language, get_translation

contains
    !---------------------------------------------------------------------------------------------!
    ! Set the selected language
    subroutine set_language(lang, lang_path)
        character(len=*), intent(in) :: lang
        character(len=*), intent(in), optional :: lang_path

        character(len=:), allocatable :: tmp_filename
        logical :: file_exists

        if (present(lang_path)) then
            lang_root = lang_path
        else if (.not. allocated(lang_root)) then
            lang_root = ''
        end if

        tmp_filename = lang_root // lang // '/' // lang //'.po'

        ! Check if the file exists
        inquire(file=tmp_filename, exist=file_exists)

        if (.not. file_exists) then
            if (debug_output) write(0,*) 'Error: The file ', trim(tmp_filename), &
                                         ' does not exist.'
            return
        end if

        if (allocated(translations)) deallocate(translations)
        allocate(translations(0))
        selected_language = lang

        call read_translations_from_po_file(tmp_filename)
    end subroutine set_language

    !---------------------------------------------------------------------------------------------!
    ! Read translations from a .po file
    subroutine read_translations_from_po_file(filename)
        character(len=*), intent(in) :: filename
        integer :: ios
        character(len=256) :: line
        character(len=256) :: key, translation
        integer :: unit, num_translations
        logical :: in_msgid

        ! Open the file for reading
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            if (debug_output) print *, "Error opening file: ", filename
            return
        end if

        in_msgid = .false.

        ! Read each line from the file
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit  ! Exit on end of file or error

            ! Check for msgid and msgstr
            if (trim(line) == "") cycle  ! Skip empty lines

            if (index(line, "msgid") == 1) then
                ! Extract the key
                read(line(7:), '(A)') key  ! Skip "msgid "
                key = adjustl(key)
                in_msgid = .true.
            else if (index(line, "msgstr") == 1 .and. in_msgid) then
                ! Extract the translation
                read(line(7:), '(A)') translation  ! Skip "msgstr "
                translation = adjustl(translation)

                ! Resize the translations array
                num_translations = size(translations) + 1
                call resize_translations(num_translations)

                ! Store the translations
                translations(num_translations)%key = key(2:len_trim(key)-1)
                translations(num_translations)%translation = trim(translation(2:len_trim(translation)-1))

                in_msgid = .false.  ! Reset for the next entry
            end if
        end do

        ! Close the file
        close(unit)
    end subroutine read_translations_from_po_file

    !---------------------------------------------------------------------------------------------!
    ! Resize the translations array
    subroutine resize_translations(new_size)
        integer, intent(in) :: new_size
        type(translation_entry), dimension(:), allocatable :: new_translations

        if (new_size == 0) then
            if (allocated(translations)) then
                deallocate(translations)
            end if
            return
        end if

        allocate(new_translations(new_size))
        if (allocated(translations)) then
            new_translations(1:size(translations)) = translations
            deallocate(translations)
        end if
        translations = new_translations
    end subroutine resize_translations

    !---------------------------------------------------------------------------------------------!
    ! Get the translation for a given key
    function get_translation(key, upper_first_char) result(translation)
        use chf, only: ucase
        use iso_c_binding, only: c_new_line

        character(len=*), intent(in)  :: key
        logical, intent(in), optional :: upper_first_char

        logical :: upper_first_char_tmp

        character(:), allocatable :: translation
        integer :: i, num_translations, pos

        ! check optional input parameters and set defaults
        ! should the first char be of upper kind? regardless the translation
        if (present(upper_first_char)) then
            upper_first_char_tmp = upper_first_char
        else
            upper_first_char_tmp = .false.
        end if

        translation = key
        if (.not. allocated(translations)) return

        num_translations = size(translations)
        if (selected_language == 'XX' .or. num_translations == 0) then
            if (debug_output) write(0,*) 'Error: languages are not initiated'
            return
        end if

        ! Search for the key in the translations
        do i = 1, num_translations
            if (key == translations(i)%key) then
                if (len(translations(i)%translation) > 0) then
                    translation = translations(i)%translation

                    ! if the key is found check if the first char should be upper case
                    if (upper_first_char_tmp) then
                        translation(1:1) = ucase(translation(1:1))
                    end if

                    ! now process possible line breaks
                    do
                        pos = index(translation, "\n")
                        if (pos == 0) exit
                        translation = trim(translations(i)%translation(1:pos-1)) // c_new_line // &
                                      trim(adjustl(translations(i)%translation(pos+2:)))

                    end do
                end if
                return
            end if
        end do
        if (i > num_translations .and. debug_output) then
            write(0,*) 'Warning: key not found: ' // key
        end if

    end function get_translation

    function get_language() result(language)
        implicit none
        character(len=2) :: language

        language = selected_language

    end function

end module translation_module