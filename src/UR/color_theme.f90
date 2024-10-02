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
module color_theme
    implicit none
    private

    type :: color_settings_type
        character(7) :: entry_bg
        character(7) :: entry_fg
        character(7) :: entry_mark_bg
        character(7) :: entry_mark_fg
        character(7) :: label_bg
        character(7) :: label_fg
        character(7) :: frame_bg
        character(7) :: frame_fg
        character(7) :: green_bg
        character(7) :: orange_bg
        character(7) :: table_bg
    end type color_settings_type

    ! colors for the "normal/default" mode
    type(color_settings_type), parameter :: DEFAULT_THEME_COLORS = color_settings_type( &
        "#FFFFEC", & ! entry_bg
        "#000000", & ! entry_fg
        "#FFFFFF", & ! entry_mark_bg
        "#000000", & ! entry_mark_fg
        "#FFFFFF", & ! label_bg
        "#000000", & ! label_fg
        "#FFFFFF", & ! frame_bg
        "#000000", & ! frame_fg
        "#00FF48", & ! green_bg
        "#F57900", & ! orange_bg
        "#FFFFFF", & ! table_bg
        "#E2FFFA")  ! GtkNotebook

    ! colors for the "contrast" mode
    type(color_settings_type), parameter :: CONTRAST_THEME_COLORS = color_settings_type( &
        "#000000", & ! entry_bg
        "#FFFFFF", & ! entry_fg
        "#000000", & ! entry_mark_bg
        "#FFFFFF", & ! entry_mark_fg
        "#000000", & ! label_bg
        "#FFFFFF", & ! label_fg
        "#1D1D1D", & ! frame_bg
        "#A1E1FF", & ! frame_fg
        "#0000d5", & ! green_bg
        "#B54900", & ! orange_bg
        "#252525" )  ! table_bg



    ! Declare a variable to hold the current color settings
    type(color_settings_type) :: current_colors = DEFAULT_THEME_COLORS
    character(16) :: current_theme_name = 'default'

    public :: set_color_theme, get_color_string, get_theme_name
    contains

    subroutine set_color_theme(theme_name)
        implicit none
        character(len=*), intent(in) :: theme_name

        select case (trim(theme_name))
        case ("default")
            current_colors = DEFAULT_THEME_COLORS
        case ("contrast")
            current_colors = CONTRAST_THEME_COLORS
        case default
            print *, "Unknown color theme. Please use 'default' or 'contrast'."
            return
        end select
        current_theme_name = trim(theme_name)

        print *, "Color mode set to: ", theme_name
    end subroutine set_color_theme

    function get_color_string(key) result(color_string)
        implicit none
        character(len=*), intent(in) :: key
        character(len=7) :: color_string

        select case (trim(key))
        case ("entry_bg")
            color_string = current_colors%entry_bg
        case ("entry_fg")
            color_string = current_colors%entry_fg
        case ("entry_mark_bg")
            color_string = current_colors%entry_mark_bg
        case ("entry_mark_fg")
            color_string = current_colors%entry_mark_fg
        case ("label_bg")
            color_string = current_colors%label_bg
        case ("label_fg")
            color_string = current_colors%label_fg
        case ("frame_bg")
            color_string = current_colors%frame_bg
        case ("frame_fg")
            color_string = current_colors%frame_fg
        case ("green_bg")
            color_string = current_colors%green_bg
        case ("orange_bg")
            color_string = current_colors%orange_bg
        case ("table_bg")
            color_string = current_colors%table_bg
        case default
            print *, "Unknown key. Please use a valid color key."
            color_string = "       "  ! Return a blank string for unknown keys
        end select
    end function get_color_string

    elemental function get_theme_name() result(theme_name)
        implicit none

        character(len(current_theme_name)) :: theme_name

        theme_name = current_theme_name


    end function get_theme_name

end module color_theme
