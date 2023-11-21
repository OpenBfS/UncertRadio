!------------------------------------------------------------------------------!
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
!    along with UncertRadio.  If not, see <http://www.gnu.org/licenses/>.
!
!    Diese Datei ist Teil von UncertRadio.
!
!    UncertRadio ist Freie Software: Sie können es unter den Bedingungen
!    der GNU General Public License, wie von der Free Software Foundation,
!    Version 3 der Lizenz oder (nach Ihrer Wahl) jeder späteren
!    veröffentlichten Version, weiterverbreiten und/oder modifizieren.
!
!    UncertRadio wird in der Hoffnung, dass es nützlich sein wird, aber
!    OHNE JEDE GEWÄHRLEISTUNG, bereitgestellt; sogar ohne die implizite
!    Gewährleistung der MARKTFÄHIGKEIT oder EIGNUNG FÜR EINEN BESTIMMTEN ZWECK.
!    Siehe die GNU General Public License für weitere Details.
!
!    Sie sollten eine Kopie der GNU General Public License zusammen mit diesem
!    Programm erhalten haben. Wenn nicht, siehe <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------!
!
! This module has been developed to handle input files in a more
! flexible and generic way
!
!
module parser_mod

    use UR_params,      only: rn
    !--------------------------------------------------------------------------!
    private
    !--------------------------------------------------------------------------!
    public :: parse
    !--------------------------------------------------------------------------!
    interface parse
        module procedure parse_int_i1,                                         &
            parse_int_i2,                                                      &
            parse_real_r1,                                                     &
            parse_real_r2,                                                     &
            parse_str,                                                         &
            parse_log
    end interface
    !--------------------------------------------------------------------------!

contains

    subroutine parse_int_i1(keyword,var,data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        integer,intent(inout)       :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !----------------------------------------------------------------------!
        call get_value(keyword,data_file,val_st,iost)
        if ( iost == 0 ) then
            read(val_st,fmt=*,IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//TRIM(data_file)//') with keyword: ', keyword
            stop
        end if
    end subroutine parse_int_i1

    subroutine parse_int_i2(keyword,var,data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        integer(8),intent(inout)    :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !----------------------------------------------------------------------!
        call get_value(keyword,data_file,val_st,iost)
        if ( iost == 0 ) then
            read(val_st,fmt=*,IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//TRIM(data_file)//') with keyword: ', keyword
            stop
        end if
    end subroutine parse_int_i2


    subroutine parse_real_r2(keyword, var, data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        REAL,intent(inout)          :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !----------------------------------------------------------------------!
        call get_value(keyword, data_file, val_st, iost)
        if ( iost == 0 ) then
            read(val_st, fmt=*, IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//TRIM(data_file)//') with keyword: ', keyword
            stop
        end if

    end subroutine parse_real_r2


    subroutine parse_real_r1(keyword, var, data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        REAL(kind=rn),intent(inout) :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !----------------------------------------------------------------------!
        call get_value(keyword, data_file, val_st, iost)
        if ( iost == 0 ) then
            read(val_st, fmt=*, IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//TRIM(data_file)//') with keyword: ', keyword
            stop
        end if

    end subroutine parse_real_r1


    subroutine parse_str(keyword,var,data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in)    :: keyword
        character(len=*),intent(in)    :: data_file
        character(len=*),intent(inout) :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !----------------------------------------------------------------------!
        call get_value(keyword,data_file,val_st,iost)

        if ( iost == 0 ) then
            read(val_st,fmt='(A)',IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//TRIM(data_file)//') with keyword: ', keyword
            stop
        end if


    end subroutine parse_str

    subroutine parse_log(keyword,var,data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in)    :: keyword
        character(len=*),intent(in)    :: data_file
        LOGICAL,intent(inout)          :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !----------------------------------------------------------------------!
        call get_value(keyword,data_file,val_st,iost)
        if ( iost == 0 ) then
            read(val_st,fmt='(L1)',IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//TRIM(data_file)//') with keyword: ', keyword
            stop
        end if


    end subroutine parse_log

    ! in thus routine the actual work (find the keyword, return the value etc.)
    ! is done
    !
    subroutine get_value(keyword, data_file, val_st, iost, comment_str)

        implicit none
        ! This routine searches each line for the keyword and returns the value
        ! as a string
        !----------------------------------------------------------------------!
        character(len=*),intent(in)      :: keyword
        character(len=*),intent(in)      :: data_file
        character(len=256),intent(out)   :: val_st
        integer,intent(out)              :: iost
        character(len=256)               :: line
        character(len=1),intent(in), optional    :: comment_str

        character(len=1)                 :: comment_str_

        integer                          :: keyword_end, value_end
        ! integer                          :: br_l, br_r
        integer                          :: iostat, nio
        integer                          :: key_found = 0

        !----------------------------------------------------------------------!
        iost = 0
        if (.not. present(comment_str) ) then
            comment_str_ = '!'
        else
            comment_str_ = comment_str
        end if

        open(newunit=nio, &
            file=data_file, &
            action="read", &
            status="unknown", &
            form="formatted")

        line = ''
        do WHILE (key_found == 0)
            ! read the next line of the input file
            !
            read(unit=nio,fmt='(A)',iostat=iostat) line

            if (iostat < 0) then
                ! end of file and no keyword is found
                ! print *, 'ERROR: Keyword not found in input file'
                iost = -1
                EXIT
            end if
            ! search for the '=' symbol in this line
            !
            keyword_end = index(line, '=')

            ! test if the keyword belong to this entry
            !
            if (line(1:keyword_end-1) == keyword) then
                key_found = 1

                !now, find the brackets {}
                ! do br_l = i+1, j-1
                !     if (line(br_l:br_l) == '{' ) then
                !         EXIT
                !     end if
                ! end do

                ! do br_r = br_l+1, j
                !     if (line(br_r:br_r) == '}' ) then
                !         EXIT
                !     end if
                ! end do

                !check if brackets have been found.
                ! if (br_l == j .or. br_r == j+1 ) then
                !     iost = 1
                !     !print *, 'ERROR: no value found (brackets not correct?)'
                !     EXIT
                ! end if

                ! check for comments
                value_end = index(line, comment_str_)

                if (value_end == 0) then
                    value_end = len_trim(line)
                else if (value_end <= keyword_end) then
                    value_end = keyword_end +1
                    !print *, 'ERROR: line starts with a comment symbol'
                end if

                ! finally set the string to the given var
                !
                ! val_st = line(br_l+1:br_r-1)
                val_st = line(keyword_end+1:value_end-1)

            end if

        end do
        close(unit=nio)

    end subroutine get_value

end module parser_mod
