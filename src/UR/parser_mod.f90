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
!------------------------------------------------------------------------------!
!
! This module has been developed to handle input files in a more
! flexible and generic way
!
!
module parser_mod

    use, intrinsic :: iso_fortran_env, only: dp => real64, &
                                             sp => real32, &
                                             di => int64,  &
                                             si => int32
    !--------------------------------------------------------------------------!
    private
    !--------------------------------------------------------------------------!
    public :: parse
    !--------------------------------------------------------------------------!
    interface parse
        module procedure parse_int_si,                                         &
                         parse_int_di,                                         &
                         parse_real_sp,                                        &
                         parse_real_dp,                                        &
                         parse_str,                                            &
                         parse_log
    end interface
    !--------------------------------------------------------------------------!

contains

    subroutine parse_int_si(keyword, var, data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        integer(si),intent(inout)   :: var

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
    end subroutine parse_int_si

    subroutine parse_int_di(keyword, var, data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        integer(di),intent(inout)   :: var

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
    end subroutine parse_int_di


    subroutine parse_real_sp(keyword, var, data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        real(sp),intent(inout)      :: var

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

    end subroutine parse_real_sp


    subroutine parse_real_dp(keyword, var, data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        real(dp),intent(inout)      :: var

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

    end subroutine parse_real_dp


    subroutine parse_str(keyword, var, data_file)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*),intent(in)                       :: keyword
        character(len=*),intent(in)                       :: data_file
        character(:),intent(inout), allocatable           :: var

        character(len=256)                                :: val_st
        integer                                           :: iost
        !----------------------------------------------------------------------!
        allocate(character(256) :: var)
        call get_value(keyword,data_file,val_st,iost)

        if ( iost == 0 ) then
            read(val_st,fmt='(A)',IOSTAT=iost) var
            var = trim(var)
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//TRIM(data_file)//') with keyword: ', keyword
            stop
        end if

    end subroutine parse_str

    subroutine parse_log(keyword, var, data_file, break)

        implicit none
        !
        !
        !----------------------------------------------------------------------!
        character(len=*), intent(in)            :: keyword
        character(len=*), intent(in)            :: data_file
        LOGICAL, intent(in), optional           :: break
        LOGICAL, intent(inout)                  :: var

        character(len=256)                      :: val_st
        integer                                 :: iost
        LOGICAL                                 :: break_ = .false.
        !----------------------------------------------------------------------!
        if (present(break)) break_ = break

        call get_value(keyword, data_file, val_st, iost)
        if ( iost == 0 ) then
            read(val_st,fmt='(L1)',IOSTAT=iost) var
        end if
        if ( iost == -1 ) then
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
        integer                          :: key_found

        !----------------------------------------------------------------------!
        iost = 0
        val_st = ''
        key_found = 0
        keyword_end = 0
        value_end = 0
        comment_str_ = '!'

        if (present(comment_str) ) comment_str_ = comment_str

        open(newunit=nio, &
            file=data_file, &
            action="read", &
            status="old", &
            form="formatted")

        line = ''
        do while (key_found == 0)
            ! read the next line of the input file
            !
            read(unit=nio,fmt='(A)',iostat=iostat) line
            if (iostat < 0) then
                ! end of file and no keyword is found
                ! print *, 'ERROR: Keyword not found in input file'
                iost = -1
                exit
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
                !         exit
                !     end if
                ! end do

                ! do br_r = br_l+1, j
                !     if (line(br_r:br_r) == '}' ) then
                !         exit
                !     end if
                ! end do

                !check if brackets have been found.
                ! if (br_l == j .or. br_r == j+1 ) then
                !     iost = 1
                !     !print *, 'ERROR: no value found (brackets not correct?)'
                !     exit
                ! end if

                ! check for comments
                value_end = index(line, comment_str_) - 1

                if (value_end == -1) then
                    value_end = len_trim(line)
                else if (value_end <= keyword_end) then
                    value_end = keyword_end
                    !print *, 'ERROR: line starts with a comment symbol'
                end if

                ! finally set the string to the given var
                !
                ! val_st = line(br_l+1:br_r-1)
                val_st = line(keyword_end+1:value_end)

            end if

        end do
        close(unit=nio)

    end subroutine get_value

end module parser_mod
