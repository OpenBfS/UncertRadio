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
module file_io

    ! A collection of routines for file input/output
    implicit none
    !----------------------------------------------------------------------------------------------!
    private
    !----------------------------------------------------------------------------------------------!
    public ::  &
        write_text_file, &
        logger, &
        read_config, &
        close_file, &
        close_all_files
    !----------------------------------------------------------------------------------------------!
    interface read_config
    !
    ! These routines have been developed to handle input files in a more
    ! flexible and generic way
    !
        module procedure parse_int_i1,                                                             &
                         parse_int_i2,                                                             &
                         parse_real_r1,                                                            &
                         parse_real_r2,                                                            &
                         parse_str,                                                                &
                         parse_log
    end interface

    type :: fileHandle
        integer            :: unit
        character(len=256) :: filename
    end type fileHandle

    type(fileHandle), allocatable :: open_files(:)
    !----------------------------------------------------------------------------------------------!

contains

    subroutine logger(unit, text, new, stdout, close)

        !------------------------------------------------------------------------------------------!
        !   A subroutine to write log files and nothing more.
        !   In the end it calls the general write_text_file routine.
        !
        !   Fortran units used by UncertRadio (historical reasons):
        !     15  : report file
        !     16  : for output to file UR-Saved-Results.csv
        !     17  : intermediate file used for printing
        !     18  : used locally in URGladeSys; PrepTranslate.txt > not used atm
        !     19  : Gladefile; Batlist_resu*;
        !     20  : Gladefile;
        !     21  : in Uncw_sub3
        !     22  : linfout.txt; Prepreport
        !     23  : LSQGEN-Output
        !     24  : mcmc output file
        !     25  : Projekt-Datei *.txp  oder *.csv (ProRead, ProSave,...)
        !     26  : fnameMCB; Batch_proc
        !     28  : mcmc output file
        !     30  : Output von NWG-Iteration (decision threshold, detection limit)
        !     32  : loadsel_diag_new
        !     34  : lesen settings.ini
        !     44  : WDListstoreFill_table
        !     55  : Output von Gleichungen-Interpretieren
        !     62  : in Batest
        !     63  : output von MCtables
        !     65  : Output von URGladeSys
        !     66  : Standard-Kontroll-Output von UR
        !     67  : Batest, Cofellipse, DisplayHelp,Loadseldiag_new,MCCalc,...diverse
        !     69  : Lsqlincov2
        !     71,72,73  : run_mcmc_metrop
        !     76  : MCcalc, Batest
        !     77  : Ausgabe URExport
        !     78  : URExport: Ausgabe covmat1.txt
        !     79  : URExport: Ausgabe data1.txt
        !     96  : Read URunits
        !     97  : URGladesys
        !
        use ur_general_globals, only: log_path, results_path
        implicit none
        !------------------------------------------------------------------------------------------!
        integer, intent(in)                                  :: unit
        character(len=*), intent(in)                         :: text
        logical, intent(in), optional                        :: new, stdout, close

        logical                                              :: tmp_new, tmp_stdout, tmp_close
        character(:), allocatable                            :: tmp_status, full_file_name
        !------------------------------------------------------------------------------------------!
        tmp_new = .false.
        tmp_close = .false.
        tmp_stdout = .false.

        if (present(new)) tmp_new = new
        if (present(close)) tmp_close = close

        if (tmp_new) then
            tmp_status = 'new'
        else if (tmp_close) then
            tmp_status = 'close'
        ! not sure if we need this case
        ! else if (tmp_close .and. tmp_new) tmp_status = 'newclose'
        else
            tmp_status = 'unknown'
        end if

        ! now set the file_name, depending on the given unit (see bellow)
        select case (unit)
        case(22)
            full_file_name = results_path // 'linfout.txt'

        case(30)
            full_file_name = log_path // 'char_limits.txt'

        case(63)
            full_file_name = results_path // 'MC_Tables.txt'

        case(66)
            ! this is the main log file
            full_file_name = log_path // 'main_log.txt'
            !tmp_stdout = .true.  ! write everything to stdout as well
        case default
            allocate(character(len(log_path) + 32) :: full_file_name)
            ! atm if the case is not specified, we fall back to the old file-Format
            write(full_file_name, '(2A, I0, A)') log_path, 'Fort' , unit, '.txt'
        end select

        if (present(stdout)) tmp_stdout = stdout

        ! this routines is designed to write to file in the first place
        if (unit > 10) then
            call write_text_file(text, full_file_name, tmp_status)
            ! write to stdout as well if desired
            if (tmp_stdout) write(*, *) trim(text)
        end if

    end subroutine logger
    !----------------------------------------------------------------------------------------------!


    subroutine write_text_file(text, full_filename, status, utf8_filename)
        !------------------------------------------------------------------------------------------!
        !   This is a very basic routine to write text files for UR in a generalizes way.
        !   It can be used to write log files, result files, etc.
        !
        ! Inputs:
        !   text:           Text to be written to the file
        !   full_filename: the full path of the file to write to
        !
        ! Optional Input:
        !   status:         Status of file operation ('new' to replace existing file)
        !
        use chf, only: flfu
        implicit none
        !------------------------------------------------------------------------------------------!
        character(len=*), intent(in)           :: text
        character(len=*), intent(in)           :: full_filename
        character(len=*), intent(in), optional :: status
        logical, intent(in), optional          :: utf8_filename

        character(len=:), allocatable          :: full_filename_final
        integer                                :: nio
        integer                                :: i
        logical                                :: tmp_utf8_filename
        logical                                :: tmp_close
        !------------------------------------------------------------------------------------------!

        tmp_utf8_filename = .true.
        tmp_close = .false.
        if (present(utf8_filename)) tmp_utf8_filename = utf8_filename

        ! if the filename has utf-8 encoding, convert to the local encoding
        if (tmp_utf8_filename) then
            full_filename_final = flfu(full_filename)
        else
            full_filename_final = full_filename
        end if

        ! check if a status is present
        if ( present(status) ) then
            ! this routine only allows to create a new file or append
            ! the text to the existing file (default case)

            if (status == 'new') then
                ! check if the file is open, if so, close it:
                if (allocated(open_files)) then
                    do i = 1, size(open_files)
                        if (trim(open_files(i)%filename) == trim(full_filename_final)) then
                            call close_file(full_filename_final)
                            exit
                        end if
                    end do
                end if
            else if (status == 'close') then
                tmp_close = .true.
            end if
        end if
        call open_file(full_filename_final, unit=nio)
        write(nio, '(A)') trim(text)
        if (tmp_close) call close_file(full_filename_final)

    end subroutine write_text_file


    subroutine open_file(filename, unit)
        !------------------------------------------------------------------------------------------!
        ! opens a specified file and adds it to the list of open files.
        !
        ! Inputs:
        !   filename - The full filename of the file to be opened.
        !
        ! Outputs:
        !   unit - The unit number assigned to the opened file.
        !
        ! Side Effects:
        !   - Opens the specified file in append mode.
        !   - Adds the file to the list of open files.
        !   - Prints an error message if the file cannot be opened.
        !
        ! Notes:
        !   - If the file is already open, the subroutine returns the existing unit number.
        !
        !------------------------------------------------------------------------------------------!

        implicit none
        !------------------------------------------------------------------------------------------!
        character(len=*), intent(in)  :: filename
        type(fileHandle), allocatable :: openFiles_tmp(:)
        integer, intent(out)          :: unit
        integer :: ios, i, num_open_files
        !------------------------------------------------------------------------------------------!
        ! Check if the file is already open
        if (allocated(open_files)) then
            do i = 1, size(open_files)
                if (trim(open_files(i)%filename) == trim(filename)) then
                    unit = open_files(i)%unit
                    return
                end if
            end do
        end if

        ! Open the file
        open(newunit=unit, file=filename, status='replace', &
             action='write', position='append', iostat=ios)
        if (ios /= 0) then
            ! Handle the error
            write (*,*) 'Error opening file:', filename, ios
            return
        end if

        ! Add the file to the list of open files
        if (.not. allocated(open_files)) then
            allocate(open_files(1))
            num_open_files = 1
        else
            num_open_files = size(open_files) + 1
            allocate(openFiles_tmp, source=open_files)
            deallocate(open_files)

            allocate(open_files(num_open_files))
            open_files(1:num_open_files-1) = openFiles_tmp
        end if

        open_files(num_open_files)%unit = unit
        open_files(num_open_files)%filename = filename

    end subroutine open_file

    subroutine close_file(filename)
        !------------------------------------------------------------------------------------------!
        ! Closes a specified file and removes it from the list of open files.
        !
        ! Inputs:
        !   filename - The full filename of the file to be closed.
        !
        ! Side Effects:
        !   - Closes the specified file.
        !   - Removes the file from the list of open files.
        !   - Prints an error message if the file is not found in the list of open files.
        !
        !------------------------------------------------------------------------------------------!
        implicit none
        !------------------------------------------------------------------------------------------!
        character(len=*), intent(in) :: filename

        type(fileHandle), allocatable :: open_files_tmp(:)
        integer :: i, num_open_files
        !------------------------------------------------------------------------------------------!

        ! Find the file in the list of open files

        if (allocated(open_files)) then
            do i = 1, size(open_files)
                if (trim(open_files(i)%filename) == trim(filename)) then
                    ! Close the file
                    close(open_files(i)%unit)

                    ! Remove the file from the list of open files (at position i)
                    num_open_files = size(open_files)
                    allocate(open_files_tmp, source=open_files)
                    deallocate(open_files)

                    allocate(open_files(num_open_files-1))
                    open_files(1:i-1) = open_files_tmp(1:i-1)
                    open_files(i:num_open_files-1) = open_files_tmp(i+1:num_open_files)
                    deallocate(open_files_tmp)

                    num_open_files = num_open_files - 1
                    if (num_open_files == 0) then
                        deallocate(open_files)
                    end if
                    return
                end if
            end do
        end if

        ! If the file is not found, print an error message
        write (*,*) 'Error closing file: ', trim(filename)

    end subroutine close_file


    subroutine close_all_files()
        !------------------------------------------------------------------------------------------!
        ! Closes all open files and deallocates the open_files array.
        !
        ! Side Effects:
        !   - Closes all open files.
        !   - Deallocates the open_files array if all files are closed.
        !
        implicit none
        !------------------------------------------------------------------------------------------!
        integer :: i
        !------------------------------------------------------------------------------------------!

        ! Close all opend files:
        do i = 1, size(open_files)
            close(open_files(i)%unit)
        end do

        deallocate(open_files)


    end subroutine close_all_files


    subroutine show_open_files()
        !------------------------------------------------------------------------------------------!
        ! Show the list of open files
        !
        ! This subroutine prints the list of open files, including their unit numbers and filenames.
        !
        ! @note This subroutine is for debugging purposes only.
        !
        implicit none
        !------------------------------------------------------------------------------------------!
        integer :: i
        !------------------------------------------------------------------------------------------!

        write(*,'(A,I2, I2)') 'N: ', size(open_files)
        do i = 1,  size(open_files)
            write(*,'(A, I3, 1X, A)') 'Unit: ', open_files(i)%unit, &
                                      'Filename: ' // open_files(i)%filename
        end do

    end subroutine show_open_files

    !---------------------------------------------------------------------------------------------!
    subroutine parse_int_i1(keyword, var, data_file)

        implicit none

        !-----------------------------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        integer,intent(inout)       :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !------------------------------------------------------------------------------------------!
        call get_value(keyword,data_file,val_st,iost)
        if ( iost == 0 ) then
            read(val_st,fmt=*,IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//trim(data_file)//') with keyword: ', keyword
            stop
        end if
    end subroutine parse_int_i1

    subroutine parse_int_i2(keyword, var, data_file)

        implicit none

        !------------------------------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        integer(8),intent(inout)    :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !------------------------------------------------------------------------------------------!
        call get_value(keyword,data_file,val_st,iost)
        if ( iost == 0 ) then
            read(val_st,fmt=*,IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//trim(data_file)//') with keyword: ', keyword
            stop
        end if
    end subroutine parse_int_i2


    subroutine parse_real_r2(keyword, var, data_file)

        implicit none

        !------------------------------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        REAL,intent(inout)          :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !------------------------------------------------------------------------------------------!
        call get_value(keyword, data_file, val_st, iost)
        if ( iost == 0 ) then
            read(val_st, fmt=*, IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//trim(data_file)//') with keyword: ', keyword
            stop
        end if

    end subroutine parse_real_r2


    subroutine parse_real_r1(keyword, var, data_file)

        use UR_types, only: rn
        implicit none

        !------------------------------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        REAL(kind=rn),intent(inout) :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !------------------------------------------------------------------------------------------!
        call get_value(keyword, data_file, val_st, iost)
        if ( iost == 0 ) then
            read(val_st, fmt=*, IOSTAT=iost) var
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//trim(data_file)//') with keyword: ', keyword
            stop
        end if

    end subroutine parse_real_r1


    subroutine parse_str(keyword, var, data_file)

        implicit none

        !------------------------------------------------------------------------------------------!
        character(len=*),intent(in)                       :: keyword
        character(len=*),intent(in)                       :: data_file
        character(:),intent(inout), allocatable           :: var

        character(len=256)                                :: val_st
        integer                                           :: iost
        !------------------------------------------------------------------------------------------!
        allocate(character(256) :: var)
        call get_value(keyword,data_file,val_st,iost)

        if ( iost == 0 ) then
            read(val_st,fmt='(A)',IOSTAT=iost) var
            var = trim(var)
        end if
        if ( iost /= 0 ) then
            print '(2A)','ERROR in input file ('//trim(data_file)//') with keyword: ', keyword
            stop
        end if

    end subroutine parse_str

    subroutine parse_log(keyword, var, data_file, break)

        implicit none

        !------------------------------------------------------------------------------------------!
        character(len=*), intent(in)            :: keyword
        character(len=*), intent(in)            :: data_file
        logical, intent(in), optional           :: break
        logical, intent(inout)                  :: var

        character(len=256)                      :: val_st
        integer                                 :: iost
        logical                                 :: break_ = .false.
        !------------------------------------------------------------------------------------------!
        if (present(break)) break_ = break

        call get_value(keyword, data_file, val_st, iost)
        if ( iost == 0 ) then
            read(val_st,fmt='(L1)',IOSTAT=iost) var
        end if
        if ( iost == -1 ) then
            print '(2A)','ERROR in input file ('//trim(data_file)//') with keyword: ', keyword
            stop
        end if


    end subroutine parse_log

    ! in thus routine the actual work (find the keyword, return the value etc.)
    ! is done
    !
    subroutine get_value(keyword, data_file, val_st, iost, comment_str)

        use chf, only: flfu
        implicit none
        ! This routine searches each line for the keyword and returns the value
        ! as a string
        !------------------------------------------------------------------------------------------!
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

        !------------------------------------------------------------------------------------------!
        iost = 0
        val_st = ''
        key_found = 0
        keyword_end = 0
        value_end = 0
        comment_str_ = '!'

        if (present(comment_str) ) comment_str_ = comment_str

        open(newunit=nio, &
             file=flfu(data_file), &
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


end module file_io
