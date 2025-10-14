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
module file_io

    ! A collection of routines for file input/output
    implicit none
    !---------------------------------------------------------------------------------------------!
    private
    !---------------------------------------------------------------------------------------------!
    public ::  &
        write_text_file, &
        logger, &
        read_config, &
        closeFile, &
        closeAllFiles
    !---------------------------------------------------------------------------------------------!
    interface read_config
    !
    ! These routines have been developed to handle input files in a more
    ! flexible and generic way
    !
        module procedure parse_int_i1,                                                            &
                         parse_int_i2,                                                            &
                         parse_real_r1,                                                           &
                         parse_real_r2,                                                           &
                         parse_str,                                                               &
                         parse_log
    end interface

    type :: fileHandle
        integer            :: unit
        character(len=256) :: filename
    end type fileHandle

    type(fileHandle), allocatable :: openFiles(:)
    integer, save :: numOpenFiles = 0
    !---------------------------------------------------------------------------------------------!

contains

    subroutine logger(unit, text, new, stdout)

        use ur_general_globals, only: log_path, results_path
        !-----------------------------------------------------------------------------------------!
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
        !
        !-----------------------------------------------------------------------------------------!
        integer, intent(in)                                  :: unit
        character(len=*), intent(in)                         :: text
        logical, intent(in), optional                        :: new, stdout

        logical                                              :: tmp_new, tmp_stdout
        character(:), allocatable                            :: tmp_status, full_file_name
        !-----------------------------------------------------------------------------------------!
        tmp_new = .false.
        if (present(new)) tmp_new = new

        if (tmp_new) then
            tmp_status = 'new'
        else
            tmp_status = 'unknown'
        end if

        tmp_stdout = .false.

        ! now set the file_name, depending on the given unit (see bellow)
        select case (unit)
        case(22)
            full_file_name = results_path // 'linfout.txt'

        case(30)
            full_file_name = log_path // 'char_limits.txt'

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

        use chf, only: flfu
        !------------------------------------------------------------------------------------------!
        !   This is a very basic routine to write text files for UR in a generalizes way.
        !   It can be used to write log files, result files, etc.
        !
        ! Inputs:
        !   text:           Text to be written to the file
        !   full_filename: the full path of the file to write to
        !
        ! Optional Input:
        !   status:         Status of file operation ('new' to replace existing file,
        !                   default is 'unknown')
        !------------------------------------------------------------------------------------------!
        character(len=*), intent(in)                         :: text
        character(len=*), intent(inout)                      :: full_filename
        character(len=*), intent(in), optional               :: status
        logical, intent(in), optional                        :: utf8_filename

        integer                                              :: nio
        integer                                              :: i
        logical                                              :: tmp_utf8_filename
        !-----------------------------------------------------------------------------------------!

        tmp_utf8_filename = .true.
        if (present(utf8_filename)) tmp_utf8_filename = utf8_filename

        ! if the filename has utf-8 encoding, convert to the local encoding
        if (tmp_utf8_filename) then
            full_filename = flfu(full_filename)
        end if

        ! check if a status is present
        if ( present(status) ) then
            ! this routine only allows to create a new file or append
            ! the text to the existing file (default case)

            if (status == 'new') then
                ! check if the file is open, if so, close it:
                do i = 1, numOpenFiles
                    if (trim(openFiles(i)%filename) == trim(full_filename)) then
                        call closeFile(full_filename)
                    end if
                end do
            end if
        end if

        call openFile(full_filename, unit=nio)
        write(nio, '(A)') trim(text)

    end subroutine write_text_file


    subroutine openFile(filename, unit)
        !------------------------------------------------------------------------------------------!
        character(len=*), intent(in)  :: filename
        type(fileHandle), allocatable :: openFiles_tmp(:)
        integer, intent(out)          :: unit
        integer :: ios, i
        !------------------------------------------------------------------------------------------!
        ! Check if the file is already open
        do i = 1, numOpenFiles
            if (trim(openFiles(i)%filename) == trim(filename)) then
                unit = openFiles(i)%unit
                return
            end if
        end do

        ! Open the file
        open(newunit=unit, file=filename, status='replace', &
             action='write', position='append', iostat=ios)
        if (ios /= 0) then
            ! Handle the error
            write (*,*) 'Error opening file:', filename, ios
            return
        end if

        ! Add the file to the list of open files
        numOpenFiles = numOpenFiles + 1
        if (.not. allocated(openFiles)) then
            allocate(openFiles(1))
        else
            allocate(openFiles_tmp(numOpenFiles-1))
            openFiles_tmp = openFiles
            deallocate(openFiles)
            allocate(openFiles(numOpenFiles))
            openFiles(1:size(openFiles_tmp)) = openFiles_tmp
        end if
        openFiles(numOpenFiles)%unit = unit
        openFiles(numOpenFiles)%filename = filename

    end subroutine openFile

    subroutine closeFile(filename)
        character(len=*), intent(in) :: filename
        integer :: i

        ! Find the file in the list of open files
        do i = 1, numOpenFiles
            if (trim(openFiles(i)%filename) == trim(filename)) then
                ! Close the file
                close(openFiles(i)%unit)

                ! Remove the file from the list of open files
                openFiles(i:numOpenFiles-1) = openFiles(i+1:numOpenFiles)
                numOpenFiles = numOpenFiles - 1
                if (numOpenFiles == 0) then
                    deallocate(openFiles)
                end if
                return
            end if
        end do

        ! If the file is not found, print an error message
        write (*,*) 'Error closing file: ', trim(openFiles(i)%filename)

    end subroutine closeFile


    subroutine closeAllFiles()
        integer :: i
        ! Close all opend files:
        do i = 1, numOpenFiles
            close(openFiles(i)%unit)
            numOpenFiles = numOpenFiles - 1
            if (numOpenFiles == 0) then
                deallocate(openFiles)
                return
            end if
        end do

    end subroutine closeAllFiles

    !---------------------------------------------------------------------------------------------!
    subroutine parse_int_i1(keyword, var, data_file)

        implicit none

        !-----------------------------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        integer,intent(inout)       :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !-----------------------------------------------------------------------------------------!
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

        !-----------------------------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        integer(8),intent(inout)    :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !-----------------------------------------------------------------------------------------!
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

        !-----------------------------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        REAL,intent(inout)          :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !-----------------------------------------------------------------------------------------!
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

        !-----------------------------------------------------------------------------------------!
        character(len=*),intent(in) :: keyword
        character(len=*),intent(in) :: data_file
        REAL(kind=rn),intent(inout) :: var

        character(len=256)          :: val_st
        integer                     :: iost
        !-----------------------------------------------------------------------------------------!
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

        !-----------------------------------------------------------------------------------------!
        character(len=*),intent(in)                       :: keyword
        character(len=*),intent(in)                       :: data_file
        character(:),intent(inout), allocatable           :: var

        character(len=256)                                :: val_st
        integer                                           :: iost
        !-----------------------------------------------------------------------------------------!
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

        !-----------------------------------------------------------------------------------------!
        character(len=*), intent(in)            :: keyword
        character(len=*), intent(in)            :: data_file
        LOGICAL, intent(in), optional           :: break
        LOGICAL, intent(inout)                  :: var

        character(len=256)                      :: val_st
        integer                                 :: iost
        LOGICAL                                 :: break_ = .false.
        !-----------------------------------------------------------------------------------------!
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
        !-----------------------------------------------------------------------------------------!
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

        !-----------------------------------------------------------------------------------------!
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
