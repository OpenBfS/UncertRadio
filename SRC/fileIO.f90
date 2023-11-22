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
!-------------------------------------------------------------------------------------------------!
module file_io

    implicit none
    !---------------------------------------------------------------------------------------------!
    private
    !---------------------------------------------------------------------------------------------!
    public ::  &
        FNopen, &
        FNclose
    !---------------------------------------------------------------------------------------------!

contains

    subroutine FNopen(fnum, file_name, status)
        !-----------------------------------------------------------------------------------------!
        use UR_VARIABLES,                   only :  work_path
        integer(4), intent(in)                   :: fnum       ! number of I/O unit

        integer(4)                               :: ios
        character(len=*), intent(in)             :: file_name
        character(len=4), intent(in), optional   :: status
        logical                                  :: exists

        !-----------------------------------------------------------------------------------------!

        if (.not. present(status) ) then
            status = 'old'
        end if
        inquire(file=trim(work_path), exist=exists)
        if(exists) then
            open(unit=fnum, file=trim(work_path) // trim(file_name), &
                status=status, action=action, iostat=ios)
            if (ios /= 0) then
                write(*,*) 'Error: could not write to file: ' // trim(work_path) // trim(file_name)
            end if
        end if

    end subroutine FNopen


    subroutine FNclose(fnum)
        !-----------------------------------------------------------------------------------------!
        use UR_VARIABLES,     only: work_path

        implicit none

        integer(4),intent(in)    :: fnum       ! number of I/O unit

        integer(4)          :: ivals(13),ios,j,k
        character(len=150)  :: fortname,str1
        character(len=255)  :: cmdstring
        logical             :: prout
        !-----------------------------------------------------------------------------------------!
        prout = .false.
        ! prout = .true.

        close (fnum)
        if(len_trim(actpath) == 0) actpath = work_path

        write(fortname,'(a,I0,a)') 'fort', fnum,'.txt'
        fortname = trim(actpath) // trim(fortname)
        if(prout) write(0,*) 'Fortname =' ,trim(fortname)

        call stat(fortname,ivals,ios)
        if(prout) write(0,'(a,i0,13i8)') 'ios=',ios,ivals(8)
        if(ios == 0 .and. ivals(8) == 0) then
            str1 = ''
            cmdstring = 'del ' // trim(fortname)
            if(prout) write(0,*) 'cmdstring=',trim(cmdstring)
            CALL EXECUTE_COMMAND_LINE(cmdstring, wait=.false., EXITSTAT=j, CMDSTAT=k,CMDMSG=str1)
            if(prout) write(0,*) ' EXITSTAT=',j,'  CMDSTAT=',k
            if(k /= 0 .and. len_trim(str1) > 0) write(66,*) '       Message=',trim(str1)
        end if

    end subroutine FNclose

!#################################################################################
end module file_io
