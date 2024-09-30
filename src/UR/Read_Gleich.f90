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
module RG
    use UR_types
    interface

        !#######################################################################
        module recursive subroutine Read_Gleich()
            implicit none
        end subroutine Read_Gleich

        module subroutine EditFormelt(nglp,nglf,prout)
            implicit none
            integer, intent(inout)   :: nglp,nglf
            logical, intent(in)      :: prout   ! with test output or not
        end subroutine EditFormelt

        module subroutine modify_Formeltext(mode)
            use UR_Gleich,     only: nglp,nglp_read,eqnum_val,Formeltext
            implicit none
            integer, intent(in)   :: mode          ! 1: remove blank lines;  2: insert original blank lines
        end subroutine modify_Formeltext

    !###########################################################################################

    end interface

end module RG
