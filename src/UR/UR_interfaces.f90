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
module ur_interfaces

    implicit none

    interface

        subroutine displayhelp(ncitem, idstr)
            implicit none
            integer, intent(in)                     :: ncitem
            character(len=*), optional, intent(in)  :: idstr
        end subroutine displayhelp

        recursive subroutine processloadpro_new(iwahl, kegrneu)
            implicit none
            integer, intent(in)             :: iwahl
            integer, intent(in), optional   :: kegrneu

        end subroutine processloadpro_new

        subroutine plot3fig(knum, nkpts, ncurve, line_styles, line_widths, &
                            xlog, ylog, xlab, ylab, ptitle, pltfile, &
                            mimax, mimay, ctextl)
            implicit none

            integer, intent(in)                    :: knum           ! number of curves
            integer, intent(in)                    :: nkpts(knum)    ! number of points per curve
            integer, intent(in)                    :: ncurve(knum)   ! numbers of curve shapes
            integer, intent(in)                    :: line_styles(knum)   ! numbers of curve shapes
            real(8), intent(in)                    :: line_widths(knum)   ! numbers of curve shapes
            real(8), intent(in), optional          :: mimax(2)      ! xminv,xmaxv
            real(8), intent(in), optional          :: mimay(2)      ! yminv,ymaxv
            logical, intent(in)                    :: xlog,ylog
            character(len=*), intent(in)           :: xlab,ylab
            character(len=*), intent(in), optional :: ctextl(knum)
            character(len=*), intent(in)           :: ptitle
            character(len=*), intent(in)           :: pltfile
        end subroutine plot3fig

    end interface
end module ur_interfaces
