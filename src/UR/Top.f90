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

module top
    use, intrinsic :: iso_c_binding,     only: c_ptr
    use ur_params,                       only: rn
    use ur_gleich,                       only: charv

    interface


        module function idpt(strid) result(ptr)
            type(c_ptr)                   :: ptr
            character(len=*),intent(in)   :: strid      ! name des id-labels aus dem glade-file
        end function idpt


        module subroutine finditemp(ptr, ncitem)

            type(c_ptr),value,intent(in)  :: ptr
            integer   ,intent(out)        :: ncitem
        end subroutine finditemp


        module subroutine finditems(dialogstr, ncitem)
            character(len=*),intent(in)   :: dialogstr
            integer   ,intent(out)        :: ncitem
        end subroutine finditems


        module subroutine fieldupdate()
        end subroutine fieldupdate


        module subroutine wrstatusbar(k,string)
            integer   ,intent(in)         :: k       ! statusbar number
            character(len=*),intent(in)   :: string  ! output text
        end subroutine wrstatusbar

        module real(rn) function dpafact(x)
            real(rn),intent(in)    :: x    !
        end function dpafact

        module logical function chupper_eq(str1,str2)
            character(len=*),intent(in) :: str1
            character(len=*),intent(in) :: str2
        end function chupper_eq


        module subroutine mdcalc(k_datvar)
            integer,intent(in)     :: k_datvar
        end subroutine mdcalc

        module subroutine pixelperstring(dialog,string,wdpixel,htpixel)
            type(c_ptr), value           :: dialog
            character(len=*),intent(in)  :: string
            integer,intent(out)          :: wdpixel,htpixel
        end subroutine pixelperstring


        module subroutine charmoda1(array,n1)
            integer   ,intent(in)    :: n1
            type(charv),allocatable,intent(inout)  :: array(:)
        end subroutine charmoda1


        module subroutine charmoda2(array,n1,n2)         ! ,istep)
            integer   ,intent(in)    :: n1, n2
            type(charv),allocatable,intent(inout)  :: array(:,:)
        end subroutine charmoda2


        module subroutine intmoda1(array,n1)
            integer   ,intent(in)    :: n1
            integer   ,allocatable   :: array(:)
        end subroutine intmoda1


        module subroutine realmoda1(array,n1,kmin)
            integer   ,intent(in)          :: n1
            integer   ,intent(in),optional :: kmin
            real(rn),allocatable           :: array(:)
        end subroutine realmoda1


        module subroutine logmoda1(array,n1)
            integer   ,intent(in)    :: n1
            logical,allocatable      :: array(:)
        end subroutine logmoda1


        module subroutine initvarstv2_cp(nng)
            integer   ,intent(in)    :: nng
        end subroutine initvarstv2_cp


        module subroutine initvarstv2(nng)
            integer   ,intent(in)    :: nng
        end subroutine initvarstv2


        module subroutine initvarstv5_cp(kxy)
            integer   ,intent(in)    :: kxy
        end subroutine initvarstv5_cp


        module subroutine initvarstv5(kxy)
            integer   ,intent(in)    :: kxy
        end subroutine initvarstv5


        module subroutine initvarstv3(ncov)
            integer   ,intent(in)    :: ncov
        end subroutine initvarstv3


        module subroutine initvarstv3_cp(ncov)
            integer   ,intent(in)    :: ncov
        end subroutine initvarstv3_cp


        module subroutine initvarstv6_cp(kxy)
            integer   ,intent(in)    :: kxy
        end subroutine initvarstv6_cp


        module subroutine initvarstv6(kxy)
            integer   ,intent(in)    :: kxy
        end subroutine initvarstv6


        module subroutine initvarstv7(kxy)
            integer   ,intent(in)    :: kxy
        end subroutine initvarstv7


        module subroutine initvarstv8(kxy)
            integer   ,intent(in)    :: kxy
        end subroutine initvarstv8


        module subroutine dread(kunit,text,ios)
            integer   , intent(in)      :: kunit
            character(:),allocatable,intent(out)    :: text
            integer   ,intent(out)      :: ios
        end subroutine dread


        module subroutine getcells(text, cell, cconv)
            character(len=*),intent(in)  :: text
            type(charv),allocatable      :: cell(:)
            character(len=1),intent(in)  :: cconv

        end subroutine getcells


        module subroutine modvarstv2(nng)
            integer   ,intent(in)    :: nng
        end subroutine modvarstv2


        module subroutine modvarstv2_cp(nng)
            integer   ,intent(in)    :: nng
        end subroutine modvarstv2_cp


        module subroutine intmoda2(array,n1,n2)
            integer   ,intent(in)    :: n1,n2
            integer   ,allocatable   :: array(:,:)
        end subroutine intmoda2

        module subroutine charmodstr(str,n)
            character(:),allocatable,intent(inout) :: str
            integer   ,intent(in)                  :: n
        end subroutine


        module subroutine realmoda2(array,n1,n2)
            integer   ,intent(in)    :: n1,n2
            real(rn),allocatable   :: array(:,:)
        end subroutine realmoda2


        module subroutine load_unit_conv(nvars)
            integer   ,intent(in)   :: nvars    ! number of vars; at the moment: ngrs+ncov
        end subroutine load_unit_conv

        module subroutine uconvert(einhp,factor,einhp_new)
            character(len=*),intent(in)  :: einhp
            real(rn),intent(out)         :: factor
            character(len=*),intent(inout) :: einhp_new
        end subroutine uconvert

    !#######################################################################

    end interface

end module top
