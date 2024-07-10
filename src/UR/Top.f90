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

    use UR_params,       only: rn
    use UR_Gleich,       only: charv

    interface


        module function idpt(strid) Result(ptr)
            use, intrinsic :: iso_c_binding,     only: c_ptr
            type(c_ptr)                   :: ptr
            character(len=*),intent(in)   :: strid      ! Name des ID-Labels aus dem Glade-File
        end function idpt


        module subroutine FindItemP(ptr, ncitem)
            use, intrinsic :: iso_c_binding,     only: c_ptr
            type(c_ptr),value,intent(in)  :: ptr
            integer(4),intent(out)        :: ncitem
        end subroutine FindItemP


        module subroutine FindItemS(dialogstr, ncitem)
            character(len=*),intent(in)   :: dialogstr
            integer(4),intent(out)        :: ncitem
        end subroutine FindItemS


        module SUBROUTINE FieldUpdate()
        END SUBROUTINE FieldUpdate


        module subroutine WrStatusbar(k,string)
            integer(4),intent(in)         :: k       ! statusbar number
            character(len=*),intent(in)   :: string  ! output text
        end subroutine WrStatusbar

        module real(rn) FUNCTION dpafact(x)
            real(rn),INTENT(IN)    :: x    !
        END function dpafact

        module logical function chupper_eq(str1,str2)
            CHARACTER(LEN=*),INTENT(IN) :: Str1
            CHARACTER(LEN=*),INTENT(IN) :: Str2
        end function chupper_eq


        module subroutine MDcalc(k_datvar)
            integer,intent(in)     :: k_datvar
        end subroutine MDcalc

        module subroutine PixelPerString(dialog,string,wdpixel,htpixel)
            use, intrinsic :: iso_c_binding,     only: c_ptr
            type(c_ptr), value           :: dialog
            character(len=*),intent(in)  :: string
            integer,intent(out)          :: wdpixel,htpixel
        end subroutine PixelPerString


        module subroutine CharModA1(array,n1)
            integer(4),intent(in)    :: n1
            type(charv),allocatable,intent(inout)  :: array(:)
        end subroutine CharModA1


        module subroutine CharModA2(array,n1,n2)         ! ,istep)
            integer(4),intent(in)    :: n1,n2
            type(charv),allocatable,intent(inout)  :: array(:,:)
        end subroutine CharModA2


        module subroutine IntModA1(array,n1)
            integer(4),intent(in)    :: n1
            integer(4),allocatable   :: array(:)
        end subroutine IntModA1


        module subroutine RealModA1(array,n1,kmin)
            integer(4),intent(in)          :: n1
            integer(4),intent(in),optional :: kmin
            real(rn),allocatable           :: array(:)
        end subroutine RealModA1


        module subroutine LogModA1(array,n1)         ! ,istep)
            integer(4),intent(in)    :: n1
            logical,allocatable      :: array(:)
        end subroutine LogModA1


        module subroutine InitVarsTV2_CP(nng)
            integer(4),intent(in)    :: nng
        end subroutine InitVarsTV2_CP


        module subroutine InitVarsTV2(nng)
            integer(4),intent(in)    :: nng
        end subroutine InitVarsTV2


        module subroutine InitVarsTV5_CP(kxy)
            integer(4),intent(in)    :: kxy
        end subroutine InitVarsTV5_CP


        module subroutine InitVarsTV5(kxy)
            integer(4),intent(in)    :: kxy
        end subroutine InitVarsTV5


        module subroutine InitVarsTV3(ncov)
            integer(4),intent(in)    :: ncov
        end subroutine InitVarsTV3


        module subroutine InitVarsTV3_CP(ncov)
            integer(4),intent(in)    :: ncov
        end subroutine InitVarsTV3_CP


        module subroutine InitVarsTV6_CP(kxy)
            integer(4),intent(in)    :: kxy
        end subroutine InitVarsTV6_CP


        module subroutine InitVarsTV6(kxy)
            integer(4),intent(in)    :: kxy
        end subroutine InitVarsTV6


        module subroutine InitVarsTV7(kxy)
            integer(4),intent(in)    :: kxy
        end subroutine InitVarsTV7


        module subroutine InitVarsTV8(kxy)
            integer(4),intent(in)    :: kxy
        end subroutine InitVarsTV8


        module subroutine DRead(kunit,text,ios)
            integer(4), intent(in)      :: kunit
            character(:),allocatable,intent(out)    :: text
            integer(4),intent(out)      :: ios
        end subroutine DRead


        module subroutine GetCells(text, cell, cconv)              !  enloc, slen,sdim,cconv)
            CHARACTER(LEN=*),INTENT(IN)  :: text
            type(charv),allocatable      :: cell(:)
            CHARACTER(LEN=1),INTENT(IN)  :: cconv

        end subroutine GetCells


        module subroutine ModVarsTV2(nng)
            integer(4),intent(in)    :: nng
        end subroutine ModVarsTV2


        module subroutine ModVarsTV2_CP(nng)
            integer(4),intent(in)    :: nng
        end subroutine ModVarsTV2_CP


        module subroutine IntModA2(array,n1,n2)
            integer(4),intent(in)    :: n1,n2
            integer(4),allocatable   :: array(:,:)
        end subroutine IntModA2

        module subroutine CharModStr(str,n)
            character(:),allocatable,intent(inout) :: str
            integer(4),intent(in)                  :: n
        end subroutine


        module subroutine RealModA2(array,n1,n2)
            integer(4),intent(in)    :: n1,n2
            real(rn),allocatable   :: array(:,:)
        end subroutine RealModA2


        module subroutine load_unit_conv(nvars)
            integer(4),intent(in)   :: nvars    ! number of vars; at the moment: ngrs+ncov
        end subroutine load_unit_conv

        module subroutine uconvert(einhp,factor,einhp_new)
            character(len=*),intent(in)  :: einhp
            real(rn),intent(out)         :: factor
            character(len=*),intent(inout) :: einhp_new
        end subroutine uconvert

    !#######################################################################

    end interface

end module top
