module ur_interfaces

    implicit none

    interface

        subroutine rechw1()
            implicit none
        end subroutine rechw1

        subroutine finditems(dialogstr, ncitem)
            !use ur_gtk_variables,  only: clobj,nclobj
            implicit none
            character(len=*),intent(in)    :: dialogstr
            integer, intent(out)           :: ncitem
        end subroutine finditems

        subroutine finditemp(ptr, ncitem)
            use, intrinsic :: iso_c_binding,     only: c_ptr,c_null_ptr,c_associated
            !use ur_gtk_variables,  only: clobj,nclobj
            implicit none
            type(c_ptr),value,intent(in)  :: ptr
            integer, intent(out)           :: ncitem
        end subroutine finditemp

        subroutine gchar (var)
            implicit none
            character*(*), intent(inout)    :: var
        end subroutine gchar

        subroutine strgencode(keystrg,textin,textout, mode, isatz)
            implicit none
            character(len=*), intent(in)     :: keystrg
            character(len=*), intent(in)     :: textin
            character(len=*), intent(out)    :: textout
            integer, intent(in)               :: mode
            integer, intent(in)               :: isatz
        end subroutine strgencode

        subroutine printgladesys(kunit)
            implicit none
            integer, intent(in)  :: kunit
        end subroutine printgladesys

        subroutine loadsel_diag_new(mode, ncitem)
            implicit none
            integer, intent(in)                  :: mode             ! 1: show dialog;  2: readout dialog and hide it
            integer, intent(in)                  :: ncitem
        end subroutine loadsel_diag_new

        subroutine displayhelp(ncitem,idstr)
            implicit none
            integer, intent(in)            :: ncitem
            character(len=*),optional,intent(in)  :: idstr
        end subroutine displayhelp

        recursive subroutine processloadpro_new(iwahl,kegrneu)
            implicit none
            integer, intent(in)            :: iwahl
            integer, intent(in),optional   :: kegrneu
        end subroutine processloadpro_new

        subroutine procmaindiag(ncitem)
            implicit none
            integer, intent(in)            :: ncitem
        end subroutine procmaindiag

        subroutine hl_gtk_listn_get_cell(list, row, col, &
        & svalue, fvalue, dvalue, ivalue, lvalue, l64value, logvalue, &
        & i8value, pbvalue)
            use, intrinsic :: iso_c_binding
            type(c_ptr), intent(in) :: list
            integer(kind=c_int), intent(in) :: row, col
            character(len=*), intent(out), optional :: svalue
            real(kind=c_float), intent(out), optional :: fvalue
            real(kind=c_double), intent(out), optional :: dvalue
            integer(kind=c_int), intent(out), optional :: ivalue
            integer(kind=c_long), intent(out), optional :: lvalue
            integer(kind=c_int64_t), intent(out), optional :: l64value
            logical, intent(out), optional :: logvalue
            integer(kind=c_int8_t), intent(out), optional :: i8value
            type(c_ptr), intent(out), optional :: pbvalue
        end subroutine hl_gtk_listn_get_cell

        subroutine hl_gtk_list_tree_get_gvalue(val, ctype, &
            svalue, fvalue, dvalue, ivalue, &
            lvalue, l64value, logvalue, &
            i8value, pbvalue)
            use, intrinsic :: iso_c_binding
            implicit none
            integer,  parameter :: type_kind=c_long
            type(c_ptr), intent(in) :: val
            integer(kind=type_kind), intent(in) :: ctype
            character(len=*), intent(out), optional :: svalue
            real(kind=c_float), intent(out), optional :: fvalue
            real(kind=c_double), intent(out), optional :: dvalue
            integer(kind=c_int), intent(out), optional :: ivalue
            integer(kind=c_long), intent(out), optional :: lvalue
            integer(kind=c_int64_t), intent(out), optional :: l64value
            logical, intent(out), optional :: logvalue
            integer(kind=c_int8_t), intent(out), optional :: i8value
            type(c_ptr), intent(out), optional :: pbvalue
        end subroutine hl_gtk_list_tree_get_gvalue

        real(rn) function mean(x)
            use ur_params, only: rn
            implicit none
            real(rn),intent(in)   :: x(:)
        end function mean

        real(rn) function sd(x)
            use ur_params, only: rn
            implicit none
            real(rn),intent(in)   :: x(:)
        end function sd

        real(rn) function pnorm(x, x0, sigma)
            use ur_params, only: rn
            implicit none
            real(rn),intent(in)           :: x
            real(rn),intent(in),optional  :: x0, sigma

        end function pnorm

        integer function findloct(carray,suchstr)
            use ur_gleich,     only: charv
            implicit none
            type(charv),intent(in)       :: carray(:)
            character(len=*),intent(in)  :: suchstr
        end function findloct

        function fltu(local_encoded_str) result(utf8_str)
            implicit none
            character(len=*),intent(in)  :: local_encoded_str
            character(:),allocatable     :: utf8_str
        end function fltu

        function flfu(utf8_str) result(local_encoded_str)
            implicit none
            character(len=*),intent(in)      :: utf8_str
            character(:),allocatable         :: local_encoded_str
        end function flfu

        real(rn) function chaver(y,nvals,k,location,posymax)
            use ur_params, only: rn
            implicit none
            integer, intent(in)   :: nvals
            real(rn),intent(in)     :: y(0:nvals)
            integer, intent(in)   :: k
            character(len=*),intent(in) :: location
            integer, intent(in),optional  :: posymax
        end function chaver

        subroutine plot3fig(knum,nkpts,ncurve,line_styles,line_widths,xlog,ylog,xlab,ylab,ptitle,pltfile, &
            mimax,mimay,ctextl)
            implicit none

            integer, intent(in)                 :: knum           ! number of curves
            integer, intent(in)                 :: nkpts(knum)    ! number of points per curve
            integer, intent(in)                 :: ncurve(knum)   ! numbers of curve shapes
            integer, intent(in)                 :: line_styles(knum)   ! numbers of curve shapes
            real(8),intent(in)                    :: line_widths(knum)   ! numbers of curve shapes
            real(8),intent(in),optional           :: mimax(2)      ! xminv,xmaxv
            real(8),intent(in),optional           :: mimay(2)      ! yminv,ymaxv
            logical,intent(in)                    :: xlog,ylog
            character(len=*),intent(in)           :: xlab,ylab
            character(len=*),intent(in),optional  :: ctextl(knum)
            character(len=*), intent(in)          :: ptitle
            character(len=*), intent(in)          :: pltfile
        end subroutine plot3fig

        subroutine plot2(knum,nkpts,ncurve,line_styles,line_widths,xlog,ylog,xlab,ylab,ptitle,pltfile, &
            mimax,mimay,ctextl)
            implicit none

            integer, intent(in)                 :: knum           ! number of curves
            integer, intent(in)                 :: nkpts(knum)    ! number of points per curve
            integer, intent(in)                 :: ncurve(knum)   ! numbers of curve shapes
            integer, intent(in)                 :: line_styles(knum)   ! numbers of curve shapes
            real(8),intent(in)                    :: line_widths(knum)   ! numbers of curve shapes
            real(8),intent(in),optional           :: mimax(2)      ! xminv,xmaxv
            real(8),intent(in),optional           :: mimay(2)      ! yminv,ymaxv
            logical,intent(in)                    :: xlog,ylog
            character(len=*),intent(in)           :: xlab,ylab
            character(len=*),intent(in),optional  :: ctextl(knum)
            character(len=*), intent(in)          :: ptitle
            character(len=*), intent(in)          :: pltfile
        end subroutine plot2

        subroutine realmoda1(array,n1,kmin)
            use ur_params, only: rn
            implicit none

            integer, intent(in)          :: n1
            integer, intent(in),optional :: kmin
            real(rn),allocatable           :: array(:),vvv(:)
        end subroutine realmoda1

        real(rn) function resulta(nn,nmax)
            use ur_params, only: rn
            implicit none

            integer, intent(in)           :: nn  ! nummer derjenigen gleichung, für die der ergebniswert berechnet wird.
            integer, intent(in),optional  :: nmax  ! nummer oberhalb nn, ab der gleichung soll die berechnung bis nn laufen
        end function resulta

        subroutine unitfind(strg1,jj,jpl,factor,strgout)
            use ur_params, only: rn
            implicit none

            character(len=*),intent(in)   :: strg1
            integer, intent(out)          :: jj         ! nummer des basiseinheit
            integer, intent(out)          :: jpl        ! nummer zu jj addiert
            real(rn),intent(out)          :: factor
            character(:),allocatable,intent(out)  :: strgout
        end subroutine unitfind

    end interface
end module ur_interfaces
