Module UR_interfaces

    ! use CHF


    implicit none

    INTERFACE

        subroutine Rechw1()
            implicit none
        end subroutine Rechw1

        subroutine FindItemS(dialogstr, ncitem)
            !use UR_gtk_variables,  only: clobj,nclobj
            implicit none
            character(len=*),intent(in)   :: dialogstr
            integer, intent(out)           :: ncitem
        end subroutine FindItemS

        subroutine FindItemP(ptr, ncitem)
            use, intrinsic :: iso_c_binding,     only: c_ptr,c_null_ptr,c_associated
            !use UR_gtk_variables,  only: clobj,nclobj
            implicit none
            type(c_ptr),value,intent(in)  :: ptr
            integer, intent(out)           :: ncitem
        end subroutine FindItemP

        subroutine gchar (var)
            implicit none
            character*(*), intent(inout)    :: var
        end subroutine gchar

        subroutine Strgencode(keystrg,textin,textout, mode, isatz)
            implicit none
            character(len=*), intent(in)     :: keystrg
            character(len=*), intent(in)     :: textin
            character(len=*), intent(out)    :: textout
            integer, intent(in)               :: mode
            integer, intent(in)               :: isatz
        end subroutine Strgencode

        subroutine PrintGladeSys(kunit)
            implicit none
            integer, intent(in)  :: kunit
        end subroutine PrintGladeSys

        subroutine Loadsel_diag_new(mode, ncitem)
            implicit none
            integer, intent(in)                  :: mode             ! 1: show dialog;  2: readout dialog and hide it
            integer, intent(in)                  :: ncitem
        end subroutine Loadsel_diag_new

        subroutine DisplayHelp(ncitem,idstr)
            implicit none
            integer, intent(in)            :: ncitem
            character(len=*),optional,intent(in)  :: idstr
        end subroutine DisplayHelp

        recursive subroutine ProcessLoadPro_new(iwahl,kEGRneu)
            implicit none
            integer, INTENT(IN)            :: iwahl
            integer, INTENT(IN),optional   :: kEGrneu
        end subroutine ProcessLoadPro_new

        subroutine ProcMainDiag(ncitem)
            implicit none
            integer, intent(in)            :: ncitem
        end subroutine ProcMainDiag

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
            use UR_params,          only: rn
            implicit none
            real(rn),intent(in)   :: x(:)
        end function mean

        real(rn) function sd(x)
            use UR_params,          only: rn
            implicit none
            real(rn),intent(in)   :: x(:)
        end function sd

        real(rn) function pnorm(x, x0, sigma)
            use UR_params,          only: rn
            implicit none
            real(rn),intent(in)           :: x
            real(rn),intent(in),optional  :: x0, sigma

        end function pnorm

        integer function FindlocT(carray,suchstr)
            use UR_Gleich,     only: charv
            implicit none
            type(charv),intent(in)       :: carray(:)
            character(len=*),intent(in)  :: suchstr
        end function FindlocT

        function FLTU(string)
            use, intrinsic :: iso_c_binding,       only: c_ptr,c_null_ptr,c_null_char,c_associated
            use gtk_sup,             only: c_f_string
            use g,                   only: g_locale_to_utf8
            implicit none
            character(len=*),intent(in)         :: string
            character(:),allocatable            :: FLTU
        end function FLTU

        function FLFU(string)
            use, intrinsic :: iso_c_binding,       only: c_ptr,c_null_ptr,c_null_char,c_associated
            use gtk_sup,             only: c_f_string
            use g,                   only: g_locale_from_utf8
            implicit none
            character(len=*),intent(inout)      :: string
            character(:),allocatable            :: FLFU
        end function FLFU

        real(rn) function Chaver(y,nvals,k,location,posYmax)
            use UR_params,          only: rn
            implicit none
            integer, intent(in)   :: nvals
            real(rn),intent(in)     :: y(0:nvals)
            integer, intent(in)   :: k
            character(len=*),intent(in) :: location
            integer, intent(in),optional  :: posYmax
        end function Chaver

        subroutine plot3fig(knum,nkpts,ncurve,line_styles,line_widths,xlog,ylog,xlab,ylab,ptitle,pltfile, &
            mimax,mimay,ctextL)
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
            character(len=*),intent(in),optional  :: ctextL(knum)
            character(len=*), intent(in)          :: PTitle
            character(len=*), intent(in)          :: pltfile
        end subroutine plot3fig

        subroutine plot2(knum,nkpts,ncurve,line_styles,line_widths,xlog,ylog,xlab,ylab,ptitle,pltfile, &
            mimax,mimay,ctextL)
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
            character(len=*),intent(in),optional  :: ctextL(knum)
            character(len=*), intent(in)          :: PTitle
            character(len=*), intent(in)          :: pltfile
        end subroutine plot2

        subroutine RealModA1(array,n1,kmin)
            use UR_params,          only: rn
            implicit none

            integer, intent(in)          :: n1
            integer, intent(in),optional :: kmin
            real(rn),allocatable           :: array(:),vvv(:)
        end subroutine RealModA1

        real(rn) Function Resulta(nn,nmax)
            USE UR_params,      ONLY: rn
            implicit none

            integer, INTENT(IN)           :: nn  ! Nummer derjenigen Gleichung, für die der Ergebniswert berechnet wird.
            integer, INTENT(IN),optional  :: nmax  ! Nummer oberhalb nn, ab der Gleichung soll die Berechnung bis nn laufen
        end function Resulta

        subroutine UnitFind(strg1,jj,jpl,factor,strgout)
            use UR_params,          only: rn
            use UR_Gleich,          only: UU,nu_other,unit_other,unit_basis
            use CHF,                only: ucase
            implicit none

            character(len=*),intent(in)   :: strg1
            integer, intent(out)        :: jj         ! nummer des Basiseinheit
            integer, intent(out)        :: jpl        ! nummer zu jj addiert
            real(rn),intent(out)          :: factor
            character(:),allocatable,intent(out)  :: strgout
        end subroutine UnitFind

    END INTERFACE
END MODULE UR_INTERFACES
