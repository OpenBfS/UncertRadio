
Module UR_interfaces

    ! use CHF

IMPLICIT NONE
  INTERFACE

    !SUBROUTINE FieldUpdate()
    !  IMPLICIT NONE
    !END SUBROUTINE FieldUpdate


    subroutine Rechw1()
      implicit none
      !!!! integer(4),INTENT(IN),optional   :: identw
    end subroutine Rechw1

    !SUBROUTINE mtxchi(a,nc,n)
    !  implicit none
    !  integer(4), intent(in)                :: nc     ! physikalische Dimension
    !  integer(4), INTENT(IN)                :: n
    !  REAL(8), INTENT(INOUT)             :: a(nc,nc)
    !  ! REAL(8), INTENT(INOUT)             :: u(nc,nc)
    !end subroutine mtxchi

    !real(8) Function gevalf (i, mw)
    !  IMPLICIT NONE
    !  integer(4),INTENT(in)               :: i             ! Function identifier
    !  REAL(8),INTENT(in),dimension(:)  :: mw            ! Variable values
    !end function gevalf

   subroutine FindItemS(dialogstr, ncitem)
     !use UR_gtk_variables,  only: clobj,nclobj
     implicit none
     character(len=*),intent(in)   :: dialogstr
     integer(4),intent(out)           :: ncitem
   end subroutine FindItemS

   subroutine FindItemP(ptr, ncitem)
     use, intrinsic :: iso_c_binding,     only: c_ptr,c_null_ptr,c_associated
     !use UR_gtk_variables,  only: clobj,nclobj
     implicit none
     type(c_ptr),value,intent(in)  :: ptr
     integer(4),intent(out)           :: ncitem
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
    integer(4),intent(in)               :: mode
    integer(4),intent(in)               :: isatz
  end subroutine Strgencode

  subroutine PrintGladeSys(kunit)
    implicit none
    integer(4),intent(in)  :: kunit
  end subroutine PrintGladeSys

  subroutine Loadsel_diag_new(mode, ncitem)
    implicit none
    integer(4),intent(in)                  :: mode             ! 1: show dialog;  2: readout dialog and hide it
    integer(4),intent(in)                  :: ncitem
  end subroutine Loadsel_diag_new

  subroutine DisplayHelp(ncitem,idstr)
    implicit none
    integer(4),intent(in)            :: ncitem
    character(len=*),optional,intent(in)  :: idstr
  end subroutine DisplayHelp

  recursive subroutine ProcessLoadPro_new(iwahl,kEGRneu)
    implicit none
    integer(4),INTENT(IN)            :: iwahl
    integer(4),INTENT(IN),optional   :: kEGrneu
  end subroutine ProcessLoadPro_new

  subroutine ProcMainDiag(ncitem)
    implicit none
    integer(4),intent(in)            :: ncitem
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
           & svalue, fvalue, dvalue, ivalue, lvalue, l64value, logvalue, &
           & i8value, pbvalue)
     use, intrinsic :: iso_c_binding
     implicit none
     integer(4), parameter :: type_kind=c_long
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
    use UR_params,                only: rn,eps1min
    implicit none
    real(rn),intent(in)   :: x(:)
    !     integer,intent(in)   :: n
  end function mean

  real(rn) function sd(x)
    use UR_params,                only: rn,eps1min
    implicit none
    real(rn),intent(in)   :: x(:)
    !     integer,intent(in)   :: n
  end function sd

  !subroutine MessageShow(message,button_set,title,resp,mtype)
  !  use, intrinsic :: iso_c_binding,      only: c_int, c_null_char
  !  implicit none
  !  character(len=*),intent(in)         :: message
  !  integer(c_int),intent(in)           :: button_set
  !  character(len=*),intent(in)         :: title
  !  integer(c_int),intent(out)          :: resp
  !  integer(c_int),intent(in),optional  :: mtype
  !end subroutine MessageShow

  real(rn) function pnorm(x, x0, sigma)
    use UR_params,     only: rn
    implicit none
    real(rn),intent(in)           :: x
    real(rn),intent(in),optional  :: x0, sigma

  end function pnorm

  integer(4) function FindlocT(carray,suchstr)
    use UR_Gleich,     only: charv
    implicit none
    type(charv),intent(in)       :: carray(:)
    character(len=*),intent(in)  :: suchstr
  end function FindlocT

  function FLTU(string)
    ! function FLTU(string) result(Fstr)
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

  !integer(4) function FindlocT(carray,suchstr,imin)
  !  use UR_Gleich,     only: charv
  !  type(charv), allocatable        :: carray(:)
  !  character(len=*),intent(in)     :: suchstr
  !  integer(4),intent(in),optional  :: imin     ! ab dem element suchen
  !end function FindLocT

  real(rn) function Chaver(y,nvals,k,location,posYmax)
    use UR_params,     only: rn
    implicit none
    integer(4),intent(in)   :: nvals
    real(rn),intent(in)     :: y(0:nvals)
    integer(4),intent(in)   :: k
    character(len=*),intent(in) :: location
    integer(4),intent(in),optional  :: posYmax
  end function Chaver

  subroutine plot3fig(knum,nkpts,ncurve,line_styles,line_widths,xlog,ylog,xlab,ylab,ptitle,pltfile, &
                                                 mimax,mimay,ctextL)
    implicit none

    integer(4),intent(in)                 :: knum           ! number of curves
    integer(4),intent(in)                 :: nkpts(knum)    ! number of points per curve
    integer(4),intent(in)                 :: ncurve(knum)   ! numbers of curve shapes
    integer(4),intent(in)                 :: line_styles(knum)   ! numbers of curve shapes
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

    integer(4),intent(in)                 :: knum           ! number of curves
    integer(4),intent(in)                 :: nkpts(knum)    ! number of points per curve
    integer(4),intent(in)                 :: ncurve(knum)   ! numbers of curve shapes
    integer(4),intent(in)                 :: line_styles(knum)   ! numbers of curve shapes
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
    use UR_params,   only: rn
    implicit none

    integer(4),intent(in)          :: n1
    integer(4),intent(in),optional :: kmin
    real(rn),allocatable           :: array(:),vvv(:)
  end subroutine RealModA1

  real(rn) Function Resulta(nn,nmax)
    USE UR_params,      ONLY: rn,eps1min,zero,one
    implicit none

    integer(4),INTENT(IN)           :: nn  ! Nummer derjenigen Gleichung, für die der Ergebniswert berechnet wird.
    integer(4),INTENT(IN),optional  :: nmax  ! Nummer oberhalb nn, ab der Gleichung soll die Berechnung bis nn laufen
  end function Resulta

  subroutine UnitFind(strg1,jj,jpl,factor,strgout)
    use UR_params,          only: rn
    use UR_Gleich,          only: UU,nu_other,unit_other,unit_basis
    use CHF,                only: ucase
    implicit none

    character(len=*),intent(in)   :: strg1
    integer(4),intent(out)        :: jj         ! nummer des Basiseinheit
    integer(4),intent(out)        :: jpl        ! nummer zu jj addiert
    real(rn),intent(out)          :: factor
    character(:),allocatable,intent(out)  :: strgout
  end subroutine UnitFind

  !subroutine PlotData(prout,ndata,allow_smooth,YmaxFraction)
  !  use UR_params,           only: rn,three,four,zero,one,two,eps1min
  !  use UR_Rmcmc
  !  Use UR_Variables,        only: print_graph,langg,gum_restricted,sListSeparator,actpath, &
  !                                 sDecimalPoint,sec_strm,gtk_strm
  !  use UR_Gleich,           only: kEGr,Symbole,Einheit,ifehl,use_bipoi,N_preset
  !  use UR_kpp,              only: use_Nbin,transform_GMA
  !  ! use UR_interfaces,       only: Chaver
  !  implicit none
  !  logical, intent(in)         :: prout,allow_smooth
  !  real(rn),intent(in)         :: ymax,YmaxFraction
  !  ! real(rn),intent(in)         :: x(:),y(:)
  !  integer(4),intent(in)       :: ndata
  !  integer(4),intent(inout)    :: kmin,kmax
  !  integer(4),intent(out)      :: lmin,lmax
  !end subroutine PlotData

  END INTERFACE
END MODULE UR_INTERFACES
