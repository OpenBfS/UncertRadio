module plgetc

  ! USE, INTRINSIC :: ISO_C_BINDING,   only:
  ! USE ISO_C_BINDING
  IMPLICIT NONE

  ! Define interface of C function.
  INTERFACE
    ! INTEGER(KIND=C_INT) FUNCTION plGetCursorX(kbutton,px,py,dx,dy,wx,wy) BIND(C)
    ! Subroutine plGetCursorX(kbutton1,px1,py1,dx1,dy1,wx1,wy1) BIND(C)
    ! Type(c_ptr) function plGetCursorX() BIND(C)
    Integer(c_int) function plGetCursorX() BIND(C)
      USE ISO_C_BINDING, only:  c_ptr,c_int
      ! USE ISO_C_BINDING

      ! use  plplot_types, only: PLGraphicsIn
     ! use URgtk_window,          only: ginX

     ! type(ginX),intent(out)    ::  mouse_gin

    !integer(c_int)        :: kbutton1,px1,py1
    !real(c_double)        :: dx1,dy1,wx1,wy1
         ! TYPE(C_FUNPTR), INTENT(IN), VALUE :: func
         ! type(plpl), INTENT(out)  :: gin
      ! type(PLGraphicsIn),intent(inout)        :: gin
    END Function plGetCursorX
  END INTERFACE

CONTAINS


  ! Call C function.
  SUBROUTINE plget (kbutton,px,py,dx,dy,wx,wy)

    USE ISO_C_BINDING,  only: c_ptr,c_int,c_double,c_funloc,c_loc,c_associated
    ! type(c_ptr),intent(out)    :: gin
    !use plplot_types,    only : PLGraphicsIn
    !type(PLGraphicsIn),intent(inout)    :: gin

    use URgtk_window,          only: ginX

    implicit none

    type(ginX),target       ::  mouse_gin

     integer        :: kbutton,px,py
     real(8)        :: dx,dy,wx,wy
     integer        :: jj

     integer(c_int)        :: kbutton1,px1,py1
     real(c_double)        :: dx1,dy1,wx1,wy1

    ! integer(c_int)        :: res,plGetCursorX
    type(c_ptr)           ::  cgin
    ! type(c_ptr), target   ::  plGetCursorX
    integer(c_int)    :: plGetCursorX
    ! call plGetCursorX(kbutton1,px1,py1,dx1,dy1,wx1,wy1)
    ! cgin = plGetCursorX
    jj = plGetCursorX

     ! if(c_associated(cgin))
      if(jj /= 0)  write(66,*) 'PLget:  jj=',jj                ! mouse_gin=',mouse_gin

    kbutton = kbutton1
    dx = dx1
    dy = dy1
    px = px1
    py = py1
    wx = wx1
    wy = wy1

  END SUBROUTINE plget


 END MODULE plgetc
