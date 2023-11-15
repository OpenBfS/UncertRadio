!#########################################################################

module UR_gtk_window

     !     Copyright (C) 2014-2023  Günter Kanisch

   ! use gui_functions,   only: window
    use, intrinsic :: iso_c_binding, only: c_ptr,c_double,c_int,c_char
    use gtk_sup
    use g

    implicit none

    type window
        ! private
        type(c_ptr) :: window_ptr
    end type

   integer(4), parameter             :: nclmax = 1250

   !   see: https://fortranwiki.org/fortran/files/character_handling_in_Fortran.html,
   !        section 10. Character arrays
   type :: charv                   ! ca. May 2020
     character(:),allocatable  :: s
   end type charv

   type Wclobj
     type(charv),allocatable  :: name(:)
     type(charv),allocatable  :: idd(:)
     type(charv),allocatable  :: label(:)
     type(c_ptr),allocatable  :: id_ptr(:)
     type(c_ptr),allocatable  :: label_ptr(:)
     type(charv),allocatable  :: signal(:)
     integer(4),allocatable   :: idparent(:)
     type(charv),allocatable  :: handler(:)
   end type


   type, bind(c)   :: GdkRGBA
      real(c_double) :: red   = 0.1_c_double
      real(c_double) :: green = 0.1_c_double
      real(c_double) :: blue  = 0.2_c_double
      real(c_double) :: alpha = 1.0_c_double
   end type

   type GErrorF
     integer(4)         :: fdomain
     integer(4)         :: fcode
     character(len=300) :: fmessage
   end type

   type KSetting
     type(c_ptr)          :: GtkSetDef
     integer(4)           :: nprops
     character(len=60)    :: sproperty(20)
     character(len=250)   :: sproperty_val(20)
   end type KSetting

   type, bind(c)      :: TreeIterF
     integer(c_int)       :: stamp
     type(c_ptr)          :: user_data
     type(c_ptr)          :: user_data2
     type(c_ptr)          :: user_data3
   end type TreeIterF

   type, bind(c)      :: ginX
     integer(c_int)       :: type
     integer(c_int)       :: state
     integer(c_int)       :: keysym
     integer(c_int)       :: subwindow
     character(c_char)    :: string
     real(c_double)       :: pX,pY
     real(c_double)       :: dX,dY
     real(c_double)       :: wX,wY
   end type ginX

end module UR_gtk_window

!#########################################################################
