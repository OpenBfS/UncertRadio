!#######################################################################

module Rw2

   use UR_params,    only: rn

 interface


   module subroutine Rechw2()
   end subroutine Rechw2
   
   
   module subroutine detlim_iter(DTxx,newvalue,it)
   real(rn), INTENT(IN)        :: DTxx      ! needed only for limit_typ = 2 (DL iteration)
   real(rn), INTENT(OUT)       :: newvalue  ! calculated value of DT or DL
   integer(4), INTENT(OUT)     :: it        ! number of iterations
   end subroutine detlim_iter
   
   
   module subroutine setupParser(iopt)
   integer(4),intent(in) :: iopt     ! for control output: (0 (none) or 1 (with))
   end subroutine setupParser
   
   
   module real(rn) function RnetVal(xAct)
   real(rn),intent(in)    :: xAct
   end function RnetVal
   
   
   module real(rn) function ActVal(Rnet)
   real(rn),intent(in)  :: Rnet
   end function ActVal

 end interface

end module Rw2
