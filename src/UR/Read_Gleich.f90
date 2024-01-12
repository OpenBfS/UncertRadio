
module RG

 interface


  !#######################################################################

  module recursive subroutine Read_Gleich()
  end subroutine Read_Gleich

  module subroutine EditFormelt(nglp,nglf,prout)
  implicit none
  integer(4),intent(inout)   :: nglp,nglf
  logical, intent(in)        :: prout   ! with test output or not
  end subroutine EditFormelt

  module subroutine modify_Formeltext(mode)
  use UR_Gleich,     only: nglp,nglp_read,eqnum_val,Formeltext
  implicit none
  integer(4),intent(in)   :: mode          ! 1: remove blank lines;  2: insert original blank lines
  end subroutine modify_Formeltext

  !###########################################################################################

 end interface

end module RG
