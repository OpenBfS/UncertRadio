!#######################################################################
module Sym1

   use UR_params,   only: rn

 interface

  !-----------------------------------------------------------------------
  module subroutine Symbol1()
  implicit none
  end subroutine Symbol1

  module subroutine PointNach(mfall)
  implicit none
  integer(4),INTENT(IN)    :: mfall    ! 1: called from Symbol1;   2: called from Rechw1
  end subroutine PointNach

  module subroutine Readj_knetto()
  end subroutine Readj_knetto

  module subroutine Readj_kbrutto()
  implicit none
  end subroutine Readj_kbrutto

  module subroutine RS_numbers
  end subroutine RS_numbers

 end interface

end module Sym1
