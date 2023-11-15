!#######################################################################

module Rw1

use UR_params,     only: rn

   interface

 !#######################################################################

  module Subroutine Rechw1()
  end subroutine Rechw1

  module subroutine covppcalc(mode)
  implicit none
  integer(4),intent(in)   :: mode        ! 1: called from Rechw1 or Rechw2;
                                         ! 2: called from MCcalc;
                                         ! 3: Test, on
  end subroutine covppcalc


  module subroutine LinCalib()
  end subroutine LinCalib


  module real(rn) function uval(symb)
  character(len=*),intent(in)  :: symb
  end function uval


  module recursive subroutine FindWparsR(kstart,klu)
  integer(4),intent(in)     :: kstart       ! index of start equation
  integer(4),intent(in)     :: klu          ! definition klu: see (begin of) upropa
  end subroutine FindWparsR

  module subroutine Find_lambda()
  implicit none
  end subroutine Find_lambda


  !########################################################################

 end interface

end module Rw1
