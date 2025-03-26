recursive subroutine FindSymb(kstart,ksearch,found, kfound)

use UR_Gleich_globals,   only: nab,nRSsy,RS_SymbolNr

! Tests whether the dependent symbol with number kstart (<= nab) depends on the symbol
! with number ksearch. If so, found = .true. , otherwise found = .false.

!   Copyright (C) 2018-2023  Günter Kanisch

implicit none

integer(4),intent(in)     :: kstart      ! number of the dependent symbol = equation number (<=nab)
integer(4),intent(in)     :: ksearch     ! number of symbol, which is tested to be used in eq. kstart
logical, intent(out)      :: found       ! true, if this dependency is found
integer(4),intent(out)    :: kfound      ! is only another control variable

integer             :: i, k1

found = .false.
kfound = 0
if(kstart > nab) return

if(nRSsy(kstart) > 0) then
  do i=1,nRSsy(kstart)
    k1 = 0
    k1 = RS_SymbolNr(kstart,i)
    if(k1 == ksearch) then
      kfound = kstart
      if(kfound > 0) found = .true.
      return
    end if
    call FindSymb(k1,ksearch,found,kfound)
    if(found) return
  end do
end if
if(kfound > 0) found = .true.
return

end subroutine FindSymb
