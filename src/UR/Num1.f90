!-------------------------------------------------------------------------------------------------!
! This file is part of UncertRadio.
!
!    UncertRadio is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    UncertRadio is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with UncertRadio. If not, see <http://www.gnu.org/licenses/>.
!
!-------------------------------------------------------------------------------------------------!

module Num1
  use UR_params,   only: rn, zero, one, two, three, half, eps1min

  implicit none

          ! funcs
          ! Xfit
          ! SearchBCI3
          ! dpi_funcs
          ! bipoi2_norm
          ! Norm_BiPoi2
          ! matwrite
          ! quick_sort_r
          ! quick_sort_i
          ! kaiser

  interface

     module subroutine funcs(ix, afunc)
       USE UR_Linft,            only: ma
       integer(4),INTENT(IN)     :: ix
       real(rn),INTENT(OUT)      :: afunc(ma)
       ! integer(4),INTENT(IN)     :: mam
     end subroutine funcs

     module subroutine findEq_afunc(ix,kEQnumber)
       USE UR_Gleich,           only: knumEGr,nab,nmodf
       USE UR_Linft,            only: ma,defineallxt,mfitfix,nchannels,numd,mac
       use Usub3,               only: FindMessk
       integer(4),INTENT(IN)     :: ix         ! number of the Xi= decay curve function
       integer(4),INTENT(OUT)    :: kEQnumber(ma) ! function values of associated with the ma fit parameters
     end subroutine findEQ_afunc

     module subroutine find_mac(mac)
       USE UR_Gleich,           only: knumEGr,nab,nmodf
       USE UR_Linft,            only: ma,defineallxt,mfitfix,nchannels,numd
       integer(4),INTENT(OUT)    :: mac
     end subroutine find_mac

     module SUBROUTINE Xfit (x, sigmax, npts, mode, xmean, sigmam, sigma)
       INTEGER(4), INTENT(IN)      :: npts
       real(rn), INTENT(IN)        :: x(npts)
       real(rn), INTENT(IN)        :: sigmax(npts)
       INTEGER(4), INTENT(IN)      :: mode
       real(rn), INTENT(OUT)       :: xmean
       real(rn), INTENT(OUT)       :: sigmam
       real(rn), INTENT(OUT)       :: sigma
     end SUBROUTINE Xfit

     module subroutine SearchBCI3(mode,imcmax,kqtyp)
       integer(4),intent(in)  :: mode     !  1: MC;  2:  MCMC-MH
       integer(4),intent(in)  :: imcmax    ! Länge des MC-Arrays
       integer(4),intent(in)  :: kqtyp    ! for:  1: output quantity; 2: DT;  3: DL
     end subroutine SearchBCI3

     module real(rn) function dpi_funcs(mwind,indeval,jp,ma,Fv1)
       integer(4),intent(in)    :: mwind      ! 'Messwert' index of the variable, with respect to which
                                              ! a partial derivative is calculated
       integer(4),intent(in)    :: indeval    ! number of the equation, of which the derivative is calculated
       integer(4),intent(in)    :: jp         ! index of afunc(), so that afunc(jp) = function value
       integer(4),intent(in)    :: ma         ! length of array afunc
       real(rn),intent(in)      :: Fv1        ! value of the unmodified function, supplied externally
     end function dpi_funcs

     module real(rn) function bipoi2_norm(xact)
       real(rn),intent(in)   :: xact
     end function bipoi2_norm

     module real(rn) function Norm_BiPoi2(xact)
       real(rn),intent(in)   :: xact
     end function Norm_BiPoi2

     module subroutine matwrite(xmat,mm,nn,kunit,frmt,ctext)
       ! integer(4),intent(in)   :: m,n        ! physical dims
       integer(4),intent(in)   :: mm,nn      ! dims to be printed
       real(rn),intent(in)     :: xmat(:,:)
       integer(4),intent(in)   :: kunit
       character(len=*),intent(in) :: frmt
       character(len=*),intent(in) :: ctext
     end subroutine matwrite

     ! RECURSIVE SUBROUTINE quick_sort(list, order)
     module RECURSIVE SUBROUTINE quick_sort_r(list,order)

       IMPLICIT NONE
       REAL(rn), DIMENSION (:), INTENT(IN OUT)  :: list
       INTEGER(4), DIMENSION (:), INTENT(OUT)  :: order
     end subroutine quick_sort_r

     module RECURSIVE SUBROUTINE quick_sort_i(list,order)

       IMPLICIT NONE
       integer(4), DIMENSION (:), INTENT(IN OUT)  :: list
       INTEGER(4), DIMENSION (:), INTENT(OUT)  :: order
     end subroutine quick_sort_i

     module SUBROUTINE kaiser(a, nrows, n, eigenv, trace, sume, ier)

       IMPLICIT NONE
       REAL (rn), INTENT(IN OUT) :: a(:,:)
       INTEGER, INTENT(IN)       :: nrows
       INTEGER, INTENT(IN)       :: n
       REAL (rn), INTENT(OUT)    :: eigenv(:)
       REAL (rn), INTENT(OUT)    :: trace
       REAL (rn), INTENT(OUT)    :: sume
       INTEGER, INTENT(OUT)      :: ier
     end subroutine kaiser

     module RECURSIVE SUBROUTINE quick_sort2_i(list,order)

       IMPLICIT NONE
       integer(4), DIMENSION (:), INTENT(IN)  :: list
       INTEGER(4), DIMENSION (:), INTENT(OUT)  :: order
     end subroutine quick_sort2_i


  end interface

end module Num1

