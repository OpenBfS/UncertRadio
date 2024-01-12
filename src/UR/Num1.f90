

module Num1
  use UR_params,   only: rn

  implicit none

          ! funcs
          ! Xfit
          ! SearchBCI3
          ! dpi_funcs
          ! bipoi2_norm
          ! Norm_BiPoi2
          ! bipoi2_int_rb
          ! matwrite
          ! quick_sort_r
          ! quick_sort_i
          ! kaiser

  interface

     !module function point_dist(a, b) result(distance)
     !  type(point), intent(in) :: a, b
     !  real :: distance
     !end function point_dist

     module subroutine funcs(ix,afunc)    !  ,mam)
       USE UR_Linft,            only: ma
       integer(4),INTENT(IN)     :: ix
       ! real(rn),INTENT(IN)        :: x
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

     module subroutine bipoi2_int_rb(y,Ns,p,tg,tb,Nb, Ptest,jmax,i1,i2)
       real(rn),intent(in)    :: y,Ns,p,tg,Tb,Nb
       real(rn),intent(out)   :: Ptest
       integer(4),intent(out) :: jmax, i1,i2
     end subroutine bipoi2_int_rb

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
       use UR_params,    only: rn
       IMPLICIT NONE
       REAL(rn), DIMENSION (:), INTENT(IN OUT)  :: list
       INTEGER(4), DIMENSION (:), INTENT(OUT)  :: order
     end subroutine quick_sort_r

     module RECURSIVE SUBROUTINE quick_sort_i(list,order)
       use UR_params,    only: rn
       IMPLICIT NONE
       integer(4), DIMENSION (:), INTENT(IN OUT)  :: list
       INTEGER(4), DIMENSION (:), INTENT(OUT)  :: order
     end subroutine quick_sort_i

     module SUBROUTINE kaiser(a, nrows, n, eigenv, trace, sume, ier)
       IMPLICIT NONE
       integer(4),parameter   :: dp = 10
       REAL (dp), INTENT(IN OUT) :: a(:,:)
       INTEGER, INTENT(IN)       :: nrows
       INTEGER, INTENT(IN)       :: n
       REAL (dp), INTENT(OUT)    :: eigenv(:)
       REAL (dp), INTENT(OUT)    :: trace
       REAL (dp), INTENT(OUT)    :: sume
       INTEGER, INTENT(OUT)      :: ier
     end subroutine kaiser

     module RECURSIVE SUBROUTINE quick_sort2_i(list,order)
       use UR_params,    only: rn
       IMPLICIT NONE
       integer(4), DIMENSION (:), INTENT(IN)  :: list
       INTEGER(4), DIMENSION (:), INTENT(OUT)  :: order
     end subroutine quick_sort2_i


  end interface

end module Num1

