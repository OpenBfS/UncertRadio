!-------------------------------------------------------------------------------------------------!
! this file is part of uncertradio.
!
!    uncertradio is free software: you can redistribute it and/or modify
!    it under the terms of the gnu general public license as published by
!    the free software foundation, either version 3 of the license, or
!    (at your option) any later version.
!
!    uncertradio is distributed in the hope that it will be useful,
!    but without any warranty; without even the implied warranty of
!    merchantability or fitness for a particular purpose.  see the
!    gnu general public license for more details.
!
!    you should have received a copy of the gnu general public license
!    along with uncertradio. if not, see <http://www.gnu.org/licenses/>.
!
!-------------------------------------------------------------------------------------------------!

module num1
  use ur_params,   only: rn, eps1min

  implicit none

          ! funcs
          ! xfit
          ! searchbci3
          ! dpi_funcs
          ! bipoi2_norm
          ! norm_bipoi2
          ! matwrite
          ! quick_sort_r
          ! quick_sort_i
          ! kaiser

  interface

     module subroutine funcs(ix, afunc)
       use ur_linft,            only: ma
       integer(4),intent(in)     :: ix
       real(rn),intent(out)      :: afunc(ma)
       ! integer(4),intent(in)     :: mam
     end subroutine funcs

     module subroutine findeq_afunc(ix,keqnumber)
       use ur_gleich,           only: knumegr,nab,nmodf
       use ur_linft,            only: ma,defineallxt,mfitfix,nchannels,numd,mac
       use usub3,               only: findmessk
       integer(4),intent(in)     :: ix         ! number of the xi= decay curve function
       integer(4),intent(out)    :: keqnumber(ma) ! function values of associated with the ma fit parameters
     end subroutine findeq_afunc

     module subroutine find_mac(mac)
       use ur_gleich,           only: knumegr,nab,nmodf
       use ur_linft,            only: ma,defineallxt,mfitfix,nchannels,numd
       integer(4),intent(out)    :: mac
     end subroutine find_mac

     module subroutine xfit (x, sigmax, npts, mode, xmean, sigmam, sigma)
       integer(4), intent(in)      :: npts
       real(rn), intent(in)        :: x(npts)
       real(rn), intent(in)        :: sigmax(npts)
       integer(4), intent(in)      :: mode
       real(rn), intent(out)       :: xmean
       real(rn), intent(out)       :: sigmam
       real(rn), intent(out)       :: sigma
     end subroutine xfit

     module subroutine searchbci3(mode,imcmax,kqtyp)
       integer(4),intent(in)  :: mode     !  1: mc;  2:  mcmc-mh
       integer(4),intent(in)  :: imcmax    ! länge des mc-arrays
       integer(4),intent(in)  :: kqtyp    ! for:  1: output quantity; 2: dt;  3: dl
     end subroutine searchbci3

     module real(rn) function dpi_funcs(mwind,indeval,jp,ma,fv1)
       integer(4),intent(in)    :: mwind      ! 'messwert' index of the variable, with respect to which
                                              ! a partial derivative is calculated
       integer(4),intent(in)    :: indeval    ! number of the equation, of which the derivative is calculated
       integer(4),intent(in)    :: jp         ! index of afunc(), so that afunc(jp) = function value
       integer(4),intent(in)    :: ma         ! length of array afunc
       real(rn),intent(in)      :: fv1        ! value of the unmodified function, supplied externally
     end function dpi_funcs

     module real(rn) function bipoi2_norm(xact)
       real(rn),intent(in)   :: xact
     end function bipoi2_norm

     module real(rn) function norm_bipoi2(xact)
       real(rn),intent(in)   :: xact
     end function norm_bipoi2

     module subroutine matwrite(xmat,mm,nn,kunit,frmt,ctext)
       ! integer(4),intent(in)   :: m,n        ! physical dims
       integer(4),intent(in)   :: mm,nn      ! dims to be printed
       real(rn),intent(in)     :: xmat(:,:)
       integer(4),intent(in)   :: kunit
       character(len=*),intent(in) :: frmt
       character(len=*),intent(in) :: ctext
     end subroutine matwrite

     ! recursive subroutine quick_sort(list, order)
     module recursive subroutine quick_sort_r(list,order)

       implicit none
       real(rn), dimension (:), intent(in out)  :: list
       integer(4), dimension (:), intent(out)  :: order
     end subroutine quick_sort_r

     module recursive subroutine quick_sort_i(list,order)

       implicit none
       integer(4), dimension (:), intent(in out)  :: list
       integer(4), dimension (:), intent(out)  :: order
     end subroutine quick_sort_i

     module subroutine kaiser(a, nrows, n, eigenv, trace, sume, ier)

       implicit none
       real (rn), intent(in out) :: a(:,:)
       integer, intent(in)       :: nrows
       integer, intent(in)       :: n
       real (rn), intent(out)    :: eigenv(:)
       real (rn), intent(out)    :: trace
       real (rn), intent(out)    :: sume
       integer, intent(out)      :: ier
     end subroutine kaiser

     module recursive subroutine quick_sort2_i(list,order)

       implicit none
       integer(4), dimension (:), intent(in)  :: list
       integer(4), dimension (:), intent(out)  :: order
     end subroutine quick_sort2_i


  end interface

end module num1
