!--------------------------------------------------------------------------------------------------!
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
!--------------------------------------------------------------------------------------------------!

module num1
    use UR_types, only: rn
    use ur_params, only: EPS1MIN

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

        module real(rn) function median(x, n)
            integer, intent(in)   :: n
            real(rn), intent(in)  :: x(n)
        end function median

        module subroutine funcs(ix, afunc)
            use ur_linft,        only: ma
            integer   ,intent(in)     :: ix
            real(rn),intent(out)      :: afunc(ma)
        end subroutine funcs

        module subroutine findeq_afunc(ix, keqnumber)
            use ur_linft,        only: ma

            integer   ,intent(in)     :: ix         ! number of the xi= decay curve function
            integer   ,intent(out)    :: keqnumber(ma) ! function values of associated with the ma fit parameters
        end subroutine findeq_afunc

        module subroutine find_mac(mac)
            integer, intent(out)    :: mac
        end subroutine find_mac

        module subroutine xfit(x, sigmax, npts, mode, xmean, sigmam, sigma)
            integer, intent(in)      :: npts
            real(rn), intent(in)     :: x(npts)
            real(rn), intent(in)     :: sigmax(npts)
            integer, intent(in)      :: mode
            real(rn), intent(out)    :: xmean
            real(rn), intent(out)    :: sigmam
            real(rn), intent(out)    :: sigma
        end subroutine xfit

        module subroutine searchbci3(mode,imcmax,kqtyp)
            integer   ,intent(in)  :: mode     !  1: mc;  2:  mcmc-mh
            integer   ,intent(in)  :: imcmax    ! länge des mc-arrays
            integer   ,intent(in)  :: kqtyp    ! for:  1: output quantity; 2: dt;  3: dl
        end subroutine searchbci3

        module real(rn) function dpi_funcs(mwind,indeval,jp,ma,fv1)
            integer   ,intent(in)    :: mwind      ! 'messwert' index of the variable, with respect to which
                                                   ! a partial derivative is calculated
            integer   ,intent(in)    :: indeval    ! number of the equation, of which the derivative is calculated
            integer   ,intent(in)    :: jp         ! index of afunc(), so that afunc(jp) = function value
            integer   ,intent(in)    :: ma         ! length of array afunc
            real(rn),intent(in)      :: fv1        ! value of the unmodified function, supplied externally
        end function dpi_funcs

        module real(rn) function bipoi2_norm(xact)
            real(rn),intent(in)   :: xact
        end function bipoi2_norm

        module real(rn) function norm_bipoi2(xact)
            real(rn),intent(in)   :: xact
        end function norm_bipoi2

        module subroutine matwrite(xmat,mm,nn,kunit,frmt,ctext)
            ! integer   ,intent(in)   :: m,n        ! physical dims
            integer   ,intent(in)   :: mm,nn      ! dims to be printed
            real(rn),intent(in)     :: xmat(:,:)
            integer   ,intent(in)   :: kunit
            character(len=*),intent(in) :: frmt
            character(len=*),intent(in) :: ctext
        end subroutine matwrite

        ! recursive subroutine quick_sort(list, order)
        module subroutine quick_sort_r(list)
            implicit none
            real(rn), dimension (:), intent(in out)  :: list
        end subroutine quick_sort_r

        module recursive subroutine quick_sort_i(list, order)
            implicit none
            integer, dimension (:), intent(in out) :: list
            integer, dimension (:), intent(out)    :: order
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

        module subroutine sym_eigensolve(n, a, lda, eigenv, ier)
            implicit none
            integer, intent(in)       :: n
            real (rn), intent(in out) :: a(n,n)
            integer, intent(in)       :: lda
            real (rn), intent(out)    :: eigenv(n)
            integer, intent(out)      :: ier
        end subroutine

        module recursive subroutine quick_sort2_i(list,order)

        implicit none
            integer   , dimension (:), intent(in)  :: list
            integer   , dimension (:), intent(out)  :: order
        end subroutine quick_sort2_i

  end interface

end module num1
