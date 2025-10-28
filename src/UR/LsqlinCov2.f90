
module LLcov2

    use UR_types, only: rn
    use UR_params, only: ZERO,ONE,TWO, EPS1MIN
    implicit none

    interface

        module subroutine LinCov2(n,nred,x1,sx1,yvar,covL,maL,R,ok,ifehl)

            !
            !  The routine prepares for the call to LsqLinCov2 for the weighted
            !  least-squares analysis. It is called from Linf() with maL=ma=3.
            !  It especially prepares the covariance matrix covyLF.
            !  Lincov2 returns the vector yvar of fitting parameters and the associated
            !  covariance matrix covL to Linf().

            implicit none

            integer, intent(in)  :: n             ! number of measured x values
            integer, intent(in)  :: nred          ! Anzahl der tatsächlich zu fittenden Parameter
            real(rn), intent(in) :: x1(n)         ! vector of x values
            real(rn), intent(in) :: sx1(n)        ! vector of standard deviations of the x values
            integer, intent(in)  :: maL           ! number of all fit parameters, including those which are
            ! not to be fitted
            real(rn), intent(out) :: yvar(maL)     ! vector of fitting parameters
            real(rn), intent(out) :: covL(maL,maL) ! covariance matrix of all fitting parameters, including
            ! also those not to be fitted
            real(rn), intent(out) :: R             ! value of the minimum function (chisq)
            logical, intent(out)  :: ok
            integer, intent(out)  :: ifehl         ! error variable;  0: nor error;  1: an error occurred
        end subroutine LinCov2

        module subroutine LsqLinCov2(x,covy1,n,nr,y,Uy,r,a,ok,maL,ifehl)

            implicit none

            integer   , intent(in) :: n             ! number of measured values
            real(rn), intent(in)   :: x(n)          ! vector of independent input values (x values)
            real(rn), intent(in)   :: covy1(n,n)    ! covariance matrix of the x values
            integer   , intent(in) :: nr            ! number of fitted output quantities (dependent unknowns)
            real(rn), intent(out)  :: y(nr)         ! vector of the values of the output quantities
            real(rn), intent(out)  :: Uy(nr,nr)     ! covariance matrix dof the output quantities
            real(rn), intent(out)  :: r             ! value of the minimum fanction (chisq)
            real(rn), intent(out)  :: a(n,nr)       ! LS design matrix: (n x r) matrix A of partial derivatives
            ! of the fitting parameters, i.e., afunc()
            logical, intent(out)   :: ok
            integer, intent(in)    :: maL           ! number of all fit parameters, including those which are not to be fitted
            integer, intent(out)   :: ifehl         ! error indicator
        end subroutine LsqLinCov2

        module subroutine RunPMLE(x,n,nr,y,yp,cyp,r,maL,ifehl)
            implicit none

            integer , intent(in) :: n             ! number of measured values
            real(rn), intent(in) :: x(n)          ! vector of independent input values (x values)
            integer, intent(in)  :: nr            ! number of fitted output quantities (dependent unknowns)
            real(rn), allocatable,intent(in)  :: y(:)     !  nr  on input: vector of the values of the output quantities (y values)
            real(rn), allocatable,intent(out) :: yp(:)    ! on output: vector of the values of the output quantities (y values)
            real(rn), allocatable,intent(out) :: cyp(:,:) ! covariance matrix associated with yp()
            real(rn), intent(out)  :: r             ! valueof the minimum function (chisq)
            integer, intent(in)    :: maL           ! total number of fit parameters, including those being not fitted
            integer, intent(out)   :: ifehl
        end subroutine RunPMLE

    end interface

end module LLcov2
