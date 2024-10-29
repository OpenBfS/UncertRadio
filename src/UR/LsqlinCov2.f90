
module LLcov2

    use UR_types
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

            integer   ,INTENT(IN)    :: n             ! number of measured x values
            integer   ,INTENT(IN)    :: nred          ! Anzahl der tatsächlich zu fittenden Parameter
            real(rn),INTENT(IN)      :: x1(n)         ! vector of x values
            real(rn),INTENT(IN)      :: sx1(n)        ! vector of standard deviations of the x values
            integer   ,INTENT(IN)    :: maL           ! number of all fit parameters, including those which are
            ! not to be fitted
            real(rn),INTENT(OUT)     :: yvar(maL)     ! vector of fitting parameters
            real(rn),INTENT(OUT)     :: covL(maL,maL) ! covariance matrix of all fitting parameters, including
            ! also those not to be fitted
            real(rn),INTENT(OUT)     :: R             ! value of the minimum function (chisq)
            LOGICAL,INTENT(OUT)      :: ok
            integer   ,INTENT(OUT)   :: ifehl         ! error variable;  0: nor error;  1: an error occurred
        end subroutine LinCov2

        module SUBROUTINE LsqLinCov2(x,covy1,n,nr,y,Uy,r,a,ok,maL,ifehl)

            implicit none

            integer   , INTENT(IN)      :: n             ! number of measured values
            real(rn), INTENT(IN)        :: x(n)          ! vector of independent input values (x values)
            real(rn), INTENT(IN)        :: covy1(n,n)    ! covariance matrix of the x values
            integer   , INTENT(IN)      :: nr            ! number of fitted output quantities (dependent unknowns)
            real(rn), INTENT(OUT)       :: y(nr)         ! vector of the values of the output quantities
            real(rn), INTENT(OUT)       :: Uy(nr,nr)     ! covariance matrix dof the output quantities
            real(rn), INTENT(OUT)       :: r             ! value of the minimum fanction (chisq)
            real(rn), INTENT(OUT)       :: a(n,nr)       ! LS design matrix: (n x r) matrix A of partial derivatives
            ! of the fitting parameters, i.e., afunc()
            LOGICAL, INTENT(OUT)        :: ok
            integer   , INTENT(IN)      :: maL           ! number of all fit parameters, including those which are not to be fitted
            integer   ,INTENT(OUT)      :: ifehl         ! error indicator
        end subroutine LsqLinCov2

        module subroutine RunPMLE(x,n,nr,y,yp,cyp,r,maL,ifehl)
            implicit none

            integer   , INTENT(IN)      :: n             ! number of measured values
            real(rn), INTENT(IN)        :: x(n)          ! vector of independent input values (x values)

            integer   , INTENT(IN)      :: nr            ! number of fitted output quantities (dependent unknowns)
            real(rn),allocatable,INTENT(IN)  :: y(:)     !  nr  on input: vector of the values of the output quantities (y values)
            real(rn),allocatable,INTENT(OUT) :: yp(:)    ! on output: vector of the values of the output quantities (y values)
            real(rn),allocatable,INTENT(OUT) :: cyp(:,:) ! covariance matrix associated with yp()
            real(rn), INTENT(OUT)       :: r             ! valueof the minimum function (chisq)
            integer   , INTENT(IN)      :: maL           ! total number of fit parameters, including those being not fitted
            integer   ,INTENT(OUT)      :: ifehl
        end subroutine RunPMLE

    end interface

end module LLcov2
