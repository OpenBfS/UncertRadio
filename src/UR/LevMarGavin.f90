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


module LMG

    USE UR_Params
    implicit none

contains


!##########################################################################################

!function [p,redX2,sigma_p,sigma_y,corr_p,R_sq,cvg_hst] = lm(func,p,t,y_dat,weight,dp,p_min,p_max,c,opts)
!% [p,redX2,sigma_p,sigma_y,corr_p,R_sq,cvg_hst] = lm(func,p,t,y_dat,weight,dp,p_min,p_max,c,opts)
!%
!% Levenberg Marquardt curve-fitting: minimize sum of weighted squared residuals
!% ----------  INPUT  VARIABLES  -----------
!% func   = function of n independent variables, 't', and m parameters, 'p',
!%          returning the simulated model: y_hat = func(t,p,c)
!% p      = initial guess of parameter values                             (n x 1)
!% t      = independent variables (used as arg to func)                   (m x 1)
!% y_dat  = data to be fit by func(t,p)                                   (m x 1)
!% weight = weights or a scalar weight value ( weight >= 0 ) ...          (m x 1)
!%          inverse of the standard measurement errors
!%          Default:  ( 1 / ( y_dat' * y_dat ))
!% dp     = fractional increment of 'p' for numerical derivatives
!%          dp(j)>0 central differences calculated
!%          dp(j)<0 one sided 'backwards' differences calculated
!%          dp(j)=0 sets corresponding partials to zero; i.e. holds p(j) fixed
!%          Default:  0.001;
!% p_min  = lower bounds for parameter values                             (n x 1)
!% p_max  = upper bounds for parameter values                             (n x 1)
!% c      = an optional matrix of values passed to func(t,p,c)
!% opts   = vector of algorithmic parameters
!%             parameter    defaults    meaning
!% opts(1)  =  prnt            3        >1 intermediate results; >2 plots
!% opts(2)  =  MaxIter      10*Npar     maximum number of iterations
!% opts(3)  =  epsilon_1       1e-3     convergence tolerance for gradient
!% opts(4)  =  epsilon_2       1e-3     convergence tolerance for parameters
!% opts(5)  =  epsilon_3       1e-1     convergence tolerance for red. Chi-square
!% opts(6)  =  epsilon_4       1e-1     determines acceptance of a L-M step
!% opts(7)  =  lambda_0        1e-2     initial value of L-M paramter
!% opts(8)  =  lambda_UP_fac   11       factor for increasing lambda
!% opts(9)  =  lambda_DN_fac    9       factor for decreasing lambda
!% opts(10) =  Update_Type      1       1: Levenberg-Marquardt lambda update
!%                                      2: Quadratic update
!%                                      3: Nielsen's lambda update equations
!%
!% ----------  OUTPUT  VARIABLES  -----------
!% p       = least-squares optimal estimate of the parameter values
!% redX2   = reduced Chi squared error criteria - should be close to 1
!% sigma_p = asymptotic standard error of the parameters
!% sigma_y = asymptotic standard error of the curve-fit
!% corr_p  = correlation matrix of the parameters
!% R_sq    = R-squared cofficient of multiple determination
!% cvg_hst = convergence history ... see lm_plots.m
!
!%   Henri Gavin, Dept. Civil & Environ. Engineering, Duke Univ. 4 May 2016
!%   modified from: http://octave.sourceforge.net/optim/function/leasqr.html
!%   using references by
!%   Press, et al., Numerical Recipes, Cambridge Univ. Press, 1992, Chapter 15.
!%   Sam Roweis       http://www.cs.toronto.edu/~roweis/notes/lm.pdf
!%   Manolis Lourakis http://www.ics.forth.gr/~lourakis/levmar/levmar.pdf
!%   Hans Nielson     http://www2.imm.dtu.dk/~hbn/publ/TR9905.ps
!%   Mathworks        optimization toolbox reference manual
!%   K. Madsen, H.B., Nielsen, and O. Tingleff
!%   http://www2.imm.dtu.dk/pubdb/views/edoc_download.php/3215/pdf/imm3215.pdf

!Npar   = length(p);                    !% number of parameters
!Npnt   = length(y_dat);                !% number of data points
!p_old  = zeros(Npar,1);                !% previous set of parameters
!y_old  = zeros(Npnt,1);                !% previous model, y_old = y_hat(t;p_old)

! prnt          = opts(1);        % >1 intermediate results; >2 plots
! MaxIter       = opts(2);        % maximum number of iterations
! epsilon_1     = opts(3);        % convergence tolerance for gradient
! epsilon_2     = opts(4);        % convergence tolerance for parameters
! epsilon_3     = opts(5);        % convergence tolerance for Chi-square
! epsilon_4     = opts(6);        % determines acceptance of a L-M step
! lambda_0      = opts(7);        % initial value of damping paramter, lambda
! lambda_UP_fac = opts(8);        % factor for increasing lambda
! lambda_DN_fac = opts(9);        % factor for decreasing lambda
! Update_Type   = opts(10);       % 1: Levenberg-Marquardt lambda update
!                                 % 2: Quadratic update
!                                 % 3: Nielsen's lambda update equations
!
!         Translation from Matlab into Fortran by G�nter Kanisch, Hamburg, 2024
!
!-------------------------------------------------------------------------------------

    subroutine lm(funcn, npnt, npar, p, t, y_dat, uy_dat, kfit, MaxIter, Iteration,    &      ! func,
                  redX2, sigma_p, sigma_y, covar_p, R_sq, fpenfact, p_penc, up_penc,   &
                  kfitmeth2, ipr, convg, jpr)             ! ,cvg_hst )

        use Brandt,           only: mtxchi
        use Num1,             only: matwrite
        use UR_Gleich,        only: ifehl, upropa_on
        use UR_linft,         only: posdef

        use UR_VARIABLES,     only: MCsim_on



        ! berechne weight aus uy_dat !

        implicit none

        external      funcn

        integer, intent(in)      :: npnt,npar, maxiter,kfit(npar),kfitmeth2      ! prnt,
        real(rn), intent(in)     :: t(npnt),y_dat(npnt),uy_dat(npnt)
        integer, intent(out)     :: iteration
        real(rn), intent(inout)  :: p(npar)
        real(rn), intent(out)    :: redX2,sigma_p(npar),sigma_y(npnt),covar_p(npar,npar),R_sq
        real(rn), intent(in)     :: p_penc(npar),up_penc(npar),fpenfact
        integer, intent(in)      :: ipr,jpr
        logical, intent(out)     :: convg

        integer         :: i,func_calls,m,n               ! global
        integer         :: Update_Type,irunPLSQ,Nfit,k1,nsumv,neq,iratmax
        logical         :: pstop,use_WLS,use_PLSQ,use_PMLE
        real(rn)        :: X2,X2_old,J(npnt,npar),weight(npnt)
        real(rn)        :: epsilon_1, epsilon_2,epsilon_3,epsilon_4,lambda_0,lambda_UP_fac, &
            lambda_DN_fac,eps,fvv,lam0,lambda,lamDN,lamUP,DoF
        real(rn)        :: ybgv,dummy,sumv,xfree,xfree_last
        real(rn)        :: JtWJ(npar,npar),h(npar),p_try(npar),JtWdy(npar),delta_y(npnt)
        real(rn)        :: rho,X2_try,y_hat(npnt),dyda(npar),y_init(npnt),WW(npnt,npnt),W2(npnt,npnt)
        real(rn)        :: JtWJ_SV(npar,npar),covar_y(npnt,npnt)
        real(rn)        :: Wpc(npar,npar),y_mean,nom,den,ratmax,X2_pen,term,alpha

        real(rn)        :: yf(npnt)
        REAL(rn),allocatable  :: JtWJFit(:,:),JtWdyFit(:),JtWJdumFit(:,:),lam_diagFit(:,:),hFit(:)
        REAL(rn),allocatable  :: covar_pFit(:,:)

        m = npnt
        n = npar

        iteration  = 0;                        !% iteration counter
        func_calls = 0;                        !% running count of function evaluations

        Update_Type = 1
        eps    = 1.E-3_rn
        X2     = 1.e-3_rn/eps;                 !% a really big initial Chi-sq value
        X2_old = 1.e-3_rn/eps;                 !% a really big initial Chi-sq value
        J = ZERO                               !% Jacobian matrix
        DoF    = Npnt - Npar + 1               !% statistical degrees of freedom

        epsilon_1 = 1.E-3_rn
        epsilon_2 = 1.E-3_rn
        epsilon_1 = 1.E-3_rn           ! 9.6.2024
        epsilon_2 = 1.E-10_rn           ! 9.6.2024
        epsilon_3 = 1.E-1_rn
        epsilon_4 = 1.E-1_rn
        lam0 = 1.E-2_rn
        lamUP = 11._rn
        lamDN = 9._rn
        lambda_DN_fac = lamDN
        lambda_UP_fac = lamUP
        lambda_0 = 0.01_rn   ! 0.1_rn  !  110._rn
        do i=1,npnt
            weight(i) = ONE / uy_dat(i)**TWO
        end do

        Nfit = 0
        do i=1,npar
            ! if(kfit(i) < 3) Nfit = Nfit + 1
            if(kfit(i) == 1) Nfit = Nfit + 1           ! 13.6.2024
        enddo
        !write(66,*) 'Lm: Nfit=',int(Nfit,2)

! allocate(idx(Nfit))
! j = 0
! do i=1,npars
!   if(ia(i) /= 0) then
!     j = j + 1
!     idx(j) = i
!   endif
! enddo

! idx   = find(dp ~= 0);                ! indices of the parameters to be fit
! Nfit = size(idx);                     ! number of parameters to fit
! stop = 0;                             ! termination flag
        pstop = .false.

!write(0,*) 'size(p)=',size(p),' size(y_dat)=',size(y_dat),' size(uy_dat)=',size(uy_dat)
!write(0,*) 'size(ia)=',size(ia),'  maxiter=',maxiter,' size(p_penc)=',size(p_penc)
!write(0,*) 'size(sigma_p)=',size(sigma_p),' size(sigma_y)=',size(sigma_y)

! extensions:
        convg = .false.
        use_WLS = .false.
        use_PLSQ = .false.
        use_PMLE = .false.
        if(kfitmeth2 == 1) use_WLS = .true.
        if(kfitmeth2 == 2) use_PLSQ = .true.
        if(kfitmeth2 == 3) use_PMLE = .true.
        if(use_PLSQ) irunPLSQ = 1

        if(ipr >= 2 .and. .not. MCsim_on) then
            write(jpr,'(3(a,i0))') 'Npnt=',npnt,' npar=',npar,' ubound(p)=',ubound(p)
            write(jpr,'(a,i0)') 'MaxIter=',MaxIter
            ! write(0,*) 'p=',sngl(p)
        endif
        neq = 0
        xfree_last = -ONE
! calculate a chi-squared value:
        X2 = ZERO
        dyda = ZERO
        do i=1,npnt
            ! write(0,*) 'i=',int(i,2),' t(i)=',sngl(t(i))
            call funcn(t(i),p,npar,kfit,fvv,ybgv,dyda,npnt)
            if(ifehl == 1) return
            y_init(i) = fvv
            J(i,1:npar) = dyda(1:npar)
            if(use_WLS) then
                X2 = X2 + (y_init(i) - y_dat(i))**TWO * weight(i)
            elseif(use_PLSQ) then
                if(irunPLSQ == 1) then
                    X2_try = X2_try + delta_y(i)**TWO * weight(i)
                else
                    X2_try = X2_try + delta_y(i)**TWO /yf(i)**ONE
                end if
            elseif(use_PMLE) then
                if(y_init(i) > 0.000001_rn) then   !   am 9.4.2024
                    X2 = X2 + TWO*(y_init(i)-y_dat(i))
                    if(y_dat(i) > ZERO) X2 = X2 - TWO*(y_dat(i)*log(y_init(i)/y_dat(i)))
                endif
            end if
            ! write(0,'(a,i0,4(a,es11.4),a,i0)') 'i=',i,' t=',t(i),' counts=',y_dat(i), ' y_init=',y_init(i), &
            !                     ' weight(i)=',sngl(weight(i)),' kfm=',kfitmeth2
        enddo
        do i=1,npar
            if(kfit(i) > 2) cycle
            if(fpenfact > ZERO .and. up_penc(i) > ZERO) X2 = X2 + fpenfact*( (p(i) - p_penc(i))**TWO/up_penc(i)**TWO )
        enddo
        Wpc = ZERO
        if(fpenfact > ZERO) then
            ! activate penalized fitting:
            do i=1,npar
                if(abs(p_penc(i)) > 1.E-10_rn) then
                    if(up_penc(i) > 1.e-10_rn) Wpc(i,i) = ONE/up_penc(i)**TWO
                end if
            end do
        endif

        !if ( var(weight) == 0 )                 % identical weights vector
        !       weight = abs(weight(1))*ones(Npnt,1);
        !       disp('using uniform weights for error analysis')
        !else
        !        weight = abs(weight(:));
        !end

        call Lm_matx(funcn,npnt,npar,t,J,p,y_dat,weight,kfit, &
            JtWJ,JtWdy,X2,y_hat, fpenfact,p_penc,up_penc,WW,W2,kfitmeth2,ipr,jpr)
        ! call matwrite(JtWJ,npar,npar,66,'(20es11.3)','JtWJ:')

        if(fpenfact > ZERO) then
            JtWJ = JtWJ + fpenfact*Wpc
            JtWdy = JtWdy - fpenfact*matmul(Wpc,(p-p_penc))
        end if

        JtWJ_SV = JtWJ

        ! if ( max(abs(JtWdy)) < epsilon_1 )
        !        fprintf(' *** Your Initial Guess is Extremely Close to Optimal ***\n')
        !        fprintf(' *** epsilon_1 = %e\n', epsilon_1);
        !        stop = 1;
        ! end

        select case (Update_Type)
          case (1)                                !% Marquardt: init'l lambda
            lambda  = lambda_0;
          case (2,3)
            ! lambda  = lambda_0 * max(diag(JtWJ)); nu=2;
        end select

40      continue

        X2_old = X2;                            !% previous value of X2
        ! if(prout) write(66,*) 'X2_old=',sngl(X2_old)

!------------------------------------- loop ----------------------------------------
        pstop = .false.
        do while ( .not. pstop .and. iteration <= MaxIter )        ! % --- Start Main Loop

            iteration = iteration + 1;

            if(allocated(JtWJFit)) deallocate(JtWJFit,JtWdyFit,JtWJdumFit,lam_diagFit,hFit)
            allocate(JtWJFit(Nfit,Nfit),JtWdyFit(Nfit),JtWJdumFit(Nfit,Nfit),lam_diagFit(Nfit,Nfit),hFit(Nfit))
            !! call matwrite(JtWJ,npar,npar,jpr,'(20es11.3)','JtWJ:')
            call matmodf(1,JtWJ,JtWjFit,npar,Nfit,kfit,npar)
            ! call matwrite(JtWJFit,Nfit,Nfit,jpr,'(20es11.3)','JtWJFit:')

            !% incremental change in parameters
            select case (Update_Type)
              case (1)                                       !% Marquardt
                !h = ( JtWJ + lambda*diag(diag(JtWJ)) ) \ JtWdy;
                if(ipr == 3) write(jpr,'(a,100i2)') 'kfit=',kfit
                ! call matwrite(JtWJ,npar,npar,66,'(20es11.3)','JtWJ:')
                ! JtWJFit = zero
                k1 = 0
                do i=1,npar
                    if(kfit(i) < 3) then
                        k1 = k1 + 1
                        JtWdyFit(k1) = JtWdy(i)
                    endif
                end do
                ! call matwrite(JtWJFit,Nfit,Nfit,66,'(20es11.3)','JtWJFit')
                JtWJdumFit(1:Nfit,1:nfit) = JtWJFit(1:Nfit,1:Nfit)
                ! call matwrite(JtWJdumFit,Nfit,Nfit,jpr,'(20es11.3)','JtWJdumFit')
                do i=1,Nfit
                    JtWJdumFit(i,i) = JtWJdumFit(i,i)*(ONE + lambda)      ! Eq. (13)
                enddo
                ! if(ipr > 2) call matwrite(JtWJdumFit,Nfit,Nfit,jpr,'(20es11.3)','JtWJdumFit mit 1+lam')
                !! call matwrite(JtWJdumFit,Nfit,Nfit,jpr,'(20es11.3)','JtWJdumFit:')
                lam_diagFit = JtWjdumFit - JtWJFit
                call mtxchi(JtWJdumFit)    ! ,Nfit)
                if(.not.posdef) then
                    write(jpr,*) ' A: matrix JtWJdumFit not posdef!'
                    write(0,*) ' A: matrix JtWJdumFit not posdef!'
                    write(66,*) 'y_dat=',sngl(y_dat)
                    call matwrite(J,npnt,npar,66,'(20es12.3)','J:')
                    call matwrite(W2,npnt,npnt,66,'(20es12.3)','W2:')
                    ! call matwrite(WW,npnt,npnt,66,'(20es12.3)','WW:')
                    call matwrite(JtWJ,npar,npar,66,'(20es12.3)','JtWJ:')
                    call matwrite(JtWJFit,Nfit,Nfit,66,'(20es12.3)','JtWJFit:')
                    write(66,*) 'y_dat=',sngl(y_dat)
                    return
                endif
                ! call matwrite(JtWJdumFit,Nfit,Nfit,jpr,'(20es11.3)','inv(JtWJdumFit):')
                hFit = matmul(JtWJdumFit, JtWdyFit)
                if(ipr >= 2) write(jpr,'(/,a,i0,a,es11.4,a,20es12.4)') 'iteration=',iteration, &
                    '  lambda=',lambda,' hFit=',hFit
                !  write(0,'(a,20es12.4)') 'JtWdy=',JtWdy

              case (2)                                       !% % Quadratic and Nielsen
                !     h = ( JtWJ + lambda*eye(Npar) ) \ JtWdy;
                JtWJdumFit(1:Nfit,1:nfit) = JtWJFit(1:Nfit,1:Nfit)
                ! call matwrite(JtWJdumFit,Nfit,Nfit,jpr,'(20es11.3)','JtWJdumFit')
                do i=1,Nfit
                    JtWJdumFit(i,i) = JtWJdumFit(i,i) + lambda      ! Eq. (12)
                enddo
                call mtxchi(JtWJdumFit)    ! ,Nfit)
                if(.not.posdef) then
                    write(jpr,*) ' A: matrix JtWJdumFit not posdef!'
                    write(0,*) ' A: matrix JtWJdumFit not posdef!'
                    return
                endif
                hFit = matmul(JtWJdumFit, JtWdyFit)
                if(ipr >= 2) write(jpr,'(/,a,i0,a,es11.4,a,20es12.4)') 'iteration=',iteration, &
                    '  lambda=',lambda,' hFit=',hFit

            end select

            ! p_try = p + h(idx);                            % update the [idx] elements --> ia()
            p_try = p
            k1 = 0
            do i=1,n
                h(i) = ZERO
                if(kfit(i) == 1) then
                    k1 = k1 + 1
                    p_try(i) = p_try(i) + hFit(k1)
                    h(i) = hFit(k1)
                endif
            enddo

            if(ipr >= 2) then
                if(ipr == 3) write(jpr,'(a,60(i2,8x))') 'param-#     =',(i,i=1,npar)
                if(ipr == 3) write(jpr,'(a,60es10.3)') 'p_try       =',p_try
                if(ipr == 3) write(jpr,'(a,60es10.3)') 'p           =',p
                ! if(ipr == 3) write(jpr,'(a,60es10.3)') 'hFit  =',hFit
                if(ipr == 3) write(jpr,'(a,60es10.3)') 'step h/p_try=',h/p_try
            end if
            sumv = ZERO
            nsumv = 0
            ratmax = ZERO
            iratmax = 0
            do i=1,npar
                if(kfit(i) < 3) then
                    sumv = sumv + abs(h(i)/p_try(i))
                    if(abs(h(i)/p_try(i)) > ratmax) then
                        ratmax = abs(h(i)/p_try(i))
                        iratmax = i
                    endif
                    nsumv = nsumv + 1
                end if
            end do
            sumv = sumv / real(nsumv,rn) * max(ONE,lambda**0.75_rn)
            if(ipr >= 2) write(jpr,*) 'sumv/mfit*lambda=',sngl(sumv)
            if(ipr >= 2) write(jpr,*) 'ratmax=',sngl(ratmax),' iratmax=',int(iratmax,2)

            do i=1,m
                call funcn(t(i),p_try,npar,kfit,fvv,ybgv,dyda,npnt)
                if(ifehl == 1) return
                delta_y(i) = y_dat(i) - fvv
                yf(i) = fvv
            enddo

            func_calls = func_calls + 1;

            ! X2_try = delta_y' * ( delta_y .* weight );     % Chi-squared error criteria
            X2_try = ZERO
            do i=1,npnt
                !call funcn(t(i),p,npar,kfit,fvv,ybgv,dyda,npnt)
                !y_init(i) = fvv
                !J(i,1:npar) = dyda(1:npar)
                if(use_WLS) then
                    X2_try = X2_try + delta_y(i)**TWO * weight(i)
                elseif(use_PLSQ) then
                    X2_try = X2_try + (yf(i) - y_dat(i))**TWO /yf(i)**ONE
                elseif(use_PMLE) then
                    if(yf(i) < 0.000001_rn) cycle   !   am 9.4.2024
                    term = TWO*(yf(i) - y_dat(i))
                    if(y_dat(i) > ZERO) term = term -  TWO*(y_dat(i)*log(yf(i)/y_dat(i)))
                    X2_try = X2_try + term
                endif
            end do
            X2_pen = ZERO
            do i=1,npar
                if(kfit(i) > 2) cycle
                if(fpenfact > ZERO .and. up_penc(i) > ZERO) then
                    term = fpenfact*( (p_try(i) - p_penc(i))/up_penc(i) )**TWO
                    X2_pen = X2_pen + term
                    X2_try = X2_try + term
                endif
            enddo

            if(Update_Type == 2) then
                alpha = dot_product(JtWDyFit,hFit) / (X2_try - X2)/TWO + TWO*dot_product(JtWDyFit,hFit)

            end if


            if(ipr > 2) write(jpr,*) 'Chi2_penal=',sngl(X2_pen)

            !  rho = (X2 - X2_try) / ( h' * (lambda * h + JtWdy) );
            dummy = dot_product(hFit, matmul(transpose(lam_diagFit),hFit) + JtWdyFit)
            rho = (X2 - X2_try)/dummy
            if(ipr >= 2) write(jpr,'(6(a,es11.4))') 'rho=',rho,' X2=',X2,' X2_try=',X2_try, &
                ' X2_old=',X2_old,' dummy=',dummy

            if ( rho > epsilon_4 .and. X2_try < X2_old) then                       ! % it IS significantly better
                X2_old = X2_try
                p = p_try
                if(ipr >= 2) write(jpr,*) 'step accepted'

                if(kfitmeth2 == 2) then
                    if(use_PLSQ .and. irunPLSQ < 4) then
                        X2 = X2_try
                        irunPLSQ = irunPLSQ + 1
                        goto 40
                    end if
                end if

                call Lm_matx(funcn,npnt,npar,t,J,p,y_dat,weight,kfit, &
                    JtWJ,JtWdy,X2,y_hat, fpenfact,p_penc,up_penc,WW,W2,kfitmeth2,ipr,jpr)
                if(fpenfact > ZERO) then
                    JtWJ = JtWJ + fpenfact*Wpc
                    JtWdy = JtWdy - fpenfact*matmul(Wpc,(p-p_penc))
                end if
                ! call matwrite(JtWJ,npar,npar,jpr,'(20es11.3)','JtWJ nach Lm_matx: accept')

                ! % decrease lambda ==> Gauss-Newton method
                select case (Update_Type)
                  case (1)                                !   % Levenberg
                    lambda = max(lambda/lambda_DN_fac,1.e-7_rn)
                end select

            else                                      !     % it IS NOT better

                X2 = X2_old;                            ! % do not accept p_try
                call Lm_matx(funcn,npnt,npar,t,J,p,y_dat,weight,kfit, &
                    JtWJ,JtWdy,X2,y_hat, fpenfact,p_penc,up_penc,WW,W2,kfitmeth2,ipr,jpr)
                if(fpenfact > ZERO) then
                    JtWJ = JtWJ + fpenfact*Wpc
                    JtWdy = JtWdy - fpenfact*matmul(Wpc,(p-p_penc))
                end if
                !call matwrite(JtWJ,npar,npar,jpr,'(20es11.3)','JtWJ nach Lm_matx: not saccept')
                ! write(jpr,*) 'JtWdy=',sngl(Jtwdy)
                ! % increase lambda  ==> gradient descent method
                select case (Update_Type)
                  case (1)                          !         % Levenberg
                    lambda = min(lambda*lambda_UP_fac,1.e+8_rn)
                    !!  if(lambda >= 1.E+12_rn) lambda = 0.01_rn     ! xxxxxxxxxxxxxxxx
                end select

            end if

            xfree = X2/max(1._rn,DoF)
            if(abs(xfree - xfree_last) < 1.E-7_rn) then
                neq = neq + 1
            else
                neq = 0
            endif

            if(.false.) then
                if(neq > 7 .and. lambda > 1.E+12_rn) then
                    write(jpr,*) '!! Convergence not yet reached! Retry !!'
                    pstop = .true.
                    cycle
                end if
            endif
            ! if(X2/DoF < 1.5_rn .and. sumv < 1.E-3_rn) then

            ! if(X2/DoF < 15._rn .and. sumv < 2.E-4_rn .and. iteration > 3) then
            if(X2/max(1._rn,DoF) < 15._rn .and. sumv < 2.2E-4_rn .and. iteration > 3) then        ! 10.4.2024
                ! if(ipr >= 2) write(jpr,*) ' **** convergence by X2/DoF and numv < 2.E-4  **** '
                if(ipr >= 2) write(jpr,*) ' **** convergence by X2/DoF and numv < 2.2E-4  **** '    ! 10.4.2024
                ! if(.true. .and. X2/DoF < 15._rn .and. ratmax < 5.e-11 .and. iteration > 3) then
                !if(.true. .and. X2/DoF < 15._rn .and. ratmax < 5.e-7_rn .and. iteration > 3) then
                !  if(ipr >= 2) write(jpr,*) ' **** convergence by X2/DoF and ratmax < 5.E-11  **** '   ! zu hart!
                pstop = .true.
                convg = .true.
            end if

            if ( maxval(abs(JtWdy)) < epsilon_1  .and. iteration > 3 ) then
                if(ipr >= 2) write(jpr,*) ' **** Convergence in r.h.s. ("JtWdy")  **** '
                if(ipr >= 2) write(jpr,*) ' **** epsilon_1 = ', sngl(epsilon_1)
                pstop = .true.
                convg = .true.
            end if

            ! if ( maxval(abs(h)./(abs(p) + 1.e-12_rn)) < epsilon_2  .and.  iteration > 2 ) then
            if ( .true. .and. maxval(abs(h/(p + 1.e-12_rn))) < epsilon_2  .and.  iteration > 3 ) then
                if(ipr >= 2) write(jpr,*) ' **** Convergence in Parameters **** '
                if(ipr >= 2) write(jpr,*) ' **** epsilon_2 = ', sngl(epsilon_2)
                pstop = .true.
                convg = .true.
            end if
            if ( X2/max(1._rn,DoF) < epsilon_3 .and. iteration > 3 ) then
                if(ipr >= 2) write(jpr,*) ' **** Convergence in reduced Chi-square  ****'
                if(ipr >= 2) write(jpr,*) ' **** epsilon_3 = ', sngl(epsilon_3)
                pstop = .true.
                convg = .true.
            endif
            if ( iteration == MaxIter ) then
                if(ipr >= 2) &
                    write(jpr,'(a,a,i0,a,L1,a,L1,a,i0)') '!!Maximum Number of Iterations Reached Without Convergence !!', &
                    ' upropa_on=',upropa_on,' MCsim_on=',MCsim_on, &
                    ' meth(+1)=',kfitmeth2
                pstop = .true.
            end if
            xfree_last = xfree       ! 22.12.2023

        end do                                      !  % --- End of Main Loop

        ! write(0,*) 'sumv=',sngl(sumv),' iterations=',int(iteration,2)           ! 'ratmax=',sngl(ratmax),

        !% --- convergence achieved, find covariance and confidence intervals

        !% ---- Error Analysis ----

        !if var(weight) == 0   % recompute equal weights for paramter error analysis
        !  weight = DoF/(delta_y'*delta_y) * ones(Npnt,1);
        !end


        redX2 = X2 / max(1._rn,DoF)     ! ;                              % reduced Chi-square
        ! write(0,*) 'red Chi-sq=',sngl(redX2)
        call Lm_matx(funcn,npnt,npar,t,J,p,y_dat,weight,kfit, &
            JtWJ,JtWdy,X2,y_hat, fpenfact,p_penc,up_penc,WW,W2,kfitmeth2,ipr,jpr)

        if(fpenfact > ZERO) then
            JtWJ = JtWJ + fpenfact*Wpc
            JtWdy = JtWdy - fpenfact*matmul(Wpc,(p-p_penc))
        end if

        ! if(ipr == 3) call matwrite(JtWJ,Nfit,Nfit,66,'(20es11.3)','JtWJ')

! % standard error of parameters
        ! covar_p = inv(JtWJ);
        ! write(66,*) 'mtxchi f�r covar_p:'
        if(allocated(covar_pFit)) deallocate(covar_pFit)
        allocate(covar_pFit(Nfit,Nfit))
        call matmodf(1,JtWJ,covar_pFit,npar,Nfit,kfit,npar)
        ! if(ipr == 3) call matwrite(covar_pFit,Nfit,Nfit,66,'(20es11.3)','covar_pFit')
        call mtxchi(covar_pFit)     ! ,nFit)
        if(.not.posdef) write(jpr,*) ' B: matrix covar_pFit not posdef!'

        sigma_p = ZERO
        k1 = 0
        do i=1,n
            if(kfit(i) < 3) then
                k1 = k1 + 1
                sigma_p(i) = sqrt(covar_pFit(k1,k1))
            endif
        end do
        call matmodf(2,covar_pFit,covar_p,Nfit,npar,kfit,npar)
        !  call matwrite(covar_p,npar,npar,66,'(20es11.3)','covar_p')

! % standard error of the fit
        covar_y = matmul(J, matmul(covar_p, transpose(J)))
        ! sigma_y = matmul(transpose(J), matmul(covar_p, J))
        y_mean = sum(y_dat)/real(npnt,rn)
        nom = ZERO
        den = ZERO
        do i=1,npnt
            sigma_y(i) = sqrt(covar_y(i,i))

            call funcn(t(i),p,npar,kfit,fvv,ybgv,dyda,npnt)
            if(ifehl == 1) return
            nom = nom + (fvv - y_mean)**TWO
            den = den + (y_dat(i) - y_mean)**TWO
        enddo
        R_sq = nom/den

    end subroutine lm

!############################################################################################

    !function [JtWJ,JtWdy,Chi_sq,y_hat,J] = lm_matx(func,t,p_old,y_old,dX2,J,p,y_dat,weight,dp,c)
    !% [JtWJ,JtWdy,Chi_sq,y_hat,J] = lm_matx(func,t,p_old,y_old,dX2,J,p,y_dat,weight,{da},{c})
    !%
    !% Evaluate the linearized fitting matrix, JtWJ, and vector JtWdy,
    !% and calculate the Chi-squared error function, Chi_sq
    !% Used by Levenberg-Marquard algorithm, lm.m
    !% -------- INPUT VARIABLES ---------
    !% func   = function ofpn independent variables, p, and m parameters, p,
    !%         returning the simulated model: y_hat = func(t,p,c)
    !% t      = independent variables (used as arg to func)                   (m x 1)
    !% p_old  = previous parameter values                                     (n x 1)
    !% y_old  = previous model ... y_old = y_hat(t;p_old);                    (m x 1)
    !% dX2    = previous change in Chi-squared criteria                       (1 x 1)
    !% J      = Jacobian of model, y_hat, with respect to parameters, p       (m x n)
    !% p      = current  parameter values                                     (n x 1)
    !% y_dat  = data to be fit by func(t,p,c)                                 (m x 1)
    !% weight = the weighting vector for least squares fit ...
    !%          inverse of the squared standard measurement errors
    !% dp     = fractional increment of 'p' for numerical derivatives
    !%          dp(j)>0 central differences calculated
    !%          dp(j)<0 one sided differences calculated
    !%          dp(j)=0 sets corresponding partials to zero; i.e. holds p(j) fixed
    !%          Default:  0.001;
    !% c      = optional vector of constants passed to y_hat = func(t,p,c)
    !%---------- OUTPUT VARIABLES -------
    !% JtWJ    = linearized Hessian matrix (inverse of covariance matrix)     (n x n)
    !% JtWdy   = linearized fitting vector                                    (n x m)
    !% Chi_sq = Chi-squared criteria: weighted sum of the squared residuals WSSR
    !% y_hat  = model evaluated with parameters 'p'                           (m x 1)
    !% J      = Jacobian of model, y_hat, with respect to parameters, p       (m x n)
    !
    !%   Henri Gavin, Dept. Civil & Environ. Engineering, Duke Univ. November 2005
    !%   modified from: ftp://fly.cnuce.cnr.it/pub/software/octave/leasqr/
    !%   Press, et al., Numerical Recipes, Cambridge Univ. Press, 1992, Chapter 15.


    subroutine Lm_matx(funcn,npnt,npar,t,J,p,y_dat,weight,kfit, &
                       JtWJ,JtWdy,Chi_sq,y_hat, fpenfact,p_penc,up_penc,WW,W2,kfitmeth2,ipr,jpr)

        use Num1,             only: matwrite

        implicit none

        integer, intent(in)     :: npnt,npar,kfitmeth2,kfit(npar)     ! number of data points; number of parameters; fit method
        real(rn), intent(in)    :: t(npnt),p(npar),y_dat(npnt),weight(npnt)
        real(rn), intent(inout) :: J(npnt,npar),WW(npnt,npnt),W2(npnt,npnt)
        real(rn), intent(out)   :: JtWJ(npar,npar),JtWdy(npar),Chi_sq,y_hat(npnt)
        real(rn), intent(in)    :: fpenfact,p_penc(npar),up_penc(npar)
        integer, intent(in)     :: ipr,jpr

        integer       :: i
        real(rn)      :: delta_y(npnt),fvv,dyda(npar),ybgv,W1(npnt),DoF

        external  funcn

        DoF = npnt - npar + 1
        ! % evaluate model using parameters 'p'
        chi_sq = ZERO
        if(kfitmeth2 == 3) then     ! use_PMLE
            W1 = ZERO
            W2 = ZERO
        end if
        do i=1,npnt
            call funcn(t(i),p,npar,kfit,fvv,ybgv,dyda,npnt)
            ! if(ifehl == 1) return
            !  write(66,*) 'i=',int(i,2),' fvv=',sngl(fvv),'y_dat=',sngl(y_dat(i)), ' p=',sngl(p)
            y_hat(i) = fvv
            ! recalculate J (derivatives) already here, originally few lines further dowwn
            J(i,1:npar) = dyda(1:npar)
            if(kfitmeth2 == 2) then        ! use_PLSQ
                chi_sq = chi_sq + (fvv - y_dat(i))**TWO / fvv
            endif
            if(kfitmeth2 == 3) then        ! use_PMLE
                if(fvv < 0.000001_rn) cycle
                chi_sq = chi_sq + TWO*(fvv - y_dat(i))
                if(y_dat(i) > ZERO) chi_sq = chi_sq - TWO* y_dat(i)*log(fvv/y_dat(i))
                if(y_dat(i) > EPS1MIN) then
                    W1(i) = ONE/fvv
                    W2(i,i) = y_dat(i)/fvv**TWO
                    ! write(66,*) 'W2: a)  i=',int(i,2),' y_dat(i)=',sngl(y_dat(i)),' fvv=',sngl(fvv)
                else
                    ! vereinfacht, aber gut:
                    !W1(i) = one/fvv
                    W2(i,i) = y_dat(i)/fvv**TWO

                    ! nicht vereinfacht, ebenfalls gut:
                    W1(i) = ONE /(fvv - y_dat(i))
                    !!!!!!       W2(i,i) = zero         ! y_dat(i)/fvv**two          ! zero
                endif
            endif
        enddo
        !if(i == 3) write(66,*) 'LM_mat: W2(3,3)=',sngl(W2(3,3)),' fvv=',sngl(fvv)
        !�if(i == 3) write(66,*) 'LM_mat: y_dat=',sngl(y_dat),' p=',sngl(p)

        !  call matwrite(W2,npnt,npnt,66,'(20es11.3)','W2:')
        !  write(66,*) 'y_hat=',sngl(y_hat)
        !  write(66,*) 'y_dat=',sngl(y_dat)
        !  write(66,*) 'p=',sngl(p)
        delta_y = y_dat - y_hat;           ! % residual error between model and data
! % Chi-squared error criteria
        !  write(0,*) 'kfitmeth2=',kfitmeth2
        WW = ZERO
        do i=1,npnt
            if(kfitmeth2 == 1) WW(i,i) = weight(i)
            if(kfitmeth2 == 2) WW(i,i) = ONE/y_hat(i)
        enddo
        if(kfitmeth2 == 1) then
            Chi_sq = dot_product(delta_y,matmul(WW, delta_y))
        else

        end if
        if(fpenfact > ZERO) then
            do i=1,npar
                if(kfit(i) == 3) cycle
                if(up_penc(i) > ZERO) Chi_sq = Chi_sq + fpenfact*( (p(i) - p_penc(i))/up_penc(i) )**TWO
            enddo
        end if
        if(ipr >= 2) write(jpr,*) 'Chi_sq=',sngl(Chi_sq),' redChi_sq=',sngl(Chi_sq/max(1._rn,DoF))

        !!!    if(ipr == 3) call matwrite(J,npnt,npar,66,'(20es11.3)','J:')
        ! if(ipr == 3)
        !  call matwrite(W2,npnt,npnt,66,'(20es11.3)','W2:')
        ! call matwrite(WW,npnt,npnt,66,'(20es11.3)','WW:')

        if(kfitmeth2 == 1) then
            JtWJ = matmul(transpose(J),matmul(WW,J))
            JtWdy = matmul(transpose(J), (weight * delta_y ))
        elseif(kfitmeth2 == 2) then
            JtWJ = matmul(transpose(J),matmul(WW,J))
            JtWdy = matmul(transpose(J), (ONE/y_hat * delta_y ))
        elseif(kfitmeth2 == 3) then
            JtWJ = matmul(transpose(J),matmul(W2,J))
            ! call matwrite(JtWJ,npar,npar,66,'(20es11.3)','JtWJ:')
            ! call matwrite(W2,npnt,npnt,66,'(20es11.3)','W2:')
            JtWdy = matmul(transpose(J), (W1 * delta_y ))
        endif

    end subroutine Lm_matx

!#########################################################################################

    subroutine matmodf(mode,matA, matB,nA,nB,kfit,nia)

        ! Mode=1: reduce the matrix A to the size of parameters to be fitted (ia(.)=1
        ! mode=2: expand the reduced matrix to the full size, with rows/cols=zero where ia(.)=0.

        use Num1,          only: matwrite
        implicit none

        integer(4),intent(in)   :: nA,nB,nia
        integer(4),intent(in)   :: mode
        real(rn),intent(in)     :: matA(nA,nA)
        real(rn),intent(out)    :: matB(nB,nB)
        integer(4),intent(in)   :: kfit(niA)

        integer(4)      :: i, k, k1, k2

        matB = ZERO
        k1 = 0
        do i=1,nia
            if(kfit(i) == 1) then
                k1 = k1 + 1
                k2 = 0
                do k=1,nia
                    if(kfit(k) == 1) then
                        k2 = k2 + 1
                        if(mode == 1) matB(k1,k2) = matA(i,k)
                        if(mode == 2) matB(i,k) = matA(k1,k2)
                    end if
                enddo
            endif
        end do
!call matwrite(matA,nA,nA,66,'(20es11.3)','matA')
!call matwrite(matB,nB,nB,66,'(20es11.3)','matB')

    end subroutine matmodf

!##################################################################################



    SUBROUTINE UserfPMLE (x, a, ma, kfit, y, dmy, dyda, np)

        use UR_Linft,      only: xB, dmesszeit, RBGmean, mfRBG_fit_PMLE

        implicit none

        integer, INTENT(IN)                   :: ma
        real(rn), INTENT(IN)                  :: x
        real(rn),DIMENSION(ma), INTENT(IN)    :: a
        real(rn), INTENT(OUT)                 :: y
        real(rn), INTENT(OUT)                 :: dmy   ! this dummy variable must not be removed, for calling LM
        real(rn), DIMENSION(ma), INTENT(OUT)  :: dyda

        integer, INTENT(IN)                   :: kfit(ma)
        integer, INTENT(IN)                   :: np

        integer                               :: i, k, nn

        dmy = ZERO

        y = ZERO         ! required total function value at x
        dyda = ZERO

        i = int(x + 0.001_rn)            ! the real value x is used to get the index i of the number of the point in the curve
        nn = 0
        y = ZERO

        if(i == np) then
            y = y + a(ma)*xB(np,ma)
            dyda(1:ma-1) = ZERO
            dyda(ma) = xB(np,ma)
            return
        end if
        do k=1,ma
            !nfd = 0
            !do j=1,mfix
            !  if(k == indfix(j)) nfd = 1
            !end do
            !if(nfd == 1) cycle
            if(kfit(k) == 1) then
                nn = nn + 1
                ! if(iap(k) == 1) then
                y = y + a(nn)*xB(i,nn)
                dyda(nn) = dyda(nn) + xB(i,nn)
            end if
        end do

        if(.not.mfRBG_fit_PMLE)  y = y + RBGmean*dmesszeit(i)
        ! write(66,*) 'i=',int(i,2),' dyda=',sngl(dyda)

    end SUBROUTINE UserfPMLE

end module LMG
