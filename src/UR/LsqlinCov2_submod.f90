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
submodule (LLcov2) LLcov2a


contains

    !     contains:
    ! LinCov2
    ! LsqLinCov2
    ! RunPMLE

    !----------------------------------------------------------------------------------------------!


    module subroutine LinCov2(n,nred,x1,sx1,yvar,covL,maL,R,ok,ifehl)

        !
        !  The routine prepares for the call to LsqLinCov2 for the weighted
        !  least-squares analysis. It is called from Linf() with maL=ma=3.
        !  It especially prepares the covariance matrix covyLF.
        !  Lincov2 returns the vector yvar of fitting parameters and the associated
        !  covariance matrix covL to Linf().
        !
        !     Copyright (C) 2014-2024  Günter Kanisch
        !------------------------------------------------------------------------------------------!


        use UR_Gleich_globals, only: Messwert,kpoint,StdUnc,kableitnum,ncov,  &
                                     kEGr,ngrs,klinf,missingval,Symbole
        use UR_Linft,       only: numd, use_PMLE, posdef, kPMLE, kfitp, mfit, ifit, mfrbg, &
                                  mfRBG_fit_PMLE, covyLF, nkovzr, konstant_r0, sdR0k, k_rbl, &
                                  parfixed, cov_fixed, d0zrate, klincall, condition_upg, &
                                  Qsumarr, use_WTLS, nhp, compare_WTLS, Qxp, mpfx, mpfx_extern, &
                                  mpfxfixed, covpp, covppc, ma, ifitSV2, sfpa
        use UR_DLIM,        only: iteration_on, iterat_passed
        use ur_general_globals, only: MCSim_on
        use UR_MCC,         only: covpmc

        use Top,            only: dpafact
        use Usub3,          only: FindMessk
        use Num1,           only: funcs,matwrite

        implicit none
        !------------------------------------------------------------------------------------------!

        integer, intent(in)    :: n             ! number of measured x values
        integer, intent(in)    :: nred          ! number of parameters actually to be fitted
        real(rn), intent(in)   :: x1(n)         ! vector of x values
        real(rn), intent(in)   :: sx1(n)        ! vector of standard deviations of the x values
        integer, intent(in)    :: maL           ! number of all fit parameters, including those which are
        ! not to be fitted
        real(rn), intent(out)  :: yvar(maL)     ! vector of fitting parameters

        real(rn), intent(out)  :: covL(maL,maL) ! covariance matrix of all fitting parameters, including
        ! also those not to be fitted
        real(rn), intent(out)  :: R             ! value of the minimum function (chisq)
        logical, intent(out)   :: ok
        integer, intent(out)   :: ifehl         ! error variable;  0: nor error;  1: an error occurred
        !------------------------------------------------------------------------------------------!

        real(rn)          :: amt(n,nred)          ! LS design matrix: (n x nred) matrix A of partial derivatives
        ! of the fitting parameters, i.e., afunc()

        real(rn),allocatable  :: covx1(:,:)      ! copy of covy
        integer           :: i,k,j

        real(rn)          :: Uyf(nred,nred)
        ! real(rn)          :: yvar2(nred)
        integer           :: kk,ir,kr,messk,messk2,irun

        real(rn)          :: dpa
        real(rn)          :: Qsum(nred,nred), Qsumx(nred,nred)
        real(rn),allocatable :: xp(:),cxp(:,:),yvar2p(:),xvar2p(:),yvar2(:)
        real(rn)          :: MesswertKP(ngrs+ncov+numd)
        real(rn)          :: xvar2a(nred),cxa(nred,nred)
        integer           :: ifitS(3), klu, ne,mfitp

        character(len=1)  :: cmessk(3)
        character(len=40) :: cnum

        !------------------------------------------------------------------------------------------!
        posdef = .true.

        cmessk = (/'A','B','C' /)  ! names of up to three counting channels (e.g. in an LSC)

        use_PMLE = .false.
        if(kPMLE ==1) use_PMLE = .true.

        klu = klinf
        IF(kfitp(1) > 0) klu = kfitp(1) - 1 + kEGr

        ! array ifit:  ifit(k):  =1: parameter k is fitted;   =2: hold paramter k fiexed at its start value;
        !                        =3: parameter k is not included in fitting;

        !ifitS(1:mAL) = ifit(1:maL)
        !ifitSV2(1:mAL) = ifit(1:mAL)   !  <----- 7.6.2026
        ifitS(1:3) = ifit(1:3)
        ifitSV2(1:3) = ifit(1:3)   !  <----- 18.6.2026
        ifehl = 0
        mfit = 0
        ! do i=1,maL
        do i=1,3
            IF(ifit(i) == 1) mfit = mfit + 1
            if(kPMLE == 1) then
                if(i == mfrbg .and. mfrbg > 0) ifit(mfrbg) = 3    ! <--   13.6.2024
            end if
        end do

        if(mfRBG_fit_PMLE) mfitp = max(mfit, mfrbg)     ! 22.6.2024      ! for the case of using PMLE
        if(.not.mfRBG_fit_PMLE) mfitp = mfit

        allocate(xp(mfitp))
        allocate(cxp(mfitp,mfitp))
        allocate(yvar2p(mfitp))
        allocate(xvar2p(mfitp))
        allocate(yvar2(mfitp))
        xp(:) = ZERO
        cxp(:,:) = ZERO
        yvar2p(:) = ZERO

        ! covariance matrix between x values:
        ! this covariance matrix is also used in MCCALC!

        if(allocated(covyLF)) deallocate(covyLF);  allocate(covyLF(n,n))
        if(allocated(covx1)) deallocate(covx1);    allocate(covx1(n,n))
        if(allocated(covppc)) deallocate(covppc);  allocate(covppc(nhp,nhp))
        covyLF(:,:) = ZERO
        covx1(:,:) = ZERO
        covppc(:,:) = ZERO

        do i=1,n
            messk = FindMessk(i)
            covyLF(i,i) = sx1(i)**TWO
            IF(nkovzr == 1) THEN
                ! take covariances between net count rates into account,
                ! if the same background count rate is applied for the net count rates
                do k=1,i
                    messk2 = FindMessk(k)
                    IF(i /= k) THEN
                        covyLF(k,i) = ZERO
                        if(konstant_r0 .and. messk == messk2) then
                            covyLF(k,i) = sdR0k(messk)**TWO
                        end if
                        if(k_rbl > 0) then
                            IF(abs(StdUnc(kpoint(k_rbl))-missingval) > EPS1MIN) covyLF(k,i) = covyLF(k,i) + StdUnc(kpoint(k_rbl))**TWO
                        end if
                        if(parfixed) covyLF(k,i) = covyLF(k,i) + cov_fixed(k,i)
                        covyLF(i,k) = covyLF(k,i)
                    END IF
                end do
            end if
        end do

        ! Prepare a copy of von covyLF:
        covx1(1:n,1:n) = covyLF(1:n,1:n)
        call LsqLinCov2(x1,covx1,n,mfit,yvar2,Uyf,r,amt,ok,maL,ifehl)

        if(ifehl == 1) then
            write(66,*) 'Lincov2: Vektor sx1: '
            write(66,'(100es10.3)') (sx1(i),i=1,n)
            write(66,*)
            return
        end if
        yvar2p = ZERO
        yvar2p(1:mfit) = yvar2(1:mfit)
        if(mfRBG_fit_PMLE) then                             ! 22.6.2024
            if(mfit == mfitp-1 ) then
                yvar2p(mfitp) = d0zrate(1)
                if(k_rbl > 0) yvar2p(mfitp) = yvar2p(mfitp) + Messwert(k_rbl)      ! 16.6.2024
            end if
        endif
        if(kPMLE == 1) then
            call RunPMLE(x1,n,mfitp,yvar2p,xp,cxp,r,mfitp,ifehl)
            ! the matrix cxp from PMLE is coped now to Uyf:  23.6.2024
            do i=1,size(Uyf,1)         ! 23.6.2024
                Uyf(i,1:size(Uyf,1)) = cxp(i,1:size(Uyf,1))
            end do
        end if
        if(ifehl == 1) return
        !-----------------------------------------------------------------


        klincall = klincall + 1

        if(.not. MCSim_on) then
            covppc(1:nhp,1:nhp) = covpp(1:nhp,1:nhp)
        elseif(MCSim_on) then                         ! 4.7.2023
            covpp(1:nhp,1:nhp) = covpmc(1:nhp,1:nhp)
        end if

        condition_upg = .false.        ! erstmals am 12.6.2024
        if(.not.allocated(Qsumarr)) then
            allocate(Qsumarr(50*50))
            Qsumarr = ZERO
        end if

        if(.not. MCSim_on .and. .not.use_WTLS) then

            condition_upg = ((klincall == 1 .OR. iteration_on .OR. iterat_passed ) .AND. nhp > 0)
            ! condition_upg = condition_upg .or. (MCSim_on .and. imc <= 4) ! <--  unlogisch, MCsim_on wird oben schon ausgeschlossen

            ! ("mpfx parameters": parameters given as arguments of UR Linfit() function)
            !  (e.g., half-lives or efficiencies including uncertainties, being part of the decay function terms)
            ! Here is always klincall=1, if mpfx parameters or their uncertainties are modified!

            ! The calculation of the covariance matrix of the mpfx parameters has been integrated
            ! in the following loop; it is executed for i=1 and klincall=1. the "local" covp is
            ! copied to the "global" covpp, from which it can be copied back when needed.
            ! covpp is used especially by WTLS.

            IF(condition_upg) THEN

                do irun=1,2
                    if(irun == 2 .and. kPMLE == 0) cycle
                    MesswertKP(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)        ! copy of Messwert array
                    if(irun == 1) then
                        call LsqLinCov2(x1,covx1,n,mfit,yvar2,Uyf,r,amt,ok,maL,ifehl)
                    else
                        call LsqLinCov2(x1,covx1,n,mfit,yvar2,Uyf,r,amt,ok,maL,ifehl)
                        if(kPMLE == 1) call RunPMLE(x1,n,mfitp,yvar2,xp,cxp,r,mfitp,ifehl)
                    end if

                    if(.not. use_WTLS .or. compare_WTLS) then
                        if(allocated(Qxp)) deallocate(Qxp)
                        allocate(Qxp(1:nred,1:nhp))
                        Qxp = ZERO
                        ! do ne=1,nred
                        do j=1,nhp
                            ! write(66,*) 'ne,j=',int(ne,2),int(j,2),' parfixed=',parfixed,' mpfxfixed(j)=',mpfxfixed(j)
                            if(mpfx_extern(j) .and. .not. MCSim_on) cycle
                            if(parfixed .and. mpfxfixed(j) == 1) cycle
                            ! if(Messwert(mpfx(j)) > zero) then
                            if(Messwert(mpfx(j)) > ZERO .and. StdUnc(mpfx(j)) > ZERO) then    ! 6.7.2023
                                dpa = Messwert(mpfx(j)) * dpafact(Messwert(mpfx(j))) - Messwert(mpfx(j))
                                Messwert(mpfx(j)) = Messwert(mpfx(j)) + dpa
                                if(irun == 1) then
                                    call LsqLinCov2(x1,covx1,n,mfit,xvar2a,cxa,r,amt,ok,maL,ifehl)
                                    do ne=1,nred
                                        Qxp(ne,j) = (xvar2a(ne) - yvar2(ne))/dpa     ! partial derivatives
                                    end do
                                else
                                    xvar2p = yvar2
                                    xvar2a(1:mfit) = xvar2p(1:mfit)
                                    yvar2p(mfitp) = d0zrate(1)         ! 16.6.2024
                                    if(mfRBG_fit_PMLE) yvar2p(mfitp) = d0zrate(1)         ! 16.6.2024   22.6.2024
                                    call LsqLinCov2(x1,covx1,n,mfit,xvar2p,cxa,r,amt,ok,maL,ifehl)
                                    if(kPMLE == 1) call RunPMLE(x1,n,mfitp,xvar2p,xp,cxp,r,mfitp,ifehl)
                                    if(ifehl == 1) return
                                    do ne=1,nred           ! 6.7.2023
                                        Qxp(ne,j) = (xvar2p(ne) - yvar2p(ne))/dpa     ! partial derivatives
                                    end do
                                end if

                                Messwert(mpfx(j)) = Messwert(mpfx(j)) - dpa
                            end if
                            ! Qxp is needed for the test calculations at the end of LSQgen
                        end do
                        ! end do
                    end if
                    if(.false.) then
                        write(cnum,*) 'Symbol=',Symbole(mpfx(1))%s
                        if(irun == 1) call matwrite(Qxp,mfit,nhp,66,'(1x,130es16.8)', &
                            'Lsqlincov2: Matrix Qxp   without kPMLE: ' // trim(cnum))
                        if(irun == 2) call matwrite(Qxp,mfit,nhp,66,'(1x,130es16.8)', &
                            'Lsqlincov2: Matrix Qxp   with kPMLE: ' // trim(cnum))
                    end if

                end do
                ! nhp and mpfx() come from Rechw1 and Upropa, resp.
                ! re-calculate the vector Qsumarr:

                ! restore the initial state  - after slight possible modifications by Qxe-calculations:
                call LsqLinCov2(x1,covx1,n,mfit,yvar2,Uyf,r,amt,ok,maL,ifehl)
                if(ifehl == 1) return
                yvar2p(1:mfit) = yvar2(1:mfit)
                ! if(mfit == mfitp-1 ) yvar2p(mfitp) = zero
                if(kPMLE == 1) call RunPMLE(x1,n,mfitp,yvar2p,xp,cxp,r,mfitp,ifehl)
                if(ifehl == 1) return
                if(kPMLE == 1) then
                    do i=1,size(Uyf,1)         ! 23.6.2024
                        Uyf(i,1:size(Uyf,1)) = cxp(i,1:size(Uyf,1))
                    end do
                end if

                IF(klincall == 1 .and. .not.iteration_on) THEN
                    write(66,*) 'yvar2=',sngl(yvar2),'  x1=',sngl(x1)
                    call matwrite(covx1,n,n,66,'(1x,130es16.8)', &
                        'Lsqlincov2: Matrix covx1:')
                    call matwrite(Uyf,mfit,mfit,66,'(1x,130es16.8)', &
                        'Lsqlincov2: Matrix Uyf   Before QMAT treatment:')
                end if

                if(.not.use_WTLS .or. compare_WTLS) then

                    !call matwrite(Qxp,nhp,nhp,66,'(1x,130es16.8)', &
                    !            'Lsqlincov2: Matrix Qxp   Before QMAT treatment:')
                    !call matwrite(covppc,nhp,nhp,66,'(1x,130es16.8)', &
                    !            'Lsqlincov2: Matrix covppc   Before QMAT treatment:')

                    ! write(66,*) 'condition_upg=',condition_upg,' klincall=',klincall

                    Qsumx = matmul(Qxp, matmul(covppc, Transpose(Qxp)))
                    IF(.true. .and. klincall == 1) THEN
                        call matwrite(Qsumx,mfit,mfit,66,'(1x,130es16.8)', &
                            'Lsqlincov2: Matrix Qsumx   Before Qsumarr:')
                        !call matwrite(Qxp,nred,nhp,66,'(1x,130es16.8)', &
                        !            'Lsqlincov2: Matrix Qxp   Before Qsumarr:')
                    end if
                end if

                forall(i=1:nred, k=1:nred)
                    Qsumarr((i-1)*nred + k) = Qsumx(i,k)
                end forall

            end if       ! condition_upg

            IF(nhp == 0 .and. (.not. use_WTLS .or. (use_WTLS .and. compare_WTLS))) then
                Qsumarr(1:size(Qsumarr)) = ZERO
            end if

            if(.not.use_WTLS .or. (use_WTLS .and. compare_WTLS)) then
                forall(i=1:nred, k=1:nred)
                    Qsum(i,k) = Qsumarr((i-1)*nred + k)
                end forall

                IF(.false. .and. klincall == 1) THEN
                    call matwrite(Qsum,mfit,mfit,66,'(1x,130es16.8)', &
                        'Lsqlincov2: Matrix Qsum after ....')
                end if
                Uyf(1:nred,1:nred) = Uyf(1:nred,1:nred) + Qsum(1:nred,1:nred)
                ! Note: if .not.MCsim_on, this Uyf covers also the PMLE case via the Qxp in Qsumx
                !       -->  not MCsim_on, independent on kPMLE
                IF(klincall == 1 .and. .not.iteration_on) THEN
                    call matwrite(Uyf,mfit,mfit,66,'(1x,130es16.8)', &
                        'Lsqlincov2: Matrix Uyf   After QMAT treatment:')
                end if
            end if
        end if   ! .not.MCsim_on

        ifit(1:maL) = ifitS(1:maL)
!----------------

! copy the shortened list yvar2 of fitting parameter values onto the full list(array) yvar:
        yvar(1:maL) = ZERO
        kk = 0
        if(kPMLE == 0) then
            do i=1,maL
                if(ifit(i) == 3) cycle
                if(mfit < mfitp) then
                    if(i <= mfit) then
                        if(ifit(i) == 2) yvar(i) = ONE       ! parameter fixed
                    end if
                end if
                kk = kk + 1
                if(kk <= nred) yvar(i) = yvar2(kk)
            end do
        else
            ! yvar(1:size(yvar)) = xp(1:size(yvar))
            yvar(1:mfitp) = xp(1:mfitp)
        end if
! copy also the shortened matrix Uyf onto the full matrix covL:
        covL(1:maL,1:maL) = ZERO
        ir = 0
! if(kPMLE == 0) then
! if(kPMLE == 0 .or. (.not.MCsim_on .and. kPMLE == 1)) then    ! 12.6.2024  am besten!
! mfit replaced by mfitp (mfitp covers also mfit for non-PMLE)   25.6.2024
        do i=1,maL
            IF(mfit < maL .and. ifit(i) > 2 .and. i /= mfrbg) CYCLE
            ir = min(ir+1,mfitp)
            kr = 0
            do k=1,maL
                IF(mfit < maL .and. ifit(k) >= 2 .and. k /= mfrbg) CYCLE
                kr = min(kr+1,mfitp)
                if(ifit(i) == 1 .and. ifit(k) == 1) then
                    covL(i,k) = Uyf(ir,kr)
                end if
            end do
        end do

        if(.false. .and. kableitnum == 0 .and. .not. iteration_on ) then
            call matwrite(covL,nred,nred,66,'(1x,130es16.8)', &
                'End of Lincov2: Matrix covL:')
            write(66,*) 'fit parameter yvar: ',real(yvar,8)
        end if


        sfpa(1:ma) = ZERO
        do i=1,ma
            if(covL(i,i) > ZERO) sfpa(i) = SQRT(covL(i,i))
        end do
        if(kPMLE == 1) sfpa(mfrbg) = sqrt(cxp(mfrbg,mfrbg))


    end subroutine LinCov2

!#######################################################################

!  The weighted least squares subroutine LsqLinCov2 calculates the fitting
!  parameters y (usually activities) for a fitting function which is linear
!  in the measured values x, usually count rates. It is possible that the
!  measured values are correlated, which requires a non-diagonal covariance
!  matrix covy.
!  The partial derivatives of the fitting function with respect to the
!  fitting parameters are elements of a matrix A.
!
!  The matrix algebra methodD is taken from then textbook:
!     Klaus Weise u. Wolfgang Wöger: Meßunsicherheit und Meßdatenauswertung.
!     Verlag Wiley-VCH Weinheim,1999,
!     S. 200 oben (Abschnitt 5.4.2 Lineare Kurvenanpassung)
!
!  For the necessary matrix algebra, if not directly available from the Fortan
!  compiler, routines are used which are taken from the textbook
!
!   Datan-Library (Fortran) from:
!   Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und Computerprogrammen;
!   4. Auflage. Spektrum, Akademischer Verlag, Heidelberg-Berlin. In German.
!   This text book is also available in an English version.
!
!  The results of LsqLinCov2 are successfully compared  with the Datan subroutine
!  LSQGEN (works iteratively). In fact, LSQGEN performs the much more complex
!  weighted total least squares (WTLS) method!
!
!  25.10.2005 Günter Kanisch, BFAFi Hamburg, Institut für Fischereiökologie

!  Note about matrices in Fortran:
!  the 1st index counts the rows, the 2nd index counts the columns of a matrix.
!
!  Copyright (C) 2014-2024  Günter Kanisch
!

!--------------------------------------------------------------------------

    module SUBROUTINE LsqLinCov2(x,covy1,n,nr,y,Uy,r,a,ok,maL,ifehl)

        use UR_Linft,     only: xA,kPMLE,ifit,mfrbg,posdef,klincall
        use UR_Gleich_globals,    only: kableitnum
        use UR_DLIM,      only: iteration_on,limit_typ
        use Brandt,       only: mtxchi
        use Num1,         only: funcs,matwrite
        use Top,          only: WrStatusbar
        use translation_module,   only: T => get_translation

        implicit none

        integer   , intent(in)      :: n             ! number of measured values
        real(rn), intent(in)        :: x(n)          ! vector of independent input values (x values)

        real(rn), intent(in)        :: covy1(n,n)    ! covariance matrix of the x values
        integer   , intent(in)      :: nr            ! number of fitted output quantities (dependent unknowns)
        real(rn), intent(out)       :: y(nr)         ! vector of the values of the output quantities
        real(rn), intent(out)       :: Uy(nr,nr)     ! covariance matrix dof the output quantities
        real(rn), intent(out)       :: r             ! value of the minimum fanction (chisq)
        real(rn), intent(out)       :: a(n,nr)       ! LS design matrix: (n x r) matrix A of partial derivatives
        ! of the fitting parameters, i.e., afunc()
        LOGICAL, intent(out)        :: ok
        integer   , intent(in)      :: maL           ! number of all fit parameters, including those which are not to be fitted
        integer   ,intent(out)      :: ifehl         ! error indicator

        integer          :: i, kn, k, mm0
        real(rn)         :: aFunc(maL)

        real(rn),allocatable  :: cs(:),xh(:),cpy(:), Ux(:,:)
        !-----------------------------------------------------------------------

        allocate(cs(n), xh(nr), cpy(n))
        allocate(Ux(n,n))

        cpy(1:n) = x(1:n)
        ifehl = 0
        ! Prepare the design matrix A (amt), dimension (n x nr), it refers only to parameters
        ! to be fitted (according to the array ifit):
        a(1:n,1:nr) = ZERO

        IF(kPMLE == 1) THEN
            if(allocated(xa)) deallocate(xA)
            allocate(xA(n,nr))
        end if

        do i=1,n
            call funcs(i,afunc)     ! External function, yields for y(i) values aFunc(1:maL),
            ! which are considered as the partial derivatives with respect
            ! to the fitting parameters
            if(ifehl == 1) return

            ! array ifit:  ifit(k):  =1: parameter k is fitted;   =2: hold paramter k fiexed at its start value;
            !                        =3: parameter k is not included in fitting;   kn = 0

            kn = 0
            do k=1,maL
                IF(ifit(k) == 1) THEN
                    if(k <= maL) then
                        kn = kn + 1
                        a(i,kn) = afunc(k)
                    endif
                    IF(kPMLE == 1) then
                        if(k < maL) xa(i,kn) = afunc(k)      ! 7.6.2024
                    end if
                end if
            end do
            IF(.false. .and. kPMLE == 1 .and. mfrbg > 0) then          ! test with example LUBW*SR89 !!!!
                if(ifit(mfrbg) <= 2) THEN
                    IF(mfrbg == 2) xa(2,i) = ONE            ! i: row,  2: column
                    IF(mfrbg == 3) xa(3,i) = ONE
                end if
            end if
        end do

        !IF(klincall == 1 .and. .not. iteration_on .AND. limit_typ < 1 .and. kableitnum == 0) THEN
        ! write(66,*) 'LSQLIN: vector x: ',sngl(x)
        ! call matwrite(A,n,nr,n,nr,66,'(20es11.4)','Matrix A in LsqLinCov2,   last column: net rate' )
        !end if

        ! For maintaining the original input matrix covy1,
        ! it is copied to the matrix Ux, which then can be inverted
        Ux(1:n,1:n) = covy1(1:n,1:n)

        IF(.false.) then
            if(klincall == 1 .and. .not.iteration_on .AND. limit_typ < 1 .and. kableitnum == 0) THEN
                call matwrite(Ux,n,n,66,'(1x,60es11.4)','Lincov2: inoput data: Ux=covyLF:')
            end if
        end if

        ! invert the matrix Ux (covariance marix of the x values), with Cholesky decomposition
        if(n == 1) then
            Ux(1,1) = ONE / Ux(1,1)
        elseif(n > 1) then
            CALL mtxchi(Ux)      ! Ux now contains its inverse
        end if

        if(.not.posdef) then
            call matwrite(Ux,n,n,66,'(1x,60es10.3)', &
                'Lincov2: Ux not pos. def.,  Ux:')
            write(66,*) 'Lincov2: Vektor x: '
            write(66,'(100es10.3)') (real(x(i),8),i=1,n)
            write(66,*)
            call WrStatusbar(3, "Lincov2: Matrix Ux " // T("not pos.def."))

            ifehl = 1
            return
        end if

        ! First step of the LS analysis: calculate the inverse of the
        ! covariance matrix of the output quantities:
        Uy = matmul(Transpose(a), matmul(Ux, a))

        ! Uy is still to be inverted:
        if(nr == 1) then
            Uy(1,1) = ONE / Uy(1,1)
        elseif(nr > 1) then
            CALL mtxchi(Uy)       ! now Uy is the desired covariance matrix
        end if

        if(.not.posdef) then
            call matwrite(Uy,nr,nr,66,'(1x,60es11.4)', 'Lincov2: mtxchi:Uy not pos. def.:  Uy:')

            call WrStatusbar(3, "Lincov2: Matrix Uy " // T("not pos.def."))
            call WrStatusbar(4, 'Lincov2: ' // T('Abortion!'))
            !call matwrite(covy,n,n,n,n,66,'(1x,40es12.5)', &
            !            'Lsqlincov2: Matrix Qxp   Before QMAT treatment:')
            ifehl = 1
            return
        end if

        IF(.false.) then
            if(.not. iteration_on .AND. limit_typ < 1 .and. kableitnum == 0) THEN
                mm0 = min(nr,6)
                call matwrite(Uy,mm0,mm0,66,'(1x,20es16.8)','Lsqlincov2: Matrix Uy:')
            end if
        end if

        ! Second step of the LS analysis: calculate the vector y of output quantity values
        y = Matmul(Uy, Matmul(Transpose(a), Matmul(Ux, x)))

        ! Third step: calculate r=Chisqr-min, according to Eq. (5.65) in Weise/Wöger:
        cs = Matmul(Ux,x)
        xh = Matmul(Transpose(a),cs)

        r = dot_product(x,cs)
        r = r - dot_product(y, xh)

        ok = .TRUE.
!-----------------------------------------------------------------------------------

    END SUBROUTINE LsqLinCov2

!######################################################################################


    module subroutine RunPMLE(x,n,nr,y,yp,cyp,r,maL,ifehl)

        use UR_Linft,     only: kPMLE,k_rbl,ifit,d0zrate,fpaSV,singlenuk,      &
                                mfrbg,mfRBG_fit_PMLE,xA,xB,kfitmeth,      &
                                ifitSV2,iap,dmesszeit,noncv_PMLE, &
                                chisqr_pmle,iteration_pmle,convg_pmle,pa_pmle, &
                                parat_kegr,RBGMean,dgrossrate,pa_mfrbg_mc          !!! ,   PMLE_Routine
        use UR_Gleich_globals,    only: kpoint,StdUnc,Messwert,kEGr
        use UR_DLIM,      only: iteration_on

        use Brandt,       only: mtxchi,mean
        use Num1,         only: funcs,matwrite

        use Top,          only: dpafact
        use ur_general_globals, only: MCSim_on
        use UR_Derivats,  only: dervtype
        use LMG
        use fparser,      only: evalf
        use RW2,          only: kqt_find
        use RND,          only: RndU


        implicit none

        integer   , intent(in)      :: n             ! number of measured values
        real(rn), intent(in)        :: x(n)          ! vector of independent input values (x values)

        integer   , intent(in)      :: nr            ! number of fitted output quantities (dependent unknowns)
        ! real(rn), intent(in)        :: y(nr)       ! on input: vector of the values of the output quantities (y values)
        real(rn),allocatable,intent(in)  :: y(:)     !  nr  on input: vector of the values of the output quantities (y values)
        real(rn),allocatable,intent(out) :: yp(:)    ! on output: vector of the values of the output quantities (y values)
        real(rn),allocatable,intent(out) :: cyp(:,:) ! covariance matrix associated with yp()
        real(rn), intent(out)       :: r             ! value of the minimum function (chisq)
        integer   , intent(in)      :: maL           ! total number of fit parameters, including those being not fitted
        integer   ,intent(out)      :: ifehl

        integer               :: i,k
        real(rn)              :: mw_rbl, umw_rbl, yvv, dmy, dyda(10)
        real(rn),allocatable  :: dpi1LF(:,:,:)
        integer               :: maxiter,iteration,kfitmeth2,ipr,npar,mfit,kqt,n1
        real(rn)              :: fpenfact,r_sq,redx2,pavor
        logical               :: convg
        real(rn),allocatable  :: p_penc(:),up_penc(:),sigma_y(:),sigma_p(:),covar_p(:,:),pa(:),uxx(:)
        real(rn),allocatable  :: t(:),xx(:)

        !-------------------------------------------------------------------------------------
        ! For PMLE, the next "free" fitting parameter (i.e. number mfrbg) of in total 3 fitting parameters
        ! is taken to represent the sum of background and blank count rates. The existing choice
        ! (fit/fixed/not used) must not be changed by the user.
        ! Internally, the parameter of number mfrbg is fully fitted in Lm. However, this is
        ! meaningful only, if the parameter numbers < mfrbg do NOT represent a quasi-constant behavior
        ! over time; if the latter would be true, the fitting routine would not yield reliable values of
        ! the two parameters which are in concurrence then.
        !
        ! the parameter mfRBG_fit_PMLE is set to .true. in Uncw_Init.( Parameter is non-linearly fitted)
        !-------------------------------------------------------------------------------------
        ifehl = 0
        dervtype = 'A'
        !!! kqt = kqt_find(iteration_on, limit_typ, MCsim_on, kqtypx)
        kqt = kqt_find()           ! 27.4.2025
        n1 = n
        if(mfRBG_fit_PMLE) n1 = n + 1

        if(ifit(mfrbg) == 3) ifit(mfrbg) = 2       ! 7.6.2024   necessary

        if(allocated(dpi1LF)) deallocate(dpi1LF);    allocate(dpi1LF(n,3,3))
        dpi1LF(:,:,:) = ZERO

        mw_rbl = ZERO
        umw_rbl = ZERO
        if(k_rbl > 0) mw_rbl = Messwert(kpoint(k_rbl))
        if(k_rbl > 0) umw_rbl = StdUnc(kpoint(k_rbl))
        RBGMean = mean(d0zrate(1:n)) !  is average of background count rates d0zrate()
        if(k_rbl > 0) RBGMean = RBGMean + mw_rbl   ! blank value added

        if(allocated(xB)) deallocate(xB)
        allocate(xB(n1,nr))
        xB = ZERO
        do i=1,n1
            ! if(i < n1) then
            if(i <= n) then
                do k=1,nr
                    if(k < nr .or. nr == 1) then
                        xB(i,k) = xA(i,k) * dmesszeit(i)
                    end if
                    if(mfRBG_fit_PMLE .and. k == nr) xB(i,k) = (d0zrate(i) + mw_rbl)/RBGMean * dmesszeit(i)     ! a fraction of RBGMean
                end do
            else
                do k=1,nr
                    if(k < nr) then
                        xB(i,k) = ZERO
                    end if
                    if(k == nr) xB(i,k) = ONE * dmesszeit(1)
                end do
            endif
        end do

        if(allocated(yp)) deallocate(yp);   allocate(yp(mfrbg))
        if(allocated(cyp)) deallocate(cyp);  allocate(cyp(mfrbg,mfrbg))
        yp = ZERO
        cyp = ZERO

        mfit = 0
        iap = 0
        do i=1,mfrbg
            IF(ifit(i) == 1) THEN
                mfit = mfit + 1
                iap(i) = 1               ! fitting the parameter: 1=yes, 0=no
            else
                iap(i) = 0
            end if
        end do

        if(mfRBG_fit_PMLE .and. mfrbg > 0) then
            iap(mfrbg) = 1   !  mfrbg designates the parameter of back ground count rate
            mfit = mfit + 1
        end if

        allocate(uxx(n1),xx(n1))

        do i=1,n
            xx(i) = x(i) + dgrossrate(i) * dmesszeit(i)
        end do
        uxx(1:n) = [ (sqrt(xx(i)),i=1,n)  ]
        if(mfRBG_fit_PMLE) then
            if(MCsim_on) xx(n1) = pa_mfrbg_mc*dmesszeit(1)         ! 26.6.2024
            if(.not.MCsim_on) xx(n1) = RBGMean*dmesszeit(1)         ! 26.6.2024
            uxx(n1) = sqrt(xx(n1))       ! 22.6.2024
        end if
        do i=1,nr
            yp(i) = y(i)
        end do
        if( mfrbg > 0 .and. mfRBG_fit_PMLE) then
            if(mfrbg <= maL) then      !  <-- am 6.6.2024
                IF(ABS(yp(mfrbg)) < 1.E-12_rn) yp(mfrbg) = fpaSV(mfrbg)

                IF(singlenuk .AND. iteration_on .and. kPMLE == 1 .and. mfrbg > 0 .and. ifit(mfrbg) == 3) THEN
                    yp(mfrbg) = RBGMean   ! fpaSV(mfrbg)       ! 16.6.2024
                end if

                IF(yp(mfrbg) <= ZERO .and. ifit(mfrbg) <= 2) then
                    yp(mfrbg) = RBGMean      ! d0zrate(1) + mw_rbl
                END IF
            endif
        end if

        ! npar = mfrbg
        if(mfRBG_fit_PMLE)      npar = mfrbg                       !  2.5.2025
        if(.not.mfRBG_fit_PMLE) npar = mfit        ! 22.6.2024     !


        ! setup parameters for penalized fitting
        ! if(allocated(p_penc)) deallocate(p_penc,up_penc,pa,t)
        allocate(p_penc(npar),up_penc(npar),pa(npar),t(n1))
        p_penc = ZERO
        up_penc = ZERO
        t(1:n1) = [ (real(i,rn),i=1,n1) ]
        if(npar > 1 .and. mfRBG_fit_PMLE) then               ! 22.6.2024
            pa(1:npar-1) = yp(1:npar-1)
            pa(npar) = RBGmean
        else
            pa(1:npar) = yp(1:npar)
        end if
        ! if(kqt == 2 .and. MCSim_on) pa(kEGr) = pa(kEGr) * (one + 0.2_rn*(RndU()-0.5_rn))
        pavor = pa(kEGr)

        fpenfact = ZERO
        if(mfRBG_fit_PMLE) then
            fpenfact = 0.0_rn   ! 0.01_rn
            p_penc = 0._rn
            up_penc = 0._rn
            p_penc(mfrbg) = pa(mfrbg)
            up_penc(mfrbg) = TWO*0.5_rn*sqrt(pa(mfrbg)/dmesszeit(1)) ! * zero
            !     ! if(kqt == 2) up_penc(mfrbg) = up_penc(mfrbg) * 0.2_rn
        endif

        allocate(sigma_y(n1),covar_p(npar,npar),sigma_p(npar))
        maxiter = 40
        iteration = 0
        ipr = 0                  ! <-- decide, how detailed shall the output be? (0,1,2,3)
        ! if(kqt == 2 .and. .not.MCsim_on) ipr= 3
        ! ipr = 3

        kfitmeth2 = kfitmeth + 1
        if(ipr == 3) then
            ! write(66,*) 'list=',int(list,2)
            write(66,*) 'n=',int(n,2),' npar=',int(npar,2)
            write(66,*) 'Startparams: ',sngl(pa)
            ! write(66,*) 'yp : ',sngl(yp)
            write(66,*) 'xx : ',sngl(xx)
            write(66,*) 'uxx: ',sngl(uxx)

        end if

        call lm(userfPMLE,n1,npar,pa,t,xx,uxx,iap,  MaxIter,Iteration,    &      ! func,
            redX2,sigma_p,sigma_y,covar_p,R_sq, fpenfact,p_penc,up_penc, &
            kfitmeth2,ipr, convg,66)
        if(.not.convg) noncv_PMLE = noncv_PMLE + 1

        chisqr_pmle = redx2
        iteration_pmle = iteration
        convg_pmle = convg
        parat_kegr = pa(kEGr)/pavor
        pa_pmle(1:npar) = pa(1:npar)

        !  Ab hier liegt das Ergebnis vor !

        if(ipr > 0 .and. kqt >= 1) then !  .and. MCsim_on) then
            WRITE(66, *)  &
                '*****',' Ergebnisse: ',iteration,' Iterat.','ChisqR=',redX2,' convg?=',convg, &
                '  kqt=',kqt,' MC=',MCsim_on
            DO i=1,npar
                WRITE(66,'(4X,a,i2,3X,a,1pg14.6,3X,a,g14.6,0p,3X,1(a,i1))') ' i=',i,  &
                    'a=',pa(i),'u(a)=',sigma_p(i),'iap=',iap(i)
            END DO
            write(66,*)
            write(66,*) 'fitted decay curve (in counts):'
            do i=1,n1
                call userfPMLE(t(i),pa,npar,iap,yvv,dmy,dyda,n1)
                WRITE(66,'(a,i3,a,es12.5,3(a,es12.5))') 'i=',i,'  xx=',xx(i),'  fitted=',yvv, &
                    ' ratefit=',yvv                ! ,'  bg=',pa(mfrbg)   !    tmedian*(mw_rbl + d0zrate(i))
            end do
            write(66,*)
            call matwrite(xB/dmesszeit(1),n1,nr,66,'(20es11.4)','Matrix xB in RunPMLE' )
            write(66,*)
        end if
        yp(1:npar) = pa(1:npar)
        do i=1,npar
            cyp(i,1:npar) = covar_p(i,1:npar)
        enddo
        ! if(kqt == 1) write(66,*) 'nach lm: yp=',sngl(yp)
        ! if(kqt == 1) call matwrite(cyp,npar,npar,66,'(20es11.3)','covmat cyp:')
        !-------------------------------------------------------------------------------------
        ! r = chisq
        r = chisqr_pmle * real(max(1,n1-npar+1),rn)       !  2025.01.23 GK

        ! if(kableitnum== 0 .and. .not. iteration_on) then
        !     ! write(66,*) 'RunPMLE:  vector yp=',sngl(yp(1:3))
        !     ! call matwrite(cyp,maL,maL,maL,maL,66,'(5es12.4)','RunPMLE:   matrix cyp:')
        ! end if

        ifit(1:3) = ifitSV2(1:3)

    end subroutine RunPMLE

    !############################################################################################

end submodule LLcov2a
