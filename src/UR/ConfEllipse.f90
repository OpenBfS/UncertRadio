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
module Celli
    use UR_types, only: rn
    use UR_params, only: ZERO, ONE, TWO, EPS1MIN, PI

    ! contains:
    !
    !   PrepEli
    !   Confidoid
    !   Trans3
    !   Scal3
    !   Rotate3
    !   TwoNormalDist
    !   Py_givenX0


contains

    !----------------------------------------------------------------------------------------------!

    subroutine PrepEli()

        ! prepares the dialog for calculating and displaying a confidence ellipse
        !   Copyright (C) 2014-2023  Günter Kanisch

        use UR_Gleich_globals, only: Symbole, knumEGr
        use UR_Linft,  only: valEGr, uncEGr, corrEGr, covEGr
        use top,       only: FindItemS
        use UR_DLIM,   only: W1minusG
        use Rout,      only: WDPutTreeViewColumnLabel, &
                             WTreeViewPutStrCell, &
                             WTreeViewPutDoubleCell, WDPutLabelString, WDSetCheckButton, &
                             WDSetComboboxAct, WTreeViewSetColorRow
        use color_theme, only: get_color_string

        implicit none

        integer              :: i, j, ifall
        real(rn)             :: rho, pSV
        character(len=60)    :: xstr
        !------------------------------------------------------------------------------------------!
        pSV = W1minusG

        if(knumEGr <= 1) return
        !------------------------------------------------------------------------------------------!
        if(.false.) then  ! if this condition is activated, the given examples can be calculated

            ! Test cases:
            !      ifall :   -1 : Example from Brandt's textbook, Fig. 5.11, P. 113
            !               GUM Suppl. 2 examples:
            !      ifall :    0 : example in section 7.7.2,    Fig. 6
            !                 1 : example in section 9.2.2.1,  Table 3;
            !                 2 : example in section 9.2.3.2,  Table 4;
            !                 3 : example in section 9.2.4.1,  Table 5
            ifall = -1

            if(ifall == -1) then
                ! Example from Brandt, Fig. 5.11, Page 113
                valEGr = (/-ONE, -ONE, ZERO /)
                uncEGr = (/3._rn, TWO,ZERO /)
                rho = 0.7_rn
                W1minusG = 0.393_rn
            end if
            if(ifall == 0) then
                ! GUM S2: Example 2: Fig. 3b
                valEGr = (/ ZERO, ZERO, ZERO /)
                uncEGR = (/ sqrt(TWO), sqrt(TWO), ZERO/)
                rho = 1.9_rn/TWO
                W1minusG = 0.95_rn
            elseif(ifall == 1 .or. ifall == 2) then
                valEGr = (/ ZERO, ZERO, ZERO /)
                uncEGR = (/ sqrt(TWO), sqrt(TWO), ZERO/)
                rho = ONE/TWO
                W1minusG = 0.95_rn
                !if(ifall == 1) pltitle = 'Example 1: Table 3'
                !if(ifall == 2) pltitle = 'Example 2: Table 4'
            elseif(ifall == 3) then
                valEGr = (/ ZERO, ZERO, ZERO /)
                uncEGR = (/ sqrt(10._rn), sqrt(10._rn), ZERO/)
                rho = 9.0_rn/10._rn
                W1minusG = 0.95_rn
                ! pltitle = 'Example 3: Table 5'
            end if

            covEGr = ZERO
            covEGr(1,1) = uncEGr(1)**TWO
            covEGr(2,2) = uncEGr(2 )**TWO
            covEGr(1,2) = rho * uncEGr(1)*uncEGr(2)
            covEGr(2,1) = covEGr(1,2)
            corrEGr = ZERO
            do i=1,3
                corrEGr(i,i) = ONE
            end do
            corrEGr(1,2) = rho
            corrEGr(2,1) = rho

        end if
        !------------------------------------------------------------------------------------------!

        do i=1, 100
            call WTreeViewSetColorRow('treeviewELI', i, get_color_string('table_bg'))
        end do

        call WDPutTreeViewColumnLabel('treeviewELI', 2, trim(symbole(1)%s))
        call WDPutTreeViewColumnLabel('treeviewELI', 3, trim(symbole(2)%s))
        if(knumEGr == 3) then
            call WDPutTreeViewColumnLabel('treeviewELI', 4, trim(symbole(3)%s))
        else
            call WDPutTreeViewColumnLabel('treeviewELI', 4, ' ')
        end if

        ! first index: column; second index: row
        call WTreeViewPutStrCell('treeviewELI', 1, 1, 'values:')
        call WTreeViewPutStrCell('treeviewELI', 1, 2, 'StdDevs:')
        call WTreeViewPutStrCell('treeviewELI', 1, 3, 'Corr. matrix:')
        do j=1,3
            call WTreeViewPutStrCell('treeviewELI', 1+j, 3,' ')
        end do

        do j=1,knumEGr
            call WTreeViewPutDoubleCell('treeviewELI', j+1, 1, valEGr(j))
            call WTreeViewPutDoubleCell('treeviewELI', j+1, 2, uncEGr(j))
        end do

        do i=1,knumEGr
            xstr = max(' ',trim(symbole(i)%s))
            call WTreeViewPutStrCell('treeviewELI', 1, i+3, xstr)
            do j=1,knumEGr
                call WTreeViewPutDoubleCell('treeviewELI', j+1, i+3, corrEGr(i,j))
            end do
        end do

        call WDPutLabelString('checkbuttonELI_EG1', trim(symbole(1)%s))
        call WDPutLabelString('checkbuttonELI_EG2', trim(symbole(2)%s))
        if(knumEGr == 3) then
            call WDPutLabelString('checkbuttonELI_EG3', trim(symbole(3)%s))
        else
            call WDPutLabelString('checkbuttonELI_EG3', ' ')
        end if

        if(knumEGr >= 2) then
            call WDSetCheckButton('checkbuttonELI_EG1',1)
            call WDSetCheckButton('checkbuttonELI_EG2',1)
            call WDSetCheckButton('checkbuttonELI_EG3',0)
        end if
        call WDSetCheckButton('checkbuttonRS',0)
        call WDSetComboboxAct('comboboxGrELI',1)

    end subroutine PrepEli

    !----------------------------------------------------------------------------------------------!

    subroutine Confidoid()

        ! run a calculation of a confidence ellipse and display in the dialog.
        ! intermediate caluclated values are store in the module UR_eli, from
        ! the routine Plot_eli takes the necessary data for displaying the ellipse.

        ! uses: Trans3, Scal3, Rotate3, TwoNormalDist, Py_givenX0

        !   Copyright (C) 2014-2025  Günter Kanisch
        !------------------------------------------------------------------------------------------!

        use gtk,             only: gtk_widget_queue_draw, gtk_window_set_keep_above, &
                                   gtk_widget_show_all, gtk_container_get_children
        use gdk_pixbuf_hl,   only: hl_gdk_pixbuf_save
        use ur_general_globals, only: plot_ellipse,plot_confidoid
        use UR_Gleich_globals,  only: Symbole
        use UR_Linft,        only: valEGr,covEGr,igsel,eliRS
        use UR_DLIM,         only: W1minusG
        use UR_GaussInt,     only: xmean, ymean, ux, uy, rho
        use top,             only: FindItemS,idpt
        use Rout,            only: WDPutTreeViewColumnLabel,WTreeViewPutStrCell, &
                                   WTreeViewPutDoubleCell,WDPutLabelString,WDSetCheckButton, &
                                   pending_events
        use Brandt,          only: mtxchi,mtxchl,qchi2,gincgm
        use Num1,            only: sym_eigensolve
        use RND,             only: Rndu
        use PLsubs,          only: CairoPlplotPrepare
        use PLsubs,          only: PlotEli
        use gtk_draw_hl,     only: hl_gtk_drawing_area_get_gdk_pixbuf
        use UR_eli,          only: xachse, yachse, p, g1, w, alpha, a1, a2, a2, a3, a4, a5, &
                                   p1, p2, p3, areaElli, theta, thetab, scf, Ascale, Atrans, &
                                   Arot, Acomb_TR, Acomb_TRS
        use plplot,          only: plend
        implicit none
        !------------------------------------------------------------------------------------------!

        logical                  :: posdef
        integer                  :: np         ! Dimension of the ellipsoid (2 or 3)
        integer                  :: i, j, ni, nj, ier

        real(rn), allocatable    :: amat0(:,:),amat(:,:),Lmat(:,:),LmatInv(:,:),ccy(:,:)
        real(rn), allocatable    :: eigenval(:),vmat(:,:)

        !-------------------------------------------------------------------------------------
        plot_ellipse = .false.
        plot_confidoid = .true.

        np = 0
        do i=1,3
            if(igsel(i) == 1) then
                np = np + 1
                if(np == 1) then
                    xachse = trim(symbole(i)%s)
                    xmean = valEGr(i)
                end if
                if(np == 2) then
                    yachse = trim(symbole(i)%s)
                    ymean = valEGr(i)
                end if
            end if
        end do
        write(66,*) 'np=',np,' xmean=',sngl(xmean),'  eliRS=',eliRS,'  igsel=',igsel
        if(np < 2) return

        if(allocated(amat0))   deallocate(amat0)
        if(allocated(amat))    deallocate(amat)
        if(allocated(Lmat))    deallocate(Lmat)
        if(allocated(LmatInv)) deallocate(LmatInv)
        if(allocated(eigenval))  deallocate(eigenval)
        if(allocated(vmat))    deallocate(vmat)
        if(allocated(ccy))    deallocate(ccy)
        allocate(amat0(1:np,1:np))
        allocate(amat(1:np,1:np))
        allocate(Lmat(1:np,1:np))
        allocate(LmatInv(1:np,1:np))
        allocate(vmat(1:np,1:np),ccy(np,np))
        allocate(eigenval(1:np))

        p = W1minusG
        g1 = qchi2(p, np)
        w = gincgm(real(np,rn)/TWO, g1/TWO)
        write(66,*) ' Prob=',sngl(p),'   g1=',sngl(g1),'  w=',sngl(w)

        ni = 0
        do i=1,3
            if(igsel(i) == 0) cycle
            ni = ni + 1
            nj = 0
            do j=1,3
                if(igsel(j) == 0) cycle
                nj = nj + 1
                amat0(ni,nj) = covEGr(i,j)
                amat(ni,nj)  = covEGr(i,j)
            end do
        end do
        write(66,*) 'Matrix amat0:     ni=',ni,'   nj=',nj
        do i=1,np
            write(66,*) (sngl(amat0(i,j)),j=1,np)
        end do
        write(66,*) 'Matrix covEGr:'
        do i=1,np
            write(66,*) (sngl(covEGr(i,j)),j=1,np)
        end do

        call mtxchl(amat,Lmat, posdef)          ! amat is modified now!
        ! Lmat must be the lower triangular matrix (see GUM S.2, 5.3.2.4):
        if(abs(Lmat(2,1)) < EPS1MIN) Lmat = TRANSPOSE(Lmat)
        LmatInv = Lmat
        call mtxchi(LmatInv)
        write(66,*) 'Matrix Uy = amat0:'
        do i=1,np
            write(66,*) (sngl(amat0(i,j)),j=1,np)
        end do
        write(66,*) 'Inverse der Matrix L:'
        do i=1,np
            write(66,*) (sngl(LmatInv(i,j)),j=1,np)
        end do

        eigenval = ZERO

        ccy(1:np,1:np) = amat(1:np,1:np)
        call sym_eigensolve(np, ccy, np, eigenval, ier)
        write(66,*) 'Eigenvalues of the covariance matrix (unsorted):  ',(sngl(eigenval(i)),i=1,2)
        write(66,*) 'Half axes, from eigenvalues:  ',(sngl(sqrt(eigenval(i)*g1)),i=1,2),'  g1=',sngl(g1)

        ux = sqrt(amat0(1,1))
        uy = sqrt(amat0(2,2))
        rho = amat0(1,2) / ux / uy
        write(66,*) '  rho=',sngl(rho),'  ux=',sngl(ux),'  uy=',sngl(uy)

        alpha = PI/TWO
        ! alpha = zero
        a1 = ux**TWO*sin(TWO*alpha) + TWO*rho*ux*uy*sin(alpha)
        a2 = ux**TWO*cos(TWO*alpha) + TWO*rho*ux*uy*cos(alpha) + uy**TWO
        a3 = ux**TWO + TWO*rho*ux*uy*cos(alpha) + uy**TWO
        a4 = sqrt(a1**TWO + a2**TWO)
        a5 = TWO*sin(alpha)
        p1 = sqrt((a3+a4)/a5) * sqrt(g1)
        p2 = sqrt((a3-a4)/a5) * sqrt(g1)

        p1 = sqrt(eigenval(1)*g1)
        p2 = sqrt(eigenval(2)*g1)
        areaElli = p1 * p2 * PI
        write(66,*) '      Area : ',sngl(areaElli),'  Pi=',sngl(PI)

        if(np == 3) p3 = sqrt(eigenval(3)*g1)

        theta = atan(a1/a2) / TWO
        write(66,*) 'Half axes analytically:       ',sngl(p1), sngl(p2),'   angle theta(deg)=',sngl(theta*180._rn/PI)
        thetab = atan(TWO*rho*ux*uy/(ux**TWO-uy**TWO)) / TWO
        write(66,*) '    angle theta(deg) after Brandt; Bohm_Zech_DESY : ',sngl(thetab*180._rn/PI)
        theta = thetab
        if(theta < ZERO) theta = theta + PI

        !--------------------------------------------------------------------------

        scf = ONE
        if(eliRS == 1) then
            ! Re-scale:
            if(uy <= ux) then
                scf(1) = ONE
                scf(2) = uy/ux
            else
                scf(1) = ux/uy
                scf(2) = ONE
            end if
            scf(3) = ONE
            scf(4) = ONE
        end if

        ! Sequence: always S x R x T ! For the back transformation, considered now,
        ! the reversed sequence must be applied!
        !               S x R x T = Scale x Rotate x Translation

        call scal3(ONE/scf(1), ONE/scf(2), ONE, Ascale)
        call Trans3(-xmean, -ymean, ZERO, Atrans)
        call Rotate3(3, -theta, Arot)
        Acomb_TR = matmul(Atrans,Arot)          ! these are applied in PlotEli
        Acomb_TRS = matmul(Acomb_TR, Ascale)    !

        write(66,*) 'theta=',sngl(theta)

        !--------------------------------------------------------------------------------------
        call PlotEli()

        !--------------------------------------------------------------------------------------
        call gtk_widget_show_all(idpt('dialogELI'))
        call pending_events

        if(allocated(amat0))   deallocate(amat0)
        if(allocated(amat))    deallocate(amat)
        if(allocated(Lmat))    deallocate(Lmat)
        if(allocated(LmatInv)) deallocate(LmatInv)
        if(allocated(eigenval))  deallocate(eigenval)
        if(allocated(vmat))    deallocate(vmat)

    end subroutine Confidoid

!#############################################################################################

    subroutine Trans3(tx,ty,tz, A)

        ! Calculate three-dimensional axis translation-matrix A


        implicit none

        real(rn),intent(in)     :: tx,ty,tz
        real(rn),intent(out)    :: a(4,4)

        integer                 :: i
        a = ZERO
        do i=1,4
            a(i,i) = ONE
        end do
        a(1,4) = -tx
        a(2,4) = -ty
        a(3,4) = -tz

    end subroutine Trans3

!#############################################################################################

    pure subroutine Scal3(sx,sy,sz, A)

        ! Calculate three-dimensional axis translation-matrix A

        implicit none

        real(rn),intent(in)     :: sx,sy,sz
        real(rn),intent(out)    :: a(4,4)

        a = ZERO
        a(1,1) = sx
        a(2,2) = sy
        a(3,3) = sz
        a(4,4) = ONE

    end subroutine Scal3

!#############################################################################################

    subroutine Rotate3(m,theta, A)

        ! Calculate three-dimensional rotation matrix for a rotation around the axis m
        ! m=1: x-axis;  m=2: y-axis;  m=3:  z-axis;

        implicit none

        integer, intent(in)  :: m         ! axis number
        real(rn),intent(in)  :: theta     ! rotation angle
        real(rn),intent(out) :: a(4,4)

        integer            :: m1, m2
        real(rn)           :: ct, st

        a = ZERO
        a(4,4) = ONE
        a(m,m) = ONE
        m1 = MOD(M,3) + 1
        m2 = MOD(m1,3) + 1
        ct = cos(theta)
        st = sin(theta)
        a(m1,m1) = ct
        a(m2,m2) = ct
        a(m1,m2) = st
        a(m2,m1) = -st

    end subroutine Rotate3

!#############################################################################################

end module Celli
