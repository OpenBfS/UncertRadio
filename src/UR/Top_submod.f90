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

submodule (top)  TOPA


contains


    module function idpt(strid) result(ptr)

        ! finds the c-pointer value of the GUI widget with id name string strid
        ! from the structure clobj derived from the Glade file

        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_gtk_variables,  only: clobj, nclobj
        implicit none

        type(c_ptr)                   :: ptr
        character(len=*),intent(in)   :: strid      ! Name des ID-Labels aus dem Glade-File

        integer                       :: i

        ! Flo: tdb
        do i = 1, nclobj
            if(clobj%idd(i)%s == strid) then
                ptr = clobj%id_ptr(i)
                exit
            end if
        end do

        if (i > nclobj) then
            write(66,*) 'Warnung: IDPT:  ', trim(strid), ': is not connected!'
            write(*,*) 'Warnung: IDPT:  ', trim(strid), ': is not connected!'
        end if

    end function idpt

!#############################################################################################

    module subroutine FindItemP(ptr, ncitem)

        ! finds for the given c-pointer value ptr the associated index number
        ! ncitem within the structure clobj derived from the Glade file

        !     Copyright (C) 2014-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,     only: c_ptr,c_null_ptr,c_associated
        use UR_gtk_variables,  only: clobj,nclobj
        implicit none

        type(c_ptr),value,intent(in)  :: ptr
        integer   ,intent(out)        :: ncitem

        integer           :: i
!---------------------------------------------------------------
        ncitem = 0
        if(.not.c_associated(ptr)) return

        do i=1,nclobj
            if(c_associated(ptr, clobj%id_ptr(i))) then
                ncitem = i
                exit
            end if
        end do

    end subroutine FindItemP

!#############################################################################################

    module subroutine FindItemS(dialogstr, ncitem)

        ! finds for the given dialogstr the associated index number
        ! ncitem within the structure clobj derived from the Glade file

        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_gtk_variables,  only: clobj,nclobj
        implicit none

        character(len=*),intent(in)  :: dialogstr
        integer   ,intent(out)       :: ncitem

        integer           :: i
!---------------------------------------------------------------
        ncitem = 0
        do i=1,nclobj
            if(trim(dialogstr) == clobj%idd(i)%s) then
                ncitem = i
                exit
            end if
        end do

    end subroutine FindItemS
!-----------------------------------------------------------------

!#############################################################################################
!
    module SUBROUTINE FieldUpdate()
        !
        !   This routine enables/disables the GUI menu items needed for saveing
        !   the project
        !
        use, intrinsic :: iso_c_binding
        use UR_Gleich,      only: ngrs, nab
        use UR_VARIABLES,   only: savep, FileTyp, langg
        use gtk,            only: gtk_widget_set_sensitive

        IMPLICIT NONE

        select case (Filetyp)
          case ('P')
            IF (SAVEP) THEN
                if(ngrs > 0 .and. nab > 0) then
                    call gtk_widget_set_sensitive(idpt('MenuSaveProject'), 1_c_int)
                    call gtk_widget_set_sensitive(idpt('MenuSaveProjectAs'), 1_c_int)
                    call gtk_widget_set_sensitive(idpt('TBSaveProject'), 1_c_int)
                    call gtk_widget_set_sensitive(idpt('TBSaveProjectAs'), 1_c_int)
                    if(langg == 'DE') Call WrStatusbar(3,'ungesichert')
                    if(langg == 'EN') Call WrStatusbar(3,'unsaved!')
                    if(langg == 'FR') Call WrStatusbar(3,'pas enregistré!')
                end if
            ELSE
                call gtk_widget_set_sensitive(idpt('MenuSaveProject'), 0_c_int)
                call gtk_widget_set_sensitive(idpt('TBSaveProject'), 0_c_int)
                call gtk_widget_set_sensitive(idpt('MenuSaveProjectAs'), 0_c_int)   ! 13.4.2023
                Call WrStatusbar(3,' ')
            END IF
        end select

    END SUBROUTINE FieldUpdate

!###############################################################################

    module subroutine WrStatusbar(k,string)

        ! writes text (string) into the statusbar number k
        !     Copyright (C) 2014-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,      only: c_ptr,c_int,c_null_char
        use gtk,                only: gtk_statusbar_get_context_id, gtk_statusbar_push, &
                                      gtk_statusbar_remove_all
        use UR_VARIABLES,       only: BATF,bat_serial,automode,autoreport,batest_user,batest_on, &
                                      bat_mc

        implicit none

        integer, intent(in)            :: k       ! statusbar number
        character(len=*), intent(in)   :: string  ! output text

        character(len=1)         :: strn
        integer(c_int)           :: context_id,res
        character(len=100)       :: strg
        !----------------------------------------------------------
        if(batf .or. bat_serial .or. autoreport .or. batest_user .or. &
            batest_on .or. bat_MC  .or. automode ) return

        write(strn,'(i1)') k
        context_id = gtk_statusbar_get_context_id (idpt('statusbar'//strn),''//c_null_char)
        ! Note: with the time, a stack begins to acculuate!
        call gtk_statusbar_remove_all(idpt('statusbar'//strn), context_id)
        strg = trim(string)
        res = gtk_statusbar_push(idpt('statusbar'//strn),context_id, trim(strg)//c_null_char)

    end subroutine WrStatusbar

!-----------------------------------------------------------------------------------------
!#######################################################################

    module real(rn) FUNCTION dpafact(x)

    ! this function is required when partial derivatives are to be calculated
    ! numerically: for a derivative with respect to x, it defines the small
    ! increment dpa of that variable (x+dpa) in the difference quotient.
    ! To avoid numerical problems, the size of dpa increases, the closer x
    ! approaches very small values.

    use UR_Gleich,     only: increase_dpafact

    implicit none

    real(rn),intent(in)    :: x    !

    ! dpafact = 1.000002d0
    if(rn == 8) then
        dpafact = 1.000001_rn
        if(abs(x) < 5.E-6_rn)  dpafact = 1.00002_rn
        if(abs(x) < 1.E-7_rn)  dpafact = 1.00005_rn
        if(abs(x) < 1.E-8_rn)  dpafact = 1.0001_rn
        if(abs(x) < 1.E-9_rn)  dpafact = 1.0005_rn
        if(abs(x) < 1.E-10_rn) dpafact = 1.005_rn
        if(abs(x) < 1.E-11_rn) dpafact = 1.01_rn
    elseif(rn == 10) then
        dpafact = 1.000000001_rn
        if(abs(x) < 5.E-9_rn)  dpafact = 1.00000002_rn
        if(abs(x) < 1.E-10_rn)  dpafact = 1.00000005_rn
        if(abs(x) < 1.E-11_rn)  dpafact = 1.0000001_rn
        if(abs(x) < 1.E-12_rn)  dpafact = 1.0000005_rn
        if(abs(x) < 1.E-13_rn) dpafact = 1.000005_rn
        if(abs(x) < 1.E-14_rn) dpafact = 1.00001_rn
    elseif(rn == 16) then
        dpafact = 1.000000001_rn
        if(abs(x) < 5.E-9_rn)  dpafact = 1.000000002_rn
        if(abs(x) < 1.E-10_rn)  dpafact = 1.000000005_rn
        if(abs(x) < 1.E-11_rn)  dpafact = 1.00000001_rn
        if(abs(x) < 1.E-12_rn)  dpafact = 1.00000005_rn
        if(abs(x) < 1.E-13_rn) dpafact = 1.0000005_rn
        if(abs(x) < 1.E-14_rn) dpafact = 1.000001_rn
    end if
    if(increase_dpafact) dpafact = 1.075_rn

    END function dpafact

    !#######################################################################

    module logical function chupper_eq(str1, str2)

        ! this function test the equality of the two strings when using
        ! upper case versions.
        !     Copyright (C) 2014-2023  Günter Kanisch

        use CHF,           only: ucase
        implicit none

        CHARACTER(LEN=*),INTENT(IN) :: Str1
        CHARACTER(LEN=*),INTENT(IN) :: Str2

        chupper_eq = trim(ucase(str1)) == trim(ucase(str2))
        return

    end function chupper_eq

    !#######################################################################

    module subroutine MDcalc(k_datvar)

        ! this routine prepares and calculates mean and standard uncertainties
        ! of one of the user-supplied data sets, with the number k_datvar. The
        ! Bayesian requirements which do not allow averaging of < 4 values, as
        ! laid out in ISO 11929 (2019), is taken into account.
        !
        ! See chapter 6.9 of the UncertRadio CHM Helpfile for more information.
        !
        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_params,      only: rn, one, two
        use UR_Gleich,      only: nvalsMD, meanMD, umeanMD, fbayMD, k_MDtyp, nvarsMD, &
                                  nvMD, ifehl, smeanMD, DistPars, MDpoint, Symbole, &
                                  IVTL, refdataMD, rinflu_known, theta_ref,  &
                                  xdataMD, ixdanf
        use UR_VARIABLES,   only: Gum_restricted
        use Brandt,         only: mean, sd
        use cHF,            only: FindlocT

        implicit none

        integer, intent(in) :: k_datvar

        integer             :: nv, ivt, nn, ks, kd
        real(rn)            :: xnv, xq, s0

        if(k_datvar <= 0) return

        ifehl = 0
        nv = nvalsMD(k_datvar)
        xnv = nv
        if(nv == 0) return
        fbayMD(k_datvar) = one
        nvMD(k_datvar) = xnv

        xq  = mean(xdataMD(ixdanf(k_datvar):ixdanf(k_datvar)+nv-1))
        s0 = sd(xdataMD(ixdanf(k_datvar):ixdanf(k_datvar)+nv-1))
        write(66,*) 'mean=xq=',sngl(xq),'  sd=s0=',sngl(s0),' k_MDtyp=',k_MDtyp(k_datvar), &
            ' nv=',int(nv,2)
        smeanMD(k_datvar) = s0
        meanMD(k_datvar) = xq
        if(Gum_restricted) then
            umeanMD(k_datvar) = s0 /sqrt(xnv)
        else
            ! ISO 11929
            ! if(k_MDtyp(k_datvar) <= 3) then
            if(nv <= 3) then
                write(66,*) 'MDCalc: less than 4 single values not allowed!   ifehl=1'
                ifehl = 1
            end if
            if(nv > 3) then
                ifehl = 0
                fbayMD(k_datvar) =  (xnv-one)/(xnv-3.0_rn) / xnv
                if(k_MDtyp(k_datvar) == 1 ) then
                    umeanMD(k_datvar)  = s0*sqrt(fbayMD(k_datvar))
                elseif(k_MDtyp(k_datvar) == 2 ) then
                    umeanMD(k_datvar)  = xq + (xnv-one)/(xnv-3.0_rn) * (xq + s0**two)
                    umeanMD(k_datvar) = sqrt(umeanMD(k_datvar)/xnv)
                    fBayMD(k_datvar) = one
                elseif(k_MDtyp(k_datvar) == 3 ) then
                    umeanMD(k_datvar) = s0 / sqrt(xnv)
                    fbayMD(k_datvar) = one/xnv
                end if
                !write(66,*) 'k_MDtyp(k_datvar)=',int(k_MDtyp(k_datvar),2),' fBay=',sngl(fbayMD(k_datvar)), &
                !            '  umeanMD(k_datvar)=',sngl(umeanMD(k_datvar))
            else
                if(k_MDtyp(k_datvar) >= 2 ) then
                    umeanMD(k_datvar) = s0 / sqrt(xnv)
                    fbayMD(k_datvar) = one
                end if
                ! write(66,*) 'meanMD=',sngl(meanMD(k_datvar)),'  umeanMD=',sngl(umeanMD(k_datvar)),  &
                !          ' k_datvar=',int(k_datvar,2),' k_MDtyp(k_datvar)=',int(k_MDtyp(k_datvar),2), &
                !          ' nv=',int(nv,2)
            end if
        end if

        ks = MDPoint(k_datvar)
        if(ivtl(ks) == 9) then
            ! for t-distribution:
            ks = MDPoint(k_datvar)
            ivt = 9
            nn = k_datvar
            if(.not. allocated(Distpars%symb)) then
                allocate(Distpars%symb(1))
            end if
            if(nn > ubound(Distpars%symb,dim=1))  &
                call CharModA1(Distpars%symb,nn)
            DistPars%ivtl(nn) = ivt
            DistPars%symb(nn)%s = Symbole(ks)%s
            DistPars%pval(nn,1) = nvMD(k_datvar)-one
            DistPars%pval(nn,2) = meanMD(k_datvar)
            DistPars%pval(nn,3) = smeanMD(k_datvar)    ! /sqrt(nvMD(k_datvar))
            IVTL(ks) = 9

            ! write(66,*) 'k_datvar=',int(k_datvar,2),'  symb=',Symbole(ks)%s
            ! write(66,*) 'Distpars%symb=',(Distpars%symb(i)%s,' ',i=1,ubound(Distpars%symb,dim=1))

        end if
    ! if(rinflu_known) then
        if(rinflu_known .and. kd <= nvarsMD) then   !  12.8.2023
            ! for small random influences:
            kd = refdataMD
            nv = nvalsMD(kd)
            xq  = mean(xdataMD(ixdanf(kd):ixdanf(kd)+nv-1))
            s0 = sd(xdataMD(ixdanf(kd):ixdanf(kd)+nv-1))
            theta_ref = ( s0**two - xq ) / xq**two
            theta_ref = sqrt(theta_ref)
            ! write(66,*) 'rinflu_known: theta_ref=',sngl(theta_ref)
        end if


    end subroutine MDcalc

    !#######################################################################

    module subroutine PixelPerString(dialog,string,wdpixel,htpixel)

        ! this routine determines the length (wdpixel) and height (htpixel) of
        ! the string, in pixel, as appearing on the given dialog, from which the
        ! pixel width and height per character can be derived.

        !     Copyright (C) 2014-2023  Günter Kanisch

        use, intrinsic :: iso_c_binding,    only: c_ptr,c_null_char,c_int,c_loc,c_associated,c_null_ptr
        use pango,     only: pango_font_description_new, pango_font_description_set_style, &
            pango_font_description_set_stretch, pango_font_description_set_size, &
            pango_layout_new, pango_layout_set_text, pango_font_description_set_family, &
            pango_layout_set_font_description, pango_layout_get_pixel_size

        use gtk,       only: PANGO_STYLE_NORMAL,PANGO_STRETCH_NORMAL,gtk_widget_get_pango_context
        use g,         only: g_object_unref
        use UR_gtk_variables, only: fontname

        implicit none

        type(c_ptr), value           :: dialog
        character(len=*),intent(in)  :: string
        integer,intent(out)          :: wdpixel,htpixel

        type(c_ptr)            :: pfd,pcontext,playout
        integer(c_int),target  :: wdret,htret
        character(len=60)      :: family
        integer                :: i1


        family = fontname   ! 'Sans'
        i1 = index(family,' ')
        !!    if(i1 > 1) family = family(1:i1-1)
    ! ptsize = 10
        pfd = c_null_ptr
        if(.not.c_associated(pfd)) pfd = pango_font_description_new()
        ! write(66,*) 'pfd=',pfd
        call pango_font_description_set_family(pfd,family//c_null_char)
        call pango_font_description_set_style(pfd, PANGO_STYLE_NORMAL)

        call pango_font_description_set_stretch(pfd, PANGO_STRETCH_NORMAL)

        pcontext = gtk_widget_get_pango_context(dialog)
        ! write(66,*) 'pcontext=',pcontext
        playout = pango_layout_new(pcontext)
        call pango_layout_set_text(playout, trim(string)//c_null_char, -1_c_int)
        call pango_layout_set_font_description(playout, pfd)

        call pango_layout_get_pixel_size(playout, c_loc(wdret) , c_loc(htret) )
        call g_object_unref(playout);
        wdpixel = wdret
        htpixel = htret
        ! write(66,*) 'wdpixel=',wdpixel,' htpixel=',htpixel

    end subroutine PixelPerString

    !########################################################################################

    module subroutine CharModA1(array,n1)

        ! this routine allocates memory for n1 records of a one-dimensional
        ! variable size character array of type(charv).  move_alloc is used.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_Gleich,    only: charv
        implicit none

        integer   ,intent(in)                 :: n1
        type(charv),allocatable,intent(inout) :: array(:)

        type(charv),allocatable  :: vvv(:)

        integer    :: i,ix,mix
        character(len=20)   :: sbuf

        sbuf = ' '
        if(.not.allocated(array)) then
            allocate(array(n1))
            do i=1,n1
                array(i)%s = sbuf(1:1)  !
            end do
            return
        end if
        ix = ubound(array,dim=1)
        if(n1 == ix) return

        allocate(vvv(n1))
        mix = min(ix,n1)
        do i=1,mix
            vvv(i)%s = array(i)%s
        end do
        if(n1 > ix) then
            do i=ix+1,n1
                vvv(i)%s = sbuf(1:1)
            end do
        end if
        if(allocated(array)) deallocate(array)
        call move_alloc(vvv, array)

    end subroutine CharModA1

    !#######################################################################

    module subroutine CharModA2(array,n1,n2)         ! ,istep)

        ! this routine allocates memory for dim numbers n1 and n2 of a two-dimensional
        ! variable size character array(n1,n2) of type(charv). move_alloc is used.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_Gleich,    only: charv
        implicit none

        integer, intent(in)                     :: n1,n2
        type(charv), allocatable, intent(inout) :: array(:,:)

        type(charv), allocatable  :: vvv(:,:)

        integer             :: i,ix1,ix2,j,mix1,mix2
        character(len=20)   :: sbuf

        sbuf = ' '
        if(.not.allocated(array)) then
            allocate(array(n1,n2))
            do i=1,n1
                do j=1,n2
                    array(i,j)%s = sbuf(1:1)
                end do
            end do
            return
        end if
        ix1 = ubound(array,dim=1)
        ix2 = ubound(array,dim=2)

        !write(65,*) 'CharModA2: anf: ix1=',int(ix1,2),' ix2=',int(ix2,2),' n1,n2=',n1,n2
        !do i=1,ix1
        !  write(65,*) (trim(array(i,k)%s),' ; ',k=1,ix2)
        !end do

        if(n1 == ix1 .and. n2 == ix2) return

        allocate(vvv(n1,n2))
        mix1 = min(ix1,n1)
        mix2 = min(ix2,n2)
        do i=1,mix1
            do j=1,mix2
                vvv(i,j)%s = array(i,j)%s
            end do
        end do
        if(n1 > ix1) then
            do i=ix1+1,n1
                do j=ix2+1,n2
                    vvv(i,j)%s = sbuf(1:1)
                end do
            end do
        end if
        if(allocated(array)) deallocate(array)
        call move_alloc(vvv, array)

        !write(65,*) 'CharModA2_after: '
        !do i=1,n1
        !  write(65,*) (array(i,k)%s,' ; ',k=1,n2)
        !end do

    end subroutine CharModA2


    !#######################################################################

    module subroutine IntModA1(array,n1)

        ! this routine allocates memory for dim n1 one-dimensional
        ! integer array.  move_alloc is used.

        !     Copyright (C) 2020-2023  Günter Kanisch

        implicit none

        integer   ,intent(in)    :: n1
        integer   ,allocatable   :: array(:),vvv(:)

        integer         :: ix

        if(.not.allocated(array)) then
            allocate(array(n1))
            array = 1
            return
        end if
        ix = ubound(array,dim=1)

        if(n1 == ix) return

        if(n1 > ix) then
            allocate(vvv(n1))
            if(ix == 0) then
                vvv = 0
                call move_alloc(vvv, array)
            else
                vvv(1:ix) = array(1:ix)
                vvv(ix+1:n1) = 0
                call move_alloc(vvv, array)
            end if
        elseif(n1 < ix) then
            if(n1 >= 1) then
                allocate(vvv(n1))
                vvv(1:n1) = array(1:n1)
                if(allocated(array)) deallocate(array)
                call move_alloc(vvv, array)
            end if
        end if

    end subroutine IntModA1

    !#######################################################################

    module subroutine RealModA1(array,n1,kmin)

        ! this routine allocates memory for dim n1 one-dimensional
        ! real(rn) array.  move_alloc is used.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,      only: rn,zero
        implicit none

        integer, intent(in)             :: n1
        integer, intent(in), optional   :: kmin
        real(rn), allocatable           :: array(:), vvv(:)

        integer              :: ix, i0
        real(rn)             :: x0

        x0 = -199._rn
        if(.not.allocated(array)) then
            allocate(array(n1))
            array = zero
            return
        end if
        ix = ubound(array,dim=1)
        i0 = lbound(array,dim=1)
        ! if(present(kmin) .and. ix < 8) write(28,*) 'n1=',int(n1,2),' i0=',int(i0,2),' ix=',int(ix,2),'  array(i0:ix)=',sngl(array(i0:ix))

        if(present(kmin)) then
            x0 = array(i0)
            i0 = kmin
        end if

        if(n1 == ix .and. .not.present(kmin)) return

    ! if(n1 > ix) then
        if(n1 >= ix) then
            allocate(vvv(i0:n1))
            if(ix == 0) then
                vvv(i0:n1) = zero
                call move_alloc(vvv, array)
                if(x0 > -199._rn .and. i0 == 0) array(0) = x0
            else
                vvv(i0:ix) = array(i0:ix)
                vvv(ix+1:n1) = zero
                call move_alloc(vvv, array)
                if(x0 > -199._rn .and. i0 == 0) array(0) = x0
            end if
            ! if(present(kmin) .and. ix < 8) write(28,*) '       am Ende: array(i0:ix)=',sngl(array(i0:ix))
        elseif(n1 < ix) then
            if(n1 >= i0) then
                allocate(vvv(i0:n1))
                vvv(i0:n1) = array(i0:n1)
                if(allocated(array)) deallocate(array)
                call move_alloc(vvv, array)
                if(x0 > -199._rn .and. i0 == 0) array(0) = x0
            end if
        end if

    end subroutine RealModA1

    !#######################################################################

    module subroutine LogModA1(array, n1)

        ! this routine allocates memory for dim n1 one-dimensional
        ! logical array.  move_alloc is used.

        !     Copyright (C) 2020-2023  Günter Kanisch

        implicit none

        integer, intent(in)       :: n1
        logical, allocatable      :: array(:), vvv(:)

        integer      :: ix

        if(.not.allocated(array)) then
            allocate(array(n1))
            array = .false.
            return
        end if
        ix = ubound(array,dim=1)

        if(n1 == ix) return

        if(n1 > ix) then
            allocate(vvv(n1))
            if(ix == 0) then
                vvv = .false.
                call move_alloc(vvv, array)
            else
                vvv(1:ix) = array(1:ix)
                vvv(ix+1:n1) = .false.
                call move_alloc(vvv, array)
            end if
        elseif(n1 < ix) then
            if(n1 >= 1) then
                allocate(vvv(n1))
                vvv(1:n1) = array(1:n1)
                if(allocated(array)) deallocate(array)
                call move_alloc(vvv, array)
            end if
        end if

    end subroutine LogModA1

    !#######################################################################

    module subroutine InitVarsTV2_CP(nng)

        ! this routine allocates memory, and initiates for dim=nng,
        ! for the *_CP versions (copies) of the Treeview2 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,     only: rn
        use UR_gleich,     only: Symbole_CP,symtyp_CP,einheit_CP,bedeutung_CP,Messwert_CP,IVTL_CP,  &
            SDFormel_CP,SDwert_CP,HBreite_CP,IAR_CP,StdUnc_CP,missingval, &
            Sensi_CP,perc_CP

        implicit none

        integer   ,intent(in)    :: nng

        integer          :: i
        real(rn),allocatable  :: rr(:)

        allocate(rr(nng))
        rr(1:nng) = missingval

        allocate(Symbole_CP(nng),symtyp_CP(nng),einheit_CP(nng),bedeutung_CP(nng))
        allocate(IVTL_CP(nng),SDFormel_CP(nng))
        allocate(Messwert_CP,source=rr)
        Allocate(IAR_CP(nng))
        allocate(SDwert_CP,source=rr)
        allocate(HBreite_CP,source=rr)
        allocate(StdUnc_CP,source=rr)
        allocate(Sensi_CP,source=rr)
        allocate(perc_CP,source=rr)

        do i=1,nng
            Symbole_CP(i)%s = ' '
            symtyp_CP(i)%s = ' '
            einheit_CP(i)%s = ' '
            Bedeutung_CP(i)%s = ' '
            SDFormel_CP(i)%s = ' '
        end do
        Messwert_CP(1:nng) = missingval
        IVTL_CP(1:nng) = 1
        SDWert_CP(1:nng) = missingval
        HBreite_CP(1:nng) = missingval
        IAR_CP(1:nng) = 1
        Stdunc_CP(1:nng) = missingval
        Sensi_CP(1:nng) = missingval
        perc_CP(1:nng) = missingval

        deallocate(rr)

    end subroutine InitVarsTV2_CP

    !#######################################################################


    module subroutine InitVarsTV2(nng)

        ! this routine allocates memory, and initiates for dim=nng,
        ! for the original versions of the Treeview2 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,     only: rn
        use UR_gleich,     only: Symbole,symtyp,einheit,bedeutung,Messwert,IVTL,  &
            SDFormel,SDwert,HBreite,IAR,StdUnc,missingval,MesswertSV, &
            Sensi,perc

        implicit none

        integer   ,intent(in)    :: nng

        integer          :: i
        real(rn),allocatable      :: rr(:)

        allocate(rr(nng))
        rr(1:nng) = missingval

        allocate(Symbole(nng),symtyp(nng),einheit(nng),bedeutung(nng))
        allocate(IVTL(nng),SDFormel(nng))
        allocate(Messwert,source=rr)
        Allocate(IAR(nng))
        allocate(SDwert,source=rr)
        allocate(HBreite,source=rr)
        allocate(StdUnc,source=rr)
        allocate(MesswertSV,source=rr)
        allocate(Sensi,source=rr)
        allocate(perc,source=rr)

        do i=1,nng
            Symbole(i)%s = ' '
            symtyp(i)%s = ' '
            einheit(i)%s = ' '
            Bedeutung(i)%s = ' '
            SDFormel(i)%s = ' '
        end do
        Messwert(1:nng) = missingval
        IVTL(1:nng) = 1
        SDWert(1:nng) = missingval
        HBreite(1:nng) = missingval
        IAR(1:nng) = 1
        Stdunc(1:nng) = missingval
        MesswertSV(1:nng) = missingval
        Sensi(1:nng) = missingval
        perc(1:nng) = missingval

        deallocate(rr)

    end subroutine InitVarsTV2

    !#######################################################################

    module subroutine InitVarsTV5_CP(kxy)

        ! this routine allocates memory and initiates for dim=kxy
        ! of the *_CP versions (copies) of the Treeview5 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,    only: rn
        use UR_gleich,    only: missingval
        use UR_Linft,     only: dmesszeit_CP,dbimpulse_CP,dbzrate_CP,sdbzrate_CP,d0messzeit_CP, &
            d0impulse_CP,d0zrate_CP,sd0zrate_CP,dnetrate_CP,sdnetrate_CP, &
            CStartzeit_CP

        implicit none

        integer, intent(in)    :: kxy
        integer                :: i
        real(rn), allocatable  :: rr(:)

        allocate(rr(kxy))
        rr(1:kxy) = missingval

        allocate(dmesszeit_CP,source=rr)
        allocate(dbimpulse_CP,source=rr)
        allocate(dbzrate_CP,source=rr)
        allocate(sdbzrate_CP,source=rr)
        allocate(d0messzeit_CP,source=rr)
        allocate(d0impulse_CP,source=rr)
        allocate(d0zrate_CP,source=rr)
        allocate(sd0zrate_CP,source=rr)
        allocate(dnetrate_CP,source=rr)
        allocate(sdnetrate_CP,source=rr)
        allocate(CStartzeit_CP(kxy))

    !dmesszeit_CP(1:kxy) = zero; dbimpulse_CP(1:kxy) = zero; dbzrate_CP(1:kxy) = zero;
    !sdbzrate_CP(1:kxy) = zero; d0messzeit_CP(1:kxy) = zero; d0impulse_CP(1:kxy) = zero;
    !d0zrate_CP(1:kxy) = zero; sd0zrate_CP(1:kxy) = zero; dnetrate_CP(1:kxy) = zero;
    !sdnetrate_CP(1:kxy) = zero;
        do i=1,kxy
            CStartzeit_CP(i)%s = ' '
        end do

        deallocate(rr)

    end subroutine InitVarsTV5_CP

    !#######################################################################

    module subroutine InitVarsTV5(kxy)

        ! this routine allocates memory and initiates for dim=kxy
        ! of the original versions of the Treeview5 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,    only: zero,rn
        use UR_gleich,    only: missingval
        use UR_Linft,     only: dmesszeit,dbimpulse,dbzrate,sdbzrate,d0messzeit, &
            d0impulse,d0zrate,sd0zrate,dnetrate,sdnetrate, &
            CStartzeit,d0zrateSV,sd0zrateSV,dtdiff

    ! use UR_MCSR,      only: netfit,rnetvar

        implicit none

        integer   ,intent(in)    :: kxy
        integer                 :: i
        real(rn),allocatable      :: rr(:)

        allocate(rr(kxy))
        rr(1:kxy) = missingval

        allocate(dmesszeit,source=rr)
        allocate(dbimpulse,source=rr)
        allocate(dbzrate,source=rr)
        allocate(sdbzrate,source=rr)
        allocate(d0messzeit,source=rr)
        allocate(d0impulse,source=rr)
        allocate(d0zrate,source=rr)
        allocate(sd0zrate,source=rr)
        allocate(dnetrate,source=rr)
        allocate(sdnetrate,source=rr)
        allocate(CStartzeit(kxy))

    !dmesszeit_CP(1:kxy) = zero; dbimpulse_CP(1:kxy) = zero; dbzrate_CP(1:kxy) = zero;
    !sdbzrate_CP(1:kxy) = zero; d0messzeit_CP(1:kxy) = zero; d0impulse_CP(1:kxy) = zero;
    !d0zrate_CP(1:kxy) = zero; sd0zrate_CP(1:kxy) = zero; dnetrate_CP(1:kxy) = zero;
    !sdnetrate_CP(1:kxy) = zero;
        do i=1,kxy
            CStartzeit(i)%s = ' '
        end do

        if(allocated(sd0zrateSV)) deallocate(sd0zrateSV,d0zrateSV)
        allocate(sd0zrateSV,source=rr)
        allocate(d0zrateSV, source=rr)

        if(allocated(dtdiff)) deallocate(dtdiff)
        allocate(dtdiff(kxy),source=rr)
        dtdiff(1:kxy) = zero

        if(allocated(sd0zrateSV)) deallocate(sd0zrateSV,d0zrateSV)
        allocate(sd0zrateSV(kxy),d0zrateSV(kxy))
        sd0zrateSV(1:kxy)= zero; d0zrateSV(1:kxy) = zero

        if(allocated(dtdiff)) deallocate(dtdiff)
        allocate(dtdiff(kxy))
        dtdiff(1:kxy) = zero

        deallocate(rr)

    end subroutine InitVarsTV5

    !#######################################################################

    module subroutine InitVarsTV3(ncov)

        ! this routine allocates memory and initiates for dim=ncov
        ! of the original versions of the Treeview3 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,    only: rn
        use UR_Gleich,    only: IsymbA, IsymbB,icovtyp,CVFormel,CovarVal,CorrVal,CovarvalSV, &
            missingval

        implicit none

        integer, intent(in)     :: ncov

        integer                 :: i
        real(rn), allocatable   :: rr(:)

        allocate(rr(ncov))
        rr(1:ncov) = missingval

        allocate(IsymbA(ncov),IsymbB(ncov),icovtyp(ncov),cvformel(ncov))
        allocate(covarval, source=rr)
        allocate(corrval, source=rr)
        allocate(covarvalSV, source=rr)

        IsymbA(1:ncov) = 0; IsymbB(1:ncov) = 0; icovtyp(1:ncov) = 1;
        covarval(1:ncov) = missingval; corrval(1:ncov) = missingval; covarvalSV(1:ncov) = missingval;
        do i=1,ncov
            CVFormel(i)%s = ' '
        end do
        ! write(0,*) 'ITV3: sizes : covarval:',int(size(covarval),2),' corrval: ',int(size(covarval),2)

        deallocate(rr)

    end subroutine InitVarsTV3

    !#######################################################################

    module subroutine InitVarsTV3_CP(ncov)

        ! this routine allocates memory and initiates for dim=ncov
        ! of the *_CP versions (copies) of the Treeview3 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,    only: rn
        use UR_Gleich,    only: IsymbA_CP, IsymbB_CP,icovtyp_CP,CVFormel_CP,CovarVal_CP, &
            missingval
        implicit none

        integer, intent(in)     :: ncov

        integer                 :: i
        real(rn), allocatable   :: rr(:)

        allocate(rr(ncov))
        rr(1:ncov) = missingval

        allocate(IsymbA_CP(ncov),IsymbB_CP(ncov),icovtyp_CP(ncov),cvformel_CP(ncov))
        allocate(covarval_CP, source=rr)

        IsymbA_CP(1:ncov) = 0; IsymbB_CP(1:ncov) = 0; icovtyp_CP(1:ncov) = 1;
        covarval_CP(1:ncov) = missingval;
        do i=1,ncov
            CVFormel_CP(i)%s = ' '
        end do

        deallocate(rr)

    end subroutine InitVarsTV3_CP

    !#######################################################################


    module subroutine InitVarsTV6_CP(kxy)

        ! this routine allocates memory and initiates for dim=kxy
        ! of the *_CP versions (copies) of the Treeview6 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_Gspk1Fit,  only: guse_CP,erg_CP,GnetRate_CP,RateCB_CP,RateBG_CP,SDRateBG_CP, &
                                effi_CP,SDeffi_CP,pgamm_CP,SDpgamm_CP,fatt_CP,SDfatt_CP,    &
                                fcoinsu_CP,SDfcoinsu_CP
        use UR_Gleich,    only: missingval
        implicit none

        integer, intent(in)    :: kxy


        allocate(guse_CP(kxy),erg_CP(kxy),GnetRate_CP(kxy),RateCB_CP(kxy),RateBG_CP(kxy))
        allocate(SDRateBG_CP(kxy),effi_CP(kxy),SDeffi_CP(kxy),pgamm_CP(kxy),SDpgamm_CP(kxy))
        allocate(fatt_CP(kxy),SDfatt_CP(kxy),fcoinsu_CP(kxy),SDfcoinsu_CP(kxy))

        guse_CP(1:kxy) = 0; erg_CP(1:kxy) = missingval; GNetRate_CP(1:kxy) = missingval;
        RateCB_CP(1:kxy) = missingval; RateBG_CP(1:kxy) = missingval; SDRateBG_CP(1:kxy) = missingval;
        effi_CP(1:kxy) = missingval; sdeffi_CP(1:kxy) = missingval; pgamm_CP(1:kxy) = missingval;
        sdpgamm_CP(1:kxy) = missingval; fatt_CP(1:kxy) = missingval; SDfatt_CP(1:kxy) = missingval;
        fcoinsu_CP(1:kxy) = missingval; SDfcoinsu_CP(1:kxy) = missingval;

    end subroutine InitVarsTV6_CP

    !#######################################################################

    module subroutine InitVarsTV6(kxy)

        ! this routine allocates memory and initiates for dim=kxy
        ! of the original versions of the Treeview6 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch


        use UR_Gspk1Fit,  only: guse,erg,GnetRate,RateCB,RateBG,SDRateBG, &
                                effi,SDeffi,pgamm,SDpgamm,fatt,SDfatt,    &
                                fcoinsu,SDfcoinsu,SDGnetRate
        use UR_Gleich,    only: missingval
        use UR_VARIABLES, only: open_project_parts,GspkDT
        implicit none

        integer, intent(in)   :: kxy


        if(open_project_parts .and. GspkDT .and. allocated(guse) .and. allocated(erg)) return   ! 20.9.2023

        allocate(guse(kxy),erg(kxy),GnetRate(kxy),RateCB(kxy),RateBG(kxy))
        allocate(SDRateBG(kxy),effi(kxy),SDeffi(kxy),pgamm(kxy),SDpgamm(kxy))
        allocate(fatt(kxy),SDfatt(kxy),fcoinsu(kxy),SDfcoinsu(kxy))
        allocate(SDGnetRate(kxy))

        guse(1:kxy) = 0; erg(1:kxy) = missingval; GNetRate(1:kxy) = missingval;
        RateCB(1:kxy) = missingval; RateBG(1:kxy) = missingval; SDRateBG(1:kxy) = missingval;
        effi(1:kxy) = missingval; sdeffi(1:kxy) = missingval; pgamm(1:kxy) = missingval;
        sdpgamm(1:kxy) = missingval; fatt(1:kxy) = missingval; SDfatt(1:kxy) = missingval;
        fcoinsu(1:kxy) = missingval; SDfcoinsu(1:kxy) = missingval; SDGnetRate(1:kxy) = missingval

    end subroutine InitVarsTV6

    !#######################################################################

    module subroutine InitVarsTV7(kxy)

        ! this routine allocates memory and initiates for dim=kxy
        ! of the original versions of the Treeview7 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_Linft,     only: xkalib,uxkalib,ykalib,uykalib
        use UR_Gleich,    only: missingval
        implicit none

        integer, intent(in)    :: kxy


        allocate(xkalib(kxy),uxkalib(kxy),ykalib(kxy),uykalib(kxy))

        xkalib(1:kxy) = missingval; uxkalib(1:kxy) = missingval; ykalib(1:kxy) = missingval;
        uykalib(1:kxy) = missingval;

    end subroutine InitVarsTV7

    !#######################################################################

    module subroutine InitVarsTV8(kxy)

        ! this routine allocates memory and initiates for dim=kxy
        ! of the original versions of the Treeview8 arrays.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,    only: zero
        use UR_Gleich,    only: meanID,MDpoint,MDpointrev,MDused,nvalsMD,k_MDtyp, &
            ixdanf,fbayMD,meanMD,smeanMD,umeanMD,ngrs,nvmd
        implicit none

        integer, intent(in)    :: kxy


        allocate(meanID(kxy),MDpoint(kxy),MDpointrev(ngrs),MDused(kxy))
        allocate(nvalsMD(kxy),k_mdtyp(kxy),ixdanf(kxy),fBayMD(kxy))
        allocate(meanMD(kxy),smeanMD(kxy),umeanMD(kxy),nvMD(kxy) )
        MDpoint = 0; MDpointrev = 0; MDused = .false.; nvalsMD = 0; k_MDtyp = 0; ixdanf = 0;
        fbayMD = zero; meanMD = zero; smeanMD = zero; umeanMD = zero; nvMD = 0;

    end subroutine InitVarsTV8

    !#######################################################################

    module subroutine DRead(kunit,text,ios)

        ! reads one record as text string from a text file connected to unit
        ! kunit.
        ! used e.g. by ProRead.

        !     Copyright (C) 2020-2023  Günter Kanisch

        implicit none

        integer   , intent(in)               :: kunit
        character(:),allocatable,intent(out) :: text
        integer   ,intent(out)               :: ios

        integer              :: lent,iwh
        character(len=100)   :: messg

        iwh = 0
        lent = 1200

        do
            if(allocated(text)) deallocate(text)
            allocate(character(len=lent) :: text)

            READ(kunit,'(a)',iostat=ios,iomsg=messg) text       !    ,iomsg=messg) text
            ! if(ios /= 0) write(66,*) 'DREAD: ios=',int(ios,2),' iwh=',int(iwh,2),' i/o messg=',messg
            !write(0,*) 'DREAD: text=',trim(text)
            ! write(0,*) 'DREAD: unit=',kunit,' ios=',ios    ! ,' messg=',trim(messg)
            !  write(66,*) 'Dread: iwh=0',' lent=',lent,' ios=',ios,'  text=',trim(text)
            if(ios /= 0 .and. ios /= -1) then    ! eof
                lent = lent * 2
                backspace kunit
                iwh = iwh + 1
                ! write(66,*) 'Dread: iwh=',int(iwh,2),' lent=',lent,' ios=',ios,'  text=',trim(text)
                ! if(iwh < 4) goto 10
                if(iwh >= 4) exit
            else
                exit
            end if
        end do
    end subroutine DRead

    !##############################################################################

    module subroutine GetCells(text, cell, cconv)              !  enloc,slen,sdim,cconv)

        ! if one got by DRead one text record from a CSV file, this routine defines
        ! the text parts between two list separator charaters as a "cell". So,
        ! this routine returns for one record a character array named cell. If the
        ! parameter cconv is set to 'u', the array elements of cell are returned
        ! as upper case strings  used e.g. by ProRead.

        !     Copyright (C) 2014-2023  Günter Kanisch

        use UR_VARIABLES,   only: sListSeparator
        use UR_Gleich,      only: charv

        implicit none

        CHARACTER(LEN=*),INTENT(IN)    :: text
        type(charv),allocatable        :: cell(:)
        CHARACTER(LEN=1),INTENT(IN)    :: cconv

        integer          :: jz,i,klast,i3,k,ir,lent
        character(len=1) :: ctr

        ctr = sListSeparator
        jz = 0
        klast = 0
        lent = len_trim(text)
        do i=1,lent
            IF(text(i:i) == ctr .or. i == lent) THEN
                ir = i
                if(i == lent .and. text(i:i) /= ctr ) ir = i+1
                IF(i > klast) THEN
                    jz = jz + 1
                    IF(jz > 200) exit
                    cell(jz)%s = ' '
                    IF(i > klast+1) THEN
                        cell(jz)%s = text(klast+1:ir-1)
                        do i3=1,LEN_TRIM(cell(jz)%s)
                            IF(ICHAR(cell(jz)%s(i3:i3)) <= 13) cell(jz)%s(i3:i3) = ' '
                            IF(cconv == 'U' .OR. cconv == 'u') THEN
                                k = ichar(cell(jz)%s(i3:i3))
                                IF( (k >= 97 .AND. k <= 122) .OR. k == 252 .OR. k == 228 .OR.   &
                                    k == 246 ) cell(jz)%s(i3:i3) = char (k-32)
                            end if
                        end do
                    end if
                    klast = i
                end if
            end if
        end do

    end subroutine GetCells

    !#####################################################################

    module subroutine ModVarsTV2(nng)

        ! this routine extends (or downsizes) the allocation of the
        ! treeview2 arrays to dim=nng

        !     Copyright (C) 2020-2023  Günter Kanisch
        use UR_gleich,     only: Symbole,symtyp,einheit,bedeutung,Messwert,IVTL,  &
                                 SDFormel,SDwert,HBreite,IAR,StdUnc

        implicit none

        integer, intent(in)    :: nng

        if(nng /= ubound(Symbole,dim=1)) call CharModA1(Symbole,nng)
        if(nng /= ubound(symtyp,dim=1)) call CharModA1(symtyp,nng)
        if(nng /= ubound(Einheit,dim=1)) call CharModA1(Einheit,nng)
        if(nng /= ubound(Bedeutung,dim=1)) call CharModA1(Bedeutung,nng)
        if(nng /= ubound(SDformel,dim=1)) call CharModA1(SDformel,nng)
        if(nng /= ubound(Messwert,dim=1)) call RealModA1(Messwert,nng)
        if(nng /= ubound(SDwert,dim=1)) call RealModA1(SDWert,nng)
        if(nng /= ubound(HBreite,dim=1)) call RealModA1(HBreite,nng)
        if(nng /= ubound(StdUnc,dim=1)) call RealModA1(StdUnc,nng)
        if(nng /= ubound(IVTL,dim=1)) call IntModA1(IVTL,nng)
        if(nng /= ubound(IAR,dim=1)) call IntModA1(IAR,nng)

    end subroutine ModVarsTV2

    !##############################################################################

    module subroutine ModVarsTV2_CP(nng)

        ! this routine extends (or downsizes) the allocation of the
        ! treeview2 arrays *_CP to dim=nng

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_gleich,     only: Symbole_CP, symtyp_CP, einheit_CP, &
                                 bedeutung_CP, Messwert_CP, IVTL_CP,  &
                                 SDFormel_CP, SDwert_CP, HBreite_CP, &
                                 IAR_CP, StdUnc_CP ! MesswertSV

        implicit none

        integer   ,intent(in)    :: nng

        call CharModA1(Symbole_CP,nng)
        call CharModA1(symtyp_CP,nng)
        call CharModA1(Einheit_CP,nng)
        call CharModA1(Bedeutung_CP,nng)
        call CharModA1(SDformel_CP,nng)
        call RealModA1(Messwert_CP,nng)
        call RealModA1(SDWert_CP,nng)
        call RealModA1(HBreite_CP,nng)
        call RealModA1(StdUnc_CP,nng)
        call IntModA1(IVTL_CP,nng)
        call IntModA1(IAR_CP,nng)

    end subroutine ModVarsTV2_CP

    !##############################################################################

    module subroutine IntModA2(array,n1,n2)

        ! this routine extends (or downsizes) the allocation of the
        ! two-dim integer array to dims n1 and n2.  move_alloc is used.

        !     Copyright (C) 2020-2023  Günter Kanisch

        implicit none

        integer   ,intent(in)    :: n1,n2
        integer   ,allocatable   :: array(:,:), vvv(:,:)

        integer         :: ix1, ix2, i1, i2

        if(.not.allocated(array)) then
            allocate(array(n1,n2))
            array = 0
            return
        end if
        ix1 = ubound(array,dim=1)
        ix2 = ubound(array,dim=2)

        if(n1 == ix1 .and. n2 == ix2) return

        allocate(vvv(n1,n2))
        vvv = 0
        do i1=1,min(ix1,n1)
            do i2=1,min(ix2,n2)
                vvv(i1,i2) = array(i1,i2)
            end do
        end do
        call move_alloc(vvv, array)

    end subroutine IntModA2

    !#######################################################################

    module subroutine CharModStr(str,n)

        ! this routine extends (or downsizes) the allocation of the
        ! character variable str to n characters

        !     Copyright (C) 2020-2023  Günter Kanisch

        implicit none

        character(:),allocatable,intent(inout) :: str
        integer   ,intent(in)                  :: n

        if(allocated(str)) deallocate(str)
        allocate(character(len=n) :: str)

    end subroutine CharModStr

    !##############################################################################

    module subroutine RealModA2(array,n1,n2)

        ! this routine extends (or downsizes) the allocation of the
        ! 2-dim real(rn) array to dims n1 and n2.  move_alloc is used.

        !     Copyright (C) 2020-2023  Günter Kanisch

        use UR_params,   only: rn
        implicit none

        integer   ,intent(in)    :: n1,n2
        real(rn),allocatable   :: array(:,:),vvv(:,:)

        integer         :: ix1,ix2,i1,i2

        if(.not.allocated(array)) then
            allocate(array(n1,n2))
            array = 0._rn
            return
        end if
        ix1 = ubound(array,dim=1)
        ix2 = ubound(array,dim=2)

        if(n1 == ix1 .and. n2 == ix2) return

        allocate(vvv(n1,n2))
        vvv = 0
        do i1=1,min(ix1,n1)
            do i2=1,min(ix2,n2)
                vvv(i1,i2) = array(i1,i2)
            end do
        end do
        call move_alloc(vvv, array)

    end subroutine RealModA2

    !#######################################################################

    module subroutine load_unit_conv(nvars)

        ! calculates for the (independent) input quantities the array of scaling
        ! factors, unit_conv_fact(), required for transforming them to basic units
        ! For more details:  See chapter 7.21 of the UncertRadio CHM Help file.

        !     Copyright (C) 2021-2023  Günter Kanisch

        use UR_params,   only: rn, one, zero
        use UR_Gleich,   only: apply_units, unit_conv_fact, ngrs, einheit, nab, ncov, &
                               Symbole, Messwert, einheit_conv, unit_conv_factSV
        use CHF,         only: ucase

        implicit none

        integer, intent(in)   :: nvars    ! number of variables; at the moment: ngrs+ncov

        integer             :: i, k, nc, klast, k2, klen, iexp
        real(rn)            :: factor, last_fact
        character(len=1)    :: cop, copa(5)
        character(len=50)   :: einhp, einhp_v

        if(.not.apply_units) return

        if(allocated(unit_conv_fact)) deallocate(unit_conv_fact)
        allocate(unit_conv_fact(nvars))
        if(allocated(einheit_conv)) deallocate(einheit_conv)
        allocate(einheit_conv(nvars))
        if(allocated(unit_conv_factSV)) deallocate(unit_conv_factSV)
        allocate(unit_conv_factSV(nvars))


        if(ngrs /= ubound(unit_conv_fact,dim=1)) call RealModA1(unit_conv_fact,nvars)
        call CharModA1(einheit,ngrs+ncov)
        if(ubound(Symbole,dim=1) < ngrs+ncov) call CharModA1(Symbole,ngrs+ncov)

        do i=1,ngrs+ncov
            einheit_conv(i)%s = ' '
        end do

        unit_conv_fact = one    ! first set them all to one!
        unit_conv_factSV = one

        do i=nab+1,ngrs+ncov
            ! only input quantities are considered here: einheit(i) may be e.g. 'Bq*s',
            ! with sub-units 'Bq' and 's'
            !
            !nfd = 0
            !do k=1,UU%nSymb
            !  if(ucase(einheit(i)%s) == ucase(UU%EinhSymb(k)%s)) then
            !    einheit_conv(i)%s = einheit(i)%s
            !    nfd = 1
            !    exit
            !  end if
            !end do
            !if(nfd == 1) cycle
            if(index(ucase(Symbole(i)%s),'_TRIGGER') > 0) Messwert(i) = zero

            cop = ''
            nc = 0
            klast = 1
            klen = len_trim(einheit(i)%s)

            do k=1,klen
                cop = einheit(i)%s(k:k)
                ! if(cop == '*' .or. cop == '/' .or. (klast > 1 .and. k == klen)) then
                if(cop == '*' .or. cop == '/' .or. cop == '^' .or. (klast > 1 .and. k == klen)) then
                    nc = nc + 1
                    copa(nc) = cop    ! single-character operators
                    k2 = k-1
                    if(k == klen) k2 = k
                    einhp = trim(adjustL(einheit(i)%s(klast:k2)))   ! may then be 'Bq' or 's'
                    call uconvert(einhp,factor,einhp_v)
                    write(66,*) '                einhp=',trim(einhp),'  factor=',sngl(factor),' einhp_v=',trim(einhp_v)  !, &
                    ! '   k=',int(k,2),' nc=',int(nc,2),' cop=',cop
                    if(nc == 1) unit_conv_fact(i) = factor
                    if(nc == 1) einheit_conv(i)%s = einhp_v
                    if(nc > 1) then
                        ! combine the individual scaling factors of the sub-units to unit_conv_fact(i):
                        if(copa(nc-1) == '*') then
                            unit_conv_fact(i) = unit_conv_fact(i) * factor
                            einheit_conv(i)%s = trim(einheit_conv(i)%s) // copa(nc-1) // trim(einhp_v)
                        end if
                        if(copa(nc-1) == '/')  then
                            unit_conv_fact(i) = unit_conv_fact(i) / factor
                            einheit_conv(i)%s = trim(einheit_conv(i)%s) // copa(nc-1) // trim(einhp_v)
                        end if
                        if(copa(nc-1) == '^')  then
                            read(einhp_v,*) iexp
                            if(nc > 2) then
                                if(copa(nc-2) == '/') then
                                    unit_conv_fact(i) = unit_conv_fact(i) / (last_fact**real(iexp-1,rn))
                                    einheit_conv(i)%s = trim(einheit_conv(i)%s) // copa(nc-1) // trim(einhp_v)
                                end if
                                if(copa(nc-2) == '*') then
                                    unit_conv_fact(i) = unit_conv_fact(i) * (last_fact**real(iexp-1,rn))
                                    einheit_conv(i)%s = trim(einheit_conv(i)%s) // copa(nc-1) // trim(einhp_v)
                                end if
                            end if
                            write(66,*) 'XXXXX:  einhp_v=',einhp_v,' factor=',sngl(factor),'  einheit(i)%s(k:k)=',einheit(i)%s(k:k)
                        end if
                    end if
                    klast = k + 1
                    last_fact = factor
                end if

            end do
            if(nc == 0) then
                call uconvert(trim(adjustL(einheit(i)%s)),unit_conv_fact(i),einhp_v)
                einheit_conv(i)%s = trim(einhp_v)
            end if
            unit_conv_factSV(i) = unit_conv_fact(i)
            write(66,*)'LUC: ',Symbole(i)%s,' ',einheit(i)%s,' unit_conv_factSV=',sngl(unit_conv_factSV(i))

        end do

    end subroutine load_unit_conv

!###############################################################################

    module subroutine uconvert(einhp, factor, einhp_new)

        ! finds for a unit einhp, without containing numerical operators (* or /), the
        ! basic unit and the associated unit conversion factor. This is based on the
        ! data read in by ReadUnits/UncW_init from the file UnitsTable.csv.

        !     Copyright (C) 2021-2023  Günter Kanisch

        use UR_params,         only: rn
        use UR_Gleich,         only: UU
        use CHF,               only: ucase

        implicit none

        character(len=*),intent(in)  :: einhp
        real(rn),intent(out)         :: factor
        character(len=*),intent(inout) :: einhp_new

        integer      :: j, k, nfd

        einhp_new = einhp
        factor = 1._rn
        do j=1,UU%nSymb
            nfd = 0
            do k=1,UU%nSymbCsd(j)
                if(trim(ucase(einhp)) == trim(ucase(UU%EinhSymbScd(j,k)%s))) then
                    factor = UU%EinhScdFact(j,k)
                    einhp_new = UU%EinhSymb(j)%s
                    ! write(66,*) 'einhp=',trim(einhp),' factor=',factor
                    nfd = 1
                    exit
                end if
            end do
            if(nfd == 0) then
                do k=1,UU%nSymbSyn(j)
                    if(trim(ucase(einhp)) == trim(ucase(UU%EinhSymbSynon(j,k)%s))) then
                        factor = 1.0_rn
                        einhp_new = einhp
                        nfd = 1
                        exit
                    end if
                end do
            end if
            if(nfd == 1) exit
        end do

    end subroutine uconvert

!#######################################################################


end submodule TOPA
