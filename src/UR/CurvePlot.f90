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
recursive subroutine CurvePlot()

    ! Plots the decay curve from a least-squares evaluation into the graphics
    ! window. It uses the curve data given by the arrays xpl, ypl and yplfit.
    ! A graphics file 'CurvePlot.png' is stored also on the disk.
    !
    ! Note that the PLPLOT library requires to receive real(8) data, not real(rn).

    !   Copyright (C) 2014-2025  Günter Kanisch

    use, intrinsic :: iso_c_binding,       only: c_ptr, c_loc, c_associated, c_int

    use plplot, only: plwidth, plcol0, plscolbg, plschr, plclear, plenv0, pllab, plssym, &
                      pllsty, plpoin, pljoin, plline, plptex, plaxes, plend1

    use cairo,               only: cairo_get_reference_count
    use gtk,                 only: gtk_widget_queue_draw,gtk_notebook_set_current_page
    use gtk_draw_hl,         only: hl_gtk_drawing_area_get_gdk_pixbuf,hl_gtk_drawing_area_cairo_destroy
    use gdk_pixbuf_hl,       only: hl_gdk_pixbuf_save

    use UR_types,            only: rn
    use UR_params,           only: ZERO
    use ur_general_globals,  only: actual_plot, results_path
    use UR_Gleich_globals,   only: nab, SymboleG, Messwert, knumEGr, kpoint
    use UR_gtk_globals,      only: nbook2

    use UR_Linft,            only: xpl,ypl,uypl,yplfit,numd,nchannels,xplz,yplz,fpa,ma,ifit,yplsing, &
                                   kPMLE,mfrbg,k_rbl,d0zrate,defineallxt,dtdiff
    use plplot_code_sub1,    only: scalable, familying, gform, three_in_one, PrepareF
    use Rout,                only: pending_events
    use PLsubs,              only: CairoPlplotPrepare
    use common_sub1,         only: cc, drawing, PrintPlot_active
    use CHF,                 only: FindlocT
    use Rw1,                 only: Find_lambda

    use UWB,                 only: gevalf
    use file_io,             only: logger
    use translation_module,  only: T => get_translation

    implicit none

    integer             :: nval,i,j,kch,k,keq,kst,icc,imax
    real(rn)            :: xleft,xright,ylow,yhigh,xmin,xmax,ymin,ymax,xfakt, &
                           MWkst,term,ffc,yshift,mw_rbl
    character(:),allocatable  :: plfile,str1,ylab
    character(len=512)  :: log_str
    type(c_ptr)         :: pixbuf
    integer(c_int)      :: ccounts
    character(len=2)    :: csymb
    logical             :: gross,x_as_integer

    yshift = 0.0
    allocate(character(len=10) :: plfile, str1, ylab)
    call gtk_notebook_set_current_page(nbook2,2_c_int)
    nval = numd


    actual_plot = 'CurvePlot'
    if(.not. PrintPlot_active) then
        call CairoPlplotPrepare('CurvePlot')
        scalable = .false.
        familying = .false.
        gform = 'png'
        three_in_one = .false.
        call PrepareF('CurvePlot')
    end if
    !-------------------------------------------------------------------------
    gross = .false.
    IF(kPMLE == 1 .and. mfrbg > 0) THEN
        ! IF(ifit(mfrbg) <= 2) gross = .TRUE.
        IF(ifit(mfrbg) >= 2) gross = .TRUE.          ! 25.6.2024
        mw_rbl = ZERO
        if(k_rbl > 0) mw_rbl = Messwert(kpoint(k_rbl))
        yshift = d0zRAte(1) + mw_rbl
    end if

    xleft = 0.0_rn
    xright = 1.0_rn
    ylow = 0.0_rn
    yhigh = 1.0_rn
    x_as_integer = .false.
    if(defineallxt .and. int(dtdiff(2) - dtdiff(1)+.00001_rn,4) == 1) x_as_integer = .true.

    kst = FindlocT(SymboleG,'TSTART')
    MWkst = Messwert(kst)

    if(allocated(xplz)) deallocate(xplz,yplz,yplsing)           ! 17.6.2023
    allocate(xplz(nchannels*51),yplz(nchannels*51),yplsing(3,nchannels*51))    ! 19.8.2023
    xplz = ZERO
    yplz = ZERO
    yplsing = ZERO
    icc = 0
    do kch=1,nchannels
        imax = 51
        if(x_as_integer) imax = nval
        do i=1,imax
            j = imax*(kch-1) + i
            if(x_as_integer) j = imax*(kch-1) + i
            do k=1,ma
                keq = (kch-1)*3 + k
                if(x_as_integer) keq = (i-1)*knumEGr + k
                if(ifit(k) > 2 .and. .not.gross) cycle          ! 27.6.2024
                if(i == 1) icc = icc + 1         ! number of individual Xi curves
                xplz(j) = xpl(1) + real(i-1,rn)*(xpl(numd)-xpl(1)) / 50._rn
                if(x_as_integer) xplz(j) = j
                if(.not.x_as_integer) Messwert(kst) = xplz(j) * 3600._rn
                ffc = gevalf(nab+keq,Messwert)
                term = fpa(k)*ffc
                if(mfrbg > 0 .and. mfrbg == k) term = fpa(k) * 1.0_rn
                yplz(j) = yplz(j) + term
                if( (nchannels == 1 .and. knumEGr == 1 .and. ifit(k) <= 2) .or. gross ) then
                    ! 19.8.2023: plot also single fit components
                    yplsing(k,j) = term
                end if
            end do
        end do
    end do

    if(.false.) then
        do i=1,51*nchannels

            write(log_str, '(*(g0))') 'i=',int(i,2),' xplz,yplz=',sngl(xplz(i)),sngl(yplz(i))
            call logger(66, log_str)
        end do
        do i=1,numd

            write(log_str, '(*(g0))') 'i=',int(i,2),' xpl,ypl=',sngl(xpl(i)),sngl(ypl(i))
            call logger(66, log_str)
        end do
    end if
    Messwert(kst) = MWkst

    xmin = minval(xpl,nval)
    xmax = maxval(xpl,nval)
    ymin = minval(ypl,nval)
    ymax = maxval(ypl,nval)
    ymin = min(ymin, minval(yplfit,nval))
    ymax = max(ymax, maxval(yplfit,nval))
    ymin = min(ymin, minval(yplz,51*nchannels))
    ymax = max(ymax, maxval(yplz,51*nchannels))


    if(ifit(1) <= 2) ymax = max(ymax, maxval(yplsing(1,:),51*nchannels))
    if(ifit(2) <= 2) ymax = max(ymax, maxval(yplsing(2,:),51*nchannels))
    if(ifit(3) <= 2) ymax = max(ymax, maxval(yplsing(3,:),51*nchannels))
    if(ifit(1) <= 2) ymin = min(ymin, minval(yplsing(1,:),51*nchannels))
    if(ifit(2) <= 2) ymin = min(ymin, minval(yplsing(2,:),51*nchannels))
    if(ifit(3) <= 2) ymin = min(ymin, minval(yplsing(3,:),51*nchannels))

    ymax = ymax * 1.2_rn
    if(ymin > ZERO) ymin = ymin / 1.2_rn
    if(ymin < ZERO) ymin = ymin * 1.4_rn
    if(ymin/ymax < 0.15 .and. ymin >= 0._rn)  ymin = 0.0_rn

    if(.not. x_as_integer) then
        xmax = xmax * 1.08_rn
        if(xmin > ZERO) xmin = xmin / 1.08_rn
        if(xmin < ZERO) xmin = xmin * 1.15_rn
        if(xmin/xmax < 0.10_rn) xmin = -xmax/50._rn
    else
        xmin = 0._rn
        xmax = real(nval + 1,rn)
    endif


    call gtk_notebook_set_current_page(nbook2,1_c_int)

    xfakt = 1._rn     !  Plplot can handle the xfakt issue completely internally

    write(log_str, '(2(a,f0.6,2x,f0.6),a,f0.6,a,i0)') 'CurvePlot:  xmin,xmax=',xmin,xmax, &
        ' ymin,ymax=',ymin,ymax,'  xfakt=',xfakt, &
        '  nval=',nval
    call logger(166, log_str)

    call plclear()

    call plwidth(1.20d0)
    call plscolbg(255, 255, 255)
    call plcol0(1)   ! Graffer
    call plschr(0.d0, 1.25d0)
    call plenv0(real(xmin,8), real(xmax,8), real(ymin,8), real(ymax,8), 0, 0)

    call plschr(0.d0, 1.4d0)
    str1 = 'Title'


    ylab = T('net count rate in s#u-1#d')
    if(gross) ylab = T('gross count rate in s#u-1#d')
    if(.not. x_as_integer) then
        call pllab(T('time in h'), trim(ylab), '')
    else
        call pllab(T('# of measurement'), trim(ylab), '')
    endif

    ! PLot the Histogram:
    call plcol0(4)   ! (4)        ! Graffer
    call plwidth(0.0d0)

    ! error bars:
    call plwidth(2.0d0)
    call plssym(0.d0, 1.5d0)

    call plcol0(1)
    call pllsty(1)

    do i=1,nval
        if(.not. x_as_integer) then
            ! measured points:
            call plpoin((/real(xpl(i),8)/),(/real(ypl(i)+yshift,8)/),21)
            ! error bars:
            call pljoin(real(xpl(i),8), real(ypl(i)+yshift-1._rn*uypl(i),8), real(xpl(i),8), &
                real(ypl(i)+yshift+1._rn*uypl(i),8))
        else
            call plpoin((/real(i,8)/),(/real(ypl(i)+yshift,8)/),21)
            call plpoin((/real(i,8)+0.1d0/),(/real(yplz(i)+yshift,8)/),5)           ! crosses for fitted values
        end if
    end do

    call plssym(0.d0, 1.d0)    ! default char size

    ! Add the fit curve:
    call plcol0(1)        !9: blue   ! Graffer


    if(nchannels == 1 .and. knumEGr == 1 .and. icc > 1 .and. .not.x_as_integer) then
        ! 17.9.2023:
        call plwidth(1.4d0)
        call pllsty(2)
        call plcol0(2)
        call plschr(0.d0, 1.0d0)
        do k=1,ma
            if(ifit(k) > 2 .and. .not. gross) cycle
            if(gross .and. k > mfrbg .and. mfrbg > 0) cycle   ! 27.6.2024
            if(k == 1) call pllsty(2)
            if(k == 2) call pllsty(3)
            if(k == 3) call pllsty(5)
            ! if(ifit(k) > 2) cycle
            write(csymb,'(A1,i1)') 'X',k
            call plline(real(xplz(1:51),8), real(yplsing(k,1:51),8))

            if(yplsing(k,1) >= 0._rn) then
                call plptex(real(xplz(1),8),real(yplsing(k,1),8)+real(ymax-ymin,8)/50.d0, 0.d0, 0.d0, 1.d0,csymb)
            else
                call plptex(real(xplz(1),8),real(yplsing(k,1),8)+real(ymax-ymin,8)/50.d0*3.d0, 0.d0, 0.d0, 1.d0,csymb)
            endif

        end do
    end if

    call plwidth(2.5d0)
    call pllsty(1)
    call plcol0(4)
    if(.not.x_as_integer) then
        do kch=1,nchannels
            call plline(real(xplz(1:51),8), real(yplz(1:51),8))
        end do
    end if

    call plcol0(3)         ! Graffer

!--------------------------------------------------------------------------------
    call plwidth(0.d0)
    call plwidth(1.2d0)
    call plcol0(1)          ! Graffer
    call plaxes(real(xmax,8), real(ymin,8), 'ats', 0.d0, 0, '  ', 0.d0, 0)
    call gtk_widget_queue_draw(drawing(3))
    call pending_events()
    call pending_events()


    do while(cairo_get_reference_count(cc(3)) > 1_c_int)
        if(c_associated(cc(3))) call hl_gtk_drawing_area_cairo_destroy(cc(3))
    end do
    ccounts = cairo_get_reference_count(cc(3))

    call plend1()

    if(trim(gform) == 'png') then
        pixbuf = hl_gtk_drawing_area_get_gdk_pixbuf(drawing(3))
        plfile = 'CurvePlot.png'
        plfile = trim(results_path) // trim(plfile)
    end if

    if(trim(gform) == 'png') call hl_gdk_pixbuf_save(pixbuf, plfile, 'png')
    if(PrintPlot_active) PrintPlot_active = .false.
    call pending_events

end subroutine CurvePlot

