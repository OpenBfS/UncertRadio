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
! module common_sub1
! module plplot_code_sub1; contains subroutine PrepareF(actual_plot)
! module handlers
! module PLsubs:  uses plplot_code_sub1;
!     contains: subroutine CairoPlplotPrepare(actual_plot)
!               subroutine testAlloc(ip,strg)
!               SUBROUTINE Printplot()
!               subroutine PlotSteps(kqt,fng)
!               subroutine MCdistrib(kr,imcmx,xmin1,xmax1)
!               SUBROUTINE ShowHist2(X,Y,NVAL,Title,mcasum)
!               subroutine Replot(kpi)
!               subroutine PlotEli
!               subroutine quantileM


module common_sub1

    use, intrinsic :: iso_c_binding,  only: c_int, c_ptr

    integer(kind=c_int) :: height_wlast=0, width_wlast=0, width_da(4), height_da(4)
    type(c_ptr)         :: windowPL
    logical             :: drawboxpackedMC, drawboxpackedELI, drawboxpackedBS,drawboxpackedCP
    type(c_ptr)         :: draw_baseMC,draw_baseELI,draw_baseBS,draw_baseCP
    logical             :: PrintPlot_active
    type(c_ptr)         :: drawing(4), cc(4)   ! 1: MCplot, 2: BSplot,  3: Curveplot,  4: ELIplot
    integer             :: ipind               ! 1: MCplot, 2: BSplot,  3: Curveplot,  4: ELIplot

end module common_sub1

!=============================================================================================

module plplot_code_sub1

    use, intrinsic :: iso_c_binding,  only: c_ptr, c_int, c_associated
    use plplot,             only: plflt, plsetopt, PL_FCI_SCRIPT, PL_FCI_UPRIGHT, PL_FCI_MEDIUM, PLESC_DEVINIT
    use ur_general_globals, only: fname_grout
    use common_sub1,        only: ipind, PrintPlot_active, drawing, cc, windowPL, width_da, height_da
    use UR_types,           only: rn

    implicit none

    real(plflt)        :: xscale, yscale, xoff, yoff
    type(c_ptr)        :: cceps, scroll_w
    logical            :: scalable
    logical            :: familying
    character(len=20)  :: gform,device                   ! 'png',... , 'eps'
    integer            :: kqtx
    logical            :: three_in_one

contains

! from subroutine x01f95(area)
    subroutine PrepareF(actual_plot)

        use, intrinsic :: iso_c_binding,  only: c_ptr, c_associated, c_null_ptr, c_int
        use plplot, only: plsstrm, plend1, plscmap0, plsdev, plsdiori, plsfont, plsdev, &
                          plsfnam, plstart, plinit, plstar, plgver

        use ur_general_globals,     only: Gum_restricted, gtk_strm, png_to_cairo_surface, &
                                          results_path

        use cairo,                  only: cairo_ps_surface_set_eps, cairo_get_reference_count
        use gtk,                    only: true

        use gtk_draw_hl,            only: hl_gtk_drawing_area_cairo_destroy, &
                                          hl_gtk_drawing_area_get_size, &
                                          hl_gtk_drawing_area_cairo_new
        use UR_gtk_globals,         only: consoleout_gtk,plinit_done,zoomf
        use file_io,                only: logger
        use common_sub1,            only: ipind,drawing,cc,width_da,height_da

        ! plplot_extra is taken from the gtk-fortran website
        use plplot_extra,  only: pl_cmd

        implicit none

        character(len=*),intent(in)  :: actual_plot

        character(len=80)    :: version
        character(len=60)    :: geometry
        logical              :: prout
        integer(c_int)       :: sizewh(2)

        ! Define colour map 0 to match the "GRAFFER" colour table in
        ! place of the PLPLOT default.
        integer               :: plsetopt_rc                           ! , plparseopts_rc
        character(len=512)    :: log_str
        integer   , parameter :: rval(16) = (/255, 0, 255, &                 !original values
        & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
        & gval(16) = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
        & 0, 0, 85, 170/), &
        & bval(16) = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
        & 127, 85, 170/)

        !integer   , parameter, dimension(5) :: &
        !     & rval = (/ 255, 0,  255, 30,  0 /),&
        !     & gval = (/ 255, 70, 215, 200, 0 /), &
        !     & bval = (/ 255, 255,  0, 40,  0 /)

        prout = .false.

        call plsstrm(gtk_strm)

        !      write(66,*) 'GRAFFER colour table:'
        !      do k=1,3
        !        if(k == 1) Write(66,'(a,2x,16i4)') 'red   :',(rval(i),i=1,rn)
        !        if(k == 2) Write(66,'(a,2x,16i4)') 'green :',(gval(i),i=1,rn)
        !        if(k == 3) Write(66,'(a,2x,16i4)') 'blue  :',(bval(i),i=1,rn)
        !      end do
        !      write(66,*)

        !    Input variables which have to be set before calling this routine:
        !    actual_plot, scalable, gform, ipind, three_in_one

        !   Note: it is not a good idea to work directly also with cairo-surfaceses cs as
        !   defined by cs = cairo_get_target(cc). It is better to work with the cairo contexts (cc).

        !         if(prout) write(66,'(a,L1,a,i0,a,a)') 'PrepareF: angekommen,   GUM_restricted=',Gum_restricted,' ipind=',ipind, &
        !             ' gform=',trim(gform)
        if(prout)  then
            write(log_str, '(a,L1,a,i0,a,a)') 'PrepareF: angekommen,   GUM_restricted=',&
                           Gum_restricted,' ipind=',ipind, &
                           ' gform=',trim(gform)
            call logger(66, log_str)
        end if
        if(consoleout_gtk) write(0,*) 'PrepareF: angekommen  actual_plot=',actual_plot

        ! Get a cairo context from the drawing area.
        if(ipind > 0) then
            if(c_associated(cc(ipind))) then
                do while(cairo_get_reference_count(cc(ipind)) > 1_c_int)
                    if(c_associated(cc(ipind))) call hl_gtk_drawing_area_cairo_destroy(cc(ipind))
                end do
            end if

            cc(ipind) = c_null_ptr
            if(.not. c_associated(cc(ipind))) cc(ipind) = hl_gtk_drawing_area_cairo_new(drawing(ipind))    ! cairo context
            if(scalable .and. (trim(gform) == 'eps' .or. trim(gform) == 'pdf' .or. trim(gform) == 'svg' )) then
                call cairo_ps_surface_set_eps(cc(ipind),TRUE)
            end if
            do while(cairo_get_reference_count(cc(ipind)) > 1_c_int)
                if(c_associated(cc(ipind))) call hl_gtk_drawing_area_cairo_destroy(cc(ipind))
            end do

        end if

        if(ipind > 0) then
            sizewh = [ width_da(ipind), height_da(ipind) ]
            if(ipind /= 4) then
                sizewh(1) = int( sizewh(1) * zoomf)
                sizewh(2) = int( sizewh(2) * zoomf)
            end if
        end if
        call plend1()
        plinit_done = .false.
        !  write(66,*) 'PrepareF: sizewh=',int(sizewh,2),'  zoomf=',zoomf

        !  Print plplot version
        if(prout) then
            call plgver(version)
            write(log_str, '(a,a)') 'PLplot library version: ', trim(version)
            call logger(66, log_str)
        end if


        ! Drivers: see file .\include\drivers.h
        !  Initialize plplot
        call plscmap0(rval, gval, bval)
        if(.not.scalable) then
            if(.not.png_to_cairo_surface) then       ! this if-else- construct: 16.5.2025 GK
                device = "extcairo"
                call plsdev(trim(device))
                ! device = 'wingcc'       ! seems to work, but in a separate window; mouse cursor invisible!
                !device = "pngcairo"    ! only for a plot file
                ! device = 'wincairo'

                ! call plsdev(trim(device))
                fname_grout = ''
                fname_grout =  trim(results_path) // "MCplotfile.png"
            else
                device = "png"
                call plsdev(trim(device))

                fname_grout =  trim(results_path) // "MCplotfile.png"
                plsetopt_rc = plsetopt("o",trim(fname_grout))

            end if
           write(66,*) 'prep: A   fname_grout=',fname_grout
        else
            device = "pscairo"
            call plsdev(trim(device))
            if(trim(gform) == 'eps') then
                device = "epscairo"
                call plsdev(trim(device))
            end if
            !  call plsdev("eps")   ! does not work with plstar
            ! call plsdev("wincairo")
            if(trim(gform) == 'pdf') then
                device = "pdfcairo"
                call plsdev(trim(device))
            end if
            if(trim(gform) == 'svg') then
                device = "svgcairo"
                call plsdev(trim(device))
            end if
            call plsfnam(trim(fname_grout))
        end if

        !device = "cgmcairo"
        !call plsdev("cgmcairo")
        !call plsfnam(' ')

        ! write(0,*) 'PrepF:  plot_confidoid=',plot_confidoid,'  ',trim(actual_plot), &
        !            '  scalable=',scalable,'  gform=',trim(gform),' ipind=',ipind

        ! By default the "extcairo" driver does not reset the background
        ! This is equivalent to the command line option "-drvopt set_background=1"
        if(trim(device) /= 'wingcc' .and.trim(device) /= 'wincairo' .and. &
           trim(device) /= 'png'       )  plsetopt_rc = plsetopt("drvopt", "set_background=1")   ! 16.5.2025
        if(trim(device) == 'wingcc') plsetopt_rc = plsetopt("drvopt", "smooth=1")

        ! The "extcairo" device doesn't read the size from the context; therefore:
        ! For the geometry parameter, Don't use, e.g., cairo_image_surface_get_width(cs), because that value often
        ! happened to be zero (same for the height)!

        ! localestr = "English_United Kingdom.1252"   ! if to be used: use plsetopt()

        if(ipind > 0) then
            write(geometry, "(I0,'x',I0)") sizewh(1),sizewh(2)
        end if
        plsetopt_rc = plsetopt("geometry",  geometry)
        if(consoleout_gtk) write(0,*) 'nach plsetopt;   geometry= ',geometry
        ! write(66,*) 'nach plsetopt;   geometry= ',geometry,' zoomf=',sngl(zoomf),' plsetopt_rc=',plsetopt_rc

        if(len_trim(fname_grout) > 0 .and. scalable) plsetopt_rc = plsetopt('ori', '0')

        call plsdiori(real(0.0,8))     ! 0.: landscape;   1: portrait

        !  Divide page into 1x3 plots
        if(.not. scalable) then
            if( trim(actual_plot) == 'MCplot' .or. trim(actual_plot) == 'BSplot' ) then
                if(.not.Gum_restricted) then
                    call plstart(device,1,3)
                    plinit_done = .true.
                else
                    call plstart(device,1,3)
                    plinit_done = .true.
                end if
            else
                ! call plstar(1,1)
                if(ipind < 4) then
                    call plstart(device,1,1)
                    plinit_done = .true.
                end if
                if(ipind == 4) then
                    plinit_done = .true.
                    call plinit
                end if
            end if
        else
            ! plinit:
            if(three_in_one) then
                if(.not.GUM_restricted) then
                    call plstar(1,3)
                    plinit_done = .true.
                end if
                if(GUM_restricted) then
                    call plinit
                    plinit_done = .true.
                end if
            else
                plinit_done = .true.
                call plinit
            end if
        end if

        ! call plsfci(4)
        call plsfont(PL_FCI_SCRIPT, PL_FCI_UPRIGHT, PL_FCI_MEDIUM)

        ! Tell the "extcairo" driver where the context is located. This must be
        ! done AFTER the plstar or plinit call.
        call pl_cmd(PLESC_DEVINIT, cc(ipind))

        ! call plfontld(3)        ! load Hershey fonts
        ! call plfont(3)

        if(consoleout_gtk) write(0,*) 'End of Preparef:'
        return

    end subroutine PrepareF

!======================================================================

end module plplot_code_sub1


!#############################################################################################

module PLsubs

    use plplot_code_sub1


contains

!#########################################################################


    subroutine CairoPlplotPrepare(actual_plot)

    ! Copyright (C) 2011
    ! Free Software Foundation, Inc.

    ! This file is part of the gtk-fortran gtk+ Fortran Interface library.

    ! This is free software; you can redistribute it and/or modify
    ! it under the terms of the GNU General Public License as published by
    ! the Free Software Foundation; either version 3, or (at your option)
    ! any later version.

    ! This software is distributed in the hope that it will be useful,
    ! but WITHOUT ANY WARRANTY; without even the implied warranty of
    ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ! GNU General Public License for more details.

    ! Under Section 7 of GPL version 3, you are granted additional
    ! permissions described in the GCC Runtime Library Exception, version
    ! 3.1, as published by the Free Software Foundation.

    ! You should have received a copy of the GNU General Public License along with
    ! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
    ! If not, see <http://www.gnu.org/licenses/>.
    !
    ! gfortran hl_plplot1e.f90 `pkg-config --cflags --libs gtk-fortran plplotd-f95`
    ! Contributed by: James Tappin
    ! PLplot code derived from PLplot's example 1 by Alan W. Irwin

        use, intrinsic :: iso_c_binding
        use g,               only: g_value_init
        use gtk,             only: gtk_widget_set_vexpand_set, &
                                   gtk_widget_set_size_request, &
                                   gtk_widget_set_vexpand, &
                                   GTK_STATE_FLAG_NORMAL, &
                                   gtk_widget_override_background_color, &
                                   gtk_notebook_set_current_page,GDK_GRAVITY_NORTH_EAST, &
                                   gtk_window_set_gravity, gtk_widget_show_all, &
                                   gtk_window_get_position,gtk_window_move,gtk_widget_get_allocation

        use gtk_sup,         only: gvalue,G_TYPE_LONG
        use UR_gtk_window,   only: GdkRGBA

        use UR_gtk_globals,  only: consoleout_gtk, winPL_shown, posx, posy, &
                                   scrwidth_min,scrwidth_max,scrheight_min,monitorUR, &
                                   zoomf, nbook2
        use Top,             only: idpt, FindItemP

        use file_io,         only: logger
        use gtk_draw_hl,     only: hl_gtk_drawing_area_get_size, hl_gtk_drawing_area_resize, &
                                   hl_gtk_drawing_area_new,gtkallocation

        implicit none

        character(len=*),intent(in)  :: actual_plot

        integer                      :: i1,i2,i3

        type(c_ptr), target          :: widthp, heightp
        integer(kind=c_int)          :: sizewh(2), width, height
        integer(kind=c_int), target  :: rootx_l, rooty_l
        type(gvalue), target         :: gint4a,gint4b
        character(len=512)           :: log_str
        character(len=12)            :: colorname
        type(GdkRGBA), target        :: URcolor

        ipind = 0
        if(trim(actual_plot) == 'MCplot') ipind = 1
        if(trim(actual_plot) == 'BSplot') ipind = 2
        if(trim(actual_plot) == 'CurvePlot') ipind = 3
        if(trim(actual_plot) == 'ELIplot') ipind = 4

        if(trim(actual_plot) == 'MCplot' .or. trim(actual_plot) == 'BSplot'  .or.  &
            trim(actual_plot) == 'CurvePlot' ) windowPL = idpt('window_graphs')

        if(trim(actual_plot)== 'ELIplot') then
            goto 90
        else
            if(c_associated(nbook2)) goto 25
        end if

        widthp = c_loc(gint4a)
        widthp = g_value_init(widthp, G_TYPE_LONG)

        heightp = c_loc(gint4b)
        heightp = g_value_init(heightp, G_TYPE_LONG)

        ! write(log_str, '(*(g0))') 'MCsubs:  widthp, heightp = ',widthp,heightp
        ! call logger(66, log_str)

25      continue

        if(ipind > 0 .and. ipind <= 3) then
            colorname = "#E2FFFA"
            read(colorname,'(1x,z2.2,z2.2,z2.2)') i1,i2,i3
            ! write(66,*) 'i1,i2,i3=',i1,i2,i3
            URcolor%red = dble(i1)/dble(256)
            URcolor%green = dble(i2)/dble(256)
            URcolor%blue = dble(i3)/dble(256)
            URcolor%alpha = dble(1.)
            call gtk_widget_override_background_color(nbook2, GTK_STATE_FLAG_NORMAL, c_loc(URColor))
            if(.false.) then
                colorname = "#FFFFFF"
                read(colorname,'(1x,z2.2,z2.2,z2.2)') i1,i2,i3
                ! write(66,*) 'i1,i2,i3=',i1,i2,i3
                URcolor%red = dble(i1)/dble(256)
                URcolor%green = dble(i2)/dble(256)
                URcolor%blue = dble(i3)/dble(256)
                URcolor%alpha = dble(1.)
                call gtk_widget_override_background_color(idpt('box_wgraphs'), GTK_STATE_FLAG_NORMAL, c_loc(URColor))
            end if
        end if

90      continue

        if(ipind /= 4) then
            call gtk_widget_show_all (windowPL)
            ! Shift the plot window to the upper right
            if(.not.winPL_shown) call gtk_window_set_gravity(windowPL, GDK_gravity_NORTH_EAST)
            if(winPL_shown) then
                call gtk_window_move(windowPL,posx,posy)
                ! write(66,*) 'windowPL: Re-Show:  posx,posy=',posx,posy
            end if
            if(.not.winPL_shown) then
                if(monitorUR == 0) then
                    call gtk_window_get_position(windowPL, c_loc(rootx_l), c_loc(rooty_l))

                    posx = rootx_l
                    posy = rooty_l

                    write(log_str, '(*(g0))') 'monitorUR=0:  posx,posy=',posx,posy
                    call logger(66, log_str)
                elseif(monitorUR > 0) then
                    if(posx <= 0 .and. posy <= 0) then
                        !posx = int(real(screenw,rn)*0.65_rn)
                        !posy = 100
                        posx = scrwidth_min + int(real(scrwidth_max - scrwidth_min,rn)*0.65_rn)
                        posy = scrheight_min + 100
!                         write(66,*) 'windowPL: first Show:  posx,posy=',posx,posy
                        write(log_str, '(*(g0))') 'windowPL: first Show:  posx,posy=',posx,posy
                        call logger(66, log_str)
                    end if
                end if
                call gtk_window_move(windowPL,posx,posy)
                winPL_shown = .true.
            end if
        end if

        ! Note: selecting pages under nbook2 only works after having called
        ! show_all (windowPL)

        if(ipind == 1 ) call gtk_notebook_set_current_page(nbook2,0_c_int)
        if(ipind == 2 ) call gtk_notebook_set_current_page(nbook2,1_c_int)
        if(ipind == 3 ) call gtk_notebook_set_current_page(nbook2,2_c_int)

        if(ipind == 4) then
            sizewh = (/ width_da(4), height_da(4) /)
            call hl_gtk_drawing_area_resize(drawing(4), size=sizewh, copy = .true.)
            call gtk_widget_show_all(idpt('boxELI'))
        end if

        ! goto 95
        if(ipind > 0 .and. ipind <= 3) then
            sizewh = [ width_da(ipind), height_da(ipind) ]
            if(ipind /= 4) then
                sizewh(1) = int( sizewh(1) * zoomf)
                sizewh(2) = int( sizewh(2) * zoomf)
                ! write(66,*) 'CPLPR: set size: sizewh=',sizewh,' zoomf=',sngl(zoomf),' drawing=',drawing(ipind)

                call gtk_widget_set_vexpand_set(idpt('box_wgraphs'), 1_c_int)
                call gtk_widget_set_vexpand (idpt('box_wgraphs'), 1_c_int)
                ! call gtk_widget_set_size_request(idpt('box_wgraphs'), width=sizewh(1), height=sizewh(2))

                ! drawing(ipind) = hl_gtk_drawing_area_new(size=(/sizewh(1),sizewh(2)/),has_alpha=FALSE)
                call hl_gtk_drawing_area_resize(drawing(ipind), size=sizewh, copy=.true.)

                call hl_gtk_drawing_area_get_size(drawing(ipind), width, height)
                ! call gtk_widget_get_allocation(drawing(ipind),c_loc(alloc))
                !width = alloc%width
                !height = alloc%height
                !  write(66,*) 'CPLPR: get size: width,height=',width,height

            end if

        end if
        call TestAlloc(ipind,'End CPP:')
        !-------------------------------------------------------------------------------------
        if(consoleout_gtk) write(0,*) 'End CairoPlplotPrepare- - - - - - - '
        return

    end subroutine CairoPlplotPrepare

    !-----------------------------------------------------------

    subroutine testAlloc(ip,strg)

        use, intrinsic :: iso_c_binding,   only: c_loc
        use gtk,           only: gtk_widget_get_allocation
        use gtk_draw_hl,   only: gtkallocation
        use common_sub1,   only: drawing
        implicit none

        integer, intent(in)   :: ip
        character(len=*),intent(in)  :: strg

        type(gtkallocation),target    :: alloc
        integer               :: width_d,height_d

        return

        if(ip == 0) return
        call gtk_widget_get_allocation(drawing(ip),c_loc(alloc))
        width_d = alloc%width
        height_d= alloc%height
        write(0,*)
        write(0,*)
        write(0,*) 'ip=',ip,' ',trim(strg),'  drawing(ip): width=',width_d,'  height=',height_d

    end subroutine testAlloc


!########################################################################################

    subroutine Printplot()

!     Copyright (C) 2014-2025  Günter Kanisch

        use, intrinsic :: iso_c_binding,      only: c_ptr,c_int
        use plplot,             only: plend1
        use UR_MCC,             only: iopt_copygr
        use ur_general_globals, only: FileTyp,fname_grout,actual_plot,fname,  &
                                      clipd,results_path,bat_mc, dir_sep, &
                                      png_to_cairo_surface, pngfile
        use plplot_code_sub1,   only: drawing,gform,familying,scalable
        use gtk,                only: gtk_clipboard_set_image,gtk_clipboard_clear
        use gtk_draw_hl,        only: hl_gtk_drawing_area_get_gdk_pixbuf
        use gdk_pixbuf_hl,      only: hl_gdk_pixbuf_save
        use UR_Gleich_globals,  only: GrFormat, kEGr
        use cairo,              only: cairo_surface_destroy
        use UR_Linft,           only: fitmeth
        use Rout,               only: FOpen,WDSetComboboxAct
        use Top,                only: WrStatusbar
        use UR_gtk_globals,     only: plinit_done,plot_setintern
        use file_io,            only: logger
        use translation_module, only: T => get_translation

        implicit none

        character(len=100) :: ploption

        type(c_ptr)        :: pixbuf
        integer            :: ifehl, i1, kqt, i, i0
        character(len=50)  :: hinweis
        character(len=512) :: log_str
        character(len=256) :: fng

        call WrStatusBar(4,T('copying') // ".... " )
        ! write(0,*) 'Printplot start'
        PrintPlot_active = .true.
        ipind = 0
        if(trim(actual_plot) == 'MCplot') ipind = 1
        ! if(trim(actual_plot) == 'BSplot') ipind = 2
        if(trim(actual_plot) == 'CurvePlot') ipind = 2
        if(trim(actual_plot) == 'ELIplot') ipind = 3

        !  pixel graphics:
        ploption = GrFormat(max(1,iopt_copygr))%s
        ! ploption = 'PNG Format'
        write(log_str, '(*(g0))') 'Printplot:   ploption=',trim(ploption),'  actual_plot=',trim(actual_plot), &
            '  bat_mc=',bat_mc
        call logger(66, log_str)

        if(trim(actual_plot) == 'MCplot') pixbuf = hl_gtk_drawing_area_get_gdk_pixbuf(drawing(1))
        if(trim(actual_plot) == 'BSplot') pixbuf = hl_gtk_drawing_area_get_gdk_pixbuf(drawing(2))
        if(trim(actual_plot) == 'CurvePlot') pixbuf = hl_gtk_drawing_area_get_gdk_pixbuf(drawing(3))
        if(trim(actual_plot) == 'ELIplot') pixbuf = hl_gtk_drawing_area_get_gdk_pixbuf(drawing(4))

        ! write(log_str, '(*(g0))') 'Nach Plot end:  pixbuf=',pixbuf,' fname_grout=',trim(fname_grout),'  fng=',trim(fng)
        ! call logger(66, log_str)

        select case (trim(ploption))

          case ('PNG Format', 'BMP Format', 'JPEG Format', 'EPS Format', 'PDF Format')

            if(ploption(1:3) == 'PNG') gform = 'png'
            if(ploption(1:3) == 'BMP') gform = 'bmp'
            if(ploption(1:4) == 'JPEG') gform = 'jpeg'
            if(ploption(1:3) == 'EPS') gform = 'eps'
            if(ploption(1:3) == 'PDF') gform = 'pdf'

            if(bat_MC) then
                gform = 'pdf'
                i0 = 0     ! 2025.01.23  GK
                do i=len_trim(fname),1,-1
                    if(fname(i:i) == dir_sep) then
                        i0 = i
                        exit
                    end if
                end do
                ! fname_grout = trim(fname)
                fname_grout = trim(fname(i0+1:))
                i1 = index(fname_grout,'.')
                if(kEGr == 1) fname_grout = fname_grout(1:i1-1) // '_MC_EG1_' // trim(fitmeth) // '.' // trim(gform)
                if(kEGr == 2) fname_grout = fname_grout(1:i1-1) // '_MC_EG2_' // trim(fitmeth) // '.' // trim(gform)
                if(kEGr == 3) fname_grout = fname_grout(1:i1-1) // '_MC_EG3_' // trim(fitmeth) // '.' // trim(gform)
                fname_grout = trim(results_path) // trim(fname_grout)
            !else
            elseif(.not.png_to_cairo_surface) then           ! <--   16.5.2025 GK
                hinweis = T("Filename for graphic output") // ': '

                FileTyp = 'G'

                call FOpen(ifehl, .true., hinweis)
                if(ifehl == 1) then
                    PrintPlot_active = .false.
                    return
                end if
                fng = trim(fname_grout)           ! basical filename
                write(log_str, '(*(g0))') 'fname_grout=',trim(fname_grout),'   gform=',trim(gform)
                call logger(66, log_str)
            end if

            scalable = .false.
            familying = .false.
            fng = trim(fname_grout)

            if(trim(gform) == 'eps' .or. trim(gform) == 'pdf' .or. trim(gform) == 'svg') then
                scalable = .true.
                familying = .false.
                goto 10
            end if
            if(len_trim(fng) == 0 .and. len_trim(pngfile) > 0) fng = pngfile      ! 20.5.2025
            if(len_trim(fng) > 0) call hl_gdk_pixbuf_save(pixbuf, fng, trim(gform))

          case ('WIN Clipboard', 'WIN Zw.Ablage')
            call gtk_clipboard_clear(clipd)
            call gtk_clipboard_set_image(clipd,pixbuf)

          case default
        end select

        if(.true. .and. trim(actual_plot) == 'MCplot' .and. png_to_cairo_surface) then
            ! 16.5.2025 GK
            kqt = 3
            call PlotSteps(kqt,fng)
        end if

        GOTO 20
        !---------------------------------------------------------
10      continue

        !---------------------------------------------------------
        kqt = 1
        if(trim(actual_plot) == 'MCplot') kqt = 3
        if(trim(actual_plot) == 'BSplot') kqt = 3
        plot_setintern = .true.
        call PlotSteps(kqt, fng)
        !-------------------------------------------------
        if(.true. .and. scalable) then
            ! write(0,*) 'Replot erreicht'
            ! After having written a plotfile *.pdf, the original graphic (png)
            ! in the graphics window needs to be restored. This is performed
            ! by a second call to PlotSteps.

            call plend1()
            plinit_done = .false.
            PrintPlot_active = .false.
            scalable = .false.
            gform = 'png'
            iopt_copygr = 1
            call WDSetComboboxAct('comboboxGrELI',1)
            call WDSetComboboxAct('comboboxBS1',1)

            png_to_cairo_surface = .true.          !
            call reload_pngfile(pngfile)           !  16.5.2025
            png_to_cairo_surface = .false.         !
        end if

20      continue
        scalable = .false.
        gform = 'png'
        call WDSetComboboxAct('comboboxGrELI',1)
        call WDSetComboboxAct('comboboxBS1',1)
        PrintPlot_active = .false.

        return
    end subroutine Printplot

    !#########################################################################

    subroutine PlotSteps(kqt,fng)

!     Copyright (C) 2014-2024  Günter Kanisch

        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        use plplot, only: plend1

        USE UR_MCC,             only: nval,xplt,yplt,title,mcasum,kqtyp
        use ur_general_globals, only: fname_grout, Gum_restricted, actual_plot, results_path, dir_sep
        use plplot_code_sub1,   only: kqtx,scalable, three_in_one, preparef
        use cairo,              only: cairo_get_reference_count

        use Rout,               only: pending_events
        use Top,                only: WrStatusbar
        use gtk_draw_hl,        only: hl_gtk_drawing_area_cairo_destroy,hl_gtk_drawing_area_draw_pixbuf
        use UR_gtk_globals,     only: plinit_done,replot_on
        use file_io,            only: logger

        implicit none

        integer,intent(in)           :: kqt
        character(len=*),intent(in)  :: fng

        integer              :: i,i1,ksv,i0
        character(len=512)           :: log_str
        character(len=1)     :: c1

        three_in_one = .true.
        if(trim(actual_plot) == 'CurvePlot') three_in_one = .false.
        if(trim(actual_plot) == 'ELIplot') three_in_one = .false.


        do kqtx=1,kqt
            if(trim(actual_plot) == 'CurvePlot' .and. kqtx > 1) exit
            if(trim(actual_plot) == 'ELIplot' .and. kqtx > 1) exit

            if(len_trim(fng) > 0)  then
                write(log_str, '(*(g0))') 'Printplot: kqtx=',kqtx,'  three_in_one=',three_in_one, &
                '  fng=',trim(fng)
                call logger(66, log_str)
            end if
            if(GUM_restricted .and. kqtx > 1) exit

            if(kqtx > 1 .and. three_in_one) goto 17

            call CairoPlplotPrepare(actual_plot)
            if(scalable .and. .not. three_in_one) then
                i0 = 0    ! 2025.01.23  GK
                i1 = index(fng,'.eps')
                if(i1 == 0) i1 = index(fng,'.pdf')
                if(i1 > 0) then
                    write(c1,'(i1)') kqtx
                    fname_grout = trim(fng(1:i1-1)) // '-' // c1 // trim(fng(i1:))
                    do i=len_trim(fname_grout),1,-1
                        if(fname_grout(i:i) == dir_sep) then
                            i0 = i
                            exit
                        end if
                    end do
                    fname_grout = trim(fname_grout(i0+1:))
                    fname_grout = trim(results_path) // trim(fname_grout)
                    write(log_str, '(*(g0))') 'PrintPlot:   kqtx=',kqtx,'  fname_grout=',trim(fname_grout)
                    call logger(66, log_str)
                end if
            end if
            call PrepareF(actual_plot)

17          continue

            if(trim(actual_plot) == 'MCplot') then
                IF(nval(kqtx) > 0) THEN
                    ksv = kqtyp
                    kqtyp = kqtx
                    call ShowHist2(xplt(1:nval(kqtx),kqtx),yplt(1:nval(kqtx),kqtx),NVAL(kqtx),Title(kqtx),mcasum(kqtx))
                    kqtyp = ksv
                end if
                if((scalable .and. .not. three_in_one)) then
                    call plend1()
                    plinit_done = .false.
                end if
            end if
            !if(trim(actual_plot) == 'BSplot') then
            !  mqt = kqtx
            !  call PrintCsv(mmvars)
            !  if((scalable .and. .not. three_in_one) .or. mqt == 3) then
            !    call plend1()
            !      plinit_done = .false.
            !  end if
            !end if
            if(trim(actual_plot) == 'CurvePlot') then
                call CurvePlot()
                if(scalable .and. .not. three_in_one) then
                    call plend1()
                    replot_on = .false.
                    call pending_events()
                    plinit_done = .false.
                end if
            end if
            if(trim(actual_plot) == 'ELIplot') then
                call PlotELI()
                if(scalable .and. .not. three_in_one) then
                    call plend1()
                    plinit_done = .false.
                    do while (cairo_get_reference_count(cc(4)) > 1_c_int)
                        call hl_gtk_drawing_area_cairo_destroy(cc(4))
                    end do
                end if
            end if

        end do

        if(trim(actual_plot) == 'MCplot' .and. plinit_done) then        ! 16.5.2025 xxxxxxxxxxxxxxxxxx
            call plend1()
            plinit_done = .false.
            do while (cairo_get_reference_count(cc(1)) > 1_c_int)
                call hl_gtk_drawing_area_cairo_destroy(cc(1))
            end do
        end if
        if(trim(actual_plot) == 'BSplot') then
            call plend1()
            plinit_done = .false.
            do while (cairo_get_reference_count(cc(2)) > 1_c_int)
                call hl_gtk_drawing_area_cairo_destroy(cc(2))
            end do
        end if
        call plend1()
        plinit_done = .false.

        PrintPlot_active = .false.

        call pending_events()
        !----------------------
    end subroutine PlotSteps

!#########################################################################

    subroutine MCdistrib(kr,imcmx,xmin1,xmax1)

        use UR_MCC,           only: estLQ,estUQ,xplt,yplt,kqtyp,npltmax,mcmax,use_shmima, &
                                    mcaplot,kcrun,mca_min,mca_max,igmin,igmax,mcafull, &
                                    xstep,nval,mcasum,stepp
        use file_io,          only: logger
        use UR_params,        only: EPS1MIN, ZERO, TWO
        use UR_types,         only: rn

        implicit none

        integer   , INTENT(IN)      :: kr
        integer   , INTENT(IN)      :: imcmx
        real(rn), INTENT(IN)        :: xmin1, xmax1

        integer             :: ig1, ig2, imaxv, kjt, ig
        integer             :: i1,i2,i,igm1,ig1min,ig1max,nmc,inv
        integer             :: kjstep,krr
        real(rn)            :: prtot,xdsum,xdsum1
        real(rn)            :: mcapmin,mcapmax,xmaxh,xsteph
        character(len=512)           :: log_str
        real(rn)            :: mcapstep,prlow,prhigh
        !----------------------------------------------------------------------


        ! For each kqtyp a multichannel spectrum (array mcafull) is constructed in
        ! MCcalc; the range between mca_min(kqtyp) and mca_max(kqtyp) is
        ! partitioned into mcmax channels.

        !     Copyright (C) 2014-2024  Günter Kanisch

        !    ! sort an MCvalue into the mcafull:
        !    IF(MCvalue >= mca_min(kqtyp) .AND. MCvaluet <= mca_max(kqtyp) ) THEN
        !      izv = INT( (MCvalue - mca_min(kqtyp)) / xstep(kqtyp) + 0.4999 )
        !      mcafull(kqtyp,izv) = mcafull(kqtyp,izv) + 1
        !      ! IF(izv > 0) mcafull(kqtyp,izv) = mcafull(kqtyp,izv) + 1
        !    end if

        !  The plot array xplt() and yplt() are derived from mcafull.

        ! Further quantities (in MCCalc):

        !   channel width in mcafull(kqtyp):
        !    xstep(kqtyp) = (mca_max(kqtyp) - mca_min(kqtyp)) / readl(mcmac,rn)

        ! For plotting for a kqtyp value, another multichannel array mcaplot
        ! is buildt; its limit are: mcapmin, mcapmax.
        ! It is constructed from mcafull by combining (summing) 10, or 20,
        ! or 30,... (=kjt) channels, so that mcaplot has not more than NPLTMAX (1000)
        ! channels.

        ! kjstep=10 is the number of mcafull channels being combined into one mcaplot
        ! channel. If necessary, it can be reduced to kjstep=1.

        !----------------------------------------------------------------------

        kjstep = 10
        kjstep = 1     ! 29.10.2025

10      CONTINUE

        call logger(166, ' ')
        write(log_str, '(a,i2,a,i3)') 'MCDistrib:  kqtyp=',kqtyp,' run=',kr
        call logger(166, log_str)
        write(log_str, '(4(a,es15.8))') '            xmin1=',real(xmin1,8),'  xmax1=',real(xmax1,8),   &
            '  mca_min=',real(mca_min(kqtyp),8),'  mca_max=',real(mca_max(kqtyp),8)
        call logger(166, log_str)

        IF(abs(xmin1) > EPS1MIN .AND. abs(xmax1) > EPS1MIN) THEN
            mcapmin = xmin1
            mcapmax = xmax1
            GOTO 100
        end if
        !-----------------------------------------------------------------------

        mcapmin = 1.0E+15_rn
        mcapmax = -1.0E+15_rn
        imaxv = mcmax/kjstep
        write(log_str, '(*(g0))') ' Distr: at the beginning:    imaxv=',imaxv,'  kjstep=',kjstep
        call logger(166, log_str)

        igmax(kqtyp) = -1
        igmin(kqtyp) = -1
        do ig=1,imaxv
            ig1 = (ig-1)*kjstep+1
            ig2 = ig*kjstep
            do i=ig1,ig2
                IF(mcafull(kqtyp,i) > 0 .AND. abs(mcapmin-1.0E+15_rn) < EPS1MIN ) THEN
                    mcapmin = mca_min(kqtyp) + real(ig1,rn)*xstep(kqtyp)
                    igmin(kqtyp) = ig
                end if
            end do
            IF(igmin(kqtyp) > 0) EXIT
        end do

        do ig=imaxv,1,-1
            ig1 = (ig-1)*kjstep+1
            ig2 = ig*kjstep
            do i=ig1,ig2
                IF(mcafull(kqtyp,i) > 0 .AND. abs(mcapmax+1.0E+15_rn) < EPS1MIN ) THEN
                    mcapmax = mca_min(kqtyp) + real(ig1,rn)*xstep(kqtyp)
                    igmax(kqtyp) = ig
                end if
            end do
            IF(igmax(kqtyp) > 0) EXIT
        end do
        write(log_str, '(2(a,i4),2(a,es15.8))') 'MCDistrib:   igmin(kqtyp)=',igmin(kqtyp), &
            '  igmax(kqtyp)=',igmax(kqtyp),  &
            ' mcapmin=',real(mcapmin,8),'  mcapmax=',real(mcapmax,8)
        call logger(166, log_str)

100     CONTINUE

        mcapstep = (mcapmax - mcapmin)/real(igmax(kqtyp),rn)
        kjt = kjstep
        do
            IF( (mcapmax-mcapmin)/ (xstep(kqtyp)*real(kjt,rn)) >= real(npltmax,rn) ) THEN
                kjt = kjt + kjstep
            else
                EXIT
            end if
        end do

        IF(kjstep == 10 .AND. igmax(kqtyp)-igmin(kqtyp) < 10) THEN
            kjstep = 1
            GOTO 10
        end if

        write(log_str, '(a,i2,3(a,i5))') 'MCDistrib:  kqtyp=',kqtyp,'  igmin=',igmin(kqtyp),' igmax=',igmax(kqtyp),'  kjt=',kjt
        call logger(166, log_str)

        mcaplot(1:npltmax,kqtyp) = 0

        nval(kqtyp) = 0
        mcasum(kqtyp) = 0

        IF(mcapmin < mca_min(kqtyp) ) then
            nmc = INT( (mca_min(kqtyp)-mcapmin)/(xstep(kqtyp)*real(kjt,rn)) + 0.499_rn )
            nval(kqtyp) = nmc
            write(log_str, '(*(g0))') '       MCDistrib:   mcapmin < mca_min(kqtyp):   nval(',kqtyp,')=  nmc=',nmc
            call logger(166, log_str)
            ig = (igmin(kqtyp)-1)*kjstep + 1
            xmaxh = mca_min(kqtyp) + xstep(kqtyp)*( real(ig-1,8)+real(ig+kjt,rn))/TWO
            xsteph = xstep(kqtyp)*real(kjt,rn)
            do inv=nmc,1,-1
                ig = (igmin(kqtyp)-1)*kjstep  - (nmc-inv+1)*kjt + 1
                xplt(inv,kqtyp) = xmaxh - (nmc-inv+1)*xsteph
                yplt(inv,kqtyp) = ZERO
            end do
        end if

        ig = (igmin(kqtyp)-1)*kjstep + 1
        ig = ig - kjt
        igm1 = 0
        ig1min = int(ZERO)
        ig1max = int(ZERO)
        do
            ig = ig + kjt
            ! IF(ig > 10000-kjt) EXIT
            IF(ig > mcmax-kjt) EXIT
            IF(mca_min(kqtyp) + real(ig,rn)*xstep(kqtyp) > mcapmax) EXIT
            IF(mca_min(kqtyp) + real(ig,rn)*xstep(kqtyp) < mcapmin) CYCLE
            nval(kqtyp) = nval(kqtyp) + 1
            igm1 = igm1 + 1
            IF(igm1 == 1) ig1min = ig
            ig1max = ig
            do ig2=ig,ig+kjt-1
                mcaplot(nval(kqtyp),kqtyp) = mcaplot(nval(kqtyp),kqtyp) + mcafull(kqtyp,ig2)
                mcasum(kqtyp) = mcasum(kqtyp) + mcafull(kqtyp,ig2)
            end do
            xplt(nval(kqtyp),kqtyp) = mca_min(kqtyp) + xstep(kqtyp)*( real(ig-1,rn)+real(ig+kjt,rn))/2.
            yplt(nval(kqtyp),kqtyp) = real(mcaplot(nval(kqtyp),kqtyp),rn) / real(kr*imcmx,rn)
            if(kqtyp == 2) then
                krr = 1
                if(use_shmima) krr = kr
                yplt(nval(kqtyp),kqtyp) = real(mcaplot(nval(kqtyp),kqtyp),rn) / real(krr*imcmx,rn)
            end if
            IF(kqtyp == 3 .AND. kr < kcrun) yplt(nval(kqtyp),kqtyp) = real(mcaplot(nval(kqtyp),kqtyp),rn) / real(1*imcmx,rn)

            if(nval(kqtyp) == size(mcaplot,1)) exit
        end do
        write(log_str, '(a,i2,a,i3,3(a,es15.8),a,i2,a,es15.8)') 'kqtyp=',kqtyp,' run=',kr,'  mcapmin=',real(mcapmin,8),  &
            ' mcapmax=',real(mcapmax,8),' mcapstep=',real(mcapstep,8),' xstep(',kqtyp,')=',real(xstep(kqtyp),8)
        call logger(166, log_str)

        write(log_str, '(6x,a,i5,a,i5)') '      ig1min=',ig1min,'   ig1max=',ig1max
        call logger(166, log_str)

        prlow = ZERO
        prhigh = ZERO
        prtot = ZERO
        i2 = 0
        i1 = 0
        stepp(kqtyp) = sngl( xstep(kqtyp)*real(kjt,8) )
        yplt(1:nval(kqtyp),kqtyp) = yplt(1:nval(kqtyp),kqtyp) / stepp(kqtyp)

        xdsum = ZERO
        xdsum1 = ZERO
        xdsum = sum(yplt(1:nval(kqtyp),kqtyp))
        xdsum1 = dot_product(yplt(1:nval(kqtyp),kqtyp), xplt(1:nval(kqtyp),kqtyp))
        do i=1,nval(kqtyp)
            prtot = prtot + yplt(i,kqtyp) * stepp(kqtyp)
            ! WRITE(166,*) 'kqtyp=',kqtyp,' run=',kr,' i=',i,' xplt(i)=',xplt(i,kqtyp),'  yplt(i)=',yplt(i,kqtyp)
            IF((kqtyp == 1 .OR. kqtyp == 3) .AND. (xplt(i,kqtyp)+0.5_rn*stepp(kqtyp)) < estLQ) THEN
                prlow = prlow + yplt(i,kqtyp)*stepp(kqtyp)
                i1 = i
            end if
            IF((kqtyp == 1 .OR. kqtyp == 2) .AND. (xplt(i,kqtyp)-0.5_rn*stepp(kqtyp)) > estUQ) THEN
                prhigh = prhigh + yplt(i,kqtyp)*stepp(kqtyp)
                IF(i2 == 0) i2 = i
            end if
        end do
        xdsum1 = xdsum1 / xdsum
        write(log_str, '(a,i2,a,i3,2(a,es11.4))') 'kqtyp=',kqtyp,' run=',kr,'  prtot=',real(prtot,8), ' Schwerpunkt, x: ',real(xdsum1,8)
        call logger(166, log_str)

        prlow = prlow + yplt(i1+1,kqtyp)/TWO* stepp(kqtyp)
        prhigh = prhigh + yplt(MAX(1,i2-1),kqtyp)/TWO* stepp(kqtyp)

        IF(kqtyp == 1 .OR. kqtyp == 3)  then
            write(log_str, '(a,i2,i3,3(a,es11.4),a,i5)') 'kqtyp,kr=',kqtyp,kr, &
            '  estpL from MCA=',real(prlow,8),'   estLQ=',real(estLQ,8),'  stepp=',real(stepp(kqtyp),8),' kjt=',kjt
            call logger(63, log_str)
        end if

    end subroutine MCDistrib

    !#########################################################################

    subroutine ShowHist2(X,Y,NVAL,Title,mcasum)

        !
        !  Display the data using selected style, layout, etc.
        !
        !     Copyright (C) 2014-2025  Günter Kanisch

        use, intrinsic :: iso_c_binding
        use plplot,               only: PLGraphicsIn, plgetcursor, plgstrm, plclear, plwidth, &
                                        plcol0, plscolbg, plschr, plsxax, pladv, plclear, plwind, &
                                        plgspa, plbox, plaxes, plenv0, pllab, plline, pljoin, &
                                        pllsty, plvpor, plgdev

        use cairo,                only: cairo_destroy, cairo_get_reference_count
        use gtk_draw_hl,          only: hl_gtk_drawing_area_cairo_destroy
        use gtk,                  only: gtk_widget_queue_draw
        use gtk_sup

        USE UR_MCC,               only: VertLines,use_shmima,shmin,shmax,shfakt,kqtyp,stepp

        USE ur_general_globals,   only: Gum_restricted
        USE UR_Gleich_globals,    only: Ucomb,Ucomb_DTv,Ucomb_DLv,MesswertSV,kEGr,coverf,Ucomb_EGr
        USE UR_DLIM,              only: detlim
        use Rout,                 only: pending_events

        use UR_gtk_globals,       only: consoleout_gtk,replot_on
        use file_io,              only: logger
        use UR_params,            only: EPS1MIN, PI, ZERO, ONE, TWO


        implicit none

        integer   , intent(in)         :: NVAL
        real(rn), intent(in)           :: X(nval),Y(nval)
        character(len=*),intent(in)    :: TITLE
        integer   , intent(in)         :: mcasum

        real(kind=plflt)    :: mmxmin,mmxmax,mmymin,mmymax

        type(PLGraphicsIn)  :: gin

        integer             :: nset,i,k,izk,kmin,kmax
        integer             :: kk,kli,kre
        real(rn)            :: xmax,ymax,xmin,ymin,deltx,ddy

        character(LEN=150)  :: str1
        real(rn),allocatable  :: x1(:), y1(:), yy(:)
        character(LEN=10)   :: cnumber, devicen
        real(rn)            :: yu1,yu2,x12,mue,sdblue,ygmax,xmink,xmaxk,prgneg          ! sd replaced  ! 2025.01.23  GK
        real(rn)            :: xleft,xright,ylow,yhigh,ykmax,xmdiff
        real(rn)            :: xx1,yy1,YmaxFraction,xx1Last,yy1Last

        logical             :: st
        integer             :: kbutton
        integer             :: kq, iposi, maxposb
        real(rn)            :: sumred,sumredpos,sumblue,sumbluepos,yrescal
        character(len=11)   :: family(5)
        character(len=8)    :: style(3)
        character(len=512)  :: log_str
        character(len=7)    :: weight(2)

        data (family(i), i=1,5) / &
            "sans-serif", &
            "serif", &
            "monospace", &
            "script", &
            "symbol" /

        data (style(i), i=1,3) / &
            "upright", &
            "italic", &
            "oblique" /

        data (weight(i), i=1,2) / &
            "medium", &
            "bold" /


        ! data PLK_Escape /Z'1B'/
        !-----------------------------------------------------------------------
        !  print_graph = .false.
        call pending_events
        ! if(sDecimalPoint == '.')   call enable_locale_c(1)    ! 3.1.2018
        !--------------------------------------------------------------------------------
        call plgdev(devicen)         ! 16.5.2025

        allocate(x1(nval),y1(nval),yy(nval))
        x1(1:nval) = x(1:nval)
        y1(1:nval) = y(1:nval)

        call plgstrm(i)

        if(.not.replot_on) then
            write(log_str, '(*(g0))') 'ShowHist: kqtyp=',kqtyp,'  nval=',nval,' Title = ',trim(title)
            call logger(166, log_str)
            write(log_str, '(*(g0))') 'sum(y1)=',sngl(sum(y1(1:nval))),'  stepp=',sngl(stepp(kqtyp))
            call logger(166, log_str)
        end if

!  define graphics area to be just inside the edge of the page
        ! IF(print_graph) THEN
            xleft = 0.
            xright = ONE
            ylow = real(3-kqtyp, rn)/3.0_rn + 0.01_rn
            yhigh = real(4-kqtyp, rn)/3.0_rn - 0.01_rn
        !else
        !    xleft = ZERO
        !    xright = ONE
        !    ylow = ZERO
        !    yhigh = ONE
        !    ylow = real(3-kqtyp, rn) / 3.0_rn
        !    yhigh = real(4-kqtyp, rn) / 3.0_rn
        !end if

! TITLE = 'Histogram'
        nset = 1
        XMAX = MaxVal(X1,NVAL)
        XMIN = MinVal(X1,NVAL)
        YMAX = MaxVal(Y1,NVAL)

        if(kqtyp == 1 .and. use_shmima) then
            xmin = shmin(1)
            xmax = shmax(1)
            ! write(166,*) 'use_shmima=T:  xmin,xmax=',sngl(xmin),sngl(xmax)
        end if
        ymin = 1.e+30_rn
        ymax = -1.e+30_rn
        do i=1,NVAL
            if(X1(i) >= xmin) then
                if(y1(i) > ymax) ymax = y1(i)
                if(y1(i) < ymin) ymin = y1(i)
            end if
        end do

        YmaxFraction = ONE/300._rn    ! This prevents the maximum from being extended by the blue curve to quite large values.
        YmaxFraction = YmaxFraction /TWO

        do i=1,nval
            IF(x1(i) >= xmin .and. y1(i) > ymax*YmaxFraction) THEN
                XMIN = x1(i)
                kmin = i
                EXIT
            end if
        end do
        do i=nval,1,-1
            IF(x1(i) > xmin .and. y1(i) > ymax*YmaxFraction) THEN
                XMAX = x1(i)
                kmax = i
                EXIT
            end if
        end do
        ymin = ZERO

        ! 16.5.2025 GK -------------------------------
        ! xfakt eliminated: !  Plplot can handle the xfakt issue completely internally
        IF(use_shmima .and. shmax(kqtyp) > 0._rn) THEN
            do kq=1,3
                if(kq /= kqtyp) cycle
                if(shmax(kq) > zero) then
                  xmin = shmin(kq)
                  xmax = shmax(kq)
                  if(kq == 2 .and. shmax(3) > zero) xmax = shmax(3)
                  if(kq == 3 .and. shmax(3) > zero) then
                      xmin = shmin(2)
                      xmax = shmax(3)
                  end if
                end if
                if(.not.replot_on) then
                    WRITE(166,'(a,es11.4,2x,es11.4)') 'mmm    : xmin,xmax=',real(xmin,8),real(xmax,8)
                end if
            end do
        else
            IF(abs(shmax(kqtyp)) < eps1min .and. kqtyp > 1) THEN
                shmax(kqtyp) = xmax
                shmin(kqtyp) = xmin
                shfakt(kqtyp) = ONE
            end if
            if(.not.replot_on) then
                WRITE(166,'(a,es11.4,2x,es11.4)') 'mmm    : xmin,xmax=',real(xmin,8),real(xmax,8)
            end if
        end if
        ! -------------------------------------------

            if(.not.replot_on) then
                write(log_str, '(a,es11.4,2x,es11.4)') 'mmm    : xmin,xmax=',real(xmin,8),real(xmax,8)
                call logger(166, log_str)
            end if

        if(.not.replot_on) then
            write(log_str, '(a,i2,a,es11.4,2x,es11.4)') 'bbbbb   ShowHist: kqtyp=',kqtyp,'  xmin,xmax=', &
                real(xmin,8),real(xmax,8)
            call logger(166, log_str)
            write(log_str, '(a,L1,a,i4,a,es12.5)') '       use_shmima=',use_shmima,'  nval=',nval,'  Ucomb=',sngl(Ucomb)
            call logger(166, log_str)
        end if

! -------- Find the maximum position of the blue curve (Gaussian)
        mue = ZERO       ! 2025.01.23  GK
        sdblue = ZERO    !
        ! 20.1.2025  GK
        ! The variable s, for the gaussian standard deviation (ISO 11929-1), is
        ! replaced by the variable sdblue!

        select case (kqtyp)
          case (1)
            mue = MesswertSV(kEGr)
            sdblue = UComb_EGr/coverf
          case (2)
            mue = ZERO
            sdblue = UComb_DTv
          case (3)
            mue = detlim
            sdblue = UComb_DLv
          case default
        end select
        ygmax = ONE/(sdblue*SQRT(TWO*PI))
                    ! write(63,*) ' ygmax=',sngl(ygmax)
        ykmax = ZERO
        sumbluepos = ZERO    ! ! sum of probability ISO 11929 Gaussian (x(i) >= 0.) (blue curve))
        ! positive part of the blue curve:
        iposi = 1
        yy(1:nval) = ygmax * EXP(-((x1(1:nval)-mue)/sdblue)**two/two) ! blue curve (ISO 11929, part 1)
        if(.not.Gum_restricted) then
          iposi = -1
          do i=1,nval
            if(iposi < 0 .and. x1(i) >= ZERO) then
              iposi = i   ! used for .not.GUM_restricted
              exit
            end if
          enddo
        endif

        sumbluepos = sum(yy(iposi:nval))
        do i=nval+1,nval+15
          xx1 = x1(nval) + real(i,rn)*stepp(kqtyp)
          sumbluepos = sumbluepos + ygmax * EXP(-((xx1-mue)/sdblue)**two/two)
        end do
        ykmax = maxval(yy(iposi:nval))
        if(ykmax > zero .and. sumbluepos > zero) then
          ykmax = ykmax * 1.10_rn
          YMAX = MAX(YMAX, ykmax)
        end if

        ! ! xmink, xmaxk: region of the "visible part (> 0)" of the blue curve
        xmink = +1.E+15_rn
        xmaxk = -1.E+15_rn
        izk = 0
        kli = 10000
        kre = 10000
        maxposb = maxloc(yy,dim=1)
        do i=1,nval
            if(i == 1) then
                ! the negative part of the blue curve:
                prgneg = ZERO
                do k=1,10000
                    kk = k + 1
                    if(kli == 10000) then
                        xx1 = x1(maxposb) - kk*stepp(kqtyp)
                        yy1 = ygmax * EXP(-((xx1- mue)/sdblue)**two/two)
                        if(kqtyp >= 1 .and. xx1 < zero) prgneg = prgneg + yy1
                        IF(yy1 < ymax*YmaxFraction) kli = maxposb - k
                    end if
                    if(kre == 10000) then
                        xx1 = x1(max(1,nval/2)) + kk*stepp(kqtyp)
                        yy1 = ygmax * EXP(-((xx1 - mue)/sdblue)**two/two)
                        IF(k > nval/2 .and. yy1 < ymax*YmaxFraction) kre = max(1,nval/2) + k
                    end if
                    if(kli /= 10000 .and. kre /= 10000) exit
                end do
            end if
            IF(izk == 0 .AND. yy(i) < ymax*YmaxFraction) CYCLE
            izk = izk + 1
            IF(izk == 1) xmink = x1(i)
            IF(izk > 1 .AND. yy(i) < ymax*YmaxFraction) CYCLE
            IF(izk > 1) xmaxk = x1(i)
        end do
        prgneg = prgneg * stepp(kqtyp)
        sumbluepos = sumbluepos * stepp(kqtyp)
        if(sumbluepos > 0._rn) then
            ! yrescal = prg/(prgneg +   prg)
            yrescal = sumbluepos
        else
            yrescal = ONE
        end if
        if(kqtyp == 1) ymax = ymax*1.15_rn
        sumblue = sumbluepos + prgneg

        if(kqtyp == 1 .and. .not.replot_on)  then
            write(log_str, '(4(a,es11.4))') 'MC: yrescal=',yrescal,'  ymax=',ymax, &
            ' prgneg=',prgneg,' stepp=',stepp(kqtyp)
            call logger(63, log_str)
        end if
        ! --------

        if(.not.use_shmima) then
            xmin = MIN(xmin,xmink)
            xmax = MAX(xmax,xmaxk)
            IF(kli < 1) THEN
                kk = ABS(kli + 1)
                xmin = MIN(xmin, x1(1) - kk*(x1(2)-x1(1)) )
            end if
            IF(kre > nval) THEN
                kk = kre - nval
                xmax = MAX(xmax, x1(nval) + kk*(x1(2)-x1(1)) )
            end if
        else
            xmin = shmin(kqtyp)
            xmax = shmax(kqtyp)
        end if
        xmdiff = xmax - xmin
        xmin = xmin - xmdiff/25._rn
        xmax = xmax + xmdiff/25._rn

        if(kqtyp == 1) then
            call plclear()
        end if
        do while(cairo_get_reference_count(cc(1)) > 1_c_int)
            if(c_associated(cc(1))) call hl_gtk_drawing_area_cairo_destroy(cc(1))
        end do

!-----------------------------------------------------------------------------------------------------

        if(.not.replot_on .and. .not.PrintPlot_active) then
            write(log_str, '(a,es11.4,2x,es11.4,a,es11.4)') 'ShowHist:  Anfang Plotten: xmin,xmax=',real(xmin,8),real(xmax,8),  &
                '   xmdiff/25.=',real(xmdiff/25._rn,8)
            call logger(166, log_str)
        end if
        if(consoleout_gtk) write(0,*) 'ShowHist:  Anfang Plotten: xmin,xmax=',sngl(xmin),sngl(xmax)

        if(kqtyp == 1 .and. shmax(1) < EPS1MIN) then
            shmin(1) = xmin
            shmax(1) = xmax
        end if

        if(kqtyp == 1) call plclear()
        call plwidth(1.20d0)

        call plscolbg(255, 255, 255)

        call plcol0(1)   ! Graffer

        ! call plschr(0.d0, 1.6d0)
        ! call plschr(0.d0, 1.50d0)     !  this slightly decreasesd second argument
        !  prevents from cuts in the label numbers. really?
!//  call plschr(0.d0, 1.7d0)  ! 1.528d0)      !  with cairo.dll from version 5.14.0!!!
        ! call plschr(0.d0,1.0d0)      !  with cairo.dll from version 5.14.0!!!
        ! call plschr(5.0d0, 0.9d0)

        call plschr(0.0d0, 1.65d0)
        if(trim(devicen) == 'png') call plschr(0.0d0, 1.22d0)         ! 16.5.2025
        if(trim(devicen) == 'png') call plwidth(1.98d0)               !

        ! call plgchr(pdef,pht)
        !    write(66,*) 'plgchr:  pdef=',sngl(pdef),' pht=',sngl(pht)
        ! call plgdiplt(pxmin, pymin, pxmax, pymax)
        !    write(66,*) 'plgdiplt:  pxmin,pymin=',sngl(pxmin),sngl(pymin),' pxmax,pymax=',sngl(pxmax),sngl(pymax)
        ! call plgpage(pxp, pyp, pxleng, pyleng, pxoff, pyoff)
        !    write(66,*) 'plgpage:  pxp,pyp,pxleng,pyleng,pxoff,pyoff', &
        !                 sngl(pxp),sngl(pyp),pxleng,pyleng,pxoff,pyoff

        deltx = xmax - xmin
        call plsxax(4,2)

        call pladv(kqtyp)

        call pending_events

        if(consoleout_gtk) write(0,*) 'ShowHist:  device=',device

        do while(cairo_get_reference_count(cc(1)) > 1_c_int)
            if(c_associated(cc(1))) call hl_gtk_drawing_area_cairo_destroy(cc(1))
        end do

        write(cnumber,'(i9)') mcasum
        write(str1,'(a,a,a,a)') TRIM(title),',  MCsum=',adjustL(cnumber)

        !Prepare the plot box:
        call plvpor(0.18d0, 0.95d0, 0.21d0, 0.85d0)          ! 16.5.2025
        call plwind(real(xmin,8), real(xmax,8), real(ymin,8), real(ymax,8))
        call plgspa(mmxmin, mmxmax, mmymin,mmymax)
        write(log_str, '(*(g0))') 'plgspa: in mm: ',mmxmin,mmxmax,mmymin,mmymax
        ! call logger(66, log_str)
        ! call plbox('bcts', 0.d0, 0, 'bcvts', 0.d0, 0)
        call plbox('bctsn', 0.d0, 0, 'bcvtsn', 0.d0, 0)       ! 16.5.2025
        call pllab('', '', trim(str1))

        if(consoleout_gtk) write(0,*) 'nach plenv'

        call plcol0(1)         ! Graffer

        ! PLot the Histogram (red curve):
        call plcol0(8)        ! Graffer
        call plwidth(0.0d0)
        call plwidth(0.9d0)
        sumred = ZERO
        sumredpos = ZERO
        x12 = stepp(kqtyp)/TWO
        do i=1,nval
            IF(x1(i) < xmin) CYCLE
            IF(x1(i) > xmax) CYCLE
            sumred = sumred + y1(i)
            if(x1(i) >= ZERO) sumredpos = sumredpos + y1(i)
            call plline((/real(x1(i)-x12,8)/), (/0.d0/) )
            call pljoin(real(x1(i)-x12,8), 0.d0, real(x1(i)-x12,8), real(y1(i),8))
            call pljoin(real(x1(i)-x12,8), real(y1(i),8), real(x1(i)+x12,8), real(y1(i),8))
            call pljoin(real(x1(i)+x12,8), real(y1(i),8), real(x1(i)+x12,8), 0.d0)
        end do

        call plcol0(1)
        ddy = EPS1MIN
        call pljoin(real(xmin,8), real(ymin+ddy,8), real(xmax,8), real(ymin+ddy,8) )

        ! Add the curve for the ISO 11929 gaussian curve (blaue Kurve):
        call plcol0(4)        !9: blue   ! Graffer
        call plwidth(1.6d0)
        if(trim(devicen) == 'png') call plwidth(2.2d0)     ! 16.5.2025

        izk = 0
        ! do i=kli,kre
        xx1last = ZERO    ! 2025.01.23 GK
        yy1last = ZERO    !
        xx1 = ZERO
        do i=kli,kre
            IF(i >=1 .AND. i <= nval) THEN
                xx1 = x1(max(i,1))
                yy1 = yy(max(i,1))
            else
                IF(i < 1 ) THEN
                    ! i runs here from kli (negative) through 0:
                    kk = ABS(i) + 1
                    xx1 = x1(1) - kk*stepp(kqtyp)
                    if(xx1 < xmin) cycle
                end if
                IF(i > nval) THEN
                    kk = i - nval
                    xx1 = x1(nval) + kk*stepp(kqtyp)
                end if
                yy1 = ygmax * EXP(-((xx1-mue)/sdblue)**TWO/two)
            end if
            if(kqtyp == 1) then
                if( (.not. Gum_restricted .and. xx1 >= ZERO) .or. Gum_restricted)  then
                    call pljoin(real(xx1last,8), real(yy1last/yrescal,8), real(xx1,8), real(yy1/yrescal,8))
                end if
            elseif(kqtyp > 1) then
                if(abs(xx1last) < 1.E-12_rn .and. abs(yy1last) < 1.E-12_rn) then
                    ! 28.10.2025 GK
                    xx1last = xx1
                    yy1last = yy1
                end if
                call pljoin(real(xx1last,8), real(yy1last,8), real(xx1,8), real(yy1,8))
            end if
            xx1last = xx1
            yy1last = yy1
        end do
        sumred = sumred * stepp(kqtyp)
        sumredpos = sumredpos * stepp(kqtyp)
        if(kqtyp == 1) sumbluepos = sumbluepos / yrescal
        if(.not.replot_on) then
            write(log_str, '(5(a,f7.5,2x),a,2i5)') 'sumblue=',sumblue,' sumred=',sumred,' sumredpos=',sumredpos,  &
                ' sumblue/sumred=',sumblue/sumred,' sumbluepos=',sumbluepos, &
                '  kli,kre=',kli,kre
            call logger(63, log_str)
        end if
        if(consoleout_gtk) write(0,*) 'nach 1. Schleife'
        if(.not.replot_on) then
            write(log_str, '(a,i2,a,es11.4,2x,es11.4,2(a,es11.4),a,i5)') 'Showhist: kqtyp=',kqtyp, &
                '  sdblue, mue=',real(sdblue,8),real(mue,8),'  gaussmax=',real(ygmax,8), &
                '  bin-width=',real(stepp(kqtyp),8),'  nval=',nval
            call logger(166, log_str)
        end if
!  Add vertical lines:
        yu1 = ZERO
        yu2 = ymax

        call plcol0(4)           ! 4: dark blue
        call pllsty(3)
        select case (kqtyp)
          case (1)
            do k=1,3
                call pljoin(real(VertLines(k),8),real(yu1,8), real(VertLines(k),8),real(yu2,8))
            end do
          case (2)
            call pljoin(real(VertLines(4),8),real(yu1,8), real(VertLines(4),8),real(yu2,8))
          case (3)
            do k=5,6
                call pljoin(real(VertLines(k),8),real(yu1,8), real(VertLines(k),8),real(yu2,8))
            end do
          case default
        end select
        call pllsty(1)
        call pending_events()
        if(consoleout_gtk) write(0,*) 'nach pllsty'

        !--------------------------------------------------------------------------------
        call plwidth(1.2d0)
        call plcol0(1)          ! Graffer

        call plaxes(real(xmin,8), real(ymin+ddy,8), 'ats', 0.d0, 0, '   ', 0.d0, 0)

        call gtk_widget_queue_draw(drawing(1))
        call pending_events
        ! Note: call plend is executed in MCStart, after the call to MCCalc,
        !          or in Plotsub1/PrintPlot

        if(.false.) then
            ! read cursor position : not used in UncertRadio
            ! plgetcursor up to now works only with 'wingcc', but unvisible cursor
            call plxormod(.true., st)
            call pending_events
            gin%px = 0
            gin%py = 0
            gin%button = -1
            do while(.true.)
                ! plGetCursor returns zero value for some devices (e.g., xwin) when
                ! the user hits the escape key or hits a mouse button or key when outside
                ! a subwindow.
                k = plGetCursor( gin )

                if(k == 0 ) exit

                ! For some devices (e.g., xcairo) plGetCursor always returns 0, but if the user hits the escape
                ! key or hits a mouse button or key when outside a subwindow, the returned keysym is PLK_escape
                !//     if( gin%keysym == PLK_escape ) exit

                write(log_str, "(a,i3,a,f0.6,a,f0.6,a,f0.6,a,f0.6)") &
                    "subwin = ", gin%subwindow, ", wx = ", gin%wX, ", wy = ", gin%wY, &
                    ", dx = ", gin%dX, ", dy = ", gin%dY
                call logger(66, log_str)
                write(log_str, "(a,z0,a,z0,a,a,z0,a,z0)") &
                    "keysym = 0x", gin%keysym, ", button = 0x", gin%button, ", string = '"//trim(gin%string)//"'", &
                    ", type = 0x", gin%type, ", state = 0x", gin%state
                call logger(66, log_str)
                write(log_str, '(4(a,f0.7))') 'xoff=',xoff,' yoff=',yoff,' xscale=',xscale,' yscale=',yscale
                call logger(66, log_str)
                call plpoin( (/gin%wX/), (/gin%wY/), 9 )
                if(kbutton == 1) then
                    write(log_str, '(*(g0))') 'PLGetCursor:  gin%wx=',gin%wx,'  gin%wy=',gin%wy,'  gin%keysym=',gin%keysym, &
                        '  gin%button=',gin%button,' st=',st
                    call logger(66, log_str)
                    exit
                end if
            end do
        end if

        call gtk_widget_queue_draw(drawing(1))
        call pending_events()

        !-------------------------------------------------------------------------------------------
        return

    end subroutine ShowHist2
!
!#######################################################################

    subroutine Replot(kpi)

!     Copyright (C) 2014-2025  Günter Kanisch

        USE UR_MCC,             only: nval
        USE plplot_code_sub1,   only: scalable, three_in_one, PrepareF
        use ur_general_globals, only: Gum_restricted, actual_plot,pngfile,cairo_png_reloaded, &
                                      png_to_cairo_surface
        use UR_gtk_globals,     only: plinit_done, plot_setintern
        use Plplot,             only: plend1, plsfnam

        use file_io,            only: logger
        use Rout,               only: pending_events
        USE UR_MCC,             ONLY: iopt_copygr


        implicit none

        integer   ,intent(in)    :: kpi

        ! integer              :: kqtx
        character(len=512)   :: log_str
        character(len=12)    :: act_plot

        if(plot_setintern) return

! return
        if(kpi == 1) act_plot = 'MCplot'
        if(kpi == 2) act_plot = 'BSplot'
        if(kpi == 3) act_plot = 'CurvePlot'
        if(kpi == 4) act_plot = 'ELIplot'

        if(kpi /= 1)  then     !  .and. kpi /= 3) then
            call CairoPlplotPrepare(act_plot)
            call PrepareF(act_plot)
        end if

        ! call Printplot:
        if(kpi == 1) then

  if(cairo_png_reloaded) then
    ! if cosntruct added 20.5.2025 GK
    ! instead of saving the extcairo pixbuf (which can have small optical problems):
    ! repeat the steps for the whole graphics output, but now directly into a png file
    ! 20.5.2025 GK
    png_to_cairo_surface = .true.
    actual_plot = 'MCplot'
    call CairoPlplotPrepare(actual_plot)
    scalable = .false.
    familying = .false.
    gform = 'png'
    iopt_copygr = 1
    png_to_cairo_surface = .true.
    call plsfnam(trim(pngfile))
         ! write(0,*) 'Replot, vor printplot: pngfile=',trim(pngfile)
    call Printplot()
         ! write(0,*) 'Replot: pngfile=',trim(pngfile)
    call reload_pngfile(pngfile)

  else

            do kqtx=1,3
                if(GUM_restricted .and. kqtx > 1) exit
                write(log_str, '(*(g0))') 'Replot 1: nval(kqtx=',nval(kqtx)
                call logger(66, log_str)
                call PlotSteps(kqtx,'')
                !IF(nval(kqtx) > 0) THEN
                !  kqtyp = kqtx
                !  call ShowHist2(xplt(1,kqtx),yplt(1,kqtx),NVAL(kqtx),Title(kqtx),mcasum(kqtx))
                !end if
                if(scalable .and. .not. three_in_one) then
                    call plend1()
                    plinit_done = .false.
                end if
            end do
            call plend1()
        end if
  end if

        if(kpi == 3) then
            actual_plot = 'CurvePlot'
            call PlotSteps(1,'')
            call pending_events
        end if
        plinit_done = .false.

    end subroutine Replot

!#############################################################################################

    subroutine PlotEli

!     Copyright (C) 2014-2025  Günter Kanisch
        use plplot
        use gtk,              only: gtk_widget_queue_draw, gtk_widget_show
        use cairo,            only: cairo_get_reference_count
        use gtk_draw_hl,      only: hl_gtk_drawing_area_cairo_destroy, &
                                    hl_gtk_drawing_area_get_gdk_pixbuf
        use gdk_pixbuf_hl,    only: hl_gdk_pixbuf_new_file,hl_gdk_pixbuf_save

        use UR_params,          only: PI, EPS1MIN, ZERO, ONE, TWO
        use ur_general_globals, only: plot_ellipse,actual_plot,results_path
        use UR_Gleich_globals,  only: einheit,GrFormat
        use UR_gtk_globals,     only: plinit_done

        use UR_GaussInt,      only: xmean, ux, ymean, uy, rho
        use plplot_code_sub1, only: PrepareF, scalable
        use UR_eli,           only: xmin, xmax, g1, scf, ymin, ymax, vect, ascale, &
                                    exmax, exmin, eymax, eymin, st, theta, ct, alphamin, &
                                    p1, p2, x1min, pltitle, xachse, yachse, distmain, angle, &
                                    x1, x2, Acomb_TR, Acomb_TRS, xt, yt, nptsel, dangle, angmin, &
                                    angmax, dist, angmain, xp1, yp1, y1, y2, xx0, yy0, xs, ys
        use UR_Linft,         only: eliRS
        use UR_MCC,           only: iopt_copygr
        use CHF,              only: lowercase
        use file_io,          only: logger


        implicit none

        integer :: i
        character(len=:),allocatable :: plfile
        character(len=512)           :: log_str
        type(c_ptr)                  :: pixbuf

        ! Graphical testing:
        ! plot_confidoid = .true.
        plot_ellipse = .true.
        three_in_one = .false.
        scalable = .false.
        if(iopt_copygr == 4) scalable = .true.    ! pdf-format

        allocate(character(len=255) :: plfile)

        gform = lowercase(GrFormat(iopt_copygr)%s(1:4))

        actual_plot = 'ELIplot'
        call CairoPlplotPrepare(actual_plot)
        call PrepareF(actual_plot)

        !if(sDecimalPoint == '.')  call enable_locale_c(1)    ! 3.1.2018

        if(eliRS == 0) then
            xmin = xmean - 1.2_rn*sqrt(g1)*ux * scf(1)
            xmax = xmean + 1.2_rn*sqrt(g1)*ux * scf(1)
            ymin = ymean - 1.2_rn*sqrt(g1)*uy * scf(2)
            ymax = ymean + 1.2_rn*sqrt(g1)*uy * scf(2)
        else
            vect = matmul(Ascale,(/-1.2_rn*sqrt(g1)*ux, ZERO, ZERO, ONE/))
            xmin = vect(1) + xmean
            vect = matmul(Ascale,(/+1.2_rn*sqrt(g1)*ux, ZERO, ZERO, ONE/))
            xmax = vect(1) + xmean
            vect = matmul(Ascale,(/ZERO, -1.2_rn*sqrt(g1)*uy, ZERO, ONE/))
            ymin = vect(2) + ymean
            vect = matmul(Ascale,(/ZERO, +1.2_rn*sqrt(g1)*uy, ZERO, ONE/))
            ymax = vect(2) + ymean
        end if

        call plclear()

        call plwidth(1.20d0)
        call plscolbg(255, 255, 255)
        call plcol0(1)   ! Graffer
        call plschr(0.d0, 1.2d0)
        ! call plschr(4.d0, 1.d0)     ! first parameter: reference height in mm; 2nd parameter: scaling factor
        ! call plprec(1, 3)         ! 3 decimals

        call plenv(real(xmin,8), real(xmax,8), real(ymin,8), real(ymax,8), 0, 0)
        call plcol0(1)         ! Graffer
        call plwidth(1.2d0)
        call pllsty (1)

        exmin = 1.E+10_rn
        exmax = -1.E+10_rn
        eymin = 1.E+10_rn
        eymax = -1.E+10_rn

        st = sin(theta)
        ct = cos(theta)

        alphamin = atan(p2*st/(p1*ct))
        x1min = xmean + p1*cos(alphamin)*ct + p2*sin(alphamin)*st
        write(log_str, '(*(g0))') '    alphamin (Grad), 1. Fall: ',sngl(alphamin*180._rn/Pi),'  exmin analytisch: ',sngl(x1min)
        call logger(66, log_str)
        if(alphamin >= Pi) then
            alphamin = alphamin - Pi
        else
            alphamin = alphamin + Pi
        end if
        x1min = xmean + p1*cos(alphamin)*ct + p2*sin(alphamin)*st
        write(log_str, '(*(g0))') '    alphamin (Grad), 2. Fall: ',sngl(alphamin*180.d0/Pi),'  exmin analytisch: ',sngl(x1min)
        call logger(66, log_str)

        write(pltitle,'(a,F6.3,1x,2(a,es9.2,1x),a,f6.2)') 'rho=',real(rho,8),'p1=',real(p1,8), &
            'p2=',real(p2,8),'theta(grd)=',real(theta*180.d0/Pi,8)       ! ,' shift=',addy
        call pllab(trim(xachse)//' / '//einheit(1)%s, trim(yachse)//' / '//einheit(1)%s, trim(pltitle))

        call plcol0(4)        !9: blue   ! Graffer
        distmain = ZERO

! Plot the middle-cross:
        call pllsty (2)
        call pljoin(real(xmean,8), real(ymin,8), real(xmean,8), real(ymax,8))
        call pljoin(real(xmin,8), real(ymean,8), real(xmax,8), real(ymean,8))

! Plot the lines for +- 1 Sigma:
        call pljoin(real(xmean-sqrt(g1)*ux,8), real(ymin,8), real(xmean-sqrt(g1)*ux,8), real(ymax,8))
        call pljoin(real(xmean+sqrt(g1)*ux,8), real(ymin,8), real(xmean+sqrt(g1)*ux,8), real(ymax,8))
        call pljoin(real(xmin,8), real(ymean-sqrt(g1)*uy,8), real(xmax,8), real(ymean-sqrt(g1)*uy,8))
        call pljoin(real(xmin,8), real(ymean+sqrt(g1)*uy,8), real(xmax,8), real(ymean+sqrt(g1)*uy,8))

        call plwidth(3.0d0)
        angle = ZERO
        x1 =  p1*cos(angle)    ! positive axe of the ellipse
        x2 =  p2*sin(angle)
        vect = matmul(Acomb_TRS,(/x1,x2,ZERO,ONE/))

! Plot the ellipse:
        call plwidth(1.2d0)
        call pllsty (1)
        if(allocated(xt)) deallocate(xt,yt)
        allocate(xt(nptsel),yt(nptsel))
        do i=1,nptsel
            angle = real(i-1,rn)/200._rn*(TWO*pi)
            x1 =  p1*cos(angle)
            x2 =  p2*sin(angle)
            vect = matmul(Acomb_TR,(/x1,x2,ZERO,ONE/))
            xt(i) = vect(1)
            yt(i) = vect(2)
            dangle = angle*180._rn/Pi
            if(i == 10*(i/10)  .or. int(dangle+0.001) == 90*(int(dangle+0.001)/90)) then
                write(log_str, '(5(a,f9.3))') ' Angle=',sngl(angle*180._rn/Pi),' x=',sngl(x1),' y=',sngl(x2), &
                    '  xtrans=',sngl(xt(i)),'  ytrans=',sngl(yt(i))
                call logger(66, log_str)
            end if
            exmin = min(exmin,xt(i))
            exmax = max(exmax,xt(i))
            if(abs(xt(i)-exmin) < EPS1MIN) angmin = angle
            if(abs(xt(i)-exmax) < EPS1MIN) angmax = angle
            eymin = min(eymin,yt(i))
            eymax = max(eymax,yt(i))
            dist = sqrt((xt(i)-xmean)**TWO + (yt(i)-ymean)**TWO)
            if(dist > distmain) then
                distmain = dist
                angmain = angle
                xp1 = xt(i)
                yp1 = yt(i)
            end if
            if(i > 1) call pljoin(real(xt(i-1),8), real(yt(i-1),8), real(xt(i),8), real(yt(i),8))

        end do

!plot (p1,0) in the rotated coordinate system: al * symbols
        call plssym(0.d0, 3.d0)          ! Symbol size: Arg-1: 0: defaut;  Arg-2: Scaling factor
        vect = matmul(Acomb_TRS, (/p1, ZERO, ZERO, ONE/))
        vect = matmul(Acomb_TRS, (/ZERO, p2, ZERO, ONE/))
        vect = matmul(Acomb_TRS, (/ZERO, ZERO, ZERO, ONE/))

! Plotten des Diagonalen-Kreuzes der Ellipse:
!         write(66,*) 'First semi axis: Length=',sngl(distmain),'  Angle=',sngl(angmain*180._rn/Pi),' xp1=',sngl(xp1),' yp1=',sngl(yp1)
        write(log_str, '(*(g0))') 'First semi axis: Length=',sngl(distmain),'  Angle=',sngl(angmain*180._rn/Pi),' xp1=',sngl(xp1),' yp1=',sngl(yp1)
        call logger(66, log_str)
        angle = ZERO
        vect = matmul(Acomb_TR,(/p1*cos(angle), p2*sin(angle), ZERO, ONE/))
        x2 = vect(1)
        y2 = vect(2)
        write(log_str, '(*(g0))') ' calculated: x2=',sngl(x2),'  y2=',sngl(y2)
        call logger(66, log_str)
        angle = angle + Pi
        vect = matmul(Acomb_TR,(/p1*cos(angle), p2*sin(angle), ZERO, ONE/))
        x1 = vect(1)
        y1 = vect(2)
        call pljoin(real(x1,8),real(y1,8), real(x2,8),real(y2,8))
        write(log_str, '(*(g0))') 'Length of the rotated first axis: ',sngl(sqrt((x2-x1)**TWO+(x2-y1)**TWO))
        call logger(66, log_str)

        angle = Pi/TWO
        vect = matmul(Acomb_TR,(/p1*cos(angle), p2*sin(angle), ZERO, ONE/))
        x2 = vect(1)
        y2 = vect(2)
        angle = angle + Pi
        vect = matmul(Acomb_TR,(/p1*cos(angle), p2*sin(angle), ZERO, ONE/))
        x1 = vect(1)
        y1 = vect(2)
        call pljoin(real(x1,8),real(y1,8), real(x2,8),real(y2,8))
        write(log_str, '(*(g0))') 'Length of the rotated second axis: ',sngl(sqrt((x2-x1)**TWO+(x2-y1)**TWO))
        call logger(66, log_str)

        if(.false.) then
            xx0 = p1
            yy0 = ZERO
            xs = xx0*cos(-theta) + yy0*sin(-theta)
            ys = -xx0*sin(-theta) + yy0*cos(-theta)
            xs = xs +xmean
            ys = ys +ymean
            call plssym(0.d0, 2.0d0)
            call plpoin((/real(xs,8)/),(/real(ys,8)/),3)

            xx0 = ZERO
            yy0 = p2
            xs = xx0*cos(-theta) + yy0*sin(-theta)
            ys = -xx0*sin(-theta) + yy0*cos(-theta)
            xs = xs +xmean
            ys = ys +ymean
            call plpoin((/real(xs,8)/),(/real(ys,8)/),2)
        end if

        call gtk_widget_queue_draw(drawing(4))

        call plend1()
        plinit_done = .false.
        call gtk_widget_show(drawing(4))

        do while (cairo_get_reference_count(cc(4)) > 1_c_int)
            call hl_gtk_drawing_area_cairo_destroy(cc(4))
        end do

        if(trim(gform) == 'png') then
            pixbuf = hl_gtk_drawing_area_get_gdk_pixbuf(drawing(4))
            plfile = 'Confid_plotfile.png'
            plfile = trim(results_path) // trim(plfile)
            call hl_gdk_pixbuf_save(pixbuf, plfile, 'png')
        end if

        !if(trim(gform) == 'pdf') then
        !  pixbuf = hl_gdk_pixbuf_new_file(plfile)
        !  call hl_gtk_drawing_area_draw_pixbuf(drawing(4), pixbuf)
        !end if

        if(allocated(xt)) deallocate(xt,yt,plfile)

    end subroutine PlotEli

!#########################################################################


    real(rn) function quantileM(p,x,n)
        use UR_params,   only: ZERO

        implicit none

        real(rn),intent(in)   :: p           ! probability
        integer   ,intent(in) :: n           ! number of values in the sorted array x
        real(rn),intent(in)   :: x(n)        ! Array of values

        integer            :: j, npos, nn
        real(rn)           :: ggamma
        !-----------------------------------------------------------------------
        ! Literature:
        !             Quantile:
        !             R.J. Hyndman & Y. Fan: Sample Quantiles in statistical Packages.
        !             The American Statistitian, Vol. 50(4), Nov. 1996, 361-365
        !             Their definition 9, according to Blom (1958)

        !             Quantile variance:
        !             K.Y. Cheung & S.M.S. Lee: VARIANCE ESTIMATION FOR SAMPLE QUANTILES USING THE m OUT
        !             OF n BOOTSTRAP. Ann. Inst. Statist. Math. Vol. 57, No. 2, 279-290 (2005)
        !             Their Eq. (1.1)

        nn = n
        npos =  0
        if(nn <= 3) then
            quantileM = ZERO
            return
        end if

        j = INT( p*(real(nn,rn) + 0.25_rn) + 3.0_rn/8.0_rn )
        j = max(1,j)
        j = min(nn-1,j)
        ggamma = p*real(nn,rn) - real(j,rn)

        !write(28,*) 'QuantileM: j=',int(j,4),'  ggamma=',sngl(ggamma),'  (npos+j)=',int(npos+j,4), &
        !         ' nn=',nn,'  p=',sngl(p),' npos=',int(npos,4)

        quantileM = (1.0_rn - ggamma)*x(npos+j) + ggamma*x(npos+j+1)

    end function quantileM

!#######################################################################

    subroutine Reload_pngfile(pngfile)

        use, intrinsic :: iso_c_binding,          only: c_ptr, c_null_char, c_int, c_associated

        use gtk_draw_hl,            only: hl_gtk_drawing_area_cairo_destroy, &
                                          hl_gtk_drawing_area_draw_pixbuf
        use gdk_pixbuf_hl,          only: hl_gdk_pixbuf_new_file
        USE ur_general_globals,     only: png_to_cairo_surface
        use cairo,                  only: cairo_get_reference_count
        use plplot_code_sub1

        implicit none

        character(len=*), intent(in)  :: pngfile

        type(c_ptr)           :: pixbuf
        integer(c_int)        :: ccounts

        ! Re-load the just saved png file back as a pixbuf to the cairo-surface:
        do while(cairo_get_reference_count(cc(1)) > 1_c_int)
            if(c_associated(cc(1))) call hl_gtk_drawing_area_cairo_destroy(cc(1))
        end do
        ccounts = cairo_get_reference_count(cc(1))
        pixbuf = hl_gdk_pixbuf_new_file(pngfile)    ! ,width,height,aspect,error)
        call hl_gtk_drawing_area_draw_pixbuf(drawing(1),pixbuf,0_c_int,0_c_int)
        png_to_cairo_surface = .false.

    end subroutine Reload_pngfile

!#######################################################################



end module PLsubs
