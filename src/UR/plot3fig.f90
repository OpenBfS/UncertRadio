
!   Based on:

!   Simple line plot and multiple windows demo.
!
!   Copyright (C) 2004-2016  Alan W. Irwin
!
!   This file is part of PLplot.
!
!   PLplot is free software; you can redistribute it and/or modify
!   it under the terms of the GNU Library General Public License as
!   published by the Free Software Foundation; either version 2 of the
!   License, or (at your option) any later version.
!
!   PLplot is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU Library General Public License for more details.
!
!   You should have received a copy of the GNU Library General Public
!   License along with PLplot; if not, write to the Free Software
!   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

!     N.B. the pl_test_flt parameter used in this code is only
!     provided by the plplot module to allow convenient developer
!     testing of either kind(1.0) or kind(1.0d0) floating-point
!     precision regardless of the floating-point precision of the
!     PLplot C libraries.  We do not guarantee the value of this test
!     parameter so it should not be used by users, and instead user
!     code should replace the pl_test_flt parameter by whatever
!     kind(1.0) or kind(1.0d0) precision is most convenient for them.
!     For further details on floating-point precision issues please
!     consult README_precision in this directory.
!
subroutine plot3fig(knum,nkpts,ncurve,line_styles,line_widths,xlog,ylog,xlab,ylab,ptitle,pltfile, &
                                                 mimax,mimay,ctextL)

    use plplot, double_PI => PL_PI
    use :: iso_fortran_env, only: stdout => output_unit

    use UR_VARIABLES,       only: sec_strm,gtk_strm
    use UR_gtk_variables,   only: plinit3_done,plinit_done

    implicit none

    integer(4),intent(in)                 :: knum           ! number of curves
    integer(4),intent(in)                 :: nkpts(knum)    ! number of points per curve
    integer(4),intent(in)                 :: ncurve(knum)   ! numbers of curve shapes
    integer(4),intent(in)                 :: line_styles(knum)   ! numbers of curve shapes
    real(8),intent(in)                    :: line_widths(knum)   ! numbers of curve shapes
    real(8),intent(in),optional           :: mimax(2)      ! xminv,xmaxv
    real(8),intent(in),optional           :: mimay(2)      ! yminv,ymaxv
    logical,intent(in)                    :: xlog,ylog
    character(len=*),intent(in)           :: xlab,ylab
    character(len=*),intent(in),optional  :: ctextL(knum)
    character(len=*),intent(in)           :: Ptitle
    character(len=*), intent(in)          :: pltfile
          character(len=50)  :: device,geometry
          real(8)            :: dummy
          integer            :: plsetopt_rc
    integer :: PLK_Escape
    data PLK_Escape /Z'1B'/

    call plgstrm(sec_strm)
    if(sec_strm == 0) then
      sec_strm = 1
      ! call plmkstrm(sec_strm)  ! crashes
    end if
    call plsstrm(sec_strm)        ! 24.1.2021
       !!!! write(0,*) 'start plot3fig: sec_strm=',int(sec_strm,2)

    if(.true.) then
      device = "pngcairo"
      call plsdev("pngcairo")
          plsetopt_rc = plsetopt("device",trim(device))     !  "locale" lower case!!
      call plsfnam(pltfile)
      goto 100
    end if

100 continue

    call enable_locale_c(1)

    !  Print plplot version
    ! call plgver(version)
    ! write (*,'(a,a)') 'PLplot library version: ', trim(version)

    !  Initialize plplot
        call plscolbg(255, 255, 255)

    write(geometry, "(I0,'x',I0)") 1100, 700
    plsetopt_rc = plsetopt("geometry",  geometry)

   ! plsetopt_rc = plsetopt("locale", "LC_numeric=German_Germany.1252")     !  "locale" klein!!!

   if(.not.plinit3_done) then
      plinit_done = .true.
      ! call plinit()
      call plstar(1,1)
   end if

    !  Do a plot
    call plot2(knum,nkpts,ncurve,line_styles,line_widths,xlog,ylog,xlab,ylab,ptitle,pltfile, &
                                                  mimax,mimay,ctextL)
    call enable_locale_c(2)

   call plend1()   ! end plotting session for current stream
             plinit3_done = .false.

    call plsstrm(gtk_strm)          ! für extcairo  ! 11.3.2021

         ! write(0,*) 'nach 200, after call plend:'
    return

contains

    !======================================================================
    subroutine plot2(knum,nkpts,ncurve,line_styles,line_widths,xlog,ylog,xlab,ylab,ptitle,pltfile, &
                                                 mimax,mimay,ctextL)

    use UR_plotp,        only: pltx,plty,xminv,xmaxv,yminv,ymaxv
    use Num1,            only: quick_sort2_i
    implicit none

    integer(4),intent(in)                 :: knum           ! number of curves
    integer(4),intent(in)                 :: nkpts(knum)    ! number of points per curve
    integer(4),intent(in)                 :: ncurve(knum)   ! numbers of curve shapes
    integer(4),intent(in)                 :: line_styles(knum)   ! numbers of curve shapes
    real(8),intent(in)                    :: line_widths(knum)   ! numbers of curve shapes
    real(8),intent(in),optional           :: mimax(2)      ! xminv,xmaxv
    real(8),intent(in),optional           :: mimay(2)      ! yminv,ymaxv
    logical,intent(in)                    :: xlog,ylog
    character(len=*),intent(in)           :: xlab,ylab
    character(len=*),intent(in),optional  :: ctextL(knum)
    character(len=*),intent(in)           :: Ptitle
    character(len=*), intent(in)          :: pltfile
    integer(4),allocatable                :: ncurvec(:)

       !  real(pl_test_flt), dimension(1:60) :: x, y
        real(8)       :: xmin, xmax, ymin, ymax

        ! Define colour map 0 to match the "GRAFFER" colour table in
        ! place of the PLPLOT default.
        integer(4)          :: i,k, ii, indx(6)  ! ,n2curve(6),plsetopt_rc   ! , plparseopts_rc
        integer(4), parameter :: rval(16) = (/255, 0, 255, &                 !original values
         & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
         & gval(16) = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
         & 0, 0, 85, 170/), &
         & bval(16) = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
         & 127, 85, 170/)

        integer(4)          :: posopt      ! between 1 and 16
        !!! integer(4)         :: line_styles(knum)
        integer(4)         :: line_colors(knum)
        !!!! real(8)            :: line_widths(knum)
        real(8)            :: text_scale,legend_width,legend_height
        character(50)      :: textL(6)       ! textL2(6),

!--------------------------------------------------------------------------------------
! Farbschema, with "Graffer":
! p. 95 (122) im Manual
! 0 black (default background)
! 1 red (default foreground)
! 2 yellow
! 3 green
! 4 aquamarine
! 5 pink
! 6 wheat
! 7 grey
! 8 brown
! 9 blue
! 10 BlueViolet
! 11 cyan
! 12 turquoise
! 13 magenta
! 14 salmon
! 15 white
!-----------------------------------------------------------------------------------

    call enable_locale_c(1)

    if(present(mimax)) then
      xminv = mimax(1)
      xmaxv = mimax(2)
    else
      xminv = 1.D+30
      ymaxv = -1.D+30
      do i=1,knum
        dummy = minval(pltx(i,1:nkpts(i)))
        xminv = min(xminv,dummy)
        dummy = maxval(pltx(i,1:nkpts(i)))
        xmaxv = max(xmaxv,dummy)
      end do
    end if
    if(present(mimay)) then
      yminv = mimay(1)
      ymaxv = mimay(2)
    else
      yminv = 1.D+30
      ymaxv = -1.D+30
      do i=1,knum
        dummy = minval(plty(i,1:nkpts(i)))
        yminv = min(yminv,dummy)
        dummy = maxval(plty(i,1:nkpts(i)))
        ymaxv = max(ymaxv,dummy)
      end do
    end if
    if(.not.present(ctextL)) then
      textL(1:knum) = ' '
    else
      do i=1,knum
        textL(i) = trim(ctextL(i))
      end do
    end if

            if(nkpts(knum) == 0) return
        xmin = xminv - (xmaxv-xminv)/25.d0
        xmax = xmaxv + (xmaxv-xminv)/25.d0
        ymin = yminv - (ymaxv-yminv)/25.d0
        ymax = ymaxv + (ymaxv-yminv)/25.d0

         ! write(*,*) 'xmin,xmax,ymin,ymax=',sngl(xmin),sngl(xmax),sngl(ymin),sngl(ymax)

        call plprec(0, 5)
        if(xmax > xmin .and. xmax-xmin < 3.D-4) call plprec(1, 5)

        !   Set up the viewport and window using PLENV. The range in X is
        !   0.0 to 6.0, and the range in Y is 0.0 to 30.0. The axes are
        !   scaled separately (just = 0), and we just draw a labelled
        !   box (axis = 0).

             call plwidth(1.40d0)
             ! call plspal0("cmap0_default.pal")
             call plspal0("cmap0_black_on_white.pal")
             call plscmap0n(10)
             call plscmap0(rval, gval, bval)
             call plscolbg(255, 255, 255)

        call plcol0(1)
             call plschr(0.d0, 1.08d0)
             call plschr(0.d0, 1.2d0)

        if(.not.xlog .and. .not. ylog) call plenv( xmin, xmax, ymin, ymax, 0, 0 )    !
        if(xlog .and. .not. ylog) call plenv( xmin, xmax, ymin, ymax, 0, 10 )    ! 10: x logari
        if(.not.xlog .and. ylog) call plenv( xmin, xmax, ymin, ymax, 0, 20 )    ! 20: y logari
        if(xlog .and. ylog) call plenv( xmin, xmax, ymin, ymax, 0, 30 )    ! 10: x,y logari

           call plwidth(0.7d0)
           call pllsty (2)      ! short dashes

        if(.not.xlog .and. .not. ylog) call plbox('gt', 0., 0, 'gt', 0., 0)
        if(xlog .and. .not. ylog) call plbox('gl', 0., 0, 'g', 0., 0)    ! x logarithm., with Gitternetz
        if(.not.xlog .and. ylog) call plbox('gt', 0., 0, 'gl', 0., 0)    ! y logarithm., with Gitternetz
        if(xlog .and. ylog) call plbox('gl', 0., 0, 'gl', 0., 0)    ! x,y logarithm., with Gitternetz

           call plwidth(1.40d0)
           call pllsty (1)    ! full line
        call plcol0(1)
        call pllab( trim(xlab), trim(ylab), trim(ptitle))

        !   Plot the data points
        call plcol0(4)
        call plwidth(1.70d0)
        ii = 0
            ! write(0,*)'ncurve=',int(ncurve(1:knum),2)
        allocate(ncurvec(knum))
        ncurvec = ncurve
        call quick_sort2_i(ncurvec,indx)
        do k=1,knum
          if(ncurve(indx(k)) > 0) ii = ii + 1
        end do

           !! line_styles = 0
           line_colors = 0
           !! line_widths = 0.d0

        ii = 0
        do k=1,knum
          i = indx(k)
             i = k

          if(ncurve(i) == 0) cycle
          if(nkpts(i) == 0) cycle

          ii = ii + 1
         ! call plcol0(ncurve(i))
          call plschr(0.d0, 1.0d0)
           !!! line_styles(ii) = ncurve(i)

           ! line_colors(ii) = ncurve(i)
           if(ii == 1) line_colors(ii) = 1
           if(ii == 2) line_colors(ii) = 2
           if(ii == 3) line_colors(ii) = 4
           if(ii == 4) line_colors(ii) = 6
           if(ii == 5) line_colors(ii) = 8
               call plcol0(line_colors(ii))

           !! line_widths(ii) = 3.0d0
           !! if(ii == 1) line_widths(ii) = 4.5d0
           !! if(nkpts(i) >= 800) line_widths(ii) = 1.0d0
           call plwidth(line_widths(ii))
           call pllsty(ncurve(i))
           if(line_styles(ii) < 50) then
               call plline( pltx(i,1:nkpts(i)), plty(i,1:nkpts(i)))
           else
             call plssym(0.d0, 0.4d0)
               call plpoin(pltx(i,1:nkpts(i)), plty(i,1:nkpts(i)), line_styles(ii)-50)
           end if
        end do
      !!  call plwidth(1.70d0)

        if(.true. .and. knum > 1) then
            ! write(0,*) 'before call PrepLegend:'
            ! write(0,*) 'size(line_styles)=',size(line_styles),'  ii=',ii
          text_scale = 0.9d0  !  0.8d0
          posopt = 11  ! 16
          call PrepLegend(ii,posopt,line_styles(1:ii),line_colors(1:ii),line_widths(1:ii),text_scale,   &
                           legend_width,legend_height,textL )
                ! write(50,*) 'legend_width=',legend_width,'legend_height=',legend_height
        end if
              call enable_locale_c(2)

        return

    end subroutine plot2

 subroutine enable_locale_c(mode)
    use, intrinsic :: iso_c_binding
    implicit none

    integer(4), intent(in)     :: mode       !  1: enable C locale;  2: disable C locale

    integer(c_int)         :: ccat
    character(kind=c_char, len=20), target :: locname

    interface
        subroutine setlocale(category, locale) bind(c)
            use, intrinsic :: iso_c_binding, only: c_ptr, c_int
            integer(c_int), value :: category
            type(c_ptr), value :: locale
        end subroutine
    end interface
!----------------------------------------------------------------
	! Call 'setlocale' function. Set locale to C default.
    ccat = 0
    if(mode == 1) then
      locname = "C"//c_null_char
      call setlocale(ccat, c_loc(locname(1:1)))
    elseif(mode == 2) then
      ! Restore system locale.
      locname = ""//c_null_char
      call setlocale(ccat, c_loc(locname(1:1)))
    end if

end subroutine enable_locale_c

!------------------------------------------------------------------------------------------------------
    subroutine PrepLegend(nlegend,posopt,line_styles,line_colors,line_widths,text_scale, &
                          legend_width,legend_height,textL)

    use plplot
    use :: iso_fortran_env, only: stdout => output_unit

    implicit none
         !!!!  kind _pl_test_flt gleich 8 (d0) gesetzt
    integer(4),parameter      :: wp = pl_test_flt    !  8

    integer(4),intent(in)     :: nlegend
    integer(4),intent(in)     :: posopt      ! between 1 and 16
    character(len=*),intent(in)   :: textL(nlegend)
    integer(4),intent(in)     :: line_styles(nlegend)
    integer(4),intent(in)     :: line_colors(nlegend)
    real(kind=wp),intent(in)  :: line_widths(nlegend)
    real(kind=wp),intent(in)  :: text_scale

    real(kind=pl_test_flt),intent(out)   :: legend_width,legend_height

    integer(4)               :: opt,position
    integer(4)               :: bg_color, bb_color, bb_style
    integer(4)               :: opt_array(nlegend)
    integer(4)               :: text_colors(nlegend)
    integer(4)               :: box_colors(nlegend)
    integer(4)               :: box_patterns(nlegend)
    real(kind=wp)            :: box_scales(nlegend)
    real(kind=wp)            :: box_line_widths(nlegend)
    integer(4)               :: symbol_numbers(nlegend), symbol_colors(nlegend)
    real(kind=wp)            :: symbol_scales(nlegend)
    character(len=20)        :: symbols(nlegend)
    real(wp)   :: x, y, plot_width,text_justification,text_offset,text_spacing
    integer(4)               :: opt_base, nrow, ncolumn
    integer(4)               :: position_options(16)
    real(kind=wp)            :: values_small(2)
    real(kind=wp)            :: values_uneven(9)
    real(kind=wp)            :: values_even(9)
    integer(4), parameter    :: COLORBAR_KINDS = 4
    integer(4)               :: colorbar_option_kinds(COLORBAR_KINDS)
    character(len=100)       :: colorbar_option_kind_labels(COLORBAR_KINDS)
    integer(4), parameter    :: COLORBAR_POSITIONS = 4
    integer(4)               :: colorbar_position_options(COLORBAR_POSITIONS)
    character(len=100)       :: colorbar_position_option_labels(COLORBAR_POSITIONS)
    integer(4), parameter    :: COLORBAR_LABELS = 4
    integer(4)               :: colorbar_label_options(COLORBAR_LABELS)
    character(len=100)       :: colorbar_label_option_labels(COLORBAR_LABELS)
    integer(4), parameter    :: COLORBAR_CAPS = 4
    integer(4)               :: colorbar_cap_options(COLORBAR_CAPS)
    character(len=100)       :: colorbar_cap_option_labels(COLORBAR_CAPS)

    real(kind=wp), parameter :: small_factor = 1.d-20

    values_small = (/ -1.0_wp, 1.0_wp /)
    values_uneven = (/ &
            -1.0_wp, 2.0_wp, 2.6_wp, 3.4_wp, &
           6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, &
           10.0_wp /)
    values_even = (/  &
           -2.0_wp, -1.0_wp, 0.0_wp, 1.0_wp, &
           2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
           6.0_wp /)

    values_small = small_factor*values_small
    values_uneven = small_factor*values_uneven
    values_even = small_factor*values_even

    position_options(1) = PL_POSITION_LEFT + PL_POSITION_TOP + PL_POSITION_OUTSIDE
    position_options(2) = PL_POSITION_TOP + PL_POSITION_OUTSIDE
    position_options(3) = PL_POSITION_RIGHT + PL_POSITION_TOP + PL_POSITION_OUTSIDE
    position_options(4) = PL_POSITION_RIGHT + PL_POSITION_OUTSIDE
    position_options(5) = PL_POSITION_RIGHT + PL_POSITION_BOTTOM + PL_POSITION_OUTSIDE
    position_options(6) = PL_POSITION_BOTTOM + PL_POSITION_OUTSIDE
    position_options(7) = PL_POSITION_LEFT + PL_POSITION_BOTTOM + PL_POSITION_OUTSIDE
    position_options(8) = PL_POSITION_LEFT + PL_POSITION_OUTSIDE
    position_options(9) = PL_POSITION_LEFT + PL_POSITION_TOP + PL_POSITION_INSIDE
    position_options(10) = PL_POSITION_TOP + PL_POSITION_INSIDE
    position_options(11) = PL_POSITION_RIGHT + PL_POSITION_TOP + PL_POSITION_INSIDE
    position_options(12) = PL_POSITION_RIGHT + PL_POSITION_INSIDE
    position_options(13) = PL_POSITION_RIGHT + PL_POSITION_BOTTOM + PL_POSITION_INSIDE
    position_options(14) = PL_POSITION_BOTTOM + PL_POSITION_INSIDE
    position_options(15) = PL_POSITION_LEFT + PL_POSITION_BOTTOM + PL_POSITION_INSIDE
    position_options(16) = PL_POSITION_LEFT + PL_POSITION_INSIDE

    ! plcolorbar options

    ! Colorbar type options
    colorbar_option_kinds(1) = PL_COLORBAR_SHADE
    colorbar_option_kinds(2) = PL_COLORBAR_SHADE + PL_COLORBAR_SHADE_LABEL
    colorbar_option_kinds(3) = PL_COLORBAR_IMAGE
    colorbar_option_kinds(4) = PL_COLORBAR_GRADIENT

    colorbar_option_kind_labels(1) = "Shade colorbars"
    colorbar_option_kind_labels(2) = "Shade colorbars with custom labels"
    colorbar_option_kind_labels(3) = "Image colorbars"
    colorbar_option_kind_labels(4) = "Gradient colorbars"

    ! Which side of the page are we positioned relative to?
    colorbar_position_options(1) = PL_POSITION_LEFT
    colorbar_position_options(2) = PL_POSITION_RIGHT
    colorbar_position_options(3) = PL_POSITION_TOP
    colorbar_position_options(4) = PL_POSITION_BOTTOM

    colorbar_position_option_labels(1) = "Left"
    colorbar_position_option_labels(2) = "Right"
    colorbar_position_option_labels(3) = "Top"
    colorbar_position_option_labels(4) = "Bottom"

    ! Colorbar label positioning options
    colorbar_label_options(1) = PL_COLORBAR_LABEL_LEFT
    colorbar_label_options(2) = PL_COLORBAR_LABEL_RIGHT
    colorbar_label_options(3) = PL_COLORBAR_LABEL_TOP
    colorbar_label_options(4) = PL_COLORBAR_LABEL_BOTTOM

    colorbar_label_option_labels(1) = "Label left"
    colorbar_label_option_labels(2) = "Label right"
    colorbar_label_option_labels(3) = "Label top"
    colorbar_label_option_labels(4) = "Label bottom"

    ! Colorbar cap options
    colorbar_cap_options(1) = PL_COLORBAR_CAP_NONE
    colorbar_cap_options(2) = PL_COLORBAR_CAP_LOW
    colorbar_cap_options(3) = PL_COLORBAR_CAP_HIGH
    colorbar_cap_options(4) = PL_COLORBAR_CAP_LOW + PL_COLORBAR_CAP_HIGH

    colorbar_cap_option_labels(1) = "No caps"
    colorbar_cap_option_labels(2) = "Low cap"
    colorbar_cap_option_labels(3) = "High cap"
    colorbar_cap_option_labels(4) = "Low and high caps"

    !     Only specify legend data that are required according to the
    !     value of opt_array for that entry.
    opt_base          = PL_LEGEND_BACKGROUND + PL_LEGEND_BOUNDING_BOX
    opt_array(1:nlegend)      = PL_LEGEND_LINE + PL_LEGEND_SYMBOL

    symbol_scales  = 1._wp

    bg_color = 0  ! 0   ! 15
    bb_color = 1
    nrow     = nlegend ! rows in legend box
    ncolumn  = 1

    position = position_options(posopt)
    opt = opt_base
    text_colors(1:nlegend) = 1

       box_colors = 0
       box_patterns = 0
         box_scales = 0._wp
         box_line_widths = 0._wp
         symbol_colors = 1

      x = 0.05_wp
      y = 0.05_wp
      plot_width = 0.1_wp
      bb_style = 1
      text_offset = 1.0_wp
      text_spacing = 2.0_wp
      text_justification = 0._wp
      symbol_scales = 0._wp
      symbol_numbers = 0
      symbols = ''

       !  write(0,*) 'directly before call pllegend:  nlegend=',nlegend
       !  write(0,*) 'opt_array: ',size(opt_array)
       !  write(0,*) 'text_colors: ',size(text_colors)
       !  write(0,*) 'box_patterns: ',size(box_patterns)
       !  write(0,*) 'box_scales: ',size(box_scales)
       !  write(0,*) 'box_line_widths: ',size(box_line_widths)
       !  write(0,*) 'line_colors: ',size(line_colors)
       !  write(0,*) 'line_widths: ',size(line_widths)
       !  write(0,*) 'symbol_colors: ',size(symbol_colors)
       !  write(0,*) 'symbol_scales: ',size(symbol_scales)
       !  write(0,*) 'symbol_numbers: ',size(symbol_numbers)
       !  write(0,*) 'symbols: ',size(symbols)

   ! Note: One should not use variables in PLPLOT, which have not been initiated (set to 0),
   ! because they then could get very large values!

    call pllegend( legend_width, legend_height, opt, position, x, y, &
         plot_width, bg_color, bb_color, bb_style, nrow, ncolumn,     &
         opt_array, text_offset, text_scale, text_spacing, &
         text_justification, text_colors, textL,     &
         box_colors, box_patterns, box_scales, &
         box_line_widths, line_colors, line_styles, &
         line_widths, symbol_colors, symbol_scales, &
         symbol_numbers, symbols  )

    write(0,*) 'legend_width,legend_height=',sngl(legend_width),sngl(legend_height)

    end subroutine PrepLegend

    !======================================================================

end subroutine plot3fig


