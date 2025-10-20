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

module tom748

    use UR_types,    only: wp => rn

    type mek
        real(wp)      :: ftol, rtol, atol
        real(wp)      :: fvalue
        integer       :: maxiter
        integer       :: mode
        real(wp)      :: xx
        integer       :: munit
    end type
    type(mek) :: me

end module tom748

!*****************************************************************************************

module root748

    use file_io, only: logger

contains



!*****************************************************************************************
!
!   source:   (downloaded 26.09.2025)
!
!   https://jacobwilliams.github.io/roots-fortran/src/root_module.F90
!

!>
!  TOMS748 rootfinding method.
!
!  Finds either an exact solution or an approximate solution of the
!  equation `f(x)=0` in the interval [ax,bx]. At the begining of each
!  iteration, the current enclosing interval is recorded as [a0,b0].
!  The first iteration is simply a secant step. Starting with the
!  second iteration, three steps are taken in each iteration. First
!  two steps are either quadratic interpolation or cubic inverse
!  interpolation. The third step is a double-size secant step. If the
!  diameter of the enclosing interval obtained after those three steps
!  is larger than 0.5*(b0-a0), then an additional bisection step will
!  be taken.
!
!### References
!  * http://www.netlib.org/toms/748
!  * G. E. Alefeld, F. A. Potra and Yixun Shi,
!    "Algorithm 748: Enclosing Zeros of Continuous Functions",
!    ACM Transactions on Mathematical Software,
!    Vol. 21. No. 3. September 1995. Pages 327-344.

    ! subroutine toms748(me,ax,bx,fax,fbx,xzero,fzero,iflag)
    ! subroutine toms748(ax,bx,xzero,fzero,iflag, f,fvalue, ftol,rtol,atol,maxiter, &

    subroutine toms748(ax,bx,xzero,fzero,iflag,itnum, fvalue, ftol, rtol, atol, maxiter, mode)
        use Rout,                only: WDPutEntryInt,pending_events
        use UR_MCC,              only: kqtypx,imcmax,xmit1,xmit1min
        use UR_Gleich_globals,   only: ifehl
        use tom748
        use UR_Linft,            only: klincall
        use UR_params,           only: EPS1MIN
        use ur_general_globals,  only: MCsim_on,bxiter_on
        use UR_DLIM,             only: modeB

        implicit none

        ! integer, PARAMETER :: wp = 10
        ! class(toms748_solver),intent(inout) :: me
        real(wp),intent(in)  :: ax      !! left endpoint of initial interval
        real(wp),intent(in)  :: bx      !! right endpoint of initial interval
        !real(wp),intent(in)  :: fax     !! `f(ax)`
        !real(wp),intent(in)  :: fbx     !! `f(ax)`
        real(wp),intent(out) :: xzero   !! abscissa approximating a zero of `f` in the interval `ax`,`bx`
        real(wp),intent(out) :: fzero   !! value of `f` at the root (`f(xzero)`)
        integer,intent(out)  :: iflag   !! status flag (`0`=root found, `-2`=max iterations reached)
        integer,intent(out)  :: itnum

        !real(wp)             :: f
        real(wp),intent(in)  :: fvalue
        real(wp),intent(in)  :: ftol
        real(wp),intent(in)  :: rtol
        real(wp),intent(in)  :: atol
        integer,intent(in)   :: maxiter

        integer, intent(in)  :: mode

        ! type(mek)         :: me

        !integer  :: itnum
        integer  :: mqt,munit
        real(wp) :: a,b,fa,fb,c,u,fu,a0,b0,tol,d,fd
        real(wp) :: prof,e,fe,tmpc
        logical  :: success, prout
        real(wp)              :: amin, bmin, fbmin, famin, brx, start

        ! Extension for UR: ...........................
        real(wp),allocatable    :: xmarr(:), ymarr(:), uymarr(:), xmitarr(:), arrmin(:)
        character(len=512)      :: log_str
        ! .............................................

        iflag = 0
        a = ax
        b = bx

        ! Extension for UR ...........................................:
        !!! modeSV = mode
        modeB = mode
        if(.not.allocated(xmarr)) allocate(xmarr(60),ymarr(60),uymarr(60),xmitarr(60))

        prout = .false.
        prout = .true.
        if(mode == 3 .or. mode == 2) prout = .true.

        if(mode == 1) prout = .true.
        if(MCsim_on ) prout = .true.
        prout = .true.
        ! external  f
        me%ftol = ftol
        me%rtol = rtol
        me%atol = atol
        me%maxiter = maxiter
        me%fvalue = fvalue

        munit = 63
        mqt = kqtypx

        bxiter_on = .true.
        allocate(arrmin(imcmax))
        ifehl = 0
        if(MCsim_on) xmit1min = xmit1

        if(mode <= 3) then
            mqt = kqtypx
            munit = 30
            if(mode == 2 .or. mode == 3) munit = 63
        end if
        if(mode == 14 .or. mode == 15) then
            munit = 63
            mqt = kqtypx
        end if
        me%mode = mode
        me%munit = munit

        write(log_str, '(a,i0,a,2es11.4,2(a,es11.4))') 'mqt=',mqt,' x1,x2=',ax,bx, &
            '  fvalue=',fvalue,'  ftol=',ftol
        call logger(munit, log_str)
        if(mode == 3) klincall = 0
        call cpu_time(start)
        !---------------------------------------------------------------------------------
        ! part for enlarging the bracketing interval (x1,x2) to (x1w,x2w), if necessary
        call check_bracket(mqt,munit,a,b,fa,fb,success)
        if(.not.success) then
            ifehl = 1
            return
        end if
        !.....................................................................
        ! Extension for UR, a few of them like this one: ................................
        write(log_str,'(a,i0,a,2es11.4,3(a,es11.4))') 'mqt=',mqt,' a,b=',a,b, &
                      ' fa=',fa,' fb=',fb,'  fvalue=',fvalue
        call logger(munit, log_str)
        !................................................................................

        ! initialization. set the number of iteration as 0.
        ! set dumb values for the variables "e" and "fe".
        e  = huge(1.0_wp)
        fe = huge(1.0_wp)

        ! iteration starts. the enclosing interval before executing the
        ! iteration is recorded as [a0, b0].
        do itnum = 1, me%maxiter

            call WDputEntryInt('TRentryMCit',itnum)
            call pending_events()
            if(MCsim_on) then
                call treat_xmit(itnum,mqt,a,b,fa,fb,amin,bmin,famin,fbmin,arrmin)
            end if

            write(log_str,'(a,i0,a,2es13.6,3(a,es13.6))') 'itnum=',itnum,' a,b=',a,b, &
                ' fa=',fa,' fb=',fb,'  fvalue=',fvalue
            call logger(munit, log_str)

            if(itnum == 8 .and. MCsim_on .and. mqt == 2) then
                if(itnum == 8) call logger(munit, '+++++++++++++++ brentx exceeds maximum iterations')
                if(itnum >= 4) call treat_mean_DT(itnum,xmarr,ymarr,uymarr,brx)
            end if

            a0 = a
            b0 = b

            ! calculates the termination criterion. stops the procedure if the
            ! criterion is satisfied.

            if (abs(fb) <= abs(fa)) then
                ! tol = get_tolerance(b)
                tol = 2.0_wp * (me%atol + 2.0_wp*abs(b)*me%rtol)
            else
                ! tol = get_tolerance(a)
                tol = 2.0_wp * (me%atol + 2.0_wp*abs(a)*me%rtol)
            end if
            ! write(63,*) 'itnum=',itnum,' tol=',sngl(tol),' me%atol=',sngl(me%atol), ' me%rtol=',sngl(me%rtol), &
            !                      ' me%ftol=',sngl(me%ftol)

            if ((b-a)<=tol) exit

            ! for the first iteration, secant step is taken.
            if (itnum == 1) then

                c=a-(fa/(fb-fa))*(b-a)

                ! call subroutine "bracket" to get a shrinked enclosing interval as
                ! well as to update the termination criterion. stop the procedure
                ! if the criterion is satisfied or the exact solution is obtained.
                ! call bracket(a,b,c,fa,fb,tol,d,fd, f)
                call bracket(a,b,c,fa,fb,tol,d,fd)
                if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit
                cycle

            end if

            ! starting with the second iteration, in the first two steps, either
            ! quadratic interpolation is used by calling the subroutine "newqua"
            ! or the cubic inverse interpolation is used by calling the subroutine
            ! "pzero". in the following, if "prof" is not equal to 0, then the
            ! four function values "fa", "fb", "fd", and "fe" are distinct, and
            ! hence "pzero" will be called.
            prof=(fa-fb)*(fa-fd)*(fa-fe)*(fb-fd)*(fb-fe)*(fd-fe)
            if ((itnum == 2) .or. (abs(prof) <= EPS1MIN)) then
                call newqua(a,b,d,fa,fb,fd,c,2)
            else
                c = pzero(a,b,d,e,fa,fb,fd,fe)
                if ((c-a)*(c-b) >= 0.0_wp) then
                    call newqua(a,b,d,fa,fb,fd,c,2)
                end if
            end if
            e=d
            fe=fd

            ! call subroutine "bracket" to get a shrinked enclosing interval as
            ! well as to update the termination criterion. stop the procedure
            ! if the criterion is satisfied or the exact solution is obtained.
            call bracket(a,b,c,fa,fb,tol,d,fd)      ! , f)
            if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit

            prof=(fa-fb)*(fa-fd)*(fa-fe)*(fb-fd)*(fb-fe)*(fd-fe)
            if (abs(prof) <= EPS1MIN) then
                call newqua(a,b,d,fa,fb,fd,c,3)
            else
                c = pzero(a,b,d,e,fa,fb,fd,fe)
                if ((c-a)*(c-b) >= 0.0_wp) then
                    call newqua(a,b,d,fa,fb,fd,c,3)
                end if
            end if

            ! call subroutine "bracket" to get a shrinked enclosing interval as
            ! well as to update the termination criterion. stop the procedure
            ! if the criterion is satisfied or the exact solution is obtained.
            call bracket(a,b,c,fa,fb,tol,d,fd)   ! , f)
            if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit
            e=d
            fe=fd

            ! takes the double-size secant step.
            if (abs(fa) < abs(fb)) then
                u=a
                fu=fa
            else
                u=b
                fu=fb
            end if
            c=u-2.0_wp*(fu/(fb-fa))*(b-a)
            if (abs(c-u) > (0.5_wp*(b-a))) then
                c=a+0.5_wp*(b-a)
            end if

            ! call subroutine bracket to get a shrinked enclosing interval as
            ! well as to update the termination criterion. stop the procedure
            ! if the criterion is satisfied or the exact solution is obtained.
            call bracket(a,b,c,fa,fb,tol,d,fd)   ! , f)
            if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit

            ! determines whether an additional bisection step is needed. and takes
            ! it if necessary.
            if ((b-a) < (0.5_wp*(b0-a0))) cycle
            e=d
            fe=fd

            ! call subroutine "bracket" to get a shrinked enclosing interval as
            ! well as to update the termination criterion. stop the procedure
            ! if the criterion is satisfied or the exact solution is obtained.
            tmpc = a+0.5_wp*(b-a)
            call bracket(a,b,tmpc,fa,fb,tol,d,fd)  ! , f)
            if ((abs(fa)<=me%ftol) .or. ((b-a)<=tol)) exit

            if (itnum == me%maxiter) iflag = -2    ! maximum iterations reached


        end do

        !return result:

        xzero = a
        fzero = fa

        write(log_str,'(a,i0,a,2es13.6,2(a,es13.6))') 'itnum=',itnum,'  final root:  a,b=',a,b,' fa=',fa,' fb=',fb       ! addition for UR
        call logger(munit, log_str)

    end subroutine toms748
    !*****************************************************************************************

    !!! contains

    !************************************************************************
    subroutine bracket(a,b,c,fa,fb,tol,d,fd)  ! ,f )

        !!  Given current enclosing interval [a,b] and a number c in (a,b), if
        !!  f(c)=0 then sets the output a=c. Otherwise determines the new
        !!  enclosing interval: [a,b]=[a,c] or [a,b]=[c,b]. Also updates the
        !!  termination criterion corresponding to the new enclosing interval.

        use tom748

        implicit none

        real(wp),intent(inout)  :: a    !! input as the current left point of the
        !! enclosing interval and output as the shrinked
        !! new enclosing interval
        real(wp),intent(inout)  :: b    !! input as the current right point of the
        !! enclosing interval and output as the shrinked
        !! new enclosing interval
        real(wp),intent(inout)  :: c    !! used to determine the new enclosing interval
        real(wp),intent(inout)  :: fa   !! f(a)
        real(wp),intent(inout)  :: fb   !! f(b)
        real(wp),intent(inout)  :: tol  !! input as the current termination
        !! criterion and output as the updated termination
        !! criterion according to the new enclosing interval
        real(wp),intent(out)    :: d    !! if the new enclosing interval
        !! is [a,c] then d=b, otherwise d=a;
        real(wp),intent(out)    :: fd   !! f(d)
        ! integer(4),intent(in)   :: mode

        real(wp) :: fc, Prfunc   ! , get_tolerance
        !! integer(4)    :: isign3
        ! type(mek)         :: me

        ! external   f

        ! adjust c if (b-a) is very small or if c is very close to a or b.
        tol = 0.7_wp*tol
        if ((b-a) <= 2.0_wp*tol) then
            c = a+0.5_wp*(b-a)
        else if (c <= a+tol) then
            c = a+tol
        else
            if (c >= b-tol) c = b-tol
        end if

        ! call subroutine to obtain f(c)
        ! fc = me%f(c)
        fc = Prfunc(me%mode,c ) - me%fvalue
        ! write(63,*) 'bracket: fc=',sngl(fc),' c=',sngl(c),' me%mode=',me%mode

        ! if c is a root, then set a=c and return. this will terminate the
        ! procedure in the calling routine.
        if (abs(fc) <= me%ftol) then

            a   = c
            fa  = fc
            d   = 0.0_wp
            fd  = 0.0_wp

        else

            ! if c is not a root, then determine the new enclosing interval.
            if ((isign3(fa)*isign3(fc)) < 0) then
                d   = b
                fd  = fb
                b   = c
                fb  = fc
            else
                d   = a
                fd  = fa
                a   = c
                fa  = fc
            end if

            ! update the termination criterion according to the new enclosing interval.
            if (abs(fb) <= abs(fa)) then
                !tol = get_tolerance(b)
                tol = 2.0_wp * (me%atol + 2.0_wp*abs(b)*me%rtol)
            else
                !tol = get_tolerance(a)
                tol = 2.0_wp * (me%atol + 2.0_wp*abs(a)*me%rtol)
            end if

        end if

    end subroutine bracket
    !************************************************************************

    !************************************************************************
    ! pure function isign3(x) result(i)
    function isign3(x) result(i)
        use UR_params, only: EPS1MIN
        use tom748

        !! sign of the variable `x` (note: return `0` if `x=0`)

        implicit none

        integer :: i
        real(wp),intent(in) :: x

        if (x > 0.0_wp) then
            i = 1
        else if (abs(x) <= EPS1MIN) then
            i = 0
        else
            i = -1
        end if

    end function isign3
    !************************************************************************

    !************************************************************************
    ! pure function get_tolerance(b) result(tol)
    function get_tolerance(b) result(tol)

        !! determines the termination criterion.

        use tom748
        implicit none

        real(wp),intent(in) :: b
        real(wp) :: tol  !! termination criterion: 2*(2*rtol*|b| + atol)

        tol = 2.0_wp * (me%atol + 2.0_wp*abs(b)*me%rtol)

    end function get_tolerance
    !************************************************************************

    !************************************************************************
    ! pure subroutine newqua(a,b,d,fa,fb,fd,c,k)
    subroutine newqua(a,b,d,fa,fb,fd,c,k)

        !! uses k newton steps to approximate the zero in (a,b) of the
        !! quadratic polynomial interpolating f(x) at a, b, and d.
        !! safeguard is used to avoid overflow.
        use UR_params, only: EPS1MIN
        use tom748

        implicit none

        real(wp),intent(in)  :: a
        real(wp),intent(in)  :: b
        real(wp),intent(in)  :: d  !! d lies outside the interval [a,b]
        real(wp),intent(in)  :: fa !! f(a), f(a)f(b)<0
        real(wp),intent(in)  :: fb !! f(b), f(a)f(b)<0
        real(wp),intent(in)  :: fd !! f(d)
        real(wp),intent(out) :: c  !! the approximate zero
        !! in (a,b) of the quadratic polynomial.
        integer,intent(in)   :: k  !! number of newton steps to take.

        integer  :: ierror,i     ! , isign3
        real(wp) :: a0,a1,a2,pc,pdc

        ! initialization
        ! find the coefficients of the quadratic polynomial
        ierror = 0
        a0 = fa
        a1 = (fb-fa)/(b-a)
        a2 = ((fd-fb)/(d-b)-a1)/(d-a)

        do    ! main loop

            ! safeguard to avoid overflow
            if ((abs(a2) <= EPS1MIN) .or. (ierror == 1)) then
                c=a-a0/a1
                return
            end if

            ! determine the starting point of newton steps
            if (isign3(a2)*isign3(fa) > 0) then
                c=a
            else
                c=b
            end if

            ! start the safeguarded newton steps
            do i=1,k
                if (ierror == 0) then
                    pc=a0+(a1+a2*(c-b))*(c-a)
                    pdc=a1+a2*((2.0_wp*c)-(a+b))
                    if (abs(pdc) <= EPS1MIN) then
                        ierror=1
                    else
                        c=c-pc/pdc
                    end if
                end if
            end do
            if (ierror/=1) exit

        end do

    end subroutine newqua
    !************************************************************************

    !************************************************************************
    !pure function pzero(a,b,d,e,fa,fb,fd,fe) result(c)
    function pzero(a,b,d,e,fa,fb,fd,fe) result(c)

        !! uses cubic inverse interpolation of f(x) at a, b, d, and e to
        !! get an approximate root of f(x). this procedure is a slight
        !! modification of aitken-neville algorithm for interpolation
        !! described by stoer and bulirsch in "Intro. to numerical analysis"
        !! springer-verlag. new york (1980).

        use tom748
        implicit none

        real(wp),intent(in) :: a,b,d,e,fa,fb,fd,fe
        real(wp) :: c

        real(wp) :: q11,q21,q31,d21,d31,q22,q32,d32,q33

        q11 = (d-e)*fd/(fe-fd)
        q21 = (b-d)*fb/(fd-fb)
        q31 = (a-b)*fa/(fb-fa)
        d21 = (b-d)*fd/(fd-fb)
        d31 = (a-b)*fb/(fb-fa)

        q22 = (d21-q11)*fb/(fe-fb)
        q32 = (d31-q21)*fa/(fd-fa)
        d32 = (d31-q21)*fd/(fd-fa)
        q33 = (d32-q22)*fa/(fe-fa)

        c = a + q31+q32+q33

    end function pzero
    !************************************************************************


    !*****************************************************************************************

    subroutine check_bracket(mqt,munit,a,b,fa,fb,success)
        use tom748
        use UR_Gleich_globals,    only: ifehl
        implicit none

        integer,intent(in)      :: mqt,munit
        real(wp),intent(inout)  :: a,b
        real(wp),intent(inout)  :: fa,fb
        logical,intent(out)     :: success

        integer              :: kj
        real(wp)             :: fkt,Prfunc
        character(len=512)   :: log_str

        fa = Prfunc(me%mode,a) - me%fvalue
        fb = Prfunc(me%mode,b) - me%fvalue

        if(fa*fb > 0._wp) then
            write(log_str,*) 'bracket. problem: a,b=',sngl(a),sngl(b),' fa,fb=',sngl(fa),sngl(fb), &
                ' fa*fb=',sngl(fa*fb)
            call logger(munit, log_str)

            if(fa < 0._wp .and. fb < 0._wp) then
                fkt = -1._wp
            else
                fkt = 1._wp
            end if
            if(mqt == 3 .and. (me%mode == 1 .or. me%mode == 2) ) fkt = -fkt
            if(abs(fa) < abs(fb)) then
                do kj=1,20
                    a = a * 1.3_wp**fkt
                    fa = Prfunc(me%mode,a) - me%fvalue
                    write(log_str,*) 'kj=',int(kj,2),' a=',sngl(a),' fa=',sngl(fa),' mode=',int(me%mode,2)
                    call logger(munit, log_str)
                    if(fa*fb < 0._wp) exit
                end do
            else
                do kj=1,20
                    b = b * 1.3_wp**fkt
                    fb = Prfunc(me%mode,b) - me%fvalue
                    write(log_str,*) 'kj=',int(kj,2),' b=',sngl(b),' fb=',sngl(fb),' mode=',int(me%mode,2)
                    call logger(munit, log_str)
                    if(fa*fb < 0._wp) exit
                end do
            end if
        end if
        success = .true.

        if(fa*fb > 0._wp) then
            success = .false.
            write(log_str,*) 'toms748: bracketing not successful!  Stopped'
            call logger(munit, log_str)
            write(log_str,*) 'bracket. problem: a,b=',sngl(a),sngl(b),' fa,fb=',sngl(fa),sngl(fb)
            call logger(munit, log_str)
            ifehl = 1
            return
        end if

    end subroutine check_bracket

    !*****************************************************************************************

    subroutine treat_xmit(itnum,mqt,a,b,fa,fb,amin,bmin,famin,fbmin,arrmin)
        use tom748
        use UR_MCC,       only: xmit1,xmit1min,arraymc,imctrue

        implicit none

        integer,intent(in)      :: itnum
        integer,intent(in)      :: mqt
        real(wp),intent(in)     :: a,b,fa,fb
        real(wp),intent(inout)  :: amin,bmin,famin,fbmin
        real(wp),allocatable    :: arrmin(:)

        if(mqt == 2) then
            if(itnum == 1 .or. (abs(fb) < abs(fa) .and. abs(fb) < fbmin)) then
                arrmin(1:imctrue) = arraymc(1:imctrue,mqt)
                xmit1min = xmit1
                fbmin = abs(fb)
                famin = abs(fa)
                amin = a
                bmin = b
            end if
        end if
        if(mqt /= 2) then
            arrmin(1:imctrue) = arraymc(1:imctrue,mqt)
            xmit1min = xmit1
            if(abs(fb) < abs(fa) .and. abs(fb) < fbmin) then
                fbmin = abs(fb)
                famin = abs(fa)
                amin = a
                bmin = b
            end if
        end if
    end subroutine treat_xmit

    !*****************************************************************************************

    subroutine treat_mean_DT(itnum,xmarr,ymarr,uymarr,brentx)
        use tom748
        use Brandt,       only: Lsqlin

        use UR_Linft,     only: mfix,xfix,indfix
        use KLF,          only: CalibInter,funcsKB
        use UWB,          only: median

        implicit none

        integer,intent(in)       :: itnum
        real(wp),allocatable     :: xmarr(:), ymarr(:), uymarr(:)
        integer,allocatable      :: list(:)
        real(wp),intent(out)     :: brentx

        integer               :: i,itr,nall
        real(wp)              :: xmid,x_1,chisq
        character(len=512)    :: log_str
        real(wp),allocatable  :: pa(:),covpa(:,:)
        logical               :: fminus,fplus

        allocate(pa(2),covpa(2,2),list(2))
        if(.not.allocated(xfix)) allocate(xfix(2),indfix(2))

        xmid = median(xmarr(1:itnum),itnum)

        itr = 0
        fminus = .false.
        fplus = .false.
        do i=1,itnum
            if(xmarr(i) > 0.70_wp*xmid .and. xmarr(i) < 1.3_wp*xmid) then
                itr = itr + 1
                xmarr(itr) = xmarr(i)
                ymarr(itr) = ymarr(i)
                uymarr(itr) = abs(ymarr(itr))*0.20_wp
                if(ymarr(itr) > 0.0_wp) fplus = .true.
                if(ymarr(itr) < 0.0_wp) fminus = .true.
                ! Flo: why write to unit 63??
                ! write(63,*) 'itr=',int(itr,2),' xmarr=',sngl(xmarr(itr)),' ymarr=',sngl(ymarr(itr)),' uymarr=',sngl(uymarr(itr))
            end if
        end do
        if(itr >= 3 .and. fplus .and. fminus) then
            pa = 0.0_wp
            list = 1
            nall = 2
            covpa = 0.0_wp
            mfix = 0
            xfix = 0

            call logger(30, ' before call LSQlin: ')
            call Lsqlin(funcsKB,xmarr,ymarr,uymarr,itr,nall,list,pa,covpa,chisq)

            write(log_str, '(*(g0,1x))') 'straight line: chisq=',sngl(chisq),' pa=',sngl(pa)
            call logger(30, log_str)

            x_1 = -pa(1)/pa(2)

            if(me%mode == 3)  then
                write(log_str, '(2(a,es12.5),a,i0)') 'DT  x_1=',x_1,' itr=',itr
                call logger(167, log_str)
            end if
            if(me%mode == 2)  then
                write(log_str, '(2(a,es12.5),a,i0)') 'DL  x_1=',x_1,' itr=',itr
                call logger(167, log_str)
            end if
            brentx = x_1
        end if
    end subroutine treat_mean_DT



! end module tom748

end module root748
