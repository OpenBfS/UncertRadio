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
real(rn) function brentx(x1, x2, tol, fvalue, mode)

    !Using Brent's method, Find the root of a function func known to lie between x1 and x2.
    !The root, returned as brentx, will be refined until its accuracy is tol.
    !Parameters: Maximum allowed number of iterations, and machine floating-point precision.

    !Based on John Burkardt's function ZERO (see below)

    ! the root, being searched for, yields the difference
    !     PrFunc(mode,x1w,......, kk) - fvalue
    ! to become zero.

    ! Modification: 21.10.2025
    !            the part for enlarging the brackets range, if necessary, was
    !            replaced by the subroutine check_bracket


    !  mode  :  1 : DL iteration, for Alyt;        (DL: detection limit; DT: decision threshold)
    !           2 : DL iteration, for MCsim;
    !           3 : DT iteration, for MCsim;
    !
    !------------------------------------------------------------------------------------

    use UR_types,     only: rn
    use UR_params,    only: EPS1MIN

    use UR_Gleich_globals, only: ifehl, use_bipoi
    use UR_DLIM,      only: nit_detl, modeB,fvalueB
    use UR_MCC,       only: kqtypx, arraymc, imctrue, xmit1, imcmax, xmit1min
    use UR_Linft,     only: klincall, mfix, xfix, indfix
    use Brandt,       only: mean,Lsqlin
    use ur_general_globals, only: MCsim_on,bxiter_on

    use UWB,          only: Resulta
    use num1,         only: median
    use file_io,      only: logger
    use KLF,          only: funcsKB

    implicit none

    ! tol corresponds to xacc

    integer, intent(in)    :: mode                 ! see above
    real(rn), intent(in)   :: x1,x2                ! bracketing values, safely encompassing the solution value
    real(rn), intent(in)   :: tol                  ! tolerance value;
    real(rn), intent(in)   :: fvalue               ! given value of the function, for which
    !      the root is to be calculated.

    integer, parameter :: itmax=60

    integer                 :: i,iter,ntry,jj,jjk,jjmax,munit
    integer                 :: mqt, modesv, nall, itr
    real(rn)                :: a,b,fa,fb
    real(rn)                :: x1w,x2w,fl,fh,prfunc,factor,fl_last,fh_last
    real(rn)                :: vorz_l,vorz_h,dummy,uq95mean,uq95meanq
    real(rn)                :: sa, sb
    real(rn)                :: fbmin,famin,ffmin,brentxf
    logical                 :: prout,success
    real(rn)                :: chisq,x_1, xmid
    real                    :: start,stop
    logical                 :: fminus, fplus
    real(rn),allocatable    :: arrmin(:)
    real(rn),allocatable    :: xmarr(:),ymarr(:),uymarr(:),pa(:),covpa(:,:),xmitarr(:)
    character(len=512)      :: log_str
    integer,allocatable     :: list(:)

    modeSV = mode
    allocate(xmarr(60),ymarr(60),uymarr(60),pa(2),covpa(2,2),list(2),xmitarr(60))
    if(.not.allocated(xfix)) allocate(xfix(2),indfix(2))

    modeB = modeSV
    fvalueB = fvalue
    uq95mean = 0.0_rn
    uq95meanq = 0.0_rn

    prout = .false.
    if(mode == 3 .or. mode == 2) prout = .true.

    if(mode == 1) prout = .true.
    if(MCsim_on ) prout = .true.

    ffmin = 1.E+30_rn
    bxiter_on = .true.
    brentx = 0.0_rn
    allocate(arrmin(imcmax))
    ntry = 25
    factor = 0.25_rn

    munit = 30
    ifehl = 0
    if(MCsim_on) xmit1min = xmit1
    x1w = x1
    x2w = x2

    if(mode <= 3) then
        mqt = kqtypx
        munit = 30
        if(mode == 2 .or. mode == 3) munit = 63
    end if

    write(log_str, '(a,i2,a,2es11.4,2(a,es11.4))') 'mqt=',mqt,' x1,x2=',x1,x2, &
                                                   '  fvalue=',fvalue,'  tol=',tol
    call logger(munit, log_str)
    jj = 0
    jjk = 0
    if(mode == 3) klincall = 0
    jjmax = 30
    call cpu_time(start)

    fL_last = 1.e+12_rn
    fh_last = 1.e+12_rn
    vorz_L = -1.0_rn
    vorz_h = 1.0_rn

    !---------------------------------------------------------------------------------
    ! part for enlarging the bracketing interval (x1,x2) to (x1w,x2w), if necessary
    ! check_bracket: introduced Octobre 2025 (GK)
    call check_bracket(mqt,munit,mode,x1w,x2w,fL,fh,fvalue,success)
    if(ifehl == 1) goto 900
    jjk = jjk + 2

    a = x1w
    b = x2w
    fbmin = 1.e+30_rn
    famin = 1.e+30_rn

    call rzero( a, b, EPS1MIN, tol, PrFunc, mode,fvalue, brentxF, itmax, iter, &
               mqt,fbmin,famin,arrmin,munit,prout,sa,sb,fa,fb, jjk,xmarr,ymarr)
    brentx = brentxF
    if(iter >= itmax) ifehl = 1
    if(iter == 8 .and. MCsim_on .and. mqt == 2) then
        call logger(munit, '+++++++++++++++ brentx exceeds maximum iterations')
    end if

    if(iter >= 4) then

        xmid = median(xmarr(1:iter),iter)
        itr = 0
        fminus = .false.
        fplus = .false.
        do i=1,iter
            if(xmarr(i) > 0.70_rn*xmid .and. xmarr(i) < 1.3_rn*xmid) then
                itr = itr + 1
                xmarr(itr) = xmarr(i)
                ymarr(itr) = ymarr(i)
                uymarr(itr) = abs(ymarr(itr))*0.20_rn
                if(ymarr(itr) > 0.0_rn) fplus = .true.
                if(ymarr(itr) < 0.0_rn) fminus = .true.
                ! Flo: why write to unit 63??
                ! write(63,*) 'itr=',int(itr,2),' xmarr=',sngl(xmarr(itr)),' ymarr=',sngl(ymarr(itr)),' uymarr=',sngl(uymarr(itr))
            end if
        end do
        if(itr >= 3 .and. fplus .and. fminus) then
            pa = 0._rn
            list = 1
            nall = 2
            covpa = 0._rn
            mfix = 0
            xfix = 0

            call logger(30, ' before call LSQlin: ')
            call Lsqlin(funcsKB,xmarr,ymarr,uymarr,itr,nall,list,pa,covpa,chisq)

            write(log_str, '(*(g0))') 'straight line: chisq=',sngl(chisq),' pa=',sngl(pa)
            call logger(30, log_str)

            x_1 = -pa(1)/pa(2)

            if(mode == 3)  then
                write(log_str, '(2(a,es12.5),a,i0)') 'DT  brentx=',brentx,' x_1=',x_1,' itr=',itr
                call logger(167, log_str)
            end if
            if(mode == 2)  then
                write(log_str, '(2(a,es12.5),a,i0)') 'DL  brentx=',brentx,' x_1=',x_1,' itr=',itr
                call logger(167, log_str)
            end if
            brentx = x_1
        end if
    end if

    if(mode <= 2) nit_detl = iter + jj

    call cpu_time(stop)
    if(prout .and. mode /= 9) then

        write(log_str,'(a,es13.6,4(a,es13.6))') 'brentx final: ',brentx, &
                                                ' a=',sngl(Sa),' b=',sngl(Sb), &
                                                ' fa=',sngl(fa),' fb=',sngl(fb)
        call logger(munit, log_str)
    end if
    if(MCsim_on) then
        arraymc(1:imctrue,mqt) = arrmin(1:imctrue)
        xmit1 = xmit1min
    end if

    if(prout .and. (mode == 2 .or. mode == 3)) then
        write(munit,'(*(g0))') 'MCsing runtime : ', &
                                sngl( (stop-start)/real(jjk,rn))
        call logger(munit, log_str)
    end if

    if(MCsim_on) then
        if(mode == 2) then
            write(log_str, '(*(g0))') 'brentx: DL from parameter=', sngl(brentx)
            call logger(munit, log_str)

            brentx = mean(arraymc(1:imctrue,mqt))
            write(log_str, '(*(g0))') ' DL(brentx)=mean(array)=',sngl(brentx)
            call logger(munit, log_str)

        elseif(mode == 3) then
            brentx = mean(arraymc(1:imctrue,mqt))
            write(log_str, '(*(g0))') ' DT(brentx):  mean(array) = ',sngl(brentx)
            call logger(munit, log_str)
        end if
    end if

    if(.false. .and. MCsim_on .and. mode == 2 .and. use_bipoi) then  ! .and. test_mg) then
        do i=1,11
            x1w = a + real(i-1,rn)*(b-a)/real(10,rn)
            dummy = PrFunc(mode,x1w)
            write(log_str, '(*(g0))') 'Scan: x1w=',sngl(x1w),' f(RD)=',sngl(dummy)
            call logger(munit, log_str)
        end do
    end if

900 continue

    bxiter_on = .false.

end function brentx

!########################################################################

! function zerof ( a, b, machep, t, f,mode,fvalue,itmax, iter )
! rearranged to:
subroutine rzero(a, b, machep, t, ff2,mode,fvalue, zerof, itmax, iter, &
                 mqt,fbmin, famin, arrmin, munit, prout, sa, sb, fa, fb, jjk, xmarr, ymarr )

    ! routine is called by brentx only.

    use UR_types,     only: rn
    use UR_params,    only: ZERO, EPS1MIN

    use UR_MCC,       only: arraymc,imctrue,xmit1,xmit1min
    use ur_general_globals, only: MCsim_on

    use file_io,      only: logger

    !*****************************************************************************80
    !
    !! ZERO seeks the root of a function F(X) in an interval [A,B].
    !
    !  Discussion:
    !
    !    The interval [A,B] must be a change of sign interval for F.
    !    That is, F(A) and F(B) must be of opposite signs. Then
    !    assuming that F is continuous implies the existence of at least
    !    one value C between A and B for which F(C) = 0.
    !
    !    The location of the zero is determined to within an accuracy
    !    of 6 * MACHEPS * abs ( C ) + 2 * T.
    !
    !    Thanks to Thomas Secretin for pointing out a transcription error in the
    !    setting of the value of P, 11 February 2013.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    11 February 2013
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Richard Brent.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Richard Brent,
    !    Algorithms for Minimization Without Derivatives,
    !    Dover, 2002,
    !    ISBN: 0-486-41998-3,
    !    LC: QA402.5.B74.
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) A, B, the endpoints of the change of
    !    sign interval.
    !
    !    Input, real ( kind = 8 ) MACHEP, an estimate for the relative machine
    !    precision.
    !
    !    Input, real ( kind = 8 ) T, a positive error tolerance.
    !
    !    Input, external real ( kind = 8 ) F, the name of a user-supplied
    !    function, of the form "FUNCTION F ( X )", which evaluates the
    !    function whose zero is being sought.
    !
    !    Output, real ( kind = 8 ) ZERO, the estimated value of a zero of
    !    the function F.
    !
        implicit none

        real ( kind = rn ) a
        real ( kind = rn ) b
        real ( kind = rn ) c
        real ( kind = rn ) d
        real ( kind = rn ) e
        real ( kind = rn ) ff2     ! f
        real ( kind = rn ) fa
        real ( kind = rn ) fb
        real ( kind = rn ) fc
        real ( kind = rn ) m
        real ( kind = rn ) machep
        real ( kind = rn ) p
        real ( kind = rn ) q
        real ( kind = rn ) r
        real ( kind = rn ) s
        real ( kind = rn ) sa
        real ( kind = rn ) sb
        real ( kind = rn ) t
        real ( kind = rn ) tol
        real ( kind = rn ),intent(out) :: zerof
        integer,intent(in)           :: munit
        logical,intent(in)           :: prout
        integer,intent(inout)        :: jjk
        real(rn),intent(out)         :: xmarr(*),ymarr(*)

        integer         :: mode,itmax,iter, mqt,itr
        real(rn)        :: fvalue,a_anf,b_anf,fbmin,famin,amin,bmin,arrmin(*)
        real(rn)        :: aminabs,bminabs,zbmin,zbminabs
        character(128)  :: log_str

        !
        !  Make local copies of A and B.
        !
        amin = ZERO    ! 2025.01.24 GK
        bmin = ZERO    !

        a_anf = a
        b_anf = b

        sa = a
        sb = b
        fa = ff2(mode, sa ) - fvalue
        fb = ff2(mode, sb ) - fvalue

        c = sa
        fc = fa
        e = sb - sa
        d = e

        iter = 0
        itr = 0
        do
            iter = iter + 1
            if ( abs ( fc ) < abs ( fb ) ) then

                sa = sb
                sb = c
                c = sa
                fa = fb
                fb = fc
                fc = fa

            end if

            tol = 2.0_rn * machep * abs ( sb ) + t
            m = 0.5_rn * ( c - sb )

            if ( abs ( m ) <= tol .or. abs(fb) < EPS1MIN ) then
                exit
            end if

            !+++++++++++++++++++++++++++++++++++++
            if(.not.Isnan(sb) .and. .not.IsNan(fb)) then
                itr = itr + 1
                xmarr(itr) = sb
                ymarr(itr) = fb
            end if

            if ( abs ( e ) < tol .or. abs ( fa ) <= abs ( fb ) ) then

                e = m
                d = e

            else

                s = fb / fa

                if ( abs(sa - c) < EPS1MIN ) then

                    p = 2.0_rn * m * s
                    q = 1.0_rn - s

                else

                    q = fa / fc
                    r = fb / fc
                    p = s * ( 2.0_rn * m * q * ( q - r ) - ( sb - sa ) * ( r - 1.0_rn ) )
                    q = ( q - 1.0_rn ) * ( r - 1.0_rn ) * ( s - 1.0_rn )

                end if

                if ( 0._rn < p ) then
                    q = - q
                else
                    p = - p
                end if

                s = e
                e = d

                if ( 2.0_rn * p < 3.0_rn * m * q - abs ( tol * q ) .and. &
                    p < abs ( 0.5_rn * s * q ) ) then
                    d = p / q
                else
                    e = m
                    d = e
                end if

            end if

            sa = sb
            fa = fb

            if ( tol < abs ( d ) ) then
                sb = sb + d
            else if ( 0._rn < m ) then
                sb = sb + tol
            else
                sb = sb - tol
            end if
            fb = ff2(mode, sb ) - fvalue

            if(MCsim_on) then                ! addition by GK:
                jjk = jjk + 1       ! number of MCsing runs
                if(mqt == 2) then
                    if(iter == 1 .or. (abs(fb) < abs(fa) .and. abs(fb) < fbmin)) then
                        arrmin(1:imctrue) = arraymc(1:imctrue,mqt)
                        xmit1min = xmit1
                        fbmin = abs(fb)
                        famin = abs(fa)
                        amin = sa
                        bmin = sb
                        zbmin = sb
                        aminabs = abs(sa)
                        bminabs = abs(sb)
                        zbminabs = abs(sb)
                    end if
                end if
                if(mqt /= 2) then
                    arrmin(1:imctrue) = arraymc(1:imctrue,mqt)
                    xmit1min = xmit1
                    if(abs(fb) < abs(fa) .and. abs(fb) < fbmin) then
                        fbmin = abs(fb)
                        famin = abs(fa)
                        amin = sa
                        bmin = sb
                        zbmin = sb
                        aminabs = abs(sa)
                        bminabs = abs(sb)
                        zbminabs = abs(sb)
                    end if
                end if
            end if

            if ( ( 0._rn < fb .and. 0._rn < fc ) .or. &
                ( fb <= 0._rn .and. fc <= 0._rn ) ) then
                c = sa
                fc = fa
                e = sb - sa
                d = e
            end if
            if(prout) then

                write(log_str, '(a,i3,5(a,es12.5))') 'iter=',iter,' sa=',sa,' sb=',sb, &
                                                    ' fa=',fa,' fb=',fb,' fb-fa=',fb-fa
                call logger(munit, log_str)
            end if
            !++++++++++++++++++++++++++++++++++++++++++++++++
            if(MCsim_on .and. mqt == 2 .and. iter == 8) then
                sa = amin
                sb = bmin
                fa = famin
                fb = fbmin
                exit
            end if
            !++++++++++++++++++++++++++++++++++++++++++++++++

            if(iter > itmax) exit
        end do
        iter = itr


        zerof = sb

        return
    ! end function zerof
    end subroutine rzero

!#######################################################################################

    subroutine check_bracket(mqt,munit,mode,a,b,fa,fb,fvalue,success)
    use UR_types,     only: rn
    use UR_Gleich_globals,    only: ifehl
    use file_io,              only: logger
    implicit none

    integer,intent(in)      :: mqt             ! =2: for Decison threshold;  =3: for detection limit
    integer,intent(in)      :: munit           ! unit number for file output
    integer,intent(in)      :: mode            ! mode in Prfunc, for MC:    =3: for decision threshold;  =2 for detection limit
    real(rn),intent(inout)  :: a,b             ! lower and upper bracketing values
    real(rn),intent(inout)  :: fa,fb           ! function values of a, b
    real(rn),intent(in)     :: fvalue          ! target function value; it is subtracted from the Prfunc value in
                                               ! order to make the target difference, for which the root is to be found,
                                               ! equal to zero
    logical,intent(out)     :: success         ! =T if fa*fb < zero, =F otherwise

    integer              :: kj,iw
    real(rn)             :: fkt,Prfunc,b0,fb0
    character(len=512)   :: log_str

    fa = Prfunc(mode,a) - fvalue
    fb = Prfunc(mode,b) - fvalue

     if(fa*fb > 0._rn) then
         write(log_str,*) 'bracketing problem: a,b=',sngl(a),sngl(b),' fa,fb=',sngl(fa),sngl(fb), &
                     ' fa*fb=',sngl(fa*fb)
         call logger(munit, log_str)

         if(fa < 0._rn .and. fb < 0._rn) then
             fkt = -1._rn
         else
             fkt = 1._rn
         end if
         if(mqt == 3 .and. (mode == 1 .or. mode == 2) ) fkt = -fkt
         if(abs(fa) < abs(fb)) then
             do kj=1,20
                 a = a * 1.3_rn**fkt
                 fa = Prfunc(mode,a) - fvalue
                   write(log_str,*) 'kj=',int(kj,2),' a=',sngl(a),' fa=',sngl(fa),' mode=',int(mode,2)
                   call logger(munit, log_str)
                 if(fa*fb < 0._rn) exit
             end do
         else
             fb0 = fb
             b0 = b
             do iw=1,2
               b = b0
               fb = fb0
               do kj=1,20
                   b = b * 1.3_rn**fkt
                   fb = Prfunc(mode,b) - fvalue
                     write(log_str,*) 'kj=',int(kj,2),' b=',sngl(b),' fb=',sngl(fb),' mode=',int(mode,2)
                     call logger(munit, log_str)
                   if(fa*fb < 0._rn) exit
               end do
               if(iw == 1 .and. fa*fb < 0._rn) exit
               if(iw == 1 .and. fa*fb > 0._rn) then
                 fkt = -fkt
               end if
             end do
         end if
    end if
    success = .true.

    if(fa*fb > 0._rn) then
        success = .false.
        write(log_str,*) 'brentx: bracketing not successful!  Stopped'
        call logger(munit, log_str)
        write(log_str,*) 'bracketing problem: a,b=',sngl(a),sngl(b),' fa,fb=',sngl(fa),sngl(fb)
        call logger(munit, log_str)
        ifehl = 1
        return
    end if

    end subroutine check_bracket

!#######################################################################################
