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
Real(rn) FUNCTION brentx(x1,x2,tol,fvalue,mode)

    !Using Brent's method, Find the root of a function func known to lie between x1 and x2.
    !The root, returned as brentx, will be refined until its accuracy is tol.
    !Parameters: Maximum allowed number of iterations, and machine floating-point precision.

    !Based on John Burkardt's function ZERO (see below)

    ! the root, being searched for, yields the difference
    !     PrFunc(mode,x1w,......, kk) - fvalue
    ! to become zero.


    !  mode  :  1 : DL iteration, for Alyt;        (DL: detection limit; DT: decision threshold)
    !           2 : DL iteration, for MCsim;
    !           3 : DT iteration, for MCsim;
    !
    !------------------------------------------------------------------------------------

    use UR_params,    only: rn,eps1min,zero,one,two,three
    use Rout,         only: WDPutEntryInt,pending_events
    use UR_Gleich,    only: ifehl,use_bipoi
    use UR_DLIM,      only: nit_detl, iteration_on, modeB,fvalueB
    use UR_MCC,       only: kqtypx,arraymc,imctrue,xmit1,imcmax,xsdv,estUQ,DT_anf,xmit1min
    use UR_Linft,     only: klincall,mfix,xfix,indfix
    use Brandt,       only: mean,Lsqlin
    use UR_VARIABLES, only: MCsim_on,bxiter_on
    use Pdfs,         only: BinPoi_2_PDF

    use Top,          only: WrStatusbar
    use UWB,          only: Resulta,median
    use LF1,          only: Linf
    use KLF,          only: CalibInter,funcsKB

    implicit none

    ! tol corresponds to xacc

    integer(4),intent(in)  :: mode                 ! see above
    REAL(rn), intent(in)   :: x1,x2                ! bracketing values, safely encompassing the solution value
    REAL(rn), intent(in)   :: tol                  ! tolerance value;
    REAL(rn), intent(in)   :: fvalue               ! given value of the function, for which
    !      the root is to be calculated.

    INTEGER(4), parameter   :: ITMAX=60
    real(rn), parameter     :: EPS = 3.E-10_rn/5._rn   /2._rn  ! 3.d-8
    INTEGER(4)      :: i,iter,ntry,jj,jjk,jjmax,m,munit
    integer(4)      :: nuq,  mqt,modeSV,icase,nall,itr
    REAL(rn)        :: a,b,fa,fb
    real(rn)        :: x1w,x2w,fL,fh,prfunc,factor,fL_last,fh_last
    real(rn)        :: vorz_L,vorz_h,dummy,uq95mean,uq95meanq,DTy,sdDTy,minfm,minxm
    real(rn)        :: abst,sa,sb
    real(rn)        :: fbmin,famin,ffmin,brentxF
    logical         :: prout,success
    real(rn)        :: fmove,ffix,xmove,xxfix,delta,chisq,x_1,x_2,xmid
    real            :: start,stop
    logical         :: start_left,fminus,fplus
    real(rn),allocatable    :: arrmin(:)
    real(rn),allocatable    :: xmarr(:),ymarr(:),uymarr(:),pa(:),covpa(:,:),xmitarr(:)
    integer(4),allocatable  :: list(:)
    modeSV = mode

    allocate(xmarr(60),ymarr(60),uymarr(60),pa(2),covpa(2,2),list(2),xmitarr(60))
    if(.not.allocated(xfix)) allocate(xfix(2),indfix(2))

    modeB = modeSV
    fvalueB = fvalue

    prout = .false.
    prout = .true.
    if(mode == 3 .or. mode == 2) prout = .true.

    if(mode == 1) prout = .true.
    if(MCsim_on ) prout = .true.

    ffmin = 1.E+30_rn

    bxiter_on = .true.

    brentx = zero

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
    write(munit,'(a,i2,a,2es11.4,2(a,es11.4))') 'mqt=',mqt,' x1,x2=',x1,x2, &
        '  fvalue=',fvalue,'  tol=',tol
    jj = 0
    jjk = 0
    if(mode == 3) klincall = 0
    jjmax = 30
    call cpu_time(start)

    fL_last = 1.e+12_rn
    fh_last = 1.e+12_rn
    vorz_L = -one
    vorz_h = +one

!---------------------------------------------------------------------------------
! part for enlarging the bracketing interval (x1,x2) to (x1w,x2w), if necessary

23  continue

    if(.not.use_bipoi) then
        fL = PrFunc(mode,x1w) - fvalue
    elseif(use_bipoi) then
        fL = PrFunc(mode,x1w) - fvalue
    end if
    fL_last = abs(fL)
24  continue
    if(.not.use_bipoi) then
        fh = PrFunc(mode,x2w) - fvalue
    elseif(use_bipoi) then
        fh = PrFunc(mode,x2w) - fvalue
    end if
    fh_last = abs(fh)
    jjk = jjk + 2      ! number of function value calculations
    if(ifehl == 1) goto 900

    m = 0
    if(prout) write(munit,'(a,i3,5(a,es11.4),a,i2)') 'brentx: m=',m, ' x1w=',x1w,' x2w=',x2w, &
        ' fL*fh=',fL*fh,' ',fL,' ',fh,' mode=',mode
    success = .true.

    if(mqt >= 2 .and. fL*fh >= 0._rn) then
        ! try to enlarge the bracketing interval:
        success = .false.
        abst = abs(x2w - x1w)

        if(fL < Zero) then
            if(fL < fh) then
                start_left = .false.
                xmove = x2w
                xxfix  = x1w
                ffix = fL
                delta = (factor*abst)
                icase = 1
            end if
            if(fL > fh) then
                start_left = .true.
                xmove = x1w
                xxfix = x2w
                ffix = fh
                delta = -(factor*abst)
                icase = 2
            end if
        else
            if(fL < fh) then
                start_left = .true.
                xmove = x1w
                xxfix  = x2w
                ffix = fh
                delta = -(factor*abst)
                icase = 3
            end if
            if(fL > fh) then
                start_left = .false.
                xmove = x2w
                xxfix = x1w
                ffix = fL
                delta = (factor*abst)
                icase = 4
            end if
        end if

        do
            if(m > 4) then
                if(mode == 3 .and. xsdv > 2.5*abs(xmit1) .and. estUQ < DT_anf/two ) then
                    ! iterate DT in case of bad statistics:
                    uq95mean = uq95mean + estUQ
                    uq95meanq = uq95meanq + estUQ**two
                    nuq = nuq + 1
                    write(munit,*) '   +++++++++++ nuq=',int(nuq,2),' estUQ=',sngl(estUQ)
                    if(nuq > 4) then
                        DTy = uq95mean/real(nuq,rn)
                        sdDTy = sqrt(uq95meanq/real(nuq,rn) - DTy**two)
                        if(sdDTy/DTy < 0.002_rn) then
                            arrmin(1:imctrue) = arraymc(1:imctrue,mqt)
                            xmit1min = xmit1       ! xmit1 comes from calling PrFunc
                            brentx = zero
                            goto 200
                        end if
                    end if
                end if
            end if

            if(m > 50) exit     ! goto 120
            xmove = xmove + delta
            fmove = PrFunc(mode,xmove) - fvalue
            m = m + 1
            xmarr(m) = xmove
            ymarr(m) = fmove
            uymarr(m) = 1._rn
            xmitarr(m) = xmit1   ! xmit1 comes from calling PrFunc

            if(m >= 18 .and. mode == 3) then
                minfm = 1.e+30_rn
                do i=1,m
                    if(xmitarr(i) < minfm) then
                        minfm = xmitarr(i)
                        minxm = xmarr(i)
                    end if
                end do
                fmove = PrFunc(mode,0._rn) - fvalue
                arrmin(1:imctrue) = arraymc(1:imctrue,2)
                xmit1min = xmit1
                brentx = zero
                goto 200
            end if

            if(ifehl == 1) goto 900   ! return
            write(munit,'(a,i3,5(a,es11.4),a,i2,a,2i3)') 'brentx 95: m=',m,' xmove=',xmove,' xxfix=',xxfix, &
                ' fmove*ffix=',fmove*ffix,' ',fmove,' ',ffix,' mode=',mode  !,' vorz_L,h=',int(vorz_L,2),int(vorz_h,2)
            if(fmove*ffix > zero) cycle     ! goto 95

            if(fmove*ffix < zero) then
                if(icase == 1 .or. icase == 4) then
                    x2w = xmove
                    fh = fmove
                elseif(icase == 2 .or. icase == 3) then
                    x1w = xmove
                    fL = fmove
                end if
                success = .true.
                !goto 120
                exit
            end if

        end do
!120  continue
        if(prout) write(munit,'(a,i3,5(a,es11.4),a,i2,a,2i3)') 'brentx: m=',m,' x1w=',x1w,' x2w=',x2w, &
            ' fL*fh=',fL*fh,' ',fL,' ',fh,' mode=',mode,' vorz_L,h=',int(vorz_L,2),int(vorz_h,2)
        if(.not.success) then
            write(munit,'(a,i0)') 'Brentx: no success in setting initial brackets, m=',m
            ifehl = 1
            goto 900       !return
        end if
    end if

130 continue

    a = x1w
    b = x2w
    fbmin = 1.e+30_rn
    famin = 1.e+30_rn
! call rzero( a, b, epsilon(1._rn), tol, PrFunc, mode,fvalue, brentx, itmax, iter, &
    call rzero( a, b, epsilon(1._rn), tol, PrFunc, mode,fvalue, brentxF, itmax, iter, &
        mqt,fbmin,famin,arrmin,munit,prout,sa,sb,fa,fb, jjk,xmarr,ymarr)
    brentx = brentxF
    if(iter >= itmax) ifehl = 1
    if(iter == 8 .and. MCsim_on .and. mqt == 2) then
        write(munit,*) '+++++++++++++++ brentx exceeds maximum iterations'
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
                if(ymarr(itr) > zero) fplus = .true.
                if(ymarr(itr) < zero) fminus = .true.
                ! Flo: why write to unit 63??
                ! write(63,*) 'itr=',int(itr,2),' xmarr=',sngl(xmarr(itr)),' ymarr=',sngl(ymarr(itr))
            end if
        end do
        if(itr >= 3 .and. fplus .and. fminus) then
            pa = 0._rn
            list = 1
            nall = 2
            covpa = 0._rn
            mfix = 0
            xfix = 0
            write(30,*) ' before call LSQlin: '
            call Lsqlin(funcsKB,xmarr,ymarr,uymarr,itr,nall,list,pa,covpa,chisq)
            ! Flo: why write to unit 63??
            ! write(63,*) 'straight line: chisq=',sngl(chisq),' pa=',sngl(pa)

            !x_1 = -pa(2)/two/pa(3) + sqrt( (pa(2)/two/pa(3))**two - pa(1)/pa(3) )
            !x_2 = -pa(2)/two/pa(3) - sqrt( (pa(2)/two/pa(3))**two - pa(1)/pa(3) )
            x_1 = -pa(1)/pa(2)
            ! Flo: why write to unit 63??
            !    write(63,*) 'x_1=',sngl(x_1),' brentx direkt: ',sngl(brentx),' x_1/brentx=',sngl(x_1/brentx)
            if(mode == 3) write(167,'(2(a,es12.5),a,i0)') 'DT  brentx=',brentx,' x_1=',x_1,' itr=',itr
            if(mode == 2) write(167,'(2(a,es12.5),a,i0)') 'DL  brentx=',brentx,' x_1=',x_1,' itr=',itr
            brentx = x_1
        end if
    end if

200 continue
    if(mode <= 2) nit_detl = iter + jj

    call cpu_time(stop)
    if(prout .and. mode /= 9) write(munit,'(a,es13.6,4(a,es13.6))') 'brentx final: ',brentx, &
        ' a=',sngl(Sa),' b=',sngl(Sb),' fa=',sngl(fa),' fb=',sngl(fb),''

    if(MCsim_on) then
        arraymc(1:imctrue,mqt) = arrmin(1:imctrue)
        xmit1 = xmit1min
    end if

    if(prout .and. (mode == 2 .or. mode == 3)) write(munit,*) 'MCsing runtime : ', &
        sngl( (stop-start)/real(jjk,rn))
    if(MCsim_on) then
        if(mode == 2) then
            write(munit,*) 'brentx: DL from parameter=',sngl(brentx)
            brentx = mean(arraymc(1:imctrue,mqt))
            write(munit,*) ' DL(brentx)=mean(array)=',sngl(brentx)
        elseif(mode == 3) then
            brentx = mean(arraymc(1:imctrue,mqt))
            write(munit,*) ' DT(brentx):  mean(array) = ',sngl(brentx)
        end if
    end if

    if(.false. .and. MCsim_on .and. mode == 2 .and. use_bipoi) then  ! .and. test_mg) then
        do i=1,11
            x1w = a + real(i-1,rn)*(b-a)/real(10,rn)
            dummy = PrFunc(mode,x1w)
            write(munit,*) 'Scan: x1w=',sngl(x1w),' f(RD)=',sngl(dummy)
        end do
    end if

900 continue

    bxiter_on = .false.

END Function brentx

!########################################################################

! function zerof ( a, b, machep, t, f,mode,fvalue,itmax, iter )
! rearranged to:
subroutine rzero ( a, b, machep, t, ff2,mode,fvalue, zerof, itmax, iter, &
    mqt,fbmin, famin,arrmin,munit,prout,sa,sb,fa,fb,jjk,xmarr,ymarr )

    ! this routine is called by brentx only.

    use UR_params,    only: rn,eps1min,one,two,three
    use UR_MCC,       only: arraymc,imctrue,xmit1,xmit1min
    use UR_variables, only: MCsim_on
    use UWB,          only: ResultA
    use CHF,          only: IsNan

!*****************************************************************************80
!
!! ZERO seeks the root of a function F(X) in an interval [A,B].
!
!  Discussion:
!
!    The interval [A,B] must be a change of sign interval for F.
!    That is, F(A) and F(B) must be of opposite signs.  Then
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

    external ff2

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
    integer(kind=4),intent(in)   :: munit
    logical,intent(in)      :: prout
    integer(4),intent(inout)   :: jjk
    real(rn),intent(out)     :: xmarr(*),ymarr(*)

    integer(4)      :: mode,itmax,iter, mqt,itr
    real(rn)        :: fvalue,a_anf,b_anf,fbmin,famin,amin,bmin,arrmin(*)
    real(rn)        :: aminabs,bminabs,zbmin,zbminabs

!
!  Make local copies of A and B.
!
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

        tol = two * machep * abs ( sb ) + t
        m = 0.5_rn * ( c - sb )

        if ( abs ( m ) <= tol .or. fb == 0._rn ) then
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

            if ( sa == c ) then

                p = two * m * s
                q = one - s

            else

                q = fa / fc
                r = fb / fc
                p = s * ( two * m * q * ( q - r ) - ( sb - sa ) * ( r - one ) )
                q = ( q - one ) * ( r - one ) * ( s - one )

            end if

            if ( 0._rn < p ) then
                q = - q
            else
                p = - p
            end if

            s = e
            e = d

            if ( two * p < three * m * q - abs ( tol * q ) .and. &
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
        if(prout) write(munit,'(a,i3,5(a,es12.5))') 'iter=',iter,' sa=',sa,' sb=',sb, &
            ' fa=',fa,' fb=',fb,' fb-fa=',fb-fa   ! ,' fvalue=',sngl(fvalue), &
        ! ' a_anf=',sngl(a_anf),' b_anf=',sngl(b_anf)

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
    ! write(30,*) 'zerof=',sngl(zerof)

    return
! end function zerof
end subroutine rzero


!#######################################################################################

subroutine rzeroRn ( a, b, machep, t, ff2,fvalue, zerof, itmax, iter, &
    munit,prout,sa,sb,fa,fb )
    ! rzeroRn correponds to rzero, but used for calculating the net count rate value.
    ! It is called directly by Rnetval.

    use UR_params,    only: rn,eps1min,one,two,three
    use UWB,          only: ResultA

!*****************************************************************************80
!
!! ZERO seeks the root of a function F(X) in an interval [A,B].
!
!  Discussion:
!
!    The interval [A,B] must be a change of sign interval for F.
!    That is, F(A) and F(B) must be of opposite signs.  Then
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

    external ff2

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

    !--------------------

    integer(4),intent(in)  :: munit
    logical,intent(in)     :: prout
    integer(4),intent(in)  :: itmax
    integer(4),intent(out) :: iter

    real(rn),intent(in)    :: fvalue

    real(rn)        :: a_anf,b_anf
!
!  Make local copies of A and B.
!
    a_anf = a
    b_anf = b
    sa = a
    sb = b
    fa = ff2(sa ) - fvalue
    fb = ff2(sb ) - fvalue
    c = sa
    fc = fa
    e = sb - sa
    d = e

    iter = 0
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

        tol = two * machep * abs ( sb ) + t
        m = 0.5_rn * ( c - sb )

        if ( abs ( m ) <= tol .or. fb == 0._rn ) then
            exit
        end if

        if ( abs ( e ) < tol .or. abs ( fa ) <= abs ( fb ) ) then

            e = m
            d = e

        else

            s = fb / fa

            if ( sa == c ) then

                p = two * m * s
                q = one - s

            else

                q = fa / fc
                r = fb / fc
                p = s * ( two * m * q * ( q - r ) - ( sb - sa ) * ( r - one ) )
                q = ( q - one ) * ( r - one ) * ( s - one )

            end if

            if ( 0._rn < p ) then
                q = - q
            else
                p = - p
            end if

            s = e
            e = d

            if ( two * p < three * m * q - abs ( tol * q ) .and. &
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
        fb = ff2(sb ) - fvalue    !!

        if ( ( 0._rn < fb .and. 0._rn < fc ) .or. &
            ( fb <= 0._rn .and. fc <= 0._rn ) ) then
            c = sa
            fc = fa
            e = sb - sa
            d = e
        end if
        if(prout) write(munit,*) ' sa=',sngl(sa),' sb=',sngl(sb),' fa=',sngl(fa), &
            '  fb=',sngl(fb),' fvalue=',sngl(fvalue)

        if(iter > itmax) exit
    end do

    zerof = sb
    ! write(30,*) 'rzeroRn: zerof=',sngl(zerof),' iter=',iter

    return
end subroutine rzeroRn

!#######################################################################################

real(rn) function ffuncRnet(x)

    ! This function is used by Rnetval, passed as an argument to the subroutine rzerosRn.
    !   Copyright (C) 2023  Günter Kanisch

    use UR_params,  only: rn,zero
    use UR_DLIM,    only: kluB
    use UR_Gleich,  only: Messwert,kEGr
    use UWB,        only: ResultA

    implicit none

    real(rn),intent(in)   :: x

    Messwert(kluB) = x
    ffuncRnet = Resulta(kEGr)

    !write(30,*) 'ffuncRnet: x=',sngl(x),' f=',sngl(ffuncRnet),'  fvalue=',sngl(fvalueB), &
    !                      ' mode=',mode,' kluB=',int(kluB,2),' Rnetmodi=',Rnetmodi

end function ffuncRnet
