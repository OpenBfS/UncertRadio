SUBROUTINE auxdrg(x,eta,mm,n,nr,nred,list,e,ok,LsqGfn)

! Computes the derivative f'(x) of f(x) at x = X. Based on
! H. Rutishauser, Ausdehnung des Rombergschen Prinzips
! (Extension of Romberg's Principle), Numer. Math. 5 (1963) 48-54
! Source: textbook S. Brandt, Data Analysis

! Calculates the matrix e of derivatives required for the general
! case least squares (LSQGEN = weighted total least squares)
! taken from the Datan-Library (Fortran) (Brandt), modified by GK

USE UR_Derivats
USE UR_Linft,         ONLY: mfit,mxind
use UR_params,        only: rn,zero,one,two,three

implicit none

integer(4), INTENT(IN)          :: n               ! 2 x number of measurement points
integer(4), INTENT(IN)          :: nr              ! number of parameters
real(rn), INTENT(INOUT)         :: x(nr)           ! vector fit parameters
real(rn), INTENT(INOUT)         :: eta(n)          ! vector
integer(4), INTENT(IN)          :: mm              ! number of measurement points
integer(4), INTENT(IN)          :: nred            ! number of parameters to be fitted
integer(4), INTENT(IN)          :: list(nr)        !  list(k): =1 fit parameter k; =0 do not fit parameter k
real(rn), INTENT(OUT)           :: e(mm,nred+n)    ! matrix of derivatives
LOGICAL, INTENT(OUT)            :: ok

real(rn), PARAMETER :: eps   = 5.E-8_rn       ! original
real(rn), PARAMETER :: epsi  = 1.E-10_rn      ! original
real(rn), PARAMETER :: delta = 10._rn
real(rn), PARAMETER :: s = 1.E-1_rn

!   For quite small parameter values one should better not
!   decrese the values of eps and epsi!
!   only the starting value for delta (or del) can be made smaller.

real(rn)      :: dx(0:9),w(0:9,3),t(0:9,0:9),a(0:9)
LOGICAL       :: lev(0:9),lmt
LOGICAL       :: lx
real(rn)      :: LsqGfn,dpafact
real(rn)       :: wx(0:9,3)

EXTERNAL  LsqGfn

integer(4)    :: k,l,m,im,i1,i2,iy,il,i,kk
real(rn)      :: del,xsav,h,fplus,fminus
real(rn)      :: ableit  , tanlyt
logical       :: compare_AnNu


dx = [ 0.0256_rn, 0.0192_rn, 0.0128_rn, 0.0096_rn, 0.0064_rn,  &
       0.0048_rn, 0.0032_rn, 0.0024_rn, 0.0016_rn, 0.0012_rn ]


!DATA (lev(k),k=0,8,2) /5*.true./
!DATA (lev(k),k=1,9,2) /5*.false./
 lev(0:9) = [ .true., .false., .true., .false., .true., .false., .true., .false., .true., .false. ]

! DATA wx(1,1) /1.3333333333333333_rn/
! DATA wx(3,1) /1.0666666666666667_rn/
! DATA wx(5,1) /1.0158730158730159_rn/
! DATA wx(7,1) /1.0039215686274510_rn/
! DATA wx(2,1) /3.3333333333333333E-01_rn/
! DATA wx(4,1) /6.6666666666666667E-02_rn/
! DATA wx(6,1) /1.5873015873015873E-02_rn/
! DATA wx(8,1) /3.9215686274509804E-03_rn/
! DATA wx(0,2) /2.2857142857142857_rn/
! DATA wx(2,2) /1.1636363636363636_rn/
! DATA wx(4,2) /1.0364372469635628_rn/
! DATA wx(6,2) /1.0088669950738916_rn/
! DATA wx(8,2) /1.0022021042329337_rn/
! DATA wx(1,2) /1.2857142857142857_rn/
! DATA wx(3,2) /1.6363636363636364E-01_rn/
! DATA wx(5,2) /3.6437246963562753E-02_rn/
! DATA wx(7,2) /8.8669950738916256E-03_rn/
! DATA wx(9,2) /2.2021042329336922E-03_rn/
! DATA wx(0,3) /1.8000000000000000_rn/
! DATA wx(2,3) /1.1250000000000000_rn/
! DATA wx(4,3) /1.0285714285714286_rn/
! DATA wx(6,3) /1.0069930069930070_rn/
! DATA wx(8,3) /1.0017391304347826_rn/
! DATA wx(1,3) /8.0000000000000000E-01_rn/
! DATA wx(3,3) /1.2500000000000000E-01_rn/
! DATA wx(5,3) /2.8571428571428571E-02_rn/
! DATA wx(7,3) /6.9930069930069930E-03_rn/
! DATA wx(9,3) /1.7391304347826087E-03_rn/

w(1,1) = one + one/three
w(3,1) = zero + 3.2_rn/three
w(5,1) = one + one/63._rn
w(7,1) = one + one/255._rn
w(2,1) = one/three
w(4,1) = two/30._rn
w(6,1) = one/63._rn
w(8,1) = one/255._rn
w(0,2) = two + two/7.0_rn
w(2,2) = one + 9._rn/55._rn
w(4,2) = one + one/(27._rn + one/2.25_rn)
w(6,2) = one + one/(112.5_rn + one/3.6_rn)
w(8,2) = one + one/(454._rn + one/9._rn)
w(1,2) = one + two/7._rn
w(3,2) = 9.0_rn/55.0_rn
w(5,2) = one/(27._rn + one/2.25_rn)
w(7,2) = one/(112.5_rn + one/3.6_rn)
w(9,2) = one/(454._rn + one/9.0_rn)
w(0,3) = 1.80_rn
w(2,3) = 1.125_rn
w(4,3) = one + two/7._rn/10._rn
w(6,3) = one + one/143._rn
w(8,3) = one + one/575._rn
w(1,3) = 0.8_rn
w(3,3) = 0.125_rn
w(5,3) = 0.20_rn/7._rn
w(7,3) = one/143._rn
w(9,3) = one/575._rn

!do i=0,9
!  do k=1,3
!    write(66,'(a,i0,a,i0,2(a,es20.10))') 'w(',i,',',k,')=',real(w(i,k),8),' w-wx=',real(w(i,k)-wx(i,k),8)
!  end do
!end do


!-----------------------------------------------------------------------
! choice:
compare_AnNu = .false.
   !compare_AnNu = .true.        ! perform a comparison between analytical and numerical derivatives

if(compare_AnNu) then
  WRITE(23,*) 'Begin of Auxdrg: mm, n, nr, nred =',mm,n,nr,nred
  write(23,*) 'eta=',(sngl(real(eta(i),8)),i=1,n)
  write(23,*) 'xpa=',(sngl(real(x(i),8)),i=1,nr)
  write(23,*) 'list=',int(list,2)
end if

ok = .TRUE.

l = nr + n

DO im=1,mm          ! number of real measurements
  i2 = 0

  DO il=1,l
    del = delta
    IF(il <= nr) THEN
      IF(list(il) == 1) THEN
        ! for a parameter to be fitted:
        i2 = i2 + 1
        lx = .TRUE.
        xsav = x(il)
           iy = 0
      ELSE
        CYCLE     ! to the end of the il loop
      END IF
    ELSE
      ! for the measured values, given in eta:
      i2 = i2 + 1
      lx = .FALSE.
      iy = il - nr
      xsav = eta(iy)
    END IF
!---------------------------
!   xsav now is the value of the parameter, according to which the partial derivative is formed.
!   If xsav is much less than 1, e.g., 2.0E-5, the starting value del
!   (originally 10) must be decreased!

    if(.not.compare_AnNu) then
      IF(dervtype == 'A') GOTO 5
    end if

    IF(ABS(xsav) < 1._rn) THEN
      del = 1.E-25_rn
      do WHILE(del <= 10._rn*ABS(xsav))
        del = del * 10._rn
      end do
        ! WRITE(23,*) 'xsav, del, eps, epsi=',sngl(xsav),sngl(del),sngl(eps),sngl(epsi)
    else
      del = 1.E+25_rn
      do WHILE(del >= 10._rn*ABS(xsav))
        del = del / 10._rn
      end do
       !  WRITE(23,*) 'xsav, del, eps, epsi=',sngl(xsav),sngl(del),  &
       !              sngl(eps),sngl(epsi)
    END IF

    if(.not.compare_AnNu) goto 10
!---------------------------

!-----------------------------------------------------------------------
!  For the following, the partial derivatives are derived analytically in
!  the user-supplied function LsqGfn2 (contained in the module WTLS).

5         CONTINUE

    fplus = lsqgfn(eta,x,n,nr,im)
    IF(lx) THEN
      ableit = dfda(il)      ! derivative with respect to the parameter
    ELSE
      ableit = 0._rn
      ! with respect to the y value:
      if(.not.ex1_Sline) then
        ! IF(iy == (nr+1)*im)   ableit = dfde(mfit+1)
        ! IF(iy == nr + (mxind+1)*im)   ableit = dfde(mxind+1)      ! 6.8.2023
        IF(i2 == nr + (mxind+1)*im)   ableit = dfde(mxind+1)      ! 6.8.2023
      else
        ! IF(iy == (nr)*im)   ableit = dfde(mfit)
        IF(iy == (mxind)*im)   ableit = dfde(mxind)      ! 6.8.2023
      end if

      ! with respect to the X value:
      if(.not.ex1_Sline) then
        ! do kk=1,nr
         do kk=1,mxind          ! 6.8.2023
          ! IF(iy == (nr+1)*im-kk) ableit = dfde(kk)
          ! IF(iy == nr + (mxind+1)*im-kk) ableit = dfde(kk)      ! 6.8.2023
          IF(i2 == nr + (mxind+1)*im-kk) ableit = dfde(kk)      ! 6.8.2023
        end do
      else
        ! IF(iy == (nr*im-1)) ableit = dfde(1)
        IF(iy == (mxind*im-1)) ableit = dfde(1)      ! 6.8.2023
      end if
    END IF

    t(0,9) = ableit   ! save the derivative on t(0,9), which finally is used
    tanlyt = ableit
       ! write(23,*) 'im=',int(im,2),' il=',int(il,2),' i2=',int(i2,2),' ableit=',sngl(ableit)

    if(.not.compare_AnNu) goto 20
    if(compare_AnNu) goto 10
!-----------------------------------------------------------------------
!  Calculate the partial derivatives numerically

    GOTO 10

    IF(lx) THEN
      h = xsav*dpafact(xsav) - xsav
      x(il) = xsav + h
    ELSE
      eta(iy) = xsav + h
    END IF
    fplus = lsqgfn(eta,x,n,nr,im)
    IF(lx) THEN
      x(il) = xsav - h
    ELSE
      eta(iy) = xsav - h
    END IF
    fminus = lsqgfn(eta,x,n,nr,im)
    IF(lx) THEN
      x(il) = xsav
    ELSE
      eta(iy) = xsav
    END IF
    t(0,9) = (fplus-fminus)/(h+h)
    lmt = .FALSE.
    GOTO 20

!-----------------------------------------------------------------------
10    CONTINUE

    DO i=1,10
      del = s*del

      if(abs(xsav) < 1.E-10_rn) del = max(del,0.01_rn)       ! important!! GK

      ! IF(i == 10 .OR. ABS(xsav+del*dx(9)-xsav) < eps) THEN     ! original; it is better not to use this statement
      IF(i == 10) THEN
         ok = .FALSE.
           WRITE(23,'(a,i2,a,i2,a,es11.4,a,es11.4,a,es11.4)') &
                 '   AUXDRG: i=',i,'  il=',il,'  xsav=',xsav, &
                 '  abs(xsav+del*dx(9)-xsav)=',abs(xsav+del*dx(9)-xsav),'  del=',del
          WRITE(23,*) 'eps, epsi = ',real(eps,4),real(epsi,4)
        RETURN
      END IF
      a = 0._rn
      t = 0._rn
      DO k=0,9
        h = del*dx(k)
        IF(lx) THEN
          ! for derivative with respect to parameters:
          x(il) = xsav + h
        ELSE
          ! for derivative with respect to mesaured values:
          eta(iy) = xsav + h
        END IF
        fplus = lsqgfn(eta,x,n,nr,im)
        IF(lx) THEN
          x(il) = xsav - h
        ELSE
          eta(iy) = xsav - h
        END IF
        fminus = lsqgfn(eta,x,n,nr,im)
        IF(lx) THEN
          x(il) = xsav
        ELSE
          eta(iy) = xsav
        END IF
        t(k,0) = (fplus-fminus)/(h+h)
        a(k) = t(k,0)
      END DO
      IF(a(0) >= a(9)) THEN
        a(0:9) = -a(0:9)
      END IF
      lmt = .TRUE.
      DO k=1,9
        h = a(k-1) - a(k)
        lmt = lmt .AND. (h <= epsi .OR. ABS(h) <= eps*ABS(a(k)) +epsi)
      END DO

      IF(lmt) EXIT
    END DO
               ! if(im >= 1) write(66,*) ' before w-loop:  t(0:9,0)=',sngl(t(0:9,0))
    DO m=1,9
      DO k=0,9-m
        IF(lev(m)) THEN
          t(k,m) = w(m-1,1)*t(k+1,m-1) - w(m,1)*t(k,m-1)
        ELSE IF(lev(k)) THEN
          t(k,m) = w(m-1,2)*t(k+1,m-1) - w(m,2)*t(k,m-1)
        ELSE
          t(k,m) = w(m-1,3)*t(k+1,m-1) - w(m,3)*t(k,m-1)
        END IF
      END DO
    END DO
!-----------------------------------------------------------------------
20        CONTINUE

    if(compare_AnNu) then
      if(abs(tanlyt) > 1.E-8_rn .and. abs((t(0,9)-tanlyt)/tanlyt)  > 0.001_rn .or.       &
         abs(tanlyt) < 1.E-8_rn .and. abs((t(0,9)-tanlyt))  > 0.0000001_rn )    &
               write (23,'(2(a,i2,1x),2x,4(a,es12.5,2x))') &
                 'im=',im,'i2=',i2,'ableitN=',t(0,9),'ableitA=',tanlyt, &
                             'h=',h,'xsav=',xsav
    end if

    e(im,i2) = t(0,9)

         ! if(im >= 1 .and. i2 >= 1) write(23,*) 'nr=',int(nr,2),' im=',int(im,2),' i2=',int(i2,2),' e(im,i2)=',sngl(e(im,i2)) ! !   '  array t(0,1:9)=',sngl(t(0,1:9))

    if(compare_AnNu) e(im,i2) = tanlyt

  END DO        ! il loop
END DO        ! im loop

END SUBROUTINE auxdrg
