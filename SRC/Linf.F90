
module LF1

!use Num1,         only: funcs,lfit8_92

contains

!#######################################################################

   ! Linf(rn0,SDrn0)      : weighted least squares calculation of net count rate rn0
   ! LinfAusf(rn0,sdrn0)  : calls Linf, writes the results to the file
   !                        linfout.txt and calls StoreLinfParms()
   ! StoreLinfParms(rn=,SDrn0) : stores the fit parameters and their
   !                             covariances in the GUI
   ! Linfout              : writes the fit-curve related results into linfout.txt

   !   Copyright (C) 2020-2023  Günter Kanisch

subroutine LinfAusf(mode,rn0,SDrn0)

use, intrinsic :: iso_c_binding,      only: c_ptr,c_int,c_null_char,c_long
USE UR_Gleich,          only: ifehl,loadingpro
USE UR_Linft,           only: ma,fpa,fpaSV,sfpa,sfpaSV,kfitp
USE UR_Variables,       ONLY: langg
USE UR_DLIM,            ONLY: iteration_on
use Rout,               only: pending_events
use Top,                only: WrStatusbar
use UR_params,          only: rn

implicit none

integer(4),INTENT(IN)  :: mode    ! 1: without call Linfout; 2: with call Linfout
real(rn), INTENT(OUT)  :: rn0     ! fitted net cout rate for the output quantity number kEGr
real(rn), INTENT(OUT)  :: SDrn0   ! its standard deviation

integer(4)      :: i
!-----------------------------------------------------------------------
ifehl = 0

IF(langg == 'DE') call WrStatusbar(4,'Rechnet...' )
IF(langg == 'EN') call WrStatusbar(4,'Calculating...' )
IF(langg == 'FR') call WrStatusbar(4,'Calcule...' )
if(.not.loadingPro) call pending_events()                !xx

call Linf(rn0,SDrn0)
IF(ifehl == 1) RETURN

IF(.not.iteration_on) THEN
  fpaSV(1:ma) = fpa(1:ma)
  sfpaSV(1:ma) = sfpa(1:ma)
END IF

IF(mode == 2) THEN
  IF(langg == 'DE') call WrStatusbar(4, &
       'Abklingkurven-Fit ansehen/speichern/drucken' )
  IF(langg == 'EN') call WrStatusbar(4, &
       'Decay curve fit: view/save/print' )
  IF(langg == 'FR') call WrStatusbar(4, &
       'Ajustement de la courbe de décroissance: afficher/enregistrer/imprimer' )
  call Linfout()
END IF

IF(kfitp(1) /= 0) call StoreLinfParms(rn0,SDrn0)

end subroutine LinfAusf

!#######################################################################

subroutine StoreLinfParms(rn0, SDrn0)

    !   Copyright (C) 2020-2023  Günter Kanisch

USE UR_Gleich,     only: klinf,knumEGr,loadingpro,missingval,Messwert,MesswertSV,StdUnc, &
                         StduncSV,covarval,corrval,covarvalSV
USE UR_Linft,      only: mfit,ifit,fpa,sfpa,kfitp,covar,covfpa
use Rout,          only: WTreeViewPutDoubleCell,pending_events
use UR_params,     only: rn,zero,one
use Top,           only: RealModA1
use Num1,          only: matwrite


implicit none

real(rn), INTENT(IN)  :: rn0
real(rn), INTENT(IN)  :: SDrn0

integer(4)        :: i,kx,k

Messwert(klinf) = rn0
MesswertSV(klinf) = Messwert(klinf)
StdUnc(klinf)   = SDrn0
StdUncSV(klinf) = SDrn0
call WTreeViewPutDoubleCell('treeview2', 5, klinf, Messwert(klinf))
call WTreeViewPutDoubleCell('treeview2', 11, klinf, StdUnc(klinf))

if(allocated(corrval)) deallocate(corrval)
if(allocated(covarvalSV)) deallocate(covarvalSV)
allocate(corrval(50),covarvalSV(50))
corrval = zero
covarvalSV = zero
do i=1,3
  IF(ifit(i) == 3) THEN
    IF(.false. .and. mfit == 2 .and. i == 3) THEN   ! 25.7.2023
      fpa(3) = one
      sfpa(3) = zero
    else
      fpa(i) = zero
      sfpa(i) = zero
    end if
  end if
  IF(i > 1 .AND. knumEGr == 1) CYCLE
  kx = kfitp(1) -1 + i
  Messwert(kx)   = fpa(i)
  MesswertSV(kx) = fpa(i)
  StdUnc(kx)     = sfpa(i)
  StdUncSV(kx)   = sfpa(i)
  call WTreeViewPutDoubleCell('treeview2', 5, kx, Messwert(kx))
  call WTreeViewPutDoubleCell('treeview2', 11, kx, StdUnc(kx))
end do
IF(mfit < 2 .or. knumEGr == 1) return

! call matwrite(covar,3,3,3,3,66,'(1x,130es13.5)','Linfout: Matrix covar :')

! Into treeview3, the correlation values of the fitparameters are written;
! however, internally the values of covariances are required

kx = kfitp(2)
if(ubound(Corrval,dim=1) < kx) call RealModA1(corrval,kx)
if(sfpa(1) > zero .and. sfpa(2) > zero) then
  Covarval(kx) = covar(1,2) / (sfpa(1)*sfpa(2))
  CorrVal(kx) = Covarval(kx)
else
  Covarval(kx) = -zero
end if
call WTreeViewPutDoubleCell('treeview3', 6, kx, CovarVal(kx))
Covarval(kx) = covar(1,2)
if(kx > ubound(CovarvalSV,dim=1)) call RealModA1(CovarvalSV,kx)
CovarValSV(kx) = Covarval(kx)

IF(mfit < 3) THEN
  Covarval(kfitp(2)+1) = missingval
  Covarval(kfitp(2)+2) = missingval
end if
kx = kfitp(2) + 1
if(ubound(Corrval,dim=1) < kx) call RealModA1(corrval,kx)
if(sfpa(2) > zero .and. sfpa(3) > zero) then
  Covarval(kx) = covar(2,3) / (sfpa(2)*sfpa(3))
  CorrVal(kx) = Covarval(kx)
else
  Covarval(kx) = zero
end if
call WTreeViewPutDoubleCell('treeview3', 6, kx, CovarVal(kx))
Covarval(kx) = covar(2,3)
CovarValSV(kx) = Covarval(kx)

kx = kfitp(2) + 2
if(ubound(Corrval,dim=1) < kx) call RealModA1(corrval,kx)
if(sfpa(1) > zero .and. sfpa(3) > zero) then
  Covarval(kx) = covar(1,3) / (sfpa(1)*sfpa(3))
  CorrVal(kx) = Covarval(kx)
else
  Covarval(kx) = zero
end if
call WTreeViewPutDoubleCell('treeview3', 6, kx, Covarval(kx))   ! format frmt
Covarval(kx) = covar(1,3)
CovarValSV(kx) = Covarval(kx)

covFPA = covar         ! copy of covar

if(.not.loadingPro) call pending_events()

end subroutine StoreLinfParms

!#######################################################################

subroutine Linf(rn0,SDrn0)

!   Copyright (C) 2020-2023  Günter Kanisch

USE UR_Gleich,     only: Messwert,MesswertSV,symbole,ngrs,ncov,kableitnum,ifehl,klinf,missingval, &
                         kpoint,upropa_on,StdUnc,isymbA,isymbB,covarval,kEGr
USE UR_Linft,      only: dnetrate,SDnetrate,covar,chisq,chisqr_nls,chisqr_wtls,chisqr, &
                         mpfxfixed,mpfx,fixedrate,SDfixedrate,parfixed,a,numd,mfit,ifit,Chis_test,export_case, &
                         export_r,fpa,sfpa,sfpaSV,ma,chisqrzz_wtls,cofact,cofactlyt,fitmeth, &
                         KPearson,kPMlE,kuse_fixed,ndatmax,nhp,nwei,posdef,wtls_wild,kfitp,dmesszeit, &
                         numd,k_rbl,use_wtls,tmedian,fpaSV,d0zrate,sd0zrate,dtdiff,fixedratemc,yfit,x,dnetfit, &
                         covyLF,xpl,ypl,uypl,yplfit,sdnetfit,mac

USE fparser,       ONLY: evalf, EvalErrMsg
USE UR_Perror
USE UR_DLIM,       ONLY: iteration_on,limit_typ
USE UR_MCC,        ONLY: kqtypx,imc
USE UR_Variables,  ONLY: MCsim_on,fname,batest_on,BATF,bat_serial, results_path
use Top,           only: WrStatusbar,dpafact
use Num1,          only: dpi_funcs,funcs,matwrite,find_mac
use UR_params,     only: rn,zero,one,two,eps1min
use LLcov2,        only: LinCov2
use WTLS,          only: GlsqUR2
use Rw1,           only: Find_lambda

implicit none

real(rn), INTENT(OUT)  :: rn0     ! fitted net cout rate for the output quantity number kEGr
real(rn), INTENT(OUT)  :: SDrn0   ! its standard deviation

integer(4)          :: i,irun,irunmx, k,klu,kx,kqt,ia(3)
integer(4)          :: kfall,j,jj1,jj2,ios

LOGICAL            :: ok
real(rn)           :: sd(ndatmax), afunc(ma)
real(rn)           :: tm,zfact,chisqrzz,yval,mfact
real(rn)           :: fv1,dpi,mwkabl,mwklu,rback,urback,parm
real(rn)           :: netratesub(ndatmax)
CHARACTER(LEN=150) :: str1,text
real(rn)           :: aG(ma),covarG(ma,ma), a_wls(ma),mw_rbl,umw_rbl
logical            :: setzero

!-----------------------------------------------------------------------

setzero = .false.
kqt = 1
if(iteration_on .and. limit_typ == 1) kqt = 2
if(iteration_on .and. limit_typ == 2) kqt = 3

! mac = 0      ! 17.9.2023
mwklu = zero
mwkabl = zero
klu = klinf
IF(kfitp(1) > 0) klu = kfitp(1)-1+kEGr

  ! array ifit:  ifit(k):  =1: parameter k is fitted;   =2: hold paramter k fiexed at its start value;
  !                        =3: parameter k is not included in fitting;
  ! arrays a, covar     :  fitting parameters and covar matrix
  ! arrays fpa, sfpa    :  fitting parameters and associated uncertainties

ifehl = 0
mfit = 0
do i=1,3
  IF(ifit(i) == 1) mfit = mfit + 1
  if(ifit(i) == 3) then
    fpa(i) = zero
    sfpa(i) = zero
    fpaSV(i) = zero
    sfpaSV(i) = zero
  end if
end do

if(mac == 0) call find_mac(mac)          ! 7.7.2023

zfact = one
mw_rbl = zero                   ! value of (net) blank count rate
umw_rbl = zero                  ! its standard deviation
if(k_rbl > 0) then
  mw_rbl = Messwert(kpoint(k_rbl))
  umw_rbl = StdUnc(kpoint(k_rbl))
end if
if(allocated(x)) deallocate(x)
if(allocated(fixedrate)) deallocate(fixedrate)
if(allocated(SDfixedrate)) deallocate(SDfixedrate)
if(allocated(yfit)) deallocate(yfit)
if(allocated(dnetfit)) deallocate(dnetfit)
if(allocated(SDnetfit)) deallocate(sDnetfit)

allocate(x(numd),fixedrate(numd),SDfixedrate(numd),yfit(numd),dnetfit(numd),sdnetfit(numd))

if(.true.) then
  ! calculate the net count rates of the decay curve and their standard uncertainties:
  dnetrate(1:numd) = Messwert(ngrs+ncov+1:ngrs+ncov+numd) - d0zrate(1:numd) - mw_rbl
  SDnetrate(1:numd) =[ (max(zero, Messwert(ngrs+ncov+i)/dmesszeit(i)),i=1,numd) ]
  SDnetrate(1:numd) = SDnetrate(1:numd) + sd0zrate(1:numd)**two + umw_rbl**two
  SDnetrate(1:numd) = sqrt(SDnetrate(1:numd))
  do i=1,numd
    if(dnetrate(i) <= zero) SDnetrate(i) = SDnetrate(i) * (one + 1.E-7_rn)
  end do
  sd(1:numd) = SDnetrate(1:numd)
  x(1:numd) = dtdiff(1:numd)
end if

 if(Messwert(ngrs+ncov+1) > 1.E+20_rn) then
   do i=1,ngrs+ncov+numd
     write(66,*) 'i=',i,' ',symbole(i)%s,' Messwert=',sngl(Messwert(i)),'  MEsswetSV=',sngl(MesswertSV(i))
   end do
 end if

!----
!   Only fixedrate() is used, but SDfixedrate() not!
!   The latter is used in Lincov2 within the uncertainty propagation
!   based on the matrix Qmat.
!   nhp: number of parameters given as arguments of UR Linfit() function; "mpfx parameters"
!   kableitnum: if > 0: designates the Messwert-element with respect to which a partial
!               derivative of the output quantity is being calculated

fixedrate(1:numd) = zero
SDfixedrate(1:numd) = zero
mpfxfixed(1:nhp) = 0

if(parfixed) then
  do k=1,3
    if(ifit(k) == 2) then
      do i=1,numd
        call funcs(i,afunc)
        fixedrate(i) = fixedrate(i) + afunc(k)
        !###### Note: afunc(k) needs not to be multiplied by a(k), because a(k)
        ! is already contained in afunc(k), based on the definition in the decay cure model dialog!

        if(kuse_fixed == 1) cycle      ! kuse_fixed is an internal test variable (Rechw1),
                                       ! which correctly should be = 2
        fv1 = afunc(k)
        do j=1,nhp
               if(kableitnum > 0) mwkabl = Messwert(kableitnum)
               if(kableitnum == klinf) mwklu = MEsswert(klu)
          if(abs(StdUnc(mpfx(j))-missingval) < eps1min .or. abs(StdUnc(mpfx(j))) < eps1min) cycle
          ! dpi: partial derivative of the output quantity with respect to Messwert(mpfx(j))
          dpi = dpi_funcs(mpfx(j),i,k,ma,fv1)
               if(kableitnum > 0) Messwert(kableitnum) = mwkabl     ! restore
               if(kableitnum == klinf) Messwert(klu) = mwklu        !
          SDfixedrate(i) = SDfixedrate(i) + ( dpi*StdUnc(mpfx(j)) )**two
             !  write(66,*) ' linf: k,i,j=',k,i,j,' dpi=',real(dpi,8)


          ! if dpi is /= 0, the uncertainty of Messwert(mpfx(j)) is already
          ! contained in fixedrate(i), then set mpfxfixed(j) = 1

          if(abs(dpi) > eps1min .and. abs(StdUnc(mpfx(j))) > eps1min .and. &
                           abs(StdUnc(mpfx(j))-missingval) > eps1min ) mpfxfixed(j) = 1
        end do
      end do
    end if
  end do
  do i=1,numd
    if(SDfixedrate(i) > zero) SDfixedrate(i) = sqrt(SDfixedrate(i))
  end do
end if
!----

if(.not.parfixed) then
  netratesub(1:numd) = dnetrate(1:numd)
else
  if(.not.MCsim_on) then
    netratesub(1:numd) = dnetrate(1:numd) - fixedrate(1:numd)
  else
    netratesub(1:numd) = dnetrate(1:numd) - fixedrateMC(1:numd)
  end if
  ! As contributions of mpfxfixed=1 parameters are involved here,
  ! their contributions are not considered by Linscov2 within the QMAT loop !
  sd(1:numd) = sqrt( sd(1:numd)**two + SDfixedrate(1:numd)**two )
end if

!----
IF(nwei == 0) sd = one   ! disregard the statistical weighting!

a(1:ma) = zero
irunmx = 1
  IF(kpearson == 1) irunmx = 3
IF(use_WTLS) irunmx = 1
IF(kPMLE == 1) irunmx = 1

do irun=1,irunmx

  IF(irun > 1) THEN
    ! With having kPMLE=1 or Neyman WLS, one should not arrive here.
    ! Note that PLSQ is done here within the irun loop!!!
    do i=1,numd
      call funcs(i,afunc)
        if(ifehl == 1) return
      yfit(i) = zero
      do k=1,ma
        parm = a(k)
        if(ifit(k) == 2) parm = one
        yfit(i) = yfit(i) + parm*afunc(k)
      end do
      yfit(i) = yfit(i) - fixedrate(i)
          rback = d0zrate(i) + mw_rbl
          urback = sqrt(sd0zrate(i)**two + umw_rbl**two)
      tm = dmesszeit(i)
      if(.not.parfixed) then
        sd(i) = SQRT( (MAX(zero,yfit(i)) + rback )/tm  + urback**two )
        if(yfit(i) <= zero) then
          sd(i) = sd(i)*(one + 1.E-7_rn)
        end if
      else
        sd(i) = SQRT( (MAX(zero,yfit(i)) + mw_rbl + d0zrate(i) + fixedrate(i) )/tm  + umw_rbl**two +  &
                      sd0zrate(i)**two + sdfixedrate(i)**two)
        if(abs(sd(i) - sqrt(umw_rbl**two + sd0zrate(i)**two + sdfixedrate(i)**two)) < 1.E-3_rn) sd(i) = sd(i)*1.002_rn
      end if
    end do
  END IF

  ! Linear fit with including covariances between input values:
  if(.not.allocated(covar)) allocate(covar(ma,ma))
  ia = 1
  call LinCov2(numd,mfit,netratesub,sd,a,covar,ma,Chisq,ok,ifehl)
  if(ifehl == 1) then
    do i=1,numd
      write(66,*) 'i=',i,'  netratesub=',sngl(netratesub(i)),  &
                         '  sd=',sngl(sd(i)),' dnetrate=',sngl(dnetrate(i)), &
                         '  Messwert(ngrs+ncov+i)=',sngl(Messwert(ngrs+ncov+i))
    end do
    return
  end if
     if(.false. .and. kqt < 2 .and. .not.upropa_on) then
       write(66,*) ' Linf: Params: ',(a(i),i=1,ma)
          !do i=1,numd
          !  write(66,*) 'i=',i,'  netratesub=',sngl(netratesub(i)),'   sd=',sngl(sd(i)),'  dnetrate=',sngl(dnetrate(i))
          !end do
     end if

  chisqr = chisq
  IF(numd > mfit) chisqr = chisq/real(MAX(numd-mfit,1),rn)
  Chisqr_NLS = Chisqr
  Chis_test(1) = zero
  Chis_test(2) = zero
  if(numd > mfit) Chis_test(1) = abs( chisqr_NLS*real(numd-mfit,rn) - real(numd-mfit,rn) ) /sqrt(two*real(numd-mfit,rn))

end do


!-----------------------
!   Export to R: produce data frame files for using them in R
kfall = 0
IF(export_r .and. .not.batest_on .and. .not.batf .and. .not.bat_serial .and. kpmle /= 1) THEN
  kfall = 0
  IF(.not.export_case(1)) kfall = 1
  IF(export_case(1) .AND. .not.export_case(2)) kfall = 2
  IF(export_case(2) .AND. .not.export_case(3)) kfall = 3
  IF(kfall == 1) THEN
    OPEN(77,FILE=trim(results_path)//'URExport-to-R.txt',STATUS='unknown')
    WRITE(77,*) '############################################################'
    WRITE(77,*)
    WRITE(77,'(a,a)') 'File = ',TRIM(fname),'    Fit method=',TRIM(fitmeth)
    WRITE(77,*)
    WRITE(77,*) 'Case: output quantity'
    export_case(2) = .FALSE.
    export_case(3) = .FALSE.

    OPEN(78,FILE=trim(results_path)//'covmat1.txt',STATUS='unknown')
    OPEN(79,FILE=trim(results_path)//'data1.txt',STATUS='unknown')
  end if

  !! if(.not.iteration_on)  write(66,*) 'Linf: export_r=',export_r,' exprt_case=',export_case, &
  !!             ' kfall=',int(kfall,2)

  ! Note: the case of the detection limit is omitted here:
  IF(kfall > 0) then
    IF(.not.export_case(kfall) .AND. kableitnum == 0     &
         .and. (kfall == 1 .or. (kfall == 2 .and. iteration_on .and. limit_typ == 1 .AND. ABS(a(1)) < 1.E-8_rn ) ) ) THEN
      IF(kfall == 2 .AND. iteration_on .AND. limit_typ == 1 ) THEN
        close (78)
        close (79)
        OPEN(78,FILE=trim(results_path)//'covmat2.txt',STATUS='unknown')
        OPEN(79,FILE=trim(results_path)//'data2.txt',STATUS='unknown')

        WRITE(77,*)
        WRITE(77,*) 'Case: Decision threshold'
      end if

      WRITE(77,*)
      WRITE(77,*) 'Blank count rate=',sngl(mw_rbl),'  background rate=',sngl(d0zrate(1))      !,'  kableitnum=',kableitnum

      WRITE(77,*) 'Input data: variance-covariance matrix:   (rank=',numd,')'
      WRITE(77,*)
      do i=1,numd
        IF(kPMLE /= 1) WRITE(77,'(40es13.5)') (covyLF(i,k),k=1,numd)
        IF(kPMLE /= 1) WRITE(78,'(40es13.5)') (covyLF(i,k),k=1,numd)

        IF(kPMLE == 1) yval = ( dnetrate(i) + mw_rbl + d0zrate(i) ) * dmesszeit(i)
        IF(kPMLE == 1) WRITE(77,'(40es13.5)') (zero,k=1,i-1), yval, (zero,k=i+1,numd)
        IF(kPMLE == 1) WRITE(78,'(40es13.5)') (zero,k=1,i-1), yval, (zero,k=i+1,numd)
      end do
      WRITE(77,*)

      WRITE(77,*) 'Arrays y, X1, x2, X3: '
      WRITE(77,*)
      text = '     y            ' // 'X1           '
      IF(ifit(2) == 1 .AND. ifit(3) > 1) text = '     y            ' // 'X1           ' // 'X2           '
      IF(ifit(2) == 1 .AND. ifit(3) == 1) text = '     y            ' // 'X1           ' // 'X2           ' // 'X3           '
      IF(ifit(2) > 1 .AND. ifit(3) == 1) text = '     y            ' // 'X1           ' // 'X3           '
      IF(kPMLE == 1 .and. ifit(2) > 1 .AND. ifit(3) > 1) text = '     y            ' // 'X1           ' // 'X2           '

      WRITE(77,'(A)') TRIM(text)
      WRITE(79,'(A)') TRIM(text)
      do i=1,numd
        call funcs(i,afunc)
            yval = dnetrate(i)
               yval = yval - fixedrate(i)
            IF(kPMLE == 1) then
              yval = ( dnetrate(i) + fixedrate(i) + mw_rbl + d0zrate(i) ) * tmedian
            end if

        WRITE(text,'(i3,1x,7es13.5)') i,yval,(afunc(j),j=1,ma)
        IF(ifit(2) > 1) THEN
          IF(kPMLE /= 1) THEN
            text = text(1:30) // text(44:56)
          else
            text = text(1:43)
          end if
        end if
        IF(ifit(3) > 1) text = text(1:43)
        WRITE(77,'(a)') TRIM(text)
        WRITE(79,'(a)') TRIM(text)
      end do
      close (78)
      close (79)

      WRITE(77,*)
      WRITE(77,*) 'Parameter values and std uncertainties obtained by UR: '
      do i=1,ma
        ! IF(ifit(i) == 0) cycle
        IF(ifit(i) > 1) cycle
        mfact = one
        IF(kPMLE == 1) mfact = tmedian
        WRITE(77,'(i2,1x,7es13.5)') i, a(i)*mfact, SQRT(covar(i,i))*mfact
      end do
      WRITE(77,*) ' Chisqr=',sngl(chisq)/REAL(max(1,numd-mfit),rn)
      WRITE(77,*)

      IF(kfall == 1) export_case(1) = .TRUE.
      IF(kfall == 2) THEN
        export_case(3) = .TRUE.
      end if

    end if
  end if
end if

!-----------------------

IF(kpearson == 1) THEN
  sd(1:numd) = SDnetrate(1:numd)
  if(parfixed) sd(1:numd) = sqrt( SDnetRate(1:numd)**two + SDfixedrate(1:numd)**two )
end if

WTLS_wild = .false.
! xxxxxxxxxxxxxxxxxxxxxxxx   invoking weighted total least squares
 IF(use_WTLS .and. kPMLE == 0) THEN

   aG(1:ma) = a(1:ma)
   a_wls(1:ma) = a(1:ma)
   do j=1,1
     aG(1:ma) = a_wls(1:ma)
     call GlsqUR2(aG,covarG,ifehl)
     ! if(kqt >= 2 .or. kableitnum > 0) exit
     if(kqt >= 2) exit
     !write(66,*) 'j=',int(j,2),' WTLS: aG :',real(aG(1:ma),8)
     !call matwrite(covarG,ma,ma,66,'(10(es17.10,2x))','Matrix aG:')
   end do

   IF(ifehl == 1) RETURN
   if(.not. posdef) then
     WTLS_wild = .true.
     ifehl = 1
   end if

   a(1:ma) = aG(1:ma)
   covar(1:ma,1:ma) = covarG(1:ma,1:ma)

 end if
 chisqr = chisq
 IF(numd > mfit) chisqr = chisq/real(MAX(numd-mfit,1),rn)

  if(WTLS_Wild) then
    write(23,*)
    write(23,*) 'Linf after call GLSQUR2: Fitted net count rates:    Chisqr=',sngl(Chisqr),  &
                '  kqtypx=',kqtypx ,'  posdef=',posdef
    do i=1,numd
      call funcs(i,afunc)
      yfit(i) = dot_product(aG(1:ma),afunc(1:ma))
      yfit(i) = yfit(i) - fixedrate(i)
      write(23,'(a,i2,10(a,f10.5,1x))') 'i=',i,' netratesub=',netratesub(i),' sd=',sd(i),' yfit=',  &
                 yfit(i),' utest=',(yfit(i)-netratesub(i))/sd(i), &
                 ' fixedrate=',fixedrate(i)
    end do
    write(23,*)
  end if

! Alternatively calculated Chisqr:
Chisqrzz = zero
do i=1,numd
  call Funcs(i,afunc)
  dnetfit(i) = zero
  do k=1,ma
    parm = a(k)
    if(ifit(k) == 2) parm = one
    if(ifit(k) == 2 .and. kPMLE == 1) parm = zero
    dnetfit(i) = dnetfit(i) + parm * afunc(k)
  end do
  if(SDnetrate(i) > zero) Chisqrzz = Chisqrzz + (dnetrate(i)-dnetfit(i))**two / SDnetrate(i)**two
end do
if(kPMLE /= 1) Chisq = chisqrzz
Chisqrzz_WTLS = chisqrzz/real(max(1,numd-mfit),rn)    ! Chisqr from WTLS, calculated similarly as with WLS

! Arrays for CurvePlot:
if(kqt == 1 .and. .not.iteration_on .and. kableitnum == 0  .and.    &
   .not.MCsim_on ) then
  if(allocated(xpl)) deallocate(xpl,ypl,uypl,yplfit)
  allocate(xpl(numd),ypl(numd),uypl(numd),yplfit(numd))
  xpl(1:numd) = x(1:numd) / 3600._rn      ! unit : hour
  ypl(1:numd) = netratesub(1:numd)
  uypl(1:numd) = sd(1:numd)
  yplfit(1:numd) = dnetfit(1:numd)

end if

! xxxxxxxxxxxxxxxxxxxxxxxx

chisqr = chisq
IF(numd > mfit) chisqr = chisq/real(MAX(numd-mfit,1),rn)
if(use_WTLS) chisqr = chisqr_WTLS

do i=1,ma
  fpa(i) = a(i)
  IF(.not.iteration_on) THEN
    fpaSV(i) = a(i)
    sfpaSV(i) = zero
    if(covar(i,i) > zero) sfpaSV(i) = SQRT(covar(i,i))
  end if
end do

sfpa(1:ma) = zero
do i=1,ma
  if(covar(i,i) > zero) sfpa(i) = SQRT(covar(i,i))
end do

rn0 = a(kEGr)
SDrn0 = sfpa(kEGr)


IF(iteration_on) THEN

  !############
  StdUnc(klu) = SDrn0
  !############

  ! Up to now, it was not considered, that the covariance values between fit parameters
  ! can vary through the iteration for DT or DL.

  IF(mfit > 1 .AND. kfitp(2) > 0 ) THEN
    kx = kfitp(2) - 1   ! row number in the covar-table treevie3
    do k=1,3
      read(Symbole(IsymbA(k))%s(5:5),*,iostat=ios) jj1          ! index of the "left-hand" Fitp parameter
      read(Symbole(IsymbB(k))%s(5:5),*,iostat=ios) jj2          ! index of the "right-hand" Fitp parameter
           if(ios /= 0) then
             write(66,*) 'Linf:    Covar: Error!  Reading the FITP index does not work; symbols mixed up??'
             write(66,*) '      Symbole : ',Symbole(IsymbA(k))%s,'  ',Symbole(IsymbB(k))%s
           end if
      Covarval(kx+k) = covar(jj1,jj2)
    end do
    ! call matwrite(covar,3,3,3,3,66,'(1x,130es13.5)','Linf: Matrix covar :')
  end if
end if

if(.not.MCSim_on .and. .not.iteration_on .and. kableitnum == 0 .and. cofact > zero .and. use_WTLS) then
  write(str1,'(a,es8.1)') 'cofact=1-',one-cofactlyt
  call WrStatusBar(2, trim(str1))
  ! write(66,*) trim(str1)
end if

if(.false. .and. use_WTLS .and. imc < 50 ) write(23,*) 'Linf am Ende:   fpa =',(sngl(fpa(i)),i=1,3),  &
                                                           ' sfpa=',(sngl(sfpa(i)),i=1,3)

end subroutine Linf

!#######################################################################

subroutine Linfout()

    !   Copyright (C) 2020-2023  Günter Kanisch

USE UR_Gleich,       only: loadingpro,nab,Rseite
USE UR_Linft,        only: ma,chisq,ndatmax,fitmeth,kPMLE,mfit,ifit,mfrbg_2_fitnonlin, &
                           nkovzr,numd,dnetfit,SDnetfit,fpa,covar,mfrbg, &
                           dbzrate,sfpaSV,dnetrate,SDnetrate,dtdiff,sdbzrate
USE UR_Variables,    ONLY: langg,results_path
use Brandt,          only: gincbt
use Num1,            only: funcs
use UR_params,       only: rn,zero,eps1min,one,two

implicit none

integer(4)        :: i,k,jdr,nterms,k1,k2,ios,ii1,kk
real(rn)          :: xd(ma,ndatmax)
real(rn)          :: afunc(ma),rpa(ma),rfi
real(rn),allocatable  :: drelf(:),utest(:),dfit(:),SDdfit(:)

real(rn)          :: tval(ma),pval(ma),df,parm,minval_net,scalef
real(rn)          :: dyda,dyda1,dyda2,u,zfact, chisqrr, dummy,chisqr3
CHARACTER(LEN=90) :: headline
character(len=11) :: cdnetzf,cdfitzf,znform
character(len=12) :: ctfpa(3)
character(len=9)  :: ccr
character(len=8)  :: cxd(3)
LOGICAL           :: gross
!-----------------------------------------------------------------------

close (22)
OPEN(22,FILE=trim(results_path) // 'linfout.txt',status='unknown')
jdr = 22

gross = .FALSE.
IF(kPMLE == 1) THEN
  IF(ifit(mfrbg) <= 2) gross = .TRUE.
end if



allocate(drelf(numd),utest(numd),dfit(numd),SDdfit(numd))

chisqr3 = zero
    minval_net = 1.E+30_rn
     ! write(22,*) 'ifit=',int(ifit,2)

do i=1,numd
  call Funcs(i,afunc)


  dnetfit(i) = zero
  do k=1,ma
    xd(k,i) = afunc(k)
    if(ifit(k) == 1) then
      dnetfit(i) = dnetfit(i) + fpa(k) * afunc(k)
    elseif(ifit(k) == 2) then
      if(k == 2 .and. mfrbg_2_fitnonlin) then
        dnetfit(i) = dnetfit(i) + fpa(k) * afunc(k)
      else
        dnetfit(i) = dnetfit(i) + one * afunc(k)
      end if
    end if
  end do
         ! write(22,*) ' xd(1:3,i)=',sngl(xd(1:3,i))
  dfit(i) = dnetfit(i)
  if(dnetrate(i) > eps1min .and. dnetrate(i) < minval_net) minval_net = dnetrate(i)

  IF(.not.gross) drelf(i) = (dnetrate(i)-dnetfit(i))/dnetfit(i)*100._rn
  IF(gross) drelf(i) = (dbzrate(i)-dfit(i))/dfit(i)*100._rn

  !  calculate the uncertainties u of the fitted values of the input values:
  u = zero
  do k1=1,ma
    dyda = afunc(k1)
    u = u + dyda**two * covar(k1,k1)
  end do
  do k1=1,ma-1
    do k2=k1+1,ma
      dyda1 = afunc(k1)
      dyda2 = afunc(k2)
      u = u + two * dyda1*dyda2*covar(k1,k2)
    end do
  end do
  u = SQRT(ABS(u))
  SDnetfit(i) = u
  SDdfit(i) = u

  chisqr3 = chisqr3 + (dnetfit(i)-dnetrate(i))**two/sdnetrate(i)**two
end do

scalef = 1.0_rn
if(minval_net < 1.e-6_rn) then
  do i=1,18
    scalef = 10._rn**real(i,rn)
    if(minval_net * scalef > 1.e-3_rn) then
      exit
    end if
  end do
end if

IF(numd > mfit) chisqr3 = chisqr3/real(numd-mfit,rn)

nterms = mfit
do i=1,ma
  tval(i) = zero
  pval(i) = zero
  rpa(i)  = zero
  if(abs(fpa(i)) > zero) then
    ! in the following 3 statements a simple t test is performed
    tval(i) = abs(fpa(i)/sfpaSV(i))
    df = max (1,numd-nterms)
    pval(i) = gincbt(0.5_rn*df,0.5_rn,df/(df+tval(i)**two))
    rpa(i) = 100._rn*sfpaSV(i)/fpa(i)
  end if
end do

IF(nkovzr == 0 .or. gross) THEN
  IF(langg == 'DE') headline = 'Ergebnis der Abklingkurven-Analyse (ohne Kovarianzen):'
  IF(langg == 'EN') headline = 'Result of decay curve analysis (without covariances):'
  IF(langg == 'FR') headline = 'Résultat de l''analyse de la courbe de décroissance (sans covariances):'
END IF
IF(nkovzr == 1 .and. .not.gross) THEN
  IF(langg == 'DE') headline = 'Ergebnis der Abklingkurven-Analyse (mit Kovarianzen):'
  IF(langg == 'EN') headline = 'Result of decay curve analysis (with covariances):'
  IF(langg == 'FR') headline = 'Résultat de l''analyse de la courbe de décroissance (avec covariances):'
END IF
IF(langg == 'DE') headline = TRIM(headline) // '      Verfahren: ' // TRIM(fitmeth)
IF(langg == 'EN') headline = TRIM(headline) // '      Method: ' // TRIM(fitmeth)
IF(langg == 'FR') headline = TRIM(headline) // '      Méthode: ' // TRIM(fitmeth)

WRITE(jdr,'(a)') TRIM(headline)
WRITE(jdr,'(10x,a,/)') 'LinFit(t) = a1*X1(t) + a2*X2(t) + a3*X3(t)'

if(scalef > 10._rn) write(jdr,'(a,es7.1,/)') 'count rate scaled with factor ',scalef

ccr = '  NetRate'
if(gross) ccr = 'GrossRate'
WRITE(jdr,'(a,a,a,a,/,a,a,/,89("-"))')  '  i      t     X1(t)    X2(t)    X3(t)   ',ccr,'    rUnc. ',  &
             '      LinFit    relDev  uTest',  &
             '        (m)                                (cps)       (%)  ',  &
             '      (cps)       (%)'
zfact = one
do i=1,numd
  rfi = zero
   cxd = ' '
  ! re-formatting the numbers, if too big for (f11.6):

  do kk=1,3
    ios = 10
    ii1 = 5
    znform ='(f8.5)'
    do
      write(cxd(kk),znform) xd(kk,i)
      read(cxd(kk),*,iostat=ios) dummy
      if(ios == 0) exit
      if(ios /= 0 .and. ii1 > 0) then
        ii1 = ii1 - 1
        if(ii1 > 0) then
          write(znform,'(a,i1,a)') '(f8.',ii1,')'
          write(cxd(kk),znform) xd(kk,i)
        end if
      end if
    end do
  end do

  ii1 = 7
  znform ='(f11.7)'
  if(.not.gross) write(cdnetzf,znform) dnetrate(i)*zfact*scalef
  if(gross)      write(cdnetzf,znform) dbzrate(i)*zfact*scalef

  ios = 1000
  do while( ios /= 0)
    read(cdnetzf,*,iostat=ios) dummy
    if(ios /= 0 .and. ii1 > 0) then
      ii1 = ii1 - 1
      if(ii1 > 0) then
        write(znform,'(a,i1,a)') '(f11.',ii1,')'
        if(.not.gross) write(cdnetzf,znform) dnetrate(i)*zfact*scalef
        if(gross)      write(cdnetzf,znform) dbzrate(i)*zfact*scalef
      end if
    end if
  end do
  ii1 = 7
  znform = '(f11.7)'
  if(.not.gross) write(cdfitzf,znform) dnetfit(i)*zfact*scalef
  if(gross)      write(cdfitzf,znform) dfit(i)*zfact*scalef

  ios = 1000
  do while( ios /= 0)
    read(cdfitzf,*,iostat=ios) dummy
    if(ios /= 0) then
      ii1 = ii1 - 1
      if(ii1 > 0) then
        write(znform,'(a,i1,a)') '(f11.',ii1,')'
        if(.not.gross) write(cdfitzf,znform) dnetfit(i)*zfact*scalef
        if(gross)      write(cdfitzf,znform) dfit(i)*zfact*scalef
      end if
    end if
  end do
  select case (gross)
    case (.false.)
      IF(abs(dnetrate(i)) > eps1min) rfi = abs(100._rn*SDnetrate(i)/(dnetrate(i)*scalef))
      utest(i) = (dnetrate(i) - dnetfit(i)) / SQRT(SDnetrate(i)**two + SDnetfit(i)**two)
      write(jdr,153) i,dtdiff(i)/(60._rn),cxd(1),cxd(2),cxd(3),  &
                     cdnetzf,rfi,cdfitzf,drelf(i),utest(i)   ! , rfi
    case (.true.)
      rfi = 100._rn*SDbzrate(i)/dbzrate(i)
      utest(i) = (dbzrate(i) - dfit(i)) / SQRT(SDbzrate(i)**two + SDdfit(i)**two)
      write(jdr,153) i,dtdiff(i)/(60._rn),cxd(1),cxd(2),cxd(3),  &
                     cdnetzf,rfi,cdfitzf,drelf(i),utest(i)
  end select
153     format(i3,f9.2,3(1x,a8),1x,a11,1x,f7.2,1x,  &
        1x,'| ',a11,2x,f5.1,3x,f4.1,     6x,es12.5)
end do

WRITE(jdr,'(88("-"))')

chisqrr = chisq
IF(numd > mfit) chisqrr = chisqrr/real(numd-mfit,rn)

do i=1,3
  znform = '(f12.7)'
  ii1 = 7
      parm = fpa(i)
      if(ifit(i) == 2) parm = one
      if(i == 2 .and. mfrbg_2_fitnonlin .and. mfrbg > 0) parm = fpa(i)
  write(ctfpa(i),znform) parm*zfact*scalef

  ios = 1000
  do while( ios /= 0)
    read(ctfpa(i),*,iostat=ios) dummy
    if(ios /= 0) then
      ii1 = ii1 - 1
      if(ii1 > 0 ) then
        write(znform,'(a,i1,a)') '(f11.',ii1,')'
        write(ctfpa(i),znform) parm*zfact*scalef
      end if
    end if
  end do
end do

if(numd >= nterms .AND. langg == 'DE') write(jdr,158)      &
                   (ctfpa(i),i=1,3),                       &
                   (rpa(i),i=1,3),                         &
                   chisqrr,                                &
                   (pval(i),i=1,3)

158   format('LinFit:  a1=',a12,2x,' a2=',a12,2x,' a3=',a12,  &
      2x,'(in cps angegeben !)'/,                                &
      7x,' ra1= ',f9.3,3x,' ra2= ',f9.3,3x,' ra3= ',f9.3,        &
      4x,'(in  %  angegeben !)',/,                               &
      7x,19x,37x,'CHi2R=',es11.3,/,                              &
      7x,'Prob= ',f8.6,4x,'Prob= ',f8.6,4x,'Prob= ',f8.6,        &
      3x,'(t-Test-Signifik. !)')

if(numd >= nterms .AND. langg == 'EN') write(jdr,160)      &
                   (ctfpa(i),i=1,3),                       &
                   (rpa(i),i=1,3),                         &
                   chisqrr,                                &
                   (pval(i),i=1,3)

160   format('LinFit:  a1=',A12,2x,' a2=',A12,2x,' a3=',A12,     &
      2x,'(given in cps !)'/,                                    &
      7x,' ra1= ',f9.3,3x,' ra2= ',f9.3,3x,' ra3= ',f9.3,        &
      4x,'(given in  %  !)',/,                                   &
      7x,19x,37x,'CHi2R=',es11.3,/,                              &
      7x,'Prob= ',f8.6,4x,'Prob= ',f8.6,4x,'Prob= ',f8.6,        &
      3x,'(t-test-signific. !)')

if(numd >= nterms .AND. langg == 'FR') write(jdr,162)      &
                   (ctfpa(i),i=1,3),                       &
                   (rpa(i),i=1,3),                         &
                   chisqrr,                                &
                   (pval(i),i=1,3)

162   format('LinFit:  a1=',A12,2x,' a2=',A12,2x,' a3=',A12,  &
      2x,'(donné en cps !)'/,                                    &
      7x,' ra1= ',f9.3,3x,' ra2= ',f9.3,3x,' ra3= ',f9.3,        &
      4x,'(donné en  %  !)',/,                                   &
      7x,19x,37x,'CHi2R=',es11.3,/,                              &
      7x,'Prob= ',f8.6,4x,'Prob= ',f8.6,4x,'Prob= ',f8.6,        &
      3x,'(t-test-signific. !)')

!-----------------------------------------------------------------------

close (22)

deallocate(drelf,utest,dfit,SDdfit)

END subroutine Linfout

!#######################################################################


!#######################################################################



end module LF1
