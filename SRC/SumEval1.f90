


!##############################################################################################

subroutine SumEvalCalc(yval,uyval)

    ! this routine returns the values yval and its standard uncertainty uyval
    ! by evaluating the UR function SumEval, e.g.,
    !    aSum = SumEval(1,4, a1,a2,a3,a4)
    ! yval and uyval refer to asum, which is to be calculated as the sum or a mean,
    ! of the 4 values a1,a2,a3 and a4. The latter are 4 independently measured
    ! activities.
    !
    ! The uncertainty uyval is calculated by numerical uncertainty propagation over the
    ! independent input quantities (number nux, symbols symb_nux), where covariances
    ! are taken into account when one of the input quantities, e.g. the background count
    ! rate, is used more than once in the calculation of yval.
    !
    ! See chapter 6.13 "Aggregating activities of several aliquots" of the UncertRadio
    ! CHM Help file for more details.
    !
    !     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,       only: rn,eps1min,zero,one,two
use, intrinsic :: iso_c_binding,   only: c_int
use gtk,             only: gtk_buttons_OK, GTK_MESSAGE_WARNING
use UR_gleich,       only: nparts,charv,Symbole,ngrs,nab, &
                           ifehl,Messwert,StdUnc,missingval,upropa_on,ksumeval,nux,symb_nux, &
                           modeSEval,iavar,faliq,upropa_on,RS_SymbolNr,kableitnum,mfactSE
use UR_DLIM,         only: iteration_on,limit_typ
use UR_VARIABLES,    only: MCsim_on,langg,modvar_on
use UWB,             only: gevalf
use Top,             only: dpafact
use LF1G,            only: LsqCoxGWM
use UR_MCC,          only: kqtyp,imc
use CHF,             only: FindlocT,testSymbol
use Rout,            only: MessageShow
use UR_Gspk1Fit,     only: Uxa,UxaMV

implicit none

real(rn),intent(out)   :: yval,uyval

integer(4)                :: i,j,nabx,symb_nabx(25),k1,k2,j1,nr,n1,kfound,iw,inet,kqt,if3
real(rn)                  :: gwx(1),gwcx(1,1),chisq,dpa,term
real(rn),allocatable      :: xvect(:),Uxinva(:,:),FV1(:),FV2(:),MesswertKP(:),ableit(:,:), &
                             dpi(:,:),SDx(:,:),xvuncorrt(:)
logical                   :: ok,found,wzero
character(:), allocatable :: str1
integer(C_int)            :: resp

! nparts: second parameter in the EvalSum function: number of activity values
! avar  : names of activity variables being arguments of SumEval function
! iavar : associated indexes in the symbol list
! nux   : number of independent input quantities
! symb_nux() : array symbols of independent input quantities
!

allocate(xvect(nparts))
allocate(Uxinva(nparts,nparts))
allocate(FV1(nparts),FV2(nparts))
allocate(MesswertKP(ngrs))
allocate(Ableit(nparts,1))    ! nparts))
allocate(xvuncorrt(nparts))

allocate(character(len=400) :: str1)

MesswertKP(1:ngrs) = Messwert(1:ngrs)

kqt = 1
if(iteration_on .and. limit_typ == 1) kqt = 2
if(iteration_on .and. limit_typ == 2) kqt = 3

if(nux > 0) goto 50

nabx = 0
symb_nabx = 0
nux = 0
symb_nux = 0
! Tests whether the dependent symbol with number iavar(i) (<= nab) depends on the symbol
! with number j.
do i=1,nparts
  do j=nab+1,ngrs   ! loop over independent (input) symbols
    call FindSymb(iavar(i),j,found, kfound)
    if(found) then
      if(nux == 0) then
        nux = nux + 1
        symb_nux(nux) = j
      else
        if(findloc(symb_nux,j,dim=1) == 0) then
          nux = nux + 1
          symb_nux(nux) = j
        end if
      end if
    end if
  end do
end do

50   continue

allocate(dpi(nparts,nux))
if(.not. allocated(SDx)) allocate(SDx(nparts,nux))
if(.not. allocated(Uxa)) allocate(Uxa(nparts,nparts))
if(.not. allocated(UxaMV)) allocate(UxaMV(nparts,nparts))

if(.true. .and. .not.iteration_on .and. .not.upropa_on .and. (.not.MCsim_on .or. modvar_on)) then
  write(66,*) 'SumEvalCalc: nux=',int(nux,2)
  !do i=1,nux
  !  write(66,*) 'i=',int(i,2),' symb_nux=',int(symb_nux(i),2),' ',Symbole(symb_nux(i))%s
  !end do
  write(66,*) 'symbols used in summands:  Symb_nux = ',(Symbole(symb_nux(i))%s,' ',i=1,nux)
end if

if(.true. .and. .not.iteration_on .and. .not.upropa_on .and. .not.MCsim_on .and. .not.modvar_on) then
  do i=1,nparts
    inet = RS_SymbolNR(iavar(i),2)
    iw = RS_SymbolNR(iavar(i),1)
    mfactSE(i) = Messwert(iavar(i)) / Messwert(inet)/Messwert(iw)
  end do
end if

  if(.false. .and. MCsim_on) then
    Uxa = zero
    do i=1,nparts
      Uxa(i,i) = 20._rn
    end do
    goto 70
  end if

Fv1 = zero
Fv2 = zero
Uxa = zero
wzero = .false.
do k1=1,nparts

  Fv1(k1) = Messwert(iavar(k1))
  xvect(k1) = Fv1(k1)

  inet = RS_SymbolNR(iavar(k1),2)
  iw   = RS_SymbolNR(iavar(k1),1)
  if(abs(Messwert(iw)) < eps1min) wzero = .true.
end do
 if(wzero) then
   IF(langg == 'DE') WRITE(str1,*) 'Ein Wert eines Einzel-Kalibrierfaktors ist null!', char(13), &
                     'Ersetze es durch z.B. 1.E-7!'
   IF(langg == 'EN') WRITE(str1,*) 'A value of a single calibration factor is zero!', char(13), &
                     'Replace it with e.g. 1.E-7! '
   IF(langg == 'FR') WRITE(str1,*) 'La valeur d''un seul facteur d''étalonnage est zéro!', char(13), &
                      'Remplacez-le par ex. 1.E-7!'
   call MessageShow(trim(str1), GTK_BUTTONS_OK, "SumEval:", resp,mtype=GTK_MESSAGE_WARNING)
   ifehl = 1
   return
 end if

! Prepare the partial derivatives and the standard uncertainties as matrices:
do k1=1,nparts
  do j1=1,nux
    n1 = symb_nux(j1)
    if(abs(StdUnc(n1) - missingval) < eps1min .or. abs(StdUnc(n1)) < eps1min) then
      SDx(k1,j1) = zero
      dpi(k1,j1) = zero
      cycle
    end if
    SDx(k1,j1) = StdUnc(n1)
    dpa = Messwert(n1)*dpafact(Messwert(n1)) - Messwert(n1)
        if(abs(dpa) < eps1min) dpa = 1.0E-10_rn
    Messwert(n1) = Messwert(n1) + dpa
    do j=nab,ksumeval+1,-1
      Messwert(j) = gevalf(j,Messwert)
    end do
    Fv2(k1) = gevalf(iavar(k1),Messwert)
    dpi(k1,j1) = (Fv2(k1)/dpa - Fv1(k1)/dpa)     ! partial derivative with respect to messwert(symb_nux(j1))
    Messwert(1:ngrs) = MesswertKP(1:ngrs)     ! restore the whole messwert array
  end do
end do

do k1=1,nparts

  do j1=1,nux
    Uxa(k1,k1) = Uxa(k1,k1) + dpi(k1,j1)*dpi(k1,j1)* SDx(k1,j1)**two
  end do
    if(.true.) then      ! calculate variances
      if(kqt == 1) then
        if(kableitnum == 0 .and. .not.iteration_on) then
          UxaMV(k1,k1) = Uxa(k1,k1)
        end if
        if(kableitnum > 0 .and. kableitnum /= ksumeval) then
          Uxa(k1,k1) = UxaMV(k1,k1)
        end if
      else
        ! UxaMV(i) is set in Modvar
        if(UxaMV(k1,k1) > zero) Uxa(k1,k1) = UxaMV(k1,k1)
      end if
    end if

  if(k1 == nparts) cycle
                 !!! cycle    ! omit covariances
  do k2=k1+1,nparts    ! calculate covariances
    do j1=1,nux
      term = dpi(k1,j1)*dpi(k2,j1)* SDx(k2,j1)**two
      Uxa(k1,k2) = Uxa(k1,k2) + term
      term = dpi(k2,j1)*dpi(k1,j1)* SDx(k2,j1)**two
      Uxa(k2,k1) = Uxa(k2,k1) + term
         !if(MCsim_on .and. kqtyp == 3 .and. imc <= 10 .and. k1 == 1) then
         !  write(63,'(a,3i3,6(a,es20.12))') 'k1,k2,n1=',k1,k2,n1,' Fv1(k1)=',Fv1(k1),' Fv2(k1)=',Fv2(k1), &
         !                                ' dpa=',dpa,' dpi(k1)=',dpi(k1),' dpi(k2)=',dpi(k2)
         !end if
     end do
  end do
end do
          !  write(66,'(2i3,a,20(es10.3,1x))') kqt,kableitnum,' SumEval: Uxa_diag: ',(Uxa(j,j),j=1,nparts)

if(.not.upropa_on .and. .not.iteration_on .and. .not.MCsim_on)  then    !  .or. modvar_on)) then
  write(66,*) 'SE:   summands:  xvect: ',sngl(xvect(1:nparts))
  write(66,*) 'SE:   xvect-covariance Matrix Ux:'
  do k1=1,nparts
    write(66,'(20es12.4)') (Uxa(k1,k2),k2=1,nparts)
  end do
  write(66,*)
end if

70   continue

if(modeSEval == 1) then
  ! mean of activity values, use the least squares mean (Cox et al.):
  nr = 1
  call LsqCoxGWM(xvect,Uxa,nparts,nr,nparts,gwx,gwcx,chisq,ableit,ok)

        IF(.not.upropa_on .and. .not.iteration_on .and. .not.MCsim_on) THEN
          WRITE(66,*) 'Least-sq. weighted mean: ',sngl(gwx(1)),  &
                       '   uncertainty   =',sngl(SQRT(gwcx(1,1))),'  ChisqR=',sngl(chisq/real(MAX(1,nparts-1),rn))
        end if
  yval = gwx(1)
  uyval = SQRT(gwcx(1,1))
end if

if(modeSEval == 2) then
  ! Sum of activity values:
  yval = sum(xvect(1:nparts))
  if(.not.iteration_on .and. .not.upropa_on .and. .not. MCsim_on) then
    faliq(1:nparts) = xvect(1:nparts) / sum(xvect(1:nparts))
  end if
  if(.not.MCsim_on .or. modvar_on) then
    uyval = zero
    do k1=1,nparts
      do k2=1,nparts
        uyval = uyval + Uxa(k1,k2)
      end do
    end do
    uyval = sqrt(uyval)
  end if
  if(.not.upropa_on .and. (.not. MCsim_on .or. modvar_on) ) then
     write(66,*) 'sum of summands added:  yval=',sngl(yval),'   uyval=',sngl(uyval)
  end if

   if(.false. .and. MCsim_on .and. imc <= 10) then
     write(63,*) 'summands:  xvect: ',sngl(xvect(1:nparts))
     write(63,*) 'xvect-covariance Matrix Ux:'
     do k1=1,nparts
       write(63,'(20es12.4)') (Uxa(k1,k2),k2=1,nparts)
     end do
     write(63,*)
   end if

end if

!ifehl = 1

end subroutine SumEvalCalc
