module FCVX

contains

subroutine FindCovx(kroot,munit,ifehl)

   ! prepares the covariance matrix covx by uncertainty propagation from covpp:
   !    covx = QsumxTest = DPmat * covpp * transpose(DPmat)
   ! where DPmat is the matrix of partial derivatives:
   !    d(afunc(jp)) / d(Messwert(mwind);  with afunc(jp) obtained by call funcs(indeval,...)
   ! and covpp is the covariance matrix of the "mpfx parameters"
   !
   ! Called only by subroutine GlsqUR2 (kroot = 1)

   !   Copyright (C) 2014-2023  Günter Kanisch

USE UR_Derivats
USE UR_Linft,     only: ifit,mpfx,covpp,covx,test_cauchy, &
                        numd,nhp,nchannels,nccg,mfit,ma,cofact,cauchy_failed2,DPmat,kEQnums
USE UR_Gleich_globals,    ONLY: Messwert,StdUnc,RS_SymbolNr,nRSsy,nab,kableitnum
USE ur_general_globals, ONLY: MCSim_on
USE UR_DLIM,      ONLY: iteration_on,limit_typ
use Top,          only: dpafact
use Num1,         only: funcs ,dpi_funcs,matwrite,findEq_afunc
use UR_params,    only: rn,ZERO,ONE,EPS1MIN,TWO
use UR_MCC,       only: imc

implicit none

integer(4),intent(in)    :: kroot          ! Ident of the calling program: 1: GLSQUR2
integer(4),intent(in)    :: munit          ! Unit-No. for file output
integer(4),intent(out)   :: ifehl          ! error indicator

integer(4)        :: mav,k,i,j,mm1,ik1,messk,nwh,km1,m_anz,n_anz,kqt,ng1,ng2
integer(4)        :: kEQnumber(3),jj     ! ,mac
real(rn)          :: bfunc(3),covvor,diffcorr,Fv1

real(rn),allocatable :: Qsumxtest(:,:),xterm(:)
real(rn)          :: xtiny,vprod
logical           :: printout,DPmat_exists
character(len=1)  :: cmessk(3)

!-----------------------------------------------------------------------------------------

allocate(Qsumxtest(numd*3,numd*3),xterm(numd*3))
Qsumxtest = ZERO

       mav = mfit
       if(kroot == 1) mav = ma
ifehl = 0
cmessk = (/'A','B','C' /)

printout = .false.
nwh = numd/nchannels
kqt = 1
if(iteration_on .and. limit_typ == 1) kqt = 2
if(iteration_on .and. limit_typ == 2) kqt = 3

if(.not.allocated(DPmat)) allocate(DPmat(numd*3,nhp))
DPmat_exists = .true.
if((kableitnum == 0 .and. .not.MCsim_on) .or.(kqt == 1 .and. MCsim_on .and. imc == 1)) then
  DPmat = ZERO
  DPmat_exists = .false.
  if(.not. allocated(kEQnums)) then
    allocate(kEQnums(nchannels*nwh,ma))
    do i=1,nchannels*nwh
      call findEq_afunc(i,kEQnumber)    ! 6.7.2023
      kEQnums(i,1:3) = kEQnumber(1:3)   !
    end do
  end if
end if

nccg = 0
if(minval(covpp) < ZERO .or. maxval(covpp) > EPS1MIN) nccg = 1

xterm = ZERO

ik1 = 0
do messk=1,nchannels
  do mm1=1,nwh
    km1 = (messk-1)*nwh + mm1

    do k=1,ma
      if(kEQnums(km1,k) == nab) cycle
      if(kroot == 2 .and. k > mav) cycle
      ik1 = ik1 + 1
      if(ifit(k) >= 2) cycle

      if(k == 1) call Funcs(km1,bfunc)
      xterm(ik1) = bfunc(k)
         Fv1 = bfunc(k)
      do j=1,nhp
        if(StdUnc(mpfx(j)) <= ZERO) cycle
        if(DPmat_exists) then
          if(kableitnum > 0 .and. kableitnum /= mpfx(j)) cycle
          jj = 0
          if(kEQnums(km1,k) > nab) then
            if(nRSsy(kEQnums(km1,k)) > 0) then
              jj = findloc(RS_SymbolNR(kEQnums(km1,k),1:nRSsy(kEQnums(km1,k))),mpfx(j),dim=1)
            end if
          end if
        else
          jj = 1
        end if
        if(jj == 0) cycle

        if(Messwert(mpfx(j)) > EPS1MIN) then
          DPmat(ik1,j) = dpi_funcs(mpfx(j),km1,k,ma,bfunc(k))
        end if
      end do
    end do
  end do
end do

ng1 = size(DPmat,1)
ng2 = size(DPmat,2)
m_anz = nhp
n_anz = ng1
            !  call matwrite(DPmat,ng1,ng2,23,'(1x,130es11.3)','FindCovx: Matrix DPmat :')

QsumxTest = matmul(DPmat, matmul(covpp, Transpose(DPmat)))

ng1 = size(QsumxTest,1)

if(allocated(covx)) deallocate(covx)
allocate(covx(1:n_anz,1:n_anz))
do i=1,ik1         !n_anz
  do j=1,ik1
    covx(i,j) = QsumxTest(i,j)
    if(i /= j .and. cofact < ONE) covx(i,j) = covx(i,j) * cofact
  end do
end do

! Testing for the Cauchy-Schwarz inequality
if(test_cauchy .and. kroot == 1) then
  xtiny = 1.E-11_rn
   !  xtiny = 1.E-13_rn
  do i=1,ik1-1
    do j=i+1,ik1
      IF(abs(covx(i,j)) > EPS1MIN) then
        vprod = covx(j,j)*covx(i,i)
        if(abs(covx(i,j))**TWO > covx(j,j)*covx(i,i)*(ONE - xtiny))  then
          cauchy_failed2 = .true.
          covvor = covx(i,j)
          diffcorr = abs(covx(i,j))**TWO - covx(j,j)*covx(i,i)
          covx(i,j) = ( covx(i,j)/abs(covx(i,j)) ) *  sqrt(covx(j,j)*covx(i,i)*(ONE-xtiny))
          covx(j,i) = covx(i,j)

          !WRITE(23,*) 'Findcovx: kroot=',kroot,'  Cauchy-Schwarz-Inequality invalid for covx, for i,j= ',i,j,  &
          !            '  sqrt(VarProd) = ',sngl(sqrt(covx(j,j)*covx(i,i))),' cov vorher:',sngl(covvor), &
          !            ' cov nachher:',sngl(covx(i,j)),' diffcorvor=',sngl(diffcorr)
        end if
      end if
    end do
  end do
end if

   if(.false. .and. kqt ==1 .and. kroot == 1) then
     write(munit,*)
     WRITE(munit,*) 'Lsqlincov2: Matrix covpp (Test):    ik1=',ik1,'  m_anz=',m_anz,' n_anz=',n_anz
     do j=1,nhp
       WRITE(munit,'(1x,130es11.3)') (covpp(j,i),i=1,nhp)
     end do
     WRITE(munit,*)
     WRITE(munit,*) 'Lsqlincov2: DPMat (Test):'
     do i=1,ik1
       WRITE(munit,'(1x,130es11.3)') (DPmat(i,j),j=1,nhp)
     end do

     WRITE(munit,*) 'numd=',numd,'  nhp=',nhp,' ik1=',ik1,' ma=',ma
     write(munit,*) ' size(DPMat,1)=',size(DPMat,1), ' size(DPMat,2)=',size(DPMat,2), &
                 '  m=',m_anz,'  n=',n_anz,' cofact=',cofact
          WRITE(munit,*) 'Lsqlincov2: Matrix-Differenz:   covx - (DPmat x covpp x DPmat_T)   ' &
                        //'(Test):     Triples: Kanal, nwh, Nuklid'
          write(munit,'(1x,130(4x,i2,5x))')  (i, i=1,numd*mav)
          write(munit,'(1x,130(3x,A1,i2,1x,i1,2x,1x))') (( (cmessk(k), mm1, i,i=1,mav), mm1=1,nwh), k=1,nchannels)
          write(munit,*)

     WRITE(munit,*) 'Lsqlincov2: Matrix-Produkt:   DPmat x covpp x DPmat_T   (Test):'
     do j=1,numd*mav
       WRITE(munit,'(1x,130es11.3)') (QSUMxTest(j,i),i=1,numd*mav)
     end do
     WRITE(munit,*)
     WRITE(munit,'(1x,130es11.3)') (xterm(i),i=1,numd*mav)
     write(munit,*)
   end if

if(kroot == 1) then
  do i=1,ik1
    if(abs(covx(i,i)) < EPS1MIN) covx(i,i) = 1.E-14_rn
  end do
end if

end subroutine FindCovx

!###################################################################################


end module FCVX
