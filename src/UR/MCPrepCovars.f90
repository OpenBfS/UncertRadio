
subroutine MCPrepCovars

! Preparations for the treatment of covariances between different random variables:
!
! The variables (symbols) involved in covariance pairs cov(SymboleA, SymboleB) are
! attributed to different groups (ncgrp groups).
! A group contains those variables which do not have covariances with any other variable
! outside this group.
! mpfx-parameters: these are the parameters given as argumnents of the UR2 function call "Linfit()".
!
! see the example output at the end of this file for the meaning of groups!
!
!     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,    only: rn,eps1min,zero,two
use UR_MCSR
use UR_Linft,     only: mpfx,FitDecay
use UR_Gspk1Fit,  only: Gamspk1_Fit
use UR_Gleich,    only: MesswertSV,StdUncSV,ncov,missingval,CovarVal,knumEGr,nab,kEGr,ngrs, &
                        nabf,nmodf,SymboleA,SymboleB,SymboleG,ISymbA,ISymbB
use UR_MCC,       only: muvect,icn,icnzg,nf1,nf2,j1,nf3,icovgrp,icovn, &
                        ncgrp,nj1,nj2,nc1,kss1,covxy
use CHF,          only: ucase
use Top,          only: RealModA1,realModA2,IntModA1,IntModA2
use Num1,         only: Quick_sort2_i

implicit none

integer(4)          :: i,i1,i2,k,kk,j,isy,icnx,nfound
CHARACTER(LEN=60)   :: chh1,chh2
character(len=100)  :: cform
integer(4),allocatable  :: icnsy(:),isrt(:)

  WRITE(63,*) CHAR(13),CHAR(10),'Preparing covariances for MC simulation:   ncov=',int(ncov,2)
ncov1 = 0

  kgt = 0
  if(FitDecay .and. knumEGR > 1) kgt = 3
  nhg = nab+nmodf+nabf

  if(allocated(icnzg)) deallocate(icnzg,nf1,nf2,nf3,icovgrp,icovn)
  if(allocated(muvect))  deallocate(muvect)
  if(allocated(covxy))  deallocate(covxy)
  if(allocated(covariter)) deallocate(covariter)

  if(allocated(kv1)) deallocate(kv1)
  if(allocated(kgl)) deallocate(kgl)
  if(allocated(kss1)) deallocate(kss1)
  allocate(kss1(ngrs))
  kss1 = 0

ncgrp = 0     ! number of groups

! ncov: given number of covariance pairs
IF(ncov > 0) THEN

  ! icn: number of correlating variables
  ! icnx: number of different variables in covar-pairs:
  icnx = 0
  allocate(icnsy(2*ncov),isrt(2*ncov))
  icnsy(1:2*ncov) = [ IsymbA(1:ncov), IsymbB(1:ncov) ]
  call Quick_sort2_i(icnsy,isrt)         ! sort index
  icnx = 1
  do i=2,2*ncov
    if(icnsy(isrt(i)) /= icnsy(isrt(i-1))) icnx = icnx + 1
  end do

    allocate(kv1(ncov),kgl(ncov),nf1(ncov),nf2(ncov),nf3(ncov))
    allocate(icovn(ncov),icnzg(icnx))
    allocate(icovgrp(icnx,icnx))
    allocate(covxy(icnx,icnx))
    allocate(covariter(ncov))
    allocate(muvect(icnx))
    kv1 = 0
    kgl = 0
    covxy = zero
    covariter = .true.
    icovgrp = 0
    icovn = 0
    muvect = zero
    icn = 0
    nf1 = 0
    nf2 = 0
    nf3 = 0
    icnzg = 0

  do k=1,ncov
    if(ncov == 0) exit
    nhg = nhg + 1

    chh1 = ucase(SymboleA(k)%s)
    chh2 = ucase(SymboleB(k)%s)
    nfound = 0

    if(FitDecay) then
      if(index(chh1,'FITP') > 0 .or. index(chh2,'FITP') > 0) then
        cycle
      end if

      ! covariances between mpfx-parameters in the FitDecay case:
      j = Findloc(mpfx,IsymbA(k),dim=1)
      if(j > 0) then
        nfound = 1
        nj1 = j
        kss1(mpfx(j)) = j
      end if
      j = Findloc(mpfx,IsymbB(k),dim=1)
      if(j > 0) then
        nfound = 1
        nj2 = j
        kss1(mpfx(j)) = j
      end if

      if(nfound == 1) then
        if(ncgrp == 0) then
          ncgrp = 1
          if(nj1 > 0) then
            icovn(ncgrp) = icovn(ncgrp) + 1
            icovgrp(ncgrp,icovn(ncgrp)) = nj1
          end if
          if(nj2 > 0) then
            icovn(ncgrp) = icovn(ncgrp) + 1
            icovgrp(ncgrp,icovn(ncgrp)) = nj2
          end if
            ! Write(63,*) 'A: ncgrp=',ncgrp,'  nj1,nj2=',nj1,nj2,'  icovn:', icovn(ncgrp)
        else
          do nc1=1,ncgrp
            do i=1,icovn(nc1)
              if(icovgrp(nc1,i) == nj1) then
                nj1 = -nj1
                i1 = nc1
              end if
              if(icovgrp(nc1,i) == nj2) then
                nj2 = -nj2
                i2 = nc1
              end if
            end do
          end do
            ! Write(63,*) 'B: ncgrp=',ncgrp,'  nj1,nj2=',nj1,nj2,'  icovn:', icovn(ncgrp)
          if(nj1 < 0 .and. nj2 > 0) then
            icovn(i1) = icovn(i1) + 1
            icovgrp(i1,icovn(i1)) = nj2
               ! Write(63,*) ' goto 33: ncgrp=',ncgrp,'   i1,i2=',i1,i2,'  nj1,nj2=',nj1,nj2
            goto 33
          end if
          if(nj2 < 0 .and. nj1 > 0) then
            icovn(i2) = icovn(i2) + 1
            icovgrp(i2,icovn(i2)) = nj1
            goto 33
          end if
          if(nj1 > 0 .and. nj2 > 0) then
            ncgrp = ncgrp + 1
            icovn(ncgrp) = icovn(ncgrp) + 1
            icovgrp(ncgrp,icovn(ncgrp)) = nj1
            icovn(ncgrp) = icovn(ncgrp) + 1
            icovgrp(ncgrp,icovn(ncgrp)) = nj2
          end if
        end if
      end if
    end if           ! FitDecay

33        continue

       ncov1 = ncov1 + 1
       kv1(ncov1) = k
       kgl(ncov1) = nhg-kgt       ! Number of the original covariance equation
       if(ncov1 == 1) nhgfx = nhg
    ! nfound==1: a mpfx-parameter is involved, then the covar-values must not be re-calculated (varied)
    if(nfound == 1) covariter(ncov1) = .false.

    IF(.NOT.Gamspk1_Fit) THEN
      i1 = IsymbA(k)
      i2 = IsymbB(k)
    else
      i1 = IsymbA(k)             ! i1 > 1
      i2 = IsymbB(k)             ! i2 > 1
    end if

    ! For a covar-pair, icnzg() points to the values of i1 and i2 in the Symbols-Grid
    nf1(ncov1) = 0
    do j1=1,icn
      IF(i1 == icnzg(j1)) nf1(ncov1) = j1
    end do
    IF(nf1(ncov1) == 0) THEN
      icn = icn + 1
      icnzg(icn) = i1
      nf1(ncov1) = icn
      covxy(icn,icn) = StdUncSV(i1)**two
      muvect(icn) = MesswertSV(i1)
    end if

    nf2(ncov1) = 0
    do j1=1,icn
      IF(i2 == icnzg(j1)) then
        nf2(ncov1) = j1
      END IF
    end do
    IF(nf2(ncov1) == 0) THEN
      icn = icn + 1
      icnzg(icn) = i2
      nf2(ncov1) = icn
      covxy(icn,icn) = StdUncSV(i2)**two
      muvect(icn) = MesswertSV(i2)
    end if
    IF(nf1(k) == nf2(k) .and. nf1(k) /= 0) WRITE(63,*) 'k=',k,' : nf1 = nf2 = ',nf1(k),'  icn=',icn

    IF(abs(covarval(k)-missingval) > eps1min) THEN
      covxy(nf1(ncov1),nf2(ncov1)) = covarval(k)
      covxy(nf2(ncov1),nf1(ncov1)) = covarval(k)
      nf3(ncov1) = k
    end if

  end do      ! k=1,ncov
  !----------------------------------------------------------------
  write(63,'(2(a,i3))') 'ncov1 = ',ncov1,'  icn=',icn
  ! write(63,*) 'covxy : ',(sngl(covxy(i,i)),i=1,ncov1)
  !do i=1,icn
  !  write(63,*) 'covxy : ',(sngl(covxy(i,j)),j=1,icn)
  !end do
  write(63,'(a,50i4)') 'nf1   : ',(nf1(i),i=1,ncov1)
  write(63,'(a,50i4)') 'nf2   : ',(nf2(i),i=1,ncov1)
  write(63,'(a,50i4)') 'icnzg(nf1) : ',(icnzg(nf1(i)),i=1,ncov1)
  write(63,'(a,50i4)') 'icnzg(nf2) : ',(icnzg(nf2(i)),i=1,ncov1)

  if(allocated(muvect0)) deallocate(muvect0)
  allocate(muvect0(size(muvect)))

  WRITE(63,*) '  MCCALC:  Kovarianz-Matrix covxy: '
  do i=1,icn
    WRITE(63,'(20es11.3)') (real(covxy(i,j1),8),j1=1,icn)
    muvect0(i) = muvect(i)
  end do

  if(.not.FitDecay .and. ncgrp == 0 .and. ncov > 0) then
    do k=1,ncov
      if(ncgrp == 0) then
        ncgrp = ncgrp + 1
        icovn(ncgrp) = icovn(ncgrp) + 1
        icovgrp(ncgrp,icovn(ncgrp)) = icovn(ncgrp)
        icovn(ncgrp) = icovn(ncgrp) + 1
        icovgrp(ncgrp,icovn(ncgrp)) = icovn(ncgrp)
      end if

      if(icovn(ncgrp) > 0) then
        nfound = 0
        do kk=1,2
          nfound = 0
          if(kk == 1) isy = IsymbA(k)
          if(kk == 2) isy = IsymbB(k)
          do j=1,icovn(ncgrp)
            if(icnzg(icovgrp(ncgrp,j)) == isy) then
              nfound = 1
              exit
            end if
          end do
          if(nfound == 0) then
            icovn(ncgrp) = icovn(ncgrp) + 1
            icovgrp(ncgrp,icovn(ncgrp)) = icovn(ncgrp)
            icnzg(icovgrp(ncgrp,icovn(ncgrp))) = isy
          end if
        end do
      end if
    end do
    write(63,*) ' icovn(ncgrp)=',icovn(ncgrp),'  icn=',icn
  end if

     ! write(63,*) ' B: ncgrp=',ncgrp
  k = 0
  if(FitDecay .and. kEgr > 1) k = knumEGr
  do nc1=1,ncgrp
    do i=1,icovn(nc1)
      k = k + 1
      if(k > ncov1) goto 17
      Messwertkq(ngrs+kv1(k)) = covarval(kv1(k))
      Messwert_eg(ngrs+kv1(k)) = covarval(kv1(k))
      do j=1,icn
        if(ubound(mpfx,dim=1) == 0) exit
        if(icnzg(j) == mpfx(icovgrp(nc1,i))) then
          icovgrp(nc1,i) = j
          exit
        end if
      end do
    end do
  end do
17      continue

  ! icovn(group i)             : number of different symbols/variables, which are involved in covariances
  !                              within the group i;
  ! icovgrp(group i, j)        : number (beginning at 1) of symbols involved in the group i; j=1,...,icovn(i);
  ! icnzg(icovgrp(group i, j)  : index of the j-th symbol of the group i i in the list of symbols ;
  ! n(n-1)/2                   : number of Covar-pairs, belonging to the number n=icovn(i) of symbols

  write(63,*)
  write(63,'(a,i2)') 'Groups: ',int(ncgrp,2)
  do i=1,ncgrp
    write(cform,'(a,i2,a,i2,a)') '(a,i2,a,i0,a,', icovn(i), 'i4,a,',icovn(i),'i4)'
    write(63,cform) '   Group ',i,';  icovgrp(',i,',:)= ', (icovgrp(i,j),j=1,icovn(i) ),  &
                            '    icnzg: ',(icnzg(icovgrp(i,j)),j=1,icovn(i))
  end do
  do i=1,ncgrp
    write(63,'(a,i2,a,10(a,a))') '   Group ',int(i,2),';  ', (SymboleG(icnzg(icovgrp(i,j)))%s,'  ',j=1,icovn(i) )
  end do
  write(63,'(a,50i4)') 'kv1(1:ncov1): ',(kv1(i),i=1,ncov1)
  write(63,'(a,i3,a,i3)') 'ncov=',ncov,' ncov1=',ncov1
  write(63,*)
end if

! Example output:
! Preparing covariances for MC simulation:   ncov=     15
! ncov1 =  12  icn= 12
! nf1   :    1   1   2   4   4   5   7   7   8  10  10  11        ! index numbers in the list of symbols involved in covariances
! nf2   :    2   3   3   5   6   6   8   9   9  11  12  12        !
! icnzg(nf1) :   25  25  26  22  22  23  28  28  29  19  19  20   ! their indexes in the primary list of symbols
! icnzg(nf2) :   26  27  27  23  24  24  29  30  30  20  21  21   !
!    MCCALC:  Kovarianz-Matrix covxy:
!   7.714E-08  2.307E-06  1.617E-06  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
!   2.307E-06  7.448E-05  5.207E-05  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
!   1.617E-06  5.207E-05  3.661E-05  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
!   0.000E+00  0.000E+00  0.000E+00  1.154E-06  1.587E-05  5.395E-08  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
!   0.000E+00  0.000E+00  0.000E+00  1.587E-05  2.227E-04  7.561E-07  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
!   0.000E+00  0.000E+00  0.000E+00  5.395E-08  7.561E-07  3.628E-09  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
!   0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  3.143E-08  1.055E-06  1.572E-06  0.000E+00  0.000E+00  0.000E+00
!   0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.055E-06  3.996E-05  5.935E-05  0.000E+00  0.000E+00  0.000E+00
!   0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.572E-06  5.935E-05  8.864E-05  0.000E+00  0.000E+00  0.000E+00
!   0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.720E-05  2.930E-06  2.550E-08
!   0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  2.930E-06  6.175E-07  4.493E-09
!   0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  2.550E-08  4.493E-09  9.197E-10
!
! Groups:  4
!    Group  1;  icovgrp(1,*)=    1   2   3    icnzg:   25  26  27
!    Group  2;  icovgrp(2,*)=    4   5   6    icnzg:   22  23  24
!    Group  3;  icovgrp(3,*)=    7   8   9    icnzg:   28  29  30
!    Group  4;  icovgrp(4,*)=   10  11  12    icnzg:   19  20  21
!    Group  1;  ESR89A  ESR89B  ESR89C
!    Group  2;  ESR90A  ESR90B  ESR90C
!    Group  3;  EY90A  EY90B  EY90C
!    Group  4;  ESR85A  ESR85B  ESR85C
! kv1(1:ncov1):    4   5   6   7   8   9  10  11  12  13  14  15
! ncov= 15 ncov1= 12


end subroutine MCPrepCovars

