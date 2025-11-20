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
    !     Copyright (C) 2014-2025  Günter Kanisch

    use UR_params,    only: EPS1MIN,ZERO,TWO
    use UR_MCSR
    use UR_Linft,     only: mpfx,FitDecay
    use UR_Gleich_globals,    only: MesswertSV,StdUncSV,ncov,missingval,CovarVal,knumEGr,nab, &
                            nabf,nmodf,SymboleA,SymboleB,ISymbA,ISymbB
    use UR_MCC,       only: muvect,icn,nf1,nf2,j1,nf3, &
                            covxy,icnvec,icnmpfx
    use CHF,          only: ucase
    use Top,          only: RealModA1,realModA2,IntModA1,IntModA2
    use Num1,         only: Quick_sort2_i
    use file_io,      only: logger

    implicit none

    integer              :: i,i1,i2,k,j,icnx,nfound,jj
    CHARACTER(LEN=60)    :: chh1,chh2
    integer, allocatable :: icnsy(:),isrt(:)
    character(len=300)   :: log_str

    WRITE(log_str,*) CHAR(13),CHAR(10),'Preparing covariances for MC simulation:   ncov=',int(ncov,2)
    call logger(63, log_str)
    ncov1 = 0

    kgt = 0
    if(FitDecay .and. knumEGR > 1) kgt = 3
    nhg = nab+nmodf+nabf

    ! if(allocated(icnzg)) deallocate(icnzg,nf1,nf2,nf3,icovgrp,icovn,kcnzg,mrv)
    if(allocated(icnvec)) deallocate(icnvec,nf1,nf2,nf3,icnmpfx)
    if(allocated(muvect))  deallocate(muvect)
    if(allocated(covxy))  deallocate(covxy)
    if(allocated(covariter)) deallocate(covariter)

    if(allocated(kv1)) deallocate(kv1)
    if(allocated(kgl)) deallocate(kgl)

    ! ncov: given number of covariance pairs
    if(ncov == 0) return

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
    allocate(icnvec(icnx),icnmpfx(icnx))
    allocate(covxy(icnx,icnx))
    allocate(covariter(ncov))
    allocate(muvect(icnx))

    kv1 = 0
    kgl = 0
    covxy = ZERO
    covariter = .true.
    muvect = ZERO
    icn = 0
    nf1 = 0
    nf2 = 0
    nf3 = 0
    icnvec = 0
    icnmpfx = 0
    ncov1 = 0
    !-------------------------------------------------------------------------------
    do k=1,ncov
        if(ncov == 0) exit
        nhg = nhg + 1

        chh1 = ucase(SymboleA(k)%s)
        chh2 = ucase(SymboleB(k)%s)
        nfound = 0
        i1 = isymbA(k)
        i2 = IsymbB(k)

        if(FitDecay) then
            if(index(chh1,'FITP') > 0 .or. index(chh2,'FITP') > 0) cycle
        end if
        ncov1 = ncov1 + 1

        if(ncov1 == 1) then
          icn = 1
          icnvec(icn) = i1
          nf1(ncov1) = icn
          muvect(icn) = MesswertSV(i1)
          covxy(icn,icn) = StdUncSV(i1)**two
          if(abs(covxy(icn,icn)) < eps1min) covxy(icn,icn) = 1.E-14_rn
          if(Fitdecay) then
            j = findLoc(mpfx,i1,dim=1)
            if(j > 0) then
              icnmpfx(icn) = j
              nfound = 1
            end if
          end if
          icn = icn + 1
          icnvec(icn) = i2
          nf2(ncov1) = icn
          muvect(icn) = MesswertSV(i2)
          covxy(icn,icn) = StdUncSV(i2)**two
          if(abs(covxy(icn,icn)) < eps1min) covxy(icn,icn) = 1.E-14_rn
          if(Fitdecay) then
            j = findLoc(mpfx,i2,dim=1)
            if(j > 0) then
              icnmpfx(icn) = j
              nfound = 1
            end if
          end if
        else
          jj = findloc(icnvec,i1,dim=1)
          if(jj == 0) then
            icn = icn + 1
            icnvec(icn) = i1
            nf1(ncov1) = icn
            muvect(icn) = MesswertSV(i1)
            covxy(icn,icn) = StdUncSV(i1)**two
            if(abs(covxy(icn,icn)) < eps1min) covxy(icn,icn) = 1.E-14_rn
            if(Fitdecay) then
              j = findLoc(mpfx,i1,dim=1)
              if(j > 0) then
                icnmpfx(icn) = j
                nfound = 1
              end if
            end if
          else
            nf1(ncov1) = jj
          end if
          jj = findloc(icnvec,i2,dim=1)
          if(jj == 0) then
            icn = icn + 1
            icnvec(icn) = i2
            nf2(ncov1) = icn
            muvect(icn) = MesswertSV(i2)
            covxy(icn,icn) = StdUncSV(i2)**two
            if(abs(covxy(icn,icn)) < eps1min) covxy(icn,icn) = 1.E-14_rn
            if(Fitdecay) then
              j = findLoc(mpfx,i2,dim=1)
              if(j > 0) then
                icnmpfx(icn) = j
                nfound = 1
              end if
            end if
          else
            nf2(ncov1) = jj
          end if
        end if

        kv1(ncov1) = k
        kgl(ncov1) = nhg-kgt       ! Number of the original covariance equation
        if(ncov1 == 1) nhgfx = nhg
        ! nfound==1: a mpfx-parameter is involved, then the covar-values must not be re-calculated (varied)
        if(nfound == 1) covariter(ncov1) = .false.

        IF(abs(covarval(k)-missingval) > EPS1MIN) THEN
            if(nf1(ncov1) > 0 .and. nf2(ncov1) > 0) then
                covxy(nf1(ncov1),nf2(ncov1)) = covarval(k)
                covxy(nf2(ncov1),nf1(ncov1)) = covarval(k)
            end if
            nf3(ncov1) = k
        end if

    end do      ! k=1,ncov

    !----------------------------------------------------------------
    write(log_str,'(2(a,i3))') 'ncov1 = ',ncov1,'  icn=',icn
    call logger(63, log_str)
    ! write(log_str,*) 'covxy : ',(sngl(covxy(i,i)),i=1,ncov1)
    ! call logger(63, log_str)
    !do i=1,icn
    !  write(log_str,*) 'covxy : ',(sngl(covxy(i,j)),j=1,icn)
    !  call logger(63, log_str)
    !end do
    write(log_str,'(a,50i4)') 'nf1   : ',(nf1(i),i=1,ncov1)
    call logger(63, log_str)
    write(log_str,'(a,50i4)') 'nf2   : ',(nf2(i),i=1,ncov1)
    call logger(63, log_str)
    write(log_str,'(a,50i4)') 'icnvec(nf1) : ',(icnvec(nf1(i)),i=1,ncov1)
    call logger(63, log_str)
    write(log_str,'(a,50i4)') 'icnvec(nf2) : ',(icnvec(nf2(i)),i=1,ncov1)
    call logger(63, log_str)
    write(log_str,'(a,50es11.3)') 'MW(icnvec(1:icn))=',MesswertSV(icnvec(1:icn))
    call logger(63, log_str)

    if(allocated(muvect0)) deallocate(muvect0)
    allocate(muvect0(size(muvect)))

    WRITE(log_str,*) '  MCCALC:  Kovarianz-Matrix covxy: '
    call logger(63, log_str)
    do i=1,icn
        WRITE(log_str,'(20es11.3)') (real(covxy(i,j1),8),j1=1,icn)
        call logger(63, log_str)
        muvect0(i) = muvect(i)
    end do


    ! Example output:
    ! Preparing covariances for MC simulation:   ncov=     9
    ! ---------------------------------------------------
    !ncov1 =   9  icn=  9
    !nf1    :    1   1   2   4   4   5   7   7   8
    !nf2    :    2   3   3   5   6   6   8   9   9
    !icnvec(nf1) :   13  13  14  16  16  17  19  19  20
    !icnvec(nf2) :   14  15  15  17  18  18  20  21  21
    !MW(icnvec(1:icn))=  8.979E-01  1.658E-03  0.000E+00  5.293E-01  3.751E-01  3.650E-04  3.371E-01  4.764E-01  8.570E-02
    !   MCCALC:  Kovarianz-Matrix covxy:
    !  2.683E-05  2.652E-08  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
    !  2.652E-08  2.308E-08  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
    !  0.000E+00  0.000E+00  1.000E-14  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
    !  0.000E+00  0.000E+00  0.000E+00  5.644E-05  3.459E-05  3.366E-08  0.000E+00  0.000E+00  0.000E+00
    !  0.000E+00  0.000E+00  0.000E+00  3.459E-05  2.987E-05  2.385E-08  0.000E+00  0.000E+00  0.000E+00
    !  0.000E+00  0.000E+00  0.000E+00  3.366E-08  2.385E-08  5.092E-09  0.000E+00  0.000E+00  0.000E+00
    !  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  6.707E-06  2.860E-06  5.145E-07
    !  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  2.860E-06  1.066E-05  7.271E-07
    !  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  5.145E-07  7.271E-07  1.321E-06
    !


end subroutine MCPrepCovars

