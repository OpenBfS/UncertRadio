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

module DECH
    use UR_types, only: rn
!-----------------------------------------------------------------------------------------
!  Package for the handling radioactive decay chains with respect to activity calculations
!  according to the Bateman equations. The task of Bateman related calculations is done
!  by matrix-based routines following the work by
!
!            E. Levy, 2019: DECAY CHAIN DIFFERENTIAL EQUATIONS: SOLUTIONS THROUGH
!            MATRIX ANALYSIS. Computer Physics Communications 234, 2019, 188-194
!
!  We differentiate between two modi of such calculations: "forward" and "backward"
!  calculations:
!         forward:  Given the activities of a decay chain at time t=0, the method by Levi,
!                   using an F-Matrix, calculates their activities after an elapsed time
!                   t=tA.
!         backward: Given the activities of a decay chain at time t=tA, the activities
!                   at time t=0 are calculated by two methods, a least-squares method
!                   and a recursive method.
!
!  The details for both modi of calculations are outlined in:
!                 Kanisch, G., et al., (2025): Zeitverhalten bei mehrgliedrigen
!                 Zerfallsreihen........ (in preparation)
!
!  The F-Matrix is used in both calculation moodi. The recursion method mentioned works
!  with the elements of the matrix F.
!  The backward calculations can be identified with what is called "decay corrections".
!
!     Routines involved:
!     LoadDecayChains
!     DCPrepareTable
!     DCReadGrid
!     DecCh
!     Decaysub1
!     fdf
!     matrix_build
!     backdecay
!
!              Copyright (C) 2024-2025  Günter Kanisch
!
!-----------------------------------------------------------------------------------------


contains

    !##################################################################################

    subroutine LoadDecayChains
        use UR_DecChain,   only: DCList,nDCL
        use Rout,          only: WDListstoreFill_1
        use ur_general_globals, only: work_path
        use CHF, only: flfu

        implicit none

        integer             :: i, ios, nio
        character(len=150)  :: texth

        ! List and structure of available decay series records:
        ! Sr-90-2N : Sr-90 # Y-90 : z12=1
        ! Zr-95-3N : Zr-95 # Nb-95m # Nb-95 : z12=0.0108# z13=0.9892# z23=0.944
        ! Pb-210-3N : Pb-210 # Bi-210 # Po-210 : z12=1# z13=1# z23=1
        ! Pb-210-2N : Pb-210 # Po-210 : z12=1

        ! List of avaiabale decay chains:   16.12.2024
        if(.not.allocated(DCList)) allocate(DCList(10))
        open(newunit=nio, file=flfu(work_path) // 'List_DecaySeries.txt', status='old')
        read(nio,*)
        do i=1, 10
            read(nio,'(a)', iostat=ios) texth
            if(ios /= 0 .or. len_trim(texth) == 0) then
                exit
            end if
            nDCL = i
            DCList(i)%s = trim(texth)
        enddo
        close (nio)

        call WDListstoreFill_1('liststore_Dchains', nDCL, DClist)

    end subroutine LoadDecayChains

    !##################################################################################

    subroutine DCPrepareTable(numChain)

    !------------------------------------------------------------------------------
    ! Purpose: Introducing a radioactive decay chain and associated properties
    !
    !          This routine refers to introducing a radionuclide decay chain into a
    !          table of a corresponding GUI set up dialog for defining variable names
    !          (symbols) for the nuclide's half-lives (T12) or decay  constants
    !          (Lambda), for branching ratios (Zji) and for the measurement
    !          conditions as described by detection efficiencies, chemical yields and
    !          other parameters.
    !
    !          For the case of applying a LSC counter with more than one energy window,
    !          up to three efficiency symbols (indicated by A, B, C attached) can be
    !          defined per radionuclide.
    !
    !          The variable names (symbols), defined in the dialog, enter into UncertRadio's
    !          common list of symbols, where values and uncertainties are attributed
    !          to them by the user.
    !
    !          The corresponding UncertRadio dialog is invoked by the appearance of the
    !          UncertRadio-specific function "SDECAY" on the right-hand side of an equation.
    !
    !          Introduced by Günter Kanisch in January 2025.
    !              Copyright (C) 2024-2025  Günter Kanisch
    !------------------------------------------------------------------------------

        use UR_DecChain
        use UR_Gleich_globals,   only: charv
        use CHF,                 only: ucase
        use Rout,                only: WTreeViewPutStrArray,WTreeViewPutDoubleArray, &
                                    WTreeViewPutIntArray,WTreeViewPutCheckArray, &
                                    WDPutEntryString,WDPutTreeViewColumnLabel, &
                                    WDGetCheckButton,WDGetEntryInt,WDGetComboboxAct, &
                                    WDListstoreFill_1,WDSetComboboxAct
        implicit none

        integer(4),intent(in)  :: numChain    ! is the index of a chain within a list of
                                                ! pre-defined decay chains

        integer(4)           :: i,i1,i2,i3,nc,i1_minus,iv,ni,L,ncc,nst,i1_m
        type(charv)          :: cnuk
        character(len=150)   :: text
        character(len=30)    :: Zjistr

        call WDGetComboboxAct('comboboxtextDCNCH',DCchannels)
        call WDGetCheckButton('DCcheckCommMeasmt',i)
        DCcommMeasmt = .false.
        if(i == 1) DCcommMeasmt = .true.
        call WDGetEntryInt('entryDCZBuildupNuk',DCBuildupAtSepar)
        if(allocated(DCindx)) deallocate(DCindx)

        if(.not.DChain_read_data) then
        if(allocated(DCnuclide)) then
            deallocate(DCnuclide,DCnuclide_sh,DCsymbT12,DCsymbLambda,DCsymbEffiA,DCsymbEffiB)
            deallocate(DCsymbEffiC,DCsymbYield,DCZji,DCZjivAL)
        end if
        endif

        call WDListstoreFill_1('liststore_Dchains', nDCL, DClist)
        call WDSetComboboxAct('ComboboxDCchains',numChain)        ! 30.4.2025


        text = DCList(numChain)%s
                    write(66,*) 'DC_chains(numChain)=text=',trim(text)

        ! ncc        : number of nuclides within the decay chain (Sr-90, Y-90 in case of a Sr analysis)
        ! N_nuclides : all nuclides taken from the grid (Sr-90, Y-90, Sr-89, Sr-85 in case of a Sr analysis)
        !              In the example indicated, for Sr-89 and Sr-85, the F-matrix is extended, only
        !              their decay factors are used as diagonal elements of F (non-diagonal elements of F
        !              are all equal to zero)

        nc = 0
        nst = 1
        i1 = index(text,':')
        chaincode%s = trim(adjustL(text(1:i1-1)))
        L = len_trim(chaincode%s)
        read(chaincode%s(L-1:L-1),*) ncc  ! number of nuclides within the decay chain
        text = text(i1+1:)
        write(66,*) 'chaincode%s=',chaincode%s, ' ncc=',int(ncc,2),' text=',trim(text)
        write(66,*) 'ubound(DCNuclide)=',ubound(DCNuclide,dim=1)

        if(.not.DChain_read_data) then
        allocate(DCnuclide(ncc + 5),DCsymbT12(ncc + 5),DCsymbLambda(ncc + 5))
        allocate(DCsymbEffiA(ncc + 5),DCsymbEffiB(ncc + 5))
        allocate(DCsymbEffiC(ncc + 5),DCsymbYield(ncc + 5))
        allocate(DCZji(15),DCZjivAL(15),DCindx(ncc+5))

        allocate(DCnuclide_sh(ncc))      ! does not appear in the grid
        else
        if(.not.allocated(DCindx)) then
            if(N_nuclides <= ncc) allocate(DCindx(ncc+5))
            if(N_nuclides > ncc) allocate(DCindx(N_nuclides+5))
        end if
        if(.not.allocated(DCZji)) allocate(DCZji(15))
        if(.not.allocated(DCZjival)) allocate(DCZjival(15))
        end if

        nst = 2
        do
        if(nst == 3) exit
        if(nst == 2) then
            nc = nc + 1
            if(nc < ncc) then
            i1 = index(text,'#')
            else
            i1 = index(text,':')
            endif
            if(i1 > 0) then
            DCnuclide(nc)%s = trim(adjustL(text(1:i1-1)))
            text = text(i1+1:)
            DCindx(nc) = nc
                write(66,*) 'nc=',nc,' text=',trim(text),'     DCnuclide=',DCnuclide(nc)%s
            if(nc == ncc) then
                nst = 3
            end if
            endif
        endif
        end do
        DCListZ(numChain) = trim(text)
        if(DChain_read_data .and. N_nuclides >= nc .and. nc > 0) then
        nc = N_nuclides
        else
        N_nuclides = nc
        end if

            ! write(0,*) 'first loop finished: DCListZ=',DCListZ(numChain)
                write(66,*) 'PrepTable:  nc=',int(nc,2),' N_nuclides=',int(N_nuclides,2)
                write(66,*) 'PrepTable: DCnuclide=',(DCnuclide(i)%s,i=1,4)

        do i=nc+1,nc+5
        DCindx(i) = i
        end do
        do ni=1,nc+5
        if(ni <= nc) then
            DCindx(ni) = ni
            if(DChain_read_data) cycle

            i1_minus = index(DCnuclide(ni)%s,'-')
            cnuk%s = DCnuclide(ni)%s(1:i1_minus-1) // DCnuclide(ni)%s(i1_minus+1:)
            DCnuclide_sh(ni)%s = cnuk%s      ! 12.1.2025  GK
            if(i1_minus > 0) then
            ! pre-define the names of variables (UR symbols) as arrays for nuclides
            DCsymbT12(ni)%s = 't' // cnuk%s
            DCsymbLambda(ni)%s = 'lam' // cnuk%s
            ! DCisChain(ni) = .true.
            DCsymbEffiA(ni)%s = 'eps'// cnuk%s
            DCsymbEffiB(ni)%s = ''
            DCsymbEffiC(ni)%s = ''
            if(DCchannels > 1) DCsymbEffiA(ni)%s = 'eps' // cnuk%s // 'A'
            if(DCchannels > 1) DCsymbEffiB(ni)%s = 'eps' // cnuk%s // 'B'
            if(DCchannels > 2) DCsymbEffiC(ni)%s = 'eps' // cnuk%s // 'C'
            DCsymbYield(ni)%s = 'eta' // DCnuclide(ni)%s(1:i1_minus-1)
            if(DCcommMeasmt .and. ni == DCBuildupAtSepar) then
                i1_m = index(DCnuclide(ni-1)%s,'-')
                DCsymbYield(ni)%s = 'eta' // DCnuclide(ni-1)%s(1:i1_m-1)
            end if
            end if
        else
            ! add the following for attached empty grid rows
            DCindx(ni) = ni
            DCnuclide(ni)%s = ''
            DCsymbT12(ni)%s = ''
            DCsymbLambda(ni)%s = ''
            DCsymbEffiA(ni)%s = ''
            DCsymbEffiB(ni)%s = ''
            DCsymbEffiC(ni)%s = ''
            DCsymbYield(ni)%s = ''
        endif
        end do
        N_nuclides = nc

        text = DCListZ(numChain)
        nzji = 0
        do
        if(len_trim(text) == 0) exit
        i2 = index(text,'=')
        i1 = index(ucase(text),'Z')
        i3 = index(text,'#')
        if(i1 > 0 .and. i2 > i1) then
            nzji = nzji + 1
            DCZji(nzji)%s = 'z' // trim(adjustL(text(i1+1:i2-1)))
            if(i3 > i2) then
            read(text(i2+1:i3-1),*) DCZjival(nzji)
            elseif (i3 == 0) then
            read(text(i2+1:),*) DCZjival(nzji)
            exit
            end if
            text = text(i3+1:)
        end if
        end do

        ! transfer the arrays of variable-names to the grid:
        call WDGetCheckButton('DCcheckVorLam',iv)
        if(iv == 1) then
        call WDPutTreeViewColumnLabel('treeview9', 3, 'Lambda'//char(13)//'Symbol')
        else
        call WDPutTreeViewColumnLabel('treeview9', 3, 'T12'//char(13)//'Symbol')
        endif

        write(66,*) 'DCnuclide:',(DCnuclide(i)%s,' ',i=1,3)
        write(66,*) 'DCsymbLambda:',(DCsymbLambda(i)%s,' ',i=1,3)
        write(66,*) 'DCsymbEffiA:',(DCsymbEffiA(i)%s,' ',i=1,3)

        call WTreeViewPutIntArray('treeview9',1,nc+4,DCindx)
        call WTreeViewPutStrArray('treeview9',2,nc+4,DCnuclide)
        if(iv == 0) call WTreeViewPutStrArray('treeview9',3,nc+4,DCsymbT12)
        if(iv == 1) call WTreeViewPutStrArray('treeview9',3,nc+4,DCsymbLambda)
        call WTreeViewPutStrArray('treeview9',4,nc+4,DCsymbEffiA)
        call WTreeViewPutStrArray('treeview9',5,nc+4,DCsymbEffiB)
        call WTreeViewPutStrArray('treeview9',6,nc+4,DCsymbEffiC)
        call WTreeViewPutStrArray('treeview9',7,nc+4,DCsymbYield)

        ZjiStr = ' '
        if(nzji > 0) then
        ZjiStr = DCZji(1)%s
        do i=2,nzji
            ZjiStr = trim(ZjiStr) // ' ' // DCZji(i)%s
        end do
        end if
        call WDPutEntryString('entryDCZji',trim(ZjiStr))


    end subroutine DCPrepareTable

    !##################################################################################

    subroutine DCReadGrid

    !------------------------------------------------------------------------------
    ! Purpose: After having re-edited by the user, the routine stores the data prepared
    !          in the setup-dialog for the radioactive decay chain and transfers the
    !          columns of its table back into string arrays.
    !
    !          Introduced by G�nter Kanisch in January 2025.
    !              Copyright (C) 2024-2025  G�nter Kanisch
    !------------------------------------------------------------------------------

        use UR_DecChain
        use UR_Gleich_globals,     only: charv
        use Rout,                 only: WDGetCheckButton,WDGetCheckButton,WDGetComboboxAct, &
                                        WDGetEntryString,WTreeViewGetStrArray,WDGetEntryInt

        implicit none

        integer(4)         :: i,iv,nc,i1
        character(len=50)  :: ZjiStr

        call WDGetCheckButton('DCcheckVorLam',iv)
        nc = N_nuclides
        call WDGetCheckButton('DCcheckSepar',i)
        apply_separation =.false.
        if(i == 1) apply_separation = .true.
        call WDGetComboboxAct('comboboxtextDCNCH',DCchannels)
        call WDGetCheckButton('DCcheckCommMeasmt',i)
        DCcommMeasmt = .false.
        if(i == 1) DCcommMeasmt = .true.
        call WDGetEntryInt('entryDCZBuildupNuk',DCBuildupAtSepar)

        call WTreeViewGetStrArray('treeview9',2,10,DCnuclide)
        do i=10,1,-1
        if(len_trim(DCnuclide(i)%s) > 0) then
            N_nuclides = i
            nc = i
            exit
        end if
        end do
        if(iv == 0) call WTreeViewGetStrArray('treeview9',3,nc,DCsymbT12)
        if(iv == 1) call WTreeViewGetStrArray('treeview9',3,nc,DCsymbLambda)
        call WTreeViewGetStrArray('treeview9',4,nc,DCsymbEffiA)
        if(DCchannels > 1) call WTreeViewGetStrArray('treeview9',5,nc,DCsymbEffiB)
        if(DCchannels > 2) call WTreeViewGetStrArray('treeview9',6,nc,DCsymbEffiC)
        call WTreeViewGetStrArray('treeview9',7,nc,DCsymbYield)
        call WDGetEntryString('entryDCZji',ZjiStr)
        nzji = 0
        if(len_trim(ZjiStr) > 0) then
        do
            i1 = index(ZjiStr,' ')
            if(i1 > 3) then
            nzji = nzji + 1
            DCZji(nzji)%s = adjustL(ZjiStr(1:i1-1))
            ZjiStr = ZjiStr(i1+1:)
            if(len_trim(ZjiStr) == 0) exit
            end if
        end do
        end if

    end subroutine DCReadGrid

    !##################################################################################

    subroutine DecCh

    !------------------------------------------------------------------------------
    ! Purpose: This routine performs the decay chain (forward) calculations. Instead
    !          of calculating numerical values, the solution is prepared as a set of
    !          symbolic equations, given as strings.
    !          These equation strings can be transferred to the text entry holding the
    !          model equations in the case of least squares fitting of a decay curve
    !          (UR variable FitDecay = true).
    !
    !          Introduced in UncertRadio by G�nter Kanisch in January 2025.
    !              Copyright (C) 2024-2025  G�nter Kanisch
    !------------------------------------------------------------------------------
        use UR_types,     only: rn
        use UR_DecChain
        use UR_Gleich_globals,     only: charv,FormeltextFit
        use CHF,           only: ucase
        use Top,           only: CharModA1
        use Rout,          only: WDPutTextviewString

        implicit none

        integer(4)               :: i,j,k,nndc,jj,i1,k1,k2,Lst,nfd,j1,n2,L
        character(len=3)         :: zzz
        type(charv),allocatable  :: Fmatstr(:,:),BmatStr(:,:),FF(:,:),Phimatstr(:),HmatStr(:,:)
        character(len=7)         :: Fii,Fjj,Fij,Fik,Fkj
        character(len=20)        :: cdummy
        character(len=400)       :: text

        DClistsDefined = .false.

        allocate(FmatStr(N_nuclides,N_nuclides),BmatStr(N_nuclides,N_nuclides))
        allocate(FF(N_nuclides,N_nuclides),PhimatStr(N_nuclides))
        allocate(Hmatstr(N_nuclides,N_nuclides))

        nfd = 0
        nndc = 0        ! Number of nuclides in the decay chain  ( <= N_nuclides)
        do i=1,N_nuclides
        if(i > 1) then
            if(DCsymbYield(i)%s /= DCsymbYield(i-1)%s) nfd = 1
        endif
        enddo
        ! If DCcommMeasmt is true and nfd is 0 : there is only one common chemical yield;
        ! therefore, the chemical yield is omitted from the Xi formulae obtained
        ! at the end of this routine. If it were not suppressed here, the chemical yield would
        ! generate covariances between the Xi formulae.

        text = DCList(ChainSelected)%s
        i1 = index(text,':')
        chaincode%s = trim(adjustL(text(1:i1-1)))
        L = len_trim(chaincode%s)
        read(chaincode%s(L-1:L-1),*) nndc

        if(allocated(FormelTextFit)) deallocate(FormeltextFit)
        n2 = 0

        ! write(0,*) 'DCZji=',(DCZji(i)%s,' ',i=1,nzji),' nzji=',nzji

        do jj=1,DCchannels
        do i=1,N_nuclides
            do j=1,N_nuclides
            FmatStr(i,j)%s = ' '
            Bmatstr(i,j)%s = ''
            FF(i,j)%s = ''
            end do
        end do

        do i=1,N_nuclides

            if(jj == 1) Phimatstr(i)%s = DCsymbEffiA(i)%s//'*'//DCsymbYield(i)%s
            if(jj == 2) Phimatstr(i)%s = DCsymbEffiB(i)%s//'*'//DCsymbYield(i)%s
            if(jj == 3) Phimatstr(i)%s = DCsymbEffiC(i)%s//'*'//DCsymbYield(i)%s

            if(DCcommMeasmt .and. nfd == 0) then
            i1 = index(Phimatstr(i)%s,'*')
            Phimatstr(i)%s = trim(Phimatstr(i)%s(1:i1-1))
            end if

            FmatStr(i,i)%s = 'exp(-' // DCsymbLambda(i)%s // '*(tAS+tstart))'
            ! if(.true.) FmatStr(i,i)%s = FmatStr(i,i)%s //'*(1 - exp(-'//DCsymbLambda(i)%s//'*tmess))/('//DCsymbLambda(i)%s//'*tmess))'
            if(.true.) FmatStr(i,i)%s = 'fd(tAs+tstart,tmess,'//DCsymbLambda(i)%s//')'
            write(cdummy,'(a,i1,a,i1,a)') 'FF(',i,',',i,')'
            FF(i,i)%s = trim(cdummy)
            write(Fii,'(a,i1,a,i1,a)') 'FF(',i,',',i,')'
            if(i == 1) cycle
            if(i > nndc) cycle

            do j=i-1,1,-1
            if(i > nndc) then
                Fmatstr(i,j)%s = '0.0'
                cycle
            endif
            Bmatstr(i,j)%s = ''
            if(nzji > 0) then
                write(zzz,'(a1,i1,i1)') 'z',j,i
                write(0,*) 'zzz=',zzz
                do j1=1,nzji
                if(ucase(zzz) == ucase(DCZji(j1)%s)) then
                    if(DCZjival(j1) > 0._rn .and. abs(DCZjival(j1) - 1.0_rn) > 1.E-5_rn) then
                    Bmatstr(i,j)%s = zzz
                    write(66,*) 'j1=',j1,' zzz=',zzz
                    endif
                end if
                end do
            end if
            if(len_trim(BmatStr(i,j)%s) > 0) then
                Bmatstr(i,j)%s = BmatStr(i,j)%s // '*' // DCsymbLambda(i)%s
            else
                Bmatstr(i,j)%s = DCsymbLambda(i)%s
            endif
                    write(66,'(2(a,i0),a,a)') 'i=',i,' j=',j,' Bmatstr=',BmatStr(i,j)%s

            write(Fjj,'(a,i1,a,i1,a)') 'FF(',j,',',j,')'
            FmatStr(i,j)%s = '1/(' // DCsymbLambda(j)%s // '-' // DCsymbLambda(i)%s // ')* ('
            Fmatstr(i,j)%s = trim(Fmatstr(i,j)%s)//' '//Bmatstr(i,j)%s//'*( '//Fii//'-'//Fjj//') '
            if(j+1 > i-1) then
                Fmatstr(i,j)%s = trim(Fmatstr(i,j)%s)//' )'
                write(Fij,'(a,i1,a,i1,a)') 'FF(',i,',',j,')'
                cycle
            end if
            do k=j+1,i-1
                write(66,'(3(a,i0))') 'i=',i,' j=',j,' k=',k
                write(Fik,'(a,i1,a,i1,a)') 'FF(',i,',',k,')'
                write(Fkj,'(a,i1,a,i1,a)') 'FF(',k,',',j,')'
                Fmatstr(i,j)%s = trim(Fmatstr(i,j)%s)//' + ( '// &
                Fik//'*'//Bmatstr(k,j)%s//' - '//Bmatstr(i,k)%s//'*'//Fkj//' )'
                if(k == i-1) Fmatstr(i,j)%s = trim(Fmatstr(i,j)%s)//' )'
                write(cdummy,'(a,i1,a,i1,a)') 'FF(',i,',',j,')'
                FF(i,j)%s = trim(cdummy)
            enddo
            enddo     ! do j=i-1
        end do    ! do i=1,

        write(66,*) 'Calculated FmatStr:   nndc=',nndc
        do i=1,N_nuclides
            do j=i,1,-1
            if(i > nndc .and. j < i) cycle
            write(66,'(a,i1,a,i1,a,a)') 'FF(',i,',',j,') = ', FmatStr(i,j)%s
            enddo
        end do

        do i=1,N_nuclides
            if((apply_separation .and. i == DCBuildupAtSepar)) then          ! xxxxxxxxxxxxxxxxxxxxxxxx ersetzen
            Hmatstr(i,i)%s = '0.0'
            else
            Hmatstr(i,i)%s = Phimatstr(i)%s //' * ' // trim(Fmatstr(i,i)%s)
            end if
            if(i == 1) cycle

            do j=i-1,1,-1
            if(i > nndc) cycle
            if(i <= nndc) then
                Hmatstr(i,j)%s = Phimatstr(i)%s // ' * ' // trim(Fmatstr(i,j)%s)
                if(apply_separation) then
                HmatStr(j,j)%s = HmatStr(j,j)%s // ' + ' // HmatStr(i,j)%s
                Hmatstr(i,j)%s = '0.0'
                end if
            else
                Hmatstr(i,j)%s = '0.0'
            end if
            enddo
        end do
        do i=1,N_nuclides
            do j=i,1,-1
            if(HmatStr(i,j)%s == '0.0') cycle
            do
                i1 = index(HmatStr(i,j)%s,'FF(')
                if(i1 > 0) then
                read(HmatStr(i,j)%s(i1:),'(3x,i1,1x,i1)') k1,k2
                HmatStr(i,j)%s = HmatStr(i,j)%s(1:i1-1) // FmatStr(k1,k2)%s // HmatStr(i,j)%s(i1+7:)
                else
                exit
                end if
            end do
            end do
        end do

        do i=1,N_nuclides
            if(nfd == 1) exit
            do j=i,1,-1
            if(HmatStr(i,j)%s == '0.0') cycle
            do
                i1 = index(HmatStr(i,j)%s,DCsymbYield(1)%s)
                if(i1 > 0) then
                Lst = len_trim(DCsymbYield(1)%s)
                HmatStr(i,j)%s = HmatStr(i,j)%s(1:i1-2) // HmatStr(i,j)%s(i1+Lst:)
                else
                exit
                end if
            end do
            end do
        end do

        write(66,*) 'Calculated HmatStr:'
        do i=1,N_nuclides
            do j=i,1,-1
            if(i > nndc .and. j < i) cycle
            if(HmatStr(i,j)%s == '0.0') cycle
            write(66,'(a,i1,a,i1,a,a)') 'HH(',i,',',j,') = ', HmatStr(i,j)%s
            enddo
        end do

        do i=1,N_nuclides
            do j=i,1,-1
            if(HmatStr(i,j)%s == '0.0') cycle
            if(len_trim(HmatStr(i,j)%s)  == 0) cycle
            n2 = n2 + 1
                call CharModA1(FormeltextFit,n2)
            write(text,'(a,i0,a,a)') 'X',n2,' = ', HmatStr(i,j)%s
            FormeltextFit(n2)%s = trim(text)
            write(66,*) 'FormeltextFit(n2)=',FormeltextFit(n2)%s
            enddo
        end do

        end do   ! do jj=1,

        call WDPutTextviewString('textviewModelEQ', FormeltextFit)

        DChain = .true.

    end subroutine DecCh


    !########################################################################

    subroutine Decaysub1(jind,Adest,uAdest)

    !------------------------------------------------------------------------------
    ! Purpose: Processing the information taken from a "SDECAY" call within
    !          an equation.
    !
    !          To each call of "SDECAY", within separate equations, a structure
    !          DCpar(jind) is attributed; they are differentiated by its index jind.
    !          Each structure holds values being necessary for decay chain calculations,
    !          such as time differences, counting times and activity start values.
    !
    !          For decay calculations according to the Bateman equations, a matrix-based
    !          method is applied (the F-Matrix, which is presented in section 4 of the
    !          paper:
    !            E. Levy, 2019: DECAY CHAIN DIFFERENTIAL EQUATIONS: SOLUTIONS THROUGH
    !            MATRIX ANALYSIS. Computer Physics Communications 234, 2019, 188-194
    !
    !          This method and its applications is described in:
    !                Kanisch et al., (2025): Zeitverhalten bei mehrgliedrigen
    !                Zerfallsreihen........ (in preparation)
    !          )
    !
    !          Introduced in UncertRadio by G�nter Kanisch in January 2025.
    !              Copyright (C) 2024-2025  G�nter Kanisch
    !------------------------------------------------------------------------------

        use UR_params,      only: zero
        use UR_Gleich_globals,      only: ifehl,Messwert,StdUnc,symboleG,kableitnum,StdUncSV, &
                                MesswertSV
        use UR_DecChain
        use CHF,            only: FindlocT,ucase
        use Num1,           only: matwrite
        use UR_DLIM,        only: iteration_on
        use ur_general_globals,   only: MCsim_on,ResultA_on,MCsim_localOff
        use UR_MCC,         only: imc
        use RW2,            only: kqt_find

        implicit none

        integer(4),intent(in)  :: jind             !  index of the 1 .. 6 DCpar structures
        real(rn),intent(out)   :: Adest,uAdest     ! activity and uncertainty of that chain
                                                ! nuclide with index Ndtestin (destination)

        real(rn)         :: ttmess,ttdiff
        logical          :: t_avg, unczero
        integer(4)       :: i,k,n, nndest,j,i1,j1,nst,ni,kdest,L,ncc,matrow,ios,kqtyp
        real(rn)         :: dummy
        character(len=150) :: ttt

        real(rn),allocatable   :: lam(:),Fmat(:,:),vec_1(:),vec_0(:),Uvec_1(:,:),Uvec_0(:,:),a0(:)
        real(rn),allocatable   :: zz(:,:),zzshort(:,:)
        real(rn),allocatable   :: vec_1_sh(:),Uvec_1_sh(:,:),lam_sh(:),zzshort_sh(:,:)
        real(rn),allocatable   :: vec_0_sh(:),Uvec_0_sh(:,:),ABLT(:,:)


        kqtyp = kqt_find()

        !write(0,*) 'jind=',jind
        !write(0,*) DCpar(jind)%indx,' ',DCpar(jind)%tdiff%s,' ',DCpar(jind)%tmess%s,' ',DCpar(jind)%forward

        ! In case of the first call: find lists of numerical values of the DC vectors
        ! to be taken from UR's Messwert and StdUnc lists/vectors.
        if(.not.DClistsDefined) then
        if(allocated(DClam)) deallocate(DCLam,uDCLam,DCeffiA,uDCeffiA)
        allocate(DCLam(N_nuclides),uDCLam(N_nuclides),DCeffiA(N_nuclides),uDCeffiA(N_nuclides))

        DCLam = 0._rn
        uDCLam = 0._rn
        DCeffiA = 0._rn
        uDCeffiA = 0._rn

        ! load data from UR's Uncrtainty/Values-Grid into the DC-array values:
        do i=1,N_nuclides

            if(len_trim(DCsymbT12(1)%s) > 0 .and. len_trim(DCsymbLambda(1)%s) == 0) then
            k = findlocT(SymboleG,ucase(DCsymbT12(i)%s),1)
            if(k == 0) then
                if(.not.iteration_on .and. kableitnum == 0) write(66,*) 'DecaySub1: not found in UR: DCsymbT12(i)=',DCsymbT12(i)%s
                !ifehl = 1
                !return
            else
                if(Messwert(k) <= zero) then
                write(66,*) 'a half-live value is missing / not given! '
                ifehl = 1
                return
                end if
                if(Messwert(k) > 0._rn) DCLam(i) = log(2._rn)/Messwert(k)
                if(StdUnc(k) > 0._rn) uDCLam(i) = DCLam(i)*(StdUnc(k)/Messwert(k))
            end if
            else
            k = findlocT(SymboleG,ucase(DCsymbLambda(i)%s),1)
            if(k == 0) then
                if(.not.iteration_on .and. kableitnum == 0) write(66,*) 'DecaySub1: not found in UR: DCsymbLambda(i)=',DCsymbLambda(i)%s
                !ifehl = 1
                !return
            else
                if(Messwert(k) > 0._rn) then
                DCLam(i) = Messwert(k)
                uDCLam(i) = StdUnc(k)
                else
                write(66,*) 'A decay constant value is missing/not given! '
                ifehl = 1
                return
                end if
            end if
            end if

            if(len_trim(DCsymbEffiA(i)%s) > 0) then
            k = findlocT(SymboleG,ucase(DCsymbEffiA(i)%s),1)
            if(k == 0 .and. len_trim(DCsymbEffiA(i)%s) > 0 ) then
                if(.not.iteration_on .and. kableitnum == 0) write(66,*) 'DecaySub1: not found in UR: DCsymbEffiA(i)=',DCsymbEffiA(i)%s
                !ifehl = 1
                !return
            else
                if(Messwert(k) > 0._rn) DCeffiA(i) = Messwert(k)
                if(Messwert(k) > 0._rn) uDCeffiA(i) = StdUnc(k)
            end if
            endif

        end do
        DClistsDefined = .true.

        end if

        ! build Fmat:
        allocate(a0(N_nuclides))
        allocate(zz(N_nuclides,N_nuclides))
        zz = 0._rn
            ! write(0,*) (DCZji(i)%s,i=1,2)
        nst = DCpar(jind)%Nstart
        do i=1,N_nuclides
            do j=1,N_nuclides
            do k=1,2  ! nZji
                read(DCZji(k)%s,'(1x,i1,i1)') j1,i1
                if(i == i1 .and. j == j1) zz(j,i) = 1._rn
            enddo
            enddo
        end do
                ! call matwrite(zz,N_nuclides,N_nuclides,0,'(5(es12.5,1x))','Matrix zz')
        L = len_trim(chaincode%s)
        read(chaincode%s(L-1:L-1),*) ncc          ! number of nuclides with the chain
        ! n = N_nuclides - DCpar(jind)%Nstart + 1
        n = ncc - DCpar(jind)%Nstart + 1

                    DCpar(jind)%nchlen = n       ! 17.1.2025  GK

        allocate(vec_0(n),vec_1(n),lam(n),Fmat(n,n),Uvec_0(n,n),Uvec_1(n,n),zzshort(n,n))
        Allocate(ABLT(n,n))
        vec_0 = 0._rn
        vec_1 =  0._rn
        Uvec_0 = 0._rn
        Uvec_1 = 0._rn
        Fmat = 0._rn
        zzshort = 0._rn

        nst = DCpar(jind)%Nstart
        lam(1:n) = DClam(nst:ncc)
        ! write(63,*) 'lam=',sngl(lam)

        nst = DCpar(jind)%Nstart
        do i=nst,ncc   ! N_nuclides
        do j=nst,ncc   ! N_nuclides
            zzshort(1+i-nst, 1+j-nst) = zz(i,j)
        end do
        end do

        t_avg = .true.
        if(DCpar(jind)%avg == 0) t_avg = .false.

        k = findlocT(SymboleG,ucase(DCpar(jind)%tdiff%s),1)
        if(k > 0 ) ttdiff = Messwert(k)
            ! write(0,*) 'tdiff:  found k=',int(k,2)
            if(k == 0) then
            read(DCpar(jind)%tdiff%s,*,iostat=ios) dummy
            if(ios == 0) ttdiff = dummy
            end if

        k = findlocT(SymboleG,ucase(DCpar(jind)%tmess%s),1)
        if(k > 0 ) ttmess = Messwert(k)
            if(k == 0) then
            read(DCpar(jind)%tmess%s,*,iostat=ios) dummy
            if(ios == 0) ttmess = dummy
            end if

                Adest = zero
                uAdest = zero

        nndest = DCpar(jind)%Ndestin - DCpar(jind)%Nstart + 1

        if(DCpar(jind)%forward == 1) then

        ! do forward decay calculations (for the time duration from t=0 to t=tdiff):
        call matrix_build(n,lam,zzshort,ttdiff,ttmess,t_avg,Fmat)
            write(66,*) 'forward: ttdiff=',sngl(ttdiff),' ttmess=',sngl(ttmess)

        do j=1,n
            k = DCpar(jind)%Symbind(j)
            if(.not.MCsim_on .or. MCsim_localOff) vec_0(j) = Messwert(k)
            if(MCsim_on .and. .not. MCsim_localOff) vec_0(j) = MesswertSV(k)
        end do
        ! multiply the F-Matrix with vector vec_0 and attribute the result to Adest:
        vec_1 = matmul(Fmat,vec_0)
        Adest = vec_1(nndest)
                write(66,*) 'forward: vec_1=',sngl(vec_1),' Adest=',sngl(Adest)

        else
        ! do backward decay calculations (from activities measured at t=tdiff back to t=0):

        do j=1,n
                ! j: index of array A_k(t), i.e., the index in the chain
            k = DCpar(jind)%Symbind(j)    ! UR Equation index

            if(.true.) then   !  .and. .not.MCsim_on) then
            if(.not.MCsim_on .or. MCsim_localOff) then
                vec_1(j) = Messwert(k)
                ! if(kqtyp == 1 .and. StdUnc(k) > 0._rn) Uvec_1(j,j) = StdUnc(k)**2._rn
                if(kqtyp >= 1) Uvec_1(j,j) = DCpar(jind)%SD_CV(kqtyp,j) ** 2._rn
            else
                vec_1(j) = MesswertSV(k)
                if(kqtyp >= 1) then
                Uvec_1(j,j) = DCpar(jind)%SD_CV(kqtyp,j) ** 2._rn
                else
                ! if(StdUncSV(k) > 0._rn) Uvec_1(j,j) = StdUncSV(k)**2._rn
                end if
            end if

                    ! if(ubound(StdUncSV,dim=1) > 0 .and. StdUnc(k) > 0._rn) Uvec_1(j,j) = StdUncSV(k)**2._rn  !######################
            if(nndest == j) kdest = k
                !write(0,*) 'DECH: Eq k=',int(k,2),' MW(k)=',sngl(Messwert(k)),' Stdunc(k)=',sngl(StdUnc(k))
            !write(63,*) 'kqtyp=',int(kqtyp,2),' iteration_on=',iteration_on
            ! write(63,*) 'DECH: Eq k=',int(k,2),' MW(k)=',sngl(Messwert(k)),' Stdunc(k)=',sngl(StdUnc(k))

                !  if(.false. .and. MCsim_on .and. Resulta_on) &
            !     write(ttt,*) ' vec_1:',sngl(vec_1(1:n)),' uvec=',(sngl(sqrt(uvec_1(i,i))),i=1,n)
            ! if(.true.) write(63,'(3(a,i3),a)') 'jind=',jind,' j=',j, ' symbol k=',k, trim(ttt)
            else
            vec_1(j) = MesswertSV(k)
            if(StdUncSV(k) > 0._rn) Uvec_1(j,j) = StdUncSV(k)**2._rn
                    !  if(ubound(StdUncSV,dim=1) > 0 .and. StdUnc(k) > 0._rn) Uvec_1(j,j) = StdUncSV(k)**2._rn  !######################
            if(nndest == j) kdest = k
                if(.false. .and. .not.MCsim_on .and. .not.Resulta_on) then
                write(ttt,*) ' vec_1:',sngl(vec_1(1:n)),' uvec=',(sngl(sqrt(uvec_1(i,i))),i=1,n)
                write(66,'(3(a,i3),a)') 'jind=',jind,' j=',j, ' symbol k=',k, trim(ttt)
                end if
            end if
        end do
            ! if(.not.MCsim_on .and. .not.Resulta_on) write(63,*) 'kdest=',int(kdest,2)

        unczero = .false.
        do i=1,n
            if(Uvec_1(i,i) <= 0._rn) unczero = .true.
        enddo
            !   if(kableitnum == 0 .and. .not.iteration_on)  &
            !         write(66,*) 'Decaysub1: back: vec_1=',sngl(vec_1),'  t_avg=',t_avg
        if(.not.unczero) then
            ! use the least-squares method to solve for vec_0, based on vec_1:
            if(.true.) then
            call backdecay(n,vec_1,uvec_1,lam,zzshort,ttdiff,ttmess,t_avg,vec_0,uvec_0,ABLT)
            Adest = vec_0(nndest)
            uAdest = sqrt(uvec_0(nndest,nndest))
                ! if(.not.MCsim_on .and. .not.Resulta_on) then
            !   if(MCsim_on .and. .not.Resulta_on .and. Adest > 150._rn .and. imc < 300) then
            !         write(63,*) 'nach call backdecay: ', &
            !          ' nndest=',int(nndest,2),' Adest=',sngl(Adest),' uAdest=',sngl(uAdest), &
            !                   ' vec_1=',sngl(vec_1(1:n))
            !  if(kableitnum == 0) &
            !     write(0,*) 'nach call backdecay: ', &
            !          ' nndest=',int(nndest,2),' Adest=',sngl(Adest),' uAdest=',sngl(uAdest), &
            !                   ' vec_1=',sngl(vec_1(1:n))
            !  end if

            if(MCsim_on .and. imc == 3) then

            end if

            ! if(MCsim_on .and. imc < 100) then
            !   write(63,*) 'imc=',imc,'  ablt(2,*)=',sngl(ablt(2,1:2))
            ! end if
            !.............   7.1.2025 GK
                ! call matwrite(ABLT,n,n,66,'(5es13.5)','matrix ablt:')
            if((.not.MCsim_on .or. MCsim_localOff)) then    ! 19.1.2025     !  .and. .not.iteration_on) then   ! <--  12.1.2025 GK
                matrow = DCpar(jind)%Ndestin - DCpar(jind)%Nstart + 1
                do i=1,10
                if(i <= matrow) then
                    DCpar(jind)%derv(i) = ABLT(matrow,i)
                else
                    DCpar(jind)%derv(i) = zero
                end if
                end do
            end if
            !.............
            else
            allocate(vec_1_sh(nndest),Uvec_1_sh(nndest,nndest),lam_sh(nndest),zzshort_sh(nndest,nndest))
            allocate(vec_0_sh(nndest),Uvec_0_sh(nndest,nndest))
            vec_1_sh(1:nndest) = vec_1(1:nndest)
            lam_sh(1:nndest) = lam(1:nndest)
            do i=1,nndest
                Uvec_1_sh(i,1:nndest) = Uvec_1(i,1:nndest)
                zzshort_sh(i,1:nndest) = zzshort(i,1:nndest)
            end do
            call backdecay(nndest,vec_1_sh,uvec_1_sh,lam_sh,zzshort_sh,ttdiff,ttmess,t_avg,vec_0_sh, &
                                                                            uvec_0_sh,ABLT)
            Adest = vec_0_sh(nndest)
            uAdest = sqrt(uvec_0_sh(nndest,nndest))
            endif
                    !  if(n >= 2)  write(66,*) 'vec_0:',vec_0(1:n)
        else
            ! use a recursive method to solve for vec_0:
            call matrix_build(n,lam,zzshort,ttdiff,ttmess,t_avg,Fmat)
            a0(1) = vec_1(1) / Fmat(1,1)
            a0(2) = 1._rn/Fmat(2,2)*( vec_1(2) - Fmat(2,1)*a0(1) )
            if(n > 2) then
            do ni=3,n
                ! a0(3) = 1._rn/Fmat(3,3)*( vec_1(3) - Fmat(3,1)*a0(1) -Fmat(3,2)*a0(2) )
                a0(ni) = vec_1(ni)
                do j=1,ni-1
                a0(ni) = a0(ni) - Fmat(ni,j)*a0(j)
                enddo
                a0(ni) = a0(ni) / Fmat(ni,ni)
            end do
            end if
            Adest = a0(nndest)
            uAdest = 0._rn
        endif

        end if

    end subroutine DecaySub1

    !########################################################################

    real(rn) function fdf(tA,tm,lam)
        implicit none

        real(rn),intent(in)   :: tA,tm,lam    ! time difference; counting time; decay constant

        if(tm <= 0._rn) then
        ! decay factor without counting time averaging
        fdf = exp(-lam*tA)
        else
        ! decay factor with counting time averaging
        fdf = exp(-lam*tA)/(lam*tm)*(1._rn - exp(-lam*tm))
        end if

    end function fdf

    !########################################################################

    subroutine matrix_build(n,lam,zz,tA,tm,t_avg,Fmat)

    !------------------------------------------------------------------------------
    ! Purpose: Processing the information taken from a "SDECAY" call within
    !          an equation.
    !
    !          To each call of "SDECAY", within separate equations, a structure
    !          DCpar(jind) is attributed, which are differentiated by its index jind.
    !          Each structure holds values being necessary for decay chain calculations,
    !          such as time differences, counting times and activity start values.
    !
    !          For decay calculations (Bateman equations), a matrix-based method is
    !          applied (the F-Matrix, which is presented in section 4 of the paper:
    !            E. Levy, 2019: DECAY CHAIN DIFFERENTIAL EQUATIONS: SOLUTIONS THROUGH
    !            MATRIX ANALYSIS. Computer Physics Communications 234, 2019, 188-194
    !          This method has been described in:
    !                Kanisch et al., (2025): Zeitverhalten bei mehrgliedrigen
    !                Zerfallsreihen........
    !
    !          Introduced in UncertRadio by G�nter Kanisch in January 2025.
    !              Copyright (C) 2024-2025  G�nter Kanisch
    !------------------------------------------------------------------------------

        use UR_params
        use CHF,              only: ucase,FindlocT
        use Num1,             only: matwrite

        implicit none

        integer(4),intent(in) :: n            ! number of decay chain members
        real(rn),intent(in)   :: lam(n)       ! array of decay constants
        real(rn),intent(in)   :: zz(n,n)      ! branching rataios given as few matrix elements
        real(rn),intent(in)   :: tA,tm        ! time difference and counting time
        logical,intent(in)    :: t_avg        ! average over counting time: Y / N
        real(rn),intent(out)  :: Fmat(n,n)    ! the F-Matrix

        integer(4)     :: i,j,ip,k,nbb
        real(rn)       :: bb(n,n),s,s1,f31,f32,f21            ! ,fdf
        logical        :: act_direct


        act_direct = .true.

        ! write(0,*) 'lam: ',sngl(lam)
        ! write(0,*) 'zz=',sngl(zz)
        !write(0,*) 'tA=',sngl(tA),' tm=',sngl(tm),' t_avg=',t_avg

        ! act_direct:  Bateman equations for atom numbers (=FALSE), or for activities (= TRUE)
        ! vec_0     :  vector of atom numbers or activities, depending on act_direct

        ! branching ratios, matrix zz(1:n,1:n) (Input):
        ! The decay chain members are ordered from 1 to n.
        ! After zeroing zz, the associated branching ratio is attributed to each
        ! transition j to i (j < i) : zz(j,i) = xb.
        ! For a decay chain without branching the values zz(j,i) are always 1.

        Fmat = zero

        ! Build the matrix B by combining the branching ratios with the decay constants!
        nbb  = 1
        bb = zero
        do i=1,n
            do j=1,i-1
            bb(i,j) = zz(j,i) * lam(i)
            end do
            bb(i,i) = -lam(i)
        enddo
            ! if(n == 3) call matwrite(bb,n,n,0,'(5(es11.4,1x))','Matrix bb')

        ! Build the matrix F:
        do i=1,n
            if(t_avg) Fmat(i,i) = fdf(tA,tm,lam(i))
            if(.not.t_avg) Fmat(i,i) = fdf(tA,0._rn*tm,lam(i))
        enddo
        if(.true.) then
        do ip=1,n-1
            do j=1,n-ip
            i = j + ip
            s1 = bb(i,j)*(Fmat(i,i) - Fmat(j,j))
                ! if(n == 5) write(66,*) "s1=",sngl(s1)," i-1=",int(i-1,2), "j+1=",int(j+1,2)," bb(i,j)=",sngl(bb(i,j))
            s = s1
            if((i-1) >= (j+1)) then
                do k=j+1,i-1
                if(k > 0) then
                    s = s + (Fmat(i,k)*bb(k,j) - bb(i,k)*Fmat(k,j))
                endif
                enddo
            endif
            Fmat(i,j) = s / (bb(i,i) - bb(j,j))
                ! if(n == 5) write(66,*) "Fmat(i,j)=",sngl(Fmat(i,j))," s=",sngl(s)," bb(i,i)=",sngl(bb(i,i))," bb(j,j)=",sngl(bb(j,j))
            enddo
        enddo
        endif
        ! call matwrite(Fmat,n,n,0,'(5(es12.5,1x))','Matrix Fmat')
        if(.true.) then
        ! an alternate build of the matrix:
        do i=1,n
            do j=i-1,1,-1
            Fmat(i,j) = bb(i,j) * (Fmat(i,i) - Fmat(j,j))
            do k=j+1,i-1
                Fmat(i,j) = Fmat(i,j) + Fmat(i,k)*bb(k,j) - bb(i,k)*Fmat(k,j)
            end do
            Fmat(i,j) = Fmat(i,j)/(bb(i,i) - bb(j,j))
            end do
        end do
        end if

            !if(n == 2)
            !   call matwrite(Fmat,n,n,0,'(5(es12.5,1x))','Matrix Fmat')
        if(n == 3) then
        ! an analytical formulation for n=3:
        f31 = lam(2)*lam(3) * ( Fmat(1,1)/(lam(2)-lam(1))/(lam(3)-lam(1)) +  &
                                Fmat(2,2)/(lam(1)-lam(2))/(lam(3)-lam(2)) +  &
                                Fmat(3,3)/(lam(1)-lam(3))/(lam(2)-lam(3))    )
        !write(0,*) 'F31-Formula=',sngl(f31)
        f32 = lam(3) * ( Fmat(2,2)/(lam(3)-lam(2)) + Fmat(3,3)/(lam(2)-lam(3)) )
        !write(0,*) 'F32-Formula=',sngl(f32)
        f21 = lam(2)/(lam(2)-lam(1))*(Fmat(1,1) - Fmat(2,2))
        !write(0,*) 'F21-Formula=',sngl(f21)
        end if

    end subroutine matrix_build

    !######################################################################################

    subroutine backdecay(n,Act,UAt,lam,zz,tA,tm,t_avg,A0,UA0,ABLT)

    !------------------------------------------------------------------------------
    ! Purpose: Refers activities measured at time tA back to t=0 by least squares
    !          analysis based on the F-Matrix which is treated as the LS design matrix.
    !
    !          Introduced in UncertRadio by G�nter Kanisch in January 2025.
    !              Copyright (C) 2024-2025  G�nter Kanisch
    !------------------------------------------------------------------------------

        use UR_params
        use Brandt,        only: mtxchi
        use Num1,          only: matwrite
        use RW2,           only: kqt_find

        implicit none

        integer(4),intent(in) :: n                ! number of decay chain nuclides
        real(rn),intent(in)   :: lam(n),zz(n,n)   ! decay constants and the branching ratio matrix
        real(rn),intent(in)   :: Act(n),UAt(n,n)  ! activities and their covariance matrix at time tA
        real(rn),intent(in)   :: tA,tm            ! time difference and counting time
        logical,intent(in)    :: t_avg            ! counting time averaging?
        real(rn),intent(out)  :: A0(n),UA0(n,n)   ! activities and their covariance matrix at time t=0
        real(rn),intent(out)  :: ABLT(n,n)        ! matrix of derivataives

        integer(4)        :: i
        real(rn)          :: Fmat(n,n)
        real(rn)          :: uATb(n,n)
        logical           :: act_direct

        act_direct = .true.     ! use activities instead of atom numbers

        ! build the F-Matrix:
        call matrix_build(n,lam,zz,tA,tm,t_avg,Fmat)
            ! call matwrite(Fmat,n,n,66,'(3(es13.6,1x))','BDy: Matrix Fmat:')

        ! least squares analysis:
        UAtb = zero
        do i=1,n
        UAtb(i,1:n) = uAt(i,1:n)
        enddo
        ! if(kableitnum == 0 .and. .not.iteration_on) call matwrite(UAtb,n,n,66,'(3(es13.6,1x))','BDy: Matrix UAbt:')

        call mtxchi(UAtb)    !  invert matrix
        UA0(1:n,1:n) = matmul(transpose(Fmat),matmul(UAtb,Fmat))
        call mtxchi(UA0)
        ABLT = matmul(UA0,matmul(transpose(Fmat),UAtb))
            ! if(kqt_find() == 2) call matwrite(ABLT,n,n,66,'(3(es13.6,1x))','BDy: Matrix ABLT:')
            ! call matwrite(Fmat,n,n,63,'(3(es13.6,1x))','BDy: Matrix Fmat:')
        A0 = matmul(ABLT,act)
        !if(kableitnum == 0 .and. .not.iteration_on) then
        !  write(66,*) 'A0=',sngl(A0)
        !  call matwrite(UA0,n,n,66,'(3(es13.6,1x))','BDy: Matrix UA0:')
        !  call matwrite(Fmat,n,n,66,'(3(es13.6,1x))','BDy: Matrix Fmat:')
        !end if

    end subroutine backdecay

    !###############################################################################

    subroutine analyze_Sdecay_Records()

        use UR_Gleich_globals,    only: RSeite,SymboleG
        use CHF,                  only: ucase,FindlocT
        use UR_DecChain,          only: nsubdec,dcpar

        implicit none

        integer(4)          :: i,jj,ii,i1,i2,narg,kkk,ios
        character(len=100)  :: text1


        !   jj (or knd)           index numbers of different SDecay-calls in the UR equations
        !   DCpar(jj)%indx        index of the UR equation
        !   DCpar(jj)%Symb(i)%s   Symbol name of DC activities referenced in the SDecay call
        !   DCpar(jj)%Symbind(i)  index of this symbol within the UR symbols list
        !   DCpar(jj)%forward     direction of decay: 0: backwards, 1: forward
        !   DCpar(jj)%tdiff%s     elapsed time difference
        !   DCpar(jj)%tmess%s     counting duration
        !   DCpar(jj)%avg         average over counting duration: yes: T, no: F
        !   DCpar(jj)%Nstart      start element of the decay chain, from which a sub-chain
        !                            shall be used; =1: wholechain; >1: sub-chain
        !   DCpar(jj)%Ndestin     to which of the chain member does the calculated value refer to?



        ! 15.12.2024 GK
                    write(66,*) 'nsubdec=',nsubdec
        do jj=1,nsubdec        ! number of different SDecay-calls in the UR equations
        ii = DCpar(jj)%indx                  ! ii: index of the UR equation
        text1 = TRIM(ucase(Rseite(ii)%s))
                            write(66,*) 'jj=',jj,' text1=',trim(text1)
        do i=1,10
            DCpar(jj)%Symb(i)%s = ' '
            DCpar(jj)%Symbind(i) = 0
        enddo
        i1 = index(text1,'SDECAY')
        text1 = text1(i1+6:)
        i1 = index(text1,'(')
        text1 = text1(i1+1:)
        i2 = index(text1,',')
        read(text1(1:i2-1),*,iostat=ios) kkk
        if(ios == 0) DCpar(jj)%forward = kkk
        narg = 1
        do
            text1 = text1(i2+1:)
            if(len_trim(text1) == 0) exit
            i2 = index(text1,',')
            if(i2 > 0) then
            narg = narg + 1
                !write(66,*) 'Zelle=',trim(adjustL(text1(1:i2-1)))
            if(narg == 2) DCpar(jj)%tdiff%s = trim(adjustL(text1(1:i2-1)))   ! elapsed time difference
            if(narg == 3) DCpar(jj)%tmess%s = trim(adjustL(text1(1:i2-1)))   ! counting duration
            if(narg == 4) then
                read(text1(1:i2-1),*) kkk
                DCpar(jj)%avg = kkk
            endif
            if(narg == 5) then
                read(text1(1:i2-1),*) kkk
                DCpar(jj)%Nstart = kkk
            elseif(narg == 6) then
                read(text1(1:i2-1),*) kkk
                DCpar(jj)%Ndestin = kkk
            end if
            if(narg >= 7) then
                DCpar(jj)%Symb(narg-6)%s = trim(adjustL(text1(1:i2-1)))
                DCpar(jj)%Symbind(narg-6) = findlocT(SymboleG,ucase(DCpar(jj)%Symb(narg-6)%s))
            end if
            ! if(narg == 7) DCpar(jj)%Symb1%s = trim(adjustL(text1(1:i2-1)))
            ! if(narg == 8) DCpar(jj)%Symb2%s = trim(adjustL(text1(1:i2-1)))
            else
            i2 = index(text1,')')
            if(i2 > 0) then
                narg = narg + 1
                DCpar(jj)%Symb(narg-6)%s = trim(adjustL(text1(1:i2-1)))
                DCpar(jj)%Symbind(narg-6) = findlocT(SymboleG,ucase(DCpar(jj)%Symb(narg-6)%s))
                !if(narg == 8) DCpar(jj)%Symb2%s = trim(adjustL(text1(1:i2-1)))
                !if(narg == 9) DCpar(jj)%Symb3%s = trim(adjustL(text1(1:i2-1)))
                exit
            endif
            end if
        enddo
        enddo

    end subroutine analyze_Sdecay_Records

!###############################################################################

end module DECH
