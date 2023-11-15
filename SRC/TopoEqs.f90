

          !     contains
          ! TopoSort
          ! chains
          ! chainseval
          ! RnetParsNew

subroutine TopoSort(knetto)

   !     Copyright (C) 2021-2023  Günter Kanisch

   ! this routine tries to determine the topolgy of equations, i.e., a principle
   ! associated with equations used for caclulating the characteristic values for
   ! an output quantity.
   ! It will be used for finding out which count rates (maybe defined as the ratio
   ! of counts and time) are contained in the expression of the net count rate
   ! (the latter given by the equation number knetto.
   ! See chapter 2.3 "Equations as tree topology" for more information.

   ! uses chains, chainseval, RnetParsNew

   ! The following example output may better show what is behind this method. The
   ! count rates contributing to the net count rate are distributed over several
   ! equations.

!    Example Ra226_U235-at-186keV_EN.txp. The net count rate is RRa.
!
!    Equations (nab=8, nmu=10):
!
!      1  : cRa = Phi * RRa
!      2  : Phi = 1. / (eps * pRA * mp)
!      3  : RRa = RS - RU5
!      4  : RS = Rb - RT - RnNE
!      5  : RU5 = AU5 * Ufakt
!      6  : Ufakt = eps * pU5 * mp
!      7  : Rb = Nb / tm
!      8  : RT = NT / tm
!
!    Table of transitions i --> j:
!
!     nd      i    j    Symb(i)  Symb(j)
!    ---------------------------------------
!      1      3    4    RRa      RS
!      2      3    5    RRa      RU5
!      3      4    7    RS       Rb
!      4      4    8    RS       RT
!      5      4   12    RS       RnNE
!      6      5   13    RU5      AU5
!      7      5    6    RU5      Ufakt
!      8      6    9    Ufakt    eps
!      9      6   14    Ufakt    pU5
!     10      6   11    Ufakt    mp
!     11      7   15    Rb       Nb
!     12      7   16    Rb       tm
!     13      8   17    RT       NT
!     14      8   16    RT       tm
!
!    Table of cascades (chain) and three identified count rates
!    as part of the net count rate:
!
!     nc     i     j   kcnt ktime krate rule Symbol     chain
!    -------------------------------------------------------------------
!      1     7    15   15    15     7     A5  Rb         3  4  7 15
!      2     7    16    0     0     0                    3  4  7 16
!      3     8    17   17    17     8     A3  RT         3  4  8 17
!      4     8    16    0     0     0                    3  4  8 16
!      5     4    12    0     0    12     A6  RnNE       3  4 12
!      6     5    13    0     0     0                    3  5 13
!      7     6     9    0     0     0                    3  5  6  9
!      8     6    14    0     0     0                    3  5  6 14
!      9     6    11    0     0     0                    3  5  6 11
!
!----------------------------------------------------------------------
   ! some information about involved variables
!
!
!  integer(4)              :: ndep
!  integer(4),allocatable  :: idep(:,:), kmulrun(:),ukenn(:),akenn(:),kcnt(:)
!  integer(4),allocatable  :: ktime(:),krate(:),iback(:,:)
!  character(len=51)       :: seqch(40)
!
!
!  ndep           : index of a right-hand side symbol within the list of symbols obtained
!                     by summing over associated symbols in the equations knetto .. nab
!
!  eqnum(ndep)    : number of Eq. to which symbol ndep belongs to
!  synum(ndep)    : number of ri-hd side symbol RS_SymnolNr(k,i)
!  opnum(ndep)    : indicates operator between ri-hd- side symbols: 1,2,3,4 for '-', '+', '*', '/'
!
!     if ndep belongs to number i in RS_SymbolNr(k,i), i.e., the list of ri-hd symbols of equation k,
!           then eqndep(ndep) = k and syndep(ndep) = i;
!           if(i < nRSsy(k)) then opnum(ndep) is the sort index of the operator RS_ops(k,i), i.e.,
!              opnum = 1,2,3,4 or 5 for RS_ops(k,i) = '-','+','*','/','^'
!
!  eqndep(k) = findloc(eqnum(1:ndep2),k,dim=1)    !    ndep number of equation k
!  syndep(RS_SymbolNr(k,i)) = findloc(synum(1:ndep2),RS_SymbolNr(k,i),dim=1)      !  ndep number of symbol of i-th symbol in Eq. k
!
!  !----------------------------------
!  seqch(40)      : chain of symbol numbers, as string of integer numbers delimited by ' '
!  ksq*           : chain-numbers (index)        ! see Rechw1, at the begin
!                   kanf0 : is knetto(kEGr)
!                   ksq1  : is kanf0 at the begin, raised then to ksq1 > kanf0
!                   ksq   : is raised
!
!  kmulrun(ngrs)  : for count rate symbols: the number of ri-hd side symbols
!
!
!  nc             : counts chains ksq1 to ksq2
!  ukenn(nab)     : ukenn(nc)=1 for distribution types ivtl(kk) of 4 or 7: explicitly named count number, or by sqrt(N)
!                   ukenn(nc)=2 for distribution type ivtl(kk)=11 : preset count number (N_preset true)
!
!      rules for identifying a count rate by its uncertainty formula:
!  akenn(nab)     : akenn(nc)=1 count rate R identfied sqrt(R)
!                   akenn(nc)=2 count rate R identfied by sqrt(t**2/N)   'N_preset=T'
!                   akenn(nc)=3 count rate R identfied by sqrt(R**2/N)   'N_preset=T'
!----------------------------------------------------------------------------------------------------

use UR_params,      only: eps1min,rn,one
use UR_gleich,      only: Symbole,nRSsy,nab,ngrs,RS_SymbolNr,ndep,eqnum,synum,opnum,symtyp,Messwert, &
                          RS_ops,kmulrun,ukenn,akenn,kcnt,ktime,krate,eqndep,syndep,ivtl,Formelt, &
                          RS_SymbUse,RS_opsPos,RSeite
use CHF,            only: ucase

implicit none

integer(4),intent(in)   :: knetto       ! equation number of the net count rate

logical            :: prout
INTEGER(4)         :: I,k,krun,ndep2,iskip,itr,i3,j,klmin,klmax,nsub,jplus,i2,ios
character(len=1)   :: op(0:5)
character(len=90)  :: string
real(rn)           :: dummy

op(0:5) = [ ' ', '-', '+', '*', '/', '^' ]

prout = .false.
  ! prout = .true.
ndep = 0

do k=knetto,nab
  ndep = ndep + nRSSy(k)
end do

if(allocated(eqnum)) deallocate(eqnum);
allocate(eqnum(ndep))
if(allocated(synum)) deallocate(synum);
allocate(synum(ndep))
if(allocated(opnum)) deallocate(opnum);
allocate(opnum(ndep))
if(allocated(eqndep)) deallocate(eqndep);
allocate(eqndep(ngrs))
if(allocated(syndep)) deallocate(syndep);
allocate(syndep(ngrs))
if(allocated(kmulrun)) deallocate(kmulrun);
allocate(kmulrun(ngrs))
if(allocated(ukenn)) deallocate(ukenn);
allocate(ukenn(nab))
if(allocated(akenn)) deallocate(akenn);
allocate(akenn(nab))
if(allocated(kcnt)) deallocate(kcnt);
allocate(kcnt(nab))
if(allocated(ktime)) deallocate(ktime);
allocate(ktime(nab))
if(allocated(krate)) deallocate(krate);
allocate(krate(nab))

!write(0,*) 'Topo: Ndep - simple=',int(ndep,2),' ubound(eqndep,dim=1)=',ubound(eqndep,dim=1), &
!            ' ngrs=',int(ngrs,2)

kmulrun = 0
eqnum = 0
synum = 0
opnum = 0
ukenn = 0
akenn = 0
kcnt = 0
ktime = 0
krate = 0
eqndep = 0
syndep = 0

if(prout) then
  write(66,'(2(a,i0))') 'ngrs=',ngrs,' nab=',nab

  do i=1,ngrs
    write(66,'(a,i0,1x,a,a,i0)') 'i=',i,Symbole(i)%s,' ivtl=',ivtl(i)
  end do
end if

do krun=1,2

  if(krun == 2) ndep2 = ndep
  ndep = 0
  do k=knetto,nab
    ! write(66,'(a,i0,a,200a)') 'Formel k=',k,' before:  Symbs=',(Symbole(RS_SymbolNr(k,i))%s,' ',i=1,nRSsy(k))

        iskip = 0
    do i=1,nRSsy(k)
             if(iskip == i) cycle
      ndep = ndep + 1
      eqnum(ndep) = k
      synum(ndep) = RS_SymbolNr(k,i)
      if(krun == 2) then
        eqndep(k) = findloc(eqnum(1:ndep2),k,dim=1) !    ndep
        syndep(RS_SymbolNr(k,i)) = findloc(synum(1:ndep2),RS_SymbolNr(k,i),dim=1)      !    ndep
      end if
      if(i < nRSsy(k)) then
        if(RS_ops(k,i) == '-') opnum(ndep) = 1
        if(RS_ops(k,i) == '+') opnum(ndep) = 2
        if(RS_ops(k,i) == '*') opnum(ndep) = 3
        if(RS_ops(k,i) == '/') opnum(ndep) = 4
        if(RS_ops(k,i) == '^' .or. RS_ops(k,i) == '*') then
          if(RS_ops(k,i) == '^') opnum(ndep) = 5
          if(symtyp(RS_SymbolNr(k,i+1))%s == 't' .or.    &
             index(ucase(Symbole(RS_SymbolNr(k,i+1))%s),'_TRIGGER') > 0) then
            ! trigger variable:
            ios = 1
            if(opnum(ndep) == 5) then
              string = RSeite(k)%s(RS_opsPos(k,i)+1:)
              i2 = index(string,'^')
              if(i2 > 0) then
                ! write(66,*) 'string=',trim(string(1:i2-1))
                read(string,*,iostat=ios) dummy
              end if
            end if
            if(ios == 0 .and. abs(Messwert(RS_SymbolNr(k,i+1))) < eps1min) then
              ! if the value of a Trigger symbol is null, omit the previous variable:
              ! decrease ndep by 1
              itr = index(Formelt(k)%s,Symbole(RS_SymbolNr(k,i+1))%s)
              jplus = 0
              klmin = 0
              klmax = 0
              do j=itr-1,1,-1
                if(klmax == 0 .and. Formelt(k)%s(j:j) == ')') then; klmax = j; cycle; end if;
                if(klmax > 0 .and. Formelt(k)%s(j:j) == ')') then; jplus = jplus +1; cycle; end if;
                if(klmax > 0 .and. Formelt(k)%s(j:j) == '(') then
                  if(jplus > 0) then
                    jplus = jplus - 1
                    cycle
                  elseif(jplus == 0) then
                    klmin = j
                    exit
                  end if
                end if
              end do
              if(klmax > 0) then
                if(ndep > 1) then
                  nsub = 0
                  do j=ndep-1,1,-1
                    if(j < 1) exit
                    if(j > size(RS_SymbolNr,dim=2)) cycle
                    if(RS_SymbolNr(k,j) == 0) cycle

                    i3 = index(Formelt(k)%s,Symbole(RS_SymbolNr(k,j))%s)
                    if(i3 > klmin .and. i3 < klmax) then
                      nsub = nsub + 1
                      write(66,*) 'removed symbol: ',Symbole(RS_SymbolNr(k,j))%s
                      RS_SymbUse(k,j) = .false.
                    end if
                  end do
                  ndep = ndep - nsub
                end if
              else
                if(ndep > 0) ndep = ndep - 1
                write(66,*) 'removed symbol: ',Symbole(RS_SymbolNr(k,i))%s
                RS_SymbUse(k,i) = .false.
              end if
            end if
            ! at the next i-loop run omit the symbol of the triggers (i+1):
            iskip = i+1
          end if
        end if
      elseif(i == nRSsy(k)) then
        opnum(ndep) = 0
      end if
      if(prout .and. krun == 1) then
        !if(ndep == 1) write(66,'(A)') ' nd      i    j    Symb(i)  Symb(j)'
        !write(66,'(i3,4x,2(i3,2x),2x,2(a,5x))') ndep,eqnum(ndep),synum(ndep),  &
        !                  Symbole(eqnum(ndep))%s, Symbole(synum(ndep))%s

        if(ndep == 1) write(66,'(A)') 'ndep  eqnum(ndep) synum(ndep) opnum(ndep)    Symb(i)  Symb(j)'
        write(66,'(i3,6x,i3,10x,i3,10x,A1,12x,2(a,5x))') ndep,eqnum(ndep),synum(ndep),op(opnum(ndep)),  &
                          Symbole(eqnum(ndep))%s, Symbole(synum(ndep))%s
      end if
    end do
  end do
end do

   if(prout) write(66,*)

end subroutine TopoSort

!########################################################################

recursive subroutine chains(kanf0,kanf,ksq)

    !     Copyright (C) 2021-2023  Günter Kanisch

use UR_gleich,   only: eqnum,synum,opnum,nab,seqch,kmulrun, &
                       eqndep,syndep,nRSsy

implicit none

integer(4),intent(in)           :: kanf0    ! net count rate equation number,
                                            ! with the first call of chains
integer(4),intent(in)           :: kanf     ! Equation number transition kanf --> ksq
integer(4),intent(inout)        :: ksq      ! chain-number

integer(4)         :: nsd,n2,i,idy,klen,kk,i1
character(len=3)   :: cnum
logical            :: prout

prout = .false.
  ! prout = .true.

  ! write(66,'(3(a,i0))') 'start chains: kanf0=',kanf0,' kanf=',kanf,' ksq=',ksq
if(ksq == 0) ksq = 1

kk = kanf
20    continue

if(kk == kanf0) then
  if(kmulrun(kk) == 0) then
    write(cnum,'(i3)') kk
    seqch(ksq) = cnum
    kmulrun(kk) = 1
  end if
  nsd = eqndep(kk)
     if(prout) write(66,'(5(a,i0))') 'ksq=',ksq,' nsd=',nsd,' kk=',kk,' kmulrun(kk)=',kmulrun(kk), &
                    ' kanf=',kanf
  n2 = synum(nsd+kmulrun(kk)-1)     ! number of r-h side symbol RS_SymnolNr(k,i)
    if(prout) write(66,'(4(a,i0),a,a)') 'A:  nsd=',nsd,' kk=',kk,' ksq=',ksq,' n2=',n2, &
                                             '  seqch(ksq)=',trim(seqch(ksq))
  call chains(kanf0,n2,ksq)
end if

if(kk > nab) then
   ! kk is the number of an independent input quantity
  seqch(ksq+1) = trim(seqch(ksq))
  write(cnum,'(i3)') kk
  seqch(ksq) = trim(seqch(ksq)) // cnum
    if(prout) write(66,*) 'chain=',trim(seqch(ksq))

  klen = len_trim(seqch(ksq))
  ksq = ksq + 1
  seqch(ksq) = seqch(ksq-1)(1:klen-3)

  do
    klen = len_trim(seqch(ksq))
    if(klen >= 6) then
      read(seqch(ksq),*) (idy,i=1,(klen/3-1)),kk
          ! if(kk == 0) write(66,'(a,i0,a,a)') 'kk=0:  ksq=',ksq,' seqch(ksq)=',trim(seqch(ksq))
      seqch(ksq) = seqch(ksq)(1:klen-3)
    else
      kk = kanf0
      exit
    end if
    if(kk <= nab) then
      if(kmulrun(kk) < nRSsy(kk)) exit
    end if
  end do
    if(prout) write(66,'(a, 30i3)') '    kmulrun=',kmulrun

  if(kk == kanf0) then
    if(kmulrun(kk) == nRSsy(kk)) return

    seqch(ksq) = seqch(ksq)(1:3)
    kmulrun(kk) = kmulrun(kk) + 1
    goto 20             !              !
  end if
end if

if(kk == 0) return

if(kmulrun(kk) < nRSsy(kk)) then
  kmulrun(kk) = kmulrun(kk) + 1
  write(cnum,'(i3)') kk
  seqch(ksq) = trim(seqch(ksq)) // cnum

  nsd = eqndep(kk)
  n2 = synum(nsd+kmulrun(kk)-1)
    if(prout) write(66,'(4(a,i0),a,a)') 'B:  nsd=',nsd,' kk=',kk,' n2=',n2, &
                 ' kmulrun=',kmulrun(kk),'  seqch(ksq)=',trim(seqch(ksq))
  call chains(kanf0,n2,ksq)
end if

return

end subroutine chains

!########################################################################

subroutine chainseval(ksq1,ksq2)

        !     Copyright (C) 2021-2023  Günter Kanisch

use UR_params,     only: rn,zero
use UR_gleich,     only: eqnum,synum,opnum,ndep,knetto,nab,seqch, &
                         sdformel,SymboleG,ukenn,akenn,N_preset,kcnt,ktime, &
                         krate,IVTL,Symbole,ngrs,nRSsy,RS_SymbolNr,iptr_cnt, &
                         iptr_rate,iptr_time,is_count,nRnetp,RnetparsInd,RS_ops, &
                         RnetParsCrate,RnetPars,RnetParsCRule,symtyp,eqndep,syndep, &
                         nRSsy,Rseite

use CHF,           only: ucase,testSymbol
use Top,           only: IntModA1,LogModA1,CharModA1
use UR_Linft,      only: SumEval_fit

implicit none

integer(4),intent(in)   :: ksq1,ksq2   ! first and last chain number of a range of chains

integer(4)         :: i0a,i0b,i1a,i1b,i2a,i2b,i3a,i3b,i1c,i3c,nc,i,j,klen,kvor,nsd,kk,nn
integer(4)         :: nsd2,k,nkk,nminus,kk1,nsdops,i4,jj
logical            :: arrive60
logical            :: tst,condA,condB,condC,prout,tsy

prout = .false.
   ! prout = .true.

    if(prout) write(66,*) 'Begin ChainsEval:'

if(ksq1 == 1) then
  if(allocated(RnetParsInd)) deallocate(RnetParsInd); allocate(RnetParsInd(ngrs))
  if(allocated(RnetParsCRate)) deallocate(RnetParsCRate); allocate(RnetParsCRate(ngrs))
  if(allocated(RnetPars)) deallocate(RnetPars); allocate(RnetPars(ngrs))
  if(allocated(RnetParsCRule)) deallocate(RnetParsCRule); allocate(RnetParsCRule(ngrs))
  RnetParsInd = 0
  RnetParsCRate = .false.  !  .false.
  RnetPars      = zero
  do jj=1,ngrs
    RnetParsCRule(jj)%s = '  '
  end do
end if

nn = ngrs
if(ksq1 == 1) then
  if(allocated(iptr_time)) deallocate(iptr_time)
  allocate(iptr_time(1));  iptr_time(1) = 0; call IntModA1(iptr_time,nn); iptr_time = 0
  if(allocated(iptr_cnt)) deallocate(iptr_cnt)
  allocate(iptr_cnt(1));   iptr_cnt(1) = 0;  call IntModA1(iptr_cnt,nn); iptr_cnt = 0
  if(allocated(is_count)) deallocate(is_count)
  allocate(is_count(1));   is_count(1) = .false.;  call LogModA1(is_count,nn); is_count =.false.
  if(allocated(iptr_rate)) deallocate(iptr_rate)
  allocate(iptr_rate(1));   iptr_rate(1) = 0;  call IntModA1(iptr_rate,nn); iptr_rate = 0
  nRnetp = 0
  iptr_cnt = 0
  iptr_time = 0
end if

!  iptr_cnt(nvar):  points from a count rate to the associated number of counts
!  iptr_time: points from a count rate to the associated counting duration

 if(prout) write(66,'(2(a,i0))') 'ksq1=',ksq1,' ksq2=',ksq2

    if(ksq2 > ubound(ukenn,dim=1)) call IntModA1(ukenn,ksq2)
    if(ksq2 > ubound(akenn,dim=1)) call IntModA1(akenn,ksq2)
    if(ksq2 > ubound(kcnt,dim=1)) call IntModA1(kcnt,ksq2)
    if(ksq2 > ubound(ktime,dim=1)) call IntModA1(ktime,ksq2)
    if(ksq2 > ubound(krate,dim=1)) call IntModA1(krate,ksq2)

do nc=ksq1,ksq2

  arrive60 = .false.
     if(prout) write(66,*) 'seqch(nc)=',seqch(nc)
  klen = len_trim(seqch(nc))
  if(klen >= 6) read(seqch(nc)(klen-6+1:),*) kvor,kk     ! the last transition kvor --> kk in this chain

          if(prout) write(66,'(2(a,i0))') 'kvor=',kvor,' kk=',kk

  ! Special cases: the distribution types ivtl (4,7,11) of numbers of counts
  if(ivtl(kk) == 4 .or. ivtl(kk) == 7) then
    ukenn(nc) = 1
    kcnt(nc) = kk
         ! write(66,'(a,i0,1x,i0)') 'versuch 14:  kvor,kk=',kvor,kk
    if(nRSsy(kvor) == 2) then
      nsd = eqndep(kvor)
      do j=1,nRSsy(eqnum(nsd))
        if(eqnum(nsd+j-1) /= kvor) exit
        if(synum(nsd+j-1) /= kk) then
          if(RS_ops(kvor,1) == '/') then
            ktime(nc) = synum(nsd+j-1)
            krate(nc) = kvor
            RnetParsCRule(nc)%s = 'A14'
                if(prout) write(66,'(4(a,i0),a,a)') 'A14:  operator=',opnum(nsd+j-2),' nsd=',nsd, &
                               ' j=',j,' nc=',nc,' Eq=',Rseite(nc)%s
            goto 60
          end if
        end if
        if(.false. .and. .not.SumEval_fit) then
          if( j == 1 .and. opnum(nsd+j-1) == 1 ) then
            krate(nc) = kk
            RnetParsCRule(nc)%s = 'A11'
               if(prout) write(66,'(4(a,i0),a,a)') 'A11:  operator=',opnum(nsd+j-1),' nsd=',nsd, &
                                 ' j=',j,' nc=',nc,' Eq=',Rseite(nc)%s
            goto 70
          end if
          if( j == 2 .and. opnum(nsd+j-2) == 1 ) then
            krate(nc) = kk
            RnetParsCRule(nc)%s = 'A11'
               if(prout) write(66,'(4(a,i0),a,a)') 'A11:  operator=',opnum(nsd+j-2),' nsd=',nsd, &
                               ' j=',j,' nc=',nc,' Eq=',Rseite(nc)%s
            goto 70
          end if
        end if
      end do
    end if

  elseif(ivtl(kk) == 11) then
    ukenn(nc) = 2
    ktime(nc) = kk
    if(nRSsy(kvor) == 2) then
      nsd = eqndep(kvor)
      do j=1,nRSsy(eqnum(nsd))
        if(eqnum(nsd+j-1) /= kvor) exit
        if(synum(nsd+j-1) /= kk) then
          kcnt(nc) = synum(nsd+j-1)
          krate(nc) = kvor
          RnetParsCRule(nc)%s = 'A2'
             if(prout) write(66,'(4(a,i0),a,a)') 'A2: kvor=',kvor,' kk=',kk,'  nsd+j-1=',nsd+j-1, &
                          ' krate(nc)=',krate(nc),' Eq=',Rseite(nc)%s

          goto 60
        end if
      end do
    end if
  end if

  if(nRSsy(kvor) >= 2) then
    nsd = eqndep(kvor)
    if(nsd > 0 .and. opnum(nsd) == 4) then     ! with operator '/' in the equation kvor
      if(len_trim(SDformel(kvor)%s) == 0) then    !  no SDformel given for kvor
        if(ucase(symtyp(kk)%s) =='M') then
          krate(nc) = kvor
          kcnt(nc) = kk
          ktime(nc) = RS_SymbolNr(kvor,2)
          goto 60
        end if
        nsd2 = syndep(kk)
        if(nRSsy(kvor) >= 2) then
          i4 = index(SDformel(kk)%s,'/')
          if(i4 == 0) then                 ! without operator '/' in SDformel(kk)
            if(testSymbol(SDformel(kk)%s,SymboleG(kk)%s)) then
              krate(nc) = kvor
              ktime(nc) = RS_SymbolNr(kvor,2)
              kcnt(nc) = kk
              RnetParsCRule(nc)%s = 'A3'             ! SymboleG(kk) is also part of SDformel(kk)
              if(prout) write(66,'(4(a,i0),a,a)') 'A3: kvor=',kvor,' kk=',kk,'  nsd+j-1=',nsd+j-1, &
                                             ' krate(nc)=',krate(nc),' Eq(kvor)=',Rseite(kvor)%s
                           ! goto 60
            end if
          elseif(i4 > 0) then               ! with operator '/' in SDformel(kk)
            if(testSymbol(SDformel(kk)%s(1:i4),SymboleG(kk)%s)) then
              krate(nc) = kk
              RnetParsCRule(nc)%s = 'A4'           ! SymboleG(kk) is also part of SDformel(kk)
                 if(prout) write(66,'(4(a,i0),a,a)') 'A4: kvor=',kvor,' kk=',kk,'  nsd+j-1=',nsd+j-1, &
                                                ' krate(nc)=',krate(nc),' Eq(kvor)=',Rseite(kvor)%s
              kcnt(nc) = 0
              do i=nab+1,ngrs
                if(testSymbol(SDformel(kk)%s(i4:),SymboleG(i)%s)) then
                  ktime(nc) = i
                  exit
                           ! goto 60
                end if
              end do
            end if
          end if
        end if
      else
        ! without operator '/' in the equation kvor;
        do j=1,nRSsy(eqnum(nsd))
          if(synum(nsd+j-1) /= kk) then
            if(synum(nsd+j-1) == RS_SymbolNr(kvor,2) .and. opnum(nsd+j-1) < 3) then
              ktime(nc) = synum(nsd+j-1)
              kcnt(nc) = kk
              krate(nc) = kvor
              RnetParsCRule(nc)%s = 'A5'
                 if(prout) write(66,'(4(a,i0),a,a)') 'A5: kvor=',kvor,' kk=',kk,'  nsd+j-1=',nsd+j-1, &
                                                ' krate(nc)=',krate(nc),' Eq(kvor)=',Rseite(kvor)%s
              exit
                          ! goto 60
            end if
          end if
        end do
      end if
    end if
    if(krate(nc) == 0 .and. nsd > 0 .and. opnum(nsd) == 1) then     ! with operator '-'
      if(nRSsy(eqnum(nsd)) > 2) then
        nminus = 0
        nkk = 0
        do j=1,nRSsy(eqnum(nsd))
          if(opnum(nsd+j-1) == 1) nminus = nminus + 1
          do k=1,nc-1
            if(synum(nsd+j-1) == krate(k)) nkk = nkk + 1
          end do
        end do
        if(nminus == nRSsy(eqnum(nsd))-1  .and. nkk >= 1) then
          krate(nc) = kk
          RnetParsCRule(nc)%s = 'A6'
             if(prout .and. kk <= nab) write(66,'(4(a,i0),a,a)') 'nkk=',nkk,' nminus=',nminus,' krate(nc)=', &
                                         krate(nc),' kk=',kk,' Eq(kk)=',Rseite(kk)%s
        end if
      end if
    end if
  end if
  if(krate(nc) > 0) then
    ! exclude a variable (symbol) as a count rate, if its symbol index is smaller than
    ! knetto (it lies "above" knetto)
    do i=1,knetto(1)-1
      do j=1,nRSsy(i)
        if(krate(nc) == RS_SymbolNr(i,j)) then
          krate(nc) = 0
          ktime(nc) = 0
          kcnt(nc) = 0
          RnetParsCRule(nc)%s = 'A0'
          goto 100
        end if
      end do
    end do
  end if

  if(kcnt(nc) > 0 .and. ktime(nc) > 0 .and. (ucase(symtyp(kk)%s) == 'M' .or. SumEval_fit) ) then
        write(66,'(3(a,i0))') 'goto 60 (sumeval?):    B1: kk=',kk,' kvor=',kvor,'  krate=',krate(nc)
    goto 60
  end if

  goto 70

60   continue
  arrive60 = .true.
  iptr_cnt(kvor) = kcnt(nc)
  iptr_time(kvor) = ktime(nc)
  if(kcnt(nc) > 0) then
    iptr_rate(kcnt(nc)) = krate(nc)
  end if
  iptr_rate(ktime(nc)) = krate(nc)
    if(prout) write(66,'(4(a,i0))') '60: nc=',nc,' kvor=',kvor,' kcnt(nc)=',kcnt(nc), &
                                    ' ktime(nc)=',ktime(nc)

  goto 100

70   continue

  if(len_trim(SDformel(kk)%s) > 0) then
    ! check whether an sd formula exists:

    i0a = index(sdformel(kvor)%s,'(')
    i1a = index(sdformel(kvor)%s,'/')
    i2a = index(sdformel(kvor)%s,')')
    i3a = index(ucase(sdformel(kvor)%s),'SQRT')

    i0b = index(sdformel(kk)%s,'(')
    i1b = index(sdformel(kk)%s,'/')
    i2b = index(sdformel(kk)%s,')')
    i3b = index(ucase(sdformel(kk)%s),'SQRT')

    condA = .false.    ! kvor has two RS-Symbole ("right-hand side of equation)"
                       ! and kk is the first of them

       ! for "B*" nad "C1": see example DWD_sr89_sr90_TDCR_procedure_V2_EN.txp

    if(i0b >= 0 .and. nRSsy(kvor) == 2 .and. kk == RS_SymbolNr(kvor,1)) then
      if(krate(nc) == 0 .and. kvor > 0) then
        nsd = eqndep(kvor)
        if(opnum(nsd) == 1) then
          krate(nc) = kk    !   kvor
          RnetParsCRule(nc)%s = 'B1'    ! following operator of kk is '-'
        end if
      end if
      if(kk /= krate(nc)) kcnt(nc) = kk
      iptr_cnt(kvor) = kcnt(nc)
      iptr_rate(nc) = kvor
      nsd = eqndep(kvor)
      do j=1,nRSsy(eqnum(nsd))
        if(eqnum(nsd+j-1) /= kvor) exit
        if(synum(nsd+j-1) /= kk) then
          ktime(nc) = synum(nsd+j-1)
          iptr_time(kvor) = ktime(nc)
          condA = .true.
          exit
        end if
      end do
    end if
    if(prout) write(66,'(4(a,i0),a,a)') 'B1: kvor=',kvor,' kk=',kk,'  nsd+j-1=',nsd+j-1, &
                       ' krate(nc)=',krate(kvor),' Eq=',Rseite(kvor)%s

    condB = .false.       ! SDFormel(kk) contains sqrt;
    if(i3b > 0 .and. testSymbol(SDformel(kk)%s,SymboleG(kk)%s)) then
      if(i1b == 0) then
        ukenn(nc) = 1
        kcnt(nc) = kk
      end if
      if(i1b > 0 .and. index(sdformel(kk)%s,SymboleG(kk)%s) < i1b) then
        if(N_preset) then
          ukenn(nc) = 2
          ktime(nc) = kk
          iptr_rate(ktime(nc)) = kvor
          condB = .true.
        else
          ukenn(nc) = 1
          if(krate(nc) == 0 .or. (RnetParsCRule(nc)%s == 'A3')) then
            if(krate(nc) == 0) then
              krate(nc) = kk
              RnetParsCRule(nc)%s = 'B2'
            end if
            if(kk <= nab) then
              nsd2 = eqndep(kk)
              if(nsd2 > 0) then
                kk1 = synum(nsd2)
              end if
            else
              kk1 = kk
            end if
            if(len_trim(SDformel(kk1)%s) > 0) then
              nsdops = 0
              do i=1,len_trim(SDformel(kk1)%s)
                if(scan(SDformel(kk1)%s,'-+/*') > 0) nsdops = nsdops + 1
              end do
            end if
            if(nsdops <= 3) then
              i1c = index(ucase(sdformel(kk1)%s),'/')
              i3c = index(ucase(sdformel(kk1)%s),'SQRT')
              if(i3c > 0 .and. i1c > i3c) then
                if(testSymbol(SDformel(kk1)%s(1:i1c),SymboleG(kk1)%s)) then
                  krate(nc) = kk1
                  RnetParsCRule(nc)%s = 'B3'
                  do i=nab+1,ngrs
                    if(testSymbol(SDformel(kk1)%s(i1c:),SymboleG(i)%s)) then
                      ktime(nc) = i
                      iptr_time(nc) = i
                      iptr_rate(i) = krate(nc)
                      condB = .true.
                      exit
                    end if
                  end do
                end if
              end if
             end if
          end if
        end if
      end if
      do i=nab+1,ngrs
        tst = testSymbol(' ' // SDformel(kk)%s(max(1,i1b):) // ' ',Symbole(i)%s)
        if(tst) then
          ktime(nc) = i
          iptr_time(kk) = ktime(nc)
          condB = .true.
          exit
        end if
      end do
    end if
             ! write(66,'(4(a,i0))') 'B2: kvor=',kvor,' kk=',kk,'  nsd+j-1=',nsd+j-1, &
             !                  ' krate(nc)=',krate(nc)

    condC = .false.     ! Symbol kvor is contained in SDFormel(kvor)
    if(i3a > 0 .and. testSymbol(SDformel(kvor)%s,SymboleG(kvor)%s)) then
      if(i0a == 0) akenn(nc) = 1          ! sqrt(R)
      if(.not.N_preset) then
        if(i0a > 0 .and. index(ucase(sdformel(kvor)%s),SymboleG(kvor)%s) < i0a ) then
          akenn(nc) = 2           ! sqrt(t**2/N)   'N_preset=T'
          condC = .true.
        end if
      else
        if(i0a > 0 .and. index(ucase(sdformel(kvor)%s),SymboleG(kvor)%s) < i0a ) then
          akenn(nc) = 3           ! sqrt(R**2/N)   'N_preset=T'
          condC = .true.
        end if
      end if
    end if
  end if
  if(prout)  write(66,'(4(a,i0),a,a)') 'B3: kvor=',kvor,' kk=',kk,'  nsd+j-1=',nsd+j-1, &
                      ' krate(nc)=',krate(nc),' Eq(kvor)=',Rseite(kvor)%s

  if(.not.condA .and. .not.condB .and. .not.condC) then
    krate(nc) = 0
    akenn(nc) = 0
    kcnt(nc) = 0
    ktime(nc) = 0
    if(nc > ubound(RnetParsCRule,dim=1)) call CharModA1(RnetParsCRule,nc)
    RnetParsCRule(nc)%s = 'B0'
  end if

100  continue
        ! if(prout) write(66,'(4(a,i0),a,a)') 'A1(behind 100): kvor=',kvor,' kk=',kk,'  nsd+j-1=',nsd+j-1, &
        !                             ' krate(nc)=',krate(nc),' Eq=',Rseite(nc)%s
  if(nc > 1) then
    if(krate(nc) == 0 .and. krate(nc-1) > 0) then
      ! If the predecessor in the equation is the gross count rate, the second
      ! term, if subtracted with '-', must also be a count rate.
      nsd = eqndep(kvor)
      do i=1,nRSsy(eqnum(nsd))
        if(nsd+i-1 > ndep) exit
            if(nsd+i-1+1 > ndep) exit
        if(synum(nsd+i-1) == krate(nc-1)) then
          if(synum(nsd+i-1+1) == kk .and. opnum(nsd+i-1) == 1 .and. opnum(nsd+i-1+1) <= 1) then
            krate(nc) = kk
            RnetParsCRule(nc)%s = 'C1'
            exit
          end if
        end if
      end do
    end if
    if(krate(nc) == 0) then
      do i=1,nc-1
        if(krate(i) == 0) cycle
        nsd = eqndep(krate(i))
        nsd2 = eqndep(kvor)
        if(nsd == 0 .or. nsd2 == 0) cycle
        if(kvor == eqnum(nsd) .and. opnum(nsd) == 1 .and. opnum(nsd2) <= 1 ) then
          krate(nc) = kvor
          RnetParsCRule(nc)%s = 'C2'
          exit
        end if
      end do
    end if
  end if

  if(krate(nc) > 0) then
    if(kcnt(nc) > 0 .and. ktime(nc) > 0 ) then
      if(iptr_rate(kcnt(nc)) == 0) iptr_rate(kcnt(nc)) = krate(nc)
      if(iptr_rate(ktime(nc)) == 0) iptr_rate(ktime(nc)) = krate(nc)
    end if
    if(RnetParsCRule(nc)%s == 'B1' .and. kcnt(nc) == ktime(nc)) then
      iptr_time(kk) = 0
      ktime(nc) = 0
      iptr_cnt(kk) = kcnt(nc)
    end if
    if(RnetParsCRule(nc)%s == 'C1' .and. kcnt(nc) == ktime(nc)) then
      iptr_time(kk) = 0
      ktime(nc) = 0
      iptr_cnt(kk) = kcnt(nc)
    end if
    call RnetParsNew(krate(nc),.true.)

    if(nc == ksq1) write(66,'(A)') ' nc   kvor   kk     ukenn akenn kcnt ktime krate rule Symbol         chain'
       if(ukenn(nc)+akenn(nc)+kcnt(nc)+ktime(nc)+krate(nc) > 0)   &
    write(66,'(8(i3,3x),a,a, a,a,T72,a,2x,a,a,i3)') nc,kvor,kk,ukenn(nc),akenn(nc),kcnt(nc),ktime(nc), &
                krate(nc),'  ',RnetParsCRule(nc)%s,'  ',Symbole(krate(nc))%s, trim(seqch(nc)),'true', &
                ' nRnetp=',nRnetp
  else
    call RnetParsNew(kk,.false.)
    if(nc == ksq1) write(66,'(A)') ' nc   kvor   kk     ukenn akenn kcnt ktime krate rule Symbol         chain'
       if(ukenn(nc)+akenn(nc)+kcnt(nc)+ktime(nc)+krate(nc) > 0)   &
    write(66,'(8(i3,3x),a,a, T72,a,2x,a,a,i3)') nc,kvor,kk,ukenn(nc),akenn(nc),kcnt(nc),ktime(nc), &
                 krate(nc),'  ',RnetParsCRule(nc)%s,trim(seqch(nc)),'false',' nRnetp=',nRnetp
  end if

end do

end subroutine chainseval

!########################################################################

subroutine RnetParsNew(krate,isRate)

        !     Copyright (C) 2021-2023  Günter Kanisch

use UR_Gleich,       only: nRnetp,RnetParsCRate,RnetParsInd
implicit none

integer(4),intent(in)     :: krate
logical,intent(in)        :: isRate

integer(4)           :: j

                 if(krate == 0) return
if(nRnetp == 0) then
  nRnetp = 1
  RnetParsInd(1) = krate
  RnetParsCRate(1) = isRate
else
  j = findloc(RnetParsind,krate,dim=1)       !  ,k1,dim=1)
  if(j == 0) then
    nRnetp = nRnetp + 1
    RnetParsInd(nRnetp) = krate         ! *kfaktor
    RnetParsCRate(nRnetp) = isRate
  end if
end if
     ! if(prout)
     ! write(66,*) 'RnetParsInd=',(RnetParsInd(j),j=1,nRnetp)
     ! write(66,*) 'RnetParsCRate=',(RnetParsCRate(j),j=1,nRnetp)

end subroutine RnetParsNew

!########################################################################
