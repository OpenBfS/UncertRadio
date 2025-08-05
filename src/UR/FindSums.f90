
module Fsum
    use UR_types, only: rn

    private
    public :: FindSums, FindBrackets

contains


    subroutine FindSums(Instr, kst, sumTermStr)

        !     GK, 3.8.2025
        !     This routine locates in Instr a sum of additive terms, which are enclosed by brackets;
        !     only the innermost bracket pair is considered. The additive terms should have the same
        !     units, which will be tested later (in CalcUnits).
        !
        !     Required routines: FindSumPair and FindBrackets.

        use UR_Gleich_globals,      only: charv

        implicit none

        type(charv),intent(in)  :: Instr          ! rh side string of an UR equation
        integer,intent(out)     :: kst            ! number of additive terms
        type(charv),intent(out) :: sumTermStr(10) ! formula strings of additive terms


        integer     :: kst1
        type(charv) :: strPair(2)    ! strPair(1): term to the left (strPair(1)) or to the
        type(charv) :: WorkStr

        kst = 0
        WorkStr%s = Instr%s

        do

            call FindSumPair(WorkStr,kst1,strPair)
            ! strPair(1) cannot not contain further '+' or '-', because the search for '+-'
            ! always starts at the left side of WorkStr;
            ! strPair(2) may contain further '+' or '-'.

            if(kst1 == 0) exit
            if(kst1 == 2) then
                kst = kst + 1
                sumTermStr(kst)%s = strPair(1)%s        ! store to sumTermStr
                if(Scan(strPair(2)%s,'+-') > 0) then
                    WorkStr%s = strPair(2)%s            ! needs further testing for '+-'
                else
                    kst = kst + 1
                    sumTermStr(kst)%s = strPair(2)%s    ! store to sumTermStr
                    exit
                end if
            end if

        end do

    end subroutine FindSums

!######################################################################################

    subroutine FindBrackets(Instr,klp,bropen,brclose)
    !     GK, 3.8.2025

        use UR_Gleich_globals,    only: charv
        use Top,                  only: IntModA1

        implicit none

        type(charv),intent(in)  :: Instr                    ! string to be analyzed
        integer(4),intent(out)  :: klp                      ! number of bracket pairs
        integer(4),allocatable,intent(out)  :: bropen(:)    ! position index of '('
        integer(4),allocatable,intent(out)  :: brclose(:)   ! position index of ')'


        integer(4)    :: kkp,klplast,j,jjj
        logical       :: kopen

        allocate(bropen(1),brclose(1))

        ! find all pairs of opening and closing brackets: bropen(klp), brclose(klp)
        klp = 0
        kkp = 0
        kopen = .false.
        bropen = 0
        brclose = 0
        klplast = 0
        klp = 1
        bropen(1) = 0
        brclose(1) = len_trim(Instr%s) + 1
        do
            if(index(Instr%s, '(') == 0) exit

            do j=1,len_trim(Instr%s)
                if(.not. kopen .and. Instr%s(j:j) == '(') then
                    jjj = findloc(bropen,j,dim=1)
                    if(jjj > 0) cycle
                    klp = klp + 1
                    if(klp > ubound(bropen,dim=1)) then
                        call IntModA1(bropen,klp)
                        call IntModA1(brclose,klp)
                    end if
                    kopen = .true.
                    bropen(klp) = j
                    cycle
                end if
                if(kopen .and. kkp == 0.and. Instr%s(j:j) == ')') then
                    jjj = findloc(brclose,j,dim=1)
                    if(jjj > 0) cycle
                    brclose(klp) = j
                    kopen = .false.
                    exit
                end if
                if(kopen .and. Instr%s(j:j) == '(') then
                    jjj = findloc(bropen,j,dim=1)
                    if(jjj > 0) cycle
                    kkp = kkp + 1
                    cycle
                end if
                if(kopen .and. Instr%s(j:j) == ')' .and. kkp > 0) then
                    jjj = findloc(brclose,j,dim=1)
                    if(jjj > 0) cycle
                    kkp = kkp - 1
                    cycle
                end if
            end do
            if(klp > klplast) then
                klplast = klp
                cycle
            else
                exit
            end if
        end do
        !  if(klp > 0) write(171,'(a,3x,10(i4,i4))') RSideU(2)%s,(bropen(j),brclose(j),j=1,klp)
        if(klp > 0) write(66,'(a,3x,10(i4,i4))') Instr%s,(bropen(j),brclose(j),j=1,klp)

    end subroutine FindBrackets

!######################################################################################

    subroutine FindSumPair(Instr,kst,strPair)
!     GK, 3.8.2025

        ! use UR_types,       only: eps1min,rn,zero,one,two,pi
        use UR_Gleich_globals,      only: charv
        use Top,            only: CharModA2,IntModA2

        implicit none

        type(charv),intent(in)        :: Instr        ! rh side string of an UR equation
        integer,intent(out)           :: kst          ! number of additive terms
        type(charv),intent(out)       :: strPair(2)   ! strPair(1): term to the left (strPair(1)) or to the
        ! right (strPair(2) of '+' or '-'
        integer(4)                    :: ngopsi       ! numbver of operators
        character(len=1),allocatable  :: opsi(:)      ! array of operator characters
        integer(4),allocatable        :: opsind(:)    ! array of operator position indexes

        integer(4)              :: j
        integer(4)              :: klp          ! number of bracket pairs
        integer(4),allocatable  :: bropen(:)    ! position index of '('
        integer(4),allocatable  :: brclose(:)   ! position index of ')'

        integer(4),allocatable   :: nopj(:)
        type(charv),allocatable  :: opsj(:,:)
        integer(4),allocatable   :: opsjind(:,:)

        integer(4)    :: ii,m,jj
        logical       :: prout2

        prout2 = .false.
        prout2 = .true.

        write(66,*) 'FindSumPair: Instr=',Instr%s
        kst = 0

        allocate(opsi(20),opsind(20))

        ngopsi = 0
        opsi = ''
        ! Identify mathematical operators in the equation (version RSideU(1)):
        do j=1,len_trim(Instr%s)
            if(Scan(Instr%s(j:j),'+-*/^') > 0) then
                ngopsi = ngopsi + 1
                opsi(ngopsi) = Instr%s(j:j)
                opsind(ngopsi) = j        ! position within the string Instr
            end if
        end do

        call FindBrackets(Instr,klp,bropen,brclose)

        !...................................
        kst = 0
        if(allocated(nopj)) deallocate(nopj)
        if(allocated(opsj)) deallocate(opsj)
        if(allocated(opsjind)) deallocate(opsjind)
        allocate(nopj(klp), opsj(klp,1), opsjind(klp,1))
        nopj = 0
        do ii=1,klp
            opsj(ii,1)%s = ' '
        end do
        opsjind = 0
        ! write(66,*) 'klp=',int(klp,2)

        do m=1,ngopsi
            if(Scan(opsi(m),'+-') == 0) cycle
            jj = 0
            do j=1,klp
                if(opsind(m) > bropen(j) .and. opsind(m) < brclose(j)) jj = j
            end do
            ! Now j indicates the innermost bracket pairs encompassing the m-th operator
            if(jj > 0) j = jj
            nopj(j) = nopj(j) + 1
            !write(66,*) ' ubound(opsj,dim=2)=',ubound(opsj,dim=2),' j=',int(j,2),' nopj(j)=',int(nopj(j))
            !write(66,*) ' ubound(opsj,dim=1)=',ubound(opsj,dim=1),' j=',int(j,2),' nopj(j)=',int(nopj(j))
            if(nopj(j) > ubound(opsj,dim=2)) then
                call charModA2(opsj,klp,nopj(j))
                call IntModA2(opsjind,klp,nopj(j))
            end if
            opsj(j,nopj(j))%s = opsi(m)
            opsjind(j,nopj(j)) = opsind(m)
            !write(66,*) 'j(klp)=',int(j,2),' m=',int(m,2),' opsj(j,nopj(j))%s=',opsj(j,nopj(j))%s, &
            !              '  opsjind(j,nopj(j))=',opsjind(j,nopj(j))
            if(scan(opsj(j,nopj(j))%s,'+-') > 0) then
                kst = kst + 1
                StrPair(kst)%s = Instr%s(bropen(j)+1:opsjind(j,1)-1)
                if(prout2) write(66,'(a,i2,a,a)') 'sumterms: kst=',int(kst,2),' StrPair(kst)=',StrPair(kst)%s
                kst = kst + 1
                StrPair(kst)%s = Instr%s(opsjind(j,1)+1:brclose(j)-1)
                if(prout2) write(66,'(a,i2,a,a)') 'sumterms: kst=',int(kst,2),' StrPair(kst)=',StrPair(kst)%s
                if (kst == 2) exit
            end if

        end do
        !...................................

    end subroutine FindSumPair


end module Fsum
