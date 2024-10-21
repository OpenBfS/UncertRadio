!-------------------------------------------------------------------------------------------------!
! this file is part of uncertradio.
!
!    uncertradio is free software: you can redistribute it and/or modify
!    it under the terms of the GNU general public license as published by
!    the free software foundation, either version 3 of the license, or
!    (at your option) any later version.
!
!    uncertradio is distributed in the hope that it will be useful,
!    but without any warranty; without even the implied warranty of
!    merchantability or fitness for a particular purpose.  see the
!    GNU general public license for more details.
!
!    you should have received a copy of the GNU general public license
!    along with uncertradio. if not, see <http://www.gnu.org/licenses/>.
!
!-------------------------------------------------------------------------------------------------!

! contains:
!    CalcUnits
!    UnitFind
!    Function_arg_resolve
!    locate_func
!    genstrings
!    generateAllBinaryStrings
!    Save_Ucheck
!    Restore_Ucheck
!    Report_Ucheck
!-------------------------------------------------------------------------------------------------!

subroutine CalcUnits()

    ! performs as a test the calculation of units of dependent quantities as function
    ! of the units of independent quantities. It includes scaling of non-basic units
    ! to basic units (e.g., from min to s; g to kg)
    ! Called by Rechw1 or ProcMenu.
    ! See chapter 7.21 of the UncertRadio CHM Help file for more details.

    !   Copyright (C) 2021-2024  Günter Kanisch

    use, intrinsic :: iso_c_binding,  only: c_int
    use UR_params,    only: EPS1MIN
    use UR_gleich,    only: nRSsy, nab, RS_SymbolNr, ngrs, ifehl, Messwert, Stdunc, ncov, &
                            einheit, einheitSV, apply_units, RSeite, symboleG, charv, kEGr, &
                            klinf, kfitcal, kgspk1, FP_for_units, uconv, Symbole, &
                            ksumeval, UU, nu_other, Formelt,unit_other, Unit_basis, &
                            PUnitMsg, npMsg, unit_conv_fact, einheit_conv, unit_conv_factSV, &
                            MesswertSV, SDWert, HBreite, missingval, IAR, MesswertSVUCH, &
                            Formeltext, retain_triggers
    use UR_VARIABLES, only: Gum_restricted, batest_user, fname
    use UR_Linft,     only: FitDecay, FitCalCurve, SumEval_fit
    use UR_Gspk1Fit,  only: Gamspk1_Fit
    use CHF,          only: ucase, StrReplace, intStr, FindlocT
    use Rout,         only: WTreeViewPutStrCell
    use fparser,      only: initf, parsef, evalf
    use Top,          only: CharModA1, IntModA1, idpt
    use xx,           only: ifehlxx
    use uwb,          only: resulta
    use file_io,      only: logger
    use gtk,          only: gtk_widget_set_visible
    use UR_types
    use translation_module, only: T => get_translation

    implicit none

    logical            :: prout,prout2
    integer            :: I,k,nng,i1,ileng,j,jk,nv,i3_1,i3_2,ij,j2,m
    integer            :: i2,jj,ngopsi,jpl,nnc,nwk,i5,n21,itt,jmax,klplast,nopj
    integer            :: i33,i2a,i2b,i2c,npw(6),k1,k2,kper,nf,i1arr(10),i2arr(10),i3arr(10),opsind(30)
    integer            :: i5arr(10),i6arr(10),i7arr(10),nfp,bropen(20),brclose(20),klp,jjj,kkp
    integer            :: opsjind(20),ios,idummy
    integer            :: ib,ib1,ib2,iblen,kk,ie
    real(rn)           :: dummy,Evalue,testvor,dpa,help

    real(rn)           :: seval,xevalf,fff,fdummy,fd_with,fd_without,ucv,Fv1,Fv2
    integer, allocatable   :: arr2dim(:,:)

    logical            :: apply_SV,openbr,divk,insideExp(20),pureSum,ucdone(200),opsinsideExp(30),opsinsidePow(30)
    logical            :: exp1dif,kopen,pureprod
    character(len=40)  :: cnum       !,RSeiteOrg
    real(rn)           :: xvar(20),dpi(20),factor,EvalFactor,Evalue_red
    character(len=30)  :: nvar(20)
    character(len=1)   :: opsi(30),lastop,prop,opsj(20)
    character(len=512) :: log_str

    type(charv),allocatable   :: RseiteG(:),EinhSymb(:),EinhSymbG(:),EinheitWK(:),ceinhwk_v(:),ceinhwks_v(:)
    type(charv),allocatable   :: EinheitWK_v(:)
    integer, allocatable      :: jj_v(:)

    real(rn),allocatable      :: uconv_v(:), EinhVal(:),zEinhVal(:)
    type(charv)               :: Rseite_CV
    character(len=:),allocatable  :: strg1,RScopy,strgv1,strgv3,str6,Einvor,cdum,ceinhwk,ceinhwi, &
        ceinhwks,str5,RSeiteOrg

    if(.not.apply_units) return
    if(ubound(einheit,dim=1) == 0) then
        ifehl = 1
        return
    endif
    prout = .false.
    ! prout = .true.
    prout2 = .false.
    ! prout2 = .true.

    allocate(character(len=10)  :: strg1,strgv1,strgv3,str5,str6,RScopy,einvor,cdum,ceinhwk, &
        ceinhwi,ceinhwks,RSeiteOrg)

    if(allocated(einheitSV)) deallocate(einheitSV)
    allocate(einheitSV(ubound(einheit,dim=1)))
    EinheitSV(1:ngrs+ncov) = Einheit(1:ngrs+ncov)    !  31.1.2024
    allocate(EinhVal(ngrs+ncov),zEinhVal(ngrs+ncov))  ! 17.9.2024, GK

    call logger(66, 'Calculating measurement units of dependent variables:')

    call CharModA1(RSeiteG,3)
    call CharModA1(EinhSymb,UU%nSymb)
    call CharModA1(EinhSymbG,UU%nSymb)
    call CharModA1(EinheitWK,ngrs)

    call CharModA1(ceinhwk_v,1)
    call CharModA1(ceinhwks_v,1)
    call CharModA1(EinheitWK_v,1)
    call IntModA1(jj_v,1)
    nwk = 0

    if(allocated(uconv)) deallocate(uconv)
    allocate(uconv(ngrs))
    uconv = 0.00000001_rn

    if(allocated(uconv_v)) deallocate(uconv_v)
    allocate(uconv_v(ngrs))
    uconv_v = 0.0_rn

    if(allocated(PUnitMsg)) deallocate(PUnitMsg)
    allocate(PUnitMsg(1))

    npMsg = 0
    ucdone = .false.

    do i=1,ngrs
        if(i > nab) then
            Messwert(i) = MEsswert(i) * unit_conv_fact(i)
            StdUnc(i) = StdUnc(i) * unit_conv_fact(i)
            if(IAR(i) == 1) then
                if(abs(SDwert(i)-missingval) > EPS1MIN) SDWert(i) = SDWert(i) * unit_conv_fact(i)
                ! note: relative values do not change by applying a conversion factor!
            end if
            if(abs(HBreite(i)-missingval) > EPS1MIN) HBreite(i) = HBreite(i) * unit_conv_fact(i)
            einheit(i)%s = einheit_conv(i)%s
            unit_conv_fact(i) = 1.0_rn
        end if
!         if(prout) write(66,'(a,i3,2x,a,a,es12.5,2x,es12.5,2x,a)') 'i=',i,Symbole(i)%s,' ucf=', &
!             unit_conv_fact(i),unit_conv_factSV(i),einheit(i)%s
        if(prout)  then
            write(log_str, '(a,i3,2x,a,a,es12.5,2x,es12.5,2x,a)') 'i=',i,Symbole(i)%s,' ucf=', &
            unit_conv_fact(i),unit_conv_factSV(i),einheit(i)%s
            call logger(66, log_str)
        end if
    end do
    do i=nab,1,-1
        Rseite_CV%s = Rseite(i)%s
        Messwert(i) = ResultA(i)
        unit_conv_fact(i) = Messwert(i) / MesswertSVUCH(i)
        StdUnc(i) = StdUnc(i) * unit_conv_fact(i)
!         write(66,'(a,i3,2x,a,2(a,es12.5))') 'i=',i,Symbole(i)%s,' Messwert_new=',Messwert(i), &
!             ' ucf=',unit_conv_fact(i)
        write(log_str, '(a,i3,2x,a,2(a,es12.5))') 'i=',i,Symbole(i)%s,' Messwert_new=',Messwert(i), &
            ' ucf=',unit_conv_fact(i)
        call logger(66, log_str)
    end do

    call initf(nab)

    do i=1,UU%nSymb
        EinhSymb(i)%s = UU%EinhSymb(i)%s
        EinhVal(i)  = UU%EinhVal(i)
        EinhSymbG(i)%s = trim(ucase(EinhSymb(i)%s))
    end do

    apply_SV = apply_units  ! must be a local variable, NOT applyunitsSV!
    FP_for_units = .true.

    do i=nab,kEGr,-1
        ! top-down loop over the equations:
        if(Fitdecay .and. i == klinf) cycle
        if(FitCalCurve .and. i == kfitcal) cycle
        if(Gamspk1_Fit .and. i == kgspk1) cycle
        if(SumEval_fit .and. i == ksumeval) then
            nng = RS_SymbolNr(i,3)
            EinheitWK(i)%s = trim(ucase(Einheit(nng)%s))
!             if(prout) write(66,*) 'i=',int(i,2),' ksumEval:  unit=',EinheitWK(i)%s
            if(prout)  then
                write(log_str, '(*(g0))') 'i=',int(i,2),' ksumEval:  unit=',EinheitWK(i)%s
                call logger(66, log_str)
            end if
            call WTreeViewPutStrCell('treeview1', 4, i, Einheit(i)%s)
            call WTreeViewPutStrCell('treeview2', 4, i, Einheit(i)%s)
            uconv(i) = uconv(i+1)
            cycle
        endif
        nng = RS_SymbolNr(i,1)     ! symbol number of the first symbol occurring in the right-hand side of Eq. i
        RseiteG(1)%s = trim(ucase(RSeite(i)%s))     ! uppercase (G) version of the right-hand side of Eq. i
        Rseite_CV%s = Rseite(i)%s
        !--cc   2.2.2024:
        if(.not.retain_triggers) then
            do
                do kk= 1,2
                    if(kk == 1) ib1 = index(RseiteG(1)%s,'KILO_TRIGGER')
                    if(kk == 2) ib1 = index(RseiteG(1)%s,'MIN_TRIGGER')
                    if(kk == 1) iblen = 12
                    if(kk == 2) iblen = 11
                    if(ib1 > 1) then
                        do ib=ib1,1,-1
                            if(RseiteG(1)%s(ib:ib) == '*' .or. RseiteG(1)%s(ib:ib) == '/') then
                                ib2 = ib - 1
                                exit
                            end if
                        end do
                        if(ib2 >= 1 .and. ib2 < ib1) then
                            ie = index(Formeltext(i)%s,'=')
                            RseiteG(1)%s = RseiteG(1)%s(1:ib2) // RseiteG(1)%s(ib1+iblen:)
                            Rseite_CV%s = Rseite_CV%s(1:ib2) // Rseite_CV%s(ib1+iblen:)
                            Formeltext(i)%s = Formeltext(i)%s(1:ie) // ' ' // Rseite_CV%s
                        endif
                    end if
                end do
                if(ib1 == 0) exit
            end do
        end if
        !--cc

        call StrReplace(RseiteG(1)%s,'**','^',.true.,.false.)
        RSeiteOrg = RseiteG(1)%s

        RseiteG(2)%s = RseiteG(1)%s
        ! try to remove special functions if their arguments have the unit 1, store in RseiteG(2)
        call Function_arg_resolve(i,prout,RSeiteG(1)%s,'RseiteG(1)','LOG',0)
        RSeiteG(2)%s = RSeiteG(1)%s

        ngopsi = 0
        opsi = ''
        ! Identify mathematical operators in the equation (version RSeiteG(1)):
        do j=1,len_trim(RSeiteG(1)%s)
            if(Scan(RSeiteG(1)%s(j:j),'+-*/^') > 0) then
                ngopsi = ngopsi + 1
                opsi(ngopsi) = RSeiteG(1)%s(j:j)
                opsind(ngopsi) = j        ! position within the string RseiteG
            end if
        end do
        RSeiteG(2)%s = RseiteG(1)%s

        if(prout)  then
            write(log_str, '(*(g0))')
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'RSeiteG(1)=',RSeiteG(1)%s
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'RSeiteG(2)=',RSeiteG(2)%s
            call logger(66, log_str)

            write(log_str, '(*(g0))') 'Anzahl ops: ',int(ngopsi,2),'  opsi=',(opsi(k),' ',k=1,ngopsi)
            call logger(66, log_str)
        end if

        call locate_func(RSeiteG(1)%s,'EXP',nf,i1arr,i2arr,i3arr)
        call locate_func(RSeiteG(1)%s,'^',nfp,i5arr,i6arr,i7arr)

        do k=1,nRSsy(i)          ! nRSsy(i): number of symbols in th r.h. side of Eq. i
            nng = RS_SymbolNr(i,k)
            i3_1 = index(RseiteG(1)%s,SymboleG(nng)%s)
            i3_2 = index(RseiteG(2)%s,SymboleG(nng)%s)
            insideExp(k) = .false.
            if(nf > 0) then
                do j=1,nf
                    if(i3_1 > i2arr(j) .and. i3_1 < i3arr(j)) insideExp(k) = .true.
                enddo
            end if
        enddo

        ! find pairs of opening and closing brackets: bropen(klp), brclose(klp)
        klp = 0
        kkp = 0
        kopen = .false.
        klplast = 0
        do
            do j=1,len_trim(RSeiteG(2)%s)
                if(.not. kopen .and. RSeiteG(2)%s(j:j) == '(') then
                    jjj = findloc(bropen,j,dim=1)
                    if(jjj > 0) cycle
                    klp = klp + 1
                    kopen = .true.
                    bropen(klp) = j
                    cycle
                end if
                if(kopen .and. kkp == 0.and. RSeiteG(2)%s(j:j) == ')') then
                    jjj = findloc(brclose,j,dim=1)
                    if(jjj > 0) cycle
                    brclose(klp) = j
                    kopen = .false.
                    exit
                end if
                if(kopen .and. RSeiteG(2)%s(j:j) == '(') then
                    jjj = findloc(bropen,j,dim=1)
                    if(jjj > 0) cycle
                    kkp = kkp + 1
                    cycle
                end if
                if(kopen .and. RSeiteG(2)%s(j:j) == ')' .and. kkp > 0) then
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
        ! if(klp > 0) write(171,'(a,3x,10(i4,i4))') RseiteG(2)%s,(bropen(j),brclose(j),j=1,klp)

        do j=1,klp
            nopj = 0
            do m=1,ngopsi
                if(opsind(m) > bropen(j) .and. opsind(m) < brclose(j)) then
                    nopj = nopj + 1
                    opsj(nopj) = opsi(m)
                    opsjind(nopj) = opsind(m)
                end if
            end do
        end do

        opsinsideExp = .false.
        opsinsidePow = .false.
        exp1dif = .false.         ! does the exp-argument contain a difference 1 - exp(...)?
        do k=1,ngopsi
            do j=1,nf
                if(opsind(k) > i2arr(j) .and. opsind(k) < i3arr(j)) opsinsideExp(k) = .true.
            enddo
            do j=1,nfp
                if(opsind(k) > i6arr(j) .and. opsind(k) < i7arr(j)) opsinsidePow(k) = .true.
            enddo
            if(opsi(k) == '-') then
                exp1dif = .false.
                if(index(RseiteG(2)%s,'EXP') > 0) then
                    jmax = len_trim(RSeiteG(2)%s)
                    do j=opsind(k),opsind(k)+10
                        if(RSeiteG(2)%s(j:min(j+2,jmax)) == 'EXP') then
                            do j2=opsind(k),max(1,opsind(k)-5),-1
                                if(RSeiteG(2)%s(j2:j2) == '(') then
                                    if(index(RSeiteG(2)%s(j2+1:opsind(k)),'1') > 0) then
                                        exp1dif = .true.   ! contains the expression: (1. - exp(...))
                                        exit
                                    end if
                                end if
                            enddo
                            if(exp1dif) exit
                        end if
                    end do
                end if
                if(.not.opsinsidePow(k) .and. .not.opsinsideExp(k) .and. .not.exp1dif) then
                    RseiteG(2)%s(opsind(k):opsind(k)) = '+'
                end if
            end if
        end do
!         if(prout) write(66,*) 'RSeiteG(2)=',RSeiteG(2)%s,'   exp1dif=',exp1dif
        if(prout)  then
            write(log_str, '(*(g0))') 'RSeiteG(2)=',RSeiteG(2)%s,'   exp1dif=',exp1dif
            call logger(66, log_str)
        end if

        strg1 = RSeiteG(2)%s
        strgv1 = RSeiteG(2)%s
        strgv3 = RSeiteG(2)%s

        ceinhwi = ''
        opsi = ''
        pureSum = .true.
        pureProd = .true.
        do j=1,ngopsi
            if(opsi(j) /= '+' .and. opsi(j) /= '-') puresum = .false.
            if(opsi(j) == '+' .or. opsi(j) == '-') pureprod = .false.
        end do
        n21 = 0  ! for time basis

        do k=1,nRSsy(i)

            nng = RS_SymbolNr(i,k)

!             write(66,*) 'Symbol i=',int(i,2),'  subsymbol nng=',int(nng,2)
            write(log_str, '(*(g0))') 'Symbol i=',int(i,2),'  subsymbol nng=',int(nng,2)
            call logger(66, log_str)

            i3_1 = index(RseiteG(1)%s,SymboleG(nng)%s)
            i3_2 = index(RseiteG(2)%s,SymboleG(nng)%s)
            ileng = len_trim(SymboleG(nng)%s)
            do j=i3_2,1,-1
                if(scan(RseiteG(2)%s(j:j),'*/+') > 0) then
                    opsi(k) = RseiteG(2)%s(j:j)
                    if(opsi(k) /= '+') pureSum = .false.
                    exit
                end if
            enddo

            divk = .false.
            i33 = index(RseiteOrg,SymboleG(nng)%s)
            if(i33 > 1) then
                do jk=i33-1,1,-1
                    if(scan(RseiteOrg(jk:jk),'+-*/') > 0) then
                        if(RseiteOrg(jk:jk) /= '/') then
                            exit
                        elseif(RseiteOrg(jk:jk) == '/') then
                            divk = .true.
                        endif
                    endif
                end do
            endif

            ceinhwk = ' '        ! String with the unit of the k-th symbol, with (numer.) numbers
            ceinhwks = ' '       ! String with the unit of the k-th symbol, with variables EinhSymb
            nnc = 0
            i1 = 0
            lastop = ''

            EinheitWK(nng)%s = trim(ucase(Einheit(nng)%s))
            do j=1,nu_other
                if(trim(EinheitWK(nng)%s) == trim(ucase(unit_other(j)))) then
                    EinheitWK(nng)%s = trim(ucase(unit_basis(j)))
                end if
            end do
            if(len_trim(EinheitWK(nng)%s) == 0) EinheitWK(nng)%s = '1'

            if(ucdone(nng)) then
                i5 = findlocT(EinheitWK_v, EinheitWK(nng)%s, 1)
                if(i5 > 0) then
                    ceinhwk = ceinhwk_v(i5)%s
                    ceinhwks = ceinhwks_v(i5)%s
                    jj = jj_v(i5)
                    if(einhVal(jj_v(i5)) >= 1.0_rn) then
                        write(cnum,'(f6.1)') einhVal(jj_v(i5))
                    else
                        write(cnum,'(f10.6)') einhVal(jj_v(i5))
                    endif
                    cnum = trim(adjustL(cnum))
                    call UnitFind(trim(EinheitWK(nng)%s(i1+1:)),jj,jpl,factor,str6)  ! required
                    goto 122
                end if
            end if

            openbr = .false.   ! put the whole expression from the following loop in brackets,
            ! if the preceding operator is  '/' (kdiv=T)

            do ij=1,nu_other
                if(trim(ucase(adjustL(EinheitWK(nng)%s(i1+1:)))) == trim(ucase(unit_other(ij)))) then
                    EinheitWK(nng)%s(i1+1:) = trim(ucase(unit_basis(ij)))
                    exit
                end if
            enddo

            do
                ! write(66,*) 'do loop:  EinheitWK(nng)%s=',EinheitWK(nng)%s

                ! loop over the components of the unit (separated by * or /),
                ! given for the k-th Symbol of equation number i
                prop = ''      ! present operator
                i2a = index(EinheitWK(nng)%s(i1+1:),'*')
                i2b = index(EinheitWK(nng)%s(i1+1:),'/')
                i2c = index(EinheitWK(nng)%s(i1+1:),'^')
                i2 = 0
                if(i2a == 0 .and. i2b > 0) then
                    i2 = i2b
                    prop = '/'
                endif
                if(i2b == 0 .and. i2a > 0) then
                    i2 = i2a
                    prop = '*'
                end if
                if(i2b == 0 .and. i2a == 0 .and. i2c > 0) then
                    i2 = i2c
                    prop = '^'
                end if

                if(i2a > 0 .and. i2b > 0) then
                    if(i2a < i2b) then
                        i2 = i2a
                        prop = '*'
                    else
                        i2 = i2b
                        prop = '/'
                    end if
                end if

                nnc = nnc + 1     ! how often is the symbol with index nng found in equation i
                ! write(66,*) 'k=',int(k,2),' i2a,i2b=',int(i2a,2),int(i2b,2),' i2=',int(i2,2)

                if(i2 == 0) then
                    ios = -1
                    if(i1 > 1) then
                        read(EinheitWK(nng)%s(i1+1:),*,iostat=ios) idummy
                    end if
                    if(ios /= 0) then
                        call UnitFind(trim(EinheitWK(nng)%s(i1+1:)),jj,jpl,factor,str6)
                        if(jj > 0 .and. jj <= UU%nSymb) then
                            if(i1 == 0) then
                                EinheitWK(nng)%s = trim(str6)
                            else
                                EinheitWK(nng)%s = EinheitWK(nng)%s(1:i1) // trim(str6)
                            end if
                        end if
                    else
                        ! EinheitWK(nng)%s = EinheitWK(nng)%s(1:i1) // EinheitWK(nng)%s(i1+1:)
                        jj = 0
                        ! write(66,*) 'i2=0:   EinheitWK(nng)%s=',EinheitWK(nng)%s
                        jj = -1
                    end if
                    if(jj == 0 .and. trim(EinheitWK(nng)%s(i1+1:)) /= '1') then
!                         write(66,*) 'Error CLCU (a):  unit=',trim(EinheitWK(nng)%s(i1+1:)),' unknown!'
                        write(log_str, '(*(g0))') 'Error CLCU (a):  unit=',trim(EinheitWK(nng)%s(i1+1:)),' unknown!'
                        call logger(66, log_str)
                        npMsg = npMsg + 1
                        call CharModA1(PUnitMsg, npMsg)
                        PUnitMsg(npMsg)%s = 'Eq. #=' // intStr(i) // ' Error CLCU (a):  unit=' //  &
                            trim(EinheitWK(nng)%s(i1+1:)) // ' is unknown!'
                        if(npMsg >= 1) then
                            ifehl = 1
                            return
                        endif
                    end if
                elseif(i2 > 0) then
                    call UnitFind(trim(EinheitWK(nng)%s(i1+1:i1+i2-1)),jj,jpl,factor,str6)
                    if(jj > 0 .and. jj <= UU%nSymb) then
                        if(i1 == 0) then
                            EinheitWK(nng)%s = trim(str6) // EinheitWK(nng)%s(i1+i2:)
                        else
                            if(prop /= '^') EinheitWK(nng)%s = EinheitWK(nng)%s(1:i1) // trim(str6) // EinheitWK(nng)%s(i1+i2:)
                            ! if(prop == '^') EinheitWK(nng)%s = EinheitWK(nng)%s(1:i1) // trim(str6) // '^' // Einheit(nng)%s(i1+i2+0:)
                        end if
                        i2 = 1 + len_trim(str6)
                    endif
                    if(jj == 0) then
!                         write(66,*) 'Error CLCU (b):  unit=',trim(adjustL(EinheitWK(nng)%s(i1+1:i1+i2-1))),' unknown!'
                        write(log_str, '(*(g0))') 'Error CLCU (b):  unit=',trim(adjustL(EinheitWK(nng)%s(i1+1:i1+i2-1))),' unknown!'
                        call logger(66, log_str)
                        npMsg = npMsg + 1
                        call CharModA1(PUnitMsg,npMsg)
                        PUnitMsg(npMsg)%s = 'Eq. #=' // intStr(i) // ' Error CLCU (b):  unit=' //  &
                            trim(EinheitWK(nng)%s(i1+1:i1+i2-1)) // ' is unknown!'
                        if(npMsg >= 1) then
                            ifehl = 1
                            return
                        endif
                    end if
                    lastop = prop
                    i1 = i1+1+i2 - 1
                endif

                if(jj > 0) then
                    if(einhVal(jj) >= 1.0_rn) then
                        write(cnum,'(f6.1)') einhVal(jj)
                    else
                        write(cnum,'(f10.6)') einhVal(jj)
                    end if
                    cnum = trim(adjustL(cnum))
                    divk = .true.   ! now for all (brackets))
                elseif(jj == -1) then
                    if(lastop == '^') then
                        write(cnum,*) idummy
                        cnum = trim(adjustL(cnum))
                        divk = .true.   ! now for all (brackets))
                    end if
                end if

                if(i2 > 0) then
                    ! at least one separator character present (* or /):
                    if(nnc == 1) then
                        if(divk) then
                            openbr = .true.
                            ceinhwk = '(' // trim(cnum) // prop
                            ceinhwks = '(' // EinhSymbG(jj)%s // prop
                        else
                            ceinhwk = trim(cnum) // prop
                            ceinhwks = EinhSymbG(jj)%s // prop
                        endif
                    elseif(nnc > 1) then
                        ceinhwk = trim(ceinhwk) // trim(cnum) // prop
                        ceinhwks = trim(ceinhwks) // EinhSymbG(jj)%s // prop
                    endif
                else
                    if(nnc == 1) then
                        if(divk .and. .not. openbr) then
                            openbr = .true.
                            ceinhwk = '('  // trim(cnum)
                            ceinhwks = '(' // EinhSymbG(jj)%s
                        else
                            ceinhwk = trim(cnum)
                            ceinhwks = EinhSymbG(jj)%s
                        end if
                    elseif(nnc > 1) then
                        if(prop /= '^') then
                            ceinhwk = trim(ceinhwk)  // trim(cnum)
                            if(jj > 0) then
                                ceinhwks = trim(ceinhwks) // EinhSymbG(jj)%s
                            else
                                ceinhwks = trim(ceinhwks) // EinheitWK(nng)%s(i1+1:)
                            end if
                        else
                            ceinhwk = trim(ceinhwk)  // EinheitWK(nng)%s(i1+1:)
                            ceinhwks = trim(ceinhwks) // EinheitWK(nng)%s(i1+1:)
                        end if
                    end if
                end if
                if(i2 > 0) then
                else
                    if(openbr) then
                        ceinhwk = trim(ceinhwk) // ')'
                        ceinhwks = trim(ceinhwks) // ')'
                    endif
                    exit
                endif
            end do !-------------------------------------------------------------- loop components within one k

122         continue

            if(trim(ceinhwks) == '(S)' .or. trim(ceinhwks) == '(1/S)') n21 = n21 + 1

            if(nwk == 0) then
                ceinhwk_v(1)%s = trim(ceinhwk)
                ceinhwks_v(1)%s = trim(ceinhwks)
                EinheitWK_v(1)%s = EinheitWk(nng)%s
                jj_v(1) = jj
                nwk = 1
                uconv_v(1) = uconv(nng)
!                 if(prout) write(66,*) 'nwk=',nwk,' ',Symbole(nng)%s,' ceinhwk=',trim(ceinhwk), &
!                     ' Einheitwk(nng)=',Einheitwk(nng)%s,' uconv(nng)=',uconv(nng)
                if(prout)  then
                    write(log_str, '(*(g0))') 'nwk=',nwk,' ',Symbole(nng)%s,' ceinhwk=',trim(ceinhwk), &
                    ' Einheitwk(nng)=',Einheitwk(nng)%s,' uconv(nng)=',uconv(nng)
                    call logger(66, log_str)
                end if
            else
                itt = findlocT(EinheitWK_v, EinheitWk(nng)%s, 1)
                if(itt == 0) then
                    nwk = nwk + 1
!                     if(prout) write(66,*) 'nwk=',nwk,' ',Symbole(nng)%s,' ceinhwk=',trim(ceinhwk), &
!                         ' Einheitwk(nng)=',Einheitwk(nng)%s,' uconv(nng)=',uconv(nng)
                    if(prout)  then
                        write(log_str, '(*(g0))') 'nwk=',nwk,' ',Symbole(nng)%s,' ceinhwk=',trim(ceinhwk), &
                        ' Einheitwk(nng)=',Einheitwk(nng)%s,' uconv(nng)=',uconv(nng)
                        call logger(66, log_str)
                    end if
                    call CharModA1(ceinhwk_v,nwk)
                    call CharModA1(ceinhwks_v,nwk)
                    call CharModA1(EinheitWK_v,nwk)
                    call IntModA1(jj_v,nwk)
                    ceinhwk_v(nwk)%s = trim(ceinhwk)
                    ceinhwks_v(nwk)%s = trim(ceinhwks)
                    EinheitWK_v(nwk)%s = EinheitWk(nng)%s
                    uconv_v(nwk) = uconv(nng)
                    jj_v(nwk) = jj
                else
                    !  uconv(nng) = einhVal(jj_v(itt))
                endif
            end if

!             if(prout) write(66,'(a,i2,a,i3,8a,a,es16.9,a,L1,a,i3)') '     Eq. i=',i,' k=',k, &
!                 ' ceinhwk=',trim(ceinhwk),'  ceinhwks=',trim(ceinhwks), &
!                 '  einheitwk(nng)=',EinheitWK(nng)%s,' cnum=',trim(cnum), &
!                 ' uconvk=',uconv(nng),' insideExp=',insideExp(k),' nng=',nng
            if(prout)  then
                write(log_str, '(a,i2,a,i3,8a,a,es16.9,a,L1,a,i3)') '     Eq. i=',i,' k=',k, &
                ' ceinhwk=',trim(ceinhwk),'  ceinhwks=',trim(ceinhwks), &
                '  einheitwk(nng)=',EinheitWK(nng)%s,' cnum=',trim(cnum), &
                ' uconvk=',uconv(nng),' insideExp=',insideExp(k),' nng=',nng
                call logger(66, log_str)
            end if

            ! an uconv(k) is equal to an uconv(nng) !

            ! Replace original symbol name by:
            !     uconv(nng))*Messwert(nng) (as string)  : in strgv1
            !                 Messwert(nng) (as string)  : in strgv3

            !  write(66,*) 'strgv1  =',trim(strgv1)
            !  write(66,*) 'strgv3  =',trim(strgv3)
            if(nng > nab) then
                ucv = unit_conv_factSV(nng)
            else
                ucv = 1.0_rn
            end if
            if(abs(ucv - 1.0_rn) < 1.e-5_rn) then
!                 if(prout2) write(66,*) 'RePl-A: ',ceinhwk,' ucv=',sngl(ucv)
                if(prout2)  then
                    write(log_str, '(*(g0))') 'RePl-A: ',ceinhwk,' ucv=',sngl(ucv)
                    call logger(66, log_str)
                end if
                call StrReplace(strg1,SymboleG(nng)%s,trim(ceinhwk),.true.,.true.)

                write(cnum,'(es16.9)') ucv*MesswertSV(nng)
                str5 = '(' // trim(adjustL(cnum)) // ')'
!                 if(prout2) write(66,*) 'RePl-B: ',str5,' ucv=',sngl(ucv)
                if(prout2)  then
                    write(log_str, '(*(g0))') 'RePl-B: ',str5,' ucv=',sngl(ucv)
                    call logger(66, log_str)
                end if
                call StrReplace(strgv1,SymboleG(nng)%s,trim(str5),.true.,.true.)

                write(cnum,'(es16.9)') MesswertSV(nng)
                str5 = '(' // trim(adjustL(cnum)) // ')'
!                 if(prout2) write(66,*) 'RePl-C: ',str5
                if(prout2)  then
                    write(log_str, '(*(g0))') 'RePl-C: ',str5
                    call logger(66, log_str)
                end if
                call StrReplace(strgv3,SymboleG(nng)%s,trim(str5),.true.,.true.)

            else
                write(cnum,'(es16.9)') ucv
                str5 = '(' // trim(adjustL(cnum)) // '*' // trim(ceinhwk) // ')'
!                 if(prout2) write(66,*) 'RePl-D: ',str5,' ucv=',sngl(ucv)
                if(prout2)  then
                    write(log_str, '(*(g0))') 'RePl-D: ',str5,' ucv=',sngl(ucv)
                    call logger(66, log_str)
                end if
                call StrReplace(strg1,SymboleG(nng)%s,trim(str5),.true.,.true.)

                write(cnum,'(es16.9)') ucv*MesswertSV(nng)
                str5 = '(' // trim(cnum) // ')'
!                 if(prout2) write(66,*) 'RePl-E: ',str5,' ucv=',sngl(ucv)
                if(prout2)  then
                    write(log_str, '(*(g0))') 'RePl-E: ',str5,' ucv=',sngl(ucv)
                    call logger(66, log_str)
                end if
                call StrReplace(strgv1,SymboleG(nng)%s,trim(str5),.true.,.true.)

                write(cnum,'(es16.9)') MesswertSV(nng)
                str5 = '(' // trim(cnum) // ')'
!                 if(prout2) write(66,*) 'RePl-F: ',str5
                if(prout2)  then
                    write(log_str, '(*(g0))') 'RePl-F: ',str5
                    call logger(66, log_str)
                end if
                call StrReplace(strgv3,SymboleG(nng)%s,trim(str5),.true.,.true.)
            end if

            RseiteG(2)%s = trim(strg1)

        end do    ! loop k -----------------------------------------------------------------------


!         if(prout) write(66,*) 'strgv1  =',trim(strgv1)
        if(prout)  then
            write(log_str, '(*(g0))') 'strgv1  =',trim(strgv1)
            call logger(66, log_str)
        end if
!         if(prout) write(66,*) 'strgv3  =',trim(strgv3)
        if(prout)  then
            write(log_str, '(*(g0))') 'strgv3  =',trim(strgv3)
            call logger(66, log_str)
        end if
        fd_with = seval(trim(strgv1))
        fd_without = seval(trim(strgv3))
!         if(prout) write(66,'(3(a,es16.9))') '    fd_with=',fd_with,' fd_without=',fd_without, &
!             ' fd_with/fd_without=',fd_with/fd_without
        if(prout)  then
            write(log_str, '(3(a,es16.9))') '    fd_with=',fd_with,' fd_without=',fd_without, &
            ' fd_with/fd_without=',fd_with/fd_without
            call logger(66, log_str)
        end if
        if(uconv(i) < 1.E-6_rn) uconv(i) = fd_with/fd_without

        ! test the exp argument and remove it finally:
        call Function_arg_resolve(i,prout,RSeiteG(2)%s,'RSeiteG(2)','EXP',n21)
        if(ifehl == 1) return

        call Function_arg_resolve(i,prout,RSeiteG(2)%s,'RSeiteG(2)','SQRT',n21)
        if(ifehl == 1) return

        ucdone(i) = .true.

        if(prout) then
            ! for example:
            ! Eq. i= 5 after inserting num. units: RSeiteG(2)=((1.0/21.0) * (6.00000000E+01*(21.0)) * 60^0) / 1.0
!             write(66,'(a,i0,a,a,a,es16.9)') 'Eq. i=',i,' after inserting num. units: RSeiteG(2)=',trim(RSeiteG(2)%s), &
!                 '  uconv(i)=',uconv(i)
            write(log_str, '(a,i0,a,a,a,es16.9)') 'Eq. i=',i,' after inserting num. units: RSeiteG(2)=',trim(RSeiteG(2)%s), &
                '  uconv(i)=',uconv(i)
            call logger(66, log_str)
        endif

        dummy = seval(RSeiteG(2)%s)
        if(ifehlxx == 1) then
!             write(66,*) 'Error in seval: arg =',RSeiteG(2)%s   ! (i2:i3)
            write(log_str, '(*(g0))') 'Error in seval: arg =',RSeiteG(2)%s   ! (i2:i3)
            call logger(66, log_str)
            npMsg = npMsg + 1
            call CharModA1(PUnitMsg,npMsg)
            PUnitMsg(npMsg)%s = 'Eq. # ' // intStr(i) // ' Error in seval: arg =' // trim(RSeiteG(2)%s)   ! (i2:i3)
            if(npMsg >= 1) then
                ifehl = 1
                return
            endif
        endif
        fdummy = dummy
        fdummy = fdummy / uconv(i)
!         if(prout) write(66,'(3(a,es16.9),a,a)') 'Final value fdummy =',fdummy,  &
!             ' seval(RSeiteG(2)%s)=',fdummy*uconv(i),' uconv(i)=',uconv(i), &
!             ' arg=',trim(RSeiteG(2)%s)
        if(prout)  then
            write(log_str, '(3(a,es16.9),a,a)') 'Final value fdummy =',fdummy,  &
            ' seval(RSeiteG(2)%s)=',fdummy*uconv(i),' uconv(i)=',uconv(i), &
            ' arg=',trim(RSeiteG(2)%s)
            call logger(66, log_str)
        end if

        RSeiteG(3)%s = RSeiteG(2)%s
        call StrReplace(RseiteG(3)%s,'1.0','1',.true.,.true.)
        do jj=1,UU%nSymb
            if(abs(Einhval(jj) - 1.0_rn) < 1.e-4) cycle
            if(Einhval(jj) >= 1.0_rn) write(cnum,'(f6.1)') Einhval(jj)
            if(Einhval(jj) < 1.0_rn) write(cnum,'(f10.6)') Einhval(jj)
            cnum = adjustL(cnum)
            call StrReplace(RseiteG(3)%s,trim(cnum),EinhSymb(jj)%s,.true.,.true.)
        end do
        goto 57

57      continue
        xevalf = 0._rn

!         if(prout) write(66,'(a,i2,a,a)') 'Eq. i=',i,'(d):  RseiteG(3)=',RseiteG(3)%s
        if(prout)  then
            write(log_str, '(a,i2,a,a)') 'Eq. i=',i,'(d):  RseiteG(3)=',RseiteG(3)%s
            call logger(66, log_str)
        end if

        ! remove all blank characters:
        cdum = ''
        do j=1,len_trim(RSeiteG(1)%s)
            if(RSeiteG(1)%s(j:j) /= ' ') cdum = trim(cdum) // RSeiteG(1)%s(j:j)
        end do
        RSeiteG(1)%s = trim(cdum)

        cdum = ''
        do j=1,len_trim(RSeiteG(3)%s)
            if(RSeiteG(3)%s(j:j) /= ' ') cdum = trim(cdum) // RSeiteG(3)%s(j:j)
        end do
        RScopy = trim(cdum)
        call parsef(i,RSeiteG(3)%s,EinhSymbG)
        Evalue = evalf(i,EinhVal) ! / uconv(i)

        zEinhVal = 1.0_rn
        EvalFactor = evalf(i,zEinhVal)
        j = int(evalfactor + .499_rn)
        if(j > 0 .and. j < 5 .and. Abs(EvalFactor - real(j,rn)) < 1.e-12_rn) Evalfactor = 1.0_rn

        Evalue_red = Evalue/EvalFactor

!         if(prout) write(66,*) 'RScopy=',trim(RScopy)
        if(prout)  then
            write(log_str, '(*(g0))') 'RScopy=',trim(RScopy)
            call logger(66, log_str)
        end if
!         if(prout) write(66,*) 'parsef: RSeiteG(3)=',RSeiteG(3)%s
        if(prout)  then
            write(log_str, '(*(g0))') 'parsef: RSeiteG(3)=',RSeiteG(3)%s
            call logger(66, log_str)
        end if
!         if(prout) write(66,'(a,i0,2(a,es16.9),a,i0,a,es16.9)') 'Eq. ',i,  &
!             ' Evalue=evalf(i,EinhVal)=',evalf(i,EinhVal),  &
!             '  EvalFactor=',EvalFactor,' j=',j,'      Evalue_red=', &
!             Evalue_red
        if(prout)  then
            write(log_str, '(a,i0,2(a,es16.9),a,i0,a,es16.9)') 'Eq. ',i,  &
            ' Evalue=evalf(i,EinhVal)=',evalf(i,EinhVal),  &
            '  EvalFactor=',EvalFactor,' j=',j,'      Evalue_red=', &
            Evalue_red
            call logger(66, log_str)
        end if
!         if(prout) write(66,'(a,100(a,1x))') 'EinhSymbG=',(EinhSymbG(k)%s,k=1,7)
        if(prout)  then
            write(log_str, '(a,100(a,1x))') 'EinhSymbG=',(EinhSymbG(k)%s,k=1,7)
            call logger(66, log_str)
        end if

        nv = 0
        xvar = 0._rn
        nvar = ''

        ! calculate partial derivatives:
        do j=1,UU%nSymb
            if(abs(EinhVal(j) - 1.0_rn) < 1.E-5_rn) cycle
            Fv1 = evalf(i,EinhVal)
            dpa = EinhVal(j) * 1.0000005_rn - EinhVal(j)
            EinhVal(j) = EinhVal(j) + dpa
            Fv2 = evalf(i,EinhVal)
            EinhVal(j) = EinhVal(j) - dpa
            ! if(abs((dummy-Evalue/EvalFactor**zero)/dpa) > 1.E-8_rn*EinhVal(j)) then
            if(abs((Fv2-Fv1)/dpa) > 1.E-8_rn*Evalue) then
                nv = nv + 1
                xvar(nv) = EinhVal(j)
                nvar(nv) = EinhSymb(j)%s
                dpi(nv) = (Fv2-Fv1)/dpa
!                 if(prout) write(66,'(2(a,i0),1(a,es16.9),a,a,1(a,es16.9))') 'nv=',nv,' j=',j, &
!                     ' xvar(nv)=',xvar(nv), &
!                     ' nvar(nv)=',trim(nvar(nv)),' dpi=',sngl(dpi(nv))
                if(prout)  then
                    write(log_str, '(2(a,i0),1(a,es16.9),a,a,1(a,es16.9))') 'nv=',nv,' j=',j, &
                    ' xvar(nv)=',xvar(nv), &
                    ' nvar(nv)=',trim(nvar(nv)),' dpi=',sngl(dpi(nv))
                    call logger(66, log_str)
                end if

            end if
        end do

        if(nv == 0 .and. abs(Evalue-1.0_rn) < 1.e-13_rn) then
            einheitWK(i)%s = '1'
            goto 100
        end if
        if(nv >= 1) then
            testvor = 1.0_rn
            Einvor = '1'
            if(nv >= 1) then

                ! The result value for the unit is interpreted as a generalized product
                ! of involved units. This means, each involved unit uk
                ! has an exponent of 1 or -1, uk**1 or uk**(-1).
                ! The routine genstrings calculates for nv involved units all 2**nv
                ! possible nv-combinations.
                ! For k2=1,nv the combinations are stored in the array npw.

                if(allocated(arr2dim)) deallocate(arr2dim)
                allocate(arr2dim(2**nv,nv))
                kper = 0
                call genstrings(nv,kper,arr2dim)

                do k2=1,2**nv
                    npw(1:nv) = arr2dim(k2,1:nv)
                    fff = 1.0_rn
                    do k1=1,nv; fff = fff * (xvar(k1)**real(npw(k1),rn)); end do;

!                     if(prout) write(66,'(2(a,i0),2(a,es16.9))') 'k2=',k2,' nv=',nv,'  fff=',fff, &
!                         ' fdummy=',fdummy
                    if(prout)  then
                        write(log_str, '(2(a,i0),2(a,es16.9))') 'k2=',k2,' nv=',nv,'  fff=',fff, &
                        ' fdummy=',fdummy
                        call logger(66, log_str)
                    end if
                    do j=1,10
                        if(abs(fdummy - real(j,rn)*fff) < 2.e-4*fdummy .or.   &
                            abs(fdummy - fff/real(j,rn)) < 2.e-4*fdummy ) then
!                             if(prout)  write(66,*) 'fdummy found: npw=',npw(1:nv),' nvar=',(trim(nvar(jk)),' ',jk=1,nv)
                            if(prout)   then
                                write(log_str, '(*(g0))') 'fdummy found: npw=',npw(1:nv),' nvar=',(trim(nvar(jk)),' ',jk=1,nv)
                                call logger(66, log_str)
                            end if
                            einvor = ''
                            do k1=1,nv
                                if(k1 == 1) then
                                    if(npw(k1) == 1)  einvor = nvar(k1)
                                    if(npw(k1) == -1) einvor = '1/' // nvar(k1)
                                else
                                    if(npw(k1) == 1)  einvor = trim(einvor) // '*' // nvar(k1)
                                    if(npw(k1) == -1) einvor = trim(einvor) // '/' // nvar(k1)
                                endif
                            enddo
!                             if(prout) write(66,*) 'unit found: einvor=',trim(einvor)
                            if(prout)  then
                                write(log_str, '(*(g0))') 'unit found: einvor=',trim(einvor)
                                call logger(66, log_str)
                            end if
                            goto 80
                        endif
                    enddo
                enddo
            endif

            help = (fdummy - 11._rn)/(1.0_rn/21._rn)
            if(abs(help - aint(help+1.e-6_rn)) < 2.e-4_rn) then
                if(index(RSeiteG(2)%s,'21.') < index(RSeiteG(2)%s,'11.')) then
                    EinheitWK(i)%s = '1/s'
                else
                    EinheitWK(i)%s = 'Bq'
                end if
                xevalf = fdummy
                goto 85
            end if
!             write(66,'(a,i2,6a,a,es11.4)') 'Error:   Eq. i=',int(i,2),' ',Formelt(i)%s,':  no unit found!  Einvor=',trim(Einvor),  &
!                 '  RSide=',RSeiteG(1)%s,' fdummy=',fdummy
            write(log_str, '(a,i2,6a,a,es11.4)') 'Error:   Eq. i=',int(i,2),' ',Formelt(i)%s,':  no unit found!  Einvor=',trim(Einvor),  &
                '  RSide=',RSeiteG(1)%s,' fdummy=',fdummy
            call logger(66, log_str)
            npMsg = npMsg + 1
            call CharModA1(PUnitMsg,npMsg)
            PUnitMsg(npMsg)%s = 'Eq. #=' // trim(intStr(i)) // '  ' // Formelt(i)%s // ':  no unit found!  Einvor=' //trim(Einvor)  &
                // ' RSide=' // RSeiteG(1)%s
            if(npMsg >= 1) then
                ifehl = 1
                return
            end if
80          continue

            xevalf = fdummy
            EinheitWK(i)%s = trim(einvor)
85          continue

            if(len_trim(EinheitWK(i)%s) > 1) then
                if(einheitWK(i)%s(1:2) == '1*') EinheitWK(i)%s = trim(EinheitWK(i)%s(3:))
            end if
            goto 100
        end if

100     continue
        if(len_trim(EinheitWK(i)%s) >= 3) then
            if(EinheitWK(i)%s(1:2) == '1*' .and. len_trim(EinheitWK(i)%s) >= 3) EinheitWK(i)%s = trim(EinheitWK(i)%s(3:))
        end if
        if(len_trim(EinheitWK(i)%s) > 4) then
            if(EinheitWK(i)%s(1:4) == 'Imp.') EinheitWK(i)%s = 'Imp' //trim(EinheitWK(i)%s(5:))
        end if
        if(len_trim(EinheitWK(i)%s) > 3) then
            if(EinheitWK(i)%s(1:3) == 'Imp') EinheitWK(i)%s = '1' // trim(EinheitWK(i)%s(4:))
        end if
        i1 = index(EinheitWK(i)%s,'*Imp')
        if(i1 > 1) then
            if(len_trim(EinheitWK(i)%s) >= i1+4) EinheitWK(i)%s = EinheitWK(i)%s(1:i1-1) // trim(EinheitWK(i)%s(i1+4:))
        end if
        i1 = index(EinheitWK(i)%s,'*1')
        if(i1 > 1) then
            if(i1 == len_trim(EinheitWK(i)%s) - 1) EinheitWK(i)%s = EinheitWK(i)%s(1:i1-1)
        end if

        if(len_trim(EinheitWK(i)%s) >= 3) then
            i1 = index(ucase(einheitWK(i)%s),'*BQ')
            if(i1 > 1) then
                strg1 = 'Bq*' // einheitWK(i)%s(1:i1-1)
                einheitWK(i)%s = trim(strg1)
                i2 = index(strg1,'*1/')
                if(i2 > 0) einheitWK(i)%s = strg1(1:i2-1) // '/' // trim(strg1(i2+3:))
            end if
            i1 = index(ucase(einheitWK(i)%s),'*µSV')
            if(i1 > 1) then
                strg1 = 'µSv*' // einheitWK(i)%s(1:i1-1)
                einheitWK(i)%s = trim(strg1)
                i2 = index(strg1,'*1/')
                if(i2 > 0) einheitWK(i)%s = strg1(1:i2-1) // '/' // trim(strg1(i2+3:))
            end if
        end if

!         write(66,'(a,i3,a,es16.9,a,a,a,i3,a,es16.9,a,a,a,a)') 'Eq. i=',i,  &
!             ' evalf=',xevalf,'  unit=',EinheitWK(i)%s,'  nv=',nv, &
!             ' uconv(i)=',uconv(i), &
!             ' ',Formelt(i)%s,', unit string=',RSeiteG(1)%s
        write(log_str, '(a,i3,a,es16.9,a,a,a,i3,a,es16.9,a,a,a,a)') 'Eq. i=',i,  &
            ' evalf=',xevalf,'  unit=',EinheitWK(i)%s,'  nv=',nv, &
            ' uconv(i)=',uconv(i), &
            ' ',Formelt(i)%s,', unit string=',RSeiteG(1)%s
        call logger(66, log_str)

        if(.true.) then
            ! danger: this prepares the explicit replacement to new units;
            ! this step must be reversed at the end of Report_ucheck !!!
            Einheit(i)%s = EinheitWK(i)%s
            call WTreeViewPutStrCell('treeview1', 4, i, Einheit(i)%s)
            call WTreeViewPutStrCell('treeview2', 4, i, Einheit(i)%s)
        end if

    end do   ! do i=nab....

    if(.not. Gum_restricted .and. batest_user .and. index(einheit(kEGr)%s,'Bq') == 0) then
!         write(171,*) 'file=',trim(fname),';  einheit(kEGr)=',einheit(kEGr)%s
        write(log_str, '(*(g0))') 'file=',trim(fname),';  einheit(kEGr)=',einheit(kEGr)%s
        call logger(171, log_str)
    end if

    if(batest_user) then

    end if

    if(ifehl == 0 .and. npMsg == 0) then
        call gtk_widget_set_visible(idpt('TESavePrjAs'), 1_c_int)
    else
        ! apply_units = apply_SV          !  this should better occur with button TEClose
        ! FP_for_units = .false.          !
    end if

end subroutine CalcUnits

!##################################################################################

subroutine UnitFind(strg1,jj,jpl,factor,strgout)

    ! finds for a given unit-string (strg1) the corresponding basic unit string
    ! (strgout), the numbers jj and jj+jp1 (referring to their location in
    ! unitsTable.csv) and the scaling factor (factor).

    !   Copyright (C) 2021-2024  Günter Kanisch

    use UR_params,          only: rn
    use UR_Gleich,          only: UU,nu_other,unit_other,unit_basis
    use CHF,                only: ucase
    implicit none

    character(len=*),intent(in)   :: strg1      ! given unit string
    integer   ,intent(out)        :: jj         ! number of the basic unit
    integer   ,intent(out)        :: jpl        ! number added to jj
    real(rn),intent(out)          :: factor     ! unit scaling factor
    character(len=*),intent(out)  :: strgout    ! corresponding basic unit

    integer                    :: j,k,nfd,i
    character(len=len(strg1))  :: strg2

    factor = 1.0_rn
    strg2 = trim(adjustL(strg1))
    strgout = ''

    do i=1,nu_other
        if(trim(ucase(adjustL(strg1))) == trim(ucase(unit_other(i)))) then
            strg2 = trim(ucase(unit_basis(i)))
            strgout = trim(strg2)
            nfd = 1
            exit
        end if
    end do

    do j=1,uu%nSymb
        nfd = 0
        if(trim(ucase(strg2)) == trim(ucase(UU%EinhSymb(j)%s))) then
            factor = 1.0_rn
            jj = j
            jpl = 0
            nfd = 1
            strgout = trim(ucase(UU%EinhSymb(j)%s))
            exit
        end if
        do k=1,UU%nSymbCsd(j)
            if(trim(ucase(strg2)) == trim(ucase(UU%EinhSymbScd(j,k)%s))) then
                factor = UU%EinhScdFact(j,k)
                jj = j
                jpl = k
                nfd = 1
                strgout = trim(ucase(UU%EinhSymb(j)%s))
                exit
            end if
        end do
        if(nfd == 0) then
            do k=1,UU%nSymbSyn(j)
                if(trim(ucase(strg2)) == trim(ucase(UU%EinhSymbSynon(j,k)%s))) then
                    jj = j
                    jpl = 0
                    factor = 1.0_rn
                    nfd = 1
                    strgout = trim(ucase(UU%EinhSymb(j)%s))
                    exit
                end if
            end do
        end if
        if(nfd == 1) exit
    end do
    if(jj > UU%nSymb) jj = 0
    return
end subroutine UnitFind

!#############################################################################

!subroutine testnumber(strg1,k1,k2,RseiteG)
!use UR_params,     only: rn,one,zero,eps1min
!use UR_Gleich,     only: charv
!implicit none!
!
!character(len=*),intent(in)   :: strg1
!integer   ,intent(in)         :: k1,k2
!type(charv),intent(inout)     :: RseiteG(:)
!
!integer               :: ileng,ios
!character(len=10)     :: cformat
!character(len=40)     :: cnum
!real(rn)              :: dummy
!
!write(cformat,'(a,i0,a)') '(f',ileng,'.0)'
!   call enable_locale_c(1)
!dummy = zero
!read(strg1,cformat,iostat=ios) dummy
!write(cnum,'(es13.6)') dummy
!    call enable_locale_c(2)
!         ! write(66,*) 'dummy=',sngl(dummy),' i1=',int(i1,2),' cnum=',trim(cnum)
!if(ios == 0 .and. abs(dummy - 1.0_rn) > eps1min .and. abs(dummy - zero) > eps1min) then
!         ! write(66,*) '1.64485 vor  abschneiden: trim(RseiteG(1)%s=',trim(RseiteG(1)%s),'  i1=',int(i1,2)
!         ! write(66,*) '1.64485 nach abschneiden: trim(RseiteG(1)%s(i1+1:))=',trim(RseiteG(1)%s(i1+1:)),' cnum=',trim(cnum)
!  if(index(cnum,'NaN') == 0) then
!    if(k1 == 1) then
!      RSeiteG(1)%s = '(1)' //trim(RseiteG(1)%s(k2+1:))
!    elseif(k1 > 1 .and. k2 < len_trim(RSeiteG(1)%s)) then
!      RSeiteG(1)%s = trim(RseiteG(1)%s(1:k1-1)) // '(1)' // trim(RseiteG(1)%s(k2+1:))
!    elseif(k2 == len_trim(RSeiteG(1)%s)) then
!      RSeiteG(1)%s = trim(RseiteG(1)%s(1:k1-1)) // '(1)'
!    end if
!  end if
!end if
!
!end subroutine testNumber
!
!#############################################################################################

subroutine Function_arg_resolve(ie,prout,RString,RSname,sfunc,n21)

    ! locates in string (Rstring) the argument-string of a function (like EXP, SQRT, LOG)
    ! and tries to rework the argument-string such that its unit is 1, so that, within Rstring,
    ! the function value can be set equal to 1.

    !   Copyright (C) 2021-2024  Günter Kanisch

    use UR_params,      only: rn, TWO
    use UR_gleich,      only: ifehl, npMsg, PUnitMsg
    use CHF,            only: ucase, StrReplace, intStr, realStr
    use xx,             only: ifehlxx
    use file_io,        only: logger
    use Top,            only: CharModA1
    implicit none

    integer   ,intent(in)           :: ie       ! equation number
    logical(4),intent(in)           :: prout    ! write control output
    character(len=*),intent(inout)  :: Rstring  ! string containing function expressions
    character(len=*),intent(in)     :: RSname   ! original name-string of Rstring
    character(len=*),intent(in)     :: sfunc    ! name of the function
    integer   ,intent(in)           :: n21      ! how often is the unit "s" contained in Rstring

    integer             :: i0, i1, i2, i3, jplus, jk, ios
    real(rn)            :: dummy, seval
    logical             :: fail

    character(len=512)           :: log_str
    character(len=len(Rstring)+100)    :: cnum

    i0 = 1
    do
        i1 = index(RString(i0:),trim(sfunc))
        if(i1 > 0) then
            i1 = i0 + i1 - 1
            ! now i1, i2 and i3 are absolute indices:
            jplus = 0
            i2 = 0
            i3 = 0
            do jk=i1+1,len_trim(RString)
                if(RString(jk:jk) == '(' .and. jplus == 0) then; i2 = jk; jplus = jplus + 1; cycle; end if
                if(RString(jk:jk) == '(') then; jplus = jplus + 1; cycle; end if
                if(RString(jk:jk) == ')' .and. jplus > 1) then; jplus = jplus - 1; cycle; end if
                if(RString(jk:jk) == ')' .and. jplus == 1) then; i3 = jk; exit; end if
            end do

            if(i2 >= 4 ) then
                if(RString(i2-3:i2-1) == 'EXP') then
                    do jk=i2+1,i2+5
                        if(RString(jk:jk) == ' ') cycle
                        if(RString(jk:jk) /= '-' .and. RString(jk:jk) /= '+') then
                            exit
                        else
                            RString(jk:jk) = ' '
!                             if(prout)  write(66,*) ' leading + or - eliminated: Rstring=',trim(Rstring)
                            if(prout)   then
                                write(log_str, '(*(g0))') ' leading + or - eliminated: Rstring=',trim(Rstring)
                                call logger(66, log_str)
                            end if
                            exit
                        end if
                    end do
                end if
            end if

            if(i3 > 0) then
                if(trim(sfunc) == 'LOG') then
                    cnum = trim(Rstring)
                    read(cnum(i2+1:i3-1),*,iostat=ios) dummy
                    if(ios == 0) then
                        if(i1 == 1) then
                            RString = '1.0' // RString(i3+1:)
                        elseif(i1 > 1) then
                            RString = RString(1:i1-1) // '1.0' // RString(i3+1:)
                        end if
                        i0 = 1
                    end if
                end if

                if(trim(sfunc) == 'EXP' .or. trim(sfunc) == 'SQRT') then
                    dummy = seval(RString(i2+1:i3-1))
!                     if(prout) write(66,'(a,a,es16.9,a,a,a,i0)') trim(sfunc),':  seval-wert: ',dummy, &
!                         '  Argument=',RString(i2+1:i3-1),'  n21=',n21
                    if(prout)  then
                        write(log_str, '(a,a,es16.9,a,a,a,i0)') trim(sfunc),':  seval-wert: ',dummy, &
                        '  Argument=',RString(i2+1:i3-1),'  n21=',n21
                        call logger(66, log_str)
                    end if
                    if(abs(abs(dummy) - abs(aint(dummy,rn))) < 1.e-7_rn .and. ifehlxx == 0 .and. abs(dummy) < 10._rn) then
                        if(i1 == 1) then
                            RString = '1.0' // RString(i3+1:)
                        elseif(i1 > 1) then
                            RString = RString(1:i1-1) // '1.0' // RString(i3+1:)
                        end if
                    else
                        fail = .true.
                        if(n21 >= 1 ) then
                            if(abs(abs(dummy) - 60._rn) < 1.e-4_rn*60._rn) fail = .false.
                            if(abs(abs(dummy) - 1.0_rn/60._rn) < 1.e-4_rn/60._rn) fail = .false.
                            if(abs(abs(dummy) - 3600._rn) < 1.e-4_rn*3600._rn) fail = .false.
                            if(abs(abs(dummy) - 1.0_rn/3600._rn) < 1.e-4_rn/3600._rn) fail = .false.
                            if(abs(abs(dummy) - 86400._rn) < 1.e-4_rn*86400._rn) fail = .false.
                            if(abs(abs(dummy) - 1.0_rn/86400._rn) < 1.e-4_rn/86400._rn) fail = .false.
                        end if

                        if(fail) then
!                             write(66,'(a,a,a,es16.9)') 'Error CLCU:  Units in ',trim(sfunc),' argument do not match:  seval=',dummy
                            write(log_str, '(a,a,a,es16.9)') 'Error CLCU:  Units in ',trim(sfunc),' argument do not match:  seval=',dummy
                            call logger(66, log_str)
!                             write(66,*) '   Eq: ',RString
                            write(log_str, '(*(g0))') '   Eq: ',RString
                            call logger(66, log_str)
!                             write(66,*) '   Exp argument: ',RString(i2+1:i3-1)
                            write(log_str, '(*(g0))') '   Exp argument: ',RString(i2+1:i3-1)
                            call logger(66, log_str)
                            npMsg = npMsg + 1
                            call CharModA1(PUnitMsg,npMsg)
                            PUnitMsg(npMsg)%s = 'Eq. #=' // intStr(ie) // &
                                ' Error CLCU:  Units in ' // trim(sfunc)  //' argument do not match:  seval=' // realStr(dummy) // &
                                ' arg(' // trim(sfunc) // ')=' // RString(i2+1:i3-1)
                            if(npMsg == 3) then
                                ifehl = 1
                                return
                            end if
                        end if
                        if(i1 == 1) then
                            RString = '1.0' // RString(i3+1:)
                        elseif(i1 > 1) then
                            RString = RString(1:i1-2) // '1.0' // RString(i3+1:)
                        end if
                    end if
!                     if(prout) write(66,'(a,i2,6a)') 'Eq. i=',ie,' after removing ',trim(sfunc),' expression: ',trim(RSname),'=',trim(RString)
                    if(prout)  then
                        write(log_str, '(a,i2,6a)') 'Eq. i=',ie,' after removing ',trim(sfunc),' expression: ',trim(RSname),'=',trim(RString)
                        call logger(66, log_str)
                    end if
                    i0 = 1
                end if
                if(.true. .and. trim(sfunc) == 'EXP') then
                    i2 = 0; i3 = 0
                    do jk=i1,1,-1
                        if(i2 == 0 .and. Scan(RString(jk:jk),' ') > 0) cycle
                        if(i2 == 0 .and. Scan(RString(jk:jk),'-') > 0) then
                            RString(jk:jk) = '+'
                            cycle
                        end if
                        if(i2 == 0 .and. Scan(RString(jk:jk),'+(') == 0) then
                            cycle   ! exit
                        end if
                        if(RString(jk:jk) == '(') then; i2 = jk; exit; end if
                    end do
                    ! write(66,*) 'try remove 1-EXP:  i2=',int(i2,2)
                    if(i2 > 0) then
                        do jk=i2,len_trim(RString)
                            if( RString(jk:jk) == ')') then; i3 = jk; exit; end if
                        end do
                        dummy = seval(trim(RString(i2:i3)))
                        if(ifehlxx == 1) then
!                             write(66,*) 'Error in seval: arg =',RString(i2:i3)
                            write(log_str, '(*(g0))') 'Error in seval: arg =',RString(i2:i3)
                            call logger(66, log_str)
                            npMsg = npMsg + 1
                            call CharModA1(PUnitMsg,npMsg)
                            PUnitMsg(npMsg)%s = 'Eq. #=' // intStr(ie) // &
                                'Error in seval: arg(1 + EXP) =' // RString(i2:i3)
                            if(npMsg == 3) then
                                ifehl = 1
                                return
                            end if
                        end if
                        if(abs(abs(dummy) - TWO) < 1.E-12) then
                            if(i2 == 1) then
                                RString = '1.0' // RString(i3+1:)
                            else
                                RString = RString(1:i2-1) // '1.0' // RString(i3+1:)
                            end if
                        end if
!                         if(prout) write(66,'(a,i2,4a)') 'Eq. i=',ie,' after removing 1-EXP containing bracket: ',trim(RSname),'=',trim(RString)
                        if(prout)  then
                            write(log_str, '(a,i2,4a)') 'Eq. i=',ie,' after removing 1-EXP containing bracket: ',trim(RSname),'=',trim(RString)
                            call logger(66, log_str)
                        end if

                        dummy = seval(trim(RString))
                        if(ifehlxx == 1) then
!                             write(66,*) 'Error in seval: arg =',trim(RString)
                            write(log_str, '(*(g0))') 'Error in seval: arg =',trim(RString)
                            call logger(66, log_str)
                            npMsg = npMsg + 1
                            call CharModA1(PUnitMsg,npMsg)
                            PUnitMsg(npMsg)%s = 'Eq. #=' // intStr(ie) // &
                                'After remove (1 + EXP()): Error in seval: arg(1 + EXP) =' // trim(RString)
                            if(npMsg == 3) then
                                ifehl = 1
                                return
                            end if
                        end if
                    end if
                    i0 = 1
                end if
            end if
        else
            exit
        end if
    end do

end subroutine Function_arg_resolve

!####################################################################################

subroutine locate_func(Rstring,sfunc,nf,i1arr,i2arr,i3arr)

    ! finds for a given unit-string (Rstring) how often (nf) the function string sfunc
    ! is contained in it. It returns for each occurrence the start- and end-psoiotions
    ! iarr() and i2arr(), with iarr3() the length.

    !   Copyright (C) 2021-2024  Günter Kanisch

    implicit none

    character(len=*),intent(in)  :: Rstring   ! String containing a function expression
    character(len=*),intent(in)  :: sfunc     ! name-string of the function
    integer   ,intent(out)       :: nf        ! number of occurences of sfunc in Rstring
    integer   ,intent(out)       :: i1arr(*),i2arr(*),i3arr(*)

    integer             :: i0,i1,i2,i3,jplus,jk,ii,klauf

    nf = 0
    i0 = 1
    do
        i1 = index(RString(i0:),trim(sfunc))
        if(i1 == 0) exit
        i1 = i0 + i1 - 1
        ! now i1, i2 and i3 are absolute indices:
        jplus = 0
        i2 = 0
        i3 = 0
        if(trim(sfunc) == 'EXP') then
            do jk=i1+1,len_trim(RString)
                if(RString(jk:jk) == '(' .and. jplus == 0) then; i2 = jk; jplus = jplus + 1; cycle; end if
                if(RString(jk:jk) == '(') then; jplus = jplus + 1; cycle; end if
                if(RString(jk:jk) == ')' .and. jplus > 1) then; jplus = jplus - 1; cycle; end if
                if(RString(jk:jk) == ')' .and. jplus == 1) then; i3 = jk; exit; end if
            end do
        end if
        if(trim(sfunc) == '^') then
            klauf = 0
            do jk=i1+1-1,len_trim(RString)
                if(klauf == 1) goto 44
                if(Rstring(jk:jk) == ' ') cycle
                if(scan(Rstring(jk:jk),')*/+-') > 0 .and. klauf == 0) then
                    i2 = i1 + 1
                    i3 = jk - 1
                    exit
                end if
                if(Rstring(jk:jk) /= '(') then
                    i2 = jk
                    do ii=1,len_trim(Rstring) - jk
                        if(scan(Rstring(jk+ii:jk+ii),' +-*/') > 0) then
                            i3 = i2+ii-1
                            exit
                        end if
                    end do
                end if
44              continue
                if(Rstring(jk:jk) == '(' .and. jplus == 0) then; klauf = 1; i2 = jk; jplus = jplus + 1; cycle; end if
                if(RString(jk:jk) == '(') then; jplus = jplus + 1; cycle; end if
                if(RString(jk:jk) == ')' .and. jplus > 1) then; jplus = jplus - 1; cycle; end if
                if(RString(jk:jk) == ')' .and. jplus == 1) then; i3 = jk; exit; end if
                ! end if
            end do
        end if
        if(i3 > 0) then
            nf = nf + 1
            i1arr(nf) = i1
            i2arr(nf) = i2
            i3arr(nf) = i3
            i0 = i3 + 1
        else
            exit
        end if
    end do

end subroutine locate_func

!##############################################################################

subroutine genstrings(n,kper,arr2dim)

    implicit none

    integer   ,intent(in)     :: n
    integer   ,intent(inout)  :: kper
    integer   ,intent(out)    :: arr2dim(2**n,n)

    integer           :: arr(n)

    call generateAllBinaryStrings(n, arr, 1, kper, arr2dim);

end subroutine genstrings

!-------------------------------------------------------------------------

recursive subroutine generateAllBinaryStrings(n,arr,i,kper,arr2dim)

    ! find the expressions of 2^n combinations of variables a1, a2,..., an,
    ! forming the expression
    !  a1*a2*  ... *an
    ! when each ai can have the exponent +1 or -1.  (generalized product)
    !
    ! Source:
    !! https://www.geeksforgeeks.org/generate-all-the-binary-strings-of-n-bits/
    ! // This code (in java) is contributed by Surendra_Gangwar
    !
    ! translated to Fortran and modified by GK

    implicit none

    integer   ,intent(in)    :: n,i
    integer   ,intent(out)   :: arr(*)
    integer   ,intent(inout) :: kper

    integer             :: j
    integer             :: arr2dim(2**n,n)

    if (i == n + 1) then
        kper = kper + 1
        arr2dim(kper,1:n) = arr(1:n)
        return
    end if

    !// First assign "0" at ith position and try for all other permutations for remaining positions
    arr(i) = 1;
    j = i+1
    call generateAllBinaryStrings(n, arr, j, kper, arr2dim);

    !// And then assign "1" at ith position and try for all other permutations for remaining positions
    arr(i) = -1;
    j = i+1
    call generateAllBinaryStrings(n, arr, j, kper, arr2dim);

end subroutine generateAllBinaryStrings

!###################################################################

subroutine Save_Ucheck()

    !   Copyright (C) 2021-2024  Günter Kanisch

    use UR_Gleich,        only: ngrs,ResultatSVUCH,UcombSVUCH,decthreshSVUCH,detlimSVUCH, &
                                KBgrenzuSVUCH,KBgrenzoSVUCH,KBgrenzuSHSVUCH,KBgrenzoSHSVUCH, &
                                EinheitSVUCH,MesswertSVUCH,SDWertSVUCH,HBreiteSVUCH,StdUncSVUCH, &
                                applyunitsSV,apply_units
    use Rout,             only: WTreeViewGetStrArray,WTreeViewGetDoubleArray,WDGetEntryDouble, &
                                WDGetCheckButton

    implicit none
    integer          :: i

    applyunitsSV = apply_units

    if(allocated(einheitSVUCH)) deallocate(einheitSVUCH)
    if(allocated(MesswertSVUCH)) deallocate(MesswertSVUCH)
    if(allocated(SDwertSVUCH)) deallocate(SDwertSVUCH)
    if(allocated(HBreiteSVUCH)) deallocate(HBreiteSVUCH)
    if(allocated(StdUncSVUCH)) deallocate(StdUncSVUCH)
    allocate(einheitSVUCH(ngrs),MesswertSVUCH(ngrs),SDwertSVUCH(ngrs),HBreiteSVUCH(ngrs),StdUncSVUCH(ngrs) )

    call WTreeViewGetStrArray('treeview2', 4, ngrs, einheitSVUCH)
    call WTreeViewGetDoubleArray('treeview2', 5, ngrs, MesswertSVUCH)
    call WTreeViewGetDoubleArray('treeview2', 8, ngrs, SDwertSVUCH)
    call WTreeViewGetDoubleArray('treeview2', 9, ngrs, HBreiteSVUCH)
    call WTreeViewGetDoubleArray('treeview2', 11, ngrs, StdUncSVUCH)

    call WDGetEntryDouble('TRentryValue', ResultatSVUCH)
    call WDGetEntryDouble('TRentryUnc', UcombSVUCH)

    call WDGetEntryDouble('TRentryDT', decthreshSVUCH)
    call WDGetEntryDouble('TRentryDL', detlimSVUCH)
    call WDGetCheckButton('TRcheckbutton3', i)
    if(i == 0) then
        call WDGetEntryDouble('TRentryLQBy', KBgrenzuSVUCH)
        call WDGetEntryDouble('TRentryUQBy', KBgrenzoSVUCH)
    else
        call WDGetEntryDouble('TRentryLQBy', KBgrenzuSHSVUCH)
        call WDGetEntryDouble('TRentryUQBy', KBgrenzoSHSVUCH)
    end if


end subroutine Save_Ucheck

!################################################################################

subroutine Restore_Ucheck()

    !   Copyright (C) 2022-2024  Günter Kanisch

    use UR_params,        only: EPS1MIN
    use UR_Gleich,        only: ngrs,EinheitSVUCH,MesswertSVUCH,SDWertSVUCH,HBreiteSVUCH, &
                                StdUncSVUCH,apply_units,applyunitsSV,Messwert,Symbole,  &
                                einheit,SDwert,HBreite,Stdunc,iar
    use Rout,             only: WTreeViewPutStrArray,WTreeViewPutDoubleArray,WTreeViewPutDoubleCell
    use CHF,              only: FindlocT
    use RW2,              only: setupParser
    use UR_types

    implicit none
    integer          :: i,k,j

    call WTreeViewPutStrArray('treeview2', 4, ngrs, einheitSVUCH)
    call WTreeViewPutDoubleArray('treeview2', 5, ngrs, MesswertSVUCH)
    call WTreeViewPutDoubleArray('treeview2', 11, ngrs, StdUncSVUCH)

    do i=1,ngrs
        einheit(i)%s = einheitSVUCH(i)%s
        Messwert(i) = MesswertSVUCH(i)
        SDWert(i) = SDWertSVUCH(i)
        HBreite(i) = HBreiteSVUCH(i)
        StdUnc(i) = StdUncSVUCH(i)
        if(IAR(i) == 1) then
            call WTreeViewPutDoubleCell('treeview2', 8, i, SDwert(i))
            call WTreeViewPutDoubleCell('treeview2', 9, i, HBReite(i))
        else
            if(abs(Messwert(i)) > EPS1MIN ) then
                call WTreeViewPutDoubleCell('treeview2', 8, i, SDwert(i)/Messwert(i))
                call WTreeViewPutDoubleCell('treeview2', 9, i, HBreite(i)/Messwert(i))
            end if
        end if
    end do

    apply_units = applyunitsSV
    do j=1,2
        if(j == 1) k = FindlocT(Symbole,'kilo_Trigger',1)
        if(j == 2) k = FindlocT(Symbole,'min_Trigger',1)
        if(k > 0) then
            if(apply_units) Messwert(k) = 0.0_rn
            if(.not. apply_units) Messwert(k) = 1.0_rn
            call WTreeViewPutDoubleCell('treeview2', 5, k, Messwert(k))
        end if
    end do

end subroutine Restore_Ucheck

!################################################################################

subroutine Report_Ucheck()

    !   Copyright (C) 2021-2024  Günter Kanisch

    use UR_types
    use UR_Gleich,         only: ifehl,nab,ngrs,Symbole,EinheitSVUCH,Einheit, &
                                 Messwert,MesswertSVUCH,StdUnc,StdUncSVUCH,einheitSV, &
                                 symtyp,npMsg,PUnitMsg,kEgr,ncov,unit_conv_fact, &    ! MesswertSV,StdUncSV, &
                                 FormelText,einheit_conv,HBreite,SDWert,SDWertSVUCH, &
                                 HBReiteSVUCH,FP_for_units

    use UR_VARIABLES,      only: results_path, EditorFileUcheck
    use file_io,           only: logger, write_text_file
    use translation_module, only: T => get_translation
    use Rout,              only: WTreeViewPutStrCell

    implicit none

    integer             :: i
    character(len=15)   :: symb, cratio
    character(len=10)   :: ein,einSVUCH,cfakt
    character(len=1)    :: styp
    character(len=512)  :: log_str
    real(rn)            :: ratio


    ifehl = 0

    EditorFileUcheck = results_path // 'Report_units_check.txt'

    write(log_str, '(*(g0))') 'EditorFileUcheck=', EditorFileUcheck
    call logger(66, log_str)

    call write_text_file(text="", full_filename=EditorFileUcheck, status='new')

    log_str = T('Unit-related error messages') // ': '
    if (npMsg == 0) log_str = trim(log_str) // ' ' // T('none')

    call write_text_file(text=log_str, full_filename=EditorFileUcheck)

    do i=1,npMsg
        call write_text_file(text=PUnitMsg(i)%s, full_filename=EditorFileUcheck)
    end do

    if(abs(MesswertSVUCH(kEGr)-Messwert(kEGr)) > 5.e-4_rn*max(1.0_rn,MesswertSVUCH(kEGr))) then
        log_str = '            ' // T('There are deviations between output quantity values!!')
        call write_text_file(text=log_str, full_filename=EditorFileUcheck)
    end if

    call write_text_file(text="", full_filename=EditorFileUcheck)

    write(log_str,'(a)') '  i   Symbol          unit_old   unit_new     MVal_scd/org   MVals_org       MVals_scd       StdUnc_org      StdUnc_scd '
    call write_text_file(text=log_str, full_filename=EditorFileUcheck)
    write(log_str,'(200a)') ('-', i=1, 128)
    call write_text_file(text=log_str, full_filename=EditorFileUcheck)

    do i=1,ngrs
        symb = Symbole(i)%s
        styp = symtyp(i)%s
        ein = Einheit(i)%s
        if(i > nab) ein = Einheit_conv(i)%s
        einSVUCH = EinheitSVUCH(i)%s
        ratio = Messwert(i) / MesswertSVUCH(i)
        call find_power(ratio,10,cfakt,ifehl)
        if(ifehl == 1) call find_power(ratio,60,cfakt,ifehl)
        cratio = '   ' // adjustL(cfakt)

        if(i <= nab) then
            write(log_str,'(i3,1x,a1,1x,3(a,1x),a15,1x,4(es15.8,1x),es11.4)') i,styp,Symb,EinSVUCH, &
            !!  Ein, uconv(i), MesswertSVUCH(i),Messwert(i)*uconv(i), &
            !!  Ein, unit_conv_fact(i), MesswertSVUCH(i),Messwert(i)*unit_conv_fact(i), &
                Ein, cratio, MesswertSVUCH(i),Messwert(i), &
                StdUncSVUCH(i),StdUnc(i)    ! unit_conv_fact(i)
        else
            write(log_str,'(i3,1x,a1,1x,3(a,1x),a15,1x,4(es15.8,1x),es11.4)') i,styp,Symb,EinSVUCH, &
            ! Ein, unit_conv_fact(i), MesswertSVUCH(i),MEsswert(i), &
                Ein, cratio, MesswertSVUCH(i),MEsswert(i), &
                StdUncSVUCH(i),StdUnc(i)
        end if
        call write_text_file(text=log_str, full_filename=EditorFileUcheck)
    end do

    write(log_str,'(200a)') ('-', i=1, 128)
    call write_text_file(text=log_str, full_filename=EditorFileUcheck)
    call write_text_file(text="", full_filename=EditorFileUcheck)
    call write_text_file(text=T('modified Equations') // ': ', full_filename=EditorFileUcheck)
    call write_text_file(text="", full_filename=EditorFileUcheck)


    do i=1,size(Formeltext)
        write(log_str,'(i2,a,a)') i,':  ',Formeltext(i)%s
        call write_text_file(text=log_str, full_filename=EditorFileUcheck)
    end do

    deallocate(PUnitMsg)

    if(npMsg > 0 .and. .not. FP_for_units) then
        ! restore now the previous/original units existing pror to this test:
        einheit(1:ngrs+ncov) = einheitSV(1:ngrs+ncov)
        unit_conv_fact(1:ngrs+ncov) = 1.0_rn
        Messwert(1:ngrs+ncov) = MesswertSVUCH(1:ngrs+ncov)
        StdUnc(1:ngrs+ncov) = StdUncSVUCH(1:ngrs+ncov)
        SDwert(1:ngrs+ncov) = SDwertSVUCH(1:ngrs+ncov)
        HBreite(1:ngrs+ncov) = HBreiteSVUCH(1:ngrs+ncov)

        do i=nab,kEGr,-1
            call WTreeViewPutStrCell('treeview1', 4, i, Einheit(i)%s)
            call WTreeViewPutStrCell('treeview2', 4, i, Einheit(i)%s)
        end do
    end if

end subroutine Report_Ucheck

!################################################################################

subroutine find_power(x,base,cfakt,ifehl)
    !  find the number of the form base^(+-nexp) which represents the value x

    use UR_types
    implicit none

    real(rn),intent(in)           :: x
    integer   ,intent(in)         :: base
    character(len=10),intent(out) :: cfakt
    integer   ,intent(out)        :: ifehl

    integer          :: nexp
    real(rn)         :: y
    character(len=2) :: cop

    ifehl = 0
    y = x
    if(abs(x - 1.0_rn) < 1.E-6_rn) then
        cfakt = '1'
        return
    end if

    nexp = 0
    cop = '  '
    if(x < 1.0_rn) cop = '1/'
    do
        nexp = nexp + 1
        if(nexp > 10) exit
        if(x < 1.0_rn) y = y * real(base,rn)
        if(x > 1.0_rn) y = y / real(base,rn)
        if((base == 10 .and. nexp > 6) .or. (base == 60 .and. nexp >= 3)) then
            ifehl = 1
            exit
        end if
        if(abs(y-1.0_rn) < 1.E-6_rn ) then
            if(base == 10) then
                if(nexp == 1) then
                    cfakt = cop//'10'
                    return
                elseif(nexp == 2) then
                    cfakt = cop//'100'
                    return
                elseif(nexp == 3) then
                    cfakt = cop//'1000'
                    return
                elseif(nexp > 3 .and. nexp <= 6 ) then
                    write(cfakt,'(a2,es7.1)') cop,real(base**nexp,rn)
                    return
                endif
            elseif(base == 60) then
                if(nexp == 1) then
                    cfakt = cop//'60'
                    return
                elseif(nexp == 2) then
                    cfakt = cop//'3600'
                    return
                endif
            endif
        end if
    end do

    if(x < 10._rn) write(cfakt,'(f7.5)') x
    if(x >= 10._rn .and. x < 100._rn) write(cfakt,'(f7.4)') x
    if(x >= 100._rn) write(cfakt,'(es10.3)') x


end subroutine find_power
