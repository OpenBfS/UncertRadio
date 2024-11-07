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
subroutine PrepReport()

    ! Prepares a complete report of the project's input data and evaluated data,
    ! displays it in the programs text editor and writes it into the file
    ! Report.txt
    ! uses WriteTitelTExt

    !     Copyright (C) 2014-2024  Günter Kanisch
    use UR_types
    use ur_variables,       only: langg, fname, results_path

    use ur_gleich
    use ur_linft
    use ur_dlim
    use ur_gspk1fit
    use ur_mcc
    use top,                only: finditems
    use rout,               only: wdgettextviewstring, wtreeviewgetdoublearray, wdgetlabelstring,  &
                                  wdgetentryint, wdgetcheckbutton, wdgetentrydouble
    use urdate,             only: get_formated_date_time
    use ur_interfaces,      only: processloadpro_new
    use ur_params,          only: EPS1MIN
    use ur_variables,       only: kmodeltype,cmodeltype, ur_version_tag
    use rdsubs,             only: writemdvec
    use uwb,                only: rebuildequations
    use chf,                only: flfu
    use translation_module, only: T => get_translation


    implicit none

    integer   , parameter   :: izlen = 105       ! maximum length of a written row

    integer                 :: i,i1,i2,izeil,izeilmax,j,ios,ii
    integer                 :: k,klinx,filen,ker,ncitem,unit,nsymaxlen
    integer                 :: nsdif,ifk1,ifk

    character(len=izlen)    :: textzeile
    character(:),allocatable  :: textdata
    character(len=82)       :: htext
    character(len=11)       :: cmesswert,csdwert,chalb,cstdunc
    character(len=12)       :: csensi,cperc,ccovarval,cicovtyp
    character(len=11)       :: civtl
    character(len=3)        :: ciar
    character(len=60)       :: cnegativ,cbci
    real(rn),allocatable    :: xstdunc(:)
    real(rn)                :: stabw_lsq,stabw_prop,dhelp
    real(rn),allocatable    :: fval_k(:),fuval_k(:)            ! (40)
    character(LEN=25)       :: nonconv
    character(len=12)       :: tchx(6)
    character(len=60)       :: csymb,sdf,csymba,csymbb,cvf,einh
    character(len=105)      :: empty
    character(len=60),allocatable  :: symbname(:)
    character(:),allocatable :: fnamek,str1,sunit,sunit2
    !-----------------------------------------------------------------------
    ! for output of uncertainties to the TAB Results,
    ! these uncertainties are already multiplied by coverf

    if(knumEGr == 0 .or. ngrs == 0) return

    allocate(symbname(ngrs))
    allocate(character(len=256)  :: fnamek)
    allocate(character(len=105)  :: str1)
    allocate(character(len=150)  :: sunit,sunit2)

    allocate(xstdunc(1))

    unit = 15
    izeilmax = 78
    ifehl = 0
    close (unit)

    open(unit, file=flfu(results_path) // 'Report.txt', iostat=ios)             ! 19.6.2024
    if(ios /= 0) then
        ifehl = 1
        return
    end if
    empty = ' '
    izeil = 1

    !-----------------------------------------------------------------------
    fnamek = fname
    filen = len_trim(fnamek)
    i1 = 1
    i2 = filen

    if(knumegr > 1 .and. kegr /= 1) then
        kEGr = 1
        call FindItemS('QFirst', ncitem)
        call ProcMenu(ncitem)
    end if

    if(filen > 50) i2= 50


    write(unit,*) T('Date') // ": " // get_formated_date_time() // &
                    T('Project') // ": ",trim(fnamek(i1:i2))


    do while (i2 < filen)
        i1 = i2 + 1
        i2 = i1 + 99
        i2 = MIN(i2,filen)
        write(unit,'(42x,a)') trim(fnamek(i1:i2))
    end do
    write(unit,'(a,a)') 'UR2: ', trim(UR_version_tag)
    write(unit,'(1x)')
    write(unit,'(a)') T('Procedure') // ": "

    write(unit,'(104a1)') ('-',i=1,104)
    write(unit,'(1x)')
    izeil = 5

    call WDGetTextviewString('textview1', Titeltext)

    call WriteTiteltext(unit,izlen,izeil)
!-----------------------------------------------------------------------
    write(unit,'(/)')
    write(unit,'(a)') T('Equations') // ':'

    write(unit,'(104a1)') ('-',i=1,104)
    write(unit,'(1x)')
    izeil = izeil + 5

    call RebuildEquations(ifk1,ifk)
    do i=1,ifk1
        write(unit,'(a)') Formeltext(i)%s
        izeil = izeil + 1
    end do
    if(FitDecay) then
        do i=ifk1+1,ifk
            write(unit,'(a)') FormeltextFit(i-ifk1)%s
            izeil = izeil + 1
        end do
    end if

!-----------------------------------------------------------------------
    if(izeil + ngrs > 78) then
        izeil = 0
        write(unit,'(a1)') CHAR(12)        ! Form feed
    end if

    write(unit,'(/)')
    write(unit,'(a,i1,a,3(a,a1,2x))') T('number of output quantities') // ": ", knumEGr,' :   ',  &
                                      (trim(Symbole(k)%s),',',k=1,knumEGr)

    write(unit,'(a,a )') T('Symbol of actual output quantity') // ": ", &
                         trim(Symbole(kEGr)%s)

    write(unit,'(a,i1)') T('number of counting channels') // ": ", nchannels

    write(unit,'(a,a)') T('Model type'),cModelType(kModelType)

    write(unit,'(a,f5.3)') 'GamDistAdd                              : ',GamDistAdd

    izeil = izeil + 4
    izeil = izeil + 1
    if(FitDecay) then
        fitmeth = 'WLS'
        if(kPearson == 1) fitmeth = 'PLSQ'
        if(kPMLE == 1) fitmeth = 'PMLE'
        if(use_WTLS) fitmeth = 'WTLS'
        ! if(kPMLE == 1) fitmeth = 'Poiss. MLE'
        write(unit,*)
        write(unit,'(a,a)') T('LSQ fitting method used') // ": ",fitmeth

        izeil = izeil + 2
    end if


    nsymaxlen = 0
    do i=1,ngrs
        nsymaxlen = max(nsymaxlen,len_trim(Symbole(i)%s)+1)
        symbname(i) = symbole(i)%s
    end do
    nsymaxlen = min(nsymaxlen,25)
    nsdif = max(0, 25 -  nsymaxlen)

    write(unit,'(/)')
    write(unit,'(a,/)') T('Symbol table') //" "// T('(type of symbol: dependent (a) or independent(u)):')

    write(sunit,'(a)') T('Symbols                  Type Unit       Meaning')

    write(unit,'(a,a)') sunit(1:nsymaxlen), trim(sunit(26:))

    write(unit,'(104a1)') ('-',i=1,104)
    izeil = izeil + 4

    do i=1,ngrs
        i1 = len_trim(Bedeutung(i)%s)
        i2 = 0
        htext = trim(Bedeutung(i)%s)
        if(i1 > 62+nsdif) then
            do j=62+nsdif,40,-1
                if(Bedeutung(i)%s(j:j) == ' ') then
                    i2 = j - 1
                    htext = trim(Bedeutung(i)%s(1:i2))
                    exit
                end if
            end do
        end if

        ! write(sunit,'(a,T26,1x,a1,2x,a,T40,2x,a)') Symbole(i)%s,symtyp(i)%s, &
        write(sunit,'(a,T26,1x,a1,2x,a,T40,2x,a)') trim(symbname(i)(1:25)),symtyp(i)%s, &
            Einheit(i)%s,trim(htext)  ! Bedeutung(i)%s
        write(unit,'(a,a)') sunit(1:nsymaxlen), trim(sunit(26:))
        izeil = izeil + 1
        if(i2 > 0) then
            write(unit,'(a,a)') empty(1:104-len_trim(Bedeutung(i)%s(i2+1:))), &
                trim(Bedeutung(i)%s(i2+1:))
            izeil = izeil + 1
        end if
    end do
!-----------------------------------------------------------------------
    write(unit,'(/)')

    if(knetto(kEGr) > 0) then
        if(langg == 'DE') write(unit,'(a,a)') 'Symbol der Netto-Zählrate            : ', &
            trim(Symbole(knetto(kEGr))%s)
        if(langg == 'EN') write(unit,'(a,a)') 'Symbol of net count rate             : ', &
            trim(Symbole(knetto(kEGr))%s)
        if(langg == 'FR') write(unit,'(a,a)') 'Symbole du taux de comptage net      : ', &
            trim(Symbole(knetto(kEGr))%s)
    else
        if(langg == 'DE') write(unit,'(a,a)') 'Symbol der Netto-Zählrate            : nicht verwendet'
        if(langg == 'EN') write(unit,'(a,a)') 'Symbol of net count rate             : not used'
        if(langg == 'FR') write(unit,'(a,a)') 'Symbole du taux de comptage net      : non utilisé'
    end if
    if(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) then
        if(kbrutto(kEGr) >0) then
            if(langg == 'DE') write(unit,'(a,a)') 'Symbol der Brutto-Zählrate           : ', &
                trim(Symbole(kbrutto(kEGr))%s)
            if(langg == 'EN') write(unit,'(a,a)') 'Symbol of gross count rate           : ', &
                trim(Symbole(kbrutto(kEGr))%s)
            if(langg == 'FR') write(unit,'(a,a)') 'Symbole du taux de comptage brut     : ', &
                trim(Symbole(kbrutto(kEGr))%s)
        else
            if(langg == 'DE') write(unit,'(a,a)') 'Symbol der Brutto-Zählrate           : nicht verwendet'
            if(langg == 'EN') write(unit,'(a,a)') 'Symbol of gross count rate           : not used'
            if(langg == 'FR') write(unit,'(a,a)') 'Symbole du taux de comptage brut     : non utilisé'
        end if
        if(kbrutto_gl(kEGr) > 0) then
            if(langg == 'DE') write(unit,'(a,a,a)')     &
                'Std.Abw.-Formel der Brutto-Zählrate  : ',trim(Rseite(kbrutto_gl(kEGr))%s), &
                '   <- Unsicherheits-Funktion'
            if(langg == 'EN') write(unit,'(a,a,a)')     &
                'Std.Dev. formula of gross count rate : ',trim(Rseite(kbrutto_gl(kEGr))%s), &
                '   <- Uncertainty function'
            if(langg == 'FR') write(unit,'(a,a,a)')     &
                'Formule de Std.Dev du taux de comptage brut : ',trim(Rseite(kbrutto_gl(kEGr))%s), &
                '   <- Fonction d''incertitude'
        end if

        izeil = izeil + 2
    end if
    write(unit,'(1x)')
    izeil = izeil + 4

    if(langg == 'DE') write(unit,'(a)') 'Eingabedaten für Mittelwerte:'
    if(langg == 'EN') write(unit,'(a)') 'Input data of means:'
    if(langg == 'FR') write(unit,'(a)') 'Données d''entrée pour les valeurs moyennes:'
    izeil = izeil + 1

    allocate(character(len=1200) :: textdata)
    do i=1,nvarsMD
        call writeMDvec(i,.false.,-1,textdata)
        ii = 0
        do
            ii = ii + 1
            if(len_trim(textdata) > 94) then
                do k=94,1,-1
                    if(textdata(k:k) == ' ') then
                        str1 = adjustL(textdata(1:k))
                        if(ii == 1) write(unit,'(a)') trim(str1)
                        if(ii > 1) write(unit,'(9x,a)') trim(str1)
                        izeil= izeil + 1
                        textdata = adjustL(textdata(k:))
                        exit
                    end if
                end do
            else
                str1 = adjustL(textdata)
                if(ii == 1) write(unit,'(a)') trim(str1)
                if(ii > 1) write(unit,'(9x,a)') trim(str1)
                izeil= izeil + 1
                exit
            end if
        end do
    end do
    deallocate(textdata)
    write(unit,'(1x)')
    izeil = izeil + 1

!-----------------------------------------------------------------------
    if(izeil + ngrs > 78) then
        izeil = 0
        write(unit,'(a1)') CHAR(12)        ! Form feed
    end if

    write(unit,'(1x)')
    if(langg == 'DE') write(unit,'(a)') 'Messwerte, Unsicherheiten ' &
        // '(Typ des Symbols: abhängig (a) oder unabhängig (u)):'
    if(langg == 'EN') write(unit,'(a)') 'Measured values, uncertainties ' &
        // '(type of symbol: dependent (a) or independent(u)):'
    if(langg == 'FR') write(unit,'(a)') 'Valeurs mesurées, incertitudes ' &
        // '(type de symbole: dépendant (a) ou indépendant (u)):'
    write(unit,'(1x)')
    izeil = izeil + 3

    if(langg == 'DE') write(sunit,'(a)') &
        'Symbol    Typ  Messwert   Vertl. Std.Abw.-Formel      Std.-Abweichung  Halbbreite        Stand. '
    if(langg == 'DE') write(sunit2,'(a)') &
        '                           Typ                                                           Unsicherht.'

    if(langg == 'EN') write(sunit,'(a)') &
        'Symbol    type value      distr. Std.Dev formula       std.-deviation  half-width        stand. '
    if(langg == 'EN') write(sunit2,'(a)') &
        '                           Type                                                          uncertainty'

    if(langg == 'FR') write(sunit,'(a)') &
        'Symbole   type valeur     distr. Std.Dev formule          écarts-type  demi-largeur      stand. '
    if(langg == 'FR') write(sunit2,'(a)') &
        '                           Type                                                          incertitude'

    nsdif = max(0, nsymaxlen - 10)

    write(unit,'(3a)') sunit(1:9),empty(1:nsdif),trim(sunit(10:))
    write(unit,'(3a)') sunit2(1:9),empty(1:nsdif),trim(sunit2(10:))

    write(unit,'(104a1)') ('-',i=1,104)
    izeil = izeil + 2

    do i=1,ngrs
        cmesswert = ' '
        if(abs(messwert(i)-missingval)>EPS1MIN) write(cmesswert,'(es11.4)') real(messwert(i),8)

        civtl = ' '
        if(IVTL(i) > 0) civtl = vdopt(IVTL(i))%s

        csdwert = ' '
        if(abs(SDWert(i)-missingval) > EPS1MIN) write(csdwert,'(es11.4)') real(SDWert(i),8)
        chalb = '  '
        if(abs(HBreite(i)-missingval) > EPS1MIN) write(chalb,'(es11.4)') real(HBreite(i),8)
        if(IAR(i) == 1) ciar = 'abs'
        if(IAR(i) == 2) ciar = 'rel'
        cstdunc = '  '
        if(abs(StdUnc(i)-missingval) > EPS1MIN) write(cstdunc,'(es11.4)') real(StdUnc(i),8)
        write(csymb,'(a)') Symbole(i)%s(1:min(len(csymb),len(Symbole(i)%s)))
        write(sdf,'(a)') sdformel(i)%s(1:min(len(sdf),len(sdformel(i)%s)))
        csymb = adjustl(csymb)
        sdf = adjustl(sdf)

        write(textzeile,25) csymb(1:nsymaxlen),symtyp(i)%s,cmesswert,civtl(1:7),   &
            sdf(1:20),csdwert,chalb,ciar,cstdunc
25      FORMAT(a,T11,1x,a1,2x,a,2x,a7,1x,a,T56,2x,a,2x,a,2x,a,2x,a)
        write(unit,'(a)') trim(Textzeile)

    end do
!-----------------------------------------------------------------------
    if(izeil + ncov > 78) then
        izeil = 0
        write(unit,'(a1)') CHAR(12)        ! Form feed
    end if

    write(unit,'(1x)')
    if(ncov > 0) then
        if(langg == 'DE') write(unit,'(a)') 'Kovarianzen/Korrelationen:'
        if(langg == 'EN') write(unit,'(a)') 'Covariances/correlations:'
        if(langg == 'FR') write(unit,'(a)') 'Covariances / Corrélations:'
    else
        if(langg == 'DE') write(unit,'(a)') 'Kovarianzen/Korrelationen: es sind keine definiert'
        if(langg == 'EN') write(unit,'(a)') 'Covariances/correlations : none defined'
        if(langg == 'FR') write(unit,'(a)') 'Covariances / corrélations: aucune définie'
    end if
    write(unit,'(1x)')
    izeil = izeil + 3

    if(ncov > 0) then
        if(langg == 'DE') write(unit,'(a)')  &
            'Symbol A        Symbol B         Typ          Formel                     (oder) Wert'
        if(langg == 'EN') write(unit,'(a)')  &
            'Symbol A        Symbol B         Type         Formula                    (or) value'
        if(langg == 'FR') write(unit,'(a)')  &
            'Symbole A       Symboll B        Type         Formule                    (ou) valeur'

        write(unit,'(89a1)') ('-',i=1,82)
        izeil = izeil + 2

        do i=1,ncov
            if(icovtyp(i) == 1) then
                if(langg == 'DE') cicovtyp = 'Kovarianz'
                if(langg == 'EN') cicovtyp = 'Covariance'
                if(langg == 'FR') cicovtyp = 'Covariance'
            end if
            if(icovtyp(i) == 2) then
                if(langg == 'DE') cicovtyp = 'Korrelation'
                if(langg == 'EN') cicovtyp = 'Correlation'
                if(langg == 'FR') cicovtyp = 'Corrélation'
            end if
            cCovarVal = ' '
            if(abs(CovarVal(i)-missingval) > EPS1MIN) then
                dhelp = CovarValSV(i)
                if(icovtyp(i) == 2) dhelp = dhelp / StdUnc(ISymbA(i)) / StdUnc(ISymbB(i))
                write(cCovarVal,'(es11.4)') real(dhelp,8)
            end if

            write(csymba,'(a)') SymboleA(i)%s
            write(csymbb,'(a)') SymboleB(i)%s
            write(cvf,'(a)') CVFormel(i)%s
            csymba = adjustL(csymba)
            csymbb = adjustL(csymbb)
            cvf = adjustL(cvf)

            write(textzeile,45) csymba(1:15),csymbb(1:15),cicovtyp, &
                CVF(1:25),ccovarVal
45          FORMAT(a15,1x,a15,2x,a,2x,a25,2x,a,2x,a,2x,a,2x,a,2x,a)
            write(unit,'(a)') trim(Textzeile)

            if(len_trim(CVFormel(i)%s) > 25) then
                write(unit,'(26x,a,a)') T('complete formula') // ': ',trim(CVFormel(i)%s)

            end if

        end do
    end if
!-----------------------------------------------------------------------
    if(FitDecay) then

        if(izeil + numd + 8 > 78) then
            izeil = 0
            write(unit,'(a1)') CHAR(12)        ! Form feed
        end if

        write(unit,'(1x)')
        write(unit,'(a)') T('Decay curve  - Input data') // ": "
        write(unit,'(1x)')
        izeil = izeil + 3

        write(unit,'(a,a)') T('Separation date') // ": ", CFaelldatum

        izeil = izeil + 2
        if(langg == 'DE') write(unit,470)
470     FORMAT('Datum+Uhrzeit             Messzeit   Brutto-    BruttoZrate  urel(BZrate)',/, &
            '                           ( s )     impulse      (cps)          ( % )')
        if(langg == 'EN') write(unit,473)
473     FORMAT('Date + Time              count time  gross     gross c.rate  urel gc.rate',/, &
            '                           ( s )     counts       (cps)          ( % )  ')
        if(langg == 'FR') write(unit,474)
474     FORMAT('Date + Temps             temps compt  brut     brut taux     urel brut taux',/, &
            '                           ( s )     compte       (cps)          ( % )  ')
        write(unit,'(104a1)') ('-',i=1,70)
        izeil = izeil + 3

        do i=1,numd
            write(unit,48) trim(CStartzeit(i)%s),real(dmesszeit(i),8),real(dbimpulse(i),8),real(dbzrate(i),8),   &
                real(sdbzrate(i)/dbzrate(i)*100._rn,8)
48          FORMAT(a,T21,5x,f8.0,3x,f8.0,3x,es11.4,2x,4x,f6.2)
            izeil = izeil + 1
        end do
        write(unit,'(a)') T('Continuation of table:')

        izeil = izeil + 1
        if(langg == 'DE') write(unit,476)
476     FORMAT('Messzeit    Nullefkt-  NE-Zrate   urel(NErate) NetRate  urel(NetRate)',/, &
            ' ( s )      impulse    (cps)        ( % )      (cps)         ( % )   ')
        if(langg == 'EN') write(unit,478)
478     FORMAT('count time  backgrd    back.rate  urel(Brate) net rate  urel(NetRate)',/, &
            ' ( s )      counts     (cps)        ( % )      (cps)         ( % ) ')
        if(langg == 'FR') write(unit,479)
479     FORMAT('temps compt brdefon    tauxBrdef  urel(tauxB) net taux  urel(NetTaux)',/, &
            ' ( s )      compte     (cps)        ( % )      (cps)         ( % ) ')

        write(unit,'(104a1)') ('-',i=1,70)
        izeil = izeil + 3
        do i=1,numd
            write(unit,49) real(d0messzeit(i),8),real(d0impulse(i),8),real(d0zrate(i),8),   &
                real(sd0zrate(i)/d0zrate(i)*100._rn,8),real(dnetrate(i),8),  &
                real(sdnetrate(i)/dnetrate(i)*100._rn,8)
49          FORMAT(f8.0,3x,f8.0,3x,es11.4,2x,f6.2,4x,es11.4,3x,f6.2)
            izeil = izeil + 1
        end do

        open(22, file=flfu(results_path) // 'linfout.txt', status='unknown')

        write(unit,'(1x)')
        izeil = izeil + 1

        if(izeil + numd + 12 > 78) then
            izeil = 0
            write(unit,'(a1)') CHAR(12)        ! Form feed
        end if

        do
            READ(22,'(a)',IOSTAT=ios) textzeile
            if(ios /= 0) exit
            write(unit,'(a)') trim(textzeile)
            izeil = izeil + 1
        end do
        close (22)

    end if

    if(Gamspk1_Fit) then
        open(22, file=flfu(results_path) // 'linfout.txt', status='unknown')

        write(unit,'(1x)')
        izeil = izeil + 1

        if(izeil + numd + 12 > 78) then
            izeil = 0
            write(unit,'(a1)') CHAR(12)        ! Form feed
        end if

        do
            READ(22,'(a)',IOSTAT=ios) textzeile
            if(ios /= 0) exit
            write(unit,'(a)') trim(textzeile)
            izeil = izeil + 1
        end do
        close (22)

    end if


    if(izeil+ngrs+ncov+numd > 78) then
        izeil = 0
        write(unit,'(a1)') CHAR(12)        ! Form feed
    end if
!-----------------------------------------------------------------------

    if(FitCalCurve) then

        if(KFitcal > 0) then
            write(unit,'(1x)')
            write(unit,'(a,a,a)') T('Calibration curve used for symbol') // ": ", trim(symbole(kfitcal)%s),' :'

            write(unit,'(1x)')
            izeil = izeil + 3
        end if

        write(unit,480)
480     FORMAT(' i   x(i)         u(x(i))        y(i)         u(y(i))        Fit           u(Fit)')
        write(unit,'(104a1)') ('-',i=1,104)
        izeil = izeil + 2
        if(allocated(fval_k)) deallocate(fval_k,fuval_k)
        call WTreeViewGetDoubleArray('treeview7', 6, nkalpts, fval_k)
        call WTreeViewGetDoubleArray('treeview7', 7, nkalpts, fuval_k)

        do i=1,nkalpts
            tchx(1) = ' '
            if(abs(xkalib(i)-missingval)>EPS1MIN) write(tchx(1),'(es12.5)') real(xkalib(i),8)
            tchx(2) = ' '
            if(abs(uxkalib(i)-missingval)>EPS1MIN) write(tchx(2),'(es12.5)') real(uxkalib(i),8)
            tchx(3) = ' '
            if(abs(ykalib(i)-missingval)>EPS1MIN) write(tchx(3),'(es12.5)') real(ykalib(i),8)
            tchx(4) = ' '
            if(abs(uykalib(i)-missingval)>EPS1MIN) write(tchx(4),'(es12.5)') real(uykalib(i),8)
            write(tchx(5),'(es12.5)') real(fval_k(i),8)
            write(tchx(6),'(es12.5)') real(fuval_k(i),8)
            write(unit,'(i2,2x,6(a,2x))') i,(tchx(k),k=1,6)
            izeil = izeil + 1
        end do
        write(unit,'(104a1)') ('-',i=1,104)
        izeil = izeil + 1

        call WDGetLabelString('DKlabelFparms', str1)
        write(unit,'(3x,a)') trim(str1)

        call WDGetLabelString('DKlabelFsdev', str1)
        write(unit,'(3x,a)') trim(str1)

        call WDGetLabelString('DKlabelChisqr', str1)
        write(unit,'(3x,a)') trim(str1)
        izeil = izeil + 3

    end if

!-----------------------------------------------------------------------
    ker = 0

270 CONTINUE

    ker = ker + 1

    if(ker > knumEGr) GOTO 9000
    if(FitDecay .and. ifit(ker) > 1) goto 270

    if(ker > 0) then
        kEGr = ker
        if(ker == 2 .and. ifit(2) > 1) goto 270
        if(ker == 3 .and. ifit(3) > 1) goto 270
        call ProcessLoadPro_new(1, ker)
        write(66,*) '....................   kEGr=',kEgr
    end if

    write(unit,'(/)')
    write(unit,'(a)') T('Uncertainty budget') // " " // T('for') // " " // trim(Symbole(kEGr)%s) // ' :'

    write(unit,'(1x)')
    izeil = izeil + 4

    nsdif = max(0, nsymaxlen - 15)
    select case (Ucontyp)
      case (1)
        if(langg == 'DE') then
            write(sunit,280)
280         FORMAT('Symbole        Typ Einheit             Werte          Standard-     Sensitiv.-      Relativer')
            write(sunit2,281)
281         FORMAT('                                                      Unsicherht   koeffizient      Beitrag(%)')
        end if
        if(langg == 'EN') then
            write(sunit,283)
283         FORMAT('Symbols       Type Unit                Values         Standard      Sensitivity      relative')
            write(sunit2,284)
284         FORMAT('                                                      uncertainty   coefficient     contribution(%)')
        end if
        if(langg == 'FR') then
            write(sunit,285)
285         FORMAT('Symboles      Type Unité               Valeurs        Standard      sensibilité      relative')
            write(sunit2,286)
286         FORMAT('                                                      incertitude   coefficient     contribution(%)')
        end if

        write(unit,'(3a)') sunit(1:15),empty(1:nsdif),trim(sunit(16:))
        write(unit,'(3a)') sunit2(1:15),empty(1:nsdif),trim(sunit2(16:))


      case (2)
        if(langg == 'DE') then
            write(sunit,291)
291         FORMAT('Symbole        Typ Einheit             Werte          Standard-     Sensitiv.-      absoluter')
            write(sunit2,292)
292         FORMAT('                                                      Unsicherht   koeffizient      Beitrag   ')
        end if
        if(langg == 'EN') then
            write(sunit,294)
294         FORMAT('Symbols       Type Unit                Values         Standard      Sensitivity      absolute')
            write(sunit2,295)
295         FORMAT('                                                      uncertainty   coefficient     contribution')
        end if
        if(langg == 'FR') then
            write(sunit,296)
296         FORMAT('Symboles      Type Unité               Valeurs        Standard      sensibilité      absolu')
            write(sunit2,297)
297         FORMAT('                                                      incertitude   coefficient     contribution')
        end if
        write(unit,'(3a)') sunit(1:15),empty(1:nsdif),trim(sunit(16:))
        write(unit,'(3a)') sunit2(1:15),empty(1:nsdif),trim(sunit2(16:))

      case default
    end select
    write(unit,'(104a1)') ('-',i=1,104)

    izeil = izeil + 3
    call WTreeViewGetDoubleArray('treeview4',6, ngrs+ncov+numd, xstdunc)
    call WTreeViewGetDoubleArray('treeview4',7, ngrs+ncov+numd, sensi)
    if(Ucontyp == 1) call WTreeViewGetDoubleArray('treeview4',8, ngrs+ncov+numd, perc)
    if(Ucontyp == 2) call WTreeViewGetDoubleArray('treeview4',8, ngrs+ncov+numd, Ucontrib)
    do i=1,ngrs+ncov+numd
        if(i > ngrs .AND. i <= ngrs+ncov) Messwert(i) = CovarVal(i-ngrs)
        cmesswert = ' '
        if(abs(messwert(i)-missingval) > EPS1MIN) write(cmesswert,'(es11.4)') real(messwert(i),8)
        cstdunc = '  '
        if(abs(StdUnc(i)-missingval) > EPS1MIN) write(cstdunc,'(es11.4)') real(StdUnc(i),8)
        if(abs(StdUnc(i)) < EPS1MIN) cstdunc = ' 0.000'
        csensi = '  '
        if(abs(sensi(i)-missingval) > EPS1MIN) write(csensi,'(es11.4)') real(sensi(i),8)
        if(abs(sensi(i)) < EPS1MIN) csensi = ' 0.000'
        select case (Ucontyp)
          case (1)
            cperc = '  '
            if(abs(perc(i)-missingval) > EPS1MIN) write(cperc,'(es11.4)') real(perc(i),8)
            if(abs(perc(i)) < EPS1MIN) cperc = ' 0.000'
          case (2)
            cperc = '  '
            if(abs(Ucontrib(i)-missingval) > EPS1MIN) write(cperc,'(es11.4)') real(Ucontrib(i),8)
            if(abs(Ucontrib(i)) < EPS1MIN) cperc = ' 0.000'
        end select

        write(csymb,'(a)') trim(Symbole(i)%s)
        write(einh,'(a)') trim(einheit(i)%s)

        csymb = adjustl(csymb)
        einh =  adjustl(einh)

        write(textzeile,32) csymb(1:15),symtyp(i)%s,einh(1:15),  &
            cmesswert,cstdunc,csensi,cperc
32      FORMAT(a15,1x,a1,2x,a15,4x,a,4x,a,4x,a,4x,a)
        write(unit,'(a)') trim(Textzeile)
    end do
!-----------------------------------------------------------------------
    if(izeil+12 > 78) then
        izeil = 0
        write(unit,'(a1)') CHAR(12)        ! Form feed
    end if

    write(unit,'(1x)')
    write(unit,'(1x)')
    write(unit,'(a,a,a)') 'Final measurement result for' //": ",trim(Symbole(kEGr)%s),':'

    write(unit,'(104a1)') ('-',i=1,104)
    write(unit,'(1x)')
    izeil = izeil + 5
    write(unit,'(a,f7.4,/     )') T('Coverage factor k:') // ": " ,coverf

    write(unit,'(a,es16.9,1x,a)') T('Value') //": ", &
                                  Messwert(kEGr), trim(einheit(kEGr)%s)

    ! Ucomb already contains the factor coverf:
    write(unit,'(a,es16.9,1x,a,2x,a,f7.3,a)') T('extendend uncertainty:'), &
                                              Ucomb,trim(einheit(kEGr)%s),             &
                                              '(',Ucomb/Messwert(kEGr)*100.,'%)'

    write(unit,'(/,a)') T('Best Bayesian estimates') // ":"

    write(unit,'(a,es16.9,1x,a)') T('Value') //": ", &
                                  WertBayes,trim(einheit(kEGr)%s)

    ! UcombBayes already contains the factor coverf:
    write(unit,'(a,es16.9,1x,a,2x,a,f7.3,a)') T('extendend uncertainty:'), &
                                              UcombBayes,trim(einheit(kEGr)%s),           &
                                              '(',UcombBayes/WertBayes*100.,'%)'

    if(langg == 'DE') write(unit,'(/,a,es16.9,1x,a,6x,a)') 'untere Bereichsgrenze   : ',KBgrenzu,trim(einheit(kEGr)%s), &
        '(Probabilistisch-symmetrisch)'
    if(langg == 'EN') write(unit,'(/,a,es16.9,1x,a,6x,a)') 'lower range limit       : ',KBgrenzu,trim(einheit(kEGr)%s), &
        '(Probabilistically symmetric)'
    if(langg == 'FR') write(unit,'(/,a,es16.9,1x,a,6x,a)') 'limite inf. de la plage : ',KBgrenzu,trim(einheit(kEGr)%s), &
        '(Probablement symétrique)'
    if(langg == 'DE') write(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'obere  Bereichsgrenze   : ',KBgrenzo,trim(einheit(kEGr)%s), &
        '(Länge=',KBgrenzo-KBgrenzu,')'
    if(langg == 'EN') write(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'upper range limit       : ',KBgrenzo,trim(einheit(kEGr)%s), &
        '(Length=',KBgrenzo-KBgrenzu,')'
    if(langg == 'FR') write(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'limite sup. de la plage : ',KBgrenzo,trim(einheit(kEGr)%s), &
        '(Longueur=',KBgrenzo-KBgrenzu,')'

    if(langg == 'DE') write(unit,'(a,es16.9,1x,a,6x,a)') 'untere Bereichsgrenze   : ',KBgrenzuSH,trim(einheit(kEGr)%s), &
        '(Kürzestes Überdeckungsintervall)'
    if(langg == 'EN') write(unit,'(a,es16.9,1x,a,6x,a)') 'lower range limit       : ',KBgrenzuSH,trim(einheit(kEGr)%s), &
        '(Shortest coverage interval)'
    if(langg == 'FR') write(unit,'(a,es16.9,1x,a,6x,a)') 'limite inf. de la plage : ',KBgrenzuSH,trim(einheit(kEGr)%s), &
        '(Intervalle de couverture le plus court)'

    if(langg == 'DE') write(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'obere Bereichsgrenze    : ',  &
        KBgrenzoSH,trim(einheit(kEGr)%s), '(Länge=',KBgrenzoSH-KBgrenzuSH,')'
    if(langg == 'EN') write(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'upper range limit       : ',  &
        KBgrenzoSH,trim(einheit(kEGr)%s), '(Length=',KBgrenzoSH-KBgrenzuSH,')'
    if(langg == 'FR') write(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'limite sup. de la plage : ',  &
        KBgrenzoSH,trim(einheit(kEGr)%s), '(Longeur=',KBgrenzoSH-KBgrenzuSH,')'

    if(langg == 'DE') write(unit,'(  a,f8.5)')        '  Wahrscheinlichkeit (1-gamma): ',W1minusG
    if(langg == 'EN') write(unit,'(  a,f8.5)')        '  Probability        (1-gamma): ',W1minusG
    if(langg == 'FR') write(unit,'(  a,f8.5)')        '  Probabilité        (1-gamma): ',W1minusG


    if(langg == 'DE') write(unit,'(/,a,7x,a,es16.9,1x,a,1x,a,1x,i3)') 'Erkennungsgrenze (DT)',' : ', &
        decthresh,trim(einheit(kEGr)%s),'; Iterationen:',nit_decl
    if(langg == 'EN') write(unit,'(/,a,5x,a,es16.9,1x,a,1x,a,1x,i3)') 'Decision threshold (DT)',' : ', &
        decthresh,trim(einheit(kEGr)%s),'; Iterations :',nit_decl
    if(langg == 'FR') write(unit,'(/,a,5x,a,es16.9,1x,a,1x,a,1x,i3)') 'Seuil de décision (DT)',' : ', &
        decthresh,trim(einheit(kEGr)%s),'; Itérations :',nit_decl

    nonconv = ' '
    if(nit_detl >= nit_detl_max) then
        if(langg == 'DE') nonconv = 'nicht konvergent!'
        if(langg == 'EN') nonconv = 'not convergent!'
        if(langg == 'FR') nonconv = 'Pas convergent!'
    end if

    if(langg == 'DE') write(unit,'(a,9x,a,es16.9,1x,a,1x,a,1x,i3,2x,a)') 'Nachweisgrenze (DL)',' : ',   &
        detlim,trim(einheit(kEGr)%s),'; Iterationen:',nit_detl,trim(nonconv)
    if(langg == 'EN') write(unit,'(a,8x,a,es16.9,1x,a,1x,a,1x,i3,2x,a)') 'Detection limit (DL)',' : ',   &
        detlim,trim(einheit(kEGr)%s),'; Iterations :',nit_detl,trim(nonconv)
    if(langg == 'FR') write(unit,'(a,3x,a,es16.9,1x,a,1x,a,1x,i3,2x,a)') 'Limite de détection (DL)',' : ',   &
        detlim,trim(einheit(kEGr)%s),'; Itérations :',nit_detl,trim(nonconv)

    if(langg == 'DE') write(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'k_alpha=',kalpha,', k_beta=', &
        kbeta,'  Methode: ',trim(NWGMeth)
    if(langg == 'EN') write(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'k_alpha=',kalpha,', k_beta=', &
        kbeta,'  Method : ',trim(NWGMeth)
    if(langg == 'FR') write(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'k_alpha=',kalpha,', k_beta=', &
        kbeta,'  Méthode : ',trim(NWGMeth)
    if(langg == 'DE') write(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'alpha=',alpha,', beta=',beta
    if(langg == 'EN') write(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'alpha=',alpha,', beta=',beta
    if(langg == 'FR') write(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'alpha=',alpha,', beta=',beta

    call WDGetEntryInt('TRentryMCanzM', kcmx)
    call WDGetEntryInt('TRentryMCanzR', kcrun)

    call WDGetEntryDouble('TRentryMCValue', xmit1)
    call WDGetEntryDouble('TRentryMCunc', xsdv)           ! contains already coverf

    call WDGetEntryDouble('TRentryMCValueRSD', rxmit1)
    call WDGetEntryDouble('TRentryMCuncRSD', rxsdv)

    if(xmit1 > 0._rn .AND. xsdv > 0._rn) then
        cbci = ' '
        if ( use_BCI ) then
            cbci = "(" // T('shortest range') // ")"
        end if

        write(unit,'(1x)')
        if(langg == 'EN') write(unit,'(a)') T('Monte Carlo Simulation:')
        if(langg == 'DE') write(unit,'(a,i8,a,i2,a,a,a)') 'Anzahl Messungen pro Run: ', kcmx,';  ', &
            kcrun,' Runs;','  ',trim(cnegativ)
        if(langg == 'EN') write(unit,'(a,i8,a,i2,a,a,a)') 'Number measurem. per run: ', kcmx,';  ', &
            kcrun,' Runs;','  ',trim(cnegativ)
        if(langg == 'FR') write(unit,'(a,i8,a,i2,a,a,a)') 'nombre de mesures par course: ', kcmx,';  ', &
            kcrun,' Courses;','  ',trim(cnegativ)

        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'primärer Wert           : ', &
            xxmit1PE,trim(einheit(kEGr)%s),'rel.SD%: ',rxmit1PE
        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'primary value           : ', &
            xxmit1PE,trim(einheit(kEGr)%s),'rel.SD%: ',rxmit1PE
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'valeur primaire         : ', &
            xxmit1PE,trim(einheit(kEGr)%s),'rel.SD%: ',rxmit1PE

        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Erweiterte Unsicherheit : ', &
        !                                  xsdvPE,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE
            xxsdvPE,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE

        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Expanded uncertainty    : ', &
        !                                  xsdvPE,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE
            xxsdvPE,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Incertitude étendue     : ', &
        !                                  xsdvPE,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE
            xxsdvPE,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE

        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'bester Wert             : ', &
            xxmit1,trim(einheit(kEGr)%s),'rel.SD%: ',rxmit1
        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'best value              : ', &
            xxmit1,trim(einheit(kEGr)%s),'rel.SD%: ',rxmit1
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'meilleure valeur        : ', &
            xxmit1,trim(einheit(kEGr)%s),'rel.SD%: ',rxmit1

        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Erweiterte Unsicherheit : ', &
            xxsdv,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdv
        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Expanded uncertainty    : ', &
            xxsdv,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdv
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Incertitude étendue     : ', &
            xxsdv,trim(einheit(kEGr)%s),'rel.SD%: ',rxsdv

        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'Untere Bereichsgrenze   : ', &
            xLQ,trim(einheit(kEGr)%s),'rel.SD%: ',rxLQ,trim(cbci), &
            '(Probabilistisch-symmetrisch)'
        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'Lower range limit       : ', &
            xLQ,trim(einheit(kEGr)%s),'rel.SD%: ',rxLQ,trim(cbci), &
            '(Probabilistic symmetric)'
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'limite inf. de la plage : ', &
            xLQ,trim(einheit(kEGr)%s),'rel.SD%: ',rxLQ,trim(cbci), &
            '(Probabilistic symmetric)'
        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'Obere Bereichsgrenze    : ', &
            xUQ,trim(einheit(kEGr)%s),'rel.SD%: ',rxUQ,trim(cbci), &
            '(Länge=',xUQ - xLQ,')'

        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'Upper range limit       : ', &
            xUQ,trim(einheit(kEGr)%s),'rel.SD%: ',rxUQ,trim(cbci), &
            '(Length=',xUQ - xLQ,')'
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'limite sup. de la plage : ', &
            xUQ,trim(einheit(kEGr)%s),'rel.SD%: ',rxUQ,trim(cbci), &
            '(Longeur=',xUQ- xLQ,')'

        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'Untere Bereichsgrenze   : ', &
            est1LQ_BCI,trim(einheit(kEGr)%s),'rel.SD%: ',rx1LQbci,trim(cbci), &
            '(Kürzestes Überdeckungsintervall)'
        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'Lower range limit       : ', &
            est1LQ_BCI,trim(einheit(kEGr)%s),'rel.SD%: ',rx1LQbci,trim(cbci), &
            '(Shortest coverage interval)'
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'limite inf. de la plage : ', &
            est1LQ_BCI,trim(einheit(kEGr)%s),'rel.SD%: ',rx1LQbci,trim(cbci), &
            '(Intervalle de couverture le plus court)'

        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'Obere Bereichsgrenze    : ', &
            est1UQ_BCI,trim(einheit(kEGr)%s),'rel.SD%: ',rx1UQbci,trim(cbci), &
            '(Länge=',est1UQ_BCI-est1LQ_BCI,')'
        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'Upper range limit       : ', &
            est1UQ_BCI,trim(einheit(kEGr)%s),'rel.SD%: ',rx1UQbci,trim(cbci), &
            '(Length=',est1UQ_BCI-est1LQ_BCI,')'
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'limite sup. de la plage : ', &
            est1UQ_BCI,trim(einheit(kEGr)%s),'rel.SD%: ',rx1UQbci,trim(cbci), &
            '(Longeur=',est1UQ_BCI-est1LQ_BCI,')'

        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Erkennungsgrenze (EKG)  : ', &
            xDT,trim(einheit(kEGr)%s),'rel.SD%: ',rxDT
        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Decision threshold (DT) : ', &
            xDT,trim(einheit(kEGr)%s),'rel.SD%: ',rxDT
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Seuil de décision (DT)  : ', &
            xDT,trim(einheit(kEGr)%s),'rel.SD%: ',rxDT
        if(langg == 'DE') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Nachweisgrenze (NWG)    : ', &
            xDL,trim(einheit(kEGr)%s),'rel.SD%: ',rxDL
        if(langg == 'EN') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Detection limit (DL)    : ', &
            xDL,trim(einheit(kEGr)%s),'rel.SD%: ',rxDL
        if(langg == 'FR') write(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Limite de détection (DL): ', &
            xDL,trim(einheit(kEGr)%s),'rel.SD%: ',rxDL

    end if

    if(FitDecay) then
        call WDGetEntryDouble('TRentryUfit', stabw_lsq)
        call WDGetEntryDouble('TRentryUprop', stabw_prop)
        call WDGetEntryDouble('TRentryChisqr', Chisqr)

        klinx = klinf
        if(kfitp(1) > 0) klinx = kfitp(1)-1+kEGr
        write(unit,'(1x)')
        if(langg == 'DE') write(unit,'(5a)') 'Standardabweichung der mit ',trim(fitmeth), &
            ' bestimmten Größe ',trim(Symbole(klinx)%s),' :'
        if(langg == 'EN') write(unit,'(5a)') 'Standard deviation of the quantity ',   &
            trim(Symbole(klinx)%s),' obtained by ',trim(fitmeth),' :'
        if(langg == 'FR') write(unit,'(5a)') 'Écart-type de la quantité ',   &
            trim(Symbole(klinx)%s),' obtenu par ',trim(fitmeth),' :'

        if(langg == 'DE') write(unit,'(4x,a,es11.4,1x,a)') 'aus der LSQ-Analyse              : ', &
            stabw_lsq, Einheit(klinx)%s
        if(langg == 'EN') write(unit,'(4x,a,es11.4,1x,a)') 'from LSQ analysis                : ', &
            stabw_lsq, Einheit(klinx)%s
        if(langg == 'FR') write(unit,'(4x,a,es11.4,1x,a)') 'de l''analyse LSQ                : ', &
            stabw_lsq, Einheit(klinx)%s
        if(langg == 'DE') write(unit,'(4x,a,es11.4,1x,a)') 'aus Unsicherheiten-Fortpflanzung : ', &
            stabw_prop, Einheit(klinx)%s
        if(langg == 'EN') write(unit,'(4x,a,es11.4,1x,a)') 'from uncertainty propagation     : ', &
            stabw_prop, Einheit(klinx)%s
        if(langg == 'FR') write(unit,'(4x,a,es11.4,1x,a)') 'de la propagation de l''incertitude : ', &
            stabw_prop, Einheit(klinx)%s
        if(langg == 'DE') write(unit,'(4x,a,f7.4)') 'reduziertes Chi-Quadrat          : ',Chisqr
        if(langg == 'EN') write(unit,'(4x,a,f7.4)') 'reduced Chi-squared              : ',Chisqr
        if(langg == 'FR') write(unit,'(4x,a,f7.4)') 'Chi-carré réduit                 : ',Chisqr
    end if

    write(unit,'(104a1)') ('-',i=1,104)

    GOTO 270      ! For the next output quantity, begin at label 270

!-----------------------------------------------------------------------

9000 CONTINUE

    close (unit)

end subroutine PrepReport

!#######################################################################


subroutine WriteTiteltext(unit,izlen,izeil)

!     Copyright (C) 2014-2024  Günter Kanisch

    use UR_Gleich,            only: Titeltext
    implicit none

    integer   ,intent(in)       :: unit
    integer   ,intent(in)       :: izlen
    integer   ,intent(inout)    :: izeil

    integer                   :: i,jj,k1,i1,i2,i3,jjmax
    character(:),allocatable  :: buffer

    do jj=size(Titeltext),2,-1
        if(len_trim(Titeltext(jj)%s) > 2) then
            jjmax = jj
            exit
        end if
    end do

    do jj=1,jjmax     ! size(Titeltext)
        if(allocated(buffer)) deallocate(buffer)
        allocate(character(len=1200) :: buffer)

        buffer = Titeltext(jj)%s
        ! if(len_trim(buffer) == 0) cycle
        do
            k1 = index(buffer,char(13))
            if(k1 == 0) exit
            if(len_trim(buffer) == 1 ) then
                if(buffer(1:1) == ' ') then
                    buffer = char(13)//char(10)
                end if
            else
                if(buffer(k1+1:k1+1) /= char(10)) buffer = buffer(1:k1-1)//buffer(k1+1:)
            end if
        end do
        i1 = 1
        i2 = 0
        do
            i1 = 1
            i2 = 0
            i3 = MIN(i1+izlen-1,len_trim(buffer))
            if(len_trim(buffer) == 0) then
                write(unit,*)
                exit
            end if
            if(i2 == 0) then
                if(i3 == izlen) then
                    do i=i3,MAX(i1,i3-40),-1
                        if(buffer(i:i) == ' ' .OR. buffer(i:i) == '-' ) then
                            write(unit,'(a)') buffer(i1:i)
                            izeil = izeil + 1
                            buffer = trim(buffer(i+1:))
                            exit
                        end if
                    end do
                else
                    write(unit,'(a)') buffer(i1:i3)
                    izeil = izeil + 1
                    buffer = trim(buffer(i3+1:))
                end if
            end if
            if(len_trim(buffer) == 0) then
                !write(unit,*)
                exit
            end if
        end do      ! endless loop
    end do
    if(allocated(buffer)) deallocate (buffer)

end subroutine WriteTiteltext
