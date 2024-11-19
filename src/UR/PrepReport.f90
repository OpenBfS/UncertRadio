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

    !     Copyright (C) 2014-2023  Günter Kanisch
    use UR_types
    use ur_variables,       only: langg, fname, results_path, wpunix

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
    write(66,*) 'wpunix=',wpunix

    open(unit,file=flfu(results_path) // 'Report.txt', iostat=ios)             ! 19.6.2024
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


    if(langg == 'DE' .or. langg == 'FR') then
        write(unit, '(A)') 'Datum: ' // get_formated_date_time() // &
                           'Projekt: ',TRIM(fnamek(i1:i2))
    else if(langg == 'EN') then
        write(unit, '(A)') 'Date : ' // get_formated_date_time() // &
                           'Project: ',TRIM(fnamek(i1:i2))
    end if

    do while (i2 < filen)
        i1 = i2 + 1
        i2 = i1 + 99
        i2 = MIN(i2,filen)
        write(unit,'(42x,a)') TRIM(fnamek(i1:i2))
    end do
    write(unit,'(a,a)') 'UR2: ',trim(UR_version_tag)
    write(unit,'(1x)')

    IF(langg == 'DE') WRITE(unit,'(a)') 'Verfahren:'
    IF(langg == 'EN') WRITE(unit,'(a)') 'Procedure:'
    IF(langg == 'FR') WRITE(unit,'(a)') 'Procédure:'
    WRITE(unit,'(104a1)') ('-',i=1,104)
    WRITE(unit,'(1x)')
    izeil = 5

    call WDGetTextviewString('textview1', Titeltext)

    call WriteTiteltext(unit,izlen,izeil)
!-----------------------------------------------------------------------
    WRITE(unit,'(/)')
    IF(langg == 'DE') WRITE(unit,'(a)') 'Gleichungen:'
    IF(langg == 'EN') WRITE(unit,'(a)') 'Equations:'
    IF(langg == 'FR') WRITE(unit,'(a)') 'Équations:'
    WRITE(unit,'(104a1)') ('-',i=1,104)
    WRITE(unit,'(1x)')
    izeil = izeil + 5

    call RebuildEquations(ifk1,ifk)
    do i=1,ifk1
        WRITE(unit,'(a)') Formeltext(i)%s
        izeil = izeil + 1
    end do
    if(FitDecay) then
        do i=ifk1+1,ifk
            WRITE(unit,'(a)') FormeltextFit(i-ifk1)%s
            izeil = izeil + 1
        end do
    end if

!-----------------------------------------------------------------------
    IF(izeil + ngrs > 78) THEN
        izeil = 0
        WRITE(unit,'(a1)') CHAR(12)        ! Form feed
    END IF

    WRITE(unit,'(/)')
    IF(langg == 'DE') WRITE(unit,'(a,i1,a,3(a,a1,2x))') 'Anzahl der Ergebnisgrößen               : ',knumEGr,' :   ',  &
        (TRIM(Symbole(k)%s),',',k=1,knumEGr)
    IF(langg == 'EN') WRITE(unit,'(a,i1,a,3(a,a1,2x))') 'Number of output quantities             : ',knumEGr,' :   ',  &
        (TRIM(Symbole(k)%s),',',k=1,knumEGr)
    IF(langg == 'FR') WRITE(unit,'(a,i1,a,3(a,a1,2x))') 'Nombre de quantités de sortie           : ',knumEGr,' :   ',  &
        (TRIM(Symbole(k)%s),',',k=1,knumEGr)

    IF(langg == 'DE') WRITE(unit,'(a,a )') 'Symbol der aktuellen Ergebnisgröße      : ', &
        TRIM(Symbole(kEGr)%s)
    IF(langg == 'EN') WRITE(unit,'(a,a )') 'Symbol of actual output quantity        : ', &
        TRIM(Symbole(kEGr)%s)
    IF(langg == 'FR') WRITE(unit,'(a,a )') 'Symbole de la quantité de sortie réelle : ', &
        TRIM(Symbole(kEGr)%s)
    IF(langg == 'DE') WRITE(unit,'(a,i1)') 'Anzahl der Messkanäle                   : ',nchannels
    IF(langg == 'EN') WRITE(unit,'(a,i1)') 'Number of counting channels             : ',nchannels
    IF(langg == 'FR') WRITE(unit,'(a,i1)') 'Nombre de canaux de comptage            : ',nchannels

    IF(langg == 'DE') WRITE(unit,'(a,a)') 'ModellTyp                               : ',cModelType(kModelType)
    IF(langg == 'EN') WRITE(unit,'(a,a)') 'Model Type                              : ',cModelType(kModelType)
    IF(langg == 'FR') WRITE(unit,'(a,a)') 'Type de modèle                          : ',cModelType(kModelType)
    WRITE(unit,'(a,f5.3)') 'GamDistAdd                              : ',GamDistAdd

    izeil = izeil + 4
    izeil = izeil + 1
    IF(FitDecay) THEN
        fitmeth = 'WLS'
        IF(kPearson == 1) fitmeth = 'PLSQ'
        IF(kPMLE == 1) fitmeth = 'PMLE'
        IF(use_WTLS) fitmeth = 'WTLS'
        ! IF(kPMLE == 1) fitmeth = 'Poiss. MLE'
        WRITE(unit,*)
        IF(langg == 'DE') WRITE(unit,'(a,a)') 'verwendete LSQ-Fitmethode            : ',fitmeth
        IF(langg == 'EN') WRITE(unit,'(a,a)') 'LSQ fitting method used              : ',fitmeth
        IF(langg == 'FR') WRITE(unit,'(a,a)') 'LSQ méthode de montage utilisée      : ',fitmeth
        izeil = izeil + 2
    end if


    nsymaxlen = 0
    do i=1,ngrs
        nsymaxlen = max(nsymaxlen,len_trim(Symbole(i)%s)+1)
        symbname(i) = symbole(i)%s
    end do
    nsymaxlen = min(nsymaxlen,25)
    nsdif = max(0, 25 -  nsymaxlen)

    WRITE(unit,'(/)')
    IF(langg == 'DE') WRITE(unit,'(a,/)') 'Symbol-Tabelle ' &
        // '(Typ des Symbols: abhängig (a) oder unabhängig (u)):'
    IF(langg == 'EN') WRITE(unit,'(a,/)') 'Symbol table ' &
        // '(type of symbol: dependent (a) or independent(u)):'
    IF(langg == 'FR') WRITE(unit,'(a,/)') 'Table de symboles ' &
        // '(type de symbole: dépendant (a) ou indépendant (u)):'


    IF(langg == 'DE') WRITE(sunit,'(a)') 'Symbole                  Typ Einheit     Bedeutung'
    IF(langg == 'EN') WRITE(sunit,'(a)') 'Symbols                  Type Unit       Meaning  '
    IF(langg == 'FR') WRITE(sunit,'(a)') 'Symboles                 Type Unité      Signification  '
    write(unit,'(a,a)') sunit(1:nsymaxlen), trim(sunit(26:))

    WRITE(unit,'(104a1)') ('-',i=1,104)
    izeil = izeil + 4

    do i=1,ngrs
        i1 = LEN_TRIM(Bedeutung(i)%s)
        i2 = 0
        htext = trim(Bedeutung(i)%s)
        IF(i1 > 62+nsdif) THEN
            do j=62+nsdif,40,-1
                IF(Bedeutung(i)%s(j:j) == ' ') THEN
                    i2 = j - 1
                    htext = TRIM(Bedeutung(i)%s(1:i2))
                    EXIT
                END IF
            end do
        END IF

        ! WRITE(sunit,'(a,T26,1x,a1,2x,a,T40,2x,a)') Symbole(i)%s,symtyp(i)%s, &
        WRITE(sunit,'(a,T26,1x,a1,2x,a,T40,2x,a)') trim(symbname(i)(1:25)),symtyp(i)%s, &
            Einheit(i)%s,trim(htext)  ! Bedeutung(i)%s
        write(unit,'(a,a)') sunit(1:nsymaxlen), trim(sunit(26:))
        izeil = izeil + 1
        IF(i2 > 0) THEN
            write(unit,'(a,a)') empty(1:104-len_trim(Bedeutung(i)%s(i2+1:))), &
                trim(Bedeutung(i)%s(i2+1:))
            izeil = izeil + 1
        END IF
    end do
!-----------------------------------------------------------------------
    WRITE(unit,'(/)')

    IF(knetto(kEGr) > 0) THEN
        IF(langg == 'DE') WRITE(unit,'(a,a)') 'Symbol der Netto-Zählrate            : ', &
            TRIM(Symbole(knetto(kEGr))%s)
        IF(langg == 'EN') WRITE(unit,'(a,a)') 'Symbol of net count rate             : ', &
            TRIM(Symbole(knetto(kEGr))%s)
        IF(langg == 'FR') WRITE(unit,'(a,a)') 'Symbole du taux de comptage net      : ', &
            TRIM(Symbole(knetto(kEGr))%s)
    else
        IF(langg == 'DE') WRITE(unit,'(a,a)') 'Symbol der Netto-Zählrate            : nicht verwendet'
        IF(langg == 'EN') WRITE(unit,'(a,a)') 'Symbol of net count rate             : not used'
        IF(langg == 'FR') WRITE(unit,'(a,a)') 'Symbole du taux de comptage net      : non utilisé'
    END IF
    IF(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit) THEN
        IF(kbrutto(kEGr) >0) then
            IF(langg == 'DE') WRITE(unit,'(a,a)') 'Symbol der Brutto-Zählrate           : ', &
                TRIM(Symbole(kbrutto(kEGr))%s)
            IF(langg == 'EN') WRITE(unit,'(a,a)') 'Symbol of gross count rate           : ', &
                TRIM(Symbole(kbrutto(kEGr))%s)
            IF(langg == 'FR') WRITE(unit,'(a,a)') 'Symbole du taux de comptage brut     : ', &
                TRIM(Symbole(kbrutto(kEGr))%s)
        else
            IF(langg == 'DE') WRITE(unit,'(a,a)') 'Symbol der Brutto-Zählrate           : nicht verwendet'
            IF(langg == 'EN') WRITE(unit,'(a,a)') 'Symbol of gross count rate           : not used'
            IF(langg == 'FR') WRITE(unit,'(a,a)') 'Symbole du taux de comptage brut     : non utilisé'
        END IF
        IF(kbrutto_gl(kEGr) > 0) THEN
            IF(langg == 'DE') WRITE(unit,'(a,a,a)')     &
                'Std.Abw.-Formel der Brutto-Zählrate  : ',TRIM(Rseite(kbrutto_gl(kEGr))%s), &
                '   <- Unsicherheits-Funktion'
            IF(langg == 'EN') WRITE(unit,'(a,a,a)')     &
                'Std.Dev. formula of gross count rate : ',TRIM(Rseite(kbrutto_gl(kEGr))%s), &
                '   <- Uncertainty function'
            IF(langg == 'FR') WRITE(unit,'(a,a,a)')     &
                'Formule de Std.Dev du taux de comptage brut : ',TRIM(Rseite(kbrutto_gl(kEGr))%s), &
                '   <- Fonction d''incertitude'
        END IF

        izeil = izeil + 2
    END IF
    WRITE(unit,'(1x)')
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
    IF(izeil + ngrs > 78) THEN
        izeil = 0
        WRITE(unit,'(a1)') CHAR(12)        ! Form feed
    END IF

    WRITE(unit,'(1x)')
    IF(langg == 'DE') WRITE(unit,'(a)') 'Messwerte, Unsicherheiten ' &
        // '(Typ des Symbols: abhängig (a) oder unabhängig (u)):'
    IF(langg == 'EN') WRITE(unit,'(a)') 'Measured values, uncertainties ' &
        // '(type of symbol: dependent (a) or independent(u)):'
    IF(langg == 'FR') WRITE(unit,'(a)') 'Valeurs mesurées, incertitudes ' &
        // '(type de symbole: dépendant (a) ou indépendant (u)):'
    WRITE(unit,'(1x)')
    izeil = izeil + 3

    IF(langg == 'DE') WRITE(sunit,'(a)') &
        'Symbol    Typ  Messwert   Vertl. Std.Abw.-Formel      Std.-Abweichung  Halbbreite        Stand. '
    IF(langg == 'DE') WRITE(sunit2,'(a)') &
        '                           Typ                                                           Unsicherht.'

    IF(langg == 'EN') WRITE(sunit,'(a)') &
        'Symbol    type value      distr. Std.Dev formula       std.-deviation  half-width        stand. '
    IF(langg == 'EN') WRITE(sunit2,'(a)') &
        '                           Type                                                          uncertainty'

    IF(langg == 'FR') WRITE(sunit,'(a)') &
        'Symbole   type valeur     distr. Std.Dev formule          écarts-type  demi-largeur      stand. '
    IF(langg == 'FR') WRITE(sunit2,'(a)') &
        '                           Type                                                          incertitude'

    nsdif = max(0, nsymaxlen - 10)

    write(unit,'(3a)') sunit(1:9),empty(1:nsdif),trim(sunit(10:))
    write(unit,'(3a)') sunit2(1:9),empty(1:nsdif),trim(sunit2(10:))

    WRITE(unit,'(104a1)') ('-',i=1,104)
    izeil = izeil + 2

    do i=1,ngrs
        cmesswert = ' '
        IF(abs(messwert(i)-missingval)>EPS1MIN) WRITE(cmesswert,'(es11.4)') real(messwert(i),8)

        civtl = ' '
        IF(IVTL(i) > 0) civtl = vdopt(IVTL(i))%s

        csdwert = ' '
        IF(abs(SDWert(i)-missingval) > EPS1MIN) WRITE(csdwert,'(es11.4)') real(SDWert(i),8)
        chalb = '  '
        IF(abs(HBreite(i)-missingval) > EPS1MIN) WRITE(chalb,'(es11.4)') real(HBreite(i),8)
        IF(IAR(i) == 1) ciar = 'abs'
        IF(IAR(i) == 2) ciar = 'rel'
        cstdunc = '  '
        IF(abs(StdUnc(i)-missingval) > EPS1MIN) WRITE(cstdunc,'(es11.4)') real(StdUnc(i),8)
        write(csymb,'(a)') Symbole(i)%s(1:min(len(csymb),len(Symbole(i)%s)))
        write(sdf,'(a)') sdformel(i)%s(1:min(len(sdf),len(sdformel(i)%s)))
        csymb = adjustl(csymb)
        sdf = adjustl(sdf)

        WRITE(textzeile,25) csymb(1:nsymaxlen),symtyp(i)%s,cmesswert,civtl(1:7),   &
            sdf(1:20),csdwert,chalb,ciar,cstdunc
25      FORMAT(a,T11,1x,a1,2x,a,2x,a7,1x,a,T56,2x,a,2x,a,2x,a,2x,a)
        WRITE(unit,'(a)') TRIM(Textzeile)

    end do
!-----------------------------------------------------------------------
    IF(izeil + ncov > 78) THEN
        izeil = 0
        WRITE(unit,'(a1)') CHAR(12)        ! Form feed
    END IF

    WRITE(unit,'(1x)')
    IF(ncov > 0) THEN
        IF(langg == 'DE') WRITE(unit,'(a)') 'Kovarianzen/Korrelationen:'
        IF(langg == 'EN') WRITE(unit,'(a)') 'Covariances/correlations:'
        IF(langg == 'FR') WRITE(unit,'(a)') 'Covariances / Corrélations:'
    else
        IF(langg == 'DE') WRITE(unit,'(a)') 'Kovarianzen/Korrelationen: es sind keine definiert'
        IF(langg == 'EN') WRITE(unit,'(a)') 'Covariances/correlations : none defined'
        IF(langg == 'FR') WRITE(unit,'(a)') 'Covariances / corrélations: aucune définie'
    END IF
    WRITE(unit,'(1x)')
    izeil = izeil + 3

    IF(ncov > 0) THEN
        IF(langg == 'DE') WRITE(unit,'(a)')  &
            'Symbol A        Symbol B         Typ          Formel                     (oder) Wert'
        IF(langg == 'EN') WRITE(unit,'(a)')  &
            'Symbol A        Symbol B         Type         Formula                    (or) value'
        IF(langg == 'FR') WRITE(unit,'(a)')  &
            'Symbole A       Symboll B        Type         Formule                    (ou) valeur'

        WRITE(unit,'(89a1)') ('-',i=1,82)
        izeil = izeil + 2

        do i=1,ncov
            IF(icovtyp(i) == 1) THEN
                IF(langg == 'DE') cicovtyp = 'Kovarianz'
                IF(langg == 'EN') cicovtyp = 'Covariance'
                IF(langg == 'FR') cicovtyp = 'Covariance'
            END IF
            IF(icovtyp(i) == 2) THEN
                IF(langg == 'DE') cicovtyp = 'Korrelation'
                IF(langg == 'EN') cicovtyp = 'Correlation'
                IF(langg == 'FR') cicovtyp = 'Corrélation'
            END IF
            cCovarVal = ' '
            IF(abs(CovarVal(i)-missingval) > EPS1MIN) THEN
                dhelp = CovarValSV(i)
                IF(icovtyp(i) == 2) dhelp = dhelp / StdUnc(ISymbA(i)) / StdUnc(ISymbB(i))
                WRITE(cCovarVal,'(es11.4)') real(dhelp,8)
            END IF

            write(csymba,'(a)') SymboleA(i)%s
            write(csymbb,'(a)') SymboleB(i)%s
            write(cvf,'(a)') CVFormel(i)%s
            csymba = adjustL(csymba)
            csymbb = adjustL(csymbb)
            cvf = adjustL(cvf)

            WRITE(textzeile,45) csymba(1:15),csymbb(1:15),cicovtyp, &
                CVF(1:25),ccovarVal
45          FORMAT(a15,1x,a15,2x,a,2x,a25,2x,a,2x,a,2x,a,2x,a,2x,a)
            WRITE(unit,'(a)') TRIM(Textzeile)

            IF(LEN_TRIM(CVFormel(i)%s) > 25) THEN
                IF(langg == 'DE') WRITE(unit,'(26x,a,a)') 'komplette Formel : ',TRIM(CVFormel(i)%s)
                IF(langg == 'EN') WRITE(unit,'(26x,a,a)') 'complete formula : ',TRIM(CVFormel(i)%s)
                IF(langg == 'FR') WRITE(unit,'(26x,a,a)') 'formule complète : ',TRIM(CVFormel(i)%s)
            end if

        end do
    END IF
!-----------------------------------------------------------------------
    IF(FitDecay) THEN

        IF(izeil + numd + 8 > 78) THEN
            izeil = 0
            WRITE(unit,'(a1)') CHAR(12)        ! Form feed
        END IF

        WRITE(unit,'(1x)')
        IF(langg == 'DE') WRITE(unit,'(a)') 'Abklingkurve - Eingangsdaten:'
        IF(langg == 'EN') WRITE(unit,'(a)') 'Decay curve  - Input data:'
        IF(langg == 'FR') WRITE(unit,'(a)') 'Courbe de décroissance - Données d''entrée:'
        WRITE(unit,'(1x)')
        izeil = izeil + 3

        IF(langg == 'DE') WRITE(unit,'(a,a)') 'Fällungsdatum : ',CFaelldatum
        IF(langg == 'EN') WRITE(unit,'(a,a)') 'Separation date : ',CFaelldatum
        IF(langg == 'FR') WRITE(unit,'(a,a)') 'Date de séparation : ',CFaelldatum
        WRITE(unit,'(1x)')
        izeil = izeil + 2

        IF(langg == 'DE') WRITE(unit,470)
470     FORMAT('Datum+Uhrzeit             Messzeit   Brutto-    BruttoZrate  urel(BZrate)',/, &
            '                           ( s )     impulse      (cps)          ( % )')
        IF(langg == 'EN') WRITE(unit,473)
473     FORMAT('Date + Time              count time  gross     gross c.rate  urel gc.rate',/, &
            '                           ( s )     counts       (cps)          ( % )  ')
        IF(langg == 'FR') WRITE(unit,474)
474     FORMAT('Date + Temps             temps compt  brut     brut taux     urel brut taux',/, &
            '                           ( s )     compte       (cps)          ( % )  ')
        WRITE(unit,'(104a1)') ('-',i=1,70)
        izeil = izeil + 3

        do i=1,numd
            WRITE(unit,48) trim(CStartzeit(i)%s),real(dmesszeit(i),8),real(dbimpulse(i),8),real(dbzrate(i),8),   &
                real(sdbzrate(i)/dbzrate(i)*100._rn,8)
48          FORMAT(a,T21,5x,f8.0,3x,f8.0,3x,es11.4,2x,4x,f6.2)
            izeil = izeil + 1
        end do
        IF(langg == 'DE') WRITE(unit,'(a)') '     Fortsetzung der Tabelle:'
        IF(langg == 'EN') WRITE(unit,'(a)') '     Continuation of table:'
        IF(langg == 'FR') WRITE(unit,'(a)') '     Suite du tableau:'
        izeil = izeil + 1
        IF(langg == 'DE') WRITE(unit,476)
476     FORMAT('Messzeit    Nullefkt-  NE-Zrate   urel(NErate) NetRate  urel(NetRate)',/, &
            ' ( s )      impulse    (cps)        ( % )      (cps)         ( % )   ')
        IF(langg == 'EN') WRITE(unit,478)
478     FORMAT('count time  backgrd    back.rate  urel(Brate) net rate  urel(NetRate)',/, &
            ' ( s )      counts     (cps)        ( % )      (cps)         ( % ) ')
        IF(langg == 'FR') WRITE(unit,479)
479     FORMAT('temps compt brdefon    tauxBrdef  urel(tauxB) net taux  urel(NetTaux)',/, &
            ' ( s )      compte     (cps)        ( % )      (cps)         ( % ) ')

        WRITE(unit,'(104a1)') ('-',i=1,70)
        izeil = izeil + 3
        do i=1,numd
            WRITE(unit,49) real(d0messzeit(i),8),real(d0impulse(i),8),real(d0zrate(i),8),   &
                real(sd0zrate(i)/d0zrate(i)*100._rn,8),real(dnetrate(i),8),  &
                real(sdnetrate(i)/dnetrate(i)*100._rn,8)
49          FORMAT(f8.0,3x,f8.0,3x,es11.4,2x,f6.2,4x,es11.4,3x,f6.2)
            izeil = izeil + 1
        end do

        open(22, file=flfu(results_path) // 'linfout.txt', status='unknown')

        write(unit,'(1x)')
        izeil = izeil + 1

        IF(izeil + numd + 12 > 78) THEN
            izeil = 0
            WRITE(unit,'(a1)') CHAR(12)        ! Form feed
        END IF

        do
            READ(22,'(a)',IOSTAT=ios) textzeile
            IF(ios /= 0) EXIT
            WRITE(unit,'(a)') TRIM(textzeile)
            izeil = izeil + 1
        end do
        close (22)

    END IF

    IF(Gamspk1_Fit) THEN
        open(22, file=flfu(results_path) // 'linfout.txt', status='unknown')

        write(unit,'(1x)')
        izeil = izeil + 1

        IF(izeil + numd + 12 > 78) THEN
            izeil = 0
            WRITE(unit,'(a1)') CHAR(12)        ! Form feed
        END IF

        do
            READ(22,'(a)',IOSTAT=ios) textzeile
            IF(ios /= 0) EXIT
            WRITE(unit,'(a)') TRIM(textzeile)
            izeil = izeil + 1
        end do
        close (22)

    END IF


    IF(izeil+ngrs+ncov+numd > 78) THEN
        izeil = 0
        WRITE(unit,'(a1)') CHAR(12)        ! Form feed
    END IF
!-----------------------------------------------------------------------

    if(FitCalCurve) then

        if(KFitcal > 0) then
            WRITE(unit,'(1x)')
            IF(langg == 'DE') WRITE(unit,'(a,a,a)') 'Kalibrierkurve, für ',trim(symbole(kfitcal)%s),' verwendet:'
            IF(langg == 'EN') WRITE(unit,'(a,a,a)') 'Calibration curve used for ',trim(symbole(kfitcal)%s),' :'
            IF(langg == 'FR') WRITE(unit,'(a,a,a)') 'Courbe d''étalonnage utilisée pour ',trim(symbole(kfitcal)%s),' :'
            WRITE(unit,'(1x)')
            izeil = izeil + 3
        end if

        WRITE(unit,480)
480     FORMAT(' i   x(i)         u(x(i))        y(i)         u(y(i))        Fit           u(Fit)')
        WRITE(unit,'(104a1)') ('-',i=1,104)
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
        WRITE(unit,'(104a1)') ('-',i=1,104)
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

    IF(ker > knumEGr) GOTO 9000
    if(FitDecay .and. ifit(ker) > 1) goto 270

    IF(ker > 0) THEN
        kEGr = ker
        if(ker == 2 .and. ifit(2) > 1) goto 270
        if(ker == 3 .and. ifit(3) > 1) goto 270
        call ProcessLoadPro_new(1, ker)
        write(66,*) '....................   kEGr=',kEgr
    end if

    WRITE(unit,'(/)')
    IF(langg == 'DE') WRITE(unit,'(a)') 'Unsicherheiten-Budget für ' // TRIM(Symbole(kEGr)%s) // ' :'
    IF(langg == 'EN') WRITE(unit,'(a)') 'Uncertainty budget for ' // TRIM(Symbole(kEGr)%s) // ' :'
    IF(langg == 'FR') WRITE(unit,'(a)') 'Budget d''incertitude pour ' // TRIM(Symbole(kEGr)%s) // ' :'
    WRITE(unit,'(1x)')
    izeil = izeil + 4

    nsdif = max(0, nsymaxlen - 15)
    select case (Ucontyp)
      case (1)
        IF(langg == 'DE') then
            WRITE(sunit,280)
280         FORMAT('Symbole        Typ Einheit             Werte          Standard-     Sensitiv.-      Relativer')
            WRITE(sunit2,281)
281         FORMAT('                                                      Unsicherht   koeffizient      Beitrag(%)')
        end if
        IF(langg == 'EN') then
            WRITE(sunit,283)
283         FORMAT('Symbols       Type Unit                Values         Standard      Sensitivity      relative')
            WRITE(sunit2,284)
284         FORMAT('                                                      uncertainty   coefficient     contribution(%)')
        end if
        IF(langg == 'FR') then
            WRITE(sunit,285)
285         FORMAT('Symboles      Type Unité               Valeurs        Standard      sensibilité      relative')
            WRITE(sunit2,286)
286         FORMAT('                                                      incertitude   coefficient     contribution(%)')
        end if

        write(unit,'(3a)') sunit(1:15),empty(1:nsdif),trim(sunit(16:))
        write(unit,'(3a)') sunit2(1:15),empty(1:nsdif),trim(sunit2(16:))


      case (2)
        IF(langg == 'DE') then
            WRITE(sunit,291)
291         FORMAT('Symbole        Typ Einheit             Werte          Standard-     Sensitiv.-      absoluter')
            WRITE(sunit2,292)
292         FORMAT('                                                      Unsicherht   koeffizient      Beitrag   ')
        end if
        IF(langg == 'EN') then
            WRITE(sunit,294)
294         FORMAT('Symbols       Type Unit                Values         Standard      Sensitivity      absolute')
            WRITE(sunit2,295)
295         FORMAT('                                                      uncertainty   coefficient     contribution')
        end if
        IF(langg == 'FR') then
            WRITE(sunit,296)
296         FORMAT('Symboles      Type Unité               Valeurs        Standard      sensibilité      absolu')
            WRITE(sunit2,297)
297         FORMAT('                                                      incertitude   coefficient     contribution')
        end if
        write(unit,'(3a)') sunit(1:15),empty(1:nsdif),trim(sunit(16:))
        write(unit,'(3a)') sunit2(1:15),empty(1:nsdif),trim(sunit2(16:))

      case default
    end select
    WRITE(unit,'(104a1)') ('-',i=1,104)

    izeil = izeil + 3
    call WTreeViewGetDoubleArray('treeview4',6, ngrs+ncov+numd, xstdunc)
    call WTreeViewGetDoubleArray('treeview4',7, ngrs+ncov+numd, sensi)
    if(Ucontyp == 1) call WTreeViewGetDoubleArray('treeview4',8, ngrs+ncov+numd, perc)
    if(Ucontyp == 2) call WTreeViewGetDoubleArray('treeview4',8, ngrs+ncov+numd, Ucontrib)
    do i=1,ngrs+ncov+numd
        IF(i > ngrs .AND. i <= ngrs+ncov) Messwert(i) = CovarVal(i-ngrs)
        cmesswert = ' '
        IF(abs(messwert(i)-missingval) > EPS1MIN) WRITE(cmesswert,'(es11.4)') real(messwert(i),8)
        cstdunc = '  '
        IF(abs(StdUnc(i)-missingval) > EPS1MIN) WRITE(cstdunc,'(es11.4)') real(StdUnc(i),8)
        IF(abs(StdUnc(i)) < EPS1MIN) cstdunc = ' 0.000'
        csensi = '  '
        IF(abs(sensi(i)-missingval) > EPS1MIN) WRITE(csensi,'(es11.4)') real(sensi(i),8)
        IF(abs(sensi(i)) < EPS1MIN) csensi = ' 0.000'
        select case (Ucontyp)
          case (1)
            cperc = '  '
            IF(abs(perc(i)-missingval) > EPS1MIN) WRITE(cperc,'(es11.4)') real(perc(i),8)
            IF(abs(perc(i)) < EPS1MIN) cperc = ' 0.000'
          case (2)
            cperc = '  '
            IF(abs(Ucontrib(i)-missingval) > EPS1MIN) WRITE(cperc,'(es11.4)') real(Ucontrib(i),8)
            IF(abs(Ucontrib(i)) < EPS1MIN) cperc = ' 0.000'
        end select

        write(csymb,'(a)') trim(Symbole(i)%s)
        write(einh,'(a)') trim(einheit(i)%s)

        csymb = adjustl(csymb)
        einh =  adjustl(einh)

        WRITE(textzeile,32) csymb(1:15),symtyp(i)%s,einh(1:15),  &
            cmesswert,cstdunc,csensi,cperc
32      FORMAT(a15,1x,a1,2x,a15,4x,a,4x,a,4x,a,4x,a)
        WRITE(unit,'(a)') TRIM(Textzeile)
    END do
!-----------------------------------------------------------------------
    IF(izeil+12 > 78) THEN
        izeil = 0
        WRITE(unit,'(a1)') CHAR(12)        ! Form feed
    END IF

    WRITE(unit,'(1x)')
    WRITE(unit,'(1x)')
    IF(langg == 'DE') WRITE(unit,'(a,a,a)') 'Gesamtes Messergebnis für ',TRIM(Symbole(kEGr)%s),':'
    IF(langg == 'EN') WRITE(unit,'(a,a,a)') 'Total measurement result for ',TRIM(Symbole(kEGr)%s),':'
    IF(langg == 'FR') WRITE(unit,'(a,a,a)') 'Résultat total de mesure pour ',TRIM(Symbole(kEGr)%s),':'
    WRITE(unit,'(104a1)') ('-',i=1,104)
    WRITE(unit,'(1x)')
    izeil = izeil + 5

    IF(langg == 'DE') WRITE(unit,'(a,f7.4,/     )') 'Erweiterungsfaktor      : ',coverf
    IF(langg == 'EN') WRITE(unit,'(a,f7.4,/     )') 'Coverage factor         : ',coverf
    IF(langg == 'FR') WRITE(unit,'(a,f7.4,/     )') 'Facteur de couverture   : ',coverf

    IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a)') 'Wert                    : ', &
        Messwert(kEGr),TRIM(einheit(kEGr)%s)
    IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a)') 'Value                   : ', &
        Messwert(kEGr),TRIM(einheit(kEGr)%s)
    IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a)') 'Valeur                  : ', &
        Messwert(kEGr),TRIM(einheit(kEGr)%s)

! Ucomb already contains the factor coverf:
    IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f7.3,a)') 'Erweiterte Unsicherheit : ', &
        Ucomb,TRIM(einheit(kEGr)%s),             &
        '(',Ucomb/Messwert(kEGr)*100.,'%)'
    IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f7.3,a)') 'Expanded uncertainty    : ', &
        Ucomb,TRIM(einheit(kEGr)%s),             &
        '(',Ucomb/Messwert(kEGr)*100.,'%)'
    IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f7.3,a)') 'Incertitude étendue     : ', &
        Ucomb,TRIM(einheit(kEGr)%s),             &
        '(',Ucomb/Messwert(kEGr)*100.,'%)'


    IF(langg == 'DE') WRITE(unit,'(/,a)') 'Beste Schätzwerte nach Bayes:'
    IF(langg == 'EN') WRITE(unit,'(/,a)') 'Best Bayesian estimates:'
    IF(langg == 'FR') WRITE(unit,'(/,a)') 'Meilleures estimations bayésiennes:'

    IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a)') 'Wert                    : ', &
        WertBayes,TRIM(einheit(kEGr)%s)
    IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a)') 'Value                   : ', &
        WertBayes,TRIM(einheit(kEGr)%s)
    IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a)') 'Valeur                  : ', &
        WertBayes,TRIM(einheit(kEGr)%s)

! UcombBayes already contains the factor coverf:
    IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f7.3,a)') 'Erweiterte Unsicherheit : ', &
        UcombBayes,TRIM(einheit(kEGr)%s),           &
        '(',UcombBayes/WertBayes*100.,'%)'
    IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f7.3,a)') 'Expanded uncertainty    : ', &
        UcombBayes,TRIM(einheit(kEGr)%s),           &
        '(',UcombBayes/WertBayes*100.,'%)'
    IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f7.3,a)') 'Incertitude étendue     : ', &
        UcombBayes,TRIM(einheit(kEGr)%s),           &
        '(',UcombBayes/WertBayes*100.,'%)'

    IF(langg == 'DE') WRITE(unit,'(/,a,es16.9,1x,a,6x,a)') 'untere Bereichsgrenze   : ',KBgrenzu,TRIM(einheit(kEGr)%s), &
        '(Probabilistisch-symmetrisch)'
    IF(langg == 'EN') WRITE(unit,'(/,a,es16.9,1x,a,6x,a)') 'lower range limit       : ',KBgrenzu,TRIM(einheit(kEGr)%s), &
        '(Probabilistically symmetric)'
    IF(langg == 'FR') WRITE(unit,'(/,a,es16.9,1x,a,6x,a)') 'limite inf. de la plage : ',KBgrenzu,TRIM(einheit(kEGr)%s), &
        '(Probablement symétrique)'
    IF(langg == 'DE') WRITE(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'obere  Bereichsgrenze   : ',KBgrenzo,TRIM(einheit(kEGr)%s), &
        '(Länge=',KBgrenzo-KBgrenzu,')'
    IF(langg == 'EN') WRITE(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'upper range limit       : ',KBgrenzo,TRIM(einheit(kEGr)%s), &
        '(Length=',KBgrenzo-KBgrenzu,')'
    IF(langg == 'FR') WRITE(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'limite sup. de la plage : ',KBgrenzo,TRIM(einheit(kEGr)%s), &
        '(Longueur=',KBgrenzo-KBgrenzu,')'

    IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,6x,a)') 'untere Bereichsgrenze   : ',KBgrenzuSH,TRIM(einheit(kEGr)%s), &
        '(Kürzestes Überdeckungsintervall)'
    IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,6x,a)') 'lower range limit       : ',KBgrenzuSH,TRIM(einheit(kEGr)%s), &
        '(Shortest coverage interval)'
    IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,6x,a)') 'limite inf. de la plage : ',KBgrenzuSH,TRIM(einheit(kEGr)%s), &
        '(Intervalle de couverture le plus court)'

    IF(langg == 'DE') WRITE(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'obere Bereichsgrenze    : ',  &
        KBgrenzoSH,TRIM(einheit(kEGr)%s), '(Länge=',KBgrenzoSH-KBgrenzuSH,')'
    IF(langg == 'EN') WRITE(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'upper range limit       : ',  &
        KBgrenzoSH,TRIM(einheit(kEGr)%s), '(Length=',KBgrenzoSH-KBgrenzuSH,')'
    IF(langg == 'FR') WRITE(unit,'(  a,es16.9,1x,a,8x,a,es16.9,a)') 'limite sup. de la plage : ',  &
        KBgrenzoSH,TRIM(einheit(kEGr)%s), '(Longeur=',KBgrenzoSH-KBgrenzuSH,')'

    IF(langg == 'DE') WRITE(unit,'(  a,f8.5)')        '  Wahrscheinlichkeit (1-gamma): ',W1minusG
    IF(langg == 'EN') WRITE(unit,'(  a,f8.5)')        '  Probability        (1-gamma): ',W1minusG
    IF(langg == 'FR') WRITE(unit,'(  a,f8.5)')        '  Probabilité        (1-gamma): ',W1minusG


    IF(langg == 'DE') WRITE(unit,'(/,a,7x,a,es16.9,1x,a,1x,a,1x,i3)') 'Erkennungsgrenze (DT)',' : ', &
        decthresh,TRIM(einheit(kEGr)%s),'; Iterationen:',nit_decl
    IF(langg == 'EN') WRITE(unit,'(/,a,5x,a,es16.9,1x,a,1x,a,1x,i3)') 'Decision threshold (DT)',' : ', &
        decthresh,TRIM(einheit(kEGr)%s),'; Iterations :',nit_decl
    IF(langg == 'FR') WRITE(unit,'(/,a,5x,a,es16.9,1x,a,1x,a,1x,i3)') 'Seuil de décision (DT)',' : ', &
        decthresh,TRIM(einheit(kEGr)%s),'; Itérations :',nit_decl

    nonconv = ' '
    IF(nit_detl >= nit_detl_max) THEN
        IF(langg == 'DE') nonconv = 'nicht konvergent!'
        IF(langg == 'EN') nonconv = 'not convergent!'
        IF(langg == 'FR') nonconv = 'Pas convergent!'
    end if

    IF(langg == 'DE') WRITE(unit,'(a,9x,a,es16.9,1x,a,1x,a,1x,i3,2x,a)') 'Nachweisgrenze (DL)',' : ',   &
        detlim,TRIM(einheit(kEGr)%s),'; Iterationen:',nit_detl,TRIM(nonconv)
    IF(langg == 'EN') WRITE(unit,'(a,8x,a,es16.9,1x,a,1x,a,1x,i3,2x,a)') 'Detection limit (DL)',' : ',   &
        detlim,TRIM(einheit(kEGr)%s),'; Iterations :',nit_detl,TRIM(nonconv)
    IF(langg == 'FR') WRITE(unit,'(a,3x,a,es16.9,1x,a,1x,a,1x,i3,2x,a)') 'Limite de détection (DL)',' : ',   &
        detlim,TRIM(einheit(kEGr)%s),'; Itérations :',nit_detl,TRIM(nonconv)

    IF(langg == 'DE') WRITE(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'k_alpha=',kalpha,', k_beta=', &
        kbeta,'  Methode: ',TRIM(NWGMeth)
    IF(langg == 'EN') WRITE(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'k_alpha=',kalpha,', k_beta=', &
        kbeta,'  Method : ',TRIM(NWGMeth)
    IF(langg == 'FR') WRITE(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'k_alpha=',kalpha,', k_beta=', &
        kbeta,'  Méthode : ',TRIM(NWGMeth)
    IF(langg == 'DE') WRITE(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'alpha=',alpha,', beta=',beta
    IF(langg == 'EN') WRITE(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'alpha=',alpha,', beta=',beta
    IF(langg == 'FR') WRITE(unit,'(3x,a,f8.6,2x,a,f8.6,2x,a,a)') 'alpha=',alpha,', beta=',beta

    call WDGetEntryInt('TRentryMCanzM', kcmx)
    call WDGetEntryInt('TRentryMCanzR', kcrun)

    call WDGetEntryDouble('TRentryMCValue', xmit1)
    call WDGetEntryDouble('TRentryMCunc', xsdv)           ! contains already coverf

    call WDGetEntryDouble('TRentryMCValueRSD', rxmit1)
    call WDGetEntryDouble('TRentryMCuncRSD', rxsdv)

    IF(xmit1 > 0._rn .AND. xsdv > 0._rn) THEN
        IF(.not.use_BCI) THEN
            IF(langg == 'DE') cbci = ' '
            IF(langg == 'EN') cbci = ' '
            IF(langg == 'FR') cbci = ' '
        else
            IF(langg == 'DE') cbci = '(kürzester Bereich)'
            IF(langg == 'EN') cbci = '(shortest range)'
            IF(langg == 'FR') cbci = '(portée la plus courte)'
        end if

        WRITE(unit,'(1x)')
        IF(langg == 'DE') WRITE(unit,'(a)') 'Monte Carlo-Simulation:'
        IF(langg == 'EN') WRITE(unit,'(a)') 'Monte Carlo Simulation:'
        IF(langg == 'FR') WRITE(unit,'(a)') 'Simulation de Monte Carlo:'

        IF(langg == 'DE') WRITE(unit,'(a,i8,a,i2,a,a,a)') 'Anzahl Messungen pro Run: ', kcmx,';  ', &
            kcrun,' Runs;','  ',TRIM(cnegativ)
        IF(langg == 'EN') WRITE(unit,'(a,i8,a,i2,a,a,a)') 'Number measurem. per run: ', kcmx,';  ', &
            kcrun,' Runs;','  ',TRIM(cnegativ)
        IF(langg == 'FR') WRITE(unit,'(a,i8,a,i2,a,a,a)') 'nombre de mesures par course: ', kcmx,';  ', &
            kcrun,' Courses;','  ',TRIM(cnegativ)

        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'primärer Wert           : ', &
            xxmit1PE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxmit1PE
        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'primary value           : ', &
            xxmit1PE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxmit1PE
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'valeur primaire         : ', &
            xxmit1PE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxmit1PE

        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Erweiterte Unsicherheit : ', &
        !                                  xsdvPE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE
            xxsdvPE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE

        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Expanded uncertainty    : ', &
        !                                  xsdvPE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE
            xxsdvPE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Incertitude étendue     : ', &
        !                                  xsdvPE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE
            xxsdvPE,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdvPE

        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'bester Wert             : ', &
            xxmit1,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxmit1
        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'best value              : ', &
            xxmit1,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxmit1
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'meilleure valeur        : ', &
            xxmit1,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxmit1

        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Erweiterte Unsicherheit : ', &
            xxsdv,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdv
        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Expanded uncertainty    : ', &
            xxsdv,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdv
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Incertitude étendue     : ', &
            xxsdv,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxsdv

        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'Untere Bereichsgrenze   : ', &
            xLQ,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxLQ,TRIM(cbci), &
            '(Probabilistisch-symmetrisch)'
        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'Lower range limit       : ', &
            xLQ,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxLQ,TRIM(cbci), &
            '(Probabilistic symmetric)'
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'limite inf. de la plage : ', &
            xLQ,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxLQ,TRIM(cbci), &
            '(Probabilistic symmetric)'
        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'Obere Bereichsgrenze    : ', &
            xUQ,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxUQ,TRIM(cbci), &
            '(Länge=',xUQ - xLQ,')'

        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'Upper range limit       : ', &
            xUQ,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxUQ,TRIM(cbci), &
            '(Length=',xUQ - xLQ,')'
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'limite sup. de la plage : ', &
            xUQ,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxUQ,TRIM(cbci), &
            '(Longeur=',xUQ- xLQ,')'

        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'Untere Bereichsgrenze   : ', &
            est1LQ_BCI,TRIM(einheit(kEGr)%s),'rel.SD%: ',rx1LQbci,TRIM(cbci), &
            '(Kürzestes Überdeckungsintervall)'
        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'Lower range limit       : ', &
            est1LQ_BCI,TRIM(einheit(kEGr)%s),'rel.SD%: ',rx1LQbci,TRIM(cbci), &
            '(Shortest coverage interval)'
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,4x,a)') 'limite inf. de la plage : ', &
            est1LQ_BCI,TRIM(einheit(kEGr)%s),'rel.SD%: ',rx1LQbci,TRIM(cbci), &
            '(Intervalle de couverture le plus court)'

        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'Obere Bereichsgrenze    : ', &
            est1UQ_BCI,TRIM(einheit(kEGr)%s),'rel.SD%: ',rx1UQbci,TRIM(cbci), &
            '(Länge=',est1UQ_BCI-est1LQ_BCI,')'
        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'Upper range limit       : ', &
            est1UQ_BCI,TRIM(einheit(kEGr)%s),'rel.SD%: ',rx1UQbci,TRIM(cbci), &
            '(Length=',est1UQ_BCI-est1LQ_BCI,')'
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5,2x,a,8x,a,es16.9,a)') 'limite sup. de la plage : ', &
            est1UQ_BCI,TRIM(einheit(kEGr)%s),'rel.SD%: ',rx1UQbci,TRIM(cbci), &
            '(Longeur=',est1UQ_BCI-est1LQ_BCI,')'

        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Erkennungsgrenze (EKG)  : ', &
            xDT,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxDT
        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Decision threshold (DT) : ', &
            xDT,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxDT
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Seuil de décision (DT)  : ', &
            xDT,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxDT
        IF(langg == 'DE') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Nachweisgrenze (NWG)    : ', &
            xDL,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxDL
        IF(langg == 'EN') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Detection limit (DL)    : ', &
            xDL,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxDL
        IF(langg == 'FR') WRITE(unit,'(a,es16.9,1x,a,2x,a,f8.5)') 'Limite de détection (DL): ', &
            xDL,TRIM(einheit(kEGr)%s),'rel.SD%: ',rxDL

    END IF

    IF(FitDecay) THEN
        call WDGetEntryDouble('TRentryUfit', stabw_lsq)
        call WDGetEntryDouble('TRentryUprop', stabw_prop)
        call WDGetEntryDouble('TRentryChisqr', Chisqr)

        klinx = klinf
        IF(kfitp(1) > 0) klinx = kfitp(1)-1+kEGr
        WRITE(unit,'(1x)')
        IF(langg == 'DE') WRITE(unit,'(5a)') 'Standardabweichung der mit ',TRIM(fitmeth), &
            ' bestimmten Größe ',TRIM(Symbole(klinx)%s),' :'
        IF(langg == 'EN') WRITE(unit,'(5a)') 'Standard deviation of the quantity ',   &
            TRIM(Symbole(klinx)%s),' obtained by ',TRIM(fitmeth),' :'
        IF(langg == 'FR') WRITE(unit,'(5a)') 'Écart-type de la quantité ',   &
            TRIM(Symbole(klinx)%s),' obtenu par ',TRIM(fitmeth),' :'

        IF(langg == 'DE') WRITE(unit,'(4x,a,es11.4,1x,a)') 'aus der LSQ-Analyse              : ', &
            stabw_lsq, Einheit(klinx)%s
        IF(langg == 'EN') WRITE(unit,'(4x,a,es11.4,1x,a)') 'from LSQ analysis                : ', &
            stabw_lsq, Einheit(klinx)%s
        IF(langg == 'FR') WRITE(unit,'(4x,a,es11.4,1x,a)') 'de l''analyse LSQ                : ', &
            stabw_lsq, Einheit(klinx)%s
        IF(langg == 'DE') WRITE(unit,'(4x,a,es11.4,1x,a)') 'aus Unsicherheiten-Fortpflanzung : ', &
            stabw_prop, Einheit(klinx)%s
        IF(langg == 'EN') WRITE(unit,'(4x,a,es11.4,1x,a)') 'from uncertainty propagation     : ', &
            stabw_prop, Einheit(klinx)%s
        IF(langg == 'FR') WRITE(unit,'(4x,a,es11.4,1x,a)') 'de la propagation de l''incertitude : ', &
            stabw_prop, Einheit(klinx)%s
        IF(langg == 'DE') WRITE(unit,'(4x,a,f7.4)') 'reduziertes Chi-Quadrat          : ',Chisqr
        IF(langg == 'EN') WRITE(unit,'(4x,a,f7.4)') 'reduced Chi-squared              : ',Chisqr
        IF(langg == 'FR') WRITE(unit,'(4x,a,f7.4)') 'Chi-carré réduit                 : ',Chisqr
    end if

    WRITE(unit,'(104a1)') ('-',i=1,104)

    GOTO 270      ! For the next output quantity, begin at label 270

!-----------------------------------------------------------------------

9000 CONTINUE

    close (unit)

end subroutine PrepReport

!#######################################################################


subroutine WriteTiteltext(unit,izlen,izeil)

!     Copyright (C) 2014-2023  Günter Kanisch

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
            i3 = MIN(i1+izlen-1,LEN_TRIM(buffer))
            IF(LEN_TRIM(buffer) == 0) then
                write(unit,*)
                EXIT
            end if
            IF(i2 == 0) THEN
                IF(i3 == izlen) THEN
                    do i=i3,MAX(i1,i3-40),-1
                        IF(buffer(i:i) == ' ' .OR. buffer(i:i) == '-' ) THEN
                            WRITE(unit,'(a)') buffer(i1:i)
                            izeil = izeil + 1
                            buffer = TRIM(buffer(i+1:))
                            EXIT
                        END IF
                    end do
                else
                    WRITE(unit,'(a)') buffer(i1:i3)
                    izeil = izeil + 1
                    buffer = TRIM(buffer(i3+1:))
                END IF
            END IF
            IF(LEN_TRIM(buffer) == 0) then
                !write(unit,*)
                EXIT
            end if
        end do      ! endless loop
    end do
    if(allocated(buffer)) deallocate (buffer)

end subroutine WriteTiteltext
