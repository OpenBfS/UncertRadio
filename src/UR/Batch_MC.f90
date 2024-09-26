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
subroutine Batch_MC()

    USE UR_Variables,     only: fname, fnameMCB, mcbatch, &
                                project_loadw, sListSeparator,&
                                UR_AUTO_output_path, &
                                work_path
    USE UR_Gleich,        only: ifehl
    USE UR_Linft,         only: fitmeth,kfitmeth,klincall,kPearson,kPMLE,use_TLSQ
    USE UR_perror

    use Rout,              only: WDPutEntryInt,pending_events

    use URdate,            only: datim
    use Rout,              only: gchar

    use Usub3,             only: SaveResults
    use UR_interfaces,     only: ProcessLoadPro_new

    implicit none

    integer                 :: i,i1,i2,i3,ios,kcmx(10),neg
    integer                 :: kfi,kmeth,fmeth(10),km,kTLSQ
    integer                 :: zt1(9)

    character(len=512)      :: text26,wpath
    character(len=20)       :: tdatum
    character(len=1)        :: ctr

!---------------------------------------------------------------------------------------
    ctr = sListSeparator         ! ';'

    call datim(zt1)
    write(tdatum,113) zt1(6),zt1(7),zt1(8),zt1(5),zt1(4),zt1(3)
113 format(i2.2,'.',i2.2,'.',i4.4,1X,i2.2,':',i2.2,':',i2.2)

    wpath = work_path
    if(len_trim(wpath) == 0) then
        wpath = trim(UR_AUTO_output_path)
    endif

    open(26,file=fnameMCB,status='old',iostat=ios)
    if(ios /= 0) then
        ifehl = 1
        return
    endif
    write(66,*) 'fnameMCB-Datei gefunden:',trim(fnameMCB)

    do kfi=1,100     ! UR-Projekte
        ! if(kfi > 2) exit
        kmeth = 0
        fmeth = 0
        read(26,'(a)',iostat=ios) text26
        if(ios /=0) exit
        call gchar(text26)
        i1 = index(text26,'FILE=')
        if(i1 > 0) then
            read(text26(i1+5:),'(a)') fname
            write(66,*) 'UR-Datei: ',trim(fname)
            do
                read(26,'(a)',iostat=ios) text26
                if(ios /=0) exit
                call gchar(text26)
                i1 = index(text26,'FITMETH=')
                if(i1 > 0) then
                    kmeth = kmeth + 1
                    read(text26(i1+8:i1+8),*) fmeth(kmeth)
                else
                    if(index(text26,'FILE=') > 0) then
                        backspace 26
                        exit
                    endif
                endif
                i1 = index(text26,'IMC=')
                if(i1 > 0) then
                    read(text26(i1+4:),*) kcmx(kmeth)
                    ! kcmx(kmeth) = kcmx(kmeth)/10
                endif
            enddo
        endif
        ! if(kfi < 4) cycle
        WRITE(66,*) 'Datei=',TRIM(fname)

        do neg=1,2
            ! neg: Nummer der Ergebnisgröße

            write(66,*) 'neg=',neg

            if(neg > 1) project_loadw = .TRUE.

            if(neg == 1) call ProcessLoadPro_new(0,1)      ! Aufruf für die 1. Ergebnisgröße
            if(neg == 2) call ProcessLoadPro_new(1,2)      ! Aufruf für die 2. Ergebnisgröße
            IF(ifehl == 1) GOTO 9000

            do km=1,kmeth

                if(kfitmeth /= fmeth(km)) then
                    ! Fitmethode einstellen:
                    kfitmeth = fmeth(km)
                    IF(kfitmeth == 0) kpearson = 0                    !
                    IF(kfitmeth == 1) kpearson = 1                    !
                    kPMLE = 0                                         !
                    kTLSQ = 0                                         !
                    IF(kfitmeth == 2) kPMLE = 1                       !
                    IF(kfitmeth == 3) kTLSQ = 1                       !
                    ! IF(kPMLE == 1 .AND. ifit(2) == 0) ifit(2) = 1     !   das muss lokal bei PMLE erfolgen
                    use_TLSQ = .FALSE.
                    ! IF(kfitmeth == 3) kTLSQ = 1         ! 10.9.2013
                    IF(kTLSQ == 1) use_TLSQ = .TRUE.

                    fitmeth = 'NLSQ'
                    IF(kpearson == 1) fitmeth = 'PLSQ'
                    IF(kPMLE == 1) fitmeth = 'PMLE'
                    IF(kTLSQ == 1) fitmeth = 'TLSQ'
                    klincall = 0              ! am 29.6.2014 ergänzt

                    if(neg == 1) call ProcessLoadPro_new(2,1)      ! Aufruf für die 1. Ergebnisgröße (Nachfahren nach Linmod)
                    if(neg == 2) call ProcessLoadPro_new(2,2)      ! Aufruf für die 2. Ergebnisgröße (Nachfahren nach Linmod)
                    if(ifehl == 1) return       ! 26.4.2017
                endif

                !//     if(neg == 1) call FindItemS('QuantityFirst', ncitem)
                !//     if(neg == 2) call FindItemS('QuantitySecond', ncitem)
                !//     call ProcMenu(ncitem)

                ! IF(.not.FitDecay .AND. .not.Gamspk1_Fit) Chisqr = -1._rn

                call WDPutEntryInt('TRentryMCanzM',kcmx(km),'(I6)')
                call WDPutEntryInt('TRentryMCanzR',1,'(i2)')

                !//    call Run_MCStart(ifehl)

                call SaveResults()
            enddo          ! km
        enddo         !  neg

    end do     ! Files

    !!!      IF(.not.loadingpro) call IosWait(100)

! WRITE(20,*) 'Ende des Tests!'

!---------------------------------------------------------------------------
9000 CONTINUE

! CALL FLUSH(18)

    close (26)
    MCbatch = .FALSE.

end subroutine Batch_MC
