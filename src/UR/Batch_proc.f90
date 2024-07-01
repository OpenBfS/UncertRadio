
subroutine Batch_proc()

!   Copyright (C) 2023  Günter Kanisch

use gtk,              only: GTK_BUTTONS_OK,GTK_MESSAGE_WARNING,GTK_MESSAGE_INFO
USE UR_Variables,     only: fname,project_loadw,sListSeparator, &
                            work_path,Gum_restricted,serial_csvinput, &
                            bat_serial,langg,bat_mc,bat_mcmc,base_project_SE, &
                            kcmxMC,kcrunMC,kfrom_SE,kto_SE,    &
                            batf_file,batf,batf_reports,kfi,linebat, dir_sep
USE UR_Gleich,        only: ifehl,SymboleG,ngrs,nab,knumEGr
USE UR_perror
use top,               only: WrStatusbar
use Rout,              only: WDPutEntryInt,pending_events,WTreeViewPutDoubleCell, &
                             WDNotebookSetCurrPage,WDGetEntryDouble,WDSetCheckButton, &
                             WDGetEntryInt
use URdate,            only: datim
use Rout,              only: MessageShow
use MCC,               only: Run_MCstart
use Usub3,             only: SaveResults
use UR_interfaces,     only: ProcessLoadPro_new
use UR_params,         only: rn,zero
use Top,               only: FindItemS
use RdSubs,            only: WandelDPkt
use UR_MCC,            only: cpu_time_MC,estUQ_BCI2,estLQ_BCI2
use UR_DLIM,           only: KBgrenzu,KBgrenzo,KBgrenzuSH,KBgrenzoSH
use CHF,               only: ucase,testSymbol
use UR_Linft,          only: FitDecay,ifit

implicit none

integer(4)              :: i,i1,i2,ios,neg,kk,resp,j,ii
integer(4)              :: k,nsy,kout,nr,nrpt,nfd,nrec
INTEGER(4)              :: zt1(9),ivals(13),kfimax,kkk

real(rn)                :: PE,uPE,PE1,uPE1,BE,BE1,BE2,uBE,uBE1,uBE2,LQ,LQ1,LQ2,UQ,UQ1,UQ2
real(rn)                :: sLQ,sLQ1,sLQ2,sUQ,sUQ1,sUQ2,DT,DT1,DT2,DL,DL1,DL2
real(rn)                :: urelBE2,ureluBE2,urelLQ2,urelUQ2,urelsLQ2,urelsUQ2,urelDT2,urelDL2
CHARACTER(LEN=256)      :: ffname,batvals,str1,file188,file189,cmdstring,file187, &
                           ReportName,prname
CHARACTER(LEN=20)       :: tdatum
CHARACTER(LEN=150)      :: thelp
character(len=400)      :: ftext
CHARACTER(LEN=1)        :: ctr
character(len=:),allocatable     :: text12,btext,ch1
character(len=30)       :: symb(60)
character(len=1)        :: valtype(60)
real(rn)                :: bvals(60)
logical                 :: old_out,retry

!---------------------------------------------------------------------------------------
ctr = sListSeparator         ! ';'
ifehl = 0

old_out = .false.
if(bat_serial) old_out = .true.
if(batf) old_out = .true.

allocate(character(len=900) :: text12,btext)
allocate(character(len=100) :: ch1)

if(.not. bat_serial .and. batf) then

11  open(186,file=batf_file,status='old',iostat=ios,iomsg=ftext)
  if(ios /= 0) then
    call ErrOpenFile(batf_file,ftext,retry)
    if(retry) goto 11
    ifehl = 1
    write(66,*) 'Error with opening batf_file File=',trim(batf_file)
    return
  endif
  close (185)
13  open(185,file='batch_out.csv',action='write',status='unknown',iostat=ios,iomsg=ftext)   ! 18.6.2024
    ! added action = 'write'              18.6.2024
  if(ios /= 0) then
    call ErrOpenFile('batch_out.csv',ftext,retry)
    if(retry) goto 13
    ifehl = 1
    write(66,*) 'Error with opening batch_out.csv'
    return
  endif

  if(.not.bat_mcmc) write(185,*) '  ',ctr,'T1',ctr,'T2(MC)',ctr
end if

call datim(zt1)
WRITE(tdatum,'(2(i2.2,''.''),i4.4,1X,2(i2.2,'':''),i2.2)') &
                             zt1(6),zt1(7),zt1(8),zt1(5),zt1(4),zt1(3)
if(bat_serial .or. batf) then
  if(bat_serial) then
    ffname = base_project_SE
    fname = trim(ffname)
    write(66,*) 'bat_serial:  fname=',trim(fname)
    project_loadw = .TRUE.
    call ProcessLoadPro_new(0,1)      ! call for the 1. output quantity

    batvals = trim(serial_csvinput)
    i1 = index(serial_csvinput,':' // dir_sep)
    if(i1 == 0) batvals = trim(work_path) // trim(serial_csvinput)
  elseif(batf) then
    ! ??
  end if

  if(bat_serial) then
    close(112)
15  open(112,file=batvals,status='old',iostat=ios,iomsg=ftext)
    if(ios /= 0) then
      call ErrOpenFile(batvals,ftext,retry)
      if(retry) goto 15
      ifehl = 1
      write(66,*) 'Error Open file batvals=',trim(batvals),' : iosstat=',int(ios,2)
      return
    endif

                     deallocate(text12); allocate(character(len=900) :: text12)
    read(112,'(a)') text12
       text12 = adjustL(ucase(text12))
    do i=1,len_trim(text12)
      if(text12(i:) == ',') text12(i:i) = '.'
    end do
    nsy = 0
    do
      if(len_trim(text12) == 0) exit
      i1 = index(text12,ctr)
      if(i1 > 0) then
        nsy = nsy + 1
        read(text12(1:i1-1),'(a)') symb(nsy)
        text12 = adjustl(text12(i1+1:))
      else
        nsy = nsy + 1
        read(text12,'(a)') symb(nsy)
        text12 = ''
      endif
      valtype(nsy) = 'v'        ! value
      ch1 = ucase(symb(nsy))
      if(index(ch1,'U(') > 0 .or. index(ch1,'U (') > 0) then
        valtype(nsy) = 'u'
        i1 = index(symb(nsy),'(')
        i2 = index(symb(nsy),')')
        symb(nsy) = symb(nsy)(i1+1:i2-1)
      endif
      if(index(ch1,'HW(') > 0 .or. index(ch1,'HW (') > 0) then
        valtype(nsy) = 'h'
        i1 = index(symb(nsy),'(')
        i2 = index(symb(nsy),')')
        symb(nsy) = symb(nsy)(i1+1:i2-1)
      endif
      nfd = 0
      ch1 = ucase(symb(nsy))

      do kk=nab+1,ngrs
        if(trim(ch1) == SymboleG(kk)%s) nfd = 1
      enddo
      IF(nfd == 0) then
        IF(langg == 'DE') WRITE(str1,'(a,a,a1,a,a1,a)') 'Das Symbol ',trim(symb(nsy)), &
                                 char(13),'ist nicht in der Symbolliste enthalten!', &
                                 char(13),'Bitte korrigieren!'
        IF(langg == 'EN') WRITE(str1,'(a,a,a1,a,a1,a)') 'The symbole',trim(symb(nsy)), &
                                 char(13),'is not part of the list of symbols!', &
                                 char(13),'Please, correct!'
        IF(langg == 'FR') WRITE(str1,'(a,a,a1,a,a1,a)') 'Le symbole ',trim(symb(nsy)), &
                                 char(13), 'ne doit pas contenir de signe moins!', &
                                 char(13), 'Corrigez s''il vous plaît!'
        call MessageShow(trim(str1), GTK_BUTTONS_OK, "Batch:", resp,mtype=GTK_MESSAGE_WARNING)
        ifehl = 1
        goto 9000
      END IF
    enddo
        write(thelp,*) (trim(symb(i)),ctr,' ',i=1,nsy)
        ! if(use_bipoi) write(184,*) trim(thelp),' p_binom',ctr,'nDT',ctr,'nDL',ctr,'DT_mcmc',ctr,'DL_mcmc',ctr,'BE_MCMC',ctr,'uBE_MCMC',ctr
  endif
endif

55    continue

close (187)
close (188)
close (189)

if(bat_serial) then
  i1 = 0
  do i=len_trim(batvals),1,-1
    if(batvals(i:i) == '.') then
      i1 = i-1
      exit
    end if
  end do
  if(old_out) then
    file187 = batvals(1:i1) // '_res.csv'
    file188 = batvals(1:i1) // '_mcmc.csv'
    file189 = batvals(1:i1) // '_mc.csv'
19    open(187,file=file187,status='unknown',action='write',iostat=ios,iomsg=ftext)            ! 11929-T1
    if(ios /= 0) then
      call ErrOpenFile(file187,ftext,retry)
      if(retry) goto 19
      ifehl = 1
      write(66,*) 'Batch_proc: Error Open file ',trim(file187),' : iosstat=',int(ios,2)
      return
    endif

20    open(188,file=file188,status='unknown',action='write',iostat=ios,iomsg=ftext)            ! mcmc-mh
    if(ios /= 0) then
      call ErrOpenFile(file188,ftext,retry)
      if(retry) goto 20
      ifehl = 1
      write(66,*) 'Batch_proc: Error Open file ',trim(file188),' : iosstat=',int(ios,2)
      return
    endif

21    open(189,file=file189,status='unknown',action='write',iostat=ios,iomsg=ftext)            ! 11929-T2
    if(ios /= 0) then
      call ErrOpenFile(file189,ftext,retry)
      if(retry) goto 21
      ifehl = 1
      write(66,*) 'Batch_proc: Error Open file ',trim(file189),' : iosstat=',int(ios,2)
      return
    endif


    write(btext,'(a)') 'File; #EG; lineBAT; PE; uPE; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL; cpu(s);'
    if(ctr == ',') then
      do i=1,len_trim(btext)
        if(btext(i:i) == ';') btext(i:i) = ctr
      end do
    end if
    write(187,'(a)') trim(btext)
    write(btext,'(a)') 'File; #EG; lineBAT; PE; uPE; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL; cpu(s);'
    if(ctr == ',') then
      do i=1,len_trim(btext)
        if(btext(i:i) == ';') btext(i:i) = ctr
      end do
    end if
    write(189,'(a)') trim(btext)
  endif
endif

if(batf .and. old_out) then
22  open(187,file='file_res.csv',status='unknown',iostat=ios,iomsg=ftext)            ! 11929-T1
    if(ios /= 0) then
      call ErrOpenFile('file_res.csv',ftext,retry)
      if(retry) goto 22
      ifehl = 1
      write(66,*) 'Batch_proc: Error Open file ','file_res.csv',' : iosstat=',int(ios,2)
      return
    endif

24  open(189,file='file_mc.csv',status='unknown',iostat=ios,iomsg=ftext)            ! 11929-T2
    if(ios /= 0) then
      call ErrOpenFile('file_mc.csv',ftext,retry)
      if(retry) goto 24
      ifehl = 1
      write(66,*) 'Batch_proc: Error Open file ','file_mc.csv',' : iosstat=',int(ios,2)
      return
    endif

  write(btext,'(a)') 'File; #EG; PE; uPE; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL; cpu(s);'
    if(ctr == ',') then
      do i=1,len_trim(btext)
        if(btext(i:i) == ';') btext(i:i) = ctr
      end do
    end if
    write(187,'(a)') trim(btext)

  write(btext,'(a)') 'File; #EG; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL; nDT; cpu(s); '   ! 'DT_BF; BF_DT; BF_DL; Pr_DL; BF=3; EKG(BF=3); counts(BF=3); p3(BF=3)'
    if(ctr == ',') then
      do i=1,len_trim(btext)
        if(btext(i:i) == ';') btext(i:i) = ctr
      end do
    end if
    write(188,'(a)') trim(btext)

  write(btext,'(a)') 'File; #EG; PE; uPE; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL; cpu(s);'
    if(ctr == ',') then
      do i=1,len_trim(btext)
        if(btext(i:i) == ';') btext(i:i) = ctr
      end do
    end if
    write(189,'(a)') trim(btext)
endif
 if(bat_mcmc) write(28,*) 'Start: ',tdatum

nrec = 1
if(bat_serial) nrec = kto_SE

if(bat_serial) kfimax = 1
if(batf) kfimax = kto_SE

do kkk=1,kfimax            ! UR-Projects
  kfi = kkk

  if(bat_serial) then
    if(kfi > 1) exit
    write(66,*) 'UR-Datei: ',trim(fname),'   kfrom_se=',int(kfrom_se,2),'  kto_se=',int(kto_se,2)
  end if

  if(batf) then

    read(186,'(a)',iostat=ios) ffname
    if(ios /= 0) exit
       if(ffname(1:1) == '#') cycle
       if(len_trim(ffname) == 0) cycle

    if(index(ffname,':') == 0 .and. index(ffname, dir_sep) == 0) then
      fname = trim(work_path) // trim(ffname)
    else
      fname = ffname
    endif
    if(kfi < kfrom_SE .or. kfi > kto_SE) cycle
      write(66,*) 'batf: fname=',trim(fname)
  endif
  !-------------------------------------------------
  do nr=1,nrec   ! record number for bat_serial:
           write(66,*) 'nr-loop: nr=',int(nr,2)
    if(bat_serial) then
      if(nr < kfrom_se) then
        read(112,*)
        cycle
      endif
      if(nr > kto_se) then
        close (112)
        goto 500
      end if
      if(nr == kfrom_se) then
        write(btext,'(a)') 'File; #EG; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL; NDT; cpu(s);'
        if(ctr == ',') then
          do i=1,len_trim(btext)
            if(btext(i:i) == ';') btext(i:i) = ctr
          end do
        end if
        write(188,'(a)') trim(btext)
      end if
    endif

    do neg=1,3
      ! neg: Number of the output quantity
      if(bat_mcmc .and. neg == 2) exit
      if(bat_serial .and. neg > 1 .and. neg > knumEGr) exit
      if(batf .and. neg > 1 .and. neg > knumEGr) exit
      if(neg > 1 .and. FitDecay .and. ifit(neg) == 2) cycle
      ifehl = 0
      if(.not.bat_serial .and. neg > 1) project_loadw = .TRUE.
      if(neg == 1) then
        call ProcessLoadPro_new(0,1)      ! call for the 1. output quantity
      endif
      if(neg == 2) then
        call ProcessLoadPro_new(2,2)      ! call for the 2. output quantity
      endif
      if(neg == 3) then
        call ProcessLoadPro_new(2,3)      ! call for the 3. output quantity
      endif
          IF(ifehl == 1) GOTO 9000

      if(neg == 1 .and. bat_serial) then
        write(str1,'(a,i0)') 'Eval line ',nr
        call WrStatusbar(3,trim(str1))
        lineBat = nr
                 deallocate(text12); allocate(character(len=900) :: text12)
        read(112,'(a)',iostat=ios) text12

        if(ios /= 0) then
          close (112)
             write(66,*) 'ios-Fehler: goto 500'
          goto 500
        end if

        do i=1,len_trim(text12)
          if(langg == 'DE' .or. langg == 'FR') then
            if(text12(i:i) == ',') text12(i:i) = '.'
            if(text12(i:i) == ctr) text12(i:i) = ' '
          elseif(langg == 'EN') then
            if(text12(i:i) == ctr) text12(i:i) = ' '
          endif
        enddo
        read(text12,*,iostat=ios) (bvals(i),i=1,nsy)
        if(ios /= 0) then
          ifehl = 1
          write(66,*) 'Batch_proc: Read error in the record : ',trim(text12)
          close(188)
          close(189)
          goto 9000
        end if
        call WDNotebookSetCurrPage('notebook1', 3)
        do i=nab,ngrs
          do k=1,nsy
            if(SymboleG(i)%s == trim(symb(k))) then
              if(valtype(k) == 'v') call WTreeViewPutDoubleCell('treeview2',5,i, bvals(k))
              if(valtype(k) == 'u') call WTreeViewPutDoubleCell('treeview2',8,i, bvals(k))
              if(valtype(k) == 'h') call WTreeViewPutDoubleCell('treeview2',9,i, bvals(k))
              exit
            end if
          enddo
        end do
          ! write(66,*) 'vor CC: bvals=',sngl(bvals(1:nsy))
        call ProcessLoadPro_new(2,1)
        call WDNotebookSetCurrPage('notebook1', 5)
           !  write(66,*) 'CC: ifehl=',int(ifehl,2)

        IF(ifehl == 1 .OR. ifehlp == 1) goto 150
      end if

      if(bat_mc) then
        ! MC-Simulation:
        call WDPutEntryInt('TRentryMCanzM',kcmxMC)    !  ,'(I7)')
        call WDPutEntryInt('TRentryMCanzR',kcrunMC)
        if(bat_serial) write(66,*) 'DC: nrpt=',nrpt,'  kcmxMC=',kcmxMC,'  kcrunMC=',kcrunMC
        call Run_MCstart(ifehl)
        if(ifehl == 1) goto 9000
          call pending_events()
          call pending_events()

        call WDGetEntryDouble('TRentryMCvalPE', PE1)
        call WDGetEntryDouble('TRentryMCvalUPE', uPe1)

        call WDGetEntryDouble('TRentryMCValue', BE1)
        call WDGetEntryDouble('TRentryMCunc', uBe1)
        call WDGetEntryDouble('TRentryMClq', LQ1)
        call WDGetEntryDouble('TRentryMCuq', UQ1)
        sLQ1 = estLQ_BCI2
        sUQ1 = estUQ_BCI2
        if(.not.gum_restricted) then
          call WDGetEntryDouble('TRentryMCdt', DT1)
          call WDGetEntryDouble('TRentryMCdl', DL1)
        endif

        if(bat_serial) then
          write(btext,*) trim(fname),ctr,int(neg,2),ctr,lineBat,ctr,sngl(PE1),ctr,sngl(uPE1),ctr, &
                         sngl(BE1),ctr,sngl(uBE1),ctr,sngl(LQ1),ctr,sngl(UQ1),ctr, &
                         sngl(sLQ1),ctr,sngl(sUQ1),ctr,  &
                         sngl(DT1),ctr,sngl(DL1),ctr,sngl(cpu_time_MC),ctr
        elseif(batf) then
          write(btext,*) trim(fname),ctr,int(neg,2),ctr,sngl(PE1),ctr,sngl(uPE1),ctr,sngl(BE1),ctr,sngl(uBE1),ctr, &
                         sngl(LQ1),ctr,sngl(UQ1),ctr,sngl(sLQ1),ctr,sngl(sUQ1),ctr,  &
                         sngl(DT1),ctr,sngl(DL1),ctr,sngl(cpu_time_MC),ctr
        endif
        call WandelDPkt(btext,2)
        if(old_out) write(189,'(a)') trim(btext)
      endif

      PE=zero; uPE=zero; BE=zero; uBE=zero; LQ=zero; UQ=zero; sLQ=zero; sUQ=zero; DT=zero; DL=zero

      call WDGetEntryDouble('TRentryValue', PE)
      call WDGetEntryDouble('TRentryUnc', uPe)
      call WDGetEntryDouble('TRentryValueBy', BE)
      call WDGetEntryDouble('TRentryUncBy', uBe)

      call WDGetEntryDouble('TRentryLQBy', LQ)
      call WDGetEntryDouble('TRentryUQBy', UQ)
      sLQ = KBgrenzuSH
      sUQ = KBgrenzoSH
      if(.not.gum_restricted) then
        call WDGetEntryDouble('TRentryDT', DT)
        call WDGetEntryDouble('TRentryDL', DL)
      endif
           write(66,*) 'nr-loop: vor kout=187','  batf=',batf,' old_out=',old_out
      if(bat_serial) then
        write(btext,*) trim(fname),ctr,int(neg,2),ctr,lineBAT,ctr,sngl(PE),ctr,sngl(uPE),ctr, &
                       sngl(BE),ctr,sngl(uBE),ctr,sngl(LQ),ctr,sngl(UQ),ctr,  &
                       sngl(sLQ),ctr,sngl(sUQ),ctr,sngl(DT),ctr,sngl(DL),ctr,sngl(cpu_time_MC)
      elseif(batf) then
        write(btext,*) trim(fname),ctr,int(neg,2),ctr,sngl(PE),ctr,sngl(uPE),ctr, &
                       sngl(BE),ctr,sngl(uBE),ctr,sngl(LQ),ctr,sngl(UQ),ctr,  &
                       sngl(sLQ),ctr,sngl(sUQ),ctr, sngl(DT),ctr,sngl(DL),ctr,sngl(cpu_time_MC)
      endif
        kout = 187
      call WandelDPkt(btext,2)
      if(old_out) write(kout,'(a)') trim(btext)
          if(.not.bat_mcmc) goto 120
      if(neg < knumEGr .and. .not.bat_mcmc) goto 120
      if(neg == knumEGr .and. .not.bat_mcmc) goto 120

120    continue

        if(batf .and. (bat_mc .or. bat_mcmc)) then
             PE=zero; uPE=zero; BE=zero; uBE=zero; LQ=zero; UQ=zero;
             sLQ=zero; sUQ=zero; DT=zero; DL=zero

          call WDGetEntryDouble('TRentryValue', PE)
          call WDGetEntryDouble('TRentryUnc', uPe)
          call WDGetEntryDouble('TRentryValueBy', BE)
          call WDGetEntryDouble('TRentryUncBy', uBe)

          call WDGetEntryDouble('TRentryLQBy', LQ)
          call WDGetEntryDouble('TRentryUQBy', UQ)
          if(.not.gum_restricted) then
            call WDGetEntryDouble('TRentryDT', DT)
            call WDGetEntryDouble('TRentryDL', DL)
          endif
          LQ = KBgrenzu
          UQ = KBgrenzo
          sLQ = KBgrenzuSH
          sUQ = KBgrenzoSH

          sLQ1 = estLQ_BCI2
          sUQ1 = estUQ_BCI2
          !sLQ2 = estLQ_BCImcmc
          !sUQ2 = estUQ_BCImcmc
            if(.not.bat_mc) then
              PE1=zero; uPE1=zero; BE1=zero; uBE1=zero; LQ1=zero; UQ1=zero;
              ! sLQ1=zero; sUQ1=zero;
              DT1=zero; DL1=zero
            end if
            if(.not.bat_mcmc) then
              BE2=zero; uBE2=zero; LQ2=zero; UQ2=zero;
              sLQ2=zero; sUQ2=zero; DT2=zero; DL2=zero
            end if

          kout = 185
          write(kout,'(a,a)') trim(fname),ctr
          write(kout,'(a,a,i1,a)') '#EG:',ctr,neg,ctr
          write(btext,'(a,a,2(es12.5,a))') 'PE',ctr,PE,ctr,PE1,ctr
           call WandelDPkt(btext,2)
           write(kout,'(a)') trim(btext)
          write(btext,'(a,a,2(es12.5,a))') 'uPE',ctr,uPE,ctr,uPE1,ctr
            call WandelDPkt(btext,2)
            write(kout,'(a)') trim(btext)

          write(btext,'(a,a,4(es12.5,a))') 'BE',ctr,BE,ctr,BE1,ctr,BE2,ctr,urelBE2,ctr
          call writeBtext(kout,btext)
          write(btext,'(a,a,4(es12.5,a))') 'uBE',ctr,uBE,ctr,uBE1,ctr,uBE2,ctr,ureluBE2,ctr
          call writeBtext(kout,btext)
          write(btext,'(a,a,4(es12.5,a))') 'LQ',ctr,LQ,ctr,LQ1,ctr,LQ2,ctr,urelLQ2,ctr
          call writeBtext(kout,btext)
          write(btext,'(a,a,4(es12.5,a))') 'UQ',ctr,UQ,ctr,UQ1,ctr,UQ2,ctr,urelUQ2,ctr
          call writeBtext(kout,btext)
          write(btext,'(a,a,4(es12.5,a))') 'sLQ',ctr,sLQ,ctr,sLQ1,ctr,sLQ2,ctr,urelsLQ2,ctr
          call writeBtext(kout,btext)
          write(btext,'(a,a,4(es12.5,a))') 'sUQ',ctr,sUQ,ctr,sUQ1,ctr,sUQ2,ctr,urelsUQ2,ctr
          call writeBtext(kout,btext)
          write(btext,'(a,a,4(es12.5,a))') 'DT',ctr,DT,ctr,DT1,ctr,DT2,ctr,urelDT2,ctr
          call writeBtext(kout,btext)
          write(btext,'(a,a,4(es12.5,a))') 'DL',ctr,DL,ctr,DL1,ctr,DL2,ctr,urelDL2,ctr
          call writeBtext(kout,btext)
          write(kout,*)
        end if

    enddo   ! do neg=
    150    continue
    write(28,'(3(a,i0))') 'bat_mcmc:   neg=',neg,'  knumEGr=',knumEGr,' nrpt=',nrpt
  end do     ! do nr=

  if(batf_reports) then
    if(ngrs > 0 .and. knumEGr > 0) then
      call PrepReport
      prname = fname
      do ii=len_trim(prname),1,-1
        if(prname(ii:ii) == '.') then
          prname = prname(1:ii-1)
          exit
        endif
      end do
      do ii=len_trim(prname),1,-1
        if(prname(ii:ii) == dir_sep) then
          prname = prname(ii+1:)
          exit
        endif
      end do
         write(66,*) 'prname=',trim(prname)
      ReportName = 'Report_' // trim(prname) // '.txt'
         write(66,*) 'ReportName=',trim(ReportName)
      close(15)
      open(15,file='Report.txt',status='old',iostat=ios,iomsg=ftext)
      close (14)
      open(14,file=Reportname,status='unknown',iostat=ios,iomsg=ftext )
      do i=1,10000
        read(15,'(a)',iostat=ios) thelp
        if(ios /= 0) exit
        write(14,'(a)',iostat=ios) trim(thelp)
      end do
      close (15)
      close (14)
    endif
  end if

end do     ! Files

500   continue
if((bat_serial .or. batf) .and. ifehl == 0) then
  IF(langg == 'DE') WRITE(str1,'(a,a1,a)') 'Die serielle Auswertung war erfolgreich!', &
              char(13),'Das Programm wird nun beendet.'
  IF(langg == 'EN') WRITE(str1,'(a,a1,a)') 'The serial evaluation was successful!', &
              char(13),'The program will be terminated now.'
  IF(langg == 'FR') WRITE(str1,'(a,a1,a)') 'L''évaluation en série a été réussie!', &
              char(13),'Le programme sera terminé maintenant.'
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "Batch:", resp,mtype=GTK_MESSAGE_INFO)
end if

call datim(zt1)
WRITE(tdatum,'(2(i2.2,''.''),i4.4,1X,2(i2.2,'':''),i2.2)')  &
                          zt1(6),zt1(7),zt1(8),zt1(5),zt1(4),zt1(3)
if(bat_mcmc) write(28,*) 'Beendet: ',tdatum

!---------------------------------------------------------------------------
9000  CONTINUE

close(185)
close (188)
call stat(file188,ivals,ios)
if(ios == 0 .and. (ivals(8) == 0 .or. ivals(8) == 46)) then
  str1 = ''
  cmdstring = 'del ' // trim(file188)
  CALL EXECUTE_COMMAND_LINE(cmdstring, wait=.false., EXITSTAT=j, CMDSTAT=k,CMDMSG=str1)
   if(k /= 0 .and. len_trim(str1) > 0) write(66,*) '       Message=',trim(str1)
end if

close (189)
call stat(file189,ivals,ios)
if(ios == 0 .and. (ivals(8) == 0 .or. ivals(8) == 46)) then
  str1 = ''
  cmdstring = 'del ' // trim(file189)
  CALL EXECUTE_COMMAND_LINE(cmdstring, wait=.false., EXITSTAT=j, CMDSTAT=k,CMDMSG=str1)
   if(k /= 0 .and. len_trim(str1) > 0) write(66,*) '       Message=',trim(str1)
end if

deallocate(text12,btext,ch1)

bat_mc = .FALSE.
bat_mcmc = .FALSE.
bat_serial = .false.
batf = .false.

end subroutine Batch_proc

!#######################################################################

subroutine ErrOpenFile(bfile,ftext,retry)

use gtk,              only: GTK_BUTTONS_OK,GTK_MESSAGE_ERROR,GTK_MESSAGE_WARNING    !,GTK_MESSAGE_INFO,
use Rout,             only: MessageShow
use UR_VARIABLES,     only: langg
use CHF,              only: ucase

implicit none

character(len=*),intent(in)  :: bfile, ftext
logical,intent(out)          :: retry

character(len=:),allocatable    :: str1
integer(4)            :: resp

allocate(character(len=500) :: str1)
retry = .false.
if(index(ucase(bfile),'.CSV') > 0 .and. index(ftext,'Permission denied') > 0) then
  IF(langg == 'DE') WRITE(str1,*) 'Fehler beim Öffnen der Datei ',trim(bfile),': ', char(13),char(13), &
                                  'Vor dem Schliessen dieser Message: bitte die genannnte Datei schliessen!'
  IF(langg == 'EN') WRITE(str1,*) 'Error on opening the file ',trim(bfile),': ',trim(ftext), char(13),char(13), &
                                   'Before closing this message: please close the named file! '
  IF(langg == 'FR') WRITE(str1,*) 'Erreur lors de l''ouverture du fichier ',trim(bfile),': ', char(13),char(13), &
                                  'Avant de fermer ce message : veuillez fermer le fichier nommé!'
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "BATF:", resp, mtype=GTK_MESSAGE_WARNING)
  retry = .true.
else
  IF(langg == 'DE') WRITE(str1,*) 'Fehler beim Öffnen der Datei ',trim(bfile),': Abbruch!',' ',trim(ftext)
  IF(langg == 'EN') WRITE(str1,*) 'Error on opening the file ',trim(bfile),': Abortion!',' ',trim(ftext)
  IF(langg == 'FR') WRITE(str1,*) 'Erreur lors de l''ouverture du fichier ',trim(bfile),': Avortement!',' ',trim(ftext)
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "BATF:", resp, mtype=GTK_MESSAGE_ERROR)
end if


deallocate(str1)

end subroutine ErrOpenFile

!#######################################################################

subroutine writeBtext(unit,btext)

use UR_VARIABLES,      only: bat_mcmc
use RdSubs,            only: WandelDPkt

implicit none

integer(4),intent(in)          :: unit
character(len=*),intent(inout) :: btext

integer(4)      :: kmax

call WandelDPkt(btext,2)
kmax = len_trim(btext); if(.not.bat_mcmc) kmax=kmax-13
write(unit,'(a)') trim(btext(1:kmax))

end subroutine writeBtext

!#######################################################################
