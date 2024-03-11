!#######################################################################

submodule (Rw1) RW1A

          !     contains:
          ! Rechw1
          ! covppcalc
          ! LinCalib
          ! uval
          ! FindWparsR


  contains


module Subroutine Rechw1()

   ! This routine executes the following set of calculations:
   !
   ! runs the routines for determining the topolgy of the equations; this helps
   ! to find the variables/quantities which constitute the expression for the net
   ! count rate;
   ! it runs the routine PointNach for adjusting the arrays after a modification
   ! of the symbol list by adding covariance symbols (behind ngrs) in it;
   ! it applies the function parser to the right-had-sides of the equations
   ! (symbole index 1..nab)
   ! behind label 126 it also adjusts the Binpoi-related parameters after the
   ! modification of the symbol list;
   !
   ! execute the calculations of reducing user-supplied vectors of single values
   ! to a mean value and the associated uncertainty:
   !
   ! FitDecay: prepares a list of symbols required for calculating the output
   ! quantity with index kEGr, excluding those symbols contained in the argument
   ! of the Linfit call;
   !
   ! checks, for existing symbols, for missing data in the arrays corresponding to
   ! the table columns;
   !
   ! it reads in data from the corresponding dialogs being invoked for FitDecay,
   ! Gamspk1, FitCalCurve or SumEval;
   !
   ! it works on SDFormel, i.e. on SD formulae defined in the Value/uncertainty
   ! table in UncertRadio, to calculate their values; in that context,
   ! also index-variables are set for special variables/symbols having a SDformel
   ! defined, i.e., for kbrutto_gl (gross count rate), knullef (blank value),
   ! bipoi_gl (gross counts symbol); at the end of that part, those already
   ! existing SDWert values are copied to the StdUnc values;
   !
   !   var_brutto_auto
   ! and two tests to check correct definitions of the gross count rate
   !
   !  a double loop follows over an inner loop over the independent variables, in which
   !   (iwh=1) the StdUnc values are determined from the information in the columns for
   ! SDformel, SDWert, HBreite and IAR (abs/rel)), if available; the second outer loop run
   ! (iwh=2) evaluates the formulae in SDformel and copies the obtained standard uncertainty to
   ! StdUnc;
   !
   ! The next larger part refers to the evaluation of values for the covariances defined
   ! in the covariance table;
   !
   ! the next bigger part refers to calculating the standard uncertainties of dependent
   ! quantities (1..nab) using uncertainty propagation;
   ! FitDecay:
   !   the arrays are prepared or prolonged, initiated with values;
   !   for fixed fit parameter(parfixed=T): calculate the matrix cov_fixed which is
   !   later added, at the begin of Lincov2, to the matrix covyLF (this requires that the
   !   diagonal values of cov_fixed must be zero); close to the end of the FitDecay part
   !   the routine Linfausf is called;
   ! Gamspk1:
   !   reads in the data by calling GamPeakVals, determines the associated mpfx-parameters
   !   and calls then Linfg1Ausf for evaluation, which also takes the covariances induced
   !   by the mpfx-parameters into account;
   ! Similarly, another, much shorter block is processed for FitCalCurve;
   !
   ! What thereafter follows, ist the uncertainty propagation for dependent variables,
   ! processed within a nn4 backward loop starting from nab down to 1.
   !   for equation kbrutto: calculate StdUnc by formula or by another formula in the case
   !                         var_brutto_auto=T; for nn4=k_autoform a longer formula is
   !                         applied;
   !                         the formulae may also depend on the distribution type (exmaple:
   !                         IVTL=7 BinPoi)
   ! The following special nn4-values (indices) indicate calls to major routines , like:
   !    index klinf (FitDecay) : calls covppcalc and then calls LinfAusf,
   !    index kgspk1 (Gamspk1) : calls Linfg1Ausf,
   !    index kusEval (SumEval): calls SumEvalCalc,
   ! which is followed by transferring the obtained result values to the corresponding
   ! Messwert() and StdUnc() fields, also to the GUI fields. This terminates the nn4 loop.
   ! Two short corrections subsequent to the nn4 loop are then calculated.
   !
   ! At the end, a call FindWParsR(kEGr,klu) is executed which forms a set of variables
   ! which form the calibration factor.
   !
   !     Copyright (C) 2014-2023  Günter Kanisch

USE UR_Gleich,        only: Symbole,Formelt,Rssy,RSeite,SDFormel,symb_kEGr,nRnetp,symtyp, &
                            SymboleG,DistPars,StdUnc,RnetParsInd,CVFormel,SymboleA,SymboleB, &
                            StdUncSV,Messwert,akenn,bipoi_gl,dep_unc_done,iptr_time, &
                            ifehl,ilam_binom,ip_binom,itm_binom,k_datvar,kbgv_binom, &
                            kbrutto_double,kbrutto_gl,kfitcal,kgspk1,klinf,knullef, &
                            knumEGr,ksumeval,loadingpro,Messwert_CP,MesswertSV,nab,  &
                            missingval,nabf,nchmax,ncov,ncovf,nfkf,ngrs,ngrsP,nmodf,nvar,  &
                            nparts,nsymb_kegr,nwpars,kEGr,ncovmx,ngrs_cp,refdatamd,        &
                            rinflu_known,seqch,Symbole_CP,theta_ref,Ucomb,ukenn,upropa_on, &
                            use_bipoi,uval_used,knetto,avar,RnetParscrate,iptr_cnt,iptr_rate, &
                            iptr_cnt,covarval,nrssy,kpoint,IVTL,MDpointrev,SDwert,kbrutto,  &
                            HBreite,nrssyanf,RS_symbolNR,use_sdf_brutto,meanMD,umeanMD,IAR, &
                            kpointKB,nvalsMD,IsymbA,IsymbB,corrval,icovtyp,StdUnc_CP,fBaymd, &
                            wparsind,apply_units_dir,unit_conv_fact,grossfail
USE UR_Linft,         only: CStartzeit,dtdiff,kfitp,ma,CFaelldatum,cov_fixed,defineallxt,  &
                            export_R,FitCalCurve,FitDecay,k_rbl,k_tmess,kal_Polgrad,   &
                            kalfit_arg_expr,kfmode,klincall,konstant_R0,kpoint_kalarg,   &
                            kuse_fixed,maKB,mfrbg,mpfx,ifit,export_case,k_tstart, mpfx_ind, &
                            kpmle,mpfxfixed,nchannels,netto_involved_Fitcal,nkpmax, &
                            nhp,nkalpts,nkovzr,numd,R0k,singlenuk,UcombLinf,wp,dmesszeit, &
                            fpa,sfpa,fpaSV,mpfx_extern,ifitSV,ndatmax,SumEval_fit,dbzrate, &
                            sfpaSV,covpp,d0zrate,sd0zrate,sdr0k,d0zrateSV,parfixed,afuncSV, &
                            k_tmess,k_tstart,nhp_defined,use_WTLS,UcombLinf_kqt1,kEQnums

USE UR_DLIM,          ONLY: iteration_on,GamDist_ZR,GamDistAdd,iterat_passed,  &
                            var_brutto_auto,k_autoform
USE UR_Variables,     ONLY: langg,gum_restricted,batest_on,automode,simul_ProSetup

USE fparser,          ONLY: initf, parsef, evalf, EvalErrMsg
USE UR_Perror
! USE UR_Loadsel
USE UR_Gspk1Fit,      only: Gamspk1_Fit,sdeffi
use UR_MCC,           only: kqtypx
use, intrinsic :: iso_c_binding,    only: c_int,c_null_char,c_ptr
use gtk,              only: gtk_buttons_OK, gtk_widget_set_sensitive,GTK_MESSAGE_WARNING

use UR_gtk_variables, only: dialogstr,ioption,consoleout_gtk,green_bg
use top,              only: FinditemS,idpt,wrstatusbar,dpafact,MDcalc,chupper_eq,CharModA1, &
                            IntModA1,RealModA1,LogModA1,ModVarsTV2,CharModStr

use CHF,              only: FindLocT,ucase
use Sym1,             only: pointnach,RS_numbers
use Rout,             only: WDListstoreClearCell,WTreeViewPutDoubleCell,WTreeViewGetDoubleCell,    &
                            WTreeViewPutComboCell,WTreeViewPutComboArray,WDListstoreClearCell, &
                            WDListstoreFill_1,MessageShow,WTreeViewPutStrCell, WTreeViewPutDoubleArray, &
                            WTreeViewSetColorCell
use URdate,           only: datdif6
use UWB,              only: gevalf,resulta,upropa
use Num1,             only: funcs,dpi_funcs,matwrite
use KLF,              only: CalibInter
use LF1,              only: LinfAusf,StoreLinfParms
use LF1G,             only: Linfg1Ausf
use LDN,              only: Loadsel_diag_new
use UR_params,        only: rn,eps1min,zero,one,two,three
use PMD,              only: GamPeakVals

implicit none

integer(4)            :: i,i1,nxx,nn4,iwh,k,nundf,j,ix,ii,i2,i3,j0,j2,i0,igl,m,nn
integer(4)            :: i11,ios,nhh,ifehlps,ios2,kngross,istep,ix1,nng,klu,knetx
integer(4)            :: k1,nhg,nst,k2,jj,nwh,mfit2,knt,nvh,mpi,m1,ncitem2,nn2,ii2
integer(4)            :: ksq1,ksq2,ksqlast,imax
real(rn)              :: res,xnn,xnueg,xg0,varg0,fBay_g
CHARACTER(LEN=50)     :: kusetext
integer(4)            :: idat1(6),idat2(6),ifehlx,jp,nfd
integer(4)            :: ksq
real(rn)              :: rn0,SDrn0,akt,SDakt,xn0,xp
real(rn)              :: xx, yval,uyval,CCV
CHARACTER(LEN=20)     :: ccdatum
LOGICAL               :: istdatum, Rw1pro
real(rn)              :: dpi1,dpi2,d0zsum,d0zsumq,dummy,af1,af2,afunc(ma)
character(len=4)      :: ch1
integer(4)            :: resp,ncitem,ifb,IVTL7
type(c_ptr)           :: tree
character(:),allocatable :: str1,RseiteG,cxkb,sdfSave,sdfG,crsG,rst
character(:),allocatable :: string
!------------------------------------------------------------------

! Jan. 2020:       Notes about how to apply the t-distribution:
! a) analytically and by MC: The values in StdUnc() always must include
!    the factor  (n-1)/(n-3)=ndf/(ndf-2). Modvar also calculates its
!    StdUnc values in this way.
! b) Only when generating random values in MC, this factor must be
!    removed in advance, because it is the associated random generator itself,
!    which introduces this factor then.
! c) The parameter sigma in DistPars (DistPars%pval(nn,2)) must not include
!    this factor.
!----------------------------------------------------------------------

kbrutto_double = 0
rw1pro = .false.
  ! rw1pro = .true.      ! here one may choose true for more output to unit 66.

!!! RW1_on = .true.

allocate(character(len=800) :: str1,RseiteG,cxkb,sdfSave,sdfG,crsG,rst )

WRITE(66,*) '##################### Rechw1: ',Symbole(kEGr)%s,'  ##################'
if(consoleout_gtk) WRITE(0,*) '##### Rechw1: ',Symbole(kEGr)%s,'  ##################'

WRITE(66,'(a,i3,i3,a,L1,a,i2)') ' knumEGr , kEGr=',knumEGr , kEGr,'  FitDecay=',FitDecay,'  ncov=',ncov

       if(.false.) then
         do i=1,ubound(Formelt,dim=1)
           write(66,'(a,i0,a,a)') 'i=',i,' Formelt(i)=',Formelt(i)%s
         end do
       end if

upropa_on = .FALSE.
UcombLinf = zero
UcombLinf_kqt1 = zero
kqtypx = 0

grossfail = .false.
if(.true. .and. .not.Gum_restricted) then
  if(.not.FitDecay .and. .not.Gamspk1_Fit .and. kEGr > 0) then     ! .and. .not.SumEval_fit
    ! call TopoSort:
    i = knetto(kEGr)
    if(.not.SumEval_fit) then
      knetx = knetto(kEGr)
    else
      knetx = FindlocT(SymboleG,trim(avar(1)))
    end if
    write(66,*) 'Begin TopoSort:'
    call TopoSort(knetx)

    ksq = 0
    seqch = ' '
    ksq1 = knetx
    call chains(knetx,ksq1,ksq)
    ksq2 = ksq
    call IntModA1(ukenn,ksq2)
    call IntModA1(akenn,ksq2)
    call chainseval(1,ksq2)
    ksqlast = ksq2+1

    if(SumEval_fit) then
      do k=2,nparts
        knetx = FindlocT(SymboleG,trim(avar(k)))
        call TopoSort(knetx)
        ksq1 = knetx
        ksq2 = ksq1
        call  chains(knetx,ksq1,ksq2)
        call IntModA1(ukenn,ksq2)
        call IntModA1(akenn,ksq2)
        call chainseval(ksq1,ksq2)
        ksqlast = ksq2+1
      end do
    end if
     ! do i=1,ksq2
     !   write(66,*) 'i=',int(i,2),RnetParsInd(i),RnetPars(i) ,RnetparsCRate(i), RnetparsCRule(i),krate(i),ktime(i),kcnt(i)
     !   if(i == ksq2) write(66,*)
     ! end do
    nchmax = ksq2

      goto 25
25    continue
        if(rw1pro)  write(66,'(a,i0,a,100(i0,1x))') 'nach 25: nRnetp=',nRnetp,' RnetParsInd:',RnetParsInd(1:nRnetp)

    j = 0
    ! do k=1,nRnetp
    !    write(66,*)  'RW1: --1    k=',int(k,2),' RnetparsInd(k)=',int(RnetparsInd(k),2)
    ! end do
    do k=1,nRnetp
      i = RnetParsInd(k)
      ! write(66,*) 'RW1: ---  k=',int(k,2),' i=',int(i,2),' ',symbole(i)%s,' ',RnetParsCRate(k)
      if(.not.RnetParsCRate(k)) cycle
      ! if(.true. .and. iptr_rate(i) > 0 .and. iptr_rate(i) <= ngrs)  &
      if(.true. ) then
        j = j + 1
        if(j == 1) write(66,'(A)')  ' Sy iptr_time iptr_cnt iptr_rate   Symbol'
                  ! write(66,*) 'RW1: ---  k=',int(k,2),' i=',int(i,2),' ',symbole(i)%s,' j=',int(j,2)
        if(iptr_cnt(i) == 0) then
          write(66,'(4(i3,4x),T36,a)') i,iptr_time(i),iptr_cnt(i),iptr_rate(i),Symbole(i)%s
        else
             if(iptr_time(i) == iptr_cnt(i))  iptr_time(i) = 0
          write(66,'(4(i3,4x),T36,a)') i,iptr_time(i),iptr_cnt(i),iptr_rate(iptr_cnt(i)),Symbole(i)%s
        end if
      end if
      if(j == 1 .and. kbrutto(kEGr) > 0) then
        ! added: 02.6.2023 GK
        if(Symbole(i)%s /= Symbole(kbrutto(kEGr))%s) grossfail = .true.
      end if
    end do
  end if
          ! write(66,*) 'j=',j
  if(.true. .and. grossfail) then
    ! since 16.8.2023:
    if(knetto(kEGr) > 0) then
      m = RS_SymbolNr(knetto(kEGr),1)
        ! write(66,*) '  m=',m,' knetto(kEGr)=',knetto(kEGr)
      if(m == kbrutto(kEGr)) grossfail = .false.
    end if
  end if
  if(grossfail) then
    IF(langg == 'DE') WRITE(str1,*) 'Warnung: Die Bruttozählrate ' // symbole(kbrutto(kEGr))%s // &
                             ' ist nicht die erste Zählrate in der Gleichung der Nettozählrate!  ',char(13), &
                             ' Ist das richtige Symbol dafür selektiert?'
    IF(langg == 'EN') WRITE(str1,*) 'Warning: The gross count rate ' // symbole(kbrutto(kEGr))%s // &
                             ' is not the first count rate in the equation for the net count rate!  ', char(13), &
                             ' Has the correct symbol been selected for it?'
    IF(langg == 'FR') WRITE(str1,*) 'Le taux de comptage brut ' // symbole(kbrutto(kEGr))%s // &
                             ' n''est pas le premier taux de comptage dans l''équation du taux de comptage net ! ', char(13), &
                             ' Le bon symbole a-t-il été sélectionné ?'
    call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw2:", resp, mtype=GTK_MESSAGE_WARNING)
    IF(langg == 'DE') call WrStatusBar(4, trim('Problem Bruttozählrate beheben!'))
    IF(langg == 'EN') call WrStatusBar(4, trim('Remove problem with gross count rate!'))
    IF(langg == 'FR') call WrStatusBar(4, trim('Supprimez le problème avec les taux de comptage brut!'))
    ifehl = 1
    goto 9000
  end if

    if(apply_units_dir) then
      write(66,*)
      call CalcUnits()
      if(ifehl == 1) goto 9000
      write(66,*)
    end if
end if

if(.not.Fitdecay .and. .not.Gamspk1_Fit .and. ncov > 0 .and. ubound(messwert,dim=1) == ngrs) then
  call ModVarsTV2(ngrs+ncov)
  do i=ngrs+1,ngrs+ncov
    Messwert(i) = Covarval(i-ngrs)
    StdUnc(i) = missingval
  end do
end if

if(Rw1Pro) then
    Write(66,'(4(a,i0))') 'Rw1: before Pointnach:  ngrs_CP=',ngrs_CP,' ngrs=',ngrs,' ncov=',ncov,' numd=',numd
  do i=1,ngrs+ncov+numd              !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    WRITE(66,*) 'Rw1:  before PointNach:  i=',i,symbole(i)%s,'   Messwert=',sngl(Messwert(i)),' StdUnc=',  &
                 sngl(StdUnc(i))
  END do
end if
  if(ifehl == 1) write(66,'(a)') 'Rw1_282: ifehl = 1'

call PointNach(2)
if(ifehl == 1) then
    write(66,*) 'Rw1_286:   error in PointNach(2)'
  goto 9000
end if

if(Rw1Pro) then
  do i=1,ngrs+ncov+numd              !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    WRITE(66,*) 'Rw1:  after PointNach:  i=',i,symbole(i)%s,'   Messwert=',sngl(Messwert(i)),' StdUnc=',  &
                 sngl(StdUnc(i))
  END do
end if

IF(FitDecay .AND. knumEgr >=2 ) THEN
  ! dummy initialization of the fit parameter values if a new project
  ! is just being created.
  IF(abs(messwert(kfitp(1))-missingval) < eps1min) Messwert(kfitp(1)) = one
  IF(abs(messwert(kfitp(1)+1)-missingval) < eps1min) Messwert(kfitp(1)+1) = one
  IF(abs(messwert(kfitp(1)+2)-missingval) < eps1min) Messwert(kfitp(1)+2) = one
END IF

call gtk_widget_set_sensitive(idpt('radiobuttonPMLE'), 1_c_int)
IF(FitDecay .AND. ( knumEGr+1 > numd .or. knumEGr > 2 .or. nchannels > 1 ) ) THEN
  ! Under this condition, it is forbidden to use PMLE
  call gtk_widget_set_sensitive(idpt('radiobuttonPMLE'), 0_c_int)
end if

iteration_on = .FALSE.
GamDist_ZR = .FALSE.

   if(rw1pro) write(66,'(a,3(i0,1x))') 'RW1 263:  ngrs,ncov,numd=',ngrs,ncov,numd
call CharModA1(SymboleG,ngrs+ncov+numd)
call CharModA1(Symbole,ngrs+ncov+numd)
do i=1,ngrs+ncov+numd
  SymboleG(i)%s = ucase(Symbole(i)%s)
end do

netto_involved_Fitcal = .false.
if(fitcalcurve .and. KFitcal > 0) then
  do i=1,nRSsy(KFitcal)
    do j=1,ngrs
      if(j == KFitcal) cycle
      if(chupper_eq(RSSy(nRSSyanf(KFitcal)+i-1)%s, Symbole(j)%s) .and. j == knetto(kEGr)) then
        netto_involved_fitcal = .true.
        exit
      end if
    end do
  end do
  write(66,'(a,L1)') 'RW1:   netto_involved_Fitcal=',netto_involved_Fitcal
end if

   if(.false.) then
     write(66,'(a,i0,a,i0)') 'RW1: Equations:   nab=',nab,' nabf=',nabf
     do i=1,nab
       write(66,'(a,i3,a,a)') 'i=',i,' ',RSeite(i)%s
     end do
     do i=1,nabf
       write(66,'(a,i3,a,i3,a,a)') 'i=',nab+i,'  i=',i,' ',SDformel(nab+i)%s
     end do
   end if
ifehlps = 0

        if(rw1pro) write(66,'(a,5i4)') 'nab,nmodf,nabf,ncovf,ngrs=',nab,nmodf,nabf,ncovf,ngrs
      if(ifehl == 1) write(66,*) 'Rw1_345  ifehl=1'

call initf(nab+nmodf+nabf+ncovf)
call CharModA1(Rseite,nab+nmodf+nabf+ncovf+2*knumEGr)
do k=nab+1,nab+nmodf+nabf+ncovf+2*kEGr + 0*1
  if(k == nab+nmodf+nabf+ncovf+1 .and. FitCalCurve) Rseite(k)%s = kalfit_arg_expr
end do
do i=1,nab
  ifehlp = 0
  igl = i
  IF(FitDecay .AND. i == klinf) CYCLE
  IF(Gamspk1_fit .AND. i == kgspk1) CYCLE
  IF(SumEval_fit .AND. i == ksumeval) CYCLE
  if(FitCalCurve .and. i == kfitcal) then
    if(i >= 1 .and.len_trim(kalfit_arg_expr) > 0 .and. kpoint_kalarg > 0) then
      call CharModA1(Rseite,nab+nmodf+nabf+ncovf+nfkf)
      do k=nab+1,nab+nmodf+nabf+ncovf+nfkf
        if(k >= nab+nmodf+1 .and. k <= nab+nmodf+nabf) Rseite(k)%s = SDformel(k-(nab+nmodf))%s
      end do
      RSeite(nab+nmodf+nabf+ncovf+nfkf)%s = TRIM(kalfit_arg_expr)
      kpoint_kalarg = nab+nmodf+nabf+ncovf+nfkf
      igl = kpoint_kalarg
        write(66,'(3(a,i0))') 'kalfit_arg_expr:  Equation No. ',kpoint_kalarg,' nab=',nab,'  nabf=',nabf
    else
      cycle
    end if
  else
    call CharModA1(Rseite,nab+nmodf+nabf+ncovf+nfkf)   ! ????????????????? necessary?
  end if
  call CharModA1(Rseite,nab+nmodf+nabf+ncovf+nfkf)        ! 18.8.2023 ergänzt

  if(igl > ubound(Rseite,dim=1))  call CharModA1(Rseite,igl)   ! am 18.8.2023 ergänzt
      if(rw1pro) write(66,'(a,i0,a,a,a,i0)') 'before parsef:  igl=',igl,'  ',RSeite(igl)%s,'  ksumeval=',ksumeval

  call parsef(igl,RSeite(igl)%s,SymboleG)
  if(ifehl == 1) then
    write(66,*) 'RW1_377:  error in parsef for function: ',Rseite(igl)%s
    goto 9000
  end if
  if(rw1pro) WRITE(66,*) 'fparser: parsef of ',Rseite(igl)%s,' done: ifehlp=',ifehlp
  IF(ifehlp == 1) WRITE(66,*) '      Rseite(igl)=',RSeite(igl)%s,'  Gamspk1_fit=',  &
                              Gamspk1_fit,'  kgspk1=',kgspk1
  if(ifehlp == 1) then
    ifehlps = 1
    write(66,*) 'RW1_385:  error in parsef for function: ',Rseite(igl)%s
    goto 9000
  end if
end do
ifehlp = ifehlps
      if(ifehl == 1) write(66,*) 'Rw1_390  ifehl=1'

tree = idpt('treeview2')
do i=1,nab
  call WDListstoreClearCell('treeview2', 5, i)
end do
if(rw1pro) then
  do i=1,ngrs
    write(66,*) 'before finding nhp:  i=',int(i,2),'  ',symbole(i)%s,'  Messwert=',sngl(Messwert(i))
  end do
end if

IF(Gamspk1_fit) WRITE(66,'(a,i0)') 'Rechw1: kpoint(2)=',kpoint(2)

IF(Gamspk1_fit) THEN
  if(.not.batest_on .and. .not.automode)   &
       call WDListstoreFill_1('liststore_symbols', ngrsP, Symbole)
end if

!#############################
! 126  continue

      if(ifehl == 1) write(66,*) 'Rw1_413  before MDcalc: ifehl=1'
  do i=1,ngrs
    if(IVTL(i) == 7) then
      ! This loop is required when by subsequent editing
      ! of the equations shifts within the symbol list occur, and this
      ! refers also to the BinPoi parameters
                if(consoleout_gtk) write(0,'(a,i0)') 'RW1_424: kbgv_binom',kbgv_binom
      nfd = 0
      if(kbgv_binom > 0) then
        if(iptr_time(kbgv_binom) == 0) nfd = 1
      end if
      if(kbgv_binom <= 0 .or. nfd == 1) then
        if(langg == 'DE') call WrStatusbar(4,'BinPoi-Parameter überprüfen:')
        if(langg == 'EN' .or. langg == 'FR') call WrStatusbar(4,'Check BinPoi-Parameters:')
        dialogstr = 'dialog_BinPoi'
        ioption = 71
        call FindItemS(trim(dialogstr), ncitem2)
        call Loadsel_diag_new(1, ncitem2)
        iptr_cnt(i) = i
        write(66,'(a,4(i0,1x))') 'itm_binom,ip_binom,ilam_binom=',itm_binom,ip_binom,ilam_binom
        IF(ifehl == 1) goto 9000
      end if
      exit
    end if
  end do

  do j=knumEGr+1,ngrs
    if(ucase(symtyp(j)%s) == 'M') then
      ! execute the calculations of reducing user-supplied vectors of
      ! single values to a mean value and the associated uncertainty:
      call MDcalc(MDpointrev(j))
          if(ifehl == 1) then
            write(66,*) 'Rw1_481  after MDcalc: ifehl=1'
            goto 9000
          end if
      Messwert(j) = meanMD(MDpointrev(j))
      SDwert(j) = umeanMD(MDpointrev(j))
      Hbreite(j) = missingval
          write(66,'(a,i0,a,i0,2(a,es12.5))') 'mean:  j=',j,'  MDpointrev(j)=',MDpointrev(j),' MW(j)=', &
                     sngl(Messwert(j)),' SDwert(j)=',sngl(SDwert(j))
      nfd = 0
      do k=1,nab
        if(iptr_cnt(k) == j) nfd = 1
      end do
      if(j /= kbrutto(kEGr) .and. nfd == 0) SDFormel(j)%s = ' '
      StdUnc(j) = SDwert(j)
      call WTreeViewPutDoubleCell('treeview2', 5, j, Messwert(j))
      call WTreeViewPutStrCell('treeview2', 7, j, SDFormel(j)%s)
      call WTreeViewPutDoubleCell('treeview2', 8, j, SDwert(j))
      call WTreeViewPutDoubleCell('treeview2', 9, j, Hbreite(j))
      cycle
    end if
  end do

      if(ifehl == 1) write(66,*) 'Rw1_503  after MDcalc: ifehl=1'

  if(FitDecay) then
    ! prepare a list of symbols required for calculating the output
    ! quantity with index kEGr, excluding those symbols contained
    ! in the argument of the Linfit call:
    nsymb_kEGr = 0
    do i=1,nRSSy(kEGr)
      nsymb_kEGr = nsymb_kEGr + 1
      call charModA1(symb_kEGr,nsymb_kegr)
      symb_kEGr(nsymb_kEGr)%s = RSSy(nRSSyanf(kEGr)+i-1)%s
      do k=kEGr+1,nab
        if(k == klinf) cycle
        if(symb_kEGr(nsymb_kEGr)%s == SymboleG(k)%s) then
          do j=1,nRSsy(k)
            nsymb_kEGr = nsymb_kEGr + 1
            call charModA1(symb_kEGr,nsymb_kegr)
            symb_kEGr(nsymb_kEGr)%s = RSSy(nRSsyanf(k)+j-1)%s
          end do
        end if
      end do
    end do
    write(66,*) ' Symbols within the output quantity formula:',(symb_kEGr(i)%s,' ',i=1,nsymb_kEGr)
  end if

  nundf = 0
  do i=nab,1,-1
    ! Check first, whether symbol values used in the equation still have
    ! missing values:
    IF(i <= knumEGr .AND. i /= kEGr) CYCLE
    IF(FitDecay .AND. knumEgr > 1 .AND. (i >= kfitp(1) .AND. i <= kfitp(1)+2 )) CYCLE
    rst = trim(ucase(Rseite(i)%s))
    do k=1,nRSsy(i)
      IF(LEN_TRIM(RSSy(nRSSyanf(i)+k-1)%s) == 0) EXIT
      do j=1,ngrs+ncov+numd
        IF(RS_SymbolNr(i,k) == j) THEN
          IF(FitDecay .AND. j == klinf) CYCLE
          IF(Gamspk1_fit .AND. j == kgspk1) CYCLE
          if(FitCalCurve .and. j == kfitcal) cycle
          if(SumEval_fit .and. j == ksumeval) cycle
          IF(abs(Messwert(j)-missingval) < eps1min) THEN

            nundf = nundf + 1
            WRITE(66,'(a,i0,a,a,a,a,a)') 'Eq.',int(i,2),': in formula ',TRIM(rst),' the symbol value for ', &
                        RSSy(nRssy(i)+k-1)%s,' is not defined!'
            WRITE(66,*) '     j=',j,'  Messwert(j)=',sngl(Messwert(j))
            WRITE(66,*) '     RS_Symbole(i,k)=',RSSy(nRssyanf(i)+k-1)%s,   &
                        ' SymboleG(j)=',SymboleG(j)%s

            IF(j > nab) THEN
              IF(FitDecay .AND. (j >= kfitp(1) .AND. j <= kfitp(1)+2 )) CYCLE
              call CharModStr(str1,500)
              IF(langg == 'DE') WRITE(str1,*) 'Der Wert von Symbol ', &
                                Symbole(j)%s,' ist nicht definiert!'
              IF(langg == 'EN') WRITE(str1,*) 'The value of the symbol ', &
                                Symbole(j)%s,' is not defined!'
              IF(langg == 'FR') WRITE(str1,*) 'La valeur du symbole ', &
                                Symbole(j)%s,' n''est pas définie!'
              call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
              ifehl = 1
                WRITE(66,'(a,i0)') 'Rechw1: j>nab: ifehl=',ifehl
              IF(langg == 'DE') call WrStatusbar(4, &
                                trim('Fehlende Werte ergänzen!' ))
              IF(langg == 'EN') call WrStatusbar(4, &
                                trim('Complement missing values' ))
              IF(langg == 'FR') call WrStatusbar(4, &
                                trim('Compléter les valeurs manquantes' ))
              goto 9000
            END IF

            IF(i > 1) GOTO 50          ! in case of an error
          END IF
          IF(ivtl(j) == 0) THEN
            call CharModStr(str1,500)
            IF(langg == 'DE') WRITE(str1,*) 'Die Verteilung von Symbol ', &
                                  RSSy(nRssyanf(i)+k-1)%s,' ist nicht definiert!'
            IF(langg == 'EN') WRITE(str1,*) 'The distribution of the symbol ', &
                                  RSSy(nRssyanf(i)+k-1)%s,' is not defined!'
            IF(langg == 'FR') WRITE(str1,*) 'La distribution du symbole ', &
                                  RSSy(nRssyanf(i)+k-1)%s,' n''est pas défini!'
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp, mtype=GTK_MESSAGE_WARNING)
            ifehl = 1
            goto 9000
          END IF
          IF(IAR(j) == 0) THEN
            call CharModStr(str1,500)
            IF(langg == 'DE') WRITE(str1,*) 'Für das Symbol ',RSSy(nRssyanf(i)+k-1)%s,  &
                              ' ist nicht definiert, ob abs. oder relat. Unsicherheit vorliegt!'
            IF(langg == 'EN') WRITE(str1,*) 'For symbol ',RSSy(nRssyanf(i)+k-1)%s,  &
                              ' it is not defined, whether uncertainty is abs. or relative!'
            IF(langg == 'FR') WRITE(str1,*) 'Pour le symbole ',RSSy(nRssyanf(i)+k-1)%s,  &
                              ' ce n''est pas défini, l''incertitude est abs. ou relatif!'
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
            ifehl = 1
            goto 9000
          END IF
        END IF
      end do       ! j
    end do     ! k

    !Calculation by formulae:
    IF(FitDecay .AND. i == klinf) THEN
      if(.not.loadingPro) then
        IF(langg == 'DE') call WrStatusbar(4, &
                         'Tabelle "Abkling-Messreihe" ausfüllen oder ändern')
        IF(langg == 'EN') call WrStatusbar(4, &
                         'Table "Values of decay curve": fill in or edit values')
        IF(langg == 'FR') call WrStatusbar(4, &
                         'Tableau "Valeurs de la courbe de décroissance": remplit ou édite les valeurs')
      end if

      IF(k_rbl > 0) then
        if(Messwert(kpoint(k_rbl)) <= zero) THEN
          call CharModStr(str1,500)
          IF(langg == 'DE') WRITE(str1,*) 'Wert der Blindwertzählrate ' // Symbole(k_rbl)%s // &
                         ' muss vorher eingegeben werden!' &
                          // CHAR(13) // 'Die Berechnung wird hier abgebrochen,' &
                          // CHAR(13) // 'damit dieser Wert nachgetragen werden kann.'
          IF(langg == 'EN') WRITE(str1,*) 'Value of blank count rate ' // Symbole(k_rbl)%s // ' must be entered!' &
                          // CHAR(13) // 'The calculation is stopped here' &
                          // CHAR(13) // 'to allow that this value can be completed.'
          IF(langg == 'FR') WRITE(str1,*) 'Valeur du taux de comptage des blancs ' // Symbole(k_rbl)%s // ' doit être entré!' &
                          // CHAR(13) // 'Le calcul est arrêté ici' &
                          // CHAR(13) // 'pour permettre à cette valeur d''être complétée.'
          call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
          ifehl = 1
          goto 9000
        end if
      END IF

      ! read decay curve data ny invoking the dialog:
      ioption = 3
      ifehlx = 0
      dialogstr = 'dialog_decayvals'
      call FindItemS(dialogstr, ncitem)
      call Loadsel_diag_new(1, ncitem)
      IF(ifehl == 1) then
           write(66,'(a,i0)') 'After Laodsel (3):  ifehl=',ifehl
        goto 9000
      end if
      ifehlx = ifehl
      if( (.not.defineallxt .and. nmodf < (nchannels*knumEGr) )  .or.   &
         (defineallxt .and. nmodf < (knumEGr*numd/nchannels*nchannels) ) ) then
          call CharModStr(str1,500)
        IF(langg == 'DE') WRITE(str1,*) 'Die Anzahl der Xi-Formeln im FitDecay-Modell ' &
                        // char(13) // 'ist nicht konsistent mit den Anzahlen von Messungem, ' &
                        // char(13) // ' Zählkanälen und Ergebnisgrößen!' &
                        // CHAR(13) // 'Die Berechnung wird hier abgebrochen, ' &
                        // 'siehe Kapitel 7.11.3 in der CHM-Hilfe.'
        IF(langg == 'EN') WRITE(str1,*) 'The numer of Xi formulae of the FitDecay model ' &
                        // CHAR(13) // 'is not consistent with the numbers of measurements,' &
                        // CHAR(13) // '  counting channels and output quantities!' &
                        // CHAR(13) // 'The calculations are stopped here, ' &
                        // 'see chapter 7.11.3 within the CHM Help.'
        IF(langg == 'FR') WRITE(str1,*) 'Le nombre de formules Xi du modèle FitDecay n''est pas ' &
                        // CHAR(13) // 'cohérent avec le nombre de mesures, ' &
                        // CHAR(13) // '  comptage des canaux et des quantités de sortie! ' &
                        // CHAR(13) // 'Les calculs sont arrêtés ici, '  &
                        // CHAR(13) // 'voir chapitre 7.11.3 dans l''aide CHM. '
        call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
        ifehl = 1
        goto 9000
      end if


      ccdatum = CFaelldatum
      do i11=1,LEN_TRIM(Ccdatum)
        IF(Ccdatum(i11:i11) == '.') Ccdatum(i11:i11) = ' '
        IF(Ccdatum(i11:i11) == ':') Ccdatum(i11:i11) = ' '
      end do
      READ(Ccdatum,*,IOSTAT=ios) (idat1(jj),jj=1,5)
      IF(ios /= 0) THEN
          !! write(66,*) 'Rw1_640:  Error reading the date/time of precipitation'
        ifehl = 1
        ! goto 9000
      END IF
      ! check, whether cStartzeit contains a date format
         READ(Ccdatum,*,IOSTAT=ios) (idat1(jj),jj=1,6)
         if(ios /= 0) then
           idat1(6) = 0
           READ(Ccdatum,*,IOSTAT=ios) (idat1(jj),jj=1,5)
         end if
      IF(idat1(3) > 70 .and. idat1(3) < 100) idat1(3) = idat1(3) + 1900
      IF(idat1(3) < 50 ) idat1(3) = idat1(3) + 2000
      do ix=1,numd
        Ccdatum = CStartzeit(ix)%s
        istdatum = .FALSE.
        READ(ccdatum,*,IOSTAT=ios) dtdiff(ix)
        IF(ios /= 0) istdatum = .TRUE.
        if(ifehl == 1 .and. istdatum) then
          write(66,*) 'Rw1_640:  Error reading the date/time of precipitation'
          ifehl = 1
          goto 9000
        end if
        if(ifehl == 1 .and. .not.istdatum) ifehl = 0

        IF(istdatum) THEN
          do i11=1,LEN_TRIM(Ccdatum)
            IF(Ccdatum(i11:i11) == '.') Ccdatum(i11:i11) = ' '
            IF(Ccdatum(i11:i11) == ':') Ccdatum(i11:i11) = ' '
          end do
          READ(Ccdatum,*,IOSTAT=ios) (idat2(jj),jj=1,5)
          IF(ios /= 0) THEN
            READ(CCdatum,*,IOSTAT=ios2) dtdiff(ix)
            IF(ios2 /= 0) THEN
              ifehl = 1
              goto 9000
            end if
          END IF
            READ(Ccdatum,*,IOSTAT=ios) (idat2(jj),jj=1,6)
            if(ios /= 0) then
              idat2(6) = 0
              READ(Ccdatum,*,IOSTAT=ios) (idat2(jj),jj=1,5)
            end if
          IF(idat2(3) > 70 .and. idat2(3) < 100) idat2(3) = idat2(3) + 1900
          IF(idat2(3) < 50 ) idat2(3) = idat2(3) + 2000
          dtdiff(ix) = datdif6(idat1,idat2) * 24._rn * 3600._rn       ! in seconds
        else
          READ(Ccdatum,*,IOSTAT=ios2) dtdiff(ix)
          IF(ios2 /= 0) THEN
            ifehl = 1
            goto 9000
          end if
        end if
        ! WRITE(66,'(a,i2,a,f10.2,a,L1)') 'Decay-curve: dtdiff(',ix,')=',dtdiff(ix),'   istdatum=',istdatum  !,' idat2(6)=',idat2(6)

      end do
      if(consoleout_gtk) write(0,'(a,i0)') 'RW1: reading of the decay curve date done:  i=',i
      CYCLE
    END IF


    IF(Gamspk1_Fit .AND. i == kgspk1) THEN
      if(.not.loadingPro) then
        IF(langg == 'DE') call WrStatusbar(4,trim('Tabelle "Spektrums-Werte" ausfüllen oder ändern' ))
        IF(langg == 'EN') call WrStatusbar(4,trim('Edit table "Spektrum values"' ))
        IF(langg == 'FR') call WrStatusbar(4,trim('Editer le tableau "Valeurs de spectre"' ))
      end if

      ! read gamma peak data by invokling the dialog:
      ioption = 5
      dialogstr = 'dialog_gspk1'
      call FindItemS(dialogstr, ncitem)
      call Loadsel_diag_new(1, ncitem)               !, c_null_ptr)
      IF(ifehl == 1) then
         write(66,*) 'RW1_706:  Error in input of gamma peak data: stopped!'
        goto 9000
      end if
      ifehlx = ifehl
      CYCLE
    END IF

    if(FitCalCurve .and. i == kfitcal) then
         write(66,'(2(a,i0))') 'RW1_720:  nkalpts=',nkalpts,' maKB=',maKB
      if(nkalpts == 0 .or. maKB == 0) then
        call LinCalib()
      end if
      maKB = kal_polgrad + 1
      write(66,'(2(a,i0))') 'Fitcalcurve:  kpointKB(kEGr)=',kpointKB(kEGr),'  kpoint_kalarg=',kpoint_kalarg
      xx = gevalf(kpoint_kalarg,Messwert)       ! evaluate the formula number kpoint_kalarg (the KalFit call)
      call CalibInter(KFmode, xx, StdUnc(kpointKB(1)), yval,uyval)
      Messwert(i) = yval
         write(66,*) 'RW1_728:  yval, uyval=',sngl(yval),sngl(uyval)
      if(abs(uyval) > eps1min) StdUnc(i) = uyval
      cycle
    end if

    IF(SumEval_Fit .AND. i == ksumeval) THEN
      call SumEvalCalc(yval,uyval)
      Messwert(i) = yval
      if(abs(uyval) > eps1min) StdUnc(i) = uyval

      CYCLE
    END IF
    res = gevalf(i,Messwert)
    IF(ifehlp == 1) then
         write(66,*) 'RW1_735:  Error with gevalf (calculate function value for Symbol ',Symbole(i)%s
      goto 9000       ! <-----------------------
    end if
    Messwert(i) = res

50        CONTINUE

  end do          ! loop i=nab,1,-1
  !--------------------------------------------------------------------------------
  WRITE(66,'(a,i0,a,i0,a,i0)') 'iwh=',iwh, '  nundf=',nundf,'  ifehl=',int(ifehl,2)

!   here, klincall is still = 0

do i=1,nab
  IF(abs(Messwert(i)-missingval) > eps1min) call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert(i))
end do

do i=nab+1,ngrs
  IF(abs(Messwert(i)-missingval) < eps1min) THEN
    write(ch1,'(i3)') i
    call CharModStr(str1,500)
    IF(langg == 'DE') str1 = 'Achtung: der Wert für das Symbol i=' // ch1 //  &
                      Symbole(i)%s // CHAR(13)// 'ist noch nicht definert! Bitte nachholen.'
    IF(langg == 'EN') str1 = 'Warning: the value of the symbol i=' // ch1 // &
                      Symbole(i)%s // CHAR(13) // 'has not yet been defined!'
    IF(langg == 'FR') str1 = 'Attention: la valeur du symbole i=' // ch1 // &
                      Symbole(i)%s // CHAR(13) // 'n''a pas encore été défini!'
    call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
    write(66,'(a,i0)') '  ifehl=1:  ngrs=',ngrs
    IF(langg == 'DE') call WrStatusbar(4,trim('Fehlende Werte ergänzen!' ))
    IF(langg == 'EN') call WrStatusbar(4,trim('Complement missing values!' ))
    IF(langg == 'FR') call WrStatusbar(4,trim('Compléter les valeurs manquantes!' ))
    ifehl = 1
    goto 9000
  end if
end do

if(allocated(MesswertSV)) deallocate(MesswertSV)
allocate(MesswertSV,source=Messwert)
MesswertSV = 0._rn

nng = ngrs+ncov+numd
if(ubound(Messwert,DIM=1)  >= nng) then
  ix1 = ubound(MesswertSV,dim=1)
  if(nng > ix1) call RealModA1(MesswertSV,nng)
  MesswertSV(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)  ! important!
    ! do i=1,ngrs
    !   write(66,*) ' RW1_782:   i=',int(i,2),' MWSV=',sngl(MesswertSV(i)),    &   ! ' StdUncSV=',sngl(StdUncSV(i)), &
    !                ' MW=',sngl(Messwert(i)),' StdUnc=',sngl(StdUnc(i))
    ! end do
end if


!WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
nabf = 0
!WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
knullef = 0
kbrutto_gl = 0
IVTL7 = 0

do i=ngrs,knumEGr+1,-1

  IF(LEN_TRIM(sdformel(i)%s) > 0) THEN
     WRITE(66,'(a,a,a,i0)') 'working on sdformel(i)=',sdformel(i)%s,'  i=',i

    do j=1,len_trim(sdformel(i)%s)
      if(sdformel(i)%s(j:j) == ',') sdformel(i)%s(j:j) = '.'
    end do
    sdfG = ucase(sdformel(i)%s)
    j0 = index(sdfG,'KALFIT')

    if(j0 > 0) then
      if(.not.FitCalCurve) FitCalCurve = .TRUE.
      i0 = INDEX(sdfG,'KALFIT')
      i1 = index(sdfG(i0:),'(')
      i2 = index(sdfG(i0:),',')
      i3 = index(sdfG(i0:),')')
      read(sdfG(i0+i1:i0+i2-1),*) KFmode
      write(66,*) 'KALFIT:  Argument expression=',trim(kalfit_arg_expr)
      call LinCalib()
      maKB = kal_polgrad + 1

      call initf(1)
      call parsef(1,kalfit_arg_expr,SymboleG)
        if(ifehl == 1) then
            write(66,*) 'RW1_820:  error in parsef for function: ',kalfit_arg_expr
          goto 9000
        end if
      xx = gevalf(1,Messwert)
      call CalibInter(KFmode, xx, zero, yval,uyval)
      write(cxkb,'(es12.5)') real(yval,8)
      j2 = index(sdfG(j0:),')')
      sdfSave = sdformel(i)%s
      sdformel(i)%s = sdfG(1:j0-1) // trim(cxkb) // trim(sdfG(j0+j2:))
      nxx = 1
      call initf(nxx)
      call parsef(1,sdformel(i)%s,SymboleG)
        if(ifehl == 1) then
            write(66,*) 'RW1_833:  error in parsef for function: ',sdformel(i)%s
          goto 9000
        end if
      IF(ifehlp == 1) then
        sdformel(i)%s = trim(sdfSave)
        goto 9000
      end if
      res = gevalf(1,Messwert)
      IF(ifehlP == 1) then
        sdformel(i)%s = trim(sdfSave)
        goto 9000
      end if
      sdformel(i)%s = trim(sdfSave)

    else

      if(GamDist_ZR) then
         !xx          dummy = Messwert(i)
         !xx          MEsswert(i) = Resulta(i)
      end if
      nxx = 1
      call initf(nxx)
      call parsef(1,sdformel(i)%s,SymboleG)
      IF(ifehlp == 1) then
         write(66,*) 'Rw1:   ifehlp=1:     sdformel=',sdformel(i)%s,' parsef error'
        goto 9000
      end if

      res = gevalf(1,Messwert)
      if(GamDist_ZR) then
        !xx        Messwert(i) = dummy
      end if
      IF(ifehlP == 1) then
         write(66,*) 'Rw1:   ifehlp=1:     sdformel=',sdformel(i)%s,'  Wert=',sngl(res)
        goto 9000
      end if
    end if
    IF(i /= kbrutto(kEGr) .OR. ( abs(SDWert(i)-missingval) < eps1min .OR.   &
                                 abs(SDWert(i)) < eps1min ) ) THEN
      if(i /= kbrutto(kEGr)) SDWert(i) = res
    END IF
    IF(abs(SDWert(i)-res) > eps1min .and. ucase(symtyp(i)%s) /= 'M') SDWert(i) = res

    ! here, the repeated transfer of the nabf SD formulae to the Rseite-array is done

    nvh = 0      ! test first, whether ths SD formula is already known
    do k=1,nab+nmodf+nabf
      if(sdformel(i)%s == Rseite(k)%s) then
        nvh = 1
        exit
      end if
    end do
    if(nvh == 0) then
        call CharModA1(Rseite,nab+nmodf+nabf+ncovf+nfkf+2*knumEGr + 1)
        do k=nab+nmodf+nabf+ncovf+nfkf+2*knumEGr, nab+nmodf+nabf+1, -1
          Rseite(k+1)%s = Rseite(k)%s
        end do
      nabf = nabf + 1
      RSeite(nab+nmodf+nabf)%s = sdformel(i)%s
    end if

    ! pick up special SD formulae:
    IF(i == kbrutto(kEGr)) then
      k = FindlocT(Rseite,SDformel(i)%s, nab+nmodf+1)
      if(k > 0) then
        kbrutto_gl(kEGr) = k         ! index of the formula in the list RSeite
        exit
      end if
    END IF
    IF(k_rbl > 0) then
      if(kpoint(k_rbl) > 0 .AND. kpoint(k_rbl) == i) then
        k = FindlocT(Rseite,SDformel(kpoint(k_rbl))%s, nab+nmodf+1)
        if(k > 0) then
          knullef = k         ! index of the formula in the list RSeite
          exit
        end if
      end if
    end if
    if(kbgv_binom > 0) then
      if(IVTL(i) == 7 .and. len_trim(SDFormel(i)%s) > 0 ) then
               ! ii = Findloc(IVTL,7,dim=1)
        IVTL7 = IVTL7 + 1
        k = FindlocT(Rseite,SDformel(i)%s, nab+nmodf+1)
        if(k > 0) then
          bipoi_gl = k         ! index of the formula in the list RSeite
          write(66,'(a,i3,a)') 'RW1:  bipoi_gl=nab+nmodf+k=',k
        end if
        call gtk_widget_set_sensitive(idpt('BinPoiPars'),1_c_int)
          use_bipoi = .true.
      end if
    end if

  END IF
    ! treating SDformel condition ends here
end do

do i=ngrs,knumEGr+1,-1
  ! other variables: copy SDwert to StdUnc:
  IF(abs(Sdwert(i)-missingval) > eps1min) then
    if(abs(StdUnc(i)-missingval) < eps1min) then
      if(IAR(i) == 1) StdUnc(i) = SDwert(i)
      if(IAR(i) == 2) StdUnc(i) = SDwert(i)*Messwert(i)
    end if
    call WTreeViewPutDoubleCell('treeview2', 8, i, SDWert(i))
    call WTreeViewPutDoubleCell('treeview2', 11, i, SDWert(i))
  end if
end do       ! End of loop variables i

   if(consoleout_gtk) write(0,*) 'RW1: End of loop of calculating the SD formulae'

if(ifehl == 1) WRITE(66,'(a,i0)') 'RW1_1008   ifehl=',ifehl
if(IVTL7 == 0) then
  use_bipoi = .false.
  kbgv_binom = 0
  bipoi_gl = 0
  ip_binom = 0
  itm_binom = 0
  ilam_binom = 0
  call gtk_widget_set_sensitive(idpt('BinPoiPars'),0_c_int)
end if

if(kbrutto(kEGr) > 0 .and. kbrutto(kEGr) <= ngrs) then
  if(IVTL(kbrutto(kEGr)) == 9 .and. ucase(symtyp(kbrutto(kEGr))%s) == 'M') then
    var_brutto_auto = .true.
    k_autoform = kbrutto(kEGr)
  else
    if(kbrutto(kEGr) <= nab) then
      if(nRSsy(kbrutto(kEGr)) == 2) then
        kngross = RS_SymbolNr(kbrutto(kEGr),1)
        if(IVTL(kngross) == 9 .and. ucase(symtyp(kngross)%s) == 'M') then
          var_brutto_auto = .true.
          k_autoform = kngross
        end if
      end if
    end if
  end if
  if(var_brutto_auto) then
    call WTreeViewSetColorCell('treeview2',7, kbrutto(kEGr), green_bg)   ! '#F57900')     ! green
  end if
  if(var_brutto_auto) goto 360
end if

IF(kbrutto_gl(kEGr) == 0 .AND. .not.FitDecay .AND. .NOT.Gamspk1_Fit .AND. .not.loadingPro .and.  &
                                             .not.SumEval_fit .and. .not.gum_restricted) THEN
  call CharModStr(str1,500)
  IF(langg == 'DE') str1 = 'Achtung: die StdAb-Formel für die Brutto-zählrate ' // CHAR(13) &
            // 'ist noch nicht definert! Bitte nachholen.'
  IF(langg == 'EN') str1 = 'Warning: the Stand.Dev formula for the gross count rate ' // CHAR(13) &
            // 'has not yet been defined!'
  IF(langg == 'FR') str1 = 'Attention: la formule Stand.Dev pour le brut compte ' // CHAR(13) &
            // 'n''a pas encore été défini!'
  call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
    if(loadingPro) gum_restricted = .true.

END IF

IF(kbrutto(kEGr) > 0 .and. kbrutto(kEGr) <= ngrs) then
  if(.not.FitDecay .AND. .NOT.Gamspk1_Fit .and. .not.SumEval_fit .AND.   &
    IVTL(kbrutto(kEGr)) /= 2 .and. IVTL(kbrutto(kEGr)) /= 3 .and. .not.gum_restricted) THEN
    if(kbrutto(kEGr) <= nab) then
      IF(INDEX(sdformel(kbrutto(kEGr))%s,symbole(kbrutto(kEGr))%s) == 0) THEN
        nfd = 0
        if(iptr_cnt(kbrutto(kEGr)) > 0) then
          ! this check:
          ! the SD formula in the green cell may read "uval(Nb)/t" in case of Rb=Nb/t ist
          do k=1,nRSsy(kbrutto(kEGr))
            if(RSSy(nRssyanf(kbrutto(kEGr))+ k-1)%s == SymboleG(iptr_cnt(kbrutto(kEGr)))%s) then
              nfd = 1
              exit
            end if
          end do
        end if
        if(nfd == 0) then
          call CharModStr(str1,500)
          IF(langg == 'DE') str1 = 'Achtung: in der StdAbw-Formel für die Brutto-Zählrate ' //  &
                                   symbole(kbrutto(kEGr))%s // CHAR(13) &
                                  // 'fehlt das Symbol der Brutto-Zählrate! Bitte ändern.'
          IF(langg == 'EN') str1 = 'Warning: in the the Stand.Dev formula for the gross count rate ' // &
                                   symbole(kbrutto(kEGr))%s  // CHAR(13) &
                                  // 'the gross count rate symbol is missing! Please, correct this!'
          IF(langg == 'FR') str1 = 'Attention: dans la formule écart-type pour le brut compte ' // &
                                   symbole(kbrutto(kEGr))%s  // CHAR(13) &
                // 'le symbole de taux de comptage de brut est manquant! S''il vous plaît, corrigez ceci!'
          call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
          if(.not.loadingPro) then
            ifehl = 1
            goto 9000
          end if
        end if
      end if
    end if
  end if
end if

360    continue
! Calculate the uncertainties of (independent) input quantities
  if(consoleout_gtk) write(0,*) 'Calculate the uncertainties of (independent) input quantities:'
StdUnc(knumEgr+1:ngrs) = zero

if(allocated(StdUncSV)) deallocate(StdUncSV)
allocate(StdUncSV,source=StdUnc)
StdUncSV(1:size(StdUnc)) = 0._rn

do iwh=1,2
   !  iwh=1: treat StdUnc only for quantities without defined SD formula;
   !  iwh=2: treat only the SD formuale
  do i=nab+1,ngrs
          if(abs(Messwert(i)) < eps1min) write(66,*)' RW1   **1 : i=',i, &
                                                '  MEsswert=',sngl(Messwert(i))
    if(iwh == 1 .and. ucase(symtyp(i)%s) == 'M') then
      if(.true. .or. i /= kbrutto(kEGr)) then
            call MDcalc(MDpointrev(i))
        if(.not.rinflu_known) then
          Messwert(i) = meanMD(MDpointrev(i))
          SDwert(i) = umeanMD(MDpointrev(i))
          Hbreite(i) = missingval
          StdUnc(i) = SDwert(i)
        else
          if(MDpointrev(i) /= refdataMD) then
            Messwert(i) = meanMD(MDpointrev(i))
            SDwert(i) = sqrt( (messwert(i) + (theta_ref*messwert(i))**two ) &
                                          / nvalsMD(MDpointrev(i)) )
            Hbreite(i) = missingval
            StdUnc(i) = SDwert(i)
          end if
        end if
        call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert(i))
        call WTreeViewPutStrCell('treeview2', 7, i, SDFormel(i)%s)
        call WTreeViewPutDoubleCell('treeview2', 8, i, SDwert(i))
        call WTreeViewPutDoubleCell('treeview2', 9, i, Hbreite(i))
        if(ivtl(i) /= 10) goto 370
      end if
    end if

    if(iwh == 2 .and. len_trim(SDFormel(i)%s) > 0) then
        call initf(1)
        call parsef(1,sdformel(i)%s,SymboleG)
        IF(ifehlp == 1) then
           write(66,*) 'Rw1:   ifehlp=1:  iwh=2   sdformel=',sdformel(i)%s,' parsef error'
          goto 9000
        end if
          if(consoleout_gtk) write(0,*) 'RW1_1120: before gevalf'
           res = -one
        res = gevalf(1,Messwert)
          if(consoleout_gtk) write(0,*) 'RW1_1123: after gevalf'
      if(res > zero) then
        SDWert(i) = res
      end if
         if(apply_units_dir) SDWert(i) = SDWert(i)/unit_conv_fact(i)    ! convert back to original unit
      IF(IAR(i) == 1) StdUnc(i) = SDWert(i)        ! res
      IF(IAR(i) == 2) StdUnc(i) = SDWert(i)*Messwert(i)

        if(IVTL(i) == 9) then
          nn2 = findlocT(DistPars%symb,Symbole(i)%s)
          if(nn2 > 0) then
            xnn = DistPars%pval(nn2,1)
            StdUnc(i) = DistPars%pval(nn2,3)
            call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
          end if
        end if
    end if

    if(iwh == 2) goto 370
    select case (ivtl(i))
        ! iar(i) =1  absolute uncertainty, iar(i)=2: relative uncertainty
      case (1)      ! Normal distribution
        IF(abs(SDwert(i)-missingval) < eps1min) CYCLE
        IF(IAR(i) == 1) StdUnc(i) = SDWert(i)
        IF(IAR(i) == 2) StdUnc(i) = SDWert(i)*Messwert(i)
      case (2)      ! rectangular distribution
        IF(abs(HBreite(i)-missingval) < eps1min) CYCLE
        IF(IAR(i) == 1) StdUnc(i) = HBreite(i)/SQRT(three)
        IF(IAR(i) == 2) StdUnc(i) = (HBreite(i)*Messwert(i))/sqrt(three)
      case (3)      ! triangular distribution
        IF(abs(HBreite(i)-missingval) < eps1min) CYCLE
        IF(IAR(i) == 1) StdUnc(i) = HBreite(i)/SQRT(6._rn)
        IF(IAR(i) == 2) StdUnc(i) = (HBreite(i)*Messwert(i))/sqrt(6._rn)
      case (4)      ! Gamma distribution = (N+1)-Regel
        if(abs(Messwert(i)) < eps1min .and. abs(GamDistAdd) < eps1min) then
          ! since12.4.2018 new version of the (N+x)-rulel
          StdUnc(i) = sqrt(one)
        else
          StdUnc(i) = SQRT(Messwert(i)+GamDistAdd)
        end if

      case(5)        ! Lognormal distribution
        IF(abs(SDwert(i)-missingval) < eps1min) CYCLE
        IF(IAR(i) == 1) StdUnc(i) = SDWert(i)
        IF(IAR(i) == 2) StdUnc(i) = SDWert(i)*Messwert(i)

      case(6,8,10)        ! Gamma distribution, beta2,beta4
        IF(abs(SDwert(i)-missingval) < eps1min) CYCLE
        IF(IAR(i) == 1) StdUnc(i) = SDWert(i)
        IF(IAR(i) == 2) StdUnc(i) = SDWert(i)*Messwert(i)

      case(9)        ! t-dist
        IF(abs(SDwert(i)-missingval) < eps1min) CYCLE
        nn2 = findlocT(DistPars%symb,Symbole(i)%s)
        if(nn2 > 0) then
          xnn = DistPars%pval(nn2,1)
          StdUnc(i) = DistPars%pval(nn2,3)         ! *sqrt(xnn/(xnn-two))
          call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
            ! write(66,*) '10:  i=',int(i,2),'  Distpars%pval=',sngl(DistPars%pval(nn2,1:3)), &
            !              ' StdUnc(i)=',sngl(StdUnc(i))
        end if

      case default
    end select

370     continue

         if(abs(StdUnc(i)-missingval) > eps1min) StdUnc(i) = abs(StdUnc(i))
    StdUncSV(i) = StdUnc(i)
    IF(abs(StdUnc(i)-missingval) > eps1min) then
      if(apply_units_dir) call WTreeViewPutDoubleCell('treeview2', 8, i, SDWert(i))

      call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))   ! format frmt
      if(StdUnc(i) > zero .and. Messwert(i) > zero) then
        if(StdUnc(i)/Messwert(i) > 2.5_rn) then
          IF(langg == 'DE') str1 = 'Achtung: die relative Std.Abw. für ' //  &
                                    symbole(i)%s // ' ist > 2.5!'
          IF(langg == 'EN') str1 = 'Warning: the relative std.dev. for ' // &
                                   symbole(i)%s  // ' is > 2.5!'
          IF(langg == 'FR') str1 = 'Attention: le relatif écart-type pour ' // &
                                   symbole(i)%s  // ' est > 2.5!'
          call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
        end if
      end if
    END IF

  end do
    ! End of calculating uncertainties of input quantities

end do   ! iwh
dep_unc_done = .false.

WRITE(66,'(5(a,i0,1x))') 'nabf=',nabf,'  kbrutto_gl=',kbrutto_gl,' ngrsP=',ngrsP,'  ifehl=',ifehl
!-----------------------------------------------------------------------

if(ubound(SymboleA,dim=1) < ngrs) call CharModA1(SymboleA,ngrs)
if(ubound(SymboleB,dim=1) < ngrs) call CharModA1(SymboleB,ngrs)

IF(.NOT. Gamspk1_Fit) THEN
  ncov  = 0       ! number of covariance pairs
  ncovf = 0       ! number of covarianzce formulae
end if

IF(Gamspk1_Fit) THEN
  ! restore StdUnc values for peak efficiencies:
  if(ngrs+ncov+numd > ubound(StdUnc,dim=1)) call RealModA1(Stdunc,ngrs+ncov+numd)
  do i=1,numd/5
    nhh = (i-1)*5 + 1
    StdUnc(ngrs+ncov+nhh+1)     = SDEffi(i)
  end do
  ! if(ncov == 0) goto 375
  ! ncov = 0
  write(66,'(a,i0)') 'Rw1_1188: ncovmx=',ncovmx
end if

if(sum(IsymbA) > 0) then
  imax = ubound(IsymbA,dim=1)
  do i=imax,1,-1
    if(IsymbA(i) > 0) then
      imax = i
      exit
    end if
  end do
  WRITE(66,'(a,12i4)') 'RECHW1: treating covariances:  IsymbA(.)=',(isymba(i),i=1,imax)
  WRITE(66,'(a,12i4)') 'RECHW1: treating covariances:  IsymbB(.)=',(isymbb(i),i=1,imax)
end if
write(66,*) 'RW1-1267:  ubound(IsymbA,dim=1)=',ubound(IsymbA,dim=1)
do i=1,ncovmx
   if(i > ubound(IsymbA,dim=1)) cycle
  IF(IsymbA(i) == 1 .AND. ISymbB(i) == 1) EXIT
  IF(IsymbA(i) == 0 .or. ISymbB(i) == 0) EXIT

write(66,*) '  RW1_1273: Treating the covariances: i=',i,'   IsymbA(i)=',isymba(i),'  ISymbB(i)=',isymbb(i)

  IF(IsymbA(i) > 0 .AND. ISymbB(i) > 0 .and. IsymbA(i) /= IsymbB(i)) THEN
    ncov = ncov + 1
    SymboleA(i) = Symbole(ISymbA(i))
    SymboleB(i) = Symbole(ISymbB(i))

    IF(LEN_TRIM(CVformel(i)%s) > 0) THEN
      do j=1,len_trim(CVformel(i)%s)
        if(CVformel(i)%s(j:j) == ',') CVformel(i)%s(j:j) = '.'
      end do
      nxx = 1
      call initf(nxx)
      call parsef(1,CVformel(i)%s,SymboleG)
      IF(ifehlP == 1) goto 9000
      res = gevalf(1,Messwert)
      IF(ifehlP == 1) goto 9000
      CovarVal(i) = res
      ncovf = ncovf + 1
      ix = ubound(Rseite,dim=1)
      if(nab+nmodf+nabf+ncovf > ix) call CharModA1(RSeite,nab+nmodf+nabf+ncovf)
      RSeite(nab+nmodf+nabf+ncovf)%s = CVformel(i)%s
    end if
    ! for a special case, if necessary, the following call PrepCovars will be repeated after
    ! the end of the (further down) nn4-loop!
    call PrepCovars(i)
     if(ifehl == 1) goto 9000
     cycle
  END IF

end do

375  continue

IF(FitDecay .AND. klinf > 0 .AND. knumEGr > 1) THEN

  do k=1,3
    nfd = 0
    do j=1,ncov
      if(k == 1 .and. SymboleA(j)%s == 'Fitp1' .and. SymboleB(j)%s == 'Fitp2') then
        nfd = 1
        exit
      end if
      if(k == 2 .and. SymboleA(j)%s == 'Fitp2' .and. SymboleB(j)%s == 'Fitp3') then
        nfd = 1
        exit
      end if
      if(k == 3 .and. SymboleA(j)%s == 'Fitp1' .and. SymboleB(j)%s == 'Fitp3') then
        nfd = 1
        exit
      end if
    end do
    if(nfd == 1) cycle

    ncov = ncov + 1
    if(ncov > ubound(SymboleA,dim=1)) call CharModA1(SymboleA,ncov)
    if(ncov > ubound(SymboleB,dim=1)) call CharModA1(SymboleB,ncov)
    if(ncov > ubound(CVFormel,dim=1)) call CharModA1(CVFormel,ncov)
    if(ncov > ubound(icovtyp,dim=1)) call IntModA1(Icovtyp,ncov)
    select case (k)
      case (1)
        SymboleA(ncov)%s = 'Fitp1'
        SymboleB(ncov)%s = 'Fitp2'
      case (2)
        SymboleA(ncov)%s = 'Fitp2'
        SymboleB(ncov)%s = 'Fitp3'
      case (3)
        SymboleA(ncov)%s = 'Fitp1'
        SymboleB(ncov)%s = 'Fitp3'
    end select
      do i=1,ngrs
        IF(ucase(SymboleA(ncov)%s) == ucase(Symbole(i)%s) ) ISymbA(ncov) = i
        IF(ucase(SymboleB(ncov)%s) == ucase(Symbole(i)%s) ) ISymbB(ncov) = i
      end do
      icovtyp(ncov) = 2
      call WTreeViewPutComboArray('treeview3', 2, ncov, IsymbA)
      call WTreeViewPutComboArray('treeview3', 3, ncov, IsymbB)
      call WTreeViewPutComboArray('treeview3', 4, ncov, icovtyp)
      do i=1,ncov
        call WTreeViewPutDoubleCell('treeview3', 6, i, zero)
      end do
  end do
  do i=1,ncov
    ! kfitp(2) is the row number within the covar table, where cov(Fitp1,Fitp2) can be found
    IF(SymboleA(i)%s == 'Fitp1' .AND. SymboleB(i)%s == 'Fitp2') kfitp(2) = i
  end do
end if
WRITE(66,'(a,4i3)') 'kfitp=',kfitp

    if(ubound(symbole,dim=1) < ngrs+ncov+numd) then
      call ModvarsTV2(ngrs+ncov+numd)
      call CharModA1(SymboleG,ngrs+ncov+numd)
      call CharModA1(Symbole_CP,ngrs+ncov+numd)
      call RealModA1(MesswertSV,ngrs+ncov+numd)
      call RealModA1(StdUncSV,ngrs+ncov+numd)
      call RealModA1(Messwert_CP,ngrs+ncov+numd)
      call RealModA1(StdUnc_CP,ngrs+ncov+numd)
    end if

!-----------------------------------------------------------------------
!  Calculate the standard uncertainties of dependent quantities (1..nab)
!  using uncertainty propagation:

if(.not.loadingPro) then
  IF(langg == 'DE') call WrStatusbar(4,'Rechnet...' )
  IF(langg == 'EN') call WrStatusbar(4,'Calculating...' )
  IF(langg == 'FR') call WrStatusbar(4,'Calcule...' )
end if

i1 = ubound(SymboleG,dim=1)
if(i1 < ngrs+ncov+numd) then
  call CharModA1(SymboleG,ngrs+ncov+numd)
  do i=i1+1,ngrs+ncov+numd
    SymboleG(i)%s = ucase(Symbole(i)%s)
  end do
end if

call initf(nab+nmodf+nabf+ncovf+nfkf)
nhg = nab+nmodf+nabf
    write(66,'(3(a,i0,1x))') 'nhg=',nhg,' nmodf=',nmodf,' nabf=',nabf
do i=1,nab+nmodf+nabf+ncovf+nfkf
     crsG = trim(ucase(Rseite(i)%s))
  ifehlp = 0
  IF(FitDecay .AND. i == klinf) CYCLE
  IF(Gamspk1_Fit .AND. i == kgspk1) CYCLE
  if(FitCalCurve .and. i == kfitcal) cycle
  IF(SumEval_fit .AND. i == ksumeval) CYCLE
  if(index(crsG,'KALFIT') > 0) cycle

  IF(i <= nhg) THEN
    if(i > nab .and. i <= nab+nmodf) then
      i1 = index(Formelt(i)%s,'=')
      RSeite(i)%s = Formelt(i)%s(i1+1:)
      RSeite(i)%s = trim(adjustL(RSeite(i)%s))
    end if
    if(len_trim(Rseite(i)%s) > 0) then
      if(i == klinf) then
        call parsef(i,RSeite(i)%s,SymboleG)
      else
        call parsef(i,RSeite(i)%s,SymboleG)
      end if
      if(ifehlp == 1) goto 9000
    end if
  else
    IF(i > nhg .and. len_trim(CVFormel(i-nhg)%s) > 0) then
      if(len_trim(Rseite(i)%s) > 0) then
          write(66,'(a,i0,a,a)') 'i=',i,' Rseite=',RSeite(i)%s
        call parsef(i,CVFormel(i-nhg)%s,SymboleG)
          if(ifehlp == 1) goto 9000
      end if
    end if
  end if
  if(rw1pro) WRITE(66,'(a,a,a,i0)') 'fparser: parsef of ',Rseite(i)%s,' done: ifehlp=',ifehlp
end do

IF(FitDecay) THEN
  istep = ngrs+ncov+numd - size(Messwert)
  if(istep > 0) then
    call RealModA1(Messwert,ngrs+ncov+numd)
    call RealModA1(MesswertSV,ngrs+ncov+numd)
    call RealModA1(StdUnc,ngrs+ncov+numd)
    call RealModA1(StdUncSV,ngrs+ncov+numd)
  end if
  ! prepare and initialize values and standard uncertainties related to covariances (Fit parameters and others)
  Messwert(ngrs+1:ngrs+ncov)   = missingval
  StdUnc(ngrs+1:ngrs+ncov)     = missingval
  MesswertSV(ngrs+1:ngrs+ncov) = missingval
  StdUncSV(ngrs+1:ngrs+ncov)   = missingval

    ! values and standard uncertainties of the gross count rates of the decay curve:
  Messwert(ngrs+ncov+1:ngrs+ncov+numd)   = dbzrate(1:numd)
  StdUnc(ngrs+ncov+1:ngrs+ncov+numd)     = SQRT(dbzrate(1:numd)/dmesszeit(1:numd))
  MesswertSV(ngrs+ncov+1:ngrs+ncov+numd) = Messwert(ngrs+ncov+1:ngrs+ncov+numd)
  StdUncSV(ngrs+ncov+1:ngrs+ncov+numd)   = StdUnc(ngrs+ncov+1:ngrs+ncov+numd)

  IF(kfitp(1) > 0 .and. knumEGr > 1) THEN
    do i=1,ma
      if(ifit(i) == 3 .and. abs(MEsswert(kfitp(1)-1+i) - zero)>eps1min) then
        k = kfitp(1)-1+i
        MEsswert(k) = zero
        StdUnc(k) = zero
        fpa(i) = zero
        sfpa(i) = zero
        MEsswert_CP(k) = zero
        StdUnc_CP(k) = zero
        fpaSV(i) = zero
        sfpaSV(i) = zero
        IF(abs(Messwert(k)-missingval) > eps1min) call WTreeViewPutDoubleCell('treeview2', 5, k, Messwert(k))  ! frmt!
        IF(abs(StdUnc(k)-missingval) > eps1min)  call WTreeViewPutDoubleCell('treeview2', 11, k, SDWert(k))  ! frmt!
      end if
    end do
  end if

  ! Find out, for which paramaters of the elements of the matrix Amat (with
  ! Messwert(mpfx(nhp)) and StdUnc > 0) an uncertainty propagtion is necessary
    !  WRITE(66,'(a,i4)') 'A   Klincall =',klincall    ! klincall should be still = 1 here
  if(.not.nhp_defined) then
    if(allocated(mpfx_ind)) deallocate(mpfx_ind)
    allocate(mpfx_ind(ngrs)); mpfx_ind = 0
  end if

  WRITE(66,'(a,i3,a,i3)') 'Finding nhp:  kfitp(1)=',kfitp(1),'  k_rbl=',k_rbl
  if(k_rbl > 0) write(66,'(a,i3)') ' kpoint(k_rbl)=',kpoint(k_rbl)
  IF(kfitp(1) > 0 .and. knumEGr >= 1) THEN
    if(.not.nhp_defined) then
          nhp = 0
    do ii=1,nkpmax             !  <--  4.7.2023
      IF(ii == k_tmess) CYCLE
      IF(ii == k_tstart) CYCLE
      IF(ii == k_rbl) CYCLE
      nst = 3
      IF(knumEGr == 1) nst = 1
      do k=1,ngrs
        if(k >= kfitp(1) .and. k < kfitp(1)+nst) cycle
        IF(kpoint(ii) == k) THEN
          IF(StdUnc(k) <= eps1min) CYCLE            ! corresponds to zero or missingval
          nhp = nhp + 1
             call IntModA1(mpfx,nhp)
             call LogModA1(mpfx_extern,nhp)
          mpfx(nhp) = k
          mpfx_ind(k) = nhp
          do k2=1,nab
            if(k2 >= kfitp(1) .and. k2 < kfitp(1)+nst) cycle
            if(k2 == klinf) cycle
            do k1=1,nRSsy(k2)
              IF(kpoint(ii) == RS_SymbolNr(k2,k1) ) THEN
                mpfx_extern(nhp) = .true.
                nfd = 0
                do m1=1,nsymb_kEGr
                  if(symb_kEGr(m1)%s == SymboleG(k2)%s) then
                    nfd = 1
                    exit
                  end if
                end do
                if(nfd == 0) mpfx_extern(nhp) = .false.
                if(nfd == 1) write(66,'(a,a,a,a,a,i0)') 'mpfx-Parameter ',SymboleG(kpoint(ii))%s, &
                           '  found in : ',SymboleG(k2)%s,'  kpoint(k1)=',kpoint(k1)
                EXIT
              end if
            end do
          end do
          EXIT
        end if
      end do

    end do
    nhp_defined = .true.
    end if    ! nhp_defined

        klincall = 0
    IF(nhp > 0) THEN
      do ii=1,nhp
           mpi = mpfx(ii)
        if(abs(StdUnc(mpi) - missingval) < eps1min .or. abs(StdUnc(mpi)) < eps1min ) then
          ! if missing, calculate the standard uncertainties of the mpfx-parameters
          if(mpi <= nab .and. mpi > klinf) then
              write(66,'(a,i0,a,L1)') '%%%%%%%%   call upropa(mpi):  mpi=',mpi,' dep_unc_done=',dep_unc_done
            call upropa(mpi)
            StdUnc(mpi) = Ucomb
          end if
        end if
        WRITE(66,*) 'mpfx: ',Symbole(mpfx(ii))%s,'  Wert=',sngl(Messwert(mpfx(ii))),  &
                    '  StdUnc=',sngl(StdUnc(mpfx(ii))),'  mpfx_extern=',mpfx_extern(ii)
      end do
      WRITE(66,'(a,i0)') 'mpfx: nhp=',nhp
    else
      WRITE(66,*) 'mpfx: nhp=0'
    end if
    write(66,'(a,50i4)') 'mpfx=',(mpfx(ii),ii=1,nhp)
    write(66,'(a,50(L1,1x))') 'mpfx_extern=',(mpfx_extern(ii),ii=1,nhp)

    if(allocated(cov_fixed)) deallocate(cov_fixed)
    allocate(cov_fixed(numd,numd)); cov_fixed = zero;
    if(allocated(mpfxfixed)) deallocate(mpfxfixed)
    allocate(mpfxfixed(nhp)); mpfxfixed = 0

    parfixed = .false.
    cov_fixed = zero
    kuse_fixed = 2
    if(kuse_fixed == 1) kusetext = 'set u(RSr85) and cov_fixed = 0'
    if(kuse_fixed == 2) kusetext = 'use full u(RSr85) and cov_fixed'
    if(allocated(wp)) deallocate(wp)
    allocate(wp(ndatmax,3))
    wp = one

    do jp=1,ma
      if(ifit(jp) == 2 .and. kPMLE == 0) then
        parfixed = .true.
            call gtk_widget_set_sensitive(idpt('radiobuttonPMLE'), 0_c_int)
        ! fixed fit parameter: contribution to the covariance of count rates of the decay curve;
        ! let the main diagonal be zero!
        WRITE(66,*) 'RW1:   kuse_fixed=',kuse_fixed,'  ',trim(kusetext)

        if(nkovzr == 0) cycle
        do i=1,numd      !  i-th count rate
          do k=i+1,numd       !  k-th count rate
            if(i == k) cycle
            do j=1,nhp       ! mpfx-parameters of the argument list parameters

              if(kuse_fixed == 1) cycle

              if(abs(StdUnc(mpfx(j))-missingval) < eps1min .or. abs(StdUnc(mpfx(j))) < eps1min) cycle
              ! partial derivatives:  d(afunc(jp)) / d(Messwert(mpfx(j))
              dpi1 = dpi_funcs(mpfx(j),i,jp,ma,missingval)
              dpi2 = dpi_funcs(mpfx(j),k,jp,ma,missingval)

              cov_fixed(i,k) = cov_fixed(i,k) + dpi1*dpi2 * StdUnc(mpfx(j))**two
              if(i == 2 .and. k == 1) write(66,*) 'Cov_fixed: Beitrag von ',symbole(mpfx(j))%s,' = ', &
                                                  sngl(dpi1*dpi2 * StdUnc(mpfx(j))**two)
            end do
            cov_fixed(k,i) = cov_fixed(i,k)
          end do
        end do
      end if
    end do

    if(parfixed) then
      call matwrite(cov_fixed,numd,numd,66,'(30es10.2)','Matrix cov_fixed:')
    end if

    call covppcalc(1)
  end if     ! kfitp(1) > 0

  ! if(allocated(covpp)) call matwrite(covpp,nhp,nhp,nhp,nhp,66,'(1x,130es11.3)','RW1:  Matrix covpp:')

  if(.false. .and. FitDecay) then
    WRITE(66,*) 'RW1: covp: SQRT(diagonal elements)/p_value:'
    do j=1,nhp
      dummy = zero
      if(covpp(j,j) > eps1min .and. abs(Messwert(mpfx(j))) > eps1min) dummy = &
                                             sqrt(covpp(j,j))/Messwert(mpfx(j))
      WRITE(66,*) sngl(dummy)
    end do
  end if

  IF(FitDecay .AND. knumEGr < 3) THEN
    ! check whether radiobuttonPMLE should be disabled
    ifb = 0
    do i=ma,2,-1
      if(ifit(i) == 1) then
        ifb = i
        exit
      end if
    end do
    if(ifb > 0) then
      call funcs(1,afunc)
      af1 = (afunc(ifb))
      call funcs(numd,afunc)
      af2 = (afunc(ifb))
      if(abs(af1-af2)/af1 < 1.E-4_rn) then
        call gtk_widget_set_sensitive(idpt('radiobuttonPMLE'), 0_c_int)
      end if
    end if
  end if

  mfit2 = 0
  do i=1,3
    if(ifit(i) == 1) mfit2 = mfit2 + 1
  end do
  IF(kfitp(1) > 0 .AND. knumEGr == 1) singlenuk = .TRUE.

  ifitSV = ifit
  knt = 0
  IF(kPMLE == 1) THEN
    ! preparation for PMLE:
    IF(singlenuk) THEN
      knt = max(knumEGr, mfit2)
      if(knt < 3) then
        if(ifit(knt+1) >= 2 )  then
          ! omit:
          ifit(knt+1) = 2
          mfrbg = knt+1
        end if
      end if
    elseIF(knumEGr < 3) THEN
      ifit(knumEGr+1) = 2
      mfrbg = knumEGr + 1
    end if
  else
    mfrbg = 0
  end if
  WRITE(66,'(a,i0,a,3(i0,1x),a,i0,a,i0)') ' mfrbg=',mfrbg,'  ifit=',ifit,'   mfit2=',mfit2,'  knt=',knt

  iterat_passed = .FALSE.
  IF(export_r) THEN
      export_case = .false.
  end if

  d0zrateSV(1:numd) = d0zrate(1:numd)
  nwh = numd/nchannels
  d0zsum = sum(d0zrate(1:nwh))/real(nwh,rn)
  d0zsumq = zero
  d0zsumq = sum( (d0zrate(1:nwh) - d0zsum)**two )
  d0zsumq = d0zsumq/real(nwh,rn)

  konstant_r0 = .FALSE.
  if(abs(one - d0zsum*real(nwh,rn)/(d0zrate(1)*real(nwh,rn))) < 1.E-6_rn) konstant_r0 = .true.

  WRITE(66,'(3(a,es12.5),a,L1)') 'RW1:  d0zsum*nwh=',real(d0zsum*real(nwh,8),8),  &
              '  d0zrate(1)*nwh=',real(d0zrate(1)*real(nwh,8),8),  &
                        ' d0zsumq=',real(d0zsumq,8),'  konstant_r0=',konstant_r0
  R0k = zero
  if(konstant_r0) then
    R0k(1) = d0zrate(1)
    sdR0k(1) = sd0zrate(1)
      ! write(66,*) 'R0k(1)=',sngl(R0k(1)),'  sdR0k(1)=',sngl(sdR0k(1))
    if(nchannels > 1) then
      R0k(2) = d0zrate(numd/nchannels+1)
      sdR0k(2) = sd0zrate(numd/nchannels+1)
       ! write(66,*) 'R0k(2)=',sngl(R0k(2)),'  sdR0k(2)=',sngl(sdR0k(2))
    end if
    if(nchannels > 2) then
      R0k(3) = d0zrate(numd/nchannels*2+1)
      sdR0k(3) = sd0zrate(numd/nchannels*2+1)
        ! write(66,*) 'R0k(3)=',sngl(R0k(3)),'  sdR0k(3)=',sngl(sdR0k(3))
    end if
  end if
                klincall = 0

  if(.true.) then
    call Linfausf(1,rn0,SDrn0)
    IF(ifehl == 1) goto 9000
      write(66,*) 'RW1: after LinfAusf:  covarval           : ',(sngl(covarval(j)), j=1,3)
      write(66,*) 'RW1: after LinfAusf:  stdunc(fp1-fp3)    : ',(sngl(StdUnc(kfitp(1)-1+j)), j=1,3)
      write(66,*) 'RW1: after LinfAusf:  stdunc_CP(fp1-fp3) : ',(sngl(StdUnc_CP(kfitp(1)-1+j)), j=1,3)
      write(66,*) 'RW1: after LinfAusf:  stduncSV(fp1-fp3)  : ',(sngl(StdUncSV(kfitp(1)-1+j)), j=1,3)
      write(66,*) 'RW1: after LinfAusf:  sfpa(1-3)          : ',(sngl(sfpa(j)), j=1,3)
      write(66,*) 'RW1: after LinfAusf:  corrVal(1-3)       : ',(sngl(corrVal(j)), j=1,ncov)
  end if

  do i=klinf-1,1,-1
    IF(i <= knumEGr .AND. i /= kEGr) CYCLE
    Messwert(i) = gevalf(i,Messwert)
    MesswertSV(i) = Messwert(i)
    IF(abs(Messwert(i)-missingval) > eps1min) call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert(i))
    IF(abs(SDwert(i)-missingval) > eps1min)  call WTreeViewPutDoubleCell('treeview2', 8, i, SDWert(i))
    IF(abs(StdUnc(i)-missingval) > eps1min)  call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
  end do

  do i=1,ncov
    if(abs(corrVal(i)) > eps1min .and. abs(covarVal(i)-missingval) > eps1min) then
      if(Stdunc(IsymbA(i)) > missingval .and. Stdunc(IsymbB(i)) > missingval) then
        CovarVal(i) = CorrVal(i)*StdUnc(ISymbA(i))*StdUnc(ISymbB(i))
      end if
    end if
    write(66,'(a,i3,3(a,es15.8))') 'i=',i,'  covarval(i)=',covarval(i),'  u(syma)=',StdUnc(ISymbA(i)), &
                                                          '  u(symb)=',StdUnc(ISymbB(i))
    Messwert(ngrs+i) = covarval(i)
    MesswertSV(ngrs+i) = covarval(i)
  end do

END if    ! FitDecay

IF(Gamspk1_Fit) THEN
  ! Loop over the different gamms peaks of the radionuclide:
  call GamPeakvals()

  ! Find out, for which paramaters of the elements of the matrix-based solution (with
  ! Messwert(mpfx(nhp)) and StdUnc > 0) an uncertainty propagtion is necessary
  !   WRITE(66,*) 'Klincall =',klincall    ! klincall should still be =1 here
  nhp = 0
  do ii=1,nRSsy(kgspk1)
    ii2 = RS_SymbolNr(kgspk1,ii)
    IF(ii2 == k_tmess) CYCLE
    IF(ii2 == k_tstart) CYCLE
    do k=kgspk1+1,ngrs
      if(Ubound(kpoint,dim=1) < ii2) call IntModA1(kpoint,ii2)
      IF(kpoint(ii2) == k) THEN
        IF(abs(StdUnc(k)) < eps1min .OR. abs(StdUnc(i)-missingval) < eps1min) CYCLE
        nhp = nhp + 1
        if(Ubound(mpfx,dim=1) < nhp) call IntModA1(mpfx,nhp)
        mpfx(nhp) = k
        EXIT
      end if
    end do
  end do
     write(66,'(a,i0,a,50(i0,1x))') 'mpfx-Parameter:  nhp=',nhp,'  mpfx(:)=',mpfx(1:nhp)

  call Linfg1ausf(1,akt,SDakt)    !    without output of protocoll
  IF(ifehl == 1) goto 9000

  do i=kgspk1-1,kEGr,-1
    IF(i <= knumEGr .AND. i /= kEGr) CYCLE
    Messwert(i) = gevalf(i,Messwert)
    MesswertSV(i) = Messwert(i)
    IF(abs(Messwert(i)-missingval) > eps1min) call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert(i))
    IF(abs(SDwert(i)-missingval) > eps1min)  call WTreeViewPutDoubleCell('treeview2', 8, i, SDWert(i))
    IF(abs(StdUnc(i)-missingval) > eps1min)  call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
  end do

END IF

if(FitCalCurve .and. KFitcal > 0) then
  if(nkalpts == 0) then
    call LinCalib()
  end if
   call CalibInter(KFmode, Messwert(kpointKB(1)), StdUnc(kpointKB(1)), yval, uyval)
  i = kfitcal
  Messwert(i) = yval
  StdUnc(i) = uyval
    if(allocated(MesswertSV)) deallocate(MesswertSV); allocate(MesswertSV, source=Messwert)
    if(allocated(StdUncSV)) deallocate(StdUncSV);  allocate(StdUncSV,source=StdUnc)
  MesswertSV(i) = yval
  StdUncSV(i) = uyval
  call WTreeViewPutDoubleCell('treeview2', 5, i, Messwert(i))
  call WTreeViewPutDoubleCell('treeview2', 11, i, StdUnc(i))
end if

!do i=1,ngrs+ncov+numd
!! do i=kEGr,ngrs+ncov+numd
!  WRITE(66,*) 'RW1: ',Symbole(i),' Messwert=',sngl(Messwert(i)),'  StdUnc=',sngl(StdUnc(i))
!end do

MesswertSV(1:ngrs+ncov+numd) = Messwert(1:ngrs+ncov+numd)
if(ubound(StdUncSV,dim=1) == 0) then
  allocate(StdUncSV,source=StdUnc)
  StdUncSV = 0._rn
end if
if(allocated(afuncSV)) deallocate(afuncsv);   allocate(afuncsv(numd,ma))

IF(FitDecay) THEN
  do i=1,numd
    ! prepare afuncSV:
    call Funcs(i,afunc)
    afuncSV(i,1:ma) = afunc(1:ma)
    ! write(66,'(a,i3,a,3es18.8)') 'RW1: i=',i,'  afuncSV(1-3)=',(sngl(afuncsv(i,k)),k=1,3)
  end do
end if

if(rw1pro) then
  WRITE(66,*) ' before calculating the uncertainties of dependent quantities:'
  WRITE(66,'(a,3(i0,1x))') ' ngrs, ncov, numd: ',ngrs,ncov,numd
  do i=1,ngrs+ncov+numd
    WRITE(66,*) i,' ',Symbole(i)%s,' Messwert=',sngl(Messwert(i)),'  StdUnc=',sngl(StdUnc(i))
  end do
end if
IF(FitDecay) WRITE(66,'(a,i4)') '   klincall=',klincall
              !  write(66,*) 'vor nn4-loop: Ucomb=',sngl(Ucomb)

do nn4=nab,1,-1
    ! WRITE(66,*) 'RW1: nn4=',nn4,'  nab=',nab
  IF(nn4 <= knumEGr .AND. nn4 /= kEGr) CYCLE

  Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)
  StdUnc(1:ngrs+ncov+numd)   = StdUncSV(1:ngrs+ncov+numd)
  StdUnc(nn4) = zero
     if(consoleout_gtk)  write(0,*) 'RW1: before call uprpoa(nn4);   nn4=',int(nn4,2), &
                       ' kbrutto(kEGr)=',int(kbrutto(kEGr),2),' ',Symbole(nn4)%s

  if(nn4 == kbrutto(kEGr) .and. .not.SumEval_fit) then
    if(len_trim(SDFormel(nn4)%s) > 0) then
      IF(IAR(nn4) == 1) StdUnc(nn4) = SDWert(nn4)
      IF(IAR(nn4) == 2) StdUnc(nn4) = SDWert(nn4)*Messwert(nn4)
      IF(IVTL(nn4) == 7) then
        xp = Messwert(ip_binom)
        xn0 = Messwert(kbgv_binom)*Messwert(itm_binom)
        StdUnc(nn4) = sqrt(Messwert(nn4)*(one-xp) + xn0*(xp - one) + &
                           (StdUnc(kbgv_binom)*Messwert(itm_binom))**two )
      end if
    elseif(var_brutto_auto) then
      if(nn4 /= k_autoform) then
        StdUnc(nn4) = StdUnc(k_autoform) / Messwert(iptr_time(nn4))
      else
        k_datvar = MDpointrev(k_autoform)
        fBay_g = fBayMD(k_datvar)**two

        nn = findlocT(DistPars%symb,Symbole(k_autoform)%s)      ! kbrutto(kEGr)
        xg0 = DistPars%pval(nn,2)          ! gross quantity at measurement
        xnueg = DistPars%pval(nn,1)
        varg0 = DistPars%pval(nn,3)**two   ! associated with the gross quantity
            write(66,*) 'fBay_g=',sngl(fBay_g),' nn=',int(nn,2),' xg0=',sngl(xg0),' xnueg=',sngl(xnueg), &
                          'varg0 without faktor=',sngl(varg0)
        if(abs(fBay_g -one)> eps1min) varg0 = varg0 * xnueg/(xnueg-two)
        StdUnc(nn4) = zero
        if(varg0 > eps1min) StdUnc(nn4) = sqrt(varg0)
      end if
    end if
  else
    if(nn4 /= ksumeval) then     ! ksumeval: see a bit further down (sumEvalFit)
      call upropa(nn4)
      StdUnc(nn4) = Ucomb
          write(66,*) 'nn4=',int(nn4,2),' ucomb=',sngl(Ucomb),' use_WTLS=',use_WTLS
    end if
  end if

  IF(ifehl == 1) goto 9000
  !// IF(FitDecay) WRITE(66,'(a,i3,a,i4)') 'RW1:  nn4-loop: nn4=',nn4,' nach UPropa:   klincall=',klincall

  !if(nn4 /= klinf) write(66,'(a,i3,a,a,3(a,es15.8))') 'RW1:  nn4=',nn4,' ', Symbole(nn4)%s,'  StdUnc(nn4)=',real(StdUnc(nn4),8),  &
  !                   '   MW(nn4)=',real(Messwert(nn4),8), ' Ucomb=',real(Ucomb,8)

  IF(FitDecay .AND. nn4 == klinf) THEN
      dep_unc_done = .true.
      call covppcalc(1)
      call Linfausf(2,rn0,SDrn0)
    IF(ifehl == 1) goto 9000
    if(.not.loadingPro) then
      IF(langg == 'DE') call WrStatusBar(4,'Rechnet...' )
      IF(langg == 'EN') call WrStatusbar(4,'Calculating...' )
      IF(langg == 'FR') call WrStatusbar(4,'Calcule...' )
    end if

    WRITE(66,'(a,es15.8,3(a,i3),a,L1)') 'RW1: upropa(klinf)=',Ucomb,'  klinf=',klinf,'  nn4=',nn4,'  nab=',nab
    StdUnc(nn4) = SDrn0
    Messwert(nn4) = rn0
    Messwert(kEGr) = gevalf(kEGr,Messwert)
    MesswertSV(kEGr) = Messwert(kEGr)
    StdUncSV(nn4) = SDrn0

    IF(abs(Messwert(nn4)-missingval)>eps1min) call WTreeViewPutDoubleCell('treeview2', 5, nn4, Messwert(nn4))
    IF(abs(StdUnc(nn4)-missingval)>eps1min) call WTreeViewPutDoubleCell('treeview2', 11, nn4, StdUnc(nn4))
    WRITE(66,*) ' from Linfit: rn0, SDrn0: ',sngl(rn0),sngl(SDrn0),  '  Ucomb=',sngl(Ucomb)
  END IF
  IF(Gamspk1_fit .AND. nn4 == kgspk1) THEN
    call Linfg1ausf(2,akt,SDakt)
    IF(ifehl == 1) goto 9000
    if(.not.loadingPro) then
      IF(langg == 'DE') call WrStatusBar(4,'Rechnet...' )
      IF(langg == 'EN') call WrStatusBar(4,'Calculating...' )
      IF(langg == 'FR') call WrStatusBar(4,'Calcule...' )
    end if
    WRITE(66,*) 'upropa(kgspk1)=',real(Ucomb,8)
    StdUnc(nn4) = SDakt
    Messwert(nn4) = akt
    IF(kgspk1 > 1) Messwert(kEGr) = gevalf(kEGr,Messwert)
    MesswertSV(kEGr) = Messwert(kEGr)
    StdUncSV(nn4) = SDakt
    IF(abs(Messwert(nn4)-missingval)>eps1min) call WTreeViewPutDoubleCell('treeview2', 5, nn4, Messwert(nn4))
    IF(abs(StdUnc(nn4)-missingval)>eps1min) call WTreeViewPutDoubleCell('treeview2', 11, nn4, StdUnc(nn4))
    WRITE(66,*) ' from Linfgf1: ',real(SDakt,8)
  END IF

  IF(SumEval_fit .AND. nn4 == ksumeval) THEN
    call SumEvalCalc(akt,SDakt)
                 write(66,*) 'sumevalCalc: akt,SDakt=',sngl(akt),sngl(SDakt)
    IF(ifehl == 1) goto 9000
    if(.not.loadingPro) then
      IF(langg == 'DE') call WrStatusBar(4,'Rechnet...' )
      IF(langg == 'EN') call WrStatusBar(4,'Calculating...' )
      IF(langg == 'FR') call WrStatusBar(4,'Calcule...' )
    end if
    WRITE(66,*) 'upropa(ksumeval)=',sngl(SDakt)
    StdUnc(nn4) = SDakt
    Messwert(nn4) = akt
    IF(ksumeval > 1) Messwert(kEGr) = gevalf(kEGr,Messwert)
    MesswertSV(kEGr) = Messwert(kEGr)
    StdUncSV(nn4) = SDakt
    IF(abs(Messwert(nn4)-missingval)>eps1min) call WTreeViewPutDoubleCell('treeview2', 5, nn4, Messwert(nn4))  ! frmt!
    IF(abs(StdUnc(nn4)-missingval)>eps1min) call WTreeViewPutDoubleCell('treeview2', 11, nn4, StdUnc(nn4))  ! frmt!
    WRITE(66,*) ' from SumEvalCalc: ',real(SDakt,8)
  END IF

  !if(nn4 /= ksumeval) then
  !  call upropa(nn4)
  !  StdUnc(nn4) = Ucomb
  !   !       ! write(66,*) 'nn4=',int(nn4,2),' ucomb=',sngl(Ucomb)
  !end if

  call WDListstoreClearCell('treeview2', 11, nn4)

  IF(abs(StdUnc(nn4)-missingval) <= 1.E+8*eps1min) StdUnc(nn4) = zero
  call WTreeViewPutDoubleCell('treeview2', 11, nn4, StdUnc(nn4))

  StdUncSv(nn4)   = StdUnc(nn4)

  if(Rw1pro) write(66,'(a,i3,a,a,3(a,es15.8))') 'RW1:  nn4=',nn4,' ', Symbole(nn4)%s,'  StdUnc(nn4)=',real(StdUnc(nn4),8),  &
                     '   MW(nn4)=',real(Messwert(nn4),8), ' Ucomb=',real(Ucomb,8)

     !WRITE(66,*) 'nn4=',nn4,' ', Symbole(nn4)%s,' MesswertSV=',sngl(MesswertSV(nn4)),'  StdUnc=',sngl(StdUnc(nn4)), &
     !           '  covarval(1): ',sngl(covarval(1))

end do   ! End nn4 loop

! two subsequent corrections:
if(.not.Fitdecay .and. .not.Gamspk1_fit .and. .not.FitCalCurve .and. .not.SumEval_fit) then
  nvar = kbrutto(kEGr)
  if(nvar > 0 .and. nvar <= nab) then
    ! if stdunc(kbrutto) would be zero, and if it were calculated without SDformel.
    ! see Upropa code, near to its end!
    use_sdf_brutto = .false.
    j = 0
    do i=1,nRssy(nvar)
      k = RS_SymbolNr(nvar,i)
      if(StdUnc(k) > zero) j = j + 1
    end do
    if(j == 0) use_sdf_brutto =.true.
  end if
end if

if(uval_used) then
  do nn4=nab,1,-1
    IF(nn4 <= knumEGr .AND. nn4 /= kEGr) CYCLE
    MesswertSV(nn4) = gevalf(nn4,MesswertSV)
    Messwert(nn4) = MesswertSV(nn4)
  end do
end if

if(ncov > 0) then
  ! added 20.11.2023:
  do i=1,ncov
    if(len_trim(CVFormel(i)%s) > 0) then
      if(index(ucase(CVFormel(i)%s),'UVAL(') == 0 .and. &
         index(ucase(CVFormel(i)%s),'UVAL (') == 0 ) cycle
      covarval(i) = gevalf(nab+nmodf+nabf+i, Messwert)
      call PrepCovars(i)
      if(ifehl == 1) goto 9000
    endif
  enddo
end if


! make the list SymboleG longer (to ngrs+ncov+numd):

call CharModA1(SymboleG,ngrs+ncov+numd)
Messwert(1:ngrs+ncov+numd) = MesswertSV(1:ngrs+ncov+numd)     ! ganz wichtig!
StdUnc(1:ngrs+ncov+numd)   = StdUncSV(1:ngrs+ncov+numd)

do i=1,ngrs+ncov+numd
  SymboleG(i)%s = ucase(Symbole(i)%s)
    if(Rw1pro) WRITE(66,*) 'i=',i,symbole(i)%s,'  Messwert=',sngl(Messwert(i)),' StdUnc=',  &
                          sngl(StdUnc(i)),'  StdUncSV=',sngl(StdUncSV(i))
END do

if(.not. Gum_restricted) then
  klu = knetto(kEGr)
  if(FitDecay) then
    klu = klinf
    if(kfitp(1) > 0) klu = kfitp(1)+kEGr-1
  elseif(Gamspk1_Fit) then
    klu = kgspk1
  elseif(FitCalCurve) then
    klu = kfitcal
  end if

  nWPars = 0
  call FindWParsR(kEGr,klu)
  !write(66,*) 'nxwpars=',nxwpars
  !write(66,*) 'recursive Wpars: ',(xwpars(i),' ',i=1,nxwpars)
  if(nWpars > 0 .and. ubound(WparsInd,dim=1) > 0) then
    write(66,'(a,30(i0,2x))') 'Calib. factor w : contains parameters: ',(WParsInd(k),k=1,nWpars)
    write(66,'(a,30(a,2x))') 'Calib. factor w : contains parameters: ',(Symbole(WParsInd(k))%s,k=1,nWpars)
      ! write(66,*) 'Net count rate contains variables: ',(trim(Symbole(RnetParsInd(k))),' ',k=1,nRnetp)
  end if

end if

 do i=1,ngrs
   call WTreeViewPutDoubleCell('treeview2',5,i,Messwert(i))
   call WTreeViewPutDoubleCell('treeview2',11,i,StdUnc(i))
 end do

!-----------------------------------------------------------------------
WRITE(66,'(a,4i4,a,i3)') '********* nab, nmodf, nabf, ncovf=',nab,nmodf,nabf,ncovf,'  ncov=',ncov
WRITE(66,'(a,3i4,a,es15.8)') 'klinf, kgspk1,kfitcal=',klinf,kgspk1,kfitcal,'   UcombLinf=',ucomblinf
do i=1,nab+nmodf+nabf+ncovf
  if(Rw1pro) WRITE(66,'(a,i0,a,a)') 'i=',i,'  RS=',Rseite(i)%s
end do
IF(FitDecay) WRITE(66,'(a,i4)') ' klincall=',klincall

9000  continue

if(allocated(RSeiteG)) deallocate(RseiteG)
if(allocated(sdfg)) deallocate(sdfg)
if(allocated(sdfsave)) deallocate(sdfsave)
!-----------------------------------------------------------------------

WRITE(66,*) '########## End of Rechw1'
if(consoleout_gtk) WRITE(0,*) '##### End of Rechw1'

end subroutine Rechw1

!#######################################################################

module subroutine covppcalc(mode)

   ! this routine takes the number ncov of known covariances (covarval())
   ! and creates from them the covariance matrix of the mpfx-parameters;
   ! Dependent on the routine calling covppcalc, this matrix has
   ! different names. The matrix is tested whether it fulfills the
   ! Cauchy-Schwartz-inequality; if this test failed, a correspondent
   ! message is written to file fort23.txt. This would indicate that the
   ! matrix is not positive definite, but this does not matter here,
   ! because the matrix generated here will not be inverted.
   !
   !     Copyright (C) 2014-2023  Günter Kanisch

use UR_Gleich,  only: Messwert,StdUnc,covarval,dep_unc_done,nab,ncov,SymboleG, &
                      SymboleA,SymboleB,Symbole
use UR_Linft,   only: mpfx, mpfx_extern,covpp,nhp,cauchy_failed1,mpfxfixed
USE fparser,    ONLY: initf, parsef, evalf, EvalErrMsg
use UR_Mcc,     only: covpmc
use UR_params,  only: rn,eps1min,zero,one,two
use CHF,        only: ucase
use Num1,       only: matwrite

implicit none

integer(4),intent(in)   :: mode        ! 1: called from Rechw1 or Rechw2;
                                       ! 2: called from MCcalc;
                                       ! 3: Test, on
integer(4)        :: k,j,jj,i,modec
character(len=60) :: chh1,chh2
real(rn)          :: relvarAtr(4),diffcorr
real(rn)          :: xtiny
!----------------------------------------------------------------------------------
modec = mode

           !!   modec = 3         ! this line for testing

relvarATr = zero
if(nhp > 0) then
  ! Variances and covariances of the mpfx-parameters:
  if(modec == 1 .or. modec == 3) then
    if(allocated(covpp)) deallocate(covpp); allocate(covpp(nhp,nhp)); covpp = zero
  end if
  if(modec == 2) then
    if(allocated(covpmc)) deallocate(covpmc); allocate(covpmc(nhp,nhp)); covpmc = zero
  end if
      ! if(nhp == 1) write(66,*) 'dep_unc_done=',dep_unc_done,'  modec=',modec,'  mpfxfixed(1)=',mpfxfixed(1)

  do k=1,nhp
    if(modec == 1 .and. mpfx_extern(k)) cycle
    if(modec <= 2 .and. symboleG(mpfx(k))%s == 'RBL') cycle
    if(modec == 3 .and. mpfx_extern(k)) cycle
    if(mpfxfixed(k) == 1) cycle
             ! if(nhp == 1) write(66,*) 'k=',k,'  stdunc(mpfx(k))=',sngl(stdunc(mpfx(k)))
    if(modec == 1) covpp(k,k) = StdUnc(mpfx(k))**two
    if(modec == 2) covpmc(k,k) = StdUnc(mpfx(k))**two
    if(modec == 3) covpp(k,k) = StdUnc(mpfx(k))**two

    if(.not.dep_unc_done .and. mpfx(k) <= nab) then
      ! If LINF is called by Rechw1 before the uncertainties of all dependent
      ! quantities,required in LINF, are calcualated:
      if((modec == 1 .or. modec == 3) .and. covpp(k,k) <= zero) covpp(k,k) = (0.01_rn*Messwert(mpfx(k)))**two
      if(modec == 2 .and. covpmc(k,k) <= zero) covpmc(k,k) = (0.01_rn*Messwert(mpfx(k)))**two
    end if

    if(modec == 1 .or. modec == 3) then
      if(covpp(k,k) <= zero) covpp(k,k) = 1.E-28_rn
    end if
    if(modec == 2) then
      if(covpmc(k,k) <= zero) covpmc(k,k) = 1.E-28_rn
    end if
       if(modec == 3) then
         ! For testing only, see the begin of this routine
         do i=4,ncov
           if(k == 1 .and. Symbole(mpfx(1))%s ==  SymboleA(i)%s .and. Symbole(mpfx(2))%s &
                                                     ==  SymboleB(i)%s ) then
             relvarAtr(1) = covarval(i)/Messwert(mpfx(1))/Messwert(mpfx(2))
             exit
           end if
           if(k == 4 .and. Symbole(mpfx(4))%s ==  SymboleA(i)%s .and. Symbole(mpfx(5))%s == &
                                                         SymboleB(i)%s ) then
             relvarAtr(2) = covarval(i)/Messwert(mpfx(4))/Messwert(mpfx(5))
             exit
           end if
           if(k == 7 .and. Symbole(mpfx(7))%s ==  SymboleA(i)%s .and. Symbole(mpfx(8))%s == &
                                                         SymboleB(i)%s ) then
             relvarAtr(3) = covarval(i)/Messwert(mpfx(7))/Messwert(mpfx(8))
             exit
           end if
           if(k == 10 .and. Symbole(mpfx(10))%s ==  SymboleA(i)%s .and. Symbole(mpfx(11))%s == &
                                                          SymboleB(i)%s ) then
             relvarAtr(4) = covarval(i)/Messwert(mpfx(10))/Messwert(mpfx(11))
             exit
           end if
         end do
         if(k <= 3) covpp(k,k) = covpp(k,k) - relvarAtr(1)*Messwert(mpfx(k))**two
         if(k > 3 .and. k <= 6) covpp(k,k) = covpp(k,k) - relvarAtr(2)*Messwert(mpfx(k))**two
         if(k > 6 .and. k <= 9) covpp(k,k) = covpp(k,k) - relvarAtr(3)*Messwert(mpfx(k))**two
         if(k > 9 .and. k <= 12) covpp(k,k) = covpp(k,k) - relvarAtr(4)*Messwert(mpfx(k))**two
       end if
    if(k < nhp) then
      ! Consider covariances between the mpfx-parameters
      do jj=1,ncov
        if(abs(covarval(jj)) < eps1min) cycle
        chh1 = ucase(symboleA(jj)%s)
        if(symboleG(mpfx(k))%s == trim(chh1)) then
          do j=k+1,nhp
            chh2 = ucase(symboleB(jj)%s)
            if(symboleG(mpfx(j))%s == trim(chh2)) then

              if(modec == 1) then
                covpp(k,j) = covarval(jj)
                covpp(j,k) = covpp(k,j)
              else if(modec == 2) then
                covpmc(k,j) = covarval(jj)
                covpmc(j,k) = covpmc(k,j)
              else if(modec == 3) then
                 covpp(k,j) = zero
                 covpp(j,k) = zero
              end if
            end if
          end do
        end if
      end do
    end if
  end do
end if
if(modec == 3) write(66,*) 'Rel. uncertainty of tracer activities: ',(sngl(sqrt(relvarAtr(i))),i=1,4)

! Check the matrix for the Cauchy-Schwarz inequality:
if(modec == 2)   return

xtiny = zero
do k=1,nhp-1
  do j=k+1,nhp
    IF(modec == 1) then
      if(abs(covpp(k,j)) > eps1min .and. abs(covpp(k,j))**two > covpp(j,j)*covpp(k,k)-xtiny) then
        cauchy_failed1 = .true.
        covpp(k,j) = ( covpp(k,j)/abs(covpp(k,j)) ) *  sqrt(covpp(j,j)*covpp(k,k)-xtiny) * (one - 1.E-08_rn)
        covpp(j,k) = covpp(k,j)
        diffcorr = abs(covpp(k,j))**two - covpp(j,j)*covpp(k,k)
        WRITE(23,*) 'CovppCalc: mode=',mode,'  Cauchy-Schwarz-Inequality was invalid for covpp, for k,j= ',k,j,  &
                    '  DiffCorr = ',sngl(diffcorr),' cov=',sngl(abs(covpp(k,j))), &
                    ' var_k=',sngl(covpp(k,k)),' var_j=',sngl(covpp(j,j))
      end if
    end if
    IF(modec == 2) then
      if(abs(covpmc(k,j))>eps1min .and. abs(covpmc(k,j))**two > covpmc(j,j)*covpmc(k,k)-xtiny) then
        cauchy_failed1 = .true.
        covpmc(k,j) = ( covpmc(k,j)/abs(covpmc(k,j)) ) * sqrt(covpmc(j,j)*covpmc(k,k)-xtiny) * (one - 1.E-08_rn)
        covpmc(j,k) = covpmc(k,j)
        diffcorr = abs(covpmc(k,j))**two - covpmc(j,j)*covpmc(k,k)
        WRITE(23,*) 'CovppCalc: mode=',mode,'  Cauchy-Schwarz-Inequality was invalid for covpmc, for k,j= ',k,j,  &
                    '  DiffCorr = ',sngl(diffcorr),' cov=',sngl(abs(covpmc(k,j))), &
                    ' var_k=',sngl(covpmc(k,k)),' var_j=',sngl(covpp(j,j))
      end if
    end if
  end do
end do

 ! call matwrite(covpp,nhp,nhp,nhp,nhp,66,'(1x,130es11.3)','covppcalc: Matrix covpp:')

end subroutine covppcalc

!#######################################################################

module subroutine LinCalib()

   ! this routine invokes the dialog for linear calibration, reads then
   ! the data from the dialog and executes then the calibration
   ! calculations by calling Xkalfit.
   !
   !     Copyright (C) 2014-2023  Günter Kanisch

use, intrinsic :: iso_c_binding,    only: c_null_ptr,c_ptr
use UR_Linft
use UR_Loadsel
use UR_Gleich,        only: loadingpro
use UR_gtk_variables, only: dialogstr,ioption
use Rout,             only: WDGetCheckButton
use top,              only: FindItemS
use LDN,              only: Loadsel_diag_new
use KLF,              only: XKalfit

implicit none

integer(4)     :: ncitem

if(.not. loadingpro) then
  ioption = 8
  dialogstr = 'dialog_kalfit'
  call FindItemS(trim(dialogstr), ncitem)
     write(66,'(a,i0)') 'Kalfit:  ncitem=',ncitem
  call Loadsel_diag_new(1, ncitem)
end if
call Xkalfit()

end subroutine LinCalib

!#############################################################################################

!--------------------------------------------------
module real(rn) function uval(symb)

     ! this is a function which returns the value StdUnc(i) of the
     ! standard uncertainty of the variable with the name
     ! in the character variable symb; i is the index of symb in the
     ! symbol list. If symb is not found (i=0), uval is set = 0.

     !     Copyright (C) 2014-2023  Günter Kanisch

use UR_params,     only: rn,zero
use UR_Gleich,     only: SymboleG,StdUnc
use CHF,           only: FindlocT,ucase

implicit none

character(len=*),intent(in)  :: symb

integer(4)    :: i
character(len=20)   :: symbg

uval = zero
symbg = ucase(symb)
i = findlocT(SymboleG,trim(symbg))
if(i > 0) uval = StdUnc(i)

end function uval
!--------------------------------------------------

!########################################################################

module recursive subroutine FindWparsR(kstart,klu)

   ! this recursive routine, starting with the right-hand-side formula of
   ! equation kstart, searches for symbols in that formula and forms
   ! lists of them (number nwPars, arrays WParsInd, WPars), where, however,
   ! the symbols of the right-hand-side of equation klu, knetto and kbrutto,
   ! which contribute to the net or gross count rate, are excluded. This
   ! means that the nWpars symbols found by this routine are just the
   ! variables forming the calibration factor.

   !     Copyright (C) 2014-2023  Günter Kanisch

use UR_Gleich,   only: kEGr,nab,nRSsy,RS_SymbolNr, &
                       knetto,kbrutto,symtyp,nWpars,WParsInd,WPars, &
                       Messwert
use Top,         only: IntModA1,RealModA1

implicit none

integer(4),intent(in)     :: kstart       ! index of start equation
integer(4),intent(in)     :: klu          ! definition klu: see (begin of) upropa

integer             :: i,k1,j

if(nWpars >= 100) return

if(kstart > nab) return

if(nRSsy(kstart) > 0) then
      !write(66,*) 'FS: kstart=',int(kstart,2),' ksearch=',int(ksearch,2),' : ', &
      !      (trim(RSSy(nRSsyanf(kstart)+i-1)%s),' ',i=1,nRSsy(kstart))
      !write(66,*) '  RS_Symbolnr=',(RS_SymbolNr(kstart,i),' ',i=1,nRSsy(kstart))
  do i=1,nRSsy(kstart)
    k1 = 0
    k1 = RS_SymbolNr(kstart,i)
    if(k1 == knetto(kEGr)) cycle
    if(k1 == kbrutto(kEGr)) cycle
    if(k1 == klu) cycle
    if(symtyp(k1)%s == 'a') then
      call FindWParsR(k1,klu)
    elseif(symtyp(k1)%s == 'u') then
      if(nWpars == 0) then
        nWpars = 1
        if(allocated(WParsInd)) deallocate(WParsInd)
        if(allocated(WPars)) deallocate(WPars)
        allocate(WparsInd(1))
        allocate(WPars(1))
        WParsInd(1) = k1
        WPars(1) = Messwert(k1)
      else
        j = findloc(WParsind,k1,dim=1)
        if(j == 0) then
          nWpars = nWpars + 1
          call IntModA1(WParsInd,nWPars)
          call RealModA1(WPars,nWPars)
          WParsInd(nWpars) = k1
          if(nWPars >= 50) goto 300
        end if
      end if
      ! write(0,*) 'k1=',k1,' ',Symbole(k1)%s
    end if
  end do
end if
300 continue

end subroutine FindWparsR

!########################################################################

module subroutine PrepCovars(i)
use UR_params,     only: rn,zero,eps1min
use UR_VARIABLES,  only: langg
use UR_Gleich,     only: SymboleG,StdUnc,icovtyp,CovarVal,missingval,IsymbA,IsymbB, &
                         CVFormel,ifehl,Symbole,CorrVal
use gtk,           only: GTK_BUTTONS_OK,GTK_MESSAGE_WARNING
use Rout,          only: MessageShow,WTreeViewPutComboCell,WTreeViewPutDoubleCell, &
                         WTreeViewGetDoubleCell
use, intrinsic :: iso_c_binding,    only: c_int
implicit none

integer(4),intent(in)        :: i

real(rn)          :: CCV
integer(c_int)    :: resp
character(:),allocatable :: str1

allocate(character(len=800) :: str1)

    IF(icovtyp(i) == 2 .AND. abs(CorrVal(i)) > eps1min .AND. abs(CovarVal(i)-missingval)>eps1min) THEN
      if(Stdunc(IsymbA(i)) > eps1min .and. Stdunc(IsymbB(i)) > eps1min) then
        ! Are the StdUnc values already known here? Yes, see above, the loop over SDfomel
        CovarVal(i) = CorrVal(i)*StdUnc(ISymbA(i))*StdUnc(ISymbB(i))
      end if
    END IF
    ! IF(abs(CovarVal(i)-missingval) > eps1min) then
    IF(abs(CovarVal(i)-missingval) > eps1min .and. LEN_TRIM(CVformel(i)%s) > 0) then  ! 14.9.2023
      if(icovtyp(i) == 1) then
        call WTreeViewPutDoubleCell('treeview3', 6, i, CovarVal(i))   ! format frmtt)
      elseif(icovtyp(1) == 2) then
        call WTreeViewPutDoubleCell('treeview3', 6, i, CorrVal(i))   ! format frmtt)
      end if
      call WTreeViewPutComboCell('treeview3', 2, i, IsymbA(i))
      call WTreeViewPutComboCell('treeview3', 3, i, IsymbB(i))
    end if
    IF(abs(CovarVal(i)-missingval) > eps1min) then
      ! 14.9.2023: test for abs(correlation) > 1 :
      call WTreeViewGetDoubleCell('treeview3', 6, i, CCV)
      if(abs(CCV-missingval) > eps1min) then
        if(Stdunc(IsymbA(i)) > eps1min .and. Stdunc(IsymbB(i)) > eps1min) then
          if(icovtyp(i) == 1) CCV = CCV / (StdUnc(ISymbA(i))*StdUnc(ISymbB(i)))
          if(abs(CCV) > 1.0_rn + 2.0E-6_rn) then
            IF(langg == 'DE') str1 = 'Achtung: Die berechnete Korrelation zwischen ' // Symbole(IsymbA(i))%s // &
                                     ' und ' // Symbole(IsymbB(i))%s // ' ist > 1!'
            IF(langg == 'EN') str1 = 'Warning: The calculated correlation between ' // Symbole(IsymbA(i))%s // &
                                     ' and ' // Symbole(IsymbB(i))%s // ' is > 1!'
            IF(langg == 'FR') str1 = 'Attention: La corrélation calculée entre ' // Symbole(IsymbA(i))%s // &
                                     ' et ' // Symbole(IsymbB(i))%s // ' est > 1 !'
            call MessageShow(trim(str1), GTK_BUTTONS_OK, "Rechw1:", resp,mtype=GTK_MESSAGE_WARNING)
            ifehl = 1
            return
          end if
        end if
      end if
    end if

end subroutine PrepCovars


end submodule Rw1A