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
module ur_general_globals

    use, intrinsic :: iso_c_binding

    use UR_types, only: rn
    use UR_gtk_window,  only: charv
    !
    !   Shared variables for any routine with 'use ur_general_variables'
    !
    implicit none

    logical                  :: fd_found(100)
    character(355)           :: fname           ! Current filename
    logical                  :: Savef = .false. ! File needs saving
    logical                  :: Savep = .false. ! project File needs saving
    logical                  :: saveas = .false.      ! 6.10.2015
    character(:),allocatable :: filter
    character(1)             :: FileTyp         ! 'F'=File; 'P'=Projekt
    integer                  :: FirstEditField
    integer                  :: icoltab,irowtab
    integer                  :: icoltabv,irowtabv
    character(30)            :: actual_grid = ''         ! am 14.7.2020
    integer                  :: top_selrow,bottom_selrow
    character(:),allocatable :: txtfile
    character(20)            :: frmt,frmtres,frmtg,frmtc            ! Format for double dialog fields
    character(20)            :: frmt_min1,frmtres_min1,frmtg_min1   ! Format for double dialog fields, for numbers< 0.1
    logical                  :: MCsim_on            ! MC simulation running?
    logical                  :: MCsim_localOff        ! MC simulation running? is considered in DChain_adjust_SD and Resulta
                                                      ! 18.1.2025  GK
    logical                  :: png_to_cairo_surface  ! 16.5.2025  GK   ! replaced print_graph
    character(len=256)       :: pngfile               ! 16.5.2025  GK
    logical                  :: cairo_png_reloaded    ! 20.5.2025  GK
    logical                  :: project_loadw       !T:  automatic loading; F: stepwise loading the project
    character(:),allocatable :: fname_getarg        ! Filename in argument of "Open UR with.."
    logical                  :: batest_on           ! is Batch test running?         !
    logical                  :: BATF, batf_mc,batf_mcmc,batf_reports            ! batestMC,batestMCMC
    logical                  :: automode

    character(:),allocatable :: Sample_Id
    character(:),allocatable :: fname_autoreport

    character(:),allocatable :: work_path             ! working/home directory of the UncertRadio binary
    character(:),allocatable :: actpath               ! should contain the current directory.
    character(:),allocatable :: help_path             ! = 'UR2_CHM/'
    character(:),allocatable :: log_path              ! = 'log/'
    character(:),allocatable :: results_path          ! = 'results/'
    character(:),allocatable :: example_path          ! = 'pros/'

    logical                  :: wpunix                 ! True if the work path is unix-like, thus running on a unix system
    logical                  :: autoreport
    character(1)             :: sListSeparator         ! e.g.:     ;
    character(1)             :: dir_sep                ! 16.04.2023   '\' on windows and '/' on unix machines
    character(1)             :: sDecimalPoint          ! e.g.: . or ,

    character(:),allocatable :: fnameMCB              ! filename for Batch_MC
    logical                  :: proStartNew
    character(:),allocatable :: EditorFileName
    character(:),allocatable :: EditorFileUcheck
    character(:),allocatable :: fname_grout
    character(:),allocatable :: project_path
    character(:),allocatable :: sWindowsVersion

    logical                  :: Gum_restricted         ! only GUM calculations, no detetction limits
    logical                  :: plot_ellipse
    logical                  :: multi_eval             ! = T for plot confidence ellipsoid
    logical                  :: ableit_fitp            ! refers the derivative to a fit parameter?
    logical                  :: plot_confidoid         ! confidence ellipsoid
    character(10)            :: actual_plot
    character(:),allocatable :: sFontName
    integer                  :: sfontsize              !  fontsize (in pts)#: 10 or 12

    logical                  :: is_plend,is_plinit
    logical                  :: Confidoid_activated

    type(c_ptr)              :: clipd                  ! clipboard C-pointer
    logical                  :: gross_negative         ! =T, if negative-linear model type selected
    integer                  :: kModelType,kModelOld
    character(:),allocatable :: cModelType(:)
    character(:),allocatable :: UR_git_hash, UR_version_tag

    character(2)             :: Excel_langg             !
    character(1)             :: Excel_sListSeparator    !
    character(1)             :: Excel_sDecimalPoint     !
    character(:),allocatable :: error_text

    character(300)           :: Batest_ref_file,Batest_out,batf_file
    character(300)           :: serial_csvinput
    character(300)           :: Batest_ref_file_ch,Batest_out_ch   ! actually chosen files
    logical                  :: bat_serial,bat_mc,bat_mcmc
    logical                  :: batest_user
    character(300)           :: base_project_SE    ! basic project file used for serial evaluation
    integer                  :: kfrom_SE,kto_SE    !
    integer                  :: kcmxMC,kcmxMCMC,kcrunMC,kcrunMCMC

    character(:),allocatable :: chh1, chh2
    integer                  :: kbd
    real(rn)                 :: Messwert_kbruttoSV,mwert1,mwert2,fv1back
    logical                  :: RW1_on=.false., RW2_on=.false.
    logical                  :: ResultA_on      ! 27.4.2025

    type(charv),allocatable  :: cgetarg(:)                    ! commandline arguments

    integer                  :: gtk_strm=0, sec_strm=1        ! PLPLOT stream-numbers
    character(len=60)        :: filtname                      ! used in WSelectFile
    logical                  :: progstart_on

    logical                  :: bxiter_on = .false.           ! brentx iteration on?

    ! use in subroutine Setsimul_rout:
    logical                  :: simul_ProSetup, done_simul_ProSetup
    character(150)           :: fileToSimulate
    logical                  :: open_project_parts,modSymb,copyEQ,FDecM,GspkDt,covTB
    logical                  :: FcalDT,MDDT,setDP

    logical                  :: modvar_on      ! are calculations in Modvar running?
    integer                  :: kfi            ! kfi file #, used in Mccalc and in Batch_proc
    integer                  :: linebat        ! line number of a batch file

    logical                  :: runauto
    logical                  :: runbatser
    logical                  :: MC_root_from_line    ! with root estimate from line fit  24.10.2025 GK

end module ur_general_globals

!#######################################################################


module UR_Gleich_globals
    use UR_types,       only: rn
    use UR_gtk_window,  only: charv

    implicit none

    logical                  :: TAB_GLEICHG_Grid
    logical                  :: TAB_GLEICHG_2Rates
    logical                  :: TAB_VALUNC_Grid

    type(charv),allocatable  :: Titeltext(:)

    integer   , parameter    :: nformax = 80  ! 70  ! 80    ! max. number of formulae
    integer   , parameter    :: nabmx  = 60   ! 80          ! max. number of dependet symbols
    integer   , parameter    :: nmumx  = 200  ! 200         ! max. number of independet symbols
    integer   , parameter    :: ncovmx = 50

    type(charv),allocatable  :: Formeltext(:)        ! String arry of equations
    type(charv),allocatable  :: FormeltextFit(:)     ! string array of formulae in a decay curve model

    type(charv),allocatable  :: Formelt(:)  ! string array of the individual equations
    type(charv),allocatable  :: RSeite(:)   ! the right-hand sides of the equations/formulae
    type(charv),allocatable  :: LSeite(:)   ! the left-hand sides of the equations/formulae

    type(charv),allocatable  :: RSeite_zero(:),RSeite_one(:)   ! modified right-hand sides of equations of the output quantities

    integer                  :: nglp              ! number of the model equations
    integer                  :: nglf              ! number of the equations in the decay fit model
    integer                  :: nglt              ! number of equations within Formelt()
    integer                  :: nab               ! number of dependent symbols
    integer                  :: nmu               ! number of measured independent symbols
    integer                  :: nabf              ! number of standard uncertainty formulae
    integer                  :: nmodf             ! number of formulae in the decayfit model
    integer                  :: nglsv             ! number of equations, without those in Formeltextfit
    type(charv),allocatable  :: varab(:)          ! auxiliary array of dependent Symbol (strings)
    type(charv),allocatable  :: varmu(:)          ! auxiliary array of independent Symbols (strings)
    CHARACTER(LEN=1)         :: ops(8)            ! list of numerical operator symbols
    integer                  :: nopsfd(nabmx)     ! number of num. operators in an equation

    type(charv),allocatable  :: symbole(:)       ! character array of symbol names
    type(charv),allocatable  :: symbole_CP(:)    ! a copy of symbole(:)

    integer                  :: nsyn
    type(charv),allocatable  :: symb_n(:)        ! new symbols found after modification of equations
    type(charv),allocatable  :: symboleG(:)      ! array of ucase(Symbole)
    type(charv),allocatable  :: symtyp(:)        ! array of symbol type
    type(charv),allocatable  :: symtyp_CP(:)     ! copy of symtyp
    type(charv),allocatable  :: einheit(:)       ! array of units
    type(charv),allocatable  :: einheitSV(:)     ! copy of einheit
    type(charv),allocatable  :: einheit_CP(:)    ! another copy of einheit
    type(charv),allocatable  :: einheit_conv(:)  ! einheit after a unit conversion
    type(charv),allocatable  :: bedeutung(:)     ! meaning of a symbol
    type(charv),allocatable  :: bedeutung_CP(:)  ! and its copy

    !-----------------------------------------------------------------------
    !  April 2020: the former matrix RS_Symbole was replaced by a 1-dim array:
    !    RS_Symbole(i,j)  -->  RSSy(nrsum) with nrsum = nRssyanf(i) + nRssy(j) - 1
    !    nsymbRS(i,j)     -->  nRssyanf(i) + nRssy(j) - 1


    ! List of symbols, ocurring in righ-hand sides of equations, always uppercase
    type(charv),allocatable  :: Rssy(:)        ! array of rhs symbols, aggregated for all equations
    integer   ,allocatable   :: nRssy(:)       ! number of symbols of the equation i (first index)
    integer   ,allocatable   :: nRssyanf(:)    ! position count numbers
                                               ! see subr. Symbol1 where they are generated

    integer   ,allocatable   :: RS_SymbolNr(:,:)   ! List of rhs symbol numbers (2nd index), occurring
                                                   !      in an equation (first index)
    logical,allocatable      :: RS_SymbUse(:,:)    ! can the symbol numbers be used? Depends on trigger variables

    character(len=1), allocatable :: RS_ops(:,:)     ! List of operators in the rhs formulae
    integer, allocatable   :: RS_opsPos(:,:)  ! List of position numbers of the operators(2nd index)
    !     in the string (1st index)

    type(charv),allocatable  :: SymboleA(:)                ! left symbol array used for covariances
    type(charv),allocatable  :: SymboleB(:)                ! right symbol array used for covariances
    type(charv),allocatable  :: CVFormel(:)                ! array for covariance formulae
    type(charv),allocatable  :: CVFormel_CP(:)             !   and a copy
    real(rn),allocatable     :: CovarVal(:)                ! array of covariance values
    real(rn),allocatable     :: CovarVal_CP(:)             !   and a copy
    real(rn),allocatable     :: CovarvalSV(:)              !   and another copy
    real(rn),allocatable     :: CorrVal(:)                 ! array of correlation values
    real(rn),allocatable     :: dpi1v(:)                   ! array of first partial derivatives
    real(rn),allocatable     :: dpi2v(:)                   ! array of first partial derivatives
    integer   ,allocatable   :: icovtyp(:)                 ! array: 1: covariance; 2: correlation
    integer   ,allocatable   :: icovtyp_CP(:)              !   and a copy
    integer   ,allocatable   :: ISymbA(:)                  ! array of left-side symbols of covariances
    integer   ,allocatable   :: ISymbA_CP(:)               !   and a copy
    integer   ,allocatable   :: ISymbB(:)                  ! array of righ-side symbols of covariances
    integer   ,allocatable   :: ISymbB_CP(:)               !   and a copy

    integer                  :: ncov                         ! number of covariance pairs
    integer                  :: ncovf                        ! number of covariance formulae
    integer                  :: nfkf                         ! number of parameters (0 or 1?) declared
    !   by FitKal call
    integer                  :: knumEGr                      ! actual number of output quantities (<=3)
    integer                  :: knumold                      ! previous number of output quantities
    integer                  :: kEGr, kEGr_old               ! number - and old number - of the active output quantity
    integer                  :: kEGrSV                       !   and a copy

    integer                  :: ngrs              ! number of symbols (sum of dependent and independent symbols)
    integer                  :: ngrs_CP           !    and a copy
    integer                  :: ngrsP             !    and another copy for sel. buttons (also: ngrs+numd+ncov))
    integer                  :: ngrs_init          ! number of symbols (sum of dependent and independent symbols)

    integer                  :: knetto(3)         ! index numbers of the net count rate symbols associated with kEGr
    integer                  :: knetto_CP(3)      !   and a copy
    integer                  :: kbrutto(3)        ! index numbers of the gross count rate symbols associated with kEGr
    integer                  :: kbrutto_CP(3)     !   and a copy
    type(charv),allocatable  :: knetto_name(:)    ! array of symbol names of the net count rates
    integer                  :: kbrutto_gl(3)     ! number of the gross count rate equation in the list of equations
    type(charv),allocatable  :: kbrutto_name(:)   ! array of symbol names of the gross count rates
    integer                  :: kbrutto2
    integer                  :: kbrutto2_kbd
    integer                  :: klinf             ! index number of the symbol declared by a Linfit call
    integer                  :: klinf_CP          !    and a copy
    integer   ,allocatable   :: kpoint(:)         ! array of symbol numbers of the arguments of the Linfit call
    integer                  :: knullef           ! number of the background count standard uncertainty formula
    integer                  :: kgspk1            ! index nummer of the symbol declared by a Gamspk1 call
    integer                  :: kgspk1_CP         !    and a copy
    integer                  :: kfitcal           ! index nummer of the symbol declared by a FitCal call
    integer                  :: ksumeval          ! index nummer of the symbol declared by a SumEval call
    integer                  :: nparts,npartsold  ! number of compartments used in SumEval
    integer                  :: modeSEval         ! for SumEval:   1: as mean value; 2: as sum value
    character(len=20)        :: avar(20)          ! names of activity variables being arguments of SumEval function
    integer                  :: iavar(20)         ! associated indexes in the symbol list
    real(rn)                 :: mfactSE(20)       ! further factor in eq. iavar(.), besides phi_i and Rn_i

    integer                  :: nux,symb_nux(50)  ! refers to subroutine SumEval1
    real(rn)                 :: faliq(20)         !

    logical                  :: use_dependent_sdwert  ! see subroutine Uncwb
    integer   ,allocatable   :: kpointKB(:)    ! array of symbol index numbers of the arguments of the FitCal call

    real(rn),allocatable     :: Messwert(:)    ! array of the symbols/variables measurement values
    real(rn),allocatable     :: MesswertSV(:)  !     and a copy -Saved
    real(rn),allocatable     :: Messwert_CP(:) !     and another copy
    integer   ,allocatable   :: ivtl(:)        ! array of distribution types of the symbols/variables
    integer   ,allocatable   :: ivtl_CP(:)     !     and a copy
    type(charv),allocatable  :: SDFormel(:)    ! array of explicitly given standard uncertainty formulae
    type(charv),allocatable  :: SDFormel_CP(:) !     and a copy
    real(rn),allocatable     :: SDWert(:)      ! array of values of the explicitly given standard uncertainty
    real(rn),allocatable     :: SDWert_CP(:)   !    and a copy
    real(rn),allocatable     :: HBreite(:)     ! array of values of explicitly given distribution half-width values
    real(rn),allocatable     :: HBreite_CP(:)  !    and a copy
    integer   ,allocatable   :: IAR(:)         ! mode of explixitly given std. uncertainty: abs or relative?
    integer   ,allocatable   :: IAR_CP(:)      !    and a copy
    real(rn),allocatable     :: STDUnc(:)      ! array of final absolute standard uncertainties
    real(rn),allocatable     :: STDUncSV(:)    !    and a copy
    real(rn),allocatable     :: STDUncSV1(:)   !    and another copy
    real(rn),allocatable     :: STDUnc_CP(:)   !    and another copy
    real(rn),allocatable     :: sensi(:)       ! array of sensitivity coefficients (partial derivatives))
    real(rn),allocatable     :: sensiSV(:)     !    and a copy
    real(rn),allocatable     :: sensi_CP(:)    !    and another copy
    real(rn),allocatable     :: perc(:)        ! array of relative fractions of the total variance of the outpuit qunatity
    real(rn),allocatable     :: percSV(:)      !    and a copy
    real(rn),allocatable     :: perc_CP(:)     !    and another copy
    real(rn)                 :: MesswertSV_klu !
    real(rn)                 :: MesswertSV_klinf !
    real(rn),allocatable     :: dpnni(:,:)     ! derivative of variable  nn (<=nab) by variable i (< nab) 10.1.2025



    real(rn)                 :: resultat        ! value calculated for the active output quantity
    real(rn)                 :: Ucomb,Ucomb_anf ! its associated absolute standard uncertainty value
    real(rn)                 :: percsum         ! sum of perc()
    real(rn)                 :: Ucomb_EGr       ! standard uncertainty of the output quantity  ! 20.1.2025 GK
    real(rn)                 :: Ucomb_DTv       ! standard uncertainty in the decision threshold case
    real(rn)                 :: Ucomb_DLv       ! standard uncertainty in the decision detection limit case
    real(rn),allocatable     :: Ucontrib(:)     ! array of uncertainty contributions to the std. uncertainty of active output quantity
    real(rn),allocatable     :: UcontribSV(:)   !     and a copy
    integer                  :: Ucontyp         ! type of contribution: 1: relat. contrib.(%); 2: as standard uncertainty

    integer                  :: nvar            ! index of gross count rate symbol
    integer                  :: ifehl           ! error number
    integer                  :: kableitnum      ! Number of parameter with respect to which the derivative is build
    CHARACTER(len=150)       :: ifehl_string    ! Hint about cell error or other errors

    logical                  :: loadingPro      ! is loading a txp project active?
    real(rn)                 :: coverf          ! Coverage factor for output quantity uncertainty
    real(rn)                 :: coverin         ! Coverage factor for input quantity uncertainties

    integer   ,allocatable   :: iptr_time(:)    ! Gamma distribution: points the counting duration symbol
    !   associated with a number of counts or with a count rate
    integer   ,allocatable   :: iptr_cnt(:)     ! Gamma distribution: popints to count rate symbol
    !   associated with a number of counts
    integer   ,allocatable   :: iptr_rate(:)    ! points to the counting rate symbol belonging a number of counts
    logical,allocatable      :: is_count(:)     ! =1, if the symbol represents a number of counts, otherwise = 0
    LOGICAL                  :: Rnetmodi        ! used for an "extra" modification of the net count rate symbol
    LOGICAL                  :: upropa_on       ! .true., if uncert. propagation is active, otherwise .false.
    integer                  :: kbrutto_double  ! number of a symbol, representing the same as the gross count rate symbol

    integer                  :: nsyd            ! auxiliary variable (Symbol1)
    integer                  :: nsydanf         ! auxiliary variable (Symbol1)
    logical                  :: use_sdf_brutto  !  "use standard deviation formula" for kbrutto

    real(rn),allocatable     :: MesswertSVG(:),StdUncSVG(:), CovarvalSVG(:)  !  copies of arrays

    real(rn), parameter      :: missingval = -999._rn

    logical                  :: syntax_check
    character(:),allocatable :: dialogfield_chg

    integer   , parameter    :: ndopt = 11            ! number of distribution types  (see translateUR)
    type(charv),allocatable  :: vdopt(:)              ! short names of the distribution types
    type(charv),allocatable  :: vcovcor(:)            ! 'covariance' or 'correlation'
    type(charv),allocatable  :: vdoptfull(:)          ! full names of the distribution types
    type(charv),allocatable  :: vdopt_pname(:,:)      ! names of extra parameters of two special distribution types
    type(charv),allocatable  :: absrel(:)             ! array containing names 'abs' or 'rel''
    integer                  :: refresh_type          ! 0: for loading a project;  1: after change of the active output quantity
    ! 2: after changes in the lin. fit model
    logical                  :: refresh_but           ! button for refreshing the calculation clicked or not?
    type(charv),allocatable  :: GrFormat(:)           ! names of graphic formats
    logical                  :: uncval_exists
    logical                  :: symlist_modified      ! has the symbol list been modified?
    logical                  :: symlist_shorter       ! has the symbol list be shortened?
    logical                  :: linmod1_on            ! has the dialog for Linfit, the Fitdecay model, be opened?
    logical                  :: linfit_rename         ! is only true, if names of key arguments of Linfit (Rbl, tstart,tmess)
    ! have been modified (incorrectly)
    logical                  :: gamspk_rename         ! the same for argument names for Gamspk1
    character(:),allocatable :: linfit_eqold          ! previous versions linfit_eq, tmess, tstart
    character(:),allocatable :: tmess_old,tstart_old  !

    real(rn)                 :: tgross, tback         ! counting duration variables for gross countrate and background countrate
    real(rn)                 :: urelw                 ! relative stdd. uncertainty of the calibration factor w
    logical                  :: uFc_calc              ! has the uncertainty of Fconst be derived; actually de-activated

    integer                  :: FP_numeq              ! number of equations (fparserW)
    type(charv),allocatable  :: FP_equat(:)           ! array of equations (fparserW)

    ! for the following: see subroutine MDcalc
    integer                  :: k_datvar              ! counts the datasets to be averaged, indicated by symtyp="m"
    integer                  :: nvarsMD               ! number of variables, for which datasets for averaging are supplied
    integer   ,allocatable   :: MDpoint(:)            ! MDpoint(k_datvar) = i = symbol index
    integer   ,allocatable   :: MDpointrev(:)         ! MDpointrev(i) = k_datvar
    integer   ,allocatable   :: nvalsMD(:)            ! number of values in the selected dataset
    real(rn),allocatable     :: nvMD(:)               ! = nvalsMD as real(rn)
    integer   ,allocatable   :: k_MDtyp(:)            ! type of mean:
    !    1: not being counts (bayes.)
    !    2: counts, with influence (bayes.)
    !    3: classical (non-bayes.), only <= 3 values exist'
    type(charv),allocatable  :: MDtyp(:)              ! contains explaining strings, the argument is k_MDtype()
    type(charv),allocatable  :: MDtyp_x(:)            ! contains explaining strings
    real(rn),allocatable     :: fbayMD(:)             ! factor: k_MDtyp=1: (n-1)/(n-3)/n;
    !         k_MDtyp=2: 1/n; k_MDtyp=3: 1
    integer   ,allocatable   :: ixdanf(:)             !  position of k-th dataset in that array, obtained by
    !  concatination of all datasets
    real(rn),allocatable     :: meanMD(:),umeanMD(:)  ! arrays of means and their stdd. uncertainties derived
    !   from arrays of values
    real(rn),allocatable     :: smeanMD(:)            ! classical std. deviation of n values: s0 = sd(data)
    real(rn),allocatable     :: xdataMD(:)            ! values of datasets, all being aggregated into one array
    integer                  :: refdataMD             ! index number of reference data set (in 0..nvarsMD)
    type(charv),allocatable  :: meanID(:),meanID_x(:) ! names (symbol_data) of the datasets associated with symbol
    logical,allocatable      :: MDused(:)             ! mean processing is used or not
    logical                  :: rinflu_known          ! if true, requires defining a reference data set
    real(rn)                 :: theta_ref             ! the theta value of the reference data set

    logical                  :: nonPoissGrossCounts   ! true, if background has Poisson counts, the net effect binomial counts

    integer                  :: nsymb_kEGr            ! number of sysmbols in the equation for output quantity kEGr
    integer                  :: knetp3                ! Index of the interfering count rate (2 or 3)
    type(charv),allocatable  :: symb_kEGr(:)         ! aray of symbols corresponding to nsymb_kEGr
    logical                  :: dep_unc_done          ! use ind RW1 and Lsqlincov2

    ! variables used in analyzing the topological chains spanned by the net cout rate symbols:
    ! look for such tables in fort66.txt:
    ! nc   kvor   kk     ukenn akenn kcnt ktime krate rule Symbol         chain
    ! 1     2     9     1     0     0    14     9     B2  Rb                 2  9  true nRnetp=  1
    ! 2     7    12     1     0     0    14    12     A4  R0                 2  7 12  true nRnetp=  2
    ! 4     5     9     1     0     0    14     9     A4  Rb                 2  4  5  9  true nRnetp=  3
    ! 6     6    12     1     0     0    14    12     B1  R0                 2  4  2  6 12  true nRnetp=  4
    ! 8     2     3     0     0     0     0     3     A6  RAN                2  4  2  6  2  3  true nRnetp=  6
    !Sy iptr_time iptr_cnt iptr_rate   Symbol
    ! 9     14      0      0           Rb
    !12     14      0      0           R0
    ! 3      0      0      0           RAN

    integer   ,allocatable   :: RnetParsInd(:)        ! yields symbol index numbers of symbols in the net count rate equation
    logical,allocatable      :: RnetParsCRate(:)      ! represents a symbol in the net count rate equation a count rate?
    integer                  :: nRnetp                ! number of symbols in the net count rate equation
    integer                  :: nchmax                ! number of topological chains starting from the net count rate
    integer                  :: nRnetpCRn             ! number of count rates in the list of those parameters
    !    contained in the net count rate formula
    real(rn),allocatable     :: RnetPars(:)           ! values of the symbols contained in the net count rate equation
    type(charv),allocatable  :: RnetParsCRule(:)      ! strings with the rules for finding the chains

    logical                  :: increase_dpafact
    logical                  :: LinTest               ! is never set True (see Rechw2 for its purpose)
    real(rn)                 :: UEG_normal,UEG_increase,DT_increase,DL_increase   ! variables used with LinTest
    logical                  :: grossfail

    integer                  :: nWpars                ! number of symbols contained in the expression for calib factor w
    integer   ,allocatable   :: WParsInd(:)           ! index numbers, in the symbol list, of those symbols in w
    real(rn),allocatable     :: WPars(:)              ! values of those sysmbols in w

    integer                  :: kbgv_binom            ! index of that background count rate associated with the
    !     binomial/poisson gross count rate
    integer                  :: itm_binom             ! index of the counting time symbol associated with the gross
    !     count rate
    integer                  :: ip_binom              ! index of the symbol correspi\F3nding to the binomial distrib. parameter p
    integer                  :: bipoi_gl(3)           !
    integer                  :: ilam_binom            ! index of the decay constant symbol associated with the decay of
    !     the nuclide behind the gross count rate
    logical                  :: use_bipoi             ! use - or not - the binomial/Poisson distribution
    real(rn)                 :: Nbin0_MV,bipoi2_sumv, bipoi2_hgt,bipoi2_maxk  ! parameter values used in context with
    !    the binomial/poisson distribution

    real(rn)                 :: var_rbtot             ! variance of the total/integrated background count rate
    integer                  :: nvars_in_rbtot        ! number of symbols in the formula for the total background
    integer   ,allocatable   :: vars_rbtot(:)         ! values of these symbols

    type :: DistribPars
        integer                   :: ivtl(10)       ! distribution types
        type(charv),allocatable   :: symb(:)        ! Symbol names
        real(rn)                  :: pval(10,4)     ! for each distrib. type up to 4 parameters
    end type

    type(DistribPars)        :: DistPars

    integer                  :: nmxDist                ! number of distributions
    logical                  :: use_DP                 ! use distpars or not
    logical                  :: defined_RSY,uval_used  ! defined RS symbols? Is the function uval used?

    logical                  :: N_preset               ! measurement: are the counts the preset paremeter,
    ! instead of the counting duration?
    ! for the following: see subroutine TopoSort:
    integer                  :: ndep
    integer   ,allocatable   :: eqnum(:), synum(:), opnum(:), kmulrun(:),ukenn(:),akenn(:),kcnt(:)
    integer   ,allocatable   :: ktime(:),krate(:),eqndep(:),syndep(:)
    character(len=51)        :: seqch(40), seqchc(30)
    real(4)                  :: cpu_topo

    ! for the following: see subroutine calcUnits:
    logical                  :: apply_units = .false.
    logical                  :: apply_units_dir = .false.
    logical                  :: applyunitsSV
    logical                  :: FP_for_units
    real(rn),allocatable     :: unit_conv_fact(:),uconv(:),unit_conv_factSV(:)        !   uconvfact_ab(:)
    integer   , parameter    :: nbasis = 20
    character(len=30)        :: unit_other(10),Unit_basis(10)
    integer                  :: nu_other
    logical                  :: retain_triggers

    type :: URunits
        integer                    :: nSymb
        integer   ,allocatable     :: nSymbCsd(:)
        integer   ,allocatable     :: nSymbSyn(:)

        type(charv),allocatable    :: EinhSymb(:)
        real(rn),allocatable       :: EinhVal(:)
        type(charv),allocatable    :: EinhSymbScd(:,:)
        real(rn),allocatable       :: EinhScdFact(:,:)
        type(charv),allocatable    :: EinhSymbSynon(:,:)
    end type URunits

    type(URunits)           :: UU         ! the data in this structure are rea by call ReadUnits

    ! a further set of saved arrays:
    real(rn)                 :: ResultatSVUCH,UcombSVUCH,decthreshSVUCH,detlimSVUCH, &
                                KBgrenzuSVUCH,KBgrenzoSVUCH,KBgrenzuSHSVUCH,KBgrenzoSHSVUCH
    type(charv),allocatable  :: EinheitSVUCH(:)
    real(rn),allocatable     :: MesswertSVUCH(:),SDWertSVUCH(:),HBreiteSVUCH(:),StdUncSVUCH(:)
    type(charv),allocatable  :: SymboleX(:),symtypX(:),SDformelX(:)
    real(rn),allocatable     :: MesswertX(:),StdUncX(:),SDwertX(:),HBreiteX(:)
    integer   ,allocatable   :: IARX(:),IVTLX(:)
    type(charv),allocatable  :: PUnitMsg(:)        !  error messages
    integer                  :: npMsg              ! number of mnessages

    integer                  :: maxlen_symb
    integer                  :: nglp_read, eqnumber(100)
    logical                  :: Formeltext_out,eqnum_val(200)
    character(len=50)        :: formelstatus
    ! three added 27.4.2025:
    character(len=200)       :: str_dummy
    integer                  :: nmax_parsed_eqs
    integer                  :: arr_indep(40),nndep
    integer                  :: nvaldiff               ! 27.7.2025

    ! 30.10.2025
    integer                  :: nvneg     ! number of Gaussian variables with a negative part
    real(rn)                 :: LTR_fraction
    integer, allocatable     :: iptr_nvneg(:)
    real(rn), allocatable    :: gaupars(:,:)

end module UR_Gleich_globals

!#######################################################################


module UR_perror
    use UR_Gleich_globals,          only: charv

    integer                 :: ifehlp, kequation
    type(charv),allocatable :: symb_new(:)
    integer                 :: nsymbnew

end module UR_perror

!#######################################################################


module UR_gtk_globals

    use, intrinsic :: iso_c_binding, only: c_double, c_char, c_int, &
                                           c_bool, c_ptr

    use UR_types,            only: rn
    use UR_gtk_window,       only: widgets_type, Wclobj, GdkRGBA, &
                                   KSetting, charv

    use gtk_sup,             only: gtktreeiter, gvalue
    use UR_Gleich_globals,   only: nmumx

    implicit none

    type(c_ptr)          :: dintval, &
                            pstring, &
                            plogval

    type(gvalue), target :: dint4, &
                            logval, &
                            stringv

    type(widgets_type), target :: UR_widgets
    type(Wclobj), target     :: clobj
    integer                  :: nclobj       ! number of widgets

    real(c_double)           :: RGBA_BG_windia(4)
    real(c_double)           :: RGBA_BG_items(4)
    character(len=7)         :: RGBA_BG_windia_hex, RGBA_BG_items_hex

    logical                  :: QuitProg         ! = T, if project to be closed

    character(:),allocatable :: str_item_clicked
    logical                  :: item_setintern      ! item is set internally, suppress the
    !   associated widget signal
    logical                  :: plot_setintern
    logical                  :: replot_on
    logical                  :: plinit_done,plinit3_done

    type(gtktreeiter), target :: iter

    integer                  :: ioption             ! used in LoadSelDiag_new
    character(:),allocatable :: dialogstr           ! name of dialog
    logical                  :: NBsoftSwitch
    logical                  :: prout_gldsys        ! output by URGladesys, y/n
    logical                  :: list_filling_on     ! GTKListStore is being filled

    integer   ,   parameter  :: nstmax = 32, ncolmax = 35
    integer                  :: TV1_lentext

    integer                  :: nstores         ! number of liststores
    type(charv),allocatable  :: storename(:)    ! names of liststores
    type(charv),allocatable  :: lsgtype(:,:)    ! e.g., gchararray
    integer                  :: ncolsmx         ! max of 2nd argument of lsgtype
    type(charv),allocatable  :: tvnames(:)      ! GTKTreeView names
    type(charv),allocatable  :: tvmodel(:)      ! GTKTreeView models

    integer                  :: lstype(nstmax, ncolmax)       ! e.g.: 1: Text, toggle;    2: Combo
    integer                  :: lscolnums(nstmax)
    integer                  :: ntvs                          ! number of treeviews
    integer                  :: tvcolindex(nstmax, ncolmax)
    integer                  :: tv_colwidth_digits(nstmax,20)
    integer                  :: tv_colwidth_pixel(nstmax,20)
    integer                  :: tvcols(nstmax)
    integer                  :: TVlastCell(3)

    logical                  :: FieldEditCB, ButtonClicked, FieldDoActCB, PageSwitchedCB,HelpButton
    logical                  :: CloseDialogCB, WinMC_resized
    logical                  :: dialogloop_on
    integer                  :: ncitemClicked
    character(len=4)         :: toggleTypeGTK
    character(len=30)        :: Notebook_labelid(6)      ! ,Notebook2_labelid(3)
    character(len=30)        :: Notebook_labeltext(6)    !   ,Notebook2_labeltext(3)
    character(:),allocatable :: chfilter                 ! FileChooser

    type, bind(c)     :: GtkRecentData
        character(c_char) :: display_name(250)
        character(c_char) :: description(250)
        character(c_char) :: mime_type(250)
        character(c_char) :: app_name(250)
        character(c_char) :: app_exec(250)
        character(c_char) :: groups(20)
        character(c_bool) :: is_private
    end type GtkRecentData

    type(KSetting)           :: Settings
    character(:),allocatable :: fontnameSV,fontname,colorname
    integer                  :: kcolortype      ! 1: bg_color; 2: selected_bg_color
    logical                  :: consoleout_gtk
    logical                  :: lstfd_syms,lstfd_symtable,lstfd_valunc,lstfd_budget   ! listtorefilled?
    integer                  :: dialog_leave             ! 0: leaving by Cancel;  1: leaving by Ok
    integer(c_int)           :: posx=0,posy=0,mainposx=0,mainposy=0,monitor_at_point

    logical                  :: dialog_on, switched_ignore
    logical                  :: winPL_shown
    integer                  :: pixel_per_char
    integer                  :: monitorUR = 1

    real(rn)                 :: zoomf, zoomf_prev

    integer                  :: scrwidth_min, scrwidth_max, scrheight_min, scrheight_max  ! screen parameter
    type(c_ptr), target      :: gscreen

    type(c_ptr)             :: nbook2
    type(c_ptr)             :: provider

end module UR_gtk_globals

!#########################################################################

MODULE UR_DLIM
    use UR_types,     only: rn
    implicit none

    real(rn)                 :: Fconst, Flinear       ! Act = Rnet * Flinear + Fconst
    real(rn)                 :: uFc                   ! u(Fconst) : not calculated
    real(rn)                 :: uFlinear              ! u(Flinear):  = u(w))
    real(rn)                 :: DCFlin(3)             !   22.12.2024
    real(rn)                 :: DCEGr(3)              !
    real(rn)                 :: DCRnet(3)             !

    real(rn)                 :: RblTot(3)             ! "Grand background" (NE + blank) = (RD - Rb)
    integer                  :: nit_decl              ! number of iterations for decision threshold (1)
    integer                  :: nit_detl              ! number of iterations for detection limit (>1)
    real(rn)                 :: decthresh             ! decision threshold value (DT)
    real(rn)                 :: detlim                ! detection limit value (DL)
    real(rn)                 :: kalpha,kbeta          ! Quantils of the normal distribution
    real(rn)                 :: alpha,beta            ! related probabilities
    LOGICAL                  :: iteration_on          ! =T during DL iterations, otherwise =F
    integer                  :: limit_typ             ! 1: DT;   2: DL
    LOGICAL                  :: iterat_passed         ! for LsqLincov2
    CHARACTER(:),allocatable :: NWGmeth
    CHARACTER(:),allocatable :: NWGMethode
    real(rn)                 :: WertBayes             ! value of "best estimate"
    real(rn)                 :: UcombBayes            ! uncertainty of the "best estimate"
    real(rn)                 :: FakRB                 ! cofactor of the gross count rate (in most cases = 1)
    real(rn)                 :: W1minusG              ! probability 1-gamma
    real(rn)                 :: KBgrenzu,KBgrenzo     ! lower and upper confidence limit (Bayes)
    real(rn)                 :: KBgrenzuSH,KBgrenzoSH  ! lower and upper limit of the shortest-length interval
    LOGICAL                  :: GamDist_Zr
    real(rn)                 :: GamDistAdd            ! =1.0 oder =0.5; (N->N+GamDistAdd)
    integer                  :: nit_detl_max          ! max. of allowed DL iterations
    real(rn)                 :: RD_Result             ! net count rate value RD, calculated in Resulta
    real(rn)                 :: A_Result              ! activity value A calculated in Resulta
    logical                  :: var_brutto_auto       ! under certain conditions, the variance formula for
    ! the gross count rate is buildt internally
    integer                  :: k_autoform            ! is the index number of the gross count rate
    real(rn)                 :: fBay_g                ! fBay factor taken from the variables associated with
    ! a mean of a dataset
    integer                  :: modeB, kluB           ! used in/for the function brentx
    real(rn)                 :: fvalueB               ! used in/for the function brentx
    integer                  :: kqtyp                 ! 9.6.2024

    real(rn)                 :: RD                    ! am 23.12.2024 aus mcsr hierher
    real(rn)                 :: ffx                   ! 23.12.2024


END MODULE UR_DLIM

!#######################################################################

module UR_Loadsel

    implicit none

    integer                   :: ichk1,ichk2,ichk3,kopt, klfrename
    integer                   :: numrowsold
    character(:),allocatable  :: Sname,Soldname
    integer                   :: NBcurrentPage,NBcurrentPage2
    integer                   :: NBpreviousPage
    character(len=260)        :: missdata_file                 ! 28.4.2025

end module UR_Loadsel

!#######################################################################

module UR_Linft

    USE UR_Gleich_globals,     ONLY: nabmx, nmumx, nformax,charv
    use UR_types, only: rn

    implicit none

    integer, parameter       :: ma=3, ndatmax=120

    real(rn),allocatable     :: sig(:),x(:),y(:),ux(:)
    real(rn),allocatable     :: yfit(:)
    real(rn)                 :: a(ma),spa(ma)
    ! real(rn)                 :: covar(ma,ma)
    real(rn),allocatable     :: covar(:,:)              !  covariance matrix
    real(rn)                 :: Chisq,Chisq2,Chisqr,Chisqr_NLS,Chisqrzz_WTLS,Chisqr_WTLS

    integer                  :: ifit(ma)              ! yes/no array for the fit of 3 decay correction terms
    integer                  :: ifitSV(ma)
    integer                  :: ifitSV2(ma)           ! 7.6.2024
    integer                  :: iap(ma)               ! 7.6.2024
    integer                  :: mfit                  ! number of components to be fitted
    integer                  :: mxind                 ! number of independet input quantities     ! 5.8.2023
    integer                  :: k_rbl                 ! index No. of the symbol RBL in the argument list of LINFIT
    integer                  :: k_tmess               ! index No. of the symbol tmess in the argument list of LINFIT
    integer                  :: k_tstart              ! index No. of the symbol tstart in the argument list of LINFIT
    integer                  :: k_tlive               ! index No. of the symbol tlive in the argument list of Gamspk1
    integer                  :: mfitfix               ! number of components to be fitted and/or to be fixed.

    LOGICAL                  :: FitDecay              ! Decay curve to be fitted: yes/no
    logical                  :: SumEval_fit           ! SumEval to be used?
    integer                  :: numd                  ! number of points at which the decay curve was mseasured
    integer                  :: nwei                  ! Fitting without (0) or with (1) weighting
    integer                  :: nkovzr                ! Fitting without (0) or with (1) cosnidering covariances
    CHARACTER(LEN=20)        :: CFaelldatum           ! Date/time of chemical separation, e.g., Y from Sr
    type(charv),allocatable  :: CStartzeit(:)         ! Date/time of start of the measurement
    type(charv),allocatable  :: CStartzeit_CP(:)      !    and a copy

    ! gross count rates related symbols:
    real(rn),allocatable     :: dmesszeit(:)          ! counting duration value
    real(rn),allocatable     :: dmesszeit_CP(:)       !    and a copy
    real(rn),allocatable     :: dbimpulse(:)          ! gross counts obtained by a measurement
    real(rn),allocatable     :: dbimpulse_CP(:)       !    and a copy
    real(rn),allocatable     :: dbzrate(:)            ! gross count rate value
    real(rn),allocatable     :: dbzrate_CP(:)         !    and a copy
    real(rn),allocatable     :: sdbzrate(:)           ! standard uncertainty of the gross count rate
    real(rn),allocatable     :: sdbzrate_CP(:)        !    and a copy

    ! background count rates related symbols:
    real(rn),allocatable     :: d0messzeit(:)
    real(rn),allocatable     :: d0messzeit_CP(:)
    real(rn),allocatable     :: d0impulse(:)
    real(rn),allocatable     :: d0impulse_CP(:)
    real(rn),allocatable     :: d0zrate(:)
    real(rn),allocatable     :: d0zrate_CP(:)
    real(rn),allocatable     :: sd0zrate(:)
    real(rn),allocatable     :: sd0zrate_CP(:)
    real(rn),allocatable     :: d0zrateSV(:)          ! further copies
    real(rn),allocatable     :: sd0zrateSV(:)         !

    real(rn),allocatable     :: dnetrate(:)           ! net count rate value
    real(rn),allocatable     :: dnetrate_CP(:)        !    and a copy
    real(rn),allocatable     :: SDnetrate(:)          ! uncertainty of dnetrate
    real(rn),allocatable     :: SDnetrate_CP(:)       !    and a copy
    real(rn),allocatable     :: dgrossrate(:)         ! 22.6.2024

    real(rn),allocatable     :: fixedrate(:)          ! sum of count rates of fixed fitting parameters
    real(rn),allocatable     :: SDfixedrate(:)        ! its standard uncertainty
    real(rn),allocatable     :: fixedrateMC(:)        ! the same but by MC simulation
    logical                  :: parfixed
    integer                  :: kuse_fixed            ! =1: sdfixedrate=0 and cov_fixed=0;  see Rechw1
    ! =2: calculate sdfixedrate and cov_fixed completely

    real(rn),allocatable     :: dtdiff(:)             ! time differences
    real(rn)                 :: fpa(ma),sfpa(ma)      ! Fit parameters and their uncertainties
    real(rn)                 :: fpaSV(ma)             ! saved Fit parameter
    real(rn)                 :: sfpaSV(ma)            ! saved Unsicherheiten of fit parameters
    real(rn)                 :: fpakq(ma)             ! the fpa values in the case of MC simul., per kqtyp
    real(rn),allocatable     :: dnetfit(:)            ! fitted values of the net count rates
    real(rn),allocatable     :: SDnetfit(:)           !    and their std. uncertainties
    integer                  :: linfzbase             ! Linfit time base on input: 1: seconds, 2: minutes
    real(rn)                 :: UcombLinf             ! uncertainty of net count rate Rn(Linfit), calc. from input qunatities
    real(rn)                 :: UcombLinf_kqt1        ! uncertainty of net count rate Rn(Linfit), calc. from input qunatities
    real(rn)                 :: UcombLfit             ! means StdUnc(klu)
    integer                  :: KPearson              ! Pearson-Chi-square-criterion? 0:no; 1:yes
    integer                  :: kPMLE                 ! Poisson PMLE-Chi-square-criterion? 0:no; 1:yes
    integer                  :: nchannels             ! number of counting channels (LSC)
    logical                  :: use_WTLS              ! apply weighted total least squares
    integer                  :: kfitp(2)              ! (1): No. of 'Fitp1' in the list 1 through ngrs
    ! (2): Nr. of cov(Fitp1,Fitp2) in the list 1 through ncov
    real(rn),allocatable     :: afuncSV(:,:)          ! decay term functions X(i,1:3)
    integer                  :: klincall              ! actual number of calls to Linfit
    integer   ,allocatable   :: mpfx(:)               ! symbol index values of the argument symbols of the Linfit call
    integer                  :: nhp                   ! number of mpfx-parameters
    integer   ,allocatable   :: mpfxfixed(:)          ! = 1, if the uncertainty of Messwert(mpfx(j)) is already
    !      contained in fixedrate(i), otherwise = 0
    logical,allocatable      :: mpfx_extern(:)        ! T: if mpfx parameter is also used outside the Linfit call
    !    (--> unc. propagation: outside of Linfit; avoid double consideration)
    ! F: if mpfx parameter is not used outside the Linfit call
    !    (--> unc. propagation: inside of Linfit)
    integer   ,allocatable   :: mpfx_ind(:)           ! mpfx_ind(k) yields j, if mpfx(j) = k
    real(rn),allocatable     :: Qsumarr(:)            ! is the matrix QsumX, but packed into a 1-dim array
    integer                  :: nccg                  ! = 0, if covariances bewtween X decay terms do not exist; =1 otherweise
    integer   ,   parameter  :: nparmx = 3            ! maxim. number of X decay terms
    ! real(rn),allocatable     :: x1A(:),x2A(:),x3A(:)  ! arrays of X decay terms
    real(rn),allocatable     :: xA(:,:), xB(:,:)       ! arrays of X decay terms

    integer                  :: kfitmeth              ! 0: WLS; 1: PLSQ; 2: PMLE; 3: WTLS
    real(rn),allocatable     :: covyLF(:,:)           ! covariance matrix of net count rates
    character(len=10)        :: fitmeth               ! fit method string
    logical                  :: singlenuk             ! = T, if only one radionuclide activity is to be decay fitted
    logical                  :: export_r              ! = T, if export to R option is activated, =F otherwise
    logical                  :: export_case(3)        ! used in Linf, for export to R
    integer                  :: mfrbg                 ! the number (1:3) of the fitted parameter (as background) in the case of PMLE
    real(rn)                 :: tmedian               ! median value of individual counting durations

    type(charv),allocatable  :: fitopt(:)             ! 'fit', 'fix', 'omit'
    real(rn),allocatable     :: cov_fixed(:,:)        ! special covar matrix for a fixed parameter; see Rechw1
    real(rn),allocatable     :: covx(:,:)             ! covariance matrix of the decay terms X()

    logical                  :: FitCalCurve           ! = T if a quantity value is to be defined from a calibration curve; otherwise F
    real(rn),allocatable     :: xkalib(:)             ! x-values of a measured calibration curve
    real(rn),allocatable     :: uxkalib(:)            !   associated uncertainty values
    real(rn),allocatable     :: ykalib(:)             ! y-values of a measured calibration curve
    real(rn),allocatable     :: uykalib(:)            !   associated uncertainty values
    real(rn),allocatable     :: ykalibSV(:)           ! a copy of ykalib
    integer                  :: kal_Polgrad           ! degree of the calibration polynomial
    integer                  :: nkalpts               ! number of calibration points
    character(len=120)       :: CCtitle               ! short title for the calibration curve
    integer                  :: KFMode                ! Kalfit: 1: calculate polynomial(x0); 2: use the inverse polynomial function
    real(rn),allocatable     :: a_kalib(:)            ! array of polynomial fit parameters
    real(rn),allocatable     :: spa_kalib(:)          !   and their std. uncertainties
    real(rn),allocatable     :: a_kalibSV(:)          ! copy of a_kalib
    real(rn),allocatable     :: covar_kalib(:,:)      ! covariance matrix of the polynomial parameters
    real(rn),allocatable     :: covar_kalibSV(:,:)    !    and a copy
    logical                  :: netto_involved_Fitcal ! a variable, used only for the special case of Example_8_with_KALFIT_EN.txp
    real(rn)                 :: ChisqKB               ! chi-square value of the calibration curve fit
    real(rn)                 :: ChisqrKB              ! reduced chi-square value of the calibration curve fit
    real(rn)                 :: ChisqrLfit            ! chi-square value of Linfit
    integer                  :: maKB                  ! number of parameters in the calibration polynomial
    character(:),allocatable :: kalfit_arg_expr     ! the expression of the 2nd argument of Kalfit, as string
    integer                  :: kpoint_kalarg         ! number of the equation of the expression kalfit_arg_expr
    logical                  :: use_UfitKal           ! = T: calculate als uncertainties of fit values of the calibration curve
    logical                  :: defineallxt           ! shall all X(t) decay terms be expolicitly given?
    integer                  :: Tnstep                ! number of steps nstep from LSQGEN (WTLS)
    real(rn),allocatable     :: covpp(:,:)            ! covariance matrix of the parameters contained in the X decay terms
    real(rn)                 :: R0k(3),sdR0k(3)       ! background count rates of up to 3 measurement channels, and uncertainties
    logical                  :: konstant_r0           ! are all background count rate values identical (measured only once)?
    logical                  :: WTLS_wild             ! for large deviations from Neeyman LSQ
    real(rn)                 :: fpaLYT(3,3)           ! final copies of fpa, before starting DT and DL calculations
    real(rn)                 :: sfpaLYT(3,3)          ! final copies of sfpa, before starting DT and DL calculations
    real(rn)                 :: covarLYT(3)           ! final copy of the covarince matrix of fpa, before starting DT and DL calculations

    real(rn)                 :: Chis_test(2)          ! (1): lincov2;   (2):  LSQGEN
    logical                  :: posdef                ! is a matrix, to be inverted, positive definite?
    real(rn)                 :: cofact,cofactlyt      ! factor for reducing a special covariance contribution
    integer                  :: ncofact               ! counts how often cofactlyt is applied

    logical                  :: test_cauchy           ! shall testing for the Cauchy-Schwarz inequality be applied? (FindCovx))
    logical                  :: cauchy_failed1        ! for covppcalc
    logical                  :: cauchy_failed2        ! for Findcovx
    logical                  :: cauchy_failed3        ! for E7lsq1UR
    logical                  :: use_otherMinv         ! apply alternative routines for MINV ond Chol?
    integer                  :: ndefall               ! if(ndefall == 1) defineallxt = .true.

    real(rn)                 :: valEGr(3),uncEGr(3)      ! values and uncertainties of the up to 3 output quantities
    real(rn)                 :: corrEGR(3,3),covEGr(3,3) ! correlations and covariances of the output quantities
    real(rn)                 :: corrFPA(3,3),covFPA(3,3) ! correlations and covariances of the Linfit fit parameters
    integer                  :: igsel(3),eliRS           ! used for the confidence ellipsoid
    logical                  :: run_corrmat            ! used in corrmatEGr
    logical                  :: dmodif                 ! used in Loadsel_diag_new, = T if the decay curve fit model has been modified
    logical                  :: condition_upg          ! a more complex condition use in Lsqlincov2
    logical                  :: mfRBG_fit_PMLE         ! fit the mfrbg parameter (BG) non-linear

    logical                  :: use_constr             ! for using constraints in non-linear fitting
    integer                  :: kconstr(3)             ! 0: für Anwendung con constr; 1: keine Anwendung
    real(rn)                 :: upcstr(3)              ! predetermined parameter uncertainties for using constraints
    real(rn)                 :: pcstr(3)               ! values of parameter constraints
    real(rn)                 :: penalty_factor

    real(rn),allocatable     :: xpl(:),ypl(:),uypl(:),yplfit(:)  ! arrays for plotting, used in CurvePlot
    real(rn),allocatable     :: xplz(:),yplz(:),yplsing(:,:)     ! ! arrays for dense plotting of fit curve

    real(rn),allocatable     :: Qxp(:,:)               ! matrix of partial derivatives with respect to mpfx parameters
    !    used in LsqlinCov2;
    !   Qxp is needed for the test calculations at the end of LSQgen
    real(rn),allocatable     :: covppc(:,:)            ! holds either cocpp, or (in the case of MC): covpmc
    ! (both calculated by covppcalc)

    logical                  :: compare_WTLS           ! if T: prepare and print a comparison of WTLS and WLS
    logical                  :: use_PMLE               ! used in LSQmar
    logical                  :: use_PLSQ               ! used in LSQmar
    logical                  :: use_WLS                ! used in LSQmar
    logical                  :: nhp_defined            ! are the arguments of Linfit() defined?
    logical                  :: use_WTLS_kal           ! used for WTLS in calibration curve fitting

    integer                  :: mfix
    integer   ,allocatable   :: indfix(:),ifitKB(:),kpt(:)
    real(rn),allocatable     :: xfix(:)
    real(rn),allocatable     :: ycopy(:),uycopy(:)
    integer                  :: nkpmax,mac
    real(rn),allocatable     :: DPmat(:,:)
    integer   ,allocatable   :: kEQnums(:,:)
    logical                  :: use_absTimeStart

    integer                  :: noncv_PMLE
    real(rn)                 :: Chisqr_pmle,pa_pmle(3),parat_kegr,RBGMean,sdR0kZ(3)
    real(rn)                 :: pa_mfrbg_mc             ! required in runPMLE when called from MCsim_on                   !
    integer                  :: iteration_pmle
    logical                  :: convg_pmle

end module UR_Linft

!#######################################################################

module UR_Gspk1Fit

    use UR_types, only: rn
    use UR_Gleich_globals,     only: charv

    implicit none

    integer   , parameter    :: kdatmax=10

    LOGICAL                  :: Gamspk1_Fit            ! perform a Gamspk1 evaluation: yes/no

    integer                  :: unitradio(5)
    ! arrays characterizing the gamma peaks:
    real(rn),allocatable     :: Erg(:)                      ! energies (keV)
    real(rn),allocatable     :: GNetRate(:),SDGNetRate(:)   ! peak net count rates and their uncertainties
    real(rn),allocatable     :: Effi(:),SDEffi(:)           ! peak efficiencies and their uncertainties
    real(rn),allocatable     :: pgamm(:), SDpgamm(:)        ! gamma emission intensities and their uncertainties
    real(rn),allocatable     :: fatt(:),SDfatt(:)           ! attenuation corrections and their uncertainties
    real(rn),allocatable     :: fcoinsu(:),SDfcoinsu(:)     ! true coincidence summing corrections their uncertainties
    real(rn),allocatable     :: RateCB(:)                   ! count rate of the compton continuum background of the peak
    real(rn),allocatable     :: RateBG(:), SDRateBG(:)      ! rate of independent backgroud and its std. uncertainty
    ! copies of the arrays above :
    real(rn),allocatable     :: Erg_CP(:),GNetRate_CP(:),SDGNetRate_CP(:)
    real(rn),allocatable     :: Effi_CP(:),SDEffi_CP(:),pgamm_CP(:)
    real(rn),allocatable     :: SDpgamm_CP(:),fatt_CP(:),SDfatt_CP(:)
    real(rn),allocatable     :: fcoinsu_CP(:),SDfcoinsu_CP(:)
    real(rn),allocatable     :: GNetRateSV(:),SDGNetRateSV(:)
    real(rn),allocatable     :: RateCB_CP(:),RateBG_CP(:)
    real(rn),allocatable     :: SDRateBG_CP(:)

    real(rn),allocatable     :: varadd_Rn(:)                ! variance of total background rate (contributios by RateCB and RateBG)
    real(rn),allocatable     :: tlive                       ! live time (counting duration))

    integer   ,allocatable   :: guse(:),guse_CP(:)          ! = T, if the peak is used for calculating the mean activity
    real(rn)                 :: gspk_sigint,gspk_sigext     ! "internal" and "external" standard uncertainty of the mean
    real(rn)                 :: gspk_qval                   ! chi-square test value
    real(rn)                 :: gspk_xmit                   ! weighted mean of individual peak activites
    real(rn)                 :: gspk_free                   ! degree of freedom
    real(rn)                 :: gspk_chisqr                 ! reducec chi-square
    real(rn),allocatable     :: aktnz(:),SDaktnz(:)         ! individual peak activities and associated uncertainties
    real(rn),allocatable     :: SDaktnzSV(:),SDaktnzMV(:)   !     and copies of the uncertainties
    real(rn)                 :: FBT                         ! factor for peak area uncertainty calculation

    character(LEN=10)        :: mwtyp                       ! type of mean value: 1: 'WeiMean'; 2: 'LSQMean'
    type(charv),allocatable  :: mwopt(:)                    ! type of mean value: given as string
    integer                  :: ecorruse                    ! =T, if correlations between peak efficiencies are used; =F otherwise
    integer                  :: WMextSD                     ! =0: use "internal" SD of weighted mean; =1: use "external" SD
    real(rn)                 :: detlim_approximate          ! auxiliary variables for a formula approxim. the DL
    logical                  :: gmodif                      ! has any of the gamma input values be changed in the dialog?
    integer                  :: UnitR_effi_old,UnitR_pgamm_old  ! probably no longer used

    real(rn),allocatable     :: UxaMV(:,:),Uxa(:,:)         ! for SumEval1
    integer                  :: kmwtyp                      ! Mittelwerttyp    ! 2025.01.23 GK

end module UR_Gspk1Fit

!#######################################################################

module UR_MCC

    use UR_types, only: rn
    use UR_Linft, only: ndatmax
    use UR_Gleich_globals, only: ncovmx, nmumx

    implicit none

    integer                  :: idum
    integer                  :: imcmax2, kqtyp, imc10, imcmax

    real(rn)                 :: estUQ, UQprob, estLQ, LQprob      ! upper and lower quantiles, and the associated probabilities
    real(rn)                 :: estLQ_BCI2,estUQ_BCI2,estLenBci2  ! limits and length of the shortest coverage interval (searchBCI3)
    real(rn)                 :: rxLQBci,rxUQBci,rxLenBci          ! their relative std. uncertainties

    real(rn)                 :: est1LQ,est1UQ,est1LQ_BCI          ! these are means of corresponding xz* or rxz* arrays ()
    real(rn)                 :: est1UQ_BCI,rx1LQ,rx1UQ            !
    real(rn)                 :: rx1LQbci,rx1UQbci                 !
    real(rn)                 :: est1LenBCI,rx1lenBCI              !

    real(rn)                 :: xxmit1,rxmit1,xxsdv,rxsdv,xLQ,rxLQ,xUQ,rxUQ  !
    real(rn)                 :: xxmit2,rxmit2,xxmit3,rxmit3,xxsdv3,rxsdv3    !
    real(rn)                 :: xDT,rxDT,XDL,rxDL,r0dummy,sdr0dummy          !
    real(rn)                 :: xxmit1PE,rxmit1PE,xxsdvPE,rxsdvPE            !


    integer, parameter       :: npltmax=1500       ! 29.10.2025        ! max. number of values to be plotted
    integer, parameter       :: mcmax=30000                ! max. array length of MC multichannel spectra
    integer, parameter       :: mc2max = 2E+06             ! max. number of MC values evaluated per MC run
    real(rn), allocatable    :: arraymc(:,:)               ! array of MC values (1st index kqt <= 3, 2nd index mcmax)

    integer, allocatable     :: mcafull(:,:)       ! three distribution spectra
    integer, allocatable     :: mcafull2(:)        ! distribution spectrum DT case, accumulated over several MC runs
    integer, allocatable     :: mcafull3(:)        ! distribution spectrum DL case, accumulated over several MC runs

    integer                  :: nval(3), mcasum(3), mcasum3, mcasum2            ! no. of values, sum of counts in the spectra
    integer                  :: mcaplot(npltmax,3)                              ! distriubution plot sepctra for result, DT and DL
    real(rn)                 :: xstep(3), xstep_min(3), mca_min(3), mca_max(3)  ! x step size in plot arrays
    real(rn)                 :: mca_min_min(3), mca_max_min(3)                  ! extreme values
    real(rn)                 :: stepp(3)

    real(rn), allocatable    :: xplt(:,:), yplt(:,:)                         ! x- and y- arrays for final plot of a distribution
    character(60)            :: title(3)                                     ! title of thet plot (Result, DT, DL))
    integer                  :: imctrue                                      ! number of successful MC trials per run; selected numer of runs
    integer                  :: kcrun                                        ! selected numer of MC runs

    ! variables used for setting up a distribution spectrum and its plot:
    integer                  :: kdposx,kdposy,igmin(3),igmax(3),kcmx,imcPE
    real(rn)                 :: VertLines(10),shmin(3),shmax(3),shfakt(3)
    logical                  :: use_shmima,use_BCI
    ! parameters used by generating random numbers   for gamma, beta and t distributions:
    real(rn), allocatable    :: c_mars(:),d_mars(:)                       ! ran_gamma8
    real(rn), allocatable    :: a_rg(:), p_rg(:), c_rg(:), uf_rg(:), vr_rg(:), d_rg(:)  ! ran_gamma2
    real(rn)                 :: d_rb(10),f_rb(10),h_rb(10),t_rb(10),c_rb(10)
    real(rn)                 :: s_rt(10),c_rt(10),a_rt(10),f_rt(10),g_rt(10)
    logical                  :: swap_rb(10)

    integer                  :: kqtypx                 ! another variable for kqtyp
    integer                  :: imc                    ! number of active MC trial
    integer                  :: qtindex                ! array index of quantile position
    real(rn), allocatable    :: covpmc(:,:)            ! covar matrix for MC, calculated by covppcalc
    integer                  :: iopt_copygr            ! index of selected graphic format for plotting
    logical                  :: MCsim_done             ! =T after MC simultation has finished

    real(rn)                 :: xmit1,xsdv,xmit2,xmit1PE,xsdvPE,xmit1qPE,xmit1min
    real(rn), allocatable    :: xmitsgl(:),xmitsglq(:),xsdvsgl(:)
    real(rn), allocatable    :: mwnet(:),mwnetmit(:),mwnetmitq(:),mwnetvgl(:),xsdnet(:),umwnetvgl(:)
    real(rn)                 :: covmw12,sdmw12,mw12,mw12q,estpU_min,estUQ_min
    real(rn), allocatable    :: xxDT(:)                        ! DT values by MC simulation
    real(rn)                 :: cpu_time_MC

    real(rn), allocatable    :: bvect(:),zvect(:),muvect(:)   ! used for random numbers of correlated variables
    real(rn), allocatable    :: muvectt(:),covxyt(:,:)          !

    ! variables used in preparing covariances for MC simulation:
    integer                  :: j1,icn, ncgrp,nj1,nj2,nc1,icc,icc1,icc2
    integer, allocatable     :: nf1(:),nf2(:),nf3(:)
    real(rn), allocatable    :: covxy(:,:)
    integer, allocatable     :: icnvec(:),icnmpfx(:)

    real(rn)                 :: medianqt(3)              ! median values of arraymc for each kqt
    character(:),allocatable :: xlabelMC,ylabelMC
    real(rn)                 :: DT_anf                   ! initial guess of the DT value

    real(rn)                 :: epsgam,uepsgam           ! efficiency and its uncertainty value
    logical                  :: eps_gamdis,w_gamdis      ! take efficiency eps of calibr. factor as gamma distributed?
    logical                  :: test_mg                  ! test mathews & Gerts case

end module UR_MCC

!#######################################################################

module UR_LSQG
    ! parameters used for WTLS (Lsqgen)
    integer             :: maxn, maxnr, maxm
    integer, parameter  :: mmax = 300        ! max. number of measured values of a curve
    integer             :: mtype             ! type of model function
    integer, parameter  :: maG = 3

end module UR_LSQG

!#######################################################################

module UR_Derivats

    use UR_types, only: rn

    real(rn), allocatable :: dfda(:)           ! partial derivatives      ! 6.8.2023
    real(rn), allocatable :: dfde(:)           ! partial derivatives
    character(len=1)      :: dervtype          ! type of derivation (N: numerical / A: analytical)
    logical               :: ex1_SLine         ! used in auxdrg

end module UR_Derivats

!#######################################################################

module UR_GaussInt

    use UR_types, only: rn

    implicit none

    integer         :: ifehlm
    integer         :: nik, ngk
    real(rn)        :: xmean, ymean
    real(rn)        :: ux,uy   ! standard deviations
    real(rn)        :: rho     ! correlation coefficient

end module UR_GaussInt

!#######################################################################

module UR_eli
    ! used for constructing the confidence ellipsoid in the case of
    ! linear unfolding with at least two output quantities
    use UR_types, only: rn

    integer   , parameter        :: nptsel = 201
    real(rn)                 :: p,p1,p2,p3,g1,w,a1,a2,a3,a4,a5, alpha, angle, rnnd
    real(rn)                 :: areaElli, theta       ! ,sqchi2
    real(rn)                 :: alphamin,angmin,angmax,exmin,exmax,eymin,eymax,st,ct,x1,x1min,x2
    real(rn)                 :: xmin,xmax,ymin,ymax,y1,y2,thetab,dist,distmain,angmain,xp1,yp1
    real(rn),allocatable     :: xt(:),yt(:)
    real(rn)                 :: Atrans(4,4), Arot(4,4), Acomb_TR(4,4),Ascale(4,4),vect(4),scf(4),Acomb_TRS(4,4)
    real(rn)                 :: xx0,yy0,xs,ys,tt1,tt2,dangle
    ! real(rn)                 :: Atrans2(3,3), Arot2(3,3), Acomb2(3,3),Ascale2(3,3),vect2(3)
    character(len=100)       :: xachse, yachse,pltitle

end module UR_eli

!#######################################################################

module UR_MCSR

    use UR_types,     only: rn
    use UR_Linft,      only: ma, ndatmax
    use UR_Gleich_globals,     only: nmumx

    integer   , parameter    :: nrx = 50    ! maximale number of MC runs
    integer                  :: kr,kmm,kmmt, nvarSV     ! loop variables

    real(rn),allocatable     :: xzmit(:),rxzmit(:), xzsdv(:),rxzsdv(:)
    real(rn),allocatable     :: xzmitPE(:),rxzmitPE(:), xzsdvPE(:),rxzsdvPE(:)
    real(rn),allocatable     :: xzLQ(:),rxzLQ(:), xzUQ(:),rxzUQ(:)
    real(rn),allocatable     :: sx(:)
    real(rn),allocatable     :: xzDL(:),xzLQbci(:),xzUQbci(:),xzLenBci(:)
    real(rn),allocatable     :: uxxDT(:),uxxDL(:)

    real(rn)                 :: sigmam,xwert,sdxwert,ruxxsdv
    integer                  :: icnvar,izv,nvt,imctrue1,imc2
    real(rn)                 :: xmin1,xmax1
    real(rn)                 :: xxx,xxq,xvor,xwt,rbltotSV(3),start, finish
    real(rn)                 :: xfpa(ma),xsfpa(ma)           ! up to three fitparameters
    real(rn)                 :: bfunc(3)
    real(rn),allocatable     :: netfit(:)                    ! fit values of the net count rates

    real(rn)                 :: xesdev1,xesdevq,xesdev2
    real(rn)                 :: xemit1,xemitq,xemit2,xesdev3,xemitq2,xesdev22,varFl

    ! further copies of various arrays:
    real(rn),allocatable     :: d0zrateSicher(:),sd0zrateSicher(:)
    real(rn),allocatable     :: SDGNetRateSicher(:),GNetRateSicher(:)
    real(rn),allocatable     :: effiSV(:),pgammSV(:),fattSV(:),fcoinsuSV(:)
    real(rn),allocatable     :: RateCBSV(:),RateBGSV(:),SDRateBGSV(:)

    real(rn),allocatable     :: rnetvar(:)
    real(rn),allocatable     :: Messwertw(:)
    real(rn),allocatable     :: MEsswertkq(:),StdUnckq(:)
    real(rn)                 :: dummy,rblindnet

    real(rn),allocatable     :: MEsswert_eg(:),relSdSv(:) ,muvect0(:)
    real(rn)                 :: fparm,factm,R0kz(3)
    integer                  :: i11,kbd,klu,nhg,kgt,kausum,ncov1,nfd,nhgfx
    integer   ,allocatable   :: kv1(:),kgl(:)

    integer                  :: messk,mms,ik,k1,k2,kix,ivant,mmkk
    integer                  :: ntwild,nt1, mcov,k11,idummy,ios,kmode,jj1,jj2,knegative,kmin,kmmt_start
    LOGICAL                  :: wait, use_afuncSV
    logical,allocatable      :: covariter(:)
    CHARACTER(:),allocatable :: ch1
    character(len=1)         :: utw,utu
    real(rn),allocatable     :: muvectR(:),d0zrateZ(:),rblindnetZ(:)
    real(rn)                 :: ut1,chit1,ratioval,ratioSD
    real(rn)                 :: ssxEG(3),mw1,mw2,xcovt,dpi,dpj,mwt1,mwt2
    real(rn)                 :: mw_rbl,uqt,dratio,dmean,ddelta,afu,umw_rbl
    real(rn)                 :: uy0,  gda_SV, mueLN,sigmaLN,urel2,DTmultLN,std2,zratio
    real(rn)                 :: mwref(20)
    real(rn),allocatable     :: MesswertORG(:),StduncORG(:)
    real(rn)                 :: RDlast,xmit1last,dxx,rtb,xx1,xx2,xxL,ff,ffL,swap,mcval(101)
    real(rn)                 :: help1,RDmin,RDmax,helpmin,sd_dt,xmit1_0,xxDT_0       ! ,MesswertVV(200)
    integer                  :: zt1(9),nc1m,nst,kfd,nminus,nplus
    integer   ,allocatable   :: ivref(:)
    logical                  :: wlognorm,DTbracketed,mcnonvary,valaccpt
    real(rn)                 :: meanmc(3),sdmc(3),hmean(2)
    real(rn)                 :: prob,qt

    real(rn)                 :: HBrt,rnnd,r1
    real(rn)                 :: xmitq,MCWert
    real(rn)                 :: help,minres,maxres

end module UR_MCSR

!#######################################################################

module UR_plotp
    ! to be used for subroutine plot3fig
    integer, parameter  :: mklenL=1000
    integer             :: nkpts(6),knum,ncurve(6)
    real(8)             :: pltx(6,2*mklenL),plty(6,2*mklenL),xminp(6),xmaxp(6)
    character(len=100)  :: xlabel(3),ylabel(3),ptitle(2)
    real(8)             :: xminv,xmaxv,yminv,ymaxv
    character(len=50)   :: textL(6)
    logical             :: xlog,ylog

end module UR_plotp

!#######################################################################

module UR_DecChain
  ! introduced: 11.12.2024 ; added to this version: 27.4.2025

    use UR_types, only: rn
    use UR_gtk_window, only: charv

    logical                  :: DChain           ! 16.12.2024  GK
    logical                  :: DChainEGr        ! 27.12.2024  GK

    type DCC            ! December 2024
        integer      :: iser
        integer      :: indx
        integer      :: forward       ! 0: backward;  1: forward
        type(charv)  :: tdiff         ! time difference
        type(charv)  :: tmess         ! counting time (over which is to averaged)
        integer      :: avg           ! counting time averaging: T / F
        integer      :: Nstart        ! index of first nuclide in the chain (Nstart>1: a sub-chain)
        integer      :: Ndestin       ! destination index of chain nuclide, the activity of which
                                        ! the decay correction shall refer to
        integer      :: nchlen        ! length of chain
        type(charv)  :: Symb(10)      ! UR's activity symbols for "Start" activities of the chain
                                        ! ("start" depends on "forward"= yes / no)
        integer      :: symbind(10)   ! Symbol list indices of Symb1 - Symb6
        real(rn)     :: derv(10)      ! derivatives
        real(rn)     :: SD_CV(3,10)   ! SD values of symbind variables (1-10) for kqtype (1-3)
    end type

    type(DCC)    :: DCpar(6)        ! for up to six different SDECAY calls
    integer      :: nsubdec
    real(rn)     :: AdestMC(6), uAdestMC(6)

    integer, allocatable  :: DCindx(:)
    type(charv),allocatable :: DCnuclide(:)
    type(charv),allocatable :: DCnuclide_sh(:)
    type(charv),allocatable :: DCsymbT12(:)     ! half-live symbol names
    type(charv),allocatable :: DCsymbEffiA(:)   ! efficiency symbol name of channel A
    type(charv),allocatable :: DCsymbEffiB(:)   ! efficiency symbol name of channel B
    type(charv),allocatable :: DCsymbEffiC(:)   ! efficiency symbol name of channel C
    type(charv),allocatable :: DCsymbYield(:)   ! chwmical yields
    type(charv),allocatable :: DCsymbLambda(:)  ! decay constants sysmbols
    type(charv),allocatable :: DCZji(:)         ! branching ratios, transitions denoted as text
    real(rn),allocatable    :: DCZjival(:)      ! values of branching ratios

    integer                 :: N_nuclides         ! number of nuclides in the decay chain
    logical                 :: apply_separation
    integer                 :: DCchannels,nzji
    integer                 :: DCBuildupAtSepar

    real(rn),allocatable    :: DCLam(:),uDCLam(:)
    real(rn),allocatable    :: DCeffiA(:),uDCeffiA(:)
    logical                 :: DClistsDefined
    logical                 :: DCcommMeasmt
    logical                 :: use_Lambda

    type(charv),allocatable :: DCList(:)
    character(len=120)      :: DCListZ(10)
    integer                 :: nDCL      ! number of Lists
    integer                 :: ChainSelected
    type(charv)             :: chaincode

    integer                 :: nDCnet    ! number of net count rates
    integer                 :: nDCgross  ! number of gross count rates
    integer                 :: nDCbg    ! number of background count rates
    integer                 :: indDCnet(10),indDCgross(10),indDCbg(10)    ! index arrays point to the UR symbol list
    type(charv)             :: DCnetRate(10),DCgrossRate(10),DCbgRate(10)

    logical                 :: DChain_read_data

    type(charv),allocatable :: nucname(:),T12Lam(:),effiA(:),effiB(:),effiC(:),eta(:)


end module UR_DecChain

!#######################################################################

