cmake_minimum_required(VERSION 3.20)

project("UncertRadio" VERSION 2.5.1
        DESCRIPTION "This is UncertRadio V2.5.1")

enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Release")
    # set Release related options
    message(STATUS "Release build type")
    set(CMAKE_Fortran_FLAGS "-O3 -fimplicit-none -ffast-math -ffree-line-length-none -ffree-form -fdiagnostics-color=always")

#elseif(CMAKE_BUILD_TYPE STREQUAL "Debug")
else()
    # build type not known -> use Debug values
    message(STATUS "Debug build type")
    set(CMAKE_Fortran_FLAGS "-g -Og -W -Wall -Wextra -fcheck=all -fbacktrace -fimplicit-none -pedantic -ffree-line-length-none -ffree-form -fdiagnostics-color=always")
endif()

# Git Hash
execute_process(COMMAND
  git rev-parse HEAD
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_SHA1
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

# Latest git tag showing the version
execute_process(COMMAND
  git describe --tags --abbrev=0
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_VERSION_TAG
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

if(NOT GIT_VERSION_TAG)
  set(GIT_VERSION_TAG "v2.5.X")
endif()


# set Compiler options
set(CPPFLAGS -DGITHASH="'${GIT_SHA1}'"\ -DGITVERSIONTAG="'${GIT_VERSION_TAG}'")
set(CMAKE_Fortran_FLAGS "${CPPFLAGS} ${CMAKE_Fortran_FLAGS}")
message(STATUS "Fortran FLAGS: ${CMAKE_Fortran_FLAGS}")

# Libraries
find_package(PkgConfig REQUIRED)

pkg_check_modules(GTK3 REQUIRED IMPORTED_TARGET gtk+-3.0)
pkg_check_modules(PLPLOT REQUIRED IMPORTED_TARGET plplot)

# set output dir
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

# Targets
add_executable(UncertRadio src/UR/UncertRadio.F90)

# add all source files for the target
target_sources(UncertRadio PRIVATE
    src/UR/Batch_proc.f90
    src/UR/Batest.f90
    src/UR/SelfKummer.F90
    src/UR/CalcUnits.f90
    src/UR/CharFuncts.f90
    src/UR/ConfEllipse.f90
    src/UR/CurvePlot.f90
    src/UR/DATDIF.f90
    src/UR/DisplayHelp.f90
    src/UR/EvalGerror.f90
    src/UR/Eval_simple.f90
    src/UR/FindCovx.f90
    src/UR/FindSymb.f90
    # src/UR/fileIO.f90
    src/UR/InitTreeViews.f90
    src/UR/Kalfit1.f90
    src/UR/Kalfit1_submod.f90
    src/UR/Linf.F90
    src/UR/Linfg1.F90
    src/UR/Linmod1.F90
    src/UR/Loadsel_diag_new.f90
    src/UR/LsqlinCov2.f90
    src/UR/LsqlinCov2_submod.f90
    src/UR/MCCalc.F90
    src/UR/MCPrepCovars.f90
    src/UR/MCsingRun.f90
    src/UR/MCsubs.f90
    src/UR/MCtables.f90
    src/UR/Modvar.f90
    src/UR/Num1.f90
    src/UR/Num1_submod.f90
    src/UR/PDfunctions.f90
    src/UR/PrepReport.f90
    src/UR/ProRead.f90
    src/UR/ProRead_CSV.F90
    src/UR/ProSave.f90
    src/UR/ProSave_CSV.F90
    src/UR/ProcMenu.f90
    src/UR/ProcessLoadPro_new.f90
    src/UR/parser_mod.f90
    src/UR/RdSubs.f90
    src/UR/Read_Gleich.f90
    src/UR/Read_Gleich_submod.f90
    src/UR/Rechw1.f90
    src/UR/Rechw1_submod.f90
    src/UR/Rechw2.f90
    src/UR/Rechw2_submod.f90
    src/UR/RootfindBS.f90
    src/UR/SetSimul_rout.f90
    src/UR/SumEval1.f90
    src/UR/Symbol1.f90
    src/UR/Symbol1_submod.f90
    src/UR/Top.f90
    src/UR/Top_submod.f90
    src/UR/TopoEqs.f90
    src/UR/TranslateUR.f90
    src/UR/URGladesys.f90
    src/UR/UR_Randoms.f90
    src/UR/UR_Winmod.f90
    src/UR/UR_gtk-routines.f90
    src/UR/UR_interfaces.f90
    src/UR/UR_modules_chardeferred.F90
    src/UR/UR_params.f90
    src/UR/UncW_Init.F90
    src/UR/UncwB.f90
    src/UR/Uncw_sub3.f90
    src/UR/WDListstoreFill_table.f90
    src/UR/WTLS_calib.f90
    src/UR/WTLS_UR.f90
    src/UR/auxdrg.f90
    src/UR/brentX.f90
    src/UR/fparserW.f90
    src/UR/gui_UR_functions.f90
    src/UR/mtx_rout.f90
    src/UR/mtx_rout_submod.f90
    src/UR/plot3fig.f90
    src/UR/procmaindiag.f90
    src/UR/procmaindiag_submod.f90
    src/UR/save.f90
    src/UR/LevMarGavin.f90

    # gtk-fortran bindings
    src/fortran_gtk3/cairo-auto.f90
    src/fortran_gtk3/gdk-auto.f90
    src/fortran_gtk3/gdk-pixbuf-auto.f90
    src/fortran_gtk3/gdk-pixbuf-hl.f90
    src/fortran_gtk3/gdkevents-auto.f90
    src/fortran_gtk3/glib-auto.f90
    src/fortran_gtk3/gtk-draw-hl.f90
    src/fortran_gtk3/gtk-hl-accelerator.f90
    src/fortran_gtk3/gtk-hl-assistant.f90
    src/fortran_gtk3/gtk-hl-button.f90
    src/fortran_gtk3/gtk-hl-chooser.f90
    src/fortran_gtk3/gtk-hl-combobox.f90
    src/fortran_gtk3/gtk-hl-container.f90
    src/fortran_gtk3/gtk-hl-dialog.f90
    src/fortran_gtk3/gtk-hl-entry.f90
    src/fortran_gtk3/gtk-hl-infobar.f90
    src/fortran_gtk3/gtk-hl-menu.f90
    src/fortran_gtk3/gtk-hl-misc.f90
    src/fortran_gtk3/gtk-hl-progress.f90
    src/fortran_gtk3/gtk-hl-spin-slider.f90
    src/fortran_gtk3/gtk-hl-tree.f90
    src/fortran_gtk3/gtk-hl.f90
    src/fortran_gtk3/gtk-sup.f90
    src/fortran_gtk3/gtk.f90
    src/fortran_gtk3/pango-auto.f90
    src/fortran_gtk3/plplot_extra.f90

    # plplot fortran bindings
    src/plplot/plplot.f90
    src/plplot/plplot_double.f90
    src/plplot/plplot_single.f90
    src/plplot/plplot_small_modules.f90
    )

if(WIN32)
  message(STATUS "Building for windows")
  target_sources(UncertRadio PRIVATE
  src/fortran_gtk3/mswindowsonly-auto.f90
  )
else()
  message(STATUS "Building for linux")
  target_sources(UncertRadio PRIVATE
      src/fortran_gtk3/unixonly-auto.f90
      )
endif()

# Link libraries
target_link_libraries(UncertRadio
        PkgConfig::GTK3
        PkgConfig::PLPLOT
        )

# install rules
install(TARGETS UncertRadio DESTINATION "bin")
install(FILES
        UR2_5.glade
        Units_Other.txt
        unitsTable.txt
        BatListRef_v06.txt
        Settings.ini
        UR2_cfg.dat
        COPYING
        README.md
        UR2_5_SingleAutoRun.xlsm DESTINATION "bin")
install(DIRECTORY icons UR2_CHM pros licenses DESTINATION "bin")
install(DIRECTORY DESTINATION "bin/results")
install(DIRECTORY DESTINATION "bin/log")

if(WIN32)
  install(CODE "execute_process(COMMAND sh install_dlls.sh \${CMAKE_INSTALL_PREFIX})")
  install(FILES $ENV{MSYSTEM_PREFIX}/bin/gspawn-win64-helper.exe DESTINATION "bin")
  install(DIRECTORY $ENV{MSYSTEM_PREFIX}/etc/gtk-3.0 DESTINATION "etc")
  install(DIRECTORY $ENV{MSYSTEM_PREFIX}/lib/gdk-pixbuf-2.0 DESTINATION "lib")
  install(DIRECTORY $ENV{MSYSTEM_PREFIX}/share/plplot5.15.0 DESTINATION "share" PATTERN "examples" EXCLUDE)
  install(DIRECTORY $ENV{MSYSTEM_PREFIX}/share/glib-2.0 DESTINATION "share")
  install(FILES
          $ENV{MSYSTEM_PREFIX}/share/icons/Adwaita/index.theme
          $ENV{MSYSTEM_PREFIX}/share/icons/Adwaita/icon-theme.cache
          DESTINATION "share/icons/Adwaita")
  install(DIRECTORY $ENV{MSYSTEM_PREFIX}/share/icons/Adwaita/16x16 DESTINATION "share/icons/Adwaita")
  install(DIRECTORY
          $ENV{MSYSTEM_PREFIX}/share/locale/de
          $ENV{MSYSTEM_PREFIX}/share/locale/fr
          DESTINATION "share/locale")
endif()
