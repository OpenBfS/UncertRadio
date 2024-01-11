cmake_minimum_required(VERSION 3.20)

project("UncertRadio" VERSION 2.5.0
        DESCRIPTION "This is UncertRadio V2.5.x")

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
  set(GIT_VERSION_TAG "v2.5")
endif()


# set Compiler options
set(CPPFLAGS -DGITHASH="'${GIT_SHA1}'"\ -DGITVERSIONTAG="'${GIT_VERSION_TAG}'")
set(CMAKE_Fortran_FLAGS "${CPPFLAGS} ${CMAKE_Fortran_FLAGS}")
message(STATUS "Fortran FLAGS: ${CMAKE_Fortran_FLAGS}")

# Libraries
find_package(PkgConfig REQUIRED)
pkg_check_modules(FGSL REQUIRED IMPORTED_TARGET fgsl)
pkg_check_modules(GTK3 REQUIRED IMPORTED_TARGET gtk+-3.0)
pkg_check_modules(PLPLOT REQUIRED IMPORTED_TARGET plplot)

# set output dir
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

# Targets
add_executable(UncertRadio SRC/UncertRadio.F90)

# add all source files for the target
target_sources(UncertRadio PRIVATE
    SRC/Batch_proc.f90
    SRC/Batest.f90
    SRC/CHGM_AMiller.F90
    SRC/CalcUnits.f90
    SRC/CharFuncts.f90
    SRC/ConfEllipse.f90
    SRC/CurvePlot.f90
    SRC/DATDIF.f90
    SRC/DisplayHelp.f90
    SRC/EvalGerror.f90
    SRC/Eval_simple.f90
    SRC/FindCovx.f90
    SRC/FindSymb.f90
    # SRC/fileIO.f90
    SRC/InitTreeViews.f90
    SRC/Kalfit1.f90
    SRC/Kalfit1_submod.f90
    SRC/Linf.F90
    SRC/Linfg1.F90
    SRC/Linmod1.F90
    SRC/Loadsel_diag_new.f90
    SRC/LsqlinCov2.f90
    SRC/LsqlinCov2_submod.f90
    SRC/MCCalc.F90
    SRC/MCPrepCovars.f90
    SRC/MCsingRun.f90
    SRC/MCsubs.f90
    SRC/MCtables.f90
    # SRC/MichelPlot.f90
    # SRC/Module_GSL.f90
    SRC/Modvar.f90
    SRC/Num1.f90
    SRC/Num1_submod.f90
    SRC/PDfunctions.f90
    SRC/PrepReport.f90
    SRC/ProRead.f90
    SRC/ProRead_CSV.F90
    SRC/ProSave.f90
    SRC/ProSave_CSV.F90
    SRC/ProcMenu.f90
    SRC/ProcessLoadPro_new.f90
    SRC/parser_mod.f90
    SRC/RdSubs.f90
    SRC/Read_Gleich.f90
    SRC/Read_Gleich_submod.f90
    SRC/Rechw1.f90
    SRC/Rechw1_submod.f90
    SRC/Rechw2.f90
    SRC/Rechw2_submod.f90
    SRC/RootfindBS.f90
    SRC/SetSimul_rout.f90
    SRC/SumEval1.f90
    SRC/Symbol1.f90
    SRC/Symbol1_submod.f90
    SRC/Top.f90
    SRC/Top_submod.f90
    SRC/TopoEqs.f90
    SRC/TranslateUR.f90
    SRC/URGladesys.f90
    SRC/UR_Randoms.f90
    SRC/UR_Winmod.f90
    SRC/UR_gtk-routines.f90
    SRC/UR_interfaces.f90
    SRC/UR_modules_chardeferred.F90
    SRC/UncW_Init.F90
    SRC/UncwB.f90
    SRC/Uncw_sub3.f90
    SRC/WDListstoreFill_table.f90
    SRC/WTLS_calib.f90
    SRC/WTLS_UR.f90
    SRC/auxdrg.f90
    SRC/brentX.f90
    SRC/constants_nswc.f90
    SRC/fparserW.f90
    SRC/gui_UR_functions.f90
    SRC/mtx_rout.f90
    SRC/mtx_rout_submod.f90
    SRC/plot3fig.f90
    SRC/procmaindiag.f90
    SRC/procmaindiag_submod.f90
    SRC/save.f90
    #SRC/testP2G.f90

    # gtk-fortran bindings
    gtk_src/cairo-auto.f90
    gtk_src/gdk-auto.f90
    gtk_src/gdk-pixbuf-auto.f90
    gtk_src/gdk-pixbuf-hl.f90
    gtk_src/gdkevents-auto.f90
    gtk_src/glib-auto.f90
    gtk_src/gtk-draw-hl.f90
    gtk_src/gtk-hl-accelerator.f90
    gtk_src/gtk-hl-assistant.f90
    gtk_src/gtk-hl-button.f90
    gtk_src/gtk-hl-chooser.f90
    gtk_src/gtk-hl-combobox.f90
    gtk_src/gtk-hl-container.f90
    gtk_src/gtk-hl-dialog.f90
    gtk_src/gtk-hl-entry.f90
    gtk_src/gtk-hl-infobar.f90
    gtk_src/gtk-hl-menu.f90
    gtk_src/gtk-hl-misc.f90
    gtk_src/gtk-hl-progress.f90
    gtk_src/gtk-hl-spin-slider.f90
    gtk_src/gtk-hl-tree.f90
    gtk_src/gtk-hl.f90
    gtk_src/gtk-sup.f90
    gtk_src/gtk.f90
    gtk_src/pango-auto.f90
    gtk_src/plplot_extra.f90

    # plplot fortran bindings
    plplot-5.15.0/plplot.f90
    plplot-5.15.0/plplot_double.f90
    plplot-5.15.0/plplot_single.f90
    plplot-5.15.0/plplot_small_modules.f90
    )

if(WIN32)
  message(STATUS "Building for windows")
  target_sources(UncertRadio PRIVATE
  gtk_src/mswindowsonly-auto.f90
  )
else()
  message(STATUS "Building for linux")
  target_sources(UncertRadio PRIVATE
      gtk_src/unixonly-auto.f90
      )
endif()

# Link libraries
target_link_libraries(UncertRadio
        PkgConfig::FGSL
        PkgConfig::GTK3
        PkgConfig::PLPLOT
        )

# install rules
install(TARGETS UncertRadio DESTINATION "bin")
install(FILES
        UR2_V12.glade
        Units_Other.txt
        unitsTable.txt
        BatListRef_v06.txt
        Settings.ini
        UR2_cfg.dat
        COPYING
        UR2_SingleAutoRun_V12.xlsm DESTINATION "bin")
install(DIRECTORY icons UR2_CHM pros licenses DESTINATION "bin")
install(DIRECTORY DESTINATION "bin/results")
install(DIRECTORY DESTINATION "bin/log")

if(WIN32)
  install(CODE "execute_process(COMMAND sh install_dlls.sh \${CMAKE_INSTALL_PREFIX})")
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
