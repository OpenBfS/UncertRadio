cmake_minimum_required(VERSION 3.20)

# Git Hash
execute_process(COMMAND
  git rev-parse HEAD
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_SHA1
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

# Latest git tag showing the version
execute_process(COMMAND
  git describe --tags --exact-match --abbrev=0
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_VERSION_TAG
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

if(NOT GIT_VERSION_TAG)
  set(GIT_VERSION_TAG "v2.6.x_dev")
endif()

project("UncertRadio"
        DESCRIPTION "This is UncertRadio ${GIT_VERSION_TAG}")

enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Release")
    # set Release related options
    message(STATUS "Release build type")
    set(CMAKE_Fortran_FLAGS "-O3 -fimplicit-none -ffast-math -ffree-line-length-none -ffree-form -fdiagnostics-color=always")
    if(WIN32)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mwindows")
    endif()
else()
    # build type not known -> use Debug values
    message(STATUS "Debug build type")
    set(CMAKE_Fortran_FLAGS "-pg -no-pie -Og -Wall -Wextra -fcheck=all -fbacktrace -fimplicit-none -pedantic -ffree-line-length-none -ffree-form -fdiagnostics-color=always")
endif()


# set Compiler options
set(CPPFLAGS -DGITHASH="'${GIT_SHA1}'"\ -DGITVERSIONTAG="'${GIT_VERSION_TAG}'")
set(CMAKE_Fortran_FLAGS "${CPPFLAGS} ${CMAKE_Fortran_FLAGS}")
message(STATUS "Fortran FLAGS: ${CMAKE_Fortran_FLAGS}")

# Libraries
find_package(PkgConfig REQUIRED)

pkg_check_modules(GTK3 REQUIRED IMPORTED_TARGET gtk+-3.0)
pkg_check_modules(PLPLOT REQUIRED IMPORTED_TARGET plplot)
pkg_check_modules(PLPLOT-FORTRAN REQUIRED IMPORTED_TARGET plplot-fortran)
pkg_check_modules(LAPACK REQUIRED IMPORTED_TARGET lapack)

# Include the ExternalProject module
include(ExternalProject)

# Fix the 2018 std for now
set(GTK_CFLAGS "-pthread -O3 -mtune=native -march=native -std=f2018")
# Define the external project
ExternalProject_Add(gtk3-fortran
    GIT_REPOSITORY https://github.com/vmagnin/gtk-fortran
    BUILD_BYPRODUCTS ${CMAKE_BINARY_DIR}/gtk_install/lib/libgtk-3-fortran.a
    GIT_TAG 0d37844 # checkout fixed gtk3 branch
    CMAKE_ARGS -DNO_BUILD_EXAMPLES=true -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/gtk_install -DCMAKE_Fortran_FLAGS_RELEASE=${GTK_CFLAGS} # Specify the installation directory
)

# Compile the resources
add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/UncertRadio.gresources.c
    COMMAND glib-compile-resources
    ARGS --target=${CMAKE_BINARY_DIR}/UncertRadio.gresources.c
         --generate-source
         --sourcedir=${CMAKE_CURRENT_SOURCE_DIR}
         ${CMAKE_CURRENT_SOURCE_DIR}/UncertRadio.gresources.xml
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/UncertRadio.gresources.xml ${CMAKE_CURRENT_SOURCE_DIR}/UR2_5.glade
)

# set output dirs to build tree (keep source tree clean)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Targets
add_executable(UncertRadio src/UR/UncertRadio.F90)

# add all source files for the target
target_sources(UncertRadio PRIVATE
    src/UR/Batch_proc.f90
    src/UR/Batest.f90
    src/UR/brentX2.f90
    src/UR/color_theme.f90
    src/UR/SelfKummer.F90
    src/UR/CalcUnits.f90
    src/UR/CharFuncts.f90
    src/UR/ConfEllipse.f90
    src/UR/CurvePlot.f90
    src/UR/DATDIF.f90
    src/UR/DECH.f90
    src/UR/DisplayHelp.f90
    src/UR/EvalGerror.f90
    src/UR/Eval_simple.f90
    src/UR/FindCovx.f90
    src/UR/FindSymb.f90
    src/UR/fileIO.f90
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
    # src/UR/parser_mod.f90
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
    src/UR/Translate_mod.f90
    src/UR/URGladesys.f90
    src/UR/UR_Randoms.f90
    src/UR/UR_Winmod.f90
    src/UR/UR_gtk-routines.f90
    src/UR/UR_interfaces.f90
    src/UR/UR_globals.f90
    src/UR/UR_params.f90
    src/UR/UR_types.f90
    src/UR/UncW_Init.F90
    src/UR/UncwB.f90
    src/UR/Uncw_sub3.f90
    src/UR/WDListstoreFill_table.f90
    src/UR/WTLS_calib.f90
    src/UR/WTLS_UR.f90
    src/UR/auxdrg.f90
    src/UR/fparserW.f90
    src/UR/gui_UR_functions.f90
    src/UR/mtx_rout.f90
    src/UR/mtx_rout_submod.f90
    src/UR/plot3fig.f90
    src/UR/procmaindiag.f90
    src/UR/procmaindiag_submod.f90
    src/UR/save.f90
    src/UR/LevMarGavin.f90
    src/UR/FindSums.f90

    # test file
    src/tests/tests.f90

    # gresource binary
    ${CMAKE_BINARY_DIR}/UncertRadio.gresources.c
    )

# Link libraries
target_link_libraries(UncertRadio
        PRIVATE
        PkgConfig::GTK3
        PkgConfig::PLPLOT
        PkgConfig::PLPLOT-FORTRAN
        PkgConfig::LAPACK
        ${CMAKE_BINARY_DIR}/gtk_install/lib/libgtk-3-fortran.a
        )

add_dependencies(UncertRadio gtk3-fortran)
target_include_directories(UncertRadio PRIVATE ${CMAKE_BINARY_DIR}/gtk_install/include/gtk-3-fortran)


# install rules
install(TARGETS UncertRadio DESTINATION "bin")
install(FILES
        UR2_5.glade
        Units_Other.txt
        unitsTable.txt
        BatListRef_v06.txt
        List_DecaySeries.txt
        Settings.ini
        UR2_cfg.dat
        COPYING
        README.md
        README.de.md
        UR2_5_SingleAutoRun.xlsm DESTINATION "bin")

install(DIRECTORY pros licenses translations DESTINATION "bin")
install(FILES docs/InfoFX1.txt DESTINATION "bin/docs")
install(DIRECTORY docs/final/html DESTINATION "bin/docs/final/")
install(DIRECTORY DESTINATION "bin/results")
install(DIRECTORY DESTINATION "bin/log")

if(WIN32)
  install(CODE "execute_process(COMMAND sh install_dlls.sh \${CMAKE_INSTALL_PREFIX})")
  install(FILES $ENV{MSYSTEM_PREFIX}/bin/gspawn-win64-helper.exe DESTINATION "bin")
  install(DIRECTORY $ENV{MSYSTEM_PREFIX}/share/plplot5.15.0 DESTINATION "share" PATTERN "examples" EXCLUDE)
  install(DIRECTORY $ENV{MSYSTEM_PREFIX}/share/glib-2.0 DESTINATION "share")
  install(DIRECTORY $ENV{MSYSTEM_PREFIX}/share/icons/AdwaitaLegacy DESTINATION "share/icons")
  install(DIRECTORY
          $ENV{MSYSTEM_PREFIX}/share/locale/de
          $ENV{MSYSTEM_PREFIX}/share/locale/fr
          DESTINATION "share/locale")
endif()

# Add an option for building the documentation
option(BUILD_DOCS "Build documentation" OFF)
if(BUILD_DOCS)
    message(STATUS "Building documentation")
    # Find Python executable
    find_program(PYTHON_EXECUTABLE python3)

    if(NOT PYTHON_EXECUTABLE)
        message(WARNING "Python3 not found. Documentation will not be built.")
    else()
        # Add a custom target to build the documentation
        add_custom_target(build_docs
            COMMAND ${PYTHON_EXECUTABLE} make_docs.py
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/docs
            COMMENT "Building documentation"
            )

        # Add the build_docs target as a dependency of the main build
        add_dependencies(UncertRadio build_docs)
    endif()
endif()
