This repository includes third‑party components and some files derived from third‑party examples.

This file summarizes the third‑party components, their licenses, where their license texts are stored (licenses/), and which files in this repository are known to contain or derive from their code. 
It is a living document: please update it if you add/remove third‑party code or discover additional derived files.

Third‑party components included or referenced in this repository
(For each component: name — license — where the license text is stored in this repo — paths in this repo that contain derived or bundled code or that use the component.)

1) PLplot (plot examples and derived code)
- License: PLplot library itself is typically LGPL for the library; some example files or bindings may be under GPL (see the upstream statements). Check the files in licenses/plplot for details.
- License files in this repo: licenses/plplot/*
- Known derived/copied files in this repo:
  - src/UR/plot3fig.f90
  - src/UR/CurvePlot.f90
  - src/UR/plgetc.f90
  - src/UR/plplot_code_sub1.f90 

2) GTK3 (GIMP Toolkit)
- License: LGPL v2
- License files in this repo: licenses/gtk3/*
- Use in repo: GTK3 is linked as an external dependency (PkgConfig::GTK3 in CMakeLists.txt).
- Notes: Used for the GUI framework; installed via system package manager.

3) gtk3-fortran (Fortran bindings for GTK3)
- License: GPL‑3.0
- License files in this repo: licenses/gtk3-fortran/*
- Known derived/copied code:
  - gtk3-fortran code is downloaded and compiled from GitHub (vmagnin/gtk-fortran) right before UncertRadio compilation. The compiled library is statically linked into UncertRadio. 


4) fparser (Fortran expression parser)
- License: BSD‑3 (public domain)
- License files in this repo: licenses/fparser/*
- Known derived/copied files in this repo:
  - src/UR/fparserW.f90 (vendored fparser module)
- Use in repo: Several source files use `use fparser` (e.g., src/UR/Loadsel_diag_new.f90).


5) LAPACK / BLAS
- License: modified BSD / netlib license (see upstream);
- License files in this repo: licenses/lapack/*
- Use in repo: LAPACK is linked (external); not necessarily vendored. CMake links to LAPACK (PkgConfig::LAPACK).

6) Glade (GTK User Interface Designer)
- License: LGPL v2+ / GPL v2+ (mixed; see licenses/glade/COPYING)
- License files in this repo: licenses/glade/*
- Use in repo: The file `UR2.glade` is a Glade UI definition file (XML). Glade itself (the editor) is not distributed; only the .glade file format is used.

7) Adwaita icon theme
- License: LGPL v3 or CC‑BY‑SA 3.0 per the license text in licenses/adwaita-icon-theme/COPYING.
- License files in this repo: licenses/adwaita-icon-theme/*
- Use in repo: icons and theme assets packaged in AppDir/AppRun and/or AppImage build scripts.
