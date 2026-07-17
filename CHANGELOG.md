# Version 2.7.1 (v2.7.1)

**Release date:** July 17, 2026
[Compare with v2.7.0](https://github.com/OpenBfS/UncertRadio/releases/tag/v2.7.0)

**Summary**

13 commits, 1 contributor

**Fixed**

- Monte Carlo mean/sd now uses all MC samples instead of histogram-filtered range
- Removed redundant `MCDistrib` calls in MC case(3)
- Added missing `EXTERNAL` declaration for `prfunc` in `brentX2` (fixes gfortran 16.x compilation)
- Corrected path for README.md in documentation build

**New**

- GUI: variable to pass button labels to gtk-fortran hl_chooser routines (requires patched gtk-fortran)

**Documentation**

- Added CHANGELOG.md and integrated into Sphinx docs
- Updated batch mode with Excel documentation
- Updated README with published JOSS DOI badge and CITATION.cff
- Added config file locations and log/results file paths
- Updated German translations to use passive voice, improved config documentation
- Restored README.md for each version in make_docs.py

# Version 2.7.0 (v2.7.0)

**Release date:** April 23, 2026
[Compare with v2.6.2](https://github.com/OpenBfS/UncertRadio/releases/tag/v2.6.2)

**Summary**

81 commits, 303 files changed, 2 contributors

**Notable Changes**

**New**

- Configurable output paths via user settings file (`~/.config/UncertRadio/UR2_cfg.dat`)
- Automated CI/CD: GitHub Actions workflows for Linux AppImage, Windows (MSYS2), Docker matrix testing, and nightly builds
- Optional CMake flag to control compiler optimization level
- Integrated logging module throughout the application

**Fixed**

- Monte Carlo calculations with covariances crossing zero
- Monte Carlo plotting artifacts
- Crash when current working directory is the binary directory
- CSV file reading errors
- Kalfix dialog and kalfit matrix routines
- Translation path resolution
- Excel VBA script writing to installation directory

**Improved**

- Out-of-source CMake build with absolute prefix paths
- Performance optimization across multiple modules
- AppImage bundling (pixbuf loaders, icons, libraries)
- Windows build support (pixbuf loaders, DLL dependencies)
- Docker build reproducibility (`.dockerignore`, `buildx`)

**Documentation**

- Added LICENSE and THIRD_PARTY_LICENSES (fixes #11)
- Updated README (Arch Linux, Debian, troubleshooting — fixes #16)
- Documentation site now archives all released versions
- Clarified Excel batch mode is Windows-only
