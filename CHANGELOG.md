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
