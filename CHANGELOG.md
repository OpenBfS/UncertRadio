# Version 2.7.2 (v2.7.2)

**Release date:** November 5, 2026
[Compare with v2.7.1](https://github.com/OpenBfS/UncertRadio/releases/tag/v2.7.1)

**Summary**

64 commits, 160 files changed, 1 contributor

**New**

- GUI: buttons to delete the lock file or to "continue at own risk" in the "already running" dialog
- Tolerant config reading via new `read_config()` routines (with unit tests)

**Fixed**

- Crash-prone paths: uninitialized variables, SIGFPE and division-by-zero (PMLE decision-limit path, `qnorm`, `modab1`, fit-method snapshots, Levenberg-Marquardt)
- Negative variances from MC decision-limit bracket probes clamped (PMLE weights, `MCsingRun`)
- PMLE refused in the model dialog unless all result slots are "fit" (LDN_1296)
- Decay-row handling: symbol arrays sized to the actual number of rows, stale timing indices reset, equation numbers rebuilt on column change
- Covariance ellipse: eigenvalue decomposition now uses the original covariance matrix
- `fname_getarg` initialization in `read_cfg`; ProRead log line length enlarged from 512 to 4096 characters
- GTK startup warnings removed
- Stray debug `write(N)` statements migrated to the central logger; no more `fort.NN` files in the working directory

**Improved**

- `run_tests` runs ~27 % faster (equation-number table built once instead of inside `funcs`)
- Faster special functions: `pnorm`/`qnorm`, `glngam`, `brentx`
- Eigenvalue and positive-definiteness helpers simplified

**Documentation**

- Restored German help texts from the legacy Word help; filled about 270 missing German translations
- German translation audit: fixed misaligned msgstr chains, dead section references, wrong example-project filenames, typos, pandoc anchor artifacts, raw TeX fragments, stray and unbalanced bold markup
- Aligned MC threshold statements in the docs with the code (0.02, or 0.150 with PMLE)
- Fixed EN sources with msgid sync: dead "Eq. (5.63)" references replaced by equation labels, example filename corrected to vTI-Y90-16748_Blw
- Fixed typos and grammar in README.md; removed duplicate README.de.md; German links point to /latest/de/; repaired the version msgstr
- AGENTS.md: documented AUTO mode and platform-specific file paths

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
