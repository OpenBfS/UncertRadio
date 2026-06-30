# UncertRadio - Coding Agent Reference

## Project Overview
**UncertRadio** is a Fortran-based GUI application for calculating characteristic limits in radioactivity measurements according to ISO 11929. It computes activity concentrations, measurement uncertainties, decision thresholds, and detection limits using numerical error propagation (ISO GUM) and Monte Carlo simulations.

- **Repository:** `https://github.com/OpenBfS/UncertRadio`
- **License:** GPL v3
- **Version:** 2.7.0 (develop branch is active)
- **Language:** Fortran 2018 with GTK3 GUI bindings

## Architecture

### Entry Point
- `src/UR/UncertRadio.F90` - Main program, handles CLI args, initializes GTK, starts event loop

### Core Modules
- `UR_globals.f90` - **Massive global state module** (1305 lines), contains all shared variables
- `UR_types.f90` - Custom type definitions (e.g., `rn` for real precision, `charv`)
- `UR_params.f90` - Project parameters and constants
- `UR_interfaces.f90` - Interface definitions for external functions

### GUI Components
- `UR_gtk-routines.f90` - GTK widget handling
- `gui_UR_functions.f90` - GUI function callbacks
- `URGladesys.f90` - Glade UI system integration
- `UR2.glade` - GTK UI definition file (compiled via gresources)

### Calculation Engine
- `Top.f90` / `Top_submod.f90` - Top-level calculation interfaces
- `Symbol1.f90` / `Symbol1_submod.f90` - Symbolic equation handling
- `Rechw1.f90` / `Rechw1_submod.f90` - Calculation routines (Part 1)
- `Rechw2.f90` / `Rechw2_submod.f90` - Calculation routines (Part 2)
- `Num1.f90` / `Num1_submod.f90` - Numerical methods
- `TopoEqs.f90` - Topological equation solver

### Monte Carlo Simulation
- `MCCalc.F90` - Main MC calculation
- `MCsubs.f90` - MC subroutines
- `MCtables.f90` - MC result tables
- `MCsingRun.f90` - Single MC run
- `MCPrepCovars.f90` - MC covariance preparation

### Linear Unfolding & Fitting
- `Linf.F90` / `Linfg1.F90` / `Linmod1.F90` - Linear inversion/fitting
- `LsqlinCov2.f90` - Linear least squares with covariance
- `Kalfit1.f90` / `Kalfit1_submod.f90` - Kalman filter fitting
- `LevMarGavin.f90` - Levenberg-Marquardt optimization

### Utility Modules
- `fparserW.f90` - Expression parser
- `fileIO.f90` - File input/output
- `ProRead.f90` / `ProSave.f90` - Project file handling (.txp format)
- `ProRead_CSV.F90` / `ProSave_CSV.F90` - CSV data import/export
- `Batest.f90` / `Batch_proc.f90` - Batch testing and processing
- `Translate_mod.f90` / `TranslateUR.f90` - Internationalization

### `_submod` Convention
Files ending in `_submod.f90` contain submodules that extend their parent module. Example:
- `Top.f90` defines the main `top` module
- `Top_submod.f90` contains additional submodules used by `top`

## Build System

### CMake Configuration
- **Minimum CMake:** 3.20
- **Fortran Standard:** f2018
- **Build Types:** Debug (default) or Release
- **Optimization:** `-O2` (default) or `-O3 -ffast-math` (ENABLE_OPTIMIZATION=ON)

### Dependencies
- **gtk-fortran** - External project, fetched from GitHub
- **GTK3** - GUI toolkit
- **PLplot** - Plotting library (Fortran bindings required)
- **LAPACK** - Linear algebra

### Build Commands
```bash
# Standard build
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j$(nproc)

# Debug build
cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build -j$(nproc)

# With docs
cmake -B build -DBUILD_DOCS=ON
cmake --build build -j$(nproc)

# Install
cmake --install build --prefix=install
```

### File Extension Convention
- `.F90` - Fixed-form Fortran (9 files: main program, CSV handlers, linear algebra)
- `.f90` - Free-form Fortran (most files)

## Testing

### Running Tests
```bash
# GUI batch test
./bin/UncertRadio run_tests

# Docker test
docker run --rm uncertradio-release \
  bash -c "/app/install/bin/UncertRadio run_tests"
```

### Test Data
- **Examples:** `examples/en/` and `examples/de/` (~70 project files)
- **Batch list:** `BatListRef_v06.txt`
- **Expected results:** Embedded in project description tabs

## Project File Format (.txp)
- JSON-based format for storing measurement procedures
- Contains input parameters, equations, and expected results
- Created/loaded via GUI or programmatically

## Documentation
- **Sphinx-based** with MyST parser
- **Location:** `docs/`
- **Build:** `python docs/make_docs.py` or via CMake
- **Online:** https://openbfs.github.io/UncertRadio/

## Internationalization
- **Languages:** English (en), German (de), French (fr)
- **Translation files:** `translations/*/`
- **Documentation translations:** `docs/locale/*/`

## File Paths & Data Locations
- **Data path:** `~/.local/share/UncertRadio/`
- **Config path:** `~/.config/UncertRadio/`
- **Log path:** `~/.local/state/UncertRadio/`
- **Examples:** `share/UncertRadio/examples/`
- **Documentation:** `share/doc/UncertRadio/`

## Important Global Variables
Key variables in `ur_general_globals` module:
- `wpunix` - Boolean, True on Unix systems
- `dir_sep` - Path separator (`/` or `\`)
- `sDecimalPoint` - Decimal separator (`.`, `,`)
- `automode` - True when running in batch mode
- `batest_on` - True during batch tests
- `data_path` - Path to data files
- `example_path` - Path to examples

## Development Notes

### Fortran Conventions
- **Implicit none** everywhere
- **Free-form** source code (`.f90`)
- **Fortran 2018** standard
- **Module-based** architecture with explicit interfaces
- **Real precision:** Defined by `rn` constant in `UR_types`

### GTK Integration
- Uses **gtk-fortran** bindings
- UI defined in **UR2.glade**
- Resources compiled via **glib-compile-resources**
- Icons embedded as **gresources**

### Common Pitfalls
1. **gtk-fortran dependency** must be built before main project
2. **PLplot** requires Fortran bindings and Cairo driver
3. **Numerical precision** issues with `-ffast-math` on some architectures
4. **Module dependencies** are complex - trace carefully before changes
5. **Global state** in `UR_globals` - avoid adding more global variables

### Code Organization
- Calculation logic is separate from GUI handling
- Batch processing uses same code path as GUI, but with `automode=true`
- Tests run via CLI with `run_tests` argument
- Documentation is built separately but included in releases

## Key Interfaces
From `Top.f90`:
- `idpt()` - Get widget pointer from Glade ID
- `mdcalc()` - Main calculation entry point
- `wrstatusbar()` - Write to status bar
- `finditems()` - Find GUI items by name

## Citation
```bibtex
@article{kanisch2026uncertRadio,
  title={UncertRadio: Software for determining characteristic limits in accordance to DIN EN ISO 11929 for radioactivity measurements},
  author={Kanisch, Günter and Ober, Florian and Aust, Marc-Oliver},
  journal={Journal of Open Source Software},
  volume={11},
  number={121},
  pages={7398},
  year={2026},
  publisher={Open Journals}
}
```
