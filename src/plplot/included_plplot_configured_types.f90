    ! Specify configured Fortran types (currently only private_plflt) to
    ! be used by the fortran binding.

    ! Configured by CMake depending on the floating-point precision
    ! adopted for the core C PLplot library.
    integer, parameter :: private_plflt  = c_double
