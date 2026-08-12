!--------------------------------------------------------------------------------------------------!
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
!--------------------------------------------------------------------------------------------------!
module UR_tests

    ! A collection of tests for UncertRadio

    implicit none
    !----------------------------------------------------------------------------------------------!
    private
    !
    public :: run_tests
    !----------------------------------------------------------------------------------------------!

contains
    subroutine run_tests()
        implicit none
        !------------------------------------------------------------------------------------------!
        write(*,'(2X,A)') "Running UncertRadio tests"

        call test_sym_eigensolve()

        call test_fd()

        call test_mtxhst()

        call test_write_text_file()

        call test_read_config_char()

        call test_read_config_logical()

        call test_read_config_int()

        call test_read_config_real()

        call test_str_replace()

        call test_color_themes()

        call test_translations()

        call test_format_nu_str()

        call test_ucase()

        call test_lowercase()

        call test_Batch_no_gui()

        write(*,'(2X,A)') "All tests done"

    end subroutine


    !=====================================================================
    !  FD‑function tests (moved from Rechw1_submod.f90)
    !=====================================================================
    subroutine test_fd()
        ! Test the parser/evaluator for the FD() function.
        use UR_params,           only: EPS1MIN
        use UR_types,            only: rn
        use UR_gtk_window,       only: charv
        use UR_Gleich_globals,   only: ifehl
        use fparser,             only: initf, parsef, evalf, EvalErrMsg
        use UR_perror,           only: ifehlp
        use ur_general_globals,  only: fd_found
        use iso_c_binding,       only: c_null_char

        implicit none
        integer                 :: errors
        integer, parameter      :: N = 5
        real(rn)                :: t4, t2, lam, t4minust2, fakt, expect
        real(rn)                :: werte(N)
        type(charv)             :: eqs(1), symb(N)

        errors = 0

        !-----------------------------------------------------------------
        !  initialise data (identical to the original routine)
        !-----------------------------------------------------------------
        t4        = 1.30320E+06_rn
        t2        = 1.20960E+06_rn
        lam       = 1.600665760E-06_rn
        t4minust2 = t4 - t2

        symb(1)%s = 'T4'
        symb(2)%s = 'T2'
        symb(3)%s = 'LAM'
        symb(4)%s = 'T4MINUST2'
        symb(5)%s = 'NULL'

        werte(1) = t4
        werte(2) = t2
        werte(3) = lam
        werte(4) = t4minust2
        werte(5) = 1.E-14_rn

        fd_found(1) = .true.

        ! Test case 1 FD(T4MINUST2, 0., LAM)
        eqs(1)%s = 'FD(T4MINUST2, 0., LAM)'

        call initf(1)
        call parsef(1, eqs(1)%s, symb)

        fakt   = evalf(1, werte)
        expect = exp(-lam*(t4-t2))

        if (abs(fakt - expect) > EPS1MIN .or. ifehlp /= 0) then
            errors = errors + 1
            write(*,'(4X,A)')'FD case 1 mismatch: got ',sngl(fakt), &
                            ' expected ',sngl(expect)

        end if


        ! Test case 2 FD(T4-T2, 0., LAM)
        eqs(1)%s = 'FD(T4-T2, 0., LAM)'

        call initf(1)
        call parsef(1, eqs(1)%s, symb)

        fakt = evalf(1, werte)

        if (abs(fakt - expect) > EPS1MIN .or. ifehlp /= 0) then
            errors = errors + 1
            write(*,'(4X,A)') 'FD case 2 mismatch: got ', sngl(fakt), &
                              ' expected ', sngl(expect)

        end if

        ! Test case 3 FD(T4-T2, 0, LAM)   (integer zero)
        eqs(1)%s = 'FD(T4-T2, 0, LAM)'

        call initf(1)
        call parsef(1, eqs(1)%s, symb)

        fakt = evalf(1, werte)

        if (abs(fakt - expect) > EPS1MIN .or. ifehlp /= 0) then
            errors = errors + 1
            write(*,'(4X,A)') 'FD case 3 mismatch: got ', sngl(fakt), &
                             ' expected ', sngl(expect)
        end if


        !  Report
        if (errors == 0) then
            write(*,'(4X,A)') "FD function: no errors"
        else
            write(*,'(4X,A,I0,A)') "FD function: Warning, found ", errors, " error(s)"
        end if

        ! Do not set the global error flag – it would interfere with other tests.
        ifehl = 0
    end subroutine test_fd

    subroutine test_sym_eigensolve()
        !------------------------------------------------------------------------------------------!
        use UR_types, only: rn
        use Num1, only: sym_eigensolve
        implicit none
        !------------------------------------------------------------------------------------------!
        integer, parameter :: n = 3
        real(rn) :: a(n,n), eigenv(n), Av(n), err
        real(rn), parameter :: tol = sqrt(epsilon(1._rn))
        real(rn), parameter :: aorig(n,n) = reshape(&
            [1.0_rn, 0.5_rn, 0.2_rn, &
             0.5_rn, 1.0_rn, 0.1_rn, &
             0.2_rn, 0.1_rn, 1.0_rn], [n,n])
        integer  :: ier, j, errors, lda
        !------------------------------------------------------------------------------------------!

        errors = 0

        ! Initialize matrix
        a = aorig

        lda = n
        call sym_eigensolve(n, a, lda, eigenv, ier)

        ! Check ier
        if (ier /= 0) then
            write(*,'(A,I0)') "sym_eigensolve returned ier = ", ier
            errors = errors + 1
        end if

        ! Verify A*v = lambda*v
        do j = 1, n
            Av = matmul(aorig, a(:,j)) - eigenv(j) * a(:,j)
            err = norm2(Av)
            if (err > tol) then
                write(*,'(A,I0,ES14.6)') "  FAIL: residual |Av-lambda*v| ", j, " = ", err
                errors = errors + 1
            end if
        end do

        ! Check eigenvalues are ascending
        do j = 1, n-1
            if (eigenv(j) > eigenv(j+1) + sqrt(epsilon(1._rn))) then
                write(*,'(A,I0)') "    FAIL: eigenvalues not ascending at position ", j
                errors = errors + 1
            end if
        end do

        if (errors == 0) then
            write(*,'(4X,A)') "sym_eigensolve: no errors"
        else
            write(*, '(4X, A,I0,A)') "sym_eigensolve: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_sym_eigensolve


    subroutine test_mtxhst()
        !------------------------------------------------------------------------------------------!
        use UR_types, only: rn
        use UR_params, only: PI, EPS1MIN
        use brandt, only: mtxhst
        implicit none
        !------------------------------------------------------------------------------------------!

        integer, parameter :: n = 10
        integer  :: lp, l, i
        real(rn) :: v(n), up, b, c(n)

        real(rn), parameter :: results(n) = &
            [0.31830988618379069_rn, &
             0.63661977236758138_rn, &
             0.95492965855137202_rn, &
             1.2732395447351628_rn, &
             374.96354436109192_rn, &
             1920.5322358084820_rn, &
             2613.6864038721087_rn, &
             3413.4307039630339_rn, &
             4319.7651360812579_rn, &
             5332.6897002267806_rn]
        integer                   :: errors
        !------------------------------------------------------------------------------------------!

        errors = 0
        ! Initialize variables
        lp = 5
        l = 6
        up = 2.23_rn
        b = 0.58288_rn

        ! Initialize vectors v and c
        do i = 1, n
            v(i) = real(i, rn)**2 / PI
            c(i) = real(i, rn) / PI
        end do

        call mtxhst(v, up, b, c, n, lp, l)

        do i=1, 10
            if (abs(c(i) + EPS1MIN - results(i)) > EPS1MIN) then
                errors = errors + 1
            end if
        end do

        if (errors == 0) then
            write(*,'(4X,A)') "mtxhst: no errors"
        else
            write(*, '(4X, A,I0,A)') "mtxhst: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_mtxhst

    subroutine test_str_replace()
        use chf, only: StrReplace
        implicit none

        character(:), allocatable :: str
        character(len=16)         :: strold, strnew
        logical                   :: all_occur, is_variable
        integer                   :: errors

        errors = 0

        ! Test case 1: Replace a simple string but only the first one
        str = 'Hello, World! World!'
        strold = 'World'
        strnew = 'Universe'
        all_occur = .false.
        is_variable = .false.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, Universe! World!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 1: should be: 'Hello, Universe!', got: '" // str // "'"
        end if

        ! Test case 2: Replace a simple string but now all
        str = 'Hello, World! World!'
        strold = 'World'
        strnew = 'Universe'
        all_occur = .true.
        is_variable = .false.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, Universe! Universe!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 2: should be: 'Hello, Universe! Universe!', got: '" // str // "'"
        end if

        ! Test case 3: Replace a string with is_variable true but no symbol
        str = 'Hello, World!'
        strold = 'World'
        strnew = 'Universe'
        all_occur = .false.
        is_variable = .true.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, World!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 3: should be: 'Hello, World!', got: '" // str // "'"
        end if

        ! Test case 4: Replace a string with is_variable true and realy could be a symbol
        str = 'Hello, + World'
        strold = 'World'
        strnew = 'Universe'
        all_occur = .false.
        is_variable = .true.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, + Universe') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 4: should be: 'Hello, + Universe', got: '" // str // "'"
        end if

        ! Test case 5: Replace a mathematical operator
        str = 'Hello, + World'
        strold = '+'
        strnew = 'new'
        all_occur = .false.
        is_variable = .false.

        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= 'Hello, new World') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 5: should be: 'Hello, new World', got: '" // str // "'"
        end if

        ! Test case 6: Replace a special character everywhere
        str = '/Hello/Path/to/a/new/World/'
        strold = '/'
        strnew = '\'
        all_occur = .true.
        is_variable = .false.
        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= '\Hello\Path\to\a\new\World\') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 6: should be: '\Hello\Path\to\a\new\World\', got: '" // str // "'"
        end if

        ! Test case 7: Replace a special character with the same
        str = '/Hello/Path/to/a/new/World/'
        strold = '/'
        strnew = '/'
        all_occur = .true.
        is_variable = .false.
        call StrReplace(str, strold, strnew, all_occur, is_variable)
        if (str /= '/Hello/Path/to/a/new/World/') then
            errors = errors + 1
            write(*,'(4X,A)') "Error Test 7: should be: '/Hello/Path/to/a/new/World/', got: '" // str // "'"
        end if

        ! Report
        if (errors == 0) then
            write(*,'(4X,A)') "StrReplace: no errors"
        else
            write(*, '(4X, A,I0,A)') "StrReplace: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_str_replace

    !---------------------------------------------------------------------------------------------!


    subroutine test_write_text_file()

        use file_io, only: write_text_file
        use chf,     only: flfu
        implicit none

        character(:), allocatable :: filename, out_text1, out_text2
        character(32)             :: tmp_string
        integer                   :: errors, nio, iostat
        !-----------------------------------------------------------------------------------------!
        errors = 0
        filename = 'Testfileä.txt'
        out_text1 = 'first Täxt'
        out_text2 = 'second T~xt'

        ! write the first text to a 'new' file,
        call write_text_file(out_text1, filename, status='new', utf8_filename=.true.)
        ! now write the second text to the same file and close it afterwards
        call write_text_file(out_text2, filename, utf8_filename=.true., status='close')

        ! read the file and check the content
        open(file=flfu(filename), newunit=nio, iostat=iostat, status='old', action='read')

        if (iostat /= 0) then
            errors = errors + 1
        else
            read(nio,'(A)') tmp_string
            ! test if input == output
            if (tmp_string /= out_text1) then
                errors = errors + 1
                write(*,'(4X,A)') "Error: input /= output" // trim(tmp_string) // ", " // out_text1
            end if

            ! read the next line and check the content
            read(nio,'(A)') tmp_string
            ! test if input == output
            if (tmp_string /= out_text2) then
                errors = errors + 1
                write(*,'(4X,A)') "Error: input /= output" // trim(tmp_string) // ", " // out_text2
            end if
            close(nio)

            ! again write to the file and force to create a new one
            call write_text_file(out_text1 // out_text2, filename, status='new', utf8_filename=.true.)
            call write_text_file(out_text1 // out_text2, filename, status='close')

            ! read the file and check the content
            open(file=flfu(filename), newunit=nio, iostat=iostat, status='old', action='read')

            if (iostat /= 0 ) then
                errors = errors + 1
            else
                read(nio,'(A)') tmp_string
                close(nio)

                if (tmp_string /= out_text1 // out_text2) then
                    errors = errors + 1
                    write(*,'(4X,A)') "Error: input /= output: " // trim(tmp_string) // ", " // &
                        out_text1 // out_text2
                end if

            end if

            ! now remove the testfile:
            open(file=flfu(filename), newunit=nio, status='old')
            close(nio, status="delete")
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "write_text_file: no errors"
        else
            write(*, '(4X, A,I0,A)') "write_text_file: Warning, found ", errors, " error(s)"
        end if
    end subroutine test_write_text_file

    !---------------------------------------------------------------------------------------------!
    !  read_config tests (file_io module)
    !---------------------------------------------------------------------------------------------!

    subroutine test_read_config_char()
        ! Test read_config with allocatable character target.
        use file_io, only: read_config
        use chf,     only: flfu
        implicit none

        character(len=:), allocatable :: tmp_str
        character(len=64)             :: cfg_file
        integer                       :: errors, nio, iostat

        errors = 0
        cfg_file = 'test_cfg_char.dat'

        ! Create temp config
        open(newunit=nio, file=flfu(cfg_file), iostat=iostat, status='new', action='write')
        write(nio, '(A)') 'CharKey=some value here'
        close(nio)

        ! Test 1: Read existing key
        call read_config('CharKey', tmp_str, cfg_file, break=.true.)
        if (.not. allocated(tmp_str)) then
            errors = errors + 1
            write(*,'(4X,A)') "Char test 1 failed: tmp_str is unallocated"
        else if (tmp_str /= 'some value here') then
            errors = errors + 1
            write(*,'(4X,A)') "Char test 1 failed: expected 'some value here', got '" // trim(tmp_str) // "'"
        end if
        if (allocated(tmp_str)) deallocate(tmp_str)

        ! Test 2: Missing key with break=.true. -> stays unallocated
        call read_config('MissingKey', tmp_str, cfg_file, break=.true.)
        if (allocated(tmp_str)) then
            errors = errors + 1
            write(*,'(4X,A)') "Char test 2 failed: tmp_str should be unallocated for missing key"
            deallocate(tmp_str)
        end if

        ! Cleanup
        open(newunit=nio, file=flfu(cfg_file), status='old')
        close(nio, status='delete')

        if (errors == 0) then
            write(*,'(4X,A)') "read_config (char): no errors"
        else
            write(*,'(4X, A,I0,A)') "read_config (char): Warning, found ", errors, " error(s)"
        end if
    end subroutine test_read_config_char

    subroutine test_read_config_logical()
        ! Test read_config with logical target.
        use file_io, only: read_config
        use chf,     only: flfu
        implicit none

        logical                       :: val
        character(len=64)             :: cfg_file
        integer                       :: errors, nio

        errors = 0
        cfg_file = 'test_cfg_logical.dat'

        ! Create temp config
        open(newunit=nio, file=flfu(cfg_file), status='new', action='write')
        write(nio, '(A)') 'LogTrue=.true.'
        write(nio, '(A)') 'LogFalse=.false.'
        write(nio, '(A)') 'LogTrueUpper=.TRUE.'
        write(nio, '(A)') 'LogFalseUpper=.FALSE.'
        close(nio)

        ! Test 1: .true.
        val = .false.
        call read_config('LogTrue', val, cfg_file, break=.true.)
        if (.not. val) then
            errors = errors + 1
            write(*,'(4X,A)') "Log test 1 failed: expected .true., got .false."
        end if

        ! Test 2: .false.
        val = .true.
        call read_config('LogFalse', val, cfg_file, break=.true.)
        if (val) then
            errors = errors + 1
            write(*,'(4X,A)') "Log test 2 failed: expected .false., got .true."
        end if

        ! Test 3: .TRUE. (uppercase)
        val = .false.
        call read_config('LogTrueUpper', val, cfg_file, break=.true.)
        if (.not. val) then
            errors = errors + 1
            write(*,'(4X,A)') "Log test 3 failed: expected .true., got .false."
        end if

        ! Test 4: .FALSE. (uppercase)
        val = .true.
        call read_config('LogFalseUpper', val, cfg_file, break=.true.)
        if (val) then
            errors = errors + 1
            write(*,'(4X,A)') "Log test 4 failed: expected .false., got .true."
        end if

        ! Test 5: Missing key with break=.true. -> default preserved
        val = .true.
        call read_config('MissingKey', val, cfg_file, break=.true.)
        if (.not. val) then
            errors = errors + 1
            write(*,'(4X,A)') "Log test 5 failed: default .true. not preserved for missing key"
        end if

        ! Cleanup
        open(newunit=nio, file=flfu(cfg_file), status='old')
        close(nio, status='delete')

        if (errors == 0) then
            write(*,'(4X,A)') "read_config (logical): no errors"
        else
            write(*,'(4X, A,I0,A)') "read_config (logical): Warning, found ", errors, " error(s)"
        end if
    end subroutine test_read_config_logical

    subroutine test_read_config_int()
        ! Test read_config with integer target.
        use file_io, only: read_config
        use chf,     only: flfu
        implicit none

        integer                       :: val
        character(len=64)             :: cfg_file
        integer                       :: errors, nio

        errors = 0
        cfg_file = 'test_cfg_int.dat'

        ! Create temp config
        open(newunit=nio, file=flfu(cfg_file), status='new', action='write')
        write(nio, '(A)') 'IntKey=42'
        close(nio)

        ! Test 1: Read existing key
        val = 0
        call read_config('IntKey', val, cfg_file, break=.true.)
        if (val /= 42) then
            errors = errors + 1
            write(*,'(4X,A,I0)') "Int test 1 failed: expected 42, got ", val
        end if

        ! Test 2: Missing key with break=.true. -> default preserved
        val = 999
        call read_config('MissingKey', val, cfg_file, break=.true.)
        if (val /= 999) then
            errors = errors + 1
            write(*,'(4X,A,I0)') "Int test 2 failed: default 999 not preserved, got ", val
        end if

        ! Cleanup
        open(newunit=nio, file=flfu(cfg_file), status='old')
        close(nio, status='delete')

        if (errors == 0) then
            write(*,'(4X,A)') "read_config (int): no errors"
        else
            write(*,'(4X, A,I0,A)') "read_config (int): Warning, found ", errors, " error(s)"
        end if
    end subroutine test_read_config_int

    subroutine test_read_config_real()
        ! Test read_config with real target.
        use UR_params, only: EPS1MIN
        use UR_types, only: rn
        use file_io, only: read_config
        use chf,     only: flfu
        implicit none

        real(rn)                      :: val
        character(len=64)             :: cfg_file
        integer                       :: errors, nio

        errors = 0
        cfg_file = 'test_cfg_real.dat'

        ! Create temp config
        open(newunit=nio, file=flfu(cfg_file), status='new', action='write')
        write(nio, '(A)') 'RealKey=3.14159265'
        close(nio)

        ! Test 1: Read existing key
        val = 0.0_rn
        call read_config('RealKey', val, cfg_file, break=.true.)
        if (abs(val - 3.14159265_rn) > EPS1MIN) then
            errors = errors + 1
            write(*,'(4X,A,ES20.12)') "Real test 1 failed: expected 3.14159265, got ", val
        end if

        ! Test 2: Missing key with break=.true. -> default preserved
        val = 999.0_rn
        call read_config('MissingKey', val, cfg_file, break=.true.)
        if (abs(val - 999.0_rn) > EPS1MIN) then
            errors = errors + 1
            write(*,'(4X,A,ES20.12)') "Real test 2 failed: default 999.0 not preserved, got ", val
        end if

        ! Cleanup
        open(newunit=nio, file=flfu(cfg_file), status='old')
        close(nio, status='delete')

        if (errors == 0) then
            write(*,'(4X,A)') "read_config (real): no errors"
        else
            write(*,'(4X, A,I0,A)') "read_config (real): Warning, found ", errors, " error(s)"
        end if
    end subroutine test_read_config_real

    !---------------------------------------------------------------------------------------------!

    subroutine test_color_themes()
        use color_theme
        implicit none

        ! Test variables
        character(len=7)  :: color_result
        character(len=16) :: theme_name_result
        integer           :: errors

        errors = 0

        ! Test 1: Default color theme
        call set_color_theme("default")
        color_result = get_color_string("entry_bg")
        if (color_result /= "#FFFFEC") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 1 Failed: Expected #FFFFEC, got ", color_result
        end if

        ! Test 2: Contrast color theme
        call set_color_theme("contrast")
        color_result = get_color_string("entry_bg")
        if (color_result /= "#000000") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 2 Failed: Expected #000000, got ", color_result
        end if

        ! Test 3: Get theme name
        theme_name_result = get_theme_name()
        if (theme_name_result /= "contrast") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 3 Failed: Expected contrast, got ", theme_name_result
        end if

        ! Test 4: Invalid theme name
        call set_color_theme("invalid_theme")  ! Should print an error message
        theme_name_result = get_theme_name()
        if (theme_name_result /= "contrast") then
            errors = errors + 1
            write(*,'(4X,A)')"Test 4 Failed: Expected contrast, got ", theme_name_result
        end if

        ! Test 5: Get unknown color key
        color_result = get_color_string("unknown_key")
        if (color_result /= "       ") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 5 Failed: Expected blank string, got ", color_result
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "color_modes: no errors"
        else
            write(*, '(4X, A,I0,A)') "color_modes: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_color_themes
    !---------------------------------------------------------------------------------------------!

    subroutine test_translations()
        use translation_module, only: set_language, get_language, T => get_translation
        use ur_general_globals, only: data_path, dir_sep
        implicit none
        ! Test variables
        character(:), allocatable :: key, translation
        character(len=2)          :: language
        integer :: errors = 0

        key = 'Equations'

        ! Test 1: Set language with a valid file
        language = 'de'
        call set_language(language, data_path // 'translations' // dir_sep)  ! Adjust the path as necessary

        if (get_language() /= language) then
            errors= errors + 1
            write(*,'(4X,A)') "Test 1 - Set Language expected 'de' but got ", trim(get_language())
        end if

        ! Test 2: Get translation for a known key
        key = "Equations"
        translation = T(key)

        if (trim(translation) /= "Gleichungen") then
            errors = errors + 1
            write(*,'(4X,A)') "Test 2 - expected 'Gleichungen' but got '" // trim(translation) // "'"
        end if

        ! Test 3: Get translation for an unknown key
        key = "someUnknownKey"
        translation = T(key)
        if (trim(translation) /= key)  then ! Should return the key itself
            errors = errors + 1
            write(*,'(4X,A)') "Test 3 - expected 'Equation' but got '" // trim(translation) // "'"
        end if

        ! Test 4: Set language with a non-existent translations root path
        language = 'fr'
        call set_language(language, data_path // 'not_there' // dir_sep)  ! Adjust the path as necessary
        ! Expect an error message to be printed
        ! Check if selected_language remains unchanged

        if (get_language() == language) then
            errors = errors + 1
            write(*,'(4X,A)') "Test 4 - Set Language should not be " // language
        end if

        ! Test 5: Check if the optional value produces upper case first letter
        !
        key = "the first"
        translation = T(key, .true.)
        if (trim(translation) /= "Die erste")  then ! The tranlation has lower first character
            errors = errors + 1
            write(*,'(4X,A)') "Test 5 - expected 'Die erste' but got '" // trim(translation) // "'"
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "translations: no errors"
        else
            write(*, '(4X, A,I0,A)') "translations: Warning, found ", errors, " error(s)"
        end if


    end subroutine test_translations
    !---------------------------------------------------------------------------------------------!

    subroutine test_format_nu_str()

        use ur_general_globals, only: sDecimalPoint
        use CHF, only: FormatNumStr

        character(len=64) :: test_input
        character(len=64) :: expected_output
        integer :: errors = 0


        sDecimalPoint = '.'
        ! Test case 1: Basic number with trailing zeros
        test_input = "123.45000"
        expected_output = "123.450"

        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            print *, "Test 1 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        ! Test case 2: Number with exponent and trailing zeros
        test_input = "1.23000E+10"
        expected_output = "1.230E+10"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)') "Test 2 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        ! Test case 3: Number with no trailing zeros
        test_input = "123.45"
        expected_output = "123.45"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)')  "Test 3 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        ! Test case 4: Empty string
        test_input = ""
        expected_output = ""
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)') "Test 4 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if
        ! Test case 5: Number with comma as decimal point
        sDecimalPoint = ','
        test_input = "123,45000"
        expected_output = "123,450"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            print *, "Test 5 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        sDecimalPoint = '.'
        ! Test case 6: Number with exponent and no trailing zeros
        test_input = "1.23E+10"
        expected_output = "1.23E+10"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)') "Test 6 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        ! Test case 7: Number with negative exponent and trailing zeros
        test_input = "1.23000E-10"
        expected_output = "1.230E-10"
        if (trim(FormatNumStr(test_input, sDecimalPoint)) /= trim(expected_output)) then
            errors = errors + 1
            write(*,'(4X,A)')  "Test 6 failed for input: " // trim(test_input) // &
                ", Expected: " // trim(expected_output) // ", is: : " // &
                trim(FormatNumStr(test_input, sDecimalPoint))
        end if

        if (errors == 0) then
            write(*, '(4X,A)') "FormatNumStr: no errors"
        else
            write(*, '(4X, A,I0,A)') "FormatNumStr: Warning, found ", errors, " error(s)"
        end if

    end subroutine test_format_nu_str


    subroutine test_ucase()

        use chf, only: ucase
        implicit none

        character(:), allocatable :: input, output
        integer                   :: errors
        !-----------------------------------------------------------------------------------------!
        errors = 0

        ! Test case 1: single character
        input = 'a'
        output = ucase(input)
        if (output /= 'A') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 2: multiple characters
        input = 'Hello, World!'
        output = ucase(input)
        if (output /= 'HELLO, WORLD!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 3: include special characters
        input = 'äöüÄÖÜ and some more [´'
        output = ucase(input)
        if (output /= 'äöüÄÖÜ AND SOME MORE [´') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 4: empty string
        input = ''
        output = ucase(input)
        if (output /= '') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "ucase: no errors"
        else
            write(*, '(4X, A,I0,A)') "ucase: Warning, found ", errors, " error(s)"
        end if
    end subroutine test_ucase


    subroutine test_lowercase()

        use chf, only: lowercase
        implicit none

        character(:), allocatable :: input, output
        integer                   :: errors
        !-----------------------------------------------------------------------------------------!
        errors = 0

        ! Test case 1: single uppercase letter
        input = 'A'
        output = lowercase(input)
        if (output /= 'a') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 2: single lowercase letter
        input = 'a'
        output = lowercase(input)
        if (output /= 'a') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 3: mixed case string
        input = 'Hello, World!'
        output = lowercase(input)
        if (output /= 'hello, world!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 4: all uppercase string
        input = 'HELLO, WORLD!'
        output = lowercase(input)
        if (output /= 'hello, world!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 5: all lowercase string
        input = 'hello, world!'
        output = lowercase(input)
        if (output /= 'hello, world!') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        ! Test case 6: string with special characters
        input = 'Hello, World! 123Ä[]'
        output = lowercase(input)
        if (output /= 'hello, world! 123Ä[]') then
            errors = errors + 1
            write(*,'(4X,A)') "Error: input /= output: " // trim(input) // ", " // output
        end if

        if (errors == 0) then
            write(*,'(4X,A)') "lowercase: no errors"
        else
            write(*, '(4X, A,I0,A)') "lowercase: Warning, found ", errors, " error(s)"
        end if
    end subroutine test_lowercase

end module UR_tests
