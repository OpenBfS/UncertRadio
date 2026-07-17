Algorithm for numerical calculation of the combined uncertainty
---------------------------------------------------------------

**Function subprogram UncPropa(p,u):**

.. code-block:: fortran

    ! Calculations in double precision

    Variance = 0.0

    ! Calculations within a DO-loop:

    ! Begin:

    do i = 1, n

        ! Define the perturbation
        Delta_p_i = 10.0**(-6) * p(i)

        ! Calculate function values
        fv1 = RESULT(..., p(i), ...)
        fv2 = RESULT(..., p(i) + Delta_p_i, ...)

        ! Sensitivity factor = partial derivative with respect to the parameter p_i:
        partial_derivative = (fv2 - fv1) / Delta_p_i

        ! Variance contribution of p_i:
        var_i = (partial_derivative * u(i))**2

        Variance = Variance + var_i

    end do

    ! End of the DO-loop:

**Uncertainty budget:** divide all values var(i) by the value
*Variance*: these are the relative contributions of the parameter I to
the variance!

:math:`UncPropa = \sqrt{Varianz}`: This is the value of the **combined
standard uncertainty**