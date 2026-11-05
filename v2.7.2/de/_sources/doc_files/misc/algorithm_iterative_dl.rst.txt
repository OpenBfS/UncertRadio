Algorithm for iterative numerical calculation of the Detection limit
--------------------------------------------------------------------


.. code-block:: fortran

    ! Starting value
    y_hat_0 = 2.0 * y_star

    i = 0

    ! Start of iteration loop:
    do
        i = i + 1

        ! Let the gross counting rate be the 8th element of the parameter array *p*;
        ! Gross counting rate and its uncertainty, p(8) and u(8), respectively, where
        ! R_0A is the background counting rate of the analyte and
        ! R_NTimp is the blank component of the counting rate due to contributions from further
        ! sources such as tracer impurities.
        ! z1 and z2 are those by UncertRadio estimated parameters in y = z1 * R_n + z2.

        p(8)(i) = (y_hat(i - 1) - z2) / z1 + R_NTimp + R_0A

        u(8)(i) = sqrt(p(8)(i) / t)

        y_hat(i) = y_star + k(1 - beta) * UncPropa(..., u(8)(i), ...)

        if (abs(y_hat(i) - y_hat(i - 1)) / y_hat(i) < 0.0001) exit ! Iteration terminated.

    end do

    ! End of iteration loop: the last obtained value y_hat(i) is the final value of the Detection limit.