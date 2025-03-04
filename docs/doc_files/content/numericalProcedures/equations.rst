Structure of equations
^^^^^^^^^^^^^^^^^^^^^^

The conditional equations defining the model for calculating the value
of the **output quantity/quantities y** are written into a text field of
the program. To improve the readability of equations auxiliary
quantities may be inferred. The **formula symbols** are extracted
automatically from the equations and are transferred into a table where
they individually can be complemented by a **unit** and a **meaning**.
After input of values and uncertainties of primarily measured quantities
a "Function parser" is used for interpretation of the equations and
calculation of the value and uncertainty of the output quantity **y**.

For the process of editing the equations it is important to infer a
quantity representing a **net counting rate,** called :math:`\mathbf{R_n}` in this
help file, on which the output quantity depends linearly:

.. math:: y = R_{n} \cdot F_{L} + F_{C}
    :label: struct_eq1

In equation :eq:`struct_eq1` the proportionality factor FL represents the procedure dependent
calibration factor. FC considers further interference contributions, as
e.g. one originating from the addition of a tracer activity before
beginning with the radiochemical analysis.

The net counting rate :math:`R_n` shall be understood as that net counting rate
(more precise: procedure dependent net counting rate) from which all
those contributions to the gross counting rate :math:`R_g` which are not
derived from the source contribution itself have been subtracted. The
latter are not only the detector-related background :math:`R_0` but also blank
contributions :math:`R_{bl}` and, if applying a tracer solution, additional blank
contributions due to impurities in the tracer solution. Additionally, a
calculated contribution :math:`R_{int}` may be included due to interference by
another radionuclide. **As an example, the procedure dependent net
counting rate may then be:**

.. math:: R_n = R_g - R_0 - R_{bl} - R_{int}
    :label: struct_eq2

The constants :math:`F_L` and :math:`F_C` can be easily determined within the program
for arbitrary types of equations, if these depend linearly on the net
counting rate. This representation allows UncertRadio to solve Eq. :eq:`struct_eq1`
for a modified net counting rate value if the output quantity value were
changed to y':

.. math:: R_{n}' = (y' - F_C) / F_L
    :label: struct_eq3

Similarly, the equation for a net counting rate :math:`R_n` can be expressed
more generally as a linear function of the gross count rate :math:`R_g`:

.. math:: R_n = F_B \cdot R_g - R_{0,total}
    :label: struct_eq4

In most cases the factor :math:`F_B` is equal to one; but FB may also differ
from one. R0total is the sum of background contributions to be
subtracted from the gross counting rate; see Eq. :eq:`struct_eq2`. At the beginning
of computations, UncertRadio determines the values of :math:`F_B` and from this
the fixed value R0total:

.. math:: R_{0,total} = F_B \cdot R_b - R_n
    :label: struct_eq5


A net counting rate value Rn' obtained by iterations within the
detection limit calculations, is associated with a modified value of the
gross counting rate:

.. math:: R_{b}' = (R_{n}' + R_{0,total}) / F_B
    :label: struct_eq6

.. tip::

   All counting rates in equation :eq:`struct_eq2` may also appear as to be multiplied
   with factors g, associated with uncertainties, as e.g.

   .. math:: R_n = g_b \cdot R_b - g_0 \cdot R_0 - g_{bl} \cdot R_{bl} - g_{int} \cdot R_{int}


Non-linear dependence
"""""""""""""""""""""

There may exist cases in which the dependence between output quantity
and net counting rate, or, when using linear unfolding, between output
quantity and the activity, is not linear. Consequently, the values :math:`F_C`
und :math:`F_L` in Eq. :eq:`struct_eq1` are only approximate ones and the inversion given by
Eq. :eq:`struct_eq3` is no longer correct.

Therefore, in addition to Eq. :eq:`struct_eq1` and Eq. :eq:`struct_eq3`, two new internal
functions are used in UncertRadio:

-  As an alternative to Eq. :eq:`struct_eq1` a function **ActVal**\ (:math:`R_{n}`) for
   calculating the value of the output quantity is used based on the
   function **RESULT** (see below);

-  For the reversion according to Eq. :eq:`struct_eq3` a new function
   **RnetVal**\ (:math:`y'`) is used as an alternative; it uses the
   numerically working secant method; it requires initial guess values
   for the lower and upper limit of the net counting rate values to be
   searched for, which are easily derived from the values of :math:`y'`, :math:`F_C`
   und :math:`F_L`.
