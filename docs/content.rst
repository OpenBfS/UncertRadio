Content
=======

Introduction
------------

**UncertRadio** allows for **the evaluation of a
measurement in the field of measuring activity** by inserting defining
equations the **full calculation of the output quantity and its combined
uncertainty (according to ISO GUM)** and to calculate the **values of
the Decision threshold and the Detection limit (according to ISO
11929:2019)**, which are closely related to uncertainty.

The program assumes the **ISO GUM interpretation which is based on the
Bayesian theory of the measurement uncertainty (Bayesian statistics)**.
The Bayesian measurement uncertainty does not have a statistical
uncertainty, nor does it consider degrees of freedom, which makes it
different from conventional (frequentist) statistics. Therefore, degrees
of freedom need not to be treated in the program.

One restriction with respect to its usage referred to the circumstance
that only those measurement problems could be treated in which the
**measurements** of counts or of counting rates **are terminated if the
counting time reaches its pre-set value**. The statistically different
case where the measurement is terminated by reaching a pre-set value of
counts, where the counting time is a random variable, is also treated by
the program, since program version 2.4.04 (see section 7.20).

The present state of the program allows calculations **for up to three
output quantities**.

The sequence of steps being treated is:

1. Input of a short text description of the measurement problem.

2. **Input** of the **equations** defining the **output quantity** `y` of the
   measurement procedure; these define the **"evaluation model"**; the first of the
   equations must define the output quantity/quantities.

   Note: If more than one output quantity is to be treated, the calculation of the
   values of output quantity, uncertainty, uncertainty budget, Decision threshold
   and Detection limit, respectively, refers only to a single output quantity.
   By starting with a project, by default the first of the output quantities
   is "activated"; at a later stage another one can be selected in the main menu to be
   the "active one".

3. Automatic **extraction** of the **formula symbols** and **input of their meaning**
   (unit, meaning); automatic classification as independent and dependent symbols; manual
   addition of other symbols, not used explicitly in the equations.

4. Selection of symbols which define the **net counting rate (Rn)** and the
   **gross counting rate**, only for the purpose of the calculation of Decision threshold
   and Detection limit; the net counting rate in this case must be the
   "**procedure dependent net counting rate**" in which the counting rate contribution
   due to interference from other radionuclides, usually obtained by calculation, is taken
   into account.

5. **Input of measured values and their associated standard uncertainties** of input
   quantities (independent symbols) in table form.

6. For the **input of measurement uncertainties** their **associated distribution**
   can be chosen from a) **normal distribution**, b) **uniform distribution**,
   c) **triangular distribution** and d) **gamma distribution**, as well as a few others;
   half-widths of the latter two are converted by the program according to ISO GUM
   to standard uncertainties; for a complete list see
   `other distributions <#special-distributions-and-their-properties>`__.

   Note: In the case of low-level applications with very low count numbers
   `the so-called "(N+x) rule)" <#low-level-applications-nx-rule>`__ (d) may be selected
   in order to improve the results, from which the associated counting rate variables are
   to be considered as being Gamma distributed.

7. For the input one can choose between **absolute and relative uncertainties**.

8.  For counting rates or the number of counts **uncertainties can also be defined by formulae**.

9.  The standard deviation of the gross count rate must be defined by a formula;
    this is the **"uncertainty function" (standard uncertainty) of the gross counting rate**,
    which is the basis for deriving values of Decision threshold and Detection limit.

10. **Input of covariances between input quantities** which can be given as formulae or
    as values of correlation coefficients in tabular form.

11. Numerical calculation of values and combined standard uncertainties for the
    quantities classified as dependent quantities and for the output quantity and of
    the **uncertainty budget**; consideration of covariances/correlations between input
    quantities is possible; calculation of the
    "`best estimates and confidence limits based on a Bayesian method <#best-estimates-according-to-bayes-and-confidence-limits>`__"
    characterizing the value and standard uncertainty of the output quantity.

12. Iterative numerical calculation of **Decision threshold and Detection limit** for the
    output quantity based on the numerical evaluation of its combined uncertainty taking
    covariances into account.

13. A `Monte Carlo simulation <#monte-carlo-simulation>`__ can be started for an examination
    of the above-mentioned calculations which allows an independent calculation of the value
    of the output quantity and its combined uncertainty; partial derivations are not necessary
    in this case: for every input quantity classified as independent a value is drawn from its
    associated distribution from which a value of the output quantity is calculated using the
    defined equations. From the many-fold repetition of this step a statistical distribution
    is obtained for the output quantity which is used then to derive the "best estimate" form
    its mean and the combined uncertainty from its standard deviation; confidence limits are
    estimated as Quantiles of that distribution, whereas Decision threshold and Detection limits
    require to create separate MC distributions with modified values of the output quantity and
    their estimation by corresponding Quantiles.

14. Finally, a **complete report can be created as a text file** containing all the equations,
    input values, uncertainty budget table and the final results - including those from the
    Monte Carlo simulation, the PDF file also contains the MC graphs.

**UncertRadio is well suited for calculation comparison** for such
solutions\ **,** which one may have already developed with spreadsheet
calculations. The latter may get quite complex and sometimes are not so
easily manageable especially with respect to the correct uncertainty
propagation.

Numerical procedures
--------------------

Structure of equations
~~~~~~~~~~~~~~~~~~~~~~

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
quantity representing a **net counting rate,** called **Rn** in this
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

**Special feature**

All counting rates in equation :eq:`struct_eq2` may also appear as to be multiplied
with factors g, associated with uncertainties, as e.g.

.. math:: R_n = g_b \cdot R_b - g_0 \cdot R_0 - g_{bl} \cdot R_{bl} - g_{int} \cdot R_{int}

**Non-linear dependence**

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

Types of models
~~~~~~~~~~~~~~~

Regarding the relation between the output quantity and the net count
rate, or the activity in the counting source in the case of linear
unfolding, three types of models of measurement are considered in
UncertRadio:

-  positive linear model, including detection limit calculation

-  negative linear model, including detection limit calculation

-  only GUM, without detection limit calculation

The relation described in the section above, represents the most often
encountered case of a **positive linear model**. In this case, the net
count rate :math:`R_{n}` increases with an increasing gross count rate. An
activity is detected, if the gross count rate is significantly larger
than the background count rate.

**Negative linear Model**

With the measurement model explained above, the net count rate Rn
increases with increasing gross count rate; it is called positive
linear. The activity is considered as detected if the gross count rate
significantly exceeds the background count rate.

A **negative linear model** (since version 2.1.10 available) is
characterized by a gross effect which must fall significantly below the
"background" for the detection of the effect. In this case the
difference in the expression :math:`R_{n}` for the net effect is reversed:

.. math:: y = (R_0 - R_b) \cdot F_L + F_C
    :label: model_eq1

:math:`R_{b}` and :math:`R_{0}` herein are not necessarily count rates. The project
Rn-222-Emanation_EN.txp is an example for it.

The case abbreviated as **"only GUM"** has only been introduced for the
situation that only an uncertainty according to GUM is of interest for a
measurement, but a detection limit shall not be calculated, as it occurs
for instance in the case of weighing a sample.

Combined standard uncertainty :math:`\mathbf{u}_{\mathbf{c}}\left( \mathbf{y} \right)` of the output quantity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The combined standard uncertainty :math:`u_{c}(y)` of the output
quantity :math:`y` is determined according to ISO GUM (ISO **G**\ uide on
**U**\ ncertainty of **M**\ easurement (1995); see also EURACHEM / CITAC
Guide "Quantifying Uncertainty in Analytical Measurement" (2000)) using
the "Gaussian law of propagating uncertainties" and taking covariances
between individual input quantities.

For a type A standard uncertainty of a quantity which must be derived
from a set of repeated measurements a necessary small statistical
evaluation of mean and standard deviation has to be performed outside
this program; only mean and standard deviation will be used in
UncertRadio. Usually, with measurements of activities the greater part
of uncertainties of quantities belongs to Type B. Within the framework
of the **Bayesian theory of measurement** (Weise & Wöger, 1999; Weise et
al., 2006) which is underlying the basics of this program an **explicit
differentiation between quantities of type A and B is not necessary**.
This is also the reason that degrees of freedom are not considered in
this program.

The analytical derivation of formulae for the combined uncertainty for
instance of a mass or volume dependent activity using the law of
propagating uncertainties may easily yield a certain number of less or
more complex formulae the correctness of which is often not easily being
controlled. Therefore, a numerical procedure is applied. The first step
consists in transferring all quantities/parameters being required for
the calculations, these may easily become more than 20, into a program
array **MeasdValue(i) = p(i)**. Then, in a subprogram RESULT the value
of the output quantity is calculated from the values of the array
elements *p(i)* by using the function parser.

Similarly, the known measurement uncertainties of the individual
quantities/parameters are transferred to an array **StdUnc(i) = u(i)**.
For the calculation of the combined uncertainty a `subroutine
Uncpropa <#URH_UNCPROPA_EN>`__ is used to which the two arrays *p(i)*
and *u(i)* are transferred. Covariances are considered in this
subroutine. The sensitivity coefficients, i.e. the partial derivates of
the function calculated with RESULT with respect to the array elements
*p(i)*, are numerically approximated by differential quotients.

Further details: see `Uncertainty
propagation <#uncertainty-propagation>`__

Iterative determination of Decision threshold and Detection limit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **calculation of detection limits is based on ISO 11929:2019** which
is derived from Bayesian methods (see also Weise et al., 2006). It
utilizes complete uncertainty propagation taking all individual
uncertainties and covariances into account where the numerical
calculations are based on the routines **RESULT** and **UncPropa**. The
values of Decision threshold and Detection limit for the output quantity
:math:`y` are calculated by using an iterative procedure. In this procedure,
the value of :math:`y` is varied, now designated as "assumed value"
:math:`\tilde{y}`. From this, **the iterated value**
:math:`\tilde{\mathbf{R}}_{\mathbf{b}}` **of the gross counting
rate** is obtained via calculating the net counting rate from
:math:`\tilde{y}`. For each iteration step the combined standard
uncertainty of :math:`\tilde{y}`, now called uncertainty
function :math:`\tilde{\mathbf{u}}\left(\tilde{\mathbf{y}} \right)`
is in turn derived from the easily calculated
:math:`\tilde{u}(\tilde{R_{b}}).`

The :math:`y^{*}` for the output quantity :math:`y` is
calculated according to ISO 11929 as follows:
document-open.pngpha} \cdot u_{c}\left(y \left( R_{n} = f^{-1}\left( \tilde{y} = 0 \right) \right) \right) ,
    :label: char_thres_eq1

where :math:`k_{1 - \alpha}` is the normal quantile belonging to the
error of first kind, :math:`\alpha`. **UncPropa** is used for
calculating the combined standard uncertainty :math:`u_{c}(y)` of the
output quantity under the constraint that the net counting rate is set
equal to zero. This is easily done.

The **Detection limit** :math:`y^{\#}` for the output quantity :math:`y` is
calculated as follows, where :math:`y^{*}` is the value of the Decision
limit taking from the preceding step and :math:`k_{1 - \beta}` is the
normal quantile belonging to the error of second kind, :math:`\beta`:

.. math:: y^{\#} = y^{*} + k_{1 - \beta} \cdot u_{c}\left(y\left( R_{n} = f^{-1}\left( \tilde{y} = 0 \right) \right) \right)
    :label: char_thres_eq2

This represents an implicit equation for :math:`y^{\#}`, because on the
right-hand side of the equation the uncertainty :math:`u` is to be calculated
for a value of :math:`R_n`, which corresponds to the value :math:`y^{\#}` on
the left-hand side; the latter is obtained by the inverse function
:math:`f^{-1}` which is easily established as :math:`R_n = (y -F_C)/F_L` from
the simple linear relationship between :math:`y` and :math:`R_n`,
:math:`y = F_L \cdot R_n + F_C`.

The solution of the implicit equation :eq:`char_thres_eq2` is obtained by a simple
iterative procedure which is demonstrated for the `detection limit
case <#algorithm-for-iterative-numerical-calculation-of-the-detection-limit-mathbfymathbf>`__.
The value of Factor is determined in the subprogram **RESULT**, while
the uncertainty in Eq. (2) is calculated with the function subprogram
**UncPropa**. In order to use UncPropa correctly, for each iteration
step the corresponding value :math:`R_{n}^{i}` is obtained from the
associated :math:`\mathbf{y}^{\mathbf{\# i}}`, which then is transformed
to the gross counting rate, which in this example is stored in the array
element :math:`{p(8)}^{i}`. The uncertainty of the latter is calculated
from the "uncertainty function (standard uncertainty) of the gross
counting rate" which has been supplied to the program by the user. In
the example the gross counting rate is calculated as if
:math:`{p(8)}^{i}` had been obtained by simple single-channel counting,
which applies to most cases: :math:`u(8)^{i} = \sqrt{p(8)^{i}/t}`.

With the version 2.2.02 (2017/12) the iteration procedure described
above has been replaced by the Ridder' method (subroutine zriddr from
the Numerical Recipes, Press et al., 1992). It works more effectively
than the secant method. Since version 2.2.11 (2018/11) the method by
Brent is applied.

**Special cases**

In the case of linear unfolding by using linear least squares analysis,
e.g. in the evaluation of a decay curve, the fitting parameter for the
desired net counting rate is that quantity which is varied by iteration
for estimating Decision threshold and Detection limit (c.f. `Note on
Decision threshold and Detection limit with linear
unfolding <#note-on-decision-threshold-and-detection-limit-for-linear-fitting>`__).

For determining the activity of a radionuclide from several gamma lines
the quantity associated with this activity is the one of which the value
is iterated (c.f. `Method for calculating Decision threshold and
Detection limit with
Gamspk1 <#approach-of-calculating-decision-threshold-and-detection-limit-for-gamspk1>`__).

Preventing "hidden" covariances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the following, it is assumed now that the arithmetic expression for
the output quantity :math:`y` containing several expressions, e.g.,
:math:`{\ a}_{1},\ a_{2},a_{3}`, each of which being functions of input
quantities :math:`x_{i}`. Often, the uncertainties
:math:`{u(a}_{1}),\ \ u(a_{2}),{\ u(a}_{3})` are calculated first from
which then :math:`u(y\left( {\ a}_{1},\ a_{2},a_{3} \right))` is
derived. If, however, there are some of the input quantities
:math:`x_{i}`, contained in more than one of the expressions
:math:`{\ a}_{1},\ a_{2},a_{3}`, then "hidden" or "overlooked"
covariances exist between some of the :math:`{\ a}_{1},\ a_{2},a_{3}`,
which would have to be considered afterwards.

This problem does not occur during the uncertainty calculations within
UncertRadio, because there the partial derivatives in its uncertainty
propagation are always build from the equation of the output quantity.
It is shown below, why this avoids the above problem.

The ansatz for the uncertainty propagation with partial derivatives
which refer to the output quantity
:math:`y = y({\ a}_{1}(\mathbf{x}),\ a_{2}(\mathbf{x}),a_{3}(\mathbf{x}))`,
is formulated as follows with the vector :math:`\mathbf{x}` of input
quantities:

At first, the square within the sum is evaluated:

.. math::
    u^{2}(y) = \sum_{i = 1}^{ni}{\left( \frac{\partial y}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right)}

.. math::
    u^{2}(y) = \sum_{i = 1}^{ni}{\left( \sum_{j = 1}^{3}{\frac{\partial y}{\partial a_{j}}\frac{\partial a_{j}}{\partial x_{i}}} \right)^{2}u^{2}\left( x_{i} \right)}

.. math::
    \sum_{i = 1}^{ni}{\left( \frac{\partial y}{\partial a_{1}}\frac{\partial a_{1}}{\partial x_{i}} + \frac{\partial y}{\partial a_{2}}\frac{\partial a_{2}}{\partial x_{i}} + \frac{\partial y}{\partial a_{3}}\frac{\partial a_{3}}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right)}


Now, the summation over :math:`i` is performed for each of the six
terms, while at the same time the partial derivatives of :math:`y` by
:math:`a_{j}` are factored out of the sums:

.. math::
    :nowrap:

    \begin{eqnarray}
        u^{2}(y) = \sum_{i = 1}^{n_i} & & \left( \frac{\partial y}{\partial a_{1}}\frac{\partial a_{1}}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right) + \\
        && \left( \frac{\partial y}{\partial a_{2}}\frac{\partial a_{2}}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right) + \\
        && \left( \frac{\partial y}{\partial a_{3}}\frac{\partial a_{3}}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right) + \\

        && 2\left( \frac{\partial y}{\partial a_{1}}\frac{\partial a_{1}}{\partial x_{i}} \right)\left( \frac{\partial y}{\partial a_{2}}\frac{\partial a_{2}}{\partial x_{i}} \right)u^{2}\left( x_{i} \right) + \\
        && 2\left( \frac{\partial y}{\partial a_{1}}\frac{\partial a_{1}}{\partial x_{i}} \right)\left( \frac{\partial y}{\partial a_{3}}\frac{\partial a_{3}}{\partial x_{i}} \right)u^{2}\left( x_{i} \right) + \\
        && 2\left( \frac{\partial y}{\partial a_{2}}\frac{\partial a_{2}}{\partial x_{i}} \right)\left( \frac{\partial y}{\partial a_{3}}\frac{\partial a_{3}}{\partial x_{i}} \right)u^{2}\left( x_{i} \right)
    \end{eqnarray}



Now, each individual sum over :math:`i` is representing a variance or a
covariance of the expressions :math:`{\ a}_{1},\ a_{2},a_{3}`:

.. math::

    u^{2}(y) = &\left( \frac{\partial y}{\partial a_{1}} \right)^{2}u^{2}\left( a_{1} \right) +
               \left( \frac{\partial y}{\partial a_{2}} \right)^{2}u^{2}\left( a_{2} \right) +
               \left( \frac{\partial y}{\partial a_{3}} \right)^{2}u^{2}\left( a_{3} \right) + \\
               &2\frac{\partial y}{\partial a_{1}}\frac{\partial y}{\partial a_{2}} cov(a_{1},a_{2}) +
                2\frac{\partial y}{\partial a_{1}}\frac{\partial y}{\partial a_{3}}cov(a_{1},a_{3}) + \\
               & 2\frac{\partial y}{\partial a_{2}}\frac{\partial y}{\partial a_{3}}cov(a_{2},a_{3})

Usually, a "hand-made" uncertainty propagation by first applying a
decomposition of :math:`y` into expressions or functions :math:`a_{j}`,
only the first three terms in Eq. (6) are used, because covariances
between the :math:`a_{j}` often are not expected; this may explain the
term "hidden" covariances.

The result of Eq. (6) is just the one which has to be expected when
"hidden" covariances between the :math:`a_{j}` are explicitly taken into
account. This demonstrates that these covariances are considered by
UncertRadio, automatically, only because it uses within its uncertainty
evaluation according to Eq. (1), partial derivatives directly of the
output quantity.

Using switching variables in equations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The function parser (fparser) implemented in UncertRadio allows to apply
**switching variables**, to which **only the two values 0 and 1 can be
attributed**. Such a variable allows to **activate another variable b**
with **b^1** or **to deactivate it by b^0**. In UncertRadio, these
variables can be declared by attaching the string "Trigger" to the
symbol name. They are, therefore, also called **"trigger variables"**.

Examples are: "min_Trigger", "kilo_Trigger"; with "60^min_Trigger" or
with "1000^kilo_Trigger" scaling factors of 60 (for minutes) or 1000 can
be switched; see chapter 2.2.7.

If a switching variable is to be used
for count rate variables, it must contain the part "Trigger" attached
to e.g. "min"; then they can be identified by the program which in turn
helps to prevent them from disturbing the process of finding such count
rates which directly contribute to the net count rate (see chapter 2.3).

Calculation of physical units for dependent variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With the program version 2.4.13, UncertRadio contains a menu item which
allows as a test to derive **the physical units of dependent variables**
based on a numerical algorithm. The units of the input quantities, often
given by "derived units" are changed to "basic units"; in addition to
this, the associated scaling factors are determined. If, for example, a
counting duration variable was given the unit "min", an associated
scaling factor of 60 is applied for changing to the basic unit "s". The
description of basic units and their derived units and of the algorithm
for "calculating" the units of dependent variables is given in `chapter
7.21 <#treatment-of-physical-units>`__.

A CSV file distributed with the program contains only a small number of
units, which is nearly sufficient for measurements of radioactivity. If
necessary, the basic units within the CSV file can be modified by the
user.

By the menu item "Edit – test physical units", the transformation to
basic units can be tested. In UncertRadio's text editor a comparison of
"original" and "converted" units is shown for the list of quantities
(symbols), as well as their associated values and uncertainties. At the
begin of this list, a first error message is shown in case of conversion
errors found by the program, which can indicate indirectly a wrong
combination of units in the indicated number of the equation.

By programming, the routine behind this menu item was applied to all of
UncertRadios example projects. There were indeed errors found, and it
was necessary in most of the examples to replace the unit "1" of a
detection probability by the unit "1/Bq/s" in order to get the unit "Bq"
for an activity as output quantity. More details are given in `chapter
7.21.3 <#invoking-the-test-of-unit-calculations>`__.

Equations as tree topology
--------------------------

The equations for calculating the value of the output quantity, as being
set up in section 2.2.1, are **hierarchical equations**. They form a
list of dependent quantity (of number *nab*), followed by the list of
independent input quantities (of number *nmu*). Therefore, numbers from
1 to (*nab*\ +\ *nmu*) are attributed to the quantities. This list may
be considered as if it were a ladder with (*nab*\ +\ *nmu*) steps, or a
decay scheme of a decaying atomic nucleus; on each step (or level) a
quantity symbol resides. According to their associated auxiliary
equations, the dependent quantities are related with other symbols on
lower positions of the ladder (or level). Connecting these symbols by
lines generates a tree structure, which is comparable to a series of
allowed level transitions of a decaying atomic nucleus. The series of
connecting lines down to the ladder step of an independent input
quantity can be compared with a cascade of level transitions of a
nucleus ending at the ground state level.

The following is restricted to applications not using linear unfolding.

Such "symbol cascades" can be generated from all "transitions" (*i*,
*j*)=\ *ij* between symbols *i* and *j* within a cascade. They can be
found by using a recursive numerical algorithm.

This method is especially applied to find out for a net count rate *Rn*,
being proportional to the output quantity value, on which individual
count rate contributions *Ri* it depends. Furthermore, the count rates
*Ri* have two additional properties:

a) a square-root-based uncertainty function like sqrt(*Ri*/*ti*) can be
   attributed to them, or,

b) they may be based on count numbers *Ni*, also being associated with
   uncertainty formulae like (sqrt(*Ni*)), or for which special
   distribution types are declared, e.g., the gamma distribution ("x+1")
   or a Poisson/binomial distribution.

With taking these additional properties into account (called "rules"
below), in most cases those symbols can be identified, which represent a
count number, including also the associated counting duration. Then, by
going one step back within the affected hierarchy ladder, the symbol
representing the associated count rate *Ri*\ =\ *Ni*/*ti* is found.

Knowing the relation between the gross count rate *Rg*, the gross count
number *Ng* and the counting duration *tg*, and their symbol numbers
within a cascade, allows, for deriving decision threshold and detection
limit, to generate a modification from *Rg* to *Rg*\ ~ from the related
modification from *Ng* to *Ng*\ ~. For simplification of this step,
index fields are generated within UncertRadio which point from a count
rate to the number of counts and to the counting duration, and vice
versa. This, however, requires that not only count rates *Ri* alone are
defined in the equations, but also the equations *Ri*/*ti*. This results
in the recommendation, to follow this in working with UncertRadio.

**Example** Ra226_U235-at-186keV_EN.txp:

::

    Equations (*nab*\ =8, *nmu*\ =10):

    Formeltext=

    1 : cRa = Phi \* RRa

    2 : Phi = 1. / (eps \* pRA \* mp)

    3 : RRa = RS - RU5

    4 : RS = Rb - RT - RnNE

    5 : RU5 = AU5 \* Ufakt

    6 : Ufakt = eps \* pU5 \* mp

    7 : Rg = Ng / tm

    8 : RT = NT / tm

::

    Table of transitions *i* *j*:

    nd i j Symb(i) Symb(j)

    1 3 4 RRa RS

    2 3 5 RRa RU5

    3 4 7 RS Rg

    4 4 8 RS RT

    5 4 12 RS RnNE

    6 5 13 RU5 AU5

    7 5 6 RU5 Ufakt

    8 6 9 Ufakt eps

    9 6 14 Ufakt pU5

    10 6 11 Ufakt mp

    11 7 15 Rg Ng

    12 7 16 Rg tm

    13 8 17 RT NT

    14 8 16 RT tm


::

    Table of cascades (chain) and three identified count rates as part of
    the net count rate:

    nc i j kcnt ktime krate rule Symbol chain

    1 7 15 15 15 7 A5 Rg 3 4 7 15

    2 7 16 0 0 0 3 4 7 16

    3 8 17 17 17 8 A3 RT 3 4 8 17

    4 8 16 0 0 0 3 4 8 16

    5 4 12 0 0 12 A6 RnNE 3 4 12

    6 5 13 0 0 0 3 5 13

    7 6 9 0 0 0 3 5 6 9

    8 6 14 0 0 0 3 5 6 14

    9 6 11 0 0 0 3 5 6 11


::

    Table of index fields of counting duration (iptr_time) and number of
    counts (iptr_cnt) to the count rate (iptr_rate)

    (*RnNE* is defined only as a net count rate of the background
    measurement)

    i iptr_time iptr_cnt iptr_rate Symbol

    7 16 15 7 Rb

    8 16 17 8 RT

    12 0 0 0 RnNE


Among the example projects belonging to UncertRadio are two, for which
the algorithm shortly introduced above in fact finds specific count rate
symbols two times:

BSH_total-gamma_var2_EN.txp

DWD_sr89_sr90_TDCR_procedure_EN.txp

In the first one, this result leads to the conclusion, that the
equations constituting the net count rate, have not been simplified
enough. In fact, it can be demonstrated that the corresponding equations
can be re-worked algebraically such that the equations of the
alternative example, BSH_total-gamma_var1_EN.txp, are exactly met.

In the second example mentioned above, the equation for *Rn_s* for
calculation the Sr-90 activity is rather complex, so that both, *R0_s*
and *R0_c*, appear twice in them, also in a non-linear form.

**Note**: While running the QC batch mode processing an additional file
fort.64 is produced showing in short form the identified count rate
contributions to the net count rate (projects not using linear
unfolding). Meanwhile, this option is deactivated.

**Note**: The example given above shows that the gross count rate Rg is
the first in the list of count rates contributing to the net count rate.
This characteristic can be used for the internal checking whether the
correct gross count rate symbol has been selected within the TAB
"Equations", because **the gross count rate is always the first of the
count rates in the expression for the net count rate.**


**File Selection Dialog**

.. image:: /_static/images/en/file_chooser.png

Note: If this dialog is used in the mode "\ **save as**\ ", *the desired
file extension of the file name must explicitly be given or edited in
the name field (at the top of the dialog).* Only the pure filename has
to be inserted into this field, the desired path name is selected in the
dialog elements below.

Clicking on "\ **Recently used**\ " shows a list of recently used files.
The filenames are hold by the *RecentManager* of GTK; the latter works
with a file "recently-used.xbel", which e.g. for WIN 7 is found in the
folder "c:\\users\\user\\AppData\\Local\\" (the actual Windows name of
the user replaces "user" in the folder name).

**Dialogs**

Dialogs, and also elements in them, are re-sizable now by the mouse,

**Input to tables**

The input of a value into a cell of a table has to be finalized with the
enter key.

**Column blocks in tables**

Column blocks can no longer be selected with UR tables, i.e. the export
of such blocks to e.g. Excel is not possible. However, the reverse way
is possible: the import of a column block taken from e.g. an Excel file,
or from the text editor Notepad ++, into a column block of equal size in
an UR table; `see
also <#within-tables-delete-rows-working-with-column-blocks>`__.

For selecting a whole row click into the right part of a cell in this
row.

**Change of the structure of project files**

Two parameters have been added to the structure of project files under
the item "Sonstige:"

GamDistAdd=1.0000

GUM_restricted=F

ModelType=PosLin

**Confidence ellipse**

When using linear unfolding with more than one output quantity, the
confidence ellipse for a pair of two output quantities may be displayed
graphically. The correlation matrix is also displayed.

**New decay factor function**

A new function fd having three parameters has been introduced for a
counting duration averaged decay factor, which makes writing decay
factors simpler:

fd(tA,tm,xlam) = exp(-xlam*tA) \* (1.d0 - exp(-xlam*tm)) / (xlam*tm)

An existing equation like

c_89=Rn3*q*lamS89*tm1*exp(lamS89*(tA+tE+tSr))/(V*etaSr*eps1*(1.-
exp(-lamS89*tm1)))

then transforms into the equation

c_89=Rn3*q/fd(tA+tE+tSr, tm1, lamS89) /(V*etaSr*eps1 ).

For the case of Sr-90 + Y-90 an existing equation like

X2 = eSr90A \* (1. - exp(-lamSr90*tmess)) / (lamSr90*tmess) \*
exp(-lamSr90*(tAS+tstart)) + &

eY90A \* lamY90/(tmess*(lamY90-lamSr90)) \* &

( -exp(-lamSr90*(tAS+tstart))/lamSr90*(exp(-lamSr90*tmess)-1.) &

+exp(-lamY90*(tAS+tstart))/lamY90*(exp(-lamY90*tmess)-1.) )

transforms into the equation

X2 = eSr90A \* fd(tAS+tstart,tmess,lamSr90) + &

eY90A \* lamY90/(lamY90-lamSr90) \* ( fd(tAS+tstart,tmess,lamSr90) -
fd(tAS+tstart,tmess,lamY90) )

**Excel-VBA for control of UR evaluations**

The VBA module has been adapted to UR2; the "\\" characters within that
argument in the command string representing the pathname of the project
file to be evaluated by UR are replaced by "/" characters. Since Version
2.1.1 this is redundant; `See <#running-ur-in-batch-mode>`__

Literature
----------

AKU, 2008\ *. Moderne Routine- und Schnellmethoden zur Bestimmung von
Sr-89 und Sr-90 bei der Umweltüberwachung*. Bericht einer
Ad-hoc-Arbeitsgruppe des Arbeitskreises Umweltüber­wachung (AKU). Bericht
FS-08-147-AKU des Fachverbandes für Strahlenschutz. TÜV Media GmbH,
Köln.

Barlow, R.J., 1999: *Statistics. A Guide to the Use of Statistical
Methods in the Physical Sciences.* The Manchester Physics Series. John
Wiley & Sons Ltd., Chichester, New York, 204 S.

Blobel, V., Lohrmann, E., 1998. *Statistische und numerische Methoden
der Datenanalyse*. B.G. Teubner Stuttgart-Leipzig, 358 S.

Brandt, S., 1999: *Datenanalyse. Mit statistischen Methoden und
Computerprogrammen*; 4. Auflage. Spektrum, Akademischer Verlag,
Heidelberg-Berlin, 646 S.

Cox, M.G., Harris, P.M., 2001: *Measurement Uncertainty and the
Propagation of distributions*. NPL, UK, Paper presented at the 10th
International Metrology Congress, Saint-Louis, France, 22-25th October
2001.

Cox, M.G., Forbes, A.B., Harris, P.M., Smith, I.M., 2004: The
classification and solution of regression problems for calibration\ *.*
NPL Report CMSC 24/03, (chapter 6.3), National Physics Laboratory,
Teddington, UK, 46

http://www.npl.co.uk/ssfm/download/nplreports.html.

Cox, M., Harris, P., Nam, G., Thomas, D., 2006: *The Use of a Monte
Carlo Method for Uncertainty Calculation, with an Application to the
Measurement of Neutron Ambient Dose Equivalent Rate*. Radiation
Protection Dosimetry 121, pp. 12-23.

Cox, M.G., Eiø, C., Mana, G. and Pennecchi, F, 2006b. *The generalized
weighted mean of correlated quantities*. Metrologia 43 S268-S275

EURACHEM/CITAC, Guide CG 4, 2000. *Quantifying uncertainty in analytical
measurement*. Second edn., 120 S.
http://www.eurachem.ul.pt/guides/QUAM2000-1.pdf.

Gilmore, G.: Practical Gamma-Ray Spectrometry. 2nd Edition; J. Wiley &
Sons Ltd; 2008.

Hauschild, T., Jentschel, M., 2001. *Comparison of maximum likelihood
estimation and chi-square statistics applied to counting experiments*.
Nucl. Instr. & Meth A 457 (1-2), S 384-401.

Hoover, W. E., 1984: *Algorithms For Confidence Circles and Ellipses*.
NOAA Technical Report NOS 107 C&GS 3; Charting and Geodetic Services;
Rockville, MD; September 1984

http://www.ngs.noaa.gov/PUBS_LIB/AlgorithmsForConfidenceCirclesAndEllipses_TR_NOS107_CGS3.pdf

International Organisation for Standardisation, 1993. *Guide to the
Expression of Uncertainty in Measurement (GUM)*. (Geneva: ISO),
corrected reprint (1995), also as ENV 13005 (1999).

International Organisation for Standardisation, 2010. Determination of
the characteristic limits (decision threshold, detection limit and
limits of the confidence interval) for measurements of ionizing
radiation — Fundamentals and application. (Geneva: ISO), 2010.

International Organisation for Standardisation, 2019. Determination of
the characteristic limits (decision threshold, detection limit and
limits of the coverage interval) for measurements of ionizing radiation
— Fundamentals and application. Part1: Elementary applications (Geneva:
ISO), 2019.

International Safety Research, Safety Support Series, 2013. Radiation
Counting Statistics. Volume 1. Canada.

Janßen, H., 2004. *Determination of Strontium-89 and Strontium-90 in
soils and sediments.* In: Quantifying uncertainty in nuclear analytical
measurements, IAEA-TECDOC-1401, pp. 149-166.

JCGM 101:2008. *Evaluation of measurement data — Supplement 1 to the
"Guide to the expression of uncertainty in measurement" — Propagation of
distributions using a Monte Carlo method* GUM, Joint Committee for
Guides in Metrology, 2008. (GUM Supplement 1)

JCGM 102:2011. *Evaluation of measurement data – Supplement 2 to the
"Guide to the expression of uncertainty in measurement" – Extension to
any number of output quantities*. Joint Committee for Guides in
Metrology; 2011; http://www.bipm.org/en/publications/guides/gum.htm

Kacker, R.N., Datla, R.U., Parr, A.C, 2002. *Combined result and
associated uncertainty from interlaboratory evaluations based on the ISO
Guide.* Metrologia 39, 279-293.

Kacker, R.N., Datla, R.U., Parr, A.C., 2004. *Statistical analysis of
CIPM key comparisons based on the ISO Guide*. Metrologia 41, 340-352.

Kanisch, G., 2004. *Quantifying Uncertainties in the Alpha-spectrometric
Analysis of Environmental Samples*. In: Quantifying uncertainty in
nuclear analytical measurements, IAEA-TECDOC 1401, Vienna, pp. 127-139;

Kanisch, G., 2016. *Generalized evaluation of environmental
radioactivity measurements with UncertRadio. Part I: Methods with linear
unfolding*. Applied radiation and Isotopes 110, 28–41;
`doi:10.1016/j.apradiso.2015.12.003 <http://dx.doi.org/10.1016/j.apradiso.2015.12.003>`__

Kanisch, G., 2016. *Generalized evaluation of environmental
radioactivity measurements with UncertRadio. Part II: Methods without
linear unfolding*. Applied radiation and Isotopes 110, 74-86;

http://dx.doi.org/10.1016/j.apradiso.2015.12.003

Kessel, R., Kacker, R., Berglund, M., 2006. *Coefficient of contribution
to the combined standard uncertainty*. Metrologia 43, S189-S195.

Knoll, G.F.. *Radiation Detection and Measurement*, 2nd edition, John
Wiley, NewYork,1989, pp. 96-99

Laurence, T. A., Chromy, B., 2009. *Efficient Levenberg-Marquardt
Minimization of the Maximum Likelihood Estimator for Poisson Deviates*.
Report LLNL-JRNL-420247, November 13, 2009.

Marsaglia, G., Tsang, W.W., 2000. *A Simple Method for Generating Gamma
Variables*. ACM Transactions on Mathematical Software, Vol. 26, No. 3,
363–372.

Mathews, I.P., Kouris, K., Jones, M.C., Spyrou, N.M.: Theoretical and
experimental investigations on the applicability of the Poisson and
Ruark-DeVol statistical density functions in the theory of radioactive
decay and counting. Nucl. Instr. Meth. 171 (1979), 369-375.

Michel, R., 2000: *Quality assurance of nuclear analytical techniques
based on Bayesian characteristic limits*. J. Radioanal. and Nucl. Chem.
245: 137-144.

Michel, R., Kirchhoff, K., 1999. *Nachweis-, Erkennungs- und
Vertrauensgrenzen bei Kernstrahlungsmessungen*. Fachverband für
Strahlenschutz e.V., Köln: TÜV-Verlag, Publikation FS-99-108-AKSIGMA,
ISSN 1013-4506, 157 S.

Miller, A. Alan Miller's Fortran Software.
https://jblevins.org/mirror/amiller/

Moreno, J., Vajda, N., Burns, K., Danesi, P.R., De Regge, P., A.
Fajgelj, A., 2004. *Radiochemical determination of Strontium-90 in
environmental samples by Liquid Scintillation Counting*. In: Quantifying
uncertainty in nuclear analytical measurements, IAEA-TECDOC-1401, pp.
167-193.

Pengra, D., 2008: *Counting statistics of random events: A tutorial*. 9
S.

http://courses.washington.edu/phys433/muon_counting/counting_stats_tutorial_b.pdf

Pishro-Nik, H., *Introduction to Probability*:

https://www.probabilitycourse.com/chapter11/11_1_2_basic_concepts_of_the_poisson_process.php

Pommé, S., Keightley, J., 2007. *Countrate estimation of a Poisson
process: unbiased fit versus central moment analysis of time interval
spectra*. Applied Modeling and Computations in Nuclear Science. In:
Semkow, T.M., Pommé, S., Jerome, S.M., Strom, D.J. (Eds.), ACS Symposium
Series 945. American Chemical Society, Washington, DC,
pp.316–334.2007.ISBN0-8412-3982-7.

Press, W. H., Teukolsky, S. A., Vetterling, W. T. and Flannery, B. P.,
1.    *Numerical Recipes in FORTRAN*, second edn. Cambridge: Cambridge
Unversity Press.

Ratel G., Michotte, C., Bochud, F. O.: Uncertainty of combined activity
estimations. Metrologia 52 (2015) S30–S41.

Rusconi, R., Forte, M., Caresana, M., Bellinzona, S., Cazzaniga, M.T.,
Sgorbati, G., 2006. *The evaluation of uncertainty in low-level LSC
measurements of water samples.* Appl. Radiat. Isot. 64, 1124-1129.

Salma, I., Zemplén-Papp, É.: Experimental investigation of statistical
models describing distribution of counts. Nucl. Instr. Meth. A 312
(1992), 591-597.

Semkow, T.M.: Bayesian Inference from the Binomial and Poisson Process
for Multiple Sampling. In: T.M. Semkow, S. Pommé, S.M. Jerome, D.L.
Strom (Ed.): Applied Modeling and Computations in Nuclear Science. ACS
Symposium Series 945, ACS, Oxford University Press, 2007.

Spyrou, N.M., Foster, J., Jones, M.C., Kouris, K., Matthews, I.P.:
Should the Poisson statistical density function be used in the
measurement of short-lived isotopes? J. Radioanal. Chem. 61 (1981),
121-130.

Thompson, M.A., 2015: *Gaussian Statistics Lecture.*

http://www.hep.phy.cam.ac.uk/~thomson/lectures/statistics/GaussianStatistics_Handout.pdf

Weise, K., Hübel, K., Michel, R., Rose, E., Schläger, M., Schrammel, D.,
Täschner, M., 2004. *Nachweisgrenze und Erkennungsgrenze bei
Kernstrahlungsmessungen: Spezielle Anwendungen. Vorschlag für eine
Norm.* Fachverband für Strahlenschutz e.V., Köln: TÜV-Verlag,
Publikation FS-04-127-AKSIGMA, ISSN 1013-4506, 31 S.

Weise, K., Wöger, W., 1999. *Meßunsicherheit und Meßdatenauswertung*.
Verlag Wiley-VCH Weinheim, 345 S.

Weise, K., Hübel, K., Rose, E., Schläger, M., Schrammel, D. Täschner,
M., Michel, R., 2006. *Bayesian decision threshold, detection limit and
confidence limits in ionizing-radiation measurement*. Radiat. Prot.
Dosimetry 121(1), 52 – 63.

Weise, K., Kanisch, G., Michel, R., Schläger, M., Schrammel, D.,
Täschner, M., 2009. *Monte Carlo determination of the characteristic
limits in measurements of ionizing radiation – Fundamentals and
numerics*. Radiation Protection Dosimetry 135 (3), 169–196.

Weise, K., Kanisch, G., Michel, R., Schläger, M., Schrammel, D.,
Täschner, M., 2013. *Characteristic values in measurements of ionizing
radiation – Materials for a critical discussion on Fundamentals and
alternatives.* Fachverband für Strahlenschutz e.V., Köln: TÜV-Verlag,
Publikation FS-2013-167-AKSIGMA, ISSN 1013-4506, 51 pp.

Wübbeler, G., Krystek, M., Elster, C., 2008. *Evaluation of measurement
uncertainty and its numerical calculation by a Monte Carlo method.*
Meas. Sci. Technol. 19, 084009 (4pp)