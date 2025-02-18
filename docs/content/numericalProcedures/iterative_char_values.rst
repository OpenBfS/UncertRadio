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

.. math:: `\cdot u_{c}\left(y \left( R_{n} = f^{-1}\left( \tilde{y} = 0 \right) \right) \right)`
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
