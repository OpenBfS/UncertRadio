TAB “Uncertainty Budget”
^^^^^^^^^^^^^^^^^^^^^^^^

Under this being selected by mouse click the uncertainty budget is
presented as a table. The output quantity is indicated which this
uncertainty budget is referred to.

The **table “Uncertainty budget”** again contains the three columns
“Symbols”, “Type” and “Unit” already known.

The columns “Value” and “Std. uncertainty” contain the input values of
the measured value and its standard uncertainty of each of the
independent measurement quantities as well as of each dependent quantity
(auxiliary and output quantity).

In the column **“Sensitivity coefficient”** partial derivatives of the
function of the output quantity **y** (i.e., the function determined by
the equations with which the value of the output quantity is calculated)
are given for each independent quantity.

From the **products Uncertainty x Sensitivity coefficient**, the values
of which are shown in the next column, the uncertainty budget is deduced
in two different ways.

The column **“relat. contribut(%)“** shows for each independent symbol -
in case its uncertainty is non-zero - the relative contribution (in %)
of its variance to the total variance of the output quantity. This
column gives the information which of the (independent) quantities
contributes at most to the combined uncertainty of the output quantity.
The indication of 100% in this column for the output quantity is only
that the control sum of the individual relative contributions. Using the
**button “Change budget type”** allows to display absolute uncertainty
contributions in this column, given in the unit of the output quantity.

The preceding **definition of the “relat. contribut(%)“** to the
uncertainty budget **has got a further plausible meaning by the recent
publication by** **Kessel, Kacker and Berglund** (2006) with the title
“Coefficient of contribution to the combined standard uncertainty“:

   That relative contribution of an input quantity :math:`x_{i}`,
   divided by 100, is in the case of un-correlated input quantities
   identical with the square of the correlation coefficient between
   :math:`x_{i}` and the output quantity **y**! This quantity is now
   called “\ **coefficient of contribution”** and is represented by the
   symbol :math:`h\left( y,x_{i} \right)`.

The generalised definition of the “coefficient of contribution” is now:

:math:`h\left( y,x_{i} \right) = \frac{\left( \partial y/\partial x_{i} \right) \bullet u\left( x_{i} \right)}{u(y)} \bullet r\left( y,x_{i} \right)`
(1)

As we have for non-correlated input quantities:

:math:`r\left( y,x_{i} \right) = \frac{\left( \partial y/\partial x_{i} \right) \bullet u\left( x_{i} \right)}{u(y)}`
(2)

from Eq. (1) then follows Eq. (3), which was already known - for
non-correlated input quantities - as the positive relative contribution
to the variance of the output quantity:

:math:`h\left( y,x_{i} \right) = \left\lbrack \frac{\left( \partial y/\partial x_{i} \right) \bullet u\left( x_{i} \right)}{u(y)} \right\rbrack^{2}`
(3)

If correlations :math:`r\left( x_{i},x_{j} \right)` between input
quantities exist, they are inserted into Eq. (1) in the factor
:math:`r\left( y,x_{i} \right)` defined as follows:

:math:`r\left( y,x_{i} \right) = \sum_{j}^{}{\left\lbrack \frac{\left( \partial y/\partial x_{j} \right) \bullet u\left( x_{j} \right)}{u(y)} \right\rbrack \bullet \left\lbrack r\left( x_{i},x_{j} \right) \right\rbrack}`
(4)

Note that this may lead now in some cases to negative values of
:math:`h\left( y,x_{i} \right)`, the coefficient of contribution.

**Notes on effects from covariances:**

If covariances are considered for the calculation of uncertainties
negative values may occur in the column “relat. contribut(%)“; this is
not a program error.

According to the mentioned paper by Kessel et al. correlations
(covariances) between input quantities are considered according to Eq.
(1) in combination with Eq. (4) for calculating the “coefficient of
contribution” and resulting values presented in the column “relat.
contribut(%) in the uncertainty budget table. Eq. (3) then is no longer
valid.

With UncertRadio this procedure according to Kessel et al. is
implemented since version 0.05 (2007/11), i.e. the values shown in the
column “relat. contribut(%)“ of the uncertainty budget correspond to
this new definition. This may be demonstrated with the **example
projects** **Kessel-2a-2006.txp and Kessel-2b-2006.txp**, which were
prepared from two examples from that publication.

TAB “Results”
-------------

Under this TAB selected by mouse click the total result for the output
quantity is shown including further variables and the values of the
Decision threshold and the Detection limit. The output quantity is
indicated which this result is referring to.

These are in detail:

**the result of the measurement:**

-  the value of the output quantity

-  the expanded uncertainty, in the same unit as that of the output
      quantity

-  the relative expanded uncertainty (in %)

-  the coverage factor (can be modified in the **menu Options**)

`best estimates according to Bayes and confidence
limits <#best-estimates-according-to-bayes-and-confidence-limits>`__
**(see also ISO 11929:2019):**

-  the value of the output quantity

-  the expanded uncertainty

-  the value of the lower confidence limit

-  the value of the upper confidence limit

-  probability :math:`(1 - \gamma)` associated with the confidence
      interval

..

   The toggle button „min. Coverage interval“ can be used to switch the
   display between probabilistically symmetric and the shortest coverage
   intervals, also in the case of the MC-Simulation.

**Decision threshold and Detection limit:**

-  the value of the Decision threshold including the number of
      iterations (actually no iterations)

-  the value of the Detection limit including the number of iterations

-  the applied quantiles of the normal distribution,
      :math:`k_{1 - \alpha}` and k_beta = :math:`k_{1 - \beta}`,
      corresponding to the errors of first and second kind

**WLS, PLSQ, PMLE or WTLS: Standard uncertainties of the fitting
parameter corresponding to the output quantity from the analysis of
decay curve:**

-  the uncertainty obtained from the least squares analysis; it is
      **NOT** multiplied with :math:`\sqrt{\chi_{R}^{2}}` if the reduced
      Chi-squared value is larger than 1; this variant of the
      uncertainty of the net counting rate is used for estimating the
      uncertainty of the output quantity;

-  that value of the uncertainty of the output quantity which is
      obtained from uncertainty propagation of the arguments of the
      Linfit function (i.e., mainly the background counting rate, if
      applicable with blank contribution) and of the uncertainties of
      the gross counting rates of the decay curve

-  the value of the reduced Chi-square :math:`\chi_{R}^{2}`

A `Monte Carlo Simulation <#monte-carlo-simulation>`__ may be started as
a modern alternative to the propagation of uncertainties:

-  input of the number *N* of simulated calculations of the output
      quantity (defining one run)

-  input of the number of runs *r*

-  Optional: selection of the coverage interval of shortest length
      (shortest **c**\ overage **i**\ nterval)

The MC simulation is started by clicking the button “Start”. The
iteration number is indicated when iteratively estimating the detection
limit.

From the r-fold repetition (runs) means and relative standard deviations
(in %) are determined for:

*Best estimates according to Bayes:*

-  the output quantity

-  the expanded uncertainty

-  the relative expanded uncertainty (%)

-  the lower confidence limit

-  the upper confidence limit

*and:*

-  the Decision threshold

-  the Detection limit

A **new Button "Save values"** was introduced. It can be used to
transfer all the values being visible in the dialog, including those
obtained by MC simulation, together with project name, date/time of
execution into a CSV file: UR‑Saved-Results.csv. If this file does not
yet exist, it is opened; then, records of data are appended to that
file. The meaning of the columns is similar to those of the file
AutoReport-Result.csv; however, there are further columns for the LINFIT
parameters, for each of the output quantity, the decision threshold and
the detection.

**In an extra dialog the three distributions are displayed which have
been obtained from the Monte Carlo simulation.**

In the **menu Options** one can define the two values of the **normal
Quantiles** corresponding to the errors of first and second kind,
respectively. See also:

`Implication of changing parameters within the Options
menu <#implication-of-changes-within-the-options-menu>`__.