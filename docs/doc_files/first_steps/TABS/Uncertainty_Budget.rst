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
uncertainty budget ****has got a further plausible meaning by the recent**
**publication by**** **Kessel, Kacker and Berglund** (2006) with the title
“Coefficient of contribution to the combined standard uncertainty“:

That relative contribution of an input quantity :math:`x_{i}`,
divided by 100, is in the case of un-correlated input quantities
identical with the square of the correlation coefficient between
:math:`x_{i}` and the output quantity **y**! This quantity is now
called “**coefficient of contribution”** and is represented by the
symbol :math:`h\left( y,x_{i} \right)`.

The generalised definition of the “coefficient of contribution” is now:

.. math::
   :label: coeff_contrib_1

   h\left( y,x_{i} \right) = \frac{\left( \partial y/\partial x_{i} \right) \bullet u\left( x_{i} \right)}{u(y)} \bullet r\left( y,x_{i} \right)

As we have for non-correlated input quantities:

.. math::
   :label: coeff_contrib_2

   r\left( y,x_{i} \right) = \frac{\left( \partial y/\partial x_{i} \right) \bullet u\left( x_{i} \right)}{u(y)}

from Eq. :eq:`coeff_contrib_1` then follows Eq. :eq:`coeff_contrib_3`, which was already known - for
non-correlated input quantities - as the positive relative contribution
to the variance of the output quantity:

.. math::
   :label: coeff_contrib_3

   h\left( y,x_{i} \right) = \left\lbrack \frac{\left( \partial y/\partial x_{i} \right) \bullet u\left( x_{i} \right)}{u(y)} \right\rbrack^{2}

If correlations :math:`r\left( x_{i},x_{j} \right)` between input
quantities exist, they are inserted into Eq. :eq:`coeff_contrib_1` in the factor
:math:`r\left( y,x_{i} \right)` defined as follows:

.. math::
   :label: coeff_contrib_4

   r\left( y,x_{i} \right) = \sum_{j}^{}{\left\lbrack \frac{\left( \partial y/\partial x_{j} \right) \bullet u\left( x_{j} \right)}{u(y)} \right\rbrack \bullet \left\lbrack r\left( x_{i},x_{j} \right) \right\rbrack}

Note that this may lead now in some cases to negative values of
:math:`h\left( y,x_{i} \right)`, the coefficient of contribution.

**Notes on effects from covariances:**

If covariances are considered for the calculation of uncertainties
negative values may occur in the column “relat. contribut(%)“; this is
not a program error.

According to the mentioned paper by Kessel et al. correlations
(covariances) between input quantities are considered according to Eq.
:eq:`coeff_contrib_1` in combination with Eq. :eq:`coeff_contrib_4` for calculating the “coefficient of
contribution” and resulting values presented in the column “relat.
contribut(%) in the uncertainty budget table. Eq. :eq:`coeff_contrib_3` then is no longer
valid.

With UncertRadio this procedure according to Kessel et al. is
implemented since version 0.05 (2007/11), i.e. the values shown in the
column “relat. contribut(%)“ of the uncertainty budget correspond to
this new definition. This may be demonstrated with the **example**
**projects** **Kessel-2a-2006.txp and Kessel-2b-2006.txp**, which were
prepared from two examples from that publication.