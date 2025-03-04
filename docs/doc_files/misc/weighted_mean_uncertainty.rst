Calculation of the weighted mean and its standard uncertainty
-------------------------------------------------------------

The weighted mean :math:`\overline{A}` of activities :math:`A_{i}` of
the individual gamma lines is calculated according to the following
equation:

:math:`\overline{A} = \frac{\sum_{i = 1}^{n}{\ \frac{A_{i}}{u^{2}\left( A_{i} \right)}}}{\sum_{i = 1}^{n}{\ \frac{1}{u^{2}\left( A_{i} \right)}}}`
(1)

The standard uncertainty of the weighted mean is calculated as follows:

:math:`u\left( \overline{A} \right) = \sqrt{\frac{1}{\sum_{i = 1}^{n}{\ \frac{1}{u^{2}\left( A_{i} \right)}}}} = u_{int}\left( \overline{A} \right)`
(2)

This is termed as “internal standard” deviation. It only considers the
uncertainties of the individual activities and is Bayes compliant.

It may, however, happen, that the individual values :math:`A_{i}` show
deviations are larger than would be expected from the uncertainties
:math:`u\left( A_{i} \right)`. In order to consider also an additional
uncertainty component due to these “external” influences, the so-called
“external standard deviation” is often used being defined as follows:

:math:`u_{ext}\left( \overline{A} \right) = \sqrt{\frac{\sum_{i = 1}^{n}{\ \frac{\left( A_{i} - \overline{A} \right)^{2}}{u^{2}\left( A_{i} \right)}}}{(n - 1)\sum_{i = 1}^{n}{\ \frac{1}{u^{2}\left( A_{i} \right)}}}}`
(3)

Note, that this type of standard deviation is, however, no longer Bayes
compliant.

The factors :math:`1/u^{2}\left( A_{i} \right)` within the sums
contained in the equations (1-3) represent statistical weights. These
must be considered as being constant. Nevertheless, before they are
applied, they are calculated from other variable values
:math:`x_{in}(j)`, which also contribute to the :math:`A_{i}`. If, after
having calculated Eq. (1), followed by a numerical uncertainty
propagation for :math:`\overline{A}` with respect to the
:math:`x_{in}(j)` by using differences quotients follows, the values
:math:`u^{2}\left( A_{i} \right)` must not be modified. Under this
constraint the uncertainty propagation for Eq. (1) directly yields the
uncertainty given by Eq. (2). This condition is considered since version
2.4.05. The values :math:`u^{2}\left( A_{i} \right)` may only be
recalculated within the iterations for calculating the decision
threshold and the detection limit, once per single iteration step.

**UncertRadio is calculating both, internal and external standard
uncertainty but for further internal calculations it makes only use of
the internal standard uncertainty.** The external standard deviation is
shown only for information. The reason for doing that is that for the
user it might be an useful information that the external standard
deviation is considerably larger than the internal standard deviation.
The latter can give advice about possible sources of errors which should
be considered.

Covariances between peak efficiency values, taken from the same
efficiency curve, are part of the covariance matrix **U\ x** of the
input values **x**:

:math:`cov\left( A_{m},A_{k} \right) = \frac{A_{m}}{\varepsilon_{m}}\frac{A_{k}}{\varepsilon_{k}} \bullet cov\left( \epsilon_{m},\epsilon_{k} \right)`

**Important note**: According to Cox et al. (2006b), the equations for
the weighted mean and its uncertainty given here can only be considered
as “good” approximations, if covariances exist between the individual
activity values. In such a case, instead, a least-squares procedure for
the mean is to be applied. The corresponding procedure is described
`least-squares-calculation-of-a-weighted-mean-and-its-standard-uncertainty`.