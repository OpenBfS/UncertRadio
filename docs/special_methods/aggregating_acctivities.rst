Aggregating activities of several aliquots
------------------------------------------

In certain cases, the measurement of a sample activity requires to
determine the output quantity value from several compartments of the
sample, or, regarding a surface contamination, from several measurements
covering the entire surface.

This chapter describes how to proceed if several activity measurements,
also of different types of measurements, need to be aggregated to obtain
a single output quantity value. This value may be calculated as a sum or
as an average of the single values. The simple aggregation method as
described below also includes the calculation of the associated decision
threshold and the detection limit.

If these measurements, however, represent repeated measurements in order
to obtain a series of measurement values for a single input quantity,
the recommended methods would be those described in section
:ref:`using data sets for mean and variance` and
:ref:`Gross quantity: Variance interpolation for a mean`.

**Activating the evaluation of several aliquot measurements**

In the text field for equations, the following call is inserted for
defining the activity as an aggregation of several values:

*Asum = SumEval(mode, np, A1, A2, …)*

The name of the symbol to the left of the = sign may be freely chosen.
The name **SumEval** represents the internal procedure which does the
calculations necessary for the aggregation of measured aliquot values.
Its arguments are:

   *mode* integer number, withe values:

   1: calculate the mean value of the individual results,

   2: calculate the sum of the individual results,

   *np* integer, number of aliquot measurements

   *A1, A2, …* the list *np* of symbol names (freely chosen)
   representing the activity or activity concentration values

Directly following the SumEval call, at first those main equations for
calculating the activities Ai are inserted,

Ai = wi \* Rneti , for i=1 to np, one after another

These are followed by the lists of equations defining the calibration
factors wi and the net count rates Rneti:

Rneti = Rbi – R0i

It is recommended to use further equations for explaining the count
rates by their associated numbers of counts.

A complete example for two aliquot measurements may be defined as
follows:

   *a = 1/F \* Asum*

   *Asum = SumEval(1, 2, A1, A2)*

   *A1 = w1 \* Rnet1*

   *A2 = w2 \* Rnet2*

   *W1 = 1/eps1*

   *W2 = 1/eps2*

   *Rnet1 = Rb1 – R0*

   *Rnet2 = Rb2 – R0*

   *Rb1 = Nb1 / tb*

   *Rb2 = Nb2 / tb*

   *R0 = N0 / t0*

Such factors found in all expressions of wi, may be extracted from the
wi, i.e., not included in SumEval, as for instance the factor 1/F (1 /
surface area) in the equation above that declaring Asum. This helps
preventing covariances between the wi. An input quantity being part of
several equations generates covariances between the quantities defined
by these equations. This is true for the count rate R0 in the example
given above, introducing a covariance between Rnet1 und Rnet2.

Such covariances, however, need not be identified explicitly by the
user. They are considered by the uncertainty propagation applied within
the *SumEval* procedure in the way, that covariance contributions of the
form

.. math:: u\left( x_{a,i},x_{a,j} \right) = \sum_{k}^{}{\frac{\partial x_{a,i}}{\partial x_{u,k}}\frac{\partial x_{a,j}}{\partial x_{u,k}}u^{2}\left( x_{u,k} \right)}

induced by the independent input quantities :math:`x_{u,k}` between
dependent quantities :math:`x_{a,i}`, are taken into account; refer to
:numref:`uncertainty propagation`.

**Note**: This procedure does not require further windows dialogs.

**Notes about calculating the decision threshold and the detection
limit**

Calculations of the decision threshold and especially the detection
limit require to vary the value :math:`a` of the output quantity. Such
an iteration step generates a modified value, denoted as :math:`a'`.
This has to be transformed to new values :math:`A_{i}^{'}` of the
individual values :math:`A_{i}` as part of *SumEval.* Two possible ways
may be applied, which, based on the sample equations given above, are
explained below.

If a mean value is to be calculated from the :math:`A_{i}`, a meaningful
option would be to set all :math:`A_{i}^{'}` to the same value
:math:`a'`. The least-squares method is used as indicated in `section
7.14 <#least-squares-calculation-of-a-weighted-mean-and-its-standard-uncertainty>`__
for calculating a weighed mean.

If instead a sum of aliquot values is to be derived, it may be
meaningful, to modify the values :math:`A_{i}^{'}` such that the
original ratios between the :math:`A_{i}` values are maintained. This
may be achieved by applying relative “form“ factors :math:`h_{i}`

:math:`h_{i} = \frac{A_{i}}{a = \sum_{j = 1}^{np}A_{j}}`

such that

:math:`{A_{i} = h}_{i}a`

Then, the modified values :math:`A_{i}^{'}` – and thereby the gross
count rates :math:`R_{b,i}^{'}` – are internally calculated from
:math:`a'`:

:math:`Asum^{'} = a^{'}\ F`

:math:`R_{net,i}^{'} = \frac{Asum^{'} \bullet h_{i}}{w_{i}}`

:math:`R_{b,i}^{'} = \frac{Asum^{'} \bullet h_{i}}{w_{i}} + R_{0}`

:math:`N_{b,i}^{'} = \left( \frac{Asum^{'} \bullet h_{i}}{w_{i}} + R_{0} \right)\ t_{m}`

The associated uncertainties then are, again referring to the complete
example given above:

:math:`u\left( N_{b,i}^{'} \right) = \sqrt{N_{b,i}^{'}}`

:math:`u\left( R_{b,i}^{'} \right) = \sqrt{R_{b,i}^{'}/t_{m}}`

:math:`u\left( R_{n,i}^{'} \right) = \sqrt{R_{b,i}^{'}/t_{m} + R_{0}/t_{0}}`

From the uncertainties of the modified gross count rates, the
uncertainty :math:`u(a^{'})` associated with the activity value
:math:`a'` is calculated. Such pairs :math:`(a^{'},\ u(a^{'}))` are used
for the iteration necessary for the detection limit calculation.

**Example project:** sumEval_sum_EN.txp, sumEval_mean_EN.txp