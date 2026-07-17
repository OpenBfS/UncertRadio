PMLE procedure for linear unfolding
-----------------------------------

PMLE in UncertRadio
^^^^^^^^^^^^^^^^^^^

The procedure for Poisson MLE (PMLE) requires that the dependent
quantities in unfolding, e.g. gross counts of a decay curve, are
Poisson-distributed. As the latter are applied as net count rates for
the other methods, these net count rate values are converted to gross
counts within the program. The gross counts of a decay curve

:math:`N_{b,i} = \left( R_{b,i} + R_{0,i} + R_{bl} \right) \cdot t_{m}`
; :math:`u\left( N_{b,i} \right) = \sqrt{N_{b,i}}`

are considered as non-correlated. :math:`t_{m}` must have the same value
for all measured count rates, otherwise the shape of the gross counts
decay curve would be disturbed.

If for instance an Y-90 decay curve shall be fitted, the corresponding
model equation for the net count rate representation

:math:`R_{b,i} = y_{1} \cdot X_{1}\left( t_{i} \right)`

converts to the model of the PMLE fit:

:math:`N_{b,i} = \left( y_{1} \cdot X_{1}\left( t_{i} \right) + y_{2} \cdot 1 \right) \cdot t_{m}`

This requires inferring a second fitting contribution with the fitting
parameter :math:`y_{2}` and :math:`X_{2}\left( t_{i} \right) = 1`. The
parameter :math:`y_{2}` represents the sum of a background and a blank
value, :math:`\left( R_{0,i} + R_{bl} \right)t_{m};` it will be fitted
by the PMLE procedure and may end up with a value which can deviate from
the value :math:`\left( R_{0,i} + R_{bl} \right)t_{m}` known from
measurement.

**Features:**

This procedure needs to have one fitting parameter more than the other
procedures used for fitting net count rate decay curves. In UncertRadio
it can therefore be applied only if not more than two physical
components are to be determined. Both of these two components should
have the property that their associated curves should be different from
being constant or quasi-constant within time. If one of these components
represents the contribution of a radionuclide with a rather large
half-live, such that the decay curve fails to show a decrease, the
fitting procedure cannot differentiate between this contribution and
that of :math:`\left( R_{0,i} + R_{bl} \right)t_{m}`. In such a case the
PMLE-procedure cannot be used.

Furthermore, the PMLE procedure can at present not be applied, if
another fitting parameter is used with the status „fixed“, as is the
case for a Sr measurement where a Sr-85 tracer was added to the sample.

The PMLE procedure is not selectable in a case with too few countings
compared to the number of parameters to be fitted.

**Applying this procedure:**

The data within the dialog “Input of decay curve“ are to be handled in
the same way as for the other fitting procedures; nothing changes there.

Has a project already be established for the use with e.g. the WLS
procedure, the program has already sufficient information about the
components to be fitted and is able to decide whether the
above-mentioned criteria for applying PMLE are fulfilled. If the
criteria are not fulfilled, the selection of the PMLE procedure in the
model dialog “model of decay curve“ is prevented

Is the PMLE procedure selectable within the model dialog, the procedure,
if selected, is processed in the same manner as for the other
procedures. Only within the view of the fit result (|FittingResults_24|), gross
counting rates are displayed instead of net count rates.

.. |FittingResults_24| image:: /icons/FittingResults_24.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

Basic information about PMLE
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For decay curves of gross counts, which are Poisson-distributed, the Poisson MLE
(PMLE) fitting method is the better one compared to weighted least squares (WLS),
if the count numbers are rather small. This is supported by corresponding
test results given at the end of :numref:`chi-square options`. Unfortunately, only non-linear
unfolding procedures can be applied. For this purpose, the Levenberg-Marquardt-method was
applied as it is implemented in the Matlab routine Lm, published by H. P. Gavin (2022).
The Matlab code was converted to Fortran and extended for the application to PMLE:

- PMLE with its special definition of the chi-square (see :ref:`chi-square options`)
  requires a modification of the Levenberg-Marquardt (LM) matrix algebra,
- For stabilizing the fitting („penalized fitting“) an additional
  Chi-square term is included, which shall prevent the fitting parameters
  from moving too far from their start values during the non-linear iteration.


Referring to the nomenclature used by H. P. Gavin in his paper, the following table
gives a short information about the extensions of his mathematical treatment implemented
here. In the first column, the equation numbers used by Gavon are cited.
The main difference between Levenberg-Marquard formulae (left column) and those for
the PMLE modifications (right column) consists in the applied covariance matrices :math:`W_0`
(left column) and :math:`W_1` and :math:`W_2` (right column).
The additional term :math:`p_f(p-p_c)^T \cdot W_c \cdot (p-p_c)` serves for stabilizing the fit.
:math:`J` designates the Jacobi-Matrix of the first partial derivatives
of the fitting function :math:`f` with respect to the parameters :math:`p`.

.. list-table::
    :widths: auto

    * - Eq.
      - LM-procedure by Gavin, with stabilization included
      - LM-PMLE-procedure by Gavin, modified for PMLE and stabilisation
    * - (2)
      - :math:`\chi^{2}_c(p) = \chi^{2}(p) + p_f(p-p_c)^T \cdot W_c \cdot (p-p_c)`
      - :math:`\chi^2{2}_c(p)=\chi^{2}_{LP} + p_f(p-p_c)^T \cdot W_c \cdot (p-p_c)`
    * - (6)
      - :math:`\frac{\partial \chi^{2}_c}{\partial p} = \frac{\partial \chi^{2}}{\partial p} + p_f 2 (p-p_c)^T \cdot W_c`
      - :math:`\frac{\partial \chi^{2}_c}{\partial p} = \frac{\partial \chi^{2}_{LP}}{\partial p} + p_f 2 (p-p_c)^T \cdot W_c`
    * - (11)
      - :math:`[J^T W_0 J + p_f W_c] \cdot h_{gn} = J^T W_0 (y-f(p))`
      - :math:`[J^T W_2 J + p_f W_c] \cdot h_{gn} = J^T W_1 (y-f(p))`
    * - (13)
      - :math:`[J^T W_0 J + p_f W_c + \lambda \cdot diag(J^T W_0 J + p_f W_c)] h_{lm}=`
      - :math:`[J^T W_2 J + p_f W_c + \lambda \cdot diag(J^T W_2 J + p_f W_c)] h_{lm}=`
    * - (13+)
      - :math:`=J^T W_0 (y-f(p))`
      - :math:`=J^T W_1 (y-f(p))`
    * - (16)
      - :math:`\rho_i(h_{lm})=\frac{\chi^2(p) - \chi^2(p+h_{lm})} { h^T_{lm} {\lambda\cdot diag(J^T W_0 J + p_f W_c)} h_{lm} + J^T W_0 d }`
      - :math:`\rho_i(h_{lm})=\frac{\chi^2(p) - \chi^2(p+h_{lm})} { h^T_{lm} {\lambda\cdot diag(J^T W_2 J + p_f W_c)} h_{lm} + J^T W_0 d }`
    * - (16+)
      - :math:`[J^T W_0 d] = J^T W_0 (y-f(p)) - p_f W_c (p-p_c)`
      - :math:`[J^T W_1 d] = J^T W_1 (y-f(p)) - p_f W_c (p-p_c)`
    * -
      - :math:`W_0 = diag(1/y_i)`
      - :math:`W_1 = diag(1/f_i)`
    * -
      - :math:`W_c = diag(1/u^2(p_{ci}))`
      - :math:`W_2 = diag(y_i/f^2_i)`



**Test with MC simulations**

The performance of the procedure shall be demonstrated for a Y-90 decay curve with 9
measurements (example project vTI-Y90-16748_BLW_V2_EN.txp).
This project was modified by increasing the gross count number of the 9th point
from a shorter counting duration (28200 s) to the duration of the other points (72000 s).
A separate Fortran routine was used for the necessary MC simulations.
The model for PMLE fitting of the gross counts :math:`N_{b,i}` is given
by :math:`y_1`: parameter of the Y-90-contribution; :math:`y_2`: parameter für the sum UG of
background counts and the net blank counts (72000 s):

.. math:: N_{b,i} = y_1 \cdot X_1(t_i) + y_2 \cdot 1

:math:`X_1(t_i)` means the Y-90 decay corrections for 9 times :math:`t_i`.
For start values of counts :math:`(y_1,y_2) = (1040,1344)` , 9 “true” values of :math:`N_{b,i}`
were calculated according to the latter equation.
After replacing them by Poisson distributed random counts, these were fitted resulting in values :math:`(y^{'}_1,y^{'}_2)`.
This step was repeated :math:`10^6` times. This allowed to extract statistical information,
separately for :math:`(y^{'}_1)` and  :math:`(y^{'}_2)`:

.. list-table::
    :widths: auto

    * - true
      - given start value
    * - meanp
      - mean of the MC-values
    * - sdp
      - MC standard deviation of the  values estimating the dispersion
    * - meansd
      - mean of the standard deviations estimated by the fitting routine for each of the :math:`10^6` fits


The sequence of steps described so far is repeated 8 times by dividing the
start values by two each time. All this was executed three times, for the
fitting methods PMLE (non-linear) and PLSQ and WLS (linear). The statistical
data obtained are presented in the following table.
Overall, the given true values are quite well reproduced by meanp;
for smaller count numbers only for the WLS procedure some deviations are observed.
A good agreement of the values for sdp (“real dispersion”) and meansd
(standard deviation estimated by the fitting procedure) means statistical consistency.
It is observed that again the WLS procedure is less consistent for lower count numbers.
The evaluation of this test also demonstrates that the PMLE method yields
lower but statistically consistent estimates of the parameters describing counts.

.. image:: /images/image754.png
