Approach of calculating Decision threshold and Detection limit for Gamspk1
--------------------------------------------------------------------------

The iterative calculation of the Decision threshold and the Detection
limit is performed after the mean :math:`\overline{A}` has been
calculated from the single values :math:`A_{i}`. The iteration is done
by variation of the mean where a varied value is :math:`A'`. Then, all
single values of the source activity :math:`A_{i}` are replaced by the
new (fictive) value of :math:`A'`. From the equation defining
:math:`A_{i}` :

.. math::
   A_{i} = R_{ni}\frac{f_{att,i\ } \cdot \ f_{coinsu,i}}{\epsilon_{i}{\  \cdot \ p}_{\gamma i}\ }
   :label: eq_gamspk1_1

one obtains by inverting this equation

.. math::
   R_{ni} = A_{i}\left( \frac{\epsilon_{i}{\  \cdot \ p}_{\gamma i}}{f_{att,i\ } \cdot \ f_{coinsu,i}}\ \  \right)
   :label: eq_gamspk1_2a

and from this with the replacement :math:`A_{i} = A'\ ` an equation for
the (fictive) net counting rates associated with the varied value
:math:`A'` :

.. math::
   R_{ni}^{'} = A'\left( \frac{\epsilon_{i}{\  \cdot \ p}_{\gamma i}}{f_{att,i\ } \cdot \ f_{coinsu,i}} \right)
   :label: eq_gamspk1_2b

The aim is now to determine the uncertainties of the :math:`R_{ni}^{'}`,
then, via uncertainty propagation in accordance with :eq:`eq_gamspk1_1`, the
uncertainties of the single activity values :math:`A_{i} = A'\ `\ and,
finally, with the chosen method for the mean to derive the uncertainty
:math:`u(A')` of the (iterated or fictive) mean value :math:`A'`.

The following ansatz (a separation) is chosen for the uncertainties
:math:`u\left( R_{ni}^{'} \right)\ `\ (`see
also <#dialog-values-from-spectrum-evaluation>`__):

.. math::
   u^{2}\left( R_{ni} \right) = \left\lbrack \frac{R_{ni}}{tlive} \right\rbrack + \frac{R_{T}}{tlive}f_{B} + \frac{R_{bg}}{tlive} + u^{2}\left( R_{bg} \right)
   :label: eq_gamspk1_3

Herein, only the first term is related directly to the contribution from
the sample activity. The remaining terms represent uncertainty
contributions of those parameters which characterize the background of
the *i*-th gamma line including also a contribution from a “peak in the
background”.

By using the last equation now with the equations :eq:`eq_gamspk1_2b`, :eq:`eq_gamspk1_3` and :eq:`eq_gamspk1_1` the
(iterated or fictive) activities of the single gamma lines and their
uncertainties can be determined as indicated already above. After these
calculations those values are available which are necessary to go to the
next iteration step and to test also for convergence of the iteration.

**Important note:**

External“ influences may exist leading to calculated values
:math:`A_{i}` of the single gamma lines which may exhibit a spreading
which may be larger than to be expected from the uncertainties of single
values.

   This effect can be found with the **weighted mean** if the “external”
   is significantly larger than the “internal” standard uncertainty or
   the value of the “reduced Chi-square” significantly larger than one
   is.

   In the case of the **arithmetic mean with additive correction** this
   may be inferred if the correction :math:`C` and particularly its
   uncertainty :math:`u(C)` lead to a significant shift of the results
   compared to the arithmetic mean (uncorrected) or its uncertainty.

These influences usually are not considered in the evaluation model
given by :eq:`eq_gamspk1_1`. Determining Decision threshold and Detection limit
requires iteration of the activity values. The inversion of that
equation, i.e. calculating the net count rates :math:`R_{ni}^{'}`
according to :eq:`eq_gamspk1_2b` to be expected for a given (iterated) value of the
activity :math:`A'`, leads directly to the elimination of that
“external” influence. Then, from the obtained net counting rates
:math:`R_{ni}^{'}` single activity values result from :eq:`eq_gamspk1_1` having all
the same identical value :math:`A'`, i.e. their spreading is equal to
zero!

This means for the calculation of Decision threshold and Detection limit
that the external effect which may have been found from the primary
evaluation of the output quantity in this latter case does not come into
effect. Insofar, the usability of the external standard deviation with
the weighted mean or with the NIST-2004 method is low, at least
regarding Decision threshold and Detection limit.
