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