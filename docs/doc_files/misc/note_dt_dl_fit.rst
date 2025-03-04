Note on Decision threshold and Detection limit for linear fitting
-----------------------------------------------------------------

At first, for the equation used in the Least squares fitting

:math:`Y\left( t_{k} \right) = a_{1} \bullet X_{1}\left( t_{k} \right) + \ a_{2} \bullet X_{2}\left( t_{k} \right) + \ a_{3} \bullet X_{3}\left( t_{k} \right)`

it must be defined, which of the fitting parameters :math:`a_{i}`
corresponds to the actually valid output quantity. For the following
notes let us assume that the parameter :math:`a_{1}` is the one
representing the output quantity.

The procedure for calculating the Decision threshold and the Detection
limit follows ISO 11929:2010. The parameter :math:`a_{1}` is modified by
iteration. At first, the parameter :math:`a_{1}` in the above equation
(i.e. the value of the Y-90 counting rate in the case of a Y-90
decay-curve) is replaced by an iterated value :math:`a_{1}^{'}` while
all other values remain unchanged. From this, new measured net counting
rates :math:`Y^{'}\left( t_{k} \right)` are calculated as well as its
new uncertainties :math:`u\left( Y'\left( t_{k} \right) \right)` (the
uncertainty function). With these new values, i.e.
(:math:`a_{1}^{'}`,\ :math:`Y^{'}\left( t_{k} \right)`,\ :math:`\ u\left( Y'\left( t_{k} \right) \right)`),
the least squares analysis calculations are repeated yielding the
uncertainty :math:`u\left( a_{1}^{'} \right)` of the iterated parameter
value :math:`a_{1}^{'}`. With this pair of values
(:math:`a_{1}^{'}`,\ :math:`\ u\left( a_{1}^{'} \right)`) it is then
tested whether the termination condition of the iteration procedure is
fulfilled; if not, the next iterated value :math:`a_{1}^{''}` is
determined and the above procedure is repeated in order to find its
uncertainty :math:`u\left( a_{1}^{''} \right)`; and so on.

A more detailed description of these calculations while iterating can be
found at the end of the following help topics:

   a) :ref:`Mathematics of linear curve fitting with WLS <Mathematics of the linear LSQ curve fitting with correlated measured values>`,

   b) :ref:`Mathematics of linear curve fitting with WTLS <Notes on linear curve-fitting using general least squares (WTLS)>`.