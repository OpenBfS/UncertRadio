Notes on linear curve-fitting using general least squares (WTLS)
----------------------------------------------------------------

The so-called “general case” of the method of least squares (WTLS) may
be used in UncertRadio in such cases where uncertainties but also
covariances are attributed to the :math:`X_{i}\left( t_{k} \right)`
values. It is the only method known to the author which allows the
inclusion of such uncertainties and covariances.

For its realisation matrix routines from the Datan-Library are applied
(converted to FORTRAN 90), the main subroutine being LSQGEN:

Datan-Library from:

Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und
Computerprogrammen; 4. Auflage. Spektrum, Akademischer Verlag,
Heidelberg-Berlin. In German.

This text book is also available in an English version.

Although the model continues to be linear with respect to the fitting
parameters, WTLS requires an iterative procedure. Therefore, starting
values of the fitting parameters are obtained from a preceding call to
the simpler LSQ routines.

Unfortunately, the mathematics used in the routine LSQGEN is rather
complicated, which is the reason that a description cannot yet be given
here. Another routine precedes LSQGEN in UncertRadio which assembles the
input data in a special form which is required by LSQGEN. Measured net
counting rates :math:`Y\left( t_{k} \right)` and the different
:math:`X_{i}\left( t_{k} \right)\ `\ values are combined in one common
vector y; the same applies to the uncertainties and covariances of the
:math:`Y\left( t_{k} \right)` and :math:`X_{i}\left( t_{k} \right)`
values, which results in a common input covariance matrix cy. The rank
of the vector :math:`\mathbf{y}` and of the quadratic matrix cy may
easily increase to a significant value. If, for example, a LSC
measurement with 3 counting channels is considered and 10 measurements
are done, one obtains:

   3x10 = 30 values :math:`Y\left( t_{k} \right)` (10 values for each of
   the channels A, B and C)

   3x3x10 = 90 values :math:`X_{i}\left( t_{k} \right)` (3 values
   :math:`X_{i}\left( t_{k} \right)` associated with each of the values
   :math:`Y\left( t_{k} \right)`)

Therefore, the rank will have the value 30+90=120. This means, that the
input covariance matrix becomes a 120x120 matrix, which makes it easily
plausible that the iteratively working routine LSQGEN may indeed become
more time-consuming.

**Note on the calculation of Decision threshold and Detection limit:**

To be comparable now with the description of the LLSQ mathematics the
result vector of the fitted parameters will here be designated as
:math:`\mathbf{y}`.

The special output quantities to be considered here refer to the
component L of the result vector :math:`\mathbf{y}` of the fitted
parameters, :math:`y_{L}`, and its uncertainty
:math:`u\left( y_{L} \right)`, which is iterated until :math:`y_{L}` and
:math:`u\left( y_{L} \right)` fulfil the terminating condition of the
iteration. :math:`y_{L}` is then replaced by one new value
:math:`y_{L}^{'}\ `\ determined by the iteration. Using this one, one
can calculate new values of the **gross counting rates** of the decay
curve, where L=1 was set without losing generality:

:math:`R_{b,i}^{'} = \left( R_{0,i} + R_{bl} \right) + y_{1}^{'} \bullet X_{1}\left( t_{i} \right) + y_{2} \bullet X_{2}\left( t_{i} \right) + y_{3} \bullet X_{3}\left( t_{i} \right)`
.

From this the **uncertainty function (standard uncertainty) of the gross
counting rate** results:

:math:`u\left( R_{b,i}^{'} \right) = \sqrt{R_{b,i}^{'}/t_{m,i}}` **.**

The net counting rates of the modified decay curve, i.e. the new
:math:`Y'\left( t_{k} \right)`, then are:

:math:`R_{n,i}^{'} = R_{b,i}^{'} - R_{0,i} - R_{bl}` ,

from which the diagonal elements of the varied covariance matrix result:

:math:`var\left( R_{n,i}^{'} \right) = \frac{R_{b,i}^{'}}{t_{m,i}} + var\left( R_{0,i} \right) + var\left( R_{bl} \right)`,

while the non-diagonal elements are left unchanged.

With these modified values and uncertainties of the net counting rates
the evaluation for the output quantity is repeated yielding the new
uncertainty value :math:`u\left( y_{1}^{'} \right)`. Now, the
convergence criterion can be tested; if it is not yet fulfilled, the
next iteration step is initiated by determining the next iteration value
:math:`y_{1}^{''}` from the pair :math:`y_{1}^{'}` and
:math:`u\left( y_{1}^{'} \right)`.