Monte Carlo Simulation
----------------------

Under the :ref:`tab “results”`, the user can start
a **Monte Carlo simulation** for
cross-checking the value and the uncertainty of the output quantity
**y**.

The simulation is done for a chosen (large) number of simulations of the
measurement. For this purpose, for each of those quantities having been
defined in the symbol list under the TAB “Equations” and characterized
there as independent (u) input quantities, simulated input values are
taken from their correspondent distributions (normal, rectangular or
triangular distributions). The underlying individual distributions have
been determined under the TAB “Values, uncertainties”.
If the :ref:`low-level applications, (n+x)-rule` has been selected there for
counting numbers, their associated values are MC sampled according to a
Gamma distribution; the values of the counting rates derived from the
counting numbers then are also Gamma distributed. If all these input
quantities have got a simulated value, a value of the output quantity -
“the first simulated value of **y**\ ” - is calculated according to the
equations already defined.

From the many-fold repetition of this step a statistical distribution of
the values of the output quantity is obtained from which its best
estimate and its associated standard uncertainty are calculated as
arithmetic mean and standard deviation, respectively. At present, with
this method only quantities having normal, rectangular or triangular
distributions can be considered.

`Obtaining MC distributions and statistics derived of it in
detail <#obtaining-mc-distributions-and-statistics-derived-of-it-in-detail>`__

The great advantage of this method is that partial derivatives with
respect to the independent quantities are not needed!

**Note**: the random generator used here has a period of about
:math:`10^{18}`.

The procedure just described is correct for the case that no
correlations exist between the input quantities. If in the TAB “Values,
uncertainties”, however, covariances between **pairs of correlating
quantities** have been given, correlated simulated values must be
attributed to these quantities.

**Important note on this issue**: Since the UncertRadio version 0.05
(2007/11) this method of producing correlated variables was completely
adapted to methods described in textbooks where matrix methods are used
(keyword: “Gaussian distributed random numbers in n dimensions”; S.
Brandt, Datenanalyse; V. Blobel and E. Lohrmann, Statistische und
numerische Methoden der Datenanalyse; as well as the Draft of the new
Supplement 1 of ISO GUM). This can be proven by trying the UncertRadio
example project files **Kessel-2a-2006.txp and Kessel-2b-2006.txp** from
the recent publication by Kessel et al. (2006)
(see also: :ref:`Meaning of the TAB “Uncertainty budget” <tab “uncertainty budget”>`).

Intermediate results of the MC simulation, partially consisting of
tables, are now (Version 2.2.11) collected in a separate text file
MC_Tables.txt.

Generally, one will find a good agreement between the results from the
MC simulation method and from the analytical method. **Therefore, the MC
method is a relatively easy and elegant alternative to the more
extensive analytical procedure**.

**What can be followed from deviations between the two methods?**

If by using the MC method a result is obtained which deviates from that
by the analytical method, one could easily conclude that there could be
an error somewhere in the analytical procedure. However, this conclusion
not always needs to be true!

**What the described MC method actually is calculating can be
interpreted as “Propagation of distributions”.** This means that it in
principle the expectation value of the output quantity is estimated,
i.e. the following *n*-fold integral:

.. math:: y = \iiint_{n}^{}{F\left( x_{1},x_{2},\ldots,x_{n} \right) \bullet \left\lbrack \varphi_{1}\left( x_{1} \right) \bullet \varphi_{2}\left( x_{2} \right)\ldots\varphi_{n}\left( x_{n} \right)\  \right\rbrack \bullet dx_{1} \bullet}dx_{2}\ldots\ dx_{n}

Herein, :math:`F\left( x_{1},x_{2},\ldots,x_{n} \right)` designates in
compact form the equations which are necessary for the calculation of
**y** (i.e. the formula with which the value of **y** is usually
calculated) and :math:`\varphi_{i}\left( x_{i} \right)` the probability
density functions of the *n* input quantities :math:`x_{i}`
characterized as independent. The widths of the
:math:`\varphi_{i}\left( x_{i} \right)` are determined from their
associated measurement uncertainties :math:`u\left( x_{i} \right)`. An
important assumption for the conventional propagation of uncertainties
is that the uncertainties of the :math:`x_{i}` should be small. In this
case, the probability density functions
:math:`\varphi_{i}\left( x_{i} \right)` approximately become delta
functions with the consequence that the *n*-fold integral reduces to the
conventionally calculated value
:math:`F\left( x_{1},x_{2},\ldots,x_{n} \right)` of the output quantity
**y**. In this sense deviations between both methods may occur with
respect to the output quantity and its uncertainty if any of the
involved uncertainties “are not small” which might also be if such
quantities belong to the denominator of the evaluation equation
(non-linearity).

The collection of project files contains **examples in which the
discussed deviation between both methods is significant**:

+--------------------------+-------------------------------------------+
| **Project file**         | **Special feature**                       |
+--------------------------+-------------------------------------------+
| ISO-Example-1a_EN.txp,   | Here, the alpha self-absorption factor    |
| ISO-Example-1b_EN.txp    | *f*, having a rather broad rectangular    |
|                          | distribution and belonging to the         |
|                          | denominator, causes this effect.          |
+--------------------------+-------------------------------------------+
| Neut                     | The field specific correction K with a    |
| ron-Dose-Cox-2006_EN.txp | significantly broad rectangular           |
|                          | distribution has practically the same     |
|                          | effect as in the wipe test example above: |
|                          | the resultant distribution is             |
|                          | significantly asymmetric.                 |
+--------------------------+-------------------------------------------+
| Calibration-o            | In this example a significantly larger    |
| f-weight-Cox-2001_EN.txp | measurement uncertainty results from the  |
|                          | MC method. Rectangular distributions are  |
|                          | attributed to three of the involved input |
|                          | quantities.                               |
+--------------------------+-------------------------------------------+
| Wuebbeler-Ex1_EN.txp     | A non-linear model function in            |
|                          | combination with large uncertainties of   |
|                          | normal distributed input quantities       |
|                          | result in an asymmetric distribution of   |
|                          | the output quantity.                      |
+--------------------------+-------------------------------------------+
| Wuebbeler-Ex2_EN.txp     | Rectangular distributed input quantities  |
|                          | result in a trapezoidal distribution of   |
|                          | the output quantity.                      |
+--------------------------+-------------------------------------------+
