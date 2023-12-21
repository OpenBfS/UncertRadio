Special Methods
===============

Uncertainty propagation
-----------------------

Methods without linear unfolding
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An equation, or a system of several equations describing the evaluation
of the measurement quantity, is called **evaluation model**. It is
stated in the form

:math:`Y = G\left( X_{1},\ldots,\ X_{m} \right)` (1)

Inserting estimates for the input quantities yields an estimate of the
output quantity, the **primary measurement result** **y**:

:math:`Y = G\left( x_{1},\ldots,\ x_{m} \right)` (2)

The standard uncertainty :math:`u(y)` associated with the primary
measurement result :math:`y` is calculated according to the following
equation, assuming that measured values of the input quantities are
statistically independent:

:math:`u^{2}(y) = \sum_{i = 1}^{m}\left( \frac{\partial G}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right)`
. (3)

This is known as **uncertainty propagation**. The partial derivatives
are also known as **sensitivity coefficients**. It is often written such
that *G* is replaced by *Y*:

:math:`u^{2}(y) = \sum_{i = 1}^{m}\left( \frac{\partial Y}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right)`

If the input quantities have been measured in a way that they are not
statistically independent, associated covariances have to be taken into
account. Then, the extended previous equation reads:

:math:`u^{2}(y) = \sum_{i = 1}^{m}\left( \frac{\partial G}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right) + 2\sum_{i = 1}^{m - 1}{\sum_{j = i + 1}^{m}{\frac{\partial G}{\partial x_{i}}\frac{\partial G}{\partial x_{j}}}u\left( x_{i},x_{j} \right)}`
(4)

Here, :math:`u\left( x_{i},x_{j} \right)` represents a more general way
of stating a covariance between two measured input quantities, which
often are also given as :math:`cov\left( x_{i},x_{j} \right)` or
:math:`covar\left( x_{i},x_{j} \right)`, respectively. The following
holds: :math:`u\left( x_{i},x_{i} \right) = u^{2}\left( x_{i} \right)`
and :math:`u\left( x_{i},x_{j} \right) = u\left( x_{j},x_{i} \right)`.

Equation (3) is the basic form of the uncertainty propagation used in UR
for methods not requiring linear unfolding. This is done by using the
values and standard uncertainties defined under the TAB “Values,
Uncertainties“. In case that covariances have also been declared in this
TAB, Equation (4) is applied.

Extension to several output quantities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Equation (4) above can also be written as follows:

:math:`u^{2}(y) = \sum_{i}^{}{\sum_{j}^{}{\frac{\partial G}{\partial x_{i}}\frac{\partial G}{\partial x_{j}}u\left( x_{i},x_{j} \right)}}\ `.
(5)

Furthermore, by extending this to the case of linear unfolding for more
than one output quantity (y then gets indices :math:`l` and :math:`k`)
this equation becomes:

:math:`u^{2}\left( y_{l},y_{k} \right) = \sum_{i}^{}{\sum_{j}^{}{\frac{\partial Y_{l}}{\partial x_{i}}\frac{\partial Y_{k}}{\partial x_{j}}u\left( x_{i},x_{j} \right)}}\ `
, (6)

There exists an equivalent of this equation in matrix algebra notation,
which indeed is applied in UR. It assumes an *n*-Vector
:math:`\mathbf{X}` and an *m*-Vector **Y**, associated with an
*n*\ ×\ *n* covariance matrix :math:`\mathbf{U}_{\mathbf{x}}` and an
*m*\ ×\ *m* covariance matrix :math:`\mathbf{U}_{\mathbf{y}}`,
respectively. Introducing furthermore an *m*\ ×\ *n* matrix
:math:`\mathbf{Q}` with elements
:math:`Q_{i,k} = \partial Y_{i}/\partial x_{k}\ `, i.e., partial
derivatives,

:math:`\mathbf{Q =}\left\lbrack \begin{array}{r}
\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{\ \ }\frac{\partial Y_{1}}{\partial x_{2}}\mathbf{\ \ \ \ldots\ }\frac{\partial Y_{1}}{\partial x_{n}} \\
\frac{\partial Y_{2}}{\partial x_{1}}\mathbf{\ \ }\frac{\partial Y_{2}}{\partial x_{2}}\mathbf{\ \ \ \ldots\ }\frac{\partial Y_{2}}{\partial x_{2}} \\
\mathbf{\vdots} \\
\frac{\partial Y_{m}}{\partial x_{1}}\mathbf{\ \ }\frac{\partial Y_{m}}{\partial x_{2}}\mathbf{\ \ \ \ldots\ }\frac{\partial Y_{m}}{\partial x_{n}}
\end{array} \right\rbrack` , (7)

allows writing Eq. (6) as follows:

:math:`\mathbf{U}_{\mathbf{y}} = \mathbf{Q\ }\mathbf{U}_{\mathbf{x}}\mathbf{\ }\mathbf{Q}^{\mathbf{T}}`
. (8)

This equation represents the way of doing uncertainty propagation, which
is used especially when partial derivatives can be calculated
numerically. Note that this equation is one which can, e.g., well be
used within Excel.

Now consider the simple case, that *m*\ =1 and *n*\ =2 holds. This may
occur when doing linear regression with 1 equation and 2 unknowns, where
:math:`\mathbf{U}_{\mathbf{x}}` is the covariance matrix of the two
fitted parameters :math:`x_{1}` and :math:`x_{2}`. The associated
equation is :math:`Y_{1} = x_{1} + x_{2}z` . Then, Eq. (8) becomes (note
that an uncertainty of :math:`z` would require an additional propagation
term):

:math:`\mathbf{Q\ }\mathbf{U}_{\mathbf{x}}\mathbf{\ }\mathbf{Q}^{\mathbf{T}} = \left( \frac{\partial Y_{1}}{\partial x_{1}}\mathbf{,\ \ }\frac{\partial Y_{1}}{\partial x_{2}} \right)\begin{pmatrix}
U_{x,1,1} & U_{x,1,2} \\
U_{x,2,1} & U_{x,2,2}
\end{pmatrix}\left( \begin{array}{r}
\frac{\partial Y_{1}}{\partial x_{1}} \\
\mathbf{\ }\frac{\partial Y_{1}}{\partial x_{2}}
\end{array} \right)\ `

:math:`= \left( \frac{\partial Y_{1}}{\partial x_{1}}\mathbf{,\ \ }\frac{\partial Y_{1}}{\partial x_{2}} \right)\left( \begin{array}{r}
U_{x,1,1}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}U_{x,1,2}\frac{\partial Y_{1}}{\partial x_{2}} \\
\mathbf{\ }U_{x,2,1}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}U_{x,2,2}\frac{\partial Y_{1}}{\partial x_{2}}
\end{array} \right)`

:math:`= U_{x,1,1}\frac{\partial Y_{1}}{\partial x_{1}}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}U_{x,1,2}\frac{\partial Y_{1}}{\partial x_{2}}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}U_{x,2,1}\frac{\partial Y_{1}}{\partial x_{1}}\frac{\partial Y_{1}}{\partial x_{2}}\mathbf{+}U_{x,2,2}\frac{\partial Y_{1}}{\partial x_{2}}\frac{\partial Y_{1}}{\partial x_{2}}`

:math:`= u^{2}\left( x_{1} \right)\left( \frac{\partial Y_{1}}{\partial x_{1}} \right)^{2}\mathbf{+}u\left( x_{1},x_{2} \right)\frac{\partial Y_{1}}{\partial x_{2}}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}u\left( x_{2},x_{1} \right)\frac{\partial Y_{1}}{\partial x_{1}}\frac{\partial Y_{1}}{\partial x_{2}}\mathbf{+}u^{2}\left( x_{2} \right)\left( \frac{\partial Y_{1}}{\partial x_{2}} \right)^{2}`

From this one recognizes the equation (4) being separated into single
terms.

6.1.3 Applications in UR
~~~~~~~~~~~~~~~~~~~~~~~~~

The case of linear unfolding is represented by several equations of the
type

:math:`x_{i} = \sum_{k = 1}^{n}{A_{k,i}y_{k}}` ,

or, in matrix notation (**A** contains the
:math:`X_{i}\left( t_{k} \right)` terms):

:math:`\mathbf{x} = \mathbf{A\ y}` (9)

Now, the desired output quantities **Y** are found in the right side of
this equation, the input quantities on the left side. As it is well
known, the linear least squares-method yields the solution to this
problem:

:math:`\mathbf{y =}\mathbf{U}_{\mathbf{y}}\mathbf{A}^{\mathbf{T}}\mathbf{U}_{\mathbf{x}}^{- 1}\left( \mathbf{x} \right)\mathbf{\ x}`
;
:math:`\mathbf{U}_{\mathbf{y}}\mathbf{=}\left( \mathbf{A}^{\mathbf{T}}\mathbf{U}_{\mathbf{x}}^{\mathbf{- 1}}\left( \mathbf{x} \right)\mathbf{\ A} \right)^{- 1}`
(10a,b)

In a next step it is assumed that the elements of the design matrix
**A**, which e.g. consist of decay corrections when analyzing a decay
curve, may contain further parameters with associated uncertainties
(given as a vector **p** with covariance matrix
:math:`\mathbf{U}_{\mathbf{p}}`). Using a transformation matrix **Q**
containing numerically calculated partial derivatives
:math:`Q_{i,k} = \partial Y_{i}/\partial p_{k}`, an extended version of
the covariance matrix :math:`\mathbf{U}_{\mathbf{y}}` can be calculated
(this does not change the value of **y**)\ **:**

:math:`\mathbf{U}_{\mathbf{y}}\mathbf{=}\left( \mathbf{A}^{\mathbf{T}}\mathbf{U}_{\mathbf{x}}^{\mathbf{- 1}}\left( \mathbf{x} \right)\mathbf{\ A} \right)^{- 1}\mathbf{+}\mathbf{Q\ }\mathbf{U}_{\mathbf{p}}\mathbf{\ }\mathbf{Q}^{\mathbf{T}}`
(11)

For every combination
:math:`y_{i}\left( p_{k} \pm \mathrm{\Delta}p_{k} \right)` (with
:math:`\mathrm{\Delta}p_{k} = 1 \bullet \ 10^{- 6}p_{k}`) of numerical
approximations of the partial derivatives

:math:`Q_{ik} = \frac{\partial y_{i}}{\partial p_{k}} \approx \frac{y_{i}\left( p_{k} + \mathrm{\Delta}p_{k} \right) - y_{i}\left( p_{k} \right)}{\mathrm{\Delta}p_{k}}`
, (12)

the matrix **A**, the output vector **y** (its elements being considered
as functions) and the covariance matrix :math:`\mathbf{U}_{\mathbf{p}}`
must be recalculated.

Alternatively, by using the chain rule for derivatives the second term
in Eq. (11) can be expressed as follows:

:math:`\mathbf{C}_{\mathbf{P}}\mathbf{\ }\mathbf{U}_{\mathbf{p}}\mathbf{\ }\mathbf{C}_{\mathbf{p}}^{\mathbf{T}}\mathbf{=}\left( \mathbf{C}_{\mathbf{A}}\mathbf{D}_{\mathbf{P}} \right)\mathbf{U}_{\mathbf{p}}\left( \mathbf{C}_{\mathbf{A}}\mathbf{D}_{\mathbf{P}} \right)^{\mathbf{T}}\mathbf{\  =}\mathbf{C}_{\mathbf{A}}\left( \mathbf{D}_{\mathbf{P}}\mathbf{\ }\mathbf{U}_{\mathbf{p}}\mathbf{\ }\mathbf{D}_{\mathbf{p}}^{\mathbf{T}} \right)\mathbf{\ }\mathbf{C}_{\mathbf{A}}^{\mathbf{T}}\mathbf{=}\mathbf{C}_{\mathbf{A}}\mathbf{\ }\mathbf{U}_{\mathbf{A}}\mathbf{\ }\mathbf{C}_{\mathbf{A}}^{\mathbf{T}}`

Herein are: :math:`\mathbf{C}_{\mathbf{A}}` : matrix of partial
derivatives of *y*\ :sub:`i` with respect to the elements of the matrix
**A**, **D\ p** : matrix of partial derivatives of the elements of **A**
with respect to parameters **p**; **U\ A** : covariance matrix of the
elements of **A**, which are functions of **p**. The chain rule for
partial derivatives takes the form **C\ p**\ = **C\ A\ ·D\ p**, with the
property
:math:`\mathbf{C}_{\mathbf{p}}^{\mathbf{T}}\mathbf{=}{\mathbf{D}_{\mathbf{p}}^{\mathbf{T}}\mathbf{C}}_{\mathbf{A}}^{\mathbf{T}}`
**.**

Especially the expression
:math:`\mathbf{C}_{\mathbf{A}}\mathbf{\ }\mathbf{U}_{\mathbf{A}}\mathbf{\ }\mathbf{C}_{\mathbf{A}}^{\mathbf{T}}`
is required within the WTLS fitting procedure; it is derived in a
subroutine which is used by both fitting methods, WLS and WTLS,
respectively. The matrix **U\ A** is calculated as indicated in the
preceding sub-chapter.

   **Note on the covariance matrix U\ A:**

   For linear unfolding with the WTLS procedure a test had been
   implemented with the previous version, which by using the Cholesky
   decomposition tests whether the input covariance matrix is positive
   definite. This has been improved by testing in advance of the
   Cholesky decomposition the Cauchy-Schwarz inequality:

   :math:`cov(i,k)^{2} \leq var(i)var(k)` .

   For pairs (i, k), for which equality is found, the associated cov(i,
   k) is multiplied by the factor (1-δ) with δ=1·10\ :sup:`-09`. Should
   the covariance matrix after these tests again be not positive
   definite (it cannot be inverted) all non-diagonal elements are
   multiplied with this factor. Should this not help, the program
   evaluation is interrupted with giving a hint on this problem.

This primary result (i.e., the values of the fit parameters; they often
represent activity values referred to the time of measurement) of linear
may need further treatment, if the fit parameters values obtained must
be inserted into to further equations. This is the case, if the activity
value at measurement (fit parameter) needs to be divided by mass/volume
and requires and a further decay correction and these new parameters
(parameter vector **q**) associated with these corrections have
uncertainties (**embedding linear unfolding**). This often may not
require matrix algebra, if the corrections of the fit parameters
:math:`y_{i}` consist in simply multiplying them with factors
:math:`\varphi_{i}\ `\ which results in the “final” output quantities
:math:`y_{i}^{'}`:

:math:`y_{i}^{'} = y_{i}\ \ \varphi_{i}\ \left( q,y_{k} \right)` (13)

Nevertheless, these functions may be arranged into a vector

:math:`\mathbf{y}^{\mathbf{'}} = \left( y_{1}\left( \mathbf{q},\mathbf{y} \right),\ {\ y}_{2}\left( \mathbf{q},\mathbf{y} \right),\ \ldots,\ \ y_{n}\left( \mathbf{q},\mathbf{y} \right), \right)^{T}`
. (14)

Calculating the covariance matrix of :math:`\mathbf{y}^{\mathbf{'}}` now
requires, by analogy to Eq. (11), an (*n* x *n*)-matrix
:math:`\mathbf{J}` of partial derivatives
:math:`J_{i,k} = \partial y_{i}^{'}/\partial y_{k}` of the functions
:math:`y_{i}^{'}` with respect to the fitted values :math:`y_{i}`. For
taking into account the uncertainties of the *n*\ :sub:`q` values **q**
requires to calculate the correspondent (*n* x *n*\ :sub:`q`)-matrix
:math:`\mathbf{Q'}` of partial derivatives
:math:`{Q'}_{i,k} = \partial y_{i}^{'}/\partial q_{k}`.

If there are no covariances between **y** and **q**, the extended
version of Eq. (11) yields the covariance associated with the finally
desired output quantities :math:`\mathbf{y}^{\mathbf{'}}`:

:math:`\mathbf{U}_{\mathbf{y'}}\mathbf{=}\mathbf{J\ }\mathbf{U}_{\mathbf{y}}\mathbf{\ }\mathbf{J}^{\mathbf{T}}\mathbf{+}\mathbf{Q'\ }\mathbf{U}_{\mathbf{q}}\mathbf{\ }\mathbf{Q'}^{\mathbf{T}}`
, (15)

where the covariance matrix **U\ y** is the one from (11), which then
results in:

:math:`\mathbf{U}_{\mathbf{y'}}\mathbf{=}\mathbf{J\ }\left\{ \left( \mathbf{A}^{\mathbf{T}}\mathbf{U}_{\mathbf{x}}^{\mathbf{- 1}}\left( \mathbf{x} \right)\mathbf{\ A} \right)^{- 1}\mathbf{+}\mathbf{Q\ }\mathbf{U}_{\mathbf{p}}\mathbf{\ }\mathbf{Q}^{\mathbf{T}} \right\}\mathbf{\ }\mathbf{J}^{\mathbf{T}}\mathbf{+}\mathbf{Q'\ }\mathbf{U}_{\mathbf{q}}\mathbf{\ }\mathbf{Q'}^{\mathbf{T}}`
(16)

Note: If the partial derivatives :math:`\partial y_{i}/\partial p_{k}`
being the elements of the matrix **Q** (Eq. 11) are referred to the
vector :math:`\mathbf{y}^{\mathbf{'}}`, i.e. replaced by
:math:`\partial{y'}_{i}/\partial p_{k}`, the term
:math:`\mathbf{Q\ }\mathbf{U}_{\mathbf{p}}\mathbf{\ }\mathbf{Q}^{\mathbf{T}}`
in Eq. (16) associated with them, hast to be removed from Eq. (16) and
moved to Eq. (15) as an additional term.

For further reading, see: `Cox et al., 2004 <#literature>`__\ *.*

Best estimates according to Bayes and confidence limits
-------------------------------------------------------

The value **y** and its combined standard uncertainty **u(y)** obtained
from the evaluation of the output quantity define the mean and the
standard deviation of a Gaussian distribution of possible values
:math:`y^{'},`

:math:`\frac{1}{u(y)\sqrt{2\pi}}\exp\left( - \frac{\left( y^{'} - y \right)^{2}}{2\ u^{2}(y)} \right)`

which is obtained from the application of the maximum information
entropy principle.

Dependent on the size of the ratio **u(y)/y** it may happen that a part
of this distribution resides in the negative domain (:math:`y^{'}`\ *<
0*). According to the Bayesian method it follows by using a positive
“model prior“, that this distribution function shall only have positive
values for positive :math:`y^{'}` values (:math:`y^{'}`: e.g. activity
values). Therefore, the part of the distribution in the negative domain
is cut at :math:`y^{'}`\ *=* 0. Using this modified (cut) distribution
**expectation values of mean and standard deviation** can be determined
which are here called “\ **best estimates according to Bayes**\ ”. Let ω
be the value of the integral of the modified distribution function from
zero to infinity, with :math:`\omega < 1`. The resulting expectation
values then are:

mean:

:math:`\widehat{y} = y + \frac{u(y)\ exp\left( - \frac{y^{2}}{2\ u^{2}(y)} \right)}{\omega\sqrt{2\pi}}`
with :math:`\omega = \Phi\left( \frac{y}{u(y)} \right)`

(:math:`\Phi` is the (cumulative) distribution function of the
standardized normal distribution)

standard deviation:

:math:`u\left( \widehat{y} \right) = \sqrt{u^{2}(y) - \left( \widehat{y} - y \right) \bullet \widehat{y}}`

This guarantees that :math:`\widehat{y}` always will have a positive
value.

**The probabilistic symmetric coverage interval**

For the result value :math:`y` and the standard uncertainty
:math:`u(y)`, this interval is given by the two limits:

:math:`y^{\vartriangleleft} = y - k_{p}u(y)` with
:math:`p = \omega(1 - \frac{\gamma}{2})`

:math:`y^{\vartriangleright} = y + k_{p}u(y)` with
:math:`q = 1 - \frac{\omega\gamma}{2})`

A normal distribution is assumed therein which is cut at the left side
at the value of zero.

**The shortest coverage interval**

This interval, no longer be symmetric, is defined by the limits:

:math:`y^{<} = y + k_{p}u(y)` with
:math:`p = (1 + \omega(1 - \gamma))/2`

:math:`y^{>} = y - k_{p}u(y)`

These are modified In the case of :math:`y^{<} < 0`:

:math:`y^{<} = 0`

:math:`y^{>} = y + k_{q}u(y)` with :math:`q = 1 - \omega\gamma`

**Numerical estimation of these interval limits in the case of a
MC-Simulation**

From a MC-Simulation of a physical quantity, an array of about
:math:`N =`\ 10\ :sup:`4` through 10\ :sup:`6` simulated values
:math:`y_{i}` results which is sorted ascending. A simple relation
between a value :math:`y_{i}` and the associated probability value
:math:`p_{i}` holds:

:math:`p_{i} = \frac{i}{N}` with :math:`\sum_{i = 1}^{N}{p_{i} = 1}`

For a given probability :math:`p_{x}` its associated index :math:`k_{x}`
within the array is found by:

:math:`k_{x} = p_{x}N` .

The associated :math:`y` value of the interval limit, considered as a
quantile lies between the two adjacent values :math:`y_{int(k_{x})}` und
:math:`y_{int\left( k_{x} \right) + 1}` .

The shortest coverage interval (min_length) corresponding to the
probability :math:`(1 - \gamma)` is searched for within a simple loop
over the array y(i) as follows:

imax = :math:`\gamma`\ \*N

min_length = 1E+20

do i=1,imax

q_left = y(i)

q_right = y(int(N*(1 – i/N)))

if(q_right – q_left < min_length) then

min_length = q_right – q_left

q_left_min = q_left

q_right_min = q_right

endif

enddo

Linear Least squares method
---------------------------

**Principle of the multi-linear least squares fitting**

**Model**

For the use of the (multi-) linear least squares fitting (LSQ analysis)
the following model of a decay curve is assumed:

:math:`Y\left( t_{k} \right) = a_{1} \bullet X_{1}\left( t_{k} \right) + \ a_{2} \bullet X_{2}\left( t_{k} \right) + \ a_{3} \bullet X_{3}\left( t_{k} \right)`
(1)

A sum of up to three terms being dependent on the counting time
:math:`t_{k}` can be fitted to measured values of the quantity *Y* being
also dependent on :math:`t_{k}`; *k* counts the measurements. The
:math:`a_{i}` are the coefficients which are to be determined by fitting
(fitting parameters). The quantities :math:`X_{i}\left( t_{k} \right)`
are functions (decay curves) which are considered to be known and which
depend only on :math:`t_{k}` and other parameters, e.g., half-lives,
which are not considered as fit parameters. They must not be dependent
on the fitting parameters :math:`a_{i}`. :math:`Y\left( t_{k} \right)`
is treated as dependent variable while the functions
:math:`X_{i}\left( t_{k} \right)` are treated as independent variables
in the LSQ analysis. For the moment it is assumed here that the
measurement of the values of\ :math:`Y\left( t_{k} \right)` is done with
a single-channel counter.

By the introduction of up to three output quantities to be treated
simultaneously, it is, for instance, possible that the simultaneous
measurement of several radionuclides is done by using a two- or
three-channel counter. Using a LSC counter with three counting channels
(energy regions) allows for instance to determine simultaneously the
activities of Sr-90, Sr-89 and Sr-85 (yield tracer).

If more than one output quantity is defined for a project, e.g., two or
three representing the activities of different radionuclides, the
program inserts three new symbols, Fitp1, Fitp2 and Fitp3, into the list
of symbols, which refer to the values of the :math:`a_{i}`. These names
are not allowed to be changed. From these, the user can derive with
further equations (in the text window under the TAB “Equations”) above
the *Linfit* call the decay corrected activity concentrations. Under the
TAB “Values, uncertainties” standard uncertainties obtained from the
linear curve fitting are assigned to these new symbols. Additionally,
their possible correlation pairs and associated correlation coefficients
are inserted into the corresponding table under the same TAB which
enables their whole covariance matrix of the three fitting parameters to
be used for the uncertainty propagation of the output quantities.

Note, that also in the case of only two output quantities all three
symbols, Fitp1, Fitp2 and Fitp3, are inserted into the symbol list.
Only, **if only one output quantity** is defined, **none** of these
parameters is inserted in the symbol list. In this case the value of the
fitting parameter is attributed directly the quantity *Rn* in the call
*Rn = Linfit(1,… )*; c.f. further down.

**Least squares routines used In UncertRadio**

Two different routines are used in the program for least squares
estimation. These are:

-  **the “simple“ least squares procedure (LSQ) which is usually used if
      the values** :math:`X_{i}\left( t_{k} \right)` **do not have
      uncertainties**; the measured values
      :math:`Y\left( t_{k} \right)`, however, have uncertainties,
      covariances between them are also taken into account. If the
      values :math:`X_{i}\left( t_{k} \right)` nevertheless have
      uncertainties, they are included by UncertRadio within the
      uncertainty propagation outside the LLSQ routine;

-  In autumn 2013, three options for selecting a fitting procedure were
      introduced, which differ in their associated Chi-square
      expressions:

-  **WLS**: Using the **Neyman Chi-square**; this procedure is identical
      to the previous procedure NLSQ; linear, without iterations;

..

   **PLSQ**: Using the **Pearson Chi-square**; linear / iteratively;

   **PMLE**: Poisson Maximum Likelihood Estimation (Poisson MLE);
   non-linear / iteratively.

-  **the “general case“ of least squares (WTLS, weighted total least
      squares)**, **which in addition considers uncertainties of the**
      :math:`X_{i}\left( t_{k} \right)` **and possible covariances
      between them.** Herein, an iterative, i.e., non-linear, matrix
      procedure is used, which is a more time-consuming method because
      of the iterations. The possible covariances between the
      :math:`X_{i}\left( t_{k} \right)` values are determined by the
      program internally by applying partial derivatives with respect to
      the symbols contained in the :math:`X_{i}\left( t_{k} \right)`
      functions; they need not to be supplied by the user.

For the background information of these fitting methods see `chapter
7.4.3 <#chi-square-options>`__.

By default, the “simpler” WLS procedure is invoked by the call *Rn =
Linfit(1,… )*. The use of the WTLS procedure may be selected within the
dialog Definition of the decay-curve model.

**Notes:**

   **Correlations between measured values**
   :math:`Y\left( t_{k} \right)` **may occur.** This for instance is the
   case if the measured values :math:`Y\left( t_{k} \right)` are net
   counting rates for which the same value of a background counting rate
   :math:`R_{0}` as well as the net blank counting rate :math:`R_{bl}`
   have been subtracted from the corresponding gross counting rates.
   These covariances - they can be calculated internally from a formula
   - need to be considered especially in the case of quite low net
   counting rates.

   In the case of the net counting rates (:math:`Y\left( t_{k} \right)`)
   the covariance formula between two of its values is given by:

   :math:`cov\left( Y\left( t_{i} \right),\ \ \ Y\left( t_{k} \right) \right) = var\left( R_{0,i} \right) + var\left( R_{bl} \right)`
   .

   This is internally used in the program to derive the complete
   covariance matrix to be used by the fitting routine. In this
   equation, :math:`var\left( R_{0,i} \right)` must be set equal to zero
   if either :math:`R_{0,i}` and :math:`R_{0,k}` are different or their
   uncertainties.

   It has to be mentioned here that for this method the background
   counting rate can be defined in the special `input dialog for decay
   curves <#dialog-values-of-decay-curve>`__. For each individual gross
   counting rate of the curve an individual background counting rate may
   be given – this may occur with LSC measurements –, or one single
   value of the background counting rate (:math:`R_{0}`) may be used for
   the whole curve. :math:`R_{0}` must contain only the detector-related
   background component.

   Furthermore, a counting rate :math:`R_{bl}` is required which refers
   to a chemical blank analysis where the detector background component
   is already subtracted. Therefore, :math:`R_{bl}` is considered as a
   „net blank count rate“; it quantifies the background due to chemicals
   and glassware used during an analysis. The symbol *Rbl*, with value
   and uncertainty, is part of the table under the TAB “Values,
   uncertainties”, the values of which are transferred by the Linfit
   Call (c.f. further down in this theme) to this numeric routine.

For the mathematics see:

a) `linear curve fitting with
   WLS <#mathematics-of-the-linear-lsq-curve-fitting-with-correlated-measured-values>`__\ **,**

b) `linear curve fitting with
   WTLS <#notes-on-linear-curve-fitting-using-general-least-squares-wtls>`__\ **.**

Definition of the model: see `Dialog Definition of the decay-curve
model <#dialog-definition-of-the-decay-curve-model>`__

After having defined the Linfit-Call within the equations, the use of
the WTLS procedure may be selected in this dialog.

**Activating the (multi-) linear Least squares fitting**

This is shown for two examples of different complexity.

1) **Simple example:**

*Assumtion*:

Number of output quantities: 1; from the LSQ fitting a net counting rate
*Rn* is obtained.

Within the text field for equations at the location where otherwise the
net counting rate *Rn* is defined, for instance

*Rn = Rg – R0 ,*

this equation is replaced by the following:

*Rn = Linfit(1, Rbl, HwzY90, Hwzlong, HwzAc228, tmess, tstart)*

**Linfit** is the name of the procedure which initiates the LSQ fitting
with its associated sub-dialogs. Its parameters are:

   *1* No. of the variant of this measurement evaluation task for which
   this type of fitting shall be used; at present not more than the
   present variant of the evaluation of an Y-90 decay curve analysis
   exists;

   *R0* background counting rate including also blank contributions
   which is subtracted from the measured Y-90 gross counting rates, in
   :math:`s^{- 1}`;

   *t0* counting time of the background measurement, in :math:`s`;

   *HwzY90* half-live of Y-90, in :math:`s`

   *Hwzlong* half-live of a longer-lived radionuclide contributing to a
   (slowly decaying) background, in :math:`s`; e.g. Th-234; if Hwzlong =
   0 is set the associated decay factor is set internally equal to 1

   *HwzAc228* half-live of the possibly interfering radionuclide Ac-228,
   in :math:`s`\ *;* this cal also simulated a contribution of
   short-lived radon decay products

   *tmess* place holder for the counting times of the individual
   counting times belonging to the net counting rates

   *tstart* place holder for the periods of time between the time of the
   Y-90/Sr-90 separation and the starting time of the individual
   measurements

**Note:** Since version 2.4.24, only the three parameters *Rbl, tmess*
and *tstart* shall be given in the Linfit call: *Rn = Linfit(1, Rbl,
tmess, tstart)*

After loading the symbols from the equations including that Linfit-call
described above the symbols from this routine are available in the
common list of symbols. In the TAB “Values, uncertainties” values and
uncertainties of the symbols *R0, t0, HwzY90, Hwzlong* and *HwzAc228*
have to be entered then, however, not for *tmess* and *tstart*.

With one exception, of course, one may use other Linfit symbols instead
of those shown above, they only need to be given in the total symbol
list; these symbols are to be considered “globally” valid.

Important: Only the symbol names *Rbl*, *tmess* and *tstart* must not be
changed, which is also true for their meaning as defined above.

After the call to Linfit, the value of the fitting parameter
:math:`a_{1}` and its uncertainty have been transferred to those of the
symbol *Rn*.

2) **More complex example:**

*Assumtion*:

Number of output quantities: 3; as a result from the LSQ fitting one
obtains the fitting parameters Fitp1, Fitp2 and Fitp3, corresponding to
the parameters |image33|, which now represent the activities (in Bq) of
Sr‑89, Sr-90 and Sr-85. This is an example taken from the example
project DWD-LSC-3kanal-V2.txp.

Within the text field for equations at the location where otherwise the
net counting rate *Rn* is defined, for instance

*Rn = Rg – R0,*

this equation is replaced by the following:

*rd = Linfit(1, Rbl, eSr85A, eSr85B, eSr85C, eSr90A, eSr90B, eSr90C,
eSr89A, eSr89B, &*

*eSr89C, eY90A, eY90B, eY90C, lamSr85, lamSr90, lamSr89, lamY90, tmess,
tstart )*

**since version 2.4.24** *this is shortened to:* *Rn = Linfit(1, Rbl,
tmess, tstart).*

The meaning of the symbols is equivalent to those in the „simple
example” given above. The symbol names *Rbl*, *tmess* and *tstart* as
well as their associated meaning must not be changed. The symbols
*eNuklidX* (in total 9) designate detection efficiencies of the
different radionuclides for the counting channels A, B or C. The
ssymbols *lamNuklid* represent the decay constants of the three
radionuclides.

Note:

Apart from the fixed symbols *Rbl*, *tmess* and *tstart,* which have to
appear in the Linfit call, neither the names of other symbols are fixed
nor their number; however, they must appear in the whole (global) symbol
list. They must be used in the equations defining the functions
:math:`X_{i}\left( t_{k} \right)`; for more information, see also
`Dialog Definition of the decay-curve
model <#dialog-definition-of-the-decay-curve-model>`__.

For Input of data from the decay curve see: `Dialog “Values of decay
curve” <#dialog-values-of-decay-curve>`__

`Viewing the result of the decay curve LSQ
fitting <#viewing-the-result-from-the-lsq-fit-to-the-decay-curve>`__

`Note on the procedure for calculating Decision threshold and Detection
limit in the case of Least Squares
fitting <#note-on-decision-threshold-and-detection-limit-for-linear-fitting>`__

Utilizing a calibration curve
-----------------------------

There were demands existing to consider within an evaluation a specific
input quantity (e.g. the detection efficiency) as being dependent on
“another” quantity (such as area mass density, or a quench factor),
which means to take its value from an associated “calibration curve” by
interpolating a polynomial function of the “other” quantity.

For this purpose, a new function KALFIT was established, for which a new
dialog allows input of values x(i) and y(i) together with their standard
uncertainties. x and y (independent and dependent variables,
respectively) represent measured values of a calibration curve. The
curve is modeled by a polynomial with maximum degree of 3 (max 4
coefficients). The polynomial coefficients are calculated by a weighted
or non-weighted (multi-) linear least squares fit.

:math:`Y(x) = a_{1} + \ a_{2} \bullet x + \ a_{3} \bullet x^{2} + a_{4} \bullet x^{3}`
(1)

Unused (empty) columns in this dialog’s grid are set internally equal to
1.

Choosing a polynomial degree of 0 (i.e. 1 coefficient to be fitted),
fitting of the *y*-values results in a **weighted mean (uncertainties
given)** or in a **non-weighted mean (uncertainties not given).** Its
standard uncertainty is that of the mean, i.e. already divided by the
square root of the number of values.

**Activating the calibration curve tool:**

eps = KALFIT(1, eskv) (2)

In this example KALFIT is called to determine the detection efficiency
value **eps** (and its uncertainty) **as a function of** a quench factor
**eskv** by using the fitted polynomial for interpolation.

The second parameter (i.er., eskv) represents a value of the
**independent quantity X**, by which value and standard uncertainty of
the dependent quantity Y (in this case of **eps**) are determined.

The first parameter of this function (here: 1) gives the information
about how to use the calibration curve for calculating the value of the
left side of Eq. (2). The value 1 means that the value for Y is
calculated (read) as polynomial just as shown above. The value 2 means
that the value for Y is determined by reversing the polynomial. An
example for the latter case is treated in the UR project
Example_8_with_KALFIT_EN.txp), for which the Eq. (2) above is replaced
by:

Cx = KALFIT(2, Rnet)

It means that Rnet designates count rates, which in a first step are
calibrated as polynomial function dependent on known concentration
values x: Rnet = Polynomial(x). The second step is to measure a count
rate Rnetx of another sample and to determine then its associated
concentration Cx. Assuming a degree 1 of the polynomial one would
calculate the concentration Cx by (Rnetx-a\ :sub:`1`)/a\ :sub:`2`; for a
higher-degree polynomial a numerical bisection method is used for the
inversion. In the mentioned UR project, the concentration refers to that
of Potassium given in g/L.

This KALFIT call invokes a new dialog (there is also a new item under
the main menu Edit), by which the x- and y-values of such a calibration
curve (including their standard uncertainties) can be input; the desired
value for eps is taken via its x-value eskv from the polynomial curve
for the y-values. Another button allows executing the polynomial fitting
(max. degree of polynomial = 3) such that the final value and the
standard uncertainty are made available to UR for the quantity eps.

The standard uncertainty of the desired Y-value (eps) is calculated by
numerical uncertainty propagation using the fitted parameters
:math:`a_{i}` and their covariance matrix and the uncertainty of the
given x-value :math:`x_{0}` (eskv).

:math:`u^{2}(Y) = {\left( \frac{\partial Y}{\partial x_{0}} \right)^{2}u}^{2}\left( x_{0} \right) + \left\lbrack \sum_{1}^{m}\left( \frac{\partial Y}{\partial a_{i}} \right)^{2}u^{2}\left( a_{i} \right) + 2\sum_{i = 1}^{m - 1}{\sum_{j = i + 1}^{m}{\frac{\partial Y}{\partial a_{i}}\frac{\partial Y}{\partial a_{j}}}u\left( a_{i},a_{j} \right)} \right\rbrack`

|image34|

Although eps – as defined by the equation above – formally is a
dependent quantity, it is treated in UR as if it were an independent
quantity, e.g. regarding the uncertainty budget.

Currently, UR assumes that the value of only one input quantity is
determined by a call to KALFIT.

The uncertainties of the x-values are currently not considered.

The following dialog shows an example of calculating a mean value
(degree of polynomial equal to zero); the values to averaged are always
the ones in the column for y(i).

|image35|

Activity determination from several gamma lines
-----------------------------------------------

In this example of use from the **gamma-ray spectrometry, preferentially
with high-resolution Germanium detectors**, it is assumed that some
different radionuclides may occur in the measured source, but the gamma
lines (more than one) of that radionuclide for which the activity shall
be determined must not interfere with other lines in the spectrum. The
case that the activity shall be determined from only a single line, is
not considered here, because that can be done with the standard
procedure of UncertRadio.

Let the net counting rates :math:`R_{ni}` of n gamma lines of the
radionuclide be given. From these the activity values :math:`A_{i}`, in
Bq, are calculated with the following equation:

:math:`A_{i} = R_{ni}\frac{f_{att,i\ } \bullet \ f_{coinsu,i}}{\epsilon_{i}{\  \bullet \ p}_{\gamma i}\ }`
(1)

The symbols herein mean:

+-------+------------------------------------------+---+---------------+
| **    | **Meaning:**                             |   | **In Windows  |
| Symbo |                                          |   | Dialogs**     |
| ls:** |                                          |   |               |
|       |                                          |   | **written     |
|       |                                          |   | as:**         |
+-------+------------------------------------------+---+---------------+
| .     | net counting rate of the gamma line at   |   | RnetRate or   |
| . mat | energy :math:`E_{i}`, in :math:`s^{- 1}` |   | PeakNetRate   |
| h:: R |                                          |   |               |
| _{ni} |                                          |   |               |
+-------+------------------------------------------+---+---------------+
| ..    | full-energy peak efficiency at energy    |   | effi, eps or  |
|  math | :math:`E_{i}`                            |   | epsPeak       |
| :: \e |                                          |   |               |
| psilo |                                          |   |               |
| n_{i} |                                          |   |               |
+-------+------------------------------------------+---+---------------+
| .     | (absolute) emission probability of the   |   | pgamm         |
| . mat | line i                                   |   |               |
| h:: { |                                          |   |               |
| \ p}_ |                                          |   |               |
| {\gam |                                          |   |               |
| ma i} |                                          |   |               |
+-------+------------------------------------------+---+---------------+
| .     | self-attenuation correction at energy    |   | fatt          |
| . mat | :math:`E_{i}`                            |   |               |
| h:: f |                                          |   |               |
| _{att |                                          |   |               |
| ,i\ } |                                          |   |               |
+-------+------------------------------------------+---+---------------+
| ..    | correction for coincidence summing of a  |   | fcoinsu       |
|  math | line at energy :math:`E_{i}`             |   |               |
| :: f_ |                                          |   |               |
| {coin |                                          |   |               |
| su,i} |                                          |   |               |
+-------+------------------------------------------+---+---------------+

The standard uncertainties :math:`u\left( A_{i} \right)` of the
activities of single lines calculated according to Eq. (1) are
calculated internally in UncertRadio by using the uncertainty
propagation for Eq. (1).

Note: C\ ovariances between the calculated activities :math:`A_{i}` are
considered. Such covariances e.g. may be inferred by reading efficiency
values from the same FEP efficiency curve :math:`\epsilon(E)`, because
their values for different energies are derived from the same identical
set of parameters obtained from fitting the curve. Considering this
would require the inclusion of the full covariance matrix of these
parameters, which however cannot be handled by the program. Instead,
single values of the covariance/correlation can be input under the TAB
“Values, Uncertainties”.

**Description of procedures for calculating a mean**

   **a)** `Weighted
   mean <#calculation-of-the-weighted-mean-and-its-standard-uncertainty>`__

   **b)** `weighted least-squares
   mean <#least-squares-calculation-of-a-weighted-mean-and-its-standard-uncertainty>`__

`Procedure for calculating Decision threshold and Detection limit for
Gamspk1 <#approach-of-calculating-decision-threshold-and-detection-limit-for-gamspk1>`__

**Invoking the evaluation of several gamma lines**

In the text field for equations the following call is used in that place
where one otherwise would define the activity *A* of the counting
source:

*A = Gamspk1(E, tlive)*

**Gamspk1** is the name of the procedure being activated with its
sub-dialogs which is doing the calculations of the chosen mean. Its
parameters are:

   *E* placeholder for the energies of individual gamma lines, in keV;
   the program automatically attributes values to :math:`E`

   *tlive* counting time (duration) (live-time) , in :math:`s`;

After loading the symbols from the equations they are available to the
**Gamspk1** function. Within the TAB “Values, uncertainties” a value and
its uncertainty must be given for the symbol *tlive*, but not for *E*.
Of course, instead of *E* and *tlive* other symbol names can be used,
they only must be defined in the symbol list of the project. These
symbols are defined always as “global” variables.

Note: For the source activity *A* as defined here the decay correction
and e.g. a mass or a volume have to be defined in the equations outside
of Gamspk1.

**Input of values:**

for further information about input: `Dialog Values from spectrum
evaluation <#dialog-values-from-spectrum-evaluation>`__

`Viewing the result of mean calculation with
Gamspk1 <#view-of-the-result-from-calculating-a-mean-with-gamspk1>`__

Monte Carlo Simulation
----------------------

Under the TAB “Results” one can start a **Monte Carlo simulation** for
cross-checking the value and the uncertainty of the output quantity
**y**.

The simulation is done for a chosen (large) number of simulations of the
measurement. For this purpose, for each of those quantities having been
defined in the symbol list under the TAB “Equations” and characterized
there as independent (u) input quantities, simulated input values are
taken from their correspondent distributions (normal, rectangular or
triangular distributions). The underlying individual distributions have
been determined under the TAB “Values, uncertainties”. If the `(N+x)
rule <#low-level-applications-nx-rule>`__ has been selected there for
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
the recent publication by Kessel et al. (2006) (see also: `Meaning of
the TAB “Uncertainty budget” <#tab-uncertainty-budget>`__).

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

:math:`y = \iiint_{n}^{}{F\left( x_{1},x_{2},\ldots,x_{n} \right) \bullet \left\lbrack \varphi_{1}\left( x_{1} \right) \bullet \varphi_{2}\left( x_{2} \right)\ldots\varphi_{n}\left( x_{n} \right)\  \right\rbrack \bullet dx_{1} \bullet}dx_{2}\ldots\ dx_{n}`.

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

Low-Level Applications, (N+x) Rule
----------------------------------

Very low numbers of counts N are Poisson-distributed instead of
Gaussian-distributed. Using Bayesian methods it can be shown that
counting rates derived from them then are Gamma-distributed. Taking a
prior assumed as uniform (Weise et al., 2009) results in an expected
value of the counting rate :math:`R\ `\ which is characterized by
:math:`\widehat{R} = (N + x)/t_{m}\ `\ with a value of
:math:`u^{2}\left( \widehat{R} \right) = (N + x)/(t_{m}^{2}\ ) = \widehat{R}/t_{m}\ `\ for
its variance. From this the recommendation follows, for the case of very
low counting numbers, to replace :math:`N` by\ :math:`\ N + x`, **being
designated as the** :math:`\left( \mathbf{N + x} \right)` **Rule here**
which is long known from the literature.

The symbol x in (N+x) may be considered as a variable designated as
GamDistAdd in UR. This results in a Gamma distribution of the variable
(count number) associated with this rule, which from a Bayesian view
results from a prior which is proportional to 1/ρ (GamDistAdd=0.0), to
ρ\ :sup:`-1/2` (GamDistAdd=0.5) or which is constant (GamDistAdd=1).

+--------------------------+---------------+---------------------------+
| *c*\ =GamDistAdd         | (1-c)         | mean                      |
+==========================+===============+===========================+
| 0                        | 1             | N+0                       |
+--------------------------+---------------+---------------------------+
| 1/2                      | 1/2           | N+1/2                     |
+--------------------------+---------------+---------------------------+
| 1                        | 0             | N+1                       |
+--------------------------+---------------+---------------------------+

It is assumed here that this rule only refers to the gross counting rate
*R*\ :sub:`g` and to the background counting rate *R*\ :sub:`0`, because
other types of counting rates usually are determined by other methods
being less direct than a measurement. Within the UncertRadio dialog, the
values of these two counting rates do not require modification, if the
:math:`(N + x)` Rule is applied directly to the gross and background
counting numbers, *N*\ :sub:`b` and *N*\ :sub:`0`, respectively. Within
the program code the following replacement rule is applied:

*Directly before* a program part which is doing calculations with the
counting numbers:

*N*\ :sub:`b` is replaced by the term (*N*\ :sub:`b`\ + GamDistAdd),

*N*\ :sub:`0` is replaced by the term (*N*\ :sub:`0`\ + GamDistAdd).

*After that* program part: these replacements are removed.

The default value of GamDistAdd is 1.

The application of the (*N*\ +x) rule to the two mentioned counting
numbers is made available within the TAB “Values, Uncertainties” by
selecting “(*N*\ +x) rule” as type of distribution.

**Note: Only for counting number variables the (N\ +x) rule may be
selected; it must not be selected for the associated counting rate
variables.** The latter are treated in this way internally and thereby
are also gamma-distributed. This means, that this rule is correctly
applicable only if a counting rate R is defined by an equation R=N/t
containing the associated number of counts N. For N only a value is
given by the user, while the field for the uncertainty must be left
empty; this uncertainty obtained by the gamma-distribution is then
calculated internally.

Two example projects (Gamma-Dist_EN.txp and Lira_gammdist_EN.txp)
demonstrate the application of the (*N*\ +x) rule for the case of very
low counting numbers.

   **Important change: In anticipation of the new version of ISO
   11929:2019 the application of the (N+x) rule has been modified as
   follows. Apart from a single exception, x=0 is used in (N+x). The
   exception is N=0: then x=1 is used. This requires that the variable
   GamDistAdd must be set to zero ( Options – Presettings). Under this
   prerequisite, x=GamDistAdd=0, UncertRadio internally adds 1 to N only
   if N=0 (this means “0+1”). This means that for N>0 a Gamma
   distribution is applied for the associated count rate R with a
   prior(R) ~ 1/R. In contrast, for N=0 a uniform prior(R) is assumed.**

   **If, however, x=GamDistAdd has been set a value > 0, which may be
   true for already existing UR projects, it will always be added to N,
   also for N>0.**

It is assumed that the (*N*\ +x) rule may be used only with measurement
procedures not requiring a linear least-squares method – and only in the
case of very low counting numbers. **For procedures using the least
squares method** applied to low counts the fitting result would be
questionable; in that case where the Poisson distribution has to be
used, the least squares method would be biased and e.g. the
Poisson-Maximum-Likelihood-Estimation would have to be applied.

With the following example, based on Gamma-Dist_EN.txp, it shall be
demonstrated that applying the MC simulation may result in asymmetric
distributions deviating substantially from the normal distribution.

The equations may be as follows:

   A = phi \* Rn

   Rn = Rg - R0

   Rg = ng / t

   R0 = n0 / t0

The (N+1) rule is selected for the quantities ng and n0.

The starting case may be given by: Phi = 1, urel(Phi) = 0,1, t =100 s
and t0 = 500 s as well as ng = 8 counts and n0 = 6 counts, i.e. Rg =
(8+1)/100=0,09 s\ :sup:`-1` and R0 = (6+1)/500=0,014 s\ :sup:`-1`. The
MC-Simulation leads to the following triple graph, showing a slight
asymmetry of the simulated (green) distributions:

+--------------------------------------------------+-------------------+
| |MCplotfile_a_EN.png|                            | Starting case     |
+==================================================+===================+
+--------------------------------------------------+-------------------+

For the next case, with t0 = 500 s, but n0 = 0 counts, especially the
distribution of the decision threshold (following triple graph) shows an
even more pronounced asymmetry. This asymmetry is the reason that the MC
value of the decision threshold is about the double of that of the
analytical procedure (blue curve).

+--------------------------------------------------+-------------------+
| |MCplotfile-b_EN.png|                            | n0 set to null    |
+==================================================+===================+
+--------------------------------------------------+-------------------+

As a third case, compared to the starting case, only the background
counting duration t0 is increased to a value of t0=100 x tm=10000 s (a
case seldom encountered in practice), which makes the background
counting rate R0 very small, especially for the decision threshold a
very asymmetric distribution is obtained from which only a very small
part is within the negative region. This shape arising from the Gamma
distribution is quite different from what one is usually dealing with,
e.g., shown under
`MC-Details <#obtaining-mc-distributions-and-statistics-derived-of-it-in-detail>`__.

+-------------------------------------------------+--------------------+
| |MCplotfile-c_EN.png|                           | t0 enlarged to     |
|                                                 | t0=100 t           |
+=================================================+====================+
+-------------------------------------------------+--------------------+

The reason is given by the fact that with such a small background
counting rate the distribution of the decision threshold is mainly
affected by the gross counting rate for which the Gamma distribution for
very low counting numbers is very asymmetric and always has positive
possible values.

Confidence ellipses
-------------------

Confidence ellipses are invoked from the main menu item “Options –
Calculate confidence ellipse”.

**Construction of the ellipse**

The construction of a confidence ellipse of a pair of output quantities
is outlined following the GUM Supplement 2 as follows.

At first the covariance matrix **U\ y** of two output quantities is
determined. For the latter, designated here as *y*\ :sub:`1` und
*y*\ :sub:`2`, their covariance matrix **U\ y** consists of the diagonal
elements :math:`u^{2}\left( y_{1} \right)` and
:math:`u^{2}\left( y_{2} \right)` and of the identical non-diagonal
elements :math:`\rho\ u\left( y_{1} \right)u\left( y_{2} \right)`, with
their correlation coefficient :math:`\rho`.

For the lower triangular matrix **L** of a Cholesky decomposition of
**U\ y**, indicated by **U\ y**\ =\ **L** **L**\ :sup:`T`, the
eigenvalues **d** are calculated by the Jacobi method.

The length values **a** of the semi-axes of the ellipse and the angle
*θ* between the axes of the ellipse and the axes of the coordinate
system are derived from the equations

:math:`a_{j} = \sqrt{d_{j\ }\chi_{(1 - \gamma);2}^{2}}` , *j* =1,2

:math:`\theta = {\frac{1}{2}\tan}^{- 1}\left( \frac{2\rho\ u\left( y_{1} \right)u\left( y_{2} \right)}{u^{2}\left( y_{1} \right) - u^{2}\left( y_{2} \right)} \right)`

where :math:`\chi_{(1 - \gamma);2}^{2} = 5.99146` is the (1-γ) quantile
of the Chi-square distribution with 2 degrees of freedom.

**Graphical realization**

The following figure shows such a confidence ellipse in the left graph.
This does not yet correspond with our knowledge of an ellipse, because
their principal axes are not vertical. This behavior originates in
different scaling units of the two coordinate axes; e.g. 5 scale units
show quite different lengths.

+-----------------------------------+-----------------------------------+
| |image36|                         | |image37|                         |
+===================================+===================================+
| both axes have different scales   | *re-scale*: both axes have the    |
|                                   | same scale (5 scale units have    |
|                                   | the same length on both axes)     |
+-----------------------------------+-----------------------------------+

This disadvantage can be removed by introducing the same scale for both
axes. This can be achieved by the following re-scaling (in the GUM
Supplement 2 it was prevented to use different scaling):

:math:`u\left( y_{1} \right)\  \leq u\left( y_{2} \right)` :
:math:`y_{1} \rightarrow y_{1}\frac{u\left( y_{2} \right)}{u\left( y_{1} \right)}`
,

or

:math:`u\left( y_{2} \right)\  \leq u\left( y_{1} \right)` :
:math:`y_{2} \rightarrow y_{2}\frac{u\left( y_{1} \right)}{u\left( y_{2} \right)}`
.

For a graphical presentation the points of the ellipse curve are at
first calculated by assuming that the origin of the ellipse is identical
with the origin of the coordinate system and that the angle between the
axes of the ellipse and of the coordinate system is zero. Applying then
a coordinate transformation, consisting of the two operations of a
translation and a rotation, are then moved to the final curve of the
ellipse which is plotted then.

The right-hand graph of the Figure shown above displays the re-scaled
ellipse; their semi-axes are now perpendicular. Within both graphs, the
intervals

:math:`y_{j} \pm u\left( y_{j} \right)\sqrt{\chi_{(1 - \gamma);2}^{2}}`

are indicated as dotted lines.

**Literature:**

JCGM 102:2011 (GUM Supplement 2, 2011)

Brandt, S., 1999, Kapitel 5.10 und A.12

Press et al., 1992, chapter 11.1

M. A. Thompson, 2015: *Gaussian Statistics Lecture*

W. E. Hoover, 1984

Using data sets for mean and variance
-------------------------------------

Mathematical background
~~~~~~~~~~~~~~~~~~~~~~~

Formulae for a mean value and its associated variance presented here are
derived by Bayes statistics. Their derivation was described by Weise et
al. (2013) in their section 5.8 and their appendix C. Two cases a) and
b) are considered (see also Table 2 in `chapter
6.12.1 <#definitions>`__):

**Unknown random influences:**

a) *Mean type 1*. For any input quantity :math:`x`\ *,* **which does not
   represent a number of counts**, the variance of *m* individual values
   is derived from the experimental variation:

:math:`u^{2}\left( \overline{x} \right) = \frac{1}{m}\frac{(m - 1)}{(m - 3)}s_{x}^{2}`
(1)

Hierein are:

:math:`\overline{x} = \sum_{i = 1}^{m}x_{i}`, and
:math:`s_{x}^{2} = \frac{1}{m - 1}\sum_{i = 1}^{m}{(x_{i} - \overline{x})^{2}}`
(2)

b) *Mean type 2*. An input quantity *n* represents **a number of
   counts** and is influenced by an additional variation, e.g., due to
   repeated sampling and/or chemical analysis, which enlarges the
   Poisson-derived variance. A normal distribution with parameters
   :math:`\mu` and :math:`\sigma^{2}` is assumed for this influence. The
   variance of the mean is then given by:

:math:`u^{2}\left( \overline{n} \right) = \frac{1}{m}\left( \overline{n} + \frac{(m - 1)}{(m - 3)}{(\overline{n} + s}_{n}^{2}) \right) = \frac{1}{m}(\overline{n} + E(S^{2},\mathbf{n}))`
(3)

   :math:`\overline{n}\ ` and :math:`s_{n}^{2}` are calculated analogue
   to :math:`\overline{x}\ ` und :math:`s_{x}^{2}`. The variance
   component

   :math:`E\left( S^{2},\mathbf{n} \right) = \frac{(m - 1)}{(m - 3)}{(\overline{n} + s}_{n}^{2})`
   (4)

   is considered as the best estimate of the parameter
   :math:`\sigma^{2}` of the involved normal distribution. The first
   term in the bracket of Eq. (2), :math:`\overline{n}` , represents the
   Poisson-related part of the variance.

Applying these formulae leads to surprising result that a variance can
be calculated only if there are more than three individual values.

*Mean type 3*. With version 2.3.01 the **classical** formula for the
standard uncertainty of the mean can be applied

:math:`u\left( \overline{x} \right) = \frac{s_{x}}{\sqrt{m}}` , (5)

if the type of mean “classical“ is selected.

**Known random influences:**

If the fraction of (4) within (3) is small, a parameter
:math:`\vartheta` can be defined as:

:math:`\vartheta^{2} = E\left( S^{2},\mathbf{n} \right)/{\overline{n}}^{2}`

by which Eq. Gl. (3) turns into:

:math:`u^{2}\left( \overline{n} \right) = \frac{1}{m}(\overline{n} + \vartheta^{2}{\overline{n}}^{2})`
(6)

By solving Eq. (4) for :math:`\vartheta^{2}`, an equation is obtained,
by which :math:`\vartheta^{2}` can be determined from the data set of
measurements of a reference sample :math:`r`:

:math:`\vartheta^{2} = \left( {m_{r}\ u}^{2}\left( {\overline{n}}_{r} \right) - {\overline{n}}_{r} \right)/{\overline{n}}_{r}^{2}`
(7)

The parameter value :math:`\vartheta` should be less than about 0.2.

Applying means in UncertRadio
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If in the symbol list under the TAB “Equations“ a symbol type is changed
into „m“, the program assumes that value and uncertainty of this
quantity are to be derived from a data set. The following input dialog
allows the input of the data set, it is invoked by the icon |image38|
from the toolbar (it requires first selecting the row of this “m”
variable in the TAB “Values, uncertainties”):

|image39|

The id values for the data sets are already known here. In the dialog
shown, the id ref_data (belonging to the input quantity ref) is selected
for data input. Besides, the type of mean and variance can be selected
from equations (1) and (3). For the extreme case that there are not more
than only 3 single values, or the data shall be evaluated in a classical
sense, the variance according to Eq. (5) can be chosen as third option.
The latter can also be used for more than 3 single values. In the dialog
shown, the standard deviations sx and s0x correspond to equations (1)
und (2) in 6.9.1.

The combobox indicated in the dialog by the label “sel. data record used
as reference“ allows to select one of the mean datasets, which is
intended to be used as a reference in the case of “\ *known* random
influences”. An example project is ISO-Example-2b_V2_EN.txp. If no
reference data set is selected, the evaluation follows that of the
option “\ *unknown* random influences“. The details for these options
are outlined in section 6.12.

Values of mean and uncertainty of such a data set are transferred by the
program to the uncertainty table under the TAB “Values, uncertainties“
by the button “Calculating uncertainties”.

The individual values of this quantity with a name symbol are saved in
the project file (\*.txp) as a single line record identified by the
associated identification (symbol_data).

For **organizing the data input** it is recommended to begin with data
input into the TAB „Values, Uncertainties“. For mean variables
characterized by „m“ as type the „t distribution“ is to be selected as
distribution type which enables a correct statistical treatment of the
mean within the mean dialog. Then, the mean dialog can be opened in
which the desired mean variable is selected; after input of associated
singe values the type of mean is selected which then can be calculated.
After leaving the dialog the calculation of uncertainties needs to be
updated/repeated.

The input of single values in this dialog was modified such, that after
input of a value the next cell is already opened for input. It happens
that the activated cell appears to be moved a bit away from the grid
cell, however, the value entered (finalized with Enter or cursor-down)
is transferred into the original grid cell. The input of values is then
finalized with typing Enter into the activated cell, which must be empty
for this purpose.

Measuring a short-lived radionuclide with comparably long counting duration
---------------------------------------------------------------------------

Basic principles
~~~~~~~~~~~~~~~~

If the product :math:`\lambda t_{m}` becomes significantly larger than
0.1, or even :math:`\geq 1`, when measuring the activity of a
short-lived radionuclide, the Poisson distribution of the gross counts
is only an approximation. A feature in this case is that the gross
counts distribution is a superposition of a binomial (sample
contribution) and a Poisson distribution (background). A characteristic
of binomially distributed sample contribution counts is that the
variance of the gross counts is smaller than the gross counts itself,
i.e., it is smaller than the variance of Poisson distributed gross
counts. The binomial distribution for detected sample counts in this
context has been applied in the literature (see e.g., Mathews et al,
1979; Spyrou et al., 1981; Salma and Zemplén-Papp, 1992; Gilmore, G.,
2008; Semkow, 2007).

Let :math:`N` be the number of atoms existing at the begin of the
measurement. The product of the probability
:math:`\left( 1 - e^{- \lambda \cdot t_{m}} \right)` for the decay of an
atom during the duration :math:`t_{m}` and the probability
:math:`\varepsilon` of detecting this decay, constitutes one parameter
:math:`p = \varepsilon \bullet \left( 1 - e^{- \lambda \cdot t_{m}} \right)`
of the binomial distribution; :math:`N` is the other. :math:`N` relates
to the activity :math:`A` by :math:`A = \lambda \bullet N`.

The literature mainly restricted the consideration to the binomial
distribution of the sample counts contribution. However, the
distribution of the gross counts (including background also) is also
required. It can be found by folding two discrete distributions, of the
binomial and the Poisson variables X und Y:

+--------+--------------------------------------------------------+---+
| X:     | .. math::                                              |   |
|        |                                                        |   |
|        |    P_{Bi}\left( x|N,p \right) = \left( \begin{aligned} |   |
|        |     & N \\                                             |   |
|        |     & x                                                |   |
|        |    \end{aligned} \right)p^{x}(1 - p)^{N - x}           |   |
+========+========================================================+===+
| Y:     | .. mat                                                 |   |
|        | h:: P_{Po}\left( y|R_{0},t_{m} \right) = \frac{\left(  |   |
|        | R_{0}t_{m} \right)^{y}e^{- R_{0}t_{m}}}{\Gamma(y + 1)} |   |
+--------+--------------------------------------------------------+---+
| Z = X  | .. math:: P_{BiPo}(                                    |   |
| + Y:   | X + Y = z) = \sum_{k = 0}^{z}{P_{Bi}\left( X = x \equi |   |
|        | v k|N,p \right)P_{Po}\left( Y = z - k|R,t_{m} \right)} |   |
+--------+--------------------------------------------------------+---+

With :math:`x \equiv k`, :math:`{y \equiv n}_{0m} = R_{0}t_{m}` and the
gross counts :math:`n_{g} = z = x + y = k + n_{0m}`, it follows:

+--------+-------------------------------------------------------+----+
|        | .. math:: P_{BiPo}\left( n_{g}|N,p,R_{0},t_{m},       | (  |
|        | t_{b} \right) = \sum_{k = 0}^{n_{g}}{P_{Bi}\left( k|N | 1) |
|        | ,p \right)P_{Po}\left( n_{g} - k|R_{0},t_{m} \right)} |    |
+========+=======================================================+====+
+--------+-------------------------------------------------------+----+

**Note:**

Strictly speaking, the form of the :math:`P_{BiPo}` distribution as just
defined is valid only for integer values of the binomial parameter
:math:`N`. For non-integer values the binomial distribution part is not
normalized to one. This problem can be avoided, if Eq. (1) is replaced
by a special numerical function, which is expressed by a hypergeometric
distribution using the so-called **Kummer confluent hypergeometric
function.** This form of distribution allows to apply also non-integer
values of :math:`N`. Its computation is also faster than applying Eq.
(1), especially for larger values of :math:`N`. This version of the
:math:`P_{BiPo}` distribution is actually applied in UncertRadio.

For a value of the gross count number to be generated by **Monte Carlo
simulation** one binomial distributed value (sample contribution) and
one Poisson-distributed value (background contribution) are generated.
Both values are added to obtain the gross count number value. However,
this follows only that procedure given by Eq. (1) (discrete :math:`k`
values), because the binomial-distribution random number generator can
only produce integer values, while the Poisson random numbers
(background contribution) are continuous. For a small number of gross
counts, the shape of the distribution is therefore a series of
overlapping peaks, one for each binomial integer value.

Aspects of uncertainties and evaluation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The expectation values and the variance of the number :math:`n_{g}` of
gross counts are calculated by moments of the :math:`P_{BiPo}`
probability density:

+---+--------------------------------------------------------------+----+
|   | .. math:: E\left(                                            | (  |
|   | n_{g} \right) = \sum_{j = 0}^{\infty}{(j)P_{BiPo}\left( j|N, | 2) |
|   | p,R_{0},t_{m},t_{b} \right)} = Np + R_{0}t_{m} = Np + n_{0m} |    |
+===+==============================================================+====+
+---+--------------------------------------------------------------+----+

+---+----------------------------------------------------------------+---+
|   | .. math:: Var\left( n_{g} \right) = \lef                       | ( |
|   | t\lbrack \sum_{j = 0}^{\infty}{\left( j^{2} \right)P_{BiPo}\le | 3 |
|   | ft( j|N,p,R_{0},t_{m},t_{b} \right)} \right\rbrack - E^{2}\lef | ) |
|   | t( n_{g} \right) = \left( Np + n_{0m} \right)(1 - p) + n_{0m}p |   |
+===+================================================================+===+
+---+----------------------------------------------------------------+---+

The corresponding values of the binomial distribution are

:math:`E(k) = Np = k` und (4)

:math:`{var}(k) = Np(1 - p) = k(1 - p)` (5)

which allow to replace in equations (2) and (3) the product :math:`Np`
by :math:`k`.

By inserting :math:`k = n_{g} - n_{0m}`, which can be interpreted as the
number of net counts, into Eq. (3), it follows:

:math:`Var\left( n_{g} \right) = n_{g}(1 - p) + R_{0}t_{m}p = n_{g}(1 - p) + n_{0m}p`
(6)

Two quantities being important for uncertainty propagation are the gross
and the net count rates. They must be based on directly measured
quantities:

**Gross count rate**:

:math:`R_{g} = \frac{n_{g}}{t_{m}}` (7)

:math:`u^{2}\left( R_{g} \right) = \frac{1}{t_{m}^{2}}\left( u^{2}(n_{g}) \right) = \frac{1}{t_{m}^{2}}\left( n_{g}(1 - p) + n_{0m}p \right) = \frac{R_{g}}{t_{m}}(1 - p) + \frac{R_{0}}{t_{m}}p`
(8)

**Net count rate:**

:math:`R_{n} = R_{g} - R_{0}`

:math:`u^{2}(R_{n}) = u^{2}(R_{g}) + u^{2}(R_{0}) = \frac{R_{g}}{t_{m}}(1 - p) + \frac{R_{0}}{t_{m}}p + \frac{R_{0}}{t_{0}} = \frac{R_{g}}{t_{m}}(1 - p) + R_{0}(\frac{p}{t_{m}} + \frac{1}{t_{0}})`

One finds for
:math:`binomial\overset{\rightarrow}{\ }Poisson \equiv \ p \rightarrow 0`
:
:math:`u^{2}\left( R_{g} \right) = \frac{R_{g}}{t_{m}};\ \ \ \ \ \ u^{2}(R_{n}) = \frac{R_{g}}{t_{m}} + \frac{R_{0}}{t_{0}}`

**Relation between activity and** :math:`\mathbf{N}`

For the activity :math:`A_{0} = \lambda N` existing at :math:`t = 0` a
number :math:`k` of counts are detected during the counting duration
:math:`t_{m}`, originating from the radionuclide decay. It is obtained
as:

:math:`k = \varepsilon\int_{0}^{t_{m}}{A_{0}e^{- \lambda t}}dt = \varepsilon\int_{0}^{t_{m}}{(\lambda N)e^{- \lambda t}}dt = N\varepsilon\left( 1 - e^{- \lambda t_{m}} \right) = Np`
(9)

It follows then from :math:`R_{s} = k/t_{m}` :

   :math:`N = \frac{R_{s}t_{m}}{p}` or
   :math:`\widetilde{N} = \frac{\left( \widetilde{a}/w \right)t_{m}}{p}`
   (10)

An equation for :math:`A_{0}` follows from the equation for the count
rate :math:`R_{s}` (:math:`f_{3}` is a correction for the decay during
the counting duration)

+---+--------------------------------------------+-----------------------+
|   | .. m                                       | (11)                  |
|   | ath:: \frac{k}{t_{m}} = \frac{\varepsilon  |                       |
|   | A_{0}}{t_{m}}\int_{0}^{t_{m}}e^{- \lambda  |                       |
|   | t}dt = \varepsilon A_{0}\frac{\left( 1 - e |                       |
|   | ^{- \lambda t_{m}} \right)}{\lambda t_{m}} |                       |
+===+============================================+=======================+
+---+--------------------------------------------+-----------------------+

+---+---------------------------------------------------+----------------+
|   | .. math:: A                                       | (12)           |
|   | _{0} = \frac{k}{t_{m}}\frac{\lambda t_{m}}{\varep |                |
|   | silon\left( 1 - e^{- \lambda t_{m}} \right)} = \f |                |
|   | rac{k}{t_{m}}\frac{1}{\varepsilon}\frac{1}{f_{3}} |                |
+===+===================================================+================+
+---+---------------------------------------------------+----------------+

**Activity concentration** :math:`\mathbf{a}`\ **:**

The relation :math:`A_{0} = \lambda N` is used for deriving the activity
concentration :math:`a`:

:math:`a = w_{0}A_{0} = w_{0}\lambda\frac{k}{p} = w_{0}\lambda\frac{n_{g} - n_{0m}}{p} = w_{0}\frac{\lambda t_{m}}{p}(\frac{n_{g}}{t_{m}} - \frac{n_{0m}}{t_{m}}) = w(R_{g} - R_{0})`
(13)

Based on this equation, the uncertainty is calculated as usual in
UncertRadio.

An example case
~~~~~~~~~~~~~~~

After Ac-228 (half-live (6,15 :math:`\pm` 0,03) h) is radiochemically
separated, it is measured during 8 h. For this measurement setup the
product :math:`\lambda_{Ac228}t_{m}` is 0.9017. As this is significantly
above 0.1, nearly 1, the Ac-228 contribution to the measured gross count
rate is considered to follow the binomial distribution. The number of
gross counts therefore follow the sum of binomial- and
Poisson-distributed contributions.

Symbols and values of input quantities:

*(taken from the UR2 project Ac228_binomial_EN.txp)*

+----+-----------------------------------------------------------------+
| .  | Number of atoms existing at the begin of measurement (          |
| .  | :math:`A = \lambda N`)                                          |
| ma |                                                                 |
| th |                                                                 |
| :: |                                                                 |
|  N |                                                                 |
+====+=================================================================+
| .  | :math:`= \varepsilon\left( 1 - e^{- \lambda t_{m}} \right)`:    |
| .  | parameter of the binomial distribution:                         |
| ma |                                                                 |
| th | :math:`Bin\left( k|N,p \right) = \left( \begin{aligned}         |
| :: |  & N \\                                                         |
|  p |  & k                                                            |
|    | \end{aligned} \right)p^{k}(1 - p)^{N - k}`;                     |
|    | (:math:`0 \leq k \leq N`) (1)                                   |
|    |                                                                 |
|    | :math:`p =`\ 0,23764104; :math:`u(p) =` 0,004982491             |
|    |                                                                 |
|    | Note: If the measurement of duration :math:`t_{m}`\ does not    |
|    | start at 0, but at :math:`t_{1}`, the parameter :math:`p` is    |
|    | extended to:                                                    |
|    | :math:`p = e^{- \                                               |
|    | lambda t_{1}}\varepsilon\left( 1 - e^{- \lambda t_{m}} \right)` |
+----+-----------------------------------------------------------------+
| .  | detection probability: 0,4 :math:`\pm` 0,0083;                  |
| .  |                                                                 |
| ma |                                                                 |
| th |                                                                 |
| :: |                                                                 |
|  \ |                                                                 |
| va |                                                                 |
| re |                                                                 |
| ps |                                                                 |
| il |                                                                 |
| on |                                                                 |
+----+-----------------------------------------------------------------+
| .  | decay constant of Ac-228, half-live t 6,15 h :math:`\pm` 0,03   |
| .  | h;                                                              |
| ma |                                                                 |
| th | :math:`\lambda =`\ 0,1127069 h\ :sup:`-1`; :math:`u(\lambda) =` |
| :: | 5,497896E-04 h\ :sup:`-1`                                       |
|  \ |                                                                 |
| la |                                                                 |
| mb |                                                                 |
| da |                                                                 |
+----+-----------------------------------------------------------------+
| .  | duration of the Ac-228 measurement (8 h), being not small       |
| .  | compared to the half-live                                       |
| ma |                                                                 |
| th |                                                                 |
| :: |                                                                 |
|  t |                                                                 |
| _{ |                                                                 |
| m} |                                                                 |
+----+-----------------------------------------------------------------+
| .  | background count rate, measured with the duration               |
| .  | :math:`t_{0}`\ = 20 h: 50 Imp./20 h = 2.50 h\ :sup:`-1`;        |
| ma |                                                                 |
| th |                                                                 |
| :: |                                                                 |
|  R |                                                                 |
| _{ |                                                                 |
| 0} |                                                                 |
+----+-----------------------------------------------------------------+
| .  | number of gross counts: 50 counts within 8 h;                   |
| .  |                                                                 |
| ma |                                                                 |
| th |                                                                 |
| :: |                                                                 |
|  n |                                                                 |
| _{ |                                                                 |
| g} |                                                                 |
+----+-----------------------------------------------------------------+
| .  | factor converting the activity (Bq) into an activity            |
| .  | concentration                                                   |
| ma |                                                                 |
| th |                                                                 |
| :: |                                                                 |
|  w |                                                                 |
| _{ |                                                                 |
| 0} |                                                                 |
+----+-----------------------------------------------------------------+

Results obtained by these data:

:math:`n_{0m} = R_{0}t_{m} = 20` counts

:math:`k = n_{g} - n_{0m} = 30` counts

:math:`N = k/p = 126.2408` counts

:math:`R_{s} = 30/8\ h^{- 1} = 3.75\ h^{- 1}`

:math:`u(R_{s}) = \sqrt{30 \bullet (1 - 0.23764104)}/8 = 0.5978\ h^{- 1}\ `

:math:`u(ng) = \sqrt{n_{g}(1 - p) + n_{0m}p} = \sqrt{50 \bullet (1 - 0.23764104) + 20 \bullet 0.23764104} = 6.5476`

:math:`A_{0} = 0.112707 \bullet 126.24 = 14.228` Bq

further results:

w = 3.79418826 u(w)= 7.90495202E-02 (w0=1)

a = 14.2282066 u(a)= 3.39566064

Sum(Product(Bi x Po)):

mean(BiPo)= 50.0000000 var(BiPo)= 42.8707695

expected Var: Ng*(1-p) + N0/t0*tm*p= 42.8707695

**Note**: The variance of the gross count number (42.871) is smaller
than the gross count number (50.0), i.e., smaller compared to a pure
Poisson distributed gross count number. By decreasing the detection
probability by a factor of 10 results in a 10-fold smaller value of
:math:`p`, i.e., :math:`p = 0.0237641`. Under this assumption the
binomial distribution can be approximated by a Poisson distribution.
Then, approximately the relation “variance of gross counts = gross
counts“ would be to be expected. This is confirmed by Eq. (6), by which
the variance value results in
:math:`50 \bullet 0.97624 + 20 \bullet 0.0237641 = 49.287\ `, which is
already close to the value of 50 to be expected for the “Poisson plus
Poisson“ case.

Implementation in UncertRadio
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first step for invoking the specaial procedure for short half-lives
is to select under the TAB “Values, Uncertainties“ the distribution type
“\ **Binom+Poiss**\ “ for the gross counts number symbol.

Thereafter, four further parameters are to be selected:
:math:`p,\ \ R_{0},\ t_{m}\ und\ \lambda`. This can be achieved by
invoking a dialog via the menu **Set binomial/poisson case:**

|image40|

This dialog may also be invoked by the program itself while establishing
such an UR project.

The symbol numbers of the four parameters *p*, *R0*, *tm*, *lambda*, are
stored in the txp file, e.g., as “BinPoi=8 10 12 9“.

Special distributions and their properties
------------------------------------------

The following probability distributions are implemented in UncertRadio:

+--------------------------------+------------+-----------------------+
| distribution                   | code       | notes                 |
+================================+============+=======================+
| Normal distribution            | Normal     |                       |
+--------------------------------+------------+-----------------------+
| Rectangular distribution       | Rectangle  |                       |
+--------------------------------+------------+-----------------------+
| Triangular distribution        | Triangle   |                       |
+--------------------------------+------------+-----------------------+
| (N+x) Rule                     | (N+x) Rule | see section 6.7       |
+--------------------------------+------------+-----------------------+
| Lognormal distribution         | LogNormal  |                       |
+--------------------------------+------------+-----------------------+
| Gamma distribution             | GammaDist  |                       |
+--------------------------------+------------+-----------------------+
| Binomial+Poisson distribution  | B          | see section 6.10.1    |
|                                | inom+Poiss |                       |
+--------------------------------+------------+-----------------------+
| Beta distribution, 2           | Beta2Dist  |                       |
| parameters                     |            |                       |
+--------------------------------+------------+-----------------------+
| T distribution                 | T-Distrib  |                       |
+--------------------------------+------------+-----------------------+
| Beta distribution, 4           | Beta4Dist  |                       |
| parameters                     |            |                       |
+--------------------------------+------------+-----------------------+
| Erlang distribution of the     | Npreset    | is a Gamma            |
| counting duration for preset   |            | distribution for      |
| count numbers                  |            | integer numbers of    |
|                                |            | counts                |
+--------------------------------+------------+-----------------------+

In addition to better known distributions, like e.g. normal, rectangular
or triangular distributions, special distributions can be applied,

-  the gamma distribution,

-  the beta distribution and

-  the *t*-distribution.

the properties of which are described in the following. Their
application requires two or three specific parameters.

In the following, the probability density functions (pdf) and the
relation of their parameters to measured data are shortly introduced.

The icon |image41| allow to invoke a dialog for showing the
distribution-related parameters of an input quantity. This requires that
the row within the table “Values, uncertainties” needs to be
highlighted.

The gamma distribution
~~~~~~~~~~~~~~~~~~~~~~

Probability density as a function of :math:`x`, with parameters
:math:`\alpha` and :math:`\beta` :

:math:`P_{\gamma}\left( x|\alpha,\beta \right) = \frac{\beta^{\alpha}}{\Gamma(\alpha)}x^{\alpha - 1}e^{- \beta x}`

:math:`\Gamma(x)` is the gamma function, with
:math:`\Gamma(x + 1) = x!.`

Mean :math:`E\lbrack x\rbrack` and variance :math:`Var\lbrack x\rbrack`
of the probability density are defined by:

:math:`E\lbrack x\rbrack = \frac{\alpha}{\beta}` ;
:math:`Var\lbrack x\rbrack = \frac{\alpha}{\beta^{2}}`

If mean and variance of measured values are attributed to them, the
equations then allow to derive the values of the two parameters:

:math:`\beta = \frac{E\lbrack x\rbrack}{Var\lbrack x\rbrack}`
:math:`\alpha = \frac{\left( E\lbrack x\rbrack \right)^{2}}{Var\lbrack x\rbrack}`

The gamma distribution can e.g. be used for counting rates or for a
detection probability with a larger relative uncertainty.

The beta distribution
~~~~~~~~~~~~~~~~~~~~~

Probability density as a function of :math:`x`, with parameters
:math:`\alpha` and :math:`\beta` :

:math:`P_{\beta}\left( x|\alpha,\beta \right) = \frac{1}{B(\alpha,\beta)}x^{\alpha - 1}(1 - x)^{\beta - 1}`

:math:`B(\alpha,\beta) = \Gamma(\alpha)\Gamma(\beta)/\Gamma(\alpha + \beta)`
is the beta function.

Mean :math:`E\lbrack x\rbrack` and variance :math:`Var\lbrack x\rbrack`
of the probability density are defined by:

:math:`E\lbrack x\rbrack = \frac{\alpha}{\alpha + \beta}` ;
:math:`Var\lbrack x\rbrack = \frac{\alpha\beta}{(\alpha + \beta)^{2}(\alpha + \beta + 1)}`

If mean and variance of measured values are attributed to them, the
equations then allow to derive the values of the two parameters:

:math:`\beta = \alpha\left( \frac{1}{E\lbrack x\rbrack} - 1 \right);`
:math:`\alpha = E\lbrack x\rbrack\left( \frac{E\lbrack x\rbrack\left( 1 - E\lbrack x\rbrack \right)}{Var\lbrack x\rbrack} - 1 \right)`

or (according to NIST):

:math:`\beta = \frac{\alpha}{\bar{x}}\left( 1 - \bar{x} \right);`
:math:`\alpha = \bar{x}\left( \frac{\bar{x}(1 - \bar{x})}{s^{2}} - 1 \right)`

In contrast to the gamma distribution, the beta distribution is defined
within the restricted range :math:`0 \leq x \leq 1`. It is thus well
suited for a detection probability which normally hast he same support.

The *t*-distribution
~~~~~~~~~~~~~~~~~~~~

For convenience, not the standard Student-*t*-distribution is applied,
but the “non-standard“ *t*-distribution, also called the
“Scaled-and-shifted” *t*-distribution. Its parameters are the number
:math:`\upsilon` of degrees of freedom and the two parameters
:math:`\widehat{\mu}` (“shift“) and :math:`{\widehat{\sigma}}^{2}`
(“scaling“).

The Probability density function:

.. math:: P_{t}(x|\upsilon,\widehat{\mu},{\widehat{\sigma}}^{2}) = \frac{\Gamma((\upsilon + 1)/2)}{\Gamma(\upsilon/2)\sqrt{\upsilon\pi}}\frac{1}{\widehat{\sigma}}\left( 1 + \frac{1}{\upsilon}\left( \frac{x - \widehat{\mu}}{\widehat{\sigma}} \right)^{2} \right)^{- \frac{\upsilon + 1}{2}}

Usually, this is written as
:math:`P_{\upsilon}(\widehat{\mu},{\widehat{\sigma}}^{2})`.

Mean :math:`E\lbrack x\rbrack` and variance :math:`Var\lbrack x\rbrack`
of the probability density are defined by:

:math:`E\lbrack x\rbrack = \widehat{\mu}`; :math:`\upsilon > 1`

:math:`{Var}\lbrack x\rbrack = {\widehat{\sigma}}^{2}\frac{\upsilon}{\upsilon - 2}`
; :math:`\upsilon > 2`

For deriving values attributed to the parameters, assume that a series
of repeated measurements :math:`x_{1},x_{2},\ldots,x_{n}` of
normal-distributed values is given, where :math:`\widehat{\mu}` and
:math:`{\widehat{\sigma}}^{2}` are considered as unknown. This leads to
the probability density of the input quantity :math:`X` given by the
:math:`t`-distribution of the form

:math:`P_{\upsilon}(\overline{x},\frac{s^{2}}{n})`

with following parameter values (:math:`\nu = n - 1`):

+---------------------------------+------------------------------------+
| .. math:: \overline{x} = \      | .. math:: s^{2} = \f               |
| frac{1}{n}\sum_{i = 1}^{n}x_{i} | rac{1}{n - 1}\sum_{i = 1}^{n}\left |
|                                 | ( x_{i} - \overline{x} \right)^{2} |
+=================================+====================================+
+---------------------------------+------------------------------------+

This leads to the following values of the expectation values given
above:

:math:`E\lbrack x\rbrack = \overline{x}`

:math:`{Var}\lbrack x\rbrack = \frac{s^{2}}{n}\frac{\upsilon}{\upsilon - 2} = \frac{s^{2}}{n}\frac{n - 1}{n - 3}`;

In this case, :math:`\overline{x}` and :math:`\frac{s^{2}}{n}` are
considered as the input values of the *t*-distribution as obtained by
measurements, while the factor :math:`(n - 1)/(n - 3)` follows from a
property of the *t*-distribution.

As the *t*-distribution refers to a series of measurements, the
associated input variable has to be declared as a mean variable in
UncertRadio (`see Applying means <#applying-means-in-uncertradio>`__).
This guarantees that the parameter values :math:`n`, or
:math:`\upsilon = df = n - 1`, and
:math:`\ mu = \overline{x},\ \ sigma = \ s` are known within the
program; they may be displayed by the following dialog invoked by the
toolbar icon |image42| :

|image43|

Random values :math:`t_{k}` of this distribution density are sampled
from :math:`t_{k} = \overline{x} + \frac{s^{2}}{n}t_{0k}`, where the
:math:`t_{0k}` are random values of the standard-*t*-distribution,
produced by a random generator. The factor :math:`(n - 1)/(n - 3)` must
not be part of the formula for :math:`t_{k}`; it implicitly results from
applying the :math:`t_{0k}`-values.

Generating random numbers
~~~~~~~~~~~~~~~~~~~~~~~~~

For gamma-distributed random numbers, the generator by Marsaglia and
Wang (2000) and another generator taken from Alan Miller’s repository of
Fortran-90 routines ( https://jblevins.org/mirror/amiller/ ) are
applied. For the two other special distributions, also routines from
Allan Miller’s repository are used.

Gross quantity: Variance interpolation for a mean
-------------------------------------------------

Definitions
~~~~~~~~~~~

According to chapter 6.9.1 the following definitions of means and
associated variances are applied:

**Table 1:**

+-----------------------------------+-----------------------------------+
| .                                 | .                                 |
| . math:: {\bar{x}}_{g} = \frac{1} | . math:: {\bar{x}}_{b} = \frac{1} |
| {n_{g}}\sum_{i = 1}^{n_{g}}x_{gi} | {n_{b}}\sum_{i = 1}^{n_{b}}x_{bi} |
+===================================+===================================+
| ..                                | ..                                |
| math:: s_{g}^{2} = \frac{1}{n_{g} | math:: s_{b}^{2} = \frac{1}{n_{b} |
|  - 1}\sum_{i = 1}^{n_{g}}\left( x |  - 1}\sum_{i = 1}^{n_{b}}\left( x |
| _{gi} - {\bar{x}}_{g} \right)^{2} | _{bi} - {\bar{x}}_{b} \right)^{2} |
+-----------------------------------+-----------------------------------+
| .. math:: f_{g} = \frac{n_{       | .. math:: f_{b} = \frac{n_{       |
| g} - 1}{n_{g} - 3}\frac{1}{n_{g}} | b} - 1}{n_{b} - 3}\frac{1}{n_{b}} |
+-----------------------------------+-----------------------------------+
| .. math:: u^{2                    | .. math:: u^{2                    |
| }({\bar{x}}_{g}) = f_{g}s_{g}^{2} | }({\bar{x}}_{b}) = f_{b}s_{b}^{2} |
+-----------------------------------+-----------------------------------+

For those input quantities, to which mean values are attributed, the
*t*-distribution is taken as type of distribution. The possible values,
which can be attributed to the three parameters (number of degrees of
freedom :math:`\upsilon`, mean value :math:`\widehat{\mu}`, standard
uncertainty :math:`\widehat{\sigma}` (scaling)) of the *t*-distribution,
are given in the following table for the example of the gross quantity
(subscript g); the table for the background quantity (subscript b) would
look similarly.

**Table 2:**

+---+--------------------+--------------------------+-----------------+
|   | **Method A**       | **Method B**             | **Method C**    |
|   |                    |                          |                 |
|   | **“not being       | **“counts, with          | **“classical“** |
|   | counts“**          | influence“**             |                 |
+===+====================+==========================+=================+
|   | **t-distribution** | **distribution unclear** | **normal        |
|   |                    |                          | distribution**  |
+---+--------------------+--------------------------+-----------------+
| . | .                  | .. math:: m - 1          | .. m            |
| . | . math:: n_{g} - 1 |                          | ath:: n_{g} - 1 |
|   |                    |                          |                 |
| m |                    |                          |                 |
| a |                    |                          |                 |
| t |                    |                          |                 |
| h |                    |                          |                 |
| : |                    |                          |                 |
| : |                    |                          |                 |
|   |                    |                          |                 |
| \ |                    |                          |                 |
| u |                    |                          |                 |
| p |                    |                          |                 |
| s |                    |                          |                 |
| i |                    |                          |                 |
| l |                    |                          |                 |
| o |                    |                          |                 |
| n |                    |                          |                 |
+---+--------------------+--------------------------+-----------------+
| . | .. math:: \frac    | .. math:: \frac{1}       | .. mat          |
| . | {1}{n_{g}}\sum_{i  | {m}\sum_{i = 1}^{m}n_{i} | h:: \frac{1}{n_ |
|   | = 1}^{n_{g}}x_{gi} |                          | {g}}\sum_{i = 1 |
| m |                    |                          | }^{n_{g}}x_{gi} |
| a |                    |                          |                 |
| t |                    |                          |                 |
| h |                    |                          |                 |
| : |                    |                          |                 |
| : |                    |                          |                 |
|   |                    |                          |                 |
| \ |                    |                          |                 |
| w |                    |                          |                 |
| i |                    |                          |                 |
| d |                    |                          |                 |
| e |                    |                          |                 |
| h |                    |                          |                 |
| a |                    |                          |                 |
| t |                    |                          |                 |
| { |                    |                          |                 |
| \ |                    |                          |                 |
| m |                    |                          |                 |
| u |                    |                          |                 |
| } |                    |                          |                 |
+---+--------------------+--------------------------+-----------------+
| . | .. math:: \sq      | .. math:: \sqrt{\l       | .               |
| . | rt{f_{g}s_{g}^{2}} | eft( \frac{\overline{n}} | . math:: \sqrt{ |
|   |                    | {m} + f_{g}{(\overline{n | f_{g}s_{g}^{2}} |
| m |                    | } + s}_{n}^{2}) \right)} |                 |
| a |                    |                          |                 |
| t |                    |                          |                 |
| h |                    |                          |                 |
| : |                    |                          |                 |
| : |                    |                          |                 |
|   |                    |                          |                 |
| \ |                    |                          |                 |
| w |                    |                          |                 |
| i |                    |                          |                 |
| d |                    |                          |                 |
| e |                    |                          |                 |
| h |                    |                          |                 |
| a |                    |                          |                 |
| t |                    |                          |                 |
| { |                    |                          |                 |
| \ |                    |                          |                 |
| s |                    |                          |                 |
| i |                    |                          |                 |
| g |                    |                          |                 |
| m |                    |                          |                 |
| a |                    |                          |                 |
| } |                    |                          |                 |
+---+--------------------+--------------------------+-----------------+
| . | .. math:: \frac{   | .. math:: \frac{         | .. math::       |
| . | n_{g} - 1}{n_{g} - | m - 1}{m - 3}\frac{1}{m} | \frac{1}{n_{g}} |
|   |  3}\frac{1}{n_{g}} |                          |                 |
| m |                    |                          |                 |
| a |                    |                          |                 |
| t |                    |                          |                 |
| h |                    |                          |                 |
| : |                    |                          |                 |
| : |                    |                          |                 |
|   |                    |                          |                 |
| f |                    |                          |                 |
| _ |                    |                          |                 |
| { |                    |                          |                 |
| g |                    |                          |                 |
| } |                    |                          |                 |
+---+--------------------+--------------------------+-----------------+

(:math:`\overline{n}\ ` and :math:`s_{n}^{2}` are estimated in the
correspondent manner like :math:`\overline{x}\ ` and :math:`s_{x}^{2}`
.)

**Notes:**

The distribution type of the type of mean “counts, with influence“ is a
superposition of a shifted t-distribution (mean
:math:`{(\overline{n} + s}_{n}^{2}`) and a normal distribution (mean 0);
see section 6.12.2. The case in the third column is program-internally
treated as “normal distributed“, even if in UR the *t-*\ distribution
has been chosen as distribution type.

The variance of the sum of a *t-*\ distributed and a normal distributed
quantity is given by the sum of their variances only if more than 5
individual values of the *t-*\ distributed quantity are used.

If the background-related quantity :math:`x_{b}` is not treated as a
mean, this means :math:`f_{b} = 1` is applied.

Principle of the MC simulation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The theoretical treatment of **Method B** from Table 2 was described by
Weise et al. (2013), in its Appendix C, especially section C.2 It is
shown there, how the expression for the variance
:math:`u^{2}(\overline{n})` of the mean :math:`\overline{n}` of the
numbers of counts

:math:`u^{2}\left( \overline{n} \right) = \frac{1}{m}\left( \overline{n} + \frac{(m - 1)}{(m - 3)}{(\overline{n} + s}_{n}^{2}) \right) = \frac{\overline{n}}{m} + \frac{1}{m}\frac{(m - 1)}{(m - 3)}{(\overline{n} + s}_{n}^{2})`
(1)

was derived. The first term therein, :math:`\frac{\overline{n}}{m},` is
interpreted as a counting uncertainty contribution. The second term is a
*t*-distributed contribution of additional random influences to the
variance.

For a normal distribution, the variable

:math:`t = \frac{\mu - \overline{n}}{s_{n}/\sqrt{m}}`

follows a Student *t*-distribution with :math:`(m - 1)` degrees of
freedom, expectation value of zero and variance :math:`(m - 1)/(m - 3)`;
:math:`\mu` is the expectation value of :math:`\overline{n}`. Solving
this equation for :math:`\mu` leads to the equation

:math:`\mu = \overline{n} + t\sqrt{\frac{s_{n}^{2}}{m}}` (2)

This is taken as a recipe for generating t-distributed random numbers.
With standard-*t*-distributed random numbers :math:`t_{rnd}`, the MC
values for simulating the distribution of
:math:`u^{2}\left( \overline{n} \right)` according to Eq. (1) are
derived as follows.

With

:math:`\mu = \overline{n} + t_{rnd}\sqrt{(s_{n}^{2} + \overline{n})/m}`
(3)

random values with mean :math:`\overline{n}` and variance
:math:`\frac{1}{m}\frac{(m - 1)}{(m - 3)}{(s}_{n}^{2} + \overline{n})`
are obtained; in a second step normal-distributed random values
:math:`z_{rnd}\sqrt{\overline{n}/m}` are added to this, where
:math:`z_{rnd}` are standard-normal distributed random values:

:math:`\mu = \overline{n} + t_{rnd}\sqrt{(s_{n}^{2} + \overline{n})/m} + z_{rnd}\sqrt{\overline{n}/m}`
(4)

This last step contributes to broadening the distribution.

For the less complicated case of **Method A,** only equation (2;
:math:`t` is replaced by :math:`t_{rnd}` ) is applied for generating
random values.

**Notes**:

By using *t*-distributed values the multiplicative factor
:math:`(m - 1)/(m - 3)` is generated automatically; therefore, this
factor must not be supplied in equations (2) and (3).

In the TAB “Values, uncertainties“ in UncertRadio those uncertainties
:math:`u(x)` are displayed, which correspond to the row for
:math:`\widehat{\sigma}` in table 2. Before generating MC values for an
assumed value :math:`\widetilde{y}` according to Eqs (2) or (3), the
value :math:`s_{n}^{2}` is calculated from the associated :math:`u(x)`
by reversing equations (2) or (3).

From the uncertainty :math:`u(.)` one calculates:

Eq. (2):
:math:`s_{n}^{2} = u^{2}(.)\ m\ \left( \frac{(m - 1)}{(m - 3)} \right)^{- 1}`

Eq. (3):
:math:`s_{n}^{2} = \left\lbrack \left( u^{2}(.)\ m - \overline{n} \right)\left( \frac{(m - 1)}{(m - 3)} \right)^{- 1} - \overline{n} \right\rbrack`

**Special feature of the MC simulation of decision threshold and
detection limit:**

In these cases, the factor of :math:`\sqrt{(m - 1)/(m - 3)\ }` for the
gross count rate is already contained in the expression of its
uncertainty varied according to Eq. (11, see below). As already
indicated in the notes above, this factor is implied by generating
random values :math:`t_{rnd}` : it is identical with the standard
uncertainty of the standard :math:`t` distribution. To prevent from
applying this factor twice, :math:`t_{rnd}` is simply replaced by
:math:`t_{rnd}/\sqrt{(m - 1)/(m - 3)\ }` in the equations (2) through
(4).

Procedures with unknown random influences
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is assumed that repeated measurements underly unknown random
influences, which are not small and lead to increased fluctuations. This
requires running some measurement series for estimating the gross and
background count rate (or gross and background quantities).

Using the gross scout rate for interpolation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the **value of a gross count rate Rg or a gross quantity xg** is
estimated by a **mean of a measurement series**, its uncertainty can no
longer be estimated by, e.g., u(Rg)=sqrt(Rg/t). Instead, this requires
an i\ **nterpolation between two known values of the variance**.
According to ISO 11929, this is solved for an assumed value
:math:`\widetilde{y}` of the output quantity by interpolating between
the variance :math:`u^{2}(y)` of the primary result and the variance
:math:`u^{2}(\widetilde{y} = 0)`:

:math:`u^{2}\left( \widetilde{y} \right) = u^{2}(0)\left( 1 - \frac{\widetilde{y}}{y} \right) + u^{2}(y)\frac{\widetilde{y}}{y}`
(5)

In UncertRadio, however, such an interpolation refers to corresponding
two variance values of the gross quantity :math:`{\widetilde{x}}_{g}`.
This case can be deduced from the one in Eq. (5). A **measurement model
with quantities**
:math:`{\widetilde{\mathbf{x}}}_{\mathbf{g}}\mathbf{,}\mathbf{\ \ }\mathbf{x}_{\mathbf{b}}\mathbf{\ ,}\mathbf{x}_{\mathbf{int}}`
**(gross, background, interference)** is assumed, in which both,
:math:`{\widetilde{x}}_{g}` and :math:`x_{b}` are treated as mean
values:

:math:`\widetilde{y} = w\left( {\widetilde{x}}_{g} - x_{0} - x_{int} \right)`
(6)

This means

:math:`u^{2}\left( \widetilde{y} \right) = w^{2}\left( u^{2}\left( {\widetilde{x}}_{g} \right) + u^{2}\left( x_{b} \right) + u^{2}\left( x_{int} \right) \right) + \left( {\widetilde{x}}_{g} - x_{b} - x_{int} \right)^{2}u^{2}(w)`
(7)

Equating the right-hand sides of (1) und (3) yields

:math:`w^{2}u^{2}\left( {\widetilde{x}}_{g} \right) = u^{2}(0)\left( 1 - \frac{\widetilde{y}}{y} \right) + u^{2}(y)\frac{\widetilde{y}}{y} - w^{2}u^{2}\left( x_{b} \right) - w^{2}u^{2}\left( x_{int} \right)`

:math:`\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \  - \left( {\widetilde{x}}_{g} - x_{b} - x_{int} \right)^{2}u^{2}(w)`
(8)

With setting

:math:`\frac{\widetilde{y}}{y} = \widetilde{q} = \frac{\left( {\widetilde{x}}_{g} - x_{0} - x_{int} \right)}{\left( x_{g} - x_{0} - x_{int} \right)} = \frac{{\widetilde{R}}_{n}\ \ }{R_{n}\ \ }`
, :math:`\widetilde{q} \geq 0` (9)

It follows:

:math:`w^{2}u^{2}\left( {\widetilde{x}}_{g} \right) = u^{2}(0)\left( 1 - \widetilde{q} \right) + u^{2}(y)\widetilde{q} - w^{2}u^{2}\left( x_{0} \right) - w^{2}u^{2}\left( x_{int} \right) - {\widetilde{R}}_{n}^{2}u^{2}(w)`
(10)

Now, with an expression for :math:`u^{2}(0)`:

:math:`u^{2}(0) = u^{2}\left( \widetilde{y} = 0 \right) = w^{2}\left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) + u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)`

:math:`u^{2}(0) = w^{2}2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right)`
(11)

the expression for the variance :math:`u^{2}({\widetilde{x}}_{g})`
becomes:

:math:`w^{2}u^{2}\left( {\widetilde{x}}_{g} \right) = w^{2}2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right)\left( 1 - \widetilde{q} \right) + u^{2}(y)\widetilde{q} - w^{2}u^{2}\left( x_{0} \right) - w^{2}u^{2}\left( x_{int} \right) - {\widetilde{R}}_{n}^{2}u^{2}(w)`

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = 2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right)\left( 1 - \widetilde{q} \right) + \frac{u^{2}(y)}{w^{2}}\widetilde{q} - u^{2}\left( x_{0} \right) - u^{2}\left( x_{int} \right) - {\widetilde{R}}_{n}^{2}u_{rel}^{2}(w)`

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = 2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right) - 2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right)\widetilde{q} + \frac{u^{2}(y)}{w^{2}}\widetilde{q} - u^{2}\left( x_{0} \right) - u^{2}\left( x_{int} \right)`
:math:`- {\widetilde{R}}_{n}^{2}u_{rel}^{2}(w)`

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = \left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right) - 2\left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)\widetilde{q} + \frac{u^{2}(y)}{w^{2}}\widetilde{q}\  - {\widetilde{R}}_{n}^{2}u_{rel}^{2}(w)`

Setting now
:math:`{\widetilde{R}}_{n}^{2} = {\widetilde{q}}^{2}R_{n}^{2}`:

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = \left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)\left( 1 - 2\widetilde{q} \right) + \frac{u^{2}(y)}{w^{2}}\widetilde{q}\  - {\widetilde{q}}^{2}R_{n}^{2}\ u_{rel}^{2}(w)`

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = \left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)\left( 1 - 2\widetilde{q} \right) + \widetilde{q}\left( \frac{u^{2}(y)}{w^{2}}\  - \widetilde{q}R_{n}^{2}\ u_{rel}^{2}(w) \right)`
(12)

For the program-internal application, :math:`y` und :math:`u^{2}(y)` are
also replaced:

:math:`\frac{u^{2}(y)}{w^{2}} = u^{2}\left( x_{g} \right) + u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right){+ \left( x_{g} - x_{0} - x_{int} \right)}^{2}u_{rel}^{2}(w)`

:math:`\frac{u^{2}(y)}{w^{2}} = u^{2}\left( R_{n} \right) + R_{n}^{2}u_{rel}^{2}(w)`

For the last round bracket in (12) one obtains:

:math:`\left( \frac{u^{2}(y)}{w^{2}}\  - \widetilde{q}R_{n}^{2}\ u_{rel}^{2}(w) \right) = u^{2}\left( R_{n} \right) + R_{n}^{2}u_{rel}^{2}(w) - R_{n}^{2}\widetilde{q}\ u_{rel}^{2}(w)`

:math:`\left( \frac{u^{2}(y)}{w^{2}}\  - \widetilde{q}R_{n}^{2}{\ u}_{rel}^{2}(w) \right) = u^{2}\left( R_{n} \right) + (1 - \widetilde{q})R_{n}^{2}u_{rel}^{2}(w)`
(13)

which by inserting it into in (12) yields:

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = \left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)\left( 1 - 2\widetilde{q} \right) + \widetilde{q}\left( u^{2}\left( R_{n} \right) + (1 - \widetilde{q})R_{n}^{2}{\ u}_{rel}^{2}(w) \right)`
(14)

In principle, equations (12) or (14) represent that equation or formula,
which would have to be entered by the user into the “green cell“ in the
table “values, uncertainties“ in UncertRadio. This would also imply to
add several auxiliary quantities to the symbol list in UncertRadio.
However, the already existing tool for treating mean values according to
chapter 6.9 (see also section 6.12.), offers the opportunity to gather
these auxiliary quantity values internally.

In equation (14), assumed values of the variable
:math:`{\widetilde{x}}_{g}` are set by the program within the iterations
for calculating the decision threshold and the detection limit. The
fixed values :math:`n_{g},n_{b},x_{g},x_{b},s_{g},f_{g},s_{b},f_{b}` are
taken from the two tables in 6.12., or from the program-internal data
arrays associated with the treatment of means. These also fixed values
for :math:`{w,x_{int},u}^{2}(x_{int})` are read from the UR table
„Values, uncertainties“.

Finally, it is no longer necessary to enter a formula into the „green
cell“ for the standard deviation of the gross quantity, if the value of
this quantity is given by a mean. This requires only to define the
*t*-distribution type for the quantity symbol :math:`n_{g}`.

   **Example projects:** ISO-Example-2a_EN.txp (with the old
   UR-treatment)

   ISO-Example-2a_V2_EN.txp (with the new UR-treatment)

**Equivalence of the linear interpolation alternatives**

The interpolation of output quantity variances to be applied according
to ISO 11929 shall be linear as in Eq. (1). As in this section the
interpolation instead refers to gross count rate variances, it needs to
be tested, whether the interpolated values according to these two
interpolation variants would agree. This has been tested with small R
program, separately for procedures A and B.

Application to Procedure A („not being counts“)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the variances :math:`u^{2}(x_{g})` und :math:`u^{2}(x_{b})` are taken
as products :math:`f_{g}s_{g}^{2}` and :math:`f_{b}s_{b}^{2}` according
to 6.12.1, for the model case :math:`y = w(x_{g} - x_{b} - xint)` the
result for the interpolated variance of the gross quantity
:math:`{\widetilde{x}}_{g}` is:

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = f_{g}s_{b}^{2} + u^{2}\left( x_{int} \right) + \widetilde{q}\left\lbrack f_{g}\left( s_{g}^{2} - s_{b}^{2} \right) - u^{2}\left( x_{int} \right) + \left( 1 - \widetilde{q} \right)x_{net}^{2}u_{rel}^{2}(w) \right\rbrack`
(15)

**Testing the variance interpolation:**

(Program Var_intpol_Ex13.R, for example 13 of ISO 11929-4)

The following formulae were applied for
:math:`u^{2}\left( {\widetilde{x}}_{g} \right)` which, after inserting
it into Eq. (7), allows the comparison with variance values calculated
according to Eq. (5) (:math:`x_{int}` has been set zero):

var_Rg_tilde_a = fg*(sb^2) + uxint^2 +

q_tilde \* (fg*(sg^2-sb^2)- uxint^2 + xn^2*(uw/w)^2*(1- q_tilde) )

var_Rg_tilde_b = ((fg+fb)*sb^2 + 2*uxint^2)*(1. - q_tilde) +

q_tilde*( (uym/w)^2) - fb*sb^2 - uxint^2 - q_tilde^2*(ym/w)^2*(uw/w)^2

q_tilde = (xg_tilde – xb - xint) / (xg – xb - xint)

With using the following values:

sg= 71.71839 sb= 5.895336 fg= 0.03857143 fb= 0.04012346

xgtilde= 75.704 q_tilde= 2.542306e-06

xg= 192.25 uxg= 14.08521 xb= 75.7037 uxb= 1.180885

ym= 116.5463 uym= 37.71288 uy0= 1.653796

and Eq. (6) for calculation of xg_tilde from y_tilde, for 11 values of
ytilde, between 0 and 116.54, the following variances were derived:

y_tilde xg_tilde var_xg_tilde_a var_xg_tilde_b vary_tilde_lin varytilde2

[1,] 0.00000 75.70370 1.340549 1.340549 2.73504 2.73504

[2,] 11.65463 87.35833 131.068433 131.068433 144.68766 144.68766

[3,] 23.30926 99.01296 236.346847 236.346847 286.64028 286.64028

[4,] 34.96389 110.66759 317.175789 317.175789 428.59290 428.59290

[5,] 46.61852 122.32222 373.555262 373.555262 570.54552 570.54552

[6,] 58.27315 133.97685 405.485263 405.485263 712.49814 712.49814

[7,] 69.92778 145.63148 412.965795 412.965795 854.45075 854.45075

[8,] 81.58241 157.28611 395.996855 395.996855 996.40337 996.40337

[9,] 93.23704 168.94074 354.578446 354.578446 1138.35599 1138.35599

[10,] 104.89167 180.59537 288.710565 288.710565 1280.30861 1280.30861

[11,] 116.54630 192.25000 198.393214 198.393214 1422.26123 1422.26123

There is no difference observed between the two compared variance values
of the gross quantity (columns 3, 4). The same observation applies to
the output quantity variance (columns 6, 6) calculated according to
equations (5) and (7).

This verifies the equivalence of the two compared interpolation methods.

Application to Procedure B („counts, with influence“)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For the model :math:`y = w(x_{g} - x_{b} - x_{int})`, the variances
:math:`u^{2}(x_{g})` and :math:`u^{2}(x_{b})`, calculated according to
Eq. (1), are given by:

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = \frac{u^{2}\left( {\overline{n}}_{b} \right)}{t_{b}^{2}} + u^{2}(x_{int}) + \widetilde{q}\left\lbrack \frac{u^{2}\left( {\overline{n}}_{g} \right)}{t_{g}^{2}} - \frac{u^{2}\left( {\overline{n}}_{b} \right)}{t_{b}^{2}} - u^{2}(x_{int}) + \left( 1 - \widetilde{q} \right)x_{net}^{2}u_{rel}^{2}(w) \right\rbrack`
(16)

with

:math:`\frac{u^{2}\left( {\overline{n}}_{g} \right)}{t_{g}^{2}} = \frac{1}{t_{g}^{2}}\left( \frac{{\overline{n}}_{g}}{m_{g}} + f_{g}{({\overline{n}}_{g} + s}_{g}^{2}) \right)`
;
:math:`\frac{u^{2}\left( {\overline{n}}_{b} \right)}{t_{b}^{2}} = \frac{1}{t_{b}^{2}}\left( \frac{{\overline{n}}_{b}}{m_{b}} + f_{b}{({\overline{n}}_{b} + s}_{b}^{2}) \right)`

**Testing the variance interpolation:**

(Program Var_intpol_Ex14.R, for example 14 of ISO 11929-4)

The following variants of equations for
:math:`u^{2}\left( {\widetilde{x}}_{g} \right)` were applied for
subsequent comparison with values from Eq. (5) (:math:`x_{int}` has been
set zero):

var_Rg_tilde_a = (un0_mean/t0)^2 + uxint^2 +

q_tilde \* ((ung_mean/tg)^2-(un0_mean/t0)^2 - uxint^2 +
Rn^2*(uw/w)^2*(1- q_tilde) )

var_Rg_tilde_b = ((un0_mean/t0)^2 + uxint^2) \* (1. - 2.*q_tilde) +

q_tilde*( (uym/w)^2 - q_tilde*(ym/w)^2*(uw/w)^2 )

var_Rg_green = urbt^2 + (uRg^2 - urbt^2)*(Rg_tilde-rbt)/(Rg-rbt) +

(uw/w)^2*(Rg_tilde-rbt)*(Rg-Rg_tilde)

with

q_tilde = (Rg_tilde - R0 - xint) / (Rg - R0 - xint)

Rg_tilde = ytilde/w + un0_mean/t0 + xint

rbt = R0 + xint; urbt = sqrt(uR0^2 + xint^2)

Note that „var_Rg_green“ denotes that formula that earlier had been
manually inserted into the “green cell” within UncertRadio.

With using the following values:

R0= 0.02723333 u(R0)= 0.002929202 Rg= 0.06798667 u(Rg)= 0.006185528

Rn= 0.04075333 w= 34.39972 uw= 2.786688 ym= 1.401903 uym= 0.261393

uy0= 0.1425014

11 values of ytilde, from 0 to 1.402, were calculated:

y_tilde var_Rg_tilde_a var_Rg_tilde_b var_Rg_tilde_green

[1,] 0.0000000 8.580222e-06 8.580222e-06 8.580222e-06

[2,] 0.1401903 1.252920e-05 1.252920e-05 1.252920e-05

[3,] 0.2803807 1.626019e-05 1.626019e-05 1.626019e-05

[4,] 0.4205710 1.977321e-05 1.977321e-05 1.977321e-05

[5,] 0.5607614 2.306823e-05 2.306823e-05 2.306823e-05

[6,] 0.7009517 2.614528e-05 2.614528e-05 2.614528e-05

[7,] 0.8411421 2.900434e-05 2.900434e-05 2.900434e-05

[8,] 0.9813324 3.164542e-05 3.164542e-05 3.164542e-05

[9,] 1.1215228 3.406851e-05 3.406851e-05 3.406851e-05

[10,] 1.2617131 3.627363e-05 3.627363e-05 3.627363e-05

[11,] 1.4019035 3.826076e-05 3.826076e-05 3.826076e-05

There is no difference observed between the three compared variance
values of the gross count rate.

In the following table, for every value of ytilde, the following
calculated values are shown:

var_y_tilde_lin from Eq. (1);

Rg_tilde (from reversing Eq. (2),

var_Rg_tilde (the above mentioned var_rg_tilde_b) ,

varytilde2 (after inserting var_Rg_tilde into Eq. (3),

ratio (the ratio varytilde2 / vary_tilde_lin)

y_tilde vary_tilde_lin Rg_tilde var_Rg_tilde varytilde2 ratio

[1,] 0.0000000 0.02030666 0.02723333 8.580222e-06 0.02030666 1

[2,] 0.1401903 0.02510862 0.03130867 1.252920e-05 0.02510862 1

[3,] 0.2803807 0.02991058 0.03538400 1.626019e-05 0.02991058 1

[4,] 0.4205710 0.03471254 0.03945933 1.977321e-05 0.03471254 1

[5,] 0.5607614 0.03951451 0.04353467 2.306823e-05 0.03951451 1

[6,] 0.7009517 0.04431647 0.04761000 2.614528e-05 0.04431647 1

[7,] 0.8411421 0.04911843 0.05168533 2.900434e-05 0.04911843 1

[8,] 0.9813324 0.05392039 0.05576067 3.164542e-05 0.05392039 1

[9,] 1.1215228 0.05872235 0.05983600 3.406851e-05 0.05872235 1

[10,] 1.2617131 0.06352432 0.06391133 3.627363e-05 0.06352432 1

[11,] 1.4019035 0.06832628 0.06798667 3.826076e-05 0.06832628 1

This verifies the equivalence of the two compared interpolation methods.

Procedures with known random influences
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is assumed that repeated measurements underly unknown random
influences, which are small. It is furthermore assumed, that the gross
and background and other counts are influenced in the same way, also in
the case of different measurements but the same measurement conditions.
With a reference analysis, i.e., with a larger number :math:`m` of
measurements of a sample (which gets a subscript r), the unknown
influence can be quantified by parameter :math:`\vartheta`, which is
applied also to the other involved measurement quantities like gross and
background counts.

The parameter :math:`\vartheta` has already been introduced in chapter
6.9.1. It is determined from the reference data set and applied to the
uncertainty calculations of the other count numbers (subscript x):

:math:`u^{2}({\overline{n}}_{x}) = \left( {\overline{n}}_{x} + \vartheta^{2}{\overline{n}}_{x}^{2} \right)/m_{x}`

or in the case of gross and background counts as well as for assumed
gross counts :math:`{\widetilde{n}}_{g}` within the detection limit
related iterations:

:math:`u^{2}({\overline{n}}_{g}) = \left( {\overline{n}}_{g} + \vartheta^{2}{\overline{n}}_{g}^{2} \right)/m_{g}`
(1)

:math:`u^{2}({\overline{n}}_{b}) = \left( {\overline{n}}_{b} + \vartheta^{2}{\overline{n}}_{b}^{2} \right)/m_{b}`
(2)

:math:`u^{2}({\widetilde{n}}_{g}) = \left( {\widetilde{n}}_{g} + \vartheta^{2}{\widetilde{n}}_{g}^{2} \right)/m_{g}`
(3)

With applying the tool for means (6.9; see also 6.12.1), the data
necessary for calculating :math:`\vartheta`, but also those data
referring to mean value-related datasets are available within the
program. Therefore, the formulae corresponding to the equations (1), (2)
and (3) are easily programmed and are part of the program. This means,
in contrast to earlier UncertRadio versions, it is no longer necessary
for the user to enter uncertainty formulae with the TAB “Values,
uncertainties“; the introduction of further auxiliary quantities also is
no longer necessary.

In addition, from the datasets supplied to UR, that one of them
representing the reference measurements from which :math:`\vartheta` has
to be derived, has to be identified. This can be done with a combobox
field within the dialog shown in chapter 6.9.2.

**Example-projects:** ISO-Example-2b_EN.txp, Mean-theta_EN.txp (with the
old UR treatment)

ISO-Example-2b_V2_EN.txp (with the new UR treatment)

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
`6.9 <#using-data-sets-for-mean-and-variance>`__ und
`6.12 <#gross-quantity-variance-interpolation-for-a-mean>`__.

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
dependent quantities :math:`x_{a,i}` , are taken into account; refer to
section `Kap. 6.1. <#uncertainty-propagation>`__

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