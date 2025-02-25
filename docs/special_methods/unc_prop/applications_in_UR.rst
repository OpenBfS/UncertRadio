6.1.3 Applications in UR
^^^^^^^^^^^^^^^^^^^^^^^^

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