Combined standard uncertainty
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The combined standard uncertainty :math:`u_{c}(y)` of the output
quantity :math:`y` is determined according to ISO GUM (ISO **G**\ uide on
**U**\ ncertainty of **M**\ easurement (1995); see also EURACHEM / CITAC
Guide "Quantifying Uncertainty in Analytical Measurement" (2000)) using
the "Gaussian law of propagating uncertainties" and taking covariances
between individual input quantities.

For a type A standard uncertainty of a quantity which must be derived
from a set of repeated measurements a necessary small statistical
evaluation of mean and standard deviation has to be performed outside
this program; only mean and standard deviation will be used in
UncertRadio. Usually, with measurements of activities the greater part
of uncertainties of quantities belongs to Type B. Within the framework
of the **Bayesian theory of measurement** (Weise & Wöger, 1999; Weise et
al., 2006) which is underlying the basics of this program an **explicit
differentiation between quantities of type A and B is not necessary**.
This is also the reason that degrees of freedom are not considered in
this program.

The analytical derivation of formulae for the combined uncertainty for
instance of a mass or volume dependent activity using the law of
propagating uncertainties may easily yield a certain number of less or
more complex formulae the correctness of which is often not easily being
controlled. Therefore, a numerical procedure is applied. The first step
consists in transferring all quantities/parameters being required for
the calculations, these may easily become more than 20, into a program
array **MeasdValue(i) = p(i)**. Then, in a subprogram RESULT the value
of the output quantity is calculated from the values of the array
elements *p(i)* by using the function parser.

Similarly, the known measurement uncertainties of the individual
quantities/parameters are transferred to an array **StdUnc(i) = u(i)**.
For the calculation of the combined uncertainty a `subroutine
Uncpropa <#URH_UNCPROPA_EN>`__ is used to which the two arrays *p(i)*
and *u(i)* are transferred. Covariances are considered in this
subroutine. The sensitivity coefficients, i.e. the partial derivates of
the function calculated with RESULT with respect to the array elements
*p(i)*, are numerically approximated by differential quotients.

Further details: see `Uncertainty
propagation <#uncertainty-propagation>`__
