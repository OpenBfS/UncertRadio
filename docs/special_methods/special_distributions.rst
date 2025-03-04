Special distributions and their properties
------------------------------------------

The following probability distributions are implemented in UncertRadio:

+--------------------------------+--------------+------------------------------------------------------+
| distribution                   | code         | notes                                                |
+================================+==============+======================================================+
| Normal distribution            | Normal       |                                                      |
+--------------------------------+--------------+------------------------------------------------------+
| Rectangular distribution       | Rectangle    |                                                      |
+--------------------------------+--------------+------------------------------------------------------+
| Triangular distribution        | Triangle     |                                                      |
+--------------------------------+--------------+------------------------------------------------------+
| (N+x) Rule                     | (N+x) Rule   | :doc:`see </special_methods/low_level_applications>` |
+--------------------------------+--------------+------------------------------------------------------+
| Lognormal distribution         | LogNormal    |                                                      |
+--------------------------------+--------------+------------------------------------------------------+
| Gamma distribution             | GammaDist    |                                                      |
+--------------------------------+--------------+------------------------------------------------------+
|| Binomial+Poisson distribution || Binom+Poiss || :doc:`see </special_methods/short-lived_nuclide>`   |
||                               ||             ||                                                     |
+--------------------------------+--------------+------------------------------------------------------+
|| Beta distribution, 2          || Beta2Dist   ||                                                     |
|| parameters                    ||             ||                                                     |
+--------------------------------+--------------+------------------------------------------------------+
| T distribution                 | T-Distrib    |                                                      |
+--------------------------------+--------------+------------------------------------------------------+
|| Beta distribution, 4          || Beta4Dist   ||                                                     |
|| parameters                    ||             ||                                                     |
+--------------------------------+--------------+------------------------------------------------------+
|| Erlang distribution of the    || Npreset     || is a Gamma                                          |
|| counting duration for preset  ||             || distribution for                                    |
|| count numbers                 ||             || integer numbers of                                  |
||                               ||             || counts                                              |
+--------------------------------+--------------+------------------------------------------------------+

In addition to better known distributions, like e.g. normal, rectangular
or triangular distributions, special distributions can be applied,

-  the gamma distribution,

-  the beta distribution and

-  the *t*-distribution.

the properties of which are described in the following. Their
application requires two or three specific parameters.

In the following, the probability density functions (pdf) and the
relation of their parameters to measured data are shortly introduced.

The icon |Distrib_24| allow to invoke a dialog for showing the
distribution-related parameters of an input quantity. This requires that
the row within the table “Values, uncertainties” needs to be
highlighted.


.. |Distrib_24| image:: /icons/Distrib_24.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link


The gamma distribution
^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^

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

:math:`{Var}\lbrack x\rbrack = {\widehat{\sigma}}^{2}\frac{\upsilon}{\upsilon - 2}` ; :math:`\upsilon > 2`

For deriving values attributed to the parameters, assume that a series
of repeated measurements :math:`x_{1},x_{2},\ldots,x_{n}` of
normal-distributed values is given, where :math:`\widehat{\mu}` and
:math:`{\widehat{\sigma}}^{2}` are considered as unknown. This leads to
the probability density of the input quantity :math:`X` given by the
:math:`t`-distribution of the form

:math:`P_{\upsilon}(\overline{x},\frac{s^{2}}{n})`

with following parameter values (:math:`\nu = n - 1`):

.. math::
    \overline{x} = \frac{1}{n}\sum_{i = 1}^{n}x_{i}

.. math::
    s^{2} = \frac{1}{n - 1}\sum_{i = 1}^{n}\left( x_{i} - \overline{x} \right)^{2}


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
UncertRadio (see :ref:`Applying means <Applying means in UncertRadio>`).
This guarantees that the parameter values :math:`n`, or
:math:`\upsilon = df = n - 1`, and
:math:`\ mu = \overline{x},\ \ sigma = \ s` are known within the
program; they may be displayed by the following dialog invoked by the
toolbar icon |Distrib_24|:

.. image:: /images/t_distribution_dialog.png
    :align: center


Random values :math:`t_{k}` of this distribution density are sampled
from :math:`t_{k} = \overline{x} + \frac{s^{2}}{n}t_{0k}`, where the
:math:`t_{0k}` are random values of the standard-*t*-distribution,
produced by a random generator. The factor :math:`(n - 1)/(n - 3)` must
not be part of the formula for :math:`t_{k}`; it implicitly results from
applying the :math:`t_{0k}`-values.

Generating random numbers
^^^^^^^^^^^^^^^^^^^^^^^^^

For gamma-distributed random numbers, the generator by Marsaglia and
Wang (2000) and another generator taken from `Alan Mille's repository <https://jblevins.org/mirror/amiller/>`_ of
Fortran-90 routines are applied. For the two other special distributions, also routines from
Allan Miller's repository are used.