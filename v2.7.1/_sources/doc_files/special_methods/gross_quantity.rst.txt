Gross quantity: Variance interpolation for a mean
-------------------------------------------------

Definitions
^^^^^^^^^^^

According to section :ref:`mathematical background` the following definitions of means and
associated variances are applied:


.. list-table::
    :widths: 50 50
    :header-rows: 0
    :name: gross_quantity_tab1

    * - :math:`\bar{x}_{g} = \frac{1}{n_{g}}\sum_{i=1}^{n_{g}}x_{gi}`
      - :math:`\bar{x}_{b} = \frac{1}{n_{b}}\sum_{i=1}^{n_{b}}x_{bi}`
    * - :math:`s_{g}^{2} = \frac{1}{n_{g}-1}\sum_{i=1}^{n_{g}}\left(x_{gi} - \bar{x}_{g}\right)^{2}`
      - :math:`s_{b}^{2} = \frac{1}{n_{b}-1}\sum_{i=1}^{n_{b}}\left(x_{bi} - \bar{x}_{b}\right)^{2}`
    * - :math:`f_{g} = \frac{n_{g}-1}{n_{g}-3}\frac{1}{n_{g}}`
      - :math:`f_{b} = \frac{n_{b}-1}{n_{b}-3}\frac{1}{n_{b}}`
    * - :math:`u^{2}(\bar{x}_{g}) = f_{g}\,s_{g}^{2}`
      - :math:`u^{2}(\bar{x}_{b}) = f_{b}\,s_{b}^{2}`


For those input quantities, to which mean values are attributed, the
*t*-distribution is taken as type of distribution. The possible values,
which can be attributed to the three parameters (number of degrees of
freedom :math:`\upsilon`, mean value :math:`\widehat{\mu}`, standard
uncertainty :math:`\widehat{\sigma}` (scaling)) of the *t*-distribution,
are given in the following table for the example of the gross quantity
(subscript g); the table for the background quantity (subscript b) would
look similarly.

.. list-table::
    :widths: 10 30 30 30
    :header-rows: 1
    :name: gross_quantity_tab2

    * -
      - Method A |br| “not being counts“
      - Method B |br| “counts, with influence“
      - Method C |br| “classical“
    * -
      - t-distribution
      - distribution unclear
      - normal distribution
    * - :math:`\upsilon`
      - :math:`n_{g} - 1`
      - :math:`m - 1`
      - :math:`n_{g} - 1`
    * - :math:`\widehat{\mu}`
      - :math:`\frac{1}{m} \sum_{i=1}^{m} n_{i}`
      - :math:`\frac{1}{n_{g}} \sum_{i=1}^{n_{g}} x_{gi}`
      - :math:`\frac{1}{m} \sum_{i=1}^{m} n_{i}`
    * - :math:`\widehat{\sigma}`
      - :math:`\sqrt{f_{g} s_{g}^{2}}`
      - :math:`\sqrt{\left( \frac{\overline{n}}{m} + f_{g} \left( \overline{n} + s_{n}^{2} \right) \right)}`
      - :math:`\sqrt{f_{g} s_{g}^{2}}`
    * - :math:`f_{g}`
      - :math:`\frac{n_{g} - 1}{n_{g} - 3} \frac{1}{n_{g}}`
      - :math:`\frac{m - 1}{m - 3} \frac{1}{m}`
      - :math:`\frac{1}{n_{g}}`


.. |br| raw:: html

   <br/>

(:math:`\overline{n}\ ` and :math:`s_{n}^{2}` are estimated in the
correspondent manner like :math:`\overline{x}\ ` and :math:`s_{x}^{2}`
.)

**Notes:**

The distribution type of the type of mean “counts, with influence“ is a
superposition of a shifted t-distribution (mean
:math:`{(\overline{n} + s}_{n}^{2}`) and a normal distribution (mean 0);
see section :ref:`principle of the mc simulation`.
The case in the third column is program-internally
treated as “normal distributed“, even if in UR the *t-*\ distribution
has been chosen as distribution type.

The variance of the sum of a *t-*\ distributed and a normal distributed
quantity is given by the sum of their variances only if more than 5
individual values of the *t-*\ distributed quantity are used.

If the background-related quantity :math:`x_{b}` is not treated as a
mean, this means :math:`f_{b} = 1` is applied.

Principle of the MC simulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The theoretical treatment of **Method B** from Table 2 was described by
Weise et al. (2013), in its Appendix C, especially section C.2 It is
shown there, how the expression for the variance
:math:`u^{2}(\overline{n})` of the mean :math:`\overline{n}` of the
numbers of counts

.. math::
    u^{2}\left( \overline{n} \right) = \frac{1}{m}\left( \overline{n} + \frac{(m - 1)}{(m - 3)}{(\overline{n} + s_{n}^{2})} \right)
    = \frac{\overline{n}}{m} + \frac{1}{m}\frac{(m - 1)}{(m - 3)}{(\overline{n} + s_{n}^{2})}
    :label: eq:variance_expression

was derived. The first term therein, :math:`\frac{\overline{n}}{m},` is
interpreted as a counting uncertainty contribution. The second term is a
*t*-distributed contribution of additional random influences to the
variance.

For a normal distribution, the variable

.. math::
    t = \frac{\mu - \overline{n}}{s_{n}/\sqrt{m}}
    :label: eq:student_t

follows a Student *t*-distribution with :math:`(m - 1)` degrees of
freedom, expectation value of zero and variance :math:`(m - 1)/(m - 3)`;
:math:`\mu` is the expectation value of :math:`\overline{n}`. Solving
this equation for :math:`\mu` leads to the equation

.. math::
    \mu = \overline{n} + t\sqrt{\frac{s_{n}^{2}}{m}}
    :label: eq:mu_expression

This is taken as a recipe for generating t-distributed random numbers.
With standard-*t*-distributed random numbers :math:`t_{rnd}`, the MC
values for simulating the distribution of
:math:`u^{2}\left( \overline{n} \right)` according to Eq. :eq:`eq:variance_expression` are
derived as follows.

With

.. math::
    \mu = \overline{n} + t_{rnd}\sqrt{(s_{n}^{2} + \overline{n})/m}
    :label: eq:mu_random

random values with mean :math:`\overline{n}` and variance

.. math::
    \frac{1}{m}\frac{(m - 1)}{(m - 3)}{(s_{n}^{2} + \overline{n})}
    :label: eq:variance_random

are obtained; in a second step normal-distributed random values

.. math::
    z_{rnd}\sqrt{\overline{n}/m}
    :label: eq:normal_random

are added to this, where
:math:`z_{rnd}` are standard-normal distributed random values:

.. math::
    \mu = \overline{n} + t_{rnd}\sqrt{(s_{n}^{2} + \overline{n})/m} + z_{rnd}\sqrt{\overline{n}/m}
    :label: eq:final_mu

This last step contributes to broadening the distribution.

For the less complicated case of **Method A,** only equation (2;
:math:`t` is replaced by :math:`t_{rnd}` ) is applied for generating
random values.

**Notes**:

By using *t*-distributed values the multiplicative factor
:math:`(m - 1)/(m - 3)` is generated automatically; therefore, this
factor must not be supplied in equations :eq:`eq:mu_expression` and :eq:`eq:mu_random`.

In the :ref:`tab “values, uncertainties”` in UncertRadio those uncertainties
:math:`u(x)` are displayed, which correspond to the row for
:math:`\widehat{\sigma}` in table 2. Before generating MC values for an
assumed value :math:`\widetilde{y}` according to Eqs :eq:`eq:mu_expression` or :eq:`eq:mu_random`, the
value :math:`s_{n}^{2}` is calculated from the associated :math:`u(x)`
by reversing equations :eq:`eq:mu_expression` or :eq:`eq:mu_random`.

From the uncertainty :math:`u(.)` one calculates:

Eq. :eq:`eq:mu_expression`:

.. math::
   s_{n}^{2} = u^{2}(.)\ m\ \left( \frac{(m - 1)}{(m - 3)} \right)^{- 1}
   :label: eq:sn_squared_2

Eq. :eq:`eq:mu_random`:

.. math::
   s_{n}^{2} = \left\lbrack \left( u^{2}(.)\ m - \overline{n} \right)\left( \frac{(m - 1)}{(m - 3)} \right)^{- 1} - \overline{n} \right\rbrack
   :label: eq:sn_squared_3

**Special feature of the MC simulation of decision threshold and
detection limit:**

In these cases, the factor of :math:`\sqrt{(m - 1)/(m - 3)\ }` for the
gross count rate is already contained in the expression of its
uncertainty varied according to Eq. (11, see below). As already
indicated in the notes above, this factor is implied by generating
random values :math:`t_{rnd}` : it is identical with the standard
uncertainty of the standard :math:`t` distribution. To prevent from
applying this factor twice, :math:`t_{rnd}` is simply replaced by
:math:`t_{rnd}/\sqrt{(m - 1)/(m - 3)\ }` in the equations :eq:`eq:mu_expression` through
:eq:`eq:final_mu`.

Procedures with unknown random influences
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is assumed that repeated measurements underly unknown random
influences, which are not small and lead to increased fluctuations. This
requires running some measurement series for estimating the gross and
background count rate (or gross and background quantities).

Using the gross count rate for interpolation
++++++++++++++++++++++++++++++++++++++++++++

If the **value of a gross count rate Rg or a gross quantity xg** is
estimated by a **mean of a measurement series**, its uncertainty can no
longer be estimated by, e.g., u(Rg)=sqrt(Rg/t). Instead, this requires
an i\ **nterpolation between two known values of the variance**.
According to ISO 11929, this is solved for an assumed value
:math:`\widetilde{y}` of the output quantity by interpolating between
the variance :math:`u^{2}(y)` of the primary result and the variance
:math:`u^{2}(\widetilde{y} = 0)`:

.. math::
    u^{2}\left( \widetilde{y} \right) = u^{2}(0)\left( 1 - \frac{\widetilde{y}}{y} \right) + u^{2}(y)\frac{\widetilde{y}}{y}
    :label: eq:gross_count_rate_int_5

In UncertRadio, however, such an interpolation refers to corresponding
two variance values of the gross quantity :math:`{\widetilde{x}}_{g}`.
This case can be deduced from the one in Eq. :eq:`eq:gross_count_rate_int_5`. A **measurement model
with quantities**
:math:`{\widetilde{\mathbf{x}}}_{\mathbf{g}}\mathbf{,}\mathbf{\ \ }\mathbf{x}_{\mathbf{b}}\mathbf{\ ,}\mathbf{x}_{\mathbf{int}}`
**(gross, background, interference)** is assumed, in which both,
:math:`{\widetilde{x}}_{g}` and :math:`x_{b}` are treated as mean
values:

.. math::
    \widetilde{y} = w\left( {\widetilde{x}}_{g} - x_{0} - x_{int} \right)
    :label: eq:gross_count_rate_int_6

This means

.. math::
    u^{2}\left( \widetilde{y} \right) = w^{2}\left( u^{2}\left( {\widetilde{x}}_{g} \right) + u^{2}\left( x_{b} \right) + u^{2}\left( x_{int} \right) \right) + \left( {\widetilde{x}}_{g} - x_{b} - x_{int} \right)^{2}u^{2}(w)
    :label: eq:gross_count_rate_int_7

Equating the right-hand sides of :eq:`eq:variance_expression` und (3) yields

.. math::
    w^{2}u^{2}\left( {\widetilde{x}}_{g} \right) = u^{2}(0)\left( 1 - \frac{\widetilde{y}}{y} \right) + u^{2}(y)\frac{\widetilde{y}}{y} - w^{2}u^{2}\left( x_{b} \right) - w^{2}u^{2}\left( x_{int} \right)
    - \left( {\widetilde{x}}_{g} - x_{b} - x_{int} \right)^{2}u^{2}(w)
    :label: eq:gross_count_rate_int_8

With setting


.. math::
    \frac{\widetilde{y}}{y} = \widetilde{q} = \frac{\left( {\widetilde{x}}_{g} - x_{0} - x_{int} \right)}{\left( x_{g} - x_{0} - x_{int} \right)} = \frac{{\widetilde{R}}_{n}}{R_{n}}, \quad \widetilde{q} \geq 0
    :label: eq:gross_count_rate_int_9


It follows:


.. math::
    w^{2}u^{2}\left( {\widetilde{x}}_{g} \right) = u^{2}(0)\left( 1 - \widetilde{q} \right) + u^{2}(y)\widetilde{q} - w^{2}u^{2}\left( x_{0} \right) - w^{2}u^{2}\left( x_{int} \right) - {\widetilde{R}}_{n}^{2}u^{2}(w)
    :label: eq:gross_count_rate_int_10

Now, with an expression for :math:`u^{2}(0)`:

.. math::
    u^{2}(0) = u^{2}\left( \widetilde{y} = 0 \right) = w^{2}\left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) + u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)
    :label: eq:gross_count_rate_int_11

.. math::
    u^{2}(0) = w^{2}2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right)
    :label: eq:gross_count_rate_int_11_2

the expression for the variance :math:`u^{2}({\widetilde{x}}_{g})`
becomes:

:math:`w^{2}u^{2}\left( {\widetilde{x}}_{g} \right) = w^{2}2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right)\left( 1 - \widetilde{q} \right) + u^{2}(y)\widetilde{q} - w^{2}u^{2}\left( x_{0} \right) - w^{2}u^{2}\left( x_{int} \right) - {\widetilde{R}}_{n}^{2}u^{2}(w)`

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = 2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right)\left( 1 - \widetilde{q} \right) + \frac{u^{2}(y)}{w^{2}}\widetilde{q} - u^{2}\left( x_{0} \right) - u^{2}\left( x_{int} \right) - {\widetilde{R}}_{n}^{2}u_{rel}^{2}(w)`

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = 2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right) - 2\left( u^{2}\left( x_{0} \right) + u^{2}(x_{int}) \right)\widetilde{q} + \frac{u^{2}(y)}{w^{2}}\widetilde{q} - u^{2}\left( x_{0} \right) - u^{2}\left( x_{int} \right)`
:math:`- {\widetilde{R}}_{n}^{2}u_{rel}^{2}(w)`

:math:`u^{2}\left( {\widetilde{x}}_{g} \right) = \left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right) - 2\left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)\widetilde{q} + \frac{u^{2}(y)}{w^{2}}\widetilde{q}\  - {\widetilde{R}}_{n}^{2}u_{rel}^{2}(w)`

Setting now
:math:`{\widetilde{R}}_{n}^{2} = {\widetilde{q}}^{2}R_{n}^{2}`:

.. math::
    \begin{align}
    u^{2}\left( {\widetilde{x}}_{g} \right) &= \left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)\left( 1 - 2\widetilde{q} \right) + \frac{u^{2}(y)}{w^{2}}\widetilde{q} - {\widetilde{q}}^{2}R_{n}^{2}\ u_{rel}^{2}(w) \\
    u^{2}\left( {\widetilde{x}}_{g} \right) &= \left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)\left( 1 - 2\widetilde{q} \right) + \widetilde{q}\left( \frac{u^{2}(y)}{w^{2}} - \widetilde{q}R_{n}^{2}\ u_{rel}^{2}(w) \right)
    \end{align}
    :label: eq:gross_count_rate_int_12

For the program-internal application, :math:`y` und :math:`u^{2}(y)` are
also replaced:

:math:`\frac{u^{2}(y)}{w^{2}} = u^{2}\left( x_{g} \right) + u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right){+ \left( x_{g} - x_{0} - x_{int} \right)}^{2}u_{rel}^{2}(w)`

:math:`\frac{u^{2}(y)}{w^{2}} = u^{2}\left( R_{n} \right) + R_{n}^{2}u_{rel}^{2}(w)`

For the last round bracket in :eq:`eq:gross_count_rate_int_12` one obtains:

.. math::
    \begin{align}
    \left( \frac{u^{2}(y)}{w^{2}} - \widetilde{q}R_{n}^{2} u_{rel}^{2}(w) \right) &= u^{2}\left( R_{n} \right) + R_{n}^{2}u_{rel}^{2}(w) - R_{n}^{2}\widetilde{q}\ u_{rel}^{2}(w) \\
    \left( \frac{u^{2}(y)}{w^{2}} - \widetilde{q}R_{n}^{2} u_{rel}^{2}(w) \right) &= u^{2}\left( R_{n} \right) + (1 - \widetilde{q})R_{n}^{2}u_{rel}^{2}(w)
    \end{align}
    :label: eq:gross_count_rate_int_13

which by inserting it into in :eq:`eq:gross_count_rate_int_12` yields:

.. math::
    u^{2}\left( {\widetilde{x}}_{g} \right) = \left( u^{2}\left( x_{0} \right) + u^{2}\left( x_{int} \right) \right)\left( 1 - 2\widetilde{q} \right) + \widetilde{q}\left( u^{2}\left( R_{n} \right) + (1 - \widetilde{q})R_{n}^{2} u_{rel}^{2}(w) \right)
    :label: eq:gross_count_rate_int_14

In principle, equations :eq:`eq:gross_count_rate_int_12` or :eq:`eq:gross_count_rate_int_14` represent that equation or formula,
which would have to be entered by the user into the “green cell“ in the
table “values, uncertainties“ in UncertRadio. This would also imply to
add several auxiliary quantities to the symbol list in UncertRadio.
However, the already existing tool for treating mean values according to
chapter 6.9 (see also section 6.12.), offers the opportunity to gather
these auxiliary quantity values internally.

In equation :eq:`eq:gross_count_rate_int_14`, assumed values of the variable
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
to ISO 11929 shall be linear as in Eq. :eq:`eq:variance_expression`. As in this section the
interpolation instead refers to gross count rate variances, it needs to
be tested, whether the interpolated values according to these two
interpolation variants would agree. This has been tested with small R
program, separately for procedures A and B.

Application to Procedure A („not being counts“)
+++++++++++++++++++++++++++++++++++++++++++++++

If the variances :math:`u^{2}(x_{g})` und :math:`u^{2}(x_{b})` are taken
as products :math:`f_{g}s_{g}^{2}` and :math:`f_{b}s_{b}^{2}` according
to 6.12.1, for the model case :math:`y = w(x_{g} - x_{b} - xint)` the
result for the interpolated variance of the gross quantity
:math:`{\widetilde{x}}_{g}` is:

.. math::
    u^{2}\left( {\widetilde{x}}_{g} \right) = f_{g}s_{b}^{2} + u^{2}\left( x_{int} \right) + \widetilde{q}\left\lbrack f_{g}\left( s_{g}^{2} - s_{b}^{2} \right) - u^{2}\left( x_{int} \right) + \left( 1 - \widetilde{q} \right)x_{net}^{2}u_{rel}^{2}(w) \right\rbrack
    :label: eq:gross_count_rate_int_15

**Testing the variance interpolation:**

(Program Var_intpol_Ex13.R, for example 13 of ISO 11929-4)

The following formulae were applied for
:math:`u^{2}\left( {\widetilde{x}}_{g} \right)` which, after inserting
it into Eq. :eq:`eq:gross_count_rate_int_7`, allows the comparison with variance values calculated
according to Eq. :eq:`eq:gross_count_rate_int_5` (:math:`x_{int}` has been set zero):

.. code-block::

    var_Rg_tilde_a = fg*(sb^2) + uxint^2 +
    q_tilde \* (fg*(sg^2-sb^2)- uxint^2 + xn^2*(uw/w)^2*(1- q_tilde) )
    var_Rg_tilde_b = ((fg+fb)*sb^2 + 2*uxint^2)*(1. - q_tilde) +
    q_tilde*( (uym/w)^2) - fb*sb^2 - uxint^2 - q_tilde^2*(ym/w)^2*(uw/w)^2
    q_tilde = (xg_tilde – xb - xint) / (xg – xb - xint)

With using the following values:

.. code-block::

    sg= 71.71839 sb= 5.895336 fg= 0.03857143 fb= 0.04012346
    xgtilde= 75.704 q_tilde= 2.542306e-06
    xg= 192.25 uxg= 14.08521 xb= 75.7037 uxb= 1.180885
    ym= 116.5463 uym= 37.71288 uy0= 1.653796


and Eq. (6) for calculation of xg_tilde from y_tilde, for 11 values of
ytilde, between 0 and 116.54, the following variances were derived:

.. list-table::
   :header-rows: 1

   * - Index
     - y_tilde
     - xg_tilde
     - var_xg_tilde_a
     - var_xg_tilde_b
     - vary_tilde_lin
     - varytilde2
   * - [1,]
     - 0.00000
     - 75.70370
     - 1.340549
     - 1.340549
     - 2.73504
     - 2.73504
   * - [2,]
     - 11.65463
     - 87.35833
     - 131.068433
     - 131.068433
     - 144.68766
     - 144.68766
   * - [3,]
     - 23.30926
     - 99.01296
     - 236.346847
     - 236.346847
     - 286.64028
     - 286.64028
   * - [4,]
     - 34.96389
     - 110.66759
     - 317.175789
     - 317.175789
     - 428.59290
     - 428.59290
   * - [5,]
     - 46.61852
     - 122.32222
     - 373.555262
     - 373.555262
     - 570.54552
     - 570.54552
   * - [6,]
     - 58.27315
     - 133.97685
     - 405.485263
     - 405.485263
     - 712.49814
     - 712.49814
   * - [7,]
     - 69.92778
     - 145.63148
     - 412.965795
     - 412.965795
     - 854.45075
     - 854.45075
   * - [8,]
     - 81.58241
     - 157.28611
     - 395.996855
     - 395.996855
     - 996.40337
     - 996.40337
   * - [9,]
     - 93.23704
     - 168.94074
     - 354.578446
     - 354.578446
     - 1138.35599
     - 1138.35599
   * - [10,]
     - 104.89167
     - 180.59537
     - 288.710565
     - 288.710565
     - 1280.30861
     - 1280.30861
   * - [11,]
     - 116.54630
     - 192.25000
     - 198.393214
     - 198.393214
     - 1422.26123
     - 1422.26123


There is no difference observed between the two compared variance values
of the gross quantity (columns 3, 4). The same observation applies to
the output quantity variance (columns 6, 6) calculated according to
equations :eq:`eq:gross_count_rate_int_5` and :eq:`eq:gross_count_rate_int_7`.

This verifies the equivalence of the two compared interpolation methods.

Application to Procedure B („counts, with influence“)
+++++++++++++++++++++++++++++++++++++++++++++++++++++

For the model :math:`y = w(x_{g} - x_{b} - x_{int})`, the variances
:math:`u^{2}(x_{g})` and :math:`u^{2}(x_{b})`, calculated according to
Eq. :eq:`eq:variance_expression`, are given by:

.. math::
    u^{2}\left( {\widetilde{x}}_{g} \right) = \frac{u^{2}\left( {\overline{n}}_{b} \right)}{t_{b}^{2}} + u^{2}(x_{int}) + \widetilde{q}\left\lbrack \frac{u^{2}\left( {\overline{n}}_{g} \right)}{t_{g}^{2}} - \frac{u^{2}\left( {\overline{n}}_{b} \right)}{t_{b}^{2}} - u^{2}(x_{int}) + \left( 1 - \widetilde{q} \right)x_{net}^{2}u_{rel}^{2}(w) \right\rbrack
    :label: eq:gross_count_rate_int_16

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

.. code-block::

    var_Rg_tilde_a = (un0_mean/t0)^2 + uxint^2 +
    q_tilde \* ((ung_mean/tg)^2-(un0_mean/t0)^2 - uxint^2 +
    Rn^2*(uw/w)^2*(1- q_tilde) )
    var_Rg_tilde_b = ((un0_mean/t0)^2 + uxint^2) \* (1. - 2.*q_tilde) +
    q_tilde*( (uym/w)^2 - q_tilde*(ym/w)^2*(uw/w)^2 )
    var_Rg_green = urbt^2 + (uRg^2 - urbt^2)*(Rg_tilde-rbt)/(Rg-rbt) +
    (uw/w)^2*(Rg_tilde-rbt)*(Rg-Rg_tilde)


with

.. code-block::

    q_tilde = (Rg_tilde - R0 - xint) / (Rg - R0 - xint)
    Rg_tilde = ytilde/w + un0_mean/t0 + xint
    rbt = R0 + xint; urbt = sqrt(uR0^2 + xint^2)

Note that „var_Rg_green“ denotes that formula that earlier had been
manually inserted into the “green cell” within UncertRadio.

With using the following values:

.. code-block::

    R0= 0.02723333 u(R0)= 0.002929202 Rg= 0.06798667 u(Rg)= 0.006185528
    Rn= 0.04075333 w= 34.39972 uw= 2.786688 ym= 1.401903 uym= 0.261393
    uy0= 0.1425014


.. list-table:: Example List Table
   :header-rows: 1

   * - Dataset
     - y_tilde
     - var_Rg_tilde_a
     - var_Rg_tilde_b
     - var_Rg_tilde_green
   * - [1,]
     - 0.0000000
     - 8.580222e-06
     - 8.580222e-06
     - 8.580222e-06
   * - [2,]
     - 0.1401903
     - 1.252920e-05
     - 1.252920e-05
     - 1.252920e-05
   * - [3,]
     - 0.2803807
     - 1.626019e-05
     - 1.626019e-05
     - 1.626019e-05
   * - [4,]
     - 0.4205710
     - 1.977321e-05
     - 1.977321e-05
     - 1.977321e-05
   * - [5,]
     - 0.5607614
     - 2.306823e-05
     - 2.306823e-05
     - 2.306823e-05
   * - [6,]
     - 0.7009517
     - 2.614528e-05
     - 2.614528e-05
     - 2.614528e-05
   * - [7,]
     - 0.8411421
     - 2.900434e-05
     - 2.900434e-05
     - 2.900434e-05
   * - [8,]
     - 0.9813324
     - 3.164542e-05
     - 3.164542e-05
     - 3.164542e-05
   * - [9,]
     - 1.1215228
     - 3.406851e-05
     - 3.406851e-05
     - 3.406851e-05
   * - [10,]
     - 1.2617131
     - 3.627363e-05
     - 3.627363e-05
     - 3.627363e-05
   * - [11,]
     - 1.4019035
     - 3.826076e-05
     - 3.826076e-05
     - 3.826076e-05

There is no difference observed between the three compared variance
values of the gross count rate.

In the following table, for every value of ytilde, the following
calculated values are shown:

var_y_tilde_lin from Eq. :eq:`eq:variance_expression`;

Rg_tilde (from reversing Eq. (2),

var_Rg_tilde (the above mentioned var_rg_tilde_b) ,

varytilde2 (after inserting var_Rg_tilde into Eq. (3),

ratio (the ratio varytilde2 / vary_tilde_lin)

.. list-table::
   :header-rows: 1

   * - Dataset
     - y_tilde
     - vary_tilde_lin
     - Rg_tilde
     - var_Rg_tilde
     - varytilde2
     - ratio
   * - [1,]
     - 0.0000000
     - 0.02030666
     - 0.02723333
     - 8.580222e-06
     - 0.02030666
     - 1
   * - [2,]
     - 0.1401903
     - 0.02510862
     - 0.03130867
     - 1.252920e-05
     - 0.02510862
     - 1
   * - [3,]
     - 0.2803807
     - 0.02991058
     - 0.03538400
     - 1.626019e-05
     - 0.02991058
     - 1
   * - [4,]
     - 0.4205710
     - 0.03471254
     - 0.03945933
     - 1.977321e-05
     - 0.03471254
     - 1
   * - [5,]
     - 0.5607614
     - 0.03951451
     - 0.04353467
     - 2.306823e-05
     - 0.03951451
     - 1
   * - [6,]
     - 0.7009517
     - 0.04431647
     - 0.04761000
     - 2.614528e-05
     - 0.04431647
     - 1
   * - [7,]
     - 0.8411421
     - 0.04911843
     - 0.05168533
     - 2.900434e-05
     - 0.04911843
     - 1
   * - [8,]
     - 0.9813324
     - 0.05392039
     - 0.05576067
     - 3.164542e-05
     - 0.05392039
     - 1
   * - [9,]
     - 1.1215228
     - 0.05872235
     - 0.05983600
     - 3.406851e-05
     - 0.05872235
     - 1
   * - [10,]
     - 1.2617131
     - 0.06352432
     - 0.06391133
     - 3.627363e-05
     - 0.06352432
     - 1
   * - [11,]
     - 1.4019035
     - 0.06832628
     - 0.06798667
     - 3.826076e-05
     - 0.06832628
     - 1

This verifies the equivalence of the two compared interpolation methods.

Procedures with known random influences
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

.. math::
   u^{2}({\overline{n}}_{g}) = \frac{{\overline{n}}_{g} + \vartheta^{2}{\overline{n}}_{g}^{2}}{m_{g}}
   :label: eq:known_random_influences_1

.. math::
   u^{2}({\overline{n}}_{b}) = \frac{{\overline{n}}_{b} + \vartheta^{2}{\overline{n}}_{b}^{2}}{m_{b}}
   :label: eq:known_random_influences_2

.. math::
   u^{2}({\widetilde{n}}_{g}) = \frac{{\widetilde{n}}_{g} + \vartheta^{2}{\widetilde{n}}_{g}^{2}}{m_{g}}
   :label: eq:known_random_influences_3


With applying the tool for means (:ref:`using data sets for mean and variance`; see also :ref:`definitions`), the data
necessary for calculating :math:`\vartheta`, but also those data
referring to mean value-related datasets are available within the
program. Therefore, the formulae corresponding to the equations :eq:`eq:known_random_influences_1`, :eq:`eq:known_random_influences_2`
and :eq:`eq:known_random_influences_3` are easily programmed and are part of the program. This means,
in contrast to earlier UncertRadio versions, it is no longer necessary
for the user to enter uncertainty formulae with the :ref:`tab “values, uncertainties”` ;
the introduction of further auxiliary quantities also is
no longer necessary.

In addition, from the datasets supplied to UR, that one of them
representing the reference measurements from which :math:`\vartheta` has
to be derived, has to be identified. This can be done with a combobox
field within the dialog shown in chapter 6.9.2.

**Example-projects:** ISO-Example-2b_EN.txp, Mean-theta_EN.txp (with the
old UR treatment)

ISO-Example-2b_V2_EN.txp (with the new UR treatment)