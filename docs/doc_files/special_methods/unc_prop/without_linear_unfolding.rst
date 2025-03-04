Methods without linear unfolding
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An equation, or a system of several equations describing the evaluation
of the measurement quantity, is called **evaluation model**. It is
stated in the form:

.. math:: Y = G\left( X_{1},\ldots,\ X_{m} \right)
    :label: wo_unfolding_eq_1

Inserting estimates for the input quantities yields an estimate of the
output quantity, the **primary measurement result** **y**:

.. math:: Y = G\left( x_{1},\ldots,\ x_{m} \right)
    :label: wo_unfolding_eq_2

The standard uncertainty :math:`u(y)` associated with the primary
measurement result :math:`y` is calculated according to the following
equation, assuming that measured values of the input quantities are
statistically independent:

.. math:: u^{2}(y) = \sum_{i = 1}^{m}\left( \frac{\partial G}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right)
    :label: wo_unfolding_eq_3

This is known as **uncertainty propagation**. The partial derivatives
are also known as **sensitivity coefficients**. It is often written such
that *G* is replaced by *Y*:

.. math:: u^{2}(y) = \sum_{i = 1}^{m}\left( \frac{\partial Y}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right)
    :label: wo_unfolding_eq_4

If the input quantities have been measured in a way that they are not
statistically independent, associated covariances have to be taken into
account. Then, the extended previous equation reads:

.. math:: u^{2}(y) = \sum_{i = 1}^{m}\left( \frac{\partial G}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right) + 2\sum_{i = 1}^{m - 1}{\sum_{j = i + 1}^{m}{\frac{\partial G}{\partial x_{i}}\frac{\partial G}{\partial x_{j}}}u\left( x_{i},x_{j} \right)}
    :label: wo_unfolding_eq_5


Here, :math:`u\left( x_{i},x_{j} \right)` represents a more general way
of stating a covariance between two measured input quantities, which
often are also given as :math:`cov\left( x_{i},x_{j} \right)` or
:math:`covar\left( x_{i},x_{j} \right)`, respectively. The following
holds: :math:`u\left( x_{i},x_{i} \right) = u^{2}\left( x_{i} \right)`
and :math:`u\left( x_{i},x_{j} \right) = u\left( x_{j},x_{i} \right)`.

Equation :eq:`wo_unfolding_eq_3` is the basic form of the uncertainty propagation used in UR
for methods not requiring linear unfolding. This is done by using the
values and standard uncertainties defined under the :ref:`tab “values, uncertainties”`.
In case that covariances have also been declared in this
TAB, Equation :eq:`wo_unfolding_eq_5` is applied.