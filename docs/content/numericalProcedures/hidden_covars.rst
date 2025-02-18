Preventing "hidden" covariances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the following, it is assumed now that the arithmetic expression for
the output quantity :math:`y` containing several expressions, e.g.,
:math:`{\ a}_{1},\ a_{2},a_{3}`, each of which being functions of input
quantities :math:`x_{i}`. Often, the uncertainties
:math:`{u(a}_{1}),\ \ u(a_{2}),{\ u(a}_{3})` are calculated first from
which then :math:`u(y\left( {\ a}_{1},\ a_{2},a_{3} \right))` is
derived. If, however, there are some of the input quantities
:math:`x_{i}`, contained in more than one of the expressions
:math:`{\ a}_{1},\ a_{2},a_{3}`, then "hidden" or "overlooked"
covariances exist between some of the :math:`{\ a}_{1},\ a_{2},a_{3}`,
which would have to be considered afterwards.

This problem does not occur during the uncertainty calculations within
UncertRadio, because there the partial derivatives in its uncertainty
propagation are always build from the equation of the output quantity.
It is shown below, why this avoids the above problem.

The ansatz for the uncertainty propagation with partial derivatives
which refer to the output quantity
:math:`y = y({\ a}_{1}(\mathbf{x}),\ a_{2}(\mathbf{x}),a_{3}(\mathbf{x}))`,
is formulated as follows with the vector :math:`\mathbf{x}` of input
quantities:

At first, the square within the sum is evaluated:

.. math::
    u^{2}(y) = \sum_{i = 1}^{ni}{\left( \frac{\partial y}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right)}

.. math::
    u^{2}(y) = \sum_{i = 1}^{ni}{\left( \sum_{j = 1}^{3}{\frac{\partial y}{\partial a_{j}}\frac{\partial a_{j}}{\partial x_{i}}} \right)^{2}u^{2}\left( x_{i} \right)}

.. math::
    \sum_{i = 1}^{ni}{\left( \frac{\partial y}{\partial a_{1}}\frac{\partial a_{1}}{\partial x_{i}} + \frac{\partial y}{\partial a_{2}}\frac{\partial a_{2}}{\partial x_{i}} + \frac{\partial y}{\partial a_{3}}\frac{\partial a_{3}}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right)}


Now, the summation over :math:`i` is performed for each of the six
terms, while at the same time the partial derivatives of :math:`y` by
:math:`a_{j}` are factored out of the sums:

.. math::
    :nowrap:

    \begin{eqnarray}
        u^{2}(y) = \sum_{i = 1}^{n_i} & & \left( \frac{\partial y}{\partial a_{1}}\frac{\partial a_{1}}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right) + \\
        && \left( \frac{\partial y}{\partial a_{2}}\frac{\partial a_{2}}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right) + \\
        && \left( \frac{\partial y}{\partial a_{3}}\frac{\partial a_{3}}{\partial x_{i}} \right)^{2}u^{2}\left( x_{i} \right) + \\

        && 2\left( \frac{\partial y}{\partial a_{1}}\frac{\partial a_{1}}{\partial x_{i}} \right)\left( \frac{\partial y}{\partial a_{2}}\frac{\partial a_{2}}{\partial x_{i}} \right)u^{2}\left( x_{i} \right) + \\
        && 2\left( \frac{\partial y}{\partial a_{1}}\frac{\partial a_{1}}{\partial x_{i}} \right)\left( \frac{\partial y}{\partial a_{3}}\frac{\partial a_{3}}{\partial x_{i}} \right)u^{2}\left( x_{i} \right) + \\
        && 2\left( \frac{\partial y}{\partial a_{2}}\frac{\partial a_{2}}{\partial x_{i}} \right)\left( \frac{\partial y}{\partial a_{3}}\frac{\partial a_{3}}{\partial x_{i}} \right)u^{2}\left( x_{i} \right)
    \end{eqnarray}



Now, each individual sum over :math:`i` is representing a variance or a
covariance of the expressions :math:`{\ a}_{1},\ a_{2},a_{3}`:

.. math::

    u^{2}(y) = &\left( \frac{\partial y}{\partial a_{1}} \right)^{2}u^{2}\left( a_{1} \right) +
               \left( \frac{\partial y}{\partial a_{2}} \right)^{2}u^{2}\left( a_{2} \right) +
               \left( \frac{\partial y}{\partial a_{3}} \right)^{2}u^{2}\left( a_{3} \right) + \\
               &2\frac{\partial y}{\partial a_{1}}\frac{\partial y}{\partial a_{2}} cov(a_{1},a_{2}) +
                2\frac{\partial y}{\partial a_{1}}\frac{\partial y}{\partial a_{3}}cov(a_{1},a_{3}) + \\
               & 2\frac{\partial y}{\partial a_{2}}\frac{\partial y}{\partial a_{3}}cov(a_{2},a_{3})

Usually, a "hand-made" uncertainty propagation by first applying a
decomposition of :math:`y` into expressions or functions :math:`a_{j}`,
only the first three terms in Eq. (6) are used, because covariances
between the :math:`a_{j}` often are not expected; this may explain the
term "hidden" covariances.

The result of Eq. (6) is just the one which has to be expected when
"hidden" covariances between the :math:`a_{j}` are explicitly taken into
account. This demonstrates that these covariances are considered by
UncertRadio, automatically, only because it uses within its uncertainty
evaluation according to Eq. (1), partial derivatives directly of the
output quantity.