Best estimates and confidence limits
------------------------------------

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

:math:`u\left( \widehat{y} \right) = \sqrt{u^{2}(y) - \left( \widehat{y} - y \right) \cdot \widehat{y}}`

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

.. code-block:: fortran

    imax = gamma*N
    min_length = 1E+20
    do i=1,imax
        q_left = y(i)
        q_right = y(int(N * (1 - i/N)))
        if (q_right - q_left < min_length) then
            min_length = q_right - q_left
            q_left_min = q_left
            q_right_min = q_right
        endif
    enddo
