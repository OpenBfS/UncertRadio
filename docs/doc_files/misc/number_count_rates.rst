Treatment of numbers of counts and count rates
----------------------------------------------

The feature for non-normal distributed numbers of counts and count rates
to be described now refers to Monte Carlo simulations according to ISO
11929-2019, part 2. According to part 1 of ISO 11929:2019, the input
quantities in any case are assumed as normal-distributed or are
attributed to this distribution by the principle of maximum entropy.

According to the GUM Supplement 1 (JCGM 101:2008), clause 6.4.11, for
counted events, which are Poisson distributed and represent an input
quantity :math:`X`, e.g., counted photons, the following step is
recommended to be taken for determining the distribution of the input
quantity. If :math:`\mathbf{q}` **events** are counted, **a Gamma
distribution is assigned to the posterior of the quantity**
:math:`\mathbf{X}` by applying the Bayes theorem and using a constant
prior. This is to be used as the distribution associated with :math:`X`:

.. math:: g_{X}(\xi) = \frac{\xi^{q}e^{- \xi}}{q!} \equiv Ga\left( \xi|q + 1,1 \right) \text{  for }  \xi \geq 0
   :label: number_count_rates_eq1


Mean and variance are :math:`E\lbrack X\rbrack = q + 1` and
:math:`Var\lbrack X\rbrack = q + 1`, respectively. This refers to
numbers of counts.

For Poisson-distributed numbers of counts, a Gamma distribution is
assigned In ISO 11929-2019 to the associated count rate :math:`\rho`,
where a prior :math:`\rho^{- 1}` is used instead of a constant prior.
Mean and variance are in this case given as :math:`q/t` und
:math:`q/t^{2}`, respectively.

In UncertRadio, the described step is treated as follows. As already
given by Eq. :eq:`number_count_rates_eq1`, the Gamma distribution is assigned to the number
:math:`n` of counts by selecting the distribution type „(N+x) rule“ for
:math:`n` (this corresponds to a prior :math:`\rho^{- 1}`). By
calculating the corresponding count rate :math:`R`, which requires an
equation like :math:`R = n/t`, the count rate is also Gamma distributed.
This also means, that a count rate to be treated in UncertRadio as Gamma
distributed, always requires defining it by an equation like
:math:`R = n/t`.

When measuring an activity, two variants are to be considered,

-  a measurement with pre-selected counting duration (the registered
   number :math:`n` is randomly distributed, following a **Poisson
   distribution**), and

-  a measurement with pre-selected numbers of counts (the counting
   duration :math:`t` is randomly distributed, following an **Erlang
   distribution**).

The Erlang distribution is addressed in the textbook by Knoll (Knoll,
G.F., Radiation Detection and Measurement, 2nd edition, (John Wiley,
NewYork,1989), pp. 96-99);

See also:

- International Safety Research, Safety Support Series, 2013. Radiation Counting Statistics. Volume 1. Canada.

- `Pengra, D., 2008 <http://courses.washington.edu/phys433/muon_counting/counting_stats_tutorial_b.pdf>`_

- `Pishro-Nik, H., Introduction to Probability <https://www.probabilitycourse.com/chapter11/11_1_2_basic_concepts_of_the_poisson_process.php>`_


**Comparing Erlang and Poisson distributions**

The two distributions are defined as follows, with :math:`\rho`
designating the count rate parameter:

**Poisson distribution:**

.. math::
    P_{Poi}(n) = \frac{(\rho t)^{n}e^{- \rho\ t}}{n!}
    :label: poi-def

.. math:: E\left\lbrack P_{poi} \right\rbrack = \ Var\left\lbrack P_{poi} \right\rbrack = \rho t

**Erlang distribution:**

.. math::
    P_{Erl}(t) = \frac{\rho^{n}t^{n - 1}e^{- \rho\ t}}{(n - 1)!}\left\lbrack \equiv Ga\left( t|n,\rho \right) \right\rbrack
    :label: erl-def

.. math:: E\left\lbrack P_{Erl} \right\rbrack = n/\rho;\ \ \ \ \ \ \ \ \ Var\left\lbrack P_{poi} \right\rbrack = n/\rho^{2}

The Erlang distribution is a Gamma distribution for integer-valued
:math:`n`. The two formulae (:eq:`poi-def` and :eq:`erl-def`) lead to a simple relation:

.. math::
    {t\ P}_{Erl}\left( t|\rho,n \right) = n\ P_{poi}\left( n|\rho,t \right)
    :label: relation

Applying the Bayes theorem with a prior :math:`\rho^{- 1}` to both
distributions results in the same posterior distribution for the count
rate :math:`\rho`, a Gamma distribution:

**Measurement with pre-set time:**

.. math::
    \frac{P_{Poi}\left( n|\rho,t \right)\rho^{- 1}}{\int_{}^{}{P_{Poi}\left( n|\rho,t \right)\rho^{- 1}d\rho}} = \frac{P_{Poi}\left( n|\rho,t \right)\rho^{- 1}}{1/n} = \frac{n(\rho t)^{n}e^{- \rho\ t}\rho^{- 1}}{n!} = \frac{t^{n}{\rho^{n - 1}e}^{- \rho\ t}}{(n - 1)!} = Ga\left( \rho|n,t \right)
    :label: measurement-time

**Measurement with pre-set counts:**

.. math::
    \frac{P_{Erl}\left( t|\rho,n \right)\rho^{- 1}}{\int_{}^{}{P_{Erl}\left( t|\rho,n \right)\rho^{- 1}d\rho}} = \frac{P_{Erl}\left( t|\rho,n \right)\rho^{- 1}}{1/t} = \frac{t\rho^{n}t^{n - 1}e^{- \rho\ t}\rho^{- 1}}{(n - 1)!} = \frac{t^{n}{\rho^{n - 1}e}^{- \rho\ t}}{(n - 1)!} = Ga\left( \rho|n,t \right)
    :label: measurement-counts

By equating the second parts of the two equations :eq:`measurement-time` and :eq:`measurement-counts`, the
simple relation of :eq:`relation` is obtained again:

.. math:: \frac{P_{Poi}\left( n|\rho,t \right)\rho^{- 1}}{1/n} = \frac{P_{Erl}\left( t|\rho,n \right)\rho^{- 1}}{1/t}

or

.. math::
    t\ P_{Erl}\left( t|\rho,n \right) = n\ P_{Poi}\left( n|\rho,t \right)
    :label: relation2

If another prior is used for the Poisson distribution,
:math:`\rho^{- 1/2}`, again a Gamma distribution is obtained, but a
different one: :math:`Ga\left( \rho|n + 1/2,t \right)`.

**In the case of pre-set counts** (math:`t`
variable), **the Erlang distribution must be assigned to** :math:`t` by
selecting the distribution type “Npreset“ **for** :math:`t`. By an also
required equation like :math:`R = n/t`, the Gamma
distribution\ :math:`\ Ga\left( \rho|n,t \right)` is thereby internally
assigned to the count rate :math:`R`.

Example project: **PresetCounts_EN.txp**