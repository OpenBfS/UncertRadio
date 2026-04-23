Introduction
------------

**UncertRadio** allows for **the evaluation of a
measurement in the field of measuring activity** by inserting defining
equations the **full calculation of the output quantity and its combined
uncertainty (according to ISO GUM)** and to calculate the **values of
the Decision threshold and the Detection limit (according to ISO
11929:2019)**, which are closely related to uncertainty.

The program assumes the **ISO GUM interpretation which is based on the
Bayesian theory of the measurement uncertainty (Bayesian statistics)**.
The Bayesian measurement uncertainty does not have a statistical
uncertainty, nor does it consider degrees of freedom, which makes it
different from conventional (frequentist) statistics. Therefore, degrees
of freedom need not to be treated in the program.

One restriction with respect to its usage referred to the circumstance
that only those measurement problems could be treated in which the
**measurements** of counts or of counting rates **are terminated if the
counting time reaches its pre-set value**. The statistically different
case where the measurement is terminated by reaching a pre-set value of
counts, where the counting time is a random variable, is also treated by
the program, since program version 2.4.04
(see :ref:`treatment of numbers of counts and count rates`).

The present state of the program allows calculations **for up to three
output quantities**.

The sequence of steps being treated is:

1. Input of a short text description of the measurement problem.

2. **Input** of the **equations** defining the **output quantity** `y` of the
   measurement procedure; these define the **"evaluation model"**;
   the first of the equations must define the output quantity/quantities.

   Note: If more than one output quantity is to be treated, the calculation
   of the values of output quantity, uncertainty, uncertainty budget,
   Decision threshold and Detection limit, respectively, refers only to a
   single output quantity.
   By starting with a project, by default the first of the output quantities
   is "activated"; at a later stage another one can be selected in the main
   menu to be the "active one".

3. Automatic **extraction** of the **formula symbols** and **input of their**
   meaning (unit, meaning); automatic classification as independent and
   dependent symbols; manual addition of other symbols, not used explicitly
   in the equations.

4. Selection of symbols which define the **net counting rate (Rn)** and the
   **gross counting rate**, only for the purpose of the calculation of
   Decision threshold and Detection limit; the net counting rate in this
   case must be the "**procedure dependent net counting rate**" in which
   the counting rate contribution due to interference from other
   radionuclides, usually obtained by calculation, is taken into account.

5. **Input of measured values and their associated standard uncertainties**
   of input quantities (independent symbols) in table form.

6. For the **input of measurement uncertainties**, their **associated distribution**
   can be chosen from: a) **normal distribution**, b) **uniform distribution**,
   c) **triangular distribution** and d) **gamma distribution**, as well as a few
   others. Half-widths of the latter two are converted by the program according
   to ISO GUM to standard uncertainties; for a complete list see
   :ref:`other distributions <special distributions and their properties>`.


   Note: In the case of low-level applications with very low count numbers,
   :ref:`the so-called "(N+x) rule <low-level applications, (n+x)-rule>` may be
   selected to improve the results, from which the associated counting rate
   variables are considered as being Gamma distributed.

7. For the input one can choose between
   **absolute and relative uncertainties**.

8.  For counting rates or the number of counts
    **uncertainties can also be defined by formulae**.

9.  The standard deviation of the gross count rate must be defined by a
    formula; this is the **"uncertainty function" (standard uncertainty) of
    the gross counting rate**,
    which is the basis for deriving values of Decision threshold
    and Detection limit.

10. **Input of covariances between input quantities** which can be given
    as formulae or as values of correlation coefficients in tabular form.

11. Numerical calculation of values and combined standard uncertainties for the
    quantities classified as dependent quantities and for the output
    quantity and of the **uncertainty budget**; consideration of
    covariances/correlations between input
    quantities is possible; calculation of the
    :ref:`best estimates and confidence limits based on a Bayesian method
    <best estimates and confidence limits>`
    characterizing the value and standard uncertainty of the output quantity.

12. Iterative numerical calculation of
    **Decision threshold and Detection limit**
    for the output quantity based on the numerical evaluation of its
    combined uncertainty taking
    covariances into account.

13. A :ref:`Monte Carlo simulation <monte carlo simulation>` can be started for an
    examination of the above-mentioned calculations. This allows an independent
    calculation of the value of the output quantity and its combined uncertainty.
    Partial derivations are not necessary in this case: for every input quantity
    classified as independent, a value is drawn from its associated distribution
    from which a value of the output quantity is calculated using the defined
    equations. From the many-fold repetition of this step, a statistical
    distribution is obtained for the output quantity. This is used to derive the
    "best estimate" from its mean and the combined uncertainty from its standard
    deviation. Confidence limits are estimated as Quantiles of that distribution,
    whereas Decision threshold and Detection limits require creating separate MC
    distributions with modified values of the output quantity and their estimation
    by corresponding Quantiles.

14. Finally, a **complete report can be created as a text file** containing
    all the equations, input values, uncertainty budget table and the
    final results - including those from the Monte Carlo simulation,
    the PDF file also contains the MC graphs.

**UncertRadio is well suited for calculation comparison** for such
solutions, which one may have already developed with spreadsheet
calculations. The latter may get quite complex and sometimes are not so
easily manageable especially with respect to the correct uncertainty
propagation.
