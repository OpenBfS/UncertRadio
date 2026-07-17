TAB “Results”
^^^^^^^^^^^^^

Under this TAB selected by mouse click the total result for the output
quantity is shown including further variables and the values of the
Decision threshold and the Detection limit. The output quantity is
indicated which this result is referring to.

These are in detail:

**the result of the measurement:**

-  the value of the output quantity

-  the expanded uncertainty, in the same unit as that of the output quantity

-  the relative expanded uncertainty (in %)

-  the coverage factor (can be modified in the **menu Options**)

:ref:`best estimates according to Bayes and confidence limits
<best estimates and confidence limits>`

**(see also ISO 11929:2019):**

-  the value of the output quantity

-  the expanded uncertainty

-  the value of the lower confidence limit

-  the value of the upper confidence limit

-  probability :math:`(1 - \gamma)` associated with the confidence interval

..

   The toggle button „min. Coverage interval“ can be used to switch the
   display between probabilistically symmetric and the shortest coverage
   intervals, also in the case of the MC-Simulation.

**Decision threshold and Detection limit:**

-  the value of the Decision threshold including the number of
   iterations (actually no iterations)

-  the value of the Detection limit including the number of iterations

-  the applied quantiles of the normal distribution,
   :math:`k_{1 - \alpha}` and k_beta = :math:`k_{1 - \beta}`,
   corresponding to the errors of first and second kind

**WLS, PLSQ, PMLE or WTLS: Standard uncertainties of the fitting
parameter corresponding to the output quantity from the analysis of
decay curve:**

-  the uncertainty obtained from the least squares analysis; it is
   **NOT** multiplied with :math:`\sqrt{\chi_{R}^{2}}` if the reduced
   Chi-squared value is larger than 1; this variant of the
   uncertainty of the net counting rate is used for estimating the
   uncertainty of the output quantity;

-  that value of the uncertainty of the output quantity which is
   obtained from uncertainty propagation of the arguments of the
   Linfit function (i.e., mainly the background counting rate, if
   applicable with blank contribution) and of the uncertainties of
   the gross counting rates of the decay curve

-  the value of the reduced Chi-square :math:`\chi_{R}^{2}`

A :ref:`monte carlo simulation` may be started as
a modern alternative to the propagation of uncertainties:

-  input of the number *N* of simulated calculations of the output
   quantity (defining one run)

-  input of the number of runs *r*

-  Optional: selection of the coverage interval of shortest length
   (shortest **c**\ overage **i**\ nterval)

The MC simulation is started by clicking the button “Start”. The
iteration number is indicated when iteratively estimating the detection
limit.

From the r-fold repetition (runs) means and relative standard deviations
(in %) are determined for:

*Best estimates according to Bayes:*

-  the output quantity

-  the expanded uncertainty

-  the relative expanded uncertainty (%)

-  the lower confidence limit

-  the upper confidence limit

*and:*

-  the Decision threshold

-  the Detection limit

A **new Button "Save values"** was introduced. It can be used to
transfer all the values being visible in the dialog, including those
obtained by MC simulation, together with project name, date/time of
execution into a CSV file: UR‑Saved-Results.csv. If this file does not
yet exist, it is opened; then, records of data are appended to that
file. The meaning of the columns is similar to those of the file
AutoReport-Result.csv; however, there are further columns for the LINFIT
parameters, for each of the output quantity, the decision threshold and
the detection.

**In an extra dialog the three distributions are displayed which have
been obtained from the Monte Carlo simulation.**

In the **menu Options** one can define the two values of the **normal
Quantiles** corresponding to the errors of first and second kind,
respectively. See also :ref:`changes within the options menu`.
