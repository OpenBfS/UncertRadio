Linear Least squares method
---------------------------

**Principle of the multi-linear least squares fitting**

**Model**

For the use of the (multi-) linear least squares fitting (LSQ analysis)
the following model of a decay curve is assumed:

.. math:: Y\left( t_{k} \right) = a_{1} \cdot X_{1}\left( t_{k} \right) + \ a_{2} \cdot X_{2}\left( t_{k} \right) + \ a_{3} \cdot X_{3}\left( t_{k} \right)

A sum of up to three terms being dependent on the counting time
:math:`t_{k}` can be fitted to measured values of the quantity *Y* being
also dependent on :math:`t_{k}`; *k* counts the measurements. The
:math:`a_{i}` are the coefficients which are to be determined by fitting
(fitting parameters). The quantities :math:`X_{i}\left( t_{k} \right)`
are functions (decay curves) which are considered to be known and which
depend only on :math:`t_{k}` and other parameters, e.g., half-lives,
which are not considered as fit parameters. They must not be dependent
on the fitting parameters :math:`a_{i}`. :math:`Y\left( t_{k} \right)`
is treated as dependent variable while the functions
:math:`X_{i}\left( t_{k} \right)` are treated as independent variables
in the LSQ analysis. For the moment it is assumed here that the
measurement of the values of\ :math:`Y\left( t_{k} \right)` is done with
a single-channel counter.

By the introduction of up to three output quantities to be treated
simultaneously, it is, for instance, possible that the simultaneous
measurement of several radionuclides is done by using a two- or
three-channel counter. Using a LSC counter with three counting channels
(energy regions) allows for instance to determine simultaneously the
activities of Sr-90, Sr-89 and Sr-85 (yield tracer).

If more than one output quantity is defined for a project, e.g., two or
three representing the activities of different radionuclides, the
program inserts three new symbols, Fitp1, Fitp2 and Fitp3, into the list
of symbols, which refer to the values of the :math:`a_{i}`. These names
are not allowed to be changed. From these, the user can derive with
further equations (in the text window under the TAB “Equations”) above
the *Linfit* call the decay corrected activity concentrations. Under the
TAB “Values, uncertainties” standard uncertainties obtained from the
linear curve fitting are assigned to these new symbols. Additionally,
their possible correlation pairs and associated correlation coefficients
are inserted into the corresponding table under the same TAB which
enables their whole covariance matrix of the three fitting parameters to
be used for the uncertainty propagation of the output quantities.

Note, that also in the case of only two output quantities all three
symbols, Fitp1, Fitp2 and Fitp3, are inserted into the symbol list.
Only, **if only one output quantity** is defined, **none** of these
parameters is inserted in the symbol list. In this case the value of the
fitting parameter is attributed directly the quantity *Rn* in the call
*Rn = Linfit(1,… )*; c.f. further down.

**Least squares routines used In UncertRadio**

Two different routines are used in the program for least squares
estimation. These are:

-  the **“simple“** least squares procedure (LSQ) which is usually used if
   the values :math:`X_{i}\left( t_{k} \right)` **do not** have
   uncertainties; the measured values
   :math:`Y\left( t_{k} \right)`, however, have uncertainties,
   covariances between them are also taken into account. If the
   values :math:`X_{i}\left( t_{k} \right)` nevertheless have
   uncertainties, they are included by UncertRadio within the
   uncertainty propagation outside the LLSQ routine;

-  In autumn 2013, three options for selecting a fitting procedure were
   introduced, which differ in their associated Chi-square
   expressions:

-  **WLS**: Using the **Neyman Chi-square**; this procedure is identical
   to the previous procedure NLSQ; linear, without iterations;

..

   **PLSQ**: Using the **Pearson Chi-square**; linear / iteratively;

   **PMLE**: Poisson Maximum Likelihood Estimation (Poisson MLE);
   non-linear / iteratively.

-  the **“general case“** of least squares (WTLS, weighted total least
   squares), which in addition considers uncertainties of the
   :math:`X_{i}\left( t_{k} \right)` and possible covariances
   between them. Herein, an iterative, i.e., non-linear, matrix
   procedure is used, which is a more time-consuming method because
   of the iterations. The possible covariances between the
   :math:`X_{i}\left( t_{k} \right)` values are determined by the
   program internally by applying partial derivatives with respect to
   the symbols contained in the :math:`X_{i}\left( t_{k} \right)`
   functions; they need not to be supplied by the user.

For the background information of these fitting methods see section :ref:`chi-square options`.

By default, the “simpler” WLS procedure is invoked by the call *Rn =
Linfit(1,… )*. The use of the WTLS procedure may be selected within the
dialog Definition of the decay-curve model.

**Notes:**

   **Correlations between measured values**
   :math:`Y\left( t_{k} \right)` **may occur.** This for instance is the
   case if the measured values :math:`Y\left( t_{k} \right)` are net
   counting rates for which the same value of a background counting rate
   :math:`R_{0}` as well as the net blank counting rate :math:`R_{bl}`
   have been subtracted from the corresponding gross counting rates.
   These covariances - they can be calculated internally from a formula
   - need to be considered especially in the case of quite low net
   counting rates.

   In the case of the net counting rates (:math:`Y\left( t_{k} \right)`)
   the covariance formula between two of its values is given by:

   :math:`cov\left( Y\left( t_{i} \right),\ \ \ Y\left( t_{k} \right) \right) = var\left( R_{0,i} \right) + var\left( R_{bl} \right)`
   .

   This is internally used in the program to derive the complete
   covariance matrix to be used by the fitting routine. In this
   equation, :math:`var\left( R_{0,i} \right)` must be set equal to zero
   if either :math:`R_{0,i}` and :math:`R_{0,k}` are different or their
   uncertainties.

   It has to be mentioned here that for this method the background
   counting rate can be defined in the special :ref:`input dialog for decay
   curves <dialog definition of the decay curve model>` counting rate of the curve
   an individual background counting rate may
   be given – this may occur with LSC measurements –, or one single
   value of the background counting rate (:math:`R_{0}`) may be used for
   the whole curve. :math:`R_{0}` must contain only the detector-related
   background component.

   Furthermore, a counting rate :math:`R_{bl}` is required which refers
   to a chemical blank analysis where the detector background component
   is already subtracted. Therefore, :math:`R_{bl}` is considered as a
   „net blank count rate“; it quantifies the background due to chemicals
   and glassware used during an analysis. The symbol *Rbl*, with value
   and uncertainty, is part of the table under the TAB “Values,
   uncertainties”, the values of which are transferred by the Linfit
   Call (c.f. further down in this theme) to this numeric routine.

For the mathematics see:

a) :ref:`linear curve fitting with
   WLS <mathematics of the linear lsq curve fitting with correlated measured values>`

b) :ref:`linear curve fitting with WTLS
   <notes on linear curve-fitting using general least squares (wtls)>`

Definition of the model: see :ref:`dialog definition of the decay curve model`.

After having defined the Linfit-Call within the equations, the use of
the WTLS procedure may be selected in this dialog.

**Activating the (multi-) linear Least squares fitting**

This is shown for two examples of different complexity.

1) **Simple example:**

*Assumtion*:

Number of output quantities: 1; from the LSQ fitting a net counting rate
*Rn* is obtained.

Within the text field for equations at the location where otherwise the
net counting rate *Rn* is defined, for instance

*Rn = Rg – R0 ,*

this equation is replaced by the following:

*Rn = Linfit(1, Rbl, HwzY90, Hwzlong, HwzAc228, tmess, tstart)*

**Linfit** is the name of the procedure which initiates the LSQ fitting
with its associated sub-dialogs. Its parameters are:

   *1*   No. of the variant of this measurement evaluation task for which
   this type of fitting shall be used; at present not more than the
   present variant of the evaluation of an Y-90 decay curve analysis
   exists;

   *R0*   background counting rate including also blank contributions
   which is subtracted from the measured Y-90 gross counting rates, in
   :math:`s^{- 1}`;

   *t0*   counting time of the background measurement, in :math:`s`;

   *HwzY90*   half-live of Y-90, in :math:`s`

   *Hwzlong*   half-live of a longer-lived radionuclide contributing to a
   (slowly decaying) background, in :math:`s`; e.g. Th-234; if Hwzlong =
   0 is set the associated decay factor is set internally equal to 1

   *HwzAc228*   half-live of the possibly interfering radionuclide Ac-228,
   in :math:`s`\ *;* this cal also simulated a contribution of
   short-lived radon decay products

   *tmess*   place holder for the counting times of the individual
   counting times belonging to the net counting rates

   *tstart*   place holder for the periods of time between the time of the
   Y-90/Sr-90 separation and the starting time of the individual
   measurements

**Note:** Since version 2.4.24, only the three parameters *Rbl, tmess*
and *tstart* shall be given in the Linfit call: *Rn = Linfit(1, Rbl,
tmess, tstart)*

After loading the symbols from the equations including that Linfit-call
described above the symbols from this routine are available in the
common list of symbols. In the TAB “Values, uncertainties” values and
uncertainties of the symbols *R0, t0, HwzY90, Hwzlong* and *HwzAc228*
have to be entered then, however, not for *tmess* and *tstart*.

With one exception, of course, one may use other Linfit symbols instead
of those shown above, they only need to be given in the total symbol
list; these symbols are to be considered “globally” valid.

Important: Only the symbol names *Rbl*, *tmess* and *tstart* must not be
changed, which is also true for their meaning as defined above.

After the call to Linfit, the value of the fitting parameter
:math:`a_{1}` and its uncertainty have been transferred to those of the
symbol *Rn*.

2) **More complex example:**

*Assumtion*:

Number of output quantities: 3; as a result from the LSQ fitting one
obtains the fitting parameters Fitp1, Fitp2 and Fitp3, corresponding to
the parameters :math:`a_{i}`, which now represent the activities (in Bq) of
Sr-89, Sr-90 and Sr-85. This is an example taken from the example
project DWD-LSC-3kanal-V2.txp.

Within the text field for equations at the location where otherwise the
net counting rate *Rn* is defined, for instance

*Rn = Rg – R0,*

this equation is replaced by the following:

*rd = Linfit(1, Rbl, eSr85A, eSr85B, eSr85C, eSr90A, eSr90B, eSr90C,
eSr89A, eSr89B, &*

*eSr89C, eY90A, eY90B, eY90C, lamSr85, lamSr90, lamSr89, lamY90, tmess,
tstart )*

**since version 2.4.24** *this is shortened to:* *Rn = Linfit(1, Rbl,
tmess, tstart).*

The meaning of the symbols is equivalent to those in the „simple
example” given above. The symbol names *Rbl*, *tmess* and *tstart* as
well as their associated meaning must not be changed. The symbols
*eNuklidX* (in total 9) designate detection efficiencies of the
different radionuclides for the counting channels A, B or C. The
ssymbols *lamNuklid* represent the decay constants of the three
radionuclides.

.. note::

    Apart from the fixed symbols *Rbl*, *tmess* and *tstart,* which have to
    appear in the Linfit call, neither the names of other symbols are fixed
    nor their number; however, they must appear in the whole (global) symbol
    list. They must be used in the equations defining the functions
    :math:`X_{i}\left( t_{k} \right)`; for more information, see also
    `Dialog Definition of the decay-curve
    model <#dialog-definition-of-the-decay-curve-model>`__.

For Input of data from the decay curve see: :ref:`dialog “values of decay curve”`.

:ref:`viewing the result from the lsq fit to the decay curve`

:ref:`note on decision threshold and detection limit for linear fitting`
