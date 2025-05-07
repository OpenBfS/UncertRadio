Application of decay series
---------------------------

Basic procedure
~~~~~~~~~~~~~~~

The time behaviour of atom numbers :math:`N_{i}(t)` of the radionuclides
(members) of a decay chain is in principle modelled by a system of first
order differential equations. They are called the Bateman equations
after H. Bateman’s publication of 1910. Their solution led to atom
numbers :math:`N_{i}(t > 0)`, which by radioactive decay can be expected
after a time duration :math:`t` from the atom number
:math:`N_{i}(t = 0)` at :math:`t = 0`.

More recent work on improved solutions for such a system applies
matrix-based methods, which are applicable in an automated way also for
more complex decay chains. The method published by E. Levy in 2019 was
chosen for UncertRadio, more specifically the :math:`\mathbf{F}`-Matrix
introduced in chapter 4 of his publication. It was found that the result
of a series expansion of an exponential function of a decay matrix as
used before lead to a single matrix, the :math:`\mathbf{F}`-Matrix. In
environmental radioactivity mainly the activities of radionuclides are
of interest instead of their atom numbers. The general relation
:math:`A_{i} = \lambda_{i} \cdot N_{i}` between activity and atom
numbers allows to derive an :math:`\mathbf{F}`-Matrix for activities.
The rank of the matrix is equal to the number of members of decay chain,
denoted as :math:`L.` In his chapter 4, Levy derived an algorithm for
calculating the elements of the matrix :math:`\mathbf{F}`, which is of
left-triangular shape. An important feature of :math:`\mathbf{F}` is
that it holds only for the given time :math:`t`; for any other time
value, :math:`\mathbf{F}` always has to be re-computed.

For calculating the elements of the :math:`\mathbf{F}\ m`\ atrix only
the decay constants :math:`\lambda_{i}` and the values of branching
ratios :math:`z_{j,i}` (mit :math:`j < i`) of the radionuclides are
required. The latter are stored in a separate matrix of rank :math:`L`.
After initialisation of this matrix by zero, only those elements
:math:`(i,j)` are set for which :math:`z_{j,i} > 0` holds. For a decay
chain of length :math:`L` and without branching just :math:`L - 1`
values :math:`z_{j,i}` exist. The diagonal elements of the matrix
:math:`\mathbf{F}` contain decay factors
:math:`\exp\left( - \lambda_{i}t \right)` (without correcting decay
during the measurement duration :math:`t_{m}`) or function values
:math:`fd(t,tm,xlam)` (with correcting decay during the measurement; see
:numref:`text field for equations`).

For a vector :math:`\mathbf{a}_{\mathbf{0}}` of start activity values
given at :math:`t = 0`, the vector :math:`\mathbf{a}_{\mathbf{t}}` of
activities after the time duration :math:`t > 0` is calculated by matrix
multiplication:

.. math::
   \mathbf{a}_{\mathbf{t}} = \mathbf{F} \cdot \mathbf{a}_{\mathbf{0}}
  :label: eq_decay_chains_1

.. math::

   \begin{pmatrix}
   A_{1}(t) \\
   A_{2}(t) \\
   \begin{matrix}
    \vdots \\
   A_{n}(t)
   \end{matrix}
   \end{pmatrix} = \begin{pmatrix}
   f_{1,1} & 0 & 0 & 0 \\
   f_{2,1} & f_{2,2} & 0 & 0 \\
    \vdots & \vdots & \vdots & \vdots \\
   f_{n,1} & f_{n,2} & \cdots & f_{n,n}
   \end{pmatrix} \cdot \begin{pmatrix}
   A_{1}(0) \\
   A_{2}(0) \\
   \begin{matrix}
    \vdots \\
   A_{n}(0)
   \end{matrix}
   \end{pmatrix}

For the application in UR this is called "forward calculation". The
activity unit used in this equation must be Bq.

When specific activities are to be considered, it must be observed that
the associated calibration factors relating count rates to specific
activities must not contain such decay corrections which are already
part of :math:`\mathbf{F}`. If this condition is fulfilled, the matrix
equation given above can also be applied for specific activities. The
matrix :math:`\mathbf{F}` in that case remains the same.

Decay corrections
~~~~~~~~~~~~~~~~~

The vector :math:`\mathbf{a}_{\mathbf{t}}\ `\ is calculated from
:math:`\mathbf{a}_{\mathbf{0}}` by :eq:`eq_decay_chains_1`; this step was called
„forward calculation”. If the decay chain has only radionuclide decaying
to a stable isotope, the inversion of this step („backward
calculation“)is often termed „decay correction“: it means the ratio
:math:`a_{1}(0)/a_{1}(t)`. The activity determined at the time of
measurement is back calculated to the date of sampling.

For decay chains of two or more members, the backward calculation
becomes more complicated. As an example, consider the decay chain
Pb-210/Bi-210/Po-210. The non-trivial problem now consists in deriving
the Pb-210 activity at the time of sampling from measurements of the
later measurements of the decay products.

For the most elegant solution method of this task, the :eq:`eq_decay_chains_1` is
interpreted as a linear least squares problem. The matrix
:math:`\mathbf{F}` is considered as the design matrix and the vector
:math:`\mathbf{a}_{\mathbf{0}}` is treated as the desired solution
vector. The vector :math:`\mathbf{a}_{\mathbf{t}}` represents the
measurements. A covariance matrix
:math:`\mathbf{U}_{\mathbf{a}_{\mathbf{t}}}` can be established
associated with :math:`\mathbf{a}_{\mathbf{t}}`. We assume that this
matrix is diagonal.

Under these assumptions the least squares problem can be solved by the
following two equations (see CHAGR-ISO-01, section 4.2):

.. math::
   \mathbf{U}_{\mathbf{a}_{\mathbf{0}}} = \left( \mathbf{F}^{T} \cdot \mathbf{U}_{\mathbf{a}_{\mathbf{t}}}^{- \mathbf{1}}\mathbf{\cdot}\mathbf{F} \right)^{- 1}
  :label: eq_decay_chains_2

.. math::
   \mathbf{a}_{\mathbf{0}} = \mathbf{U}_{\mathbf{a}_{\mathbf{0}}} \cdot \mathbf{F}^{T} \cdot \mathbf{U}_{\mathbf{a}_{\mathbf{t}}}^{- \mathbf{1}} \cdot \mathbf{a}_{\mathbf{t}}
  :label: eq_decay_chains_3

The computation of these equations is easily done. The advantage of
these equations is that the vector :math:`\mathbf{a}_{\mathbf{0}}` and
the associated covariance matrix
:math:`\mathbf{U}_{\mathbf{a}_{\mathbf{0}}}` are practically obtained by
the same combined step.

Alternatively, a recursion procedure can be applied. According to the
literature [Blobel, Lohrmann, chapter 3.4, Press at al., 1992, section
2.3], the elements of the vector :math:`\mathbf{a}_{\mathbf{0}}` are
calculated by a recursive scheme („forward substitution“) from the left
triangular matrix (:math:`\mathbf{F}`). The scheme is described by the
following equations:

.. math::
   {k = 1:\ \ \ \ \ \ \ \ \ a}_{0;1} = \frac{a_{t;1}}{f_{1,1}}

.. math::
   k > 1:\ \ \ \ \ \ \ \ \  a_{0;k} = \frac{1}{f_{k,k}}\left\lbrack a_{t;k} - \sum_{j = 1}^{k - 1}\left( f_{k,j} \cdot a_{0;j} \right) \right\rbrack
  :label: eq_decay_chains_4

Before calculating the uncertainties, the components :math:`a_{0;j}` of
:math:`\mathbf{a}_{\mathbf{0}}` in the right-hand side of the equation
for :math:`k > 1` need to be substituted by corresponding components of
the vector :math:`\mathbf{a}_{\mathbf{t}}`.

.. math::
   a_{0;j} = A_{k}(0) = \sum_{i = 1}^{k}{d_{k,i} \cdot A_{i}(t)}
  :label: eq_decay_chains_5

The calculations according to :eq:`eq_decay_chains_5`,
which may become more tedious for
longer decay chains, are simplified by the LS procedure. The
coefficients :math:`d_{j,i}` formally define a matrix
:math:`\mathbf{D}`, which can be established by :eq:`eq_decay_chains_3`:

.. math::
   \mathbf{D} = \mathbf{U}_{\mathbf{a}_{\mathbf{0}}} \cdot \mathbf{F}^{T} \cdot \mathbf{U}_{\mathbf{a}_{\mathbf{t}}}^{- \mathbf{1}}
  :label: eq_decay_chains_6

The equations :eq:`eq_decay_chains_5` and :eq:`eq_decay_chains_6`
formulated for a 3-member decay chain are:

.. math::
   A_{1}(0) = d_{1,1} \cdot A_{1}(t)

.. math::
   A_{2}(0) = d_{2,1} \cdot A_{1}(t) + d_{2,2} \cdot A_{2}(t)
  :label: eq_decay_chains_7

.. math::
   A_{3}(0) = d_{3,1} \cdot A_{1}(t) + d_{3,2} \cdot A_{2}(t) + d_{3,3} \cdot A_{3}(t)

:eq:`eq_decay_chains_5` can be re-formulated as follows by tracing back the
uncertainties of :math:`A_{i}(t)` to the uncertainties of the input
quantities :math:`x_{j}`:

.. math::
   u^{2}\left( A_{k}(0) \right) = \sum_{i = 1}^{k}{d_{k,i}^{2} \cdot u^{2}\left( A_{i}(t) \right)}

.. math::
   u^{2}\left( A_{k}(0) \right) = \sum_{i = 1}^{k}{d_{k,i}^{2} \cdot \sum_{j}^{}\left( \frac{\partial A_{i}(t)}{{\partial x}_{j}} \right)^{2}u^{2}\left( x_{j}(t) \right)}

.. math::
   u^{2}\left( A_{k}(0) \right) = \sum_{j}^{}{u^{2}\left( x_{j}(t) \right)}\sum_{i = 1}^{k}\left( d_{k,i} \cdot \frac{\partial A_{i}(t)}{{\partial x}_{j}} \right)^{2}
  :label: eq_decay_chains_8

This means, the sensitivity coefficients of the uncertainty propagation
are multiplied by factors :math:`d_{k,i}` multipliziert. Dependent on
the size of :math:`d_{k,i}`, these factors have the potential to raise
the uncertainties of :math:`A_{k}(0)`. For the example of the decay
chain Pb-210/Bi-210/Po-210, this effect increases by increasing the time
interval :math:`t` relative to the half-live of Bi-210.

UncertRadio contains the code for calculating :math:`\mathbf{F}` and the
:eq:`eq_decay_chains_1` through :eq:`eq_decay_chains_8`.

Impact on the decision threshold and the detection limit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ISO 11929-related procedure for deriving the decision threshold and
the detection limit needs to be modified in the case of the decay
corrections for a decay chain. This is explained for the decay chain
Pb-210/Bi-210/Po-210, for which the activity *A1*\ (*0*) of the first
member of the chain shall be calculated. The common relation *A1*\ (*0*)
=w\*\ *Rn* between activity and net count rate does no longer apply,
because this activity originates from two count rates, those of Bi-210
and Po-210.

The corresponding relation shall be modelled by an equation like
*A1*\ (*0*)= *w1*\ \*\ *Rn1* + *w2*\ \*\ *Rn2.* A modified or assumed
activity :math:`\widetilde{A}` of the output quantity is obtained by
multiplying the primary values :math:`A_{1}(0)` by a “modifying factor”
:math:`\widetilde{q}`:

.. math::
   \widetilde{A} = \widetilde{q} \cdot A_{1}(0) = w_{1} \cdot \left( R_{n1} \cdot \widetilde{q} \right) + w_{1} \cdot \left( R_{n1} \cdot \widetilde{q} \right) = w_{1} \cdot {\widetilde{R}}_{n1} + w_{2} \cdot {\widetilde{R}}_{n2}
  :label: eq_decay_chains_9

The modified count rate values :math:`{\widetilde{R}}_{nk} =`
:math:`\widetilde{q} \cdot {\widetilde{R}}_{nk}` (:math:`k`\ = 1,2)
lead to modified values
:math:`{\widetilde{R}}_{bk} = {\widetilde{R}}_{nk} + R_{0,k}` and
uncertainties
:math:`u^{2}\left( {\widetilde{R}}_{b,k} \right) = {\widetilde{R}}_{b,k}/t_{b}`
of the gross count rates, from which the uncertainty
:math:`u(\widetilde{A})` is derived, based on uncertainty propagation of
Eq. (18). This represents one iteration step in calculating the
detection limit. Values for the two „calibration factors“ *w1* and *w2*
are not always easily calculated, because they contain also elements of
the :math:`\mathbf{F\ }`\ matrix. They can more generally be derived
numerically by the partial derivatives of the output quantity with
respect to the net count rates :math:`R_{n,k}` :
:math:`w_{k} = \partial A_{1}(0)/\partial R_{n,k}`.


Implementation of decay chains in UncertRadio
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The implementation requires an additional function for executing the
more complex calculations of decay corrections for a decay chain. A
function SDECAY is implemented in UncertRadio which is called within the
set of user-defined equations for the evaluation of the model. The
function call has the form:

.. code-block::

   Symb0 = SDECAY(fmode, tdiff, tms, avg, Nstart, Ndest,
                  SymbAct1, SymAct2, SymbAct3, ...)


`SDECAY` calculates a value and a standard uncertainty by using the
least-squares method outlined above, which are attributed to the
variable named Symb0 (which is an activity in Bq).

The meaning of the function parameters are explained in the following
table.

+-----------+----------------------------------------------------------+
| Variable  | Meaning                                                  |
+===========+==========================================================+
| fmode     | Forward calculation (=1) or backward calculation (=0),   |
|           | starting from the respective activities; *integer*       |
+-----------+----------------------------------------------------------+
| tdiff     | Time difference; *UR symbol*                             |
+-----------+----------------------------------------------------------+
| tms       | Measurement duration; *UR symbol*                        |
+-----------+----------------------------------------------------------+
| avg       | Include (=1) or not (=0) the corrections for decay       |
|           | during the measurement; *integer*                        |
+-----------+----------------------------------------------------------+
| Nstart    | number of that decay chain member, from which on the     |
|           | decay shall be considered (Nstart >1) of if the full     |
|           | decay chain si to be considered (Nstart=1); *integer*    |
+-----------+----------------------------------------------------------+
| Ndest     | number of that decay chain member, the activity of which |
|           | shall be calculated; *integer*                           |
+-----------+----------------------------------------------------------+
| SymbAct1, | List of the UR activity symbols of the starting values   |
| SymbAct2, | of the decay chain (fmode=1) or of the end values of the |
| …         | decay chain (fmode=0); one may use more than the 3       |
|           | activity symbols shown in the call                       |
+-----------+----------------------------------------------------------+
| Symb0     | UR symbol of the arrays Messwert and StdUnc, to which    |
|           | the SDECAY function value and standard uncertainty are   |
|           | attributed to                                            |
+-----------+----------------------------------------------------------+

.. note::
   the values of Nstart and Ndest always refer to the full decay
   chain, even if Nstart > 1 is selected.


**Example calls:**

.. code-block::

   Symb0 = SDECAY(fmode, tdiff, tms, avg, Nstart, Ndest, SymbAct1, SymbAct2, SymbAct3)


- ``cPb210_t1 = SDECAY(0, t2minust1, tmBi210, 0, 1, 1, cPb210_t2, cBi210_t2, cPo210_t2)``

    “0“: (fmode): backward calculation;
    “t2minusT1“: (diff) time difference t2 – t1;
    “tmBi210“: (tms) counting duration (taken from Bi-210);
    “0, 1, 1“ mean: “0“: (avg): without correcting decay during the
    measurement; the first “1“: (Nstart) the decay chain starts from member
    1; the second “1“: (Ndest) the decay chain member for which the activity
    shall be calculated.

    “From the activities of the three decay chain members (the last three
    symbols within the call), given at the time t2, the activity of Pb-210
    (the first member: Ndest=1) at t1 (fmode=0) is to be calculated without
    correcting for decay during measurement; the measurement duration,
    tmBi210, is not used.”

- ``cPo210_t1 = SDECAY(0, t2minust1, tmBi210, 0, 2, 3, cPb210_t2, cBi210_t2, cPo210_t2)``

    “0“: (fmode): backward calculation;
    “t2minusT1“: (diff) time difference t2 – t1;
    “tmBi210“: (tms) counting duration (taken from Bi-210);
    “0, 2, 3“ mean: “0“: (avg): without correcting decay during the
    measurement; “2”: (Nstart) the decay chain starts from member 2
    (sub-chain Bi-210/Po-210); “3”: (Ndest) the third member of the full
    decay chain for which the activity shall be calculated.

    “The decay chain considered starts from the second member (i.e., the
    chain Bi-210/Po-210). From the activities of the two decay chain members
    of three (the last three symbols/parameters within the call), given at
    the time t2, the activity of Po-210 (Ndest=3) at t1 is to be calculated
    without correcting for decay during measurement; the measurement
    duration, tmBi210, is not used.”

If a call of `SDECAY` is found in one or more equations, a decay chain
dialog is invoked:

.. image:: /images/dialog_decay_chain_empty.png


In the upper part of this dialog certain measurement related conditions
can be defined. A list box allows the selection of the decay chain from
some pre-defined decay chains, in this case, the chain
Pb-210/Bi-210/Po-210. The possible condition selections are:

+------------------+---------------------------------------------------+
| Is a chemical    | In the case of combined Sr-89/Sr-90 measurements: |
| separation       | yes, the separation of Y-90 from Sr-90            |
| applied?         |                                                   |
+==================+===================================================+
| No. (1,2 or 3)   |                                                   |
| of the           |                                                   |
| radionuclide,    |                                                   |
| which is build   |                                                   |
| up since the     |                                                   |
| separation       |                                                   |
+------------------+---------------------------------------------------+
| Common           | May be yes in the case of beta-emitting           |
| measurement of   | radionuclides; depends on the measurement design  |
| the decay chain  |                                                   |
| members on the   |                                                   |
| same detector?   |                                                   |
+------------------+---------------------------------------------------+
| Number of        | Usually 1; in the case of LSC measurements, there |
| counting         | may be used more than one channel                 |
| channels (energy |                                                   |
| windows):        |                                                   |
+------------------+---------------------------------------------------+
| Shall decay      | Yes: decay constants used; No: half-lives used    |
| constants be the |                                                   |
| input quantities |                                                   |
| instead of       |                                                   |
| half-lives?      |                                                   |
+------------------+---------------------------------------------------+

A few pre-defined decay chains are available in a file
:file:`List_DecaySeries.txt`, which is read by UR if required:


   List of available decay series:

   ``Sr-90-2N : Sr-90 # Y-90 : z12=1``

   ``Zr-95-3N : Zr-95 # Nb-95m # Nb-95 : z12=0.0108# z13=0.9892# z23=0.944``

   ``Pb-210-3N : Pb-210 # Bi-210 # Po-210 : z12=1# z23=1``

   ``Pb-210-2N : Pb-210 # Po-210 : z12=1``


The structure of the file is simple:

  - every decay chain gets a short name (a string);

  - then the nuclide names follow, separated by the character #;

  - then the necessary branching ratios zji (with :math:`j < i`), which
    are not zero.

With the button "Transfer selections to Grid" pre-defined symbol names
are transferred to the grid for detection efficiencies (up to three when
using more than one counting channel) and chemical yields, which are
pre-defined from the radionuclide name. The columns of unused detection
efficiencies are left empty.

.. image:: /images/dialog_decay_chain_example.png
   :align: center

Now, the pre-defined symbols in the table (grid) can be modified.
Thereafter, the symbols in the table are merged into the symbol list of
the UR project by using the button “implement the dialog data”. For
these symbols values and uncertainties have to be inserted in the TAB
„Values, uncertainties”.

For a further editing of this dialog at a later time, it can be
re-opened from the **Menu Edit – Edit Decay chain**.

Generation of decay factor formulas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the case of radiochemical Sr-89/Sr-90 analyses, formulas for decay
factors need to be established. In beta radiation counting both Sr
isotopes, Sr-89 and Sr-90, contribute to the same count rate. The decay
of Sr-90 is accompanied by an ingrowth of its daughter product Y-90
which also contributes to the count rate. The complete formulas for this
application can already be complicated, especially, when corrections for
the decay during the measurement are included.

For such an application, UncertRadio supports the user by an additional
option for building these formulas as strings which are then transferred
into the text field in the dialog for defining the evaluation model of
decay curves.

This option can be invoked from the **Menu Edit – Edit decay chain.**
This option does not require an SDECAY function call within an UR
equation.

For the example mentioned at the begin of this section, the dialogs
layout is as follows:

.. image:: /images/image594.jpg
   :align: center


With the button `Generate Xi formulas for decay curve fit model` the
corresponding formula strings are generated and transferred in to the
associated text field of the model dialog.

**The (coded) result is the following:**

.. code-block::

   X1 = eSr90A * fd(tAs+tstart,tmess,lamSr90) + eY90A *  1/(lamSr90-lamY90)* ( lamY90*
         ( fd(tAs+tstart,tmess,lamY90)-fd(tAs+tstart,tmess,lamSr90)) )
   X2 = eSr89A * fd(tAs+tstart,tmess,lamSr89)
   X3 = eSr85A * fd(tAs+tstart,tmess,lamSr85)
   X4 = eSr90B * fd(tAs+tstart,tmess,lamSr90) + eY90B *  1/(lamSr90-lamY90)* ( lamY90*
         ( fd(tAs+tstart,tmess,lamY90)-fd(tAs+tstart,tmess,lamSr90)) )
   X5 = eSr89B * fd(tAs+tstart,tmess,lamSr89)
   X6 = eSr85B * fd(tAs+tstart,tmess,lamSr85)
   X7 = eSr90C * fd(tAs+tstart,tmess,lamSr90) + eY90C *  1/(lamSr90-lamY90)* ( lamY90*
         ( fd(tAs+tstart,tmess,lamY90)-fd(tAs+tstart,tmess,lamSr90)) )
   X8 = eSr89C * fd(tAs+tstart,tmess,lamSr89)
   X9 = eSr85C * fd(tAs+tstart,tmess,lamSr85)


.. image:: /images/image595.jpg


Example project
~~~~~~~~~~~~~~~

- :file:`Pb210_Bi210_Po210_series_backwards_EN.txp`

A short description is included in this example project file.


Decay Chain Literature
~~~~~~~~~~~~~~~~~~~~~~

Levy, E.: *Decay chain differential equations: Solutions through matrix
analysis*. Computer Physics Communications, 2019, Vol. 234, S. 188-194.

[Blobel, Lohrmann, Kapitel 3.4]

Blobel, V., Lohrmann, E.: *Statistische und numerische Methoden der
Datenanalyse*. Teubner Studienbücher Physik. 1. Auflage. Stuttgart:
Vieweg+Teubner Verlag, 1998, 358 S. ISBN 978-3-519-03243-4

Kapitel 4.2 in:

Kanisch, G., Aust, M.-O., Bruchertseifer, F., Dalheimer, A., Heckel, A.,
Hofmann, S., et al.: *Bestimmung der charakteristischen Grenzen bei der
Aktivitätsbestimmung radioaktiver Stoffe – Teil 1: Grundlagen.* Version
Mai 2022. *CHAGR-ISO-01*

In: Bundesministerium für Umwelt, Naturschutz, nukleare Sicherheit und
Verbraucherschutz, (Hrsg.): Messanleitungen für die Überwachung
radioaktiver Stoffe in der Umwelt und externer StrahIung. ISSN
1865-8725. Verfügbar unter: https://www.bmuv.de/WS1517

In Vorbereitung: Kanisch, G., Aust, M.-O., Bruchertseifer, F.,
Dalheimer, A., Heckel, A., Hofmann, S., et al.: *Zeitverhalten bei
mehrgliedrigen Zerfallsreihen.* Version Maixxx 2025. *ZERFALL/MEHRGL*

In: Bundesministerium für Umwelt, Naturschutz, nukleare Sicherheit und
Verbraucherschutz, (Hrsg.): Messanleitungen für die Überwachung
radioaktiver Stoffe in der Umwelt und externer StrahIung. ISSN
1865-8725. Verfügbar unter: https://www.bmuv.de/WS1517
