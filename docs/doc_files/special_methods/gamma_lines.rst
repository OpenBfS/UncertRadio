Activity determination from several gamma lines
-----------------------------------------------

In this example of use from the **gamma-ray spectrometry, preferentially
with high-resolution Germanium detectors**, it is assumed that some
different radionuclides may occur in the measured source, but the gamma
lines (more than one) of that radionuclide for which the activity shall
be determined must not interfere with other lines in the spectrum. The
case that the activity shall be determined from only a single line, is
not considered here, because that can be done with the standard
procedure of UncertRadio.

Let the net counting rates :math:`R_{ni}` of n gamma lines of the
radionuclide be given. From these the activity values :math:`A_{i}`, in
Bq, are calculated with the following equation:

.. math:: A_{i} = R_{ni}\frac{f_{att,i\ } \cdot \ f_{coinsu,i}}{\epsilon_{i}{\  \cdot \ p}_{\gamma i}\ }
    :label: gamma_lines_eq1

The symbols herein mean:

+----------------------------+-------------------------------------------+-----------------------+
|| Symbols                   || Meaning                                  || In Windows Dialog    |
||                           ||                                          || written as           |
+----------------------------+-------------------------------------------+-----------------------+
|| :math:`R_{ni}`            || net counting rate of the gamma line at   || RnetRate or          |
||                           || energy :math:`E_{i}`, in :math:`s^{- 1}` || PeakNetRate          |
+----------------------------+-------------------------------------------+-----------------------+
|| :math:`\epsilon_{i}`      || full-energy peak efficiency at energy    || effi, eps or epsPeak |
||                           || :math:`E_{i}`                            ||                      |
+----------------------------+-------------------------------------------+-----------------------+
|| :math:`{\ p}_{\gamma, i}` || (absolute) emission probability of the   || pgamm                |
||                           || line i                                   ||                      |
+----------------------------+-------------------------------------------+-----------------------+
|| :math:`f_{att',i}`        || self-attenuation correction at energy    || fatt                 |
||                           || :math:`E_{i}`                            ||                      |
+----------------------------+-------------------------------------------+-----------------------+
|| :math:`f_{coinsu,i}`      || correction for coincidence summing of a  || fcoinsu              |
||                           || line at energy :math:`E_{i}`             ||                      |
+----------------------------+-------------------------------------------+-----------------------+

The standard uncertainties :math:`u\left( A_{i} \right)` of the
activities of single lines calculated according to Eq. :eq:`gamma_lines_eq1` are
calculated internally in UncertRadio by using the uncertainty
propagation for Eq. :eq:`gamma_lines_eq1`.

Note: C\ ovariances between the calculated activities :math:`A_{i}` are
considered. Such covariances e.g. may be inferred by reading efficiency
values from the same FEP efficiency curve :math:`\epsilon(E)`, because
their values for different energies are derived from the same identical
set of parameters obtained from fitting the curve. Considering this
would require the inclusion of the full covariance matrix of these
parameters, which however cannot be handled by the program. Instead,
single values of the covariance/correlation can be input under the TAB
“Values, Uncertainties”.

**Description of procedures for calculating a mean**

- a) :ref:`Weighted mean <calculation of the weighted mean and its standard uncertainty>`

- b) :ref:`Weighted least-squares mean
     <least-squares calculation of a weighted mean and its standard uncertainty>`


:ref:`Procedure for calculating Decision threshold and Detection limit for
Gamspk1 <approach of calculating decision threshold and detection limit for gamspk1>`


**Invoking the evaluation of several gamma lines**

In the text field for equations the following call is used in that place
where one otherwise would define the activity *A* of the counting
source:

*A = Gamspk1(E, tlive)*

**Gamspk1** is the name of the procedure being activated with its
sub-dialogs which is doing the calculations of the chosen mean. Its
parameters are:

   *E* placeholder for the energies of individual gamma lines, in keV;
   the program automatically attributes values to :math:`E`

   *tlive* counting time (duration) (live-time) , in :math:`s`;

After loading the symbols from the equations they are available to the
**Gamspk1** function. Within the TAB “Values, uncertainties” a value and
its uncertainty must be given for the symbol *tlive*, but not for *E*.
Of course, instead of *E* and *tlive* other symbol names can be used,
they only must be defined in the symbol list of the project. These
symbols are defined always as “global” variables.

.. note::
   For the source activity *A* as defined here the decay correction
   and e.g. a mass or a volume have to be defined in the equations outside
   of Gamspk1.

**Input of values:**

for further information about input:
:ref:`Dialog Values from spectrum evaluation
<dialog values from spectrum evaluation>`

:ref:`Viewing the result of mean calculation with Gamspk1
<view of the result from calculating a mean with gamspk1>`
