Miscellaneous
=============

Implication of changes within the Options menu
----------------------------------------------

Only the **change from TAB “Values, uncertainties” to the TAB
“Uncertainty budget”** initiates the calculation of decision threshold,
detection limit, result and further statistical parameters (shown later
in the TAB “Results“). These calculations depend on the parameter
settings in the **menu Options – pre-settings**. If parameters therein
have been changed these calculations have to be repeated which is
achieved by the above-mentioned TAB change. Take note of the hint
occurring then in the right-most status bar field which indicates how to
continue then.

Algorithm for numerical calculation of the combined uncertainty
---------------------------------------------------------------

**Function subprogram UncPropa(p,u):**

! Calculations in double precision

:math:`Variance` = 0

! Calculations within a DO-loop:

! Begin:

do i=1 to n

:math:`\mathrm{\Delta}p_{i} = 10^{- 6}\ p_{i}`

:math:`fv_{1} = RESULT(\ldots,\ p_{i},\ \ldots)`

:math:`fv_{2} = RESULT(\ldots,\ p_{i} + \mathrm{\Delta}p_{i},\ldots)`

! Sensitivity factor = partial derivative with respect to the parameter
*p\ i* :

:math:`\frac{\partial f}{\partial p_{i}} \approx \frac{fv_{2} - fv_{1}}{\mathrm{\Delta}p_{i}}`

! Variance contribution of :math:`p_{i}` :

:math:`var(i) = \left( \frac{\partial f}{\partial p_{i}} \bullet u_{i} \right)^{2}`

:math:`Variance = Variance + var(i)`

end do

! End of the DO-loop:

**Uncertainty budget:** divide all values var(i) by the value
*Variance*: these are the relative contributions of the parameter I to
the variance!

:math:`UncPropa = \sqrt{Varianz}`: This is the value of the **combined
standard uncertainty**

Algorithm for iterative numerical calculation of the Detection limit :math:`\mathbf{y}^{\mathbf{\#}}`
-----------------------------------------------------------------------------------------------------

Starting value: :math:`y^{\# 0} = 2 \bullet y^{*}`

i = 0

! Start of iteration loop:

do

i = i + 1

! Let the gross counting rate be the 8th element of the parameter array
*p*;

! gross counting rate and its uncertainty, :math:`p(8)` and
:math:`u(8)`, respectively, where

! :math:`R_{0A}` is the background counting rate of the analyte and

! :math:`R_{NTimp}` is the blank component of the counting rate due to
contributions from further

! sources such as **t**\ racer **imp**\ urities

! *z*\ :sub:`1` and *z*\ :sub:`2` are those by UncertRadio estimated
parameters in *y* = *z*\ :sub:`1` \* *R*\ :sub:`n` + *z*\ :sub:`2`.

:math:`p(8)^{i} = \frac{y^{\#\ i - 1} - z_{2}}{z_{1}} + R_{NTimp} + R_{0A}`
; and

:math:`u(8)^{i} = \sqrt{\frac{p(8)^{i}}{t}}`

:math:`{\ y}^{\#\ i} = y^{*} + k_{1 - \beta} \bullet UncPropa\left( \ldots,\ u(8)^{i},\ldots \right)`

:math:`if\left( \frac{abs(y^{\#\ i} - y^{\#\ i - 1}}{y^{\#\ i}} < 0.0001 \right)\ `
EXIT ! Iteration terminated.

end do

! End of iteration loop: the last obtained value :math:`y^{\# i}` is the

! final value of the Detection limit.

Mathematics of the linear LSQ curve fitting with correlated measured values
---------------------------------------------------------------------------

.. _literature-1:

Literature
~~~~~~~~~~

ISO 11929:2010, Appendix C.5.4

Klaus Weise a. Wolfgang Wöger, 1999: Meßunsicherheit und
Meßdatenauswertung.

Verlag Wiley-VCH Weinheim, in German

S. 200 oben (Section 5.4.2 Lineare Kurvenanpassung)

   | Roger J. Barlow, 1999: Statistics. A Guide to the Use of
     Statistical Methods in the Physical
   | Sciences. The Manchester Physics Series. John Wiley & Sons Ltd.,
     Chichester, New York.

Section 6.6, pp. 111-113.

For its realisation matrix routines from the Datan-Library are applied
(converted to FORTRAN 90):

Datan-Library from:

Siegmund Brandt: Datenanalyse. Mit statistischen Methoden und
Computerprogrammen; 4. Auflage. Spektrum, Akademischer Verlag,
Heidelberg-Berlin, 1999. In German.

This text book is also available in an English version.

Further references:

   T. Hauschild, M. Jentschel, 2001: Comparison of maximum likelihood
   estimation and chi-square statistics applied to counting experiments.
   Nucl. Instr. & Meth A 457 (1-2), S 384-401.

   S. Pommé & J. Keightley, 2007: Countrate estimation of a Poisson
   process: unbiased fit versus central moment analysis of time interval
   spectra. Applied Modeling and Computations in Nuclear Science. In:
   Semkow, T.M., Pommé, S., Jerome, S.M., Strom, D.J. (Eds.), ACS
   Symposium Series 945. American Chemical Society, Washington, DC,
   pp.316–334.2007.ISBN0-8412-3982-7.

   T.A. Laurence and B. Chromy, 2009: Efficient Levenberg-Marquardt
   Minimization of the Maximum Likelihood Estimator for Poisson
   Deviates. Report LLNL-JRNL-420247, November 13, 2009.

Basics
~~~~~~

The evaluation model :math:`\mathbf{M}` is formulated in the following
form (according to Weise & Wöger; *note: the meaning of X and Y is just
interchanged compared to Weise & Wöger)*:

:math:`\mathbf{M}\left( \mathbf{X,\ Y} \right)\mathbf{= X - A\ Y = 0}`

The nr-vector :math:`\mathbf{Y}` characterises the output parameters to
be estimated and the n-vector :math:`\mathbf{X}` input measurement
values to be fitted by the model. The (*n* x *nr*) matrix
:math:`\mathbf{A}` contains the partial derivatives of the fitting
function with respect to the parameters to be estimated which are the
functions :math:`X_{i}(t)`. *Variances as well as covariances between
measured x-values are collected in the non-diagonal uncertainty matrix*
:math:`\mathbf{U}_{\mathbf{x}}`.

According to Weise & Wöger the following equations are obtained as
solution (see also Appendix C.5.4 of ISO 11929:2010, Eq. (C.32); same
notation as here!):

:math:`\mathbf{Y =}\mathbf{U}_{\mathbf{y}}\mathbf{\ A}^{\mathbf{T}}\mathbf{\ U}_{\mathbf{x}}^{\mathbf{- 1}}\mathbf{\ x}`;
:math:`\mathbf{U}_{\mathbf{y}}\mathbf{=}\left( \mathbf{A}^{\mathbf{T}}\mathbf{\ }\mathbf{U}_{\mathbf{x}}^{\mathbf{- 1}}\mathbf{\ A} \right)^{\mathbf{- 1}}`
(5.63)

Here, :math:`\mathbf{y}` is the resulting output vector of the fitted
parameters and :math:`\mathbf{U}_{\mathbf{y}}^{\mathbf{- 1}}` its
associated covariance matrix.

For the value of the minimum function one obtains by using
:math:`\mathbf{\ A}^{\mathbf{T}}\mathbf{\ U}_{\mathbf{x}}^{\mathbf{- 1}}\mathbf{\ x =}\mathbf{U}_{\mathbf{y}}^{\mathbf{- 1}}\mathbf{\ y}`:

:math:`\mathbf{\min}\mathbf{\chi}^{\mathbf{2}}\mathbf{=}\left( \mathbf{Ay - x} \right)^{\mathbf{T}}\mathbf{\ }\mathbf{U}_{\mathbf{x}}^{\mathbf{- 1}}\mathbf{\ }\left( \mathbf{Ay - x} \right)\mathbf{=}\mathbf{x}^{\mathbf{T}}\mathbf{\ }\mathbf{U}_{\mathbf{x}}^{\mathbf{- 1}}\mathbf{\ x -}\mathbf{y}^{\mathbf{T}}\mathbf{\ }\mathbf{U}_{\mathbf{y}}^{\mathbf{- 1}}\mathbf{\ y}`
(5.65)

:math:`\chi_{R}^{2} = \min{\chi^{2}/(n - nr)}`

The covariance matrix :math:`\mathbf{U}_{\mathbf{y}}^{\mathbf{- 1}}` can
be multiplied with :math:`\chi_{R}^{2}`, if the value of the latter is
larger than one although it is not allowed from the strict Bayesian
point of view and therefore not performed in UncertRadio.

Chi-square options
~~~~~~~~~~~~~~~~~~

The following literature is recommended:

   S. Baker, R.D. Cousins, 1983: Clarifications of the use of Ch-square
   and likelihood functions. Nucl. Instr. & Meth 221, S 437-442.

   T. Hauschild, M. Jentschel, 2001: Comparison of maximum likelihood
   estimation and chi-square statistics applied to counting experiments.
   Nucl. Instr. & Meth A 457 (1-2), S 384-401.

The calculations shown under Basics use the “\ **Neyman Chi-square“
expression,** defined as follows (mnemonic: **WLS**):

:math:`\chi_{N}^{2} = \sum_{j}^{}\frac{\left( x_{j} - f\left( t_{j};\mathbf{y} \right) \right)^{2}}{u^{2}\left( x_{j} \right)}`
,

i.e., the diagonal elements of :math:`\mathbf{U}_{\mathbf{x}}` are the
variances of the measured values\ :math:`x_{j}` . This may lead to a
bias of **y** in the case of very low counting rates. For reducing the
bias the so-called “\ **Pearson Chi-square“** expression can be used
which is defined as (mnemonic: **PLSQ**):

:math:`\chi_{P}^{2} = \sum_{j}^{}\frac{\left( x_{j} - f\left( t_{j}\mathbf{;y} \right) \right)^{2}}{{\widetilde{u}}^{2}\left( f\left( t_{j};\mathbf{y} \right) \right)}`

The variance given in the nominator is calculated with using the
function :math:`f` to be fitted.

:math:`{\widetilde{\mathbf{U}}}_{\mathbf{x}}(f) = diag\left\lbrack \frac{\left( \mathbf{Ay} + R_{0,i} + R_{bl} \right)}{t_{m}} + var\left( R_{0,i} \right) + var\left( R_{bl} \right) \right\rbrack`
,

where :math:`R_{0,i}` and :math:`R_{bl}` represent the background or net
blank count rate, respectively.

As this function value can only be calculated from the value of **y**
which is to be fitted, the minimization of the chi-square expression
requires an iterative procedure. The iteration can be written in analogy
to the Eq. (5.63), with (m) and (m‑1) being two consecutive steps:

:math:`\mathbf{y}_{\mathbf{(m)}} = \mathbf{U}_{\mathbf{y,(m)}}\mathbf{\ }\mathbf{A}^{T}\mathbf{U}_{\mathbf{x,(m - 1)}}^{\mathbf{- 1}}\left( \mathbf{x} \right)\mathbf{\ x}`
;
:math:`\mathbf{\ }\mathbf{U}_{\mathbf{y,(m)}} = \left( \mathbf{A}^{T}\mathbf{U}_{\mathbf{x,(m - 1)}}^{\mathbf{- 1}}\left( \mathbf{x} \right)\mathbf{A} \right)^{- 1}`

Before repeating an iteration, the “measured“ variances need to be
replaced from the fitting function by re-calculating the diagonal
elements of :math:`\mathbf{U}_{\mathbf{x}}` with using the values
:math:`\mathbf{Ay}` from the preceding step (the non-diagonal elements
are not changed):

:math:`\mathbf{U}_{\mathbf{x}}^{\mathbf{'}} = diag\left\lbrack \frac{\left( \mathbf{Ay} + R_{0,i} + R_{bl} \right)}{t_{m}} + var\left( R_{0,i} \right) + var\left( R_{bl} \right) \right\rbrack`

**Note**: If the covariances between measured values are equal to zero,
i.e. the matrix :math:`\mathbf{U}_{\mathbf{x}}` is diagonal with
variances as its elements, this procedure completely corresponds to the
better known *weighted linear least-squares fitting method.*

As an alternative to the Pearson Chi-square expression the so-called
„\ **Poisson Maximum Likelihood Estimation**\ “-procedure (or Poisson
MLE) may be used for reducing the bias. The associated Chi-square
expression is (mnemonic: **PMLE**):

:math:`\chi_{\lambda}^{2} = 2\left\lbrack \sum_{j}^{}\left( f\left( t_{j};\ \mathbf{y} \right) - x_{j} \right) - \sum_{j;x_{j} \neq 0\ }^{}{x_{j}\ln\left( \frac{f\left( t_{j};\ \mathbf{y} \right)}{x_{j}} \right)} \right\rbrack`
.

The minimization of this expression requires a non-linear fitting
method: in UncertRadio a modified version of the “Levenberg-Marquardt“
fitting routine LSQmar from the textbook „Data analyses” by S. Brandt.
The modification of LSQmar necessary for Poisson MLE can be taken from
the report LLNL-JRNL-420247 (2009).

Shall net count rates be fitted, this procedure requires gross counts to
be fitted. This small transformation is done within UncertRadio; it has
the advantage that gross counts are always non-correlated, which may be
different when working with net count rates.

The PMLE method can be selected in the example project
vTI-Y90-16748_BLW_V2_EN.txp.

According to Baker & Cousins the application of the **PMLE method
preserves the area under the curve to be fitted,** while PLSQ tends to
overestimation and WLS to underestimation.

`Notes on the PMLE
procedure <#notes-on-the-pmle-procedure-for-linear-unfolding>`__

**Statistical tests of Chi-squared options**

The function to be fitted is represented by the sum of two gaussian
peaks with equal area and identical width (sigma = 5.1 channels, net
peak peak areas of 150 counts each, located at channels 30 and 90) on a
constant background of 4 counts per channel. Thus, the fit function has
6 parameters, which are considered as true values: 4.0, 150, 30, 5.1,
150, 90. These are considered as true values, from which a large number
of Nsp=400 spectra are simulated, with their „true“ channel counts being
replaced by Poisson distributed counts (noisy). Each of these spectra
are fitted with the different fitting methods using non-linear as well
as linear fitting. The following figure shows such a spectrum, with the
“true” fitting function and those obtained by the non-linear methods WLS
and PMLE.

+-----------------------------------------------------------------------+
| |Ein Bild, das Text, Diagramm, Reihe, Screenshot enthält. Automatisch |
| generierte Beschreibung|                                              |
+=======================================================================+
+-----------------------------------------------------------------------+

The evaluation of 400 spectra in each case, which are fitted
non-linearly by the methods WLS, PLSQ and PMLE, is tabulated in the
following. In each case, from 400 fitted values of the 6 parameters, a
mean value (pa=), a standard deviation between the 400 values (sd=) and
mean value of the individual the standard uncertainties (mean(sds..)),
calculated by the fitting routine, are derived. In addition, sums (over
the 6 parameters) of absolute deviations (fitted value minus true value)
and of differences (sd minus sds) are calculated, which are given in two
additional lines below each single table. These values serve as good
indicators of the consistency of fitting.

Non-linear fitting:

mean values: for method WLS

i=1 pa= 2.75534E+00 sd(pa(i))= 2.34472E-01 mean(sds(i))= 1.73935E-01

i=2 pa= 1.57094E+02 sd(pa(i))= 1.91809E+01 mean(sds(i))= 1.59540E+01

i=3 pa= 2.99839E+01 sd(pa(i))= 7.59367E-01 mean(sds(i))= 5.81371E-01

i=4 pa= 5.18531E+00 sd(pa(i))= 5.09876E-01 mean(sds(i))= 3.88957E-01

i=5 pa= 1.58079E+02 sd(pa(i))= 1.84934E+01 mean(sds(i))= 1.59980E+01

i=6 pa= 8.99873E+01 sd(pa(i))= 7.84781E-01 mean(sds(i))= 5.79122E-01

sum of absolute deviations from true(pa) : 16.5316238

sum of absolute deviations of two sd types : 6.28735828

mean values: for method PLSQ

i=1 pa= 3.99617E+00 sd(pa(i))= 2.11079E-01 mean(sds(i))= 1.73482E-01

i=2 pa= 1.49851E+02 sd(pa(i))= 1.75574E+01 mean(sds(i))= 1.57870E+01

i=3 pa= 3.00262E+01 sd(pa(i))= 6.38806E-01 mean(sds(i))= 5.91121E-01

i=4 pa= 5.07812E+00 sd(pa(i))= 4.50239E-01 mean(sds(i))= 3.95102E-01

i=5 pa= 1.50346E+02 sd(pa(i))= 1.63419E+01 mean(sds(i))= 1.58062E+01

i=6 pa= 8.99958E+01 sd(pa(i))= 6.39568E-01 mean(sds(i))= 5.90819E-01

sum of absolute deviations from true(pa) : 0.551150203

sum of absolute deviations of two sd types : 2.49521804

mean values: for method PMLE

i=1 pa= 4.00893E+00 sd(pa(i))= 2.10194E-01 mean(sds(i))= 2.07077E-01

i=2 pa= 1.51928E+02 sd(pa(i))= 1.69198E+01 mean(sds(i))= 1.68943E+01

i=3 pa= 2.99583E+01 sd(pa(i))= 6.46945E-01 mean(sds(i))= 6.26552E-01

i=4 pa= 5.08396E+00 sd(pa(i))= 4.22725E-01 mean(sds(i))= 4.34723E-01

i=5 pa= 1.49174E+02 sd(pa(i))= 1.73843E+01 mean(sds(i))= 1.67870E+01

i=6 pa= 8.99848E+01 sd(pa(i))= 6.22015E-01 mean(sds(i))= 6.35672E-01

sum of absolute deviations from true(pa) : 2.83508420

sum of absolute deviations of two sd types : 0.671966136

The best absolute deviation for (Fit value minus true value) is obtained
for the PLSQ method; the best consistency of fitted uncertainties (abs.
deviation (sd minus sds)) is found for the PMLE method.

That in this analysis the methods PLSQ and PMLE yield better consistency
values than the classical fit (WLS), can be explained by the condition
of having a quite low background of 4 counts per channel. If this
background would be significantly increased, the difference between the
fitting methods would be much smaller.

The spectra can also be treated by linear fitting methods, if the width
parameter and the two peak position parameters are held fixed at their
true values. In this case, only the background parameter and the two
peak areas are fitted. The results of such an evaluation are given in
the following.

Linear fitting:

mean values: method = WLS

i=1 pa= 2.77091E+00 sd(pa(i))= 2.19480E-01 mean(sds(i))= 1.61749E-01

i=2 pa= 1.53462E+02 sd(pa(i))= 1.78632E+01 mean(sds(i))= 1.50700E+01

i=5 pa= 1.54138E+02 sd(pa(i))= 1.74788E+01 mean(sds(i))= 1.51000E+01

sum of absolute deviations from true(pa) : 8.82905006

sum of absolute deviations of two sd types : 5.22971344

mean values: method = PLSQ

i=1 pa= 4.00339E+00 sd(pa(i))= 1.95047E-01 mean(sds(i))= 1.92895E-01

i=2 pa= 1.49325E+02 sd(pa(i))= 1.63338E+01 mean(sds(i))= 1.58374E+01

i=5 pa= 1.49789E+02 sd(pa(i))= 1.53364E+01 mean(sds(i))= 1.58544E+01

sum of absolute deviations from true(pa) : 0.889460266

sum of absolute deviations of two sd types : 1.01658702

Similarly, as for non-linear fitting, the results for the PLSQ method
show a better consistency than for the WLS method.

Export of input data to R
~~~~~~~~~~~~~~~~~~~~~~~~~

Since this version of UncertRadio it is possible export the input data,
depending on the chosen fitting procedure (apart from WTLS: **R** does
not yet support this!), in a format which as compatible with the
corresponding R routine into a text file (URExport-to-R.txt, or
similarly). This file / these files can easily be imported by the
**statistics package R.** This allows comparing the results between UR
and R. This option is invoked by the menu item “options – LSQ export to
R” and can be used for the cases of calculating the output quantity and
the decision threshold. The data required for this refer to counting
rates only, not to the result for the output quantity. For the case of
the output quantity (see URExport-to-R.txt) one obtains:

(Note: the covariance shown below is truncated at the right hand)

**Case: output quantity**

Blank count rate= 4.66670009E-08 background rate= 1.88333332E-03

Input data: variance-covariance matrix: (rank= 18 )

2.87780E-07 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.46788E-07 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 1.96152E-07 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 1.64805E-07 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 1.41898E-07 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 1.35870E-07
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
1.43104E-07

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

Arrays y, X1, x2, X3:

y X1 X3 (Eingangsdaten-Matrix)

1 5.65134E-03 8.80220E-01 2.73093E-01

2 4.47079E-03 8.07210E-01 1.10866E-01

3 3.01245E-03 7.40257E-01 4.50075E-02

4 2.10968E-03 6.78857E-01 1.82714E-02

5 1.44995E-03 6.22550E-01 7.41751E-03

6 1.27634E-03 5.70913E-01 3.01124E-03

7 1.48468E-03 5.23559E-01 1.22245E-03

8 9.98564E-04 4.80133E-01 4.96272E-04

9 8.47116E-04 4.40700E-01 2.03089E-04

10 8.24953E-04 4.03787E-01 8.17887E-05

11 1.24162E-03 3.70295E-01 3.32032E-05

12 5.12453E-04 3.39582E-01 1.34793E-05

13 9.63842E-04 3.11415E-01 5.47210E-06

14 3.38842E-04 2.85585E-01 2.22147E-06

15 1.65231E-04 2.61897E-01 9.01838E-07

16 -7.78244E-05 2.40175E-01 3.66113E-07

17 2.69398E-04 2.20253E-01 1.48629E-07

18 1.99953E-04 2.01985E-01 6.03378E-08

Parameter values and std uncertatinties obtained by UR:

1 2.83190E-03 3.55440E-04

3 1.45234E-02 2.01819E-03

Chisqr= 1.23143363

For the import to R the covariance matrix and the input data Are written
to separate files:

Output quantity: covmat1.txt and data1.txt

Decision threshold: covmat2.txt and data2.txt

With these files a statistical evaluation by R can be done as follows:

(load package MASS) (R)

One obtains with R for the output quantity:

> covmat <- read.table("covmat1.txt")

> data <- read.table("data1.txt")

> res <- lm.gls(formula = y ~ X1 + X3 - 1, data = data, W = covmat,
inverse = TRUE)

> summary.lm(res)

Call:

lm.gls(formula = y ~ X1 + X3 - 1, data = data, W = covmat, inverse =
TRUE)

Residuals:

Min 1Q Median 3Q Max

-8.076e-04 -4.422e-04 -3.702e-04 -3.134e-05 5.747e-04

Coefficients:

Estimate Std. Error t value Pr(>|t\|)

X1 2.832e-03 1.643e-07 17231 <2e-16 \**\*

X3 1.452e-02 9.332e-07 15564 <2e-16 \**\*

---

Signif. codes: 0 ‘\**\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.0004624 on 16 degrees of freedom

Multiple R-squared: 0.9625, Adjusted R-squared: 0.9578

F-statistic: 205.1 on 2 and 16 DF, p-value: 3.95e-12

Warning message:

In summary.lm(res) : calling summary.lm(<fake-lm-object>) ...

To be able to compare the uncertainty with that given by UR, the
uncertainty from R (Std. error) is divided by the value Residual
standard error.

Thus, one obtains from R an uncertainty of the fitted parameter X1:

1.643E-07 / 0.0004624 = 3.5532E-04

Case: Decision threshold

Blank count rate= 4.66670009E-08 background rate= 1.88333332E-03

Input data: variance-covariance matrix: (rank= 18 )

2.29269E-07 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 1.47460E-07 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 1.14249E-07 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 1.00767E-07 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 9.52931E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 9.30711E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
9.21690E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08 2.61574E-08
2.61574E-08

Arrays y, X1, x2, X3:

y X1 X3

1 3.96624E-03 8.80220E-01 2.73093E-01

2 1.61015E-03 8.07210E-01 1.10866E-01

3 6.53662E-04 7.40257E-01 4.50075E-02

4 2.65363E-04 6.78857E-01 1.82714E-02

5 1.07728E-04 6.22550E-01 7.41751E-03

6 4.37335E-05 5.70913E-01 3.01124E-03

7 1.77542E-05 5.23559E-01 1.22245E-03

8 7.20756E-06 4.80133E-01 4.96272E-04

9 2.94955E-06 4.40700E-01 2.03089E-04

10 1.18786E-06 4.03787E-01 8.17887E-05

11 4.82232E-07 3.70295E-01 3.32032E-05

12 1.95772E-07 3.39582E-01 1.34793E-05

13 7.94798E-08 3.11415E-01 5.47210E-06

14 3.22691E-08 2.85585E-01 2.22147E-06

15 1.31030E-08 2.61897E-01 9.01838E-07

16 5.32199E-09 2.40175E-01 3.66113E-07

17 2.16298E-09 2.20253E-01 1.48629E-07

18 8.80329E-10 2.01985E-01 6.03378E-08

Parameter values and std uncertatinties obtained by UR:

1 1.98944E-11 3.10543E-04

3 1.45234E-02 1.73864E-03

Chisqr= 8.88178420E-16

For the case of the decision threshold one obtains with R

(in this case, only the uncertainty associated with X1 is value of
interest; the value of X1 should be close to zero):

> covmat <- read.table("covmat2.txt")

> data <- read.table("data2.txt")

> res <- lm.gls(formula = y ~ X1 + X3 - 1, data = data, W = covmat,
inverse = TRUE)

> summary.lm(res)

Call:

lm.gls(formula = y ~ X1 + X3 - 1, data = data, W = covmat, inverse =
TRUE)

Residuals:

Min 1Q Median 3Q Max

-1.370e-09 -5.200e-13 -4.300e-13 3.721e-11 8.881e-10

Coefficients:

Estimate Std. Error t value Pr(>|t\|)

X1 2.193e-11 1.334e-13 1.644e+02 <2e-16 \**\*

X3 1.452e-02 7.467e-13 1.945e+10 <2e-16 \**\*

---

Signif. codes: 0 ‘\**\*’ 0.001 ‘\*\*’ 0.01 ‘\*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.295e-10 on 16 degrees of freedom

Multiple R-squared: 1, Adjusted R-squared: 1

F-statistic: 5.105e+13 on 2 and 16 DF, p-value: < 2.2e-16

Warning message:

In summary.lm(res) : calling summary.lm(<fake-lm-object>) ...

Thus, one obtains from R an uncertainty of the fitted parameter X1:

1.334E-13 / 4.295E-10 = 3.10594E-04

Note on the calculation of Decision threshold and Detection limit:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These special output quantities refer to the component :math:`y_{1}` of
the result vector :math:`\mathbf{y}` and its uncertainty
:math:`u\left( y_{1} \right)`, which is iterated until :math:`y_{1}` and
:math:`u\left( y_{1} \right)` fulfil the terminating condition of the
iteration. :math:`y_{1}` is replaced by one new value :math:`y_{1}^{'}`
determined by the iteration. From this one obtains according to the
model
:math:`\mathbf{x}^{\mathbf{'}}\mathbf{= A\ }\mathbf{y}^{\mathbf{'}}`,
which yields a modified covariance matrix
:math:`\mathbf{U}_{\mathbf{x}}^{\mathbf{'}}`. For this purpose, at first
new values are attributed to the **gross counting rates** of the decay
curve:

:math:`R_{b,i}^{'} = \left( R_{0,i} + R_{bl} \right) + y_{1}^{'} \bullet X_{1}\left( t_{i} \right) + y_{2} \bullet X_{2}\left( t_{i} \right) + y_{3} \bullet X_{3}\left( t_{i} \right)`

From this the **Uncertainty function (standard uncertainty) of the gross
counting rate** results:

:math:`u\left( R_{b,i}^{'} \right) = \sqrt{R_{b,i}^{'}/t_{m,i}}` **.**

The net counting rates of the modified decay curve then are:

:math:`R_{n,i}^{'} = R_{b,i}^{'} - R_{0,i} - R_{bl}` ,

from which the diagonal elements of the varied covariance matrix
:math:`\mathbf{U}_{\mathbf{x}}^{\mathbf{'}}` (variances of the net
counting rates) result:

:math:`var\left( R_{n,i}^{'} \right) = \frac{R_{b,i}^{'}}{t_{m,i}} + var\left( R_{0,i} \right) + var\left( R_{bl} \right)`
,

while the non-diagonal elements are left unchanged.

From the right-hand formula of Eq. (5.63) above the uncertainty
:math:`u\left( y_{1}^{'} \right)` is determined. With the pair
:math:`y_{1}^{'}` and :math:`u\left( y_{1}^{'} \right)` the next
iteration step can be started; this iteration may be repeated if the
convergence criterion is not yet met.

Note on Decision threshold and Detection limit for linear fitting
-----------------------------------------------------------------

At first, for the equation used in the Least squares fitting

:math:`Y\left( t_{k} \right) = a_{1} \bullet X_{1}\left( t_{k} \right) + \ a_{2} \bullet X_{2}\left( t_{k} \right) + \ a_{3} \bullet X_{3}\left( t_{k} \right)`

it must be defined, which of the fitting parameters :math:`a_{i}`
corresponds to the actually valid output quantity. For the following
notes let us assume that the parameter :math:`a_{1}` is the one
representing the output quantity.

The procedure for calculating the Decision threshold and the Detection
limit follows ISO 11929:2010. The parameter :math:`a_{1}` is modified by
iteration. At first, the parameter :math:`a_{1}` in the above equation
(i.e. the value of the Y-90 counting rate in the case of a Y-90
decay-curve) is replaced by an iterated value :math:`a_{1}^{'}` while
all other values remain unchanged. From this, new measured net counting
rates :math:`Y^{'}\left( t_{k} \right)` are calculated as well as its
new uncertainties :math:`u\left( Y'\left( t_{k} \right) \right)` (the
uncertainty function). With these new values, i.e.
(:math:`a_{1}^{'}`,\ :math:`Y^{'}\left( t_{k} \right)`,\ :math:`\ u\left( Y'\left( t_{k} \right) \right)`),
the least squares analysis calculations are repeated yielding the
uncertainty :math:`u\left( a_{1}^{'} \right)` of the iterated parameter
value :math:`a_{1}^{'}`. With this pair of values
(:math:`a_{1}^{'}`,\ :math:`\ u\left( a_{1}^{'} \right)`) it is then
tested whether the termination condition of the iteration procedure is
fulfilled; if not, the next iterated value :math:`a_{1}^{''}` is
determined and the above procedure is repeated in order to find its
uncertainty :math:`u\left( a_{1}^{''} \right)`; and so on.

A more detailed description of these calculations while iterating can be
found at the end of the following help topics:

   a) `Mathematics of linear curve fitting with
   WLS <#mathematics-of-the-linear-lsq-curve-fitting-with-correlated-measured-values>`__,

   b) `Mathematics of linear curve fitting with
   WTLS <#notes-on-linear-curve-fitting-using-general-least-squares-wtls>`__.

Notes on linear curve-fitting using general least squares (WTLS)
----------------------------------------------------------------

The so-called “general case” of the method of least squares (WTLS) may
be used in UncertRadio in such cases where uncertainties but also
covariances are attributed to the :math:`X_{i}\left( t_{k} \right)`
values. It is the only method known to the author which allows the
inclusion of such uncertainties and covariances.

For its realisation matrix routines from the Datan-Library are applied
(converted to FORTRAN 90), the main subroutine being LSQGEN:

Datan-Library from:

Siegmund Brandt, 1999: Datenanalyse. Mit statistischen Methoden und
Computerprogrammen; 4. Auflage. Spektrum, Akademischer Verlag,
Heidelberg-Berlin. In German.

This text book is also available in an English version.

Although the model continues to be linear with respect to the fitting
parameters, WTLS requires an iterative procedure. Therefore, starting
values of the fitting parameters are obtained from a preceding call to
the simpler LSQ routines.

Unfortunately, the mathematics used in the routine LSQGEN is rather
complicated, which is the reason that a description cannot yet be given
here. Another routine precedes LSQGEN in UncertRadio which assembles the
input data in a special form which is required by LSQGEN. Measured net
counting rates :math:`Y\left( t_{k} \right)` and the different
:math:`X_{i}\left( t_{k} \right)\ `\ values are combined in one common
vector y; the same applies to the uncertainties and covariances of the
:math:`Y\left( t_{k} \right)` and :math:`X_{i}\left( t_{k} \right)`
values, which results in a common input covariance matrix cy. The rank
of the vector :math:`\mathbf{y}` and of the quadratic matrix cy may
easily increase to a significant value. If, for example, a LSC
measurement with 3 counting channels is considered and 10 measurements
are done, one obtains:

   3x10 = 30 values :math:`Y\left( t_{k} \right)` (10 values for each of
   the channels A, B and C)

   3x3x10 = 90 values :math:`X_{i}\left( t_{k} \right)` (3 values
   :math:`X_{i}\left( t_{k} \right)` associated with each of the values
   :math:`Y\left( t_{k} \right)`)

Therefore, the rank will have the value 30+90=120. This means, that the
input covariance matrix becomes a 120x120 matrix, which makes it easily
plausible that the iteratively working routine LSQGEN may indeed become
more time-consuming.

**Note on the calculation of Decision threshold and Detection limit:**

To be comparable now with the description of the LLSQ mathematics the
result vector of the fitted parameters will here be designated as
:math:`\mathbf{y}`.

The special output quantities to be considered here refer to the
component L of the result vector :math:`\mathbf{y}` of the fitted
parameters, :math:`y_{L}`, and its uncertainty
:math:`u\left( y_{L} \right)`, which is iterated until :math:`y_{L}` and
:math:`u\left( y_{L} \right)` fulfil the terminating condition of the
iteration. :math:`y_{L}` is then replaced by one new value
:math:`y_{L}^{'}\ `\ determined by the iteration. Using this one, one
can calculate new values of the **gross counting rates** of the decay
curve, where L=1 was set without losing generality:

:math:`R_{b,i}^{'} = \left( R_{0,i} + R_{bl} \right) + y_{1}^{'} \bullet X_{1}\left( t_{i} \right) + y_{2} \bullet X_{2}\left( t_{i} \right) + y_{3} \bullet X_{3}\left( t_{i} \right)`
.

From this the **uncertainty function (standard uncertainty) of the gross
counting rate** results:

:math:`u\left( R_{b,i}^{'} \right) = \sqrt{R_{b,i}^{'}/t_{m,i}}` **.**

The net counting rates of the modified decay curve, i.e. the new
:math:`Y'\left( t_{k} \right)`, then are:

:math:`R_{n,i}^{'} = R_{b,i}^{'} - R_{0,i} - R_{bl}` ,

from which the diagonal elements of the varied covariance matrix result:

:math:`var\left( R_{n,i}^{'} \right) = \frac{R_{b,i}^{'}}{t_{m,i}} + var\left( R_{0,i} \right) + var\left( R_{bl} \right)`,

while the non-diagonal elements are left unchanged.

With these modified values and uncertainties of the net counting rates
the evaluation for the output quantity is repeated yielding the new
uncertainty value :math:`u\left( y_{1}^{'} \right)`. Now, the
convergence criterion can be tested; if it is not yet fulfilled, the
next iteration step is initiated by determining the next iteration value
:math:`y_{1}^{''}` from the pair :math:`y_{1}^{'}` and
:math:`u\left( y_{1}^{'} \right)`.

Within tables: delete rows, working with column blocks
------------------------------------------------------

Having already obtained a symbol table form the interpretation the
equations, a **subsequent modification of the equations may result in
new symbols or other symbols may become redundant**, which leads to
changes in the symbol table.

**Additionally occurring new symbols** are indicated in the symbol table
by **green colored rows**. For these, the columns *Unit* and *Meaning*
may be edited.

Some **symbols may have become redundant**, because they are no longer
used; the corresponding symbol rows are shifted to the end of the table
and indicated by **yellow color**. Here, the user must decide whether
these rows can be deleted; if so, these can be deleted by clicking the
toolbar icon |image44| .

At present, **Column blocks** cannot be defined within tables;
corresponding blocks of data can therefore not be exported to, e.g.,
Excel. It is however possible by “copy and paste” to **insert such a
column block in an UR table** which has been selected in Excel and
copied to the Windows clipboard. This requires the following steps:

-  open the upper-left cell of the UR table by double-clicking into it;

-  insert the block into this cell using “paste” in the mouse context
   menu; In this moment, this cell holds the whole block.

-  pressing the enter button once extends this block over the area of
   cells.

Text field for equations
------------------------

Equations can be written into this text field line-by-line. A special
end-of-line character is not necessary; only in the case that **an
equation has to be continued in additional lines** each (but not the
last) line of this equation must have a “&” character at its end.

The **equations** must be **set up** in a **hierarchical** way.

One **starts with that basic equation** which defines the **output
quantity y**. This may for example read:

*y = w \* Rn - Ai*

Naturally, another symbol can be used for the output quantity. In the
following lines for those symbols used only in the right-hand parts of
the preceding equations, if they do not already represent a primary
input quantity, further equations are defined (**secondary equations**),
for example:

*Rn = Rg - R0* net counting rate

w *= 1. / (eps \* eta \* m) \* f1* procedure dependent calibration
factor

*f1 = exp(+log(2.) \* t1 / tr)* inverse decay factor

*Ai (=z2)* an interference contribution to be subtracted

Herein, *Ai* represents an interference contribution to the activity,
i.e. *Ai* equals the constant *FC* determined internally by the program.
The factor *w* corresponds to the other constant, *FL.*

Notes: a) If more than one output quantity were defined for the project,
e.g. three, then for each output quantity one basic equation must exist;
these then are the first three equations. b) In such cases where
interference by another radionuclide exists, *Rn* must be understood as
the “\ **procedure dependent net counting rate**\ ” the equation of
which contains an extra term calculated for this interference.

*The simple expression Rn = Rg - R0 may be used also in the case of
additional interference contributions. The latter (interference)
contribution is taken automatically into account by the internally
determined auxiliary quantity FC (*\ `see
also <#numerical-procedures>`__\ *).*

Because of the hierarchical structure, the **equations are evaluated
from bottom to top for obtaining values** for all the quantities. This
means that in any equation only such symbols can be used in it belonging
to secondary (auxiliary) equations following that equation. The program
internally tests whether this condition is fulfilled; if not, the user
will get an associated warning.

It is necessary to **use explicitly an equation defining the net
counting rate** *Rn.* In this important equation it is allowed for the
symbol of the gross counting rate to be multiplied with a factor; in
seldom cases, this may be necessary. The value of this factor is
identified by the program; it only may play a role for determining
Decision threshold and Detection limit.

Note: The gross counting rate symbol must be directly contained in the
equation defining the net counting rate, or, another symbol in the
latter equation points to a further auxiliary equation in which then
contains it.

Example: *Rn = Rn1 - Rblank*; *Rn1 = Rg - R0*.

The **procedure dependent factor w** in the above example contains the
inverse decay factor *f1* for correcting the radioactive decay of a
radionuclide r, having the half-live *tr,* in the time duration *t1*
between sampling and the beginning of the measurement. The detection
efficiency, chemical yield and sample mass are *eps*, *eta* and *m,*
respectively.

The **symbols** occurring in the equations to the left of the equation
sign are classified as **“dependent (a)”**, those of the symbols of the
right-hand sides and not occurring somewhere left of the equation sign,
as **“independent (u)” input quantities**.

It is possible to make full use of secondary equations. By doing this,
in the conventional way of uncertainty propagation it happens that
**easily overlooked covariances between dependent quantities occur**
**when using their uncertainties for propagation**. However, **this
cannot happen in UncertRadio**, because it uses only uncertainties from
independent quantities.

The **syntax for writing formula symbols** should be the **same as for
creating variable names in programming languages**. The program here
uses FORTRAN 90 internally. It is not differentiated between lower and
upper-case characters. However, it is recommended to the user to make
this differentiation for a better readability of the equations. The use
o f the underscore (\_) is allowed within symbol names, but not for the
first character of a name. A formula symbol must always begin with an
alphabetic character.

For numbers occurring in equations as well as in tables the **decimal
character** must always be a dot (decimal point). Numbers in equations,
e.g. 1. and 2. within the equations for *Fact* and *f1* shown above, are
interpreted always as double precision numbers internally by the
function parser.

**Internal functions and operators:**

All internal calculations are done with "double precision" arithmetic.

The following intrinsic arithmetic functions can be used, similarly -
but not fully identical - as in MS Excel:

sqrt(x) square root function

exp(x) exponential function

log(x), ln(x) natural logarithm

log10(x) common logarithm

A new function fd() with three parameters can be used which calculates a
decay factor averaged over the counting duration:

fd(tA,tm,xlam) = exp(-xlam*tA) \* (1 - exp(-xlam*tm)) / (xlam*tm)

This function did not exist in UR1.

Some projects may require applying an uncertainty u(x) of an input
quantity value x as an own value. A **function uval(x)** was therefore
introduced by extending the function parser. As an example, the relative
uncertainty :math:`u_{rel}(w)` can be introduced as a variable urelw as
follows:

urelw = uval(w) / w

The argument of the function uval() must be an existing single symbol
taken belonging to the symbol table. An arithmetic expression of more
than one variables is not allowed; the latter case, e.g. uval(a+b),
would mean to perform an uncertainty propagation for such an expression,
what uval() is not made for. If the value of uval(x) shall be treated as
a constant value, x must not represent a gross count or gross count
rate, because their values and uncertainties vary during calculating the
decision threshold and the detection limit.

In addition to conventional operators +, -, \* and /, for the
exponentiation one can use \*\*: a**b means a to the power of b, for
which writing a^b is also allowed.

**Notes:**

The program already contains a procedure which allows **estimating the
net counting rate as a result of weighted multi-linear Least squares
fitting applied to a measured decay curve**. This is available for decay
curves of Y-90 and may be easily applied e.g. to combined build-up/decay
curves measured in a source containing Y-90, Sr-89 and Sr-90.

*This tool is not yet in its final state. Therefore, it is necessary to
consider further applications; tips about such examples would be highly
acknowledged by the author!*

Further information: `use of Least squares fit <#URH_LSQ_EN>`__

For the **field of gamma spectrometry** there is a procedure available
allowing **the activity of a radionuclide with several gamma lines to be
estimated as a mean of single line activities.** Two methods for
calculating means are offered.

The first method is that of the **weighted mean**, for which so-called
“internal” and “external” standard deviations can be calculated. If the
values of the two standard deviations are of quite similar size, one can
draw the conclusion that the single line activity values are under
“statistical control”. This is a well-known procedure; however, it
should be noted that the use of the “external” standard deviation is not
really Bayes conform.

The second method uses a matrix-based least squares procedure instead of
formulae for the weighted mean. It is better suited for including
covariances.

This method can only be used, if the gamma lines used for calculating
the activity of the radionuclide are not interfered by gamma lines
belonging to other radionuclides.

Further information: `Activity calculation from several gamma
lines <#activity-determination-from-several-gamma-lines>`__

Editing the symbol list
-----------------------

Clicking the button “Load symbols from equations” enables the **symbol
table for editing** while the **correct syntax of the equations** is
being checked with the internal function parser. In the case of an error
corresponding warnings are given via Windows Message-box dialogs, which
are acknowledged, and one has then look for reason behind that error
message.

The symbol table consists of four text columns "**Symbol**", "**Type**",
"**Unit**" and "**Meaning**".

The symbols which were extracted automatically are characterised in the
column “Type” as dependent (a) or independent (u) quantities, where the
independent ones are listed first. **Only the independent symbols
(quantities) are the ones the measurement uncertainties of which are
considered by the program. This is the important point which prevents at
the very beginning that covariances could otherwise be inferred between
dependent symbols because they would share common (independent)
symbols.**

Within the symbol list, such input quantities for which a mean and its
uncertainty are to be derived from a data set, can be marked in the type
column with “m“ instead of “a“ (dependent) or “u“ (independent).
Furthermore, a quantity, to be derived by an equation, which shall be
treated as a parameter without uncertainty, can be defined by sitting
its type to “p“.

The columns “Unit” and “Meaning” are to be completed by the user.
However, it is recommended to this later, at least after the equations
have got their final state, because subsequent changes in the equations
may lead to a different ordering of the symbols. This may also be
postponed to a later session.

Due to subsequent changes within equations it may happen, that some of
the symbols do not occur in the present set of equations. Such symbols
are listed at the end of the symbol list, where they can be row-wise
removed by the user (see below: Notes on editing tables).

After a mouse click inside a table **mouse wheel scrolling** is
possible.

**Very important:**

Most often it is necessary to add manually such symbols at the end of
the symbol table which do not explicitly occur in the equations but are
needed within those formulae describing standard uncertainties; e.g.
counting times tm or t0 are required to calculate the standard
uncertainty of gross or background counting rates, sqrt(Rg / tm) or
sqrt(R0 / t0), respectively.

**See** `Notes on editing
tables <#within-tables-delete-rows-working-with-column-blocks>`__

Dialog “Values of decay curve”
------------------------------

The following picture gives an overview of the structure of the dialog.

+-----------------------------------------------------------------------+
| |image45|                                                             |
+=======================================================================+
+-----------------------------------------------------------------------+

Initially, in this dialog **date and time of the Y-90/Sr-90 separation**
is entered using the format “DD.MM.YYYY HH:MM:SS”, e.g. “12.06.2006
09:05:00”. It is recommended to use the 4-digit version of the year. The
program internally replaces all decimal dots by blanks and then reads
the six date/time elements format-free.

One can choose between s and min for the **basis time unit for the
counting time**. Internally, if min is chosen by the user, this is
converted to the basic unit s.

The value of the net blank counting rate shown (disabled) in this dialog
is that of the symbol *Rbl*, which has been entered by the user within
the TAB „Values, uncertainties“. It must not contain a detector
background contribution.

For the **input of the single measurement values of the decay curve** a
table with 11 columns is available, the 11 columns of which have the
following meaning; therein, only those columns shown in black color
shall be filled in by the user, while the columns with red color are
calculated by the program (see below).

+-----+-----------+----------------------------------------------------+
| Co  | Name      | Meaning                                            |
| lum |           |                                                    |
| n-# |           |                                                    |
+=====+===========+====================================================+
| *   |           |                                                    |
| Mea |           |                                                    |
| sur |           |                                                    |
| eme |           |                                                    |
| nts |           |                                                    |
| of  |           |                                                    |
| the |           |                                                    |
| gr  |           |                                                    |
| oss |           |                                                    |
| co  |           |                                                    |
| unt |           |                                                    |
| ing |           |                                                    |
| rat |           |                                                    |
| e:* |           |                                                    |
+-----+-----------+----------------------------------------------------+
| 1   | Start     | Date/time of the start of the *k*-th measurement   |
|     | date      | (input format as shown above), or,                 |
|     |           |                                                    |
|     |           | time duration between Y-90/Sr-90 separation and    |
|     |           | the start of the *k*-th measurement (as a number   |
|     |           | with decimal point); the program realizes          |
|     |           | automatically which of the two possibilities is    |
|     |           | used.                                              |
+-----+-----------+----------------------------------------------------+
| 2   | Count     | counting time of the *k*-th measurement            |
|     | time      |                                                    |
+-----+-----------+----------------------------------------------------+
| 3   | Counts    | gross counts of the *k*-th measurement             |
+-----+-----------+----------------------------------------------------+
| 4   | CountRate | calculated gross counting rate                     |
+-----+-----------+----------------------------------------------------+
| 5   | u(C       | uncertainty of the gross counting rate             |
|     | ountRate) |                                                    |
+-----+-----------+----------------------------------------------------+
| *   |           |                                                    |
| Mea |           |                                                    |
| sur |           |                                                    |
| eme |           |                                                    |
| nts |           |                                                    |
| of  |           |                                                    |
| the |           |                                                    |
| b   |           |                                                    |
| ack |           |                                                    |
| gro |           |                                                    |
| und |           |                                                    |
| co  |           |                                                    |
| unt |           |                                                    |
| ing |           |                                                    |
| rat |           |                                                    |
| e:* |           |                                                    |
+-----+-----------+----------------------------------------------------+
| 6   | Count     | counting time of the *k*-th background measurement |
|     | time      |                                                    |
+-----+-----------+----------------------------------------------------+
| 7   | Counts    | counts of the *k*-th background measurement        |
+-----+-----------+----------------------------------------------------+
| 8   | CountRate | calculated background counting rate                |
+-----+-----------+----------------------------------------------------+
| 9   | u(C       | uncertainty of the background counting rate        |
|     | ountRate) |                                                    |
+-----+-----------+----------------------------------------------------+
| *   |           |                                                    |
| Res |           |                                                    |
| ult |           |                                                    |
| ing |           |                                                    |
| val |           |                                                    |
| ues |           |                                                    |
| of  |           |                                                    |
| the |           |                                                    |
| net |           |                                                    |
| co  |           |                                                    |
| unt |           |                                                    |
| ing |           |                                                    |
| rat |           |                                                    |
| e:* |           |                                                    |
+-----+-----------+----------------------------------------------------+
| 10  | Net       | calculated net counting rate                       |
|     | CountRate |                                                    |
+-----+-----------+----------------------------------------------------+
| 11  | u(NetC    | uncertainty of the net counting rate               |
|     | ountRate) |                                                    |
+-----+-----------+----------------------------------------------------+

With the **button „Calculate count rates“** the values of gross counting
rate, background counting rate and the net counting rate as well as
their associated uncertainties are calculated in the columns 4/5, 8/9
and 10/11 (in the columns marked by red color). In this calculation, the
value of the net blank counting rate *Rbl* is considered (see above).

**Simplifying input to columns 2, 6 und 7:**

A new option within this dialog has been inserted, by which within one
of these columns a fixed value can be transferred to all necessary cells
within the selected column.

|image46|

**Tips for facilitating the input of values into the first three
columns:**

**Import from ASCII file:** If the values to be entered into the first
three columns exist as an ASCII file, also column-wise separated, each
of those columns (one after another) can be marked and copied into the
Windows clipboard and pasted into the corresponding UncertRadio dialog
column (right Mouse button: “paste”). The mouse pointer needs only to be
set into the uppermost cell of this column in advance.

**Import from Excel file:** If the values to be entered exist already in
an Excel file they can be copied with a mouse operation “copy and paste”
via Windows clipboard to the corresponding cell area in the UncertRadio
dialog. In this case, all three columns can be copied simultaneously in
one step.

Further information about `Editing of
tables <#within-tables-delete-rows-working-with-column-blocks>`__

Dialog Definition of the decay curve model
------------------------------------------

One single output quantity defined
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following picture gives an overview of the structure of the dialog.

+-----------------------------------------------------------------------+
| |image47|                                                             |
+=======================================================================+
+-----------------------------------------------------------------------+

**One single output quantity defined**

In this dialog the `described multi-linear least squares
method <#URH_LSQ_EN>`__ is presented for the case of more complex Y-90
decay-curves :math:`Y\left( t_{k} \right)` (net counting rates)

:math:`Y\left( t_{k} \right) = a_{1} \bullet X_{1}\left( t_{k} \right) + \ a_{2} \bullet X_{2}\left( t_{k} \right) + \ a_{3} \bullet X_{3}\left( t_{k} \right)`

The three terms describe different radionuclide decay contributions to
the net counting rates and are dependent on the time *t* but also on the
counting time(s).

For the case of a Y-90 decay curve the meaning of the three terms is as
follows:

+-------+--------------------------------------------------------------+
| .. ma | represents the decay of the Y-90 component; :math:`a_{1}`    |
| th::  | (in :math:`s^{- 1}`) is the Y-90 counting rate contribution  |
| a_{1} | at the time of Y/Sr separation which is to be fitted;        |
|  \bul |                                                              |
| let X |                                                              |
| _{1}\ |                                                              |
| left( |                                                              |
|  t_{k |                                                              |
| } \ri |                                                              |
| ght): |                                                              |
+=======+==============================================================+
| .. ma | represents the (practically) constant counting rate          |
| th::  | contribution from an impurity due to long-lived unidentified |
| a_{2} | radionuclide (half-live assumed to be infinitely,            |
|  \bul | :math:`X_{2}\left( t_{k} \right)` is then set equal to 1).   |
| let X | An example of this is Th-234; if this known or identified    |
| _{2}\ | its half-live Hwzlong can be specified (in the TAB “Values,  |
| left( | uncertainties“);\ :math:`{\ a}_{2}` (in :math:`s^{- 1}`)     |
|  t_{k | gives the size of this contribution (to be fitted).          |
| } \ri |                                                              |
| ght): |                                                              |
+-------+--------------------------------------------------------------+
| :ma   | represents the decay of the relatively short-lived Ac-228    |
| th:`a | which can occur as impurity in the Y oxalate source; this    |
| _{3}  | term may also be used if the presence of short-lived         |
| \bull | contaminations of the counting source by short-lived Radon   |
| et X_ | decay products; :math:`a_{3}` (in :math:`s^{- 1}`) gives the |
| {3}\l | size of this contribution;                                   |
| eft(  |                                                              |
| t_{k} |                                                              |
|  \rig |                                                              |
| ht)`: |                                                              |
+-------+--------------------------------------------------------------+

The user can choose whether the second and/or the third term shall be
used in the model.

Furthermore, it may be chosen whether the fitting shall be done with
statistical weighting with the inverse variances of the net counting
rates - or non-weighted. The internal use of covariances of measured net
counting rates may be de-selected for testing.

The type of the fitting procedure can be selected from four variants:
WLS, PLSQ, PMLE und WTLS. The more complex but more time-consuming total
least squares procedure (WTLS) is able to take uncertainties of the
:math:`X_{i}\left( t_{k} \right)` values and covariances between them
into account. See also: `Chi-square options <#chi-square-options>`__

A checkbox within this dialog allows to select instead of the simpler
least squares analysis (WLS) the more complex but also more
time-consuming general least squares procedure (WTLS). The latter is
able to consider directly also uncertainties of the
:math:`X_{i}\left( t_{k} \right)` values and covariances between them.

In most cases, the term functions Xi(t) between different measurements
only differ by the time difference to the time of the Sr/Y separation
(parameter t: tstart). Therefore, these functions Xi(t) needed to be
given only once, for the first measurement (in case of more than one
counting channel: for the first measurement of each counting channel).

However, values of the counting efficiencies contained therein may now
differ from measurement to measurement. In this case, which can be
selected by a new checkbox, all term functions for each measurement have
to be input explicitly. This increases the length of the argument list
of the Linfit Call by additional values of counting efficiencies;
**since version 2.4.24** these detection efficiency parameters need no
longer be specified explicitly within the Linfit call. Look at the new
example project Sr89-Sr90_IAEA_AQ-27_2013_V2_EN.txp which demonstrates
this.

In a text field of the dialog the equations for the three functions
:math:`X_{i}\left( t_{k} \right)` can be defined. For the above example
of the analysis of a Y-90 decay-curve they have to be defined as
follows:

X1 = (1. - exp(-log(2.)*tmess/HwzY90)) / (log(2.)*tmess/HwzY90) \*
exp(-log(2.)*tstart/HwzY90)

X2 = (1. - exp(-log(2.)*tmess/Hwzlong)) / (log(2.)*tmess/Hwzlong) \*
exp(-log(2.)*tstart/Hwzlong)

X3 = (1. - exp(-log(2.)*tmess/HwzAc228)) / (log(2.)*tmess/HwzAc228) \*
exp(-log(2.)*tstart/HwzAc228)

Here, tmess and tstart are counting time and the time durations between
Y-90/Sr-90 separation and the starts of the measurements, respectively.
In fact, both variables are arrays and their values for each single
measurement may be defined in a special dialog. Hwzxxx are the
radionuclide specific half-lives. **The formulae given above take
radioactive decay during the measurements into account**.

With using the new decay function fd() the above equations are shorter:

X1 = fd(tstart, tmess, log(2)/HwzY90)

X2 = fd(tstart, tmess, log(2)/Hwzlong)

X3 = fd(tstart, tmess, log(2)/HwzAc228)

Extension to two or three output quantities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Extension to two or three output quantities**

By the simultaneous measurement of for instance Sr-90, Sr-89, and may be
also of Sr-85, with a LSC counter the contributions of these
radionuclides to the counting rates in two or three counting channels
(energy regions), designated as A, B and C, are determined. Further
details may be taken from the report AKU (2008), which is dealing with
modern methods of the Sr-89/Sr-90 determination.

The number of counting channels (nchs) can be selected in the dialog. In
the present case up to three decay curves
:math:`Y_{A}\left( t_{k} \right)`, :math:`Y_{B}\left( t_{k} \right)` and
:math:`Y_{C}\left( t_{k} \right)` have to be considered. Then up to
three model equations are used instead of only one:

:math:`Y_{A}\left( t_{k} \right) = a_{1} \bullet X_{A1}\left( t_{k} \right) + \ a_{2} \bullet X_{A2}\left( t_{k} \right) + \ a_{3} \bullet X_{A3}\left( t_{k} \right)`

:math:`Y_{B}\left( t_{k} \right) = a_{1} \bullet X_{B1}\left( t_{k} \right) + \ a_{2} \bullet X_{B2}\left( t_{k} \right) + \ a_{3} \bullet X_{B3}\left( t_{k} \right)`

:math:`Y_{C}\left( t_{k} \right) = a_{1} \bullet X_{C1}\left( t_{k} \right) + \ a_{2} \bullet X_{C2}\left( t_{k} \right) + \ a_{3} \bullet X_{C3}\left( t_{k} \right)`

Now, the fitting parameters :math:`a_{i}\ `\ represent activities
instead of counting rates in the first example; see above: one single
output quantity. These are automatically inserted by the program into
the list of symbols as new symbols FITP1, FITP2 and FITP3.

Within the program, this case is reduced to that of one single decay
curve by concatenating the three fields of counting rates (in the order
A, B and C). The same applies to the independent decay functions.

Counting rates:

:math:`Y\left( t_{k} \right) = \left\{ \begin{array}{r}
Y_{A}\left( t_{k} \right) \\
Y_{B}\left( t_{k} \right) \\
Y_{C}\left( t_{k} \right)
\end{array} \right\}`

Decay functions: Input within the dialog field for the terms:

+---------------------------------------+------------------------------+
| .. math:: X_{A1}\left( t_{k} \right)  | .. math:: X1 = \ldots        |
+=======================================+==============================+
| .. math:: X_{A2}\left( t_{k} \right)  | .. math:: X2 = \ldots        |
+---------------------------------------+------------------------------+
| .. math:: X_{A3}\left( t_{k} \right)  | .. math:: X3 = \ldots        |
+---------------------------------------+------------------------------+
| .. math:: X_{B1}\left( t_{k} \right)  | .. math:: X4 = \ldots        |
+---------------------------------------+------------------------------+
| .. math:: X_{B2}\left( t_{k} \right)  | .. math:: X5 = \ldots        |
+---------------------------------------+------------------------------+
| .. math:: X_{B3}\left( t_{k} \right)  | .. math:: X6 = \ldots        |
+---------------------------------------+------------------------------+
| .. math:: X_{C1}\left( t_{k} \right)  | .. math:: X7 = \ldots        |
+---------------------------------------+------------------------------+
| .. math:: X_{C2}\left( t_{k} \right)  | .. math:: X8 = \ldots        |
+---------------------------------------+------------------------------+
| .. math:: X_{C3}\left( t_{k} \right)  | .. math:: X9 = \ldots        |
+---------------------------------------+------------------------------+

For a better understanding of this application, the reader is referred
to the example project *DWD-LSC-3kanal-V2.txp*, which corresponds to a
quite detailed presentation of the equations in the report AKU (2008;
page 160). For this application, the nine decay functions are defined as
follows:

   X1 = eSr89A \* (1. - exp(-lamSr89*tmess)) / (lamSr89*tmess) \*
   exp(-lamSr89*(tAS+tstart))

   X2 = eSr90A \* (1. - exp(-lamSr90*tmess)) / (lamSr90*tmess) \*
   exp(-lamSr90*(tAS+tstart)) +eY90A \* &

   lamY90/(tmess*(lamY90-lamSr90)) \*(
   -exp(-lamSr90*(tAS+tstart))/lamSr90*(exp(-lamSr90\* &

   tmess)-1.)+exp(-lamY90*(tAS+tstart))/lamY90*(exp(-lamY90*tmess)-1.) )

   X3 = eSr85A \* (1. - exp(-lamSr85*tmess)) / (lamSr85*tmess) \*
   exp(-lamSr85*(tAS+tstart))

   X4 = eSr89B \* (1. - exp(-lamSr89*tmess)) / (lamSr89*tmess) \*
   exp(-lamSr89*(tAS+tstart))

   X5 = eSr90B \* (1. - exp(-lamSr90*tmess)) / (lamSr90*tmess) \*
   exp(-lamSr90*(tAS+tstart)) +eY90B \* &

   lamY90/(tmess*(lamY90-lamSr90)) \*(
   -exp(-lamSr90*(tAS+tstart))/lamSr90*(exp(-lamSr90\* &

   tmess)-1.)+exp(-lamY90*(tAS+tstart))/lamY90*(exp(-lamY90*tmess)-1.) )

   X6 = eSr85B \* (1. - exp(-lamSr85*tmess)) / (lamSr85*tmess) \*
   exp(-lamSr85*(tAS+tstart))

   X7 = eSr89C \* (1. - exp(-lamSr89*tmess)) / (lamSr89*tmess) \*
   exp(-lamSr89*(tAS+tstart))

   X8 = eSr90C \* (1. - exp(-lamSr90*tmess)) / (lamSr90*tmess) \*
   exp(-lamSr90*(tAS+tstart)) +eY90C \* &

   lamY90/(tmess*(lamY90-lamSr90)) \*(
   -exp(-lamSr90*(tAS+tstart))/lamSr90*(exp(-lamSr90\* &

   tmess)-1.)+exp(-lamY90*(tAS+tstart))/lamY90*(exp(-lamY90*tmess)-1.) )

   X9 = eSr85C \* (1. - exp(-lamSr85*tmess)) / (lamSr85*tmess) \*
   exp(-lamSr85*(tAS+tstart))

Herein, decay constants lamNuclide instead of half-lives are used. The
symbols beginning with e represent for the considered radionuclides
their detection probabilities in the counting channels A, B and C.

With using the new decay function fd() the above equations are shorter:

   X1 = eSr89A \* fd(tAS+tstart,tmess,lamSr89)

   X2 = eSr90A \* fd(tAS+tstart,tmess,lamSr90) + &

   eY90A \* lamY90/(lamY90-lamSr90) \* ( fd(tAS+tstart,tmess,lamSr90) -
   fd(tAS+tstart,tmess,lamY90) )

   X3 = eSr85A \* fd(tAS+tstart,tmess,lamSr85)

   X4 = eSr89B \* fd(tAS+tstart,tmess,lamSr89)

   X5 = eSr90B \* fd(tAS+tstart,tmess,lamSr90) + &

   eY90B \* lamY90/(lamY90-lamSr90) \* ( fd(tAS+tstart,tmess,lamSr90) -
   fd(tAS+tstart,tmess,lamY90) )

   X6 = eSr85B \* fd(tAS+tstart,tmess,lamSr85)

   X7 = eSr89C \* fd(tAS+tstart,tmess,lamSr89)

   X8 = eSr90C \* fd(tAS+tstart,tmess,lamSr90) + &

   eY90C \* lamY90/(lamY90-lamSr90) \* ( fd(tAS+tstart,tmess,lamSr90) -
   fd(tAS+tstart,tmess,lamY90) )

   X9 = eSr85C \* fd(tAS+tstart,tmess,lamSr85)

The contribution of the fourth radionuclide, Y-90, which is in-growing
from the decay of Sr-90, is attributed for by additional terms with
eY90X in the expressions for X2, X5 and X8.

If the same calibration activity *A*\ :sub:`cal` of a radionuclide was
used for the efficiency calibration of the two to three energy windows,
these efficiencies are correlated. Their covariances, pair-wise given by

:math:`cov\left( \varepsilon_{N},\ \varepsilon_{M} \right) = \varepsilon_{N}\ \varepsilon_{M}\ u_{rel}^{2}(A_{cal})`

are to be inserted for each pair of energy windows N and M, separately
for the present radionuclides, in the covariance grid under the TAB
"Values, uncertainties".

Organizing of the Xi Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

a) number of Xi formulae =

(number of counting channels) x (number of applied output quantities)

(applied output quantities: fitting parameters, for which “fit“ or
“fixed“ was selected)

or

b) number of Xi formulae =

(number of measurements) x (number of Xi formulae) x

x (number of counting channels)

(if a formulae (Xi) is defined explicitly for each of the measurements)

The **prescribed sequence** of the Xi formulae is indicated in the
following two examples. It formally corresponds to the sequence which
would be obtained by an SQL statement

“ORDER BY counting channel, number of measurements, number of output
quantity”.

Example 1: Case a): 2 counting channels, 4 measurements, 3 output
quantities; the Xi(t) differ between measurements

+---------------+--------------------+------------+----+--------------+
| counting      | measurement No.    | index of X |    | running No.  |
| channel       |                    | i (t)      |    |              |
+===============+====================+============+====+==============+
| 1             | 1                  | 1          |    | 1            |
+---------------+--------------------+------------+----+--------------+
| 1             | 1                  | 2          |    | 2            |
+---------------+--------------------+------------+----+--------------+
| 1             | 1                  | 3          |    | 3            |
+---------------+--------------------+------------+----+--------------+
| 1             | 2                  | 1          |    | 4            |
+---------------+--------------------+------------+----+--------------+
| 1             | 2                  | 2          |    | 5            |
+---------------+--------------------+------------+----+--------------+
| 1             | 2                  | 3          |    | 6            |
+---------------+--------------------+------------+----+--------------+
| 1             | 3                  | 1          |    | 7            |
+---------------+--------------------+------------+----+--------------+
| 1             | 3                  | 2          |    | 8            |
+---------------+--------------------+------------+----+--------------+
| 1             | 3                  | 3          |    | 9            |
+---------------+--------------------+------------+----+--------------+
| 1             | 4                  | 1          |    | 10           |
+---------------+--------------------+------------+----+--------------+
| 1             | 4                  | 2          |    | 11           |
+---------------+--------------------+------------+----+--------------+
| 1             | 4                  | 3          |    | 12           |
+---------------+--------------------+------------+----+--------------+
| 2             | 1                  | 1          |    | 13           |
+---------------+--------------------+------------+----+--------------+
| 2             | 1                  | 2          |    | 14           |
+---------------+--------------------+------------+----+--------------+
| 2             | 1                  | 3          |    | 15           |
+---------------+--------------------+------------+----+--------------+
| 2             | 2                  | 1          |    | 16           |
+---------------+--------------------+------------+----+--------------+
| 2             | 2                  | 2          |    | 17           |
+---------------+--------------------+------------+----+--------------+
| 2             | 2                  | 3          |    | 18           |
+---------------+--------------------+------------+----+--------------+
| 2             | 3                  | 1          |    | 19           |
+---------------+--------------------+------------+----+--------------+
| 2             | 3                  | 2          |    | 20           |
+---------------+--------------------+------------+----+--------------+
| 2             | 3                  | 3          |    | 21           |
+---------------+--------------------+------------+----+--------------+
| 2             | 4                  | 1          |    | 22           |
+---------------+--------------------+------------+----+--------------+
| 2             | 4                  | 2          |    | 23           |
+---------------+--------------------+------------+----+--------------+
| 2             | 4                  | 3          |    | 24           |
+---------------+--------------------+------------+----+--------------+

Example 2: case b), like example 1, but the Xi(t) do NOT differ between
measurements:

+---------------+----------------------+--------------+-----+---------+
| counting      | measurement No.      | index of X i |     | running |
| channel       |                      | (t)          |     | No.     |
+===============+======================+==============+=====+=========+
| 1             | 1                    | 1            |     | 1       |
+---------------+----------------------+--------------+-----+---------+
| 1             | 1                    | 2            |     | 2       |
+---------------+----------------------+--------------+-----+---------+
| 1             | 1                    | 3            |     | 3       |
+---------------+----------------------+--------------+-----+---------+
| 2             | 1                    | 1            |     | 4       |
+---------------+----------------------+--------------+-----+---------+
| 2             | 1                    | 2            |     | 5       |
+---------------+----------------------+--------------+-----+---------+
| 2             | 1                    | 3            |     | 6       |
+---------------+----------------------+--------------+-----+---------+

One parameter excluded from fitting
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**One parameter excluded from fitting**

There are three options for the terms
:math:`a_{j} \bullet X_{j}\left( t_{k} \right)` :

|image48|

The option “fixed“ became necessary by using a Sr-85 tracer within the
Sr-90/Sr-89 analysis such that the Sr-85 activity is not subject to
fitting but the chemical Sr yield is determined by an independent gamma
measurement of Sr-85. This means that the Sr-85 contribution to the beta
counting rates can be calculated separately. Select the Sr-85 option to
“fix“ for enabling this case.

As the fitting routine cannot include the uncertainty of the Sr-85
activity (or its count rate), a special treatment was inferred. At
first, for each of the gross count rate measurements the Sr-85 beta
counting rate (and its uncertainty) is calculated and subtracted from
the already available net counting rate
:math:`R_{n}\left( t_{i} \right)`.

:math:`R_{nk}\left( t_{i} \right) = R_{n}\left( t_{i} \right) - X_{3}\left( t_{i},{A_{85g}{,\ \varepsilon}_{85b},\lambda}_{85} \right)`

The symbols are: :math:`A_{85g},` the Sr-85 activity obtained by
gamma-spectrometry;\ :math:`\varepsilon_{85b}` , the Sr-85 beta counting
efficiency and :math:`\lambda_{85}` the Sr-85 decay constant.

The function :math:`X_{3}` is determined by that equation, which the
user defines as equation for X3 within the dialog for setting up a decay
curve model. An example:

X3 = ASr85_Gam \* eSr85 \* (1. - exp(-lamSr85*tmess)) / (lamSr85*tmess)
\*

exp(-lamSr85*(tAS+tstart))

The associated fitting parameter :math:`a_{3}` (to be fixed) is
internally set to the value 1. Collecting the input quantities
:math:`A_{85g}`, :math:`\varepsilon_{85b}` and :math:`\lambda_{85}` into
a vector :math:`z`, i.e.
:math:`z = \left( A_{85g},\varepsilon_{85b},\lambda_{Sr85} \right)^{T}`,
allows to calculate the covariance matrix components needed for
uncertainty propagation as follows:

diagonal values:

:math:`u^{2}\left( R_{nk}\left( t_{i} \right) \right) = u^{2}\left( R_{n}\left( t_{i} \right) \right) + \sum_{j = 1}^{3}\left( \frac{\partial R_{nk}\left( t_{i} \right)}{\partial z_{j}} \right)^{2}u^{2}\left( z_{j} \right)`

non-diagonal values:

:math:`u\left( R_{nk}\left( t_{i} \right),R_{nk}\left( t_{k} \right) \right) = {u\left( R_{nk}\left( t_{k} \right),R_{nk}\left( t_{i} \right) \right) = u}^{2}\left( R_{BG} \right) + \sum_{j = 1}^{3}{\frac{\partial R_{nk}\left( t_{i} \right)}{\partial z_{j}}\ \frac{\partial R_{nk}\left( t_{k} \right)}{\partial z_{j}}}u^{2}\left( z_{j} \right)`

The first term within the last equation does only occur if for
calculating the net count rates :math:`R_{n}\left( t_{i} \right)` always
the same value of the background contribution is used (abbreviated here
as :math:`R_{BG}`). The partial derivatives are calculated numerically.

The remaining unknown components for Sr-90 and Sr-89 are fitted to these
Sr-85-corrected net counting rates (including their covariance matrix).

The symbols collected above into the vector :math:`z` must be included
in the list of arguments of the call to Linfit, for example (note, that
the equation for cSr85 is a dummy, i.e. only a place-holder):

   cSr90 = Fitp1 \* PhiSr90

   cSr89 = Fitp2 \* PhiSr89

   cSr85 = Fitp3 \* 1

   rd = Linfit(1, Rbl, **ASr85_Gam**, **eSr85**, eSr90, eSr89, eY90,
   **lamSr85**, lamSr90,

   lamSr89, lamY90, tmess, tstart )

   phiSr90 = 1 / (etaSr*Vol) \* exp(lamSr90 \* (tBS - tAS))

   phiSr89 = 1 / (etaSr*Vol) \* exp(lamSr89 \* (tBS - tAS))

   X1 = eSr90 \* (1. - exp(-lamSr90*tmess)) / (lamSr90*tmess) \*
   exp(-lamSr90*(tAS+tstart)) + &

   eY90 \* lamY90/(tmess*(lamY90-lamSr90)) \* &

   ( -exp(-lamSr90*(tAS+tstart))/lamSr90*(exp(-lamSr90*tmess)-1.) &

   +exp(-lamY90*(tAS+tstart))/lamY90*(exp(-lamY90*tmess)-1.) )

   X2 = eSr89 \* (1. - exp(-lamSr89*tmess)) / (lamSr89*tmess) \*
   exp(-lamSr89*(tAS+tstart))

   X3 = ASr85_Gam \* eSr85 \* (1. - exp(-lamSr85*tmess)) /
   (lamSr85*tmess) \* exp(-lamSr85*(tAS+tstart))

**Since version 2.4.24** the equation for rd shall be shortened to:

rd = Linfit(1, Rbl, tmess, tstart )

Viewing the result from the LSQ fit to the decay curve
------------------------------------------------------------

As a result, one obtains a table displayed by the programs internal
editor (in this case no editing allowed) which has the following
structure.

Result of decay curve analysis (with covariances): Method: PLSQ
LinFit(t) = a1*X1(t) + a2*X2(t) + a3*X3(t)

i t X1(t) X2(t) X3(t) NetRate rUnc. LinFit relDev uTest

(m) (cps) (%) (cps) (%)

-----------------------------------------------------------------------------------

1 433.00 0.83176 0.00000 0.00000 0.0017278 23.33 \| 0.0016381 5.5 0.2

2 1633.00 0.67000 0.00000 0.00000 0.0013944 28.49 \| 0.0013195 5.7 0.2 3
2833.00 0.53970 0.00000 0.00000 0.0009500 40.99 \| 0.0010629 -10.6 -0.3

4 4033.00 0.43474 0.00000 0.00000 0.0009361 41.57 \| 0.0008562 9.3 0.2

5 5237.00 0.34994 0.00000 0.00000 0.0006306 60.85 \| 0.0006892 -8.5 -0.1

6 6437.00 0.28189 0.00000 0.00000 0.0006722 57.19 \| 0.0005552 21.1 0.3

7 7637.00 0.22707 0.00000 0.00000 0.0006306 60.85 \| 0.0004472 41.0 0.5

8 8837.00 0.18291 0.00000 0.00000 0.0002833 133.18 \| 0.0003602 -21.3
-0.2

9 10037.00 0.15710 0.00000 0.00000 0.0004382 103.13 \| 0.0003094 41.6
0.3

----------------------------------------------------------------------------------

LinFit: a1= 0.0019694 a2= 0.0000000 a3= 0.0000000 (given in cps !) ra1=
16.240 ra2= 0.000 ra3= 0.000 (given in % !) CHi2R= 8.481E-02 Prob=
0.000272 Prob= 0.000000 Prob= 0.000000 (t-test-signific. !)

The table columns are:

-  No. of measurement i

-  time duration between the Y-90/Sr-90 separation and the start time of
   the i-th measurement, is given in this example in m

-  decay factors of the defined components dependent on t; in this
   example, only the decay of Y-90 is considered; 0.000 indicates that
   this decay component has not been defined/used

-  :math:`NetRate`, net counting rates, in 1/s

-  relative standard uncertainty, in %

-  :math:`LinFit`, the value obtained from fitting the model for the net
   counting rate, in 1/s

-  relDev., relative deviation
   :math:`(NetRate - LinFit)/LinFit \bullet 100`, in %

-  an u-test value, defined as
   :math:`(NetRate - LinFit)/\sqrt{u^{2}(NetRate) + u^{2}(LinFit)}`.
   Absolute values being larger than 2, e.g. may indicate deviations
   from the model

After the table two rows follow showing the parameter values
:math:`a_{i}` and their relative standard uncertainties :math:`{ra}_{i}`
obtained from the LSQ fitting. The first parameter, :math:`a_{1}`, gives
the value of the Y-90 net counting rate being decay corrected to the
time of the Y-90/Sr-90 separation. Chi2R is the value obtained for the
reduced Chi-square.

**After closing** the editor window, **the model itself can again be
edited** under the menu item “Edit->Decay curve->model of the decay
curve” or with corresponding icon |image49| if the first result is
considered for improvement.

Calculation of the weighted mean and its standard uncertainty
-------------------------------------------------------------

The weighted mean :math:`\overline{A}` of activities :math:`A_{i}` of
the individual gamma lines is calculated according to the following
equation:

:math:`\overline{A} = \frac{\sum_{i = 1}^{n}{\ \frac{A_{i}}{u^{2}\left( A_{i} \right)}}}{\sum_{i = 1}^{n}{\ \frac{1}{u^{2}\left( A_{i} \right)}}}`
(1)

The standard uncertainty of the weighted mean is calculated as follows:

:math:`u\left( \overline{A} \right) = \sqrt{\frac{1}{\sum_{i = 1}^{n}{\ \frac{1}{u^{2}\left( A_{i} \right)}}}} = u_{int}\left( \overline{A} \right)`
(2)

This is termed as “internal standard” deviation. It only considers the
uncertainties of the individual activities and is Bayes compliant.

It may, however, happen, that the individual values :math:`A_{i}` show
deviations are larger than would be expected from the uncertainties
:math:`u\left( A_{i} \right)`. In order to consider also an additional
uncertainty component due to these “external” influences, the so-called
“external standard deviation” is often used being defined as follows:

:math:`u_{ext}\left( \overline{A} \right) = \sqrt{\frac{\sum_{i = 1}^{n}{\ \frac{\left( A_{i} - \overline{A} \right)^{2}}{u^{2}\left( A_{i} \right)}}}{(n - 1)\sum_{i = 1}^{n}{\ \frac{1}{u^{2}\left( A_{i} \right)}}}}`
(3)

Note, that this type of standard deviation is, however, no longer Bayes
compliant.

The factors :math:`1/u^{2}\left( A_{i} \right)` within the sums
contained in the equations (1-3) represent statistical weights. These
must be considered as being constant. Nevertheless, before they are
applied, they are calculated from other variable values
:math:`x_{in}(j)`, which also contribute to the :math:`A_{i}`. If, after
having calculated Eq. (1), followed by a numerical uncertainty
propagation for :math:`\overline{A}` with respect to the
:math:`x_{in}(j)` by using differences quotients follows, the values
:math:`u^{2}\left( A_{i} \right)` must not be modified. Under this
constraint the uncertainty propagation for Eq. (1) directly yields the
uncertainty given by Eq. (2). This condition is considered since version
2.4.05. The values :math:`u^{2}\left( A_{i} \right)` may only be
recalculated within the iterations for calculating the decision
threshold and the detection limit, once per single iteration step.

**UncertRadio is calculating both, internal and external standard
uncertainty but for further internal calculations it makes only use of
the internal standard uncertainty.** The external standard deviation is
shown only for information. The reason for doing that is that for the
user it might be an useful information that the external standard
deviation is considerably larger than the internal standard deviation.
The latter can give advice about possible sources of errors which should
be considered.

Covariances between peak efficiency values, taken from the same
efficiency curve, are part of the covariance matrix **U\ x** of the
input values **x**:

:math:`cov\left( A_{m},A_{k} \right) = \frac{A_{m}}{\varepsilon_{m}}\frac{A_{k}}{\varepsilon_{k}} \bullet cov\left( \epsilon_{m},\epsilon_{k} \right)`

**Important note**: According to Cox et al. (2006b), the equations for
the weighted mean and its uncertainty given here can only be considered
as “good” approximations, if covariances exist between the individual
activity values. In such a case, instead, a least-squares procedure for
the mean is to be applied. The corresponding procedure is described
`below <#least-squares-calculation-of-a-weighted-mean-and-its-standard-uncertainty>`__.

Least-squares calculation of a weighted mean and its standard uncertainty
-------------------------------------------------------------------------

UncertRadio applies a matrix-based procedure described in section
„\ `Mathematics of the linear LSQ curve
fitting.. <#mathematics-of-the-linear-lsq-curve-fitting-with-correlated-measured-values>`__\ “,
**x** = **A** **y.**

The single activity values :math:`A_{i}\ `\ are taken as elements of the
vector **x**. The design matrix **A** = (1,1,…,1)\ :sup:`T` in this case
has only one column the elements of which all are equal to1; **y**
reduces to a vector consisting of only one value, the desired weighted
mean.

For applying this procedure, the quadratic covariance matrix **U\ x** is
needed. UncertRadio assembles all the data into the corresponding
algebraic elements and calculates also the required elements of the
covariance matrix as already `described for the weighted
mean <#calculation-of-the-weighted-mean-and-its-standard-uncertainty>`__.
The matrix **U\ y** consists of only one element, the variance
associated with weighted mean.

Approach of calculating Decision threshold and Detection limit for Gamspk1
--------------------------------------------------------------------------

The iterative calculation of the Decision threshold and the Detection
limit is performed after the mean :math:`\overline{A}` has been
calculated from the single values :math:`A_{i}`. The iteration is done
by variation of the mean where a varied value is :math:`A'`. Then, all
single values of the source activity :math:`A_{i}` are replaced by the
new (fictive) value of :math:`A'`. From the equation defining
:math:`A_{i}` :

:math:`A_{i} = R_{ni}\frac{f_{att,i\ } \bullet \ f_{coinsu,i}}{\epsilon_{i}{\  \bullet \ p}_{\gamma i}\ }`
(1)

one obtains by inverting this equation

:math:`R_{ni} = A_{i}\left( \frac{\epsilon_{i}{\  \bullet \ p}_{\gamma i}}{f_{att,i\ } \bullet \ f_{coinsu,i}}\ \  \right)`
(2a)

and from this with the replacement :math:`A_{i} = A'\ ` an equation for
the (fictive) net counting rates associated with the varied value
:math:`A'` :

:math:`R_{ni}^{'} = A'\left( \frac{\epsilon_{i}{\  \bullet \ p}_{\gamma i}}{f_{att,i\ } \bullet \ f_{coinsu,i}} \right)`
(2b)

The aim is now to determine the uncertainties of the :math:`R_{ni}^{'}`,
then, via uncertainty propagation in accordance with Eq. (1), the
uncertainties of the single activity values :math:`A_{i} = A'\ `\ and,
finally, with the chosen method for the mean to derive the uncertainty
:math:`u(A')` of the (iterated or fictive) mean value :math:`A'`.

The following ansatz (a separation) is chosen for the uncertainties
:math:`u\left( R_{ni}^{'} \right)\ `\ (`see
also <#dialog-values-from-spectrum-evaluation>`__):

:math:`u^{2}\left( R_{ni} \right) = \left\lbrack \frac{R_{ni}}{tlive} \right\rbrack + \frac{R_{T}}{tlive}f_{B} + \frac{R_{bg}}{tlive} + u^{2}\left( R_{bg} \right)`
(3)

Herein, only the first term is related directly to the contribution from
the sample activity. The remaining terms represent uncertainty
contributions of those parameters which characterize the background of
the *i*-th gamma line including also a contribution from a “peak in the
background”.

By using the last equation now with the equations (2b), (3) and (1) the
(iterated or fictive) activities of the single gamma lines and their
uncertainties can be determined as indicated already above. After these
calculations those values are available which are necessary to go to the
next iteration step and to test also for convergence of the iteration.

**Important note:**

„External“ influences may exist leading to calculated values
:math:`A_{i}` of the single gamma lines which may exhibit a spreading
which may be larger than to be expected from the uncertainties of single
values.

   This effect can be found with the **weighted mean** if the “external”
   is significantly larger than the “internal” standard uncertainty or
   the value of the “reduced Chi-square” significantly larger than one
   is.

   In the case of the **arithmetic mean with additive correction** this
   may be inferred if the correction :math:`C` and particularly its
   uncertainty :math:`u(C)` lead to a significant shift of the results
   compared to the arithmetic mean (uncorrected) or its uncertainty.

These influences usually are not considered in the evaluation model
given by Eq. (1). Determining Decision threshold and Detection limit
requires iteration of the activity values. The inversion of that
equation, i.e. calculating the net count rates :math:`R_{ni}^{'}`
according to Eq. (1b) to be expected for a given (iterated) value of the
activity\ :math:`\ A'`, leads directly to the elimination of that
“external” influence. Then, from the obtained net counting rates
:math:`R_{ni}^{'}` single activity values result from Eq. (1) having all
the same identical value :math:`A'`, i.e. their spreading is equal to
zero!

This means for the calculation of Decision threshold and Detection limit
that the external effect which may have been found from the primary
evaluation of the output quantity in this latter case does not come into
effect. Insofar, the usability of the external standard deviation with
the weighted mean or with the NIST-2004 method is low, at least
regarding Decision threshold and Detection limit.

Dialog Values from spectrum evaluation
--------------------------------------

Within a table in this dialog for each of the gamma lines measured
values of the used quantities (symbols) and their uncertainties can be
input line-by-line.

It is emphasized that it is expected in the case of a naturally
occurring radionuclide that from the net counting rate also the net
counting rate of the corresponding background peak has already been
subtracted. **This must be considered also in advance when estimating
the uncertainty of the net counting rate.** Under this assumption the
input of the net counting rate of the background peak is not necessary.

It is assumed that all necessary values and their uncertainties can be
taken from the evaluation report produced by the gamma spectrometry
software.

The following picture gives an overview of the structure of the dialog.

+-----------------------------------------------------------------------+
| |image50|                                                             |
+=======================================================================+
+-----------------------------------------------------------------------+

The measured values are:

+---------------+----------------------------------------+---+---------+
| **Symbol      | **Meaning:**                           |   | **Sym   |
| names**       |                                        |   | bols:** |
|               |                                        |   |         |
| **in the      |                                        |   |         |
| dialog:**     |                                        |   |         |
+---------------+----------------------------------------+---+---------+
| Rnet          | net counting rate of the gamma line    |   | ..      |
|               |                                        |   |  math:: |
|               | at energy :math:`E_{i}`, in            |   |  R_{ni} |
|               | :math:`s^{- 1}`                        |   |         |
+---------------+----------------------------------------+---+---------+
| RT            | counting rate of the integrated        |   | .       |
|               | Compton-background in the region       |   | . math: |
|               | 1,7×Fwhm of peak *i* at energy         |   | : R_{T} |
|               | :math:`E_{i}`, in :math:`s^{- 1}`      |   |         |
+---------------+----------------------------------------+---+---------+
| Rbg           | net counting rate of a peak at energy  |   | ..      |
|               | :math:`E_{i}\ `\ in a separately       |   |  math:: |
|               | measured background spectrum, in       |   |  R_{bg} |
|               | :math:`s^{- 1}`                        |   |         |
+---------------+----------------------------------------+---+---------+
| effi          | full energy peak efficiency at energy  |   | .       |
|               | :math:`E_{i}`                          |   | . math: |
|               |                                        |   | : \epsi |
|               |                                        |   | lon_{i} |
+---------------+----------------------------------------+---+---------+
| pgamm         | gamma emission probability of the line |   | .. ma   |
|               | *i*                                    |   | th:: {\ |
|               |                                        |   |  p}_{\g |
|               |                                        |   | amma i} |
+---------------+----------------------------------------+---+---------+
| f_att         | self-attenuation correction for energy |   | .. math |
|               | :math:`E_{i}` ;                        |   | :: f_{a |
|               | it is used in its multiplicative form; |   | tt,i\ } |
+---------------+----------------------------------------+---+---------+
| f_coin        | coincidence summing correction of      |   | .       |
|               |                                        |   | . math: |
|               | the line at energy :math:`E_{i}` ; it  |   | : f_{co |
|               | is used in its multiplicative form;    |   | insu,i} |
+---------------+----------------------------------------+---+---------+

The units of the net counting rates can be given in cps
(:math:`s^{- 1}`) or in cpm (:math:`\min^{- 1}`).

The measured values have to be inserted as absolute values, not as
relative values. For inserting their associated uncertainties, the radio
buttons allow to choose between values in % or absolute values.

**Note:** Seven radio buttons were available up to the version 2.4.18;
since version 2.4.19 only 5 can be used. The radio buttons for effi and
pgamm were removed because their values can be supplied as absolute
values only. If earlier projects are loaded, which were still defined
for 7 radio buttons, the data are internally adapted to 5 buttons
achieved by dividing the values of effi and pgamm by 100 if their
buttons were not given as “abs”.

The activities *A*\ :sub:`i` of the individual lines are calculated as
follows:

:math:`A_{i} = R_{ni}\frac{f_{att,i\ } \bullet \ f_{coinsu,i}}{\epsilon_{i}{\  \bullet \ p}_{\gamma i}\ }`

Furthermore, the uncertainties of the net count rates are calculated by
the program according to the following equation:

:math:`u^{2}\left( R_{ni} \right) = \frac{R_{ni}}{tlive} + \frac{R_{T}}{tlive}f_{B} + \frac{R_{bg}}{tlive} + u^{2}\left( R_{bg} \right)`

Herein, :math:`f_{B}` is a factor which depends on how the net counting
rate *R*\ :sub:`ni` of the peak has been evaluated. In the case of the
“classical” total peak area (TPA) method, it is given by:

:math:`f_{B} = \left( 1 + \frac{b}{2L} \right)`

*b* is the width of the peak at its base, e.g. *b*\ =1,7xFwhm, and *L*
is the number of channels which are used on both sides of the peak for
determining the count rate of the background continuum.

If, however, *R*\ :sub:`ni` is determined by the method of peak fitting,
:math:`f_{B}` may be approximated by a fixed value being „slightly
larger than 1“. Then, this factor depends on the method used for peak
fitting; it may be estimated by some sort of “calibration” calculations.

Within the upper part of the dialog a radio list field allows to
**choose the type of mean** between:

-  `weighted
   mean <#calculation-of-the-weighted-mean-and-its-standard-uncertainty>`__,

-  `mean by a weighted least-squares
   method <#least-squares-calculation-of-a-weighted-mean-and-its-standard-uncertainty>`__.

Furthermore, a number field in this dialog allows inputting the value of
the factor :math:`f_{B}`; the use of efficiency covariances can be
selected or de-selected.

The gamma energies and the net counting rates and their standard
uncertainties must be entered directly in the table.

In the first column of the table one can select or de-select individual
gamma lines.

View of the result from calculating a mean with Gamspk1
-------------------------------------------------------

**For the weighted mean of the single line activities** one obtains the
following interim report for the case of a measurement of Co-60 on a
HPGe detector:

----------------------------------------------------------------------

(1 + b/2L) equivalent factor for Compton BG rate: 1.120

Individual peak data:

(pgamm*fcoin is a measure for the importance of the line!)

i E PNRate epsPeak pgamm fatt fcoin (pgamm*fcoin)

keV cps %

----------------------------------------------------------------------------------

1 1173.20 5.699E-03 0.7790 0.99850 1.0000 1.0615 1.0599 values

2.71 1.6789 0.03000 1.0000 1.3810 u_rels in %

2 1332.50 5.360E-03 0.7030 0.99986 1.0000 1.0641 1.0640 values

2.76 1.6245 0.00060 1.0000 1.3890 u_rels in %

Results from individual peak activities:

A(i) = PeakNetRate(i) \* (fatt(i) \* fcoin(i)) / (epsPeak(i) \*
pgamm(i))

i E(keV) Activity (Bq) rel.StdDev (%)

--------------------------------------------------

1 1173.20 7.7771E-01 3.61

2 1332.50 8.1147E-01 3.63

Evaluation of the weighted mean:

weighted mean = 0.79379

int. std. dev. of the mean = 2.23372E-02 ( 2.81 %) (Bayes compliant)

ext. std. dev. of the mean = 1.85227E-02 ( 2.33 %) (not Bayes compliant)

Chi-square = test value T = 0.68763

reduced Chi-square = 0.68763

significance (Chi-square > T) = 4.06973 %

Note: only the internal standard deviation will be used hereafter!

----------------------------------------------------------------------

In the first table of this report the input data are shown in shortened
form without reproducing their uncertainties. In this example the
corrections for coincidence summing (fcoinsu) are quite significant
because of the well-type counting geometry. The product (pgamm \*
fcoinsu) given in the last column of that table is a measure for the
weighting of the individual gamma lines.

The second table of this report shows the activity values (in Bq) and
their relative standard uncertainties (in %) calculated for the
individual lines. What follows are the data obtained from calculating
the weighted mean.

**For the mean obtained by applying weighted least-squares** (LSQ Mean)
calculated from several peaks one obtains for the lower result-part
shown above for the weighted mean:

(Note: in this case the activity variances of the two gamma lines are
practically identical; thus, there are nearly no deviations between this
method and that of the weighted mean.)

Evaluation of the weighted mean by least-squares:

weighted mean = 0.79358

std. dev. of the mean = 2.23360E-02 ( 2.81 %)

reduced Chi-square = 0.86694



Obtaining MC distributions and statistics derived of it in detail
-----------------------------------------------------------------

During the Monte Carlo simulation successively three different
distributions are produced from which some statistics are derived. The
latter are the arithmetic mean, the standard deviation of the
distribution and especially certain quantiles.

At the beginning the (large) number N of simulated measurement values of
the output quantity is defined. This defines one “run”. Several runs
(number *r*; maximum 50) may be calculated which then allows statistical
evaluations to be made of the statistics mentioned above; from the
r-fold repetition of the simulation one gets for instance with the
standard deviation of the *r* values of a statistic an indication of its
uncertainty.

**Determining a quantile**

The MC procedure use previously was primarily designed for nearly
symmetric distributions of the output quantity. With applying more and
more the GUM Supplement 1 with asymmetric distributions of some specific
input quantities may result in a rather asymmetric distribution of the
output quantity. Extreme values, to be expected for the distribution,
had to be fixed in the program in advance, which turned out to be a
disadvantage.

Therefore, with version 2.1.9 the MC procedure was modified – and
thereby simplified, in that all MC values of the output quantity are now
store in an array. Now, for every of the three above mentioned
distributions MC simulation yields arrays of up to 2 000 000 MC values.
From these arrays mean and standard deviation are easily calculated.
Quantiles are estimated by a distribution-free method, which requires
sorting the arrays first. Only for the purpose of graphical
representation as a histogram, the MC values have to be sorted into
20 000 Bins („multi-channel“); the limits of the histograms are
determined after the MC simulation – and not before. These modifications
made the MC procedure code easier to handle.

1. **Bayesian estimates**

..

   For the estimation of mean, standard deviation and lower and upper
   confidence limits according to the Bayesian method only the
   distribution truncated at zero is used, values below zero are not
   used and not considered for the total number of values. The
   confidence limits are estimated as lower and upper quantile of the
   distribution. The total number of values considered in one run is
   only about N/2.

   With Version 1.03 an option was added within the dialog „Results“, MC
   section, with which it is possible to calculate a special pair of
   coverage limits which has the shortest distance (Bayesian coverage
   interval of shortest length) instead of the symmetrical confidence
   limits. Test case: Neutron-Dose-Cox-2006_EN.txp. The activation of
   this option does not lead to a significant increase of simulation
   duration.

   Input parameters being associated with an asymmetric or rectangular
   distribution may result in a significant **asymmetry of the output
   quantity distribution**. In the decision threshold case this may
   cause some deviation of the mean value from zero.

   At first, a deviation from an expected distribution symmetrically to
   zero is tested. This is done by comparing the mean value
   *y*\ :sub:`1`, deviating from zero, with the MC uncertainty of the
   mean, *u*\ (*y*\ :sub:`1`)/√\ *N* (see below). In the case of

   :math:`\frac{y_{1}}{u\left( y_{1} \right)/\sqrt{N}} > 0.10\ `

   **an iteration is performed for the decision threshold using the
   secant method**, which shifts the mean value closer to zero\ **.** If
   necessary, up to 12 steps are performed, each of them requiring a
   full MC simulation; this leads to increasing computational effort. If
   the above ratio does not fall below 0.10, that distribution with the
   lowest ratio is selected from those of the 12 steps, from which the
   decision threshold is then calculated. *y*\ :sub:`1` can be reduced
   by typically one to two orders of magnitude; it may happen, however,
   that *y*\ :sub:`1` is reduced by hardly more that a factor of 10.

2. **Decision threshold**

..

   For the estimation of the Decision threshold a modification is
   applied which consists of setting the “true value” of the net
   counting rate (thereby also of the activity) equal to zero. This
   results in a distribution which to about 50 % has negative values. In
   this case, no truncation at zero is applied. The upper
   (1-:math:`\alpha`) quantile of this distribution represents the
   simulated value of the Decision threshold.

3. **Detection limit**

..

   The estimation of the Detection limit is the most time-consuming part
   of the simulation, because in this case the distribution is shifted
   several times by changing the mean of the distribution, each time
   after the total number *N* is reached. The aim of the step-wise
   shifting hereby is that the (lower) ß Quantile of this distribution
   has to come as close to the value of the Decision threshold
   (determined before, see above) as possible. This iteration procedure
   is based on a bi-section method but with dividing the interval by
   linear interpolation.

   Negative values are explicitly considered in these calculations.

The motivation not to discard negative parts of the distributions for
the estimation of Decision threshold and Detection limit comes from the
fact that the primary result for the output quantity shall be directly
comparable with Decision threshold. If the negative parts would be
discarded, a transformation being in analogy to the truncation would
have to be applied to the primary output quantity before such a
comparison

To make the distributions non-negative in the case of the Bayesian
estimates is possible after a comparison between primary output quantity
and the Decision limit has been done and has led to the conclusion that
the assumption of a non-zero activity value in the sample is true.

**Estimating the MC uncertainties of the characteristic values**

Performing a MC-Simulation with only one single run did up to now not
give any estimates of the uncertainties associated the characteristic
values. Furthermore, when using a small number of runs, the estimates
obtained from the spreading within the runs also are not very reliable.

As the resulting MC distributions in most cases are normal
distributions, uncertainties of characteristic values can be roughly
estimated as follows (see e.g. Barlow, 1999):

+--------------------+-------------+----------------------------------+
| characteristic     | width       | formula for the (absolute)       |
| value:             | parameter:  | uncertainty:                     |
+====================+=============+==================================+
| Value of output    | .. ma       | .                                |
| quantity           | th:: \sigma | . math:: \frac{\sigma}{\sqrt{N}} |
+--------------------+-------------+----------------------------------+
| uncertainty        | .. ma       | ..                               |
|                    | th:: \sigma |  math:: \frac{\sigma}{\sqrt{2N}} |
+--------------------+-------------+----------------------------------+
| lower confidence   | .. ma       | .. math:: \frac{\sigma}          |
| limit              | th:: \sigma | {\varphi\left( \Phi^{- 1}(1 - \g |
|                    |             | amma/2) \right)}\sqrt{\frac{(1 - |
|                    |             |  \gamma/2) \bullet \gamma/2}{N}} |
+--------------------+-------------+----------------------------------+
| upper confidence   | .. ma       | dito                             |
| limit              | th:: \sigma |                                  |
+--------------------+-------------+----------------------------------+
| decision threshold | .. math     | .. math:: u_{MC}\l               |
|                    | :: \sigma^{ | eft( y^{*} \right) = \frac{\sigm |
|                    | *} = \frac{ | a^{*}}{\varphi\left( \Phi^{- 1}( |
|                    | y^{*}}{k_{1 | 1 - \alpha) \right)}\sqrt{\frac{ |
|                    |  - \alpha}} | (1 - \alpha) \bullet \alpha}{N}} |
+--------------------+-------------+----------------------------------+
| detecion limit     | .. ma       | :math:`u_{MC}\left( y^{\         |
|                    | th:: \sigma | #} \right) = \sqrt{u_{MC}^{2}\le |
|                    | ^{\#} = \fr | ft( y^{*} \right) + u^{\#\ 2}}`, |
|                    | ac{y^{\#} - |                                  |
|                    |  y^{*}}{k_{ | with                             |
|                    | 1 - \beta}} |                                  |
|                    |             | .. math:: u^{\#} = \frac{\si     |
|                    |             | gma^{\#}}{\varphi\left( \Phi^{-  |
|                    |             | 1}(1 - \beta) \right)}\sqrt{\fra |
|                    |             | c{(1 - \beta) \bullet \beta}{N}} |
+--------------------+-------------+----------------------------------+

(N: number of MC-simulated single measurements; ϕ(.) and Φ(.): density
function and distribution function of the standard normal distribution,
respectively; see Options dialog for the probabilities α, β and γ).

The MC uncertainties calculated according to this table are given as
relative values in percent within the MC part under the TAB „Results“,
but only in the case of one single run.

**Graphical presentations**

The distributions which are produced according to the three methods
discussed above are displayed as histograms in a separate window while
the simulation is running. They show each distribution accumulated from
the *r* runs which stabilize after only few (of *r*) repetitions. In the
case of the Detection limit the accumulated distribution is displayed
after the *r* runs are terminated. The x-axis (abscissa) corresponds to
values of the evaluated quantity shown in the title of a plot; the
y-axis (ordinate) shows the probability.

Example of the separate window with the MC graphs:

+-----------------------------------------------------------------------+
| |MCplotfile-d_EN.png|                                                 |
+=======================================================================+
+-----------------------------------------------------------------------+

**Vertical green lines** in the graphs characterize, from the left to
the right, the following values:

**distribution of: values:**

output quantity lower confidence limit, best estimate (Bayesian), upper
confidence limit

Decision threshold its value

Detection limit Decision threshold, Detection limit

After each single MC run that **Gaussian curve is potted in blue color**
which corresponds to the result of the analytical procedure.

Note: The separate window with the MC graphs is maintained after
completion of the MC simulation calculations. This allows for additional
inspection of data shown under the different TABs and for invoking a
result report and to go then back to “Results” TAB with this window. If
data or options were changed during this step having the consequence
that the original assumptions underlying the MC simulation are no longer
valid, this MC window will be closed. This is also done when again
calculations in the TAB “Values, uncertainties” or calculations
initiated by the change from TAB “Values, uncertainties” to the TAB
“Uncertainty budget” are invoked.

Notes on the PMLE procedure for linear unfolding
------------------------------------------------

The procedure for Poisson MLE (PMLE) requires that the dependent
quantities in unfolding, e.g. gross counts of a decay curve, are
Poisson-distributed. As the latter are applied as net count rates for
the other methods, these net count rate values are converted to gross
counts within the program. The gross counts of a decay curve

:math:`N_{b,i} = \left( R_{b,i} + R_{0,i} + R_{bl} \right) \bullet t_{m}`
; :math:`u\left( N_{b,i} \right) = \sqrt{N_{b,i}}`

are considered as non-correlated. :math:`t_{m}` must have the same value
for all measured count rates, otherwise the shape of the gross counts
decay curve would be disturbed.

If for instance an Y-90 decay curve shall be fitted, the corresponding
model equation for the net count rate representation

:math:`R_{b,i} = y_{1} \bullet X_{1}\left( t_{i} \right)`

converts to the model of the PMLE fit:

:math:`N_{b,i} = \left( y_{1} \bullet X_{1}\left( t_{i} \right) + y_{2} \bullet 1 \right) \bullet t_{m}`

This requires inferring a second fitting contribution with the fitting
parameter :math:`y_{2}` and :math:`X_{2}\left( t_{i} \right) = 1`. The
parameter :math:`y_{2}` represents the sum of a background and a blank
value, :math:`\left( R_{0,i} + R_{bl} \right)t_{m};` it will be fitted
by the PMLE procedure and may end up with a value which can deviate from
the value :math:`\left( R_{0,i} + R_{bl} \right)t_{m}` known from
measurement.

**Features:**

This procedure needs to have one fitting parameter more than the other
procedures used for fitting net count rate decay curves. In UncertRadio
it can therefore be applied only if not more than two physical
components are to be determined. Both of these two components should
have the property that their associated curves should be different from
being constant or quasi-constant within time. If one of these components
represents the contribution of a radionuclide with a rather large
half-live, such that the decay curve fails to show a decrease, the
fitting procedure cannot differentiate between this contribution and
that of :math:`\left( R_{0,i} + R_{bl} \right)t_{m}`. In such a case the
PMLE-procedure cannot be used.

Furthermore, the PMLE procedure can at present not be applied, if
another fitting parameter is used with the status „fixed“, as is the
case for a Sr measurement where a Sr-85 tracer was added to the sample.

The PMLE procedure is not selectable in a case with too few countings
compared to the number of parameters to be fitted.

**Applying this procedure:**

The data within the dialog “Input of decay curve“ are to be handled in
the same way as for the other fitting procedures; nothing changes there.

Has a project already be established for the use with e.g. the WLS
procedure, the program has already sufficient information about the
components to be fitted and is able to decide whether the
above-mentioned criteria for applying PMLE are fulfilled. If the
criteria are not fulfilled, the selection of the PMLE procedure in the
model dialog “model of decay curve“ is prevented

Is the PMLE procedure selectable within the model dialog, the procedure,
if selected, is processed in the same manner as for the other
procedures. Only within the view of the fit result (|image51|), gross
counting rates are displayed instead of net count rates.

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

:math:`g_{X}(\xi) = \frac{\xi^{q}e^{- \xi}}{q!} \equiv Ga\left( \xi|q + 1,1 \right)`
for :math:`\xi \geq 0` (1)

Mean and variance are :math:`E\lbrack X\rbrack = q + 1` and
:math:`Var\lbrack X\rbrack = q + 1`, respectively. This refers to
numbers of counts.

For Poisson-distributed numbers of counts, a Gamma distribution is
assigned In ISO 11929-2019 to the associated count rate :math:`\rho`,
where a prior :math:`\rho^{- 1}` is used instead of a constant prior.
Mean and variance are in this case given as :math:`q/t` und
:math:`q/t^{2}`, respectively.

In UncertRadio, the described step is treated as follows. As already
given by Eq. (1), the Gamma distribution is assigned to the number
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

International Safety Research, Safety Support Series, 2013. Radiation
Counting Statistics. Volume 1. Canada.

Pengra, D., 2008:

http://courses.washington.edu/phys433/muon_counting/counting_stats_tutorial_b.pdf

Pishro-Nik, H., Introduction to Probability:

https://www.probabilitycourse.com/chapter11/11_1_2_basic_concepts_of_the_poisson_process.php

.

**Comparing Erlang and Poisson distributions**

The two distributions are defined as follows, with :math:`\rho`
designating the count rate parameter:

Poisson distribution
:math:`P_{Poi}(n) = \frac{(\rho t)^{n}e^{- \rho\ t}}{n!}` (2)

:math:`E\left\lbrack P_{poi} \right\rbrack = \ Var\left\lbrack P_{poi} \right\rbrack = \rho t`

Erlang distribution
:math:`P_{Erl}(t) = \frac{\rho^{n}t^{n - 1}e^{- \rho\ t}}{(n - 1)!}\left\lbrack \equiv Ga\left( t|n,\rho \right) \right\rbrack`
(3)

:math:`E\left\lbrack P_{Erl} \right\rbrack = n/\rho;\ \ \ \ \ \ \ \ \ Var\left\lbrack P_{poi} \right\rbrack = n/\rho^{2}`

The Erlang distribution is a Gamma distribution for integer-valued
:math:`n`. The two formulae (2 and 3) lead to a simple relation:

:math:`{t\ P}_{Erl}\left( t|\rho,n \right) = n\ P_{poi}\left( n|\rho,t \right)`
(4)

Applying the Bayes theorem with a prior :math:`\rho^{- 1}` to both
distributions results in the same posterior distribution for the count
rate :math:`\rho`, a Gamma distribution:

**Measurement with pre-set time:**

:math:`\frac{P_{Poi}\left( n|\rho,t \right)\rho^{- 1}}{\int_{}^{}{P_{Poi}\left( n|\rho,t \right)\rho^{- 1}d\rho}} = \frac{P_{Poi}\left( n|\rho,t \right)\rho^{- 1}}{1/n} = \frac{n(\rho t)^{n}e^{- \rho\ t}\rho^{- 1}}{n!} = \frac{t^{n}{\rho^{n - 1}e}^{- \rho\ t}}{(n - 1)!} = Ga\left( \rho|n,t \right)`
(5)

**Measurement with pre-set counts:**

:math:`\frac{P_{Erl}\left( t|\rho,n \right)\rho^{- 1}}{\int_{}^{}{P_{Erl}\left( t|\rho,n \right)\rho^{- 1}d\rho}} = \frac{P_{Erl}\left( t|\rho,n \right)\rho^{- 1}}{1/t} = \frac{t\rho^{n}t^{n - 1}e^{- \rho\ t}\rho^{- 1}}{(n - 1)!} = \frac{t^{n}{\rho^{n - 1}e}^{- \rho\ t}}{(n - 1)!} = Ga\left( \rho|n,t \right)`
(6)

By equating the second parts of the two equations (5) and (6), the
simple relation of Eq. (4) is obtained again:

:math:`\frac{P_{Poi}\left( n|\rho,t \right)\rho^{- 1}}{1/n} = \frac{P_{Erl}\left( t|\rho,n \right)\rho^{- 1}}{1/t}`

or

:math:`t\ P_{Erl}\left( t|\rho,n \right) = n\ P_{Poi}\left( n|\rho,t \right)`
(7)

If another prior is used for the Poisson distribution,
:math:`\rho^{- 1/2}`, again a Gamma distribution is obtained, but a
different one: :math:`Ga\left( \rho|n + 1/2,t \right)`.

**In the case of pre-set counts** (counting duration :math:`t`
variable), **the Erlang distribution must be assigned to** :math:`t` by
selecting the distribution type “Npreset“ **for** :math:`t`. By an also
required equation like :math:`R = n/t`, the Gamma
distribution\ :math:`\ Ga\left( \rho|n,t \right)` is thereby internally
assigned to the count rate :math:`R`.

Example project: **PresetCounts_EN.txp**

Treatment of physical units
---------------------------

Within equations for calculating the value of an output quantity, the
input quantities are fully described by a value and by a unit.
Sometimes, it is not fully considered that instead of basic units (such
as kg, m, s) derived units (such as g, cm, min) are used. For
calculating the output quantity value correctly, with a combination of
basic units as its unit, scale factors for derived units have to be
inserted in the equations. There are two concepts supporting this within
UncertRadio:

-  Application of Trigger variables used to explicitly introduce unit
   scaling factors in equations; the reader is referred to section
   7.21.5. The scope of application of such triggers normally is wider
   than its use for scale factors.

-  One can try to derive the unit of a dependent quantity from the units
   of its input quantities by computations. This method can be used
   within UncertRadio, at present mainly designed as a testing option
   for a project. The more detailed description of such a method is the
   main purpose of this section.

Therefore, the aim is to correctly define for a project the number
values and units of its input quantities. This also includes the use of
derived units. It should then be possible for the program, to derive the
unit of the output quantity, with including the conversion to basic
units of the output quantity.

For successfully going this way, it is necessary to put more value on a
systematized application of units and their notation in text editors.
Therefore, it is **important to describe units of input quantities most
completely**. To arrive, for instance, in the example for measuring an
activity, at the unit “Bq” as a part of the output quantity’s unit, the
following is especially important:

-  Don’t leave the unit field of a detection efficiency empty, but use
   the unit string “1/Bq/s“;

-  Apply a unit like “Bq*s/kg” for a calibration factor;

-  For a chemical yield, if determined by weighing, the unit strings
   “g/g“ or “g/kg“ may apply.

Missing units of input quantities may prevent from calculating the
output quantity unit correctly.

**Important:** Unfortunately, this automated way of calculating units is
not compatible with applying trigger variables for scaling factors.

Collection of basic units and derived units
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is necessary to differentiate between basic units and derived units.
For evaluating an UncertRadio project, the aim is to replace derived
units by their basic units. Such a conversion requires to apply an
associated conversion factor to values und uncertainties of the
quantities.

The UncertRadio installation includes two CSV files,
**unitsTable_EN.csv** and **units_other_EN.csv**. These are shown below.

**unitsTable_EN.csv:**

The meaning of the columns is:

column A: basic units;

column B: a characteristic numerical value attributed to the basic unit;

column C: derived units;

column D: the scaling factor associated with the unit in column C;

column E and the following: within the row of the basic unit, several
synonymous unit names can be inserted.

**Note:** As CSV files are language dependent, these two CSV files have
been replaced by text files, which are shown next below the two CSV
files.

|image52|

*Note: The entries for m2 and m3 have been removed and transferred to
Units_other.txt*

**Units_other_EN.csv:**

This file contains only two columns, A and B. In column A, a unit name
can be given in a notation preferred by a laboratory, column B gives the
correct notation.

|image53|

**The following two text files replace the CSV files**:

**unitsTable.txt** (first part)\ **:**

   base:Base unit;base#: base unit value; syn:synonym;derv:derived
   unit;conv: scale factor

   base=Bq

   base#=11.

   derv=mBq

   conv=1.00E-03

   derv=µBq

   conv=1.00E-06

   derv=kBq

   conv=1.00E+03

   base=s

   base#=21.

   derv=min

   conv=60.

   derv=h

   conv=3600.

   derv=d

   conv=86400.

   base=1/s

   base#=0.047619047619

   derv=cps

   conv=1.0

   derv=cpm

   conv=0.01666666666667

   derv=cph

   conv=0.000277777777778

   derv=1/min

   conv=0.01666666666667

   derv=1/h

   conv=0.000277777777778

   base=kg

   base#=41.

   derv=g

   conv=1.00E-03

   derv=mg

   conv=1.00E-06

   **. . .**

**units_other.txt** (complete):

   unit=Bq s

   ubase=Bq*s

   unit=m2

   ubase=m^2

   unit=m³

   ubase=m^3

   unit=cm2

   ubase=cm^2

   unit=cm3

   ubase=cm^3

The scaling factor associated with a counting duration is used (by
inversion) for a count rate variable *R* and, in most cases, this factor
is the same for *R* and for *u*\ (*R*), as long as the Poisson
statistics is applicable. An exception is given by the gross count rate
discussed in chapter 6.10, which is the sum of a binomial and a Poisson
distributed quantity. For a calibration factor *w* or *phi*, which can
be treated as a generalized product, the scaling factors for *w* or
*phi* and for *u*\ (*w*) or *u*\ (*phi*) are the same.

Explaining the calculation of units of dependent quantities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a dependent quantity the procedure is based on its equation given to
UncertRadio. The right-hand side of this equation is an arithmetic
expression (formula) of variable names. For calculating a unit, in a
first step, the variable names are replaced by unit names as strings; a
variable name “eps“ for a detection probability, e.g., is replaced by
“(1/eps/s)“; the brackets shall assure that this expression, after
insertion into the equation, is treated algebraically correct. In a
later step, the unit parts contained in it, “Bq“ and “s“, are replaced
by the characteristic numerical values “11“ and “21“ given in column B
of the file unitsTable.csv.

Before basic units can used for calculations, the following
modifications remain to be applied:

-  the right-hand side of an equation with the number :math:`i` consists
   of some quantities numbered by :math:`k`; the unit of the quantity
   with number :math:`k` can consist of one or more unit parts. The
   :math:`k`-th quantity has an index or address :math:`nng(k)` within
   the completed symbol table within UR.

-  outside of arguments of function, all minus characters are replaced
   by plus characters; this shall assure that in case of a simple net
   count rate the difference of the unit values shall not become zero.

-  For functions used inside a formula, like Log(), Exp() and Sqrt(),
   the variable names inside their arguments are replaced by unit names
   and later by their characteristic unit values. In this was, the
   argument of such a function gets a form which can be calculated
   numerically. For this purpose, a **second, simpler function parser is
   used UncertRadio, called seval**, which can calculate the formula
   string, if containing only numbers, directly, it does not operate on
   variables.

-  If the argument of Log() (mostly 2) does not contain a variable with
   non-empty unit, i.e., a number, the expression Log(Argument) is set
   equal to 1.

-  The unit of an input quantity with number :math:`k` can contain more
   than one unit parts, such that the unit represents a small formula.
   For a detection probability :math:`eps`, the unit could be 1/mBq/min.
   The unit parts are converted to basic units and the associated
   conversion factors are combined in the same way to build the
   conversion of this input quantity: in the example, the scaling factor
   of the combined unit is:
   :math:`uconv\left( nng(k \right)) = 1/0,001/60\  = \ 16,66667`, if
   the desired unit shall be 1/Bq/s.

-  To enable calculation, a unit string is build, for the example
   :math:`eps`, from the characteristic unit values (see
   unitsTable.csv): “(1/11.0/21.0)“.

-  Within an equation :math:`i` (for a dependent quantity) every single
   variable name contained in it (number :math:`k`) is replaced by such
   a string. The scaling factor :math:`uconv(i)` for the quantity
   associated with equation :math:`i` is determined from:

:math:`uconv(i) = seval(strgv1)/seval(strgv3)` (1)

Herein, the string :math:`strgv1` is the formula string of equation
:math:`i`, in which the names of the symbols :math:`k` are replaced by
the product :math:`uconv\left( nng(k) \right) \bullet Messwert(nng(k)`
converted to a string; :math:`strgv3` is the formula string of equation
:math:`i`, in which the names of the symbols :math:`k` are replace by
:math:`Messwert(nng(k))` converted to a string. Note: Messwert() denotes
the array of measurement values (called MVals() in this test).

Example for (LAMSR \* TS \* 60^0) / (1. - EXP(-LAMSR \* TS \* 60^0)):

strgv1=(( 7.63000000E-10) \* ( 2.40000000E+04) \* 60^0) / (1. - EXP(-(
7.63000000E-10) \* ( 2.40000000E+04) \* 60^0))

strgv3=(( 7.63000000E-10) \* ( 4.00000000E+02) \* 60^0) / (1. - EXP(-(
7.63000000E-10) \* ( 4.00000000E+02) \* 60^0))

uconv(i) = 1.00000906

-  It is assumed that the argument of an Exp function contains only
   quantities like a decay constant *lambda* (1/s) and a counting
   duration *t* (s). It is then allowed for this argument, that besides
   the characteristic unit values also scaling factors of
   :math:`60^{\pm 1}`, :math:`60^{\pm 2}` or :math:`86400^{\pm 1}` and
   at the same time also associated factors like :math:`60^{0}`, build
   from Trigger variables, may occur. If then the overall argument value
   is not an integer value and is not equal to :math:`60^{\pm 1}`,
   :math:`60^{\pm 2}` or :math:`86400^{\pm 1}`, a unit error is assumed;
   otherwise, the whole Exp(Argument) expression is set equal to 1.0
   (Exp(Argument)=1).

Often, the Exp() expression occurs in the form of Form (1.0 – Exp()).
The Minus sign in it is replaced by a Plus sign. If the analysis of the
Exp() expression alone led to the result that it was set equal to 1, the
whole (1.0 – Exp()) expression is set equal to 1. This still requires
finding in the string the position of the left (opening) bracket.

-  In the case of a sum in the argument of Sqrt(), e.g., for the
   variance of a net count rate with three terms with the unit (1/s^2),
   the value 3.0 \* (1/s^2) is expected for the argument. If, in this
   example, the factor 3.0 is not obtained, but a non-integer value, it
   can be assumed that at least one of the three terms carries a
   differing unit. If, however, an integer-valued factor is obtained,
   the whole sqrt expression can be replaced by the unit 1.

Following the replacement of an algebraic function expression for
equation :math:`i` by substrings containing the characteristic unit
values, a formula string should have been obtained (as a string
RSeiteG2(i)), which can be evaluated by *seval*. The resulting numerical
value is *Evalue*:

:math:`\mathbf{Evalue}\  = \ seval(RSeiteG2(i))\ /\ uconv(i)` (2)

Arrived at this stage, it may have happened that some unities cancelled
out.

The question then arises, how many – and which ones – basic units remain
to contribute to *Evalue* (Eq. (2))? Therefore, in this formula string,
the individual unit values, like “21.0“ for “s“ string, are replaced by
their basic units ( as strings). Based on pairs “Basic unit name, unit
value“, which are taken from the first two columns of the file
unitsTable.csv, the more complex function parser *parsef* can be used
for calculating partial derivatives of the formula :math:`i` with
respect to the basic units. Only such individual units contribute to the
unit of :math:`i` which show partial derivatives having (practically)
non-zero values.

Now, when the set of participating unit parts is known, e.g., “Bq“, “s“
and “kg“, it has to be found out, which of them belong to the nominator
or to the denominator of a generalized product. For these three unit
parts, abbreviated now by a, b and c, the following :math:`2^{3} = 8`
possibilities have to tested:

:math:`a^{\pm 1} \bullet b^{\pm 1} \bullet c^{\pm 1}` (3)

The 8 possible combinations are tested numerically; if one of it results
in the above-mentioned value *Evalue* (Eq. (2), the correct combination
is found: e.g., “Bq*s/kg“, if the value *Evalue* is equal to
11.0*21.0/41.0.

Adjustments in the procedure
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, the units of all (independent) input quantities are replaced by
basic units. If this implies a scaling factor not being one, the
associate quantity value is (temporarily) multiplied with this factor.

In the next step, the units of the dependent quantities are calculated
from the just treated input quantity units as described above.

The scaling factors of the dependent quantities are not derived from
unit calculations. Instead, after the modification of the input
quantities, they are calculated internally using the function Resulta
without considering their units. Their associated unit scaling factors
unit_conv_factor() are calculated thereafter, simply as ratios of the
new quantity values and their previous values. Values of the
uncertainties are then scaled by these factors unit_conv_factor().

Invoking the test of unit calculations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The calculation of units of dependent quantities cab be invoked under
the menu item “Edit – test physical units“. The project should be
developed such far that values are available under the TAB “Results”.

**Important**:

-  The calculation of units implies the conversion to basic units. If
   other basic units are desired, the latter must be declared as basic
   units in the file unitsTable.csv. For example, if the unit kg shall
   be replaced by the unit g, make g to the basic unit and the kg to a
   derived unit.

-  If the project contains a Trigger variable, invoking the unit test
   modus is prevented from invoking. The reason is, that a modified
   project saved directly by this test mode as a new file, normally does
   not work properly.

-  If the test modus still indicates errors, this modus must be finished
   with explicitly using the close button of the Editor Tab. This leads
   to restoring the original status of program data.

-  If no unit-related errors are shown, an addition button appears. It
   allows to save the modified state of the project as a new project
   file. This normally is necessary only if there are obvious deviations
   between the output quantity values.

The program executes the calculations according to chapter 7.21.2 and
then displays in the program editor a comparison for the list of
symbols.


For dependent quantities (indexes :math:`i`), the unit names given
primarily by the user are replaced by “calculated“ unit names, which
means that one must take care about the changed status of the project.

For the output of this test, UncertRadio calculates scaled values of
measurement and associated standard uncertainties (*MVals_scd* und
*StdUnc_scd*) as follows:

-  The column „MVal_scd/MVals_org“ shows the obtained actual scaling
   factors unit_conv_factor().

-  The values *Mvals_scd* (dependent), derived with the function Resulta
   from the modified (independent) input quantity values.

-  All values *StdUnc_scd*, by scaling the previous values *StdUnc_org*
   with the factors unit_conv_factor().

The output of the comparison test in the editor starts with a first
error message, if the comparison between the value Evalue (Eq. (2)) and
values from Eq. (3) (previous section) does not come to any agreement.

Introduction of Trigger variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The program cannot give direct recommendations how to proceed if the
test runs into an error. One reason can be a scaling factor which
already exists in an equation like 60 for transforming “min“ into “s“.
The impact by such already existing scaling factors can be reduced by
using special switching variables (see chapter 2.2.6): for “Minutes
(min)“ or for “Gram (g)“ already existing scaling factors 60 or 1/1000
are to be replaced as follows (alternatively, these factors are replaced
by 1 and their unit is changed explicitly to “s” of “kg”):

60 60^min_Trigger

1/1000 1/1000^kilo_Trigger

These two special Trigger names are directly interpreted by UncertRadio;
the value zero is assigned to them, if the Menu item „test physical
units“ is applied, and prior to the test, they get the value 1.

The problem, that the expected unit of the output quantity, “Bq“ in the
case of an activity, does not contain the substring “Bq“, but “1/s“
instead, can very often be solved by attributing the unit “1/Bq/s” to
the detection probability.

**Test example: Janszen-Sr-89-Sr-90_V2_DE.txp**

The execution of the test stops with an error message. Then, the
following changes are applied:

The first two equations:

   a89 = As89 / (ms/1000.)

   a90 = AsS90 / (ms/1000.)

are changed to:

   a89 = As89 / (ms/1000.^kilo_Trigger)

   a90 = AS90 / (ms/1000.^kilo_Trigger)

To obtain the desired unit “Bq/kg“, set the units of the four detection
probabilities epsXXX to “1/Bq/s”.

The project file including these changes is available as
**Janszen-Sr-89-Sr-90_V3_DE.txp**.

If a unit error is introduced, e.g., by changing in the symbol table
under the TAB “Equations“ the unit of t2m0 from “s“ to “min“, then move
the calculations forward to the TAB “Results“. An expected error message
is then obtained when the unit test is called, in this case:

Error messages:

Eq. #=8 Error CLCU: Units in EXP argument do not match:
seval=-60.0000000 arg(EXP)=-(1.0/21.0) \* ( 6.000000E+01*(21.0))

Eq. #=4 RD89 = RSr - w*RY*f7 - Abl*(etaSr \* epsSrSr89 \* f1): no unit
found! Einvor=1 RSide=RSR-W*RY*F7-ABL*(ETASR*EPSSRSR89*F1)

**Similar changes** had to be applied to the project
**Moreno-Sr90_IAEA-135_EN.txp**.

The unit-strings “u (uamu)“ were replaced by “u“. Within the equations
for the quantities a, w, f2 and epsSr, the two switching variables
“kilo_Trigger” or “min_Trigger” were introduced. A unit “mg Sr“ was
replaced by “mg“.

The project file including these changes is available as
**Moreno-Sr90_IAEA-135_V2_EN.txp**.

Experiences with the option for calculating units
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The experience with the option for testing or calculating physical
quantities could be improved with the version 2.4.21. As such, this
option could be integrated for the first time in the test with
„QC-Batch-Test“, i.e., into the automatic evaluation run over all
example projects. This allowed to identify some projects exhibiting
specially selected units, for example,

Wuebbeler_Ex2\*.txp (Ohm, Volt, Ampere)

Sterlinski\*.txp (n-activation, unit ng/g).

For the projects Kessel\*.txp and Calibration-of-weight-Cox\*.txp
(calibration of masses), deviating factors of 1000 were observed. The
project Calibration-of-weight-Cox\*.txp is still too complicated with
respect to deriving units. For the projects Neutron-dose-Cox-2006\*.txp,
changes of the resulting value by a factor of 100 per 36 are observed
from changes in the input quantity units.

These projects are left as they are.

A real unit error was observed with the four projects sumEval*V2\*.txp
(DE+EN). For their version V2, at that time, the output quantity unit
was set Bq/m²t, however, the associated area of 400 cm² had not been
changed to 0,04 m². This error of the V2 version now soon became obvious
by the new test modus. The new project version shown with the correct
units, but output quantity values changed by the factor 100x100, has
been saved, while still being in the test mode, as version V3 projects.

From the project Moreno-Sr90_IAEA-135_V2\*.txp, the triggers and the
associated scaling factors of 60 or 1000 have been removed from the
evaluation equations and the projects were then successful tested for
unit calculations. With the processed derived units, the missing of the
constant factors has been equalized. Value and uncertainty of the output
quantity finally is left unchanged. The new project produced within the
test modus is now released as a V3 version.

The same applied to the project Janszen-Sr-89-Sr-90_V3\*.txp. The
kilo_trigger and the associated of the mass (g) were removed. The
missing factor of 1000 in the equations has been equalized by the unit
calculations, value and uncertainty of the output quantity are left
unchanged. A new version V4 of these projects have been produced.

.. |Ein Bild, das Uhr enthält. Automatisch generierte Beschreibung| image:: media/image1.png
   :width: 4.4685in
   :height: 2.75591in
.. |Ein Bild, das Screenshot enthält. Automatisch generierte Beschreibung| image:: media/image2.jpg
   :width: 5.36614in
   :height: 1.9685in
.. |image1| image:: media/image3.png
   :width: 4.62552in
   :height: 3.99865in
.. |image2| image:: media/image4.png
   :width: 0.1982in
   :height: 0.19466in
.. |view-refresh.png| image:: media/image5.png
   :width: 0.24095in
   :height: 0.24409in
.. |image3| image:: media/image6.png
   :width: 5.39644in
   :height: 4.13386in
.. |Ein Bild, das Text enthält. Automatisch generierte Beschreibung| image:: media/image7.png
   :width: 6.75579in
   :height: 1.15895in
.. |document-open.png| image:: media/image8.png
   :width: 0.24571in
   :height: 0.24409in
.. |document-save.png| image:: media/image9.png
   :width: 0.24571in
   :height: 0.24409in
.. |document-save-as.png| image:: media/image10.png
   :width: 0.24571in
   :height: 0.24409in
.. |image4| image:: media/image11.png
   :width: 0.23509in
   :height: 0.19685in
.. |image5| image:: media/image11.png
   :width: 0.21239in
   :height: 0.19685in
.. |image6| image:: media/image11.png
   :width: 0.20587in
   :height: 0.19685in
.. |image7| image:: media/image11.png
   :width: 0.22837in
   :height: 0.19685in
.. |image8| image:: media/image11.png
   :width: 0.20587in
   :height: 0.19685in
.. |image9| image:: media/image11.png
   :width: 0.22837in
   :height: 0.19685in
.. |image10| image:: media/image11.png
   :width: 0.23509in
   :height: 0.19685in
.. |help-contents.png| image:: media/image12.png
   :width: 0.24571in
   :height: 0.24409in
.. |dialog-information.png| image:: media/image13.png
   :width: 0.2441in
   :height: 0.24409in
.. |image11| image:: media/image11.png
   :width: 0.28693in
   :height: 0.2092in
.. |image12| image:: media/image14.png
   :width: 0.22189in
   :height: 0.23622in
.. |image13| image:: media/image4.png
   :width: 0.19444in
   :height: 0.19097in
.. |image14| image:: media/image15.png
   :width: 0.26772in
   :height: 0.26378in
.. |image15| image:: media/image16.png
   :width: 0.35015in
   :height: 0.329in
.. |image16| image:: media/image11.png
   :width: 0.23509in
   :height: 0.19685in
.. |image17| image:: media/image17.png
   :width: 3.95986in
   :height: 3.89274in
.. |image18| image:: media/image18.emf
   :width: 7.125in
   :height: 9.89217in
.. |image19| image:: media/image19.emf
   :width: 7.09375in
   :height: 7.92668in
.. |image20| image:: media/image11.png
   :width: 0.28693in
   :height: 0.2092in
.. |image21| image:: media/image14.png
   :width: 0.22851in
   :height: 0.24016in
.. |image22| image:: media/image11.png
   :width: 0.22837in
   :height: 0.19685in
.. |image23| image:: media/image20.png
   :width: 4.32785in
   :height: 6.69291in
.. |image24| image:: media/image21.png
   :width: 5.95832in
   :height: 1.82903in
.. |image25| image:: media/image11.png
   :width: 0.23509in
   :height: 0.19685in
.. |image26| image:: media/image22.emf
   :width: 7.48031in
   :height: 5.73228in
.. |image27| image:: media/image23.emf
   :width: 5.10976in
   :height: 2.5578in
.. |image28| image:: media/image24.emf
   :width: 4.76124in
   :height: 2.72435in
.. |image29| image:: media/image25.png
   :width: 7.184in
   :height: 4.82464in
.. |image30| image:: media/image26.png
   :width: 7.2074in
   :height: 3.64048in
.. |image31| image:: media/image27.png
   :width: 5.61688in
   :height: 3.65342in
.. |image32| image:: media/image28.png
   :width: 5.40966in
   :height: 3.17306in
.. |Ein Bild, das Text, Screenshot, Schrift, Software enthält. Automatisch generierte Beschreibung| image:: media/image29.png
   :width: 5.72917in
   :height: 2.08333in
.. |Ein Bild, das Text, Screenshot, Schrift, Reihe enthält. Automatisch generierte Beschreibung| image:: media/image30.png
   :width: 3.75in
   :height: 1.51042in
.. |image33| image:: media/image31.wmf
.. |image34| image:: media/image32.png
   :width: 5.97583in
   :height: 4.27117in
.. |image35| image:: media/image33.png
   :width: 5.39869in
   :height: 4.26897in
.. |MCplotfile_a_EN.png| image:: media/image34.png
   :width: 3.76038in
   :height: 4.92126in
.. |MCplotfile-b_EN.png| image:: media/image35.png
   :width: 3.76925in
   :height: 4.92126in
.. |MCplotfile-c_EN.png| image:: media/image36.png
   :width: 3.76925in
   :height: 4.92126in
.. |image36| image:: media/image37.png
   :width: 3.16844in
   :height: 5.51181in
.. |image37| image:: media/image38.png
   :width: 3.16844in
   :height: 5.51181in
.. |image38| image:: media/image4.png
   :width: 0.24747in
   :height: 0.24306in
.. |image39| image:: media/image39.png
   :width: 3.78147in
   :height: 5.04196in
.. |image40| image:: media/image40.png
   :width: 3.37043in
   :height: 2.70012in
.. |image41| image:: media/image15.png
   :width: 0.26772in
   :height: 0.26378in
.. |image42| image:: media/image15.png
   :width: 0.26772in
   :height: 0.26378in
.. |image43| image:: media/image41.png
   :width: 3.48239in
   :height: 3.60247in
.. |Ein Bild, das Text, Diagramm, Reihe, Screenshot enthält. Automatisch generierte Beschreibung| image:: media/image42.png
   :width: 6.33889in
   :height: 4.19506in
.. |image44| image:: media/image11.png
   :width: 0.23509in
   :height: 0.19685in
.. |delete_rows_icon| image:: media/image43.png
   :width: 7.35865in
   :height: 2.91293in
.. |image46| image:: media/image44.png
   :width: 6.36596in
   :height: 3in
.. |image47| image:: media/image45.png
   :width: 8.15503in
   :height: 4.3008in
.. |image48| image:: media/image46.png
   :width: 1.35809in
   :height: 0.73629in
.. |image49| image:: media/image11.png
   :width: 0.21239in
   :height: 0.19685in
.. |image50| image:: media/image47.png
   :width: 8.0558in
   :height: 3.8769in
.. |MCplotfile-d_EN.png| image:: media/image48.png
   :width: 4.05442in
   :height: 5.51181in
.. |image51| image:: media/image11.png
   :width: 0.22837in
   :height: 0.19685in
.. |image52| image:: media/image49.png
   :width: 6.40645in
   :height: 7.89366in
.. |image53| image:: media/image50.png
   :width: 6.32826in
   :height: 2.89298in
.. |Ein Bild, das Tisch enthält. Automatisch generierte Beschreibung| image:: media/image51.png
   :width: 8.30315in
   :height: 7.48031in
