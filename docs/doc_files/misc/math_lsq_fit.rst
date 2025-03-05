Mathematics of the linear LSQ curve fitting with correlated measured values
---------------------------------------------------------------------------

Basics
^^^^^^

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

Here, :math:`\mathbf{y}` is the resulting output vector of the fitted
parameters and :math:`\mathbf{U}_{\mathbf{y}}^{\mathbf{- 1}}` its
associated covariance matrix.

For the value of the minimum function one obtains by using
:math:`\mathbf{\ A}^{\mathbf{T}}\mathbf{\ U}_{\mathbf{x}}^{\mathbf{- 1}}\mathbf{\ x =}\mathbf{U}_{\mathbf{y}}^{\mathbf{- 1}}\mathbf{\ y}`:

:math:`\mathbf{\min}\mathbf{\chi}^{\mathbf{2}}\mathbf{=}\left( \mathbf{Ay - x} \right)^{\mathbf{T}}\mathbf{\ }\mathbf{U}_{\mathbf{x}}^{\mathbf{- 1}}\mathbf{\ }\left( \mathbf{Ay - x} \right)\mathbf{=}\mathbf{x}^{\mathbf{T}}\mathbf{\ }\mathbf{U}_{\mathbf{x}}^{\mathbf{- 1}}\mathbf{\ x -}\mathbf{y}^{\mathbf{T}}\mathbf{\ }\mathbf{U}_{\mathbf{y}}^{\mathbf{- 1}}\mathbf{\ y}`


:math:`\chi_{R}^{2} = \min{\chi^{2}/(n - nr)}`

The covariance matrix :math:`\mathbf{U}_{\mathbf{y}}^{\mathbf{- 1}}` can
be multiplied with :math:`\chi_{R}^{2}`, if the value of the latter is
larger than one although it is not allowed from the strict Bayesian
point of view and therefore not performed in UncertRadio.

Chi-square options
^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^^^^^

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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


PMLE Literature
^^^^^^^^^^^^^^^

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