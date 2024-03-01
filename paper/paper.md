---
title: 'UncertRadio: Software for determining characteristic threshold values in accordance to ISO 11929 for environmental radioactivity measurements'
tags:
  - Numerical uncertainty calculation
  - Environmental Radioactivity
  - Gamma-ray spectrometry
  - DIN ISO 11929
  - Monte Carlo
authors:
  - name: Günter Kanisch
    #equal-contrib: true
    affiliation: 1

  - name: Florian Ober
    corresponding: true # (This is how to denote the corresponding author)
    equal-contrib: false
    orcid: 0009-0008-4697-3242
    affiliation: 2

  - name: Marc-Oliver Aust
    orcid: 0000-0002-5165-8641
    equal-contrib: false
    affiliation: 3
affiliations:
 - name: Hamburg, Germany, formerly Thünen Institute
   index: 1
 - name: Max Rubner-Institut, Kiel, Germany
   index: 2
 - name: Thünen Institute, Bremerhaven, Germany
   index: 3

date: xx March 2024
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
# aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
# aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary


<!-- # Statement of need

`UncertRadio` is an

`UncertRadio` was designt -->

# Summary and statement of need

In Germany, radioactive substances in the environment are monitored in accordance with the Treaty establishing the
European Atomic Energy Community (EURATOM) of 1957 and the Radiation Protection Act
[@bundesrepublikdeutschland2017]. The Federal agencies are responsible to develop and define sampling
and analysis procedures to ensure consistent measurements and evaluations of environmental
contamination during monitoring. Evaluations of the output quantities of the
measurement of environmental radionuclide activity concentrations, and estimating associated uncertainties,
are based on the ISO GUM [@jointcommitteeforguidesinmetrology2008]. The variety of such determinations or
measurements and consequently of evaluation methods is quite large. The standard @DIN11929-1:2021 series,
consisting of four parts, extends this guide by methods for deriving the decision
threshold (DT), the detection limits (DL) and limits of coverage intervals.

It has been demonstrated earlier [@kanisch2016a;@kanisch2016b] how a generalized evaluation of a large
variety of environmental radioactivity measurements with and without using linear unfolding,
can be performed with a single program, `UncertRadio`. It is able to evaluate a set of
equations describing the measurement evaluation model, provided by the user during runtime,
through a function parser called fparser [@schmehl2008].

For ISO GUM compatible uncertainty propagation, such numerical calculations include also the calculation of
partial derivatives approximated by difference quotients. Thus, the software frees the user from
manually supporting partial derivatives, which avoids possible user mistakes like over-looked covariances.
`UncertRadio` takes care of these situations, because the numerical calculation of the partial derivatives
is organised such that the program always uses derivatives of the ~~(only numerically available)~~
formula of the output quantity.

Furthermore, the evaluation within `UncertRadio` is extended to include @DIN11929-1:2021 decision
thresholds and detection limits. Alternative to numerical uncertainty propagation,
the "propagation of distributions" of input variables with Monte Carlo (MC) simulation in
accordance to @DIN11929-2:2021 is included.

\sout{The classical way of uncertainty
calculation often consisted in breaking up the formula for the output quantity into a general
product of few smaller parts (e.$\,$g. factors), first apply the uncertainty propagation to these
parts and combine then their uncertainties to the output quantity uncertainty. This way of handling
may easily lead to non-recognized or over-looked covariances between the smaller parts by such
input quantities occurring in more than one of the smaller parts.}

Measurement procedures dealing with more than one output quantity generally would suggest
applying linear unfolding methods, i.$\,$e, linear weighted least-squares [@DIN11929-3:2021]. The
important methods of determining strontium radionuclide activities of Sr-90 and Sr-89 in environmental
samples are one of such examples. It may be characterized by the evaluation scheme "two equations
with two unknowns".


# Scientific references of UncertRadio
`UncertRadio`

In a publication Part I [@kanisch2016a], an overview considering evaluations without using
linear unfolding is presented.

A way is shown of how to further formalize the equations necessary
for the evaluation of the output quantity. Apart from a set of equations by which the value
of the output quantity can be described, fulfilling few assumptions is sufficient to solve
all the necessary tasks:

i.   The sum a detection dependent background count rate $R_0$ and, if applicable, an interference
     count rate $R_{(\text{BI})}$ is subtracted. So, there is a linear dependency between $R_n$ and a
     gross counting rate $R_g$;
ii.	 The dependency between the output quantity $y$ (activity concentration) and $R_n$ is also
     linear, $y=F_C + F_L \cdot R_n$;
iii. The uncertainty of the gross counting rate $R_g$ can be expressed by a simple formula, in
     most cases simply as $u(R_g)=\sqrt{R_g /t_m}$ with a counting duration $t_m$.
     The gross count rate is the only one containing the contribution by the radionuclide
     activity which is to be measured.

$F_C$ usually is zero, but it may become negative in few special cases. The proportionality
factor $F_L$ represents a procedure-dependent calibration factor, i.$\,$e., the quantity $w$ used in
@DIN11929-1:2021. The symbols associated with the net counting rate and the gross count
rate need to be identified by the user in a program dialog.

\sout{Deriving values of decision threshold or detection limit requires finding other
values $\tilde R_n$ from given, or modified, values $\tilde y$ of the output quantity,
where \~ denotes such
modified values. The decision threshold, denoted by $y^*$, as an example, requires finding
the uncertainty $u(\tilde y=0)$ for the modified output quantity value $\tilde y=0$. The
above-mentioned linear relations allow to find the associated $\tilde R_g$ for which the
uncertainty value is $u(\tilde R_g)=\sqrt{\tilde R_g/t_m}$.
Re-starting the uncertainty calculation now from the
pair $\tilde R_g$ and $u(\tilde R_g)$ leads to the uncertainty $u(\tilde y=0)$ and
thereby to the decision threshold $y^* = k_{1-\alpha} \cdot u(\tilde y=0)$.
The detection limit, denoted by $y^\#$, is obtained by iteration from
the implicit relation $y^\# =y^* +k_{1-\beta} \cdot u(y^\#)$.}

In the publication Part II [@kanisch2016b] the extension of the method to such evaluation
models is described which require a weighted linear least-squares method (WLS) for a part
of the evaluation model. Now, there is a series of time-dependent measured net count rates
from the sample preparation available, which in most cases form a decay curve or a build-up
curve. Theoretical functions of such curves are defined from the Bateman equations for
radioactive decay of single radionuclides or of decay-chains of two radionuclides, like e.g.
the parent-daughter pair Sr-90, Y-90, encountered in the radiochemistry of Strontium isotopes.
In the present version of `UncertRadio`, up to three radionuclides are allowed for contributing
to the decay curve. A weighted least-squares fit of the function to the decay-curve is applied.

\sout{The $k (k\, \text{in} \, \{1,2,3\})$ fitted parameters represent count rate values
$R_{(n,k)} (t=0)$ defined for a
specific point of time $t=0$; the latter often is the time of a specific radiochemical separation
step. The values $R_{(n,k)} (t=0)$ and their uncertainties, also obtained from the fit, define
the first stage of the overall evaluation. In the second evaluation stage, these net count
rates are converted to activities or activity concentrations for the time of sampling by
applying procedure-dependent calibration factors including necessary corrections for radioactive
decay.}

\sout{The WLS step is performed by matrix-algebra. Basically, it is described by a matrix equation $x = \textbf{A}y$.
Herein, x and y are the vectors of the measured net count rates (number $n_x$) and of the fitting
parameters (number $n_y \leq 3$). The "design matrix" $\textbf{A}$ contains the different decay correction
functions. The elements of $\textbf{A}$ may include parameters with uncertainties, e.$\,$g.
half-lives or chemical yields. These require further uncertainty propagation steps,
also formulated in matrix notation.
The uncertainties of $x$ form the diagonal elements of a covariance matrix $\textbf{U}_x$. Depending on the
measurement conditions, $\textbf{U}_x$ may also contain covariances, i.$\,$e. non-diagonal elements.}

# ~~Option of testing physical units~~

\sout{It is advisable to state also physical units of the various variables of a project.
`UncertRadio` got an additional tool for testing the correctness of given units or the consistency of
units of independent and dependent variables. As the set of necessary units used in environmental
radioactivity is small, an algorithm was implemented which "calculates" units. In a specific input
file a set of "basic" units is defined and a set of other units related to corresponding basic units
by conversion factors, like 60 for converting minutes to seconds. Calculations of units is the
possible by assigning specific real numbers to basic units. This test algorithm is described in
section 7.21 of the CHM Help file.}

# Applications and examples
The program comes with a large set of about 100 example projects (which are structured text files),
also formulated in English and in German. They can be opened by `UncertRadio` and thereby gives the
user an impression of how, e.$\,$g. the set of equations is organized for the various measurement models.
Included in these examples are those examples taken from @DIN11929-1:2021, those
13 examples taken from @DIN11929-4:2021 and three examples from an IAEA Project [@international2004iaea].
A short overview of the example projects is given in section 3.3 of the CHM Help file.

One example file (Janszen-Sr-89-Sr-90_V4_EN.txp) handles a set of 32 equations with 73 variables.
Few other examples to be mentioned here are:

- Examples taken from the Metrology related publications;
- several examples of radiochemical determinations of Strontium-90 or Strontium-90/Strontium-89,
  with and without using linear unfolding, using beta counter as well as liquid scintillation counting
  with up to three energy windows; \autoref{fig:image1} shows an example of the decay curve model dialog;
- examples of evaluating Yttrium-90 decay curves for the Strontium-90 determination,
  where special possible interference situations are covered;
- a more complex example of alpha-spectrometry of Americium-241 with using two different tracers;
- examples of measuring neutron and photon doses;
- two gamma spectrometry examples refer to the measurement of a single radionuclide, where the
  determination of the activity and the characteristic limits is based on a weighted mean of
  several gamma line activities. \autoref{fig:image2} shows the associated dialog for input of the gamma lines.

The example project files coming with `UncertRadio` also contains the example of the IAEA AQ27
report but designt for using linear unfolding (file Sr89-Sr90_IAEA_AQ-27_2013_V2_EN.txp).
The associated `UncertRadio` results agree with those stated in the report.

Two other example files for a combined Sr-90 and Sr-89 determination, J-ALUFT-Sr89-Sr-90_V2_EN.txp
and J-ALUFT_Sr-89_Sr-90_Linf_EN.txp, demonstrate the complete agreement of the two versions, where
the second file uses linear unfolding.

These two comparisons demonstrate the agreement of the two versions. We suggest that generally the
linear unfolding method will be the more effective method.

For the context of measuring Tritium activities in air samples, it is demonstrated with two example
projects how the full evaluation of two output quantities, organic and water-soluble Tritium, is
set up for linear unfolding. The method of measurement is described by Duda et al. (2018).
The two example project files are Tritium_4Bubbler_used_1-3_EN.txp and Tritium_4Bubbler_used_2-3_EN.txp.

A special case arises if results of several individual measurements are to be combined into one
output quantity by applying a weighted mean. One example is given by a gamma-ray spectrometry
application, where a single radionuclide emits $n > 1$ gamma rays with different energies. For each
gamma-ray an individual activity value is obtained. It is desired to build a single weighted mean
from the $n$ individual activity values. The open question then often is, how to calculate the
associated decision threshold and detection limit. In `UncertRadio`, the method of linear unfolding
solves all this. Two examples project files (Several-peaks-nuclide-activity-V3_EN.txp and La140_REMSPEC-4Lines-V3_EN.txp)
were prepared which demonstrate how the evaluation equations are set up correspondingly.

The publication IAEA-AQ27 may serve as a literature example, which is often
referred to by other publications, like that of Holmgren Rondal and Ramebäck (2018). However, they
prefer to solve the "two equations" sequentially by building subtractions. This may lead to more
complicated uncertainty propagation formulae. A reason for avoiding the linear unfolding method might
generally be the application of matrix algebra for calculating the output quantity and its uncertainty.
When using `UncertRadio` for this type of application the matrix algebra is hidden to the user.

Another Report on radiostrontium determination methods (EPA, 2011) demonstrates, especially in its
Appendices A and B, the very significant and error-prone effort for setting up manually the various
formulae required for programming the uncertainty propagation. And these calculations did not yet
include those of detection limits. When using `UncertRadio` instead, the user would only the supply
the rather simple equations for activity calculations, the remaining effort, uncertainty propagation
and detection limit calculations, is taken by the program.

A procedures Manual for monitoring of radioactive substances in the environment
and of external radiation is [available online](https://www.bmuv.de/en/themen/strahlenschutz/ionisierende-strahlung/ueberwachung-der-radioaktivitaet-in-der-umwelt/procedures-manuals).
Within the individual procedures of the Manual, the necessary example calculations are carried out with
`UncertRadio`. In suitable applications, Excel files with embedded VBA code are also presented
and the results are compared with `UncertRadio`.
In addition, a collection of `UncertRadio` example project files, prepared
by Federal coordinating offices is [available](https://www.bmuv.de/en/themen/strahlenschutz/ionisierende-strahlung/ueberwachung-der-radioaktivitaet-in-der-umwelt/procedures-manuals/files).

![Details of the model of a WLS analysis of a more complex decay curve; 3 radionuclides, 3 energy windows; decay corrections show in the lower part. \label{fig:image1}](image1_LinfitWLS.png){width=100%}

![Dialog with details for calculating the activity from several gamma peaks taking self-attenuation and true-coincidence-summing corrections into account\label{fig:image2}](image2_spectrumanalysis.png){width=100%}


# Availability and documentation

`UncertRadio` is available free for download as compiled windows binaries since about 2014.
However, the source code was not open source until now.
As mentioned above, it has been used in serveral scientific publications, is integrated in
the standard @DIN11929-1:2021 series and actively used by the German monitoring authorities
for monitoring enviromental radioactivity. In order to ensure the future of the program, it was decided
to make UncertRadio accesible to the general public as open source software under the
GNU General Public License 3.

`UncertRadio` is written in modern Fortran utilizing many Fortran 2003 and 2008 features, e.$\,$g. the C-interoperability.
Therefore, a reasonably up-to-date Fortran compiler is required, whereby only Gfortran version 11 or
higher has been tested lately.

For the graphical user interface Gtk 3 is used with gtk-fortran [@magnin2019] providing the
required Fortran bindings. The GUI is designt whith the user
interface designer software @glade2022 and PLplot is implemented for graphical
presentations [@theplplotteam].

As of today, the `UncertRadio` source code consists of about 450 procedures covering
about $\text{48}\,\text{000}$ (non-comment) lines of code.
Many numerical procedure source codes were inspired from the work of
[Alan Miller](https://wp.csiro.au/alanmiller/;https://jblevins.org/mirror/amiller/),
by [John Burkardt](https://people.sc.fsu.edu/~jburkardt/) and by @brandt1999.

The source code is availiable on [GitHub](https://github.com/OpenBfS/UncertRadio).
A detailed building instruction is provided within the README file.
`UncertRadio` works on Linux and Windows. Nevertheless, only Windows
binaries are currently provided for download. They are available on GitHub and on the homepage of the
[Thünen-Institute](https://www.thuenen.de/en/institutes/fisheries-ecology/fields-of-activity/marine-environment/coordination-centre-of-radioactivity/uncertradio).

`UncertRadio` contains an extensive `compiled HTML help` (chm) file for the description of the program
features. It is available in a German and an English language version. The individual help topics
are available from within `UncertRadio` using various help buttons.
A sphinx documentation based on this help file is in preparation.

Code problems can be reported in the "issues" tab on [GitHub](https://github.com/OpenBfS/UncertRadio).
The authors are happy to help within their capabilities.
Feedback and contributions via pull request are greatly
appreciated.

# Conflict of interest
The authors declare no financial conflicts of interest.

# Acknowledgements
The authors would like to thank all those who have contributed to improving `UncertRadio` by
reporting problems and suggesting new features.
We thank also D. Schrammel (KIT, Karlsruhe, Germany) for many
comparison test calculations using MAPLE and Prof. R. Michel (formerly IRS, Hannover, Germany)
for further test calculations, who also applied `UncertRadio` in preparation of @DIN11929-4:2021.

Futhermore, the authors would especially like to thank Marco Lechner and Josef Peter, who have contributed
with their knowledge and constructive suggestions to make `UncertRadio` open source.

# References


<!-- # Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{if} x < 0\cr
1\textrm{else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text. -->

<!-- # Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@kanisch2016`  ->  "Author et al. (2001)"
- `[@kanisch2016]` -> "(Author et al., 2001)"
- `[@kanisch2016; @kanisch2017]` -> "(Author1 et al., 2001; Author2 et al., 2002)" -->

<!-- # Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% } -->
