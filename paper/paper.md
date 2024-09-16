---
title: '`UncertRadio`: Software for determining characteristic limits in accordance to DIN EN ISO 11929 for radioactivity measurements'
tags:
  - Environmental Radioactivity
  - Numerical uncertainty calculation
  - characteristic limits
  - DIN EN ISO 11929
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
 - name: Hamburg, Germany, formerly Thünen-Institute
   index: 1
 - name: Max Rubner-Institute, Kiel, Germany
   index: 2
 - name: Thünen-Institute, Bremerhaven, Germany
   index: 3

date: 27 August 2024
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
# aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
# aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

<!-- # Statement of need

`UncertRadio` is an

`UncertRadio` was designt -->

# Summary and statement of need

In Germany, radioactive substances in the environment are monitored in accordance with the
Treaty establishing the European Atomic Energy Community (EURATOM) of 1957 and the Radiation Protection Act
[@bundesrepublikdeutschland2017]. The Evaluation of radiometric measurements requires the
estimation of associated uncertainties as defined in the ISO$~$GUM$~$[@jointcommitteeforguidesinmetrology2008].
In addition, German law requires that the `characteristic limits` (decision threshold (DT) and the detection limit (DL)) are determined on the basis of this uncertainty in accordance with @DIN11929-1:2021 to @DIN11929-4:2021.

`UncertRadio` is the only publicly available software to
determine these values in a user-centralized way for a variety of applications
from Alpha, Beta and Gamma radiation measurements including dosimetry for up to three radionuclides simultaneously. Therefore, it is especially suited for modern liquid scintillation measurement procedures
of e.$\,$g. Strontium isotopes. The user only needs to define the evaluation model by providing a set of equations to calculate the output quantity value.
However, the required partial derivatives are calculated internally and don't need to
be supplied by the user.

There are two main analytical approaches used within the software:

- Procedures without linear unfolding: The basic evaluation model is linear in the net count rate.
  Thus, the output value can be calculated directly [@kanisch2016a].

- Procedures utilizing linear unfolding methods: The model additionally includes linear least squares procedures
  for fitting e.$\,$g. time-dependent decay or build-up curves [@kanisch2016b].

Additionally, the model can be evaluated with a Monte Carlo simulation [@DIN11929-2:2021]
(\autoref{fig:image0}). This represents the method of propagating whole distributions, which has
advantages if the distributions of the input variable distributions significantly
deviate from the normal distribution; see ISO GUM Supplements 1
and 2 [@jointcommitteeforguidesinmetrology2008a, @jointcommitteeforguidesinmetrology2011].

![Example of results obtained with the Monte Carlo simulation [@DIN11929-2:2021]. The results (output quantity, decision threshold and detection limit) calculated following @DIN11929-1:2021 are also included. \label{fig:image0}](UR2MC_EN.png){width=80%}

# Scientific references
`UncertRadio` has been used in several scientific publications. It is referenced by the standard
@DIN11929-1:2021 to @DIN11929-4:2021 series and actively used by the German authorities for monitoring
environmental radioactivity and external radiation.

In @kanisch2016a, an overview considering evaluations without using linear unfolding has been presented.
Two significant linear relationships in the model equations for the net count rate
(common in evaluation models) were identified providing a generalized approach for the determination
of the characteristic limits.
The second part [@kanisch2016b] extends the evaluation models to include
linear unfolding methods utilizing a (weighted) linear least-squares approach for
the first stage of the model. This step is solved using matrix-algebra which also takes parameters with uncertainties in the design matrix into account.

# Applications, examples and quality control
`UncertRadio` includes a set of approximately 70 example projects, which are
structured text files available in both English and German language.
They provide an impression of how the set of equations is organized for
various measurement models. A short overview of the example projects is
given in section 3.3 of the `UncertRadio` Help file. These examples contributed to the
validation of UncertRadio.
To check if UncertRadio is working correctly,
all examples can be run automatically by selecting "Options/QC batch test" in
the main menu.

# Availability and documentation

`UncertRadio` is available free for download as compiled windows binaries since 2014.

Recently, it was decided to make the source code
available as open source software under the GNU General Public License 3.
`UncertRadio` is written in modern Fortran utilizing many Fortran 2003 and 2008
features, e.$\,$g. the C-interoperability.

The graphical user interface is build with GTK 3 in combination with gtk-fortran [@magnin2019], which provides the required Fortran bindings. PLplot is implemented for the graphical presentations [@theplplotteam].

Many of the utilized numerical procedures are derived from the work of
[Alan Miller](https://wp.csiro.au/alanmiller/;https://jblevins.org/mirror/amiller/),
[John Burkardt](https://people.sc.fsu.edu/~jburkardt/) and @brandt1999.
A function parser [@schmehl2008] is included for interpreting user-defined equations.

The `UncertRadio` source code is available on [GitHub](https://github.com/OpenBfS/UncertRadio).
Detailed building instructions are provided within the README file.
`UncertRadio` works both on Linux and Windows and comes with language packages
for English, French and German. Nevertheless, only Windows
binaries are currently provided for download. They are available on GitHub and
on the homepage of the
[Thünen-Institute](https://www.thuenen.de/en/institutes/fisheries-ecology/fields-of-activity/marine-environment/coordination-centre-of-radioactivity/uncertradio).

`UncertRadio` contains an extensive `compiled HTML help` (chm) file for the description of the program
features in German and English language. The individual help topics
are available from within `UncertRadio` using various help buttons.
A sphinx documentation based on this help file is in preparation. The current development goals
and open issues can be found in the README file.

Code problems can be reported in the [issues](https://github.com/OpenBfS/UncertRadio/issues) tab on GitHub.
The authors are happy to help within their capabilities.
Feedback and contributions via [pull](https://github.com/OpenBfS/UncertRadio/pulls) request are greatly
appreciated.

# Conflict of interest
The authors declare no financial conflicts of interest.

# Acknowledgements
The authors thank all those who have contributed to improving `UncertRadio` by
reporting problems and suggesting new features.
We also thank D. Schrammel (KIT, Karlsruhe, Germany) for many
comparison test calculations and Prof. R. Michel (formerly IRS, Hannover, Germany),
who tested and applied `UncertRadio` during preparation of @DIN11929-4:2021.

Furthermore, the authors especially thank Marco Lechner and Josef Peter, who have contributed
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




<!-- It has been demonstrated earlier  how a generalized evaluation of a large
variety of environmental radioactivity measurements with and without using linear unfolding,
can be performed with a single program, `UncertRadio`. It is able to evaluate a set of
equations describing the measurement evaluation model, provided by the user during runtime,
through a function parser called fparser [@schmehl2008].


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
with two unknowns". -->

<!-- i.   The sum a detection dependent background count rate $R_0$ and, if applicable, an interference
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
to the decay curve. A weighted least-squares fit of the function to the decay-curve is applied. -->

<!-- The example project files coming with `UncertRadio` also contains the example of the IAEA AQ27
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
by Federal coordinating offices is [available](https://www.bmuv.de/en/themen/strahlenschutz/ionisierende-strahlung/ueberwachung-der-radioaktivitaet-in-der-umwelt/procedures-manuals/files). -->
