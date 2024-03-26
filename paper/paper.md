---
title: 'UncertRadio: Software for determining characteristic limits in accordance to ISO 11929 for radioactivity measurements'
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

In Germany, radioactive substances in the environment are monitored in accordance with the
Treaty establishing the European Atomic Energy Community (EURATOM) of 1957 and the Radiation Protection Act
[@bundesrepublikdeutschland2017]. Evaluations of the measurement of environmental radionuclide
activity concentrations, and estimating associated uncertainties, are based on the
ISO GUM [@jointcommitteeforguidesinmetrology2008] and @DIN11929-1:2021 series.
The basic part of ISO 11929 requires deriving values of the decision threshold (DT)
and the detection limit (DL), also summarized under "characteristic values" in the standard.
This is usually not covered by other uncertainty-only related, especially publicly available software.
`UncertRadio` solves this problem in a user-centralized way.

It can be used for a variety of applications from Alpha, Beta and Gamma measurements, but also
from dosimetry. It has the capability to derive the characteristic limits for up to three
radionuclides simultaneously, whose output quantity values, e.$\,$g. activity values, of which are
dependent from each other due to the measurement. Therefore, it is especially suited for modern
liquid scintillation measurement procedures of e.$\,$g. Strontium isotopes.
Applying `UncertRadio` means that the user needs to formulate the evaluation model used to
calculate all values characterising the output quantity. However, a special advantage is,
that no partial derivatives have to be supplied.

There are two main analytical approaches used within the software:

- procedures without linear unfolding: the basic evaluation model is linear in the net count rate [@kanisch2016a]
  and can be calculated in a rather direct way,

- procedures with linear unfolding: the model in addition includes linear least squares
  procedure for fitting e.$\,$g. for time-dependent decay or build-up curves [@kanisch2016b].

Alternatively, an evaluation by Monte Carlo simulation may be used within both approaches.
This represents the method of propagating whole distributions is in advantage in the case of
significant deviations from the normal distribution; see ISO GUM
Supplements 1 [@jointcommitteeforguidesinmetrology2008a] and 2 [@jointcommitteeforguidesinmetrology2011].


# Scientific references
`UncertRadio` has been used in several scientific publications. It is integrated in the standard DIN ISO 11929
[@DIN11929-1:2021] - [@DIN11929-4:2021] series and actively used by the German authorities for monitoring
environmental radioactivity.

In @kanisch2016a, an overview considering evaluations without using
linear unfolding is presented. In this study, two significant linear relationships in the model
equations that are common to all evaluation models were evaluated, allowing the determination of
characteristic limits.
The second part [@kanisch2016b] describes the extension to such evaluation models,
which require a weighted linear least-squares method (WLS) for a part of the evaluation model.
The WLS step is performed by matrix-algebra also including parameters with uncertainties,
which require further uncertainty propagation steps, also formulated in matrix notation.

Some selected projects using `UncertRadio`:

- @DIN11929-4:2021: All examples for Monte Carlo simulations were performed with `UncertRadio`,

- Fast method for the determination of radiostrontium and plutonium isotopes in food samples [@dolique2019],

- Spreadsheet design and validation for characteristic limits determination in gross alpha and beta measurement
  [@etde_22274408]

- Online procedures manual for monitoring of radioactive substances in the environment and
  of external radiation. Within the individual procedures of the manual,
  the necessary example calculations are carried out with `UncertRadio`.

- Study on the uncertainty of passive area dosimetry systems for environmental
  radiation monitoring in the framework of the EMPIR "Preparedness" project [@iurlaro2021]

- International Electrotechnical Commission, 2023: Radiation Protection Instrumentation -
  Determination of Uncertainty in Measurement [@behrens2023]


# Applications and examples
The `UncertRadio` program includes approximately 70 example projects, which are
structured text files available in both English and German.
They give the user an impression of how the set of equations is organized for
the various measurement models. Included examples were taken from @DIN11929-1:2011,
13 examples were taken from @DIN11929-4:2021 and three examples from an IAEA
Project [@international2004iaea]. A short overview of the example projects is
given in section 3.3 of the `UncertRadio` CHM Help file.

Many of the application examples originate from the working group "AK-SIGMA" of the
"German-Suisse Association for Radiation Protection", from Measuring Instructions
of the German Co-ordinating Offices, from a German National Supplement 1 of
DIN ISO 11929 [@DIN11929_Supp12014] and from the literature. These examples contributed to the
validation of UncertRadio.

Few other included examples to be mentioned are:

- radiochemical determinations of Strontium-90 or Strontium-90/Strontium-89,
  with and without using linear unfolding, using beta counter as well as liquid
  scintillation counting with up to three energy windows; \autoref{fig:image1} shows an example
  of the decay curve model dialog;

- a complex example of alpha-spectrometry of Americium-241 with using two different tracers;
  gamma spectrometry measurement of a single gamma-ray emitting radionuclide, where
  the determination of the activity and the characteristic limits is based on a
  weighted mean of several gamma line activities. \autoref{fig:image2} shows the associated
  dialog for input of the gamma lines.


![Details of the model of a WLS analysis of a more complex decay curve; 3 radionuclides, 3 energy windows; decay corrections show in the lower part. \label{fig:image1}](image1_LinfitWLS.png){width=100%}

![Dialog with details for calculating the activity from several gamma peaks taking self-attenuation and true-coincidence-summing corrections into account\label{fig:image2}](image2_spectrumanalysis.png){width=100%}


# Availability and documentation

`UncertRadio` is available free for download as compiled windows binaries since 2014.

To preserve the future of the `UncertRadio`, it was recently decided to make the source
available as open source software under the GNU General Public License 3. It is written in modern
Fortran utilizing many Fortran 2003 and 2008 features, e.$\,$g. the C-interoperability.
Therefore, a reasonably up-to-date Fortran compiler is required, whereby only Gfortran
version 11 or higher has been tested lately.


For the graphical user interface Gtk 3 is used with gtk-fortran [@magnin2019] providing the
required Fortran bindings. The GUI is designt whith the user
interface designer software @glade2022 and PLplot is implemented for graphical
presentations [@theplplotteam].

As of today, the `UncertRadio` source code consists of about 450 procedures covering
about $\text{48}\,\text{000}$ (non-comment) lines of code.
Many numerical procedure source codes were inspired from the work of
[Alan Miller](https://wp.csiro.au/alanmiller/;https://jblevins.org/mirror/amiller/),
by [John Burkardt](https://people.sc.fsu.edu/~jburkardt/) and by @brandt1999.

The `UncertRadio` source code is availiable on [GitHub](https://github.com/OpenBfS/UncertRadio).
A detailed building instruction is provided within the README file.
`UncertRadio` works on Linux and Windows. Nevertheless, only Windows
binaries are currently provided for download. They are available on GitHub and on the homepage of the
[Thünen-Institute](https://www.thuenen.de/en/institutes/fisheries-ecology/fields-of-activity/marine-environment/coordination-centre-of-radioactivity/uncertradio).

`UncertRadio` contains an extensive `compiled HTML help` (chm) file for the description of the program
features. It is available in a German and an English language version. The individual help topics
are available from within `UncertRadio` using various help buttons.
A sphinx documentation based on this help file is in preparation. The current development goals
and open issues can be found in the readdme file.

Code problems can be reported in the "issues" tab on [GitHub](https://github.com/OpenBfS/UncertRadio).
The authors are happy to help within their capabilities.
Feedback and contributions via pull request are greatly
appreciated.

# Conflict of interest
The authors declare no financial conflicts of interest.

# Acknowledgements
The authors would like to thank all those who have contributed to improving `UncertRadio` by
reporting problems and suggesting new features.
We also thank D. Schrammel (KIT, Karlsruhe, Germany) for many
comparison test calculations utilizing the @maple_software and Prof. R. Michel (formerly IRS, Hannover, Germany)
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
