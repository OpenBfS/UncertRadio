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


# Summary

In Germany, radioactive substances in the environment are monitored in accordance with the
Treaty establishing the European Atomic Energy Community (EURATOM) of 1957 and the
German Radiation Protection Act [@bundesrepublikdeutschland2017]. The evaluation of radiometric measurements requires the
estimation of associated uncertainties as defined in the ISO$~$GUM$~$[@jointcommitteeforguidesinmetrology2008].
In addition, German law requires that the `characteristic limits` (decision threshold (DT) and the detection limit (DL))
are determined on the basis of this uncertainty in accordance with ISO 11929-1-3:2025 [-@ISO.11929-1:2025; -@ISO.11929-2:2025; -@ISO.11929-3:2025] to ISO 11929-4:2022 [-@ISO.11929-4:2022].

# Statement of need

To the best of the authors' knowledge, `UncertRadio` is the only publicly available software to
determine the `characteristic limits` in a user-centralized way. `UncertRadio` can be used for a variety of applications
from alpha, beta and gamma radiation measurements including dosimetry for up to three radionuclides simultaneously. Therefore, it is especially suited for modern liquid scintillation measurement procedures
of e.$\,$g. strontium isotopes. The user only needs to define the evaluation model by
providing a set of equations in text-form to calculate the output quantity value.
The required partial derivatives are calculated internally.

There are two main analytical approaches used within the software:

- Procedures without linear unfolding: The basic evaluation model is linear in the net count rate.
  Thus, the output value can be calculated directly [@kanisch2016a].

- Procedures utilizing linear unfolding methods: The model additionally includes linear least squares procedures
  for fitting e.$\,$g. time-dependent decay or build-up curves [@kanisch2016b].

Additionally, the model can be evaluated with a Monte Carlo simulation following ISO 11929-2 [-@ISO.11929-2:2025]
(see \autoref{fig:image0}). This represents the method of propagating whole distributions, which has
advantages if the distributions of the input variable distributions significantly
deviate from the normal distribution; see ISO GUM Supplements 1 [-@jointcommitteeforguidesinmetrology2008a]
and 2 [-@jointcommitteeforguidesinmetrology2011].

![Example of results obtained with the Monte Carlo simulation using weighted linear least squares (WLS) in accordance with ISO 11929-2:2025 [@ISO.11929-2:2025]. The three displayed distributions refer, top-down, to the output quantity, the decision threshold, and the detection limit. The curves shown in blue represent the Gaussians calculated analytically [-@ISO.11929-1:2025]. The vertical dashed lines indicate the lower and upper limits of the coverage interval, with the mean value shown between them (upper graph); the decision threshold (middle graph); and both the decision threshold and detection limit (lower graph).\label{fig:image0}](UR2MC_EN.png){width=65%}

# Scientific references
`UncertRadio` has been used in several scientific publications. It is referenced by the standard
ISO 11929-1-3:2025 [-@ISO.11929-1:2025; -@ISO.11929-2:2025; -@ISO.11929-3:2025] to ISO 11929-4:2022 [-@ISO.11929-4:2022] and actively used by the German authorities for monitoring environmental radioactivity and external radiation.

In @kanisch2016a, an overview considering evaluations without using linear unfolding has been presented.
Two significant linear relationships in the model equations for the net count rate
(common in evaluation models) were identified providing a generalized approach for the determination
of the characteristic limits.
@kanisch2016b extends the evaluation models to include
linear unfolding methods utilizing a weighted linear least-squares (WLS) approach for
the first stage of the model. This step is solved using matrix-algebra which also takes parameters with uncertainties in the design matrix into account.

# Software design
`UncertRadio` is written in modern Fortran utilizing many Fortran 2003 and 2008
features, e.$\,$g. the C-interoperability. The graphical user interface (GUI) is built with GTK 3 in combination with gtk-fortran [@magnin2019], which provides the required Fortran bindings. PLplot is implemented for the graphical presentations [@theplplotteam].

Many of the utilized numerical procedures are derived from the work of @miller_web ,
@burkardt_web and @brandt1999. A Fortran function parser [@schmehl2008] is included
for interpreting user-defined equations.

The project utilizes a CMake‑based build system that automatically detects required dependencies, and produces both stand‑alone Windows binaries and Linux builds. Continuous integration is handled by GitHub Actions workflows, which compile the code on Linux and Windows, run the included tests and publish the built artifacts (for Windows). This ensures reproducible builds and enables rapid verification of every commit.

The documentation is built with `Sphinx` [@sphinx_web] from reStructuredText sources and is publicly hosted on the project's [GitHub Pages](https://openbfs.github.io/UncertRadio/) site.

# Applications, examples and quality control
`UncertRadio` includes a set of approximately 70 example projects, which are
structured text files available in both English and German languages.
They are mostly based on real-world applications but also cover the examples in ISO 11929-4 [-@ISO.11929-4:2022].
These examples illustrate the structure of the set of equations for various measurement models.
A short overview of all example projects is given in [section 2.5](https://openbfs.github.io/UncertRadio/doc_files/first_steps/example_projects.html) of the `UncertRadio` documentation.
These examples contributed to the validation of `UncertRadio`. To verify if `UncertRadio` is working correctly,
all examples can be run automatically by selecting "Options/QC batch test" in
the main menu. Since Version 2.6 this can also be done in the terminal by running:

```bash
./UncertRadio run_tests

```

Should an error occur, the authors would be grateful for any reports submitted via the GitHub [issues](https://github.com/OpenBfS/UncertRadio/issues) page.

# Availability and documentation
`UncertRadio` is available free for download as compiled Windows binaries since 2014.

Recently, it was decided to make the source code
available as open source software under the GNU General Public License 3.
The `UncertRadio` source code is available on [GitHub](https://github.com/OpenBfS/UncertRadio).
Detailed building instructions are provided within the README file.
`UncertRadio` works both on Linux and Windows and comes with language packages
for English, French and German. Nevertheless, only Windows
binaries are currently provided for download. They are available on GitHub and
on the homepage of the
[Thünen-Institute](https://www.thuenen.de/en/institutes/fisheries-ecology/fields-of-activity/marine-environment/coordination-centre-of-radioactivity/uncertradio).

Until Version 2.5.3 `UncertRadio` contained an extensive `compiled HTML help` (chm) documentation for the
description of the program features in German and English languages. However, since Version 2.6 it has been replaced by a modern Python `Sphinx` [@sphinx_web] based documentation, but is lacking some German
translations. Thus, the old chm version is still available in the repository or upon request.
The individual help topics are available from within `UncertRadio` using various help buttons.
In addition, the complete documentation is available on [GitHub Pages](https://openbfs.github.io/UncertRadio/).
The current development goals and open issues can be found in the [README](https://github.com/OpenBfS/UncertRadio/blob/main/README.md) file.

Code problems can be reported in the [issues](https://github.com/OpenBfS/UncertRadio/issues) tab on GitHub.
The authors offer to help within their capabilities.
Feedback and contributions via [pull](https://github.com/OpenBfS/UncertRadio/pulls) request are greatly
appreciated.

# Conflict of interest
The authors declare no financial conflicts of interest.

# Acknowledgements
The authors thank all those who have contributed to improving `UncertRadio` by
reporting problems and suggesting new features.
We also thank D. Schrammel (KIT, Karlsruhe, Germany) for many comparison test
calculations as well as Prof. R. Michel (formerly IRS, Hannover, Germany),
for testing and applying `UncertRadio` during the preparation of ISO 11929-4 [-@ISO.11929-4:2022].

Furthermore, the authors especially thank Marco Lechner and Josef Peter, who have contributed
with their knowledge and constructive suggestions to make `UncertRadio` open source.

# References
