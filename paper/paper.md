---
title: 'UncertRadio: '
tags:
  - Numerical Uncertainy calculation
  - Environmental Radioactivity
  - Gamma-ray spectrometry
  - DIN ISO 11929
  - Monte Carlo
authors:
  - name: Günter Kanisch
    corresponding: true # (This is how to denote the corresponding author)
    equal-contrib: true
  - name: Florian Ober
    orcid: 0000-0000-0000-0000
    affiliation: 1
  - name: Marc-Oliver Aust
    affiliation: 2
affiliations:
 - name: Max Rubner-Institut
   index: 1
 - name: Thünen Institut
   index: 2

date: xx January 2024
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
# aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
# aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary


# Statement of need

`UncertRadio` is an 

`UncertRadio` was designed t

# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text.

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@kanisch2016`  ->  "Author et al. (2001)"
- `[@kanisch2016]` -> "(Author et al., 2001)"
- `[@kanisch2016; @kanisch2017]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements


# References
