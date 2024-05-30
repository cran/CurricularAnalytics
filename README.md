
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CurricularAnalytics

CurricularAnalytics is an R package that provides comprehensive
functionality for implementing a Curricular Analytics framework in
university curricula.

## Features

- **Metric Calculations:** CurricularAnalytics includes a collection of
  functions to calculate important metrics such as the delay factor,
  blocking factor, centrality, and structural complexity. These metrics
  provide quantitative measures of various aspects of your curriculum,
  helping you assess its efficiency and effectiveness.

- **Curriculum Graph Manipulation:** This package provides intuitive
  functions to create, manipulate, and analyze curriculum graphs. You
  can easily construct curriculum graphs from your own data or existing
  formats and perform operations like adding or removing courses,
  modifying prerequisites, and more.

- **Visualization:** CurricularAnalytics offers a range of visualization
  options to help you explore and present your curriculum data
  effectively. You can generate visual representations of curriculum
  graphs, highlighting important nodes and edges, to gain a visual
  understanding of your curriculum’s structure.

## Installation

To install CurricularAnalytics, you can use the `devtools` package for
the latest unstable version or CRAN for the latest stable version:

``` r
# unstable
devtools::install_github("Danyulll/CurricularAnalytics")

# stable
install.packages("CurricularAnalytics")
```

## Getting Started

Once you have installed CurricularAnalytics, you can import it into your
R environment and start utilizing its functionalities. We have provided
detailed documentation and examples in the vignette to help you get
started quickly.

``` r
vignette("Introduction to Curricular Analytics")
```

## Further Reading and Future Plans

For a complete introduction to the topic of Curricular Analytics please
see (Heileman et al. 2018). Currently CurricularAnalytics only
implements the concepts found in the above paper. There are future plans
to implement predictive models and an interactive R Shiny app based on
the metrics in this package.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-heileman2018curricular" class="csl-entry">

Heileman, Gregory L, Chaouki T Abdallah, Ahmad Slim, and Michael
Hickman. 2018. “Curricular Analytics: A Framework for Quantifying the
Impact of Curricular Reforms and Pedagogical Innovations.” *arXiv
Preprint arXiv:1811.09676*.

</div>

<div id="ref-hickman2017development" class="csl-entry">

Hickman, Michael S. 2017. “Development of a Curriculum Analysis and
Simulation Library with Applications in Curricular Analytics.”

</div>

<div id="ref-slim2021restructuring" class="csl-entry">

Slim, Ahmad, Gregory L Heileman, Chaouki T Abdallah, Ameer Slim, and
Najem N Sirhan. 2021. “Restructuring Curricular Patterns Using Bayesian
Networks.” In *EDM*.

</div>

</div>
