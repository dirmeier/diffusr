<h1 align="center"> diffusr </h1>

[![Build Status](https://travis-ci.org/dirmeier/diffusr.svg?branch=master)](https://travis-ci.org/dirmeier/diffusr)
[![codecov](https://codecov.io/gh/dirmeier/diffusr/branch/master/graph/badge.svg)](https://codecov.io/gh/dirmeier/diffusr)
[![CRAN](http://www.r-pkg.org/badges/version/diffusr)](https://cran.r-project.org/package=diffusr)
[![Downloads](http://cranlogs.r-pkg.org/badges/diffusr?color=brightgreen)](https://cran.r-project.org/package=diffusr)

https://cranlogs.r-pkg.org/badges/diffusr
Network diffusion algorithms in R.

## Introduction

`diffusr` implements several algorithms for network diffusion such as *Markov random walks with restarts* and *weighted neighbor classification*. Network diffusion has been studied extensively in bioinformatics, e.g. in the field of cancer gene prioritization.
To my knowledge only few packages implement all of the diffusion algorithms. In the first version of `diffusr` I implemented the two methods above, several others will follow in future patches.

## Installation
 
Install `diffusr` using:
```{r}
devtools::install_github("dirmeier/diffusr") 
```
from the R-console.

## Usage

Load the package using `library(diffusr)`. We provide a vignette for the package that can be called using: `vignette("diffusr")`.
Basically that is all you have to know.

## Author

* Simon Dirmeier <a href="mailto:simon.dirmeier@bsse.ethz.ch">simon.dirmeier@bsse.ethz.ch</a>
