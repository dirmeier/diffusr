<h1 align="center"> diffusr </h1>

[![Build Status](https://travis-ci.org/dirmeier/diffusr.svg?branch=master)](https://travis-ci.org/dirmeier/diffusr)
[![codecov](https://codecov.io/gh/dirmeier/diffusr/branch/master/graph/badge.svg)](https://codecov.io/gh/dirmeier/diffusr)
[![CRAN](http://www.r-pkg.org/badges/version/diffusr?color=brightgreen)](https://cran.r-project.org/package=diffusr)
[![Downloads](http://cranlogs.r-pkg.org/badges/diffusr?color=brightgreen)](https://cran.r-project.org/package=diffusr)

Network diffusion algorithms in R.

## Introduction

`diffusr` implements several algorithms for network diffusion such as *Markov random walks with restarts* and *weighted neighbor classification*. Network diffusion has been studied extensively in bioinformatics, e.g. in the field of cancer gene prioritization. Network diffusion algorithms generally spread information in the form of node weights along the edges of a graph to other nodes. These weights can for example be interpreted as temperature, an initial amount of water, the activation of neurons in the brain, or the location of a random surfer in the internet. The information (node weights) is iteratively propagated to other nodes until a equilibrium state or stop criterion occurs.

## Installation
 
Install `diffusr` using:
```{r}
install.packages("diffusr")
```

Alternatively use the latest version from github:
```{r}
devtools::install_github("dirmeier/diffusr") 
```

## Usage

Load the package using `library(diffusr)`. We provide a vignette for the package that can be called using: `vignette("diffusr")`.
Basically that is all you have to know.

## Author

* Simon Dirmeier <a href="mailto:mail@simon-dirmeier.net">mail@simon-dirmeier.net</a>
