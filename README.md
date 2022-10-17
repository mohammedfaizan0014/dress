
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!--# fable <a href='https://fable.tidyverts.org'><img src='man/figures/logo.png' align="right" height="138.5" /></a>-->
<!-- badges: start -->

[![R-CMD-check](https://github.com/mohammedfaizan0014/dress/workflows/R-CMD-check/badge.svg)](https://github.com/mohammedfaizan0014/dress/actions)
<!--[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fable)](https://cran.r-project.org/package=fable)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html) -->
<!-- badges: end -->

The R package `dress` provides some measures for disclosure risk
associated with the release of protected data, irrespective of what
mechanism was used to protect it. Key principles of the disclosure
framework include distinctness, accuracy and un-deniability. This method
can be applied to any pair of original and protected data-sets despite a
difference in dimensionality and without assuming any particular joint
probability structure between the original and protected data.

## Installation

You can install the **stable** version from
[CRAN](https://cran.r-project.org/package=dress):

``` r
install.packages("dress")
```

You can install the **development** version from
[GitHub](https://github.com/mohammedfaizan0014/dress)

``` r
# install.packages("remotes")
remotes::install_github("mohammedfaizan0014/dress")
```

Installing this software requires a compiler

## Example

``` r
library(svMisc)
library(dress)
library(sdcMicro)


# ##################
# ##all continuous###################
CASC_sample <- CASCrefmicrodata[,c(2,3,4,6)]
CASC_protected <- addNoise(CASC_sample,noise = 100)$xm #Additive Noise protected

DRisk_NN <- drscore(
  Sample = CASC_sample, #Original Sample
  Protected = CASC_protected,
  delta = 0.05,
  kdistinct = 0.05, #k distinct threshold if integer then
                 # probability threshold is k/SS (SS = sample size)
  ldeniable = 5, # l undeniable threshold if integer then
                         # probability threshold is l/SS (SS = sample size)
  neighbourhood = 1,
  #Possible 'neighbourhood' types
  # 1 = Mahalanobis (Based on Mahalanobis Distance)
  # 2 = DSTAR   (Based on Density Based Distance)
  # 3 = StdEuclid (Based on Standardised (by std dev) Euclidean Distance)
  # 4 = RelEuclid (Relative Euclidean Distance sum_k ((Xk-Yk)/Xk)^2)
  neigh_type = 'prob',
  #Possible 'neigh_type' types
  #constant = fixed threshold on distance
  #prob = Nearest Neighbour Probability Neighbourhood used (Worst Case Scenario 1)
  #estprob = = Nearest Neighbour Probability Neighbourhood used based on protected density (Worst Case Scenario 2)
  numeric.vars = 1:4, #Which Variables are continuous?
  outlier.par = list(centre = median,
                     scale = var,
                     thresh = 0.01)
  #Parameters to adjust how MV outliers are determined.
  #Default is that lie 99% (based on Chi-Square n-1 dist) away from median after scale by variance.
)
#> 
#> ###################################################################### 
#> #                     Disclosure Risk Assessment                     # 
#> ###################################################################### 
#> Nearest Neighbour Neighbourhood with parameters:
#>         delta = 0.05, kdistinct = 0.05, ldeniable = 0.00462962962962963. 
#> 
#> Number of Observations in the Sample                        1080
#> Number of Observations in the Protected Sample              1080
#> Number of Continuous Variables                              4
#> Number of Key Categories                                    1
#> Number of Outliers in Sample                                38
#> Number of Distinct Points in Sample                         1080
#> Number of Distinct Outliers in Sample                       38
#> Number of Exact Matches in Sample                           0
#> Number of Interval Matches in Sample                        0
#> Number of Outlier Interval Matches in Sample                0
#> Number of Distint Outlier Interval Matches in Sample        0 
#>  
#> Delta Disclosure Risk of Sample                             0.1102
#> Delta Disclosure Risk of Sample Outliers                    0.7895
#> Proportion Distinct                                         1
#> Proportion Estimated                                        0.8981
#> Proportion Undeniable                                       0.1102 
#>  
#> Category Level Disclosure Risk: 
#>  
#>     N.Obs     DRisk  Out_DRisk Distinct Estimated Undeniable
#> All  1080 0.1101852 0.02777778        1 0.8981481  0.1101852

#Update neighbourhood to fixed threshold definition
DRisk_Fxd <- update(DRisk_NN,neigh_type = 'constant',
                          delta = 1)
#> 
#> ###################################################################### 
#> #                     Disclosure Risk Assessment                     # 
#> ###################################################################### 
#> Threshold Neighbourhood with parameters:
#>         delta = 1, kdistinct = 0.05, ldeniable = 0.00462962962962963. 
#> 
#> Number of Observations in the Sample                        1080
#> Number of Observations in the Protected Sample              1080
#> Number of Continuous Variables                              4
#> Number of Key Categories                                    1
#> Number of Outliers in Sample                                38
#> Number of Distinct Points in Sample                         642
#> Number of Distinct Outliers in Sample                       38
#> Number of Exact Matches in Sample                           0
#> Number of Interval Matches in Sample                        6
#> Number of Outlier Interval Matches in Sample                0
#> Number of Distint Outlier Interval Matches in Sample        0 
#>  
#> Delta Disclosure Risk of Sample                             0.1463
#> Delta Disclosure Risk of Sample Outliers                    0.6842
#> Proportion Distinct                                         0.5944
#> Proportion Estimated                                        0.9546
#> Proportion Undeniable                                       0.15 
#>  
#> Category Level Disclosure Risk: 
#>  
#>     N.Obs     DRisk  Out_DRisk  Distinct Estimated Undeniable
#> All  1080 0.1462963 0.02407407 0.5944444 0.9546296       0.15
```

## Learning the Mathematics

-   *[On the Disclosure Risk Framework for Micro-Level Data]()*
    <!-- the paper  -->

## Getting help

-   Common questions about dress package are often found on
    [](https://stackoverflow.com/).
