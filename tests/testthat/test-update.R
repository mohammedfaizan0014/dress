#Testing on data frame values

#data frame, numeric values without missing value

#load library
library(testthat)
library(assertthat)
library(dress)
library(sdcMicro)

test_that("Class of Parameters Update", {

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

  #Update neighbourhood to fixed threshold definition
  DRisk_Fxd <- update(DRisk=DRisk_NN,neigh_type = 'constant',
                      delta = 1)

  expect_s3_class(DRisk_Fxd, "DRisk")

})


