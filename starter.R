

## Some needed packages and libraries
rm(list=ls())

p_needed <- c("knitr", "MASS", "separationplot", "ggplot2", "reshape2", "ggthemes","brglm","readr","haven",
              "MASS","lmtest","sandwich","stargazer","lme4","tidyverse","Zelig","sjstats","countrycode","dplyr","tidyr",
              "stringr","plyr","heatmapFit","plotROC","pROC","gridExtra")
            
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)


## Some needed functions for future use. 

## Function for summary table. 

summaryTable <- function (x) {
  
  var <- na.omit(x)
  obs <- length(var)
  mean <- mean(var)
  std <- sd(var)
  min <- min(var)
  max <- max(var)
  median <- median(var)
  
  return(round(c(obs,mean,std,min,max,median),3))
}

## A function to get the mean of personalism level of each group in grouped data.

getMean <- function (x) {mean(x[,"personalism"])}



## Functions for simulations. 
response_function <- function(x) {
  1 / (1 + exp(-x))
}
stochastic_component <- function(ndraws, p) {
  rbinom(n = ndraws, size = 1, prob = p)
}
sim_function <-
  function(seed = 17,
           nsim = 1000,
           coefs,
           vcov,
           scenario,
           response_function,
           predicted_values = F,
           stochastic_component) {
    if (is.null(dim(scenario))) {
      stop("The scenario needs to be in a matrix.")
    }
    if (length(coefs) != ncol(scenario)) {
      stop("The scenario and the parameter vector don’t fit.")
    }
    set.seed(seed)
    # Set up the sampling distribution
    S <- mvrnorm(nsim, coefs, vcov)
    # Calculate mu and p
    # The linear component
    mu <- S %*% t(scenario)
    # The response function
    ev <- response_function(mu)
    if (predicted_values) {
      pv <-
        array(stochastic_component(ndraws = prod(dim(ev)),
                                   p = ev), dim = dim(ev))
      return(list(ev = ev, pv = pv))
    }
    return(list(ev = ev))
  }


