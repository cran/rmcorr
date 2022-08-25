## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = FALSE)
options(width = 80)
library("knitr", "rmarkdown", "rmcorr") #, "stats")
require(knitr)
require(rmarkdown)
require(rmcorr)

## -----------------------------------------------------------------------------
my.rmc <- rmcorr(participant = Subject, measure1 = PaCO2, measure2 = pH, 
                 dataset = bland1995)
 
# Structure of rmcorr object
#str(my.rmc)
 
# Extract rmcorr model coefficients
coef.rmc  <- my.rmc$model$coefficients
coef.rmc
 
slope.rmc <- coef.rmc[length(coef.rmc)] #Last value in coefficients is the slope
slope.rmc
 
# Confidence intervals around all estimates
coef.CIs <- stats::confint(my.rmc$model) 
coefs.all <- cbind(coef.rmc, coef.CIs)
coefs.all

