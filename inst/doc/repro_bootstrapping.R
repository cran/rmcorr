## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = FALSE)
options(width = 80)
library(knitr)
library(rmarkdown)
library(rmcorr) 


## -----------------------------------------------------------------------------
set.seed(532) 
boot.blandrmc <- rmcorr(Subject, PaCO2, pH, bland1995,
                        CIs = "bootstrap",
                        nreps = 100, 
                        bstrap.out = T)
boot.blandrmc

## -----------------------------------------------------------------------------
boot.rmcorr.samplingdist <- round(boot.blandrmc$resamples, digits = 2)
boot.rmcorr.mean <- mean(boot.blandrmc$resamples)
boot.rmcorr.median <- median(boot.blandrmc$resamples)

x.vals <- sprintf("%.2f", seq(-0.80, 0.00, by = 0.10))

hist(boot.rmcorr.samplingdist, 
     main = "Sampling Distribution of Bootstrapped Effect Sizes",
     xaxt = "n",
     xlab = "Effect Size",
     las = 1)
abline(v = boot.rmcorr.mean,   col = "red",  lwd = 2)
abline(v = boot.rmcorr.median, col = "blue", lwd = 2)
axis(1, 
     at = as.numeric(x.vals),
     labels = x.vals) 

#Compare point-est effect for bootstrap vs. non-bootstrap model
#Boostrapped effect sizes
#Mean
boot.rmcorr.mean
#Median
boot.rmcorr.median

#Non-bootstrapped 
blandrmc <- rmcorr(Subject, PaCO2, pH, bland1995)
blandrmc$r


