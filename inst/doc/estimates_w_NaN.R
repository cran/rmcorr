## ----include = FALSE----------------------------------------------------------
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
library(ggplot2)
library(ggExtra) 


## ----eval = FALSE-------------------------------------------------------------
#  install.packages("ggExtra")
#  require(ggExtra)

## -----------------------------------------------------------------------------
load(file = "../man/data/ghosh_synth.rda")

#Look at data
ghosh_synth #Note lots of repeated zeros in A3 and A4

set.seed(40) #Make jittering reproducible 
p <- ggplot(ghosh_synth, aes(x = A4, y = A3)) +
            geom_point(alpha = 0.2) +
            geom_jitter(width = 2, height = 2) 
p1 <- ggMarginal(p, type="histogram")
p1

rmc.ghosh <- rmcorr(Subject, A3, A4, ghosh_synth)

rmc.ghosh

#The default rmcorr plot doesn't jitter values, this masks identical values because they are drawn on top of each other
plot(rmc.ghosh)

## -----------------------------------------------------------------------------
set.seed(67) 
small.noise1 <- rnorm(dim(ghosh_synth)[[1]], 0, 0.2)
small.noise2 <- rnorm(dim(ghosh_synth)[[1]], 0, 0.2)
    
ghosh_synth$A3.noise <- ghosh_synth$A3 + small.noise1
ghosh_synth$A4.noise <- ghosh_synth$A4 + small.noise2

rmc.ghosh.noise <- rmcorr(Subject, A3.noise, A4.noise, ghosh_synth)

rmc.ghosh.noise

p2 <- ggplot(ghosh_synth, aes(x = A3.noise, y = A4.noise, 
       group = factor(Subject), color = factor(Subject))) +
       ggplot2::geom_point(ggplot2::aes(colour = factor(Subject), 
                                        alpha = 0.10)) +
       ggplot2::geom_line(aes(y = rmc.ghosh.noise$model$fitted.values),
                         linetype = 1) +
     theme(legend.position="none")

p3 <- ggMarginal(p2, type="histogram")
p3

