## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = FALSE)
options(width = 80)
library("knitr", "rmarkdown", "rmcorr") 
require(knitr)
require(rmarkdown)
require(rmcorr)
require(psych)

#Kludge to add cocor b/c of lib loc error 
#if(!require("cocor"))
#  install.packages("cocor")

## -----------------------------------------------------------------------------
brainvolage.rmc <- rmcorr(participant = Participant, measure1 = Age, measure2 = Volume, dataset = raz2005)

#Old, incorrect CI using error df
psych::r.con(brainvolage.rmc$r, brainvolage.rmc$df, p = 0.95)

#Current, correct CI using effective sample size
brainvolage.rmc$CI

#Manual calculation
psych::r.con(brainvolage.rmc$r, brainvolage.rmc$df + 2, p = 0.95)

## -----------------------------------------------------------------------------
#Old, incorrect CI using error df
psych::r.con(0.88, 4, p = 0.95)

#Current, correct CI using effective sample size
psych::r.con(0.88, 6, p = 0.95)

## -----------------------------------------------------------------------------
#Old, incorrect CI using error df
psych::r.con(0.88, 19, p = 0.95)

#Current, correct CI using effective sample size
psych::r.con(0.88, 21, p = 0.95)

