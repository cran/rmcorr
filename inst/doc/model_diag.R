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
library(gglm) 


## ----eval = FALSE-------------------------------------------------------------
#  #Install gglm
#  install.packages("gglm")
#  require(gglm)

## -----------------------------------------------------------------------------
raz.rmc <- rmcorr(participant = Participant, measure1 = Age, 
                  measure2 = Volume, dataset = raz2005) 

#Using gglm
 gglm(raz.rmc$model)

#using base R 
#plot(raz.rmc$model)

