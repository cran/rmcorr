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

#Kludge to add corrplot b/c of lib loc error 
if(!require("corrplot"))
  install.packages("corrplot")
require(corrplot)

## ---- eval = FALSE------------------------------------------------------------
#  #Install corrplot
#  install.packages("corrplot")
#  require(corrplot)

## -----------------------------------------------------------------------------
dist_rmc_mat <- rmcorr_mat(participant = Subject, 
                           variables = c("Blindwalk Away",
                                         "Blindwalk Toward",
                                         "Triangulated BW",
                                         "Verbal",
                                         "Visual matching"),
                           dataset = twedt_dist_measures,
                           CI.level = 0.95)

corrplot(dist_rmc_mat$matrix)

## -----------------------------------------------------------------------------
#Number of models being plotted
n.models <- length(dist_rmc_mat$models)

#Change graphing parameters to plot side-by-side
#with narrower margins
par(mfrow = c(3,4), 
    mar = c(2.75, 2.4, 2.4, 1.4))

for (i in 1:n.models) {
    plot(dist_rmc_mat$models[[i]])
    }

#Reset graphing parameters
#dev.off()

## -----------------------------------------------------------------------------
#Third component: Summary
dist_rmc_mat$summary

#p-values only
dist_rmc_mat$summary$p.vals

#Vector of original, unadjusted p-values for all 10 comparisons
p.vals <- dist_rmc_mat$summary$p.vals

p.vals.bonferroni <- p.adjust(p.vals, 
                              method = "bonferroni",
                              n = length(p.vals))

p.vals.fdr <- p.adjust(p.vals, 
                       method = "fdr",
                       n = length(p.vals))

#All p-values together
all.pvals <- cbind(p.vals, p.vals.bonferroni, p.vals.fdr)
colnames(all.pvals) <- c("Unadjusted", "Bonferroni", "fdr")
round(all.pvals, digits = 5)

