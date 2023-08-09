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
library(cocor) 


## ---- eval = FALSE------------------------------------------------------------
#  #Install cocor
#  install.packages("cocor")
#  require(cocor)

## -----------------------------------------------------------------------------
#1) Run rmcorr on two different datasets
model1.marusich2016_exp2  <- rmcorr(Pair, HVT_capture, MARS, marusich2016_exp2)
model1.marusich2016_exp2

model2.gilden2010         <- rmcorr(sub, rt, acc, gilden2010 )
model2.gilden2010

#2) Extract relevant parameters
#Model 1
rmcorr1 <- model1.marusich2016_exp2$r
rmcorr1

n1 <- model1.marusich2016_exp2$df + 2 #note the same kludge as power above
n1                                    #this is the effective sample size

#Model 2
rmcorr2 <- model2.gilden2010$r
rmcorr2

n2 <- model2.gilden2010$df + 2 
n2

#3) Compare the two indendent rmcorr coefficients
cocor.indep.groups(rmcorr1, rmcorr2, n1, n2, 
                   var.labels = c(model1.marusich2016_exp2$var[2:3],
                                  model2.gilden2010$vars[2:3]))


## -----------------------------------------------------------------------------
variables.overlap<- c("Blindwalk Away",
                      "Blindwalk Toward",
                      "Triangulated BW")

dist_rmc_mat_overlap <- rmcorr_mat(participant = Subject, 
                                   variables = variables.overlap, 
                                   dataset = twedt_dist_measures,
                                   CI.level = 0.95)

#dist_rmc_mat_action$summary

#Use summary component 
model1.bwa.bwt <- dist_rmc_mat_overlap$summary[1,] 
model2.bwa.tri <- dist_rmc_mat_overlap$summary[2,]
model3.bwt.tri <- dist_rmc_mat_overlap$summary[3,]

r.jk <- model1.bwa.bwt$rmcorr.r
r.jh <- model2.bwa.tri$rmcorr.r #overlap
r.kh <- model3.bwt.tri$rmcorr.r


#Since there is missing data, the results are unbalanced. We use the average effective sample size.
n    <- mean(dist_rmc_mat_overlap$summary$effective.N)

cocor.dep.groups.overlap(r.jk, 
                         r.jh, 
                         r.kh, 
                         n, 
                         alternative = "two.sided", 
                         test = "all", 
                         var.labels = variables.overlap) #Same as variables used in rmcorr_mat()

## -----------------------------------------------------------------------------
variables.nonoverlap  <- c("Blindwalk Away",
                           "Blindwalk Toward",
                           "Verbal",
                           "Visual matching")

dist_rmc_mat_nonoverlap <- rmcorr_mat(participant = Subject, 
                                      variables = variables.nonoverlap, 
                                      dataset = twedt_dist_measures,
                                      CI.level = 0.95)

dist_rmc_mat_nonoverlap$summary

#Use summary component 
model1.bwa.bwt   <- dist_rmc_mat_nonoverlap$summary[1,] 
model2.verb.vis  <- dist_rmc_mat_nonoverlap$summary[6,]
model3.bwa.verb  <- dist_rmc_mat_nonoverlap$summary[2,]
model4.bwa.vis   <- dist_rmc_mat_nonoverlap$summary[3,] 
model5.bwt.verb  <- dist_rmc_mat_nonoverlap$summary[4,] 
model6.bwt.vis   <- dist_rmc_mat_nonoverlap$summary[5,] 

#Cheatsheet
    #j = bwa
    #k = bwt
    #h = verb
    #m = vis

r.jk <- model1.bwa.bwt$rmcorr.r  #Action measures
r.hm <- model2.verb.vis$rmcorr.r #Direct measures
r.jh <- model3.bwa.verb$rmcorr.r #bwa ~ verb
r.jm <- model4.bwa.vis$rmcorr.r  #bwa ~ vis
r.kh <- model5.bwt.verb$rmcorr.r #bwt ~ verb
r.km <- model6.bwt.vis$rmcorr.r  #bwt ~ vis

#Since there is missing data, we use the average effective sample size.
n    <- round(mean(dist_rmc_mat_nonoverlap$summary$effective.N), digits = 0) + 2

cocor.dep.groups.nonoverlap(r.jk,
                            r.hm, 
                            r.jh, 
                            r.jm, 
                            r.kh, 
                            r.km, 
                            n, 
                            alternative = "two.sided", 
                            test = "all", 
                            var.labels = variables.nonoverlap) #Same as variables used in rmcorr_mat()

