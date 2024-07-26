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
library(dplyr)
library(patchwork)
library(esc)
library(psych)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("dplyr")
#  require(dplyr)
#  
#  install.packages("esc")
#  require(esc)
#  
#  install.packages("patchwork")
#  require(patchwork)

## ----echo = FALSE-------------------------------------------------------------
N.Marusich2016 <- length(unique(marusich2016_exp2$Pair))
k.Marusich2016 <- length(unique(marusich2016_exp2$SourceReliablity))
total.obs.Marusich2016 <- length(marusich2016_exp2$Pair)

## -----------------------------------------------------------------------------
overfit.plot <- 
    ggplot(data = marusich2016_exp2, aes(x = MARS, y = HVT_capture)) +
    geom_point(aes(colour = factor(Pair))) +
    geom_smooth(method= "lm", level = 0.95) +
    coord_cartesian(xlim = c(2,4), ylim=c(0,30)) + 
    ylab("Capture Time (seconds)") + 
    theme_minimal() +
    theme(legend.position="none") 

marusich2016_avg <- marusich2016_exp2 %>%
                    group_by(Pair) %>%
                    summarize(Mean_MARS = mean(MARS),
                              Mean_HVT_capture = mean(HVT_capture))

average.plot <- 
    ggplot(data = marusich2016_avg, 
           aes(x = Mean_MARS, y = Mean_HVT_capture)) +
    geom_smooth(fullrange = TRUE, method= "lm", level = 0.95) +
    coord_cartesian(xlim = c(2,4), ylim=c(0,30)) +
    geom_point(aes(colour = factor(Pair))) +
    xlab("Mean MARS") +
    ylab("Mean Capture Time (seconds)") +
    scale_colour_discrete(name = "Dyad") +
    theme_minimal()

overfit.cor <- cor.test(marusich2016_exp2$MARS, marusich2016_exp2$HVT_capture)

average.cor <- cor.test(marusich2016_avg$Mean_MARS, marusich2016_avg$Mean_HVT_capture)

df.s <- rbind(overfit.cor$parameter, average.cor$parameter)

r.s  <- rbind(round(rbind(overfit.cor$estimate, average.cor$estimate), digits = 2)) 

CI.s <- formatC(rbind(overfit.cor$conf.int, 
          average.cor$conf.int), digits = 2,
          format = 'f')

p.vals <- rbind(round(overfit.cor$p.value, digits = 3), 
                prettyNum(average.cor$p.value, digits = 2, 
                          drop0trailing = TRUE))

overfit.plot + average.plot 

## -----------------------------------------------------------------------------
marusich.rmc <- rmcorr(participant = Pair, 
                       measure1 = MARS, 
                       measure2 = HVT_capture, 
                       data = marusich2016_exp2)
rmc.plot <- 
    ggplot(data = marusich2016_exp2, 
           aes(x = MARS, y = HVT_capture, group = factor(Pair), color = factor(Pair))) +
    geom_point(aes(colour = factor(Pair))) +
    geom_line(aes(y = marusich.rmc$model$fitted.values)) +
    theme_minimal() +
    scale_colour_discrete(name = "Dyad") +
    ylab("Capture Time (seconds)")

rmc.r <- round(marusich.rmc$r, 2)
rmc.CI <- round(marusich.rmc$CI, 2)
rmc.p <- formatC(marusich.rmc$p, format = "fg", digits = 2)
                # prettyNum(average.cor$p.value, digits = 2,
                          # drop0trailing = TRUE))

rmc.plot

## -----------------------------------------------------------------------------
N.vals <- seq(5, 100, by = 1)
sd.Z <- 1/sqrt(N.vals-3)

sd.overfit <- data.frame(cbind(N.vals, sd.Z))

sd.cor <- sd.Z[N.Marusich2016-4] #Have to subtract 4 because N.vals starts at 5, not 1
sd.overf <- sd.Z[total.obs.Marusich2016-4]

#We could determine the inflated sample size for the overfit model using its degrees of freedom + 2
df.s + 2  == total.obs.Marusich2016

sd.overfit.plot <- 
    ggplot(data = sd.overfit, 
           aes(x = N.vals, y = sd.Z)) +
    coord_cartesian(xlim = c(0,100), ylim=c(0, 0.75)) +
    geom_point(alpha = 0.25) +
    geom_segment(x = 28, y =  sd.cor, xend = 84, 
                 yend =  sd.cor, linetype = 2) +
    geom_segment(x = 84, y =  sd.cor, xend = 84, 
                 yend =  sd.overf, colour = "red",
                 linetype = 2) +
    xlab("Analysis Sample Size") + 
    ylab(expression(paste("Fisher ", italic("Z"), " standard deviation"))) +
    annotate(geom = "point", x = 28, y = sd.cor, 
             colour = "black", size = 2) +
    annotate(geom = "point", x = 84, y = sd.overf, 
             colour = "red", size = 2) +
    theme_minimal()

sd.overfit.plot

## -----------------------------------------------------------------------------
#Calculate the expected correlation using the reported p-value and actual sample size 
#esc_t assumes p-value is two-tailed and uses a Fisher Z transformation
calc.z <- esc_t(p = as.numeric(p.vals[1]), 
                      totaln = N.Marusich2016, 
                      es.type = "r")$es


reported.r = as.numeric(r.s[1])

#Overfit?
fisherz2r(calc.z) != abs(reported.r)  
#Note calc.z will always be positive using esc_t() this way
#If the reported.r is not a Fisher Z value - either it should be transformed or the calc.z should be transformed

fisherz2r(calc.z) 
reported.r

