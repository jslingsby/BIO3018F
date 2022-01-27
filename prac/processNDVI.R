###Get libraries and functions
library(tidyverse)
library(readr)

#source("prac/01_downloadNDVI.R")
source("prac/03_negexp_model.R")

###Get data
dat <- read_csv("prac/MODISdat.csv")

###Plot all timeseries
dat %>% #filter(calendar_date > as.Date("2015-01-01")) %>%
  ggplot(aes(x = calendar_date, y = value*scale)) + 
  geom_line() +
#  geom_point() +
  facet_wrap(.~ site) +
  ylab("NDVI")


### Fit the full model using MLE
# set parameters
par <- c(alpha = 0.2, gamma = 0.4, lambda = 0.5, A = 0.6, phi = 0)
# fit model
fit_negexpMLES <- fit.negexpS.MLE(dat, par)
# plot
plot.NDVI(dat)
# add curve with MLE parameters
lines(dat$age, pred.negexpS(fit_negexpMLES$par,dat$age), col = 'skyblue', lwd = 3)


##########################################
##' Fit negative exponential plus mystery term using maximum likelihood estimation
##' Function (a) to define the FULL model using MLE:
##' @param theta  parameter vector in order: alpha, gamma, lambda, A, phi
##' @param x      vector of x values
##' @return vector of model predictions

pred.negexpS <- function(theta, x){
  NDVI = theta[1] + theta[2] * (1 - exp(- x/theta[3])) +
    theta[4] * sin(2*pi*x + (theta[5] + pi/6*(3 - 1)))
}

##' Function (b) to fit the full model and minimize the -ln.likelihood
##'
##' @param dat  dataframe of NDVI, age
##' @param par  vector of initial parameter guesstimates (on order of theta)
##' @return  output from numerical optimization

fit.negexpS.MLE <- function(dat,par){
  
  ## define log likelihood
  lnL.negexpS <- function(theta,dat){
    -sum(dnorm(dat$NDVI, pred.negexpS(theta, dat$age), 0.001, log=TRUE), na.rm=TRUE) #Note that I added a standard deviation of 0.001 (in reality we should get that from the MODIS data)
  }
  
  ## fit by numerical optimization
  optim(par, fn = lnL.negexpS, dat=dat, control = list(maxit = 1000))
}