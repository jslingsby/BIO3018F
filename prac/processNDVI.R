###Get libraries and functions
library(tidyverse)
library(readr)

#source("prac/01_downloadNDVI.R")
source("prac/02_plot.NDVI.R")
source("prac/03_negexp_model.R")

###Get data
adat <- read_csv("prac/MODISdat_batch_28Jan.csv")
#adat <- adat %>% filter(calendar_date > as.Date("2015-9-14"))

###Plot all timeseries
adat %>%
  ggplot(aes(x = calendar_date, y = value*scale)) + 
  geom_line() +
#  geom_point() +
  facet_wrap(.~ site) +
  ylab("NDVI")


###Run through sites fitting the model

# Get site names
sitnms <- unique(adat$site)

# Set initial parameters
par <- c(alpha = 0.2, gamma = 0.4, lambda = 0.5, A = 0.6, phi = 0)

# Make output table
out <- data.frame(initial = par)

par(mfrow=c(2,3))

for(i in 1:length(sitnms)) {
  
dat <- adat %>% filter(site == sitnms[i])

# calculate age from date
dat$age <- (as.numeric(dat$calendar_date) - min(as.numeric(dat$calendar_date), na.rm = T))/365.25

# scale NDVI
dat$NDVI <- dat$value*dat$scale

# fit models
fit_negexpMLES <- fit.negexpS.MLE(dat, par)

# plot
plot.NDVI(dat, ylim = c(0.1, 0.9), main = sitnms[i])
# add curve with MLE parameters
lines(dat$age, pred.negexpS(fit_negexpMLES$par,dat$age), col = 'skyblue', lwd = 3)
# lines(dat$age, pred.S(fit_SMLE$par,dat$age), col = 'skyblue', lwd = 3)


# bind to output
out <- cbind(out, fit_negexpMLES$par)
}

names(out) <- c("initial", sitnms)
t(out)

##########################################
##' Fit intercepts and sinusoidal term, but no negative exponential plus mystery term using maximum likelihood estimation
##' Function (a) to define the FULL model using MLE:
##' @param theta  parameter vector in order: alpha, gamma, A, phi
##' @param x      vector of x values
##' @return vector of model predictions

pred.S <- function(theta, x){
  NDVI = theta[1] + theta[2] +
    theta[4] * sin(2*pi*x + (theta[5] + pi/6*(3 - 1)))
}

##' Function (b) to fit the full model and minimize the -ln.likelihood
##'
##' @param dat  dataframe of NDVI, age
##' @param par  vector of initial parameter guesstimates (on order of theta)
##' @return  output from numerical optimization

fit.S.MLE <- function(dat,par){
  
  ## define log likelihood
  lnL.S <- function(theta,dat){
    -sum(dnorm(dat$NDVI, pred.S(theta, dat$age), 0.001, log=TRUE), na.rm=TRUE) #Note that I added a standard deviation of 0.001 (in reality we should get that from the MODIS data)
  }
  
  ## fit by numerical optimization
  optim(par, fn = lnL.S, dat=dat, control = list(maxit = 1000))
}