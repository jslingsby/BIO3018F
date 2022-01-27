##' Functions to fit negative exponential model using MLE
##' 
##' 1) Fit SIMPLE negative exponential model using maximum likelihood estimation
##' 
##' Function (a) to define the model for the SIMPLE negative exponential model using MLE:
##' @param theta  parameter vector in order: alpha, gamma, lambda
##' @param x      vector of x values
##' @return vector of model predictions

pred.negexp <- function(theta, x){
  NDVI = theta[1] + theta[2] * (1 - exp(- x/theta[3]))
}


##' Function (b) to fit the SIMPLE negative exponential model and minimize the -ln.likelihood
##' @param dat  dataframe of NDVI, age
##' @param par  vector of initial parameter guesstimates (on order of theta)
##' @return  output from numerical optimization

fit.negexp.MLE <- function(dat,par){
  
  ## define log likelihood
  lnL.negexp <- function(theta,dat){
    -sum(dnorm(dat$NDVI, pred.negexp(theta, dat$age), 0.001, log=TRUE), na.rm=TRUE) #Note that I added a standard deviation of 0.001 (in reality we should get that from the MODIS data)
  }
  
  ## fit by numerical optimization
  optim(par, fn = lnL.negexp, dat=dat, control = list(maxit = 1000))
}

##########################################

##' 2) Fit negative exponential plus mystery term using maximun likelihood estimation
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