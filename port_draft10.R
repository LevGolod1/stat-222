## Stat 222 - Stock Portfolio Project
## Portfolio Construction

## http://www.pnas.org/content/suppl/2009/07/15/0904287106.DCSupplemental/Appendix_PDF.pdf

## NOTES
## 1. Using regular old R Optim - 
##    Approximate method Eqn (4) in the link
## 2. For now assuming that we have perfectly predicted returns 
## 3. Currently restricting myself to the time period Jan 1980- Dec 2010

## do CV for 1. different methods and 2. different values of Tau and Epsilon

### CHANGES 
## 1. allowing different target return levels, p (rho)


## ## SETUP
# rm(list = ls())
setwd("~/GoogleDrive/Berkeley/02 Spring Semester/STAT 222 - Capstone/project_share/analysis/portfolio_const")
options(max.print = 100)
library(dplyr)
library(lubridate)
# library(foreach)
# library(doParallel)
# ncores <- detectCores() - 1


## ## Auxiliary functions 
## L1 norm
l1norm <- function(v) sum(abs(v))
## L2 norm 
l2norm <- function(v) (t(v) %*% v)**0.5


## ## GET DATA 
load("../../stock_data/data/sp500/returns/allreturns.Rdata")
returns <- (allreturns)
rm(allreturns)
ym <- gsub('-', '', substr(returns[,1], 1,7))
rownames(returns) <- ym

## Process data:
## Remove months with all NAs (Jan 1980, March 2017+). 
## Interpolate missing value for 'FRT' Oct 1987
nas <- apply(returns[,-1], 1, function(x) mean(is.na(x)))
returns <- returns[-c(which(nas==1)),]
returns[returns$date == ymd('19871001'), 'FRT'] <- 
  mean(returns[,'FRT'], na.rm = TRUE)

## partition data into different years
years <- seq.int(1980,2009)
Rlist <- vector("list", length(years))
names(Rlist) <- years
i <- 1
for (year in years){
  
  start = ymd(paste0(year, '0101')) 
  end = start + years(0) + months(11) 
  R <- returns %>% 
    filter(date %within% interval(start, end)) 
  # Make date the row name, and remove the date column
  ym <- substring(R[,'date'], 3,7) 
  rownames(R) <- ym 
  R <- R[,-1]
  # don't include a stock if there are any NAs for that stock in the year
  nas <- apply(R, 2, function(x) sum(is.na(x)))
  R <- R[,nas==0]
  R <- as.matrix(R)
  Rlist[[i]] <- R
  i <- i + 1
  
}

rm(end, i, nas, start, year, years, ym, R)



## ## OPTIMIZATION FUNCTIONS

## min1 - calculates the value of the objective (loss) function for
##   a given weight vector w, predicted return matrix R, desired level
##   of returns p ('rho'), and penalty coefficients tau (L1) and eps (constr)
# w = rep(1/260,260); p = -0.0003627118; eps = 1e-3; tau = 1;  R=Rlist$`1990`
min1 <- function(w, p, eps, tau, R){
  
  ## calculate parameters 
  T <- nrow(R)
  N <- ncol(R)
  t1 <- rep(1,T)
  n1 <- rep(1,N)
  mu <- apply(R, 2, mean)
  y = t1 * p
  
  ## Identify constraints - A %*% w - a == 0 
  # <w . mu-hat> = rho
  # <w . n1 > = 1
  # A: matrix of constraints. a: vector of what the constraints equal
  A = rbind(mu, n1)
  a = c(p, 1)
  
  ## constraints term
  con <- l2norm(A %*% w - a)**2
  
  ## portfolio variability term 
  var <- eps * l2norm(R %*% w - y)**2
  
  ## penalty term 
  pen <- tau * l1norm(w)
  
  return(con+var+pen)
  
}
# w1 = rep(1/260,260)
# min1(w = op$par, p=-0.0003627118, tau=1, eps = 1e-4, R=Rlist$`1990`)


## o.min1 - wraps min1 in an optim-friendly wrapper
## wrap the function in optim-friendly format
# w1 is the weight vector. data is a list of other quantities 
o.min1 <- function(w, data){
  with(data, min1(w, data$p, data$eps, data$tau, data$R))
}
# min1(w = w1, p=0.01, tau=10, eps = 1e-4, R=Rlist$`1990`)
# o.min1(w = w1, data = list(p=0.01, tau=10,  eps=1e-4, R=Rlist$`1990`))
# rm(w1,w2,w3)



#### portfn1 - takes in a matrix of predicted returns and hyperparameters,
## conducts optimization, and spits out the weight vector, as well as a 
## performance summary: returns and variance compared to what the 'naive'
## strategy (equal weight to all assets)achieved
### Args:
## R - matrix of returns 
## tau - coefficient for size of L1 penalty
## eps - epsilon - coeff for size of constraing penalty
## iter - number of iterations (inner)
## method - optimization method


# R = Rlist$`1990`; tau=1; eps=1e-4; iter=1e+6; method='CG'; lower=NULL; upper=NULL
portfn1 <- function(tau, eps, R, iter=1e+6, method='CG', 
                    lower=NULL, upper=NULL){
  
  N <- ncol(R)
  n1 <- rep(1,N)
  mu <- apply(R, 2, mean)
  p <- mean(mu)
  
  ## naive strategy - equal weights for all assets
  w0 = rep(1/N,N)

  ## choose a good starting pt - single asset w highest returns
  w1 <- rep(0, N)
  w1[which(mu == max(mu))] <- 1
  
  time <- system.time(
    op <- optim(w1, o.min1,  data = list(p=p,eps=eps,tau=tau,R=R), 
                method=method, 
                control=list(maxit=iter, trace=FALSE))
  )
  w = op$par 
    
  # sample mean and cov
  mu <- apply(R, 2, mean)
  cov <- t(R) %*% R - mu %*% t(mu)
  # calculate sample returns and variance for testing data
  # for the optimized w as well as the 'naive' w
  myreturn <- w%*%mu
  naivereturn <- w0%*%mu
  myvar <- (t(w) %*% cov %*% w)
  naivevar <- (t(w0) %*% cov %*% w0)
  
  ## Create summary of results 
  summary = c(
    w_sum = sum(w), # sum of portfolio weights (pre-adjustment)
    
    w_l1 = l1norm(w), # l1 norm of weights (after adjustment )
    
    100*mean(w < 1e-10), # (approx) sparsity percentage
    
    # excess returns (in excess of naive) - hope > 0
    #  units: 1% greater monthly returns means excess_returns = 0.01
    excess_return = (myreturn - naivereturn),
    
    # % decrease in variance (cf naive) - hope > 0
    #  units: percent change relative to variance of naive strategy
    decrease_var = 100*(1 -  myvar/naivevar),
    
    op_conv = as.integer(op$convergence), # convergence code,
    
    op_fncounts = as.integer(op$counts[1]), # number of  iterations
    
    op_val = op$value, # value of objective fn, under optimal solution
    
    time = time[1:3]) # computation time for optimization
  
  names(summary) <- c('w_sum', 'w_l1', 'sparsity', 'excess_return', 'decrease_var', 
                      'op_conv', 'op_fncounts', 'op_val', 'time_usr', 
                      'time_sys', 'time_el')
  
  ## Return summary + optimal weight vector w 
  return(list(summary = summary, w = w))

}


### TRY OUT THE FUNCTION
R1 = Rlist$`1990`
tau1 = 1
eps1 = 1e-4
iter1 = 1e+6
m1 = 'CG'
# o1 <- portfn1(R = Rlist$`1990`, tau=1, eps=1e-4, iter=1e+6, method='CG')
o1 <- portfn1(R = R1, tau=tau1, eps=eps1, iter=iter1, method=m1)
# print(o1$summary)

### CONFIRM FUNCTION OUTPUT - Returns, Variance, Value of obj fn
w1 = o1$w 
n1 <- ncol(R1)
w0 = rep(1/n1,n1)
# sum and norm 
c(sum(w1), l1norm(w1)) 
mu1 <- apply(R1, 2, mean)
p1 <- mean(mu1)
cov1 <- t(R1) %*% R1 - mu1 %*% t(mu1)
# my returns and variance
a <- c(return = t(w1) %*% mu1, var = (t(w1) %*% cov1 %*% w1))
b <- c(return = t(w0) %*% mu1, var = (t(w0) %*% cov1 %*% w0))
a
# excess return/ decrease var vs naive 
c(a[1]-b[1], 100*(1-a[2]/b[2]))
# value of objective fn
min1(w1,p1,eps1,tau1, R1)
rm(R1,tau1,eps1,iter1,m1,p1,a,b,n1,mu1,cov1)


#### CROSS-VALIDATION - different combinations of eps and Tau

tauvals <- 10^(-10:3)
epsvals <- 10^(-10:3)
cvtable <- expand.grid(tau=tauvals,eps=epsvals)
for (name in names(o1$summary)){
  command <- paste0('cvtable$', name,' <- NA')
  eval(parse(text=command))
}

R1 = Rlist$`1990`
iter1 = 1e+3
m1 = 'CG'

o.list <- vector('list', nrow(cvtable))
for (i in 8:nrow(cvtable)){
# for (i in 1:3){
  
  tau1 = as.numeric(cvtable[i,'tau'])
  eps1 = as.numeric(cvtable[i,'eps'])
  cat('Starting job ', i, ' ### tau= ', tau1, ', eps =', eps1, '\n', sep = '')
  
  output <- portfn1(tau=tau1, eps=eps1, R=R1, iter=iter1, method=m1)
  cvtable[i,3:ncol(cvtable)] <- output$summary
  o.list[[i]] <- list(params = c(tau = tau1, eps = eps1), 
                      optim = output)
  print(output$summary)
  cat('Finishing job ', i,  '### \n', sep = '')
  
}

save(cvtable, file='./cv/cvtable2.Rdata')

