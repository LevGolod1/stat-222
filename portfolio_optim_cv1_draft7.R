## Stat 222 - Stock Portfolio Project
## Cross-Validation for Hyper-Parameters for Portfolio Optimization
## Tau - L1 penalty for portfolio weights
## Epsilon - regulates the tradeoff between 1. minimizing variance and 
#   2. satisfying the constraints: 
#   i. sum of portfolio weights = 1, ii. average monthly return equals the target

## NB: working with PERFECT INFORMATION 
##  (using the true returns each month instead of predictions)
## Will later run this on actual predictions

library(ggplot2)
library(dplyr)
library(tidyr)
options(max.print = 100, digits=4)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

load('2017.03.06_cv/scf_cv.Rdata')
load('2017.03.06_cv/scf_cv_table.Rdata')
load('../exploratory/gspc.Rdata')

## convergence
cvtable$op_conv <- factor(cvtable$op_conv,
                          levels=c(0,1),
                          labels=c('yes','no'))
plt_conv <- ggplot(cvtable, aes(tau, eps, colour = (op_conv))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Convergence of Optimization Algorithm')
plt_conv

# plt_conv2 <- ggplot(cvtable, aes(tau, eps)) +
#   geom_raster(aes(fill=as.numeric(op_conv))) + 
#   scale_x_log10() +
#   scale_y_log10() +
#   ggtitle('Convergence of Optimization Algorithm')
# plt_conv2 + scale_fill_continuous(name='Convergence', values=c(1,2))

plt_conv2 <- ggplot(cvtable, aes(tau, eps)) +
  geom_raster(aes(fill=op_conv)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Convergence of Optimization Algorithm') +
  theme_bw() + 
  # scale_fill_discrete(name='Convergence')
  scale_fill_brewer(palette = "Greens", name='Convergence')
plt_conv2

## Time Elapsed
summary(cvtable[, 'time_el'])
summary(cvtable[cvtable$op_conv=='yes', 'time_el'])
summary(cvtable[cvtable$op_conv=='no', 'time_el'])

## Some values of tau/epsilon didn't converge
paste0(rownames(cvtable[cvtable$op_conv=='no',]), collapse=', ')
cvtable[cvtable$op_conv=='no', c('op_fncounts', 'time_el')]


## sum of weights - should be very close to 1 
plt_wsum <-  ggplot(cvtable, aes(tau, eps)) +
  geom_raster(aes(fill=w_sum)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Sum of Portfolio Weights') +
  theme_bw() +
  # scale_fill_gradientn(name='',colours = terrain.colors(10))
  scale_fill_gradient(name='',low='mistyrose2',high='darkolivegreen3')
# plt_wsum + scale_fill_brewer(name='Sum of Portfolio Weights')
plt_wsum 


## L1 norm of weights - 
## should not be below 1
## close to 1 means very little shorting
## exactly 1 means no shorting
## greater than 1 means substantial shorting
plt_l1norm <-  ggplot(cvtable, aes(tau, eps)) +
  geom_raster(aes(fill=w_l1)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('L1 Norm of Portfolio Weights') +
  # scale_fill_gradient(name='',low='mistyrose2',high='darkolivegreen3')
  scale_fill_gradientn(name='',colours = terrain.colors(100))
# plt_wsum + scale_fill_brewer(name='Sum of Portfolio Weights')
plt_l1norm 


## Sparsity -- % of coefficients set to 0.
#  that is, % of available stocks where we bought 0 shares 
## Not seeing as much sparsity as hoped for
plt_spars <-  ggplot(cvtable, aes(tau, eps)) +
  geom_raster(aes(fill=sparsity)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Sparsity') +
  # scale_fill_gradientn(name='',colours = terrain.colors(100))
  scale_fill_gradient(name='',low='mistyrose2',high='darkolivegreen3')
# plt_wsum + scale_fill_brewer(name='Sum of Portfolio Weights')
plt_spars 



## calculate variance & returns of FULL PORTFOLIO 
## This means takes existing weights and scale so they sum to 1
# - since the way I did it first was so silly
# variance, rel to Naive Portfolio
# returns in excess of Naive
R = Rlist$`1990`
N <- ncol(R)
mu <- apply(R, 2, mean)
w0 = rep(1/N,N)
cov <- t(R) %*% R - mu %*% t(mu)
naivevar <- (t(w0) %*% cov %*% w0)  
naive_return <- mean(mu)
# variance of monthly returns
naive_mo_var <- var(apply(R, 1, function(x) w0 %*% x))

varlist <- rep(NA, length(o.list))
returnlist <- rep(NA, length(o.list))
l1 <- rep(NA, length(o.list))
var_mo_returns <- rep(NA, length(o.list))
for (i in 1:length(o.list)){
  w <- o.list[[i]]$optim$w
  w <- w/sum(w)
  myvar <- (t(w) %*% cov %*% w)
  myreturn <- w%*%mu
  varlist[i] <- myvar
  returnlist[i] <- myreturn
  l1[i] <- sum(abs(w))
  var_mo_returns[i] <- var(apply(R, 1, function(x) w %*% x))/naive_mo_var
}
cvtable$rel_var <- varlist/naivevar
cvtable$rel_var_mo <- var_mo_returns
cvtable$excess_return <- returnlist - naive_return
cvtable$w_l1 <- l1

plt_rel_var <-  ggplot(cvtable, aes(tau, eps)) +
  geom_raster(aes(fill=rel_var_mo)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Variance (Relative to Variance of Naive Portfolio') +
  # scale_fill_gradientn(name='',colours = terrain.colors(100))
  scale_fill_gradient(name='',low='mistyrose2',high='darkolivegreen3')
# plt_wsum + scale_fill_brewer(name='Sum of Portfolio Weights')
plt_rel_var 


## Excess returns - beyond those of 'naive portfolio' - should be close to 0
plt_returns <-  ggplot(cvtable, aes(tau, eps)) +
  geom_raster(aes(fill=excess_return)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Excess Returns') +
  # scale_fill_gradientn(name='',colours = terrain.colors(100))
  scale_fill_gradient(name='',low='mistyrose2',high='darkolivegreen3')
# plt_wsum + scale_fill_brewer(name='Sum of Portfolio Weights')
plt_returns 


## Let's zoom in on the values of Tau and Eps that seem to have 
#  given reasonable results 
cv_good <- cvtable

## convergence
# cv_good <- filter(cv_good, op_conv =='yes')
## sum of weights close to 1
# cv_good <- filter(cv_good, abs(w_sum-1) < 0.005)
## L1 norm < 2
cv_good <- filter(cv_good, w_l1 < 2)
## Variance < 2 
cv_good <- filter(cv_good, rel_var_mo <= 1)
dim(cv_good)
cv_good <- arrange(cv_good, rel_var_mo)
cv_good[1:10,
        c('tau','eps','sparsity','excess_return', 'rel_var_mo','w_l1')]

# C


### PLOTS
## L1 norm
plt_l1norm <-  ggplot(cv_good, aes(tau, eps)) +
  geom_raster(aes(fill=w_l1)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('L1 Norm of Portfolio Weights') +
  theme_bw()+
  scale_fill_gradient(name='',low='plum',high='forestgreen')
plt_l1norm 

## Sparsity
plt_spars <-  ggplot(cv_good, aes(tau, eps)) +
  geom_raster(aes(fill=sparsity)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Sparsity (% of Weights Set to Zero)') +
  theme_bw() +
  scale_fill_gradient(name='',low='plum',high='forestgreen')
plt_spars 

# Variance (Rel to Naive)
plt_rel_var <-  ggplot(cv_good, aes(tau, eps)) +
  geom_raster(aes(fill=rel_var_mo)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Variance (Relative to Naive Portfolio)') +
  theme_bw() +
  scale_fill_gradient(name='',low='plum',high='forestgreen')
# plt_wsum + scale_fill_brewer(name='Sum of Portfolio Weights')
plt_rel_var 


## Excess returns - beyond those of 'naive portfolio' - should be close to 0. Bc of the approximate nature of the optimization, 
# is sometimes non-zero
plt_returns <-  ggplot(cv_good, aes(tau, eps)) +
  geom_raster(aes(fill=excess_return)) + 
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Excess Returns (Compared to Naive Portfolio)') +
  theme_bw()+
  scale_fill_gradient(name='',low='plum',high='forestgreen')
# plt_wsum + scale_fill_brewer(name='Sum of Portfolio Weights')
plt_returns 

multiplot(plt_l1norm, plt_spars, plt_rel_var, plt_returns, cols=2)


## Single best portfolio - in terms of decr. variance 
cv_good[1, ]
cv_good[1, c('tau','eps')]
best <- as.numeric(rownames(
  cvtable[cvtable$tau==cv_good[1, 'tau'] & 
          cvtable$eps==cv_good[1, 'eps'],]))
bestw <- o.list[[best]]$optim$w        
bestw <- bestw/sum(bestw)
names(bestw) <- names(mu)
sort(abs(bestw),decreasing=T)[1:10]
cat(names(sort(abs(bestw),decreasing=T))[1:10])
bestw[names(sort(abs(bestw),decreasing=T))[1:10]]
cumsum(sort(abs(bestw),decreasing=T))[1:10]

# returns - overall for 1990 
bestw%*% mu
# returns - monthly
my_returns <- apply(R, 1, function(x) bestw %*% x)
mean(my_returns)
var(my_returns)
my_port <- cumprod(c(1, my_returns+1))

# compare to SP 500 in the same period
naive_returns <- apply(R, 1, function(x) w0 %*% x)
mean(naive_returns)
var(naive_returns)
naive_port <- cumprod(c(1, naive_returns+1))

# compare to naive portfolio in same period 
sp500_returns <- gspc.mo[gspc.mo$yr==1990,'r']
mean(sp500_returns)
var(sp500_returns)
sp500_port <- cumprod(c(1, sp500_returns+1))

## plot the monthly returns of our portfolio, compared to
#  1. naive portfolio - equal weights to all stocks
#  2. SP500 index
dat <- data.frame(my_port, naive_port, sp500_port)
dat <- gather(dat, type, value, c(my_port, naive_port, sp500_port))
dates <- as.POSIXct(paste(1990, 1:12, '01', sep = '-'))
dates <- c(as.POSIXct(paste(1989, 12, '01', sep = '-')),
           dates)
dat$date <- dates

plt_result_compar <- ggplot(dat, aes(date, value, 
                                     colour=factor(type),
                                     shape=factor(type))) +
  geom_line()+
  geom_point()+
  scale_colour_discrete(name="Portfolio", 
                      labels=c('Optimized','Naive','SP500 Index'))+
  scale_shape_discrete(guide='none')+
  ggtitle("Portfolio Performance Comparison: 1990")

              

