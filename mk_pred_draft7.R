## Stat 222 - Stock Portfolio Project
## Data Processing
## Reading in csv files for stock prices and saving out .Rdata files
# rm(list = ls())

setwd("~/GoogleDrive/Berkeley/02 Spring Semester/STAT 222 - Capstone/project_share/stock_data/mkdata/sp500")

## load packages, set options
library(lubridate)
library(dplyr)
options(dplyr.print_max = Inf)
options(max.print = 100)

## identify directories 
## get data - list of stock prices
readdir <- "../../data/sp500/price/"
savedir <- "../../data/sp500/returns/"
load(paste0(readdir, 'pricelist.Rdata'))


## list to store monthly returns (& total volume) for each stock 
returnslist <- vector('list',length(pricelist))
names(returnslist) <- paste0(gsub("_price", "", names(pricelist)),'_returns')

## data frame for all returns 
## each row is a month, each col is the return of a different stock
start <- ymd('1980-01-01')
end <- ymd('2017-06-01')
nmonths <- interval(start,end)/months(1)
allmonths <- start + months(seq.int(0,nmonths-1))
allreturns <- data.frame(date=allmonths)

for (i in 1:length(pricelist)){

  writeLines(paste("####", i, names(pricelist)[i]))
  ## get data for ith company
  dat <- arrange(pricelist[[i]],date)
  ## monthly date
  dat$ym <- ymd(paste(year(dat$date),month(dat$date),'1',sep = "-")) 
  
  ## collapse by month: TOTAL volume and MEAN price
  returns <- dat %>%
    group_by(ticker, ym) %>%
    summarize(price=mean(price),
              vol=sum(as.numeric(vol)))
  rm(dat)

  ## calculate monthly PERCENT returns drop for the first month of data because 
  ## are were no returns that month
  current <- returns$price[-1]
  prev <- returns$price[-length(returns$price)]
  returns <- returns[-1,]
  returns$returns <- (current-prev)/prev
  returns <- as.data.frame(returns)
  
  timerange <- interval(min(returns$ym),max(returns$ym))
  ticker <- as.character(returns[1,'ticker'])
  allreturns[,ticker]=NA
  
  ind <- allreturns$date %in% returns$ym
  allreturns[ind, ticker] <- returns$returns
  # timerange <- interval(min(returns$ym),max(returns$ym))
  # allreturns[allreturns$date %within% timerange, ticker] <- returns$returns
               # ticker] <- returns$returns
  
  assign(ticker, returns)
  rm(returns)
  eval(parse(text=paste0('returnslist[[i]] <- `',ticker,'`')))
  eval(parse(text=paste0("rm(`",ticker,"`)")))
  
}

# save out results
save(returnslist, file=paste0(savedir, 'returnslist.Rdata'))
save(allreturns, file=paste0(savedir, 'allreturns.Rdata'))

