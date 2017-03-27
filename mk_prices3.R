## Stat 222 - Stock Portfolio Project
## Data Processing
## Reading in csv files for stock prices and saving out .Rdata files

setwd("~/GoogleDrive/Berkeley/02 Spring Semester/STAT 222 - Capstone/project_share/stock_data/mkdata/sp500")

# load packages 
# https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
library(lubridate)

# identify directories 
readdir <- "../../raw/yahoo_complete/out/"
savedir <- "../../data/sp500/price/"

# loop through all names of price data files 
allfiles <- list.files(readdir, "price.csv")

# list to store all prices 
pricelist <- as.list(rep(NA,length(allfiles)))
tickers <- gsub("\\..+", "", allfiles)
names(pricelist) <- paste0(tickers, "_price")

i = 1
for (file in allfiles){
  
  # identify the ticker symbol 
  ticker <- gsub("\\..+", "", file)
  writeLines(c("\nReading data for ", ticker))
  
  # read the csv for one historical stock price 
  path <- paste0(readdir, file)
  dat <- read.csv(path, stringsAsFactors = FALSE)
  dat$ticker <- ticker
  
  # Only keeping date, adj close, volume
  dat <- dat[,c(8,1,7,6)]
  
  # Check to make sure we have the right columns
  if (!all.equal(names(dat), c("ticker", "Date", "Adj.Close", "Volume"))){
    cat(file)
    stop("Wrong columns buddy")
  }
  
  # NB I am renaming adj close to 'price'
  # NB I am renaming adj volume to 'vol'
  names(dat) <- c('ticker', 'date', 'price', 'vol')
  
  # changing date from character string to a lubridate Date object
  dat$date <- ymd(dat$date)
  
  # Save the file 
  savepath <- paste0(savedir, 'indiv/', ticker, "_price.Rdata")
  savefile <- paste0(ticker, '_price')
  assign(savefile, dat)
  save(list=savefile, file=savepath)
  
  # Add the file to the pricelist list
  # eval(parse( text=paste0('pricelist[[',i,']] <- ',savefile) ))
  eval(parse( text=paste0('pricelist[[',i,']] <- `',savefile,'`') ))
  
  # Clear files from R memory
  rm(dat)
  eval(parse(text=paste0("rm(`",savefile,"`)")))

  i = i + 1
  
}

# save the pricelist 
save(pricelist, file=paste0(savedir, "pricelist.Rdata"))
