### Stat 222
### Presentation 1
### Exploratory Data Analysis
### Graphs

# library(lubridate)

### Plot the performance of the SP 500 index 
## price over time, and annual returns

## Get data - SP500 index, ticker symbol GSPC
## This guy lives in his own folder
gspc <- read.csv('../../stock_data/raw/sp500_indices/yahoo_GSPC.csv', stringsAsFactors = FALSE)


gspc <- gspc[,c('Date','Adj.Close')]
names(gspc) <- c('date', 'adjclose')
gspc$yr <- as.numeric(strftime(gspc$date, '%Y'))
gspc$mo <- as.numeric(strftime(gspc$date, '%m'))
gspc$d <- as.numeric(strftime(gspc$date, '%d'))

# aggregate avg price within each month 
gspc.mo <- aggregate(adjclose ~ yr + mo, gspc, FUN = mean)
gspc.mo <- gspc.mo[order(gspc.mo$yr, gspc.mo$mo),]
rownames(gspc.mo) <- NULL

# # check results
# gspc.mo[gspc.mo$yr %in% c(2000,2017) & gspc.mo$mo == 2,]
# mean(gspc[gspc$yr == 2017 & gspc$mo == 2, 'adjclose'])
# mean(gspc[gspc$yr == 2000 & gspc$mo == 2, 'adjclose'])

# calculate monthly returns (as a % of the previous month's avg)
gspc.mo$prev <- c(NA, gspc.mo$adjclose[1:(nrow(gspc.mo)-1)])
gspc.mo$r <- gspc.mo$adjclose/gspc.mo$prev - 1

# create fake dates (1st of each month) for plotting 
gspc.mo$dt <- as.POSIXct(paste(gspc.mo$yr, gspc.mo$mo, '01', sep = '-'))

# Plot avg monthly price: SP500 Index (GSPC)
plot1 <- ggplot( gspc.mo, aes(as.Date(dt)) ) +
  geom_line( aes(y=adjclose) ) + 
  labs( title = 'S&P 500 Index - Average Monthly Closing Price',
        x = '', y = '') +
        # ,x = 'Date', y = 'Adjusted Closing Price') + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y')
# ggsave(plot1, file='../presentation1/plots/SP500_price.png', 
#        width = 5, height = 3)

# Plot monthly returns (% gain/loss)
gspc.mo$pos <- gspc.mo$r > 0
plot2 <- ggplot( gspc.mo[-1, ], aes( x=as.Date(dt), y=r, fill=pos) ) + 
                   geom_bar(stat='identity') + 
  labs( title = 'S&P 500 Index - Monthly Returns',
        x = '', y = '') + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y')+
               # limits = c(as.Date("1950-01-01"), as.Date("2017-01-01")) ) + 
  scale_y_continuous(limits = c(-0.15, 0.15), labels = scales::percent) + 
  guides(fill=FALSE)
# ggsave(plot2, file='../presentation1/plots/SP500_returns.png', 
#        width = 5, height = 3)

### Summary Statistics 
# Annual Returns:
# year-over-year change in average yearly price
gspc.yr <- aggregate(adjclose ~ yr, gspc, FUN = mean)
gspc.yr <- gspc.yr[order(gspc.yr$yr),]
gspc.yr$prev <- c(NA, gspc.yr$adjclose[1:(nrow(gspc.yr)-1)])
gspc.yr$r <- gspc.yr$adjclose/gspc.yr$prev - 1
# mean(gspc.mo$r, na.rm = TRUE)*12
sprintf('SP 500 : Average Annual Return (1950-present): %s percent', round(100*mean(gspc.mo$r, na.rm = TRUE)*12, 4))
sprintf('SP 500 :Standard Deviation of Annual Return (1950-present): %s percent', round(var(gspc.yr$r*100,na.rm = TRUE)**0.5, 4))

rm(plot1,plot2)
save.image('gspc.Rdata')
