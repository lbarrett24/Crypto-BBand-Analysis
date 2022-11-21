require(lubridate)
require(quantmod)
require(xts)
require(cowplot)
require(scales)
require(ggplot2)

#initializing data
df1 <- read.csv("LunarCrushData.csv", header=T)
View(df1)

#transforming data into xts object
crypto_dta = xts::xts(df1[,-1], order.by = as.Date(as.character(df1[,1]),  "%m-%d-%y"))
#visualizing data
chartSeries(crypto_dta, theme="black", TA="addVo();addBBands()")

bband.beta <- BBands(df1[,2,3], n = 20, sd = 2)
dat.beta = cbind(crypto_dta, bband.beta)
dat2.beta = dat.beta[-(1:19), ]

##New column: 1 if pctB is greater than 100%, 0 o.w.
dat2.beta$indx1 = ifelse(dat2.beta$pctB>=1, 1, 0)
##New column: 1 if pctB is less than 0%, 0 o.w.
dat2.beta$indx2 = ifelse(dat2.beta$pctB<=0, 1, 0)

##Stores extreme values in new xts/zoo object
pctB.pos = dat2.beta[which(dat2.beta$indx1==1),]
pctB.neg = dat2.beta[which(dat2.beta$indx2==1),]

#Bolinger Band function: 2 standard deviations
bolinger.extremes = function(currency){
  crypto_dta = xts::xts(df1[,-1], order.by = as.Date(as.character(df1[,1]),  "%m-%d-%y"))
  
  #conduct bband function and remove first 19 day MA values
  bband.beta <- BBands(currency[,2,3], n = 20, sd = 2)
  dat.beta = cbind(crypto_dta, bband.beta)
  dat2.beta = dat.beta[-(1:19), ]
  
  #New column: 1 if pctB is greater than 100%, 0 o.w.
  dat2.beta$indx1 = ifelse(dat2.beta$pctB>=1, 1, 0)
  #New column: 1 if pctB is less than 0%, 0 o.w.
  dat2.beta$indx2 = ifelse(dat2.beta$pctB<=0, 1, 0)
  
  #Stores extreme values in new xts/zoo object
  pctB.pos = dat2.beta[which(dat2.beta$indx1==1),]
  pctB.neg = dat2.beta[which(dat2.beta$indx2==1),]
  
  #find % volatility in price action
  percent.pos = round((pctB.pos[,9] * 100), digits = 2)
  percent.pos.diff = percent.pos - 100
  
  percent.neg = round((pctB.neg[,9] * 100), digits = 2)
  
  #add hours column
  hours.1 <- rep(10:24, len = 15)
  hours.2 <- rep(1:24, len = length(df1[,1]) - 15)
  hour <- c(hours.1, hours.2)
  names(hour) <- "crypto_dta.Hours"
  hour <- hour[-(0:19)]
  
  #add hours to bband/OHLVC data
  final.frame <- cbind(dat2.beta, hour)
  
  pos.hours <- subset(final.frame, indx1 == 1)
  pos.hours.dunski <- pos.hours$hour
  neg.hours <- subset(final.frame, indx2 == 1)
  neg.hours.donski <- neg.hours$hour
  
  #return total COIN traded and % volatility
  pos_band <- cbind(percent.pos.diff, pctB.pos[,5], pos.hours.dunski[,1])
  neg_band <- cbind(percent.neg, pctB.neg[,5], neg.hours.donski[,1])
  
  #merging negative and positive volatility lists
  output <- rbind(pos_band, neg_band)
  names(output) <- c("pctB", "Volume", "Hour")
  output
}
bband.data.2 <- bolinger.extremes(df1)
###############################################################################
#Bolinger Band function: 2.5 standard deviation
chartSeries(crypto_dta, theme="black", TA="addVo();addBBands(sd = 2.5)")
bolinger.extremes.2.5 = function(currency){
  crypto_dta = xts::xts(df1[,-1], order.by = as.Date(as.character(df1[,1]),  "%m-%d-%y"))
  
  #conduct bband function and remove first 19 day MA values
  bband.beta <- BBands(currency[,2,3], n = 20, sd = 2.5)
  dat.beta = cbind(crypto_dta, bband.beta)
  dat2.beta = dat.beta[-(1:19), ]
  
  #New column: 1 if pctB is greater than 100%, 0 o.w.
  dat2.beta$indx1 = ifelse(dat2.beta$pctB>=1, 1, 0)
  #New column: 1 if pctB is less than 0%, 0 o.w.
  dat2.beta$indx2 = ifelse(dat2.beta$pctB<=0, 1, 0)
  
  #Stores extreme values in new xts/zoo object
  pctB.pos = dat2.beta[which(dat2.beta$indx1==1),]
  pctB.neg = dat2.beta[which(dat2.beta$indx2==1),]
  
  #find % volatility in price action
  percent.pos = round((pctB.pos[,9] * 100), digits = 2)
  percent.pos.diff = percent.pos - 100
  
  percent.neg = round((pctB.neg[,9] * 100), digits = 2)
  
  #add hours column
  hours.1 <- rep(10:24, len = 15)
  hours.2 <- rep(1:24, len = length(df1[,1]) - 15)
  hour <- c(hours.1, hours.2)
  names(hour) <- "crypto_dta.Hours"
  hour <- hour[-(0:19)]
  
  #add hours to bband/OHLVC data
  final.frame <- cbind(dat2.beta, hour)
  
  pos.hours <- subset(final.frame, indx1 == 1)
  pos.hours.dunski <- pos.hours$hour
  neg.hours <- subset(final.frame, indx2 == 1)
  neg.hours.donski <- neg.hours$hour
  
  #return total COIN traded and % volatility
  pos_band <- cbind(percent.pos.diff, pctB.pos[,5], pos.hours.dunski[,1])
  neg_band <- cbind(percent.neg, pctB.neg[,5], neg.hours.donski[,1])
  
  #merging negative and positive volatility lists
  output <- rbind(pos_band, neg_band)
  names(output) <- c("pctB", "Volume", "Hour")
  output
}
bband.data.2.5 <- bolinger.extremes.2.5(df1)

###############################################################################
#Volatility visualizations
#2.5 standard deviations at 1.2% market volatility
#Scatter plot showing hours of high volatility across days/month
Volatility.by.hour.2.5 <- ggplot(bband.data.2.5, aes(x=Index, y = Hour)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  scale_x_date(name = "Date", date_minor_breaks = "1 day", date_breaks = "1 day", date_labels = "%d %b")+
  scale_y_continuous(name= "Hour", breaks = 0:24) + ggtitle("Price Action Volatility by Hour")

#line graph showing days when coin is at highest trading volume
Volume.volatility.2.5 <- ggplot(bband.data.2.5, aes(x=Index, y = Volume)) + 
  geom_point() + 
  geom_smooth() +
  scale_x_date(name = "Date", date_minor_breaks = "1 day", date_breaks = "1 day", date_labels = "%d %b") + 
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + ggtitle("Volume Traded during Volatile Periods")

#merging plots
plot_grid(Volatility.by.hour.2.5, Volume.volatility.2.5)

###############################################################################
#Volatility Visualizations
#2.5 standard deviations at 1.2% market volatility
#Scatter plot showing hours of high volatility across days/month
Volatility.by.hour.2 <- ggplot(bband.data.2, aes(x=Index, y = Hour)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  scale_x_date(name = "Date", date_minor_breaks = "1 day", date_breaks = "1 day", date_labels = "%d %b")+
  scale_y_continuous(name= "Hour", breaks = 0:24) + ggtitle("Price Action Volatility by Hour")

#line graph showing days when coin is at highest trading volume
Volume.volatility.2 <- ggplot(bband.data.2, aes(x=Index, y = Volume)) +
  geom_point() + 
  geom_smooth() +
  scale_x_date(name = "Date", date_minor_breaks = "1 day", date_breaks = "1 day", date_labels = "%d %b") + 
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  ggtitle("Volume Traded during Volatile Periods")

#merging plots
plot_grid(Volatility.by.hour.2, Volume.volatility.2)