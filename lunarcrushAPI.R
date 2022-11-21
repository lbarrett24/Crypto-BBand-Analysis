rm(list=ls())

require("pbapply")
require("data.table")
require("httr")
require("rvest")
require("dplyr")
require("lubridate")
require("jsonlite")
require("coinmarketcapr")
require("quantmod")
require("TTR")
require(devtools)
require(rgdax)
require(geckor)
require(ggplot2)

##pull data from Lunar Crush API
url.1 <- paste0("https://api.lunarcrush.com/v2?data=assets&key=sfz2kan1h2b7w4ve04bk9j&symbol=ETH&interval=hour&data_points=720")

exp <- GET(url.1)

dta <- jsonlite::fromJSON(url.1, simplifyVector = TRUE)

dta <- dta %>% as.data.frame()

##filter extra information
##Retrieve OHLVC data with UNIX time only
ohlvc <- cbind(dta$data.timeSeries[[1]][["time"]], dta$data.timeSeries[[1]][["open"]],
               dta$data.timeSeries[[1]][["high"]], dta$data.timeSeries[[1]][["low"]],
               dta$data.timeSeries[[1]][["close"]], dta$data.timeSeries[[1]][["volume"]])

colnames(ohlvc)<- c("Time","ETH.Open","ETH.High","ETH.Low","ETH.Close","ETH.Volume")

##change UNIX time to datetime
ETH.Time <- as.POSIXct(ohlvc[,1], origin = "1970-01-01")
##Add new date to data frame and remove UNIX time
ohlvc.data <- data.frame(ETH.Time, ohlvc)
ohlvc.data <- ohlvc.data[,-2]
View(ohlvc.data)

write.csv(ohlvc.data, "Desktop\\Winn.Solutions\\DS.Projects\\lunarcrushdata.csv", row.names = FALSE)

############################################################################################################ 
url.2 <- paste0("https://api.lunarcrush.com/v2?data=assets&key=sfz2kan1h2b7w4ve04bk9j&symbol=CEEK&interval=day&data_points=720")

exp.2 <- GET(url.2)

dta.2 <- jsonlite::fromJSON(url.2,simplifyVector = TRUE)

dta.2 <- dta.2 %>% as.data.frame()

ohlvc.CEEK <- cbind(dta.2$data.timeSeries[[1]][["time"]], dta.2$data.timeSeries[[1]][["open"]],
               dta.2$data.timeSeries[[1]][["high"]], dta.2$data.timeSeries[[1]][["low"]],
               dta.2$data.timeSeries[[1]][["close"]], dta.2$data.timeSeries[[1]][["volume"]])

colnames(ohlvc.CEEK)<- c("Time","CEEK.Open","CEEK.High","CEEK.Low","CEEK.Close","CEEK.Volume")

##change UNIX time to datetime
CEEK.Time <- as.POSIXct(ohlvc.CEEK[,1], origin = "1970-01-01")
##Add new date to data frama and remove UNIX time
ohlvc.data.CEEK <- data.frame(CEEK.Time, ohlvc.CEEK)
ohlvc.data.CEEK <- ohlvc.data.CEEK[,-2]
head(ohlvc.data.CEEK)
View(ohlvc.data.CEEK)
write.csv(ohlvc.data.CEEK, "Desktop\\Winn.Solutions\\DS.Projects\\lunarcrushCEEK_mid_April.csv", row.names = FALSE)

############################################################################################################ 



