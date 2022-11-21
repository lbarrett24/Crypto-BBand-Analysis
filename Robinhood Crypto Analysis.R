##functions for bollinger band price action identification
require(RobinHood)
require(httr)
require(lubridate)
require(quantmod)
require(PerformanceAnalytics)
require(rvest)
require(pbapply)

RH = api_login(username = "********", password = "********", mfa_code = "********")

##establishing new environment
PASS <- new.env()
assign("username","********",envir = PASS)
assign("password","********",envir = PASS)
assign("mfa_code", "********", envir = PASS)

##adjusting time zone differences
tmDIFF = round(as.numeric(difftime(Sys.time(),
                                   lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                   units = "hours")),0)
RobinHood(username = PASS$username, password = PASS$password, mfa_code = PASS$mfa_code)

##Crypto OHLC data
get_historicals_crypto = function(COIN,interval,span,bounds){
  RH = RobinHood(username = PASS$username, password = PASS$password, mfa_code = PASS$mfa_code)
  # URL and token
  url = paste0("https://nummus.robinhood.com/currency_pairs/?symbols=",COIN)
  token <- paste("Bearer", RH$tokens.access_token)
  
  # GET call
  reqID <- GET(url,
               add_headers("Accept" = "application/json",
                           "Content-Type" = "application/json",
                           "Authorization" = token))
  
  # Format return
  reqID <- mod_json(reqID, "fromJSON")
  reqID <- as.data.frame(reqID$results)
  # extract Crypto ID
  cryptoID = reqID[which(reqID$asset_currency$code == COIN),"id"]
  # url to get data
  url = paste0("https://api.robinhood.com/marketdata/forex/historicals/",cryptoID,
               "/?symbol=",COIN,"&interval=",interval,"&span=",span,"&bounds=",bounds)
  token <- paste("Bearer", RH$tokens.access_token)
  
  # GET call
  dta <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Authorization" = token
             ))
  
  # Format return
  dta2 <- mod_json(dta, "fromJSON")
  dta2 <- as.data.frame(dta2$data_points)
  
  dta2$begins_at <- as.POSIXct(as.character(dta2$begins_at), 
                               format="%Y-%m-%dT%H:%M:%SZ",TZ="UTC")
  dta2 = dta2[,c("begins_at","open_price",
                 "high_price","low_price",
                 "close_price")]
  logout(RH)
  dta2
}

##Example of function
eth_data <- get_historicals_crypto("ETH", "week", "month", "extended")

##combining historical data with most recent quote with rbind
get_latest_crypto_quote = function(COIN,dta2,interval)
{
  RH = RobinHood(username = PASS$username, password = PASS$password)
  rn = get_quote_crypto(RH,COIN)
  toRbind = rn[,c("open_price","high_price","low_price","mark_price")]
  tm = as.POSIXct(last(dta2$begins_at),format="%Y-%m-%d %H:%M:%S")
  tm = get_next_bar(tm,interval)
  toRbind = cbind(tm,toRbind)
  colnames(toRbind) = c("begins_at","open_price","high_price",
                        "low_price","close_price")
  logout(RH)
  rbind(dta2,toRbind)
}

##Calculate BBand and return hit when price exceeds BBand
get_crypto_bands = function(dta2){
  colnames(dta2)[2:5] = c("Open","High","Low","Close")
  crypto= xts(dta2[,2:5], order.by = dta2$begins_at)
  crypto = reclass(apply(crypto,2,as.numeric),match.to = crypto)
  #chartSeries(OHLC(crypto))
  #write.zoo(crypto,"~/Desktop/ETH.csv")
  Bands =  BBands(HLC(crypto),n = 20,sd = 2)
  
  crypto = cbind(crypto,Bands)
  crypto = na.omit(crypto)
  crypto = crypto[,-ncol(crypto)]
  crypto$sig = NA
  
  crypto
}

