library(htmltools)
library(xml2)
library(rvest)
library(lubridate)
library(tictoc)

########################################################
########################################################
########################################################
########################################################
######################################################## FOR STOCKS

possible_json = possibly(.f = jsonlite::fromJSON, otherwise = 'ERROR' )
possibly_parse_date_time = possibly(.f = parse_date_time, otherwise = "All Day")
possibly_s3read_using = possibly(s3read_using, otherwise = "ERROR")
possibly_riingo_fx_prices = possibly(riingo_fx_prices, otherwise = "ERROR")
possibly_riingo_crypto_prices = possibly(riingo_crypto_prices, otherwise = "ERROR")
possibly_riingo_prices = possibly(riingo_prices, otherwise = "ERROR")

api.key.av = "1RF2MSZAZY8XHUGV"

ts.month = seq(ym('2007-08'),ym('2023-10'),by='month')
ts.month = format(ts.month, "%Y-%m")

tic()
for(i in 1:length(ts.month)){
  full.url = paste0("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=SPY&interval=5min&month=",ts.month[i],"&outputsize=full&apikey=",api.key.av)
  
  test_get = httr::GET(full.url)
  
  test_get$status_code
  
  test = rawToChar(test_get$content)
  
  test = possible_json(test, flatten = TRUE)
  
  data = test$`Time Series (5min)`
  
  if(i == 1){
    data.comb = data
  }else{
    data.comb = c(data,data.comb)
  }
  Sys.sleep(20)
}
toc()

data.comb.rem = data.comb[-which(duplicated(names(data.comb)))]

########################################################
########################################################
########################################################
########################################################
######################################################## FOR CRYPTO/FOREX

df.btc.historical = df.comb[grepl("red",df.comb$impact) & df.comb$actual != "" & df.comb$forecast != "" & !is.na(df.comb$Tag),]
df.btc.historical = na.omit(df.btc.historical)

ts = seq(ymd('2007-08-05'),ymd('2023-10-01'),by='3 days')

btc.historical.thirty.comb = possibly_riingo_crypto_prices(ticker = "BTCUSDT",
                                                           start_date = ts[length(ts)] - 2,
                                                           end_date = ts[length(ts)] + 2,
                                                           resample_frequency = "30min")[0,]

for(i in 1:length(ts)){
  dat = possibly_riingo_crypto_prices(ticker = "BTCUSDT",
                                      start_date = ts[i] - 2,
                                      end_date = ts[i] + 2,
                                      resample_frequency = "30min")
  if(dat[1] == "ERROR"){
    print(paste0("ERROR FOUND AT ",i))
    next()
  }
  
  btc.historical.thirty.comb = rbind(btc.historical.thirty.comb,dat)
  print(paste0(i," of: ",length(ts)))
}

btc.historical.thirty.comb = btc.historical.thirty.comb[-which(duplicated(btc.historical.thirty.comb$date)),]


df.btc.historical$one.hr.back = NA
df.btc.historical$thirty.min.back = NA
df.btc.historical$price.news.break = NA
df.btc.historical$thirty.min.forward = NA
df.btc.historical$one.hr.forward = NA

for(i in 1:nrow(df.btc.historical)){
  ind = which(as_datetime(btc.historical.thirty.comb$date) == df.btc.historical$POSIXct[i])
  if(length(ind) == 0){
    next()
  }
  
  df.btc.historical$one.hr.back[i] = btc.historical.thirty.comb$close[ind-2]
  df.btc.historical$thirty.min.back[i] = btc.historical.thirty.comb$close[ind-1]
  df.btc.historical$price.news.break[i] = btc.historical.thirty.comb$close[ind]
  df.btc.historical$thirty.min.forward[i] = btc.historical.thirty.comb$close[ind+1]
  df.btc.historical$one.hr.forward[i] = btc.historical.thirty.comb$close[ind+2]
  print(i)
}
df.btc.news.prices = na.omit(df.btc.historical)
saveRDS(df.btc.news.prices, "AlphaVantageData/df.btc.news.prices.rds")

########################################################
########################################################
########################################################
########################################################
########################################################