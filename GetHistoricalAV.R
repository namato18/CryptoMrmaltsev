library(htmltools)
library(xml2)
library(rvest)
library(lubridate)
library(tictoc)
library(stringr)
library(purrr)
library(riingo)
library(aws.s3)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

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
ts.month = tail(ts.month, 100)

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
if(any(duplicated(names(data.comb)))){
  data.comb.rem = data.comb[-which(duplicated(names(data.comb)))]
}

df.stock.comb = data.frame(data.comb[[1]])[0,]

for(i in 218391:length(data.comb)){
  dat = data.frame(data.comb[[i]])
  df.stock.comb = rbind(df.stock.comb,dat)
  print(i)
}
df.stock.comb$date = NA
df.stock.comb$date = names(data.comb)

colnames(df.stock.comb) = c("open","high","low","close","volume",'date')


tmpdir = tempdir()

saveRDS(df.stock.comb, paste0(tmpdir,"/df.stock.comb.rds"))

put_object(
  file = paste0(tmpdir,"/df.stock.comb.rds"),
  object = "df.stock.spy.prices.rds",
  bucket = "cryptomlbucket/ForexFactoryData/News_With_Prices"
)

df.historical = df.comb[grepl("red",df.comb$impact) & df.comb$actual != "" & df.comb$forecast != "" & !is.na(df.comb$Tag),]

df.historical$one.hr.back = NA
df.historical$thirty.min.back = NA
df.historical$price.news.break = NA
df.historical$thirty.min.forward = NA
df.historical$one.hr.forward = NA

for(i in 6037:nrow(df.historical)){
  ind = which(as_datetime(df.stock.comb$date) == df.historical$POSIXct[i])
  if(length(ind) == 0){
    print(paste0("skipping: ",i))
    next()
  }
  
  df.historical$one.hr.back[i] = df.stock.comb$close[ind-2]
  df.historical$thirty.min.back[i] = df.stock.comb$close[ind-1]
  df.historical$price.news.break[i] = df.stock.comb$close[ind]
  df.historical$thirty.min.forward[i] = df.stock.comb$close[ind+1]
  df.historical$one.hr.forward[i] = df.stock.comb$close[ind+2]
  print(i)
}

df.historical.naomit = na.omit(df.historical)

tmpdir = tempdir()

saveRDS(df.historical.naomit, paste0(tmpdir,"/df.historical.naomit.rds"))

put_object(
  file = paste0(tmpdir,"/df.historical.naomit.rds"),
  object = "df.stock.news.prices.SPY5min.rds",
  bucket = "cryptomlbucket/ForexFactoryData/News_With_Prices"
)

########################################################
########################################################
########################################################
########################################################
######################################################## FOR CRYPTO

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

tmpdir = tempdir()

saveRDS(df.btc.news.prices, paste0(tmpdir,"/df.btc.news.prices.rds"))

put_object(
  file = paste0(tmpdir,"/df.btc.news.prices.rds"),
  object = "df.btc.news.prices.rds",
  bucket = "cryptomlbucket/ForexFactoryData/News_With_Prices"
)
########################################################
########################################################
########################################################
########################################################
######################################################## FOR FOREX
# pair = c("USDCAD", 'AUDUSD','GBPUSD')
pair = c("USDCAD","AUDUSD","GBPUSD","EURUSD", "USDCHF", "USDJPY", "CHFJPY", "CNHJPY", "NZDJPY")
timeframe = c("5min","30min","60min")
fallback = c(1,6,12)


for(k in 6:length(pair)){
  
  
  df.forex.historical = df.comb[grepl("red|ora",df.comb$impact) & df.comb$actual != "" & df.comb$forecast != "" & !is.na(df.comb$Tag),]
  df.forex.historical = na.omit(df.forex.historical)
  
  ts = seq(ymd('2007-08-05'),ymd('2023-10-01'),by='2 weeks')
  
  forex.historical.thirty.comb = possibly_riingo_fx_prices(ticker = pair[k],
                                                           resample_frequency = "5min")[0,]
  for(i in 1:length(ts)){
    dat = possibly_riingo_fx_prices(ticker = pair[k],
                                    start_date = ts[i] - 14,
                                    end_date = ts[i],
                                    resample_frequency = "5min")
    if(is.null(nrow(dat))){
      print(paste0("ERROR FOUND AT ",i))
      next()
    }
    
    forex.historical.thirty.comb = rbind(forex.historical.thirty.comb,dat)
    print(paste0(i," of: ",length(ts)))
  }
  
  if(any(duplicated(forex.historical.thirty.comb$date))){
    forex.historical.thirty.comb = forex.historical.thirty.comb[-which(duplicated(forex.historical.thirty.comb$date)),]
  }
  
  df.forex.historical$one.hr.back = NA
  df.forex.historical$thirty.min.back = NA
  df.forex.historical$price.news.break = NA
  df.forex.historical$thirty.min.forward = NA
  df.forex.historical$one.hr.forward = NA

  for(j in 1:length(fallback)){
    
    numeric_value <- as.numeric(str_extract(timeframe[j], "\\d+"))
    
    for(i in 1:nrow(df.forex.historical)){
      ind = which(as_datetime(forex.historical.thirty.comb$date) == df.forex.historical$POSIXct[i])
      if(length(ind) == 0){
        next()
      }
      
      df.forex.historical$one.hr.back[i] = forex.historical.thirty.comb$open[ind-(fallback[j]*2)]
      df.forex.historical$thirty.min.back[i] = forex.historical.thirty.comb$open[ind-(fallback[j])]
      df.forex.historical$price.news.break[i] = forex.historical.thirty.comb$open[ind]
      df.forex.historical$thirty.min.forward[i] = forex.historical.thirty.comb$open[ind+(fallback[j])]
      df.forex.historical$one.hr.forward[i] = forex.historical.thirty.comb$open[ind+(fallback[j]*2)]
      print(i)
    }
    df.forex.news.prices = na.omit(df.forex.historical)
    
    colnames(df.forex.news.prices)[c(12,13,15,16)] = c(paste0(numeric_value*2,"min.back"),
                                                       paste0(numeric_value,"min.back"),
                                                       paste0(numeric_value,"min.forward"),
                                                       paste0(numeric_value*2,"min.forward"))
    
    tmpdir = tempdir()
    
    saveRDS(df.forex.news.prices, paste0(tmpdir,"/df.forex.news.prices.rds"))
    
    put_object(
      file = paste0(tmpdir,"/df.forex.news.prices.rds"),
      object = paste0("df.forex.news.prices.",pair[k],timeframe[j],".rds"),
      bucket = "cryptomlbucket/ForexFactoryData/News_With_Prices"
    )
    
    print(paste0("done with: ",timeframe[j]))
  }
}
