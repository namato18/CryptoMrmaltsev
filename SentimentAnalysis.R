library(riingo)
library(jsonlite)
library(lubridate)
library(rvest)
library(xml2)
library(dplyr)
library(aws.s3)
library(xts)
library(xgboost)
library(httr)
library(CandleStickPattern)
library(quantmod)
library(caret)
library(quantmod)
library(purrr)

possibly_riingo_crypto_prices = possibly(riingo_crypto_prices, otherwise = "ERROR")

str1 = readRDS('tickers/str1.rds')


# httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))


ts.seq = seq(ymd("2023-01-01"), ymd(Sys.Date()), by="2 weeks")
ts.seq = format(ts.seq, format = "%Y%m%dT%H%M")
api.key = "1RF2MSZAZY8XHUGV"

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)


Sys.setenv(TZ='UTC')



# Funding rate data
funding.data = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FundingData", object = "FundingData.rds")
fd.for.merge = funding.data %>%
  select(fundingRate, date)
colnames(fd.for.merge) = c("funding.rate","date.8hr")
fd.for.merge$date.8hr = as.character(fd.for.merge$date.8hr)
fd.for.merge$date.8hr = as.POSIXlt(fd.for.merge$date.8hr, tz = 'UTC')
#

# webscrape fear

url = "https://production.dataviz.cnn.io/index/fearandgreed/graphdata/2021-07-05"

test_get = httr::GET(url)

test_get$status_code

test = rawToChar(test_get$content)

test = fromJSON(test, flatten = TRUE)

test$fear_and_greed_historical

df.fear.greed = test$fear_and_greed_historical$data

df.fear.greed$date = as.POSIXct(df.fear.greed$x/1000, origin = "1970-01-01", tz = "UTC")
df.fg.for.merge = df.fear.greed %>%
  select(y, date)
colnames(df.fg.for.merge) = c("rating", "just.date")

# Add VIX index
df.comb = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/AlphaVantageData", object = "df.comb.historical.blockchain.rds")

ind = which(duplicated(df.comb$title) & duplicated(df.comb$overall_sentiment_score))

df.comb.rem = df.comb[-ind,]
df.comb.rem$time = strptime(df.comb.rem$time_published, format = "%Y%m%dT%H%M%S")


df.vix = getSymbols("^VIX", auto.assign = FALSE)
colnames(df.vix) = c("open","high","low","close","volume","adjusted")
df.vix = as.data.frame(df.vix)
df.vix$time = as.POSIXct(row.names(df.vix))

df.vix.for.merge = df.vix %>%
  select(open, time)
colnames(df.vix.for.merge) = c("vix.open","just.date")

bad.coins = c()

for(i in 191:length(str1)){
  
  
x = possibly_riingo_crypto_prices(str1[i], start_date = Sys.Date() - lubridate::dmonths(14), end_date = Sys.Date(), resample_frequency = "1day")
x2 = possibly_riingo_crypto_prices(str1[i], start_date = Sys.Date() - lubridate::dmonths(28), end_date = Sys.Date() - lubridate::dmonths(14), resample_frequency = "1day")


if(length(x) == 1){
  bad.coins = c(bad.coins,i)
  next()
}

if(length(x2) == 1){
  x = x[,c(1,4:9)]
}else{
  x = x[,c(1,4:9)]
  x2 = x2[,c(1,4:9)]
  
  x = rbind(x2, x)
}



ind = which(duplicated(x$date))

if(length(ind > 0)){
  x = x[-ind,]
}


# trend detection
x.xts = as.xts(x)
upTrend = as.data.frame(up.trend(x.xts))
downTrend = as.data.frame(down.trend(x.xts))

x$upTrend = as.numeric(upTrend[,1])
x$downTrend = as.numeric(downTrend[,1])

x$upTrend = Lag(x$upTrend, 1)
x$downTrend = Lag(x$downTrend, 1)
#
x$date.8hr = floor_date(x$date, "8hour")

start.time = format(x$date[1] - lubridate::days(21), format = "%Y%m%dT%H%M")
end.time = format(x$date[1], format = "%Y%m%dT%H%M")






# for(i in 2:length(ts.seq)){
#   # full.url = paste0("https://www.alphavantage.co/query?function=NEWS_SENTIMENT&tickers=CRYPTO:BTC&limit=1000&time_from=",ts.seq[i-1],"&time_to=",ts.seq[i],"&sort=EARLIEST&apikey=",api.key)
  # full.url = paste0("https://www.alphavantage.co/query?function=NEWS_SENTIMENT&topics=blockchain&limit=1000&time_from=",ts.seq[i-1],"&time_to=",ts.seq[i],"&sort=EARLIEST&apikey=",api.key)
  # 
  # # full.url = paste0("https://www.alphavantage.co/query?function=NEWS_SENTIMENT&tickers=CRYPTO:BTC&limit=1000&time_from=","20231008T0000","&time_to=","20231022T0000","&sort=EARLIEST&apikey=",api.key)
  # 
  # test_get = httr::GET(full.url)
  # 
  # test_get$status_code
  # 
  # test = rawToChar(test_get$content)
  # 
  # test = fromJSON(test, flatten = TRUE)
  # df = test$feed
# 
#   if(i == 2){
#     df.comb = df
#   }else{
#     df.comb = rbind(df.comb,df)
#   }
# 
#   print(paste0(i, " of: ", length(ts.seq)))
#   Sys.sleep(21)
# }
# df.comb$ticker.sentiment.individual = NA
# 
# for(i in 1:nrow(df.comb)){
#   ind.for.sel = which(df.comb$ticker_sentiment[[i]]$ticker == "CRYPTO:INJ")
#   df.comb$ticker.sentiment.individual[i] = df.comb$ticker_sentiment[[i]]$ticker_sentiment_score[ind.for.sel]
# }
# 
# 
# saveRDS(df.comb, "df.comb.historical.inj.rds")
# tmpdir = tempdir()
# 
# saveRDS(df.comb, paste0(tmpdir,"/df.comb.historical.blockchain.rds"))
# 
# aws.s3::put_object(
#   file = paste0(tmpdir,"/df.comb.historical.blockchain.rds"),
#   object = "df.comb.historical.blockchain.rds",
#   bucket = "cryptomlbucket/AlphaVantageData"
# )
# 
# df.comb = readRDS("df.comb.historical.link.rds")



x$just.date = as.Date(x$date)

x = left_join(x, df.fg.for.merge, by = "just.date")
x = left_join(x, df.vix.for.merge, by = "just.date")
x = left_join(x, fd.for.merge, by = "date.8hr")

# if(is.na(x$rating[1])){
#   time.dif = x$date[1] - df.fg.for.merge$just.date
#   time.dif = time.dif[time.dif > 0]
#   time.dif = time.dif[length(time.dif)]
#   
#   x$rating[1] = df.fg.for.merge$rating[x$date[1] - df.fg.for.merge$just.date == time.dif]
# }

# x$rating = na.locf(x$rating)


x = na.omit(x)

if(nrow(x) < 25){
  bad.coins = c(bad.coins,i)
  next()
}

x$news.1hr = NA
x$news.8hr = NA
x$news.24hr = NA
for(k in 1:nrow(x)){

  x$news.1hr[k] = mean(df.comb.rem[which(df.comb.rem$time > x$date[k] - lubridate::hours(1) & df.comb.rem$time <= x$date[k]),]$overall_sentiment_score)
  x$news.8hr[k] = mean(df.comb.rem[which(df.comb.rem$time > x$date[k] - lubridate::hours(8) & df.comb.rem$time <= x$date[k]),]$overall_sentiment_score)
  x$news.24hr[k] = mean(df.comb.rem[which(df.comb.rem$time > x$date[k] - lubridate::hours(24) & df.comb.rem$time <= x$date[k]),]$overall_sentiment_score)
  


  

  # fear.greed.sub = (x$date[i] - df.fear.greed$date)
  # fear.greed.neg = fear.greed.sub[which(fear.greed.sub > 0)]
  # fear.greed.ind = which(fear.greed.neg == min(fear.greed.neg))
  # fear.greed.dif = fear.greed.neg[fear.greed.ind]
  # fear.greed.val = df.fear.greed$rating[which(fear.greed.sub == fear.greed.dif)]
  
  # vix.sub = x$date[i] - df.vix$time
  # vix.pos = vix.sub[which(vix.sub > 0)]
  # vix.ind = which(vix.pos == min(vix.pos))
  # vix.dif = vix.pos[vix.ind]
  # vix.val = df.vix$open[which(vix.sub == vix.dif)]
  
  # 
  # fear.greed.val = df.fear.greed$rating[which(df.fear.greed$date >= x$date[i] - lubridate::days(1) & df.fear.greed$date <= x$date[i])]
  # if(length(fear.greed.val) == 0){
  #   for(j in 1:25){
  #     fear.greed.val = df.fear.greed$rating[which(df.fear.greed$date >= x$date[i] - lubridate::days(1 + j) & df.fear.greed$date <= x$date[i])]
  #     
  #     if(length(fear.greed.val) == 0){
  #       next()
  #     }else{
  #       break()
  #     }
  #   }
  # }
  # 
  # x$fear.greed[i] = fear.greed.val
  # x$vix.open[i] = vix.val
  
  # print(i)
}
x[is.na(x)] = 0

x$OH = (x$high - x$open) / x$open * 100

x$volume.prev = Lag(x$volume, 1)
x$volume.1 = Lag(x$volume.prev, 1)
x$volume.8 = Lag(x$volume.prev, 8)
x$volume.24 = Lag(x$volume.prev, 24)
x$prev.open.1 = Lag(x$open, 1)
x$prev.open.8 = Lag(x$open, 8)
x$prev.open.24 = Lag(x$open, 24)

x$VC.1 = (x$volume.1 - x$volume.prev) / x$volume.prev * 100
x$VC.8 = (x$volume.8 - x$volume.prev) / x$volume.prev * 100
x$VC.24 = (x$volume.24 - x$volume.prev) / x$volume.prev * 100

x$OO.1 = (x$prev.open.1 - x$open) / x$open * 100
x$OO.8 = (x$prev.open.8 - x$open) / x$open * 100
x$OO.24 = (x$prev.open.24 - x$open) / x$open * 100

x = na.omit(x)

# df = x %>%
#   select(upTrend, downTrend, rating, vix.open, funding.rate, news.1hr, news.8hr, news.24hr,
#          VC.1, VC.8, VC.24, OO.1, OO.8, OO.24)
df = x %>%
  select(upTrend, downTrend, rating, vix.open, funding.rate, news.1hr, news.8hr, news.24hr)
df[] <- lapply(df, as.numeric)


# SET OUTCOMES
# outcomes = x$OH
for(j in seq(from = 0.2, to = 3, by = 0.2)){
  
outcomes = rep(0, nrow(df))
outcomes[x$OH >= j] = 1
#


set.seed(123)
sample.split = sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob=c(0.8,0.2))

train = df[sample.split,]
test = df[!sample.split,]

train = as.matrix(train)
test = as.matrix(test)

out.train = outcomes[sample.split]
out.test = outcomes[!sample.split]


############## GENERATE MODELS
set.seed(123)
# xgb_caret = caret::train(x = train,
#                          y = out.train,
#                          method = "xgbTree",
#                          objective = "binary:logistic",
#                          trControl = trainControl(method = "cv",
#                                                   number = 5,
#                                                   repeats = 1,
#                                                   verboseIter = TRUE),
#                          tuneGrid = expand.grid(nrounds = c(100,200),
#                                                 eta = c(0.01, 0.05),
#                                                 max_depth = c(10,20,50),
#                                                 colsample_bytree = c(0.5,1),
#                                                 subsample  = c(0.5,1),
#                                                 gamma = c(0,50),
#                                                 min_child_weight = c(0,20)))

# bst = xgboost(data = train,
#               label = out.train,
#               objective = "binary:logistic",
#               max.depth = 50,
#               nrounds = 200,
#               eta = 0.01,
#               gamma = 0,
#               colsample_bytree = 1,
#               min_child_weight = 20,
#               subsample = 0.5,
#               verbose = TRUE)
bst = xgboost(data = train,
              label = out.train,
              objective = "binary:logistic",
              max.depth = 50,
              nrounds = 200,
              eta = 0.01,
              gamma = 0,
              colsample_bytree = 1,
              subsample = 0.5,
              verbose = FALSE)

predictions = predict(bst, test)


df.examine = data.frame(actual.high = out.test,
                        predicted.prob = predictions)
df.examine$prediction = 0
df.examine$prediction[df.examine$predicted.prob >= 0.5] = 1

tmpdir = tempdir()

saveRDS(bst, paste0(tmpdir,"bst_",str1[i],"_blockchain_",j,".rds"))
saveRDS(df.examine, paste0(tmpdir,"df.examine_",str1[i],"_blockchain_",j,".rds"))


aws.s3::put_object(
  file = paste0(tmpdir,"bst_",str1[i],"_blockchain_",j,".rds"),
  object = paste0("bst_",str1[i],"_blockchain_",j,".rds"),
  bucket = "cryptomlbucket/AlphaVantageData/blockchain_models"
)

aws.s3::put_object(
  file = paste0(tmpdir,"df.examine_",str1[i],"_blockchain_",j,".rds"),
  object = paste0("df.examine_",str1[i],"_blockchain_",j,".rds"),
  bucket = "cryptomlbucket/AlphaVantageData/blockchain_models"
)

}

# 
# precision = length(which(df.examine$prediction == 1 & df.examine$actual.high == 1)) / length(which(df.examine$prediction == 1))
# recall = length(which(df.examine$prediction == 1 & df.examine$actual.high == 1)) / length(which(df.examine$actual.high == 1))
# length(which(df.examine$prediction == 1))

print(paste0(i," out of: ",length(str1)))

}

# df.examine$acceptable = 0
# df.examine$acceptable[abs(df.examine$predicted.high - df.examine$actual.high) < 0.2] = 1
# 
# RMSE = (mean((df.examine$actual.high - df.examine$predicted.high)^2))^(1/2)
