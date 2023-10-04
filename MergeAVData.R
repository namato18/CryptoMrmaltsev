library(stringr)
library(aws.s3)
library(purrr)
library(riingo)

possibly_riingo_fx_prices = possibly(riingo_fx_prices, otherwise = "ERROR")
possibly_riingo_crypto_prices = possibly(riingo_crypto_prices, otherwise = "ERROR")
possibly_riingo_prices = possibly(riingo_prices, otherwise = "ERROR")


Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

# ls.file.names = list.files(path = "AlphaVantageData/", full.names = TRUE)
# ls.files = list.files(path = "AlphaVantageData/")
# 
# ind = grep(pattern="df.simple.comb", ls.file.names)
# 
# ls.file.names[ind]
# 
# short.names = str_match(string = ls.file.names[ind], pattern = "df\\.simple\\.comb\\.(.*)\\.rds")[,2]
# 
# 
# for(i in 1:length(short.names)){
#   df = readRDS(ls.file.names[ind[i]])
#   
#   df$Topic = NA
#   df$Topic = short.names[i]
#   
#   if(i == 1){
#     df.master.comb = df[0,]
#   }
#   
#   df.master.comb = rbind(df.master.comb, df)
# }
# # Grab all forex historicals --------------------------------------------
# df.forex = df.master.comb[grep("FOREX",df.master.comb$ticker),] %>%
#   filter(relevance_score > 0.5 & (ticker_sentiment_label == "Bullish") | (ticker_sentiment_label == "Bearish"))
# 
# df.forex$one.hr.back = NA
# df.forex$thirty.min.back = NA
# df.forex$price.news.break = NA
# df.forex$thirty.min.forward = NA
# df.forex$one.hr.forward = NA
# 
# ticker.forex = str_replace(string = df.forex$ticker, pattern = "FOREX:", replacement = "") 
# 
# ticker.forex[ticker.forex == "USD"] = "usdcad"
# ticker.forex[ticker.forex == "EUR"] = "eurusd"
# ticker.forex[ticker.forex == "GBP"] = "gbpusd"
# ticker.forex[ticker.forex == "NZD"] = "nzdusd"
# ticker.forex[ticker.forex == "AUD"] = "audusd"
# ticker.forex[ticker.forex == "CHF"] = "chfjpy"
# 
# ind = grep("usdcad|eurusd|gbpusd|nzdusd|audusd", ticker.forex)
# ticker.forex = ticker.forex[ind]
# 
# for(i in 1:length(ticker.forex)){
#   test = possibly_riingo_fx_prices(ticker = ticker.forex[i], start_date = df.forex$date[ind[i]] - (60*60), end_date = df.forex$date[ind[i]] + (60*60), resample_frequency = "30min")
#   
#   if(test[1] == "ERROR"){
#     next()
#   }
#   df.forex$one.hr.back[i] = test$close[1]
#   df.forex$thirty.min.back[i] = test$close[2]
#   df.forex$price.news.break[i] = test$close[3]
#   df.forex$thirty.min.forward[i] = test$close[4]
#   df.forex$one.hr.forward[i] = test$close[5]
#   print(i)
# }
# # --------------------------------------------
# 
# # Grab all Crypto --------------------------------------------
# df.crypto = df.master.comb[grep("CRYPTO",df.master.comb$ticker),] %>%
#   filter(relevance_score > 0.5 & (ticker_sentiment_label == "Bullish") | (ticker_sentiment_label == "Bearish"))
# 
# df.crypto$one.hr.back = NA
# df.crypto$thirty.min.back = NA
# df.crypto$price.news.break = NA
# df.crypto$thirty.min.forward = NA
# df.crypto$one.hr.forward = NA
# 
# ticker.crypto = str_replace(string = df.crypto$ticker, pattern = "CRYPTO:", replacement = "")
# 
# for(i in 1:length(ticker.crypto)){
#   test = possibly_riingo_crypto_prices(ticker = paste0(ticker.crypto[i],'USDT'), start_date = df.crypto$date[i] - (60*60), end_date = df.crypto$date[i] + (60*60), resample_frequency = "30min")
#   
#   if(test[1] == "ERROR"){
#     next()
#   }
#   df.crypto$one.hr.back[i] = test$close[1]
#   df.crypto$thirty.min.back[i] = test$close[2]
#   df.crypto$price.news.break[i] = test$close[3]
#   df.crypto$thirty.min.forward[i] = test$close[4]
#   df.crypto$one.hr.forward[i] = test$close[5]
#   print(i)
#   Sys.sleep(4)
# }
# # --------------------------------------------
# 
# #stopped at 1610
# # Grab all Stocks --------------------------------------------
# df.stock = df.master.comb[!grepl("FOREX|CRYPTO",df.master.comb$ticker),] %>%
#   filter(relevance_score > 0.5 & (ticker_sentiment_label == "Bullish") | (ticker_sentiment_label == "Bearish"))
# 
# df.stock$one.day.back.end = NA
# df.stock$current.day.end = NA
# df.stock$one.day.forward.end = NA
# 
# for(i in 1:nrow(df.stock)){
#   test = possibly_riingo_prices(ticker = paste0(df.stock$ticker[i]), start_date = df.stock$date[i] - (60*60*24), end_date = df.stock$date[i] + (60*60*24), resample_frequency = "daily")
#   
#   if(test[1] == "ERROR"){
#     next()
#   }
#   df.stock$one.day.back.end = test$close[1]
#   df.stock$current.day.end = test$close[2]
#   df.stock$one.day.forward.end = test$close[3]
#   print(i)
#   
#   Sys.sleep(4)
# }
# 
# # --------------------------------------------
# df.comb.all = rbind(df.forex, df.crypto)
# 
# tmp.dir = tempdir()
# 
# saveRDS(df.master.comb, paste0(tmp.dir,"/df.master.comb.rds"))
# 
# put_object(
#   file = paste0(tmp.dir,"/df.master.comb.rds"),
#   object = "df.master.comb.rds",
#   bucket = "cryptomlbucket/AlphaVantageData"
# )

# --------------------------------------------
# --------------------------------------------
# --------------------------------------------
# --------------------------------------------
# -------------------------------------------- for updating singular data.sets
df.comb.all = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/AlphaVantageData", object = "df.comb.all")

df.comb.without.one = df.comb.all[!grepl("CRYPTO",df.comb.all$ticker),]

df.comb.added = rbind(df.comb.without.one, df.crypto)

tmp.dir = tempdir()

saveRDS(df.comb.added, paste0(tmp.dir,"/df.comb.all.v2.rds"))

put_object(
  file = paste0(tmp.dir,"/df.comb.all.v2.rds"),
  object = "df.comb.all.v2.rds",
  bucket = "cryptomlbucket/AlphaVantageData"
)
# --------------------------------------------
# --------------------------------------------
# --------------------------------------------
# --------------------------------------------
# --------------------------------------------

