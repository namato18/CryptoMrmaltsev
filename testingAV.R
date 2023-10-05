library(htmltools)
library(xml2)
library(purrr)
library(rvest)
library(lubridate)
library(quantmod)
library(tictoc)
library(stringr)
library(aws.s3)
library(riingo)

api.key.av = "1RF2MSZAZY8XHUGV"

possible_json = possibly(.f = jsonlite::fromJSON, otherwise = 'ERROR' )
possibly_parse_date_time = possibly(.f = parse_date_time, otherwise = "All Day")
possibly_s3read_using = possibly(s3read_using, otherwise = "ERROR")
possibly_riingo_fx_prices = possibly(riingo_fx_prices, otherwise = "ERROR")
# 
# 
# # Using the AV API --------------------------------
# 
# ts.av = seq(ymd('2023-01-01'),ymd('2023-09-26'),by='2 days')
# ts.av = str_replace_all(ts.av, pattern = "-", replacement = "")
# ts.av = paste0(ts.av,"T0000")
# 
# time.series = ts.av
# api.key = api.key.av
# topic="blockchain"
# 
# for(i in 1:(length(time.series)-1)){
#   full.url = paste0("https://www.alphavantage.co/query?function=NEWS_SENTIMENT&limit=1000&time_from=",time.series[i],"&time_to=",time.series[i + 2],"&topics=",topic,"&apikey=",api.key)
#   
#   test_get = httr::GET(full.url)
#   
#   test_get$status_code
#   
#   test = rawToChar(test_get$content)
#   
#   test = possible_json(test, flatten = TRUE)
#   df = test$feed
#   
#   if(i == 1){
#     assign("df.comb.earnings",df,.GlobalEnv)
#   }else{
#     df.comb.earnings = rbind(df.comb.earnings,df)
#   }
#   
#   Sys.sleep(3)
#   
#   print(i)
# }
# 
# df.comb = df.comb.blockchain
# # df.comb = df.comb.earnings
# saveRDS(df.comb, paste0("AlphaVantageData/df.comb.",topic,".rds"))
# 
# x = lubridate::parse_date_time(df.comb$time_published,"ymdHMS") %>%
#   round_date("30 min")
# 
# df.comb$date = x
# 
# # df.comb$tickers = NA
# # for(i in 1:nrow(df.comb)){
# #   df.comb$tickers[i] = paste0(df.comb$ticker_sentiment[[i]]$ticker, collapse = ",")
# # }
# 
# df.simple.comb = data.frame(ticker = character(),
#                             relevance_score = character(),
#                             ticker_sentiment_score = character(),
#                             ticker_sentiment_label = character(),
#                             date = character(),
#                             title = character(),
#                             source = character())
# 
# for(i in 1:nrow(df.comb)){
#   df.simple = df.comb$ticker_sentiment[[i]]
#   if(nrow(df.simple) == 0){
#     next()
#   }
#   df.simple$date = df.comb$date[i]
#   df.simple$title = df.comb$title[i]
#   df.simple$source = df.comb$source[i]
#   
#   df.simple.comb = rbind(df.simple.comb, df.simple)
#   print(i)
# }
# df.simple.comb = df.simple.comb[!duplicated(df.simple.comb),]
# saveRDS(df.simple.comb,paste0("AlphaVantageData/df.simple.comb.",topic,".rds"))
# 
# 
# df.forex = df.simple.comb[grep("FOREX",df.simple.comb$ticker),]
# df.crypto = df.simple.comb[grep("CRYPTO",df.simple.comb$ticker),]
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
# 
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
# saveRDS(df.forex, paste0("AlphaVantageData/df.forex.",topic,".rds"))

# --------------------------------
# 
# ticker.usdt = str_replace(string = df.crypto$ticker, pattern = "CRYPTO:", replacement = "") %>%
#   paste0("USDT")
# day.only = ymd(df.crypto$date)
# 
# 
# 
# 
# riingo::riingo_crypto_prices(ticker = "BTCUSDT", start_date = df.crypto$date[1] - (60*60),end_date = df.crypto$date[1] + (60*60), resample_frequency = "30min")

# 2007-08-05

# Webscraping ForexFactory --------------------------------
# ts.test = seq(ymd('2007-08-05'),ymd('2023-09-24'),by='weeks')
# 
# months.test = month(ts.test)
# months.abr = month.abb
# 
# months.test = months.abr[months.test]
# 
# days.test = day(ts.test)
# years.test = year(ts.test)
# 
# for(j in 1:length(ts.test)){
#   j=1
#   url = paste0("https://www.forexfactory.com/calendar?week=", ts.test[j])
#   
# }
# 
# page = read_html(url)
# 
# date = page %>% html_nodes(".date") %>% html_text()
# time = (page %>% html_nodes(".calendar__time") %>% html_text())[-1]
# currency = (page %>% html_nodes(".calendar__currency") %>% html_text())[-1]
# impact = page %>% html_nodes(".calendar__impact") %>% html_children() %>% html_attrs()
# event.title = page %>% html_nodes(".calendar__event-title") %>% html_text()
# actual = (page %>% html_nodes(".calendar__actual") %>% html_text())[-1]
# forecast = (page %>% html_nodes(".calendar__forecast") %>% html_text())[-1]
# previous = (page %>% html_nodes(".calendar__previous") %>% html_text())[-1]
# 
# df = data.frame(time = time,
#                 currency = currency,
#                 event.title = event.title,
#                 actual = actual,
#                 forecast = forecast,
#                 previous = previous
# )
# df$date = NA
# df$date[1] = date[1]
# 
# for(i in 2:nrow(df)){
#   if(df$time[i]==""){
#     df$time[i] = df$time[i-1]
#   }
# }
# 
# x = parse_date_time(df$time, "%I:%M%p")
# 
# date.counter = 2
# 
# for(i in 2:length(x)){
#   if(is.na(x[i])){
#     next()
#   }
#   if((x[i] < x[i-1]) == TRUE){
#     df$date[i] = date[date.counter]
#     date.counter = date.counter + 1
#   }
# }
# Analyze Gathered Data --------------------------------
ts.test = seq(ymd('2007-08-05'),ymd('2023-10-01'),by='weeks')
months.test = month(ts.test)
months.abr = month.abb

months.test = tolower(months.abr[months.test])

days.test = day(ts.test)
years.test = year(ts.test)

week.test = paste0(months.test,days.test,".",years.test)

df.comb = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/ForexFactoryData/WithImpact", object = paste0("df_",week.test[1],".rds"))[0,]

tic()
for(i in 1:length(ts.test)){
  df = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/ForexFactoryData/WithImpact", object = paste0("df_",week.test[i],".rds"))
  
  if(is.null(nrow(df))){
    print(paste0("Error'd out at: ",i))
    next()
  }else{
    df.comb = rbind(df.comb, df)
  }
  
}
toc()

df.comb$Tag = NA
df.comb$Tag[grep("CPI|GDP|Price Index|PPI|Earnings",df.comb$event.title, ignore.case = TRUE)] = "Inflation"
df.comb$Tag[grep("Sales|Consumer|Spending|Loan|production|lend|trade balance",df.comb$event.title, ignore.case = TRUE)] = "Growth"
df.comb$Tag[grep("Employment|Job",df.comb$event.title, ignore.case = TRUE)] = "Employment"
df.comb$Tag[grep("FOMC|official bank|rate|monetary policy|money supply|budget",df.comb$event.title, ignore.case = TRUE)] = "Central Bank"
df.comb$Tag[grep("bond",df.comb$event.title, ignore.case = TRUE)] = "Bonds"
df.comb$Tag[grep("HPI|construction|hous|building|home|mortgage",df.comb$event.title, ignore.case = TRUE)] = "Housing"
df.comb$Tag[grep("consumer|sentiment",df.comb$event.title, ignore.case = TRUE)] = "Consumer Surveys"
df.comb$Tag[grep("PMI|business climate|manufacturing index|business index",df.comb$event.title, ignore.case = TRUE)] = "Business Surveys"
df.comb$Tag[grep("speaks",df.comb$event.title, ignore.case = TRUE)] = "Speeches"

df.comb$POSIXct = paste0(df.comb$date," ",df.comb$time) %>%
  strptime(format = "%a %b %d %Y %I:%M%p") %>%
  as.POSIXct(tz = "UTC")

saveRDS(df.comb,"AlphaVantageData/df.tagged.rds")


