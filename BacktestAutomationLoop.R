library(stringr)
library(lubridate)
library(xgboost)
library(quantmod)
library(caret)
library(riingo)
library(usethis)
library(CandleStickPattern)
library(plotly)
library(chron)
library(aws.s3)
library(dplyr)
library(purrr)
library(tictoc)

tic()
api.key.av = "1RF2MSZAZY8XHUGV"
options(scipen=999)
possible_json = possibly(.f = jsonlite::fromJSON, otherwise = 'ERROR' )
possibly_riingo_crypto_prices = possibly(riingo_crypto_prices, otherwise = "ERROR")

possibly_s3read_using = possibly(s3read_using, otherwise = "ERROR")

readRenviron(".Renviron")
Sys.setenv(TZ='UTC')
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)


# token.names.df = readRDS("tickers/token.names.df.rds")

Sys.getenv()


shortBacktestCoins = c("REEFUSDT")
shortBacktestInterval = "4hour"
confidenceBacktestAutomation = seq(from = 0.1, to = 0.9, by = 0.1)
shortBacktestTimeframe = 360
feeInput = 0
shortBacktestTarget = seq(from = 0.2, to = 3, by = 0.2)
shortBacktestTP = seq(from = 0.2, to = 4, by = 0.2)
shortBacktestSL = 0

df.examine = data.frame(Coin = character(),
                        Interval = character(),
                        Confidence = character(),
                        Timeframe = character(),
                        Fee = character(),
                        Target = character(),
                        TakeProfit = character(),
                        StopLoss = character(),
                        PL = character())

ohlc.list = list()
to.remove = c()

if(shortBacktestInterval == '4hour' | shortBacktestInterval == '8hour'| shortBacktestInterval == '1hour'| shortBacktestInterval == '15min' |
   shortBacktestInterval == '30min' | shortBacktestInterval == '45min'){
  #df1 = riingo_crypto_prices('REEFUSDT', end_date = Sys.Date(), resample_frequency = '4hour')
  #df1 = df1[-nrow(df1),]
  #df2 = riingo_crypto_latest('REEFUSDT', resample_frequency = '4hour')
  #df = rbind(df1,df2)
  usd.usdt = str_match(string = shortBacktestCoins, pattern = "(.*)USDT")[,2]
  # df.usd = riingo_crypto_prices(paste0(usd.usdt, "USD"), start_date = Sys.Date() - as.numeric(shortBacktestTimeframe), end_date = Sys.Date(), resample_frequency = shortBacktestInterval, exchanges = "mexc")
  df1 = riingo_crypto_prices(shortBacktestCoins, start_date = Sys.Date() - as.numeric(shortBacktestTimeframe), end_date = Sys.Date(), resample_frequency = shortBacktestInterval, exchanges = "mexc")
  df1 = df1[-nrow(df1),]
  df2 = riingo_crypto_latest(shortBacktestCoins, resample_frequency = shortBacktestInterval, exchanges = "mexc")
  df = rbind(df1,df2)
  if(any(duplicated(df$date))){
    df = df[-duplicated(df$date),]
  }
}else{
  df = riingo_crypto_prices(shortBacktestCoins, start_date = Sys.Date() - as.numeric(shortBacktestTimeframe), end_date = Sys.Date(), resample_frequency = shortBacktestInterval, exchanges = "mexc")
}

# Modify data to be more useable
df = df[,4:9]
df$Percent.Change = NA

colnames(df) = c("Date","Open","High","Low","Close","Volume","Percent.Change")
df$Percent.Change = round((((df$High / df$Open) * 100) - 100), digits = 1)



#Add column for binary previouos day change+
df$Previous = NA
for(k in 2:nrow(df)){
  if(df$Percent.Change[k - 1] <= 0){
    df$Previous[k] = 0
  }else{
    df$Previous[k] = 1
  }
}

# df$Percent.Change = c(NA,df$Percent.Change[-nrow(df)])


# Remove first row since we can't use it
df = df[-1,]
df.9 = df

# Adding Moving Averages
df$MA10 = NA
df$MA20 = NA

for(k in 21:nrow(df)){
  df$MA10[k] = mean(df$Close[k-10:k])
  df$MA20[k] = mean(df$Close[k-20:k])
}
# df$MA10 = round(df$MA10, digits = 2)
# df$MA20 = round(df$MA20, digits = 2)

# Add column for if MA10 is above or below MA20
df$MAAB = 0

df$MAAB[df$MA10 > df$MA20] = 1

df = df[,-which(colnames(df) %in% c("MA10","MA20"))]

# Convert to actual dates and remove year and change to numeric
#df$Date = str_replace(string = df$Date, pattern = "T", replacement = " ")
#df$Date = str_replace(string = df$Date, pattern = "Z", replacement = "")

df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")

df = df[!is.na(df$Date),]


df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")

df = as.xts(df)


candle.list = list(hammer(df), inverted.hammer(df), bearish.engulf(df), bullish.engulf(df), up.trend(df), down.trend(df))

# Remove unusable rows
for(k in 1:length(candle.list)){
  df = cbind(df, candle.list[[k]])
}
df = df[-(1:20),]


# Add lagged values
for(k in 1:5){
  high.lag = Lag(df$High, k)
  close.lag = Lag(df$Close, k)
  percent.change.lag = ((high.lag/close.lag) - 1) * 100
  df = cbind(df, percent.change.lag)
  
}

df = df[-c(1:5),]

df[is.na(df)] = 0

# remove only last row
df = df[-nrow(df),]

### Grab open high low close for later
df.ohlc = as.data.frame(df[,c(1:4)])
df.ohlc$Coins = shortBacktestCoins
df.ohlc$Time = row.names(df.ohlc)

# REMOVE FIRST ONE TO GET TIMEING RIGHT FOR PURCHASES
df.ohlc = df.ohlc[-1,]

### Remove OPEN HIGH LOW CLOSE
df = df[,-c(1:4)]




for(z in 1:length(shortBacktestTarget)){
  for(y in 1:length(confidenceBacktestAutomation)){
    for(n in 1:length(shortBacktestTP)){
      
      
      
      
      for(i in 1:length(shortBacktestCoins)){
        

        bst = s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/TiingoBoosts"),
                           object = paste0("bst_",shortBacktestCoins[i],"_",shortBacktestInterval,shortBacktestTarget[z],".rds"))
        
  
        ############################################# 
        ############################################# PREDICT CURRENT CANDLE
        predict.next = predict(bst, df)
        
        
        
        if(i == 1){
          predictions.comb = data.frame(predict.next)
        }else{
          if(length(predict.next) != nrow(predictions.comb)){
            print("skipping coin")
            to.remove = c(to.remove, i)
            next()
          }
          predictions.comb = cbind(predictions.comb, predict.next)
        }
        
        temp.list = list(df.ohlc = df.ohlc)
        assign(paste0("temp.list.",shortBacktestCoins[i]),temp.list,.GlobalEnv)
        
        ohlc.list = c(ohlc.list,temp.list)
        
        print(paste0(i," out of: ",length(shortBacktestCoins)))
      }
      
      if(length(to.remove) != 0){
        colnames(predictions.comb) = shortBacktestCoins[-to.remove]
      }else{
        colnames(predictions.comb) = shortBacktestCoins
      }
      
      t.predictions.comb = t(predictions.comb)
      
      woulda.bought = c()
      confidence.scores = c()
      for(i in 1:ncol(t.predictions.comb)){
        x = (which(t.predictions.comb[,i] >= confidenceBacktestAutomation[y] & t.predictions.comb[,i] == max(t.predictions.comb[,i])))
        conf = max(t.predictions.comb[,i])
        if(length(x) < 1){
          x = NA
          conf = NA
        }
        
        confidence.scores = c(confidence.scores, conf)
        woulda.bought = c(woulda.bought,x)
      }
      
      df.purchases = ohlc.list[[woulda.bought[1]]][0,]
      
      for(i in 1:(length(woulda.bought))){
        if(is.na(woulda.bought[i])){
          next()
        }
        
        temp.df = ohlc.list[[woulda.bought[i]]][i,]
        df.purchases = rbind(df.purchases,temp.df)
        
      }
      if(any(is.na(confidence.scores))){
        df.purchases$Confidence = round(confidence.scores[-which(is.na(confidence.scores))], 3)
      }else{
        df.purchases$Confidence = round(confidence.scores, 3)
      }
      
      df.purchases = na.omit(df.purchases)
      
      df.purchases$OH = round((df.purchases$High - df.purchases$Open) / df.purchases$Open * 100, 3)
      df.purchases$OL = round((df.purchases$Low - df.purchases$Open) / df.purchases$Open * 100, 3)
      df.purchases$OC = round((df.purchases$Close - df.purchases$Open) / df.purchases$Open * 100, 3)
      
      #df.purchases = left_join(df.purchases, df.coins.running[,c(3,7)], by = "Coins")
      df.purchases$TakeProfit = shortBacktestTP[n]
      df.purchases$StopLoss = shortBacktestSL
      
      df.purchases$PL = 0
      df.purchases$PL[df.purchases$OH >= df.purchases$TakeProfit] = df.purchases$TakeProfit[df.purchases$OH >= df.purchases$TakeProfit]
      df.purchases$PL[df.purchases$OH < df.purchases$TakeProfit] = df.purchases$OC[df.purchases$OH < df.purchases$TakeProfit]
      
      
      # IN DEPTH STOP LOSS
      if(shortBacktestSL != 0){
        
        df.purchases$PL[df.purchases$OL <= shortBacktestSL] = shortBacktestSL
        
        for(i in 1:nrow(df.purchases)){
          if(df.purchases$OH[i] >= shortBacktestTP[n] & df.purchases$OL[i] <= shortBacktestSL){
            
            df = possibly_riingo_crypto_prices(ticker = df.purchases$Coins[i],
                                               start_date = as.Date(df.purchases$Time[i]),
                                               end_date = as.Date(df.purchases$Time[i]) + 1,
                                               resample_frequency = "5min",
                                               exchanges = "mexc")
            df = df %>%
              filter(df$date >= df.purchases$Time[i] & df$date <= as_datetime(df.purchases$Time[i]) + as.duration(shortBacktestInterval))
            df = df[-nrow(df),]
            
            first.TP = min(which(((df$high - df$open[1]) / df$open[1] * 100) >= shortBacktestTP[n]))
            first.SL = min(which(((df$low - df$open[1]) / df$open[1] * 100) <= shortBacktestSL))
            
            if(first.TP < first.SL){
              df.purchases$PL[i] = shortBacktestTP[n]
            }else{
              df.purchases$PL[i] = shortBacktestSL
            }
          }
          print(i)
        }
      }
      
      numeric_cols = sapply(df.purchases, is.numeric)
      df.purchases[numeric_cols] = lapply(df.purchases[numeric_cols], signif, digits = 6)
      
      PL = sum(df.purchases$PL)
      
      df.purchases$OH = paste0(df.purchases$OH, " %")
      df.purchases$OC = paste0(df.purchases$OC, " %")
      df.purchases$TakeProfit = paste0(df.purchases$TakeProfit, " %")
      df.purchases$PL = paste0(df.purchases$PL, " %")
      
      colnames(df.purchases) = c("Open", "High", "Low", "Close", "Coin", "Time (UTC)", "Confidence Scores", "Open/High","Open/Low", "Open/Close", "Take Profit","Stop Loss", "PL")
      
      fee.to.subtract = feeInput * nrow(df.purchases) * 2
      PL = PL - fee.to.subtract
      
      
      temp.df2 = data.frame(
        Coin = shortBacktestCoins,
        Interval = shortBacktestInterval,
        Confidence = confidenceBacktestAutomation[y],
        Timeframe = shortBacktestTimeframe,
        Fee = 0,
        Target = shortBacktestTarget[z],
        TakeProfit = shortBacktestTP[n],
        StopLoss = 0,
        PL = PL
      )
      
      df.examine = rbind(df.examine, temp.df2)
      print(n)
    }
    print(y)
  }
  print(z)
}

toc()

write.csv(df.examine, file = "df.examine.reefusdt.4hour.360day.mexc.csv")
