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
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
# symbol='btcusd'
createCandlePlot = function(symbol){
  return(get(paste0('df_candleplot_',symbol)))
}

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################


# CALC TIME TO NEXT CANDLE CLOSE
getTimeRemaining2 = function(timeframe, type){
  
  utcTime = lubridate::now(tzone = 'UTC')
  utcTime = format(utcTime, format = "%H:%M:%S")
  
  if(type == "Forex"){
    utcTime = utcTime - as.times("01:00:00")
  }
  
  if(timeframe == "15min"){
    hour.t = hours(chron(times=utcTime))
    minutes.t = minutes(chron(times=utcTime))
    
    candle.times = floor(seq(from = 0, to = 60, by = 14.95))
    
    
    ind = which(candle.times >= minutes.t)[1]
    candle.t.ind = candle.times[ind]
    
    end.of.candle = chron(times=paste0(hour.t,":",candle.t.ind,":59"))
    
    remainingTime = end.of.candle - utcTime
    return(remainingTime)
  }
  
  if(timeframe == "1hour"){
    hour.t = hours(chron(times=utcTime))
    
    end.of.candle = chron(times=paste0(hour.t,":59:59"))
    
    remainingTime = end.of.candle - utcTime
    return(remainingTime)
  }
  
  if(timeframe == "2hour"){
    hour.t = hours(chron(times=utcTime))
    
    two.hrs = seq(from = 0, to = 24, by = 2)
    
    if(any(hour.t %in% two.hrs)){
      end.of.candle = chron(times=paste0(as.numeric(hour.t)+1,":59:59"))
    }else{
      end.of.candle = chron(times=paste0(hour.t,":59:59"))
    }
    remainingTime = end.of.candle - utcTime
    
    return(remainingTime)
  }
  
  if(timeframe == '4hour'){
    if(utcTime >= chron(times="20:00:00")){
      remainingTime = chron(times="23:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="16:00:00")){
      remainingTime = chron(times="19:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="12:00:00")){
      remainingTime = chron(times="15:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="08:00:00")){
      remainingTime = chron(times="11:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="04:00:00")){
      remainingTime = chron(times="7:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="00:00:00")){
      remainingTime = chron(times="3:59:59") - utcTime
      return(remainingTime)
    }
  }
  if(timeframe == '8hour'){
    if(utcTime >= chron(times="16:00:00")){
      remainingTime = chron(times="23:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="08:00:00")){
      remainingTime = chron(times="15:59:59") - utcTime
      return(remainingTime)
    }
    if(utcTime >= chron(times="00:00:00")){
      remainingTime = chron(times="7:59:59") - utcTime
      return(remainingTime)
    }
  }
  if(timeframe == '1day' | timeframe == "daily"){
    remainingTime = chron(times="23:59:59") - utcTime
    return(remainingTime)
    
  }
  
  
}

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################


# CALC TIME TO NEXT CANDLE CLOSE
# getTimeRemaining = function(timeframe){
#   
#   utcTime = lubridate::now(tzone = 'UTC')
#   utcTime = format(utcTime, format = "%H:%M:%S")
#   if(timeframe == '4hour'){
#     if(utcTime >= chron(times="20:00:00")){
#       remainingTime = chron(times="23:59:59") - utcTime
#       return(remainingTime)
#     }
#     if(utcTime >= chron(times="16:00:00")){
#       remainingTime = chron(times="19:59:59") - utcTime
#       return(remainingTime)
#     }
#     if(utcTime >= chron(times="12:00:00")){
#       remainingTime = chron(times="15:59:59") - utcTime
#       return(remainingTime)
#     }
#     if(utcTime >= chron(times="08:00:00")){
#       remainingTime = chron(times="11:59:59") - utcTime
#       return(remainingTime)
#     }
#     if(utcTime >= chron(times="04:00:00")){
#       remainingTime = chron(times="7:59:59") - utcTime
#       return(remainingTime)
#     }
#     if(utcTime >= chron(times="00:00:00")){
#       remainingTime = chron(times="3:59:59") - utcTime
#       return(remainingTime)
#     }
#   }
#   if(timeframe == '8hour'){
#     if(utcTime >= chron(times="16:00:00")){
#       remainingTime = chron(times="23:59:59") - utcTime
#       return(remainingTime)
#     }
#     if(utcTime >= chron(times="08:00:00")){
#       remainingTime = chron(times="15:59:59") - utcTime
#       return(remainingTime)
#     }
#     if(utcTime >= chron(times="00:00:00")){
#       remainingTime = chron(times="7:59:59") - utcTime
#       return(remainingTime)
#     }
#   }
#   if(timeframe == '1day'){
#     remainingTime = chron(times="23:59:59") - utcTime
#     return(remainingTime)
#     
#   }
#   
# }


#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################


createModel <- function(Type,TargetIncreasePercent, SuccessThreshold, Symbol, Timeframe, TP=0){
  
  # Symbol = 'AUDUSD'
  # Type = "Forex"
  # Timeframe = '4hour'
  # TargetIncreasePercent = "0.6"
  # SuccessThreshold = '0.5'
  # df = readRDS(paste0("bsts/df_",'ETHUSD','4hour',".rds"))
  # sample.split = readRDS(paste0("bsts/sample.split_",'ETHUSD','4hour',"1",".rds"))
  # outcome = readRDS(paste0("bsts/outcome_",'ETHUSD','4hour',"1",".rds"))
  # test = readRDS(paste0("bsts/test_",'ETHUSD','4hour',"1",".rds"))
  # train = readRDS(paste0("bsts/train_",'ETHUSD','4hour',"1",".rds"))
  
  if(Type != "Forex"){
    df = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("df_",Symbol,"_",Timeframe,".rds"))
    sample.split = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("sample.split_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    outcome = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("outcome_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    test = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("test_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    
    outcome.train = outcome[sample.split]
    outcome.test = outcome[!sample.split]
    
    bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    
    
    # Predict
    predictions = predict(bst, test)
    Actual.Percent.High = round((((df$High / df$Open) * 100) - 100), digits = 1)
    Actual.Percent.Close = round((((df$Close / df$Open) * 100) - 100), digits = 1)
    Actual.Percent.Low = round((((df$Low / df$Open) * 100) - 100), digits = 1)
    compare2 = data.frame("Actual" = outcome.test,
                          "Actual.Percent.High" = Actual.Percent.High[which(!sample.split) + 1],
                          "Actual.Percent.Low" = Actual.Percent.Low[which(!sample.split) + 1],
                          "Actual.Percent.Close" = Actual.Percent.Close[which(!sample.split) + 1],
                          "Confidence.Score" = round(predictions, digits = 4),
                          "Signal" = NA)
    
    compare2$Signal[compare2$Confidence.Score >= SuccessThreshold] = 1
    compare2$Signal[compare2$Confidence.Score < SuccessThreshold] = 0
    
    
    compare2$profit = NA
    compare2$profit[compare2$Actual.Percent.High >= TargetIncreasePercent | compare2$Actual.Percent.Close > 0] = 1
    compare2$profit[compare2$Actual.Percent.High < TargetIncreasePercent & compare2$Actual.Percent.Close < 0] = 0
    
    compare2 = na.omit(compare2)
    
    assign("compare2",compare2,.GlobalEnv)
    
    df = data.frame(outcome.test, predictions)
    
    colnames(df) = c("outcome.test","pred")
    
    df$decision = 0
    df$decision[df$pred >= SuccessThreshold] = 1
    assign('compare',df,.GlobalEnv)
    
    true.pos = length(which(df$outcome.test == 1 & df$decision == 1))
    false.pos = length(which(df$outcome.test == 0 & df$decision == 1))
    false.neg = length(which(df$outcome.test == 1 & df$decision == 0))
    
    
    precision = true.pos / (true.pos + false.pos) * 100
    recall = true.pos / (true.pos + false.neg) * 100
    f1 = 2*((precision * recall)/(precision + recall)) / 100
    
    precision = round(precision, digits = 4)
    recall = round(recall, digits = 4)
    f1 = round(f1, digits = 4)
    
    assign("precision",precision,.GlobalEnv)
    assign("recall",recall,.GlobalEnv)
    assign("f1",f1,.GlobalEnv)
  }else{
    df = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("df_",Symbol,"_",Timeframe,".rds"))
    sample.split = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("sample.split_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    outcome = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("outcome_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    test = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("test_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    
    outcome.train = outcome[sample.split]
    outcome.test = outcome[!sample.split]
    
    bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    
    
    # Predict
    predictions = predict(bst, test)
    Actual.Percent.High = round((((df$High / df$Open) * 100) - 100), digits = 1)
    Actual.Percent.Close = round((((df$Close / df$Open) * 100) - 100), digits = 1)
    Actual.Percent.Low = round((((df$Low / df$Open) * 100) - 100), digits = 1)
    compare2 = data.frame("Actual" = outcome.test,
                          "Actual.Percent.High" = Actual.Percent.High[which(!sample.split) + 1],
                          "Actual.Percent.Low" = Actual.Percent.Low[which(!sample.split) + 1],
                          "Actual.Percent.Close" = Actual.Percent.Close[which(!sample.split) + 1],
                          "Confidence.Score" = round(predictions, digits = 4),
                          "Signal" = NA)
    
    compare2$Signal[compare2$Confidence.Score >= SuccessThreshold] = 1
    compare2$Signal[compare2$Confidence.Score < SuccessThreshold] = 0
    
    
    compare2$profit = NA
    compare2$profit[compare2$Actual.Percent.High >= TargetIncreasePercent | compare2$Actual.Percent.Close > 0] = 1
    compare2$profit[compare2$Actual.Percent.High < TargetIncreasePercent & compare2$Actual.Percent.Close < 0] = 0
    
    compare2 = na.omit(compare2)
    
    assign("compare2",compare2,.GlobalEnv)
    compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
    # compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_","AUDUSD","_","8hour","0.05",".rds"))
    # SuccessThreshold = 0.5
    
    df = compare
    
    colnames(df) = c("outcome.test","pred")
    
    
    df$decision = 0
    df$decision[df$pred >= SuccessThreshold] = 1
    assign('compare',df,.GlobalEnv)
    
    true.pos = length(which(df$outcome.test == 1 & df$decision == 1))
    false.pos = length(which(df$outcome.test == 0 & df$decision == 1))
    false.neg = length(which(df$outcome.test == 1 & df$decision == 0))
    
    
    precision = true.pos / (true.pos + false.pos) * 100
    recall = true.pos / (true.pos + false.neg) * 100
    f1 = 2*((precision * recall)/(precision + recall)) / 100
    
    precision = round(precision, digits = 4)
    recall = round(recall, digits = 4)
    f1 = round(f1, digits = 4)
    
    assign("precision",precision,.GlobalEnv)
    assign("recall",recall,.GlobalEnv)
    assign("f1",f1,.GlobalEnv)
    
  }
  
  # train = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/bsts_T/bsts", object = paste0("train_",Symbol,Timeframe,TargetIncreasePercent,".rds"))
  
  
  # df = readRDS(paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/df_",Symbol,"_",Timeframe,".rds"))
  # sample.split = readRDS(paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/sample.split_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
  # outcome = readRDS(paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/outcome_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
  # test = readRDS(paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/test_",Symbol,"_",Timeframe,TargetIncreasePercent,".rds"))
  # train = readRDS(paste0("bsts/train_",Symbol,Timeframe,TargetIncreasePercent,".rds"))
  
  
  
  
  # assign('train',train,.GlobalEnv)
  # 
  # 
  # 
  # # df$DBreakLow = NA
  # # df$BreakHigh = NA
  # # 
  # # for(i in 2:(nrow(df)-1)){
  # #   if(df$Low[i] <= df$Low[i-1]){
  # #     df$DBreakLow[i+1] = 0
  # #   }else{
  # #     df$DBreakLow[i+1] = 1
  # #   }
  # # 
  # #   if(df$High[i] >= df$High[i-1]){
  # #     df$BreakHigh[i+1] = 1
  # #   }else{
  # #     df$BreakHigh[i+1] = 0
  # #   }
  # # }
  # # 
  # # DBreakLow.test = df$DBreakLow[which(!sample.split)]
  # # BreakHigh.test = df$BreakHigh[which(!sample.split)]
  # 
  # 
  # compare$Signal[compare$Confidence.Score >= SuccessThreshold] = 1
  # compare$Signal[compare$Confidence.Score < SuccessThreshold] = 0
  # 
  # compare$profit = NA
  # compare$profit[compare$Actual.Percent.High >= TargetIncreasePercent | compare$Actual.Percent.Close > 0] = 1
  # compare$profit[compare$Actual.Percent.High < TargetIncreasePercent & compare$Actual.Percent.Close < 0] = 0
  # 
  # 
  # 
  # compare = na.omit(compare)
  # 
  # 
  # accuracy = length(which(compare$Actual == compare$Signal)) / nrow(compare) * 100
  # print(accuracy)
  # 
  # 
  # if(TP == 0){
  #   examine = compare[compare$Signal == 1, ]
  #   accuracy2 = sum(as.numeric(as.character(examine$Actual.Percent.Close)))
  #   print(accuracy2)
  # }else{
  #   
  #   examine = compare[compare$Signal == 1, ]
  #   winning.trades = examine[examine$Actual == 1,]
  #   winning.trades$Actual.Percent.High[winning.trades$Actual.Percent.High > TP ] = TP
  #   winning.trades.above = winning.trades[winning.trades$Actual.Percent.High == TP,]
  #   winning.trades.below = winning.trades[winning.trades$Actual.Percent.High < TP,]
  #   winning.sum.below = sum(as.numeric(as.character(winning.trades.below$Actual.Percent.Close)))
  #   winning.sum.above = sum(as.numeric(as.character(winning.trades.above$Actual.Percent.High)))
  #   winning.sum = winning.sum.above + winning.sum.below
  #   # missed.trades = examine[examine$Actual == 0,]
  #   # missed.trades$Actual.Percent.Close[missed.trades$Actual.Percent.Close < SL] = SL
  #   # missed.sum = sum(as.numeric(as.character(missed.trades$Actual.Percent.Close)))
  #   accuracy2 = winning.sum
  #   # accuracy2 = sum(as.numeric(as.character(examine$Actual.Percent.Close)))
  #   print(accuracy2)
  # }
  # 
  # 
  # yes.buy = compare[compare$Signal == 1, ]
  # yes.buy.above.zero = length(which(yes.buy$Actual == 0 & yes.buy$Actual.Percent.Close > 0))
  # yes.buy.correct.perc = (length(which(yes.buy$Signal == yes.buy$Actual)) + yes.buy.above.zero)  / nrow(yes.buy) * 100
  # 
  # no.buy = compare[compare$Signal == 0, ]
  # no.buy.correct.perc = length(which(no.buy$Signal == no.buy$Actual)) / nrow(no.buy) * 100
  # 
  # 
  # assign('yes.buy.correct.perc',yes.buy.correct.perc,.GlobalEnv)
  # assign("no.buy.correct.perc",no.buy.correct.perc,.GlobalEnv)
  # # assign("overall.accuracy",accuracy,.GlobalEnv)
  # assign("compare",compare,.GlobalEnv)
  # assign("sum.percentage",accuracy2,.GlobalEnv)
  # assign('bst',bst,.GlobalEnv)
}


#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################

# dfTEST = riingo_crypto_prices('cultusd', end_date = Sys.Date(), resample_frequency = '4hour')
# df2 = riingo_crypto_latest('strongusdt', resample_frequency = '4hour')



predict.tomorrow.multiple <- function(Type,Symbols, Timeframe, SuccessThreshold){
  # # Symbols = Symbols
  # Symbols = c('AAPL')
  # Timeframe = 'weekly'
  # i = 1
  # SuccessThreshold = 0.9
  # Type="Stocks"
  assign("predictions.df.indi1", NULL, .GlobalEnv)
  assign("predictions.df.indi2", NULL, .GlobalEnv)
  assign("predictions.df.indi3", NULL, .GlobalEnv)
  assign("predictions.df.indi4", NULL, .GlobalEnv)
  
  
  print(Type)
  predictions.df.comb = data.frame("Coin" = character(),
                                   "Price Change" = character(),
                                   "C.Score.HIT.TARGET" = character(),
                                   "C.Score.MISS.TARGET" = character(),
                                   "C.Score.BreakPrevoiusHigh" = character(),
                                   "Previous.High" = character(),
                                   "C.Score.BreakPrevoiusLow" = character(),
                                   "Previous.High" = character(),
                                   "Signal" = character())
  
  
  for(i in 1:length(Symbols)){
    
    if(Type == "Stocks"){
      df = riingo_prices(Symbols[i],Sys.Date() - 300, end_date = Sys.Date(), resample_frequency = Timeframe)
    }else{
      if(Timeframe == '4hour' | Timeframe == '8hour'| Timeframe == '1hour'| Timeframe == '15min'){
        df1 = riingo_crypto_prices(Symbols[i], start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = Timeframe)
        df1 = df1[-nrow(df1),]
        df2 = riingo_crypto_latest(Symbols[i], resample_frequency = Timeframe)
        df = rbind(df1,df2)
      }else{
        df = riingo_crypto_prices(Symbols[i],Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = Timeframe)
      }
      df = df[,4:9]
    }
    
    # Modify data to be more useable
    df = df %>%
      select("date","open","high","low","close","volume")
    
    df$Percent.Change = NA
    
    colnames(df) = c("Date","Open","High","Low","Close","Volume","Percent.Change")
    df$Percent.Change = round((((df$High / df$Open) * 100) - 100), digits = 1)
    
    # df$DBreakL = NA
    # df$BreakH = NA
    # 
    # for(k in 2:(nrow(df)-1)){
    #   if(df$Low[k] <= df$Low[k-1]){
    #     df$DBreakL[k+1] = 0
    #   }else{
    #     df$DBreakL[k+1] = 1
    #   }
    #   
    #   if(df$High[k] >= df$High[k-1]){
    #     df$BreakH[k+1] = 1
    #   }else{
    #     df$BreakH[k+1] = 0
    #   }
    # }
    
    #Add column for binary previouos day change+
    df$Previous = NA
    for(k in 2:nrow(df)){
      if(df$Percent.Change[k - 1] <= 0){
        df$Previous[k] = 0
      }else{
        df$Previous[k] = 1
      }
    }
    
    # Remove first row since we can't use it
    df = df[-1,]
    
    df_candle_plot = tail(df,30) %>%
      plot_ly(x = ~Date, type="candlestick",
              open = ~Open, close = ~Close,
              high = ~High, low = ~Low)
    df_candle_plot = df_candle_plot %>% layout(title = paste0('Last 30 candles for ',toupper(Symbols[i])),
                                               xaxis = list(rangeslider = list(visible = F)))
    
    assign(paste0('df_candleplot_',Symbols[i]),df_candle_plot,.GlobalEnv)
    
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
    if(!grepl(pattern ="day|daily|weekly|week",Timeframe)){
      df$Date = str_replace(string = df$Date, pattern = "T", replacement = " ")
      df$Date = str_replace(string = df$Date, pattern = "Z", replacement = "")
      
      df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
    }else{
      df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d")
    }
    
    df = df[!is.na(df$Date),]
    
    df = as.xts(df)
    
    
    
    # Add candelstick patterns
    # candle.list = list(CSPDarkCloudCover(df),CSPDoji(df),CSPEngulfing(df),CSPGap(df),CSPHammer(df),CSPHarami(df),
    #                    CSPInsideDay(df),CSPInvertedHammer(df),CSPKicking(df),CSPLongCandle(df),CSPMarubozu(df),
    #                    CSPNLongWhiteCandles(df),CSPPiercingPattern(df),CSPStar(df),
    #                    CSPStomach(df),CSPTasukiGap(df),CSPThreeBlackCrows(df),CSPThreeInside(df),CSPThreeLineStrike(df),
    #                    CSPThreeMethods(df),CSPThreeOutside(df),CSPThreeWhiteSoldiers(df))
    candle.list = list(hammer(df), inverted.hammer(df), bearish.engulf(df), bullish.engulf(df), up.trend(df), down.trend(df))
    # trend = candlesticks::TrendDetectionSMA(df)
    
    # Remove unusable rows
    
    
    for(k in 1:length(candle.list)){
      df = cbind(df, candle.list[[k]])
    }
    # df = cbind(df, trend$Trend)
    
    
    
    # Add lagged values
    for(k in 1:5){
      high.lag = Lag(df$High, k)
      close.lag = Lag(df$Close, k)
      percent.change.lag = ((high.lag/close.lag) - 1) * 100
      df = cbind(df, percent.change.lag)
      
    }
    
    df = df[-(1:20),]
    
    if(nrow(df) > 5){
      df = df[-c(1:5),]
    }
    
    df[is.na(df)] = 0
    # Round columns to be more general
    # df$Close = round(df$Close, digits = 3)
    # df$Open = round(df$Open, digits = 3)
    # df$High = round(df$High, digits = 3)
    # df$Low = round(df$Low, digits = 3)
    
    
    
    # grab second to last entry since that is the most recent closed candle
    if(nrow(df) > 1){
      df = df[nrow(df)-1,]
    }
    
    
    
    # grab the high/low from the last candle
    prev.high = as.data.frame(df)$High[1]
    prev.low = as.data.frame(df)$Low[1]
    prev.close = as.data.frame(df)$Close[1]
    
    break.high.perc = round(((prev.high / prev.close) - 1) * 100, digits = 3)
    break.low.perc = round(((prev.low / prev.close) - 1) * 100, digits = 3)
    
    df = df[,-c(1:4)]
    
    
    
    if(Timeframe == "1hour" | Timeframe == "15min"){
      predictions.df.pos = data.frame("Coin" = rep(toupper(Symbols[i]),5),
                                      "Price Change" = seq(from = 0.2, to = 1, by = 0.2),
                                      "C.Score.HIT.TARGET" = rep(NA,5),
                                      "C.Score.MISS.TARGET" = rep(NA,5),
                                      "C.Score.BreakPrevoiusHigh" = rep(NA,5),
                                      "Previous.High" = rep(NA,5),
                                      "C.Score.BreakPrevoiusLow" = rep(NA,5),
                                      "Previous.Low" = rep(NA,5),
                                      "Signal" = rep("DON'T BUY SIGNAL",5))
      predictions.df.neg = data.frame("Coin" = rep(toupper(Symbols[i]),5),
                                      "Price Change" = seq(from = -1, to = -0.2, by = 0.2),
                                      "C.Score.HIT.TARGET" = rep(NA,5),
                                      "C.Score.MISS.TARGET" = rep(NA,5),
                                      "C.Score.BreakPrevoiusHigh" = rep(NA,5),
                                      "Previous.High" = rep(NA,5),
                                      "C.Score.BreakPrevoiusLow" = rep(NA,5),
                                      "Previous.Low" = rep(NA,5),
                                      "Signal" = rep("DON'T BUY SIGNAL",5))
      from_ = 0.2
      to_ = 1
      by_ = 0.2
    }else{
      predictions.df.pos = data.frame("Coin" = rep(toupper(Symbols[i]),15),
                                      "Price Change" = seq(from = 0.2, to = 3, by = 0.2),
                                      "C.Score.HIT.TARGET" = rep(NA,15),
                                      "C.Score.MISS.TARGET" = rep(NA,15),
                                      "C.Score.BreakPrevoiusHigh" = rep(NA,15),
                                      "Previous.High" = rep(NA,15),
                                      "C.Score.BreakPrevoiusLow" = rep(NA,15),
                                      "Previous.Low" = rep(NA,15),
                                      "Signal" = rep("DON'T BUY SIGNAL",15))
      predictions.df.neg = data.frame("Coin" = rep(toupper(Symbols[i]),15),
                                      "Price Change" = seq(from = -3, to = -0.2, by = 0.2),
                                      "C.Score.HIT.TARGET" = rep(NA,15),
                                      "C.Score.MISS.TARGET" = rep(NA,15),
                                      "C.Score.BreakPrevoiusHigh" = rep(NA,15),
                                      "Previous.High" = rep(NA,15),
                                      "C.Score.BreakPrevoiusLow" = rep(NA,15),
                                      "Previous.Low" = rep(NA,15),
                                      "Signal" = rep("DON'T BUY SIGNAL",15))
      from_ = 0.2
      to_ = 3
      by_ = 0.2
    }
    
    predictions.pos = c()
    predictions.neg = c()
    for(j in seq(from = from_, to = to_, by=by_)){
      
      bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",Symbols[i],"_",Timeframe,j,".rds"))
      # bst = readRDS(paste0("../bsts-7-18-2023/bst_",Symbols[i],"_",Timeframe,j,".rds"))
      
      
      # bst = readRDS(paste0("bsts/bst_",Symbols[i],Timeframe,j,".rds"))
      
      # convert to matrix for predictions
      df = as.matrix(df)
      predict.next = predict(bst, df)
      
      
      
      predictions.pos = c(predictions.pos,predict.next)
    }
    for(j in seq(from = (to_ * -1), to = (from_ * -1), by=by_)){
      
      bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",Symbols[i],"_",Timeframe,j,".rds"))
      # bst = readRDS(paste0("../bsts-7-18-2023/bst_",Symbols[i],"_",Timeframe,j,".rds"))
      
      
      # bst = readRDS(paste0("bsts/bst_",Symbols[i],Timeframe,j,".rds"))
      
      # convert to matrix for predictions
      df = as.matrix(df)
      predict.next = predict(bst, df)
      
      
      
      predictions.neg = c(predictions.neg,predict.next)
    }
    
    ###############################
    ############################### READ IN DATASET
    if(Type == "Stocks"){
      df = riingo_prices(Symbols[i],Sys.Date() - 300, end_date = Sys.Date(), resample_frequency = Timeframe)
    }else{
      if(Timeframe == '4hour' | Timeframe == '8hour'| Timeframe == '1hour'| Timeframe == '15min'){
        df1 = riingo_crypto_prices(Symbols[i], start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = Timeframe)
        df1 = df1[-nrow(df1),]
        df2 = riingo_crypto_latest(Symbols[i], resample_frequency = Timeframe)
        df = rbind(df1,df2)
      }else{
        df = riingo_crypto_prices(Symbols[i],Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = Timeframe)
      }
      df = df[,4:9]
    }
    
    ###############################
    ############################### JUST FILTERING OUT UNECESSARY COLUMNS
    df = df %>%
      select("date","open","high","low","close","volume")
    ###############################
    ############################### CHANGE NAMES
    colnames(df) = c("Date","Open","High","Low","Close","Volume")
    
    
    ###############################
    ############################### ADD IN MOVING AVERAGES
    df$MA10 = NA
    df$MA20 = NA
    df$VMA20 = NA
    
    for(k in 21:nrow(df)){
      df$MA10[k] = mean(df$Close[k-10:k])
      df$MA20[k] = mean(df$Close[k-20:k])
      df$VMA20[k]= mean(df$Volume[k-20:k])
    }
    
    ###############################
    ############################### ADD IN CHECKS FOR CLOSING VALUES
    C1 = rep(0, nrow(df))
    C2 = rep(0, nrow(df))
    C3 = rep(0, nrow(df))
    
    for(k in 4:nrow(df)){
      if(df$Close[k] > df$Close[k-1]){
        C1[k] = 1
      }
      if(df$Close[k-1] > df$Close[k-2]){
        C2[k] = 1
      }
      if(df$Close[k-2] > df$Close[k-3]){
        C3[k] = 1
      }
    }
    
    df$P3C = C1 + C2 + C3
    
    
    ###############################
    ############################### DEFINE OTHER INPUT VALUES
    df$OH = (df$High - df$Open)/df$Open * 100
    df$CH = (df$Close - df$Open)/ df$Open * 100
    df$LH = (df$High - df$Low) / df$Low * 100
    df$LC = (df$Close - df$Low) / df$Low * 100
    
    df$HMA = (df$High - df$MA20)/ df$MA20 * 100
    df$LMA = (df$Low - df$MA20)/ df$MA20 * 100
    df$CMA = (df$Close - df$MA20)/ df$MA20 * 100
    df$VMA = (df$Volume - df$VMA20) / df$VMA20 * 100
    
    lag1Vol = Lag(df$Volume, 1)
    df$VolumeD = (df$Volume - lag1Vol)/lag1Vol * 100
    
    ###############################
    ############################### DETERMINE OUTCOME VALUES
    BreakL = NA
    BreakH = NA
    
    for(k in 2:(nrow(df))){
      if(df$Low[k] <= df$Low[k-1]){
        BreakL[k] = 1
      }else{
        BreakL[k] = 0
      }
      
      if(df$High[k] >= df$High[k-1]){
        BreakH[k] = 1
      }else{
        BreakH[k] = 0
      }
    }
    
    BreakH = c(BreakH, NA)
    BreakH = BreakH[-1]
    
    BreakL = c(BreakL, NA)
    BreakL = BreakL[-1]
    ###############################
    ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
    df = df[-c(1:20,nrow(df)),-c(1:5)]
    BreakL = BreakL[-c(1:20,length(BreakL))]
    BreakH = BreakH[-c(1:20,length(BreakH))]
    
    
    ###############################
    ############################### ROUND ALL INPUTS TO 2 DIGITS
    df = round(df, 2)
    
    ###############################
    ############################### SELECT ONLY CERTAIN INPUTS FOR BUILDING THE MODEL
    df = df %>%
      select("LH","LC","VolumeD","VMA","LMA","HMA","P3C")
    
    ###############################
    ############################### GRAB LAST FULL CLOSED CANDLE
    df = df[nrow(df)-1,]
    
    bstBH = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",Symbols[i],"_",Timeframe,"BreakH.rds"))
    bstBL = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/TiingoBoosts", object = paste0("bst_",Symbols[i],"_",Timeframe,"BreakL.rds"))
    
    # bstBH = readRDS(paste0("../bsts-7-18-2023/bst_",Symbols[i],"_",Timeframe,"BreakH.rds"))
    # bstBL = readRDS(paste0("../bsts-7-18-2023/bst_",Symbols[i],"_",Timeframe,"BreakL.rds"))
    
    df = as.matrix(df)
    
    predict.BH = predict(bstBH, df)
    predict.BL = predict(bstBL, df)
    
    ############ POSITIVE DATASET
    predictions.df.pos$C.Score.BreakPrevoiusHigh = round(predict.BH, digits = 3)
    predictions.df.pos$C.Score.BreakPrevoiusLow = round(predict.BL, digits = 3)
    
    predictions.df.pos$C.Score.HIT.TARGET = predictions.pos
    
    buyCond1 = predictions.df.pos$C.Score.HIT.TARGET >= SuccessThreshold
    # buyCond2 = predictions.df.pos$C.Score.BreakPrevoiusLow <= 0.4
    # buyCond3 = predictions.df.pos$C.Score.BreakPrevoiusLow > 0.4 & predictions.df.pos$break.low.perc < 0.33 
    
    predictions.df.pos$Previous.High = paste0(break.high.perc,"%")
    predictions.df.pos$Previous.Low = paste0(break.low.perc, "%")
    
    
    
    
    predictions.df.pos$Price.Change = paste0(predictions.df.pos$Price.Change,"% or more")
    predictions.df.pos$Signal[buyCond1] = "BUY SIGNAL"
    predictions.df.pos[nrow(predictions.df.pos)+1,] <- NA
    ############ 
    
    ############ NEGATIVE DATASET
    predictions.df.neg$C.Score.BreakPrevoiusHigh = round(predict.BH, digits = 3)
    predictions.df.neg$C.Score.BreakPrevoiusLow = round(predict.BL, digits = 3)
    predictions.df.neg$Previous.High = paste0(break.high.perc,"%")
    predictions.df.neg$Previous.Low = paste0(break.low.perc, "%")
    
    
    predictions.df.neg$Price.Change = paste0(predictions.df.neg$Price.Change,"% or more")
    predictions.df.neg$C.Score.HIT.TARGET = predictions.neg
    predictions.df.neg$Signal[predictions.df.neg$C.Score.HIT.TARGET >= SuccessThreshold] = "BUY SIGNAL"
    ############ 
    
    
    
    predictions.df.comb = rbind(predictions.df.comb, predictions.df.neg, predictions.df.pos)
    predictions.df.comb$C.Score.MISS.TARGET = 1 - predictions.df.comb$C.Score.HIT.TARGET
    predictions.df.comb$C.Score.HIT.TARGET = round(predictions.df.comb$C.Score.HIT.TARGET, 3)
    predictions.df.comb$C.Score.MISS.TARGET = round(predictions.df.comb$C.Score.MISS.TARGET, 3)
    
    #INDIVIDUAL
    predictions.df.indi = data.frame(rbind(predictions.df.neg,predictions.df.pos))
    predictions.df.indi = predictions.df.indi[,-4]
    colnames(predictions.df.indi) = c("Coin","Price.Change","CS HT","CS BH", "Prev.High","CS BL", "Prev.Low","Signal")
    predictions.df.indi$`CS HT` = round(predictions.df.indi$`CS HT`, 3)
    assign(paste0("predictions.df.indi",i), predictions.df.indi, .GlobalEnv)
    
    
  }
  predictions.df.comb = predictions.df.comb[,-4]
  colnames(predictions.df.comb) = c("Coin","Price.Change","CS HT","CS BH", "Prev.High","CS BL", "Prev.Low","Signal")
  assign("predictions.df.comb",predictions.df.comb,.GlobalEnv)
  
  
  
  
  
}

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################





predict_week = function(symbol, timeframe,type){
  
  symbol = toupper(symbol)
  # symbol = 'ETHUSDT'
  # timeframe = 'daily'
  # type = "Crypto"
  data = data.frame(getSymbols.tiingo(Symbols = symbol, auto.assign = FALSE,api.key = '6fbd6ce7c9e035489f6238bfab127fcedbe34ac2', periodicity = timeframe))
  # data = data.frame(getSymbols(symbol, auto.assign = FALSE, periodicity = timeframe))
  data = data[,1:4]
  data = na.omit(data)
  # data = round(data, digits = 0)
  
  colnames(data) = c('open','high','low','close')
  
  data$time = row.names(data)
  
  data$time = gsub(pattern = "X", replacement = "", x = data$time)
  data$time = gsub(pattern = "\\.", replacement = "-", x = data$time)
  
  if(timeframe == 'daily'){
    if(type == "Stocks" | type == "Forex"){
      data.add = data.frame(time = seq(from = as_date(Sys.Date()),
                                       by = "day", length.out = 9),
                            open = NA,
                            high = NA,
                            low = NA,
                            close = NA)
    }else{
      data.add = data.frame(time = seq(from = as_date(Sys.Date() + 1),
                                       by = "day", length.out = 7),
                            open = NA,
                            high = NA,
                            low = NA,
                            close = NA)
    }
    
  }else{
    data.add = data.frame(time = seq(from = as_date(Sys.Date()),
                                     by = "week", length.out = 7),
                          open = NA,
                          high = NA,
                          low = NA,
                          close = NA)
  }
  
  data.add$time = as.character(data.add$time)
  data = rbind(data, data.add)
  
  data.xts = data
  
  data.xts$time = as.POSIXct(data.xts$time, format = "%Y-%m-%d")
  
  
  data.xts = as.xts(data.xts)
  
  # Add lagged values
  for(k in 7:21){
    lagging = Lag(data$close, k)
    # lagging = LagOHLC(data.xts, 7)
    # ind = which(names(lagging) == paste0("close.Lag.",7))
    data = cbind(data, lagging)
    
  }
  
  data= data[-c(1:21),]
  
  
  data$month = lubridate::month(data$time)
  data$day = lubridate::day(data$time)
  
  
  
  data_selected = data[,-c(1:5)]
  
  
  # SPLIT INTO TRAIN AND TEST
  train <- data_selected[1:(nrow(data)-7), ]
  
  if(type == "Stocks" | type == "Forex"){
    pred <- data_selected[((nrow(data) - 9 + 1)):(nrow(data)-2), ]
  }else{
    pred <- data_selected[((nrow(data) - 7 + 1)):nrow(data), ]
  }
  
  
  
  x_train = as.matrix(train)
  x_pred = as.matrix(pred)
  
  y_train <- data[1:(nrow(data)-7), 4]
  
  # symbol = 'BTC-USD'
  # timeframe = 'daily'
  # bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/bsts_T/bsts_T", object = paste0("bst_T_",'btcusdt','daily',".rds"))
  
  bst = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/bsts_T/bsts_T", object = paste0("bst_T_",symbol,timeframe,".rds"))
  if(bst[1] == 'ERROR'){
    bst = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/bsts_T/bsts_T", object = paste0("bst_T_",tolower(symbol),timeframe,".rds"))
  }
  # bst = readRDS(paste0('bsts/bst_',symbol,Timeframe,TargetIncreasePercent,".rds"))
  
  # xgb_model$bestTune
  
  
  xgb_pred <- predict(bst, x_pred)
  # saveRDS(bst, file = paste0("bsts_T/bst_",lfiles.names[i],".rds"))
  
  data_y = data[((nrow(data) - 30 + 1)):(nrow(data) - 7), 4]
  add.na = rep(NA, 7)
  
  predicted_y = rep(NA, 23)
  predicted_y[23] = data_y[23]
  
  predicted_y = c(predicted_y, xgb_pred)
  data_y = c(data_y, add.na)
  times = data$time[(nrow(data)-29):nrow(data)]
  x = data.frame(cbind(data_y, predicted_y))
  # x = round(x, digits = 0)
  x = cbind(x, times)
  x$times = as.Date(x$times)
  
  pad.x = pad(x)
  
  for(i in 1:(nrow(pad.x)-2)){
    pad.x$predicted_y[i] = pad.x$predicted_y[i + 2]
  }
  
  for(i in 2:nrow(pad.x)){
    if(!is.na(pad.x$data_y[i-1]) & is.na(pad.x$data_y[i])){
      pad.x$data_y[i] = pad.x$data_y[i-1]
    }
    if(!is.na(pad.x$predicted_y[i-1]) & is.na(pad.x$predicted_y[i])){
      pad.x$predicted_y[i] = pad.x$predicted_y[i-1]
    }
  }
  
  pad.x$predicted_y[c(nrow(pad.x),nrow(pad.x)-1)] = NA
  pad.x$data_y[c(nrow(pad.x),nrow(pad.x)-1)] = NA
  
  
  ind.rem = which(!is.na(pad.x$data_y) & !is.na(pad.x$predicted_y))[-1]
  pad.x$data_y[ind.rem] = NA
  
  saturdays = which(!is.na(pad.x$predicted_y) & grepl(pattern = "Saturday", x = weekdays(pad.x$times)))
  if(length(saturdays) > 0 ){
    vals = pad.x$predicted_y[saturdays:nrow(pad.x)]
    lag.vals = Lag(vals, 2)
    
    pad.x$predicted_y[saturdays:nrow(pad.x)] = lag.vals
  }
  
  sundays = which(!is.na(pad.x$predicted_y) & grepl(pattern = "Sunday", x = weekdays(pad.x$times)))
  if(length(sundays) > 0 ){
    vals = pad.x$predicted_y[sundays:nrow(pad.x)]
    lag.vals = Lag(vals, 1)
    
    pad.x$predicted_y[sundays:nrow(pad.x)] = lag.vals
  }
  
  if(type == "Stocks" | type == "Forex"){
    x = pad.x
    for(i in 2:nrow(x)){
      if(is.na(x$predicted_y[i])){
        x$predicted_y[i] = x$predicted_y[i-1]
      }
    }
  }
  
  x$predicted_y[!is.na(x$data_y) & !is.na(x$predicted_y)] = x$data_y[!is.na(x$data_y) & !is.na(x$predicted_y)] 
  
  for.scale = c(x$data_y,x$predicted_y)
  
  if(timeframe == 'daily'){
    plot.out = ggplot(data = x, aes(x = times)) + 
      geom_line(aes(y = data_y), color = "blue") +
      geom_line(aes(y = predicted_y), color = "red") +
      xlab("Date") +
      ylab("Price") +
      ggtitle(paste0("Predicted Stock Price for ",symbol)) +
      scale_x_date(date_breaks = "1 day", date_labels =  "%d %B") +
      scale_y_continuous(limits = c(min(for.scale, na.rm = TRUE),max(for.scale, na.rm = TRUE))) +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  }else{
    plot.out = ggplot(data = x, aes(x = times)) + 
      geom_line(aes(y = data_y), color = "blue") +
      geom_line(aes(y = predicted_y), color = "red") +
      xlab("Date") +
      ylab("Price") +
      ggtitle(paste0("Predicted Stock Price for ",symbol)) +
      scale_x_date(date_breaks = "1 week", date_labels =  "%d %B") +
      scale_y_continuous(limits = c(min(for.scale, na.rm = TRUE),max(for.scale, na.rm = TRUE))) +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  }
  colnames(x) = c("Past Close Price","Predicted Close Price","Date")
  assign("week.forecast.df",x,.GlobalEnv)
  print(x)
  
  
  return(plot.out)
}



#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################




build.TV.model <- function(df, timeframe){
  
  Symbols = toString(df$name)
  print(Symbols)
  # Symbols = 'BTCUSDT.csv'
  Symbols = str_match(string = Symbols, pattern = "(.*)\\.csv")[,2]
  
  df = read.csv(df$datapath)
  # df = read.csv('TVData/BTCUSDT.csv')
  # timeframe = '1day'
  # Symbols = 'ethusd'
  # targetPercentage = "1"
  
  
  
  # Remove uncecessary columns
  df = df[,1:5]
  
  # Modify data to be more useable
  df$Percent.Change = NA
  #df = df[-1,-c(1:3,10:11)]
  colnames(df) = c("Date","Open","High","Low","Close","Percent.Change")
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
  
  # Remove first row since we can't use it
  df = df[-1,]
  
  
  # Adding Moving Averages
  df$MA10 = NA
  # df$MA20 = NA
  
  for(k in 21:nrow(df)){
    df$MA10[k] = mean(df$Close[k-10:k])
    # df$MA20[k] = mean(df$Close[k-20:k])
  }
  # df$MA10 = round(df$MA10, digits = 2)
  # df$MA20 = round(df$MA20, digits = 2)
  
  # Add column for if MA10 is above or below MA20
  # df$MAAB = 0
  # 
  # df$MAAB[df$MA10 > df$MA20] = 1
  
  
  # Convert to actual dates and remove year and change to numeric
  df$Date = str_replace(string = df$Date, pattern = "T", replacement = " ")
  df$Date = str_replace(string = df$Date, pattern = "Z", replacement = "")
  
  df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
  
  df = as.xts(df)
  
  
  
  # Add candelstick patterns
  # candle.list = list(CSPDarkCloudCover(df),CSPDoji(df),CSPEngulfing(df),CSPGap(df),CSPHammer(df),CSPHarami(df),
  #                    CSPInsideDay(df),CSPInvertedHammer(df),CSPKicking(df),CSPLongCandle(df),CSPMarubozu(df),
  #                    CSPNLongWhiteCandles(df),CSPPiercingPattern(df),CSPStar(df),
  #                    CSPStomach(df),CSPTasukiGap(df),CSPThreeBlackCrows(df),CSPThreeInside(df),CSPThreeLineStrike(df),
  #                    CSPThreeMethods(df),CSPThreeOutside(df),CSPThreeWhiteSoldiers(df))
  candle.list = list(hammer(df), inverted.hammer(df), bearish.engulf(df), bullish.engulf(df), up.trend(df), down.trend(df))
  
  # candle.list = list(CSPHammer(df), CSPInvertedHammer(df),CSPEngulfing(df))
  # trend = candlesticks::TrendDetectionSMA(df)
  
  
  for(k in 1:length(candle.list)){
    df = cbind(df, candle.list[[k]])
  }
  
  # Remove unusable rows
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
  
  
  
  
  
  
  # Round columns to be more general
  # df$Close = round(df$Close, digits = 3)
  # df$Open = round(df$Open, digits = 3)
  # df$High = round(df$High, digits = 3)
  # df$Low = round(df$Low, digits = 3)
  
  
  
  outcome = rep(NA, nrow(df))
  
  outcome[df$Percent.Change >= 0.9] = 1
  outcome[df$Percent.Change < 0.9] = 0
  
  outcome = c(outcome, NA)
  outcome = outcome[-1]
  
  
  
  
  # Remove last row from df since we can't use it
  outcome = outcome[-(length(outcome))]
  df = df[-(nrow(df)),]
  
  df = data.frame(df, row.names = NULL)
  # df = df[,c(1:11,12:25)]
  
  ### Remove OPEN HIGH LOW CLOSE
  df = df[,-c(1:4)]
  
  # Split data into train and test
  set.seed(123)
  sample.split = sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob=c(0.8,0.2))
  
  
  # saveRDS(sample.split, file = paste0("bsts/sample.split_",file.names[i],j,".rds"))
  
  
  # Remvoe last sample int since I said so
  #sample.split = sample.split[-which(sample.split == nrow(df))]
  
  train = df[sample.split,]
  test = df[!sample.split,]
  
  train = as.matrix(train)
  test = as.matrix(test)
  
  # saveRDS(train, file = paste0("bsts/train_",file.names[i],j,".rds"))
  # saveRDS(test, file = paste0("bsts/test_",file.names[i],j,".rds"))
  
  outcome.train = outcome[sample.split]
  outcome.test = outcome[!sample.split]
  
  
  
  # Creat boosted model
  bst = xgboost(data = train,
                label = outcome.train,
                objective = "binary:logistic",
                max.depth = 20,
                nrounds = 200,
                eta = 0.3)
  
  # bst = readRDS('bsts/bst_ETHUSD1day1.rds')
  
  # saveRDS(bst, file = paste0("bsts/bst_",file.names[i],j,".rds"))
  # print(file.names[i])
  
  predictions = predict(bst, test)
  Actual.Percent.High = round((((df$High / df$Open) * 100) - 100), digits = 1)
  Actual.Percent.Close = round((((df$Close / df$Open) * 100) - 100), digits = 1)
  Actual.Percent.Low = round((((df$Low / df$Open) * 100) - 100), digits = 1)
  compare = data.frame("Actual" = outcome.test,
                       "Actual.Percent.High" = Actual.Percent.High[which(!sample.split) + 1],
                       "Actual.Percent.Low" = Actual.Percent.Low[which(!sample.split) + 1],
                       "Actual.Percent.Close" = Actual.Percent.Close[which(!sample.split) + 1],
                       "Confidence.Score" = round(predictions, digits = 4),
                       "Signal" = NA)
  
  compare$Signal[compare$Confidence.Score >= 0.9] = 1
  compare$Signal[compare$Confidence.Score< 0.9] = 0
  
  predictions.df.comb = data.frame("Coin" = character(),
                                   "Price Change" = character(),
                                   "Confidence.Score.HIT.TARGET" = character(),
                                   "Confidence.Score.MISS.TARGET" = character(),
                                   "Signal" = character())
  
  
  for(i in 1:1){
    
    if(timeframe == '4hour' | timeframe == '8hour'){
      df1 = riingo_crypto_prices(Symbols[i], end_date = Sys.Date(), resample_frequency = timeframe)
      df1 = df1[-nrow(df1),]
      df2 = riingo_crypto_latest(Symbols[i], resample_frequency = timeframe)
      df = rbind(df1,df2)
    }else{
      df = riingo_crypto_prices(Symbols[i], end_date = Sys.Date(), resample_frequency = timeframe)
    }
    # Modify data to be more useable
    df = df[,4:8]
    df$Percent.Change = NA
    
    colnames(df) = c("Date","Open","High","Low","Close","Percent.Change")
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
    
    # Remove first row since we can't use it
    df = df[-1,]
    
    
    # Adding Moving Averages
    df$MA10 = NA
    # df$MA20 = NA
    
    for(k in 21:nrow(df)){
      df$MA10[k] = mean(df$Close[k-10:k])
      # df$MA20[k] = mean(df$Close[k-20:k])
    }
    # df$MA10 = round(df$MA10, digits = 2)
    # df$MA20 = round(df$MA20, digits = 2)
    
    
    
    # Add column for if MA10 is above or below MA20
    # df$MAAB = 0
    # 
    # df$MAAB[df$MA10 > df$MA20] = 1
    
    
    df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
    
    df = as.xts(df)
    
    
    
    # Add candelstick patterns
    # candle.list = list(CSPDarkCloudCover(df),CSPDoji(df),CSPEngulfing(df),CSPGap(df),CSPHammer(df),CSPHarami(df),
    #                    CSPInsideDay(df),CSPInvertedHammer(df),CSPKicking(df),CSPLongCandle(df),CSPMarubozu(df),
    #                    CSPNLongWhiteCandles(df),CSPPiercingPattern(df),CSPStar(df),
    #                    CSPStomach(df),CSPTasukiGap(df),CSPThreeBlackCrows(df),CSPThreeInside(df),CSPThreeLineStrike(df),
    #                    CSPThreeMethods(df),CSPThreeOutside(df),CSPThreeWhiteSoldiers(df))
    candle.list = list(hammer(df), inverted.hammer(df), bearish.engulf(df), bullish.engulf(df), up.trend(df), down.trend(df))
    # trend = candlesticks::TrendDetectionSMA(df)
    
    # Remove unusable rows
    
    
    for(k in 1:length(candle.list)){
      df = cbind(df, candle.list[[k]])
    }
    # df = cbind(df, trend$Trend)
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
    # Round columns to be more general
    # df$Close = round(df$Close, digits = 3)
    # df$Open = round(df$Open, digits = 3)
    # df$High = round(df$High, digits = 3)
    # df$Low = round(df$Low, digits = 3)
    
    df = df[nrow(df)-1,]
    
    
    
    predictions.df.pos = data.frame("Coin" = rep(toupper(Symbols[i]),1),
                                    "Price Change" = 1,
                                    "Confidence.Score.HIT.TARGET" = rep(NA,1),
                                    "Confidence.Score.MISS.TARGET" = rep(NA,1),
                                    "Signal" = rep("DON'T BUY SIGNAL",1))
    
    predictions.pos = c()
    predictions.neg = c()
    for(j in 1:1){
      # bst = readRDS(paste0('bsts/bst_',toupper(Symbols[i]),timeframe,j,'.rds'))
      
      # bst = readRDS(paste0('bsts/bst_',toupper(Symbols[i]),timeframe,j,'.rds'))
      df = as.matrix(df)
      predict.next = predict(bst, df)
      predictions.pos = c(predictions.pos,predict.next)
    }
    predictions.df.pos$Price.Change = paste0(predictions.df.pos$Price.Change,"% or more")
    predictions.df.pos$Confidence.Score.HIT.TARGET = predictions.pos
    predictions.df.pos$Signal[predictions.df.pos$Confidence.Score.HIT.TARGET >= 0.9] = "BUY SIGNAL"
    
    predictions.df.comb = rbind(predictions.df.comb,predictions.df.pos)
    predictions.df.comb$Confidence.Score.MISS.TARGET = 1 - predictions.df.comb$Confidence.Score.HIT.TARGET
    predictions.df.comb$Confidence.Score.HIT.TARGET = round(predictions.df.comb$Confidence.Score.HIT.TARGET, 3)
    predictions.df.comb$Confidence.Score.MISS.TARGET = round(predictions.df.comb$Confidence.Score.MISS.TARGET, 3)
    
    
  }
  # assign('predictions.df.comb',predictions.df.comb,.GlobalEnv)
  return(datatable(predictions.df.comb))
  
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
predict.next.ohlc = function(symbol, output){
  
  pair = str_match(string = symbol, pattern = "(.*)_")[,2]
  timeframe = str_match(string = symbol, pattern = "_(.*)")[,2]
  
  # symbol = "AUDUSD_1day"
  # prediction = "BreakH"
  # pair = "AUDUSD"
  # timeframe = "1day"
  
  df1 = riingo_fx_prices(pair, start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = timeframe)
  df1 = df1[-nrow(df1),]
  df2 = httr::GET(paste0("https://api.tiingo.com/tiingo/fx/",pair,"/prices?resampleFreq=",timeframe,"&token=6fbd6ce7c9e035489f6238bfab127fcedbe34ac2"))
  request_char = rawToChar(df2$content)
  request_json = jsonlite::fromJSON(request_char, flatten = TRUE)
  df2 = request_json
  
  df2$date = str_replace(string = df2$date, pattern = "T", replacement = " ")
  df2$date = str_replace(string = df2$date, pattern = "Z", replacement = "")
  df2$date = as.POSIXct(df2$date, format = "%Y-%m-%d %H:%M:%S")
  df2 = df2[,c(2,1,3:6)]
  
  df = rbind(df1,df2)
  
  df = df[,-1]
  
  ###############################
  ############################### CHANGE NAMES
  colnames(df) = c("Date","Open","High","Low","Close")
  
  
  ###############################
  ############################### ADD IN MOVING AVERAGES
  df$MA10 = NA
  df$MA20 = NA
  
  for(k in 21:nrow(df)){
    df$MA10[k] = mean(df$Close[k-10:k])
    df$MA20[k] = mean(df$Close[k-20:k])
  }
  
  ###############################
  ############################### DEFINE OTHER INPUT VALUES
  df$OH = (df$High - df$Open)/df$Open * 100
  df$CH = (df$Close - df$Open)/ df$Open * 100
  df$LH = (df$High - df$Low) / df$Low * 100
  df$LC = (df$Close - df$Low) / df$Low * 100
  
  df$HMA = (df$High - df$MA20)/ df$MA20 * 100
  df$LMA = (df$Low - df$MA20)/ df$MA20 * 100
  df$CMA = (df$Close - df$MA20)/ df$MA20 * 100
  
  if(grepl(pattern = "day", x = timeframe)){
    df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d")
  }else{
    df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
  }
  df.xts = as.xts(df)
  UpTrend = as.data.frame(up.trend(df.xts))$`Up Trend`
  DownTrend = as.data.frame(down.trend(df.xts))$`Down Trend`
  
  df$UpTrend = as.numeric(UpTrend)
  df$DownTrend = as.numeric(DownTrend)
  
  Previous1 = rep(0, nrow(df))
  Previous1[df$Close > df$Open] = 1
  Previous2 = Lag(Previous1, 1)
  Previous3 = Lag(Previous1, 2)
  
  df$Previous1 = Previous1
  df$Previous2 = Previous2
  df$Previous3 = Previous3
  
  ###############################
  ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
  df = df[nrow(df)-1,-c(1:4)]
  
  df.m = as.matrix(df)
  
  bst.open = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_Open.rds"))
  bst.high = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_High.rds"))
  bst.low = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_Low.rds"))
  bst.close = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_Close.rds"))
  
  # bst.open = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Open.rds"))
  # bst.high = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_High.rds"))
  # bst.low = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Low.rds"))
  # bst.close = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Close.rds"))
  
  pred.open = predict(bst.open, df.m)
  pred.high = predict(bst.high, df.m)
  pred.low = predict(bst.low, df.m)
  pred.close = predict(bst.close, df.m)
  
  p.change.high = (pred.high - df$Close)/df$Close * 100
  
  assign("pred_High",pred.high,.GlobalEnv)
  assign("p.change.high",p.change.high,.GlobalEnv)
  
  output$predictPercentChangeHigh = renderInfoBox({
    infoBox("Predicted High", round(pred_High, digits = 3),icon = icon("bullseye"))
  })
  
  
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

predict.next.bh.bl.tar = function(symbol,timeframe, success.thresh){
  # symbol = "USDCAD"
  # timeframe = "1hour"
  # success.thresh = "0.8"
  
  assign("predictions.df.indi1", NULL, .GlobalEnv)
  assign("predictions.df.indi2", NULL, .GlobalEnv)
  assign("predictions.df.indi3", NULL, .GlobalEnv)
  assign("predictions.df.indi4", NULL, .GlobalEnv)
  
  predictions.df.comb = data.frame("Coin" = character(),
                                   "Price Change" = character(),
                                   "C.Score.HIT.TARGET" = character(),
                                   "C.Score.MISS.TARGET" = character(),
                                   "C.Score.BreakPrevoiusHigh" = character(),
                                   "Previous.High" = character(),
                                   "C.Score.BreakPrevoiusLow" = character(),
                                   "Previous.High" = character(),
                                   "Signal" = character())
  for(j in 1:length(symbol)){
    pair = symbol[j]
    
    predictions = c("BreakH","BreakL","Target")
    for(i in 1:length(predictions)){
      prediction = predictions[i]
      # 
      # symbol = "AUDUSD_1day"
      # prediction = "BreakL"
      # pair = "AUDUSD"
      # timeframe = "5min"
      
      df1 = riingo_fx_prices(pair, start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = timeframe)
      df1 = df1[-nrow(df1),]
      df2 = httr::GET(paste0("https://api.tiingo.com/tiingo/fx/",pair,"/prices?resampleFreq=",timeframe,"&token=6fbd6ce7c9e035489f6238bfab127fcedbe34ac2"))
      request_char = rawToChar(df2$content)
      request_json = jsonlite::fromJSON(request_char, flatten = TRUE)
      df2 = request_json
      
      df2$date = str_replace(string = df2$date, pattern = "T", replacement = " ")
      df2$date = str_replace(string = df2$date, pattern = "Z", replacement = "")
      df2$date = as.POSIXct(df2$date, format = "%Y-%m-%d %H:%M:%S")
      
      if(!is.null(nrow(df2))){
        df2 = df2[,c(2,1,3:6)]
        
        df = rbind(df1,df2)
        
        df = df[,-1]
      }else{
        df = df1
        df = df[,-1]
      }
      
      
      ###############################
      ############################### CHANGE NAMES
      colnames(df) = c("Date","Open","High","Low","Close")
      
      df_candle_plot = tail(df,30) %>%
        plot_ly(x = ~Date, type="candlestick",
                open = ~Open, close = ~Close,
                high = ~High, low = ~Low)
      df_candle_plot = df_candle_plot %>% layout(title = paste0('Last 30 candles for ',toupper(pair)),
                                                 xaxis = list(rangeslider = list(visible = F)))
      
      assign(paste0('df_candleplot_',pair),df_candle_plot,.GlobalEnv)
      ###############################
      ############################### ADD IN MOVING AVERAGES
      df$MA10 = NA
      df$MA20 = NA
      
      for(k in 21:nrow(df)){
        df$MA10[k] = mean(df$Close[k-10:k])
        df$MA20[k] = mean(df$Close[k-20:k])
      }
      
      ###############################
      ############################### DEFINE OTHER INPUT VALUES
      df$OH = (df$High - df$Open)/df$High * 100
      df$CH = (df$Close - df$Open)/ df$Close * 100
      df$LH = (df$High - df$Low) / df$High * 100
      df$LC = (df$Close - df$Low) / df$Low * 100
      
      df$HMA = (df$High - df$MA20)/ df$High * 100
      df$LMA = (df$Low - df$MA20)/ df$Low * 100
      df$CMA = (df$Close - df$MA20)/ df$Close * 100
      
      if(grepl(pattern = "day", x = timeframe)){
        df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d")
      }else{
        df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
      }
      df.xts = as.xts(df)
      UpTrend = as.data.frame(up.trend(df.xts))$`Up Trend`
      DownTrend = as.data.frame(down.trend(df.xts))$`Down Trend`
      
      df$UpTrend = as.numeric(UpTrend)
      df$DownTrend = as.numeric(DownTrend)
      
      Previous1 = rep(0, nrow(df))
      Previous1[df$Close > df$Open] = 1
      Previous2 = Lag(Previous1, 1)
      Previous3 = Lag(Previous1, 2)
      
      df$Previous1 = Previous1
      df$Previous2 = Previous2
      df$Previous3 = Previous3
      
      ###############################
      ############################### DETERMINE OUTCOME VALUES
      BreakL = NA
      BreakH = NA
      
      for(k in 2:(nrow(df))){
        if(df$Low[k] <= df$Low[k-1]){
          BreakL[k] = 1
        }else{
          BreakL[k] = 0
        }
        
        if(df$High[k] >= df$High[k-1]){
          BreakH[k] = 1
        }else{
          BreakH[k] = 0
        }
      }
      
      BreakH = c(BreakH, NA)
      BreakH = BreakH[-1]
      
      BreakL = c(BreakL, NA)
      BreakL = BreakL[-1]
      
      ###############################
      ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
      if(!is.null(nrow(df2))){
        df = df[nrow(df)-1,]
      }else{
        df = df[nrow(df),]
      }
      
      
      previous.low = df$Low
      previous.high = df$High
      previous.open = df$Open
      previous.close = df$Close
      
      df = df[,-c(1:5)]
      BreakL = BreakL[-c(1:20,length(BreakL))]
      BreakH = BreakH[-c(1:20,length(BreakH))]
      
      
      ###############################
      ############################### ROUND ALL INPUTS TO 2 DIGITS
      df = round(df, 2)
      
      df = as.matrix(df)
      
      predictions.df.pos = data.frame("Coin" = rep(toupper(pair),5),
                                      "Price Change" = seq(from= 0.05, to = 0.25, by=0.05),
                                      "C.Score.HIT.TARGET" = rep(NA,5),
                                      "C.Score.MISS.TARGET" = rep(NA,5),
                                      "C.Score.BreakPrevoiusHigh" = rep(NA,5),
                                      "Previous.High" = rep(NA,5),
                                      "C.Score.BreakPrevoiusLow" = rep(NA,5),
                                      "Previous.Low" = rep(NA,5),
                                      "Signal" = rep("DON'T BUY SIGNAL",5))
      
      predictions.df.neg = data.frame("Coin" = rep(toupper(pair),5),
                                      "Price Change" = seq(from= -0.25, to = -0.05, by=0.05),
                                      "C.Score.HIT.TARGET" = rep(NA,5),
                                      "C.Score.MISS.TARGET" = rep(NA,5),
                                      "C.Score.BreakPrevoiusHigh" = rep(NA,5),
                                      "Previous.High" = rep(NA,5),
                                      "C.Score.BreakPrevoiusLow" = rep(NA,5),
                                      "Previous.Low" = rep(NA,5),
                                      "Signal" = rep("DON'T BUY SIGNAL",5))
      predictions.pos = c()
      predictions.neg = c()
      
      if(prediction == "BreakH" | prediction == "BreakL"){
        bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",pair,"_",timeframe,"_",prediction,".rds"))
        pred = predict(bst, df)
        assign(paste0("pred_",prediction),pred,.GlobalEnv)
      }else{
        for(z in seq(from= 0.05, to = 0.25, by=0.05)){
          bst.pos = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",pair,"_",timeframe,z,".rds"))
          bst.neg = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",pair,"_",timeframe,-z,".rds"))
          pred.pos = predict(bst.pos, df)
          pred.neg = predict(bst.neg, df)
          predictions.pos = c(predictions.pos, pred.pos)
          predictions.neg = c(predictions.neg, pred.neg)
        }
      }
    }
    # POSITIVE
    predictions.df.pos$Price.Change = paste0(predictions.df.pos$Price.Change, "%")
    
    predictions.df.pos$C.Score.HIT.TARGET = round(predictions.pos, 3)
    predictions.df.pos$C.Score.MISS.TARGET = round(1 - predictions.pos, 3)
    
    predictions.df.pos$C.Score.BreakPrevoiusHigh = round(pred_BreakH, 3)
    predictions.df.pos$Previous.High = round(previous.high, 3)
    
    predictions.df.pos$C.Score.BreakPrevoiusLow = round(pred_BreakL, 3)
    predictions.df.pos$Previous.Low = round(previous.low, 3)
    
    predictions.df.pos$Signal[predictions.df.pos$C.Score.HIT.TARGET >= success.thresh] = "BUY SIGNAL"
    
    predictions.df.pos[nrow(predictions.df.pos)+1,] = NA
    
    # NEGATIVE
    predictions.df.neg$Price.Change = paste0(predictions.df.neg$Price.Change, "%")
    
    predictions.df.neg$C.Score.HIT.TARGET = round(predictions.neg, 3)
    predictions.df.neg$C.Score.MISS.TARGET = round(1 - predictions.neg, 3)
    
    predictions.df.neg$C.Score.BreakPrevoiusHigh = round(pred_BreakH, 3)
    predictions.df.neg$Previous.High = round(previous.high, 3)
    
    predictions.df.neg$C.Score.BreakPrevoiusLow = round(pred_BreakL, 3)
    predictions.df.neg$Previous.Low = round(previous.low, 3)
    
    predictions.df.neg$Signal[predictions.df.neg$C.Score.HIT.TARGET >= success.thresh] = "BUY SIGNAL"
    
    #INDIVIDUAL
    predictions.df.indi = data.frame(rbind(predictions.df.neg,predictions.df.pos))
    predictions.df.indi = predictions.df.indi[,-4]
    colnames(predictions.df.indi) = c("Coin","Price.Change","CS HT","CS BH", "Prev.High","CS BL", "Prev.Low","Signal")
    predictions.df.indi$`CS HT` = round(predictions.df.indi$`CS HT`, 3)
    assign(paste0("predictions.df.indi",j), predictions.df.indi, .GlobalEnv)
    print(j)
    
    # COMBINED
    predictions.df.comb = data.frame(rbind(predictions.df.comb,predictions.df.neg,predictions.df.pos))
    
  }
  predictions.df.comb = predictions.df.comb[,-4]
  colnames(predictions.df.comb) = c("Coin","Price.Change","CS HT","CS BH", "Prev.High","CS BL", "Prev.Low","Signal")
  assign("predictions.df.comb", predictions.df.comb, .GlobalEnv)
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

GetTopHolders = function(token.account.name){
  # page = read_html(paste0("https://etherscan.io/token/generic-tokenholders2?m=dim&a=",token.account.name,"&s=39025187376288180&sid=e88ba71b362fc00233af8a8db211da32&p=1"))
  # 
  # holders = page %>% html_nodes(".js-clipboard") %>% html_attr("data-clipboard-text")
  # x = page %>% html_nodes("td") %>% html_text()
  # 
  # percentage = x[grep("%",x)] %>% trimws()
  # value = x[grep("\\$",x)] %>% trimws()
  # quantity = x[seq(from=3,to=300,by=6)] %>% trimws()
  # holders.name = x[seq(from=2,to=300,by=6)] %>% trimws()
  # 
  # user.coin.holdings = data.frame(holders.name,
  #                                 holder.wallet = holders,
  #                                 quantity,
  #                                 percentage,
  #                                 value)
  user.coin.holdings = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/EtherscanData", object = paste0("userpass.df.",token.account.name,".rds"))
  
  
  return(user.coin.holdings)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

GetBlockTime = function(days){
  url = paste0("https://api.etherscan.io/api",
               "?module=block",
               "&action=getblocknobytime",
               "&timestamp=",round(as.numeric(as.POSIXct(Sys.time())- (60*60*24*days)),0),
               "&closest=before",
               "&apikey=YourApiKeyToken"
  )
  test_get = httr::GET(url)
  
  test_get$status_code
  
  test = rawToChar(test_get$content)
  
  test = possible_json(test, flatten = TRUE)
  block.number = test$result
  
  return(block.number)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

GetHolderInfo = function(coin.address, holder.address, days){
  print("made it inside function")
  block.number = GetBlockTime(30)
  # # #
  # coin.address = "0xdac17f958d2ee523a2206206994597c13d831ec7"
  # holder.address = "0x47ac0Fb4F2D84898e4D9E7b4DaB3C24507a6D503"
  
  # get token transactions of account
  url = paste0("https://api.etherscan.io/api?module=account&action=tokentx",
               "&contractaddress=",coin.address,
               "&address=",holder.address,
               "&page=1",
               "&offset=10000",
               "&startblock=",block.number,
               "&endblock=27025780",
               "&sort=asc",
               "&apikey=HKYWSDAZQS14QKVB7KY1AKTQURYMEPFFZU")
  
  test_get = httr::GET(url)
  
  test = rawToChar(test_get$content)
  
  test = possible_json(test, flatten = TRUE)
  message = test$message
  
  print(message)
  
  
  if(message == "No transactions found"){
    print(message)
    status = "inactive"
    assign("status",status,.GlobalEnv)
    
    return(NULL)
  }else{
    status = "active"
    assign("status",status,.GlobalEnv)
    
    df = test$result
    
    df$datetime = as_datetime(as.numeric(df$timeStamp))
    df$actualValue = round(as.numeric(df$value) / (1 * 10^(as.numeric(df$tokenDecimal))),0)
    
    df$in.out = "out"
    df$in.out[df$to == tolower(holder.address)] = "in"
    
    
    
    df = df %>%
      select("from","to","in.out","tokenName","datetime","actualValue")
    assign('df.test',df,.GlobalEnv)
    
    seven.day.df = df[df$datetime >= Sys.Date() - 7,]
    assign("seven.day.df",seven.day.df,.GlobalEnv)
    
    
    
    
    return(df)
  }
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

Backtest.AV <- function(df, startDate, endDate, topic, type){
  df = df.comb.all
  topic = "blockchain"
  startDate = "2023-01-01"
  endDate = "2023-03-01"
  type = "CRYPTO"
  
  df.all.daterange = df %>%
    filter(date >= startDate & date <= endDate & as.numeric(relevance_score) >= 0.5)
  
  df.all.daterange$thirty.min.back.change = (df.all.daterange$thirty.min.back - df.all.daterange$price.news.break) / df.all.daterange$price.news.break * 100
  df.all.daterange$thirty.min.forward.change = (df.all.daterange$thirty.min.forward - df.all.daterange$price.news.break) / df.all.daterange$price.news.break * 100
  df.all.daterange$hour.forward.change = (df.all.daterange$one.hr.forward - df.all.daterange$price.news.break) / df.all.daterange$price.news.break * 100
  df.all.daterange$change.sum = df.all.daterange$thirty.min.forward.change - df.all.daterange$thirty.min.back.change
  
  df.all.daterange.grp = df.all.daterange %>% group_by(Topic)
  df.all.daterange.grp = na.omit(df.all.daterange.grp)
  
  df.rm.dup = df.all.daterange.grp[which(!duplicated(df.all.daterange.grp[,c(1,5)])),]
  
  if(type == "ALL"){
    df.filter.type = df.rm.dup
  }else{
    df.filter.type = df.rm.dup[grepl(pattern = type, df.rm.dup$ticker),]
  }
  
  df.filter.type.bull = df.filter.type %>%
    filter(ticker_sentiment_label == "Bullish" | ticker_sentiment_label == "Somewhat-Bullish")
  df.filter.type.bear = df.filter.type %>%
    filter(ticker_sentiment_label == "Bearish" | ticker_sentiment_label == "Somewhat-Bearish")
  df.bull.bear.news = rbind(df.filter.type.bull, df.filter.type.bear)
  
  
  t.sent.bull = summarise(df.filter.type.bull, mean.sent = mean(as.numeric(ticker_sentiment_score)))
  t.change.bull = summarise(df.filter.type.bull, mean.change = mean(hour.forward.change))
  
  t.sent.bear = summarise(df.filter.type.bear, mean.sent = mean(as.numeric(ticker_sentiment_score)))
  t.change.bear = summarise(df.filter.type.bear, mean.change = mean(hour.forward.change))
  
  if(length(which(t.change.bull$mean.change < 0)) > 0){
    t.pie.bull = t.change.bull[-(which(t.change.bull$mean.change < 0)),]
  }else{
    t.pie.bull = t.change.bull
  }
  if(length(which(t.change.bear$mean.change < 0)) > 0){
    t.pie.bear = t.change.bear[-(which(t.change.bear$mean.change > 0)),]
  }else{
    t.pie.bear = t.change.bear
  }
  
  t.pie.bear$mean.change = abs(t.pie.bear$mean.change)
  
  fig.bull <- plot_ly(t.pie.bull, labels = ~Topic, values = ~mean.change, type = "pie")
  fig.bear <- plot_ly(t.pie.bear, labels = ~Topic, values = ~mean.change, type = "pie")
  
  figs = list(fig.bull = fig.bull,
              fig.bear = fig.bear,
              tbl.bull = t.change.bull,
              tbl.bear = t.change.bear,
              news.feed = df.bull.bear.news)
  return(figs)
  
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

BackTestFF = function(region,topic,date.range,asset,timeframe,impact,sub.filter = "All"){
  # region = "USD"
  # asset = "EURUSD"
  # timeframe = "5min"
  # topic = "Employment"
  # date.range = c("2020-01-01",'2023-09-09')
  # sub.filter = "Unemployment Claims"
  # impact = "yel"
  
  timeframe.numeric = as.numeric(str_extract(timeframe,pattern = "\\d+"))
  
  
  if(asset == "SPY"){
    type = "stock"
  }else if(asset %in% c("ETHUSDT","BTCUSDT","BNBUSDT", "LINKUSDT","SOLUSDT", "INJUSDT","SNXUSDT","DYDXUSDT","MATICUSDT")){
    type = "crypto"
  }else{
    type = "forex"
  }
  
  df = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/ForexFactoryData/News_With_Prices", object = paste0("df.",type,".news.prices.",asset,timeframe,".rds"))
  
  if(impact != "All"){
    df = df[grepl(pattern = impact, x = df$impact),]
  }
  
  df$formatted_dates <- as.factor(format(df$POSIXct, "%B %Y"))
  
  ind.K.actual = grep(pattern = "K", x = df$actual)
  ind.K.forecast = grep(pattern = "K", x = df$forecast)
  ind.M.actual = grep(pattern = "M", x = df$actual)
  ind.M.forecast = grep(pattern = "M", x = df$forecast)
  ind.B.actual = grep(pattern = "B", x = df$actual)
  ind.B.forecast = grep(pattern = "B", x = df$forecast)
  ind.perc.actual = grep(pattern = "%", x = df$actual)
  ind.perc.forecast = grep(pattern = "%", x = df$forecast)
  
  df$actual[ind.K.actual] = as.numeric(sub(pattern = "K", replacement = "", x = df$actual[ind.K.actual])) * 1000
  df$forecast[ind.K.forecast] = as.numeric(sub(pattern = "K", replacement = "", x = df$forecast[ind.K.forecast])) * 1000
  
  df$actual[ind.M.actual] = as.numeric(sub(pattern = "M", replacement = "", x = df$actual[ind.M.actual])) * 1000000
  df$forecast[ind.M.forecast] = as.numeric(sub(pattern = "M", replacement = "", x = df$forecast[ind.M.forecast])) * 1000000
  
  df$actual[ind.B.actual] = as.numeric(sub(pattern = "B", replacement = "", x = df$actual[ind.B.actual])) * 1000000000
  df$forecast[ind.B.forecast] = as.numeric(sub(pattern = "B", replacement = "", x = df$forecast[ind.B.forecast])) * 1000000000
  
  df$actual[ind.perc.actual] = as.numeric(sub("%","",df$actual[ind.perc.actual]))
  df$forecast[ind.perc.forecast] = as.numeric(sub("%","",df$forecast[ind.perc.forecast]))
  
  
  if(sub.filter != "All"){
    df = df %>%
      filter(event.title == sub.filter)
  }
  
  df = df %>%
    filter(currency == region & Tag == topic & df$POSIXct >= date.range[1] & df$POSIXct <= date.range[2]) %>%
    group_by(formatted_dates)
  
  df.to.summarize = df[,c(5,6,12:17)]
  df.to.summarize.pie = df[,c(4,13:15)]
  colnames(df.to.summarize.pie) = c("title","once.back","current","once.forward")
  colnames(df.to.summarize) = c("actual","forecast","double.back","once.back","current","once.forward","double.forward","formatted_dates")
  
  ##
  df.to.summarize.pie = df.to.summarize.pie %>%
    group_by(title)
  
  df.to.summarize.pie$Change.Backward = abs(round((df.to.summarize.pie$once.back - df.to.summarize.pie$current) / df.to.summarize.pie$current * 100, 3))
  df.to.summarize.pie$Change.Forward = abs(round((df.to.summarize.pie$once.forward - df.to.summarize.pie$current) / df.to.summarize.pie$current * 100, 3))
  
  df.to.summarize.pie$Change = df.to.summarize.pie$Change.Backward + df.to.summarize.pie$Change.Forward
  
  x.pie = summarize(df.to.summarize.pie, sum.change = mean(Change))
  
  fig.pie1 = plot_ly(x.pie, labels = ~title, values = ~sum.change, type = "pie")  %>%
    layout(paper_bgcolor = "transparent",
           font = list(color = 'white'),
           title = paste0("Pie Chart for ",topic))
  ##
  
  
  char_cols <- sapply(df.to.summarize, is.character)
  df.to.summarize[char_cols] = lapply(df.to.summarize[char_cols], as.numeric)
  
  df.summarized = summarise_all(df.to.summarize, sum)
  
  df.summarized$Result = "Beat"
  df.summarized$Result[df.summarized$actual < df.summarized$forecast] = "Miss"
  df.summarized$Result[df.summarized$actual == df.summarized$forecast] = "Neutral"
  df.summarized$double.back.perc = paste0(round((df.summarized$double.back - df.summarized$current) / df.summarized$current * 100, 3),"%")
  df.summarized$once.back.perc = paste0(round((df.summarized$once.back - df.summarized$current) / df.summarized$current * 100, 3),"%")
  df.summarized$once.forward.perc = paste0(round((df.summarized$once.forward - df.summarized$current) / df.summarized$current * 100, 3),"%")
  df.summarized$double.forward.perc = paste0(round((df.summarized$double.forward - df.summarized$current) / df.summarized$current * 100, 3),"%")
  
  df.summarized$Region = region
  df.summarized$Topic = topic
  df.summarized$Asset = asset
  
  df.summarized = df.summarized %>%
    select(formatted_dates, Region, Topic, Asset, forecast, actual, Result, double.back.perc, once.back.perc,once.forward.perc,double.forward.perc)
  colnames(df.summarized) = c("Month",
                              "Region",
                              "Topic",
                              "Asset",
                              "Forecast",
                              "Actual",
                              "Result",
                              paste0(timeframe.numeric*2,"min Back % Change"),
                              paste0(timeframe.numeric,"min Back % Change"),
                              paste0(timeframe.numeric,"min Forward % Change"),
                              paste0(timeframe.numeric*2,"min Forward % Change")
  )
  set.all= setNames("All","All")
  unique.sub.topics = setNames(unique(df$event.title), unique(df$event.title))
  unique.sub.topics = c(set.all,unique.sub.topics)
  
  df.summarized = df.summarized %>% arrange(my(df.summarized$Month))
  
  to.return = list(df.summarized = df.summarized,
                   unique.sub.topics = unique.sub.topics,
                   pie2 = fig.pie1)
  return(to.return)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

CreatePie <- function(region,topic,date.range,asset,timeframe,impact,sub.filter = "All"){
  # region = "USD"
  # asset = "ETHUSDT"
  # timeframe = "5min"
  # topic = "Growth"
  # start_date = "2018-01-01"
  # end_date = "2023-01-01"
  # sub.filter = "All"
  
  timeframe.numeric = as.numeric(str_extract(timeframe,pattern = "\\d+"))
  
  
  if(asset == "SPY"){
    type = "stock"
  }else if(asset %in% c("ETHUSDT","BTCUSDT","BNBUSDT", "LINKUSDT","SOLUSDT", "INJUSDT","SNXUSDT","DYDXUSDT","MATICUSDT")){
    type = "crypto"
  }else{
    type = "forex"
  }
  
  df = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/ForexFactoryData/News_With_Prices", object = paste0("df.",type,".news.prices.",asset,timeframe,".rds"))
  
  if(impact != "All"){
    df = df[grepl(pattern = impact, x = df$impact),]
  }
  
  df$formatted_dates <- as.factor(format(df$POSIXct, "%B %Y"))
  
  df = df %>%
    filter(currency == region & df$POSIXct >= date.range[1] & df$POSIXct <= date.range[2]) %>%
    group_by(Tag)
  
  df.for.pie = df[,c(10,13,14,15)]
  
  colnames(df.for.pie) = c('Tag', "Once.Back",'Current', 'Once.Forward')
  
  df.for.pie$Change.Backward = abs(round((df.for.pie$Once.Back - df.for.pie$Current) / df.for.pie$Current * 100, 3))
  df.for.pie$Change.Forward = abs(round((df.for.pie$Once.Forward - df.for.pie$Current) / df.for.pie$Current * 100, 3))
  
  df.for.pie$Change = df.for.pie$Change.Backward + df.for.pie$Change.Forward
  
  x = summarize(df.for.pie, sum.change = mean(Change))
  
  fig.pie = plot_ly(x, labels = ~Tag, values = ~sum.change, type = "pie") %>%
    layout(paper_bgcolor = "transparent",
           font = list(color = 'white'),
           title = "Pie Chart Overview")
  
  return(fig.pie)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

CreateTimeseries <- function(region,topic,date.range,asset,timeframe,impact,sub.filter = "All",timeseries.selected){
  # region = "USD"
  # asset = "USDCAD"
  # timeframe = "5min"
  # topic = "Growth"
  # start_date = "2018-01-01"
  # end_date = "2023-01-01"
  # sub.filter = "All"
  # timeseries.selected = c(1,2)
  
  timeframe.numeric = as.numeric(str_extract(timeframe,pattern = "\\d+"))
  
  
  if(asset == "SPY"){
    type = "stock"
  }else if(asset %in% c("ETHUSDT","BTCUSDT","BNBUSDT", "LINKUSDT","SOLUSDT", "INJUSDT","SNXUSDT","DYDXUSDT","MATICUSDT")){
    type = "crypto"
  }else{
    type = "forex"
  }
  
  df = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/ForexFactoryData/News_With_Prices", object = paste0("df.",type,".news.prices.",asset,timeframe,".rds"))
  
  if(impact != "All"){
    df = df[grepl(pattern = impact, x = df$impact),]
  }
  
  df$formatted_dates <- as.factor(format(df$POSIXct, "%B %Y"))
  
  if(sub.filter != "All"){
    df = df %>%
      filter(event.title == sub.filter)
  }
  
  df = df %>%
    filter(currency == region & Tag == topic & df$POSIXct >= date.range[1] & df$POSIXct <= date.range[2]) %>%
    group_by(formatted_dates)
  
  unique.months = unique(df$formatted_dates)
  
  # selected.filter.df = df %>%
  #   filter(formatted_dates == unique.months[timeseries.selected] & !duplicated(date))
  
  selected.filter.df = df %>%
    filter(!duplicated(date))
  selected.filter.df = selected.filter.df[grepl(pattern = paste0(unique.months[timeseries.selected], collapse = "|"),selected.filter.df$formatted_dates), ]
  
  for(i in 1:nrow(selected.filter.df)){
    x = selected.filter.df[i,c(12:16)]
    x.axis = c(paste0(timeframe.numeric*2," min back"),paste0(timeframe.numeric," min back"),paste0("news break"),paste0(timeframe.numeric," min forward"),paste0(timeframe.numeric*2," min forward"))
    
    current.price = x[,3][[1]]
    prices = c()
    for(j in 1:ncol(x)){
      pr = (x[1,j][[1]] - current.price) / current.price * 100
      prices = c(prices, pr)
    }
    if(i == 1){
      plot1 = plot_ly(x = x.axis, y = prices, type="scatter", mode="lines", name = selected.filter.df$POSIXct[i]) %>%
        layout(
          xaxis = list(categoryorder = "array", categoryarray = x.axis),
          title = "Time-Series of News Break"
        )
    }else{
      plot1 = plot1 %>%
        add_trace(x = x.axis, y = prices, mode="lines",name = selected.filter.df$POSIXct[i])
    }
    
    
  }
  
  return(plot1)
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

FearGreedToday <- function(){
  url = paste0("https://production.dataviz.cnn.io/index/fearandgreed/graphdata/",Sys.Date())
  
  test_get = httr::GET(url)
  
  test_get$status_code
  
  test = rawToChar(test_get$content)
  
  test = fromJSON(test, flatten = TRUE)
  
  fear.greed.score = test$fear_and_greed$score
  fear.greed.rating = test$fear_and_greed$rating
  
  to.return = list(fear.greed.score = fear.greed.score,
                   fear.greed.rating = fear.greed.rating)
  
  return(to.return)
}
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
ReturnSentimentValue <- function(x){
  if(x <= -0.35){
    val = "Bearish"
  }
  if(x > -0.35 & x <= -0.15){
    val = "Somewhat-Bearish"
  }
  if(x > -0.15 & x < 0.15){
    val = "Neutral"
  }
  if(x > 0.15 & x <= 0.35){
    val = "Somewhat-Bullish"
  }
  if(x >= 0.35){
    val = "Bullish"
  }
  
  return(val)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

PerformSentimentAnalysis <- function(coin, confidence, type){
  # coin = c("BTCUSDT",'ETHUSDT',"LINAUSDT",'REEFUSDT')
  # confidence = 0.5

  
  
  assign("predictions.df.indi11", NULL, .GlobalEnv)
  assign("predictions.df.indi22", NULL, .GlobalEnv)
  assign("predictions.df.indi33", NULL, .GlobalEnv)
  assign("predictions.df.indi44", NULL, .GlobalEnv)
  
  if(type != "Crypto"){
    return()
  }
  
  targets= seq(from = 0.2, to = 3, by = 0.2)
  
  for(z in 1:length(coin)){
    # Funding rate data
    posixct.time = as.POSIXct(Sys.Date() - 3, tz = 'UTC')

    ms = as.numeric(posixct.time, units = "ms") * 1000

    url = paste0("https://fapi.binance.com/fapi/v1/fundingRate?limit=1000&symbol=",coin[z],"&startTime=",ms)
    test_get = httr::GET(url)

    test_get$status_code

    test = rawToChar(test_get$content)

    test = fromJSON(test, flatten = TRUE)

    test$date = as.POSIXct(test$fundingTime / 1000, origin = "1970-01-01", tz = "UTC")

    funding.data = test

    fd.for.merge = funding.data[nrow(funding.data),] %>%
      select(fundingRate, date)
    colnames(fd.for.merge) = c("funding.rate","date.8hr")
    fd.for.merge$date.8hr = as.character(fd.for.merge$date.8hr)
    fd.for.merge$date.8hr = as.POSIXlt(fd.for.merge$date.8hr, tz = 'UTC')
    # 
    # fd.for.merge = data.frame(funding.rate = 0.0001,
    #                           date.8hr = "hi")
    
    #
    
    # webscrape fear
    
    url = "https://production.dataviz.cnn.io/index/fearandgreed/graphdata/2023-01-01"
    
    test_get = httr::GET(url)
    
    test_get$status_code
    
    test = rawToChar(test_get$content)
    
    test = fromJSON(test, flatten = TRUE)
    
    df.fear.greed = test$fear_and_greed_historical$data
    
    df.fear.greed$date = as.POSIXct(df.fear.greed$x/1000, origin = "1970-01-01", tz = "UTC")
    df.fg.for.merge = df.fear.greed[nrow(df.fear.greed),] %>%
      select(y, date)
    colnames(df.fg.for.merge) = c("rating", "just.date")
    
    # Add VIX index
    time.formatted = ymd(Sys.Date()) - 3
    time.formatted = format(time.formatted, format = "%Y%m%dT%H%M")
    
    full.url = paste0("https://www.alphavantage.co/query?function=NEWS_SENTIMENT&topics=blockchain&limit=1000&time_from=",time.formatted,"&sort=EARLIEST&apikey=",api.key.av)
    
    test_get = httr::GET(full.url)
    
    test_get$status_code
    
    test = rawToChar(test_get$content)
    
    test = fromJSON(test, flatten = TRUE)
    
    if(grepl("rate limit", test[1])){
      # print("rate limit reached")
      return()
    }
    
    df.comb = test$feed
    
    ind = which(duplicated(df.comb$title) & duplicated(df.comb$overall_sentiment_score))
    
    df.comb.rem = df.comb[-ind,]
    df.comb.rem$time = strptime(df.comb.rem$time_published, format = "%Y%m%dT%H%M%S")
    
    
    df.vix = getSymbols("^VIX", auto.assign = FALSE)
    colnames(df.vix) = c("open","high","low","close","volume","adjusted")
    df.vix = as.data.frame(df.vix)
    df.vix$time = as.POSIXct(row.names(df.vix))
    
    df.vix.for.merge = df.vix[nrow(df.vix),] %>%
      select(open, time)
    colnames(df.vix.for.merge) = c("vix.open","just.date")
    
    x = possibly_riingo_crypto_prices(coin[z], start_date = Sys.Date() - lubridate::dmonths(14), end_date = Sys.Date(), resample_frequency = "1day")
    
    if(length(x) == 1){
      stop()
    }else{
      x = x[,c(1,4:9)]
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
    
    x$just.date = as.Date(x$date)
    
    x = x[nrow(x),]
    
    x$rating = df.fg.for.merge$rating[1]
    x$vix.open = df.vix.for.merge$vix.open[1]
    x$funding.rate = fd.for.merge$funding.rate[1]
    
    x$news.1hr = NA
    x$news.8hr = NA
    x$news.24hr = NA
    
    
    x$news.1hr = mean(df.comb.rem[which(df.comb.rem$time > x$date - lubridate::hours(1) & df.comb.rem$time <= x$date),]$overall_sentiment_score)
    x$news.8hr = mean(df.comb.rem[which(df.comb.rem$time > x$date - lubridate::hours(8) & df.comb.rem$time <= x$date),]$overall_sentiment_score)
    x$news.24hr = mean(df.comb.rem[which(df.comb.rem$time > x$date - lubridate::hours(24) & df.comb.rem$time <= x$date),]$overall_sentiment_score)
    
    
    x[is.na(x)] = 0
    
    df = x %>%
      select(upTrend, downTrend, rating, vix.open, funding.rate, news.1hr, news.8hr, news.24hr)
    df[] <- lapply(df, as.numeric)
    
    df.m = as.matrix(df)
    
    predictions = c()
    
    for(i in 1:length(targets)){
      bst = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/AlphaVantageData/blockchain_models", object = paste0("bst_",coin[z],"_blockchain_",targets[i],".rds"))
      
      prediction = predict(bst, df.m)
      
      predictions = c(predictions, prediction)
    }
    
    df.to.return = data.frame(Coin = rep(coin[z], length(predictions)),
                              Price.Change = paste0(targets,"%"),
                              Confidence.Hit.Target = round(predictions,3),
                              signal = rep("DON'T BUY SIGNAL", length(predictions)))
    df.to.return$signal[df.to.return$Confidence.Hit.Target > confidence] = "BUY SIGNAL"
    
    colnames(df.to.return) = c("Coin", "Price.Change", "CS HT", "Signal")
    
    assign(paste0("predictions.df.indi",z,z), df.to.return, .GlobalEnv)
    
  }
  
  one.hr.sentiment = ReturnSentimentValue(df$news.1hr)
  eight.hr.sentiment = ReturnSentimentValue(df$news.8hr)
  twenfour.hr.sentiment = ReturnSentimentValue(df$news.24hr)
  
  to.return = list(
    one.hr.sentiment = one.hr.sentiment,
    eight.hr.sentiment = eight.hr.sentiment,
    twenfour.hr.sentiment = twenfour.hr.sentiment
  )
  
  return(to.return)
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

BacktestSentiment <- function(Type,TargetIncreasePercent, SuccessThreshold, Symbol, Timeframe){
  # Type = "Crypto"
  # Timeframe = "1day"
  # TargetIncreasePercent = 0.2
  # SuccessThreshold = 0.5
  # Symbol = "REEFUSDT"
  
  if(Type == "Crypto" & Timeframe == "1day"){
    
    
    df = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/AlphaVantageData/blockchain_models", object = paste0("df.examine_",Symbol,"_blockchain_",TargetIncreasePercent,".rds"))
    
    df$prediction = 0
    df$prediction[df$predicted.prob >= SuccessThreshold] = 1
    
    true.pos = length(which(df$actual.high == 1 & df$prediction == 1))
    false.pos = length(which(df$actual.high == 0 & df$prediction == 1))
    false.neg = length(which(df$actual.high == 1 & df$prediction == 0))
    
    
    precision = true.pos / (true.pos + false.pos) * 100
    recall = true.pos / (true.pos + false.neg) * 100
    f1 = 2*((precision * recall)/(precision + recall)) / 100
    
    precision = round(precision, digits = 4)
    recall = round(recall, digits = 4)
    f1 = round(f1, digits = 4)
    
    to.return = list(df.examine = df,
                     precision = precision,
                     recall = recall,
                     f1 = f1)
    
    return(to.return)
  }
}

