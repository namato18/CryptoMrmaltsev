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

options(scipen=999)
possible_json = possibly(.f = jsonlite::fromJSON, otherwise = 'ERROR' )

possibly_s3read_using = possibly(s3read_using, otherwise = "ERROR")

readRenviron(".Renviron")
Sys.setenv(TZ='UTC')
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

tokens_html = read_html("https://etherscan.io/tokens")
tokens = tokens_html %>% html_nodes(".link-dark") %>%
  html_attr("href")
tokens = tokens[grep(pattern = "token",tokens)]
tokens = str_match(string = tokens, pattern = "token/(.*)")[,2]

names = tokens_html %>% html_nodes(".fw-medium") %>% html_text()
names = names[1:50]

token.names.df = data.frame(cbind(names,tokens))

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
getTimeRemaining = function(timeframe){
  
  utcTime = lubridate::now(tzone = 'UTC')
  utcTime = format(utcTime, format = "%H:%M:%S")
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
  if(timeframe == '1day'){
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



predict.tomorrow.multiple <- function(Type,Symbols, Timeframe, SuccessThreshold, .GlobalEnv){
  # # Symbols = Symbols
  # Symbols = c('AAPL')
  # Timeframe = 'weekly'
  # i = 1
  # SuccessThreshold = 0.9
  # Type="Stocks"
  
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
    
    
  }
  assign("predictions.df.comb",predictions.df.comb,.GlobalEnv)
  
  
  
  
  
}

#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################





predict_week = function(symbol, timeframe){
  symbol = toupper(symbol)
  # symbol = 'AAPL'
  # timeframe = 'daily'
  data = data.frame(getSymbols.tiingo(Symbols = symbol, auto.assign = FALSE,api.key = '6fbd6ce7c9e035489f6238bfab127fcedbe34ac2', periodicity = timeframe))
  # data = data.frame(getSymbols(symbol, auto.assign = FALSE, periodicity = timeframe))
  data = data[-nrow(data),1:4]
  data = na.omit(data)
  # data = round(data, digits = 0)
  
  colnames(data) = c('open','high','low','close')
  
  data$time = row.names(data)
  
  data$time = gsub(pattern = "X", replacement = "", x = data$time)
  data$time = gsub(pattern = "\\.", replacement = "-", x = data$time)
  
  if(timeframe == 'daily'){
    data.add = data.frame(time = seq(from = as_date(Sys.Date()),
                                     by = "day", length.out = 7),
                          open = NA,
                          high = NA,
                          low = NA,
                          close = NA)
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
  
  pred <- data_selected[((nrow(data) - 7 + 1)):nrow(data), ]
  
  
  
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
  
  if(timeframe == 'daily'){
    plot.out = ggplot(data = x, aes(x = times)) + 
      geom_line(aes(y = data_y), color = "blue") +
      geom_line(aes(y = predicted_y), color = "red") +
      xlab("Date") +
      ylab("Price") +
      ggtitle(paste0("Predicted Stock Price for ",symbol)) +
      scale_x_date(date_breaks = "1 day", date_labels =  "%d %B") +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  }else{
    plot.out = ggplot(data = x, aes(x = times)) + 
      geom_line(aes(y = data_y), color = "blue") +
      geom_line(aes(y = predicted_y), color = "red") +
      xlab("Date") +
      ylab("Price") +
      ggtitle(paste0("Predicted Stock Price for ",symbol)) +
      scale_x_date(date_breaks = "1 week", date_labels =  "%d %B") +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  }
  
  
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
      
      # symbol = "AUDUSD_1day"
      # prediction = "BreakL"
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
      df = df[nrow(df)-1,]
      
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
        for(j in seq(from= 0.05, to = 0.25, by=0.05)){
          bst.pos = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",pair,"_",timeframe,j,".rds"))
          bst.neg = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",pair,"_",timeframe,-j,".rds"))
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
    
    # COMBINED
    predictions.df.comb = data.frame(rbind(predictions.df.comb,predictions.df.neg,predictions.df.pos))
    
  }
  assign("predictions.df.comb", predictions.df.comb, .GlobalEnv)
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

GetTopHolders = function(token.account.name){
  page = read_html(paste0("https://etherscan.io/token/generic-tokenholders2?m=dim&a=",token.account.name,"&s=39025187376288180&sid=e88ba71b362fc00233af8a8db211da32&p=1"))
  
  holders = page %>% html_nodes(".js-clipboard") %>% html_attr("data-clipboard-text")
  x = page %>% html_nodes("td") %>% html_text()
  
  percentage = x[grep("%",x)] %>% trimws()
  value = x[grep("\\$",x)] %>% trimws()
  quantity = x[seq(from=3,to=300,by=6)] %>% trimws()
  holders.name = x[seq(from=2,to=300,by=6)] %>% trimws()
  
  user.coin.holdings = data.frame(holders.name,
                                  holder.wallet = holders,
                                  quantity,
                                  percentage,
                                  value)
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
  block.number = GetBlockTime(30)
  # # #
  # coin.address = token.names.df$tokens[1]
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
  
  test_get$status_code
  
  test = rawToChar(test_get$content)
  
  test = possible_json(test, flatten = TRUE)
  message = test$message
  df = test$result
  
  if(message == "No transactions found"){
    print(message)
  }else{
    
    df$datetime = as_datetime(as.numeric(df$timeStamp))
    df$actualValue = round(as.numeric(df$value) / (1 * 10^(as.numeric(df$tokenDecimal))),0)
    
    df = df %>%
      select("from","to","tokenName","datetime","actualValue")
    
    return(df)
  }

}

