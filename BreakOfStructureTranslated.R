library(dplyr)
library(riingo)
library(purrr)
library(zoo)

riingo::riingo_set_token("6fbd6ce7c9e035489f6238bfab127fcedbe34ac2")

# Define error catching
possibly_riingo_crypto_prices = possibly(riingo_crypto_prices, otherwise = "ERROR")

# function for grabbing historical data
fetch_binance_data <- function(symbol = "BTCUSDT", interval = "1hour", start.time = Sys.Date() - 7, end.time = Sys.Date()){
  
  df = possibly_riingo_crypto_prices(ticker = symbol, start_date = start.time, end_date = end.time, resample_frequency = interval, exchanges = "binance")
  
  df = df %>%
    select(ticker, date, open, high, low, close, volume)
  
  colnames(df) = c("ticker","Date","Open","High","Low","Close","Volume")
  
  return(df)
}


# Function for finding breaks in structure
identify_break_of_structure <- function(df){
  # Identify swing high/low
  for.high.roll = zoo(df$High, order.by = df$Date)
  for.low.roll = zoo(df$Low, order.by = df$Date)
  
  df$SwingHigh = df$High == rollapply(for.high.roll, width = 5, FUN = max, fill = NA)
  df$SwingLow = df$Low == rollapply(for.low.roll, width = 5, FUN = min, fill = NA)
  
  df = na.omit(df)
  
  # Check if current high is higher than previous high
  diff.x = diff(df$High)
  is.higher.high = c(FALSE, diff.x > 0)
  
  # Check if current low is higher than previous high
  diff.x = diff(df$Low)
  is.higher.low = c(FALSE, diff.x > 0)
  
  # Check if current high is lower than previous high
  diff.x = diff(df$High)
  is.lower.high = c(FALSE, diff.x < 0)
  
  # Check if current low is lower than previous low
  diff.x = diff(df$Low)
  is.lower.low = c(FALSE, diff.x < 0)
  
  # Identify breaks of structure
  breaks = c()
  
  for(i in 1:nrow(df)){
    if(df$SwingHigh[i] & is.lower.high[i]){
      breaks = c(breaks, paste0("Break of Structure ", df$Date[i], " LH"))
    }else if(df$SwingLow[i] & is.lower.low[i]){
      breaks = c(breaks, paste0("Break of Structure ", df$Date[i], " LL"))
    }else if(df$SwingHigh[i] & is.higher.high[i]){
      breaks = c(breaks, paste0("Break of Structure ", df$Date[i], " HH"))
    }else if(df$SwingLow[i] & is.higher.low[i]){
      breaks = c(breaks, paste0("Break of Structure ", df$Date[i], " HL"))
    }
  }
  
  return(breaks)
}

# Example Usage
# Fetch binance coin data
btc_data = fetch_binance_data(symbol = "BTCUSDT", interval = "15min", start.time = "2023-10-28", end.time = "2023-11-03")

# Identify breaks of structure
breaks.list = identify_break_of_structure(df = btc_data)
print(breaks.list)
