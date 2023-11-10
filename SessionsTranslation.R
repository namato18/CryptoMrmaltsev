library(lubridate)

df = fetch_binance_data(symbol = "BTCUSDT", interval = "15min", start.time = "2023-10-30", end.time = Sys.Date())

#################################################################################
calculate_previous_week_high_low <- function(df, day){
  # day = wday("2023-10-30")
  # df = btc_data
  
  week_start = Sys.Date() - (day - 2)
  previous_week_end = week_start - days(1)
  previous_week_start = previous_week_end - days(6)
  
  previous_week_data = df %>%
    filter(df$Date >= previous_week_start & df$Date <= previous_week_end)
  
  prev_week_high = max(previous_week_data$High)
  prev_week_low = min(previous_week_data$Low)
  
  to.return = list(prev_week_high = prev_week_high,
                   prev_week_low = prev_week_low)
  
  return(to.return)
}

#################################################################################

calculate_monday_high_low <- function(df, day){
  day.num = wday(day)
  
  monday = day - (day.num - 2)
  
  monday_data = df %>%
    filter(Date >= monday & Date < monday + days(1))
  
  if(nrow(monday_data) == 0){
    to.return = list(monday.data.high = 0,
                     monday.data.low = 0)
  }else{
    to.return = list(monday.data.high = max(monday_data$High),
                     monday.data.low = min(monday_data$Low))
  }
  

  
  return(to.return)
  
}
#################################################################################

get_session_prices <- function(df, start_hour, duration_hours, day){
  # start_hour = 12
  # duration_hours = 2
  # day = as.Date("2023-11-05")
  
  session_start = day + hours(start_hour)
  session_end = session_start + hours(duration_hours)
  
  session_data = df %>%
    filter(Date >= session_start & Date <= session_end)
  
  if(nrow(session_data) == 0){
    to.return = list(open.price = 0,
                     close.price = 0)
  }else{
    open.price = session_data$Open[1]
    close.price = session_data$Close[nrow(session_data)]
    
    to.return = list(open.price = open.price,
                     close.price = close.price)
  }
  

  
  return(to.return)
}

#################################################################################

df = fetch_binance_data(symbol = "BTCUSDT", interval = "1hour", start.time = "2023-10-29", end.time = "2023-11-05")

date_range = seq(from = as.Date("2023-10-29"), to = as.Date("2023-11-05"), by = "days")


Date = as.Date(character(0))
`Sydney Session Open` = numeric(0)
`Sydney Session Close` = numeric(0)
`Tokyo Session Open` = numeric(0)
`Tokyo Session Close` = numeric(0)
`London Session Open` = numeric(0)
`London Session Close` = numeric(0)
`New York Session Open` = numeric(0)
`New York Session Close` = numeric(0)
`New York Kill Zone Open` = numeric(0)
`New York Kill Zone Close` = numeric(0)
`London Open Kill Zone Open` = numeric(0)
`London Open Kill Zone Close` = numeric(0)
`London Close Kill Zone Open` = numeric(0)
`London Close Kill Zone Close` = numeric(0)
`Previous Day High` = numeric(0)
`Daily High` = numeric(0)
`Daily Low` = numeric(0)
`Previous Day Low` = numeric(0)
`Monday High` = numeric(0)
`Monday Low` = numeric(0)
`Previous Week High` = numeric(0)
`Previous Week Low` = numeric(0)

sessions <- list(
  "Sydney" = list(start_hour = 21, duration = 9),
  "Tokyo" = list(start_hour = 0, duration = 9),
  "London" = list(start_hour = 7, duration = 9),
  "New York" = list(start_hour = 13, duration = 9)
)

kill_zones <- list(
  "New York Kill Zone" = list(start_hour = 12, duration = 2),
  "London Open Kill Zone" = list(start_hour = 7, duration = 2),
  "London Close Kill Zone" = list(start_hour = 16, duration = 2)
)

for(i in 1:length(date_range)){
  Date[i] = date_range[i]
  
  x = calculate_previous_week_high_low(df, date_range[i])
  `Previous Week High`[i] = x$prev_week_high
  `Previous Week Low`[i] = x$prev_week_low
  
  x = calculate_monday_high_low(df, date_range[i])
  `Monday High` = x$monday.data.high
  `Monday Low` = x$monday.data.low
  
  for(j in 1:length(sessions)){

    x = get_session_prices(df, sessions[[j]]$start_hour, sessions[[j]]$duration, date_range[i])
    
    assign(paste0(labels(sessions)[j]," Session Open"), c(get(paste0(labels(sessions)[j]," Session Open")), x$open.price))
    assign(paste0(labels(sessions)[j]," Session Close"), c(get(paste0(labels(sessions)[j]," Session Close")), x$close.price))
  }
  
  for(j in 1:length(kill_zones)){

    x = get_session_prices(df, kill_zones[[j]]$start_hour, kill_zones[[j]]$duration, date_range[i])
    
    assign(paste0(labels(kill_zones)[j]," Open"), c(get(paste0(labels(kill_zones)[j]," Open")), x$open.price))
    assign(paste0(labels(kill_zones)[j]," Close"), c(get(paste0(labels(kill_zones)[j]," Close")), x$close.price))
  }
  
  
}

results_df = data.frame(`Sydney Session Open` = `Sydney Session Open`,
                        `Sydney Session Close` = `Sydney Session Close`,
                        `Tokyo Session Open` = `Tokyo Session Open`,
                        `Tokyo Session Close` = `Tokyo Session Close`,
                        `New York Session Open` = `New York Session Open`,
                        `New York Session Close` = `New York Session Close`,
                        `London Session Open` = `London Session Open`,
                        `London Session Close` = `London Session Close`)





