library(htmltools)
library(xml2)
library(purrr)
library(rvest)
library(lubridate)
library(quantmod)
library(tictoc)

api.key.av = "1RF2MSZAZY8XHUGV"

possible_json = possibly(.f = jsonlite::fromJSON, otherwise = 'ERROR' )
possibly_parse_date_time = possibly(.f = parse_date_time, otherwise = "All Day")
possibly_s3read_using = possibly(s3read_using, otherwise = "ERROR")


# Using the AV API --------------------------------

Get_News_Sentiment = function(ticker = NULL,topic = NULL,time_from = NULL,time_to = NULL,sort = "LATEST",limit = 1000,api.key = api.key.av){
  ticker = "BTCUSDT"
  
  base.url = paste0("https://www.alphavantage.co/query?function=NEWS_SENTIMENT")
  
  ticker.url = paste0("&",ticker)
  
  full.url = paste0(base.url, ticker.url,"&apikey=",api.key.av)
  
  test_get = httr::GET(full.url)
  
  test_get$status_code
  
  test = rawToChar(test_get$content)
  
  test = possible_json(test, flatten = TRUE)
  df = test$feed
}


# 2007-08-05

# Webscraping ForexFactory --------------------------------
ts.test = seq(ymd('2007-08-05'),ymd('2023-09-24'),by='weeks')

months.test = month(ts.test)
months.abr = month.abb

months.test = months.abr[months.test]

days.test = day(ts.test)
years.test = year(ts.test)

for(j in 1:length(ts.test)){
  j=1
  url = paste0("https://www.forexfactory.com/calendar?week=", ts.test[j])
  
}

page = read_html(url)

date = page %>% html_nodes(".date") %>% html_text()
time = (page %>% html_nodes(".calendar__time") %>% html_text())[-1]
currency = (page %>% html_nodes(".calendar__currency") %>% html_text())[-1]
impact = page %>% html_nodes(".calendar__impact") %>% html_children() %>% html_attrs()
event.title = page %>% html_nodes(".calendar__event-title") %>% html_text()
actual = (page %>% html_nodes(".calendar__actual") %>% html_text())[-1]
forecast = (page %>% html_nodes(".calendar__forecast") %>% html_text())[-1]
previous = (page %>% html_nodes(".calendar__previous") %>% html_text())[-1]

df = data.frame(time = time,
           currency = currency,
           event.title = event.title,
           actual = actual,
           forecast = forecast,
           previous = previous
           )
df$date = NA
df$date[1] = date[1]

for(i in 2:nrow(df)){
  if(df$time[i]==""){
    df$time[i] = df$time[i-1]
  }
}

x = parse_date_time(df$time, "%I:%M%p")

date.counter = 2

for(i in 2:length(x)){
  if(is.na(x[i])){
    next()
  }
  if((x[i] < x[i-1]) == TRUE){
    df$date[i] = date[date.counter]
    date.counter = date.counter + 1
  }
}
# --------------------------------

df = possibly_s3read_using(FUN = readRDS, bucket = "cryptomlbucket/ForexFactoryData", object = "df_apr1.2007.rds")

test.dates = parse_date_time(df$date, "%B %d %Y")

test.datetimes = paste0(df$date, " ", df$time)

test.datetimes = parse_date_time(test.datetimes, "%B %d %Y %I:%M%p")

test.datetimes

x = riingo::riingo_fx_prices(ticker = "audusd", start_date = "2020-01-01", resample_frequency = "30min")


