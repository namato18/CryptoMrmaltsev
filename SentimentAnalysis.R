library(riingo)

api.key = "1RF2MSZAZY8XHUGV"


x = riingo_crypto_prices("BTCUSDT", start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = "1hour")
x = x[,c(1,4:9)]

start.time = format(Sys.time() - lubridate::days(3), format = "%Y%m%dT%H%M")


full.url = paste0("https://www.alphavantage.co/query?function=NEWS_SENTIMENT&limit=1000&time_from=",start.time,"&apikey=",api.key)
full.url = "'https://www.alphavantage.co/query?function=NEWS_SENTIMENT&tickers=AAPL&apikey=demo'"
test_get = httr::GET(full.url)

test_get$status_code

test = rawToChar(test_get$content)

test = possible_json(test, flatten = TRUE)
df = test$feed


