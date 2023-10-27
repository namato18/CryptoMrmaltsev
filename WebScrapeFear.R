library(rvest)
library(xml2)

url = "https://production.dataviz.cnn.io/index/fearandgreed/graphdata/2022-10-25"

test_get = httr::GET(url)

  test_get$status_code

  test = rawToChar(test_get$content)

  test = fromJSON(test, flatten = TRUE)
  
  test$fear_and_greed_historical

  df.fear.greed = test$fear_and_greed_historical$data
  
  df.fear.greed$date = as.POSIXct(df.fear.greed$x/1000, origin = "1970-01-01", tz = "UTC")
