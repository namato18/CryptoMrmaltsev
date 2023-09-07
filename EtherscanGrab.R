library(etherscanr)
library(purrr)
library(lubridate)

options(scipen=999)

possible_json = possibly(.f = jsonlite::fromJSON, otherwise = 'ERROR' )

# grab top accounts
source("GetTopAccounts.R")

top.accounts = GetTopAccounts()

api.key = "HKYWSDAZQS14QKVB7KY1AKTQURYMEPFFZU"

etherscanr::etherscan_set_api_key(api.key)

current = as.POSIXct(Sys.time())
current - (60*60*24*7)

# get block by timestamp
url = paste0("https://api.etherscan.io/api",
             "?module=block",
             "&action=getblocknobytime",
             "&timestamp=",round(as.numeric(as.POSIXct(Sys.time())- (60*60*24*7)),0),
             "&closest=before",
             "&apikey=YourApiKeyToken"
)
test_get = httr::GET(url)

test_get$status_code

test = rawToChar(test_get$content)

test = possible_json(test, flatten = TRUE)
df.block.time = test$result

###############
# get token transactions of account
url = paste0("https://api.etherscan.io/api?module=account&action=tokentx",
   "&contractaddress=",token.names.df$tokens[3],
   "&address=",user.coin.holdings$holder.wallet[3],
   "&page=1",
   "&offset=10000",
   "&startblock=",df.block.time,
  "&endblock=27025780",
   "&sort=asc",
   "&apikey=",api.key)

test_get = httr::GET(url)

test_get$status_code

test = rawToChar(test_get$content)

test = possible_json(test, flatten = TRUE)
df = test$result

df$datetime = as_datetime(as.numeric(df$timeStamp))
df$ActualValue = round(as.numeric(df$value) / as.numeric(df$tokenDecimal), digits = 0)

ts.grab = paste0("https://api.etherscan.io/api",
"?module=block",
"&action=getblocknobytime",
"&timestamp=1578638524",
"&closest=before",
"&apikey=YourApiKeyToken")


