library(rvest)
library(dplyr)
library(xml2)
library(stringr)
library(htmltools)

tokens_html = read_html("https://etherscan.io/tokens")
tokens = tokens_html %>% html_nodes(".link-dark") %>%
  html_attr("href")
tokens = tokens[grep(pattern = "token",tokens)]
tokens = str_match(string = tokens, pattern = "token/(.*)")[,2]

names = tokens_html %>% html_nodes(".fw-medium") %>% html_text()
names = names[1:50]

token.names.df = data.frame(cbind(names,tokens))
  

# GetTopAccounts = function(){
#   pages = c("https://etherscan.io/accounts", "https://etherscan.io/accounts/2","https://etherscan.io/accounts/3","https://etherscan.io/accounts/4")
#   
#   top.accounts = c()
#   
#   for(i in 1:length(pages)){
#     page = read_html(pages[1])
#     
#     x = page %>% html_nodes(".js-clipboard") %>% html_attr("data-clipboard-text")
#     top.accounts = c(top.accounts,x)
#   }
#   return(top.accounts)
# }



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


  
  #get specific users transactions
  page2 = read_html(paste0("https://etherscan.io/token/generic-tokentxns2?m=dim&contractAddress=0xdac17f958d2ee523a2206206994597c13d831ec7&a=0xf977814e90da44bfa03b6295a0616a897441acec&sid=9d40509de6ec51ffb875259e1234d427&p=1"))
  x2 = page2 %>% html_nodes("a") %>% html_text()


