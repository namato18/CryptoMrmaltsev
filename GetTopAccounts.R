library(rvest)
library(dplyr)

GetTopAccounts = function(){
  pages = c("https://etherscan.io/accounts", "https://etherscan.io/accounts/2","https://etherscan.io/accounts/3","https://etherscan.io/accounts/4")
  
  top.accounts = c()
  
  for(i in 1:length(pages)){
    page = read_html(pages[i])
    
    x = page %>% html_nodes(".js-clipboard") %>% html_attr("data-clipboard-text")
    top.accounts = c(top.accounts,x)
  }
  return(top.accounts)
}


accounts = GetTopAccounts()
