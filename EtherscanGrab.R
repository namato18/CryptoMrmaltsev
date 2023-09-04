library(etherscanr)

api.key = "HKYWSDAZQS14QKVB7KY1AKTQURYMEPFFZU"

etherscanr::etherscan_set_api_key(api.key)

etherscan_balance(accounts = "0x00000000219ab540356cBB839Cbe05303d7705Fa")
