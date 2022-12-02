library(dplyr)
options(scipen = 10)

ethbtc_results <- readRDS("ethbtc/ethbtc_results.rds")

n = nrow(ethbtc_results)

table(ethbtc_results$pnl_btc_terms > 0)/n
table(ethbtc_results$pnl_eth_terms > 0)/n

table(ethbtc_results$pnl_btc_terms > 0 & ethbtc_results$pnl_eth_terms > 0)/n

table(ethbtc_results$strat_btc_terms > ethbtc_results$hodl_btc_terms)/n

table(ethbtc_results$strat_btc_terms > ethbtc_results$hodl_btc_terms & 
        ethbtc_results$strat_eth_terms > ethbtc_results$hodl_eth_terms)/n




