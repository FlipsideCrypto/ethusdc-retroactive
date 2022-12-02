library(dplyr)
options(scipen = 10)

ethusdc_results <- readRDS("ethusdc_results.rds")

n = nrow(ethusdc_results)

table(ethusdc_results$pnl_usd_terms > 0)/n
table(ethusdc_results$pnl_eth_terms > 0)/n

table(ethusdc_results$pnl_usd_terms > 0 & ethusdc_results$pnl_eth_terms > 0)/n

table(ethusdc_results$strategy_usd_terms > ethusdc_results$hodl_usd_terms)/n

table(ethusdc_results$strategy_usd_terms > ethusdc_results$hodl_usd_terms & 
        ethusdc_results$strategy_eth_terms > ethusdc_results$hodl_eth_terms)/n




