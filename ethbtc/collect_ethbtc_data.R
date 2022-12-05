library(shroomDK)
library(zoo) # infill NAs & rolling Median
source("../key_functions.R")


# Swaps 

# Large swap history 
swap_spreads <- c(12360000, 13370000, 14370000, 14870000, 15370000, 15576600)
swaps <- list()
for(i in 1:5){
  swap_query <- "
  SELECT
  BLOCK_NUMBER, BLOCK_TIMESTAMP,
  TX_HASH,
  TICK, AMOUNT0_ADJUSTED, AMOUNT1_ADJUSTED,
  PRICE_1_0, PRICE_0_1
  FROM     ethereum.uniswapv3.ez_swaps
                    WHERE POOL_ADDRESS = '0xcbcdf9626bc03e24f779434178a73a0b4bad62ed' AND 
                    BLOCK_NUMBER > min_block
                    AND 
                    BLOCK_NUMBER <= max_block
                    ORDER BY BLOCK_NUMBER DESC
  "
  swap_query <- gsub("min_block", swap_spreads[i], swap_query)
  swap_query <- gsub("max_block", swap_spreads[i+1], swap_query)
  
  swaps[[i]] <- auto_paginate_query(
    query = swap_query,
    api_key = readLines("api_key.txt")
  )
}

all_swaps <- do.call(rbind, swaps)

# LP Actions 

ethbtc_lp_actions <- auto_paginate_query(
  query = "
  SELECT 
  BLOCK_NUMBER, BLOCK_TIMESTAMP,
  TX_HASH, ACTION,
  NF_TOKEN_ID,
   AMOUNT0_ADJUSTED, AMOUNT1_ADJUSTED,
  LIQUIDITY,
  TOKEN0_SYMBOL, TOKEN1_SYMBOL,
  TICK_LOWER, TICK_UPPER,
  PRICE_LOWER_0_1, PRICE_UPPER_0_1,
  LIQUIDITY_PROVIDER,
  NF_POSITION_MANAGER_ADDRESS
  FROM ethereum.uniswapv3.ez_lp_actions
                    WHERE POOL_ADDRESS = '0xcbcdf9626bc03e24f779434178a73a0b4bad62ed' AND 
                    BLOCK_NUMBER <= 15576600
                    ORDER BY BLOCK_NUMBER DESC
  ",
  api_key = readLines("api_key.txt")
)


# Collected Fees
# Known Issue where Closure of positions mix withdrawn tokens as if they were collected fees
# subtraction will be sorted out 
ethbtc_fees <- auto_paginate_query(
  query = "
  SELECT
  BLOCK_NUMBER, BLOCK_TIMESTAMP,
  TX_HASH, NF_TOKEN_ID, 
  AMOUNT0_ADJUSTED, AMOUNT1_ADJUSTED,
  TICK_LOWER, TICK_UPPER,
  PRICE_LOWER, PRICE_UPPER,
  LIQUIDITY_PROVIDER,
  NF_POSITION_MANAGER_ADDRESS
  FROM ethereum.uniswapv3.ez_position_collected_fees
                    WHERE POOL_ADDRESS = '0xcbcdf9626bc03e24f779434178a73a0b4bad62ed' AND 
                    BLOCK_NUMBER <= 15576600
                    ORDER BY BLOCK_NUMBER DESC
  ",
  api_key = readLines("api_key.txt")
)

# Historical BTC prices at all LP_ACTIONS blocks

min_block = min(ethbtc_lp_actions$BLOCK_NUMBER) - 101

blocks = c(min_block, min_block + 1e6, 
           min_block + 2e6, min_block + 3e6, 
           min_block + 4e6)

ethbtc_price <- list()
for(i in 1:length(blocks)){
  ethbtc_price[[i]]  <- get_ethbtc_price(min_block = blocks[i],
                                   max_block = blocks[i] + 1e6, 
                                   api_key = readLines('api_key.txt'))
  
}

all_ethbtc_prices <- do.call(rbind, ethbtc_price)
all_ethbtc_prices <- all_ethbtc_prices[order(all_ethbtc_prices$BLOCK_NUMBER),]

# if a block has no trades, infill the BLOCK_NUMBER and persist the most recent 
# ETH Weighted Average Price, with 0 VOLUME and 0 NUM_SWAPS
infill <- data.frame(
  BLOCK_NUMBER = min(all_ethbtc_prices$BLOCK_NUMBER):max(all_ethbtc_prices$BLOCK_NUMBER)
)

filled_ethbtc_prices <- merge(all_ethbtc_prices, infill, all.x = TRUE, all.y = TRUE)

filled_ethbtc_prices[is.na(filled_ethbtc_prices$"BTC_VOLUME"), c("BTC_VOLUME","NUM_SWAPS")] <- 0

# Improves analysis speed to front-load these calculations and is more smoothed 
filled_ethbtc_prices$BTC_WAVG_PRICE <- zoo::na.locf(filled_ethbtc_prices$BTC_WAVG_PRICE)
BTC_MARKET_PRICE <- zoo::rollmedian(x = filled_ethbtc_prices$BTC_WAVG_PRICE, k = 99, align = "left")
diff_median <- nrow(filled_ethbtc_prices) - length(BTC_MARKET_PRICE)
BTC_MARKET_PRICE <- c(filled_ethbtc_prices$BTC_WAVG_PRICE[1:diff_median], BTC_MARKET_PRICE)

filled_ethbtc_prices$BTC_MARKET_PRICE <- BTC_MARKET_PRICE

# R Save Format 

saveRDS(all_swaps, "ethbtc_swaps.rds")
saveRDS(ethbtc_lp_actions, "ethbtc_lp_actions.rds")
saveRDS(ethbtc_fees, "ethbtc_fees.rds")
saveRDS(filled_ethbtc_prices, "ethbtc_prices.rds")
