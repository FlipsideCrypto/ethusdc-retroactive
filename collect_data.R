library(shroomDK)

lp_actions <- auto_paginate_query(
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
                    WHERE POOL_ADDRESS = '0x88e6a0c2ddd26feeb64f039a2c41296fcb3f5640' AND 
                    BLOCK_NUMBER <= 15576600
                    ORDER BY BLOCK_NUMBER DESC
  ",
  api_key = readLines("api_key.txt")
)

fees <- auto_paginate_query(
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
                    WHERE POOL_ADDRESS = '0x88e6a0c2ddd26feeb64f039a2c41296fcb3f5640' AND 
                    BLOCK_NUMBER <= 15576600
                    ORDER BY BLOCK_NUMBER DESC
  ",
  api_key = readLines("api_key.txt")
)


swap_spreads <- c(12370000, 13370000, 14370000, 14870000, 15370000, 15576600)
swaps <- list()
for(i in 1:5){
  swap_query <- "
  SELECT
  BLOCK_NUMBER, BLOCK_TIMESTAMP,
  TX_HASH,
  TICK, AMOUNT0_ADJUSTED, AMOUNT1_ADJUSTED,
  PRICE_1_0, PRICE_0_1
  FROM     ethereum.uniswapv3.ez_swaps
                    WHERE POOL_ADDRESS = '0x88e6a0c2ddd26feeb64f039a2c41296fcb3f5640' AND 
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

# R Save Format 
saveRDS(lp_actions, "lp_actions.rds")
saveRDS(fees, "fees.rds")
saveRDS(all_swaps, "all_swaps.rds")

