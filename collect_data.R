library(shroomDK)

# LP Actions 

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

# Collected Fees
 # Known Issue where Closure of positions mix withdrawn tokens as if they were collected fees
 # subtraction will be sorted out 
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

# Large swap history 
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

# Historical ETH prices at all LP_ACTIONS blocks

min_block = min(lp_actions$BLOCK_NUMBER) - 101

blocks = c(min_block, min_block + 1e6, 
           min_block + 2e6, min_block + 3e6, 
           min_block + 4e6)

eth_price <- list()
for(i in 1:length(blocks)){
eth_price[[i]]  <- get_eth_price(min_block = blocks[i],
                                 max_block = blocks[i] + 1e6, 
                                 api_key = readLines('api_key.txt'))
  
}

all_eth_prices <- do.call(rbind, eth_price)
all_eth_prices <- all_eth_prices[order(all_eth_prices$BLOCK_NUMBER),]

# R Save Format 
saveRDS(lp_actions, "lp_actions.rds")
saveRDS(fees, "fees.rds")
saveRDS(all_swaps, "all_swaps.rds")
saveRDS(unique(all_eth_prices), "eth_prices.rds")
