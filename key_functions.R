library(gmp) # big int math

fee_correction <- function(id_fees, id_lp_actions){
  
  # fee-collection problem only occurs in decrease_liquidity
  # sometimes in the same transaction the same exact liquidity is added and immediately removed
  # occurs in poorly written NF_POSITION_MANAGERS.
  
  id_lp_actions <- id_lp_actions %>% filter(ACTION == "DECREASE_LIQUIDITY")
  
  fee_tx_in_lp = (id_fees$TX_HASH %in% id_lp_actions$TX_HASH)
  lp_tx_in_fee = (id_lp_actions$TX_HASH %in% id_fees$TX_HASH)
  amount_cols = c("AMOUNT0_ADJUSTED", "AMOUNT1_ADJUSTED")
  
  # if there are no tx hashes in both tables move on 
  if(sum(fee_tx_in_lp) == 0){
    return(id_fees)
    
  } else { 
    # if it perfectly lines up that ALL tx hashes in 
    # both tables have fees collected double counting withdrawals;
    # fix by subtracting the double counting
    # note: it is assumed transactions are ordered by block number for alignment to work
    if(sum(fee_tx_in_lp) == sum(lp_tx_in_fee)){
      if(mean(id_fees[fee_tx_in_lp, amount_cols] >= id_lp_actions[lp_tx_in_fee, amount_cols]) == 1){
        id_fees[fee_tx_in_lp, amount_cols] <- {
          id_fees[fee_tx_in_lp, amount_cols] - id_lp_actions[lp_tx_in_fee, amount_cols] 
        }
      } else { 
        for(i in id_fees$TX_HASH[fee_tx_in_lp]){
          if(
            mean(
              id_fees[id_fees$TX_HASH == i, amount_cols] >= 
              id_lp_actions[id_lp_actions$TX_HASH == i, amount_cols]
            ) == 1
          ){
            id_fees[id_fees$TX_HASH == i, amount_cols] <- {
              id_fees[id_fees$TX_HASH == i, amount_cols]  - 
                id_lp_actions[id_lp_actions$TX_HASH == i, amount_cols]
            }
          } else next()
        }
        }
    } else { 
      # else go 1 by one to overlapping tx and check again
      
      for(i in id_fees$TX_HASH[fee_tx_in_lp]){
        if(
          mean(
            id_fees[id_fees$TX_HASH == i, amount_cols] >= 
            id_lp_actions[id_lp_actions$TX_HASH == i, amount_cols]
          ) == 1
        ){
          id_fees[id_fees$TX_HASH == i, amount_cols] <- {
            id_fees[id_fees$TX_HASH == i, amount_cols]  - 
              id_lp_actions[id_lp_actions$TX_HASH == i, amount_cols]
          }
        } else next()
      }
    }
    
    return(id_fees)
  }
}

accounting <- function(id_lp_actions, id_fees, price_col){
  
  # cost basis negative for subtraction 
  cost_basis <- id_lp_actions %>% filter(ACTION == "INCREASE_LIQUIDITY") %>% 
    mutate(
      token0 = -1*AMOUNT0_ADJUSTED,
      token1 = -1*AMOUNT1_ADJUSTED,
      accounting = "cost_basis"
    ) %>% 
    select(BLOCK_NUMBER, accounting, token0, token1, all_of(price_col))
  
  withdrawals <- id_lp_actions %>% filter(ACTION == "DECREASE_LIQUIDITY") %>% 
    mutate(
      token0 = AMOUNT0_ADJUSTED,
      token1 = AMOUNT1_ADJUSTED,
      accounting = "withdrawal"
    ) %>% 
    select(BLOCK_NUMBER, accounting, token0, token1, all_of(price_col))
  
  fees <- data.frame(
    BLOCK_NUMBER = max(withdrawals$BLOCK_NUMBER),
    accounting = "fee revenue",
    token0 = sum(id_fees$AMOUNT0_ADJUSTED),
    token1 = sum(id_fees$AMOUNT1_ADJUSTED)
  )
  
  # price fees at position closure, i.e., last withdrawal
  fees[[price_col]] <- tail(withdrawals[[price_col]], n = 1)
  
  accounting_tbl <- rbind(cost_basis, withdrawals, fees)
  
  return(accounting_tbl)
  
}

pnl <- function(id_accounting, t0col, t1col, price_col, price_base = "t1/t0"){
  
  # allow user to choose the base that is easier to read
  # e.g., usdc/eth may be easier to read than eth/usdc; which means t0/t1 instead of t1/t0
  # while ETH/BTC may be easier than BTC/ETH; which is t1/t0 the default!
  
  if(price_base == 't1/t0'){
    pnl = data.frame(
      pnl_t0_terms = sum(id_accounting$token0, (id_accounting$token1/id_accounting[[price_col]])),
      pnl_t1_terms = sum(id_accounting$token1, (id_accounting$token0*id_accounting[[price_col]]))
    )
  } else if(price_base == 't0/t1'){
    pnl = data.frame(
      pnl_t0_terms = sum(id_accounting$token0, (id_accounting$token1*id_accounting[[price_col]])),
      pnl_t1_terms = sum(id_accounting$token1, (id_accounting$token0/id_accounting[[price_col]]))
    )
  } else {
    stop("price_base must be 't1/t0' (default) or 't0/t1'")
  }
  
  
  colnames(pnl) <- c(t0col, t1col)
  
  return(pnl)
}

hodl_reference <- function(id_accounting, t0col, t1col, price_col, price_base = "t1/t0"){
  
  price_at_close_block = id_accounting[[price_col]][id_accounting$BLOCK_NUMBER == max(id_accounting$BLOCK_NUMBER)][1] 
  
  if(price_base == 't1/t0'){
    hodl <- id_accounting %>% 
      filter(accounting == 'cost_basis') %>% 
      summarise(
        hodl_t0_terms = abs(sum(token0) + sum(token1 / price_at_close_block)),
        hodl_t1_terms = abs(sum(token1) + sum(token0 * price_at_close_block)),
      )
  } else if(price_base == 't0/t1'){
    hodl <- id_accounting %>% 
      filter(accounting == 'cost_basis') %>% 
      summarise(
        hodl_t0_terms = abs(sum(token0) + sum(token1 * price_at_close_block)),
        hodl_t1_terms = abs(sum(token1) + sum(token0 / price_at_close_block)),
      )
  } else {
    stop("price_base must be 't1/t0' (default) or 't0/t1'")
  }
  
  colnames(hodl) <- c(t0col, t1col)
  
  return(hodl)
}

strategy_reference <- function(id_accounting, t0col, t1col, price_col, price_base = "t1/t0"){
  
  price_at_close_block = id_accounting[[price_col]][id_accounting$BLOCK_NUMBER == max(id_accounting$BLOCK_NUMBER)][1] 
  
  if(price_base == 't1/t0'){
    strat <- id_accounting %>% 
      filter(accounting != 'cost_basis') %>% 
      summarise(
        strategy_t0_terms = abs(sum(token0) + sum(token1 / price_at_close_block)),
        strategy_t1_terms = abs(sum(token1) + sum(token0 * price_at_close_block)),
      )
  } else if(price_base == 't0/t1'){
    strat <- id_accounting %>% 
      filter(accounting != 'cost_basis') %>% 
      summarise(
        strategy_t0_terms = abs(sum(token0) + sum(token1 * price_at_close_block)),
        strategy_t1_terms = abs(sum(token1) + sum(token0 / price_at_close_block)),
      )
  } else {
    stop("price_base must be 't1/t0' (default) or 't0/t1'")
  }
  
  colnames(strat) <- c(t0col, t1col)
  return(strat)
  
}

get_eth_price <- function(min_block, max_block, api_key){
  price_query <- {
    "
    with uniswap_ETH_stable_swaps AS (
SELECT * FROM ethereum.uniswapv3.ez_swaps 
WHERE 
POOL_ADDRESS IN (
'0x8ad599c3a0ff1de082011efddc58f1908eb6e6d8', -- ETH USDC 0.3%
'0x88e6a0c2ddd26feeb64f039a2c41296fcb3f5640', -- ETH USDC 0.05%
'0x4e68ccd3e89f51c3074ca5072bbac773960dfa36', -- ETH USDT 0.3%
'0x11b815efb8f581194ae79006d24e0d814b7697f6', -- ETH USDT 0.05%
'0x7bea39867e4169dbe237d55c8242a8f2fcdcc387', -- ETH USDC 1%
'0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8'   -- ETH DAI 0.3% 
) AND 
BLOCK_NUMBER >= _min_block_ AND 
BLOCK_NUMBER <= _max_block_
  ),
    
eth_stable_price AS (
SELECT BLOCK_NUMBER, BLOCK_TIMESTAMP, 
IFF(TOKEN1_SYMBOL = 'WETH', ABS(DIV0(AMOUNT0_ADJUSTED, AMOUNT1_ADJUSTED)), 
           ABS(DIV0(AMOUNT1_ADJUSTED, AMOUNT0_ADJUSTED))
   ) as eth_stable_trade_price,
  IFF(TOKEN1_SYMBOL = 'WETH', ABS(AMOUNT1_ADJUSTED), ABS(AMOUNT0_ADJUSTED)) as eth_volume,
IFF(TOKEN1_SYMBOL = 'WETH', TOKEN0_SYMBOL, TOKEN1_SYMBOL) as stable           
           FROM uniswap_eth_stable_swaps
   		 WHERE ABS(AMOUNT0_ADJUSTED) > 1e-8 AND ABS(AMOUNT1_ADJUSTED) > 1e-8
),

eth_block_price AS ( 
SELECT BLOCK_NUMBER, BLOCK_TIMESTAMP, 
  div0(SUM(eth_stable_trade_price * eth_volume),sum(eth_volume)) as eth_wavg_price,
  SUM(eth_volume) as eth_volume,
  COUNT(*) as num_swaps
    FROM eth_stable_price
    GROUP BY BLOCK_NUMBER, BLOCK_TIMESTAMP
)

SELECT * FROM eth_block_price
ORDER BY BLOCK_NUMBER DESC
    "
  }
  
  price_query <- gsub("_min_block_", min_block, price_query)
  price_query <- gsub("_max_block_", max_block, price_query)
  
  prices = auto_paginate_query(price_query, api_key = api_key)
}

get_ethbtc_price <- function(min_block, max_block, api_key){
  
 price_query <- {
    "
    with uniswap_btc_eth_swaps AS (
SELECT * FROM ethereum.uniswapv3.ez_swaps WHERE POOL_ADDRESS IN (
'0xcbcdf9626bc03e24f779434178a73a0b4bad62ed', -- wbtc ETH 0.3%
'0x4585fe77225b41b697c938b018e2ac67ac5a20c0' -- wbtc ETH 0.05% 
    ) AND 
BLOCK_NUMBER >= _min_block_ AND 
BLOCK_NUMBER <= _max_block_
  ),
    
btc_eth_price AS (
SELECT BLOCK_NUMBER, BLOCK_TIMESTAMP, 
IFF(TOKEN1_SYMBOL = 'WBTC', ABS(DIV0(AMOUNT0_ADJUSTED, AMOUNT1_ADJUSTED)), 
           ABS(DIV0(AMOUNT1_ADJUSTED, AMOUNT0_ADJUSTED))
   ) as btc_eth_trade_price,
  IFF(TOKEN1_SYMBOL = 'WBTC', ABS(AMOUNT1_ADJUSTED), ABS(AMOUNT0_ADJUSTED)) as btc_volume,
IFF(TOKEN1_SYMBOL = 'WBTC', TOKEN0_SYMBOL, TOKEN1_SYMBOL) as eth           
           FROM uniswap_btc_eth_swaps
   		 WHERE ABS(AMOUNT0_ADJUSTED) > 1e-8 AND ABS(AMOUNT1_ADJUSTED) > 1e-8
),

btc_block_price AS ( 
SELECT BLOCK_NUMBER, BLOCK_TIMESTAMP, 
  div0(SUM(btc_eth_trade_price * btc_volume),sum(btc_volume)) as btc_wavg_price,
  SUM(btc_volume) as btc_volume,
  COUNT(*) as num_swaps
    FROM btc_eth_price
    GROUP BY BLOCK_NUMBER, BLOCK_TIMESTAMP
)

SELECT * FROM btc_block_price
ORDER BY BLOCK_NUMBER DESC
    "
  }
  
  price_query <- gsub("_min_block_", min_block, price_query)
  price_query <- gsub("_max_block_", max_block, price_query)
  
  prices = auto_paginate_query(price_query, api_key = api_key)
}

market_eth_price_at_block <- function(eth_prices, block){
  eth_prices[eth_prices$BLOCK_NUMBER == block, "ETH_MARKET_PRICE"]
}

# Assumes price is desired in X/Y format, e.g., WBTC/ETH.
tick_to_price <- function(tick, decimal_adjustment = 1, yx = FALSE){
  
  p <- sqrt(1.0001)^(2*tick) 
  if(yx == TRUE){
    p <- p^-1
  }
  
  p <- p * decimal_adjustment
  
  return(p)
}

# Assumes price is in X/Y format, e.g., WBTC/ETH
get_closest_tick <- function(desired_price, tick_spacing = 60, decimal_adjustment = 1, yx = FALSE){
  # base = sqrt(1.0001)
  # y = sqrt(price / decimal_adjustment)
  # base^tick = y
  # tick = log(y, base)
  # price = decimal_adjustment*(base^tick)^2
  
  # assumes x/y pricing    
  
  r <- list(
    desired_price = desired_price,
    actual_price = NULL,
    tick = NULL
  )
  
  initial_tick <- log( sqrt(desired_price / decimal_adjustment), sqrt(1.0001) )  
  
  if(initial_tick %% tick_spacing == 0){
    r$actual_price <- desired_price # exact match
    r$tick <- initial_tick
  } else { 
    final_tick <- round(initial_tick / tick_spacing)*tick_spacing
    r$tick <- final_tick 
    r$actual_price <- sqrt(1.0001)^(2*final_tick) * decimal_adjustment
  }
  
  if(yx == TRUE){
    r$note <- "v3 assumes X/Y format, your tick may need to be made negative, or price inversed."
  }
  
  return(r)
  
}

match_tokens_to_range <- function(x = NULL, y = NULL, P, pa, pb, yx = TRUE){ 
  
  if(is.null(x) & is.null(y)){
    stop("amount of token x OR amount of token y must be provided")
  }
  
  if(!is.null(x) & !is.null(y)){
    stop("one of amount x or amount y should be unknown, NULL")
  }
  
  r <- list(
    amount_x = NULL,
    amount_y = NULL,
    current_price = P,
    min_price = pa,
    max_price = pb
  )
  
  if(!is.null(y)){
    r$amount_y <- y
  } else if (!is.null(x)){ 
    r$amount_x <- x
  }
  
  # if x is provided and prices are in X/Y format 
  if(!is.null(x) & yx == FALSE){
    
    Lx = x * sqrt(P * pb) / ( sqrt(pb) - sqrt(P) )
    y_ = Lx * ( sqrt(P) - sqrt(pa) )
    r$amount_y = y_^-1
    
  }
  
  # if y is provided and prices are in X/Y format
  if(!is.null(y) & yx == FALSE){
    Ly = y * sqrt(P * pb) / ( sqrt(pb) - sqrt(P) )
    x_ = Ly * ( sqrt(P) - sqrt(pa) )
    
    r$amount_x <- x_
  }
  
  
  # if x is provided and prices are in Y/X format 
  # use swap, inverse, and recursion
  if(!is.null(x) & yx == TRUE){
    r$amount_y <- match_tokens_to_range(x = x,
                                        P = P^-1,
                                        pa = pb^-1, 
                                        pb = pa^-1, yx = FALSE)$amount_y
  }
  
  # if y is provided and prices are in Y/X format 
  # use swap, inverse, and recursion
  if(!is.null(y) & yx == TRUE){
    r$amount_x <- match_tokens_to_range(y = y,
                                        P = P^-1,
                                        pa = pb^-1, 
                                        pb = pa^-1, yx = FALSE)$amount_x
  }
  
  return(r)
  
}


price_all_tokens <- function(x, y, P, pa = NULL, pb = NULL, yx = TRUE){
  
  if(is.null(pa) & is.null(pb)){
    stop("min price pa OR max price pb must be provided")
  }
  
  if(!is.null(pa) & !is.null(pb)){
    stop("one of min price or max price should be unknown, NULL")
  }
  
  r <- list(
    amount_x = x,
    amount_y = y,
    current_price = P,
    min_price = NULL,
    max_price = NULL
  )
  
  if(!is.null(pa)){
    r$min_price <- pa
  } else { 
    r$max_price <- pb
  }
  
  # if min_price pa is given and prices are in Y/X format
  if(!is.null(pa) & yx == TRUE){
    
    f1 <- (y^2)/(x^2)
    f2 <- sqrt(pa) - sqrt(P) + (y/(sqrt(P)*x))
    pb <- f1 * (f2)^-2
    
    r$max_price <- pb
    
  }
  
  # if min_price pa is NOT given and prices are in Y/X format
  if(is.null(pa) & yx == TRUE){
    f1 <- y / (sqrt(pb) * x)
    f2 <- y / (sqrt(P) * x)
    pa <- (f1 + sqrt(P) - f2)^2
    r$min_price <- pa
  }
  
  # if min_price pa is given and prices are in X/Y format
  # use inverse and recursion
  if(!is.null(pa) & yx == FALSE){
    r$max_price <- price_all_tokens(x = x, y = y,
                                    P = P^-1, pb = pa^-1, yx = TRUE)$min_price^-1
  }
  
  # if min_price is NOT given and prices are in X/Y format
  # use inverse and recursion 
  if(is.null(pa) & yx == FALSE){
    r$min_price <- price_all_tokens(x = x, y = y,
                                    P = P^-1, pa = pb^-1, yx = TRUE)$max_price^-1
  }
  
  return(r)
}


price_to_sqrtpx96 <- function(P){
  # Uniswap stores prices as square roots in 64.96 (64 bits integer, 96 bit fractional)
  # assume sqrt price is a rational and use gmp big integer
  
  gmp::as.bigq(sqrt(P)) * gmp::as.bigz(2)^96
  
}

sqrtpx96_to_price <- function(sqrtpX96){
  p <- as.bigq(sqrtpX96)/(as.bigz(2)^96)
  
  # expect a small amount of precision loss, it's unavoidable.
  as.numeric(p^2)
}


get_liquidity <- function(x, y, P, pa, pb, yx = TRUE){ 
  
  # if prices are in Y/X format, invert them and act as X/Y
  if(yx == FALSE){
    return(get_liquidity(x, y, P = P^-1, pa = pb^-1, pb = pa^-1, yx = TRUE))
  }
  
  # include minimum of Lx and Ly as done in contract
  
  # formal math in Uni v3 implemented w/ sqrt px96 price formats 
  
  getLiq_amount0 <- function(mintickx96, maxtickx96, amount0){
    if( mintickx96 >  maxtickx96){
      temp = mintickx96
      mintickx96 <- maxtickx96
      maxtickx96 <- temp
    }
    intermediate = mintickx96*maxtickx96/as.bigz(2^96)
    
    return(amount0*intermediate / (maxtickx96 - mintickx96))
  }
  
  getLiq_amount1 <- function(mintickx96, maxtickx96, amount1){
    if( mintickx96 >  maxtickx96){
      temp = mintickx96
      mintickx96 <- maxtickx96
      maxtickx96 <- temp
    }
    
    return(amount1*as.bigz(2^96)/(maxtickx96 - mintickx96))
  }
  
  getLiq <- function(current_pricex96, mintickx96, maxtickx96, amount0, amount1){
    if( mintickx96 >  maxtickx96){
      temp = mintickx96
      mintickx96 <- maxtickx96
      maxtickx96 <- temp
    }
    
    if(current_pricex96 <= mintickx96){
      liq = getLiq_amount0(mintickx96, maxtickx96, amount0)
    } else if (current_pricex96 < maxtickx96){
      liq0 <- getLiq_amount0(current_pricex96, maxtickx96, amount0)
      liq1 <- getLiq_amount1(mintickx96, current_pricex96, amount1)
      
      #ifelse() cannot handle subassignment of big rationals 
      if(liq0 < liq1){
        liq <- liq0
      } else {
        liq <- liq1
      }
      
    } else { 
      liq = getLiq_amount1(mintickx96, maxtickx96, amount1)
    }
    
    return(liq)
    
  }
  
  # always return integer 
  Lx = as.bigz(getLiq_amount0(mintickx96 = price_to_sqrtpx96(pa), 
                              maxtickx96 = price_to_sqrtpx96(pb), 
                              amount0 = x))
  
  Ly = as.bigz(getLiq_amount1(mintickx96 = price_to_sqrtpx96(pa), 
                              maxtickx96 = price_to_sqrtpx96(pb), 
                              amount1 = y))
  
  L = as.bigz(getLiq(current_pricex96 = price_to_sqrtpx96(P), 
                     mintickx96 = price_to_sqrtpx96(pa),
                     maxtickx96 = price_to_sqrtpx96(pb),
                     amount0 = x,
                     amount1 = y))
  
  return(L)
}


swap_within_tick <- function(L, sqrtpx96, dx = NULL, dy = NULL, 
                             decimal_x = 1e18,
                             decimal_y = 1e18, 
                             fee = 0.003){
  # Change in price = Delta(Y) / L 
  # change in Inverse(Price) = Delta(X)/L
  
  # price in *square root* 64.96 form 
  L = as.bigz(L)
  P = as.bigz(sqrtpx96)
  c96 = (as.bigz(2)^96)
  # inverse price
  iP = P^-1
  
  # adjust real dx or dy to 96 int & adjust for fees
  if(!is.null(dx)){
    dxa <- as.bigq(dx) * (1 - fee) * decimal_x / c96
  }
  if(!is.null(dy)){
    dya <- as.bigq(dy) * (1 - fee) * c96 * decimal_y
  }
  
  r = list(
    liquidity = L,
    dx = NULL,
    dy = NULL,
    price1 = P,
    price2 = NULL,
    fee = NULL
  )
  
  if(is.null(dx) & is.null(dy)){
    stop("A change in x or y is required to use liquidity")
  }
  
  if(!is.null(dx) & !is.null(dy)){
    stop("Only 1 swap can be noted")
  }
  
  if(!is.null(dx)){
    # iP_new - iP = dx/L
    # iP_new = dx/L + iP
    
    iP_new = dxa/L + iP
    P_new = iP_new^-1
    
    # dy = (P_new - P)*L
    dya = (P_new - P)*L
    
    # convert back to real units 
    dyz = as.numeric( dya / c96 ) / decimal_y
    
    r$dx = dx * (1 - fee)
    r$dy = dyz
    r$price2 <- as.bigz(P_new)
    r$fee = fee * dx
    
  } else if(!is.null(dy)){
    # dy =  (P_new - P)*L
    # P_new = dy/L + P 
    
    P_new = dya/L + P 
    
    iP_new = P_new^-1
    
    dxa = (iP_new - iP)*L
    
    # convert to real units 
    dxz = as.numeric(dxa * c96) / decimal_x
    
    r$dx <- dxz 
    r$dy <- dy * (1 - fee)
    r$price2 <- as.bigz(P_new)
    r$fee <- fee * dy
  }
  
  return(r)
  
}

size_price_change_in_tick <- function(L, sqrtpx96, sqrtpx96_target, dx = TRUE, 
                                      decimal_adjustment = 1e18, fee = 0.003){
  # given a liquidity and known new price, how much of a change is required to get to the new price
  # Includes fees!
  
  # price in *square root* 64.96 form 
  L = as.bigz(L)
  P = as.bigz(sqrtpx96)
  P_target = as.bigz(sqrtpx96_target)
  c96 = as.bigz(2)^96
  # inverse price
  iP = P^-1
  iP_target = P_target^-1
  
  if(dx == TRUE){
    dxa = (iP_target - iP) * L
    dx = as.bigq(dxa) / (1 - fee) / decimal_adjustment * c96
    return(as.numeric(dx))
  } else { 
    dya = (P_target - P)*L
    dy = dya / (1 - fee) / c96 / decimal_adjustment
    return(as.numeric(dy))
  }
}

find_recalculation_price <- function(positions, current_price, price_up = TRUE){ 
  
  p <- positions[, c("min_price","max_price")]
  
  # if price going up, get the closest available price above current price
  if(price_up == TRUE){
    closest_price <- p[p > current_price][which.min(p[p > current_price])]
  } else { 
    closest_price <- p[p < current_price][which.max(p[p < current_price])]
  }
  
  return(closest_price)
}


check_positions <- function(ptbl, P){
  if( !("min_price" %in% colnames(ptbl)) | !("max_price" %in% colnames(ptbl))){
    stop("Cannot find min_price and max_price columns.")
  }
  
  ptbl <- ptbl %>% mutate(
    active = ifelse(P > min_price & P < max_price, TRUE, FALSE)
  )
  return(ptbl)
}

swap_across_ticks <- function(ptbl, sqrtpx96, 
                              fee_tbl = NULL, 
                              trade_record = NULL,
                              dx = NULL, 
                              dy = NULL, 
                              decimal_x = 1e18, 
                              decimal_y = 1e18, 
                              fee = 0.003){
  
  if(is.null(dx) & is.null(dy)){
    stop("A change in x or y is required to use liquidity")
  }
  
  if(!is.null(dx) & !is.null(dy)){
    stop("Only 1 swap can be done at a time")
  }
  
  # if dx is null; we're selling dy, which means price is going up! 
  # (P = Y/X; more Y is more P)
  if(is.null(dx)){
    
    amount <- dy
    price <- sqrtpx96_to_price(sqrtpX96 = sqrtpx96)
    update_ptbl <- check_positions(ptbl, price)
    
    
    # record fees separately 
    # if it is blank assume this is a fresh swap and make the fee tbl
    if(is.null(fee_tbl)){
      fee_tbl <-  update_ptbl[, c("position","liquidity", "active")]
      fee_tbl$yfee <- 0
      
      # otherwise refresh the active positions but retain any previous fee
    } else { 
      yfee = fee_tbl$yfee
      fee_tbl <-  update_ptbl[,c("position","liquidity", "active")]
      fee_tbl$yfee <- yfee
    }
    
    recalc_price <- find_recalculation_price(positions = update_ptbl, 
                                             current_price = price, 
                                             price_up = TRUE)
    
    # sum liquidity in active positions 
    current_L <- sum(as.bigz(update_ptbl$liquidity[update_ptbl$active]))  
    
    # maximum change without recalc
    max_y <- size_price_change_in_tick(
      L = current_L, 
      sqrtpx96 = sqrtpx96, 
      sqrtpx96_target = price_to_sqrtpx96(recalc_price), 
      dx = FALSE, # return dy to sell
      decimal_adjustment = decimal_y,
      fee = fee)
    
    # if you can sell without recalculation; swap within tick 
    if(max_y >= amount){
      
      swap = swap_within_tick(L = current_L,
                              sqrtpx96 = price_to_sqrtpx96(price), 
                              dy = amount, 
                              decimal_x = decimal_x, 
                              decimal_y = decimal_y, 
                              fee = fee)
      
      # attribute fees to positions 
      new_fees <- as.numeric(
        swap$fee * fee_tbl$active * as.bigq(fee_tbl$liquidity) / 
          sum(as.bigq(fee_tbl$liquidity)[fee_tbl$active])
      )
      
      # If no previous trade record is provided make a new one and return it
      # this was a swap within a tick, not across them.
      if(is.null(trade_record)){
        
        fee_tbl$yfee = fee_tbl$yfee + new_fees
        
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dy_in = swap$dy,
          dy_fee = swap$fee,
          dx_out = swap$dx,
          fee_tbl = fee_tbl
        )
      } else { 
        
        # get the original trade record and original fee_tbl 
        tr = trade_record 
        ft = trade_record$fee_tbl
        
        # add previous fees from record to latest fees and active positions
        fee_tbl$yfee = ft$yfee + new_fees
        
        # update accordingly
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dy_in = tr$dy_in + swap$dy,
          dy_fee = tr$dy_fee + swap$fee,
          dx_out = tr$dx_out + swap$dx,
          fee_tbl = fee_tbl
        )
      }
      
      # you're done; return the results.
      return(trade_record)
      
      
      # else swap as much as you can and repeat process
    } else { 
      
      leftover = amount - max_y
      
      # swap max y 
      # track leftover 
      swap = swap_within_tick(L = current_L, 
                              sqrtpx96 = price_to_sqrtpx96(price), 
                              dy = max_y, 
                              decimal_x = decimal_x, 
                              decimal_y = decimal_y, 
                              fee = fee)
      
      # attribute fees to position
      new_fees <- as.numeric(
        swap$fee * fee_tbl$active * as.bigq(fee_tbl$liquidity) / 
          sum(as.bigq(fee_tbl$liquidity)[fee_tbl$active])
      )
      
      
      # UPDATE past trade record if it exists 
      # or make new one
      
      if(is.null(trade_record)){
        
        fee_tbl$yfee = fee_tbl$yfee + new_fees
        
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dy_in = swap$dy,
          dy_fee = swap$fee,
          dx_out = swap$dx,
          fee_tbl = fee_tbl
        )
      } else { 
        # get the original trade record and original fee_tbl 
        tr = trade_record 
        ft = trade_record$fee_tbl
        
        # add previous fees from record to latest fees and active positions
        fee_tbl$yfee = ft$yfee + new_fees
        
        # update accordingly
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dy_in = tr$dy_in + swap$dy,
          dy_fee = tr$dy_fee + swap$fee,
          dx_out = tr$dx_out + swap$dx,
          fee_tbl = fee_tbl
        )
      }
      
      # call the function again with new information including *adding* trade records
      # until the final trade_record is output
      swap_across_ticks(ptbl = trade_record$ptbl,
                        sqrtpx96 = trade_record$new_price, 
                        fee_tbl = trade_record$fee_tbl, 
                        trade_record = trade_record,
                        dx = NULL, 
                        dy = leftover, 
                        decimal_x = decimal_x, 
                        decimal_y = decimal_y, 
                        fee = fee)
      
      
    }
  } else if(is.null(dy)){ 
    amount <- dx
    price <- sqrtpx96_to_price(sqrtpX96 = sqrtpx96)
    update_ptbl <- check_positions(ptbl, price)
    
    
    # record fees separately 
    # if it is blank assume this is a fresh swap and make the fee tbl
    if(is.null(fee_tbl)){
      fee_tbl <-  update_ptbl[, c("position","liquidity", "active")]
      fee_tbl$xfee <- 0
      
      # otherwise refresh the active positions but retain any previous fee
    } else { 
      xfee = fee_tbl$xfee
      fee_tbl <-  update_ptbl[,c("position","liquidity", "active")]
      fee_tbl$xfee <- xfee
    }
    
    # else we're selling dx, price is going down 
    # (P = Y/X, more X is less P)
    recalc_price <- find_recalculation_price(positions = update_ptbl, 
                                             current_price = price, 
                                             price_up = FALSE)
    
    # sum liquidity in active positions 
    current_L <- sum(as.bigz(update_ptbl$liquidity[update_ptbl$active]))  
    
    # maximum change without recalc
    max_x <- size_price_change_in_tick(
      L = current_L, 
      sqrtpx96 = sqrtpx96, 
      sqrtpx96_target = price_to_sqrtpx96(recalc_price), 
      dx = TRUE, # return dx to sell
      decimal_adjustment = decimal_y,
      fee = fee)
    
    # if you can sell without recalculation; swap within tick 
    if(max_x >= amount){
      
      swap = swap_within_tick(L = current_L,
                              sqrtpx96 = price_to_sqrtpx96(price), 
                              dx = amount, 
                              decimal_x = decimal_x, 
                              decimal_y = decimal_y, 
                              fee = fee)
      
      # attribute fees to positions 
      new_fees <- as.numeric(
        swap$fee * fee_tbl$active * as.bigq(fee_tbl$liquidity) / 
          sum(as.bigq(fee_tbl$liquidity)[fee_tbl$active])
      )
      
      # If no previous trade record is provided make a new one and return it
      # this was a swap within a tick, not across them.
      if(is.null(trade_record)){
        
        fee_tbl$xfee = fee_tbl$xfee + new_fees
        
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dx_in = swap$dx,
          dx_fee = swap$fee,
          dy_out = swap$dy,
          fee_tbl = fee_tbl
        )
      } else { 
        
        # get the original trade record and original fee_tbl 
        tr = trade_record 
        ft = trade_record$fee_tbl
        
        # add previous fees from record to latest fees and active positions
        fee_tbl$xfee = ft$xfee + new_fees
        
        # update accordingly
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dx_in = tr$dx_in + swap$dx,
          dx_fee = tr$dx_fee + swap$fee,
          dy_out = tr$dy_out + swap$dy,
          fee_tbl = fee_tbl
        )
      }
      
      # you're done; return the results.
      return(trade_record)
      
      
      # else swap as much as you can and repeat process
    } else { 
      
      leftover = amount - max_x
      
      # swap max x 
      # track leftover 
      swap = swap_within_tick(L = current_L, 
                              sqrtpx96 = price_to_sqrtpx96(price), 
                              dx = max_x, 
                              decimal_x = decimal_x, 
                              decimal_y = decimal_y, 
                              fee = fee)
      
      # attribute fees to position
      new_fees <- as.numeric(
        swap$fee * fee_tbl$active * as.bigq(fee_tbl$liquidity) / 
          sum(as.bigq(fee_tbl$liquidity)[fee_tbl$active])
      )
      
      # UPDATE past trade record if it exists 
      # or make new one
      
      if(is.null(trade_record)){
        
        fee_tbl$xfee = fee_tbl$xfee + new_fees
        
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dx_in = swap$dx,
          dx_fee = swap$fee,
          dy_out = swap$dy,
          fee_tbl = fee_tbl
        )
      } else { 
        # get the original trade record and original fee_tbl 
        tr = trade_record 
        ft = trade_record$fee_tbl
        
        # add previous fees from record to latest fees and active positions
        fee_tbl$xfee = ft$xfee + new_fees
        
        # update accordingly
        trade_record <- list(
          ptbl = update_ptbl,
          new_price = swap$price2,
          dx_in = tr$dx_in + swap$dx,
          dx_fee = tr$dx_fee + swap$fee,
          dy_out = tr$dy_out + swap$dy,
          fee_tbl = fee_tbl
        )
      }
      
      # call the function again with new information including *adding* trade records
      # until the final trade_record is output
      swap_across_ticks(ptbl = trade_record$ptbl,
                        sqrtpx96 = trade_record$new_price, 
                        fee_tbl = trade_record$fee_tbl, 
                        trade_record = trade_record,
                        dx = leftover, 
                        dy = NULL, 
                        decimal_x = decimal_x, 
                        decimal_y = decimal_y, 
                        fee = fee)
      
    }
    
  }
}

