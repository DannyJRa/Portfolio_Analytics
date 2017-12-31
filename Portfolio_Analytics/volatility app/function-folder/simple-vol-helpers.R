## Calculate component returns

componentReturns_df <- function(stock1, stock2, stock3, stock4, stock5, start_date){
  
  symbols <- c(stock1, stock2, stock3, stock4, stock5)
  
  prices <- 
    getSymbols(symbols, src = 'yahoo', from = start_date, 
               auto.assign = TRUE, warnings = FALSE) %>% 
    map(~Cl(get(.))) %>% 
    reduce(merge) %>%
    `colnames<-`(symbols)
  
  # generate daily return series for funds
  prices_monthly <- to.monthly(prices, indexAt = "first", OHLC = FALSE)
  returns <- na.omit(ROC(prices_monthly, 1, type = "continuous"))
  
  
  returns_df <- returns %>% 
    as_tibble(preserve_row_names = TRUE) %>% 
    mutate(date = ymd(row.names)) %>% 
    select(-row.names) %>% 
    select(date, everything())
}


# Calculate rolling Portfolio Standard Deviation

rolling_portfolio_sd <- function(returns_df, start = 1, window = 6, weights){
  
  start_date <- returns_df$date[start]
  
  end_date <-  returns_df$date[c(start + window)]
  
  interval_to_use <- returns_df %>% filter(date >= start_date & date < end_date)
  
  returns_xts <- interval_to_use %>% as_xts(date_col = date) 
  
  w <- weights
  
  results_as_xts <- StdDev(returns_xts, weights = w, portfolio_method = "single")
  results_as_xts <- round(results_as_xts, 4) * 100
  
  results_to_tibble <- as_tibble(t(results_as_xts[,1])) %>% 
    mutate(date = ymd(end_date)) %>% 
    select(date, everything()) 
  
}

# Look how long this code chunk is. Easier to stash this in a helpers.r file!