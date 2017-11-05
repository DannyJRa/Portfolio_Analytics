# Single Portfolio depending on n
#https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ05-performance-analysis-with-tidyquant.html

n = 3

stock_returns_monthly <- ticker %>%
    tq_get(get  = "stock.prices",
           from = "2010-01-01",
           to   = "2015-12-31") %>%
           group_by(symbol) %>%
           tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 col_rename = "Ra")





#Second, get baseline asset returns, which is the exact same as Steps 1B and 2B from the Single Portfolio example.
#baseline_returns_monthly <- "XLK" %>%
baseline_returns_monthly <- "GDAXI" %>%
    tq_get(get  = "stock.prices",
           from = "2010-01-01",
           to   = "2015-12-31") %>%
           tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 col_rename = "Rb")

stock_returns_monthly_multi <- stock_returns_monthly %>%
    tq_repeat_df(n = n)
stock_returns_monthly_multi


weights <- c(
    0.50, 0.25, 0.25,
     0.50, 0.4, 0.1,
     0.50, 0.4, 0.1
)
stocks <- ticker[1:3]
weights_table <-  tibble(stocks) %>%
    tq_repeat_df(n = n) %>%
    bind_cols(tibble(weights)) %>%
    group_by(portfolio)
weights_table


portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>%
    tq_portfolio(assets_col  = symbol, 
                 returns_col = Ra, 
                 weights     = weights_table, 
                 col_rename  = "Ra")
portfolio_returns_monthly_multi



RaRb_multiple_portfolio <- left_join(portfolio_returns_monthly_multi, 
                                     baseline_returns_monthly,
                                     by = "date")
RaRb_multiple_portfolio


P_CAPM=RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)


P_SR=RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio)

P_VaR <- RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR)



tq_performance_fun_options()

P_Stats<-RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Stats)
