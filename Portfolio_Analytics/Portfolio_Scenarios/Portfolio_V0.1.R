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

#replace
stock_returns_monthly=Quotes_returns_monthly



#Second, get baseline asset returns, which is the exact same as Steps 1B and 2B from the Single Portfolio example.
#baseline_returns_monthly <- "XLK" %>%
#baseline_returns_monthly <- "GDAXI" %>%
#    tq_get(get  = "stock.prices",
#           from = "2010-01-01",
 #          to   = "2015-12-31") %>%
  #         tq_transmute(select     = adjusted, 
   #              mutate_fun = periodReturn, 
    #             period     = "monthly", 
      #           col_rename = "Rb")

baseline_returns_monthly



stock_returns_monthly_multi <- stock_returns_monthly %>%
    tq_repeat_df(n = n)
stock_returns_monthly_multi


weights <- c(
    0.50, 0.25, 0.25,
     0.50, 0.4, 0.1,
     1, 0, 0
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



## Customizing tq_performance

args(SharpeRatio)
RaRb_multiple_portfolio %>%
    tq_performance(Ra              = Ra, 
                   performance_fun = SharpeRatio)

RaRb_multiple_portfolio %>%
    tq_performance(Ra              = Ra, 
                   performance_fun = SharpeRatio,
                   Rf              = 0.03 / 12)


RaRb_multiple_portfolio %>%
    tq_performance(Ra = Ra,
                   performance_fun = SharpeRatio,
                   Rf = 0.03 / 12,
                   p = 0.99)




#CHARTS


args(Return.portfolio)

wts_map <- tibble(
    symbols = c(ticker[1:2]),
    weights = c(0.5, 0.5)
)
wts_map




portfolio_returns_monthly <- stock_returns_monthly %>%
    tq_portfolio(assets_col = symbol,
                 returns_col = Ra,
                 weights = wts_map,
                 col_rename = "Ra")



portfolio_returns_monthly %>%
    ggplot(aes(x = date, y = Ra)) +
    geom_bar(stat = "identity", fill = palette_light()[[1]]) +
    labs(title = "Portfolio Returns",
         subtitle = "50% AAPL, 0% GOOG, and 50% NFLX",
         caption = "Shows an above-zero trend meaning positive returns",
         x = "", y = "Monthly Returns") +
         geom_smooth(method = "lm") +
         theme_tq() +
         scale_color_tq() +
         scale_y_continuous(labels = scales::percent)


#Custom

portfolio_growth_monthly <- stock_returns_monthly %>%
    tq_portfolio(assets_col = symbol,
                 returns_col = Ra,
                 weights = wts_map,
                 col_rename = "investment.growth",
                 wealth.index = TRUE) %>%
                 mutate(investment.growth = investment.growth * 10000)


portfolio_growth_monthly %>%
    ggplot(aes(x = date, y = investment.growth)) +
    geom_line(size = 2, color = palette_light()[[1]]) +
    labs(title = "Portfolio Growth",
         subtitle = "50% AAPL, 0% GOOG, and 50% NFLX",
         caption = "Now we can really visualize performance!",
         x = "", y = "Portfolio Value") +
         geom_smooth(method = "loess") +
         theme_tq() +
         scale_color_tq() +
         scale_y_continuous(labels = scales::dollar)












#CHARTS

# Multiple Portfolios

portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
    tq_portfolio(assets_col = symbol,
                 returns_col = Ra,
                 weights = weights_table,
                 col_rename = "investment.growth",
                 wealth.index = TRUE) %>%
                 mutate(investment.growth = investment.growth * 10000)


portfolio_growth_monthly_multi %>%
    ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
    geom_line(size = 2) +
    labs(title = "Portfolio Growth",
         subtitle = "Comparing Multiple Portfolios",
         caption = "Portfolio 3 is a Standout!",
         x = "", y = "Portfolio Value",
         color = "Portfolio") +
         geom_smooth(method = "loess") +
         theme_tq() +
         scale_color_tq() +
         scale_y_continuous(labels = scales::dollar)



