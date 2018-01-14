

Quotes_annual_returns <- Quotes %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "yearly", 
                 type       = "arithmetic")
Quotes_annual_returns




plot3=Quotes_annual_returns %>%
    ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Quotes: Annual Returns",
         subtitle = "Get annual returns quickly with tq_transmute!",
         y = "Annual Returns", x = "") + 
         facet_wrap(~ symbol, ncol = 2) +
         theme_tq() + 
         scale_fill_tq()
#plot3

Quotes_daily_log_returns <- Quotes %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "daily", 
                 type       = "log",
                 col_rename = "monthly.returns")

Quotes_daily_log_returns %>%
    ggplot(aes(x = monthly.returns, fill = symbol)) +
    geom_density(alpha = 0.5) +
    labs(title = "Quotes: Charting the Daily Log Returns",
         x = "Monthly Returns", y = "Density") +
         theme_tq() +
         scale_fill_tq() + 
         facet_wrap(~ symbol, ncol = 2)







#Without Periodicity Aggregation

Quotes_daily <- Quotes %>%
    group_by(symbol)

Quotes_daily %>%
    ggplot(aes(x = date, y = adjusted, color = symbol)) +
    geom_line(size = 1) +
    labs(title = "Daily Stock Prices",
         x = "", y = "Adjusted Prices", color = "") +
         facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
         scale_y_continuous(labels = scales::dollar) +
         theme_tq() + 
         scale_color_tq()


#With Periodicity Aggregation

Quotes_monthly <- Quotes %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = to.period, 
                 period     = "months")

Quotes_monthly %>%
    ggplot(aes(x = date, y = adjusted, color = symbol)) +
    geom_line(size = 1) +
    labs(title = "Monthly Stock Prices",
         x = "", y = "Adjusted Prices", color = "") +
         facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
         scale_y_continuous(labels = scales::dollar) +
         theme_tq() + 
         scale_color_tq()



# Asset Returns
Quotes_returns_monthly <- Quotes %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn,
                 period = "monthly",
                 col_rename = "Ra")


# Baseline Returns
baseline_returns_monthly <- "XLK" %>%
    tq_get(get  = "stock.prices",
           from = "2007-01-01", 
           to   = "2017-11-01") %>%
           tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn,
                 period = "monthly",
                 col_rename = "Rb")




#Next, join the asset returns with the baseline returns by date.
############################### make date seq from: getsymbols_irregular data.R
### fsdfd#################
#sfdsf########################


returns_joined <- left_join(Quotes_returns_monthly, 
                            baseline_returns_monthly,
                            by = "date")
returns_joined

returns_joined <- na.omit(returns_joined)

Quotes_rolling_corr <- returns_joined %>%
    tq_transmute_xy(x          = Ra, 
                    y          = Rb,
                    mutate_fun = runCor,
                    n          = 6,
                    col_rename = "rolling.corr.6")
#And, we can plot the rolling correlations for the Quotes stocks.

## already plot_RollingCorr
#quotes_rolling_corr %>%
    #ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
    #geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    #geom_line(size = 1) +
    #labs(title = "quotes: six month rolling correlation to xlk",
         #x = "", y = "correlation", color = "") +
         #facet_wrap(~symbol, ncol = 2) +
         #theme_tq() +
         #scale_color_tq()

Quotes <- na.omit(Quotes)





Quotes_macd <- Quotes %>%
    group_by(symbol) %>%
    tq_mutate(select     = close, 
              mutate_fun = RSI, 
              n          = 14, 
              maType     = SMA) 

Quotes_macd

Quotes_macd %>%
    filter(date >= as_date("2016-10-01")) %>%
    ggplot(aes(x = date)) + 
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    geom_line(aes(y = SMA, col = symbol))


#Example 5: Use xts apply.quarterly to Get the Max and Min Price for Each Quarter



Quotes
Quotes_max_by_qtr <- Quotes %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = apply.quarterly,
                 FUN = max,
                 col_rename = "max.close") %>%
                 mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
                 select(-date)
Quotes_max_by_qtr

#The minimum each quarter can be retrieved in much the same way. The data frames can be joined using left_join to get the max and min by quarter.

Quotes_min_by_qtr <- Quotes %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = apply.quarterly,
                 FUN = min,
                 col_rename = "min.close") %>%
                 mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
                 select(-date)

Quotes_by_qtr <- left_join(Quotes_max_by_qtr, Quotes_min_by_qtr,
                         by = c("symbol" = "symbol",
                                "year.qtr" = "year.qtr"))
Quotes_by_qtr


byQuarterAll <- Quotes_by_qtr %>%
    ggplot(aes(x = year.qtr, color = symbol)) +
    geom_segment(aes(xend = year.qtr, y = min.close, yend = max.close),
                 size = 1) +
                 geom_point(aes(y = max.close), size = 2) +
                 geom_point(aes(y = min.close), size = 2) +
                 facet_wrap(~symbol, ncol = 2, scale = "free_y") +
                 labs(title = "FANG: Min/Max Price By Quarter",
         y = "Stock Price", color = "") +
         theme_tq() +
         scale_color_tq() +
         scale_y_continuous(labels = scales::dollar) +
         theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_blank())
byQuarterAll



