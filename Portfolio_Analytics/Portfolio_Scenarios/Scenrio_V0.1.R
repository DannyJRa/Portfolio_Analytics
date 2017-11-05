# Using csv input
tq_transmute_fun_options()$TTR

library(tidyquant)
data(FANG)
FANG
#############
#### Overwrite FANG
setwd("C:/OneDrive/PowerBI_test/Portfolio_Slicer/Input")

library(data.table)


Quotes <- fread("C:/OneDrive/PowerBI_test/Portfolio_Slicer/Input/Quotes_detail.csv")

Quotes$date = as.Date(Quotes$date)

#Quotes=as.data.frame(Quotes)
Quotes <-subset(Quotes, symbol=="SIE.DE" | symbol=="CBK.DE")



########               does strange things
##Quotes = filter(Quotes, symbol == c("SIE.DE","CBK.DE"))
#t = subset(Quotes, date == "2007-01-02")
#########

Quotes_annual_returns <- Quotes %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "yearly", 
                 type       = "arithmetic")
Quotes_annual_returns



Quotes_annual_returns %>%
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





Quotes %>%
    group_by(symbol) %>%
    tq_transmute(select     = open:volume, 
                 mutate_fun = to.period, 
                 period     = "months")

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
                 period     = "monthly")

# Baseline Returns

getSymbols(c("GDAXI"))

baseline_returns_monthly <- "GDAXI" %>%
    tq_get(get  = "stock.prices",
           from = "2013-01-01", 
           to   = "2016-12-31") %>%
           tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn,
                 period     = "monthly")

#Next, join the asset returns with the baseline returns by date.
############################### make date seq from: getsymbols_irregular data.R
####################
#########################


returns_joined <- left_join(Quotes_returns_monthly, 
                            baseline_returns_monthly,
                            by = "date")
returns_joined

returns_joined <- na.omit(returns_joined)

Quotes_rolling_corr <- returns_joined %>%
    tq_transmute_xy(x          = monthly.returns.x, 
                    y          = monthly.returns.y,
                    mutate_fun = runCor,
                    n          = 6,
                    col_rename = "rolling.corr.6")
#And, we can plot the rolling correlations for the Quotes stocks.

Quotes_rolling_corr %>%
    ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    geom_line(size = 1) +
    labs(title = "Quotes: Six Month Rolling Correlation to XLK",
         x = "", y = "Correlation", color = "") +
         facet_wrap(~ symbol, ncol = 2) +
         theme_tq() + 
         scale_color_tq()

Quotes <- na.omit(Quotes)

##Example 4: Use TTR MACD to Visualize Moving Average Convergence Divergence

Quotes_macd <- Quotes %>%
    group_by(symbol) %>%
    tq_mutate(select     = close, 
              mutate_fun = MACD, 
              nFast      = 12, 
              nSlow      = 26, 
              nSig       = 9, 
              maType     = SMA) %>%
              mutate(diff = macd - signal) %>%
              select(-(open:volume))
Quotes_macd
#And, we can visualize the data like so.

Quotes_macd %>%
    filter(date >= as_date("2016-10-01")) %>%
    ggplot(aes(x = date)) + 
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    geom_line(aes(y = macd, col = symbol)) +
    geom_line(aes(y = signal), color = "blue", linetype = 2) +
    geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) +
    facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
    labs(title = "Quotes: Moving Average Convergence Divergence",
         y = "MACD", x = "", color = "") +
         theme_tq() +
         scale_color_tq()



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

