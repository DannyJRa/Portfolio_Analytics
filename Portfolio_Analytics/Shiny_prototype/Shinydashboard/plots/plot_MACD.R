##Example 4: Use TTR MACD to Visualize Moving Average Convergence Divergence
### INPUT
Symbol=input$symbol

### Group by
Quotes_macd <- Quotes %>%
    group_by(symbol) %>%
    tq_mutate(select = close,
              mutate_fun = MACD,
              nFast = 12,
              nSlow = 26,
              nSig = 9,
              maType = SMA) %>%
              mutate(diff = macd - signal) %>%
              select(-(open:volume))
Quotes_macd

##By Filter



Quotes_macd <- Quotes %>%
    filter(symbol==Symbol) %>%
    tq_mutate(select = close,
              mutate_fun = MACD,
              nFast = 12,
              nSlow = 26,
              nSig = 9,
              maType = SMA) %>%
              mutate(diff = macd - signal) %>%
              select(-(open:volume))



#And, we can visualize the data like so.

plot_MACD <- Quotes_macd %>%
    filter(date >= as_date("2016-10-01")) %>%
    ggplot(aes(x = date)) +
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    geom_line(aes(y = macd, col = symbol)) +
    geom_line(aes(y = signal), color = "blue", linetype = 2) +
    geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) +
    facet_wrap(~symbol, ncol = 2, scale = "free_y") +
    labs(title = "Quotes: Moving Average Convergence Divergence",
         y = "MACD", x = "", color = "") +
         theme_tq() +
         scale_color_tq()