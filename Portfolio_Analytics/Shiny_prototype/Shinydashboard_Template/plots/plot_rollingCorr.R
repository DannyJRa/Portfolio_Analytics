Symbol="SIE.DE"
Symbol=readRDS("symbol.rds")
#n=input$symbol

plot_rollingCorr=Quotes_rolling_corr %>%
  filter(symbol == Symbol) %>%
  ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(size = 1) +
  labs(title = "Quotes: Six Month Rolling Correlation to XLK",
       x = "", y = "Correlation", color = "") +
  facet_wrap(~symbol, ncol = 2) +
  theme_tq() +
  scale_color_tq()

