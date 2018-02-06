######
#INPUT
#input$symbol="SIE.DE"
#######


Quotes_by_qtr2 <- Quotes_by_qtr %>%
  filter(symbol==input$symbol)

byQuarter=Quotes_by_qtr2 %>%
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

byQuarter
