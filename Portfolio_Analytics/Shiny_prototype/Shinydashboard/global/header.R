#require(devtools)
#install_github('rCharts', 'ramnathv')



library(rCharts)
  hair_eye = as.data.frame(HairEyeColor)
p2 <- nPlot(Freq ~ Hair, group = 'Eye',
            data = subset(hair_eye, Sex == "Female"),
            type = 'multiBarChart'
)
p2$chart(color = c('brown', 'blue', '#594c26', 'green'))
p2

#### Highcharts
h1 <- Highcharts$new()
h1$chart(type = "spline")
h1$series(data = c(1, 3, 2, 4, 5, 4, 6, 2, 3, 5, NA), dashStyle = "longdash")
h1$series(data = c(NA, 4, 1, 3, 4, 2, 9, 1, 2, 3, 4), dashStyle = "shortdot")
h1$legend(symbolWidth = 80)
h1

####