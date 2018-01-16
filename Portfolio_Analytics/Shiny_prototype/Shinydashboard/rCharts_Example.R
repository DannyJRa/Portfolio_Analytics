#require(devtools)
#install_github('rCharts', 'ramnathv')

###SOURCE: https://github.com/ramnathv/rCharts/tree/master/demo

library(rCharts)
  hair_eye = as.data.frame(HairEyeColor)
p2 <- nPlot(Freq ~ Hair, 
            group = 'Eye',
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



hair_eye = as.data.frame(HairEyeColor)
p3 <- nPlot(Freq, 
            
            data = subset(hair_eye, Sex == "Female"),
            type = 'stackedAreaChart'
)
p3$chart(color = c('brown', 'blue', '#594c26', 'green'))
p3



########from github

## {title: Scatter Chart}
p1 <- nPlot(mpg ~ wt, group = 'cyl', data = mtcars, type = 'scatterChart')
p1$xAxis(axisLabel = 'Weight')
p1

..p.() # ================================

## {title: MultiBar Chart}
hair_eye = as.data.frame(HairEyeColor)
p2 <- nPlot(Freq ~ Hair, group = 'Eye', data = subset(hair_eye, Sex == "Female"), type = 'multiBarChart')
p2$chart(color = c('brown', 'blue', '#594c26', 'green'))
p2

..p.() # ================================

## {title: MultiBar Horizontal Chart}
p3 <- nPlot(~ cyl, group = 'gear', data = mtcars, type = 'multiBarHorizontalChart')
p3$chart(showControls = F)
p3

..p.() # ================================

## {title: Pie Chart}
p4 <- nPlot(~ cyl, data = mtcars, type = 'pieChart')
p4

..p.() # ================================

## {title: Donut Chart}
p5 <- nPlot(~ cyl, data = mtcars, type = 'pieChart')
p5$chart(donut = TRUE)
p5

..p.() # ================================

## {title: Line Chart}
data(economics, package = 'ggplot2')
p6 <- nPlot(uempmed ~ date, data = economics, type = 'lineChart')
p6

..p.() # ================================

## {title: Line with Focus Chart }
ecm <- reshape2::melt(economics[,c('date', 'uempmed', 'psavert')], id = 'date')
p7 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineWithFocusChart')
#test format dates on the xAxis
#also good test of javascript functions as parameters
#dates from R to JSON will come over as number of days since 1970-01-01
#so convert to milliseconds 86400000 in a day and then format with d3
#on lineWithFocusChart type xAxis will also set x2Axis unless it is specified
p7$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
#test xAxis also sets x2Axis
p7

..p.() # ================================

#now test setting x2Axis to something different
#test format dates on the x2Axis
#test to show %Y format which is different than xAxis
p7$x2Axis( tickFormat="#!function(d) {return d3.time.format('%Y')(new Date( d * 86400000 ));}!#" )
p7

..p.() # ================================

#test set xAxis again to make sure it does not override set x2Axis
p7$xAxis( NULL, replace = T)
p7

..p.() # ================================

## {title: Stacked Area Chart}
dat <- data.frame(t=rep(0:23,each=4),var=rep(LETTERS[1:4],4),val=round(runif(4*24,0,50)))
p8 <- nPlot(val ~ t, group =  'var', data = dat, type = 'stackedAreaChart', id = 'chart')
p8


..p.() # ================================

## {title: InteractiveGuidline(Multi-Tooltips) on Line}
p9 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineChart')
p9$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
#try new interactive guidelines feature
p9$chart(useInteractiveGuideline=TRUE)
p9


..p.() # ================================

## {title: InteractiveGuidline(Multi-Tooltips) on Stack}
p10 <- p8
p10$chart(useInteractiveGuideline=TRUE)
p10

..p.() # ================================

## {title: showDistX and showDistY}
p11 <- p1
p11$chart(showDistX = TRUE, showDistY = TRUE)
p11

..p.() # ================================

## {title: multiChart}
p12 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'multiChart')
p12$params$multi = list(
  uempmed = list(type="area",yAxis=1),
  psavert = list(type="line",yAxis=2)
)
p12$setTemplate(script = system.file(
  "/libraries/nvd3/layouts/multiChart.html",
  package = "rCharts"
))
p12

..p.() # ================================

## ## {title: Facets}
## facet has not been supported yet.
## 
## p13 <- nPlot(mpg ~ wt, data = mtcars, group = "gear", type = "scatterChart")
## p13$params$facet = "cyl"
## p13$templates$script = system.file(
##   "/libraries/nvd3/layouts/nvd3FacetPlot.html",
##   package = "rCharts"
## )
## p13
## 
## ..p.() # ================================
## 
## hair_eye = as.data.frame(HairEyeColor)
## p14 <- nPlot(Freq ~ Hair, group = 'Sex', data = hair_eye, type = 'multiBarChart')
## p14$params$facet="Eye"
## p14$templates$script = system.file(
##   "/libraries/nvd3/layouts/nvd3FacetPlot.html",
##   package = "rCharts"
## )
## p14
## 
## ..p.() # ================================
## 
## p15 <- nPlot(Freq ~ Hair, group = 'Eye', data = hair_eye, type = 'multiBarChart')
## p15$params$facet="Sex"
## p15$templates$script = system.file(
##   "/libraries/nvd3/layouts/nvd3FacetPlot.html",
##   package = "rCharts"
## )
## p15
## 
## ..p.() # ================================

## {title: Sparklines}
p16 <- nPlot(uempmed ~ date, data = economics, type = 'sparklinePlus',height=100,width=500)
p16$chart(xTickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#")
p16

..p.() # ================================

## semi replicate sparkline with a full nvd3 model by setting short height and turning off lots of things
require(quantmod)

spy <- getSymbols("SPY",auto.assign=FALSE,from="2013-01-01")
colnames(spy) <- c("open","high","low","close","volume","adjusted")

spy.df <- data.frame(index(spy),spy)
colnames(spy.df)[1] <- "date"

p17 <- nPlot(
  x = "date",
  y = "volume",
  data = spy.df,
  type = "multiBarChart",
  height = 200)
p17$chart(showControls = FALSE, showLegend = FALSE, showXAxis = FALSE, showYAxis = FALSE) 
p17$xAxis(tickFormat = 
            "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#"
)
p17


################## Highcharts
..p. <- function() invisible(readline("\nPress <return> to continue: "))
library(rCharts)

# Example 1
hPlot(Pulse ~ Height, data = MASS::survey, type = "scatter", group = "Exer")

..p.() # ================================

## Example 2
a <- hPlot(Pulse ~ Height, data = MASS::survey, type = "bubble", title = "Zoom demo", subtitle = "bubble chart", size = "Age", group = "Exer")
a$chart(zoomType = "xy")
a$exporting(enabled = T)
a

..p.() # ================================

## Example 4
x <- data.frame(key = c("a", "b", "c"), value = c(1, 2, 3))
hPlot(x = "key", y = "value", data = x, type = "pie")

..p.() # ================================

## Example 5
a <- hPlot(Pulse ~ Height, data = MASS::survey, type = 'scatter', group = 'Sex', radius = 6, group.na = "Not Available")
a$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')
a$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
a$plotOptions(scatter = list(marker = list(symbol = 'circle')))
a$tooltip(formatter = "#! function() { return this.x + ', ' + this.y; } !#")
a

..p.() # ================================

## Example 6
hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = c('column', 'line'), group = 'Sex', radius = 6)

..p.() # ================================

## Example 7
hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = 'bar', group = 'Sex', group.na = 'NA\'s')

..p.() # ================================

## Example 8
a <- hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = 'column', group = 'Sex', group.na = 'NA\'s')
a$plotOptions(column = list(dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
a$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif')), replace = F)
a

..p.() # ================================

## Example 9 (not working properly)
drill_function <- "#! function() {
var drilldown = this.drilldown;
function setChart(name, categories, data, color) {
chart.xAxis[0].setCategories(categories, false);
chart.series[0].remove(false);
chart.addSeries({
name: name,
data: data,
color: color || 'black'
}, false);
chart.redraw();
};
if (drilldown) { // drill down
setChart(drilldown.name, drilldown.categories, drilldown.data, drilldown.color);
} else { // restore
setChart(name, categories, data);
}
} !#"

a <- rCharts::Highcharts$new()
a$chart(type = "column")
a$series(data = list(
  list(y = 15, drilldown = list(data = c(1, 2, 3))), 
  list(y = 20, drilldown = list(data = c(1, 2, 3)))), name = "test")
a$xAxis(categories = c("Brand A", "Brand B"))
a$plotOptions(column = list(cursor = 'pointer', point = list(events = list(click = drill_function))))
a

..p.() # ================================

## Example 10
a <- hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = 'bar', group = 'Sex', group.na = 'NA\'s')
a$plotOptions(bar = list(cursor = 'pointer', point = list(events = list(click = "#! function() { alert ('Category: '+ this.category +', value: '+ this.y); } !#"))))
a

..p.() # ================================

## Example 11
a <- rCharts::Highcharts$new()
a$series(data = list(
  list(y = 8, url = "https://github.com/metagraf/rHighcharts", color = "lightblue"),
  list(y = 14, url = "https://github.com/metagraf/rVega", color = "lightpink"),
  list(y = 71, url = "https://github.com/ramnathv/rCharts", color = "lightgreen")
), type = "column", name = "Number of Stars")
a$plotOptions(column = list(cursor = 'pointer', point = list(events = list(click = "#! function() { location.href = this.options.url; } !#"))))
a$xAxis(categories = c("rHighcharts", "rVega", "rCharts"), title = list(text = ""))
a$yAxis(title = list(text = ""))
a$legend(enabled = F)
a

..p.() # ================================



########## PolyCharts

## Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
p1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
p1
p1$show(cdn = T)
# p1$show(static = F)

..p.() # ================================

## Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
p2 <- rPlot(Freq ~ Hair, color = 'Eye', data = hair_eye, type = 'bar')
p2$facet(var = 'Eye', type = 'wrap', rows = 2)
p2

..p.() # ================================

## Example 3 Boxplot
data(tips, package = 'reshape2')
p3 <- rPlot(x = 'day', y = 'box(tip)', data = tips, type = 'box')
p3

..p.() # ================================

## Example 4 
require(plyr)
dat = count(mtcars, .(gear, am))
p4 <- rPlot(x = 'bin(gear, 1)', y = 'freq', data = dat, type = 'bar', 
            list(var = 'am', type = 'wrap'))
p4

..p.() # ================================

## Example 5 (Heat Map)
dat = expand.grid(x = 1:5, y = 1:5)
dat = transform(dat, value = sample(1:5, 25, replace = T))
p5 <- rPlot(x = 'bin(x, 1)', y = 'bin(y, 1)', color = 'value', data = dat, type = 'tile')
p5

..p.() # ================================

# Example 6 (NBA Heat Map)
require(reshape2); require(scales); require(plyr)
nba <- read.csv('http://datasets.flowingdata.com/ppg2008.csv')
nba.m <- ddply(melt(nba), .(variable), transform, rescale = rescale(value))
p6 <- rPlot(Name ~ variable, color = 'rescale', data = nba.m, type = 'tile', height = 600)
p6$guides("{color: {scale: {type: gradient, lower: white, upper: steelblue}}}")
p6













################# DIMPLE CHARTS


library(rCharts)

#get data used by dimple for all of its examples as a first test
data <- read.delim(
  "http://pmsi-alignalytics.github.io/dimple/data/example_data.tsv"
)

# original data is ~ 1000 rows, which is too huge as demo.
set.seed(42)
data <- data[sample(nrow(data), 50), ]

#eliminate . to avoid confusion in javascript
colnames(data) <- gsub("[.]","",colnames(data))

## example 1 vt bar
d1 <- dPlot(
  x ="Month" ,
  y = "UnitSales",
  data = data,
  type = "bar"
)
d1$xAxis(orderRule = "Date")
d1

..p.() # ================================

## example 2 vt stacked bar
d1 <- dPlot(
  x ="Month" ,
  y = "UnitSales",
  groups = "Channel",
  data = data,
  type = "bar"
)
d1$xAxis(orderRule = "Date")
d1$legend(
  x = 60,
  y = 10,
  width = 700,
  height = 20,
  horizontalAlign = "right"
)
d1

..p.() # ================================

## example 3 vertical 100% bar
#use from above and just change y axis type
d1$yAxis(type = "addPctAxis")
d1

..p.() # ================================

## example 4 vertical grouped bar
d1 <- dPlot(
  x = c("PriceTier","Channel"),
  y = "UnitSales",
  groups = "Channel",
  data = data,
  type = "bar"
)
d1$legend(
  x = 60,
  y = 10,
  width = 700,
  height = 20,
  horizontalAlign = "right"
)
d1

..p.() # ================================

## example 5 vertical stack grouped bar
d1 <- dPlot(
  x = c("PriceTier","Channel"),
  y = "UnitSales",
  groups = "Owner",
  data = data,
  type = "bar"
)
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 6 vertical 100% Grouped Bar
#just change y Axis
d1$yAxis(type = "addPctAxis")
d1

..p.() # ================================

## example 7 horizontal bar
d1 <- dPlot(
  Month ~ UnitSales,
  data = data,
  type = "bar"
)
d1$xAxis(type = "addMeasureAxis")
#good test of orderRule on y instead of x
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1


..p.() # ================================

## example 8 horizontal stacked bar
d1 <- dPlot(
  Month ~ UnitSales,
  groups = "Channel",
  data = data,
  type = "bar"
)
d1$xAxis(type = "addMeasureAxis")
#good test of orderRule on y instead of x
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1

..p.() # ================================

## example 9 horizontal 100% bar
d1$xAxis(type = "addPctAxis")
d1


..p.() # ================================

## example 10 horizontal stacked bar
d1 <- dPlot(
  x = "UnitSales", 
  y = c("PriceTier","Channel"),
  groups = "Channel",
  data = data,
  type = "bar"
)
d1$xAxis(type = "addMeasureAxis")
#good test of orderRule on y instead of x
d1$yAxis(type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 11 horizontal stacked grouped bar
d1 <- dPlot(
  x = "UnitSales", 
  y = c("PriceTier","Channel"),
  groups = "Owner",
  data = data,
  type = "bar"
)
d1$xAxis(type = "addMeasureAxis")
#good test of orderRule on y instead of x
d1$yAxis(type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1

..p.() # ================================

## example 12 horizontal 100% grouped bar
d1$xAxis(type = "addPctAxis")
d1


..p.() # ================================

## example 13 vertical marimekko
d1 <- dPlot(
  UnitSales ~ Channel,
  groups = "Owner",
  data = data,
  type = "bar"
)
d1$xAxis(type = "addAxis", measure = "UnitSales", showPercent = TRUE)
d1$yAxis(type = "addPctAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1
#test with storyboard
d1$set(storyboard = "Date")
d1

..p.() # ================================

## example 14 horizontal marimekko
d1 <- dPlot(
  Channel ~ UnitSales,
  groups = "Owner",
  data = data,
  type = "bar"
)
d1$yAxis(type = "addAxis", measure = "UnitSales", showPercent = TRUE)
d1$xAxis(type = "addPctAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 15 block matrix
d1 <- dPlot(
  x = c("Channel","PriceTier"),
  y = "Owner",
  groups = "PriceTier",
  data = data,
  type = "bar"
)
d1$yAxis(type = "addCategoryAxis")
d1$xAxis(type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 16 Scatter
d1 <- dPlot(
  OperatingProfit~UnitSales,
  groups = c("SKU","Channel"),
  data = subset(data, Date == "01/12/2012"),
  type = "bubble"
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 17 Vertical Lollipop
d1 <- dPlot(
  UnitSales ~ Month,
  groups = "Channel",
  data = data,
  type = "bubble"
)
#defaults to yAxis (Measure) and xAxis (Category)
d1$xAxis( orderRule = "Date")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 18 Vertical Grouped Lollipop
d1 <- dPlot(
  y = "UnitSales",
  x = c("PriceTier","Channel"),
  groups = "Channel",
  data = data,
  type = "bubble"
)
#defaults to yAxis (Measure) and xAxis (Category)
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 19 Horizontal Lollipop
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  groups = "Channel",
  data = data,
  type = "bubble"
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addCategoryAxis", orderRule = "Date")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 20 Horizontal Grouped Lollipop
d1 <- dPlot(
  x = "UnitSales",
  y = c("PriceTier","Channel"),
  groups = "Channel",
  data = data,
  type = "bubble"
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1



..p.() # ================================

## example 21 Dot Matrix
d1 <- dPlot(
  y = "Owner",
  x = c("Channel","PriceTier"),
  groups = "PriceTier",
  data = data,
  type = "bubble"
)
d1$xAxis( type = "addCategoryAxis" )
d1$yAxis( type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 22 Bubble
d1 <- dPlot(
  x = "UnitSalesMonthlyChange",
  y = "PriceMonthlyChange",
  z = "OperatingProfit",
  groups = c("SKU","Channel"),
  data = subset(data, Date == "01/12/2012"),
  type = "bubble"
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 23 Vertical Bubble Lollipop
d1 <- dPlot(
  x = "Month",
  y = "UnitSales",
  z = "OperatingProfit",
  groups = "Channel",
  data = subset(
    data,
    Date %in% c(
      "01/07/2012",
      "01/08/2012",
      "01/09/2012",
      "01/10/2012",
      "01/11/2012",
      "01/12/2012"
    )
  ),
  type = "bubble"
)
d1$xAxis( type = "addCategoryAxis", orderRule = "Date" )
d1$yAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#..p.() # ================================

## example 24 Vertical Grouped Bubble Lollipop
d1 <- dPlot(
  x = c("PriceTier","Channel"),
  y = "UnitSales",
  z = "OperatingProfit",
  groups = "Channel",
  data = subset(
    data,
    Date %in% c(
      "01/07/2012",
      "01/08/2012",
      "01/09/2012",
      "01/10/2012",
      "01/11/2012",
      "01/12/2012"
    )
  ),
  type = "bubble"
)
d1$xAxis( type = "addCategoryAxis" )
d1$yAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 25 Horizontal Bubble Lollipop
d1 <- dPlot(
  y = "Month",
  x = "UnitSales",
  z = "OperatingProfit",
  groups = "Channel",
  data = subset(
    data,
    Date %in% c(
      "01/07/2012",
      "01/08/2012",
      "01/09/2012",
      "01/10/2012",
      "01/11/2012",
      "01/12/2012"
    )
  ),
  type = "bubble"
)
d1$yAxis( type = "addCategoryAxis", orderRule = "Date" )
d1$xAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#..p.() # ================================

## example 26 Horizontal Grouped Bubble Lollipop
d1 <- dPlot(
  y = c("PriceTier","Channel"),
  x = "UnitSales",
  z = "OperatingProfit",
  groups = "Channel",
  data = subset(
    data,
    Date %in% c(
      "01/07/2012",
      "01/08/2012",
      "01/09/2012",
      "01/10/2012",
      "01/11/2012",
      "01/12/2012"
    )
  ),
  type = "bubble"
)
d1$yAxis( type = "addCategoryAxis" )
d1$xAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 27 Bubble Matrix
d1 <- dPlot(
  x = c( "Channel", "PriceTier"),
  y = "Owner",
  z = "Distribution",
  groups = "PriceTier",
  data = data,
  type = "bubble",
  aggregate = "dimple.aggregateMethod.max"
)
d1$xAxis( type = "addCategoryAxis" )
d1$yAxis( type = "addCategoryAxis" )
d1$zAxis( type = "addMeasureAxis", overrideMax = 200 )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 28 Area
d1 <- dPlot(
  UnitSales ~ Month,
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area"
)
d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1


..p.() # ================================

## example 29 Stacked Area
d1 <- dPlot(
  UnitSales ~ Month,
  groups = "Channel",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area"
)
d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 30 100% Stacked Area
#just change type for y axis
d1$yAxis( type = "addPctAxis" )
d1



..p.() # ================================

## example 31 Grouped Area
d1 <- dPlot(
  y = "UnitSales",
  x = c("Owner","Month"),
  groups = "Owner",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area"
)
d1$xAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1



..p.() # ================================

## example 32 Grouped Stacked Area
d1 <- dPlot(
  y = "UnitSales",
  x = c("Owner","Month"),
  groups = "SKU",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=70,y=30,height=340,width=330),
  barGap = 0.05,
  lineWeight = 1,
  height = 400,
  width = 590
)
d1$xAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 430,
  y = 20,
  width = 100,
  height = 300,
  horizontalAlign = "left"
)
d1


..p.() # ================================

## example 33 Grouped 100% Area
d1$yAxis( type = "addPctAxis" )
d1


..p.() # ================================

## example 34 Vertical Area
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=80,y=30,height=480,width=330),
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1



..p.() # ================================

## example 35 Vertical Stacked Area
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  groups = "Channel",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=80,y=30,height=480,width=330),
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$legend(
  x = 60,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


..p.() # ================================

## example 36 Vertical 100% Stacked Area
d1$xAxis(type = "addPctAxis")
d1


..p.() # ================================

## example 37 Vertical Grouped Area
d1 <- dPlot(
  x = "UnitSales",
  y = c("Owner","Month"),
  groups = "Owner",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=90,y=30,height=470,width=330),
  lineWeight = 1,
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1

..p.() # ================================

## example 38 Vertical Grouped Stacked Area
d1 <- dPlot(
  x = "UnitSales",
  y = c("Owner","Month"),
  groups = "SKU",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=90,y=30,height=320,width=330),
  lineWeight = 1,
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$legend(
  x = 430,
  y = 20,
  width = 100,
  height = 300,
  horizontalAlign = "left"
)
d1



..p.() # ================================

## example 39 Vertical Group 100% Area
d1$xAxis( type = "addPctAxis" )
d1





..p.() # ================================

## example 40 Line
d1 <- dPlot(
  UnitSales ~ Month,
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line"
)
d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1


..p.() # ================================

## example 41 Multiple Line
d1 <- dPlot(
  UnitSales ~ Month,
  groups = "Channel",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line"
)
d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1

..p.() # ================================

## example 42 Grouped Single Line
d1 <- dPlot(
  y = "UnitSales",
  x = c("Owner","Month"),
  groups = "Owner",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  barGap = 0.05
)
d1$xAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1



..p.() # ================================

## example 43 Grouped Multiple line
d1 <- dPlot(
  y = "UnitSales",
  x = c("Owner","Month"),
  groups = "Brand",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=70,y=30,height=420,width=330),
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 510,
  y = 20,
  width = 100,
  height = 300,
  horizontalAlign = "left"
)
d1



..p.() # ================================

## example 44 Vertical Line
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=80,y=30,height=480,width=330),
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1



..p.() # ================================

## example 45 Vertical Multiple Line
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  groups = "Channel",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=80,y=30,height=480,width=330),
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1$legend(
  x = 60,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1



..p.() # ================================

## example 46 Vertical Grouped Line
d1 <- dPlot(
  x = "UnitSales",
  y = c("Owner","Month"),
  groups = "Owner",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=90,y=30,height=470,width=330),
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1



..p.() # ================================

## example 47 Vertical Grouped Multi Line
d1 <- dPlot(
  x = "UnitSales",
  y = c("Owner","Month"),
  groups = "Brand",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=90,y=30,height=320,width=330),
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$legend(
  x = 430,
  y = 20,
  width = 100,
  height = 300,
  horizontalAlign = "left"
)
d1


..p.() # ================================

## example 48 timeAxis
data( economics, package = "ggplot2" )
economics$date = format(economics$date, "%Y-%m-%d")
d1 <- dPlot(
  x = "date",
  y = "uempmed",
  data = economics,
  type = "line",
  height = 400,
  width = 700,
  bounds = list(x=50,y=20,width=650,height=300)
)
d1$xAxis(
  type = "addTimeAxis",
  inputFormat = "%Y-%m-%d",
  outputFormat = "%b %Y"
)
d1
