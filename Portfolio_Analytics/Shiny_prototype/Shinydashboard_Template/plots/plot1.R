n=5
n=readRDS("slider.rds")
n=input$slider

set.seed(122)
histdata <- rnorm(500)
data <- histdata[seq_len(n)]
hist(data)
