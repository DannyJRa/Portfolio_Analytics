#if (!exists("input$run")) {
#    input=NULL
#    input$run = 1
#}


#create plot

# plots for most recent activity
plot1 <- get_spdsplits(my_acts, stoken, acts = input$run, units = 'metric')

