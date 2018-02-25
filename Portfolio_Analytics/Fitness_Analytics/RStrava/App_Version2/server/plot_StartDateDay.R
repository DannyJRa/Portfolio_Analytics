## ------------------------------------------------------------------------
#runs=c(runTr0,runTr1)

plotdata<-Running %>% 
  mutate(Day=date(time)) %>% 
  filter(Day==input$inputTest)

#plotdata<-Running %>% 
#  mutate(Day=date(time)) %>% 
#  filter(Day=="2018-02-24")

run2<-plotdata %>% group_by(km)%>%
  summarise(avg_speed = mean(speed), 
            min_time = min(time), 
            max_time = max(time)) %>%
  mutate(t=(max_time-min_time)/60)

run2=as.data.frame(run2)

