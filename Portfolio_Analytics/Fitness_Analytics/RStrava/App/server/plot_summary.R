## ------------------------------------------------------------------------
#runs=c(runTr0,runTr1)
runsSummary <- summary(runs)
plot_summary<-plot(runsSummary, group = c("total", "moving"),
  what = c("avgSpeed", "distance", "duration", "avgHeartRate"))
