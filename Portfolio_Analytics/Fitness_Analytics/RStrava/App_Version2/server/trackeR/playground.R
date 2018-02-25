## ------------------------------------------------------------------------
#plot_basic=plot(runs, session = 1, what = "speed", threshold = FALSE)


## gfdgdf------------------------------------------------------------------------
p = plot(runs, session = 1, what = "speed")

plot(runs, session = 1, what = "speed", smooth = TRUE, fun = "median", width = 20)
run4 <- threshold(runs[1])
run4S <- smoother(run4, what = "speed", fun = "median", width = 20)
plot(run4S, what = "speed", smooth = FALSE)


## ------------------------------------------------------------------------
plot(runs, session = 1, what = "pace")
plot(runs, session = 1, what = "pace", smooth = TRUE, fun = "median", width = 20)
run4 <- threshold(runs[1])
run4S <- smoother(run4, what = "pace", fun = "median", width = 20)
plot(run4S, what = "pace", smooth = FALSE)


## ------------------------------------------------------------------------
#runs=c(runTr0,runTr1)
plot(runs, session = 1:2)
p = plot(runs, session = 2, what = c("altitude", "pace"))

library(ggplot2)
library(plotly)
p = ggplotly(p)
p
summary(runs, session = 2, movingThreshold = 1)

## ------------------------------------------------------------------------

test2 = as.data.frame(runs)
plot_basic = leafletRoute(runs, sessions = 1)


## ------------------------------------------------------------------------
#runs=c(runTr0,runTr1)
#runsSummary <- summary(runs)
#plot(runsSummary, group = c("total", "moving"),
#  what = c("avgSpeed", "distance", "duration", "avgHeartRate"))

## ------------------------------------------------------------------------
timeline(runsSummary)




runsT <- threshold(runs)
dpRuns <- distributionProfile(runsT, what = c("speed", "heart.rate"))
dpRunsS <- smoother(dpRuns)
cpRuns <- concentrationProfile(dpRunsS)
plot(cpRuns, multiple = TRUE, smooth = FALSE)


## A ridgeline plot of the concentration profiles------------------------------------------------------------------------
ridges(cpRuns, what = "speed")


## ------------------------------------------------------------------------
runZones <- zones(runs, what = "speed", breaks = c(0, 2:6, 12.5))
Zones = as.data.frame((runZones))
plot(runZones)

runZones <- zones(runs, what = "distance", breaks = c(0, 1:10, 10))
Zones = as.data.frame((runZones))
plot(runZones)


runZones <- zones(runs, what = "pace", breaks = c(0, 1:10, 10))
Zones = as.data.frame((runZones))
plot(runZones)


## ------------------------------------------------------------------------
wexp <- Wprime(runs, session = 1, quantity = "expended", cp = 4, version = "2012")
plot(wexp, scaled = TRUE)

## ------------------------------------------------------------------------
dProfile <- distributionProfile(runs, session = 1:2, what = "speed",
  grid = list(speed = seq(0, 12.5, by = 0.05)))
plot(dProfile, multiple = TRUE)

## ------------------------------------------------------------------------
cProfile <- concentrationProfile(dProfile, what = "speed")
plot(cProfile, multiple = TRUE)