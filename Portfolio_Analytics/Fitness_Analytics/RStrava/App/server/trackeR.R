## ----Download tcx files----------------------------------------

# TODO:Load state from previously suspended application
#TODO URL definition
############### get URL
#id=1391423025
#URL <- "https://www.strava.com/activities/1391423025/export_tcx"

#########################

filepath <- paste0("R:/5_IT/5_Secrets/TCX/")

#TrainFile <- paste0(filepath,id,".tcx")


# download the datasets
#if (!file.exists(TrainFile))
#{
#    download.file(URL, destfile = TrainFile)
#}




## ----message=FALSE, warning=FALSE----------------------------------------
library("trackeR")
# download the datasets
if (!file.exists(paste0(filepath,"runs.rds")))
{
#   runs <- readDirectory(filepath)
  str(runs)

#https://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/
saveRDS(runs,file=paste0(filepath,"runs.rds"))
}

runs=readRDS(paste0(filepath,"runs.rds"))

#run=input$run


