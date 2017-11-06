print("loading app...")

source("global/dependencies.R")
source("global/sidebar.R")
source("Portfolio_Scenarios/LoadQuotes.R")
#source("Portfolio_Scenarios/SingleAssets.R")

# utils functions
#sourceDir("global/futures-data", trace = FALSE)


# init db
#db <- Db$new()$singleton$init(localf="local/contracts_data.sqlite", portablef="portable.sqlite")

# loading modules 
sourceDir("modules", trace = FALSE)





