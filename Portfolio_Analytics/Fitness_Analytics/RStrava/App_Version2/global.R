print("loading app...")

source("global/dependencies.R")
source("global/header.R")
source("global/sidebar.R")



# utils functions
#sourceDir("global/futures-data", trace = FALSE)


# init db
#db <- Db$new()$singleton$init(localf="local/contracts_data.sqlite", portablef="portable.sqlite")

# loading modules 
sourceDir("modules", trace = FALSE)





