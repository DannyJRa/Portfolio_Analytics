library(plumber)

serve_model <- plumb("plumber_titanic/titanic-api.R")
serve_model$run(port = 8000)