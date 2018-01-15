library(shinyjs)
library(shiny)

shinyApp(
  ui = fluidPage(
    useShinyjs(),
    strong("Enter an R expression"),
    runcodeUI()
  ),
  server = function(input, output) {
    runcodeServer()
  }
)