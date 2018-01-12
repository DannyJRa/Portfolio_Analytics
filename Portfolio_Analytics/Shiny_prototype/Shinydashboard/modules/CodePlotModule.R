####Log
#

#module wrapper
CodePlotUI <- function(id) {
    ns <- NS(id)

    #  tagList(

  #module wrapper

  #comment
    #ui <-
  #
    pageWithSidebar(
      headerPanel("ggplot Live"),
      
      sidebarPanel(
        tabsetPanel(
          tabPanel("Editor", 
                   # Code editor window.
                   aceEditor(ns("plotCode"), 
                             mode = "r", 
                             value = "ggplot(data = cars,\n    aes(x = speed, y = dist)) + \n    geom_point(col = 'red')"),
                   
                   downloadButton(
                     outputId = ns("downloadCode"), 
                     label    = "Download Code")
          ),
          
          tabPanel("How To",
                   list(
                     h5("1. Upload your data file(s) in CSV format. You may use multiple files in your plot, but you must upload them one at a time."),
                     h5("2. Enter your ggplot2 code on the 'Editor' tab. Enter only the command to create your plot. All data manipulation should be done prior to using this app."),
                     h5("3. Click the 'Update Plot' button on the 'Plot' tab to see the results."),
                     h5("4. If you need to consult ggplot2 docs or resources, use the links on the 'Debug' tab."),
                     h5("5. Repeat steps 2-4 as necessary until the plot is complete."),
                     h5("6. Download the plot using the options on the 'Download' tab."),
                     h5("7. Download the code using the download button on the 'Editor' tab."),
                     p()
                   )
          )
        )
        
        ##### #######
        ,div(HTML("Text"))
        ##############
      ),
      
      mainPanel(
        tabsetPanel(
          
          tabPanel("Data Manager",
                   # User uploads data file here.
                   fileInput(
                     inputId = ns("uploadFile"),
                     label   = "Upload a CSV data file. You may upload multiple files and use them all in your plot, but you must upload them one at a time. Note: If you upload a file with a name that's already being used, the app will append a numeric suffix to ensure no data is overwritten."),
                   
                   h5("Data available:"),
                   
                   # This is returned once a valid data.frame name is entered
                   # above. It allows the user to pick an x-variable and y-variable
                   # from the column names of the selected data.frame.
                   uiOutput(outputId = ns("dataInfo"))
          ),
          
          tabPanel("Plot",
                   # Show the plot itself.
                   plotOutput(outputId = ns("plot")),
                   
                   # Button to update plot.
                   actionButton(ns("plotButton"), "Update Plot")
                   
          ),
          tabPanel("Knitr",
                   
             ########      
             div(
               class="container-fluid",
               div(class="row-fluid",
                   div(class="span6",
                       h2("Source R-Markdown"),  
                       aceEditor(ns("rmd"), mode="markdown", value='### Sample knitr Doc

This is some markdown text. It may also have embedded R code
which will be executed.

```{r}
2*3
rnorm(5)
```

It can even include graphical elements.

```{r}
hist(rnorm(100))
```

'),
                       actionButton(ns("eval"), "Update")
                   ),
                   div(class="span6",
                       h2("Knitted Output"),
                       htmlOutput(ns("knitDoc"))
                   )
                   )
                   )
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
           
                   
                   
          ),
          
          tabPanel("Download", 
                   
                   # Allow the user to choose the download file type.
                   selectInput(
                     inputId = ns("downloadPlotType"),
                     label   = h5("Select download file type"),
                     choices = list(
                       "PDF"  = "pdf",
                       "BMP"  = "bmp",
                       "JPEG" = "jpeg",
                       "PNG"  = "png")),
                   
                   # Allow the user to set the height and width of the plot download.
                   h5(HTML("Set download image dimensions<br>(units are inches for PDF, pixels for all other formats)")),
                   
                   numericInput(
                     inputId = ns("downloadPlotHeight"),
                     label = "Height (inches)",
                     value = 7,
                     min = 1,
                     max = 100),
                   
                   numericInput(
                     inputId = ns("downloadPlotWidth"),
                     label = "Width (inches)",
                     value = 7,
                     min = 1,
                     max = 100),
                   
                   # Choose download filename.
                   textInput(
                     inputId = ns("downloadPlotFileName"),
                     label = h5("Enter file name for download")),
                   
                   div(),
                   
                   # File downloads when this button is clicked.
                   downloadButton(
                     outputId = ns("downloadPlot"), 
                     label    = "Download Plot")
          )
          
          ,
          
          tabPanel("RMarkdown",
                   
                   uiOutput(ns("markdown"))
                   
          )
        )
      )
    )  
    
    
   



###################3
}



CodePlot <- function(input, output, session) {
#server <- function(input, output) {

  
  #####
  # Code download
  #####
  
  # Include a downloadable file of the plot in the output list.
  output$downloadCode <- downloadHandler(
    filename = function() {
      paste("ggplot2Code.R")   
    },
    # The argument content below takes filename as a function
    # and returns what's printed to it.
    content = function(con) {
      codeLines <- unlist(split(input$plotCode, "\n"))
      writeLines(codeLines, con)
    }
  )
  
  #####
  # Data tab
  ##### 
  
  # Returns the name of the data file uploaded by the user.
  getDataName <- reactive({
    # Get name from input, remove .csv, then ensure it's a syntactically-valid object name.
    out <- input$uploadFile$name
    if (length(out) > 0) {
      out <- sub(".csv", "", out)
      out <- make.names(out)
    } else out <- NULL
    out
  })
  
  getDataPath <- reactive({
    # Get data path, will be used to read data file.
    input$uploadFile$datapath
  })
  
  # Returns the data file uploaded by the user.
  getData <- reactive({
    if (!is.null(getDataPath())) {
      out <- read.csv(getDataPath(), 
                      header = TRUE,
                      stringsAsFactors = FALSE)
    } else out <- NULL
    out
  })
  
  updateDataList <- reactive({
    if (!is.null(getDataName())) {
      dataName <- getDataName()
      
      # Defaults to using file name as dataName, but will
      # avoid overwriting a previously uploaded data by appending a numeric suffix.
      if (exists(dataName)) {
        suffix <- 1L
        while (exists(paste(dataName, suffix, sep=""))) suffix <- suffix + 1L
        dataName <- paste(dataName, suffix, sep="")
      }
      
      dataList[[dataName]]$name      <<- dataName
      dataList[[dataName]]$colnames  <<- colnames(getData())
      assign(dataName, getData(), envir = .GlobalEnv)
      
      out <- dataList
    } else out <- NULL
    out
    
  })
  
  output$dataInfo <- renderUI({
    if (length(updateDataList()) == 0L) out <- HTML("<pre>No data files have been uploaded yet.</pre>")
    else {
      out <- lapply(
        updateDataList(),
        FUN = function(x) {
          HTML(sprintf("<pre><strong>data.frame:</strong> %s<p><strong>column names: </strong>%s</pre>", x$name, paste(sort(x$colnames), collapse=", "))) 
        })
    }
    out
  })
  
  #####
  # Help tab
  #####
  
  helpFunction <- reactive({
    input$helpFunctionSelect
  })
  
  output$helpFunctionFormals <- renderUI({
    list(
      strong(sprintf("Formal arguments for function %s:", helpFunction())),
      HTML(paste(names(formals(match.fun(helpFunction()))), collapse=", "))
    )
  })
  
  
  #####
  # Plotting tab
  #####
  
  # Get plot code from the aceEditor input object, and remove line breaks from it.
  plotCode <- reactive({
    input$plotCode
  })        
  
  # Create a plot object from the code in plotCode()
  plotObject <- reactive({
    plotNo <- input$plotButton
    isolate(eval(parse(text = gsub("\\n", "", plotCode()))))
  })
  
  # Include the printed plot in the output list.
  output$plot <- renderPlot({
    print(plotObject())
  })
  
  
  
  ########33
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('slides.rmd', quiet = TRUE)))
  })
  
  #####
  # Download tab
  #####
  
  # Get the selected download file type.
  downloadPlotType <- reactive({
    input$downloadPlotType  
  })
  
  observe({
    plotType    <- input$downloadPlotType
    plotTypePDF <- plotType == "pdf"
    plotUnit    <- ifelse(plotTypePDF, "inches", "pixels")
    plotUnitDef <- ifelse(plotTypePDF, 7, 480)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotHeight",
      label = sprintf("Height (%s)", plotUnit),
      value = plotUnitDef)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotWidth",
      label = sprintf("Width (%s)", plotUnit),
      value = plotUnitDef)
    
  })
  
  
  # Get the download dimensions.
  downloadPlotHeight <- reactive({
    input$downloadPlotHeight
  })
  
  downloadPlotWidth <- reactive({
    input$downloadPlotWidth
  })
  
  # Get the download file name.
  downloadPlotFileName <- reactive({
    input$downloadPlotFileName
  })
  
  # Include a downloadable file of the plot in the output list.
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(downloadPlotFileName(), downloadPlotType(), sep=".")   
    },
    # The argument content below takes filename as a function
    # and returns what's printed to it.
    content = function(con) {
      # Gets the name of the function to use from the 
      # downloadFileType reactive element. Example:
      # returns function pdf() if downloadFileType == "pdf".
      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(plotObject())
      dev.off(which=dev.cur())
    }
  )
  
  ######
  # RMarkdown Panel
  #####
  
  output$knitDoc <- renderUI({
    input$eval
    return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = TRUE))))
  })  
  
  
  
  
  
  
  ##########3

}

#shinyApp(ui, server)