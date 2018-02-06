source("global.R")
source("updatedScript.R")
library(shinyjqui)
library(tidyverse)
library(openxlsx)
Logged = FALSE
my_username <- ""
my_password <- ""
library(ggedit)
library(brew)



ui <- dashboardPage(skin = "blue",
                    
                    ## Header
                    header,
                    
                    ## Sidebar content
                    sidebar,
                    
                    ## Body content
                    dashboardBody(
                      tabItems(
                        # Login tab
                        tabItem(tabName = "login",
                                verbatimTextOutput("dataInfo")
                                ),
                        
                        # First tab content
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  box(
                                    "Box1",
                                  
                                  box(
                                    "Box content here", br(), "More box content"
                                   
                                      )     
                                      
                                    )       
                                 )
                                  )
                        ),
                        
                        # First tab content
                        tabItem(tabName = "symbol",
                                fluidRow(
                                  box(width = 12,
                                      plotOutput("plotMACD", height = 250))
                                ),
                                fluidRow(
                                  # A static valueBox
                                  valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
                                  
                                  # Dynamic valueBoxes
                                  valueBoxOutput("progressBox"),
                                  
                                  valueBoxOutput("approvalBox")
                                )
                        ),
                        
                        # Second tab content #########
                        tabItem(tabName = "widgets",
                                fluidRow(
                                  tabBox(
                                    title = "First tabBox",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1", height = "250px",
                                    tabPanel("Tab1", "First tab content"),
                                    tabPanel("Tab2", plotOutput("byQuarterAll", height = 250))
                                  ),
                                  tabBox(
                                    side = "right", height = "250px",
                                    selected = "Tab3",
                                    tabPanel("Tab1", tableOutput('allocation')),
                                    tabPanel("Tab2", "Tab content 2"),
                                    tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                                  )
                                ),
                                
                                
                                fluidRow(
                                  tabBox(
                                    # Title can include an icon
                                    title = tagList(shiny::icon("gear"), "tabBox status"),
                                    tabPanel("Tab1",
                                             "Currently selected tab from first box:",
                                             verbatimTextOutput("tabset1Selected")
                                    ),
                                    tabPanel("Tab2", 
                                             
                                             
                                             
                                             orderInput('source', 'Source', items = month.abb,
                                                        as_source = TRUE, connect = 'dest'),
                                             orderInput('dest', 'Dest', items = NULL, placeholder = 'Drag items here...'),
                                             verbatimTextOutput('order') 
                                             
                                             
                                             
                                             
                                             
                                             
                                    )
                                  )
                                )
                        ),
                        ##############
                        
                        # GDP tab content
                        tabItem(tabName = "GDP",
                                "test"
                               
                                )
                                
                        )
                        
                        
                        
                        
                      )
                      
                    














# Server logic -----------------------------------------------------------------
################ #
######### SERVER #############################
###################### #


server <- function(input, output,session) {
  
  ##### Password ###########
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  
  
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }     
    }
  })
  
  output$dataInfo <- renderPrint({
    if (values$authenticated) "You are logged in as User: Test"
    else "You are NOT authenticated"
  })
  
  #####################
  
  
  
  ###### tbl2 #############  
  
  output$tbl2 <- renderTable({
    head(rock, n = 6) },
    digits = 2)
  
  
  ########## Dynamic Menu Item #############
  ##menu item
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
  
  ###########
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"))
    )
  })
  
  
  
  ######## plot 1 ###################
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    saveRDS(input$slider,"slider.rds")
    source("plots/plot1.R",local=T) 
  })
  
  ######## plot rolling correlation ###################
  
  output$plotRolling <- renderPlot({
    saveRDS(input$symbol,"symbol.rds")
    source("plots/plot_rollingCorr.R",local=T) 
    plot_rollingCorr
  })
  
  ########## plot MACD
  output$plotMACD <- renderPlot({
    
    
    
    source("plots/plot_MACD.R", local = T)
    
    ######### Progress BAr
    ##### https://shiny.rstudio.com/articles/progress.html
    ##########
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Making plot", value = 0)
    Sys.sleep(3)
    
    ##################333
    plot_MACD
  })
  
  
  ###### Include Modules ###############
  callModule(gapModule, "asia", asia_data)
  callModule(gapModule, "europe", europe_data)
  callModule(gapModule, "oceania", oceania_data)
  callModule(tableTest, "test")
  callModule(CodePlot, "codeplot")
  callModule(CodePlot, "codeplot2")
  ####### tbl ###############    
  
  output$tbl = DT::renderDataTable(
    #iris, options = list(lengthChange = FALSE)
    Quotes_by_qtr
  )
  
  
  ######## plot 3 ############    
  
  source("global/SingleAssets.R")
  
  output$plot3 <- renderPlot({
    plot3
  })
  
  #### byQuarter ###########
  output$byQuarter <- renderPlot({
    source("global/plot_SymbolbyQuarter.R",local=T)
    byQuarter
  })
  
  ##### byQuarterAll #########    
  output$byQuarterAll <- renderPlot({
    byQuarterAll
  })
  
  
  ############# info boxes
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$slider, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })   
  
  
  
  ######
  # RMarkdown Panel
  #####
  
  output$knitDoc <- renderUI({
    input$eval
    #only updated within knitDoc
    source("updatedScript.R",local=T)
    return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = TRUE))))
  })  
  
  ####
  # Update AceEcditor
  #
  
  
  # Load a figure and open the Figure tab
  observeEvent(eval, {
    # get contents of figure R file
    ################################################
    
    dkReplace = function(mysource, myreplaces) {
      for (x in names(myreplaces)) {
        mysource = gsub(x, myreplaces[x], mysource)
      }
      return(mysource)
    }
    
    dfstr=mtcars
    factors=c("wt")
    rmdsource = paste(readLines("modules/1_scenarios/Portfolio_Scenarios.rmd"), collapse="\n")
    rmdsub = dkReplace(rmdsource, c(
      
      mydf=dfstr,
      input1=factors[1])
    )
    
    brewout = capture.output(brew(text=rmdsub))
    test=paste(brewout, collapse="\n")    
    
    
    updateAceEditor(session, "rmd", mode="markdown", value=paste(brewout, collapse="\n"))
    
    
    
    ####################################  
    
    
    #  x <- includeText("global.R")
    
    #  updateAceEditor(session, "rmd",
    #                  mode="markdown", value = x)
    
    #   		updateAceEditor(session, "code_only",
    # 											mode="r", value = x)
    
    updateTabsetPanel(session, "contitnent", selected = "Knitr")
  })
  
  
  ####
  ## ggedit
  ###
  
  callModule(ggEdit,'pUI',obj=reactive(p))
  
  
  ####
  output$text2 <- renderText(input$symbol)
  
  
  ####shinyjqui
  output$order <- renderPrint({ print(input$dest_order) })
  
  
  # Data Reactivity ------------------------------------------------------    
  
  
  cat1SalesByYear <- orders.extended %>%
    group_by(year, category1) %>%
    summarize(price.total = sum(price.extended), 
              qty.total   = sum(quantity))
  
  
  # Plots on Analysis Tab ------------------------------------------------
  
  # rChart - Sales By Category1
  output$primaryBikeCatOut <- renderChart({
    # Error handling
    # if (is.null(orders.extended.filtered())) return(rCharts$new())
    
    cat1SalesByYearOutDF <- nPlot(
      price.total ~ year,
      group = "category1",
      data = cat1SalesByYear,
      type = "multiBarChart",
      dom = "primaryBikeCatOut",
      width = 550
    )
    
    cat1SalesByYearOutDF$chart(margin = list(left = 85))
    cat1SalesByYearOutDF$yAxis(axisLabel = "Sales", width = 80,
                               tickFormat = "#! function(d) {return '$' + d/1000000 + 'M'} !#")
    cat1SalesByYearOutDF$xAxis(axisLabel = "Year", width = 70)
    cat1SalesByYearOutDF$chart(stacked = T)
    cat1SalesByYearOutDF
  })
  
  
  #####3 Using Scheduled Data in Shiny
  ########3
  
  logfilename="./reactiveData/AllocationCurrent.txt"
  # readLines(logfilename)
  #####3 Using Scheduled Data in Shiny
  ########3
  
  # ============================================================
  # This part of the code monitors the file for changes once per
  # 0.5 second (500 milliseconds).
  fileReaderData <- reactiveFileReader(500, session,
                                       logfilename, readLines)
  
  output$fileReaderText <- renderText({
    # Read the text, and make it a consistent number of lines so
    # that the output box doesn't grow in height.
    text <- fileReaderData()
    length(text) <- 14
    text[is.na(text)] <- ""
    paste(text, collapse = '\n')
  })
  
  
  
  
  
  
  #    updated_data <- reactiveFileReader(
  #  intervalMillis = 5000,
  #  filePath = "./reactiveData",
  #  readFunc = openxlsx::read.xlsx
  #)
  
  #    library(openxlsx)
  #   updated_data=read.xlsx("./reactiveData/AllocationCurrent.xlsx")
  
  
  #   output$allocation <- renderTable({
  #     as.data.frame(updated_data)
  #    }) 
  
  
  ##############
  chart1 <- reactive({
    ggplot(data = mtcars, aes(x=input$x, y=mpg))+geom_point()
  })
  
  output$regPlot <- renderPlot({
    chart1()
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  ####################3
  
  
  
  
  
  ####### Shiny Options #################    
  
  #Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)