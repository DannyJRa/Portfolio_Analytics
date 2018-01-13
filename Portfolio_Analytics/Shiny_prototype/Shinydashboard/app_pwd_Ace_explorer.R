source("global.R")
source("updatedScript.R")
Logged = FALSE
my_username <- ""
my_password <- ""
library(ggedit)
p=ggplot(iris,aes(x =Sepal.Length,y=Sepal.Width))
p=p+geom_point(mapping=aes(colour=Species),alpha=1)+geom_line()+
  scale_colour_manual(values=c('#2E1815','#008B45','#6495ED'))
ui <- dashboardPage(skin = "blue",

## Header
  dashboardHeader(
  title = "My Dashboard",
  titleWidth = 350,

## Message menus (static)
      dropdownMenu(type = "messages",
        messageItem(
        from = "Sales Dept",
        message = "Sales are steady this month."
        ),
        messageItem(
        from = "New User",
        message = "How do I register?",
        icon = icon("question"),
        time = "13:45"
        ),
        messageItem(
        from = "Support",
        message = "The new server is ready.",
        icon = icon("life-ring"),
        time = "2014-12-01"
        )
        )
      # Test ##############
      
       ,
      dropdownMenu(type = "notifications",
          notificationItem(
            text = "5 new users today",
            icon("users")
          ),
          notificationItem(
            text = "12 items delivered",
            icon("truck"),
            status = "success"
          ),
          notificationItem(
            text = "Server load at 86%",
            icon = icon("exclamation-triangle"),
            status = "warning"
          )
        )

        ##############
        ,


##### Task menu ###########
        dropdownMenu(type = "tasks", badgeStatus = "success",
          taskItem(value = 90, color = "green",
            "Documentation"
          ),
          taskItem(value = 17, color = "aqua",
            "Project X"
          ),
          taskItem(value = 75, color = "yellow",
            "Server deployment"
          ),
          taskItem(value = 80, color = "red",
            "Overall project"
          )
        )
#########

  ),

## Sidebar content
    sidebar,

  ## Body content
dashboardBody(
    tabItems(
# Login tab
      tabItem(tabName = "login",
         verbatimTextOutput("dataInfo")),
      
# First tab content
      tabItem(tabName = "dashboard",
        fluidRow(
          box(
            plotOutput("plot1", height = 250)),

          box(
            "Box content here", br(), "More box content",
            #Slider Input
            sliderInput("slider", "Slider input:", 1, 100, 50),
            #Text Input
            textInput("text", "Text input:")
            ),
         #### # Second Row
         fluidRow(
            box(
                title = "Histogram", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("byQuarter", height = 250)
            ),
            box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                "Box content here", br(), "More box content",
                sliderInput("slider", "Slider input:", 1, 100, 50),
                textInput("text", "Text input:")
        ),

        ################# Input symbol
        box(
            title = "Inputs2", status = "warning", solidHeader = TRUE,
            selectInput("symbol", "Symbol:", 
              choices=ticker),
            hr(),
            helpText("Data from AT&T (1961) The World's Telephones.")
        )

        )


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
          tabPanel("Tab1", tableOutput('tbl2')),
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
            tabPanel("Tab2", "Tab content 2")
          )
        )
      ),
##############

      # GDP tab content
      tabItem(tabName = "GDP",
              
        tabsetPanel(id = "continent",
            tabPanel("Test",ggEditUI('pUI')),
            tabPanel("Asia", gapModuleUI("asia")),
            tabPanel("Europe", tableTestUI("test")),
            tabPanel("Oceania", DT::dataTableOutput("tbl")),
            tabPanel("Codeplot", CodePlotUI("codeplot")),
            tabPanel("Codeplot2", CodePlotUI("codeplot2")),
            tabPanel("NoModul",
                     
                     
                     
                     tabPanel("Knitr",
                              
                             
                              ########      
                              div(
                                class="container-fluid",
                                div(class="row-fluid",
                                    div(class="span6",
                                        h2("Source R-Markdown"),  
                                        aceEditor("rmd", mode="markdown", value="global.R"),
                                        actionButton("eval", "Update")
                                    ),
                                    div(class="span6",
                                        h2("Knitted Output"),
                                        htmlOutput("knitDoc")
                                    )
                                )
                              )  )    
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     )
        )
        
      )




    )

)
)

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





######## plot 1 ############
    set.seed(122)
    histdata <- rnorm(500)

    output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
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
    
    source("Portfolio_Scenarios/SingleAssets.R")
    
    output$plot3 <- renderPlot({
    plot3
    })

#### byQuarter ###########
    output$byQuarter <- renderPlot({
      source("Portfolio_Scenarios/plot_SymbolbyQuarter.R",local=T)
      byQuarter
    })

##### byQuarterAll #########    
        output$byQuarterAll <- renderPlot({
      byQuarterAll
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
      rmdsource = paste(readLines("ggplot_new.rmd"), collapse="\n")
      rmdsub = dkReplace(rmdsource, c(mydf=dfstr, input1=factors[1]))
      
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

####### Shiny Options #################    
    
    #Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(stopApp)
}

shinyApp(ui, server)