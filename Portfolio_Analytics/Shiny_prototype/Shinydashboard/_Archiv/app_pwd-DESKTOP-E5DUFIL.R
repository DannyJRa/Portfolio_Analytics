source("global.R")

Logged = FALSE
my_username <- "test"
my_password <- "test"


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
      ##
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


        ### Task menu
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
        ###########3
        ########3
    
  ),

    ## Sidebar content
    sidebar,

  ## Body content
dashboardBody(
    tabItems(
      
      tabItem(tabName = "login", verbatimTextOutput("dataInfo")),
      
# First tab content
      tabItem(tabName = "dashboard",
        fluidRow(


          box(plotOutput("plot1", height = 250)),


          box(
            "Box content here", br(), "More box content",
            sliderInput("slider", "Slider input:", 1, 100, 50),
            textInput("text", "Text input:")
            )

            ,

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

# Second tab content
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


      # GDP tab content
tabItem(tabName = "GDP",
        tabsetPanel(id = "continent",
        tabPanel("Test"),
            tabPanel("Asia", gapModuleUI("asia")),
            tabPanel("Europe", tableTestUI("test")),
            tabPanel("Oceania", DT::dataTableOutput("tbl"))#,
           # tabPanel("test", DT::dataTableOutput("tbl")),
        )
        
      )




    )

)
)

server <- function(input, output,session) {

  ##### Password
  
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
  
  
  
  
  
    output$tbl2 <- renderTable({
                head(rock, n = 6) },
                 digits = 2)



    ##menu item
    output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
    })






    set.seed(122)
    histdata <- rnorm(500)

    output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
     })


    callModule(gapModule, "asia", asia_data)
    callModule(gapModule, "europe", europe_data)
    callModule(gapModule, "oceania", oceania_data)
    callModule(tableTest, "test")

    
    
    
    output$tbl = DT::renderDataTable(
      #iris, options = list(lengthChange = FALSE)
      Quotes_by_qtr
      )
    ########
    ##
    ######
    source("Portfolio_Scenarios/SingleAssets.R")
    output$plot3 <- renderPlot({
    plot3
    })
    #########
    #### Plot
    ##################################################################################
    output$byQuarter <- renderPlot({
      source("Portfolio_Scenarios/plot_SymbolbyQuarter.R",local=T)
      byQuarter
    })
    ###################################################################################
    output$byQuarterAll <- renderPlot({
      byQuarterAll
    })
    
    
    
    
    

    
    
    
    
    #Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(stopApp)
}



shinyApp(ui, server)