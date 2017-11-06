source("global.R")




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
  plotOutput("plot3", height = 250)
),

box(
  title = "Inputs", status = "warning", solidHeader = TRUE,
  "Box content here", br(), "More box content",
  sliderInput("slider", "Slider input:", 1, 100, 50),
  textInput("text", "Text input:")
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
        tabPanel("Tab2", "Tab content 2")
        ),
        tabBox(
        side = "right", height = "250px",
        selected = "Tab3",
        tabPanel("Tab1", "Tab content 1"),
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




    #Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(stopApp)
}



shinyApp(ui, server)