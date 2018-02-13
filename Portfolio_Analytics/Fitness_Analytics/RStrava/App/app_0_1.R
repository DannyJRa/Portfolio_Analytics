source("global.R")
##
#Source: https://stackoverflow.com/questions/43404058/starting-shiny-app-after-password-input-with-shinydashboard
library(shiny)
library(shinydashboard)
library(leaflet)

Logged = FALSE
my_username <- ""
my_password <- ""

ui <- dashboardPage(skin='blue',
       
                       
                    ## Header
                    header,
                    
                    ## Sidebar content
                    sidebar,
                    
                    ## Body content
                  dashboardBody("Test",

                    
                                 
                    
                               tags$h1(
                                        "My header"
                                      )
                               ,
                              tags$div(
                              "Some text followed by a break",
                              tags$br(),
                              "Some text following a break"
                              ),



                                  # actionButton("show", "Login"),
                                #  verbatimTextOutput("dataInfo"),
                                  numericInput("run", "Observations:", 10, min = 1, max = 100),
                                  verbatimTextOutput("value"),
                                  plotOutput('plot1'),
                              leafletOutput('plot_basic'),
                
                                  
                                  tags$div(
                                    HTML('<div id="u560"><!-- group -->
      <div class="clearfix" id="u560_align_to_page">
                                         <div class="rounded-corners grpelem" id="u600"><!-- simple frame --></div>
                                         <div class="clearfix grpelem" id="pu641-4"><!-- column -->
                                         <div class="clearfix colelem" id="u641-4"><!-- content -->
                                         <p>Visit us</p>
                                         </div>
                                         <div class="clearfix colelem" id="u640-4"><!-- content -->
                                         <p>Park Ave, San Jose, CA</p>
                                         </div>
                                         </div>
                                         <div class="rounded-corners grpelem" id="u617"><!-- simple frame --></div>
                                         <div class="clearfix grpelem" id="pu635-4"><!-- column -->
                                         <div class="clearfix colelem" id="u635-4"><!-- content -->
                                         <p>Email us</p>
                                         </div>
                                         <div class="clearfix colelem" id="u634-4"><!-- content -->
                                         <p>hi@website.com</p>
                                         </div>
                                         </div>
                                         <div class="rounded-corners grpelem" id="u613"><!-- simple frame --></div>
                                         <div class="clearfix grpelem" id="pu612-4"><!-- column -->
                                         <div class="clearfix colelem" id="u612-4"><!-- content -->
                                         <p>Call us</p>
                                         </div>
                                         <div class="clearfix colelem" id="u631-4"><!-- content -->
                                         <p>800 456 45 4545</p>
                                         </div>
                                         </div>
                                         </div>
                                         </div>')
                                  ),
                                  a="5",
                                  tags$h1("paste0(a)"),
                               #  ,tags$div(data-value = "test")      # bad
                                 tags$div(`data-value` = "test"),    # good
                               
                               
                               attendeeForm <- HTML('
  <p>
                                                    
                                                    <label for="name">Name: </label>
                                                    <input style="width: 150px;" id="name" type="text" placeholder="Enter name", class="name">
                                                    
                                                    <!--Department field-->
                                                    <label for="department">Department: </label>
                                                    <input style="width: 150px;" id="department" type="text" placeholder="Enter department", class="department">
                                                    
                                                    <!--Email field-->
                                                    <label for="email">Email: </label>
                                                    <input style="width: 150px;" id="email" type="text" placeholder="example@email.com", class="email">
                                                    
                                                    <!--Remove button-->
                                                    <input type = "button" class="remover" value = &#10008>
                                                    </p>
                                                    '),
                               # Using tags$ each time
                               tags$div(class = "myclass",
                                        tags$h3("header"),
                                        tags$p("text")
                               ),
                               # Equivalent to above, but using withTags
                               withTags(
                                 div(class = "myclass",
                                     h3("header"),
                                     p("text")
                                 )
                               ),
                               
                               tags$head(tags$style(HTML('
   div.columns       { width: 900px; }
div.columns div   { width: 300px; height: 100px; float: left; }
                                                         div.grey          { background-color: #cccccc; }
                                                         div.red           { background-color: #e14e32; }
                                                         div.clear         { clear: both; }
                                                         '))),
                               tags$div(
                                 HTML('
<div class="columns">
    <div id=dataInfo2 class="shiny-html-output"></div>
                                      <div class="grey">Column 2</div>
                                      <div class="red" >Column 3</div>
                                      </div>
                                      <div class="clear"></div>

                             
                               '),
                                 absolutePanel(id = "controls", class = "panel panel-default", fixed =     TRUE, 
                                               style="padding-left: 8px; padding-right: 8px; padding-top: 8px; padding-bottom: 8px",
                                               draggable = TRUE, top = 126, left = "auto", right = 20, bottom = "auto",
                                               width = 250, height = "auto",
                                               uiOutput("textBox", width = 10),
                                               br(),
                                               htmlOutput("dataInfo"))

                               
                               )

 

                               )
                    
                    
                    
                    
                
)

server = function(input, output,session) {
  
    #### Password modal box
    #source("server/password.R", local = T)
  

    ###Save Input if changes
    #Source: http://www.programfaqs.com/faq/reactive-variables-and-input-in-r-shiny-save-everything-but-not-every-time/
    observeEvent(reactiveValuesToList(input),
    {

        lapply(names(reactiveValuesToList(input)), function(item)
        {
            saveRDS(input[[item]],paste("test",item,"rds",sep = "."))
        })

    }

  )


    ####################
    source("server/server.R", local = T)
  
  
  
    #Km for selected run for POPUP
    output$dataInfo <- renderPrint(
    {
     runNo = input$run
     run = compile_activity(my_acts[runNo])
     run_distance = run$distance
     run_distance
  })
    

    #Km for selected run
    output$dataInfo2 <- renderText(
    {
    runNo = input$run
    run = compile_activity(my_acts[runNo])
    run_distance = as.double(run$distance)
    paste0(round(run_distance, 0), " m")
    })
    
    
    #Plot: Average speed by miles
    output$plot1 <- renderPlot(
    {
    source("server/plot1.R", local = T)
    #plot output here not in source file; doesnt work
    plot1
    })
    

    # Test
    output$value <- renderText(
    {
    input$obs
    })
    
    #Plot: Average speed by miles
    output$plot_basic <- renderPlot(
     {
     
     #plot output here not in source file; doesnt work
     plot_basic
     })




 

 
  
}

shinyApp(ui,server)