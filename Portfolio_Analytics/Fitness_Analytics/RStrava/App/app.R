##
#Source: https://stackoverflow.com/questions/43404058/starting-shiny-app-after-password-input-with-shinydashboard
library(shiny)
library(shinydashboard)

Logged = FALSE
my_username <- ""
my_password <- ""

ui <- dashboardPage(skin='blue',
                    dashboardHeader( title = "Dashboard"
                                     
                                                   
                                     ),
                    dashboardSidebar(),
                    dashboardBody("Test",
                                  # actionButton("show", "Login"),
                                #  verbatimTextOutput("dataInfo"),
                                  numericInput("run", "Observations:", 10, min = 1, max = 100),
                                  verbatimTextOutput("value"),
                                  plotOutput('plot1'),
                
                                  
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
                               
                               
                               ))
                    
                    
                    
                    
                
)

server = function(input, output,session) {
  
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
  
  
  ####################
  library(rStrava)
  path="R:/5_IT/5_Secrets/"
  load(paste0(path,"Strava_stoken.Rdata"))
  
  # get activities, get activities by location, plot
  my_acts <- get_activity_list(stoken)
  
  
  
  
  
  output$dataInfo <- renderPrint({
    
    runNo=input$run
        run=compile_activity(my_acts[runNo])
    
    run_distance=run$distance
    
    run_distance
    
  })
    

  
  output$dataInfo2 <- renderText({
    
    runNo=input$run
    run=compile_activity(my_acts[runNo])
    
    run_distance=as.double(run$distance)
    paste0(round(run_distance,0)," m")
  })
    
    
  #  source("data.R",local=T)
    output$plot1 <- renderPlot({
      runNo=input$run
      # plots for most recent activity
      test=get_spdsplits(my_acts, stoken, acts = runNo, units = 'imperial')
      test
    })
    
    
    output$value <- renderText({ input$obs }) 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
 
  
}

shinyApp(ui,server)