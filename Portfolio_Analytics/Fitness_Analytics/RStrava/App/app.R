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
  dashboardBody(
    tabItems(
      # Login tab
      tabItem(tabName = "login",
              verbatimTextOutput("dataInfo")
      ),
      
      tabItem(tabName = "summary",
                ######
                
                box(
                  plotOutput("plot_summary",width = 500)
                )
                ######## 



      ),
      
      # First tab content
      tabItem(tabName = "dashboard",

      fluidRow(
# A static infoBox
      infoBoxOutput("approvalBox"),
      infoBox("Distanz", 10 * 2, icon = icon("credit-card")),
# Dynamic infoBoxes
      infoBoxOutput("progressBox")

    ),


              fluidRow(
                ######
                box(
                  numericInput("run", "Observations:", 10, min = 1, max = 100)
                )
                #######
                
                
                ######
                ,
                box(
                  "Test"
                )
                ######
                
                
                ######
                ,
                box(
                  plotOutput("plot1", height = 250)
                )
                ########
                
   

                ######
                ,
                box(
                  plotOutput("plot_heatmap", height = 250)
                )
                ########  


              ,
                box(
                 leafletOutput('plot_basic')
                 )
              )
      )   
    )
    #close body
  )
                    
                    
                    
#close ui                
)




server = function(input, output,session) {

  ##### load server ###############
  source("server/server.R", local = T)
    
    
#### Password modal box #######
#source("server/password.R", local = T)
################################  
  
  
  
  
  
###############################################  
##########Save input to rds#########
    ###Save Input if changes
    #Source: http://www.programfaqs.com/faq/reactive-variables-and-input-in-r-shiny-save-everything-but-not-every-time/
  #   observeEvent(reactiveValuesToList(input),
  #   {
  # 
  #       lapply(names(reactiveValuesToList(input)), function(item)
  #       {
  #           saveRDS(input[[item]],paste("test",item,"rds",sep = "."))
  #       })
  # 
  #   }
  # 
  # )
  ######################################
  ######################################



    # Km for selected run for POPUP ####
    output$dataInfo <- renderPrint(
    {
     runNo = input$run
     run = compile_activity(my_acts[runNo])
     run_distance = run$distance
     run_distance
  })
    

    #Km for selected run ####
    output$dataInfo2 <- renderText(
    {
    runNo = input$run
    run = compile_activity(my_acts[runNo])
    run_distance = as.double(run$distance)
    paste0(round(run_distance, 0), " m")
    })
    
    
    #Plot: Average speed by miles ####
    output$plot1 <- renderPlot(
    {
    source("server/plot1.R", local = T)
    #plot output here not in source file; doesnt work
    plot1
    })


    #Plot: Summary ####
    output$plot_summary <- renderPlot(
    {
    source("server/plot_summary.R", local = T)
    #plot output here not in source file; doesnt work
    plot_summary
    })

    #Plot: Heatmap ####
    output$plot_heatmap <- renderPlot(
    {
    source("server/plot_heatmap.R", local = T)
    #plot output here not in source file; doesnt work
    plot_heatmap
    })

######## llaflet
    output$plot_basic <- renderLeaflet(
      {
        
        #plot output here not in source file; doesnt work
        plot_basic
      })
    



    ########## INFO BOXES
    output$progressBox <- renderInfoBox(
{
    infoBox(
      "Hoehenmeter", paste0(25 + input$run, "%"), icon = icon("list"),
      color = "purple"
    )
})
    output$approvalBox <- renderInfoBox(
{
        runNo = input$run
        run = compile_activity(my_acts[runNo])
        run_distance = as.double(run$distance)
    infoBox(

      "Distanz", paste0(25 + run_distance, " km"), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
})

 

 
#### end server   
}

shinyApp(ui,server)