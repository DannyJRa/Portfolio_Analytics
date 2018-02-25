sidebar <- dashboardSidebar(
  
  sidebarUserPanel(
    "Danny",
    subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
    # Image file should be in www/ subdir
    image = "userimage.png"
  ),
 # sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
  sidebarMenu(id = "sidebar", #bookmarking
              menuItem("Login", tabName = "login", icon = icon("user")),
              
              ## slider

    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
   ##dynamic 
  # menuItem("Menu Item 2", tabName = "tab2", textOutput("text2"), startExpanded = FALSE,
  #          expandedName = "expanded"),
  
  #  sidebarMenuOutput("menu"),
             
   # menuItemOutput("menuitem"),
   
   
   menuItem("Inputs", icon = icon("bar-chart-o"),
            # Input directly under menuItem
            selectInput("inputTest", "Input Test",
                        choices = StartDateDay, multiple=TRUE, selectize=TRUE,
                        width = '98%')
            )
   )
 )
