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
              sliderInput("slider3", "Slider input:", 1, 100, 50),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Symbol", tabName = "symbol", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green"),
   menuItem("GDP", icon = icon("th"), tabName = "GDP",
             badgeLabel = "new", badgeColor = "green"),
   ##dynamic 
  # menuItem("Menu Item 2", tabName = "tab2", textOutput("text2"), startExpanded = FALSE,
  #          expandedName = "expanded"),
  
  #  sidebarMenuOutput("menu"),
             
   # menuItemOutput("menuitem"),
   
   
   menuItem("Inputs", icon = icon("bar-chart-o"),
            # Input directly under menuItem
            selectInput("inputTest", "Input Test",
                        choices = c("a", "b", "c", "d"), multiple=TRUE, selectize=TRUE,
                        width = '98%'),
            
            # Input inside of menuSubItem
            menuSubItem(icon = NULL,
                        sliderInput("inputTest2", "Input test 2", min=0, max=10, value=5,
                                    width = '95%'),
                        #########
                        #### ?????????????????????????????????????????
                        ##########
            menuSubItem("Test5",icon = NULL, "Test")
                    
            )
   )
 )
)