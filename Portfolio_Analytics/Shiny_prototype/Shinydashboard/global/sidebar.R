sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar", #bookmarking
              menuItem("Login", tabName = "login", icon = icon("user")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Symbol", tabName = "symbol", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green"),
   menuItem("GDP", icon = icon("th"), tabName = "GDP",
             badgeLabel = "new", badgeColor = "green"),

             ##dynamic
    menuItemOutput("menuitem")
 )
)