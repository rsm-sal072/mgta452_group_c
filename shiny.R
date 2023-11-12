# Libraries
library(shiny)
library(shinydashboard)
library(shinythemes)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "MGTA 452 Collecting and Analyzing Large Data",
                  tags$li(class = "dropdown",
                          tags$img(src = "rady_logo.png", height = "50px", style = "padding-top:10px; padding-left:20px;"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Directions", tabName = "directions", icon = icon("directions")),
      menuItem("Page 2", tabName = "page2"),
      menuItem("Page 3", tabName = "page3"),
      menuItem("Page 4", tabName = "page4"),
      menuItem("Page 5", tabName = "page5")
    )
  ),
  dashboardBody(
    shinytheme("flatly"),
    tabItems(
      tabItem(tabName = "directions",
              h2("Directions"),
              p("Here are the directions..."),
              p("Group Members: Name1, Name2, Name3...")),
      tabItem(tabName = "page2",
              h2("Page 2 Content")),
      tabItem(tabName = "page3",
              h2("Page 3 Content")),
      tabItem(tabName = "page4",
              h2("Page 4 Content")),
      tabItem(tabName = "page5",
              h2("Page 5 Content"))
    )
  )
)

# Server
server <- function(input, output) { }

# Run the app
shinyApp(ui, server)
