library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  div(class = "title-style", 
      tags$h1("MGTA 452 Collecting and Analyzing Large Data (We will rename this later)", 
              style = "text-align: center; font-size: 32px; margin-bottom: 30px;")), 
  h2("US Census Data Analysis Project (Group C). (We will rename this later)", 
     style = "text-align: center; font-size: 20px; margin-bottom: 20px;"), 
  div(style = "text-align: right;", 
      tags$img(src = "rady_logo.png", height = 100, width = 430)),
  navbarPage("",
             tabPanel("Introduction",
                      tabsetPanel(
                        tabPanel("Project Topic Introduction",
                                 style = "margin-top: 20px;",
                                 p("This is where we are going to introduce our Data; US Census data and explain what we are aim to accomplish throughout this project")
                        ),
                        tabPanel("Group C Introduction",
                                 style = "margin-top: 20px;",
                                 tags$style(type="text/css", 
                                            ".team-member {display: flex; align-items: center; margin-bottom: 20px; text-align: left;}",
                                            ".team-member img {margin-right: 15px;}",
                                            ".team-member .info {text-align: left;}",
                                            ".team-member .info h3, .team-member .info p {margin: 0;}",
                                            ".team-member .info p {font-size: 16px;}"),
                                 div(class = "container", 
                                     div(class = "team-member",
                                         img(src = "Augie.png", height = 400, width = 300, alt = "Augie"),
                                         div(class = "info",
                                             h3("Augustine Donovan"),
                                             p("audonovan@ucsd.edu")
                                         )),
                                     div(class = "team-member",
                                         img(src = "Alan.png", height = 400, width = 300, alt = "Alan"),
                                         div(class = "info",
                                             h3("Alan Shapiro"),
                                             p("a1shapiro@ucsd.edu")
                                         )),
                                     div(class = "team-member",
                                         img(src = "Robin.png", height = 400, width = 300, alt = "Robin"),
                                         div(class = "info",
                                             h3("Robin Reese"),
                                             p("Robin.Reese@rady.ucsd.edu", style = "font_size: 16px;")
                                         )),
                                     div(class = "team-member",
                                         img(src = "Sangho.png", height = 400, width = 300, alt = "Sangho"),
                                         div(class = "info",
                                             h3("Sangho Lee"),
                                             p("sal072@ucsd.edu", style = "font-size: 16px;")
                                         )),
                                     div(class = "team-member",
                                         img(src = "Shifali.png", height = 400, width = 300, alt = "Shifali"),
                                         div(class = "info",
                                             h3("Shefali Sinha"),
                                             p("s3sinha@ucsd.edu")
                                         ))
                                 )
                        )
                      )
             ),
             tabPanel("Index"),
             tabPanel("Predictive Analysis",
                      h3("** Predictive Analysis **"),
                      h5("1. *What will be the demographic makeup of major US cities in 10 years?* - Projecting future changes in ethnicity, age distribution, and migration patterns."),
                      h5("2. *How will the aging population affect the job market by 2030?* - Predicting shifts in employment sectors due to an increasing number of retirees."),
                      h6("Currently, there are only two bullet points based on Augie's questions. More detailed content and analytical visualizations will be added once we have access to the data")),
             tabPanel("Descriptive Analysis",
                      h3("** Descriptive Analysis **"),
                      h5("1. *Which state has the most unusual ratio of people to domestic pets?* - Investigating the correlation between human population and pet ownership."),
                      h5("2. *Is there a state where people have an unusually high number of hobbies or leisure activities?* - Analyzing census data on leisure activities and comparing across states."),
                      h6("Currently, there are only two bullet points based on Augie's questions. More detailed content and analytical visualizations will be added once we have access to the data")),
             tabPanel("Regression Analysis",
                      h3("** Regression Analysis **"),
                      h5("1. *Do states with higher numbers of pet owners have lower stress-related health issues?* - Exploring the correlation between pet ownership (from other sources) and health data from the census."),
                      h5("2. *Is there a relationship between the number of fast-food restaurants in an area and the number of single-person households?* - Combining census data on household composition with external data on fast-food locations."),
                      h6("Currently, there are only two bullet points based on Augie's questions. More detailed content and analytical visualizations will be added once we have access to the data")),
             tabPanel("Conclusion")
  )
)

server <- function(input, output) { }

shinyApp(ui = ui, server = server)
