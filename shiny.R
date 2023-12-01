library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)





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
                      h6("Currently, there are only two bullet points based on Augie's questions. More detailed content and analytical visualizations will be added later")),
             tabPanel("Descriptive Analysis",
                      tabsetPanel(
                        tabPanel("Introduction",
                                 h3("** Descriptive Analysis **"),
                                 h5("1. *Which state has the most unusual ratio of people to domestic pets?* - Investigating the correlation between human population and pet ownership."),
                                 h5("2. *Is there a state where people have an unusually high number of hobbies or leisure activities?* - Analyzing census data on leisure activities and comparing across states."),
                                 h6("Currently, there are only two bullet points based on Augie's questions. More detailed content and analytical visualizations will be added later")),
                        tabPanel("Data",
                                 br(),
                                 fluidRow(
                                   column(3, pickerInput("filterReport", "Report", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
                                   column(3, pickerInput("filterCategory", "Category", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
                                   column(3, pickerInput("filterLabel", "Label", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)))
                                 ),
                                 DT::dataTableOutput("dataView"),
                                 downloadButton("downloadData", "Download Data")
                        ),
                        tabPanel("Plot_Trend",
                                 br(),
                                 pickerInput("selectCategory", "Select Category", 
                                             choices = c("SEX", "AGE", "EDUCATIONAL ATTAINMENT", 
                                                         "EMPLOYMENT STATUS", "WORK EXPERIENCE", 
                                                         "RACE AND HISPANIC OR LATINO ORIGIN")),
                                 plotOutput("trendPlot", height = "800px", width = "100%")),
                        tabPanel("Plot_Race",
                                 br(),
                                 plotOutput("racePlot", height = "800px", width = "100%")
                        )
                      )),
             
             
             tabPanel("Regression Analysis",
                      h3("** Regression Analysis **"),
                      h5("1. *Do states with higher numbers of pet owners have lower stress-related health issues?* - Exploring the correlation between pet ownership (from other sources) and health data from the census."),
                      h5("2. *Is there a relationship between the number of fast-food restaurants in an area and the number of single-person households?* - Combining census data on household composition with external data on fast-food locations."),
                      h6("Currently, there are only two bullet points based on Augie's questions. More detailed content and analytical visualizations will be added later")),
             tabPanel("Conclusion")
  )
)

server <- function(input, output, session) {
  
  # Define the path to Data.R
  data_file_path <- "Data.R"
  
  
  
  
  # Check if the file exists before sourcing
  if (file.exists(data_file_path)) {
    source(data_file_path)
  } else {
    stop("The file Data.R was not found in the specified path: ", data_file_path)
  }
  
  
  
  
  observe({
    updatePickerInput(session, "filterReport", 
                      choices = sort(unique(all_data$Report)),
                      selected = all_data$Report)
    updatePickerInput(session, "filterCategory", 
                      choices = sort(unique(all_data$Category)),
                      selected = all_data$Category)
    updatePickerInput(session, "filterLabel", 
                      choices = sort(unique(all_data$Label)),
                      selected = all_data$Label)
    
  })
  
  output$dataView <- DT::renderDataTable({
    filtered_data <- all_data
    
    if (!is.null(input$filterReport)) {
      filtered_data <- filtered_data[filtered_data$Report %in% input$filterReport, ]
    }
    if (!is.null(input$filterCategory)) {
      filtered_data <- filtered_data[filtered_data$Category %in% input$filterCategory, ]
    }
    if (!is.null(input$filterLabel)) {
      filtered_data <- filtered_data[filtered_data$Label %in% input$filterLabel, ]
    }
    if (!is.null(input$filterYear)) {
      filtered_data <- filtered_data[all_data$Year >= input$filterYear[1] & all_data$Year <= input$filterYear[2], ]
    }
    
    datatable <- DT::datatable(filtered_data,
                               extensions = c("Buttons", "FixedHeader"), 
                               options = list(pageLength = 50,
                                              dom = "Blfrtip",
                                              buttons = c("copy", "csv", "excel"),
                                              scrollX = TRUE,
                                              scrollY = "700px",
                                              fixedHeader = TRUE,
                                              fixedColumns = list(leftColumns = 2)),
                               rownames = FALSE) %>%
      formatStyle('Report', backgroundColor = styleEqual(levels = unique(filtered_data$Report), values = rep("lightyellow", length(unique(filtered_data$Report)))))
    
    datatable
    
  })
  
  
  
  
  output$trendPlot <- renderPlot({
    category <- input$selectCategory
    title <- paste("US Poverty Rate by", category)
    
    data_to_plot <- all_data 
    
    if (category == "EMPLOYMENT STATUS") {
      data_to_plot <- all_data_employment_highlevel
    } else if (category == "WORK EXPERIENCE") {
      data_to_plot <- all_data_employment_lowlevel
    }
    
    plot_trend(data_to_plot, category, title)
  })
  
  
  
  
  output$racePlot <- renderPlot({
    # Recreate melted_data using the entire race_df
    melted_data <- tidyr::pivot_longer(race_df, 
                                       cols = c(Race_to_Total_Population, Race_to_Total_in_Poverty),
                                       names_to = 'Type', 
                                       values_to = 'Percentage')
    
    # Plotting code
    ggplot(melted_data, aes(x = Label, y = Percentage, fill = Type)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = sprintf("%.2f%%", Percentage*100)),  
                vjust = -0.5, position = position_dodge(width = 0.9),
                size = 5,  
                fontface = "bold") +  
      labs(title = 'Comparison of Race Representation in Total Population vs Total in Poverty in 2022',
           x = 'Race', y = 'Percentage') +
      scale_fill_manual(values = c("darkblue", "darkorange")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 24, face = "bold", hjust = 0.5)
      )
    
  })
  
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("all_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(all_data, file, row.names = FALSE)
    })
  
  
}

shinyApp(ui = ui, server = server)