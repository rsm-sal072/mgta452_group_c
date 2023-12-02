library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)



ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  div(class = "title-style", 
      tags$h1("MGTA 452 Collecting and Analyzing Large Data", 
              style = "text-align: center; font-size: 32px; margin-bottom: 30px;")), 
  h2("US Census Data Analysis Project (Group C)", 
     style = "text-align: center; font-size: 20px; margin-bottom: 20px;"), 
  div(style = "text-align: right;", 
      tags$img(src = "rady_logo.png", height = 100, width = 430)),
  navbarPage("",
             tabPanel("Introduction",
                      tabsetPanel(
                        tabPanel("Project Topic Introduction",
                                 style = "margin-top: 20px;",
                                 fluidRow(
                                   column(6,  
                                          p(style = "font-size: 20px; line-height: 1.6; text-align: justify; font-family: Arial, sans-serif; font-weight: 500;",
                                            "- In this project, we delve into an in-depth analysis of the US Census data, a rich repository of demographic, social, and economic information.",
                                            br(), 
                                            br(),
                                            "- Our goal is to uncover meaningful insights into the socio-economic landscape of the United States.",
                                            br(),
                                            br(),
                                            "- We aim to explore key trends and patterns related to employment, poverty, and demographic changes.",
                                            br(),
                                            br(),
                                            "- By leveraging advanced analytical techniques, our analysis will not only highlight current socio-economic conditions but also project future trends.",
                                            br(),
                                            br(),
                                            "- This exploration is crucial for understanding the dynamics of various demographic groups and informing policy decisions.",
                                            br(),
                                            br(),
                                            "- Ultimately, this project seeks to provide a comprehensive understanding of the intricate tapestry of American society as depicted through the lens of the Census data.")
                                   ),
                                   column(6, 
                                          img(src = "us_census.png", height = "80%", width = "100%"),
                                          p(style = "font-size: 16px; font-family: 'Times New Roman', serif; text-align: center; font-style: italic; margin-top: 10px;",
                                            "Source: ",
                                            a(href = "https://policylab.rutgers.edu/uscensus/", "https://policylab.rutgers.edu/uscensus/", target = "_blank")
                                          )
                                   )
                                 )
                                 
                        ),
                        tabPanel("Group C Introduction",
                                 style = "margin-top: 20px;",
                                 tags$style(type = "text/css", 
                                            ".team-grid {display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; padding: 20px;}",
                                            ".team-member {text-align: center;}",
                                            ".team-member img {height: 200px; width: 200px; border-radius: 50%; margin-bottom: 10px; object-fit: cover;}",
                                            ".team-member .info h3 {margin: 0; font-size: 20px; font-weight: bold;}",
                                            ".team-member .info p {font-size: 16px;}"),
                                 h2("Meet Group C", style = "text-align: center; margin-bottom: 30px;"),
                                 div(class = "team-grid",
                                     div(class = "team-member",
                                         img(src = "Augie.png", alt = "Augie"),
                                         div(class = "info",
                                             h3("Augustine Donovan"),
                                             p("audonovan@ucsd.edu")
                                         )
                                     ),
                                     div(class = "team-member",
                                         img(src = "Alan.png", alt = "Alan"),
                                         div(class = "info",
                                             h3("Alan Shapiro"),
                                             p("a1shapiro@ucsd.edu")
                                         )
                                     ),
                                     div(class = "team-member",
                                         img(src = "Robin.png", alt = "Robin"),
                                         div(class = "info",
                                             h3("Robin Reese"),
                                             p("Robin.Reese@rady.ucsd.edu")
                                         )
                                     ),
                                     div(class = "team-member",
                                         img(src = "Sangho.png", alt = "Sangho"),
                                         div(class = "info",
                                             h3("Sangho Lee"),
                                             p("sal072@ucsd.edu")
                                         )
                                     ),
                                     div(class = "team-member",
                                         img(src = "Shifali.png", alt = "Shifali"),
                                         div(class = "info",
                                             h3("Shefali Sinha"),
                                             p("s3sinha@ucsd.edu")
                                         ))))
                        
                        
                      )),
             tabPanel("Index",
                      div(class = "index-content", 
                          tags$style(type = "text/css", 
                                     ".index-content {text-align: left; margin-top: 20px; font-family: Arial, sans-serif; padding-left: 20px;}",
                                     ".index-main {font-weight: bold; font-size: 18px; margin-top: 15px;}",
                                     ".index-sub {margin-left: 20px; font-size: 16px; margin-top: 5px;}"),
                          h3("Application Index", style = "margin-bottom: 30px; "),
                          
                          div(class = "index-main", "Predictive Analysis"),
                          div(class = "index-sub", "1. Demographic makeup of US cities"),
                          div(class = "index-sub", "2. Aging population and job market"),
                          
                          div(class = "index-main", "Descriptive Analysis"),
                          div(class = "index-sub", "Introduction: Overview of the data sources and objectives of the descriptive analysis."),
                          div(class = "index-sub", "Data: Interactive exploration of the dataset with various filters and search options."),
                          div(class = "index-sub", "Plot (Trend): Visualization of key trends in the data, such as poverty rates and employment patterns."),
                          div(class = "index-sub", "Plot (Race): Graphical representation comparing racial demographics in the context of poverty and population statistics."),
                          
                          div(class = "index-main", "Regression Analysis"),
                          div(class = "index-sub", "1. Pet ownership and health issues"),
                          div(class = "index-sub", "2. Fast-food restaurants and single-person households"),
                          
                          div(class = "index-main", "Conclusion")
                      )
             )
             ,
             tabPanel("Predictive Analysis",
                      h3("** Predictive Analysis **"),
                      h5("1. *What will be the demographic makeup of major US cities in 10 years?* - Projecting future changes in ethnicity, age distribution, and migration patterns."),
                      h5("2. *How will the aging population affect the job market by 2030?* - Predicting shifts in employment sectors due to an increasing number of retirees."),
                      h6("Currently, there are only two bullet points based on Augie's questions. More detailed content and analytical visualizations will be added later")),
             tabPanel("Descriptive Analysis",
                      tabsetPanel(
                        tabPanel("Introduction",
                                 br(),
                                 h3("** Descriptive Analysis of Poverty and Employment Data **"),
                                 h4("This section provides insights into various socioeconomic indicators across different demographic groups.", style = "color: #800000;"),
                                 h4("1. *Employment Status and Poverty Rates* - Explore the relationship between different employment statuses and poverty rates."),
                                 h4("2. *Work Experience and Poverty Trends* - Analyze how various levels of work experience correlate with poverty trends."),
                                 h5(HTML("<i>The analysis leverages data on employment status, work experience, and poverty rates across different demographic categories.</i>"), style = "color: #006400;"),
                                 br(),
                                 br(),
                                 h3("** Trends in Poverty and Employment **"),
                                 h4("Visualize and analyze trends in poverty rates across different employment statuses and work experiences."),
                                 h4("Compare these trends over the years to understand how socio-economic factors have evolved."),
                                 h5(HTML("<i>Interactive visualizations allow for detailed exploration of the data.</i>"), style = "color: #006400;"), 
                                 br(),
                                 br(),
                                 h3("** Race, Population, and Poverty **"),
                                 h4("Explore the representation of different racial groups in the total population and among those in poverty."),
                                 h4("This analysis provides a nuanced view of racial disparities in socioeconomic status."),
                                 h5(HTML("<i>Data visualizations highlight the differences in percentage representation across racial groups.</i>"), style = "color: #006400;")),
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
                        tabPanel("Plot (Trend)",
                                 br(),
                                 pickerInput("selectCategory", "Select Category", 
                                             choices = c("SEX", "AGE", "EDUCATIONAL ATTAINMENT", 
                                                         "EMPLOYMENT STATUS", "WORK EXPERIENCE", 
                                                         "RACE AND HISPANIC OR LATINO ORIGIN")),
                                 plotOutput("trendPlot", height = "800px", width = "100%")),
                        tabPanel("Plot (Race)",
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