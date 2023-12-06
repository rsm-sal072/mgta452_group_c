library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)



ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      body { 
        font-family: 'Verdana', sans-serif !important; 
      }
    "))
  ),
  div(class = "title-style", 
      tags$h1("US Census Data Analysis Project (Group C)", 
              style = "text-align: center; font-size: 32px; margin-bottom: 3px;")), 
  h2("- MGTA 452 Collecting and Analyzing Large Data -", 
     style = "text-align: center; font-size: 18px; margin-bottom: 20px;"), 
  div(style = "text-align: right;", 
      tags$img(src = "rady_logo.png", height = 100, width = 430)),
  navbarPage(
    id = "main_nav",
    "",
    tabPanel("🏠",
             tabsetPanel(
               tabPanel("Greetings", value = "greetings", uiOutput("greetings")),
               tabPanel("PPTx",
                        tags$iframe(style = "height:1000px; width:100%", src = "ppt.pdf")
               )
               
               
             )),
    
    tabPanel("Electric Vehicle",
             tabsetPanel(
               tabPanel("Analysis", value = "ev", uiOutput("ev")),
               tabPanel("Data"),
               tabPanel("Plot")
             )),
    
    tabPanel("Poverty",
             tabsetPanel(
               tabPanel("Analysis", value = "poverty", uiOutput("poverty")),
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
    
    
    tabPanel("Migration",
             
             tabsetPanel(
               tabPanel("Analysis", value = "migratin", uiOutput("migration")),
               tabPanel("Data",
                        br(),
                        pickerInput("dataSelector", "Select Data:",
                                    choices = c("Age Data" = "age_data_df",
                                                "Education Data" = "education_data_df",
                                                "Gender Data" = "gender_data_df",
                                                "Race Data" = "race_data_df")),
                        DT::dataTableOutput("dataTable"),
                        downloadButton("downloadSelectedData", "Download Selected Data")
               ),
               tabPanel("Plots",
                        br(),
                        pickerInput("plotSelector", "Select Plot:",
                                    choices = c("Moved within Same County (2012-2022)" = "plot_1",
                                                "Moved from Different County, Same State (2012-2022)" = "plot_2",
                                                "Moved from Different State (2012-2022)" = "plot_3",
                                                "Moved from Abroad (2012-2022)" = "plot_4",
                                                "Education - Moved within Same County (2012-2022)" = "plot_5",
                                                "Education - Moved from Different County, Same State (2012-2022)" = "plot_6",
                                                "Education - Moved from Different State (2012-2022)" = "plot_7",
                                                "Education - Moved from Abroad (2012-2022)" = "plot_8",
                                                "Gender - Moved within Same County (2012-2022)" = "plot_9",
                                                "Gender - Moved from Different County, Same State (2012-2022)" = "plot_10",
                                                "Gender - Moved from Different State (2012-2022)" = "plot_11",
                                                "Gender - Moved from Abroad (2012-2022)" = "plot_12",
                                                "Race - Moved within Same County (2012-2022)" = "plot_13",
                                                "Race - Moved from Different County, Same State (2012-2022)" = "plot_14",
                                                "Race - Moved from Different State (2012-2022)" = "plot_15",
                                                "Race - Moved from Abroad (2012-2022)" = "plot_16")),
                        plotOutput("selectedPlot", height = "800px", width = "100%")
               )
             )
    )
  )
)





server <- function(input, output, session) {
  
  active_tab <- reactiveVal("greetings")
  active_tab_2 <- reactiveVal("poverty")
  active_tab_3 <- reactiveVal("migration")
  active_tab_4 <- reactiveVal("ev")
  
  
  
  ################################################## Greetings ##################################################
  observeEvent(input$main_nav, {
    if(input$main_nav == "greetings") {
      rendered_html <- rmarkdown::render("greetings.Rmd", output_dir = "www", output_file = "greetings.html")
      
      active_tab(input$main_nav)
    }
  })
  
  output$greetings <- renderUI({
    if (active_tab() == "greetings") {
      tags$iframe(src = "greetings.html", style = "width:100%; height:600px;")
    }
  })
  
  
  ################################################## Poverty ##################################################
  observeEvent(input$main_nav, {
    if(input$main_nav == "poverty") {
      rendered_html <- rmarkdown::render("poverty.Rmd", output_dir = "www", output_file = "poverty.html")
      
      active_tab(input$main_nav)
    }
  })
  
  output$poverty <- renderUI({
    if (active_tab_2() == "poverty") {
      tags$iframe(src = "poverty.html", style = "width:100%; height:600px;")
    }
  })
  
  ################################################## Migration ##################################################
  observeEvent(input$main_nav, {
    if(input$main_nav == "migration") {
      rendered_html <- rmarkdown::render("migration.Rmd", output_dir = "www", output_file = "migration.html")
      
      active_tab(input$main_nav)
    }
  })
  
  output$migration <- renderUI({
    if (active_tab_3() == "migration") {
      tags$iframe(src = "migration.html", style = "width:100%; height:600px;")
    }
  })
  
  ################################################## EV ##################################################
  observeEvent(input$main_nav, {
    if(input$main_nav == "ev") {
      rendered_html <- rmarkdown::render("ev.Rmd", output_dir = "www", output_file = "ev.html")
      
      active_tab(input$main_nav)
    }
  })
  
  output$ev <- renderUI({
    if (active_tab_4() == "ev") {
      tags$iframe(src = "ev.html", style = "width:100%; height:600px;")
    }
  })
  
  
  
  #################################################################################################################################################
  
  
  
  
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
  
  
  
  
  output$dataTable <- DT::renderDataTable({
    req(input$dataSelector)
    selectedData <- switch(input$dataSelector,
                           "age_data_df" = age_data_df,
                           "education_data_df" = education_data_df,
                           "gender_data_df" = gender_data_df,
                           "race_data_df" = race_data_df)
    DT::datatable(selectedData,
                  extensions = c("Buttons", "FixedHeader"), 
                  options = list(pageLength = 50,
                                 dom = "Blfrtip",
                                 buttons = c("copy", "csv", "excel"),
                                 scrollX = TRUE,
                                 scrollY = "700px",
                                 fixedHeader = TRUE,
                                 fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE) 
  })
  
  
  
  
  
  output$selectedPlot <- renderPlot({
    req(input$plotSelector)
    selectedPlot <- switch(input$plotSelector,
                           "plot_1" = plot_1,
                           "plot_2" = plot_2,
                           "plot_3" = plot_3,
                           "plot_4" = plot_4,
                           "plot_5" = plot_5,
                           "plot_6" = plot_6,
                           "plot_7" = plot_7,
                           "plot_8" = plot_8,
                           "plot_9" = plot_9,
                           "plot_10" = plot_10,
                           "plot_11" = plot_11,
                           "plot_12" = plot_12,
                           "plot_13" = plot_13,
                           "plot_14" = plot_14,
                           "plot_15" = plot_15,
                           "plot_16" = plot_16)
    print(selectedPlot)
  })
  
  
  
  
  
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("all_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(all_data, file, row.names = FALSE)
    })
  
  output$downloadSelectedData <- downloadHandler(
    filename = function() {
      paste(input$dataSelector, Sys.Date(), ".csv", sep = "-")
    },
    content = function(file) {
      req(input$dataSelector)
      selectedData <- switch(input$dataSelector,
                             "age_data_df" = age_data_df,
                             "education_data_df" = education_data_df,
                             "gender_data_df" = gender_data_df,
                             "race_data_df" = race_data_df)
      write.csv(selectedData, file, row.names = FALSE)
    })
  
  
}

shinyApp(ui = ui, server = server)