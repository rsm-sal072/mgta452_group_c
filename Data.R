library(readxl)
library(tidyverse)
library(scales)
library(stringr)
library(RColorBrewer)
library(sf)
library(viridis)

################# Augie ###################

directory <- getwd()

# Main categories
main_categories <- c('AGE', 'SEX', 'RACE AND HISPANIC OR LATINO ORIGIN', 'EDUCATIONAL ATTAINMENT', 'EMPLOYMENT STATUS', 
                     'WORK EXPERIENCE', 'All Individuals below:', 'ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS', 
                     'UNRELATED INDIVIDUALS FOR WHOM POVERTY STATUS IS DETERMINED')

# Initialize list to store DataFrames from each file
dfs <- list()

# Loop through each file in the directory
file_paths <- list.files(directory, pattern = "\\.xlsx$", full.names = TRUE)

for (file_path in file_paths) {
  # Read the Excel file from the second tab
  df <- read_excel(file_path, sheet = 'Data', skip = 2)
  
  # Initialize variable to keep track of the current category
  current_category <- NA
  
  # Function to determine the category for each row
  assign_category <- function(label) {
    if (label %in% main_categories) {
      current_category <<- label
    }
    return(current_category)
  }
  
  # Assign categories
  df$Category <- sapply(df$Label, assign_category)
  
  # Filter rows and drop columns with 'Margin of Error'
  df <- df %>% 
    filter(Label %in% c('Under 18 years', '18 to 64 years', '18 to 34 years', '35 to 64 years', '65 years and over', 
                        'Male', 'Female', 'Less than high school graduate', 'High school graduate (includes equivalency)',
                        "Some college, associate's degree", "Bachelor's degree or higher", 'Employed', 'Unemployed', "Asian alone", 
                        'Black or African American alone', 'White alone', "Native Hawaiian and Other Pacific Islander alone",
                        "Hispanic or Latino origin (of any race)", 'White', 'American Indian and Alaska Native alone',
                        'Black or African American', 'American Indian and Alaska Native', 'Asian', 'Native Hawaiian and Other Pacific Islander',
                        'Hispanic or Latino origin (of any race)', "Worked full-time, year-round in the past 12 months",
                        "Worked part-time or part-year in the past 12 months", "Did not work")) %>%
    select(-contains('Margin of Error')) %>%
    rename(Total_Population = Estimate...2, Below_Poverty = Estimate...4, Percent_Below_Poverty = Estimate...6)
  
  # Extract the year from the file name and add as a new column
  df$Report <- str_extract(basename(file_path), "^[^\\.]+")
  
  # Convert columns to integers after removing commas
  df$Total_Population <- as.integer(gsub(",", "", df$Total_Population))
  df$Below_Poverty <- as.integer(gsub(",", "", df$Below_Poverty))
  
  # Calculate Poverty Rate
  df$Poverty_Rate <- df$Below_Poverty / df$Total_Population
  
  # Add the DataFrame to the list
  dfs[[length(dfs) + 1]] <- df
}

# Concatenate all DataFrames in the list
all_data <- bind_rows(dfs)

# Define the mapping
label_mapping <- c('White' = 'White', 'White alone' = 'White', 'Black or African American' = 'Black or African American', 
                   'Black or African American alone' = 'Black or African American', 'American Indian and Alaska Native' = 'American Indian and Alaska Native',
                   'American Indian and Alaska Native alone' = 'American Indian and Alaska Native', 'Asian' = 'Asian', 
                   'Asian alone' = 'Asian', 'Native Hawaiian and Other Pacific Islander' = 'Native Hawaiian and Other Pacific Islander',
                   'Native Hawaiian and Other Pacific Islander alone' = 'Native Hawaiian and Other Pacific Islander', 
                   'Some other race' = 'Other Race', 'Some other race alone' = 'Other Race', 
                   'Hispanic or Latino origin (of any race)' = 'Hispanic or Latino')

# Update the 'Label' column
all_data$Label <- ifelse(all_data$Label %in% names(label_mapping), label_mapping[all_data$Label], all_data$Label)

# Convert 'Report' to datetime and set to the end of the year
all_data$Year <- ymd(paste0(str_extract(all_data$Report, "\\d{4}"), "-12-31"))

# Fill potentially missing data
all_data$Total_Population[is.na(all_data$Total_Population)] <- 0
all_data$Below_Poverty[is.na(all_data$Below_Poverty)] <- 0

# Additional processing if needed
all_data$Poverty_Rate <- all_data$Below_Poverty / all_data$Total_Population

# functions to display ,
print_number_with_commas <- function(number) {
  formatted_number <- format(number, big.mark = ",", scientific = FALSE)
  cat(formatted_number, "\n")
}

all_data %>% 
  mutate(Poverty_Rate = round(Poverty_Rate, 2)) %>% 
  relocate(Report, Year, Category, Label, Poverty_Rate, Percent_Below_Poverty, Below_Poverty, Total_Population) -> all_data


# Subsetting data for high-level employment status
all_data_employment_highlevel <- all_data %>%
  filter(Category == 'EMPLOYMENT STATUS' & Label %in% c('Employed', 'Unemployed'))

# Subsetting data for low-level work experience
all_data_employment_lowlevel <- all_data %>%
  filter(Category == 'WORK EXPERIENCE' & Label %in% c('Worked full-time, year-round in the past 12 months', 
                                                      'Worked part-time or part-year in the past 12 months', 
                                                      'Did not work'))





plot_trend <- function(data, category, title) {
  filtered_data <- data %>% filter(Category == category)
  
  ggplot(filtered_data, aes(x = Year, y = Poverty_Rate, color = Label)) +
    geom_line() +
    geom_text(aes(label = sprintf("%.0f%%", Poverty_Rate*100)), 
              vjust = -0.5, hjust = 1.5, 
              size = 5, fontface = "bold") +
    labs(title = title, x = 'Year', y = '% Below Poverty Line') +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    scale_color_brewer(palette = "Set1") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
      axis.text.y = element_text(size = 15, face = "bold"),
      axis.title.x = element_text(size = 17, face = "bold"),
      axis.title.y = element_text(size = 17, face = "bold"),
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 15, face = "bold"),
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5)
    )
  
}

# Example usage
plot_trend(all_data, 'SEX', 'US Poverty Rate by Gender')
plot_trend(all_data, 'AGE', 'US Poverty Rate by Age')
plot_trend(all_data, 'EDUCATIONAL ATTAINMENT', 'US Poverty Rate by Education Attainment')
plot_trend(all_data_employment_highlevel, 'EMPLOYMENT STATUS', 'US Poverty Rate by Employment Status')
plot_trend(all_data_employment_lowlevel, 'WORK EXPERIENCE', 'US Poverty Rate by Work Experience')
plot_trend(all_data, 'RACE AND HISPANIC OR LATINO ORIGIN', 'US Poverty Rate by Race')



# Subset data for race and Hispanic or Latino origin for the year 2022
race_df <- all_data %>% 
  filter(Category == 'RACE AND HISPANIC OR LATINO ORIGIN', lubridate::year(Year) == 2022)

# Calculate total population and total in poverty
total_population <- sum(race_df$Total_Population, na.rm = TRUE)
total_in_poverty <- sum(race_df$Below_Poverty, na.rm = TRUE)

# Calculate percentages
race_df$Race_to_Total_Population <- race_df$Total_Population / total_population
race_df$Race_to_Total_in_Poverty <- race_df$Below_Poverty / total_in_poverty

# Reshape the DataFrame
melted_data <- tidyr::pivot_longer(race_df, 
                                   cols = c(Race_to_Total_Population, Race_to_Total_in_Poverty),
                                   names_to = 'Type', 
                                   values_to = 'Percentage')

ggplot(melted_data, aes(x = Label, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = 'Comparison of Race Representation in Total Population vs Total in Poverty',
       x = 'Race', y = 'Percentage') +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






################### Shefali ####################

# Data 1
age_data_df <- read_csv('Age_Data.csv')

age_data_df <- as.data.frame(age_data_df)
age_data_df <- filter(age_data_df, !Age %in% c('1 to 4 years', '5 to 17 years', '75 years and over'))

age_data_df %>% 
  relocate(Year) %>% 
  relocate(-Total) -> age_data_df


# Data 2
education_data_df <- read_csv('Education_Data.csv')

education_data_df <- education_data_df %>%
  mutate(`Education Attainment` = str_trim(`Education Attainment`, side = "both")) %>% 
  relocate(Year) %>% 
  relocate(-Total)


# Data 3
gender_data_df <- read_csv('Gender_Data.csv')

gender_data_df <- gender_data_df %>%
  mutate(Sex = str_trim(Sex, side = "both")) %>% 
  relocate(Year) %>% 
  relocate(-Total)


# Data 4
race_data_df <- read_csv('Race_Data.csv')

race_data_df <- race_data_df %>%
  mutate(Race = str_trim(Race, side = "both")) %>%
  filter(!Race %in% c('White alone, not Hispanic or Latino', 'Some other race')) %>% 
  relocate(Year) %>% 
  relocate(-Total)






title_size <- 22
axis_label_size <- 20
axis_text_size <- 18
legend_title_size <- 18
legend_text_size <- 16
point_size <- 3  # Adjust the point size here

# Define a common theme style
common_theme <- theme_classic() +
  theme(
    legend.title = element_text(face = "bold", size = legend_title_size),
    legend.text = element_text(face = "bold", size = legend_text_size),
    axis.title.x = element_text(face = "bold", size = axis_label_size),
    axis.title.y = element_text(face = "bold", size = axis_label_size),
    axis.text.x = element_text(face = "bold", size = axis_text_size),
    axis.text.y = element_text(face = "bold", size = axis_text_size),
    plot.title = element_text(face = "bold", size = title_size),
    legend.position = "right"
  )

# Apply the common style to all plots

# Plot 1
plot_1 <- ggplot(age_data_df, aes(x = factor(Year), y = `Moved within same county`, group = Age, color = Age)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved within Same County (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Age Group")

# Plot 2
plot_2 <- ggplot(age_data_df, aes(x = factor(Year), y = `Moved from different county. same state`, group = Age, color = Age)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Different County, Same State (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Age Group")

# Plot 3
plot_3 <- ggplot(age_data_df, aes(x = factor(Year), y = `Moved from different state`, group = Age, color = Age)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Different State (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Age Group")

# Plot 4
plot_4 <- ggplot(age_data_df, aes(x = factor(Year), y = `Moved from abroad`, group = Age, color = Age)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Abroad (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Age Group")

# Plot 5
plot_5 <- ggplot(education_data_df, aes(x = factor(Year), y = `Moved within same county`, group = `Education Attainment`, color = `Education Attainment`)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved within Same County (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Education Level")

# Plot 6
plot_6 <- ggplot(education_data_df, aes(x = factor(Year), y = `Moved from different county. same state`, group = `Education Attainment`, color = `Education Attainment`)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Different County, Same State (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Education Attainment")

# Plot 7
plot_7 <- ggplot(education_data_df, aes(x = factor(Year), y = `Moved from different state`, group = `Education Attainment`, color = `Education Attainment`)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Different State (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Education Attainment")

# Plot 8
plot_8 <- ggplot(education_data_df, aes(x = factor(Year), y = `Moved from abroad`, group = `Education Attainment`, color = `Education Attainment`)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Abroad (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Education Attainment")

# Plot 9
plot_9 <- ggplot(gender_data_df, aes(x = factor(Year), y = `Moved within same county`, group = Sex, color = Sex)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved within Same County (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Sex")

# Plot 10
plot_10 <- ggplot(gender_data_df, aes(x = factor(Year), y = `Moved from different county. same state`, group = Sex, color = Sex)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Different County, Same State (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Sex")

# Plot 11
plot_11 <- ggplot(gender_data_df, aes(x = factor(Year), y = `Moved from different state`, group = Sex, color = Sex)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Different State (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Sex")

# Plot 12
plot_12 <- ggplot(gender_data_df, aes(x = factor(Year), y = `Moved from abroad`, group = Sex, color = Sex)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Abroad (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Sex")

# Plot 13
plot_13 <- ggplot(race_data_df, aes(x = factor(Year), y = `Moved within same county`, group = Race, color = Race)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved within Same County (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Race")

# Plot 14
plot_14 <- ggplot(race_data_df, aes(x = factor(Year), y = `Moved from different county. same state`, group = Race, color = Race)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Different County, Same State (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Race")

# Plot 15
plot_15 <- ggplot(race_data_df, aes(x = factor(Year), y = `Moved from different state`, group = Race, color = Race)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Different State (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Race")

# Plot 16
plot_16 <- ggplot(race_data_df, aes(x = factor(Year), y = `Moved from abroad`, group = Race, color = Race)) +
  geom_line(size = 1.5) +
  geom_point(size = point_size) +
  scale_color_brewer(palette = "Dark2") +  
  ggtitle('Moved from Abroad (2012-2022)') +
  xlab('Year') +
  ylab('% Population') +
  common_theme +
  labs(color = "Race")







################# Robin ###################

# Read Data
shapefile <- readRDS("shapefile.rds")
df_robin <- read_csv("ev-zipcode-demographics.csv")



# robin_plot1
vehicle_types <- c('Diesel', 'Electric', 'Flex_Fuel', 'Gasoline', 'Gasoline_Hybrid', 
                   'Hydrogen', 'Natural_Gas', 'PHEV', 'Propane')

total_vehicles_by_type <- df_robin %>% 
  select(all_of(vehicle_types)) %>% 
  summarise_all(sum)

total_vehicles <- sum(total_vehicles_by_type)

vehicle_type_percentages <- total_vehicles_by_type / total_vehicles * 100

vehicle_type_percentages_long <- gather(vehicle_type_percentages, key = "Vehicle_Type", value = "Percentage") 



robin_plot1 <- ggplot(vehicle_type_percentages_long, aes(x = reorder(Vehicle_Type, -Percentage), y = Percentage, fill = Vehicle_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.3, fontface = "bold", color = "black", position = position_dodge(width = 0.9),
            size = 5,  
            fontface = "bold") +
  scale_fill_brewer(palette = "Dark2") +  
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 15, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 15),
        axis.title.x = element_text(face = "bold", size = 17),
        axis.title.y = element_text(face = "bold", size = 17),
        plot.title = element_text(face = "bold", size = 24, hjust = 0.5)) +
  labs(title = 'Percentage of Total Vehicle Types Relative to Total Number of Vehicles in California',
       x = 'Vehicle Type', y = 'Percentage (%)') 






