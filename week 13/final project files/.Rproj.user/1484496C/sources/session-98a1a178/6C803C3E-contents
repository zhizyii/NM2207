library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(shiny)
library(plotly)

shinyApp(
  ui <- fluidPage(
    includeCSS("styles.css"),
    sidebarLayout(
      sidebarPanel(
        selectInput("industry", "Select Industry", choices = c("Manufacturing","Electricity, Gas, Steam and Air Conditioning Supply","Transportation and Storage","Agriculture, Forestry and Fishing","Construction"))
      ),
      
      mainPanel(
        plotlyOutput("industry_plot")
      )
    )
  ),
  
  server <- function(input, output) {
    # Read the data and preprocess it
    data <- read.csv("Annual_Greenhouse_Gas_(GHG)_Air_Emissions_Accounts.csv")
    
    # Create a new data frame with only the needed columns of suitable names
    new_names <- c("Region", "Industry", "Gas_Type", 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
    
    new_data <- data %>%
      filter(Country %in% c("Africa", "Americas", "Asia", "Europe", "Oceania")) %>%
      filter(Industry %in% c("Manufacturing", "Electricity, Gas, Steam and Air Conditioning Supply", "Transportation and Storage", "Agriculture, Forestry and Fishing", "Construction")) %>%
      select(Country, Industry, Gas_Type, F2010, F2011, F2012, F2013, F2014, F2015, F2016, F2017, F2018, F2019, F2020, F2021) %>%
      setNames(new_names)
    
    output$industry_plot <- renderPlotly({
      industry <- input$industry
      years <- 2010:2021
      
      # Calculate the sums for each year
      results <- sapply(years, function(year) {
        filtered_data <- new_data %>%
          filter(Industry == industry) %>%
          summarise(Year = as.integer(year), Total_Emissions = sum(get(as.character(year)))
          )
      })
      
      result <- t(results)
      years <- as.numeric(result[, "Year"])
      emissions <- as.numeric(result[,"Total_Emissions"])
      
      # Create a ggplot2 plot
      p <- ggplot(data = data.frame(Year = years, Total_Emissions = emissions), aes(x = Year, y = Total_Emissions)) +
        geom_line(color = "blue") +
        labs(x = "Year", y = "Emissions", title = paste("Emissions Over the Years for", industry)) +
        theme(plot.background = element_rect(fill = "#F4F1E8"))
      
      # Convert ggplot2 plot to Plotly
      plotly_chart <- ggplotly(p)
      
      # Return the Plotly chart
      plotly_chart
    })
  },
)
