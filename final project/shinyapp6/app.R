library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

data <- read.csv("Annual_Greenhouse_Gas_(GHG)_Air_Emissions_Accounts.csv")

# Create a new data frame with only the needed columns of suitable names
new_names <- c("Region", "Industry", "Gas_Type", 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

new_data <- data %>%
  filter(Country %in% c("Africa", "Americas", "Asia", "Europe", "Oceania")) %>%
  filter(Industry %in% c("Manufacturing", "Electricity, Gas, Steam and Air Conditioning Supply", "Transportation and Storage", "Agriculture, Forestry and Fishing", "Construction")) %>%
  select(Country, Industry, Gas_Type, F2010, F2011, F2012, F2013, F2014, F2015, F2016, F2017, F2018, F2019, F2020, F2021) %>%
  setNames(new_names)

# Define the UI for the Shiny app
ui <- fluidPage(
  includeCSS("styles.css"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gas_type", "Select Gas Type", choices = c("Carbon dioxide","Fluorinated gases","Methane","Nitrous oxide")),
    ),
    mainPanel(
      plotlyOutput("emissions_plot")
    )
  )
)

# Define the server for the Shiny app
server <- function(input, output) {
  filtered_data <- reactive({
    new_data %>%
      filter(Gas_Type == input$gas_type) %>%
      select(-c(Industry, Gas_Type))
  })
  
  output$emissions_plot <- renderPlotly({
    total_emissions <- filtered_data() %>%
      group_by(Region) %>%
      summarize(Total_Emissions = sum(c_across(starts_with("20"))))
    
    p <- ggplot(total_emissions, aes(x = Region, y = Total_Emissions, fill = Region)) +
      geom_bar(stat = "identity") +
      labs(x = "Region", y = paste("Total", input$gas_type, "Emissions"), fill = "Region") +
      ggtitle(paste("Total", input$gas_type, "Emissions by Region")) +
      theme(axis.text.x = element_text( hjust = 1),
            plot.background = element_rect(fill = "#F4F1E8"),  # Set the background color of the plot
            legend.background = element_rect(fill = "#F4F1E8"))
    
    plotly_chart <- ggplotly(p)
    plotly_chart
  })
}

shinyApp(ui, server)
