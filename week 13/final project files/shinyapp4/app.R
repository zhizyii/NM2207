library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

data <- read.csv("Annual_Greenhouse_Gas_(GHG)_Air_Emissions_Accounts.csv")

# Create a mapping from full industry names to short-form names
industry_mapping <- c(
  "Manufacturing" = "M",
  "Electricity, Gas, Steam and Air Conditioning Supply" = "E",
  "Transportation and Storage" = "T",
  "Agriculture, Forestry and Fishing" = "A",
  "Construction" = "C"
)

# Create a new data frame with only the needed columns of suitable names
new_names <- c("Region", "Industry", "Gas_Type", 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

new_data <- data %>%
  filter(Country %in% c("Africa", "Americas", "Asia", "Europe", "Oceania")) %>%
  filter(Industry %in% names(industry_mapping)) %>%
  select(Country, Industry, Gas_Type, F2010, F2011, F2012, F2013, F2014, F2015, F2016, F2017, F2018, F2019, F2020, F2021) %>%
  setNames(new_names)

# Define the UI for the Shiny app
ui <- fluidPage(
  includeCSS("styles.css"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gas_type", "Select Gas Type", choices = c("Carbon dioxide", "Fluorinated gases", "Methane", "Nitrous oxide")),
      htmlOutput("info_text"),
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
      select(-c(Region, Gas_Type))
  })
  
  info_text <- "Note that the Industry names shown in the plot are of their short forms."
  
  output$info_text <- renderText({
    info_text
  })
  
  output$emissions_plot <- renderPlotly({
    total_emissions <- filtered_data() %>%
      group_by(Industry) %>%
      summarize(Total_Emissions = sum(c_across(starts_with("20")))
      )
    
    total_emissions$Industry_short <- sapply(total_emissions$Industry, function(industry) {
      substring(industry, 1, 1)  # Extract the first letter of the industry name
    })
    
    p <- ggplot(total_emissions, aes(x = Industry_short, y = Total_Emissions, fill = Industry_short, text = Industry_short)) +
      geom_bar(stat = "identity") +
      labs(x = "Industry", y = paste("Total", input$gas_type, "Emissions"), fill = "Industry") +
      ggtitle(paste("Total", input$gas_type, "Emissions by Industry")) +
      theme(axis.text.x = element_text(hjust = 1),
            plot.background = element_rect(fill = "#F4F1E8"),  # Set the background color of the plot
            legend.background = element_rect(fill = "#F4F1E8"))
    
    plotly_chart <- ggplotly(p)
    plotly_chart
  })
}

shinyApp(ui, server)
