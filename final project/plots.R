# Load the required libraries
library(shiny)
library(ggplot2)

# Define the user interface
ui <- fluidPage(
  titlePanel("Emissions Over the Years by Industry"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("industry", "Select Industry", choices = unique(new_data$Industry))
    ),
    
    mainPanel(
      plotOutput("industry_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$industry_plot <- renderPlot({
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
    co2_emissions <- as.numeric(result[,"Total_Emissions"])
    
    # Create a plot
    plot(years, co2_emissions, type = "l",
         col = "blue", xlab = "Year",
         ylab = "Emissions", main = paste(industry))
  })
}

# Create the Shiny app
shinyApp(ui = ui, server = server)




