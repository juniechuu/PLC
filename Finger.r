library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Longest Middle Finger Finder"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("measurements", "Enter Measurements (comma-separated):", 
                    placeholder = "e.g., 7.5, 8.0, 6.9, ..."),
      actionButton("calculate", "Find Longest and Average")
    ),
    
    mainPanel(
      verbatimTextOutput("result"),
      plotOutput("measurementPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$calculate, {
    measurements <- input$measurements
    
    if (measurements == "") {
      output$result <- renderText("Please enter some measurements.")
      output$measurementPlot <- renderPlot(NULL)  # Clear plot
    } else {
      # Split input by comma and convert to numeric
      measurements <- as.numeric(unlist(strsplit(measurements, ",")))
      
      # Check for non-numeric entries
      if (any(is.na(measurements))) {
        output$result <- renderText("Please ensure all entries are numeric.")
        output$measurementPlot <- renderPlot(NULL)  # Clear plot
      } else {
        # Calculate the maximum and average measurements
        max_length <- max(measurements, na.rm = TRUE)
        avg_length <- mean(measurements, na.rm = TRUE)
        
        output$result <- renderText({
          paste("The longest middle finger is:", max_length, "units.\n",
                "The average middle finger length is:", round(avg_length, 2), "units.")
        })
        
        # Render the plot with frequency on the x-axis and measurements on the y-axis
        output$measurementPlot <- renderPlot({
          ggplot(data.frame(measurements), aes(x = measurements)) +
            geom_histogram(binwidth = 0.5, fill = "#75AADB", color = "black", alpha = 0.7) +
            geom_vline(aes(xintercept = max_length), color = "red", linetype = "dashed", size = 1) +
            geom_vline(aes(xintercept = avg_length), color = "green", linetype = "dotted", size = 1) +
            labs(title = "Distribution of Middle Finger Measurements",
                 x = "Frequency", 
                 y = "Measurement (units)") +
            theme_minimal()  # Frequency on the x-axis, Measurements on the y-axis
        })
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
