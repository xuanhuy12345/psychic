library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readxl)

# Load data

PsychicApp_Sample1_Data <- read.csv("/Users/admin/Downloads/PsychicApp Sample1 Data - Sheet1.csv")

# Define UI for the app
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Psychic Models"),
  sidebarLayout(
    sidebarPanel(
      selectInput("groupID", "Group ID:", choices = c("sample1"), selected = "sample1"),
      selectInput("cardNum", "Card Numbers:", choices = c(2,5), selected = 2, multiple = FALSE),
      selectInput("cardAttempts", "Card Attempts:", choices = c(10,20,50), selected = 2, multiple = FALSE),
      selectInput("numorprop", "Number or Proportion:", choices = c("Number", "Proportion"), selected = "Number"),
      selectInput(inputId = "openOrClosed",
                  label = "Open Deck or Closed Deck:",
                  choices = c("Open", "Closed"),
                  multiple = FALSE,
                  selectize = TRUE ),
      sliderInput("extreme", "As extreme as:", min = 0, max = 1, value = 0.2, step = 0.1),
      checkboxGroupInput("options", "Options", choices = c("Binomial Distribution", "Normal Distribution"), selected = "Binomial Distribution"),
      checkboxInput("sumstats", "Summary Statistics", value = FALSE)
    ),
    mainPanel(
      plotOutput("Plot"),
      verbatimTextOutput("statsOut")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  output$Plot <- renderPlot({
    if (input$numorprop == "Number") {
      p_extreme <- input$extreme
    } else {  # If proportion is selected, calculate the number based on the proportion
      p_extreme <- round(as.numeric(input$cardAttempts) * input$extreme)
    }
    
    # Calculate probabilities based on the selected distribution
    if ("Binomial Distribution" %in% input$options) {
      probs <- dbinom(0:as.numeric(input$cardAttempts), size = as.numeric(input$cardAttempts), prob = 1/as.numeric(input$cardNum))
    } else if ("Normal Distribution" %in% input$options) {
      mean_val <- as.numeric(input$cardAttempts) * (1/as.numeric(input$cardNum))
      sd_val <- sqrt(as.numeric(input$cardAttempts) * (1/as.numeric(input$cardNum)) * (1 - 1/as.numeric(input$cardNum)))
      probs <- dnorm(0:as.numeric(input$cardAttempts), mean = mean_val, sd = sd_val)
    }
    
    # Plotting
    data_to_plot <- data.frame(Successes = 0:as.numeric(input$cardAttempts), Probability = probs)
    ggplot(data_to_plot, aes(x = Successes, y = Probability, fill = Successes >= p_extreme)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("gray", "blue")) +
      geom_vline(xintercept = p_extreme - 0.5, color = "red", linetype = "dashed", size = 1.2) +
      labs(title = "Histogram of ESP Game Successes", subtitle = paste("As extreme as (>=)", p_extreme),
           x = "Number of Successes", y = "Probability Density") +
      theme_minimal()
  })
  
  # Optional: Display summary statistics
  output$statsOut <- renderPrint({
    if (input$sumstats) {
      mean_val <- mean(data_to_plot$Successes * data_to_plot$Probability)
      sd_val <- sqrt(var(data_to_plot$Successes * data_to_plot$Probability))
      cat("Mean:", mean_val, "\n", "Standard Deviation:", sd_val, "\n")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

