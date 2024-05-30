library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readxl)

# Load data
# Ensure that "PsychicApp Sample1 Data.xlsx" is in the working directory or provide the full path.
PsychicApp_Sample1_Data <- read_excel("PsychicApp Sample1 Data.xlsx")

# Define UI for the app
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Psychic Models"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices =  c("sample1"),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "sample1"),
      
      selectInput(inputId = "cardNum",
                  label = "Card Number:",
                  choices = c("2", "5"),
                  multiple = FALSE,
                  selectize = TRUE),
      
      selectInput(inputId = "cardAttempts",
                  label = "Card Attempts:",
                  choices = c("10", "20", "50"),
                  multiple = FALSE,
                  selectize = TRUE,
                  selected = "Challenge"),
      
      selectInput(inputId = "numorprop",
                  label = "Number or Proportion:",
                  choices = c("Number", "Proportion"),
                  multiple = FALSE,
                  selectize = TRUE ),
      
      selectInput(inputId = "openOrClosed",
                  label = "Open Deck or Closed Deck:",
                  choices = c("Open", "Closed"),
                  multiple = FALSE,
                  selectize = TRUE ),

      sliderInput(inputId = "extreme",
                  label = "As extreme as:",
                  min = 0,
                  max = 1,
                  value = 0,
                  step = 0.1),
  
      checkboxGroupInput(inputId = "options", 
                         label = "Options", 
                         choices = c("Binomial Distribution", "Normal Distribution"), 
                         selected = "Binomial Distribution"),
      
      checkboxInput(inputId = "sumstats", 
                    "Summary Statistics", 
                    value = FALSE)
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
      p_extreme <- round(input$cardAttempts * input$extreme)
    }
    
    # Calculate probabilities based on the selected distribution
    if ("Binomial Distribution" %in% input$options) {
      probs <- dbinom(0:input$cardAttempts, size = input$cardAttempts, prob = 1/input$cardNum)
    } else if ("Normal Distribution" %in% input$options) {
      mean_val <- input$cardAttempts * (1/input$cardNum)
      sd_val <- sqrt(input$cardAttempts * (1/input$cardNum) * (1 - 1/input$cardNum))
      probs <- dnorm(0:input$cardAttempts, mean = mean_val, sd = sd_val)
    }
    
    # Plotting
    data_to_plot <- data.frame(Successes = 0:input$cardAttempts, Probability = probs)
    ggplot(data_to_plot, aes(x = Successes, y = Probability, fill = Successes >= p_extreme)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("gray", "light blue")) +
      geom_vline(xintercept = p_extreme - 0.5, color = "blue", linetype = "dashed", size = 1.2) +
      labs(title = "Histogram", subtitle = paste("As extreme as (>=)", p_extreme),
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


