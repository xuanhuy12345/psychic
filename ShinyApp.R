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
#    if (input$numorprop == "Number") {
 #     p_extreme <- input$extreme
#    } else {  # If proportion is selected, calculate the number based on the proportion
#      p_extreme <- round(as.numeric(input$cardAttempts) * input$extreme)
 #   }
    
    ggplot(data, aes(x = Successes/NumTries, fill = Successes/NumTries >= input$extreme)) +
      geom_histogram(binwidth = 0.1, bins = 10, color = "black", show.legend = FALSE) +
      geom_vline(xintercept = input$extreme-0.05, color = "blue", linetype = "dashed", size = 1.2) +
      scale_fill_manual(values = c("gray","lightblue")) + 
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
}

