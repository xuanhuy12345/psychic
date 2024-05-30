library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readxl)

# Load data

data <- read.csv("/Users/admin/Downloads/PsychicApp Sample1 Data - Sheet1 (1).csv")

# Define UI for the app
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Psychic Models"),
  sidebarLayout(
    sidebarPanel(
      selectInput("groupID", "Group ID:", choices = c("sample1"), selected = "sample1"),
      selectInput("cardNum", "Card Numbers:", choices = c(2,5), selected = 2, multiple = FALSE),
      selectInput("cardAttempts", "Card Attempts:", choices = c(10,20,50), selected = 10, multiple = FALSE),
      selectInput("numorprop", "Number or Proportion:", choices = c("Number", "Proportion"), selected = "Number"),
      selectInput(inputId = "openOrClosed",
                  label = "Open Deck or Closed Deck:",
                  choices = c("open", "closed"),
                  multiple = FALSE,
                  selectize = TRUE ),
      uiOutput("sliderUI"),
      checkboxGroupInput("options", "Options", choices = c("Binomial Distribution", "Normal Distribution"), selected = "Binomial Distribution"),
      checkboxInput("sumstats", "Summary Statistics", value = FALSE)
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
)
server <- function(input, output, session) {
  filtered_data <- reactive({
    data %>%
      filter(
        NumTries == input$cardAttempts,
        NumCard == input$cardNum,
        Deck == input$openOrClosed
      )
  })
  
  output$sliderUI <- renderUI({
    if (input$numorprop == "Number") {
      max_value <- as.numeric(input$cardAttempts)
      sliderInput("extreme", "As extreme as:", min = 0, max = max_value, value = 2, step = 1)
    } else {
      sliderInput("extreme", "As extreme as:", min = 0, max = 1, value = 0.2, step = 0.1)
    }
  })
  
  output$Plot <- renderPlot({
    data_plot <- filtered_data()
    if (input$numorprop == "Number") {
      ggplot(data_plot, aes(x = Successes, fill = Successes >= input$extreme)) +
        geom_histogram(binwidth = 1, bins = 10, color = "black", show.legend = FALSE) +
        geom_vline(xintercept = input$extreme - 0.5, color = "blue", size = 1.5) +
        scale_fill_manual(values = c("gray", "lightblue")) + 
        theme_minimal()
    } else {
      ggplot(data_plot, aes(x = Successes / NumTries, fill = Successes / NumTries >= input$extreme)) +
        geom_histogram(binwidth = 0.1, bins = 15, color = "black", show.legend = FALSE) +
        geom_vline(xintercept = input$extreme - 0.05, color = "blue", size = 1.5) +
        scale_fill_manual(values = c("gray", "lightblue")) + 
        theme_minimal()
    }
  })
}

shinyApp(ui = ui, server = server)
