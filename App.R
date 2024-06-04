library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readxl)

# Load data
data <- read_excel("PsychicApp Sample1 Data (1).xlsx")


# Define UI for the app
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("Psychic Models"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("groupID", 
                  "Group ID:", 
                  choices = c("sample1"), 
                  selected = "sample1"),
      
      selectInput("cardNum", 
                  "Card Numbers:", 
                  choices = c(2,5), 
                  selected = 2, 
                  multiple = FALSE),
      
      selectInput("cardAttempts", 
                  "Card Attempts:", 
                  choices = c(10,20,50), 
                  selected = 10, 
                  multiple = FALSE),
      
      selectInput("numOrProp", 
                  "Number or Proportion:", 
                  choices = c("Number", "Proportion"), 
                  selected = "Number"),
      
      selectInput(inputId = "openOrClosed",
                  label = "Open Deck or Closed Deck:",
                  choices = c("open", "closed"),
                  multiple = FALSE,
                  selectize = TRUE ),
      
      uiOutput("sliderUI"),
      
      checkboxGroupInput("options", 
                         "Options", 
                         choices = c("Binomial Distribution",  "Normal Distribution", "Hypergeometric Distribution"), 
                         selected = "None"),
      
      checkboxInput("sumstats", 
                    "Summary Statistics", 
                    value = FALSE)
    ),
    
    mainPanel(
      
      plotOutput("Plot"),
      uiOutput("summaryStatsUI")
    )
  )
)

# Define Server for the app
server <- function(input, output, session) {
  # Filter the data set based on the user's selection
  filtered_data <- reactive({
    data %>%
      filter(
        NumTries == input$cardAttempts,
        NumCard == input$cardNum,
        Deck == input$openOrClosed
      )
  })
  
  
  # Generate different slider UI for number or proportion inputs.
  output$sliderUI <- renderUI({
    if (input$numOrProp == "Number") {
      max_value <- as.numeric(input$cardAttempts)
      sliderInput("extreme", 
                  "As extreme as:", 
                  min = 0, 
                  max = max_value, 
                  value = 7, step = 1)
    } else {
      sliderInput("extreme", 
                  "As extreme as:", 
                  min = 0, 
                  max = 1, 
                  value = 0.7, 
                  step = 0.1)
    } # if/else
  }) # output$sliderUI 
  
  
  output$Plot <- renderPlot({
    # Get filtered data
    data_plot <- filtered_data()
    total_counts <- nrow(data_plot)
    
    # Generate histogram for the chosen data
    p <- if (input$numOrProp == "Number") { 
      # Histogram for number data
      ggplot(data_plot, aes(x = Successes, fill = Successes >= input$extreme)) +
        geom_histogram(binwidth = 1, bins = 10, color = "white", alpha = 1, show.legend = FALSE,cex.axis = 2) +
        geom_vline(xintercept = input$extreme - 0.5, color = "darkred",  linetype = "dashed", size = 1) +
        scale_fill_manual(values = c("gray", "orange")) + 
        labs(
          title = "Histogram of Proportion of Successes",
          y = "Frequency",
          x = "Number of Successes"
        ) +
        theme_grey() +
        theme(text = element_text(family="Times", size = 20), plot.title=element_text(face="bold"))
    } else {
      # Histogram for proportion data
      ggplot(data_plot, aes(x = Successes / NumTries, fill = Successes / NumTries >= input$extreme)) +
        geom_histogram(binwidth = 0.1, bins = 10, color = "white", alpha = 1, show.legend = FALSE) +
        geom_vline(xintercept = input$extreme - 0.05, color = "darkred", linetype = "dashed", size = 1) +
        scale_fill_manual(values = c("gray", "orange")) + 
        labs(
          title = "Histogram of Proportion of Successes",
          y = "Frequency",
          x = "Proportion of Successes"
        ) +
        theme_grey() +
        theme(text = element_text(family="Times", size = 20), plot.title=element_text(face="bold"))
    } # if/else
    
    prob <- ifelse(as.numeric(input$cardNum) == 5, 0.2, 0.5)
    mean <- as.numeric(input$cardAttempts) * prob
    sd <- sqrt(as.numeric(input$cardAttempts) * prob * (1 - prob))
    
    # Add Binomial Distribution Overlay
    if ("Binomial Distribution" %in% input$options) {
      if (input$numOrProp == "Number") {
        # Binomial distribution for number data
        binom_data <- data.frame(Successes = 0:as.numeric(input$cardAttempts))
        binom_data$Frequency <- dbinom(binom_data$Successes, size = as.numeric(input$cardAttempts), prob = prob) * total_counts
        p <- p + geom_bar(data = binom_data, aes(y = Frequency), stat = "identity", width = 0.1, fill = "red", alpha = 0.8, position = "identity")
      } else {
        # Binomial distribution for proportion data
        binom_data <- data.frame(Proportion = (0:as.numeric(input$cardAttempts)) / as.numeric(input$cardAttempts)) 
        binom_data$Frequency <- dbinom(0:as.numeric(input$cardAttempts), size = as.numeric(input$cardAttempts), prob = prob) * total_counts
        p <- p + geom_bar(data = binom_data, aes(x = Proportion, y = Frequency), stat = "identity", width = 0.01, fill = "red", alpha = 0.8, position = "identity")
      } # if/else
    } # if/else
    
    # Add Normal Distribution Overlay
    if ("Normal Distribution" %in% input$options) {
      if (input$numOrProp == "Number") {
        #norm_data <- data.frame(x = seq(min(data_plot$Successes), max(data_plot$Successes), length.out = 300))
        #norm_data$y <- dnorm(norm_data$x, mean, sd) *  diff(hist(data_plot$Successes, plot = FALSE)$breaks)[1]
        #p <- p + geom_line(data=norm_data,aes(x=x,y=y),color='red')
        p <- p + stat_function(fun = function(x) total_counts * dnorm(x, mean = mean, sd = sd), col = "yellow", lwd = 1)
      } else {
        p <- p + stat_function(fun = function(x) {
             scaled_x <- x * (as.numeric(input$cardAttempts))  
             total_counts * dnorm(scaled_x, mean = mean, sd = sd)
        }, xlim = c(0, 1), col = "yellow", lwd = 1)
      }
    }
    
    return(p)
  })
  
  output$summaryStatsUI <- renderUI({
    if (input$sumstats) {
      data_stats <- filtered_data()
      stats <- data_stats %>%
        summarise(
          Mean = mean(Successes),
          Median = median(Successes),
          SD = sd(Successes),
          Min = min(Successes),
          Max = max(Successes)
        )
      fluidRow(
        column(12, tableOutput("summaryStatsTable"))
      )
    }
  })
  
  output$summaryStatsTable <- renderTable({
    if (input$sumstats) {
      data_stats <- filtered_data()
      data_stats %>%
        summarise(
          Mean = mean(Successes),
          Median = median(Successes),
          SD = sd(Successes),
          Min = min(Successes),
          Max = max(Successes)
        )
    }
  })
}

shinyApp(ui = ui, server = server)
