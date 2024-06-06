library(shiny)
library(ggplot2)
library(tidyverse)


# Load data
data <- PsychicSample1


data <- data %>% 
  mutate(successratio = Successes / NumTries) 

ui <- fluidPage(
  titlePanel("Psychic Models"),
  sidebarLayout(
    sidebarPanel(
      selectInput("groupID", 
                  "Group ID:", 
                  choices = c("sample1"), 
                  selected = "sample1"),
      
      selectInput("cardNum", 
                  "Number of Cards:", 
                  choices = c(2,5), 
                  selected = 2, 
                  multiple = FALSE),
      
      selectInput("cardAttempts", 
                  "Attempts:", 
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
      
      radioButtons("options",
                   "Options",
                   choices = c("Binomial Distribution", "Normal Distribution", "None"),
                   selected = "None"),
      
      checkboxInput("sumstats", 
                    "Summary Statistics", 
                    value = FALSE)
    ),
    
    mainPanel(
      
      plotOutput("Plot"),
      uiOutput("summaryStatsUI"),
      uiOutput("binomDistUI")
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
        geom_histogram(binwidth = 1, color = "blue4", alpha = .5, show.legend = FALSE) +
        geom_vline(xintercept = input$extreme - 0.5, color = "darkblue",  linetype = "dashed", size = 1) +
        scale_fill_manual(values = c("steelblue2", "cyan")) + 
        labs(
          title = "Histogram of Number of Successes",
          y = "Frequency",
          x = "Number of Successes"
        ) +
        theme_bw() +
        theme(text = element_text(family="Times", size = 20), plot.title=element_text(face="bold")) +
        scale_x_continuous(breaks = seq(0, 10, by = 1))
    } else {
      # Histogram for proportion data
      ggplot(data_plot, aes(x = Successes / NumTries, fill = Successes / NumTries >= input$extreme)) +
        geom_histogram(binwidth = 0.1, color = "blue4", alpha = 1, show.legend = FALSE) +
        geom_vline(xintercept = input$extreme - 0.05, color = "darkblue", linetype = "dashed", size = 1) +
        scale_fill_manual(values = c("steelblue2", "cyan")) + 
        labs(
          title = "Histogram of Proportion of Successes",
          y = "Frequency",
          x = "Proportion of Successes"
        ) +
        theme_bw() +
        theme(text = element_text(family="Times", size = 20), plot.title=element_text(face="bold")) +
        scale_x_continuous(breaks = seq(0, 10, by = 1))
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
        p <- p + geom_histogram(data = binom_data, aes(y = Frequency, fill = binom_data$Successes >= input$extreme), 
                                binwidth = 1, stat = "identity", color = "gold", fill = "yellow", alpha = 0.5, position = "identity",show.legend = FALSE)
      } else {
        # Binomial distribution for proportion data
        binom_data <- data.frame(Proportion = (0:as.numeric(input$cardAttempts)) / as.numeric(input$cardAttempts)) 
        binom_data$Frequency <- dbinom(0:as.numeric(input$cardAttempts), size = as.numeric(input$cardAttempts), prob = prob) * total_counts
        p <- p + geom_histogram(data = binom_data, aes(x = Proportion, y = Frequency, fill = binom_data$Proportion >= input$extreme), 
                                binwidth = 0.1, stat = "identity", color = "gold", fill = "yellow",alpha = 0.5, position = "identity", show.legend = FALSE)
        
      } # if/else
    } # if/else
    
    # Add Normal Distribution Overlay
    if ("Normal Distribution" %in% input$options) {
      if (input$numOrProp == "Number") {
        p <- p + stat_function(fun = function(x) total_counts * dnorm(x, mean = mean, sd = sd), 
                               geom = "area", color = "gold", fill = "yellow", alpha = 0.5, lwd = 1,
                               xlim = c(mean-3*sd, mean+3*sd))
        
      } else {
        p <- p + stat_function(fun = function(x) {
          scaled_x <- x * (as.numeric(input$cardAttempts))  
          total_counts * dnorm(scaled_x, mean = mean, sd = sd)
        }, 
        geom = "area", fill = "yellow", color = "gold", alpha = 0.2,
        xlim = c(0, input$extreme - 0.05))
        p <- p + stat_function(fun = function(x) {
          scaled_x <- x * (as.numeric(input$cardAttempts))  
          total_counts * dnorm(scaled_x, mean = mean, sd = sd)
        }, 
        geom = "area", fill = "orange", color = "gold", alpha = 0.2, lwd = 1, 
        xlim = c(input$extreme - 0.05, 1))
      }
    }
    
    return(p)
  })
  
  
  
  
  output$summaryStatsUI <- renderUI({
    if (input$sumstats) {
      fluidRow(
        column(12, tableOutput("summaryStatsTable"))
      )
    }
  })
  
  output$summaryStatsTable <- renderTable({
    if (input$sumstats) {
      data_stats <- filtered_data()
      cardAttempts <- as.numeric(input$cardAttempts)
      extreme <- as.numeric(input$extreme)
      cardNum <- as.numeric(input$cardNum)
      if (input$options == "Binomial Distribution") {
        data.frame(
          Statistics = c("Sample Size", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max"),
          Sample = c(nrow(data_stats), mean(data_stats$Successes), sd(data_stats$Successes), min(data_stats$Successes), quantile(data_stats$Successes, 0.25), median(data_stats$Successes), quantile(data_stats$Successes, 0.75), max(data_stats$Successes)),
          Theoretical = c(nrow(data_stats), cardAttempts * 1 / cardNum, sqrt(cardAttempts * (1 / cardNum) * (1 - 1 / cardNum)), 0, qbinom(0.25, cardAttempts, 1 / cardNum), qbinom(0.5, cardAttempts, 1 / cardNum), qbinom(0.75, cardAttempts, 1 / cardNum), cardAttempts)
        )
      } else if (input$options == "Normal Distribution") {
        data.frame(
          Statistics = c("Sample Size", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max"),
          Sample = c(nrow(data_stats), mean(data_stats$Successes), sd(data_stats$Successes), min(data_stats$Successes), quantile(data_stats$Successes, 0.25), median(data_stats$Successes), quantile(data_stats$Successes, 0.75), max(data_stats$Successes)),
          Theoretical = c(nrow(data_stats), cardAttempts * (1 / cardNum), sqrt(cardAttempts * (1 / cardNum) * (1 - 1 / cardNum)), -Inf, qnorm(0.25, cardAttempts * (1 / cardNum), sqrt(cardAttempts * (1 / cardNum) * (1 - 1 / cardNum))), qnorm(0.5, cardAttempts * (1 / cardNum), sqrt(cardAttempts * (1 / cardNum) * (1 - 1 / cardNum))), qnorm(0.75, cardAttempts * (1 / cardNum), sqrt(cardAttempts * (1 / cardNum) * (1 - 1 / cardNum))), Inf)
        )
      } else {
        data.frame(
          Statistics = c("Sample Size", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max"),
          Sample = c(nrow(data_stats), mean(data_stats$Successes), sd(data_stats$Successes), min(data_stats$Successes), quantile(data_stats$Successes, 0.25), median(data_stats$Successes), quantile(data_stats$Successes, 0.75), max(data_stats$Successes))
        )
      }
    }
  })
  
  output$binomDistUI <- renderUI({
    fluidRow(
      column(12, tableOutput("binomDistTable"))
    )
  })
  
  output$binomDistTable <- renderTable({
    # Ensure all inputs are numeric
    cardAttempts <- as.numeric(input$cardAttempts)
    extreme <- as.numeric(input$extreme)
    cardNum <- as.numeric(input$cardNum)
    data_stats <- filtered_data()
    sample <- if (input$numOrProp == "Number") {
      sum(data_stats$Successes>=extreme) / nrow(data_stats)
    } else {
      sum(data_stats$successratio>=extreme) / nrow(data_stats)
    }
    binom <- if (input$numOrProp == "Number") {
      pbinom(extreme, size = cardAttempts, prob = 1 / cardNum, lower.tail = FALSE)
    } else {
      pbinom(extreme * cardAttempts, size = cardAttempts, prob = 1 / cardNum, lower.tail = FALSE)
    }
    norm <- if (input$numOrProp == "Number") {
      pnorm(extreme, mean = cardAttempts / cardNum, sd = sqrt(cardAttempts * (cardNum-1)) / cardNum, lower.tail = FALSE)
    } else {
      pnorm(extreme * cardAttempts, mean = cardAttempts / cardNum, sd = sqrt(cardAttempts * (cardNum-1)) / cardNum, lower.tail = FALSE)
    }
    data.frame(
      Type = c("Simple Random Probability", "Binomial Distribution", "Normal Distribution"),
      Probability = c(sample , binom, norm) 
    )
  })
}

shinyApp(ui = ui, server = server)
