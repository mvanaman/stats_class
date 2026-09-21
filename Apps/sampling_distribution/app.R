# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------
# -------------------------------------------------------------------------
library(tidyverse)
library(collapsibleTree)
library(dplyr)
library(fontawesome)
library(purrr)
library(shiny)
library(shinybrowser)
library(shinycssloaders)
library(shinydashboard)
library(stringr)

# Initialize Vector -------------------------------------------------------
set.seed(100)
vec <- rnorm(50000,16,5)

samp <- function(x,n){
  set.seed(100)
  x <- rnorm(50000,16,5)
  
  x <- sample(x,n)
}

# set theme -----
ggplot2::theme_set(theme_classic())

# -------------------------------------------------------------------------
# UI ----------------------------------------------------------------------
# -------------------------------------------------------------------------
ui <-
  dashboardPage(title = "Sampling Distribution",
    dashboardHeader(),
    dashboardSidebar(collapsed = T,
                     menuItem(text = "Press the button to sample 5",
                              icon = icon("circle-info"))),
    dashboardBody(
      fluidRow(
        box(title = "Population Summary",status = "primary",
          plotOutput("fulldat",height = 200),
          width = 8),
        box(title = "Central Tendency Details", status = "primary",
          paste0("Mean: ",round(mean(vec),2)),
          br(),
          paste0("Median: ",round(median(vec),2)),
          br(),
          paste0("Range: ",round(max(vec)-min(vec),2)),
          br(),
          paste0("Variance: ",round(var(vec),2)),width = 4)),
      fluidRow(
        box(
          plotOutput("pieceDat",height = 200),
          width = 8),
        box(title = "Sampling Options", status = "primary",
          actionButton("anim",label = "Animated Sampling*"),
          br(),br(),
          actionButton("inc",label = "Increment +5"),
          br(),br(),
          actionButton("clear",label = "Clear"),
          width = 4)),
      fluidRow(
        box(
          plotOutput("combinedDat",height = 200),
          width = 8),
        box(
          htmlOutput("hi"),
          width = 4))
        )
      )
# -------------------------------------------------------------------------
# Server ------------------------------------------------------------------
# -------------------------------------------------------------------------
server <- function(input, output) {
  
  # Create Reactive Values
  values <- reactiveValues(mean = numeric(0), n = 0)
  
  # Observe Button Press
  observeEvent(input$inc, {
    # Log new sample each press
    new_sample <- sample(vec, 5)
    # Get Mean of sample
    new_mean <- mean(new_sample)
    values$sample <- new_sample
    # Add mean to value
    values$mean <- c(values$mean, new_mean)
    # Increment counter
    values$n <- values$n + 1
  })
  
  ## Observe 1000 Press
  #observeEvent(input$anim, {
  #  # Log new sample each press
  #  new_sample <- sample(vec, 1000)
  #  # Get Mean of sample
  #  new_mean <- mean(new_sample)
  #  values$sample <- new_sample
  #  # Add mean to value
  #  values$mean <- c(values$mean, new_mean)
  #  # Increment counter
  #  values$n <- values$n + 1000
  #})
  
  # Observer Clear Press
  observeEvent(input$clear, {
    # Remove everything!
   values$sample <- NULL
   values$mean <- NULL
   values$n <- 0
  })
  
  output$fulldat <- renderPlot({
    vec <- data.frame(vec = vec)
    ggplot(vec, aes(x = vec)) +
      geom_histogram() +
      labs(x = "", y = "Frequency") +
      scale_y_continuous(limits = c(0, 4200), expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5), limits = c(0, 30))
  })
  
  output$pieceDat <- renderPlot({
    if(input$inc == 0)
      return()
    values <- data.frame(sample = values$sample)
    ggplot(values, aes(x = sample)) +
      geom_histogram() +
      labs(x = "", y = "Frequency", title = "Sample Data") +
      scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5), limits = c(0, 30))
  })

    output$combinedDat <- renderPlot({
      if(input$inc == 0)
        return()
      values <- data.frame(mean = values$mean)
      ggplot(values, aes(x = mean)) +
        geom_histogram() +
        labs(x = "Sample Means", y = "Frequency", title = "Sampling Distribution of the Mean") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
        scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5), limits = c(0, 30)) +
        geom_vline(xintercept = mean(values$mean))
    })
    
    output$hi <- renderUI({
      HTML(
      paste0("<strong>Reps</strong>: ", values$n,
             "<br>",
             "<strong>Mean</strong>: ", round(mean(values$mean), 2),
             "<br>",
             "<strong>SD</strong>: ",round(sd(values$mean), 2)))
    })

}

shinyApp(ui, server)