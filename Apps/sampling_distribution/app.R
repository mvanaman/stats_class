# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------
# -------------------------------------------------------------------------

packages <- c(
  "tidyverse",
  "collapsibleTree",
  "fontawesome",
  "purrr",
  "shiny",
  "shinybrowser",
  "shinycssloaders",
  "shinydashboard",
  "stringr"
)

lapply(packages, require, character.only = TRUE)


# -------------------------------------------------------------------------
# Initialize Vector -------------------------------------------------------
# -------------------------------------------------------------------------

set.seed(100)

vec <- rnorm(50000, 16, 5)


# Set theme
ggplot2::theme_set(theme_classic())


# -------------------------------------------------------------------------
# UI ----------------------------------------------------------------------
# -------------------------------------------------------------------------

ui <- dashboardPage(
  
  title = "Sampling Distribution",
  
  dashboardHeader(),
  
  dashboardSidebar(
    collapsed = TRUE,
    
    menuItem(
      text = "Choose a sample size and draw samples",
      icon = icon("circle-info")
    )
  ),
  
  dashboardBody(
    
    # ---------------------------------------------------------------------
    # Population
    # ---------------------------------------------------------------------
    
    fluidRow(
      
      box(
        title = "Population Summary",
        status = "primary",
        
        plotOutput(
          "fulldat",
          height = 200
        ),
        
        width = 8
      ),
      
      box(
        title = "Population Parameters",
        status = "primary",
        
        paste0(
          "Mean: ",
          round(mean(vec), 2)
        ),
        
        br(),
        
        paste0(
          "Standard Deviation: ",
          round(sd(vec), 2)
        ),
        
        width = 4
      )
    ),
    
    
    # ---------------------------------------------------------------------
    # Sample
    # ---------------------------------------------------------------------
    
    fluidRow(
      
      box(
        plotOutput(
          "pieceDat"
          # height = 200
        ),
        width = 8
      ),
      
      box(
        title = "Sampling Options",
        status = "primary",
        
        actionButton(
          "clear",
          label = "Clear"
        ),
        br(),
        br(),
        sliderInput(
          inputId = "sample_size",
          label = "Choose Sample Size:",
          min = 2,
          max = 100,
          value = 10,
          step = 1
        ),
        
        actionButton(
          "draw_sample",
          label = "Draw Sample"
        ),
        
        br(),
        br(),
        
        sliderInput(
          inputId = "n_samples",
          label = "Choose a Number of Samples to Draw:",
          min = 2,
          max = 100,
          value = 5,
          step = 10
        ),
        
        actionButton(
          "draw_many",
          label = "Draw Samples"
        ),
        
        width = 4
      )
    ),
    
    
    # ---------------------------------------------------------------------
    # Sampling Distribution
    # ---------------------------------------------------------------------
    
    fluidRow(
      
      box(
        plotOutput(
          "combinedDat",
          height = 200
        ),
        width = 8
      ),
      
      box(
        htmlOutput("samp_dist_info"),
        width = 4
      )
    )
  )
)


# -------------------------------------------------------------------------
# Server ------------------------------------------------------------------
# -------------------------------------------------------------------------

server <- function(input, output) {
  
  
  # -----------------------------------------------------------------------
  # Reactive Values
  # -----------------------------------------------------------------------
  
  values <- reactiveValues(
    mean = numeric(0),
    sample = numeric(0),
    n = 0
  )
  
  
  # -----------------------------------------------------------------------
  # Draw Sample
  # -----------------------------------------------------------------------
  
  observeEvent(input$draw_sample, {
    
    # Draw sample using selected sample size
    new_sample <- sample(
      vec,
      input$sample_size
    )
    
    
    # Calculate sample mean
    new_mean <- mean(new_sample)
    
    
    # Save current sample
    values$sample <- new_sample
    
    
    # Add sample mean to sampling distribution
    values$mean <- c(
      values$mean,
      new_mean
    )
    
    
    # Increment number of repetitions
    values$n <- values$n + 1
    
  })
  
  observeEvent(input$draw_many, {
    
    # Draw many samples and calculate the mean of each one
    new_means <- replicate(
      input$n_samples,
      mean(
        sample(
          vec,
          input$sample_size
        )
      )
    )
    
    # Blank the single-sample panel
    values$sample <- numeric(0)
    
    # Add all new means to the sampling distribution
    values$mean <- c(
      values$mean,
      new_means
    )
    
    # Increase repetition counter
    values$n <- values$n + input$n_samples
  })
  
  # -----------------------------------------------------------------------
  # Reset when sample size changes
  # -----------------------------------------------------------------------
  
  observeEvent(
    input$sample_size,
    {
      
      values$sample <- numeric(0)
      values$mean <- numeric(0)
      values$n <- 0
      
    },
    ignoreInit = TRUE
  )
  
  
  # -----------------------------------------------------------------------
  # Clear Button
  # -----------------------------------------------------------------------
  
  observeEvent(input$clear, {
    
    values$sample <- numeric(0)
    values$mean <- numeric(0)
    values$n <- 0
    
  })
  
  
  # -----------------------------------------------------------------------
  # Population Plot
  # -----------------------------------------------------------------------
  
  output$fulldat <- renderPlot({
    
    population <- data.frame(
      vec = vec
    )
    
    
    ggplot(
      population,
      aes(x = vec)
    ) +
      geom_histogram() +
      
      labs(
        x = "",
        y = "Frequency"
      ) +
      
      scale_y_continuous(
        limits = c(0, 4200),
        expand = c(0, 0)
      ) +
      
      scale_x_continuous(
        breaks = seq(
          from = 0,
          to = 30,
          by = 5
        ),
        limits = c(0, 30)
      )
  })
  
  
  # -----------------------------------------------------------------------
  # Sample Plot
  # -----------------------------------------------------------------------
  
  output$pieceDat <- renderPlot({
    
    if (length(values$sample) == 0) {
      return()
    }
    
    
    sample_data <- data.frame(
      sample = values$sample
    )
    
    
    ggplot(
      sample_data,
      aes(x = sample)
    ) +
      
      geom_histogram() +
      
      labs(
        x = "Sample Data",
        y = "Frequency",
        title = paste0(
          "Sample Data (n = ",
          input$sample_size,
          ")"
        ),
        subtitle = paste0(
          "Mean = ",
          round(mean(sample_data$sample), 2),
          "\nSD = ",
          round(sd(sample_data$sample), 2)
        )
      ) +
      
      scale_y_continuous(
        expand = c(0, 0)
      ) +
      
      scale_x_continuous(
        breaks = seq(
          from = 0,
          to = 30,
          by = 5
        ),
        limits = c(0, 30)
      )
  })
  
  
  # -----------------------------------------------------------------------
  # Sampling Distribution Plot
  # -----------------------------------------------------------------------
  
  output$combinedDat <- renderPlot({
    
    if (length(values$mean) == 0) {
      return()
    }
    
    
    sampling_data <- data.frame(
      mean = values$mean
    )

    ggplot(
      sampling_data,
      aes(x = mean)
    ) +
      
      geom_histogram(alpha = 0.5) +
            
      labs(
        x = "Sample Means",
        y = "Frequency",
        title = "Sampling Distribution"
      ) +
      
      scale_y_continuous(
        expand = c(0, 0)
      ) +
      
      scale_x_continuous(
        breaks = seq(
          from = 0,
          to = 30,
          by = 5
        ),
        limits = c(0, 30)
      ) +
      
      geom_vline(
        xintercept = mean(values$mean)
      )
  })
  
  
  # -----------------------------------------------------------------------
  # Sampling Distribution Summary
  # -----------------------------------------------------------------------
  
  output$samp_dist_info <- renderUI({
    
    if (length(values$mean) == 0) {
      
      HTML(
        paste0(
          "<strong>Size of Each Sample</strong>: ",
          "0",
          # input$sample_size,
          "<br>",
          "<strong>Number of Samples in Distribution</strong>: 0",
          "<br>",
          "<strong>Mean</strong>: --",
          "<br>",
          "<strong>Standard Error</strong>: --"
        )
      )
      
    } else {
      
      se_value <- if (length(values$mean) > 1) {
        round(sd(values$mean), 2)
      } else {
        "--"
      }
      
      
      HTML(
        paste0(
          "<strong>Sample Size</strong>: ",
          input$sample_size,
          "<br>",
          "<strong>Total Samples</strong>: ",
          values$n,
          "<br>",
          "<strong>Mean</strong>: ",
          round(mean(values$mean), 2),
          "<br>",
          "<strong>Standard Error</strong>: ",
          se_value
        )
      )
    }
  })
}


# -------------------------------------------------------------------------
# Run App -----------------------------------------------------------------
# -------------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)