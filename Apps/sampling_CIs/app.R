#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(ggthemes)
library(DT)
library(here)

options(scipen = 999)

conf_sim <- function(mu, sd, n_samples, sample_size = 10, confidence = 95, ...) {# add n for number of samples
  samples <- replicate(n = n_samples, expr = rnorm(n = sample_size, mean = mu, sd = sd), ...)
  colnames(samples) <- 1:ncol(samples)
  samples <- as_tibble(samples)
  samples <- samples %>% 
    pivot_longer(cols = everything(), names_to = "Sample", values_to = "Sample_Value") %>% 
    mutate(Sample = as.numeric(Sample)) %>% 
    arrange(Sample)
  # get sample statistics
  samples <- samples %>%
    group_by(Sample) %>%
    summarise(Mean = mean(Sample_Value), SD = sd(Sample_Value), SE = SD / sqrt(sample_size))
  # add CIs
  z <- case_when(confidence == 90 ~ 1.645, confidence == 95 ~ 1.96, confidence == 99 ~ 2.576)
  samples <- samples %>%
    mutate(
      Lower = Mean - z * sd/sqrt(sample_size),
      Upper = Mean + z * sd/sqrt(sample_size),
      Sample = 1:nrow(.),
      "Interval Contains Population Mean?" = ifelse(Lower < mu, ifelse(Upper > mu, "Yes", "No"), "No"),
      "Interval Contains Population Mean?" = factor(`Interval Contains Population Mean?`, levels = c("No", "Yes"))
    ) %>% 
    select(Sample, Mean, Lower, Upper, `Interval Contains Population Mean?`)
  
  colorset <-  c('No' = 'red', 'Yes' = 'black')
  xlim <- c(0, ifelse(n_samples == 1, 2, n_samples))

  plot <- samples %>%
    ggplot(aes(x = Sample, y = Mean)) +
    geom_point(aes(color = `Interval Contains Population Mean?`), alpha = .6) +
    geom_errorbar(
      aes(ymin = Lower, ymax = Upper, color = `Interval Contains Population Mean?`), 
      alpha = .4,
      width = 0
      ) +
    scale_color_manual(values = colorset, name = expression(paste('CI Captures ', mu, "?", sep = ""))) +
    geom_hline(aes(yintercept = mu, linetype = "mu"), color = "purple") +
    labs(title = "95% Confidence Intervals") +
    ylab(label = NULL) +
    xlab(label = "Sample") +
    coord_flip(xlim = xlim, ylim = c(25, 75), clip = "off") +
    theme_tufte() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      plot.margin = unit(c(0, 0, 1, 0.5), "cm"),
      text = element_text(size = 25)
    ) +
    # guides(color = guide_legend(title = "CI Captures \nPopulation Value?")) +
    scale_linetype_manual(
      name = NULL, 
      values = 2,
      labels = expression(mu),
      guide = guide_legend(override.aes = list(color = "purple"))
    ) 
  
  return(list(plot = plot, samples = samples))
}

# App -----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      h3("Population Mean (\\( \\mu \\)) = 50"),
      br(),
      actionButton(
        "draw",
        label = "Draw New Samples"
      ),
      br(),
      br(),
      sliderInput(
        "sd", 
        label = withMathJax("Population Standard Deviation (\\( \\sigma \\)):"),
        min = 5, 
        max = 15,
        value = 10 
      ),
      numericInput(
        "n_samples", 
        label = "Number of Samples to Draw:", 
        value = 100
      ),
      selectInput(
        "confidence", 
        label = "Confidence Level:",
        choices = c(90, 95, 99)
      ),
      numericInput(
        "sample_size", 
        label = "Size of Each Sample:", 
        value = 10
      ),
      DT::DTOutput("data_tab")
    ),
    mainPanel(
      plotOutput("plot", height = "950px")
    )
  )
)

server <- function(input, output, session) {

  CIs <- eventReactive(input$draw, {
    
    conf_sim(
      n_samples = input$n_samples,
      sample_size = input$sample_size, 
      mu = 50,
      sd = input$sd,
      confidence = input$confidence
    )
    
  })
  
  output$plot <- renderPlot({
    CIs()$plot
  }, res = 96)
  
  output$data_tab <- DT::renderDT({
    DT::datatable(
      CIs()$samples,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = 1:20, dom = "plt")
    ) %>%
      DT::formatRound(columns = c("Mean", "Lower", "Upper"), digits = 2)
  })
  
}

shinyApp(ui = ui, server = server)