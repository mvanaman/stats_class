# to deploy, paste into console:
# rsconnect::deployApp(
#   appDir  = here::here("Apps", "sample_z_teaching"),
#   appName = "sample_z_teaching",
#   appMode = "shiny"
# )

# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------
# -------------------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinydashboard)
library(patchwork)
library(ggbeeswarm)
library(ggrepel)

# -------------------------------------------------------------------------
# Fixed Parameters
# -------------------------------------------------------------------------

mu <- 50
sd <- 10

# -------------------------------------------------------------------------
# UI ----------------------------------------------------------------------
# -------------------------------------------------------------------------

ui <- dashboardPage(
  
  title = "Sampling Distribution (For Teaching)",
  
  dashboardHeader(),
  
  dashboardSidebar(
    disable = TRUE
  ),
  
  dashboardBody(
    
    fluidRow(
      
      box(
        title = "Sampling Distribution",
        footer = "This is a sampling distribution of the means of all possible samples from a population with a mean of 50 and a standard deviation of 10. Draw a sample — did its confidence interval capture the population mean?",
        status = "primary",
        
        plotOutput(
          "pieceDat"
          # height = "80%"
        ),
        
        width = 8
      ),
      
      box(
        sliderInput(
          "confidence",
          "Set the Confidence Level:",
          min = 80,
          max = 99,
          value = 95,
          step = 1
        ),
        sliderInput(
          "n",
          "Set the Sample Size:",
          min = 2,
          max = 50,
          value = 10,
          step = 1
        ),
        
        title = "Draw a Sample From the Sampling Distribution",
        footer = withMathJax(HTML(
          "&bull; Confidence interval: \\(\\bar{x} \\pm [z_{\\text{crit}} \\times SE]\\) 
<br>&bull; \\(SE\\) : standard deviation of the sampling distribution; smaller SEs mean more precision (less sampling variability) in \\(\\bar{x}\\). 
<br>&bull; \\(z_\\bar{x}\\): the deviation of a sample mean from its population mean expressed in standard error units.
<br>&bull; \\(z_{\\text{crit}}\\): critical \\(z\\)-value that determines the confidence level. 
<br>&bull; Confidence level: the long-run proportion of confidence intervals that capture the population mean. When \\(z_{\\text{crit}}\\) = 1.96, the confidence level is 95%. This is because 95% of sample means (on z-scale) fall between \\(z\\) = -1.96 and 1.96, thus 95% of confidence intervals will capture the population mean."
                    )),
        status = "primary",
        
        actionButton(
          "draw_sample",
          label = "Draw Sample"
        ),
        
        # br(),
        # br(),
        # 
        # uiOutput("sample_info"),
        
        width = 4
      )
    )
  )
)

# -------------------------------------------------------------------------
# Server ------------------------------------------------------------------
# -------------------------------------------------------------------------

server <- function(input, output) {
  
  n <- reactive({
    input$n
  })
  
  se <- reactive({
    sd / sqrt(n())
  })
  
  peak_y <- reactive({
    dnorm(
      mu,
      mean = mu,
      sd = se()
    )
  })
  
  z <- reactive({
    qnorm(1 - (1 - input$confidence / 100) / 2)
  })
  
  samp <- reactiveVal(NULL)
  
  observeEvent(input$draw_sample, {
    
    samp(
      rnorm(
        n(),
        mean = mu,
        sd = sd
      )
    )
  })
  
  observeEvent(input$n, {
    samp(NULL)
  }, ignoreInit = TRUE)
  
  sample_stats <- reactive({
    
    req(samp())
    
    xbar <- mean(samp())
    sample_sd <- sd(samp())
    
    samp_z <- (xbar - mu) / se()
    
    ci_lower <- xbar - z() * se()
    ci_upper <- xbar + z() * se()
    
    list(
      mean = xbar,
      raw_diff = xbar - mu,
      sd = sample_sd,
      se = se(),
      lower = ci_lower,
      upper = ci_upper,
      samp_z = samp_z,
      ci_capture = ifelse(
        ci_lower <= mu & mu <= ci_upper,
        "Yes",
        "No"
        ),
      ci_capture_label = ifelse(
        ci_lower <= mu & mu <= ci_upper,
        "<b style='color:darkgreen;'>YES</b>",
        "<b style='color:red;'>NO</b>"
      )
    )
  })
  
  # -------------------------------------------------------------------------
  # Static Base Plot
  # -------------------------------------------------------------------------
  
  p <- reactive({
    p <- ggplot() +
      geom_segment(
        aes(x = mu, xend = mu, y = 0, yend = peak_y())
      )  +
      stat_function(
        fun = dnorm,
        args = list(
          mean = mu,
          sd = se()
        ),
        xlim = c(
          20, 80
          # mu - 5 * se(),
          # mu + 5 * se()
        )
      ) +
      scale_x_continuous(
        expand = c(0, 0)
      ) +
      annotate(
        "text",
        x = mu,
        y = peak_y(),
        vjust = -0.75,
        label = paste0(
          "\u03bc = ",
          sprintf("%.1f", mu),
          ", \u03C3 = ",
          sprintf("%.1f", sd),
          "\nStd. Error = ",
          sprintf("%.1f", se())
        ),
        fontface = "bold"
      ) +
      scale_y_continuous( # guarantee y axis height for mu label
        expand = expansion(mult = c(0, 0.25))
      ) +
      labs(
        x = "Sample Mean Values",
        y = "Frequency of Sample Mean Values"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      )
  })
  
  output$pieceDat <- renderPlot({
    

    
    # ---------------------------------------------------------------
    # Before first sample: show base distribution only
    # ---------------------------------------------------------------
    
    if (is.null(samp())) {
      
      p() / plot_spacer() +
        plot_layout(
          heights = unit(
            c(1, 0.5),
            c("null", "in")
          )
        )
      
    } else {
      
      # -------------------------------------------------------------
      # After sample is drawn
      # -------------------------------------------------------------
      
      s <- samp()
      stats <- sample_stats()
      
      p_samp <- p() +
        annotate(
          "rect",
          xmin = stats$lower,
          xmax = stats$upper,
          ymin = 0,
          ymax = peak_y(),
          alpha = 0.2,
          fill = ifelse(stats$ci_capture == "Yes", "darkgreen", "red")
        ) +
        geom_segment(
          aes(
          x = stats$mean,
          xend = stats$mean,
          y = 0,
          yend = peak_y()
          ),
          color = "grey40",
          linetype = "longdash"
        ) +
        # annotate(
        #   "text",
        #   x = stats$mean,
        #   y = peak_y(),
        #   vjust = -0.25,
        #   parse = TRUE,
        #   label = paste0(
        #     "bar(x) == '",
        #     sprintf("%.1f", stats$mean),
        #     "' * ',' ~~ z[bar(x)] == '",
        #     sprintf("%.1f", stats$samp_z),
        #     "'"
        #   )
        # ) +
        theme(
          plot.margin = unit(c(0, 0, 0, 0), "cm")
        )
      
      
      p_box <- ggplot(
        tibble(samp = s),
        aes(y = samp)
      ) +
        geom_boxplot(
          aes(x = 1),
          width = 0.05,
          position = position_nudge(x = 0.01)
        ) +
        geom_quasirandom(
          aes(x = .8),
          alpha = 0.35,
          width = 0.15
        ) +
        geom_text_repel(
          aes(x = .8, label = sprintf("%.1f", samp))
        ) +
        coord_flip() +
        scale_y_continuous(
          expand = c(0, 0),
          limits = c(
            20, 80
            # mu - 5 * se(),
            # mu + 5 * se()
          )
        ) +
        scale_x_continuous(
          expand = c(0, 0),
          limits = c(.6, 1.05)
        ) +
        labs(y = "Data From Sample Draw") +
        theme_minimal() +
        theme(
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.title.y = element_blank()
        )
      
      
      p_samp / p_box +
        plot_layout(
          heights = unit(
            c(1, 1),
            c("null", "in")
          )
        )
    }
  }
  )
  
  # output$sample_info <- renderUI({
  #   
  #   if (is.null(samp())) {
  #     
  #     withMathJax(
  #       HTML(
  #       paste0(
  #         "<strong>Sample SD:</strong> --",
  #         "<br>",
  #         "<strong>Sample Mean:</strong> --",
  #         "<br>",
  #         "<strong>Deviation from Pop. Mean: </strong> --",
  #         "<br>",
  #         "<strong>Deviation in SE Units:</strong> --",
  #         "<br>",
  #         "<strong>95% CI [Lower, Upper]:</strong> --",
  #         "<br>",
  #         "<strong>CI Capture Population Mean?</strong> --"
  #       )
  #     )
  #     )
  #     
  #   } else {
  #     
  #     stats <- sample_stats()
  #     
  #     HTML(
  #       paste0(
  #         "<strong>Sample SD:</strong> ",
  #         sprintf("%.2f", stats$sd),
  #         "<br>",
  #         "<strong>Sample Mean:</strong> ",
  #         sprintf("%.1f", stats$mean),
  #         "<br>",
  #         "<strong>Deviation from Pop. Mean: </strong> ",
  #         sprintf("%.2f", stats$raw_diff),
  #         "<br>",
  #         "<strong>Deviation in SE Units:</strong> ",
  #         sprintf("%.2f", stats$samp_z),
  #         "<br>",
  #         "<strong>95% CI [Lower, Upper]:</strong> [",
  #         sprintf("%.2f", stats$lower),
  #         ", ",
  #         sprintf("%.2f", stats$upper),
  #         "]",
  #         "<br>",
  #         "<strong>CI Capture Population Mean? </strong>",
  #         stats$ci_capture_label
  #       )
  #     )
  #   }
  # })
}


# -------------------------------------------------------------------------
# Run App -----------------------------------------------------------------
# -------------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)