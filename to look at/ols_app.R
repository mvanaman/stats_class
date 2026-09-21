packages <- c("tidyverse", "shiny", "wesanderson", "DT", "ggExtra", "ggrepel", "ggiraph")
lapply(packages, require, character.only = TRUE)

plot_guess <- function(
    slope, y_intercept, resids_guess = FALSE, resids_OLS = FALSE
){
  # get data ----
  set.seed(350)
  Sigma <- matrix(c(10,3,3,2),2,2)
  data <- MASS::mvrnorm(n = 20, mu = rep(5, 2), Sigma, empirical = TRUE) %>% abs() %>% round(2)
  x <- data[, 1]
  y <- data[, 2]
  # fit OLS
  OLS <- lm(y ~ x)
  # statistics from guess
  slope <- slope
  y_intercept <- y_intercept
  y_hat_guess <- y_intercept + (slope * x)
  # sums of squares from guess
  y_resid_guess <- y - y_hat_guess
  y_resid_guess_sq <- y_resid_guess^2
  SSs <- sum(y_resid_guess_sq)
  ## combine into table
  data <- tibble(
    X = x, 
    Y = y, 
    "Predicted Y (OLS)" = OLS$fitted.values,
    "Residual (OLS)" = OLS$residuals,
    "Squared Residual (OLS)" = OLS$residuals^2,
    "Predicted Y (Guess)" = y_hat_guess, 
    "Residual (Guess)" = y_resid_guess,
    "Squared Residual (Guess)" = y_resid_guess_sq
  ) %>% 
    mutate(
      across(.cols = where(is.numeric), round, 1),
      interactive_label = paste(
        "Observed Y: ",
        format(Y, nsmall = 1),
        "<br>",
        "Predicted Y (OLS): ", 
        format(`Predicted Y (OLS)`, nsmall = 1),
        "<br>",
        "Predicted Y (Guess): ",
        format(`Predicted Y (Guess)`, nsmall = 1)
        )
      )
  data_tab <- data %>% 
    mutate(across(.cols = 3:(ncol(.)-1), round, 2), across(.cols = 3:ncol(.), format, nsmall = 2))
  # get summary Stats ----
  SSs <- tibble("Sum of Sqaured Residuals" = c(sum(data$`Squared Residual (Guess)`), sum(data$`Squared Residual (OLS)`)))
  regression_guess <- tibble("<i>Y</i>-Intercept" = y_intercept, Slope = slope)
  regression_OLS <- tibble(
    "<i>Y</i>-Intercept" = OLS$coefficients["(Intercept)"],
    Slope = OLS$coefficients["x"]
  )
  ## r-squared for OLS
  model_OLS <- sum((data$Y - OLS$fitted.values)^2)
  residual_OLS <- sum((data$Y - mean(data$Y))^2)
  ## r-squared from guess
  model_guess <- sum((data$Y - y_hat_guess)^2)
  residual_guess <- sum((data$Y - mean(data$Y))^2)
  ## combine into table
  reg_table <- bind_rows(regression_guess, regression_OLS)
  reg_table <- cbind(Line = c("Your Guess", "OLS"), reg_table)
  reg_table <- cbind(reg_table, SSs)
  reg_table <- arrange(reg_table, Line)
  reg_table_long <- reg_table %>% 
    pivot_longer(cols = -Line, names_to = "Statistic", values_to = "Value") 
  reg_table_guess <- reg_table_long %>% filter(Line == "Your Guess") %>% select(-Line)
  reg_table_OLS <- reg_table_long %>% filter(Line == "OLS") %>% select(-Line)
  reg_table_long <- full_join(reg_table_OLS, reg_table_guess, by = "Statistic")
  reg_table_long <- reg_table_long %>% 
    rename("OLS" = Value.x, "Your Guess" = Value.y) %>% 
    mutate(
      across(.cols = where(is.numeric), round, 1),
      across(.cols = where(is.numeric), format, nsmall = 1)
      )
  reg_table_long[nrow(reg_table_long), ] <- as.list(paste("<i>", reg_table_long[nrow(reg_table_long), ],"</i>"))
  
  # display plot -----
  plot <- ggplot(
    data, 
    aes(
      x = X,
      y = Y,
      tooltip = interactive_label, 
      data_id = interactive_label
      )
    ) +
    geom_point(alpha = 0) +
    geom_point_interactive(
      shape = 1, 
      hover_nearest = TRUE, 
      color = wes_palette("Moonrise2")[3]
      ) +
    geom_abline(
      data = reg_table, 
      aes(slope = Slope, intercept = `<i>Y</i>-Intercept`, color = Line), 
      size = c(1, 1),
      show.legend = TRUE
    ) +
    # connects dots to line of best guess
    {if (resids_guess)
      geom_segment(
        aes(
          x = X,
          y = Y,
          xend = X,
          yend = `Predicted Y (Guess)`
        ),
        linetype = "dashed",
        color = "#C27D38",
        alpha = 0.60
      ) 
    } +
    {if (resids_guess)
      geom_text_repel(aes(label = `Residual (Guess)`), size = 3)
    } +
    {if (resids_OLS)
      # connects dots to line of OLS
      geom_segment(
        aes(
          x = X,
          y = Y,
          xend = X,
          yend = `Predicted Y (OLS)`
        ),
        linetype = "dashed",
        color = "#798E87",
        alpha = 0.60
      ) 
    } +
    {if (resids_OLS)
      geom_text_repel(aes(label = `Residual (OLS)`), size = 3) 
    } +
    theme_classic() +
    theme(legend.position = "bottom") +
    scale_color_manual(
      values = wes_palette(2, name = "Moonrise2", type = "discrete"), name = ""
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2))

  plot <- ggMarginal(
    plot, 
    type = "density", 
    xparams = list(fill = wes_palette("Moonrise2")[3]),
    yparams = list(fill = wes_palette("Moonrise2")[3])
    )
  plot <- girafe(
    # ggobj = plot,
    code = {print(plot)},
    options = list(
      opts_tooltip(
        css = "background: rgba(204, 197, 145, .4); padding:5px; border-radius:6px"
        ),
      opts_hover(css = "fill:#CCC591; stroke:#29211F; stroke-width:1px;")
      )
    )

    return( # ----
          list(
            data_tab = data_tab,
            plot = plot,
            reg_table_long = reg_table_long
          )
  )
}

# css <- jsonlite::fromJSON(readLines("custom.css"), warn = F)
ui <- fluidPage(
  # tags$link(rel = "stylesheet", type = "text/css", href = css),
  # tags$style(rel = "stylesheet", type = "text/css", href = "custom.css"),
  sidebarLayout(
    sidebarPanel(
      sliderInput( # 1st (and only) entry in this column
        "y_intercept", # object name that gets referred to in server
        label = HTML("Guess the <i>Y</i>-Intercept"), # label displayed to user above text box
        value = 5, # default is the mean of data$Y above, always update if data changes
        step = 0.01, # increment for if user uses clicker thing to move values up and down
        min = 0, 
        max = 10,
        round = 1,
        ticks = FALSE
      ),
      sliderInput(
        "slope",
        label = "Guess the Slope",
        value = 0,
        step = 0.01,
        min = -2,
        max = 2,
        round = 1,
        ticks = FALSE
      ),
      tableOutput("reg_table_long"),
      checkboxInput(
        "resids_guess",
        label = "Plot residuals (for your guess)?", 
        value = FALSE 
      ),
      checkboxInput(
        "resids_OLS", 
        label = "Plot residuals (for OLS)?", 
        value = FALSE 
        )
      ),
    mainPanel(ggiraphOutput("plot", width = 900, height = 850))
  )
  )

server <- function(input, output, session) {
  
  # create objects to use later
  # wrapping in the reactive({}) function lets you re-used object
  slope <- reactive({input$slope})
  y_intercept <- reactive({input$y_intercept})
  resids_guess <- reactive({input$resids_guess})
  resids_OLS <- reactive({input$resids_OLS})

  guess_plot <- reactive({
    plot_guess(
      slope = slope(), 
      y_intercept = y_intercept(),
      resids_guess = resids_guess(),
      resids_OLS = resids_OLS()
    )})
  
  output$plot <- renderggiraph({
    guess_plot()$plot
  })
  
  output$reg_table_long <- renderTable({
    guess_plot()$reg_table_long
  }, 
  sanitize.text.function=function(x){x},
  align = "lcc"
  )
  
}

shinyApp(ui = ui, server = server)

# increase sample size DONE
# move legend to left DONE
# add marginals DONE  
# adjust figure dimensions DONE
# switch to ggrepel DONE
# if needed, tweak position jitter
# increase menu size DONE
# move table to bottom DONE
# adjust background of hover DONE
