library(shiny)
conf.sim <- function(population.mean, population.sd, conf.level, sample.sizes) {
    require(ggplot2)
    require(dplyr)
    require(wesanderson)
    sample.draws <- replicate(100, (mean(rnorm(sample.sizes, mean = population.mean, sd = population.sd))))
    sample.draws <- as.data.frame(sample.draws)
    colnames(sample.draws) <- "sample.values"
    z.value <- ifelse(conf.level == .90, 1.645, ifelse(conf.level == .99, 2.576, 1.96))
    sample.draws <-
        sample.draws %>%
        mutate(
            lower = sample.values - z.value * sd(sample.values),
            upper = sample.values + z.value * sd(sample.values))
    ci <-
        sample.draws %>% mutate(Capture = ifelse(lower < population.mean, ifelse(upper > population.mean, "Yes", "No"), "No"))
    ci$Capture <- factor(ci$Capture, levels = c("No", "Yes"))
    pop_sample_same <-
        ci %>%
        ggplot(aes(x = 1:100, y = sample.values)) +
        geom_point(aes(color = Capture), alpha = .6, size = 1) +
        geom_errorbar(aes(
            ymin = lower,
            ymax = upper,
            color = Capture
        ), alpha = .4) +
        scale_color_manual(values = c('No' = '#CB2314', 'Yes' = '#354823')) +
        geom_hline(yintercept = c(population.mean),
                   linetype = "dashed",
                   color = "#3F5151") +
        annotate("text",
                 x = -12,
                 y = population.mean,
                 label = "mu",
                 parse = TRUE,
                 size = 10) +
        theme_classic() +
        theme(
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 20),
            legend.position = c(0.135, 0.95),
            plot.margin = unit(c(0, 0, 2, 0), "cm")
        ) +
        coord_flip(xlim = c(0, 100), clip = "off") +
        ylim(c(population.mean - population.mean, population.mean +  population.mean)) +
        guides(color = guide_legend(title = expression(paste('Interval captures ', mu, "?")), override.aes = list(size = 2))) +
        labs(title = paste(conf.level * 100, "% ", "Confidence Intervals", sep = "")) +
        labs(y = NULL, x = "Sample Number")
    return(pop_sample_same)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Variability in Sample Means Around a Population Mean"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "samplesizes",
                "Choose your sample size:",
                min = 10,
                max = 100,
                value = 30
            ),
            textInput("mean",
                        "Enter the Population Mean:",
                        value = 50),
            textInput("sd",
                    "Enter the Population Standard Deviation:",
                    value = 10),
        radioButtons("conflevel",
                     "Enter the Confidence Level:",
                     choices = c(0.90, 0.95, 0.99)),
        actionButton("submit", "Draw 100 new samples!")
    ),
        # Show a plot of the generated set of confidence intervals
        mainPanel(
           plotOutput("ciPlot", height = 600)
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    mean <- eventReactive(input$submit, {
        input$mean
    })
    sd <- eventReactive(input$submit, {
        input$sd
    })
    conf.level <- eventReactive(input$submit, {
        input$conflevel
    })
    sample.sizes <- eventReactive(input$submit, {
        input$samplesizes
    })
    output$ciPlot <- renderPlot({
        conf.sim(population.mean = as.numeric(mean()),
                 population.sd = as.numeric(sd()),
                 conf.level = as.numeric(conf.level()),
                 sample.sizes = sample.sizes())
        })
}
# Run the application
shinyApp(ui = ui, server = server)
