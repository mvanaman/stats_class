library(shiny)
library(shinythemes)
samp.dist.sim <-
    function(population.mean, population.sd, sample.sizes) {
        require(ggplot2)
        require(dplyr)
        require(wesanderson)
        require(grid)
        # sample draws for empirical distribution
        sample.draws <- replicate(
            100, mean(rnorm(n = sample.sizes, mean = population.mean, sd = population.sd)))
        sample.draws <- as.data.frame(sample.draws)
        colnames(sample.draws) <- "sample.means"
        breaks <- pretty(
            range(sample.draws$sample.means),
            n = nclass.FD(sample.draws$sample.means),
            min.n = 1)
        bwidth <- breaks[2] - breaks[1]
        mean.sd <- data.frame(
            labels = c(paste("Mean of these 100 samples = ", round(mean(sample.draws$sample.means), 3)),
                       paste('Standard deviation of the means ("standard error") = ', round(sd(sample.draws$sample.means), 3)))
        )

        samp.dist <- ggplot(sample.draws, aes(x = sample.means)) +
            geom_histogram(
                aes(y = (..count..)),
                binwidth = bwidth,
                fill = "#FAEFD1",
                color = "#DC863B",
                alpha = 0.7) +
            stat_function(
                fun = function(x) dnorm(x, mean = mean(sample.draws$sample.means), sd = sd(sample.draws$sample.means)) * 100 * bwidth,
                color = "#35274A", size = 0.75, alpha = 0.75) +
            ylim(c(0, 30)) +
            geom_vline(xintercept = c(population.mean),
                       linetype = "dashed",
                       color = "#3F5151") +
            scale_x_continuous(
                limits = c(population.mean - 1 * population.sd, population.mean +  1 * population.sd),
                sec.axis = sec_axis(~.,
                                    breaks = population.mean,
                                    labels = expression(mu))) +
            labs(title = "Sampling Distribution",
                 x = expression("" %<-% "Sample Means" %->% ""),
                 y = "Frequency",
                 caption = paste(as.character(mean.sd[1, ]), "\n", as.character(mean.sd[2, ]), sep = "")) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5, vjust = 5),
                  plot.margin = unit(c(1, 0, 1, 0), "cm"),
                  plot.caption = element_text(hjust = 0.5, vjust = -0.5),
                  plot.subtitle = element_text(hjust = ),
                  text = element_text(size = 20),
                  axis.line.x.top = element_blank(),
                  axis.text.x.top = element_text(size = 25)
            ) +
            coord_cartesian(ylim = c(0, 31), clip = "off")
        return(samp.dist)
    }
# Define UI for application that draws a histogram
ui <- fluidPage(
    shinythemes::themeSelector(),
    # Application title
    titlePanel("Sampling Variability Around a Population Mean"),
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
            actionButton("submit", "Draw 100 new samples and plot them!")
        ),
        # Show a plot of the generated set of confidence intervals
        mainPanel(
            plotOutput("distPlot", height = 600)
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
    sample.sizes <- eventReactive(input$submit, {
        input$samplesizes
    })
    output$distPlot <- renderPlot({
        samp.dist.sim(population.mean = as.numeric(mean()),
                 population.sd = as.numeric(sd()),
                 sample.sizes = sample.sizes())
    })
}
# Run the application
shinyApp(ui = ui, server = server)
