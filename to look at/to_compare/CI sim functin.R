conf.sim <- function(population.mean, population.sd, conf.level, sample.size) {
  require(ggplot2)
  require(dplyr)
  require(wesanderson)
  sample.draws <- replicate(100, (mean(rnorm(sample.size, mean = population.mean, sd = population.sd))))
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
  ), alpha = .6) +
  scale_color_manual(values = c('No' = '#550307', 'Yes' = '#354823')) +
  geom_hline(yintercept = population.mean,
             linetype = "dashed",
             color = "#3F5151") +
    annotate("text",
             x = -14,
             y = population.mean,
             label = "mu",
             parse = TRUE) +
    theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    plot.margin = unit(c(0, 0, 1, 0.5), "cm")
  ) +
  coord_flip(xlim = c(0, 100), clip = "off") +
    ylim(c(population.mean - (2.5 * population.sd), population.mean + (2.5 * population.sd))) +
  guides(color = guide_legend(title = "Interval Captures \nPopulation Mean?")) +
  labs(title = paste(conf.level * 100, "% ", "Confidence Intervals", sep = "")) +
  ylab(label = NULL) +
  xlab(label = "Sample Number")

  return(pop_sample_same)
}

conf.sim(population.mean = 200, population.sd = 10, conf.level = .90, sample.size = 100)

