library(tidyverse)
library(janitor)
library(patchwork)

# OUR SAMPLE
class_sample <- tibble(
  Perceived_Immorality = c(
    2,
    1,
    2.67,
    2.3,
    0,
    0.33,
    2.6,
    3,
    -3,
    2.3,
    4,
    4
  )
)
(our_class <- ggplot(class_sample, aes(x = Perceived_Immorality)) +
  geom_histogram(binwidth = 1) +
  labs(y = "Frequency",
       title = "Our Class",
       x = "Perceived Immorality Score",
       subtitle = paste(
    "Mean = ", 
    round(mean(class_sample$Perceived_Immorality), 2), # MEAN
    ",\nStd. Dev. = ",
    round(sd(class_sample$Perceived_Immorality), 2), # SD
    sep = ""
  )) +
  theme_minimal())



## ASSUME POP MEAN = 2.00, SD = 1.50
set.seed(20)
sample <- tibble(Perceived_Immorality = round(rnorm(n = 12, mean = 2.00, sd = 2.5), 1))
print(sample)
random_new_sample <- ggplot(sample, aes(x = Perceived_Immorality)) +
  geom_histogram(binwidth = 1) +
  labs(y = "Frequency",
       title = "New Random Sample",
       x = "Perceived Immorality Score",
       subtitle = paste(
         "Mean = ", 
         round(mean(sample$Perceived_Immorality), 2), # MEAN
         ",\nStd. Dev. = ",
         round(sd(sample$Perceived_Immorality), 2), # SD
         sep = ""
       )) +
  theme_minimal()
our_class + random_new_sample + 
  patchwork::plot_layout(axes = "collect_x")

sample_means <- tibble(
  means = c(
    mean(class_sample$Perceived_Immorality),
    mean(sample$Perceived_Immorality)
  )
)
ggplot(sample_means, aes(x = means)) +
    geom_histogram() +
    labs(y = "Frequency",
         title = "Means of Our Two Samples",
         x = "Perceived Immorality Sample Mean",
         subtitle = paste(
           "Mean of means = ", 
           round(mean(sample_means$means), 2), # MEAN
           "\nStd. Dev. of Means = ",
           round(sd(sample_means$means), 2), # SD
           sep = ""
         )) +
    theme_minimal()


set.seed(350)
n <- 999998
random_samples <- lapply(
  1:n,
  function(x) mean(rnorm(n = 12, mean = 2.00, sd = 2.5))
  ) %>% 
  unlist() %>% 
  tibble("Perceived_Immorality_Sample_Means" = .) %>% 
  add_row(Perceived_Immorality_Sample_Means = c(sample_means[[1]][1], sample_means[[1]][2]))
# Get Mean of Means
mean(random_samples$Perceived_Immorality_Sample_Means)
sd(random_samples$Perceived_Immorality_Sample_Means)
# mean of sampling distribution
# SE = s of sampling distribution

# sampling distribution: distribution of 
# means of means of samples
# Standard error: s of sampling 
# distribution
ggplot(random_samples, aes(x = Perceived_Immorality_Sample_Means)) +
  geom_histogram() +
  labs(y = "Frequency",
       title = paste("Means of ", "1,000,000", " Random Samples", sep = ""),
       x = "Sample Mean",
       subtitle = paste(
         "Mean of Means = ", 
         round(mean(random_samples$Perceived_Immorality_Sample_Means), 5), # MEAN
         ",\nStd. Dev. of Means = ",
         round(sd(random_samples$Perceived_Immorality_Sample_Means), 2), # SD
         sep = ""
       )) +
  theme_minimal()


























# type 1 error
data <- seq(0.01, 1, by = .01)
sample(data, size = 1)

# type 2 error
data_2 <- c(
        rep(0.01, 20),
        rep(0.02, 20),
        rep(0.03, 20),
        rep(0.04, 20),
        sample(seq(0.05, 1, by = 0.01), size = 20, replace = TRUE)
)
data_2
data_2 %>% 
        as_tibble() %>% 
        mutate(sig = ifelse(value < 0.05, "yes", "no")) %>% 
        tabyl(sig) %>% 
        adorn_pct_formatting()
sample(data_2, size = 1)
