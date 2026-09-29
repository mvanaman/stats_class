library(tidyverse)
library(jtools)
set.seed(100)

N <- 1000

# Standardized sleep_quality
sleep_z <- rnorm(N)

# Temperature causes both variables
# Cor(temp, ice cream) ≈ .80
# Cor(temp, crime)     ≈ .60
caff_z <- 0.80 * sleep_z + sqrt(1 - 0.80^2) * rnorm(N)
att_z <- -0.60 * sleep_z + sqrt(1 - 0.60^2) * rnorm(N)

# Give variables more intuitive units
dat <- tibble(
  sleep_quality = 80 + 10 * sleep_z,
  caffeine = 500 + 100 * caff_z,
  attention = 20 + 5 * att_z
)
dat %>% 
  # select(caffeine, attention) %>% 
  psych::pairs.panels(pch = ".")
dat %>% 
  slice(sample(1:nrow(.), size = 1))

lm(attention ~ caffeine, data = dat) %>% 
  summ(scale= T, center = T,   transform.response = TRUE)

lm(attention ~ caffeine + sleep_quality, data = dat) %>% 
  summ(scale= T, center = T,   transform.response = TRUE )

dat_random <- dat %>%
  mutate(
    caffeine = sample(caffeine)
  )
psych::pairs.panels(dat_random, pch = ".")
dat_random %>% 
  slice(sample(1:nrow(.), size = 1))
lm(attention ~ caffeine, data = dat_random) %>% 
  summ(scale= T, center = T,   transform.response = TRUE)
lm(attention ~ caffeine + sleep_quality, data = dat_random) %>% 
  summ(scale= T, center = T,   transform.response = TRUE )
