# HW 1
packages <- c("here", "tidyverse", "gt")
lapply(packages, require, character.only = TRUE)

generate_class_table <- function(seed, n, m, sd, key = FALSE, ...) {
  set.seed(seed)
  # grade <- c( # for testing
  #   65,
  #   86,
  #   66,
  #   63,
  #   53,
  #   73,
  #   64,
  #   63,
  #   57,
  #   62,
  #   72,
  #   75,
  #   65,
  #   66,
  #   61,
  #   66
  # )
  grade <- round(rnorm(n = n, mean = m, sd = sd), 0)
  m <- mean(grade)
  sd <- sd(grade)
  dev <- unlist(lapply(grade, function(i) i - m))
  dev_2 <- dev^2
  SS <- sum(dev_2)
  df <- n - 1
  s_2 <- sd^2
  class_blank <- tibble(
    i = 1:n,
    grade = grade,
    devs = "",
    devs_2 = ""
  )
    class_blank_key <- class_blank %>% 
      mutate(
        devs = dev,
        devs_2 = dev_2,
        across(
          .cols = c(
            devs,
            devs_2
          ), 
          \(x) format(round(x, 3), nsmall = 3)
        )
          ) %>% 
      gt() %>%
      cols_label(
        i = md("$i$"),
        grade = "grade",
        devs = md("$x_i - \\bar{x}$"),
        devs_2 = md("$(x_i - \\bar{x})^2$")
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "all"
        ),
        locations = cells_body()
      ) %>%
      tab_options(data_row.padding = px(1))
    
    class_key_stats <- tibble(
      Statistic = c(
        "$\\bar{x}$",
        "$n$",
        "$SS$",
        "$df$",
        "$s^2$",
        "$s$"
      ),
      Value = c(
        m,
        n,
        SS,
        df,
        SS / df,
        sd
      )
    ) %>% 
      mutate(
        across(.cols = -Statistic, \(x) str_squish(format(round(x, 3), nsmall = 3)))
      ) %>% 
      gt() %>% 
      fmt_markdown(columns = Statistic) %>%
      tab_options(data_row.padding = px(1))
    
  class_blank_tab <- class_blank %>%
    gt() %>%
    cols_label(
      i = md("$i$"),
      grade = "grade",
      devs = md("$x_i - \\bar{x}$"),
      devs_2 = md("$(x_i - \\bar{x})^2$")
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "all"
      ),
      locations = cells_body()
    ) %>%
    tab_options(data_row.padding = px(1))
  return(list(
    blank_table = class_blank_tab, 
    key_table = class_blank_key, 
    key_stats = class_key_stats)
    )
}

stats_blank <- tibble(
  Statistic = c(
    "$\\bar{x}$",
    "$n$",
    "$SS$",
    "$df$",
    "$s^2$",
    "$s$"
  ),
  Value = c(rep("", 6))
) %>% 
  gt() %>% 
  fmt_markdown(columns = Statistic) %>%
  tab_options(data_row.padding = px(1))