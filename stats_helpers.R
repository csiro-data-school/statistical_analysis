library(tidyverse)

income_data <- read_csv("data/income.csv")

sample_income <- function(n) {
  sampled_data <- sample_n(income_data, size = n, replace = TRUE, weight = Count) %>% 
    mutate(income = map2_dbl(low, high, ~runif(1, .x, .y)))
  
  sampled_data$income
}

sample_height <- function(n) {
  low_n <- ceiling(n/2)
  high_n <- floor(n/2)
  
  low_heights <- rnorm(low_n, mean = 168, sd = 3)
  high_heights <- rnorm(high_n, mean = 178, sd = 3)
  
  c(low_heights, high_heights)
}

sample_nuggety <- function(n) {
  values <- c(1:10, 50)
  weights <- c(1,2,3,4,5,5,4,3,2,1,0.05)
  
  sample(values, size = n, replace = T, prob = weights)
}
