library(tidyverse)

# Exercise 1 helper code

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

# Exercise 4 helper code

x = seq(1, 10, by = 0.5)
perfect_linear <- (2 * x) + 5

set.seed(50)
test_data <- tibble(x, perfect_linear) %>% 
  mutate(
    #Normal noise
    normal_noise = perfect_linear + rnorm(n = length(x), mean = 0, sd = 2),
    
    #Non-normal noise: use exponential distribution
    non_normal_noise = perfect_linear + rexp(n = length(x), rate = 0.5),
    
    #Outliers: normal noise plus 10% chance of adding 8 to the value
    outliers = perfect_linear + 
      rnorm(n = length(x)) + 
      sample(c(0, 8), prob = c(0.9, 0.1), size = length(x), replace = T),
    
    #Non-linear data: make the relationship quadratic instead
    non_linear = x^2 + 5
  ) %>% 
  sample_frac()


