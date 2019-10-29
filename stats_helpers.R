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

# Exercise 5 helper code
tweet_data <- read_csv("data/tweet_data.csv")

just_margin <- lm(DoF ~ trump_margin, data = tweet_data)
just_party  <- lm(DoF ~ party, data = tweet_data)
additive    <- lm(DoF ~ trump_margin + party, data = tweet_data)
interactive <- lm(DoF ~ trump_margin * party, data = tweet_data)

tweet_models <- tweet_data %>% 
  mutate(
    `DoF ~ trump_margin` = predict(just_margin),
    `DoF ~ party` = predict(just_party),
    `DoF ~ trump_margin + party` = predict(additive),
    `DoF ~ trump_margin * party` = predict(interactive)
    ) %>% 
  gather(model, prediction, -c(twitter:trump_margin)) %>% 
  mutate(model = factor(model, levels = c("DoF ~ trump_margin", "DoF ~ party", "DoF ~ trump_margin + party","DoF ~ trump_margin * party"))) %>% 
  ggplot(aes(x = trump_margin, y = DoF, group = party)) +
  geom_point(aes(colour = party)) +
  geom_line(aes(y = prediction), size = 1) +
  facet_wrap(~model) +
  theme_linedraw(base_size = 14) +
  scale_color_brewer(palette="Set1", direction = -1) +
  labs(
    title = "Does vote margin affect formality in tweets?",
    x = "2016 Trump vote margin",
    y = "Degree of Formality",
    colour = "Party"
  ) +
  theme(legend.position = "bottom")
