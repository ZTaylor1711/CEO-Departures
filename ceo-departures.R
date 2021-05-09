#clear environment
rm(list = ls())

#load libraries
library(tidyverse)

#load CEO Departure data from Tidy Tuesday
departures_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv")

###filter data set and visualize
departures_raw %>%
  filter(departure_code < 9) %>%
  mutate(involuntary = if_else(departure_code %in% 3:4, "involuntary", "other")) %>%
  filter(fyear > 1995, fyear < 2019) %>%
  count(fyear, involuntary) %>%
  ggplot(aes(fyear, n, color = involuntary)) +
  geom_line(size = 1.2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", lty = 2) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Number of CEO departures", color = NULL)

#creating new dataframe filtering to years with enough data to analyze. Including >1995, <2019
departures <- departures_raw %>%
  filter(departure_code < 9) %>%
  mutate(involuntary = if_else(departure_code %in% 3:4, "involuntary", "other")) %>%
  filter(fyear > 1995, fyear < 2019)

###Bootstrapping model to check robustness of result
#loading
library(broom)

#creating new dataframe to pivot wider
df <- departures %>%
  count(fyear, involuntary) %>%
  pivot_wider(names_from = involuntary, values_from = n)

#model CEO Departures using general Linear Model
mod <- glm(cbind(involuntary, other) ~ fyear, data = df, family = "binomial")
summary(mod)

#convert logistic scale to linear scale to ease interpreation of results
tidy(mod, exponentiate = TRUE)

#creat bootstrap samples
library(rsample)

set.seed(123)
ceo_folds <- bootstraps(departures, times = 1e3)
ceo_folds

#create function to apply model to each bootstramp sample and return coefficients
fit_binom <- function(split) {
  df <- analysis(split) %>%
    count(fyear, involuntary) %>%
    pivot_wider(names_from = involuntary, values_from = n)
  
  mod <- glm(cbind(involuntary, other) ~ fyear, data = df, family = "binomial")
  tidy(mod, exponentiate = TRUE)
}

#apply function to each bootstrap sample
boot_models <- ceo_folds %>% mutate(coef_info = map(splits, fit_binom))
boot_models


###Explore Results
percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals

#Visualize Results
boot_models %>%
  unnest(coef_info) %>%
  filter(term == "fyear") %>%
  ggplot(aes(estimate)) +
  geom_vline(xintercept = 1, lty = 2, color = "black", size = 2) +
  geom_histogram(fill = "light blue", color = "black") +
  labs(
    x = "Annual increase in involuntary CEO departures",
    title = "Over this time period, CEO departures are increasingly involuntary",
    subtitle = "Each passing year corresponds to a departure being 1-2% more likely to be involuntary"
  ) + 
  geom_vline(xintercept = 1.01611, lty = 2, color = "red", size = 1.5)


# adding vertical line to show mean effect from bootstrap samples
mean_effect <- boot_models %>%
  unnest(coef_info) %>%
  filter(term == "fyear")

mean_effect_value <- (mean_effect$estimate)
