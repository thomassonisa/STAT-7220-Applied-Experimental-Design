## Comparative Experiments Code ##

#install.packages(c('tidyverse','readxl'))

## Uncomment and run the above code if these packages are not installed ##

## Load the required libraries ##

library(tidyverse)
library(readxl)

## Read in the Can Data ##

can_data <- read_excel("Comparative Experiments/Can Temperature.xlsx")

## Get a glimpse of the data ##

can_data |>
  glimpse()

## <dbl> is a numeric/quantitative data type ##
## <chr> is a character/categorical data type ##

## Perform Descriptive Analyses ##

#install.packages('rstatix')

library(rstatix)

can_data |>
  group_by(Treatment) |>
  get_summary_stats(Temperature, type = "mean_sd")

## So we can see that the mean temperature of cans in the 
## plastic cooler is 34.4 degrees and the mean of the cans
## in the steel cooler is 33.1 degrees. ##

## Generate a Boxplot ##

can_data |>
  ggplot(aes(x = Treatment, y = Temperature)) +
  geom_boxplot() +
  labs(title = "Boxplot of Can Temperatures by Cooler Type",
       x = "Cooler Type",
       y = "Temperature (Degrees Fahrenheit)") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.50))

## Since the two boxes do not have substantial overlap,
## this suggests that the may be a statistically meaningful
## difference between the two group means ##

## Calculate Estimates for Grand Mean ##

mu_hat <- mean(can_data$Temperature)

print(mu_hat)

## The grand mean is 33.75 ##

## Now, let's save those group means we calculated earlier ##

group_means <- can_data |>
  group_by(Treatment) |>
  summarise(mean_temp = mean(Temperature))

print(group_means)

## Finally, we can calculate our tau hats ##

tau_hat <- group_means$mean_temp - mu_hat

print(tau_hat) 

## In the case of just two treatment groups,
## the tau hat's will just be the opposite sign of each other ##

## Performing the Test in a Simpler Way ##

## H0: The mean temperature of cans in the plastic cooler 
## is equal to the mean temperature of cans in the steel cooler ##

## HA: The mean temperature of cans in the plastic cooler
## is not equal to the mean temperature of cans in the steel cooler ##

## Build the AOV Model ##

#install.packages('broom')

library(broom)

can_aov <- aov(Temperature ~ Treatment, data = can_data)

can_aov |>
  tidy()

## The p-value is less than 0.05, so we have substantial evidence
## in favor of the alternative hypothesis ##

## Perform a T-Test (Two Group Case) ##
## H0: The mean temperature of cans in the plastic cooler 
## is equal to the mean temperature of cans in the steel cooler ##

## HA: The mean temperature of cans in the plastic cooler
## is not equal to the mean temperature of cans in the steel cooler ##

ttest <- t.test(Temperature ~ Treatment, data = can_data)

## Check Results ##

ttest |>
  tidy() |>
  select(statistic,p.value)

## The p-value is less than 0.05, so we have substantial evidence
## in favor of the alternative hypothesis ##
