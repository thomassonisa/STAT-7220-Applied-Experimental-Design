## Experiments with Random Effects ##

library(tidyverse)
library(readxl)
library(broom)
library(rstatix)
#install.packages('lme4')
library(lme4)

## Read in the Data ##

math <- read_excel("math_scores.xlsx")

math |>
  glimpse()

##

## Calculate Means and SDs by School ##

math |>
  group_by(School) |>
  get_summary_stats(MathScore,type='mean_sd') |>
  select(-variable)

## Fit a random effects model using the nlme package


random_effects_model <- lme(MathScore ~ 1, random = ~ 1 | School, data = data)

summary(random_effects_model)

# Extract the variance components

var_components <- VarCorr(random_effects_model)
var_components

dat <- tibble(Teacher = as.factor(rep(1:5,each=7)),
              Score = c(35,32,41,42,31,36,35,36,32,34,40,37,44,42,28,27,23,15,29,28,33,27,16,40,32,36,38,22,32,34,26,28,23,36,28))

mod <- lmer(Score ~ 1 + (1|Teacher), data = dat,REML=F)

summary(mod)
