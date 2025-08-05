## STAT 7220 - Assumption Checking ##

library(tidyverse)
library(readxl)

## Read in the Egg Rating Data ##

egg_data <- read_excel("Egg Rating.xlsx")

## Build a One-Way ANOVA Model ##

egg_mod <- aov(Rating ~ Technique, data=egg_data)

## Evaluate Normality Assumption using QQ Plot ##

egg_data |>
  ggplot(aes(sample=resid(egg_mod))) +
  geom_qq() +
  geom_qq_line() +
  labs(x = "Theoretical Values",
       y = "Empirical Values") +
  theme_classic()

## As we can see, the points mostly fall near the line
## bisecting the graph. We do have some deviation but
## not a tremendous or alarming amount. ##

## Let's perform the Shaprio-Wilk Test of Normality ##

library(broom)

egg_mod |>
  resid() |>
  shapiro.test() |>
  tidy()

## As we can see, p > 0.05, which would indicate
## that the test more strongly supports the null
## hypothesis. Contextually, this means that 
## we have strong evidence in favor of the normality
## assumption. ##

## Evaluate Constant Variance Assumption ##

egg_data |>
  ggplot(aes(x=fitted(egg_mod),y=resid(egg_mod))) +
  geom_point() +
  geom_hline(yintercept=0,linetype='dashed',
             color='red') +
  geom_hline(yintercept=3,color='blue') +
  geom_hline(yintercept=-3,color='blue') +
  labs(x = "Predicted y's",
       y = "Residuals") +
  theme_classic()

## As we can see, for each group mean, the residuals
## are mostly centered around 0, which implies that
## the residual mean being approximately 0 seems reasonable.
## Additionally, while the residuals are not exactly the
## same width, they are not drastically different either. 
## I would conclude that the constant variance assumption
## is reasonable. ##

## Note, the blue lines of +/- 3 can be used as a rough guide
## for identifying outliers. More on this later ##

## Levene's Test ##

library(car)

leveneTest(egg_mod) |>
  tidy() |>
  select(statistic,p.value)

## Since p > 0.05, this indicates that the data more strongly
## support H0, meaning that the assumption of constant variance 
## seems reasonable ##
