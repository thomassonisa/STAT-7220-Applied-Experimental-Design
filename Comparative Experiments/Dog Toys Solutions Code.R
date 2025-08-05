## Rubber Dog Balls Experiment Answer Key ##

install.packages('tinytex')
tinytex::install_tinytex()

library(tidyverse)
library(readxl)

## Read in Rubber Dog Ball Data ##

dogs <- read_excel("Dog Toys.xlsx")

## 1. Specify the outcome and independent variables. 
## What lurking variables might be present? ##

## Answer: Outcome = PSI when ball rips; IV = Formulation.
## Could be a lurking variable if different operators are
## used to test the balls ##


## 2. Briefly explain why a completely randomized design 
## might be appropriate in this context ##

## Answer: The ball manufacturing process for the 
## different formulations are reasonably independent 
## of each other. Moreover, since there runs a low risk
## of lurking variables, a CRD seems appropriate ##


## 3. Perform an appropriate exploratory analysis ##

## Descriptive Analysis ##

library(rstatix)

dogs |>
  group_by(Formulation) |>
  get_summary_stats(PSI, type = "mean_sd")

## We can see the mean PSI for F2 > PSI for F1 ##

## Graphical Analysis ##

dogs |>
  ggplot(aes(x = Formulation, y = PSI)) +
  geom_boxplot() +
  labs(title = "PSI by Formulation",
       x = "Formulation",
       y = "PSI") +
  theme_classic()

## Formulation 1 & 2 don't have a lot of overlap. ##
## It seems like Formulation 2 is more durable. ##

## 4. Perform an appropriate inferential analysis 
## (including the specification of H0 & H1) ##

## H0: mu1 = mu2
## H1: mu1 =/= mu2 ##

## Fit ANOVA ##

aov_mod <- aov(PSI ~ Formulation, data = dogs)

library(broom)

aov_mod |>
  tidy()

## 5. Provide contextual conclusions -- 
## which formulation seems more durable? ##

## Answer: Since the p-value from the test is 
## less than our typically used alpha = 0.05 
## threshold, this would indicate, in conjunction
## with the descriptive analysis, that Formulation 2
## is more durable than Formulation 1. ##


## 6. What questions remain? ##

## Answer: Does the diameter of the ball make a difference?
## What about its ability to withstand piercing, like might
## happen with sharp dog teeth? Also, dogs drool a lot, so
## how does that affect the durability of the balls? ##
