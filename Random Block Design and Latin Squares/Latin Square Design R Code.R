## STAT 7220 Latin Square Design Code ##

library(tidyverse)
library(readxl)

## Read in Data ##

dog_toys <- read_excel("Latin Square Dog Toys.xlsx")

dog_toys |>
  glimpse()

## Perform Descriptive Analysis ##

## Summary Statistics ##

library(rstatix)

dog_toys |>
  group_by(Formulation) |>
  get_summary_stats(PSI, type = "mean_sd")

## F3 seems to have the greatest PSI ##

## Generate Boxplot for Formulation ##

dog_toys |>
  ggplot(aes(x = Formulation, y = PSI)) +
  geom_boxplot() +
  labs(title = "Breaking PSI of Different Rubber Formulations",
       subtitle = "Dog Toy Durability Test") +
  theme_classic()

## Formulation 3 is generally higher than formulations 1 and 2 
## but it also seems like F2 >> F1. ##

## Fit ANOVA Model ##

dog_mod <- aov(PSI ~ Machine + Operator + Formulation, data = dog_toys)

## Evaluate Assumptions ##

## Check for Normality ##

dog_toys |>
  ggplot(aes(sample=resid(dog_mod))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q Plot of Residuals") +
  theme_classic()

## Not bad but not great. This sometimes happens with small
## samples. Let's try a Shapiro-Wilk test. ##

library(broom)

dog_mod |>
  resid() |>
  shapiro.test() |>
  tidy()

## Since p < 0.05, the data support the alternative hypothesis that the
## residuals do not follow a normal distribution ##

## However, the SW test is very sensitive to even small shifts 
## in normality. Considering that that QQ plot looks pretty good,
## we can probably proceed with the ANOVA ##

## Check for Constant Variance ##

## Residuals vs Fitted Values ##

dog_toys |>
  ggplot(aes(x=fitted(dog_mod),y=resid(dog_mod))) +
  geom_point() +
  geom_hline(yintercept=0,linetype='dashed',
             color='red') +
  geom_hline(yintercept=3,color='blue') +
  geom_hline(yintercept=-3,color='blue') +
  labs(x = "Predicted y's",
       y = "Residuals") +
  theme_classic()

## The residuals look pretty good. There doesn't seem to be any
## pattern in the residuals and the points seem to be randomly
## scattered around the 0 line. Again, with a small sample size
## it's a little tough to fully evaluate but not bad ##

## Check for Homogeneity of Variance using BP Test ##

library(lmtest)

bptest(dog_mod) |>
  tidy() |>
  select(statistic, p.value)

## Since p < 0.05, this indicates that the data support the alternative
## hypothesis that the variances are not constant. The BP Test, like
## the SW Test, is very sensitive to even small deviations from
## homogeneity of variance. Given the small sample size, we can
## probably proceed with the ANOVA ##

dog_mod |>
  tidy()

## Here we can see that the p-value for Formulation is less than 0.05,
## which indicates that the data support the alternative hypothesis
## that at least two of the formulations have different means. ##

## To determine which two, we can perform a Tukey HSD Test ##

TukeyHSD(dog_mod) |>
  tidy() |>
  select(contrast,estimate,adj.p.value)

## Focusing our attention on just the Formulation contrasts, we see
## that F3 is significantly different from F1 and F2. F1 and F2 are
## not significantly different from each other at the 0.05 level ##
