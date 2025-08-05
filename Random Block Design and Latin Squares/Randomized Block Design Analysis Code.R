## STAT 7220 - Randomized Block Design Analysis Code ##

library(tidyverse)
library(readxl)

## Read in File ##

exercise <- read_excel("Exercise and Diabetes.xlsx")

## Explore data structure ##

exercise |>
  glimpse()

## Descriptive Analysis - Means and Standard Deviations ##

library(rstatix)

exercise |>
  group_by(`BMI Category`,`Exercise Treatment`) |>
  get_summary_stats(HbA1c, type = "mean_sd") |>
  arrange(`Exercise Treatment`)

## As we can see here, the mean HbA1c tends to increase within each BMI 
## category. Additionally, within the exercise treatments, it looks like
## HbA1c tends to decrease as we move from aerobic to resistance to combined.
## This may potentially indicate that the combined treatment is the most
## effective at reducing HbA1c levels, controlling for BMI category. ##

## Generate a Boxplot ##

## Note, forcing the levels of the BMI Category to be in a specific order
## using the factor function ##

exercise |>
  mutate(`BMI Category` = factor(`BMI Category`,levels=c("Underweight",
                                                         "Normal Weight",
                                                         "Overweight",
                                                         "Obese"))) |>
  ggplot(aes(x=`Exercise Treatment`,y=HbA1c,fill=`BMI Category`)) +
  geom_boxplot() +
  labs(title = "Effect of Exercise Intervention on HbA1c Percentage",
       subtitle = "by BMI Category") +
  theme_classic()

## As we can see, the boxplot shows that the combined and resistance
## treatments tend to produce lower HbA1c levels than the aerobic treatment.
## Additionally, it appears that the underweight and normal weight categories
## had lower HBA1c levels comparing the combined and resistance groups. ##

## Perform a Blocked ANOVA Model (i.e., two-way ANOVA) ##

## Fit Blocked ANOVA Model ##

block_mod <- aov(HbA1c ~ `Exercise Treatment` + `BMI Category`,
                 data=exercise)

## Extract ANOVA Table ##

library(broom)

block_mod |>
  tidy()

## As shown, the p-value for the Exercise Treatment is significant at the 0.05
## level (5.56e-13 < 0.05). This indicates the data more strongly support the
## alternative hypothesis that may be a difference in mean HbA1c levels between
## the exercise treatments controlling for BMI category. ##

## Let's evaluate the assumptions of the ANOVA model ##

## Normality (as before) ##

exercise |>
  ggplot(aes(sample=resid(block_mod))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q Plot of Residuals") +
  theme_classic()

## With the exception of a couple of points deviating
## from the line, it looks decent! Let's see what Shapiro-Wilk
## says: ##

block_mod |>
  resid() |>
  shapiro.test() |>
  tidy()

## Since p > 0.05, this indicates that the data more strongly
## support the null hypothesis that the residuals are normally
## distributed. ##

## Moving onto constant variance ##

## Build Scatterplot Evaluating Residuals vs 
## Predicted Values ##

exercise |>
  ggplot(aes(x=fitted(block_mod),y=resid(block_mod))) +
  geom_point() +
  geom_hline(yintercept=0,linetype='dashed',
             color='red') +
  geom_hline(yintercept=3,color='blue') +
  geom_hline(yintercept=-3,color='blue') +
  labs(x = "Predicted y's",
       y = "Residuals") +
  theme_classic()

## As we can see, the residuals appear to be randomly scattered
## around the 0 line, which is a good sign. We also don't see
## wildly varying residuals as we move from left to right. So
## I would say the constant variance assumption is reasonably met ##

## Let's check with Breush-Pagan Test ##

library(lmtest)

bptest(block_mod) |>
  tidy() |>
  select(statistic,p.value)

## Since p > 0.05, we have evidence in favor of the null hypothesis
## which states that the residuals have constant variance. ##

## Now, since our p-value for the Exercise Treatment is significant,
## let's perform our Tukey's HSD post-hoc test: ##

## Perform Tukey's HSD Post-Hoc Test ##

exercise |>
  rename(Exercise_Treatment = `Exercise Treatment`,
         BMI_Category = `BMI Category`) |>
  tukey_hsd(HbA1c ~ Exercise_Treatment+BMI_Category) |>
  filter(term == "Exercise_Treatment") |>
  select(group1,group2,estimate,p.adj)
