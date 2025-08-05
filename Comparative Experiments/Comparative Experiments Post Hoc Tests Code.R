# STAT 7220 - Post Hoc Test Code ##

library(tidyverse)
library(readxl)

## Read in Egg Data ##

egg_data <- read_excel("Egg Rating.xlsx")

## Perform Descriptive Analysis ##

library(rstatix)

egg_data |>
  group_by(Technique) |>
  get_summary_stats(Rating,type="mean_sd") |>
  select(Technique,mean,sd)

## So bacon grease seems to be the most preferred method
## followed by olive oil and butter. Based on the standard
## deviations, it seems like our ultimate conclusion will
## be that we should go with bacon grease. ##

## Generate a Boxplot ##

egg_data |>
  ggplot(aes(x=Technique,y=Rating)) +
  geom_boxplot() +
  labs(title = "Fried Egg Cooking Technique Ratings") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.50))

## As we can see, Bacon Grease doesn't have much overlap
## with the other two groups which gives us a similar result
## to what our means/sds were telling us. Butter and 
## Olive Oil have a lot of overlap which may indicate
## that they are not significantly different. ##

## Let's build the ANOVA model ##

egg_mod <- aov(Rating ~ Technique, data=egg_data)

## Evaluate the Result ##

library(broom)

egg_mod |>
  tidy()

## So we can see that the p-value is less than 0.05 which
## indicates that there is a significant difference between
## at least two of the groups. But it doesn't tell us
## which groups are different. ##

## To make this determination, we must perform a post hoc test ##

## Perform Tukey's HSD ##

TukeyHSD(egg_mod) |>
  tidy() |>
  select(contrast,estimate,adj.p.value)

## So we can see that we have two significant differences:
## Bacon Grease vs. Butter and Bacon Grease vs. Olive Oil. ##
## This confirms our suspicions that Bacon Grease is the
## best method for frying eggs according to consumer tastes ##