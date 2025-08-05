## 2^2 Factorial Design Code ##

library(tidyverse)
library(readxl)
library(rstatix)

## Read in Data ##

wings <- read_excel("2^2 Wings Example.xlsx")

wings |>
  glimpse()

## Exploratory Analysis ##

## Main Effects ##

wings |>
  group_by(Push_Notifications) |>
  get_summary_stats(Sales, type = "mean_sd")

wings |>
  group_by(Loyalty_Program) |>
  get_summary_stats(Sales, type = "mean_sd")

## Here, it seems like enabled notifications
## and the enhanced loyalty program have a
## positive effect on sales. ##

## Interaction Effect ##

wings |>
  group_by(Push_Notifications,Loyalty_Program) |>
  get_summary_stats(Sales, type = "mean_sd")

## We can see the same phenomenon observed
## separately in the main effects here also
## in the interaction effect. However, the 
## within push_notifcation differences in
## mean are not the same (~50 for disabled vs ~20 for enabled).
## This may suggest a significant interaction effect ##

## Boxplots ##

wings |>
  ggplot(aes(x = Push_Notifications, y = Sales, fill = Loyalty_Program)) +
  geom_boxplot() +
  geom_point(data = wings |>
               group_by(Push_Notifications,Loyalty_Program) |>
               get_summary_stats(Sales,type='mean_sd'),
             aes(x=Push_Notifications,y=mean,group=Loyalty_Program),
             shape=4,size=5) +
  geom_line(data = wings |>
              group_by(Push_Notifications,Loyalty_Program) |>
              get_summary_stats(Sales,type='mean_sd'),
            aes(x=Push_Notifications,y=mean,color=Loyalty_Program,
                group=Loyalty_Program)) +
  labs(title = "Sales by Push Notifications and Loyalty Program",
       x = "Push Notifications",
       y = "Sales",
       fill = "Loyalty Program") +
  theme_classic()

## Here we can see the clear gap in sales between disabled and 
## enabled push notifications. We can also see a wider gap between
## the boxes within each level of push notifications (and non-parallel lines
## for the interaction plot). Practically, this may suggest that if 
## we decide to have the push notifications disabled, we should definitely
## use the enhanced loyalty program. The difference in means between
## the loyalty programs when push notifications are enabled isn't as great. ##

## ANOVA ##

wings_mod <- aov(Sales~Push_Notifications*Loyalty_Program,data=wings)

## Evaluate Assumptions ##

## Normality ##

wings |>
  ggplot(aes(sample=resid(wings_mod))) +
  geom_qq() +
  geom_qq_line() +
  theme_classic()

library(broom)

wings_mod |>
  resid() |>
  shapiro.test() |>
  tidy()

## We do have some deviance in the tails of the qq plot,
## but we have a p-value greater than 0.05 from the SW test. 
## In this case, because the sample size is so small, the
## three deviating points don't concern me so much. So
## normality seems reasonable ##

## Constant Variance ##

wings |>
  ggplot(aes(x=fitted(wings_mod),y=rstandard(wings_mod))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-3,3),linetype='dashed') +
  theme_classic()

library(lmtest)

wings_mod |>
  bptest() |>
  tidy()

## Again, because our sample size is so small, we may 
## be inclined to worry about what we see in the 
## scatterplot since the heights of the vertical lines
## aren't consistent. However, the BP test has a p-value
## greater than 0.05, indicating evidence for constant
## variance. This is another instance where the small
## sample may be overly influencing our interpretation
## of this assumption. In general, I'd say it's safe
## to proceed unless we saw lines of wildly differing
## heights. ##

## Assessing the Results ##

wings_mod |>
  tidy()

## The interaction effect is not significant but our 
## main effects are. This makes sense given what we 
## saw in the descriptive statistics and boxplots. ##

## Since we only have two levels in each of our main
## effects, there isn't a need to conduct a post-hoc
## test. ##

## However, we can calculate and interpret the effect 
## sizes for our main and interaction effects, partial eta^2 ##

wings_mod |>
  partial_eta_squared()

## Generally, small effects are around 0.01, medium effects
## are around 0.06, and large effects are around 0.14. ##

## In our case, 0.75 and 0.58, which again represent the proportion
## of variance in the response explained by our A and B effects, respectively,
## are quite large. 0.21 is also large using these guidelines but with a sample size
##  of 12, it was not large enough to be considered statistically meaningfully 
## different than 0. This shows how a small sample size can be just as impactful
## on inference as a large sample!! ##

