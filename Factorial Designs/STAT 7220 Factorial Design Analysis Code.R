## STAT 7220 Factorial Designs Code ##

library(tidyverse)
library(readxl)
library(rstatix)

## Read in the Data ##

helmet <- read_excel("Football Helmet.xlsx")

helmet |>
  glimpse()

## Perform Descriptive Analysis ##

helmet |>
  group_by(Shell) |>
  get_summary_stats(Response,type='mean_sd') |>
  select(Shell,mean,sd)

## So we can see that the polycarbonate helmets experience a greater 
## force than the new composite helmets. ##

helmet |>
  group_by(Padding) |>
  get_summary_stats(Response,type='mean_sd') |>
  select(Padding,mean,sd)

## Helmets with the gel padding experience a substantially lesser 
## force than the helmets with the foam padding. ##

helmet |>
  group_by(Shell,Padding) |>
  get_summary_stats(Response,type='mean_sd') |>
  select(Shell,Padding,mean,sd)

## Observing the interactions, we can see that the polycarbonate
## helmets generally experience the greatest force but the 
## gel padding does reduce the force experienced for both shell
## types. ##

## Let's look at a boxplot to visualize the data. ##

helmet |>
  ggplot(aes(x=Shell)) +
  geom_boxplot(aes(y=Response,fill=Padding)) +
  geom_point(data = helmet |>
               group_by(Shell) |>
               get_summary_stats(Response,type='mean_sd'),
             aes(x=Shell,y=mean),shape=4,size=5) +
  theme_classic()

## Obviously, the polycarbonate helmets experience a greater
## in particular when the foam padding is used. Contextually,
## we would hypothesize at the very least that the gel padding
## will reduce the force experienced. ##

## Let's visualize the interaction effect to better interpret
## its possible effect on the experiment ##

helmet |>
  group_by(Shell,Padding) |>
  get_summary_stats(Response,type='mean_sd') |>
  ggplot(aes(x=Shell,y=mean,group=Padding,color=Padding)) +
  geom_point() +
  geom_line() +
  theme_classic() 

## If we have no interaction effect, the lines would be parallel.
## However, we can see that the lines are not parallel, indicating
## that there is a non-zero interaction. Practically, we can see
## that the decrease in mean force experienced is greater for
## foam padding (moving from polycarbonate to new composite) than
## for gel padding. ##

## Perform ANOVA ##

mod <- aov(Response ~ Shell*Padding,data=helmet)

## Evaluate Normality Assumption ##

## QQ-Plot ##

helmet |>
  ggplot(aes(sample=resid(mod))) +
  geom_qq() +
  geom_qq_line() +
  theme_classic()

## Doesn't look too bad! Let's now check SW Test ##

mod |>
  resid() |>
  shapiro_test()

## Since p > 0.05, this indicates the data more strongly
## support the null hypothesis that the data is normally
## distributed. ##

## Check Constant Variance ##

helmet |>
  ggplot(aes(x=fitted(mod),y=rstandard(mod))) +
  geom_point() +
  geom_hline(yintercept=0,linetype='dashed',color='blue') +
  geom_hline(yintercept=3,linetype='dashed',color='red') +
  geom_hline(yintercept=-3,linetype='dashed',color='red') +
  theme_classic()

## The heights of the points are relatively constant, so we can
## assume that the data has constant variance. Let's check BP
## test ##

library(lmtest)

mod |>
  bptest()

## Since p > 0.05, this indicates that the data more strongly
## supports the null hypothesis that the data has constant variance. ##

## Now, we can move into the ANOVA table ##

library(broom)

mod |>
  tidy()

## First, examining our interaction effect, we can see that it is
## not statistically significant at the 0.05 level. This means, 
## even though we see some relationship between shell type and 
## padding as it relates to force experienced, the interaction
## is not statistically meaningful. ##

## Now, let's look at the main effects. Both are statistically
## significant at the 0.05 level. This means that the shell type
## and padding type have a statistically significant effect on
## the force experienced. ##

## Since we only have two levels for each of the treatment effects,
## we don't necessarily have to perform post-hoc tests. However,
## we can still perform them for the sake of completeness. ##

## Tukey's PostHoc Test ##

TukeyHSD(mod) |>
  tidy() |>
  filter(term %in% c("Shell","Padding")) |>
  select(term,contrast,estimate,adj.p.value)