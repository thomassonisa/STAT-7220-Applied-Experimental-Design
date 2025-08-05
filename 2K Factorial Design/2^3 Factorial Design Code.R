## 2^3 Factorial Design Code ##

library(tidyverse)
library(readxl)
library(rstatix)

## Read Data from File ##

df <- read_xlsx("2^3 Chess Example.xlsx")

df |>
  glimpse()

## Exploratory Analysis ##

## Subscription ##

df |>
  group_by(Subscription) |>
  get_summary_stats(Purchases,type='mean_sd') |>
  select(-variable)

## So we can see that those with a premium subscription
## spend more per month, on average, then those with the basic
## subscription. ##

## App Offers ##

df |>
  group_by(App_Offers) |>
  get_summary_stats(Purchases,type='mean_sd') |>
  select(-variable)

## Those who received the in-app purchase offers
## spent substantially more per month, on average, 
## than those who did not receive the offers. ##

## Coaching ##

df |>
  group_by(Coaching) |>
  get_summary_stats(Purchases,type='mean_sd') |>
  select(-variable)

## Those who received coaching spent more per month, on average,
## than those who did not receive coaching. ##

## Subscription x App Offers ##

df |>
  group_by(Subscription,App_Offers) |>
  get_summary_stats(Purchases,type='mean_sd') |>
  select(-variable)

## Within the Basic subscription, the difference in 
## means is around 20. Within the Premium subscription,
## it's about the same. This would suggest lack of a 
## significant two-way interaction effect between
## Subscription and App Offers. ##

## Subscription x Coaching ##

df |>
  group_by(Subscription,Coaching) |>
  get_summary_stats(Purchases,type='mean_sd') |>
  select(-variable)

## Within the Basic subscription, the difference in
## means is again around 20. Within the Premium subscription,
## it's about the same. This would suggest lack of a
## significant two-way interaction effect between
## Subscription and Coaching. ##

## App Offers x Coaching ##

df |>
  group_by(App_Offers,Coaching) |>
  get_summary_stats(Purchases,type='mean_sd') |>
  select(-variable)

## Within the No App Offers group, the difference in
## means is around 15. Within the Yes App Offers group,
## it's over 20. This may suggest a significant
## two-way interaction effect between App Offers and
## Coaching. ##

## Subscription x App Offers x Coaching ##

df |>
  group_by(Subscription,App_Offers,Coaching) |>
  get_summary_stats(Purchases,type='mean_sd') |>
  select(-variable)

## Within the Basic subscription and No App Offers group,
## the difference in means is around 15. Within the Basic
## subscription and Yes App Offers group, it's close, around 13. 
## Within the Premium subscription and No App Offers group,
## it's around 14. Within the Premium subscription and Yes
## App Offers group, it's around 20. This may suggest a
## small three-way interaction but it's hard to say. ##

## Boxplot/Interaction Effect Combo Plot ##

## Here, we are going to make use of a cool package 
## called 'patchwork' to combine multiple plots into
## one figure ##

## Let's first build a combo plot for the "No" Coaching group ##

p1 <- df |>
  filter(Coaching == "No") |>
  ggplot(aes(x = Subscription, y = Purchases, fill = App_Offers)) +
  geom_boxplot() +
  geom_point(data = df |>
               filter(Coaching == "No") |>
               group_by(Subscription,App_Offers) |>
               get_summary_stats(Purchases,type='mean_sd'),
             aes(x=Subscription,y=mean,group=App_Offers),
             shape=4,size=5) +
  geom_line(data = df |>
              filter(Coaching == "No") |>
              group_by(Subscription,App_Offers) |>
              get_summary_stats(Purchases,type='mean_sd'),
            aes(x=Subscription,y=mean,group=App_Offers,
                color=App_Offers)) +
  labs(title = "Did Not Receive Coaching",
       x = "Subscription",
       y = "Purchases",
       fill = "In-App Offers") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(limits=c(0,75)) +
  guides(color='none')

p2 <- df |>
  filter(Coaching == "Yes") |>
  ggplot(aes(x = Subscription, y = Purchases, fill = App_Offers)) +
  geom_boxplot() +
  geom_point(data = df |>
               filter(Coaching == "Yes") |>
               group_by(Subscription,App_Offers) |>
               get_summary_stats(Purchases,type='mean_sd'),
             aes(x=Subscription,y=mean,group=App_Offers),
             shape=4,size=5) +
  geom_line(data = df |>
              filter(Coaching == "Yes") |>
              group_by(Subscription,App_Offers) |>
              get_summary_stats(Purchases,type='mean_sd'),
            aes(x=Subscription,y=mean,group=App_Offers,
                color=App_Offers)) +
  labs(title = "Received Coaching",
       x = "Subscription",
       y = "Purchases",
       fill = "In-App Offers") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(limits=c(0,75)) +
  guides(color='none')

library(patchwork)

p1 + p2 +
  plot_layout(guides='collect')

## First, we can clearly see a jump in monthly purchases
## between basic and premium subscriptions. Second, we can
## that within each subscription type, those who received
## the in-app offers spent more per month than those who
## did not. Third, we can see that the slopes of the interaction
## lines within each coaching group are approximately the same.
## This suggests no significant interaction between subscription
## and in-app offers. Now, for those who received coaching, we
## can see an increase in monthly purchases compared to those 
## who did not receive coaching. This suggests coaching may have
## a significant effect on monthly purchases. Finally, comparing
## the slopes of the interaction lines between the coaching groups
## are slightly different. This may suggest a significant three-way
## interaction effect between subscription, in-app offers, and coaching. ##

## Let's build our ANOVA model ##

mod <- aov(Purchases ~ Subscription * App_Offers * Coaching, data = df)

## Check Normality Assumption ##

## Q-Q Plot ##

df |>
  ggplot(aes(sample=resid(mod))) +
  geom_qq() +
  geom_qq_line() +
  theme_classic()

## Looks pretty good! How about S-W test? ##

library(broom)

mod |>
  resid() |>
  shapiro.test() |>
  tidy()

## Since p > 0.05, this suggests our residuals
## reasonably follow a normal distribution. ##

## Constant Variance ##

## Resiuduals Plot ##

df |>
  ggplot(aes(x=fitted(mod),y=rstandard(mod))) +
  geom_point() +
  geom_hline(yintercept=0,color='blue') +
  geom_hline(yintercept=3,color='red') +
  geom_hline(yintercept=-3,color='red') +
  theme_classic()

## Not bad! The points generally randomly vary about
## the centerline of 0. No points are above or below
## +/- 3. This suggests our residuals have constant
## variance. ##

## B-P Test ##

library(lmtest)

mod |>
  bptest() |>
  tidy()

## Since p < 0.05, this suggests our residuals do not 
## have a constant variance. However, since our sample
## size is so small, the test may be overly sensitive.
## We can proceed, but just be mindful of the result in
## the conclusions. ##

## ANOVA Table ##

mod |>
  tidy()

## We start with our interaction terms. The three-way
## interaction term is not significant. The two-way
## interaction terms are also not significant. So now
## we can focus on the main effects. The main effect
## of Subscription is significant. The main effect of
## App Offers is significant. The main effect of Coaching
## is also significant. ##

## This result is not surprising based on what we saw in the 
## descriptive analysis. ##

## Given that we only have two levels for each of our main
## effects, there isn't a need to conduct post-hoc tests. ##

## However, we can tabulate our partial eta squared values for
## our main effects :) ##

mod |>
  partial_eta_squared() 

## Here, we can see that the main effects all have large
## effect sizes. The two-way interaction between app_offers
## and coaching also has a large effect, but not nearly
## as large as the main effects. The other two-way interactions
## and our three-way interaction are all very small. ##

## Remember, the interpretation of partial eta squared is
## the proportion of variance in the dependent variable (Purchases)
## that can be attributed to the independent variable (Subscription,
## App Offers, Coaching, etc.) while controlling for the other
## independent variables. ##


