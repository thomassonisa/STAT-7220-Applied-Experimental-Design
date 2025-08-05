## STAT 7220 Factorial Experimental Designs ##

library(tidyverse)
library(readxl)
library(rstatix)

## Generate Data ##

outer_helmet_materials <- c("Polycarbonate","New Composite")
inner_helmet_materials <- c("Foam","Gel")

n_replicates <- 10

factor_combinations <- expand.grid(Shell=outer_helmet_materials, Padding=inner_helmet_materials,
                                   Replicate = 1:n_replicates) |>
  select(-Replicate) |>
  arrange(Shell,Padding)

df <- factor_combinations |>
  mutate(Shell_Effect = ifelse(Shell == "Polycarbonate", 5, -5),
         Padding_Effect = ifelse(Padding == "Foam", 5, -5),
         Interaction_Effect = ifelse(Shell == "Polycarbonate" & Padding == "Foam", 10, 
                                     ifelse(Shell == "Polycarbonate" & Padding == "Gel", 0,
                                            ifelse(Shell == "New Composite" & Padding == "Foam", -5, -10)))) |>
           
  mutate(Response = abs(
    round(
      80 + Shell_Effect + Padding_Effect + Interaction_Effect + rnorm(1,mean=0,sd=10),
      2)
  )
  )

## Generate Random Noise ##

noise <- rnorm(nrow(df), mean = 0, sd = 10)

df <- df |>
  mutate(Response = round(Response + noise,2)) |>
  select(-Shell_Effect, -Padding_Effect, -Interaction_Effect)

## Build a function to generate two-way factorial design data ##

ad_format_levels <- c("Video","Image")
ad_content_levels <- c("Funny","Informational")

n_replicates <- 25

intercept <- 20

factor_combinations <- expand.grid(Format=ad_format_levels, Content=ad_content_levels,
                                   Replicate = 1:n_replicates) |>
  select(-Replicate) |>
  arrange(Format,Content) |>
  mutate(Format_Effect = ifelse(Format == "Video", 5, -5),
         Content_Effect = ifelse(Content == "Funny", 5, -5),
         Interaction_Effect = ifelse(Format == "Video" & Content == "Funny",10, 
                                     ifelse(Format == "Video" & Content == "Informational", 0,
                                            ifelse(Format == "Image" & Content == "Informational", -10, -5)))) |>
  
  mutate(Time = abs(
    round(
      intercept + Format_Effect + Content_Effect + Interaction_Effect + rnorm(1,mean=0,sd=5),
      2)
  ))

## Generate Random Noise ##

noise <- rnorm(nrow(factor_combinations), mean = 0, sd = 5)

factor_combinations <- factor_combinations |>
  mutate(Time = abs(round(Time + noise,2))) |>
  select(-Format_Effect, -Content_Effect, -Interaction_Effect)

## Save to Excel File ##

writexl::write_xlsx(factor_combinations,"Instagram Data.xlsx")

mod2 <- aov(Time~Format*Content,data=factor_combinations)

## Evaluate Normality ##

shapiro.test(residuals(mod2))

## Evaluate Homogeneity of Variance ##

lmtest::bptest(mod2)

## Generate Interaction Plot ##

interaction.plot(factor_combinations$Format,factor_combinations$Content,factor_combinations$Time)

summary(mod2)
