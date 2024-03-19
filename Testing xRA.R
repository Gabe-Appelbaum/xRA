library(tidyverse)
library(corrplot)
library(gt)
#library(forestmangr)

xRA_23 <- read_csv("data/player xRA 2023.csv") %>%
  select(Name, 'xRA_23' = xRA_100)
xRA_22 <- read_csv("data/player xRA 2022.csv") %>%
  select(Name, 'xRA_22' = xRA_100)
xRA_21 <- read_csv("data/player xRA 2021.csv") %>%
  select(Name, 'xRA_21' = xRA_100)

stats_23 <- read_csv("data/xwOBA_wRC+_2023.csv") %>%
  rename('xwOBA_23' = xwOBA, 'wRC+_23' = 'wRC+', 'wOBA_23' = wOBA)
stats_22 <- read_csv("data/xwOBA_wRC+_2022.csv") %>%
  rename('xwOBA_22' = xwOBA, 'wRC+_22' = 'wRC+', 'wOBA_22' = wOBA)
stats_21 <- read_csv("data/xwOBA_wRC+_2021.csv") %>%
  rename('xwOBA_21' = xwOBA, 'wRC+_21' = 'wRC+', 'wOBA_21' = wOBA)

predict_23 <- {list(xRA_23, xRA_22, stats_23, stats_22) %>%
  reduce(inner_join) %>%
  select(-Name) %>%
  cor() %>%
  as.data.frame() %>%
  select(xRA_23, xwOBA_23, 'wRC+_23', wOBA_23) %>%
  slice(2,6:8) %>%
  rownames_to_column("predictor") %>%
  mutate(across(where(is.numeric), round, 2))}

predict_22 <- {list(xRA_22, xRA_21, stats_22, stats_21) %>%
  reduce(inner_join) %>%
  select(-Name) %>%
  cor() %>%
  as.data.frame() %>%
  select(xRA_22, xwOBA_22, 'wRC+_22', wOBA_22) %>%
  slice(2,6:8) %>%
  rownames_to_column("predictor") %>%
  mutate(across(where(is.numeric), round, 2))}

predict_23_r2 <- list(xRA_23, xRA_22, stats_23, stats_22) %>%
    reduce(inner_join) %>%
    select(-Name) 
summary(lm(xRA_23~xRA_22, predict_23_r2)) #.48
summary(lm(xRA_23~xwOBA_22, predict_23_r2)) #.4
summary(lm(xwOBA_23~xRA_22, predict_23_r2)) #.36


