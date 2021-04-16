# Apple Thinning Multispec: Treatment Selection
# author: Jimmy Larson
# created: 4.15.21
# last edited: 4.15.21

## packages----
library(tidyverse)
library(randomizr)
## set seed----
set.seed(100)
## create treatments----
m <- as.data.frame(complete_ra(N= 10, num_arms = 2, conditions = c("ctrl", "met")))
m %>%
  rename(trt = `complete_ra(N = 10, num_arms = 2, conditions = c("ctrl", "met"))`) %>%
  mutate(plot = seq(1,10),
         flag = case_when(trt == "ctrl" ~ "pinkPolka",
                          trt == "met" ~ "yellowChecker")) -> map
write_csv(map, "trt_assignment_map.csv")
