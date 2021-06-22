# Apple Thinning Multispec: Treatment Selection
# author: Jimmy Larson
# created: 6.22.21
# last edited: 6.22.21

## packages----
library(tidyverse)
library(randomizr)
## set seed----
set.seed(100)
## create treatments----
m <- as.data.frame(complete_ra(N= 15, num_arms = 3, conditions = c("ctrl", "low", "high")))
m %>%
  rename(trt = `complete_ra(N = 15, num_arms = 3, conditions = c("ctrl", "low", "high"))`) %>%
  mutate(plot = seq(1,15),
         flag = case_when(trt == "ctrl" ~ "pink",
                          trt == "low" ~ "yellowBlack",
                          trt == "high" ~ "redBlack"),
         tree.num = case_when(plot == 1 ~ "1-3",
                              plot == 2 ~ "6-8",
                              plot == 3 ~ "11-13",
                              plot == 4 ~ "15-17",
                              plot == 5 ~ "21-23",
                              plot == 6 ~ "30-32",
                              plot == 7 ~ "34-36",
                              plot == 8 ~ "37-39",
                              plot == 9 ~ "43-45",
                              plot == 10 ~ "47-79",
                              plot == 11 ~ "53-55",
                              plot == 12 ~ "67-69",
                              plot == 13 ~ "74-76",
                              plot == 14 ~ "82-84",
                              plot == 15 ~ "90-92",)) -> map
write_csv(map, "plot_map/2021/trt_assignment_map.csv")
