# Harvest Data Analysis
# author: Jimmy Larson
# created: 1.11.21
# last edited: 1.11.21

## packages----
library(tidyverse)
library(ggpubr)
library(car)
library(hrbrthemes)
## load theme----
theme_jl <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black")
## load data----
harvest_0 <- read_csv("data/sorterASE/sorter_data_H0.csv")
harvest_1 <- read_csv("data/sorterASE/sorter_data_H1.csv")
## merge harvest dates----
harvest_0 %>%
  mutate(harvest_date = "aug_12") -> harvest_0
harvest_1 %>%
  mutate(harvest_date = "aug_19") -> harvest_1
harvest <- rbind(harvest_0, harvest_1)
## replace ASE labels
harvest %>%
  mutate(spur.prune = case_when(ASE == "ASE8" ~ "8",
                                ASE == "ASE5" ~ "5",
                                ASE == "ASE2" ~ "2",
                                ASE == "ASEINF" ~ "none",
                                ASE == "TREE73" ~ "5")) -> harvest
## remove invalid rows----
harvest %>%
  filter(!Diameter == "__invalid__") %>%
  mutate(diameter = as.numeric(Diameter),
         weight = as.numeric(Weight),
         blush = as.numeric(BlushPercentage),
         length = as.numeric(Length)) %>%
  select(harvest_date, row, rep, spur.prune, tree, ClassName, weight, blush, diameter, AppleQuality, length) -> harvest
## exploratory graphs----
### fruit size
harvest %>%
  group_by(spur.prune) %>%
  ggplot(aes(x = spur.prune, y = diameter, fill = spur.prune))+
  geom_violin() +
  geom_boxplot(color = "black", width = 0.1) +
  scale_fill_brewer(palette = "Greens")+
  labs(x = ~"Spur Pruning Severity - Spurs/LCSA ("*cm^2*")",
       y = "Fruit Diameter")+
  theme_jl+
  theme(legend.position = "none")
### % blush
harvest %>%
  group_by(spur.prune) %>%
  ggplot(aes(x = spur.prune, y = blush, fill = spur.prune))+
  geom_violin() +
  geom_boxplot(color = "black", width = 0.1) +
  scale_fill_brewer(palette = "Greens")+
  labs(x = ~"Spur Pruning Severity - Spurs/LCSA ("*cm^2*")",
       y = "Red Fruit Color (% Blush)")+
  theme_jl+
  theme(legend.position = "none")
### weight
harvest %>%
  group_by(spur.prune) %>%
  ggplot(aes(x = spur.prune, y = weight, fill = spur.prune))+
  geom_violin() +
  geom_boxplot(color = "black", width = 0.1) +
  scale_fill_brewer(palette = "Greens")+
  labs(x = ~"Spur Pruning Severity - Spurs/LCSA ("*cm^2*")",
       y = "Fruit Weight")+
  theme_jl+
  theme(legend.position = "none")
