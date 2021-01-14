# Harvest Data Analysis
# author: Jimmy Larson
# created: 1.11.21
# last edited: 1.11.21

## packages----
library(tidyverse)
library(ggpubr)
library(car)
library(hrbrthemes)
#install.packages("sjPlot")
library(sjPlot)
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
                                ASE == "ASEINF" ~ "None")) -> harvest
## remove invalid rows----
harvest %>%
  filter(!Diameter == "__invalid__") %>%
  mutate(diameter = as.numeric(Diameter),
         weight = as.numeric(Weight),
         blush = as.numeric(BlushPercentage),
         length = as.numeric(Length)) %>%
  select(harvest_date, row, rep, spur.prune, tree, ClassName, weight, blush, diameter, AppleQuality, length) -> harvest
## calculate yield per tree
harvest %>%
  group_by(row, rep, spur.prune,tree) %>%
  summarise(yield= (sum(weight)/1000)) -> yield
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
## regression----
harvest$spur.prune <- factor(harvest$spur.prune, levels = c("None", "8", "5", "2"))
yield$spur.prune <- factor(yield$spur.prune, levels = c("None", "8", "5", "2"))
### yield 
yield.model <- lm(yield ~ spur.prune, data = yield)
summary(yield.model)

plot_model(yield.model, show.data = TRUE, jitter = .5, colors = "Set1",
           axis.title = c( "Yield/tree (kg)"),
           title = "Yield per Tree",
           axis.labels = c("8 buds/LCSA", "5 buds/LCSA", "2 buds/LCSA"))
#ggsave("figs/yield.png")
### fruit size
size.model <- lm(diameter ~ spur.prune, data = harvest)
summary(size.model)

plot_model(size.model, type = "eff", show.data = TRUE, jitter = .5, colors = "Set2",
           axis.title = c("Spur Pruning Severity - Spurs/LCSA", "Fruit Diameter (mm)"),
           title = "Observed and Predicted Values for Fruit Diameter")
#ggsave("figs/size.png")
### blush
blush.model <- lm(blush ~ spur.prune, data = harvest)
summary(blush.model)

plot_model(blush.model, type = "eff", show.data = TRUE, jitter = .5, colors = "Set1",
           axis.title = c("Spur Pruning Severity - Spurs/LCSA", "Fruit Blush (%)"),
           title = "Observed and Predicted Values for Fruit Blush")
#ggsave("figs/blush.png")

