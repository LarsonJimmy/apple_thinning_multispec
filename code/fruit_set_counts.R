# Fruit Set Counts
# author: Jimmy Larson
# created: 6.11.20
# last edited: 8.24.20

## packages----
library(tidyverse)
library(ggpubr)
library(car)
library(hrbrthemes)
library(sjPlot)
library(RColorBrewer)
## load theme----
theme_jl <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black")
## load data----
counts <- read_csv("data/apple_multispec_fruit_counts_6_1_2020.csv")
## remove trees that were hand thinned accidentally----
counts %>%
  filter(tree %in% c("5", "6", "18", "20", "21", "24", "73", "114",
                                  "135", "138", "146", "147")) -> counts
## select to correct number of spurs per branch----
counts %>%
  mutate(lcsa.cm = (((limb.circ.cm/pi) /2)^2)*pi) %>%
  filter(case_when(spur.prune == 8 & lcsa.cm < .503 ~ spur <= 4,
                   spur.prune == 8 & .503 <= lcsa.cm & lcsa.cm < .785 ~ spur <= 6,
                   spur.prune == 8 & .785 <= lcsa.cm & lcsa.cm < 1.54 ~ spur <= 9,
                   spur.prune == 8 & 1.54 <= lcsa.cm  ~ spur <= 12,
                   spur.prune == 5 & lcsa.cm < .503 ~ spur <= 3,
                   spur.prune == 5 & .503 <= lcsa.cm & lcsa.cm < .785 ~ spur <= 4,
                   spur.prune == 5 & .785 <= lcsa.cm & lcsa.cm < 1.54 ~ spur <= 6,
                   spur.prune == 5 & 1.54 <= lcsa.cm  ~ spur <= 8,
                   spur.prune == 2 & lcsa.cm < .503 ~ spur <= 1,
                   spur.prune == 2 & .503 <= lcsa.cm & lcsa.cm < .785 ~ spur <= 2,
                   spur.prune == 2 & .785 <= lcsa.cm & lcsa.cm < 1.54 ~ spur <= 2,
                   spur.prune == 2 & 1.54 <= lcsa.cm  ~ spur <= 1,
                   spur.prune == "none"  ~ spur <= 14,
                   spur.prune == "none"  ~ spur <= 14,
                   spur.prune == "none"  ~ spur <= 14,
                   spur.prune == "none"  ~ spur <= 14)) -> counts
## get total branch counts----
counts %>%
  group_by(row, plot, tree, spur.prune, branch, lcsa.cm) %>%
  summarise(fruit.branch = sum(fruit)) -> counts.branch
## analyze and plot total branch numbers----
### anova test
counts.branch.aov <- aov(fruit.branch ~ spur.prune, data = counts.branch)
summary.aov(counts.branch.aov)
#### test normality
plot(counts.branch.aov, 2)
counts.branch.resid <- residuals(object = counts.branch.aov)
shapiro.test(x = counts.branch.resid) # did not solve normality problems
#### test homogeniety of variance
plot(counts.branch.aov, 1)
leveneTest(fruit.branch ~ as.factor(spur.prune), data = counts.branch)
### remove outliers (did not solve normality problems)
#counts.branch.outlier <- counts.branch[-c(13,48, 60),]
### anova test
#counts.branch.aov <- aov(fruit.branch ~ spur.prune, data = counts.branch.outlier)
#summary.aov(counts.branch.aov)
#### test normality
#plot(counts.branch.aov, 2)
#counts.branch.resid <- residuals(object = counts.branch.aov)
#shapiro.test(x = counts.branch.resid)
#### test homogeniety of variance
#plot(counts.branch.aov, 1)
#leveneTest(fruit.branch ~ as.factor(spur.prune), data = counts.branch)
### kruskal wallis test
kruskal.test(fruit.branch ~ spur.prune, data = counts.branch) # p < 0.05
### pairwise test
pairwise.wilcox.test(counts.branch$fruit.branch, counts.branch$spur.prune, p.adjust.method = "BH")
### plot data
counts.branch %>%
  mutate(prune.level = as.factor(spur.prune)) %>%
  group_by(prune.level) %>%
  summarise(max = max(fruit.branch)) %>%
  mutate(sep = c("b", "a", "a", "a"))-> max.fruit
counts.branch %>%
  mutate(prune.level = as.factor(spur.prune)) %>%
  group_by(prune.level) %>%
  ggplot() +
  geom_boxplot(aes(x = prune.level, y = fruit.branch, fill = prune.level)) +
  geom_text(data = max.fruit, aes(x = prune.level, y = max + 0.4, label = sep), vjust=0, color="black",
            position = position_dodge(.9), size= 5,parse = T) +
  scale_fill_brewer(palette = "Greens")+
  labs(x = ~"Spur Pruning Severity - Spurs/LCSA ("*cm^2*")",
       y = "Fruit per branch")+
  theme_bw()+
  theme(legend.position = "none")
## analyze and plot counts/lcsa----
### calculate fruit per lcsa
counts.branch %>%
  mutate(fruit.lcsa = fruit.branch / lcsa.cm) -> counts.lcsa
### build anova
counts.lcsa.aov <- aov(fruit.lcsa ~ spur.prune, data = counts.lcsa)
summary.aov(counts.lcsa.aov)
#### test normality
plot(counts.lcsa.aov, 2)
counts.lcsa.resid <- residuals(object = counts.lcsa.aov)
shapiro.test(x = counts.lcsa.resid)
#### test homogeniety of variance
plot(counts.lcsa.aov, 1)
leveneTest(fruit.lcsa ~ as.factor(spur.prune), data = counts.lcsa)
### Tukey Test
TukeyHSD(counts.lcsa.aov, which = "spur.prune")
### pairwise.t.test
pairwise.t.test(counts.lcsa$fruit.lcsa, counts.lcsa$spur.prune, p.adjust.method = "BH")
### plot data
counts.lcsa %>%
  mutate(prune.level = as.factor(spur.prune)) %>%
  group_by(prune.level) %>%
  summarise(max = max(fruit.lcsa)) %>%
  mutate(sep = c("b", "a", "ab", "a"))-> max.lcsa.fruit
counts.lcsa %>%
  mutate(prune.level = as.factor(spur.prune)) %>%
  group_by(prune.level) %>%
  ggplot() +
  geom_boxplot(aes(x = prune.level, y = fruit.lcsa, fill = prune.level)) +
  geom_text(data = max.lcsa.fruit, aes(x = prune.level, y = max + 0.4, label = sep), vjust=0, color="black",
            position = position_dodge(.9), size= 5,parse = T) +
  scale_fill_brewer(palette = "Greens")+
  labs(x = ~"Spur Pruning Severity - Spurs/LCSA ("*cm^2*")",
       y = ~"Fruit per lcsa ("*cm^2*")")+
  theme_jl+
  theme(legend.position = "none")
#ggsave("figs/fruit_lcsa.png")
## analyze and plot fruit/spur----
### build anova
spur.den.aov <- aov(counts$fruit ~ counts$spur.prune, data = counts)
summary.aov(spur.den.aov)
### test normality
plot(spur.den.aov, 2)
spur.den.resid <- residuals(object = spur.den.aov)
shapiro.test(x = spur.den.resid) # p<0.05 
#### test homogeniety of variance
plot(spur.den.aov, 1)
leveneTest(fruit ~ as.factor(spur.prune), data = counts)
### kruskal wallis test
kruskal.test(fruit ~ spur.prune, data = counts) # p < 0.05
### pairwise test
pairwise.wilcox.test(counts$fruit, counts$spur.prune, p.adjust.method = "BH")
## plot data
counts %>%
  mutate(prune.level = as.factor(spur.prune)) %>%
  group_by(prune.level) %>%
  summarise(avg.spur = mean(fruit)) %>%
  mutate(sep = c("ab", "b", "bc", "a")) -> avg.spur.den
counts %>%
  mutate(prune.level = as.factor(spur.prune)) %>%
  group_by(prune.level) %>%
  ggplot() +
  geom_boxplot(aes(x = prune.level, y = fruit, fill = prune.level)) +
  geom_text(data = avg.spur.den, aes(x = prune.level, y = avg.spur + 3.4, label = sep), vjust=0, color="black",
            position = position_dodge(.9), size= 5,parse = T) +
  scale_fill_brewer(palette = "Greens")+
  labs(x = ~"Spur Pruning Severity - Spurs/LCSA ("*cm^2*")",
       y = ~"Fruit per Spur")+
  theme_jl+
  theme(legend.position = "none")
#ggsave("figs/fruit_spur.png")
## regression----
counts$spur.prune <- factor(counts$spur.prune, levels = c("none", "8", "5", "2"))
counts.lcsa$spur.prune <- factor(counts.lcsa$spur.prune, levels = c("none", "8", "5", "2"))
### fruit per spur
spur.model <- lm(fruit ~ spur.prune, data = counts)
summary(spur.model)

plot_model(spur.model, type = "eff", show.data = TRUE, jitter = .5, colors = "Set2",
           axis.title = c("Spur Pruning Severity - Spurs/LCSA", "Fruit per Spur"),
           title = "Observed and Predicted Values for Fruit per Spur")
ggsave("figs/fruit_spur.png")
### fruit per lcsa
branch.model <- lm(fruit.lcsa ~ spur.prune, data = counts.lcsa)
summary(branch.model)

plot_model(branch.model, type = "eff", show.data = TRUE, jitter = .5, colors = "Set2",
           axis.title = c("Spur Pruning Severity - Spurs/LCSA", "Fruit per LCSA"),
           title = "Observed and Predicted Values for Fruit per LCSA")
ggsave("figs/fruit_lcsa.png")
