# Tree selection: ltr and MAD determination
# author: Jimmy Larson
# created: 3/5/20
# last edited: 3/12/20

## packages----
library(tidyverse)
library(broom)
library(RColorBrewer)
## read in data ----
trees <- read_csv("data/trees_selected_final.csv")
limbs <- read_csv("data/ltr_limb_measurements_march_2020.csv")
map <- read_csv("data/apple_multispec_map_final.csv")
## assign tree class by trunk----
trees$tcsa.cm <- format(round(trees$tcsa.cm, 2))
trees %>%
  summarise(max = max(tcsa.cm)) -> trees.tcsa.max
trees %>%
  summarise(min = min(tcsa.cm)) -> trees.tcsa.min
trees.tcsa.dif <- trees.tcsa.max$max - trees.tcsa.min$min
ltr.class <- seq(trees.tcsa.min$min, trees.tcsa.max$max, length.out = 6)
ltr.class <- as.data.frame(ltr.class)
ltr.class %>%
  mutate(class.id = seq(1,6)) -> ltr.class
trees %>%
  mutate(ltr.class = case_when(tcsa.cm >= 11.65 & tcsa.cm < 13.706 ~ "1",
                               tcsa.cm >= 13.706 & tcsa.cm < 15.762 ~ "2",
                               tcsa.cm >= 15.762 & tcsa.cm < 17.818 ~ "3",
                               tcsa.cm >= 17.818 & tcsa.cm < 19.874 ~ "4",
                               tcsa.cm >= 19.874 & tcsa.cm <=21.930  ~"5")) -> trees

## assign treatments to trees----
trees %>%
  mutate(rep = rep(1:5, times = 6, each = 3)) %>%
  select(row, rep, tree, tcsa.cm, ltr.class) -> trees
trees$tcsa.cm <- as.numeric(trees$tcsa.cm)
### write_csv(trees, "data/thinning_multispec_map.csv")
## calculate lcsa, arrange limbs low to high, calculate cumulative sum----
desir.ltr <-  1.25
desir.ltr.df <- as_data_frame(c(1.25))
desir.ltr.df %>%
  rename(ltr = value) -> desir.ltr.df
limbs$ltr.class <- as.character(limbs$ltr.class)
limbs %>%
  mutate(limb.diam = limb.diam.mm*0.1) %>%
  mutate(lcsa = (((limb.diam/2)^2)*pi)) %>%
  group_by(rep, ltr.class) %>%
  arrange(lcsa, .by_group = TRUE) %>%
  group_by(rep, ltr.class) %>%
  mutate(lcsa.sum = cumsum(lcsa)) %>%
  mutate(tcsa.cm = case_when(ltr.class == 1 & rep == 1 ~ 12.43,
                             ltr.class == 1 & rep == 2 ~ 12.84,
                             ltr.class == 1 & rep == 3 ~ 13.66,
                             ltr.class == 2 & rep == 1 ~ 13.87,
                             ltr.class == 2 & rep == 2 ~ 13.87,
                             ltr.class == 2 & rep == 3 ~ 15.6,
                             ltr.class == 3 & rep == 1 ~ 17.43,
                             ltr.class == 3 & rep == 2 ~ 15.82,
                             ltr.class == 3 & rep == 3 ~ 16.73,
                             ltr.class == 4 & rep == 1 ~ 19.62,
                             ltr.class == 4 & rep == 2 ~ 17.9,
                             ltr.class == 4 & rep == 3 ~ 19.62,
                             ltr.class == 5 & rep == 1 ~ 20.12,
                             ltr.class == 5 & rep == 2 ~ 21.93,
                             ltr.class == 5 & rep == 3 ~ 20.88)) %>%
  mutate(ltr = lcsa.sum / tcsa.cm) -> limbs.ltr 
limbs.ltr %>%
  filter(ltr <= desir.ltr) -> limbs.max.diam
limbs.ltr %>%
  group_by(ltr.class, rep, tcsa.cm) %>%
  summarise(max(limb.diam.mm)) %>%
  rename(MAD = `max(limb.diam.mm)`) -> limbs_MAD 
limbs.ltr %>%
  group_by(ltr.class, rep, tcsa.cm) %>%
  summarise(max(lcsa.sum)) %>%
  rename(lcsa = `max(lcsa.sum)`) -> limbs.lcsa
limbs.max.diam %>%
  group_by(ltr.class, rep, tcsa.cm) %>%
  summarise(max(limb.diam.mm)) %>%
  rename(MAD = `max(limb.diam.mm)`) -> MAD_class 
limbs.max.diam %>%
  group_by(ltr.class, rep, tcsa.cm) %>%
  summarise(max(lcsa.sum)) %>%
  rename(lcsa = `max(lcsa.sum)`) -> lcsa_class
limbs_MAD <- full_join(limbs_MAD, limbs.lcsa, by = c("ltr.class", "rep", "tcsa.cm" ))
MAD_class <- full_join(MAD_class, lcsa_class, by = c("ltr.class", "rep", "tcsa.cm" ))
limbs_MAD %>%
  mutate(ltr = lcsa / tcsa.cm) -> limbs_MAD
MAD_class %>%
  mutate(ltr = lcsa / tcsa.cm) -> MAD_class
### calculate number of limbs to remove
limbs.ltr %>%
  filter(ltr > desir.ltr) %>%
  group_by(ltr.class, rep, tcsa.cm) %>%
  count() -> limb.removal.n

MAD_class <- full_join(MAD_class, limb.removal.n, by = c("ltr.class", "rep", "tcsa.cm"))
## regress plot max allowable limb diameter against TCSA----
###MAD_class_2 <- MAD_class[-7,] ### remove outlier class 3 rep 1 and class 4 
### lcsa regression----
lcsa_regression <- lm(lcsa ~ tcsa.cm, data = limbs_MAD)
glance(lcsa_regression)
### remove outliers for class 3 and 5
limbs_MAD.2 <- limbs_MAD[-c(8,9,13),]
lcsa_regression.2 <- lm(lcsa ~ tcsa.cm, data = limbs_MAD.2)
glance(lcsa_regression.2)
### remove outliers for class 3
limbs_MAD.3 <- limbs_MAD[-c(8,9),]
lcsa_regression.3 <- lm(lcsa ~ tcsa.cm, data = limbs_MAD.3)
glance(lcsa_regression.3)
limbs_MAD %>%
  ggplot()+
  geom_point(aes(x = tcsa.cm, y = ltr, color = ltr.class))+
  labs(x = "TCSA (cm)",
       y = "LTR",
       color = "Trunk Size Class")+
  theme_bw()

### MAD regression----
ggplot(MAD_class, aes(x = tcsa.cm, y = MAD))+
  geom_point(aes(x = tcsa.cm, y = MAD, color = ltr.class))+
  geom_smooth(method = "lm")+
  labs(x = "TCSA (cm)",
     y = "Maximum Allowable Limb Diameter (mm)",
     color = "Trunk Size Class")+
  theme_bw()

MAD_class$rep <- as.character(MAD_class$rep)
MAD.regression <- lm(MAD ~ tcsa.cm, data = MAD_class)
tidy(MAD.regression)
glance(MAD.regression)

augment(MAD.regression, data = MAD_class) %>%
  ggplot()+
  geom_point(aes(x = .fitted, y = MAD, color = ltr.class))+
  geom_abline(intercept = 0, slope = 1, color = "red")+
  labs(x = "MAD (mm) - modeled",
       y = "MAD (mm) - observed",
       color = "Trunk Size Class")+
  annotate(geom = "text", x = 12, y = 16, label = "R^{2}: 0.391", parse = TRUE)+
  theme_bw()

augment(MAD.regression, data = MAD_class) %>%
  ggplot()+
  geom_point(aes(x = MAD, y = .resid, color = ltr.class))+
  geom_hline(yintercept = 0)+
  labs(x = "MAD (mm)",
       y = "Residuals",
       color = "Trunk Size Class")+
  theme_bw()

### remove outliers for class 3
MAD_class.2 <- MAD_class[-c(7),]
MAD.regression.2 <- lm(MAD ~ tcsa.cm, data = MAD_class.2)
glance(MAD.regression.2)

augment(MAD.regression.2, data = MAD_class.2) %>%
  ggplot()+
  geom_point(aes(x = .fitted, y = MAD, color = ltr.class))+
  geom_abline(intercept = 0, slope = 1, color = "red")+
  labs(x = "MAD (mm) - modeled",
       y = "MAD (mm) - observed",
       color = "Trunk Size Class")+
  annotate(geom = "text", x = 12, y = 16, label = "R^{2}: 0.474", parse = TRUE)+
  theme_bw()
### remove outliers for class 3
MAD_class.3 <- MAD_class[-c(2,7,10),]
MAD.regression.3 <- lm(MAD ~ tcsa.cm + ltr, data = MAD_class.3)
glance(MAD.regression.3)

### regression for number of limbs to remove
removal.regression <- lm(n ~ tcsa.cm, data = limb.removal.n)
glance(removal.regression)

ggplot(MAD_class, aes(x = MAD, y = n, color = ltr.class, label = rep))+
  geom_point()+
  geom_text(hjust = 0, nudge_x = 0.08,
            show.legend = FALSE)+
  labs(x = "Maximum Allowable Limb Diameter (mm)",
       y= "Number of Limbs to Remove",
       color = "Trunk Size Class")+
  theme_bw()
## limb diameter vs. ltr plot----
limbs.ltr %>%
  rename(trunk.class = ltr.class) -> limbs.ltr
ggplot(limbs.ltr, aes(x = ltr, y = limb.diam.mm, color = as.character(rep)))+
  geom_point()+
  facet_wrap(~ trunk.class, labeller = "label_both")+
  geom_smooth(method = "lm")+
  scale_fill_brewer(palette = "Set2")+
  geom_vline(xintercept = 1.25, color = "red")+
  #annotate(geom = "text", x = 5, y = 1.4, label = "ltr = 1.25")+
  labs(x = "Limb Diameter (mm)",
       y = "LTR",
       color = "Rep")+
  theme_bw()
## Limb diam to ltr regression
limbs.ltr %>%
  filter(trunk.class == "1") -> class.1.ltr
class.1.ltr.regress <- lm(limb.diam.mm ~ ltr, data =  class.1.ltr)
class.1.MAD.predict <- predict(class.1.ltr.regress, newdata = desir.ltr.df)
glance(class.1.ltr.regress)
limbs.ltr %>%
  filter(trunk.class == "2") -> class.2.ltr
class.2.ltr.regress <- lm(limb.diam.mm ~ ltr, data =  class.2.ltr)
class.2.MAD.predict <-predict(class.2.ltr.regress, newdata = desir.ltr.df)
glance(class.2.ltr.regress)
limbs.ltr %>%
  filter(trunk.class == "3") -> class.3.ltr
class.3.ltr.regress <- lm(limb.diam.mm ~ ltr, data =  class.3.ltr)
class.3.MAD.predict <-predict(class.3.ltr.regress, newdata = desir.ltr.df)
glance(class.3.ltr.regress)
limbs.ltr %>%
  filter(trunk.class == "4") -> class.4.ltr
class.4.ltr.regress <- lm(limb.diam.mm ~ ltr, data =  class.4.ltr)
class.4.MAD.predict <-predict(class.4.ltr.regress, newdata = desir.ltr.df)
glance(class.4.ltr.regress)
limbs.ltr %>%
  filter(trunk.class == "5") -> class.5.ltr
class.5.ltr.regress <- lm(limb.diam.mm ~ ltr, data =  class.5.ltr)
class.5.MAD.predict <-predict(class.5.ltr.regress, newdata = desir.ltr.df)
glance(class.5.ltr.regress)

### write out MAD values
MAD_values <- as_data_frame(c(class.1.MAD.predict, class.2.MAD.predict, class.3.MAD.predict,
                              class.4.MAD.predict, class.5.MAD.predict))
MAD_values %>%
  rename(MAD = value) %>%
  mutate(trunk.class = c(seq(1:5))) -> MAD_values
map %>%
  rename(trunk.class = ltr.class) -> map
map <- full_join(map, MAD_values, by = "trunk.class")
## write out excel files ----
write_csv(limbs.ltr, "lcsa_full_dataset.csv")
write_csv(MAD_class, "MAD_full_dataset.csv")
write_csv(MAD_class.3, "MAD_outliers_removed.csv")
write_csv(map, "apple_multispec_map_MAD.csv")
