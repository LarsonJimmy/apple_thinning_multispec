# Tree selection: trunk measurements
# author: Jimmy Larson
# created: 3/5/20
# last edited: 3/5/20

## packages----
library(tidyverse)
## load data----
trunks.init <- read_csv("data/tree_selec_trunks_march_2020.csv")
## calculate cross sectional area----
trunks.init %>%
  mutate(r.cm = (circ.cm/(2*pi))) %>%
  mutate(tcsa.cm = ((r.cm^2)*pi))-> trunks.init
trunks.init$row <- as.factor(trunks.init$row)
## plot tcsa variability----
ggplot(trunks.init, aes(x = row, y = tcsa.cm))+
  geom_boxplot()+
  theme_bw()

ggplot(trunks, aes(x = tree, y = tcsa.cm, colour = row))+
  geom_point()+
  theme_bw()

## remove outliers ----
trunks.init %>%
  filter(tcsa.cm > quantile(tcsa.cm, 0.08),
         tcsa.cm < quantile(tcsa.cm, 0.92)) -> trunks
## write file for selected trees----
write_csv(trunks, "trunks_selected_final.csv")
