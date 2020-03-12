# Equilifruit disk adjustments
# author: Jimmy Larson
# created: 3/12/20
# last edited: 3/12/20

## packages----
library(tidyverse)

## form dataset
equilidisk <- as_data_frame(c(seq(8, 28, by = 2)))

equilidisk %>%
  rename(diam.mm = value) %>%
  mutate(r.cm = (diam.mm*0.1)/2,
         lcsa.cm = (r.cm^2)*pi,
         F2 = lcsa.cm*2,
         F5 = lcsa.cm*5,
         F8 = lcsa.cm*8)
