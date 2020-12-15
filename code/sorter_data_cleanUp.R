# ASE: Fruit Sorter Data Clean Up
# author: Jimmy Larson
# created: 12/14/20
# last edited: 12/14/20

## load packages----
library(tidyverse)
library(ggpubr)
library(car)
library(lubridate)
library(RColorBrewer)
library(ggrepel)
library(hrbrthemes)
library(latex2exp)
## inputs----
path <- "data/sorterASE/JimmyASE_H1/" # enter path here
saveFile <- "sorter_data_H1.csv" # enter info to save file as
pattern <- "_\\w+_\\w+_\\w+_\\w+" # set pattern to extract treatment and rep info from file name
trtInfo <- c("blank","row", "rep", "ASE", "tree") # treamtment info to pull from file 
fileName <- list.files(path= path, pattern="*.csv", full.names=TRUE, recursive=FALSE)
nFiles <- length(fileName)
## for loop----
out.file <- ""
for (i in 1:nFiles) {
  ## load in data
  file <- read_delim(fileName[i], ";", escape_double = FALSE, trim_ws = TRUE)
  ## select rows----
  file <- file[grep(">|<|Juice", file$ClassName),]
  ## extract treatment info from filename----
  string <- fileName[i]
  trt <- regmatches(string, regexpr(pattern, string))
  ## add treatment info to file----
  file %>%
    mutate(treatment_rep = trt) %>%
    separate(treatment_rep, sep = "_", into = trtInfo) %>%
    select(row, rep, ASE, tree, ClassName, Weight, BlushPercentage, Diameter, AppleQuality, Length) -> file
  ## bind files together
  out.file <- rbind(out.file, file)
}
out.file <- out.file[-1,]
write.csv(out.file, saveFile)
