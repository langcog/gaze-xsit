rm(list = ls())
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)

d <- read_csv("processed_data/processed.csv")

### merge in subject info
subinfo <- read_csv("info/subinfo.csv")

d %<>% 
  mutate(subid = gsub(".txt", "", subid)) %>%
  left_join(subinfo)
  
### analyses

# raw
qplot(x, data=d) + 
  xlim(c(0,1980))

qplot(x, facets = ~ stimulus, 
      data=d) + 
  xlim(c(0,1980))

# get AOIs
# note: actually get these numbers from the stimuli themselves
d %<>% mutate(aoi = factor(ifelse(x > 250 & x < 750, "left", 
                           ifelse(x > 1250 & x < 1750, "right", 
                                  ifelse(x > 750 & x < 1250, "face", NA)))))

qplot(aoi, fill = aoi, 
      data = d) 

qplot(aoi, fill = aoi, facets = condition~., 
      data = d) 