###########################################
## PREPROCESSING SCRIPT FOR GAZE-XSIT
## read in data files and consolidate them
###########################################

## PRELIMINARIES
rm(list = ls())
library(stringr)
library(tidyverse)
library(kmetR)

raw.data.path <- "../../data/1_raw_data/data_adult/"
processed.data.path <- "../../data/2_processed_data/"

## PURRR MAP TO READ IN FILES
files <- dir(raw.data.path, pattern = "*.txt")

d <- purrr::map_df(files, .f = read_smi, 
                   file_path = raw.data.path,
                   length_header = 40) %>% 
  mutate(subid = str_trim(subid))

## Preprocess all data
d_processed <- d %>% 
  split(.$subid) %>% 
  purrr::map_df(preprocess_smi, x_max = 1920, y_max = 1080, samp_rate = 30) %>% 
  mutate(t.stim = round(t.stim, 3))

## Diagnositic plots
d_processed %>% 
  filter(subid == subid[3]) %>% 
  mutate(x = ifelse(x < 1 | x > 1919, NA, x), 
         y = ifelse(y < 1 | y > 1079, NA, y)) %>% 
  ggplot(aes(x = t.stim, y = x)) + 
  geom_line() + 
  geom_point(alpha = .3) + 
  facet_wrap(~stimulus) + 
  ylim(c(0,1920)) + 
  geom_hline(yintercept = 1920 / 2, lty = 2)

d_processed %>% 
  filter(subid == subid[3]) %>% 
  mutate(x = ifelse(x < 1 | x > 1919, NA, x), 
         y = ifelse(y < 1 | y > 1079, NA, y)) %>% 
  ggplot(aes(x = x, y = y, col = t.stim)) + 
  geom_line() + 
  geom_point(alpha = .3) + 
  facet_wrap(~stimulus) + 
  xlim(c(0,1920)) + 
  ylim(c(0,1080)) 

## WRITE DATA OUT TO CSV FOR EASY ACCESS
write_csv(d_processed, paste0(processed.data.path, "processed_adult_data.csv")) 
