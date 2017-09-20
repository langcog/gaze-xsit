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
d_processed <- preprocess_smi(d) %>% 
  mutate(t.stim = round(t.stim, 3))

## WRITE DATA OUT TO CSV FOR EASY ACCESS
write_csv(d_processed, paste0(processed.data.path, "processed_adult_data.csv")) 