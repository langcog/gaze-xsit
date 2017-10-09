### Load libraries

library(magrittr)
library(knitr)
library(langcog)
library(directlabels)
library(tidyverse)
theme_set(theme_bw())

## score participant
score_smi_participant <- function(subject_df) {
  subject_df %>%  
    split(.$trial_num_full) %>% 
    purrr::map_df(score_smi_trial)
}


## score trial
score_smi_trial <- function(trial_df) {
  # check if there is a shift in the trial after the critical onset
  # and record where the shift started and ended
  crit.window.responses <- trial_df %>%
    select(subid, t.stim, ss_looking_char, trial_num_full, correct) %>%
    group_by(trial_num_full, ss_looking_char, correct, subid) %>%
    summarise(min_t = min(t.stim)) %>%
    arrange(min_t)
  
  # store info about the first shift
  shift_start <- crit.window.responses$ss_looking_char[1]
  
  # check if there is only one "response" in the target_looking vector
  # if 1, then there was no shift (i.e., no change from response at crit.onset)
  if (nrow(crit.window.responses) == 1) {
    trial_df <- trial_df %>% 
      mutate(rt = NA, 
             shift_start = shift_start,
             shift_end = NA,
             shift_type = "no_shift",
             shift_correct = NA)
    
  } else {
    # get the earliest time point when target looking switches from the critical onset value
    shift_end <- crit.window.responses$ss_looking_char[2]
    trial_df <- trial_df %>% 
      mutate(rt = crit.window.responses$min_t[2], 
             shift_start = shift_start,
             shift_end = shift_end,
             shift_correct = crit.window.responses$correct[2],
             shift_type = case_when(
               shift_start == "face" & shift_end == "away" ~ "face_away",
               shift_start == "face" & shift_correct == T ~ "face_target",
               shift_start == "face" & shift_correct == F ~ "face_distracter",
               shift_start == "left" | shift_start == "right" & shift_end == "away" ~ "image_away",
               shift_start == "left" | shift_start == "right" & shift_end == "face" ~ "image_face",
               shift_start == "away" & shift_end == "face" & shift_correct == F ~ "away_face",
               shift_start == "away" & shift_end == "left" & shift_correct == F ~ "away_distracter",
               shift_start == "away" & shift_end == "left" & shift_correct == T ~ "away_target",
               shift_start == "away" & shift_end == "right" & shift_correct == T ~ "away_target",
               shift_start == "away" & shift_end == "right" & shift_correct == F ~ "away_distracter",
               shift_start == "left" & shift_end == "right" & shift_correct == T ~ "distracter_target",
               shift_start == "left" & shift_end == "right" & shift_correct == F ~ "target_distracter",
               shift_start == "right" & shift_end == "left" & shift_correct == T ~ "distracter_target",
               shift_start == "right" & shift_end == "left" & shift_correct == F ~ "target_distracter",
               TRUE ~ "NA"
             ))
  }
  
  trial_df %>% 
    select(subid, trial_num_full, rt, shift_start, shift_end, shift_correct, shift_type) %>% 
    unique()
}