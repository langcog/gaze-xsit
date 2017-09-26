score_smi_gaze_xsit <- function(trial_df) {
  # check if there is a shift in the trial after the critical onset
  # and record where the shift started and ended
  crit.window.responses <- trial_df %>%
    select(t.stim, ss_looking_char, trial_num, correct, subid, trial.type) %>%
    group_by(trial_num, ss_looking_char, correct, trial.type) %>%
    summarise(min_t = min(t.stim)) %>%
    arrange(min_t)
  
  # store info about the first shift
  shift_start <- crit.window.responses$ss_looking_char[1]
  
  # check if there is only one "response" in the target_looking vector
  # if 1, then there was no shift (i.e., no change from response at crit.onset)
  if (nrow(crit.window.responses) == 1) {
    trial_score <- data.frame(rt = NA, 
                              shift_start = shift_start,
                              shift_end = NA,
                              shift_type = "no_shift")
    
  } else {
    # get the earliest time point when target looking switches from the critical onset value
    shift_end <- crit.window.responses$ss_looking_char[2]
    rt <- crit.window.responses$min_t[2]
    correct <- crit.window.responses$correct[2]
    trial_score <- data.frame(rt = rt, 
                              shift_start = shift_start,
                              shift_end = shift_end,
                              correct = correct,
                              shift_type = case_when(
                                shift_start == "face" & shift_end == "away" ~ "face_away",
                                shift_start == "face" & correct == T ~ "face_target",
                                shift_start == "face" & correct == F ~ "face_distracter",
                                shift_start == "left" | shift_start == "right" & shift_end == "face" ~ "image_face",
                                shift_start == "away" & shift_end == "face" & correct == F ~ "away_face",
                                shift_start == "away" & shift_end == "left" & correct == F ~ "away_distracter",
                                shift_start == "away" & shift_end == "left" & correct == T ~ "away_target",
                                shift_start == "away" & shift_end == "right" & correct == T ~ "away_target",
                                shift_start == "away" & shift_end == "right" & correct == F ~ "away_distracter",
                                shift_start == "left" & shift_end == "right" & correct == T ~ "distracter_target",
                                shift_start == "left" & shift_end == "right" & correct == F ~ "target_distracter",
                                shift_start == "right" & shift_end == "left" & correct == T ~ "distracter_target",
                                shift_start == "right" & shift_end == "left" & correct == F ~ "target_distracter",
                                TRUE ~ "NA"
                              ))
  }
  
  # add the rt and score to the trial data frame and return
  trial_df %>% 
    select(subid, trial_num, trial.type) %>% 
    unique() %>% 
    cbind(., trial_score)
}