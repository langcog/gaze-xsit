---
  title: "gaze-xsit_adult"
author: "Allison Dods"
date: "February 2, 2016"
output: html_document
---
  
  
  ```{r setup} 
rm(list = ls())
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(lme4)
```

```{r create mastersheet}
## output: d (mastersheet with times, trial, subject, condition, and gazes)
## input: processed data spreadsheet, subject info spreadsheet, trial info

#read in processed data
d <- read_csv("processed_data_ettest/processed_ettest.csv")

# join subject info
#subinfo <- read_csv("info/subinfo_adult.csv")
#d %<>% 
#  mutate(subid = gsub(".txt", "", subid)) %>%
#  left_join(subinfo)

# join trial info
trialinfo <- read_csv("info/trialinfo.csv")
d %<>% left_join(trialinfo, by = "stimulus")

# after adding shortnames we want to flag which trials
# were and were not the target of gaze on exposure trials

d %<>% 
  filter(trial.type == "exposure") %>% 
  mutate(same_switch_trial = ifelse(target_object == look, "same", "switch")) %>% 
  select(shortname, same_switch_trial) %>% 
  left_join(d, ., by = "shortname")
  
## get the proportion of "away" looks




# filter out unusable participants
d %<>% filter(keep == "y")
```

```{r AOI setup}
## output: all.exp (spreadsheet of exposure trial data)

# label AOIs
d %<>% mutate(aoi = factor(ifelse(x > 210 & x < 750 & y < 540, "left", 
                                  ifelse(x > 1170 & x < 1710 & y < 540, "right", 
                                         ifelse(x == 0 & y == 1050, "away",
                                          ifelse(x < 1230 & x > 690 & y > 540, "face", NA))))))

## sanity check: what proportion of experiment was ss looking away?
## add in per subid in groupby
away_looking_df <- d %>% 
  group_by(aoi) %>% 
  summarise(count = n()) 

away_looking_df %>%
  summarise(total_time_slices = sum(count)) %>% 
  cbind(away_looking_df) %>% 
  group_by(aoi) %>% 
  summarise(prop_looking = count / total_time_slices) %>% 
  ggplot(aes(x=aoi, y=prop_looking), data = .) +
  geom_bar(stat = "identity")

# create all.exp, 
all.exp <- d %>% 
  filter(trial.type == "exposure" & aoi != "face") %>%
  xtabs(~aoi + shortname  + target_object, .) %>% 
  as.data.frame() %>%
  filter(aoi != "face")
#this is redundant -- look at this
all.exp <- d %>% 
  filter(trial.type == "exposure" & aoi != "face") %>%
  unique() 
```

```{r exposure ng}
#create table for looks to target by trial name (exposure only)
exp.looks.to.target <- all.exp%>%
  select(shortname, condition) %>%
  unique()

#nogaze: proportion of looking to the left over both objects for each trial
ng.exp <- all.exp %>%
  filter(condition == "nogaze")

subs <- unique(ng.exp$subid)
trials <- unique(ng.exp$shortname)
total.looks <- 0
counted.trials <- 0
for (trial in trials){
  trialtotal <- 0
  counted.subs <- 0
  for (sub in subs){
    temp <- subset(ng.exp, shortname == trial & subid == sub)
    left.to.total <- temp[[which(temp$target_object == "left" &
                                   temp$aoi == "left"), "Freq"]] +
      temp[[which(temp$target_object == "left" & temp$aoi == "right"), "Freq"]]
    right.to.total <- temp[[which(temp$target_object == "right" &
                                    temp$aoi == "left"), "Freq"]] +
      temp[[which(temp$target_object == "right" & temp$aoi == "right"), "Freq"]]
    if(left.to.total + right.to.total != 0){
      if(left.to.total == 0){
        correct_TO = "right"
        incorrect_TO = "left"
      } else{
        correct_TO = "left"
        incorrect_TO = "right"
      }
      temp <- subset(temp, target_object == correct_TO)
      corr <- temp[[which(temp$aoi == correct_TO), "Freq"]]
      incorr <- temp[[which(temp$aoi == incorrect_TO), "Freq"]]
      target <- corr / (corr+incorr)
      all.exp$exptarget[all.exp$subid == sub & all.exp$shortname == trial & 
                          all.exp$target_object == correct_TO] <- target
      total.looks <- total.looks + target
      counted.trials = counted.trials + 1
      counted.subs = counted.subs + 1
      trialtotal <- trialtotal + target
    }
  }
  trialmean <- trialtotal / counted.subs
  exp.looks.to.target$targetprop[exp.looks.to.target$shortname == trial &
                                   exp.looks.to.target$condition == "nogaze"] <-trialmean
}

#proportion of looks to left object across No Gaze exposure trials
nsmean <- total.looks / counted.trials

```

```{r exposure g}
#gaze: proportion of looking to object of gaze for each trial

g.exp <- all.exp %>%
  filter(condition == "gaze")

subs <- unique(g.exp$subid)
trials <- unique(g.exp$shortname)
total.looks <- 0
counted.trials <- 0
for (trial in trials){
  trialtotal <- 0
  counted.subs <- 0
  for (sub in subs){
    temp <- subset(g.exp, shortname == trial & subid == sub)
    left.to.total <- temp[[which(temp$target_object == "left" &
                                   temp$aoi == "left"), "Freq"]] +
      temp[[which(temp$target_object == "left" & temp$aoi == "right"), "Freq"]]
    right.to.total <- temp[[which(temp$target_object == "right" &
                                    temp$aoi == "left"), "Freq"]] +
      temp[[which(temp$target_object == "right" & temp$aoi == "right"), "Freq"]]
    if(left.to.total + right.to.total != 0){
      if(left.to.total == 0){
        correct_TO = "right"
        incorrect_TO = "left"
      } else{
        correct_TO = "left"
        incorrect_TO = "right"
      }
      temp <- subset(temp, target_object == correct_TO)
      corr <- temp[[which(temp$aoi == correct_TO), "Freq"]]
      incorr <- temp[[which(temp$aoi == incorrect_TO), "Freq"]]
      target <- corr / (corr+incorr)
      all.exp$exptarget[all.exp$subid == sub & all.exp$shortname == trial & 
                          all.exp$target_object == correct_TO] <- target
      total.looks <- total.looks + target
      counted.trials = counted.trials + 1
      counted.subs = counted.subs + 1
      trialtotal <- trialtotal + target
    }
  }
  trialmean <- trialtotal / counted.subs
  exp.looks.to.target$targetprop[exp.looks.to.target$shortname == trial &
                                   exp.looks.to.target$condition == "gaze"] <-trialmean
}

### summarise looking to aois

trial_df <- g.exp %>% 
  group_by(shortname, aoi, look) %>% 
  summarise(num_time_slices = n()) %>% 
  mutate(gaze_target = ifelse(aoi == look, "gaze_target", "not_gaze_target"))

### compute prop looking

trial_df <- trial_df %>% 
  group_by(shortname) %>% 
  summarise(total_time_slices = sum(num_time_slices)) %>% 
  left_join(trial_df, ., by = "shortname") %>% 
  mutate(prop_looking = num_time_slices / total_time_slices)

### visualize gaze following

trial_df %>% 
  group_by(gaze_target) %>% 
  summarise(m_gf = mean(prop_looking)) %>% 
  ggplot(aes(x=gaze_target, y = m_gf), data = .) +
  geom_bar(stat = "identity") +
  theme_bw()
  
  

#proportion of looks to left object across Gaze exposure trials
socmean <- total.looks / counted.trials

all.exp <- all.exp %>%
  filter(!is.na(exptarget))
```

```{r remove temporary things, echo=FALSE}
rm(corr)
rm(correct_TO)
rm(incorr)
rm(incorrect_TO)
rm(subs)
rm(trials)
rm(temp)
rm(counted.subs)
rm(counted.trials)
rm(left.to.total)
rm(right.to.total)
rm(sub)
rm(target)
rm(total.looks)
rm(trial)
rm(trialmean)
rm(trialtotal)
```

```{r test looking rates ng}
#for each pair of trials, success rate is proportion of looking to target object
#in test trial

all.test <- d %>% 
  filter(trial.type == "test" & aoi != "face") %>%
  xtabs(~aoi + shortname + target_object, .) %>% 
  as.data.frame() %>%
  filter(aoi != "face")


all.test <- d %>% 
  filter(trial.type == "test" & aoi != "face") %>%
  unique() 

#create table for looks to target by trial name (exposure only)
test.looks.to.target <- all.test%>%
  select(shortname, condition) %>%
  unique()

#nogaze: proportion of looking to the left over both objects for each trial
ng.test <- all.test %>%
  filter(condition == "nogaze")



subs <- unique(ng.test$subid)
trials <- unique(ng.test$shortname)
nttotal <- 0
counted.trials <- 0
for (trial in trials){
  trialtotal <- 0
  counted.subs <- 0
  for (sub in subs){
    temp <- subset(ng.test, shortname == trial & subid == sub)
    left.to.total <- temp[[which(temp$target_object == "left" &
                                   temp$aoi == "left"), "Freq"]] +
      temp[[which(temp$target_object == "left" & temp$aoi == "right"), "Freq"]]
    right.to.total <- temp[[which(temp$target_object == "right" &
                                    temp$aoi == "left"), "Freq"]] +
      temp[[which(temp$target_object == "right" & temp$aoi == "right"), "Freq"]]
    if(left.to.total + right.to.total != 0){
      if(left.to.total == 0){
        correct_TO = "right"
        incorrect_TO = "left"
      } else{
        correct_TO = "left"
        incorrect_TO = "right"
      }
      temp <- subset(temp, target_object == correct_TO)
      corr <- temp[[which(temp$aoi == correct_TO), "Freq"]]
      incorr <- temp[[which(temp$aoi == incorrect_TO), "Freq"]]
      target <- corr / (corr+incorr)
      all.test$testtarget[all.test$subid == sub & all.test$shortname == trial & 
                            all.test$target_object == correct_TO] <- target
      nttotal <- nttotal + target
      counted.trials = counted.trials + 1
      counted.subs = counted.subs + 1
      trialtotal <- trialtotal + target
    }
  }
  trialmean <- trialtotal / counted.subs
  test.looks.to.target$targetprop[test.looks.to.target$shortname == trial &
                                    test.looks.to.target$condition == "nogaze"] <-trialmean
}

#proportion of looks to left object across No Gaze exposure trials
ntmean <- nttotal / counted.trials #currently .60

```

```{r test looking rates g}
#gaze: proportion of looking to object of gaze for each trial

g.test <- all.test %>%
  filter(condition == "gaze")

subs <- unique(g.test$subid)
trials <- unique(g.test$shortname)
socttotal <- 0
counted.trials <- 0
for (trial in trials){
  trialtotal <- 0
  counted.subs <- 0
  for (sub in subs){
    temp <- subset(g.test, shortname == trial & subid == sub)
    left.to.total <- temp[[which(temp$target_object == "left" &
                                   temp$aoi == "left"), "Freq"]] +
      temp[[which(temp$target_object == "left" & temp$aoi == "right"), "Freq"]]
    right.to.total <- temp[[which(temp$target_object == "right" &
                                    temp$aoi == "left"), "Freq"]] +
      temp[[which(temp$target_object == "right" & temp$aoi == "right"), "Freq"]]
    if(left.to.total + right.to.total != 0){
      if(left.to.total == 0){
        correct_TO = "right"
        incorrect_TO = "left"
      } else{
        correct_TO = "left"
        incorrect_TO = "right"
      }
      temp <- subset(temp, target_object == correct_TO)
      corr <- temp[[which(temp$aoi == correct_TO), "Freq"]]
      incorr <- temp[[which(temp$aoi == incorrect_TO), "Freq"]]
      target <- corr / (corr+incorr)
      all.test$testtarget[all.test$subid == sub & all.test$shortname == trial & 
                            all.test$target_object == correct_TO] <- target
      socttotal <- socttotal + target
      counted.trials = counted.trials + 1
      counted.subs = counted.subs + 1
      trialtotal <- trialtotal + target
    }
  }
  trialmean <- trialtotal / counted.subs
  test.looks.to.target$targetprop[test.looks.to.target$shortname == trial &
                                    test.looks.to.target$condition == "gaze"] <-trialmean
}

#proportion of looks to left object across Gaze exposure trials
soctmean <- socttotal / counted.trials

all.test <- all.test %>%
  filter(!is.na(testtarget))
```


### summarise looking to aois

trial_df <- all.test %>% 
  group_by(shortname, aoi, target_object, same_switch_trial) %>% 
  summarise(num_time_slices = n()) 

### compute prop looking

trial_df <- trial_df %>% 
  group_by(shortname, same_switch_trial) %>% 
  summarise(total_time_slices = sum(num_time_slices)) %>% 
  left_join(trial_df, ., by = "shortname") %>% 
  mutate(prop_looking = num_time_slices / total_time_slices, 
         target_looking = ifelse(aoi == target_object, "target", "distractor"))

### visualize gaze following

trial_df %>% 
  group_by(same_switch_trial.x, target_looking) %>% 
  summarise(m_target_looking = mean(prop_looking)) %>% 
  ggplot(aes(x=same_switch_trial.x, y = m_target_looking, fill = target_looking), 
         data = .) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  scale_fill_solarized()
  


```{r remove temp things, echo=FALSE}
rm(corr)
rm(correct_TO)
rm(incorr)
rm(incorrect_TO)
rm(subs)
rm(trials)
rm(temp)
rm(counted.subs)
rm(counted.trials)
rm(left.to.total)
rm(right.to.total)
rm(sub)
rm(target)
rm(total.looks)
rm(trial)
rm(trialmean)
rm(trialtotal)
```

```{r analysis}
all.looks <- all.exp %>%
  left_join(all.test, by = c("subid", "condition", "aoi", "shortname")) %>%
  select(subid, condition, shortname, exptarget, testtarget) %>%
  unique()


##x-axis: how an individual did on exposure trial
##y-axis: how the individual did on test trial
qplot(x=exptarget, y=testtarget, color = condition, 
      data=all.looks) +
  facet_grid( . ~ condition) + 
  geom_smooth(method="loess", span = 1) +
  theme_bw()

library(langcog)
ms.looks <- all.looks %>%
  mutate(exptarget.bin = cut(exptarget, 4)) %>%
  group_by(exptarget.bin, condition) %>%
  multi_boot_standard(column = "testtarget", na.rm = TRUE)

qplot(exptarget.bin, mean, ymin = ci_lower, ymax = ci_upper, 
      group = condition, col = condition, geom = c("line","pointrange"),
      position = position_dodge(width = .05),
      data= ms.looks) +
  geom_hline(yintercept = .5, lty = 2) + 
  theme_bw() +
  scale_colour_solarized() + 
  ylim(c(0,1))

```