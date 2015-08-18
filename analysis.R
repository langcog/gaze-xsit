rm(list = ls())
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)
library(tidyr)

d <- read_csv("processed_data/processed.csv")

### merge in subject info
subinfo <- read_csv("info/subinfo.csv")

d %<>% 
  mutate(subid = gsub(".txt", "", subid)) %>%
  left_join(subinfo)
  
### join trial info
trialinfo <- read_csv("info/trialinfo.csv")

d %<>% left_join(trialinfo, by = "stimulus")

### analyses

# raw
qplot(x, data=d) + 
  xlim(c(0,1980))

qplot(x, facets = ~ stimulus, 
      data=d) + 
  xlim(c(0,1980))

qplot(x, facets = ~ stimulus, data=d) +
  xlim(c(0,1980))

qplot(x, facets = ~ shortname, data=d) +
  xlim(c(0,1980))

qplot(x, facets = gaze.condition ~ trial.type, data=d) +
  xlim(c(0,1980))

########### AOI ANALYSIS ###########

# get AOIs
d %<>% mutate(aoi = factor(ifelse(x > 210 & x < 750 & y < 540, "left", 
                           ifelse(x > 1170 & x < 1710 & y < 540, "right", 
                                  ifelse(x < 1230 & x > 690 & y > 540, "face", NA)))))

# plots by subject
qplot(aoi, fill = aoi, 
      facets = .~ subid, 
      data = d) 


qplot(aoi, fill = aoi, 
      data = d) 

d_filtered <- d %>% filter(trial.type != "familiar")
qplot(aoi, fill = aoi, facets = trial.type ~ gaze.condition, data = d_filtered) 

# manipulation check on exposure trials
# compare the distribution of looking to one vs. both objects 
# for all participants 


all.exp <- d %>% 
  filter(trial.type == "exposure" & aoi != "face") %>%
  xtabs(~aoi + shortname + subid + target_object, .) %>% 
  as.data.frame() %>%
  filter(aoi != "face")

##plyr - drop equals false

all.exp <- d %>% 
  filter(trial.type == "exposure" & aoi != "face") %>%
  select(subid, condition) %>%
  unique() %>%
  left_join(all.exp, by = "subid")

##qplot(data=filter(ss, aoi != "face"), x=aoi, y = Freq, facets = subid ~ shortname,
##      geom = "bar", stat = "identity", fill = condition)


############ DETERMINE TARGET ON EXPOSURE ################

#create table for looks to target by trial name (exposure only)
exp.looks.to.target <- all.exp%>%
  select(shortname, condition) %>%
  unique()

#nogaze: proportion of looking to the left over both objects for each trial
ng.exp <- all.exp %>%
  filter(condition == "nogaze")

#mark naive participants in doc
#eventually - amount of time spent looking at kept object
#correct object is one kept
#slack help channel
  
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
nsmean <- total.looks / counted.trials #currently .499


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

#proportion of looks to left object across Gaze exposure trials
socmean <- total.looks / counted.trials

all.exp <- all.exp %>%
  filter(!is.na(exptarget))

### REMOVE MESSY TEMP THINGS ####
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

####### BY TRIAL #########

##plot look proportions by trial
##summarise & group by trial

#plots proportion looking to target by trial name across subjects
qplot(targetprop, 
      facets = .~ condition, 
      binwidth = .10,
      data = exp.looks.to.target) 

####### TEST LOOKING RATES ###########

#for each pair of trials, success rate is proportion of looking to target object
#in test trial

#1: just get base success rate
#2: plot base success rate by gaze vs no gaze
#3: further break it up to reflect whether or not they looked at target in exposure

all.test <- d %>% 
  filter(trial.type == "test" & aoi != "face") %>%
  xtabs(~aoi + shortname + subid + target_object, .) %>% 
  as.data.frame() %>%
  filter(aoi != "face")

##plyr - drop equals false

all.test <- d %>% 
  filter(trial.type == "test" & aoi != "face") %>%
  select(subid, condition) %>%
  unique() %>%
  left_join(all.test, by = "subid")

#create table for looks to target by trial name (exposure only)
test.looks.to.target <- all.test%>%
  select(shortname, condition) %>%
  unique()

#nogaze: proportion of looking to the left over both objects for each trial
ng.test <- all.test %>%
  filter(condition == "nogaze")

#mark naive participants in doc
#eventually - amount of time spent looking at kept object
#correct object is one kept
#slack help channel

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
ntmean <- nttotal / counted.trials #currently .499


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

### REMOVE MESSY TEMP THINGS ####
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

qplot(targetprop, 
      facets = .~ condition, 
      data = test.looks.to.target) 


######### SAME/SWITCH ############

#"same": looked more at target object on exposure
#"switch": looked more at non-target object on exposure

##dealing with same trials only, should see a reasonably high rate of success on 
##test trials



all.looks <- all.exp %>%
  left_join(all.test, by = c("subid", "condition", "aoi", "shortname")) %>%
  select(subid, condition, shortname, exptarget, testtarget) %>%
  unique()


##x-axis: how an individual did on exposure trial
##y-axis: how the individual did on test trial
qplot(x=exptarget, y=testtarget, color = condition, data=all.looks)


####### TIMECOURSE ANALYSES #######
ms <- d %>%
  mutate(t.stim.binned = round(t.stim * 10) / 10) %>%
  group_by(t.stim.binned, gaze.condition) %>%
  summarise(face = mean(aoi == "face", na.rm=TRUE), 
            left = mean(aoi == "left", na.rm=TRUE),
            right = mean(aoi == "right", na.rm=TRUE)) %>%
  gather(stim, prop, face, left, right)

qplot(t.stim.binned, prop, col = stim, 
      geom = "line",
      facets = ~ gaze.condition, 
      data=ms) + 
  ylim(c(0,1)) +
  geom_hline(yintercept = .5, lty = 2, col = "black")




