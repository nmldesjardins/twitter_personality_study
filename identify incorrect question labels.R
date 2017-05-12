# During the course of merging the data, I found that two questions had been
# incorrectly labeled in Qualtrics, which meant they were also incorrect
# when the question labels were parsed to create the long dataset. Because the
# issue was so simple (the question names just needed to be changed prior to
# restructuring the data), I made the correction in the appropriate places in
# `merge_restructure_deID.R`. This code file just documents how I got there.
# (Note: this was done prior to removing and re-naming the ID variables)

## investigate the cases that have only 1 response for a target - CORRECTED 5.3.17

resp[which(resp$Freq == 1),] # all for targets 55 & 14

resp[which(resp$target_user == 55),]
resp[which(resp$target_user == 14),]

resp[which(resp$unique_id2 == 250),]

View(df[which(df$target_user == 14 & (df$unique_id2 == 250 | df$unique_id2 == 688 | df$unique_id2 == 484)),])
View(df[which(df$target_user == 55 & (df$unique_id2 == 542 | df$unique_id2 == 214 | df$unique_id2 == 2)),])


# from the spot check, it looks like these were questions that were labeled
# incorrectly in qualtrics, and so were parsed to look like real questions
# for target = 14, key = Q1474
# for target = 55, key = Q5594

resp[which(resp$unique_id2 == 250),]
resp[which(resp$unique_id2 == 688),]
resp[which(resp$unique_id2 == 542),]
resp[which(resp$unique_id2 == 214),]

# looks like these folks all saw the same sets of participants
# the error that results in target = 55/Freq = 1 appears to be tied to target # 6
# 14 looks tied to target 91
# the spot check are all for friends or followers

singleresp <- resp[which(resp$Freq == 1),]
tmp <- df[which(df$unique_id2 %in% singleresp$unique_id2),]
summary(as.factor(tmp$stim)) # from all 3 stim
summary(as.factor(tmp$target_user))

View(tmp[which(tmp$target_user==19),])
resp[which(resp$unique_id2 == 598),]


# the summary counts for targets look funny because some of the affected
# perceivers completed the survey more than once

# just look at stim for target = 14 | 55

tmp <- tmp %>% filter(target_user == 14 | target_user == 55)
summary(as.factor(tmp$stim)) # so really just followers and friends affected

# going back to the original surveys:
# Q5594 should be Q6.15; Q1474 should be Q91.6
# fixed in earlier pre-processing steps on 5.3.17
