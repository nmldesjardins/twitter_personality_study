################################################################################
####                                SETUP                                   ####
################################################################################

# load the tidyverse
library(tidyverse)
# set working directory
setwd('~/Dropbox/Twitter and personality project/personality impressions study/Personality Ratings Study/third times a charm/raw files from qualtrics')


################################################################################
####                             DATA INGESTION                             ####
################################################################################

# HUMAN SUBJECTS POOL DATA -----------------------------------------------------

# Study 1  ----

## read in the data from the original qualtrics files

# profiles
header <- tbl_df(read.csv("Twitter_Profiles___First_100_HSP.csv",
                            nrows=1))
prof_hsp <- tbl_df(read.csv("Twitter_Profiles___First_100_HSP.csv",
                            header = F, stringsAsFactors = F, skip = 2))
names(prof_hsp) <- names(header)
prof_hsp$stim = 'profile'

# friends
frheader <- tbl_df(read.csv("Twitter_Friends___First_100.csv",
                          nrows=1))
fri_hsp <- tbl_df(read.csv("Twitter_Friends___First_100.csv",
                            header = F, stringsAsFactors = F, skip = 2))
names(fri_hsp) <- names(frheader)
fri_hsp$stim = 'friends'

# followers
foheader <- tbl_df(read.csv("Twitter_Followers_First_100_Sona.csv",
                            nrows=1))
fol_hsp <- tbl_df(read.csv("Twitter_Followers_First_100_Sona.csv",
                           header = F, stringsAsFactors = F, skip = 2))
names(fol_hsp) <- names(foheader)
fol_hsp$stim = 'followers'


## get rid of variables we won't need

# these are qualtrics-generated variables (like timing, location), and
# variables that just indicate the user saw a page (e.g., instructions, debrief)
# the variable names are slightly different for each survey because of how they
# were created in qualtrics
# also does some re-naming so datasets will eventually line up/corrects
# qualtrics errors

# profiles
prof_hsp <- subset(prof_hsp, select = -c(endtiming_1:endtiming_4, Q102.1, Q102.7, 
                                         end, LocationLatitude,LocationLongitude,
                                         LocationAccuracy,V1:V7, debrief,
                                         Q1.1, Q1.3:Q1.5, Comments, X, Q3024))
prof_hsp <- prof_hsp[, -grep(".30_", colnames(prof_hsp))]

# friends
fri_hsp <- subset(fri_hsp, select = -c(Q5960_1:Q5960_4, Q3029_1:Q3029_4,
                                         LocationLatitude,LocationLongitude,
                                         LocationAccuracy,V1:V7, Q102.1, Q102.7,
                                         Q1.1, Q1.3:Q1.5, Debrief, Comments, X, Q3031))
fri_hsp <- fri_hsp[, -grep(".30_", colnames(fri_hsp))]
names(fri_hsp)[names(fri_hsp) == 'Q5594'] <- 'Q6.15'
names(fri_hsp)[names(fri_hsp) == 'Q1474'] <- 'Q91.6'


# followers
fol_hsp <- subset(fol_hsp, select = -c(Q5960_1:Q5960_4, Q3026, Q3028,
                                         LocationLatitude,LocationLongitude,
                                         LocationAccuracy,V1:V7, Q3033,
                                         Q1.1, Q102.1, Q102.7, 
                                        Q102.13, X, Q102.15))
fol_hsp <- fol_hsp[, -grep(".30_", colnames(fol_hsp))]
names(fol_hsp)[names(fol_hsp) == 'Q5594'] <- 'Q6.15'
names(fol_hsp)[names(fol_hsp) == 'Q1474'] <- 'Q91.6'

length(names(prof_hsp))
length(names(fri_hsp))
length(names(fol_hsp))


## merge all three together

# first check the age variables (won't merge if different types)
str(fol_hsp[,grep("..24", colnames(fol_hsp))])
str(fri_hsp[,grep("..24", colnames(fri_hsp))])
str(prof_hsp[,grep("..24", colnames(prof_hsp))])

# some Ps entered characters (e.g., '?') as ages for the profiles
# change to integer
age_vars = c('Q52.24','Q53.24','Q54.24','Q55.24','Q56.24','Q71.24')
prof_hsp[,age_vars] <- as.integer(unlist(prof_hsp[,age_vars]))


# now bind

hsp <- bind_rows(list(prof_hsp, fri_hsp, fol_hsp), .id = 'df')


### Restructure 

# v10 = finished survey (1 if did)
# q1.2 = consent (should be 1)
# questions start on qN.4

hsp_long <- hsp %>%
                gather(key, value, -V8, -V9, -V10, -id, -Q1.2, -stim, -df, na.rm=T) %>%
                extract(key,c('target_user','question'),"Q([:digit:]+).([:digit:]+)", 
                remove=F)

## drop extra instruction items

# qn.1 = instructions
# qn.2 = profile picture; qn.3 = 'this person...'
# qn.27 = ladder instructions
# type_convert converts dtypes from chr to more useful types
# keep all user 102 qs (these are participant demographics)

hsp_long <- hsp_long %>% type_convert() %>%
                filter(question > 3 | target_user == 102) %>% # only keep actual questions + demographics
                filter(question != 27) # drop q27 (ladder instructions)

summary(as.factor(hsp_long$question))
summary(as.factor(hsp_long$target_user))

# identify the sample + study
hsp_long$sample = 'hsp'
hsp_long$study = 1


# Study 2  ----
# the data from study 2 are arranged slightly differently, so i'll deal with
# it separately here. i'll then merge it into the existing long file so that the
# final unique identifier will capture all of the hsp participants (there were
# restrictions in place to keep them from participating in more than 1 of these
# 4 surveys, but i want to ID them all together just in case someone slipped
# through the system)

decheader <- tbl_df(read.csv("Twitter_Profiles__Behavioral_Affordances_Followup.csv",
                            nrows=1))
dec <- tbl_df(read.csv("Twitter_Profiles__Behavioral_Affordances_Followup.csv",
                           header = F, stringsAsFactors = F, skip = 2))
names(dec) <- names(decheader)
dec$stim = 'profile'


dec <- subset(dec, select = -c(Q1.1, Q1.3:Q1.5, Q102.1, Q102.7, Q102.9, Q103.1:Q103.4,
                                V1:V7, LocationLatitude,LocationLongitude,
                                LocationAccuracy, X))
dec <- dec[, -grep(".31_", colnames(dec))]

dec_long <- dec %>%
        gather(key, value, -V8, -V9, -V10, -id, -Q1.2, -stim, na.rm=T) %>%
        extract(key,c('target_user','question'),"Q([:digit:]+).([:digit:]+)", 
                remove=F)

## drop extra instruction items
# qn.1 = instructions; qn.2 = profile picture; qn.3 = 'this person...'
# type_convert converts dtypes from chr to more useful types
# keep all user 102 qs (these are participant demographics)

dec_long <- dec_long %>% type_convert() %>% 
        filter(question > 3 | target_user == 102) 

# identify the sample + study
dec_long$sample = 'hsp'
dec_long$study = 2
dec_long$df = 4 # this was automatically created for 1:3 for the 3 personality files

# Merge S1 & S2 ----
s1s2hsp <- bind_rows(hsp_long,dec_long)

summary(as.factor(s1s2hsp$study))
summary(as.factor(s1s2hsp$sample))
summary(as.factor(s1s2hsp$stim))


## Create a unique user id
# this isn't strictly necessary here, as we'll have to make a new
# id when both samples are combined (so they don't overlap). I did it here
# just to be sure I had a good handle on N
s1s2hsp$unique_id <- as.numeric(as.factor(with(s1s2hsp, paste(id))))

length(unique(s1s2hsp$id))
length(unique(s1s2hsp$unique_id)) # 460 unique IDs, ranging from 1:460

# at this point, everyone appears to be uniquely identified
# but! some users have too many responses -- incomplete attempts and duplicate
# attempts (e.g., where they started, didn't finish, and then came back and
# started again) are still included
summary(as.factor(s1s2hsp$id))
summary(as.factor(s1s2hsp$unique_id))


# MTURK DATA -----------------------------------------------------
# we'll follow the same process as we did above, but now for the mturkers

## read in the data from the original qualtrics files

# profiles
headerm <- tbl_df(read.csv("Twitter_Profiles___First_100.csv",
                          nrows=1))
prof_mt <- tbl_df(read.csv("Twitter_Profiles___First_100.csv",
                            header = F, stringsAsFactors = F, skip = 2))
names(prof_mt) <- names(headerm)
prof_mt$stim = 'profile'

# friends
frheaderm <- tbl_df(read.csv("Twitter_Friends___First_100___MTURK.csv",
                            nrows=1))
fri_mt <- tbl_df(read.csv("Twitter_Friends___First_100___MTURK.csv",
                           header = F, stringsAsFactors = F, skip = 2))
names(fri_mt) <- names(frheaderm)
fri_mt$stim = 'friends'

# followers
foheaderm <- tbl_df(read.csv("Twitter_Followers_First_100_MTURK.csv",
                            nrows=1))
fol_mt <- tbl_df(read.csv("Twitter_Followers_First_100_MTURK.csv",
                           header = F, stringsAsFactors = F, skip = 2))
names(fol_mt) <- names(foheaderm)
fol_mt$stim = 'followers'


## get rid of variables we won't need

# profiles
prof_mt <- subset(prof_mt, select = -c(endtiming_1:endtiming_4, Q102.1, Q102.7, 
                                         end, LocationLatitude,LocationLongitude,
                                         LocationAccuracy,V1:V7,
                                         Q1.1, Q1.3:Q1.5, Comments, X, Q3024, 
                                         HITcode))
prof_mt <- prof_mt[, -grep(".30_", colnames(prof_mt))]
names(prof_mt)[names(prof_mt) == 'MTurkID'] <- 'id'

# friends
fri_mt <- subset(fri_mt, select = -c(Q5960_1:Q5960_4, Q3029_1:Q3029_4,
                                       LocationLatitude,LocationLongitude,
                                       LocationAccuracy,V1:V7, Q102.1, Q102.7,
                                       Q1.1, Q1.3:Q1.5, Comments, X, Q3031,
                                     Q3035))
fri_mt <- fri_mt[, -grep(".30_", colnames(fri_mt))] #q1.2 = consent

names(fri_mt)[names(fri_mt) == 'consent'] <- 'Q1.2'
names(fri_mt)[names(fri_mt) == 'Q3033'] <- 'id'
names(fri_mt)[names(fri_mt) == 'Q5594'] <- 'Q6.15'
names(fri_mt)[names(fri_mt) == 'Q1474'] <- 'Q91.6'


# followers
fol_mt <- subset(fol_mt, select = -c(Q5960_1:Q5960_4, Q3026, Q3028, Q3021,
                                       LocationLatitude,LocationLongitude,
                                       LocationAccuracy,V1:V7,
                                       Q1.1, Q102.1, Q102.7,
                                       Q102.13, X, Q102.15))
fol_mt <- fol_mt[, -grep(".30_", colnames(fol_mt))]

names(fol_mt)[names(fol_mt) == 'Q102.11'] <- 'id'
names(fol_mt)[names(fol_mt) == 'Q5594'] <- 'Q6.15'
names(fol_mt)[names(fol_mt) == 'Q1474'] <- 'Q91.6'

length(names(prof_mt))
length(names(fri_mt))
length(names(fol_mt))


## merge all three together

# first check the age variables (won't merge if different types)
str(fol_mt[,grep("..24", colnames(fol_mt))])
str(fri_mt[,grep("..24", colnames(fri_mt))])
str(prof_mt[,grep("..24", colnames(prof_mt))])

# now bind

mturk <- bind_rows(list(prof_mt, fri_mt, fol_mt), .id = 'df')


### Restructure 

# v10 = finished survey (1 if did)
# q1.2 = consent (should be 1)
# questions start on qn.4

mt_long <- mturk %>%
        gather(key, value, -V8, -V9, -V10, -id, -Q1.2, -stim, -df, na.rm=T) %>%
        extract(key,c('target_user','question'),"Q([:digit:]+).([:digit:]+)", 
                remove=F)


## drop extra instruction items
# qn.1 = instructions; qn.2 = profile picture; qn.3 = 'this person...'
# qn.27 = ladder instructions
# type_convert converts dtypes from chr to more useful types
# keep all user 102 qs (these are participant demographics)

mt_long <- mt_long %>% type_convert() %>%
        filter(question > 3 | target_user == 102) %>% 
        filter(question != 27)

summary(as.factor(mt_long$question))
summary(as.factor(mt_long$target_user))

# identify sample + study
mt_long$sample = 'mturk'
mt_long$study = 1


## Create a unique user id
mt_long$unique_id <- as.numeric(as.factor(with(mt_long, paste(toupper(id)))))

length(unique(mt_long$id))
length(unique(mt_long$unique_id)) # 257 unique IDs; 1:257

# again, duplicates and unfinished are still included
summary(as.factor(mt_long$id))
summary(as.factor(mt_long$unique_id))

summary(as.factor(mt_long[which(mt_long$unique_id==256),]$id)) # id = NA, which is why the count is so high for this one



# COMBINE MTURK & HSP ----------------------------------------------------------

# change hsp id from numeric so they play well together
length(unique(s1s2hsp$id))
s1s2hsp$id <- as.character(s1s2hsp$id) 
length(unique(s1s2hsp$id))
str(s1s2hsp$id)
head(s1s2hsp$id)

df <- bind_rows(list(s1s2hsp, mt_long), .id = 'sam')

df$unique_id2 <- as.numeric(as.factor(with(df, paste(toupper(id)))))

length(unique(df$id)) # original ids
length(unique(df$unique_id2)) # new ids



################################################################################
####                             PRE-PROCESSING                             ####
################################################################################

# the next few steps will do some filtering that is only possible with the
# identified data, as well as go through a few pre-processing steps to make the
# dataset more user-friendly


#### ID-BASED FILTERING --------------------------------------------------------

### remove test cases (where id is missing or invalid)
# The test cases are instances where one of the researchers went through the 
# study to ensure it was working properly; these are identified by cases that 
# have no id or have invalid ids. 

# remove cases with invalid or missing ids
df <- df %>% filter(id != "99999" & id != "dekubaba" & is.na(id) == F)


# remove identifying ids and sample-specific unique_ids to avoid confusion later
df <- df %>% select(-c(id,unique_id))

# rename id var
names(df)[names(df) == 'unique_id2'] <- 'pID'


#### PRE-PROCESSING --------------------------------------------------------


### in this section i do some clean-up to make the resulting dataset more 
### user-friendly

## update target ids
# now, the first 101 perciever ids overlap with the target ids
# we'll keep the target ids as-is (2:101), but add 200 to the perciever ids

summary(df$pID)
df <- df %>% mutate(pID = pID + 200)

# the target for demographic questions is the perceiver (the participant);
# change from 102 to the pID

summary(df$target_user)
df <- df %>% mutate(target_user = ifelse(target_user == 102, pID, target_user))

## give the questions better names

# because participants could select multiple options for their own race, i'll
# label those separately at the end (because of how the original question labels 
# were parsed, all of the race options show up as question 6 - it didn't
# maintain the 6_1, 6_2, etc., which will end up causing problems later)

# study 1

s1q <- c(seq(4,26,1),28,29)
s1q_lab <-c('outgoing','cold','thorough','nervous','actimag','reserved',
            'trusting','lazy','relaxed','fewartint','trustworthy','selfesteem',
            'funny','assertive','arrogant','intelligent','impulsive',
            'centerattn', 'attractive','ilike','age','sex','race','ses','know')

s1q_demo <- c(seq(2,5,1),8,9,10)        
s1q_demo_lab <- c('sub_sex','sub_age','sub_EngFirstLang','sub_durationEng', 
                  'sub_SES','sub_twitteruse','sub_socMediaUse')


s1ql<- data.frame(s1q,s1q_lab)
s1ql$study = 1
colnames(s1ql)<-c('question','qlabel','study')
s1qld <- data.frame(s1q_demo, s1q_demo_lab)
s1qld$study = 1
colnames(s1qld)<-c('question','qlabel','study')

# study 2
s2q <- seq(4,30,1)
s2q_lab <- c('trustOrganize','followSM','helpConflict','hire', 'friendshipOnline',
             'helpIntellChallenge','movieTVrec','introduceFriend','adviceGoodImpression',
             'romanticPartner_self','triviaPartner','casualSocial','trustInfoOnline',
             'groupProject','buySomethingOnline','interestNewsOnline','askNotes',
             'romanticPartner_friend','converseIntellStim','workTaskConflict',
             'valueOpinion','trustDeadlines','friendsOffline','workStressfulTask',
             'musicRec','paperFeedback','know')

s2q_demo <- c(seq(2,6,1),10,11,12)
s2q_demo_lab <- c('sub_sex','sub_sexOrientation','sub_age','sub_EngFirstLang',
                  'sub_durationEng','sub_SES','sub_twitteruse',
                  'sub_socMediaUse')


s2ql<- data.frame(s2q,s2q_lab)
s2ql$study = 2
colnames(s2ql)<-c('question','qlabel','study')

s2qld <- data.frame(s2q_demo, s2q_demo_lab)
s2qld$study = 2
colnames(s2qld)<-c('question','qlabel','study')


# combine
qlabs <- bind_rows(list(s1ql, s2ql))
qlabs_demo <- bind_rows(list(s1qld, s2qld))

# one issue here is that the demos have the same range of question numbers (1:30)
# as the other survery questions. to facilitate a cleaner join, i add 50 to those
# numbers in both (note that we retain the 'key' variable, which is the original
# question as produced by qualtrics, in case some of this gets mixed up along
# the way.)

summary(qlabs_demo$question)
qlabs_demo <- qlabs_demo %>% mutate(question = question + 50)

summary(df$question)
df <- df %>% mutate(question = ifelse(pID == target_user, question + 50, question))

# combine
qlabs <- bind_rows(list(qlabs, qlabs_demo))

# join to df (105645 obs, 14 vars)

df <- left_join(df, qlabs, by = c('question', 'study'))


## now label race vars

# get appropriate question/label combos (they're the same for all of the 
# surveys; will just get them from the original profiles file)
qlabpairs <- tbl_df(read.csv("Twitter_Profiles___First_100_HSP.csv",
                            header = T, stringsAsFactors = F, nrows = 2))

qlabpairs[, grep("102.6_", colnames(qlabpairs))]

df <- df %>% mutate(qlabel = ifelse(key == 'Q102.6_1','sub_raceBlk',
                                    ifelse(key == 'Q102.6_2','sub_raceAsian',
                                           ifelse(key == 'Q102.6_3','sub_raceHisp',
                                                  ifelse(key == 'Q102.6_4','sub_raceNatAm',
                                                         ifelse(key == 'Q102.6_5','sub_raceWht',
                                                                ifelse(key == 'Q102.6_6','sub_raceOther',qlabel)))))))




qlabpairsS2 <- tbl_df(read.csv("Twitter_Profiles__Behavioral_Affordances_Followup.csv",
                             header = T, stringsAsFactors = F, nrows = 2))

qlabpairsS2[, grep("102.8_", colnames(qlabpairsS2))]


df <- df %>% mutate(qlabel = ifelse(key == 'Q102.8_1','sub_raceBlk',
                                    ifelse(key == 'Q102.8_2','sub_raceAsian',
                                           ifelse(key == 'Q102.8_3','sub_raceHisp',
                                                  ifelse(key == 'Q102.8_4','sub_raceNatAm',
                                                         ifelse(key == 'Q102.8_5','sub_raceWht',
                                                                ifelse(key == 'Q102.8_6','sub_raceOther',qlabel)))))))



summary(as.factor(df$qlabel))

## make variable names more descriptive

names(df)
colnames(df) <- c("sam",'df','start_date','end_date','finished','consent','stim',
                  'orig_quest_num','target_user','question','value','sample',
                  'study','pID','qlabel')





#### write df; this is the file that should be made available.
write.csv(df, 'Personality Impressions on Twitter_052317.csv', row.names = F)

