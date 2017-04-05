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

# these are mostly qualtrics-generated variables (like timing, location), and
# variables that just indicate the user saw a page (e.g., instructions, debrief)
# i did these separately because the variable names are slightly different for 
# each survey because of how they were created in qualtrics

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

# followers
fol_hsp <- subset(fol_hsp, select = -c(Q5960_1:Q5960_4, Q3026, Q3028,
                                         LocationLatitude,LocationLongitude,
                                         LocationAccuracy,V1:V7, Q3033,
                                         Q1.1, Q102.1, Q102.7, 
                                        Q102.13, X, Q102.15))
fol_hsp <- fol_hsp[, -grep(".30_", colnames(fol_hsp))]

length(names(prof_hsp))
length(names(fri_hsp))
length(names(fol_hsp))


## merge all three together

# first check the age variables (won't merge if different types)
str(fol_hsp[,grep("..24", colnames(fol_hsp))])
str(fri_hsp[,grep("..24", colnames(fri_hsp))])
str(prof_hsp[,grep("..24", colnames(prof_hsp))])

age_vars = c('Q52.24','Q53.24','Q54.24','Q55.24','Q56.24','Q71.24')
prof_hsp[,age_vars] <- as.integer(unlist(prof_hsp[,age_vars]))


# now bind

hsp <- bind_rows(list(prof_hsp, fri_hsp, fol_hsp), .id = 'df')


### Restructure 

# v10 = finished survey (1 if did)
# q1.2 = consent (should be 1)
# questions start on qn.4

hsp_long <- hsp %>%
                gather(key, value, -V8, -V9, -V10, -id, -Q1.2, -stim, -df, na.rm=T) %>%
                extract(key,c('target_user','question'),"Q([:digit:]+).([:digit:]+)", 
                remove=F)

## drop extra instruction items
# qn.1 = instructions; qn.2 = profile picture; qn.3 = 'this person...'
# qn.27 = ladder instructions
# type_convert converts dtypes from chr to more useful types
# keep all user 102 qs (these are participant demographics)

hsp_long <- hsp_long %>% type_convert() %>%
                filter(question > 3 | target_user == 102) %>% 
                filter(question != 27)

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

dec_long <- dec_long %>% type_convert() %>% filter(question > 3 | target_user == 102) 

# identify the sample + study
dec_long$sample = 'hsp'
dec_long$study = 2
dec_long$df = 4

# Merge S1 & S2 ----
s1s2hsp <- bind_rows(hsp_long,dec_long)


## Create a unique user id
s1s2hsp$unique_id <- as.numeric(as.factor(with(s1s2hsp, paste(id))))

length(unique(s1s2hsp$id))
length(unique(s1s2hsp$unique_id))

# remove the sona id - do this at the very end, just to make sure everything
# is as it should be once the mturkers pop in
# hsp_long <- subset(hsp_long, select = -c(id))

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


# followers
fol_mt <- subset(fol_mt, select = -c(Q5960_1:Q5960_4, Q3026, Q3028, Q3021,
                                       LocationLatitude,LocationLongitude,
                                       LocationAccuracy,V1:V7,
                                       Q1.1, Q102.1, Q102.7,
                                       Q102.13, X, Q102.15))
fol_mt <- fol_mt[, -grep(".30_", colnames(fol_mt))]
names(fol_mt)[names(fol_mt) == 'Q102.11'] <- 'id'

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
        filter(question > 3 | target_user == 102) %>% filter(question != 27)

summary(as.factor(mt_long$question))
summary(as.factor(mt_long$target_user))

# identify sample
mt_long$sample = 'mturk'


## Create a unique user id
mt_long$unique_id <- as.numeric(as.factor(with(mt_long, paste(id))))

length(unique(mt_long$id))
length(unique(mt_long$unique_id))



# COMBINE MTURK & HSP ----------------------------------------------------------
hsp_long$id <- as.character(hsp_long$id)  
str(hsp_long$id)
head(hsp_long$id)

df <- bind_rows(list(hsp_long, mt_long), .id = 'sam')

df$unique_id2 <- as.numeric(as.factor(with(df, paste(id, sample))))

length(unique(df$id))
length(unique(df$unique_id2))

View(df)

# 581:584 are likely duds; one is the mturk code, one is an email address, 
# and 2 are NAs

# remove the sona id
hsp_long <- subset(hsp_long, select = -c(id))
