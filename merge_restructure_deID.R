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
                extract(key,c('user','question'),"Q([:digit:]+).([:digit:]+)", 
                remove=F)

## drop extra instruction items
# qn.1 = instructions; qn.2 = profile picture; qn.3 = 'this person...'
# qn.27 = ladder instructions
# type_convert converts dtypes from chr to more useful types
# keep all user 102 qs (these are participant demographics)

hsp_long <- hsp_long %>% type_convert() %>%
                filter(question > 3 | user == 102) %>% filter(question != 27)

summary(as.factor(hsp_long$question))
summary(as.factor(hsp_long$user))

## Create a unique user id
hsp_long$unique_id <- as.numeric(as.factor(with(hsp_long, paste(id))))

length(unique(hsp_long$id))
length(unique(hsp_long$unique_id))

# remove the sona id
hsp_long <- subset(hsp_long, select = -c(id))

# identify the sample
hsp_long$sample = 'hsp'


# MTURK DATA -----------------------------------------------------

## read in the data from the original qualtrics files

# profiles
header <- tbl_df(read.csv("Twitter_Profiles___First_100.csv",
                          nrows=1))
prof_hsp <- tbl_df(read.csv("Twitter_Profiles___First_100.csv",
                            header = F, stringsAsFactors = F, skip = 2))
names(prof_hsp) <- names(header)
prof_hsp$stim = 'profile'

# friends
frheader <- tbl_df(read.csv("Twitter_Friends___First_100___MTURK.csv",
                            nrows=1))
fri_hsp <- tbl_df(read.csv("Twitter_Friends___First_100___MTURK.csv",
                           header = F, stringsAsFactors = F, skip = 2))
names(fri_hsp) <- names(frheader)
fri_hsp$stim = 'friends'

# followers
foheader <- tbl_df(read.csv("Twitter_Followers_First_100_MTURK.csv",
                            nrows=1))
fol_hsp <- tbl_df(read.csv("Twitter_Followers_First_100_MTURK.csv",
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
        extract(key,c('user','question'),"Q([:digit:]+).([:digit:]+)", 
                remove=F)

## drop extra instruction items
# qn.1 = instructions; qn.2 = profile picture; qn.3 = 'this person...'
# qn.27 = ladder instructions
# type_convert converts dtypes from chr to more useful types
# keep all user 102 qs (these are participant demographics)

hsp_long <- hsp_long %>% type_convert() %>%
        filter(question > 3 | user == 102) %>% filter(question != 27)

summary(as.factor(hsp_long$question))
summary(as.factor(hsp_long$user))

## Create a unique user id
hsp_long$unique_id <- as.numeric(as.factor(with(hsp_long, paste(id))))

length(unique(hsp_long$id))
length(unique(hsp_long$unique_id))

# remove the sona id
hsp_long <- subset(hsp_long, select = -c(id))

# identify the sample
hsp_long$sample = 'hsp'

