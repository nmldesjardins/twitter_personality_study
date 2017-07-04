setwd("~/Dropbox/PSD lab/Twitter and personality project/personality impressions study/psych (1)")

tweets <- read_csv("psycho_tweets.csv")
users <- read_csv("psycho_users.csv")
urls <- read_csv("psycho_urls.csv")
ff <- read_csv("psycho_friend_follower.csv")

head(users)
head(urls)

length(unique(urls$tweet_id))
length(unique(tweets$tweet_id))
head(tweets)

urlt<- left_join(urls,tweets, by = 'tweet_id')
head(urlt)

head(ff)

head(ff %>% arrange(friend_id), 10)
head(ff %>% arrange(follower_id), 10)

ff %>% group_by(friend_id) %>% summarize(count = n()) %>% summary()

ff %>% group_by(follower_id) %>% summarize(count = n()) %>% summary()


ff %>% group_by(friend_id) %>% summarize(count = n()) %>% filter(count > 1)
ff %>% filter(friend_id == 813286 | friend_id == 759251 )

ff %>% filter(friend_id == 16715894 & follower_id == 759251) # follower of 759251

ff %>% filter(follower_id == 759251) 


length(unique(ff$friend_id)) # 5006
length(unique(ff$follower_id)) # 5217

users %>% filter(id == 759251) #laurron; follows 759251


ff %>% group_by(friend_id) %>% summarize(count = n()) %>% filter(count == 25) #191
ff %>% group_by(follower_id) %>% summarize(count = n()) %>% filter(count == 25) #188

ff %>% filter(follower_id == 12757222 | friend_id == 12757222) %>% arrange(friend_id)
users %>% filter(id == 12757222)
users %>% filter(id == 24229522)


ff %>% filter(follower_id == 12757222) %>% arrange(friend_id)


ff %>% group_by(friend_id) %>% summarize(count = n()) %>% filter(count == 3) 
ff %>% group_by(follower_id) %>% summarize(count = n()) %>% filter(count == 25) #188


users %>% filter(id == 14824849)
