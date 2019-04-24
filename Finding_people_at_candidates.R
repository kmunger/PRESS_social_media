  ###Finding people
#install.packages("ROAuth")
library(streamR)
library(smappR)
library(ROAuth)
library(tweetscores)
library(dplyr)
setwd("C:/Users/Kevin/Dropbox/experiments/")
load("C:/Users/Kevin/Dropbox/credentials/oauth_token8.Rdata")


today<-Sys.Date()

#today<-"2016-10-19"
##search by dictionary
filterStream(file.name=paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_trump_", today, ".json"), 
             track=c("realdonaldtrump"), 
             tweets=200000, oauth=my_oauth)
Sys.time()


today<-Sys.Date()
##search by dictionary
filterStream(file.name=paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_hillary_", today, "_1.json"), 
             track=c("hillaryclinton"), 
             tweets=200000, oauth=my_oauth)
Sys.time()



##start with TRUMP




#today<-"2016-10-16"
#tweets_t<-parseTweets(paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_trump_2016-09-22.json"))

###decide if trump or hillary

tweets_t<-parseTweets(paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_hillary_", today, "_1.json"))

tweets_t<-parseTweets(paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_trump_", today, ".json"))

replies<-filter(tweets_t, in_reply_to_status_id_str!="NA")

##need to filter to recent ones bc I doubled up   



###take over to python
#today<-"2016-10-16"

  
  write.csv(replies$text, paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_hillary_", today, ".csv"), 
            fileEncoding ="UTF-8")


write.csv(replies$text, paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_trump_", today, ".csv"), 
          fileEncoding ="UTF-8")




replies$agg<- read.csv(paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/agg_graded_", today, ".csv"), fileEncoding ="UTF-8",
                       header=FALSE)
replies$agg<-replies$agg$V1
#trump_ids<-replies$user_id_str

replies_sorted<-replies[with(replies, order(-agg)),]

##filter out trump responses
##25073877 is trump's user id str--but it looks like this isn't the move, too many ppl ---actually it is!!



replies_not_dt<-filter(replies_sorted, in_reply_to_user_id_str!="25073877")

head(replies_sorted)

### 1339835893 is hrc

replies_not_hrc<-filter(replies_sorted, in_reply_to_user_id_str!="1339835893")




##uniquify responses--should restrict the number of elite mentions
replies_unique<-subset(replies_not_hrc, !duplicated(in_reply_to_user_id_str)) 


replies_unique<-subset(replies_not_dt, !duplicated(in_reply_to_user_id_str)) 



##display the relevant URLS
?browseURL

k<-length(replies_unique$text)

urls<-character(k)
for (i in 1:k){
  name<-gsub(".json", "", replies_unique$screen_name[i])
  
  id<-replies_unique$id_str[i]
  urls[i]<-paste0("https://twitter.com/", name,"/status/", id)
}

for(i in 1:40){
  browseURL(urls[i])
  
}

##for the beginning, I just need to assign them to one of two relevant bots, and the right language

##then see next R file for re-scraping





















##get covariates for block randomization--get 100 of these ppl


##so the first step is getting a bunch of possible candidates and then I have to hand code some stuff



get_covariates<-getUsersBatch(screen_names =  replies_not_dt$screen_name[1:1000], oauth_folder =  "C:/Users/Kevin/Dropbox/credentials/")

##filter for accounts that are less old

get_covariates$created_at<-formatTwDate(get_covariates$created_at)

get_covariates<-filter(get_covariates,  created_at<as.POSIXct("2016-05-01"))

##need to incorporate tweet IDS
tweet_info_for_get_covariates<-replies_not_dt[replies_not_dt$screen_name %in% get_covariates$screen_name,]

tweet_info_for_get_covariates<-subset(tweet_info_for_get_covariates,!duplicated(tweet_info_for_get_covariates$screen_name))

##need to merge, set up small df

merge_df <- tweet_info_for_get_covariates[c("id_str", "screen_name")]


get_covariates<-merge(get_covariates, merge_df, by="screen_name")

get_covariates$tweet_id_str<-get_covariates$id_str.y

##make a separate doc for me to check

write.csv(get_covariates, paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_trump_get_covariates", today, ".csv"))

##restrict replies

k<-length(get_covariates$tweet_id_str)

urls<-character(k)
for (i in 1:k){
  name<-gsub(".json", "", get_covariates$screen_name[i])
  
  id<-get_covariates$tweet_id_str[i]
  urls[i]<-paste0("https://twitter.com/", name,"/status/", id)
}

##browse URLS

for(i in 1:344){
  browseURL(urls[i])
  
}

##read in the evaluated people

evaluated<-read.csv("C:/Users/kevin/Dropbox/experiments/Tsubjects/at_trump_get_covariates2016-09-15_done.csv")

evaluated<-filter(evaluated, keep==1)

for(i in 1:length(evaluated$screen_name)){
  
  getTimeline(filename=paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/", evaluated$screen_name[i], ".json"), screen_name=evaluated$screen_name[i], 
              n=1000, oauth_folder="C:/Users/Kevin/Dropbox/credentials/")
}


tweets_t<-parseTweets(paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/", evaluated$screen_name[1], ".json"))

for(i in 2:length(evaluated$screen_name)){
  
  tweets_t<-rbind(tweets_t,parseTweets(paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/", evaluated$screen_name[i], ".json")))
  
}

agg_df<-tweets_t[c("text", "user_id_str")]



###take over to python
write.csv(agg_df, paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/at_trump_histories.csv"), 
          fileEncoding ="UTF-8")

agg_df$agg<- read.csv(paste0("C:/Users/Kevin/Dropbox/experiments/Tsubjects/agg_graded_histories_scores.csv"), fileEncoding ="UTF-8",
                       header=FALSE)
agg_df$agg<-agg_df$agg$V1

##aggregate aggression scores by user

user_scores<- aggregate(x = agg_df$agg, 
                               by = list(unique.values = agg_df$user_id_str), 
                               FUN = mean)

evaluated$user_score<-user_scores$x


#######
evaluated$ideology<-rep(0, length(evaluated$screen_name))

##convert to character, gotta be careful with this
evaluated$screen_name<-as.character(evaluated$screen_name)



for(i in 1:length(evaluated$screen_name)){
  
  possibleError <- tryCatch(
    friends<-getFriends(evaluated$screen_name[i], oauth_folder="C:/Users/Kevin/Dropbox/credentials/" ),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")){next}
  friends<-getFriends(evaluated$screen_name[i], oauth_folder="C:/Users/Kevin/Dropbox/credentials/" )
  
  possibleError <- tryCatch(
    evaluated$ideology[i]<-estimateIdeology2(evaluated$screen_name[i], friends=friends ),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")){next}
 
}

evaluated$ideology[is.finite(evaluated$ideology)==FALSE]<-0

##############now we need to do the prognostic scores

##log friends count

evaluated$friends_count<-log(evaluated$friends_count )
evaluated$friends_count[is.finite(evaluated$friends_count)==FALSE]<-0


evaluated$friends_count


summary(lm(user_score ~ friends_count + info_profile + woman + nonwhite +ideology, data=evaluated ))



####################################################################################################
##just testing this twitter conspiracy
for (i in 1:10){
  
  error <- tryCatch( getTimeline(filename=paste0("C:/Users/Kevin/Dropbox/experiments/dem_subjects/",replies_sorted$screen_name[i], ".json"),
                                 screen_name=replies_sorted$screen_name[i], 
                                 n=10, oauth_folder="C:/Users/Kevin/Dropbox/credentials/"),
                     error = function(e) e)
  # if error is found, go to next loop iteration
  if (inherits(error, 'error')){  next }
  
  
}
test_tweets<-list()
for (i in 1:10){
  
  test_tweets[[i]]<-parseTweets(paste0("C:/Users/Kevin/Dropbox/experiments/dem_subjects/", replies_sorted$screen_name[i], ".json"))
  
}
test_tweets[[5]]$text
