
library(httr)
library(streamR)
library(devtools)
library(Rcpp)
library(ROAuth)
library(quanteda)
library(smappR)
library(dplyr)
library(twitteR)
library(base64enc)
library(jsonlite)
library(tweetscores)
library(foreign)
library(curl)
?install_github
today<-Sys.Date()
load("C:/Users/Kevin/Dropbox/credentials/oauth_token8.Rdata")

data<-read.csv("C:/Users/Kevin/Dropbox/experiments/subjects.csv", stringsAsFactors = F)
data<-filter(data, username!="")




##pre-scraping--by day
data$date<-as.Date(data$date)
data$username<-gsub(" ", "", data$username)
data$name_no_at<-gsub("@", "", data$username)
data_today<-filter(data, date==today)



users_to_save<-getUsersBatch(screen_names= data_today$username, oauth_folder =  "C:/Users/Kevin/Dropbox/credentials/")

write.csv(users_to_save, paste0("C:/Users/Kevin/Dropbox/experiments/pol_subjects/profiles", today, ".csv"), 
          fileEncoding ="UTF-8")


##gotta get rid of spaces


##for some reason this keeps breaking????--DON'T ONLY PUT IN ONE SCREEN NAME!!!!
for (i in 1:length(data_today$username)){
  
  getTimeline(filename=paste0("C:/Users/Kevin/Dropbox/experiments/pol_subjects/", today, "/", data_today$username[i], ".json"), 
              screen_name=data_today$username, 
              n=3200, oauth_folder="C:/Users/Kevin/Dropbox/credentials/")
  
  
}




###let's scrape the bots; for some reason it didn't work to put in all 4 at once, needed to do it one at a time??
bots<-c("MatthewDavis245","NeilHarris158","ToddRobinson157","BrettJones946")

for (i in 1:4){
  
  getTimeline(filename=paste0("C:/Users/Kevin/Dropbox/experiments/pol_subjects/bots/", bots[i], ".json"), 
              screen_name=bots[i], 
              n=200, oauth_folder="C:/Users/Kevin/Dropbox/credentials/")
  
  
}




###now I gotta check their ideology!! 
data$ideology<-rep(NA, length(data$username))
#data$interest<-rep(NA, length(data$username))

#got stopped at 142

for(i in 142:length(data$username)){
  possibleError <- tryCatch(
    friends<-getFriends(screen_name=data$name_no_at[i], oauth_folder="C:/Users/Kevin/Dropbox/credentials/")
    ,
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")){
next  }
  
  print(i)
  possibleError <- tryCatch(
    results<-estimateIdeology2(data$name_no_at[i] , friends),
    error=function(e) e
  )
  if(inherits(possibleError, "error")){
next
  }
  
  
  
  
  data$ideology[i]<-results
#  data$interest[i]<-results$Rhat['beta']
  sleep=1
}


?estimateI
results[1]$samples

?estimateIdeology
table(data$ideology)





###now I gotta check for balance

data$treatment<-data$feelings
data$treatment[data$proper.rules.==1]<-2
data$treatment[data$public.==1]<-3

data$anon<-data$Anonymity..nothing..2..everyting..0.

trump<-filter(data, X.Matthew.Trump==1)
R<-filter(data, X.Todd.R==1)
D<-filter(data, X.Brett.D==1)
hill<-filter(data, X.Neil.Hillary==1)
control<-filter(data, control==1)

##check for balance

table(trump$treatment)/length(trump$treatment)
table(R$treatment)/length(R$treatment)
table(D$treatment)/length(D$treatment)
table(hill$treatment)/length(hill$treatment)

table(trump$anon, trump$treatment)
table(R$anon, R$treatment)
table(D$anon, D$treatment)
table(hill$anon, hill$treatment)












############let's try w less trycatch: rescrape Nov 9

today<-"rescrape_nov23"

for (i in 1:length(data$name_no_at)){
  
  possibleError <- tryCatch(
    getTimeline(filename=paste0("C:/Users/kevin/Dropbox/experiments/pol_subjects/post/", today, "/", data$name_no_at[i], ".json"), 
                screen_name=data$name_no_at[i], 
                n=3200, oauth_folder="C:/Users/Kevin/Dropbox/credentials/", verbose=FALSE, trim_user=TRUE),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")){
    #REAL WORK
      next}
  Sys.sleep(2)
  print(i)
}


##so apparently I 
i<-1
for (i in 1:length(data$name_no_at)){
  
  possibleError <- tryCatch(
    getTimeline(filename=paste0("C:/Users/Kevin/Dropbox/experiments/post/", today, "/", data$name_no_at[i], "_all.json"), 
                screen_name=data$name_no_at, 
                n=200, oauth_folder="C:/Users/Kevin/Dropbox/credentials/", verbose=TRUE, trim_user=TRUE),
    error=function(e) e
  )
  
  if(!inherits(possibleError, "error")){
    #REAL WORK
    getTimeline(filename=paste0("C:/Users/Kevin/Dropbox/experiments/post/", today, "/", data$name_no_at[i], "_all.json"), 
                screen_name=data$name_no_at, 
                n=400, oauth_folder="C:/Users/Kevin/Dropbox/credentials/", verbose=TRUE, trim_user=TRUE)}
  
  print(i)
}



