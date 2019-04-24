##check attrition
library(dplyr)

weak_subjects<-read.csv("C:/Users/kevin/Dropbox/experiments/diagnostic_plots/weak_subjects_real.csv", stringsAsFactors = F)

subjects<-read.csv("C:/Users/Kevin/Dropbox/experiments/subjects.csv", stringsAsFactors = F, encoding = "UTF-8")

data<-read.csv("C:/Users/Kevin/Dropbox/experiments/divided_subjects.csv", stringsAsFactors = F, encoding = "UTF-8")

load("C:/Users/kevin/Dropbox/experiments/pol_subjects/tweetment_time.RData")


kept_subjects<-read.csv("C:/Users/Kevin/Dropbox/experiments/subjects_all_coded.csv", stringsAsFactors = F, encoding = "UTF-8")

##which ones aren't good-- need to get original #'s
table(subjects$Control)

##combine and factorize bots


subjects$candidate<-subjects$Trump
subjects$candidate[subjects$Repub==1]<-2
subjects$candidate[subjects$Clinton==1]<-3
subjects$candidate[subjects$Dem==1]<-4
subjects$candidate[subjects$Control==1]<-0



subjects$rightist[subjects$candidate ==1 | subjects$candidate ==2]<-1
subjects$rightist[subjects$candidate ==3 | subjects$candidate ==4]<-0

##
table(subjects$rightist)
table(subjects$Control)


table(subjects_active$rightist & subjects_active$Control ==0)

##rightists: 118 to 108 (2 by my error)

table(subjects_active$leftist & subjects_active$Control ==0)

##leftists: 104 to 100 (2 by my error)

##Control: 108 to 102



#####################break it down

subjects_326<-read.csv("C:/Users/Kevin/Dropbox/experiments/subjects_all_coded.csv", stringsAsFactors = F, encoding = "UTF-8")

subjects$username<-gsub("@", "", subjects$username )

subjects$username<-gsub(" ", "", subjects$username )

full_ppl<-subjects$username

ppl_326<-subjects_326$name_no_at


dropped_4<-full_ppl[full_ppl %in% ppl_326 ==FALSE]

###

subjects$Control[subjects$username %in% dropped_4 == TRUE]

subjects$rightist[subjects$username %in% dropped_4 == TRUE]


##so there were 2 rightists and 2 leftists dropped by mistakes in the tweeting

##############################drilling down into the weak subjects
too_many<-filter(weak_subjects, too_many_bot==1)
too_many_ppl<-too_many$ppl

subjects$Control[subjects$username %in% too_many_ppl == TRUE]

##3  control dropped

subjects$rightist[subjects$username %in% too_many_ppl == TRUE]

###5 rightists and 1 leftist dropped



############suspended
suspended<-filter(weak_subjects, suspended==1)
suspended_ppl<-suspended$ppl

subjects$Control[subjects$username %in% suspended_ppl == TRUE]

##0 control dropped

subjects$rightist[subjects$username %in% suspended_ppl == TRUE]

###2 rightist and 1 leftist suspended


############weird
weird<-filter(weak_subjects, weird==1)
weird_ppl<-weird$ppl

subjects$Control[subjects$username %in% weird_ppl == TRUE]

##0 control dropped

subjects$rightist[subjects$username %in% weird_ppl == TRUE]

###2 rightist and 1 leftist weird

