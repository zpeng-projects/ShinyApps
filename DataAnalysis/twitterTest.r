
rm(list=ls())

library(devtools)
library(twitteR)

api_key <- "HJB9l39OhH7XKqr6deYHROft6"

api_secret <- "l4ZyUExemzqPnEeD5qtw5aHuuN8oSMCOe7pNSNaE7lTVHzuGYI"

access_token <- "2534082073-qlC6nlEgvmHb1zzJCqDhZhtIqIAYgZZBDJcOVra"

access_token_secret <- "GSdNj3Clasqm4Ub5Nl647y8cuafphvPi5NpcHkdasFk0p"

# consumer k: HJB9l39OhH7XKqr6deYHROft6
# Cons. secret: l4ZyUExemzqPnEeD5qtw5aHuuN8oSMCOe7pNSNaE7lTVHzuGYI
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
searchTwitter("iphone")

tweets<-searchTwitter("#rstats",n=50)
head(strip_retweets(tweets,strip_manual=TRUE,strip_mt=TRUE))

crantastic<-getUser("crantastic")
crantastic$getDescription()
crantastic$getFollowersCount()
crantastic$getFriends()
crantastic$getFavorites()

df<-twListToDF(tweets)


# Twitter timeline is simply a stream of tweets. We support two timelines,
# the user timeline and the home timeline. The former provides the most recent
# tweets of a specified user while the latter is used to display your own most recent
# tweets. These both return a list of status objects

cran_tweets<-userTimeline('cranatic')

# examples

r_tweets<-searchTwitter("#rstats",n=100)
sources<-sapply(r_tweets,function(x)x$getStatusSource())
sources<-gsub("</a>","",sources)
sources<-strsplit(sources,">")
sources<-sapply(sources,function(x)ifelse(length(x)>1,x[2],x[1]))
source_table=table(sources)
pie(source_table[source_table>10])

For this we need a list of words that contains positive and negative sentiment words. I have downloaded the list from Google and it is easily available.