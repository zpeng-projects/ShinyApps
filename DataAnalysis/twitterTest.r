
rm(list=ls())

library(devtools)
library(twitteR)
library(ggplot2)

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

<<<<<<< HEAD
#For this we need a list of words that contains positive and 
#negative sentiment words. I have downloaded the list from Google 
#and it is easily available.
txt<- VectorSource(text.d)
txt.corpus<-Corpus(txt)
inspect(txt.corpus)
txt.corpus<-tm_map(txt.corpus,tolower)
txt.corpus<-tm_map(txt.corpus,removePunctuation)
txt.corpus<-tm_map(txt.corpus,removeNumbers)
txt.corpus<-tm_map(txt.corpus,removeWords,stopwords("english"))

#Next do stemming: load the ‘SnowballC’ package (Bouchet-Valat, 2013) which
#allows us to identify specific stem elements using the ‘tm
#map’ function of the ‘tm’ package
library(snowballC)
txt.corpus<-tm_map(txt.corpus,stemDocument)
detach("package:snowballC")
inspect(txt.corpus)

txt.corpus<-tm_map(txt.corpus,stripWhitespace)
inspect(txt.corpus)

#create something called a Term Document Matrix (TDM),
#hich is a matrix of frequency counts for each word used in the corpus
tdm<-TermDocumentMatrix(txt.corpus)
inspect(tdm[1:20,])

#Next, we can begin to explore the TDM, using the ‘findFreqTerm
#s’ function, to find which words were used most.
findFreqTerms(x=tdm,lowfreq=8,highfreq=Inf)

#Next, we can use the ‘findAssocs’ function to find words which 
#associate together. Here, we are specifying the TDM to use,
#the term we want to find associates for, and the lowest 
#acceptable correlation limit
# This returns a vector of terms which are associated with 
#‘comput’ at = 0.60 or more (correlation) – and reports each 
#association in descending order.
findAssocs(x=tdm,term="comput",corlimit=0.6)

#If desired, terms which occur very infrequently (i.e. sparse terms)
#can be removed; leaving only the ‘common’ terms. Below, the
#‘sparse’ argument refers to the MAXIMUM sparse-ness allowed for
#a term to be in the returned matrix; in other words, the larger 
#the percentage, the more terms will be retained(the smaller the 
#percentage, the fewer [but more common] terms will be retained
tdm.com.60<-removeSparseTerms(x=tdm,sparse=0.6)

##-------------------------------
library(twitteR)
delta.tweets<-searchTwitter("@delta",n=1500)


=======

##----------------------------
delta.tweets<- searchTwitter("@delta",n=100)
df<-twListToDF(delta.tweets)
delta.text<-df$text;

hu.liu.pos<-scan("positive-words.txt", what="character", comment.char=";")
hu.liu.neg<-scan("negative-words.txt", what="character", comment.char=";")

until_day<-as.character(Sys.Date()-1)
a<-twListToDF(searchTwitter('NQ', n=100000,since='2011-03-01', until=until_day,lang="en"))
nrow(a)
min(as.Date(a[,5]))
plot(a[,5])
hist(a[,5],breaks=20)
>>>>>>> b6cb7b81543e9b245698158f70b8c62b6df5f25e
