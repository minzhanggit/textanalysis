#### load.libs ####
library(tidyverse)
library(tidytext)

library(SnowballC) # for stemming
library(topicmodels) # for building topic model 
library('ldatuning') # for tuning topic model
library(LDAvis)
library(pequod)# for running simple slope analysis
library(lsr)
library(tm)

# Load Data ####
ML3<-read.csv(file="ML3 Final Data/ML3AllSitesandmTurk.csv",header=TRUE,stringsAsFactors=FALSE)

# Data Preprocessing ####
# originally, essays were stored in two different variables "lowpower", "highpower", we will store it in a single "PowerText" variable #
LowPower<-subset(ML3,ML3$PowerCond=="LowPower")
HighPower<-subset(ML3,ML3$PowerCond=="HighPower")
# this also took out people who did not participate in this study (PowerCond = NA)
LowPower$PowerText<-LowPower$lowpower
HighPower$PowerText<-HighPower$highpower
PowerData<-rbind(LowPower,HighPower)
PowerData$PowerCond<-as.factor(PowerData$PowerCond)
nrow(PowerData)

# how long were the essays #
str(PowerData$PowerText)
PowerData$TextLength<-nchar(PowerData$PowerText,allowNA=TRUE)
hist(PowerData$TextLength)

# Keep only relevant variables #
dat = PowerData %>%
  select(Site,session_id,age,ethnicity, gender,
         PowerCond,PowerText,TextLength,sarcasm,
         AttentionCheck,lowpower_order,highpower_order) 
names(dat)[2]= 'PID' # this the unique ID for each participant
dat$PID = as.character(dat$PID)

save(dat,file ='dat.rdata')

# Topic modeling text preprocessing ----------------------------------------------------------
load(file = 'dat.rdata')
nrow(dat) # 2988

## tokenize & remove stop words ##
poweressay <- dat %>%
  select(PID,PowerText)%>%
  unnest_tokens(word,PowerText)%>% # tokenize
  anti_join(stop_words) # remove stop words


poweressay %>% 
  count(word, sort=T)%>%
  print(n=30)

## get word stems & remove NA ###
poweressay <- poweressay %>% 
  filter(!is.na(word))%>% # remove NAs 
  mutate(wordStemmed = wordStem(word))

## check most frequent words (stems) ##
poweressay%>% 
  count(wordStemmed, sort=T)%>%
  print(n=30)
# stems -> words
unique(subset(poweressay,wordStemmed == 'dai')$word) # day
unique(subset(poweressay,wordStemmed == 'plai')$word) # play
unique(subset(poweressay,wordStemmed == 'monei')$word) # money


## remove common but uninformative word steams ## 
## e.g. words in the writing prompt: power, time, feel, control ##
CommonWordStems = c('power','time','feel','control') 

poweressay <- poweressay %>%
  filter(!wordStemmed %in% CommonWordStems)



## throw out rare words(e.g. typos) ##
## here, define rare words as word frequency == 1 only appeared once## 
wordCount<- poweressay%>% 
  count(wordStemmed, sort=T)%>%
  print(n=30) 
tail(wordCount,n = 20)
sum(wordCount$n == 1) 

poweressay <- poweressay%>%
  filter(!wordStemmed %in% subset(wordCount,n==1)$wordStemmed)

## throw out comments that are too short, because they don't provide enough information for analysis## 
# distribution of comment length
commentLength <- poweressay %>% 
  count(PID,sort = T)

hist(commentLength$n)
commentLength%>%
  filter(n<1000)%>%
  ggplot(aes(n))+
  geom_histogram(bins = 100)

boxplot(commentLength$n,outline = F)

# view comments of different length in the csv file
dat.length <- inner_join(dat,commentLength, by= "PID")
write.csv(with(dat.length,data.frame(PowerText,n)), file = 'comments orded by length.csv')

## set minimum comment length based on distribution and eyeballing the comments
MinCommentLength = 3 
poweressay <- poweressay%>%
  filter(PID %in% subset(commentLength, n >= MinCommentLength)$PID)
length(unique(poweressay$PID)) # 2911

## convert tidy data into document term matrix
dat.complete.dtm <- poweressay %>%
  count(PID,wordStemmed)%>%
  cast_dtm(PID,wordStemmed,n)
dat.complete.dtm

# Determine Number of topics -----------------------------------------------
# FindTopicsNumber calculates 4 different indexes for model fit and topic quality
# see explanation http://www.rpubs.com/MNidhi/NumberoftopicsLDA
result <- FindTopicsNumber(
  dat.complete.dtm,
  topics = seq(from = 2, to = 15, by = 1), # based on experience and rough reading of the essays, unlikely for one essay to have more than 10 topics, set a safe range
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

# visualize different parameters of model fit and topic quality
FindTopicsNumber_plot(result)
# based on the parameters, the optimal number of topics might be 2-4
# try out 2-7 topics

# Build Topic models ----------------------------------------------------------
numTopics <- c(2:7) 

for (i in c(1:length(numTopics))){
  theLDA <- LDA(dat.complete.dtm, k = numTopics[i], method="Gibbs",
                control = list(alpha = 1/numTopics[i],iter=10000,burnin=1000,seed = 1234))
  saveRDS(theLDA,file=paste0('LDA.',numTopics[i],'.rds'))
}

# What are the topics? ----------------------------------------------------
## 1. Top words of the topic ##
## Graphs to show top 10 words in each topic##
for (i in numTopics){
  theLDA <- read_rds(paste0('LDA.',i,'.rds'))
  theTopicsBeta <- tidy(theLDA, matrix = "beta") #beta roughly = how likely a word is used for this topic 
  TopicsTop <- theTopicsBeta %>%
    group_by(topic) %>%
    top_n(10, beta) %>% # get the top 10 words in each topic based on beta
    ungroup() %>%
    arrange(topic, -beta) %>%
    ungroup() %>%
    mutate(x = n():1) 
  
  plTopicWeights <- TopicsTop %>% # plot the top words of all topics
    mutate(topic=factor(topic)) %>%
    ggplot(aes(x=x,y=beta,fill=topic)) +
    geom_bar(stat='identity',show.legend = F) +
    coord_flip() +
    facet_wrap(~topic,scales='free_y') +
    scale_x_continuous(breaks = TopicsTop$x,
                       labels = TopicsTop$term,
                       expand = c(0,0)) +
    ylim(0,0.06)+
    labs(title='Top Words by Topic',
         x = 'word',
         y = 'beta')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
          axis.text.y = element_text(size = 8))
  ggsave(plTopicWeights,
         filename = paste0('Top Words in',i,'.png'))
}
## Output: 4 topics seem to capture distinctive topics the best


## 2. Comments representative of each topic ##

## identify the main topic for each document, based on Gamma  
for (i in numTopics){
  theLDA <- read_rds(paste0('LDA.',i,'.rds'))
  theTopicsGamma <- tidy(theLDA, matrix = "gamma") # gamma roughly = the "percentage" of a topic in this comment, adds up to 100%
  
  comment.classification <- theTopicsGamma%>% # classify each comment based on its most prominant topic
    group_by(document)%>%
    top_n(1,gamma)%>% # for each document, get the topic with the highest gamma
    ungroup()
  
  theTopicsGammaMeta <- comment.classification %>%
    inner_join(with(dat,data.frame(PID,PowerText)),
               by=c("document"="PID"))%>% # order documents by gamma value
    arrange(topic,desc(gamma)) 
  
  write.csv(theTopicsGammaMeta[,],file = paste0(
  i,' Topics_Comments categorized by topic.csv'))
}

## Alternative topic representation with LDAvis -----------------------------------------------------
## In addition to top words, LDAvis represent the distance between topics visually 
## also display other info visually 
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 2) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

numTopics%>%
  map(~ serVis(topicmodels2LDAvis(read_rds(
    paste0('LDA.',.,'.rds')))))


# Using topics for moderation analysis  -----------------------------------
## survey dataset ##
load(file = 'dat.rdata')  
nrow(dat) 

## The four topic topic model ##
theLDA <- read_rds(paste0('LDA.',4,'.rds'))
TopicsGamme4 <- tidy(theLDA, matrix = "gamma")
names(TopicsGamme4)[1] = 'PID'
names(TopicsGamme4) # long, 4 rows (one for each topic) per PPT
nrow(TopicsGamme4) 

# Put together the survey data and the topic model data ####
dat.topic = inner_join(dat,TopicsGamme4,by= 'PID') # join survey and topic model data
# dat.topic is a long dataset where each participant has 4 rows, one for each topic
dat.topic.wide = dat.topic %>%
  spread(topic, gamma) # make it a wide dataset, each participant has one row, and four variables for storing gamma of each topic
names(dat.topic.wide)[13:16]= paste0('Topic',names(dat.topic.wide)[13:16])
## Output ##
save(dat.topic.wide,file ='dat.topic.wide.rdata')

# A detail in classification #####
theTopicsGamma <- tidy(theLDA, matrix = "gamma")

comment.classification <- theTopicsGamma%>%
  group_by(document)%>%
  top_n(1,gamma)%>%  ## for each document, get the topic with the highest gamma ##
  ungroup()

nrow(comment.classification) # 3045, some comments have ties in topics gamma (e.g. two topics has gamma = 0.3)
nrow(comment.classification) - nrow(dat.topic.wide)# repeat ppt rows. 

# use the duplicate.random function to randomly select a row from the ties and, and delete all other. 
duplicated.random = function(x, incomparables = FALSE, ...) 
{ 
  if ( is.vector(x) ) 
  { 
    permutation = sample(length(x)) 
    x.perm      = x[permutation] 
    result.perm = duplicated(x.perm, incomparables, ...) 
    result      = result.perm[order(permutation)] 
    return(result) 
  } 
  else if ( is.matrix(x) ) 
  { 
    permutation = sample(nrow(x)) 
    x.perm      = x[permutation,] 
    result.perm = duplicated(x.perm, incomparables, ...) 
    result      = result.perm[order(permutation)] 
    return(result) 
  } 
  else 
  { 
    stop(paste("duplicated.random() only supports vectors", 
               "matrices for now.")) 
  } 
} 
sum(duplicated.random(comment.classification$document)) 

comment.classification = comment.classification[!duplicated.random(comment.classification$document),]
nrow(comment.classification) 

## Join the comment classification data with survey & topic model data #### 
theTopicsGammaMeta <- comment.classification %>%
  inner_join(with(dat,data.frame(PID,PowerText)),
             by=c("document"="PID"))%>% 
  arrange(topic,desc(gamma)) 
names(theTopicsGammaMeta)[1] = 'PID'
dat.topic.wide.4 = left_join(dat.topic.wide,theTopicsGammaMeta)
## Output ##
save(dat.topic.wide.4,file ='dat.topic.wide.4.rdata')

# Analyses ####
## Who talks about which topic? /When ####
## Analyze topics from the V3 4 topic model 
## Topic 1: Close Relationship: Friend & Families 
## Topic 2: Job: Interview & Evaluation
## Topic 3: School: Student & teacher
## Topic 4: Team Sports: Coach & Player

table(dat.topic.wide.4$topic) # number of comments categorized in each topic

## 	Which topic did participants write about more in High Power/ Low Power conditions ? ##
with(dat.topic.wide.4,table(topic,PowerCond))
dat.topic.wide.4 %>%
  select(Topic1, Topic2, Topic3, Topic4)%>%
  map(~(lm(. ~ PowerCond, data = dat.topic.wide)))%>%
  map(summary)
# Close relationship: More LP
# Job: -
# School: - 
# Team Sports: More HP

## Robustnss checks ##
# Mturk online participants vs. university lab participants #
# is the above finding because most participants are students?
dat.topic.wide.4$mTurk = ifelse(dat.topic.wide.4$Site == 'mTurk','Turk','Uni')
dat.topic.wide.4 %>%
  select(Topic1, Topic4)%>%
  map(~(lm(. ~ mTurk*PowerCond, data = dat.topic.wide.4 )))%>%
  map(summary)

mturk = subset(dat.topic.wide, Site == "mTurk")
nrow(mturk)
mturk %>%
  select(Topic1, Topic4)%>%
  map(~(lm(. ~ PowerCond, data = mturk )))%>%
  map(summary)
# Close relationship and Team Sports effects true in Mturk sample

## 	Which topic did different gender write about ? ##
str(dat.topic.wide.4$gender)
sum(is.na(dat.topic.wide.4$gender))
dat.topic.gender = dat.topic.wide.4%>%
  filter(!is.na(gender))%>%
  mutate(Gender = case_when(
    gender == '1' ~ 'F',
    gender == '2' ~ 'M',
    TRUE ~ 'NA'
  ))

dat.topic.gender = subset(dat.topic.gender ,Gender !='NA')
table(dat.topic.gender$Gender) # more female

table(dat.topic.gender$Gender,dat.topic.gender$mTurk) 
# Mturk is gender balanced, University sample more women
dat.topic.gender %>%
  select(Topic1, Topic2, Topic3, Topic4)%>%
  map(~(lm(. ~ mTurk+Gender*PowerCond, data = dat.topic.gender)))%>% # test for gender difference, controlling for Mturk vs. university difference
  map(anova)

# Gender had effect only for topic 4 
summary(lm(Topic4 ~ mTurk+Gender*PowerCond, data = dat.topic.gender))
# Men writes more about team sports, but no interaction with Power Condition

# Did power manipulation have different effects on perspective taking (sarcasm) in different contexts (topic)? -------------------------------------

anova(lm(sarcasm ~ topic*PowerCond,dat.topic.wide.4)) # treating topic as a categorical variable (topic with highest gamma for each comment)
# treating topic as a continuous variable (gamma)
anova(lm(sarcasm ~ Topic1*PowerCond,dat.topic.wide.4)) 
anova(lm(sarcasm ~ Topic2*PowerCond,dat.topic.wide.4)) 
anova(lm(sarcasm ~ Topic3*PowerCond,dat.topic.wide.4)) #* interaction 
anova(lm(sarcasm ~ Topic4*PowerCond,dat.topic.wide.4)) 

m<-lmres(sarcasm ~ Topic3*PowerCond, centered=c('Topic3'),data = dat.topic.wide.4)
slope<-simpleSlope(m,pred="PowerCondLowPower",mod1="Topic3")
summary(slope) 
# when ppl write more about school: student and teacher context, low power are better at perspective taking than high power. 

anova(lm(sarcasm ~ PowerCond,subset(dat.topic.wide.4, topic == '3'))) #* interaction 
# also true when we test it only in comments categorized in this topic 

# ideas for further analysis ##
# is it due to age difference? #
# sentiment analysis to see if high power or low power comments of this topic show different sentiments than other topics
# read essays to get ideas 
# can this be replicated with other datasets?