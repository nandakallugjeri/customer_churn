library(randomForest)
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(party)



churn_train<-read.csv("data/churn_train.csv")
churn_list<-read.csv("data/churn_list.csv")


#check for missing target
which(is.na(churn_train$churn))


#collinearity analysis

num<-unlist(lapply(churn_train, is.numeric))
ch.train.cor<-churn_train[,num]
cor.matrix<-cor(ch.train.cor)
corrplot(cor.matrix, order="hclust", tl.pos='n')
highcor<-findCorrelation(cor.matrix, cutoff=0.9, exact=TRUE, name=TRUE)
#corrplot(cor(ch.train.cor[,-highcor]), order="hclust", tl.pos='n')
highcor

#remove correlated vars
train.cor<-churn_train[,!names(churn_train) %in% highcor]


#histograms

library(reshape2)

ggplot(data = melt(train.cor), aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

train.cor<-train.cor %>% mutate(vmail_flag=ifelse(number_vmail_messages==0,"no","yes"))

str(train.cor)

summary(train.cor$number_vmail_messages)

train.log<-train.cor %>% mutate(number=log(total_intl_calls))

ggplot(data = melt(train.log), aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

train.cor %>% ggplot() + 
  geom_histogram(aes(number_vmail_messages))

train.cor %>% ggplot() + 
  geom_histogram(aes(total_day_calls))

train.cor %>% ggplot() + 
  geom_histogram(aes(total_day_charge))

#continue with other variables



#bar plots - churn based on class
ggplot(data=train.cor, aes(international_plan)) +
  geom_bar(aes(fill = churn))

ggplot(data=train.cor, aes(voice_mail_plan)) +
  geom_bar(aes(fill = churn))

ggplot(data=train.cor, aes(state)) +
  geom_bar(aes(fill = churn))

#
train.cor %>% filter(churn=='')%>%
  ggplot( aes(voice_mail_plan)) +
  geom_bar(aes(fill = churn))


