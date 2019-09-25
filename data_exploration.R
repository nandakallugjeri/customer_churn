churn_list<-read.csv("data/churn_train.csv")
attach(churn_list)
library(dplyr)
library(ggplot2)

churn_list %>% ggplot() + 
  geom_histogram(aes(total_day_minutes))

churn_list %>% ggplot() + 
  geom_histogram(aes(total_day_calls))

churn_list %>% ggplot() + 
  geom_histogram(aes(total_day_charge))

#continue with other variables



#bar plots - churn based on class
ggplot(data=churn_list, aes(international_plan)) +
  geom_bar(aes(fill = churn))

ggplot(data=churn_list, aes(voice_mail_plan)) +
  geom_bar(aes(fill = churn))

ggplot(data=churn_list, aes(state)) +
  geom_bar(aes(fill = churn))

#
churn_list %>% filter(churn=='')%>%
  ggplot( aes(voice_mail_plan)) +
  geom_bar(aes(fill = churn))
