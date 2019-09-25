library(gbm)
library(caTools)

#splitting
smp_size <- floor(0.80 * nrow(train.log))

train_ind <- sample(seq_len(nrow(train.log)), size = smp_size)

train <- train.log[train_ind, ]
test <- train.log[-train_ind, ]


#gbm with all vars

myControl<-trainControl(method = "cv", 
                        number = 5, 
                        # repeats = 5, 
                        verboseIter = TRUE,
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE, # IMPORTANT!
                        savePredictions = TRUE,
                        #sampling="smote"
)

model_gbm <- caret::train(churn ~ .,
                          data = train[,!names(train) %in% c("number","area_code")],
                          method = "gbm",
                          preProcess = c("nzv"),
                          trControl = myControl)
model_gbm


gbm.pred<-predict(model_gbm,newdata=test)
confusionMatrix(gbm.pred,test$churn)
mean(rf.pred==testingset$y)
varImp(model_gbm2)

out <- colAUC(gbm.pred, test$churn, plotROC = TRUE)


# results<-resamples(list(GBM=model_gbm,CTREE=model_ctree))
# bwplot(results)


#final list selection

churn_list<-churn_list %>% mutate(number=log(total_intl_calls))
gbm.pred.list<-predict(model_gbm,newdata=churn_list,type="prob")
churn_list$pred<-gbm.pred.list$yes


churn_list_sort<-arrange(churn_list,desc(pred))
final_list<-churn_list_sort[1:300,1]

write.csv(final_list,"final_list.csv")
saveRDS(model_gbm,"Group2_model.rds")
