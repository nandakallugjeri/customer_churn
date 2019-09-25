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

gbm.pred<-predict(model_gbm,newdata=test,type="prob")
table(gbm.pred,test$churn)
confusionMatrix(gbm.pred,test$churn)
mean(rf.pred==testingset$y)
varImp(model_gbm2)

out <- colAUC(gbm.pred, test$churn, plotROC = TRUE)


# results<-resamples(list(GBM=model_gbm,CTREE=model_ctree))
# bwplot(results)
