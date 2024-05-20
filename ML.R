library(caret)
library(doParallel)
library(dplyr)


cl <- makePSOCKcluster(15)
registerDoParallel(cl)


general_df_adni <- readRDS("D:/AZ/adni/objects/df_imputed_not_complete_1073")


set.seed(42)
ind <- sample(2, nrow(general_df_adni), replace = T, prob = c(.7, .3))
training <- as.data.frame(scale(general_df_adni[ind==1,-ncol(general_df_adni),]))
test <- as.data.frame(scale(general_df_adni[ind==2,-ncol(general_df_adni),]))

training <- as.data.frame(scale(general_df_adni[ind==1,vars_oi]))
test <- as.data.frame(scale(general_df_adni[ind==2,vars_oi]))


training$target <- general_df_adni[ind==1,"DIAGNOSIS", drop=T]
test$target <- general_df_adni[ind==2,"DIAGNOSIS", drop=T]


vars_oi <- c("LDELTOTAL", "LIMMTOTAL", "GDMEMORY", "apoe")

control <- trainControl(method="repeatedcv", number=5, repeats=3, 
                        summaryFunction = multiClassSummary,
                        classProbs = TRUE,
                        sampling = "up")


my_glm <- train(target ~ .,
                data = training,
                method = "rpart",
                metric = "ROC",
                #tuneLength = 21,
                #tuneGrid = expand.grid(.mtry=4),
                trControl = control)


predict_unseen <- predict(my_glm, test)
table_mat <- table(test$target, predict_unseen)
cm <- confusionMatrix(table_mat)
cm
varImp(my_glm)
