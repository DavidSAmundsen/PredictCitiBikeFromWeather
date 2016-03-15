library(dplyr)
library(lubridate)
library(caret)

load(file = "data/data_frames.dat")

al_grid <- expand.grid(.alpha = c(0, 0.1, 0.5, 0.7, 1), 
                       .lambda = seq(0, 20, by = 0.1))
ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

lm_fit <- train(numtrips ~ .,
                data = df_train,
                method = "glmnet", 
                tuneGrid = al_grid,
                metric = "RMSE",
                trControl = ctrl)

pr_lm <- predict(lm_fit, newdata = df_test)
RMSE(pr_lm, df_test$numtrips)
varImp(lm_fit)

###
# Regression tree
###

rtGrid <- expand.grid(cp = seq(0.001, 0.2, by = 0.005))
ctrl <- trainControl(method = "cv", number = 10)
rt_fit <- train(numtrips ~ ., data = df_train,   
                method = "rpart", 
                tuneGrid = rtGrid,
                metric='RMSE',
                trControl = ctrl)

plot(rt_fit$finalModel)
# text(rt_fit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

varImp(rt_fit, scale = TRUE)
pr_rt <- predict(rt_fit, newdata = df_test)
RMSE(pr_rt, df_test$numtrips)

###
# Random forest
###

rfGrid <- expand.grid(mtry = 10)
ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
rf_fit <- train(numtrips ~ ., data = df_train,   
                method = "rf", 
                tuneGrid = rfGrid,
                metric = "RMSE",
                trControl = ctrl)
plot(rf_fit$finalModel)

pr_rf <- predict(rf_fit, newdata = df_test)
RMSE(pr_rf, df_test$numtrips)

save(lm_fit, pr_lm, rt_fit, pr_rt, rf_fit, pr_rf, file = "data/models.dat")