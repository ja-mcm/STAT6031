library(data.table)
library(caret)
library(MASS)

### For reproducibility
set.seed(111)





#### SOME BOILERPLATE CODE TO COMPARE OLS AGAINST ROBUST REGRESSION
# Get rid of high dimensional dummy variables, just for convenience
final_data[,c("id","host_location", "host_response_rate", "host_acceptance_rate", "latitude","longitude", "amenities_list", "last_review", "host_neighbourhood", "neighbourhood_cleansed"):=NULL]


#### TRAIN MODEL
### Set up train/test split
tt_split <- createDataPartition(
  y = final_data$price,
  p = .80, ## The percentage of data in the training set
  list = FALSE
)

data_train <- final_data[ tt_split,]
data_test  <- final_data[-tt_split,]

### Run 2 full regression models (robust vs regular LS), for comparison
lm_1 <- lm(log(price)~.,data=data_train)
lm_1_robust <- rlm(log(price)~.,data=data_train)

summary(lm_1)
summary(lm_1_robust)

# Robust regression has lower training RMSE:
# .387 vs .4468

# TRAINING predictions 
train_preds <- cbind(exp(lm_1$fitted.values), exp(lm_1_robust$fitted.values), exp(lm_1$model[1])) |> as.data.table()
head(train_preds, 30)


### Cross validation for improving robustness of training 
instruct_cv <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

model_cv_lasso <- train(log(price) ~ .,
                  data = data_train,
                  preProc = c("center", "scale"), ## pre-processing of variables
                  method = "lasso",  # now we're using the lasso method
                  na.action=na.exclude,
                  trControl = instruct_cv)  

model_cv_boostlm <- train(log(price) ~ .,
                          data = data_train,
                          preProc = c("center", "scale"), ## pre-processing of variables
                          method = "BstLm", 
                          na.action=na.exclude,
                          trControl = instruct_cv)  

model_cv_lm <- train(log(price) ~ .,
                          data = data_train,
                          preProc = c("center", "scale"), ## pre-processing of variables
                          method = "lm", 
                          na.action=na.exclude,
                          trControl = instruct_cv)  

model_cv_rf <- train(log(price) ~ .,
                     data = data_train,
                     preProc = c("center", "scale"), ## pre-processing of variables
                     method = "rf", 
                     na.action=na.exclude,
                     trControl = instruct_cv)  

#### TEST MODEL
# Use 20% of data to evaluate goodness of fit for each candidate model
lm_1_pred <- predict(lm_1,data_test)
lm_1_robust_pred <- predict(lm_1_robust,data_test)
cv_1_lasso_pred <- predict(model_cv_lasso,data_test)
cv_1_boost_pred <- predict(model_cv_boostlm,data_test)
cv_1_lm_pred <- predict(model_cv_lm,data_test)
cv_1_rf_pred <- predict(model_cv_lm,data_test)

### Compare TEST predictions
test_preds <- cbind(exp(lm_1_pred), 
                    exp(lm_1_robust_pred), 
                    1, 
                    1,
                    1,
                    1,
                    data_test$price) |> as.data.table()
test_preds <- test_preds[!is.na(V1)]
setnames(test_preds, c("LM", "LM_ROBUST", "LASSO", "LM_BOOST", "LM_CV", "RAND_FOREST", "ACTUAL"))
test_preds[,LASSO:=exp(cv_1_lasso_pred)]
test_preds[,LM_BOOST:=exp(cv_1_boost_pred)]
test_preds[,LM_CV:=exp(cv_1_lm_pred)]
test_preds[,RAND_FOREST:=exp(cv_1_rf_pred)]

head(test_preds, 30)


### RMSE of predictions
# Based on test predictions, the lasso method wins!
test_preds[!is.na(LM), sqrt(sum((LM-ACTUAL)^2/.N))] #LM (213.84)
test_preds[!is.na(LM), sqrt(sum((LM_ROBUST-ACTUAL)^2/.N))] #LM_ROBUST (238.95)
# Add other models
test_preds[!is.na(LM), sqrt(sum((LASSO-ACTUAL)^2/.N))] #LASSO (209.79)
test_preds[!is.na(LM), sqrt(sum((LM_BOOST-ACTUAL)^2/.N))] #LM_BOOST (192.2457)
test_preds[!is.na(LM), sqrt(sum((LM_CV-ACTUAL)^2/.N))] #LM (213.84)
test_preds[!is.na(LM), sqrt(sum((LM_CV-ACTUAL)^2/.N))] #RAND_FOREST (  )

# off by $210, on average....



