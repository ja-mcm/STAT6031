library(data.table)
library(caret)
library(MASS)
library(glmnet)
library(boot)


### For reproducibility
set.seed(111)

# Get rid of high dimensional dummy variables, just for convenience
final_data[,c("id", "host_location", "amenities_list", "neighbourhood_cleansed", "host_response_rate", "host_acceptance_rate", "latitude", "longitude"):=NULL]

# Get rid of rows with NAs
final_data <- na.omit(final_data)

# Create Dummy Variables
inds <- unique(final_data$host_response_time)
final_data[, (inds) := lapply(inds, function(x) {ifelse(host_response_time == x, 1, 0)})]
final_data[,host_response_time:=NULL] ### Drop original column
final_data[,`a few days or more`:=NULL] ### drop one dummy column, to avoid perfect collinearity

inds <- unique(final_data$room_type)
final_data[, (inds) := lapply(inds, function(x) {ifelse(room_type == x, 1, 0)})]
final_data[,room_type:=NULL] ### Drop original column
final_data[,`Shared room`:=NULL] ### drop one dummy column, to avoid perfect collinearity


final_data[,log_price := log(price)]
final_data[,price:=NULL]

# Scale and center 
cols <- colnames(final_data)
final_data[, (cols) := lapply(.SD, scale), .SDcols=cols]



#### TRAIN MODEL
### Set up train/test split
tt_split <- createDataPartition(
  y = final_data$log_price,
  p = .80, ## The percentage of data in the training set
  list = FALSE
)

data_train <- final_data[ tt_split,]
data_test  <- final_data[-tt_split,]

### Run 2 full regression models (robust vs regular LS), for comparison
lm_1 <- lm(log_price~.,data=data_train)
lm_1_robust <- rlm(log_price~.,data=data_train)

summary(lm_1)
#summary(lm_1_robust)


### Run Stepwise model
null_model <- lm(log(price) ~ 1, data = data_train[complete.cases(data_train)])
full_model <- lm(log(price) ~ ., data = data_train[complete.cases(data_train)])

lm_1_step <- step(null_model, scope = list(lower = null_model, upper = full_model),
                       k = log(nrow(data_train)), trace = F)

summary(lm_1_step)


### Run ridge regression
lm_1_ridge <- glmnet(final_data[, !"price", with=FALSE],
                     final_data[,price],
                     alpha=0,
                     lambda=10^seq(-2,log10(exp(4)),length=101),
                     nfolds=10)

coef(lm_1_ridge)


### Run lasso regression
lm_1_lasso<-glmnet(final_data[, !"price", with=FALSE],
                      final_data[,price],
                     alpha=1,
                     lambda=10^seq(-2,log10(exp(4)),length=101),
                     nfolds=10)

coef(lm_1_lasso)


#### CV method does not work.....
#lm_1_lasso<-cv.glmnet(final_data[, !"price", with=FALSE],
#                   final_data[,price],
#                   alpha=1,
#                   lambda=10^seq(-2,log10(exp(4)),length=101),
#                   nfolds=10)





# TRAINING predictions 
train_preds <- cbind(exp(lm_1$fitted.values), exp(lm_1_robust$fitted.values), exp(lm_1$model[1])) |> as.data.table()
head(train_preds, 30)


### Cross validation for improving robustness of training 

lassoGrid <-  expand.grid()


instruct_cv <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

model_cv_glmnet <- train(log(price) ~ .,
                  data = data_train,
                  preProc = c("center", "scale"), ## pre-processing of variables
                  method = "glmnet",  
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

model_cv_lasso <- train(log(price) ~ .,
                     data = data_train,
                     preProc = c("center", "scale"), ## pre-processing of variables
                     method = "lasso", 
                     na.action=na.exclude,
                     trControl = instruct_cv)  

#### TEST MODEL
# Use 20% of data to evaluate goodness of fit for each candidate model
lm_1_pred <- predict(lm_1,data_test)
lm_1_robust_pred <- predict(lm_1_robust,data_test)
lm_1_ridge_pred <- predict(lm_1_ridge,data_test)
lm_1_lasso_pred <- predict(lm_1_lasso,data_test)


cv_1_glmnet_pred <- predict(model_cv_glmnet,data_test)
cv_1_boost_pred <- predict(model_cv_boostlm,data_test)
cv_1_lm_pred <- predict(model_cv_lm,data_test)
cv_1_lasso <- predict(model_cv_lasso,data_test)

### Compare TEST predictions
test_preds <- cbind(exp(lm_1_pred), 
                    exp(lm_1_robust_pred), 
                    1, 
                    1,
                    1,
                    1,
                    data_test$price) |> as.data.table()
test_preds <- test_preds[!is.na(V1)]
setnames(test_preds, c("LM", "LM_ROBUST", "GLM_NET", "LM_BOOST", "LM_CV", "LASSO", "ACTUAL"))
test_preds[,GLM_NET:=exp(model_cv_stepwise)]
test_preds[,LM_BOOST:=exp(cv_1_boost_pred)]
test_preds[,LM_CV:=exp(cv_1_lm_pred)]
test_preds[,LASSO:=exp(cv_1_lasso)]

head(test_preds, 30)


### RMSE of predictions
# Based on test predictions, the lasso method wins!
test_preds[!is.na(LM), sqrt(sum((LM-ACTUAL)^2/.N))] #LM (120.554)
test_preds[!is.na(LM), sqrt(sum((LM_ROBUST-ACTUAL)^2/.N))] #LM_ROBUST (122.169)
# Add other models
test_preds[!is.na(LM), sqrt(sum((GLM_NET-ACTUAL)^2/.N))] #GLM_NET (260.470)
test_preds[!is.na(LM), sqrt(sum((LM_BOOST-ACTUAL)^2/.N))] #LM_BOOST (116.463)
test_preds[!is.na(LM), sqrt(sum((LM_CV-ACTUAL)^2/.N))] #LM (120.553)
test_preds[!is.na(LM), sqrt(sum((LASSO-ACTUAL)^2/.N))] #LASSO (119.618)

# off by $120, on average....

### We could probably grid.tune the lasso to check more lambda values 
# Here, it only explored 0.1, 0.5, 0.9


