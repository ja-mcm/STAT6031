library(data.table)
library(caret)

### For reproducibility
set.seed(111)







#### SOME BOILERPLATE CODE TO COMPARE OLS AGAINST ROBUST REGRESSION
# Get rid of high dimensional dummy variables, just for convenience
final_data[,c("host_location", "host_response_rate", "host_acceptance_rate", "latitude","longitude"):=NULL]

lm_1 <- lm(log(price)~.,data=final_data)
lm_1_robust <- rlm(log(price)~.,data=final_data)

summary(lm_1)
summary(lm_1_robust)

cbind(lm_1$fitted.values |> exp(), lm_1_robust$fitted.values |> exp(), lm_1$model[1] |> exp()) |> head(100)


# Robust regression has lower RMSE (.3922 vs .4476)



#### SOME BOILERPLATE CODE FOR USING CARET / CROSS-VALIDATION (if we decide to go this way)

### Set up train/test split
tt_split <- createDataPartition(
  y = mtcars$mpg,
  p = .80, ## The percentage of data in the training set
  list = FALSE
)

data_train <- mtcars[ tt_split,]
data_test  <- mtcars[-tt_split,]


### Cross validation for improving robustness of training 
instruct_cv <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

model_cv <- train(mpg ~ .,
                  data = mtcars,
                  preProc = c("center", "scale"), ## pre-processing of variables
                  method = "lasso",  # now we're using the lasso method
                  trControl = instruct_cv)  

model_cv

plsFit <- train(
  data = data_train,
  method = "pls",
  preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl
)
