library(data.table)
library(caret)

### For reproducibility
set.seed(111)


lm_1 <- lm(price ~ beds + bathrooms + near_top_10 + review_scores_rating, bnb_data)
summary(lm_1)

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
