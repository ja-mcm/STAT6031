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

### Keep a reference dataset
model_data <- copy(final_data)



#### TRAIN MODEL
### Set up train/test split
tt_split <- createDataPartition(
  y = model_data$price,
  p = .80, ## The percentage of data in the training set
  list = FALSE
)

## Create scaled and non-scaled datasets 
# Scaled will be used for regression model/predictions
# Non-scaled will help us evaluate our predictions
data_train <- model_data[ tt_split,]
data_test  <- model_data[-tt_split,]
data_orig_train <- final_data[ tt_split,]
data_orig_test <- final_data[ -tt_split,]

###
all.equal(data_train,data_orig_train)


model_data[,log_price := log(price)]
model_data[,price:=NULL]

# Scale and center 
cols <- colnames(model_data)
model_data[, (cols) := lapply(.SD, scale), .SDcols=cols]

# Re-split, now that modeling data is scaled
data_train <- model_data[ tt_split,]
data_test  <- model_data[-tt_split,]




### Run 2 full regression models (regular LS and robust), for comparison
lm_1 <- lm(log_price~.,data=data_train)
#lm_1_robust <- rlm(log_price~.,data=data_train)

summary(lm_1)
#summary(lm_1_robust)


### Run Stepwise model
null_model <- lm(log_price ~ 1, data = data_train[complete.cases(data_train)])
full_model <- lm(log_price ~ ., data = data_train[complete.cases(data_train)])

lm_1_step <- step(null_model, scope = list(lower = null_model, upper = full_model),
                       k = log(nrow(data_train)), trace = F)

summary(lm_1_step)


### Run ridge regression
lm_1_ridge <- glmnet(data_train[, !"log_price", with=FALSE],
                     data_train[,log_price],
                     alpha=0,
                     lambda=10^seq(-2,log10(exp(4)),length=101),
                     nfolds=10)

coef(lm_1_ridge)


### Run lasso regression
lm_1_lasso<-glmnet(data_train[, !"log_price", with=FALSE],
                   data_train[,log_price],
                     alpha=1,
                     lambda=10^seq(-2,log10(exp(4)),length=101),
                     nfolds=10)

coef(lm_1_lasso)


### TO DO
# - find "best" Ridge/Lasso model coefficients



#### TEST MODEL
# Use 20% of data to evaluate goodness of fit for each candidate model
lm_1_pred <- predict(lm_1,data_test)
#lm_1_robust_pred <- predict(lm_1_robust,data_test)
lm_1_step_pred <- predict(lm_1_step,data_test)
lm_1_ridge_pred <- predict(lm_1_ridge,data_test[,!"log_price", with=FALSE] |> as.matrix())
lm_1_lasso_pred <- predict(lm_1_lasso,data_test[,!"log_price", with=FALSE] |> as.matrix())


set_price <- function(X) {exp(X) * final_data[,mean(price)] |> currency()}

### Compare TEST predictions
test_preds <- cbind(set_price(lm_1_pred), 
                    1,
                    set_price(lm_1_step_pred), 
                    set_price(lm_1_ridge_pred)[,1], 
                    set_price(lm_1_lasso_pred)[,40],
                    set_price(data_test$log_price)) |> as.data.table()
test_preds <- test_preds[!is.na(V1)]
setnames(test_preds, c("LM", "LM_ROBUST", "STEPWISE", "RIDGE", "LASSO", "ACTUAL"))

head(test_preds, 30)


### RMSE of predictions
# Based on test predictions, the lasso method wins!
test_preds[!is.na(LM), sqrt(sum((LM-ACTUAL)^2/.N))] #LM
test_preds[!is.na(LM), sqrt(sum((LM_ROBUST-ACTUAL)^2/.N))] #LM_ROBUST
# Add other models
test_preds[!is.na(LM), sqrt(sum((STEPWISE-ACTUAL)^2/.N))] #STEPWISE
test_preds[!is.na(LM), sqrt(sum((RIDGE-ACTUAL)^2/.N))] #RIDGE
test_preds[!is.na(LM), sqrt(sum((LASSO-ACTUAL)^2/.N))] #LASSO

# Outliers
test_preds[LM==max(test_preds$LM)]
data_orig_test[545]


