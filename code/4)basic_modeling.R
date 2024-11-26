library(data.table)
library(caret)
library(MASS)
library(glmnet)
library(boot)
library(formattable)


### For reproducibility
set.seed(111)

# Get rid of high dimensional dummy variables, just for convenience
final_data[,c("id", "host_location", "amenities_list", "neighbourhood_cleansed", "host_response_rate", "host_acceptance_rate"):=NULL]


### NOTE --- Imputation led to WAY worse R^2 (.65 --> .56)

# # Impute missing reviews with the median value
# # We'll be introducing an interaction variable, which will probe whether having MANY high review scores affects price, above & beyond the effect of just having good reviews
# # So, these records with zero reviews will be left out of that interaction, which makes sense since they have no observations...
# final_data[, review_scores_rating := replace(review_scores_rating, is.na(review_scores_rating), median(review_scores_rating, na.rm=TRUE))]
# final_data[, review_scores_accuracy := replace(review_scores_accuracy, is.na(review_scores_accuracy), median(review_scores_accuracy, na.rm=TRUE))]
# final_data[, review_scores_cleanliness := replace(review_scores_cleanliness, is.na(review_scores_cleanliness), median(review_scores_cleanliness, na.rm=TRUE))]
# final_data[, review_scores_checkin := replace(review_scores_checkin, is.na(review_scores_checkin), median(review_scores_checkin, na.rm=TRUE))]
# final_data[, review_scores_communication := replace(review_scores_communication, is.na(review_scores_communication), median(review_scores_communication, na.rm=TRUE))]
# final_data[, review_scores_location := replace(review_scores_location, is.na(review_scores_location), median(review_scores_location, na.rm=TRUE))]
# final_data[, review_scores_value := replace(review_scores_value, is.na(review_scores_value), median(review_scores_value, na.rm=TRUE))]
#final_data[is.na(host_is_superhost), host_is_superhost := 0]

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

final_data[,host_owns_gt_5:=ifelse(host_total_listings_count > 5, 1, 0)]
final_data[,host_owns_2_5:=ifelse(host_total_listings_count < 5 & host_total_listings_count > 1, 1,0 )]
final_data[,host_total_listings_count:=NULL]

# Add interaction variable for # of reviews * review 
# we suspect that the number of high review scores might matter
final_data[,review_score_ct_interaction:=review_scores_rating * number_of_reviews]


### Keep a reference dataset
model_data <- copy(final_data)



#### TRAIN MODEL
### Set up train/test split
tt_split <- createDataPartition(
  y = model_data$price,
  p = .80, ## The percentage of data in the training set
  list = FALSE
)

# scale price by log, to make the right-skewed distribution look more normal
model_data[,log_price := log(price)]
model_data[,price:=NULL]

## Create scaled and non-scaled datasets 
# Scaled will be used for regression model/predictions
# Non-scaled will help us evaluate our predictions
data_train <- model_data[ tt_split,]
data_test  <- model_data[-tt_split,]
data_orig_train <- final_data[ tt_split,]
data_orig_test <- final_data[ -tt_split,]

data_test[,c("latitude", "longitude"):=NULL]
data_train[,c("latitude", "longitude"):=NULL]


### Run 2 full regression models (regular LS and robust), for comparison
lm <- lm(log_price~.,data=data_train)
#lm_robust <- rlm(log_price~.,data=data_train)

summary(lm)
#summary(lm_robust)


### Run Stepwise model
null_model <- lm(log_price ~ 1, data = data_train[complete.cases(data_train)])
full_model <- lm(log_price ~ ., data = data_train[complete.cases(data_train)])

lm_step <- step(null_model, scope = list(lower = null_model, upper = full_model),
                       k = log(nrow(data_train)), trace = F)

summary(lm_step)


### Run ridge regression
lm_ridge_cv<-cv.glmnet(data_train[, !"log_price", with=FALSE] |> as.matrix(),
                             data_train[,log_price] |> as.matrix(),
                             alpha=0,
                             lambda=10^seq(-2,log10(exp(4)),length=101),
                             nfolds=10)

coef(lm_ridge_cv, s=lm_ridge_cv$lambda.min)
plot(lm_ridge_cv)



### Run lasso regression
lm_lasso_cv<-cv.glmnet(data_train[, !"log_price", with=FALSE] |> as.matrix(),
                         data_train[,log_price] |> as.matrix(),
                         alpha=1,
                         lambda=10^seq(-2,log10(exp(4)),length=101),
                         nfolds=10)
coef(lm_lasso_cv, lm_lasso_cv$lambda.min)
plot(lm_lasso_cv)



#### TEST MODEL
# Use 20% of data to evaluate goodness of fit for each candidate model
lm_pred <- predict(lm,data_test)
#lm_robust_pred <- predict(lm_robust,data_test)
lm_step_pred <- predict(lm_step,data_test)
lm_ridge_pred <- predict(lm_ridge_cv,
                         s =lm_ridge_cv$lambda.min,
                         data_test[,!"log_price", with=FALSE] |> as.matrix())
lm_lasso_pred <- predict(lm_lasso_cv,
                         s =lm_lasso_cv$lambda.min,
                         data_test[,!"log_price", with=FALSE] |> as.matrix())

set_price <- function(X) {exp(X) |> currency()}


### Compare TEST predictions
test_preds <- cbind(seq(1:length(lm_pred)), 
                    set_price(lm_pred),
                    1,
                    set_price(lm_step_pred), 
                    set_price(lm_ridge_pred), 
                    set_price(lm_lasso_pred),
                    set_price(data_test$log_price),
                    data_orig_test$price) |> as.data.table()
test_preds <- test_preds[!is.na(V1)]
setnames(test_preds, c("INDX", "LM", "LM_ROBUST", "STEPWISE", "RIDGE", "LASSO", "ACTUAL", "REAL_ACTUAL"))

head(test_preds, 30)


test_preds[,SSE_LM:=(LM-ACTUAL)^2]
test_preds[,SSE_STEPWISE:=(STEPWISE-ACTUAL)^2]
test_preds[,SSE_RIDGE:=(RIDGE-ACTUAL)^2]
test_preds[,SSE_LASSO:=(LASSO-ACTUAL)^2]
test_preds[,LASSO_ERROR:=(LASSO-ACTUAL)]


lm_lasso_pred_Train <- predict(lm_lasso_cv,
                         s =lm_lasso_cv$lambda.min,
                         data_train[,!"log_price", with=FALSE] |> as.matrix())

data_orig_train[,LASSO_ERROR:= exp(lm_lasso_pred_Train) - exp(data_train$log_price) ]
data_orig_train[,LASSO_SIGN:= ifelse(LASSO_ERROR>0,1,-1)]
data_orig_test[,LASSO_ERROR:= test_preds$LASSO_ERROR]
data_orig_test[,LASSO_SIGN:= ifelse(LASSO_ERROR>0,1,-1)]


### RMSE of predictions
# Based on test predictions, the lasso method wins!
test_preds[,sqrt(sum(SSE_LM))/.N]
test_preds[,sqrt(sum(SSE_STEPWISE))/.N]
test_preds[,sqrt(sum(SSE_RIDGE))/.N]
test_preds[,sqrt(sum(SSE_LASSO))/.N]


#Variable cleanup
rm(all_amenities)
rm(dest_points)
rm(filtered_data)
rm(null_model)
rm(full_model)
rm(tt_split)
rm(cols)
rm(inds)


### TO DO 
## Diagnostics
# Biggest residuals
test_preds[order(-SSE_LM)][1:10]


# Outliers
test_preds[LM==max(test_preds$LM)]
data_orig_test[545]
data_orig_test[652]
data_orig_test[571]


# Boxplot (range of predictions vs range of actual observations)
boxplot(data_orig_test$LASSO_ERROR)
