library(tidyverse)
library(DataExplorer)
library(vroom)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(readr)
library(GGally)
library(poissonreg)
library(recipes)
library(rsample)
library(magrittr)
library(tidymodels)
library(lubridate)
library(poissonreg) #if you want to do penalized, poisson regression
library(rpart)
library(ranger)
library(stacks) # you need this library to create a stacked model
library('xgboost')

bike_test <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/test.csv")
bike_test
bike_train <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/train.csv")
bike_train
bike_sample <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/sampleSubmission.csv")
bike_sample


clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

# Feature Engineering #maybe create an interaction between hour and weekday, maybe make a hour a factor

bike_recipe <- recipe(count~., data=clean_bike) %>%
  step_time(datetime, features="hour") %>% #extract hour
  step_mutate(datetime_hour= factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>%
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_interact(terms = ~ datetime_hour*workingday)%>%
  step_interact(terms = ~ temp*humidity)%>%
  step_mutate(year = factor(year(datetime)))%>% #extract year as a vector and factorize
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% # Make mean 0, sd=1
  step_poly(temp, degree=2) %>% 
  step_poly(windspeed, degree=2) %>%
  step_poly(weather, degree = 2) %>%
  step_corr(all_predictors())  # Remove highly correlated predictors

# Preprocessing
prepped_recipe <- prep(my_recipe)
#Baking
test <- bake(prepped_recipe, new_data = bike_train)
test


# ## Define a Model
# xg_model <-boost_tree(
#   trees = 100,
#   tree_depth = 6,
#   learn_rate = 0.1,
#   loss_reduction = 0.1,
#   sample_size = 0.8, 
#   mode = "regression"
# )%>%
#   set_engine("xgboost")%>% 
#   set_mode("regression")
xg_model <- boost_tree(
  trees = 100,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = 0.8,
  mode = "regression"
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Workflow and Tuning
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(xg_model)

# Cross-Validation
folds <- vfold_cv(clean_bike, v = 10)

# Set up grid of tuning values
grid_of_tuning_params <- grid_regular(
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  levels = 3
)

# Tune Model
CV_results <- bike_workflow %>%
  tune_grid(resamples = folds,
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse, mae))

# Find Best Parameters
bestTune <- select_best(CV_results, metric = "rmse")
bestTune

# Finalize and Fit the Best Model
final_wf <- finalize_workflow(bike_workflow, bestTune) %>%
  fit(data = clean_bike)

# Make Predictions on Test Data
xg_preds <- predict(final_wf, new_data = bike_test)

## Format the Predictions for Submission to Kaggle
xg_kaggle_submission <- xg_preds %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=xg_kaggle_submission, file="xgPreds.csv", delim=",")
#lowest score so far 0.42303

#tweak tuning params with best tune preds
clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

# Feature Engineering #maybe create an interaction between hour and weekday, maybe make a hour a factor

bike_recipe <- recipe(count~., data=clean_bike) %>%
  step_time(datetime, features="hour") %>% #extract hour
  step_mutate(datetime_hour= factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>%
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_interact(terms = ~ datetime_hour*workingday)%>%
  step_interact(terms = ~ temp*humidity)%>%
  step_mutate(year = factor(year(datetime)))%>% #extract year as a vector and factorize
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% # Make mean 0, sd=1
  step_poly(temp, degree=2) %>% 
  step_poly(windspeed, degree=2) %>%
  step_poly(weather, degree = 2) %>%
  step_corr(all_predictors())  # Remove highly correlated predictors

# Preprocessing
prepped_recipe <- prep(my_recipe)
#Baking
test <- bake(prepped_recipe, new_data = bike_train)


## Define a Model
xg_model <-boost_tree(
  trees = 100,
  tree_depth = 13,
  learn_rate = 0.1,
  loss_reduction =  0.0000562,
  sample_size = 0.8,
  mode = "regression"
)%>%
  set_engine("xgboost")%>%
  set_mode("regression")

# Finalize and Fit the Best Model
final_wf <- finalize_workflow(bike_workflow, bestTune) %>%
  fit(data = clean_bike)

# Make Predictions on Test Data
xg_preds <- predict(final_wf, new_data = bike_test)

## Format the Predictions for Submission to Kaggle
xg_kaggle_submission <- xg_preds %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=xg_kaggle_submission, file="xgPreds1.csv", delim=",")
#lowest score so far 0.42303 with cross validation
#tweaking the tree depth and loss reduction got it down to 0.42199