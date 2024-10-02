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

bike_test <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/test.csv")
bike_test
bike_train <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/train.csv")
bike_train
bike_sample <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/sampleSubmission.csv")
bike_sample


#September 11, 2024
## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response
  fit(formula=log(count)~weather+temp+humidity+windspeed, data=bike_train)
my_linear_model

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
                            new_data=bike_test) # Use fit to predict11
bike_predictions ## Look at the output

## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(count = exp(count))%>%
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")

#September 13 2024

#Kaggle submission with poisson model
my_pois_model <- poisson_reg() %>% #Type of model
  set_engine("glm") %>% # GLM = generalized(not normal) linear model-cause of DF Null dev residual dev BIC output?
  set_mode("regression") %>%
  fit(formula=count~weather+temp+humidity+windspeed , data=bike_train)
my_pois_model

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_pois_model,
                            new_data=bike_test) # Use fit to predict
bike_predictions ## Look at the output

## Format the Predictions for Submission to Kaggle
pois_kaggle_submission <- bike_predictions %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=pois_kaggle_submission, file="./PoissonPreds.csv", delim=",")

#September 16, 2024 Data wrangling and making recipe and workflow functions
#1 clean data
# Data Cleaning may be overfitting the data, warning for the response, keep adjusting the features so I don't 
#have too much correlated data.

clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

# Feature Engineering #maybe create an interaction between hour and weekday, maybe make a hour a factor

my_recipe <- recipe(count ~ ., data = clean_bike) %>% #warning could mean too many features, maybe too complex
  step_mutate(weather = ifelse(weather== 4,3, weather)) %>%  # Recode weather category
  step_mutate(weather = factor(weather, levels=1:3, labels=c("Clear", "Mist", "Precipitation"))) %>% #Convert season to factor
  step_mutate(season = factor(season, levels=1:4, labels=c("Spring","Summer","Fall", "Winter"))) %>%  # Convert season to factor
  # step_mutate(holiday = factor(holiday, levels=c(0,1), labels=c("No", "Yes")))%>% # Convert holiday as a factor
  # step_mutate(workingday = factor(workingday, levels=c(0,1), labels=c("No", "Yes")))%>% #Convert workingday as a factor
  # step_mutate(minute = as.factor(minute(datetime)))%>%  # Extract min. from datetime column
  step_time(datetime, features = "hour")%>% #extract hour
  step_mutate(datetime_hour = factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>% #factor hour
  step_date(datetime, features = "dow" )%>% #extract day of week
  #step_mutate(workingday = factor(workingday, levels=c(0,1), labels=c("No", "Yes")))%>% #Convert workingday as a factor
  # step_mutate(month = as.factor(month(datetime))) %>%
  #step_interact(terms = ~ hour*dow)%>% #create interaction term between hour and day of the week
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>%  # Remove zero-variance predictors
  step_dummy(all_nominal_predictors()) %>%  # Create dummy variables
  #step_poly(temp, degree = 2)%>%  # Create polynomial features for temp
  #step_poly(windspeed, degree=2) %>%
  step_corr(all_predictors())  # Remove highly correlated predictors

# Preprocessing
prepped_recipe <- prep(my_recipe)
#Baking
test <- bake(prepped_recipe, new_data = bike_train)
test

## Define a Model
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")
## Combine into a Workflow and fit
bike_workflow <- workflow() %>% #sets up a series of steps that you can apply to any dataset
  add_recipe(my_recipe) %>%
  add_model(lin_model) %>%
  fit(data=clean_bike)#fit the workflow

## Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = bike_test)

## Format the Predictions for Submission to Kaggle
linear_kaggle_submission <- lin_preds %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=linear_kaggle_submission, file="BakedlinearPreds.csv", delim=",")

#kaggle score: 0.64417

#9/18/24 Penalized Regression

## Create a recipe
bike_recipe <- recipe(count~., data=clean_bike) %>%
  step_time(datetime, features="hour") %>% #extract hour
  step_mutate(datetime_hour= factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>%
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_interact(terms = ~ datetime_hour*workingday)%>%
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% # Make mean 0, sd=1
  step_corr(all_predictors())  # Remove highly correlated predictors


# Preprocessing
prepped_recipe <- prep(bike_recipe)
#Baking
test1 <- bake(prepped_recipe, new_data = bike_train)
#test1

## Penalized regression model
preg_model <- linear_reg(penalty=0, mixture= 0) %>% #Set model and tuning so far 0,0 got me my lowest score
  set_engine("glmnet") # Function to fit in R
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data=clean_bike)

## Run all the steps on test data
predict(preg_wf, new_data=bike_test)

## Format the Predictions for Submission to Kaggle
preg_linear_kaggle_submission <- predict(preg_wf, new_data=bike_test) %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=preg_linear_kaggle_submission, file="PenalizedPreds.csv", delim=",")
#kaggle score: 0.48734

9/20/24
clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

## Create a recipe
bike_recipe <- recipe(count~., data=clean_bike) %>%
  step_time(datetime, features="hour") %>% #extract hour
  step_mutate(datetime_hour= factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>%
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_interact(terms = ~ datetime_hour*workingday)%>%
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% # Make mean 0, sd=1
  step_corr(all_predictors())  # Remove highly correlated predictors


# Preprocessing
prepped_recipe <- prep(bike_recipe)
#Baking
tuned_bike <- bake(prepped_recipe, new_data = bike_train)
tuned_bike


## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
add_recipe(bike_recipe) %>%
add_model(preg_model)

## Grid of values to tune over
grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(clean_bike, v = 5, repeats=1)
#TUNING MODELS IN R
## Run the CV
CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=grid_of_tuning_params,
          metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL #mean absolute error, rsquared

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best(metric = "rmse")

#TUNING MODELS IN R
## Finalize the Workflow & fit it
final_wf <-
preg_wf %>%
finalize_workflow(bestTune) %>%
fit(data=clean_bike)

## Predict
final_wf %>%
predict(new_data = bike_test)

## Format the Predictions for Submission to Kaggle
tuned_linear_kaggle_submission <- predict(final_wf, new_data=bike_test) %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=tuned_linear_kaggle_submission, file="TunedPreds.csv", delim=",")
#kaggle score: 0.47968

#September 23, 2024 Regression Tree Model

clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

## Create a recipe
bike_recipe <- recipe(count~., data=clean_bike) %>%
  step_time(datetime, features="hour") %>% #extract hour
  step_mutate(datetime_hour= factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>%
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_interact(terms = ~ datetime_hour*workingday)%>%
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% # Make mean 0, sd=1
  step_corr(all_predictors())  # Remove highly correlated predictors


# Preprocessing
prepped_recipe <- prep(bike_recipe)
#Baking
tree_bike <- bake(prepped_recipe, new_data = bike_train)
tree_bike


## Regression tree model
tree_model <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% #Type of model
  set_engine("rpart") %>% # What R function to use7
  set_mode("regression")


## Create a workflow with model & recipe
tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(tree_model)

## Set up grid of tuning values
grid_of_tuning_params <- grid_regular(tree_depth(),
                                      cost_complexity(),
                                      min_n(),
                                      levels = 3) ## L^2 total tuning possibilities

grid_of_tuning_params
## Set up K-fold 
## Split data for CV
folds <- vfold_cv(clean_bike, v = 5, repeats=1)
#TUNING MODELS IN R
## Run the CV
CV_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL #mean absolute error, rsquared

## Find best tuning parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")

#TUNING MODELS IN R
## Finalize the Workflow & fit it
final_wf <-
  tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=clean_bike)

## Finalize workflow and predict
final_wf %>%
  predict(new_data = bike_test)

## Format the Predictions for Submission to Kaggle
tuned_linear_kaggle_submission <- predict(final_wf, new_data=bike_test) %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=tuned_linear_kaggle_submission, file="TreePreds.csv", delim=",")
#kaggle score: 0.62831

#random forest model September 25, 2025 9/30/24 try to lower kaggle score with poly features, and year factored
#install.packages("ranger")
#library(ranger)

clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

## Create a recipe
bike_recipe <- recipe(count~., data=clean_bike) %>%
  step_time(datetime, features="hour") %>% #extract hour
  step_mutate(datetime_hour= factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>%
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_interact(terms = ~ datetime_hour*workingday)%>%
  step_mutate(year = factor(year(datetime))) %>%  # Extract year as a vector and factorize
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% # Make mean 0, sd=1
  step_poly(temp, degree=2) %>% 
  step_poly(windspeed, degree=2) %>%
  step_poly(weather, degree = 2) %>%
  step_corr(all_predictors())  # Remove highly correlated predictors


# Preprocessing
prepped_recipe <- prep(bike_recipe)
#Baking
forest_bike <- bake(prepped_recipe, new_data = bike_train)
forest_bike


## Regression tree model 9/30/24 try 750 trees
rf_model <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=750) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")


## Create a workflow with model & recipe
tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(rf_model)

## Set up grid of tuning values
grid_of_tuning_params <- grid_regular(mtry(range=c(1,10)),
                                      min_n(),
                                      levels = 3) ## L^2 total tuning possibilities

grid_of_tuning_params

## Set up K-fold 
## Split data for CV
folds <- vfold_cv(clean_bike, v = 5, repeats=1)
#TUNING MODELS IN R
## Run the CV
CV_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL #mean absolute error, rsquared

## Find best tuning parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")

#TUNING MODELS IN R
## Finalize the Workflow & fit it
final_wf <-
  tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=clean_bike)

## Finalize workflow and predict
final_wf %>%
  predict(new_data = bike_test)

## Format the Predictions for Submission to Kaggle
tuned_linear_kaggle_submission <- predict(final_wf, new_data=bike_test) %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=tuned_linear_kaggle_submission, file="RFPreds.csv", delim=",")

#kaggle score: 0.47537
# with 1000 trees my score dropped slightly to 0.47392
#750 trees, polynomial features for temp, weather and windspeed, extracting and factoring year slightly lowered
#my score to 0.46418

# stacked model September 27, 2024

## Split data for CV
folds <- vfold_cv(clean_bike, v = 5, repeats=1)

## Create a control grid
untunedModel <- control_stack_grid() #If tuning over a grid
tunedModel <- control_stack_resamples() #If not tuning a model


## Create other resampling objects with different ML algorithms to include in a stacked model,
#candidate # 1
clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

# Feature Engineering #maybe create an interaction between hour and weekday, maybe make a hour a factor

my_recipe <- recipe(count ~ ., data = clean_bike) %>% #warning could mean too many features, maybe too complex
  step_mutate(weather = ifelse(weather== 4,3, weather)) %>%  # Recode weather category
  step_mutate(weather = factor(weather, levels=1:3, labels=c("Clear", "Mist", "Precipitation"))) %>% #Convert season to factor
  step_mutate(season = factor(season, levels=1:4, labels=c("Spring","Summer","Fall", "Winter"))) %>%  # Convert season to factor
  step_time(datetime, features = "hour")%>% #extract hour
  step_mutate(datetime_hour = factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>% #factor hour
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>%  # Remove zero-variance predictors
  step_dummy(all_nominal_predictors()) %>%  # Create dummy variables
  step_corr(all_predictors())  # Remove highly correlated predictors

## Define a Model
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")
## Combine into a Workflow and fit
bike_workflow <- workflow() %>% #sets up a series of steps that you can apply to any dataset
  add_recipe(my_recipe) %>%
  add_model(lin_model) 

lin_reg_model <-
fit_resamples(bike_workflow,
              resamples = folds,
              metrics =metric_set(rmse, mae, rsq),
              control = untunedModel
)
#candidate # 2
## Penalized regression model
clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

## Create a recipe
bike_recipe <- recipe(count~., data=clean_bike) %>%
  step_time(datetime, features="hour") %>% #extract hour
  step_mutate(datetime_hour= factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>%
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_interact(terms = ~ datetime_hour*workingday)%>%
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% # Make mean 0, sd=1
  step_corr(all_predictors())  # Remove highly correlated predictors

## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

## Grid of values to tune over
grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 5) ## L^2 total tuning possibilities

# Run the CV
preg_models <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae, rsq),
            control = untunedModel) # including the control grid in the tuning ensures you can
# call on it later in the stacked model

# #candidate 2 regression trees
# tree_model <- decision_tree(tree_depth = tune(), # setting it up to tell it later 
#                           cost_complexity = tune(), 
#                           min_n = tune()) %>% 
#   set_engine("rpart") %>%  # what R function to use 
#   set_mode("regression") 
# 
# 
# tree_wf <- workflow() %>% 
#   add_recipe(bike_recipe) %>% 
#   add_model(tree_model) 
# 
# tree_models <- tree_wf %>% 
#   tune_grid(resamples = folds, 
#             grid=tree_tuning_grid, 
#             metrics = metric_set(rmse, mae), 
#             control = untunedModel) 

## Specify with models to include, make the stack
bike_stack <- stacks() %>%
  add_candidates(lin_reg_model)%>%
  add_candidates(preg_models) 

stack_model <- my_stack %>% 
  blend_predictions() %>% 
  fit_members() 
as_tibble(bike_stack) 
## Use the stacked data to get a prediction
# stack_model %>% predict(fitted_stack=bike_test)
stack_model %>% predict(new_data =bike_test)

## Format the Predictions for Submission to Kaggle
stacked_linear_kaggle_submission <- predict(stack_model, new_data = bike_test) %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=stacked_linear_kaggle_submission, file="StackPreds.csv", delim=",")
#kaggle score is 0.47864 which is a little higher than my random forest results, and penalized regression model
#may add the tree model to see if that helps lower my score if I am feeling ambitious :)

##9/30/24 KNN and then maybe Bart
install.packages('kknn')
library('kknn')

clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))

# Feature Engineering #maybe create an interaction between hour and weekday, maybe make a hour a factor

bike_recipe <- recipe(count~., data=clean_bike) %>%
  step_time(datetime, features="hour") %>% #extract hour
  step_mutate(datetime_hour= factor(datetime_hour, levels=c(0:23), labels=c(0:23))) %>%
  step_date(datetime, features = "dow" )%>% #extract day of week
  step_interact(terms = ~ datetime_hour*workingday)%>%
  step_rm(datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors())%>% # Make mean 0, sd=1
  step_corr(all_predictors())  # Remove highly correlated predictors

# Preprocessing
prepped_recipe <- prep(my_recipe)
#Baking
test <- bake(prepped_recipe, new_data = bike_train)
test

## Define a Model
knn_model <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() %>% #sets up a series of steps that you can apply to any dataset
  add_recipe(my_recipe) %>%
  add_model(knn_model) %>%
  fit(data=clean_bike)#fit the workflow

## Run all the steps on test data
knn_preds <- predict(bike_workflow, new_data = bike_test)

## Format the Predictions for Submission to Kaggle
knn_kaggle_submission <- knn_preds %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=knn_kaggle_submission, file="KNNPreds.csv", delim=",")
#worst recent score 0.78987

#try bart
install.packages("dbarts")
library("dbarts")
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

## Define a Model
bart_model <- parsnip::bart(
  trees = 1000,
  engine = "dbarts", 
  mode = "regression",
  prior_terminal_node_coef = .95, #Tune prior coefficient
  prior_terminal_node_expo = 2, #Tune prior exponent
  prior_outcome_range = 2)

## Combine into a Workflow and fit
bike_workflow <- workflow() %>% #sets up a series of steps that you can apply to any dataset
  add_recipe(bike_recipe) %>%
  add_model(bart_model) %>%
  fit(data=clean_bike)#fit the workflow

## Run all the steps on test data
bart_preds <- predict(bike_workflow, new_data = bike_test)

## Format the Predictions for Submission to Kaggle
bart_kaggle_submission <- bart_preds %>%
  rename(count=.pred) %>%
  mutate(count = exp(count)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, count) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=bart_kaggle_submission, file="BartPreds.csv", delim=",")
#lowest score so far 0.43613
#adding polynomial features for temp, windspeed, and weather slightly lowered my kaggle score to 0.43384
#factoring year slightly lowered my kaggle score to 0.43335
# a little lower with an interaction between humidity and weather at 0.43235
#woah adding 1000 trees and prior parameters lowered my score to a 0.36539!