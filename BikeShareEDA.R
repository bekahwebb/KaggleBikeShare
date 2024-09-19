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


#September 9 2024

bike_test <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/test.csv")
bike_test
bike_train <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/train.csv")
bike_train
bike_sample <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/sampleSubmission.csv")
bike_sample

#EDA on training data
#ggpairs(bike_train, eval = "false")# not loading plots for some reason, maybe too big

variable_plot <- DataExplorer::plot_intro(bike_train) # visualization of glimpse! 

correlation_plot <- DataExplorer::plot_correlation(bike_train) # correlation heat map 


#plots
plot1 <- ggplot(bike_train, aes(x = weather)) +
  geom_bar() +
  labs(title = "Frequency of Categories", x = "Weather", y = "Count") +
  theme_minimal()
plot2 <- ggplot(bike_train, aes(x = temp, y = count)) +
  geom_point() +
  geom_smooth(se=FALSE)
plot3 <- ggplot(bike_train, aes(x = humidity, y = count)) +
  geom_point() +
  geom_smooth(se=FALSE)
plot4 <- ggplot(bike_train, aes(x = windspeed, y = count)) +
  geom_point() +
  geom_smooth(se=FALSE)

plot1


plot1 + plot2 #side by side
plot3 / plot4 #stacked
four_panel <- (plot1 + plot2) / (plot3 + plot4) #4 panel plot
four_panel

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

#Data wrangling September 16, 2024 and making recipe and workflow functions
#1 clean data
# Data Cleaning may be overfitting the data, warning for the response, keep adjusting the features so I don't 
#have too much correlated data.

clean_bike <- bike_train %>%
  select(-c('casual','registered')) %>%
  mutate(count = log(count))



# Feature Engineering #maybe create an interaction between hour and weekday, maybe make a hour a factor

my_recipe <- recipe(count ~ ., data = clean_bike) %>%
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
bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(lin_model) %>%
  fit(data=bike_train)#fit the workflow

## Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = bike_test)

## Format the Predictions for Submission to Kaggle
linear_kaggle_submission <- lin_preds %>%
  mutate(count = exp(.pred)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) %>% #needed for right format to Kaggle
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count))
  

## Write out the file
vroom_write(x=linear_kaggle_submission, file="BakedlinearPreds.csv", delim=",")

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
  fit(data=bike_train)

## Run all the steps on test data
predict(preg_wf, new_data=bike_test)

## Format the Predictions for Submission to Kaggle
preg_linear_kaggle_submission <- predict(preg_wf, new_data=bike_test) %>%
  mutate(count = exp(.pred)) %>%  # Back-transform the log to original scale
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  mutate(datetime=as.character(format(datetime))) %>% #needed for right format to Kaggle
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count))


## Write out the file
vroom_write(x=preg_linear_kaggle_submission, file="PenalizedPreds.csv", delim=",")
