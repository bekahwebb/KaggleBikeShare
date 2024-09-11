library(DataExplorer)
library(vroom)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(readr)
library(GGally)


bike_test <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/test.csv")
bike_test
bike_train <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/train.csv")
bike_train
bike_sample <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/sampleSubmission.csv")
bike_sample

#EDA on training data
ggpairs(bike_train)# not loading plots for some reason, maybe too big

variable_plot <- DataExplorer::plot_intro(bike_train) # visualization of glimpse! 

correlation_plot <- DataExplorer::plot_correlation(bike_train) # correlation heat map 


#scatterplots
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

plot1 + plot2 #side by side
plot3 / plot4 #stacked
four_panel <- (plot1 + plot2) / (plot3 + plot4) #4 panel plot
four_panel


## Setup and Fit the Linear Regression Model3
my_linear_model <- linear_reg() %>% #Type of model4
  set_engine("lm") %>% # Engine = What R function to use5
  set_mode("regression") %>% # Regression just means quantitative response6
  fit(formula=count~weather+temp+humidity+windspeed, data=bike_train)
my_linear_model

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
                            new_data=bike_test) # Use fit to predict11
bike_predictions ## Look at the output

## Format the Predictions for Submission to Kaggle1
kaggle_submission <- bike_predictions %>%
  bind_cols(., bike_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file9
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")