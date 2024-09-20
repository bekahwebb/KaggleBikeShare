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
