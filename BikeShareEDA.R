install.packages("vroom")
library(vroom)
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
install.packages("patchwork")
library(patchwork)

bike_test <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/test.csv")
bike_test
bike_train <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/train.csv")
bike_train
bike_sample <- vroom("C:/Users/bekah/Downloads/bike-sharing-demand/sampleSubmission.csv")
bike_sample

scatterplots
plot1 <- ggplot(bike_train, aes(x = weather, y = count)) +
  geom_point() +
  geom_smooth(se=FALSE)
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
