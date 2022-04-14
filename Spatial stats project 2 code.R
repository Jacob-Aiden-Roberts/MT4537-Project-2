 # Project 2 spatial stats - Seal prediction via GAM

library(geoR)
library(mgcv)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(akima)

#Import observed data
observed_data <- read.csv("proj2_obs_data.csv", header=TRUE)

#Create model from observed data
gam_model <- gam(count ~ s(x ,y) + s(cov1) + s(cov2), data = observed_data, family = quasipoisson)
summary(gam_model)
#Plot smooths of variables in GAM
draw(gam_model)

#Predict with new variable values
predict_data <- read.csv("proj2_pred_data.csv", header=TRUE)
prediction1 <- predict(gam_model, predict_data, type="response")

#Create data frame for plotting which now contains predictions
plot_df <- predict_data
plot_df <- mutate(predict_data, prediction = prediction1)

#######Plot
#We cannot plot immediately as the data given is not on a regular grid shape
#Hence we will use akima::interp() to approximate a grid from data_predict

#Interpolate
d1 <- with(plot_df, akima::interp(x=x,y=y,z=prediction,nx=200,ny=100)) # Interpolate into a regular grid. #nx is twice size of ny as original data has max(y) ~1000 and max(x) ~ 2000
d2 <- data.table::melt(d1$z, na.rm=TRUE) # Create data frame from interpolation. na.rm=TRUE will remove NA values


pred_plot <- ggplot(d2, aes(x=Var1, y=Var2, z=value)) + geom_contour_filled() +
  theme_light()
pred_plot

#plot observed
e1 <- with(observed_data, akima::interp(x=x,y=y,z=count,nx=200,ny=100, duplicate="strip"))
e2 <- data.table::melt(e1$z, na.rm=TRUE)

obs_plot <- ggplot(e2, aes(x=Var1, y=Var2, z=value)) + geom_contour_filled() +
  theme_light()
obs_plot


#Recreate analysis but without cov2
gam_model2 <- gam(count ~ s(x ,y) + s(cov1), data = observed_data, family = quasipoisson)
summary(gam_model2)

prediction2 <- predict(gam_model2, predict_data, type="response")
plot_df <- predict_data
plot_df <- mutate(predict_data, prediction = prediction2)

dd1 <- with(plot_df, akima::interp(x=x,y=y,z=prediction,nx=200,ny=100)) # Interpolate into a regular grid. #nx is twice size of ny as original data has max(y) ~1000 and max(x) ~ 2000
dd2 <- data.table::melt(dd1$z, na.rm=TRUE) # Create data frame from interpolation. na.rm=TRUE will remove NA values

pred_plot <- ggplot(dd2, aes(x=Var1, y=Var2, z=value)) + geom_contour_filled() +
  theme_light()
pred_plot

##Check fit of gam_model
par(mfrow=c(2,2))
gam.check(gam_model)

#Change degrees of freedom for gam_model
gam_model <- gam(count ~ s(x, y, k=100) + s(cov1,k=100) + s(cov2,k=100), data = observed_data, family = quasipoisson)
summary(gam_model)
gam.check(gam_model)
