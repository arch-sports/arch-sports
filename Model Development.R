library(ggplot2)

#Four assumptions of linear regression models
# 1. Linearity: Relationship between x and y is linear
# 2. Independence: Observations are independent of each other
# 3. Homoscedasticity: Variance of residual is the same for any value of X
# 4. Normality: For any fixed value of X, Y is normally distributed

if (!file.exists("lax_to_jfk/lax_to_jfk.csv")) {
  url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"
  
  download.file(url, destfile = "lax_to_jfk.tar.gz")
  untar("lax_to_jfk.tar.gz")
}

sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(),
                                         'DivArrDelay' = col_number()))

aa_delays <- sub_airline %>%
  filter(CarrierDelay != "NA",
         Reporting_Airline == "AA")

linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes,
                   data = aa_delays)

print(summary(linear_model))

new_depdelay <- data.frame(
  DepDelayMinutes = c(12, 19, 24))

pred_lm <- predict(linear_model, newdata = new_depdelay, interval = 'confidence')

print(pred_lm)

#Multiple linear regression

mlr <- lm(ArrDelayMinutes ~ CarrierDelay + LateAircraftDelay,
          data = aa_delays)

print(summary(mlr))

CarrierDelay <- c(10, 20, 30)
LateAircraftDelay <- c(20, 60, 30)
new_multidelay <- data.frame(CarrierDelay, LateAircraftDelay)

pred_mlr <- predict(mlr, newdata = new_multidelay, interval = 'confidence')
print(pred_mlr)

#Assessing Models Visually
linear_plot <- ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() +
  stat_smooth(method = 'lm', col = 'red')

print(linear_plot)
plot(linear_model, ask = FALSE)

#Polynomial Regression
time <- 6:19
temp <- c(4,6,7,9,10,11,11.5,12,12,11.5,11,10,9,8)
plot(time, temp, ask = FALSE)

polyfit2 <- lm(temp ~ poly(time, 2, raw = TRUE))
print(summary(polyfit2))

poly_plot <- ggplot(data = NULL, aes(time, temp)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2))
print(poly_plot)

#Assessing models numerically

#Mean Squared Error(MSE)
#MSE = Average((actual - predicted)^2)
mse <- mean(linear_model$residuals^2)
print(mse)

rmse <- sqrt(mse)
print(rmse)

#R-Squared/R^2
#Coefficient of Determination
#R^2 = (1 - (MSE of regression line)/(MSE of data average))
#Use summary([[model]])$r.squared
#ex: summary(linear_model)$r.squared
print(summary(linear_model)$r.squared)




#Visualizing
visualize_plot <- ggplot(aa_delays, aes(x = DepDelayMinutes,
                                        y = ArrDelayMinutes)) +
  geom_point() +
  stat_smooth(method = 'lm',
              col = 'red',
              se = FALSE)
print(visualize_plot)

#Add the predicted values in the original dataset
aa_delays$predicted <- predict(linear_model)
predicted_plot <- ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'red') +
  geom_point(aes(y = predicted), color = 'green') +
  geom_segment(aes(xend = DepDelayMinutes, yend = predicted), alpha = .2)
print(predicted_plot)



























