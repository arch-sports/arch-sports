library(tidymodels)
library(tidyverse)
library(Metrics)

if (!file.exists("lax_to_jfk/lax_to_jfk.csv")) {
  url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"
  
  download.file(url, destfile = "lax_to_jfk.tar.gz")
  untar("lax_to_jfk.tar.gz")
}

sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))

flight_delays <- sub_airline %>%
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0)) %>%
  select(c(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay, DayOfWeek, Month))

set.seed(1234)
flight_split <- initial_split(flight_delays)
train_data <- training(flight_split)
test_data <- testing(flight_split)

flight_split2 <- initial_split(flight_delays, prop = 0.8)
train_data2 <- training(flight_split2)
test_data2 <- testing(flight_split2)

lm_spec <- linear_reg() %>%
  set_engine(engine = 'lm')
print(lm_spec)

train_fit <- lm_spec %>%
  fit(ArrDelayMinutes ~ DepDelayMinutes, data = train_data)
print(train_fit)

train_results <- train_fit %>%
  predict(new_data = train_data) %>%
  mutate(truth = train_data$ArrDelayMinutes)
print(head(train_results))

test_results <- train_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$ArrDelayMinutes)

rmse_train1 <- rmse(train_results$truth, train_results$.pred)
print(rmse_train1)

rmse_test1 <- rmse(train_results$truth, train_results$.pred)
print(rmse_test1)

rsq_train1 <- rsq(train_results, truth = truth, estimate = .pred)
print(rsq_train1)

rsq_test1 <- rsq(test_results, truth = truth, estimate = .pred)
print(rsq_test1)


test_results_plot <- test_results %>%
  mutate(train = 'testing') %>%
  bind_rows(train_results %>% mutate(train = 'training')) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = 'orange',
              size = 1.5) +
  geom_point(color = '#006EA1',
             alpha = 0.5) +
  facet_wrap(~train) +
  labs(x = 'Truth',
       y = 'Predicted Arrival Delays (min)')
print(test_results_plot)

train_fit2 <- lm_spec %>%
  fit(ArrDelayMinutes ~ DepDelayMinutes, 
      data = train_data2)

train_results2 <- train_fit2 %>%
  predict(new_data = train_data2) %>%
  mutate(truth = train_data2$ArrDelayMinutes)

test_results2 <- train_fit2 %>%
  predict(new_data = test_data2) %>%
  mutate(truth = test_data2$ArrDelayMinutes)

rmse_train2 <- sqrt(mean((train_results2$truth - train_results2$.pred)^2))
rsq_train2 <- rsq(train_results2, truth = truth, estimate = .pred)
print(rmse_train2)
print(rsq_train2)


rmse_test2 <- sqrt(mean((test_results2$truth - test_results2$.pred)^2))
rsq_test2 <- rsq(test_results2, truth = truth, estimate = .pred)
print(rmse_test2)
print(rsq_test2)

train_fit3 <- lm_spec %>%
  fit(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay + WeatherDelay,
      data = train_data)

train_results3 <- train_fit3 %>%
  predict(new_data = train_data) %>%
  mutate(truth = train_data$ArrDelayMinutes)

test_results3 <-train_fit3 %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$ArrDelayMinutes)

rmse_train3 <- sqrt(mean((train_results3$truth - train_results3$.pred)^2))
rsq_train3 <- rsq(train_results3, truth = truth, estimate = .pred)
print(rmse_train3)
print(rsq_train3)

rmse_test3 <- sqrt(mean((test_results3$truth - test_results3$.pred)^2))
rsq_test3 <- rsq(test_results3, truth = truth, estimate = .pred)
print(rmse_test3)
print(rsq_train3)

train_fit4 <- lm_spec %>%
  fit(ArrDelayMinutes ~ poly(DepDelayMinutes, 3),
      data = train_data)

train_results4 <- train_fit4 %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$ArrDelayMinutes)

test_results4 <- train_fit4 %>%
  # Make the predictions and save the predicted values
  predict(new_data = test_data) %>%
  # Create a new column to save the true values
  mutate(truth = test_data$ArrDelayMinutes)

rmse_train4 <- sqrt(mean((train_results4$truth - train_results4$.pred)^2))
rsq_train4 <- rsq(train_results4, truth = truth, estimate = .pred)
print(rmse_train4)
print(rsq_train4)

rmse_test4 <- sqrt(mean((test_results4$truth - test_results4$.pred)^2))
rsq_test4 <- rsq(test_results4, truth = truth, estimate = .pred)
print(rmse_test4)
print(rsq_test4)

set.seed(1234)
cv_folds <- vfold_cv(train_data, v = 10)
results <- fit_resamples(lm_spec,
                         ArrDelayMinutes ~ DepDelayMinutes,
                         resamples = cv_folds)

results_cv <- results %>% collect_metrics()
print(results_cv)
rmse_train5 <- results_cv[1,3]
rsq_train5 <- results_cv[2,3]
print(rmse_train5)
print(rsq_train5)

cv_folds_3 <- vfold_cv(train_data, v = 3)
results <- fit_resamples(
  lm_spec, 
  ArrDelayMinutes ~ DepDelayMinutes, 
  resamples = cv_folds_3)
results_cv <- results %>% collect_metrics()
results_cv
rmse_train6 <- results_cv[1,3]
rsq_train6 <- results_cv[2,3]
print(rmse_train6)
print(rsq_train6)

flight_recipe <- recipe(ArrDelayMinutes ~ ., data = train_data)

#Ridge regularization
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>%
  set_engine('glmnet')

ridge_wf <- workflow() %>%
  add_recipe(flight_recipe)

ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)


ridge_fit_output <- ridge_fit %>%
  extract_fit_parsnip() %>%
  tidy()
print(ridge_fit_output)

#Lasso regularization
lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine('glmnet')

lasso_wf <- workflow() %>%
  add_recipe(flight_recipe)

lasso_fit <- lasso_wf %>%
  add_model(lasso_spec) %>%
  fit(data = train_data)

lasso_fit_output <- lasso_fit %>%
  extract_fit_parsnip() %>%
  tidy()
print(lasso_fit_output)

#Elastic Net regularization
elasticnet_spec <- linear_reg(penalty = 0.1, mixture = 0.3) %>%
  set_engine('glmnet')

elasticnet_wf <- workflow() %>%
  add_recipe(flight_recipe)

elasticnet_fit <- elasticnet_wf %>%
  add_model(elasticnet_spec) %>%
  fit(data = train_data)

elasticnet_fit_output <- elasticnet_fit %>%
  extract_fit_parsnip() %>%
  tidy()
print(elasticnet_fit_output)

#Elastic Net with all variables of training data
flight_recipe <- recipe(ArrDelayMinutes ~ ., data = train_data)

elasticnet_spec <- linear_reg(penalty = 0.2, mixture = 0.5) %>%
  set_engine('glmnet')

elasticnet_wf <- workflow() %>%
  add_recipe(flight_recipe)

elasticnet_fit <- elasticnet_wf %>%
  add_model(elasticnet_spec) %>%
  fit(data = train_data)

elasticnet_fit_output <- elasticnet_fit %>%
  extract_fit_parsnip() %>%
  tidy()
print(elasticnet_fit_output)

#Grid Search
tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine('glmnet')

lasso_wf <- workflow() %>%
  add_recipe(flight_recipe)

flight_cvfolds <- vfold_cv(train_data)

lambda_grid <- grid_regular(levels = 50,
                            penalty(range = c(-3, 0.3)))

lasso_grid <- tune_grid(
  lasso_wf %>% add_model(tune_spec),
  resamples = flight_cvfolds,
  grid = lambda_grid)

results_grid <- show_best(lasso_grid, metric = 'rmse')
rmse_train7 <- results_grid[1,4]
print(rmse_train7)

library(plotly)
lasso_grid_plot <- lasso_grid %>%
  collect_metrics() %>%
  filter(.metric == 'rmse') %>%
  ggplot(aes(penalty, mean)) +
  geom_line(size = 1, color = 'red') +
  scale_x_log10() +
  ggtitle("RMSE")

print(ggplotly(lasso_grid_plot))

results_grid <- show_best(lasso_grid, metric = 'rsq')
rsq_train7 <- results_grid[1,4]
print(rsq_train7)

lasso_grid_plot <- lasso_grid %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  ggplot(aes(penalty, mean)) +
  geom_line(size=1, color="red") +
  scale_x_log10() +
  ggtitle("RSQ")

print(ggplotly(lasso_grid_plot))


#Comparing models performance
model <- c('lm1', 'lm2', 'mlr', 'poly', 'cv1', 'cv2', 'reg')
rsq <- c(rsq_train1$.estimate, rsq_train2$.estimate, rsq_train3$.estimate, rsq_train4$.estimate, rsq_train5$mean, rsq_train6$mean, rsq_train7$mean )
rmse <- c(rmse_train1, rmse_train2, rmse_train3, rmse_train4,rmse_train5$mean, rmse_train6$mean, rmse_train7$mean)
traindata_cm <- data.frame(model, rsq, rmse)
print(traindata_cm)






