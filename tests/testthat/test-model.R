# Load testthat for unit testing
library(testthat)
library(evalcast)
library(quantgen)
library(dplyr)
library(stringr)

# Constants
indicator <- "chng"
signal <- "outpatient"
geo_level <- "state"
signal_suffix <- ""
lambda <- 0.1
gamma <- 0.1
sqrt_max_raw <- 1
test_lag_group <- 1
model_save_dir <- "./cache"
geo <- "pa"
value_type <- "fraction"
date_format = "%Y%m%d"
training_days <- 7
training_end_date <- as.Date("2022-01-01")
training_start_date <- training_end_date - training_days

# Generate Test Data
n_train <- 1000
n_test <- 60
main_covariate <- c("log_value_7dav")
weights_related <- c("value_7dav_diff", "value_slope_diff")
dayofweek_covariate <- c("Mon_ref")
response <- "log_value_target"
train_beta_vs <- log(rbeta(n_train, 2, 5))
test_beta_vs <- log(rbeta(n_test, 2, 5))
train_data <- data.frame(log_value_7dav = train_beta_vs,
                         log_value_target = train_beta_vs)
test_data <- data.frame(log_value_7dav = test_beta_vs,
                        log_value_target = test_beta_vs)
for (cov in weights_related){
  train_data[[cov]] <- rnorm(n_train, 0, 1)
  test_data[[cov]] <- rnorm(n_test, 0, 1)
}
for (cov in c(dayofweek_covariate)){
  train_data[[cov]] <- sample(c(0, 1), n_train, replace = TRUE)
  test_data[[cov]] <- sample(c(0, 1), n_test, replace = TRUE)
}
covariates <- c(main_covariate, dayofweek_covariate)


test_that("testing the generation of model filename prefix", {
  model_file_name <- generate_filename(indicator, signal,
                                       geo_level, signal_suffix, lambda, gamma,
                                       training_end_date=training_end_date)
  expected <- str_interp(
    "${format(training_end_date, date_format)}_chng_outpatient_state_tw365_lambda0.1_gamma0.1.rds"
  )
  expect_equal(model_file_name, expected)
})

test_that("testing prediction column exponentiation", {
  # Basic example
  input <- data.frame(
    reference_date = 5,
    predicted_tau0.1 = c(0, 1, 1),
    predicted_tau0.5 = c(2, 0, 1)
  )
  expected <- data.frame(
    reference_date = 5,
    predicted_tau0.1 = c(1, exp(1), exp(1)),
    predicted_tau0.5 = c(exp(2), 1, exp(1))
  )
  expect_equal(expected, exponentiate_preds(input, c(0.1, 0.5)))


  # Realistic test df
  pred_cols = paste0("predicted_tau", TAUS)
  test_data <- mutate(test_data, reference_date = as.Date("2022-12-02"))

  for (tau in TAUS){
    test_data[[paste0("predicted_tau", as.character(tau))]] <- log(quantile(exp(train_beta_vs), tau))
  }

  expected <- test_data
  for (col_name in pred_cols){
    expected[[col_name]] <- exp(test_data[[col_name]])
  }

  result <- exponentiate_preds(test_data, TAUS)
  expect_equal(result, expected)
})

test_that("testing generating or loading the model", {
  # Check the model that does not exist
  tau = 0.5
  gamma <- 0.1
  lambda <- 0.1
  model_file_name <- generate_filename(indicator, signal,
                                       geo_level, signal_suffix, lambda, gamma,
                                       geo=geo, test_lag_group=test_lag_group, tau=tau,
                                       training_end_date=training_end_date,
                                       training_days=training_days)
  model_path <- file.path(model_save_dir, model_file_name)

  # Generate the model and check again
  obj <- get_model(model_path, train_data, covariates, response, TAUS,
                   sqrt_max_raw, lambda, gamma, LP_SOLVER, train_models=TRUE)
  expect_true(file.exists(model_path))
  created <- file.info(model_path)$ctime
  expect_equal(attr(obj, "sqrt_max_raw"), sqrt_max_raw)

  # Check that the model was not generated again.
  # Load existed model
  obj <- get_model(model_path, train_data, covariates, response, TAUS,
                   sqrt_max_raw, lambda, gamma, LP_SOLVER, train_models=FALSE)
  expect_equal(file.info(model_path)$ctime, created)

  expect_silent(file.remove(model_path))
  # Remove the model generated at the end
})

test_that("testing making predictions", {
  # Mock model object for testing
  tau <- 0.5
  model_file_name <- generate_filename(indicator, signal,
                                       geo_level, signal_suffix, lambda, gamma,
                                       geo=geo, test_lag_group=test_lag_group, tau=tau,
                                       training_end_date=training_end_date,
                                       training_days=training_days)
  model_path <- file.path(model_save_dir, model_file_name)

  obj <- get_model(model_path, train_data, covariates, response, tau,
                   sqrt_max_raw, lambda, gamma, LP_SOLVER, train_models=TRUE)

  expect_error(get_prediction(test_data, tau, covariates, response, NULL),
               "Model not found. Ensure the model is trained or loaded before prediction.")

  result <- get_prediction(test_data, tau, covariates, response, obj)

  expect_true("predicted_tau0.5" %in% colnames(result))
  expect_equal(nrow(result), nrow(test_data))
  expect_false(any(is.na(result$predicted_tau0.5)))
  expect_true("gamma" %in% colnames(result))
  expect_true("lambda" %in% colnames(result))
  expect_equal(unique(result$gamma), gamma)
  expect_equal(unique(result$lambda), lambda)
})


test_that("testing result evaluation", {
  taus <- c(0.1, 0.5, 0.9)
  test_data <- data.frame(
    reference_date = c(as.Date("2022-01-01"), as.Date("2022-01-02")),
    report_date = c(as.Date("2022-01-02"), as.Date("2022-01-02")),
    location = "test_location",
    predicted_tau0.1 = c(log(100), NA),
    predicted_tau0.5 = c(log(100), NA),
    predicted_tau0.9 = c(log(100), NA),
    response = c(log(100), NA)
  )

  # Run the evaluate function
  result <- evaluate(test_data, taus, "response")

  # Test if the result is a data frame
  expect_s3_class(result, "data.frame")

  # Test if WIS column is added
  expect_true("wis" %in% colnames(result))

  # Test if WIS calculation is numeric and non-negative
  expect_type(result$wis, "double")
  expect_true(all(result$wis[!is.na(result$wis)] >= 0))

  # Example value here is just a placeholder
  expected_wis <- c(0, NA)
  expect_equal(result$wis, expected_wis, tolerance = 1e-6)
})


test_that("testing adding square root scale", {
  sample_data <- data.frame(value_7dav = c(1, 4, 9, 16, 25, 36, 49, 64))
  sqrt_max_raw_value <- sqrt(max(sample_data$value_7dav))

  # Run function
  transformed_data <- add_sqrtscale(sample_data, sqrt_max_raw_value)

  expect_true(all(c("sqrty0", "sqrty1", "sqrty2") %in% colnames(transformed_data)))

  expect_equal(transformed_data$sqrty0, ifelse(sample_data$value_7dav < (sqrt_max_raw_value * (1/4))^2, 1, 0))
  expect_equal(transformed_data$sqrty1, ifelse(sample_data$value_7dav >= (sqrt_max_raw_value * (1/4))^2 & sample_data$value_7dav < (sqrt_max_raw_value * (2/4))^2, 1, 0))
  expect_equal(transformed_data$sqrty2, ifelse(sample_data$value_7dav >= (sqrt_max_raw_value * (2/4))^2 & sample_data$value_7dav < (sqrt_max_raw_value * (3/4))^2, 1, 0))
})

test_that("testing data_filteration", {
  data <- data.frame(lag = -5:5, value = rnorm(11))

  # Test with lag_pad as a single value
  result <- data_filteration(test_lag = 0, data = data, lag_pad = 2)
  expected_lags <- -2:2
  expect_equal(result$lag, expected_lags)

  # Test with lag_pad as a vector (different left and right padding)
  result <- data_filteration(test_lag = 0, data = data, lag_pad = c(3, 1))
  expected_lags <- -3:1
  expect_equal(result$lag, expected_lags)

  # Test with lag_pad larger than two elements (should issue a warning)
  expect_warning(
    data_filteration(test_lag = 0, data = data, lag_pad = c(2, 1, 5)),
    "lag_pad has more than two elements. Only the first two will be used."
  )

  # Test edge case: when no data matches the filtering condition
  result <- data_filteration(test_lag = 10, data = data, lag_pad = 1)
  expect_equal(nrow(result), 0)  # Should return an empty dataframe

  # Test with test_lag as a vector
  result <- data_filteration(test_lag = c(-1, 1), data = data, lag_pad = 2)
  expected_lags <- -3:3
  expect_equal(result$lag, expected_lags)

  # Test with test_lag larger than two elements
  expect_warning(
    data_filteration(test_lag = c(-1, 0, 1), data = data, lag_pad = 2),
    "test_lag has more than two elements. Only the first two will be used."
  )

  # Test with lag_pad as a vector as while test_lag_group as a vector
  result <- data_filteration(test_lag = c(-1, 1), data = data, lag_pad = c(1, 2))
  expected_lags <- -2:3
  expect_equal(result$lag, expected_lags)

})
