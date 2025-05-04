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
kept_bins <- "sqrty0"
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
                                       training_end_date=training_end_date,
                                       model_save_dir=model_save_dir)
  expected <- str_interp(
    file.path(
      model_save_dir,
      "chng_outpatient_state/${format(training_end_date, date_format)}_tw365_lambda0.1_gamma0.1.rds"
    )
  )
  expect_equal(model_file_name, expected)

  unlink(file.path(model_save_dir, "chng_outpatient_state"), recursive = TRUE)
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
  tau <- 0.5
  gamma <- 0.1
  lambda <- 0.1
  model_file_name <- generate_filename(indicator, signal,
                                       geo_level, signal_suffix, lambda, gamma,
                                       geo=geo, test_lag_group=test_lag_group, tau=tau,
                                       training_end_date=training_end_date,
                                       training_days=training_days,
                                       model_save_dir=model_save_dir)
  # Generate the model and check again
  obj <- get_model(model_file_name, train_data, covariates, response, TAUS,
                   sqrt_max_raw, kept_bins,
                   lambda, gamma, LP_SOLVER, train_models=TRUE)
  expect_true(file.exists(model_file_name))
  created <- file.info(model_file_name)$ctime
  expect_equal(attr(obj, "sqrt_max_raw"), sqrt_max_raw)

  # Check that the model was not generated again.
  # Load existed model
  obj <- get_model(model_file_name, train_data, covariates, response, TAUS,
                   sqrt_max_raw, kept_bins,
                   lambda, gamma, LP_SOLVER, train_models=FALSE)
  expect_equal(file.info(model_file_name)$ctime, created)

  unlink(file.path(model_save_dir, "chng_outpatient_state"), recursive = TRUE)
})

test_that("testing making predictions", {
  # Mock model object for testing
  tau <- 0.5
  model_file_name <- generate_filename(indicator, signal,
                                       geo_level, signal_suffix, lambda, gamma,
                                       geo=geo, test_lag_group=test_lag_group, tau=tau,
                                       training_end_date=training_end_date,
                                       training_days=training_days,
                                       model_save_dir=model_save_dir)

  obj <- get_model(model_file_name, train_data, covariates, response, tau,
                   sqrt_max_raw, kept_bins,
                   lambda, gamma, LP_SOLVER, train_models=TRUE)

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
  unlink(file.path(model_save_dir, "chng_outpatient_state"), recursive = TRUE)
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


test_that("testing adding square root scale with rare bin removal", {
  sample_data <- data.frame(value_7dav = c(1, 4, 9, 16, 25, 36, 49, 64))
  sqrt_max_raw_value <- sqrt(max(sample_data$value_7dav))

  result <- add_sqrtscale(sample_data, sqrt_max_raw_value, rare_thresh = 0.01)
  transformed_data <- result$data
  kept_bins <- result$kept_bins

  # Check that only the kept bins are present
  expect_true(all(kept_bins %in% colnames(transformed_data)))

  # Check content of each kept bin
  if ("sqrty0" %in% kept_bins) {
    expect_equal(transformed_data$sqrty0,
                 ifelse(sample_data$value_7dav < (sqrt_max_raw_value * (1/4))^2, 1, 0))
  }
  if ("sqrty1" %in% kept_bins) {
    expect_equal(transformed_data$sqrty1,
                 ifelse(sample_data$value_7dav >= (sqrt_max_raw_value * (1/4))^2 &
                          sample_data$value_7dav < (sqrt_max_raw_value * (2/4))^2, 1, 0))
  }
  if ("sqrty2" %in% kept_bins) {
    expect_equal(transformed_data$sqrty2,
                 ifelse(sample_data$value_7dav >= (sqrt_max_raw_value * (2/4))^2 &
                          sample_data$value_7dav < (sqrt_max_raw_value * (3/4))^2, 1, 0))
  }
})

test_that("add_sqrtscale handles bin creation, rare removal, and return structure correctly", {
  # --- Test 1: Basic bin creation ---
  sample_data1 <- data.frame(value_7dav = c(1, 4, 9, 16, 25, 36, 49, 64))
  sqrt_max1 <- sqrt(max(sample_data1$value_7dav))
  result1 <- add_sqrtscale(sample_data1, sqrt_max1, rare_thresh = 0.01)
  df1 <- result1$data
  bins1 <- result1$kept_bins

  expect_true(all(paste0("sqrty", 0:2) %in% bins1))
  expect_true(all(bins1 %in% colnames(df1)))

  # --- Test 2: Rare bins dropped ---
  sample_data2 <- data.frame(value_7dav = c(1, 2, 3, 4, 5, 6, 7, 64))
  sqrt_max2 <- sqrt(max(sample_data2$value_7dav))
  result2 <- add_sqrtscale(sample_data2, sqrt_max2, rare_thresh = 0.2)
  df2 <- result2$data
  bins2 <- result2$kept_bins

  expect_lt(length(bins2), 3)
  expect_true(all(bins2 %in% colnames(df2)))

  # --- Test 3: All bins dropped if all are rare ---
  sample_data3 <- data.frame(value_7dav = c(100, 200, 300))
  sqrt_max3 <- sqrt(max(sample_data3$value_7dav))
  result3 <- add_sqrtscale(sample_data3, sqrt_max3, rare_thresh = 0.9)
  df3 <- result3$data
  bins3 <- result3$kept_bins

  expect_equal(length(bins3), 0)
  expect_false(any(grepl("^sqrty", colnames(df3))))

  # --- Test 4: Edge threshold bin retention ---
  sample_data4 <- data.frame(value_7dav = rep(c(1, 16, 36), times = c(3, 3, 4)))
  sqrt_max4 <- sqrt(max(sample_data4$value_7dav))
  result4 <- add_sqrtscale(sample_data4, sqrt_max4, rare_thresh = 0.3)
  df4 <- result4$data
  bins4 <- result4$kept_bins

  expect_true(length(bins4) >= 1)
  expect_true(all(bins4 %in% colnames(df4)))

  # --- Test 5: Return structure correctness ---
  sample_data5 <- data.frame(value_7dav = c(1, 4, 9, 16))
  sqrt_max5 <- sqrt(max(sample_data5$value_7dav))
  result5 <- add_sqrtscale(sample_data5, sqrt_max5)

  expect_true(is.list(result5))
  expect_true("data" %in% names(result5))
  expect_true("kept_bins" %in% names(result5))
  expect_true(is.data.frame(result5$data))
  expect_true(is.character(result5$kept_bins))
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
