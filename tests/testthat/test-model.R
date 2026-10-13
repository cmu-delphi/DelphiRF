# Load testthat for unit testing
library(testthat)
library(dplyr)
library(stringr)

set.seed(20261005)

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
model_save_dir <- tempfile("delphirf-model-cache-")
geo <- "pa"
value_type <- "fraction"
date_format = "%Y%m%d"
training_days <- 7
training_end_date <- as.Date("2022-01-01")

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

test_that("model APIs default to the persistent user cache", {
  expected_default <- quote(default_model_save_dir())
  expected_dir <- if (getRversion() >= "4.0.0") {
    tools::R_user_dir("DelphiRF", "cache")
  } else {
    file.path(tempdir(), "DelphiRF", "models")
  }

  expect_identical(formals(generate_filename)$model_save_dir, expected_default)
  expect_identical(formals(revision_forecast)$model_save_dir, expected_default)
  expect_identical(formals(cv_revision_forecast)$model_save_dir, expected_default)
  expect_identical(formals(DelphiRF)$model_save_dir, expected_default)
  expect_identical(default_model_save_dir(), expected_dir)
})

test_that("generating a model filename does not create its cache directory", {
  cache_dir <- tempfile("delphirf-filename-only-")

  path <- generate_filename(
    indicator, signal, geo_level, signal_suffix, lambda, gamma,
    training_end_date = training_end_date,
    model_save_dir = cache_dir
  )

  expect_false(dir.exists(cache_dir))
  expect_true(startsWith(path, cache_dir))
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
    predicted_tau0.1 = exp(c(0, 1, 1)) - 1,
    predicted_tau0.5 = exp(c(2, 0, 1)) - 1
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
    expected[[col_name]] <- exp(test_data[[col_name]]) - 1
  }

  result <- exponentiate_preds(test_data, TAUS, value_type = "fraction",
                               fraction_input = "value")
  expect_equal(result, expected)

  ratio_result <- exponentiate_preds(
    input, c(0.1, 0.5), value_type = "fraction",
    fraction_input = "numerator_denominator"
  )
  expect_equal(
    ratio_result$predicted_tau0.1,
    exp(input$predicted_tau0.1)
  )
  expect_equal(
    ratio_result$predicted_tau0.5,
    exp(input$predicted_tau0.5)
  )
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
                   lambda, gamma, "glpk", train_models=TRUE)
  expect_true(file.exists(model_file_name))
  created <- file.info(model_file_name)$ctime
  expect_equal(attr(obj, "sqrt_max_raw"), sqrt_max_raw)

  # Check that the model was not generated again.
  # Load existed model
  obj <- get_model(model_file_name, train_data, covariates, response, TAUS,
                   sqrt_max_raw, kept_bins,
                   lambda, gamma, "glpk", train_models=FALSE)
  expect_equal(file.info(model_file_name)$ctime, created)

  unlink(file.path(model_save_dir, "chng_outpatient_state"), recursive = TRUE)
})

test_that("quantreg backend implements non-negative case weights", {
  x <- matrix(1, nrow = 3, ncol = 1,
              dimnames = list(NULL, "constant_predictor"))
  y <- c(0, 10, 10)
  weighted <- fit_quantreg_lasso(
    x, y, taus = 0.5, lambda = 0.1, case_weights = c(10, 1, 1)
  )
  unweighted <- fit_quantreg_lasso(
    x, y, taus = 0.5, lambda = 0.1, case_weights = rep(1, 3)
  )
  newx <- matrix(1, nrow = 1, ncol = 1)
  expect_lt(abs(predict(weighted, newx)), 1e-5)
  expect_equal(as.numeric(predict(unweighted, newx)), 10, tolerance = 1e-5)
  expect_error(
    fit_quantreg_lasso(x, y, 0.5, case_weights = c(1, -1, 1)),
    "non-negative"
  )
})

test_that("default quantreg backend supports multiple quantiles and records backend", {
  x <- matrix(seq_len(30), ncol = 2,
              dimnames = list(NULL, c("x1", "x2")))
  y <- seq_len(15)
  fit <- fit_quantreg_lasso(x, y, taus = c(0.1, 0.5, 0.9), lambda = 0.1)
  prediction <- predict(fit, x[1:4, , drop = FALSE])
  expect_equal(dim(prediction), c(4L, 3L))

  path <- tempfile(fileext = ".rds")
  obj <- get_model(
    path, train_data, covariates, response, 0.5,
    sqrt_max_raw, kept_bins, lambda, gamma, "glpk", TRUE,
    model_backend = "quantreg"
  )
  expect_s3_class(obj, "delphirf_quantreg_lasso")
  expect_equal(attr(obj, "model_backend"), "quantreg")
  unlink(path)
})

test_that("quantreg backend handles rank-deficient predictors", {
  x <- cbind(
    original = seq_len(30),
    duplicate = 2 * seq_len(30),
    constant = 1
  )
  y <- seq_len(30) + rep(c(-1, 0, 1), 10)

  fit <- fit_quantreg_lasso(x, y, taus = c(0.1, 0.5, 0.9))
  predictions <- predict(fit, newx = x[1:3, , drop = FALSE])

  expect_equal(dim(predictions), c(3L, 3L))
  expect_true(all(is.finite(predictions)))
  expect_true(length(fit$dropped_predictors) >= 2L)
  expect_equal(nrow(fit$coefficients), ncol(x) + 1L)
})

test_that("optional quantgen backend records its backend", {
  skip_if_not_installed("quantgen")
  legacy_path <- tempfile(fileext = ".rds")
  legacy <- get_model(
    legacy_path, train_data, covariates, response, 0.5,
    sqrt_max_raw, kept_bins, lambda, gamma, "glpk", TRUE,
    model_backend = "quantgen"
  )
  expect_equal(attr(legacy, "model_backend"), "quantgen")
  expect_true(is.finite(as.numeric(predict(
    legacy, newx = as.matrix(test_data[1, covariates, drop = FALSE])
  ))))
  unlink(legacy_path)
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
                   lambda, gamma, "glpk", train_models=TRUE)

  expect_error(get_prediction(test_data, tau, covariates, response, NULL),
               "Model not found. Ensure the model is trained or loaded before prediction.")

  result <- get_prediction(test_data, tau, covariates, response, obj)

  expect_s3_class(result, "tbl_df")
  expect_true("predicted_tau0.5" %in% colnames(result))
  expect_equal(nrow(result), nrow(test_data))
  expect_false(any(is.na(result$predicted_tau0.5)))
  expect_true("gamma" %in% colnames(result))
  expect_true("lambda" %in% colnames(result))
  expect_equal(unique(result$gamma), gamma)
  expect_equal(unique(result$lambda), lambda)
  unlink(file.path(model_save_dir, "chng_outpatient_state"), recursive = TRUE)
})


test_that("evaluate uses twice mean pinball loss and preserves missing rows", {
  taus <- c(0.1, 0.5, 0.9)
  test_data <- data.frame(
    reference_date = c(as.Date("2022-01-01"), as.Date("2022-01-02")),
    report_date = c(as.Date("2022-01-02"), as.Date("2022-01-02")),
    location = "test_location",
    predicted_tau0.1 = c(8, NA),
    predicted_tau0.5 = c(9, NA),
    predicted_tau0.9 = c(13, NA),
    response = c(10, NA)
  )

  result <- evaluate(test_data, taus, "response")

  expect_s3_class(result, "data.frame")
  expect_true("wis" %in% colnames(result))
  expect_type(result$wis, "double")
  expected <- mean(c(0.4, 1.0, 0.6))
  expect_equal(result$wis, c(expected, NA_real_), tolerance = 1e-12)
})

test_that("median-only WIS equals absolute error for either error sign", {
  test_data <- data.frame(
    predicted_tau0.5 = c(8, 12, 10),
    response = c(10, 10, 10)
  )

  result <- evaluate(test_data, 0.5, "response")

  expect_equal(result$wis, c(2, 2, 0))
  expect_equal(result$wis, abs(test_data$predicted_tau0.5 - test_data$response))
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

test_that("revision_forecast with train_models=FALSE loads cached model and produces identical predictions", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE))

  set.seed(42)
  nn_train <- 200
  nn_test  <- 30

  make_rf_data <- function(nn, start_date) {
    dates <- seq(as.Date(start_date), by = "day", length.out = nn)
    data.frame(
      reference_date   = dates,
      report_date      = dates + 3L,
      lag              = 3L,
      value_7dav       = runif(nn, 0, 1),
      log_value_7dav   = rnorm(nn),
      log_value_target = rnorm(nn),
      value_7dav_diff  = rnorm(nn),
      value_slope_diff = rnorm(nn),
      Mon_ref          = sample(c(0L, 1L), nn, replace = TRUE)
    )
  }

  train_data <- make_rf_data(nn_train, "2021-01-01")
  test_data  <- make_rf_data(nn_test,  "2021-07-20")

  rf_args <- list(
    taus             = 0.5,
    smoothed_target  = FALSE,
    params_list      = c("log_value_7dav", "Mon_ref"),
    temporal_resol   = "daily",
    lambda           = 0.1,
    gamma            = 0.1,
    lp_solver        = "glpk",
    model_save_dir   = tmpdir,
    indicator        = "test",
    signal           = "sig",
    geo_level        = "state",
    geo              = "pa",
    training_days    = nn_train,
    make_predictions = TRUE
  )

  result_fit <- do.call(
    revision_forecast,
    c(list(train_data = train_data, test_data = test_data, train_models = TRUE), rf_args)
  )
  expect_s3_class(result_fit, "tbl_df")
  expect_gt(nrow(result_fit), 0)

  result_cached <- do.call(
    revision_forecast,
    c(list(train_data = train_data, test_data = test_data, train_models = FALSE), rf_args)
  )
  expect_gt(nrow(result_cached), 0)
  expect_equal(result_fit$predicted_tau0.5, result_cached$predicted_tau0.5)
})

test_that("DelphiRF keeps the testing cutoff out of training", {
  cutoff <- as.Date("2024-01-10")
  n_train <- 12L
  n_test <- 2L
  values <- seq_len(n_train + n_test)
  df <- data.frame(
    reference_date = seq(cutoff - n_train, by = "day", length.out = n_train + n_test),
    report_date = c(seq(cutoff - n_train, cutoff - 1L, by = "day"),
                    seq(cutoff, by = "day", length.out = n_test)),
    target_date = rep(cutoff, n_train + n_test),
    genuine_event = rep(TRUE, n_train + n_test),
    lag = rep(8L, n_train + n_test),
    value_7dav = values,
    value_7dav_lag7 = values,
    log_value_7dav = log1p(values),
    log_value_7dav_lag7 = log1p(values)
  )

  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, ...) {
      test_data$latest_training_report_date <- max(train_data$report_date)
      test_data
    },
    .package = "DelphiRF"
  )

  result <- DelphiRF(
    df,
    testing_start_date = cutoff,
    test_lag_groups = 8,
    lag_pad = 0,
    training_days = 30
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$latest_training_report_date < cutoff))
  expect_equal(unique(result$latest_training_report_date), cutoff - 1L)
})

test_that("cross-validation respects its training window", {
  training_end <- as.Date("2024-02-01")
  target_dates <- c(
    rep(as.Date("2024-01-01"), 10),
    rep(seq(training_end - 9L, training_end, by = "day"), each = 3),
    rep(training_end + 1L, 10)
  )
  nn <- length(target_dates)
  df <- data.frame(
    reference_date = as.Date("2023-12-01") + seq_len(nn),
    report_date = rep(training_end, nn),
    target_date = target_dates,
    lag = rep(8L, nn)
  )
  seen <- new.env(parent = emptyenv())
  seen$training_target_dates <- list()
  seen$validation_target_dates <- list()

  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, ...) {
      seen$training_target_dates[[length(seen$training_target_dates) + 1L]] <-
        train_data$target_date
      seen$validation_target_dates[[length(seen$validation_target_dates) + 1L]] <-
        test_data$target_date
      data.frame(wis = 0)
    },
    .package = "DelphiRF"
  )

  cv_revision_forecast(
    df, test_lag = 8L, taus = 0.5,
    lambda_candidates = 0.1, gamma_candidates = 0.1,
    lag_pad_candidates = 0, n_folds = 2,
    training_end_date = training_end, training_days = 10,
    genuine_training = FALSE, genuine_testing = FALSE
  )

  used_dates <- c(
    unlist(seen$training_target_dates),
    unlist(seen$validation_target_dates)
  )
  expect_gt(length(used_dates), 0L)
  expect_true(all(used_dates > training_end - 10L))
  expect_true(all(used_dates <= training_end))
})

test_that("quantile rearrangement sorts each prediction row", {
  predictions <- matrix(c(
    0.4, 0.2, 0.1, 0.9, 0.3,
    1.0, 1.0, 0.5, 2.0, 1.5
  ), nrow = 2, byrow = TRUE)
  rearranged <- rearrange_quantile_predictions(predictions)
  expect_equal(rearranged[1, ], c(0.1, 0.2, 0.3, 0.4, 0.9))
  expect_equal(rearranged[2, ], c(0.5, 1.0, 1.0, 1.5, 2.0))
  expect_true(all(apply(rearranged, 1, function(x) all(diff(x) >= 0))))
})

test_that("get_prediction returns monotone quantiles", {
  test_data <- data.frame(x = c(0, 1), truth = c(0, 0))
  fake_model <- structure(list(), gamma = 0.1, lambda = 0.1)
  local_mocked_bindings(
    predict = function(object, newx) {
      matrix(rep(c(0.4, 0.2, 0.1, 0.9, 0.3), each = nrow(newx)),
             nrow = nrow(newx))
    },
    .package = "DelphiRF"
  )
  result <- get_prediction(test_data, c(.1, .2, .5, .8, .9), "x", "truth",
                           fake_model, make_evaluation = FALSE)
  expect_equal(as.numeric(result[1, paste0("predicted_tau", c(.1,.2,.5,.8,.9))]),
               c(0.1, 0.2, 0.3, 0.4, 0.9))
})
