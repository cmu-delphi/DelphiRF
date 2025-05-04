#' Revision Forecast Based on Training and Testing Data
#'
#' This function trains a forecast model for a given geographical location
#' and lag set. It assumes that the input training and testing data are
#' already well-filtered. It also evaluates forecast accuracy using the
#' weighted interval score (WIS).
#'
#' @param train_data A data frame containing the training dataset.
#' @param test_data A data frame containing the testing dataset.
#' @param smoothed_target A logical value indicating whether the target variable should be smoothed.
#' @param taus A numeric vector specifying the quantiles for prediction intervals.
#' @param params_list A list of model parameters used for training and prediction.
#' @param lagged_term_list A numeric vector specifying the lag values for which reported
#'                 values on the reference date are considered in model training.
#' @template train_models-template
#' @template make_predictions-template
#' @template model_save_dir-template
#' @template indicator-template
#' @template signal-template
#' @template test_lag_group-template
#' @template lp_solver-template
#' @template geo_level-template
#' @template geo-template
#' @template signal_suffix-template
#' @template lambda-template
#' @template gamma-template
#' @template value_type-template
#' @template test_lag_group-template
#' @template training_end_date-template
#' @template training_days-template
#'
#' @return A list containing:
#'   \item{test_data}{The test dataset with added forecast evaluations.}
#'   \item{coefs}{Model coefficients from the trained model.}
#'
#' @importFrom dplyr %>% select bind_rows
#' @importFrom tidyr drop_na
#' @importFrom rlang syms
#' @export
revision_forecast <- function(train_data, test_data, taus,
                              smoothed_target=TRUE,
                              lagged_term_list=NULL,
                              params_list=NULL,
                              temporal_resol="daily",
                              lambda = 0.1, gamma = 0.1,
                              lp_solver=LP_SOLVER, test_lag_group="",
                              geo="ma", value_type="count",
                              model_save_dir="./receiving",
                              indicator="testdata", signal="",
                              geo_level="state", signal_suffix="",
                              training_end_date="",
                              training_days =365,
                              train_models = TRUE,
                              make_predictions=TRUE) {



  if (nrow(train_data) < 10) {
    stop("Not enough data to train.")
  }

  if (as.character(training_end_date) == "") {
    training_end_date <- max(train_data$report_date)
  } else {
    train_data <- train_data %>%
      filter(report_date <= as.Date(training_end_date))
  }

  if (is.null(lagged_term_list)) {
    if (temporal_resol == "daily") {
      lagged_term_list <- c(1, 7)
    } else if (temporal_resol == "weekly") {
      lagged_term_list <- c(7, 14)
    } else {
      stop("Invalid temporal_resol value. Choose 'weekly' or 'daily'.")
    }
  }

  if (is.null(params_list)) {
    params_list <- create_params_list(train_data, lagged_term_list)
  }

  if (smoothed_target) {
    response <- "log_value_target_7dav"
  } else {
    response <- "log_value_target"
  }

  basic_cols <- c("reference_date", "lag", "report_date")
  extra_cols <- c("value_slope_diff", "value_7dav_diff")

  #Preprocess the data for a specific location
  #train_data <- add_weights_related(train_data)

  test_data_list <- list()

  if (train_models) {
    sqrt_max_raw <- sqrt(max(train_data$value_7dav, na.rm=TRUE))
    train_result <- add_sqrtscale(train_data, sqrt_max_raw)
    train_data <- train_result$data
    kept_bins <- train_result$kept_bins
    #for (col in kept_bins) {
    #  proportion <- sum(train_data[[col]]) / nrow(train_data)
    #  cat(sprintf("Sum of %s: %.4f\n", col, proportion))
    }
    train_data <- train_data[, c(basic_cols, params_list, extra_cols, kept_bins, response)] %>% drop_na()
  }

  # pre-process the test data with max_raw
  if (make_predictions) {
    # Get model path
    model_path <- generate_filename(indicator=indicator, signal=signal,
                                    geo_level=geo_level, signal_suffix=signal_suffix,
                                    lambda=lambda[1], gamma=gamma[1],
                                    training_end_date=training_end_date,
                                    training_days=training_days,
                                    geo=geo, value_type=value_type,
                                    test_lag_group=test_lag_group, tau="_all",
                                    model_save_dir=model_save_dir)
    # Get the trained_model
    obj <- get_model(model_path, train_data, params_list, response, taus,
                     sqrt_max_raw, kept_bins,
                     lambda[1], gamma[1], lp_solver, train_models)

    sqrt_max_raw <- attr(obj, "sqrt_max_raw")
    kept_bins <- attr(obj, "kept_bins")
    #test_data <- add_sqrtscale(test_data, sqrt_max_raw)
    test_data <- add_sqrtscale_test(test_data, sqrt_max_raw, kept_bins)
    test_data <- test_data %>%
      drop_na(!!!syms(c(params_list, kept_bins, basic_cols))) %>%
      select(all_of(c(basic_cols, params_list, kept_bins, response)))
    test_data <- get_prediction(test_data, taus, params_list, response, obj,
                                make_evaluation=TRUE)
    test_data_list <- append(test_data_list, list(test_data))
  }

  # Iterate through additional lambda and gamma values
  for (l in lambda) {
    for (g in gamma) {
      if (l == lambda[1] & g == gamma[1]) next
      # Get model path
      model_path <- generate_filename(indicator=indicator, signal=signal,
                                           geo_level=geo_level, signal_suffix=signal_suffix,
                                           lambda=l, gamma=g, training_end_date=training_end_date,
                                           training_days=training_days,
                                           geo=geo, value_type=value_type,
                                           test_lag_group=test_lag_group, tau="_all",
                                           model_save_dir=model_save_dir)


      # Get the trained_model
      obj <- get_model(model_path, train_data, params_list, response, taus, sqrt_max_raw,
                       l, g, lp_solver, train_models)

      if (make_predictions) {
        test_data <- get_prediction(test_data, taus, params_list, response, obj,
                                    make_evaluation=TRUE)
        test_data_list <- append(test_data_list, list(test_data))
      }
    }
  }

  return(as.data.frame(bind_rows(test_data_list)))
}

#' Cross-Validation for Forecast Revision
#'
#' This function performs cross-validation to optimize WIS for forecast revision.
#' It trains forecast models for a given geographical location and a lag set.
#' It evaluates different combinations of lambda, gamma, and lag padding values
#' across multiple folds.
#'
#' @param df Data frame containing the input data.
#' @param test_lag Numeric vector specifying the test lag(s).
#' @param taus Numeric vector of quantiles to estimate.
#' @param lambda_candidates Numeric vector of lambda values to try.
#' @param gamma_candidates Numeric vector of gamma values to try.
#' @param lag_pad_candidates Numeric vector of lag padding values to try.
#' @param lagged_term_list Optional list of lagged terms to be included in the model.
#' @param params_list Optional list of additional parameters for the model.
#' @param smoothed_target A logical value indicating whether the target variable should be smoothed.
#' @param temporal_resol Character; either "daily" or "weekly" resolution.
#' @param n_folds Integer, number of cross-validation folds.
#' @template train_models-template
#' @template make_predictions-template
#' @template model_save_dir-template
#' @template indicator-template
#' @template lp_solver-template
#' @template signal-template
#' @template geo_level-template
#' @template geo-template
#' @template signal_suffix-template
#' @template lambda-template
#' @template gamma-template
#' @template value_type-template
#' @template training_end_date-template
#' @template training_days-template
#'
#' @importFrom dplyr filter
#'
#' @export
#'
cv_revision_forecast <- function(df, test_lag, taus=TAUS,
                                 smoothed_target=TRUE,
                                 lagged_term_list=NULL,
                                 params_list=NULL,
                                 temporal_resol="daily",
                                 lambda_candidates = c(0.01, 0.1, 1),
                                 gamma_candidates = c(0.1, 1, 10),
                                 lag_pad_candidates = c(0, 1, 2, 3),
                                 lp_solver=LP_SOLVER,
                                 geo="ma", value_type="count",
                                 model_save_dir="./receiving",
                                 indicator="testdata", signal="",
                                 geo_level="state", signal_suffix="",
                                 training_end_date="",
                                 training_days=365,
                                 n_folds = 5) {
  if (as.character(training_end_date) == "") {
    training_end_date <- max(df$report_date)
  } else {
    train_data <- train_data %>%
      filter(report_date <= as.Date(training_end_date))
  }

  folds <- rep(seq(1, n_folds), length.out = nrow(df))

  best_lambda <- NULL
  best_gamma <- NULL
  best_pad <- NULL
  best_score <- Inf

  for (lambda in lambda_candidates) {
    for (gamma in gamma_candidates) {
      for (lag_pad in lag_pad_candidates){

        scores <- c()
        for (fold in 1:n_folds) {
          validation_idx <- which(folds == fold, arr.ind = TRUE)
          validation_data <- df[validation_idx, ]
          training_data <- df[-validation_idx, ]

          train_data <- data_filteration(test_lag, training_data, lag_pad)
          val_data <- data_filteration(test_lag, validation_data, 0)

          if (length(test_lag) == 1) {
            prefix_for_lag <- as.character(test_lag)
          } else {
            prefix_for_lag <- paste0(as.character(test_lag[1]), "_", as.character(test_lag[2]))
          }

          results <- revision_forecast(train_data, val_data, taus,
                                     smoothed_target, lagged_term_list,
                                     params_list, temporal_resol,
                                     lambda, gamma, lp_solver, prefix_for_lag,
                                     geo, value_type, model_save_dir,
                                     indicator, signal, geo_level,
                                     signal_suffix, training_end_date,
                                     training_days,
                                     train_models=TRUE,
                                     make_predictions=TRUE)

          scores <- c(scores, mean(results$wis, na.rm=TRUE))
        }

        avg_score <- mean(scores, na.rm=TRUE)

        if (is.na(avg_score)) {
          avg_score <- Inf
        }

        if (avg_score < best_score) {
          best_score <- avg_score
          best_gamma <- gamma
          best_pad <- lag_pad
          best_lambda <- lambda
          }
      }# lag_pad candidates
    } # gamma candidates
  } # lambda candidates
  return(list(best_lambda = best_lambda, best_gamma = best_gamma, best_lag_pad = best_pad))
}

#' Revision Forecast for a Specific Location
#'
#' This function performs a forecasting operation using historical data, applying
#' hyper-parameters and lag-based adjustments for different test lag groups.
#'
#' @param df A data frame containing historical data with `target_date` and `report_date` columns.
#' @param testing_start_date A character or Date object specifying the start date for testing.
#' @param taus A numeric vector of quantiles for probabilistic forecasting.
#' @param lagged_term_list A list of lagged terms to be used as predictors.
#' @param params_list A list of model parameters for forecasting.
#' @param test_lag_groups A vector of test lag groups to process (default: `TEST_LAG_GROUPS`).
#' @param lag_pad A numeric or named list of lag padding values (default: `LAG_PAD`).
#' @param smoothed_target A logical value indicating whether the target variable should be smoothed.
#' @param temporal_resol Character; either "daily" or "weekly" resolution.
#' @template train_models-template
#' @template make_predictions-template
#' @template model_save_dir-template
#' @template indicator-template
#' @template lp_solver-template
#' @template signal-template
#' @template geo_level-template
#' @template geo-template
#' @template signal_suffix-template
#' @template lambda-template
#' @template gamma-template
#' @template value_type-template
#' @template training_end_date-template
#' @template training_days-template
#'
#' @importFrom dplyr %>% filter bind_rows
#' @export
DelphiRF <- function(df, testing_start_date, taus=TAUS,
                     test_lag_groups=TEST_LAG_GROUPS,
                     smoothed_target=TRUE,
                     lagged_term_list=NULL,
                     params_list=NULL,
                     lambda=LAMBDA, gamma=GAMMA, lag_pad=LAG_PAD,
                     temporal_resol="daily",
                     lp_solver=LP_SOLVER,
                     geo="ma", value_type="count",
                     model_save_dir="./receiving",
                     indicator="testdata", signal="",
                     geo_level="state", signal_suffix="",
                     training_end_date="",
                     training_days=365,
                     train_models = TRUE,
                     make_predictions = TRUE) {

  testing_start_date <- as.Date(testing_start_date)

  geo_train_data <- df %>%
    dplyr::filter(.data$target_date <= testing_start_date) %>%
    dplyr::filter(.data$target_date > testing_start_date - training_days)
  # Add weighting-related features to training data
  geo_train_data <- add_weights_related(geo_train_data)

  geo_test_data <- df %>%
    dplyr::filter(.data$report_date >= testing_start_date)

  test_data_list <- list()

  # Split the test lag group if it's a range (e.g., "15-21")
  for (test_lag_group in test_lag_groups) {

    info <- strsplit(as.character(test_lag_group), "-")[[1]]
    if (length(info) == 1) {
      test_lag <- as.integer(info[1])
    } else {
      test_lag <- c(as.integer(info[1]), as.integer(info[2]))
    }

    # Retrieve hyperparameters for the given test lag group
    l_p <- handle_hyperparam(lag_pad, test_lag_group)
    l <- handle_hyperparam(lambda, test_lag_group)
    g <- handle_hyperparam(gamma, test_lag_group)

    train_data <- data_filteration(test_lag, geo_train_data, l_p)
    if (nrow(train_data) == 0) next
    test_data <- data_filteration(test_lag, geo_test_data, 0)
    if (nrow(test_data) == 0) next

    results <- revision_forecast(train_data, test_data, taus,
                                 smoothed_target, lagged_term_list,
                                 params_list, temporal_resol,
                                 l, g, lp_solver, test_lag_group,
                                 geo, value_type, model_save_dir,
                                 indicator, signal, geo_level,
                                 signal_suffix, as.character(testing_start_date),
                                 training_days, train_models,
                                 make_predictions)

    test_data_list <- append(test_data_list, list(results))
  }
  return(as.data.frame(bind_rows(test_data_list)))

}




