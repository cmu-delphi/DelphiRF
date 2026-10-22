#' Revision Forecast Based on Training and Testing Data
#'
#' This function trains a forecast model for a given geographical location
#' and lag set. It assumes that the input training and testing data are
#' already well-filtered: the caller selects the training window, the test
#' rows and the lag group. It also evaluates forecast accuracy using the
#' weighted interval score (WIS).
#'
#' One model is fitted (or loaded from the cache) for every combination of
#' the values in `lambda` and `gamma`. With `make_predictions = TRUE` the
#' forecasts of all combinations are stacked in the result, with `lambda` and
#' `gamma` columns identifying each one.
#'
#' @param train_data A data frame containing the training dataset, with
#'   `report_date` and `target_date` columns. Rows reported after
#'   `training_end_date`, or whose target matures after it, are dropped before
#'   fitting. It must already contain the weighting features added by
#'   [add_weights_related()], with anchors built by [create_weight_anchors()]
#'   at the forecast origin.
#' @param test_data A data frame containing the testing dataset. It must
#'   contain the response column; rows whose target is not yet available carry
#'   `NA` there and receive an `NA` WIS.
#' @param smoothed_target Logical; use the seven-day-average target response
#'   when `TRUE`, or the unsmoothed target response when `FALSE`. This option
#'   selects a prepared response column; it does not smooth data in this function.
#' @param taus A numeric vector specifying the quantiles for prediction
#'   intervals. Duplicate levels are removed while preserving first-occurrence
#'   order.
#' @param params_list Character vector of predictor columns. When `NULL`, it
#'   is built by [create_params_list()].
#' @param lagged_term_list A numeric vector specifying the lag values for which reported
#'                 values on the reference date are considered in model training.
#' @param temporal_resol Character; either `"daily"` or `"weekly"` resolution.
#' @param model_backend Quantile-regression backend. `"quantreg"` is the
#'   default, using standardized predictors and weighted lasso fits.
#'   `"quantgen"` retains the previous optional solver.
#' @param time_limit Optional solver time limit passed to the `quantgen`
#'   backend. It is ignored by the default `quantreg` backend.
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
#' @template working_scale-template
#' @template training_end_date-template
#' @template training_days-template
#'
#' @return A tibble containing forecasts and, when the completed revision value
#'   is available, evaluation scores: `wis_working`, the WIS on the model's
#'   working (log) scale, and `wis_reporting`, the WIS of the reporting-scale
#'   predictions against the target on the reporting scale. It is empty when
#'   `make_predictions` is `FALSE` or no test row has all its predictors.
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
                              working_scale,
                              model_save_dir=default_model_save_dir(),
                              indicator="testdata", signal="",
                              geo_level="state", signal_suffix="",
                              training_end_date="",
                              training_days=365,
                              train_models = TRUE,
                              make_predictions=TRUE,
                              model_backend = c("quantreg", "quantgen"),
                              time_limit = NULL) {

  taus <- normalize_taus(taus)
  model_backend <- match.arg(model_backend)
  working_scale <- check_working_scale(working_scale)

  if (!all(c("report_date", "target_date") %in% names(train_data))) {
    stop("train_data needs report_date and target_date columns, so that only rows ",
         "whose target is known by training_end_date are used.")
  }
  training_end_date <- validate_training_end_date(
    training_end_date,
    default = max(train_data$report_date)
  )
  # A training row must be reported, and its target must have matured, by the
  # training end date; a later target would be information from the future.
  train_data <- train_data %>%
    filter(report_date <= as.Date(training_end_date),
           target_date <= as.Date(training_end_date))
  if (nrow(train_data) < 10) {
    stop("Not enough data to train.")
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

  check_lagged_term_list(lagged_term_list, temporal_resol)
  if (is.null(params_list)) {
    params_list <- create_params_list(train_data, lagged_term_list, temporal_resol)
  }

  # Settings a cached model must share with this call before it is reused
  # (see get_model()); the response, predictors and data are added there.
  fit_contract <- list(
    training_end_date = as.character(as.Date(training_end_date)),
    training_days = as.numeric(training_days),
    test_lag_group = as.character(test_lag_group),
    temporal_resol = temporal_resol,
    target_lag = if ("target_lag" %in% names(train_data)) {
      sort(unique(as.numeric(train_data$target_lag)))
    } else NA_real_,
    # The forecast context the observation weights were anchored to.
    weight_anchors = if (all(c("ref_7dav_value", "ref_slope_value") %in% names(train_data))) {
      anchors <- unique(as.data.frame(train_data)[c("lag", "ref_7dav_value", "ref_slope_value")])
      anchors <- anchors[do.call(order, unname(anchors)), , drop = FALSE]
      rownames(anchors) <- NULL
      rlang::hash(anchors)
    } else NA_character_
  )

  if (smoothed_target) {
    response <- "log_value_target_7dav"
  } else {
    response <- "log_value_target"
  }

  basic_cols <- c("reference_date", "lag", "report_date")
  extra_cols <- c("value_slope_diff", "value_7dav_diff")

  selected_train_cols <- c(basic_cols, params_list, extra_cols, response)
  train_data <- train_data[, unique(selected_train_cols)] %>% drop_na()

  if (!nrow(train_data)) {
    warning(sprintf(
      "No training rows after preprocessing [geo=%s lag_group=%s]; skipping",
      geo, test_lag_group
    ))
    return(tibble::tibble())
  }

  if (make_predictions) {
    test_data <- test_data %>%
      drop_na(!!!syms(c(params_list, basic_cols))) %>%
      select(dplyr::all_of(c(basic_cols, params_list, response)))
  }

  # Fit (or load) one model per lambda-gamma combination; predict with each
  # only when requested, so a training-only call still saves every model.
  test_data_list <- list()
  for (l in lambda) {
    for (g in gamma) {
      model_path <- generate_filename(indicator=indicator, signal=signal,
                                      geo_level=geo_level, signal_suffix=signal_suffix,
                                      lambda=l, gamma=g, training_end_date=training_end_date,
                                      training_days=training_days,
                                      geo=geo, value_type=value_type,
                                      test_lag_group=test_lag_group, tau="_all",
                                      model_save_dir=model_save_dir)
      if (model_backend != "quantgen") {
        model_path <- sub("\\.rds$", paste0("_backend_", model_backend, ".rds"),
                          model_path)
      }
      obj <- get_model(model_path, train_data, params_list, response, taus,
                       l, g, lp_solver, train_models,
                       model_backend = model_backend, time_limit = time_limit,
                       fit_contract = fit_contract)

      # Every test row may lack a predictor; the models are still fitted
      # and saved, and this group then has no forecasts.
      if (make_predictions && nrow(test_data)) {
        test_data_list <- append(test_data_list, list(
          get_prediction(test_data, taus, params_list, response, obj,
                         make_evaluation=TRUE)
        ))
      }
    }
  }

  result <- bind_rows(test_data_list)
  if (nrow(result)) {
    result <- exponentiate_preds(result, taus, working_scale = working_scale)
    result <- add_reporting_wis(result, taus, response, working_scale)
  }
  return(tibble::as_tibble(result))
}

validate_genuine_event_options <- function(df, genuine_training,
                                           genuine_testing) {
  if (!is.logical(genuine_training) || length(genuine_training) != 1L ||
      is.na(genuine_training)) {
    stop("genuine_training must be one non-missing logical value.")
  }
  if (!is.logical(genuine_testing) || length(genuine_testing) != 1L ||
      is.na(genuine_testing)) {
    stop("genuine_testing must be one non-missing logical value.")
  }
  if ((genuine_training || genuine_testing) &&
      !("genuine_event" %in% names(df))) {
    stop(paste0(
      "A logical genuine_event column is required when genuine_training or ",
      "genuine_testing is TRUE. Set both parameters to FALSE to use all rows."
    ))
  }
  if ((genuine_training || genuine_testing) &&
      !is.logical(df$genuine_event)) {
    stop("genuine_event must be a logical column containing TRUE, FALSE, or NA.")
  }
  invisible(TRUE)
}

# Resolve and validate the training cutoff shared by the forecasting entry
# points. Only DelphiRF() has an explicit forecast origin, so only that caller
# can enforce that the cutoff is not later than the origin.
validate_training_end_date <- function(training_end_date, default,
                                       forecast_origin = NULL) {
  if (length(training_end_date) == 0L ||
      (length(training_end_date) == 1L &&
       identical(as.character(training_end_date), ""))) {
    training_end_date <- default
  }
  if (length(training_end_date) != 1L) {
    stop("training_end_date must be empty or one valid date.", call. = FALSE)
  }
  training_end_date <- tryCatch(
    suppressWarnings(as.Date(training_end_date)),
    error = function(e) as.Date(NA)
  )
  if (is.na(training_end_date)) {
    stop("training_end_date must be empty or one valid date.", call. = FALSE)
  }
  if (!is.null(forecast_origin)) {
    forecast_origin <- tryCatch(
      suppressWarnings(as.Date(forecast_origin)),
      error = function(e) as.Date(NA)
    )
    if (length(forecast_origin) != 1L || is.na(forecast_origin)) {
      stop("forecast_origin must be one valid date.", call. = FALSE)
    }
    if (training_end_date > forecast_origin) {
      stop("training_end_date cannot be after the forecast origin.", call. = FALSE)
    }
  }
  training_end_date
}

#' Cross-Validation for Forecast Revision
#'
#' This function performs cross-validation to optimize WIS for forecast revision.
#' It trains forecast models for a given geographical location and a lag set.
#' It evaluates different combinations of lambda, gamma, and lag padding values
#' across multiple folds.
#'
#' The function first keeps rows inside the training window: `report_date` on
#' or before `training_end_date`, and `target_date` in
#' (`training_end_date - training_days`, `training_end_date`]. Rows are then
#' assigned to folds in turn (row 1 to fold 1, row 2 to fold 2, and so on), so
#' the folds are not ordered in time: a validation fold can lie between
#' training rows. The function is intended for hyperparameter tuning within
#' one training window, not for prospective evaluation. Candidates are compared
#' by mean `wis_working`, the WIS on the working (log) scale.
#'
#' Fold models are tuning by-products: unless `model_save_dir` is given, they
#' are written to a temporary directory that is removed when the function
#' returns, so tuning never touches the shared model cache.
#'
#' @param df Data frame containing the input data, with the weighting features
#'   added by [add_weights_related()] (anchors from [create_weight_anchors()]
#'   at `training_end_date`).
#' @param test_lag Numeric vector specifying the test lag(s).
#' @param taus Numeric vector of quantiles to estimate. Duplicate levels are
#'   removed while preserving first-occurrence order.
#' @param lambda_candidates Numeric vector of lambda values to try.
#' @param gamma_candidates Numeric vector of gamma values to try.
#' @param lag_pad_candidates Numeric vector of lag padding values to try.
#' @param lagged_term_list Optional numeric vector of positive lag values, in
#'   days, to include in the model.
#' @param params_list Optional character vector naming predictor columns. When
#'   `NULL`, it is built by [create_params_list()].
#' @param smoothed_target Logical; use the seven-day-average target response
#'   when `TRUE`, or the unsmoothed target response when `FALSE`.
#' @param temporal_resol Character; either "daily" or "weekly" resolution.
#' @param n_folds Integer, number of cross-validation folds.
#' @param genuine_training Logical; if `TRUE`, use only rows whose
#'   `genuine_event` value is `TRUE` in each cross-validation training fold.
#' @param genuine_testing Logical; if `TRUE`, use only rows whose
#'   `genuine_event` value is `TRUE` in each validation fold.
#' @param model_backend Quantile-regression backend passed to
#'   [revision_forecast()].
#' @param time_limit Optional solver time limit passed to the `quantgen`
#'   backend.
#' @param model_save_dir Directory for the fold models. `NULL` (default) uses
#'   a temporary directory removed on exit.
#' @template indicator-template
#' @template lp_solver-template
#' @template signal-template
#' @template geo_level-template
#' @template geo-template
#' @template signal_suffix-template
#' @template value_type-template
#' @template working_scale-template
#' @template training_end_date-template
#' @template training_days-template
#'
#' @return A list with `best_lambda`, `best_gamma` and `best_lag_pad`, the
#'   combination with the lowest mean WIS across folds (all three `NULL` when
#'   no combination could be scored), and `scores`, a data frame with one row
#'   per candidate: `lambda`, `gamma`, `lag_pad` and `mean_wis_working`
#'   (`NA` when the candidate could not be scored).
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
                                 working_scale,
                                 model_save_dir=NULL,
                                 indicator="testdata", signal="",
                                 geo_level="state", signal_suffix="",
                                 training_end_date="",
                                 training_days=365,
                                 n_folds = 5,
                                 genuine_training = TRUE,
                                 genuine_testing = TRUE,
                                 model_backend = c("quantreg", "quantgen"),
                                 time_limit = NULL) {
  taus <- normalize_taus(taus)
  model_backend <- match.arg(model_backend)
  working_scale <- check_working_scale(working_scale)
  validate_genuine_event_options(df, genuine_training, genuine_testing)
  if (is.null(model_save_dir)) {
    model_save_dir <- tempfile("delphirf-cv-")
    on.exit(unlink(model_save_dir, recursive = TRUE), add = TRUE)
  }

  training_end_date <- validate_training_end_date(
    training_end_date,
    default = max(df$report_date, na.rm = TRUE)
  )
  training_start_date <- training_end_date - training_days
  df <- df %>%
    filter(
      report_date <= training_end_date,
      target_date > training_start_date,
      target_date <= training_end_date
    )

  folds <- rep(seq(1, n_folds), length.out = nrow(df))

  best_lambda <- NULL
  best_gamma <- NULL
  best_pad <- NULL
  best_score <- Inf
  candidate_scores <- list()

  for (lambda in lambda_candidates) {
    for (gamma in gamma_candidates) {
      for (lag_pad in lag_pad_candidates){

        scores <- c()
        for (fold in 1:n_folds) {
          validation_idx <- which(folds == fold, arr.ind = TRUE)
          validation_data <- df[validation_idx, ]
          training_data <- df[-validation_idx, ]
          if (genuine_training) {
            training_data <- training_data[
              !is.na(training_data$genuine_event) & training_data$genuine_event,
              , drop = FALSE
            ]
          }
          if (genuine_testing) {
            validation_data <- validation_data[
              !is.na(validation_data$genuine_event) & validation_data$genuine_event,
              , drop = FALSE
            ]
          }

          train_data <- data_filteration(test_lag, training_data, lag_pad)
          val_data <- data_filteration(test_lag, validation_data, 0)

          if (nrow(train_data) < 10L || nrow(val_data) == 0L) next

          if (length(test_lag) == 1) {
            prefix_for_lag <- as.character(test_lag)
          } else {
            prefix_for_lag <- paste0(as.character(test_lag[1]), "_", as.character(test_lag[2]))
          }

          results <- revision_forecast(
            train_data = train_data, test_data = val_data, taus = taus,
            smoothed_target = smoothed_target,
            lagged_term_list = lagged_term_list,
            params_list = params_list,
            temporal_resol = temporal_resol,
            lambda = lambda, gamma = gamma, lp_solver = lp_solver,
            test_lag_group = prefix_for_lag, geo = geo,
            value_type = value_type, working_scale = working_scale,
            model_save_dir = model_save_dir,
            indicator = indicator, signal = signal, geo_level = geo_level,
            signal_suffix = signal_suffix,
            training_end_date = training_end_date,
            training_days = training_days,
            train_models = TRUE, make_predictions = TRUE,
            model_backend = model_backend, time_limit = time_limit
          )

          scores <- c(scores, mean(results$wis_working, na.rm=TRUE))
        }

        avg_score <- mean(scores, na.rm=TRUE)
        candidate_scores[[length(candidate_scores) + 1L]] <- data.frame(
          lambda = lambda, gamma = gamma, lag_pad = lag_pad,
          mean_wis_working = if (is.finite(avg_score)) avg_score else NA_real_)

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
  return(list(best_lambda = best_lambda, best_gamma = best_gamma, best_lag_pad = best_pad,
              scores = do.call(rbind, candidate_scores)))
}

#' Revision Forecast for a Specific Location
#'
#' This function performs a forecasting operation using historical data, applying
#' hyper-parameters and lag-based adjustments for different test lag groups.
#'
#' With origin `c = testing_start_date` and training end date `e` (equal to
#' `c` unless `training_end_date` is given):
#'
#' - training rows have `report_date <= e` and a target that matured inside the
#'   training window, `e - training_days < target_date <= e`;
#' - test rows are every row of `df` with `report_date >= c`. There is no upper
#'   limit, so pass only the releases you want forecast: to forecast a window
#'   of `w` days, keep rows with `report_date < c + w` in `df`.
#'
#' Rows with a reporting lag at or beyond the target lag are already removed
#' by [data_preprocessing()].
#'
#' If every gap between the distinct `lag` values in `df` is 7 days,
#' `temporal_resol` is switched to `"weekly"` with a message.
#'
#' Each test lag group is fitted separately on the training rows whose lag is
#' within `lag_pad` of the group. The default groups,
#' [TEST_LAG_GROUPS_DAILY] and [TEST_LAG_GROUPS_WEEKLY], cover lags 0-59
#' (daily) and 0-70 (weekly); for a different target lag, pass
#' `test_lag_groups` that cover lags `0` to `L - 1`.
#'
#' @param df A data frame from [data_preprocessing()], with `target_date` and
#'   `report_date` columns.
#' @param testing_start_date A character or Date object giving the forecast
#'   origin.
#' @param genuine_training Logical; if `TRUE`, train only on rows whose
#'   `genuine_event` value is `TRUE`. These are raw reports with usable
#'   observed values, including unchanged repeated reports; raw-but-missing
#'   reports and grid-created rows are excluded.
#' @param genuine_testing Logical; if `TRUE`, predict only rows whose
#'   `genuine_event` value is `TRUE`.
#' @param model_backend Quantile-regression backend. `"quantreg"` is the
#'   default, using standardized predictors and weighted lasso fits.
#'   `"quantgen"` preserves the previous optional implementation.
#' @param time_limit Optional solver time limit passed to the `quantgen`
#'   backend.
#' @param taus A numeric vector of quantiles for probabilistic forecasting.
#'   Duplicate levels are removed while preserving first-occurrence order.
#' @param lagged_term_list Optional numeric vector of positive lag values, in
#'   days, to include as predictors.
#' @param params_list Optional character vector naming predictor columns. When
#'   `NULL`, it is built by [create_params_list()].
#' @param test_lag_groups A vector of test lag groups to process. When `NULL`,
#'   daily or weekly defaults are selected from `temporal_resol`.
#' @param lag_pad A numeric or named list of lag padding values (default: `LAG_PAD`).
#' @param smoothed_target Logical; use the seven-day-average target response
#'   when `TRUE`, or the unsmoothed target response when `FALSE`.
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
#' @template working_scale-template
#' @template training_end_date-template
#' @template training_days-template
#'
#' @details Lag groups with fewer than ten eligible training rows, or with no
#'   test rows, are skipped rather than stopping forecasts for the remaining
#'   groups.
#' @return A tibble containing the available forecasts across requested lag
#'   groups.
#'
#' @importFrom dplyr %>% filter bind_rows
#' @export
DelphiRF <- function(df, testing_start_date, taus=TAUS,
                     test_lag_groups=NULL,
                     smoothed_target=TRUE,
                     lagged_term_list=NULL,
                     params_list=NULL,
                     lambda=LAMBDA, gamma=GAMMA, lag_pad=LAG_PAD,
                     temporal_resol="daily",
                     lp_solver=LP_SOLVER,
                     geo="ma", value_type="count",
                     working_scale,
                     model_save_dir=default_model_save_dir(),
                     indicator="testdata", signal="",
                     geo_level="state", signal_suffix="",
                     training_end_date="",
                     training_days=365,
                     train_models = TRUE,
                     make_predictions = TRUE,
                     genuine_training=TRUE,
                     genuine_testing=TRUE,
                     model_backend = c("quantreg", "quantgen"),
                     time_limit = NULL) {

  taus <- normalize_taus(taus)
  testing_start_date <- as.Date(testing_start_date)
  model_backend <- match.arg(model_backend)
  working_scale <- check_working_scale(working_scale)
  validate_genuine_event_options(df, genuine_training, genuine_testing)

  effective_training_end_date <- validate_training_end_date(
    training_end_date,
    default = testing_start_date,
    forecast_origin = testing_start_date
  )

  # ---- Auto-detect temporal resolution from lag spacing ----
  if ("lag" %in% names(df)) {
    lag_vals <- sort(unique(df$lag))
    
    if (length(lag_vals) >= 2) {
      lag_diffs <- unique(diff(lag_vals))
      
      # Detect weekly spacing
      if (length(lag_diffs) == 1 && lag_diffs == 7) {
        if (temporal_resol != "weekly") message("Auto-detected weekly temporal resolution from lag spacing.")
        temporal_resol <- "weekly"
      }
    }
  }

  if (is.null(test_lag_groups)) {
    if (temporal_resol == "daily") {
      test_lag_groups = TEST_LAG_GROUPS_DAILY
    }
    else if (temporal_resol == "weekly") {
      test_lag_groups = TEST_LAG_GROUPS_WEEKLY
    }
  }

  geo_train_data <- df %>%
    dplyr::filter(.data$report_date <= testing_start_date) %>%
    dplyr::filter(.data$report_date <= effective_training_end_date) %>%
    dplyr::filter(.data$target_date <= effective_training_end_date) %>%
    dplyr::filter(.data$target_date > effective_training_end_date - training_days)
  if (genuine_training) {
    geo_train_data <- geo_train_data %>%
      dplyr::filter(!is.na(.data$genuine_event) & .data$genuine_event)
  }
  # Similarity weights compare training rows with the reporting context at
  # this origin: one anchor per lag from every row reported by the origin,
  # whether or not its target has matured or it is a genuine report.
  weight_anchors <- create_weight_anchors(df, testing_start_date)
  geo_train_data <- add_weights_related(geo_train_data, weight_anchors)

  geo_test_data <- df %>%
    dplyr::filter(.data$report_date >= testing_start_date)
  if (genuine_testing) {
    geo_test_data <- geo_test_data %>%
      dplyr::filter(!is.na(.data$genuine_event) & .data$genuine_event)
  }

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
    # Sparse reporting cadences can leave an individual lag group with too
    # few genuine revisions. Skip that group instead of aborting the entire
    # location/origin forecast; revision_forecast() requires at least 10 rows.
    if (nrow(train_data) < 10) next
    test_data <- data_filteration(test_lag, geo_test_data, 0)
    if (nrow(test_data) == 0) next

    results <- revision_forecast(
      train_data = train_data, test_data = test_data, taus = taus,
      smoothed_target = smoothed_target,
      lagged_term_list = lagged_term_list, params_list = params_list,
      temporal_resol = temporal_resol,
      lambda = l, gamma = g, lp_solver = lp_solver,
      test_lag_group = test_lag_group, geo = geo, value_type = value_type,
      working_scale = working_scale,
      model_save_dir = model_save_dir, indicator = indicator, signal = signal,
      geo_level = geo_level, signal_suffix = signal_suffix,
      training_end_date = as.character(effective_training_end_date),
      training_days = training_days, train_models = train_models,
      make_predictions = make_predictions,
      model_backend = model_backend, time_limit = time_limit
    )

    test_data_list <- append(test_data_list, list(results))
  }
  return(tibble::as_tibble(bind_rows(test_data_list)))

}
