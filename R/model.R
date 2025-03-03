#' Filter Training and Testing Data Based on Lag
#'
#' This function filters the input data based on the specified test lag and lag padding.
#' The lag padding can be either a single value (applied symmetrically) or a vector of two values,
#' specifying separate left and right padding.
#'
#' @param test_lag Numeric or vector, the reference lag used for filtering.
#' @param data Data frame containing the lagged data to be filtered.
#' @param lag_pad Numeric or vector, specifying the lag padding. If a single value is provided,
#'   it is applied symmetrically. If a vector of length two is provided, the first element is used
#'   for left padding and the second for right padding.
#'
#' @importFrom rlang .data .env
#'
#' @export
data_filteration <- function(test_lag, data, lag_pad) {
  if (length(lag_pad) == 1) {
    lag_pad_l <- lag_pad
    lag_pad_r <- lag_pad
  } else {
    lag_pad_l <- lag_pad[1]
    lag_pad_r <- lag_pad[2]
    if (length(lag_pad) > 2) {
      warning("lag_pad has more than two elements. Only the first two will be used.")
    }
  }
  if (length(test_lag) == 1) {
    test_lag_l <- test_lag
    test_lag_r <- test_lag
  } else {
    test_lag_l <- test_lag[1]
    test_lag_r <- test_lag[2]
    if (length(test_lag) > 2) {
      warning("test_lag has more than two elements. Only the first two will be used.")
    }
  }
  filtered_data <- data %>%
    filter(.data$lag >= test_lag_l - lag_pad_l & .data$lag <= test_lag_r + lag_pad_r)
  return (filtered_data)
}

#' Add Square Root Scale Indicator Columns
#'
#' This function adds new columns to the dataset to indicate
#' the scale of values at the square root level. The function divides the range
#' of values into 4 square root-based bins and assigns binary indicators.
#'
#' @param df Data frame containing the data.
#' @param sqrt_max_raw The maximum value in the dataset, used to determine bin thresholds.
#' @return A data frame with additional binary indicator columns for square root scaling.
#' @export
add_sqrtscale<- function(df, sqrt_max_raw) {

  for (split in seq(0, 2)){ # Ignore the last bin
    y0_col <- paste0("sqrty", as.character(split))

    qv_pre <- sqrt_max_raw * split / 4
    qv_next <- sqrt_max_raw * (split+1) / 4

    df[[y0_col]] <- ifelse((df$value_7dav >= (qv_pre)^2) & (df$value_7dav < (qv_next)^2), 1, 0)

  }
  return (as.data.frame(df))
}

#' Fetch model and use to generate predictions/perform corrections
#'
#' @param taus vector of numeric quantiles
#' @param covariates vector of strings for the name of covariates
#' @param make_evaluation Logical
#' @param obj pre-trained model
#'
#' @template test_data-template
#' @template response-template
#'
#' @importFrom stats predict coef
#' @importFrom stringr str_interp

#'
#' @export
get_prediction <- function(test_data, taus, covariates, response, obj,
                           make_evaluation = TRUE) {
  # Ensure model object exists before prediction
  if (is.null(obj)) {
    stop("Model not found. Ensure the model is trained or loaded before prediction.")
  }

  #start_time <- Sys.time()
  # Generate predictions
  y_hat_all <- predict(obj, newx = as.matrix(test_data[covariates]))
  y_hat_all <- matrix(as.numeric(y_hat_all), nrow= dim(test_data)[1])
  test_data[paste0("predicted_tau", as.character(taus))] = y_hat_all #+ test_data[["log_value_7dav"]]

  # Perform evaluation if requested
  if (make_evaluation) {
    if (!(response %in% colnames(test_data))) {
      test_data[[response]] <- NULL
    }
    test_data <-evaluate(test_data, taus, response=response)
  }

  #end_time <- Sys.time()
  #elapsed_time <- end_time - start_time

  #coef_list = c(paste(covariates, '_coef', sep=''))
  #coef_matrix = t(as.matrix(coef(obj)))
  #coef_combined_result = data.frame(tau=taus, geo_value=geo, test_lag_group=test_lag_group,
  #                                  training_end_date=training_end_date,
  #                                  training_start_date=training_start_date,
  #                                  lambda=lambda, gamma=gamma)
  #coef_combined_result[c("intercept", coef_list)] = coef_matrix
  #coef_combined_result[coef_list] = coef_matrix

  # Add metadata columns to test data
  test_data$gamma <- attr(obj, "gamma")
  test_data$lambda <- attr(obj, "lambda")

  return (as.data.frame(test_data))
}

#' Evaluation of the test results based on WIS score
#' The WIS score calculation is based on the weighted_interval_score function
#' from the `evalcast` package from Delphi
#'
#' @param test_data dataframe with a column containing the prediction results of
#'    each requested quantile. Each row represents an update with certain
#'    (reference_date, report_date, location) combination.
#' @template taus-template
#'
#' @importFrom evalcast weighted_interval_score
#'
#' @export
evaluate <- function(test_data, taus, response) {
  n_row <- nrow(test_data)
  taus_list <- as.list(data.frame(matrix(replicate(n_row, taus), ncol=n_row)))
  pred_cols <- paste0("predicted_tau", taus)

  # Calculate WIS
  predicted_all <- as.matrix(test_data[, pred_cols])
  predicted_trans <- as.list(data.frame(t(predicted_all - test_data[[response]])))
  test_data$wis <- mapply(weighted_interval_score, taus_list, predicted_trans, 0)

  return (test_data)
}

#' Un-log predicted values
#'
#' @param test_data dataframe with a column containing the prediction results of
#'    each requested quantile. Each row represents an update with certain
#'    (reference_date, report_date, location) combination.
#' @template taus-template
#'
#' @importFrom dplyr bind_cols select starts_with
exponentiate_preds <- function(test_data, taus) {
  pred_cols <- paste0("predicted_tau", taus)

  # Drop original predictions and join on exponentiated versions
  test_data <- bind_cols(
    select(test_data, -starts_with("predicted")),
    exp(test_data[, pred_cols])
  )

  return(test_data)
}

#' Retrieve or Train a Model
#'
#' This function retrieves a cached model if available or trains a new quantile regression model if necessary.
#' The trained model is saved with additional metadata attributes.
#'
#' @param model_path Path to the cached model file.
#' @param train_data Data frame containing the training data.
#' @param response Name of the response variable.
#' @param tau Quantile to be predicted (between 0 and 1).
#' @param sqrt_max_raw Maximum raw value at square root level.
#' @template gamma-template
#' @template lambda-template
#' @template covariates-template
#' @template lp_solver-template
#' @template train_models-template

#' @return The trained or loaded model object.
#'
#' @importFrom stringr str_interp
#' @importFrom quantgen quantile_lasso
get_model <- function(model_path, train_data, covariates, response, tau,
                      sqrt_max_raw, lambda, gamma, lp_solver, train_models) {
  if (train_models || !file.exists(model_path)) {
    if (!train_models && !file.exists(model_path)) {
      warning(str_interp("user requested use of cached model but file {model_path}"),
        " does not exist; training new model")
    }
    # Quantile regression
    vec_7dav <- train_data[["value_7dav_diff"]]
    vec_slope <- train_data[["value_slope_diff"]]
    if (is.null(vec_7dav) || is.null(vec_slope)) {
      normalized_7dav_diff <- if (is.null(vec_7dav)) 1 else (vec_7dav - min(vec_7dav)) / (max(vec_7dav) - min(vec_7dav))
      normalized_slope_diff <- if (is.null(vec_slope)) 1 else (vec_slope - min(vec_slope)) / (max(vec_slope) - min(vec_slope))
      weights <- exp(-gamma * normalized_7dav_diff * normalized_slope_diff)
    } else {
      weights <- NULL
    }
    obj <- quantile_lasso(as.matrix(train_data[covariates]),
                         train_data[[response]], # - train_data[["log_value_7dav"]],
                         tau = tau,
                         lambda = lambda, standardize = TRUE, lp_solver = lp_solver, intercept=TRUE,
                         weights = weights)

    # Save model to cache.
    create_dir_not_exist(dirname(model_path))
    # add extra infomation
    attr(obj, "sqrt_max_raw") <- sqrt_max_raw
    attr(obj, "gamma") <- gamma
    attr(obj, "lambda") <- lambda
    attr(obj, "lp_solver") <- lp_solver
    saveRDS(obj, file=model_path)
  } else {
    # Load model from cache invisibly. Object has the same name as the original
    # model object, `obj`.
    print(str_interp("Loading from ${model_path}"))
    obj <- readRDS(model_path)
  }

  return(obj)
}

#' Construct filename for model with given parameters
#'
#' @template indicator-template
#' @template signal-template
#' @template geo-template
#' @template signal_suffix-template
#' @template lambda-template
#' @template gamma-template
#' @template value_type-template
#' @template test_lag_group-template
#' @param tau Decimal quantile to be predicted. Values must be between 0 and 1.
#' @param model_mode Boolean, indicates whether the file name is for a model.
#' @template training_end_date-template
#' @template training_start_date-template
#'
#' @return Path to file containing model object.
#'
#' @importFrom stringr str_interp
#'
generate_filename <- function(indicator, signal,
                              geo_level, signal_suffix, lambda, gamma,
                              training_end_date, training_days=365, geo="",
                              value_type = "", test_lag_group="", tau="", model_mode = TRUE) {
  if (lambda != "") {
    lambda <- str_interp("lambda${lambda}")
  }
  if (gamma!= "") {
    gamma <- str_interp("gamma${gamma}")
  }
  if (test_lag_group != "") {
    test_lag_group <- str_interp("lag${test_lag_group}")
  }
  if (is.numeric(training_days)) {
    training_days <- str_interp("tw${as.character(training_days)}")
  }
  if (tau != "") {
    tau <- str_interp("tau${tau}")
  }
  if (model_mode) {
    file_type <- ".rds"
  } else {
    file_type <- ".csv.gz"
  }
  training_end_date <- tryCatch(
    format(as.Date(training_end_date), "%Y%m%d"),
    error = function(e) {
      # Fallback action if formatting fails
      ""
    }
  )
  components <- c(training_end_date,
                  indicator, signal, signal_suffix, value_type,
                  geo_level, geo, test_lag_group, tau, training_days,
                  lambda, gamma)

  filename <- paste0(
    # Drop any empty strings.
    paste(components[components != ""], collapse="_"),
    file_type
  )
  return(filename)
}


#' Create Parameters List
#'
#' This function generates a list of parameter names based on the provided training data and lag list.
#' It dynamically constructs parameter names using predefined constants and incorporates log lag adjustments
#' when multiple lag values exist.
#'
#' @param train_data Data frame containing training data, including lag values.
#' @param lagged_term_list Numeric vector specifying the list of lags to be considered.
#'
#' @export
#'
#' @importFrom dplyr mutate select
#'
create_params_list <- function(train_data, lagged_term_list) {
  dayofweek <- c("Mon", "Weekends")
  params_list <- c(
    paste0(dayofweek, "_ref"),
    paste0(dayofweek, "_issue"),
    WEEK_ISSUES[1],
    Y7DAV,
    paste0("log_value_7dav_lag", lagged_term_list),
    paste0("log_delta_value_7dav_lag", lagged_term_list),
    SQRT_SCALES
  )
  # Include log lag adjustments if multiple lags exist
  if (length(unique(train_data$lag)) > 1){
    params_list <- c(params_list, LOG_LAG)
  }
  return(params_list)
}
