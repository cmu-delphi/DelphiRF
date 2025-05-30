#' Return params file as an R list
#'
#' Reads a parameters file. If the file does not exist, the function will create a copy of
#' '"params.json.template" and read from that.
#'
#' A params list should contain the following fields. If not included,
#' they will be filled with default values when possible.
#'
#' params$ref_lag: reference lag, after x days, the update is considered to be
#'     the response. 60 is a reasonable choice for CHNG outpatient data
#' params$input_dir: link to the input data file
#' params$test_dates: list of two elements, the first one is the start date and
#'     the second one is the end date
#' params$training_days: set it to be 270 or larger if you have enough data
#' params$num_col: the column name for the counts of the numerator, e.g. the
#'     number of COVID claims
#' params$denom_col: the column name for the counts of the denominator, e.g. the
#'     number of total claims
#' params$geo_level: character vector of "state" and "county", by default
#' params$taus: vector of considered quantiles
#' params$lambda: the level of lasso penalty
#' params$export_dir: directory to save corrected data to
#' params$lp_solver: LP solver to use in quantile_lasso(); "gurobi" or "glpk"
#'
#' @param path path to the parameters file; if not present, will try to copy the file
#'     "params.json.template"
#' @param template_path path to the template parameters file
#' @param train_models Logical; whether to train models (`TRUE`) or use existing ones (`FALSE`).
#' @param make_predictions Logical; whether to generate predictions (`TRUE`) or not (`FALSE`).
#' @param indicators string specifying a single indicator to process or all
#'     indicators ("all", default)
#'
#' @return a named list of parameters values
#'
#' @export
#'
#' @importFrom dplyr if_else
#' @importFrom jsonlite read_json
read_params <- function(path = "params.json", template_path = "params.json.template",
                        train_models=TRUE, make_predictions=TRUE,
                        indicators = c("all", unique(INDICATORS_AND_SIGNALS$indicator))) {
  if (!file.exists(path)) {file.copy(template_path, path)}
  params <- read_json(path, simplifyVector = TRUE)

  # Required parameters
  if (!("input_dir" %in% names(params)) || !dir.exists(params$input_dir)) {
    stop("input_dir must be set in `params` and exist")
  }
  params$train_models <- train_models
  params$make_predictions <- make_predictions

  indicators <- match.arg(indicators)
  if (length(indicators) != 1) stop("`indicators` arg must be a single string")
  params$indicators <- indicators

  ## Set default parameter values if not specified
  # Paths
  if (!("export_dir" %in% names(params))) {params$export_dir <- "./receiving"}
  if (!("cache_dir" %in% names(params))) {params$cache_dir <- "./cache"}

  # Parallel parameters
  if (!("parallel" %in% names(params))) {params$parallel <- FALSE}
  if (!("parallel_max_cores" %in% names(params))) {params$parallel_max_cores <- .Machine$integer.max}

  # Model parameters
  if (!("taus" %in% names(params))) {params$taus <- TAUS}
  if (!("lambda" %in% names(params))) {params$lambda <- LAMBDA}
  if (!("lp_solver" %in% names(params))) {params$lp_solver <- LP_SOLVER}
  if (!("lag_pad" %in% names(params))) {params$lag_pad <- LAG_PAD}

  # Data parameters
  if (!("num_col" %in% names(params))) {params$num_col <- "num"}
  if (!("denom_col" %in% names(params))) {params$denom_col <- "denom"}
  if (!("geo_levels" %in% names(params)) || length(params$geo_levels) == 0) {
    params$geo_levels <- c("state", "county")
  }
  if (!("value_types" %in% names(params))) {params$value_types <- c("count", "fraction")}

  # Date parameters
  if (!("training_days" %in% names(params))) {params$training_days <- TRAINING_DAYS}
  if (!("ref_lag" %in% names(params))) {params$ref_lag <- REF_LAG}
  if (!("test_dates" %in% names(params)) || length(params$test_dates) == 0) {
    start_date <- TODAY
    end_date <- TODAY
    params$test_dates <- seq(start_date, end_date, by="days")
  } else {
    if (length(params$test_dates) != 2) {
      stop("`test_dates` setting in params must be a length-2 list of dates")
    }
    params$test_dates <- seq(
      as.Date(params$test_dates[1]),
      as.Date(params$test_dates[2]),
      by="days"
    )
  }
  if (params_element_exists_and_valid(params, "training_end_date")) {
    if (as.Date(params$training_end_date) > TODAY) {
      stop("training_end_date can't be in the future")
    }
  }

  if (!("test_lag_groups" %in% names(params))) {
    params$test_lag_groups <- TEST_LAG_GROUPS_DAILY
  }


  return(params)
}

#' Create directory if not already existing
#'
#' @param path string specifying a directory to create
#'
#' @export
create_dir_not_exist <- function(path)
{
  if (!dir.exists(path)) { dir.create(path) }
}

#' Check input data for validity
#'
#' @param df A data.frame
#' @template value_type-template
#' @template num_col-template
#' @template denom_col-template
#' @template signal_suffixes-template
#' @template refd_col-template
#' @template lag_col-template
#' @template issued_col-template
#'
#' @return list of input dataframe augmented with lag column, if it
#'     didn't already exist, and character vector of one or two value
#'     column names, depending on requested `value_type`
validity_checks <- function(df, value_type, num_col, denom_col, signal_suffixes,
                            refd_col = "reference_date", lag_col = "lag", issued_col = "report_date") {
  if (!missing(signal_suffixes) && !all(is.na(signal_suffixes)) && !all(signal_suffixes == "")) {
    num_col <- paste(num_col, signal_suffixes, sep = "_")
    denom_col <- paste(denom_col, signal_suffixes, sep = "_")
  }

  # Check data type and required columns
  if (value_type == "count") {
    if ( all(num_col %in% colnames(df)) ) { value_cols=c(num_col) }
    else { stop("No valid column name detected for the count values!") }
  } else if (value_type == "fraction") {
    value_cols = c(num_col, denom_col)
    if ( !all(value_cols %in% colnames(df)) ) {
      stop("No valid column name detected for the fraction values!")
    }
  }

  # reference_date must exist in the dataset
  if ( !(refd_col %in% colnames(df)) ) {
    stop("No reference date column detected for the reference date!")
  }

  if (!(inherits(df[[refd_col]], "Date"))) {
    stop("Reference date column must be of `Date` type")
  }

  # report_date and lag should exist in the dataset
  if ( !(lag_col %in% colnames(df)) || !(issued_col %in% colnames(df)) ) {
    stop("Issue date and lag fields must exist in the input data")
  }

  if (!(inherits(df[[issued_col]], "Date"))) {
    stop("Issue date column must be of `Date` type")
  }

  if ( any(is.na(df[[lag_col]])) || any(is.na(df[[issued_col]])) ||
    any(is.na(df[[refd_col]])) ) {
    stop("Issue date, lag, or reference date fields contain missing values")
  }

  # Drop duplicate rows.
  duplicate_i <- duplicated(df)
  if (any(duplicate_i)) {
    warning("Data contains duplicate rows, dropping")
    df <- df[!duplicate_i,]
  }

  if (anyDuplicated(df[, c(refd_col, issued_col, "geo_value", "state_id")])) {
    stop("Data contains multiple entries with differing values for at",
         " least one reference date-issue date-location combination")
  }

  return(list(df = df, value_cols = value_cols))
}

#' Check available training days
#'
#' @param report_date contents of input data's `report_date` column
#' @template training_days-template
training_days_check <- function(report_date, training_days) {
  valid_training_days = as.integer(max(report_date) - min(report_date)) + 1
  if (training_days > valid_training_days) {
    warning(sprintf("Only %d days are available at most for training.", valid_training_days))
  }
}

#' Subset list of counties to those included in the 200 most populous in the US
#'
#' @importFrom dplyr select %>% arrange desc pull
#' @importFrom rlang .data
#' @importFrom utils head
get_populous_counties <- function() {
  return(
    covidcast::county_census %>%
      dplyr::select(pop = .data$POPESTIMATE2019, fips = .data$FIPS) %>%
      # Drop megacounties (states)
      filter(!endsWith(.data$fips, "000")) %>%
      arrange(desc(.data$pop)) %>%
      pull(.data$fips) %>%
      head(n=200)
  )
}

#' Write a message to the console with the current time
#'
#' @param text the body of the message to display
#'
#' @export
msg_ts <- function(text) {
  message(sprintf("%s --- %s", format(Sys.time()), text))
}

#' Generate key for identifying a value_type-signal combo
#'
#' If `signal_suffix` is not an empty string, concatenate the two arguments.
#' Otherwise, return only `value_type`.
#'
#' @template value_type-template
#' @template signal_suffix-template
make_key <- function(value_type, signal_suffix) {
  if (signal_suffix == "" || is.na(signal_suffix)) {
    key <- value_type
  } else {
    key <- paste(value_type, signal_suffix)
  }

  return(key)
}

#' Check if an element in params exists and is not missing
#'
#' @template params-template
#' @param key string indicating name of element within `params` to check
params_element_exists_and_valid <- function(params, key) {
  return(key %in% names(params) && !is.null(params[[key]]) && !is.na(params[[key]]))
}

#' Assert a logical value
#'
#' Will issue a \code{stop} command if the given statement is false.
#'
#' @param statement a logical value
#' @param msg a character string displayed as an additional message
#'
#' @export
assert <- function(statement, msg="")
{
  if (!statement)
  {
    stop(msg, call.=(msg==""))
  }
}

#' Handle Hyperparameter Selection Based on Test Lag Group
#'
#' This function selects the appropriate hyperparameter value based on whether
#' `param` is a named list or a single numeric value. If `param` is a list and
#' `test_lag_group` exists as a key, it returns the corresponding value. If not,
#' it falls back to `param$others`. If `param` has no names, it assumes a single
#' numeric value and returns it.
#'
#' @param param A numeric value or a named list containing hyperparameters for different `test_lag_group`s.
#' @param test_lag_group A character or numeric value representing the test lag group.
#'
#' @return The selected hyperparameter value as a numeric.
#' @examples
#' # Case where param is a single numeric value
#' handle_hyperparam(0.1, 1) # Returns 0.1
#'
#' # Case where param is a named list
#' param_list <- list("1" = 0.1, "2" = 0.2, "others" = 0.3)
#' handle_hyperparam(param_list, 1) # Returns 0.1
#' handle_hyperparam(param_list, 3) # Returns 0.3 (default "others")
#'
#' @export
handle_hyperparam <- function(param, test_lag_group) {
  if (is.list(param)) {
    if (is.null(names(param))) {
      stop(sprintf("Error: Parameter is not provided for test lag group %s.", test_lag_group))
    } else if (as.character(test_lag_group) %in% names(param)) {
      return(param[[as.character(test_lag_group)]])
    } else {
      return(param$others)
    }
  } else {
    return(as.numeric(param))
  }
}
