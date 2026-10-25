# Return a CRAN-compliant, persistent cache directory on supported R versions.
# Keep the previous session cache as a compatibility fallback for R < 4.0.
default_model_save_dir <- function() {
  if (getRversion() >= "4.0.0") {
    return(tools::R_user_dir("DelphiRF", "cache"))
  }
  file.path(tempdir(), "DelphiRF", "models")
}

#' Create a directory and any missing parent directories
#'
#' @param path String specifying a directory to create.
#' @return `path`, invisibly.
#'
#' @export
create_dir_not_exist <- function(path)
{
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Could not create directory: ", path)
  }
  invisible(path)
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

#' Write a message to the console with the current time
#'
#' @param text the body of the message to display
#' @return `NULL`, invisibly, after emitting the timestamped message.
#'
#' @export
msg_ts <- function(text) {
  invisible(message(sprintf("%s --- %s", format(Sys.time()), text)))
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

#' Assert a logical value
#'
#' Will issue a \code{stop} command if the given statement is false.
#'
#' @param statement a logical value
#' @param msg a character string displayed as an additional message
#' @return `NULL`, invisibly, when `statement` is `TRUE`.
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
#' it falls back to `param$others`; a list with neither an entry for
#' `test_lag_group` nor an `others` entry is an error. If `param` is not a
#' list, it is returned as a number.
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
    } else if ("others" %in% names(param)) {
      return(param$others)
    } else {
      stop(sprintf(paste0("No value for test lag group %s and no 'others' entry; ",
                          "add one of them to the hyperparameter list."),
                   test_lag_group))
    }
  } else {
    return(as.numeric(param))
  }
}
