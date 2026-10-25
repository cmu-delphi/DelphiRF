#' Constants
#'
#' Default settings and column names shared by the DelphiRF functions.
#'
#' @description
#' - `TAUS`: quantile levels forecast by default. Together with the median they
#'   give the 50%, 80%, 95% and 98% central prediction intervals.
#' - `REF_LAG`: a default target lag \eqn{L}, in days.
#' - `LAG_PAD`: default number of neighbouring reporting lags added on each
#'   side of a test lag group when selecting its training rows (see
#'   [data_filteration()]).
#' - `TRAINING_DAYS`: a default training-window length, in days.
#' - `LAMBDA`: default lasso penalty for the quantile regression.
#' - `GAMMA`: default decay rate of the observation weights built by
#'   [add_weights_related()].
#' - `LP_SOLVER`: linear-programming solver used only by the optional
#'   `quantgen` backend.
#' - `YITL`: column holding the log-transformed provisional value,
#'   `log_value_raw`.
#' - `SLOPE`: column holding the log 7-day slope, `log_7dav_slope`.
#' - `Y7DAV`: column holding the log-transformed 7-day average,
#'   `log_value_7dav`; always a model predictor.
#' - `RESPONSE`: column holding the unsmoothed log target, `log_value_target`.
#' - `LOG_LAG`: column holding the inverse-lag predictor `inv_log_lag`, equal
#'   to `1 / (lag + 1)`. It is added when the training data span more than one
#'   lag.
#' - `WEEKDAYS_ABBR`: weekday labels used for the `_ref` and `_issue`
#'   indicators, Monday to Sunday.
#' - `WEEK_ISSUES`: week-of-month indicators for the report date. Weeks 4 and
#'   later are the baseline category (see [add_weekofmonth()]).
#' - `TEST_LAG_GROUPS_DAILY`: default test lag groups for daily data. The
#'   groups cover lags 0 to 59, so they suit target lags of 60 days.
#' - `TEST_LAG_GROUPS_WEEKLY`: default test lag groups for weekly data,
#'   lags 0 to 70 in steps of 7 days.
#'
#' @name constants
NULL

#' @rdname constants
#' @export
TAUS <- c(0.01, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 0.99)

#' @rdname constants
#' @export
REF_LAG <- 60

#' @rdname constants
#' @export
LAG_PAD <- 1

#' @rdname constants
#' @export
TRAINING_DAYS <- 365

#' @rdname constants
#' @export
LAMBDA <- 0.1

#' @rdname constants
#' @export
GAMMA <- 0.1

#' @rdname constants
#' @export
LP_SOLVER <- "gurobi"

#' @rdname constants
#' @export
YITL <- "log_value_raw"

#' @rdname constants
#' @export
SLOPE <- "log_7dav_slope"

#' @rdname constants
#' @export
Y7DAV <- "log_value_7dav"

#' @rdname constants
#' @export
RESPONSE <- "log_value_target"

#' @rdname constants
#' @export
LOG_LAG <- "inv_log_lag"

#' @rdname constants
#' @export
WEEKDAYS_ABBR <- c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun")

#' @rdname constants
#' @export
WEEK_ISSUES <- c("W1_issue", "W2_issue", "W3_issue")

#' @rdname constants
#' @export
TEST_LAG_GROUPS_DAILY <- c(as.character(0:14), c("15-21", "22-35", "36-49", "50-59"))

#' @rdname constants
#' @export
TEST_LAG_GROUPS_WEEKLY <- c(as.character(seq(0, 70, 7)))
