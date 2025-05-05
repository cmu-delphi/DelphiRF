#' Constants
#'
#' These constants are used for forecast revision.
#' @title Constants
#' @description A set of constants used in the model.

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
LP_SOLVER <- "gurobi" # LP solver to use in quantile_lasso(); "gurobi" or "glpk"

# Response Variables
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

# Dates
#' @rdname constants
#' @export
WEEKDAYS_ABBR <- c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun") # wd

#' @rdname constants
#' @export
WEEK_ISSUES <- c("W1_issue", "W2_issue", "W3_issue") # wm

#' @rdname constants
#' @export
SQRT_SCALES <- c("sqrty0", "sqrty1", "sqrty2")

#' @rdname constants
#' @export
TODAY <- Sys.Date()

#' @rdname constants
#' @export
TEST_LAG_GROUPS_DAILY <- c(as.character(0:14), c("15-21", "22-35", "36-49", "50-59"))
TEST_LAG_GROUPS_DAILY <- c(as.character(seq(0, 70, 7)))


# For Delphi Signals
#' @importFrom tibble tribble
#' @rdname constants
#' @export
INDICATORS_AND_SIGNALS <- tibble::tribble(
  ~indicator, ~signal, ~name_suffix, ~sub_dir,
  "changehc", "covid", "", "chng",
  "changehc", "flu", "", "chng",
  "claims_hosp", "", "", "claims_hosp",
  # "dv",,,
  "quidel", "covidtest", c("total", "age_0_4", "age_5_17", "age_18_49", "age_50_64", "age_65plus", "age_0_17"), "quidel_covidtest"
)
