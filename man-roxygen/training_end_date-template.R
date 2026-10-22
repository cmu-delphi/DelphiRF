#' @param training_end_date A single valid date used as the training cutoff.
#'   When empty, `DelphiRF()` uses `testing_start_date`, while
#'   [revision_forecast()] and [cv_revision_forecast()] use the latest input
#'   report date. `DelphiRF()` requires the cutoff not to be later than its
#'   forecast origin. Lower-level functions have no forecast-origin argument
#'   and therefore validate the date but cannot apply that ordering check.
