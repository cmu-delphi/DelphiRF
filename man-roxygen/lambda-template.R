#' @param lambda Non-negative lasso penalty on the predictor coefficients;
#'   the intercept is not penalized. [revision_forecast()] accepts a vector and
#'   fits one model per value; [delphirf_nowcast_preprocessed()] also accepts a named list with one
#'   value per test lag group and an `others` entry (see [handle_hyperparam()]).
