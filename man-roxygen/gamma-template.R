#' @param gamma Non-negative decay rate of the observation weights
#'   `exp(-gamma * d)`, where `d` is the product of the min-max normalized
#'   `value_7dav_diff` and `value_slope_diff` from [add_weights_related()]:
#'   each training row's level and trend distance from the forecast-origin
#'   anchor for its lag. `0` gives equal weights. [revision_forecast()] accepts a vector and fits one model per
#'   value; [delphirf_nowcast_preprocessed()] also accepts a named list with one value per test lag
#'   group and an `others` entry (see [handle_hyperparam()]).
