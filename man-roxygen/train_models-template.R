#' @param train_models Logical; when `TRUE`, fit new models and overwrite
#'   the cache. When `FALSE`, load cached models; a missing model is fitted
#'   with a warning, and a cached model fitted under a different contract
#'   (response, predictors, `taus`, hyperparameters, training cutoff and
#'   window, target lag, or training data; see [get_model()]) is an error.
