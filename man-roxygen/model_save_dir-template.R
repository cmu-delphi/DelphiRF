#' @param model_save_dir Directory for model cache files. On R 4.0 and later,
#'   models are stored by default in DelphiRF's platform-specific user cache
#'   directory, as returned by `tools::R_user_dir("DelphiRF", "cache")`, so they
#'   can be reused across R sessions. Older R versions use a session temporary
#'   directory. Supply an explicit path to use a different location.
