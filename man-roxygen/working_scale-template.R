#' @param working_scale Required; there is no default. The scale the model
#'   is fitted on, which sets how predictions are returned to the reporting
#'   scale: `"log_value"` when preprocessing received one value column (a count
#'   or a directly supplied rate), modelled as `log(value + 1)`; `"log_ratio"`
#'   when it received numerator and denominator columns, modelled as
#'   `log(numerator + 1) - log(denominator + 1)`.
