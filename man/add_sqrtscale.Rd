% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{add_sqrtscale}
\alias{add_sqrtscale}
\title{Add Square Root Scale Indicator Columns}
\usage{
add_sqrtscale(df, sqrt_max_raw, rare_thresh = 0.05)
}
\arguments{
\item{df}{Data frame containing the data.}

\item{sqrt_max_raw}{The maximum value in the dataset, used to determine bin thresholds.}
}
\value{
A data frame with additional binary indicator columns for square root scaling.
}
\description{
This function adds new columns to the dataset to indicate
the scale of values at the square root level. The function divides the range
of values into 4 square root-based bins and assigns binary indicators.
}
