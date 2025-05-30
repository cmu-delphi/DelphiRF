% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{create_params_list}
\alias{create_params_list}
\title{Create Parameters List}
\usage{
create_params_list(train_data, lagged_term_list, temporal_resol)
}
\arguments{
\item{train_data}{Data frame containing training data, including lag values.}

\item{lagged_term_list}{Numeric vector specifying the list of lags to be considered.}

\item{temporal_resol}{Character; either "daily" or "weekly" resolution.}
}
\description{
This function generates a list of parameter names based on the provided training data and lag list.
It dynamically constructs parameter names using predefined constants and incorporates log lag adjustments
when multiple lag values exist.
}
