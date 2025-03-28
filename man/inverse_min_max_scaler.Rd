% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{inverse_min_max_scaler}
\alias{inverse_min_max_scaler}
\title{Inverse Min-Max Scaling Function}
\usage{
inverse_min_max_scaler(x_scaled, x_min, x_max)
}
\arguments{
\item{x_scaled}{A numeric vector scaled between 0 and 1.}

\item{x_min}{The original minimum value before scaling.}

\item{x_max}{The original maximum value before scaling.}
}
\value{
A numeric vector in its original range.
}
\description{
Converts a scaled vector (0 to 1) back to its original range using provided x_min and x_max values.
}
