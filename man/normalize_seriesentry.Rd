% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{normalize_seriesentry}
\alias{normalize_seriesentry}
\title{Normalize a single time series entry}
\usage{
normalize_seriesentry(seriesentry)
}
\arguments{
\item{seriesentry}{A list containing time series data (`x`, `xx`, `pt_ff`, `up_ff`, `low_ff`).}
}
\value{
A modified series entry with normalized values.
}
\description{
Applies min-max scaling to all relevant numeric elements of a series entry.
}
