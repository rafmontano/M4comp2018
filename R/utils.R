#' Min-Max Scaling Function 
#'
#' Scales a numeric vector or matrix between 0 and 1 using provided x_min and x_max values.
#'
#' @param x A numeric vector or matrix to be scaled.
#' @param x_min The minimum value to use for scaling.
#' @param x_max The maximum value to use for scaling.
#' @return A scaled numeric vector or matrix between 0 and 1.
#' @export
min_max_scaler <- function(x, x_min, x_max) {
  if (x_max - x_min == 0) {
    warning("Constant vector/matrix: returning zeros")
    return(matrix(0, nrow = nrow(x), ncol = ncol(x)))  # Handle matrices
  }
  
  (x - x_min) / (x_max - x_min)
}

#' Inverse Min-Max Scaling Function
#'
#' Converts a scaled vector (0 to 1) back to its original range using provided x_min and x_max values.
#'
#' @param x_scaled A numeric vector scaled between 0 and 1.
#' @param x_min The original minimum value before scaling.
#' @param x_max The original maximum value before scaling.
#' @return A numeric vector in its original range.
#' @export
inverse_min_max_scaler <- function(x_scaled, x_min, x_max) {
  x_scaled * (x_max - x_min) + x_min
}


#' Normalize a single time series entry
#'
#' Applies min-max scaling to all relevant numeric elements of a series entry.
#'
#' @param seriesentry A list containing time series data (`x`, `xx`, `pt_ff`, `up_ff`, `low_ff`).
#' @return A modified series entry with normalized values.
#' @export
normalize_seriesentry <- function(seriesentry) {
  # Get min and max values for the main series (x)
  x_min <- min(seriesentry$x, na.rm = TRUE)
  x_max <- max(seriesentry$x, na.rm = TRUE)
  
  # Apply min-max scaling to x and xx
  seriesentry$x <- min_max_scaler(seriesentry$x, x_min, x_max)
  if (!is.null(seriesentry$xx)) {
    seriesentry$xx <- min_max_scaler(seriesentry$xx, x_min, x_max)
  }
  
  # Handle matrices for pt_ff, up_ff, and low_ff
  if (!is.null(seriesentry$pt_ff) && is.matrix(seriesentry$pt_ff)) {
    seriesentry$pt_ff <- min_max_scaler(seriesentry$pt_ff, x_min, x_max)
  }
  if (!is.null(seriesentry$up_ff) && is.matrix(seriesentry$up_ff)) {
    seriesentry$up_ff <- min_max_scaler(seriesentry$up_ff, x_min, x_max)
  }
  if (!is.null(seriesentry$low_ff) && is.matrix(seriesentry$low_ff)) {
    seriesentry$low_ff <- min_max_scaler(seriesentry$low_ff, x_min, x_max)
  }
  
  # Store min and max values for inverse transformation
  seriesentry$x_min <- x_min
  seriesentry$x_max <- x_max
  
  return(seriesentry)
}

#' Normalize an entire dataset
#'
#' Applies `normalize_seriesentry()` to each series in a dataset.
#'
#' @param dataset A list of time series entries.
#' @return A dataset with all series entries normalized.
#' @export
normalize_database <- function(dataset) {
  lapply(dataset, normalize_seriesentry)
}
