#' Reflectivity interpolated
#'
#' @export
#' @param r a vector of reflectivity values
#' @param ... additional arguments passed to \code{na.fill}
ref_interp <- function(r, ...) {
  interpolate(r, ...)
}




#' Calculate summary over time of a variable.
#'
#' @export
#' @param times vector of time measurements.
#' @param values values to summarize.
#' @param FUN function to use to summarize.
summarize_over_time <- function(times, values) {

}
