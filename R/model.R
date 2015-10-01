#' Estimate Marshal Palmer Rain Expectations
#'
#' @importFrom zoo na.fill
#' @export
#'
#' @param ref radar reflectivity.
#' @param minutes_past minutes past the hour.
#' @return vector of expected rain measurements
model_marshal_palmer <- function(ref, minutes_past) {
  t <- time_difference(minutes_past)
  mm <- marshal_palmer(interpolate(ref))
  sum(mm * t)
}
