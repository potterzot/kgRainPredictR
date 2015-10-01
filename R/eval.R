#' Calculate Mean Absolute Error (MAE).
#'
#' @export
#' @rdname model_error
#' @return numerical.
mae <- function(...) {
  list(
    metric = "mae",
    value = model_error(..., lambda = 1)
  )
}

#' Calculate Root Mean Squared Error (RMSE).
#'
#' @export
#' @rdname model_error
#' @return numerical.
rmse <- function(...) {
  model_error(..., lambda = 2)
}

#' Calculate generic model error.
#'
#' @param err vector of errors.
#' @param actual vector of test/actual data.
#' @param predicted vector of predicted data.
#' @param lambda the power factor
#' @param weights weights applied to error function.
#' @return a measure of model error.
model_error <- function(err, actual, predicted, lambda, weights, na.rm = F) {
  if (missing(err)) err = actual - predicted
  if (na.rm) err <- na.omit(err)
  if (missing(weights)) weights = rep(1, length(err))
  (sum((weights * abs(err))^lambda) / sum(weights))^(1/lambda)
}
