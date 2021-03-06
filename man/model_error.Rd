% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/eval.R
\name{mae}
\alias{mae}
\alias{model_error}
\alias{rmse}
\title{Calculate Mean Absolute Error (MAE).}
\usage{
mae(...)

rmse(...)

model_error(err, actual, predicted, lambda, weights, na.rm = F)
}
\arguments{
\item{err}{vector of errors.}

\item{actual}{vector of test/actual data.}

\item{predicted}{vector of predicted data.}

\item{lambda}{the power factor}

\item{weights}{weights applied to error function.}
}
\value{
numerical.

numerical.

a measure of model error.
}
\description{
Calculate Mean Absolute Error (MAE).

Calculate Root Mean Squared Error (RMSE).

Calculate generic model error.
}

