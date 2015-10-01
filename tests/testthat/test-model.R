library(kgRainPredictR)
context("model.R")

test_that("marshall_palmer() produces correct result.", {
  minutes_past <- c(3, 16, 25, 35, 45, 55)
  ref <- c(1, 2, NA, 4, 5, 6)
  expect_that(marshall_palmer(ref, min), 0.05754446)
})
