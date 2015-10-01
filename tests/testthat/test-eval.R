library(kgRainPredictR)
context("test evaluation metric and other test functions.")

test_that("mae() provides correct answer", {
  a = c(1:5)
  p = c(1,2,2,2,5)
  w = c(0.5, 1, 3, 0.5, 0.5)
  expect_equal(mae(a,p), mean(abs(a-p))) #0.6
  expect_equal(mae(a,p,weights=w), sum(w * abs(a - p))/sum(w)) #0.7272...
})
