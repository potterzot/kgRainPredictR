library(rain.prediction)
context("Data input and output")

file_dir = system.file("tests/testthat/", package = "rain.prediction")

test_that("createSubmission() creates a submission file in the right format", {
  file_name = paste0(file_dir, "/test-io.csv")
  d <- data.frame("Id" = c(1,2,3),
                 "Expected" = c(73, 23, 49))
  createSubmission(d[,c("Id", "Expected")],
                   file_name,
                   zip = FALSE)
  d2 <- read.csv(file_name)
  expect_equal(d, d2)
})

test_that("createSubmission() errors on invalid path, or zip option.", {
  file_name = paste0(file_dir, "/nonexistentdir/test-io.csv")
  d <- data.frame("Id" = c(1,2,3),
                  "Expected" = c(73, 23, 49))
  expect_error(createSubmission(d[,c("Id", "Expected")],
                   file_name), paste0(file_name, " location does not exist."))
  expect_error(createSubmission(d[,c("Id", "Expected")],
                   file_name,
                   zip = "hi"), "zip must be TRUE or FALSE.")

})
