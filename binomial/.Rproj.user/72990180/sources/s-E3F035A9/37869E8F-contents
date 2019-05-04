context("Checkers")

test_that("check_prob() works as expected", {
  expect_length(check_prob(0.5), 1)
  expect_equal(check_prob(0.5), TRUE)
  expect_error(check_prob(-0.5))
})

test_that("check_trials() works as expected", {
  expect_length(check_trials(5), 1)
  expect_equal(check_trials(5), TRUE)
  expect_error(check_trials(-1))
})

test_that("check_success() works as expected", {
  expect_equal(check_success(5, 6), TRUE)
  expect_error(check_success(6, 5))
  expect_error(check_trials(-1, 2))
})
