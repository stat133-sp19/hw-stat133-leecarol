context("Checking binomial functions")

test_that("bin_choose() works as expected", {
  expect_length(bin_choose(5, 2), 1)
  expect_type(bin_choose(5, 2), 'double')
  expect_equal(bin_choose(5, 2), 10)
})

test_that("bin_probability() works as expected", {
  expect_equal(bin_probability(2, 5, 0.5), 0.3125)
  expect_type(bin_probability(2, 5, 0.5), 'double')
  expect_length(bin_probability(2, 5, 0.5), 1)
})

test_that("bin_distribution() works as expected", {
  expect_equal(bin_distribution(5, 0.5)$probability, c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125))
  expect_equal(bin_distribution(5, 0.5)$success, c(0, 1, 2, 3, 4, 5))
  expect_s3_class(bin_distribution(5, 0.5), c("bindis", "data.frame"))
})

test_that("bin_cumulative() works as expected", {
  expect_equal(bin_cumulative(5, 0.5)$cumulative, c(0.03125, 0.18750, 0.50000, 0.81250, 0.96875, 1.00000))
  expect_s3_class(bin_cumulative(5, 0.5), c("bincum", "data.frame"))
  expect_length(bin_cumulative(5, 0.5), 3)
})

test_that("bin_variable() works as expected", {
  expect_equal(bin_variable(5, 0.5), list(trials = 5, prob = 0.5))
  expect_equal(bin_variable(10, 0.3), list(trials = 10, prob = 0.3))
  expect_length(bin_variable(5, 0.5), 2)
})
