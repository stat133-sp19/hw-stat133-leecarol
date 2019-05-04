context("Checking summary measure functions")

test_that("aux_mean() works as expected", {
  expect_length(aux_mean(5, 0.5), 1)
  expect_type(aux_mean(5, 0.5), 'double')
  expect_equal(aux_mean(5, 0.5), 2.5)
  expect_equal(aux_mean(10, 0.2), 2)
})

test_that("aux_variance() works as expected", {
  expect_length(aux_variance(5, 0.5), 1)
  expect_type(aux_variance(5, 0.5), 'double')
  expect_equal(aux_variance(5, 0.5), 5 * 0.5 * (1 - 0.5))
  expect_equal(aux_variance(10, 0.2), 10 * 0.2* (1 - 0.2))
})

test_that("aux_mode() works as expected", {
  expect_length(aux_mode(5, 0.5), 1)
  expect_type(aux_mode(5, 0.5), 'double')
  expect_equal(aux_mode(5, 0.5), c(5 * 0.5 + 0.5 - 1, 5 * 0.5 + 0.5))
  expect_equal(aux_mode(10, 0.2), 2)
})

test_that("aux_skewness() works as expected", {
  expect_length(aux_skewness(5, 0.5), 1)
  expect_type(aux_skewness(5, 0.5), 'double')
  expect_equal(aux_skewness(5, 0.5), (1-2*0.5) / (sqrt(5*0.5*(1-0.5))))
  expect_equal(aux_skewness(10, 0.2), (1-2*0.5) / (sqrt(10*0.5*(1-0.5))))
})

test_that("aux_kurtosis() works as expected", {
  expect_length(aux_kurtosis(5, 0.5), 1)
  expect_type(aux_kurtosis(5, 0.5), 'double')
  expect_equal(aux_kurtosis(5, 0.5), (1-6*0.5*(1-0.5)) / (5*0.5*(1-0.5)))
  expect_equal(aux_kurtosis(10, 0.2), (1-6*0.5*(1-0.5)) / (10*0.5*(1-0.5)))
})
