
library(testthat)
library(PRNG)

# Uniformity Test
test_that("Uniformity Test", {
  random_numbers <- runf(n = 100, Time = FALSE)
  ks_result <- ks.test(random_numbers, "punif", 0, 1)
  expect_true(ks_result$p.value > 0.05, "Generated random numbers do not follow a uniform distribution")
})

# Independence Test
test_that("Independence Test", {
  random_numbers <- runf(N = 1000)
  autocorr_result <- acf(random_numbers, plot = FALSE)
  expect_true(all(abs(autocorr_result$acf[2:10]) < 0.1), "Consecutive random numbers are not independent.")
})

# Seed Sensitivity Test
test_that("Seed Sensitivity Test", {
  seed1 <- runf(n = 1000, x00 = 0.1, Time = FALSE)
  seed2 <- runf(n = 1000, x00 = 0.1001, Time = FALSE)
  expect_false(identical(seed1, seed2), "Small changes in seed do not result in different sequences.")
})

# Performance Test
test_that("Performance Test", {
  start_time <- Sys.time()
  runf(n = 100000, Time = FALSE)
  end_time <- Sys.time()
  expect_true(difftime(end_time, start_time, units = "secs") < 1, "Random number generation is too slow.")
})

test_that("Edge Cases Test", {
  expect_error(runf(N = -10), "N must be a positive integer")
  expect_error(runf(N = 0), "N must be a positive integer")
  expect_error(runf(x00 = -0.1), "x00 must be in the range \\[0, 1\\]")
  expect_error(runf(x00 = 1.1), "x00 must be in the range \\[0, 1\\]")
  expect_error(runf(x01 = -0.1), "x01 must be in the range \\[0, 1\\]")
  expect_error(runf(x01 = 1.1), "x01 must be in the range \\[0, 1\\]")
  expect_error(runf(x02 = -0.1), "x02 must be in the range \\[0, 1\\]")
  expect_error(runf(x02 = 1.1), "x02 must be in the range \\[0, 1\\]")
  expect_error(runf(a1 = 3.4), "a1 must be in the range \\[3.5, 4\\]")
  expect_error(runf(a1 = 4.1), "a1 must be in the range \\[3.5, 4\\]")
  expect_error(runf(a2 = 0.4), "a2 must be >= 0.5")
})

test_that("Valid Inputs Test", {
  expect_silent(runf(N = 10, x00 = 0.5, x01 = 0.5, x02 = 0.5, a1 = 3.8, a2 = 0.7))
})

# Distribution Tests (Normal)
test_that("Distribution Test - Normal", {
  normal_numbers <- rnorm(n = 1000)
  shapiro_test <- shapiro.test(normal_numbers)
  expect_true(shapiro_test$p.value > 0.05, "Generated numbers do not follow a normal distribution")
})
