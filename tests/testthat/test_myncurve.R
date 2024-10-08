test_that("mean value is correct", {
  result = myncurve(0, 1, 1)
  expect_equal(result$mu, 1)
})

test_that("standard deviation is correct", {
  result = myncurve(0, 1, 1)
  expect_equal(result$sigma, 1)
})

test_that("calculated area is correct", {
  result = myncurve(0, 1, 1)
  expect_equal(result$a, 0)
})
