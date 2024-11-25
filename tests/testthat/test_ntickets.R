test_that("ntickets is correct", {
  result = ntickets(200, 0.02, 0.95)
  expect_equal(result$nd, 205)
})

test_that("ntickets is correct", {
  result = ntickets(200, 0.02, 0.95)
  expect_equal(result$nc, 203.8)
})

test_that("ntickets is correct", {
  result = ntickets(200, 0.02, 0.95)
  expect_equal(result$N, 200)
})
test_that("ntickets is correct", {
  result = ntickets(200, 0.02, 0.95)
  expect_equal(result$p, 0.95)
})
test_that("ntickets is correct", {
  result = ntickets(200, 0.02, 0.95)
  expect_equal(result$gamma, 0.02)
})


