test_that("myncurve mu works", {
  result <- myncurve(a=6, mu=10,sigma=5)
  expect_equal(result$mu, 10)
})

test_that("myncurve sigma works", {
  result <- myncurve(a=6, mu=10,sigma=5)
  expect_equal(result$sigma, 5)
})

test_that("myncurve probability works", {
  result <- myncurve(a=6, mu=10,sigma=5)
  expect_equal(result$Probability, 0.2119)
})
