context("Test for bucketize")

library("backtest")

test_that("Test-case for bucketize", {

  load("bucketize.test.RData")
  load("bucketize.test2.RData")
  
  ## save(tmp.1, tmp.1.x, tmp.1.y, truth.1, file = "bucketize.test.RData", compress = TRUE)
  
  result.1 <- backtest:::bucketize(tmp.1, tmp.1.x, tmp.1.y, compute = length)## save(tmp.2, tmp.2.x, tmp.2.y, truth.2, file = "bucketize.test2.RData")
  result.2 <- backtest:::bucketize(tmp.2, tmp.2.x, tmp.2.y, compute = mean)
  
  expect_true(all.equal(result.1, truth.1))
  expect_true(all.equal(result.2, truth.2))

})