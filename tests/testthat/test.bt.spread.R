context("Test for bt.spread")

library("backtest")

test_that("Test for bt.spread", {

load("bt.spread.test.RData")

## save(m, n, sd, truth, file = "bt.spread.test.RData", compress = TRUE)

expect_true(all(mapply(all.equal, backtest:::.bt.spread(m, n, sd), truth)))
  
})
