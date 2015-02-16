context("Test for bt.mean")

library("backtest")

test_that("Tests for function bt.mean", {

load("bt.mean.test.RData")

## save(x, truth, file = "bt.mean.test.RData", compress = TRUE)

expect_true(all.equal(backtest:::.bt.mean(x), truth))

})