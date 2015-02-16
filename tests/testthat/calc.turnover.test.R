context("Test for calc.turnover")

library("backtest")

test_that("Test-case for calc.turnover", {

load("calc.turnover.test.RData")

## save(x.id, x.bucket, x.date, x.truth, file = "calc.turnover.test.RData", compress = TRUE)

x.result <- backtest:::calc.turnover(x.id, x.bucket, x.date)

expect_true(all.equal(x.result, x.truth))
})