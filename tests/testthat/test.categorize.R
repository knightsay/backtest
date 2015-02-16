context("Test for categorize")

library("backtest")

test_that("Test-case for categorize", {

load("categorize.test.RData")

## save(tmp.1, tmp.1.n, truth.1, tmp.2, truth.2, file = "categorize.test.RData", compress = TRUE)

result.1 <- backtest:::categorize(tmp.1, n = tmp.1.n)
result.2 <- backtest:::categorize(tmp.2)

expect_true(all.equal(result.1, truth.1))
expect_true(all.equal(result.2, truth.2))
})