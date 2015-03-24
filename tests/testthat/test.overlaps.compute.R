context("Test for overlaps.compute")

library("backtest")

test_that("Test-case for overlaps.compute", {

load("overlaps.compute.test.RData")

## save(over.data, weight.truth, file = "overlaps.compute.test.RData")

result <- backtest:::overlaps.compute(over.data, "in.factor", "date", "id", 2)
weight.result <- result$weight

expect_true(all.equal(weight.truth, weight.result))
})