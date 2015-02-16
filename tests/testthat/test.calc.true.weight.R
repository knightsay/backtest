context("Test for calc.true.weight")

library("backtest")

test_that("Test-case for calc.true.weight", {
  
load("calc.true.weight.test.RData")

## save(calc.data, calc.truth, file = "calc.true.weight.RData")

result <- backtest:::calc.true.weight(calc.data, "date", "id", 2)
true.weight.result <- result$weight

expect_true(all.equal(true.weight.result, calc.truth))
})