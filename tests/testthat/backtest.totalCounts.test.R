context("Test for Total Counts")

library("backtest")

test_that("Test-case for the totalCounts method", {

  # save(x, true.tc, file = "backtest.totalCounts.test.RData")
  load("backtest.totalCounts.test.RData")
  
  bt <- backtest(x, in.var = c("ret_12_0_m", "vim"), ret.var = "ret_0_1_y",
                 by.period = FALSE, date.var = "date")
  
  expect_true(all.equal(totalCounts(bt), true.tc))
})