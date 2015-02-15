context("Test for Summary Method")

library("backtest")

test_that("test for summary method and make sure summary doesn't crash", {
  load("backtest.summary.test.RData")
  
  ## save(x, file = "backtest.summary.test.RData", compress = TRUE)
  
  bt.1 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)
  bt.2 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.var = "country", 
                   by.period = FALSE)
  bt.3 <- backtest(x, id.var = "id", in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", 
                   by.period = FALSE)
  bt.4 <- backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = "ret.var.1", by.var = "country", 
                   by.period = FALSE)
  bt.5 <- backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = c("ret.var.1", "ret.var.2"), 
                   by.period = FALSE)
  bt.6 <- backtest(x, in.var = c("in.var.1"), ret.var = c("ret.var.1", "ret.var.2"), 
                   by.period = FALSE)
 
  expect_true({summary(bt.1); TRUE})
  expect_true({summary(bt.2); TRUE})
  expect_true({summary(bt.3); TRUE})
  expect_true({summary(bt.4); TRUE})
  expect_true({summary(bt.5); TRUE})
  expect_true({summary(bt.6); TRUE})
})

