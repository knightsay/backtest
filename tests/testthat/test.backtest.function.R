context("Test backtest.function")

library("backtest")

test_that("test for backtest.function", {
  
  load("backtest.function.test.RData")
  
  ## What is comment for? Can we delete it?
  ## save(x, truth.1, truth.2, truth.3, truth.4.A, truth.4.B, file = "backtest.function.test.RData", compress = TRUE)
  
  bt.1 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE)
  bt.2 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.var = "country", 
                   by.period = FALSE)
  bt.3 <- backtest(x, id.var = "id", in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date", 
                   by.period = FALSE)
  bt.4 <- backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = "ret.var.1", by.var = "country", 
                   by.period = FALSE)
  bt.5 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", 
                   buckets = 2)
  bt.6 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.period = TRUE, date.var = "date", 
                   natural = TRUE, id = "id", buckets = 2)
  bt.7 <- backtest(x, id.var = "id", in.var = "in.var.1", ret.var = "ret.var.1", by.period = FALSE, 
                   date.var = "date", overlaps = 2)
  
  expect_true(all.equal(truth.1, as.numeric(bt.1@results[1,1 , , ,"means"])))
  expect_true(all(mapply(all.equal, truth.2, bt.2@results[1,1 , , ,"means"])))
  expect_true(all(mapply(all.equal, truth.3, bt.3@results[1,1 , , ,"means"])))
  expect_true(all(mapply(all.equal, truth.4.A, bt.4@results["ret.var.1","in.var.1", , ,"means"])))
  expect_true(all(mapply(all.equal, truth.4.B, bt.4@results["ret.var.1","in.var.2", , ,"means"])))
  expect_true(all(mapply(all.equal, truth.5, bt.5@results[1,1 , , ,"means"])))
  expect_true(all(mapply(all.equal, truth.6, bt.6@results[1,1 , , ,"means"])))
})  



