#
# test-assess_features.R
# -----------------------
#
# Test suite for testing function assess_features
#
#
context("Testing function: assess_features()")
library(testthat)

test_that("Testing basic functionality", {

  library(mlStocks)
  library(testthat)
  set.seed(123)
  features          = c("Perc52WkHiRank", "PQMaxNDayRetRank")
  train_set         = Earnings[1:3000, c("Ret1", features)]
  val_set           = Earnings[3001:3500, c("Ret1", features)]
  Nrepeat     = 1
  mlalgo      = "xgboost"
  mlpar       = list(nrounds = 1000)
  mlpar       = pad_mlpar(mlalgo = mlalgo, mlpar = mlpar)
  meritFUN    = "trading_returns"

  meritFUNpar = list(long_thres = 0.005, short_thres = -75)

  #######

  value <- assess_features(features = features, train_set = train_set, val_set = val_set,
                           Nrepeat = Nrepeat, mlalgo = mlalgo, mlpar = mlpar,
                           meritFUN = meritFUN, meritFUNpar = meritFUNpar)

  expect_equal(value[["features"]], features)
  expect_equal(value[["summary"]][, "PctTraded"], 57.6)
  expect_equal(round(value[["values"]][1, "Trade_Rets"], 4), 0.0046)

  #--------------------------------------------------------
  # Now use RMSE merit function
  #--------------------------------------------------------
  meritFUN    = "RMSE"
  value <- assess_features(features = features, train_set = train_set, val_set = val_set,
                           Nrepeat = Nrepeat, mlalgo = mlalgo, mlpar = mlpar,
                           meritFUN = meritFUN, meritFUNpar = meritFUNpar)

  expect_equal(value[["features"]], features)
  expect_equal(round(value[["values"]][1, "RMSE"], 4), 0.0651)

})
