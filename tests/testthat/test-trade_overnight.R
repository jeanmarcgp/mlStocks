#
# test-trade_overnight.R
# ----------------------
#
# Test suite for testing function trade_overnight
#
#
context("Testing function: trade_overnight()")
library(testthat)

test_that("Testing basic functionality", {

  library(mlStocks)
  library(testthat)
  dolvolname        = "DolVolDaily3m"
  predmat                = Earnings[1:3000, c("Ret1", "dtBuy", "Ticker", dolvolname)]
  colnames(predmat)[1:2] = c("rets", "date")
  set.seed(123)     # Generate somewhat correlated yhat to rets
  predmat$yhat      = predmat$rets + runif(n = nrow(predmat), min = -0.25, max = 0.25)
  yhatcol           = "yhat"
  retcol            = "rets"
  datecol           = "date"
  maxweight         = 0.2
  maxposn           = 10

  dateseries        = xts_data["2011", ]
  longthresh        =  0.25
  shortthresh       = -0.25

  ####################

  dolvolthresh      = 25  # in $M
  x <- trade_overnight(predmat, maxposn = maxposn, maxweight = maxweight, longthresh = longthresh,
                       shortthresh = shortthresh, dolvolname = dolvolname, dolvolthresh = dolvolthresh,
                       dateseries = dateseries, datecol = datecol,
                       yhatcol = yhatcol, retcol = retcol)

  cnames <- c("rets", "date", "Ticker", dolvolname, "yhat")
  expect_equal(colnames(x$details), cnames)

  sumtrades <- sum(x$summary[, "Ntrades"])
  sprint("sum of all trades: %s", sumtrades)
  expect_equal(sumtrades, 115)

  ############

  dolvolthresh = 1
  x <- trade_overnight(predmat, maxposn = maxposn, maxweight = maxweight, longthresh = longthresh,
                       shortthresh = shortthresh, dolvolname = dolvolname, dolvolthresh = dolvolthresh,
                       dateseries = dateseries, datecol = datecol,
                       yhatcol = yhatcol, retcol = retcol)

  sumtrades <- sum(x$summary[, "Ntrades"])
  sprint("sum of all trades: %s", sumtrades)
  expect_equal(sumtrades, 247)

})

