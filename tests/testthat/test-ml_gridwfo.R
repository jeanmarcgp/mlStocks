#
# test-grid_wfo.R
# ---------------
#
# Test suite for testing function ml_grid_wfo
#
#
context("Testing function: ml_gridwfo()")
library(testthat)

test_that("Testing basic functionality", {

  library(mlStocks)
  library(testthat)
  set.seed(123)
  featurelist  = list("Perc52WkHiRank", "PQMaxNDayRetRank", "PQMinNDayRetRank")
  temp         = Earnings[, c(1, 2, 13, 59, which(colnames(Earnings) %in% featurelist))]
  ycol         = 1
  datecol      = 2
  df           = subset_dfdates(temp, timeframe = "2011-01-01/2012-02-15", datecol = datecol)

  IDcol        = c("Ticker")
  searchmethod = "list"
  wfodates     = "months"
  wfo_offset   = 0
  mktseries    = xts_gspc
  trainwin     = 12
  meritFUN     = "RMSE"
  tradePAR     = NA
  mlalgo       = "xgboost"
  mlpar        = NULL

  ##############################################


  x <- ml_gridwfo(df, ycol = ycol, featurelist = featurelist, datecol = datecol, IDcol = IDcol,
                  searchmethod = searchmethod, wfodates = wfodates, wfo_offset = wfo_offset,
                  mktseries = mktseries, trainwin = trainwin, meritFUN = meritFUN,
                  tradePAR = tradePAR, mlalgo = mlalgo, mlpar = mlpar)

  #expect_equal(round(as.numeric(last(x$summary))[1], 4), 1.0089)

  xtsplot(x$summary[, "Equity_Curve"])

  #################################

  meritFUN     = "overnight_returns"
  meritFUNpar  = list(maxposn = 10, maxweight = 0.25, longthresh = 0.0, shortthresh = -0.05,
                      dolvolname = "DolVolDaily3m", dolvolthresh = 1)
  IDcol        = c("Ticker", "DolVolDaily3m")

  x <- ml_gridwfo(df, ycol = ycol, featurelist = featurelist, datecol = datecol, IDcol = IDcol,
                  searchmethod = searchmethod, wfodates = wfodates, wfo_offset = wfo_offset,
                  mktseries = mktseries, trainwin = trainwin, meritFUN = meritFUN,
                  meritFUNpar = meritFUNpar, tradePAR = tradePAR, mlalgo = mlalgo, mlpar = mlpar)

  xtsplot(x$summary[, "Equity_Curve"], main = paste("dolvolthresh", meritFUNpar$dolvolthresh))

  ##################

  meritFUN     = "overnight_returns"
  meritFUNpar  = list(maxposn = 10, maxweight = 0.25, longthresh = 0.0, shortthresh = -0.05,
                      dolvolname = "DolVolDaily3m", dolvolthresh = 25)


  x <- ml_gridwfo(df, ycol = ycol, featurelist = featurelist, datecol = datecol, IDcol = IDcol,
                  searchmethod = searchmethod, wfodates = wfodates, wfo_offset = wfo_offset,
                  mktseries = mktseries, trainwin = trainwin, meritFUN = meritFUN,
                  meritFUNpar = meritFUNpar, tradePAR = tradePAR, mlalgo = mlalgo, mlpar = mlpar)

  xtsplot(x$summary[, "Equity_Curve"], main = paste("dolvolthresh", meritFUNpar$dolvolthresh))


})

