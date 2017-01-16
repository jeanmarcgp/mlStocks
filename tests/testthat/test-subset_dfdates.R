#
# test-subset_dfdates.R
# ----------------------
#
# Test suite for testing function subset_dfdates
#
#
context("Testing function: subset_dfdates()")
library(testthat)

test_that("Testing basic functionality", {

  library(testthat)
  library(xtsanalytics)
  df        = Earnings[, c(1,2,13)]
  timeframe = "2011-01-03/2011-01-21"
  datecol   = 2
  keep_NAs  = FALSE


  #---------------------------------------------------
  # Test to include both start and end boundaries
  #---------------------------------------------------
  bounds_rm = c(FALSE, FALSE)
  dftest <- subset_dfdates(df, timeframe = timeframe, datecol = datecol, keep_NAs = keep_NAs, bounds_rm = bounds_rm)
  N      <- nrow(dftest)
  expect_equal(dftest[1, "dtBuy"], as.Date("2011-01-03"))   # include first date
  expect_equal(dftest[N, "dtBuy"], as.Date("2011-01-21"))   # include last date

  #---------------------------------------------------
  # Test to include start but not end boundary
  #---------------------------------------------------
  bounds_rm <- c(FALSE, TRUE)
  dftest <- subset_dfdates(df, timeframe = timeframe, datecol = datecol, keep_NAs = keep_NAs, bounds_rm = bounds_rm)
  N      <- nrow(dftest)
  expect_equal(dftest[1, "dtBuy"], as.Date("2011-01-03"))   # include first date
  expect_equal(dftest[N, "dtBuy"], as.Date("2011-01-20"))   # EXclude last date

  #---------------------------------------------------
  # Test to include start but exclude end boundary
  #---------------------------------------------------
  bounds_rm <- c(TRUE, FALSE)
  dftest <- subset_dfdates(df, timeframe = timeframe, datecol = datecol, keep_NAs = keep_NAs, bounds_rm = bounds_rm)
  N      <- nrow(dftest)
  expect_equal(dftest[1, "dtBuy"], as.Date("2011-01-04"))   # EXclude first date
  expect_equal(dftest[N, "dtBuy"], as.Date("2011-01-21"))   # include last date

  #---------------------------------------------------
  # Test to include start but exclude end boundary
  #---------------------------------------------------
  bounds_rm <- c(TRUE, TRUE)
  dftest <- subset_dfdates(df, timeframe = timeframe, datecol = datecol, keep_NAs = keep_NAs, bounds_rm = bounds_rm)
  N      <- nrow(dftest)
  expect_equal(dftest[1, "dtBuy"], as.Date("2011-01-04"))   # EXclude first date
  expect_equal(dftest[N, "dtBuy"], as.Date("2011-01-20"))   # EXclude last date



})

