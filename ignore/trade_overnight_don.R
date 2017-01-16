
####################################################################################
# FILE trade_overnight DON.R
#
####################################################################################
#
#
#
# I found an error in my trade_overnight_don.R that I’ve been using.  I don’t know if you fixed it ,Jean-Marc.  The wt I was using was wrong when more than 20 stocks met the long_thres limit.  I attached what I’m using and how I fixed it.
#
# I added two columns to the return Nlongs and AvgMktDay. The AvgMktDay is the mean return of all the returns for the day which should be a more realistic average that 1000 random 20 stock return selections would give.  I think the market_returns Jean-Marc was using only averaged the returns of the stocks that passed the long_thres or short_thres filter – NOT all average of all possible trades for the day which random would select from.
#
# I’ve attached what I corrected it to.  So now there are 4 columns returned for each day – Equity_Curve, AllStocks, Nlongs, Ntrades, AvgMktDay
#
# This definitely affected the 4th run I made where I set the long_thres = 0 so nearly all were available to trade.  So I haven’t sent that run out and I’m now rerunning it.  This error MIGHT have affected the other runs I sent out, so I’ll have to rerun them as well.
#
# My current run is using 44 features and stops at a maximum of 4 features and takes the best up to that point but sets the long_thres=0 rather than the optimized value so more are available for trading.  I think because the wt was off, the results are not nearly as good as they should have been.  We’ll see when it finishes.  This run with the 4 feature limit takes 38 minutes or so vs. 2.5 hours.
#
# As Ren said, the hard part is testing and finding bugs.  This was a big one!
#



#'
#' @export
#----------------------------------------------------------------------------------
trade_overnight <- function(predmat, maxposn = 1, maxweight=0.20, longthresh = 0.01,
                            shortthresh = -100, dateseries = NA,
                            datecol = NA, yhatcol = "yhat", retcol = "rets") {

  # ###########  For code testing only ##########
  # library(xtsanalytics)
  # predmat <- data.frame(yhat  = c(0.02, 0.01,  0.05, -0.03, -0.04),
  #                       rets  = c(0.01, 0.04, -0.02, -0.01,  0.02),
  #                       date  = c("2012-01-01", "2012-01-05", "2012-01-05",
  #                                 "2012-01-05", "2012-01-10"))
  # yhatcol = "yhat"
  # retcol  = "rets"
  # datecol = "date"
  # maxposn = 5
  # spy     = xts_data["2012", ]
  # dateseries = spy
  # longthresh = 0.0
  # maxinv=0.20
  #
  # ####################


  if(class(predmat) == "xts") alldates <- index(predmat)
  if(is.na(datecol)) alldates <- row.names(predmat) else
    alldates <- predmat[, datecol]

  alldates   <- unique(alldates)
  Ndates     <- length(alldates)
  results    <- emptyxts(cnames = c("rets", "AllStocks","Nlongs","Ntrades", "AvgMktDay"),
                         order.by = as.Date(alldates))
  # totTrades=0
  # totAll=0
  # ec=1

  for(i in 1:Ndates) {
    df      <- predmat[predmat[, datecol] == alldates[i], ]

    avgmktday=mean(df[,1])
    # tf    <- paste0(alldates[i], "/", alldates[i])
    # df    <- subset_dfdates(predmat, timeframe = tf, datecol = datecol)
    nall <- nrow(df)
    # Filter those trades to ensure they exceed longthresh
    #rowsel <- ifelse(as.numeric(df[, yhatcol]) >= longthresh, TRUE, FALSE)
    #dfsub  <- df[df[,yhatcol]>= longthresh, ]
    # Filter those trades to ensure they exceed longthresh
    rowsel <- ifelse(as.numeric(df[, yhatcol]) >= longthresh, TRUE, FALSE)
    dfsub  <- df[rowsel, ]
    nlong <- nrow(dfsub)
    if (nlong==0){
      retsum <- 0
      ntrades <- 0
    } else
    {
      df2   <- dfsub[order(dfsub[, yhatcol], decreasing = TRUE), ]

      # Select the top trades, up to maxposn
      dfsub <- df2[1:min(nlong, maxposn), ]

      ntrades=nrow(dfsub)
      # trade the returns in dfsub, equal weighted
      # only longs implemented
      wt = min(maxweight,1/ntrades)
      retsum <- wt * sum(dfsub[, retcol])
    }

    results[i, "rets"]    <- retsum
    results[i, "AllStocks"] <- nall
    results[i, "Nlongs"]  <- nlong
    results[i, "Ntrades"]  <- ntrades
    results[i, "AvgMktDay"]  <- avgmktday

    # ec=ec*(1+retsum)
    # totTrades=totTrades+nlong
    # totAll=totAll+nall
    #
    # sprint("Trade returns %s, %s, %s, %s, %s, %s, %s", alldates[i], retsum, nlong, nall, ec, totTrades, totAll)
  }

  #--------------------------------------------------
  # Calculate equity curve
  #--------------------------------------------------
  ec         <- cumprod_na(1 + results[, "rets"])
  colnames(ec) <- "Equity_Curve"
  ec           <- xtsbind(ec, results[, c("AllStocks", "Ntrades", "Nlongs", "AvgMktDay")])
  # ec$Ntrades <- results[, "Ntrades"]
  # ec$AllStocks <- results[, "AllStocks"]

  #--------------------------------------------------
  # Outer pad with dateseries if available
  #--------------------------------------------------
  if(!is.na(dateseries)[[1]]) {

    tf      <- paste0(index(first(ec)), "/", index(last(ec)))
    tfdates <- index(dateseries[tf,])

    ec      <- xtsbind(ec, tfdates)
    ec$Nlongs <- ifelse(is.na(ec$Nlongs), 0, ec$Nlongs)
    ec$Ntrades <- ifelse(is.na(ec$Ntrades), 0, ec$Ntrades)
    ec$AllStocks <- ifelse(is.na(ec$AllStocks), 0, ec$AllStocks)
    ec$AvgMktDay <- ifelse(is.na(ec$AvgMktDay), 0, ec$AvgMktDay)
    ec     <- na.locf(ec, na.rm = TRUE)

  }

  return(ec)

}
