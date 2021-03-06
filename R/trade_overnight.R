
####################################################################################
# FILE trade_overnight.R
#
####################################################################################
#
#' Build an equity curve based on multiple successive overnight trades
#'
#' This function executes overnight trades based on a dataframe of returns
#' predictions and actual returns for a number of securities. Multiple
#' securities may be traded on the same date.  The function allows for
#' a maximum number of simultaneous positions, a maximum portfolio weight per
#' position and long and short thresholds tested against the predicted return
#' (yhat) to trade only those securities with the highest return potential.
#'
#'
#' A dataframe with at least three columns must be provided:
#' the actual overnight return, a return estimate (yhat) generated by some model, and
#' the date such trade would have been executed.  Other columns may be included
#' for trade  identification purposes, such as the ticker symbol.
#' Each row corresponds to a potential stock trade, so there may be many rows with
#' different stocks on the same date.
#'
#' The long threshold and short thresholds are used to test yhat and select only
#' those securities where yhat >= long_thresh or yhat <= short_thresh for long and
#' short trades respectively.
#'
#' When more trades are available than maxposn, the trades are sorted by the
#' absolute value of yhat in descending order, then the top maxposn trades are
#' selected.  This allows similar and fair treatment of long and short trades.
#' If a long only strategy is desired, simply set short_thresh to a large negative number.
#'
#' During the trade sort, identical values of yhat are not reordered, per the "shell"
#' method in function sort.  See ?sort.
#'
#'
#' @param predmat     A dataframe or xts matrix containing  at minumum a date column, a
#'                    prediction column (yhat) and an overnight returns colum.
#'                    The names of these columns should be specified with the
#'                    datecol, yhatcol and retcol arguments.  Other columns
#'                    are considered ID columns and are passed through to the
#'                    trade details xts that is returned. Such columns may contain,
#'                    for example, the ticker symbols.
#'
#' @param maxposn     The maximum number of equally weighted simultaneous positions
#'                    to take for an overnight trade.
#'
#' @param maxweight   The maximum weight given to any trade.  This is important
#'                    to limit how large a position will be when only one or very
#'                    stocks are traded during one day.
#'
#' @param longthresh  The threshold above which a long trade is taken.  This
#'                    threshold is compared with the yhatcol value for this decision.
#'
#' @param shortthresh The threshold below which a short trade is taken.  This
#'                    threshold is compared with the yhatcol value for this decision.
#'                    NOT YET IMPLEMENTED!
#'
#' @param dolvolname  The column name in predmat that corresponds to the dollar
#'                    volume traded for the stock. Typically, this will be a 3 month
#'                    rolling daily dollar volume average.  It is used to filter
#'                    out stocks that are thinly traded by eliminating trades when
#'                    the dollar volume is less than dolvolthresh.
#'
#' @param dolvolthresh The dollar volume threshold used to filter out stock trades
#'                     below this number.
#'
#' @param dateseries  An optional xts series (such as SPY or GSPC) that has all dates
#'                    the market is open.  This is used to fill in dates not present
#'                    in predmat to carry forward the equity curve.  This way, the
#'                    resulting equity curve can be plotted without any big
#'                    time frames missing. Default is NA (no dates provided)
#'
#' @param datacol     The column name in predmat containing the dates.  If
#'                    predmat is an xts matrix, this should be set to NA which
#'                    will result in taking the xts matrix dates instead.
#'
#' @param yhatcol     The column name in predmat with the predicted values (yhat).
#'
#' @param retcol      The column name in predmat with the overnight returns.  Note
#'                    that the assumed overnight return starts on the current date
#'                    (close of market) to the next day's sale.  The user must
#'                    ensure these returns are properly lagged to ensure there is
#'                    no lookahead bias.
#'
#' @return  Returns a list of three items:
#'
#' \itemize{
#'   \item
#'     \strong{$summary       } An xts matrix with the resulting equity curve. The xts matrix
#'                              index will either correspond to the predmat dates only
#'                              (dateseries = NA), or an outer merge of the predmat dates with
#'                              the dates in dateseries, properly subsetted at the start and
#'                              ending boundaries of predmat.
#'
#'                              In addition to the equity curve, the matrix includes a column
#'                              for the number of trades performed on each date (Ntrades), and
#'                              the total number of stocks available to trade on each date (allstocks).
#'   \item
#'     \strong{$returns       } A single column xts matrix of the equity curve returns.
#'   \item
#'     \strong{$trade_details } A dataframe containing the details of each trade executed.
#'
#' }
#'
#'
#'
#' @export
#----------------------------------------------------------------------------------
trade_overnight <- function(predmat, maxposn = 10, maxweight = 0.25,
                            longthresh = 0.01, shortthresh = -100, dolvolname = NA,
                            dolvolthresh = 1e6, dateseries = NA,
                            datecol = NA, yhatcol = "yhat", retcol = "rets") {

  # ###########  For code testing only ##########
  # library(xtsanalytics)
  # # Use embedded Earnings dataset to test...
  # dolvolname        = "DolVolDaily3m"
  # predmat           = Earnings[1:3000, c("Ret1", "dtBuy", "Ticker", dolvolname)]
  # colnames(predmat)[1:2] = c("rets", "date")
  # set.seed(123)     # Generate somewhat correlated yhat to rets
  # predmat$yhat      = predmat$rets + runif(n = nrow(predmat), min = -0.25, max = 0.25)
  # #plot(predmat$rets, predmat$yhat)
  # yhatcol           = "yhat"
  # retcol            = "rets"
  # datecol           = "date"
  # maxweight         = 0.2
  # maxposn           = 8
  # dolvolthresh      = 5  # in $M
  # spy               = xts_data["2011", ]
  # dateseries        = spy
  # longthresh        = 0.2
  # shortthresh       = -0.2
  #
  # ####################

  #------------------------------------------------------------------
  # Ensure alldates contain dates, irrespective of class or format
  #------------------------------------------------------------------
  if(class(predmat) == "xts") alldates <- index(predmat)
  if(is.na(datecol)) alldates <- row.names(predmat) else
    alldates <- predmat[, datecol]

  alldates   <- unique(alldates)
  Ndates     <- length(alldates)
  if(Ndates < 1) stop("predmat does not contain valid dates!")

  #------------------------------------------------------------------
  # Initialize containers to accumulate results
  #------------------------------------------------------------------
  results    <- emptyxts(cnames = c("rets", "allstocks", "Ntrades"),
                         order.by = as.Date(alldates))

  cnames     <- colnames(predmat)
  dftrades   <- data.frame(matrix(NA, nrow = 0, ncol = length(cnames),
                                  dimnames = list(NULL, cnames)))


  #-----------------------------------------------------------------
  # MAIN LOOP:  Trade each stock as specified
  #-----------------------------------------------------------------
  for(i in 1:Ndates) {
    #---------------------------------------------------------------------
    # Extract all rows on a given date, then find eligible long & short
    # trades by testing against thresholds. Combine to get eligible rows
    #---------------------------------------------------------------------
    df        <- predmat[predmat[, datecol] == alldates[i], ]
    #df        <- predmat[predmat[, datecol] == "1990-01-01", ]
    longrows     <- ifelse(as.numeric(df[, yhatcol]) >= longthresh, TRUE, FALSE)
    shortrows    <- ifelse(as.numeric(df[, yhatcol]) <= shortthresh, TRUE, FALSE)
    eligiblerows <- longrows | shortrows
    df           <- df[eligiblerows, ]

    # remove thinly traded stocks if dolvolname is provided
    if(!is.na(dolvolname))
      df           <- df[df[, dolvolname] >= dolvolthresh, ]


    allstocks    <- nrow(df)

    #----------------------------------------------------------------------
    # Sort & Select up to maxposn only more stocks than maxposn
    #----------------------------------------------------------------------
    if(allstocks <= maxposn) {
      dfsub <- df
    } else {
      #-------------------------------------------------------------------
      # Sort by absolute yhat to take the best long or short predictions
      # then select top trades, up to maxposn; exclude column abs_yhat
      # yhat ties are not reordered, per the sort "shell" method.
      #-------------------------------------------------------------------
      df2          <- df
      df2$abs_yhat <- abs(df2[, yhatcol])
      df2          <- df2[order(df2[, "abs_yhat"], decreasing = TRUE, method = "shell"), ]
      dfsub  <- df2[1:min(nrow(df2), maxposn), 1:(ncol(df2) - 1)]
    }

    #-------------------------------------------------------------
    # Trade the returns in dfsub, equal weighted or up to maxinv
    #-------------------------------------------------------------
    ntrades  <- nrow(dfsub)
    if(ntrades == 0) {
      retsum <- 0
    } else {
      wt     <- min(maxweight, 1 / ntrades)
      retsum <- wt * sum(dfsub[, retcol])
    }


    #----------------------------------------------------------
    # Build the results matrix and rbind dftrades
    #----------------------------------------------------------
    results[i, "rets"]      <- retsum
    results[i, "allstocks"] <- allstocks
    results[i, "Ntrades"]   <- nrow(dfsub)

    dftrades <- rbind(dftrades, dfsub)
  }

  #--------------------------------------------------
  # Calculate equity curve
  #--------------------------------------------------
  ec           <- cumprod_na(1 + results[, "rets"])
  colnames(ec) <- "Equity_Curve"
  ec           <- xtsbind(ec, results[, c("allstocks", "Ntrades")])

  #--------------------------------------------------
  # Inner pad with dateseries if available
  #--------------------------------------------------
  if(!is.na(dateseries)[[1]]) {

    tf      <- paste0(index(first(ec)), "/", index(last(ec)))
    tfdates <- index(dateseries[tf,])

    ec           <- xtsbind(ec, tfdates)
    ec$Ntrades   <- ifelse(is.na(ec$Ntrades), 0, ec$Ntrades)
    ec$allstocks <- ifelse(is.na(ec$allstocks), 0, ec$allstocks)
    ec           <- na.locf(ec, na.rm = TRUE)

  }

  #--------------------------------------------------
  # Build the return list
  #--------------------------------------------------
  rlist <- list(rets    = results[, "rets", drop = FALSE],
                summary = ec,
                details = dftrades ) # the trade details df

  return(rlist)

}  ##############  END Function trade_overnight  #################
