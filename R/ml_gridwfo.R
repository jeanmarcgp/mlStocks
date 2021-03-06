
####################################################################################
# FILE ml_gridwfo.R
#
####################################################################################
#
#' Walk-Forward Optimization using ML grid search at each WFO date
#'
#' Performs a grid search of machine learning models at each
#' walk-forward optimization date.
#'
#' This approach differs from normal WFO as follows.  At each WFO date,
#' a number of ML models are trained, each using a different set of features.
#' A subset of the training window, called pretrain, is used to build each
#' model.  This pretrain set starts at the beginning of the training window
#' but ends one WFO period early.  The last WFO period in the training window
#' is used for model validation (the validation period).  In other words, the
#' validation period consists of the period between the last two consecutive
#' WFO periods in the training window.
#'
#' Each of these models are then validated at each thresholds specified by
#' the tradePAR dataframe.  A performance score is given to each model based on
#' the chosen scoring function, meritFUN. The model with the highest
#' score wins, and its feature set is then used to build the final model.  The final
#' model uses the entire training window and is used to predict the next WFO period,
#' with threshold and maximum positions as specified by the tradePAR row performing
#' best.
#'
#' @param df            The dataframe containing the target
#'                      variable (y) and the features.
#'
#' @param ycol          The name of the column in dfcontaining the target variable.
#'
#' @param featurelist   A list containing vectors of feature names.  Each vector
#'                      may be a single feature name or a set of feature names.
#'                      The grid search is performed using the method specified by
#'                      parameter searchMethod (see below).
#'
#' @param datecol       The name of the column in df containing the date at which
#'                      a prediction is made.
#'
#' @param IDcol         A vector of column names that identify a given trade.  At a
#'                      minimum, it should contain the ticker symbol column name in df to
#'                      identify which stock is being traded.
#'
#' @param searchmethod  The method used to search through the featurelist.  Two methods
#'                      are supported:  "list" and "forwardsearch".  The list method simply
#'                      iterates through the list one set at a time and picks the best
#'                      performing set as evaluated by meritFUN.  The forwardsearch method
#'                      on the other hand performs a forward search by combining feature sets
#'                      one at a time using the forward search algorithm.  This is slower
#'                      but can find combinations of features that may be more predictive.
#'
#' @param wfodates      This is either a vector of database release dates, or it
#'                      is calculated at the periodic endpoints of the market open
#'                      dates.  To calculate at periodic endpoints, wfodates must
#'                      contain one of "weeks", months", "quarters" or "years", which specify
#'                      the period.  The wfodates are then determined as the inner
#'                      merge between the datecol in df, and the periodic index endpoints
#'                      extracted from mktseries.  Function endpoints is used to
#'                      calculate such periodic index endpoints.
#'
#' @param wfo_offset    The offset in market days at which the training period ends
#'                      compared to the wfodate.  For example, if wfo_offset = 5, then
#'                      the training period ends 5 market days before wfodates.
#'
#' @param mktseries     An xts times series of an index or security that includes
#'                      all dates in the dataframe df.  It is used to calculate
#'                      the WFO dates using the endpoints function.
#'
#' @param trainwin      The size of the training period (a positive integer) used for
#'                      training the final model.  This number corresponds to the
#'                      number of WFO periods used for training.  For example,
#'                      if wfodates = "months", and trainwin = 8, then the training
#'                      period is 8 months ending at wfodate - wfo_offset.
#'
#' @param meritFUN      The name of the scoring function used to evaluate the
#'                      performance of each grid ML model.  This is performed
#'                      on the validation set, which consists of the period
#'                      between the last two WFO dates in the training window.
#'                      Supported scoring functions include the following:
#'  \itemize{
#'     \item
#'     \strong{overnight_returns:} This function uses the trade_overnight function
#'                      to build an equity curve based on trading parameters
#'                      passed to trade_overnight via meritFUNpar.  The
#'                      raw return is used as the score, so the model with
#'                      the highest return is selected.
#'     \item
#'     \strong{overnight_MDD:}  This function compute at the worst drawdown
#'                      during the validation period, from an equity curve
#'                      generated by trade_overnight.  The lowest MDD model is
#'                      selected.
#'     \item
#'     \strong{RMSE:}  This function computes the root mean square error
#'                      between y and yhat over the validation period.  The
#'                      model with the lowest RMSE score is selected.
#'     \item
#'     \strong{overnight_MAR:} This function uses the trade_overnight function
#'                      to build an equity curve, then the MDD and total return
#'                      over the validation period is calculated in order to obtain
#'                      the MAR ratio.  The model with the highest MAR is selected.
#'     \item
#'     \strong{overnight_F1score:} not yet implemented
#'
#'     \strong{overnight_Sharpe:} not yet implemented
#'
#'  }
#'
#'
#' @param meritFUNpar   A list of parameters to be passed on to meritFUN.  If
#'                      meritFUN has not parameters, then this can be set to NULL.
#'
#' @param tradePAR      A dataframe of parameters used to convert yhat into long
#'                      trades and short trades. The dataframe must have FOUR columns:
#'                      "long_thresh", "short_thresh", "max_posn" and "max_weight".
#'                      Each row corresponds to one evaluation of yhat tested against the long
#'                      and the short thresholds.  Long trades result when yhat > long_thresh
#'                      and short trades result when yhat < short_thresh.  Max_posn
#'                      is the maximum number of simultaneous positions held, and max_weight is
#'                      the maximum weight any position may have in the portfolio. On days when
#'                      more trades are available than max_posn, then only the best trades are
#'                      executed.  In this context, best trades means those with the highest
#'                      yhat (long) or lowest yhat (for short positions), as measured by the
#'                      absolute value of yhat i.e. take the highest absolute yhats up to max_posn,
#'                      then apply the correct sign to go long or short. Default NA means
#'                      long_thresh = 0, short_thresh = -1000, max_posn = 10, max_weight = 0.25.
#'
#' @param mlalgo        The name of the machine learning algorithm used.  Currently, "xgboost"
#'                      and "h2o.rf" are supported.
#'
#' @param mlpar         A list containing the machine learning algorithm parameters.  If
#'                      empty or incomplete, then it is padded using function pad_mlpar().#'
#'
#'
#' @return  Returns a list of two elements.  The first element is an xts matrix
#'          with indices made from the inner merge of mktseries merged with the
#'          datecol dates in df.  The details for this matrix are below.  The
#'          second element is a dataframe of all trades details executed during
#'          the WFO run.
#'
#'          The xts matrix includes the following 3 columns:
#'
#' \itemize{
#'   \item
#'     \strong{Equity_Curve } The Equity Curve resulting from the grid wfo
#'                            search and optimization.
#'
#'     \strong{allstocks    } The number of assets available to trade on the current
#'                            date, that is, the index of the returned xts matrix.
#'
#'     \strong{Ntrades      } The number of trades performed on the current date.
#'
#' }
#'
#' @export
#-------------------------------------------------------------------------------------
ml_gridwfo <- function(df, ycol = 1, featurelist, datecol = NA, IDcol = NULL,
                       searchmethod = "list", wfodates = "months", wfo_offset = 0,
                       mktseries = xts_gspc, trainwin = 8, meritFUN = "RMSE",
                       meritFUNpar = NULL,
                       tradePAR = NA, mlalgo = "xgboost", mlpar = NA ) {


  # ######### Code for testing ONLY   #########
  # library(mlStocks)
  # featurelist  = list("Perc52WkHiRank", "PQMaxNDayRetRank", "PQMinNDayRetRank")
  # temp         = Earnings[, c(1, 2, 13, 59, which(colnames(Earnings) %in% featurelist))]
  # ycol         = 1
  # datecol      = 2
  # df           = subset_dfdates(temp, timeframe = "2011-01-01/2012-04-10", datecol = datecol)
  #
  # IDcol        = c("Ticker", "DolVolDaily3m")
  #
  # searchmethod = "list"
  # wfodates     = "months"
  # wfo_offset   = 0
  # mktseries    = xts_gspc
  # trainwin     = 12
  # meritFUN     = "RMSE"
  # meritFUNpar  = NULL
  #
  # meritFUN     = "overnight_returns"
  # meritFUNpar  = list(maxposn = 10, maxweight = 0.25, longthresh = 0.0, shortthresh = -0.01,
  #                     dolvolname = "DolVolDaily3m", dolvolthresh = 25)
  #
  # tradePAR     = NA
  # mlalgo       = "xgboost"
  # mlpar        = NULL
  #
  # ##############################################


  #------------------------------------------------------
  # Test for dataframe class, and extract WFO dates
  #------------------------------------------------------
  if(is.null(IDcol))
    stop("IDcol must include at least one column name to identify the trades.")

  if(class(df) == "data.frame") dfclass <- TRUE else
    dfclass <- FALSE

  if(dfclass) {
    stopifnot(!is.null(mktseries))   # mktseries cannot be NULL

    firstdate <- as.Date(df[1, datecol])
    lastdate  <- as.Date(df[nrow(df), datecol])
    datadates <- unique(as.Date(df[, datecol]))  # get all unique dates
    dateindex <- index(mktseries[paste0(firstdate, "/", lastdate), ])

    #----------------------------------------------------------------
    # Test that dateindex contains all datadates (plus inner dates)
    #----------------------------------------------------------------
    if(!all(datadates %in% dateindex)) {
      # Some dates in datadates are not in dateindex, so stop!
      bad_dates <- datadates[which(!(datadates %in% dateindex))]
      sprint("The following dates in df don't exist in mktseries:")
      print(bad_dates)
      stop("Stopping execution.  Check the dates!")
    }

    #---------------------------------------------------------------------
    # Extract WFO dates if wfodates in weeks, months, quarters or years
    #---------------------------------------------------------------------
    if(wfodates[[1]] %in% c("weeks", "months", "quarters", "years")) {
      wfoendpts  <- endpoints(mktseries[dateindex, ], on = wfodates)[-1]

    } else {
      # Test that all dates are valid if provided as function argument
      sprint("wfodates as DataDate")
      wfodatesTF <- (wfodates == as.Date(wfodates))
      if(sum(wfodatesTF) != length(wfodates)) {
        sprint("The following dates are not valid dates:")
        print(wfodates[!wfodatesTF])
        stop("Stopping execution.  Fix the wfodates!")
      }

      temp      <- mktseries[dateindex, ]
      wfoendpts <- temp[as.Date(wfodates), , which.i = TRUE]

    } #####  ENDIF wfodates  #####

    #------------------------------------------------------------------
    # Offset the WFO dates as needed
    #------------------------------------------------------------------
    wfodates_i <- wfoendpts - wfo_offset
    wfodates_i <- wfodates_i[wfodates_i > 0]
    wfodates   <- dateindex[wfodates_i]
    sprint("wfodates are:")
    print(wfodates)

    ###############################################
    #  NOTE offset doesn't work.  Must be able to
    # predict after the database release date!
    # So i need the end of train date and first predict
    # date, separated by the offset.
    ###############################################
  } else {
    #---------------------------------------------------
    # Not dfclass, so stop (implement xts later)
    #---------------------------------------------------
    stop("class(df) != 'data.frame' so stopping execution.")

  }

  #------------------------------------------------------
  # Extract all key columns as names, not numbers
  #------------------------------------------------------
  ycolname    <- colnames(df[, ycol, drop = FALSE])
  IDcolname   <- colnames(df[, IDcol, drop = FALSE])
  datecolname <- colnames(df[, datecol, drop = FALSE])

  #--------------------------------------------------------------
  # Check boundaries and test for valid searchmethod
  #--------------------------------------------------------------
  Ndates <- length(wfodates) - 1   # Last date is for final predictions
  if(Ndates < trainwin)
    stop("Not enough wfodates to have a complete training set")
  if(trainwin < 2)
    stop("trainwin must be >= 2 to allow room for validation during grid search.")
  if(!searchmethod %in% c("list", "forwardsearch"))
    stop("searchmethod is not valid.  Must be list or forwardsearch.")

  firstwfo <- wfodates[trainwin]

  #--------------------------------------------------------------
  # Ensure tradePAR and meritFUNpar have proper default values
  #--------------------------------------------------------------
  if(is.na(tradePAR[[1]]))
    tradePAR <- data.frame(long_thresh = 0, short_thresh = -1000,
                           max_posn = 10, max_weight = 0.25)

  if(is.null(meritFUNpar) && meritFUN != "RMSE")
    stop("meritFUNpar cannot be null when meritFUN != RMSE.")

  #--------------------------------------------------------------
  # Create containers to accumulate results
  #--------------------------------------------------------------
  rets     <- emptyxts(cnames = "rets", order.by = index(mktseries))
  tsumm    <- emptyxts(cnames = c("Equity_Curve", "allstocks", "Ntrades"),
                       order.by = index(mktseries))
  trade_details <- NULL

  #------------------------------------------------------
  #  Main Loop:  Iterate over WFO dates
  #------------------------------------------------------
  for(idate in trainwin:Ndates) {
    #idate = trainwin  ###########################################################
    #-----------------------------------------------------------------------
    # Subset pretrainset, validset, trainset and predset from df
    # Careful to ensure no date overlap using bounds_rm.
    #-----------------------------------------------------------------------
    train_tf  <- paste0(wfodates[idate - trainwin + 1], "/", wfodates[idate])
    trainset  <- subset_dfdates(df, timeframe = train_tf, datecol = datecol, bounds_rm = c(FALSE, FALSE))

    pretrain_tf <- paste0(wfodates[idate - trainwin + 1], "/", wfodates[idate - 1])
    pretrainset <- subset_dfdates(df, timeframe = pretrain_tf, datecol = datecol, bounds_rm = c(FALSE, FALSE))

    valid_tf    <- paste0(wfodates[idate - 1], "/", wfodates[idate])
    validset    <- subset_dfdates(df, timeframe = valid_tf, datecol = datecol, bounds_rm = c(TRUE, FALSE))

    pred_tf     <- paste0(wfodates[idate], "/", wfodates[idate + 1])
    predset     <- subset_dfdates(df, timeframe = pred_tf, datecol = datecol, bounds_rm = c(TRUE, FALSE))

    #-----------------------------------------------------------------------
    # If more than one feature/threshold combination, then do grid search
    #-----------------------------------------------------------------------

    best_tradePAR     <- 1
    best_featureset   <- featurelist[[1]]
    if(length(featurelist) > 1 || nrow(tradePAR) > 1) {
      #-------------------------------------------------------------------
      # Grid search to find the best_tradePAR and best_featureset
      #-------------------------------------------------------------------
      sprint("Grid search CODE goes here - NOT IMPLEMENTED ")

      switch(searchmethod,
             list          = {
               sprint("searchmethod is list")

               #--------------------------------------------------------------
               # Set initial values for mscore and best model
               #--------------------------------------------------------------
               if(meritFUN %in% c("RMSE")) mscore <- 1e6 else
                 mscore <- -1e6
               bestmodel <- 1

               for(i in 1:length(featurelist)) {
                 subcols <- c(ycolname, featurelist[[i]])

                 if(meritFUN == "RMSE") {
                   #----------------------------------------------------------
                   # RMSE scoring method - minimize mscore
                   #----------------------------------------------------------
                   mraw   <- assess_features(features    = featurelist[[i]],
                                             train_set   = pretrainset[, subcols],
                                             val_set     = validset[, subcols],
                                             Nrepeat     = 1,
                                             mlalgo      = mlalgo,
                                             mlpar       = mlpar,
                                             meritFUN    = meritFUN,
                                             meritFUNpar = NULL    )

                   # Update mscore and bestmodel if current model performs better
                   if(mscore > mraw$values) {
                     mscore    <- mraw$values
                     bestmodel <- i
                   }

                 } else {
                   if(meritFUN %in% c("overnight_returns", "overnight_MDD",
                                      "overnight_MAR", "overnight_Sharpe")) {
                     yhat  <- ml_trainpredict(train_set   = pretrainset[, subcols],
                                              valid_set   = validset[, subcols],
                                              ycol        = ycol,
                                              mlalgo      = mlalgo,
                                              mlpar       = mlpar  )
                     predmat <- cbind(validset[, c(datecolname, IDcolname)], yhat)

                     #--------------------------------------------------------------
                     # NOTE:  Add "DolVolDaily3m" or similar feature to control
                     # for thinly traded stocks.
                     #-------------------------------------------------------------
                     mraw  <- trade_overnight(predmat      = predmat,
                                              maxposn      = meritFUNpar$maxposn,
                                              maxweight    = meritFUNpar$maxweight,
                                              longthresh   = meritFUNpar$longthresh,
                                              shortthresh  = meritFUNpar$shortthresh,
                                              dolvolname   = meritFUNpar$dolvolname,
                                              dolvolthresh = meritFUNpar$dolvolthresh,
                                              dateseries   = mktseries,
                                              datecol      = datecolname,
                                              yhatcol      = "predict",
                                              retcol       = ycolname  )



                     switch(meritFUN,
                            overnight_MDD = {
                              mraw_rets  <- TTR::ROC(mraw$summary[, "Equity_Curve"], type = "discrete")
                              mraw_score <- xtsmdd(mraw_rets)  # MDD

                            },
                            overnight_returns = {
                              mraw_score  <- last(mraw$summary[, "Equity_Curve"]) - 1  #  Total returns

                            },
                            overnight_Sharpe = {
                              mraw_gain  <- last(mraw$summary[, "Equity_Curve"]) - 1
                              mraw_rets  <- TTR::ROC(mraw$summary[, "Equity_Curve"], type = "discrete")[-1]
                              mraw_sd    <- sd(mraw_rets)
                              mfactor    <- sqrt(252 / nrow(mraw_rets))
                              mraw_score <- mfactor * mraw_gain / mraw_sd   # Sharpe ratio annualized

                            },
                            overnight_MAR = {
                              mraw_gain  <- last(mraw$summary[, "Equity_Curve"]) - 1
                              mraw_rets  <- TTR::ROC(mraw$summary[, "Equity_Curve"], type = "discrete")
                              mraw_mdd   <- xtsmdd(mraw_rets)
                              mraw_score <- mraw_gain / mraw_mdd

                            }, {
                              # Default - throw error
                              stop("ERROR: meritFUN invalid.")
                            }
                     )

                     #--------------------------------------------------------------------
                     # Maximize mscore: update bestmodel if current model does better
                     #--------------------------------------------------------------------
                     if(mscore < mraw_score) {
                       mscore <- mraw_score
                       bestmodel <- i
                     }

                   }
                 }


                 # Assess the performance of the current feature set
                 # use assess_features, and add other performance functions.

               }  #####  END for i loop  ####

               #-------------------------------------------------------------------
               # Select feature set from best model as best_featureset
               #-------------------------------------------------------------------
               best_featureset <- featurelist[[bestmodel]]

             },
             forwardsearch = {
               sprint("method is fwdsearch")
             },              {
                 # Switch statement default, so stop execution
                 stop("invalid search method.  stopping in switch statement.")
             })


      # loop over features using either list of forward search.
      # evaluate all models (one or many) for best thresholds
      # return best threshold row number and best feature set.

    } ####### ENDIF gridsearch  #########

    #------------------------------------------------------------------
    # Extract the column names of interest
    # Train FINAL model and predict at WFO date using best_featureset
    #------------------------------------------------------------------
    subcols   <- c(ycolname, best_featureset)
    sprint("Predicting on wfodate: %s", wfodates[idate])
    yhat      <- ml_trainpredict(train_set   = trainset[, subcols],
                                 valid_set   = predset[, subcols],
                                 ycol        = ycol,
                                 mlalgo      = mlalgo,
                                 mlpar       = mlpar  )

    #------------------------------------------------------------------
    # Trade the model using best_tradePAR parameters.
    #   . Accumulate returns over WFO period to WFO + 1
    #------------------------------------------------------------------
    yhat   <- cbind(yhat, predset[, datecol, drop = FALSE],
                    predset[, IDcolname, drop = FALSE])

    trades <- trade_overnight(predmat       = yhat,
                              maxposn       = tradePAR[best_tradePAR, "max_posn"],
                              maxweight     = tradePAR[best_tradePAR, "max_weight"],
                              longthresh    = tradePAR[best_tradePAR, "long_thresh"],
                              shortthresh   = tradePAR[best_tradePAR, "short_thresh"],
                              dateseries    = mktseries,
                              datecol       = datecolname,
                              yhatcol       = "predict",
                              retcol        = ycolname  )

    #------------------------------------------------------------------------
    # Accumulate rets, trade details and trade summary in their containers
    #------------------------------------------------------------------------
    retswfo                 <- trades[["rets"]]
    rets[index(retswfo), ]  <- retswfo

    summwfo                 <- trades[["summary"]]
    tsumm[index(summwfo), ] <- summwfo

    trade_details <- rbind(trade_details, trades[["details"]])


  } ########  END Main Loop:  iterate wFO dates  ##########


  #------------------------------------------------------------------
  # Trim NAs in xts containers and build the return list
  #------------------------------------------------------------------
  rets               <- na.trim(rets,  sides = "both")
  rets[is.na(rets)]  <- 0
  ec                 <- cumprod_na(1 + rets)

  tsumm               <- na.trim(tsumm, sides = "both")
  tsumm[is.na(tsumm[, "allstocks"]), "allstocks"] <- 0
  tsumm[is.na(tsumm[, "Ntrades"]), "Ntrades"]     <- 0

  #tsumm_na            <- is.na(tsumm[, 2] | is.na(tsumm[, 3]))
  #tsumm[tsumm_na, ]   <- rep(0, ncol(tsumm))
  tsumm$Equity_Curve  <- ec


  rlist <- list(rets    = rets,
                summary = tsumm,
                details = trade_details )

  return(rlist)


}  ###### END FUNCTION ml_gridwfo  ######
