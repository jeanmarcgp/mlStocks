
####################################################################################
# FILE assess_features.R
#
# FUNCTIONS IN THIS FILE:
# . assess_features
# . MSD
# . trading_returns
#
####################################################################################
#
#' Assess the predictive power of a set of features using a machine learning model
#'
#' Trains a machine learning model using the features provided as argument,
#' then validates it against the validation set.  The function evaluates the results
#' using an externally provided function to obtain a figure of merit associated
#' with the given set of features.  When used iteratively to loop through a group
#' of different sets of features, the figure of merit can be compared to assess the
#' relative predictive power of each feature set.
#'
#' This is a low-level function normally used within a higher level loop to
#' perform feature selection through iteratively training and validation.
#'
#'
#' @param features    A vector of features with which to train the machine learning
#'                    algorithm.
#'
#' @param train_set   The training set used to build the model.  Column 1 should
#'                    contain the target variable (y).  The features argument above
#'                    is used to subset the train_set to extract the features for training.
#'
#' @param val_set     The validation set used to validate the model's performance.
#'                    Column 1 should contain the target variable (y).  The features
#'                    argument above is used to subset the val_set and extract the
#'                    features for predicting.
#'
#' @param Nrepeat     Number of times to iterate the train-validate process.  This is
#'                    useful to build and validate multiple identical models and compile
#'                    statistics on the figure of merits for all runs.  Doing this helps
#'                    to empirically determine the hyper-parameter values by ensuring all
#'                    such models make similar predictions.
#'
#' @param mlalgo      The machine learning algorithm used to build the model.
#'
#' @param mlpar       A named list containing the machine learning model parameters.
#'                    If a parameter is missing, then the model's defaults are used.
#'
#' @param meritFUN    The name of a function used to calculate a numeric figure
#'                    of merit (FOM) to include in the return list for evaluation by
#'                    an upper layer function.
#' @param meritFUNpar The name of a function used to calculate a numeric figure
#'                    of merit (FOM) to include in the return list for evaluation by
#'                    an upper layer function.
#'
#' @return  Returns a list with the following elements:
#'
#'
#' \describe{
#'   \item{\preformatted{$features}}{
#'      A vector of the features used to develop the model.  This is identical to
#'      the features argument.
#'   }
#'   \item{\preformatted{$FOM}}{
#'      The numeric figure of merit associated with the model, based on evaluating
#'      it against the validation set and using the provided function meritFUN.
#'   }
#'   \item{\preformatted{$meritFUN}}{
#'      The name of the merit function meritFUN used to calculated FOM.
#'   }
#'   \item{\preformatted{$resmat}}{
#'      The matrix containing the predicted (yhat) and actual (y) results based on
#'      making a prediction using the validation set.
#'   }
#' }
#'
#' @export
#-----------------------------------------------------------------------------------
assess_features <- function(features, train_set, val_set, Nrepeat = 1, mlalgo = "rf",
                            mlpar       = list(mtry = 1, ntree = 1000, min_rows = 5),
                            meritFUN    = "trading_returns",
                            meritFUNpar = list(long_thres = 0, short_thres = 0)) {

  # ######## For Testing Code #######
  # library(mlStocks)
  # features          = c("Perc52WkHiRank", "PQMaxNDayRetRank")
  # train_set         = Earnings[1:3000, c("Ret1", features)]
  # val_set           = Earnings[3001:3500, c("Ret1", features)]
  # Nrepeat     = 2
  # mlalgo      = "xgboost"
  # mlpar       = pad_mlpar(mlalgo = mlalgo)
  # meritFUN    = "trading_returns"
  # meritFUN    = "RMSE"
  # meritFUNpar = list(long_thres = 0.005, short_thres = -75)
  #
  # #######

  #-------------------------------------------------------------------
  # Extract the relevant columns to create train & validation sets
  #-------------------------------------------------------------------
  if(class(features) == "character") {
    traindata <- train_set[, c(1, which(colnames(train_set) %in% features))]
    valdata   <- val_set[, c(1,  which(colnames(val_set) %in% features))]
  } else {
    traindata <- train_set[, c(1, features)]
    valdata   <- val_set[, c(1, features)]
  }

  nc   <- ncol(traindata)
  stopifnot(nc == ncol(valdata))

  #-------------------------------------------------------------------
  # Ensure meritFUNpar has valid parameters for chosen meritFUN
  #-------------------------------------------------------------------
  if(meritFUN %in% c("trading_returns")) {
    if(!is.numeric(meritFUNpar[["long_thres"]]))
      stop("meritFUNpar$long_thresh must be specified and numeric.")

    if(!is.numeric(meritFUNpar[["short_thres"]]))
      stop("meritFUNpar$short_thresh must be specified and numeric.")
  }

  #-------------------------------------------------------------------
  # MAIN LOOP:  Repeat train-predict N times and compile stats
  #-------------------------------------------------------------------
  FOMdf  <- NULL
  for(i in 1:Nrepeat) {
    if(Nrepeat > 1) {
      sprint("\nFunction assess_features: Run # %s", i)
      sprint("----------------------------------------")
    }

    yhat <- ml_trainpredict(train_set = traindata,
                            valid_set = valdata,
                            ycol      = 1,
                            mlalgo    = mlalgo,
                            mlpar     = mlpar)

    # Compute figure of merit
    FOMvec <- switch(meritFUN,
                     RMSE            = RMSE(yhat),
                     trading_returns = trading_returns(yhat,
                                                       long_thres  = meritFUNpar$long_thres,
                                                       short_thres = meritFUNpar$short_thres),
                     stop("meritFUN not a valid function in switch statement.")
                     )

    FOMdf <- rbind(FOMdf, as.data.frame(t(FOMvec)))

  }  ##########  END MAIN LOOP  ##########


  #---------------------------------------------------------
  # Build the return list
  #---------------------------------------------------------
  if(meritFUN %in% c("trading_returns")) {
    df      <- data.frame(alpha_mean  = mean(FOMdf$Trade_Alpha),
                          alpha_sd    = sd(FOMdf$Trade_Alpha),
                          PctTraded   = mean(FOMdf$PctTraded),
                          PctLongs    = mean(FOMdf$PctLongs),
                          PctShorts   = mean(FOMdf$PctShorts),
                          Nmarket     = FOMvec["Nmarket"])
    FOMlist <- list(features   = features,
                    summary    = df,
                    values     = FOMdf )
  } else {
    colnames(FOMdf) <- "RMSE"
    FOMlist <- list(features   = features,
                    values     = FOMdf)
  }

  return(FOMlist)

}  ##########  END FUNCTION assess_features  ##########



#-----------------------------------------------------------------------------------
#  FUNCTION RMSE:
#
#' Calculate the Root Mean Square Error of a 2 column matrix
#'
#' Given a two column matrix (typically yhat and y), this function
#' calculates the root mean square error between y and yhat.
#'
#' ssuming N is the number or rows, the calculation for the RMSE
#' value is done according to the following formula:
#'
#' \deqn{\sqrt \sum(yhat - y)^2 / N}
#'
#' @param mat  The two column matrix used to calculate the mean squared difference.
#'
#' @return Returns the RMS error of the two columns in the matrix.
#'
#' @export
#-----------------------------------------------------------------------------------
RMSE <- function(mat) {
  stopifnot(ncol(mat) == 2)

  #mat      <- matrix(c(1,2,3,1.1, 2.2, 3.25), ncol = 2)
  ######

  mdiff    <- mat[,2] - mat[, 1]
  msd      <- sqrt(sum(mdiff^2) / nrow(mat))

  return(msd)

} ##########  END FUNCTION MSD  ##########

#-----------------------------------------------------------------------------------
#  FUNCTION trading_returns
#
#' Calculate the average trading returns based on predicted returns
#'
#' A two column matrix is provided containing a prediction in
#' column 1 (yhat), and the actual trading returns in column 2 (y).
#' The function first identifies valid long and short trades by
#' examining yhat, and flagging the trade as a long trade
#' if yhat >= long_thres, or as a short trade if yhat <= short_thres.
#' Vectors with yhat values in the band between the two thresholds
#' are ignored (deemed not worth trading).
#'
#' @param mat          The two column matrix to calculate the trading returns.
#'                     The first column is the prediction (yhat), while the
#'                     second column is the target (y).
#'
#' @param long_thres   The threshold above which the yhat column is flagged
#'                     as a trade to go long (buying the security).
#'
#' @param short_thres  The threshold below which the yhat colum is flagged as
#'                     a trading to go short (shorting the security).
#'
#' @return   Returns the average trading return over all flagged trades in the
#'           matrix.  For short trades, a negative value of y corresponds to
#'           a positive return for this calculation.
#'
#' @export
#-----------------------------------------------------------------------------------
trading_returns <- function(mat, long_thres = 0, short_thres = 0) {
  stopifnot(ncol(mat) == 2)

  # ##### For code testing only #####
  # mat         = matrix(c(0.05, -0.03, 0.08, 0.1, 0.06, -0.04, 0.075, 0.088), ncol = 2)
  # colnames(mat) <- c("yhat", "y")
  # print(mat)
  # long_thres  = 0.055
  # short_thres = -0.03
  # #####

  mat            <- as.data.frame(mat)
  mat$longrets   <- ifelse(mat[, 1] >= long_thres, mat[, 2], NA)
  mat$shortrets  <- ifelse(mat[, 1] <= short_thres, -mat[, 2], NA)

  mat$traderets  <- rowSums(mat[, c("longrets", "shortrets")], na.rm = TRUE)
  Nmarket        <- nrow(mat)
  Nlongs         <- length(which(!is.na(mat$longrets)))
  Nshorts        <- length(which(!is.na(mat$shortrets)))
  Ntrades        <- Nlongs + Nshorts

  mean_traderets <- sum(mat$traderets) / Ntrades
  market_rets    <- mean(mat[, 2])

  trade_alpha    <- mean_traderets - market_rets
  Pctlongs       <- round(100 * Nlongs / Nmarket, 1)
  Pctshorts      <- round(100 * Nshorts / Nmarket, 1)
  Pcttraded      <- round(100 * (Nlongs + Nshorts) / Nmarket, 1)

  #-----------------------------------------
  # Build the return vector
  #-----------------------------------------
  FOM     <- c(trade_alpha, mean_traderets, market_rets,
               Nlongs, Nshorts, Nmarket, Pctlongs, Pctshorts, Pcttraded)

  names(FOM) <- c("Trade_Alpha", "Trade_Rets", "Market_Rets", "Nlongs",
                  "Nshorts", "Nmarket", "PctLongs", "PctShorts", "PctTraded")

  return(FOM)

}  ##########  END FUNCTION trading_returns  ##########
