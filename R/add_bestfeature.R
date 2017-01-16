####################################################################################
# FILE add_bestfeature.R
#
#
####################################################################################
#
#' Add the best feature from a list to improve an existing set
#'
#' Given a list of candidate features and a separate base set of features,
#' this functon will seach through the list to find the single most predictive
#' feature to add to the base set.  This is useful during search in feature
#' selection algorithms.
#'
#' For each feature in the feature_list set, train a machine learning model
#' by combining the candidate feature with the base_set and validate
#' against a validation data set. The function evaluates the performance
#' against the validation set using an externally provided function to obtain
#' a figure of merit associated with the feature set being evaluated.  When used
#' iteratively to loop through different sets of features, the figure of merit
#' can be compared to assess the relative predictive power of each feature set.
#' The best performing set is identified and the associated features set is
#' returned as a dataframe ($bestruns) as part of a list.
#'
#' This is a low-level function normally used within a higher level loop to
#' perform feature selection through iteratively training and validation.
#'
#' @param base_set     The base set of features that is given and used in all
#'                     model train/validate runs.  At every run, one feature
#'                     from the feature_list is selected and added to this
#'                     base_set, and used to train and validate the model.
#'
#' @param feature_list A list or a vector of features used during the iterative
#'                     loop.  At each iteration, one feature from this list is
#'                     extracted and combined with the base_set, and the resulting
#'                     set is used to train the machine learning model.
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
#'   \item{\preformatted{$summary}}{
#'      A dataframe containing the summary results for each feature set tested.  Each
#'      feature set is a dataframe row.  The columns include:
#'
#'      \itemize{
#'         \item
#'         \strong{added_feature  } The feature added to the feature set at the given run.
#'
#'         \item
#'         \strong{alpha_mean     } The average of the alpha generated among all identical models.
#'                                  The function generates Nrepeat identical models for this purpose,
#'                                  and alpha is the model's mean return - the market's mean return.
#'
#'         \item
#'         \strong{alpha_sd  }      The standard deviation of the alphas obtained from all
#'                                  Nrepeat identical models.
#'
#'         \item
#'         \strong{Normalized_sd  } The normalized alpha standard deviation, taken as alpha_sd / alpha_mean.
#'
#'
#'         \item
#'         \strong{PctTraded      } The percentage of vectors in the validation set that are selected to
#'                                  be traded according to the meritFUN argument.
#'
#'         \item
#'         \strong{PctLongs       } The percentage of vectors in the validation set that are
#'                                  traded as longs (buy trades).
#'
#'         \item
#'         \strong{PctShorts      } The percentage of vectors in the validation set that are traded
#'                                  as shorts (sell trades).
#'
#'         \item
#'         \strong{Nmarket        } The number of vectors available in the validation set.  This defines
#'                                  the market against which the models attempt to extract positive alpha.
#'      }
#'
#'   }
#'   \item{\preformatted{$bestrun}}{
#'      A dataframe row identical as the summary above, but containing the best run data as measured by
#'      the highest alpha_mean. It also has one additional column 'feature_set', which is a
#'      character string of the best feature set, where each feature is separated by a comma.
#'   }
#'   \item{\preformatted{$bestset}}{
#'      A character vector of the best feature set.  This is the set used as the best run but organized
#'      as a character vector instead of a single string as in bestrun$featureset above.
#'   }
#'   \item{\preformatted{$rundetails}}{
#'      A dataframe containing the details of each underlying run.  The number of rows equals
#'      Nrepeat runs * the number of features tested.  For example, if 3 features are tested
#'      against the base set and for each feature set we rebuild the model 8 times, then we have
#'      3 * 8 = 24 runs and rows in this dataframe.  The columns are as follows:
#'      \itemize{
#'         \item
#'         \strong{Trade_Alpha  } The alpha generated by the given run.  Alpha is the average
#'                                return of all trades identified by the run, minus the average
#'                                of all trades in the market.
#'         \item
#'         \strong{Trade_Rets  }  The average return of all trades identified by the run.
#'
#'         \item
#'         \strong{Market_Rets  } The average return of all trades in the market (validation set).
#'
#'         \item
#'         \strong{NLongs       } The number of vectors in the validation set that are
#'                                traded as longs (buy trades).
#'
#'         \item
#'         \strong{NShorts      } The number of vectors in the validation set that are traded
#'                                as shorts (sell trades).
#'
#'         \item
#'         \strong{Nmarket      } The number of vectors available in the validation set. This defines
#'                                the market against which the models attempt to extract a positive alpha.
#'
#'         \item
#'         \strong{PctLongs     } The percentage of vectors in the validation set that are
#'                                traded as longs (buy trades).
#'
#'         \item
#'         \strong{PctShorts    } The percentage of vectors in the validation set that are traded
#'                                as shorts (sell trades).
#'
#'         \item
#'         \strong{PctTraded    } The percentage of vectors in the validation set that are traded
#'                                in total (Longs + Shorts).
#'
#'
#'
#'      }
#'   }
#' }
#'
#' @export
#-----------------------------------------------------------------------------------
add_bestfeature <-  function(base_set, feature_list, train_set, val_set, Nrepeat = 1,
                             mlalgo      = "h2o_rf",
                             mlpar       = list(mtry = 1, ntree = 1000, min_rows = 5),
                             meritFUN    = "trading_returns",
                             meritFUNpar = list(long_thres = 0, short_thres = 0)) {

  # ##########  For code testing only  ###########
  # library(xtsanalytics)
  # base_set          = c("PQCAGRLongNRank", "PERank", "PQCAGRShortNRank")
  # feature_list      = c("Perc52WkHiRank", "PQMaxNDayRetRank")
  # train_set         = Earnings[1:3000, c("Ret1", base_set, feature_list)]
  # val_set           = Earnings[3001:3500, c("Ret1", base_set, feature_list)]
  # Nrepeat    = 2
  # mlalgo     = "xgboost"
  # mlpar      = pad_mlpar(mlalgo = mlalgo)
  #
  # meritFUN   = "trading_returns"
  # meritFUNpar = list(long_thres = 0.005, short_thres = -75)
  #
  # ##############################

  #--------------------------------------------------------
  # Clean up the training and validation sets from NAs
  #--------------------------------------------------------
  train_set <- train_set[complete.cases(train_set), ]
  val_set   <- val_set[complete.cases(val_set), ]

  #-------------------------------------------
  # Convert feature lists to vectors
  # Ensure fvec excludes bvec
  #-------------------------------------------
  bvec  <- unlist(base_set)
  fvec  <- unlist(feature_list)
  fvec  <- fvec[!(fvec %in% bvec)]
  if(length(fvec) == 0) fvec <- NA

  sprint("fvec length is, %s.  fvec is:", length(fvec))
  print(fvec)

  #-----------------------------------------------------------------------
  # Initiliaze dataframes to collect results from each iteration
  #-----------------------------------------------------------------------
  rundf <- NULL
  df    <- data.frame(added_feature = fvec)  # First col sets nrows

  #---------------------------------------------------------
  #  MAIN LOOP
  #---------------------------------------------------------
  for(i in 1:length(fvec)) {
    # Build feature set
    if(!is.na(fvec[i])) fset <- c(bvec, fvec[i]) else
      fset <- bvec
    sprint("fset is:")
    print(fset)
    fset_cat <- stringr::str_c(fset, collapse = ", ")
    FOM  <- assess_features(features = fset,     train_set   = train_set,
                            val_set  = val_set,  Nrepeat     = Nrepeat,
                            mlalgo   = mlalgo,   mlpar       = mlpar,
                            meritFUN = meritFUN, meritFUNpar = meritFUNpar)

    df[i, "alpha_mean"]    <- FOM$summary$alpha_mean
    df[i, "alpha_sd"]      <- FOM$summary$alpha_sd
    df[i, "Normalized_sd"] <- FOM$summary$alpha_sd / FOM$summary$alpha_mean
    df[i, "PctTraded"]     <- FOM$summary$PctTraded
    df[i, "PctLongs"]      <- FOM$summary$PctLongs
    df[i, "PctShorts"]     <- FOM$summary$PctShorts
    df[i, "Nmarket"]       <- FOM$summary$Nmarket
    df[i, "feature_set"]   <- fset_cat
    dfdet                  <- FOM$values
    dfdet$added_feature    <- fvec[i]
    rundf                  <- rbind(rundf, dfdet)

    sprint("\n======================================================================")
    sprint("   Results for feature set:")
    print(fset_cat, row.names = FALSE)
    sprint("------------------------------------------------------------\n")
    print(df[i, ])

  }  ##########  END MAIN LOOP  ##########



  #------------------------------------------------------
  # Build the return list
  #------------------------------------------------------
  nc      <- ncol(rundf)
  bestrun <- df[which(df$alpha_mean == max(df$alpha_mean)), ]
  bestset <- trimspaces(unlist(str_split(bestrun$feature_set, ",")))
  retlist <- list(summary    =  df,
                  bestrun    =  bestrun,
                  bestset    =  bestset,
                  rundetails =  rundf[, c(nc, 1:(nc-1))])

  return(retlist)

}  #############  END FUNCTION add_bestfeature  ###############



