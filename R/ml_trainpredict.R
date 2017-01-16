
####################################################################################
# FILE ml_trainpredict.R
#
####################################################################################
#
#' Train and predict a Machine Learning model
#'
#' This is a generic function that uses the specified machine learning algorithm
#' to train a model from the training set.  It then makes its prediction from
#' a specified validation set.  It is essentially a wrapper to ease the use
#' of using multiple machine learning algorithms.
#'
#' @param train_set   The training set used for training the model
#'
#' @param valid_set   The validation set used to make predictions
#'
#' @param ycol        The name or column number where the target variable y
#'                    is found in the training and validation set.  The validation
#'                    set ycol is only used to bind the y column adjacent to yhat.
#'                    This allows for easy comparison of actual vs predicted values
#'                    in the matrix returned.
#'
#' @param IDinfo      This is a matrix of one or multiple columns and the same number
#'                    of rows as in the validation set.  It normally contains the date
#'                    information related to each trade, but may include other columns
#'                    such as the symbol traded, etc.  These columns are binded as
#'                    additional columns in the data frame returned.  None of this
#'                    information is used to make predictions and values do not
#'                    have to be unique (for instance, there may be multiple predictions
#'                    made on a given date). Default is NULL (no IDinfo added to
#'                    the dataframe returned).
#'
#' @param mlalgo      The machine learning algorithm to use during training
#'                    and prediction.  Currently supported algorithms include:
#'                    "rf", "h2o.rf", and "xgboost".
#'
#' @param mlpar       A list containing the parameters required by the ML algorithm
#'                    selected by mlalgo.  If NULL, then
#'
#' @return Returns a dataframe with the following 3 columns and the same number of
#'         rows as the validation set provided.  Column 1, named "predict"
#'         contains the predicted value.  Column 2 contains the actual
#'         target value as provided by ycol in the validation set.  Column 3
#'         is the datecol if it was provided as an argument.  Note that datecol
#'         must be of same length as the validation set.
#'
#' @export
#-----------------------------------------------------------------------------------
ml_trainpredict <- function(train_set = NULL, valid_set = NULL, ycol = 1,
                            IDinfo = NULL, mlalgo = "xgboost", mlpar = NULL) {


  # ###########  Code for testing  ##########
  # library(xtsanalytics)
  # features          = c("Perc52WkHiRank", "PQMaxNDayRetRank")
  # train_set         = Earnings[1:3000, c("Ret1", features)]
  # valid_set         = Earnings[3001:3500, c("Ret1", features)]
  # IDinfo            = Earnings[3001:3500, "dtBuy", drop = FALSE]
  #
  # ycol =
  # #################################################


  if(!is.null(IDinfo))
    if(nrow(IDinfo) != nrow(valid_set)) {
      stop("nrow of IDinfo argument not identical to nrow of valid_set")
    }


  # Convert ycol to a numeric if a name
  if(!is.numeric(ycol))
    ycol <- which(colnames(train_set) == ycol)

  if(ycol < 1 | ycol > ncol(train_set))
    stop("ycol out of range or not a valid column name")

  traindata <- train_set
  validdata <- valid_set

  #------------------------------------------------------------------
  # Ensure that mlpar is padded by its defaults if a parameter is
  # not user specified
  #------------------------------------------------------------------
  mlpar <- pad_mlpar(mlalgo = mlalgo, mlpar = mlpar)

  switch(mlalgo,
         h2o.rf = {
           #---------------------------------------------------------------------
           # h2o Random Forests modeling and prediction.
           #---------------------------------------------------------------------
           h2o::h2o.removeAll()

           y          <- colnames(traindata)[ycol]
           train      <- h2o::as.h2o(traindata)
           validation <- h2o::as.h2o(validdata)
           ml <-  h2o::h2o.randomForest(x                 = features,
                                        y                 = y,
                                        training_frame    = train,
                                        #validation_frame  = validation,
                                        mtries            = mlpar$mtry,
                                        ntrees            = mlpar$ntree,
                                        min_rows          = mlpar$min_rows,
                                        max_depth         = mlpar$max_depth
           )

           sprint("predicting h2o random forest model...")
           yhat    <- as.data.frame(predict(ml, validation))
           predmat <- cbind(yhat, valid_set[, ycol, drop = FALSE])

         },
         xgboost = {
           #----------------------------------------------------------------
           # xgboost selected.
           #----------------------------------------------------------------
           sprint("Training xgboost model...")

           xtrain   <- as.matrix(traindata[, -ycol])
           ytrain   <- as.matrix(traindata[, ycol, drop = FALSE])
           xtest    <- as.matrix(validdata[, -ycol])
           ytest    <- as.matrix(validdata[, ycol, drop = FALSE])
           DM.train <- xgb.DMatrix(data = xtrain, label = ytrain)
           DM.test  <- xgb.DMatrix(data = xtest,  label = ytest)
           ml       <- xgboost::xgboost(DM.train,
                                        missing          = mlpar$missing,
                                        params           = mlpar$params,
                                        nround           = mlpar$nrounds,
                                        verbose          = mlpar$verbose,
                                        print_every_n    = mlpar$print_every_n,
                                        early_stop_round = mlpar$early_stop_round, #
                                        maximize         = mlpar$maximize   #
           )
           sprint("predicting xgboost model...")
           yhat    <- predict(ml,DM.test)
           predmat <- valid_set[, ycol, drop = FALSE]
           predmat$predict <- as.numeric(yhat)
           predmat <- predmat[, c(2, 1)] # predict, Ret1

         }, {
           #----------------------------------------------
           # Default in switch statement
           #----------------------------------------------
           stop("Not h2o.rf or xgboost")
         })


  #-------------------------------------------------------
  # Add IDinfo if provided
  #-------------------------------------------------------
  predmat   <- as.data.frame(predmat)
  if(!is.null(IDinfo))
    predmat[, colnames(IDinfo)] <- IDinfo

  return(predmat)

}  ########  END Function ml_trainpredict  ############


