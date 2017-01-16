
####################################################################################
# FILE pad_mlpar.R
#
#
####################################################################################
#
#' Pads a list of ML algorithm paramters with defaults if not provided
#'
#' For any given ML algorithm, several parameters must be provided.  This
#' function will return a list (mlpar) with a sensible set of default
#' arguments, unless it is overridden by specifying it in the mlpar
#' argument.
#'
#' @param mlalgo   The name, in quote or the ml algorithm to populate with
#'                 its parameters.  Supported ml algorithms include "h2o.rf",
#'                 "xgboost".
#'
#' @param mlpar    A list of named ml parameters to override the default. If
#'                 nothing is specified, then NULL is assumed and the funtion
#'                 use all the embedded defaults.
#'
#' @return Returns a list of parameters for the said ml algorithm.
#'
#' @export
#------------------------------------------------------------------------------------
pad_mlpar <- function(mlalgo = NULL, mlpar = NULL) {

  switch(mlalgo,
         h2o.rf = {
           if(is.null(mlpar$mtry))      mlpar$mtry       <- -1
           if(is.null(mlpar$ntree))     mlpar$ntree      <- 999
           if(is.null(mlpar$min_rows))  mlpar$min_rows   <- 5
           if(is.null(mlpar$max_depth)) mlpar$max_depth  <- 20
         },
         xgboost = {
           if(is.null(mlpar$nrounds))   mlpar$nrounds    <- 1000
           if(is.null(mlpar$verbose))   mlpar$verbose    <- 1
           if(is.null(mlpar$print_every_n)) mlpar$print_every_n <- 50

           if(is.null(mlpar$missing))   mlpar$missing    <- NULL
           if(is.null(mlpar$params))    mlpar$params <- list(objective = "reg:linear",
                                                             eta       = 0.01,
                                                             max_depth = 1)


         }, {
           #----------------------------------------------
           # Default, stop function
           #----------------------------------------------
           stop("mlalgo must be a valid mlalgo name.")
         })

  return(mlpar)

}

