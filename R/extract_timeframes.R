####################################################################################
# FILE extract_timeframe.R
#
# Functions in this file
#  .extract_timeframes
#
####################################################################################
#
#'
#' Extract the timeframe from a date column in one or multiple dataframes
#'
#' For each dataframe in the provided list, find the oldest and
#' newest dated row from a specified date column. Report the
#' corresponding timeframe. In addition, the number of rows of each
#' dataframe is counted and reported as a percentage of the total
#' of all dataframe rows.  This provides a sense of the size of each
#' dataframe compared to the list provided.
#'
#' The dataframe list may contain one or more xts matrices.  In
#' such case, the cdate argument is ignored, and the index of the
#' xts matrix is used to calculate the timeframe.
#'
#' @param dflist   A list of dataframes to be examined.  The list should
#'                 be named, as each name is associated with a row in the
#'                 dataframe returned by the function.  See details.
#'
#' @param cdate    The name or number specifying the column in the dataframes
#'                 containing the dates.  The dates may be of class Date or
#'                 char.  If char, they will first be converted to class Date.
#'
#' @param showpct  Logical.  If true (default) then a column is included
#'                 in the returned dataframe showing the size (by number of
#'                 rows) of the dataframe examined relative to the size of
#'                 all dataframes in the list provided.  This is expressed
#'                 as a percentage.
#'
#' @param printout Logical.  Specifies whether to print the returned dataframe
#'                 to the console or not.  Default is TRUE.
#'
#' @return   A dataframe is created with the timeframe information.
#'           Each dataframe in the provided list has an associated
#'           row with it.  The column information includes the following:
#'
#' @export
#-----------------------------------------------------------------------------
extract_timeframes <- function(dflist, cdate, showpct = TRUE, printout = TRUE) {


  # ######  For code testing only  ###########
  # alldata            = as.data.frame(xts_data[1:200, 1:4])
  # alldata[, "dates"] = row.names(alldata)
  # row.names(alldata) = NULL
  #
  # trainset           = alldata[1:125, ]
  # validset           = alldata[126:161, ]
  # testset            = alldata[162:203, ]
  #
  # dflist   = list(Training_set   = trainset,
  #                 Validation_set = validset,
  #                 Test_set       = testset)
  #
  # dflist   = list(xts_data[1:30, 1:3])
  # cdate    = "dates"
  # showpct  = TRUE
  # printout = TRUE
  # #############

  Ndf    <- unlist(lapply(dflist, nrow))  # nrow of each dataframe
  Ntotal <- sum(Ndf)

  retdf  <- data.frame(DataSet  = names(Ndf))  # First col sets names of datasets

  for(i in 1:length(dflist)) {

    df          <- dflist[[i]]

    if(any(class(df) == "xts")) {
      #  Convert xts to dataframe and add date column
      df         <- as.data.frame(df)
      df$dfcdate <- row.names(df)
    } else {
      df$dfcdate  <- as.Date(df[, cdate])
    }

    tfmin       <- min(df$dfcdate)
    tfmax       <- max(df$dfcdate)

    retdf[i, "Begins"] <- as.character(tfmin)
    retdf[i, "Ends"]   <- as.character(tfmax)
    if(showpct) retdf[i, "PctData"] <- round(100 * Ndf[i] / Ntotal, 2)

  }

  if(printout) {
    sprint("===========================================")
    sprint("  Data Sets Timeframes and Relative Sizes")
    sprint("===========================================")
    print(retdf, row.names = FALSE)
  }

  return(retdf)

}  ########  END FUNCTION extract_timeframes  ##########





