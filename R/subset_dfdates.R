####################################################################################
# FILE subset_dfdates.R
#
# Functions in this file
#  .subset_dfdates
#
####################################################################################
#
#' Subsets a dataframe by dates
#'
#' The dataframe argument must contain a column with valid dates.  The
#' timeframe argument specifies a timeframe xts-style daily.  For example,
#' "2011-01-01/2011-12-31" is valid whereas "2011" is not.
#'
#' @param df         The dataframe to subset by dates
#'
#' @param timeframe  A character string specifying the timeframe as two
#'                   full dates separated by a slash.  For example,
#'                   "2010-10-01/2010-10-22".
#'
#' @param datecol    The column name where the dates are found in the dataframe.
#'                   Although these may be of class char, they will be converted
#'                   to class Date before subsetting is applied.
#'
#' @param keep_NAs   Logical flag specifying whether to apply complete.cases
#'                   to the dataframe to eliminate rows with NAs.
#'
#' @param bounds_rm  Logical vector of length 2.  Specified whether to exclude
#'                   the rows at the boundary dates specified in timeframe.  The
#'                   first element specifies whether to exclude the start dates
#'                   while the second element specified whether to exclude the
#'                   end dates.  Default is c(FALSE, FALSE), so both the beginning
#'                   dates and end dates are included.
#'
#' @return  Returns a subsetted dataframe containing rows with dates within the
#'          timeframe supplied.  Also, complete.cases is applied to eliminate
#'          rows with NAs if keep_NAs = FALSE (default).
#'
#' @export
#-----------------------------------------------------------------------------------
subset_dfdates <- function(df, timeframe, datecol, keep_NAs = FALSE,
                           bounds_rm = c(FALSE, FALSE)) {



  cnames  <- colnames(df)
  ndate   <- as.numeric(as.Date(df[, datecol]))
  df      <- cbind(ndate, df)
  df      <- df[order(df$ndate), ]  # sort by ascending dates

  datevec <- as.Date(unlist(str_split(timeframe, "/")))
  datenum <- as.numeric(datevec)

  #----------------------------------------------------------
  # Remove or include beginning dates and end dates
  #----------------------------------------------------------
  if(bounds_rm[1]) startdates <- df$ndate > datenum[1] else
    startdates <- df$ndate >= datenum[1]

  if(bounds_rm[2]) enddates   <- df$ndate < datenum[2] else
    enddates   <- df$ndate <= datenum[2]

  dfsub   <- df[(startdates & enddates), ]
  dfsub   <- dfsub[, cnames]

  if(!keep_NAs) dfsub <- dfsub[complete.cases(dfsub), ]

  return(dfsub)

}  ############  END FUNCTION subset_dfdates  ##############




