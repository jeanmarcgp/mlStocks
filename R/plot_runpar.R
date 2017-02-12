#####################################################################
#
#  FILE plot_runpar.R
#
#
##############################################################################
#
# Function plot_runpar
#
#' Plot the parameters for a simulation run
#'
#' Plots the parameters for a simulation in its own plotting window.
#' This is normally used as a header page to describe the key
#' parameters for a simulation.  The function provides
#'
#' @param df            Either a list of parameters, or a dataframe containing
#'                      the run parameters to plot on the screen.  If a list,
#'                      each item should be named as the names correspond to the
#'                      parameter name.  If a dataframe, the row names should
#'                      be the parameter names.
#'
#' @param featurelist   A list of feature vectors to plot on the bottom panel.
#'
#' @param main          The main title at the top panel of the screen.  If no
#'                      title is desired, then set to an empty string "".
#'
#'
#'
#' @param exclude_items  A vector of row numbers or row names corresponding
#'                       to the rows to exclude from showing in the top panel.
#'
#'
#' @export
#---------------------------------------------------------------------------
plot_runpar <- function(df, main = "Run Parameters", featurelist,
                            exclude_items = NA ) {


  #############################################################
  #########  Code used for quick testing during development ######
  # library(mlStocks)
  # df      = list(job_name     = "my job",
  #                featurevec   = "mom126, sdnaup126, mom189, mom252, something, else, more stuff and more stuff, and then some more, and so on",
  #                type         = "h2o.ensemble",
  #                par1         = "asda asd ggh h sd",
  #                par12        = "asda asd ggh h sd"       )
  #
  # featurelist  = list(set1 = c("mom1234", "sdnaup33", "some item"),
  #                     set2 = c("somethign else", "yet more stuff") )
  #
  # main    = "Plot Title goes here"
  #
  # exclude_items = NA
  #
  ####################################################

  #------------------------------------------------------------
  # Prepare the data and asset symbols for plotting
  # Process as a list, then convert to data frame.
  #------------------------------------------------------------
  if(class(df) == "data.frame") df <- as.list(df)
  df <- df[!(names(df) %in% exclude_items)]

  # unlist all elements in the list
  dlist <- lapply(df, FUN = unlist)

  # concatenate all elements, separated by commas
  dlist2 <- lapply(dlist, FUN = str_c, collapse = ", ")

  # then wrap long strings
  dlist3 <- lapply(dlist2, FUN = str_wrap, width = 40)

  # convert to a dataframe
  df     <- data.frame(as.character(dlist3), row.names = names(dlist3))


  flist1 <- lapply(featurelist, FUN = unlist)
  flist2 <- lapply(flist1, FUN = str_c, collapse = ", ")
  flist3 <- lapply(flist2, FUN = str_wrap, width = 40)
  dffeat <- data.frame(as.character(flist3), row.names = names(flist3))

  #-------------------------------------------------------------
  # Set plot region layout as 2 panels, then plot the text!
  #-------------------------------------------------------------
  op <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2)), heights = c(1, 1), widths = 1)

  # Plot the parameter table and the main title
  textplot(df, mar = c(0, 0.5, 3.3, 0.5), show.colnames = FALSE,
           cmar = 4, wrap.rownames = 20)
  title(main, cex.main = 1.7)

  # Plot the featurelist as a matrix
  textplot(dffeat, show.colnames = FALSE,
           mar = c(0, 0.5, 3.0, 0.5))
  title("Feature Vectors")

  par(op)


}  #######  END FUNCTION plot_parameters  #######
