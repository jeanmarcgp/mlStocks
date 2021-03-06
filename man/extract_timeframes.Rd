% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_timeframes.R
\name{extract_timeframes}
\alias{extract_timeframes}
\title{Extract the timeframe from a date column in one or multiple dataframes}
\usage{
extract_timeframes(dflist, cdate, showpct = TRUE, printout = TRUE)
}
\arguments{
\item{dflist}{A list of dataframes to be examined.  The list should
be named, as each name is associated with a row in the
dataframe returned by the function.  See details.}

\item{cdate}{The name or number specifying the column in the dataframes
containing the dates.  The dates may be of class Date or
char.  If char, they will first be converted to class Date.}

\item{showpct}{Logical.  If true (default) then a column is included
in the returned dataframe showing the size (by number of
rows) of the dataframe examined relative to the size of
all dataframes in the list provided.  This is expressed
as a percentage.}

\item{printout}{Logical.  Specifies whether to print the returned dataframe
to the console or not.  Default is TRUE.}
}
\value{
A dataframe is created with the timeframe information.
          Each dataframe in the provided list has an associated
          row with it.  The column information includes the following:
}
\description{
For each dataframe in the provided list, find the oldest and
newest dated row from a specified date column. Report the
corresponding timeframe. In addition, the number of rows of each
dataframe is counted and reported as a percentage of the total
of all dataframe rows.  This provides a sense of the size of each
dataframe compared to the list provided.
}
\details{
The dataframe list may contain one or more xts matrices.  In
such case, the cdate argument is ignored, and the index of the
xts matrix is used to calculate the timeframe.
}

