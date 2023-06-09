% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/medianNA.R
\name{medianNA}
\alias{medianNA}
\title{Median NA}
\usage{
medianNA(df)
}
\arguments{
\item{df}{The dataframe}
}
\value{
The dataframe with NA values replaced by that column's median
}
\description{
This function allows users to replace the NA values of a dataframe with that column's median. The user does not need to specify the numeric columns, as the function knows to ignore columns containing characters.
}
