#' Median NA
#'
#' @param df The dataframe that needs to have NA values changed to that column's median
#'
#' @return The dataframe with NA values replaced by that column's median
#' @export
#'
#' @examples

medianNA <- function(df){
  # Creating function to get columns with only numeric values
  colNumeric <- unlist(lapply(df, is.numeric))
  df_numeric <- df[ , colNumeric]
  # Creating sub-function to replace the NA values
  df[colNumeric] <- lapply(df[colNumeric], function(x) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    x
  })
  df
}


