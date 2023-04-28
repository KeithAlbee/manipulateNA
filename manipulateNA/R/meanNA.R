#' Title
#'
#' @param df The dataframe that needs to have NA values changed to that column's mean
#'
#' @return The dataframe with NA values replaced by that column's mean
#' @export
#'
#' @examples

meanNA <- function(df){
  # Creating function to get columns with only numeric values
  colNumeric <- unlist(lapply(df, is.numeric))
  df_numeric <- df[ , colNumeric]
  # Creating sub-function to replace the NA values
  df[colNumeric] <- lapply(df[colNumeric], function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  })
  df
}
