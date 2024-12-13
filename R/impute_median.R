#' Impute Missing Values with Median
#'
#' This function replaces missing values in numeric columns of a dataset with the median of each respective column.
#'
#' @param data A data frame containing numeric columns. Non-numeric columns will be ignored.
#' @return A data frame with missing values in numeric columns replaced by the column median.
#' @examples
#' data <- data.frame(x = c(1, NA, 3), y = c(4, 5, NA))
#' dere(data)
#' @export

dere <- function(data) {
  # Calculate missing value counts for each column
  mc <- sapply(data, function(colmn) sum(is.na(colmn)))

  # Filtering out columns with no missing values
  mc <- mc[mc > 0]

  # Print missing counts (optional)
  cat("\nMissing Counts Table: \n")
  print(mc)

  # Print the median for each column
  cat("\nMedian for each column:\n")
  medians <- sapply(data, function(colmn) median(colmn, na.rm = TRUE))
  print(medians)


  # Updating the NA values with the column's median
  ud <- data.frame(sapply(data, function(colmn) {
    colmn[is.na(colmn)] <- median(colmn, na.rm = TRUE)  # Replace NA with column's median
    return(colmn)
  }))

  # Return the updated data frame
  return(ud)
}

