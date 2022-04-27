#' Remove constant columns in data
#'
#' @description Given some data X, removes all columns in the data that are a
#'   constant value (while ignoring NAs).
#'
#' @param X A data frame.
#'
#' @return A data frame where constant columns have been removed.
#'
#' @examples
#' X <- data.frame(a = c(1, 1, 1), b = c(1, 2, 3))
#' X_cleaned <- remove_constant_columns(X)
remove_constant_columns <- function(X) {
  const_cols <- purrr::map_lgl(X, ~all(duplicated(.x)[-1L]))
  X_cleaned <- X %>%
    dplyr::select(which(!const_cols))
  return(X_cleaned)
}
