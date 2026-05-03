#' Utilities for biblioCNKI
#'
#' Internal helper functions.
#'
#' @name utils
#' @keywords internal
NULL

#' Re-export magrittr pipe
#'
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Check required columns in a data frame
#'
#' @param df A data frame.
#' @param required_cols Character vector of required column names.
#' @return Invisible TRUE if all columns present; otherwise an error is thrown.
#' @export
check_columns <- function(df, required_cols) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}