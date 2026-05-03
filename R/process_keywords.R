#' Split Keywords into Rows
#'
#' Takes the cleaned data and splits multiple keywords (separated by `;;`) into individual rows.
#'
#' @param cleaned_data Data frame from `process_citation_data`.
#' @param output Optional output path for Excel file.
#' @return A data frame with columns Pid, Date, Keyword (each keyword in a separate row).
#' @export
process_keywords <- function(cleaned_data, output = NULL) {
  if (!all(c("Pid", "Date", "Keyword") %in% names(cleaned_data))) {
    stop("Input must contain Pid, Date, Keyword columns.")
  }

  kw <- cleaned_data %>%
    dplyr::select(Pid, Date, Keyword) %>%
    tidyr::separate_rows(Keyword, sep = ";;") %>%
    dplyr::mutate(Keyword = trimws(Keyword)) %>%
    dplyr::filter(Keyword != "")

  if (!is.null(output)) {
    writexl::write_xlsx(kw, path = output)
  }

  kw
}