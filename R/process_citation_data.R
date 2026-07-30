#' Process Citation Data from Excel
#'
#' Reads CNKI exported Excel file, selects columns, cleans missing values,
#' standardises dates and returns a tidy data frame. Optionally writes to Excel.
#'
#' @param input Path to Excel file.
#' @param sheet Sheet number or name (default 1).
#' @param output Optional path to save processed Excel file.
#' @return A data frame with columns Pid, Title, Author, AuthorAddress, Date, plus original Keyword column.
#' @export
process_citation_data <- function(input, sheet = 1, output = NULL) {
  raw <- readxl::read_excel(input, sheet = sheet, col_names = TRUE)

  df <- raw %>%
    dplyr::select(
      Title = `Title-题名`,
      Author = `Author-作者`,
      AuthorAddress = `Organ-单位`,
      RawDate = `PubTime-发表时间`,
      Keyword = `Keyword-关键词`,
      Summary = `Summary-摘要`
    ) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::mutate(
      RawDate = gsub("年|月", "-", RawDate),
      RawDate = gsub("日|号", "", RawDate),
      RawDate = sub("\\s.+", "", RawDate)
    ) %>%
    dplyr::mutate(
      Date = lubridate::parse_date_time(RawDate, orders = c("ymd", "ym", "ydm", "mdy")) %>%
        as.Date(),
      Pid = dplyr::row_number()
    ) %>%
    dplyr::select(Pid, Title, Author, AuthorAddress, Date, Keyword, Summary)

  if (!is.null(output)) {
    writexl::write_xlsx(df, path = output)
    message("Cleaned data saved to ", output)
  }

  df
}