#' Cumulative Article Count Plot
#'
#' Creates a line plot of cumulative number of articles per year.
#'
#' @param cleaned_data Data frame with a `Date` column.
#' @param output Optional file path to save PNG.
#' @return A ggplot object.
#' @export
cumulative_plot <- function(cleaned_data, output = NULL) {
  years <- as.integer(format(cleaned_data$Date, "%Y"))
  year_range <- range(years, na.rm = TRUE)
  year_seq <- seq(year_range[1], year_range[2])
  yr_count <- as.data.frame(table(Year = years), stringsAsFactors = FALSE)
  yr_count$Year <- as.integer(as.character(yr_count$Year))
  df <- merge(data.frame(Year = year_seq), yr_count, by = "Year", all.x = TRUE)
  df$Freq[is.na(df$Freq)] <- 0
  df$Cumulative <- cumsum(df$Freq)

  p <- ggplot(df, aes(x = Year, y = Cumulative)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    scale_x_continuous(breaks = year_seq) +
    labs(
      title = paste("Cumulative Articles from", year_range[1], "to", year_range[2]),
      x = "Year", y = "Cumulative Count"
    ) +
    theme_minimal()

  if (!is.null(output)) {
    ggplot2::ggsave(output, plot = p, width = 11, height = 6, units = "cm", dpi = 200)
  }

  p
}