#' Keyword Dumbbell Plot
#'
#' Visualises the temporal span (Q1, median, Q3) of the top N keywords.
#'
#' @param keyword_data Long-format data frame with columns Pid, Date, Keyword (from `process_keywords`).
#' @param start_year Integer, start year.
#' @param end_year Integer, end year.
#' @param top_n Number of top keywords to show.
#' @param output Optional PNG path.
#' @return A ggplot object.
#' @export
keydumbbell_plot <- function(keyword_data, start_year, end_year, top_n, output = NULL) {
  df <- keyword_data %>%
    dplyr::mutate(Year = as.integer(format(Date, "%Y"))) %>%
    dplyr::filter(Year >= start_year, Year <= end_year)

  top_kw <- df %>%
    dplyr::count(Keyword, sort = TRUE) %>%
    dplyr::slice_head(n = top_n)

  df_top <- df %>%
    dplyr::filter(Keyword %in% top_kw$Keyword)

  stats <- df_top %>%
    dplyr::group_by(Keyword) %>%
    dplyr::summarise(
      Q1 = floor(stats::quantile(Year, 0.25)),
      Median = floor(stats::median(Year)),
      Q3 = floor(stats::quantile(Year, 0.75)),
      Count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Q1)

  p <- ggplot(stats, aes(x = Q1, xend = Q3, y = factor(Keyword, levels = Keyword))) +
    geom_segment(color = "gray", linewidth = 1) +
    geom_point(aes(x = Q1), color = "#90EE90", size = 2) +
    geom_point(aes(x = Q3), color = "#8B0000", size = 2) +
    geom_point(aes(x = Median, size = Count/2), color = "#A3B9C4") +
    theme_minimal() +
    labs(
      x = "Year", y = "Keywords",
      title = paste("Keyword Trends (", start_year, "-", end_year, ")", sep = "")
    ) +
    theme(axis.text.y = element_text(size = 6))

  if (!is.null(output)) {
    ggplot2::ggsave(output, plot = p, width = 15, height = 18, units = "cm", dpi = 150)
  }

  p
}