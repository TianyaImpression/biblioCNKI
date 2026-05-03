#' Launch the biblioCNKI Shiny Application
#'
#' Opens the interactive Shiny interface for bibliometric analysis.
#' @export
launch_shiny <- function() {
  app_path <- system.file("shiny", "app.R", package = "biblioCNKI")
  if (app_path == "") {
    stop("Shiny app not found. Please reinstall the package.")
  }
  shiny::runApp(app_path)
}