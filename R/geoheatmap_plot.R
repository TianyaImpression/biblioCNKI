#' Institutional Heatmap with Geovis Earth Basemap
#'
#' Generates an interactive heatmap of institution locations using Leaflet.
#' Requires Amap API Key for geocoding (passed via `geocode_batch_processing`)
#' and Geovis Earth Token for satellite basemap.
#'
#' @param geocoded_data Data frame from `geocode_batch_processing` with longitude, latitude.
#' @param geovis_token Geovis Earth Token for satellite and label tiles. If NULL, default OSM is used.
#' @param output Optional HTML file path to save map.
#' @return A leaflet map object.
#' @export
geoheatmap_plot <- function(geocoded_data, geovis_token = NULL, output = NULL) {
  valid <- geocoded_data %>%
    dplyr::filter(!is.na(longitude) & !is.na(latitude))

  map <- leaflet::leaflet(valid)

  if (!is.null(geovis_token) && nzchar(geovis_token)) {
    satellite_url <- paste0(
      "https://tiles1.geovisearth.com/base/v1/img/{z}/{x}/{y}?format=webp&tmsIds=w&token=",
      geovis_token
    )
    label_url <- paste0(
      "https://tiles1.geovisearth.com/base/v1/cia/{z}/{x}/{y}?format=webp&tmsIds=w&token=",
      geovis_token
    )
    map <- map %>%
      leaflet::addTiles(urlTemplate = satellite_url) %>%
      leaflet::addTiles(urlTemplate = label_url)
  } else {
    map <- map %>% leaflet::addTiles()
  }

  map <- map %>%
    leaflet.extras::addHeatmap(
      lng = ~longitude, lat = ~latitude,
      radius = 15, blur = 20, max = 0.8,
      gradient = c("blue", "cyan", "yellow", "red"),
      cellSize = 10, minOpacity = 0.5
    )

  if (!is.null(output)) {
    htmlwidgets::saveWidget(map, file = output)
  }

  map
}