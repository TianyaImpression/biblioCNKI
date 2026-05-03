#' Geocode Addresses via Amap API
#'
#' Splits multiple author addresses (separated by `;`), geocodes each unique address
#' using Amap (Gaode) API, and returns a data frame with longitude & latitude.
#'
#' @param cleaned_data Data frame from `process_citation_data`.
#' @param api_key Your Amap Web API key.
#' @param output Optional output path for Excel.
#' @return Data frame with Pid, AuthorAddress, Date, longitude, latitude.
#' @export
geocode_batch_processing <- function(cleaned_data, api_key, output = NULL) {
  standardize_address <- function(addr) {
    clean <- addr %>%
      gsub("[[:punct:]]", "", .) %>%
      trimws() %>%
      gsub("\\s+", "", .)
    rules <- list(
      "新疆" = "新疆维吾尔自治区", "南京" = "江苏省南京市", "江苏" = "江苏省",
      "北京" = "北京市", "吉林" = "吉林省", "宁夏" = "宁夏回族自治区",
      "陕西" = "陕西省", "浙江" = "浙江省"
    )
    if (!grepl("省|市|自治区|特别行政区", clean)) {
      for (key in names(rules)) {
        if (grepl(key, clean)) {
          clean <- paste0(rules[[key]], clean)
          break
        }
      }
    }
    clean
  }

  geocode_gaode <- function(address, key, max_retries = 5, timeout = 15) {
    base_url <- "https://restapi.amap.com/v3/geocode/geo"
    variants <- c(
      standardize_address(address),
      paste0(address, "正门"),
      gsub("学院$", "大学", address),
      address
    )
    for (addr in unique(variants)) {
      for (i in seq_len(max_retries)) {
        resp <- tryCatch(
          httr::GET(base_url, query = list(key = key, address = addr), httr::timeout(timeout)),
          error = function(e) NULL
        )
        Sys.sleep(0.3 * (2^(i-1)))
        if (!is.null(resp) && resp$status_code == 200) {
          content <- httr::content(resp, as = "parsed")
          if (content$status == "1" && length(content$geocodes) > 0) {
            loc <- content$geocodes[[1]]$location
            return(as.numeric(strsplit(loc, ",")[[1]]))
          }
        }
      }
    }
    c(NA, NA)
  }

  addr_df <- cleaned_data %>%
    dplyr::select(Pid, AuthorAddress, Date) %>%
    tidyr::separate_rows(AuthorAddress, sep = ";") %>%
    dplyr::mutate(AuthorAddress = trimws(AuthorAddress)) %>%
    dplyr::filter(AuthorAddress != "")

  pb <- utils::txtProgressBar(max = nrow(addr_df), style = 3)
  coords <- vector("list", nrow(addr_df))
  for (i in seq_len(nrow(addr_df))) {
    coords[[i]] <- geocode_gaode(addr_df$AuthorAddress[i], api_key)
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  result <- addr_df %>%
    dplyr::mutate(
      longitude = sapply(coords, `[`, 1),
      latitude  = sapply(coords, `[`, 2)
    )

  if (!is.null(output)) {
    openxlsx::write.xlsx(result, output)
    message("Geocoding result saved to ", output)
  }

  result
}