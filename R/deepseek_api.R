#' DeepSeek Chat Completion (OpenAI Compatible)
#'
#' Calls DeepSeek's API using the standard `/v1/chat/completions` endpoint.
#' Default model is `deepseek-v4-flash`.
#'
#' @param prompt The user message.
#' @param api_key Your DeepSeek API key.
#' @param model Model name, defaults to "deepseek-v4-flash".
#' @param temperature Sampling temperature (0-2).
#' @return A character string with the assistant's reply.
#' @export
deepseek_chat <- function(prompt, api_key, model = "deepseek-v4-flash", temperature = 0.7) {
  url <- "https://api.deepseek.com/v1/chat/completions"
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = "You are a helpful assistant for bibliometric analysis."),
      list(role = "user", content = prompt)
    ),
    temperature = temperature
  )

  resp <- httr::POST(
    url,
    httr::add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::timeout(60)
  )

  if (httr::status_code(resp) == 200) {
    content <- httr::content(resp, as = "parsed")
    return(content$choices[[1]]$message$content)
  } else {
    stop("DeepSeek API error: ", httr::content(resp, as = "text"))
  }
}