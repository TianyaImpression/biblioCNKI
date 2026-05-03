#' Segment Chinese text using DeepSeek or compatible API
#'
#' This function sends Chinese text to a large language model (DeepSeek or local) and
#' asks it to tokenize the text into meaningful words, removing stopwords.
#'
#' @param text A character string of Chinese text.
#' @param api_key API key for the service. For local models, set to any non-empty string if not required.
#' @param base_url API endpoint URL. Default is DeepSeek's chat completions endpoint.
#' @param model Model name. Default `"deepseek-v4-flash"`.
#'
#' @return A list with components `words` (character vector) and `success` (logical).
#' @export
#'
#' @importFrom httr add_headers POST timeout status_code content
#' @importFrom jsonlite toJSON
segment_chinese_with_deepseek <- function(
    text,
    api_key,
    base_url = "https://api.deepseek.com/v1/chat/completions",
    model = "deepseek-v4-flash"
) {
  if (is.na(text) || nchar(trimws(text)) == 0) {
    return(list(words = character(0), success = FALSE, error = "Empty input"))
  }

  if (nchar(text) > 3000) text <- substr(text, 1, 3000)

  system_prompt <- paste0(
    "你是一个中文自然语言处理专家。请对下面的中文文本进行分词，",
    "只保留有意义的实词（名词、动词、形容词、专业术语），去除标点、数字和常见停用词。",
    "结果只输出逗号分隔的词语，不要有任何额外解释。"
  )

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = text)
    ),
    temperature = 0.1,
    max_tokens = 2000
  )

  tryCatch({
    resp <- httr::POST(
      url = base_url,
      httr::add_headers(
        "Authorization" = if (grepl("deepseek", base_url)) paste("Bearer", api_key) else api_key,
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::timeout(60)
    )
    if (httr::status_code(resp) == 200) {
      content <- httr::content(resp, as = "parsed")
      words <- strsplit(trimws(content$choices[[1]]$message$content), "[,， ]+")[[1]]
      words <- words[nchar(words) > 1]
      stopwords <- c("的", "了", "在", "和", "是", "与", "及", "或", "等",
                     "对", "该", "其", "将", "可以", "进行", "通过", "不同",
                     "研究", "分析", "结果", "表明", "显示", "主要")
      words <- words[!words %in% stopwords]
      return(list(words = unique(words), success = TRUE))
    } else {
      return(list(words = character(0), success = FALSE,
                  error = paste("HTTP", httr::status_code(resp))))
    }
  }, error = function(e) {
    list(words = character(0), success = FALSE, error = e$message)
  })
}