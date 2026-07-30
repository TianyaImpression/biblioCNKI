#' Build Standard API Request Headers
#'
#' Only adds Authorization header when an API key is provided
#' (local/custom models may not require one).
#'
#' @param api_key API key (optional for local models)
#' @return An httr add_headers object
#' @keywords internal
build_api_headers <- function(api_key = "") {
  if (!is.null(api_key) && nzchar(trimws(api_key))) {
    httr::add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    )
  } else {
    httr::add_headers(
      "Content-Type" = "application/json"
    )
  }
}

#' Get API Endpoint URL
#'
#' @param provider API provider type, "deepseek" or "custom"
#' @param custom_url Custom API URL (only used when provider = "custom")
#' @return API endpoint URL string
#' @keywords internal
get_api_url <- function(provider = "deepseek", custom_url = NULL) {
  if (provider == "deepseek") {
    "https://api.deepseek.com/v1/chat/completions"
  } else if (provider == "custom") {
    if (is.null(custom_url) || trimws(custom_url) == "") {
      stop("自定义 API 模式下必须提供 API 端点 URL")
    }
    url <- trimws(custom_url)
    if (!grepl("/chat/completions$", url)) {
      url <- paste0(url, "/v1/chat/completions")
    }
    url
  } else {
    stop("不支持的 API 提供商类型: ", provider)
  }
}

#' Unified AI API Call Function
#'
#' Supports DeepSeek and any OpenAI-compatible API (local LLMs, Ollama, etc.).
#' Local/custom models do not require an API key — pass an empty string.
#'
#' @param messages List of messages, each with role and content
#' @param api_key API key (required for DeepSeek, optional for custom)
#' @param provider API provider, "deepseek" or "custom"
#' @param custom_url Custom API endpoint URL (only when provider = "custom")
#' @param model Model name
#' @param max_tokens Maximum tokens in response
#' @param temperature Temperature parameter (0-2)
#' @param timeout_sec Timeout in seconds
#'
#' @return On success: the response text content. On failure: a string
#'   starting with "失败：" containing the error details.
#' @keywords internal
call_ai_api <- function(messages,
                        api_key = "",
                        provider = "deepseek",
                        custom_url = NULL,
                        model = NULL,
                        max_tokens = 8000,
                        temperature = 0.3,
                        timeout_sec = 30) {

  # DeepSeek mode requires an API key; custom/local models may not
  if (provider == "deepseek" && (is.null(api_key) || !nzchar(trimws(api_key)))) {
    stop("DeepSeek API 密钥不能为空")
  }

  api_url <- get_api_url(provider, custom_url)

  if (is.null(model) || !nzchar(trimws(model))) {
    model <- if (provider == "deepseek") "deepseek-v4-flash" else "gpt-3.5-turbo"
  }

  request_body <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens,
    temperature = temperature
  )

  headers <- build_api_headers(api_key)

  tryCatch({
    response <- httr::POST(
      url = api_url,
      headers,
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      httr::timeout(timeout_sec)
    )

    if (httr::status_code(response) == 200) {
      raw_text <- httr::content(response, as = "text", encoding = "UTF-8")
      content <- jsonlite::fromJSON(raw_text, simplifyVector = FALSE)

      if (is.null(content$choices) || length(content$choices) == 0) {
        err_msg <- if (!is.null(content$error$message)) {
          content$error$message
        } else {
          "API 响应中未找到 choices 字段"
        }
        return(paste("失败：", err_msg))
      }

      result <- content$choices[[1]]$message$content
      if (is.null(result)) {
        return("失败：API 响应中未找到 message.content 字段")
      }

      # Attach token usage as an attribute
      if (!is.null(content$usage)) {
        usage <- list(
          prompt_tokens     = content$usage$prompt_tokens     %||% 0,
          completion_tokens = content$usage$completion_tokens %||% 0,
          total_tokens      = content$usage$total_tokens      %||% 0
        )
        attr(result, "usage") <- usage
      }

      return(result)
    } else {
      error_body <- tryCatch(
        httr::content(response, as = "text", encoding = "UTF-8"),
        error = function(e) "无法解析错误响应"
      )
      return(paste("失败：API状态码", httr::status_code(response), "-", error_body))
    }
  }, error = function(e) {
    paste("失败：", as.character(e$message))
  })
}

#' Null coalescing operator
#' @param x Left value
#' @param y Right default value
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Segment Chinese Text Using AI API
#'
#' Sends Chinese text to an LLM (DeepSeek or local/custom) for tokenization.
#' Returns meaningful words after filtering stopwords and single-character tokens.
#'
#' @param text A character string of Chinese text.
#' @param api_key API key. For local models that don't need one, pass an empty string.
#' @param provider API provider. Either "deepseek" or "custom".
#' @param custom_url Custom API endpoint URL. Only used when provider = "custom".
#'   Auto-appends \verb{/v1/chat/completions} if not present.
#' @param model Model name. For DeepSeek, defaults to \code{"deepseek-v4-flash"}.
#'   For custom, user must specify.
#' @param max_tokens Maximum tokens for the response (default 1000).
#' @param timeout_sec HTTP timeout in seconds (default 30).
#'
#' @return A list with components:
#'   \item{words}{Character vector of extracted words}
#'   \item{success}{Logical indicating success}
#'   \item{error}{Error message if failed}
#'   \item{usage}{Token usage (if available)}
#' @export
segment_chinese_with_deepseek <- function(
    text,
    api_key = "",
    provider = "deepseek",
    custom_url = NULL,
    model = NULL,
    max_tokens = 1000,
    timeout_sec = 30
) {
  if (is.na(text) || is.null(text) || trimws(text) == "") {
    return(list(words = character(0), success = FALSE, error = "输入文本为空"))
  }

  if (nchar(text) > 2000) {
    text <- substr(text, 1, 2000)
  }

  system_prompt <- paste0(
    "你是一个专业的中文自然语言处理专家，擅长进行科学文献分析。",
    "请对以下中文文本进行分词处理。分词要求：",
    "1. 识别专业术语、科技术语和专有名词，保持其完整性；",
    "2. 去除常见的停用词（如'的'、'和'、'在'、'是'等）；",
    "3. 只保留有意义的名词、动词、形容词等实词；",
    "4. 过滤掉标点符号和数字，数学公式，百分数，年份等数学式；",
    "5. 每个词语之间用逗号分隔；",
    "请严格按照'词语1,词语2,词语3,...'的格式返回分词结果，不要添加任何解释或额外文本。"
  )

  messages <- list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = paste("请对以下文本进行分词：\n\n", text))
  )

  result <- call_ai_api(
    messages = messages,
    api_key = api_key,
    provider = provider,
    custom_url = custom_url,
    model = model,
    max_tokens = max_tokens,
    temperature = 0.1,
    timeout_sec = timeout_sec
  )

  usage_attr <- attr(result, "usage")

  if (is.character(result) && grepl("^失败", result)) {
    return(list(words = character(0), success = FALSE, error = result))
  }

  segmented_text <- gsub("\n|\\s+", "", result)

  words <- strsplit(segmented_text, ",")[[1]]
  words <- trimws(words[words != ""])
  words <- words[nchar(words) > 1]

  stopwords_custom <- c("的", "和", "与", "及", "在", "是", "了", "对", "于", "中",
                        "使用", "可以", "研究", "分析", "基于", "方法", "数据",
                        "结果", "表明", "显示", "我们", "他们", "它们", "这些",
                        "那些", "这个", "那个")
  words <- words[!words %in% stopwords_custom]

  list(words = words, success = TRUE, usage = usage_attr)
}
