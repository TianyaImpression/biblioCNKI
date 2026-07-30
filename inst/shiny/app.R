library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(leaflet)
library(htmlwidgets)

ui <- navbarPage(
  title = "biblioCNKI - 中文文献计量工具箱",
  theme = shinytheme("flatly"),
  
  # ---------- 1. 数据导入与清洗 ----------
  tabPanel("数据导入与清洗",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "上传知网Excel文件 (.xlsx)", accept = c(".xlsx")),
        numericInput("sheet", "Sheet 序号", 1, min = 1),
        checkboxInput("use_demo", "使用内置示例数据"),
        actionButton("clean", "开始清洗"),
        br(), br(),
        downloadButton("dl_clean", "下载清洗后数据"),
        br(), br(),
        actionButton("kw_extract", "提取关键词"),
        downloadButton("dl_kw", "下载关键词数据")
      ),
      mainPanel(
        h4("预览（清洗后数据前100行）"),
        DT::dataTableOutput("clean_table"),
        verbatimTextOutput("log")
      )
    )
  ),
  
  # ---------- 2. 发文趋势 ----------
  tabPanel("发文趋势",
    sidebarLayout(
      sidebarPanel(
        actionButton("trend_plot", "绘制累积发文图"),
        br(), br(),
        downloadButton("dl_trend", "下载图片")
      ),
      mainPanel(plotOutput("trendPlot"))
    )
  ),
  
  # ---------- 3. 关键词哑铃图 ----------
  tabPanel("关键词趋势",
    sidebarLayout(
      sidebarPanel(
        numericInput("start_year", "起始年", 2010),
        numericInput("end_year", "结束年", 2024),
        numericInput("top_n", "前N个关键词", 45),
        actionButton("dumb_plot", "生成哑铃图"),
        br(), br(),
        downloadButton("dl_dumb", "下载图片")
      ),
      mainPanel(plotOutput("dumbPlot", height = "700px"))
    )
  ),
  
  # ---------- 4. 机构热力图 ----------
  tabPanel("机构热力图",
    sidebarLayout(
      sidebarPanel(
        textInput("amap_key", "高德API Key"),
        textInput("geovis_token", "中科星图Token (可选)"),
        actionButton("geo_go", "开始地理编码并绘图"),
        br(), br(),
        downloadButton("dl_geo", "下载地理编码结果"),
        downloadButton("dl_heatmap", "下载热力图HTML")
      ),
      mainPanel(leafletOutput("heatmap", height = "600px"))
    )
  ),
  
  # ---------- 5. AI中文分词 ----------
  tabPanel("AI中文分词",
    sidebarLayout(
      sidebarPanel(
        radioButtons("api_mode", "API模式",
                     choices = c("DeepSeek (云端)" = "cloud", "本地/自定义" = "local"),
                     selected = "cloud"),
        conditionalPanel(
          condition = "input.api_mode == 'cloud'",
          textInput("ds_api_key", "DeepSeek API Key", value = "")
        ),
        conditionalPanel(
          condition = "input.api_mode == 'local'",
          textInput("local_base_url", "本地/自定义API地址",
                    value = "http://localhost:1234/v1/chat/completions"),
          textInput("local_api_key", "API Key (如模型无需密钥可留空)", value = "")
        ),
        textInput("ds_model", "模型名称（DeepSeek默认 deepseek-v4-flash，本地/自定义需填写）", value = "deepseek-v4-flash"),
        fileInput("segment_file", "上传补充文本Excel (可选，仅当未清洗或无清洗数据时使用)"),
        textAreaInput("user_text", "或直接输入中文文本（补充清洗数据）", rows = 6,
                      placeholder = "在此粘贴中文摘要、标题或关键词..."),
        actionButton("segment_btn", "开始分词"),
        br(), br(),
        downloadButton("dl_segment", "导出分词结果 (CSV)")
      ),
      mainPanel(
        h4("分词结果"),
        verbatimTextOutput("segment_result")
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    clean = NULL,
    kw    = NULL,
    geo   = NULL,
    seg_words = NULL,
    dumb_plot = NULL
  )
  
  # 消息通知辅助函数
  notify <- function(msg, type = "message") {
    showNotification(msg, type = type, duration = 5)
  }
  
  # ---------- 1. 清洗数据 ----------
  observeEvent(input$clean, {
    if (input$use_demo) {
      demo_path <- system.file("extdata", "ExampleData.xlsx", package = "biblioCNKI")
      if (demo_path == "") {
        notify("未找到内置示例数据，请确保 inst/extdata/ExampleData.xlsx 存在。", "error")
        return()
      }
      file_path <- demo_path
    } else {
      req(input$file)
      file_path <- input$file$datapath
    }
    
    tryCatch({
      rv$clean <- biblioCNKI::process_citation_data(file_path, sheet = input$sheet)
      notify("数据清洗完成！")
    }, error = function(e) {
      notify(paste("清洗失败:", e$message), "error")
    })
  })
  
  output$clean_table <- DT::renderDataTable({
    req(rv$clean)
    head(rv$clean, 100)
  }, options = list(scrollX = TRUE))
  
  output$log <- renderText({
    if (!is.null(rv$clean)) paste0("清洗得到 ", nrow(rv$clean), " 条有效记录")
  })
  
  output$dl_clean <- downloadHandler(
    filename = "cleaned_data.xlsx",
    content = function(file) {
      req(rv$clean)
      writexl::write_xlsx(rv$clean, file)
    }
  )
  
  # ---------- 2. 提取关键词 ----------
  observeEvent(input$kw_extract, {
    req(rv$clean)
    rv$kw <- biblioCNKI::process_keywords(rv$clean)
    notify(paste0("关键词提取完成，共 ", nrow(rv$kw), " 条"))
  })
  
  output$dl_kw <- downloadHandler(
    filename = "keywords.xlsx",
    content = function(file) {
      req(rv$kw)
      writexl::write_xlsx(rv$kw, file)
    }
  )
  
  # ---------- 3. 累积发文趋势 ----------
  trend_plot <- eventReactive(input$trend_plot, {
    req(rv$clean)
    biblioCNKI::cumulative_plot(rv$clean)
  })
  
  output$trendPlot <- renderPlot({ trend_plot() })
  
  output$dl_trend <- downloadHandler(
    filename = "cumulative.png",
    content = function(file) {
      ggsave(file, plot = trend_plot(), width = 11, height = 6, units = "cm", dpi = 200)
    }
  )
  
  # ---------- 4. 关键词哑铃图 ----------
  observeEvent(input$dumb_plot, {
    if (is.null(rv$kw)) {
      notify("请先在「数据导入与清洗」标签页点击「提取关键词」", "error")
      return()
    }
    tryCatch({
      rv$dumb_plot <- biblioCNKI::keydumbbell_plot(rv$kw, input$start_year, input$end_year, input$top_n)
      notify("哑铃图生成完成！")
    }, error = function(e) {
      notify(paste("哑铃图生成失败:", e$message), "error")
      rv$dumb_plot <- NULL
    })
  })

  output$dumbPlot <- renderPlot({
    req(rv$dumb_plot)
    rv$dumb_plot
  })
  
  output$dl_dumb <- downloadHandler(
    filename = "keyword_dumbbell.png",
    content = function(file) {
      req(rv$dumb_plot)
      ggsave(file, plot = rv$dumb_plot, width = 15, height = 18, units = "cm", dpi = 150)
    }
  )
  
  # ---------- 5. 热力图 ----------
  observeEvent(input$geo_go, {
    req(rv$clean, input$amap_key)
    withProgress(message = "地理编码中...", {
      rv$geo <- biblioCNKI::geocode_batch_processing(rv$clean, api_key = input$amap_key)
    })
    notify("地理编码完成，请查看热力图。")
  })
  
  output$heatmap <- renderLeaflet({
    req(rv$geo)
    gtoken <- if (nzchar(input$geovis_token)) input$geovis_token else NULL
    biblioCNKI::geoheatmap_plot(rv$geo, geovis_token = gtoken)
  })
  
  output$dl_geo <- downloadHandler(
    filename = "geocoded_addresses.xlsx",
    content = function(file) {
      req(rv$geo)
      openxlsx::write.xlsx(rv$geo, file)
    }
  )
  
  output$dl_heatmap <- downloadHandler(
    filename = "heatmap.html",
    content = function(file) {
      req(rv$geo)
      gtoken <- if (nzchar(input$geovis_token)) input$geovis_token else NULL
      m <- biblioCNKI::geoheatmap_plot(rv$geo, geovis_token = gtoken)
      htmlwidgets::saveWidget(m, file)
    }
  )
  
  # ---------- 6. AI中文分词 ----------
  observeEvent(input$segment_btn, {
    # 收集文本：优先使用清洗后的数据，再补充上传文件和手动输入
    text <- ""

    # 1) 从清洗后的数据中提取 Title 和 Summary
    if (!is.null(rv$clean)) {
      title_text <- if ("Title" %in% names(rv$clean)) {
        paste(rv$clean$Title, collapse = "\n\n")
      } else ""
      summary_text <- if ("Summary" %in% names(rv$clean)) {
        paste(rv$clean$Summary, collapse = "\n\n")
      } else ""
      text <- paste(title_text, summary_text, sep = "\n\n")
      notify(sprintf("已从清洗数据中提取 %d 条标题和摘要", nrow(rv$clean)), "message")
    }

    # 2) 上传的 Excel 文件（补充来源）
    if (!is.null(input$segment_file)) {
      df <- readxl::read_excel(input$segment_file$datapath)
      possible_cols <- intersect(c("Title", "Summary", "AB", "TI", "Abstract"), names(df))
      if (length(possible_cols) > 0) {
        file_text <- paste(df[[possible_cols[1]]], collapse = "\n\n")
        text <- paste(text, file_text, sep = "\n\n")
      } else {
        text <- paste(text, paste(df[[1]], collapse = "\n\n"), sep = "\n\n")
        notify("上传文件未检测到标准列名，已使用第一列内容补充", "warning")
      }
    }

    # 3) 手动输入的文本
    if (nzchar(input$user_text)) {
      text <- paste(text, input$user_text, sep = "\n\n")
    }

    if (nchar(trimws(text)) == 0) {
      notify("请先进行数据清洗，或上传文件，或输入文本", "error")
      return()
    }
    
    withProgress(message = "正在分词...", {
      provider <- if (input$api_mode == "cloud") "deepseek" else "custom"
      res <- biblioCNKI::segment_chinese_with_deepseek(
        text,
        api_key   = if (input$api_mode == "cloud") input$ds_api_key else input$local_api_key,
        provider  = provider,
        custom_url = if (input$api_mode == "local") input$local_base_url else NULL,
        model     = input$ds_model
      )
    })
    
    if (res$success) {
      rv$seg_words <- res$words
      output$segment_result <- renderText({
        paste(res$words, collapse = ", ")
      })
    } else {
      output$segment_result <- renderText({
        paste("分词失败：", res$error)
      })
      rv$seg_words <- NULL
    }
  })
  
  output$dl_segment <- downloadHandler(
    filename = function() paste0("word_segmentation_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$seg_words)
      write.csv(data.frame(word = rv$seg_words), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)