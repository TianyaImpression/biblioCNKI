# biblioCNKI

**biblioCNKI** 是一个用于中国知网（CNKI）引文文献计量分析的 R 包。它整合了数据清洗、关键词提取、发文趋势绘图、机构地理编码与热力图、关键词时间哑铃图以及 DeepSeek 智能分析等完整流程，并提供一个 Shiny 图形界面，无需编写代码即可完成分析。

## 功能

- 自动清洗知网导出的 Excel 原始数据
- 按 `;;` 分隔符拆分关键词，整理为标准长表
- 生成累积发文趋势折线图
- 绘制关键词出现年份分布哑铃图（含 Q1/Median/Q3 及频次）
- 调用高德 API 批量地理编码发文机构地址，生成交互式热力图（支持中科星图卫星底图）
- 集成 DeepSeek API（支持 `deepseek-v4-flash` / `deepseek-v4-pro`），实现摘要智能分析
- 所有分析结果均可一键下载

## 安装

```r
# 安装依赖包
install.packages(c("shiny", "shinythemes", "DT", "readxl", "dplyr", "tidyr",
                   "ggplot2", "stringr", "lubridate", "writexl", "openxlsx",
                   "httr", "jsonlite", "leaflet", "leaflet.extras", "htmlwidgets",
                   "magrittr"))

# 方法一：从 GitHub 安装（推荐）
install.packages("devtools")
devtools::install_github("TianyaImpression/biblioCNKI")

# 方法二：从本地源码安装（需先下载或克隆仓库）
# devtools::install("path/to/biblioCNKI")
```

## 启动 biblioCNKI Shiny APP

```r
library(biblioCNKI)
launch_shiny()
```

## 参考文献

张凯, 徐洋, 陈晓娜, 等. 基于文献计量的生态遥感中国知网引文数据集（2010–2024）[J/OL]. 中国科学数据(中英文网络版), 1-10[2026-07-30]. https://link.cnki.net/urlid/11.6035.N.20260611.1432.004.