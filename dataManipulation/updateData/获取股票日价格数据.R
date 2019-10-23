library(dplyr)
library(Tushare)
library(reshape2)
library(stringr)
library(RMySQL)
library(lubridate)

#----------------------------
# getStockData：批量输出指数/股票日数据
#----------------------------
stockInfo <- read.csv("F:\\weiyunSync\\weiyunSync\\DataSource\\tushare\\localdata\\目标股票代码.csv", header = T, stringsAsFactors = F)

getStockData <- function(stockInfo, dateFrom = "1900-01-01", dateTo = "2099-12-31"){
  # 默认是从目标代码数据获取数据
  stockInfo <- stockInfo
  
  # 日期格式和转换
  d2d <- function(dateStr, type){
    if (type == "-"){
      dateStr <- str_c(substring(dateStr, 1, 4), substring(dateStr, 6, 7), substring(dateStr, 9, 10))
    }
    else{
      dateStr <- str_c(substring(dateStr, 1, 4), "-", substring(dateStr, 5, 6), "-", substring(dateStr, 7, 8))
    }
    
    return(dateStr)
  }
  
  dateFrom <- d2d(dateFrom, "-")
  dateTo <- d2d(dateTo, "-")
  
  # 获取单只股票数据
  getOneStockData <- function(stockName, stockCode, dateFrom, dateTo, stockType){
    api <- pro_api(token = '6aa4ece07d628c8f5e80add7f40e4afefd954a0a3031d0f3d71b6966')
    if (stockType == "index") apiName = "index_daily"
    if (stockType == "stock") apiName = "daily"
    
    TEMP = api(api_name = apiName, ts_code = stockCode, start_date = dateFrom, end_date = dateTo)
    
    TEMP <- TEMP %>%
      mutate(stockType = stockType, stockName = stockName) %>%
      select(stockName, everything()) %>%
      mutate(trade_date = d2d(trade_date, ""))
    
    return(TEMP)
  }
  
  
  # 导出所有目标股票数据
  outputDf <- NULL
  for (i in 1:nrow(stockInfo)){
    stockName <- stockInfo$stockName[i]
    stockCode <- stockInfo$stockCode[i]
    stockType <- stockInfo$stockType[i]
    
    tempDf <- getOneStockData(stockName, stockCode, dateFrom, dateTo, stockType)
    outputDf <- rbind(outputDf, tempDf)
  }
  
  outputDf <- outputDf %>% arrange(trade_date)
  return(outputDf)
}


#----------------------------
# updateDate：比较本地数据库, 输出更新的数据
#----------------------------
source("S:\\stockDB.R") # 股票数据库
tushareDbConnect <- chooseStockDB("local")

updateIndexData <- function(df, conn){

  dbData <- dbGetQuery(conn, "SELECT CONCAT(ts_code, '_', trade_date) AS TEMP_id FROM index_daily")
  
  # 比较数据
  df <- df %>%
    mutate(TEMP_id = str_c(ts_code, "_", trade_date)) %>%
    filter(!(TEMP_id %in% dbData$TEMP_id)) %>%
    select(-TEMP_id)
  
  return(df)
}

outputDf <- updateIndexData(getStockData(stockInfo, as.character(today()), as.character(today()))) %>%
  filter(stockType == "index") %>%
  select(-stockName, -stockType) %>%
  unique()

write.csv(outputDf, str_c("X:\\本地数据\\输出数据\\", "特定股票日数据(", as.character(today()), ").csv"), row.names = F)



