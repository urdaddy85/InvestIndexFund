library(dplyr)
library(RMySQL)
library(reshape2)
library(stringr)

# 参数初始化
getMaxDayData <- 29

# 连接数据库并读取数据
drv <- dbDriver("MySQL")
tushareDbConnect <- dbConnect(drv, username = "eric", password = "Eric19850417,./", host = "111.231.250.31", port = 3306, dbname = "Tushare")
dbSendQuery(tushareDbConnect, 'SET NAMES gbk')

dbData <- dbGetQuery(tushareDbConnect, "SELECT distinct ts_code, trade_date FROM index_daily")
dbData <- tbl_df(dbData)

targetTsCode <- c("399006.SZ", "399905.SZ", "000300.SH")

# df <- dbData %>% filter(ts_code == "399905.SZ")
# dayInterval <- getMaxDayData

pythonOurputTushareMinuteDataDateFormatAll <- function(df, dayInterval, targetTsCode){
  
  pythonOurputTushareMinuteDataDateFormat <- function(df, dayInterval){
    df <- df %>% mutate(trade_date = as.Date(trade_date))
    
    # 按照最大输出日分组
    rowN <- nrow(df)
    typeComplete <- trunc(rowN/dayInterval)
    type <- c(rep(1:typeComplete, each = dayInterval), rep(typeComplete + 1, rowN %% dayInterval))
    df$type <- type
    
    # 输出min max格式
    dfSum <- df %>%
      group_by(ts_code, type) %>%
      summarise(minDate = min(trade_date), maxDate = max(trade_date)) %>%
      ungroup()
    
    return(dfSum)
  }
  
  outputDf <- NULL
  for (tt in targetTsCode){
    dfSub <- df %>% filter(ts_code == tt)
    outputDf <- rbind(outputDf, pythonOurputTushareMinuteDataDateFormat(dfSub, dayInterval))
  }
  
  return(outputDf)
}

tushareMinuteDataOutputPlan <- pythonOurputTushareMinuteDataDateFormatAll(dbData, getMaxDayData, targetTsCode)
tushareMinuteDataOutputPlan <- tushareMinuteDataOutputPlan %>%
  mutate(maxDateReal = maxDate + 1) %>%
  mutate(minDate = str_replace_all(as.character(minDate), "-", "")) %>%
  mutate(maxDate = str_replace_all(as.character(maxDate), "-", "")) %>%
  mutate(maxDateReal = str_replace_all(as.character(maxDateReal), "-", ""))

write.csv(tushareMinuteDataOutputPlan, "X:\\本地数据\\输出数据\\分时数据\\导出计划.csv", row.names = F)

