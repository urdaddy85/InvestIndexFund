library(RMySQL)
library(dplyr)
library(stringr)
library(lubridate)
library(reshape2)
library(zoo)


# ===========================
# 1. 获取数据
# ===========================

# 连接数据库
drv <- dbDriver("MySQL")
tushareDbConnect <- dbConnect(drv, username = "eric", password = "Eric19850417,./", host = "111.231.250.31", port = 3306, dbname = "Tushare")
dbSendQuery(tushareDbConnect, 'SET NAMES gbk')
myDataOrigin <- tbl_df(dbGetQuery(tushareDbConnect, 'SELECT * FROM index_daily'))

# 读入目标数据，初始化参数
indexDb <- tbl_df(read.csv("X:\\本地数据\\目标股票代码.csv", header = T, stringsAsFactors = F))
targetIndex <- c("399006.SZ", "399905.SZ", "000300.SH") #*
targetDb <- indexDb %>%
  filter(stockCode %in% targetIndex) %>%
  select(stockCode, stockNameAbrr)
targetIndexAbrr <- targetDb$stockNameAbrr #*
indexInterval <- c(10) #*
maInterval <- c(5) #*
standIndex <- c("close", "open", "pre_close") #*

withDataOrder <- c("trade_date", str_c(rep(targetIndexAbrr, times = length(standIndex) + length(indexInterval) + length(maInterval)), "_", rep(c(standIndex, str_c("chg_pct_", indexInterval), str_c("ma_", maInterval)), each = length(targetIndexAbrr))))


# ===========================
# 2. 预处理
# ===========================

# 清洗
myDataTarget <- myDataOrigin %>%
  filter(ts_code %in% targetIndex) %>%
  select(ts_code, trade_date, close, open, pre_close)

# 重命名ts_code
code2cname <- targetIndexAbrr
names(code2cname) <- targetIndex
myDataTarget <- myDataTarget %>% mutate(tsNameAbrr = code2cname[ts_code]) 

# trade_date转日期格式
myDataTarget <- myDataTarget %>% mutate(trade_date = as.Date(trade_date))


# ===========================
# 3. 策略处理
# ===========================

# 添加策略基础数据

addStrategyBasicDataBatch <- function(df, targetIndexAbrr, indexInterval, maInterval, standIndex, withDataOrder){
  
  addStrategyBasicData <- function(df, stockNameAbrr, indexInterval, standIndex){
    temp <- df[, c("trade_date", standIndex)]
    
    # 添加策略数据
    
    ## 添加绝对值之后数据
    for (i in indexInterval){
      interval <- i - 1
      indexIntervalName <- str_c("chg_pct_", i)
      lagClose <- temp$close/lag(temp$close, interval) - 1
      temp[, indexIntervalName] <- ifelse(is.na(lagClose), 0, lagClose)
    }
    
    ## 添加移动平均数据
    for (j in maInterval){
      interval <- j - 1
      maIntervalName <- str_c("ma_", j)
      lagClose <- rollmean(c(lag(temp$close, interval), temp$close[(length(temp$close)- interval + 1):length(temp$close)]), j)
      temp[, maIntervalName] <- ifelse(is.na(lagClose), 0, lagClose)
    }
    
    
    # 添加指数名
    names(temp)[2:length(temp)] <- str_c(stockNameAbrr, "_", names(temp)[2:length(temp)])
    
    return(temp)
  }
  
  outputDf <- NULL
  for (tia in targetIndexAbrr){
    tempDf <- df %>% filter(tsNameAbrr == tia)
    stockNameAbrr <- tia
    outputDf <- rbind(outputDf, melt(addStrategyBasicData(tempDf, stockNameAbrr, indexInterval, standIndex), id = "trade_date"))
  }

  outputDf <- dcast(outputDf, trade_date ~ variable, sum, value.var = "value")
  outputDf <- outputDf[, withDataOrder]
  
  return(outputDf)
}

myDataTargetAddStrategyBasicData <- tbl_df(addStrategyBasicDataBatch(myDataTarget, targetIndexAbrr, indexInterval, maInterval, standIndex, withDataOrder))


# 添加策略
df <- myDataTargetAddStrategyBasicData
tradeDate <- "2008-07-12"

addStrategy <- function(df, targetIndexAbrr, standIndex, tradeDate = "1990-01-01", strategy = "N"){
  # 日期限制
  df <- df %>% filter(trade_date >= as.Date(tradeDate))
  tia <- length(targetIndexAbrr)
  si <- length(standIndex)
  rowN <- nrow(df)
  
  # 添加策略
  # -------------------------
  # N策略：
  # 买入条件：近N个交易日涨幅排名第一
  # 卖出条件：近N个交易日涨幅排名非第一
  # -------------------------
  if (strategy = "N"){
    
    # getWantedCol：名称筛选函数
    getWantedCol <- function(df, str){
      validCol <- str_detect(names(df), str)
      validCol[1] <- TRUE
      
      if (str == "close"){
        validColPreClose <- str_detect(names(df), "pre_close")
        validCol[which(validCol&validColPreClose)] <- FALSE
      }
      return(validCol)
    }
    
    # 变化率df
    tempChgPct <- df[, getWantedCol(df, "chg_pct")]
    colNum <- length(tempChgPct)
    
    # 收盘价/开盘价df
    tempClose <- df[, getWantedCol(df, "close")]
    tempOpen <- df[, getWantedCol(df, "open")]
    
    # 添加最强指数/决策列
    tempChgPct$bestIndex <- "NULL"
    tempChgPct$decision <- "NULL"
    tempChgPct$buyPoint <- 0
    tempChgPct$sellPoint <- 0
    tempChgPct$profitPct <- 0
    
    # 添加策略
    for (i in 1:rowN){
      maxTargetIndexValue <- max(unlist(tempChgPct[i, 2:colNum]))
      maxTargetIndex <- names(which(unlist(tempChgPct[i, 2:colNum]) == maxTargetIndexValue))
      maxTargetIndex <- substring(maxTargetIndex, 1, str_locate(maxTargetIndex, "_")[1]-1)
      if (i == 1){
        if (maxTargetIndex <= 0){
          tempChgPct[i, "bestIndex"] <- "money"
        }else{
          tempChgPct[i, "bestIndex"] <- maxTargetIndex
        }
        
        if (empChgPct[i, "bestIndex"] != "money"){
          tempChgPct[i, "decision"] <- "买入"
        }else{
          tempChgPct[i, "decision"] <- "保持"
        }
        
        tempChgPct[i, "buyPoint"] <- 0
        tempChgPct[i, "sellPoint"] <- 0
        tempChgPct[i, "profitPct"] <- 0
      }
      if (i > 1){
        
      }
    
    }
  }
}






