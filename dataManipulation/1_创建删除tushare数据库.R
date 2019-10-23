library(RMySQL)
library(dplyr)
library(stringr)
library(reshape2)
library(data.table)

source("S:\\stockDB.R") # 股票数据库
tushareDbConnect <- chooseStockDB("local")
source("F:\\DataAnalysis\\Invest\\dataManipulation\\referenceCode\\assitCode.R") # 辅助数据


# 创建数据库：CT createDB
# ---------------------------

## 创建"指数日交易"数据库：createDailyDB
createDailyDB <- function(dbCon){
  sqlCT <- "CREATE TABLE `index_daily`(
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键',
  `ts_code` VARCHAR(20) COMMENT '指数代码',
  `trade_date` date NOT NULL COMMENT '交易日期',
  `close` DECIMAL(10, 2)  DEFAULT NULL COMMENT '收盘点位',
  `open` DECIMAL(10, 2)  DEFAULT NULL COMMENT '开盘点位',
  `high` DECIMAL(10, 2)  DEFAULT NULL COMMENT '最高点位',
  `low` DECIMAL(10, 2)  DEFAULT NULL COMMENT '最低点位',
  `pre_close` DECIMAL(10, 2)  DEFAULT NULL COMMENT '昨日收盘点',
  `change` DECIMAL(10, 2)  DEFAULT NULL COMMENT '涨跌点',
  `pct_chg` DECIMAL(10, 2)  DEFAULT NULL COMMENT '涨跌幅(%)',
  `vol` DECIMAL(20, 2)  DEFAULT NULL COMMENT '成交量(手)',
  `amount` DECIMAL(20, 2)  DEFAULT NULL COMMENT '成交额(千元)',
  PRIMARY KEY (`id`))
  ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='指数日交易数据';
  "

  sqlCT <- paste(sqlCT, "", sep = "")

  dbGetQuery(dbCon, sqlCT)
}
# createDailyDB(tushareDbConnect)


## 创建"指数分时交易"数据库：CT createMinDb
createMinDB <- function(timingStr, dbCon){
  dbSendQuery(dbCon, 'SET NAMES gbk')
  for (s in timingStr){
    part1 <- "CREATE TABLE `index_daily_"
    part2 <- str_c(s, "`(")
    part3 <- "`id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键',
`ts_code` VARCHAR(20) COMMENT '指数代码',
`trade_time` VARCHAR(30) NOT NULL COMMENT '交易时间',
`trade_date` date NOT NULL COMMENT '交易日期',
`open` DECIMAL(10, 2)  DEFAULT NULL COMMENT '开盘点位',
`high` DECIMAL(10, 2)  DEFAULT NULL COMMENT '最高点位',
`low` DECIMAL(10, 2)  DEFAULT NULL COMMENT '最低点位',
`close` DECIMAL(10, 2)  DEFAULT NULL COMMENT '收盘点位',
`pre_close` DECIMAL(10, 2)  DEFAULT NULL COMMENT '昨日收盘点',
`vol` DECIMAL(20, 2)  DEFAULT NULL COMMENT '成交量(手)',
`amount` DECIMAL(20, 2)  DEFAULT NULL COMMENT '成交额(千元)',
PRIMARY KEY (`id`))
ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='指数分时交易数据';"
    
    sqlCT <- paste(part1, part2, part3, sep="") # 如果使用str_c, 则会造成乱码
    dbGetQuery(dbCon, sqlCT)
  }
}
# createMinDB(genMinSec(), tushareDbConnect)



# 删除数据库：dropDB
# ---------------------------

# 删除数据库表
dropMinDB <- function(timingStr, dbCon){
  for (s in timingStr){
    part1 <- "DROP TABLE `index_daily_"
    sqlCT <- str_c(part1, s, "`")
    dbGetQuery(dbCon, sqlCT)
  }
}
# dropMinDB(genMinSec(), tushareDbConnect)

