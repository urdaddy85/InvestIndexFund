library(dplyr)
library(stringr)
library(data.table)

start_time <- Sys.time()

# 读取数据
setwd("F:\\当前项目\\当前个人项目\\quant\\分时数据")
fileName <- dir()
fileName <- fileName[fileName!="导出计划.csv"]

myData <- NULL
for (fn in fileName){
  temp <- fread(fn)
  if (nrow(temp) > 0){
    temp$info <- fn
    myData <- rbind(myData, temp)
  }
}

usedTime <- Sys.time() - start_time
usedTime

getMinSec <- function(str){
  str <- str_c(substring(str, 12, 13), substring(str, 15, 16))
  return(str)
}

myData <- myData %>% mutate(minSec = getMinSec(trade_time))
