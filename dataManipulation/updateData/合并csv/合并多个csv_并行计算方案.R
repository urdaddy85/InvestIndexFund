library(doParallel)      #加载doParallel包用于之后注册进程
library(foreach)  
library(dplyr)
library(stringr)
library(data.table)

start_time <- Sys.time()
# 读取数据
setwd("F:\\当前项目\\当前个人项目\\quant\\分时数据")
fileName <- dir()
fileName <- fileName[fileName!="导出计划.csv"]


# 并行计算
cl<- makeCluster(detectCores())      
registerDoParallel(cl)       #进行进程注册

code = function(fn){
  temp <- fread(fn)
  if (nrow(temp) > 0){
    temp$info <- fn
    return(temp)
  }
}
 
myData <- foreach(i = 1:length(fileName), .export = 'fread', .combine = 'rbind') %dopar% code(fileName[i])
stopCluster(cl)

usedTime <- Sys.time() - start_time
usedTime