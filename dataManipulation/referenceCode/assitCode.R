# 生成时点数据
genMinSec <- function(){
  completeSingle <- function(str){
    str <- as.character(str)
    return(ifelse(str_length(str) == 1, str_c("0", str), str))
  }
  
  hourData <- completeSingle(c(9:11, 13:15))
  
  min9 <- completeSingle(c(30:59))
  n9 <- length(min9)
  
  min10 <- completeSingle(c(0:59))
  n10 <- length(min10)
  
  min11 <- completeSingle(c(0:30))
  n11 <- length(min11)
  
  min13 <- completeSingle(c(1:59))
  n13 <- length(min13)
  
  min14 <- completeSingle(c(0:59))
  n14 <- length(min14)
  
  min15 <- completeSingle(c(0))
  n15 <- length(min15)
  
  return(str_c(rep(hourData, c(n9, n10, n11, n13, n14, n15)), c(min9, min10, min11, min13, min14, min15)))
}


# 时点处理函数
getMinSec <- function(str){
  str <- str_c(substring(str, 12, 13), substring(str, 15, 16))
  return(str)
}