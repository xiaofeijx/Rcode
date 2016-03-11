#身份证校验程序

df <- read.csv("Report.csv",stringsAsFactors= F, na.strings = "")

#经对已删除卡进行校验，如果需要对全部卡进行校验，请在下面的语句前加上#
df <- df[df$审核状态 != "已删除卡",]
df$zj <-df$有效证件号
df$zj <- gsub("'","",df$zj)
df$zjf <- substring(df$zj,1,17)
df$zjl <- substring(df$zj,18,18)


#7. 9 .10 .5. 8. 4. 2. 1. 6. 3. 7. 9. 10. 5. 8. 4. 2. 
df$zjjy <- (as.numeric(substring(df$zjf,1,1))*7+
  as.numeric(substring(df$zjf,2,2))*9+
  as.numeric(substring(df$zjf,3,3))*10+
  as.numeric(substring(df$zjf,4,4))*5+
  as.numeric(substring(df$zjf,5,5))*8+  
  as.numeric(substring(df$zjf,6,6))*4+
  as.numeric(substring(df$zjf,7,7))*2+
  as.numeric(substring(df$zjf,8,8))+
  as.numeric(substring(df$zjf,9,9))*6+
  as.numeric(substring(df$zjf,10,10))*3+
  as.numeric(substring(df$zjf,11,11))*7+
  as.numeric(substring(df$zjf,12,12))*9+
  as.numeric(substring(df$zjf,13,13))*10+
  as.numeric(substring(df$zjf,14,14))*5+
  as.numeric(substring(df$zjf,15,15))*8+
  as.numeric(substring(df$zjf,16,16))*4+
  as.numeric(substring(df$zjf,17,17))*2) %% 11


for(i in 1:nrow(df)){
  if ( is.na(df$zjjy[i])) {
    df$zjjym[i] <- NA
  } else if (df$zjjy[i] == 0) {
    df$zjjym[i] <- 1
  } else if (df$zjjy[i] == 1) {
    df$zjjym[i] <- 0
  } else if (df$zjjy[i] == 2) {
    df$zjjym[i] <- "x"
  } else if (df$zjjy[i] == 3) {
    df$zjjym[i] <- 9
  } else if (df$zjjy[i] == 4) {
    df$zjjym[i] <- 8
  } else if (df$zjjy[i] == 5) {
    df$zjjym[i] <- 7
  } else if (df$zjjy[i] == 6) {
    df$zjjym[i] <- 6
  } else if (df$zjjy[i] == 7) {
    df$zjjym[i] <- 5
  } else if (df$zjjy[i] == 8) {
    df$zjjym[i] <- 4
  } else if (df$zjjy[i] == 9) {
    df$zjjym[i] <- 3
  } else if (df$zjjy[i] == 10) {
    df$zjjym[i] <- 2
  } 
}

df$select <- (df$zjl == df$zjjym)
write.csv(df[df$select == FALSE | is.na(df$select),],"证件号码校验.csv")
write.csv(df,"全部卡片校验.csv")
