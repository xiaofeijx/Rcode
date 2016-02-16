#==============
#传染病重复报卡检测程序
#0、需要安装的包为plyr,dplyr,tmcn
#1、安装tmcn包
#安装方法：  Tools ->Install packages -> install from ->package archives 
#进行手工安装
#2、将导出的卡片保存到我的文档下面，在将文件名改为reportd.csv,d是duplicate的含义
#传染病分类.csv也要放在我的文档下面
#3、在Rstudio里新建一个"R script"文件，将代码复制进去；
#   全选代码，点击Run(在代码区的右上角)
#4、结果将以文件的形式保存在我的文档下面
#5、df_name_** 表示先查姓名和疾病大类，在相同的情况下，分别查电话dh，现住址xzz和证件号码zj
#6、df_zj查找身份证和疾病大类相同的卡片
#7、df_dh查找电话和疾病大类相同的卡片
#8、查出的卡片只是疑似重卡，需要人工进一步核实。
#9、欢迎反馈：QQ7880774
#2016.2.16


#reading the data
df <- read.csv("Reportd.csv",stringsAsFactors = F)


#提取姓名，年龄，疾病名称等有需要的信息
showcol<- grep("卡片ID|患者姓名|疾病名称|年龄|报告卡录入时间|有效证件号|联系电话|现住地址国标|现住详细地址|^报告单位$|审核状态",names(df))
df2 <- df[,showcol]
rm(df)
#提取终审核卡片，和非其它类的疾病
df2 <- df2[(substring(df2$疾病名称,1,3)!="其它:" & substring(df2$疾病名称,1,4)!="其它疾病") & (df2$审核状态 == "已终审卡"),]
#dn <- data.frame(a=unique(df2$疾病名称))
#write.csv(dn,"dn.csv")

#疾病分类映射
alld <- read.csv("传染病分类.csv",stringsAsFactors = F)
library(dplyr)
df2 <- left_join(df2,alld,by=c("疾病名称"="疾病名称"))
detach(package:dplyr)

#根据姓名查重和疾病大类是否相同查重
#生成姓名的拼音格式
library(tmcn)
df2$name <- toPinyin(df2$患者姓名)

#定义查重函数，直接查重
#将每一个信息（如姓名）与全部姓名比对，相同的在select里定义为TRUE
finddu <- function(mydf,col){
  mydf$select <- FALSE
  col <- as.numeric(col)
  
  for (i in 1:nrow(mydf)) {
    pd <- (mydf[,col] == mydf[,col][i]) #判断
    mydf$select <- mydf$select | pd
    if (sum(pd) == 1)   mydf$select[i] <- FALSE  
}
return(mydf[mydf$select == TRUE,])
}


#判断姓名是否相同
#name在第13列
#查姓名
namedf <- finddu(df2,13)

#在姓名相同的情况下：查疾病大类
library(plyr)
namedf <- ddply(namedf,.(name),finddu,col=12)

#在疾病大类相同的情况下，查身份证号码是否相同
namedf_zj <- namedf[namedf$有效证件号 != "",]
namedf_zj <- ddply(namedf_zj,.(name),finddu,col=3)
namedf_zj <- ddply(namedf_zj,.(name),finddu,col=12)
write.csv(namedf_zj,"df_name_zj.csv")

#在疾病大类相同的情况下，查电话号码是否相同
namedf_dh <- namedf[namedf$联系电话 != "",]
#去掉前面已经查出来的卡片
namedf_dh <- namedf[!(namedf$卡片ID %in% namedf_zj$卡片ID),] 
namedf_dh <- ddply(namedf_dh,.(name),finddu,col=5)
namedf_dh <- ddply(namedf_dh,.(name),finddu,col=12)
write.csv(namedf_dh,"df_name_dh.csv")

#在疾病大类相同的情况下，查现住址编码是否相同
#去掉前面已经查出来的卡片
namedf_xzz <- namedf[!(namedf$卡片ID %in% namedf_zj$卡片ID) & !(namedf$卡片ID %in% namedf_dh$卡片ID),] 
namedf_xzz <- ddply(namedf_xzz,.(name),finddu,col=6)
namedf_xzz <- ddply(namedf_xzz,.(name),finddu,col=12)
write.csv(namedf_xzz,"df_name_xzz.csv")

#身份证号查重
#证件号在第3列
#先把前面查出来的重卡去掉
zjdf <- df2[!(df2$卡片ID %in% namedf_zj$卡片ID) & !(df2$卡片ID %in% namedf_dh$卡片ID) & !(df2$卡片ID %in% namedf_xzz$卡片ID),]
zjdf <- zjdf[zjdf$有效证件号 != "",] #有证件号的
#证件号是否相同
zjdf <- finddu(zjdf,3)
#疾病大类是否相同
zjdf <- ddply(zjdf,.(有效证件号),finddu,col=12)
write.csv(zjdf,"df_zj.csv")

#电话号码查重
#先把前面查出来的重卡去掉
dhdf <- df2[!(df2$卡片ID %in% namedf_zj$卡片ID) & !(df2$卡片ID %in% namedf_dh$卡片ID) & !(df2$卡片ID %in% namedf_xzz$卡片ID),]
dhdf <- dhdf[dhdf$联系电话 != "",] #去掉空的
#查电话号码是否相同
dhdf <-  finddu(dhdf,5)
#查疾病大类是否相同
dhdf <- ddply(dhdf,.(联系电话),finddu,col=12)
#查姓是否相同
dhdf$familyname <- substr(dhdf$患者姓名,1,1)
dhdf <- ddply(dhdf,.(联系电话),finddu,col=15)
write.csv(dhdf,"df_dh.csv")
