#查找重复行
#reading the data
df <- read.csv("Reportd.csv",stringsAsFactors = F)
#names(df)
#提取姓名，年龄，疾病名称

showcol<- grep("卡片ID|患者姓名|疾病名称|年龄|报告卡录入时间|有效证件号|联系电话|现住地址国标|现住详细地址|审核状态",names(df))
df2 <- df[,showcol]
rm(df)
df2 <- df2[(substring(df2$疾病名称,1,3)!="其它:" & substring(df2$疾病名称,1,4)!="其它疾病") & (df2$审核状态 == "已终审卡"),]
#dn <- data.frame(a=unique(df2$疾病名称))
#write.csv(dn,"dn.csv")

#疾病分类映射
alld <- read.csv("传染病分类.csv",stringsAsFactors = F)

library(dplyr)
df2 <- left_join(df2,alld,by=c("疾病名称"="疾病名称"))
head(df2)

detach(package:dplyr)

#根据姓名查重和疾病大类是否相同查重
#生成姓名的拼音格式
library(tmcn)
df2$name <- toPinyin(df2$患者姓名)

#定义查重函数，直接查重
finddu <- function(mydf,col){
  mydf$select <- FALSE
  col <- as.numeric(col)
  
  for (i in 1:nrow(mydf)) {
    pd <- (mydf[,col] == mydf[,col][i]) #判断
    mydf$select <- mydf$select | pd
    if (sum(pd) == 1)   mydf$select[i] <- FALSE  
}
return(mydf)
}


#判断姓名是否相同
#name在第12列
namedf <- finddu(df2,12)
namedf <- namedf[namedf$select == TRUE,]

#定义查重函数
#疾病名称查重
finddub <- function(mydf){
  mydf$select <- FALSE
  
  for (i in 1:nrow(mydf)) {
    pd <- (mydf$疾病大类 == mydf$疾病大类[i]) #判断
    mydf$select <- mydf$select | pd
    if (sum(pd) == 1)   mydf$select[i] <- FALSE  
  }
  return(mydf[mydf$select == TRUE,])
}

library(plyr)
namedf <- ddply(namedf,.(name),finddub)
write.csv(namedf,"df_name.csv")


#身份证号查重
#证件号在第3列
zjdf <- df2[!(df2$卡片ID %in% namedf$卡片ID),]
zjdf <- zjdf[zjdf$有效证件号 != "",] #有证件号的
zjdf <- finddu(zjdf,3)
zjdf <- zjdf[zjdf$select == TRUE,]

zjdf <- ddply(zjdf,.(有效证件号),finddub)
write.csv(zjdf,"df_zj.csv")

#电话号码查重
dhdf <- df2[!(df2$卡片ID %in% namedf$卡片ID),]
dhdf <- dhdf[!(dhdf$卡片ID %in% zjdf$卡片ID),]
dhdf <- dhdf[dhdf$联系电话 != "",] #去掉空的
dhdf <-  finddu(dhdf,5)

dhdf <- dhdf[dhdf$select == TRUE,]

dhdf <- ddply(dhdf,.(联系电话),finddub)
write.csv(dhdf,"df_dh.csv")




#姓名转拼音
library(tmcn)
df2$name <- toPinyin(df2$患者姓名)
str(df2)
#年龄划分
#age1 从0岁开始，6岁一个组
#age2 从-3岁开始，6岁一个组
df2$age <- ifelse(substr(df[,9],nchar(df[,9]),nchar(df[,9]))=="岁",
                 as.numeric(substr(df[,9],1,nchar(df[,9])-1)),
                 (ifelse(substr(df[,9],nchar(df[,9]),nchar(df[,9]))=="月",
                         as.numeric(substr(df[,9],1,nchar(df[,9])-1))/12,
                         as.numeric(substr(df[,9],1,nchar(df[,9])-1))/365)))

str(df)
breaks1 <- seq(0,108,by=6)
breaks2 <- seq(-3,105,by=6)

df2$age1 <- cut(df2$age,breaks1)
df2$age2 <- cut(df2$age,breaks2)

#疾病分类映射
alld <- read.csv("传染病分类.csv")

library(dplyr)
df3 <- left_join(df2,alld,by=c("疾病名称"="疾病名称"))

head(df3)

#age1
df4 <- df3[,c(5,7,9)]
head(df4)
head(arrange(dd,name))
dup1 <- df3[duplicated(df4),]




























#A sample data frame:
  df <- read.table(header=T, text='
                   label value
                   A     4
                   B     3
                   C     6
                   B     3
                   B     1
                   A     2
                   A     4
                   A     4
                   ')


# Is each row a repeat?
duplicated(df)
# FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE

# Show the repeat entries
df[duplicated(df),]
# label value
#     B     3
#     A     4
#     A     4

# Show unique repeat entries 
nn <- unique(df[duplicated(df),])
# label value
#     B     3
#     A     4
  

nn$dup <- 1
  
total <- merge(df,nn,all.x=T)

total[!is.na(total$dup),]

library(dplyr)
filter(total,dup == 1)
  
  
# Original data with repeats removed. These do the same:
unique(df)
df[!duplicated(df),]
# label value
#     A     4
#     B     3
#     C     6
#     B     1
#     A     2
cname <- read.table("cname.txt",header =F,stringsAsFactors = F)
  cnamelist <- strsplit(cname$V1,split="")
  str(cname)
  cnamedf <- as.data.frame(cnamelist)


  
  
  