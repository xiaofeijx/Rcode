#传染病报卡查重程序
#读入所有报告数据
library(readr)
library(readxl)
library(tidyverse)
df <- read_csv("C:\\Users\\fxf\\Documents\\报告卡2023-12-20+14_21_45.csv",
               col_select = c("患者姓名","有效证件号","疾病名称","报告卡录入时间","审核状态","现住地址国标"),
               locale=locale(encoding="GBK"))
#删除已删除卡片
df <- df %>% filter(审核状态 != "已删除卡")

dfclass <- read_xls("C:\\Users\\fxf\\Documents\\传染病分类.xls")

#传染病病种与大类的映射数据
diseasemap <- dfclass %>% select(名称,疾病大类)

#两个数据之间疾病名称与疾病大类的映射，合并患者姓名和有效身份证件号码
df <- df %>% left_join(diseasemap,by=join_by(疾病名称==名称)) %>% 
  mutate(name_code= paste0(患者姓名,有效证件号))

#按照姓名+身份证号和疾病大类统计出现次数，并且筛选出现大于1次的数据
count_df <- df %>% group_by(name_code,疾病大类) %>% summarise(num=n()) %>% filter(num>=2)

#查看有重复报卡的病种
unique(count_df$疾病大类[!is.na(count_df$疾病大类)])

#在原始卡片中选择报卡次数大于1次的报卡
cfdataall <- df[which(df$name_code %in% count_df$name_code),] %>% 
  mutate(area=str_sub(现住地址国标,1,6))

# cfdataall <- df %>% 
#   slice(which(df$name_code %in% count_df$name_code)) %>% 
#   mutate(area=str_sub(现住地址国标,1,6))

#筛选查重时间少于等于180天的病种
checkdisease <- unique(dfclass$疾病大类[dfclass$重卡天数 <=180 &!is.na(dfclass$重卡天数) & dfclass$疾病大类 %in% unique(count_df$疾病大类[!is.na(count_df$疾病大类)])])


#开始查重
#按照需要查重的病种筛选数据，一个一个病种根据时间进行判断
for (diseasename in checkdisease){
  checkdf <- cfdataall %>% filter(疾病大类 == diseasename) %>% arrange(name_code,报告卡录入时间)
  tianshu <- unique(dfclass$重卡天数[dfclass$疾病大类==diseasename])
  
  for (i in 1:nrow(checkdf)){
    if(i==1){
      basename <- checkdf$name_code[1]
      basediadate  <- date(checkdf$报告卡录入时间[1])
      checkdf$cf <- FALSE
      
    } else {
      if(checkdf$name_code[i] == checkdf$name_code[i-1]){
        if( (date(checkdf$报告卡录入时间[i]) - date(checkdf$报告卡录入时间[i-1])) <= tianshu) {
          checkdf$cf[i]<- TRUE
        } else {
          basename <-  checkdf$name_code[i]
          basediadate<-  date(checkdf$报告卡录入时间[i])
        }
      } else {
        basename <-  checkdf$name_code[i]
        basediadate<-  date(checkdf$报告卡录入时间[i])
      }
    }
    
  }  
  write.csv(checkdf,paste0(diseasename,".csv"))
}
