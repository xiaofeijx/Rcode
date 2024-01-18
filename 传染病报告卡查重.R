#传染病报卡查重程序
#操作步骤
#1、导出传染病报告卡，读入数据；
#2、读入"传染病分类"数据
#3、运行所有代码
#4、重复报卡按照传染病分类保存在默认工作目录

#确保安装了readxl,tidyverse,pinyin三个包
library(readxl)
library(tidyverse)
library(pinyin)

#读入所有报告卡数据
df <- read_csv("C:\\Users\\fxf\\Documents\\报告卡(1).csv",
               col_select = c("患者姓名","有效证件号","疾病名称","报告卡录入时间","审核状态","现住地址国标"),
               locale=locale(encoding="GBK"))
#删除已删除卡片
df <- df %>% filter(审核状态 != "已删除卡")

dfclass <- read_xls("C:\\Users\\fxf\\Documents\\传染病分类.xls")

#传染病病种与大类的映射数据
diseasemap <- dfclass %>% select(名称,疾病大类)

#两个数据之间疾病名称与疾病大类的映射，合并患者姓名和有效身份证件号码
df <- df %>% left_join(diseasemap,by=join_by(疾病名称==名称))

#按照身份证号和疾病大类统计出现次数，并且筛选出现大于1次的数据
count_df <- df %>% group_by(有效证件号,疾病大类) %>% summarise(num=n()) %>% filter(num>=2)
#count_df <- df[duplicated(df$有效证件号),]

#查看有重复报卡的病种
unique(count_df$疾病大类[!is.na(count_df$疾病大类)])

#在原始卡片中选择报卡次数大于1次的报卡,同时提取现住址国标
cfdataall <- df[which(df$有效证件号 %in% count_df$有效证件号),] %>% 
  mutate(area=str_sub(现住地址国标,1,6))
unique(cfdataall$area)
cfdataall$area <- factor(cfdataall$area,
                         levels =c("330402","330411","330481","330482","330424","330421","330483") ,
                         labels =c("南湖区","秀洲区","海宁市","平湖市","海盐县","嘉善县","桐乡市") )
#生成拼音与姓名分列
cfdataall <- cfdataall |> mutate(
  pinyin = py(患者姓名, dic = pydic(method = "toneless",dic = "pinyin2")),
  namesp = str_split(患者姓名,pattern = "")
)


# cfdataall <- df %>% 
#   slice(which(df$name_code %in% count_df$name_code)) %>% 
#   mutate(area=str_sub(现住地址国标,1,6))

#筛选查重时间少于等于180天的病种
checkdisease <- unique(dfclass$疾病大类[dfclass$重卡天数 <=180 &!is.na(dfclass$重卡天数) & dfclass$疾病大类 %in% unique(count_df$疾病大类[!is.na(count_df$疾病大类)])])

#记录时间
now <- str_replace_all(str_sub(as.character(now()),1,19),pattern = "[ :]",replacement = "+")
#开始查重
#按照需要查重的病种筛选数据，一个一个病种根据就诊时间差、姓名有有两个字相同，拼音相同进行判断
for (diseasename in checkdisease){
  checkdf <- cfdataall %>% filter(疾病大类 == diseasename) %>% arrange(有效证件号,报告卡录入时间)
  if(nrow(checkdf) <=1) print("数据行数太少")
  tianshu <- unique(dfclass$重卡天数[dfclass$疾病大类==diseasename])
  checkdf$cf <- "首次就诊"
  
  for (i in 1:nrow(checkdf)){
    if(i==1){
      basename <- checkdf$有效证件号[1]
      basediadate  <- date(checkdf$报告卡录入时间[1])
    } else {
      if(checkdf$有效证件号[i] == checkdf$有效证件号[i-1]){
        if( (date(checkdf$报告卡录入时间[i]) - date(checkdf$报告卡录入时间[i-1])) <= tianshu) {
          if( sum(unlist(checkdf$namesp[i]) %in% unlist(checkdf$namesp[i-1]))>=2 
              |checkdf$pinyin[i] ==checkdf$pinyin[i-1])       checkdf$cf[i]<- "可疑重复报卡"
        } else {
          basename <-  checkdf$有效证件号[i]
          basediadate<-  date(checkdf$报告卡录入时间[i])
        }
      } else {
        basename <-  checkdf$有效证件号[i]
        basediadate<-  date(checkdf$报告卡录入时间[i])
      }
    }
    
  }  
  checkdf <- checkdf |> select(-namesp)
  write.csv(checkdf,paste0(diseasename,now,".csv"))
}




#以下用map函数进行循环
# duplicatecheck <- function(diseasename){
#   checkdf <- cfdataall %>% filter(疾病大类 == diseasename) %>% arrange(name_code,报告卡录入时间)
#   if(nrow(checkdf) <=1) print("数据行数太少")
#   tianshu <- unique(dfclass$重卡天数[dfclass$疾病大类==diseasename])
#   checkdf$cf <- "首次就诊"
#   
#   for (i in 1:nrow(checkdf)){
#     if(i==1){
#       basename <- checkdf$name_code[1]
#       basediadate  <- date(checkdf$报告卡录入时间[1])
#       
#     } else {
#       if(checkdf$name_code[i] == checkdf$name_code[i-1]){
#         if( (date(checkdf$报告卡录入时间[i]) - date(checkdf$报告卡录入时间[i-1])) <= tianshu) {
#           checkdf$cf[i]<- "可疑重复报卡"
#         } else {
#           basename <-  checkdf$name_code[i]
#           basediadate<-  date(checkdf$报告卡录入时间[i])
#           
#         }
#       } else {
#         basename <-  checkdf$name_code[i]
#         basediadate<-  date(checkdf$报告卡录入时间[i])
#         
#       }
#     }
#     
#   }  
#   write.csv(checkdf,paste0(diseasename,".csv"))
# }
# 
# map(checkdisease,duplicatecheck)

