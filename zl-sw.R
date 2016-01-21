#########################死亡与肿瘤########

#任务一描述：zl330400是肿瘤卡，sw330400是死亡卡。对于肿瘤卡中的
#"可用卡","失访卡","死卡","死亡卡"，如果卡片的死亡日期为空，那么
#在sw卡中通过身份证匹配查找这个人，将可以匹配到数据合并在一起
#将合并的数据按县区拆分，写到csv文件中。
#先把数据转为UTF-8
library(dplyr)
sw0 <- read.csv("C:/Users/Administrator/Documents/sw/sw330400.csv")
zl0 <- read.csv("C:/Users/Administrator/Documents/sw/zl330400.csv")

head(sw)
head(zl)

sw <- tbl_df(sw0)
zl <- tbl_df(zl0)


zl <- zl[zl$报告卡状态 %in% c("可用卡","失访卡","死卡","死亡卡" ) & zl$死亡日期 == "",]

zl <- zl[!(zl$身份证号码 == "'"),]
sw <- sw[!(sw$证件号码 == "'") & (sw$报告卡状态 == "可用卡"),]

zl2 <-inner_join(zl,sw,by=c("身份证号码"="证件号码"))



write.csv(zl2,"c:/zl2.csv")

zl330402 <- zl2[zl2$区县=="33040200  南湖区",]
write.csv(zl330402,"c:/zl330402.csv")
zl330421 <- zl2[zl2$区县=="33042100  嘉善县",]
write.csv(zl330421,"c:/zl330421.csv")

zl330482 <- zl2[zl2$区县=="33048200  平湖市",]
write.csv(zl330482,"c:/zl330482.csv")

zl330424 <- zl2[zl2$区县=="33042400  海盐县",]
write.csv(zl330424,"c:/zl330424.csv")

zl330481 <- zl2[zl2$区县=="33048100  海宁市",]
write.csv(zl330481,"c:/zl330481.csv")

zl330411 <- zl2[zl2$区县=="33041100  秀洲区",]
write.csv(zl330411,"c:/zl330411.csv")

zl330483 <- zl2[zl2$区县=="33048300  桐乡市",]
write.csv(zl330483,"c:/zl330483.csv")




##################################
##sw <- read.csv("C:/Users/Administrator/Documents/sw/sw330400.csv")
##zl <- read.csv("C:/Users/Administrator/Documents/sw/zl330400.csv")
#任务二描述：zl330400是肿瘤卡，sw330400是死亡卡。对于死亡卡中的数据
#按照身份证号码与肿瘤卡匹配，对与能在肿瘤卡中匹配到的数据，
#提取肿瘤卡中的"报告卡编号","姓名","诊断日期","ICD.10",
#将肿瘤卡提出出的信息，按身份证号码，合并到死亡卡中
#最后的数据是以死亡卡主，能匹配到肿瘤卡的条目
#会在最后出现肿瘤卡提出的部分信息

sw <- tbl_df(sw0)
zl <- tbl_df(zl0)

names(zl)
zl <- zl[zl$报告卡状态 %in% c("可用卡","失访卡","死卡","死亡卡" ),c("报告卡编号","姓名","诊断日期","ICD.10","身份证号码")]

zl <- zl[!(zl$身份证号码 == "'"),]
sw2 <- sw[!(sw$证件号码 == "'"),c("证件号码")]


zl2 <-inner_join(zl,sw2,by=c("身份证号码"="证件号码"))

sw3 <- left_join(sw,zl2,by=c("证件号码"="身份证号码"))

write.csv(sw3,"c:/sw3.csv")

sw3 <- read.csv("c:/sw3.csv")

sw330402 <- sw3[sw3$户口地址区县=="33040200  南湖区",]
write.csv(sw330402,"c:/sw330402.csv")
sw330421 <- sw3[sw3$户口地址区县=="33042100  嘉善县",]
write.csv(sw330421,"c:/sw330421.csv")

sw330482 <- sw3[sw3$户口地址区县=="33048200  平湖市",]
write.csv(sw330482,"c:/sw330482.csv")

sw330424 <- sw3[sw3$户口地址区县=="33042400  海盐县",]
write.csv(sw330424,"c:/sw330424.csv")

sw330481 <- sw3[sw3$户口地址区县=="33048100  海宁市",]
write.csv(sw330481,"c:/sw330481.csv")

sw330411 <- sw3[sw3$户口地址区县=="33041100  秀洲区",]
write.csv(sw330411,"c:/sw330411.csv")

sw330483 <- sw3[sw3$户口地址区县=="33048300  桐乡市",]
write.csv(sw330483,"c:/sw330483.csv")






#############肿瘤原始拆分
zl <- zl0

zlys330402 <- zl[zl$区县=="33040200  南湖区",]
write.csv(zlys330402,"c:/zlys330402.csv")

zlys330421 <- zl[zl$区县=="33042100  嘉善县",]
write.csv(zlys330421,"c:/zlys330421.csv")

zlys330482 <- zl[zl$区县=="33048200  平湖市",]
write.csv(zlys330482,"c:/zlys330482.csv")

zlys330424 <- zl[zl$区县=="33042400  海盐县",]
write.csv(zlys330424,"c:/zlys330424.csv")

zlys330481 <- zl[zl$区县=="33048100  海宁市",]
write.csv(zlys330481,"c:/zlys330481.csv")

zlys330411 <- zl[zl$区县=="33041100  秀洲区",]
write.csv(zlys330411,"c:/zlys330411.csv")

zlys330483 <- zl[zl$区县=="33048300  桐乡市",]
write.csv(zlys330483,"c:/zlys330483.csv")


#########需要死亡补发病的
#任务三：对于死亡卡中，根本死亡原因开头为"C"的死亡卡，如果通过身份证号码匹配，
#无法在肿瘤卡中找到相应的卡片，那么把这些死亡卡筛选出来
library(dplyr)

sw <- tbl_df(sw0)
zl <- tbl_df(zl0)


zl <- zl[zl$报告卡状态 %in% c("可用卡","失访卡","死卡","死亡卡" ),]

zl <- zl[!(zl$身份证号码 == "'"),]

sw <- sw[!(sw$证件号码 == "'") & (sw$报告卡状态 == "可用卡") & (substr(sw$根本死亡原因,1,1)=="C"),]





notinzl <- data.frame(id=setdiff(sw$证件号码, zl$身份证号码))
swnotinzl <- left_join(notinzl,sw,by=c("id"="证件号码"))

write.csv(swnotinzl,"c:/swnotinzl.csv")

swbfb330402 <- swnotinzl[swnotinzl$户口地址区县=="33040200  南湖区",]
write.csv(swbfb330402,"c:/swbfb330402.csv")

swbfb330421 <- swnotinzl[swnotinzl$户口地址区县=="33042100  嘉善县",]
write.csv(swbfb330421,"c:/swbfb330421.csv")

swbfb330482 <- swnotinzl[swnotinzl$户口地址区县=="33048200  平湖市",]
write.csv(swbfb330482,"c:/swbfb330482.csv")

swbfb330424 <- swnotinzl[swnotinzl$户口地址区县=="33042400  海盐县",]
write.csv(swbfb330424,"c:/swbfb330424.csv")

swbfb330481 <- swnotinzl[swnotinzl$户口地址区县=="33048100  海宁市",]
write.csv(swbfb330481,"c:/swbfb330481.csv")

swbfb330411 <- swnotinzl[swnotinzl$户口地址区县=="33041100  秀洲区",]
write.csv(swbfb330411,"c:/swbfb330411.csv")

swbfb330483 <- swnotinzl[swnotinzl$户口地址区县=="33048300  桐乡市",]
write.csv(swbfb330483,"c:/swbfb330483.csv")

##################试验
library(dplyr)

library(readr)

b <- read_csv("C:/Users/Administrator/Documents/sw/zl330400.csv")
b
names(b)
