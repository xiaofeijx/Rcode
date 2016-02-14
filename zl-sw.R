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

b <- read_csv("C:/Users/Administrator/Documents/sw/zl3304008.csv")
b[1,]
dim(b)
names(b)

################糖尿病
#糖尿病中死亡日期空与2014-2015死亡库核对
library(dplyr)
sw0 <- read.csv("C:/Users/Administrator/Documents/sw/sw330400.csv")
#tnb <- read.csv("C:/Users/Administrator/Documents/sw/tnb330400.csv")
head(tnb)
tail(tnb)
tnb <- read.csv("C:/Users/Administrator/Documents/sw/tnb330400.csv",
                na.strings = "",
                stringsAsFactors = FALSE)


sw <- tbl_df(sw0)
tnb <- tbl_df(tnb)


tnb <- tnb[tnb$报告卡状态 %in% c("可用卡","失访卡","死卡","死亡卡" ) & tnb$死亡日期 == "",]

tnb <- tnb[!(tnb$身份证号码 == "'"),]

sw <- sw[!(sw$证件号码 == "'") & (sw$报告卡状态 == "可用卡"),]

tnb2 <-inner_join(tnb,sw,by=c("身份证号码"="证件号码"))

tnbswk330402 <- tnb2[tnb2$户口地址区县=="33040200  南湖区",]
write.csv(tnbswk330402,"c:/tnbswk330402.csv")

tnbswk330421 <- tnb2[tnb2$户口地址区县=="33042100  嘉善县",]
write.csv(tnbswk330421,"c:/tnbswk330421.csv")

tnbswk330482 <- tnb2[tnb2$户口地址区县=="33048200  平湖市",]
write.csv(tnbswk330482,"c:/tnbswk330482.csv")

tnbswk330424 <- tnb2[tnb2$户口地址区县=="33042400  海盐县",]
write.csv(tnbswk330424,"c:/tnbswk330424.csv")

tnbswk330481 <- tnb2[tnb2$户口地址区县=="33048100  海宁市",]
write.csv(tnbswk330481,"c:/tnbswk330481.csv")

tnbswk330411 <- tnb2[tnb2$户口地址区县=="33041100  秀洲区",]
write.csv(tnbswk330411,"c:/tnbswk330411.csv")

tnbswk330483 <- tnb2[tnb2$户口地址区县=="33048300  桐乡市",]
write.csv(tnbswk330483,"c:/tnbswk330483.csv")

###################
#日期错误，年龄小于20，糖尿病类型等于2型和妊娠糖尿病
names(tnb)
#"出生日期"  "诊断日期""报卡日期""死亡日期" ,"初访时间" "最后随访时间"

write.csv(tnb[1:10,],"c:/nn.csv")
tnb$出生日期 <- as.Date(as.character(tnb$出生日期))
tnb$死亡日期 <- as.Date(as.character(tnb$死亡日期))
tnb$诊断日期 <- as.Date(as.character(tnb$诊断日期))
tnb$报卡日期 <- as.Date(as.character(tnb$报卡日期))
tnb$初访时间 <- as.Date(as.character(tnb$初访时间))
tnb$最后随访时间 <- as.Date(as.character(tnb$最后随访时间))


tnb <- tnb[tnb$报告卡状态 %in% c("可用卡","失访卡","死卡","死亡卡" ),]

#出生日期小于所有日期
luoji1 <- (tnb$出生日期> tnb$死亡日期) | (tnb$出生日期> tnb$诊断日期) | (tnb$出生日期> tnb$报卡日期) | (tnb$出生日期> tnb$初访时间) | (tnb$出生日期> tnb$最后随访时间)

#诊断日期大于死亡日期
luoji2 <- (tnb$诊断日期 > tnb$死亡日期)
luoji2[is.na(luoji2)] <- FALSE


#诊断日期大于初访或随访日期、报告日期
luoji3 <- (tnb$诊断日期> tnb$初访时间)
luoji3[is.na(luoji3)] <- FALSE

luoji4 <- (tnb$诊断日期> tnb$最后随访时间)
luoji4[is.na(luoji4)] <- FALSE

luoji5 <- (tnb$诊断日期> tnb$报卡日期)
luoji5[is.na(luoji5)] <- FALSE

#最后随访时间大于死亡日期
luoji6 <- (tnb$最后随访时间 > tnb$死亡日期)
luoji6[is.na(luoji6)] <- FALSE

luojilast <- luoji2 | luoji3 | luoji4 | luoji5 | luoji6
sum(luojilast)

write.csv(tnb[luojilast,],"c:/tnb日期逻辑错误.csv")
############

write.csv(tnb[luoji2,],"c:/tnb诊断日期大于死亡日期.csv")

tnb$age <- floor((tnb$诊断日期 - tnb$出生日期)/365.25)
tnb$age[1:10]

luoji7 <- tnb$age <20  & (tnb$糖尿病类型 =="II型糖尿病")
luoji8 <- tnb$age <15  & (tnb$糖尿病类型 =="妊娠糖尿病")
write.csv(tnb[luoji7,],"c:/tnb年龄与糖尿病类型核查.csv")
write.csv(tnb[tnb$age <0,],"c:/tnb年龄为负值.csv")
write.csv(tnb[luoji8,],"c:/tnb妊娠糖尿病.csv")

write.csv(tnb[luoji7 | luoji8,],"c:/tnb年龄问题.csv")


##############心脑历史库与2014-2015死亡库比对
sw0 <- read.csv("C:/Users/Administrator/Documents/sw/sw330400.csv",stringsAsFactors = F)
xn <-  read.csv("C:/Users/Administrator/Documents/sw/xnxg330400.csv",stringsAsFactors = F)
library(dplyr)
sw <- tbl_df(sw0)
xn <- tbl_df(xn)
names(xn)
sw <- sw[!(sw$证件号码 == "'") & (sw$报告卡状态 == "可用卡"),]
xn1 <- xn[xn$卡状态 %in% c("可用卡","失访卡","死卡","死亡卡" ) & xn$死亡日期 == "" & xn$身份证号 !="'",]

xnswk <-inner_join(xn1,sw,by=c("身份证号"="证件号码"))
xnswk$常住户口地址区县[1:50]

write.csv(xnswk,"c:/xnswk330400.csv")

xnswk330402 <- xnswk[xnswk$常住户口地址区县=="33040200  南湖区",]
write.csv(xnswk330402,"c:/xnswk330402.csv")

xnswk330421 <- xnswk[xnswk$常住户口地址区县=="33042100  嘉善县",]
write.csv(xnswk330421,"c:/xnswk330421.csv")

xnswk330482 <- xnswk[xnswk$常住户口地址区县=="33048200  平湖市",]
write.csv(xnswk330482,"c:/xnswk330482.csv")

xnswk330424 <- xnswk[xnswk$常住户口地址区县=="33042400  海盐县",]
write.csv(xnswk330424,"c:/xnswk330424.csv")


####
xnswk330481 <- xnswk[xnswk$常住户口地址区县=="33048100  海宁市",]
write.csv(xnswk330481,"c:/xnswk330481.csv")

xnswk330411 <- xnswk[xnswk$常住户口地址区县=="33041100  秀洲区",]
write.csv(xnswk330411,"c:/xnswk330411.csv")

xnswk330483 <- xnswk[xnswk$常住户口地址区县=="33048300  桐乡市",]
write.csv(xnswk330483,"c:/xnswk330483.csv")

##############心脑逻辑校验
#年龄小于15岁，写到一个文件中
xn2 <- xn[xn$卡状态 %in% c("可用卡","失访卡","死卡","死亡卡" ),]
names(xn2)
xn2$发病日期[1:10]

xn2$发病日期 <- as.Date(xn2$发病日期)
xn2$出生日期 <- as.Date(xn2$出生日期)

xn2$死亡日期 <- as.Date(xn2$死亡日期)
xn2$age <- floor((xn2$发病日期 - xn2$出生日期)/365.25)
xnage <- xn2[xn2$age <20,]
write.csv(xnage,"c:/xnage20.csv")

#发病日期大于死亡日期
luoji <- xn2$发病日期 > xn2$死亡日期
luoji[is.na(luoji)] <- FALSE

xn2fabingsiwang <- xn2[luoji,]
write.csv(xn2fabingsiwang,"c:/xn2发病大于死亡330400.csv")

xn2fabingsiwang330402 <- xn2fabingsiwang[xn2fabingsiwang$常住户口地址区县=="33040200  南湖区",]
write.csv(xn2fabingsiwang330402,"c:/xn2发病大于死亡330402.csv")

xn2fabingsiwang330421 <- xn2fabingsiwang[xn2fabingsiwang$常住户口地址区县=="33042100  嘉善县",]
write.csv(xn2fabingsiwang330421,"c:/xn2发病大于死亡330421.csv")

xn2fabingsiwang330482 <- xn2fabingsiwang[xn2fabingsiwang$常住户口地址区县=="33048200  平湖市",]
write.csv(xn2fabingsiwang330482,"c:/xn2发病大于死亡330482.csv")

xn2fabingsiwang330424 <- xn2fabingsiwang[xn2fabingsiwang$常住户口地址区县=="33042400  海盐县",]
write.csv(xn2fabingsiwang330424,"c:/xn2发病大于死亡330424.csv")

xn2fabingsiwang330481 <- xn2fabingsiwang[xn2fabingsiwang$常住户口地址区县=="33048100  海宁市",]
write.csv(xn2fabingsiwang330481,"c:/xn2发病大于死亡330481.csv")

xn2fabingsiwang330411 <- xn2fabingsiwang[xn2fabingsiwang$常住户口地址区县=="33041100  秀洲区",]
write.csv(xn2fabingsiwang330411,"c:/xn2发病大于死亡330411.csv")

xn2fabingsiwang330483 <- xn2fabingsiwang[xn2fabingsiwang$常住户口地址区县=="33048300  桐乡市",]
write.csv(xn2fabingsiwang330483,"c:/xn2发病大于死亡330483.csv")
