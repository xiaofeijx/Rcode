library(readxl)

mydata <- read_xlsx("C:\\Users\\fxf\\Documents\\流感日报.xlsx")

library(tsibble)
library(feasts)
library(tidyverse)

mydata$date <- ymd(mydata$date)
#11月数据
mydata11 <- mydata %>% filter(date>= ymd("2023-11-1"))



mydata11 <- as_tsibble(mydata11,index=date)
#mydata2 <- mydata %>% fill_gaps( 流感发病数 = 0)



mydata11 |>
  model(
    STL(报告日期 ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components()  |>
  autoplot()+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  labs(x="日期")+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.5, vjust = .5))

bg <- mydata11 |>
  model(
    STL(报告日期 ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() 

  
  hebing <- tibble(date=bg$date,
                  流感报告病例数=bg$报告日期,
                   流感报告病例数趋势=bg$trend) %>% 
    pivot_longer(cols=c(流感报告病例数,流感报告病例数趋势),names_to = "流感日数据",values_to = "fbs")
  

  
  ggplot(data=hebing,aes(x=date,y=fbs,color=流感日数据))+
    geom_line()+
    scale_x_date(date_breaks = "7 day",date_labels = "%m-%d")+
    theme(legend.position = "top")+
    labs(x="日期",y="病例数")+
    theme_classic()+
    theme(legend.position = "top")
  


#预测

library(forecast)
library(tseries)
a <- mydata11 |>
  model(
    STL(流感发病数 ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() 

fit <- auto.arima(a$trend)

forecast(fit,10)
plot(forecast(fit,10),col.main="darkgreen")



# coerce tibble to tsibble w/o a key
tbl1 <- tibble(
  date = as.Date("2017-01-01") + 0:9,
  value = rnorm(10)
)
as_tsibble(tbl1)
# supply the index to suppress the message
as_tsibble(tbl1, index = date)



#流感报卡分析
#流感年龄分布

library(readr)
library(tidyverse)
library(ggsci)
library(paletteer)
lg <- read_csv("C:\\Users\\fxf\\Documents\\报告卡2024-01-03+08_30_54.csv",locale=locale(encoding="GBK"))

lastchar <- str_sub(lg$年龄,nchar(lg$年龄),nchar(lg$年龄))
lg$age <- ifelse(lastchar=="岁",
                 as.numeric(str_sub(lg$年龄,1,str_length(lg$年龄)-1)),
                 (ifelse(lastchar=="月",
                         as.numeric(str_sub(lg$年龄,1,str_length(lg$年龄)-1))/12,
                         as.numeric(str_sub(lg$年龄,1,str_length(lg$年龄)-1))/365)))

#年龄分组
lg$agecut <- cut(lg$age,c(0,5,15,25,35,45,55,65,100))

#提取实验室检测分类
lg <- lg %>% mutate(bd = case_when(
  str_detect(lg$备注,"混合")~"混合感染",
  str_detect(lg$备注,"甲") & str_detect(lg$备注,"乙")~"混合感染",
  str_detect(lg$备注,"甲")~"甲流",
  str_detect(lg$备注,"乙")~"乙流",
  str_detect(lg$备注,"A")~"甲流",
  str_detect(lg$备注,"B")~"乙流",
  str_detect(lg$备注,"H3")~"甲流",
  str_detect(lg$备注,"Vic")~"乙流", 
  TRUE~"临床诊断"
),
bd=factor(bd,levels=c("甲流","乙流","混合感染","临床诊断")))


lg %>% count(bd)

lg$县区审核时间 <- as.Date(lg$县区审核时间)
lg <- lg[lg$县区审核时间>=ymd("2023-11-01")&lg$县区审核时间<=ymd("2024-01-02"),]
ggplot(data=lg[lg$县区审核时间>=ymd("2023-10-01"),],aes(x=县区审核时间))+
  geom_histogram(aes(fill=bd),binwidth = 1)+
  labs(x="日期",y="报告病例数",fill="诊断类型")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","临床诊断"),
                    values = c("#4dac26","#2b83ba","#d7191c","#f46d43"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top")

#按年龄分组
# ggplot(data=lg,aes(x=lurusj))+
#   geom_histogram()+
#   facet_grid(rows = vars(agecut))+
#   labs(x="日期",y="报告病例数")+
#   scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
#   theme(axis.text.x = element_text(angle = 45,hjust = 0.5, vjust = .5, size=8),
#         axis.line = element_line(colour = "black"),
#         panel.background = element_blank(),
#         legend.position = "top")



lg %>% count(bd)

lg %>% count(agecut)
lg$lurusj <-as.Date(lg$县区审核时间)

lggp <- lg %>% filter(!is.na(bd)) %>% group_by(lurusj,bd) %>% 
  summarise(renshu= n())

ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col()+
  labs(x="日期",y="报告病例数")

#病例数分布
ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col()+
  labs(x="日期",y="报告病例数",fill="诊断分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(breaks = c(500,1000,1500,2000,2500))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","临床诊断"),
                    values = c("#4dac26","#2b83ba","#d7191c","#f46d43"))+
   theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top" ,
        panel.grid.major.y = element_line(colour = "grey85"))

#百分比
ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col(position = "fill")+
#geom_vline(xintercept=seq.Date(from=ymd("2023-11-01"),to=ymd("2024-01-02"),by="1 day"),color="white")
  labs(x="日期",y="百分比（%）",fill="诊断分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","临床诊断"),
                    values = c("#4dac26","#2b83ba","#d7191c","#f46d43"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top")
  




#实验不同参数
library(scales)
ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col(position = "fill")+
#  geom_hline(yintercept = c(0.25,0.5,0.75),color="grey95",linewidth=0.05)+
  #geom_vline(xintercept=seq.Date(from=ymd("2023-11-01"),to=ymd("2024-01-02"),by="1 day"),color="white")
  labs(x="日期",y="百分比（%）",fill="诊断分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(breaks=seq(0.1,0.9,by=0.1),labels =label_percent())+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.text.y = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top",
        panel.grid.major.y = element_line(colour = "grey85"))


library(patchwork)
#上下图
#病例数分布
p1 <- ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col()+
  labs(x="",y="报告病例数",fill="病毒分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(breaks = c(500,1000,1500,2000,2500))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","临床诊断"),
                    values = c("#4dac26","#2b83ba","#d7191c","#f46d43"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top" ,
        panel.grid.major.y = element_line(colour = "grey85"))


p2 <- ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col(position = "fill")+
  #geom_vline(xintercept=seq.Date(from=ymd("2023-11-01"),to=ymd("2024-01-02"),by="1 day"),color="white")
  labs(x="日期",y="百分比",fill="病毒分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","临床诊断"),
                    values = c("#4dac26","#2b83ba","#d7191c","#f46d43"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "none")

p1/p2

#颜色实验
#百分比
ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col(position = "fill")+
  #geom_vline(xintercept=seq.Date(from=ymd("2023-11-01"),to=ymd("2024-01-02"),by="1 day"),color="white")
  labs(x="日期",y="百分比（%）",fill="诊断分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","临床诊断"),
                      values=c(甲流="#F3533A",乙流="#FA9f42",混合感染="#8ad879",临床诊断="#5acfc9"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top")


