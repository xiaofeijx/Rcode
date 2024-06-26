#流感报卡分析
#流感年龄分布

library(readr)
library(tidyverse)
#library(ggsci)
#library(paletteer)
lg <- read_csv("C:\\Users\\fxf\\Documents\\报告卡2024-02-08+08_54_35.csv",
               col_select=c(年龄,性别,县区审核时间,备注),
               locale=locale(encoding="GBK")) %>% 
  #  select(年龄,县区审核时间,备注) %>%
  mutate(县区审核时间= as.Date(县区审核时间)) %>% 
  filter(县区审核时间>=ymd("2023-11-1")&县区审核时间 <=ymd("2024-02-8")) %>% 
  arrange(desc(县区审核时间))


b <- lg %>% count(县区审核时间)

age_cor <- function(x){
  value <- as.numeric(str_remove(x,"\\D$"))
  case_when(
    str_ends(x,"天")~value/365,
    str_ends(x,"月")~value/12,
    str_ends(x,"岁")~value,
    .default = NA
    )
}
lg$age <- age_cor(lg$年龄)


# lastchar <- str_sub(lg$年龄,nchar(lg$年龄),nchar(lg$年龄))
# lg$age <- ifelse(lastchar=="岁",
#                  as.numeric(str_sub(lg$年龄,1,str_length(lg$年龄)-1)),
#                  (ifelse(lastchar=="月",
#                          as.numeric(str_sub(lg$年龄,1,str_length(lg$年龄)-1))/12,
#                          as.numeric(str_sub(lg$年龄,1,str_length(lg$年龄)-1))/365)))

#年龄分组
lg$agecut <- cut(lg$age,c(0,2,5,10,15,25,65,100))

#提取实验室检测分类
lg <- lg %>% mutate(bd = case_when(
  #  str_detect(lg$备注,"混合")~"混合感染",
  str_detect(lg$备注,"甲") & str_detect(lg$备注,"乙")~"混合感染",
  str_detect(lg$备注,"甲")~"甲流",
  str_detect(lg$备注,"乙")~"乙流",
  str_detect(lg$备注,"A")~"甲流",
  #  str_detect(lg$备注,"甲流a")~"甲流",
  str_detect(lg$备注,"流感a")~"甲流",
  # str_detect(lg$备注,"甲流b")~"乙流",
  str_detect(lg$备注,"流感b")~"乙流",
  str_detect(lg$备注,"B")~"乙流",
  str_detect(lg$备注,"H3")~"甲流",
  str_detect(lg$备注,"Vic")~"乙流", 
  .default ="未分型"
),
bd=factor(bd,
          levels=c("甲流","乙流","混合感染","未分型"),
          labels=c("甲流","乙流","混合感染","未分型")))
#write.csv(lg,"lg.csv")

lg %>% count(bd)
#lg %>% filter(is.na(bd)) %>% write.csv("na.csv")

#lg$县区审核时间 <- as.Date(lg$县区审核时间)
pacman::p_load(showtext)
# font_add("msyh", "msyh")
showtext::showtext_auto()
a <- font_files()
font_add("st", "simsun.ttc")
font_add("song","STSONG.TTF")
font_add("times","times.ttf")

ggplot(data=lg,aes(x=县区审核时间))+
  geom_histogram(aes(fill=bd),binwidth = 1)+
  labs(x="日期",y="报告病例数",fill="诊断类型")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","未分型"),
                    labels = c("甲流", "乙流", "混合感染","未分型"),
                    values=c(甲流="#42B540FF",乙流="#1f78b4",混合感染="#e41a1c",未分型="#fc8d62"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        text = element_text(family = "st"))


#按性别：
ggplot(data=lg,aes(x=县区审核时间))+
  geom_histogram(aes(fill=bd),binwidth = 1)+
  labs(x="日期",y="报告病例数",fill="诊断类型")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  facet_grid(性别~.)+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","未分型"),
                    labels = c("甲流", "乙流", "混合感染","未分型"),
                    values=c(甲流="#33a02c",乙流="#1f78b4",混合感染="#e41a1c",未分型="#fc8d62"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        text = element_text(family = "st"))


lg %>% count(bd)

lg %>% count(agecut)
lg$lurusj <-as.Date(lg$县区审核时间)

lggp <- lg %>% group_by(lurusj,bd) %>% 
  summarise(renshu= n())

lggpwide <-lggp %>%  pivot_wider(names_from = bd,values_from = renshu)

library(jsonlite)
write_json(lggpwide,"influ.json")

ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col()+
  labs(x="日期",y="报告病例数")+
  theme(text = element_text(family = "Songti SC"))


#按年龄分组
ggplot(data=lg,aes(x=lurusj))+
  geom_histogram()+
  facet_grid(rows = vars(agecut))+
  labs(x="日期",y="报告病例数")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  theme(axis.text.x = element_text(angle = 45,hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top")

#病例数分布
ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col()+
  labs(x="日期",y="报告病例数",fill="诊断分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(breaks = c(500,1000,1500,2000,2500))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","未分型"),
                    labels= c("甲流", "乙流", "混合感染","未分型"),
                    values=c(甲流="#33a02c",乙流="#1f78b4",混合感染="#e41a1c",未分型="#fc8d62"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top" ,
        panel.grid.major.y = element_line(colour = "grey85"),
        legend.text = element_text(size = 10,family = "st"),
        legend.title = element_text(size = 11,face = "italic",family = "st"),
  )

#百分比
ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col(position = "fill")+
  #geom_vline(xintercept=seq.Date(from=ymd("2023-11-01"),to=ymd("2024-01-02"),by="1 day"),color="white")
  labs(x="日期",y="百分比（%）",fill="诊断分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","未分型"),
                    values=c(甲流="#33a02c",乙流="#1f78b4",混合感染="#e41a1c",未分型="#fc8d62"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top",
        panel.grid.major.y = element_line(colour = "grey85"))





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
  labs(title="嘉兴市流感报告病例病毒检出情况",x="",y="报告病例数",fill="病毒分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks = c(500,1000,1500,2000,2500))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","未分型"),
                    values=c(甲流="#33a02c",乙流="#1f78b4",混合感染="#e41a1c",未分型="#fc8d62"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8,family="times"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        #legend.position = "top" ,
        panel.grid.major.y = element_line(colour = "grey85"),
        text = element_text(family = "st"),
        legend.text = element_text(size=9),
        #legend.title = element_text(size=10,face = "bold"),
        legend.position = c(.9, .75),
        plot.title = element_text(hjust = 0.5,size = 15,face = "bold"))


p2 <- ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col(position = "fill")+
  #geom_vline(xintercept=seq.Date(from=ymd("2023-11-01"),to=ymd("2024-01-02"),by="1 day"),color="white")
  labs(x="日期",y="百分比",fill="病毒分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m-%d")+
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","未分型"),
                    values=c(甲流="#33a02c",乙流="#1f78b4",混合感染="#e41a1c",未分型="#fc8d62"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8,family="times"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "none",
        text = element_text(family = "st"))

p1/p2

#颜色实验
#百分比
ggplot(lggp,aes(x=lurusj,y=renshu,fill=bd))+
  geom_col(position = "fill")+
  #geom_vline(xintercept=seq.Date(from=ymd("2023-11-01"),to=ymd("2024-01-02"),by="1 day"),color="white")
  labs(x="日期",y="百分比（%）",fill="诊断分类")+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","未分型"),
                    values=c(甲流="#33a02c",乙流="#1f78b4",混合感染="#e41a1c",未分型="#fc8d62"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=8),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top")

#gtsummary
library(gtsummary)
lgtb <- lg %>% select(lurusj,bd) %>% 
  mutate(lurusj=factor(lurusj))



lgtb %>%
  tbl_cross(
    row = lurusj,
    col = bd,
    percent = "row",
    label = list(bd ~ "病毒分型",lurusj="报卡时间")
  ) %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path='流感.docx') 




#按周分析
library(tsibble)
lg$week <- yearweek(lg$县区审核时间)

lgweek <- lg %>% group_by(week,bd) %>% summarise(renshu=n())


ggplot(data=lgweek,aes(x=as.character(week),y=renshu,fill=bd))+
  geom_col()+
  labs(x="周",y="报告病例数",fill="病毒分类")+
  # scale_x_continuous(labels =c("2023 W44","2023 W45","2023 W46","2023 W47","2023 W48","2023 W49","2023 W50",
  #                       "2023 W51","2023 W52","2024 W01","2024 W02"  ))+
  scale_fill_manual(limits = c("甲流", "乙流", "混合感染","未分型"),
                    values=c(甲流="#33a02c",乙流="#1f78b4",混合感染="#e41a1c",未分型="#fc8d62"))+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = .5, size=10),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "top" ,
        panel.grid.major.y = element_line(colour = "grey85"),
        text = element_text(family = "st"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11,face = "bold"))


#流感和新冠
library(readr)
library(tidyverse)
#library(ggsci)
#library(paletteer)
lg <- read_csv("C:\\Users\\fxf\\Documents\\流感报告卡2024-02-09+08_36_41.csv",
               col_select=c(年龄,性别,疾病名称,县区审核时间,备注),
               locale=locale(encoding="GBK")) |>  
  #  select(年龄,县区审核时间,备注) %>%
  mutate(县区审核时间= as.Date(县区审核时间)) |>  
  filter(县区审核时间>=ymd("2023-11-1")&县区审核时间 <=ymd("2024-02-09")) |> 
  arrange(desc(县区审核时间))

sg <- read_csv("C:\\Users\\fxf\\Documents\\新冠报告卡2024-02-09+08_38_14.csv",
               col_select=c(年龄,性别,疾病名称,县区审核时间,备注),
               locale=locale(encoding="GBK")) |>  
  #  select(年龄,县区审核时间,备注) %>%
  mutate(县区审核时间= as.Date(县区审核时间)) |>  
  filter(县区审核时间>=ymd("2023-11-1")&县区审核时间 <=ymd("2024-02-09")) |> 
  arrange(desc(县区审核时间))

all <- bind_rows(lg,sg)


#lg$县区审核时间 <- as.Date(lg$县区审核时间)
pacman::p_load(showtext)
# font_add("msyh", "msyh")
showtext::showtext_auto()
a <- font_files()
font_add("st", "simsun.ttc")
font_add("song","STSONG.TTF")
font_add("times","times.ttf")

ggplot(data=all,aes(x=县区审核时间))+
  geom_histogram(aes(fill=疾病名称),binwidth = 1)+
  scale_fill_manual(values=c(流行性感冒="#33a02c",新型冠状病毒感染="#1f78b4"))+
  labs(y="报告病例数")+
  guides(fill = FALSE)+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  facet_grid(疾病名称~.,scales = "free")+
  theme(text = element_text(family = "st"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),      
)


data_sum <- all |> group_by(县区审核时间,疾病名称) |> 
  summarise(n=n()) 


ggplot(data=data_sum,aes(x=县区审核时间,y=n))+
  geom_col(aes(fill=疾病名称))+
  scale_fill_manual(values=c(流行性感冒="#33a02c",新型冠状病毒感染="#1f78b4"))+
  labs(y="报告病例数")+
  guides(fill = FALSE)+
  scale_x_date(date_breaks = "7 day",date_labels = "%m月\n%d日")+
  facet_grid(疾病名称~.,scales = "free")+
  theme(text = element_text(family = "st"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),      
  )


ggplot(data=lg,aes(x=县区审核时间))+
  geom_bar()+
  scale_x_binned()

ggplot(mtcars) +
  geom_bar(aes(mpg)) +
  scale_x_binned()
  