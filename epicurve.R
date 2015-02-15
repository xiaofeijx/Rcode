#流行曲线
library(ggplot2)
library(scales)
mydata <- read.csv("time.csv")
#summary(mydata)
#mydf表示要输入的data.frame,其中的发病时间的变量名必须为time,
#binwidth为直方图的分组宽度，有数字表示，时间单位同X轴标签单位;
#中间要有空格
#breaks表示X轴上出现的标签间隔，要求带时间单位，如"12 hours","3 days"
epicurve.plot <- function(mydf,binwidth,breaks){
  mydf$time <- as.POSIXct(as.character(mydf$time))
  unit <-strsplit(breaks, " ")
  if (unit[[1]][2]=="hours") {
       ggplot(mydf,aes(x=time))+
      geom_histogram(binwidth=binwidth*3600)+
      scale_x_datetime(expand=c(0.05,0.05),breaks=date_breaks(breaks),labels=date_format("%m-%d\n%H:%M"))+
      labs(x="时间",y="病例数")
       }  else if (unit[[1]][2]=="days") {
           ggplot(data=mydf,aes(time))+
        geom_histogram(binwidth=binwidth*3600*24)+
        scale_x_datetime(expand=c(0.05,0.05),breaks=date_breaks(breaks),labels=date_format("%m-%d"))+
        labs(x="时间",y="病例数")}
  
  } 
     
png("xinfeng.png",height=300,width=600)

epicurve.plot(mydata,binwidth=2,breaks="2 days")
dev.off()
epicurve.plot(mydata,binwidth=3,breaks="2 hours")
X

str(mydata)

for( i in 1:61){
  a <- as.POSIXct(mydata$time[i])
  print(i)
  print()
}
breaks="12 days"
unit <-strsplit(breaks, " ")
unit[[1]][2]

ggplot(data=mtcars,aes(mpg,wt),width=800,height=600)+geom_point()

#卡方检验
mydata=c(46,31,44,23)
mydata2<-c(mydata[1]-mydata[2],mydata[2],mydata[3]-mydata[4],mydata[4])
mydata2
mymx <-matrix(mydata2,byrow=T,nrow=2)
mymx
chisq.test(mymx)
result <-chisq.test(mymx)
summary(result)

#自定义卡方函数
mychisq.test <-function(total1,n1,total2,n2){
  mydata=c(total1,n1,total2,n2)
mydata2<-c(mydata[1]-mydata[2],mydata[2],mydata[3]-mydata[4],mydata[4])
mydata2
mymx <-matrix(mydata2,byrow=T,nrow=2)
mymx
return(list(chisq.test(mymx),fisher.test(mymx)))
  
}




mychisq.test(1154,187,923,224)

chisq.test(matrix(c(43,247,99,481,70,315,129,384,21,57),ncol=2,byrow=T))
a=round(217/0.986)
b=round(188/0.855)
mychisq.test(a,217,b,188)
mychisq.test(2722,735,627,137)
mychisq.test(46,4,44,3)
mychisq.test(46,43,44,32)
mychisq.test(46,6,44,5)
chisq.test(matrix(c(42,90,58,94,20,248,490,327,419,58),ncol=2))
chisq.test(matrix(c(1,9,12,35,1,289,571,373,478,77),ncol=2))

fisher.test(matrix(c(1,9,12,35,1,289,571,373,478,77),ncol=2))

my <-c(113,37,95,35,12,1)
md <-c(my[1]-my[2],my[2],my[3]-my[4],my[4],my[5]-my[6],my[6])
mm <-matrix(md,byrow=T, ncol=2)
summary(mm)
mm
chisq.test(mm)

#双样本T检验，不提供原始数据
#设样本1均数为x1,样本量n1,标准差s1
#设样本1均数为x2,样本量n2,标准差s2
#首先进行等方差检验，然后进行T检验
x1<-815
x2<-796
n1 <-46
n2 <-44
s1 <-28
s2 <-42
myf.value=s1^2/s2^2
#F检验的P值
p.value=pf(myf.value,n1,n2)
#97.5的F分布下的F值
qf(0.975,n1,n2)
#计算T值
myt=(x1-x2)/(s1^2/n1+s2^2/n2)^0.5
myt
#计算自由度
mydf=(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
mydf=as.integer(mydf)
#计算P值

ifelse (myt>0,myt.value<-(1-pt(myt,mydf))*2,2*pt(myt,mydf)) 


myt.test <-list(myt,myt.value)
myt.test

#自定义函数
myt.test <-function(x1,x2,n1,n2,s1,s2){
  myt=(x1-x2)/(s1^2/n1+s2^2/n2)^0.5

#计算自由度
mydf=(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
mydf=as.integer(mydf)
#计算P值

  myt.value<-ifelse (myt>0,(1-pt(myt,mydf))*2,2*pt(myt,mydf)) 


myt.test <-list(tvalue=myt,pvalue=myt.value)
return(myt.test)
}

myt.test(42.50,34.20,60,60,4.34,5.73)
myt.test(41,43,55,55,13,12)

myt.test(5.10,7.59,56,54,1.28,0.90)
myt.test(6.79,7.48,56,54,1.12,1.33)
myt.test(7.71,10.1,56,54,0.47,0.89)
a <- myt.test(815,796,46,44,28,42)
myt.test(49,78,46,44,15,22)
myt.test(617,609,46,44,29,27)
myt.test(53,69,46,44,16,21)
#97.5时的T值
qt(0.975,mydf)


#验证公式
sample1=rnorm(46,45,20)
x1=mean(sample1)
n1=46
s1=sd(sample1)

sample2=rnorm(44,30,12)
x2=mean(sample2)
n2=44
s2=sd(sample2)

t.test(sample1,sample2)
#回归分析，ISwR数据
options(na.action=na.exclude)
lm.velo<-lm(short.velocity~blood.glucose)
qplot(blood.glucose,short.velocity,data=thuesen)+geom_smooth(method="lm")
plot(blood.glucose,short.velocity)

fitted(lm.velo)
resid(lm.velo)

pred.frame <- data.frame(blood.glucose=thuesen$blood.glucose)
pp<-predict(lm.velo,int="p",newdata=pred.frame)
mydf<-data.frame(pred.frame,pp)
my.naexclude <- data.frame(thuesen,fitted=fitted(lm.velo),resid=resid(lm.velo))
#画回归，链接点和回归线
qplot(blood.glucose,short.velocity,data=my.naexclude)+
  geom_smooth(method="lm")+
  geom_linerange(aes(ymin=fitted,ymax=short.velocity),fullrange=T,colour="grey50")+
  geom_ribbon(aes(x=blood.glucose,ymin=lwr,ymax=upr),data=mydf,fill=alpha("red",I(1/10)))


x.resid <-data.frame(blood.glucose[!is.na(short.velocity)],resid(lm.velo))



colnames(x.resid)<-c("a","b")

qplot(a,b,data=x.resid)
+geom_hline(yintercept=0,size=2)

cc <-complete.cases(thuesen)


p <-seq(0,1,0.05)
1-pbinom(1,5,p)



bb<- matrix(c(59,31,6,48,25,17),byrow=T,nrow=2)
bb
chisq.test(bb)

year <-c(2007,2008,2009,2010,2011)
#inci <-c(222.32,201.96,204.17,191.97,186.44)
inci <-c(46.08,284.73,235.08,359.34,262.79)
sz <-c(NA,41.2,58.81,130.75,71.14)
fx <-c(260.49,186.09,130.86,166.46,160.78)
incidf <- data.frame(x=year,incidence=inci,szkb=sz,fxb=fx)
mymelt <- melt(incidf,id="x",measure=c("incidence","szkb","fxb"))
summary(mymelt)
mymelt$mygroup <- mymelt$variable
ggplot(data=mymelt,aes(x,value))+
  geom_line(size=1,aes(colour=mygroup)

+
  labs(y="发病率（/10万）",x="")+
  opts(axis.title.y=theme_text(size=14,angle=90))

library(vcd)
data("Titanic")
mosaic(Titanic)
            ## load Coal Miners data
            data("CoalMiners")
            ## compute log odds ratios
            lor <- oddsratio(CoalMiners,log=F)
            
CoalMiners
            summary(lor)
            plot(lor)
            
f <-7:20
p <- 
curve(choose(7,3)*choose(x-7,1)/choose(x,4),7,20)
curve(x^5*(1-x)^2,0,1,xlab="p",ylab="L(p)")
curve(x^4*(1-x)^3,0,1,add=T)

x <- mtcars$am
head(mtcars)
L <- function(p,x) prod(dbinom(x,size=1,prob=p))
  optimize(L,interval=c(0,1),x=x,maximum=T)
            
myL <- function(mu,sigma2){
  -sum(dnorm(x,mean=mu,sd=sqrt(sigma2),log=T))
}

x <- PlantGrowth$weight
    max <- mle(myL,start=list(mu=5,sigma2=0.5))
summary(max)
 tosscoin(1)  


eventread <-read.csv("zhoudaixiong.csv",header=T)
eventread$time <- as.POSIXct(eventread$time)
eventread$zero <- c(rep(0,7))
eventread$timebk <- as.numeric(eventread$time)
library(ggplot2)
library(scales)
ggplot(data=eventread,aes(x=time,y=series))+geom_point()+
  scale_x_datetime(labels=date_format("%m-%d"),limits=c(as.POSIXct("2014-11-1"),as.POSIXct("2014-12-1")),breaks=eventread$time)+
  annotate("text",x=eventread$time,y=eventread$series,label=eventread$events,vjust=0.5,hjust=-0.05,size=5)+
  geom_linerange(aes(ymax=series,ymin=zero))
 

            
            
            




          