


library(R0)

#估计代间距
#SIvec是一个向量，每一个数字表示一个一代病例与二代病例发病间隔时间。
GTn<- est.GT(serial.interval=SIvec)

#生成代际分布可以用generation. time函数，其中type参数指定分布
#可以根据实际情况选择gamma分布、weibull分布和lognormal等分布
#也可以指定为经验分布（empirical）；val设置分布的均值和标准差
#若为分布为 empirical，此处为一系列的分布值；step表示离散化的步长，通常为1。

GTn <- generation.time(type, val, step)

#基于罹患率的算法

R0AR <- est.R0.AR(AR,pop.size)

#基于指数增长率的方法

R0EG <- est.R0.EG(epid,GT,date.first.obs)

#极大似然估计方法

R0ML <- est.R0.ML(epid, GT, date.first.obs,import)

#序贯贝叶斯法

R0SB <- est.R0.SB(epid, GT, date.first.obs)

#多种算法同时使用

R0 <- estimate.R(epid, GT, date.first.obs, methods)




library(R0)

GTn <- generation.time(type= "gamma",
                       val=c( 7.5, 3.4),
                       step = 1)

#绘制代际时间分布图

par(mar=c( 5, 6, 4, 2))

plot(GTn,col= "blue",lwd = 4,cex.axis= 1.8,
     
     cex.lab= 1.8,cex.main= 2)

casedata <- read.csv("r0.csv")
R0EG <- est.R0.EG(casedata$case,GTn,begin=1, end =11)


