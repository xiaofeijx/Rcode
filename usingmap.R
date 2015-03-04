library(rgdal)
library(ggplot2)
library(rgeos)
#画浙江地图
shape.dir = "F:/CHN_adm"
Zhejiang.shp = readOGR(shape.dir, layer = "CHN_adm2")
str(Zhejiang.shp)
names(Zhejiang.shp)
Zhejiang.shp$NL_NAME_2
Zhejiang.shp$VARNAME_2
Zhejiang.shp = Zhejiang.shp[Zhejiang.shp$NAME_1=="Zhejiang",]

Zhejiang.df=fortify(Zhejiang.shp)
names(Zhejiang.df)
names(Zhejiang.df)[1:2]=c("x","y")
mydat=data.frame(id=unique(sort(Zhejiang.df$id)))
mydat$rand=runif(length(mydat$id))

Zhejiang.map = ggplot(mydat) +
    geom_map(aes(map_id = id, fill = rand), color = "white", map = Zhejiang.df) +
    scale_fill_gradient(high = "darkgreen",low = "lightgreen") +
    expand_limits(Zhejiang.df) + coord_map() 
#添加地名
tmp = coordinates(Zhejiang.shp)
tmp = as.data.frame(tmp)
tmp$Names=Zhejiang.shp$NAME_2
tmp$Names = gsub('市','',Zhejiang.shp$NL_NAME_2)
Zhejiang.map + geom_text(aes(x = V1,y = V2,label = Names,size = 10,colour = "white",fontface=3),data = tmp)

#画嘉兴地图用网上地图
shape.dir = "F:/CHN_adm"
china.shp= readOGR(shape.dir, layer = "CHN_adm3")
str(china.shp, max.level=2)
names(china.shp@data)
china.shp$NAME_2

# Zhejiang.shp$NL_NAME_2
# Zhejiang.shp$VARNAME_2
jiaxing.shp = china.shp[china.shp$NAME_2=="Jiaxing",]
plot(jiaxing.shp)
jiaxing.shp=fortify(jiaxing.shp)
names(Zhejiang.df)
names(Zhejiang.df)[1:2]=c("x","y")
mydat=data.frame(id=unique(sort(Zhejiang.df$id)))
mydat$rand=runif(length(mydat$id))

Zhejiang.map = ggplot(mydat) +
  geom_map(aes(map_id = id, fill = rand), color = "white", map = Zhejiang.df) +
  scale_fill_gradient(high = "darkgreen",low = "lightgreen") +
  expand_limits(Zhejiang.df) + coord_map() 
#添加地名
tmp = coordinates(Zhejiang.shp)
tmp = as.data.frame(tmp)
tmp$Names=Zhejiang.shp$NAME_2
tmp$Names = gsub('市','',Zhejiang.shp$NL_NAME_2)
Zhejiang.map + geom_text(aes(x = V1,y = V2,label = Names,size = 10,colour = "white",fontface=3),data = tmp)


#用中国CDC给的地图
shape.dir = "F:/shp/shp"
china.shp= readOGR(shape.dir, layer = "quxian Polygon",stringsAsFactors=F)
str(china.shp, max.level=2)
names(china.shp@data)
china.shp$PYNAME[2480]
jxindex <- which(china.shp$CNTY_CODE %in% c(330402,330411,330421,330424,330481,330482,330483))

# Zhejiang.shp$NL_NAME_2
# Zhejiang.shp$VARNAME_2
jiaxing.shp = china.shp[jxindex,]
#plot(jiaxing.shp)
names(jiaxing.shp)
head(jiaxing.shp@data)
jiaxing.shp@data$NAME[jiaxing.shp$PYNAME == "Jiashan Xian"] <- "嘉善县" 
jiaxing.shp@data$NAME[jiaxing.shp$PYNAME == "Haiyan Xian"] <- "海盐县" 
jiaxing.shp@data$NAME[jiaxing.shp$PYNAME == "Tongxiang Shi"] <- "桐乡市" 
jiaxing.shp@data$NAME[jiaxing.shp$PYNAME == "Pinghu Shi"] <- "平湖市" 
jiaxing.shp@data$NAME[jiaxing.shp$PYNAME == "Haining Shi"] <- "海宁市" 
jiaxing.shp@data$NAME[jiaxing.shp$PYNAME == "Nanhu Qu"] <- "南湖区" 
jiaxing.shp@data$NAME[jiaxing.shp$PYNAME == "Xiuzhou Qu"] <- "秀洲区" 
jiaxing.df=fortify(jiaxing.shp)
names(jiaxing.df)[1:2]=c("x","y")


names(jiaxing.df)

mydat=data.frame(id=unique(sort(jiaxing.df$id)))
mydat$rand=runif(length(mydat$id))

jiaxing.map = ggplot(mydat) +
  geom_map(aes(map_id = id, fill = rand), color = "white", map =jiaxing.df) +
  scale_fill_gradient(high = "darkgreen",low = "lightgreen") +
  expand_limits(jiaxing.df) + coord_map() 
#添加地名
tmp = coordinates(jiaxing.shp)
tmp = as.data.frame(tmp)
tmp$Names=jiaxing.shp$NAME
#tmp$Names = gsub('市','',Zhejiang.shp$NL_NAME_2)
jiaxing.map + geom_text(aes(x = V1,y = V2,label = Names,size = 10,colour = "white",fontface=3),data = tmp)
