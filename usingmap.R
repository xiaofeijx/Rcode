library(rgdal)
library(ggplot2)
library(rgeos)

shape.dir = "d:/chinamap"
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
