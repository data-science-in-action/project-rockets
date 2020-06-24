#迁出地图
library(usethis)
library(devtools)
library(REmap)
options(remap.js.web = T)
options(remap.ak = "9H3FoljQv8CGKeWWrxtFVNEGCTd8Ui8z" )   
destin <- read.csv(file="./data/destination.csv",header=F)
names(destin) <- c("names","values")
markLine_data <- data.frame(origin=rep("武汉",10),
                            destination=destin[1:10,1],
                            color=rep("#fff",10)
)
markPoint_data <- markLine_data[markLine_data!=""]
markPoint_data1 <- markPoint_data[which(markPoint_data!="武汉"&markPoint_data!="#fff")]
remapC(destin, title="2020年1月10日至2020年2月29日 武汉人口迁出图", subtitle="前10目标省份",
       theme=get_theme(theme="Dark",  #背景颜色
                       lineColor = "#FFFFFF",  #线条颜色
                       titleColor = "#FFFFFF",  #标题颜色
                       pointShow = T,  #是否展示各省会城市所在点，设置为True时展示
       ),
       color=c('#CD0000','#FFEC8B'), #颜色渐变方案  
       markLineData=markLine_data,   
       markLineTheme=markLineControl(color="white", 
                                     lineWidth=2, #线条宽度
                                     lineType="dashed"   #线条形状：虚线
       ),  #对图中线的样式进行调整
       markPointData=markPoint_data1,
       markPointTheme=markPointControl(symbolSize=13,  #调整点的大小
                                       effect=T,     #调整点是否显示动态效果
                                       effectType="scale",    #调整点的形状
                                       color="white"
       )  #对图中点的样式进行调整
)

#疫情地图
library(REmap)
data <- read.csv(file="./data/data.csv",header=F)
names(data) <- c("names","values")
remapC(data,
       maptype = 'china',
       color = c('#CD0000','#FFEC8B'),   
       theme = get_theme("dark"),
       title = "全国疫情地图",
       subtitle="2月29日累计确诊人数",
)

#相关系数图
data<-read.csv("./data/new_correlation.csv")
attach(data)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(ggpmisc)
theme_set(ggpubr::theme_pubr()+
                  theme(legend.position = "top"))
library(ggpubr)
p1<- ggscatter(data, x = "mig", y = "distance",color = "black",
               add = "reg.line",conf.int = TRUE,    
               add.params = list(fill = "lightgray")
)+
        stat_cor(method = "pearson", 
                 label.x = 3, label.y = 30)
p2<- ggscatter(data, x = "mig", y = "train",color = "black",
               add = "reg.line",conf.int = TRUE,    
               add.params = list(fill = "lightgray")
)+
        stat_cor(method = "pearson", 
                 label.x = 3, label.y = 30)
p3<- ggscatter(data, x = "mig", y = "GDP",color = "black",
               add = "reg.line",conf.int = TRUE,    
               add.params = list(fill = "lightgray")
)+
        stat_cor(method = "pearson", 
                 label.x = 3, label.y = 30)
p4<- ggscatter(data, x = "mig", y = "population",color = "black",
               add = "reg.line",conf.int = TRUE,    
               add.params = list(fill = "lightgray")
)+
        stat_cor(method = "pearson", 
                 label.x = 3, label.y = 30)
ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,labels=c("A","B","C","D"))

#分类图
data <- read.csv("./data/new_correlation.csv")
library(ggplot2)
library(ggrepel)
ggplot(data)+ geom_point(aes(lag, ccf), color="grey", size=3)+
        geom_label_repel(aes(lag, ccf, fill=factor(cluster), 
                             label=province), fontface="bold", color="white", 
                         box.padding=unit(0.35, "lines"), point.padding=unit(0.5, "lines"), 
                         segment.colour = "grey50")+ theme_classic(base_size = 16)

#滞后地图
library(jsonlite)
library(rjson)
library(RJSONIO)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(plyr)
library(dplyr)
library(sf)
library(geojsonio)
library(sp)
library(ggthemes)
library(rgdal)
x<-readOGR("./data/省级行政区.shp",stringsAsFactors=FALSE,encoding="UTF-8")
prov_map<-fortify(x)
map=read.csv("./data/map.csv")
totalp=data.frame(id=map$id,pnum=map$lag,
                  Timelag=cut(map$lag,breaks=c(0,11,21,30),
                              labels=c("<=10","11-20",">=21"),
                              order = TRUE,include.lowest = T,right = F))
ttp<-merge.data.frame(prov_map,totalp,by.prov_map="id",by.totalp="id")
ggplot()+
        geom_polygon(data=ttp,aes(x=long,y=lat,group=group,
                                  fill=Timelag),colour="black",size=0.25)+
        scale_fill_manual(values = brewer.pal(3,"Blues"))


#相关系数地图
library(jsonlite)
library(rjson)
library(RJSONIO)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(plyr)
library(dplyr)
library(sf)
library(geojsonio)
library(sp)
library(ggthemes)
library(rgdal)
x<-readOGR("./data/省级行政区.shp",stringsAsFactors=FALSE,encoding="UTF-8")
prov_map<-fortify(x)
map=read.csv("./data/map.csv")
totalp1=data.frame(id=map$id,pnum=map$ccf,
                   correlation=cut(map$ccf,breaks=c(0,0.3,0.4,0.5,0.6,1),
                                   labels=c("<0.3","0.3-0.4","0.4-0.5","0.5-0.6",">=0.6"),
                                   order = TRUE,include.lowest = T,right = F))

ttp1<-merge.data.frame(prov_map,totalp1,by.prov_map="id",by.totalp1="id")

ggplot()+
        geom_polygon(data=ttp1,aes(x=long,y=lat,group=group,
                                   fill=correlation),colour="black",size=0.25)+
        scale_fill_manual(values = brewer.pal(5,"Greens"))
