#cluster for Dec.

?agnes

library(cluster)

clu<-agnes(data3[3:21],metric="manhattan",stand=TRUE,
           method="ward")
bannerplot(clu)
?bannerplot
data3$clu<-cutree(clu,k=4)

data3_sub<-data3[data3$clu==4,]



library(fpc)

ins<-clara(data3[3:21],4)
ins$clustering
kmeans(data3[3:21],4)
?kmeans


library(kml3d)
?kml3d()



#对电子烟部分聚类

library(cluster)
library(stringr)
names(data3)
names(data3)[str_detect(names(data3),pattern = "^EC")]  #搜索所有以EC开头的变量名

data3[9:15][!complete.cases(data3[9:15]),]
data3_EC<-data3[c("ID",names(data3)[str_detect(names(data3),
                                               pattern = "^EC")])]
#data3_EC<-na.omit(data3_EC)
table(complete.cases(data3_EC))


#data3_EC<-subset(data3_EC,select=-EC_HARM)
str(data3_EC)
data3_EC$zEC_HEARD<-ifelse(data3_EC$EC_HEARD==2,1,0)
data3_EC$zEC_USE.LIFE<-ifelse(data3_EC$EC_USE.LIFE==2,1,0)
data3_EC$zEC_USE.PAST.MON<-ifelse(data3_EC$EC_USE.PAST.MON==2,1,0)
data3_EC$zEC_INT<-scale(data3_EC$EC_INT)
data3_EC$zEC_BF.TRY <-scale(data3_EC$EC_BF.TRY)
data3_EC$zEC_BF.EC<-scale(data3_EC$EC_BF.EC)

data3_EC[!complete.cases(data3_EC),]
hc<-hclust(dist(data3_EC[c("zEC_INT","zEC_BF.TRY","zEC_BF.EC")],method = "euclidean"))
plot(hc)
data3_EC$hclu<-cutree(hc,k=3)

library(NbClust)
nc<-NbClust(data3_EC[c("zEC_INT","zEC_BF.TRY","zEC_BF.EC")],distance  = "euclidean",min.nc=2,max.nc=10,method="average")
barplot(table(nc$Best.nc[1,]))
data3_EC$nclu<-hclust(dist(data3_EC[c("zEC_INT","zEC_BF.TRY","zEC_BF.EC")],method  = "euclidean"),method="average") %>% cutree(.,k=3)

clu<-agnes(data3_EC[c("zEC_INT","zEC_BF.TRY","zEC_BF.EC")],metric="euclidean",stand=TRUE,
           method="ward")
bannerplot(clu)
pltree(clu)
data3_EC$clu<-cutree(clu,k=3)


kclu<-kmeans(data3_EC[c("zEC_INT","zEC_BF.TRY","zEC_BF.EC")],3)
data3_EC$kclu<-kclu$cluster

ins<-data3_EC[c("ID","clu","kclu","hclu","nclu",
                names(data3)[str_detect(names(data3),
                                        pattern = "^EC_((?!(HARM))|(HARM\\.))")])]
dfm_data3_EC<-melt(ins,
                   id=c("ID","clu","kclu","hclu","nclu"))
ggplot(dfm_data3_EC, aes(variable, value,group=ID)) +
  geom_jitter(height=0.4,aes(color=factor(dfm_data3_EC$kclu)))


#比较各个聚类的差异
for(i in c("nclu","clu","kclu","hclu")){
  for(j in c("nclu","clu","kclu","hclu")){
    print("--------------------------------")
    print(paste(i,"\\",j))
    print(table(data3_EC[i][,1],data3_EC[j][,1]))
    print("--------------------------------")
  }
}
