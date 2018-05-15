#2017-12  
#预处理
#人口统计学的缺失和别的区分对待


#EC_HEARD和之后的题用于筛查


data3<-read.table("2017-12.txt",header=T,stringsAsFactors = T,sep=";")
names(data3)<-c("ID","version",variable_ATT,
                "EC_HEARD","EC_HARM","EC_USE.LIFE","EC_USE.PAST.MON","EC_INT","EC_BF.TRY","EC_BF.EC",
                variable_TU[-length(variable_TU)],
                "GENDER","AGE","ETHNIC","DEGREE")

data3$AGE[data3$AGE==0]<-NA
data3[data3==0]<-10

#删除重复个案
data3<-data3[!duplicated(data3),]


data3[!complete.cases(data3),]
data3[manyNAs(data3,0.1),]

data3 <- data3[-manyNAs(data3,0.1),] #删除缺失变量的个数多于1/10的个案

nrow(data3)


{
#Demogrphic
#GENDER AGE ETHNIC MAJOR DEGREE
#gender  0-prefer not to answer 1-Male 2-Female
#data3$GENDER[data3$GENDER==3]<-NA
data3$GENDER <- factor(data3$GENDER,  
                          levels = c(1,2,3), 
                          labels = c("Male","Female","Perfer.Not.to.Say"))

#ETHNIC 1-HAN,2-OTHER,3-PREFER NOT 2 SAY
#data3$ETHNIC[data3$ETHNIC==3]<-NA
data3$ETHNIC <- factor(data3$ETHNIC,levels=1:3,
                          labels=c("HAN","MINORITY","Perfer.Not.to.Say"))


#DEGREE
#data_try$DEGREE[data_try$DEGREE==0]<-NA
#data3$DEGREE[data3$DEGREE==6]<-NA
data3$DEGREE<- factor(data3$DEGREE, levels= c(1,2,3,4,5,6),
                      labels= c("Under College","College","Bachlor","Master","Doctor","Prefer.Not.to.Say"))

} #编码人口统计学变量

{#编码TU变量
#编码TU数据
#TU_LIFE: Have you ever smoked a cigarette, even one or two puffs, in your life?
#data3$TU_LIFE<-factor(data3$TU_LIFE,levels=c(2,1), labels=c("No","Yes"))

#TU_PAST.MONTH

#data3$TU_PAST.MON<-factor(data3$TU_PAST.MON,levels=c(2,1), labels=c("No","Yes"))

#TU_FREQ
data3$TU_FREQ[data3$TU_FREQ==6]<-NA
#levels=c(0,1,2,3,4,5), labels=c("Prefer Not to say","I smoke every day","I smoke 4-6 days per week","I smoke 2-3 days per week","I smoke once a week","less than once a week"))

#TU_STOP
#data3$TU_STOP[data3$TU_STOP %in% c(3,4)]<-NA
data3$TU_STOP <- factor(data3$TU_STOP,levels=c(1,2,3,4), 
                           labels=c("Yes","No","Not.Sure","Prefer.Not.to.Say"))
ins<-dummy.code(data3$TU_STOP)
colnames(ins)<-(colnames(dummy.code(data3$TU_STOP)) %>% paste("TU_STOP",.,sep="."))
data3<-cbind(data3,ins)

#TU_STOP_12MON
#data3$TU_STOP_23MON[data3$TU_STOP_23MON %in% c(3,4)]<-NA
data3$TU_STOP_23MON <- factor(data3$TU_STOP_23MON,levels=c(1,2,3,4), 
                                 labels=c("Yes","No","Not.Sure","Prefer.Not.to.Say"))
ins<-dummy.code(data3$TU_STOP_23MON)
colnames(ins)<-(colnames(dummy.code(data3$TU_STOP_23MON)) %>% paste("TU_STOP_23MON",.,sep="."))
data3<-cbind(data3,ins)
#TU_QUIT: :How likely are you going to quit in the next year?

#TU_40Y: How likely are you going to stay a smoker when you are 40?

#SmokerOrNot
#data3$SmokerOrNot<-ifelse(data3$TU_PAST.MON == "Yes",1,0)
#data3$SmokerOrNot<-factor(data3$SmokerOrNot,levels=0:1,labels=c("NonSmoker","Smoker"))
}#编码TU变量

{
  #EC_HEARD
  #data3$EC_HEARD<-factor(data3$EC_HEARD,levels=c(2,1), labels=c("No","Yes"))
  #EC_HARM
  #EC_HARM 4-don't know 1-less harmful  2-as harmful  3-more harmful
  
  #data3$EC_HARM[data3$EC_HARM==4]<-NA
  data3$EC_HARM<-factor(data3$EC_HARM,levels=1:4,labels=c("less.harmful","as.harmful","more.harmful","Not.Sure"))
  ins<-dummy.code(data3$EC_HARM)
  colnames(ins)<-(colnames(dummy.code(data3$EC_HARM)) %>% paste("EC_HARM",.,sep="."))
  data3<-cbind(data3,ins)
  #EC_USE   .LIFE  .PAST.MON
  #data3$EC_USE.LIFE<-factor(data3$EC_USE.LIFE,levels=c(2,1), labels=c("No","Yes"))
  #data3$EC_USE.PAST.MON<-factor(data3$EC_USE.PAST.MON,levels=c(2,1), labels=c("No","Yes"))
  #EC_BF.TRY  1：absolutely no   10：absolutely yes
  #EC_BF.EC   1：absolutely no   10：absolutely yes
  
  #补NA
  data3["EC_INT"] <- centralImputation(data3["EC_INT"]) #用中位数补缺失值
  data3["EC_BF.TRY"]<-centralImputation(data3["EC_BF.TRY"])
  data3["EC_BF.EC"]<-centralImputation(data3["EC_BF.EC"])
  
}#编码EC变量

#data3 <- data3[-manyNAs(data3,0.15),] #删除在过多选项中选择“不愿回答”的被试(不多于4题),



data3<-subset(data3,!(data3$TU_LIFE==2 & data3$TU_PAST.MON==1)) #删除前后回答不一致的被试
data3<-subset(data3,!(data3$EC_HEARD==2 & (data3$EC_USE.LIFE==1 | data3$EC_USE.PAST.MON==1)))
data3<-subset(data3,!(data3$EC_USE.LIFE==2 & data3$EC_USE.PAST.MON==1))
str(data3)
data3[table(data3$ID)>1,]
nrow(data3)
write.csv(data3,"data3.csv")
###########################################
#
#       样本分布
#
###########################################

if(T){
###  Missing Data
data3[!complete.cases(data3),] %>% nrow;data3 %>% nrow
(data3[!complete.cases(data3),] %>% nrow)/(data3 %>% nrow)

table(data3$GENDER,useNA = "always")
##  demography
for(i in c("GENDER","ETHNIC","DEGREE","AGE")){
  print("-------------------------------------")
  print(i)
  if(i == "AGE"){
    ggplot(data3,aes(AGE))+geom_histogram(bins = 30)
    print(dstats(data3$AGE))
    print(table(data3[i],useNA = "always"))
    
  } else {
    print(table(data3[i],useNA = "always"))
    print(table(data3[i],useNA = "always")%>% prop.table(.))
    }
  
  print("-------------------------------------")
}



## Tobacco Use
for(i in variable_TU[-length(variable_TU)]){
  print("--------------------------------------")
  print(i)
  print(table(data3[i]))
  print(table(data3[i]) %>% prop.table())
  print("--------------------------------------")
}


p1<-ggplot(data3,aes(TU_QUIT))+geom_histogram(binwidth = 1)
p2<-ggplot(data3,aes(TU_FREQ))+geom_histogram(binwidth = 1)
p3<-ggplot(data3,aes(TU_40Y))+geom_histogram(binwidth = 1) #本题由于收取了大量40岁以上的被试，需考虑
lay_out(list(p1, 1, 1:3),
        list(p2,2,1:2),list(p3,2,3))

##ATT


p1<-ggplot(data3,aes(eval(parse(text=variable_ATT[1]))))+
  geom_histogram(binwidth = 1)+xlab(variable_ATT[1])
p2<-ggplot(data3,aes(eval(parse(text=variable_ATT[2]))))+
  geom_histogram(binwidth = 1)+xlab(variable_ATT[2])
p3<-ggplot(data3,aes(eval(parse(text=variable_ATT[3]))))+
  geom_histogram(binwidth = 1)+xlab(variable_ATT[3])
p4<-ggplot(data3,aes(eval(parse(text=variable_ATT[4]))))+
  geom_histogram(binwidth = 1)+xlab(variable_ATT[4])
p5<-ggplot(data3,aes(eval(parse(text=variable_ATT[5]))))+
  geom_histogram(binwidth = 1)+xlab(variable_ATT[5])
p6<-ggplot(data3,aes(eval(parse(text=variable_ATT[6]))))+
  geom_histogram(binwidth = 1)+xlab(variable_ATT[6])

lay_out(list(p1,1,1),list(p2,1,2),list(p3,1,3),
        list(p4,2,1),list(p5,2,2),list(p6,2,3))


##EC

for(i in c(variable_EC,"EC_INT")){
  print("--------------------------------------")
  print(i)
  print(t(table(data3[i])))
  print(table(data3[i]) %>% prop.table())
  print("--------------------------------------")
}

p1<-ggplot(data3,aes(EC_INT))+
  geom_histogram(binwidth = 1)+xlab("EC_INT")
p2<-ggplot(data3,aes(EC_BF.TRY))+
  geom_histogram(binwidth = 1)+xlab("EC_BF.TRY")
p3<-ggplot(data3,aes(EC_BF.EC))+
  geom_histogram(binwidth = 1)+xlab("EC_BF.AJI")
lay_out(list(p1, 1, 1:2),
        list(p2,2,1),list(p3,2,2))


} #描述统计


cor.test(data3$EC_INT[data3$clu==2]
    ,data3$TU_QUIT[data3$clu==2]
    ,use = "complete.ob")

ggplot(data3,aes(EC_INT,TU_QUIT))+geom_jitter(aes(color=factor(data3$clu)),size=2)
ggplot(data3,aes(EC_INT,TU_QUIT))+geom_jitter(aes(color=factor(ins$clustering)),size=2)
ggplot(data3[data3$clu==5,],aes(EC_INT,TU_QUIT))+geom_jitter()

cor(data3[names(data3)[str_detect(names(data3),
                                  "(^ATT)|((^EC_)(?!HARM)|(HARM-))|(^TU_)(?!STOP)")]],
    use="complete.obs") %>% as.matrix() %>% write.csv(.,file="ins2.csv")
ins<-data3[names(data3)[str_detect(names(data3),
                        pattern = "(^ATT)|((^EC_)(?!HARM)|(HARM-))|(^TU_)(?!STOP)")]]
cor.ins<-corr.test(ins)
cor.ins$r %>% write.csv(.,file="corr.csv")
cor.ins$p %>% write.csv(.,file="corp.csv")
names(data3)[str_detect(names(data3),"(^ATT)|((^EC_)(?!HARM)|(HARM-))|(^TU_)(?!STOP)")]
str(data3)

######################################
#
#              cluster
#
######################################
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


#####################################################################
if(T){
  ###  Missing Data
  data3_sub[!complete.cases(data3_sub),] %>% nrow;data3_sub %>% nrow
  (data3_sub[!complete.cases(data3_sub),] %>% nrow)/(data3_sub %>% nrow)
  
  
  ##  demography
  for(i in c("GENDER","ETHNIC","DEGREE","AGE")){
    print("-------------------------------------")
    print(i)
    if(i == "AGE"){
      ggplot(data3_sub,aes(AGE))+geom_histogram(bins = 30)
      print(dstats(data3_sub$AGE))
    } else {print(table(data3_sub[i]))}
    
    print("-------------------------------------")
  }
  
  
  
  ## Tobacco Use
  for(i in variable_TU[-length(variable_TU)]){
    print("--------------------------------------")
    print(i)
    print(table(data3_sub[i]))
    print("--------------------------------------")
  }
  
  
  p1<-ggplot(data3_sub,aes(TU_QUIT))+geom_histogram(binwidth = 1)
  p2<-ggplot(data3_sub,aes(TU_FREQ))+geom_histogram(binwidth = 1)
  p3<-ggplot(data3_sub,aes(TU_40Y))+geom_histogram(binwidth = 1) #本题由于收取了大量40岁以上的被试，需考虑
  lay_out(list(p1, 1, 1:3),
          list(p2,2,1:2),list(p3,2,3))
  
  ##ATT
  
  
  p1<-ggplot(data3_sub,aes(eval(parse(text=variable_ATT[1]))))+
    geom_histogram(binwidth = 1)+xlab(variable_ATT[1])
  p2<-ggplot(data3_sub,aes(eval(parse(text=variable_ATT[2]))))+
    geom_histogram(binwidth = 1)+xlab(variable_ATT[2])
  p3<-ggplot(data3_sub,aes(eval(parse(text=variable_ATT[3]))))+
    geom_histogram(binwidth = 1)+xlab(variable_ATT[3])
  p4<-ggplot(data3_sub,aes(eval(parse(text=variable_ATT[4]))))+
    geom_histogram(binwidth = 1)+xlab(variable_ATT[4])
  p5<-ggplot(data3_sub,aes(eval(parse(text=variable_ATT[5]))))+
    geom_histogram(binwidth = 1)+xlab(variable_ATT[5])
  p6<-ggplot(data3_sub,aes(eval(parse(text=variable_ATT[6]))))+
    geom_histogram(binwidth = 1)+xlab(variable_ATT[6])
  
  lay_out(list(p1,1,1),list(p2,1,2),list(p3,1,3),
          list(p4,2,1),list(p5,2,2),list(p6,2,3))
  
  
  ##EC
  
  for(i in c(variable_EC,"EC_INT")){
    print("--------------------------------------")
    print(i)
    print(t(table(data3_sub[i])))
  }
  
  p1<-ggplot(data3_sub,aes(EC_INT))+
    geom_histogram(binwidth = 1)+xlab("EC_INT")
  p2<-ggplot(data3_sub,aes(EC_BF.TRY))+
    geom_histogram(binwidth = 1)+xlab("EC_BF.TRY")
  p3<-ggplot(data3_sub,aes(EC_BF.EC))+
    geom_histogram(binwidth = 1)+xlab("EC_BF.AJI")
  lay_out(list(p1, 1, 1:2),
          list(p2,2,1),list(p3,2,2))
  
  
} #描述统计

variable_ATT
for(i in names(data3)){
  t<-paste(i,"~clu",sep="")
  fml<-eval(parse(text=t))
  print("----------------------")
  print(summaryBy(fml,data3,na.rm=T))
  print("----------------------")
}

