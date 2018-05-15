#total


data_SmokerOrNot<-subset(data_try,
                         select=c("ID","SmokerOrNot","GENDER","GRADE","EC_BF.TRY","EC_BF.EC"))
data_SmokerOrNot<-na.omit(data_SmokerOrNot)
#"因变量为尝试电子烟的可能性，自变量为 1.是否是烟民(SmokerOrNot) 2.电子烟的口味（普通烟草口味vs.非烟草口味）(variable)"
#首先需要融合数据，标注组内变量
dfm.Smoker.EC<-melt(data_SmokerOrNot,
                    id=c("ID","SmokerOrNot","GENDER","GRADE"),
                    variable.name = "flavor")
#删去4名女性吸烟者
dfm.Smoker.EC<-dfm.Smoker.EC[!(dfm.Smoker.EC$GENDER=="Female" & dfm.Smoker.EC$SmokerOrNot=="Smoker"),]



manova.Smoker.EC.BF<-aov(value~flavor*SmokerOrNot*GRADE*GENDER
                         +Error(ID/flavor),dfm.Smoker.EC)

summary(manova.Smoker.EC.BF)
      
      with(dfm.Smoker.EC,
           interaction.plot(flavor,SmokerOrNot,value,
                            type="b",col=c("red","blue"),pch=c(16,19),
                            xlab="0 - Non-Smokers;  1 - Smokers \n EC_BF.TRY=烟草口味 EC_BF.EC=非烟草口味"))
      
      
      boxplot(value ~ flavor*SmokerOrNot*GENDER,data=dfm.Smoker.EC,col=c("red","blue"),xlab="0 - Non-Smokers;  1 - Smokers \n EC_BF.TRY=烟草口味 EC_BF.EC=非烟草口味")
      
      summary_EC.total<-summaryBy(value~GENDER+flavor+GRADE+SmokerOrNot,data=dfm.Smoker.EC,FUN=dstats)
      
      ins<-unique(summary_EC.total[c("GENDER","GRADE","SmokerOrNot")])
      ins$version<-paste(ins$GENDER,ins$GRADE,ins$SmokerOrNot,sep="_")
      summary_EC.total<-merge(ins,summary_EC.total,id=c("GENDER","GRADE","SmokerOrNot"))
      rm(ins)
      summary_EC.total$version<-factor(summary_EC.total$version)
      summary_EC.total$value.n[summary_EC.total$value.n<10]<-NA
      summary_EC.total<-na.omit(summary_EC.total)


  #plot
  ggplot(summary_EC.total,aes(x=flavor,y=value.mean))+
    geom_text(aes(label=version),size=4)+
    geom_line(size=1,
              aes(group=version,
                  color=GENDER,
                  linetype=SmokerOrNot))+
    geom_point(aes(shape=GRADE),size=4)+
    geom_errorbar(aes(ymax=value.mean+value.sd/sqrt(value.n-1),
                      ymin=value.mean-value.sd/sqrt(value.n-1),
                      color=summary_EC.total$GENDER),width=0.05)+
    theme_classic()
#其他两题在GENDER，GRADE上没有差异，说明各组在认知上没有差异，却造成了INTENTION的差异，
#其原因可能是social norm

