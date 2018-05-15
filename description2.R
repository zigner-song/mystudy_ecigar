
N<-length(data_try$ID)# N为所有纳入后续分析的被试量
data_Smoker.EC<-(subset(data_try,
                               select=c("ID","SmokerOrNot",variable_EC,variable_Demography)))
data_Smoker.EC[!complete.cases(data_Smoker.EC),]
manyNAs(data_Smoker.EC)
##Demography

if(F){  
  
###*Gender*  ###

  data_gender<-data.frame(table(data_try$GENDER))
  colnames(data_gender)<-c("Gender","Freq")
  data_gender$Prop<-data_gender[["Freq"]]/N*100
  data_gender

  
  ###*Age*  ###
  
  data_age<-data.frame(table(data_try$AGE))
  colnames(data_age)<-c("AGE","Freq")
  data_age$Prop<-data_age[["Freq"]]/N*100
  data_age
  
  
  ###*Major*  ###
  
  data_major<-data.frame(table(data_try$MAJOR))
  colnames(data_major)<-c("Major","Freq")
  data_major$Prop<-data_major[["Freq"]]/N*100
  data_major
  
  
  ###*Degree* ###
  
  data_degree<-data.frame(table(data_try$DEGREE))
  colnames(data_degree)<-c("Degree","Freq")
  data_degree$Prop<-data_degree[["Freq"]]/N*100
  data_degree
  
  
  #**Tobacco Use**
    -----------------
    
    ##Current Smokers##  
  #  *current smokers are defned as "smoked at least once in the past 30 days"*  
  #  <!--TU_LIFE-->
  #  TU_LIFE: Have you ever smoked a cigarette, even one or two puffs, in your life?
  data.frame(table(data_try$TU_LIFE))
  
  #<!--TU_PAST.MON-->
  #  <i><font face="Times New Roman"><small>TU_PAST.MON: </small><b>Have you smoked any cigarettes in the past month?</b></font></i>
    
  data.frame(table(data_try$TU_PAST.MON))
  
  
  #<!--TU_FREQ-->
  #  <i><font face="Times New Roman"><small>TU_FREQ:  </small><b>How often do you smoke?</b></font></i>
    
  ins<-data.frame(table(data_try$TU_FREQ))
  ins$Meanings<-c("Prefer Not to say","I smoke every day","I smoke 4-6 days per week","I smoke 2-3 days per week","I smoke once a week","less than once a week")
  ins
  remove(ins)
  
  
  
  #<!--TU_STOP-->
  #  TU_STOP:  </small><b>During the past 12 months, have you stopped smoking for one day or longer because you were trying to quit smoking?</b></font></i>
    
  data.frame(table(data_try$TU_STOP))
  
  
  #<!--TU_STOP_12Mon-->
  #  <i><font face="Times New Roman"><small>TU_STOP_12MON:  </small><b>During the past 12 months, have you tryed to quit smoking? if the prior answer is "YES"</b></font></i>
    
  data.frame(table(data_try$TU_STOP_23MON))
  
  
  #<!--TU_QUIT-->
  #  <i><font face="Times New Roman"><small>TU_QUIT:  </small><b>How likely are you going to quit in the next year? </b></font></i>
    
  histWithNormalCurve(data_try$TU_QUIT,main="How likely are you going to quit in the next year?",xlab="1-No Plan to Quit          10-Determine to Quit")  
  
  
  #<!--TU_40Y-->
  #  <i><font face="Times New Roman"><small>TU_40Y:  </small><b>How likely are you going to stay a smoker when you are 40?  </b></font></i>
    
  histWithNormalCurve(data_try$TU_40Y,main="How likely are you going to stay a smoker when you are 40? ",xlab="1-Absolutely          10-Absolutely Not")  
  
  
  #**Intentions**
  #  ------------
  #  <!--INT_TRY-->
  #  <i><font face="Times New Roman"><small>INT_TRY:  </small><b>Do you think you will try a cigarette soon?  </b></font></i>
    
  histWithNormalCurve(data_try$INT_TRY,main="How likely are you going to stay a smoker when you are 40?\n (For Non-Smokers Only) ",xlab="1-Absolutely Not          10-Absolutely")  
  
  
  #<!--INT_NEXT.Y-->
  #  <i><font face="Times New Roman"><small>INT_NEXT.Y:  </small><b>Do you think you will smoke a cigarette anytime in the next year?</b></font></i>
    
  histWithNormalCurve(data_try$INT_TRY,main="Do you think you will smoke a cigarette anytime in the next year?\n (For Non-Smokers Only)",xlab="1-Absolutely Not          10-Absolutely")  
  
  
  
  #<!--INT_BF.OFFER-->
  #  <i><font face="Times New Roman"><small>INT_BF.OFFER:  </small><b>If one of your best friends offered you a cigarette, would you smoke it?</b></font></i>
    
  histWithNormalCurve(data_try$INT_BF.OFFER,main="If one of your best friends offered you a cigarette, would you smoke it?\n (For Non-Smokers Only)",xlab="1-Absolutely Not          10-Absolutely")  
  
  
  
  #**E-cigarette**
  #  --------------------
  #  <!--EC_HEARD-->
  #  <i><font face="Times New Roman"><small>EC_HEARD:  </small><b>Have you ever heard of electronic cigarettes, often called e-cigarettes?</b></font></i>
    
  data.frame(table(data_try$EC_HEARD))
  
  
  #<!--EC_HARM-->
  #EC_HARM: Compared with a regular cigarette, how harmful do you think e-cigarettes are?  (1-less harmful; 2-same harmful; 3-more harmful)
    
  data.frame(table(data_try$EC_HARM))
  
  
  #<!--EC_USE.LIFE-->
  # EC_USE.LIFE:  </small><b>Have you ever used electronic cigarettes in your life?</b></font></i>
    
  data.frame(table(data_try$EC_USE.LIFE))
  
  
  #<!--EC_USE.PAST.MON-->
  #  <font face="Times New Roman"><i><small>EC_USE.PAST.MON:  </small>For <u><b>E-cigarrete users</b></u>:Have you used electronic cigarettes in the past month?</i></font>
    
  data.frame(table(data_try$EC_USE.PAST.MON))
  
  
  #<!--EC_BF.TRY-->
  #  <font face="Times New Roman"><i><small>EC_BF.TRY:  </small>For <u><b>Non-E-cigarrete users</b></u>:If one of your best friends were to offer you an e-cigarette, would you try it?</i></font>
    
  histWithNormalCurve(data_try$EC_BF.TRY,
                      main="If one of your best friends were to offer you an e-cigarette, would you try it?",
                      xlab="1-Absolutely Not          10-Absolutely")
  sapply(data_try["EC_BF.TRY"],FUN=mean,na.rm=T)
  
  
  #<!--EC_BF.EC-->
  #EC_BF.EC:  </small>For Non-E-cigarrete users:If one of your best friends were to offer you a flavored e-cigarette (chocolate, mint, apple, etc.), would you try it?</i></font>
    
  histWithNormalCurve(data_try$EC_BF.EC,
                      main="",
                      xlab="1-Absolutely Not          10-Absolutely") 
  sapply(data_try["EC_BF.EC"],FUN=dstats,na.rm=T)
  
  #<!--TU*EC-->
    
  data_Smoker.EC<-na.omit(subset(data_try,
                                 select=c("ID","SmokerOrNot",
                                          "GENDER","GRADE",
                                          "EC_HEARD","EC_HARM","EC_BF.TRY","EC_BF.EC")))
  
  
}  
  
  #########################################################
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #Smokers vs. Non-Smokers in E-cigarrete</font>

    ###<i><font face="Times New Roman">  Smoker * Heard </font></i>###
    
    
  table_Smoker.Heard<-xtabs(~SmokerOrNot+EC_HEARD,data=data_try)
  addmargins(table_Smoker.Heard,2)
  prop.table(table_Smoker.Heard,1)
  (chisq.Smoker.Heard<-chisq.test(table_Smoker.Heard))
  
  
  #是否吸烟-是否听说过电子烟  列联相关不显著</font>
    
    ###<i><font face="Times New Roman">  Smoker * Harm </font></i>###
    
    
  table_Smoker.Harm<-xtabs(~SmokerOrNot+EC_HARM,data=data_try)
  addmargins(table_Smoker.Harm,2)
  prop.table(table_Smoker.Harm,1)
  chisq.test(table_Smoker.Harm)
  
  
  #是否吸烟-认为电子烟危害大小  列联相关不显著</font>
    
    ### Smoker * Use.Life
    
  table_Smoker.Use.Life<-xtabs(~SmokerOrNot+EC_USE.LIFE,data=data_try)
  addmargins(table_Smoker.Use.Life,2)
  prop.table(table_Smoker.Use.Life,1)
  chisq.test(table_Smoker.Use.Life)
  
  
  
  #是否吸烟-是否曾吸食过电子烟  列联相关显著，吸烟者吸食电子烟的比率更高</font></p>
  
    
    ###Smoker * BF (TRY & FLAVOR) ###
  #  Smoker * BF.TRY
  
  summaryBy(EC_BF.TRY~SmokerOrNot,data=data_try,FUN=dstats)
  t.test(EC_BF.TRY~SmokerOrNot,data=data_try)
  
 
  #  Smoker * BF.EC
  
  summaryBy(EC_BF.EC~SmokerOrNot,data=data_try,FUN=dstats)
  t.test(EC_BF.EC~SmokerOrNot,data=data_try)
  
  #<font face="黑体" color="red">吸烟者更可能尝试好友提供的电子烟，不论口味</font>  
  #  <br/>
  #  <p><i><b>口味是否会影响人们尝试电子烟？</b></i></p>
    
  #  <!--计算相关性
  if(F){
  #cor(data_Smoker.EC$EC_BF.TRY[data_Smoker.EC$SmokerOrNot==1],data_Smoker.EC$EC_BF.EC[data_Smoker.EC$SmokerOrNot==1])
  #"For Smokers"
  cor.test(data_Smoker.EC$EC_BF.TRY[data_Smoker.EC$SmokerOrNot==1],data_Smoker.EC$EC_BF.EC[data_Smoker.EC$SmokerOrNot==1])
  
  #cor(data_Smoker.EC$EC_BF.TRY[data_Smoker.EC$SmokerOrNot==0],data_Smoker.EC$EC_BF.EC[data_Smoker.EC$SmokerOrNot==0])
  #"For Non-Smokers"
  cor.test(data_Smoker.EC$EC_BF.TRY[data_Smoker.EC$SmokerOrNot==0],data_Smoker.EC$EC_BF.EC[data_Smoker.EC$SmokerOrNot==0])
  
  }
    
    
  #summaryBy(EC_BF.TRY+EC_BF.EC~SmokerOrNot,data=data_Smoker.EC,FUN=mean)
  
  "因变量为尝试电子烟的可能性，自变量为 1.是否是烟民(SmokerOrNot) 2.电子烟的口味（普通烟草口味vs.非烟草口味）(variable)"
  #首先需要融合数据，标注组内变量
  dfm.Smoker.EC<-melt(subset(data_Smoker.EC,
                             select=c("ID","SmokerOrNot","GENDER","GRADE","EC_BF.TRY","EC_BF.EC")),
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
  
  
  if(F){
  #电子烟VS普通香烟 BF.OFFER
  t.test(data_try$INT_BF.OFFER,data_try$EC_BF.TRY,paired=T,na.rm=T)
  cor(data_try$INT_BF.OFFER,data_try$EC_BF.TRY,use="complete.obs")
  cor.test(data_try$INT_BF.OFFER,data_try$EC_BF.TRY,use="complete.obs")
  
  #结果表明，交互作用不显著(p=.136)，主效应显著：吸烟者更容易在朋友推荐下使用电子烟；如果是非香烟口味的电子烟则更容易使用。  </font>
    
  #For Smokers only in E-cigarrete and intentions
  
    
  #生成一个表 内容为Smokers在TU和EC上的表现
  data_SmokersOnly.EC<-subset(data_try,
                              SmokerOrNot=="Smoker",
                              select=c("ID","TU_FREQ","TU_STOP","TU_STOP_23MON","TU_QUIT","TU_40Y","EC_HEARD","EC_USE.LIFE","EC_BF.TRY","EC_BF.EC"))
  data_SmokersOnly.EC$TU_STOP_23MON[data_SmokersOnly.EC$TU_STOP==2]<-"No"
  #data_SmokersOnly.EC$TU_STOP_23MON[data_SmokersOnly.EC$TU_STOP==1]<-"???"
  #data_SmokersOnly.EC$ECerOrNot[data_SmokersOnly.EC$EC_USE.LIFE==2]<-1
  
  
  
  #电子烟使用者目前的用烟、戒烟情况
  
  summary.TU.EC<-summaryBy(TU_FREQ+TU_QUIT+TU_40Y~EC_USE.LIFE,
                           data=data_SmokersOnly.EC,
                           FUN=dstats)
  summary.TU.EC$N<-table(data_SmokersOnly.EC$EC_USE.LIFE)
  summary.TU.EC
  
  xtabs(~EC_USE.LIFE+TU_STOP,data=data_SmokersOnly.EC)
  
  if(T){
    t.test(TU_FREQ~EC_USE.LIFE,data=data_SmokersOnly.EC)
    t.test(TU_QUIT~EC_USE.LIFE,data=data_SmokersOnly.EC)
    t.test(TU_40Y~EC_USE.LIFE,data=data_SmokersOnly.EC)
  }
  
  
  
  #结果显示曾经使用过电子烟对被试平时与戒烟没有显著的影响
  
    

  #  <font face="Calibri">For Non-Smokers only in E-cigarrete and intentions</font>

  
  data_NonSmokersOnly.EC<-subset(data_try,SmokerOrNot==0,select=c("ID","SE_BEST.FRI","SE_PARTNER","SE_PARTY","SE_ALL.FRI","INT_TRY","INT_NEXT.Y","INT_BF.OFFER","EC_HEARD","EC_HARM","EC_USE.LIFE","EC_BF.TRY","EC_BF.EC"))
  
  #-->
    
  # Gender * EC

    
  data_Gender.EC<-subset(data_try,
                         GENDER =="Female"|GENDER=="Male",
                         select=c("ID","GENDER","SmokerOrNot","EC_HEARD","EC_HARM",
                                  "EC_USE.LIFE","EC_USE.PAST.MON","EC_BF.TRY","EC_BF.EC"))
  
  
  #性别×是否听说过电子烟/危害/曾经使用过电子烟/上月曾经使用过电子烟/朋友推荐下尝试(烟草口味/非烟草口味)
  
  print("HEARD")
  table.GENDER.EC_HEARD<-xtabs(~GENDER+EC_HEARD,data=data_Gender.EC)
  table.GENDER.EC_HEARD
  chisq.test(table.GENDER.EC_HEARD)
  
  print("HARM")
  table.GENDER.EC_HARM<-xtabs(~GENDER+EC_HARM,data=data_Gender.EC,na.action(na.omit))
  table.GENDER.EC_HARM
  chisq.test(table.GENDER.EC_HARM)
  
  print("use.life")
  table.GENDER.EC_USE.LIFE<-xtabs(~GENDER+EC_USE.LIFE,data=data_Gender.EC,na.action(na.omit))
  table.GENDER.EC_USE.LIFE
  chisq.test(table.GENDER.EC_USE.LIFE)
  
  print("use.last month")
  table.GENDER.EC_USE.PAST.MON<-xtabs(~GENDER+EC_USE.PAST.MON,data=data_Gender.EC,na.action(na.omit))
  table.GENDER.EC_USE.PAST.MON
  chisq.test(table.GENDER.EC_USE.PAST.MON)
  
  
  print("Best Friend's offer(TRY=cigar flavor, EC= non-cigar flavor)")
  summaryBy(EC_BF.TRY+EC_BF.EC~GENDER,
            data=data_Gender.EC,FUN=dstats,na.rm=T)
  t.test(EC_BF.TRY~GENDER,data=data_Gender.EC)
  t.test(EC_BF.EC~GENDER,data=data_Gender.EC)
  
  #"Best Friend's offer~ flavor * Gender"
  dfm.Gender.EC<-melt(subset(data_Gender.EC,select=c("ID","GENDER","EC_BF.TRY","EC_BF.EC")),id=c("ID","GENDER"))
  dfm.Gender.EC<-na.omit(dfm.Gender.EC)
  manova.Gender.EC<-aov(value~GENDER*variable+Error(ID/variable),dfm.Gender.EC)
  summary(manova.Gender.EC)
  with(dfm.Gender.EC,interaction.plot(variable,GENDER,value,type="b",col=c("red","blue"),pch=c(16,19),xlab=" \n EC_BF.TRY=烟草口味 EC_BF.EC=非烟草口味"))
  
  #boxplot(value ~ variable*GENDER,data=dfm.Gender.EC,col=c("red","blue"),xlab=" \n EC_BF.TRY=烟草口味 EC_BF.EC=非烟草口味")
  
  #男性有更多的人曾尝试过电子烟，在朋友提供下更可能去吸食电子烟（烟草口味&非烟草口味）
    
    
  #  <font face="Calibri">Grade * EC</font>
  #  --------
    
  data_GRADE.EC<-subset(data_try,data_try$DEGREE!="Prefer Not to Say",
                        select=c("ID","GENDER","SmokerOrNot","DEGREE","EC_HEARD","EC_HARM",
                                 "EC_USE.LIFE","EC_USE.PAST.MON","EC_BF.TRY","EC_BF.EC"))
  data_GRADE.EC$GRADE[data_GRADE.EC$DEGREE=="Under College"
                      |data_GRADE.EC$DEGREE=="College"
                      |data_GRADE.EC$DEGREE=="Bachlor"]<-0
  data_GRADE.EC$GRADE[data_GRADE.EC$DEGREE=="Master"|data_GRADE.EC$DEGREE=="Doctor"]<-1
  data_GRADE.EC$GRADE<-factor(data_GRADE.EC$GRADE,levels=c(0,1),labels=c("Undergraduated","Graduated"))
  
  
  

  table.GRADE.EC_HEARD<-xtabs(~GRADE+EC_HEARD,data=data_GRADE.EC)
  table.GRADE.EC_HEARD
  chisq.test(table.GRADE.EC_HEARD)
  
  print("HARM")
  table.GRADE.EC_HARM<-xtabs(~GRADE+EC_HARM,data=data_GRADE.EC,na.action(na.omit))
  table.GRADE.EC_HARM
  chisq.test(table.GRADE.EC_HARM)
  
  print("use.life")
  table.GRADE.EC_USE.LIFE<-xtabs(~GRADE+EC_USE.LIFE,data=data_GRADE.EC,na.action(na.omit))
  table.GRADE.EC_USE.LIFE
  chisq.test(table.GRADE.EC_USE.LIFE)
  
  print("use.last month")
  table.GRADE.EC_USE.PAST.MON<-xtabs(~GRADE+EC_USE.PAST.MON,data=data_GRADE.EC,na.action(na.omit))
  table.GRADE.EC_USE.PAST.MON
  chisq.test(table.GRADE.EC_USE.PAST.MON)
  
  
  print("Best Friend's offer(TRY=cigar flavor, EC= non-cigar flavor)")
  summaryBy(EC_BF.TRY+EC_BF.EC~GRADE,
            data=data_GRADE.EC,FUN=dstats,na.rm=T)
  t.test(EC_BF.TRY~GRADE,data=data_GRADE.EC)
  t.test(EC_BF.EC~GRADE,data=data_GRADE.EC)
  
  #"Best Friend's offer~ flavor * Gender"
  dfm.GRADE.EC<-melt(subset(data_GRADE.EC,select=c("ID","GRADE","EC_BF.TRY","EC_BF.EC")),id=c("ID","GRADE"))
  dfm.GRADE.EC<-na.omit(dfm.GRADE.EC)
  manova.GRADE.EC<-aov(value~GRADE*variable+Error(ID/variable),dfm.GRADE.EC)
  summary(manova.GRADE.EC)
  with(dfm.GRADE.EC,interaction.plot(variable,GRADE,value,type="b",col=c("red","blue"),pch=c(16,19),xlab=" \n EC_BF.TRY=烟草口味 EC_BF.EC=非烟草口味"))
  
  #boxplot(value ~ variable*GENDER,data=dfm.Gender.EC,col=c("red","blue"),xlab=" \n EC_BF.TRY=烟草口味 EC_BF.EC=非烟草口味")
  
  #<br/><font face="黑体" color="red">本科以上学历在同学建议下更多的可能吸食电子烟</font> 
    
  #  <font face="Calibri">Smoker × Gender × Grade × EC</font>
 
  #data_EC<-data_Grade.EC
  dfm.EC<-melt(subset(data_GRADE.EC,select=c("ID","SmokerOrNot","GENDER","GRADE","EC_BF.TRY","EC_BF.EC")),id=c("ID","SmokerOrNot","GENDER","GRADE"))
  dfm.EC<-na.omit(dfm.EC)
  manova.EC<-aov(value~(SmokerOrNot*GENDER*GRADE*variable)+Error(ID/variable)+(SmokerOrNot*GENDER*GRADE),data=dfm.EC)
  summary(manova.EC)
  #with(dfm.EC,interaction.plot(variable,SmokerOrNot,GENDER,GRADE,value,type="b",col=c("red","blue"),pch=c(16,19),xlab=" \n EC_BF.TRY=烟草口味 EC_BF.EC=非烟草口味"))
  boxplot(value ~ SmokerOrNot*GENDER*GRADE*variable,data=dfm.EC,xlab=" \n EC_BF.TRY=烟草口味 EC_BF.EC=非烟草口味")
  #TukeyHSD(manova.EC,c("GENDER","GRADE"))
  
  }
  
  ##<!--非烟民对于电子烟的口味的意向是否显著
  ##大纲
  ##1.背景信息
  ##2.特殊效应（每一个之间的关系）
  ##3.总体的预测
  
  
  ##有什么问题，这些问题和什么有关，人群
  
  ##总体上
  ##-->