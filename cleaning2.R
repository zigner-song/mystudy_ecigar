#library()
#source('E:/R/corcoef_test.R', encoding = 'UTF-8')

data1=read.csv("data1.csv",header = T)
data2=read.csv("data2.csv",header = T)
data1$round<-1
data2$round<-2

data<-rbind(data1,data2)
rm(data1);rm(data2)
data$ID<-data$ID+ifelse(data$round==2,1000,0)


#data$valid[data$times>=300|is.na(data$time)] <- T
data$valid[data$times<300] <- F
data$valid[is.na(data$valid)==T]<-T


data[data=="(跳过)"]<-NA
data[data=="(空)"]<-NA
data[data=="-3"]<-NA
data[data=="<NA>"]<-NA
#删除重复的题
data<-subset(data,select = - EC_BF.TRY2)
#save(data)

data_try<-data

#Demogrphic
  #GENDER AGE ETHNIC MAJOR DEGREE
  #gender  0-prefer not to answer 1-Male 2-Female
  #data_try$GENDER[data_try$GENDER==0]<-NA
  data_try$GENDER <- factor(data_try$GENDER,  
                            levels = c(1,2,0), 
                            labels = c("Male","Female",NA))
  
  
  #AGE 1: 18-25 2:25-40 3:41-60 4:60+ 5: PREFER NOT 2 SAY
  data_try$AGE<- factor(data_try$AGE, levels= 1:5, labels= c("18-25","25-40","41-60","60+","Prefer Not to Say"))
  #data_try$AGE[data_try$GENDER==5]<-NA
  
  #ETHNIC 1-HAN,2-OTHER,3-PREFER NOT 2 SAY
  data_try$ETHNIC <- factor(data_try$ETHNIC,levels=1:3,
                            labels=c("HAN","MINORITY","Prefer Not to Say"))
  data_try$ETHNIC[data_try$ETHNIC==3]<-NA
  
  #MAJOR
  data_try$MAJOR<- factor(data_try$MAJOR, levels= 1:7, 
                          labels= c("Social Science","Humanity","Natural Science","Engineer","Art","Others","Prefer Not to Say"))
  #data_try$MAJOR[data_try$MAJOR==7]<-NA
  
  #DEGREE
  #data_try$DEGREE[data_try$DEGREE==0]<-NA
  data_try$DEGREE<- factor(data_try$DEGREE, levels= c(1,2,3,4,5,6), labels= c("Under College","College","Bachlor","Master","Doctor","Prefer Not to Say"))
  data_try$DEGREE[data_try$DEGREE=="Prefer Not to Say"]<-NA
  
  data_try$GRADE[data_try$DEGREE %in% c("Under College","College","Bachlor")]<-0
  data_try$GRADE[data_try$DEGREE %in% c("Master","Doctor")]<-1
  data_try$GRADE[is.na(data_try$DEGREE)]<-NA
  data_try$GRADE<-factor(data_try$GRADE, levels=c(0,1), labels=c("Undergraduate","Graduated"))
  
  variable_Demography<-c("GENDER","AGE","ETHNIC","MAJOR","DEGREE","GRADE")
  

#编码TU数据
  variable_TU<-c("TU_LIFE","TU_PAST.MON","TU_FREQ","TU_STOP","TU_STOP_23MON","TU_QUIT","TU_40Y","SmokerOrNot")
  #TU_LIFE: Have you ever smoked a cigarette, even one or two puffs, in your life?
  data_try$TU_LIFE<-factor(data_try$TU_LIFE,levels=c(2,1), labels=c("No","Yes"))
  
  #TU_PAST.MONTH
  #data_try$TU_PAST.MON[data_try$TU_LIFE==1]<-"No"
  data_try$TU_PAST.MON<-factor(data_try$TU_PAST.MON,levels=c(2,1), labels=c("No","Yes"))
  
  #TU_FREQ
  #data_try$TU_FREQ[data_try$TU_FREQ==6]<-0
  #data_try$TU_FREQ<-factor(data_try$TU_FREQ,levels=c(0,1,2,3,4,5), labels=c("Prefer Not to say","I smoke every day","I smoke 4-6 days per week","I smoke 2-3 days per week","I smoke once a week","less than once a week"))
  
  #TU_STOP
  data_try$TU_STOP[data_try$TU_STOP %in% c(3,4)]<-NA
  data_try$TU_STOP <- factor(data_try$TU_STOP,levels=c(1,2),#3,4), 
                             labels=c("Yes","No"))#,"Not Sure","Prefer Not to Say"))
  
  #TU_STOP_12MON
  data_try$TU_STOP_23MON[data_try$TU_STOP_23MON %in% c(3,4)]<-NA
  data_try$TU_STOP_23MON <- factor(data_try$TU_STOP_23MON,levels=c(1,2),#,3,4), 
                                   labels=c("Yes","No"))#,"Not Sure","Prefer Not to Say"))
  
  #TU_QUIT: :How likely are you going to quit in the next year?
  
  #TU_40Y: How likely are you going to stay a smoker when you are 40?
  
  #SmokerOrNot
  data_try$SmokerOrNot[data_try$TU_PAST.MON == "Yes"]<-1
  #data_try$SmokerOrNot[data_try$TU_LIFE=="No"|data_try$TU_PAST.MON=="No"]<-0
  data_try$SmokerOrNot[is.na(data_try$SmokerOrNot)]<-0
  
  data_try$SmokerOrNot<-factor(data_try$SmokerOrNot,levels=0:1,labels=c("NonSmoker","Smoker"))


#INTENTIONS
  #
  #INT_TRY:Do you think you will try a cigarette soon?
  #       1-definitely no   10-definitely
  #
  #INT_NEXT.Y: Do you think you will smoke a cigarette anytime in the next year?
  #
  #INT_BF.OFFER


#E-Cigarettes
  variable_EC<-c("EC_HEARD","EC_HARM","EC_USE.LIFE","EC_USE.PAST.MON",
                 "EC_BF.TRY","EC_BF.EC")
  #EC_HEARD
  data_try$EC_HEARD<-factor(data_try$EC_HEARD,levels=c(2,1), labels=c("No","Yes"))
  #EC_HARM
  #EC_HARM 4-don't know 1-less harmful  2-as harmful  3-more harmful
  #data_try$EC_HARM<-factor(data_try$EC_HARM,levels=c(1,2,3,0), labels=c("Less Harmful","As Harmful","More Harmful","I Don't know"))
  data_try$EC_HARM[data_try$EC_HARM==4]<-NA
  #EC_USE   .LIFE  .PAST.MON
  data_try$EC_USE.LIFE<-factor(data_try$EC_USE.LIFE,levels=c(2,1), labels=c("No","Yes"))
  data_try$EC_USE.PAST.MON<-factor(data_try$EC_USE.PAST.MON,levels=c(2,1), labels=c("No","Yes"))
  #EC_BF.TRY  1：absolutely no   10：absolutely yes
  #EC_BF.EC   1：absolutely no   10：absolutely yes

  
#Social Norm
  
  variable_Dnorm<-c("DN_MS_COLL","DN_FS_COLL","DN_MCS_COUN","DN_FCS_COUN","DN_MYA","DN_FYA","DN_AM","DN_AW")
  variable_Inorm<-c("IN_PAR","IN_SPOU","IN_FRI","IN_PEO_AGE",
                    "IN_SUCC_BUS","IN_COOL","IN_WEAL","IN_FAV_CELE","IN_CHI_MEN","IN_CHI_WOM")
  variable_Inorm1<-c("IN_PAR","IN_SPOU","IN_FRI","IN_PEO_AGE")
  variable_Inorm2<-c("IN_SUCC_BUS","IN_COOL","IN_WEAL","IN_FAV_CELE")
  variable_Inorm3<-c("IN_FRI","IN_PEO_AGE")
  variable_ATT<-c("ATT_BEA_UG","ATT_GO_BAD","ATT_CLE_DIR","ATT_SAF_DAN","ATT_PLEA_UNPL","ATT_NIC_AWF")
  data_try$IN_SPOU[data_try$IN_SPOU==5]<-NA
  
  
#today<-as.character(Sys.Date())
#write.csv(data_try,paste("data_",today,".csv"))
  
  
  
#cleaning
  
  summary(data_try$valid)
  data_try<-subset(data_try,valid==1)
  