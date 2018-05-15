#descriptive analysis of the e-cigar use

N<-length(data_try$ID)# N为所有纳入后续分析的被试量
data_Smoker.EC<-(subset(data_try,
                        select=c("ID","SmokerOrNot",variable_EC,variable_Demography)))

###  Missing Data
data_Smoker.EC[!complete.cases(data_Smoker.EC),]
manyNAs(data_Smoker.EC)


##  demography
  for(i in 1:length(variable_Demography)){
    print("-------------------------------------")
    print(variable_Demography[i])
    print(table(data_try[variable_Demography[i]]))
    }
  
## Tobacco Use
  for(i in 1:length(variable_TU)){
    print("--------------------------------------")
    print(variable_TU[i])
    print(table(data_try[variable_TU[i]]))
  }

  histWithNormalCurve(data_try$TU_QUIT,
                    main="How likely are you going to quit in the next year?",
                    xlab="1-No Plan to Quit          10-Determine to Quit")  
  histWithNormalCurve(data_try$TU_40Y,
                      main="How likely are you going to stay a smoker when you are 40? ",
                      xlab="1-Absolutely          10-Absolutely Not")  
  

## E-Smoking
  for(i in variable_EC){
    print("--------------------------------------")
    print(i)
    print(t(table(data_try[i])))
  }
  
  
## Smoker * E-Smoking
  xtabs(~SmokerOrNot+EC_HEARD,data=data_try)
  xtabs(~SmokerOrNot+EC_HARM,data=data_try)
  xtabs(~SmokerOrNot+EC_USE.LIFE,data=data_try)
  
  summaryBy(EC_BF.TRY~SmokerOrNot,data=data_try,FUN=dstats)
  t.test(EC_BF.TRY~SmokerOrNot,data=data_try)
  
  summaryBy(EC_BF.EC~SmokerOrNot,data=data_try,FUN=dstats)
  t.test(EC_BF.EC~SmokerOrNot,data=data_try)
  
  dfm.Smoker.EC<-melt(subset(data_Smoker.EC,
                             !is.na(EC_BF.TRY) & !is.na(EC_BF.EC) &
                               (!(GENDER=="Female" & SmokerOrNot=="Smoker") &
                                  !is.na(GENDER) & ! is.na(GRADE)),
                             select=c("ID","SmokerOrNot",variable_Demography,variable_EC)),
                      id=c("ID","SmokerOrNot",variable_Demography,variable_EC[1:4]),
                      variable.name = "flavor")
  
  
  aov(value~flavor*SmokerOrNot*GENDER*GRADE+
            Error(ID/flavor),
      dfm.Smoker.EC) %>% summary
  
  
  
  
  