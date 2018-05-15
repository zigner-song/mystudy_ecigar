
# compare the social norms of the groups(SmokerOrNot*GENDER*GRADE)

data_social.norm<-subset(data_try,
                         select=c("ID","round",variable_Dnorm,variable_Inorm,variable_TU,variable_EC,"GENDER","GRADE"))
data_social.norm$DN_FS_COLL<-as.numeric(data_social.norm$DN_FS_COLL)

str(data_social.norm)


data_social.norm<-data_social.norm[complete.cases(data_social.norm[variable_Dnorm]),]



Dnorm<-data_social.norm[,which(colnames(data_social.norm)=="DN_MS_COLL"): which( colnames(data_social.norm)=="DN_AW")]
dfm.Dnorm<-melt(data_social.norm,
                id=names(data_social.norm)[-(which(colnames(data_social.norm)=="DN_MS_COLL"): which( colnames(data_social.norm)=="DN_AW"))],
                variable.name = "DN")
dfm.Dnorm$DN_GENDER<-ifelse(dfm.Dnorm$DN %in% c("DN_MS_COLL","DN_MCS_COUN","DN_MYA","DN_AM"),1,0)
dfm.Dnorm$DN_GENDER<-factor(dfm.Dnorm$DN_GENDER,levels=0:1,labels = c("Male","Female"))


####大体看一下
aov(value~DN*GENDER*GRADE*SmokerOrNot+Error(DN/ID),
    data=dfm.Dnorm[!is.na(dfm.Dnorm$GENDER)&
                     !is.na(dfm.Dnorm$GRADE)&
                     !is.na(dfm.Dnorm$SmokerOrNot),]) %>% summary

ggbarplot(dfm.Dnorm,x="DN",y="value",fill ="DN_GENDER",col="black",add = "mean_se")+
  rotate_x_text(angle = 30)+geom_hline(yintercept = mean(dfm.Dnorm$value),linetype=2)+
  stat_compare_means(comparisons=
                       list(variable_Dnorm[1:2],
                            variable_Dnorm[3:4],
                            variable_Dnorm[5:6],
                            variable_Dnorm[7:8]))
  

ggboxplot(myeloma, x="molecular_group", y="DEPDC1", 
          color = "molecular_group", add = "jitter", legend="none")+ 
  rotate_x_text(angle = 45)+ 
  geom_hline(yintercept = mean(myeloma$DEPDC1), linetype=2)+# Add horizontal line at base mean 
  stat_compare_means(method = "anova", label.y = 1600)+ # Add global annova p-value 
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")# Pairwise comparison against all

for(i in 1:ncol(Dnorm)){
  l1<-summary(lm(Dnorm[,i]~GENDER+GRADE+SmokerOrNot, data=data_social.norm))
  print(colnames(Dnorm[i]))
  print(l1)
  print("---------------------------------------------------------------------------------------")
}




#???????????????????
Inorm2<-data_social.norm[variable_Inorm2]


for(i in 1:ncol(Inorm2)){
  mod<- polr(as.factor(Inorm2[,i]) ~ SmokerOrNot+GENDER+GRADE, data = data_social.norm, Hess=TRUE)
  l1<-summary(mod)
  coeffs <- coef(summary(mod))
  p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
  results<-cbind(coeffs, "p value" = round(p,3))
  print(colnames(Inorm2[i]))
  print(l1)
  print(results)
  print("")
}


for(i in 1:ncol(Inorm2)){
  mod<- polr(as.factor(Inorm2[,i]) ~ SmokerOrNot+GENDER+GRADE, data = data_social.norm, Hess=TRUE)
  l1<-summary(mod)
  coeffs <- coef(summary(mod))
  p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
  results<-cbind(coeffs, "p value" = round(p,3))
  
  print(colnames(Inorm2[i]))
  print(l1)
  print(results)
  print("")
}


#############################################
# regarded as a likert scale
#############################################

for(i in 1:ncol(Inorm2)){
  mod<- lm(Inorm2[,i] ~ SmokerOrNot+GENDER+GRADE, data = data_social.norm)
  l1<-summary(mod)
  print(colnames(Inorm2[i]))
  print(l1)
}



## Females have higher estimates (more accurate) of the prevelance than males
## no significant difference between smokers and non-smokers










Inorm1<-data_social.norm[c(variable_Inorm1,variable_Inorm3)]



sapply(Inorm1, table)



for(i in 1:ncol(Inorm1)){
  mod<- polr(as.factor(Inorm1[,i]) ~ SmokerOrNot+GENDER+GRADE, data = data_social.norm, Hess=TRUE)
  l1<-summary(mod)
  coeffs <- coef(summary(mod))
  p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
  results<-cbind(coeffs, "p value" = round(p,3))
  print(colnames(Inorm1[i]))
  print(l1)
  print(results)
  print("")
}

#######
#regard as likert scale
for(i in 1:ncol(Inorm1)){
  mod<- lm(Inorm1[,i] ~ SmokerOrNot*GENDER+GRADE, data = data_social.norm)
  l1<-summary(mod)
  print(colnames(Inorm1[i]))
  print(l1)
}




