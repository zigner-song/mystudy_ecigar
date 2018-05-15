#model learning
#Y=clu
data3<-merge(data3,data3_EC[c("ID",
                              na.exclude(str_extract(names(data3_EC),".{0,}clu$")))])

data3$clu<-factor(data3$clu)
data3$kclu<-factor(data3$kclu)
############################
#
#    logistics regression
#
############################

#参考http://blog.csdn.net/charlotte28/article/details/52570190

#编写公式




fml.logi<-str_extract(names(data3),pattern = "((^ATT_.{1,})|(^TU_.{1,}))|(GENDER)|(DEGREE)|(AGE)") %>% 
          str_extract(.,pattern = "^((?!23MON\\.{1,}).)*$") %>%
          str_extract(.,pattern = "^((?!STOP\\.{1,}).)*$") %>% 
          na.exclude() %>%
          paste(.,collapse="+") %>% 
          paste("clu ~ ",.,"-TU_40Y",sep="") %>%
          as.formula()

require(foreign)  
require(nnet) 


model.logi<-multinom(fml.logi, data = data3)
z<-summary(model.logi)$coefficients/summary(model.logi)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1))*2)

step(model.logi)
kkk<-na.omit(data3[str_extract(names(data3),pattern = "((^ATT_.{1,})|(^TU_.{1,}))|(GENDER)|(DEGREE)|(AGE)|(kclu)") %>% 
                     str_extract(.,pattern = "^((?!23MON\\.{1,}).)*$") %>%
                     str_extract(.,pattern = "^((?!STOP\\.{1,}).)*$") %>% 
                     na.exclude()])
table(predict(model.logi,data=data3),kkk$kclu)
%>% prop.table(.,margin=2)

############################
#
#      Decision Tree
#
############################


library(rpart)


rt.model<-rpart(fml.logi,data=data3)


prettyTree(rt.model)

printcp(rt.model)#cp统计量

plot(rt.model$cptable[,1],type="b")


rt.model.prune <- prune(rt.model,cp=0.03)#jianzhi
rt.model.prune
prettyTree(rt.model.prune)


ggplot(dfm_data3_EC, aes(variable, value,group=ID)) +
  geom_jitter(height=0.4,aes(color=factor(dfm_data3_EC$clu)))

rt.predict<-(predict(rt.model,data3)==
               apply(data.frame(predict(rt.model,data3)),
                                            FUN=max,MARGIN=1) %>% as.numeric) %*% 1:3
rt.predict.prune<-(predict(rt.model.prune,data3)==
                     apply(data.frame(predict(rt.model.prune,data3)),
                                            FUN=max,MARGIN=1) %>% as.numeric) %*% 1:3
table(rt.predict,data3$clu)%>% prop.table(.,margin=2)
table(rt.predict.prune,data3$clu)
table(rt.predict.prune,data3$clu) %>% prop.table(.,margin=2)


############################
#
#      SVM
#
############################

library(e1071)
svm.model<-svm(fml.logi, data = data3)

table(predict(svm.model,data3),data3$clu)
