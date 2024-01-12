#ROC curve which indicate at least 60 days survival
library(pROC)
roc1=roc(df$X60.day.death,df$Kallistatin.D1.,smooth=FALSE)
wilcox.test(df$Kallistatin.D1.~df$X60.day.death) #cal p-value

#area under the curve
auc(roc1)
ci.auc(roc1) #confidence interval for the ROC
coords(roc1,'best',best.method='youden') #to determine best cut off,specificity & sensitivity

plot(roc1,ylab= 'Sensitivity %',xlab='Specificity %')
text(0.9,0.9,'p = 0.019')
text(0.9,0.8,'Area= 0.66')
text(0.35,0.35,'Cut off = 4.0µg/ml')
text(0.35,0.3,'Specificity = 50%')
text(0.35,0.25,'Sensitivity = 82%')

#combined data of Kallistatin D1,IL-6 and CRP 
Kall_fact=ifelse(df$Kallistatin.D1.<4.0,"low","high")
Kall_fact=factor(Kall_fact)
IL6_fact=ifelse(df$IL.6.D1.<161.8,"low","high")
IL6_fact=factor(IL6_fact)
CRP=ifelse(df$CRP<136.5,'low','high')
CRP=factor(CRP)
death=df$X60.day.death
df2=data.frame(death,Kall_fact,IL6_fact,CRP)
m1 <- glm(death~Kall_fact+IL6_fact+CRP, data = df2,family=binomial)
pred_prob1=predict(m1,type="response")

library(pROC)
roc1=roc(death, pred_prob1)
plot(roc1)
roc1$auc
ci.auc(roc1) #95% confidence interval
wilcox.test(pred_prob1~df$X60.day.death) #test for p-value
