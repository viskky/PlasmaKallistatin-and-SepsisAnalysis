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
