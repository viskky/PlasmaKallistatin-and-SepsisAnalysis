#Kaplan-meier survival curve
library(survival)
group=ifelse(df$Kallistatin.D1.<4,0,1)
group=factor(group)
ST=as.numeric(df$Days.60.day.death.)
State=as.numeric(df$Hospital.death)
df_surv=data.frame(group,State,ST)
survobj=Surv(ST,State)
fit=survfit(survobj~group,data=df_surv,conf.type='plain')

summary(fit)
plot(fit, col= c('grey','black'),lwd=4, ylab = 'Survival', xlab='Days')
survdiff(survobj~group,data=df_surv) #log rank test
text(10,0.6,'p= 0.01')
legend(20,0.4, c('kallistatin < 4.0µg/ml','Kallistatin > 4.0µg/ml'), lty = 1, col=c('grey','black'),lwd=4,bty='n')
