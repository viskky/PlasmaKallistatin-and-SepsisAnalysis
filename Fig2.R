
BC1=ifelse(df$Blood.Culture==0,"Negative BC","Positive BC")
BC=c(BC1, BC1, BC1)
BC =factor(BC,levels=c("Negative BC","Positive BC"))
Kallistatin=c(df$Kallistatin.D1.,df$Kallistatin.D3.,df$Kallistatin.D5.)
n=length(df$Kallistatin.D1.)
Days=factor(rep(c("Day 1", "Day 3", "Day 5"),c(n,n,n)))
ln_Kallistatin=log2(Kallistatin)
df2=data.frame(ln_Kallistatin,Sepsis,Days,BC)
boxplot(ln_Kallistatin ~ BC+Days, data = df2,ylim=c(-2,9),las=1, ylab='Kallistatin (Log2, µg/ml)',
        at = c(1:2, 4:5, 7:8),col=c("white","gray60"),xaxt = "n",yaxt = "n")
axis(1,at=c(1.5,4.5,7.5),labels=c("Day 1","Day 3","Day 5"))
axis(2,at=-1:9,las=2)
legend("topright",levels(BC),fill =c("white","gray60"), bty="n")

wilcox.test(df$Kallistatin.D1.~df$Blood.Culture) #determine the p-value for the boxplot
wilcox.test(df$Kallistatin.D3.~df$Blood.Culture) #determine the p-value for the boxplot
wilcox.test(df$Kallistatin.D5.~df$Blood.Culture) #determine the p-value for the boxplot

text(1.5,6,'p = 0.022') #add text to plot.
text(4.5,9,'p = 0.035') #add text to plot.
text(8,7,'p = 0.35') #add text to plot.


Sepsis1=ifelse(df$Shock==0,"Severe Sepsis","Septic shock")
Sepsis=c(Sepsis1,Sepsis1,Sepsis1)
Sepsis=factor(Sepsis,levels=c("Severe Sepsis","Septic shock"))
Kallistatin=c(df$Kallistatin.D1.,df$Kallistatin.D3.,df$Kallistatin.D5.)
n=length(df$Kallistatin.D1.)
Days=factor(rep(c("Day 1", "Day 3", "Day 5"),c(n,n,n)))
ln_Kallistatin=log2(Kallistatin)
df2=data.frame(ln_Kallistatin,Sepsis,Days,BC)
boxplot(ln_Kallistatin ~ Sepsis+Days, data = df2,ylim=c(-1,9),las=1,ylab='Kallistatin (Log2, µg/ml)' ,
        at = c(1:2, 4:5, 7:8),col=c("white","gray60"),xaxt = "n",yaxt = "n")
axis(1,at=c(1.5,4.5,7.5),labels=c("Day 1","Day 3","Day 5"))
axis(2,at=-1:9,las=2)
legend("topright",levels(Sepsis),fill =c("white","gray60") bty="n")

wilcox.test(df$Kallistatin.D1.~df$Shock) #determine the p-value for the boxplot
wilcox.test(df$Kallistatin.D3.~df$Shock) #determine the p-value for the boxplot
wilcox.test(df$Kallistatin.D5.~df$Shock) #determine the p-value for the boxplot

text(1.5,6,'p = 0.004') #add text to plot.
text(4.5,9,'p = 0.01') #add text to plot.
text(7.5,7,'p = 0.574') #add text to plot.
