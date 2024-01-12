#median and Interquantile range
tapply(df$Kallistatin.D1.,df$Hospital.death,median,na.rm=TRUE) #Median
tapply(df$Kallistatin.D1.,df$Hospital.death,quantile,na.rm=TRUE) #Interquantile range

tapply(df$Kallistatin.D3.,df$Hospital.death,median,na.rm=TRUE) #Median
tapply(df$Kallistatin.D3.,df$Hospital.death,quantile,na.rm=TRUE) #Interquantile range

tapply(df$Kallistatin.D5.,df$Hospital.death,median,na.rm=TRUE) #Median
tapply(df$Kallistatin.D5.,df$Hospital.death,quantile,na.rm=TRUE) #Interquantile range

tapply(df$Kallikrein.D1.,df$Hospital.death,median,na.rm=TRUE) #Median
tapply(df$Kallikrein.D1.,df$Hospital.death,quantile,na.rm=TRUE) #Interquantile range



#p-value for unnormal distribution variables
wilcox.test(df$Kallistatin.D1.~df$Hospital.death,method='spearman')
wilcox.test(df$Kallistatin.D3.~df$Hospital.death,method='spearman')
wilcox.test(df$Kallistatin.D5.~df$Hospital.death,method='spearman')
wilcox.test(df$Kallikrein.D1.~df$Hospital.death,method='spearman')
