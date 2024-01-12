# Read in data and rid missing values
read.csv(file.choose(), head=TRUE,sep=";",dec=",",na.strings="#NULL!") 

#simplify and extract the variables useful for analysis
df=raw[,c(1,2,3,8,9,10,18,20:23,25:46)] 

df$Sex=factor(df$Sex) # 1= men
df$Hospital.death=factor(df$Hospital.death) # 0 = survival, 1 = death
df$X60.day.death=factor(df$X60.day.death) # 0 = survival, 1 = death
df$Blood.Culture=factor(df$Blood.Culture) # 0 = negative, 1 = positive
df$Shock=factor(df$Shock) # 0 = severe sepsis, 1 = Septic chock
df$ARDS=factor(df$ARDS)

#number 0f survival and non survival
table(df$Hospital.death) 

#calculate the mean and the standard deviation, and the p-value for the variable Age
tapply(df$Age,df$Hospital.death,mean,na.rm=TRUE)
tapply(df$Age,df$Hospital.death,sd,na.rm=TRUE) 
tapply(df$WBC,df$Hospital.death,mean) 
tapply(df$WBC,df$Hospital.death,sd)  
tapply(df$APACHE.II,df$Hospital.death,mean) 
tapply(df$APACHE.II,df$Hospital.death,sd) 
tapply(df$SOFA.D1.,df$Hospital.death,mean) 
tapply(df$SOFA.D1.,df$Hospital.death,sd) 


#p-value for the continuous variables
t.test(Age~Hospital.death,data=df)
t.test(WBC~Hospital.death,data=df)
t.test(Age~Hospital.death,data=df)
t.test(APACHE.II~Hospital.death,data=df)
t.test(SOFA.D1.~Hospital.death,data=df)

#Proportions
my_table1=table(df$Sex,df$Hospital.death)
prop.table(my_table1,2)
my_table2=table(df$Pneumonia.Extrapulmonary,df$Hospital.death)
prop.table(my_table2,2)
my_table3=table(df$DM,df$Hospital.death)
prop.table(my_table3,2)
my_table4=table(df$Kidney.Dz,df$Hospital.death)
prop.table(my_table4,2)

#p-value for categorical variaables
chisq.test(my_table1)
chisq.test(my_table2)
chisq.test(my_table3)
chisq.test(my_table4)


#median and interquartile range for non normal distributed variable
tapply(df$qSOFA,df$Hospital.death,median,na.rm=TRUE)
tapply(df$qSOFA,df$Hospital.death,quantile,na.rm=TRUE)

tapply(df$SOFA.D3.,df$Hospital.death,median,na.rm=TRUE)
tapply(df$SOFA.D3.,df$Hospital.death,quantile,na.rm=TRUE)


#p-value for skewed distribution
wilcox.test(df$qSOFA~df$Hospital.death)
wilcox.test(df$Days.ICU.~df$Hospital.death)
wilcox.test(df$Days.mechanical.ventilation.~df$Hospital.death)
