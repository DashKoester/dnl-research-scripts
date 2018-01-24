# @author: Dashiell Koester

rm(list=ls())
library(ggplot2)

##read in the new continuous time file
gf.tests<-read.table("** Data file ommitted to protect the lab's research **",header=T)
time<-read.csv("** Data file ommitted to protect the lab's research **",header=T,sep=',')


##create data frames
all<-data.frame(merge(time,gf.tests,by='Subject'))
all$deltaFS<-all$PostFigureSeriesBothTotal-all$PreFigureSeriesBothTotal
all$means<-rowMeans(data.frame(all$PostFigureSeriesBothTotal, all$PostLSATBothTotal, all$PostMRTotal, all$PostNumberSeriesTotal, all$PostShipley2AbstTotal, all$PostLetterSeriesTotal))

##custom theme for ggplot
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(color = "black"))



########## PRESTART #########

##convert prestart times into categorical variables
all$Pre30 <- cut(all$PreStart, breaks = c(0, 495, 525, 555, 585, 615, 645, 675, 705, 735, 765, 795, 825, 855, 885, 915, 945, 975, 1005, 1035, 1065, 1095, 1125), labels = c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5", "16", "16.5", "17", "17.5", "18", "18.5"))
all$Pre60 <- cut(all$PreStart, breaks = c(0, 510, 570, 630, 690, 750, 810, 870, 930, 990, 1050, 1110), labels = c("8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"))
all$Pre120 <- cut(all$PreStart, breaks = c(0, 540, 660, 780, 900, 1020, 1140), labels = c("8", "10", "12", "14", "16", "18"))


##convert variables from string to double
all$Pre30 = as.double(as.character(all$Pre30))
all$Pre60 <- as.double(as.character(all$Pre60))
all$Pre120 = as.double(as.character(all$Pre120))


##add categorical variables to dataframe
all<-cbind(all, all$Pre30)
all<-cbind(all, all$Pre60)
all<-cbind(all, all$Pre120)


##create variables for tests using 30 minute increments
preFS30<-aggregate(all$PreFigureSeriesBothTotal,by=list(all$Pre30),mean) #FS
preLSAT30<-aggregate(all$PreLSATBothScore,by=list(all$Pre30),mean) #LSAT


##create variables for tests using 60 minute increments
preFS60<-aggregate(all$PreFigureSeriesBothTotal,by=list(all$Pre60),mean) #FS
preLSAT60<-aggregate(all$PreLSATBothScore,by=list(all$Pre60),mean) #LSAT


##create variables for tests using 120 minute increments
preFS120<-aggregate(all$PreFigureSeriesBothTotal,by=list(all$Pre120),mean) #FS
preLSAT120<-aggregate(all$PreLSATBothScore,by=list(all$Pre120),mean) #LSAT


#plot individual tests by pre start time (30 min increments)
par(mfrow=c(2,1))
plot(preFS30,main='Figure Series',xlab='Time pre test began',ylab='pre FS',type='o',lty=1)
plot(preLSAT30,main='LSAT',xlab='Time pre test began',ylab='pre LSAT', type='o')


#plot individual tests by pre start time (60 min increments)
par(mfrow=c(2,1))
plot(preFS60,main='Figure Series',xlab='Time pre test began',ylab='pre FS',type='o',lty=1)
plot(preLSAT60,main='LSAT',xlab='Time pre test began',ylab='pre LSAT', type='o')


#plot individual tests by pre start time (120 min increments)
par(mfrow=c(2,1))
plot(preFS120,main='Figure Series',xlab='Time pre test began',ylab='pre FS',type='o',lty=1)
plot(preLSAT120,main='LSAT',xlab='Time pre test began',ylab='pre LSAT', type='o')


############ POSTSTART ###########

##convert poststart times into categorical variables
all$Post30 <- cut(all$PostStart, breaks = c(0, 495, 525, 555, 585, 615, 645, 675, 705, 735, 765, 795, 825, 855, 885, 915, 945, 975, 1005, 1035, 1065, 1095, 1125), labels = c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5", "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5", "16", "16.5", "17", "17.5", "18", "18.5"))
all$Post60 <- cut(all$PostStart, breaks = c(0, 510, 570, 630, 690, 750, 810, 870, 930, 990, 1050, 1110), labels = c("8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"))
all$Post120 <- cut(all$PostStart, breaks = c(0, 540, 660, 780, 900, 1020, 1140), labels = c("8", "10", "12", "14", "16", "18"))


##convert variables from string to double
all$Post30 = as.double(as.character(all$Post30))
all$Post60 <- as.double(as.character(all$Post60))
all$Post120 = as.double(as.character(all$Post120))


##add categorical variables to dataframe
all<-cbind(all, all$Post30)
all<-cbind(all, all$Post60)
all<-cbind(all, all$Post120)


##create variables for tests using 30 minute increments
postFS30<-aggregate(all$PostFigureSeriesBothTotal,by=list(all$Post30),mean) #FS
postLSAT30<-aggregate(all$PostLSATBothTotal,by=list(all$Post30),mean) #LSAT
postMR30<-aggregate(all$PostMRTotal,by=list(all$Post30),mean) #MR
postNS30<-aggregate(all$PostNumberSeriesTotal,by=list(all$Post30),mean) #Number Series
postSA30<-aggregate(all$PostShipley2AbstTotal,by=list(all$Post30),mean) #shipley
postLS30<-aggregate(all$PostLetterSeriesTotal,by=list(all$Post30),mean) #Letter Series


##create variables for tests using 60 minute increments
postFS60<-aggregate(all$PostFigureSeriesBothTotal,by=list(all$Post60),mean) #FS
postLSAT60<-aggregate(all$PostLSATBothTotal,by=list(all$Post60),mean) #LSAT
postMR60<-aggregate(all$PostMRTotal,by=list(all$Post60),mean) #MR
postNS60<-aggregate(all$PostNumberSeriesTotal,by=list(all$Post60),mean) #Number Series
postSA60<-aggregate(all$PostShipley2AbstTotal,by=list(all$Post60),mean) #shipley
postLS60<-aggregate(all$PostLetterSeriesTotal,by=list(all$Post60),mean) #Letter Series


##create variables for tests using 120 minute increments
postFS120<-aggregate(all$PostFigureSeriesBothTotal,by=list(all$Post120),mean) #FS
postLSAT120<-aggregate(all$PostLSATBothTotal,by=list(all$Post120),mean) #LSAT
postMR120<-aggregate(all$PostMRTotal,by=list(all$Post120),mean) #MR
postNS120<-aggregate(all$PostNumberSeriesTotal,by=list(all$Post120),mean) #Number Series
postSA120<-aggregate(all$PostShipley2AbstTotal,by=list(all$Post120),mean) #shipley
postLS120<-aggregate(all$PostLetterSeriesTotal,by=list(all$Post120),mean) #Letter Series


#plot individual tests by post start time (30 min increments)
par(mfrow=c(3,2)) 
#par(mfrow=c(1,1))
plot(postFS30,main='Figure Series',xlab='Time post test began',ylab='post FS score',type='o',lty=1)
plot(postLSAT30,main='LSAT',xlab='Time post test began',ylab='post LSAT score', type='o')
plot(postMR30,main='MR',xlab='Time post test began',ylab='post MR score', type='o')
plot(postNS30,main='Number Series',xlab='Time post test began',ylab='post NS', type='o')
plot(postSA30,main='Shipley Abstract',xlab='Time post test began',ylab='post SA score', type='o')
plot(postLS30,main='Letter Series',xlab='Time post test began',ylab='post LS score', type='o')


#plot individual tests by post start time (60 min increments)
par(mfrow=c(3,2))
plot(postFS60,main='Figure Series',xlab='Time post test began',ylab='post FS',type='o',lty=1)
plot(postLSAT60,main='LSAT',xlab='Time post test began',ylab='post LSAT', type='o')
plot(postMR60,main='MR',xlab='Time post test began',ylab='post MR', type='o')
plot(postNS60,main='Number Series',xlab='Time post test began',ylab='post NS', type='o')
plot(postSA60,main='Shipley Abstract',xlab='Time post test began',ylab='post SA', type='o')
plot(postLS60,main='Letter Series',xlab='Time post test began',ylab='post LS', type='o')


#plot individual tests by post start time (120 min increments)
par(mfrow=c(3,2))
plot(postFS120,main='Figure Series',xlab='Time post test began',ylab='post FS score',type='o',lty=1)
plot(postLSAT120,main='LSAT',xlab='Time post test began',ylab='post LSAT score', type='o')
plot(postMR120,main='MR',xlab='Time post test began',ylab='post MR score', type='o')
plot(postNS120,main='Number Series',xlab='Time post test began',ylab='post NS score', type='o')
plot(postSA120,main='Shipley Abstract',xlab='Time post test began',ylab='post SA score', type='o')
plot(postLS120,main='Letter Series',xlab='Time post test began',ylab='post LS score', type='o')


#average scores from each test per 30 min increment
all.tests30<-data.frame(postFS30,postLSAT30[,2],postMR30[,2],postNS30[,2],postSA30[,2],postLS30[,2])
colnames(all.tests30)<-c('time', 'fs','lsat','mr','ns','sa','ls')
all.tests30$ave<-rowMeans(all.tests30[,2:7])

#average scores from each test per 60 min increment
all.tests60<-data.frame(postFS60,postLSAT60[,2],postMR60[,2],postNS60[,2],postSA60[,2],postLS60[,2])
colnames(all.tests60)<-c('time','fs','lsat','mr','ns','sa','ls')
all.tests60$ave<-rowMeans(all.tests60[,2:7])

#average scores from each test per 120 min increment
all.tests120<-data.frame(postFS120,postLSAT120[,2],postMR120[,2],postNS120[,2],postSA120[,2],postLS120[,2])
colnames(all.tests120)<-c('time','fs','lsat','mr','ns','sa','ls')
all.tests120$ave<-rowMeans(all.tests120[,2:7])


#average each subject's tests
test.ave<-data.frame(all[,c("PostFigureSeriesBothTotal","PostLSATBothTotal","PostMRTotal","PostNumberSeriesTotal","PostShipley2AbstTotal","PostLetterSeriesTotal")])
test.ave$means<-rowMeans(test.ave[,1:6])


#plot average of six tests
par(mfrow=c(1,1))
plot(aggregate(test.ave$means,by=list(all$Post30),mean),main="Average for all tests (30)",xlab='Time post test started (30 min increments)',ylab='Average of 6 Mitre tests @ post',type='o',col='blue', xaxt="n")
axis(side=1, at=c(8, 10, 12, 14, 16, 18))
plot(aggregate(test.ave$means,by=list(all$Post60),mean),main="Average for all tests (60)",xlab='Time post test started (60 min increments)',ylab='Average of 6 Mitre tests @ post',type='o',col='blue', xaxt="n")
axis(side=1, at=c(8, 10, 12, 14, 16, 18))
plot(aggregate(test.ave$means,by=list(all$Post120),mean),main="Average for all tests (120)",xlab='Time post test started (120 min increments)',ylab='Average of 6 Mitre tests @ post',type='o',col='blue', xaxt="n")
axis(side=1, at=c(8, 10, 12, 14, 16, 18))

# Create dataframes for lo:lo, lo:hi, hi:hi, hi:lo time frames
lo.lo <- data.frame()
hi.hi <- data.frame()
lo.hi <- data.frame()
hi.lo <- data.frame()


# Separate subjects into dataframes based on pre/post time
for (i in 1:length(all[,1])) {
 
  #time variables
  preTime <- all$Pre120[i]
  postTime <- all$Post120[i]
 
  
  #lo:lo
  if ((preTime == 8 || preTime == 12 || preTime == 16) && (postTime == 8 || postTime == 12 || postTime == 16)) {
    lo.lo <- rbind(lo.lo, c(all$Subject[i], all$Pre120[i], all$Post120[i],
                            + all$PreFigureSeriesBothTotal[i], all$PostFigureSeriesBothTotal[i],
                            + all$PreLSATBothScore[i], all$PostLSATBothTotal[i]))
  }
  
  
  #hi:hi
  else if ((preTime == 10 || preTime == 14 || preTime == 18) && (postTime == 10 || postTime == 14 || postTime == 18)) {
    hi.hi <- rbind(hi.hi, c(all$Subject[i], all$Pre120[i], all$Post120[i],
                            + all$PreFigureSeriesBothTotal[i], all$PostFigureSeriesBothTotal[i],
                            + all$PreLSATBothScore[i], all$PostLSATBothTotal[i]))
  }
  
  
  #lo:hi
  else if ((preTime == 8 || preTime == 12 || preTime == 16) && (postTime == 10 || postTime == 14 || postTime == 18)) {
    lo.hi <- rbind(lo.hi, c(all$Subject[i], all$Pre120[i], all$Post120[i],
                            + all$PreFigureSeriesBothTotal[i], all$PostFigureSeriesBothTotal[i],
                            + all$PreLSATBothScore[i], all$PostLSATBothTotal[i]))
  }
  
  
  #hi:lo
  else if ((preTime == 10 || preTime == 14 || preTime == 18) && (postTime == 8 || postTime == 12 || postTime == 16)) {
    hi.lo <- rbind(hi.lo, c(all$Subject[i], all$Pre120[i], all$Post120[i],
                            + all$PreFigureSeriesBothTotal[i], all$PostFigureSeriesBothTotal[i],
                            + all$PreLSATBothScore[i], all$PostLSATBothTotal[i]))
  }
}


# Define column names
colnames(lo.lo) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")
colnames(hi.hi) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")
colnames(lo.hi) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")
colnames(hi.lo) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")


# @TODO: Figure out how to rename last row to "Means"
# insert row of test means into each data frame
lo.lo <- rbind(lo.lo,  colMeans(lo.lo))
hi.hi <- rbind(hi.hi,  colMeans(hi.hi))
lo.hi <- rbind(lo.hi,  colMeans(lo.hi))
hi.lo <- rbind(hi.lo,  colMeans(hi.lo))


# insert row of subject pool's test means into each data frame
lo.lo <- rbind(lo.lo, c(mean(all$Subject), mean(all$PreStart), mean(all$PostStart), mean(all$PreFigureSeriesBothTotal), mean(all$PostFigureSeriesBothTotal), mean(all$PreLSATBothScore), mean(all$PostLSATBothTotal)))
hi.hi <- rbind(hi.hi, c(mean(all$Subject), mean(all$PreStart), mean(all$PostStart), mean(all$PreFigureSeriesBothTotal), mean(all$PostFigureSeriesBothTotal), mean(all$PreLSATBothScore), mean(all$PostLSATBothTotal)))
lo.hi <- rbind(lo.hi, c(mean(all$Subject), mean(all$PreStart), mean(all$PostStart), mean(all$PreFigureSeriesBothTotal), mean(all$PostFigureSeriesBothTotal), mean(all$PreLSATBothScore), mean(all$PostLSATBothTotal)))
hi.lo <- rbind(hi.lo, c(mean(all$Subject), mean(all$PreStart), mean(all$PostStart), mean(all$PreFigureSeriesBothTotal), mean(all$PostFigureSeriesBothTotal), mean(all$PreLSATBothScore), mean(all$PostLSATBothTotal)))


# Create line plot for test means per data frame
cutTestMeans <- data.frame()
cutTestMeans <- rbind(cutTestMeans,  colMeans(lo.lo))
cutTestMeans <- rbind(cutTestMeans,  colMeans(hi.hi))
cutTestMeans <- rbind(cutTestMeans,  colMeans(lo.hi))
cutTestMeans <- rbind(cutTestMeans,  colMeans(hi.lo))
colnames(cutTestMeans) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")
rownames(cutTestMeans) <- c("lo.lo", "hi.hi", "lo.hi", "hi.lo")
plot(cutTestMeans,main='Means',xlab='Test Taken',ylab='Average Score')




# Create dataframes for lo:lo, lo:hi, hi:hi, hi:lo using highest peak and lowest trough 
# **Note**: currently created with set times for peak/trough. Should change to variable in future

lo.lo2 <- data.frame()
hi.hi2 <- data.frame()
lo.hi2 <- data.frame()
hi.lo2 <- data.frame()



# Separate subjects into dataframes based on pre/post time
for (i in 1:length(all[,1])) {
  
  #time variables
  preTime <- all$Pre120[i]
  postTime <- all$Post120[i]
  
  
  #lo:lo
  if (preTime == 12 && postTime == 12) {
    lo.lo2 <- rbind(lo.lo2, c(all$Subject[i], all$Pre120[i], all$Post120[i],
                            + all$PreFigureSeriesBothTotal[i], all$PostFigureSeriesBothTotal[i],
                            + all$PreLSATBothScore[i], all$PostLSATBothTotal[i]))
  }
  
  
  #hi:hi
  else if (preTime == 14 && postTime == 14) {
    hi.hi2 <- rbind(hi.hi2, c(all$Subject[i], all$Pre120[i], all$Post120[i],
                            + all$PreFigureSeriesBothTotal[i], all$PostFigureSeriesBothTotal[i],
                            + all$PreLSATBothScore[i], all$PostLSATBothTotal[i]))
  }
  
  
  #lo:hi
  else if (preTime == 12 && postTime == 14) {
    lo.hi2 <- rbind(lo.hi2, c(all$Subject[i], all$Pre120[i], all$Post120[i],
                            + all$PreFigureSeriesBothTotal[i], all$PostFigureSeriesBothTotal[i],
                            + all$PreLSATBothScore[i], all$PostLSATBothTotal[i]))
  }
  
  
  #hi:lo
  else if (preTime == 14 && postTime == 12) {
    hi.lo2 <- rbind(hi.lo2, c(all$Subject[i], all$Pre120[i], all$Post120[i],
                            + all$PreFigureSeriesBothTotal[i], all$PostFigureSeriesBothTotal[i],
                            + all$PreLSATBothScore[i], all$PostLSATBothTotal[i]))
  }
}


# Define column names
colnames(lo.lo2) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")
colnames(hi.hi2) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")
colnames(lo.hi2) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")
colnames(hi.lo2) <- c("Subject", "PreTime", "PostTime", "Pre-FS Score", "Post-FS Score", "Pre-LSAT Score", "Post-LSAT Score")

lo.lo2 <- rbind(lo.lo2,  colMeans(lo.lo2))
hi.hi2 <- rbind(hi.hi2,  colMeans(hi.hi2))
lo.hi2 <- rbind(lo.hi2,  colMeans(lo.hi2))
hi.lo2 <- rbind(hi.lo2,  colMeans(hi.lo2))

lo.lo2
hi.hi2
lo.hi2
hi.lo2
