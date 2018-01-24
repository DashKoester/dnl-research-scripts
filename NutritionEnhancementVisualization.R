# @author: Dashiell Koester

rm(list=ls(all=T))
library(ggplot2)
library(reshape2)

# import data
round4<-read.table('/home/dash/Desktop/Research/DNL Research Project/Nutrition & Cognition/NPSummaryDataCNLM.dat',header=T)

# custom theme for ggplot
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(color = "black"))


# Multiple plot function
# Plots multiple ggplots in the same window
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Split round4 into separate cycles
cycle1 <- subset(round4,round4$Group==1)
cycle1 <- na.omit(cycle1)
cycle1.means <- colMeans(cycle1[,5:56])

cycle2 <- subset(round4,round4$Group==2)
cycle2 <- na.omit(cycle2)
cycle2.means <- colMeans(cycle2[,5:56])

both.means <- rbind(cycle1.means,cycle2.means)
par(mfrow=c(1,1))


# # # # #IFR WORDS# # # # # #
dat <- data.frame(c(both.means["cycle1.means","PreIFR_WordsWordsRecalled"], both.means["cycle1.means","PostIFR_WordsWordsRecalled"], both.means["cycle2.means","PreIFR_WordsWordsRecalled"], both.means["cycle2.means","PostIFR_WordsWordsRecalled"]))
rownames(dat) <- c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)<- 'value'
dat$PrePost <- c('Pre', 'Post')
dat$Cycle <- factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position <- c(1, 2, 3, 4)

# calculate standard error
standDev <- sd(dat$value)
se <- standDev/2

# basic barplot of data
barplot(t(dat),main="IFR_WordsRecalled")


# basic ggplot of data
ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
                cleanup +
                ggtitle("IFR Words") +
                theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
                xlab(NULL) +
                ylab("# of Words Correct") +
                scale_fill_manual(labels = c("Post", "Pre"),
                                  values = c("blue", "red"),
                                  # values = c("white", "light blue")
                                  # values = c("light green" , "light yellow"),
                                  guide = guide_legend(title=NULL, reverse = TRUE))


# # # # # # #PIC WORDS# # # # # 
dat<-data.frame(c(both.means["cycle1.means","PreIFR_PicturesWordsRecalled"],both.means["cycle1.means","PostIFR_PicturesWordsRecalled"],both.means["cycle2.means","PreIFR_PicturesWordsRecalled"],both.means["cycle2.means","PostIFR_PicturesWordsRecalled"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
standDev = sd(dat$value)
se = standDev/2

# basic barplot of data
barplot(t(dat),main="IFR Picture Words")


# basic ggplot of data
ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("IFR Picture Words") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# of Words Correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))


# # # # PAIRED ASSOCIATES# # # # # 
dat<-data.frame(c(both.means["cycle1.means","PrePairedAssociatesWordsRecalled"],both.means["cycle1.means","PostPairedAssociatesWordsRecalled"],both.means["cycle2.means","PrePairedAssociatesWordsRecalled"],both.means["cycle2.means","PostPairedAssociatesWordsRecalled"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
paStandDev = sd(dat$value)
paSE = paStandDev/2

# basic barplot of data
barplot(t(dat),main="Paired Associates Words")


# basic ggplot of data
paPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - paSE, ymax = value + paSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Paired Associates Words") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# of Words Correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))

paPlot

# # # # # # PAIRED ASSOCIATES DELAY# # # # # #
dat<-data.frame(c(both.means["cycle1.means","PrePairedAssociates_DelayWordsRecalled"],both.means["cycle1.means","PostPairedAssociates_DelayWordsRecalled"],both.means["cycle2.means","PrePairedAssociates_DelayWordsRecalled"],both.means["cycle2.means","PostPairedAssociates_DelayWordsRecalled"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
standDev = sd(dat$value)
se = standDev/2

# basic barplot of data
barplot(t(dat),main="Paired Associates Words (Delay)")


# basic ggplot of data
paDelayPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Paired Associates Words (Delay) ") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# of Words Correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
paDelayPlot


# stack PA plots
multiplot(paPlot, paDelayPlot)
par(mfrow = c(1,1))

# # # # # # #NUMBER SERIES# # # # # # # # 
dat<-data.frame(c(both.means["cycle1.means","PreNumberSeriesCorrectTrials"],both.means["cycle1.means","PostNumberSeriesCorrectTrials"],both.means["cycle2.means","PreNumberSeriesCorrectTrials"],both.means["cycle2.means","PostNumberSeriesCorrectTrials"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
nsStandDev = sd(dat$value)
nsSE = nsStandDev/2

# basic barplot of data
barplot(t(dat),main="Number Series")


# basic ggplot of data
numSeriesPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - nsSE, ymax = value + nsSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Number Series") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# Correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
numSeriesPlot


 # # # # # # NUMBER SERIES RT# # # # # # # #
dat<-data.frame(c(both.means["cycle1.means","PreNumberSeriesCorrectTrialRT"],both.means["cycle1.means","PostNumberSeriesCorrectTrialRT"],both.means["cycle2.means","PreNumberSeriesCorrectTrialRT"],both.means["cycle2.means","PostNumberSeriesCorrectTrialRT"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
nsRTStandDev = sd(dat$value)
nsRTSE = nsRTStandDev/2

# basic barplot of data
barplot(t(dat),main="Number Series RT")


# basic ggplot of data
numSeriesRTPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - nsRTSE, ymax = value + nsRTSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Number Series RT") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Reaction Time (ms)") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
numSeriesRTPlot


# # NUMBER SERIES RTSTD# # 
dat <- data.frame(c(both.means["cycle1.means", "PreNumberSeriesCorrectTrialRTStd"],both.means["cycle1.means", "PostNumberSeriesCorrectTrialRTStd"],both.means["cycle2.means", "PreNumberSeriesCorrectTrialRTStd"],both.means["cycle2.means", "PostNumberSeriesCorrectTrialRTStd"]))
rownames(dat) <- c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat) <- 'value'
dat$PrePost <- c('Pre', 'Post')
dat$Cycle <- factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position <- c(1, 2, 3, 4)

# calculate standard error
standDev <- sd(dat$value)
se <- standDev/2

# basic barplot of data
barplot(t(dat),main="Number Series RT (std)")


# basic ggplot of data
numSeriesSTDPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Number Series RT (std)") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Reaction Time (ms)") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))


numSeriesSTDPlot


# stack numSeries plots
multiplot(numSeriesPlot, numSeriesRTPlot, numSeriesSTDPlot)
par(mfrow = c(1,1))



# # # # # KEEP TRACK WORDS# # # # # #
dat<-data.frame(c(both.means["cycle1.means","PreKeepTrackWordsRecalled"],both.means["cycle1.means","PostKeepTrackWordsRecalled"],both.means["cycle2.means","PreKeepTrackWordsRecalled"],both.means["cycle2.means","PostKeepTrackWordsRecalled"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
standDev = sd(dat$value)
se = standDev/2

# basic barplot of data
barplot(t(dat),main="Keep Track Words")


# basic ggplot of data
ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Keep Track Words") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# of Words Correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))



# # # # # LETTER SET# # # # # # #
dat<-data.frame(c(both.means["cycle1.means","PreLetterSetsCorrectTrials"],both.means["cycle1.means","PostLetterSetsCorrectTrials"],both.means["cycle2.means","PreLetterSetsCorrectTrials"],both.means["cycle2.means","PostLetterSetsCorrectTrials"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
lsStandDev = sd(dat$value)
lsSE = lsStandDev/2

# basic barplot of data
barplot(t(dat),main="Letter Set")


# basic ggplot of data
letterSetPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - lsSE, ymax = value + lsSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Letter Set Correct") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
letterSetPlot



# # # # # LETTER SET RT# # # # # 
dat<-data.frame(c(both.means["cycle1.means","PreLetterSetsCorrectTrialRT"],both.means["cycle1.means","PostLetterSetsCorrectTrialRT"],both.means["cycle2.means","PreLetterSetsCorrectTrialRT"],both.means["cycle2.means","PostLetterSetsCorrectTrialRT"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
lsRTStandDev = sd(dat$value)
lsRTSE = lsRTStandDev/2

# basic barplot of data
barplot(t(dat),main="Letter Set RT")


# basic ggplot of data
letterSetRTPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - lsRTSE, ymax = value + lsRTSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Letter Set RT") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Reaction Time (ms)") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))

letterSetRTPlot


# # # # # LETTER SET RT STD# # # # # 
dat<-data.frame(c(both.means["cycle1.means","PreLetterSetsCorrectTrialRTStd"],both.means["cycle1.means","PostLetterSetsCorrectTrialRTStd"],both.means["cycle2.means","PreLetterSetsCorrectTrialRTStd"],both.means["cycle2.means","PostLetterSetsCorrectTrialRTStd"]))
rownames(dat) <- c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat) <- 'value'
dat$PrePost <- c('Pre', 'Post')
dat$Cycle <- factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position <- c(1, 2, 3, 4)

# calculate standard error
standDev <- sd(dat$value)
se <- standDev/2

# basic barplot of data
barplot(t(dat),main="Letter Set RT (std)")


# basic ggplot of data
letterSetRTSTDPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Letter Set RT (std)") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Reaction Time (ms)") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))

letterSetRTSTDPlot


# stack letterSeries plots
multiplot(letterSetPlot, letterSetRTPlot)# , letterSetRTSTDPlot)
par(mfrow=c(1,1))

# # # # # ROTATION SPAN# # # # #
dat<-data.frame(c(both.means["cycle1.means","PreRotationSpanTotal"],both.means["cycle1.means","PostRotationSpanTotal"],both.means["cycle2.means","PreRotationSpanTotal"],both.means["cycle2.means","PostRotationSpanTotal"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
standDev = sd(dat$value)
se = standDev/2

# basic barplot of data
barplot(t(dat),main="Rotation Span")


# basic ggplot of data
rsPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Rotation Span") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))

rsPlot


# # # # # ROTATION SPAN ABSOLUTE# # # # #
dat<-data.frame(c(both.means["cycle1.means","PreRotationSpanAbsoluteScore"],both.means["cycle1.means","PostRotationSpanAbsoluteScore"],both.means["cycle2.means","PreRotationSpanAbsoluteScore"],both.means["cycle2.means","PostRotationSpanAbsoluteScore"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
rsaStandDev = sd(dat$value)
rsaSE = rsaStandDev/2

# basic barplot of data
barplot(t(dat),main="Rotation Span (Absolute)")


# basic ggplot of data
rsaAbsPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - rsaSE, ymax = value + rsaSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Rotation Span (Absolute)") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
rsaAbsPlot


# # # # # ROTATION SPAN ERROR# # # # #
dat<-data.frame(c(both.means["cycle1.means","PreRotationSpanErrorTotal"],both.means["cycle1.means","PostRotationSpanErrorTotal"],both.means["cycle2.means","PreRotationSpanErrorTotal"],both.means["cycle2.means","PostRotationSpanErrorTotal"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
rsaErrorStandDev = sd(dat$value)
rsaErrorSE = rsaErrorStandDev/2

# basic barplot of data
barplot(t(dat),main="Rotation Span (Error)")


# basic ggplot of data
rsaAbsErrorPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - rsaErrorSE, ymax = value + rsaErrorSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Rotation Span (Error)") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))

rsaAbsErrorPlot

# stack Rotation Span plots
multiplot(rsPlot, rsaAbsPlot, rsaAbsErrorPlot)
par(mfrow=c(1,1))

# # # # # #SYMMETRY SPAN# # # 
dat<-data.frame(c(both.means["cycle1.means","PreSymmetrySpanTotal"],both.means["cycle1.means","PostSymmetrySpanTotal"],both.means["cycle2.means","PreSymmetrySpanTotal"],both.means["cycle2.means","PostSymmetrySpanTotal"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
standDev = sd(dat$value)
se = standDev/2

# basic barplot of data
barplot(t(dat),main="Symmetry Span")


# basic ggplot of data
ssPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Symmetry Span") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
ssPlot


# # # # # #SYMMETRY SPAN ABSOLUTE# # # 
dat<-data.frame(c(both.means["cycle1.means","PreSymmetrySpanAbsoluteScore"],both.means["cycle1.means","PostSymmetrySpanAbsoluteScore"],both.means["cycle2.means","PreSymmetrySpanAbsoluteScore"],both.means["cycle2.means","PostSymmetrySpanAbsoluteScore"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
ssaStandDev = sd(dat$value)
ssaSE = ssaStandDev/2

# basic barplot of data
barplot(t(dat),main="Symmetry Span (Absolute)")


# basic ggplot of data
ssAbsolutePlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Symmetry Span (Absolute)") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
ssAbsolutePlot

# # # # # #SYMMETRY SPAN ERROR# # # 
dat<-data.frame(c(both.means["cycle1.means","PreSymmetrySpanErrorTotal"],both.means["cycle1.means","PostSymmetrySpanErrorTotal"],both.means["cycle2.means","PreSymmetrySpanErrorTotal"],both.means["cycle2.means","PostSymmetrySpanErrorTotal"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
sseStandDev = sd(dat$value)
sseSE = sseStandDev/2

# basic barplot of data
barplot(t(dat),main="Symmetry Span (Error)")


# basic ggplot of data
ssErrorPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - sseSE, ymax = value + sseSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Symmetry Span (Error)") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("# correct") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
ssErrorPlot

# stack Symmetry Span plots
multiplot(ssPlot, ssAbsolutePlot, ssErrorPlot)
par(mfrow=c(1,1))


# # # # # #STROOP CONGRUOUS RT# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopCongruousRT"],both.means["cycle1.means","PostStroopCongruousRT"],both.means["cycle2.means","PreStroopCongruousRT"],both.means["cycle2.means","PostStroopCongruousRT"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
stroopRTStandDev = sd(dat$value)
stroopRTSE = stroopRTStandDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Congruous RT")


# basic ggplot of data
stroopRTPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - stroopRTSE, ymax = value + stroopRTSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Congruous RT") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Reaction Time (ms)") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
stroopRTPlot


# # # # # #STROOP CONGRUOUS RT STD# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopCongruousRTStd"],both.means["cycle1.means","PostStroopCongruousRTStd"],both.means["cycle2.means","PreStroopCongruousRTStd"],both.means["cycle2.means","PostStroopCongruousRTStd"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
stroopRTSTDStandDev = sd(dat$value)
stroopRTSTDSE = stroopRTSTDStandDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Congruous RT (STD)")


# basic ggplot of data
stroopRTSTDPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - stroopRTSE, ymax = value + stroopRTSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Congruous RT (STD)") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Reaction Time (ms)") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
stroopRTSTDPlot


# # # # # #STROOP CONGRUOUS ACC# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopCongruousAcc"],both.means["cycle1.means","PostStroopCongruousAcc"],both.means["cycle2.means","PreStroopCongruousAcc"],both.means["cycle2.means","PostStroopCongruousAcc"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
stroopACCStandDev = sd(dat$value)
stroopACCSE = stroopACCStandDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Congruous ACC")


# basic ggplot of data
stroopACCPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - stroopACCSE, ymax = value + stroopACCSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Congruous ACC") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Score") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
stroopACCPlot


# stack Stroop plots
multiplot(stroopRTPlot, stroopRTSTDPlot, stroopACCPlot)
par(mfrow=c(1,1))



# # # # # #STROOP INCONGRUOUS RT# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopIncongruousRT"],both.means["cycle1.means","PostStroopIncongruousRT"],both.means["cycle2.means","PreStroopIncongruousRT"],both.means["cycle2.means","PostStroopIncongruousRT"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
stroopRTStandDev = sd(dat$value)
stroopRTSE = stroopRTStandDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Incongruous RT")


# basic ggplot of data
stroopRTPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - stroopRTSE, ymax = value + stroopRTSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Incongruous RT") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Reaction Time (ms)") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
stroopRTPlot


# # # # # #STROOP INCONGRUOUS RT STD# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopIncongruousRTStd"],both.means["cycle1.means","PostStroopIncongruousRTStd"],both.means["cycle2.means","PreStroopIncongruousRTStd"],both.means["cycle2.means","PostStroopIncongruousRTStd"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
stroopRTSTDStandDev = sd(dat$value)
stroopRTSTDSE = stroopRTSTDStandDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Incongruous RT (STD)")


# basic ggplot of data
stroopRTSTDPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - stroopRTSE, ymax = value + stroopRTSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Incongruous RT (STD)") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Reaction Time (ms)") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
stroopRTSTDPlot


# # # # # #STROOP CONGRUOUS ACC# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopIncongruousAcc"],both.means["cycle1.means","PostStroopIncongruousAcc"],both.means["cycle2.means","PreStroopIncongruousAcc"],both.means["cycle2.means","PostStroopIncongruousAcc"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
stroopACCStandDev = sd(dat$value)
stroopACCSE = stroopACCStandDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Incongruous ACC")


# basic ggplot of data
stroopACCPlot <- ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - stroopACCSE, ymax = value + stroopACCSE),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Incongruous ACC") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Score") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
stroopACCPlot


# stack Symmetry Span plots
multiplot(stroopRTPlot, stroopRTSTDPlot, stroopACCPlot)
par(mfrow=c(1,1))


# # # # # #STROOP EFFECT# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopEffect"],both.means["cycle1.means","PostStroopEffect"],both.means["cycle2.means","PreStroopEffect"],both.means["cycle2.means","PostStroopEffect"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
standDev = sd(dat$value)
se = standDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Effect")


# basic ggplot of data
ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Effect") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Score") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))


# # # # # #STROOP COST# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopCost"],both.means["cycle1.means","PostStroopCost"],both.means["cycle2.means","PreStroopCost"],both.means["cycle2.means","PostStroopCost"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
standDev = sd(dat$value)
se = standDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Cost")


# basic ggplot of data
ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Cost") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Score") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))


# # # # # #STROOP BENEFIT# # # 
dat<-data.frame(c(both.means["cycle1.means","PreStroopBenefit"],both.means["cycle1.means","PostStroopBenefit"],both.means["cycle2.means","PreStroopBenefit"],both.means["cycle2.means","PostStroopBenefit"]))
rownames(dat)<-c('Cyle1-Pre','Cyle1-Post','Cycle2-Pre','Cycle2-Post')
colnames(dat)= 'value'
dat$PrePost = c('Pre', 'Post')
dat$Cycle = factor(c('Intervention 1', 'Intervention 1', 'Intervention 2', 'Intervention 2'))
dat$Position = c(1, 2, 3, 4)

# calculate standard error
standDev = sd(dat$value)
se = standDev/2

# basic barplot of data
barplot(t(dat),main="Stroop Benefit")


# basic ggplot of data
ggplot(dat, aes(Cycle, value, fill = PrePost, group = Position)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                size = 1, width = .4, position = position_dodge(.9)) +
  cleanup +
  ggtitle("Stroop Benefit") +
  theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=.55)) +
  xlab(NULL) +
  ylab("Score") +
  scale_fill_manual(labels = c("Post", "Pre"),
                    values = c("blue", "red"),
                    guide = guide_legend(title=NULL, reverse = TRUE))
