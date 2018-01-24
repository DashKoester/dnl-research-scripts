# @author: Dashiell Koester

install.packages('xlsx', dependencies = TRUE)
library(xlsx)

##import/reshape data
round4<-read.table("** Data file ommitted to protect the lab's research **",header=T)
round4$Group <- as.factor(round4$Group)


####IFR WORDS####
IFR <- lm(PostIFR_WordsWordsRecalled ~ PreIFR_WordsWordsRecalled + Group, data = round4)
str(IFR)
IFR.stat <- IFR$coefficients[[3]]
IFR.p <- summary(IFR)$coefficients[,4][[3]]


##create data frame and add first test's data
statAnalysis <- data.frame(rbind("IFR Words" = c(IFR.stat, IFR.p)))
colnames(statAnalysis) <- c("Stat", "P-Value")


####PIC WORDS####
picWords <- lm(PostIFR_PicturesWordsRecalled ~ PreIFR_PicturesWordsRecalled + Group, data = round4)
str(picWords)
picWords.stat <- picWords$coefficients[[3]]
picWords.p <- summary(picWords)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Pic Words" = c(picWords.stat, picWords.p))


####PAIRED ASSOCIATES####
PA <- lm(PostPairedAssociatesWordsRecalled ~ PrePairedAssociatesWordsRecalled + Group, data = round4)
str(PA)
PA.stat <- PA$coefficients[[3]]
PA.p <- summary(PA)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "PA" = c(PA.stat, PA.p))



####PAIRED ASSOCIATES DELAY
PADELAY <- lm(PostPairedAssociates_DelayWordsRecalled ~ PrePairedAssociates_DelayWordsRecalled + Group, data = round4)
str(PADELAY)
PADELAY.stat <- PADELAY$coefficients[[3]]
PADELAY.p <- summary(PADELAY)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "PA Delay" = c(PADELAY.stat, PADELAY.p))


####NUMBER SERIES CORRECT####
NumberSeries <- lm(PostNumberSeriesCorrectTrials ~ PreNumberSeriesCorrectTrials + Group, data=round4)
str(NumberSeries)
NumberSeries.stat <- NumberSeries$coefficients[[3]] ##this is the stats for the test
NumberSeries.p <- summary(NumberSeries)$coefficients[,4][[3]] ##this is the p value for the test
statAnalysis <- rbind(statAnalysis, "NS" = c(NumberSeries.stat, NumberSeries.p))


####NUMBER SERIES RT####
nsRT <- lm(PostNumberSeriesCorrectTrialRT ~ PreNumberSeriesCorrectTrialRT+ Group, data = round4)
str(nsRT)
nsRT.stat <- nsRT$coefficients[[3]]
nsRT.p <- summary(nsRT)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "NS RT" = c(nsRT.stat, nsRT.p))

####NUMBER SERIES RTsd####
nsRTSD <- lm(PostNumberSeriesCorrectTrialRTStd ~ PreNumberSeriesCorrectTrialRTStd + Group, data = round4)
str(nsRTSD)
nsRTSD.stat <- nsRTSD$coefficients[[3]]
nsRTSD.p <- summary(nsRTSD)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "NS RT STD" = c(nsRTSD.stat, nsRTSD.p))


####KEEP TRACK WORDS####
KTW <- lm(PostKeepTrackWordsRecalled ~ PreKeepTrackWordsRecalled + Group, data = round4)
str(KTW)
KTW.stat <- KTW $coefficients[[3]]
KTW.p <- summary(KTW)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Keep Track Words" = c(KTW.stat, KTW.p))


####LETTER SET CORRECT####
lsC <- lm(PostLetterSetsCorrectTrials ~ PreLetterSetsCorrectTrials + Group, data = round4)
str(lsC)
lsC.stat <- lsC $coefficients[[3]]
lsC.p <- summary(lsC)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "LS" = c(lsC.stat, lsC.p))


####LETTER SERIES RT####
lsRT <- lm(PostLetterSetsCorrectTrialRT ~ PreLetterSetsCorrectTrialRT + Group, data = round4)
str(lsRT)
lsRT.stat <- lsRT $coefficients[[3]]
lsRT.p <- summary(lsRT)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "LS RT" = c(lsRT.stat, lsRT.p))


####LETTER SERIES RTSD####
lsRTSD <- lm(PostLetterSetsCorrectTrialRTStd ~ PreLetterSetsCorrectTrialRTStd+ Group, data = round4)
str(lsRTSD)
lsRTSD.stat <- lsRTSD $coefficients[[3]]
lsRTSD.p <- summary(lsRTSD)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "LS RT STD" = c(lsRTSD.stat, lsRTSD.p))


####Rotation Span Total####
rotTOT <- lm(PostRotationSpanTotal ~ PreRotationSpanTotal + Group, data = round4)
str(rotTOT)
rotTOT.stat <- rotTOT $coefficients[[3]]
rotTOT.p <- summary(rotTOT)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "RS Tot" = c(rotTOT.stat, rotTOT.p))


####Rotation Absolute####
rotABS <- lm(PostRotationSpanAbsoluteScore ~ PreRotationSpanAbsoluteScore + Group, data = round4)
str(rotABS)
rotABS.stat <- rotABS $coefficients[[3]]
rotABS.p <- summary(rotABS)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "RS Abs" = c(rotABS.stat, rotABS.p))


####Rotation Error####
rotERR <- lm(PostRotationSpanErrorTotal ~ PreRotationSpanErrorTotal + Group, data = round4)
str(rotERR)
rotERR.stat <- rotERR $coefficients[[3]]
rotERR.p <- summary(rotERR)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "RS Err" = c(rotERR.stat, rotERR.p))


####Symmetry Span Total####
symTOT <- lm(PostSymmetrySpanTotal ~ PreSymmetrySpanTotal + Group, data = round4)
str(symTOT)
symTOT.stat <- symTOT $coefficients[[3]]
symTOT.p <- summary(symTOT)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "SS Tot" = c(symTOT.stat, symTOT.p))


####Symmetry Span Absolute####
symABS <- lm(PostSymmetrySpanAbsoluteScore ~ PreSymmetrySpanAbsoluteScore + Group, data = round4)
str(symABS)
symABS.stat <- symABS $coefficients[[3]]
symABS.p <- summary(symABS)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "SS Abs" = c(symABS.stat, symABS.p))


####Symmetry Span Error####
symERR <- lm(PostSymmetrySpanErrorTotal ~ PreSymmetrySpanErrorTotal + Group, data = round4)
str(symERR)
symERR.stat <- symERR $coefficients[[3]]
symERR.p <- summary(symERR)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "SS Err" = c(symERR.stat, symERR.p))


####Stroop Congruous RT####
stroopCONRT <- lm(PostStroopCongruousRT ~ PreStroopCongruousRT + Group, data = round4)
str(stroopCONRT)
stroopCONRT.stat <- stroopCONRT $coefficients[[3]]
stroopCONRT.p <- summary(stroopCONRT)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stroop Con RT" = c(stroopCONRT.stat, stroopCONRT.p))


####Stroop Congruous RTSTD####
stroopCONRTSD <- lm(PostStroopCongruousRTStd ~ PreStroopCongruousRTStd + Group, data = round4)
str(stroopCONRTSD)
stroopCONRTSD.stat <- stroopCONRTSD $coefficients[[3]]
stroopCONRTSD.p <- summary(stroopCONRTSD)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stroop Con RT STD" = c(stroopCONRTSD.stat, stroopCONRTSD.p))


####STROOP CONGRUOUS ACC####
stroopCONACC <- lm(PostStroopCongruousAcc ~ PreStroopCongruousAcc + Group, data = round4)
str(stroopCONACC)
stroopCONACC.stat <- stroopCONACC $coefficients[[3]]
stroopCONACC.p <- summary(stroopCONACC)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stroop Con Acc" = c(stroopCONACC.stat, stroopCONACC.p))


####STROOP INCONGRUOUS RT####
stroopINCRT <- lm(PostStroopIncongruousRT ~ PreStroopIncongruousRT + Group, data = round4)
str(stroopINCRT)
stroopINCRT.stat <- stroopINCRT $coefficients[[3]]
stroopINCRT.p <- summary(stroopINCRT)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stroop Inc RT" = c(stroopINCRT.stat, stroopINCRT.p))


####STROOP INCONGRUOUS RTSTD####
stroopINCRTSTD <- lm(PostStroopIncongruousRTStd ~ PreStroopIncongruousRTStd + Group, data = round4)
str(stroopINCRTSTD)
stroopINCRTSTD.stat <- stroopINCRTSTD $coefficients[[3]]
stroopINCRTSTD.p <- summary(stroopINCRTSTD)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stroop Inc RT STD" = c(stroopINCRTSTD.stat, stroopINCRTSTD.p))


####STROOP INCONGRUOUS ACC####
stroopINCACC <- lm(PostStroopIncongruousAcc ~ PreStroopIncongruousAcc + Group, data = round4)
str(stroopINCACC)
stroopINCACC.stat <- stroopINCACC $coefficients[[3]]
stroopINCACC.p <- summary(stroopINCACC)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stroop Inc Acc" = c(stroopINCACC.stat, stroopINCACC.p))


####STROOP EFFECT####
stroopE <- lm(PostStroopEffect ~ PreStroopEffect + Group, data = round4)
str(stroopE)
stroopE.stat <- stroopE $coefficients[[3]]
stroopE.p <- summary(stroopE)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stroop Effect" = c(stroopE.stat, stroopE.p))


####STROOP COST####
##stroopC <- lm(PostStroopCost ~ PreStroopCost + C(Group, contr.treatment(2, base = 2)), data = round4) ##SWITCHES THE REFERENCE GROUP
stroopC <- lm(PostStroopCost ~ PreStroopCost + Group, data = round4)
str(stroopC)
stroopC.stat <- stroopC $coefficients[[3]]
stroopC.p <- summary(stroopC)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stroop Cost" = c(stroopC.stat, stroopC.p))


####STROOP BENEFIT####
stroopB <- lm(PostStroopBenefit ~ PreStroopBenefit + Group, data = round4)
str(stroopB)
stroopB.stat <- stroopB $coefficients[[3]]
stroopB.p <- summary(stroopB)$coefficients[,4][[3]]
statAnalysis <- rbind(statAnalysis, "Stoop Benefit" = c(stroopB.stat, stroopB.p))


##write data frame to excel file
write.xlsx(statAnalysis, 'C:/Users/Dash Koester/Desktop/Research Project/Nutrition & Cognition/Statistical Analysis.xlsx')
