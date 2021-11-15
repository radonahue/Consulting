library(openxlsx)
library(dplyr)
library(meta)
library(metafor)
data<-read.xlsx("Final Studies_Meta Analysis_1.3.xlsx", rows =2:15)
#trying to fill in some missing values that were merged/blank in the excel file
data[2,1]<-data[1,1]
data[3,1]<-data[1,1]
data[2,2]<-data[1,2]
data[3,2]<-data[1,2]
data[2,3]<-data[1,3]
data[3,3]<-data[1,3]
data[2,5]<-data[1,5]
data[3,5]<-data[1,5]

#stripping out the confidence intervals and converting them back to numeric
#data$test<-data$Mean.Difference.in.Systolic.BP

data[1,7]<-substr(data[1,7], 2, 5)
data[2,7]<-substr(data[2,7], 2, 5)
data[3,7]<-substr(data[3,7], 2, 4)
data[4,7]<-substr(data[4,7], 2, 6)
data[5,7]<-substr(data[5,7], 2, 7)
data[6,7]<-substr(data[6,7], 2, 7)
#3 mo for study 5 was excluded originally, so I'm trimming it here
data[7,7]<-substr(data[7,7], 25, 30)
data[8,7]<-substr(data[8,7], 2, 4)

data$Mean.Difference.in.Systolic.BP<-as.numeric(data$Mean.Difference.in.Systolic.BP)

#data$test2<-(data$Mean.Difference.in.Diastolic.BP)

data[1,8]<-substr(data[1,8], 2, 4)
data[2,8]<-substr(data[2,8], 2, 4)
data[3,8]<-substr(data[3,8], 2, 4)
data[4,8]<-substr(data[4,8], 2, 5)
data[5,8]<-substr(data[5,8], 2, 8)
data[6,8]<-substr(data[6,8], 2, 8)
#3 mo for study 5 was excluded originally, so I'm trimming it here
data[7,8]<-substr(data[7,8], 26, 30)
data[8,8]<-substr(data[8,8], 2, 4)

data$Mean.Difference.in.Diastolic.BP<-as.numeric(data$Mean.Difference.in.Diastolic.BP)

#dealing with the merged cell issue
data[2,9]<-substr(data[1,9], 8, 13)
data[1,9]<-substr(data[1,9], 1, 5)
#trimming out 3 mo
data[7,9]<-substr(data[7,9], 9, 15)

#SBP
data[2, 10]<-substr(data[1, 10], 7, 11)
data[1, 10]<-substr(data[1, 10], 1, 5)
#trimming out 6 months
data[7, 10]<-substr(data[7, 10], 8, 13)

data$SBP<-as.numeric(data$SBP)

#DBP
data[2, 11]<-substr(data[1, 11], 7, 11)
data[1, 11]<-substr(data[1, 11], 1, 5)
data[7, 11]<-substr(data[7, 11], 7, 13)

data$DBP<-as.numeric(data$DBP)

#In.Months
data[7,12]<-substr(data[7,12], 3, 3)
#As.given.in.study
data[7,13]<-substr(data[7,13], 5, 12)

#Fixing duration
data[2,12]<-data[1, 12]
data[3,12]<-data[1, 12]

data$In.months<-as.numeric(data$In.months)

meta<-data[,c(1, 6:8, 10:12)]

names(meta) <- c("StudyID", "Size", "MeanDiff_SBP", "MeanDiff_DBP", "SE_SBP", "SE_DBP", "Duration_M")

meta_SBP <- metagen(TE = MeanDiff_SBP, seTE = SE_SBP, studlab = StudyID, data = meta, sm = "MD", comb.fixed = F, comb.random = T, method.tau = "SJ", title = "SBP", prediction = TRUE)
meta_SBP

funnel.meta(meta_SBP, studlab = TRUE)

meta_DBP <- metagen(TE = MeanDiff_DBP, seTE = SE_DBP, studlab = StudyID, data = meta, sm = "MD", comb.fixed = F, comb.random = T, method.tau = "SJ", title = "DBP", prediction = TRUE)
meta_DBP

funnel.meta(meta_DBP, studlab = TRUE)
