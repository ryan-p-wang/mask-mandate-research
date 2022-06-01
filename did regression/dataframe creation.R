#Ryan Wang
#Dataframe creation
setwd("/Users/ryanwang/Dropbox/My Mac (Ryan’s MacBook Pro)/Desktop/mask mandate project/mask mandate project/did regression")
library(sandwich)
library(lmtest)
library(panelView)
library(ggplot2)
library(openxlsx)


data <- read.csv("state_dex copy.csv")
names(data)
dim(data)

data_NV <- data[which(data$state=="NV"),]
data_MT <- data[which(data$state=="MT"),]
data_KS <- data[which(data$state=="KS"),]
data_KY <- data[which(data$state=="KY"),]
data_AL <- data[which(data$state=="AL"),]
data_AZ <- data[which(data$state=="AZ"),]
data_ID <- data[which(data$state=="ID"),]
data_MO <- data[which(data$state=="MO"),]
data_TN <- data[which(data$state=="TN"),]
data_GA <- data[which(data$state=="GA"),]
data_AK <- data[which(data$state=="AK"),]
data_FL <- data[which(data$state=="FL"),]

full_data <- rbind(data_NV, data_MT, data_KS, data_KY, data_AL, data_AZ,
                   data_ID, data_MO, data_TN, data_GA, data_AK, data_FL)
mm_data <- data.frame(full_data$state, full_data$date)
colnames(mm_data)[1] <- "state"
colnames(mm_data)[2] <- "date"

mm_data$treat <- 0
treated <- c("NV", "MT", "KS", "KY", "AL")
for (i in treated){
  mm_data[which(mm_data$state==i),]$treat <- 1
}

mm_data$post <- 0
mm_data$date<-as.Date(mm_data$date)
mm_data[which(mm_data$state=="NV" & mm_data$date>=as.Date("2020-06-26")),]$post <- 1
mm_data[which(mm_data$state=="MT" & mm_data$date>=as.Date("2020-07-15")),]$post <- 1
mm_data[which(mm_data$state=="KS" & mm_data$date>=as.Date("2020-07-03")),]$post <- 1
mm_data[which(mm_data$state=="KY" & mm_data$date>=as.Date("2020-07-10")),]$post <- 1
mm_data[which(mm_data$state=="AL" & mm_data$date>=as.Date("2020-07-16")),]$post <- 1
mm_data$treat_post <- 0
mm_data$treat_post <- mm_data$treat*mm_data$post

dex_data <- rbind(data_NV[,3:28], data_MT[,3:28], data_KS[,3:28],
                  data_KY[,3:28], data_AL[,3:28], data_AZ[,3:28],
                  data_ID[,3:28], data_MO[,3:28], data_TN[,3:28],
                  data_GA[,3:28], data_AK[,3:28], data_FL[,3:28])
mm_data <- cbind(mm_data, dex_data)

OxCGRT_data <- read.csv("OxCGRT_US_latest.csv")
OxCGRT_data$Date <- as.character(OxCGRT_data$Date)
states <- c("Nevada", "Montana", "Kansas", "Kentucky", "Alabama",
            "Arizona", "Idaho", "Missouri", "Tennessee", "Georgia",
            "Alaska", "Florida")
OxCGRT_frame <- data.frame()
for (i in states){
  OxCGRT_frame <- rbind(OxCGRT_frame, OxCGRT_data[which(OxCGRT_data$RegionName==i
                        & as.Date(OxCGRT_data$Date, "%Y %m %d")>=as.Date("2020-01-20")
                        & as.Date(OxCGRT_data$Date, "%Y %m %d")<=as.Date("2021-08-06")),])
}
mm_data$stringencyindex <- OxCGRT_frame$StringencyIndex
mm_data$economicsupportindex <- OxCGRT_frame$EconomicSupportIndex

risk_data <- read.csv("COVIDrisk.csv")
risk_data <- cbind(risk_data$date, risk_data$state, risk_data$riskLevels.overall)
colnames(risk_data) <- c("date", "state", "risklevelsoverall")
states <- c("NV", "MT", "KS", "KY", "AL", "AZ", "ID", "MO", "TN",
            "GA", "AK", "FL")
Risk_frame <- data.frame()
for (i in states){
  riskbystate <- rbind(risk_data[which(risk_data[,2]==i),])
  daydiff <- as.Date(riskbystate[1, 1]) - as.Date("2020-1-20")
  daydiff_frame <- data.frame(matrix(0, nrow = daydiff, ncol = 3))
  colnames(daydiff_frame) <- c("date", "state", "risklevelsoverall")
  Risk_frame <- rbind(Risk_frame, daydiff_frame,
                      risk_data[which(risk_data[,2]==i &
                                      risk_data[,1]<=as.Date("2021-08-06")),])
  remove(riskbystate, daydiff)
}

mm_data$risklevelsoverall <- Risk_frame$risklevelsoverall

mei_data <- read.csv("MEI_states_scaled.csv")
mei_frame <- data.frame()
for (i in states){
  meibystate <- mei_data[,i]
  meibystate <- meibystate[which(as.Date(mei_data$Time, format="%d-%b-%Y")>=as.Date("2020-01-20")
                                 & as.Date(mei_data$Time, format="%d-%b-%Y")<=as.Date("2021-08-06"))]
  daydiff <- as.Date("2021-08-06") - as.Date("2021-03-27") + 1
  daydiff_frame <- data.frame(matrix(NA, nrow = daydiff, ncol = 1))
  colnames(daydiff_frame) <- c("meibystate")
  mei_frame <- rbind(mei_frame, data.frame(meibystate), daydiff_frame)
}

mm_data$mei <- mei_frame$meibystate

write.xlsx(mm_data, "/Users/ryanwang/Dropbox/My Mac (Ryan’s MacBook Pro)/Desktop/mask mandate project/mask mandate project/did regression/mm_data.xlsx",
           overwrite = TRUE)

