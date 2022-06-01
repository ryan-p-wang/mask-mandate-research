#Ryan Wang
#dex state plots
setwd("/Users/ryanwang/Dropbox/My Mac (Ryan’s MacBook Pro)/Desktop/mask mandate project/mask mandate project/model-free analysis")
library(sandwich)
library(lmtest)
library(panelView)
library(ggplot2)

data <- read.csv("state_dex.csv")
data <- data[which(as.Date(data$date)<=as.Date("2020-12-31")),]
names(data)
dim(data)

#DEX plots of Treatment States
data_AL <- data[which(data$state=="AL"),]
summary(data_AL)
ALdf <- data.frame(Time=c(time(data_AL$date)),Dex=c(data_AL$dex))
ALplot <- ggplot(ALdf,aes(x=Time,y=Dex))
ALplot + geom_line(colour = 'blue') + xlab('Date') + ylab('DEX') +
  geom_vline(xintercept = as.Date("2020-07-16") - as.Date("2020-01-20"),
             linetype = "dashed") +
  ggtitle('Device Exposure in Alabama')

data_KS <- data[which(data$state=="KS"),]
summary(data_KS)
KSdf <- data.frame(Time=c(time(data_KS$date)),Dex=c(data_KS$dex))
KSplot <- ggplot(KSdf,aes(x=Time,y=Dex))
KSplot + geom_line(colour = 'blue') + xlab('Date') + ylab('DEX') +
  geom_vline(xintercept = as.Date("2020-07-03") - as.Date("2020-01-20"),
             linetype = "dashed") +
  ggtitle('Device Exposure in Kansas')

data_KY <- data[which(data$state=="KY"),]
summary(data_KY)
KYdf <- data.frame(Time=c(time(data_KY$date)),Dex=c(data_KY$dex))
KYplot <- ggplot(KYdf,aes(x=Time,y=Dex))
KYplot + geom_line(colour = 'blue') + xlab('Date') + ylab('DEX') +
  geom_vline(xintercept = as.Date("2020-07-10") - as.Date("2020-01-20"),
             linetype = "dashed") +
  ggtitle('Device Exposure in Kentucky')

data_MT <- data[which(data$state=="MT"),]
summary(data_MT)
MTdf <- data.frame(Time=c(time(data_MT$date)),Dex=c(data_MT$dex))
MTplot <- ggplot(MTdf,aes(x=Time,y=Dex))
MTplot + geom_line(colour = 'blue') + xlab('Date') + ylab('DEX') +
  geom_vline(xintercept = as.Date("2020-07-15") - as.Date("2020-01-20"),
             linetype = "dashed") +
  ggtitle('Device Exposure in Montana')

data_NV <- data[which(data$state=="NV"),]
summary(data_NV)
NVdf <- data.frame(Time=c(time(data_NV$date)),Dex=c(data_NV$dex))
NVplot <- ggplot(NVdf,aes(x=Time,y=Dex))
NVplot + geom_line(colour = 'blue') + xlab('Date') + ylab('DEX') +
  geom_vline(xintercept = as.Date("2020-06-26") - as.Date("2020-01-20"),
             linetype = "dashed") +
  ggtitle('Device Exposure in Nevada')


#DEX plots of Control States
data_GA <- data[which(data$state=="GA"),]
summary(data_GA)
GAdf <- data.frame(Time=c(time(data_GA$date)),Dex=c(data_GA$dex))
GAplot <- ggplot(GAdf,aes(x=Time,y=Dex))
GAplot + geom_line(colour = 'red') + xlab('Date') + ylab('DEX') +
  ggtitle('Device Exposure in Georgia')

data_MO <- data[which(data$state=="MO"),]
summary(data_MO)
MOdf <- data.frame(Time=c(time(data_MO$date)),Dex=c(data_MO$dex))
MOplot <- ggplot(MOdf,aes(x=Time,y=Dex))
MOplot + geom_line(colour = 'red') + xlab('Date') + ylab('DEX') +
  ggtitle('Device Exposure in Missouri')

data_TN <- data[which(data$state=="TN"),]
summary(data_TN)
TNdf <- data.frame(Time=c(time(data_TN$date)),Dex=c(data_TN$dex))
TNplot <- ggplot(TNdf,aes(x=Time,y=Dex))
TNplot + geom_line(colour = 'red') + xlab('Date') + ylab('DEX') +
  ggtitle('Device Exposure in Tennessee')

data_ID <- data[which(data$state=="ID"),]
summary(data_ID)
IDdf <- data.frame(Time=c(time(data_ID$date)),Dex=c(data_ID$dex))
IDplot <- ggplot(IDdf,aes(x=Time,y=Dex))
IDplot + geom_line(colour = 'red') + xlab('Date') + ylab('DEX') +
  ggtitle('Device Exposure in Idaho')

data_AZ <- data[which(data$state=="AZ"),]
summary(data_AZ)
AZdf <- data.frame(Time=c(time(data_AZ$date)),Dex=c(data_AZ$dex))
AZplot <- ggplot(AZdf,aes(x=Time,y=Dex))
AZplot + geom_line(colour = 'red') + xlab('Date') + ylab('DEX') +
  ggtitle('Device Exposure in Arizona')

data_AK <- data[which(data$state=="AK"),]
summary(data_AK)
AKdf <- data.frame(Time=c(time(data_AK$date)),Dex=c(data_AK$dex))
AKplot <- ggplot(AKdf,aes(x=Time,y=Dex))
AKplot + geom_line(colour = 'red') + xlab('Date') + ylab('DEX') +
  ggtitle('Device Exposure in Alaska')

data_FL <- data[which(data$state=="FL"),]
summary(data_FL)
FLdf <- data.frame(Time=c(time(data_FL$date)),Dex=c(data_FL$dex))
FLplot <- ggplot(FLdf,aes(x=Time,y=Dex))
FLplot + geom_line(colour = 'red') + xlab('Date') + ylab('DEX') +
  ggtitle('Device Exposure in Florida')

#5 Number Summary of Treatment & Control Groups
data_treat <- data[which(data$state=="AL" | data$state=="KS" | data$state=="KY" | data$state=="MT" | data$state=="NV"),]
summary(data_treat)

data_control <- data[which(data$state=="GA" | data$state=="MO" | data$state=="TN" | data$state=="ID" | data$state=="AZ" | data$state=="AK" | data$state=="FL"),]
summary(data_control)

