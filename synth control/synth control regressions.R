#Ryan Wang
#Synthetic Control
setwd("/Users/ryanwang/Dropbox/My Mac (Ryan’s MacBook Pro)/Desktop/mask mandate project/mask mandate project/synth control")
library(sandwich)
library(lmtest)
library(panelView)
library(ggplot2)
library(broom)
library(openxlsx)
library(dplyr)
library(gsynth)
library(stargazer)

data <- read.xlsx('mm_data copy.xlsx', detectDates = TRUE)
data <- data[which(as.Date(data$date)<=as.Date("2020-12-31")),]
data$risklevelsoverallnum <- as.numeric(data$risklevelsoverall)
names(data)
dim(data)
summary.data.frame(data)

#model 1: dex
#model 2: mei

#Treatment 1: 5 treatment vs. 7 control
out1 <- gsynth(dex~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph1 <- plot(out1, type = "counterfactual", raw = "none", main = "All States Gsynth - DEX")

out2 <- gsynth(mei~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph2 <- plot(out2, type = "counterfactual", raw = "none", main = "All States Gsynth - MEI")

#Treatment 2: Alabama vs. 7 control
data2 <- data[which(data$state=="AL" | data$treat==0),]
out3 <- gsynth(dex~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data2, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph3 <- plot(out3, type = "counterfactual", raw = "none", main = "Alabama Gsynth - DEX")

out4 <- gsynth(mei~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data2, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph4 <- plot(out4, type = "counterfactual", raw = "none", main = "Alabama Gsynth - MEI")

#Treatment 3: Kansas vs. 7 control
data3 <- data[which(data$state=="KS" | data$treat==0),]
out5 <- gsynth(dex~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data3, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph5 <- plot(out5, type = "counterfactual", raw = "none", main = "Kansas Gsynth - DEX")

out6 <- gsynth(mei~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data3, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph6 <- plot(out6, type = "counterfactual", raw = "none", main = "Kansas Gsynth - MEI")

#Treatment 4: Kentucky vs. 7 control
data4 <- data[which(data$state=="KY" | data$treat==0),]
out7 <- gsynth(dex~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data4, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph7 <- plot(out7, type = "counterfactual", raw = "none", main = "Kentucky Gsynth - DEX")

out8 <- gsynth(mei~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data4, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph8 <- plot(out8, type = "counterfactual", raw = "none", main = "Kentucky Gsynth - MEI")

#Treatment 5: Montana vs. 7 control
data5 <- data[which(data$state=="MT" | data$treat==0),]
out9 <- gsynth(dex~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data5, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph9 <- plot(out9, type = "counterfactual", raw = "none", main = "Montana Gsynth - DEX")

out10 <- gsynth(mei~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data5, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph10 <- plot(out10, type = "counterfactual", raw = "none", main = "Montana Gsynth - MEI")

#Treatment 6: Nevada vs. 7 control
data6 <- data[which(data$state=="NV" | data$treat==0),]
out11 <- gsynth(dex~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data6, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph11 <- plot(out11, type = "counterfactual", raw = "none", main = "Nevada Gsynth - DEX")

out12 <- gsynth(mei~ treat_post+stringencyindex+risklevelsoverallnum+num_devices, data = data6, index = c("state", "date"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE, na.rm = TRUE)
graph12 <- plot(out12, type = "counterfactual", raw = "none", main = "Nevada Gsynth - MEI")

#Compiling Results
m3 <- rbind(out3$est.avg, out3$est.beta) %>% as.data.frame() %>% round(.,5)
m4 <- rbind(out4$est.avg, out4$est.beta) %>% as.data.frame() %>% round(.,5)
m5 <- rbind(out5$est.avg, out5$est.beta) %>% as.data.frame() %>% round(.,5)
m6 <- rbind(out6$est.avg, out6$est.beta) %>% as.data.frame() %>% round(.,5)
m7 <- rbind(out7$est.avg, out7$est.beta) %>% as.data.frame() %>% round(.,5)
m8 <- rbind(out8$est.avg, out8$est.beta) %>% as.data.frame() %>% round(.,5)
m9 <- rbind(out9$est.avg, out9$est.beta) %>% as.data.frame() %>% round(.,5)
m10 <- rbind(out10$est.avg, out10$est.beta) %>% as.data.frame() %>% round(.,5)
m11 <- rbind(out11$est.avg, out11$est.beta) %>% as.data.frame() %>% round(.,5)
m12 <- rbind(out12$est.avg, out12$est.beta) %>% as.data.frame() %>% round(.,5)

m3[,1]<-as.character(m3[,1])
rownames(m3)<-c()
m3[which(m3[,5]<=0.01),1]<-paste0(m3[which(m3[,5]<0.01),1],"***")
m3[which(m3[,5]<=0.05 & m3[,5]>0.01),1]<-paste0(m3[which(m3[,5]<=0.05 & m3[,5]>0.01),1],"**")
m3[which(m3[,5]<=0.10 & m3[,5]>0.05),1]<-paste0(m3[which(m3[,5]<=0.10 & m3[,5]>0.05),1],"*")
m3$S.E.<-paste0("(",m3$S.E.,")")
m3<-c(m3$Estimate[1],m3$S.E.[1],m3$Estimate[2],m3$S.E.[2],m3$Estimate[3],m3$S.E.[3],m3$Estimate[4],m3$S.E.[4]) %>% data.frame()

m4[,1]<-as.character(m4[,1])
rownames(m4)<-c()
m4[which(m4[,5]<=0.01),1]<-paste0(m4[which(m4[,5]<0.01),1],"***")
m4[which(m4[,5]<=0.05 & m4[,5]>0.01),1]<-paste0(m4[which(m4[,5]<=0.05 & m4[,5]>0.01),1],"**")
m4[which(m4[,5]<=0.10 & m4[,5]>0.05),1]<-paste0(m4[which(m4[,5]<=0.10 & m4[,5]>0.05),1],"*")
m4$S.E.<-paste0("(",m4$S.E.,")")
m4<-c(m4$Estimate[1],m4$S.E.[1],m4$Estimate[2],m4$S.E.[2],m4$Estimate[3],m4$S.E.[3],m4$Estimate[4],m4$S.E.[4]) %>% data.frame()

m5[,1]<-as.character(m5[,1])
rownames(m5)<-c()
m5[which(m5[,5]<=0.01),1]<-paste0(m5[which(m5[,5]<0.01),1],"***")
m5[which(m5[,5]<=0.05 & m5[,5]>0.01),1]<-paste0(m5[which(m5[,5]<=0.05 & m5[,5]>0.01),1],"**")
m5[which(m5[,5]<=0.10 & m5[,5]>0.05),1]<-paste0(m5[which(m5[,5]<=0.10 & m5[,5]>0.05),1],"*")
m5$S.E.<-paste0("(",m5$S.E.,")")
m5<-c(m5$Estimate[1],m5$S.E.[1],m5$Estimate[2],m5$S.E.[2],m5$Estimate[3],m5$S.E.[3],m5$Estimate[4],m5$S.E.[4]) %>% data.frame()

m6[,1]<-as.character(m6[,1])
rownames(m6)<-c()
m6[which(m6[,5]<=0.01),1]<-paste0(m6[which(m6[,5]<0.01),1],"***")
m6[which(m6[,5]<=0.05 & m6[,5]>0.01),1]<-paste0(m6[which(m6[,5]<=0.05 & m6[,5]>0.01),1],"**")
m6[which(m6[,5]<=0.10 & m6[,5]>0.05),1]<-paste0(m6[which(m6[,5]<=0.10 & m6[,5]>0.05),1],"*")
m6$S.E.<-paste0("(",m6$S.E.,")")
m6<-c(m6$Estimate[1],m6$S.E.[1],m6$Estimate[2],m6$S.E.[2],m6$Estimate[3],m6$S.E.[3],m6$Estimate[4],m6$S.E.[4]) %>% data.frame()

m7[,1]<-as.character(m7[,1])
rownames(m7)<-c()
m7[which(m7[,5]<=0.01),1]<-paste0(m7[which(m7[,5]<0.01),1],"***")
m7[which(m7[,5]<=0.05 & m7[,5]>0.01),1]<-paste0(m7[which(m7[,5]<=0.05 & m7[,5]>0.01),1],"**")
m7[which(m7[,5]<=0.10 & m7[,5]>0.05),1]<-paste0(m7[which(m7[,5]<=0.10 & m7[,5]>0.05),1],"*")
m7$S.E.<-paste0("(",m7$S.E.,")")
m7<-c(m7$Estimate[1],m7$S.E.[1],m7$Estimate[2],m7$S.E.[2],m7$Estimate[3],m7$S.E.[3],m7$Estimate[4],m7$S.E.[4]) %>% data.frame()

m8[,1]<-as.character(m8[,1])
rownames(m8)<-c()
m8[which(m8[,5]<=0.01),1]<-paste0(m8[which(m8[,5]<0.01),1],"***")
m8[which(m8[,5]<=0.05 & m8[,5]>0.01),1]<-paste0(m8[which(m8[,5]<=0.05 & m8[,5]>0.01),1],"**")
m8[which(m8[,5]<=0.10 & m8[,5]>0.05),1]<-paste0(m8[which(m8[,5]<=0.10 & m8[,5]>0.05),1],"*")
m8$S.E.<-paste0("(",m8$S.E.,")")
m8<-c(m8$Estimate[1],m8$S.E.[1],m8$Estimate[2],m8$S.E.[2],m8$Estimate[3],m8$S.E.[3],m8$Estimate[4],m8$S.E.[4]) %>% data.frame()

m9[,1]<-as.character(m9[,1])
rownames(m9)<-c()
m9[which(m9[,5]<=0.01),1]<-paste0(m9[which(m9[,5]<0.01),1],"***")
m9[which(m9[,5]<=0.05 & m9[,5]>0.01),1]<-paste0(m9[which(m9[,5]<=0.05 & m9[,5]>0.01),1],"**")
m9[which(m9[,5]<=0.10 & m9[,5]>0.05),1]<-paste0(m9[which(m9[,5]<=0.10 & m9[,5]>0.05),1],"*")
m9$S.E.<-paste0("(",m9$S.E.,")")
m9<-c(m9$Estimate[1],m9$S.E.[1],m9$Estimate[2],m9$S.E.[2],m9$Estimate[3],m9$S.E.[3],m9$Estimate[4],m9$S.E.[4]) %>% data.frame()

m10[,1]<-as.character(m10[,1])
rownames(m10)<-c()
m10[which(m10[,5]<=0.01),1]<-paste0(m10[which(m10[,5]<0.01),1],"***")
m10[which(m10[,5]<=0.05 & m10[,5]>0.01),1]<-paste0(m10[which(m10[,5]<=0.05 & m10[,5]>0.01),1],"**")
m10[which(m10[,5]<=0.10 & m10[,5]>0.05),1]<-paste0(m10[which(m10[,5]<=0.10 & m10[,5]>0.05),1],"*")
m10$S.E.<-paste0("(",m10$S.E.,")")
m10<-c(m10$Estimate[1],m10$S.E.[1],m10$Estimate[2],m10$S.E.[2],m10$Estimate[3],m10$S.E.[3],m10$Estimate[4],m10$S.E.[4]) %>% data.frame()

m11[,1]<-as.character(m11[,1])
rownames(m11)<-c()
m11[which(m11[,5]<=0.01),1]<-paste0(m11[which(m11[,5]<0.01),1],"***")
m11[which(m11[,5]<=0.05 & m11[,5]>0.01),1]<-paste0(m11[which(m11[,5]<=0.05 & m11[,5]>0.01),1],"**")
m11[which(m11[,5]<=0.10 & m11[,5]>0.05),1]<-paste0(m11[which(m11[,5]<=0.10 & m11[,5]>0.05),1],"*")
m11$S.E.<-paste0("(",m11$S.E.,")")
m11<-c(m11$Estimate[1],m11$S.E.[1],m11$Estimate[2],m11$S.E.[2],m11$Estimate[3],m11$S.E.[3],m11$Estimate[4],m11$S.E.[4]) %>% data.frame()

m12[,1]<-as.character(m12[,1])
rownames(m12)<-c()
m12[which(m12[,5]<=0.01),1]<-paste0(m12[which(m12[,5]<0.01),1],"***")
m12[which(m12[,5]<=0.05 & m12[,5]>0.01),1]<-paste0(m12[which(m12[,5]<=0.05 & m12[,5]>0.01),1],"**")
m12[which(m12[,5]<=0.10 & m12[,5]>0.05),1]<-paste0(m12[which(m12[,5]<=0.10 & m12[,5]>0.05),1],"*")
m12$S.E.<-paste0("(",m12$S.E.,")")
m12<-c(m12$Estimate[1],m12$S.E.[1],m12$Estimate[2],m12$S.E.[2],m12$Estimate[3],m12$S.E.[3],m12$Estimate[4],m12$S.E.[4]) %>% data.frame()

term<-c("Treatment","", "Stringency Index","", "Overall risk level","", "Number of devices","")

gsynth<-cbind(term, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
colnames(gsynth)<-c("term","(3)", "(4)","(5)","(6)","(7)","(8)","(9)","(10)", "(11)", "(12)")


View(gsynth)
write.xlsx(gsynth, "/Users/ryanwang/Dropbox/My Mac (Ryan’s MacBook Pro)/Desktop/mask mandate project/mask mandate project/synth control/gsynth.xlsx", overwite = T)


