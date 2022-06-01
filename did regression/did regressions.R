#Ryan Wang
#did regressions
setwd("/Users/ryanwang/Dropbox/My Mac (Ryan’s MacBook Pro)/Desktop/mask mandate project/mask mandate project/did regression")
library(sandwich)
library(lmtest)
library(panelView)
library(ggplot2)
library(broom)
library(openxlsx)
library(dplyr)
library(gsynth)

data <- read.xlsx("mm_data.xlsx", detectDates = TRUE)
data <- data[which(as.Date(data$date)<=as.Date("2020-12-31")),]
names(data)
dim(data)
summary.data.frame(data)


#model 1: dex
#model 2: mei

#Treatment 1: 5 treatment vs. 7 control
m1 <- lm(dex~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data) %>% coeftest() %>% tidy()
m1<-m1[1:8,]

m2 <- lm(mei~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data) %>% coeftest() %>% tidy()
m2<-m2[1:8,]


#Treatment 2: Alabama vs. Georgia
data2 <- data[which(data$state=="AL" | data$state=="GA"),]

m3 <- lm(dex~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data2) %>% coeftest() %>% tidy()
m3<-m3[1:8,]

m4 <- lm(mei~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data2) %>% coeftest() %>% tidy()
m4<-m4[1:8,]


#Treatment 3: Kansas vs. Missouri
data3 <- data[which(data$state=="KS" | data$state=="MO"),]

m5 <- lm(dex~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data3) %>% coeftest() %>% tidy()
m5<-m5[1:8,]

m6 <- lm(mei~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data3) %>% coeftest() %>% tidy()
m6<-m6[1:8,]


#Treatment 4: Kentucky vs. Tennessee
data4 <- data[which(data$state=="KY" | data$state=="TN"),]

m7 <- lm(dex~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data4) %>% coeftest() %>% tidy()
m7<-m7[1:8,]

m8 <- lm(mei~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data4) %>% coeftest() %>% tidy()
m8<-m8[1:8,]


#Treatment 5: Montana vs. Idaho
data5 <- data[which(data$state=="MT" | data$state=="ID"),]

m9 <- lm(dex~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data5) %>% coeftest() %>% tidy()
m9<-m9[1:8,]

m10 <- lm(mei~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data5) %>% coeftest() %>% tidy()
m10<-m10[1:8,]


#Treatment 6: Nevada vs. Arizona
data6 <- data[which(data$state=="NV" | data$state=="AZ"),]

m11 <- lm(dex~ treat_post+stringencyindex+risklevelsoverall+num_devices+
           factor(state)+factor(date), data = data6) %>% coeftest() %>% tidy()
m11<-m11[1:8,]

m12 <- lm(mei~ treat_post+stringencyindex+risklevelsoverall+num_devices+
            factor(state)+factor(date), data = data6) %>% coeftest() %>% tidy()
m12<-m12[1:8,]

### Output the results

m1$estimate<-as.character(m1$estimate)
m1$std.error<-paste0("(",as.character(m1$std.error),")")
m1[which(m1[,5]<=0.01),]$estimate<-paste0(m1[which(m1[,5]<0.01),]$estimate,"***")
m1[which(m1[,5]<=0.05 & m1[,5]>0.01),]$estimate<-paste0(m1[which(m1[,5]<=0.05 & m1[,5]>0.01),]$estimate,"**")
m1[which(m1[,5]<=0.10 & m1[,5]>0.05),]$estimate<-paste0(m1[which(m1[,5]<=0.10 & m1[,5]>0.05),]$estimate,"*")
estimate<-c(m1$estimate[1],m1$std.error[1] ,m1$estimate[2],m1$std.error[2],m1$estimate[3],m1$std.error[3],m1$estimate[4],m1$std.error[4],m1$estimate[5],m1$std.error[5],m1$estimate[6],m1$std.error[6],m1$estimate[7],m1$std.error[7],m1$estimate[8],m1$std.error[8])
m1<-data.frame(estimate)

m2$estimate<-as.character(m2$estimate)
m2$std.error<-paste0("(",as.character(m2$std.error),")")
m2[which(m2[,5]<=0.01),]$estimate<-paste0(m2[which(m2[,5]<0.01),]$estimate,"***")
m2[which(m2[,5]<=0.05 & m2[,5]>0.01),]$estimate<-paste0(m2[which(m2[,5]<=0.05 & m2[,5]>0.01),]$estimate,"**")
m2[which(m2[,5]<=0.10 & m2[,5]>0.05),]$estimate<-paste0(m2[which(m2[,5]<=0.10 & m2[,5]>0.05),]$estimate,"*")
estimate<-c(m2$estimate[1],m2$std.error[1] ,m2$estimate[2],m2$std.error[2],m2$estimate[3],m2$std.error[3],m2$estimate[4],m2$std.error[4],m2$estimate[5],m2$std.error[5],m2$estimate[6],m2$std.error[6],m2$estimate[7],m2$std.error[7],m2$estimate[8],m2$std.error[8])
m2<-data.frame(estimate)

m3$estimate<-as.character(m3$estimate)
m3$std.error<-paste0("(",as.character(m3$std.error),")")
m3[which(m3[,5]<=0.01),]$estimate<-paste0(m3[which(m3[,5]<0.01),]$estimate,"***")
m3[which(m3[,5]<=0.05 & m3[,5]>0.01),]$estimate<-paste0(m3[which(m3[,5]<=0.05 & m3[,5]>0.01),]$estimate,"**")
m3[which(m3[,5]<=0.10 & m3[,5]>0.05),]$estimate<-paste0(m3[which(m3[,5]<=0.10 & m3[,5]>0.05),]$estimate,"*")
estimate<-c(m3$estimate[1],m3$std.error[1] ,m3$estimate[2],m3$std.error[2],m3$estimate[3],m3$std.error[3],m3$estimate[4],m3$std.error[4],m3$estimate[5],m3$std.error[5],m3$estimate[6],m3$std.error[6],m3$estimate[7],m3$std.error[7],m3$estimate[8],m3$std.error[8])
m3<-data.frame(estimate)

m4$estimate<-as.character(m4$estimate)
m4$std.error<-paste0("(",as.character(m4$std.error),")")
m4[which(m4[,5]<=0.01),]$estimate<-paste0(m4[which(m4[,5]<0.01),]$estimate,"***")
m4[which(m4[,5]<=0.05 & m4[,5]>0.01),]$estimate<-paste0(m4[which(m4[,5]<=0.05 & m4[,5]>0.01),]$estimate,"**")
m4[which(m4[,5]<=0.10 & m4[,5]>0.05),]$estimate<-paste0(m4[which(m4[,5]<=0.10 & m4[,5]>0.05),]$estimate,"*")
estimate<-c(m4$estimate[1],m4$std.error[1] ,m4$estimate[2],m4$std.error[2],m4$estimate[3],m4$std.error[3],m4$estimate[4],m4$std.error[4],m4$estimate[5],m4$std.error[5],m4$estimate[6],m4$std.error[6],m4$estimate[7],m4$std.error[7],m4$estimate[8],m4$std.error[8])
m4<-data.frame(estimate)

m5$estimate<-as.character(m5$estimate)
m5$std.error<-paste0("(",as.character(m5$std.error),")")
m5[which(m5[,5]<=0.01),]$estimate<-paste0(m5[which(m5[,5]<0.01),]$estimate,"***")
m5[which(m5[,5]<=0.05 & m5[,5]>0.01),]$estimate<-paste0(m5[which(m5[,5]<=0.05 & m5[,5]>0.01),]$estimate,"**")
m5[which(m5[,5]<=0.10 & m5[,5]>0.05),]$estimate<-paste0(m5[which(m5[,5]<=0.10 & m5[,5]>0.05),]$estimate,"*")
estimate<-c(m5$estimate[1],m5$std.error[1] ,m5$estimate[2],m5$std.error[2],m5$estimate[3],m5$std.error[3],m5$estimate[4],m5$std.error[4],m5$estimate[5],m5$std.error[5],m5$estimate[6],m5$std.error[6],m5$estimate[7],m5$std.error[7],m5$estimate[8],m5$std.error[8])
m5<-data.frame(estimate)

m6$estimate<-as.character(m6$estimate)
m6$std.error<-paste0("(",as.character(m6$std.error),")")
m6[which(m6[,5]<=0.01),]$estimate<-paste0(m6[which(m6[,5]<0.01),]$estimate,"***")
m6[which(m6[,5]<=0.05 & m6[,5]>0.01),]$estimate<-paste0(m6[which(m6[,5]<=0.05 & m6[,5]>0.01),]$estimate,"**")
m6[which(m6[,5]<=0.10 & m6[,5]>0.05),]$estimate<-paste0(m6[which(m6[,5]<=0.10 & m6[,5]>0.05),]$estimate,"*")
estimate<-c(m6$estimate[1],m6$std.error[1] ,m6$estimate[2],m6$std.error[2],m6$estimate[3],m6$std.error[3],m6$estimate[4],m6$std.error[4],m6$estimate[5],m6$std.error[5],m6$estimate[6],m6$std.error[6],m6$estimate[7],m6$std.error[7],m6$estimate[8],m6$std.error[8])
m6<-data.frame(estimate)

m7$estimate<-as.character(m7$estimate)
m7$std.error<-paste0("(",as.character(m7$std.error),")")
m7[which(m7[,5]<=0.01),]$estimate<-paste0(m7[which(m7[,5]<0.01),]$estimate,"***")
m7[which(m7[,5]<=0.05 & m7[,5]>0.01),]$estimate<-paste0(m7[which(m7[,5]<=0.05 & m7[,5]>0.01),]$estimate,"**")
m7[which(m7[,5]<=0.10 & m7[,5]>0.05),]$estimate<-paste0(m7[which(m7[,5]<=0.10 & m7[,5]>0.05),]$estimate,"*")
estimate<-c(m7$estimate[1],m7$std.error[1] ,m7$estimate[2],m7$std.error[2],m7$estimate[3],m7$std.error[3],m7$estimate[4],m7$std.error[4],m7$estimate[5],m7$std.error[5],m7$estimate[6],m7$std.error[6],m7$estimate[7],m7$std.error[7],m7$estimate[8],m7$std.error[8])
m7<-data.frame(estimate)

m8$estimate<-as.character(m8$estimate)
m8$std.error<-paste0("(",as.character(m8$std.error),")")
m8[which(m8[,5]<=0.01),]$estimate<-paste0(m8[which(m8[,5]<0.01),]$estimate,"***")
m8[which(m8[,5]<=0.05 & m8[,5]>0.01),]$estimate<-paste0(m8[which(m8[,5]<=0.05 & m8[,5]>0.01),]$estimate,"**")
m8[which(m8[,5]<=0.10 & m8[,5]>0.05),]$estimate<-paste0(m8[which(m8[,5]<=0.10 & m8[,5]>0.05),]$estimate,"*")
estimate<-c(m8$estimate[1],m8$std.error[1] ,m8$estimate[2],m8$std.error[2],m8$estimate[3],m8$std.error[3],m8$estimate[4],m8$std.error[4],m8$estimate[5],m8$std.error[5],m8$estimate[6],m8$std.error[6],m8$estimate[7],m8$std.error[7],m8$estimate[8],m8$std.error[8])
m8<-data.frame(estimate)

m9$estimate<-as.character(m9$estimate)
m9$std.error<-paste0("(",as.character(m9$std.error),")")
m9[which(m9[,5]<=0.01),]$estimate<-paste0(m9[which(m9[,5]<0.01),]$estimate,"***")
m9[which(m9[,5]<=0.05 & m9[,5]>0.01),]$estimate<-paste0(m9[which(m9[,5]<=0.05 & m9[,5]>0.01),]$estimate,"**")
m9[which(m9[,5]<=0.10 & m9[,5]>0.05),]$estimate<-paste0(m9[which(m9[,5]<=0.10 & m9[,5]>0.05),]$estimate,"*")
estimate<-c(m9$estimate[1],m9$std.error[1] ,m9$estimate[2],m9$std.error[2],m9$estimate[3],m9$std.error[3],m9$estimate[4],m9$std.error[4],m9$estimate[5],m9$std.error[5],m9$estimate[6],m9$std.error[6],m9$estimate[7],m9$std.error[7],m9$estimate[8],m9$std.error[8])
m9<-data.frame(estimate)

m10$estimate<-as.character(m10$estimate)
m10$std.error<-paste0("(",as.character(m10$std.error),")")
m10[which(m10[,5]<=0.01),]$estimate<-paste0(m10[which(m10[,5]<0.01),]$estimate,"***")
m10[which(m10[,5]<=0.05 & m10[,5]>0.01),]$estimate<-paste0(m10[which(m10[,5]<=0.05 & m10[,5]>0.01),]$estimate,"**")
m10[which(m10[,5]<=0.10 & m10[,5]>0.05),]$estimate<-paste0(m10[which(m10[,5]<=0.10 & m10[,5]>0.05),]$estimate,"*")
estimate<-c(m10$estimate[1],m10$std.error[1] ,m10$estimate[2],m10$std.error[2],m10$estimate[3],m10$std.error[3],m10$estimate[4],m10$std.error[4],m10$estimate[5],m10$std.error[5],m10$estimate[6],m10$std.error[6],m10$estimate[7],m10$std.error[7],m10$estimate[8],m10$std.error[8])
m10<-data.frame(estimate)

m11$estimate<-as.character(m11$estimate)
m11$std.error<-paste0("(",as.character(m11$std.error),")")
m11[which(m11[,5]<=0.01),]$estimate<-paste0(m11[which(m11[,5]<0.01),]$estimate,"***")
m11[which(m11[,5]<=0.05 & m11[,5]>0.01),]$estimate<-paste0(m11[which(m11[,5]<=0.05 & m11[,5]>0.01),]$estimate,"**")
m11[which(m11[,5]<=0.10 & m11[,5]>0.05),]$estimate<-paste0(m11[which(m11[,5]<=0.10 & m11[,5]>0.05),]$estimate,"*")
estimate<-c(m11$estimate[1],m11$std.error[1] ,m11$estimate[2],m11$std.error[2],m11$estimate[3],m11$std.error[3],m11$estimate[4],m11$std.error[4],m11$estimate[5],m11$std.error[5],m11$estimate[6],m11$std.error[6],m11$estimate[7],m11$std.error[7],m11$estimate[8],m11$std.error[8])
m11<-data.frame(estimate)

m12$estimate<-as.character(m12$estimate)
m12$std.error<-paste0("(",as.character(m12$std.error),")")
m12[which(m12[,5]<=0.01),]$estimate<-paste0(m12[which(m12[,5]<0.01),]$estimate,"***")
m12[which(m12[,5]<=0.05 & m12[,5]>0.01),]$estimate<-paste0(m12[which(m12[,5]<=0.05 & m12[,5]>0.01),]$estimate,"**")
m12[which(m12[,5]<=0.10 & m12[,5]>0.05),]$estimate<-paste0(m12[which(m12[,5]<=0.10 & m12[,5]>0.05),]$estimate,"*")
estimate<-c(m12$estimate[1],m12$std.error[1] ,m12$estimate[2],m12$std.error[2],m12$estimate[3],m12$std.error[3],m12$estimate[4],m12$std.error[4],m12$estimate[5],m12$std.error[5],m12$estimate[6],m12$std.error[6],m12$estimate[7],m12$std.error[7],m12$estimate[8],m12$std.error[8])
m12<-data.frame(estimate)

term<-c("Intercept","","Treatment","", "Stringency Index","","Overall risk level1","", "Overall risk level2","", "Overall risk level3" ,"","Overall risk level5","", "Number of Devices","")

did<-cbind(term,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
colnames(did)<-c("term","(1)", "(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)","(11)","(12)")

View(did)
write.xlsx(did, "/Users/ryanwang/Dropbox/My Mac (Ryan’s MacBook Pro)/Desktop/mask mandate project/mask mandate project/did regression/did.xlsx", overwite = T)

#stargazer(did,summary=F,rownames=F)
