##created by user#59.299 ####
############################################################################################
rm(list=ls())
getwd()
#setwd("E:/Phenotype/mahdi.akbz")
setwd("YourDirectory")
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("rio")
library(cowplot)
library(ggplot2)
library(rio)
##########################(data.reading)###############################################################
#1- Save your data into "setwd" directory without header. Also, you should know the column number
#of your variables. For example, as seen bellow, #col1 is the "PID" or #col5 is the "gender". 
#2- Replac all "TargetVar" with "Your Variable Name", for example "tg". 
#3- Run all line in R and follow your data in ".sav" and ".csv" format in your directory. 
#Note: Before use transformed variables into your work, please check and compare qqplots and histograms
#for makeing a final decision.
########################################################################################################
data<-read.delim("Data.Name",sep="\t",header=FALSE)
data.TargetVar<-data.frame(data$V1,data$V2,data$V5,data$V12,data$V13,data$V14,data$V15,data$V16,data$V17,data$V18,data$V19,data$V20,data$V21,data$V22,data$V23,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
names(data.TargetVar)<-c("PID","PN","gender","age1","age2","age3","age4","age5","age6","TargetVar1","TargetVar2","TargetVar3","TargetVar4","TargetVar5","TargetVar6","rTargetVar1","rTargetVar2","rTargetVar3","rTargetVar4","rTargetVar5","rTargetVar6","sTargetVar1","sTargetVar2","sTargetVar3","sTargetVar4","sTargetVar5","sTargetVar6","wsTargetVar1","wsTargetVar2","wsTargetVar3","wsTargetVar4","wsTargetVar5","wsTargetVar6")
##########################(winsorizing function)##############################################
winsor<-function (x, fraction=.01)
{
   if(length(fraction) != 1 || fraction < 0 ||
         fraction > 0.5) {
      stop("bad value for 'fraction'")
   }
   lim <- quantile(x, probs=c(fraction, 1-fraction),na.rm=TRUE)
   x[ x < lim[1] ] <- lim[1]
   x[ x > lim[2] ] <- lim[2]
   x
}
#wins.STZ.TargetVar1<-winsor(STZ.TargetVar1)
##########################(standardizing function)###########################################
stzn<-function (x)
	{
	x-mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE)
	}
##########################(residualinzing)####################################################
attach(data.TargetVar)
lmTargetVar1<-lm(TargetVar1~age1+gender)
lmTargetVar2<-lm(TargetVar2~age2+gender)
lmTargetVar3<-lm(TargetVar3~age3+gender)
lmTargetVar4<-lm(TargetVar4~age4+gender)
lmTargetVar5<-lm(TargetVar5~age5+gender)
lmTargetVar6<-lm(TargetVar6~age6+gender)

data.TargetVar[names(lmTargetVar1$residuals),"rTargetVar1"]<-lmTargetVar1$residuals
data.TargetVar[names(lmTargetVar2$residuals),"rTargetVar2"]<-lmTargetVar2$residuals
data.TargetVar[names(lmTargetVar3$residuals),"rTargetVar3"]<-lmTargetVar3$residuals
data.TargetVar[names(lmTargetVar4$residuals),"rTargetVar4"]<-lmTargetVar4$residuals
data.TargetVar[names(lmTargetVar5$residuals),"rTargetVar5"]<-lmTargetVar5$residuals
data.TargetVar[names(lmTargetVar6$residuals),"rTargetVar6"]<-lmTargetVar6$residuals

##########################(standardizing)############################################
data.TargetVar$sTargetVar1<-stzn(data.TargetVar$rTargetVar1)
data.TargetVar$sTargetVar2<-stzn(data.TargetVar$rTargetVar2)
data.TargetVar$sTargetVar3<-stzn(data.TargetVar$rTargetVar3)
data.TargetVar$sTargetVar4<-stzn(data.TargetVar$rTargetVar4)
data.TargetVar$sTargetVar5<-stzn(data.TargetVar$rTargetVar5)
data.TargetVar$sTargetVar6<-stzn(data.TargetVar$rTargetVar6)
##########################(Winsorization)##########################################
data.TargetVar$wsTargetVar1<-winsor(data.TargetVar$sTargetVar1)
data.TargetVar$wsTargetVar2<-winsor(data.TargetVar$sTargetVar2)
data.TargetVar$wsTargetVar3<-winsor(data.TargetVar$sTargetVar3)
data.TargetVar$wsTargetVar4<-winsor(data.TargetVar$sTargetVar4)
data.TargetVar$wsTargetVar5<-winsor(data.TargetVar$sTargetVar5)
data.TargetVar$wsTargetVar6<-winsor(data.TargetVar$sTargetVar6)
##########################(Checking.result)##########################################
prTargetVar1<-qplot(sample = data.TargetVar$rTargetVar1)
prTargetVar2<-qplot(sample = data.TargetVar$rTargetVar2)
prTargetVar3<-qplot(sample = data.TargetVar$rTargetVar3)
prTargetVar4<-qplot(sample = data.TargetVar$rTargetVar4)
prTargetVar5<-qplot(sample = data.TargetVar$rTargetVar5)
prTargetVar6<-qplot(sample = data.TargetVar$rTargetVar6)
pTargetVar1<-qplot(sample = data.TargetVar$wsTargetVar1)
pTargetVar2<-qplot(sample = data.TargetVar$wsTargetVar2)
pTargetVar3<-qplot(sample = data.TargetVar$wsTargetVar3)
pTargetVar4<-qplot(sample = data.TargetVar$wsTargetVar4)
pTargetVar5<-qplot(sample = data.TargetVar$wsTargetVar5)
pTargetVar6<-qplot(sample = data.TargetVar$wsTargetVar6)

plot_grid(prTargetVar1,prTargetVar2,prTargetVar3,prTargetVar4,prTargetVar5,prTargetVar6,pTargetVar1,pTargetVar2,pTargetVar3,pTargetVar4,pTargetVar5,pTargetVar6,labels=c("prTargetVar1","prTargetVar2","prTargetVar3","prTargetVar4","prTargetVar5","prTargetVar6","pTargetVar1","pTargetVar2","pTargetVar3","pTargetVar4","pTargetVar5","pTargetVar6"))

par(mfrow=c(2,6))
hist(data.TargetVar$rTargetVar1)
hist(data.TargetVar$rTargetVar2)
hist(data.TargetVar$rTargetVar3)
hist(data.TargetVar$rTargetVar4)
hist(data.TargetVar$rTargetVar5)
hist(data.TargetVar$rTargetVar6)
hist(data.TargetVar$wsTargetVar1)
hist(data.TargetVar$wsTargetVar2)
hist(data.TargetVar$wsTargetVar3)
hist(data.TargetVar$wsTargetVar4)
hist(data.TargetVar$wsTargetVar5)
hist(data.TargetVar$wsTargetVar6)
################################# (Exporting DAta) #####################################
#Totally
data.TargetVar.ex<-data.frame(data.TargetVar$PID,data.TargetVar$PN,data.TargetVar$gender,data.TargetVar$age1,data.TargetVar$age2,data.TargetVar$age3,data.TargetVar$age4,data.TargetVar$age5,data.TargetVar$age6,data.TargetVar$wsTargetVar1,data.TargetVar$wsTargetVar2,data.TargetVar$wsTargetVar3,data.TargetVar$wsTargetVar4,data.TargetVar$wsTargetVar5,data.TargetVar$wsTargetVar6)
names(data.TargetVar.ex)<-c("PID","PN","gender","age1","age2","age3","age4","age5","age6","TargetVar1","TargetVar2","TargetVar3","TargetVar4","TargetVar5","TargetVar6")
export(data.TargetVar.ex,"data.TargetVar.ex.csv")
convert("data.TargetVar.ex.csv","data.TargetVar.ex.sav")



