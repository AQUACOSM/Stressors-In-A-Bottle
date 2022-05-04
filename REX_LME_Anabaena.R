## REX adaptation of Qing LME script

##run-through with blue concentration
install.packages("nlme")
library(nlme)
install.packages("dplyr")
library(dplyr)
install.packages("magrittr")
library(magrittr)
install.packages("lubridate")
library(lubridate)


###########################################
#### Blue Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

BlueConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","Blue Concentration","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#BlueConc.dat$Date <- ymd(BlueConc.dat$Date)
#Calculating the error bar by range; time series with range
BlueConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                             levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
BlueConc.dat.sum <- na.omit(BlueConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  BlueConc.dat.mean = mean(`Blue Concentration`),
  BlueConc.std.er=sd(`Blue Concentration`)/sqrt(length(`Blue Concentration`))*1.96
)%>%as.data.frame()

treat<-unique(BlueConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
##Plotting the data with error bars
plot(BlueConc.dat.sum$ExperimentDay,BlueConc.dat.sum$BlueConc.dat.mean,ylim = c(min(BlueConc.dat.sum$BlueConc.dat.mean-BlueConc.dat.sum$BlueConc.std.er),max(BlueConc.dat.sum$BlueConc.dat.mean+BlueConc.dat.sum$BlueConc.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8,  blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Anabaena flos-aquae (ug/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=BlueConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=BlueConc.dat.sum[which(BlueConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$BlueConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$BlueConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$BlueConc.dat.mean-box.dat$BlueConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$BlueConc.dat.mean+box.dat$BlueConc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)



#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topleft", legend = c("18캜 & No Runoff","24캜 & No Runoff","18캜 & Runoff","24캜 & Runoff"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
       text.col =col,cex = 1,lwd = c(3,3,3,3)) 

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

###########################################
#### REX adaptation of Qing LME script ####
###########################################

dat.bc <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","Blue Concentration")]
dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.bc.sum<-na.omit(dat.bc)%>%as.data.frame()
#df.bc$Precip_treatment[which(df.bc$ExperimentDay<=14)]<-"AvP" # heatwave simulation after the sampling on 2018-07-02

names(df.bc.sum)[6] <- "Bc_Conc"
df.bc.sum$MicrocosmNumber <- as.factor(df.bc.sum$MicrocosmNumber)

#df.bc.sum$ExperimentDay <- as.numeric(df.bc.sum$ExperimentDay)
df.bc.sum$ExperimentDay <- df.bc.sum$ExperimentDay%>%as.numeric

require(nlme)
require(car)
lme_model <- lme(Bc_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.bc.sum)
shapiro.test(lme_model$residuals[,2])
#normal: 0.0008025
#sqrt: 3.725e-05
#log: 2.16e-06
#recip.: < 2.2e-16

# normality test on the residuals
plot(df.bc.sum$Date,lme_model$residuals[,2])
hist(lme_model$residuals[,2])
#shapiro.test(lme_model$residuals[,2])


# Initial knowlege by the pilot lme model
anova(lme_model)
summary(lme_model)


##### example ####
require(dplyr)
var <- aggregate(df.bc.sum$`Blue Concentration`, by=paste(df.bc.sum$Precip_level, df.bc.sum$ExperimentDay)%>%as.character%>%list, FUN=var)

assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
df.bc.sum$wg <- sapply(paste(df.bc.sum$Precip_level, df.bc.sum$ExperimentDay), assign_var)

lme_model <- lme(VAR~Precip_TREATMENT*Temp_TREATMENT*TIME, weights = varFunc(~wg), random = ~1|SUBJECT, data = df.bc.sum)
shapiro.test(lme_model$residuals[,2])
plot(df.bc.sum$Date,lme_model$residuals[,2])

summary(lme_model)
Anova(lme_model, t=3)

# Test the effect of run-off
#Before runoff event
Before.bc <- df.bc.sum[which(df.bc.sum$ExperimentDay<14),]%>%as.data.frame()
lme_Before.bc <- lme(Bc_Conc~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.bc)

#p-value < 2.2e-16
plot(lme_Before.bc$residuals[,2])
shapiro.test(lme_Before.bc$residuals[,2])
#normal: 4.895e-09
#sqrt: 0.04043
#log: 0.0006151
#recip.: 3.592e-13
anova(lme_Before.bc)
#temp, time significant
summary(lme_Before.bc)
#time, temp*time significant

#After runoff event
After.bc <- df.bc.sum[which(df.bc.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods

#var <- aggregate(After.bc$Bc_Conc, by=paste(After.bc$ExperimentDay, After.bc$Temp_level, After.bc$Precip_level)%>%as.character%>%list, FUN=var)
#assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
#After.bc$wg <- sapply(paste(After.bc$ExperimentDay, After.bc$Temp_level, After.bc$Precip_level), assign_var)

lme_After.bc <- lme(sqrt(Bc_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.bc)

#p-value < 2.2e-16
plot(lme_After.bc$residuals[,2])
shapiro.test(lme_After.bc$residuals[,2])
#sqrt: 0.1884
#log: 6.533e-05
anova(lme_After.bc)
summary(lme_After.bc)
#ExT significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# check raw data treatment for normality
  ##use the raw data from df.bc or use the as.factor altered data (df.bc.sum)?????
#############################
#df.bc.sum as.factor altered data:
#AvT <- df.bc.sum[which(df.bc.sum$Temp_level=="AvT"),]
#BlueCon<-c(df.bc.sum$`Blue Concentration`)
#lme_AvT <- lme(VAR~Precip_TREATMENT*TIME, random = ~1|SUBJECT,data = AvT)
#anova(lme_AvT)
#shapiro.test(lme_AvT$residuals[,2])
  #p-value = 0.03361. Not larger than 0.05, so requires transformation for normality

#############################
#raw data:
#AvT.r <- dat.bc[which(dat.bc$Temp_level=="AvT"),]
#BlueCon<-c(dat.bc$`Blue Concentration`)
#lme_AvT.r <- lme(BlueCon~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber,data = AvT)
  #^error with Precip_level variable length. After solving this, will have same issue with ExperimentDay and MicrocosmNumber
#anova(lme_AvT.r)
#shapiro.test(lme_AvT.r$residuals[,2])




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# data transformation for normality: 1).square-root transform; 2). logarithm; 3). reciprocal tranformation: 1/values
#C$PO4 <- sqrt(C$PO4)# square-root transform
#lme_C <- lme(PO4~Temp_treatment*date, random = ~1|core_no,data = C)
#shapiro.test(lme_C$residuals[,2])
#summary(lme_C) 


#B <- df.PO4[which(df.PO4$P_treatment=="B"),]
#lme_B <- lme(PO4~Temp_treatment*date, random = ~1|core_no,data = B)
#shapiro.test(lme_B$residuals[,2])
#summary(lme_B)

#C_B <- df.PO4[which(df.PO4$P_treatment=="C"|df.PO4$P_treatment=="B"),]
#lme_C_B <- lme(PO4~P_treatment*Temp_treatment*date, random = ~1|core_no,data = C_B)
#shapiro.test(lme_C_B$residuals[,2])
#C_B$PO4 <- sqrt(C_B$PO4)
#lme_C_B <- lme(PO4~P_treatment*Temp_treatment*date, random = ~1|core_no,data = C_B)
#shapiro.test(lme_C_B$residuals[,2])
#summary(lme_C_B)
#anova(lme_C_B)

#LMB <- df.PO4[which(df.PO4$P_treatment=="LMB"),]
#lme_LMB <- lme(PO4~Temp_treatment*date, random = ~1|core_no,data = LMB)
#shapiro.test(lme_LMB$residuals[,2])
#plot(lme_LMB$residuals[,2])
#LMB <- LMB[which(LMB$date>=7),]
#lme_LMB <- lme(PO4~Temp_treatment*date, random = ~1|core_no,data = LMB)
#shapiro.test(lme_LMB$residuals[,2])
#summary(lme_LMB)
#anova(lme_LMB)
#lme_LMB$coefficients




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

##### Maggie blue algae visualization #####

#### Averaging values over significant factors (here: time and T_treatments) ####
df.bc.sum <- as.data.frame(df.bc.sum)

Bluea.sum <- na.omit(df.bc.sum)%>%group_by(Temp_level,ExperimentDay)%>%summarise(
  Bluea.mean = mean(`Blue Concentration`),
  Bluea.std.er=sd(`Blue Concentration`)/sqrt(length(`Blue Concentration`))*1.96
)%>%as.data.frame()


treat<-(unique(df.bc.sum$Temp_level))
#col <- 1:length(treat)
col <- c("deepskyblue", "orangered1")
pch <- 1:length(treat)
lty <- c(1,1)
lwd <- c(3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(Bluea.sum$ExperimentDay,Bluea.sum$Bluea.mean,ylim = c(min(Bluea.sum$Bluea.mean-Bluea.sum$Bluea.std.er),max(Bluea.sum$Bluea.mean+Bluea.sum$Bluea.std.er)),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Blue algae concentration (ug  ",L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.bc.sum$ExperimentDay%>%unique(),labels = df.bc.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,2000,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=Bluea.sum[which(Bluea.sum$Temp_level==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$Bluea.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$Bluea.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$Bluea.mean-box.dat$Bluea.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$Bluea.mean+box.dat$Bluea.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("18C","24C"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
       text.col =col,cex = 1,lwd = c(3,3,3,3),bty = "n")

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# BACI LABEL
#mtext(text="Before",at=mean(c(1,8)),side = 3,line = 0,cex = 2)
#mtext(text="Heatwave",at=mean(c(8,15)),side = 3,line = 0,cex = 2)
#mtext(text="After",at=mean(c(15,21)),side = 3,line = 0,cex = 2)

#from pervious scripts:
#mtext(text="Before Runoff",at=mean(c(ymd("2018-02-28"),ymd("2018-03-12"))),side = 3,line = 0,cex = 1.25)
#mtext(text="After Runoff",at=mean(c(ymd("2018-03-14"),ymd("2018-03-23"))),side = 3,line = 0,cex = 1.25)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# LEGEND
#legend("topleft", legend = c("No phoslock","Phoslock, first phase 20째C","Phoslock, 30째C","Phoslock, 20째C"), col = col,lty=lty,pch = pch,text.col =col,cex = 0.5,lwd = lwd,bty="n")

#from previous script
#legend("topleft", legend = c("18C Normal","24C Normal","18C Extreme","24C Extreme"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
#       text.col =col,cex = 1,lwd = c(3,3,3,3)) 


#####

#### Visualization of significant groups ####

#df.bc.sum$treat <- NA
#prior.PO4 <- df.bc.sum[which(df.bc.sum$date <= 7),]
#prior.PO4$treat = ifelse(prior.PO4$P_treatment=="LMB","LMB","C")

#post.PO4 <- df.bc.sum[which(df.bc.sum$date >= 7),]
#post.PO4$treat[which(post.PO4$P_treatment=="LMB"&post.PO4$Temp_treatment=="HW")] <- "LMB HW"
#post.PO4$treat[which(post.PO4$P_treatment=="LMB"&post.PO4$Temp_treatment=="T")] <- "LMB T"
#post.PO4$treat[which(is.na(post.PO4$treat)==T)]<-"C"


#### Averaging values over new groups (named column "treat") ####
#prior.PO4.sum <- na.omit(prior.PO4)%>%group_by(treat,date)%>%summarise(
#  PO4.mean = mean(PO4),
#  range.max=max(PO4),
#  range.min=min(PO4)
#)

#post.PO4.sum <- na.omit(post.PO4)%>%group_by(treat,date)%>%summarise(
#  PO4.mean = mean(PO4),
#  range.max=max(PO4),
#  range.min=min(PO4)
#)

#df.bc.sum.sum <- rbind(prior.PO4.sum, post.PO4.sum)

#treat<-unique(df.bc.sum.sum$treat)
#col <- 1:length(treat)
#pch <- 1:length(treat)
#lty <- 1:length(treat)
#lwd <- 1:length(treat)

#par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1.5)
#par("usr")

#plot(df.bc.sum.sum$date,df.bc.sum.sum$PO4.mean,ylim = c(min(df.bc.sum.sum$range.min),max(df.bc.sum.sum$range.max)),type="n",xlab="",ylab = "",xaxt="n",las=1)
#mtext(expression(paste(PO[4]^{"2-"}," (g ", m^{-3},")",sep =" ")),side = 2,line = 3,cex=2)
#mtext("Time (d)",side = 1,line = 2.5,cex=2)
#axis(side = 1,at=df.bc.sum.sum$date%>%unique(),labels = c("1","3","7","9","11","14","16","21"))

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# RECTANGLE FOR POST-HEATWAVE
#rect(xleft = 7.1,xright = 15,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 1, green = 0.8, blue = 0.8, alpha =  0.5),border = NA)

    #from previous script
#rect(xleft = ymd("2018-03-13"),xright = ymd("2018-03-23"),ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, 
#                                                                                                               blue = 1, alpha = 0.5),
#     border = NA)



##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ERROR BARS
#for (i in 1:length(treat)) {
#  box.dat=df.bc.sum.sum[which(df.bc.sum.sum$treat==treat[i]),]
#  lines(box.dat$date+(-length(treat)/2+i)*0.1,box.dat$PO4.mean,col=col[i],lty=lty[i],lwd=lwd[i])
#  points(box.dat$date+(-length(treat)/2+i)*0.1,box.dat$PO4.mean,col=col[i],pch=pch[i],cex=2)
#  arrows(box.dat$date+(-length(treat)/2+i)*0.1,box.dat$range.min,box.dat$date+(-length(treat)/2+i)*0.1,box.dat$range.max,code=3,angle = 90,length=.05,col=col[i])
#}

    #from previous script
#for (i in 1:length(treat)) {
#  box.dat=BlueConc.dat.sum[which(BlueConc.dat.sum$Treat==treat[i]),]
#  lines(box.dat$Date+(-length(treat)/2+i)*0.1,box.dat$BlueConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
#  points(box.dat$Date+(-length(treat)/2+i)*0.1,box.dat$BlueConc.dat.mean,col=col[i],pch=pch[i],cex=1)
#  arrows(box.dat$Date+(-length(treat)/2+i)*0.1,box.dat$range.min,box.dat$Date+(-length(treat)/2+i)*0.1,
#         box.dat$range.max,code=3,angle = 90,length=.05,col=col[i])
#}


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# BACI LABEL
#mtext(text="Before",at=mean(c(1,8)),side = 3,line = 0,cex = 2)
#mtext(text="Heatwave",at=mean(c(8,15)),side = 3,line = 0,cex = 2)
#mtext(text="After",at=mean(c(15,21)),side = 3,line = 0,cex = 2)

#from pervious scripts:
#mtext(text="Before Runoff",at=mean(c(ymd("2018-02-28"),ymd("2018-03-12"))),side = 3,line = 0,cex = 1.25)
#mtext(text="After Runoff",at=mean(c(ymd("2018-03-14"),ymd("2018-03-23"))),side = 3,line = 0,cex = 1.25)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# LEGEND
#legend("topleft", legend = c("No phoslock","Phoslock, first phase 20째C","Phoslock, 30째C","Phoslock, 20째C"), col = col,lty=lty,pch = pch,text.col =col,cex = 0.5,lwd = lwd,bty="n")

    #from previous script
#legend("bottomleft", legend = c("18C Normal","24C Normal","18C Extreme","24C Extreme"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
#       text.col =col,cex = 1,lwd = c(3,3,3,3)) 
