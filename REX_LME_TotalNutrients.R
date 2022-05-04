#########
### REX Total Nutrients (measured)
#########


### Phosphorus 

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

TotPConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","TotP (mg/L)","ExperimentDay")]
#dat.totpO4 <- dat.totpO4[-which(dat.totpO4$core_no%in%c(37:39)),]
#TotPConc.dat$Date <- ymd(TotPConc.dat$Date)
#Calculating the error bar by range; time series with range
TotPConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                          levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
TotPConc.dat.sum <- na.omit(TotPConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  TotPConc.dat.mean = mean(`TotP (mg/L)`),
  TotPConc.std.er=sd(`TotP (mg/L)`)/sqrt(length(`TotP (mg/L)`))*1.96
)%>%as.data.frame()

treat<-unique(TotPConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(TotPConc.dat.sum$ExperimentDay,TotPConc.dat.sum$TotPConc.dat.mean,ylim = c(min(TotPConc.dat.sum$TotPConc.dat.mean-TotPConc.dat.sum$TotPConc.std.er),2.5),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8,  blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Total Phosphorus (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=TotPConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=TotPConc.dat.sum[which(TotPConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$TotPConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$TotPConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$TotPConc.dat.mean-box.dat$TotPConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$TotPConc.dat.mean+box.dat$TotPConc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)

#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topright", legend = c("18°C & No Runoff","24°C & No Runoff","18°C & Runoff","24°C & Runoff"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
       text.col =col,cex = 1,lwd = c(3,3,3,3)) 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
###########################################
#### REX adaptation of Qing LME script ####
###########################################

#### LME RAW DATA ####
##run-through with N (mg/L)
install.packages("nlme")


dat.totp <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","ExperimentDay","TotP (mg/L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.totp.sum<-na.omit(dat.totp)%>%as.data.frame()
names(df.totp.sum)[5] <- "TotP_Conc"
df.totp.sum$MicrocosmNumber <- as.factor(df.totp.sum$MicrocosmNumber)
df.totp.sum$Temp_level <- factor(df.totp.sum$Temp_level, levels = c("AvT", "ExT"))
df.totp.sum$Precip_level <- factor(df.totp.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.totp.sum$TotP_Conc)

require(nlme)
require(car)

lme_modeltotp <- lme(TotP_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.totp.sum)

# normality test on the residuals
plot(lme_modeltotp$residuals[,2])
hist(lme_modeltotp$residuals[,2])
shapiro.test(lme_modeltotp$residuals[,2])
#p-value = 0.02177


# Initial knowlege by the pilot lme model
anova(lme_modeltotp)
#precip, temp significant
summary(lme_modeltotp)
#no significant



##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modeltotp <- lme(sqrt(TotP_Conc)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.totp.sum)

shapiro.test(lme_modeltotp$residuals[,2])
#none: p-value = 0.02177
#sqrt: p-value = 0.2104
#log: p-value = 2.341e-07
#reciprocal: p-value = < 2.2e-16
plot(lme_modeltotp$residuals[,2])
hist(lme_modeltotp$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(TotP_Conc~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.totp.sum)
plot(df.totp.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.totp.sum$ExperimentDay)
#all 3: p-value = 8.015e-11
#w/o precip: p-value = 3.239e-11
#w/o temp: p-value = 4.61e-10
#w/o precip and temp: 2.249e-10


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.totp <- df.totp.sum[which(df.totp.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.totp <- lme(log(TotP_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.totp)

shapiro.test(lme_Before.totp$residuals[,2])
#normal: p-value = 8.872e-07
#sqrt: p-value = 0.008293
#log: p-value = 0.1952
#recip: p-value = 5.049e-10
plot(lme_Before.totp$residuals[,2])
anova(lme_Before.totp)
#temp, time, temp*time significant
summary(lme_Before.totp)
#time, time*temp significant



#AFTER RUNOFF EVENT
After.totp <- df.totp.sum[which(df.totp.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.totp <- lme(sqrt(TotP_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.totp)

shapiro.test(lme_After.totp$residuals[,2])
#normal: p-value = 0.4091
#sqrt: p-value = 0.5426
#log: p-value = 0.0003946
#recip: p-value = 3.043e-13
plot(lme_After.totp$residuals[,2])
anova(lme_After.totp)
#no significant 
summary(lme_After.totp)
#time significant 













### Nitrogen

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

TotNConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","TotN (mg/L)","ExperimentDay")]
#TotNConc.dat$Date <- ymd(TotNConc.dat$Date)
#Calculating the error bar by range; time series with range
TotNConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                              levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
TotNConc.dat.sum <- na.omit(TotNConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  TotNConc.dat.mean = mean(`TotN (mg/L)`),
  TotNConc.std.er=sd(`TotN (mg/L)`)/sqrt(length(`TotN (mg/L)`))*1.96
)%>%as.data.frame()

treat<-unique(TotNConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(TotNConc.dat.sum$ExperimentDay,TotNConc.dat.sum$TotNConc.dat.mean,ylim = c(min(TotNConc.dat.sum$TotNConc.dat.mean-TotNConc.dat.sum$TotNConc.std.er),max(TotNConc.dat.sum$TotNConc.dat.mean+TotNConc.dat.sum$TotNConc.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Total Nitrogen (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=TotNConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=TotNConc.dat.sum[which(TotNConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$TotNConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$TotNConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$TotNConc.dat.mean-box.dat$TotNConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$TotNConc.dat.mean+box.dat$TotNConc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)

#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topright", legend = c("18°C & No Runoff","24°C & No Runoff","18°C & Runoff","24°C & Runoff"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
       text.col =col,cex = 1,lwd = c(3,3,3,3)) 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
###########################################
#### REX adaptation of Qing LME script ####
###########################################

#### LME RAW DATA ####
##run-through with NO3 (mg/L)
install.packages("nlme")


dat.TotN <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","TotN (mg/L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.TotN.sum<-na.omit(dat.TotN)%>%as.data.frame()
names(df.TotN.sum)[6] <- "TotN_Conc"
df.TotN.sum$MicrocosmNumber <- as.factor(df.TotN.sum$MicrocosmNumber)
df.TotN.sum$Temp_level <- factor(df.TotN.sum$Temp_level, levels = c("AvT", "ExT"))
df.TotN.sum$Precip_level <- factor(df.TotN.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.TotN.sum$DissN_Conc)

require(nlme)
require(car)

lme_modelTotN <- lme(TotN_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.TotN.sum)

# normality test on the residuals
plot(lme_modelTotN$residuals[,2])
hist(lme_modelTotN$residuals[,2])
shapiro.test(lme_modelTotN$residuals[,2])
#p-value = 5.288e-10

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.TotN <- df.TotN.sum[which(df.TotN.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.TotN <- lme(log(TotN_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.TotN)

shapiro.test(lme_Before.TotN$residuals[,2])
#normal: p-value = 1.095e-10
#sqrt: p-value = 8.021e-06
#log: p-value = 0.06885
#recip: p-value = 5.721e-05
plot(lme_Before.TotN$residuals[,2])
anova(lme_Before.TotN)
#temp significant
summary(lme_Before.TotN)


#AFTER RUNOFF EVENT
After.TotN <- df.TotN.sum[which(df.TotN.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.TotN <- lme(sqrt(TotN_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.TotN)

shapiro.test(lme_After.TotN$residuals[,2])
#normal: p-value = 0.0002454
#sqrt: p-value = 0.8125
#log: p-value = 0.09964
#recip: p-value = 6.468e-11
plot(lme_After.TotN$residuals[,2])
anova(lme_After.TotN)
#time, temp significant
summary(lme_After.TotN)
#time, temp significant












