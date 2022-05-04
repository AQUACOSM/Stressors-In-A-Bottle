###########################################
#### NO3 Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

NO3Conc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","NO3 (mg-L)","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#NO3Conc.dat$Date <- ymd(NO3Conc.dat$Date)
#Calculating the error bar by range; time series with range
NO3Conc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                          levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
NO3Conc.dat.sum <- na.omit(NO3Conc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  NO3Conc.dat.mean = mean(`NO3 (mg-L)`),
  NO3Conc.std.er=sd(`NO3 (mg-L)`)/sqrt(length(`NO3 (mg-L)`))*1.96
)%>%as.data.frame()

treat<-unique(NO3Conc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(NO3Conc.dat.sum$ExperimentDay,NO3Conc.dat.sum$NO3Conc.dat.mean,ylim = c(min(NO3Conc.dat.sum$NO3Conc.dat.mean-NO3Conc.dat.sum$NO3Conc.std.er),max(NO3Conc.dat.sum$NO3Conc.dat.mean+NO3Conc.dat.sum$NO3Conc.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("NO3 (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=NO3Conc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=NO3Conc.dat.sum[which(NO3Conc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$NO3Conc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$NO3Conc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$NO3Conc.dat.mean-box.dat$NO3Conc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$NO3Conc.dat.mean+box.dat$NO3Conc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)

#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topright", legend = c("18C Normal","24C Normal","18C Extreme","24C Extreme"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
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


dat.n3 <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","NO3 (mg-L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.n3.sum<-na.omit(dat.n3)%>%as.data.frame()
names(df.n3.sum)[6] <- "N3_Conc"
df.n3.sum$MicrocosmNumber <- as.factor(df.n3.sum$MicrocosmNumber)
df.n3.sum$Temp_level <- factor(df.n3.sum$Temp_level, levels = c("AvT", "ExT"))
df.n3.sum$Precip_level <- factor(df.n3.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.n3.sum$N3_Conc)

require(nlme)
require(car)

lme_modeln3 <- lme(N3_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.n3.sum)

# normality test on the residuals
plot(lme_modeln3$residuals[,2])
hist(lme_modeln3$residuals[,2])
shapiro.test(lme_modeln3$residuals[,2])
#p-value = 7.49e-16


# Initial knowlege by the pilot lme model
anova(lme_modeln3)
summary(lme_modeln3)
#precip, temp and time are all p significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modeln3 <- lme(log(N3_Conc)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.n3.sum)

shapiro.test(lme_modeln3$residuals[,2])
#sqrt: p-value = 1.774e-07
#log: p-value = 0.2126
#reciprocal: p-value = 4.006e-08
plot(lme_modeln3$residuals[,2])
hist(lme_modeln3$residuals[,2])
anova(lme_modeln3)
#precip, temp, time, precip*time significant
summary(lme_modeln3)
#precip, time, precip*time significant

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
#lme_model <- lme(N3_Conc~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.n3.sum)
#plot(df.n3.sum$ExperimentDay,lme_model$residuals[,2])
#bptest(lme_model$residuals[,2]~df.n3.sum$ExperimentDay)
#all 3: p-value = 2.564e-11
#w/o precip: p-value = 9.586e-11
#w/o temp: p-value = 2.615e-10
#w/o precip and temp: 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.n3 <- df.n3.sum[which(df.n3.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.n3 <- lme(log(N3_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.n3)

shapiro.test(lme_Before.n3$residuals[,2])
#normal: p-value = 
#sqrt: p-value = 
#log: p-value = 0.3033
#recip: p-value = 
plot(lme_Before.n3$residuals[,2])
anova(lme_Before.n3)
#experiment day significant
summary(lme_Before.n3)


#AFTER RUNOFF EVENT
After.n3 <- df.n3.sum[which(df.n3.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.n3 <- lme(log(N3_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.n3)

shapiro.test(lme_After.n3$residuals[,2])
#normal: p-value = 
#sqrt: p-value = 
#log: p-value = 0.09481
#recip: p-value = 
plot(lme_After.n3$residuals[,2])
anova(lme_After.n3)
#time, time*precip significant
summary(lme_After.n3)
#time, time*precip significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Before = time After = time, time*precip) 

Before.n3 <- df.n3.sum[which(df.n3.sum$ExperimentDay<=14),]%>%as.data.frame()
After.n3 <- df.n3.sum[which(df.n3.sum$ExperimentDay>=14),]%>%as.data.frame()

#After.n3.ExP <- After.n3[which(After.n3$Temp_level=="ExT"),]%>%as.data.frame()
#After.n3.AvP <- After.n3[which(After.n3$Temp_level=="AvT"),]%>%as.data.frame()

#lme_After.n3.AvT <- lme(sqrt(N3_Conc)~ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.n3.AvT)

#shapiro.test(lme_After.n3.AvT$residuals[,2])
#normal: p-value = p-value = 0.001702
#sqrt: p-value = 0.6634
#log: p-value = 0.000767
#recip: p-value = 5.063e-15
#plot(lme_After.n3.AvT$residuals[,2])
#anova(lme_After.n3.AvT)
#no significant factors
#summary(lme_After.n3.AvT)

n3.Before.sum <- na.omit(Before.n3)%>%group_by(ExperimentDay)%>%summarise(
  n3.mean = mean(N3_Conc),
  n3.std.er=sd(N3_Conc)/sqrt(length(N3_Conc))*1.96
)%>%as.data.frame()
n3.Before.sum$Treatments <- "No_P_effect"
#n3.Before.sum <- n3.Before.sum[,-1]

#n.After.sum <- na.omit(After.n)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  n.mean = mean(N_Conc),
#  n.std.er=sd(N_Conc)/sqrt(length(N_Conc))*1.96
#)%>%as.data.frame()
#n.After.sum$Treatments <- n.After.sum$Precip_level
#n.After.sum <- n.After.sum[,-1]

n3.After.sum <- na.omit(After.n3)%>%group_by(Precip_level, ExperimentDay)%>%summarise(
  n3.mean = mean(N3_Conc),
  n3.std.er=sd(N3_Conc)/sqrt(length(N3_Conc))*1.96
)%>%as.data.frame()
n3.After.sum$Treatments <- n3.After.sum$Precip_level
n3.After.sum <- n3.After.sum[,-1]

n3.sum.plot <- rbind(n3.Before.sum, n3.After.sum)
n3.sum.plot$Treatments <- factor(n3.sum.plot$Treatments, levels=c("No_P_effect","AvP", "ExP"))



#### GRAPH ####
treat<-(unique(n3.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c( "black", "aquamarine3", "brown3")
pch <- 1:length(treat)
lty <- c(1,1,1)
lwd <- c(3,3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(n3.sum.plot$ExperimentDay,n3.sum.plot$n3.mean,ylim = c(min(n3.sum.plot$n3.mean-n3.sum.plot$n3.std.er),max(60)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Nitrate concentration (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.n3.sum$ExperimentDay%>%unique(),labels = df.n3.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,60,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=n3.sum.plot[which(n3.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$n3.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$n3.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$n3.mean-box.dat$n3.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$n3.mean+box.dat$n3.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("bottomleft", legend = c("All Treatments","No Runoff Event","Runoff Event"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




###--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 





















