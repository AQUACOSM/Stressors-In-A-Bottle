###########################################
#### P Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

PConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","P (particulate) (mg-L)","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#PConc.dat$Date <- ymd(PConc.dat$Date)
#Calculating the error bar by range; time series with range
PConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                          levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
PConc.dat.sum <- na.omit(PConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  PConc.dat.mean = mean(`P (particulate) (mg-L)`),
  PConc.std.er=sd(`P (particulate) (mg-L)`)/sqrt(length(`P (particulate) (mg-L)`))*1.96
)%>%as.data.frame()

treat<-unique(PConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(PConc.dat.sum$ExperimentDay,PConc.dat.sum$PConc.dat.mean,ylim = c(min(PConc.dat.sum$PConc.dat.mean-PConc.dat.sum$PConc.std.er),2.5),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8,  blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Seston Phosphorus (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=PConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=PConc.dat.sum[which(PConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$PConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$PConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$PConc.dat.mean-box.dat$PConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$PConc.dat.mean+box.dat$PConc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)

#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topleft", legend = c("18C Normal","24C Normal","18C Extreme","24C Extreme"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
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


dat.p <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","ExperimentDay","P (particulate) (mg-L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.p.sum<-na.omit(dat.p)%>%as.data.frame()
names(df.p.sum)[5] <- "P_Conc"
df.p.sum$MicrocosmNumber <- as.factor(df.p.sum$MicrocosmNumber)
df.p.sum$Temp_level <- factor(df.p.sum$Temp_level, levels = c("AvT", "ExT"))
df.p.sum$Precip_level <- factor(df.p.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.p.sum$P_Conc)

require(nlme)
require(car)

lme_modelp <- lme(P_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.p.sum)

# normality test on the residuals
plot(lme_modelp$residuals[,2])
hist(lme_modelp$residuals[,2])
shapiro.test(lme_modelp$residuals[,2])
#p-value = 3.614e-05


# Initial knowlege by the pilot lme model
anova(lme_modelp)
#precip, temp and time significant
summary(lme_modelp)
#time, precip, temp*precip significant



##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modelp <- lme(sqrt(P_Conc)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.p.sum)

shapiro.test(lme_modelp$residuals[,2])
#sqrt: p-value = 0.0001731
#log: p-value = 1.654e-05
#reciprocal: p-value = 9.071e-10
plot(lme_modelp$residuals[,2])
hist(lme_modelp$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(P_Conc~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.p.sum)
plot(df.p.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.p.sum$ExperimentDay)
#all 3: p-value = 8.015e-11
#w/o precip: p-value = 3.239e-11
#w/o temp: p-value = 4.61e-10
#w/o precip and temp: 2.249e-10


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.p <- df.p.sum[which(df.p.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.p <- lme(sqrt(P_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.p)

shapiro.test(lme_Before.p$residuals[,2])
#normal: p-value = 0.005547
#sqrt: p-value = 0.1012
#log: p-value = 0.003191
#recip: p-value = 1.899e-07
plot(lme_Before.p$residuals[,2])
anova(lme_Before.p)
#temp, time, temp*time significant
summary(lme_Before.p)
#time, time*temp significant


#AFTER RUNOFF EVENT
After.p <- df.p.sum[which(df.p.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.p <- lme(sqrt(P_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.p)

shapiro.test(lme_After.p$residuals[,2])
#normal: p-value = 0.1778
#sqrt: p-value = 0.6485
#log: p-value = 0.002533
#recip: p-value = 1.037e-12
plot(lme_After.p$residuals[,2])
anova(lme_After.p)
#precip significant (barely)
summary(lme_After.p)
#time significant 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Before = time, temp, time*temp  After = time) 

Before.p <- df.p.sum[which(df.p.sum$ExperimentDay<=12),]%>%as.data.frame()
After.p <- df.p.sum[which(df.p.sum$ExperimentDay>=12),]%>%as.data.frame()


p.Before.sum <- na.omit(Before.p)%>%group_by(Temp_level,ExperimentDay)%>%summarise(
  p.mean = mean(P_Conc),
  p.std.er=sd(P_Conc)/sqrt(length(P_Conc))*1.96
)%>%as.data.frame()
p.Before.sum$Treatments <- p.Before.sum$Temp_level
p.Before.sum <- p.Before.sum[,-1]

#p.After.sum <- na.omit(After.p)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  p.mean = mean(P_Conc),
#  p.std.er=sd(P_Conc)/sqrt(length(P_Conc))*1.96
#)%>%as.data.frame()
#p.After.sum$Treatments <- p.After.sum$Precip_level
#p.After.sum <- p.After.sum[,-1]

p.After.sum <- na.omit(After.p)%>%group_by(ExperimentDay)%>%summarise(
  p.mean = mean(P_Conc),
  p.std.er=sd(P_Conc)/sqrt(length(P_Conc))*1.96
)%>%as.data.frame()
p.After.sum$Treatments <- "No_T_effect"
#p.After.sum <- p.After.sum[,-1]

p.sum.plot <- rbind(p.Before.sum, p.After.sum)
p.sum.plot$Treatments <- factor(p.sum.plot$Treatments, levels=c("AvT", "ExT", "No_T_effect"))



#### GRAPH ####
treat<-(unique(p.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("deepskyblue", "orangered1", "black")
pch <- 1:length(treat)
lty <- c(1,1,1)
lwd <- c(3,3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(p.sum.plot$ExperimentDay,p.sum.plot$p.mean,ylim = c(min(p.sum.plot$p.mean-p.sum.plot$p.std.er),max(150)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Phosphorus concentration (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.p.sum$ExperimentDay%>%unique(),labels = df.p.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,150,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=p.sum.plot[which(p.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$p.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$p.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$p.mean-box.dat$p.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$p.mean+box.dat$p.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("18C","24C","All Treatments"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 



















