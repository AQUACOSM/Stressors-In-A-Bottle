###########################################
#### PO4 Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

P4Conc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","PO4-P (corrected, dissolved) (mg-L)","ExperimentDay")]
P4Conc.dat$`PO4-P (corrected, dissolved) (mg-L)` <- as.numeric(P4Conc.dat$`PO4-P (corrected, dissolved) (mg-L)`)
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#P4Conc.dat$Date <- ymd(P4Conc.dat$Date)
#Calculating the error bar by range; time series with range
P4Conc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                          levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
P4Conc.dat <- na.omit(P4Conc.dat)
P4Conc.dat.sum <- na.omit(P4Conc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  P4Conc.dat.mean = mean(`PO4-P (corrected, dissolved) (mg-L)`,na.rm=T),
  P4Conc.std.er=sd(`PO4-P (corrected, dissolved) (mg-L)`)/sqrt(length(`PO4-P (corrected, dissolved) (mg-L)`))*1.96
)%>%as.data.frame()


treat<-unique(P4Conc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(P4Conc.dat.sum$ExperimentDay,P4Conc.dat.sum$P4Conc.dat.mean,ylim = c(min(0),2.5),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Dissolved Phosphate (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=P4Conc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=P4Conc.dat.sum[which(P4Conc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$P4Conc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$P4Conc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$P4Conc.dat.mean-box.dat$P4Conc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$P4Conc.dat.mean+box.dat$P4Conc.std.er),code=3,angle = 90,length=.05,col=col[i])
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

rex_March2020$`PO4-P (corrected, dissolved) (mg-L)`
dat.p4 <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","ExperimentDay","PO4-P (corrected, dissolved) (mg-L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1
dat.p4$`PO4-P (corrected, dissolved) (mg-L)`<- as.numeric(dat.p4$`PO4-P (corrected, dissolved) (mg-L)`)

df.p4.sum<-na.omit(dat.p4)%>%as.data.frame()
names(df.p4.sum)[5] <- "P4_Conc"
df.p4.sum$P4_Conc<- as.numeric(df.p4.sum$P4_Conc)
df.p4.sum$MicrocosmNumber <- as.factor(df.p4.sum$MicrocosmNumber)
df.p4.sum$Temp_level <- factor(df.p4.sum$Temp_level, levels = c("AvT", "ExT"))
df.p4.sum$Precip_level <- factor(df.p4.sum$Precip_level, levels = c("AvP", "ExP"))


plot(df.p4.sum$P4_Conc)

require(nlme)
require(car)

lme_modelp4 <- lme(P4_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.p4.sum)

# normality test on the residuals
plot(lme_modelp4$residuals[,2])
hist(lme_modelp4$residuals[,2])
shapiro.test(lme_modelp4$residuals[,2])
#p-value = 8.785e-16


# Initial knowlege by the pilot lme model
anova(lme_modelp4)
#precip, time, precip*time significant
summary(lme_modelp4)
#precip, time, precip*time significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modelp4 <- lme(1/(P4_Conc+0.1)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.p4.sum)

shapiro.test(lme_modelp4$residuals[,2])
#sqrt: p-value = 4.607e-06
#log: p-value = 0.0001183
#reciprocal: p-value = 0.00121
plot(lme_modelp4$residuals[,2])
hist(lme_modelp4$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(P4_Conc~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.p4.sum)
plot(df.p4.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.p4.sum$ExperimentDay)
#all 3: p-value = 4.794e-06
#w/o precip: p-value = 
#w/o temp: p-value = 6.497e-06
#w/o precip and temp: 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.p4 <- df.p4.sum[which(df.p4.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.p4 <- lme(P4_Conc~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.p4)

shapiro.test(lme_Before.p4$residuals[,2])
#normal: p-value = 4.017e-10
#sqrt: p-value = 0.04054
#log: p-value =0.00935
#recip: p-value = 9.38e-12
plot(lme_Before.p4$residuals[,2])
anova(lme_Before.p4)
#time significant
summary(lme_Before.p4)
#time significant


#AFTER RUNOFF EVENT
After.p4 <- df.p4.sum[which(df.p4.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.p4 <- lme(P4_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.p4)

shapiro.test(lme_After.p4$residuals[,2])
#normal: p-value = < 2.2e-16
#sqrt: p-value = 3.73e-12
#log: p-value = 3.73e-12
#recip: p-value = 3.915e-13
plot(lme_After.p4$residuals[,2])
anova(lme_After.p4)
#precip significant (barely)
summary(lme_After.p4)
#time significant 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Before = time  After = ???) 

Before.p4 <- df.p4.sum[which(df.p4.sum$ExperimentDay<=12),]%>%as.data.frame()
After.p4 <- df.p4.sum[which(df.p4.sum$ExperimentDay>=12),]%>%as.data.frame()


p4.Before.sum <- na.omit(Before.p4)%>%group_by(ExperimentDay)%>%summarise(
  p4.mean = mean(P4_Conc),
  p4.std.er=sd(P4_Conc)/sqrt(length(P4_Conc))*1.96
)%>%as.data.frame()
p4.Before.sum$Treatments <- "No_T_effect"
#p4.Before.sum <- p4.Before.sum[,-1]

#p4.After.sum <- na.omit(After.p4)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  p4.mean = mean(P4_Conc),
#  p4.std.er=sd(P4_Conc)/sqrt(length(P4_Conc))*1.96
#)%>%as.data.frame()
#p4.After.sum$Treatments <- p4.After.sum$Precip_level
#p4.After.sum <- p4.After.sum[,-1]

p4.After.sum <- na.omit(After.p4)%>%group_by(ExperimentDay)%>%summarise(
  p4.mean = mean(P4_Conc),
  p4.std.er=sd(P4_Conc)/sqrt(length(P4_Conc))*1.96
)%>%as.data.frame()
p4.After.sum$Treatments <- "No_T_effect"
#p4.After.sum <- p4.After.sum[,-1]

p4.sum.plot <- rbind(p4.Before.sum, p4.After.sum)
p4.sum.plot$Treatments <- factor(p4.sum.plot$Treatments, levels=c("No_T_effect"))



#### GRAPH ####
treat<-(unique(p4.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("black")
pch <- 1:length(treat)
lty <- c(1)
lwd <- c(3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(p4.sum.plot$ExperimentDay,p4.sum.plot$p4.mean,ylim = c(min(0),max(1.5)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Phosphate concentration (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.p4.sum$ExperimentDay%>%unique(),labels = df.p4.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,1.5,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=p4.sum.plot[which(p4.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$p4.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$p4.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$p4.mean-box.dat$p4.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$p4.mean+box.dat$p4.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("All Treatments"), col = col,lty=lty,pch = pch,
        text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 



















