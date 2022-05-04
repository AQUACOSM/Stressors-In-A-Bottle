###########################################
#### C Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

CConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","C (mg-L)","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#CConc.dat$Date <- ymd(CConc.dat$Date)
#Calculating the error bar by range; time series with range
CConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                          levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
CConc.dat.sum <- na.omit(CConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  CConc.dat.mean = mean(`C (mg-L)`),
  CConc.std.er=sd(`C (mg-L)`)/sqrt(length(`C (mg-L)`))*1.96
)%>%as.data.frame()

treat<-unique(CConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(CConc.dat.sum$ExperimentDay,CConc.dat.sum$CConc.dat.mean,ylim = c(min(CConc.dat.sum$CConc.dat.mean-CConc.dat.sum$CConc.std.er),max(CConc.dat.sum$CConc.dat.mean+CConc.dat.sum$CConc.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("C (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=CConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=CConc.dat.sum[which(CConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$CConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$CConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$CConc.dat.mean-box.dat$CConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$CConc.dat.mean+box.dat$CConc.std.er),code=3,angle = 90,length=.05,col=col[i])
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
##run-through with C (mg/L)
install.packages("nlme")


dat.c <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","C (mg-L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.c.sum<-na.omit(dat.c)%>%as.data.frame()
names(df.c.sum)[6] <- "C_Conc"
df.c.sum$MicrocosmNumber <- as.factor(df.c.sum$MicrocosmNumber)
df.c.sum$Temp_level <- factor(df.c.sum$Temp_level, levels = c("AvT", "ExT"))
df.c.sum$Precip_level <- factor(df.c.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.c.sum$C_Conc)

require(nlme)
require(car)

lme_modelc <- lme(C_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.c.sum)

# normality test on the residuals
plot(lme_modelc$residuals[,2])
hist(lme_modelc$residuals[,2])
shapiro.test(lme_modelc$residuals[,2])
#p-value = 1.659e-11


# Initial knowlege by the pilot lme model
anova(lme_modelc)
#precip, temp and time, time interactions significant
summary(lme_modelc)
#time, temp*time significant



##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modelc <- lme(1/(C_Conc)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.c.sum)

shapiro.test(lme_modelc$residuals[,2])
#sqrt: p-value = 1.778e-06
#log: p-value = 6.116e-07
#reciprocal: p-value = 5.35e-05
plot(lme_modelc$residuals[,2])
hist(lme_modelc$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(C_Conc~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.c.sum)
plot(df.c.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.c.sum$ExperimentDay)
#all 3: p-value = 1.044e-10
#w/o precip: p-value = 2.446e-10
#w/o temp: p-value = 6.276e-10
#w/o precip and temp: 5.324e-10


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.c <- df.c.sum[which(df.c.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.c <- lme(log(C_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.c)

shapiro.test(lme_Before.c$residuals[,2])
#normal: p-value = 2.435e-08
#sqrt: p-value = 0.001607
#log: p-value = 0.4157
#recip: p-value = 0.01768
plot(lme_Before.c$residuals[,2])
anova(lme_Before.c)
#temp, time, temp*time significant
summary(lme_Before.c)
#time, temp*time significant

# LME for separate periods
#var <- aggregate(Before.c$C_Conc, by=paste(Before.c$ExperimentDay, Before.c$Temp_level)%>%as.character%>%list, FUN=var)
#assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
#Before.c$wn <- sapply(paste(Before.c$ExperimentDay, Before.c$Temp_level), assign_var)
#lme_Before.c <- lme(sqrt(C_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, weights = varFunc(~wn), data = Before.c)

#shapiro.test(lme_Before.c$residuals[,2])
#plot(lme_Before.c$residuals[,2])
#anova(lme_Before.c)



#AFTER RUNOFF EVENT
After.c <- df.c.sum[which(df.c.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.c <- lme(sqrt(C_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.c)

shapiro.test(lme_After.c$residuals[,2])
#normal: p-value = 0.0008873
#sqrt: p-value = 0.3986
#log: p-value = 0.000767
#recip: p-value = 5.063e-15
plot(lme_After.c$residuals[,2])
anova(lme_After.c)
#no significant factors
summary(lme_After.c)
#no significant factors


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Before = time, temp, time*temp  After = ???) 

Before.c <- df.c.sum[which(df.c.sum$ExperimentDay<=12),]%>%as.data.frame()
After.c <- df.c.sum[which(df.c.sum$ExperimentDay>=12),]%>%as.data.frame()


c.Before.sum <- na.omit(Before.c)%>%group_by(Temp_level,ExperimentDay)%>%summarise(
  c.mean = mean(C_Conc),
  c.std.er=sd(C_Conc)/sqrt(length(C_Conc))*1.96
)%>%as.data.frame()
c.Before.sum$Treatments <- c.Before.sum$Temp_level
c.Before.sum <- c.Before.sum[,-1]

#c.After.sum <- na.omit(After.c)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  c.mean = mean(C_Conc),
#  c.std.er=sd(C_Conc)/sqrt(length(C_Conc))*1.96
#)%>%as.data.frame()
#c.After.sum$Treatments <- c.After.sum$Precip_level
#c.After.sum <- c.After.sum[,-1]

c.After.sum <- na.omit(After.c)%>%group_by(ExperimentDay)%>%summarise(
  c.mean = mean(C_Conc),
  c.std.er=sd(C_Conc)/sqrt(length(C_Conc))*1.96
)%>%as.data.frame()
c.After.sum$Treatments <- "No_T_effect"
#c.After.sum <- c.After.sum[,-1]

c.sum.plot <- rbind(c.Before.sum, c.After.sum)
c.sum.plot$Treatments <- factor(c.sum.plot$Treatments, levels=c("AvT", "ExT", "No_T_effect"))



#### GRAPH ####
treat<-(unique(c.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("deepskyblue", "orangered1", "black")
pch <- 1:length(treat)
lty <- c(1,1,1)
lwd <- c(3,3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(c.sum.plot$ExperimentDay,c.sum.plot$c.mean,ylim = c(min(c.sum.plot$c.mean-c.sum.plot$c.std.er),max(c.sum.plot$c.mean+c.sum.plot$c.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Carbon concentration (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.c.sum$ExperimentDay%>%unique(),labels = df.c.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,190,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=c.sum.plot[which(c.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$c.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$c.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$c.mean-box.dat$c.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$c.mean+box.dat$c.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("18C","24C","All Treatments"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 




















