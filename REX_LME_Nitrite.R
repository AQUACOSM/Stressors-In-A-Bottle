###########################################
#### NO2 Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

NO2Conc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","NO2 (mg-L)","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#NO2Conc.dat$Date <- ymd(NO2Conc.dat$Date)
#Calculating the error bar by range; time series with range
NO2Conc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                          levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
NO2Conc.dat.sum <- na.omit(NO2Conc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  NO2Conc.dat.mean = mean(`NO2 (mg-L)`),
  NO2Conc.std.er=sd(`NO2 (mg-L)`)/sqrt(length(`NO2 (mg-L)`))*1.96
)%>%as.data.frame()

treat<-unique(NO2Conc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(NO2Conc.dat.sum$ExperimentDay,NO2Conc.dat.sum$NO2Conc.dat.mean,ylim = c(min(NO2Conc.dat.sum$NO2Conc.dat.mean-NO2Conc.dat.sum$NO2Conc.std.er),max(NO2Conc.dat.sum$NO2Conc.dat.mean+NO2Conc.dat.sum$NO2Conc.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("NO2 (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=NO2Conc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=NO2Conc.dat.sum[which(NO2Conc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$NO2Conc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$NO2Conc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$NO2Conc.dat.mean-box.dat$NO2Conc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$NO2Conc.dat.mean+box.dat$NO2Conc.std.er),code=3,angle = 90,length=.05,col=col[i])
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
##run-through with NO2 (mg/L)
install.packages("nlme")


dat.n2 <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","NO2 (mg-L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.n2.sum<-na.omit(dat.n2)%>%as.data.frame()
names(df.n2.sum)[6] <- "N2_Conc"
df.n2.sum$MicrocosmNumber <- as.factor(df.n2.sum$MicrocosmNumber)
df.n2.sum$Temp_level <- factor(df.n2.sum$Temp_level, levels = c("AvT", "ExT"))
df.n2.sum$Precip_level <- factor(df.n2.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.n2.sum$N2_Conc)

require(nlme)
require(car)

lme_modeln2 <- lme(N2_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.n2.sum)

# normality test on the residuals
plot(lme_modeln2$residuals[,2])
hist(lme_modeln2$residuals[,2])
shapiro.test(lme_modeln2$residuals[,2])
#p-value = 4.777e-10


# Initial knowlege by the pilot lme model
anova(lme_modeln2)
#precip, time, precip*time, temp*time significant
summary(lme_modeln2)
#precip, time, precip*time, temp*time significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modeln2<- lme(log(N2_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.n2.sum)

shapiro.test(lme_modeln2$residuals[,2])
#sqrt: p-value = 0.001985
#log: p-value = 0.0398
#reciprocal: p-value = p-value < 2.2e-16
plot(lme_modeln2$residuals[,2])
hist(lme_modeln2$residuals[,2])
anova(lme_modeln2)
#precip, time, precip*time, temp*time significant
summary(lme_modeln2)
#precip, time, temp, precip*temp, precip*time, temp*time significant

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_modeln2 <- lme(N2_Conc~ExperimentDay, random = ~1|MicrocosmNumber, data = df.n2.sum)
plot(df.n2.sum$ExperimentDay,lme_modeln2$residuals[,2])
bptest(lme_modeln2$residuals[,2]~df.n2.sum$ExperimentDay)
#all 3: p-value = 7.869e-08
#w/o precip: p-value = 9.586e-11
#w/o temp: p-value = 2.855e-07
#w/o precip and temp: 1.142e-07


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.n2 <- df.n2.sum[which(df.n2.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.n2 <- lme(N2_Conc~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.n2)

shapiro.test(lme_Before.n2$residuals[,2])
#normal: p-value = 1.275e-09
#sqrt: p-value = 0.0001977
#log: p-value = 0.003487
#recip: p-value = 1.441e-13
plot(lme_Before.n2$residuals[,2])
anova(lme_Before.n2)
#temp and experiment day significant
summary(lme_Before.n2)

# LME for separate periods
#var <- aggregate(Before.n$N_Conc, by=paste(Before.n$ExperimentDay, Before.n$Temp_level)%>%as.character%>%list, FUN=var)
#assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
#Before.n$wn <- sapply(paste(Before.n$ExperimentDay, Before.n$Temp_level), assign_var)
#lme_Before.n <- lme(sqrt(N_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, weights = varFunc(~wn), data = Before.n)

#shapiro.test(lme_Before.n$residuals[,2])
#plot(lme_Before.n$residuals[,2])
#anova(lme_Before.n)



#AFTER RUNOFF EVENT
After.n2 <- df.n2.sum[which(df.n2.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.n2 <- lme(log(N2_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.n2)

shapiro.test(lme_After.n2$residuals[,2])
#normal: p-value = 4.753e-08
#sqrt: p-value = 0.000186
#log: p-value = 0.151
#recip: p-value = 4.545e-06
plot(lme_After.n2$residuals[,2])
anova(lme_After.n2)
#temp, time significant 
summary(lme_After.n2)
#time significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Before = time, temp  After = ???) 

Before.n2 <- df.n2.sum[which(df.n2.sum$ExperimentDay<=12),]%>%as.data.frame()
After.n2 <- df.n2.sum[which(df.n2.sum$ExperimentDay>=12),]%>%as.data.frame()

n2.Before.sum <- na.omit(Before.n2)%>%group_by(ExperimentDay)%>%summarise(
  n2.mean = mean(N2_Conc),
  n2.std.er=sd(N2_Conc)/sqrt(length(N2_Conc))*1.96
)%>%as.data.frame()
n2.Before.sum$Treatments <- "No_T_effect"
#2.Before.sum <- n2.Before.sum[,-1]

#n.After.sum <- na.omit(After.n)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  n.mean = mean(N_Conc),
#  n.std.er=sd(N_Conc)/sqrt(length(N_Conc))*1.96
#)%>%as.data.frame()
#n.After.sum$Treatments <- n.After.sum$Precip_level
#n.After.sum <- n.After.sum[,-1]

n2.After.sum <- na.omit(After.n2)%>%group_by(Temp_level, ExperimentDay)%>%summarise(
  n2.mean = mean(N2_Conc),
  n2.std.er=sd(N2_Conc)/sqrt(length(N2_Conc))*1.96
)%>%as.data.frame()
n2.After.sum$Treatments <- n2.After.sum$Temp_level
n2.After.sum <- n2.After.sum[,-1]

n2.sum.plot <- rbind(n2.Before.sum, n2.After.sum)
n2.sum.plot$Treatments <- factor(n2.sum.plot$Treatments, levels=c("No_T_effect", "AvT", "ExT"))



#### GRAPH ####
treat<-(unique(n2.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("black", "deepskyblue", "orangered1")
pch <- 1:length(treat)
lty <- c(1,1,1)
lwd <- c(3,3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(n2.sum.plot$ExperimentDay,n2.sum.plot$n2.mean,ylim = c(min(n2.sum.plot$n2.mean-n2.sum.plot$n2.std.er),max(10)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Nitrite concentration (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.n2.sum$ExperimentDay%>%unique(),labels = df.n2.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,10,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=n2.sum.plot[which(n2.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$n2.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$n2.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$n2.mean-box.dat$n2.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$n2.mean+box.dat$n2.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("All Treatments","18C","24C"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 





















