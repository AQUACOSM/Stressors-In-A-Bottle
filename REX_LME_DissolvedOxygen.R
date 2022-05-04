###########################################
#### DO Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

DO.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","Dissolved Oxygen","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#DO.dat$Date <- ymd(DO.dat$Date)
#Calculating the error bar by range; time series with range
DO.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                       levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
DO.dat.sum <- na.omit(DO.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  DO.dat.mean = mean(`Dissolved Oxygen`),
  DO.std.er=sd(`Dissolved Oxygen`)/sqrt(length(`Dissolved Oxygen`))*1.96
)%>%as.data.frame()

treat<-unique(DO.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(DO.dat.sum$ExperimentDay,DO.dat.sum$DO.dat.mean,ylim = c(min(DO.dat.sum$DO.dat.mean-DO.dat.sum$DO.std.er),max(DO.dat.sum$DO.dat.mean+DO.dat.sum$DO.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("DO (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=DO.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=DO.dat.sum[which(DO.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$DO.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$DO.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$DO.dat.mean-box.dat$DO.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$DO.dat.mean+box.dat$DO.std.er),code=3,angle = 90,length=.05,col=col[i])
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
##run-through with DO (mg/L)
install.packages("nlme")


dat.DO <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","Dissolved Oxygen")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.DO.sum<-na.omit(dat.DO)%>%as.data.frame()
names(df.DO.sum)[6] <- "DO"
df.DO.sum$MicrocosmNumber <- as.factor(df.DO.sum$MicrocosmNumber)
df.DO.sum$Temp_level <- factor(df.DO.sum$Temp_level, levels = c("AvT", "ExT"))
df.DO.sum$Precip_level <- factor(df.DO.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.DO.sum$DO)

require(nlme)
require(car)

lme_modelDO <- lme(DO~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.DO.sum)

# normality test on the residuals
plot(lme_modelDO$residuals[,2])
hist(lme_modelDO$residuals[,2])
shapiro.test(lme_modelDO$residuals[,2])
#p-value = 3.499e-11


# Initial knowlege by the pilot lme model
anova(lme_modelDO)
#time, temp, precip significant
summary(lme_modelDO)
# time, precip*time, time*temp significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modelDO <- lme(log(DO)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.DO.sum)

shapiro.test(lme_modelDO$residuals[,2])
#sqrt: p-value = 3.702e-08
#log: p-value = 2.427e-06
#reciprocal: p-value = 6.758e-08
#plot(lme_modelDO$residuals[,2])
#hist(lme_modelDO$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(DO~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.DO.sum)
plot(df.DO.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.DO.sum$ExperimentDay)
#all 3: p-value = 0.2974
#w/o precip: p-value = 
#w/o temp: p-value = 
#w/o precip and temp: p-value = 

require(dplyr)

var <- aggregate(df.DO.sum$DO, by=paste(df.DO.sum$Precip_level, df.DO.sum$ExperimentDay, df.DO.sum$Temp_level)%>%as.character%>%list, FUN=var)

assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
df.DO.sum$wDO <- sapply(paste(df.DO.sum$Precip_level, df.DO.sum$ExperimentDay, df.DO.sum$Temp_level), assign_var)

lme_model <- lme(log(DO)~Precip_level*Temp_level*TIME, weights = varFunc(~wDO), random = ~1|MicrocosmNumber, data = df.DO.sum)
shapiro.test(lme_model$residuals[,2])
#normal: p-value = 1.393e-10
#sqrt: p-value = 7.381e-07
#log: p-value = 3.041e-05
#reciprocal: p-value = 1.611e-08

#plot(df.DO.sum$Date,lme_model$residuals[,2])
#summary(lme_model)
#Anova(lme_model, t=3)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.DO <- df.DO.sum[which(df.DO.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.DO <- lme(DO~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.DO)

shapiro.test(lme_Before.DO$residuals[,2])
#normal: p-value = 0.01692
#sqrt: p-value = 0.01163
#log: p-value = 0.007196
#recip: p-value = 0.002338
plot(lme_Before.DO$residuals[,2])
anova(lme_Before.DO)
#temp, experiment day significant
summary(lme_Before.DO)
#time, time*temp significant


#AFTER RUNOFF EVENT
After.DO <- df.DO.sum[which(df.DO.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.DO <- lme(DO~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.DO)

shapiro.test(lme_After.DO$residuals[,2])
#normal: p-value = 1.686e-08
#sqrt: p-value = 1.028e-06
#log: p-value = 6.447e-06
#recip: p-value = 2.228e-08
plot(lme_After.DO$residuals[,2])
anova(lme_After.DO)
#temp, temp*time significant
summary(lme_After.DO)
#no significant factors

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#no lme tests significant, therefore sticking to time as plotting factor
#(here: Before = time, temp  After = ???) 

Before.DO <- df.DO.sum[which(df.DO.sum$ExperimentDay<=12),]%>%as.data.frame()
After.DO <- df.DO.sum[which(df.DO.sum$ExperimentDay>=12),]%>%as.data.frame()

DO.Before.sum <- na.omit(Before.DO)%>%group_by(ExperimentDay)%>%summarise(
  DO.mean = mean(DO),
  DO.std.er=sd(DO)/sqrt(length(DO))*1.96
)%>%as.data.frame()
DO.Before.sum$Treatments <- "No_T_effect"
#DO.Before.sum <- DO.Before.sum[,-1]

#DO.After.sum <- na.omit(After.DO)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  DO.mean = mean(DO),
#  DO.std.er=sd(DO)/sqrt(length(DO))*1.96
#)%>%as.data.frame()
#DO.After.sum$Treatments <- DO.After.sum$Precip_level
#DO.After.sum <- DO.After.sum[,-1]

DO.After.sum <- na.omit(After.DO)%>%group_by(ExperimentDay)%>%summarise(
  DO.mean = mean(DO),
  DO.std.er=sd(DO)/sqrt(length(DO))*1.96
)%>%as.data.frame()
DO.After.sum$Treatments <- "No_T_effect"
#DO.After.sum <- DO.After.sum[,-1]

DO.sum.plot <- rbind(DO.Before.sum, DO.After.sum)
DO.sum.plot$Treatments <- factor(DO.sum.plot$Treatments, levels=c("No_T_effect"))



#### GRAPH ####
treat<-(unique(DO.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("black")
pch <- 1:length(treat)
lty <- c(1)
lwd <- c(3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(DO.sum.plot$ExperimentDay,DO.sum.plot$DO.mean,ylim = c(min(DO.sum.plot$DO.mean-DO.sum.plot$DO.std.er),max(12)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Dissolved Oxygen (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.DO.sum$ExperimentDay%>%unique(),labels = df.DO.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,12,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=DO.sum.plot[which(DO.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$DO.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$DO.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$DO.mean-box.dat$DO.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$DO.mean+box.dat$DO.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("All Treatments"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 


 

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 




















