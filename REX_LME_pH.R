###########################################
#### pH Raw Data Graph, Error Bars ####
###########################################
par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

pH.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","pH","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#pH.dat$Date <- ymd(pH.dat$Date)
#Calculating the error bar by range; time series with range
pH.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                       levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
pH.dat.sum <- na.omit(pH.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  pH.dat.mean = mean(`pH`),
  pH.std.er=sd(`pH`)/sqrt(length(`pH`))*1.96
)%>%as.data.frame()


treat<-unique(pH.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

##Plotting the data with error bars
plot(pH.dat.sum$ExperimentDay,pH.dat.sum$pH.dat.mean,ylim = c(min(pH.dat.sum$pH.dat.mean-pH.dat.sum$pH.std.er),max(pH.dat.sum$pH.dat.mean+pH.dat.sum$pH.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#plot(pH.dat.sum$Date,pH.dat.sum$pH.dat.mean,ylim = c(min(pH.dat.sum$range.min),max(pH.dat.sum$range.max)),
#     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23, ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8,blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("pH",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=pH.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=pH.dat.sum[which(pH.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$pH.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$pH.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$pH.dat.mean-box.dat$pH.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$pH.dat.mean+box.dat$pH.std.er),code=3,angle = 90,length=.05,col=col[i])
}

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
##run-through with pH 
install.packages("nlme")


dat.pH <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","pH")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.pH.sum<-na.omit(dat.pH)%>%as.data.frame()
names(df.pH.sum)[6] <- "pH"
df.pH.sum$MicrocosmNumber <- as.factor(df.pH.sum$MicrocosmNumber)
df.pH.sum$Temp_level <- factor(df.pH.sum$Temp_level, levels = c("AvT", "ExT"))
df.pH.sum$Precip_level <- factor(df.pH.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.pH.sum$pH)

require(nlme)
require(car)

lme_modelpH <- lme(pH~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.pH.sum)

# normality test on the residuals
plot(lme_modelpH$residuals[,2])
hist(lme_modelpH$residuals[,2])
shapiro.test(lme_modelpH$residuals[,2])
#p-value = 2.688e-13


# Initial knowlege by the pilot lme model
anova(lme_modelpH)
#time, temp, precip significant
summary(lme_modelpH)
#precip, time, precip*time, time*temp are all p significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modelpH <- lme(1/(pH)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.pH.sum)

shapiro.test(lme_modelpH$residuals[,2])
#sqrt: p-value = 5.657e-13
#log: p-value = 1.05e-12
#reciprocal: p-value = 2.212e-12
plot(lme_modelpH$residuals[,2])
hist(lme_modelpH$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(pH~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.pH.sum)
plot(df.pH.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.pH.sum$ExperimentDay)
#all 3: p-value = 0.05329
#w/o precip: p-value = 
#w/o temp: p-value = 
#w/o precip and temp: p-value = 

require(dplyr)

var <- aggregate(df.pH.sum$pH, by=paste(df.pH.sum$Precip_level, df.pH.sum$ExperimentDay, df.pH.sum$Temp_level)%>%as.character%>%list, FUN=var)

assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
df.pH.sum$wpH <- sapply(paste(df.pH.sum$Precip_level, df.pH.sum$ExperimentDay, df.pH.sum$Temp_level), assign_var)

lme_model <- lme(1/(pH)~Precip_level*Temp_level*TIME, weights = varFunc(~wpH), random = ~1|MicrocosmNumber, data = df.pH.sum)
shapiro.test(lme_model$residuals[,2])
#normal: p-value = 4.081e-12
#sqrt: p-value = 1.114e-11
#log: p-value = 3.131e-11
#reciprocal: p-value = 2.5e-10

#plot(df.pH.sum$Date,lme_model$residuals[,2])
#summary(lme_model)
#Anova(lme_model, t=3)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.pH <- df.pH.sum[which(df.pH.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.pH <- lme(log(pH)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.pH)

shapiro.test(lme_Before.pH$residuals[,2])
#normal: p-value = 0.5319
#sqrt: p-value = 0.56
#log: p-value = 0.571
#recip: p-value = 0.3241
plot(lme_Before.pH$residuals[,2])
anova(lme_Before.pH)
#experiment day significant
summary(lme_Before.pH)
#time significant


#AFTER RUNOFF EVENT
After.pH <- df.pH.sum[which(df.pH.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.pH <- lme(pH~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.pH)

shapiro.test(lme_After.pH$residuals[,2])
#normal: p-value = 9e-14
#sqrt: p-value = 4.456e-14
#log: p-value = 2.2e-14
#recip: p-value = 5.345e-15
plot(lme_After.pH$residuals[,2])
anova(lme_After.pH)
#temp significant
summary(lme_After.pH)
#temp*time barely not-significant

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Before = time  After = ???) 

Before.pH <- df.pH.sum[which(df.pH.sum$ExperimentDay<=12),]%>%as.data.frame()
After.pH <- df.pH.sum[which(df.pH.sum$ExperimentDay>=12),]%>%as.data.frame()

pH.Before.sum <- na.omit(Before.pH)%>%group_by(ExperimentDay)%>%summarise(
  pH.mean = mean(pH),
  pH.std.er=sd(pH)/sqrt(length(pH))*1.96
)%>%as.data.frame()
pH.Before.sum$Treatments <- "No_T_effect"
#pH.Before.sum <- pH.Before.sum[,-1]

#pH.After.sum <- na.omit(After.pH)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  pH.mean = mean(pH),
#  pH.std.er=sd(pH)/sqrt(length(pH))*1.96
#)%>%as.data.frame()
#pH.After.sum$Treatments <- pH.After.sum$Precip_level
#pH.After.sum <- pH.After.sum[,-1]

pH.After.sum <- na.omit(After.pH)%>%group_by(ExperimentDay)%>%summarise(
  pH.mean = mean(pH),
  pH.std.er=sd(pH)/sqrt(length(pH))*1.96
)%>%as.data.frame()
pH.After.sum$Treatments <- "No_T_effect"
#pH.After.sum <- pH.After.sum[,-1]

pH.sum.plot <- rbind(pH.Before.sum, pH.After.sum)
pH.sum.plot$Treatments <- factor(pH.sum.plot$Treatments, levels=c("No_T_effect"))



#### GRAPH ####
treat<-(unique(pH.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("black")
pch <- 1:length(treat)
lty <- c(1)
lwd <- c(3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(pH.sum.plot$ExperimentDay,pH.sum.plot$pH.mean,ylim = c(min(pH.sum.plot$pH.mean-pH.sum.plot$pH.std.er),max(12)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("pH",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.pH.sum$ExperimentDay%>%unique(),labels = df.pH.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,12,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=pH.sum.plot[which(pH.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$pH.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$pH.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$pH.mean-box.dat$pH.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$pH.mean+box.dat$pH.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("All Treatments"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 




















