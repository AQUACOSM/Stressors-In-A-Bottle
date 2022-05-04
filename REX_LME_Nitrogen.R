###########################################
#### N Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

NConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","N (mg-L)","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#NConc.dat$Date <- ymd(NConc.dat$Date)
#Calculating the error bar by range; time series with range
NConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                             levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
NConc.dat.sum <- na.omit(NConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  NConc.dat.mean = mean(`N (mg-L)`),
  NConc.std.er=sd(`N (mg-L)`)/sqrt(length(`N (mg-L)`))*1.96
)%>%as.data.frame()

treat<-unique(NConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(NConc.dat.sum$ExperimentDay,NConc.dat.sum$NConc.dat.mean,ylim = c(min(NConc.dat.sum$NConc.dat.mean-NConc.dat.sum$NConc.std.er),80),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Seston Nitrogen (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=NConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=NConc.dat.sum[which(NConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$NConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$NConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$NConc.dat.mean-box.dat$NConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$NConc.dat.mean+box.dat$NConc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)

#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topleft", legend = c("18°C & No Runoff","24°C & No Runoff","18°C & Runoff","24°C & Runoff"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
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


dat.n <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","N (mg-L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.n.sum<-na.omit(dat.n)%>%as.data.frame()
names(df.n.sum)[6] <- "N_Conc"
df.n.sum$MicrocosmNumber <- as.factor(df.n.sum$MicrocosmNumber)
df.n.sum$Temp_level <- factor(df.n.sum$Temp_level, levels = c("AvT", "ExT"))
df.n.sum$Precip_level <- factor(df.n.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.n.sum$N_Conc)

require(nlme)
require(car)

lme_modeln <- lme(N_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.n.sum)

# normality test on the residuals
plot(lme_modeln$residuals[,2])
hist(lme_modeln$residuals[,2])
shapiro.test(lme_modeln$residuals[,2])
#p-value = 6.833e-10


# Initial knowlege by the pilot lme model
anova(lme_modeln)
#precip, temp and time significant
summary(lme_modeln)
#time, time*temp significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modeln <- lme(sqrt(N_Conc)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.n.sum)

shapiro.test(lme_modeln$residuals[,2])
  #sqrt: p-value = 4.11e-06
  #log: p-value = 3.011e-08
  #reciprocal: p-value = 1.573e-07
plot(lme_modeln$residuals[,2])
hist(lme_modeln$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(N_Conc~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.n.sum)
plot(df.n.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.n.sum$ExperimentDay)
  #all 3: p-value = 2.564e-11
  #w/o precip: p-value = 9.586e-11
  #w/o temp: p-value = 2.615e-10
  #w/o precip and temp: 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.n <- df.n.sum[which(df.n.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.n <- lme(log(N_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.n)

shapiro.test(lme_Before.n$residuals[,2])
#normal: p-value = 3.165e-07
#sqrt: p-value = 0.009908
#log: p-value = 0.6171
#recip: p-value = 0.0002716
plot(lme_Before.n$residuals[,2])
anova(lme_Before.n)
  #temp and experiment day significant
summary(lme_Before.n)
#time, temp*time significant

# LME for separate periods
#var <- aggregate(Before.n$N_Conc, by=paste(Before.n$ExperimentDay, Before.n$Temp_level)%>%as.character%>%list, FUN=var)
#assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
#Before.n$wn <- sapply(paste(Before.n$ExperimentDay, Before.n$Temp_level), assign_var)
#lme_Before.n <- lme(sqrt(N_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, weights = varFunc(~wn), data = Before.n)

#shapiro.test(lme_Before.n$residuals[,2])
#plot(lme_Before.n$residuals[,2])
#anova(lme_Before.n)



#AFTER RUNOFF EVENT
After.n <- df.n.sum[which(df.n.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.n <- lme(sqrt(N_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.n)

shapiro.test(lme_After.n$residuals[,2])
#normal: p-value = p-value = 0.001702
#sqrt: p-value = 0.6634
#log: p-value = 0.000767
#recip: p-value = 5.063e-15
plot(lme_After.n$residuals[,2])
anova(lme_After.n)
#no significant factors
summary(lme_After.n)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Before = time, temp  After = ???) 

Before.n <- df.n.sum[which(df.n.sum$ExperimentDay<=12),]%>%as.data.frame()
After.n <- df.n.sum[which(df.n.sum$ExperimentDay>=12),]%>%as.data.frame()

After.n.ExT <- After.n[which(After.n$Temp_level=="ExT"),]%>%as.data.frame()
After.n.AvT <- After.n[which(After.n$Temp_level=="AvT"),]%>%as.data.frame()

lme_After.n.AvT <- lme(sqrt(N_Conc)~ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.n.AvT)

shapiro.test(lme_After.n.AvT$residuals[,2])
#normal: p-value = p-value = 0.001702
#sqrt: p-value = 0.6634
#log: p-value = 0.000767
#recip: p-value = 5.063e-15
plot(lme_After.n.AvT$residuals[,2])
anova(lme_After.n.AvT)
#no significant factors
summary(lme_After.n.AvT)

n.Before.sum <- na.omit(Before.n)%>%group_by(Temp_level,ExperimentDay)%>%summarise(
  n.mean = mean(N_Conc),
  n.std.er=sd(N_Conc)/sqrt(length(N_Conc))*1.96
)%>%as.data.frame()
n.Before.sum$Treatments <- n.Before.sum$Temp_level
n.Before.sum <- n.Before.sum[,-1]

#n.After.sum <- na.omit(After.n)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  n.mean = mean(N_Conc),
#  n.std.er=sd(N_Conc)/sqrt(length(N_Conc))*1.96
#)%>%as.data.frame()
#n.After.sum$Treatments <- n.After.sum$Precip_level
#n.After.sum <- n.After.sum[,-1]

n.After.sum <- na.omit(After.n)%>%group_by(ExperimentDay)%>%summarise(
  n.mean = mean(N_Conc),
  n.std.er=sd(N_Conc)/sqrt(length(N_Conc))*1.96
)%>%as.data.frame()
n.After.sum$Treatments <- "No_T_effect"
#n.After.sum <- n.After.sum[,-1]

n.sum.plot <- rbind(n.Before.sum, n.After.sum)
n.sum.plot$Treatments <- factor(n.sum.plot$Treatments, levels=c("AvT", "ExT", "No_T_effect"))



#### GRAPH ####
treat<-(unique(n.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("deepskyblue", "orangered1", "black")
pch <- 1:length(treat)
lty <- c(1,1,1)
lwd <- c(3,3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(n.sum.plot$ExperimentDay,n.sum.plot$n.mean,ylim = c(min(n.sum.plot$n.mean-n.sum.plot$n.std.er),max(60)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Nitrogen concentration (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.n.sum$ExperimentDay%>%unique(),labels = df.n.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,60,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=n.sum.plot[which(n.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$n.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$n.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$n.mean-box.dat$n.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$n.mean+box.dat$n.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("18C No Runoff","24C No Runoff","All Treatments"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 





















