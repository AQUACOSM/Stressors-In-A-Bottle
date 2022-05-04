###########################################
#### NH4 Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

N4Conc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","NH4 (mg-L)","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#N4Conc.dat$Date <- ymd(N4Conc.dat$Date)
#Calculating the error bar by range; time series with range
N4Conc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                          levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
N4Conc.dat.sum <- na.omit(N4Conc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  N4Conc.dat.mean = mean(`NH4 (mg-L)`),
  N4Conc.std.er=sd(`NH4 (mg-L)`)/sqrt(length(`NH4 (mg-L)`))*1.96
)%>%as.data.frame()

treat<-unique(N4Conc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(N4Conc.dat.sum$ExperimentDay,N4Conc.dat.sum$N4Conc.dat.mean,ylim = c(min(N4Conc.dat.sum$N4Conc.dat.mean-N4Conc.dat.sum$N4Conc.std.er),max(N4Conc.dat.sum$N4Conc.dat.mean+N4Conc.dat.sum$N4Conc.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("NH4 (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=N4Conc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=N4Conc.dat.sum[which(N4Conc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$N4Conc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$N4Conc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$N4Conc.dat.mean-box.dat$N4Conc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$N4Conc.dat.mean+box.dat$N4Conc.std.er),code=3,angle = 90,length=.05,col=col[i])
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
##run-through with NH4 (mg/L)
install.packages("nlme")


dat.n4 <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","NH4 (mg-L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.n4.sum<-na.omit(dat.n4)%>%as.data.frame()
names(df.n4.sum)[6] <- "N4_Conc"
df.n4.sum$MicrocosmNumber <- as.factor(df.n4.sum$MicrocosmNumber)
df.n4.sum$Temp_level <- factor(df.n4.sum$Temp_level, levels = c("AvT", "ExT"))
df.n4.sum$Precip_level <- factor(df.n4.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.n4.sum$N4_Conc)

require(nlme)
require(car)

lme_modeln4 <- lme(N4_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.n4.sum)

# normality test on the residuals
plot(lme_modeln4$residuals[,2])
hist(lme_modeln4$residuals[,2])
shapiro.test(lme_modeln4$residuals[,2])
#p-value = < 2.2e-16


# Initial knowlege by the pilot lme model
anova(lme_modeln4)
summary(lme_modeln4)
#precip, temp and time are all p significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modeln4 <- lme(1/(N4_Conc)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.n4.sum)

shapiro.test(lme_modeln4$residuals[,2])
#sqrt: p-value = < 2.2e-16
#log: p-value = 2.445e-14
#reciprocal: p-value = < 2.2e-16
plot(lme_modeln4$residuals[,2])
hist(lme_modeln4$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(N4_Conc~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.n4.sum)
plot(df.n4.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.n4.sum$ExperimentDay)
#all 3: p-value = 0.00638
#w/o precip: p-value = 0.01049
#w/o temp: p-value = 0.005187
#w/o precip and temp: 0.009194

var <- aggregate(df.n4.sum$N4_Conc, by=paste(df.n4.sum$ExperimentDay, df.n4.sum$Temp_level)%>%as.character%>%list, FUN=var)
assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
df.n4.sum$wn4 <- sapply(paste(df.n4.sum$ExperimentDay, df.n4.sum$Temp_level), assign_var)
lme_model <- lme(1/(N4_Conc)~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, weights=varFunc(~wn4) ,data = df.n4.sum)

shapiro.test(lme_model$residuals[,2])
#normal: p-value = 0.01049
#sqrt: p-value = 8.869e-10
#log: p-value = 4.046e-07
#recip: p-value = 3.08e-10

plot(lme_model$residuals[,2])
hist(lme_model$residuals[,2])

summary(lme_model)
Anova(lme_model, t=3)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.n4 <- df.n4.sum[which(df.n4.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.n4 <- lme(N4_Conc~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.n4)

shapiro.test(lme_Before.n4$residuals[,2])
#normal: p-value = 4.036e-16
#sqrt: p-value = 4.06e-11
#log: p-value = 3.29e-06
#recip: p-value = < 2.2e-16
plot(lme_Before.n4$residuals[,2])
anova(lme_Before.n4)
summary(lme_Before.n4)


#AFTER RUNOFF EVENT
After.n4 <- df.n4.sum[which(df.n4.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.n4 <- lme(N4_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.n4)

shapiro.test(lme_After.n4$residuals[,2])
#normal: p-value = < 2.2e-16
#sqrt: p-value = 2.621e-16
#log: p-value = 5.077e-08
#recip: p-value = 1.262e-08
plot(lme_After.n4$residuals[,2])
anova(lme_After.n4)
summary(lme_After.n4)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: no normality test passed) 

Before.n4 <- df.n4.sum[which(df.n4.sum$ExperimentDay<=12),]%>%as.data.frame()
After.n4 <- df.n4.sum[which(df.n4.sum$ExperimentDay>=12),]%>%as.data.frame()


n4.Before.sum <- na.omit(Before.n4)%>%group_by(ExperimentDay)%>%summarise(
  n4.mean = mean(N4_Conc),
  n4.std.er=sd(N4_Conc)/sqrt(length(N4_Conc))*1.96
)%>%as.data.frame()
n4.Before.sum$Treatments <- "No_T_effect"
#n4.Before.sum <- "No_T_Effect"

#n.After.sum <- na.omit(After.n)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  n.mean = mean(N_Conc),
#  n.std.er=sd(N_Conc)/sqrt(length(N_Conc))*1.96
#)%>%as.data.frame()
#n.After.sum$Treatments <- n.After.sum$Precip_level
#n.After.sum <- n.After.sum[,-1]

n4.After.sum <- na.omit(After.n4)%>%group_by(ExperimentDay)%>%summarise(
  n4.mean = mean(N4_Conc),
  n4.std.er=sd(N4_Conc)/sqrt(length(N4_Conc))*1.96
)%>%as.data.frame()
n4.After.sum$Treatments <- "No_T_effect"
#n.After.sum <- n.After.sum[,-1]

n4.sum.plot <- rbind(n4.Before.sum, n4.After.sum)
n4.sum.plot$Treatments <- factor(n4.sum.plot$Treatments, levels=c("No_T_effect"))



#### GRAPH ####
treat<-(unique(n4.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c( "black")
pch <- 1:length(treat)
lty <- c(1)
lwd <- c(3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(n4.sum.plot$ExperimentDay,n4.sum.plot$n4.mean,ylim = c(min(n4.sum.plot$n4.mean-n4.sum.plot$n4.std.er),max(n4.sum.plot$n4.mean+n4.sum.plot$n4.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Ammonia concentration (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.n4.sum$ExperimentDay%>%unique(),labels = df.n4.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,10,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=n4.sum.plot[which(n4.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$n4.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$n4.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$n4.mean-box.dat$n4.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$n4.mean+box.dat$n4.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("All Treatments"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 




















