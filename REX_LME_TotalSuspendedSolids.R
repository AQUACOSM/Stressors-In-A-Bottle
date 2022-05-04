###########################################
#### TSS Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

TSS.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","Total Suspended Solids","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#TSS.dat$Date <- ymd(TSS.dat$Date)
#Calculating the error bar by range; time series with range
TSS.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                            levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
TSS.dat.sum <- na.omit(TSS.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  TSS.dat.mean = mean(`Total Suspended Solids`),
  TSS.std.er=sd(`Total Suspended Solids`)/sqrt(length(`Total Suspended Solids`))*1.96
)%>%as.data.frame()

treat<-unique(TSS.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(TSS.dat.sum$ExperimentDay,TSS.dat.sum$TSS.dat.mean,ylim = c(min(TSS.dat.sum$TSS.dat.mean-TSS.dat.sum$TSS.std.er),max(TSS.dat.sum$TSS.dat.mean+TSS.dat.sum$TSS.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Total Suspended Solids",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=TSS.dat.sum$ExperimentDay%>%unique(),labels = c(0,7,14,21))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=TSS.dat.sum[which(TSS.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$TSS.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$TSS.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$TSS.dat.mean-box.dat$TSS.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$TSS.dat.mean+box.dat$TSS.std.er),code=3,angle = 90,length=.05,col=col[i])
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
##run-through with TSS
install.packages("nlme")


dat.tss <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","Total Suspended Solids")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.tss.sum<-na.omit(dat.tss)%>%as.data.frame()
names(df.tss.sum)[6] <- "TSS"
df.tss.sum$MicrocosmNumber <- as.factor(df.tss.sum$MicrocosmNumber)
df.tss.sum$Temp_level <- factor(df.tss.sum$Temp_level, levels = c("AvT", "ExT"))
df.tss.sum$Precip_level <- factor(df.tss.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.tss.sum$TSS)

require(nlme)
require(car)

lme_modeltss <- lme(TSS~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.tss.sum)

# normality test on the residuals
plot(lme_modeltss$residuals[,2])
hist(lme_modeltss$residuals[,2])
shapiro.test(lme_modeltss$residuals[,2])
#p-value = 6.338e-12


# Initial knowlege by the pilot lme model
anova(lme_modeltss)
#precip, time, precip*temp, precip*time, precip*temp*time
summary(lme_modeltss)
#time, precip*time significant



##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modeltss <- lme(log(TSS)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.tss.sum)

shapiro.test(lme_modeltss$residuals[,2])
#sqrt: p-value = 2.046e-06
#log: p-value = 0.377
#reciprocal: p-value = 0.1141
plot(lme_modeltss$residuals[,2])
hist(lme_modeltss$residuals[,2])
anova(lme_modeltss)
#precip, time, precip*time significant
summary(lme_modeltss)
#time significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
#lme_model <- lme(TSS~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.tss.sum)
#plot(df.tss.sum$ExperimentDay,lme_model$residuals[,2])
#bptest(lme_model$residuals[,2]~df.tss.sum$ExperimentDay)
#all 3: p-value = 
#w/o precip: p-value = 
#w/o temp: p-value = 
#w/o precip and temp: 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.tss <- df.tss.sum[which(df.tss.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.tss <- lme(log(TSS)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.tss)

shapiro.test(lme_Before.tss$residuals[,2])
#normal: p-value = 3.165e-07
#sqrt: p-value = 0.009908
#log: p-value = 0.2732
#recip: p-value = 0.0002716
#plot(lme_Before.tss$residuals[,2])
anova(lme_Before.tss)
#temp and experiment day significant
summary(lme_Before.tss)

# LME for separate periods
#var <- aggregate(Before.n$N_Conc, by=paste(Before.n$ExperimentDay, Before.n$Temp_level)%>%as.character%>%list, FUN=var)
#assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
#Before.n$wn <- sapply(paste(Before.n$ExperimentDay, Before.n$Temp_level), assign_var)
#lme_Before.n <- lme(sqrt(N_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, weights = varFunc(~wn), data = Before.n)

#shapiro.test(lme_Before.n$residuals[,2])
#plot(lme_Before.n$residuals[,2])
#anova(lme_Before.n)



#AFTER RUNOFF EVENT
After.tss <- df.tss.sum[which(df.tss.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.tss <- lme(log(TSS)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.tss)

shapiro.test(lme_After.tss$residuals[,2])
#normal: p-value = 1.402e-05
#sqrt: p-value = 0.005195
#log: p-value = 0.6361
#recip: p-value = 0.0002028
#plot(lme_After.tss$residuals[,2])
anova(lme_After.tss)
#no significant factors
summary(lme_After.tss)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Full = time) 

Before.tss <- df.tss.sum[which(df.tss.sum$ExperimentDay<=14),]%>%as.data.frame()
After.tss <- df.tss.sum[which(df.tss.sum$ExperimentDay>=14),]%>%as.data.frame()


tss.Before.sum <- na.omit(Before.tss)%>%group_by(ExperimentDay)%>%summarise(
  tss.mean = mean(TSS),
  tss.std.er=sd(TSS)/sqrt(length(TSS))*1.96
)%>%as.data.frame()
tss.Before.sum$Treatments <- "No_P_effect"
#tss.Before.sum <- tss.Before.sum[,-1]

#n.After.sum <- na.omit(After.n)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  n.mean = mean(N_Conc),
#  n.std.er=sd(N_Conc)/sqrt(length(N_Conc))*1.96
#)%>%as.data.frame()
#n.After.sum$Treatments <- n.After.sum$Precip_level
#n.After.sum <- n.After.sum[,-1]

tss.After.sum <- na.omit(After.tss)%>%group_by(ExperimentDay)%>%summarise(
  tss.mean = mean(TSS),
  tss.std.er=sd(TSS)/sqrt(length(TSS))*1.96
)%>%as.data.frame()
tss.After.sum$Treatments <- "No_P_effect"
#tss.After.sum <- tss.After.sum[,-2]

tss.sum.plot <- rbind(tss.Before.sum, tss.After.sum)
tss.sum.plot$Treatments <- factor(tss.sum.plot$Treatments, levels=c("No_P_effect"))



#### GRAPH ####
treat<-(unique(tss.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("black")
pch <- 1:length(treat)
lty <- c(1)
lwd <- c(3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(tss.sum.plot$ExperimentDay,tss.sum.plot$tss.mean,ylim = c(min(tss.sum.plot$tss.mean-tss.sum.plot$tss.std.er),max(tss.sum.plot$tss.mean+tss.sum.plot$tss.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Total Suspended Solids (mg  ", L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.tss.sum$ExperimentDay%>%unique(),labels = df.tss.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,40,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=tss.sum.plot[which(tss.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$tss.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$tss.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$tss.mean-box.dat$tss.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$tss.mean+box.dat$tss.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("All Treatments"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 




















