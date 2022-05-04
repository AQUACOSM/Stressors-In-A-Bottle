###########################################
#### phytot Raw Data Graphytot, Error Bars ####
###########################################
par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

PhyTot.dat <- rex_totals[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","PhyTot","ExperimentDay")]
#dat.PO4 <- dat.PO4[-which(dat.PO4$core_no%in%c(37:39)),]
#PhyTot.dat$Date <- ymd(PhyTot.dat$Date)
#Calculating the error bar by range; time series with range
PhyTot.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                           levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
PhyTot.dat.sum <- na.omit(PhyTot.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  PhyTot.dat.mean = mean(`PhyTot`),
  PhyTot.std.er=sd(`PhyTot`)/sqrt(length(`PhyTot`))*1.96
)%>%as.data.frame()


treat<-unique(PhyTot.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)



##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


##Plotting the data with error bars
plot(PhyTot.dat.sum$ExperimentDay,PhyTot.dat.sum$PhyTot.dat.mean,ylim = c(min(PhyTot.dat.sum$PhyTot.dat.mean-PhyTot.dat.sum$PhyTot.std.er),max(PhyTot.dat.sum$PhyTot.dat.mean+PhyTot.dat.sum$PhyTot.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#plot(PhyTot.dat.sum$Date,PhyTot.dat.sum$PhyTot.dat.mean,ylim = c(min(PhyTot.dat.sum$range.min),max(PhyTot.dat.sum$range.max)),
#     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23, ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8,blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Total Chlorophyll a",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=PhyTot.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=PhyTot.dat.sum[which(PhyTot.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$PhyTot.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$PhyTot.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$PhyTot.dat.mean-box.dat$PhyTot.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$PhyTot.dat.mean+box.dat$PhyTot.std.er),code=3,angle = 90,length=.05,col=col[i])
}

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
##run-through with phytot 
install.packages("nlme")


dat.phytot <- rex_totals[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","PhyTot")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.phytot.sum<-na.omit(dat.phytot)%>%as.data.frame()
names(df.phytot.sum)[6] <- "phytot"
df.phytot.sum$MicrocosmNumber <- as.factor(df.phytot.sum$MicrocosmNumber)
df.phytot.sum$Temp_level <- factor(df.phytot.sum$Temp_level, levels = c("AvT", "ExT"))
df.phytot.sum$Precip_level <- factor(df.phytot.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.phytot.sum$phytot)

require(nlme)
require(car)

lme_modelphytot <- lme(phytot~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.phytot.sum)

# normality test on the residuals
plot(lme_modelphytot$residuals[,2])
hist(lme_modelphytot$residuals[,2])
shapiro.test(lme_modelphytot$residuals[,2])
#p-value = 0.05349


# Initial knowlege by the pilot lme model
anova(lme_modelphytot)
#time, precip, temp*time, temp*precip, time*precip significant
#temp*time*precip almost significant
summary(lme_modelphytot)
#precip, time, precip*temp, time*precip are all p significant
#temp*time*precip almost significant


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME TRANSFORMATION WHOLE DATA SET ####

lme_modelphytot <- lme(1/(phytot)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.phytot.sum)

shapiro.test(lme_modelphytot$residuals[,2])
#none: p-value = 0.05349
#sqrt: p-value = 3.307e-05
#log: p-value = 2.367e-06
#reciprocal: p-value = 3.936e-16
plot(lme_modelphytot$residuals[,2])
hist(lme_modelphytot$residuals[,2])


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### LME WEIGHTED DATA, TRANSFORMATION ####
lme_model <- lme(phytot~Temp_level*Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.phytot.sum)
plot(df.phytot.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.phytot.sum$ExperimentDay)
#all 3: p-value = 0.05329
#w/o precip: p-value = 
#w/o temp: p-value = 
#w/o precip and temp: p-value = 

require(dplyr)

var <- aggregate(df.phytot.sum$phytot, by=paste(df.phytot.sum$Precip_level, df.phytot.sum$ExperimentDay, df.phytot.sum$Temp_level)%>%as.character%>%list, FUN=var)

assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
df.phytot.sum$wphytot <- sapply(paste(df.phytot.sum$Precip_level, df.phytot.sum$ExperimentDay, df.phytot.sum$Temp_level), assign_var)

lme_model <- lme(1/(phytot)~Precip_level*Temp_level*TIME, weights = varFunc(~wphytot), random = ~1|MicrocosmNumber, data = df.phytot.sum)
shapiro.test(lme_model$residuals[,2])
#normal: p-value = 4.081e-12
#sqrt: p-value = 1.114e-11
#log: p-value = 3.131e-11
#reciprocal: p-value = 2.5e-10

#plot(df.phytot.sum$Date,lme_model$residuals[,2])
#summary(lme_model)
#Anova(lme_model, t=3)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.phytot <- df.phytot.sum[which(df.phytot.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.phytot <- lme(phytot~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.phytot)

shapiro.test(lme_Before.phytot$residuals[,2])
#normal: p-value =  1.311e-05
#sqrt: p-value = 0.008526
#log: p-value = 4.272e-06
#recip: p-value = 1.71e-08
plot(lme_Before.phytot$residuals[,2])
anova(lme_Before.phytot)
#temp, time, temp*time significant
summary(lme_Before.phytot)
#time, temp*time significant


#AFTER RUNOFF EVENT
After.phytot <- df.phytot.sum[which(df.phytot.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.phytot <- lme(phytot~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.phytot)

shapiro.test(lme_After.phytot$residuals[,2])
#normal: p-value = 0.5723
#sqrt: p-value = 0.01998
#log: p-value = 6.827e-06
#recip: p-value = 5.12e-15
plot(lme_After.phytot$residuals[,2])
anova(lme_After.phytot)
#temp, time, temp*time, time*precip significant
summary(lme_After.phytot)
#temp, time, temp*time barely not-significant

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### PLOT DATA ####

#### Averaging values over significant factors ####
#(here: Before = time  After = ???) 

Before.phytot <- df.phytot.sum[which(df.phytot.sum$ExperimentDay<=12),]%>%as.data.frame()
After.phytot <- df.phytot.sum[which(df.phytot.sum$ExperimentDay>=12),]%>%as.data.frame()

phytot.Before.sum <- na.omit(Before.phytot)%>%group_by(ExperimentDay)%>%summarise(
  phytot.mean = mean(phytot),
  phytot.std.er=sd(phytot)/sqrt(length(phytot))*1.96
)%>%as.data.frame()
phytot.Before.sum$Treatments <- "No_T_effect"
#phytot.Before.sum <- phytot.Before.sum[,-1]

#phytot.After.sum <- na.omit(After.phytot)%>%group_by(Precip_level, Temp_level)%>%summarise(
#  phytot.mean = mean(phytot),
#  phytot.std.er=sd(phytot)/sqrt(length(phytot))*1.96
#)%>%as.data.frame()
#phytot.After.sum$Treatments <- phytot.After.sum$Precip_level
#phytot.After.sum <- phytot.After.sum[,-1]

phytot.After.sum <- na.omit(After.phytot)%>%group_by(ExperimentDay)%>%summarise(
  phytot.mean = mean(phytot),
  phytot.std.er=sd(phytot)/sqrt(length(phytot))*1.96
)%>%as.data.frame()
phytot.After.sum$Treatments <- "No_T_effect"
#phytot.After.sum <- phytot.After.sum[,-1]

phytot.sum.plot <- rbind(phytot.Before.sum, phytot.After.sum)
phytot.sum.plot$Treatments <- factor(phytot.sum.plot$Treatments, levels=c("No_T_effect"))



#### GRAphytot ####
treat<-(unique(phytot.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("black")
pch <- 1:length(treat)
lty <- c(1)
lwd <- c(3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(phytot.sum.plot$ExperimentDay,phytot.sum.plot$phytot.mean,ylim = c(min(phytot.sum.plot$phytot.mean-phytot.sum.plot$phytot.std.er),max(12)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("phytot",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.phytot.sum$ExperimentDay%>%unique(),labels = df.phytot.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,12,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=phytot.sum.plot[which(phytot.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$phytot.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$phytot.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$phytot.mean-box.dat$phytot.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$phytot.mean+box.dat$phytot.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("All Treatments"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

