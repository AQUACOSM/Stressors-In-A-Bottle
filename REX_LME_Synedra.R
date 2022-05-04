## REX adaptation of Qing LME script

###########################################
#### Brown Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

BrownConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real",
                                  "Brown Concentration","ExperimentDay")]
#BrownConc.dat$Date <- ymd(BrownConc.dat$Date)

#Calculating the error bar by range; time series with range
BrownConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                              levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
BrownConc.dat.sum <- na.omit(BrownConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  BrownConc.dat.mean = mean(`Brown Concentration`),
  BrownConc.std.er=sd(`Brown Concentration`)/sqrt(length(`Brown Concentration`))*1.96
)%>%as.data.frame()

treat<-unique(BrownConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
##Plotting the data with error bars
plot(BrownConc.dat.sum$ExperimentDay,BrownConc.dat.sum$BrownConc.dat.mean,ylim = c(min(BrownConc.dat.sum$BrownConc.dat.mean-BrownConc.dat.sum$BrownConc.std.er),2335.143),
     type="n",xlab="",ylab = "",xaxt="n",las=1)

#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)


mtext(expression(paste("Synedra sp. (ug/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=BrownConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=BrownConc.dat.sum[which(BrownConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$BrownConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$BrownConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$BrownConc.dat.mean-box.dat$BrownConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$BrownConc.dat.mean+box.dat$BrownConc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)

#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topleft", legend = c("18°C & No Runoff","24°C & No Runoff","18°C & Runoff","24°C & Runoff"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
       text.col =col,cex = 1,lwd = c(3,3,3,3)) 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

###########################################
#### REX adaptation of Qing LME script ####
###########################################

##run-through with brown concentration
install.packages("nlme")


dat.brc <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Date","ExperimentDay","Brown Concentration")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

#df.brc$Precip_treatment[which(df.brc$ExperimentDay<=14)]<-"AvP" # heatwave simulation after the sampling on 2018-07-02

df.brc.sum<-na.omit(dat.brc)%>%as.data.frame()
names(df.brc.sum)[7] <- "Br_Conc"
df.brc.sum$MicrocosmNumber <- as.factor(df.brc.sum$MicrocosmNumber)
df.brc.sum$Temp_level <- factor(df.brc.sum$Temp_level, levels = c("AvT", "ExT"))
df.brc.sum$Precip_level <- factor(df.brc.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.brc.sum$Br_Conc)

require(nlme)
require(car)

lme_modelbr <- lme(Br_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.brc.sum)


# normality test on the residuals
plot(lme_modelbr$residuals[,2])
hist(lme_modelbr$residuals[,2])
shapiro.test(lme_modelbr$residuals[,2])
#p-value = 1.625e-10


# Initial knowlege by the pilot lme model
anova(lme_modelbr)
summary(lme_modelbr)

# Normality correction for whole dataset
require(dplyr)
var <- aggregate(df.brc.sum$Br_Conc, by=paste(df.brc.sum$Precip_level, df.brc.sum$ExperimentDay, df.brc.sum$Temp_level)%>%as.character%>%list, FUN=var)

assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
df.brc.sum$wg <- sapply(paste(df.brc.sum$Precip_level, df.brc.sum$ExperimentDay, df.brc.sum$Temp_level), assign_var)

lme_AvT.br <- lme(1/(Br_Conc+.1)~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = df.brc.sum)
#sqrt: p-value = 0.02633
#log: p-value = 1.135e-15
#recip.: p-value < 2.2e-16
plot(lme_AvT.br$residuals[,2])
shapiro.test(lme_AvT.br$residuals[,2])
anova(lme_AvT.br)
summary(lme_AvT.br)


# Splitting data by period (before and after run-off)
Before.br <- df.brc.sum[which(df.brc.sum$ExperimentDay<14),]%>%as.data.frame()
#AvT.gc <- df.gc.sum[which(df.gc.sum$Temp_level=="AvT"),]%>%as.data.frame()

# LME for separate periods
var <- aggregate(Before.br$Br_Conc, by=paste(Before.br$ExperimentDay, Before.br$Temp_level)%>%as.character%>%list, FUN=var)
assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
Before.br$wg <- sapply(paste(Before.br$ExperimentDay, Before.br$Temp_level), assign_var)

lme_Before.br <- lme(Br_Conc~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, weights = varFunc(~wg), data = Before.br)
#normal:p-value = 0.1776
#sqrt: p-value = 0.001734
#log: p-value = 1.73e-09
#recip.: p-value = 1.813e-12
#p-value < 2.2e-16
plot(lme_Before.br$residuals[,2])
shapiro.test(lme_Before.br$residuals[,2])
anova(lme_Before.br)
#temp, time significant
summary(lme_Before.br)


#After runoff event
After.br <- df.brc.sum[which(df.brc.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
var <- aggregate(After.br$Br_Conc, by=paste(After.br$ExperimentDay, After.br$Temp_level, After.br$Precip_level)%>%as.character%>%list, FUN=var)
assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
After.br$wg <- sapply(paste(After.br$ExperimentDay, After.br$Temp_level, After.br$Precip_level), assign_var)

lme_After.br <- lme(sqrt(Br_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.br)

#p-value < 2.2e-16
plot(lme_After.br$residuals[,2])
shapiro.test(lme_After.br$residuals[,2])
#normal: p-value = 6.684e-06
#sqrt: p-value = 0.8583
#log: p-value = < 2.2e-16
#recip.: p-value = < 2.2e-16
anova(lme_After.br)
#temp, time significant
summary(lme_After.br)
#no significance

# Plot by warming and time
#### Averaging values over significant factors (here: time for ExT, time and Precipitation treatments for AvT) ####

ExT.br <- df.brc.sum[which(df.brc.sum$Temp_level=="ExT"),]%>%as.data.frame()
AvT.br <- df.brc.sum[which(df.brc.sum$Temp_level=="AvT"),]%>%as.data.frame()


brc.ExT.sum <- na.omit(ExT.br)%>%group_by(ExperimentDay)%>%summarise(
  brc.mean = mean(Br_Conc),
  brc.std.er=sd(Br_Conc)/sqrt(length(Br_Conc))*1.96
)%>%as.data.frame()
brc.ExT.sum$Treatments <- "ExT"

brc.AvT.sum <- na.omit(AvT.br)%>%group_by(ExperimentDay)%>%summarise(
  brc.mean = mean(Br_Conc),
  brc.std.er=sd(Br_Conc)/sqrt(length(Br_Conc))*1.96
)%>%as.data.frame()
brc.AvT.sum$Treatments <- "AvT"


brc.sum.plot <- rbind(brc.ExT.sum, brc.AvT.sum)
brc.sum.plot$Treatments <- factor(brc.sum.plot$Treatments, levels=c("AvT", "ExT"))

treat<-(unique(brc.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("deepskyblue", "orangered1")
pch <- 1:length(treat)
lty <- c(1,1)
lwd <- c(3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(brc.sum.plot$ExperimentDay,brc.sum.plot$brc.mean,ylim = c(min(brc.sum.plot$brc.mean-brc.sum.plot$brc.std.er),max(2000)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Brown algae (ug  ",L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.brc.sum$ExperimentDay%>%unique(),labels = df.brc.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,2000,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=brc.sum.plot[which(brc.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$brc.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$brc.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$brc.mean-box.dat$brc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$brc.mean+box.dat$brc.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("18 °C","24 °C"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# check raw data treatment for normality
##use the raw data from df.bc or use the as.factor altered data (df.bc.sum)?????
#############################
#df.bc.sum as.factor altered data:
plot(VARbr)
#Average Temperature
AvT.br <- df.brc.sum[which(df.brc.sum$Temp_level=="AvT"),]
BlueCon<-c(df.brc.sum$`Brown Concentration`)

#lme_AvT.br <- lme(VARbr~Precip_TREATMENTbr*TIMEbr, random = ~1|SUBJECT,data = df.brc.sum)
    #p-value = 9.966e-13. Not larger than 0.05, so requires transformation for normality
#lme_AvT.br <- lme(sqrt(VARbr)~Precip_TREATMENTbr*TIMEbr, random = ~1|SUBJECT,data = df.brc.sum)
    #p-value = 0.0001105
#lme_AvT.br <- lme(log(VARbr+0.1)~Precip_TREATMENTbr*TIMEbr, random = ~1|SUBJECT,data = df.brc.sum)
    #p-value = 2.244e-15


    #AvT.br
lme_AvT.br <- lme(1/(VARbr+0.1)~Precip_TREATMENTbr*TIMEbr, random = ~1|SUBJECTbr,data = AvT.br)
#p-value < 2.2e-16
anova(lme_AvT.br)
shapiro.test(lme_AvT.br$residuals[,2])
    #p-value < 2.2e-16


  #Extreme Temperature
plot(VARbr)
ExT.br <- df.gc.sum[which(df.brc.sum$Temp_level=="ExT"),]
BrownCon<-c(df.brc.sum$`Brown Concentration`)

#lme_ExT.br <- lme(VARbr~Precip_TREATMENTbr*TIMEbr, random = ~1|SUBJECT,data = ExT.br)
   #p-value = 9.966e-13. Not larger than 0.05, so requires transformation for normality
#lme_ExT.br <- lme(sqrt(VARbr)~Precip_TREATMENTbr*TIMEbr, random = ~1|SUBJECT,data = df.brc.sum)
    #p-value = 0.0001105
#lme_ExT.br <- lme(log(VARbr+0.1)~Precip_TREATMENTbr*TIMEbr, random = ~1|SUBJECT,data = df.brc.sum)
    #p-value = 2.244e-15

lme_ExT.br <- lme(1/(VARbr+0.1)~Precip_TREATMENTbr*Temp_TREATMENTbr*TIMEbr, random = ~1|SUBJECTbr,data = ExT.br)
anova(lme_ExT.br)
shapiro.test(lme_ExT.br$residuals[,2])
    #p-value = 2.244e-15

plot(lme_ExT.br$residuals[,2], col=df.brc.sum$Temp_level%>%as.factor())


#############################
#Weighting the distribution
require(dplyr)
var <- aggregate(df.brc.sum$`Brown Concentration`, by=paste(df.brc.sum$Precip_level, df.brc.sum$ExperimentDay)%>%as.character%>%list, FUN=var)

assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
df.brc.sum$wbr <- sapply(paste(df.brc.sum$Precip_level, df.brc.sum$ExperimentDay), assign_var)

lme_modelbr <- lme(VARbr~Precip_TREATMENTbr*Temp_TREATMENTbr*TIMEbr, weights = varFunc(~wbr), random = ~1|SUBJECT, data = df.brc.sum)
shapiro.test(lme_modelbr$residuals[,2])
plot(df.brc.sum$Date,lme_modelbr$residuals[,2])

summary(lme_modelbr)
Anova(lme_modelbr, t=3)

#############################
#raw data:
#AvT.brr <- dat.brc[which(dat.brc$Temp_level=="AvT"),]
#BrownCon<-c(dat.brc$`Brown Concentration`)
#lme_AvT.brr <- lme(BrownCon~Precip_level*Temp_level*ExperimentDay, random = ~1|MicrocosmNumber,data = df.brc.sum)
#^error with Precip_level variable length. After solving this, will have same issue with ExperimentDay and MicrocosmNumber
#anova(lme_AvT.brr)
#shapiro.test(lme_AvT.brr$residuals[,2])
##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#### Data splitting by hand ####
ExT.brc <- df.brc.sum[which(df.brc.sum$Temp_level=="ExT"),]%>%as.data.frame()
AvT.brc <- df.brc.sum[which(df.brc.sum$Temp_level=="AvT"),]%>%as.data.frame()

##Extreme temperature
names(ExT.brc)[6] <- "VARbr"

#lme_ExT_br <- lme(VARbr~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = ExT.brc)
  #p-value = 4.082e-06
#lme_ExT_br <- lme(sqrt(VARbr)~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = ExT.brc)
  #p-value = 0.001913
#lme_ExT_br <- lme(sqrt(VARbr+.1)~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = ExT.brc)
  #p-value = 0.001972
#lme_ExT_br <- lme(log(VARbr+.1)~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = ExT.brc)
  #p-value = 3.366e-09

lme_ExT_br <- lme(1/(VARbr+.1)~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = ExT.brc)
  #p-value < 2.2e-16
shapiro.test(lme_ExT_br$residuals[,2])
plot(ExT.brc$ExperimentDay,lme_ExT_br$residuals[,2])

var <- aggregate(ExT.brc$VARbr, by=paste(ExT.brc$Precip_level, ExT.brc$ExperimentDay)%>%as.character%>%list, FUN=function(x){var(x)+1})
assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
ExT.brc$wg <- sapply(paste(ExT.brc$Precip_level, ExT.brc$ExperimentDay), assign_var)
lme_ExT_br <- lme(1/(VARbr+.1)~Precip_level*ExperimentDay, weights = varFunc(~wg), random = ~1|MicrocosmNumber, data = ExT.brc)

shapiro.test(lme_ExT_br$residuals[,2])
  #p-value < 2.2e-16

plot(lme_ExT_br$residuals[,2])
hist(lme_ExT_br$residuals[,2])

summary(lme_ExT_br)
Anova(lme_ExT_br, t=3)


##Average temperature
names(AvT.brc)[6] <- "VARbr"

#lme_AvT_br <- lme(VARbr~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = AvT.brc)
lme_AvT_br <- lme(sqrt(VARbr)~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = AvT.brc)
#lme_AvT_br <- lme(1/(VARbr+.1)~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = AvT.brc)
shapiro.test(lme_AvT_br$residuals[,2])
plot(AvT.brc$ExperimentDay,lme_AvT_br$residuals[,2])

var <- aggregate(AvT.brc$VARbr, by=paste(AvT.brc$Precip_level, AvT.brc$ExperimentDay)%>%as.character%>%list, FUN=function(x){var(x)+1})
assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
AvT.brc$wg <- sapply(paste(AvT.brc$Precip_level, AvT.brc$ExperimentDay), assign_var)
lme_AvT_br <- lme(1/(VARbr+.1)~Precip_level*ExperimentDay, weights = varFunc(~wg), random = ~1|MicrocosmNumber, data = AvT.brc)

shapiro.test(lme_AvT_br$residuals[,2])

plot(lme_AvT_br$residuals[,2])
hist(lme_AvT_br$residuals[,2])

summary(lme_AvT_br)
Anova(lme_AvT_br, t=3)



#### Averaging values over significant factors (here: time (p=0.0660) for ExT, time and precip treatment for AvT) ####
ExT.brc <- df.brc.sum[which(df.brc.sum$Temp_level=="ExT"),]%>%as.data.frame()
AvT.brc <- df.brc.sum[which(df.brc.sum$Temp_level=="AvT"),]%>%as.data.frame()

brc.ExT.sum <- na.omit(ExT.brc)%>%group_by(Precip_level,ExperimentDay)%>%summarise(
  brc.mean = mean(`Brown Concentration`),
  brc.std.er=sd(`Brown Concentration`)/sqrt(length(`Brown Concentration`))*1.96
)%>%as.data.frame()
brc.ExT.sum$Treatments <- brc.ExT.sum$Precip_level

brc.AvT.sum <- na.omit(AvT.brc)%>%group_by(Precip_level, ExperimentDay)%>%summarise(
  brc.mean = mean(`Brown Concentration`),
  brc.std.er=sd(`Brown Concentration`)/sqrt(length(`Brown Concentration`))*1.96
)%>%as.data.frame()
brc.AvT.sum$Treatments <- brc.AvT.sum$Precip_level

brc.sum.plot <- rbind(brc.ExT.sum, brc.AvT.sum)
brc.sum.plot$Treatments <- factor(brc.sum.plot$Treatments)
#brc.sum.plot$Treatments <- factor(brc.sum.plot$Treatments, levels=c("AvP", "ExP"))
brc.sum.plot <- brc.sum.plot[,-2]

##re-parsing data
#Q example:
dat.PO4$treat <- NA
prior.PO4 <- dat.PO4[which(dat.PO4$date <= 7),]
prior.PO4$treat = ifelse(prior.PO4$P_treatment=="LMB","LMB","C")

post.PO4 <- dat.PO4[which(dat.PO4$date >= 7),]
post.PO4$treat[which(post.PO4$P_treatment=="LMB"&post.PO4$Temp_treatment=="HW")] <- "LMB HW"
post.PO4$treat[which(post.PO4$P_treatment=="LMB"&post.PO4$Temp_treatment=="T")] <- "LMB T"
post.PO4$treat[which(is.na(post.PO4$treat)==T)]<-"C"

#### Averaging values over new groups (named column "treat") ####
prior.PO4.sum <- na.omit(prior.PO4)%>%group_by(treat,date)%>%summarise(
  PO4.mean = mean(PO4),
  range.max=max(PO4),
  range.min=min(PO4)
)

post.PO4.sum <- na.omit(post.PO4)%>%group_by(treat,date)%>%summarise(
  PO4.mean = mean(PO4),
  range.max=max(PO4),
  range.min=min(PO4)
)

dat.PO4.sum <- rbind(prior.PO4.sum, post.PO4.sum)



#REX:
#df.brc.sum$Treatments<-NA
#prior.br<-df.brc.sum[which(df.brc$ExperimentDay <= 13),]
#brc.sum.plot<-df.brc.sum[which(df.brc.sum$Temp_level=="ExT"),]%>%as.data.frame()
#prior.PO4$treat = ifelse(prior.PO4$P_treatment=="LMB","LMB","C")
#prior.br$Treatments = ifelse(prior.br$Temp_level=="AvT","AvT","ExT")
#prior.br$Treatments<-df.brc.sum[which(df.brc.sum$Temp_level=="AvT","ExT"),]%>%as.data.frame()

#post.br<-df.brc.sum[which(df.brc.sum$ExperimentDay >= 13),]
#post.br$Treatments[which(df.brc.sum$Precip_level=="AvP"&df.brc.sum$Temp_level=="AvT")] <- "AvP AvT"
#post.br$Treatments[which(df.brc.sum$Precip_treatment=="AvP"&df.brc.sum$Temp_treatment=="ExT")] <- "AvP ExT"
#post.br$Treatments[which(df.brc.sum$Precip_treatment=="ExP"&df.brc.sum$Temp_treatment=="AvT")] <- "ExP AvT"
#post.br$Treatments[which(df.brc.sum$Precip_treatment=="ExP"&df.brc.sum$Temp_treatment=="ExT")] <- "ExP ExT"


#prior.br.sum<-na.omit(prior.br$Treatments)%>%group_by(Treatments,ExperimentDay)%>%summarise(
#  Brown.mean = mean(`Brown Concentration`),
#  range.max=max(`Brown Concentration`),
#  range.min=min(`Brown Concentration`)
#)

#post.br.sum<-na.omit(post.br$Treatments)%>%group_by(Treatments,ExperimentDay)%>%summarise(
#  Brown.mean = mean(`Brown Concentration`),
#  range.max=max(`Brown Concentration`),
#  range.min=min(`Brown Concentration`)
#)

#df.brc.sum<- rbind(prior.br.sum, post.br.sum)





#### Brown Visualization ####
treat<-(unique(brc.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch <- 1:length(treat)
lty <- c(1,1,1,1)
lwd <- c(3,3,3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(brc.sum.plot$ExperimentDay,brc.sum.plot$brc.mean,ylim = c(min(brc.sum.plot$brc.mean-brc.sum.plot$brc.std.er),max(brc.sum.plot$brc.mean+brc.sum.plot$brc.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Brown algae (ug  ",L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.brc.sum$ExperimentDay%>%unique(),labels = df.brc.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,2000,labels="Runoff event", col="black")

#from previous script
for (i in 1:length(treat)) {
  box.dat=brc.sum.plot[which(brc.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$brc.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$brc.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$brc.mean-box.dat$brc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$brc.mean+box.dat$brc.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("24C No Runoff","24C Runoff","18C No Runoff","18C Runoff"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 













##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# data transformation for normality: 1).square-root transform; 2). logarithm; 3). reciprocal tranformation: 1/values
#C$PO4 <- sqrt(C$PO4)# square-root transform
#lme_C <- lme(PO4~Temp_treatment*date, random = ~1|core_no,data = C)
#shapiro.test(lme_C$residuals[,2])
#summary(lme_C) 


#B <- df.PO4[which(df.PO4$P_treatment=="B"),]
#lme_B <- lme(PO4~Temp_treatment*date, random = ~1|core_no,data = B)
#shapiro.test(lme_B$residuals[,2])
#summary(lme_B)

#C_B <- df.PO4[which(df.PO4$P_treatment=="C"|df.PO4$P_treatment=="B"),]
#lme_C_B <- lme(PO4~P_treatment*Temp_treatment*date, random = ~1|core_no,data = C_B)
#shapiro.test(lme_C_B$residuals[,2])
#C_B$PO4 <- sqrt(C_B$PO4)
#lme_C_B <- lme(PO4~P_treatment*Temp_treatment*date, random = ~1|core_no,data = C_B)
#shapiro.test(lme_C_B$residuals[,2])
#summary(lme_C_B)
#anova(lme_C_B)

#LMB <- df.PO4[which(df.PO4$P_treatment=="LMB"),]
#lme_LMB <- lme(PO4~Temp_treatment*date, random = ~1|core_no,data = LMB)
#shapiro.test(lme_LMB$residuals[,2])
#plot(lme_LMB$residuals[,2])
#LMB <- LMB[which(LMB$date>=7),]
#lme_LMB <- lme(PO4~Temp_treatment*date, random = ~1|core_no,data = LMB)
#shapiro.test(lme_LMB$residuals[,2])
#summary(lme_LMB)
#anova(lme_LMB)
#lme_LMB$coefficients




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#### Visualization of significant groups ####

#dat.PO4$treat <- NA
#prior.PO4 <- dat.PO4[which(dat.PO4$date <= 7),]
#prior.PO4$treat = ifelse(prior.PO4$P_treatment=="LMB","LMB","C")

#post.PO4 <- dat.PO4[which(dat.PO4$date >= 7),]
#post.PO4$treat[which(post.PO4$P_treatment=="LMB"&post.PO4$Temp_treatment=="HW")] <- "LMB HW"
#post.PO4$treat[which(post.PO4$P_treatment=="LMB"&post.PO4$Temp_treatment=="T")] <- "LMB T"
#post.PO4$treat[which(is.na(post.PO4$treat)==T)]<-"C"


#### Averaging values over new groups (named column "treat") ####
#prior.PO4.sum <- na.omit(prior.PO4)%>%group_by(treat,date)%>%summarise(
#  PO4.mean = mean(PO4),
#  range.max=max(PO4),
#  range.min=min(PO4)
#)

#post.PO4.sum <- na.omit(post.PO4)%>%group_by(treat,date)%>%summarise(
#  PO4.mean = mean(PO4),
#  range.max=max(PO4),
#  range.min=min(PO4)
#)

#dat.PO4.sum <- rbind(prior.PO4.sum, post.PO4.sum)

#treat<-unique(dat.PO4.sum$treat)
#col <- 1:length(treat)
#pch <- 1:length(treat)
#lty <- 1:length(treat)
#lwd <- 1:length(treat)

#par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1.5)
#par("usr")

#plot(dat.PO4.sum$date,dat.PO4.sum$PO4.mean,ylim = c(min(dat.PO4.sum$range.min),max(dat.PO4.sum$range.max)),type="n",xlab="",ylab = "",xaxt="n",las=1)
#mtext(expression(paste(PO[4]^{"2-"}," (g ", m^{-3},")",sep =" ")),side = 2,line = 3,cex=2)
#mtext("Time (d)",side = 1,line = 2.5,cex=2)
#axis(side = 1,at=dat.PO4.sum$date%>%unique(),labels = c("1","3","7","9","11","14","16","21"))




##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# RECTANGLE FOR POST-HEATWAVE
#rect(xleft = 7.1,xright = 15,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 1, green = 0.8, blue = 0.8, alpha =  0.5),border = NA)

#from previous script
#rect(xleft = ymd("2018-03-13"),xright = ymd("2018-03-23"),ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, 
#                                                                                                               blue = 1, alpha = 0.5),
#     border = NA)



##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ERROR BARS
#for (i in 1:length(treat)) {
#  box.dat=dat.PO4.sum[which(dat.PO4.sum$treat==treat[i]),]
#  lines(box.dat$date+(-length(treat)/2+i)*0.1,box.dat$PO4.mean,col=col[i],lty=lty[i],lwd=lwd[i])
#  points(box.dat$date+(-length(treat)/2+i)*0.1,box.dat$PO4.mean,col=col[i],pch=pch[i],cex=2)
#  arrows(box.dat$date+(-length(treat)/2+i)*0.1,box.dat$range.min,box.dat$date+(-length(treat)/2+i)*0.1,box.dat$range.max,code=3,angle = 90,length=.05,col=col[i])
#}

#from previous script
#for (i in 1:length(treat)) {
#  box.dat=BrownConc.dat.sum[which(BrownConc.dat.sum$Treat==treat[i]),]
#  lines(box.dat$Date+(-length(treat)/2+i)*0.1,box.dat$BrownConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
#  points(box.dat$Date+(-length(treat)/2+i)*0.1,box.dat$BrownConc.dat.mean,col=col[i],pch=pch[i],cex=1)
#  arrows(box.dat$Date+(-length(treat)/2+i)*0.1,box.dat$range.min,box.dat$Date+(-length(treat)/2+i)*0.1,
#         box.dat$range.max,code=3,angle = 90,length=.05,col=col[i])
#}



##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# BACI LABEL
# mtext(text="Before",at=mean(c(1,8)),side = 3,line = 0,cex = 2)
# mtext(text="Heatwave",at=mean(c(8,15)),side = 3,line = 0,cex = 2)
# mtext(text="After",at=mean(c(15,21)),side = 3,line = 0,cex = 2)

#from pervious scripts:
#mtext(text="Before Runoff",at=mean(c(ymd("2018-02-28"),ymd("2018-03-12"))),side = 3,line = 0,cex = 1.25)
#mtext(text="After Runoff",at=mean(c(ymd("2018-03-14"),ymd("2018-03-23"))),side = 3,line = 0,cex = 1.25)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# LEGEND
# legend("topleft", legend = c("No phoslock","Phoslock, first phase 20Â°C","Phoslock, 30Â°C","Phoslock, 20Â°C"), col = col,lty=lty,pch = pch,text.col =col,cex = 0.5,lwd = lwd,bty="n")

#from previous script
#legend("bottomleft", legend = c("18C Normal","24C Normal","18C Extreme","24C Extreme"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
#       text.col =col,cex = 1,lwd = c(3,3,3,3)) 
