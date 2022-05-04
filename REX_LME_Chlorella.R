###########################################
#### Green Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")
library(dplyr)
GreenConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real",
                                  "Green Concentration","ExperimentDay")]
#GreenConc.dat$Date <- ymd(GreenConc.dat$Date)

#Calculating the error bar by range; time series with range
GreenConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                              levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
GreenConc.dat.sum <- na.omit(GreenConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  GreenConc.dat.mean = mean(`Green Concentration`),
  GreenConc.std.er=sd(`Green Concentration`)/sqrt(length(`Green Concentration`))*1.96
)%>%as.data.frame()

treat<-unique(GreenConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
##Plotting the data with error bars
plot(GreenConc.dat.sum$ExperimentDay,GreenConc.dat.sum$GreenConc.dat.mean,ylim = c(min(GreenConc.dat.sum$GreenConc.dat.mean-GreenConc.dat.sum$GreenConc.std.er),2335.143),
     type="n",xlab="",ylab = "",xaxt="n",las=1)

#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Chlorella vulgaris (ug/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=GreenConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=GreenConc.dat.sum[which(GreenConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$GreenConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$GreenConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$GreenConc.dat.mean-box.dat$GreenConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$GreenConc.dat.mean+box.dat$GreenConc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)

#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topleft", legend = c("18캜 & No Runoff","24캜 & No Runoff","18캜 & Runoff","24캜 & Runoff"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
       text.col =col,cex = 1,lwd = c(3,3,3,3)) 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
























###########################################
#### REX adaptation of Qing LME script ####
###########################################


##run-through with brown concentration
install.packages("nlme")


dat.gc <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Date","ExperimentDay","Green Concentration")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

#df.gc$Precip_treatment[which(df.gc$ExperimentDay<=14)]<-"AvP" # heatwave simulation after the sampling on 2018-07-02

df.gc.sum<-na.omit(dat.gc)%>%as.data.frame()
names(df.gc.sum)[7] <- "G_Conc"
df.gc.sum$MicrocosmNumber <- as.factor(df.gc.sum$MicrocosmNumber)
df.gc.sum$Temp_level <- factor(df.gc.sum$Temp_level, levels = c("AvT", "ExT"))
df.gc.sum$Precip_level <- factor(df.gc.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.gc.sum$G_Conc)

require(nlme)
require(car)

lme_modelg <- lme(G_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.gc.sum)


# normality test on the residuals
plot(lme_modelg$residuals[,2])
hist(lme_modelg$residuals[,2])
shapiro.test(lme_modelg$residuals[,2])
#p-value = 4.97e-11



# Initial knowlege by the pilot lme model
anova(lme_modelg)
summary(lme_modelg)



# Splitting data by period (before and after run-off)
Before.g <- df.gc.sum[which(df.gc.sum$ExperimentDay<14),]%>%as.data.frame()
#AvT.gc <- df.gc.sum[which(df.gc.sum$Temp_level=="AvT"),]%>%as.data.frame()

# LME for separate periods
var <- aggregate(Before.g$G_Conc, by=paste(Before.g$ExperimentDay, Before.g$Temp_level)%>%as.character%>%list, FUN=var)
assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
Before.g$wg <- sapply(paste(Before.g$ExperimentDay, Before.g$Temp_level), assign_var)

lme_Before.g <- lme(G_Conc~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, weights = varFunc(~wg), data = Before.g)
#normal:p-value = 0.02026
#sqrt: p-value = 4.741e-06
#log: p-value = 1.122e-09
#recip.: p-value = 8.886e-13
plot(lme_Before.g$residuals[,2])
shapiro.test(lme_Before.g$residuals[,2])
anova(lme_Before.g)
#temp, time significant
summary(lme_Before.g)


#After runoff event
After.g <- df.gc.sum[which(df.gc.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
var <- aggregate(After.g$G_Conc, by=paste(After.g$ExperimentDay, After.g$Temp_level, After.g$Precip_level)%>%as.character%>%list, FUN=var)
assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
After.g$wg <- sapply(paste(After.g$ExperimentDay, After.g$Temp_level, After.g$Precip_level), assign_var)

lme_After.g <- lme(G_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.g)

plot(lme_After.g$residuals[,2])
shapiro.test(lme_After.g$residuals[,2])
#normal: p-value = 3.725e-08
#sqrt: p-value = 1.857e-07
#log: p-value = 2.187e-06
#recip.: p-value = 4.576e-07
anova(lme_After.g)
#temp, time significant
summary(lme_After.g)
#no significance





























































# check raw data treatment for normality
##use the raw data from df.gc or use the as.factor altered data (df.bc.sum)?????
#############################
#df.gc.sum as.factor altered data:
plot(VARg)
AvT.g <- df.gc.sum[which(df.gc.sum$Temp_level=="AvT"),]
GreenCon<-c(df.gc.sum$`Green Concentration`)
lme_AvT.g <- lme(VARg~TIME*Precip_TREATMENTg, random = ~1|SUBJECT,data = df.gc.sum)
anova(lme_AvT.g)
summary(lme_AvT.g)
shapiro.test(lme_AvT.g$residuals[,2])
#p-value = 4.414e-06

plot(lme_AvT.g$residuals[,2], col=df.gc.sum$Temp_level%>%as.factor())
#p-value = 9.863e-09. Not larger than 0.05, so requires transformation for normality

require(dplyr)

lme_model <- lme(VARg~Precip_TREATMENTg*Temp_TREATMENTg*TIME,random = ~1|SUBJECT, data = df.gc.sum)
plot(df.gc.sum$ExperimentDay,lme_model$residuals[,2])
bptest(lme_model$residuals[,2]~df.gc.sum$ExperimentDay)

var <- aggregate(df.gc.sum$`Green Concentration`, by=paste(df.gc.sum$Precip_level, df.gc.sum$ExperimentDay, df.gc.sum$Temp_level)%>%as.character%>%list, FUN=var)

assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
df.gc.sum$wg <- sapply(paste(df.gc.sum$Precip_level, df.gc.sum$ExperimentDay, df.gc.sum$Temp_level), assign_var)

lme_model <- lme(VARg~Precip_TREATMENTg*Temp_TREATMENTg*TIME, weights = varFunc(~wg), random = ~1|SUBJECT, data = df.gc.sum)
shapiro.test(lme_model$residuals[,2])
#p-value = 4.97e-11
plot(df.gc.sum$Date,lme_model$residuals[,2])

summary(lme_model)
Anova(lme_model, t=3)

#### Data splitting by hand ####

ExT.gc <- df.gc.sum[which(df.gc.sum$Temp_level=="ExT"),]%>%as.data.frame()
AvT.gc <- df.gc.sum[which(df.gc.sum$Temp_level=="AvT"),]%>%as.data.frame()

# for extreme temperature
names(ExT.gc)[6] <- "VARg"
lme_ExT_g <- lme(VARg~ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = ExT.gc)
shapiro.test(lme_ExT_g$residuals[,2])
#normal: 2.208e-13
#sqrt: 1.452e-09
#log: 5.42e-05
#recip.: 0.03543
plot(ExT.gc$ExperimentDay,lme_ExT_g$residuals[,2])
summary(lme_ExT_g)

lme_ExT_bptest <- lme(1/(VARg+.1)~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = ExT.gc)
plot(ExT.gc$ExperimentDay, lme_ExT_bptest$residuals[,2])
bptest(lme_ExT_bptest$residuals[,2]~ExT.gc$Precip_level*ExT.gc$ExperimentDay)

var <- aggregate(ExT.gc$VARg, by=paste(ExT.gc$Precip_level, ExT.gc$ExperimentDay)%>%as.character%>%list, FUN=function(x){var(x)+1})

assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
ExT.gc$wg <- sapply(paste(ExT.gc$Precip_level,ExT.gc$ExperimentDay), assign_var)

lme_ExT_g <- lme(1/(VARg+.1)~Precip_level*ExperimentDay , weights = varFunc(~wg), random = ~1|MicrocosmNumber, data = ExT.gc)

shapiro.test(lme_ExT_g$residuals[,2])
#weight log: p-value = 0.07382

plot(lme_ExT_g$residuals[,2])
hist(lme_ExT_g$residuals[,2])

summary(lme_ExT_g)
Anova(lme_ExT_g, t=3)

#### Averaging values over significant factors (here: time for ExT, time and Precipitation treatments for AvT) ####

ExT.gc <- df.gc.sum[which(df.gc.sum$Temp_level=="ExT"),]%>%as.data.frame()
AvT.gc <- df.gc.sum[which(df.gc.sum$Temp_level=="AvT"),]%>%as.data.frame()


gc.ExT.sum <- na.omit(ExT.gc)%>%group_by(ExperimentDay)%>%summarise(
  gc.mean = mean(`Green Concentration`),
  gc.std.er=sd(`Green Concentration`)/sqrt(length(`Green Concentration`))*1.96
)%>%as.data.frame()
gc.ExT.sum$Treatments <- "No_P_effect"

gc.AvT.sum <- na.omit(AvT.gc)%>%group_by(Precip_level, ExperimentDay)%>%summarise(
  gc.mean = mean(`Green Concentration`),
  gc.std.er=sd(`Green Concentration`)/sqrt(length(`Green Concentration`))*1.96
)%>%as.data.frame()
gc.AvT.sum$Treatments <- gc.AvT.sum$Precip_level
gc.AvT.sum <- gc.AvT.sum[,-1]

gc.sum.plot <- rbind(gc.ExT.sum, gc.AvT.sum)
gc.sum.plot$Treatments <- factor(gc.sum.plot$Treatments, levels=c("No_P_effect","AvP", "ExP"))

treat<-(unique(gc.sum.plot$Treatments))
#col <- 1:length(treat)
col <- c("orangered1", "deepskyblue", "blue3")
pch <- 1:length(treat)
lty <- c(1,1,1)
lwd <- c(3,3,3)

par(mfrow=c(1,1),mar=c(1,5,0,3),omd=(c(0.1,0.95,0.25,0.9)),xaxs="r",yaxs="r",cex=1)
par("usr")

plot(gc.sum.plot$ExperimentDay,gc.sum.plot$gc.mean,ylim = c(min(gc.sum.plot$gc.mean-gc.sum.plot$gc.std.er),max(2000)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Green algae (ug  ",L^{-1},")",sep =" ")),side = 2,line = 3,cex=1)
mtext("Time (d)",side = 1,line = 2.5,cex=1)
axis(side = 1,at=df.gc.sum$ExperimentDay%>%unique(),labels = df.gc.sum$ExperimentDay%>%unique())

abline(v=13, lty=2, col="black")
text(13,2000,labels="Runoff event", col="black", )

#from previous script
for (i in 1:length(treat)) {
  box.dat=gc.sum.plot[which(gc.sum.plot$Treatments==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$gc.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,box.dat$gc.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,(box.dat$gc.mean-box.dat$gc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.3,
         (box.dat$gc.mean+box.dat$gc.std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("24C","18C No Runoff","18C Runoff"), col = col,lty=lty,pch = pch,
       text.col =col,cex = 1,lwd = 2, bty = "n") 

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# BACI LABEL
#mtext(text="Before",at=mean(c(1,8)),side = 3,line = 0,cex = 2)
#mtext(text="Heatwave",at=mean(c(8,15)),side = 3,line = 0,cex = 2)
#mtext(text="After",at=mean(c(15,21)),side = 3,line = 0,cex = 2)

#from pervious scripts:
#mtext(text="Before Runoff",at=mean(c(ymd("2018-02-28"),ymd("2018-03-12"))),side = 3,line = 0,cex = 1.25)
#mtext(text="After Runoff",at=mean(c(ymd("2018-03-14"),ymd("2018-03-23"))),side = 3,line = 0,cex = 1.25)


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# LEGEND
#legend("topleft", legend = c("No phoslock","Phoslock, first phase 20째C","Phoslock, 30째C","Phoslock, 20째C"), col = col,lty=lty,pch = pch,text.col =col,cex = 0.5,lwd = lwd,bty="n")

#from previous script
#legend("bottomleft", legend = c("18C Normal","24C Normal","18C Extreme","24C Extreme"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
#       text.col =col,cex = 1,lwd = c(3,3,3,3)) 




# for average temperature
#names(AvT.gc)[6] <- "VARg"
#lme_AvT_g <- lme(1/(VARg+.1)~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber, data = AvT.gc)
#shapiro.test(lme_AvT_g$residuals[,2])
#plot(AvT.gc$ExperimentDay,lme_AvT_g$residuals[,2])

#var <- aggregate(AvT.gc$VARg, by=paste(AvT.gc$Precip_level, AvT.gc$ExperimentDay)%>%as.character%>%list, FUN=function(x){var(x)+1})
#assign_var <- function(x){1/(var$x[which(var$Group.1==x)])}
#AvT.gc$wg <- sapply(paste(AvT.gc$Precip_level, AvT.gc$ExperimentDay), assign_var)
#lme_AvT_g <- lme(1/(VARg+.1)~Precip_level*ExperimentDay, weights = varFunc(~wg), random = ~1|MicrocosmNumber, data = AvT.gc)

#shapiro.test(lme_AvT_g$residuals[,2])

#plot(lme_AvT_g$residuals[,2])
#hist(lme_AvT_g$residuals[,2])

#summary(lme_AvT_g)
#Anova(lme_AvT_g, t=3)


#############################
#raw data:
#AvT.gr <- dat.gc[which(dat.gc$Temp_level=="AvT"),]
#GreenCon<-c(dat.gc$`Green Concentration`)
#lme_AvT.gr <- sqrt(VARg~Precip_level*ExperimentDay, random = ~1|MicrocosmNumber,data = AvT)
#^error with Precip_level variable length. After solving this, will have same issue with ExperimentDay and MicrocosmNumber
#anova(lme_AvT.gr)
#shapiro.test(lme_AvT.gr$residuals[,2])



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
#  box.dat=GreenConc.dat.sum[which(GreenConc.dat.sum$Treat==treat[i]),]
#  lines(box.dat$Date+(-length(treat)/2+i)*0.1,box.dat$GreenConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
#  points(box.dat$Date+(-length(treat)/2+i)*0.1,box.dat$GreenConc.dat.mean,col=col[i],pch=pch[i],cex=1)
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
#  mtext(text="Before Runoff",at=mean(c(ymd("2018-02-28"),ymd("2018-03-12"))),side = 3,line = 0,cex = 1.25)
#  mtext(text="After Runoff",at=mean(c(ymd("2018-03-14"),ymd("2018-03-23"))),side = 3,line = 0,cex = 1.25)
  
  
  ##--break
  #~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  # LEGEND
 # legend("topleft", legend = c("No phoslock","Phoslock, first phase 20째C","Phoslock, 30째C","Phoslock, 20째C"), col = col,lty=lty,pch = pch,text.col =col,cex = 0.5,lwd = lwd,bty="n")
  
  #from previous script
 # legend("bottomleft", legend = c("18C Normal","24C Normal","18C Extreme","24C Extreme"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
#         text.col =col,cex = 1,lwd = c(3,3,3,3)) 
  