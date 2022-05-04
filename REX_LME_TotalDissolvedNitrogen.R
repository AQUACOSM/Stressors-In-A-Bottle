###########################################
#### Total Dissolved Nitrogen Raw Data Graph, Error Bars ####
###########################################

par(mfrow=c(1,1),mar=c(0,5,0,0),omd=(c(0.05,0.98,0.25,0.95)),xaxs="r",yaxs="r",cex=1)
par("usr")

DissNConc.dat <- rex_March2020[,c("MicrocosmNumber","Treatment","Temp_level","Precip_level","Precip_level_real","DissN (mg/L)","ExperimentDay")]
#DissNConc.dat$Date <- ymd(DissNConc.dat$Date)
#Calculating the error bar by range; time series with range
DissNConc.dat$Treat <- factor(paste(rex_March2020$Temp_level,rex_March2020$Precip_level_real,sep = " & "),
                            levels = c("AvT & AvP","ExT & AvP","AvT & ExP","ExT & ExP"))
DissNConc.dat.sum <- na.omit(DissNConc.dat)%>%group_by(Treat,ExperimentDay)%>%summarise(
  DissNConc.dat.mean = mean(`DissN (mg/L)`),
  DissNConc.std.er=sd(`DissN (mg/L)`)/sqrt(length(`DissN (mg/L)`))*1.96
)%>%as.data.frame()

treat<-unique(DissNConc.dat.sum$Treat)
col <- c("deepskyblue", "blue3", "orangered1", "darkred")
pch=c(19,19,15,15) 
lty=c(1,1,5,5) 
lwd <- c(2,2,2,2)

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
##Plotting the data with error bars
plot(DissNConc.dat.sum$ExperimentDay,DissNConc.dat.sum$DissNConc.dat.mean,ylim = c(min(DissNConc.dat.sum$DissNConc.dat.mean-DissNConc.dat.sum$DissNConc.std.er),max(DissNConc.dat.sum$DissNConc.dat.mean+DissNConc.dat.sum$DissNConc.std.er)),
     type="n",xlab="",ylab = "",xaxt="n",las=1)
#Add rectangle during heatwave
rect(xleft = 13,xright = 23,ybottom = par("usr")[3],ytop = par("usr")[4],col=rgb(red = 0.7, green = 0.8, blue = 1, alpha = 0.5),
     border = NA)

mtext(expression(paste("Total Dissolved Nitrogen (mg/L)",sep =" ")),side=2,line=3.5,cex=1)
axis(side = 1,at=DissNConc.dat.sum$ExperimentDay%>%unique(),labels = c(0,2,5,7,9,12,14,16,18,21,23))
mtext(expression(paste("Experiment Day",sep =" ")),side=1,line=3.5,cex=1)


for (i in 1:length(treat)) {
  box.dat=DissNConc.dat.sum[which(DissNConc.dat.sum$Treat==treat[i]),]
  lines(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$DissNConc.dat.mean,col=col[i],lty=lty[i],lwd=lwd[i])
  points(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,box.dat$DissNConc.dat.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,(box.dat$DissNConc.dat.mean-box.dat$DissNConc.std.er),box.dat$ExperimentDay+(-length(treat)/2+i)*0.1,
         (box.dat$DissNConc.dat.mean+box.dat$DissNConc.std.er),code=3,angle = 90,length=.05,col=col[i])
}
## ^the error bars are in the "arrows" line (arrows turned 90 degrees)

#add BACI label:
mtext(text="Before Runoff",at=mean(c(0,12)),side = 3,line = 0,cex = 1.25)
mtext(text="After Runoff",at=mean(c(14,23)),side = 3,line = 0,cex = 1.25)

legend("topright", legend = c("18°C & No Runoff","24°C & No Runoff","18°C & Runoff","24°C & Runoff"), col = col,lty=c(1,1,5,5),pch = c(19,19,15,15),
       text.col =col,cex = 1,lwd = c(3,3,3,3)) 


##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
###########################################
#### REX adaptation of Qing LME script ####
###########################################

#### LME RAW DATA ####
##run-through with NO3 (mg/L)
install.packages("nlme")


dat.DissN <- rex_March2020[,c("MicrocosmNumber","Temp_level","Precip_level","Date","ExperimentDay","DissN (mg/L)")]
#dat.bc$date <- dat.bc$Date %>% ymd %>% yday-dat.bc$Date[1]%>%ymd%>%yday+1

df.DissN.sum<-na.omit(dat.DissN)%>%as.data.frame()
names(df.DissN.sum)[6] <- "DissN_Conc"
df.DissN.sum$MicrocosmNumber <- as.factor(df.DissN.sum$MicrocosmNumber)
df.DissN.sum$Temp_level <- factor(df.DissN.sum$Temp_level, levels = c("AvT", "ExT"))
df.DissN.sum$Precip_level <- factor(df.DissN.sum$Precip_level, levels = c("AvP", "ExP"))

plot(df.DissN.sum$DissN_Conc)

require(nlme)
require(car)

lme_modelDissN <- lme(DissN_Conc~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = df.DissN.sum)

# normality test on the residuals
plot(lme_modelDissN$residuals[,2])
hist(lme_modelDissN$residuals[,2])
shapiro.test(lme_modelDissN$residuals[,2])
#p-value = 1.17e-15

##--break
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#### SPLIT DATA BY TIME ####


#BEFORE RUNOFF EVENT
Before.DissN <- df.DissN.sum[which(df.DissN.sum$ExperimentDay<14),]%>%as.data.frame()

#Normality of Before Runoff group
lme_Before.DissN <- lme(log(DissN_Conc)~Temp_level*ExperimentDay, random = ~1|MicrocosmNumber, data = Before.DissN)

shapiro.test(lme_Before.DissN$residuals[,2])
#normal: p-value = 4.719e-11
#sqrt: p-value = 1.72e-05
#log: p-value = 0.2817
#recip: p-value = 1.803e-06
plot(lme_Before.DissN$residuals[,2])
anova(lme_Before.DissN)
#experiment day significant
summary(lme_Before.DissN)


#AFTER RUNOFF EVENT
After.DissN <- df.DissN.sum[which(df.DissN.sum$ExperimentDay>=14),]%>%as.data.frame()

# LME for separete periods
lme_After.DissN <- lme(1/(DissN_Conc)~Temp_level*ExperimentDay*Precip_level, random = ~1|MicrocosmNumber, data = After.DissN)

shapiro.test(lme_After.DissN$residuals[,2])
#normal: p-value = 6.603e-06
#sqrt: p-value = 0.006261
#log: p-value = 0.04706
#recip: p-value = 0.8958
plot(lme_After.DissN$residuals[,2])
anova(lme_After.DissN)
#time, time*precip significant
summary(lme_After.DissN)
#time, time*precip significant












