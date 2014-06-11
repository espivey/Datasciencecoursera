rm(list=ls(all=TRUE))
gc()

source('C:/Users/Ed/SkyDrive/Meter Analytics/Data_Dump_Import.R')
library(lubridate)
library(plyr)



#The purpose of this function is to calculate when the average start up and shutdown of a facility occurs

# We still need to use an algorithm to determine if Saturday or Sunday has occupancy.


dir<-file.choose()
data<-Data_Import(dir)
# str(data)
# head(data)

data<-na.omit(data)
data$Timestamp<-as.POSIXct(data$Timestamp, origin ='1970/01/01')
data$DOW<-weekdays(data$Timestamp)

### Generates Column 'data$Weekday' identifying Weekday, Saturday and Sunday as entries
weekdays<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
data$Weekday[which(data$DOW%in%weekdays)]<-'Weekday'
data$Weekday[which(data$DOW=='Saturday')]<-'Saturday'
data$Weekday[which(data$DOW=='Sunday')]<-'Sunday'
data$Hour<-hour(data$Timestamp)
data$DOY <- as.numeric(format(data$Timestamp, "%j"))+hour/24


data$DOY <- as.numeric(data$Timestamp)/(3600*24) - floor(as.numeric(data$Timestamp[1])/(3600*24))


### Generates Column 'data$Time_Period' identifying AM and PM
data$Time_Period[which(data$Hour<12)]<-'AM'
data$Time_Period[which(data$Hour>=12)]<-'PM'

mean.demand<-ddply(data,.(Site,Weekday,Time_Period,Hour),summarize,Mean_Demand=mean(Demand),Max_Demand=max(Demand))

lf<-ddply(mean.demand,.(Site,Weekday,Time_Period,Hour),summarize,LF=Mean_Demand/Max_Demand)
mean.demand.lf<-join(mean.demand,lf,by=c('Site','Weekday','Time_Period','Hour'))

mean.demand.lf$Lag.LF<-c(NA,abs(diff(mean.demand.lf$LF,lag=1)))
mean.demand.lf<-na.omit(mean.demand.lf)
max.diff.lf<-ddply(mean.demand.lf,.(Site,Weekday,Time_Period),summarize,Lag.LF=max(Lag.LF))

start.end.time<-join(max.diff.lf,mean.demand.lf,type='left',match='first')






## Reduce data sets to temperature and kW and DOY
kW_AM <- data$Demand[data$Time_Period=='AM']
kW_PM <- data$Demand[data$Time_Period=='PM']
Temp_AM <- data$Weather[data$Time_Period=='AM']
Temp_PM <- data$Weather[data$Time_Period=='PM']
DOY_AM <- data$DOY[data$Time_Period=='AM']
DOY_PM <- data$DOY[data$Time_Period=='PM']

## K-Means Cluster Analysis on kW
## AM Clusters
FitAM <- kmeans(kW_AM, 5, iter.max = 25, nstart = 25) # 5 cluster solution
data_clus_AM <- data.frame(DOY_AM, kW_AM, Temp_AM, FitAM$cluster) # append cluster assignment
kW_clus_AM <- aggregate(kW_AM, by=list(FitAM$cluster), FUN=mean)    ## Shows centers of clustered values

## PM Clusters
FitPM <- kmeans(kW_PM, 5, iter.max = 25, nstart = 25) # 5 cluster solution
data_clus_PM <- data.frame(DOY_PM, kW_PM, Temp_PM, FitPM$cluster) # append cluster assignment
kW_clus_PM <- aggregate(kW_PM, by=list(FitPM$cluster), FUN=mean)    ## Shows centers of clustered values

##  Reorder clusters on kW Values - AM (low to high - 1 to 5)
AM5 <- which.max(kW_clus_AM$x)
AM1 <- which.min(kW_clus_AM$x)
kW_clus_AMless2 <- kW_clus_AM[c(-AM5,-AM1), ]
AM4 <- kW_clus_AMless2$Group.1[which.max(kW_clus_AMless2$x)]
AM2 <- kW_clus_AMless2$Group.1[which.min(kW_clus_AMless2$x)]
AM3 <- 15 - AM1 - AM2 - AM4 - AM5

data_clus_AM$Type <- NA
data_clus_AM$Type[data_clus_AM$FitAM.cluster == AM5] <- 5
data_clus_AM$Type[data_clus_AM$FitAM.cluster == AM4] <- 4
data_clus_AM$Type[data_clus_AM$FitAM.cluster == AM3] <- 3
data_clus_AM$Type[data_clus_AM$FitAM.cluster == AM2] <- 2
data_clus_AM$Type[data_clus_AM$FitAM.cluster == AM1] <- 1

data_clus_AM <- data_clus_AM[, -4]  ## remove old cluster numbers now that replaced with sequential numbers
colnames(data_clus_AM)<-c("DOY_Frac","kW","Temp_F","Clust#")

##  Reorder clusters on Power Values - PM  (low to high - 1 to 5)
PM5 <- which.max(kW_clus_PM$x)
PM1 <- which.min(kW_clus_PM$x)
kW_clus_PMless2 <- kW_clus_PM[c(-PM5,-PM1), ]
PM4 <- kW_clus_PMless2$Group.1[which.max(kW_clus_PMless2$x)]
PM2 <- kW_clus_PMless2$Group.1[which.min(kW_clus_PMless2$x)]
PM3 <- 15 - PM1 - PM2 - PM4 - PM5

data_clus_PM$Type <- NA
data_clus_PM$Type[data_clus_PM$FitPM.cluster == PM5] <- 10
data_clus_PM$Type[data_clus_PM$FitPM.cluster == PM4] <- 9
data_clus_PM$Type[data_clus_PM$FitPM.cluster == PM3] <- 8
data_clus_PM$Type[data_clus_PM$FitPM.cluster == PM2] <- 7
data_clus_PM$Type[data_clus_PM$FitPM.cluster == PM1] <- 6

data_clus_PM <- data_clus_PM[, -4]  ## remove old cluster numbers now that replaced with sequential numbers
colnames(data_clus_PM)<-c("DOY_Frac","kW","Temp_F","Clust#")
# head(data_clus_AM)
# head(data_clus_PM)
data_clus <- rbind(data_clus_AM,data_clus_PM)
data_clus <- data_clus[order(data_clus$DOY_Frac),]
# head(data_clus,199)

### Now seperate each 'AM' and 'PM' periods into their seperate clusters, 5 clusters each period
AM1clust <- data_clus_AM[data_clus_AM$PMType==1, ]
AM2clust <- data_clus_AM[data_clus_AM$PMType==2, ]
AM3clust <- data_clus_AM[data_clus_AM$PMType==3, ]
AM4clust <- data_clus_AM[data_clus_AM$PMType==4, ]
AM5clust <- data_clus_AM[data_clus_AM$PMType==5, ]
PM1clust <- data_clus_PM[data_clus_PM$PMType==6, ]
PM2clust <- data_clus_PM[data_clus_PM$PMType==7, ]
PM3clust <- data_clus_PM[data_clus_PM$PMType==8, ]
PM4clust <- data_clus_PM[data_clus_PM$PMType==9, ]
PM5clust <- data_clus_PM[data_clus_PM$PMType==10, ]
# head(PM5clust)

##  Lets look at the Data
Start <- 160
End <- Start + 8  
par(mfrow=c(1,1), mar=c(4,4,1,1), oma=c(.5,.5,.5,.5))
par(xaxs="r", yaxs="r")
plot(data_clus$DOY_Frac[data_clus$DOY_Frac >= Start & data_clus$DOY_Frac < End], data_clus$kW[data_clus$DOY_Frac >= Start & data_clus$DOY_Frac < End], type='l', bty = 'n', xlab = 'Day of Year with Fraction', col='darkslategray',
     ylab = 'kW', ylim=c(0,max(data_clus$kW)), cex.axis=.75, cex.lab=.75, xlim=c(Start,End))
points(AM1clust$DOY_AM[AM1clust$DOY_AM >= Start & AM1clust$DOY_AM < End], AM1clust$kW_AM[AM1clust$DOY_AM >= Start & AM1clust$DOY_AM < End], pch=20, col='dodgerblue1')
points(AM2clust$DOY_AM[AM2clust$DOY_AM >= Start & AM2clust$DOY_AM < End], AM2clust$kW_AM[AM2clust$DOY_AM >= Start & AM2clust$DOY_AM < End], pch=20, col='darkgoldenrod1')
points(AM3clust$DOY_AM[AM3clust$DOY_AM >= Start & AM3clust$DOY_AM < End], AM3clust$kW_AM[AM3clust$DOY_AM >= Start & AM3clust$DOY_AM < End], pch=20, col='darkolivegreen4')
points(AM4clust$DOY_AM[AM4clust$DOY_AM >= Start & AM4clust$DOY_AM < End], AM4clust$kW_AM[AM4clust$DOY_AM >= Start & AM4clust$DOY_AM < End], pch=20, col='darkorange3')
points(AM5clust$DOY_AM[AM5clust$DOY_AM >= Start & AM5clust$DOY_AM < End], AM5clust$kW_AM[AM5clust$DOY_AM >= Start & AM5clust$DOY_AM < End], pch=20, col='firebrick1')
points(PM1clust$DOY_PM[PM1clust$DOY_PM >= Start & PM1clust$DOY_PM < End], PM1clust$kW_PM[PM1clust$DOY_PM >= Start & PM1clust$DOY_PM < End], pch=20, col='dodgerblue3')
points(PM2clust$DOY_PM[PM2clust$DOY_PM >= Start & PM2clust$DOY_PM < End], PM2clust$kW_PM[PM2clust$DOY_PM >= Start & PM2clust$DOY_PM < End], pch=20, col='darkgoldenrod3')
points(PM3clust$DOY_PM[PM3clust$DOY_PM >= Start & PM3clust$DOY_PM < End], PM3clust$kW_PM[PM3clust$DOY_PM >= Start & PM3clust$DOY_PM < End], pch=20, col='darkolivegreen')
points(PM4clust$DOY_PM[PM4clust$DOY_PM >= Start & PM4clust$DOY_PM < End], PM4clust$kW_PM[PM4clust$DOY_PM >= Start & PM4clust$DOY_PM < End], pch=20, col='darkorange4')
points(PM5clust$DOY_PM[PM5clust$DOY_PM >= Start & PM5clust$DOY_PM < End], PM5clust$kW_PM[PM5clust$DOY_PM >= Start & PM5clust$DOY_PM < End], pch=20, col='firebrick4')
abline(h=1320)
abline(h=1311, col="red")







