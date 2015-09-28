




library(stringr)
library(lubridate)
library(ggplot2)
library(plyr)


raw= read.csv("out.txt")
raw$V1 <- as.character(raw$X.200)
length(grep("StartTime", raw$V1,))  
## number of trips 403!


trips <- data.frame(cbind(raw[grep("StartTime", raw$V1,),]$V1, raw[grep("EndTime", raw$V1,),]$V1) )

trips$start = str_trim(str_split_fixed(trips$X1, "=", 2)[,2])
trips$end = str_trim(str_split_fixed(trips$X2, "=", 2)[,2])

#as.POSIXlt
trips$startT <- as.POSIXct(trips$start, format="%Y-%m-%d %H:%M:%S", tz = "CET")
trips$endT <- as.POSIXct(trips$end, format="%Y-%m-%d %H:%M:%S")

#trips$startT <-ymd_hms(trips$start)
#trips$endT <-ymd_hms(trips$end)


## summertime!
#interv <- as.interval(as.Date("30.03.2014", "%d.%m.%Y"), as.Date("26.10.2014", "%d.%m.%Y"))
daylight <- as.interval(ymd_hm("2014-03-30 00:03"), ymd_hm("2014-10-26 24:23"))

#trips$daylight <- ifelse(trips$startT %within% daylight, 1,0)

trips[trips$startT %within% daylight,]$startT <- trips[trips$startT %within% daylight,]$startT +60*60
trips[trips$startT %within% daylight,]$endT <- trips[trips$startT %within% daylight,]$endT +60*60


trips$driveTime <- (trips$endT - trips$startT )/ 60 

trips$mon<- format(trips$startT, "%m %b")
trips$wday <- format(trips$startT, "%w %a")
trips$hour <- hour(trips$startT)
## morning trips!
table(trips$hour)
trips$morning <- ifelse(trips$hour %in% c(7,8),1,0)
trips$evening <- ifelse(trips$hour %in% c(15,16,17,18,19,20),1,0)

### working time!?


## remove
max(trips$driveTime)  #Time difference of 9571.433 mins

trips <- trips[trips$driveTime <60,]
trips$dt <- as.numeric(trips$driveTime)

#ddply(trips, .(mon), summarise, N=length(mon), durchschnit =mean(driveTime))


ggplot(trips, aes(mon, dt, group=mon )) + geom_boxplot()

ggplot(trips, aes(wday, dt, group=wday )) + geom_boxplot()

ggplot(trips, aes(timeZ, dt  )) + geom_boxplot()
#ggplot(trips, aes(startT, dt, group=wday, color=wday)) +geom_smooth()

summary(lm(dt~mon+wday+ timeZ, data=trips))

summary(lm(dt~timeZ, data=trips))
## morning trips!
ggplot(trips[trips$morning==1,], aes(wday, dt, group=wday )) + geom_boxplot()


trips[trips$morning==1,]$startT

trips$startTH <- strftime(trips$startT, format="%H:%M")

### guten morgen frankfurt
ggplot(trips[trips$morning==1,], aes(startTH)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

### abends!
ggplot(trips[trips$evening==1,], aes(startTH)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(trips[trips$evening==1,], aes(hour)) + geom_bar() 

## 
summary(trips[trips$evening==1,]$dt)
summary(trips[trips$morning==1,]$dt)

trips$timeZ <- "rest"
trips$timeZ <-ifelse(trips$evening==1, "evening", trips$timeZ)
trips$timeZ <-ifelse(trips$morning==1, "morning", trips$timeZ)

ggplot(trips, aes(x=dt, group=timeZ,fill=timeZ)) + geom_density(alpha=.3)

#### morgen!
ggplot(trips[trips$morning==1,], aes(x=startTH, group=wday,fill=wday)) + geom_density(alpha=.3) +theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(trips[trips$morning==1,], aes(x=dt, group=wday,fill=wday)) + geom_density(alpha=.3) +theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(trips[trips$morning==1,], aes(x=dt, group=mon,fill=mon)) + geom_density(alpha=.3) +theme(axis.text.x = element_text(angle = 90, hjust = 1))

## abend
ggplot(trips[trips$evening==1,], aes(x=startTH, group=wday,fill=wday)) + geom_density(alpha=.3) +theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(trips[trips$evening==1,], aes(x=dt, group=wday,fill=wday)) + geom_density(alpha=.3) +theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(trips[trips$evening==1,], aes(x=dt, group=mon,fill=mon)) + geom_density(alpha=.3) +theme(axis.text.x = element_text(angle = 90, hjust = 1))

### km gefahren?!
#4.9 km pro strecke!
nrow(trips[trips$evening==1 || trips$morning==1,])
## 402 trips
402*4.9
### knapp 2k KM (1969.8)
## durchschnittsgeschwindigkeit.

4.9 / 14.4*60
## 20.41 km/h


#### working time!?

## same date .. evening and morning!
trips$date <- as.Date(trips$startT)

working <- trips[trips$morning ==1,]

working <- merge(working, trips[trips$evening ==1,], by=c("date"))

working <- working[,c("startT.x", "endT.x", "dt.x", "startT.y", "endT.y", "dt.y")]

working$wt <- working$startT.y -working$endT.x
working$wt <- as.numeric(working$wt) ## in hours
working$mon<- format(working$startT.x, "%m %b")
working$wday <- format(working$startT.x, "%w %a")

##
ggplot(working, aes(x=wt, group=1,fill=1)) + geom_density(alpha=.3)

## per weekday!
ggplot(working, aes(x=wt, group=wday, fill=wday)) + geom_density(alpha=.3)

ggplot(working, aes(x=wt, group=mon, fill=mon)) + geom_density(alpha=.3)

ggplot(working, aes(y=wt, x=mon)) +geom_boxplot()
ggplot(working, aes(y=wt, x=wday)) +geom_boxplot()

##nice!
summary(lm(wt~wday+mon+dt.x+dt.y, data=working))
  
lm <- lm(wt~wday+mon+dt.x+dt.y, data=working)
stargazer(lm,  type = "html", title="Explain time spend at work.")


### weather API 
#c6c52898b8e323c5
#http://api.wunderground.com/api/Your_Key/history_YYYYMMDD/q/CA/San_Francisco.json
#http://api.wunderground.com/api/c6c52898b8e323c5/history_20140115/q/Germany/Frankfurt.json



