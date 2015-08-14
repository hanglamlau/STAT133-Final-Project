##############import the data#############

setwd('../data')
storms <- read.csv("storms.csv",stringsAsFactors=F)
tracks <- read.csv("tracks.csv",stringsAsFactors=F)

storms <- storms[,-1]
tracks <- tracks[,-1]

storms$date <- as.Date(storms$date,'%m/%d/%Y')
tracks$date <- as.Date(tracks$date,'%m/%d/%Y')

storms <- storms[storms$date>=as.Date('01/01/1980','%m/%d/%Y') & storms$date<=as.Date('12/31/2010','%m/%d/%Y'),]

tracks <- tracks[tracks$date>=as.Date('01/01/1980','%m/%d/%Y') & tracks$date<=as.Date('12/31/2010','%m/%d/%Y'),]

head(storms)
tail(storms)

head(tracks)
tail(tracks)

######################analysis of per year###########
per_year <- format(storms$date,'%Y')

num_peryear <- aggregate(storms$id,list(per_year),length)
names(num_peryear) <- c('Year','Number of Storms')
head(num_peryear)
num_peryear####frequency table
barplot(num_peryear[[2]],as.numeric(num_peryear[[1]]),
        names.arg=num_peryear[[1]],cex.names = 0.8,
        main="Number of Storms Per Year")#####barplot of numers of storms per year




tracks_35 <- tracks[tracks$wind>=35,]
num_peryear35 <- aggregate(tracks_35$id,
                                  list(format(tracks_35$date,'%Y')),unique)
num_peryear35[,1] <- 1980:2010
num_peryear35[,2] <- as.numeric(sapply(num_peryear35[,2],length))
names(num_peryear35) <- c('Year',"Storms over 35")
num_peryear35####frequency table
barplot(num_peryear35[[2]],as.numeric(num_peryear[[1]]),
        names.arg=num_peryear35[[1]],cex.names = 0.8,
        main="Number of Storms Per Year with over 35 knots")#####barplot of numers of storms per year with >=35




tracks_64 <- tracks[tracks$wind>=64,]
num_peryear64 <- aggregate(tracks_64$id,
                           list(format(tracks_64$date,'%Y')),unique)
num_peryear64[,1] <- 1980:2010
num_peryear64[,2] <- as.numeric(sapply(num_peryear64[,2],length))
names(num_peryear64) <- c('Year',"Storms over 64")
num_peryear64######frequency table
barplot(num_peryear64[[2]],as.numeric(num_peryear[[1]]),
        names.arg=num_peryear64[[1]],cex.names = 0.8,
        main="Number of Storms Per Year with over 64 knots")#####barplot of numers of storms per year with >=64




tracks_96 <- tracks[tracks$wind>=96,]
num_peryear96 <- aggregate(tracks_96$id,
                           list(format(tracks_96$date,'%Y')),unique)
num_peryear96[,2] <- as.numeric(sapply(num_peryear96[,2],length))
names(num_peryear96) <- c('Year',"Storms over 96")
num_peryear96###frequency table
barplot(num_peryear96[[2]],as.numeric(num_peryear[[1]]),
        names.arg=num_peryear96[[1]],cex.names = 0.8,
        main="Number of Storms Per Year with over 96 knots")#####barplot of numers of storms per year with >=96

########################anaylysis per month##############
per_month <- format(storms$date,'%B')

num_permonth <- aggregate(storms$id,list(per_month),length)
names(num_permonth) <- c('Month','Number of Storms')
head(num_permonth)
num_permonth####frequency table
barplot(num_permonth[[2]],c(4,8,12,7,6,5,11,10,9),
        names.arg=num_permonth[[1]],cex.names = 0.8,
        main="Number of Storms Per month")#####barplot of numers of storms per month



tracks_35 <- tracks[tracks$wind>=35,]
num_permonth35 <- aggregate(tracks_35$id,
                           list(format(tracks_35$date,'%B')),unique)
num_permonth35[,2] <- as.numeric(sapply(num_permonth35[,2],length))
names(num_permonth35) <- c('Month',"Storms over 35")
num_permonth35####frequency table
barplot(num_permonth35[[2]],1:10,
        names.arg=num_permonth35[[1]],cex.names = 0.8,
        main="Number of Storms Per month with over 35 knots")#####barplot of numers of storms per month with >=35




tracks_64 <- tracks[tracks$wind>=64,]
num_permonth64 <- aggregate(tracks_64$id,
                           list(format(tracks_64$date,'%B')),unique)
num_permonth64[,2] <- as.numeric(sapply(num_permonth64[,2],length))
names(num_permonth64) <- c('Month',"Storms over 64")
num_permonth64######frequency table
barplot(num_permonth64[[2]],1:8,
        names.arg=num_permonth64[[1]],cex.names = 0.8,
        main="Number of Storms Per month with over 64 knots")#####barplot of numers of storms per month with >=64



tracks_96 <- tracks[tracks$wind>=96,]
num_permonth96 <- aggregate(tracks_96$id,
                           list(format(tracks_96$date,'%B')),unique)
num_permonth96[,2] <- as.numeric(sapply(num_permonth96[,2],length))
names(num_permonth96) <- c('Month',"Storms over 96")
num_permonth96###frequency table
barplot(num_permonth96[[2]],1:5,
        names.arg=num_permonth96[[1]],cex.names = 0.8,
        main="Number of Storms Per month with over 96 knots")#####barplot of numers of storms per month with >=96

#############avg number of storms over 35,64,96 knots####################

summary(num_peryear35[[2]])
sd(num_peryear35[[2]])


summary(num_peryear64[[2]])
sd(num_peryear64[[2]])


summary(num_peryear96[[2]])
sd(num_peryear96[[2]])

###################regression analysis###################

######reg 1: mean
mean_pressure <- aggregate(tracks$press,list(tracks$id),mean)
mean_wind <- aggregate(tracks$wind,list(tracks$id),mean)

mean_pressure <- mean_pressure[mean_pressure[[2]]!=0,]
mean_wind <- mean_wind[mean_wind[[1]]%in%mean_pressure[[1]],]

lm_mean <- lm(mean_wind[[2]]~mean_pressure[[2]])

library(ggplot2)
lm_meandata <- as.data.frame(cbind(mean_pressure[[2]],mean_wind[[2]]))
head(lm_meandata)
ggplot(data=lm_meandata,aes(x=V1,y=V2))+
  geom_point()+
  geom_abline(intercept=coef(lm_mean)[1],slope=coef(lm_mean)[2],size=1)+
  xlab('mean pressure')+
  ylab('mean wind speed')+
  ggtitle('Regression of Mean Storm Pressure and Mean Storm Speed')

######reg 2 :median
median_pressure <- aggregate(tracks$press,list(tracks$id),median)
median_wind <- aggregate(tracks$wind,list(tracks$id),median)

median_pressure <- median_pressure[median_pressure[[2]]!=0,]
median_wind <- median_wind[median_wind[[1]]%in%median_pressure[[1]],]

lm_median <- lm(median_wind[[2]]~median_pressure[[2]])
lm_mediandata <- as.data.frame(cbind(median_pressure[[2]],median_wind[[2]]))
head(lm_mediandata)
ggplot(data=lm_mediandata,aes(x=V1,y=V2))+
  geom_point()+
  geom_abline(intercept=coef(lm_median)[1],slope=coef(lm_median)[2],size=1)+
  xlab('median pressure')+
  ylab('median wind speed')+
  ggtitle('Regression of Median Storm Pressure and Median Storm Speed')

#############plot EP and NA data######################
setwd('../rawdata')
EPdata <- read.csv('EPrawdata.csv',skip=1)
NAdata <- read.csv('NArawdata.csv',skip=1)

head(EPdata)
head(NAdata)
 
EPdata <- EPdata[-1,]
NAdata <- NAdata[-1,]


