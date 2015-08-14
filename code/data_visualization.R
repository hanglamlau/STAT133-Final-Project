#======================================================================
#title: "Project Visualization""
#name: "Tian Xia & Hang Lam Lau "
# Date: 08-07-2015
# Description: visualizing the
#trajectory of the storms in both the East Pacific (EP)
#and the North Atlantic (NA) basins
#======================================================================
library(stringr)
library(maps)
library(dplyr)
library(ggplot2)
setwd('../rawdata')


#load the data
Ep<-read.csv('EPrawdata.csv',
             skip=1, stringsAsFactors=FALSE)
Na<- read.csv('NArawdata.csv', 
              skip = 1,stringsAsFactors = FALSE)

#Formatting the columns and take out the unwanted rows
a<-sub("^ ", "", Ep$Wind.WMO.)
Ep$Wind.WMO. <- as.numeric(a)
b<-sub("^ ", "", Ep$Longitude)
Ep$Longitude <- as.numeric(b)
c<-sub("^ ", "", Ep$Latitude)
Ep$Latitude<- as.numeric(c)
Ep$Season<- as.numeric(Ep$Season)

d<-sub("^ ", "", Na$Wind.WMO.)
Na$Wind.WMO. <-as.numeric(d)
e<-sub("^ ", "", Na$Longitude)
Na$Longitude<- as.numeric(e)
f<-sub("^ ", "", Na$Latitude)
Na$Latitude<- as.numeric(f)
Na$Season <- as.numeric(Na$Season)


Ep <- Ep[-1,]
Na <- Na[-1,]


#takes out month from both datasets for later use
time.date <- strsplit(Na$ISO_time, " ")
iso.date <- unlist(sapply(time.date, function(x) x[1]))
iso.month <- str_sub(iso.date,start =6, end =7)
Na$Month <- factor(iso.month, labels=c(month.name))

# extract month for dataset EP.basin
time.date <- strsplit(Ep$ISO_time, " ")
iso.date <- unlist(sapply(time.date, function(x) x[1]))
iso.month <- str_sub(iso.date,start =6, end=7)
Ep$Month <- factor(iso.month, labels=c(month.name)[-4])

#merge up the dataframe
EpNa <- rbind(Na, Ep)


#Using dplyr filter function to clear up unwanted data
EpNa <- EpNa %>%
  filter(Latitude > -999,                                 
         Longitude > -999)




# select storms between 1980 and 2010
EpNa_sub1 <- subset(EpNa, Season %in% 1980:2010)
EpNa_sub1 <- EpNa_sub1[!EpNa_sub1$Name == "UNNAMED",]

# add and ID with name and season
EpNa_sub1$ID <- as.factor(paste(EpNa_sub1$Name, EpNa_sub1$Season, sep="."))

# storm name as factor
EpNa_sub1$Name <- as.factor(EpNa_sub1$Name)

# world map
basemap <- map_data("world")


#trajectory of all the storms(1980 - 2010)
map1 <- ggplot(EpNa_sub1, aes(x = Longitude, y = Latitude, group = ID)) + 
  theme(panel.background = element_rect(fill = "#696969", colour = "#bdbdbd"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "#3f3f3f", colour = "#545454", size = 0.1) + 
  geom_path(data = EpNa_sub1, aes(group = ID, colour = Wind.WMO.), size = 0.4) + 
  xlim(-140, -30) + ylim(5, 50) + 
  labs(x = "", y = "", colour = "Wind \n(knots)")+
  ggtitle("Hurricane Trajectories by Year 1980 - 2010")

map1


#trajectory of storms per month [all years 1980-2010] (one facet per month)
map2<- ggplot(EpNa_sub1, aes(x = Longitude, y = Latitude, group = ID)) + 
  theme(panel.background = element_rect(fill = "#696969", colour = "#7e7e7e"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "#3f3f3f", colour = "#545454", size = 0.1) + 
  geom_path(data = EpNa_sub1, aes(group = ID, colour = Wind.WMO.), size = 0.3) + 
  xlim(-140, -30) + ylim(5, 50) + 
  labs(x = "", y = "", colour = "Wind \n(knots)") +
  ggtitle("Hurricane Trajectories by Month 1980 - 2010")+
  facet_wrap(~ Month)


map2


# trajectory of storms in decade 1980s (one facet per year)
EpNa_sub2 <- subset(EpNa, Season %in% 1980:1989)
EpNa_sub2 <- EpNa_sub2[!EpNa_sub2$Name == "UNNAMED",]
EpNa_sub2$ID <- as.factor(paste(EpNa_sub2$Name, EpNa_sub2$Season, sep="."))
EpNa_sub2$Name <- as.factor(EpNa_sub2$Name)


map3<- ggplot(EpNa_sub2, aes(x = Longitude, y = Latitude, group = ID)) + 
  theme(panel.background = element_rect(fill = "#696969", colour = "#7e7e7e"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "#3f3f3f", colour = "#545454", size = 0.1) + 
  geom_path(data = EpNa_sub2, aes(group = ID, colour = Wind.WMO.), size = 0.3) + 
  xlim(-140, -30) + ylim(5, 50) + 
  labs(x = "", y = "", colour = "Wind \n(knots)") +
  ggtitle("Hurricane Trajectories by decade 1980s")+
  facet_wrap(~ Season)
map3

# trajectory of storms in decade 1990s (one facet per year)

EpNa_sub3 <- subset(EpNa, Season %in% 1990:1999)
EpNa_sub3 <- EpNa_sub3[!EpNa_sub3$Name == "UNNAMED",]
EpNa_sub3$ID <- as.factor(paste(EpNa_sub3$Name, EpNa_sub3$Season, sep="."))
EpNa_sub3$Name <- as.factor(EpNa_sub3$Name)


map4<- ggplot(EpNa_sub3, aes(x = Longitude, y = Latitude, group = ID)) + 
  theme(panel.background = element_rect(fill = "#696969", colour = "#7e7e7e"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "#3f3f3f", colour = "#545454", size = 0.1) + 
  geom_path(data = EpNa_sub3, aes(group = ID, colour = Wind.WMO.), size = 0.3) + 
  xlim(-140, -30) + ylim(5, 50) + 
  labs(x = "", y = "", colour = "Wind \n(knots)") +
  ggtitle("Hurricane Trajectories by decade 1990s")+
  facet_wrap(~ Season)
map4


# trajectory of storms in decade 2000s (one facet per year)

EpNa_sub4 <- subset(EpNa, Season %in% 2000:2010)
EpNa_sub4 <- EpNa_sub4[!EpNa_sub4$Name == "UNNAMED",]
EpNa_sub4$ID <- as.factor(paste(EpNa_sub4$Name, EpNa_sub4$Season, sep="."))
EpNa_sub4$Name <- as.factor(EpNa_sub4$Name)


map5<- ggplot(EpNa_sub4, aes(x = Longitude, y = Latitude, group = ID)) + 
  theme(panel.background = element_rect(fill = "#696969", colour = "#7e7e7e"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_polygon(data = map_data("world"), 
               aes(x = long, y = lat, group = group), 
               fill = "#3f3f3f", colour = "#545454", size = 0.1) + 
  geom_path(data = EpNa_sub4, aes(group = ID, colour = Wind.WMO.), size = 0.3) + 
  xlim(-140, -30) + ylim(5, 50) + 
  labs(x = "", y = "", colour = "Wind \n(knots)") +
  ggtitle("Hurricane Trajectories by decade 2000s")+
  facet_wrap(~ Season)
map5


#PDF and PNG images generator

setwd('../images')

pdf('trajectory_of_all_the_storms_1980-2010.pdf')
map1
dev.off()


png('trajectory_of_all_the_storms_1980-2010.png')
map1
dev.off()

pdf('trajectory_of_all_the_storms_by_months_1980-2010.pdf')
map2
dev.off

png('trajectory_of_all_the_storms_by_months_1980-2010.png')
map2
dev.off


pdf('trajectory_of_all_the_storms_by_decade_1980s.pdf')
map3
dev.off

png('trajectory_of_all_the_storms_by_decade_1980s.png')
map3
dev.off

pdf('trajectory_of_all_the_storms_by_decade_1990s.pdf')
map4
dev.off

png('trajectory_of_all_the_storms_by_decade_1990s.png')
map4
dev.off

pdf('trajectory_of_all_the_storms_by_decade_2000s.pdf')
map5
dev.off

png('trajectory_of_all_the_storms_by_decade_2000s.png')
map5
dev.off




