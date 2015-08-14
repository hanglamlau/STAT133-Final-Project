############load the rawdata#############
setwd('./rawdata')
rawdata <- read.csv('rawdata',header = FALSE, stringsAsFactors=FALSE)

################get the header information##########

dim(rawdata)
class(rawdata)
table(apply(rawdata,1,nchar))######so there should be 1777 entries of header

header <- rawdata[grepl('M',rawdata[,1]),]
head(header)

################clean the header data frame###########
class(header)
header_id <- 1:1777
header_date <- as.Date(substr(header,7,17),'%m/%d/%Y')

header_days <- as.numeric(substr(header,20,22))
hist(header_days)

header_names <- gsub(' ','',substr(header,35,47))

storms <- data.frame(id=header_id,date=header_date,days=header_days,name=header_names,row.names = NULL)
storms$date <- format(storms$date,'%m/%d/%Y')

head(storms)
tail(storms)

#################export the storms file##########
getwd()
write.csv(storms,'../data/storms.csv')


################retrieve the tracks information########
head(rawdata,20)

j <- row.names(rawdata)###########j is the index of header
j <- ifelse(nchar(rawdata[j,1])==85,j,0)
j  <- j[j!=0]
j <- as.numeric(j)
rawdata[j,1]

k <- row.names(rawdata)###########k is the index of trailer
k <- ifelse(nchar(rawdata[k,1])==36,k,0)
k  <- k[k!=0]
k <- as.numeric(k)
rawdata[k,1]

rep_id <- k-j-1#########generate id of storm
id <- rep(1:1777,rep_id)
length(id)

tracks <- rawdata[grepl('[\\*]',rawdata[,1]),]
length(tracks)
length(id)
raw_tracks <- rawdata[grepl('[\\*]',rawdata[,1]),]
tracks <- paste(id,tracks)#######track information with id

head(raw_tracks,20)
table_track <- data.frame('id'=1:51556,'date'=1:51556,'period'=1:51556,'stage'=1:51556,
                          'lat'=1:51556,'long'=1:51556,'wind'=1:51556,'press'=1:51556)
table_track$id <- rep(id,each=4)
table_track$date <- rep(substr(raw_tracks,7,11),each=4)

period <- c('00h','06h','12h','18h')
table_track$period <- rep(period)

vec_stage <- c(12,29,46,63)
stage <- character(0)
for (i in 1:12889){
  stage <- c(stage,substr(raw_tracks[i],vec_stage[1],vec_stage[1]))
  stage <- c(stage,substr(raw_tracks[i],vec_stage[2],vec_stage[2]))
  stage <- c(stage,substr(raw_tracks[i],vec_stage[3],vec_stage[3]))
  stage <- c(stage,substr(raw_tracks[i],vec_stage[4],vec_stage[4]))
}
table_track$stage <- stage
table_track$stage[table_track$stage=="*"] <- 'cyclone'
table_track$stage[table_track$stage=="S"] <- 'subtropical'
table_track$stage[table_track$stage=="E"] <- 'extratropical'
table_track$stage[table_track$stage=="L"] <- 'remanent'
table_track$stage[table_track$stage=="W"] <- 'wave'

lat <- character(0)
for (i in 1:12889){
  lat <- c(lat,substr(raw_tracks[i],13,15))
  lat <- c(lat,substr(raw_tracks[i],30,32))
  lat <- c(lat,substr(raw_tracks[i],47,49))
  lat <- c(lat,substr(raw_tracks[i],64,66))
}
table_track$lat <- as.numeric(lat)/10


long <- character(0)
for (i in 1:12889){
  long <- c(long,substr(raw_tracks[i],16,19))
  long <- c(long,substr(raw_tracks[i],33,36))
  long <- c(long,substr(raw_tracks[i],50,53))
  long <- c(long,substr(raw_tracks[i],67,70))
}
long <- as.numeric(long)/10
long <- ifelse(long>=180,long-360,long)
table_track$long <- long

wind <- character(0)
for (i in 1:12889){
  wind<- c(wind,substr(raw_tracks[i],20,23))
  wind<- c(wind,substr(raw_tracks[i],37,40))
  wind<- c(wind,substr(raw_tracks[i],54,57))
  wind<- c(wind,substr(raw_tracks[i],71,74))
}
wind <- as.numeric(wind)
summary(wind)
table_track$wind <- wind

press <- character(0)
for (i in 1:12889){
  press<- c(press,substr(raw_tracks[i],24,28))
  press<- c(press,substr(raw_tracks[i],41,45))
  press<- c(press,substr(raw_tracks[i],58,62))
  press<- c(press,substr(raw_tracks[i],75,79))
}
table_track$press <- as.numeric(press)

year <- format(as.Date(storms$date,'%m/%d/%Y'),'%Y')
year <- rep(year,rep_id)
year <- rep(year,each=4)
table_track$date <- as.Date(paste(table_track$date,'/',year,sep=''),'%m/%d/%Y')

table_track$date[24765:24788] <- table_track$date[24765:24788]+365
table_track$date[47365:47392] <- table_track$date[47365:47392]+365
table_track$date <- format(table_track$date,'%m/%d/%Y')  

######################to check whether the date is right#########
# result <- vector("numeric",51555)
# for (i in 1:51555){
#   result[i] <- table_track$date[i+1]-table_track$date[i]
# }
# table1 <- table_track[which(result<0),]
# table2 <- table_track[which(result<0)+1,]
# 
# which(table1$id==table2$id)
# table1[220,]
# table2[220,]
# 
# table1[514,]
# table2[514,]

#################to check whether the date is right################

#################delete some entries according to requirements#########
table_track <- table_track[table_track$lat!=0 | table_track$long!=0 | table_track$wind!=0 | table_track$press!=0,]

head(table_track)
tail(table_track)

#######################export the track csv##############

setwd('../data')
write.csv(table_track[,1:8],"tracks.csv")

