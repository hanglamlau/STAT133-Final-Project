##############set up the folders and files################

dir.create('rawdata')
dir.create('data')
dir.create('images')
dir.create('code')
dir.create('resources')
dir.create('report')


file.create('./code/data_cleaning.R')
file.create('./code/data_analysis.R')
file.create('./code/data_visualization.R')
file.create('./report/report.Rmd')
file.create('README.md')

################download the data########################

setwd('./rawdata')
setInternet2(use=FALSE)
url <- 'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat'
download.file(url,'rawdata')

url2 <- 'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv'
download.file(url2,'EPrawdata.csv')

url3 <- 'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv'
download.file(url3,'NArawdata.csv')

setwd('../')

