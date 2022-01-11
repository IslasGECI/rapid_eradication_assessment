source("src/rea.R")

tagCent <- read.table("data/raw/GMIStaticDeviceLocations.csv",sep=",",header=T)
tmpshape <- rgdal::readOGR(dsn = "data/raw", layer = "GreatMercPerimiter")
parameters <- read.table("data/raw/GMIParameters.csv",sep=",",header=T)
sessiondat <- read.table("data/raw/GMIMonitoringData.csv",sep=",",header=T)

