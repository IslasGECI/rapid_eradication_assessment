source("src/rea.R")

tagCent <- read.table("data/raw/GMIStaticDeviceLocations.csv",sep=",",header=T)
tmpshape <- rgdal::readOGR(dsn = "data/raw", layer = "GreatMercPerimiter")