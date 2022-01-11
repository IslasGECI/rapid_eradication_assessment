source("src/rea.R")
library("spatstat")

tagCent <- read.table("data/raw/GMIStaticDeviceLocations.csv", sep = ",", header = T)
tmpshape <- rgdal::readOGR(dsn = "data/raw", layer = "GreatMercPerimiter")
parameters <- read.table("data/raw/GMIParameters.csv", sep = ",", header = T)
sessiondat <- read.table("data/raw/GMIMonitoringData.csv", sep = ",", header = T)

spatstat.options(checkpolygons = T)
shape <- as(tmpshape, "owin")

POFx2 <- function(tagCent, tmpshape, shape, parameters, sessiondat, yearafter, k) {
  trying <- tryCatch(
    POFx(tagCent, tmpshape, shape, parameters, sessiondat, k, yearafter),
    error = function(e) e
  )
  if (class(trying)[1] == "simpleError") {
    print(NA)
  } else {
    print(trying[5])
  }
}

mediannight <- function(k, yearafter) {
  step1 <- replicate(5, POFx2(tagCent, tmpshape, shape, parameters, sessiondat, yearafter, k))
  step2 <- mean(unlist(step1), na.rm = TRUE)
}
allyears <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)
allnights <- 1:40
gridnew <- expand.grid(allnights, allyears) ## grid of all combinations of values
subgrid <- matrix(0, 40, 2)
submap <- matrix(0, 40, 9)
for (i in allnights) {
  subgrid <- subset(gridnew, gridnew[, 1] == i)
  submap[i, ] <- mapply(mediannight, subgrid[, 1], subgrid[, 2])
}
