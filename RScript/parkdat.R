#==============================================================================
# MODIFY PARKS DATA
#==============================================================================

# WORKING DIRECTORY
getwd()
setwd("/Users/Jonathan/Documents/R/Chicago/WorkingDirectory")

# READ/TIDY PARKS SHAPEFILES
library(rgdal)
library(broom)
library(maptools)
park <- readOGR(dsn="./Shapefiles/Parks_Aug2012", layer="Parks_Aug2012")
parkdat <- tidy(park, region="PARK")
parkpoly <- cbind(parkdat$long, parkdat$lat)
colnames(parkpoly) <- c("long", "lat")

# SAVE CHECKPOINT
save(park, file="/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/park.RData")
save(parkdat, file="/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/parkdat.RData")
save(parkpoly, file="/Users/Jonathan/Documents/R/Chicago/WorkingDirectory/RDataFiles/parkpoly.RData")

