#==============================================================================
# MODIFY PARKS DATA
#==============================================================================

# READ/TIDY PARKS SHAPEFILES
library(rgdal)
library(broom)
library(maptools)
park <- readOGR(dsn="Shapefiles/Parks_Aug2012", layer="Parks_Aug2012")
parkdat <- tidy(park, region="PARK")
parkpoly <- cbind(parkdat$long, parkdat$lat)
colnames(parkpoly) <- c("long", "lat")

# SAVE
save(park, file="RData/park.RData")
save(parkdat, file="RData/parkdat.RData")
save(parkpoly, file="RData/parkpoly.RData")

