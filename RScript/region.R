#==============================================================================
# DISSOLVE POLYGON
#==============================================================================

# modified from: https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/

# LOAD PACKAGES
library(rgdal)
library(rgeos)
library(dplyr)

# READ AND MODIFY SHAPEFILES
region <- readOGR(dsn="Shapefiles/CommAreas", layer="CommAreas")
plot(region)
lookup <- data.frame()
lookup <- rbind(lookup, region@data)
lookup$AREA_NUMBE <- as.character(lookup$AREA_NUMBE)
lookup$COMMUNITY <- as.character(lookup$COMMUNITY)
lookup$CITY <- NA
lookup$CITY <- "Chicago"

# MERGE
region@data$AREA_NUMBE <- as.character(region@data$AREA_NUMBE)
region@data <- full_join(region@data, lookup, by="AREA_NUMBE")
row.names(region) <- row.names(region@data)
region <- spChFIDs(region, row.names(region))

# DISSOLVE
region <- gUnaryUnion(region, id=region@data$country)

# MODIFY DATAFRAME
row.names(region) <- as.character(1:length(region))
lookup <- unique(lookup$CITY)
lookup <- as.data.frame(lookup)
colnames(lookup) <- "CITY"
region <- SpatialPolygonsDataFrame(region, lookup)

# SAVE
save(region, file="RData/region.RData")



