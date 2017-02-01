#==============================================================================
# DISSOLVE POLYGON
#==============================================================================

# LOAD PACKAGES
library(rgdal)
library(rgeos)
library(dplyr)

# READ AND MODIFY SHAPEFILES
region <- readOGR(dsn="./Shapefiles/CommunityAreas", layer="CommunityAreas")
plot(region)
lookup <- data.frame()
lookup <- rbind(lookup, region@data)
lookup$area_numbe <- as.character(lookup$area_numbe)
lookup$community <- as.character(lookup$community)
lookup$city <- NA
lookup$city <- "Chicago"

# MERGE
region@data$area_numbe <- as.character(region@data$area_numbe)
region@data <- full_join(region@data, lookup, by="area_numbe")
region@data <- select(region@data, -community.x)
row.names(region) <- row.names(region@data)
region <- spChFIDs(region, row.names(region))

# DISSOLVE
region <- gUnaryUnion(region, id=region@data$country)

# MODIFY DATAFRAME
row.names(region) <- as.character(1:length(region))
lookup <- unique(lookup$city)
lookup <- as.data.frame(lookup)
colnames(lookup) <- "city"
region <- SpatialPolygonsDataFrame(region, lookup)
plot(region)