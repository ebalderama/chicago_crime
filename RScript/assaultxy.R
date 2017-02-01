#==============================================================================
# MODIFY ASSAULT DATA
#==============================================================================

# READ IN ASSAULT DATA
assaultdat <- read.csv("CSV/ChicagoAssaults.csv", stringsAsFactors=FALSE)
save(assaultdat, file="RData/assaultdat.RData")

# MATRIX OF LATITUDES AND LONGITUDES OF ASSAULTS
assaultlatlong <- na.omit(cbind(assaultdat$Longitude, assaultdat$Latitude))
colnames(assaultlatlong) <- c("longitude", "latitude")
head(assaultlatlong)
class(assaultlatlong)

# CONVERT ASSAULTS TO SPATIAL POINTS FORMAT
llproj <- '+init=epsg:4269'
assaultsp <- SpatialPoints(na.omit(assaultlatlong), proj4string=CRS(llproj))

# CONVERT ASSAULTS TO MERCATOR PROJECTION
assaultmerc <- spTransform(assaultsp, CRS(proj4string(park)))

# CONVERT ASSAULTS TO COORDINATES
assaultxy <- coordinates(assaultmerc)

# SAVE
save(assaultxy, file="RData/assaultxy.RData")
